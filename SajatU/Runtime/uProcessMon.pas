(*

    WinBox Reloaded R2 - An universal GUI for many emulators

    Copyright (C) 2020, Laci bá'

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

*)

unit uProcessMon;

interface

uses Windows, SysUtils, Classes, Forms, Variants, ActiveX, ComObj;

type
  TProcess = record
    ProcessID: DWORD;
    ExecutablePath: string;
    CommandLine: string;
    KernelModeTime: uint64;
    UserModeTime: uint64;
    WorkingSetSize: uint64;
    PercentCPU,
    PercentRAM: extended;
    Data: Integer;
  end;

  TProcessMonitor = class
  private
    //procedure SetFilter(const Value: string); virtual;
    function GetCount: integer;
    function GetProcess(I: integer): TProcess;
  protected
    //FFilter: string;
    FList: TArray<TProcess>;
    FUpdate: TNotifyEvent;
    FLastSystemTime: uint64;
    function CreateDataField(const Process: TProcess): Integer; virtual;
    procedure InternalUpdate(var ProcessList: TArray<TProcess>);
  public
    constructor Create(const AFilter: string = ''); reintroduce; virtual;
    procedure Update; virtual;
    function FindByPID(const PID: DWORD): integer;
    //property Filter: string read FFilter write SetFilter;
    property Processes[I: integer]: TProcess read GetProcess; default;
    property Count: integer read GetCount;
    property OnUpdate: TNotifyEvent read FUpdate write FUpdate;
  end;

  TAsyncProcessMonitor = class(TProcessMonitor)
  private
    FThread: TThread;
    FTask: integer;
    FCritSect: TRTLCriticalSection;
    function GetUpdating: boolean;
  public
    constructor Create(const AFilter: string = ''); override;
    procedure Update; override;
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
    property IsUpdating: boolean read GetUpdating;
  end;

  TProcessMonitorThread = class(TThread)
  private
    FAsyncMonitor: TAsyncProcessMonitor;
  protected
    procedure Execute; override;
    constructor Create(AAsyncMonitor: TAsyncProcessMonitor); reintroduce;
  end;

  TFindWindow = record
    dwFindType,
    dwPID: DWORD;
    szClassName,
    szTitle: array [0..MAX_PATH + 1] of char;
    hwnd: HWND;
  end;
  PFindWindow = ^TFindWindow;

const
  FIND_WINDOW_MAIN = 1;
  FIND_WINDOW_VISIBLE = 2;
  FIND_WINDOW_CLASSNAME = 4;
  FIND_WINDOW_TITLE = 8;

function GetProcessList(Filter: string): TArray<TProcess>;
function GetProcessListEx(Filter: string; FWMIService: OleVariant): TArray<TProcess>;
function KillProcess(const PID: DWORD): boolean;

function GetProcessFileName(const PID: DWORD): string;

function FindWindowByPID(hwnd: HWND; pFindWindow: PFindWindow): BOOL; stdcall;

implementation

uses uCommUtil;

var
  FSWbemLocator, FWMIService: OleVariant;
  MemoryStatus: TMemoryStatusEx;

function GetProcessFileName(const PID: DWORD): string;
var
  hProcess: THandle;
  Buffer: array [0..MAX_PATH] of char;
begin
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false, PID);
  if (hProcess <> INVALID_HANDLE_VALUE) and (hProcess <> 0) then begin
    FillChar(Buffer, SizeOf(Buffer), #0);
    if GetModuleFileName(hProcess, @Buffer[0], MAX_PATH) <> 0 then
      Result := String(Buffer);
    CloseHandle(hProcess);
  end
  else
    Result := '';
end;

function KillProcess(const PID: DWORD): boolean;
var
  hProcess: THandle;
begin
  hProcess := OpenProcess(PROCESS_TERMINATE, false, PID);
  if (hProcess <> INVALID_HANDLE_VALUE) and (hProcess <> 0) then begin
    Result := TerminateProcess(hProcess, 1);
    CloseHandle(hProcess);
  end
  else
    Result := false;
end;

function FindWindowByPID(hwnd: HWND; pFindWindow: PFindWindow): BOOL; stdcall;
var
  lpdwProcessId, I: DWORD;
  szBuffer: array [0..MAX_PATH + 1] of char;

  Found: boolean;
begin
  GetWindowThreadProcessId(hwnd, lpdwProcessId);
  Found := pFindWindow^.dwPID = lpdwProcessId;
  if Found then
    for I := 0 to 3 do
      if ((1 shl I) and pFindWindow^.dwFindType) <> 0 then
        case I of
          0: Found := Found and (GetWindow(hwnd, GW_OWNER) = 0);
          1: Found := Found and IsWindowVisible(hwnd);
          2: begin
               GetClassName(hwnd, @szBuffer[0], MAX_PATH);
               Found := Found and
                 (StrComp(PWideChar(@szBuffer[0]), Addr(pFindWindow^.szClassName[0])) = 0);
             end;
          3: begin
               GetWindowText(hwnd, @szBuffer[0], MAX_PATH);
               Found := Found and
                 (StrComp(PWideChar(@szBuffer[0]), Addr(pFindWindow^.szTitle[0])) = 0);
             end;
        end;

  if Found then
    pFindWindow^.hwnd := hwnd;
  Result := not Found;
end;

function EnumProcesses(lpidProcess: LPDWORD; cb: DWORD; var cbNeeded: DWORD): BOOL; stdcall; external 'psapi.dll';
{$EXTERNALSYM EnumProcesses}

function IsEmptyOrNull(const Value: Variant): Boolean;
begin
  Result := VarIsClear(Value) or VarIsEmpty(Value) or VarIsNull(Value) or (VarCompareValue(Value, Unassigned) = vrEqual);
  if (not Result) and VarIsStr(Value) then
    Result := Value = '';
end;

function FileTimeToUInt64(const FileTime: TFileTime): uint64; inline;
var
  I: _LARGE_INTEGER;
begin
  I.LowPart := FileTime.dwLowDateTime;
  I.HighPart := FileTime.dwHighDateTime;
  Result := uint64(I.QuadPart);
end;

function GetSystemCPUUsage: uint64;
var
  ftSysIdle, ftSysKernel, ftSysUser: TFileTime;
begin
  GetSystemTimes(ftSysIdle, ftSysKernel, ftSysUser);
  Result := FileTimeToUInt64(ftSysKernel) + FileTimeToUInt64(ftSysUser);
end;

function GetProcessCPUUsage(const Process: TProcess): uint64; inline;
begin
  Result := Process.KernelModeTime + Process.UserModeTime;
end;

function GetProcessList(Filter: string): TArray<TProcess>;
begin
  Result := GetProcessListEx(Filter, FWMIService);
end;

function GetProcessListEx(Filter: string; FWMIService: OleVariant): TArray<TProcess>;
var
  Buffer: array [0..1023] of DWORD;
  Needed, I: DWORD;

  FExecutablePath: string;
  FWbemObjectSet: OleVariant;
begin
  EnumProcesses(@Buffer[0], 1024, Needed);
  Needed := Needed div 4;

  SetLength(Result, 0);
  for I := 0 to Needed - 1 do
    try
      FWbemObjectSet:= FWMIService.Get(Format('Win32_Process.Handle="%d"', [Buffer[I]]));
       if not IsEmptyOrNull(FWbemObjectSet.ExecutablePath) then begin
         FExecutablePath := FWbemObjectSet.ExecutablePath;

         SetLength(Result, length(Result) + 1);
         with Result[High(Result)] do begin
           ProcessID := Buffer[I];
           ExecutablePath := FExecutablePath;

           if not IsEmptyOrNull(FWbemObjectSet.CommandLine) then
             CommandLine := FWbemObjectSet.CommandLine
           else
             CommandLine := '';

           UserModeTime := FWbemObjectSet.UserModeTime;
           KernelModeTime := FWbemObjectSet.KernelModeTime;
           WorkingSetSize := FWbemObjectSet.WorkingSetSize;

           PercentCPU := 0;
           PercentRAM := WorkingSetSize / MemoryStatus.ullTotalPhys * 100;
           Data := 0;
         end;
       end;
    except
    end;
end;

{ TProcessMonitor }

constructor TProcessMonitor.Create(const AFilter: string);
begin
  FLastSystemTime := 0;
  //FFilter := AFilter;
end;

function TProcessMonitor.CreateDataField(const Process: TProcess): Integer;
begin
  Result := 0;
end;

function TProcessMonitor.FindByPID(const PID: DWORD): integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FList) do
    if FList[I].ProcessID = PID then
      exit(I);
end;

function TProcessMonitor.GetCount: integer;
begin
  Result := length(FList);
end;

function TProcessMonitor.GetProcess(I: integer): TProcess;
begin
  Result := FList[I];
end;

procedure TProcessMonitor.InternalUpdate(var ProcessList: TArray<TProcess>);
var
  I, Index: integer;
  SystemTime: uint64;
begin
  SystemTime := GetSystemCPUUsage;

  for I := 0 to High(ProcessList) do
    with ProcessList[I] do begin
      Index := FindByPID(ProcessID);

      if (Index <> -1) and (FLastSystemTime <> 0) then
        PercentCPU := (GetProcessCPUUsage(ProcessList[I]) - GetProcessCPUUsage(FList[Index])) /
                      (SystemTime - FLastSystemTime) * 100
      else
        PercentCPU := 0;

      Data := CreateDataField(ProcessList[I]);
    end;
  FList := ProcessList;
  FLastSystemTime := SystemTime;
end;

//procedure TProcessMonitor.SetFilter(const Value: string);
//begin
//  FFilter := Value;
//  Update;
//end;

procedure TProcessMonitor.Update;
var
  ProcessList: TArray<TProcess>;
begin
  ProcessList := GetProcessList(''); //FFilter);
  InternalUpdate(ProcessList);
  if Assigned(FUpdate) then
    FUpdate(Self);
end;

{ TProcessMonitorThread }

constructor TProcessMonitorThread.Create(AAsyncMonitor: TAsyncProcessMonitor);
begin
  FAsyncMonitor := AAsyncMonitor;
  inherited Create(false);
end;

procedure TProcessMonitorThread.Execute;
var
  ProcessList: TArray<TProcess>;
  //LocalFilter: string;

  FSWbemLocator,
  FWMIService: OleVariant;
begin
  try
    CoInitialize(nil);
    FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
    FWMIService   := FSWbemLocator.ConnectServer('localhost', 'root\CIMV2', '', '');

    while not Terminated do begin
      if FAsyncMonitor.FTask = 1 then begin
        //Synchronize(
        //  procedure
        //  begin
        //    LocalFilter := FAsyncMonitor.FFilter;
        //  end);

        ProcessList := GetProcessListEx('', FWMIService); //LocalFilter, FWMIService);
        Synchronize(
          procedure
          begin
            FAsyncMonitor.InternalUpdate(ProcessList);
            FAsyncMonitor.Lock;
            if Assigned(FAsyncMonitor.FUpdate) then
              FAsyncMonitor.FUpdate(FAsyncMonitor);
            FAsyncMonitor.Unlock;
            InterlockedExchange(FAsyncMonitor.FTask, 0);
          end);
      end
      else
        Sleep(250);
    end;
  finally
    CoUninitialize;
    EndThread(0);
  end;
end;

{ TAsyncProcessMonitor }

constructor TAsyncProcessMonitor.Create(const AFilter: string);
begin
  InitializeCriticalSection(FCritSect);
  FTask := 0;

  FThread := TProcessMonitorThread.Create(Self);
  inherited;
  FThread.Suspended := false;
end;

destructor TAsyncProcessMonitor.Destroy;
begin
  FThread.Terminate;
  FThread.WaitFor;
  FThread.Free;

  DeleteCriticalSection(FCritSect);
  inherited;
end;

function TAsyncProcessMonitor.GetUpdating: boolean;
begin
  Result := FTask <> 0;
end;

procedure TAsyncProcessMonitor.Lock;
begin
  EnterCriticalSection(FCritSect);
end;

procedure TAsyncProcessMonitor.Unlock;
begin
  LeaveCriticalSection(FCritSect);
end;

procedure TAsyncProcessMonitor.Update;
begin
  InterlockedExchange(FTask, 1);
end;

initialization
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatusEx(MemoryStatus);
  FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  FWMIService   := FSWbemLocator.ConnectServer('localhost', 'root\CIMV2', '', '');

end.
