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

unit uProcProfile;

interface

uses
  Windows, Messages, SysUtils, uBaseProfile, uProcessMon, uCommUtil;

(* A TProcessMonitor egy adott feltétel szerint kapcsolja a megadott
   TProcessMonitor elemeit.

   A feltételt CheckProcess felülírásával lehet megadni, alapértelmezés
   szerint a futtatható fájl neve kerül csak ellenõrzésre.

   A Monitor frissítésekor az OnUpdate eseményben futtatni kell az
   UpdatePIDs utasítást, hogy mindig szinkronban legyenek a profilok
   a Monitor által felfedett folyamatokkal.

   Ez a profiltípus ezen felül képes elindítani és megállítani a kapcsolt
   folyamatokat.  *)

type
  TProcessProfile = class(TProfile)
  private
    FMonitor: TProcessMonitor;
    FIndexMap: TArray<integer>;
    FStarting: boolean;
    function GetBytesOfRAM: uint64;
    function GetCount: integer;
    function GetPercentCPU: extended;
    function GetPercentRAM: extended;
    function GetProcess(I: integer): TProcess;
  protected
    function CheckProcess(const Process: TProcess): boolean; virtual;
    function GetState: integer; virtual;
  public
    constructor Create(const AProfileID: string; AMonitor: TProcessMonitor); reintroduce; virtual;

    procedure UpdatePIDs;

    function Start(const Parameters: string = '';
      const nShow: integer = SW_SHOWNORMAL): boolean; virtual;
    class function StateToStr(const ProfileState: Integer): string;
    procedure Terminate(const All: boolean = true);

    property Monitor: TProcessMonitor read FMonitor;
    property Processes[I: integer]: TProcess read GetProcess; default;
    property Count: integer read GetCount;
    property PercentCPU: extended read GetPercentCPU;
    property PercentRAM: extended read GetPercentRAM;
    property BytesOfRAM: uint64 read GetBytesOfRAM;
    property State: integer read GetState;
  end;

(* Ha egy speciális ProcessMonitor változatot (szinkron/aszinkron)
   származtatunk le, ami a Data mezõt a CreateDataField utasítás
   felülírásával feltölti a program fõablakának azonosítójával, akkor
   alkalmazható ez az objektum.

   Ilyenkor a TWinProfile a Data mezõ felhasználásával alap ablakkezelõ
   lehetõségeket biztosít.

   Több folyamat esetén a TWinProfile csak az indextáblában elsõvel kommunikál. *)

type
  TWinProfile = class(TProcessProfile)
  private
    function GetHandle: HWND;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
  public
    procedure BringToFront;
    function Execute(const CommandID: NativeUInt): boolean; inline;
    function IsChecked(const CommandID: NativeUInt): boolean;
    function Perform(const Msg, WParam, LParam: NativeUInt): NativeUInt;
    function Close: boolean; inline;

    property Handle: HWND read GetHandle;
    property Caption: string read GetTitle write SetTitle;
  end;

const
  PROCESS_STATE_UNKNOWN        = -1;
  PROCESS_STATE_STOPPED        = 0;
  PROCESS_STATE_RUNNING        = 1;
  PROCESS_STATE_PAUSED         = 2; //Késõbbiekben használva, mivel VM-ekrõl van szó.
                                    //Nem azt jelenti hogy a folyamat suspended/nem.
  PROCESS_STATE_ERROR_MULTINST = 3;
  PROCESS_STATE_RUN_PENDING    = 4;

  PROCESS_STATE_SAVED          = 5; //Jelenleg egyik emulátor sem tud ilyet, de
                                    //késõbbi célokra fenntartva (pl. VirtualBox?).

resourcestring
  STR_PROCESS_STATE_UNKNOWN        = 'Ismeretlen';
  STR_PROCESS_STATE_STOPPED        = 'Leállítva';
  STR_PROCESS_STATE_RUNNING        = 'Fut';
  STR_PROCESS_STATE_PAUSED         = 'Szüneteltetve';
  STR_PROCESS_STATE_ERROR_MULTINST = 'Több példány!';
  STR_PROCESS_STATE_RUN_PENDING    = 'Indítás...';
  STR_PROCESS_STATE_SAVED          = 'Altatva';

var
  AssignmentLogging: boolean = false;
  ProcessLogging: boolean = false;

implementation

resourcestring
  EInvalidProcessIndex = 'Érvénytelen folyamat-index, vagy definíálatlan monitor (%s.Processes[%d]).';

{ TProcessProfile }

function TProcessProfile.CheckProcess(const Process: TProcess): boolean;
begin
  Result := WideUpperCase(Process.ExecutablePath) =
            WideUpperCase(ExecutablePath);
end;

constructor TProcessProfile.Create(const AProfileID: string;
  AMonitor: TProcessMonitor);
begin
  FStarting := false;
  FMonitor := AMonitor;
  inherited Create(AProfileID);
end;

function TProcessProfile.GetBytesOfRAM: uint64;
var
  I: integer;
begin
  Result := 0;
  for I in FIndexMap do
    Result := Result + Monitor[I].WorkingSetSize;
end;

function TProcessProfile.GetCount: integer;
begin
  Result := length(FIndexMap);
end;

function TProcessProfile.GetPercentCPU: extended;
var
  I: integer;
begin
  Result := 0;
  for I in FIndexMap do
    Result := Result + Monitor[I].PercentCPU;
end;

function TProcessProfile.GetPercentRAM: extended;
var
  I: integer;
begin
  Result := 0;
  for I in FIndexMap do
    Result := Result + Monitor[I].PercentRAM;
end;

function TProcessProfile.GetProcess(I: integer): TProcess;
begin
  if Assigned(FMonitor) and (I >= 0) and (I < length(FIndexMap)) then
    Result := FMonitor[FIndexMap[I]]
  else
    raise Exception.CreateFmt(EInvalidProcessIndex, [ClassName, I]);
end;

function TProcessProfile.GetState: integer;
begin
  if not Assigned(FMonitor) then
    exit(PROCESS_STATE_UNKNOWN);

  case length(FIndexMap) of
    0: if FStarting then
         Result := PROCESS_STATE_RUN_PENDING
       else
         Result := PROCESS_STATE_STOPPED;

    1: begin
         Result := PROCESS_STATE_RUNNING;
         FStarting := false;
       end;

    else Result := PROCESS_STATE_ERROR_MULTINST;
  end;
end;

function TProcessProfile.Start(const Parameters: string;
  const nShow: integer): boolean;
var
  szAppName, szWorkDir, szCmdLine: string;
  siStartup: TStartupInfo;
  piProcess: TProcessInformation;
begin
  if not (FileExists(ExecutablePath) and //létezzen a mappa és a progi
    DirectoryExists(ExcludeTrailingPathDelimiter(WorkingDirectory))) then
      exit(false);

  if length(FIndexMap) > 0 then //csak 1x lehet elindítani
    exit(false);

  szAppName := '#0';
  szCmdLine := format('"%s" %s %s'#0, [ExecutablePath, Parameters, OptionalParams]);
  szWorkDir := ExcludeTrailingPathDelimiter(ExtractFilePath(ExecutablePath)) + #0;

  FillChar(piProcess, SizeOf(piProcess), #0);
  FillChar(siStartup, SizeOf(siStartup), #0);
  siStartup.cb := SizeOf(siStartup);
  siStartup.dwFlags := STARTF_USESHOWWINDOW;
  siStartup.wShowWindow := nShow;

  Result := CreateProcess(nil, @szCmdLine[1], nil, nil, false, 0, nil,
                 @szWorkDir[1], siStartup, piProcess);

  if ProcessLogging then begin
    if Result then
      dbgLog('CreateProcess: Success')
    else
      dbgLogFmt('CreateProcess: Failed!. Reason: %s.', [SysErrorMessage(GetLastError)]);

    dbgLogFmt('CommandLine: %s, PID: %d, TID: %d',
          [szCmdLine, piProcess.dwProcessID, piProcess.dwThreadID]);
  end;

  FStarting := Result; //a monitor visszajelzéséig "indítás folyamatban" állapot, ha sikerült az indítás
  CloseHandle(piProcess.hProcess);
  CloseHandle(piProcess.hThread);
end;

class function TProcessProfile.StateToStr(const ProfileState: Integer): string;
begin
  case ProfileState of
    PROCESS_STATE_STOPPED:        Result := STR_PROCESS_STATE_STOPPED;
    PROCESS_STATE_RUNNING:        Result := STR_PROCESS_STATE_RUNNING;
    PROCESS_STATE_PAUSED:         Result := STR_PROCESS_STATE_PAUSED;
    PROCESS_STATE_ERROR_MULTINST: Result := STR_PROCESS_STATE_ERROR_MULTINST;
    PROCESS_STATE_RUN_PENDING:    Result := STR_PROCESS_STATE_RUN_PENDING;
    else
      Result := STR_PROCESS_STATE_UNKNOWN;
  end;
end;

procedure TProcessProfile.Terminate(const All: boolean);
var
  I: Integer;
begin
  for I in FIndexMap do begin
    KillProcess(FMonitor[I].ProcessID);

    if ProcessLogging then
      dbgLogFmt('Kill process PID %d', [FMonitor[I].ProcessID]);

    if not All then
      break;
  end;

  FMonitor.Update;
end;

procedure TProcessProfile.UpdatePIDs;
var
  I: Integer;
begin
  SetLength(FIndexMap, 0);
  if Assigned(FMonitor) then
    for I := 0 to FMonitor.Count - 1 do
      if CheckProcess(FMonitor[I]) then begin
        SetLength(FIndexMap, length(FIndexMap) + 1);
        FIndexMap[High(FIndexMap)] := I;

        if AssignmentLogging then
          dbgLogFmt('PID %d, HWND %.8x is Profile %s (%s)',
            [FMonitor[I].ProcessID, FMonitor[I].Data, FriendlyName, ProfileID]);

      end;
end;

{ TWinProfile }

procedure TWinProfile.BringToFront;
begin
  if Count > 0 then
    with Processes[0] do
      BringWindowToFront(Data);
end;

function TWinProfile.Close: boolean;
begin
  Result := Perform(WM_CLOSE, 0, 0) = 0;
end;

function TWinProfile.Execute(const CommandID: NativeUInt): boolean;
begin
  Result := Perform(WM_COMMAND, CommandID, 0) = 0;
end;

function TWinProfile.GetHandle: HWND;
begin
  if Count > 0 then
    Result := Processes[0].Data
  else
    Result := 0;
end;

function TWinProfile.GetTitle: string;
begin
  Result := GetWindowTitle(GetHandle);
end;

function TWinProfile.IsChecked(const CommandID: NativeUInt): boolean;
begin
  Result := (GetMenuState(GetMenu(GetHandle), CommandID, MF_BYCOMMAND) and MF_CHECKED) <> 0;
end;

function TWinProfile.Perform(const Msg, WParam, LParam: NativeUInt): NativeUInt;
begin
  if Count > 0 then
    Result := SendMessage(Processes[0].Data, Msg, WParam, LParam)
  else
    Result := UINT(-1);
end;

procedure TWinProfile.SetTitle(const Value: string);
begin
  SetWindowText(GetHandle, PChar(Value));
end;

end.
