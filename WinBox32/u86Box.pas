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

unit u86Box;

interface

uses
  Types, Windows, SysUtils, Classes, IniFiles, Registry, uProcessMon,
  uBaseProfile, uProcProfile, uWinBox, uCommUtil, Generics.Collections;

type
  T86BoxProfile = class(TWinBoxProfile)
  protected
    function CheckProcess(const Process: TProcess): boolean; override;
    function GetState: integer; override;
  public
    function OpenConfig(out Config: TCustomIniFile): boolean; override;
    class function InternalName: string; override;
    function Start(const Parameters: string = ''; const nShow: integer = SW_SHOWNORMAL): boolean; override;
    function SettingsDlg: boolean; override;
    procedure ReloadIcon; override;
  end;

  T86Box = class
    class function GetWorkingDirectory(const Process: TProcess): string;

    class function GetDeviceName(const Config: TCustomIniFile; const DeviceType: integer; const Resolve: boolean = false): string;
    class function GetDeviceData(const Config: TCustomIniFile; const DataType: integer): integer;

    class function ResolveDevName(const ID: string): string; overload;
    class function ResolveDevName(const ID: integer; const DeviceType: string): string; overload;

    class function FormatMemSize(const Config: TCustomIniFile): string;
    class function FormatCPU(const Config: TCustomIniFile): string;
    class function Format3Dfx(const Config: TCustomIniFile): string;
    class function FormatFDDs(const Config: TCustomIniFile): string;
    class function FormatHDDs(const Config: TCustomIniFile): string;
    class function FormatCDROMs(const Config: TCustomIniFile): string;
    class function FormatExStors(const Config: TCustomIniFile): string;
    class function FormatCOMs(const Config: TCustomIniFile): string;
    class function FormatLPTs(const Config: TCustomIniFile): string;

    class function GetHDDData(Config: TCustomIniFile; const ID: integer; out AType, AFileName: string): int64;
    class function GetCDROMData(Config: TCustomIniFile; const ID: integer; out AType, AFileName: string): integer;
    class function GetExStorData(Config: TCustomIniFile; const AClass: string; const ID: integer; out AType, AFileName: string): integer;

    class function GetImgAspect(Config: TCustomIniFile): TPoint;

    class procedure AddProfiles(List: TObjectList<TWinBoxProfile>; Monitor: TWinBoxMonitor);
  end;

var
  NameDefs: TCustomIniFile;

const
  SVoodooNameDefs   = 'voodoo';
  SJoystickNameDefs = 'joystick';

const
  IDM_ACTION_SCREENSHOT	  = 40011;
  IDM_ACTION_HRESET	      = 40012;
  IDM_ACTION_RESET_CAD    = 40013;
  IDM_ACTION_EXIT	       	= 40014;
  IDM_ACTION_CTRL_ALT_ESC = 40015;
  IDM_ACTION_PAUSE	      = 40016;
  IDM_CONFIG	        	  = 40020;

{$R 'Data\rc86Box.RES'}

implementation

uses uLang;

const
  SGlobalNameDefs  = 'global';
  SConfigFile      = '86box.cfg';
  SExecutableName  = '86box.exe';
  SNameDefs        = 'NAMEDEFS';
  SPathParamUpper1 = '-P';
  SPathParamUpper2 = '--VMPATH';
  SCmdLine         = '--vmpath "%s" %s';
  SDeviceNameNone  = 'none';
  SExStorZip       = 'zip';
  SExStorMo        = 'mo';

resourcestring
  StrSLI = ' SLI';
  StrSMO = ', %s MO';
  StrSZIP = ', %s ZIP';
  StrSDMHz = '%s / %d MHz';
  StrS2fMHz = '%s / %.2f MHz';
  StrDxS = ', %dx %s';
  StrCOMd = ', COM%d';
  StrLPTd = ', LPT%d';
  StrSettingsCmdLine = '-S -N';
  SVmIconPng = 'vm-icon.png';
  SRegSubKey = '.86Box';
  Str86Box = '86Box';
  StrNameDefs = 'NameDefs.';

function InitNameDefsMem: TMemIniFile;
var
  Stream: TStream;
  List: TStringList;

  Text: string;
  I: Integer;
begin
  Result := TMemIniFile.Create('');

  Stream := TResourceStream.Create(hInstance, SNameDefs, RT_RCDATA);

  with TStringStream.Create do
  try
    LoadFromStream(Stream);
    Text := DataString;
  finally;
    Free;
  end;

  List := TStringList.Create;
  Language.ReadSectionValues('NameDefs', List);
  for I := 0 to List.Count - 1 do
    Text := StringReplace(Text,
      List.Names[I],
      List.ValueFromIndex[I],
      [rfReplaceAll]);

  List.Clear;
  List.Text := Text;
  Result.SetStrings(List);
  Stream.Free;
  List.Free;
end;

{ T86Box }

class procedure T86Box.AddProfiles(List: TObjectList<TWinBoxProfile>;
  Monitor: TWinBoxMonitor);
var
  L: TStringList;
  I: integer;
begin
  if not Assigned(List) then
    exit;

  L := TStringList.Create;
  with TRegIniFile.Create(SRegBaseKey + SRegSubKey) do begin
    ReadSections(L);
    Free;
  end;

  for I := 0 to L.Count - 1 do
    with List[List.Add(T86BoxProfile.Create(L[I], Monitor))] do
       SectionKey := SRegBaseKey + SRegSubKey;

  L.Free;
end;

class function T86Box.Format3Dfx(const Config: TCustomIniFile): string;
begin
  if GetDeviceData(Config, 2) = 0 then
    Result := ResolveDevName(SDeviceNameNone)
  else begin
    Result := ResolveDevName(GetDeviceData(Config, 3), SVoodooNameDefs);
    if GetDeviceData(Config, 4) = 1 then
      Result := Result + StrSLI;
  end;
end;

class function T86Box.FormatCDROMs(const Config: TCustomIniFile): string;
var
  I, C, FSpeed: integer;
  FType, FFileName: string;
begin
  Result := '';
  C := 0;
  for I := 1 to 4 do begin
    FSpeed := GetCDROMData(Config, I, FType, FFileName);
    if FType <> 'NONE' then begin
      if C = 2 then
        Result := Result + ' ...'
      else
        Result := Result + format(StrDxS, [FSpeed, FType]);
    end;
    inc(C);
  end;

  Result := Copy(Result, 3, MaxInt);
  if Result = '' then
    Result := ResolveDevName(SDeviceNameNone);
end;

class function T86Box.FormatCOMs(const Config: TCustomIniFile): string;
var
  I: integer;
begin
  Result := '';
  for I := 1 to 4 do
    if GetDeviceData(Config, I + 5) <> 0 then
      Result := Result + format(StrCOMd, [I]);

  Result := Copy(Result, 3, MaxInt);
  if Result = '' then
    Result := ResolveDevName(SDeviceNameNone);
end;

class function T86Box.FormatCPU(const Config: TCustomIniFile): string;
var
  Temp: integer;
begin
  Temp := T86Box.GetDeviceData(Config, 1);
  if Temp < 8000000 then
    Result := format(StrS2fMHz,
      [GetDeviceName(Config, 1, true), Temp / 1e6])
  else
    Result := format(StrSDMHz,
      [GetDeviceName(Config, 1, true), Temp div 1000000]);
end;

class function T86Box.FormatExStors(const Config: TCustomIniFile): string;
var
  I: integer;
  FType, FFileName: string;
begin
  Result := '';
  for I := 1 to 4 do begin
    GetExStorData(Config, SExStorZip, I, FType, FFileName);
    if FType <> 'NONE' then begin
      Result := format(StrSZIP, [FType]);
      break;
    end;
  end;

  for I := 1 to 4 do begin
    GetExStorData(Config, SExStorMo, I, FType, FFileName);
    if FType <> 'NONE' then begin
      Result := Result + format(StrSMO, [FType]);
      break;
    end;
  end;

  Result := Copy(Result, 3, MaxInt);
  if Result = '' then
    Result := ResolveDevName(SDeviceNameNone);
end;

class function T86Box.FormatFDDs(const Config: TCustomIniFile): string;
begin
  Result := GetDeviceName(Config, 7, true) + ', ' +
            GetDeviceName(Config, 8, true);

  if GetDeviceName(Config, 9) <> 'none' then
    Result := Result + ', ...';
end;

class function T86Box.FormatHDDs(const Config: TCustomIniFile): string;
var
  I, C: integer;

  FSize: int64;
  FType, FFileName: string;
begin
  Result := '';

  C := 0;
  for I := 1 to 4 do begin
    FSize := GetHDDData(Config, I, FType, FFileName);
    if FSize > 0 then begin
      if C = 2 then begin
        Result := Result + ' ...';
        break;
      end
      else begin
        inc(C);
        Result := ', ' + FileSizeToStr(FSize, 0, 1000) + ' ' + FType;
      end;
    end;
  end;

  Result := Copy(Result, 3, MaxInt);
  if Result = '' then
    Result := ResolveDevName(SDeviceNameNone);
end;

class function T86Box.FormatLPTs(const Config: TCustomIniFile): string;
var
  I: integer;
begin
  Result := '';
  for I := 1 to 3 do
    if GetDeviceData(Config, I + 9) <> 0 then
      Result := Result + format(StrLPTd, [I]);

  Result := Copy(Result, 3, MaxInt);
  if Result = '' then
    Result := ResolveDevName(SDeviceNameNone);
end;

class function T86Box.FormatMemSize(const Config: TCustomIniFile): string;
begin
  Result := FileSizeToStr(int64(GetDeviceData(Config, 0)) * 1024, 0)
end;

class function T86Box.GetCDROMData(Config: TCustomIniFile; const ID: integer;
  out AType, AFileName: string): integer;
var
  L: TStringList;
begin
  L := TStringList.Create;
  ExtractStrings([','], [' '], PChar(
    Config.ReadString('Floppy and CD-ROM drives', 'cdrom_0' + IntToStr(ID) + '_parameters', '0, none')), L);

  if (L.Count >= 2) then
      AType := UpperCase(L[1])
  else
      AType := '';

  Result := Config.ReadInteger('Floppy and CD-ROM drives', 'cdrom_0' + IntToStr(ID) + '_speed', 8);
  AFileName := Config.ReadString('Floppy and CD-ROM drives', 'cdrom_0' + IntToStr(ID) + 'image_path', '');
  L.Free;
end;

class function T86Box.GetDeviceData(const Config: TCustomIniFile;
  const DataType: integer): integer;
begin
  case DataType of
    0: Result := Config.ReadInteger('Machine', 'mem_size', 256);
    1: Result := Config.ReadInteger('Machine', 'cpu_speed', 4772728);
    2: Result := Config.ReadInteger('Video', 'voodoo', 0);
    3: Result := Config.ReadInteger('3DFX Voodoo Graphics', 'type', 0);
    4: Result := Config.ReadInteger('3DFX Voodoo Graphics', 'sli', 0);
    5: Result := Config.ReadInteger('Input devices', 'joystick_type', 0);
    6..7:
       Result := Config.ReadInteger('Ports (COM & LPT)',
         'serial' + IntToStr(DataType - 5) + '_enabled', 1);
    8..9:
       Result := Config.ReadInteger('Ports (COM & LPT)',
         'serial' + IntToStr(DataType - 5) + '_enabled', 0);
    10: Result := Config.ReadInteger('Ports (COM & LPT)',
         'lpt1_enabled', 1);
    11..12:
       Result := Config.ReadInteger('Ports (COM & LPT)',
         'lpt' + IntToStr(DataType - 9) + '_enabled', 0);
    else
      Result := 0;
  end;
end;

class function T86Box.GetDeviceName(const Config: TCustomIniFile;
  const DeviceType: integer; const Resolve: boolean = false): string;
begin
  case DeviceType of
    0: Result := Config.ReadString('Machine', 'machine', 'ibmpc');
    1: Result := Config.ReadString('Machine', 'cpu_family', '8088');
    2: Result := Config.ReadString('Video', 'gfxcard', 'cga');
    3: Result := Config.ReadString('Sound', 'sndcard', 'none');
    4: Result := Config.ReadString('Sound', 'midi_device', 'none');
    5: Result := Config.ReadString('Network', 'net_card', 'none');
    6: Result := Config.ReadString('Network', 'net_type', 'none');
    7..8:
       Result := Config.ReadString('Floppy and CD-ROM drives',
         'fdd_0' + IntToStr(DeviceType - 6) + '_type', '525_2dd');
    9..10:
       Result := Config.ReadString('Floppy and CD-ROM drives',
         'fdd_0' + IntToStr(DeviceType - 6) + '_type', 'none');
    11: Result := Config.ReadString('Storage controllers', 'scsicard', 'none');
    12: Result := Config.ReadString('Input devices', 'mouse_type', 'none')
    else Result := SDeviceNameNone;
  end;

  if Resolve then
    Result := ResolveDevName(Result);
end;

class function T86Box.GetExStorData(Config: TCustomIniFile;
  const AClass: string; const ID: integer; out AType,
  AFileName: string): integer;
var
  L: TStringList;
  i1: integer;
begin
  L := TStringList.Create;
  ExtractStrings([','], [' '], PChar(
    Config.ReadString('Other removable devices',
       format('%s_0%d_parameters', [AClass, ID]), '0, none')), L);

  if (L.Count >= 2) and TryStrToInt(L[0], i1) then begin
    Result := i1;
    AType := UpperCase(L[1]);
  end
  else begin
    Result := 0;
    AType := '';
  end;

  AFileName := Config.ReadString('Other removable devices',
    format('%s_0%d_image_path', [AClass, ID]), '');
  L.Free;
end;

class function T86Box.GetWorkingDirectory(const Process: TProcess): string;
var
  I: integer;
begin
  Result := '';
  with CommandLineToArgs(Process.CommandLine) do
    try
       for I := 0 to Count - 1 do
        if ((UpperCase(Strings[I]) = SPathParamUpper1) or
            (UpperCase(Strings[I]) = SPathParamUpper2))
           and (I < Count - 1) then begin
             Result := IncludeTrailingPathDelimiter(
               ExpandFileNameTo(
                 ExcludeTrailingPathDelimiter(Strings[I + 1]),
                 ExtractFilePath(Process.ExecutablePath)));
           end;
    finally
      Free;
    end;
  if Result = '' then
     Result := ExtractFilePath(Process.ExecutablePath);
end;

class function T86Box.GetHDDData(Config: TCustomIniFile;
  const ID: integer; out AType, AFileName: string): int64;
var
  L: TStringList;
  i1, i2, i3: integer;
begin
  L := TStringList.Create;
  ExtractStrings([','], [' '], PChar(
    Config.ReadString('Hard disks', 'hdd_0' + IntToStr(ID) + '_parameters', '0, 0, 0, 0, none')), L);

  if (L.Count >= 5) and TryStrToInt(L[0], i1)
    and TryStrToInt(L[1], i2) and TryStrToInt(L[2], i3) then begin
      Result := int64(i1) * i2 * i3 * 512;
      AType := UpperCase(L[4]);
    end
  else begin
      Result := 0;
      AType := '';
  end;

  AFileName := Config.ReadString('Hard disks', 'hdd_0' + IntToStr(ID) + '_fn', '');
  L.Free;
end;

class function T86Box.GetImgAspect(Config: TCustomIniFile): TPoint;
var
  Text: string;
begin
  Text := Config.ReadString('General', 'window_fixed_res', '');

  if Text = '' then
    Text := Config.ReadString('WinBox', 'WindowSize', '960x720');

  Result := Point(1, 1);
  TryStrToInt(TextLeft(Text, 'x'), Result.X);
  TryStrToInt(TextRight(Text, 'x'), Result.Y);
end;

class function T86Box.ResolveDevName(const ID: string): string;
begin
  Result := NameDefs.ReadString(SGlobalNameDefs, ID, ID);
end;

class function T86Box.ResolveDevName(const ID: integer;
  const DeviceType: string): string;
var
  S: string;
begin
  S := IntToStr(ID);
  Result := NameDefs.ReadString(DeviceType, S, S);
end;

{ T86BoxProfile }

function T86BoxProfile.CheckProcess(const Process: TProcess): boolean;
begin
  Result := WideUpperCase(
    IncludeTrailingPathDelimiter(T86Box.GetWorkingDirectory(Process))) =
           WideUpperCase(
    IncludeTrailingPathDelimiter(WorkingDirectory));
end;

function T86BoxProfile.GetState: integer;
begin
  Result := inherited GetState;

  if Result = PROCESS_STATE_RUNNING then
    Result := Result + ord(IsChecked(IDM_ACTION_PAUSE));
end;

class function T86BoxProfile.InternalName: string;
begin
  Result := Str86Box;
end;

function T86BoxProfile.OpenConfig(out Config: TCustomIniFile): boolean;
begin
  Result := FileExists(WorkingDirectory + SConfigFile) and
            CanLockFile(WorkingDirectory + SConfigFile, GENERIC_READ);

  if Result then
    try
      //Config := TIniFile.Create(WorkingDirectory + SConfigFile)
      Config := TMemIniFile.Create(WorkingDirectory + SConfigFile, TEncoding.UTF8)
    except
      Result := false;
      Config.Free;
      Config := nil;
    end
end;

procedure T86BoxProfile.ReloadIcon;
begin
  if FileExists(WorkingDirectory + SVmIconPng) then
    Icon.LoadFromFile(WorkingDirectory + SVmIconPng)
  else
    inherited;
end;

function T86BoxProfile.SettingsDlg: boolean;
begin
  if Count > 0 then begin
    BringToFront;
    Result := Execute(IDM_CONFIG);
  end
  else if State <> PROCESS_STATE_RUN_PENDING then
    Result := Start(StrSettingsCmdLine)
  else
    Result := false;
end;

function T86BoxProfile.Start(const Parameters: string;
  const nShow: integer): boolean;
var
  Config: TCustomIniFile;
  Temp: string;
  FileName: array [0..7] of string; //Check for 8 HDD is unused by programs
  I: integer;
begin
  if OpenConfig(Config) then
    try
      for I := Low(FileName) to High(FileName) do begin
        T86Box.GetHDDData(Config, I, Temp, FileName[I]);
        if FileName[I] <> '' then
          FileName[I] := ExpandFileNameTo(FileName[I], WorkingDirectory);
      end;

      for I := Low(FileName) to High(FileName) do
        if (FileName[I] <> '') and not CanLockFile(FileName[I]) then
            raise Exception.Create(_T('StrLockedVdiskImg'));
    finally
      Config.Free;
    end;

  Result := inherited Start(
    format(SCmdLine, [ExcludeTrailingPathDelimiter(WorkingDirectory),
                      Parameters]), nShow);
end;

initialization
  NameDefs := InitNameDefsMem;

finalization
  NameDefs.Free;

end.
