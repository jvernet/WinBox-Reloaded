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

unit uWinBox;

interface

uses Windows, SysUtils, Classes, Controls, IniFiles, Forms, Registry, uBaseProfile, 
     uProcProfile, Generics.Collections, Generics.Defaults, uCommUtil, uProcessMon;

  (* Egy speciális absztrakt osztály, ami biztosít lehetõségeket a szokásos
   szStart/WinBox jellegû GUI számára. *)

type
  TProgressEvent = procedure (Sender: TObject; Min, Max: integer) of object;

  IWinBoxFrame = interface
    ['{05EFF6A1-715F-4A6A-BCBA-B46D6F7F4DF4}']
    procedure UpdateFull;
    procedure UpdateDelta;
    procedure FolderChange(Sender: TObject; FileName: string; ChangeType: Cardinal);
    procedure ReleaseFiles;
  end;

  TWinBoxProfile = class abstract(TWinProfile)
  private
    FProgress: TProgressEvent;
  protected
  public
    constructor Create(const AProfileID: string; AMonitor: TProcessMonitor); override;

    function OpenConfig(out Config: TCustomIniFile): boolean; virtual; // = false
    class function InternalName: string; virtual;
                                         
    class function DefExecutablePath: string; override;
    
    procedure ReloadIcon; override;
    procedure SaveIcon; override;

    function ProfileDlg: boolean; //profilbeállítások
    function SettingsDlg: boolean; virtual; abstract; //konfigurációs beállítások

    class function CreateProfileWizard(out ProfileID: string): boolean; virtual; abstract;
    //profilkészítési varázsló, ha sikeres értéke "true"

    function BackupFiles(const DestFolder: string): boolean; virtual; abstract;
    //a megadott mappába biztonsági másolatot készít minden szükséges konfigurációról
    //  és objektumról (pl. regisztrációs adatbázis kulcsok) - ha sikeres értéke "true"

    function MoveFiles(const DestFolder: string): boolean; virtual; abstract;
    //Hasonlít a backupra, de nem másol hanem áthelyez - de nem teljesen ugyanaz.
    // Célszerû egy közös fájlmûvelet sor létrehozása, és csak a mûveletet
    // cserélni pl. egy SHFileOperation kapcsán.

    function RestoreFiles(const SrcFolder: string; out ProfileID: string): boolean; virtual; abstract;
    //a biztonsági mentés készítõ által létrehozott könyvtár tartalmát állítja helyre
    //  egy új vagy meglévõ profilba - eredmények a visszatérési értékben

    //klónozás = Backup -> Temp + Temp -> Restore

    property OnProgress: TProgressEvent read FProgress write FProgress;
  end;

  TWinBoxMonitor = class(TAsyncProcessMonitor)
  protected
    function CreateDataField(const Process: TProcess): Integer; override;
  end;

//profil listákkal kapcsolatos feladatok
function FindProfileByID(const AProfileID: string; Profiles: TObjectList<TWinBoxProfile>): integer;
procedure SortProfiles(const Profiles: TObjectList<TWinBoxProfile>);

//ezt pedig a Monitor.Update után kell közvetlenül meghívni
procedure UpdatePIDs(Profiles: TObjectList<TWinBoxProfile>);

resourcestring
  StrWinBox = 'WinBox';
  SVmIconPng = 'vm-icon.png';

var
  MonitorLogging: boolean = false;

implementation

uses
  frmProfileDlg;

{$R 'Data\rcWinBox.res'}

const
  nFindWndClass = 1;
  szFindWndClass: array [0..nFindWndClass - 1] of string =
    ('86BoxMainWnd');

function FindProfileByID(const AProfileID: string;
  Profiles: TObjectList<TWinBoxProfile>): integer;
var
  I: integer;
begin
  Result := -1;
  if Assigned(Profiles) then
    for I := 0 to Profiles.Count - 1 do
      if Profiles[I].ProfileID = AProfileID then
        exit(I);
end;

procedure SortProfiles(const Profiles: TObjectList<TWinBoxProfile>);
begin
  if Assigned(Profiles) then
    Profiles.Sort(TComparer<TWinBoxProfile>.Construct(
      function (const L, R: TWinBoxProfile): integer
      begin
         Result := StrCmpLogicalW(PChar(L.FriendlyName), PChar(R.FriendlyName));
      end));
end;

procedure UpdatePIDs(Profiles: TObjectList<TWinBoxProfile>);
var
  I: Integer;
begin
  for I := 0 to Profiles.Count - 1 do
    Profiles[I].UpdatePIDs;
end;

{ TWinBoxProfile }

constructor TWinBoxProfile.Create(const AProfileID: string;
  AMonitor: TProcessMonitor);
begin
  inherited;
  SectionKey := SectionKey + '.' + InternalName;
end;

class function TWinBoxProfile.DefExecutablePath: string;
begin
  with TRegIniFile.Create(SRegRootKey, KEY_READ) do begin
    Result := ReadString(SRegConfigKey + '.' + InternalName, StrExecutablePath,
      inherited DefExecutablePath + InternalName + PathDelim + InternalName + '.exe');
    Free;
  end;
end;

class function TWinBoxProfile.InternalName: string;
begin
  Result := StrWinBox;
end;

function TWinBoxProfile.OpenConfig(out Config: TCustomIniFile): boolean;
begin
  Result := false;
end;

function TWinBoxProfile.ProfileDlg: boolean;
begin
  with TProfileDialog.Create(nil) do begin
    Profile := Self;
    Result := ShowModal = mrOK;
    Free;
  end;
end;

procedure TWinBoxProfile.ReloadIcon;
begin
  if FileExists(WorkingDirectory + SVmIconPng) then
    Icon.LoadFromFile(WorkingDirectory + SVmIconPng)
  else
    DefIcon;
end;

procedure TWinBoxProfile.SaveIcon;
begin
  if HasIcon then
    Icon.SaveToFile(WorkingDirectory + SVmIconPng);
end;

{ TWinBoxMonitor }

function TWinBoxMonitor.CreateDataField(const Process: TProcess): Integer;
var
  FindWindow: TFindWindow;
  NextClass: string;
begin
  Result := 0;
  for NextClass in szFindWndClass do begin
    with FindWindow do begin
      dwFindType := FIND_WINDOW_CLASSNAME;
      dwPID := Process.ProcessID;
      StrPLCopy(@szClassName[0], NextClass + #0, 260);
      hwnd := 0;
    end;
    EnumWindows(@FindWindowByPID, NativeInt(@FindWindow));
    if FindWindow.hwnd <> 0 then begin
      if MonitorLogging then
        dbgLogFmt('PID: %d, HWND: 0x%.8x, ClassName: %s',
          [Process.ProcessID, FindWindow.hwnd, NextClass]);
      exit(FindWindow.hwnd);
    end;
  end;
end;

end.
