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

unit uBaseProfile;

interface

uses Windows, SysUtils, Classes, Graphics, IniFiles, Registry, IOUtils,
     uCommUtil, uLang;

const
  SRegBaseKey      = 'Software\Laci bá''\WinBox\Profiles';
  SRegRootKey      = 'Software\Laci bá''\WinBox\';
  SAppDataEnvVar   = 'APPDATA';
  SDefIconResName  = 'EMPTY';
  SRegConfigKey    = 'Configuration';

resourcestring
  SDefaultAppDataFolder = 'Laci bá''\WinBox';

  StrExecutablePath = 'ExecutablePath';
  StrFriendlyName = 'FriendlyName';
  StrWorkingDirectory = 'WorkingDirectory';
  StrBackgroundColor = 'BackgroundColor';

  StrOptionalParams = 'OptionalParams';
  StrRootDirectory  = 'RootDirectory';

  StrAutoAppearance = 'AutoAppearance';
  StrApperanceValues = 'ApperanceValues';

  StrDefaultAppearance = 'video_fullscreen_first=0'#13#10 +
                         'video_fullscreen_scale=1'#13#10 +
                         'dpi_scale=0'#13#10 +
                         'vid_resize=2';

(* A TProfile megvalósítja a regisztrációs adatbázisbeli profil kezelését. *)

type
  TProfile = class
  private
    FColor: TColor;
    FProfileID,
    FExecutablePath,
    FFriendlyName,
    FWorkingDirectory: string;
    FIcon: TWICImage;
    FHint: string;
    FTag: integer;
    FRootKey: string;
    FOptParams: string;
    procedure SetExecutablePath(const Value: string);
    procedure SetWorkingDirectory(const Value: string);
    function GetColor: TColor;
    procedure SetRootKey(const Value: string);
  protected
  public
    constructor Create(const AProfileID: string); reintroduce; virtual;
    destructor Destroy; override;

    function OpenProfile(const Path: string = SRegBaseKey;
      const Access: Cardinal = KEY_ALL_ACCESS): TRegIniFile; inline;

    procedure LoadProfile; virtual;
    procedure SaveProfile; virtual;

    class function DefExecutablePath: string; virtual;    // = SDefaultAppDataFolder\
    function DefWorkingDirectory: string; virtual;  // = SDefaultDocumentFolder\FProfileID\

    procedure DefIcon(const Name: string = SDefIconResName); virtual;

    function HasIcon: boolean;
    procedure ReloadIcon; virtual;
    procedure SaveIcon; virtual;

    //Ezek csak a registryt módosítják, az objektumok nem követik le a módosításokat.
    class function CreateProfile(const AExecutablePath, AFriendlyName, AWorkingDirectory: string;
      const APath: string = SRegBaseKey): string; overload; // = FProfileID
    class procedure CreateProfile(const AProfileID, AExecutablePath, AFriendlyName, AWorkingDirectory: string;
      const APath: string = SRegBaseKey); overload;
    class procedure DeleteProfile(const AProfileID: string; const APath: string = SRegBaseKey);

    property ProfileID: string read FProfileID;
    property FriendlyName: string read FFriendlyName write FFriendlyName;
    property WorkingDirectory: string read FWorkingDirectory write SetWorkingDirectory;
    property ExecutablePath: string read FExecutablePath write SetExecutablePath;
    property OptionalParams: string read FOptParams write FOptParams;
    property SectionKey: string read FRootKey write SetRootKey;
    property Hint: string read FHint write FHint;
    property Tag: integer read FTag write FTag;
    property Icon: TWICImage read FIcon;
    property Color: TColor read GetColor write FColor;
  end;

function DefWorkingRootDirectory: string;

implementation

function DefWorkingRootDirectory: string;
begin
  with TRegIniFile.Create(SRegRootKey, KEY_READ) do begin
    Result := IncludeTrailingPathDelimiter(
        ReadString(SRegConfigKey, StrRootDirectory,
          IncludeTrailingPathDelimiter(TPath.GetDocumentsPath)
          + _T('SDefaultDocumentsFolder')));
    Free;
  end;
end;

{ TProfile }

constructor TProfile.Create(const AProfileID: string);
begin
  FProfileID := AProfileID;
  FIcon := TWICImage.Create;
  SetRootKey(SRegBaseKey);
end;

class function TProfile.CreateProfile(const AExecutablePath, AFriendlyName,
  AWorkingDirectory, APath: string): string;
var
  ID: TGUID;
begin
  try
    CreateGUID(ID);
    Result := GUIDToString(ID);
    CreateProfile(Result, AExecutablePath, AFriendlyName, AWorkingDirectory, APath);
  except
    Result := '';
    raise;
  end;
end;

class procedure TProfile.CreateProfile(const AProfileID, AExecutablePath,
  AFriendlyName, AWorkingDirectory, APath: string);
begin
  if AProfileID <> '' then
    with TProfile.Create(AProfileID) do
      try
        SetRootKey(APath);
        SetExecutablePath(AExecutablePath);
        FFriendlyName := AFriendlyName;
        SetWorkingDirectory(AWorkingDirectory);
        SaveProfile;
      finally
        Free;
      end;
end;

class function TProfile.DefExecutablePath: string;
begin
  Result := IncludeTrailingPathDelimiter(
    GetEnvironmentVariable(SAppDataEnvVar)) + SDefaultAppDataFolder + PathDelim;
end;

procedure TProfile.DefIcon(const Name: string);
var
  Stream: TStream;
begin
  Stream := TResourceStream.Create(hInstance, Name, RT_RCDATA);
  FIcon.LoadFromStream(Stream);
  Stream.Free;
end;

function TProfile.DefWorkingDirectory: string;
begin
  Result := DefWorkingRootDirectory + FProfileID + PathDelim;
end;

class procedure TProfile.DeleteProfile(const AProfileID, APath: string);
begin
  with TRegIniFile.Create(APath) do begin
    EraseSection(AProfileID);
    Free;
  end;
end;

destructor TProfile.Destroy;
begin
  FIcon.Free;
  inherited;
end;

function TProfile.GetColor: TColor;
begin
  if FColor = clNone then
    Result := clWindow
  else
    Result := FColor;
end;

function TProfile.HasIcon: boolean;
type
  TReloadIcon = procedure of object;
var
  Impl: TReloadIcon;
  Base: TReloadIcon;
  ClassTProfile: TClass;
begin
  Impl := ReloadIcon;
  ClassTProfile := Self.ClassType;

  while (ClassTProfile <> nil) and (ClassTProfile <> TProfile) do
    ClassTProfile := ClassTProfile.ClassParent;

  if ClassTProfile = nil then
    exit(false);

  Base := TProfile(@ClassTProfile).ReloadIcon;
  Result := TMethod(Impl).Code <> TMethod(Base).Code;
end;

procedure TProfile.LoadProfile;
begin
  with OpenProfile(FRootKey, KEY_READ) do begin
    FExecutablePath   := ReadString(FProfileID, StrExecutablePath, DefExecutablePath);
    FFriendlyName     := ReadString(FProfileID, StrFriendlyName, FProfileID);
    FWorkingDirectory := ReadString(FProfileID, StrWorkingDirectory, DefWorkingDirectory);
    FOptParams        := ReadString(FProfileID, StrOptionalParams, '');
    FColor            := TColor(ReadInteger(FProfileID, StrBackgroundColor, Integer(clNone)));
    Free;
  end;

  if HasIcon then
    ReloadIcon;
end;

function TProfile.OpenProfile(const Path: string;
  const Access: Cardinal): TRegIniFile;
begin
  Result := TRegIniFile.Create(Path, Access);
end;

procedure TProfile.ReloadIcon;
begin
end;

procedure TProfile.SaveIcon;
begin
end;

procedure TProfile.SaveProfile;
begin
  with OpenProfile(FRootKey, KEY_ALL_ACCESS) do begin
    WriteString(FProfileID, StrExecutablePath, FExecutablePath);
    WriteString(FProfileID, StrFriendlyName, FFriendlyName);
    WriteString(FProfileID, StrWorkingDirectory, FWorkingDirectory);

    if FOptParams = '' then
      DeleteKey(FProfileID, StrOptionalParams)
    else
      WriteString(FProfileID, StrOptionalParams, FOptParams);

    if FColor = clNone then
      DeleteKey(FProfileID, StrBackgroundColor)
    else
      WriteInteger(FProfileID, StrBackgroundColor, Integer(FColor));

    Free;
  end;
end;

procedure TProfile.SetExecutablePath(const Value: string);
begin
  if not FileExists(Value) then
    dbgLog('While setting TProfile.ExecutablePath: ' + SysErrorMessage(ERROR_FILE_NOT_FOUND));

  FExecutablePath := Value;
end;

procedure TProfile.SetRootKey(const Value: string);
begin
  FRootKey := Value;
  LoadProfile;
end;

procedure TProfile.SetWorkingDirectory(const Value: string);
begin
  if not DirectoryExists(Value) then
    raise Exception.Create(SysErrorMessage(ERROR_PATH_NOT_FOUND));

  FWorkingDirectory := IncludeTrailingPathDelimiter(Value);
end;

end.
