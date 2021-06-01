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

unit frmUpdate;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, IOUtils,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Registry, DateUtils, Zip,
  uBaseProfile, uLang;

type
  IAutoUpdate = interface
    ['{768238E2-A485-4DE5-AEA8-51E506DD81BC}']
    function Execute(const ByCommand: boolean = false): boolean; stdcall;
    function HasUpdate: boolean; stdcall;
    function AutoUpdate: boolean; stdcall;
  end;

  TUpdateThread = class;

  TUpdateForm = class(TForm, IAutoUpdate, ILanguageSupport)
    Label1: TLabel;
    LogBox: TListBox;
    Button1: TButton;
    lbState: TLabel;
    Progress: TProgressBar;
    Shape1: TShape;
    State: TLabel;
    lbFileName: TLabel;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LogBoxDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure WMStartup(var Msg: TMessage); message WM_USER;
  protected
    Updater: TUpdateThread;
  public
    function Execute(const ByCommand: boolean = false): boolean; stdcall;
    function HasUpdate: boolean; stdcall;
    function AutoUpdate: boolean; stdcall;

    procedure GetTranslation(Language: TLanguage); stdcall;
    procedure Translate; stdcall;
  end;

  TUpdateThread = class(TThread)
  private
    function CheckCancel: boolean;
    function VersionCheck: boolean;
    function Download: boolean;
    function Extract: boolean;
  protected
    procedure Execute; override;

    procedure Log(const S: string);
    procedure LogFmt(const S: string; const Args: array of const);
    function MessageBox(const Text: string; const Flags: longword): longword;
    function MessageBoxFmt(const Text: string; const Args: array of const; const Flags: longword): longword;
  public
    Form: TUpdateForm;

    FProgress, FProgressMax: integer;
    FState, FFileName: string;

    FMode, FAutoUpdate, FGetSource: integer;
    FRepositories: array [0..1] of string;
    FPaths: array [0..0] of string;
    FCancelled, FBuild: integer;
    FEmulator, FRoms, FSource: string;

    procedure OnZipProgress(Sender: TObject; FileName: string;
      Header: TZipHeader; Position: Int64);
  end;

var
  UpdateForm: TUpdateForm;

implementation

uses uCommUtil, uWebUtils;

resourcestring
  SRegRootKey    = 'Software\Laci bá''\WinBox\';
  SRegConfigKey  = 'Configuration';
  StrExecutablePath = 'ExecutablePath';
  StrRepository = 'Repository';
  Def86BoxRepo = 'https://ci.86box.net/job/86Box';
  Def86RomsRepo = 'https://github.com/86Box/roms';
  Def86SrcRepo = 'https://github.com/86Box/86Box';
  StrWinBox = 'WinBox';
  StrAutoUpdate = 'AutoUpdate';
  StrDownloadSource = 'DownloadSource';


{$R *.dfm}

function TUpdateForm.AutoUpdate: boolean;
begin
  Result := Updater.FAutoUpdate <> 0;
end;

procedure TUpdateForm.Button1Click(Sender: TObject);
begin
  if Button1.Caption = _T('StrKilépés') then
    Close
  else
    InterlockedExchange(Updater.FCancelled, 1);
end;

function TUpdateForm.Execute(const ByCommand: boolean): boolean;
begin
  Updater.FMode := ord(ByCommand);
  Result := ShowModal = mrOK;
end;

procedure TUpdateForm.FormCreate(Sender: TObject);
begin
  Updater := TUpdateThread.Create(true);
  Updater.Form := Self;

  Translate;
  with Updater, TRegIniFile.Create(SRegRootKey, KEY_READ) do begin
    FRepositories[0] := ReadString(SRegConfigKey + '.86Box', StrRepository, Def86BoxRepo);
    FAutoUpdate := ReadInteger(SRegConfigKey + '.86Box', StrAutoUpdate, 1);
    FGetSource := ReadInteger(SRegConfigKey + '.86Box', StrDownloadSource, 0);
    FPaths[0] := ReadString(SRegConfigKey + '.86Box', StrExecutablePath,
      TProfile.DefExecutablePath + '86Box\86Box.exe');
    Free;

    if FGetSource <> 0 then
      Self.Progress.Max := 6;
  end;
end;

procedure TUpdateForm.FormDestroy(Sender: TObject);
begin
  Updater.Free;
end;

procedure TUpdateForm.FormShow(Sender: TObject);
begin
  LogBox.Clear;
  Updater.FCancelled := 0;
  Button1.Caption := _T('StrMegszakítás');

  PostMessage(Handle, WM_USER, 0, 0);
  OnShow := nil;
end;

procedure TUpdateForm.GetTranslation(Language: TLanguage);
begin
  if Assigned(Language) then
    Language.GetTranslation('UpdateFrm', Self);
end;

function TUpdateForm.HasUpdate: boolean;
begin
  Result := (not FileExists(Updater.FPaths[0])) or
     (jenkinsCheckUpdate(Updater.FRepositories[0], GetFileTime(Updater.FPaths[0])));
end;

procedure TUpdateForm.LogBoxDblClick(Sender: TObject);
begin
   if LogBox.ItemIndex <> -1 then
     ShowMessage(LogBox.Items[LogBox.ItemIndex]);
end;

procedure TUpdateForm.Timer1Timer(Sender: TObject);
begin
  if Assigned(Updater) and not Updater.Suspended then
    Updater.Synchronize(procedure
                        begin
                          Progress.Position := Updater.FProgress;
                          Progress.Max := Updater.FProgressMax;
                          State.Caption := Updater.FState;
                          lbFileName.Caption := Updater.FFileName;
                        end);
end;

procedure TUpdateForm.Translate;
begin
  if Assigned(Language) then
    Language.Translate('UpdateFrm', Self);
end;

procedure TUpdateForm.WMStartup(var Msg: TMessage);
begin
  Updater.Suspended := false;
end;

{ TUpdateThread }

function TUpdateThread.CheckCancel: boolean;
begin
  Result := FCancelled <> 0;
  if Result then begin
    Log(_T('StrAFolyamatotAFelha'));
    FState := _T('StrÜresjárat');
  end;
end;

function TUpdateThread.Download: boolean;
var
  Temp: string;
begin
  Result := true;

  try
    FEmulator := TPath.GetTempFileName;
    LogFmt(_T('StrAzEmulátorFájlaina'), [ExtractFileName(FEmulator)]);
    Temp := jenkinsGetBuild(FRepositories[0], FBuild);
    if Temp = '' then begin
      Log(SysErrorMessage(ERROR_REM_NOT_LIST));
      exit(false);
    end;
    httpsGet(Temp, FEmulator);
    FProgress := FProgress + 1;
    FFileName := (ExtractFileName(FEmulator));

    LogFmt(_T('StrROMképekLetöltése'), [Def86RomsRepo]);
    FRoms := TPath.GetTempFileName;
    LogFmt(_T('StrROMképekFájlainak'), [ExtractFileName(FRoms)]);
    gitClone(Def86RomsRepo, FRoms);
    FProgress := FProgress + 1;
    FFileName := (ExtractFileName(FRoms));

    if FGetSource <> 0 then begin
      LogFmt(_T('StrForráskódLetöltés2'), [Def86SrcRepo]);
      FSource := TPath.GetTempFileName;
      LogFmt(_T('StrForráskódLetöltése'), [ExtractFileName(FSource)]);
      gitClone(Def86SrcRepo, FSource);
      FProgress := FProgress + 1;
      FFileName := (ExtractFileName(FSource));
    end;
  except
    on E: Exception do begin
      Log(_T('StrHibaTörténtAFájl2'));
      Log(#9 + E.Message);
      Result := false;
      FFileName := ('-');
    end;
  end;
end;


function TUpdateThread.Extract: boolean;
var
  Root: string;
begin
  Result := true;
  Root := ExtractFilePath(FPaths[0]);
  Log(_T('StrAKorábbiVerzióElt'));
  if DirectoryExists(Root) and not DeleteWithShell(ExcludeTrailingPathDelimiter(Root)) then begin
    Log(_T('StrHibaTörténtAKoráb'));
    exit(false);
  end;
  ForceDirectories(Root);

  try
    with TZipFile.Create do begin
      try
        OnProgress := OnZipProgress;
        Log(_T('StrBinárisokKibontása'));
        Open(FEmulator, zmRead);
        ExtractAll(Root);
        LogFmt(_T('StrDFájlKibontva'), [length(FileInfos)]);
        Close;
        FProgress := FProgress + 1;

        Log(_T('StrROMképekKibontása'));
        Open(FRoms, zmRead);
        ExtractAll(Root);
        LogFmt(_T('StrDFájlKibontva'), [length(FileInfos)]);
        Close;
        FProgress := FProgress + 1;

        if FGetSource <> 0 then begin
          Log(_T('StrForráskódKibontása'));
          Open(FSource, zmRead);
          ExtractAll(Root);
          LogFmt(_T('StrDFájlKibontva'), [length(FileInfos)]);
          Close;
          FProgress := FProgress + 1;
        end;
      finally
        Free;
      end;
    end;

    Log(_T('StrKönyvtárÁtnevezés2'));
    if not RenameFile(Root + 'roms-master', Root + 'roms') then begin
      Log(_T('StrÁtnevezésKözbenHib'));
      Result := false;
    end;

    if FGetSource <> 0 then begin
      Log(_T('StrKönyvtárÁtnevezése'));
      if not RenameFile(Root + '86box-master', Root + 'source') then begin
        Log(_T('StrÁtnevezésKözbenHib'));
        Result := false;
      end;
    end;
  except
    on E: Exception do begin
      Log(_T('StrHibaTörténtAFájlo'));
      Log(#9 + E.Message);
      FFileName := ('-');
      Result := false;
    end;
  end;
end;

function TUpdateThread.VersionCheck: boolean;
var
  DateLocal, DateOnline: TDateTime;
  I: integer;
begin
  FFileName := ('-');
  if FileExists(FPaths[0]) then begin
    DateLocal := GetFileTime(FPaths[0]);
    if DateLocal = 0 then
      Result := MessageBox(_T('StrAJelenlegiVerzióN'), MB_ICONQUESTION or MB_YESNO) = mrYes
    else begin
      LogFmt(_T('StrJelenlegiVerzióS'), [DateTimeToStr(DateLocal)]);
      LogFmt(_T('StrLegfrisebbVerzióKe'), [FRepositories[0]]);
      try
         FBuild := jenkinsLastBuild(FRepositories[0]);
         if FBuild <> -1 then begin
           DateOnline := jenkinsGetDate(FRepositories[0], FBuild);
           LogFmt(_T('StrElérhetõVerzióS'), [DateTimeToStr(DateOnline)]);
           Result := CompareDate(DateLocal, DateOnline) < 0;
           if not Result then begin
             Result := (FMode = 1) and (MessageBox(
               _T('StrATelepítettVerzió'),
               MB_ICONWARNING or MB_YESNO) = mrYes);
             if not Result then begin
               FCancelled := 1;
               CheckCancel;
             end;
           end
           else with jenkinsGetChangelog(FRepositories[0], FBuild) do begin
             Log(_T('StrLegfrisebbVáltozáso'));
             for I := 0 to Count - 1 do
               Log(#9 + Strings[I]);
             Result := MessageBoxFmt(_T('StrFrissítésElérhetõ'),
                 [FBuild, DateTimeToStr(DateOnline)],
                 MB_ICONQUESTION or MB_YESNO) = mrYes;
             if not Result then begin
               FCancelled := 1;
               CheckCancel;
             end;
             Free;
           end;
         end
         else begin
           Log(SysErrorMessage(ERROR_REM_NOT_LIST));
           Result := false;
         end;
      except
         Log(SysErrorMessage(ERROR_REM_NOT_LIST));
         Result := false;
      end;
    end;
  end
  else begin
    Result := true;
    Log(_T('StrJelenlegNincsEgyet'));
    FBuild := jenkinsLastBuild(FRepositories[0]);
  end;
end;

procedure TUpdateThread.Execute;
begin
  repeat
    FState := (_T('StrVerziókEllenõrzése'));
    if not VersionCheck then break;
    if CheckCancel then break;

    FState := (_T('StrFájlokLetöltése'));
    Application.ProcessMessages;
    if not Download then break;
    if CheckCancel then break;

    FState := (_T('StrFájlokKibontása'));
    if not Extract then break;
    break;
  until false;

  FState := (_T('StrÜresjárat'));
  Log(_T('StrIdeiglenesFájlokEl'));
  if FileExists(FEmulator) then
    DeleteFile(FEmulator);
  if FileExists(FRoms) then
    DeleteFile(FRoms);
  if FileExists(FSource) then
    DeleteFile(FSource);
  FProgress := (0);
  FFileName := ('-');

  if FMode = 0 then
    Synchronize(procedure
                begin
                  Form.Close
                end)
  else
    Synchronize(procedure
                begin
                  Form.Button1.Caption := _T('StrKilépés');
                end);

  Terminate;
  EndThread(0);
end;

procedure TUpdateThread.Log(const S: string);
begin
  Synchronize(procedure
              begin
                Form.LogBox.ItemIndex := Form.LogBox.Items.Add(S);
              end);
end;


procedure TUpdateThread.LogFmt(const S: string; const Args: array of const);
begin
  Log(format(S, Args));
end;

function TUpdateThread.MessageBox(const Text: string; const Flags: longword): longword;
var
  RetVal: integer;
begin
  Synchronize(procedure
              begin
                RetVal := Windows.MessageBox(Form.Handle, PChar(Text), PChar(StrWinBox), Flags);
              end);
  Result := RetVal;
end;

function TUpdateThread.MessageBoxFmt(const Text: string;
  const Args: array of const; const Flags: longword): longword;
begin
  Result := MessageBox(format(Text, Args), Flags);
end;

procedure TUpdateThread.OnZipProgress(Sender: TObject; FileName: string;
  Header: TZipHeader; Position: Int64);
begin
  FFileName := ExtractFileName(FileName);
end;

end.
