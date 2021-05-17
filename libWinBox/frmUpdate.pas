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
  uBaseProfile;

type
  IAutoUpdate = interface
    ['{768238E2-A485-4DE5-AEA8-51E506DD81BC}']
    function Execute(const ByCommand: boolean = false): boolean; stdcall;
    function HasUpdate: boolean; stdcall;
    function AutoUpdate: boolean; stdcall;
  end;

  TUpdateForm = class(TForm, IAutoUpdate)
    Label1: TLabel;
    LogBox: TListBox;
    Button1: TButton;
    lbState: TLabel;
    Progress: TProgressBar;
    Shape1: TShape;
    State: TLabel;
    lbFileName: TLabel;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LogBoxDblClick(Sender: TObject);
  private
    procedure WMStartup(var Msg: TMessage); message WM_USER;
  protected
    procedure Log(const S: string);
    procedure LogFmt(const S: string; const Args: array of const);
    function MessageBox(const Text: string; const Flags: longword): longword;
    function MessageBoxFmt(const Text: string; const Args: array of const; const Flags: longword): longword;
  public
    FMode, FAutoUpdate, FGetSource: integer;
    FRepositories: array [0..1] of string;
    FPaths: array [0..0] of string;
    FCancelled: boolean;
    FBuild: integer;
    FEmulator, FRoms, FSource: string;

    function CheckCancel: boolean;
    function VersionCheck: boolean;
    function Download: boolean;
    function Extract: boolean;
    function Execute(const ByCommand: boolean = false): boolean; stdcall;
    function HasUpdate: boolean; stdcall;
    function AutoUpdate: boolean; stdcall;

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
  StrAJelenlegiVerzióN = 'A jelenlegi verzió nem lekérhetõ. Kívánja folytatn' +
  'i?';
  StrJelenlegiVerzióS = 'Jelenlegi 86Box verzió: %s';
  StrLegfrisebbVerzióKe = 'Frissítések keresése itt: %s';
  StrElérhetõVerzióS = 'Elérhetõ 86Box verzió: %s';
  StrATelepítettVerzió = 'A telepített verzió azonos vagy frissebb mint a le' +
  'tölthetõ.'#13#10'Kívánja mégis letölteni?';
  StrFrissítésElérhetõ = 'Frissítés elérhetõ (build: %d, dátum: %s).'#13#10'Kívánja le' +
  'tölteni?';
  EATávoliKiszolgáló = 'A távoli kiszolgáló nem elérhetõ, ellenõrizze az int' +
  'ernetkapcsolatot!';
  StrJelenlegNincsEgyet = 'Jelenleg nincs egyetlen telepített verzió sem.';
  StrAFolyamatotAFelha = 'A folyamatot a felhasználó megszakította.';
  StrKilépés = '&Kilépés';
  StrLegfrisebbVáltozáso = 'Legfrisebb változások: ';
  StrVerziókEllenõrzése = 'Verziók ellenõrzése...';
  StrÜresjárat = 'Üresjárat';
  StrMegszakítás = '&Megszakítás';
  StrFájlokLetöltése = 'Fájlok letöltése...';
  StrFájlokKibontása = 'Fájlok kibontása...';
  StrIdeiglenesFájlokEl = 'Ideiglenes fájlok eltávolítása...';
  StrHibaTörténtAFájlo = 'Hiba történt a fájlok kibontása során.';
  StrÁtnevezésKözbenHib = 'Átnevezés közben hiba történt.';
  StrKönyvtárÁtnevezése = 'Könyvtár átnevezése: 86Box-master';
  StrKönyvtárÁtnevezés2 = 'Könyvtár átnevezése: roms-master';
  StrForráskódKibontása = 'Forráskód kibontása...';
  StrDFájlKibontva = '%d fájl kibontva.';
  StrROMképekKibontása = 'ROM-képek kibontása...';
  StrBinárisokKibontása = 'Binárisok kibontása...';
  StrHibaTörténtAKoráb = 'Hiba történt a korábbi verzió eltávolítása során.';
  StrAKorábbiVerzióElt = 'A korábbi verzió eltávolítása...';
  StrHibaTörténtAFájl2 = 'Hiba történt a fájlok letöltése során.';
  StrForráskódLetöltése = 'Forráskód letöltése ide: "%s"';
  StrForráskódLetöltés2 =  'Forráskód letöltése innen: "%s"';
  StrROMképekFájlainak = 'ROM-képek fájlainak letöltése ide: "%s"';
  StrROMképekLetöltése = 'ROM-képek letöltése innen: "%s"';
  StrAzEmulátorFájlaina = 'Az emulátor fájlainak letöltése ide: "%s"';

{$R *.dfm}

function TUpdateForm.AutoUpdate: boolean;
begin
  Result := FAutoUpdate <> 0;
end;

procedure TUpdateForm.Button1Click(Sender: TObject);
begin
  if Button1.Caption = StrKilépés then
    Close
  else
    FCancelled := true;
end;

function TUpdateForm.CheckCancel: boolean;
begin
  Result := FCancelled;
  if Result then begin
    Log(StrAFolyamatotAFelha);
    State.Caption := StrÜresjárat;
  end;
end;

function TUpdateForm.Download: boolean;
var
  Temp: string;
begin
  Result := true;

  try
    FEmulator := TPath.GetTempFileName;
    LogFmt(StrAzEmulátorFájlaina, [ExtractFileName(FEmulator)]);
    Temp := jenkinsGetBuild(FRepositories[0], FBuild);
    if Temp = '' then begin
      Log(EATávoliKiszolgáló);
      exit(false);
    end;
    httpsGet(Temp, FEmulator);
    Progress.Position := Progress.Position + 1;
    lbFileName.Caption := ExtractFileName(FEmulator);

    LogFmt(StrROMképekLetöltése, [Def86RomsRepo]);
    FRoms := TPath.GetTempFileName;
    LogFmt(StrROMképekFájlainak, [ExtractFileName(FRoms)]);
    gitClone(Def86RomsRepo, FRoms);
    Progress.Position := Progress.Position + 1;
    lbFileName.Caption := ExtractFileName(FRoms);

    if FGetSource <> 0 then begin
      LogFmt(StrForráskódLetöltés2, [Def86SrcRepo]);
      FSource := TPath.GetTempFileName;
      LogFmt(StrForráskódLetöltése, [ExtractFileName(FSource)]);
      gitClone(Def86SrcRepo, FSource);
      Progress.Position := Progress.Position + 1;
      lbFileName.Caption := ExtractFileName(FSource);
    end;
  except
    on E: Exception do begin
      Log(StrHibaTörténtAFájl2);
      Log(#9 + E.Message);
      Result := false;
      lbFileName.Caption := '-';
    end;
  end;
end;

function TUpdateForm.Execute(const ByCommand: boolean): boolean;
begin
  FMode := ord(ByCommand);
  Result := ShowModal = mrOK;
end;

function TUpdateForm.Extract: boolean;
var
  Root: string;
begin
  Result := true;
  Root := ExtractFilePath(FPaths[0]);
  Log(StrAKorábbiVerzióElt);
  if DirectoryExists(Root) and not DeleteToBin(ExcludeTrailingPathDelimiter(Root)) then begin
    Log(StrHibaTörténtAKoráb);
    exit(false);
  end;
  ForceDirectories(Root);

  try
    with TZipFile.Create do begin
      try
        OnProgress := OnZipProgress;
        Log(StrBinárisokKibontása);
        Open(FEmulator, zmRead);
        ExtractAll(Root);
        Log(format(StrDFájlKibontva, [length(FileInfos)]));
        Close;
        Progress.Position := Progress.Position + 1;

        Log(StrROMképekKibontása);
        Open(FRoms, zmRead);
        ExtractAll(Root);
        Log(format(StrDFájlKibontva, [length(FileInfos)]));
        Close;
        Progress.Position := Progress.Position + 1;

        if FGetSource <> 0 then begin
          Log(StrForráskódKibontása);
          Open(FSource, zmRead);
          ExtractAll(Root);
          Log(format(StrDFájlKibontva, [length(FileInfos)]));
          Close;
          Progress.Position := Progress.Position + 1;
        end;
      finally
        Free;
      end;
    end;

    Log(StrKönyvtárÁtnevezés2);
    if not RenameFile(Root + 'roms-master', Root + 'roms') then begin
      Log(StrÁtnevezésKözbenHib);
      Result := false;
    end;

    if FGetSource <> 0 then begin
      Log(StrKönyvtárÁtnevezése);
      if not RenameFile(Root + '86box-master', Root + 'source') then begin
        Log(StrÁtnevezésKözbenHib);
        Result := false;
      end;
    end;
  except
    on E: Exception do begin
      Log(StrHibaTörténtAFájlo);
      Log(#9 + E.Message);
      lbFileName.Caption := '-';
      Result := false;
    end;
  end;
end;

procedure TUpdateForm.FormCreate(Sender: TObject);
begin
  with TRegIniFile.Create(SRegRootKey, KEY_READ) do begin
    FRepositories[0] := ReadString(SRegConfigKey + '.86Box', StrRepository, Def86BoxRepo);
    FAutoUpdate := ReadInteger(SRegConfigKey + '.86Box', StrAutoUpdate, 1);
    FGetSource := ReadInteger(SRegConfigKey + '.86Box', StrDownloadSource, 0);
    FPaths[0] := ReadString(SRegConfigKey + '.86Box', StrExecutablePath,
      TProfile.DefExecutablePath + '86Box\86Box.exe');
    Free;

    if FGetSource <> 0 then
      Progress.Max := 6;
  end;
end;

procedure TUpdateForm.FormShow(Sender: TObject);
begin
  LogBox.Clear;
  FCancelled := false;
  Button1.Caption := StrMegszakítás;

  PostMessage(Handle, WM_USER, 0, 0);
  OnShow := nil;
end;

function TUpdateForm.HasUpdate: boolean;
begin
  Result := (not FileExists(FPaths[0])) or
     (jenkinsCheckUpdate(FRepositories[0], GetFileTime(FPaths[0])));
end;

procedure TUpdateForm.Log(const S: string);
begin
  LogBox.ItemIndex := LogBox.Items.Add(S);
end;

procedure TUpdateForm.LogBoxDblClick(Sender: TObject);
begin
  if LogBox.ItemIndex <> -1 then
    ShowMessage(LogBox.Items[LogBox.ItemIndex]);
end;

procedure TUpdateForm.LogFmt(const S: string; const Args: array of const);
begin
  LogBox.ItemIndex := LogBox.Items.Add(format(S, Args));
end;

function TUpdateForm.MessageBox(const Text: string; const Flags: longword): longword;
begin
  Result := Windows.MessageBox(Handle, PChar(Text), PChar(StrWinBox), Flags);
end;

function TUpdateForm.MessageBoxFmt(const Text: string;
  const Args: array of const; const Flags: longword): longword;
begin
  Result := Windows.MessageBox(Handle, PChar(format(
    Text, Args)), PChar(StrWinBox), Flags);
end;

procedure TUpdateForm.OnZipProgress(Sender: TObject; FileName: string;
  Header: TZipHeader; Position: Int64);
begin
  lbFileName.Caption := ExtractFileName(FileName);

  Application.ProcessMessages;
end;

function TUpdateForm.VersionCheck: boolean;
var
  DateLocal, DateOnline: TDateTime;
  I: integer;
begin
  lbFileName.Caption := '-';
  if FileExists(FPaths[0]) then begin
    DateLocal := GetFileTime(FPaths[0]);
    if DateLocal = 0 then
      Result := MessageBox(StrAJelenlegiVerzióN, MB_ICONQUESTION or MB_YESNO) = mrYes
    else begin
      LogFmt(StrJelenlegiVerzióS, [DateTimeToStr(DateLocal)]);
      LogFmt(StrLegfrisebbVerzióKe, [FRepositories[0]]);
      try
         FBuild := jenkinsLastBuild(FRepositories[0]);
         if FBuild <> -1 then begin
           DateOnline := jenkinsGetDate(FRepositories[0], FBuild);
           LogFmt(StrElérhetõVerzióS, [DateTimeToStr(DateOnline)]);
           Result := CompareDate(DateLocal, DateOnline) < 0;
           if not Result then begin
             Result := (FMode = 1) and (MessageBox(
               StrATelepítettVerzió,
               MB_ICONWARNING or MB_YESNO) = mrYes);
             if not Result then begin
               FCancelled := true;
               CheckCancel;
             end;
           end
           else with jenkinsGetChangelog(FRepositories[0], FBuild) do begin
             Log(StrLegfrisebbVáltozáso);
             for I := 0 to Count - 1 do
               Log(#9 + Strings[I]);
             Result := MessageBoxFmt(StrFrissítésElérhetõ,
                 [FBuild, DateTimeToStr(DateOnline)],
                 MB_ICONQUESTION or MB_YESNO) = mrYes;
             if not Result then begin
               FCancelled := true;
               CheckCancel;
             end;
             Free;
           end;
         end
         else begin
           Log(EATávoliKiszolgáló);
           Result := false;
         end;
      except
         Log(EATávoliKiszolgáló);
         Result := false;
      end;
    end;
  end
  else begin
    Result := true;
    Log(StrJelenlegNincsEgyet);
    FBuild := jenkinsLastBuild(FRepositories[0]);
  end;
end;

procedure TUpdateForm.WMStartup(var Msg: TMessage);
begin
  repeat
    State.Caption := StrVerziókEllenõrzése;
    if not VersionCheck then break;
    if CheckCancel then break;

    State.Caption := StrFájlokLetöltése;
    Application.ProcessMessages;
    if not Download then break;
    if CheckCancel then break;

    State.Caption := StrFájlokKibontása;
    if not Extract then break;
    break;
  until false;

  State.Caption := StrÜresjárat;
  Log(StrIdeiglenesFájlokEl);
  if FileExists(FEmulator) then
    DeleteFile(FEmulator);
  if FileExists(FRoms) then
    DeleteFile(FRoms);
  if FileExists(FSource) then
    DeleteFile(FSource);
  Progress.Position := 0;
  lbFileName.Caption := '-';

  if FMode = 0 then
    Close
  else
    Button1.Caption := StrKilépés;
end;

end.
