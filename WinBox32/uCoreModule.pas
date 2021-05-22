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

unit uCoreModule;

interface

uses
  Windows, SysUtils, Classes, Forms, u86Box, uWinBox, Generics.Collections,
  ExtCtrls, Actions, ActnList, ShellAPI, Vcl.Menus,ImageList, ImgList,
  Controls, Dialogs, ExtDlgs, Graphics, uBaseProfile, uProcProfile,
  VCLTee.Chart, uFolderMon, uWinBoxLib, ExtActns, StdActns, IOUtils,
  Vcl.VirtualImageList, Vcl.BaseImageCollection, Vcl.ImageCollection,
  Registry;

type
  TCore = class(TDataModule)
    MonitorTimer: TTimer;
    Actions: TActionList;
    acWorkDir: TAction;
    acPrinterDir: TAction;
    acScreenshotDir: TAction;
    acStart: TAction;
    acStop: TAction;
    acPause: TAction;
    acReset: TAction;
    acSettings: TAction;
    acStopForce: TAction;
    acCtrlAltDel: TAction;
    acCtrlAltEsc: TAction;
    acUpdateList: TAction;
    acAbout: TAction;
    acChangeColor: TAction;
    acResetColor: TAction;
    acBringToFront: TAction;
    acUpdateVMPage: TAction;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    acStart2: TMenuItem;
    acPause1: TMenuItem;
    N5: TMenuItem;
    acCtrlAltDel1: TMenuItem;
    acCtrlAltEsc1: TMenuItem;
    acReset1: TMenuItem;
    N6: TMenuItem;
    acStart1: TMenuItem;
    acStopForce1: TMenuItem;
    N7: TMenuItem;
    acSettings1: TMenuItem;
    N2: TMenuItem;
    acScreenshotDir1: TMenuItem;
    acPrinterDir1: TMenuItem;
    N4: TMenuItem;
    acWorkDir1: TMenuItem;
    Nzet1: TMenuItem;
    acBringToFront1: TMenuItem;
    acUpdateVMPage1: TMenuItem;
    N3: TMenuItem;
    acChangeColor1: TMenuItem;
    acResetColor1: TMenuItem;
    MenuItem2: TMenuItem;
    acUpdateList1: TMenuItem;
    Eszkzk1: TMenuItem;
    Processzorterhels1: TMenuItem;
    Megjelents1: TMenuItem;
    N8: TMenuItem;
    Nyomtats1: TMenuItem;
    Exportlsbitkpknt1: TMenuItem;
    Exportlsmetafjlknt1: TMenuItem;
    Processzorterhels2: TMenuItem;
    Megjelents2: TMenuItem;
    N9: TMenuItem;
    Nyomtats2: TMenuItem;
    Exportlsbitkpknt2: TMenuItem;
    Exportlsmetafjlknt2: TMenuItem;
    Processzorterhels3: TMenuItem;
    Megjelents3: TMenuItem;
    N10: TMenuItem;
    Nyomtats3: TMenuItem;
    Exportlsbitkpknt3: TMenuItem;
    Exportlsmetafjlknt3: TMenuItem;
    Sg1: TMenuItem;
    acAbout1: TMenuItem;
    PrintDialog: TPrintDialog;
    SaveBmp: TSavePictureDialog;
    SaveEmf: TSavePictureDialog;
    ColorDialog: TColorDialog;
    VMMenu: TPopupMenu;
    Virtulisgpindtsa1: TMenuItem;
    Virtulisgpszneteltetse1: TMenuItem;
    N14: TMenuItem;
    CtrlAltDelkldse1: TMenuItem;
    CtrlAltEsckldse1: TMenuItem;
    Virtulisgphardveresjraindtsa1: TMenuItem;
    N15: TMenuItem;
    Virtulisgplelltsa1: TMenuItem;
    Virtulisgpknyszertettlelltsa1: TMenuItem;
    N16: TMenuItem;
    Hardverbelltsok1: TMenuItem;
    N17: TMenuItem;
    Virtulisgpeltrbehozsa1: TMenuItem;
    Virtulisgpinformcikfrisstse1: TMenuItem;
    N19: TMenuItem;
    Httrsznmegvltoztatsa1: TMenuItem;
    Httrsznvisszalltsa1: TMenuItem;
    N21: TMenuItem;
    Kpernykpekmegnyitsa1: TMenuItem;
    Nyomtattlcamegnyitsa1: TMenuItem;
    N18: TMenuItem;
    Munkaknyvtrmegnyitsa1: TMenuItem;
    PerfMenu: TPopupMenu;
    Processzorterhels4: TMenuItem;
    Megjelents4: TMenuItem;
    N11: TMenuItem;
    Nyomtats4: TMenuItem;
    Exportlsbitkpknt4: TMenuItem;
    Exportlsmetafjlknt4: TMenuItem;
    Lefoglaltmemria1: TMenuItem;
    Megjelents5: TMenuItem;
    N12: TMenuItem;
    Nyomtats5: TMenuItem;
    Exportlsbitkpknt5: TMenuItem;
    Exportlsmetafjlknt5: TMenuItem;
    Futvirtulisgpekszma1: TMenuItem;
    Megjelents6: TMenuItem;
    N13: TMenuItem;
    Nyomtats6: TMenuItem;
    Exportlsbitkpknt6: TMenuItem;
    Exportlsmetafjlknt6: TMenuItem;
    FolderMonitor: TFolderMonitor;
    acProfileDlg: TAction;
    Profilbelltsok1: TMenuItem;
    Profilbelltsok2: TMenuItem;
    acDiskCatalog: TAction;
    acAutoUpdate: TAction;
    Merevlemezadatbzismegnyitsa1: TMenuItem;
    Emultorfrisstsekkeresse1: TMenuItem;
    N20: TMenuItem;
    acb86Box: TBrowseURL;
    acbDOSBox: TBrowseURL;
    acbWinWorldPC: TBrowseURL;
    acbAllBootDisks: TBrowseURL;
    acbLinuxDistros: TBrowseURL;
    Dokumentci86Box1: TMenuItem;
    DokumentciDOSBox1: TMenuItem;
    N22: TMenuItem;
    N23: TMenuItem;
    LetltsekAllBootDisks1: TMenuItem;
    LetltsekWinWorldPC1: TMenuItem;
    LetltsekLinuxDistros1: TMenuItem;
    acbClassicDOSGames: TBrowseURL;
    ClassicDOSGamesrgijtkletltsek1: TMenuItem;
    Fjl1: TMenuItem;
    acQuit: TFileExit;
    KilpsaWinBoxReloadedalkalmazsbl1: TMenuItem;
    N24: TMenuItem;
    acNewHDD: TAction;
    jvirtulismerevlemezltrehozsa1: TMenuItem;
    acStopAll: TAction;
    acStopAllForce: TAction;
    N25: TMenuItem;
    Virtulisgplelltsa2: TMenuItem;
    Azsszesvirtulisgpknyszertettlelltsa1: TMenuItem;
    acNewVM: TAction;
    acDeleteVM: TAction;
    jvirtulisgpltrehozsa1: TMenuItem;
    N26: TMenuItem;
    Virtulisgpeltvoltsa1: TMenuItem;
    N27: TMenuItem;
    Akijelltvirtulisgpeltvoltsa1: TMenuItem;
    HomeMenu: TPopupMenu;
    jvirtulisgpltrehozsa2: TMenuItem;
    jvirtulismerevlemezltrehozsa2: TMenuItem;
    N28: TMenuItem;
    Azsszesvirtulisgplelltsa1: TMenuItem;
    Virtulisgplelltsa3: TMenuItem;
    N29: TMenuItem;
    Akijelltvirtulisgpeltvoltsa2: TMenuItem;
    N30: TMenuItem;
    Virtulisgpeklistjnakfrisstse1: TMenuItem;
    Emultorfrisstsekkeresse2: TMenuItem;
    KilpsaWinBoxReloadedalkalmazsbl2: TMenuItem;
    N32: TMenuItem;
    ImageCollection: TImageCollection;
    Icons16: TVirtualImageList;
    Icons32: TVirtualImageList;
    acProgSettings: TAction;
    Programbelltsok1: TMenuItem;
    N1: TMenuItem;
    ToolsSep2: TMenuItem;
    acUpdateTools: TAction;
    Felhasznlieszkzklistjnakfrisstse1: TMenuItem;
    UserTools: TMenuItem;
    ToolsSep: TMenuItem;
    acVogonsDrivers: TBrowseURL;
    BrowseURL1: TMenuItem;
    miDebug: TMenuItem;
    dbgCmdlWorkDir: TMenuItem;
    dbgLogMonitor: TMenuItem;
    N31: TMenuItem;
    Naplzsafolyamatmend1: TMenuItem;
    Naplzsafolyamat1: TMenuItem;
    N33: TMenuItem;
    Folyamatlistakiratsa1: TMenuItem;
    Frisstsekmonitorozsa1: TMenuItem;
    procedure MonitorTimerTimer(Sender: TObject);
    procedure ReloadProfiles(Sender: TObject);
    procedure acDirExecute(Sender: TObject);
    procedure acDirUpdate(Sender: TObject);
    procedure acVMExecute(Sender: TObject);
    procedure acVMUpdate(Sender: TObject);
    procedure acUpdateVMPageExecute(Sender: TObject);
    procedure btnChartClick(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
    procedure FolderMonitorChange(Sender: TObject; FileName: string;
      ChangeType: Cardinal);
    procedure acDiskCatalogExecute(Sender: TObject);
    procedure acAutoUpdateExecute(Sender: TObject);
    procedure acNewHDDExecute(Sender: TObject);
    procedure acStopAllUpdate(Sender: TObject);
    procedure acStopAllExecute(Sender: TObject);
    procedure acNewVMExecute(Sender: TObject);
    procedure acDeleteVMExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure acProgSettingsExecute(Sender: TObject);
    procedure ReloadTools(Sender: TObject);
    procedure acAutoUpdateUpdate(Sender: TObject);
    procedure miDebugFeature(Sender: TObject);
  private
    FRelProf: TNotifyEvent;
    FUpdate: TNotifyEvent;
  public
    ItemIndex: integer;
    FirstUpdate: boolean;
    Monitor: TWinBoxMonitor;
    Profiles: TObjectList<TWinBoxProfile>;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MonitorUpdate(Sender: TObject);
    procedure ToolClick(Sender: TObject);

    function IsAllStopped: boolean;
    function ValidateIndex: boolean;

    procedure EnableUI(const Value: boolean);

    property OnReloadProfiles: TNotifyEvent read FRelProf write FRelProf;
    property OnMonitorUpdate: TNotifyEvent read FUpdate write FUpdate;
  end;

var
  Core: TCore;

resourcestring
  StrWinBox = 'WinBox';

var
  UpdateLogging: boolean = false;

implementation

uses frmMainForm, uCommUtil, frmProgSettDlg, frmAbout, uProcessMon, frmSplash;

resourcestring
  StrEnsureHardStopCmd = 'A virtuális gép(ek) ilyen módon történõ leállítása adatvesztést okozhat. Biztos benne?';
  StrEgyébLemezképek = 'Egyéb lemezképek';
  StrImg = '.img';
  StrVdisk2dimg = 'vdisk%.2d.img';
  StrÚjVirtuálisGépD = 'Új virtuális gép #%d';
  AskDelete = 'A "%s" nevû virtuális gép eltávolítására készül. A virtuális géphez a munkakönyvtárban %s mennyiségû fájl tartozik. ' +
              #13#10'Kívánja ezeket a fájlokat is eltávolítani?'#13#10 +
              #13#10'Igen: A virtuális gép és a fájlok egyidejû törlése.'+
              #13#10'Nem: A virtuális gép törlése, a fájlok megtartása mellett.'+
              #13#10'Mégse: A törlési mûvelet megszakítása.' +
              #13#10#13#10'A mûvelet nem visszafordítható mûvelet, célszerû elõtte biztonsági másolatot készíteni.';
  SUnableToDelete = 'A fájlok eltávolítása nem sikerült, mivel használatban vannak. Ezeket késõbb majd kézzel el kell távolítani.'#13#10#13#10 +
                    'Kívánja megnyitni a munkakönyvtárat?';


{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TCore.ReloadProfiles(Sender: TObject);
var
  I: Integer;
begin
  FolderMonitor.Active := false;
  ItemIndex := -1;

  Profiles.Clear;
  T86Box.AddProfiles(Profiles, Monitor);
  SortProfiles(Profiles);

  //PageControl hivatkozások betöltése osztály szerint
  for I := 0 to Profiles.Count - 1 do
    if Profiles[I] is T86BoxProfile then
      Profiles[I].Tag := 2;

  if Assigned(FRelProf) then
    FRelProf(Self);
end;

function TCore.ValidateIndex: boolean;
begin
  Result := (ItemIndex >= 0) and (ItemIndex < Profiles.Count);
end;

procedure TCore.acAboutExecute(Sender: TObject);
begin
  AboutFrm.ShowModal;
end;

procedure TCore.acAutoUpdateExecute(Sender: TObject);
begin
  if Assigned(WinBoxMain) then
    CreateAutoUpdate(WinBoxMain).Execute(true);
end;

procedure TCore.acAutoUpdateUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not FirstUpdate and IsAllStopped;
end;

procedure TCore.acDeleteVMExecute(Sender: TObject);
var
  List: TStringList;
  WorkDir: string;
begin
  if ValidateIndex then begin
     List := TStringList.Create;
     WorkDir := '';
     case MessageBox(0, PChar(format(AskDelete,
        [Profiles[ItemIndex].FriendlyName,
         FileSizeToStr(GetFiles(Profiles[ItemIndex].WorkingDirectory, true, List), 2)])),
           PChar(StrWinBox), MB_ICONWARNING or MB_YESNOCANCEL) of
       mrYes: begin
                WorkDir := ExcludeTrailingPathDelimiter(
                              Profiles[ItemIndex].WorkingDirectory);
                (WinBoxMain.Pages.ActivePage.Controls[0] as IWinBoxFrame).ReleaseFiles;
                TProfile.DeleteProfile(Profiles[ItemIndex].ProfileID,
                                      Profiles[ItemIndex].SectionKey);
              end;
       mrNo: TProfile.DeleteProfile(Profiles[ItemIndex].ProfileID,
                                    Profiles[ItemIndex].SectionKey);
     end;
     List.Free;
     ReloadProfiles(Self);

     if (WorkDir <> '') then begin
       if not DeleteToBin(WorkDir) and (MessageBox(0, PChar(SUnableToDelete),
         PChar(StrWinBox), MB_ICONINFORMATION or MB_YESNO) = mrYes) then
           ShellExecute(0, 'open', PChar(WorkDir), nil, nil, SW_SHOWNORMAL);
     end;
  end;
end;

procedure TCore.acDirExecute(Sender: TObject);
begin
  ForceDirectories((Sender as TAction).Hint);
  ShellExecute(0, 'open', PChar((Sender as TAction).Hint), nil, nil, SW_SHOWNORMAL);
end;

procedure TCore.acDirUpdate(Sender: TObject);
begin
  with Sender as TAction do
    Enabled := ValidateIndex and
      (((Hint <> '') and DirectoryExists(Hint)) or (Tag = 1));
end;

procedure TCore.acDiskCatalogExecute(Sender: TObject);
begin
  if Assigned(WinBoxMain) then
    CreateSelectHDD(WinBoxMain).Execute;
end;

procedure TCore.acNewHDDExecute(Sender: TObject);
var
  I: integer;
  Directory, AFileName, Temp: string;
  UID: TGUID;
begin
  if ValidateIndex then
    Directory := IncludeTrailingPathDelimiter(Profiles[ItemIndex].WorkingDirectory)
  else
    Directory := DefWorkingRootDirectory + StrEgyébLemezképek + PathDelim;

  AFileName := '';
  for I := 0 to 99 do begin
    Temp := format(StrVdisk2dimg, [I]);
    if not FileExists(Directory + Temp) then begin
      AFileName := Temp;
      break;
    end;
  end;

  if AFileName = '' then begin
    CreateGUID(UID);
    AFileName := GUIDToString(UID) + StrImg;
  end;

  if Assigned(WinBoxMain) then
    with CreateWizardHDD(WinBoxMain) do begin
      FileName := PChar(Directory + AFileName);
      Execute(true);
    end;
end;

procedure TCore.acNewVMExecute(Sender: TObject);
var
  I: integer;
begin
  with CreateWizardVM(Self) do begin
    FriendlyName := PChar(format(StrÚjVirtuálisGépD, [Profiles.Count + 1]));
    if Execute(true) then begin
      ReloadProfiles(Self);
      I := FindProfileByID(ProfileID, Profiles);
      if Assigned(WinBoxMain) then begin
        WinBoxMain.List.ItemIndex := I + 2;
        WinBoxMain.ListClick(Self);
      end;
      if OpenSettings and ValidateIndex then
        Profiles[ItemIndex].SettingsDlg;
    end;
  end;
end;

procedure TCore.acProgSettingsExecute(Sender: TObject);
begin
  if Assigned(WinBoxMain) then
    with TProgSettDlg.Create(WinBoxMain) do
      try
        ShowModal;
        ReloadTools(Self);
      finally
        Free;
      end;
end;

procedure TCore.acStopAllExecute(Sender: TObject);
var
  P: TWinBoxProfile;
begin
  if ((Sender as TComponent).Tag = 2) and
     (MessageBox(0, PChar(StrEnsureHardStopCmd), PChar(StrWinBox),
              MB_ICONWARNING or MB_YESNO) <> mrYes) then
                exit;

  for P in Profiles do
    case (Sender as TComponent).Tag of
      1: P.Close;
      2: P.Terminate(true);
    end;
end;

procedure TCore.acStopAllUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not IsAllStopped;
end;

procedure TCore.ReloadTools(Sender: TObject);
var
  L: TStringList;
  I: integer;

  M: TMenuItem;
begin
  UserTools.Clear;
  L := TStringList.Create;
  with TRegIniFile.Create(SRegRootKey, KEY_READ) do
    try
      ReadSectionValues(SRegConfigKey + '.Tools', L);
      for I := L.Count - 1 downto 0 do begin
        M := TMenuItem.Create(UserTools);
        M.Caption := L.Names[I];
        M.Hint := L.ValueFromIndex[I];
        M.OnClick := ToolClick;

        UserTools.Add(M);
      end;
      UserTools.Visible := L.Count > 0;
      ToolsSep.Visible := UserTools.Visible;
    finally
      L.Free;
    end;
end;

procedure TCore.ToolClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    ShellExecute(0, 'open', PChar((Sender as TMenuItem).Hint), nil, nil, SW_SHOWNORMAL);
end;

procedure TCore.acUpdateVMPageExecute(Sender: TObject);
begin
  if ValidateIndex then
    (WinBoxMain.Pages.ActivePage.Controls[0] as IWinBoxFrame).UpdateFull;
end;

procedure TCore.acVMExecute(Sender: TObject);
begin
  if ValidateIndex then
    with Profiles[ItemIndex] do
      case (Sender as TAction).Tag of
        2: Start;
        3: Close;
        4: Execute(IDM_ACTION_PAUSE);
        5: Execute(IDM_ACTION_HRESET);
        6: SettingsDlg;
        7: if MessageBox(Handle, PChar(StrEnsureHardStopCmd), PChar(StrWinBox),
                MB_ICONWARNING or MB_YESNO) = mrYes then
                  Terminate(true);
        8: Execute(IDM_ACTION_RESET_CAD);
        9: Execute(IDM_ACTION_CTRL_ALT_ESC);
        10: if Assigned(WinBoxMain) then begin
              ColorDialog.Color := ColorToRGB(Color);
              if ColorDialog.Execute then begin
                Color := ColorDialog.Color;
                SaveProfile;
                (WinBoxMain.Pages.ActivePage.Controls[0] as IWinBoxFrame).UpdateFull;
              end;
            end;
        11: if Assigned(WinBoxMain) then begin
              Color := clNone;
              SaveProfile;
              (WinBoxMain.Pages.ActivePage.Controls[0] as IWinBoxFrame).UpdateFull;
            end;
        12: BringToFront;
        13: if ProfileDlg then
              ReloadProfiles(Self);
      end;
end;

procedure TCore.acVMUpdate(Sender: TObject);
var
  S: integer;
begin
  with (Sender as TAction) do begin
    Enabled := ValidateIndex;
    if Enabled then
      with Profiles[ItemIndex] do begin
        S := State;
        case (Sender as TAction).Tag of
          2: Enabled := S = PROCESS_STATE_STOPPED;
          4: begin
               Enabled := (S = PROCESS_STATE_RUNNING) or (S = PROCESS_STATE_PAUSED);
               Checked := (S = PROCESS_STATE_PAUSED);
             end;
          7: Enabled := (S = PROCESS_STATE_RUNNING) or
                        (S = PROCESS_STATE_ERROR_MULTINST) or
                        (S = PROCESS_STATE_PAUSED);
          3, 5, 8, 9:
             Enabled := (S = PROCESS_STATE_RUNNING);
          12:
             Enabled := Count > 0;
          13:
             Enabled := (S = PROCESS_STATE_STOPPED);
        end;
      end;
  end;
end;

constructor TCore.Create(AOwner: TComponent);
begin
  inherited;
  FirstUpdate := true;

  ReloadTools(Self);

  Profiles := TObjectList<TWinBoxProfile>.Create;
  ItemIndex := -1;

  Monitor := TWinBoxMonitor.Create;
  Monitor.OnUpdate := MonitorUpdate;

  ReloadProfiles(Self);
end;

procedure TCore.DataModuleCreate(Sender: TObject);
begin
  Icons16.SetSize(Icons16.Width * Screen.PixelsPerInch div 96,
                  Icons16.Height * Screen.PixelsPerInch div 96);
  Icons32.SetSize(Icons32.Width * Screen.PixelsPerInch div 96,
                  Icons32.Height * Screen.PixelsPerInch div 96);

  {$IFNDEF DEBUG}
  miDebug.Visible := IsDebuggerPresent;
  {$ELSE}
  miDebug.Visible := true;
  {$ENDIF}

  EnableUI(false);
end;

destructor TCore.Destroy;
begin
  Monitor.Free; //ezt KELL elõbb felszabadítani
  Profiles.Free;
  inherited;
end;

procedure TCore.EnableUI(const Value: boolean);
var
  I: Integer;
begin
  for I := 0 to Actions.ActionCount - 1 do
    if Actions[I].Tag = -1 then
      Actions[I].Enabled := Value;

  if Assigned(WinBoxMain) then
    WinBoxMain.List.Enabled := Value;

  if Value then
    Screen.Cursor := crDefault
  else
    Screen.Cursor := crAppStart;
end;

procedure TCore.FolderMonitorChange(Sender: TObject; FileName: string;
  ChangeType: Cardinal);
begin
  if ValidateIndex and Assigned(WinBoxMain) then
    (WinBoxMain.Pages.ActivePage.Controls[0] as IWinBoxFrame).FolderChange(
      Sender, FileName, ChangeType);
end;

function TCore.IsAllStopped: boolean;
var
  I: Integer;
begin
  Result := true;
  for I := 0 to Profiles.Count - 1 do
    if Profiles[I].Count > 0 then
      exit(false);
end;

procedure TCore.btnChartClick(Sender: TObject);
begin
  if Assigned(WinBoxMain) then
    with WinBoxMain, (Sender as TMenuItem) do
      case HelpContext of
        1: begin
             List.ItemIndex := 1;
             ListClick(List);
             pgCharts.ActivePageIndex := Tag;
           end;
        2: if PrintDialog.Execute then
             (pgCharts.Pages[Tag].Controls[0] as TChart).Print;
        3: if SaveBmp.Execute then
             (pgCharts.Pages[Tag].Controls[0] as TChart).SaveToBitmapFile(SaveBmp.FileName);
        4: if SaveEmf.Execute then
             with pgCharts.Pages[Tag].Controls[0] as TChart do begin
               if LowerCase(ExtractFileExt(SaveEmf.FileName)) = '.wmf' then
                 SaveToMetafile(SaveEmf.FileName)
               else
                 SaveToMetafileEnh(SaveEmf.FileName);
             end;
      end;
end;

procedure TCore.MonitorTimerTimer(Sender: TObject);
begin
  if not Monitor.IsUpdating then begin
    if UpdateLogging then
      dbgLog('Monitor Update Started');

    Monitor.Update;
  end;
end;

procedure TCore.MonitorUpdate(Sender: TObject);
begin
  if UpdateLogging then
    dbgLog('Monitor Update Responded');

  UpdatePIDs(Profiles);

  if FirstUpdate then begin
    FirstUpdate := false;
    WinBoxSplash.Close;
    WinBoxSplash.Free;

    with CreateAutoUpdate(nil) do begin
      if AutoUpdate and IsAllStopped and HasUpdate then
        Execute(false);
    end;
    EnableUI(true);
  end;

  if Assigned(FUpdate) then
    FUpdate(Self);

  if UpdateLogging then
    dbgLog('Monitor Update Done');
end;

procedure TCore.miDebugFeature(Sender: TObject);
var
  P: TProcess;
  I: integer;
begin
  case (Sender as TComponent).Tag of
    1: begin
         P.CommandLine := InputBox('P.CommandLine', 'P.CommandLine = ', '');
         ShowMessage(T86Box.GetWorkingDirectory(P));
       end;
    2: MonitorLogging := (Sender as TMenuItem).Checked;
    3: ProcessLogging := (Sender as TMenuItem).Checked;
    4: AssignmentLogging := (Sender as TMenuItem).Checked;
    5: for I := 0 to Monitor.Count - 1 do begin
         P := Monitor[I];
         dbgLogFmt('PID: %d, HWND: 0x%.8x, %s, %s',
           [P.ProcessID, P.Data, P.ExecutablePath, P.CommandLine]);
       end;
    6: UpdateLogging := (Sender as TMenuItem).Checked;
  end;
end;

end.
