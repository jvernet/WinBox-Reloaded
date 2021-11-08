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

unit frmWizardVM;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Vcl.Samples.Spin, ComCtrls, Vcl.Imaging.pngimage, ExtCtrls, FileCtrl,
  uBaseProfile, uVMSample, frmSelectHDD, frmWizardHDD, ShellAPI, IniFiles, Zip,
  uLang, Registry;

type
  IWizardVM = interface
    ['{1AD7EC11-8D44-4A44-8D73-13C29AA4A33C}']
    function GetProfileID: PChar; stdcall;
    function GetWorkingDirectory: PChar; stdcall;
    procedure SetWorkingDirectory(const Value: PChar); stdcall;
    function GetFriendlyName: PChar; stdcall;
    function GetDiskData: TDiskData; stdcall;
    function GetOpenSettings: boolean; stdcall;
    procedure SetFriendlyName(const Value: PChar); stdcall;

    procedure RecreateProfileID; stdcall;

    function Execute(const AutoCreate: boolean): boolean; stdcall;
    function TryCreate: boolean; stdcall;

    property DiskData: TDiskData read GetDiskData;
    property WorkingDirectory: PChar read GetWorkingDirectory write SetWorkingDirectory;
    property ProfileID: PChar read GetProfileID;
    property FriendlyName: PChar read GetFriendlyName write SetFriendlyName;
    property OpenSettings: boolean read GetOpenSettings;
  end;

  TWizardVM = class(TForm, IWizardVM, ILanguageSupport)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    TabSheet2: TTabSheet;
    Label3: TLabel;
    Label5: TLabel;
    TabSheet5: TTabSheet;
    Label25: TLabel;
    Label26: TLabel;
    TabSheet6: TTabSheet;
    Label4: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    Label6: TLabel;
    Label7: TLabel;
    TabSheet3: TTabSheet;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    lbOption1: TLabel;
    cbOption1: TComboBox;
    cbOption2: TComboBox;
    cbOption3: TComboBox;
    cbOption4: TComboBox;
    lbOption2: TLabel;
    lbOption3: TLabel;
    lbOption4: TLabel;
    TabSheet4: TTabSheet;
    Label11: TLabel;
    Label12: TLabel;
    cbHDD: TCheckBox;
    Label13: TLabel;
    Label14: TLabel;
    lbHDD: TLabel;
    lbCHS: TLabel;
    cbCDROM: TCheckBox;
    Label17: TLabel;
    lbCDROM: TLabel;
    Image2: TImage;
    Label19: TLabel;
    btnHDD: TButton;
    lbtxNoteCDROM: TLabel;
    lbNoteCDROM: TLabel;
    CheckBox1: TCheckBox;
    TabSheet7: TTabSheet;
    Label15: TLabel;
    Label16: TLabel;
    Edit1: TEdit;
    Label18: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Path: TEdit;
    Button3: TButton;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnHDDClick(Sender: TObject);
    procedure cbHDDClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FAutoCreate: boolean;
    ADiskData: TDiskData;
    ProfileID: string;
    function GetProfileID: PChar; stdcall;
    function GetWorkingDirectory: PChar; stdcall;
    function GetFriendlyName: PChar; stdcall;
    function GetDiskData: TDiskData; stdcall;
    function GetOpenSettings: boolean; stdcall;
    procedure SetWorkingDirectory(const Value: PChar); stdcall;
    procedure SetFriendlyName(const Value: PChar); stdcall;
  protected
    function Execute(const AutoCreate: boolean): boolean; stdcall;
    procedure RecreateProfileID; stdcall;
    function TryCreate: boolean; stdcall;
    function GetGeometry: TDiskGeometry;
    procedure UpdateCHS;
  public
    Samples: TVMSampleFilter;
    DiskTool: IWizardHDD;
    procedure FixItemIndex(Control: TCustomListControl);

    procedure GetTranslation(Language: TLanguage); stdcall;
    procedure Translate; stdcall;
  end;

var
  WizardVM: TWizardVM;

implementation

uses uCommUtil;

resourcestring
  StrSamples = '\Templates';

const
  Extensions: array [boolean] of string = ('img', 'vhd');

{$R *.dfm}

{ TWizardVM }

procedure TWizardVM.Button1Click(Sender: TObject);
begin
  if Sender = Button1 then
    case PageControl1.ActivePageIndex of
      1: begin
           if Edit1.Text = '' then
             raise Exception.Create(SysErrorMessage(ERROR_INVALID_PARAMETER));

           if (Path.Text = '') or (Path.Text = '\') then
             raise Exception.Create(SysErrorMessage(ERROR_PATH_NOT_FOUND));

           Path.Text := IncludeTrailingPathDelimiter(Path.Text);
         end;
      2: if ListBox2.ItemIndex = -1 then
           raise Exception.Create(SysErrorMessage(ERROR_INVALID_PARAMETER));
      5: if (not FAutoCreate) or TryCreate then
           ModalResult := mrOK
         else
           exit;
    end;

  PageControl1.ActivePageIndex := PageControl1.ActivePageIndex +
    (Sender as TComponent).Tag;

  Button2.Enabled := PageControl1.ActivePageIndex > 0;
  Button1.Enabled := PageControl1.ActivePageIndex < PageControl1.PageCount - 1;

  if ListBox2.ItemIndex <> -1 then
    with ListBox2.Items.Objects[ListBox2.ItemIndex] as TVMSample do
      case PageControl1.ActivePageIndex of
        3: begin
             if Sender = Button1 then begin
                 lbOption1.Caption := GetOptionName(0);
                 lbOption2.Caption := GetOptionName(1);
                 lbOption3.Caption := GetOptionName(2);
                 lbOption4.Caption := GetOptionName(3);
                 GetOptions(0, cbOption1.Items);
                 GetOptions(1, cbOption2.Items);
                 GetOptions(2, cbOption3.Items);
                 GetOptions(3, cbOption4.Items);
                 FixItemIndex(cbOption1);
                 FixItemIndex(cbOption2);
                 FixItemIndex(cbOption3);
                 FixItemIndex(cbOption4);
               end;
             if (not (cbOption1.Enabled or cbOption2.Enabled
                  or cbOption3.Enabled or cbOption4.Enabled))
                  and (Sender is TButton) then begin
                     (Sender as TButton).Click;
                     exit;
                  end;
           end;
        4: if Sender = Button1 then begin
             ADiskData := DiskDataHDD;

             cbHDD.Enabled := HasDefHDD;
             cbHDD.Checked := cbHDD.Enabled and OptionHDD;
             btnHDD.Enabled := cbHDD.Checked;
             if cbHDD.Enabled then begin
               with CreateSelectHDD(nil) do begin
                  DiskData := ADiskData;
                  LocatePhysCHS;
                  SetConnectorFilter(false);
                  SetCHSFilter(true);
                  if Execute(true) then
                    ADiskData := DiskData;
                end;

               DiskTool.DiskData := ADiskData;
               DiskTool.FileName := PChar(Path.Text + 'vdisk00.' + Extensions[DiskTool.GetIsVHD]);
               DiskTool.SetConnectorFilter(true);
             end;

             UpdateCHS;

             lbCDROM.Caption := DescOfCDROM;
             lbNoteCDROM.Caption := NoteForCDROM;

             cbCDROM.Enabled := HasDefCDROM;
             cbCDROM.Checked := cbCDROM.Enabled and OptionCDROM;
           end;
      end;
end;

procedure TWizardVM.Button3Click(Sender: TObject);
var
  Directory: string;
begin
  SysUtils.ForceDirectories(Path.Text);
  Directory := ExcludeTrailingPathDelimiter(Path.Text);
  if SelectDirectory(_T('StrValasszaKiAHaszna'), '', Directory, [sdNewUI], Self) then
    Path.Text := IncludeTrailingPathDelimiter(Directory);
end;

procedure TWizardVM.Button4Click(Sender: TObject);
begin
  SysUtils.ForceDirectories(Path.Text);
  ShellExecute(Handle, 'open', PChar(ExcludeTrailingPathDelimiter(Path.Text)),
    nil, nil, SW_SHOWNORMAL);
end;

procedure TWizardVM.cbHDDClick(Sender: TObject);
begin
  btnHDD.Enabled := cbHDD.Checked;
end;

procedure TWizardVM.btnHDDClick(Sender: TObject);
begin
  if ListBox2.ItemIndex <> -1 then
    with ListBox2.Items.Objects[ListBox2.ItemIndex] as TVMSample do begin
      if DiskTool.Execute(false) then begin
        ADiskData := DiskTool.DiskData;
        UpdateCHS;
      end
      else if String(DiskTool.FileName) = '' then
        DiskTool.FileName := PChar(Path.Text + 'vdisk00.' + Extensions[DiskTool.GetIsVHD]);
    end;
end;

function TWizardVM.Execute(const AutoCreate: boolean): boolean;
begin
  FAutoCreate := AutoCreate;
  Result := ShowModal = mrOK;
end;

procedure TWizardVM.FixItemIndex(Control: TCustomListControl);
begin
  if Control.GetCount > 0 then begin
    Control.ItemIndex := 0;
    Control.Enabled := true;
  end
  else begin
    Control.ItemIndex := -1;
    Control.Enabled := false;
  end;
end;

procedure TWizardVM.FormCreate(Sender: TObject);
begin
  LoadImage('BANNER_NEW', Image1);

  Samples := TVMSampleFilter.Create(true);
  DiskTool := CreateWizardHDD(Self);

  RecreateProfileID;
end;

procedure TWizardVM.FormDestroy(Sender: TObject);
begin
  Samples.Free;
end;

procedure TWizardVM.FormShow(Sender: TObject);
var
  Path: string;
begin
  Translate;

  Path := ExcludeTrailingPathDelimiter(
    ExtractFilePath(Application.ExeName)) + StrSamples;

  if not SysUtils.DirectoryExists(Path) then
      Path := IncludeTrailingPathDelimiter(
    GetEnvironmentVariable(SAppDataEnvVar)) + SDefaultAppDataFolder + StrSamples;

  PageControl1.ActivePageIndex := 0;
  Samples.Load(Path);
  Samples.GetManufacturers(ListBox1.Items);

  FixItemIndex(ListBox1);
  ListBox1Click(Self);
end;

function TWizardVM.GetDiskData: TDiskData;
begin
  Result := ADiskData;
end;

function TWizardVM.GetFriendlyName: PChar;
begin
  Result := PChar(Edit1.Text);
end;

function TWizardVM.GetGeometry: TDiskGeometry;
begin
  if not ADiskData.dgPhysicalGeometry.IsEmpty then
    Result := ADiskData.dgPhysicalGeometry
  else
    Result := ADiskData.dgTranslatedGeometry;
end;

function TWizardVM.GetOpenSettings: boolean;
begin
  Result := CheckBox1.Checked;
end;

function TWizardVM.GetProfileID: PChar;
begin
  Result := PChar(ProfileID);
end;

procedure TWizardVM.GetTranslation(Language: TLanguage);
begin
  if Assigned(Language) then
    Language.GetTranslation('WizardVM', Self);
end;

function TWizardVM.GetWorkingDirectory: PChar;
begin
  Result := PChar(Path.Text);
end;

procedure TWizardVM.ListBox1Click(Sender: TObject);
begin
  if ListBox1.ItemIndex >= 0 then
    Samples.GetByManufacturers(ListBox1.Items[ListBox1.ItemIndex], ListBox2.Items);

  FixItemIndex(ListBox2);
  ListBox2Click(Self);
end;

procedure TWizardVM.ListBox2Click(Sender: TObject);
begin
  if ListBox2.ItemIndex <> -1 then
    with ListBox2.Items.Objects[ListBox2.ItemIndex] as TVMSample do
      Label7.Caption := SupportedOS;
end;

procedure TWizardVM.RecreateProfileID;
var
  G: TGUID;
begin
  CreateGUID(G);
  ProfileID := GUIDToString(G);
  Path.Text := DefWorkingRootDirectory + ProfileID + PathDelim;
  Edit1.Text := ProfileID;
end;

procedure TWizardVM.SetFriendlyName(const Value: PChar);
begin
  Edit1.Text := Value;
end;

procedure TWizardVM.SetWorkingDirectory(const Value: PChar);
begin
  if (Value = '') or (Value = '\') then
    exit
  else
    Path.Text := IncludeTrailingPathDelimiter(Value);
end;

procedure TWizardVM.Translate;
begin
  if Assigned(Language) then
    Language.Translate('WizardVM', Self);
end;

function TWizardVM.TryCreate: boolean;
var
  Sample: TVMSample;
  Config: TIniFile;

  List: TStringList;
  I: integer;
  Key: HKEY;
begin
  //Result := false;

  if ListBox2.ItemIndex = -1 then
    raise Exception.Create(SysErrorMessage(ERROR_INVALID_PARAMETER));

  Sample := ListBox2.Items.Objects[ListBox2.ItemIndex] as TVMSample;

  with TZipFile.Create do
    try
      SysUtils.ForceDirectories(Path.Text);
      ExtractZipFile(Sample.FileName, Path.Text);
      //DeleteFile(Path.Text + 'winbox.inf');
      DeleteWithShell(Path.Text + 'winbox.*', false);

      Config := TIniFile.Create(Path.Text + Sample.ConfigFile);
      try
        if cbOption1.ItemIndex <> -1 then
          Sample.AddOption(0, cbOption1.Text, Config);
        if cbOption2.ItemIndex <> -1 then
          Sample.AddOption(1, cbOption2.Text, Config);
        if cbOption3.ItemIndex <> -1 then
          Sample.AddOption(2, cbOption3.Text, Config);
        if cbOption4.ItemIndex <> -1 then
          Sample.AddOption(3, cbOption4.Text, Config);

        Config.WriteString('WinBox', 'WindowSize',
            Sample.GetCustomKey('General', 'WindowSize', '960x720'));

        if LowerCase(Sample.EmulatorType) <> 'dosbox' then begin
          if cbCDROM.Checked then
            Sample.AddCDROM(Config);
        end;

        if (LowerCase(Sample.EmulatorType) = '86box') then begin
          if cbHDD.Checked then
            with GetGeometry do begin
              Config.WriteString('Hard disks', 'hdd_01_parameters',
                format('%d, %d, %d, 0, %s', [S, H, C, LowerCase(Sample.ConnectorHDD)]));
              Config.WriteString('Hard disks', 'hdd_01_fn',
                Copy(CompactFileNameTo(
                  DiskTool.FileName,
                  ExcludeTrailingPathDelimiter(Path.Text)), 3, MaxInt));
              if LowerCase(Sample.ConnectorHDD) = 'scsi' then
                Config.WriteString('Hard disks', 'hdd_01_scsi_id', Sample.SlotOfHDD)
              else
                Config.WriteString('Hard disks',
                  format('hdd_01_%s_channel', [LowerCase(Sample.ConnectorHDD)]),
                  Sample.SlotOfHDD);
              DiskTool.TryCreate;
            end;

          List := TStringList.Create;
          with TRegIniFile.Create(SRegRootKey) do
            try
              case ReadInteger(SRegConfigKey + '.86Box', StrAutoAppearance, 1) of
                0: ;
                1: begin
                     List.Text := StrDefaultAppearance;
                     List.Values['window_fixed_res'] :=
                       Sample.GetCustomKey('General', 'WindowSize', '960x720');
                   end;
                else if RegOpenKeyEx(CurrentKey, PChar(SRegConfigKey + '.86Box'), 0, Access, Key) = ERROR_SUCCESS then begin
                     RegReadMulti(Key, StrApperanceValues, List);
                     RegCloseKey(Key);

                     if List.Values['vid_resize'] = '2' then
                       List.Values['window_fixed_res'] :=
                          Sample.GetCustomKey('General', 'WindowSize', '960x720');
                   end;
              end;

              for I := 0 to List.Count - 1 do
                Config.WriteString('General', List.Names[I], List.ValueFromIndex[I]);
            finally
              Free;
              List.Free;
            end;
        end;

        List := Sample.GetRenameList;
        try
          for I := 0 to List.Count - 1 do
            RenameFile(Path.Text + List.Names[I],
                       Path.Text + List.ValueFromIndex[I]);
        finally
          List.Free;
        end;

        TProfile.CreateProfile(
          ProfileID,
          TProfile.DefExecutablePath + Sample.EmulatorType +
            PathDelim + Sample.EmulatorType + '.exe',
          Edit1.Text,
          Path.Text, SRegBaseKey + '.' + Sample.EmulatorType);
      finally
        Config.Free;
      end;
    finally
      Free;
    end;

  Result := true;
end;

procedure TWizardVM.UpdateCHS;
var
  Geometry: TDiskGeometry;
  HDType: string;
begin
  if ListBox2.ItemIndex <> -1 then
    with ListBox2.Items.Objects[ListBox2.ItemIndex] as TVMSample do
      HDType := ConnectorHDD
  else
    HDType := '';

  Geometry := GetGeometry;
  if cbHDD.Enabled then begin
    lbHDD.Caption := format('%s %s', [FileSizeToStr(Geometry.Size, 2, 1000),
      HDType]);
    with Geometry do
      lbCHS.Caption := format('C: %d H: %d S: %d', [C, H, S]);
  end
  else begin
    lbHDD.Caption := _T('StrNincs');
    lbCHS.Caption := lbHDD.Caption;
  end;
end;

end.
