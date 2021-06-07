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

unit frmProgSettDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.Imaging.pngimage, Vcl.ExtCtrls, ShellAPI, FileCtrl, uBaseProfile,
  Registry, uCoreModule, u86Box, IOUtils, Vcl.CheckLst, Math, IniFiles;

type
  TProgSettDlg = class(TForm)
    PageControl1: TPageControl;
    Button1: TButton;
    Button2: TButton;
    TabSheet2: TTabSheet;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Image3: TImage;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label2: TLabel;
    ComboBox1: TComboBox;
    Label3: TLabel;
    Path: TEdit;
    Button3: TButton;
    Button4: TButton;
    Label4: TLabel;
    Image1: TImage;
    Button5: TButton;
    TabSheet1: TTabSheet;
    Label5: TLabel;
    Image2: TImage;
    ListView1: TListView;
    Button6: TButton;
    Button7: TButton;
    GroupBox3: TGroupBox;
    Label6: TLabel;
    Edit1: TEdit;
    Label7: TLabel;
    Memo1: TMemo;
    Button8: TButton;
    Button9: TButton;
    GroupBox4: TGroupBox;
    Button10: TButton;
    OpenDialog1: TOpenDialog;
    Button11: TButton;
    TabSheet3: TTabSheet;
    GroupBox5: TGroupBox;
    Image4: TImage;
    Label8: TLabel;
    Label9: TLabel;
    RadioButton1: TRadioButton;
    Label10: TLabel;
    RadioButton2: TRadioButton;
    CheckListBox1: TCheckListBox;
    Label11: TLabel;
    ComboBox2: TComboBox;
    Label12: TLabel;
    ComboBox3: TComboBox;
    RadioButton3: TRadioButton;
    Memo2: TMemo;
    RadioButton4: TRadioButton;
    Button12: TButton;
    OpenDialog2: TOpenDialog;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure Button9Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure UpdateAppearancePage(Sender: TObject);
    procedure CustomAppearanceChange(Sender: TObject);
    procedure Button12Click(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  ProgSettDlg: TProgSettDlg;

implementation

uses uLang, uCommUtil;

resourcestring
  StrAutoUpdate = 'AutoUpdate';
  StrDownloadSource = 'DownloadSource';
  StrRepository = 'Repository';
  Def86BoxRepo = 'https://ci.86box.net/job/86Box';

type
  TCheckListKey = record
    Name: string;
    Default: integer;
  end;

const
  CheckListKeys: array [0..7] of TCheckListKey =
    (//(Name: 'window_remember';     Default: 0),
     //   Cause a deadlock since makes 86Box continously write to the INI file.
     //   WinBox can handle it, since it reads the INI when the monitor notifies
     //     a change, but ultimately slows down the VM.
     //   Can be re-enabled if this behaviour will be changed in the future,
     //     such as 86Box only write these values at e.g. closing.

     (Name: 'force_43';            Default: 0),
     (Name: 'enable_overscan';     Default: 0),
     (Name: 'dpi_scale';           Default: 1),
     (Name: 'video_filter_method'; Default: 1),
     (Name: 'vid_cga_contrast';    Default: 0),
     (Name: 'update_icons';        Default: 1),
     (Name: 'confirm_exit';        Default: 1),
     (Name: 'enable_discord';      Default: 0));

{$R *.dfm}

procedure TProgSettDlg.Button11Click(Sender: TObject);
begin
  Path.Text := IncludeTrailingPathDelimiter(
    TPath.GetDocumentsPath + PathDelim + _T('SDefaultDocumentsFolder'));
end;

procedure TProgSettDlg.Button12Click(Sender: TObject);
var
  Config: TMemIniFile;
begin
  with Core do
    if ValidateIndex and (Profiles[ItemIndex] is T86BoxProfile) then
      OpenDialog2.InitialDir := Profiles[ItemIndex].WorkingDirectory;

  if OpenDialog2.Execute then begin
    if FileExists(OpenDialog2.FileName) and
       CanLockFile(OpenDialog2.FileName, GENERIC_READ) then begin
           Config := TMemIniFile.Create(OpenDialog2.FileName, TEncoding.UTF8);
           try
             with Config do begin
               Memo2.Clear;
               Config.DeleteKey('General', 'window_fixed_res');
               Config.ReadSectionValues('General', Memo2.Lines);
               UpdateAppearancePage(Memo2);
             end;
           finally
             Config.Free;
           end;
         end
    else
      raise Exception.Create(_T('EOpenConfigLocked', [OpenDialog2.FileName]));
  end;
end;

procedure TProgSettDlg.Button2Click(Sender: TObject);
var
  I: integer;
  Key: HKEY;

  function GetScaleID: integer;
  begin
    if RadioButton4.Checked then
      Result := 0
    else if RadioButton2.Checked then
      Result := 2
    else if RadioButton3.Checked then
      Result := 3
    else
      Result := 1;
  end;

begin
  if (Path.Text = '') or (Path.Text = '\') then
    raise Exception.Create(SysErrorMessage(ERROR_PATH_NOT_FOUND));

  Path.Text := IncludeTrailingPathDelimiter(Path.Text);

  if Assigned(ListView1.Selected) then
    with ListView1.Selected do begin
      if ((Caption <> Edit1.Text) or
           ((SubItems.Count > 0) and (Memo1.Text <> SubItems[0]))) and
         (Edit1.Text <> '') and (Memo1.Text <> '') and
         (MessageBox(Handle, _P('StrModositasokTortente'),
           PChar(Application.Title), MB_YESNO or MB_ICONQUESTION) = mrYes) then
            Button10.Click;
    end
  else if (Edit1.Text <> '') and (Memo1.Text <> '') and
          (MessageBox(Handle, _P('StrModositasokTortente'),
            PChar(Application.Title), MB_YESNO or MB_ICONQUESTION) = mrYes) then
              Button9.Click;

  SysUtils.ForceDirectories(Path.Text);
  with TRegIniFile.Create(SRegRootKey) do
    try
      WriteString(SRegConfigKey, StrRootDirectory, Path.Text);
      WriteString(SRegConfigKey + '.86Box', StrRepository, ComboBox1.Text);
      WriteInteger(SRegConfigKey + '.86Box', StrAutoUpdate, ord(CheckBox1.Checked));
      WriteInteger(SRegConfigKey + '.86Box', StrDownloadSource, ord(CheckBox2.Checked));

      WriteInteger(SRegConfigKey + '.86Box', StrAutoAppearance, GetScaleID);

      if RegOpenKeyEx(CurrentKey, PChar(SRegConfigKey + '.86Box'), 0, Access, Key) = ERROR_SUCCESS then begin
        RegWriteMulti(Key, StrApperanceValues, Memo2.Lines);
        RegCloseKey(Key);
      end;

      EraseSection(SRegConfigKey + '.Tools');
      for I := 0 to ListView1.Items.Count - 1 do
        with ListView1.Items[I] do
          if SubItems.Count > 0 then
            WriteString(SRegConfigKey + '.Tools', Caption, SubItems[0]);
    finally
      Free;
    end;

  ModalResult := mrOK;
end;

procedure TProgSettDlg.Button3Click(Sender: TObject);
var
  Directory: string;
begin
  SysUtils.ForceDirectories(Path.Text);
  Directory := ExcludeTrailingPathDelimiter(Path.Text);
  if SelectDirectory(_T('StrValasszaKiAzUjVi'), '', Directory, [sdNewUI], Self) then
    Path.Text := IncludeTrailingPathDelimiter(Directory);
end;

procedure TProgSettDlg.Button4Click(Sender: TObject);
begin
  SysUtils.ForceDirectories(Path.Text);
  ShellExecute(Handle, 'open', PChar(ExcludeTrailingPathDelimiter(Path.Text)),
    nil, nil, SW_SHOWNORMAL);
end;

procedure TProgSettDlg.Button5Click(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(ExtractFilePath(T86BoxProfile.DefExecutablePath)),
    nil, nil, SW_SHOWNORMAL);
end;

procedure TProgSettDlg.Button8Click(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    if (Edit1.Text = '') or (WideLowerCase(
          ChangeFileExt(ExtractFileName(Memo1.Text), ''))
          = WideLowerCase(Edit1.Text)) then
      Edit1.Text := ChangeFileExt(ExtractFileName(OpenDialog1.FileName), '');

    Memo1.Text := OpenDialog1.FileName;
  end;
end;

procedure TProgSettDlg.Button9Click(Sender: TObject);

  function ValidateInput: boolean;
  begin
    Result := (Edit1.Text <> '') and (Memo1.Text <> '');
  end;

  function Find(const S: string): integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to ListView1.Items.Count - 1 do
      if ListView1.Items[I].Caption = S then
        exit(I);
  end;

begin
  case (Sender as TComponent).Tag of
    4: ListView1.Clear;
    3: ListView1.DeleteSelected;
    2: if Assigned(ListView1.Selected) and ValidateInput then
         with ListView1.Selected do begin
           Caption := Edit1.Text;
           SubItems.Clear;
           SubItems.Add(Memo1.Text);
         end
       else
         MessageBox(Handle, PChar(SysErrorMessage(ERROR_INVALID_DATA)), PChar(Application.Title), MB_ICONERROR or MB_OK);

    1: if not ValidateInput then begin
         Button8.Click;
         if ValidateInput then
           with ListView1.Items.Add do begin
             Caption := Edit1.Text;
             SubItems.Add(Memo1.Text);
           end
         else
           MessageBox(Handle, PChar(SysErrorMessage(ERROR_INVALID_DATA)), PChar(Application.Title), MB_ICONERROR or MB_OK);
       end
       else if Find(Edit1.Text) = -1 then
         with ListView1.Items.Add do begin
           Caption := Edit1.Text;
           SubItems.Add(Memo1.Text);
         end
  end;
  ListView1SelectItem(Self, ListView1.Selected, Assigned(ListView1.Selected));
end;

procedure TProgSettDlg.CustomAppearanceChange(Sender: TObject);
var
  I: integer;
begin
  if RadioButton2.Checked then
    with Memo2.Lines do begin
      BeginUpdate;
      Clear;
      for I := 0 to Min(CheckListBox1.Count - 1, High(CheckListKeys)) do
        if ord(CheckListBox1.Checked[I]) <> CheckListKeys[I].Default then
          Values[CheckListKeys[I].Name] := IntToStr(ord(CheckListBox1.Checked[I]));

      if ComboBox2.ItemIndex <> 0 then
        Values['video_fullscreen_scale'] := IntToStr(ComboBox2.ItemIndex);

      case ComboBox3.ItemIndex of
        1, 2: Values['vid_resize'] := IntToStr(ComboBox3.ItemIndex);
        3:    begin
                Values['vid_resize'] := '1';
                Values['scale'] := '0';
              end;
        4, 5: begin
                Values['vid_resize'] := '1';
                Values['scale'] := IntToStr(ComboBox3.ItemIndex - 2);
              end;
      end;

      EndUpdate;
    end;
end;

procedure TProgSettDlg.FormCreate(Sender: TObject);
begin
  Core.Icons32.GetIcon(8, Image1.Picture.Icon);
  Core.Icons32.GetIcon(11, Image2.Picture.Icon);
  Core.Icons32.GetIcon(0, Image3.Picture.Icon);
  Core.Icons32.GetIcon(30, Image4.Picture.Icon);

  Language.Translate('ProgSettDlg', Self);
  OpenDialog1.Filter := _T('OpenExeDialog');
  OpenDialog2.Filter := _T('Open86BoxDlg');
end;

procedure TProgSettDlg.FormShow(Sender: TObject);
var
  L: TStringList;
  I: integer;

  Key: HKEY;
begin
  PageControl1.ActivePageIndex := 0;
  Path.Text := DefWorkingRootDirectory;
  with TRegIniFile.Create(SRegRootKey, KEY_READ) do
    try
      ComboBox1.Text := ReadString(SRegConfigKey + '.86Box', StrRepository, Def86BoxRepo);
      ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(ComboBox1.Text);
      CheckBox1.Checked := ReadInteger(SRegConfigKey + '.86Box', StrAutoUpdate, 1) <> 0;
      CheckBox2.Checked := ReadInteger(SRegConfigKey + '.86Box', StrDownloadSource, 0) <> 0;

      RadioButton1.Checked := false;
      RadioButton2.Checked := false;
      RadioButton3.Checked := false;
      RadioButton4.Checked := false;
      case ReadInteger(SRegConfigKey + '.86Box', StrAutoAppearance, 1) of
        0: RadioButton4.Checked := true;
        2: RadioButton2.Checked := true;
        3: RadioButton3.Checked := true;
        else
          RadioButton1.Checked := true;
      end;

      Memo2.Clear;
      if RegOpenKeyEx(CurrentKey, PChar(SRegConfigKey + '.86Box'), 0, Access, Key) = ERROR_SUCCESS then begin
        RegReadMulti(Key, StrApperanceValues, Memo2.Lines);
        RegCloseKey(Key);
      end;
      UpdateAppearancePage(Self);

      ListView1.Clear;
      L := TStringList.Create;
      try
        ReadSectionValues(SRegConfigKey + '.Tools', L);
        for I := 0 to L.Count - 1 do
          with ListView1.Items.Add do begin
            Caption := L.Names[I];
            SubItems.Add(L.ValueFromIndex[I]);
          end;
      finally
        L.Free;
      end;
    finally
      Free;
    end;
end;

procedure TProgSettDlg.ListView1SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Assigned(ListView1.Selected) then begin
    Edit1.Text := ListView1.Selected.Caption;
    if ListView1.Selected.SubItems.Count > 0 then
      Memo1.Text := ListView1.Selected.SubItems[0]
    else
      Memo1.Text := '';
  end
  else begin
    Memo1.Text := '';
    Edit1.Text := '';
  end;
end;

procedure TProgSettDlg.UpdateAppearancePage(Sender: TObject);
var
  I: Integer;

  function ReadKey(const Name: string; const Default: integer): integer;
  var
    I: integer;
  begin
    if TryStrToInt(Memo2.Lines.Values[Name], I) then
      Result := I
    else
      Result := Default
  end;

begin
  CheckListBox1.Enabled := RadioButton2.Checked;
  ComboBox2.Enabled := RadioButton2.Checked;
  ComboBox3.Enabled := RadioButton2.Checked;

  Memo2.Enabled := RadioButton3.Checked;
  Button12.Enabled := RadioButton3.Checked;

  if RadioButton1.Checked then
    Memo2.Lines.Text := StrDefaultAppearance;

  for I := 0 to Min(High(CheckListKeys), CheckListBox1.Count - 1) do
    with CheckListKeys[I] do
      CheckListBox1.Checked[I] := ReadKey(Name, Default) <> 0;

  I := ReadKey('vid_resize', 0);
  if (I > 0) and (I < 3) then
    ComboBox3.ItemIndex := I
  else
    case ReadKey('scale', 1) of
      0: ComboBox3.ItemIndex := 3;
      1: ComboBox3.ItemIndex := 0;
      2: ComboBox3.ItemIndex := 4;
      3: ComboBox3.ItemIndex := 5;
      else
        ComboBox3.ItemIndex := -1;
    end;

  I := ReadKey('video_fullscreen_scale', 0);
  if I < ComboBox2.Items.Count then
    ComboBox2.ItemIndex := I
  else
    ComboBox2.ItemIndex := -1;
end;

end.
