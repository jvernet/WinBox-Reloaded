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
  Registry, uCoreModule, u86Box, IOUtils;

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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ProgSettDlg: TProgSettDlg;

implementation

resourcestring
  StrVálasszaKiAzÚjVi = 'Válassza ki az új virtuális gépek tárolási helyét:';
  StrAutoUpdate = 'AutoUpdate';
  StrDownloadSource = 'DownloadSource';
  StrRepository = 'Repository';
  Def86BoxRepo = 'https://ci.86box.net/job/86Box';
  StrMódosításokTörténte = 'Módosítások történtek az eszközlistán. Kívánja m' +
  'enteni?';

{$R *.dfm}

procedure TProgSettDlg.Button11Click(Sender: TObject);
begin
  Path.Text := IncludeTrailingPathDelimiter(
    TPath.GetDocumentsPath + PathDelim + SDefaultDocumentsFolder);
end;

procedure TProgSettDlg.Button2Click(Sender: TObject);
var
  I: integer;
begin
  if (Path.Text = '') or (Path.Text = '\') then
    raise Exception.Create(SysErrorMessage(ERROR_PATH_NOT_FOUND));

  Path.Text := IncludeTrailingPathDelimiter(Path.Text);

  if Assigned(ListView1.Selected) then
    with ListView1.Selected do begin
      if ((Caption <> Edit1.Text) or
           ((SubItems.Count > 0) and (Memo1.Text <> SubItems[0]))) and
         (Edit1.Text <> '') and (Memo1.Text <> '') and
         (MessageBox(Handle, PChar(StrMódosításokTörténte),
           PChar(Application.Title), MB_YESNO or MB_ICONQUESTION) = mrYes) then
            Button10.Click;
    end
  else if (Edit1.Text <> '') and (Memo1.Text <> '') and
          (MessageBox(Handle, PChar(StrMódosításokTörténte),
            PChar(Application.Title), MB_YESNO or MB_ICONQUESTION) = mrYes) then
              Button9.Click;

  SysUtils.ForceDirectories(Path.Text);
  with TRegIniFile.Create(SRegRootKey) do
    try
      WriteString(SRegConfigKey, StrRootDirectory, Path.Text);
      WriteString(SRegConfigKey + '.86Box', StrRepository, ComboBox1.Text);
      WriteInteger(SRegConfigKey + '.86Box', StrAutoUpdate, ord(CheckBox1.Checked));
      WriteInteger(SRegConfigKey + '.86Box', StrDownloadSource, ord(CheckBox2.Checked));

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
  if SelectDirectory(StrVálasszaKiAzÚjVi, '', Directory, [sdNewUI], Self) then
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

procedure TProgSettDlg.FormCreate(Sender: TObject);
begin
  Core.Icons32.GetIcon(8, Image1.Picture.Icon);
  Core.Icons32.GetIcon(11, Image2.Picture.Icon);
  Core.Icons32.GetIcon(0, Image3.Picture.Icon);
end;

procedure TProgSettDlg.FormShow(Sender: TObject);
var
  L: TStringList;
  I: integer;
begin
  PageControl1.ActivePageIndex := 0;
  Path.Text := DefWorkingRootDirectory;
  with TRegIniFile.Create(SRegRootKey, KEY_READ) do
    try
      ComboBox1.Text := ReadString(SRegConfigKey + '.86Box', StrRepository, Def86BoxRepo);
      ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(ComboBox1.Text);
      CheckBox1.Checked := ReadInteger(SRegConfigKey + '.86Box', StrAutoUpdate, 1) <> 0;
      CheckBox2.Checked := ReadInteger(SRegConfigKey + '.86Box', StrDownloadSource, 0) <> 0;

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

end.
