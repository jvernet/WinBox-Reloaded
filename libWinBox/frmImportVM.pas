unit frmImportVM;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.ComCtrls, Vcl.ExtCtrls, uBaseProfile, uCommUtil, uLang;

type
  IImportVM = interface
    ['{E162D25D-8F3B-4032-8726-077EF4499FAE}']
    function GetProfileID: PChar; stdcall;
    function GetFriendlyName: PChar; stdcall;
    function GetOpenSettings: boolean; stdcall;
    procedure SetFriendlyName(const Value: PChar); stdcall;
    function Execute(const AutoCreate: boolean): boolean; stdcall;
    function TryCreate: boolean; stdcall;

    procedure RecreateProfileID; stdcall;

    property FriendlyName: PChar read GetFriendlyName write SetFriendlyName;
    property OpenSettings: boolean read GetOpenSettings;
    property ProfileID: PChar read GetProfileID;
  end;

  TImportVM = class(TForm, IImportVM, ILanguageSupport)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    TabSheet7: TTabSheet;
    Label15: TLabel;
    Label16: TLabel;
    Label18: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Edit1: TEdit;
    Path: TEdit;
    Button3: TButton;
    TabSheet5: TTabSheet;
    Label25: TLabel;
    Label26: TLabel;
    CheckBox1: TCheckBox;
    TabSheet6: TTabSheet;
    Label3: TLabel;
    TabSheet2: TTabSheet;
    Label4: TLabel;
    Label5: TLabel;
    RadioButton1: TRadioButton;
    Label6: TLabel;
    RadioButton2: TRadioButton;
    Label7: TLabel;
    Label8: TLabel;
    Edit2: TEdit;
    Button4: TButton;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  public
    FAutoCreate: boolean;
    FProfileID: string;
    function GetFriendlyName: PChar; stdcall;
    procedure SetFriendlyName(const Value: PChar); stdcall;
    function Execute(const AutoCreate: boolean): boolean; stdcall;
    procedure GetTranslation(Language: TLanguage); stdcall;
    procedure Translate; stdcall;
    function GetOpenSettings: boolean; stdcall;
    function TryCreate: boolean; stdcall;

    function GetProfileID: PChar; stdcall;
    procedure RecreateProfileID; stdcall;
  end;

var
  ImportVM: TImportVM;

implementation

{$R *.dfm}

{ TForm1 }

procedure TImportVM.Button1Click(Sender: TObject);
begin
  if Sender = Button1 then
    case PageControl1.ActivePageIndex of
      1: begin
           if Edit1.Text = '' then
             raise Exception.Create(SysErrorMessage(ERROR_INVALID_PARAMETER));

           if (Path.Text = '') or not FileExists(Path.Text) then
             raise Exception.Create(SysErrorMessage(ERROR_PATH_NOT_FOUND));
         end;
      2: if RadioButton2.Checked and ((Edit2.Text = '') or not FileExists(Edit2.Text)) then
             raise Exception.Create(SysErrorMessage(ERROR_PATH_NOT_FOUND));
      3: if (not FAutoCreate) or TryCreate then
           ModalResult := mrOK
         else
           exit;
    end;

  PageControl1.ActivePageIndex := PageControl1.ActivePageIndex +
    (Sender as TComponent).Tag;

  Button2.Enabled := PageControl1.ActivePageIndex > 0;
  Button1.Enabled := PageControl1.ActivePageIndex < PageControl1.PageCount - 1;
end;

procedure TImportVM.Button3Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Path.Text := OpenDialog1.FileName;
end;

procedure TImportVM.Button4Click(Sender: TObject);
begin
  if OpenDialog2.Execute then
    Edit2.Text := OpenDialog2.FileName;
end;

function TImportVM.Execute(const AutoCreate: boolean): boolean;
begin
  FAutoCreate := AutoCreate;
  Result := ShowModal = mrOK;
end;

procedure TImportVM.FormCreate(Sender: TObject);
begin
  LoadImage('BANNER_IMP', Image1);
  Translate;
end;

procedure TImportVM.FormShow(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
  Path.Text := '';

  RadioButton2.Checked := false;
  RadioButton1.Checked := true;
  RadioButton1Click(Self);

  CheckBox1.Checked := false;
  RecreateProfileID;
end;

function TImportVM.GetFriendlyName: PChar;
begin
  Result := PChar(Edit1.Text);
end;

function TImportVM.GetOpenSettings: boolean;
begin
  Result := CheckBox1.Checked;
end;

function TImportVM.GetProfileID: PChar;
begin
  Result := PChar(FProfileID);
end;

procedure TImportVM.GetTranslation(Language: TLanguage);
begin
  if Assigned(Language) then
    Language.GetTranslation('ImportVM', Self);
end;

procedure TImportVM.RadioButton1Click(Sender: TObject);
begin
  Edit2.Enabled := RadioButton2.Checked;
  Button4.Enabled := RadioButton2.Checked;

  if RadioButton1.Checked then
    Edit2.Text := TProfile.DefExecutablePath + '86Box\86Box.exe';
end;

procedure TImportVM.RecreateProfileID;
var
  G: TGUID;
begin
  CreateGUID(G);
  FProfileID := GUIDToString(G);
end;

procedure TImportVM.SetFriendlyName(const Value: PChar);
begin
  Edit1.Text := String(Value);
end;

procedure TImportVM.Translate;
begin
  if Assigned(Language) then begin
    Language.Translate('ImportVM', Self);
    OpenDialog1.Filter := _T('Open86BoxDlg');
    OpenDialog2.Filter := _T('OpenExeDialog');
  end;
end;

function TImportVM.TryCreate: boolean;
begin
  TProfile.CreateProfile(
    FProfileID,
    Edit2.Text,
    Edit1.Text,
    ExtractFilePath(Path.Text),
    SRegBaseKey + '.86Box');

  Result := true;
end;

end.
