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

unit frmWizardHDD;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage, Vcl.Samples.Spin, frmSelectHDD, Printers;

type
  IWizardHDD = interface
    ['{57E94996-C896-45BD-811D-08BA692D0B49}']
    function GetSparse: boolean; stdcall;
    procedure SetSparse(const Value: boolean); stdcall;
    function GetFileName: PChar; stdcall;
    procedure SetFileName(const Value: PChar); stdcall;
    function GetDiskData: TDiskData; stdcall;
    procedure SetDiskData(const Value: TDiskData); stdcall;
    procedure SetConnectorFilter(const Lock: boolean); stdcall;
    function Execute(const AutoCreate: boolean): boolean; stdcall;
    function TryCreate: boolean; stdcall;

    property Sparse: boolean read GetSparse write SetSparse;
    property FileName: PChar read GetFileName write SetFileName;
    property DiskData: TDiskData read GetDiskData write SetDiskData;
  end;

  TWizardHDD = class(TForm, IWizardHDD)
    Image1: TImage;
    PageControl1: TPageControl;
    Bevel1: TBevel;
    Button1: TButton;
    TabSheet1: TTabSheet;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    TabSheet2: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    Edit1: TEdit;
    Button3: TButton;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    ComboBox1: TComboBox;
    TabSheet3: TTabSheet;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Image2: TImage;
    Label11: TLabel;
    ComboBox2: TComboBox;
    Label12: TLabel;
    TrackBar1: TTrackBar;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Label16: TLabel;
    SpinEdit3: TSpinEdit;
    TabSheet4: TTabSheet;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    SpinEdit4: TSpinEdit;
    Label21: TLabel;
    SpinEdit5: TSpinEdit;
    Label22: TLabel;
    SpinEdit6: TSpinEdit;
    Label23: TLabel;
    SpinEdit7: TSpinEdit;
    Label24: TLabel;
    SpinEdit8: TSpinEdit;
    Button4: TButton;
    Bevel2: TBevel;
    CheckBox1: TCheckBox;
    TabSheet5: TTabSheet;
    Label25: TLabel;
    Memo1: TMemo;
    Label26: TLabel;
    Button5: TButton;
    Label27: TLabel;
    Image3: TImage;
    Button6: TButton;
    TabSheet6: TTabSheet;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    PrintDialog1: TPrintDialog;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure TabSheet5Show(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    FDiskData: TDiskData;
    FAutoCreate: boolean;
    procedure UpdateUI;
    function CreateSparseFile: boolean;
    function CreateRealFile: boolean;
  public
    function GetSparse: boolean; stdcall;
    procedure SetSparse(const Value: boolean); stdcall;
    function GetFileName: PChar; stdcall;
    procedure SetFileName(const Value: PChar); stdcall;
    function GetDiskData: TDiskData; stdcall;
    procedure SetDiskData(const Value: TDiskData); stdcall;
    procedure SetConnectorFilter(const Lock: boolean); stdcall;
    function Execute(const AutoCreate: boolean): boolean; stdcall;
    function TryCreate: boolean; stdcall;
  end;

var
  WizardHDD: TWizardHDD;

function CreateWizardHDD(const AOwner: TComponent): IWizardHDD; stdcall;

implementation

{$R *.dfm}

uses uCommUtil, frmWaitForm;

resourcestring
  StrAMegadottHelyenMá = 'A megadott helyen már létezik fájl, kívánja felülí' +
  'rni?'#13#10#13#10'A mûvelet nem fordítható vissza.';
  StrWinBox = 'WinBox';

  SDiskData2 = 'Fájlnév: %s'#13#10'Névleges kapacitás: %s'#13#10'Modellnév: %s %s'#13#10 +
  'Fizikai geometria:  C: %d H: %d S: %d B: 512'#13#10'Megadandó geometria:  C: %d H: %d S: %d B: 512'#13#10 +
  'Csatolófelület: %s'#13#10'Írás-elõkompenzáció (WPCom): %d'#13#10'Fej parkolópozíció (LZone): %d';
  SDiskData1 = 'Fájlnév: %s'#13#10'Kapacitás: %s'#13#10'Geometria:  C: %d H: %d S: %d B: 512'#13#10 +
  'Csatolófelület: %s'#13#10'Írás-elõkompenzáció (WPCom): %d'#13#10'Fej parkolópozíció (LZone): %d';
  SDiskData3 = 'Fájlnév: %s'#13#10'Kapacitás: %s'#13#10'Geometria:  C: %d H: %d S: %d B: 512'#13#10 +
  'Írás-elõkompenzáció (WPCom): %d'#13#10'Fej parkolópozíció (LZone): %d';

  SDiskSparse = 'A lemezkép ritka fájlként kerül létrehozásra.';
  StrAKötetNemTámogatj = 'A kötet nem támogatja a ritka fájlok létrehozását.'#13#10#13#10+
  'A WinBox megkísérelheti a fájl szokványos létrehozását, de ez hosszabb ideig is eltarthat.'#13#10 +
  #13#10'Kívánja szokványos módon létrehozni a képfájlt?';
  StrLemezképfájlLétreho = 'Lemezképfájl létrehozása...';
  EHibaTörténtASzaba = 'Hiba történt a szabad terület lekérdezése közben.';
  ENincsElegendõSzaba = 'Nincs elegendõ szabad hely a megadott helyen.'#13#10#13#10'Adjon meg más' +
  'ik elérési utat a képfájl létrehozásához, vagy válasszon kisebb kapacitást.';

function CreateWizardHDD(const AOwner: TComponent): IWizardHDD; stdcall;
begin
  Result := TWizardHDD.Create(AOwner) as IWizardHDD;
end;

procedure TWizardHDD.Button1Click(Sender: TObject);
var
  SelectDlg: ISelectHDD;
begin
  if Sender = Button1 then
    case PageControl1.ActivePageIndex of
      0: begin
           ComboBox2Change(Self);
           TrackBar1.Position := 1024;
           TrackBar1Change(Self);
         end;
      1: begin
           if (Edit1.Text = '') or (DirectoryExists(Edit1.Text)) then
             raise Exception.Create(SysErrorMessage(ERROR_PATH_NOT_FOUND));
           if FileExists(Edit1.Text) and (MessageBox(Handle, PChar(StrAMegadottHelyenMá),
              PChar(StrWinBox), MB_YESNO or MB_ICONWARNING) <> mrYes) then exit;
           case ComboBox1.ItemIndex of
             2: PageControl1.ActivePageIndex := PageControl1.ActivePageIndex + 1;
             1: begin
                  SelectDlg := THDSelect.Create(Self) as ISelectHDD;
                  SelectDlg.DiskData := FDiskData;
                  if not FDiskData.dgPhysicalGeometry.IsEmpty then
                    SelectDlg.LocatePhysCHS
                  else if not FDiskData.dgTranslatedGeometry.IsEmpty then
                    SelectDlg.LocateTransCHS;
                  SelectDlg.SetConnectorFilter(not CheckBox1.Enabled);
                  if SelectDlg.Execute(false) then begin
                    PageControl1.ActivePageIndex := PageControl1.ActivePageIndex + 2;
                    FDiskData := SelectDlg.DiskData;
                    CheckBox1.Checked := (pos('IDE', FDiskData.szConnector) <> 0) or
                                         (pos('SCSI', FDiskData.szConnector) <> 0);
                  end
                  else
                    exit;
                  SelectDlg := nil;
                end;
           end;
         end;
       2: begin
            with FDiskData, dgTranslatedGeometry do begin
              C := SpinEdit1.Value;
              H := SpinEdit2.Value;
              S := SpinEdit3.Value;
              dgPhysicalGeometry.C := 0;
              dgPhysicalGeometry.H := 0;
              dgPhysicalGeometry.S := 0;
              dwWritePreComp := C div 2;
              dwLandZone := C + 1;
              szModel := '';
              case ComboBox2.ItemIndex of
                0: szConnector := 'ESDI';
                6: szConnector := 'SCSI';
                else szConnector := 'IDE';
              end;
              CheckBox1.Checked := ComboBox2.ItemIndex <> 0;
            end;
            PageControl1.ActivePageIndex := PageControl1.ActivePageIndex + 1;
          end;
       3: with FDiskData, dgTranslatedGeometry do begin
            C := SpinEdit4.Value;
            H := SpinEdit5.Value;
            S := SpinEdit6.Value;
            dgPhysicalGeometry.C := 0;
            dgPhysicalGeometry.H := 0;
            dgPhysicalGeometry.S := 0;
            dwWritePreComp := SpinEdit7.Value;
            dwLandZone := SpinEdit8.Value;
            szConnector := '';
            szModel := '';
          end;
       4: if (not FAutoCreate) or TryCreate then
            ModalResult := mrOK
          else
            exit;
    end;

  if Sender = Button2 then
    case PageControl1.ActivePageIndex of
      3: PageControl1.ActivePageIndex := PageControl1.ActivePageIndex - 1;
      4: PageControl1.ActivePageIndex := PageControl1.ActivePageIndex - 2;
    end;

  PageControl1.ActivePageIndex := PageControl1.ActivePageIndex +
    (Sender as TComponent).Tag;
  if PageControl1.ActivePageIndex = 4 then
    TabSheet5Show(Self);

  UpdateUI;

  Button2.Enabled := PageControl1.ActivePageIndex > 0;
  Button1.Enabled := PageControl1.ActivePageIndex < PageControl1.PageCount - 1;
end;

procedure TWizardHDD.Button3Click(Sender: TObject);
begin
  SaveDialog1.FileName := ExtractFileName(Edit1.Text);
  SaveDialog1.InitialDir := ExtractFilePath(Edit1.Text);
  ForceDirectories(SaveDialog1.InitialDir);
  if SaveDialog1.Execute then
    Edit1.Text := SaveDialog1.FileName;
end;

procedure TWizardHDD.Button4Click(Sender: TObject);
begin
  SpinEdit7.Value := SpinEdit4.Value div 2;
  SpinEdit8.Value := SpinEdit4.Value + 1;
end;

procedure TWizardHDD.Button5Click(Sender: TObject);
begin
  SaveDialog2.FileName := ChangeFileExt(ExtractFileName(Edit1.Text), '.log');
  SaveDialog2.InitialDir := ExtractFilePath(Edit1.Text);
  ForceDirectories(SaveDialog2.InitialDir);
  if SaveDialog2.Execute then
    Memo1.Lines.SaveToFile(SaveDialog2.FileName);
end;

procedure TWizardHDD.Button6Click(Sender: TObject);
var
  F: TextFile;
  S: string;
begin
  if PrintDialog1.Execute then begin
    AssignPrn(F);
    try
      Rewrite(F);
      for S in Memo1.Lines do
        WriteLn(F, S);
    finally
      CloseFile(F);
    end;
  end;
end;

procedure TWizardHDD.ComboBox2Change(Sender: TObject);
const
  Limits: array [-1..6, 1..3] of integer =
    ((    1,  1,  1),
     (42366, 16, 99),
     ( 1024, 16, 63),
     ( 4096, 16, 63),
     ( 8192, 16, 63),
     (16320, 16, 63),
     (66576, 16, 63),
     ( 2658, 255, 99));
begin
  SpinEdit1.MaxValue := Limits[ComboBox2.ItemIndex, 1];
  SpinEdit2.MaxValue := Limits[ComboBox2.ItemIndex, 2];
  SpinEdit3.MaxValue := Limits[ComboBox2.ItemIndex, 3];
  TrackBar1.Max := Integer(int64(SpinEdit1.MaxValue) *
    SpinEdit2.MaxValue * SpinEdit3.MaxValue div 2048);
  TrackBar1.Frequency := TrackBar1.Max div 100;
end;

function TWizardHDD.CreateRealFile: boolean;
const
  BytesPerSector = 512;
var
  I, Count, BufferSize: integer;
  Buffer: Pointer;
begin
  Result := true;
  with FDiskData.dgTranslatedGeometry do begin
    BufferSize := H * S * 512;
    Count := C;
  end;

  GetMem(Buffer, BufferSize);
  try
    FillChar(Buffer^, SizeOf(Buffer), #0);
      with TWaitForm.Create(Self) do
        try
          ProgressBar.Max := Count;
          Show;
          with TFileStream.Create(Edit1.Text, fmCreate) do
            try
              for I := 1 to Count do begin
                WriteBuffer(Buffer^, BufferSize);
                ProgressBar.Position := I;
                Application.ProcessMessages;
              end;
            finally
              Free;
            end;
        finally
          Close;
          Free;
        end;
  finally
    FreeMem(Buffer, BufferSize);
  end;
end;

function TWizardHDD.CreateSparseFile: boolean;
var
  BytesReturned: longword;
const
  faSparseFile = $200;
  cbTestSize = 524288; //512 kB
begin
  with TFileStream.Create(Edit1.Text, fmCreate) do begin
    Result := DeviceIoControl(Handle, FSCTL_SET_SPARSE, nil, 0, nil, 0, BytesReturned, nil);
    SetFilePointerEx(Handle, cbTestSize, nil, FILE_BEGIN);
    SetEndOfFile(Handle);
    Free;
  end;

  BytesReturned := GetCompressedFileSize(PChar(Edit1.Text), @BytesReturned);
  Result := Result and ((FileGetAttr(Edit1.Text) and faSparseFile) = faSparseFile) and
      (BytesReturned < cbTestSize);

  if Result then
    with TFileStream.Create(Edit1.Text, fmCreate), FDiskData.dgTranslatedGeometry do begin
      Result := DeviceIoControl(Handle, FSCTL_SET_SPARSE, nil, 0, nil, 0, BytesReturned, nil);
      SetFilePointerEx(Handle, Int64(C) * H * S * 512, nil, FILE_BEGIN);
      SetEndOfFile(Handle);
      Free;
    end;
end;

function TWizardHDD.Execute(const AutoCreate: boolean): boolean;
begin
  FAutoCreate := AutoCreate;
  Result := ShowModal = mrOK;
end;

procedure TWizardHDD.FormCreate(Sender: TObject);
begin
  LoadImage('BANNER_HDD', Image1);
end;

procedure TWizardHDD.FormShow(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
  Button2.Enabled := false;
  Button1.Enabled := true;
end;

function TWizardHDD.GetDiskData: TDiskData;
begin
  Result := FDiskData;
end;

function TWizardHDD.GetFileName: PChar;
begin
  Result := PChar(Edit1.Text);
end;

function TWizardHDD.GetSparse: boolean;
begin
  Result := CheckBox1.Checked;
end;

procedure TWizardHDD.SetConnectorFilter(const Lock: boolean);
begin
  if Lock and (FDiskData.szConnector = '') then
    exit;

  UpdateUI;
  ComboBox1.Enabled := (ComboBox1.ItemIndex <> 1) or not Lock;
  ComboBox2.Enabled := not Lock;
  CheckBox1.Enabled := ComboBox2.Enabled ;
end;

procedure TWizardHDD.SetDiskData(const Value: TDiskData);
begin
  FDiskData := Value;
end;

procedure TWizardHDD.SetFileName(const Value: PChar);
begin
  Edit1.Text := Value;
end;

procedure TWizardHDD.SetSparse(const Value: boolean);
begin
  CheckBox1.Checked := Value;
end;

procedure TWizardHDD.TabSheet5Show(Sender: TObject);
begin
  Memo1.Clear;
  if (not FDiskData.dgPhysicalGeometry.IsEmpty) and (FDiskData.szModel <> '') then
    with FDiskData do
      Memo1.Text := Format(SDiskData2,
        [Edit1.Text, FileSizeToStr(UInt64(dwNominalSize) * 1000 * 1000, 2, 1000),
         szManufacturer, szModel, dgPhysicalGeometry.C, dgPhysicalGeometry.H,
         dgPhysicalGeometry.S, dgTranslatedGeometry.C, dgTranslatedGeometry.H,
         dgTranslatedGeometry.S, szConnector, dwWritePrecomp, dwLandZone])
  else if FDiskData.szConnector <> '' then
    with FDiskData do
      Memo1.Text := Format(SDiskData1,
        [Edit1.Text,
         FileSizeToStr(UInt64(dgTranslatedGeometry.C) * dgTranslatedGeometry.H * dgTranslatedGeometry.S * 512, 2, 1000),
         dgTranslatedGeometry.C, dgTranslatedGeometry.H,
         dgTranslatedGeometry.S, szConnector, dwWritePrecomp, dwLandZone])
  else with FDiskData do
      Memo1.Text := Format(SDiskData3,
        [Edit1.Text,
         FileSizeToStr(UInt64(dgTranslatedGeometry.C) * dgTranslatedGeometry.H * dgTranslatedGeometry.S * 512, 2, 1000),
         dgTranslatedGeometry.C, dgTranslatedGeometry.H,
         dgTranslatedGeometry.S, dwWritePrecomp, dwLandZone]);

  if CheckBox1.Checked then
    Memo1.Lines.Add(SDiskSparse);
end;

procedure TWizardHDD.TrackBar1Change(Sender: TObject);
begin
  SpinEdit2.Value := SpinEdit2.MaxValue;
  SpinEdit3.Value := SpinEdit3.MaxValue;
  SpinEdit1.Value := int64(TrackBar1.Position * 2048 div SpinEdit2.Value div SpinEdit3.Value);

  Label13.Caption := IntToStr(Integer(Int64(SpinEdit1.Value) *
    SpinEdit2.Value * SpinEdit3.Value div 2048)) + ' MB';
end;

function TWizardHDD.TryCreate: boolean;
var
  FreeAvail,
  TotalSpace: Int64;
begin
  Result := false;

  if (Edit1.Text = '') or DirectoryExists(Edit1.Text) then
    raise Exception.Create(SysErrorMessage(ERROR_PATH_NOT_FOUND));

  ForceDirectories(ExtractFilePath(Edit1.Text));

  if not System.SysUtils.GetDiskFreeSpaceEx(
    PChar(IncludeTrailingPathDelimiter(ExtractFilePath(Edit1.Text))),
    FreeAvail, TotalSpace, nil) then
      raise Exception.Create(EHibaTörténtASzaba);

  with FDiskData.dgTranslatedGeometry do
    if FreeAvail < Int64(C) * H * S * 512 then
      raise Exception.Create(ENincsElegendõSzaba);

  try
    if not CheckBox1.Checked then
      Result := CreateRealFile
    else begin
      Result := CreateSparseFile;
      if not Result and (MessageBox(Handle, PChar(StrAKötetNemTámogatj), PChar(StrWinBox),
        MB_YESNO or MB_ICONQUESTION) = mrYes) then
          Result := CreateRealFile;
    end;
  except
    on E: Exception do begin
      MessageBox(Handle, PChar(E.Message), PChar(StrWinBox), MB_OK or MB_ICONERROR);
      Result := false;
    end;
  end;
end;

procedure TWizardHDD.UpdateUI;
const
  DefCHS: TDiskGeometry = (C: 1024; H: 16; S: 63);
var
  ACHS: TDiskGeometry;
begin
  CheckBox1.Checked := false;
  if (pos('MFM', UpperCase(FDiskData.szConnector)) <> 0) or
     (pos('RLL', UpperCase(FDiskData.szConnector)) <> 0) or
     (pos('PS/2', UpperCase(FDiskData.szConnector)) <> 0) then
    ComboBox1.ItemIndex := 1
  else if pos('ESDI', UpperCase(FDiskData.szConnector)) <> 0 then
    ComboBox2.ItemIndex := 0
  else if pos('SCSI', UpperCase(FDiskData.szConnector)) <> 0 then begin
    ComboBox2.ItemIndex := 6;
    CheckBox1.Checked := true;
  end
  else begin
    ComboBox2.ItemIndex := 5;
    CheckBox1.Checked := true;
  end;
  ComboBox2Change(Self);

  if not FDiskData.dgTranslatedGeometry.IsEmpty then
    ACHS := FDiskData.dgTranslatedGeometry
  else if not FDiskData.dgPhysicalGeometry.IsEmpty then
    ACHS := FDiskData.dgPhysicalGeometry
  else with DefCHS do begin
    ACHS := DefCHS;
    FDiskData.dwWritePreComp := ACHS.C div 2;
    FDiskData.dwLandZone := ACHS.C + 1;
  end;

  with ACHS do begin
    SpinEdit1.Value := C;
    SpinEdit2.Value := H;
    SpinEdit3.Value := S;
    TrackBar1.Position := Integer(Int64(C) * H * S div 2048);
    Label13.Caption := IntToStr(TrackBar1.Position) + ' MB';

    SpinEdit4.Value := C;
    SpinEdit5.Value := H;
    SpinEdit6.Value := S;
  end;

  SpinEdit7.Value := FDiskData.dwWritePreComp;
  SpinEdit8.Value := FDiskData.dwLandZone;
end;

end.
