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

unit frmSelectHDD;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Datasnap.DBClient, Vcl.Grids,
  Vcl.DBGrids, Vcl.StdCtrls, Vcl.DBCtrls, Vcl.ExtCtrls, Vcl.Samples.Spin;

type
  TDiskGeometry = record
    C, H, S: integer;
    function IsEmpty: boolean;
    function Size: uint64;
  end;

  TDiskData = record
    dgPhysicalGeometry,
    dgTranslatedGeometry: TDiskGeometry;
    dwNominalSize, dwLandZone, dwWritePreComp: integer;
    szManufacturer: array [0..34] of char;
    szModel: array [0..29] of char;
    szConnector: array [0..19] of char;
  end;

  ISelectHDD = interface
    ['{BCC01793-CEDA-4491-B1FF-98F9723BB6AA}']
    function Execute(const Silent: boolean): boolean; stdcall;
    function GetDiskData: TDiskData; stdcall;
    procedure SetDiskData(const Data: TDiskData); stdcall;
    procedure SetConnectorFilter(const Lock: boolean); stdcall;
    procedure SetCHSFilter(const Enabled: boolean); stdcall;

    function LocatePhysCHS: boolean; stdcall;
    function LocateTransCHS: boolean; stdcall;

    property DiskData: TDiskData read GetDiskData write SetDiskData;
  end;

  THDSelect = class(TForm, ISelectHDD)
    CDS: TClientDataSet;
    DBGrid1: TDBGrid;
    GroupBox1: TGroupBox;
    cbGyartoFilter: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    Label5: TLabel;
    Label6: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    RadioButton3: TRadioButton;
    Label7: TLabel;
    cbCsatoloFilter: TComboBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    ComboBox3: TComboBox;
    Label8: TLabel;
    Button3: TButton;
    Button4: TButton;
    DBNavigator1: TDBNavigator;
    DataSource1: TDataSource;
    Edit1: TEdit;
    Label9: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CDSFilterRecord(DataSet: TDataSet; var Accept: Boolean);
    procedure FilterChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure DBGrid1CellClick(Column: TColumn);
    procedure Button4Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FDiskData: TDiskData;
    FBookmark: TBookmark;
  protected
    function Execute(const Silent: boolean): boolean; stdcall;
    function GetDiskData: TDiskData; stdcall;
    procedure SetDiskData(const Data: TDiskData); stdcall;
    procedure SetConnectorFilter(const Lock: boolean); stdcall;
    procedure SetCHSFilter(const Enabled: boolean); stdcall;

    function LocatePhysCHS: boolean; stdcall;
    function LocateTransCHS: boolean; stdcall;
  public
    procedure LoadTable;
  end;

var
  HDSelect: THDSelect;

function CreateSelectHDD(const AOwner: TComponent): ISelectHDD; stdcall;

implementation

{$R *.dfm}
{$R 'Data\rcWinBoxLib.res'}

resourcestring
  StrBármelyGyártó = '(Bármely gyártó)';
  StrBármelyCsatoló = '(Bármely csatoló)';
  ENemSikerültASort = 'Nem sikerült a sort hozzáadni: "%s", sor: %d';
  EHibásRekordAFájlb = 'Hibás rekord a fájlban: sor: %d, mezõ: %d, %s';

function CreateSelectHDD(const AOwner: TComponent): ISelectHDD; stdcall;
begin
  Result := THDSelect.Create(AOwner) as ISelectHDD;
end;

procedure THDSelect.Button1Click(Sender: TObject);
begin
  Panel2.Visible := not Panel2.Visible;
  GroupBox1.Visible := not GroupBox1.Visible;

  if Panel2.Visible then begin
    ClientHeight := ClientHeight - GroupBox1.Height + Panel2.Height;
    Top := Top - (-GroupBox1.Height + Panel2.Height) div 2;
  end
  else begin
    ClientHeight := ClientHeight + GroupBox1.Height - Panel2.Height;
    Top := Top - (GroupBox1.Height - Panel2.Height) div 2;
  end;
end;

procedure THDSelect.Button4Click(Sender: TObject);
begin
  with FDiskData do begin
    StrPLCopy(@szManufacturer[0], CDS.Fields[1].AsString + #0, 35);
    StrPLCopy(@szModel[0], CDS.Fields[2].AsString + #0, 30);
    dwNominalSize := CDS.Fields[3].AsInteger;
    dgPhysicalGeometry.C := CDS.Fields[4].AsInteger;
    dgPhysicalGeometry.H := CDS.Fields[5].AsInteger;
    dgPhysicalGeometry.S := CDS.Fields[6].AsInteger;
    dwWritePreComp := CDS.Fields[7].AsInteger;
    dwLandZone := CDS.Fields[8].AsInteger;
    StrPLCopy(@szConnector[0], CDS.Fields[9].AsString + #0, 20);
    dgTranslatedGeometry.C := CDS.Fields[10].AsInteger;
    dgTranslatedGeometry.H := CDS.Fields[11].AsInteger;
    dgTranslatedGeometry.S := CDS.Fields[12].AsInteger;
  end;
  ModalResult := mrOK;
end;

procedure THDSelect.CDSFilterRecord(DataSet: TDataSet; var Accept: Boolean);

  function CheckValue(A, B, Op: integer): boolean;
  begin
    case Op of
      0: Result := A = B;  //egyenlõ
      1: Result := A <> B; //nem egyenlõ
      2: Result := A < B;  //kisebb
      3: Result := A <= B; //kisebb, vagy egyenlõ
      4: Result := A > B;  //nagyobb
      5: Result := A >= B; //nagyobb, vagy egyenlõ
      else Result := true;
    end;
  end;

begin
  if cbGyartoFilter.ItemIndex > 0 then
    Accept := Accept and (CDS.Fields[1].AsString = cbGyartoFilter.Items[cbGyartoFilter.ItemIndex]);

  if cbCsatoloFilter.ItemIndex > 0 then
    Accept := Accept and (CDS.Fields[9].AsString = cbCsatoloFilter.Items[cbCsatoloFilter.ItemIndex]);

  if RadioButton1.Checked then
    Accept := Accept and
              (CDS.Fields[4].AsInteger = SpinEdit1.Value) and
              (CDS.Fields[5].AsInteger = SpinEdit2.Value) and
              (CDS.Fields[6].AsInteger = SpinEdit3.Value)
  else if RadioButton2.Checked then begin
    Accept := Accept and
              CheckValue(CDS.Fields[3].AsInteger,
                         SpinEdit5.Value,
                         ComboBox1.ItemIndex) and
              CheckValue(CDS.Fields[3].AsInteger,
                         SpinEdit4.Value,
                         ComboBox2.ItemIndex);
  end;

  if Edit1.Text <> '' then
    Accept := Accept and
              (pos(Edit1.Text, UpperCase(CDS.Fields[2].AsString)) <> 0);
end;

procedure THDSelect.ComboBox3Change(Sender: TObject);
begin
  if ComboBox3.ItemIndex > 0 then
    CDS.IndexFieldNames := ComboBox3.Text
  else
    CDS.IndexFieldNames := '';
end;

procedure THDSelect.DBGrid1CellClick(Column: TColumn);
begin
  DBGrid1.SelectedRows.CurrentRowSelected := True;
end;

function THDSelect.Execute(const Silent: boolean): boolean;
begin
  if Silent then begin
    FormShow(Self);
    Button4Click(Self);
    Result := true;
  end
  else
    Result := ShowModal = mrOK;
end;

procedure THDSelect.FormCreate(Sender: TObject);
var
  I: Integer;
const
  HiddenColumns: set of byte = [0];
begin
  if CDS.State = dsInactive then
    CDS.CreateDataSet;

  for I := 0 to DBGrid1.Columns.Count - 1 do
    if I in HiddenColumns then
      DBGrid1.Columns[I].Visible := false
    else
      ComboBox3.Items.Add(CDS.FieldDefs[I].Name);

  ComboBox3.ItemIndex := 3;
  ComboBox3Change(ComboBox3);

  FDiskData.szConnector := '';
  LoadTable;
end;

procedure THDSelect.FormShow(Sender: TObject);
begin
  if not FDiskData.dgPhysicalGeometry.IsEmpty then begin
    SpinEdit1.Value := FDiskData.dgPhysicalGeometry.C;
    SpinEdit2.Value := FDiskData.dgPhysicalGeometry.H;
    SpinEdit3.Value := FDiskData.dgPhysicalGeometry.S;
  end;
end;

function THDSelect.GetDiskData: TDiskData;
begin
  Result := FDiskData;
end;

function DownCase(Ch: Char): Char;
begin
  if CharInSet(Ch, ['A'..'Z']) then
    Result := Chr(Ord(Ch) - Ord('A') + Ord('a'))
  else
    Result := Ch;
end;

function CorrectNames(const Name: string): string;
const
  Alpha = ['0'..'9', 'a'..'z', 'A'..'Z'];
var
  I, J, K: integer;
begin
  Result := Name;

  if (Result = '') or (length(Result) <= 3) or (Result[1] = '(') then
    exit;

  J := 1;
  K := -1;
  for I := 1 to High(Name) do begin
    if CharInSet(Result[I], Alpha) then begin
      if (J = -1) then
        Result[I] := DownCase(Result[I])
      else begin
        Result[I] := UpCase(Result[I]);
        J := -1;
      end;
    end
    else
      J := 1;

    if (K = -1) and (Result[I] = ' ') then
      K := I;
  end;

  if (K <= 4) and (K <> -1) then
    for I := 1 to K do
      Result[I] := UpCase(Result[I]);
end;

procedure THDSelect.LoadTable;
var
  R: TResourceStream;
  List, Rec: TStringList;
  I, J: Integer;
begin
  CDS.ReadOnly := false;
  CDS.DisableControls;

  cbGyartoFilter.Clear;
  cbGyartoFilter.Items.Add(StrBármelyGyártó);
  cbCsatoloFilter.Clear;
  cbCsatoloFilter.Items.Add(StrBármelyCsatoló);

  List := TStringList.Create;
  try
    R := TResourceStream.Create(hInstance, 'HDD_TABLA', RT_RCDATA);
    List.LoadFromStream(R);
    R.Free;

    CDS.EmptyDataSet;
    Rec := TStringList.Create;
    try
      for I := 0 to List.Count - 1 do begin
        Rec.Clear;
        ExtractStrings([';'],[], PChar(List[I]), Rec);

        if (Rec.Count >= 10) then begin
          if (cbGyartoFilter.Items.IndexOf(Rec[1]) = -1) then
              cbGyartoFilter.Items.Add(CorrectNames(Rec[1]));
          if (cbCsatoloFilter.Items.IndexOf(Rec[9]) = -1)
             and ((FDiskData.szConnector = '') or (pos(String(FDiskData.szConnector), Rec[9]) <> 0)) then
              cbCsatoloFilter.Items.Add(Rec[9]);
        end;

        CDS.Append;

        CDS.Fields[0].Value := Rec[0];
        CDS.Fields[1].Value := CorrectNames(Rec[1]);
        for J := 2 to Rec.Count - 1 do
          try
            CDS.Fields[J].Value := Rec[J];
          except
              on E: EDatabaseError do begin
                 CDS.Cancel;  // Failed, discard the record
                 MessageBox(Handle, PChar(
                   format(EHibásRekordAFájlb, [I + 1, J, Rec[J]])),
                   PChar(Application.Name), MB_ICONERROR or MB_OK);
                 break;    // Continue with next record
              end;
            end;

        if CDS.State = dsInsert then // It's not dsInsert if we Cancelled the Insert
          try
            CDS.Post;
          except
            on E:EDatabaseError do begin
              // log error instead of showing
              MessageBox(Handle, PChar(
                   format(ENemSikerültASort, [List[I], I + 1])),
                   PChar(Application.Name), MB_ICONERROR or MB_OK);
              CDS.Cancel;
            end;
          end;
      end;
    finally
      Rec.Free;
    end;
  finally
    cbGyartoFilter.ItemIndex := 0;
    if FDiskData.szConnector = '' then
      cbCsatoloFilter.ItemIndex := 0
    else if cbCsatoloFilter.Items.Count > 1 then
      cbCsatoloFilter.ItemIndex := 1;
    CDS.EnableControls;
    CDS.ReadOnly := true;
    CDS.Filtered := false;
    CDS.Filtered := true;
    List.Free;
  end;
end;

function THDSelect.LocatePhysCHS: boolean;
var
  A: Variant;
begin
  if FDiskData.dgPhysicalGeometry.IsEmpty then
    exit(false);

  A := VarArrayCreate([0, 2], varVariant);
  A[0] := FDiskData.dgPhysicalGeometry.C;
  A[1] := FDiskData.dgPhysicalGeometry.H;
  A[2] := FDiskData.dgPhysicalGeometry.S;
  Result := CDS.Locate('C;H;S', A, []);
end;

function THDSelect.LocateTransCHS: boolean;
var
  A: Variant;
begin
  if FDiskData.dgTranslatedGeometry.IsEmpty then
    exit(false);

  A := VarArrayCreate([0, 2], varVariant);
  A[0] := FDiskData.dgTranslatedGeometry.C;
  A[1] := FDiskData.dgTranslatedGeometry.H;
  A[2] := FDiskData.dgTranslatedGeometry.S;
  Result := CDS.Locate('TC;TH;TS', A, []);
end;

procedure THDSelect.SetCHSFilter(const Enabled: boolean);
begin
  if Enabled then
    RadioButton1.Checked := true
  else
    RadioButton3.Checked := true;

  FilterChange(Self);
end;

procedure THDSelect.SetConnectorFilter(const Lock: boolean);
begin
  if FDiskData.szConnector <> '' then
    cbCsatoloFilter.ItemIndex :=
       cbCsatoloFilter.Items.IndexOf(string(FDiskData.szConnector));
  if cbCsatoloFilter.ItemIndex = -1 then
    cbCsatoloFilter.ItemIndex := 0;

  FilterChange(Self);
  cbCsatoloFilter.Enabled := (not Lock) or (cbCsatoloFilter.ItemIndex = 0);
end;

procedure THDSelect.SetDiskData(const Data: TDiskData);
begin
  FDiskData := Data;
end;

procedure THDSelect.FilterChange(Sender: TObject);
begin
  FBookmark := CDS.GetBookmark;

  CDS.Filtered := false;
  CDS.Filtered := true;

  if CDS.BookmarkValid(FBookmark) then
    CDS.GotoBookmark(FBookmark);

  SpinEdit1.Enabled := RadioButton1.Checked;
  SpinEdit2.Enabled := RadioButton1.Checked;
  SpinEdit3.Enabled := RadioButton1.Checked;

  ComboBox1.Enabled := RadioButton2.Checked;
  ComboBox2.Enabled := RadioButton2.Checked;
  SpinEdit4.Enabled := RadioButton2.Checked;
  SpinEdit5.Enabled := RadioButton2.Checked;
end;

{ TDiskGeometry }

function TDiskGeometry.IsEmpty: boolean;
begin
  Result := (C = 0) or (H = 0) or (S = 0);
end;

function TDiskGeometry.Size: uint64;
begin
  Result := uint64(C) * H * S * 512;
end;

end.
