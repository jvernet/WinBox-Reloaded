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

unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Edit1: TEdit;
    Button6: TButton;
    Button7: TButton;
    Edit2: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  uCommUtil, uWinBoxLib;

procedure TForm1.Button1Click(Sender: TObject);
var
  Data: TDiskData;
begin
  try
  FillChar(Data, SizeOf(Data), #0);
  Screen.Cursor := crHourGlass;
  with CreateSelectHDD(Self) do begin
    Data.szConnector := 'IDE';
    Data.dgPhysicalGeometry.C := 723;
    Data.dgPhysicalGeometry.H := 11;
    Data.dgPhysicalGeometry.S := 63;
    SetDiskData(Data);

    LocatePhysCHS();

    SetConnectorFilter(false);
    SetCHSFilter(true);

    if Execute(true) then
      Data := GetDiskData;

    ShowMessage(FileSizeToStr(Data.dgPhysicalGeometry.Size, 2, 1000) + ' ' +
      String(Data.szManufacturer) + ' ' + String(Data.szModel));
    Data.dwNominalSize := 0;
  end;
  finally
    Screen.Cursor := crArrow;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  with CreateAutoUpdate(Self) do begin
    Execute(true);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  with CreateAutoUpdate(Self) do begin
    Execute(false);
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  with CreateAutoUpdate(Self) do begin
    ShowMessage(BoolToStr(HasUpdate, true));
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  D: TDiskData;
begin
  FillChar(D, SizeOf(D), #0);
  //D.szConnector := 'XT IDE';
  with CreateWizardHDD(Self) do begin
    DiskData := D;
    SetConnectorFilter(true);
    Execute(false);
    D := DiskData;
    ShowMessage(FileSizeToStr(D.dgTranslatedGeometry.Size, 2, 1000));
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  CreateWizardVM(Self).Execute(true);
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  ShowMessage(CompactFileNameTo(Edit1.Text, Edit2.Text));
end;

function TextLeft(const S: string; const Separator: char = '.'): string;
var
  P: integer;
begin
  P := pos(Separator, S);
  if P <> 0 then
    Result := Copy(S, 1, P - 1)
  else
    Result := '';
end;

function TextRight(const S: string; const Separator: char = '.'): string;
var
  P: integer;
begin
  P := pos(Separator, S);
  if P <> 0 then
    Result := Copy(S, P + 1, MaxInt)
  else
    Result := '';
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  Button1.Caption := TextLeft(Edit1.Text);
  Button2.Caption := TextRight(Edit1.Text);
end;

end.
