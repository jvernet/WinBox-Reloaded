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

unit frmAbout;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, ShellAPI;

type
  TAboutFrm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Image1: TImage;
    Shape1: TShape;
    procedure Label8Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutFrm: TAboutFrm;

const
  WelcomeImg = 'WELCOME';

implementation

{$R *.dfm}

uses uCommUtil;

procedure TAboutFrm.FormCreate(Sender: TObject);
begin
  LoadImage('WELCOME', Image1);
end;

procedure TAboutFrm.Label8Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar((Sender as TControl).Hint), nil, nil, SW_SHOWNORMAL);
end;

end.
