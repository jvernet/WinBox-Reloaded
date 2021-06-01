unit frmSplash;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TWinBoxSplash = class(TForm)
    Image1: TImage;
    Shape1: TShape;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WinBoxSplash: TWinBoxSplash;

implementation

{$R *.dfm}

uses uCommUtil;

procedure TWinBoxSplash.FormCreate(Sender: TObject);
begin
  LoadImage('SPLASH', Image1);
  Screen.Cursor := crAppStart;
end;

end.
