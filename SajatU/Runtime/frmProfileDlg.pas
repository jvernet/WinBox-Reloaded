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

unit frmProfileDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, GDIPAPI, GDIPOBJ, GDIPUTIL, IOUtils,
  uWinBox, uCommUtil, ExtDlgs, Menus;

type
  TProfileDialog = class(TForm)
    Label1: TLabel;
    imgIcon: TImage;
    edName: TEdit;
    Label2: TLabel;
    lbInternalID: TLabel;
    Label6: TLabel;
    edEmulator: TEdit;
    btnBrowse: TButton;
    btnDefault: TButton;
    Shape1: TShape;
    btnCancel: TButton;
    btnOK: TButton;
    Bevel1: TBevel;
    OpenExeDialog: TOpenDialog;
    pmIcon: TPopupMenu;
    miDefIcon: TMenuItem;
    allzs1: TMenuItem;
    OpenPicDlg: TOpenPictureDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    lbVersion: TLabel;
    Label5: TLabel;
    lbDate: TLabel;
    Label8: TLabel;
    edOptParams: TMemo;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure miDefIconClick(Sender: TObject);
    procedure miBrowseIconClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure edEmulatorChange(Sender: TObject);
  private
    { Private declarations }
  public
    Profile: TWinBoxProfile;
    IsIconChanged: boolean;
    IsFactoryIcon: boolean;
  end;

var
  ProfileDialog: TProfileDialog;

implementation

resourcestring
  EANévNemLehetÜres = 'A használni kívánt név nem lehet üres.';
  EHibaTörténtASPa = 'Hiba történt a kép mentése közben: failed to use encoder.';
  StrAKorábbanHasznált = 'Ha volt korábban használt ikon, az most a Lomtárba kerül.';
  StrIsmeretlenVerzió = 'Ismeretlen';

{$R *.dfm}

procedure TProfileDialog.btnOKClick(Sender: TObject);
begin
  if edName.Text = '' then
    raise Exception.Create(EANévNemLehetÜres);

  if not FileExists(edEmulator.Text) then
    raise Exception.Create(SysErrorMessage(ERROR_PATH_NOT_FOUND));

  Profile.FriendlyName := edName.Text;
  Profile.ExecutablePath := edEmulator.Text;
  Profile.OptionalParams := edOptParams.Text;
  Profile.SaveProfile;

  if IsIconChanged then begin
    if FileExists(Profile.WorkingDirectory + SVmIconPng) then begin
      MessageBox(Handle, PChar(StrAKorábbanHasznált),
        PChar(Application.Title), MB_ICONINFORMATION or MB_OK);
      DeleteToBin(Profile.WorkingDirectory + SVmIconPng);
    end;
    if not IsFactoryIcon then
      Profile.SaveIcon;
  end;

  ModalResult := mrOK;
end;

procedure TProfileDialog.edEmulatorChange(Sender: TObject);
begin
  if FileExists(edEmulator.Text) then begin
    lbVersion.Caption := Profile.InternalName + ' ' + FileVersion(edEmulator.Text);
    lbDate.Caption := DateToStr(GetFileTime(edEmulator.Text));
  end
  else begin
    lbVersion.Caption := StrIsmeretlenVerzió;
    lbDate.Caption := StrIsmeretlenVerzió;
  end;
end;

procedure TProfileDialog.btnCancelClick(Sender: TObject);
begin
  Profile.ReloadIcon;
  ModalResult := mrCancel;
end;

procedure TProfileDialog.miDefIconClick(Sender: TObject);
begin
  Profile.DefIcon;
  IsIconChanged := FileExists(Profile.WorkingDirectory + SVmIconPng);
  IsFactoryIcon := true;
  imgIcon.Picture.Assign(Profile.Icon);
end;

procedure TProfileDialog.miBrowseIconClick(Sender: TObject);
var
  Input: TGPImage;
  Output: TGPBitmap;
  Encoder: TGUID;
  Graphics: TGPGraphics;
  Temp: string;
begin
  if OpenPicDlg.Execute then begin
    Input := TGPImage.Create(OpenPicDlg.FileName);
    try
      Output := TGPBitmap.Create(64, 64, PixelFormat32bppARGB);
      try
        Graphics := TGPGraphics.Create(Output);
        try
          with Graphics do begin
            SetCompositingMode(CompositingModeSourceCopy);
            SetInterpolationMode(InterpolationModeHighQualityBicubic);
            SetPixelOffsetMode(PixelOffsetModeHighQuality);
            SetSmoothingMode(SmoothingModeHighQuality);
            DrawImage(Input, 0, 0, Output.GetWidth, Output.GetHeight);
          end;
        finally
          Graphics.Free;
        end;
        if GetEncoderClsid('image/png', Encoder) <> -1 then begin
          Temp := ChangeFileExt(TPath.GetTempFileName, '.png');
          Output.Save(Temp, Encoder);
          Profile.Icon.LoadFromFile(Temp);
          IsIconChanged := true;
          IsFactoryIcon := false;
          DeleteFile(Temp);

          imgIcon.Picture.Assign(Profile.Icon);

          GroupBox1.Invalidate;
          Bevel1.Invalidate;
          imgIcon.Invalidate;
        end
        else
          raise Exception.Create(EHibaTörténtASPa);
      finally
        Output.Free;
      end;
    finally
      Input.Free;
    end;
  end;
end;

procedure TProfileDialog.btnBrowseClick(Sender: TObject);
begin
  OpenExeDialog.FileName := edEmulator.Text;
  if OpenExeDialog.Execute then
    edEmulator.Text := OpenExeDialog.FileName;
end;

procedure TProfileDialog.btnDefaultClick(Sender: TObject);
begin
  edEmulator.Text := Profile.DefExecutablePath;
end;

procedure TProfileDialog.FormShow(Sender: TObject);
begin
  lbInternalID.Caption := Profile.ProfileID;
  edName.Text := Profile.FriendlyName;
  edEmulator.Text := Profile.ExecutablePath;
  edOptParams.Text := Profile.OptionalParams;

  IsIconChanged := false;
  IsFactoryIcon := false;
  imgIcon.Picture.Assign(Profile.Icon);
end;

end.
