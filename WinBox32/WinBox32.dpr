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

program WinBox32;

uses
  MidasLib,
  Windows,
  uCommUtil,
  Vcl.Forms,
  frmMainForm in 'frmMainForm.pas' {WinBoxMain},
  uCoreModule in 'uCoreModule.pas' {Core: TDataModule},
  frm86Box in 'frm86Box.pas' {Frame86Box: TFrame},
  u86Box in 'u86Box.pas',
  uWinBoxLib in 'uWinBoxLib.pas',
  frmProgSettDlg in 'frmProgSettDlg.pas' {ProgSettDlg},
  frmAbout in 'frmAbout.pas' {AboutFrm},
  frmSplash in 'frmSplash.pas' {WinBoxSplash};

{$R *.res}

var
  Handle: HWND;

begin
  Handle := FindWindow('TWinBoxMain', nil);
  if (Handle <> 0) then begin
    BringWindowToFront(Handle);
    Halt(1);
  end;

  if FindWindow('TWinBoxSplash', nil) <> 0 then
    Halt(2);

  Application.Initialize;
  WinBoxSplash := TWinBoxSplash.Create(Application);
  WinBoxSplash.Show;
  Application.ProcessMessages;

  Application.ActionUpdateDelay := 50;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'WinBox Reloaded';
  Application.CreateForm(TCore, Core);
  Application.CreateForm(TWinBoxMain, WinBoxMain);
  Application.CreateForm(TAboutFrm, AboutFrm);
  Application.Run;
end.
