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

library libWinBox;

uses
  MidasLib,
  SysUtils,
  Classes,
  Forms,
  uLang,
  frmSelectHDD in 'frmSelectHDD.pas' {HDSelect},
  frmUpdate in 'frmUpdate.pas' {UpdateForm},
  frmWizardHDD in 'frmWizardHDD.pas' {WizardHDD},
  frmWaitForm in 'frmWaitForm.pas' {WaitForm},
  uVMSample in 'uVMSample.pas',
  frmWizardVM in 'frmWizardVM.pas' {WizardVM};

{$R *.res}

function CreateWizardVM(const AOwner: TComponent): IWizardVM; stdcall;
begin
  Result := TWizardVM.Create(AOwner) as IWizardVM;
end;

function CreateAutoUpdate(const AOwner: TComponent): IAutoUpdate; stdcall;
begin
  Result := TUpdateForm.Create(AOwner) as IAutoUpdate;
end;

procedure SetLanguage(const AFileName, ALocale: PChar); stdcall;
begin
  Locale := String(ALocale);
  Language := TLanguage.Create(String(AFileName));
end;

exports
  CreateSelectHDD,
  CreateAutoUpdate,
  CreateWizardHDD,
  CreateWizardVM,
  SetLanguage;

begin
end.
