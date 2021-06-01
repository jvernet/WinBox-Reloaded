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

unit frm86Box;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Buttons, IniFiles, ShellAPI,
  uWinBox, u86Box, uPicturePager;

type
  TCategoryPanel =  class (ExtCtrls.TCategoryPanel)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TFrame86Box = class(TFrame, IWinBoxFrame)
    Splitter1: TSplitter;
    CatPanels: TCategoryPanelGroup;
    CatPorts: TCategoryPanel;
    lbLPT: TLabel;
    Label5: TLabel;
    lbCOM: TLabel;
    Label7: TLabel;
    CatInput: TCategoryPanel;
    lbJoystick: TLabel;
    Label27: TLabel;
    lbMouse: TLabel;
    Label29: TLabel;
    CatNetwork: TCategoryPanel;
    lbNetType: TLabel;
    Label31: TLabel;
    lbNetCard: TLabel;
    Label33: TLabel;
    CatStorage: TCategoryPanel;
    lbSCSI: TLabel;
    lbCD: TLabel;
    lbHDD: TLabel;
    lbFloppy: TLabel;
    Label23: TLabel;
    Label25: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label8: TLabel;
    lbExStor: TLabel;
    CatSound: TCategoryPanel;
    lbMidi: TLabel;
    Label19: TLabel;
    lbAudio: TLabel;
    Label21: TLabel;
    CatVideo: TCategoryPanel;
    lb3Dfx: TLabel;
    Label9: TLabel;
    lbVGA: TLabel;
    Label12: TLabel;
    CatSystem: TCategoryPanel;
    lbEmulator: TLabel;
    Label3: TLabel;
    lbPC: TLabel;
    Label13: TLabel;
    lbMemSize: TLabel;
    lbCPU: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    BotBevel: TBevel;
    btnPrinter: TButton;
    Label1: TLabel;
    RightPanel: TPanel;
    lbHostCPU: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    lbHostRAM: TLabel;
    lbScreenshots: TLabel;
    Label20: TLabel;
    lbDiskSize: TLabel;
    pbHostCPU: TProgressBar;
    pbHostRAM: TProgressBar;
    btnImgNext: TButton;
    btnImgPrev: TButton;
    lbState: TLabel;
    Bevel1: TBevel;
    Label2: TLabel;
    TopPanel: TPanel;
    lbTitle: TLabel;
    btnWorkDir: TSpeedButton;
    Screenshots: TPicturePager;
    procedure btnVMClick(Sender: TObject);
    procedure ScreenshotsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScreenshotsUpdate(Sender: TObject);
  private
    procedure RecalcDiskSize;
  public
    Ratio: extended;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateFull;
    procedure UpdateDelta;
    procedure ReleaseFiles;
    procedure FolderChange(Sender: TObject; FileName: string; ChangeType: Cardinal);
  end;

resourcestring
  StrCPUD = '%d%%';
  Str86BoxVersionStr = '86Box %s';

implementation

{$R *.dfm}

uses uCoreModule, uCommUtil, uLang;

resourcestring
  StrPrinter = 'printer';
  StrScreenshots = 'screenshots';

{ Tfrm86Box }

procedure TFrame86Box.btnVMClick(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1: Core.acWorkDir.Execute;
    2: Core.acPrinterDir.Execute;
    3: Core.acScreenshotDir.Execute;
  end;
end;

constructor TFrame86Box.Create(AOwner: TComponent);
begin
  inherited;
  SetWindowLong(CatPanels.Handle, GWL_STYLE,
    GetWindowLong(CatPanels.Handle, GWL_STYLE) and not WS_BORDER);

  Screenshots.AspectX := 4;
  Screenshots.AspectY := 3;

  RightPanel.FullRepaint := false;

  Language.Translate('86Box', Self);
end;

destructor TFrame86Box.Destroy;
begin
  inherited;
end;

procedure TFrame86Box.FolderChange(Sender: TObject; FileName: string;
  ChangeType: Cardinal);
var
  I: integer;
begin
  FileName := UpperCase(FileName);
  I := pos(PathDelim, FileName);
  if I <> 0 then
    FileName := Copy(FileName, 1, I - 1);

  if FileName = '86BOX.CFG' then
    UpdateFull
  else if FileName= 'SCREENSHOTS' then begin
    Screenshots.UpdateList;
  end
  else if FileName = 'PRINTER' then begin
    Core.acPrinterDir.Update;
    btnPrinter.Enabled := Core.acPrinterDir.Enabled;
  end;

  RecalcDiskSize;
end;

procedure TFrame86Box.RecalcDiskSize;
var
  Files: TStringList;
begin
  if Core.ItemIndex = -1 then exit;

  Files := TStringList.Create;
  lbDiskSize.Caption := FileSizeToStr(
    GetFiles(Core.Profiles[Core.ItemIndex].WorkingDirectory, true, Files), 2);
  Files.Free;
end;

procedure TFrame86Box.ReleaseFiles;
begin
  Screenshots.Folder := '';
end;

procedure TFrame86Box.ScreenshotsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Mouse: TPoint;
begin
  if Screenshots.ItemIndex <> -1 then
    case Button of
      mbLeft: ShellExecute(Handle, 'open',
        PChar(Screenshots.FileName[Screenshots.ItemIndex]), nil, nil, SW_SHOWNORMAL);
      mbRight:
        if GetCursorPos(Mouse) then
          ShowSysPopup(Screenshots.FileName[Screenshots.ItemIndex], Mouse.X, Mouse.Y, Handle);
    end;
end;

procedure TFrame86Box.ScreenshotsUpdate(Sender: TObject);
begin
  Label2.Visible := Screenshots.ItemIndex = -1;
end;

procedure TFrame86Box.UpdateDelta;
begin
  if Core.ItemIndex = -1 then exit;

  with Core.Profiles[Core.ItemIndex] do begin
    lbState.Caption := TWinBoxProfile.StateToStr(State);
    pbHostCPU.Position := round(PercentCPU);
    ColorProgress(pbHostCPU);
    lbHostCPU.Caption := format(StrCPUD, [pbHostCPU.Position]);
    pbHostRAM.Position := round(PercentRAM);
    ColorProgress(pbHostRAM);
    lbHostRAM.Caption := format('%d%% (%s)', [
      pbHostRAM.Position, FileSizeToStr(BytesOfRAM, 0)]);
  end;
end;

procedure TFrame86Box.UpdateFull;
var
  Config: TCustomIniFile;
begin
  if Core.ItemIndex = -1 then exit;

  with Core.Profiles[Core.ItemIndex] do begin
    lbTitle.Caption := FriendlyName;

    Self.Color := Color;
    Self.Font.Color := GetTextColor(Self.Color);

    lbState.Font.Color := Self.Font.Color;

    CatPanels.HeaderFont.Color := Self.Font.Color;
    CatPanels.ChevronColor := Self.Font.Color;
    CatPanels.GradientBaseColor := Self.Color;
    CatPanels.GradientColor := Self.Color;

    lbScreenshots.Font.Color := GetLinkColor(Self.Color);

    Config := nil;
    if OpenConfig(Config) and Assigned(Config) then
      with T86Box do begin
        lbPC.Caption := GetDeviceName(Config, 0, true);
        lbMemSize.Caption := FormatMemSize(Config);
        lbCPU.Caption := FormatCPU(Config);

        lbVGA.Caption := GetDeviceName(Config, 2, true);
        lb3Dfx.Caption := Format3Dfx(Config);

        lbAudio.Caption := GetDeviceName(Config, 3, true);
        lbMidi.Caption := GetDeviceName(Config, 4, true);

        lbFloppy.Caption := FormatFDDs(Config);
        lbHDD.Caption := FormatHDDs(Config);
        lbCD.Caption := FormatCDROMs(Config);
        lbExStor.Caption := FormatExStors(Config);
        lbSCSI.Caption := GetDeviceName(Config, 11, true);

        lbNetCard.Caption := GetDeviceName(Config, 5, true);
        lbNetType.Caption := GetDeviceName(Config, 6, true);

        lbMouse.Caption := GetDeviceName(Config, 12, true);
        lbJoystick.Caption := ResolveDevName(GetDeviceData(Config, 5), SJoystickNameDefs);

        lbCOM.Caption := FormatCOMs(Config);
        lbLPT.Caption := FormatLPTs(Config);

        with T86Box.GetImgAspect(Config) do begin
          Screenshots.AspectX := X;
          Screenshots.AspectY := Y;
        end;

        Config.Free;
      end;

    Core.acWorkDir.Hint := ExcludeTrailingPathDelimiter(WorkingDirectory);
    Core.acPrinterDir.Hint := IncludeTrailingPathDelimiter(WorkingDirectory) + StrPrinter;
    Core.acScreenshotDir.Hint := IncludeTrailingPathDelimiter(WorkingDirectory) + StrScreenshots;

    lbEmulator.Caption := format(Str86BoxVersionStr, [FileVersion(ExecutablePath)]);

    Core.acPrinterDir.Update;
    btnPrinter.Enabled := Core.acPrinterDir.Enabled;
    Screenshots.Folder := Core.acScreenshotDir.Hint;
    RecalcDiskSize;

    CatPanels.Invalidate;
  end;

  UpdateDelta;
end;

{ TCategoryPanel }

constructor TCategoryPanel.Create(AOwner: TComponent);
begin
  inherited;
  BevelOuter := bvNone;
end;

end.
