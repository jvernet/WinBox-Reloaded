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

unit frmMainForm;

interface

uses
  Types, Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, GraphUtil, ComCtrls, ExtCtrls, StdCtrls, ImageList,
  ImgList, uProcProfile, uWinBox, VclTee.TeeGDIPlus, VCLTee.TeEngine, VCLTee.TeeProcs,
  VCLTee.Chart, VCLTee.Series, Vcl.Menus, Vcl.ToolWin, Vcl.VirtualImageList,
  Vcl.BaseImageCollection, Vcl.ImageCollection;

type
  TListBox = class(StdCtrls.TListBox)
  private
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
  end;

  TWinBoxMain = class(TForm)
    List: TListBox;
    Splitter: TSplitter;
    Pages: TPageControl;
    tabHome: TTabSheet;
    tabPerfMon: TTabSheet;
    tab86Box: TTabSheet;
    StatusBar: TStatusBar;
    Label10: TLabel;
    Label11: TLabel;
    ImgWelcome: TImage;
    pgCharts: TPageControl;
    TabSheet4: TTabSheet;
    ChartCPU: TChart;
    TabSheet5: TTabSheet;
    ChartRAM: TChart;
    TabSheet2: TTabSheet;
    ChartVMs: TChart;
    pnpBottom: TPanel;
    pnpRight: TPanel;
    lbHRAM: TLabel;
    pbRAM: TProgressBar;
    pnpLeft: TPanel;
    lbHCPU: TLabel;
    pbCPU: TProgressBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton11: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    TabSheet1: TTabSheet;
    ImageCollection: TImageCollection;
    ListImages: TVirtualImageList;
    ToolBar2: TToolBar;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    procedure ListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure ListClick(Sender: TObject);
    procedure ListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
  private
  protected
    procedure Resize; override;
  public
    //Lista kirajzolásához szükséges cuccok
    HalfCharHeight, BorderThickness: integer;
    clHighlight1, clHighlight2: TColor;

    //Átméretezés aránytartása
    Ratio: extended;

    constructor Create(AOwner: TComponent); override;
    procedure CoreReloadProfiles(Sender: TObject);
    procedure CoreMonitorUpdate(Sender: TObject);
    destructor Destroy; override;

    procedure ResetChart(Chart: TChart);
    procedure AddSeries(Chart: TChart; AColor: TColor; const FriendlyName: string);
    procedure AddValue(ASeries: TFastLineSeries; const Value: extended);

    procedure WMEnterSizeMove(var Message: TMessage); message WM_ENTERSIZEMOVE;
  end;

var
  WinBoxMain: TWinBoxMain;

implementation

{$R *.dfm}
{$R 'Data\rcWinBoxMain.res'}

uses uCoreModule, frm86Box, uCommUtil, uWinBoxLib, frmAbout;

resourcestring
  StrMemória1fS = 'Memória: %.1f%%'#13#10'(%s)';
  StrCPU1f = 'CPU: %.1f%%';
  StrIndítás = '&Indítás...';
  StrLeállítás = '&Leállítás...';
  StrCtrlAltDel = 'Ctrl+Alt+&Del';
  StrHWReset = 'H/W &Reset';
  StrBeállítások = '&Beállítások';
  StrElõtérbeHozás = '&Elõtérbe hozás';
  StrNyomtatótálca = '&Nyomtatótálca';
  StrNévjegy = 'Név&jegy...';
  StrListaFrissítése = 'Lista &frissítése';
  StrTeljesLeállítás = 'Teljes &leállítás';
  StrÚjMerevlemez = 'Új merev&lemez';
  StrÚjGép = '&Új gép';

const
  DefRatio = 0.28;
  MaxPoints = 60;
  ScrollPoints = 1;

procedure TWinBoxMain.AddSeries(Chart: TChart; AColor: TColor;
  const FriendlyName: string);
begin
  with Chart.AddSeries(TFastLineSeries.Create(Chart)) as TFastLineSeries do begin
    Title := FriendlyName;
    Color := AColor;
    AutoRepaint := true;
    XValues.Order := loNone;
    LinePen.OwnerCriticalSection := nil;
    FastPen := true;
  end;
end;

procedure TWinBoxMain.AddValue(ASeries: TFastLineSeries; const Value: extended);
var
  Temp: extended;
begin
  with ASeries do begin
    if Count > MaxPoints then begin
      Delete(0, ScrollPoints);

      Temp := XValues.Last;
      GetHorizAxis.SetMinMax(
        Temp - MaxPoints + ScrollPoints,
        Temp + ScrollPoints);
    end;

    if Count = 0 then
      AddXY(1, Value)
    else
      AddXY(XValues.Last + 1, Value);
  end;
end;

procedure TWinBoxMain.CoreMonitorUpdate(Sender: TObject);
var
  I: integer;

  Temp, SumCPU, SumRAM: extended;
  Count: TArray<integer>;

  SumBytesOfRAM: uint64;
begin
  SumCPU := 0; SumRAM := 0; SumBytesOfRAM := 0;
  SetLength(Count, Pages.PageCount - 2);

  with Core do begin
    for I := 0 to Profiles.Count - 1 do begin
      if Profiles[I].Count > 0 then
        inc(Count[Profiles[I].Tag - 2], Profiles[I].Count);

      Temp := Profiles[I].PercentCPU;
      AddValue(ChartCPU.Series[I] as TFastLineSeries, Temp);
      SumCPU := SumCPU + Temp;

      Temp := Profiles[I].PercentRAM;
      AddValue(ChartRAM.Series[I] as TFastLineSeries, Temp);
      SumRAM := SumRAM + Temp;

      SumBytesOfRAM := SumBytesOfRAM + Profiles[I].BytesOfRAM;
    end;
  end;

  for I := 0 to High(Count) do
    AddValue(ChartVMs.Series[I] as TFastLineSeries, Count[I]);

  pbCPU.Position := Round(SumCPU);
  ColorProgress(pbCPU);
  lbHCPU.Caption := format(StrCPU1f, [SumCPU]);

  pbRAM.Position := Round(SumRAM);
  ColorProgress(pbRAM);
  lbHRAM.Caption := format(StrMemória1fS, [SumRAM,
    FileSizeToStr(SumBytesOfRAM, 2)]);

  List.Invalidate;
  if Pages.ActivePageIndex > 1 then
    (Pages.ActivePage.Controls[0] as IWinBoxFrame).UpdateDelta;
end;

procedure TWinBoxMain.CoreReloadProfiles(Sender: TObject);
var
  I, L: Integer;
  cTemp: TColor;
begin
  ResetChart(ChartCPU);
  ResetChart(ChartRAM);
  for I := 0 to Core.Profiles.Count - 1 do begin
    cTemp := RandomColor;
    AddSeries(ChartCPU, cTemp, Core.Profiles[I].FriendlyName);
    AddSeries(ChartRAM, cTemp, Core.Profiles[I].FriendlyName);
  end;

  ResetChart(ChartVMs);
  for I := 2 to Pages.PageCount - 1 do begin
    cTemp := RandomColor;
    AddSeries(ChartVMs, cTemp, Pages.Pages[I].Caption);
  end;

  L := List.ItemIndex;

  with List.Items do begin
    BeginUpdate;
    Clear;
    Add(Pages.Pages[0].Caption);
    Add(Pages.Pages[1].Caption);
    for I := 0 to Core.Profiles.Count - 1 do
      Add(Core.Profiles[I].FriendlyName);
    EndUpdate;
  end;

  List.Invalidate;
  if L < List.Count then
    List.ItemIndex := L;

  if (List.Count > 0) and (List.ItemIndex = -1) then
    List.ItemIndex := 0;

  ListClick(List);
end;

constructor TWinBoxMain.Create(AOwner: TComponent);
var
  H, L, S: word;

  I: integer;
begin
  inherited;
  HalfCharHeight := Canvas.TextHeight('W');
  BorderThickness := (List.ItemHeight - ListImages.Height) div 2;

  clHighlight1 := ColorToRGB(clHighlight);
  ColorRGBToHLS(clHighlight1, H, L, S);
  L := L * 10 div 8;
  clHighlight2 := ColorHLSToRGB(H, L, S);

  Ratio := DefRatio;

  Core.OnMonitorUpdate := CoreMonitorUpdate;
  Core.OnReloadProfiles := CoreReloadProfiles;
  CoreReloadProfiles(Self);

  with TFrame86Box.Create(Self) do begin
    Parent := tab86Box;
    Align := alClient;
    //csak úgy scalel a TCategoryGroup ha nyitva van alapból -> be kell csukni itt
    for I := 0 to CatPanels.ControlCount - 1 do
      if CatPanels.Controls[I] <> CatSystem then
        (CatPanels.Controls[I] as TCategoryPanel).Collapsed := true;
  end;

  for I := 0 to Pages.PageCount - 1 do
    Pages.Pages[I].TabVisible := false;
  Pages.ActivePageIndex := 0;

  LoadImage(WelcomeImg, ImgWelcome);
end;

destructor TWinBoxMain.Destroy;
begin
  Core.OnReloadProfiles := nil;
  Core.OnMonitorUpdate := nil;
  inherited;
end;

procedure TWinBoxMain.FormShow(Sender: TObject);
begin
  ToolButton1.Caption := StrIndítás;
  ToolButton2.Caption := StrLeállítás;
  ToolButton4.Caption := StrCtrlAltDel;
  ToolButton5.Caption := StrHWReset;
  ToolButton6.Caption := StrBeállítások;
  ToolButton9.Caption := StrElõtérbeHozás;
  ToolButton10.Caption := StrNyomtatótálca;
  ToolButton8.Caption := StrNévjegy;

  ToolButton12.Caption := StrÚjGép;
  ToolButton13.Caption := StrÚjMerevlemez;
  ToolButton15.Caption := StrTeljesLeállítás;
  ToolButton16.Caption := StrListaFrissítése;
  ToolButton17.Caption := StrBeállítások;
  ToolButton22.Caption := StrNévjegy;

  ToolBar1.ShowCaptions := true;
  ToolBar2.ShowCaptions := true;

  pgCharts.ActivePageIndex := 0;

  ClientHeight := 442 * Screen.PixelsPerInch div 96;
end;

procedure TWinBoxMain.ListClick(Sender: TObject);
begin
  Core.ItemIndex := -1;
  Core.FolderMonitor.Active := false;
  case List.ItemIndex of
    -1: Pages.ActivePageIndex := 0;
    0, 1: Pages.ActivePageIndex := List.ItemIndex;
    else begin
      Core.ItemIndex := List.ItemIndex - 2;

      Core.FolderMonitor.Folder := Core.Profiles[Core.ItemIndex].WorkingDirectory;
      Core.FolderMonitor.Active := true;

      Pages.ActivePageIndex := Core.Profiles[Core.ItemIndex].Tag;
      (Pages.ActivePage.Controls[0] as IWinBoxFrame).UpdateFull;
    end;
  end;
  ToolBar1.Visible := List.ItemIndex > 1;
  ToolBar2.Visible := not ToolBar1.Visible;
end;

procedure TWinBoxMain.ListDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  StateText: string;
begin
  with Control as TListBox, Canvas do begin
    if odSelected in State then begin
      Brush.Color := clHighlight;
      Font.Color := clHighlightText;
      GradientFillCanvas(Canvas, clHighlight2, clHighlight1, Rect, gdVertical);
    end
    else begin
      Brush.Color := clWindow;
      Font.Color := clWindowText;
      FillRect(Rect);
    end;

    Brush.Style := bsClear;

    HalfCharHeight := TextHeight('W') div 2;
    if Index < 2 then begin
      ListImages.Draw(Canvas, Rect.Left + BorderThickness,
                      Rect.Top + BorderThickness, Index);

      Font.Style := [fsBold];
      TextOut(Rect.Left + ListImages.Width + 2 * BorderThickness + 1,
              (Rect.Top + Rect.Bottom) div 2 - HalfCharHeight,
              Items[Index]);
    end
    else begin
      StretchDraw(Types.Rect(Rect.Left + BorderThickness,
                             Rect.Top + BorderThickness,
                             Rect.Left + BorderThickness + ListImages.Width,
                             Rect.Top + BorderThickness + ListImages.Height),
                  Core.Profiles[Index - 2].Icon);

      Font.Style := [fsBold];
      TextOut(Rect.Left + ListImages.Width + 2 * BorderThickness + 1,
              Rect.Top + (Rect.Bottom - Rect.Top) div 3 - HalfCharHeight,
              Items[Index]);

      Font.Style := [];
      StateText := TWinBoxProfile.StateToStr(Core.Profiles[Index - 2].State);
      TextOut(Rect.Left + ListImages.Width + 2 * BorderThickness + 1,
              Rect.Top + (Rect.Bottom - Rect.Top) div 3 * 2 - HalfCharHeight,
              StateText);
    end;
  end;
end;

procedure TWinBoxMain.ListMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Mouse: TPoint;
  TempIndex: integer;
begin
  TempIndex := List.ItemIndex;
  List.ItemIndex := List.ItemAtPos(Point(X, Y), true);
  if (List.ItemIndex = -1) and (TempIndex >= 0) and (TempIndex < List.Count) then
    List.ItemIndex := TempIndex;

  ListClick(List);
  case Button of
    mbRight: if GetCursorPos(Mouse) then
               case List.ItemIndex of
                 0: Core.HomeMenu.Popup(Mouse.X, Mouse.Y);
                 1: Core.PerfMenu.Popup(Mouse.X, Mouse.Y);
                 else Core.VMMenu.Popup(Mouse.X, Mouse.Y);

                 //ha már ide eljutottál a klónozást, a backupot
                 //  az eltávolítást az átnevezést és az áthelyezést
                 //  tedd már be pls a VMMenu menübe is, thx
               end;
  end;
end;

procedure TWinBoxMain.ResetChart(Chart: TChart);
begin
  Chart.RemoveAllSeries;
  Chart.Axes.Bottom.Scroll(60 - Chart.Axes.Bottom.Maximum);
end;

procedure TWinBoxMain.Resize;
begin
  inherited;
  List.Width := Round(Ratio * ClientWidth);
end;

procedure TWinBoxMain.WMEnterSizeMove(var Message: TMessage);
begin
  if ClientWidth <> 0 then
    Ratio := List.Width / ClientWidth
  else
    Ratio := DefRatio;
end;

{ TListBox }

procedure TListBox.CNDrawItem(var Msg: TWMDrawItem);
begin
  with Msg.DrawItemStruct^ do
    itemState := itemState and not ODS_FOCUS;
  inherited;
end;

end.
