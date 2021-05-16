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

unit uPicturePager;

interface

uses Windows, SysUtils, Classes, Controls, StdCtrls, Graphics,
     GDIPAPI, GDIPOBJ;

(*  Fájllista alapú sokrétû GDI+ interpolált képmegjelenítõ control. *)

type
  TStretchMode = (smNone, smStretch, smFixAspect, smAutoAspect);

  TPicturePager = class(TGraphicControl)
  private
    FItemIndex: integer;
    FImage: TGPImage;
    FList: TStringList;
    FUpdate: TNotifyEvent;
    FFolder: string;
    FStretch: TStretchMode;
    FAspY: integer;
    FNext: TButton;
    FCenter: boolean;
    FAspX: integer;
    FPrev: TButton;
    FFilter: string;
    procedure SetItemIndex(const Value: integer);
    function GetCount: integer;
    function GetFileName(I: integer): string;
    procedure SetFolder(const Value: string);
    procedure SetAspX(const Value: integer);
    procedure SetAspY(const Value: integer);
    procedure SetCenter(const Value: boolean);
    procedure SetNext(const Value: TButton);
    procedure SetPrev(const Value: TButton);
    procedure SetStretch(const Value: TStretchMode);
    procedure SetFilter(const Value: string);
  protected
    procedure RecalcAspect;
    procedure InternalUpdList;
    procedure UpdatePreview;
    procedure NextClick(Sender: TObject);
    procedure PrevClick(Sender: TObject);

    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateList;
    procedure UpdateControls;

    function CanNext: boolean;
    function CanPrev: boolean;

    procedure SetFileList(Strings: TStrings);

    procedure Next;
    procedure Prev;

    property AspectX: integer read FAspX write SetAspX;
    property AspectY: integer read FAspY write SetAspY;
    property FileName[I: integer]: string read GetFileName; default;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property ButtonNext: TButton read FNext write SetNext;
    property ButtonPrev: TButton read FPrev write SetPrev;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property Center: boolean read FCenter write SetCenter;
    property Count: integer read GetCount;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Cursor;
    property Filter: string read FFilter write SetFilter;
    property Folder: string read FFolder write SetFolder;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property Stretch: TStretchMode read FStretch write SetStretch;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Tag;
    property Visible;
    property OnUpdate: TNotifyEvent read FUpdate write FUpdate;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnContextPopup;
    property OnGesture;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('WinBox', [TPicturePager]);
end;

{ TScreenshotPager }

function TPicturePager.CanAutoSize(var NewWidth,
  NewHeight: Integer): Boolean;
begin
  Result := Assigned(FImage);
  if Result then begin
    if Align in [alNone, alLeft, alRight] then
      NewWidth := FImage.GetWidth;
    if Align in [alNone, alTop, alBottom] then
      NewHeight := FImage.GetHeight;
  end;
end;

function TPicturePager.CanNext: boolean;
begin
  Result := FItemIndex < FList.Count - 1;
end;

function TPicturePager.CanPrev: boolean;
begin
  Result := FItemIndex > 0;
end;

constructor TPicturePager.Create(AOwner: TComponent);
begin
  FItemIndex := -1;
  FList := TStringList.Create;
  FAspY := 1;
  FAspX := 1;
  FStretch := smAutoAspect;
  FCenter := true;
  FFilter := '*.png';
  inherited;
end;

destructor TPicturePager.Destroy;
begin
  FList.Free;
  inherited;
end;

function TPicturePager.GetCount: integer;
begin
  Result := FList.Count;
end;

function TPicturePager.GetFileName(I: integer): string;
begin
  if (I >= 0) and (I < FList.Count) then
    Result := FList[I]
  else
    raise Exception.Create(SysErrorMessage(ERROR_INVALID_INDEX));
end;

procedure TPicturePager.InternalUpdList;
begin
  if FItemIndex >= FList.Count then
    FItemIndex:= FList.Count - 1;
  if FItemIndex < -1 then
    FItemIndex := -1;

  if (FList.Count > 0) and (FItemIndex = -1) then
    FItemIndex := 0;

  UpdateControls;
  UpdatePreview;
end;

procedure TPicturePager.Next;
begin
  if CanNext then
    inc(FItemIndex);

  UpdatePreview;
end;

procedure TPicturePager.NextClick(Sender: TObject);
begin
  Next;
  UpdateControls;
end;

procedure TPicturePager.Paint;
var
  Graphics: TGPGraphics;
  W, H, L, T: integer;
begin
  if csDesigning in ComponentState then
    with Canvas do begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;

  if Assigned(FImage) then begin
      Graphics := TGPGraphics.Create(Canvas.Handle);
      try
        Graphics.SetCompositingMode(CompositingModeSourceCopy);
        Graphics.SetInterpolationMode(InterpolationModeBilinear);
        Graphics.SetPixelOffsetMode(PixelOffsetModeHighQuality);
        Graphics.SetSmoothingMode(SmoothingModeHighQuality);

        if FStretch <> smNone then begin
          if ClientWidth * FAspY > ClientHeight * FAspX then begin
            W := ClientHeight * FAspX div FAspY;
            H := ClientHeight;
          end
          else begin
            W := ClientWidth;
            H := ClientWidth * FAspY div FAspX;
          end
        end
        else begin
          W := FImage.GetWidth;
          H := FImage.GetHeight;
        end;

        if FCenter then begin
          L := (ClientWidth - W) div 2;
          T := (ClientHeight - H) div 2;
        end
        else begin
          L := 0;
          T := 0;
        end;

        Graphics.DrawImage(FImage, L, T, W, H);
      finally
        Graphics.Free;
      end;
  end;
end;

procedure TPicturePager.Prev;
begin
  if CanPrev then
    dec(FItemIndex);

  UpdatePreview;
end;

procedure TPicturePager.PrevClick(Sender: TObject);
begin
  Prev;
  UpdateControls;
end;

procedure TPicturePager.RecalcAspect;
begin
  case FStretch of
    smNone: FAspX := 0;
    smStretch: begin
                 FAspX := ClientWidth;
                 FAspY := ClientHeight;
               end;
    smAutoAspect: if Assigned(FImage) then begin
                    FAspX := FImage.GetWidth;
                    FAspY := FImage.GetHeight;
                  end
                  else
                    FAspX := 0;
  end;

  if (FAspX = 0) or (FAspY = 0) then begin
    FAspX := 1;
    FAspY := 1;
  end;
end;

procedure TPicturePager.Resize;
begin
  inherited;
  if FStretch = smStretch then
    RecalcAspect;
end;

procedure TPicturePager.SetAspX(const Value: integer);
begin
  if Value <= 0 then
    raise Exception.Create(SysErrorMessage(ERROR_INVALID_PARAMETER));

  if (FStretch <> smFixAspect) then
    exit;

  FAspX := Value;
  Invalidate;
end;

procedure TPicturePager.SetAspY(const Value: integer);
begin
  if Value <= 0 then
    raise Exception.Create(SysErrorMessage(ERROR_INVALID_PARAMETER));

  if (FStretch <> smFixAspect) then
    exit;

  FAspY := Value;
  Invalidate;
end;

procedure TPicturePager.SetCenter(const Value: boolean);
begin
  FCenter := Value;
  Invalidate;
end;

procedure TPicturePager.SetFileList(Strings: TStrings);
begin
  FList.Clear;
  FList.AddStrings(Strings);
  InternalUpdList;
end;

procedure TPicturePager.SetFilter(const Value: string);
begin
  if Value = '' then
    FFilter := '*.*'
  else
    FFilter := Value;

  UpdateList;
end;

procedure TPicturePager.SetFolder(const Value: string);
begin
  FFolder := Value;
  UpdateList;
end;

procedure TPicturePager.SetItemIndex(const Value: integer);
begin
  if csLoading in ComponentState then
    exit;

  if (Value >= -1) and (Value < FList.Count) then begin
    FItemIndex := Value;
    UpdateControls;
    UpdatePreview;
  end
  else
    raise Exception.Create(SysErrorMessage(ERROR_INVALID_INDEX));
end;

procedure TPicturePager.SetNext(const Value: TButton);
begin
  if Assigned(FNext) and not Assigned(Value) then
    with FNext do begin
      OnClick := nil;
      Enabled := true;
    end;

  FNext := Value;
  UpdateControls;
end;

procedure TPicturePager.SetPrev(const Value: TButton);
begin
  if Assigned(FPrev) and not Assigned(Value) then
    with FPrev do begin
      OnClick := nil;
      Enabled := true;
    end;

  FPrev := Value;
  UpdateControls;
end;

procedure TPicturePager.SetStretch(const Value: TStretchMode);
begin
  FStretch := Value;
  RecalcAspect;
  Invalidate;
end;

procedure TPicturePager.UpdateControls;
begin
  if Assigned(FNext) then begin
    FNext.Enabled := CanNext;
    FNext.OnClick := NextClick;
  end;

  if Assigned(FPrev) then begin
    FPrev.Enabled := CanPrev;
    FPrev.OnClick := PrevClick;
  end;
end;

procedure TPicturePager.UpdateList;
var
  SearchResult: TSearchRec;
  Path: string;
begin
  FList.Clear;
  if DirectoryExists(FFolder) then begin
    Path := IncludeTrailingPathDelimiter(FFolder);
    if FindFirst(Path + FFilter, faAnyFile, SearchResult) = 0 then begin
      repeat
        if (SearchResult.Attr and faDirectory) = 0 then
          FList.Add(Path + SearchResult.Name);
      until FindNext(SearchResult) <> 0;

      FindClose(SearchResult);
    end;
  end;
  InternalUpdList;
end;

procedure TPicturePager.UpdatePreview;
begin
  if Assigned(FImage) then
    FreeAndNil(FImage);

  if (FItemIndex >= 0) and (FItemIndex < FList.Count) then
    FImage := TGPImage.Create(FList[FItemIndex]);

  if Assigned(FUpdate) then
    FUpdate(Self);

  if FStretch = smAutoAspect then
    RecalcAspect;

  if AutoSize then
    AdjustSize;
  Invalidate;
end;

end.
