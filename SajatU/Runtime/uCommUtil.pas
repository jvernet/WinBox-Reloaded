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

unit uCommUtil;

interface

uses Windows, SysUtils, Classes, Graphics, ComObj, ShellAPI,
     ShlObj, ExtCtrls, ComCtrls, WinCodec;

procedure dbgLogFmt(const S: string; const Data: array of const);
procedure dbgLog(const S: string); inline;

function GetFiles(dir: string; subdir: Boolean; List: TStringList): uint64;

function FileSizeToStr(const Value: uint64; const Round: byte;
  const Divisor: extended = 1024): string;

procedure LoadImage(const Name: string; Image: TImage);
procedure ColorProgress(const Control: TProgressBar); inline;

//színek a háttérszín alapján
function GetTextColor(const Color: TColor): TColor;
function GetLinkColor(Color: TColor): TColor;

function RandomColor: TColor;

function FileVersion(const FileName: string): string;
//function DeleteToBin(FileName: string): boolean;
function DeleteWithShell(FileName: string; const AllowUndo: boolean = true): boolean;

function GetFileTime(const FileName: string): TDateTime;

{$EXTERNALSYM StrCmpLogicalW}
function StrCmpLogicalW(psz1, psz2: PWideChar): Integer; stdcall; external 'shlwapi.dll';

//Source: https://stackoverflow.com/questions/1581975/how-to-pop-up-the-windows-context-menu-for-a-given-file-using-delphi/1584204
procedure ShowSysPopup(aFile: string; x, y: integer; HND: HWND);

function TextRight(const S: string; const Separator: char = '.'): string;
function TextLeft(const S: string; const Separator: char = '.'): string;

function CommandLineToArgs(const CommandLine: string): TStringList;
function ExpandFileNameTo(const FileName, BaseDir: string): string;
function CompactFileNameTo(const FileName, BaseDir: string): string;

function CommandLineToArgvW(lpCmdLine: LPCWSTR; var pNumArgs: integer): PPWideChar; stdcall; external 'shell32.dll';
{$EXTERNALSYM CommandLineToArgvW}

function PathIsRelativeW(pszPath: LPCWSTR): BOOL; stdcall; external 'shlwapi.dll';
{$EXTERNALSYM PathIsRelativeW}

function PathCanonicalizeW(lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall;
  external 'shlwapi.dll';
{$EXTERNALSYM PathCanonicalizeW}

function PathRelativePathToW(pszPath: PChar; pszFrom: PChar; dwAttrFrom: DWORD;
  pszTo: PChar; dwAttrTo: DWORD): LongBool; stdcall; external 'shlwapi.dll';
{$EXTERNALSYM PathRelativePathToW}

procedure BringWindowToFront(const Handle: HWND);
function GetWindowTitle(const hwnd: HWND): string;
function GetWindowClass(const hwnd: HWND): string;

function CanLockFile(const FileName: string; const Access: DWORD = GENERIC_READ or GENERIC_WRITE): boolean;

function RegReadMulti(const Key: HKEY; const ValueName: string; Strings: TStrings): boolean;
function RegWriteMulti(const Key: HKEY; const ValueName: string; Strings: TStrings): boolean;

implementation

function RegReadMulti(const Key: HKEY; const ValueName: string; Strings: TStrings): boolean;
var
  Buffer, Temp: PChar;
  ValueType, ValueLen: DWORD;
begin
  Result := RegQueryValueEx(Key, PChar(ValueName), nil, @ValueType, nil, @ValueLen) = ERROR_SUCCESS;
  if Result and (ValueType = REG_MULTI_SZ) then begin
    GetMem(Buffer, ValueLen);
    try
      Result := RegQueryValueEx(Key, PChar(ValueName), nil, nil, PByte(Buffer), @ValueLen) = ERROR_SUCCESS;
      Temp := Buffer;
      if Result then
        while Temp^ <> #0 do begin
          Strings.Add(Temp);
          Temp := PChar(Pointer(Integer(Temp) + (lstrlen(Temp) + 1) * SizeOf(Char)));
        end;
    finally
      FreeMem(Buffer, ValueLen);
    end;
  end;
end;

function RegWriteMulti(const Key: HKEY; const ValueName: string; Strings: TStrings): boolean;
var
  Text, S: string;
begin
  Text := '';
  for S in Strings do
    Text := Text + S + #0;
  Text := Text + #0;

  Result := RegSetValueEx(Key, PChar(ValueName), 0, REG_MULTI_SZ,
    Pointer(@Text[1]), Length(Text) * SizeOf(Char)) = ERROR_SUCCESS;
end;

function CanLockFile(const FileName: string; const Access: DWORD): boolean;
var
  Handle: THandle;
begin
  Handle := CreateFile(PChar(FileName), Access,
                       0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

  Result := (Handle <> 0) and (Handle <> INVALID_HANDLE_VALUE);
  if Result then
    CloseHandle(Handle);
end;

procedure dbgLogFmt(const S: string; const Data: array of const);
begin
  OutputDebugStringW(PChar(format(S, Data)));
end;

procedure dbgLog(const S: string);
begin
  OutputDebugStringW(PChar(S));
end;

function GetWindowTitle(const hwnd: HWND): string;
var
  Buffer: array [0..MAX_PATH + 1] of char;
begin
  GetWindowText(hwnd, @Buffer, MAX_PATH);
  Result := string(Buffer);
end;

function GetWindowClass(const hwnd: HWND): string;
var
  Buffer: array [0..MAX_PATH + 1] of char;
begin
  GetClassName(hwnd, @Buffer, MAX_PATH);
  Result := string(Buffer);
end;

procedure BringWindowToFront(const Handle: HWND);
var
  Placement: TWindowPlacement;
begin
  Placement.length := SizeOf(Placement);

  if GetWindowPlacement(Handle, Placement) and
     ((Placement.showCmd and SW_SHOWMINIMIZED) <> 0) then begin
       Placement.showCmd := SW_SHOWNORMAL;
       SetWindowPlacement(Handle, Placement);
     end;
  BringWindowToTop(Handle);
end;

function CompactFileNameTo(const FileName, BaseDir: string): string;
var
  Path: array[0 .. MAX_PATH - 1] of char;
begin
  PathRelativePathToW(@Path[0], PChar(BaseDir),
     FILE_ATTRIBUTE_DIRECTORY, PChar(FileName), 0);

  Result := Path;
end;

function ExpandFileNameTo(const FileName, BaseDir: string): string;
var
  Buffer: array [0..MAX_PATH-1] of Char;
begin
  if PathIsRelativeW(PChar(FileName)) then begin
    Result := IncludeTrailingPathDelimiter(BaseDir)+FileName;
  end else begin
    Result := FileName;
  end;
  if PathCanonicalizeW(@Buffer[0], PChar(Result)) then begin
    Result := Buffer;
  end;
end;

function CommandLineToArgs(const CommandLine: string): TStringList;
var
  Buffer: PPWideChar;
  Count, I: integer;
begin
  Result := TStringList.Create;
  Buffer := CommandLineToArgvW(PChar(CommandLine), Count);
  if Buffer <> nil then
    for I := 0 to Count - 1 do
      Result.Add(PPWideChar(Integer(Buffer) + I * SizeOf(Pointer))^);
  LocalFree(THandle(Buffer));
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

function GetFileTime(const FileName: string): TDateTime;
var
  SystemTime, LocalTime: TSystemTime;
  fad: TWin32FileAttributeData;
begin
  if GetFileAttributesEx(PChar(FileName), GetFileExInfoStandard, @fad) and
     FileTimeToSystemTime(fad.ftLastWriteTime, SystemTime) and
     SystemTimeToTzSpecificLocalTime(nil, SystemTime, LocalTime) then
       Result := SystemTimeToDateTime(LocalTime)
  else
       Result := 0;
end;

function DeleteWithShell(FileName: string; const AllowUndo: boolean): boolean;
var
  FileOp: TSHFileOpStruct;
begin
  FillChar(FileOp, SizeOf(FileOp), #0);
  FileName := FileName + #0#0;
  with FileOp do begin
    wFunc := FO_DELETE;
    pFrom := @FileName[1];
    fFlags := FOF_SILENT or FOF_NOCONFIRMATION;

    if AllowUndo then
      fFlags := fFlags or FOF_ALLOWUNDO;
  end;
  Result := SHFileOperation(FileOp) = 0;
end;

(*
function DeleteToBin(FileName: string): boolean;
var
  FileOp: TSHFileOpStruct;
begin
  if FileExists(FileName) or DirectoryExists(FileName) then begin
    FillChar(FileOp, SizeOf(FileOp), #0);
    FileName := FileName + #0#0;
    with FileOp do begin
      wFunc := FO_DELETE;
      pFrom := @FileName[1];
      fFlags := FOF_SILENT or FOF_NOCONFIRMATION or FOF_ALLOWUNDO;
    end;
    Result := SHFileOperation(FileOp) = 0;
  end
  else
    Result := false;
end;
*)

function FileVersion(const FileName: string): string;
var
  VerInfoSize: Cardinal;
  VerValueSize: Cardinal;
  Dummy: Cardinal;
  PVerInfo: Pointer;
  PVerValue: PVSFixedFileInfo;
begin
  Result := '';
  if not FileExists(FileName) then
    exit;

  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);
  GetMem(PVerInfo, VerInfoSize);
  try
    if GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, PVerInfo) then
      if VerQueryValue(PVerInfo, '\', Pointer(PVerValue), VerValueSize) then
        with PVerValue^ do
          Result := Format('v%d.%d', [
            HiWord(dwFileVersionMS),
            LoWord(dwFileVersionMS)]);
  finally
    FreeMem(PVerInfo, VerInfoSize);
  end;
end;

procedure LoadImage(const Name: string; Image: TImage);
var
  Bitmap: TWICImage;
  Stream: TResourceStream;
  Factory: IWICImagingFactory;
  Scaler: IWICBitmapScaler;
begin
  Bitmap := TWICImage.Create;
  Stream := TResourceStream.Create(hInstance, Name, RT_RCDATA);
  try
    Factory := TWICImage.ImagingFactory;
    Bitmap.LoadFromStream(Stream);
    Factory.CreateBitmapScaler(Scaler);
    Scaler.Initialize(Bitmap.Handle, Image.Width, Image.Height,
      WICBitmapInterpolationModeHighQualityCubic);
    Bitmap.Handle := IWICBitmap(Scaler);
    Scaler := nil;
    Factory := nil;
  finally
    Stream.Free;
    Image.Picture.Assign(Bitmap);
    Bitmap.Free;
  end;
end;

procedure ColorProgress(const Control: TProgressBar); inline;
begin
  if Control.Position > 90 then
    Control.State := pbsError
  else if Control.Position > 75 then
    Control.State := pbsPaused
  else
    Control.State := pbsNormal;
end;

function GetTextColor(const Color: TColor): TColor;
begin
  if (Color = clBtnFace) or (Color = clWindow) then
    Result := clWindowText
  else if 0.299 * GetRValue(Color) + 0.587 * GetGValue(Color) +
      0.114 * GetBValue(Color) > 127 then
    Result := clBlack
  else
    Result := clWhite;
end;

function RandomColor: TColor;
begin
  Result := rgb(random($100), random($100), random($100));
end;

function GetLinkColor(Color: TColor): TColor;
begin
  if (Color = clBtnFace) or (Color = clWindow) then
    Result := clHotLight
  else begin
    Color := ColorToRGB(Color);
    Result := RGB($FF - GetRValue(Color),
                  $FF - GetGValue(Color),
                  $FF - GetBValue(Color));
  end;
end;

function FileSizeToStr(const Value: uint64; const Round: byte;
  const Divisor: extended = 1024): string;
var
  I: integer;
  fValue: extended;
const
  szA: array [0..8] of char = (' ', 'k', 'M', 'G', 'T', 'P', 'E', 'P', 'Z');
begin
  fValue := Value;
  I := 0;
  while (fValue >= Divisor) and (I < 9) do begin
    fValue := fValue / Divisor;
    inc(I);
  end;
  Result := FloatToStrF(fValue, ffNumber, 8, Round) + ' ' + szA[I] + 'B';
end;

function GetFiles(dir: string; subdir: Boolean; List: TStringList): uint64;
var
  rec: TSearchRec;
  found: Integer;
  size: _LARGE_INTEGER;
begin
  Result := 0;
  if dir[Length(dir)] <> '\' then dir := dir + '\';
  found := FindFirst(dir + '*.*', faAnyFile, rec);
  while found = 0 do
  begin

    size.QuadPart := 0;
    size.LowPart := GetCompressedFileSize(PChar(dir + rec.Name), @size.HighPart);
    Result := Result + uint64(size.QuadPart);

    if (rec.Attr and faDirectory > 0) and (rec.Name[1] <> '.') and (subdir = True) then
      Inc(Result, GetFiles(dir + rec.Name, True, List));

    if (rec.Attr and faDirectory = 0) then
      List.Add(dir + rec.Name);
    found := FindNext(rec);
  end;
  FindClose(rec);
end;

//Source: https://stackoverflow.com/questions/1581975/how-to-pop-up-the-windows-context-menu-for-a-given-file-using-delphi/1584204
procedure ShowSysPopup(aFile: string; x, y: integer; HND: HWND);
var
  Root: IShellFolder;
  ShellParentFolder: IShellFolder;
  chEaten,dwAttributes: ULONG;
  FilePIDL,ParentFolderPIDL: PItemIDList;
  CM: IContextMenu;
  Menu: HMenu;
  Command: LongBool;
  ICM2: IContextMenu2;

  ICI: TCMInvokeCommandInfo;
  ICmd: integer;
  P: TPoint;
Begin
  OleCheck(SHGetDesktopFolder(Root));//Get the Desktop IShellFolder interface

  OleCheck(Root.ParseDisplayName(HND, nil,
    PWideChar(WideString(ExtractFilePath(aFile))),
    chEaten, ParentFolderPIDL, dwAttributes)); // Get the PItemIDList of the parent folder

  OleCheck(Root.BindToObject(ParentFolderPIDL, nil, IShellFolder,
  ShellParentFolder)); // Get the IShellFolder Interface  of the Parent Folder

  OleCheck(ShellParentFolder.ParseDisplayName(HND, nil,
    PWideChar(WideString(ExtractFileName(aFile))),
    chEaten, FilePIDL, dwAttributes)); // Get the relative  PItemIDList of the File

  ShellParentFolder.GetUIObjectOf(HND, 1, FilePIDL, IID_IContextMenu, nil, CM); // get the IContextMenu Interace for the file

  if CM = nil then Exit;
  P.X := X;
  P.Y := Y;

  Windows.ClientToScreen(HND, P);

  Menu := CreatePopupMenu;

  try
    CM.QueryContextMenu(Menu, 0, 1, $7FFF, CMF_EXPLORE or CMF_CANRENAME);
    CM.QueryInterface(IID_IContextMenu2, ICM2); //To handle submenus.
    try
      Command := TrackPopupMenu(Menu, TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RIGHTBUTTON or
        TPM_RETURNCMD, p.X, p.Y, 0, HND, nil);
    finally
      ICM2 := nil;
    end;

    if Command then
    begin
      ICmd := LongInt(Command) - 1;
      FillChar(ICI, SizeOf(ICI), #0);
      with ICI do
      begin
        cbSize := SizeOf(ICI);
        hWND := 0;
        lpVerb := MakeIntResourceA(ICmd);
        nShow := SW_SHOWNORMAL;
      end;
      CM.InvokeCommand(ICI);
    end;
  finally
     DestroyMenu(Menu)
  end;
End;

initialization
  randomize;

end.
