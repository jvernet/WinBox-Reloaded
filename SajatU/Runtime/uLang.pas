(*

    WinBox Reloaded R2 - An universal GUI for many emulators

    Copyright (C) 2020, Laci b�'

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

unit uLang;

interface

uses Windows, SysUtils, Classes, Controls, StdCtrls, Menus, ActnList,
     ExtCtrls, Forms, CheckLst, IniFiles;

const
  Codes: array [0..1, 0..10] of char =
    (('''', '"', '\', '0', 'a', 'b', 'f', 'n', 'r', 't', 'v'),
     ('''', '"', '\', #0,  #7,  #8,  #12, #10, #13,  #9, #11));

type
  TLanguage = class(TMemIniFile)
  public
    procedure Translate(const Section: string; Control: TControl); overload;
    procedure Translate(const Section: string; Control: TMenuItem); overload;
    procedure Translate(const Section: string; Control: TCustomActionList); overload;

    procedure GetTranslation(const Section: string; Control: TControl); overload;
    procedure GetTranslation(const Section: string; Control: TMenuItem); overload;
    procedure GetTranslation(const Section: string; Control: TCustomActionList); overload;
  end;

  ILanguageSupport = interface
    ['{D9A1A056-B556-4BF6-B2D3-EDF9F14B1F47}']
    procedure GetTranslation(Language: TLanguage); stdcall;
    procedure Translate; stdcall;
  end;

function GetSystemLanguage: string; inline;
function GetLanguage(const LCID: LangID): string;

function EscapeString(const Input: string): string;
function UnescapeString(const Input: string): string;

var
  Language: TLanguage = nil;
  Locale: string;

function _T(const Key: string): string; overload;
function _T(const Key: string; const Args: array of const): string; overload;
function _P(const Key: string): PChar; overload;
function _P(const Key: string; const Args: array of const): PChar; overload;

resourcestring
  StrStrings = 'Strings';
  StrLangDir = 'Languages\';
  StrFileNameBase = 'Language.';

implementation

function _T(const Key: string): string;
begin
  Result := UnescapeString(Language.ReadString(StrStrings, Key, Key));
end;

function _T(const Key: string; const Args: array of const): string;
begin
  Result := format(_T(Key), Args);
end;

function _P(const Key: string): PChar;
begin
  Result := PChar(_T(Key));
end;

function _P(const Key: string; const Args: array of const): PChar;
begin
  Result := PChar(_T(Key, Args));
end;

procedure InitLanguage;
var
  FileName: string;
  I: integer;
begin
  Locale := GetSystemLanguage;
  if ParamCount > 1 then
    for I := 1 to ParamCount - 1 do
      if LowerCase(ParamStr(I)) = '-lang' then begin
        Locale := ParamStr(I + 1);
        break;
      end;

  FileName := ExtractFilePath(paramstr(0)) + StrLangDir + StrFileNameBase + Locale;

  if not FileExists(FileName) then begin
    Locale := 'en-US';
    FileName := ExtractFilePath(paramstr(0)) + StrLangDir +  StrFileNameBase + Locale;
  end;

  if not FileExists(FileName) then begin
    Locale := 'hu-HU';
    FileName := '';
  end;

  Language := TLanguage.Create(FileName, TEncoding.UTF8);
end;

const
  LOCALE_NAME_MAX_LENGTH = 85;

function LCIDToLocaleName(Locale: LCID; lpName: LPWSTR; cchName: Integer;
  dwFlags: DWORD): Integer; stdcall; external kernel32;

function GetSystemLanguage: string;
begin
  Result := GetLanguage(GetSystemDefaultLCID);
end;

function GetLanguage(const LCID: LangID): string;
var
  Buffer : array [0..LOCALE_NAME_MAX_LENGTH + 1] of WideChar;
begin
  if LCIDToLocaleName(LCID, @Buffer[0],
    LOCALE_NAME_MAX_LENGTH, 0) <> 0 then
      Result := String(PChar(@Buffer[0]))
  else
      Result := '';
end;

function EscapeString(const Input: string): string;
var
  I, J: Integer;
  Found: boolean;
begin
  Result := '';
  for I := 1 to length(Input) do begin
    case Input[I] of
      '[': Result := Result + '\x5B';
      ']': Result := Result + '\x5D';
      '=': Result := Result + '\x3D';
      else begin
        Found := false;
        for J := 0 to 10 do
          if Input[I] = Codes[1, J] then begin
            Result := Result + '\' + Codes[0, J];
            Found := true;
            break;
          end;

        if not Found then
          Result := Result + Input[I];
      end;
    end;
  end;
end;

function UnescapeString(const Input: string): string;
var
  I, J: Integer;
  Temp: longword;
begin
  Result := '';
  I := 1;
  while I <= length(Input) do begin
    if (Input[I] = '\') and (I < length(Input)) then begin
      if (Input[I + 1] = 'x') and (I < length(Input) - 1) and
         TryStrToUInt('0' + Copy(Input, I + 1, 3), Temp) then begin
          Result := Result + Chr(Byte(Temp));
          inc(I, 4);
      end
      else if (Input[I + 1] = 'u') and (I < length(Input) - 3) and
         TryStrToUInt('0x' + Copy(Input, I + 2, 4), Temp) then begin
          Result := Result + Chr(Word(Temp));
          inc(I, 6);
      end
      else
        for J := 0 to 10 do
          if Input[I + 1] = Codes[0, J] then begin
            Result := Result + Codes[1, J];
            inc(I, 2);
            break;
          end;
      continue;
    end
    else
      Result := Result + Input[I];

    inc(I);
  end;
end;

{ TLanguage }

procedure TLanguage.GetTranslation(const Section: string;
  Control: TControl);
var
  Buffer: array of char;
  I: integer;

  procedure WriteItems(const Name: string; Items: TStrings);
  var
    I: integer;
  begin
    for I := 0 to Items.Count - 1 do
      WriteString(Section, Name + '.Items[' + IntToStr(I) + ']', EscapeString(Items[I]));
  end;

begin
  if Control.Hint <> '' then
    WriteString(Section, Control.Name + '.Hint', EscapeString(Control.Hint));

  if Control is TComboBox then
    WriteItems(Control.Name, (Control as TComboBox).Items)
  else if Control is TListBox then
    WriteItems(Control.Name, (Control as TListBox).Items)
  else if Control is TRadioGroup then
    WriteItems(Control.Name, (Control as TRadioGroup).Items)
  else if Control is TCheckListBox then
    WriteItems(Control.Name, (Control as TCheckListBox).Items)
  else begin
    I := Control.GetTextLen;
    if I > 0 then begin
      SetLength(Buffer, I + 1);
      Control.GetTextBuf(@Buffer[0], I + 1);
      WriteString(Section, Control.Name, EscapeString(String(PChar(@Buffer[0]))));
    end;
  end;

  if Control is TWinControl then
    with Control as TWinControl do
      for I := 0 to ControlCount - 1 do
        if not (Controls[I] is TFrame) then
          GetTranslation(Section, Controls[I]);
end;

procedure TLanguage.Translate(const Section: string; Control: TControl);
var
  I: integer;
  A: array of boolean;
  Temp: string;

  procedure ReadItems(const Name: string; Items: TStrings);
  var
    I: integer;
  begin
    for I := 0 to Items.Count - 1 do
      Items[I] := UnescapeString(ReadString(Section,
        Name + '.Items[' + IntToStr(I) + ']',
        EscapeString(Items[I])));
  end;

begin
  if Control is TComboBox then
    with (Control as TComboBox) do begin
      I := ItemIndex;
      ReadItems(Control.Name, Items);
      ItemIndex := I;
    end
  else if Control is TListBox then
    with (Control as TListBox) do begin
      I := ItemIndex;
      ReadItems(Control.Name, Items);
      ItemIndex := I;
    end
  else if Control is TRadioGroup then
    with (Control as TRadioGroup) do begin
      I := ItemIndex;
      ReadItems(Control.Name, Items);
      ItemIndex := I;
    end
  else if Control is TCheckListBox then
    with (Control as TCheckListBox) do begin
      SetLength(A, Items.Count);
      for I := 0 to Items.Count - 1 do
        A[I] := Checked[I];
      I := ItemIndex;
      ReadItems(Control.Name, Items);
      ItemIndex := I;
      for I := 0 to Items.Count - 1 do
        Checked[I] := A[I];
      SetLength(A, 0);
    end
  else begin
    Temp := UnescapeString(ReadString(Section, Control.Name, ''));
    if Temp <> '' then
      Control.SetTextBuf(PChar(Temp));
  end;

  Control.Hint := UnescapeString(ReadString(Section, Control.Name + '.Hint',
    EscapeString(Control.Hint)));

  if Control is TWinControl then
    with Control as TWinControl do
      for I := 0 to ControlCount - 1 do
        if not (Controls[I] is TFrame) then
          Translate(Section, Controls[I]);
end;

procedure TLanguage.GetTranslation(const Section: string; Control: TMenuItem);
var
  I: integer;
  HasAction: boolean;
begin
  with Control do begin
    HasAction := Assigned(Action) and (Action is TCustomAction);
    if (Caption <> '') and (Caption <> '-') and
      (not HasAction or (HasAction and (Caption <> (Action as TCustomAction).Caption))) then
        WriteString(Section, Name, EscapeString(Caption));

    if (Hint <> '') and
      (not HasAction or (HasAction and (Caption <> (Action as TCustomAction).Caption))) then
        WriteString(Section, Name + '.Hint', EscapeString(Hint));

    for I := 0 to Count - 1 do
       GetTranslation(Section, Items[I]);
  end;
end;

procedure TLanguage.GetTranslation(const Section: string;
  Control: TCustomActionList);
var
  I: integer;
begin
  with Control do
    for I := 0 to ActionCount - 1 do
      with Actions[I] do begin
        if Caption <> '' then
          WriteString(Section, Name, Caption);

        if Hint <> '' then
          WriteString(Section, Name + '.Hint', Hint);
      end;
end;

procedure TLanguage.Translate(const Section: string;
  Control: TCustomActionList);
var
  I: integer;
begin
  with Control do
    for I := 0 to ActionCount - 1 do
      with Actions[I] do begin
        if Caption <> '' then
          Caption := ReadString(Section, Name, Caption);

        if Hint <> '' then
          Hint := ReadString(Section, Name + '.Hint', Hint);
      end;
end;

procedure TLanguage.Translate(const Section: string; Control: TMenuItem);
var
  I: integer;
begin
  with Control do begin
    if (Caption <> '') and (Caption <> '-') then
      Caption := UnescapeString(ReadString(Section, Name, EscapeString(Caption)));

    if (Hint <> '') then
      Hint := UnescapeString(ReadString(Section, Name + '.Hint', EscapeString(Hint)));

    for I := 0 to Count - 1 do
       Translate(Section, Items[I]);
  end;
end;

initialization
  if not IsLibrary then
    InitLanguage;

finalization
  if Assigned(Language) then
    Language.Free;

end.
