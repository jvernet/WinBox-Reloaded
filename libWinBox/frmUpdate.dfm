object UpdateForm: TUpdateForm
  Left = 0
  Top = 0
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsDialog
  Caption = 'Emul'#225'torok friss'#237't'#233'se'
  ClientHeight = 361
  ClientWidth = 440
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    440
    361)
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 0
    Top = 0
    Width = 440
    Height = 278
    Anchors = [akLeft, akTop, akRight, akBottom]
    Brush.Color = clWindow
    Pen.Style = psClear
    ExplicitWidth = 430
    ExplicitHeight = 268
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 31
    Height = 13
    Caption = '&Napl'#243':'
    FocusControl = LogBox
  end
  object lbState: TLabel
    Left = 8
    Top = 284
    Width = 48
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '&Folyamat:'
    FocusControl = Progress
  end
  object State: TLabel
    Left = 72
    Top = 284
    Width = 45
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = #220'resj'#225'rat'
  end
  object LogBox: TListBox
    Left = 8
    Top = 27
    Width = 424
    Height = 240
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
    TabWidth = 15
    OnDblClick = LogBoxDblClick
  end
  object Button1: TButton
    Left = 163
    Top = 328
    Width = 115
    Height = 25
    Anchors = [akBottom]
    Caption = '&Megszak'#237't'#225's'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Progress: TProgressBar
    Left = 8
    Top = 305
    Width = 424
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Max = 4
    TabOrder = 2
  end
end
