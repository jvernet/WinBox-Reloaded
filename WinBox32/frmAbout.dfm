object AboutFrm: TAboutFrm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'N'#233'vjegy'
  ClientHeight = 288
  ClientWidth = 403
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  DesignSize = (
    403
    288)
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 0
    Top = 0
    Width = 403
    Height = 235
    Anchors = [akLeft, akTop, akRight, akBottom]
    Brush.Color = clWindow
    Pen.Style = psClear
  end
  object Label1: TLabel
    Left = 104
    Top = 17
    Width = 123
    Height = 19
    Caption = 'WinBox Reloaded'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 120
    Top = 49
    Width = 73
    Height = 13
    Caption = 'Kiad'#225'si d'#225'tum: '
  end
  object lbDate: TLabel
    Left = 208
    Top = 49
    Width = 60
    Height = 13
    Caption = '2021.05.27.'
  end
  object Label4: TLabel
    Left = 120
    Top = 68
    Width = 57
    Height = 13
    Caption = 'Verzi'#243'sz'#225'm:'
  end
  object lbVersion: TLabel
    Left = 208
    Top = 68
    Width = 22
    Height = 13
    Caption = 'v1.1'
  end
  object Label6: TLabel
    Left = 104
    Top = 97
    Width = 172
    Height = 16
    Caption = 'K'#233'sz'#237'tette: Laci b'#225#39' 2020-2021'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label7: TLabel
    Left = 120
    Top = 129
    Width = 48
    Height = 13
    Caption = 'Weboldal:'
  end
  object Label8: TLabel
    Left = 208
    Top = 129
    Width = 79
    Height = 13
    Cursor = crHandPoint
    Hint = 'http://users.atw.hu/laciba'
    Caption = 'Laci b'#225#39' honlapja'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHotLight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = Label8Click
  end
  object Label9: TLabel
    Left = 104
    Top = 180
    Width = 126
    Height = 16
    Caption = 'Kapcsol'#243'd'#243' projektek:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label10: TLabel
    Left = 120
    Top = 202
    Width = 226
    Height = 13
    Caption = 'Egyel'#337're "csak" az 86Box, folytat'#225's a j'#246'v'#337'ben.'
  end
  object Image1: TImage
    Left = 16
    Top = 17
    Width = 73
    Height = 73
  end
  object Label11: TLabel
    Left = 120
    Top = 148
    Width = 43
    Height = 13
    Caption = 'Ford'#237't'#225's:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lbTranslatedBy: TLabel
    Left = 208
    Top = 148
    Width = 122
    Height = 13
    Caption = 'Ez a program alapnyelve.'
  end
  object Button1: TButton
    Left = 306
    Top = 248
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 0
  end
end
