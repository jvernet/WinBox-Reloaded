object AboutFrm: TAboutFrm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'N'#233'vjegy'
  ClientHeight = 282
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
    282)
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 0
    Top = 0
    Width = 403
    Height = 230
    Anchors = [akLeft, akTop, akRight, akBottom]
    Brush.Color = clWindow
    Pen.Style = psClear
    ExplicitWidth = 465
    ExplicitHeight = 225
  end
  object Label1: TLabel
    Left = 104
    Top = 17
    Width = 147
    Height = 19
    Caption = 'WinBox Reloaded R2'
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
  object Label3: TLabel
    Left = 208
    Top = 49
    Width = 60
    Height = 13
    Caption = '2021.05.14.'
  end
  object Label4: TLabel
    Left = 120
    Top = 68
    Width = 57
    Height = 13
    Caption = 'Verzi'#243'sz'#225'm:'
  end
  object Label5: TLabel
    Left = 208
    Top = 68
    Width = 104
    Height = 13
    Caption = 'v1.0 Release Preview'
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
    Top = 161
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
    Top = 193
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
  object Button1: TButton
    Left = 306
    Top = 242
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 0
    ExplicitLeft = 368
    ExplicitTop = 250
  end
end
