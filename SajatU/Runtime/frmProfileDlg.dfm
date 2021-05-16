object ProfileDialog: TProfileDialog
  Left = 0
  Top = 0
  ActiveControl = edName
  BorderStyle = bsDialog
  Caption = 'Profilbe'#225'll'#237't'#225'sok'
  ClientHeight = 450
  ClientWidth = 415
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    415
    450)
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 0
    Top = 0
    Width = 415
    Height = 406
    Anchors = [akLeft, akTop, akRight, akBottom]
    Brush.Color = clWindow
    Pen.Style = psClear
    ExplicitWidth = 459
    ExplicitHeight = 206
  end
  object btnCancel: TButton
    Left = 318
    Top = 412
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&M'#233'gse'
    TabOrder = 0
    OnClick = btnCancelClick
  end
  object btnOK: TButton
    Left = 237
    Top = 412
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    TabOrder = 1
    OnClick = btnOKClick
  end
  object GroupBox1: TGroupBox
    Left = 22
    Top = 16
    Width = 371
    Height = 113
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Alapadatok'
    TabOrder = 2
    DesignSize = (
      371
      113)
    object Bevel1: TBevel
      Left = 22
      Top = 24
      Width = 64
      Height = 64
    end
    object imgIcon: TImage
      Left = 22
      Top = 24
      Width = 64
      Height = 64
      Cursor = crHandPoint
      Center = True
      PopupMenu = pmIcon
      Proportional = True
      Stretch = True
      OnClick = miBrowseIconClick
    end
    object lbInternalID: TLabel
      Left = 120
      Top = 75
      Width = 229
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'lbInternalID'
      ExplicitWidth = 264
    end
    object Label1: TLabel
      Left = 104
      Top = 26
      Width = 23
      Height = 13
      Caption = '&N'#233'v:'
    end
    object Label2: TLabel
      Left = 104
      Top = 56
      Width = 78
      Height = 13
      Caption = '&Bels'#337' azonos'#237't'#243':'
    end
    object edName: TEdit
      Left = 144
      Top = 23
      Width = 205
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 22
    Top = 135
    Width = 371
    Height = 254
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Emul'#225'tor be'#225'll'#237't'#225'sok'
    TabOrder = 3
    DesignSize = (
      371
      254)
    object Label6: TLabel
      Left = 22
      Top = 78
      Width = 48
      Height = 13
      Caption = '&El'#233'r'#233'si '#250't:'
      FocusControl = edEmulator
    end
    object Label4: TLabel
      Left = 22
      Top = 28
      Width = 33
      Height = 13
      Caption = 'Verzi'#243':'
    end
    object lbVersion: TLabel
      Left = 88
      Top = 28
      Width = 43
      Height = 13
      Caption = 'lbVersion'
    end
    object Label5: TLabel
      Left = 22
      Top = 51
      Width = 44
      Height = 13
      Caption = 'Friss'#237'tve:'
    end
    object lbDate: TLabel
      Left = 88
      Top = 51
      Width = 31
      Height = 13
      Caption = 'lbDate'
    end
    object Label8: TLabel
      Left = 22
      Top = 128
      Width = 100
      Height = 13
      Caption = 'Egy'#233'ni &param'#233'terek:'
    end
    object edEmulator: TEdit
      Left = 88
      Top = 74
      Width = 261
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edEmulatorChange
    end
    object btnBrowse: TButton
      Left = 274
      Top = 101
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Tall'#243'z'#225's...'
      TabOrder = 1
      OnClick = btnBrowseClick
    end
    object btnDefault: TButton
      Left = 156
      Top = 101
      Width = 112
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Alap'#233'rtelmezett'
      TabOrder = 2
      OnClick = btnDefaultClick
    end
    object edOptParams: TMemo
      Left = 22
      Top = 147
      Width = 327
      Height = 85
      Anchors = [akLeft, akTop, akRight, akBottom]
      ScrollBars = ssBoth
      TabOrder = 3
      WantReturns = False
    end
  end
  object OpenExeDialog: TOpenDialog
    DefaultExt = 'exe'
    Filter = 
      'Futtathat'#243' f'#225'jlok (*.exe; *.bat; *.cmd; *.com)|*.exe;*.bat;*.cmd' +
      ';*.com|Minden f'#225'jl (*.*)|*.*'
    Left = 224
    Top = 24
  end
  object pmIcon: TPopupMenu
    Left = 88
    Top = 24
    object allzs1: TMenuItem
      Caption = '&Tall'#243'z'#225's...'
      OnClick = miBrowseIconClick
    end
    object miDefIcon: TMenuItem
      Caption = 'Gy'#225'ri &ikon...'
      OnClick = miDefIconClick
    end
  end
  object OpenPicDlg: TOpenPictureDialog
    Filter = 
      'T'#225'mogatott form'#225'tumok (*.bmp; *.gif; *.jpeg; *.jpg; *.exif; *.pn' +
      'g; *.tif; *.tiff)|*.bmp; *.gif; *.jpeg; *.jpg; *.exif; *.png; *.' +
      'tif; *.tiff|Bitk'#233'pek (*.bmp)|*.bmp|GIF-k'#233'pek (*.gif)|*.gif|JPEG-' +
      'k'#233'pek (*.jpg; *.jpeg)|*.jpg;*.jpeg|EXIF-k'#233'pek (*.exif)|*.exif|PN' +
      'G-k'#233'pek (*.png)|*.png|TIFF-k'#233'pek (*.tif; *.tiff)|*.tif; *.tiff|M' +
      'inden f'#225'jl (*.*)|*.*'
    Left = 24
    Top = 24
  end
end
