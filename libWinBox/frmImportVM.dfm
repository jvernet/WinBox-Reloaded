object ImportVM: TImportVM
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Megl'#233'v'#337' 86Box virtu'#225'lis g'#233'p import'#225'l'#225'sa'
  ClientHeight = 309
  ClientWidth = 479
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
    479
    309)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 263
    Width = 479
    Height = 46
    Align = alBottom
    Shape = bsSpacer
    ExplicitTop = 280
    ExplicitWidth = 496
  end
  object Image1: TImage
    AlignWithMargins = True
    Left = 1
    Top = 2
    Width = 140
    Height = 260
    Margins.Left = 1
    Margins.Top = 2
    Margins.Right = 1
    Margins.Bottom = 1
    Align = alLeft
    Center = True
    Proportional = True
    Stretch = True
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitHeight = 263
  end
  object Button1: TButton
    Tag = 1
    Left = 383
    Top = 273
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Tov'#225'bb >'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Tag = -1
    Left = 302
    Top = 273
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '< &Vissza'
    Enabled = False
    TabOrder = 1
    OnClick = Button1Click
  end
  object PageControl1: TPageControl
    Left = 142
    Top = 0
    Width = 337
    Height = 263
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      TabVisible = False
      DesignSize = (
        329
        253)
      object Label1: TLabel
        Left = 16
        Top = 16
        Width = 256
        Height = 13
        Caption = 'Virtu'#225'lis g'#233'p import'#225'l'#225'sa - '#252'dv'#246'zli a var'#225'zsl'#243'!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label2: TLabel
        Left = 16
        Top = 48
        Width = 297
        Height = 202
        Anchors = [akLeft, akTop, akRight, akBottom]
        AutoSize = False
        Caption = 
          'Ez a var'#225'zsl'#243' v'#233'gigvezeti '#214'nt egy megl'#233'v'#337' 86Box virtu'#225'lis g'#233'p im' +
          'port'#225'l'#225's'#225'nak folyamat'#225'n.'#13#10#13#10'A folyamat sor'#225'n egy, m'#225'r kor'#225'bban k' +
          'ialak'#237'tott 86Box virtu'#225'lis g'#233'pet helyezhet el a WinBox list'#225'j'#225'n.' +
          #13#10#13#10'A l'#233'p'#233'sek megkezd'#233's'#233'hez kattintson a Tov'#225'bb gombra.'
        WordWrap = True
        ExplicitHeight = 184
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'TabSheet7'
      ImageIndex = 6
      TabVisible = False
      DesignSize = (
        329
        253)
      object Label15: TLabel
        Left = 16
        Top = 16
        Width = 127
        Height = 13
        Caption = 'Alapadatok megad'#225'sa'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label16: TLabel
        Left = 34
        Top = 85
        Width = 23
        Height = 13
        Caption = '&N'#233'v:'
      end
      object Label18: TLabel
        Left = 18
        Top = 45
        Width = 287
        Height = 34
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          #205'rja be a virtu'#225'lis g'#233'p nev'#233't, amivel k'#233's'#337'bb azonos'#237'thatja a Win' +
          'Box list'#225'j'#225'ban.'
        WordWrap = True
      end
      object Label20: TLabel
        Left = 18
        Top = 112
        Width = 287
        Height = 54
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'A konfigur'#225'ci'#243't a 86Box.cfg f'#225'jl tartalmazza, ami az import'#225'land' +
          #243' g'#233'p munkak'#246'nyvt'#225'r'#225'ban tal'#225'lhat'#243'.'
        WordWrap = True
      end
      object Label21: TLabel
        Left = 18
        Top = 152
        Width = 86
        Height = 13
        Caption = 'Konfigur'#225'ci'#243's &f'#225'jl:'
      end
      object Label3: TLabel
        Left = 16
        Top = 208
        Width = 297
        Height = 42
        AutoSize = False
        Caption = 
          'A virtu'#225'lis g'#233'p f'#225'jlai az eredeti munkak'#246'nyvt'#225'rban lesznek t'#225'rol' +
          'va, elmozgat'#225'sra nem ker'#252'lnek az import'#225'l'#225'skor.'
        WordWrap = True
      end
      object Edit1: TEdit
        Left = 71
        Top = 81
        Width = 210
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object Path: TEdit
        Left = 18
        Top = 171
        Width = 240
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'Path'
      end
      object Button3: TButton
        Left = 264
        Top = 168
        Width = 35
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 2
        OnClick = Button3Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 4
      TabVisible = False
      object Label4: TLabel
        Left = 16
        Top = 16
        Width = 125
        Height = 13
        Caption = 'Emul'#225'tor kiv'#225'laszt'#225'sa'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label5: TLabel
        Left = 16
        Top = 48
        Width = 289
        Height = 42
        AutoSize = False
        Caption = 
          'A virtu'#225'lis g'#233'pek kezel'#233'se sor'#225'n a WinBox k'#233'pes kezelni k'#252'l'#246'nf'#233'l' +
          'e verzi'#243'j'#250' emul'#225'torokat is.'
        WordWrap = True
      end
      object Label6: TLabel
        Left = 34
        Top = 174
        Width = 281
        Height = 41
        AutoSize = False
        Caption = 
          'Ha a virtu'#225'lis g'#233'p egy specifikus 86Box verzi'#243't ig'#233'nyel, akkor i' +
          'tt defini'#225'lhatja azt. '
        WordWrap = True
      end
      object Label7: TLabel
        Left = 34
        Top = 215
        Width = 48
        Height = 13
        Caption = 'El'#233'r'#233'si &'#250't:'
        FocusControl = Edit2
      end
      object Label8: TLabel
        Left = 34
        Top = 111
        Width = 279
        Height = 42
        AutoSize = False
        Caption = 
          'Folyamatosan frissen tartott verzi'#243', a WinBox menedzsel'#233'se alatt' +
          '.'
        WordWrap = True
      end
      object RadioButton1: TRadioButton
        Left = 16
        Top = 88
        Width = 269
        Height = 17
        Caption = '&Automatikus 86Box verzi'#243' haszn'#225'lata'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = RadioButton1Click
      end
      object RadioButton2: TRadioButton
        Left = 16
        Top = 151
        Width = 229
        Height = 17
        Caption = '&Egyedi 86Box verzi'#243' haszn'#225'lata'
        TabOrder = 1
        OnClick = RadioButton1Click
      end
      object Edit2: TEdit
        Left = 88
        Top = 213
        Width = 170
        Height = 19
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 2
        Text = 'Edit2'
      end
      object Button4: TButton
        Left = 264
        Top = 211
        Width = 35
        Height = 25
        Caption = '...'
        Enabled = False
        TabOrder = 3
        OnClick = Button4Click
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'TabSheet5'
      ImageIndex = 4
      TabVisible = False
      DesignSize = (
        329
        253)
      object Label25: TLabel
        Left = 16
        Top = 16
        Width = 103
        Height = 13
        Caption = 'Minden k'#233'szen '#225'll!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label26: TLabel
        Left = 16
        Top = 48
        Width = 297
        Height = 153
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'A var'#225'zsl'#243' most m'#225'r minden sz'#252'ks'#233'ges adatot ismer a virtu'#225'lis g'#233 +
          'p import'#225'l'#225's'#225'hoz.'#13#10#13#10'Ha az import'#225'l'#225'sa ut'#225'n szeretne finombe'#225'll'#237 +
          't'#225'sokat elv'#233'gezni, haszn'#225'lja az al'#225'bbi jel'#246'l'#337'n'#233'gyzetet.'#13#10#13#10#13#10#13#10#13 +
          #10'A befejez'#233'shez kattintson a Tov'#225'bb gombra.'
        WordWrap = True
      end
      object CheckBox1: TCheckBox
        Left = 16
        Top = 128
        Width = 289
        Height = 17
        Caption = '&Tov'#225'bbi be'#225'll'#237't'#225'sok megjelen'#237't'#233'se a l'#233'trehoz'#225's ut'#225'n'
        TabOrder = 0
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'TabSheet6'
      DoubleBuffered = False
      ImageIndex = 5
      ParentDoubleBuffered = False
      TabVisible = False
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'cfg'
    FileName = '86box.cfg'
    Filter = 
      '86Box konfigur'#225'ci'#243's f'#225'jlok (86box.cfg)|86box.cfg|Minden f'#225'jl (*.' +
      '*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 40
    Top = 256
  end
  object OpenDialog2: TOpenDialog
    DefaultExt = 'exe'
    Filter = 
      'Futtathat'#243' f'#225'jlok (*.exe; *.bat; *.cmd; *.com)|*.exe;*.bat;*.cmd' +
      ';*.com|Minden f'#225'jl (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 120
    Top = 256
  end
end
