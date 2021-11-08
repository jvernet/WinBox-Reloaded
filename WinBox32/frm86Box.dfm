object Frame86Box: TFrame86Box
  Left = 0
  Top = 0
  Width = 526
  Height = 357
  DoubleBuffered = True
  ParentBackground = False
  ParentDoubleBuffered = False
  TabOrder = 0
  DesignSize = (
    526
    357)
  object Splitter1: TSplitter
    AlignWithMargins = True
    Left = 334
    Top = 46
    Height = 266
    Margins.Left = 0
    Margins.Top = 5
    Margins.Right = 0
    Margins.Bottom = 5
    Align = alRight
    Beveled = True
    ExplicitLeft = 338
    ExplicitTop = 47
    ExplicitHeight = 276
  end
  object BotBevel: TBevel
    AlignWithMargins = True
    Left = 3
    Top = 320
    Width = 520
    Height = 34
    Align = alBottom
    Shape = bsTopLine
    ExplicitLeft = 0
    ExplicitTop = 307
    ExplicitWidth = 526
  end
  object Label1: TLabel
    Left = 18
    Top = 330
    Width = 37
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '&'#193'llapot:'
  end
  object lbState: TLabel
    Left = 69
    Top = 330
    Width = 41
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'lbState'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object CatPanels: TCategoryPanelGroup
    AlignWithMargins = True
    Left = 3
    Top = 44
    Width = 326
    Height = 270
    Margins.Right = 5
    VertScrollBar.Tracking = True
    Align = alClient
    BevelEdges = []
    BevelInner = bvNone
    BevelOuter = bvNone
    ChevronColor = clWindowText
    GradientBaseColor = clWindow
    GradientColor = clWindow
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -11
    HeaderFont.Name = 'Tahoma'
    HeaderFont.Style = []
    ParentColor = True
    TabOrder = 0
    object CatPorts: TCategoryPanel
      Top = 563
      Height = 78
      Caption = 'Portok'
      ParentColor = True
      TabOrder = 0
      object lbLPT: TLabel
        Left = 111
        Top = 27
        Width = 23
        Height = 13
        Caption = 'LPT1'
      end
      object Label5: TLabel
        Left = 16
        Top = 8
        Width = 65
        Height = 13
        Caption = 'Soros portok:'
      end
      object lbCOM: TLabel
        Left = 111
        Top = 8
        Width = 65
        Height = 13
        Caption = 'COM1, COM2'
      end
      object Label7: TLabel
        Left = 16
        Top = 27
        Width = 85
        Height = 13
        Caption = 'P'#225'rhuzamos port:'
      end
    end
    object CatInput: TCategoryPanel
      Top = 485
      Height = 78
      Caption = 'Beviteli eszk'#246'z'#246'k'
      ParentColor = True
      TabOrder = 1
      object lbJoystick: TLabel
        Left = 99
        Top = 27
        Width = 71
        Height = 13
        Caption = '3Dfx Voodoo 2'
      end
      object Label27: TLabel
        Left = 16
        Top = 8
        Width = 26
        Height = 13
        Caption = 'Eg'#233'r:'
      end
      object lbMouse: TLabel
        Left = 99
        Top = 8
        Width = 65
        Height = 13
        Caption = 'S3 Trio64 PCI'
      end
      object Label29: TLabel
        Left = 16
        Top = 27
        Width = 65
        Height = 13
        Caption = 'J'#225't'#233'kvez'#233'rl'#337':'
      end
    end
    object CatNetwork: TCategoryPanel
      Top = 407
      Height = 78
      Caption = 'H'#225'l'#243'zat'
      ParentColor = True
      TabOrder = 2
      object lbNetType: TLabel
        Left = 99
        Top = 27
        Width = 26
        Height = 13
        Caption = 'SLiRP'
      end
      object Label31: TLabel
        Left = 16
        Top = 8
        Width = 77
        Height = 13
        Caption = 'H'#225'l'#243'zati eszk'#246'z:'
      end
      object lbNetCard: TLabel
        Left = 99
        Top = 8
        Width = 47
        Height = 13
        Caption = 'ne2k_pnp'
      end
      object Label33: TLabel
        Left = 16
        Top = 27
        Width = 70
        Height = 13
        Caption = 'Csatol'#225'si m'#243'd:'
      end
    end
    object CatStorage: TCategoryPanel
      Top = 271
      Height = 136
      Caption = 'T'#225'rol'#243
      ParentColor = True
      TabOrder = 3
      object lbSCSI: TLabel
        Left = 93
        Top = 84
        Width = 92
        Height = 13
        Caption = 'Adaptec AHA-154x'
      end
      object lbCD: TLabel
        Left = 93
        Top = 46
        Width = 18
        Height = 13
        Caption = '52x'
      end
      object lbHDD: TLabel
        Left = 93
        Top = 27
        Width = 54
        Height = 13
        Caption = '20 MB MFM'
      end
      object lbFloppy: TLabel
        Left = 93
        Top = 8
        Width = 131
        Height = 13
        Caption = '3.5" 1.44 MB, 5.25" 1.2 MB'
      end
      object Label23: TLabel
        Left = 17
        Top = 8
        Width = 36
        Height = 13
        Caption = 'Floppy:'
      end
      object Label25: TLabel
        Left = 17
        Top = 27
        Width = 61
        Height = 13
        Caption = 'Merevlemez:'
      end
      object Label14: TLabel
        Left = 16
        Top = 46
        Width = 67
        Height = 13
        Caption = 'CD-meghajt'#243':'
      end
      object Label15: TLabel
        Left = 16
        Top = 84
        Width = 66
        Height = 13
        Caption = 'SCSI-vez'#233'rl'#337':'
      end
      object Label8: TLabel
        Left = 16
        Top = 65
        Width = 65
        Height = 13
        Caption = 'K'#252'ls'#337' t'#225'rol'#243'k:'
      end
      object lbExStor: TLabel
        Left = 93
        Top = 65
        Width = 25
        Height = 13
        Caption = 'Nincs'
      end
    end
    object CatSound: TCategoryPanel
      Top = 193
      Height = 78
      Caption = 'Audi'#243
      ParentColor = True
      TabOrder = 4
      object lbMidi: TLabel
        Left = 93
        Top = 27
        Width = 25
        Height = 13
        Caption = 'Nincs'
      end
      object Label19: TLabel
        Left = 16
        Top = 8
        Width = 61
        Height = 13
        Caption = 'Hangeszk'#246'z:'
      end
      object lbAudio: TLabel
        Left = 93
        Top = 8
        Width = 25
        Height = 13
        Caption = 'Nincs'
      end
      object Label21: TLabel
        Left = 16
        Top = 27
        Width = 62
        Height = 13
        Caption = 'MIDI eszk'#246'z:'
      end
    end
    object CatVideo: TCategoryPanel
      Top = 115
      Height = 78
      Caption = 'K'#233'perny'#337
      ParentColor = True
      TabOrder = 5
      object lb3Dfx: TLabel
        Left = 93
        Top = 27
        Width = 25
        Height = 13
        Caption = 'Nincs'
      end
      object Label9: TLabel
        Left = 16
        Top = 8
        Width = 61
        Height = 13
        Caption = 'Vide'#243'k'#225'rtya:'
      end
      object lbVGA: TLabel
        Left = 93
        Top = 8
        Width = 111
        Height = 13
        Caption = 'Color Graphics Adapter'
      end
      object Label12: TLabel
        Left = 16
        Top = 27
        Width = 64
        Height = 13
        Caption = '3D gyors'#237't'#225's:'
      end
    end
    object CatSystem: TCategoryPanel
      Top = 0
      Height = 115
      Caption = 'Rendszer'
      ParentColor = True
      TabOrder = 6
      object lbEmulator: TLabel
        Tag = 1
        Left = 80
        Top = 65
        Width = 30
        Height = 13
        Caption = '86Box'
      end
      object Label3: TLabel
        Left = 16
        Top = 8
        Width = 29
        Height = 13
        Caption = 'T'#237'pus:'
      end
      object lbPC: TLabel
        Left = 80
        Top = 8
        Width = 34
        Height = 13
        Caption = 'IBM PC'
      end
      object Label13: TLabel
        Left = 16
        Top = 27
        Width = 44
        Height = 13
        Caption = 'Mem'#243'ria:'
      end
      object lbMemSize: TLabel
        Left = 80
        Top = 27
        Width = 32
        Height = 13
        Caption = '256 kB'
      end
      object lbCPU: TLabel
        Left = 80
        Top = 46
        Width = 50
        Height = 13
        Caption = '8088/4.77'
      end
      object Label16: TLabel
        Left = 16
        Top = 46
        Width = 56
        Height = 13
        Caption = 'Processzor:'
      end
      object Label17: TLabel
        Left = 16
        Top = 65
        Width = 46
        Height = 13
        Caption = 'Emul'#225'tor:'
      end
    end
  end
  object btnPrinter: TButton
    Tag = 2
    Left = 401
    Top = 324
    Width = 115
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Nyomtat'#243't'#225'lca...'
    TabOrder = 1
    OnClick = btnVMClick
  end
  object RightPanel: TPanel
    AlignWithMargins = True
    Left = 340
    Top = 44
    Width = 176
    Height = 265
    Margins.Right = 10
    Margins.Bottom = 8
    Align = alRight
    BevelEdges = [beLeft]
    BevelOuter = bvNone
    Constraints.MinHeight = 170
    Constraints.MinWidth = 170
    ParentBackground = False
    ParentColor = True
    TabOrder = 2
    DesignSize = (
      176
      265)
    object Label2: TLabel
      Left = 11
      Top = 37
      Width = 160
      Height = 113
      Alignment = taCenter
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Caption = '(nincs)'
      Layout = tlCenter
      ExplicitWidth = 168
    end
    object lbHostCPU: TLabel
      Left = 154
      Top = 156
      Width = 17
      Height = 13
      Alignment = taRightJustify
      Anchors = [akRight, akBottom]
      Caption = '0%'
      ExplicitLeft = 161
      ExplicitTop = 154
    end
    object Label4: TLabel
      Left = 11
      Top = 156
      Width = 24
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = '&CPU:'
      ExplicitTop = 154
    end
    object Label6: TLabel
      Left = 11
      Top = 198
      Width = 44
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = '&Mem'#243'ria:'
      ExplicitTop = 196
    end
    object lbHostRAM: TLabel
      Left = 128
      Top = 198
      Width = 43
      Height = 13
      Alignment = taRightJustify
      Anchors = [akRight, akBottom]
      Caption = '0% (0 B)'
      ExplicitLeft = 135
      ExplicitTop = 196
    end
    object lbScreenshots: TLabel
      Tag = 3
      Left = 11
      Top = 12
      Width = 86
      Height = 13
      Cursor = crHandPoint
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = '&K'#233'perny'#337'k'#233'pek:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHotLight
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = btnVMClick
      ExplicitWidth = 94
    end
    object Label20: TLabel
      Left = 11
      Top = 244
      Width = 104
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Elfoglalt lemezter'#252'let:'
      ExplicitTop = 242
    end
    object lbDiskSize: TLabel
      Left = 148
      Top = 244
      Width = 23
      Height = 13
      Alignment = taRightJustify
      Anchors = [akRight, akBottom]
      Caption = '0 MB'
      ExplicitLeft = 155
      ExplicitTop = 242
    end
    object Bevel1: TBevel
      Left = 11
      Top = 37
      Width = 160
      Height = 113
      Anchors = [akLeft, akTop, akRight, akBottom]
      ExplicitWidth = 168
    end
    object Screenshots: TPicturePager
      AlignWithMargins = True
      Left = 13
      Top = 39
      Width = 156
      Height = 111
      Cursor = crHandPoint
      Anchors = [akLeft, akTop, akRight, akBottom]
      ButtonNext = btnImgNext
      ButtonPrev = btnImgPrev
      ItemIndex = -1
      Center = True
      Filter = '*.png'
      Stretch = smFixAspect
      OnUpdate = ScreenshotsUpdate
      OnMouseDown = ScreenshotsMouseDown
      ExplicitWidth = 150
    end
    object pbHostCPU: TProgressBar
      Left = 11
      Top = 175
      Width = 160
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 0
    end
    object pbHostRAM: TProgressBar
      Left = 11
      Top = 217
      Width = 160
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 1
    end
    object btnImgNext: TButton
      Left = 137
      Top = 6
      Width = 33
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '>'
      Enabled = False
      TabOrder = 2
    end
    object btnImgPrev: TButton
      Left = 103
      Top = 6
      Width = 33
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '<'
      Enabled = False
      TabOrder = 3
    end
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 526
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Color = 15854306
    ParentBackground = False
    TabOrder = 3
    object lbTitle: TLabel
      Left = 18
      Top = 12
      Width = 90
      Height = 16
      Caption = 'Ismeretlen PC'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnWorkDir: TSpeedButton
      Tag = 1
      Left = 0
      Top = 0
      Width = 526
      Height = 41
      Align = alClient
      Flat = True
      OnClick = btnVMClick
      ExplicitLeft = -1
      ExplicitWidth = 436
    end
  end
end
