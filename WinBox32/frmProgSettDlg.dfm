object ProgSettDlg: TProgSettDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Programbe'#225'll'#237't'#225'sok'
  ClientHeight = 494
  ClientWidth = 451
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
    451
    494)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 435
    Height = 431
    ActivePage = TabSheet3
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet2: TTabSheet
      Caption = 'Alapvet'#337
      ImageIndex = 1
      DesignSize = (
        427
        403)
      object GroupBox1: TGroupBox
        Left = 13
        Top = 8
        Width = 402
        Height = 186
        Anchors = [akLeft, akTop, akRight]
        Caption = #218'j virtu'#225'lis g'#233'pek helye'
        TabOrder = 0
        DesignSize = (
          402
          186)
        object Label1: TLabel
          Left = 60
          Top = 145
          Width = 331
          Height = 38
          Anchors = [akLeft, akRight, akBottom]
          AutoSize = False
          Caption = 
            'Ez a be'#225'll'#237't'#225's csak az '#250'jonnan l'#233'trehozott virtu'#225'lis g'#233'peket '#233'ri' +
            'nti, a m'#225'r megl'#233'v'#337'ek hely'#233'n nem v'#225'ltoztat.'
          WordWrap = True
        end
        object Image3: TImage
          Left = 18
          Top = 142
          Width = 32
          Height = 32
          Anchors = [akLeft, akBottom]
          Stretch = True
          ExplicitTop = 77
        end
        object Label3: TLabel
          Left = 18
          Top = 68
          Width = 48
          Height = 13
          Caption = '&El'#233'r'#233'si '#250't:'
          FocusControl = Path
        end
        object Label4: TLabel
          Left = 60
          Top = 25
          Width = 325
          Height = 30
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 
            'Itt megadhatja hogy az '#250'j virtu'#225'lis g'#233'pek a sz'#225'm'#237't'#243'g'#233'pen melyik ' +
            'meghajt'#243'n, '#233's milyen k'#246'nyvt'#225'rban ker'#252'ljenek kialak'#237't'#225'sra.'
          WordWrap = True
        end
        object Image1: TImage
          Left = 18
          Top = 24
          Width = 32
          Height = 32
        end
        object Path: TEdit
          Left = 18
          Top = 87
          Width = 330
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = 'Path'
        end
        object Button3: TButton
          Left = 354
          Top = 83
          Width = 35
          Height = 25
          Anchors = [akTop, akRight]
          Caption = '...'
          TabOrder = 1
          OnClick = Button3Click
        end
        object Button4: TButton
          Left = 232
          Top = 114
          Width = 149
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'K'#246'nyvt'#225'r &megnyit'#225'sa...'
          TabOrder = 2
          OnClick = Button4Click
        end
        object Button11: TButton
          Left = 110
          Top = 114
          Width = 116
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = '&Alap'#233'rtelmezett'
          TabOrder = 3
          OnClick = Button11Click
        end
      end
      object GroupBox2: TGroupBox
        Left = 13
        Top = 200
        Width = 402
        Height = 193
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Automatikus friss'#237't'#233's'
        TabOrder = 1
        DesignSize = (
          402
          193)
        object Label2: TLabel
          Left = 18
          Top = 24
          Width = 175
          Height = 13
          Caption = '&86Box beszerz'#233'se ebb'#337'l a forr'#225'sb'#243'l:'
          FocusControl = ComboBox1
        end
        object CheckBox1: TCheckBox
          Left = 18
          Top = 131
          Width = 303
          Height = 17
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'Emul'#225'tor friss'#237't'#233'sek keres'#233'se &automatikusan'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object CheckBox2: TCheckBox
          Left = 18
          Top = 154
          Width = 265
          Height = 17
          Anchors = [akLeft, akRight, akBottom]
          Caption = '&Forr'#225'sk'#243'd let'#246'lt'#233'se, ha lehets'#233'ges'
          TabOrder = 1
        end
        object ComboBox1: TComboBox
          Left = 18
          Top = 43
          Width = 367
          Height = 79
          Style = csSimple
          Anchors = [akLeft, akTop, akRight, akBottom]
          ItemIndex = 0
          TabOrder = 2
          Text = 'https://ci.86box.net/job/86Box'
          Items.Strings = (
            'https://ci.86box.net/job/86Box'
            'https://ci.86box.net/job/86Box-Dev'
            'https://ci.86box.net/job/86Box-Debug')
        end
        object Button5: TButton
          Left = 280
          Top = 150
          Width = 99
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'Megtekint'#233's...'
          TabOrder = 3
          OnClick = Button5Click
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = '86Box VM-ek'
      ImageIndex = 2
      object GroupBox5: TGroupBox
        Left = 13
        Top = 8
        Width = 402
        Height = 386
        Caption = #218'j g'#233'pek megjelen'#233'se'
        TabOrder = 0
        object Image4: TImage
          Left = 20
          Top = 24
          Width = 32
          Height = 32
        end
        object Label8: TLabel
          Left = 64
          Top = 24
          Width = 321
          Height = 41
          AutoSize = False
          WordWrap = True
        end
        object Label9: TLabel
          Left = 64
          Top = 24
          Width = 321
          Height = 41
          AutoSize = False
          Caption = 
            'Itt megadhatja hogy '#250'j 86Box g'#233'pek milyen megjelen'#233'ssel legyenek' +
            ' l'#233'trehozva. Ez k'#233's'#337'bb szem'#233'lyre szabhat'#243' g'#233'penk'#233'nt.'
          WordWrap = True
        end
        object Label10: TLabel
          Left = 32
          Top = 94
          Width = 353
          Height = 35
          AutoSize = False
          Caption = 
            'Fix m'#233'retez'#233's'#369' ablak (pl. 960x720), '#233's 4:3 k'#233'par'#225'ny teljes k'#233'per' +
            'ny'#337'n. '
          WordWrap = True
        end
        object Label11: TLabel
          Left = 32
          Top = 237
          Width = 141
          Height = 13
          Caption = '&Teljes k'#233'perny'#337's m'#233'retez'#233's: '
          FocusControl = ComboBox2
        end
        object Label12: TLabel
          Left = 32
          Top = 211
          Width = 108
          Height = 13
          Caption = '&Ablak m'#233'retez'#233'si m'#243'd:'
          FocusControl = ComboBox1
        end
        object RadioButton1: TRadioButton
          Left = 18
          Top = 71
          Width = 367
          Height = 17
          Caption = '&Alap'#233'rtelmezett megjelent'#237't'#233'si be'#225'll'#237't'#225'sok'
          TabOrder = 0
          OnClick = UpdateAppearancePage
        end
        object RadioButton2: TRadioButton
          Left = 18
          Top = 120
          Width = 183
          Height = 17
          Caption = '&Egy'#233'ni m'#233'retez'#233'si be'#225'll'#237't'#225'sok'
          TabOrder = 1
          OnClick = UpdateAppearancePage
        end
        object CheckListBox1: TCheckListBox
          Left = 32
          Top = 143
          Width = 337
          Height = 58
          OnClickCheck = CustomAppearanceChange
          ItemHeight = 13
          Items.Strings = (
            'R'#246'gz'#237'tett 4:3 k'#233'par'#225'ny megtart'#225'sa'
            'T'#250'lp'#225'szt'#225'z'#225's enged'#233'lyez'#233'se (mint EGA/VGA)'
            'M'#233'retez'#233's a rendszer DPI '#233'rt'#233'k'#233're'
            'Line'#225'ris sk'#225'l'#225'z'#225's alkalmaz'#225'sa (ha lehets'#233'ges)'
            'Kontraszt megn'#246'vel'#233'se (monokr'#243'm kijelz'#337'n'#233'l)'
            #193'llapotsori ikonok friss'#237't'#233'se'
            'Kil'#233'p'#233'si meger'#337's'#237't'#233's k'#233'r'#233'se'
            'Discord t'#225'mogat'#225's enged'#233'lyez'#233'se')
          TabOrder = 2
        end
        object ComboBox2: TComboBox
          Left = 179
          Top = 234
          Width = 190
          Height = 21
          Style = csDropDownList
          ItemIndex = 1
          TabOrder = 4
          Text = 'R'#246'gz'#237'tett 4:3 k'#233'par'#225'ny'
          OnChange = CustomAppearanceChange
          Items.Strings = (
            'Ny'#250'jt'#225's'
            'R'#246'gz'#237'tett 4:3 k'#233'par'#225'ny'
            'Eredeti k'#233'par'#225'ny megtart'#225'sa'
            'Csak eg'#233'sz sz'#225'm'#250' nagy'#237't'#225's')
        end
        object ComboBox3: TComboBox
          Left = 179
          Top = 207
          Width = 190
          Height = 21
          Style = csDropDownList
          ItemIndex = 2
          TabOrder = 3
          Text = 'R'#246'gz'#237'tett felbont'#225's a sablonb'#243'l'
          OnChange = CustomAppearanceChange
          Items.Strings = (
            'Pillanatnyi felbont'#225's szerint'
            'Szabadon '#225'tm'#233'retezhet'#337
            'R'#246'gz'#237'tett felbont'#225's a sablonb'#243'l'
            'Eredeti k'#233'p 0,5x kicsiny'#237't'#233'se'
            'Eredeti k'#233'p 1,5x nagy'#237't'#225'sa'
            'Eredeti k'#233'p 2x nagy'#237't'#225'sa')
        end
        object RadioButton3: TRadioButton
          Left = 18
          Top = 264
          Width = 215
          Height = 17
          Caption = '&K'#233'zi megad'#225's'#250' be'#225'll'#237't'#225'sok'
          TabOrder = 5
          OnClick = UpdateAppearancePage
        end
        object Memo2: TMemo
          Left = 32
          Top = 287
          Width = 217
          Height = 58
          Lines.Strings = (
            'window_remember 0'
            'force_43 0'
            'enable_overscan 0'
            'video_filter_method 1'
            'vid_cga_contrast 0'
            'dpi_scale 1'
            'vid_cga_contrast'
            'update_icons 1'
            'confirm_exit 1'
            'enable_discord 0')
          ScrollBars = ssVertical
          TabOrder = 6
        end
        object RadioButton4: TRadioButton
          Left = 18
          Top = 351
          Width = 327
          Height = 17
          Caption = '&Ne ker'#252'ljenek l'#233'trehoz'#225'sra megjelen'#237't'#233'si be'#225'll'#237't'#225'sok'
          TabOrder = 8
          OnClick = UpdateAppearancePage
        end
        object Button12: TButton
          Left = 255
          Top = 285
          Width = 114
          Height = 25
          Caption = '&Bet'#246'lt'#233's f'#225'jlb'#243'l...'
          TabOrder = 7
          OnClick = Button12Click
        end
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Eszk'#246'z'#246'k'
      ImageIndex = 1
      object GroupBox3: TGroupBox
        Left = 13
        Top = 254
        Width = 402
        Height = 139
        Caption = #218'j/kiv'#225'lasztott elem tulajdons'#225'gai'
        TabOrder = 0
        DesignSize = (
          402
          139)
        object Label6: TLabel
          Left = 50
          Top = 28
          Width = 23
          Height = 13
          Alignment = taRightJustify
          Caption = '&N'#233'v:'
        end
        object Label7: TLabel
          Left = 25
          Top = 56
          Width = 48
          Height = 13
          Alignment = taRightJustify
          Caption = '&El'#233'r'#233'si '#250't:'
        end
        object Edit1: TEdit
          Left = 85
          Top = 24
          Width = 203
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
        object Memo1: TMemo
          Left = 85
          Top = 56
          Width = 298
          Height = 65
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssVertical
          TabOrder = 1
          WantReturns = False
        end
        object Button8: TButton
          Left = 294
          Top = 22
          Width = 89
          Height = 25
          Anchors = [akTop, akRight]
          Caption = '&Tall'#243'z'#225's...'
          TabOrder = 2
          OnClick = Button8Click
        end
      end
      object GroupBox4: TGroupBox
        Left = 13
        Top = 8
        Width = 402
        Height = 240
        Caption = 'Eszk'#246'zlista'
        TabOrder = 1
        DesignSize = (
          402
          240)
        object Image2: TImage
          Left = 18
          Top = 22
          Width = 32
          Height = 32
        end
        object Label5: TLabel
          Left = 64
          Top = 18
          Width = 317
          Height = 48
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 
            'Ezen az oldalon defini'#225'lhat parancsokat, vagy programokat, melye' +
            'ket gyakran haszn'#225'l a virtu'#225'lis g'#233'pek kapcs'#225'n, '#233's '#237'gy a WinBox m' +
            'en'#252'j'#233'b'#337'l k'#246'nnyen el'#337' tudja h'#237'vni majd azokat.'
          WordWrap = True
          ExplicitWidth = 303
        end
        object ListView1: TListView
          Left = 18
          Top = 72
          Width = 365
          Height = 123
          Anchors = [akLeft, akTop, akRight, akBottom]
          Columns = <
            item
              Caption = 'N'#233'v'
              Width = 130
            end
            item
              AutoSize = True
              Caption = 'El'#233'r'#233'si '#250't'
            end>
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
          OnSelectItem = ListView1SelectItem
        end
        object Button9: TButton
          Tag = 1
          Left = 18
          Top = 201
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = '&Hozz'#225'ad'#225's'
          TabOrder = 1
          OnClick = Button9Click
        end
        object Button7: TButton
          Tag = 4
          Left = 310
          Top = 201
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = '&Ki'#252'r'#237't'#233's'
          TabOrder = 2
          OnClick = Button9Click
        end
        object Button6: TButton
          Tag = 3
          Left = 229
          Top = 201
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = '&T'#246'rl'#233's'
          TabOrder = 3
          OnClick = Button9Click
        end
        object Button10: TButton
          Tag = 2
          Left = 99
          Top = 201
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = '&M'#243'dos'#237't'#225's'
          TabOrder = 4
          OnClick = Button9Click
        end
      end
    end
  end
  object Button1: TButton
    Left = 352
    Top = 453
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&M'#233'gse'
    ModalResult = 2
    TabOrder = 1
  end
  object Button2: TButton
    Left = 271
    Top = 453
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 2
    OnClick = Button2Click
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'exe'
    Filter = 
      'Futtathat'#243' f'#225'jlok (*.exe; *.com; *.cmd; *.bat)|*.exe; *.com; *.c' +
      'md; *.bat|Minden f'#225'jl (*.*)|*.*'
    Left = 48
    Top = 440
  end
  object OpenDialog2: TOpenDialog
    DefaultExt = 'cfg'
    FileName = '86box.cfg'
    Filter = 
      '86Box konfigur'#225'ci'#243's f'#225'jlok (86box.cfg)|86box.cfg|Minden f'#225'jl (*.' +
      '*)|*.*'
    Left = 112
    Top = 440
  end
end
