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
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet2: TTabSheet
      Caption = 'Alapvet'#337
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
    object TabSheet1: TTabSheet
      Caption = 'Eszk'#246'z'#246'k'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox3: TGroupBox
        Left = 18
        Top = 254
        Width = 388
        Height = 131
        Caption = #218'j/kiv'#225'lasztott elem tulajdons'#225'gai'
        TabOrder = 0
        DesignSize = (
          388
          131)
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
          Width = 189
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
        object Memo1: TMemo
          Left = 85
          Top = 56
          Width = 284
          Height = 57
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssVertical
          TabOrder = 1
          WantReturns = False
        end
        object Button8: TButton
          Left = 280
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
        Left = 18
        Top = 18
        Width = 388
        Height = 230
        Caption = 'Eszk'#246'zlista'
        TabOrder = 1
        DesignSize = (
          388
          230)
        object Image2: TImage
          Left = 18
          Top = 21
          Width = 32
          Height = 32
        end
        object Label5: TLabel
          Left = 64
          Top = 18
          Width = 303
          Height = 48
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 
            'Ezen az oldalon defini'#225'lhat parancsokat, vagy programokat, melye' +
            'ket gyakran haszn'#225'l a virtu'#225'lis g'#233'pek kapcs'#225'n, '#233's '#237'gy a WinBox m' +
            'en'#252'j'#233'b'#337'l k'#246'nnyen el'#337' tudja h'#237'vni majd azokat.'
          WordWrap = True
        end
        object ListView1: TListView
          Left = 18
          Top = 72
          Width = 351
          Height = 113
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
          Top = 191
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = '&Hozz'#225'ad'#225's'
          TabOrder = 1
          OnClick = Button9Click
        end
        object Button7: TButton
          Tag = 4
          Left = 296
          Top = 191
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = '&Ki'#252'r'#237't'#233's'
          TabOrder = 2
          OnClick = Button9Click
        end
        object Button6: TButton
          Tag = 3
          Left = 215
          Top = 191
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
          Top = 191
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
    Left = 216
    Top = 248
  end
end
