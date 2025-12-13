object FSampleform: TFSampleform
  Left = 0
  Top = 0
  AlphaBlendValue = 200
  Caption = 'Flowmotion Sample'
  ClientHeight = 960
  ClientWidth = 1438
  Color = clBlack
  TransparentColor = True
  TransparentColorValue = clFuchsia
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    1438
    960)
  TextHeight = 15
  object Flowmotion1: TFlowmotion
    Left = 0
    Top = 0
    Width = 1438
    Height = 960
    LoadMode = lmLazy
    FlowLayout = flSorted
    AnimationSpeed = 3
    Spacing = 4
    MaxColumns = 24
    MaxRows = 24
    OnImageLoad = Flowmotion1ImageLoad
    OnItemSelected = Flowmotion1ItemSelected
    ThreadPriority = tpNormal
    OnSelectedItemMouseDown = Flowmotion1SelectedItemMouseDown
    OnAllAnimationsFinished = Flowmotion1AllAnimationsFinished
    OnSelectedImageDblClick = Flowmotion1SelectedImageDblClick
    PageSize = 1000
    Align = alClient
    Color = clBlack
    ParentColor = False
    TabOrder = 0
    TabStop = True
    ExplicitWidth = 1442
    ExplicitHeight = 967
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 97
    Height = 927
    Color = 855309
    ParentBackground = False
    TabOrder = 1
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 65
      Caption = 'Add 1 pic random entry'
      TabOrder = 0
      WordWrap = True
      OnClick = Button1Click
    end
    object Button5: TButton
      Left = 8
      Top = 79
      Width = 75
      Height = 66
      Caption = 'Add 1 pic from rect'
      TabOrder = 1
      WordWrap = True
      OnClick = Button5Click
    end
    object Button2: TButton
      Left = 8
      Top = 295
      Width = 75
      Height = 34
      Caption = 'Load piclist'
      TabOrder = 2
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 5
      Top = 822
      Width = 75
      Height = 25
      Caption = 'Clear'
      TabOrder = 3
      OnClick = Button3Click
    end
    object Button8: TButton
      Left = 8
      Top = 471
      Width = 75
      Height = 25
      Caption = 'Deselect'
      TabOrder = 4
      OnClick = Button8Click
    end
    object Button6: TButton
      Left = 8
      Top = 431
      Width = 25
      Height = 25
      Caption = '<'
      TabOrder = 5
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 58
      Top = 431
      Width = 25
      Height = 25
      Caption = '>'
      TabOrder = 6
      OnClick = Button7Click
    end
    object Button9: TButton
      Left = 8
      Top = 516
      Width = 75
      Height = 66
      Caption = 'Load background pic'
      TabOrder = 7
      WordWrap = True
      OnClick = Button9Click
    end
    object Button10: TButton
      Left = 8
      Top = 596
      Width = 75
      Height = 66
      Caption = 'Clear background pic'
      TabOrder = 8
      WordWrap = True
      OnClick = Button10Click
    end
    object Button4: TButton
      Left = 5
      Top = 864
      Width = 75
      Height = 49
      Caption = 'Clear to pos'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 9
      WordWrap = True
      OnClick = Button4Click
    end
    object Button11: TButton
      Left = 8
      Top = 151
      Width = 75
      Height = 66
      Caption = 'Add 1 pic async'
      TabOrder = 10
      WordWrap = True
      OnClick = Button11Click
    end
    object Button12: TButton
      Left = 5
      Top = 335
      Width = 86
      Height = 34
      Caption = 'Add list async'
      TabOrder = 11
      OnClick = Button12Click
    end
    object Button13: TButton
      Left = 8
      Top = 223
      Width = 75
      Height = 66
      Caption = 'Add 1 pic opendialog'
      TabOrder = 12
      WordWrap = True
      OnClick = Button13Click
    end
    object Button15: TButton
      Left = 5
      Top = 679
      Width = 75
      Height = 48
      Caption = 'Move last image to 0'
      TabOrder = 13
      WordWrap = True
      OnClick = Button15Click
    end
    object Button16: TButton
      Left = 5
      Top = 743
      Width = 75
      Height = 50
      Caption = 'Drag selected on'
      TabOrder = 14
      WordWrap = True
      OnClick = Button16Click
    end
    object Button19: TButton
      Left = 8
      Top = 375
      Width = 83
      Height = 50
      Caption = 'Flood 150 pics async'
      TabOrder = 15
      WordWrap = True
      OnClick = Button19Click
    end
  end
  object Panel2: TPanel
    Left = 1221
    Top = 4
    Width = 217
    Height = 773
    Anchors = [akTop, akRight]
    Color = 855309
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 2
    StyleElements = [seClient, seBorder]
    ExplicitLeft = 1225
    object Label1: TLabel
      Left = 16
      Top = 11
      Width = 87
      Height = 15
      Caption = 'Animationspeed'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 16
      Top = 50
      Width = 57
      Height = 15
      Caption = 'Glowwidth'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 16
      Top = 91
      Width = 76
      Height = 15
      Caption = 'Hottrackwidth'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 8
      Top = 223
      Width = 39
      Height = 15
      Caption = 'Layout:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 88
      Top = 215
      Width = 121
      Height = 30
      Caption = 'Save/load positions for freefloat layout:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Panel3: TPanel
      Left = 8
      Top = 168
      Width = 65
      Height = 33
      Caption = 'Glowcolor'
      Color = clAqua
      ParentBackground = False
      TabOrder = 0
      OnClick = Panel3Click
    end
    object Panel4: TPanel
      Left = 79
      Top = 168
      Width = 65
      Height = 33
      Caption = 'HotColor'
      Color = clTeal
      ParentBackground = False
      TabOrder = 1
      OnClick = Panel4Click
    end
    object Button14: TButton
      Left = 172
      Top = 7
      Width = 39
      Height = 25
      Caption = 'reset'
      TabOrder = 2
      OnClick = Button14Click
    end
    object CheckBox1: TCheckBox
      Left = 40
      Top = 145
      Width = 121
      Height = 17
      Caption = 'selected Breathing'
      Checked = True
      Color = 855309
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      State = cbChecked
      TabOrder = 3
      StyleElements = [seClient, seBorder]
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 122
      Width = 121
      Height = 17
      Caption = 'Hotzoom enabled'
      Checked = True
      Color = 855309
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      State = cbChecked
      TabOrder = 4
      StyleElements = [seClient, seBorder]
      OnClick = CheckBox2Click
    end
    object SpinEdit1: TSpinEdit
      Left = 109
      Top = 8
      Width = 57
      Height = 24
      MaxValue = 20
      MinValue = 0
      TabOrder = 5
      Value = 3
      OnChange = SpinEdit1Change
    end
    object SpinEdit2: TSpinEdit
      Left = 109
      Top = 47
      Width = 57
      Height = 24
      MaxValue = 20
      MinValue = 0
      TabOrder = 6
      Value = 2
      OnChange = SpinEdit2Change
    end
    object SpinEdit3: TSpinEdit
      Left = 109
      Top = 88
      Width = 57
      Height = 24
      MaxValue = 20
      MinValue = 0
      TabOrder = 7
      Value = 1
      OnChange = SpinEdit3Change
    end
    object ComboBox1: TComboBox
      Left = 7
      Top = 251
      Width = 66
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 8
      Text = 'normal'
      OnChange = ComboBox1Change
      Items.Strings = (
        'normal'
        'free float')
    end
    object Button17: TButton
      Left = 94
      Top = 254
      Width = 113
      Height = 25
      Caption = 'Save positions'
      Enabled = False
      TabOrder = 9
      OnClick = Button17Click
    end
    object Button18: TButton
      Left = 94
      Top = 285
      Width = 113
      Height = 25
      Caption = 'Load pics with pos'
      Enabled = False
      TabOrder = 10
      OnClick = Button18Click
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 344
      Width = 185
      Height = 417
      Caption = 'Caption'
      TabOrder = 11
      object Label6: TLabel
        Left = 16
        Top = 297
        Width = 46
        Height = 15
        Caption = 'Font size'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object Label7: TLabel
        Left = 16
        Top = 337
        Width = 31
        Height = 15
        Caption = 'Alpha'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object Label8: TLabel
        Left = 16
        Top = 381
        Width = 42
        Height = 15
        Caption = 'Y-offset'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object Label9: TLabel
        Left = 88
        Top = 202
        Width = 70
        Height = 15
        Caption = 'selected item'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object Label10: TLabel
        Left = 96
        Top = 97
        Width = 70
        Height = 15
        Caption = 'normal items'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object CheckBox3: TCheckBox
        Left = 16
        Top = 25
        Width = 121
        Height = 17
        Caption = 'show caption'
        Checked = True
        Color = 855309
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        State = cbChecked
        TabOrder = 0
        StyleElements = [seClient, seBorder]
        OnClick = CheckBox3Click
      end
      object Panel7: TPanel
        Left = 3
        Top = 88
        Width = 65
        Height = 33
        Caption = 'Font color'
        Color = clWhite
        ParentBackground = False
        TabOrder = 1
        OnClick = Panel7Click
      end
      object Panel8: TPanel
        Left = 3
        Top = 127
        Width = 166
        Height = 33
        Caption = 'Caption background color'
        Color = clBlack
        ParentBackground = False
        TabOrder = 2
        OnClick = Panel8Click
      end
      object SpinEdit4: TSpinEdit
        Left = 101
        Top = 294
        Width = 57
        Height = 24
        MaxValue = 40
        MinValue = 2
        TabOrder = 3
        Value = 10
        OnChange = SpinEdit4Change
      end
      object SpinEdit5: TSpinEdit
        Left = 101
        Top = 334
        Width = 57
        Height = 24
        MaxValue = 255
        MinValue = 2
        TabOrder = 4
        Value = 180
        OnChange = SpinEdit5Change
      end
      object SpinEdit6: TSpinEdit
        Left = 101
        Top = 377
        Width = 57
        Height = 24
        MaxValue = 200
        MinValue = 0
        TabOrder = 5
        Value = 8
        OnChange = SpinEdit6Change
      end
      object CheckBox4: TCheckBox
        Left = 32
        Top = 48
        Width = 121
        Height = 17
        Caption = 'only on hover'
        Checked = True
        Color = 855309
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        State = cbChecked
        TabOrder = 6
        StyleElements = [seClient, seBorder]
        OnClick = CheckBox4Click
      end
      object Panel9: TPanel
        Left = 3
        Top = 231
        Width = 166
        Height = 33
        Caption = 'Caption background color'
        Color = clAqua
        ParentBackground = False
        TabOrder = 7
        OnClick = Panel9Click
      end
      object Panel10: TPanel
        Left = 3
        Top = 192
        Width = 65
        Height = 33
        Caption = 'Font color'
        Color = clBlack
        ParentBackground = False
        TabOrder = 8
        OnClick = Panel10Click
      end
    end
    object CheckBox5: TCheckBox
      Left = 8
      Top = 316
      Width = 121
      Height = 17
      Caption = 'show hints'
      Checked = True
      Color = 855309
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      State = cbChecked
      TabOrder = 12
      StyleElements = [seClient, seBorder]
      OnClick = CheckBox5Click
    end
  end
  object Panel5: TPanel
    Left = 312
    Top = 251
    Width = 217
    Height = 143
    Caption = 'ActivationZone 1 (Drag selected here)'
    TabOrder = 3
  end
  object Panel6: TPanel
    Left = 328
    Top = 759
    Width = 217
    Height = 135
    Caption = 'ActivationZone 2 (Drag selected here)'
    TabOrder = 4
  end
  object Panel11: TPanel
    Left = 888
    Top = 743
    Width = 297
    Height = 184
    Anchors = [akRight, akBottom]
    Caption = 'Keep AreaFreeRect (not right working atm)'
    TabOrder = 5
    Visible = False
  end
  object OpenDialog1: TOpenDialog
    Left = 144
    Top = 32
  end
  object ColorDialog1: TColorDialog
    Left = 144
    Top = 112
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 50
    OnTimer = Timer1Timer
    Left = 280
    Top = 40
  end
  object Timer2: TTimer
    Enabled = False
    Interval = 10
    OnTimer = Timer2Timer
    Left = 576
    Top = 88
  end
end
