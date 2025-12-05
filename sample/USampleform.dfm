object FSampleform: TFSampleform
  Left = 0
  Top = 0
  AlphaBlendValue = 200
  Caption = 'Flowmotion Sample'
  ClientHeight = 953
  ClientWidth = 1434
  Color = clBlack
  TransparentColor = True
  TransparentColorValue = clFuchsia
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    1434
    953)
  TextHeight = 15
  object Flowmotion1: TFlowmotion
    Left = 0
    Top = 0
    Width = 1434
    Height = 953
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
    PageSize = 100
    Align = alClient
    Color = clBlack
    ParentColor = False
    TabOrder = 0
    TabStop = True
    ExplicitWidth = 1438
    ExplicitHeight = 960
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 97
    Height = 881
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
      Top = 774
      Width = 75
      Height = 25
      Caption = 'Clear'
      TabOrder = 3
      OnClick = Button3Click
    end
    object Button8: TButton
      Left = 8
      Top = 423
      Width = 75
      Height = 25
      Caption = 'Deselect'
      TabOrder = 4
      OnClick = Button8Click
    end
    object Button6: TButton
      Left = 8
      Top = 383
      Width = 25
      Height = 25
      Caption = '<'
      TabOrder = 5
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 58
      Top = 383
      Width = 25
      Height = 25
      Caption = '>'
      TabOrder = 6
      OnClick = Button7Click
    end
    object Button9: TButton
      Left = 8
      Top = 468
      Width = 75
      Height = 66
      Caption = 'Load background pic'
      TabOrder = 7
      WordWrap = True
      OnClick = Button9Click
    end
    object Button10: TButton
      Left = 8
      Top = 548
      Width = 75
      Height = 66
      Caption = 'Clear background pic'
      TabOrder = 8
      WordWrap = True
      OnClick = Button10Click
    end
    object Button4: TButton
      Left = 5
      Top = 816
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
      Top = 631
      Width = 75
      Height = 48
      Caption = 'Move last image to 0'
      TabOrder = 13
      WordWrap = True
      OnClick = Button15Click
    end
    object Button16: TButton
      Left = 5
      Top = 695
      Width = 75
      Height = 50
      Caption = 'Drag selected on'
      TabOrder = 14
      WordWrap = True
      OnClick = Button16Click
    end
  end
  object Panel2: TPanel
    Left = 1217
    Top = 0
    Width = 217
    Height = 289
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
    ExplicitLeft = 1221
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
    object Panel3: TPanel
      Left = 16
      Top = 194
      Width = 65
      Height = 33
      Caption = 'Glowcolor'
      Color = clAqua
      ParentBackground = False
      TabOrder = 0
      OnClick = Panel3Click
    end
    object Panel4: TPanel
      Left = 16
      Top = 233
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
      Left = 16
      Top = 122
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
      Top = 145
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
  end
  object OpenDialog1: TOpenDialog
    Left = 144
    Top = 32
  end
  object ColorDialog1: TColorDialog
    Left = 144
    Top = 112
  end
end
