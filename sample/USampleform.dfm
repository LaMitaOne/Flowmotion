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
    Height = 726
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
      Left = 8
      Top = 633
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
      Left = 8
      Top = 664
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
  end
  object Panel2: TPanel
    Left = 224
    Top = 0
    Width = 297
    Height = 129
    Color = 855309
    ParentBackground = False
    TabOrder = 2
    object Label1: TLabel
      Left = 168
      Top = 8
      Width = 87
      Height = 15
      Caption = 'Animationspeed'
    end
    object Panel3: TPanel
      Left = 24
      Top = 24
      Width = 65
      Height = 33
      Caption = 'Glowcolor'
      TabOrder = 0
      OnClick = Panel3Click
    end
    object Panel4: TPanel
      Left = 24
      Top = 80
      Width = 65
      Height = 33
      Caption = 'HotColor'
      TabOrder = 1
      OnClick = Panel4Click
    end
    object TrackBar1: TTrackBar
      Left = 144
      Top = 24
      Width = 150
      Height = 45
      Max = 20
      Position = 3
      TabOrder = 2
      OnChange = TrackBar1Change
    end
    object Button14: TButton
      Left = 208
      Top = 64
      Width = 75
      Height = 25
      Caption = 'reset'
      TabOrder = 3
      OnClick = Button14Click
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
