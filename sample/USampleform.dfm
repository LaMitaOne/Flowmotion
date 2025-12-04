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
    Left = 24
    Top = 19
    Width = 97
    Height = 654
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
      Top = 223
      Width = 75
      Height = 34
      Caption = 'Load piclist'
      TabOrder = 2
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 8
      Top = 561
      Width = 75
      Height = 25
      Caption = 'Clear'
      TabOrder = 3
      OnClick = Button3Click
    end
    object Button8: TButton
      Left = 8
      Top = 351
      Width = 75
      Height = 25
      Caption = 'Deselect'
      TabOrder = 4
      OnClick = Button8Click
    end
    object Button6: TButton
      Left = 8
      Top = 311
      Width = 25
      Height = 25
      Caption = '<'
      TabOrder = 5
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 58
      Top = 311
      Width = 25
      Height = 25
      Caption = '>'
      TabOrder = 6
      OnClick = Button7Click
    end
    object Button9: TButton
      Left = 8
      Top = 396
      Width = 75
      Height = 66
      Caption = 'Load background pic'
      TabOrder = 7
      WordWrap = True
      OnClick = Button9Click
    end
    object Button10: TButton
      Left = 8
      Top = 476
      Width = 75
      Height = 66
      Caption = 'Clear background pic'
      TabOrder = 8
      WordWrap = True
      OnClick = Button10Click
    end
    object Button4: TButton
      Left = 8
      Top = 592
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
      Left = 8
      Top = 263
      Width = 75
      Height = 34
      Caption = 'Add list async'
      TabOrder = 11
      OnClick = Button12Click
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 88
    Top = 328
  end
end
