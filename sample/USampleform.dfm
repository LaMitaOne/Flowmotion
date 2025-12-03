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
  object Label1: TLabel
    Left = 192
    Top = 208
    Width = 98
    Height = 40
    Caption = 'Clicked'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -29
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
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
    TabOrder = 5
    TabStop = True
    ExplicitLeft = -24
    ExplicitTop = -8
  end
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
  object Button2: TButton
    Left = 8
    Top = 151
    Width = 75
    Height = 66
    Caption = 'Load piclist'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 223
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 136
    Top = 56
    Width = 161
    Height = 81
    Caption = 'Clear to pos'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 8
    Top = 79
    Width = 75
    Height = 66
    Caption = 'Add 1 pic from rect'
    TabOrder = 4
    WordWrap = True
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 8
    Top = 285
    Width = 25
    Height = 25
    Caption = '<'
    TabOrder = 6
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 58
    Top = 285
    Width = 25
    Height = 25
    Caption = '>'
    TabOrder = 7
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 8
    Top = 254
    Width = 75
    Height = 25
    Caption = 'Deselect'
    TabOrder = 8
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 8
    Top = 316
    Width = 75
    Height = 66
    Caption = 'Load background pic'
    TabOrder = 9
    WordWrap = True
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 8
    Top = 388
    Width = 75
    Height = 66
    Caption = 'Clear background pic'
    TabOrder = 10
    WordWrap = True
    OnClick = Button10Click
  end
  object Panel1: TPanel
    Left = 136
    Top = 176
    Width = 193
    Height = 72
    Caption = 'DblClicked selected'
    Color = 1907997
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clTeal
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 11
    Visible = False
    StyleElements = [seClient, seBorder]
  end
  object OpenDialog1: TOpenDialog
    Left = 88
    Top = 328
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = Timer1Timer
    Left = 216
    Top = 232
  end
end
