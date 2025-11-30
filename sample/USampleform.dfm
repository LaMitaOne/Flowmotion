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
    Left = 96
    Top = 24
    Width = 929
    Height = 713
    LoadMode = lmLazy
    FlowLayout = flSorted
    Spacing = 4
    MaxColumns = 24
    MaxRows = 24
    ThreadPriority = tpNormal
    PageSize = 100
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBlack
    ParentColor = False
    TabOrder = 5
    TabStop = True
  end
  object Button1: TButton
    Left = 8
    Top = 125
    Width = 75
    Height = 65
    Caption = 'Add 1 pic random entry'
    TabOrder = 0
    WordWrap = True
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 423
    Width = 75
    Height = 66
    Caption = 'Load piclist'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 536
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 144
    Top = 48
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
    Top = 215
    Width = 75
    Height = 66
    Caption = 'Add 1 pic from rect'
    TabOrder = 4
    WordWrap = True
    OnClick = Button5Click
  end
end
