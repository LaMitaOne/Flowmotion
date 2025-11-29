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
  OnCreate = FormCreate
  DesignSize = (
    1434
    953)
  TextHeight = 15
  object Flowmotion1: TFlowmotion
    Left = 104
    Top = 8
    Width = 1330
    Height = 945
    LoadMode = lmLazy
    FlowLayout = flSorted
    AnimationSpeed = 10
    ThreadPriority = tpHigher
    PageSize = 100
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBlack
    ParentColor = False
    TabOrder = 0
    TabStop = True
    ExplicitWidth = 875
    ExplicitHeight = 749
  end
  object Button1: TButton
    Left = 8
    Top = 24
    Width = 75
    Height = 65
    Caption = 'Add 1 pic from random pos'
    TabOrder = 1
    WordWrap = True
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Load piclist'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 3
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
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 8
    Top = 95
    Width = 75
    Height = 66
    Caption = 'Add 1 pic from pos'
    TabOrder = 5
    WordWrap = True
    OnClick = Button5Click
  end
end
