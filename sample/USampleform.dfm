object FSampleform: TFSampleform
  Left = 0
  Top = 0
  AlphaBlend = True
  AlphaBlendValue = 200
  Caption = 'Flowmotion Sample'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBlack
  TransparentColor = True
  TransparentColorValue = clFuchsia
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Flowmotion1: TFlowmotion
    Left = 96
    Top = 0
    Width = 532
    Height = 442
    AnimationSpeed = 12
    Spacing = 0
    BackgroundColor = clBlack
    ZoomAnimationType = zatFade
    Color = clBlack
    ParentColor = False
    TabOrder = 0
    TabStop = True
  end
  object Button1: TButton
    Left = 8
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Add 1 pic'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Add piclist'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 3
    OnClick = Button3Click
  end
end
