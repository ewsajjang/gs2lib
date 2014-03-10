object vMain: TvMain
  Left = 0
  Top = 0
  Caption = 'vMain'
  ClientHeight = 69
  ClientWidth = 207
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 105
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 24
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Logggggg'
    TabOrder = 1
    OnClick = Button2Click
  end
end
