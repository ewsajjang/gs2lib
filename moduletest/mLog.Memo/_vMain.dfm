object vMain: TvMain
  Left = 0
  Top = 0
  Caption = 'vMain'
  ClientHeight = 414
  ClientWidth = 602
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
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 518
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 585
    Height = 362
    ImeName = 'Microsoft IME 2010'
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Button2: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Logggggg'
    TabOrder = 2
    OnClick = Button2Click
  end
  object CheckBox1: TCheckBox
    Left = 439
    Top = 12
    Width = 73
    Height = 17
    Caption = 'AutoScroll'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = CheckBox1Click
  end
end
