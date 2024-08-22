object Form1: TForm1
  Left = 369
  Top = 174
  Caption = 'NiceSideBar Demo - priyatna.org'
  ClientHeight = 442
  ClientWidth = 586
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesktopCenter
  OnCreate = FormCreate
  TextHeight = 15
  object Button1: TButton
    Left = 312
    Top = 120
    Width = 129
    Height = 25
    Caption = 'Set ItemIndex'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 312
    Top = 160
    Width = 129
    Height = 25
    Caption = 'Set SubItemIndex'
    TabOrder = 1
    OnClick = Button2Click
  end
end
