object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'My Alg Demo Form'
  ClientHeight = 489
  ClientWidth = 695
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 489
    Align = alLeft
    TabOrder = 0
    object Button1: TButton
      Left = 24
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Print'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 24
      Top = 56
      Width = 75
      Height = 25
      Caption = 'Page Setup'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 24
      Top = 104
      Width = 113
      Height = 25
      Caption = 'Print multipage'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 24
      Top = 144
      Width = 75
      Height = 25
      Caption = 'Preview'
      TabOrder = 3
      OnClick = Button4Click
    end
  end
  object ScrollBox1: TScrollBox
    Left = 185
    Top = 0
    Width = 510
    Height = 489
    HorzScrollBar.Smooth = True
    HorzScrollBar.Style = ssHotTrack
    HorzScrollBar.Tracking = True
    VertScrollBar.Smooth = True
    VertScrollBar.Style = ssHotTrack
    VertScrollBar.Tracking = True
    Align = alClient
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object PreviewImage: TImage
      Left = 0
      Top = 0
      Width = 105
      Height = 105
      AutoSize = True
    end
  end
  object JvPageSetupDialog1: TJvPageSetupDialog
    Left = 104
    Top = 152
  end
end
