object Form1: TForm1
  Left = 192
  Top = 128
  Caption = 'Form1'
  ClientHeight = 808
  ClientWidth = 720
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 557
    Top = 0
    Width = 163
    Height = 808
    Align = alRight
    TabOrder = 0
    object Label1: TLabel
      Left = 7
      Top = 7
      Width = 78
      Height = 15
      Caption = 'Margins (inch)'
    end
    object Label2: TLabel
      Left = 7
      Top = 40
      Width = 20
      Height = 15
      Caption = 'Left'
    end
    object Label3: TLabel
      Left = 7
      Top = 68
      Width = 20
      Height = 15
      Caption = 'Top'
    end
    object Label4: TLabel
      Left = 7
      Top = 96
      Width = 29
      Height = 15
      Caption = 'Right'
    end
    object Label5: TLabel
      Left = 7
      Top = 124
      Width = 39
      Height = 15
      Caption = 'Bottom'
    end
    object Label6: TLabel
      Left = 7
      Top = 230
      Width = 54
      Height = 15
      Caption = 'Zoom (%)'
    end
    object LeftMarginEdit: TEdit
      Left = 53
      Top = 35
      Width = 88
      Height = 23
      TabOrder = 0
      OnKeyPress = LeftMarginEditKeyPress
    end
    object TopMarginEdit: TEdit
      Left = 53
      Top = 64
      Width = 88
      Height = 23
      TabOrder = 1
      OnKeyPress = LeftMarginEditKeyPress
    end
    object RightMarginEdit: TEdit
      Left = 53
      Top = 92
      Width = 88
      Height = 23
      TabOrder = 2
      OnKeyPress = LeftMarginEditKeyPress
    end
    object BottomMarginEdit: TEdit
      Left = 53
      Top = 120
      Width = 88
      Height = 23
      TabOrder = 3
      OnKeyPress = LeftMarginEditKeyPress
    end
    object ApplyMarginsButton: TButton
      Left = 21
      Top = 268
      Width = 121
      Height = 22
      Caption = 'Apply'
      TabOrder = 4
      OnClick = ApplyMarginsButtonClick
    end
    object OrientationRGroup: TRadioGroup
      Left = 7
      Top = 155
      Width = 142
      Height = 58
      Caption = 'Orientation'
      Items.Strings = (
        'Portrait'
        'Landscape')
      TabOrder = 5
      OnClick = OrientationRGroupClick
    end
    object ZoomEdit: TEdit
      Left = 71
      Top = 226
      Width = 35
      Height = 23
      TabOrder = 6
      Text = '70'
    end
    object ZoomUpDown: TUpDown
      Left = 106
      Top = 226
      Width = 15
      Height = 23
      Associate = ZoomEdit
      Increment = 10
      Position = 70
      TabOrder = 7
      OnClick = ZoomUpDownClick
    end
    object btPrint: TButton
      Left = 21
      Top = 296
      Width = 121
      Height = 22
      Caption = 'Print'
      TabOrder = 8
      OnClick = btPrintClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 557
    Height = 808
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object PreviewPaintbox: TPaintBox
      Left = 1
      Top = 1
      Width = 555
      Height = 806
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -37
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
      OnPaint = PreviewPaintboxPaint
      ExplicitWidth = 442
      ExplicitHeight = 398
    end
  end
end
