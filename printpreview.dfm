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
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
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
      Width = 72
      Height = 15
      Caption = 'Margins (cm)'
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
      Left = 15
      Top = 259
      Width = 54
      Height = 15
      Caption = 'Zoom (%)'
    end
    object Label7: TLabel
      Left = 7
      Top = 222
      Width = 65
      Height = 15
      Caption = 'DPI monitor'
    end
    object Label8: TLabel
      Left = 7
      Top = 287
      Width = 91
      Height = 15
      Caption = 'Paper size (mm)'
    end
    object Label9: TLabel
      Left = 7
      Top = 320
      Width = 31
      Height = 15
      Caption = 'Width'
    end
    object Label10: TLabel
      Left = 7
      Top = 348
      Width = 36
      Height = 15
      Caption = 'Height'
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
      Top = 380
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
    end
    object ZoomEdit: TEdit
      Left = 79
      Top = 255
      Width = 35
      Height = 23
      TabOrder = 6
      Text = '100'
    end
    object ZoomUpDown: TUpDown
      Left = 114
      Top = 255
      Width = 15
      Height = 23
      Associate = ZoomEdit
      Min = 10
      Max = 1000
      Position = 100
      TabOrder = 7
    end
    object btPrint: TButton
      Left = 21
      Top = 408
      Width = 121
      Height = 22
      Caption = 'Print'
      TabOrder = 8
      OnClick = btPrintClick
    end
    object DPIEdit: TEdit
      Left = 79
      Top = 219
      Width = 35
      Height = 23
      TabOrder = 9
      Text = '102'
    end
    object UpDown1: TUpDown
      Left = 114
      Top = 219
      Width = 16
      Height = 23
      Associate = DPIEdit
      Min = 50
      Max = 4801
      Position = 102
      TabOrder = 10
    end
    object edPaperWidth: TEdit
      Left = 53
      Top = 315
      Width = 88
      Height = 23
      TabOrder = 11
      Text = '210'
      OnKeyPress = LeftMarginEditKeyPress
    end
    object edPaperHeight: TEdit
      Left = 53
      Top = 344
      Width = 88
      Height = 23
      TabOrder = 12
      Text = '297'
      OnKeyPress = LeftMarginEditKeyPress
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 557
    Height = 808
    HorzScrollBar.Smooth = True
    HorzScrollBar.Style = ssHotTrack
    HorzScrollBar.Tracking = True
    VertScrollBar.Smooth = True
    VertScrollBar.Style = ssHotTrack
    VertScrollBar.Tracking = True
    Align = alClient
    TabOrder = 1
    object ImagePreview: TImage
      Left = 0
      Top = 0
      Width = 105
      Height = 105
      AutoSize = True
    end
  end
end
