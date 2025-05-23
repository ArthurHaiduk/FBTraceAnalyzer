object fProgressDlg: TfProgressDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Progress'
  ClientHeight = 63
  ClientWidth = 510
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pTop: TPanel
    Left = 0
    Top = 0
    Width = 510
    Height = 33
    Align = alTop
    TabOrder = 0
    object pb: TProgressBar
      AlignWithMargins = True
      Left = 1
      Top = 6
      Width = 508
      Height = 21
      Margins.Left = 0
      Margins.Top = 5
      Margins.Right = 0
      Margins.Bottom = 5
      Align = alClient
      TabOrder = 0
    end
  end
  object pMain: TPanel
    Left = 0
    Top = 33
    Width = 510
    Height = 30
    Align = alClient
    TabOrder = 1
    object lblMsg: TLabel
      Left = 232
      Top = 6
      Width = 54
      Height = 13
      Caption = 'Process file'
    end
  end
end
