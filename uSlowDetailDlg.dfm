object fSlowDetailDlg: TfSlowDetailDlg
  Left = 0
  Top = 0
  Caption = 'fSlowDetailDlg'
  ClientHeight = 403
  ClientWidth = 906
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 376
    Width = 906
    Height = 27
    Align = alBottom
    TabOrder = 0
    object btnClose: TButton
      AlignWithMargins = True
      Left = 816
      Top = 4
      Width = 86
      Height = 19
      Align = alRight
      Caption = 'Close'
      TabOrder = 0
      OnClick = btnCloseClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 906
    Height = 376
    ActivePage = tsSQL
    Align = alClient
    TabOrder = 1
    object tsSQL: TTabSheet
      Caption = 'SQL'
      object MemoSQL: TMemo
        Left = 0
        Top = 0
        Width = 898
        Height = 348
        Align = alClient
        Lines.Strings = (
          'MemoSQL')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tsPlan: TTabSheet
      Caption = 'Plan'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 331
      object MemoPlan: TMemo
        Left = 0
        Top = 0
        Width = 898
        Height = 348
        Align = alClient
        Lines.Strings = (
          'Memo1')
        TabOrder = 0
      end
    end
    object tsParams: TTabSheet
      Caption = 'Input Parameters'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 331
      object MemoParams: TMemo
        Left = 0
        Top = 0
        Width = 898
        Height = 348
        Align = alClient
        Lines.Strings = (
          'Memo1')
        TabOrder = 0
      end
    end
    object TsPerf: TTabSheet
      Caption = 'Performance'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 331
      object MemoPerf: TMemo
        Left = 0
        Top = 0
        Width = 898
        Height = 348
        Align = alClient
        Lines.Strings = (
          'Memo1')
        TabOrder = 0
      end
    end
  end
end
