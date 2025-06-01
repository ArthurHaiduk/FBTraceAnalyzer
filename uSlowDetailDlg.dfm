object fSlowDetailDlg: TfSlowDetailDlg
  Left = 0
  Top = 0
  Caption = 'fSlowDetailDlg'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 258
    Width = 635
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitLeft = 264
    ExplicitTop = 192
    ExplicitWidth = 185
    object btnClose: TButton
      Left = 552
      Top = 8
      Width = 75
      Height = 25
      Caption = 'btnClose'
      TabOrder = 0
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 635
    Height = 258
    ActivePage = tsSQL
    Align = alClient
    TabOrder = 1
    object tsSQL: TTabSheet
      Caption = 'tsSQL'
      object MemoSQL: TMemo
        Left = 0
        Top = 0
        Width = 627
        Height = 230
        Align = alClient
        Lines.Strings = (
          'MemoSQL')
        TabOrder = 0
        ExplicitLeft = 168
        ExplicitTop = 72
        ExplicitWidth = 185
        ExplicitHeight = 89
      end
    end
    object tsPlan: TTabSheet
      Caption = 'tsPlan'
      ImageIndex = 1
      object MemoPlan: TMemo
        Left = 0
        Top = 0
        Width = 627
        Height = 230
        Align = alClient
        Lines.Strings = (
          'Memo1')
        TabOrder = 0
        ExplicitLeft = 168
        ExplicitTop = 72
        ExplicitWidth = 185
        ExplicitHeight = 89
      end
    end
    object tsParams: TTabSheet
      Caption = 'tsParams'
      ImageIndex = 2
      object MemoParams: TMemo
        Left = 0
        Top = 0
        Width = 627
        Height = 230
        Align = alClient
        Lines.Strings = (
          'Memo1')
        TabOrder = 0
        ExplicitLeft = 168
        ExplicitTop = 72
        ExplicitWidth = 185
        ExplicitHeight = 89
      end
    end
    object TsPerf: TTabSheet
      Caption = 'TsPerf'
      ImageIndex = 3
      object MemoPerf: TMemo
        Left = 0
        Top = 0
        Width = 627
        Height = 230
        Align = alClient
        Lines.Strings = (
          'Memo1')
        TabOrder = 0
        ExplicitLeft = 168
        ExplicitTop = 72
        ExplicitWidth = 185
        ExplicitHeight = 89
      end
    end
  end
end
