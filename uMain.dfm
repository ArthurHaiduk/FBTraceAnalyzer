object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 525
  ClientWidth = 1128
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pMain: TPanel
    Left = 0
    Top = 0
    Width = 1128
    Height = 525
    Align = alClient
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 1
      Top = 25
      Width = 1126
      Height = 499
      ActivePage = tsGeneral
      Align = alClient
      TabOrder = 0
      object tsGeneral: TTabSheet
        Caption = 'General'
        object gbSelectStatements: TGroupBox
          Left = 3
          Top = 3
          Width = 241
          Height = 130
          Caption = 'Select statements'
          TabOrder = 0
          object lbSelectCount: TLabel
            Left = 16
            Top = 24
            Width = 29
            Height = 13
            Caption = 'Count'
          end
          object lbSelectTotalTime: TLabel
            Left = 16
            Top = 51
            Width = 47
            Height = 13
            Caption = 'Total time'
          end
          object lbSelectMaxTime: TLabel
            Left = 16
            Top = 78
            Width = 43
            Height = 13
            Caption = 'Max time'
          end
          object lbSelectAvgTime: TLabel
            Left = 16
            Top = 105
            Width = 42
            Height = 13
            Caption = 'Avg time'
          end
          object edSelectCount: TEdit
            Left = 112
            Top = 21
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 0
          end
          object edSelectTotalTime: TEdit
            Left = 112
            Top = 48
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 1
          end
          object edSelectMaxTime: TEdit
            Left = 112
            Top = 75
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 2
          end
          object edSelectAvgTime: TEdit
            Left = 112
            Top = 102
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 3
          end
        end
        object gbUpdateStatements: TGroupBox
          Left = 497
          Top = 3
          Width = 241
          Height = 130
          Caption = 'Update statements'
          TabOrder = 1
          object Label5: TLabel
            Left = 16
            Top = 24
            Width = 29
            Height = 13
            Caption = 'Count'
          end
          object Label6: TLabel
            Left = 16
            Top = 51
            Width = 47
            Height = 13
            Caption = 'Total time'
          end
          object Label7: TLabel
            Left = 16
            Top = 78
            Width = 43
            Height = 13
            Caption = 'Max time'
          end
          object Label8: TLabel
            Left = 16
            Top = 105
            Width = 42
            Height = 13
            Caption = 'Avg time'
          end
          object edUpdateCount: TEdit
            Left = 112
            Top = 21
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 0
          end
          object edUpdateTotalTime: TEdit
            Left = 112
            Top = 48
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 1
          end
          object edUpdateMaxTime: TEdit
            Left = 112
            Top = 75
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 2
          end
          object edUpdateAvgTime: TEdit
            Left = 112
            Top = 102
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 3
          end
        end
        object gbInsertStatements: TGroupBox
          Left = 250
          Top = 3
          Width = 241
          Height = 130
          Caption = 'Insert statements'
          TabOrder = 2
          object lbInsertCount: TLabel
            Left = 16
            Top = 24
            Width = 29
            Height = 13
            Caption = 'Count'
          end
          object Label2: TLabel
            Left = 16
            Top = 51
            Width = 47
            Height = 13
            Caption = 'Total time'
          end
          object Label3: TLabel
            Left = 16
            Top = 78
            Width = 43
            Height = 13
            Caption = 'Max time'
          end
          object Label4: TLabel
            Left = 16
            Top = 105
            Width = 42
            Height = 13
            Caption = 'Avg time'
          end
          object edInsertCount: TEdit
            Left = 112
            Top = 21
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 0
          end
          object edInsertTotalTime: TEdit
            Left = 112
            Top = 48
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 1
          end
          object edInsertMaxTime: TEdit
            Left = 112
            Top = 75
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 2
          end
          object edInsertAvgTime: TEdit
            Left = 112
            Top = 102
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 3
          end
        end
        object gbDeleteStatements: TGroupBox
          Left = 744
          Top = 3
          Width = 241
          Height = 130
          Caption = 'Delete statements'
          TabOrder = 3
          object Label9: TLabel
            Left = 16
            Top = 24
            Width = 29
            Height = 13
            Caption = 'Count'
          end
          object Label10: TLabel
            Left = 16
            Top = 51
            Width = 47
            Height = 13
            Caption = 'Total time'
          end
          object Label11: TLabel
            Left = 16
            Top = 78
            Width = 43
            Height = 13
            Caption = 'Max time'
          end
          object Label12: TLabel
            Left = 16
            Top = 105
            Width = 42
            Height = 13
            Caption = 'Avg time'
          end
          object edDeleteCount: TEdit
            Left = 112
            Top = 21
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 0
          end
          object edDeleteTotalTime: TEdit
            Left = 112
            Top = 48
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 1
          end
          object edDeleteMaxTime: TEdit
            Left = 112
            Top = 75
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 2
          end
          object edDeleteAvgTime: TEdit
            Left = 112
            Top = 102
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 3
          end
        end
        object gbStats: TGroupBox
          Left = 3
          Top = 132
          Width = 982
          Height = 53
          Caption = 'Statistics count'
          TabOrder = 4
          object lbSt1RollbackCount: TLabel
            Left = 757
            Top = 24
            Width = 69
            Height = 13
            Caption = 'Rollback count'
          end
          object lbSt1CommitCount: TLabel
            Left = 510
            Top = 24
            Width = 65
            Height = 13
            Caption = 'Commit count'
          end
          object lbSt1ErrorCount: TLabel
            Left = 263
            Top = 24
            Width = 54
            Height = 13
            Caption = 'Error count'
          end
          object lbSt1RecordCount: TLabel
            Left = 16
            Top = 24
            Width = 64
            Height = 13
            Caption = 'Record count'
          end
          object edSt1ErrorCount: TEdit
            Left = 359
            Top = 21
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 0
          end
          object edSt1CommitCount: TEdit
            Left = 606
            Top = 21
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 1
          end
          object edSt1RollbackCount: TEdit
            Left = 853
            Top = 21
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 2
          end
          object edSt1RecordCount: TEdit
            Left = 112
            Top = 21
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 3
          end
        end
        object cxVerticalGrid1: TcxVerticalGrid
          Left = 3
          Top = 191
          Width = 233
          Height = 200
          TabOrder = 5
          Version = 1
        end
        object cxVerticalGrid2: TcxVerticalGrid
          Left = 266
          Top = 191
          Width = 247
          Height = 200
          TabOrder = 6
          Version = 1
        end
        object cxVerticalGrid3: TcxVerticalGrid
          Left = 555
          Top = 191
          Width = 247
          Height = 200
          TabOrder = 7
          Version = 1
        end
      end
      object tsOverview: TTabSheet
        Caption = 'Overview'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object cxGridErrors: TcxGrid
          Left = 0
          Top = 0
          Width = 1118
          Height = 200
          Align = alTop
          TabOrder = 0
          object cxGridErrorsDBTableView1: TcxGridDBTableView
            NavigatorButtons.ConfirmDelete = False
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <>
            DataController.Summary.SummaryGroups = <>
          end
          object cxGridErrorsLevel1: TcxGridLevel
            GridView = cxGridErrorsDBTableView1
          end
        end
        object cxGridIndexes: TcxGrid
          Left = 0
          Top = 200
          Width = 1118
          Height = 200
          Align = alTop
          TabOrder = 1
          object cxGridIndexesDBTableView1: TcxGridDBTableView
            NavigatorButtons.ConfirmDelete = False
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <>
            DataController.Summary.SummaryGroups = <>
          end
          object cxGridIndexesLevel1: TcxGridLevel
            GridView = cxGridIndexesDBTableView1
          end
        end
      end
      object tsSlow: TTabSheet
        Caption = 'Slow queries'
        ImageIndex = 2
        object cxGridSlow: TcxGrid
          Left = 0
          Top = 0
          Width = 1118
          Height = 471
          Align = alClient
          TabOrder = 0
          object tvSlow: TcxGridDBTableView
            OnDblClick = tvSlowDblClick
            NavigatorButtons.ConfirmDelete = False
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <>
            DataController.Summary.SummaryGroups = <>
            object Rank: TcxGridDBColumn
            end
            object TimeMs: TcxGridDBColumn
            end
            object SQLType: TcxGridDBColumn
            end
            object Rows: TcxGridDBColumn
            end
            object PlanNat: TcxGridDBColumn
            end
            object SQLText: TcxGridDBColumn
            end
            object Plan: TcxGridDBColumn
            end
            object Params: TcxGridDBColumn
            end
            object Perf: TcxGridDBColumn
            end
          end
          object cxGridSlowLevel1: TcxGridLevel
            GridView = tvSlow
          end
        end
      end
    end
    object pTop: TPanel
      Left = 1
      Top = 1
      Width = 1126
      Height = 24
      Align = alTop
      TabOrder = 1
      object bGetStatistics: TButton
        Left = 1
        Top = 1
        Width = 1124
        Height = 22
        Align = alClient
        Caption = 'Get statistics'
        TabOrder = 0
        OnClick = bGetStatisticsClick
      end
    end
  end
  object mmMain: TMainMenu
    Left = 64
    Top = 432
    object miFile: TMenuItem
      Caption = 'File'
      object miOpen: TMenuItem
        Caption = 'Open'
        OnClick = miOpenClick
      end
    end
  end
  object odMain: TOpenDialog
    Left = 128
    Top = 432
  end
end
