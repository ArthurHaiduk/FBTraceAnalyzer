unit uSlowDetailDlg;

interface

uses
   SysUtils, Classes, Controls, StdCtrls, ComCtrls, ExtCtrls, Forms;
type
  TSlowStmtData = record
    SQLText   : string;
    PlanText  : string;
    ParamText : string;
    PerfText  : string;
  end;


type
  TfSlowDetailDlg = class(TForm)
    btnClose: TButton;
    PageControl1: TPageControl;
    tsSQL: TTabSheet;
    tsPlan: TTabSheet;
    tsParams: TTabSheet;
    TsPerf: TTabSheet;
    MemoSQL: TMemo;
    MemoPlan: TMemo;
    MemoParams: TMemo;
    MemoPerf: TMemo;
    pnlBottom: TPanel;
    procedure btnCloseClick(Sender: TObject);
  public
    procedure LoadFromSlowStmt(const Data: TSlowStmtData);
  end;

var
  fSlowDetailDlg: TfSlowDetailDlg;

implementation

{$R *.dfm}

procedure TfSlowDetailDlg.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfSlowDetailDlg.LoadFromSlowStmt(const Data: TSlowStmtData);
begin
  MemoSQL.Lines.Text    := Data.SQLText;
  MemoPlan.Lines.Text   := Data.PlanText;
  MemoParams.Lines.Text := Data.ParamText;
  MemoPerf.Lines.Text   := Data.PerfText;
end;


end.
