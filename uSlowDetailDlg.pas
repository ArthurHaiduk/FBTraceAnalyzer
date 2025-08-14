unit uSlowDetailDlg;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ComCtrls, ExtCtrls, Forms,
  Graphics, Messages, Math, uGutterHook;

type
  TSlowStmtData = record
    SQLText: string;
    PlanText: string;
    ParamText: string;
    PerfText: string;
  end;

type
  TfSlowDetailDlg = class(TForm)
    btnClose: TButton;
    PageControl1: TPageControl;
    tsSQL, tsPlan, tsParams, tsPerf: TTabSheet;
    MemoSQL, MemoPlan, MemoParams, MemoPerf: TMemo;
    pnlBottom: TPanel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FInited: Boolean;
    FGutSQL: TGutterHook;
    FGutPlan: TGutterHook;
    FGutParams: TGutterHook;
    FGutPerf: TGutterHook;
    procedure EnsureMemoFixed;
  public
    procedure LoadFromSlowStmt(const Data: TSlowStmtData);
  end;

implementation

{$R *.dfm}

procedure SetupMemoFixed(M: TMemo);
begin
  M.ParentFont := False;
  if Screen.Fonts.IndexOf('Consolas') >= 0 then
    M.Font.Name := 'Consolas'
  else if Screen.Fonts.IndexOf('Lucida Console') >= 0 then
    M.Font.Name := 'Lucida Console'
  else
    M.Font.Name := 'Courier New';
  M.Font.Pitch := fpFixed;
  M.WordWrap := False;
  M.ScrollBars := ssBoth;
end;

procedure TfSlowDetailDlg.EnsureMemoFixed;
begin
  if FInited then
    Exit;

  SetupMemoFixed(MemoSQL);
  SetupMemoFixed(MemoPlan);
  SetupMemoFixed(MemoParams);
  SetupMemoFixed(MemoPerf);

  FGutSQL := TGutterHook.Create(MemoSQL);
  FGutPlan := TGutterHook.Create(MemoPlan);
  FGutParams := TGutterHook.Create(MemoParams);
  FGutPerf := TGutterHook.Create(MemoPerf);

  FInited := True;
end;

procedure TfSlowDetailDlg.FormCreate(Sender: TObject);
begin
  EnsureMemoFixed;
end;

procedure TfSlowDetailDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FGutSQL);
  FreeAndNil(FGutPlan);
  FreeAndNil(FGutParams);
  FreeAndNil(FGutPerf);
end;

procedure TfSlowDetailDlg.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfSlowDetailDlg.LoadFromSlowStmt(const Data: TSlowStmtData);
begin
  EnsureMemoFixed;
  MemoSQL.Lines.Text := Data.SQLText;
  MemoPlan.Lines.Text := Data.PlanText;
  MemoParams.Lines.Text := Data.ParamText;
  MemoPerf.Lines.Text := Data.PerfText;

  if Assigned(FGutSQL) then
    FGutSQL.Invalidate;
  if Assigned(FGutPlan) then
    FGutPlan.Invalidate;
  if Assigned(FGutParams) then
    FGutParams.Invalidate;
  if Assigned(FGutPerf) then
    FGutPerf.Invalidate;
end;

end.
