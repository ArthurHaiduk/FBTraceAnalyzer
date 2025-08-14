program FBTraceAnalyzer;

uses
  Forms,
  uMain in 'uMain.pas' {Form1},
  TraceLogParser in 'Functions\TraceLogParser.pas',
  FuncTrace in 'Functions\FuncTrace.pas',
  uProgressDlg in 'uProgressDlg.pas' {fProgressDlg},
  uSlowDetailDlg in 'uSlowDetailDlg.pas' {fSlowDetailDlg},
  ITraceLogParser in 'Interfaces\ITraceLogParser.pas',
  uTraceContracts in 'Functions\uTraceContracts.pas',
  uTraceCache in 'Functions\uTraceCache.pas',
  uGutterHook in 'Functions\uGutterHook.pas',
  uStatCard in 'Frames\uStatCard.pas' {StatCard: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
