program FBTraceAnalyzer;

uses
  Forms,
  uMain in 'uMain.pas' {Form1},
  TraceLogParser in 'Functions\TraceLogParser.pas',
  FuncTrace in 'Functions\FuncTrace.pas',
  uProgressDlg in 'uProgressDlg.pas' {fProgressDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
