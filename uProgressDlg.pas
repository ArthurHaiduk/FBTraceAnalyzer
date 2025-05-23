unit uProgressDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TfProgressDlg = class(TForm)
    pb: TProgressBar;
    pTop: TPanel;
    pMain: TPanel;
    lblMsg: TLabel;
  private
    FLastTick: DWORD;
  public
    procedure SafeUpdate(const APercent: Integer);
  end;

var
  fProgressDlg: TfProgressDlg;

implementation

{$R *.dfm}

procedure TfProgressDlg.SafeUpdate(const APercent: Integer);
begin
  if pb.Position <> APercent then
  begin
    pb.Position := APercent;
    if GetTickCount - FLastTick > 100 then
    begin
      FLastTick := GetTickCount;
      Application.ProcessMessages;
    end;
  end;
end;

end.
