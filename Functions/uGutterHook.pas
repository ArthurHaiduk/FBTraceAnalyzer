unit uGutterHook;

interface

uses
  Windows, Messages, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Math,
  SysUtils;

type
  TGutterHook = class
  private
    FMemo: TMemo;
    FPB: TPaintBox;
    FOldProc: TWndMethod;
    procedure PBPaint(Sender: TObject);
    procedure MemoWndProc(var Msg: TMessage);
    procedure DoPaint;
  public
    constructor Create(AMemo: TMemo);
    destructor Destroy; override;
    procedure Attach;
    procedure Detach;
    procedure Invalidate;
  end;

implementation

constructor TGutterHook.Create(AMemo: TMemo);
begin
  inherited Create;
  FMemo := AMemo;

  FPB := TPaintBox.Create(FMemo.Parent);
  FPB.Parent := FMemo.Parent;
  FPB.Align := alLeft;
  FPB.Width := 48;
  FPB.OnPaint := PBPaint;

  Attach;
end;

destructor TGutterHook.Destroy;
begin
  Detach;
  FPB.Free;
  inherited;
end;

procedure TGutterHook.Attach;
begin
  if Assigned(FMemo) then
  begin
    FOldProc := FMemo.WindowProc;
    FMemo.WindowProc := MemoWndProc;
  end;
end;

procedure TGutterHook.Detach;
begin
  if Assigned(FMemo) and Assigned(FOldProc) then
  begin
    FMemo.WindowProc := FOldProc;
    FOldProc := nil;
  end;
end;

procedure TGutterHook.Invalidate;
begin
  FPB.Invalidate;
end;

procedure TGutterHook.PBPaint(Sender: TObject);
begin
  DoPaint;
end;

procedure TGutterHook.MemoWndProc(var Msg: TMessage);
begin
  if Assigned(FOldProc) then
    FOldProc(Msg);

  case Msg.Msg of
    WM_VSCROLL, WM_MOUSEWHEEL, WM_KEYUP, WM_KEYDOWN, WM_SIZE, WM_LBUTTONDOWN,
      WM_LBUTTONUP, WM_CHAR, WM_SETTEXT, EM_REPLACESEL:
      FPB.Invalidate;
  end;
end;

procedure TGutterHook.DoPaint;
var
  C: TCanvas;
  tm: TTextMetric;
  lineH: Integer;
  first: Integer;
  total: Integer;
  visible: Integer;
  i, y, w: Integer;
  s: string;
  digits: Integer;
  RText: TRect;
  TopPad: Integer;
begin
  C := FPB.Canvas;

  C.Brush.Color := clBtnFace;
  C.FillRect(FPB.ClientRect);

  C.Font.Assign(FMemo.Font);
  GetTextMetrics(C.Handle, tm);
  lineH := tm.tmHeight;
  if lineH <= 0 then
    Exit;

  SendMessage(FMemo.Handle, EM_GETRECT, 0, LPARAM(@RText));
  TopPad := RText.Top;
  Inc(TopPad);
  first := FMemo.Perform(EM_GETFIRSTVISIBLELINE, 0, 0);
  total := FMemo.Lines.Count;
  visible := (FPB.ClientHeight - TopPad) div lineH + 2;

  digits := Max(1, Length(IntToStr(Max(1, total))));
  w := Max(36, C.TextWidth('0') * digits + 12);
  if FPB.Width <> w then
    FPB.Width := w;

  for i := 0 to visible - 1 do
  begin
    if first + i >= total then
      Break;
    y := TopPad + i * lineH;

    s := IntToStr(first + i + 1);
    RText := Rect(0, y, FPB.Width - 2, y + lineH);
    DrawText(C.Handle, PChar(s), Length(s), RText,
      DT_RIGHT or DT_TOP or DT_SINGLELINE);
  end;

  C.Pen.Color := clSilver;
  C.MoveTo(FPB.Width - 1, 0);
  C.LineTo(FPB.Width - 1, FPB.Height);
end;

end.
