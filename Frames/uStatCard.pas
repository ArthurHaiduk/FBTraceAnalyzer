unit uStatCard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TStatCard = class(TFrame)
  private
    pCard: TPanel;
    lbTitle: TLabel;
    lbValue: TLabel;
    lbMeta1: TLabel;
    lbMeta2: TLabel;
    lbMeta3: TLabel;
    function FormatIntSp(N: Int64): string;
    procedure Relayout;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetTitle(const S: string);
    procedure SetValueInt64(const N: Int64);
    procedure SetMeta(const S1, S2, S3: string);
  end;

implementation

{$R *.dfm}

procedure TStatCard.Relayout;
const
  GAP_AFTER_VALUE = 10;
  LINE_STEP = 14;
  PAD_BOTTOM = 18;
begin
  lbMeta1.Top := lbValue.Top + lbValue.Height + GAP_AFTER_VALUE;
  lbMeta2.Top := lbMeta1.Top + LINE_STEP;
  lbMeta3.Top := lbMeta2.Top + LINE_STEP;

  Height := lbMeta3.Top + PAD_BOTTOM;
end;

procedure TStatCard.SetMeta(const S1, S2, S3: string);
begin
  lbMeta1.Caption := S1;
  lbMeta2.Caption := S2;
  lbMeta3.Caption := S3;
  Relayout;
end;

constructor TStatCard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 240;

  pCard := TPanel.Create(Self);
  pCard.Parent := Self;
  pCard.Align := alClient;
  pCard.BevelOuter := bvNone;
  pCard.ParentBackground := False;
  pCard.Color := clWhite;

  lbTitle := TLabel.Create(pCard);
  lbTitle.Parent := pCard;
  lbTitle.Left := 16;
  lbTitle.Top := 12;
  lbTitle.Caption := 'Title';
  lbTitle.Font.Color := clGrayText;
  lbTitle.Font.Size := 9;

  lbValue := TLabel.Create(pCard);
  lbValue.Parent := pCard;
  lbValue.Left := 16;
  lbValue.Top := 34;
  lbValue.Caption := '0';
  lbValue.Font.Size := 18;
  lbValue.Font.Style := [fsBold];

  lbMeta1 := TLabel.Create(pCard);
  lbMeta1.Parent := pCard;
  lbMeta1.Left := 16;
  lbMeta1.Top := 58;
  lbMeta1.Font.Size := 8;
  lbMeta1.Font.Color := clGrayText;

  lbMeta2 := TLabel.Create(pCard);
  lbMeta2.Parent := pCard;
  lbMeta2.Left := 16;
  lbMeta2.Top := 72;
  lbMeta2.Font.Size := 8;
  lbMeta2.Font.Color := clGrayText;

  lbMeta3 := TLabel.Create(pCard);
  lbMeta3.Parent := pCard;
  lbMeta3.Left := 16;
  lbMeta3.Top := 86;
  lbMeta3.Font.Size := 8;
  lbMeta3.Font.Color := clGrayText;

  Relayout;

  Height := 104;
end;

procedure TStatCard.SetTitle(const S: string);
begin
  lbTitle.Caption := S;
end;

procedure TStatCard.SetValueInt64(const N: Int64);
begin
  lbValue.Caption := FormatIntSp(N);
  Relayout;
end;

function TStatCard.FormatIntSp(N: Int64): string;
var
  S: string;
  i, cnt: Integer;
begin
  S := IntToStr(N);
  Result := '';
  cnt := 0;
  for i := Length(S) downto 1 do
  begin
    Inc(cnt);
    Result := S[i] + Result;
    if (cnt mod 3 = 0) and (i > 1) then
      Result := ' ' + Result;
  end;
end;

end.
