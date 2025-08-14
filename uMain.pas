unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus, TraceLogParser, ComCtrls,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  dxSkinsCore, dxSkinsDefaultPainters, cxStyles, cxEdit,
  cxInplaceContainer, cxVGrid,
  Generics.Collections, dxSkinscxPCPainter, cxCustomData, cxFilter, cxData,
  cxDataStorage, DB, cxDBData, cxGridLevel, cxClasses, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, DBClient,
  ITraceLogParser, uTraceContracts, uTraceCache, IniFiles, ShlObj, StrUtils,
  cxMemo, cxTextEdit, dxSkinsdxRibbonPainter, dxRibbon,
  dxRibbonSkins, dxSkinBlack, dxSkinsForm, acPNG, DwmApi, Types, uStatCard,
  cxCheckBox;

type
  TMargins = record
    cxLeftWidth: Integer;
    cxRightWidth: Integer;
    cyTopHeight: Integer;
    cyBottomHeight: Integer;
  end;

  TForm1 = class(TForm)
    pMain: TPanel;
    lbSt1RecordCount: TLabel;
    edSt1RecordCount: TEdit;
    edSt1CommitCount: TEdit;
    lbSt1CommitCount: TLabel;
    edSt1ErrorCount: TEdit;
    lbSt1ErrorCount: TLabel;
    edSt1RollbackCount: TEdit;
    lbSt1RollbackCount: TLabel;
    gbStats: TGroupBox;
    cxVerticalGrid1: TcxVerticalGrid;
    cxVerticalGrid2: TcxVerticalGrid;
    tvSlow: TcxGridDBTableView;
    cxGridSlowLevel1: TcxGridLevel;
    cxGridSlow: TcxGrid;
    Rank: TcxGridDBColumn;
    TimeMs: TcxGridDBColumn;
    SQLType: TcxGridDBColumn;
    Rows: TcxGridDBColumn;
    PlanNat: TcxGridDBColumn;
    SQLText: TcxGridDBColumn;
    Plan: TcxGridDBColumn;
    Params: TcxGridDBColumn;
    Perf: TcxGridDBColumn;
    odMain: TOpenDialog;
    sbStatus: TStatusBar;
    pHeader: TPanel;
    Image1: TImage;
    pHeaderBar: TPanel;
    lbFile: TLabel;
    lbApp: TLabel;
    lbOpen: TLabel;
    lbExit: TLabel;
    PopupMenuFile: TPopupMenu;
    miOpen: TMenuItem;
    pCards: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tvSlowDblClick(Sender: TObject);
    procedure cxVerticalGrid1DblClick(Sender: TObject);
    procedure bClearFiltersClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
  private
    FFileName: string;
    FTraceLogParser: TTraceLogParser;
    FSlowData: TClientDataSet;
    FSlowDS: TDataSource;
    FLoadedFromCache: Boolean;
    FLastOpMS: Cardinal;
    FCacheFmt, FCacheApi: Cardinal;
    FPerfShort: TStringField;
    FParamShort: TStringField;
    FCardSelect, FCardInsert, FCardUpdate, FCardDelete: TStatCard;
    pOuter, spTopMargin, spBottomMargin, spLeftMargin, spRightMargin,
      spCardsGap, pBody, pLeft, pGutter, pCenter, pnlIdx, spIdxGap,
      pnlErr, pSearch: TPanel;
    edSearch: TEdit;
    bClear: TButton;
    chLive: TCheckBox;
    tmrLive: TTimer;
    FStyleRepo: TcxStyleRepository;
    stHeader, stOdd, stEven, stBg, stCategory: TcxStyle;
    FNormalBounds: TRect;
    FHasNormalBounds: Boolean;
    ColRowNo, ColExecAt, ColApp, ColClient: TcxGridDBColumn;
    procedure ExecAtGetDisplayText(Sender: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord; var AText: string);
    procedure ApplyModernLook;
    procedure BuildSinglePageLayout;
    procedure CreateSlowSearchUI;
    procedure ApplySearchFilter(Sender: TObject);
    procedure ClearSearch(Sender: TObject);
    procedure CreateCards;
    procedure LayoutCards;
    procedure CDSCalcFields(DataSet: TDataSet);
    procedure SetupSlowGrid;
    procedure EnsureGridColumnsOrder;
    procedure BestFitSlowGrid;
    function GetSettingsFile: string;
    procedure LoadLastFile;
    procedure SaveLastFile;
    procedure InitSlowDataset;
    procedure FillSlowGrid;
    procedure ShowStatistics;
    procedure TimeMsGetDisplayText(Sender: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord; var AText: string);
    procedure RowNoGetDisplayText(Sender: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord; var AText: string);
    procedure EnsureStatusBar;
    procedure SetStatusIdle;
    procedure SetStatusResult;
    procedure TruncTextGetDisplayText(Sender: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord; var AText: string);
    procedure PerfFieldGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure ParamFieldGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure WMNCHitTest(var Msg: TWMNCHitMessage); message WM_NCHITTEST;
    procedure StartDrag(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetupLink(L: TLabel; const ACaption: string; ALeft: Integer);
    procedure LinkMouseEnter(Sender: TObject);
    procedure LinkMouseLeave(Sender: TObject);
    procedure LinkMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LinkFileClick(Sender: TObject);
    procedure LinkOpenClick(Sender: TObject);
    procedure LinkExitClick(Sender: TObject);
    procedure ClearCaptionOnPanels;
    procedure EnableRoundedCorners(Small: Boolean = True);
    procedure RoundControl(AControl: TWinControl; Radius: Integer);
    procedure LiveToggle(Sender: TObject);
    procedure LiveTick(Sender: TObject);
  end;

function DwmExtendFrameIntoClientArea(hWnd: hWnd;
  const pMarInset: TMargins): HRESULT; stdcall; external 'dwmapi.dll';
function DwmSetWindowAttribute(hWnd: hWnd; dwAttribute: DWORD;
  pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall;
external 'dwmapi.dll' name 'DwmSetWindowAttribute';

var
  Form1: TForm1;

implementation

uses
  FuncTrace, uProgressDlg, uSlowDetailDlg;
{$R *.dfm}

const
  Grip = 8;
  M_OUT = 12;
  M_GUTTER = 12;
  M_GAP = 8;

const
  DWMWA_WINDOW_CORNER_PREFERENCE = 33;
  DWMWCP_DEFAULT = 0;
  DWMWCP_DONOTROUND = 1;
  DWMWCP_ROUND = 2;
  DWMWCP_ROUNDSMALL = 3;

procedure TForm1.ExecAtGetDisplayText(Sender: TcxCustomGridTableItem;
  ARecord: TcxCustomGridRecord; var AText: string);
var
  V: Variant;
  DT: TDateTime;
begin
  V := ARecord.Values[Sender.Index];
  if VarIsNull(V) or VarIsEmpty(V) then
  begin
    AText := '';
    Exit;
  end;
  DT := VarToDateTime(V);
  AText := FormatDateTime('dd.mm.yyyy hh:nn:ss', DT);
end;

procedure TForm1.EnableRoundedCorners(Small: Boolean);
var
  pref: Cardinal;
begin
  if Small then
    pref := DWMWCP_ROUNDSMALL
  else
    pref := DWMWCP_ROUND;
  DwmSetWindowAttribute(Handle, DWMWA_WINDOW_CORNER_PREFERENCE, @pref,
    SizeOf(pref));
end;

procedure TForm1.RoundControl(AControl: TWinControl; Radius: Integer);
var
  rgn: HRGN;
begin
  if (AControl = nil) or (AControl.Handle = 0) then
    Exit;
  rgn := CreateRoundRectRgn(0, 0, AControl.Width, AControl.Height, Radius,
    Radius);
  SetWindowRgn(AControl.Handle, rgn, True);
end;

procedure TForm1.ApplyModernLook;
begin
  if Assigned(pLeft) then
  begin
    pLeft.ParentBackground := False;
    pLeft.Color := $F6F7F9;
  end;
  if Assigned(pGutter) then
  begin
    pGutter.ParentBackground := False;
    pGutter.Color := $F6F7F9;
  end;
  if Assigned(pCenter) then
    pCenter.BevelOuter := bvNone;

  FStyleRepo := TcxStyleRepository.Create(Self);

  stBg := TcxStyle.Create(FStyleRepo);
  stBg.Color := clWhite;

  stHeader := TcxStyle.Create(FStyleRepo);
  stHeader.Color := $F3F4F6;
  stHeader.TextColor := $202124;
  stHeader.Font.Style := [fsBold];

  stOdd := TcxStyle.Create(FStyleRepo);
  stOdd.Color := clWhite;

  stEven := TcxStyle.Create(FStyleRepo);
  stEven.Color := $FAFAFA;

  stCategory := TcxStyle.Create(FStyleRepo);
  stCategory.Color := $ECEFF1;
  stCategory.TextColor := $202124;
  stCategory.Font.Style := [fsBold];

  tvSlow.OptionsView.HeaderAutoHeight := True;
  tvSlow.OptionsView.DataRowHeight := 22;
  tvSlow.OptionsView.GridLines := glHorizontal;
  tvSlow.OptionsView.Indicator := True;

  tvSlow.Styles.Background := stBg;
  tvSlow.Styles.ContentOdd := stOdd;
  tvSlow.Styles.ContentEven := stEven;
  tvSlow.Styles.Header := stHeader;
  tvSlow.Styles.Indicator := stHeader;
  tvSlow.Styles.Footer := stHeader;

  Rank.HeaderAlignmentHorz := taCenter;
  PlanNat.HeaderAlignmentHorz := taCenter;

  PlanNat.PropertiesClassName := 'TcxCheckBoxProperties';
  if PlanNat.Properties is TcxCheckBoxProperties then
    with TcxCheckBoxProperties(PlanNat.Properties) do
    begin
      ValueChecked := True;
      ValueUnchecked := False;
      ImmediatePost := False;
      ReadOnly := True;
    end;

  cxVerticalGrid1.OptionsView.RowHeaderWidth := 180;
  cxVerticalGrid2.OptionsView.RowHeaderWidth := 180;

  cxVerticalGrid1.Styles.Background := stBg;
  cxVerticalGrid1.Styles.Content := stBg;
  cxVerticalGrid1.Styles.Category := stCategory;
  cxVerticalGrid1.Styles.Header := stHeader;
  cxVerticalGrid1.Styles.Inactive := stBg;

  cxVerticalGrid2.Styles.Assign(cxVerticalGrid1.Styles);
end;

procedure TForm1.BuildSinglePageLayout;
var
  inner: TPanel;
begin
  if pOuter = nil then
  begin
    pOuter := TPanel.Create(Self);
    pOuter.Parent := pMain;
    pOuter.Align := alClient;
    pOuter.BevelOuter := bvNone;

    spTopMargin := TPanel.Create(pOuter);
    spTopMargin.Parent := pOuter;
    spTopMargin.Align := alTop;
    spTopMargin.Height := M_OUT;
    spTopMargin.BevelOuter := bvNone;

    spLeftMargin := TPanel.Create(pOuter);
    spLeftMargin.Parent := pOuter;
    spLeftMargin.Align := alLeft;
    spLeftMargin.Width := M_OUT;
    spLeftMargin.BevelOuter := bvNone;

    spRightMargin := TPanel.Create(pOuter);
    spRightMargin.Parent := pOuter;
    spRightMargin.Align := alRight;
    spRightMargin.Width := M_OUT;
    spRightMargin.BevelOuter := bvNone;

    spBottomMargin := TPanel.Create(pOuter);
    spBottomMargin.Parent := pOuter;
    spBottomMargin.Align := alBottom;
    spBottomMargin.Height := M_OUT;
    spBottomMargin.BevelOuter := bvNone;
  end;

  if pCards = nil then
    CreateCards
  else
    pCards.Parent := pOuter;
  pCards.Align := alTop;

  if spCardsGap = nil then
  begin
    spCardsGap := TPanel.Create(pOuter);
    spCardsGap.Parent := pOuter;
    spCardsGap.Align := alTop;
    spCardsGap.Height := M_GAP;
    spCardsGap.BevelOuter := bvNone;
  end;

  if pBody = nil then
  begin
    pBody := TPanel.Create(Self);
    pBody.Parent := pOuter;
    pBody.Align := alClient;
    pBody.BevelOuter := bvNone;
  end;

  if pLeft = nil then
  begin
    pLeft := TPanel.Create(Self);
    pLeft.Parent := pBody;
    pLeft.Align := alLeft;
    pLeft.Width := 300;
    pLeft.BevelOuter := bvNone;
  end;

  if pGutter = nil then
  begin
    pGutter := TPanel.Create(Self);
    pGutter.Parent := pBody;
    pGutter.Align := alLeft;
    pGutter.Width := M_GUTTER;
    pGutter.BevelOuter := bvNone;
  end;

  if pCenter = nil then
  begin
    pCenter := TPanel.Create(Self);
    pCenter.Parent := pBody;
    pCenter.Align := alClient;
    pCenter.BevelOuter := bvNone;
  end;

  if pnlIdx = nil then
  begin
    pnlIdx := TPanel.Create(Self);
    pnlIdx.Parent := pLeft;
    pnlIdx.Align := alTop;
    pnlIdx.Height := 240;
    pnlIdx.BevelOuter := bvNone;

    spIdxGap := TPanel.Create(pLeft);
    spIdxGap.Parent := pLeft;
    spIdxGap.Align := alTop;
    spIdxGap.Height := M_GAP;
    spIdxGap.BevelOuter := bvNone;
  end;

  if pnlErr = nil then
  begin
    pnlErr := TPanel.Create(Self);
    pnlErr.Parent := pLeft;
    pnlErr.Align := alClient;
    pnlErr.BevelOuter := bvNone;
  end;

  inner := TPanel.Create(pnlIdx);
  inner.Parent := pnlIdx;
  inner.Align := alClient;
  inner.BevelOuter := bvNone;

  with TPanel.Create(inner) do
  begin
    Parent := inner;
    Align := alTop;
    Height := 4;
    BevelOuter := bvNone;
  end;
  with TPanel.Create(inner) do
  begin
    Parent := inner;
    Align := alLeft;
    Width := 4;
    BevelOuter := bvNone;
  end;
  with TPanel.Create(inner) do
  begin
    Parent := inner;
    Align := alRight;
    Width := 4;
    BevelOuter := bvNone;
  end;
  with TPanel.Create(inner) do
  begin
    Parent := inner;
    Align := alBottom;
    Height := 4;
    BevelOuter := bvNone;
  end;

  cxVerticalGrid1.Parent := inner;
  cxVerticalGrid1.Align := alClient;

  inner := TPanel.Create(pnlErr);
  inner.Parent := pnlErr;
  inner.Align := alClient;
  inner.BevelOuter := bvNone;

  with TPanel.Create(inner) do
  begin
    Parent := inner;
    Align := alTop;
    Height := 4;
    BevelOuter := bvNone;
  end;
  with TPanel.Create(inner) do
  begin
    Parent := inner;
    Align := alLeft;
    Width := 4;
    BevelOuter := bvNone;
  end;
  with TPanel.Create(inner) do
  begin
    Parent := inner;
    Align := alRight;
    Width := 4;
    BevelOuter := bvNone;
  end;
  with TPanel.Create(inner) do
  begin
    Parent := inner;
    Align := alBottom;
    Height := 4;
    BevelOuter := bvNone;
  end;

  cxVerticalGrid2.Parent := inner;
  cxVerticalGrid2.Align := alClient;

  cxGridSlow.Parent := pCenter;
  cxGridSlow.Align := alClient;

  gbStats.Visible := False;

  CreateSlowSearchUI;
end;

procedure TForm1.CreateSlowSearchUI;
var
  inner, padL, padR: TPanel;
begin
  if pSearch <> nil then
    Exit;

  pSearch := TPanel.Create(Self);
  pSearch.Parent := pCenter;
  pSearch.Align := alTop;
  pSearch.Height := 36;
  pSearch.BevelOuter := bvNone;

  inner := TPanel.Create(pSearch);
  inner.Parent := pSearch;
  inner.Align := alClient;
  inner.BevelOuter := bvNone;

  padL := TPanel.Create(inner);
  padL.Parent := inner;
  padL.Align := alLeft;
  padL.Width := 8;
  padL.BevelOuter := bvNone;

  padR := TPanel.Create(inner);
  padR.Parent := inner;
  padR.Align := alRight;
  padR.Width := 8;
  padR.BevelOuter := bvNone;

  edSearch := TEdit.Create(inner);
  edSearch.Parent := inner;
  edSearch.Align := alClient;
  edSearch.TextHint := 'Пошук у SQL / PLAN / Params / Perf';
  edSearch.OnChange := ApplySearchFilter;

  chLive := TCheckBox.Create(inner);
  chLive.Parent := inner;
  chLive.Align := alRight;
  chLive.Caption := 'Live';
  chLive.Width := 80;
  chLive.OnClick := LiveToggle;

  bClear := TButton.Create(inner);
  bClear.Parent := inner;
  bClear.Align := alRight;
  bClear.Width := 96;
  bClear.Caption := 'Очистити';
  bClear.OnClick := ClearSearch;
end;

procedure TForm1.ApplySearchFilter(Sender: TObject);
var
  S: string;
begin
  S := Trim(edSearch.Text);
  FSlowData.Filtered := False;
  if S = '' then
    Exit;

  S := StringReplace(S, '''', '''''', [rfReplaceAll]);
  FSlowData.Filter := Format(
    '(SQLText LIKE ''%%%0:s%%'') OR (PlanLine LIKE ''%%%0:s%%'') OR ' +
      '(ParamShort LIKE ''%%%0:s%%'') OR (PerfShort LIKE ''%%%0:s%%'') ' +
      'OR (AppExe LIKE ''%%%0:s%%'') OR (Client LIKE ''%%%0:s%%'')', [S]);
  FSlowData.Filtered := True;
end;

procedure TForm1.ClearSearch(Sender: TObject);
begin
  edSearch.Text := '';
  FSlowData.Filtered := False;
  if FSlowData.RecordCount > 0 then
  begin
    FSlowData.First;
    tvSlow.DataController.GotoFirst;
    tvSlow.Controller.TopRowIndex := 0;
  end;
end;

procedure TForm1.ClearCaptionOnPanels;
begin
  if Assigned(pCards) then
    pCards.Caption := '';
end;

procedure TForm1.CreateCards;
const
  H = 96;
begin
  if pCards = nil then
  begin
    pCards := TPanel.Create(Self);
    pCards.Parent := pOuter;
    pCards.Align := alTop;
    pCards.Height := H;
    pCards.BevelOuter := bvNone;
  end;

  FCardSelect := TStatCard.Create(nil);
  FCardSelect.Parent := pCards;
  FCardSelect.Name := 'CardSelect';
  FCardSelect.SetTitle('Вибірки');
  FCardInsert := TStatCard.Create(nil);
  FCardInsert.Parent := pCards;
  FCardInsert.Name := 'CardInsert';
  FCardInsert.SetTitle('Вставки');
  FCardUpdate := TStatCard.Create(nil);
  FCardUpdate.Parent := pCards;
  FCardUpdate.Name := 'CardUpdate';
  FCardUpdate.SetTitle('Оновлення');
  FCardDelete := TStatCard.Create(nil);
  FCardDelete.Parent := pCards;
  FCardDelete.Name := 'CardDelete';
  FCardDelete.SetTitle('Видалення');

  LayoutCards;
end;

procedure TForm1.LayoutCards;
const
  PAD_TOP = 8;
  PAD_BOTTOM = 8;
  G = 16;
  M = 16;
var
  W, L, T: Integer;
begin
  if (pCards = nil) or (FCardSelect = nil) or (FCardInsert = nil) or
    (FCardUpdate = nil) or (FCardDelete = nil) then
    Exit;

  W := (pCards.ClientWidth - M - M - G * 3) div 4;
  if W < 160 then
    W := 160;

  T := PAD_TOP;
  L := M;

  FCardSelect.SetBounds(L, T, W, FCardSelect.Height);
  Inc(L, W + G);
  FCardInsert.SetBounds(L, T, W, FCardInsert.Height);
  Inc(L, W + G);
  FCardUpdate.SetBounds(L, T, W, FCardUpdate.Height);
  Inc(L, W + G);
  FCardDelete.SetBounds(L, T, W, FCardDelete.Height);

  pCards.Height := FCardSelect.Height + PAD_TOP + PAD_BOTTOM;
end;

procedure TForm1.LinkMouseEnter(Sender: TObject);
begin (Sender as TLabel)
  .Font.Style := [fsUnderline];
end;

procedure TForm1.LinkMouseLeave(Sender: TObject);
begin (Sender as TLabel)
  .Font.Style := [];
end;

procedure TForm1.LinkMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  StartDrag(Sender, Button, Shift, X, Y);
end;

procedure TForm1.LinkFileClick(Sender: TObject);
begin
  PopupMenuFile.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TForm1.LinkOpenClick(Sender: TObject);
var
  dlg: TfProgressDlg;
  CacheFile: string;
  t0: Cardinal;
  Snap: ITraceSnapshot;
  SnapOk: Boolean;
begin
  if Trim(FFileName) = '' then
  begin
    MessageDlg('Файл не відкритий!', mtError, [mbOk], 0);
    Exit;
  end;
  EnsureStatusBar;
  CacheFile := ChangeFileExt(FFileName, '.fbta');
  Snap := nil;
  SnapOk := LoadSnapshotFromFile(CacheFile, Snap, FCacheFmt, FCacheApi);
  if SnapOk and (Snap <> nil) then
  begin
    sbStatus.Panels[0].Text := 'Завантаження з кешу…';
    Application.ProcessMessages;
    t0 := GetTickCount;
    FTraceLogParser.ApplySnapshot(Snap);
    FLastOpMS := GetTickCount - t0;
    FLoadedFromCache := True;
    ShowStatistics;
    SetStatusResult;
    Exit;
  end;
  FLoadedFromCache := False;
  sbStatus.Panels[0].Text := 'Парсинг…';
  Application.ProcessMessages;
  Screen.Cursor := crHourGlass;
  dlg := TfProgressDlg.Create(Self);
  try
    dlg.lblMsg.Caption := 'Обробка файлу…';
    dlg.pb.Max := 100;
    dlg.pb.Position := 0;
    dlg.Show;
    Application.ProcessMessages;
    t0 := GetTickCount;
    FTraceLogParser.ParseLog(FFileName, TProc<Int64,
      Int64>( procedure(const APos, ATotal: Int64)begin dlg.SafeUpdate
          (APos); end));
    FLastOpMS := GetTickCount - t0;
  finally
    dlg.Free;
    Screen.Cursor := crDefault;
  end;
  try
    Snap := FTraceLogParser.MakeSnapshot(FFileName);
    SaveSnapshotToFile(CacheFile, Snap);
  except
  end;
  ShowStatistics;
  SetStatusResult;
end;

procedure TForm1.LinkExitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.WMNCHitTest(var Msg: TWMNCHitMessage);
var
  pt: TPoint;
begin
  pt := ScreenToClient(Point(Msg.XCursor, Msg.YCursor));

  if pt.X < Grip then
    if pt.Y < Grip then
      Msg.Result := HTTOPLEFT
    else if pt.Y > ClientHeight - Grip then
      Msg.Result := HTBOTTOMLEFT
    else
      Msg.Result := HTLEFT
    else if pt.X > ClientWidth - Grip then
      if pt.Y < Grip then
        Msg.Result := HTTOPRIGHT
      else if pt.Y > ClientHeight - Grip then
        Msg.Result := HTBOTTOMRIGHT
      else
        Msg.Result := HTRIGHT
      else if pt.Y < Grip then
        Msg.Result := HTTOP
      else if pt.Y > ClientHeight - Grip then
        Msg.Result := HTBOTTOM
      else if pt.Y < pHeader.Height then
        Msg.Result := HTCAPTION
      else
        Msg.Result := HTCLIENT;
end;

function OneLineEllipsis(const S: string; MaxLen: Integer = 160): string;
var
  T: string;
begin
  T := StringReplace(S, sLineBreak, ' ', [rfReplaceAll]);
  T := StringReplace(T, #13, ' ', [rfReplaceAll]);
  T := StringReplace(T, #10, ' ', [rfReplaceAll]);
  T := Trim(T);
  if MaxLen < 1 then
    MaxLen := 1;
  if Length(T) > MaxLen then
    Result := Copy(T, 1, MaxLen - 1) + '…'
  else
    Result := T;
end;

function IsOnlySeparators(const S: string): Boolean;
var
  i: Integer;
  ch: Char;
begin
  for i := 1 to Length(S) do
  begin
    ch := S[i];
    if not(ch in [#13, #10, ' ', #9, '-', '_', '=', '*', '.', '—']) then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := Length(S) > 0;
end;

procedure TForm1.PerfFieldGetText(Sender: TField; var Text: string;
  DisplayText: Boolean);
var
  S: string;
begin
  S := Sender.AsString;
  if IsOnlySeparators(Trim(S)) then
    Text := ''
  else
    Text := OneLineEllipsis(S, 160);
end;

procedure TForm1.ParamFieldGetText(Sender: TField; var Text: string;
  DisplayText: Boolean);
var
  S: string;
begin
  S := Sender.AsString;
  if IsOnlySeparators(Trim(S)) then
    Text := ''
  else
    Text := OneLineEllipsis(S, 160);
end;

procedure TForm1.CDSCalcFields(DataSet: TDataSet);
var
  S: string;
begin
  S := FSlowData.FieldByName('PerfText').AsString;
  if IsOnlySeparators(Trim(S)) then
    FPerfShort.AsString := ''
  else
    FPerfShort.AsString := OneLineEllipsis(S, 160);

  S := FSlowData.FieldByName('ParamText').AsString;
  if IsOnlySeparators(Trim(S)) then
    FParamShort.AsString := ''
  else
    FParamShort.AsString := OneLineEllipsis(S, 160);
end;

function GetFocusedRowCaption(VG: TcxVerticalGrid): string;
var
  R: TcxCustomRow;
begin
  Result := '';
  R := VG.FocusedRow;
  if R = nil then
    Exit;

  if R is TcxEditorRow then
    Result := TcxEditorRow(R).Properties.Caption
  else if R is TcxCategoryRow then
    Result := TcxCategoryRow(R).Properties.Caption;
end;

procedure TForm1.cxVerticalGrid1DblClick(Sender: TObject);
var
  Key: string;
begin
  Key := GetFocusedRowCaption(cxVerticalGrid1);
  if Key = '' then
    Exit;

  FSlowData.Filtered := False;
  FSlowData.Filter := Format('PlanLine LIKE ''%%%s%%''',
    [StringReplace(Key, '''', '''''', [rfReplaceAll])]);
  FSlowData.Filtered := True;

  if FSlowData.RecordCount > 0 then
  begin
    FSlowData.First;
    tvSlow.DataController.GotoFirst;
    tvSlow.Controller.TopRowIndex := 0;
  end;

  sbStatus.Panels[0].Text := 'Фільтр за індексом: ' + Key;
  cxGridSlow.SetFocus;
  tvSlow.Controller.TopRowIndex := 0;
end;

procedure TForm1.TruncTextGetDisplayText(Sender: TcxCustomGridTableItem;
  ARecord: TcxCustomGridRecord; var AText: string);
begin
  AText := OneLineEllipsis(AText, 160);
end;

procedure TForm1.SetupSlowGrid;
begin
  tvSlow.OptionsData.Editing := False;
  tvSlow.OptionsData.Inserting := False;
  tvSlow.OptionsData.Deleting := False;
  tvSlow.OptionsSelection.CellSelect := False;
  tvSlow.OptionsBehavior.AlwaysShowEditor := False;
  tvSlow.OptionsBehavior.ImmediateEditor := False;

  tvSlow.OptionsView.ColumnAutoWidth := True;
  tvSlow.OptionsView.CellAutoHeight := False;
  tvSlow.OptionsView.GroupByBox := False;
  tvSlow.OptionsBehavior.IncSearch := True;

  Rank.Width := 60;
  TimeMs.Width := 90;
  SQLType.Width := 90;
  Rows.Width := 70;
  PlanNat.Width := 50;
  SQLText.Width := 600;
  Plan.Width := 380;
  Params.Width := 260;
  Perf.Width := 280;

  SQLText.PropertiesClassName := 'TcxTextEditProperties';
  Plan.PropertiesClassName := 'TcxTextEditProperties';
  Params.PropertiesClassName := 'TcxTextEditProperties';
  Perf.PropertiesClassName := 'TcxTextEditProperties';

  if ColRowNo = nil then
  begin
    ColRowNo := tvSlow.CreateColumn as TcxGridDBColumn;
    ColRowNo.Caption := '№';
    ColRowNo.HeaderAlignmentHorz := taCenter;
    ColRowNo.Options.Editing := False;
    ColRowNo.Options.Sorting := False;
    ColRowNo.Options.Filtering := False;
    ColRowNo.Width := 48;
    ColRowNo.OnGetDisplayText := RowNoGetDisplayText;
    ColRowNo.Index := 0;
  end;

  if ColExecAt = nil then
  begin
    ColExecAt := tvSlow.CreateColumn as TcxGridDBColumn;
    ColExecAt.Caption := 'ExecTime';
    ColExecAt.DataBinding.FieldName := 'ExecAt';
    ColExecAt.HeaderAlignmentHorz := taCenter;
    ColExecAt.MinWidth := 120;
    ColExecAt.OnGetDisplayText := ExecAtGetDisplayText;
  end;

  if ColApp = nil then
  begin
    ColApp := tvSlow.CreateColumn as TcxGridDBColumn;
    ColApp.Caption := 'App';
    ColApp.DataBinding.FieldName := 'AppExe';
  end;

  if ColClient = nil then
  begin
    ColClient := tvSlow.CreateColumn as TcxGridDBColumn;
    ColClient.Caption := 'Client';
    ColClient.DataBinding.FieldName := 'Client';
  end;

  EnsureGridColumnsOrder;
end;

procedure TForm1.EnsureGridColumnsOrder;
begin
  if Assigned(ColRowNo) then
    ColRowNo.Index := 0;
  Rank.Index := 1;
  if Assigned(ColExecAt) then
    ColExecAt.Index := 2;
  SQLType.Index := 3;
  TimeMs.Index := 4;
  Rows.Index := 5;
  PlanNat.Index := 6;
  SQLText.Index := 7;
  Plan.Index := 8;
  Params.Index := 9;
  Perf.Index := 10;
  if Assigned(ColApp) then
    ColApp.Index := 11;
  if Assigned(ColClient) then
    ColClient.Index := 12;
end;

procedure TForm1.bClearFiltersClick(Sender: TObject);
begin
  FSlowData.Filtered := False;
  sbStatus.Panels[0].Text := 'Фільтр вимкнено';
end;

procedure TForm1.BestFitSlowGrid;
var
  i: Integer;
begin
  tvSlow.BeginUpdate;
  try
    for i := 0 to tvSlow.ColumnCount - 1 do
      tvSlow.Columns[i].ApplyBestFit;
  finally
    tvSlow.EndUpdate;
  end;
end;

function TForm1.GetSettingsFile: string;
var
  P: array [0 .. MAX_PATH] of Char;
  Dir: string;
begin
  if SHGetFolderPath(0, CSIDL_APPDATA, 0, 0, @P[0]) = S_OK then
    Dir := IncludeTrailingPathDelimiter(P) + 'FBTraceAnalyzer\'
  else
    Dir := ExtractFilePath(Application.ExeName);
  ForceDirectories(Dir);
  Result := Dir + 'settings.ini';
end;

procedure TForm1.LoadLastFile;
var
  Ini: TIniFile;
  S: string;
begin
  Ini := TIniFile.Create(GetSettingsFile);
  try
    S := Trim(Ini.ReadString('General', 'LastFile', ''));
    if (S <> '') and FileExists(S) then
    begin
      FFileName := S;
      odMain.InitialDir := ExtractFilePath(S);
    end;
  finally
    Ini.Free;
  end;
end;

procedure TForm1.miOpenClick(Sender: TObject);
begin
  if odMain.Execute(Handle) then
    FFileName := odMain.FileName;
  SaveLastFile;
end;

procedure TForm1.SaveLastFile;
var
  Ini: TIniFile;
begin
  if Trim(FFileName) = '' then
    Exit;
  Ini := TIniFile.Create(GetSettingsFile);
  try
    Ini.WriteString('General', 'LastFile', FFileName);
  finally
    Ini.Free;
  end;
end;

procedure TForm1.EnsureStatusBar;
var
  i: Integer;
begin
  if sbStatus = nil then
    for i := 0 to ComponentCount - 1 do
      if Components[i] is TStatusBar then
      begin
        sbStatus := TStatusBar(Components[i]);
        Break;
      end;

  if sbStatus = nil then
  begin
    sbStatus := TStatusBar.Create(Self);
    sbStatus.Parent := Self;
    sbStatus.Align := alBottom;
  end;

  sbStatus.SimplePanel := False;
  while sbStatus.Panels.Count < 3 do
    sbStatus.Panels.Add;

  sbStatus.Panels[0].Width := 250;
  sbStatus.Panels[1].Width := 250;
  sbStatus.Panels[2].Width := 250;
end;

procedure TForm1.SetStatusIdle;
begin
  EnsureStatusBar;
  sbStatus.Panels[0].Text := 'Готово';
  sbStatus.Panels[1].Text := '';
  sbStatus.Panels[2].Text := '';
end;

procedure TForm1.SetStatusResult;
var
  src: string;
begin
  EnsureStatusBar;
  if FLoadedFromCache then
    src := 'Кеш'
  else
    src := 'Парсер';
  sbStatus.Panels[0].Text := 'Джерело: ' + src;
  sbStatus.Panels[1].Text := Format('Час: %d мс', [FLastOpMS]);
  if FLoadedFromCache then
    sbStatus.Panels[2].Text := Format('fmt=%d api=%d', [FCacheFmt, FCacheApi])
  else
    sbStatus.Panels[2].Text := '';
end;

procedure TForm1.StartDrag(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

  procedure ToggleMaxRestore;
  begin
    if Form1.WindowState = wsMaximized then
    begin
      Form1.WindowState := wsNormal;
      if Form1.FHasNormalBounds then
        Form1.BoundsRect := Form1.FNormalBounds;
    end
    else
    begin
      if Form1.WindowState = wsNormal then
      begin
        Form1.FNormalBounds := Form1.BoundsRect;
        Form1.FHasNormalBounds := True;
      end;
      Form1.WindowState := wsMaximized;
    end;
  end;

begin
  if (Button = mbLeft) and (ssDouble in Shift) then
  begin
    ToggleMaxRestore;
    Exit;
  end;

  if Button = mbLeft then
  begin
    ReleaseCapture;
    SendMessage(Form1.Handle, WM_NCLBUTTONDOWN, HTCAPTION, 0);
  end;
end;

function TopBorderHeight: Integer;
begin
  Result := GetSystemMetrics(SM_CYFRAME) + GetSystemMetrics(SM_CXPADDEDBORDER);
end;

procedure TForm1.SetupLink(L: TLabel; const ACaption: string; ALeft: Integer);
begin
  L.Parent := pHeaderBar;
  L.Caption := ACaption;
  L.Left := ALeft;
  L.Top := 14;
  L.Font.Color := clWhite;
  L.Font.Size := 10;
  L.Cursor := crHandPoint;
  L.Transparent := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  S: LongInt;
begin
  FTraceLogParser := TTraceLogParser.Create;
  EnsureStatusBar;
  SetStatusIdle;

  InitSlowDataset;
  LoadLastFile;

  sbStatus.Panels[0].Text := IfThen(FFileName <> '', 'Файл: ' + FFileName,
    'Файл не вибрано');

  tvSlow.OptionsCustomize.GroupBySorting := False;
  tvSlow.OptionsView.GroupByBox := False;

  S := GetWindowLong(Handle, GWL_STYLE);
  S := (S or WS_SIZEBOX) and not WS_CAPTION;
  SetWindowLong(Handle, GWL_STYLE, S);
  SetWindowPos(Handle, 0, 0, 0, 0, 0,
    SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED or SWP_NOZORDER);
  EnableRoundedCorners(True);

  pHeader.Top := 0;
  pHeader.Height := 52;

  pHeader.OnMouseDown := StartDrag;
  pHeaderBar.OnMouseDown := StartDrag;
  Image1.OnMouseDown := StartDrag;
  lbApp.OnMouseDown := StartDrag;

  SetupLink(lbFile, 'File', pHeaderBar.Width - 240);
  SetupLink(lbOpen, 'Open Log', lbFile.Left + lbFile.Width + 24);
  SetupLink(lbExit, 'Exit', lbOpen.Left + lbOpen.Width + 24);

  lbFile.OnClick := LinkFileClick;
  lbOpen.OnClick := LinkOpenClick;
  lbExit.OnClick := LinkExitClick;

  ClearCaptionOnPanels;

  CreateCards;
  BuildSinglePageLayout;
  ApplyModernLook;

  tmrLive := TTimer.Create(Self);
  tmrLive.Interval := 500;
  tmrLive.Enabled := False;
  tmrLive.OnTimer := LiveTick;

  Self.DoubleBuffered := True;
  Self.WindowState := wsMaximized;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCardDelete);
  FreeAndNil(FCardUpdate);
  FreeAndNil(FCardInsert);
  FreeAndNil(FCardSelect);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  LayoutCards;
  if WindowState = wsNormal then
  begin
    FNormalBounds := BoundsRect;
    FHasNormalBounds := True;
  end;

  RoundControl(pCards, 10);
  RoundControl(pSearch, 8);
  RoundControl(pLeft, 10);
end;

procedure TForm1.InitSlowDataset;
begin
  FSlowData := TClientDataSet.Create(Self);

  with TIntegerField.Create(FSlowData) do
  begin
    FieldName := 'Rank';
    DataSet := FSlowData;
  end;
  with TFloatField.Create(FSlowData) do
  begin
    FieldName := 'TimeMs';
    DataSet := FSlowData;
  end;
  with TStringField.Create(FSlowData) do
  begin
    FieldName := 'SQLType';
    Size := 10;
    DataSet := FSlowData;
  end;
  with TLargeintField.Create(FSlowData) do
  begin
    FieldName := 'Rows';
    DataSet := FSlowData;
  end;
  with TBooleanField.Create(FSlowData) do
  begin
    FieldName := 'PlanNat';
    DataSet := FSlowData;
  end;

  with TWideMemoField.Create(FSlowData) do
  begin
    FieldName := 'SQLText';
    DataSet := FSlowData;
  end;
  with TWideMemoField.Create(FSlowData) do
  begin
    FieldName := 'PlanLine';
    DataSet := FSlowData;
  end;
  with TWideMemoField.Create(FSlowData) do
  begin
    FieldName := 'ParamText';
    DataSet := FSlowData;
  end;
  with TWideMemoField.Create(FSlowData) do
  begin
    FieldName := 'PerfText';
    DataSet := FSlowData;
  end;

  with TDateTimeField.Create(FSlowData) do
  begin
    FieldName := 'ExecAt';
    DataSet := FSlowData;
  end;
  with TStringField.Create(FSlowData) do
  begin
    FieldName := 'AppExe';
    Size := 255;
    DataSet := FSlowData;
  end;
  with TStringField.Create(FSlowData) do
  begin
    FieldName := 'Client';
    Size := 128;
    DataSet := FSlowData;
  end;

  FPerfShort := TStringField.Create(FSlowData);
  FPerfShort.FieldName := 'PerfShort';
  FPerfShort.Size := 255;
  FPerfShort.DataSet := FSlowData;
  FParamShort := TStringField.Create(FSlowData);
  FParamShort.FieldName := 'ParamShort';
  FParamShort.Size := 255;
  FParamShort.DataSet := FSlowData;

  FSlowData.OnCalcFields := CDSCalcFields;
  FSlowData.CreateDataSet;

  FSlowDS := TDataSource.Create(Self);
  FSlowDS.DataSet := FSlowData;
  tvSlow.DataController.DataSource := FSlowDS;

  Rank.DataBinding.FieldName := 'Rank';
  TimeMs.DataBinding.FieldName := 'TimeMs';
  TimeMs.OnGetDisplayText := TimeMsGetDisplayText;
  SQLType.DataBinding.FieldName := 'SQLType';
  Rows.DataBinding.FieldName := 'Rows';
  PlanNat.DataBinding.FieldName := 'PlanNat';
  SQLText.DataBinding.FieldName := 'SQLText';
  Plan.DataBinding.FieldName := 'PlanLine';
  Params.DataBinding.FieldName := 'ParamShort';
  Perf.DataBinding.FieldName := 'PerfShort';

  SetupSlowGrid;
end;

procedure TForm1.RowNoGetDisplayText(Sender: TcxCustomGridTableItem;
  ARecord: TcxCustomGridRecord; var AText: string);
begin
  if ARecord <> nil then
    AText := IntToStr(ARecord.Index + 1)
  else
    AText := '';
end;

procedure TForm1.TimeMsGetDisplayText(Sender: TcxCustomGridTableItem;
  ARecord: TcxCustomGridRecord; var AText: string);
var
  V: Variant;
  Ms: Int64;
begin
  V := ARecord.Values[Sender.Index];
  if VarIsNull(V) or VarIsEmpty(V) then
  begin
    AText := '';
    Exit;
  end;
  Ms := Round(VarAsType(V, varDouble));
  AText := Format('%.2d:%.2d:%.2d', [Ms div 3600000, (Ms div 60000) mod 60,
    (Ms div 1000) mod 60]);
end;

function FormatMsSmart(Ms: Int64): string;
begin
  if Ms < 1000 then
    Result := Format('%d ms', [Ms])
  else if Ms < 60 * 1000 then
    Result := Format('%d.%03d s', [Ms div 1000, Ms mod 1000])
  else
    Result := Format('%.2d:%.2d:%.2d', [Ms div 3600000, (Ms div 60000) mod 60,
      (Ms div 1000) mod 60]);
end;

procedure TForm1.ShowStatistics;
var
  IdxDict: TDictionary<string, Integer>;
  ErrDict: TDictionary<string, Integer>;
  IdxArr: ITraceLogParser.TIndexStatArray;
  ErrArr: ITraceLogParser.TStringArray;
  i, P: Integer;
  Key: string;
begin
  edSt1ErrorCount.Text := IntToStr(FTraceLogParser.ErrorCount);
  edSt1CommitCount.Text := IntToStr(FTraceLogParser.CommitCount);
  edSt1RollbackCount.Text := IntToStr(FTraceLogParser.RollbackCount);
  edSt1RecordCount.Text := IntToStr(FTraceLogParser.RecordCount);

  IdxDict := TDictionary<string, Integer>.Create;
  try
    IdxArr := FTraceLogParser.IndexUsage;
    for i := Low(IdxArr) to High(IdxArr) do
      IdxDict.AddOrSetValue(IdxArr[i].Name, IdxArr[i].Count);
    FillDictToVGrid(cxVerticalGrid1, IdxDict, 'Використання індексів', True);
  finally
    IdxDict.Free;
  end;

  ErrDict := TDictionary<string, Integer>.Create;
  try
    ErrArr := FTraceLogParser.ErrorTypes;
    for i := Low(ErrArr) to High(ErrArr) do
    begin
      P := Pos('=', ErrArr[i]);
      if P > 0 then
      begin
        Key := Copy(ErrArr[i], 1, P - 1);
        ErrDict.AddOrSetValue(Key, StrToIntDef(Copy(ErrArr[i], P + 1, MaxInt),
            0));
      end;
    end;
    FillDictToVGrid(cxVerticalGrid2, ErrDict, 'Типи помилок', False);
  finally
    ErrDict.Free;
  end;

  FillSlowGrid;

  if Assigned(FCardSelect) then
    FCardSelect.SetValueInt64(FTraceLogParser.CountSelect);
  if Assigned(FCardInsert) then
    FCardInsert.SetValueInt64(FTraceLogParser.CountInsert);
  if Assigned(FCardUpdate) then
    FCardUpdate.SetValueInt64(FTraceLogParser.CountUpdate);
  if Assigned(FCardDelete) then
    FCardDelete.SetValueInt64(FTraceLogParser.CountDelete);

  if Assigned(FCardSelect) then
    FCardSelect.SetMeta('Разом: ' + FormatTimeMsToHMS
        (Round(FTraceLogParser.TotalTimeSelect)),
      'Макс:  ' + FormatTimeMsToHMS
        (Round(FTraceLogParser.MaxTimeSelect)),
      'Сер.:  ' + FormatMsSmart(Round(FTraceLogParser.AvgTimeSelect)));
  if Assigned(FCardInsert) then
    FCardInsert.SetMeta('Разом: ' + FormatTimeMsToHMS
        (Round(FTraceLogParser.TotalTimeInsert)),
      'Макс:  ' + FormatTimeMsToHMS
        (Round(FTraceLogParser.MaxTimeInsert)),
      'Сер.:  ' + FormatMsSmart(Round(FTraceLogParser.AvgTimeInsert)));
  if Assigned(FCardUpdate) then
    FCardUpdate.SetMeta('Разом: ' + FormatTimeMsToHMS
        (Round(FTraceLogParser.TotalTimeUpdate)),
      'Макс:  ' + FormatTimeMsToHMS
        (Round(FTraceLogParser.MaxTimeUpdate)),
      'Сер.:  ' + FormatMsSmart(Round(FTraceLogParser.AvgTimeUpdate)));
  if Assigned(FCardDelete) then
    FCardDelete.SetMeta('Разом: ' + FormatTimeMsToHMS
        (Round(FTraceLogParser.TotalTimeDelete)),
      'Макс:  ' + FormatTimeMsToHMS
        (Round(FTraceLogParser.MaxTimeDelete)),
      'Сер.:  ' + FormatMsSmart(Round(FTraceLogParser.AvgTimeDelete)));

  LayoutCards;
end;

procedure TForm1.tvSlowDblClick(Sender: TObject);
var
  D: TfSlowDetailDlg;
  SD: TSlowStmtData;
begin
  SD.SQLText := FSlowData.FieldByName('SQLText').AsString;
  SD.PlanText := FSlowData.FieldByName('PlanLine').AsString;
  SD.ParamText := FSlowData.FieldByName('ParamText').AsString;
  SD.PerfText := FSlowData.FieldByName('PerfText').AsString;

  D := TfSlowDetailDlg.Create(Self);
  try
    D.LoadFromSlowStmt(SD);
    D.ShowModal;
  finally
    D.Free;
  end;
end;

function PerfSummaryForGrid(const S: string): string;
var
  L: TStringList;
  i, found: Integer;
  line: string;
  pieces: array [0 .. 1] of string;
begin
  Result := '';
  L := TStringList.Create;
  try
    L.Text := StringReplace(S, #13#10, #10, [rfReplaceAll]);
    found := 0;
    for i := 0 to L.Count - 1 do
    begin
      line := Trim(L[i]);
      if (line = '') or IsOnlySeparators(line) then
        Continue;
      if SameText(Copy(line, 1, 5), 'Table') then
        Continue;
      pieces[found] := line;
      Inc(found);
      if found = Length(pieces) then
        Break;
    end;
    case found of
      0:
        Result := '';
      1:
        Result := pieces[0];
    else
      Result := pieces[0] + ' | ' + pieces[1];
    end;
    if Result <> '' then
      Result := OneLineEllipsis(Result, 160);
  finally
    L.Free;
  end;
end;

procedure TForm1.FillSlowGrid;
var
  SlowArr: TSlowArray;
  i: Integer;
  S: string;
begin
  tvSlow.BeginUpdate;
  FSlowData.DisableControls;
  try
    FSlowData.EmptyDataSet;
    SlowArr := FTraceLogParser.TopSlow(100000);

    for i := Low(SlowArr) to High(SlowArr) do
    begin
      FSlowData.Append;
      FSlowData.FieldByName('Rank').AsInteger := i + 1;
      FSlowData.FieldByName('TimeMs').AsFloat := SlowArr[i].TimeMs;
      FSlowData.FieldByName('SQLType').AsString := SlowArr[i].SQLType;
      FSlowData.FieldByName('Rows').AsLargeInt := SlowArr[i].Rows;
      FSlowData.FieldByName('PlanNat').AsBoolean := SlowArr[i].PlanNat;
      FSlowData.FieldByName('SQLText').AsString := SlowArr[i].SQLText;
      FSlowData.FieldByName('PlanLine').AsString := SlowArr[i].PlanText;
      FSlowData.FieldByName('ParamText').AsString := SlowArr[i].ParamText;
      FSlowData.FieldByName('PerfText').AsString := SlowArr[i].PerfText;

      try
        FSlowData.FieldByName('ExecAt').AsDateTime := SlowArr[i].ExecAt;
      except
        on Exception do
          FSlowData.FieldByName('ExecAt').Clear;
      end;
      try
        FSlowData.FieldByName('AppExe').AsString := SlowArr[i].AppExe;
      except
        on Exception do
          FSlowData.FieldByName('AppExe').Clear;
      end;
      try
        FSlowData.FieldByName('Client').AsString := SlowArr[i].Client;
      except
        on Exception do
          FSlowData.FieldByName('Client').Clear;
      end;

      S := SlowArr[i].ParamText;
      if IsOnlySeparators(Trim(S)) then
        FSlowData.FieldByName('ParamShort').AsString := ''
      else
        FSlowData.FieldByName('ParamShort').AsString := OneLineEllipsis(S, 160);

      S := SlowArr[i].PerfText;
      FSlowData.FieldByName('PerfShort').AsString := PerfSummaryForGrid(S);

      FSlowData.Post;
    end;
  finally
    FSlowData.EnableControls;
    tvSlow.EndUpdate;
  end;

  if FSlowData.RecordCount > 0 then
  begin
    FSlowData.First;
    tvSlow.DataController.GotoFirst;
    tvSlow.Controller.TopRowIndex := 0;
  end;

  EnsureGridColumnsOrder;
end;

procedure TForm1.LiveToggle(Sender: TObject);
begin
  if Trim(FFileName) = '' then
    Exit;
  if chLive.Checked then
  begin
    FTraceLogParser.TailBegin(FFileName, True);
    tmrLive.Enabled := True;
    sbStatus.Panels[0].Text := 'Live tail…';
  end
  else
  begin
    tmrLive.Enabled := False;
    sbStatus.Panels[0].Text := 'Live off';
  end;
end;

procedure TForm1.LiveTick(Sender: TObject);
begin
  if not tmrLive.Enabled then
    Exit;
  if FTraceLogParser.TailPoll then
  begin
    ShowStatistics;
  end;
end;

end.
