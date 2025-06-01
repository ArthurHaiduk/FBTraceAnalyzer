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
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, DBClient;

type
  TForm1 = class(TForm)
    mmMain: TMainMenu;
    miFile: TMenuItem;
    miOpen: TMenuItem;
    pMain: TPanel;
    bGetStatistics: TButton;
    gbSelectStatements: TGroupBox;
    lbSelectCount: TLabel;
    edSelectCount: TEdit;
    lbSelectTotalTime: TLabel;
    edSelectTotalTime: TEdit;
    lbSelectMaxTime: TLabel;
    edSelectMaxTime: TEdit;
    lbSelectAvgTime: TLabel;
    edSelectAvgTime: TEdit;
    gbInsertStatements: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edInsertCount: TEdit;
    edInsertTotalTime: TEdit;
    edInsertMaxTime: TEdit;
    edInsertAvgTime: TEdit;
    gbUpdateStatements: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    edUpdateCount: TEdit;
    edUpdateTotalTime: TEdit;
    edUpdateMaxTime: TEdit;
    edUpdateAvgTime: TEdit;
    gbDeleteStatements: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    edDeleteCount: TEdit;
    edDeleteTotalTime: TEdit;
    edDeleteMaxTime: TEdit;
    edDeleteAvgTime: TEdit;
    lbInsertCount: TLabel;
    PageControl1: TPageControl;
    tsGeneral: TTabSheet;
    pTop: TPanel;
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
    cxVerticalGrid3: TcxVerticalGrid;
    tsOverview: TTabSheet;
    tsSlow: TTabSheet;
    cxGridErrorsDBTableView1: TcxGridDBTableView;
    cxGridErrorsLevel1: TcxGridLevel;
    cxGridErrors: TcxGrid;
    cxGridIndexesDBTableView1: TcxGridDBTableView;
    cxGridIndexesLevel1: TcxGridLevel;
    cxGridIndexes: TcxGrid;
    tvSlow: TcxGridDBTableView;
    cxGridSlowLevel1: TcxGridLevel;
    cxGridSlow: TcxGrid;
    Rank: TcxGridDBColumn;
    TimeMs: TcxGridDBColumn;
    SQLType: TcxGridDBColumn;
    Rows: TcxGridDBColumn;
    PlanNat: TcxGridDBColumn;
    SQLText: TcxGridDBColumn;
    odMain: TOpenDialog;
    Plan: TcxGridDBColumn;
    Params: TcxGridDBColumn;
    Perf: TcxGridDBColumn;
    procedure miOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bGetStatisticsClick(Sender: TObject);
    procedure tvSlowDblClick(Sender: TObject);
  private
    FFileName: string;
    FTraceLogParser: TTraceLogParser;
    FSlowData: TClientDataSet;
    FSlowDS: TDataSource;
    procedure InitSlowDataset;
    procedure FillSlowGrid;
    procedure ShowStatistics;
      procedure TimeMsGetDisplayText(
    Sender: TcxCustomGridTableItem;
    ARecord: TcxCustomGridRecord;
    var AText: string);
  end;

var
  Form1: TForm1;

implementation

uses FuncTrace, uProgressDlg, uSlowDetailDlg;
{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTraceLogParser := TTraceLogParser.Create;
  InitSlowDataset;
end;

procedure TForm1.InitSlowDataset;
begin
  FSlowData := TClientDataSet.Create(Self);
  with FSlowData.FieldDefs do
  begin
    Add('Rank', ftInteger);
    Add('TimeMs', ftFloat);
    Add('SQLType', ftString, 10);
    Add('Rows', ftLargeint);
    Add('PlanNat', ftBoolean);
    Add('SQLText', ftWideMemo);
    Add('PlanLine' , ftWideMemo);
    Add('ParamText', ftWideMemo);
    Add('PerfText' , ftWideMemo);
  end;
  FSlowData.CreateDataSet;

  FSlowDS := TDataSource.Create(Self);
  FSlowDS.DataSet := FSlowData;
  tvSlow.DataController.DataSource := FSlowDS;

  // прив’язуємо поля до колонок (назви колонок такі, як у DFM)
  Rank.DataBinding.FieldName := 'Rank';
  TimeMs.DataBinding.FieldName := 'TimeMs';
  TimeMs.OnGetDisplayText      := TimeMsGetDisplayText;
  SQLType.DataBinding.FieldName := 'SQLType';
  Rows.DataBinding.FieldName := 'Rows';
  PlanNat.DataBinding.FieldName := 'PlanNat';
  SQLText.DataBinding.FieldName := 'SQLText';
  Plan.DataBinding.FieldName   := 'PlanLine';
  Params.DataBinding.FieldName := 'ParamText';
  Perf.DataBinding.FieldName   := 'PerfText';
end;

procedure TForm1.TimeMsGetDisplayText(Sender: TcxCustomGridTableItem;
  ARecord: TcxCustomGridRecord; var AText: string);
var
  Ms : Int64;
begin
  // Беремо числове значення без конверсій у текст
  Ms := Round(VarAsType(ARecord.Values[Sender.Index], varDouble));  // мілісекунди

  // Конвертуємо у hh:mm:ss
  AText := Format('%.2d:%.2d:%.2d',
                  [ Ms div 3600000,                // години
                    (Ms div 60000) mod 60,         // хвилини
                    (Ms div 1000)  mod 60 ]);      // секунди
end;

procedure TForm1.miOpenClick(Sender: TObject);
begin
  if odMain.Execute(Handle) then
    FFileName := odMain.FileName;
end;

procedure TForm1.bGetStatisticsClick(Sender: TObject);
var
  dlg: TfProgressDlg;
begin
  if Trim(FFileName) = '' then
  begin
    MessageDlg('Файл не відкритий!', mtError, [mbOk], 0);
    Exit;
  end;

  dlg := TfProgressDlg.Create(Self);
  try
    dlg.lblMsg.Caption := 'Обробка файлу…';
    dlg.pb.Max := 100;
    dlg.pb.Position := 0;
    dlg.Show;
    Application.ProcessMessages;

    FTraceLogParser.ParseLog(FFileName,
      procedure(const APos, ATotal: Int64)begin dlg.SafeUpdate(APos); end);

  finally
    dlg.Close;
    dlg.Free;
  end;

  ShowStatistics;
end;

procedure TForm1.ShowStatistics;
var
  IdxDict: TDictionary<string, Integer>;
  ErrDict: TDictionary<string, Integer>;

  IdxArr: TraceLogParser.TIndexStatArray;
  ErrArr: TraceLogParser.TStringArray;

  TopArrTP: TraceLogParser.TStringArray;
  TopArr: FuncTrace.TStringArray;

  i, p: Integer;
  key: string;
begin
  edSelectCount.Text := IntToStr(FTraceLogParser.CountSelect);
  edSelectTotalTime.Text := FormatTimeMsToHMS
    (Round(FTraceLogParser.TotalTimeSelect));
  edSelectMaxTime.Text := FormatTimeMsToHMS
    (Round(FTraceLogParser.MaxTimeSelect));
  edSelectAvgTime.Text := FormatTimeMsToHMS
    (Round(FTraceLogParser.AvgTimeSelect));

  edInsertCount.Text := IntToStr(FTraceLogParser.CountInsert);
  edInsertTotalTime.Text := FormatTimeMsToHMS
    (Round(FTraceLogParser.TotalTimeInsert));
  edInsertMaxTime.Text := FormatTimeMsToHMS
    (Round(FTraceLogParser.MaxTimeInsert));
  edInsertAvgTime.Text := FormatTimeMsToHMS
    (Round(FTraceLogParser.AvgTimeInsert));

  edUpdateCount.Text := IntToStr(FTraceLogParser.CountUpdate);
  edUpdateTotalTime.Text := FormatTimeMsToHMS
    (Round(FTraceLogParser.TotalTimeUpdate));
  edUpdateMaxTime.Text := FormatTimeMsToHMS
    (Round(FTraceLogParser.MaxTimeUpdate));
  edUpdateAvgTime.Text := FormatTimeMsToHMS
    (Round(FTraceLogParser.AvgTimeUpdate));

  edDeleteCount.Text := IntToStr(FTraceLogParser.CountDelete);
  edDeleteTotalTime.Text := FormatTimeMsToHMS
    (Round(FTraceLogParser.TotalTimeDelete));
  edDeleteMaxTime.Text := FormatTimeMsToHMS
    (Round(FTraceLogParser.MaxTimeDelete));
  edDeleteAvgTime.Text := FormatTimeMsToHMS
    (Round(FTraceLogParser.AvgTimeDelete));

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
      p := Pos('=', ErrArr[i]);
      if p > 0 then
      begin
        key := Copy(ErrArr[i], 1, p - 1);
        ErrDict.AddOrSetValue(key, StrToIntDef(Copy(ErrArr[i], p + 1, MaxInt),
            0));
      end;
    end;

    FillDictToVGrid(cxVerticalGrid2, ErrDict, 'Типи помилок', False);
  finally
    ErrDict.Free;
  end;

  TopArrTP := FTraceLogParser.TopUsedIndexes(100);
  SetLength(TopArr, Length(TopArrTP));
  for i := Low(TopArrTP) to High(TopArrTP) do
    TopArr[i] := TopArrTP[i];

  FillArrayToVGrid(cxVerticalGrid3, TopArr,
    Format('Топ-%d індексів', [Length(TopArr)]));
  FillSlowGrid;
end;

procedure TForm1.tvSlowDblClick(Sender: TObject);
var
  D  : TfSlowDetailDlg;
  SD : TSlowStmtData;
begin
  // Заповнюємо структуру з поточного запису ґріда
  SD.SQLText   := FSlowData.FieldByName('SQLText').AsString;
  SD.PlanText  := FSlowData.FieldByName('PlanLine').AsString;    // ← додасте поле
  SD.ParamText := FSlowData.FieldByName('ParamText').AsString;   // ← додасте поле
  SD.PerfText  := FSlowData.FieldByName('PerfText').AsString;    // ← додасте поле

  D := TfSlowDetailDlg.Create(Self);
  try
    D.LoadFromSlowStmt(SD);
    D.ShowModal;
  finally
    D.Free;
  end;
end;

procedure TForm1.FillSlowGrid;
var
  SlowArr : TSlowArray;
  i       : Integer;
begin
  FSlowData.DisableControls;
  try
    FSlowData.EmptyDataSet;
    SlowArr := FTraceLogParser.TopSlow(500);      // показуємо топ-100
    for i := Low(SlowArr) to High(SlowArr) do
    begin
      FSlowData.Append;
      FSlowData.FieldByName('Rank').AsInteger    := i + 1;
      FSlowData.FieldByName('TimeMs').AsFloat    := SlowArr[i].TimeMs;
      FSlowData.FieldByName('SQLType').AsString  := SlowArr[i].SQLType;
      FSlowData.FieldByName('Rows').AsLargeInt   := SlowArr[i].Rows;
      FSlowData.FieldByName('PlanNat').AsBoolean := SlowArr[i].PlanNat;
      FSlowData.FieldByName('SQLText').AsString  := SlowArr[i].SQLText;
      FSlowData.FieldByName('PlanLine').AsString := SlowArr[i].PlanText;
      FSlowData.FieldByName('ParamText').AsString:= SlowArr[i].ParamText;
      FSlowData.FieldByName('PerfText').AsString := SlowArr[i].PerfText;
      FSlowData.Post;
    end;
  finally
    FSlowData.EnableControls;
  end;
end;

end.
