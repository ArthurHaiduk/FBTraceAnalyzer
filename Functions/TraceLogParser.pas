unit TraceLogParser;

{ ---------------------------------------------------------------------------
  Firebird Trace‑log parser — FBTraceAnalyzer project
  ---------------------------------------------------------------------------
  Delphi 2010‑compatible (no inline‑vars, no record constructors).
  Parses trace‑logs from `fbtracemgr` and collects statistics + list of
  slow statements (full SQL, PLAN, parameters, perf).
  --------------------------------------------------------------------------- }

interface

uses
  SysUtils, Classes, StrUtils, Math,
  Generics.Collections, Generics.Defaults;

type
  TProgressProc = reference to procedure(const APos, ATotal: Int64);

  TErrorType = (etDeadlock, etForeignKey, etUniqueKey, etNotNull,
    etLockConflict, etGeneral);

  TIndexStat = record
    Name: string;
    Count: Integer;
  end;

  TStringArray = array of string;
  TIndexStatArray = array of TIndexStat;

  { ---- info for a single slow statement ------------------------------- }
  TSlowStmt = record
    SQLType: string;
    TimeMs: Double;
    Rows: Int64;
    SQLText: string;
    PlanNat: Boolean;
    PlanText: string;
    ParamText: string;
    PerfText: string;
  end;

  TSlowArray = array of TSlowStmt;

  { -------------------------------------------------------------------- }
  TTraceLogParser = class
  private
    { global stats }
    FSelCnt, FInsCnt, FUpdCnt, FDelCnt: Integer;
    FSelTot, FInsTot, FUpdTot, FDelTot: Double;
    FSelMax, FInsMax, FUpdMax, FDelMax: Double;

    FErrCnt: Integer;
    FErrTypeCnt: array [TErrorType] of Integer;
    FCommitCnt: Integer;
    FRollCnt: Integer;
    FRecCnt: Int64;

    FIdxMap: TDictionary<string, Integer>;
    FSlowList: TList<TSlowStmt>;

    FSlowThreshold: Double; // ms; 0 = collect all

    procedure ResetStats;

    function GetAvgTimeSelect: Double;
    function GetAvgTimeInsert: Double;
    function GetAvgTimeUpdate: Double;
    function GetAvgTimeDelete: Double;

    procedure AddIndex(const AName: string);
    function ErrName(ET: TErrorType): string;
    function ExtractMs(const S: string): Double;
  public
    constructor Create;
    destructor Destroy; override;

    { counters }
    property CountSelect: Integer read FSelCnt;
    property CountInsert: Integer read FInsCnt;
    property CountUpdate: Integer read FUpdCnt;
    property CountDelete: Integer read FDelCnt;

    property TotalTimeSelect: Double read FSelTot;
    property TotalTimeInsert: Double read FInsTot;
    property TotalTimeUpdate: Double read FUpdTot;
    property TotalTimeDelete: Double read FDelTot;

    property MaxTimeSelect: Double read FSelMax;
    property MaxTimeInsert: Double read FInsMax;
    property MaxTimeUpdate: Double read FUpdMax;
    property MaxTimeDelete: Double read FDelMax;

    property AvgTimeSelect: Double read GetAvgTimeSelect;
    property AvgTimeInsert: Double read GetAvgTimeInsert;
    property AvgTimeUpdate: Double read GetAvgTimeUpdate;
    property AvgTimeDelete: Double read GetAvgTimeDelete;

    property ErrorCount: Integer read FErrCnt;
    property CommitCount: Integer read FCommitCnt;
    property RollbackCount: Integer read FRollCnt;
    property RecordCount: Int64 read FRecCnt;

    property SlowThreshold: Double read FSlowThreshold write FSlowThreshold;

    function SlowQueries: TSlowArray;
    function TopSlow(N: Integer = 10): TSlowArray;

    function ErrorTypes: TStringArray;
    function IndexUsage: TIndexStatArray;
    function TopUsedIndexes(ATop: Integer = 10): TStringArray;

    procedure ParseLog(const FileName: string;
      const AProgress: TProgressProc = nil);
  end;

implementation

uses Windows; { for FillChar }

{ =========================== helpers =================================== }
constructor TTraceLogParser.Create;
begin
  inherited Create;
  FIdxMap := TDictionary<string, Integer>.Create(1000);
  FSlowList := TList<TSlowStmt>.Create;
  FSlowThreshold := 500; // 0.5 sec
end;

destructor TTraceLogParser.Destroy;
begin
  FIdxMap.Free;
  FSlowList.Free;
  inherited Destroy;
end;

procedure TTraceLogParser.ResetStats;
begin
  FillChar(Self.FSelCnt, SizeOf(FSelCnt) + SizeOf(FInsCnt) * 3 + SizeOf
      (FSelTot) * 4, 0);
  FillChar(FErrTypeCnt, SizeOf(FErrTypeCnt), 0);
  FIdxMap.Clear;
  FSlowList.Clear;
end;

function TTraceLogParser.GetAvgTimeSelect: Double;
begin
  if FSelCnt > 0 then
    Result := FSelTot / FSelCnt
  else
    Result := 0;
end;

function TTraceLogParser.GetAvgTimeInsert: Double;
begin
  if FInsCnt > 0 then
    Result := FInsTot / FInsCnt
  else
    Result := 0;
end;

function TTraceLogParser.GetAvgTimeUpdate: Double;
begin
  if FUpdCnt > 0 then
    Result := FUpdTot / FUpdCnt
  else
    Result := 0;
end;

function TTraceLogParser.GetAvgTimeDelete: Double;
begin
  if FDelCnt > 0 then
    Result := FDelTot / FDelCnt
  else
    Result := 0;
end;

function TTraceLogParser.ExtractMs(const S: string): Double;
var
  p, i: Integer;
  num: string;
  val: Double;
begin
  Result := -1;
  p := Pos(' ms', S);
  if p = 0 then
    p := Pos(' sec', S);
  if p = 0 then
    p := Pos(' s', S);
  if p = 0 then
    Exit;
  i := p - 1;
  while (i > 0) and (S[i] = ' ') do
    Dec(i);
  while (i > 0) and (S[i] in ['0' .. '9', '.', ',']) do
    Dec(i);
  num := Copy(S, i + 1, p - i - 1);
  if TryStrToFloat(StringReplace(num, ',', '.', [rfReplaceAll]), val) then
  begin
    if Copy(S, p + 1, 3) = 'sec' then
      val := val * 1000
    else if Copy(S, p + 1, 1) = 's' then
      val := val * 1000;
    Result := val;
  end;
end;

procedure TTraceLogParser.AddIndex(const AName: string);
var
  c: Integer;
begin
  if AName = '' then
    Exit;
  if FIdxMap.TryGetValue(AName, c) then
    FIdxMap[AName] := c + 1
  else
    FIdxMap.Add(AName, 1);
end;

function TTraceLogParser.ErrName(ET: TErrorType): string;
begin
  case ET of
    etDeadlock:
      Result := 'Deadlock';
    etForeignKey:
      Result := 'ForeignKey';
    etUniqueKey:
      Result := 'UniqueKey';
    etNotNull:
      Result := 'NotNull';
    etLockConflict:
      Result := 'LockConflict';
  else
    Result := 'General';
  end;
end;

function TTraceLogParser.ErrorTypes: TStringArray;
var
  ET: TErrorType;
  i: Integer;
begin
  SetLength(Result, 6);
  i := 0;
  for ET := Low(TErrorType) to High(TErrorType) do
  begin
    Result[i] := Format('%s=%d', [ErrName(ET), FErrTypeCnt[ET]]);
    Inc(i);
  end;
end;

function TTraceLogParser.IndexUsage: TIndexStatArray;
var
  pair: TPair<string, Integer>;
  i: Integer;
begin
  SetLength(Result, FIdxMap.Count);
  i := 0;
  for pair in FIdxMap do
  begin
    Result[i].Name := pair.Key;
    Result[i].Count := pair.Value;
    Inc(i);
  end;
end;

function TTraceLogParser.TopUsedIndexes(ATop: Integer): TStringArray;
var
  iu: TIndexStatArray;
  lst: TList<TIndexStat>;
  i, cnt: Integer;
begin
  iu := IndexUsage;
  lst := TList<TIndexStat>.Create;
  try
    for i := 0 to High(iu) do
      lst.Add(iu[i]);
    lst.Sort(TComparer<TIndexStat>.Construct( function(const A,
          B: TIndexStat): Integer begin Result := B.Count - A.Count; end));
    if ATop < 1 then
      ATop := 1;
    cnt := lst.Count;
    if cnt > ATop then
      cnt := ATop;
    SetLength(Result, cnt);
    for i := 0 to cnt - 1 do
      Result[i] := lst.Items[i].Name;
  finally
    lst.Free;
  end;
end;

function TTraceLogParser.SlowQueries: TSlowArray;
var
  i: Integer;
begin
  SetLength(Result, FSlowList.Count);
  for i := 0 to FSlowList.Count - 1 do
    Result[i] := FSlowList.Items[i];
end;

function TTraceLogParser.TopSlow(N: Integer): TSlowArray;
var
  i: Integer;
begin
  if N < 1 then
    N := 1;
  FSlowList.Sort(TComparer<TSlowStmt>.Construct( function(const A,
        B: TSlowStmt): Integer begin if A.TimeMs > B.TimeMs then Result :=
        -1 else if A.TimeMs < B.TimeMs then Result := 1 else Result := 0;
      end));
  if N > FSlowList.Count then
    N := FSlowList.Count;
  SetLength(Result, N);
  for i := 0 to N - 1 do
    Result[i] := FSlowList.Items[i];
end;

{ ----------------------------------------------------------------------- }
procedure TTraceLogParser.ParseLog(const FileName: string;
  const AProgress: TProgressProc);
const
  BUF_SIZE = 16 * 1024 * 1024;
var
  FS: TFileStream;
  Buf: TBytes;
  Tail: RawByteString;
  EventBuf: TStringList;
  FileSize, BytesDone: Int64;
  LastPct: Integer;

  { =============================================================== }
  procedure ProcessEvent(const L: TStringList);
  var
    hdr, SQLType, planLine, line, lineLower: string;
    i, startIdx, p1, p2, pNum: Integer;
    ms, recCount: Double;
    isNat: Boolean;
    ET: TErrorType;
    stmt: TSlowStmt;
    sb, paramSb, perfSb: TStringBuilder;
    inParam: Boolean;
    comma: Integer;
    idxName: string;
    function IsSQLLine(const S: string): Boolean;
    begin
      Result := (S <> '') and not(S[1] in ['-', '^']) and not SameText
        (Copy(S, 1, 5), 'PLAN ') and not SameText(S, 'Parameters:') and not
        ((Length(S) > 2) and (S[2] = ':') and (S[3] = '\'));
    end;

  begin
    if L.Count = 0 then
      Exit;
    hdr := L[0];

    { quick exits for commit/rollback }
    if Pos('COMMIT_TRANSACTION', hdr) > 0 then
    begin
      Inc(FCommitCnt);
      Exit;
    end;
    if Pos('ROLLBACK_TRANSACTION', hdr) > 0 then
    begin
      Inc(FRollCnt);
      Exit;
    end;

    { errors }
    if (Pos('ERROR', hdr) > 0) or (Pos('FAILED', hdr) > 0) then
    begin
      Inc(FErrCnt);
      ET := etGeneral;
      for line in L do
      begin
        lineLower := LowerCase(line);
        if Pos('deadlock', lineLower) > 0 then
          ET := etDeadlock
        else if (Pos('foreign', lineLower) > 0) and (Pos('key', lineLower) > 0)
          then
          ET := etForeignKey
        else if (Pos('unique', lineLower) > 0) or (Pos('duplicate',
            lineLower) > 0) then
          ET := etUniqueKey
        else if Pos(' null ', lineLower) > 0 then
          ET := etNotNull
        else if Pos('lock conflict', lineLower) > 0 then
          ET := etLockConflict;
      end;
      Inc(FErrTypeCnt[ET]);
      Exit;
    end;

    if Pos('EXECUTE_STATEMENT_FINISH', hdr) = 0 then
      Exit;

    { locate "Statement NNN:" }
    startIdx := -1;
    planLine := '';
    for i := 1 to L.Count - 1 do
    begin
      line := L[i];
      if (startIdx < 0) and (Pos('Statement ', line) = 1) then
        startIdx := i;
      if (planLine = '') and (Pos('PLAN', line) = 1) then
        planLine := line;
    end;
    if startIdx < 0 then
      Exit; // no SQL

    { get first real SQL line for type }
    i := startIdx + 1;
    while (i < L.Count) and not IsSQLLine(Trim(L[i])) do
      Inc(i);
    if i >= L.Count then
      Exit;
    SQLType := UpperCase(Copy(Trim(L[i]), 1, Pos(' ', Trim(L[i]) + ' ') - 1));
    if SQLType = '' then
      Exit;

    { NATURAL/INDEX info }
    isNat := Pos('NATURAL', planLine) > 0;
    if planLine <> '' then
    begin
      p1 := Pos('INDEX', planLine);
      while p1 > 0 do
      begin
        p1 := PosEx('(', planLine, p1);
        if p1 = 0 then
          Break;
        p2 := PosEx(')', planLine, p1);
        if p2 = 0 then
          Break;
        line := Copy(planLine, p1 + 1, p2 - p1 - 1);
        repeat
          comma := Pos(',', line);
          idxName := IfThen(comma > 0, Trim(Copy(line, 1, comma - 1)),
            Trim(line));
          AddIndex(idxName);
          if comma > 0 then
            line := Trim(Copy(line, comma + 1, MaxInt))
          else
            line := '';
        until line = '';
        p1 := PosEx('INDEX', planLine, p2);
      end;
    end;

    { timing & records fetched }
    ms := -1;
    recCount := 0;
    for i := L.Count - 1 downto 0 do
    begin
      line := L[i];
      lineLower := LowerCase(line);
      if (ms < 0) and ((Pos(' ms', lineLower) > 0) or (Pos(' sec',
            lineLower) > 0) or (Pos(' s', lineLower) > 0)) then
        ms := ExtractMs(line);
      if (recCount = 0) and (Pos(' records fetched', lineLower) > 0) then
      begin
        p1 := Pos(' records fetched', lineLower) - 1;
        while (p1 > 0) and (line[p1] = ' ') do
          Dec(p1);
        pNum := p1;
        while (pNum > 0) and (line[pNum] in ['0' .. '9']) do
          Dec(pNum);
        Inc(pNum);
        recCount := StrToIntDef(Copy(line, pNum, p1 - pNum + 1), 0);
        Inc(FRecCnt, Trunc(recCount));
      end;
      if (ms >= 0) and (recCount > 0) then
        Break;
    end;
    if ms < 0 then
      ms := ExtractMs(hdr);

    { update global counters }
    if SameText(SQLType, 'SELECT') then
    begin
      Inc(FSelCnt);
      if ms >= 0 then
      begin
        FSelTot := FSelTot + ms;
        if ms > FSelMax then
          FSelMax := ms;
      end;
    end
    else if SameText(SQLType, 'INSERT') then
    begin
      Inc(FInsCnt);
      if ms >= 0 then
      begin
        FInsTot := FInsTot + ms;
        if ms > FInsMax then
          FInsMax := ms;
      end;
    end
    else if SameText(SQLType, 'UPDATE') then
    begin
      Inc(FUpdCnt);
      if ms >= 0 then
      begin
        FUpdTot := FUpdTot + ms;
        if ms > FUpdMax then
          FUpdMax := ms;
      end;
    end
    else if SameText(SQLType, 'DELETE') then
    begin
      Inc(FDelCnt);
      if ms >= 0 then
      begin
        FDelTot := FDelTot + ms;
        if ms > FDelMax then
          FDelMax := ms;
      end;
    end;

    { collect details if qualified }
    if (FSlowThreshold = 0) or (ms >= FSlowThreshold) then
    begin
      FillChar(stmt, SizeOf(stmt), 0);
      stmt.SQLType := SQLType;
      stmt.TimeMs := ms;
      stmt.Rows := Trunc(recCount);
      stmt.PlanNat := isNat;
      if Pos('PLAN', line) = 1 then
      begin
        if stmt.PlanText <> '' then
          stmt.PlanText := stmt.PlanText + sLineBreak;
        stmt.PlanText := stmt.PlanText + line;
      end;

      sb := TStringBuilder.Create;
      paramSb := TStringBuilder.Create;
      perfSb := TStringBuilder.Create;
      try
        { SQL lines }
        i := startIdx + 1;
        while (i < L.Count) and not IsSQLLine(Trim(L[i])) do
          Inc(i);
        while (i < L.Count) and IsSQLLine(Trim(L[i])) do
        begin
          if sb.Length > 0 then
            sb.AppendLine;
          sb.Append(Trim(L[i]));
          Inc(i);
        end;
        stmt.SQLText := sb.ToString;

        { parameters & performance }
        inParam := False;
        for line in L do
        begin
          lineLower := LowerCase(Trim(line)); // ① оновлюємо

          { --- параметри ---------------------------------------------------- }
          if (Pos('parameters:', lineLower) = 1) or // ②
            (Pos('param', lineLower) = 1) then
          begin
            inParam := True;
            //Continue;
          end;

          if inParam then
          begin
            if Trim(line) = '' then
            begin
              inParam := False;
              Continue;
            end;
            paramSb.AppendLine(Trim(line));
            Continue;
          end;

          { --- перфоманс ---------------------------------------------------- }
          if (Pos('elapsed', lineLower) > 0) or
            ((Pos(' ms', lineLower) > 0) and (Pos('fetch', lineLower) > 0))
            or (Pos('reads', lineLower) > 0) or (Pos('writes', lineLower) > 0)
            or (Pos('records fetched', lineLower) > 0) then
            perfSb.AppendLine(Trim(line));

          { --- PLAN --------------------------------------------------------- }
          if Pos('PLAN', line) = 1 then // ③
          begin
            if stmt.PlanText <> '' then
              stmt.PlanText := stmt.PlanText + sLineBreak;
            stmt.PlanText := stmt.PlanText + Trim(line);
            if Pos('NATURAL', line) > 0 then
              stmt.PlanNat := True;
          end;
        end;
        stmt.ParamText := paramSb.ToString;
        stmt.PerfText := perfSb.ToString;
      finally
        sb.Free;
        paramSb.Free;
        perfSb.Free;
      end;

      FSlowList.Add(stmt);
    end;
  end; // ProcessEvent

  procedure UpdateProgress;
  var
    p: Integer;
  begin
    if not Assigned(AProgress) then
      Exit;
    if FileSize = 0 then
      Exit;
    p := (BytesDone * 100) div FileSize;
    if p <> LastPct then
    begin
      LastPct := p;
      AProgress(p, 100);
    end;
  end;

  function IsEventStart(const S: string): Boolean;
  begin
    Result := (Pos('EXECUTE_STATEMENT_FINISH', S) > 0) or
      (Pos('ERROR AT', S) > 0) or (Pos('FAILED:', S) > 0) or
      (Pos('COMMIT_TRANSACTION', S) > 0) or (Pos('ROLLBACK_TRANSACTION',
        S) > 0);
  end;

  procedure FeedLine(p: PAnsiChar; Len: Integer);
  var
    S: string;
  begin
    if Len <= 0 then
      Exit;
    SetString(S, p, Len);
    if IsEventStart(S) then
    begin
      if EventBuf.Count > 0 then
        ProcessEvent(EventBuf);
      EventBuf.Clear;
    end;
    EventBuf.Add(S);
  end;

var
  ReadBytes: Integer;
  StartPtr, CurPtr, EndPtr: PAnsiChar;
  LineLen: Integer;
  Chunk: RawByteString;
begin
  ResetStats;
  EventBuf := TStringList.Create;
  try
    FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      FileSize := FS.Size;
      BytesDone := 0;
      LastPct := -1;
      SetLength(Buf, BUF_SIZE);
      Tail := '';
      while True do
      begin
        ReadBytes := FS.Read(Buf[0], Length(Buf));
        if ReadBytes = 0 then
          Break;
        SetString(Chunk, PAnsiChar(@Buf[0]), ReadBytes);
        Tail := Tail + Chunk;
        StartPtr := PAnsiChar(Tail);
        EndPtr := StartPtr + Length(Tail);
        CurPtr := StartPtr;
        while CurPtr < EndPtr do
        begin
          if CurPtr^ = #10 then
          begin
            LineLen := CurPtr - StartPtr;
            if (LineLen > 0) and (StartPtr[LineLen - 1] = #13) then
              Dec(LineLen);
            FeedLine(StartPtr, LineLen);
            Inc(CurPtr);
            StartPtr := CurPtr;
          end
          else
            Inc(CurPtr);
        end;
        if StartPtr < EndPtr then
          SetString(Tail, StartPtr, EndPtr - StartPtr)
        else
          Tail := '';
        Inc(BytesDone, ReadBytes);
        UpdateProgress;
      end;
      if Tail <> '' then
        FeedLine(PAnsiChar(Tail), Length(Tail));
      if EventBuf.Count > 0 then
        ProcessEvent(EventBuf);
      BytesDone := FileSize;
      UpdateProgress;
    finally
      FS.Free;
    end;
  finally
    EventBuf.Free;
  end;
end;

end.
