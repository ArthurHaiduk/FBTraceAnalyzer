{$HINTS OFF}
{$WARNINGS OFF}
unit TraceLogParser;

interface

uses
  SysUtils, Classes, StrUtils,
  Generics.Collections;

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

  TTraceLogParser = class
  private
    FSelCnt, FInsCnt, FUpdCnt, FDelCnt: Integer;
    FSelTot, FInsTot, FUpdTot, FDelTot: Double;
    FSelMax, FInsMax, FUpdMax, FDelMax: Double;

    FErrCnt: Integer;
    FErrTypeCnt: array [TErrorType] of Integer;
    FCommitCnt: Integer;
    FRollCnt: Integer;
    FRecCnt: Int64;

    FIdxMap: TDictionary<string, Integer>;

    procedure ResetStats;
    function GetAvgTimeSelect: Double;
    function GetAvgTimeInsert: Double;
    function GetAvgTimeUpdate: Double;
    function GetAvgTimeDelete: Double;

    procedure AddIndex(const AName: string);
    procedure SortIdx(var A: TIndexStatArray);
    function SafeStrToFloat(const S: string): Double;
    function ExtractMs(const S: string): Double;
    function ErrName(ET: TErrorType): string;
  public
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

    function ErrorTypes: TStringArray;
    function IndexUsage: TIndexStatArray;
    function TopUsedIndexes(ATop: Integer = 10): TStringArray;

    procedure ParseLog(const FileName: string;
      const AProgress: TProgressProc = nil);
  end;

implementation

procedure TTraceLogParser.ResetStats;
begin
  FSelCnt := 0;
  FInsCnt := 0;
  FUpdCnt := 0;
  FDelCnt := 0;
  FSelTot := 0;
  FInsTot := 0;
  FUpdTot := 0;
  FDelTot := 0;
  FSelMax := 0;
  FInsMax := 0;
  FUpdMax := 0;
  FDelMax := 0;

  FErrCnt := 0;
  FillChar(FErrTypeCnt, SizeOf(FErrTypeCnt), 0);
  FCommitCnt := 0;
  FRollCnt := 0;
  FRecCnt := 0;

  if Assigned(FIdxMap) then
    FIdxMap.Free;
  FIdxMap := TDictionary<string, Integer>.Create(1000);
end;

function TTraceLogParser.SafeStrToFloat(const S: string): Double;
var
  FS: TFormatSettings;
  Tmp: string;
  V: Double;
  ch: Char;
begin
  Tmp := '';
  for ch in S do
    if ch in ['0' .. '9', '.', ','] then
      Tmp := Tmp + ch;

  GetLocaleFormatSettings(0, FS);
  FS.DecimalSeparator := '.';
  Tmp := StringReplace(Tmp, ',', '.', [rfReplaceAll]);

  if TryStrToFloat(Tmp, V, FS) then
    Result := V
  else
    Result := 0;
end;

function TTraceLogParser.ExtractMs(const S: string): Double;
var
  p, i: Integer;
  num: string;
begin
  Result := -1;

  p := Pos(' ms', LowerCase(S));
  if p > 0 then
  begin
    i := p - 1;
    while (i > 0) and (S[i] in ['0' .. '9', '.', ',']) do
      Dec(i);
    num := Trim(Copy(S, i + 1, p - i - 1));
    Exit(SafeStrToFloat(num));
  end;

  p := Pos(' sec', LowerCase(S));
  if p = 0 then
    p := Pos(' s', LowerCase(S));
  if p > 0 then
  begin
    i := p - 1;
    while (i > 0) and (S[i] in ['0' .. '9', '.', ',', ' ']) do
      Dec(i);
    num := Trim(Copy(S, i + 1, p - i - 1));
    Exit(SafeStrToFloat(num) * 1000);
  end;
end;

procedure TTraceLogParser.AddIndex(const AName: string);
var
  cnt: Integer;
begin
  if AName = '' then
    Exit;
  if FIdxMap.TryGetValue(AName, cnt) then
    FIdxMap[AName] := cnt + 1
  else
    FIdxMap.Add(AName, 1);
end;

procedure TTraceLogParser.SortIdx(var A: TIndexStatArray);
var
  i, j: Integer;
  t: TIndexStat;
begin
  for i := Low(A) to High(A) - 1 do
    for j := i + 1 to High(A) do
      if A[j].Count > A[i].Count then
      begin
        t := A[i];
        A[i] := A[j];
        A[j] := t;
      end;
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
  Pair: TPair<string, Integer>;
  i: Integer;
begin
  SetLength(Result, FIdxMap.Count);
  i := 0;
  for Pair in FIdxMap do
  begin
    Result[i].Name := Pair.Key;
    Result[i].Count := Pair.Value;
    Inc(i);
  end;
end;

function TTraceLogParser.TopUsedIndexes(ATop: Integer): TStringArray;
var
  Tmp: TIndexStatArray;
  i, c: Integer;
begin
  Tmp := IndexUsage;
  if Length(Tmp) = 0 then
    Exit;
  SortIdx(Tmp);
  if ATop < 1 then
    ATop := 1;
  c := Length(Tmp);
  if ATop < c then
    c := ATop;

  SetLength(Result, c);
  for i := 0 to c - 1 do
    Result[i] := Tmp[i].Name;
end;

procedure TTraceLogParser.ParseLog(const FileName: string;
  const AProgress: TProgressProc);
var
  R: TStreamReader;
  Line: string;
  EventBuf: TStringList;
  BytesRead: Int64;
  LastPercent: Integer;

  procedure UpdateProgress;
  var
    p: Integer;
  begin
    if not Assigned(AProgress) or (R.BaseStream.Size = 0) then
      Exit;

    p := (BytesRead * 100) div R.BaseStream.Size;
    if p <> LastPercent then
    begin
      LastPercent := p;
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

  procedure ProcessEvent(const L: TStringList);
  var
    hdr, sqlType, planLine, idxLine: string;
    i, j, p1, p2: Integer;
    ms: Double;
    ET: TErrorType;
    S: string;
  begin
    if L.Count = 0 then
      Exit;
    hdr := L[0];

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

    if (Pos('ERROR', hdr) > 0) or (Pos('FAILED', hdr) > 0) then
    begin
      Inc(FErrCnt);
      ET := etGeneral;
      for S in L do
      begin
        if ContainsText(S, 'deadlock') then
          ET := etDeadlock
        else if ContainsText(S, 'foreign') and ContainsText(S, 'key') then
          ET := etForeignKey
        else if ContainsText(S, 'unique') or ContainsText(S, 'duplicate') then
          ET := etUniqueKey
        else if ContainsText(S, ' null ') then
          ET := etNotNull
        else if ContainsText(S, 'lock conflict') then
          ET := etLockConflict;
      end;
      Inc(FErrTypeCnt[ET]);
      Exit;
    end;

    if Pos('EXECUTE_STATEMENT_FINISH', hdr) = 0 then
      Exit;

    sqlType := '';
    for i := 1 to L.Count - 1 do
      if Pos('Statement ', L[i]) = 1 then
      begin
        j := i + 1;
        while (j < L.Count) and ((Trim(L[j]) = '') or (L[j][1] in ['-', '^']))
          do
          Inc(j);
        if j < L.Count then
          sqlType := UpperCase(Copy(Trim(L[j]), 1,
              Pos(' ', Trim(L[j]) + ' ') - 1));
        Break;
      end;
    if sqlType = '' then
      Exit;

    planLine := '';
    for i := 1 to L.Count - 1 do
      if Pos('PLAN', L[i]) = 1 then
      begin
        planLine := L[i];
        Break;
      end;

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
        idxLine := Copy(planLine, p1 + 1, p2 - p1 - 1);
        repeat
          idxLine := Trim(idxLine);
          if idxLine = '' then
            Break;
          p2 := Pos(',', idxLine);
          if p2 = 0 then
          begin
            AddIndex(idxLine);
            idxLine := '';
          end
          else
          begin
            AddIndex(Trim(Copy(idxLine, 1, p2 - 1)));
            idxLine := Trim(Copy(idxLine, p2 + 1, MaxInt));
          end;
        until idxLine = '';
        p1 := PosEx('INDEX', planLine, p2);
      end;
    end;

    ms := -1;
    for i := L.Count - 1 downto 0 do
      if (Pos(' ms', L[i]) > 0) or (Pos(' sec', L[i]) > 0) or
        (Pos(' s', L[i]) > 0) then
      begin
        ms := ExtractMs(L[i]);
        Break;
      end;

    if ms < 0 then
      ms := ExtractMs(hdr);

    for i := L.Count - 1 downto 0 do
      if Pos(' records fetched', LowerCase(L[i])) > 0 then
      begin
        j := Pos(' records fetched', LowerCase(L[i])) - 1;
        while (j > 1) and (L[i][j] = ' ') do
          Dec(j);
        p1 := j;
        while (p1 > 1) and (L[i][p1 - 1] in ['0' .. '9']) do
          Dec(p1);
        Inc(FRecCnt, StrToIntDef(Copy(L[i], p1, j - p1 + 1), 0));
        Break;
      end;

    if sqlType = 'SELECT' then
    begin
      Inc(FSelCnt);
      if ms >= 0 then
      begin
        FSelTot := FSelTot + ms;
        if ms > FSelMax then
          FSelMax := ms;
      end;
    end
    else if sqlType = 'INSERT' then
    begin
      Inc(FInsCnt);
      if ms >= 0 then
      begin
        FInsTot := FInsTot + ms;
        if ms > FInsMax then
          FInsMax := ms;
      end;
    end
    else if sqlType = 'UPDATE' then
    begin
      Inc(FUpdCnt);
      if ms >= 0 then
      begin
        FUpdTot := FUpdTot + ms;
        if ms > FUpdMax then
          FUpdMax := ms;
      end;
    end
    else if sqlType = 'DELETE' then
    begin
      Inc(FDelCnt);
      if ms >= 0 then
      begin
        FDelTot := FDelTot + ms;
        if ms > FDelMax then
          FDelMax := ms;
      end;
    end;
  end;

begin
  ResetStats;
  LastPercent := -1;

  R := TStreamReader.Create(FileName, TEncoding.Default, False, 1 shl 20);
  // 1 МБ буфер
  EventBuf := TStringList.Create;
  try
    BytesRead := 0;
    while not R.EndOfStream do
    begin
      Line := R.ReadLine;
      BytesRead := R.BaseStream.Position;
      UpdateProgress;

      if IsEventStart(Line) then
      begin
        if EventBuf.Count > 0 then
          ProcessEvent(EventBuf);
        EventBuf.Clear;
      end;

      EventBuf.Add(Line);
    end;

    if EventBuf.Count > 0 then
      ProcessEvent(EventBuf);
    BytesRead := R.BaseStream.Size;
    UpdateProgress;
  finally
    EventBuf.Free;
    R.Free;
  end;
end;

end.
