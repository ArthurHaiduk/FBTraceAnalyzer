unit TraceLogParser;

interface

uses
  SysUtils, Classes, StrUtils, Generics.Collections,
  ITraceLogParser, uTraceContracts, Generics.Defaults;

type
  TTraceLogParser = class
  private type
    TStmtBuild = record
      Id: Integer;
      SQL: string;
      Plan: string;
      Params: string;
      Perf: string;
      Rows: Int64;
      Ms: Double;
      ExecAt: TDateTime;
      AppExe: string;
      Client: string;
      TxnId: Integer;
      procedure Clear;
    end;

  var
    FSlowList: TList<TSlowStmt>;
    FActive: TDictionary<Integer, TStmtBuild>;
    FIdxUsage: TDictionary<string, Integer>;
    FErrTypes: TDictionary<string, Integer>;
    FCountSel, FCountIns, FCountUpd, FCountDel: Int64;
    FSumSel, FSumIns, FSumUpd, FSumDel: Double;
    FMaxSel, FMaxIns, FMaxUpd, FMaxDel: Double;
    FCommits, FRollbacks, FErrors: Integer;
    FTailFile: string;
    FTailPos: Int64;
    FTailCarry: string;
    FSynthId: Integer;
    FLastFP: string;
    FLastAt: TDateTime;
    FLastKey: string;
    procedure ResetAll;
    class function MakeTxnKey(const TxnId: Integer;
      const SQL, App, Client: string): string; static;
    procedure RevokeFromTotals(const SQLType: string; const TimeMs: Double);
    class function SkipLeadNoise(const S: string): string; static;
    class function NextVerbAfterWith(const S: string): string; static;
    class function CollapseWS(const S: string): string; static;
    class function CanonSql(const S: string): string; static;
    class function MakeFingerprint(const SQL, Plan, Params, App, Client: string)
      : string; static;
    class function StartsWith(const S, Prefix: string): Boolean; static;
    class function ExtractIntDef32(const S: string; Def: Integer): Integer;
      static;
    class function ExtractIntDef64(const S: string; Def: Int64): Int64; static;
    class function ExtractIsoDT(const S: string; out DT: TDateTime): Boolean;
      static;

    class function ExtractMsFromPerf(const Perf: string): Double; static;
    class function ExtractRowsFromPerf(const Perf: string): Int64; static;

    class function SqlTypeOf(const SQL: string): string; static;
    class function ContainsNatural(const Plan: string): Boolean; static;
    class procedure CollectIndexUsage(const Plan: string;
      Dict: TDictionary<string, Integer>); static;

    procedure DictSetInt(var D: TDictionary<string, Integer>; const K: string;
      Delta: Integer);

    procedure FinishStatement(const B: TStmtBuild);
    procedure ProcessChunk(const Text: string; Reset: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ParseLog(const FileName: string; const Progress: TProc<Int64,
      Int64>);
    procedure TailBegin(const FileName: string; StartFromEnd: Boolean);
    function TailPoll: Boolean;

    function TopSlow(const MaxItems: Integer): TSlowArray;
    function IndexUsage: ITraceLogParser.TIndexStatArray;
    function ErrorTypes: ITraceLogParser.TStringArray;
    function RecordCount: Integer;

    property CommitCount: Integer read FCommits;
    property RollbackCount: Integer read FRollbacks;
    property ErrorCount: Integer read FErrors;

    property CountSelect: Int64 read FCountSel;
    property CountInsert: Int64 read FCountIns;
    property CountUpdate: Int64 read FCountUpd;
    property CountDelete: Int64 read FCountDel;

    property TotalTimeSelect: Double read FSumSel;
    property TotalTimeInsert: Double read FSumIns;
    property TotalTimeUpdate: Double read FSumUpd;
    property TotalTimeDelete: Double read FSumDel;

    property MaxTimeSelect: Double read FMaxSel;
    property MaxTimeInsert: Double read FMaxIns;
    property MaxTimeUpdate: Double read FMaxUpd;
    property MaxTimeDelete: Double read FMaxDel;

    function AvgTimeSelect: Double;
    function AvgTimeInsert: Double;
    function AvgTimeUpdate: Double;
    function AvgTimeDelete: Double;

    function MakeSnapshot(const FileName: string): ITraceSnapshot;
    procedure ApplySnapshot(const Snap: ITraceSnapshot);
  end;

implementation

class function TTraceLogParser.SkipLeadNoise(const S: string): string;
var
  i, L: Integer;
begin
  i := 1;
  L := Length(S);
  while i <= L do
  begin
    while (i <= L) and (S[i] <= ' ') do
      Inc(i);
    if (i <= L - 1) and (S[i] = '-') and (S[i + 1] = '-') then
    begin
      Inc(i, 2);
      while (i <= L) and not(S[i] in [#10, #13]) do
        Inc(i);
      Continue;
    end;
    if (i <= L - 1) and (S[i] = '/') and (S[i + 1] = '*') then
    begin
      Inc(i, 2);
      while (i <= L - 1) and not((S[i] = '*') and (S[i + 1] = '/')) do
        Inc(i);
      if i <= L - 1 then
        Inc(i, 2);
      Continue;
    end;
    Break;
  end;
  Result := Trim(Copy(S, i, MaxInt));
end;

class function TTraceLogParser.NextVerbAfterWith(const S: string): string;
var
  i, L, lvl: Integer;
  T: string;
begin
  T := S;
  i := 5;
  L := Length(T);
  while (i <= L) and (T[i] <= ' ') do
    Inc(i);
  while i <= L do
  begin
    while (i <= L) and (T[i] in ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '_', '$'])
      do
      Inc(i);
    while (i <= L) and (T[i] <= ' ') do
      Inc(i);
    if (i <= L - 1) and ((T[i] = 'A') or (T[i] = 'a')) and
      ((i + 1 <= L) and ((T[i + 1] = 'S') or (T[i + 1] = 's'))) then
    begin
      Inc(i, 2);
      while (i <= L) and (T[i] <= ' ') do
        Inc(i);
      if (i <= L) and (T[i] = '(') then
      begin
        lvl := 1;
        Inc(i);
        while (i <= L) and (lvl > 0) do
        begin
          if T[i] = '(' then
            Inc(lvl)
          else if T[i] = ')' then
            Dec(lvl);
          Inc(i);
        end;
      end;
      while (i <= L) and (T[i] <= ' ') do
        Inc(i);
      if (i <= L) and (T[i] = ',') then
      begin
        Inc(i);
        while (i <= L) and (T[i] <= ' ') do
          Inc(i);
        Continue;
      end
      else
        Break;
    end
    else
      Break;
  end;
  Result := Trim(Copy(T, i, MaxInt));
end;

class function TTraceLogParser.CollapseWS(const S: string): string;
var
  i: Integer;
  ch: Char;
  was: Boolean;
  r: string;
begin
  r := '';
  was := False;
  for i := 1 to Length(S) do
  begin
    ch := S[i];
    if ch in [#9, #10, #13, ' '] then
    begin
      if not was then
      begin
        r := r + ' ';
        was := True;
      end;
    end
    else
    begin
      r := r + ch;
      was := False;
    end;
  end;
  Result := Trim(r);
end;

class function TTraceLogParser.CanonSql(const S: string): string;
var
  T: string;
begin
  T := Trim(S);
  if (T <> '') and (T[Length(T)] = ';') then
    Delete(T, Length(T), 1);
  Result := CollapseWS(T);
end;

class function TTraceLogParser.MakeTxnKey(const TxnId: Integer;
  const SQL, App, Client: string): string;
begin
  Result := IntToStr(TxnId) + '|' + CanonSql(SQL) + '|' + Trim(App) + '|' + Trim
    (Client);
end;

procedure TTraceLogParser.RevokeFromTotals(const SQLType: string;
  const TimeMs: Double);
begin
  if SQLType = 'SELECT' then
  begin
    Dec(FCountSel);
    FSumSel := FSumSel - TimeMs;
  end
  else if SQLType = 'INSERT' then
  begin
    Dec(FCountIns);
    FSumIns := FSumIns - TimeMs;
  end
  else if SQLType = 'UPDATE' then
  begin
    Dec(FCountUpd);
    FSumUpd := FSumUpd - TimeMs;
  end
  else if SQLType = 'DELETE' then
  begin
    Dec(FCountDel);
    FSumDel := FSumDel - TimeMs;
  end;
end;

class function TTraceLogParser.MakeFingerprint(const SQL, Plan, Params, App,
  Client: string): string;
begin
  Result := CanonSql(SQL) + '|' + CollapseWS(Plan) + '|' + CollapseWS(Params)
    + '|' + Trim(App) + '|' + Trim(Client);
end;

procedure TTraceLogParser.TStmtBuild.Clear;
begin
  Id := -1;
  SQL := '';
  Plan := '';
  Params := '';
  Perf := '';
  Rows := 0;
  Ms := 0;
  ExecAt := 0;
  AppExe := '';
  Client := '';
  TxnId := -1;
end;

type
  TSlowStmtComparer = class(TComparer<TSlowStmt>)
  public
    function Compare(const A, B: TSlowStmt): Integer; override;
  end;

function TSlowStmtComparer.Compare(const A, B: TSlowStmt): Integer;
begin
  if A.TimeMs < B.TimeMs then
    Result := 1
  else if A.TimeMs > B.TimeMs then
    Result := -1
  else
    Result := 0;
end;

class function TTraceLogParser.StartsWith(const S, Prefix: string): Boolean;
begin
  Result := (Length(S) >= Length(Prefix)) and
    (CompareText(Copy(S, 1, Length(Prefix)), Prefix) = 0);
end;

class function TTraceLogParser.ExtractIntDef32(const S: string;
  Def: Integer): Integer;
var
  V, Code: Integer;
begin
  Val(S, V, Code);
  if Code = 0 then
    Result := V
  else
    Result := Def;
end;

class function TTraceLogParser.ExtractIntDef64(const S: string;
  Def: Int64): Int64;
var
  V: Int64;
  Code: Integer;
begin
  Val(S, V, Code);
  if Code = 0 then
    Result := V
  else
    Result := Def;
end;

class function TTraceLogParser.ExtractIsoDT(const S: string;
  out DT: TDateTime): Boolean;
var
  y, mo, D, hh, mi, ss, Ms: Word;
  i, j, Val, scale: Integer;
  frac: string;
  tdate, ttime: TDateTime;
begin
  Result := False;
  if (Length(S) < 19) or (Pos('T', S) <> 11) then
    Exit;
  y := ExtractIntDef32(Copy(S, 1, 4), -1);
  mo := ExtractIntDef32(Copy(S, 6, 2), -1);
  D := ExtractIntDef32(Copy(S, 9, 2), -1);
  hh := ExtractIntDef32(Copy(S, 12, 2), -1);
  mi := ExtractIntDef32(Copy(S, 15, 2), -1);
  ss := ExtractIntDef32(Copy(S, 18, 2), -1);
  if (y < 1) or (mo < 1) or (D < 1) or (hh > 23) or (mi > 59) or (ss > 59) then
    Exit;
  Ms := 0;
  if (Length(S) >= 21) and ((S[20] = '.') or (S[20] = ',')) then
  begin
    i := 21;
    j := i;
    while (j <= Length(S)) and (S[j] in ['0' .. '9']) do
      Inc(j);
    frac := Copy(S, i, j - i);
    if frac <> '' then
    begin
      Val := ExtractIntDef32(frac, 0);
      scale := 1;
      for i := 1 to Length(frac) do
        scale := scale * 10;
      Ms := Round((Val / scale) * 1000.0);
      if Ms > 999 then
        Ms := 999;
    end;
  end;
  if not TryEncodeDate(y, mo, D, tdate) then
    Exit;
  if not TryEncodeTime(hh, mi, ss, Ms, ttime) then
    Exit;
  DT := tdate + ttime;
  Result := True;
end;

class function TTraceLogParser.ExtractMsFromPerf(const Perf: string): Double;
var
  SL: TStringList;
  i, P, K: Integer;
  L, Dig: string;
begin
  Result := 0;
  SL := TStringList.Create;
  try
    SL.Text := Perf;
    for i := 0 to SL.Count - 1 do
    begin
      L := Trim(SL[i]);
      P := Pos(' ms', L);
      if P > 1 then
      begin
        K := P - 1;
        while (K >= 1) and (L[K] = ' ') do
          Dec(K);
        Dig := '';
        while (K >= 1) and (L[K] in ['0' .. '9', '.']) do
        begin
          Dig := L[K] + Dig;
          Dec(K);
        end;
        Result := StrToFloatDef(StringReplace(Dig, ',', '.', [rfReplaceAll]),
          0);
        Exit;
      end;
    end;
  finally
    SL.Free;
  end;
end;

class function TTraceLogParser.ExtractRowsFromPerf(const Perf: string): Int64;
var
  SL: TStringList;
  i, P, K: Integer;
  L, Dig, LL: string;
begin
  Result := 0;
  SL := TStringList.Create;
  try
    SL.Text := Perf;
    for i := 0 to SL.Count - 1 do
    begin
      LL := LowerCase(Trim(SL[i]));
      P := Pos(' records fetched', LL);
      if P > 0 then
      begin
        K := P - 1;
        while (K >= 1) and (LL[K] = ' ') do
          Dec(K);
        Dig := '';
        while (K >= 1) and (LL[K] in ['0' .. '9']) do
        begin
          Dig := LL[K] + Dig;
          Dec(K);
        end;
        Result := StrToInt64Def(Dig, 0);
        Exit;
      end;
      P := Pos(' fetch(es)', LL);
      if P > 0 then
      begin
        K := P - 1;
        while (K >= 1) and (LL[K] = ' ') do
          Dec(K);
        Dig := '';
        while (K >= 1) and (LL[K] in ['0' .. '9']) do
        begin
          Dig := LL[K] + Dig;
          Dec(K);
        end;
        Result := StrToInt64Def(Dig, 0);
        Exit;
      end;
      P := Pos(' rows affected', LL);
      if P > 0 then
      begin
        K := P - 1;
        while (K >= 1) and (LL[K] = ' ') do
          Dec(K);
        Dig := '';
        while (K >= 1) and (LL[K] in ['0' .. '9']) do
        begin
          Dig := LL[K] + Dig;
          Dec(K);
        end;
        Result := StrToInt64Def(Dig, 0);
        Exit;
      end;
    end;
  finally
    SL.Free;
  end;
end;

class function TTraceLogParser.SqlTypeOf(const SQL: string): string;
var
  T, U: string;
begin
  T := SkipLeadNoise(SQL);
  if (Length(T) >= 4) and ((T[1] = 'w') or (T[1] = 'W')) and
    ((Length(T) >= 4) and (LowerCase(Copy(T, 1, 4)) = 'with')) then
  begin
    T := NextVerbAfterWith(T);
    T := SkipLeadNoise(T);
  end;
  U := LowerCase(T);
  if StartsText('select', U) then
    Exit('SELECT');
  if StartsText('insert', U) then
    Exit('INSERT');
  if StartsText('update or insert', U) then
    Exit('INSERT');
  if StartsText('update', U) then
    Exit('UPDATE');
  if StartsText('delete', U) then
    Exit('DELETE');
  if StartsText('merge', U) then
    Exit('MERGE');
  if StartsText('execute procedure', U) then
    Exit('EXEC_PROC');
  if StartsText('execute block', U) then
    Exit('EXEC_PROC');
  Result := 'OTHER';
end;

class function TTraceLogParser.ContainsNatural(const Plan: string): Boolean;
begin
  Result := Pos(' NATURAL', UpperCase(' ' + Plan)) > 0;
end;

class procedure TTraceLogParser.CollectIndexUsage(const Plan: string;
  Dict: TDictionary<string, Integer>);
var
  Up: string;
  lenUp, posPtr, iOpen, iClose, level, K: Integer;
  ch: Char;
  tok: string;
begin
  Up := UpperCase(Plan);
  lenUp := Length(Up);
  posPtr := 1;
  while posPtr <= lenUp do
  begin
    iOpen := PosEx('INDEX', Up, posPtr);
    if iOpen = 0 then
      Break;
    iOpen := PosEx('(', Up, iOpen + 5);
    if iOpen = 0 then
    begin
      Inc(posPtr, 6);
      Continue;
    end;
    level := 1;
    iClose := iOpen + 1;
    while (iClose <= lenUp) and (level > 0) do
    begin
      ch := Up[iClose];
      if ch = '(' then
        Inc(level)
      else if ch = ')' then
        Dec(level);
      Inc(iClose);
    end;
    if level <> 0 then
    begin
      posPtr := iOpen + 1;
      Continue;
    end;
    tok := '';
    for K := iOpen + 1 to iClose - 2 do
    begin
      ch := Up[K];
      if (ch in ['A' .. 'Z', '0' .. '9', '_', '$']) then
        tok := tok + ch
      else
      begin
        if tok <> '' then
        begin
          if not Dict.ContainsKey(tok) then
            Dict.Add(tok, 1)
          else
            Dict.Items[tok] := Dict.Items[tok] + 1;
          tok := '';
        end;
      end;
    end;
    if tok <> '' then
    begin
      if not Dict.ContainsKey(tok) then
        Dict.Add(tok, 1)
      else
        Dict.Items[tok] := Dict.Items[tok] + 1;
    end;
    posPtr := iClose;
  end;
end;

procedure TTraceLogParser.DictSetInt(var D: TDictionary<string, Integer>;
  const K: string; Delta: Integer);
begin
  if not D.ContainsKey(K) then
    D.Add(K, Delta)
  else
    D.Items[K] := D.Items[K] + Delta;
end;

procedure TTraceLogParser.FinishStatement(const B: TStmtBuild);
const
  DEDUP_WINDOW_SEC = 2;
var
  r, Last: TSlowStmt;
  Key: string;
  Secs: Integer;
  ReplaceLast, IgnoreDup: Boolean;
begin
  if B.Id < 0 then
    Exit;
  if (Trim(B.SQL) = '') and (Trim(B.Plan) = '') and (Trim(B.Params) = '') and
    (Trim(B.Perf) = '') then
    Exit;

  Key := MakeTxnKey(B.TxnId, B.SQL, B.AppExe, B.Client);
  Secs := 0;
  ReplaceLast := False;
  IgnoreDup := False;

  if (FSlowList.Count > 0) and (Key = FLastKey) then
  begin
    Last := FSlowList[FSlowList.Count - 1];
    if (B.ExecAt > 0) and (Last.ExecAt > 0) then
      Secs := Round(Abs((B.ExecAt - Last.ExecAt) * 24 * 60 * 60));
    if Secs <= DEDUP_WINDOW_SEC then
    begin
      if ((B.Params <> '') and (Last.ParamText = '')) or
        ((B.Ms > 0) and (Last.TimeMs <= 0)) or
        ((B.Perf <> '') and (Last.PerfText = '')) then
        ReplaceLast := True
      else
        IgnoreDup := True;
    end;
  end;

  if IgnoreDup and (Secs <= DEDUP_WINDOW_SEC) then
    Exit;

  if ReplaceLast then
  begin
    RevokeFromTotals(Last.SQLType, Last.TimeMs);
    FSlowList.Delete(FSlowList.Count - 1);
  end;

  r.TimeMs := B.Ms;
  r.Rows := B.Rows;
  r.SQLType := SqlTypeOf(B.SQL);
  r.PlanNat := ContainsNatural(B.Plan);
  r.SQLText := TrimRight(B.SQL);
  r.PlanText := TrimRight(B.Plan);
  r.ParamText := TrimRight(B.Params);
  r.PerfText := TrimRight(B.Perf);
  r.ExecAt := B.ExecAt;
  r.AppExe := B.AppExe;
  r.Client := B.Client;

  CollectIndexUsage(r.PlanText, FIdxUsage);

  if r.SQLType = 'SELECT' then
  begin
    Inc(FCountSel);
    FSumSel := FSumSel + r.TimeMs;
    if r.TimeMs > FMaxSel then
      FMaxSel := r.TimeMs;
  end
  else if r.SQLType = 'INSERT' then
  begin
    Inc(FCountIns);
    FSumIns := FSumIns + r.TimeMs;
    if r.TimeMs > FMaxIns then
      FMaxIns := r.TimeMs;
  end
  else if r.SQLType = 'UPDATE' then
  begin
    Inc(FCountUpd);
    FSumUpd := FSumUpd + r.TimeMs;
    if r.TimeMs > FMaxUpd then
      FMaxUpd := r.TimeMs;
  end
  else if r.SQLType = 'DELETE' then
  begin
    Inc(FCountDel);
    FSumDel := FSumDel + r.TimeMs;
    if r.TimeMs > FMaxDel then
      FMaxDel := r.TimeMs;
  end;

  FSlowList.Add(r);
  FLastKey := Key;
  FLastAt := B.ExecAt;
end;

constructor TTraceLogParser.Create;
begin
  inherited Create;
  FSlowList := TList<TSlowStmt>.Create;
  FActive := TDictionary<Integer, TStmtBuild>.Create;
  FIdxUsage := TDictionary<string, Integer>.Create;
  FErrTypes := TDictionary<string, Integer>.Create;
  FSynthId := 0;
end;

destructor TTraceLogParser.Destroy;
begin
  FErrTypes.Free;
  FIdxUsage.Free;
  FActive.Free;
  FSlowList.Free;
  inherited;
end;

procedure TTraceLogParser.ResetAll;
begin
  FSlowList.Clear;
  FActive.Clear;
  FIdxUsage.Clear;
  FErrTypes.Clear;
  FCountSel := 0;
  FCountIns := 0;
  FCountUpd := 0;
  FCountDel := 0;
  FSumSel := 0;
  FSumIns := 0;
  FSumUpd := 0;
  FSumDel := 0;
  FMaxSel := 0;
  FMaxIns := 0;
  FMaxUpd := 0;
  FMaxDel := 0;
  FCommits := 0;
  FRollbacks := 0;
  FErrors := 0;
  FTailFile := '';
  FTailPos := 0;
  FTailCarry := '';
  FSynthId := 0;
  FLastKey := '';
  FLastAt := 0;
end;

procedure TTraceLogParser.ProcessChunk(const Text: string; Reset: Boolean);
var
  L, S, P, E: Integer;
  Line, TrimLine, U, AfterTS: string;
  CurId: Integer;
  B: TStmtBuild;
  HasB: Boolean;
  InSQL, InPlan, InParams, InPerf, ExpectSQLAfterDash, InHeader: Boolean;
  LastDT: TDateTime;
  LastApp, LastClient: string;
  LastTxn: Integer;

  function IsTSHeader(const ALine: string; out EventUpper: string): Boolean;
  var
    DT: TDateTime;
    ip: Integer;
    tail, hdr: string;
  begin
    Result := False;
    EventUpper := '';
    if Length(ALine) < 19 then
      Exit;
    hdr := Copy(ALine, 1, 23);
    if not ExtractIsoDT(hdr, DT) then
    begin
      hdr := Copy(ALine, 1, 19);
      if not ExtractIsoDT(hdr, DT) then
        Exit;
    end;
    ip := LastDelimiter(')', ALine);
    if ip > 0 then
      tail := Trim(Copy(ALine, ip + 1, MaxInt))
    else
      tail := Trim(Copy(ALine, 20, MaxInt));
    EventUpper := UpperCase(tail);
    LastDT := DT;
    InHeader := True;
    Result := True;
  end;

  function IsDashLine(const ALine: string): Boolean;
  var
    i, c: Integer;
  begin
    c := 0;
    for i := 1 to Length(ALine) do
      if ALine[i] = '-' then
        Inc(c)
      else if not(ALine[i] in [#9, #32]) then
      begin
        c := 0;
        Break;
      end;
    Result := c >= 5;
  end;

  function IsCaretLine(const ALine: string): Boolean;
  var
    i, c: Integer;
  begin
    c := 0;
    for i := 1 to Length(ALine) do
      if ALine[i] = '^' then
        Inc(c)
      else if not(ALine[i] in [#9, #32]) then
      begin
        c := 0;
        Break;
      end;
    Result := c >= 5;
  end;

  function ParseStmtId(const ALine: string): Integer;
  var
    i, j: Integer;
    S: string;
  begin
    Result := -1;
    if not StartsText('Statement', Trim(ALine)) then
      Exit;
    S := Trim(Copy(Trim(ALine), 10, MaxInt));
    i := 1;
    while (i <= Length(S)) and (S[i] = ' ') do
      Inc(i);
    j := i;
    while (j <= Length(S)) and (S[j] in ['0' .. '9']) do
      Inc(j);
    if j > i then
      Result := ExtractIntDef32(Copy(S, i, j - i), -1);
  end;

  function ExtractTxnId(const ALine: string): Integer;
  var
    P, q: Integer;
    S: string;
  begin
    Result := -1;
    P := Pos('(TRA_', UpperCase(ALine));
    if P = 0 then
      Exit;
    P := P + 5;
    q := P;
    while (q <= Length(ALine)) and (ALine[q] in ['0' .. '9']) do
      Inc(q);
    S := Copy(ALine, P, q - P);
    Result := StrToIntDef(S, -1);
  end;

  function ExtractClient(const ALine: string): string;
  var
    P, q, E, r: Integer;
    S: string;
  begin
    Result := '';
    P := Pos('TCP', UpperCase(ALine));
    if P = 0 then
      P := Pos('XNET', UpperCase(ALine));
    if P = 0 then
      Exit;
    q := PosEx(':', ALine, P);
    if q = 0 then
      Exit;
    E := q + 1;
    while (E <= Length(ALine)) and not(ALine[E] in [')', ' ', #9, ',', ';']) do
      Inc(E);
    S := Copy(ALine, q + 1, E - q - 1);
    r := Pos('/', S);
    if r > 0 then
      S := Copy(S, 1, r - 1);
    Result := Trim(S);
  end;

  function ExtractApp(const ALine: string): string;
  var
    S: string;
    P: Integer;
  begin
    Result := '';
    S := Trim(ALine);
    if Pos('.exe', LowerCase(S)) = 0 then
      Exit;
    P := LastDelimiter(':', S);
    if (P > 2) and (Copy(S, P + 1, MaxInt) = IntToStr(StrToIntDef(Copy(S,
            P + 1, MaxInt), -1))) then
      Result := Copy(S, 1, P - 1)
    else
      Result := S;
  end;

  procedure EnsureActive;
  begin
    if CurId < 0 then
    begin
      Inc(FSynthId);
      CurId := FSynthId;
      B.Clear;
      B.Id := CurId;
      B.ExecAt := LastDT;
      B.AppExe := LastApp;
      B.Client := LastClient;
      B.TxnId := LastTxn;
      if FActive.ContainsKey(CurId) then
        FActive.Items[CurId] := B
      else
        FActive.Add(CurId, B);
      InSQL := True;
      InPlan := False;
      InParams := False;
      InPerf := False;
    end;
  end;

  procedure StartStmt(const AId: Integer);
  begin
    CurId := AId;
    HasB := FActive.TryGetValue(CurId, B);
    if not HasB then
    begin
      B.Clear;
      B.Id := CurId;
      B.ExecAt := LastDT;
      B.AppExe := LastApp;
      B.Client := LastClient;
      B.TxnId := LastTxn;
      FActive.Add(CurId, B);
    end;
    InSQL := False;
    InPlan := False;
    InParams := False;
    InPerf := False;
    ExpectSQLAfterDash := True;
  end;

  procedure UpdateB;
  begin
    if FActive.ContainsKey(CurId) then
      FActive.Items[CurId] := B
    else
      FActive.Add(CurId, B);
  end;

  procedure FinishCur;
  begin
    if (CurId >= 0) and FActive.TryGetValue(CurId, B) then
    begin
      if (B.Ms <= 0) and (B.Perf <> '') then
        B.Ms := ExtractMsFromPerf(B.Perf);
      if (B.Rows = 0) and (B.Perf <> '') then
        B.Rows := ExtractRowsFromPerf(B.Perf);
      FinishStatement(B);
      FActive.Remove(CurId);
    end;
    CurId := -1;
    InSQL := False;
    InPlan := False;
    InParams := False;
    InPerf := False;
    ExpectSQLAfterDash := False;
  end;

begin
  if Reset then
    ResetAll;

  CurId := -1;
  InSQL := False;
  InPlan := False;
  InParams := False;
  InPerf := False;
  ExpectSQLAfterDash := False;
  InHeader := False;
  LastDT := 0;
  LastApp := '';
  LastClient := '';
  LastTxn := -1;

  L := Length(Text);
  S := 1;
  while S <= L do
  begin
    P := S;
    while (P <= L) and (Text[P] <> #10) and (Text[P] <> #13) do
      Inc(P);
    E := P - 1;
    if E >= S then
      Line := Copy(Text, S, E - S + 1)
    else
      Line := '';
    if (P <= L) and (Text[P] = #13) and (P < L) and (Text[P + 1] = #10) then
      Inc(P, 2)
    else if P <= L then
      Inc(P);
    S := P;

    TrimLine := Trim(Line);
    U := UpperCase(TrimLine);

    if IsTSHeader(Line, AfterTS) then
    begin
      if Pos('COMMIT', AfterTS) > 0 then
        Inc(FCommits);
      if Pos('ROLLBACK', AfterTS) > 0 then
        Inc(FRollbacks);
      if (Pos('ERROR', AfterTS) > 0) or (Pos('EXCEPTION', AfterTS) > 0) then
      begin
        Inc(FErrors);
        DictSetInt(FErrTypes, AfterTS, 1);
      end;
      FinishCur;
      Continue;
    end;

    if InHeader then
    begin
      if IsDashLine(Line) then
      begin
        InHeader := False;
        ExpectSQLAfterDash := True;
        Continue;
      end;
      if LastClient = '' then
        LastClient := ExtractClient(Line);
      if LastApp = '' then
        LastApp := ExtractApp(Line);
      if LastTxn = -1 then
        LastTxn := ExtractTxnId(Line);
      Continue;
    end;

    if StartsText('Statement', TrimLine) then
    begin
      FinishCur;
      StartStmt(ParseStmtId(TrimLine));
      Continue;
    end;

    if IsDashLine(Line) then
    begin
      if CurId >= 0 then
        ExpectSQLAfterDash := True;
      Continue;
    end;

    if IsCaretLine(Line) then
      Continue;

    if StartsText('PLAN', TrimLine) or StartsText('PLAN(', TrimLine) then
    begin
      EnsureActive;
      if FActive.TryGetValue(CurId, B) then
      begin
        B.Plan := B.Plan + Line + sLineBreak;
        UpdateB;
      end;
      InSQL := False;
      InPlan := True;
      InParams := False;
      InPerf := False;
      ExpectSQLAfterDash := False;
      Continue;
    end;

    if StartsText('PARAM', TrimLine) or StartsText('BIND', TrimLine)
      or StartsText('VARIABLE', TrimLine) then
    begin
      EnsureActive;
      if FActive.TryGetValue(CurId, B) then
      begin
        B.Params := B.Params + Line + sLineBreak;
        UpdateB;
      end;
      InSQL := False;
      InPlan := False;
      InParams := True;
      InPerf := False;
      ExpectSQLAfterDash := False;
      Continue;
    end;

    if (Pos(' ms', TrimLine) > 0) or (Pos('RECORDS FETCHED', U) > 0) or
      (Pos('FETCH(ES)', U) > 0) or (Pos('ROWS AFFECTED', U) > 0) then
    begin
      EnsureActive;
      if FActive.TryGetValue(CurId, B) then
      begin
        B.Perf := B.Perf + Line + sLineBreak;
        if B.Ms <= 0 then
          B.Ms := ExtractMsFromPerf(B.Perf);
        if B.Rows = 0 then
          B.Rows := ExtractRowsFromPerf(B.Perf);
        UpdateB;
      end;
      InSQL := False;
      InPlan := False;
      InParams := False;
      InPerf := True;
      ExpectSQLAfterDash := False;
      Continue;
    end;

    if StartsText('SQL:', TrimLine) then
    begin
      EnsureActive;
      if FActive.TryGetValue(CurId, B) then
      begin
        B.SQL := B.SQL + Trim(Copy(Line, Pos(':', Line) + 1, MaxInt))
          + sLineBreak;
        UpdateB;
      end;
      InSQL := True;
      InPlan := False;
      InParams := False;
      InPerf := False;
      ExpectSQLAfterDash := False;
      Continue;
    end;

    if ExpectSQLAfterDash and (TrimLine <> '') then
    begin
      EnsureActive;
      if FActive.TryGetValue(CurId, B) then
      begin
        B.SQL := B.SQL + Line + sLineBreak;
        UpdateB;
      end;
      InSQL := True;
      InPlan := False;
      InParams := False;
      InPerf := False;
      ExpectSQLAfterDash := False;
      Continue;
    end;

    if InPlan then
    begin
      if FActive.TryGetValue(CurId, B) then
      begin
        if TrimLine <> '' then
        begin
          B.Plan := B.Plan + Line + sLineBreak;
          UpdateB;
        end;
      end;
      Continue;
    end;

    if InParams then
    begin
      if FActive.TryGetValue(CurId, B) then
      begin
        if TrimLine <> '' then
        begin
          B.Params := B.Params + Line + sLineBreak;
          UpdateB;
        end;
      end;
      Continue;
    end;

    if InPerf then
    begin
      if FActive.TryGetValue(CurId, B) then
      begin
        B.Perf := B.Perf + Line + sLineBreak;
        UpdateB;
      end;
      Continue;
    end;

    if TrimLine <> '' then
    begin
      EnsureActive;
      if FActive.TryGetValue(CurId, B) then
      begin
        B.SQL := B.SQL + Line + sLineBreak;
        UpdateB;
      end;
    end;
  end;

  if CurId >= 0 then
    FinishCur;
end;

procedure TTraceLogParser.ParseLog(const FileName: string;
  const Progress: TProc<Int64, Int64>);
const
  CHUNK_SIZE = 1048576;
var
  FS: TFileStream;
  Ms: TMemoryStream;
  Buf: TBytes;
  ReadCnt: Integer;
  Total, Done: Int64;
  S: string;
begin
  ResetAll;
  if not FileExists(FileName) then
    Exit;
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Total := FS.Size;
    SetLength(Buf, CHUNK_SIZE);
    Ms := TMemoryStream.Create;
    try
      Done := 0;
      while True do
      begin
        ReadCnt := FS.Read(Buf[0], Length(Buf));
        if ReadCnt <= 0 then
          Break;
        Ms.WriteBuffer(Buf[0], ReadCnt);
        Done := Done + ReadCnt;
        if Assigned(Progress) and (Total > 0) then
          Progress((Done * 100) div Total, 100);
      end;
      if Ms.Size > 0 then
      begin
        SetLength(Buf, Ms.Size);
        Ms.Position := 0;
        Ms.ReadBuffer(Buf[0], Ms.Size);
        S := TEncoding.UTF8.GetString(Buf);
      end
      else
        S := '';
    finally
      Ms.Free;
    end;
  finally
    FS.Free;
  end;
  FTailFile := FileName;
  FTailPos := Length(S);
  FTailCarry := '';
  ProcessChunk(S, True);
end;

procedure TTraceLogParser.TailBegin(const FileName: string;
  StartFromEnd: Boolean);
var
  FS: TFileStream;
begin
  FTailFile := FileName;
  FTailCarry := '';
  if not FileExists(FileName) then
  begin
    FTailPos := 0;
    Exit;
  end;
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    if StartFromEnd then
      FTailPos := FS.Size
    else
      FTailPos := 0;
  finally
    FS.Free;
  end;
end;

function TTraceLogParser.TailPoll: Boolean;
var
  FS: TFileStream;
  Need: Int64;
  B: TBytes;
  Chunk, Text: string;
  P: Integer;
begin
  Result := False;
  if FTailFile = '' then
    Exit;
  if not FileExists(FTailFile) then
    Exit;
  FS := TFileStream.Create(FTailFile, fmOpenRead or fmShareDenyNone);
  try
    if FS.Size < FTailPos then
      FTailPos := 0;
    Need := FS.Size - FTailPos;
    if Need <= 0 then
      Exit;
    SetLength(B, Need);
    FS.Position := FTailPos;
    FS.ReadBuffer(B[0], Need);
    FTailPos := FS.Size;
  finally
    FS.Free;
  end;
  Chunk := TEncoding.UTF8.GetString(B);
  Text := FTailCarry + Chunk;
  FTailCarry := '';
  P := LastDelimiter(#10, Text);
  if P > 0 then
  begin
    ProcessChunk(Copy(Text, 1, P), False);
    FTailCarry := Copy(Text, P + 1, MaxInt);
  end
  else
    FTailCarry := Text;
  Result := True;
end;

function TTraceLogParser.TopSlow(const MaxItems: Integer): TSlowArray;
var
  L: TList<TSlowStmt>;
  i, N: Integer;
  Cmp: IComparer<TSlowStmt>;
begin
  L := TList<TSlowStmt>.Create;
  try
    for i := 0 to FSlowList.Count - 1 do
      L.Add(FSlowList[i]);
    Cmp := TSlowStmtComparer.Create;
    L.Sort(Cmp);
    N := L.Count;
    if (MaxItems > 0) and (N > MaxItems) then
      N := MaxItems;
    SetLength(Result, N);
    for i := 0 to N - 1 do
      Result[i] := L[i];
  finally
    L.Free;
  end;
end;

function TTraceLogParser.IndexUsage: ITraceLogParser.TIndexStatArray;
var
  Pair: TPair<string, Integer>;
  i: Integer;
begin
  SetLength(Result, FIdxUsage.Count);
  i := 0;
  for Pair in FIdxUsage do
  begin
    Result[i].Name := Pair.Key;
    Result[i].Count := Pair.Value;
    Inc(i);
  end;
end;

function TTraceLogParser.ErrorTypes: ITraceLogParser.TStringArray;
var
  Pair: TPair<string, Integer>;
  i: Integer;
begin
  SetLength(Result, FErrTypes.Count);
  i := 0;
  for Pair in FErrTypes do
  begin
    Result[i] := Pair.Key + '=' + IntToStr(Pair.Value);
    Inc(i);
  end;
end;

function TTraceLogParser.RecordCount: Integer;
begin
  Result := FSlowList.Count;
end;

function TTraceLogParser.AvgTimeSelect: Double;
begin
  if FCountSel = 0 then
    Result := 0
  else
    Result := FSumSel / FCountSel;
end;

function TTraceLogParser.AvgTimeInsert: Double;
begin
  if FCountIns = 0 then
    Result := 0
  else
    Result := FSumIns / FCountIns;
end;

function TTraceLogParser.AvgTimeUpdate: Double;
begin
  if FCountUpd = 0 then
    Result := 0
  else
    Result := FSumUpd / FCountUpd;
end;

function TTraceLogParser.AvgTimeDelete: Double;
begin
  if FCountDel = 0 then
    Result := 0
  else
    Result := FSumDel / FCountDel;
end;

function TTraceLogParser.MakeSnapshot(const FileName: string): ITraceSnapshot;
var
  S: ITraceSnapshot;
  FP: TFileFingerprintDTO;
  FS: TFileStream;
  IdxArr: array of TIndexStatDTO;
  SlowArr: array of TSlowStmtDTO;
  Pair: TPair<string, Integer>;
  i, N: Integer;
  r: TSlowStmt;
begin
  S := NewTraceSnapshot;
  FP.Size := 0;
  FP.WriteTimeLow := 0;
  FP.WriteTimeHigh := 0;
  if FileExists(FileName) then
  begin
    FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      FP.Size := FS.Size;
    finally
      FS.Free;
    end;
  end;
  S.SetFingerprint(FP);
  S.SetSlowThreshold(0);
  S.SetCounts(FCountSel, FCountIns, FCountUpd, FCountDel);
  S.SetTotals(FSumSel, FSumIns, FSumUpd, FSumDel);
  S.SetMaxima(FMaxSel, FMaxIns, FMaxUpd, FMaxDel);
  S.SetErrMeta(FErrors, FCommits, FRollbacks, FSlowList.Count);

  SetLength(IdxArr, FIdxUsage.Count);
  i := 0;
  for Pair in FIdxUsage do
  begin
    IdxArr[i].Name := Pair.Key;
    IdxArr[i].Count := Pair.Value;
    Inc(i);
  end;
  S.SetIndexUsage(IdxArr);

  N := FSlowList.Count;
  SetLength(SlowArr, N);
  for i := 0 to N - 1 do
  begin
    r := FSlowList[i];
    SlowArr[i].SQLType := r.SQLType;
    SlowArr[i].TimeMs := r.TimeMs;
    SlowArr[i].Rows := r.Rows;
    SlowArr[i].PlanNat := r.PlanNat;
    SlowArr[i].SQLText := r.SQLText;
    SlowArr[i].PlanText := r.PlanText;
    SlowArr[i].ParamText := r.ParamText;
    SlowArr[i].PerfText := r.PerfText;
    SlowArr[i].ExecAt := r.ExecAt;
    SlowArr[i].AppExe := r.AppExe;
    SlowArr[i].Client := r.Client;
  end;
  S.SetSlowList(SlowArr);
  Result := S;
end;

procedure TTraceLogParser.ApplySnapshot(const Snap: ITraceSnapshot);
var
  IdxArr: TIndexStatDTOArray;
  SlowArr: TSlowStmtDTOArray;
  i: Integer;
  r: TSlowStmt;
begin
  ResetAll;
  if Snap = nil then
    Exit;

  FCountSel := Snap.GetCountSelect;
  FCountIns := Snap.GetCountInsert;
  FCountUpd := Snap.GetCountUpdate;
  FCountDel := Snap.GetCountDelete;

  FSumSel := Snap.GetTotalTimeSelect;
  FSumIns := Snap.GetTotalTimeInsert;
  FSumUpd := Snap.GetTotalTimeUpdate;
  FSumDel := Snap.GetTotalTimeDelete;

  FMaxSel := Snap.GetMaxTimeSelect;
  FMaxIns := Snap.GetMaxTimeInsert;
  FMaxUpd := Snap.GetMaxTimeUpdate;
  FMaxDel := Snap.GetMaxTimeDelete;

  FErrors := Snap.GetErrorCount;
  FCommits := Snap.GetCommitCount;
  FRollbacks := Snap.GetRollbackCount;

  IdxArr := Snap.GetIndexUsage;
  for i := Low(IdxArr) to High(IdxArr) do
    DictSetInt(FIdxUsage, IdxArr[i].Name, IdxArr[i].Count);

  SlowArr := Snap.GetSlowList;
  for i := Low(SlowArr) to High(SlowArr) do
  begin
    r.SQLType := SlowArr[i].SQLType;
    r.TimeMs := SlowArr[i].TimeMs;
    r.Rows := SlowArr[i].Rows;
    r.PlanNat := SlowArr[i].PlanNat;
    r.SQLText := SlowArr[i].SQLText;
    r.PlanText := SlowArr[i].PlanText;
    r.ParamText := SlowArr[i].ParamText;
    r.PerfText := SlowArr[i].PerfText;
    FSlowList.Add(r);
  end;
end;

end.
