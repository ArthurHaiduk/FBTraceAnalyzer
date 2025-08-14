unit uTraceCache;

interface

uses Classes, SysUtils, uTraceContracts;

const
  CACHE_FORMAT_VERSION = 1;
  PARSER_API_VERSION = 1;

function LoadSnapshotFromFile(const FileName: string; out Snap: ITraceSnapshot;
  out FmtVer, ApiVer: Cardinal): Boolean;

procedure SaveSnapshotToFile(const FileName: string;
  const Snap: ITraceSnapshot);

implementation

type
  TTag = Cardinal;

const
  TAG_META = $4D455441;
  TAG_IDXM = $4944584D;
  TAG_SLOW = $534C4F57;

  MAGIC: array [0 .. 3] of AnsiChar = ('F', 'B', 'T', 'A');

procedure WriteU(S: TStream; const Str: string);
var
  U: UTF8String;
  L: Integer;
begin
  U := UTF8Encode(Str);
  L := Length(U);
  S.WriteBuffer(L, SizeOf(L));
  if L > 0 then
    S.WriteBuffer(PAnsiChar(U)^, L);
end;

function ReadU(S: TStream): string;
var
  L: Integer;
  U: UTF8String;
begin
  S.ReadBuffer(L, SizeOf(L));
  SetLength(U, L);
  if L > 0 then
    S.ReadBuffer(PAnsiChar(U)^, L);
  Result := UTF8Decode(U);
end;

procedure WriteTag(S: TStream; Tag: TTag; const Payload: TStream);
var
  Len: Int64;
begin
  S.WriteBuffer(Tag, SizeOf(Tag));
  Len := Payload.Size;
  S.WriteBuffer(Len, SizeOf(Len));
  Payload.Position := 0;
  if Len > 0 then
    S.CopyFrom(Payload, Len);
end;

procedure SkipTag(S: TStream);
var
  Len: Int64;
begin
  S.ReadBuffer(Len, SizeOf(Len));
  S.Seek(Len, soFromCurrent);
end;

procedure SaveMETA(S: TStream; const Snap: ITraceSnapshot);
var
  MS: TMemoryStream;
  FP: TFileFingerprintDTO;
  D: Double;
  I: Integer;
  L: Int64;
  Arr: TIntArray;
begin
  MS := TMemoryStream.Create;
  try
    FP := Snap.GetFingerprint;
    MS.WriteBuffer(FP, SizeOf(FP));
    D := Snap.GetSlowThreshold;
    MS.WriteBuffer(D, SizeOf(D));

    I := Snap.GetCountSelect;
    MS.WriteBuffer(I, SizeOf(I));
    I := Snap.GetCountInsert;
    MS.WriteBuffer(I, SizeOf(I));
    I := Snap.GetCountUpdate;
    MS.WriteBuffer(I, SizeOf(I));
    I := Snap.GetCountDelete;
    MS.WriteBuffer(I, SizeOf(I));

    D := Snap.GetTotalTimeSelect;
    MS.WriteBuffer(D, SizeOf(D));
    D := Snap.GetTotalTimeInsert;
    MS.WriteBuffer(D, SizeOf(D));
    D := Snap.GetTotalTimeUpdate;
    MS.WriteBuffer(D, SizeOf(D));
    D := Snap.GetTotalTimeDelete;
    MS.WriteBuffer(D, SizeOf(D));

    D := Snap.GetMaxTimeSelect;
    MS.WriteBuffer(D, SizeOf(D));
    D := Snap.GetMaxTimeInsert;
    MS.WriteBuffer(D, SizeOf(D));
    D := Snap.GetMaxTimeUpdate;
    MS.WriteBuffer(D, SizeOf(D));
    D := Snap.GetMaxTimeDelete;
    MS.WriteBuffer(D, SizeOf(D));

    I := Snap.GetErrorCount;
    MS.WriteBuffer(I, SizeOf(I));
    I := Snap.GetCommitCount;
    MS.WriteBuffer(I, SizeOf(I));
    I := Snap.GetRollbackCount;
    MS.WriteBuffer(I, SizeOf(I));
    L := Snap.GetRecordCount;
    MS.WriteBuffer(L, SizeOf(L));

    Arr := Snap.GetErrorTypeCounts;
    I := Length(Arr);
    MS.WriteBuffer(I, SizeOf(I));
    if I > 0 then
      MS.WriteBuffer(Arr[0], SizeOf(Integer) * I);

    WriteTag(S, TAG_META, MS);
  finally
    MS.Free;
  end;
end;

procedure LoadMETA(S: TStream; const Snap: ITraceSnapshot);
var
  Len: Int64;
  P0: Int64;
  FP: TFileFingerprintDTO;
  Dsel, Dins, Dupd, Ddel: Double;
  Msel, Mins, Mupd, Mdel: Double;
  Csel, Cins, Cupd, Cdel: Integer;
  ErrCnt, CommitCnt, RollbackCnt: Integer;
  RecCnt: Int64;
  I, ArrLen: Integer;
  Arr: TIntArray;
  DThr: Double;
begin
  S.ReadBuffer(Len, SizeOf(Len));
  P0 := S.Position;

  S.ReadBuffer(FP, SizeOf(FP));
  Snap.SetFingerprint(FP);
  S.ReadBuffer(DThr, SizeOf(DThr));
  Snap.SetSlowThreshold(DThr);

  S.ReadBuffer(Csel, SizeOf(Csel));
  S.ReadBuffer(Cins, SizeOf(Cins));
  S.ReadBuffer(Cupd, SizeOf(Cupd));
  S.ReadBuffer(Cdel, SizeOf(Cdel));
  Snap.SetCounts(Csel, Cins, Cupd, Cdel);

  S.ReadBuffer(Dsel, SizeOf(Dsel));
  S.ReadBuffer(Dins, SizeOf(Dins));
  S.ReadBuffer(Dupd, SizeOf(Dupd));
  S.ReadBuffer(Ddel, SizeOf(Ddel));
  Snap.SetTotals(Dsel, Dins, Dupd, Ddel);

  S.ReadBuffer(Msel, SizeOf(Msel));
  S.ReadBuffer(Mins, SizeOf(Mins));
  S.ReadBuffer(Mupd, SizeOf(Mupd));
  S.ReadBuffer(Mdel, SizeOf(Mdel));
  Snap.SetMaxima(Msel, Mins, Mupd, Mdel);

  S.ReadBuffer(ErrCnt, SizeOf(ErrCnt));
  S.ReadBuffer(CommitCnt, SizeOf(CommitCnt));
  S.ReadBuffer(RollbackCnt, SizeOf(RollbackCnt));
  S.ReadBuffer(RecCnt, SizeOf(RecCnt));
  Snap.SetErrMeta(ErrCnt, CommitCnt, RollbackCnt, RecCnt);

  S.ReadBuffer(ArrLen, SizeOf(ArrLen));
  SetLength(Arr, ArrLen);
  for I := 0 to ArrLen - 1 do
    S.ReadBuffer(Arr[I], SizeOf(Integer));
  Snap.SetErrTypeCounts(Arr);

  S.Seek(P0 + Len, soFromBeginning);
end;

procedure SaveIDXM(S: TStream; const Snap: ITraceSnapshot);
var
  MS: TMemoryStream;
  IU: TIndexStatDTOArray;
  I, Cnt: Integer;
begin
  MS := TMemoryStream.Create;
  try
    IU := Snap.GetIndexUsage;
    Cnt := Length(IU);
    MS.WriteBuffer(Cnt, SizeOf(Cnt));
    for I := 0 to Cnt - 1 do
    begin
      WriteU(MS, IU[I].Name);
      MS.WriteBuffer(IU[I].Count, SizeOf(IU[I].Count));
    end;
    WriteTag(S, TAG_IDXM, MS);
  finally
    MS.Free;
  end;
end;

procedure LoadIDXM(S: TStream; const Snap: ITraceSnapshot);
var
  Len: Int64;
  P0: Int64;
  Cnt, I: Integer;
  Arr: TIndexStatDTOArray;
  Name: string;
  Val: Integer;
begin
  S.ReadBuffer(Len, SizeOf(Len));
  P0 := S.Position;

  S.ReadBuffer(Cnt, SizeOf(Cnt));
  SetLength(Arr, Cnt);
  for I := 0 to Cnt - 1 do
  begin
    Name := ReadU(S);
    S.ReadBuffer(Val, SizeOf(Val));
    Arr[I].Name := Name;
    Arr[I].Count := Val;
  end;
  Snap.SetIndexUsage(Arr);

  S.Seek(P0 + Len, soFromBeginning);
end;

procedure LoadSLOW(S: TStream; const Snap: ITraceSnapshot);
var
  Len: Int64;
  P0: Int64;
  Cnt, I: Integer;
  SL: TSlowStmtDTOArray;
  B: Byte;
begin
  S.ReadBuffer(Len, SizeOf(Len));
  P0 := S.Position;

  S.ReadBuffer(Cnt, SizeOf(Cnt));
  SetLength(SL, Cnt);
  for I := 0 to Cnt - 1 do
  begin
    SL[I].SQLType := ReadU(S);
    S.ReadBuffer(SL[I].TimeMs, SizeOf(SL[I].TimeMs));
    S.ReadBuffer(SL[I].Rows, SizeOf(SL[I].Rows));
    S.ReadBuffer(B, SizeOf(B));
    SL[I].PlanNat := (B <> 0);

    SL[I].SQLText := ReadU(S);
    SL[I].PlanText := ReadU(S);
    SL[I].ParamText := ReadU(S);
    SL[I].PerfText := ReadU(S);
  end;
  Snap.SetSlowList(SL);

  S.Seek(P0 + Len, soFromBeginning);
end;

procedure SaveSLOW(S: TStream; const Snap: ITraceSnapshot);
var
  MS: TMemoryStream;
  SL: TSlowStmtDTOArray;
  I, Cnt: Integer;
  B: Byte;
begin
  MS := TMemoryStream.Create;
  try
    SL := Snap.GetSlowList;
    Cnt := Length(SL);
    MS.WriteBuffer(Cnt, SizeOf(Cnt));
    for I := 0 to Cnt - 1 do
    begin
      WriteU(MS, SL[I].SQLType);
      MS.WriteBuffer(SL[I].TimeMs, SizeOf(SL[I].TimeMs));
      MS.WriteBuffer(SL[I].Rows, SizeOf(SL[I].Rows));
      B := 0;
      if SL[I].PlanNat then
        B := 1;
      MS.WriteBuffer(B, SizeOf(B));

      WriteU(MS, SL[I].SQLText);
      WriteU(MS, SL[I].PlanText);
      WriteU(MS, SL[I].ParamText);
      WriteU(MS, SL[I].PerfText);
    end;
    WriteTag(S, TAG_SLOW, MS);
  finally
    MS.Free;
  end;
end;

function LoadSnapshotFromFile(const FileName: string; out Snap: ITraceSnapshot;
  out FmtVer, ApiVer: Cardinal): Boolean;
var
  FS: TFileStream;
  Hdr: array [0 .. 3] of AnsiChar;
  Tag: TTag;
begin
  Result := False;
  Snap := nil;
  if not FileExists(FileName) then
    Exit;

  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    FS.ReadBuffer(Hdr, SizeOf(Hdr));
    if not CompareMem(@Hdr, @MAGIC, SizeOf(Hdr)) then
      Exit;

    FS.ReadBuffer(FmtVer, SizeOf(FmtVer));
    FS.ReadBuffer(ApiVer, SizeOf(ApiVer));

    Snap := NewTraceSnapshot;

    while FS.Position < FS.Size do
    begin
      FS.ReadBuffer(Tag, SizeOf(Tag));
      case Tag of
        TAG_META:
          LoadMETA(FS, Snap);
        TAG_IDXM:
          LoadIDXM(FS, Snap);
        TAG_SLOW:
          LoadSLOW(FS, Snap);
      else
        SkipTag(FS);
      end;
    end;

    Result := True;
  finally
    FS.Free;
  end;
end;

procedure SaveSnapshotToFile(const FileName: string;
  const Snap: ITraceSnapshot);
var
  FS: TFileStream;
  V: Cardinal;
begin
  if (FileName = '') or (Snap = nil) then
    Exit;
  FS := TFileStream.Create(FileName + '.tmp', fmCreate);
  try
    FS.WriteBuffer(MAGIC, SizeOf(MAGIC));
    V := CACHE_FORMAT_VERSION;
    FS.WriteBuffer(V, SizeOf(V));
    V := PARSER_API_VERSION;
    FS.WriteBuffer(V, SizeOf(V));

    SaveMETA(FS, Snap);
    SaveIDXM(FS, Snap);
    SaveSLOW(FS, Snap);
  finally
    FS.Free;
  end;
  RenameFile(FileName + '.tmp', FileName);
end;

end.
