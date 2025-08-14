unit uTraceContracts;

interface

uses Generics.Collections;

type
  TIndexStatDTO = record
    Name: string;
    Count: Integer;
  end;

  TSlowStmtDTO = record
    SQLType: string;
    TimeMs: Double;
    Rows: Int64;
    PlanNat: Boolean;
    SQLText: string;
    PlanText: string;
    ParamText: string;
    PerfText: string;
    ExecAt: TDateTime;
    AppExe: string;
    Client: string;
  end;

  TIntArray = array of Integer;
  TIndexStatDTOArray = array of TIndexStatDTO;
  TSlowStmtDTOArray = array of TSlowStmtDTO;

  TFileFingerprintDTO = packed record
    Size: Int64;
    WriteTimeLow: Cardinal;
    WriteTimeHigh: Cardinal;
  end;

  ITraceSnapshot = interface(IInterface)
    ['{0F8F5E9E-7C92-4C8B-A7E1-0A2E3B6B1E01}']
    function GetFingerprint: TFileFingerprintDTO;
    function GetSlowThreshold: Double;

    function GetCountSelect: Integer;
    function GetCountInsert: Integer;
    function GetCountUpdate: Integer;
    function GetCountDelete: Integer;

    function GetTotalTimeSelect: Double;
    function GetTotalTimeInsert: Double;
    function GetTotalTimeUpdate: Double;
    function GetTotalTimeDelete: Double;

    function GetMaxTimeSelect: Double;
    function GetMaxTimeInsert: Double;
    function GetMaxTimeUpdate: Double;
    function GetMaxTimeDelete: Double;

    function GetErrorCount: Integer;
    function GetErrorTypeCounts: TIntArray;
    function GetCommitCount: Integer;
    function GetRollbackCount: Integer;
    function GetRecordCount: Int64;

    function GetIndexUsage: TIndexStatDTOArray;
    function GetSlowList: TSlowStmtDTOArray;

    procedure SetFingerprint(const V: TFileFingerprintDTO);
    procedure SetSlowThreshold(const V: Double);

    procedure SetCounts(const Sel, Ins, Upd, Del: Integer);
    procedure SetTotals(const Sel, Ins, Upd, Del: Double);
    procedure SetMaxima(const Sel, Ins, Upd, Del: Double);
    procedure SetErrMeta(const ErrCnt, CommitCnt, RollCnt: Integer;
      const RecCnt: Int64);
    procedure SetErrTypeCounts(const Arr: array of Integer);

    procedure SetIndexUsage(const Arr: array of TIndexStatDTO);
    procedure SetSlowList(const Arr: array of TSlowStmtDTO);
  end;

function NewTraceSnapshot: ITraceSnapshot;

implementation

type
  TTraceSnapshot = class(TInterfacedObject, ITraceSnapshot)
  private
    FFp: TFileFingerprintDTO;
    FSlowThr: Double;
    FCnt: array [0 .. 3] of Integer;
    FTot: array [0 .. 3] of Double;
    FMax: array [0 .. 3] of Double;
    FErrCnt, FCommit, FRoll: Integer;
    FErrType: TIntArray;
    FRec: Int64;
    FIdx: TIndexStatDTOArray;
    FSlow: TSlowStmtDTOArray;
  public
    constructor Create;
    function GetFingerprint: TFileFingerprintDTO;
    function GetSlowThreshold: Double;
    function GetCountSelect: Integer;
    function GetCountInsert: Integer;
    function GetCountUpdate: Integer;
    function GetCountDelete: Integer;
    function GetTotalTimeSelect: Double;
    function GetTotalTimeInsert: Double;
    function GetTotalTimeUpdate: Double;
    function GetTotalTimeDelete: Double;
    function GetMaxTimeSelect: Double;
    function GetMaxTimeInsert: Double;
    function GetMaxTimeUpdate: Double;
    function GetMaxTimeDelete: Double;
    function GetErrorCount: Integer;
    function GetErrorTypeCounts: TIntArray;
    function GetCommitCount: Integer;
    function GetRollbackCount: Integer;
    function GetRecordCount: Int64;
    function GetIndexUsage: TIndexStatDTOArray;
    function GetSlowList: TSlowStmtDTOArray;
    procedure SetFingerprint(const V: TFileFingerprintDTO);
    procedure SetSlowThreshold(const V: Double);
    procedure SetCounts(const Sel, Ins, Upd, Del: Integer);
    procedure SetTotals(const Sel, Ins, Upd, Del: Double);
    procedure SetMaxima(const Sel, Ins, Upd, Del: Double);
    procedure SetErrMeta(const ErrCnt, CommitCnt, RollCnt: Integer;
      const RecCnt: Int64);
    procedure SetErrTypeCounts(const Arr: array of Integer);
    procedure SetIndexUsage(const Arr: array of TIndexStatDTO);
    procedure SetSlowList(const Arr: array of TSlowStmtDTO);
  end;

constructor TTraceSnapshot.Create;
begin
  SetLength(FErrType, 6);
end;

function NewTraceSnapshot: ITraceSnapshot;
begin
  Result := TTraceSnapshot.Create;
end;

function TTraceSnapshot.GetFingerprint: TFileFingerprintDTO;
begin
  Result := FFp;
end;

function TTraceSnapshot.GetSlowThreshold: Double;
begin
  Result := FSlowThr;
end;

function TTraceSnapshot.GetCountSelect: Integer;
begin
  Result := FCnt[0];
end;

function TTraceSnapshot.GetCountInsert: Integer;
begin
  Result := FCnt[1];
end;

function TTraceSnapshot.GetCountUpdate: Integer;
begin
  Result := FCnt[2];
end;

function TTraceSnapshot.GetCountDelete: Integer;
begin
  Result := FCnt[3];
end;

function TTraceSnapshot.GetTotalTimeSelect: Double;
begin
  Result := FTot[0];
end;

function TTraceSnapshot.GetTotalTimeInsert: Double;
begin
  Result := FTot[1];
end;

function TTraceSnapshot.GetTotalTimeUpdate: Double;
begin
  Result := FTot[2];
end;

function TTraceSnapshot.GetTotalTimeDelete: Double;
begin
  Result := FTot[3];
end;

function TTraceSnapshot.GetMaxTimeSelect: Double;
begin
  Result := FMax[0];
end;

function TTraceSnapshot.GetMaxTimeInsert: Double;
begin
  Result := FMax[1];
end;

function TTraceSnapshot.GetMaxTimeUpdate: Double;
begin
  Result := FMax[2];
end;

function TTraceSnapshot.GetMaxTimeDelete: Double;
begin
  Result := FMax[3];
end;

function TTraceSnapshot.GetErrorCount: Integer;
begin
  Result := FErrCnt;
end;

function TTraceSnapshot.GetErrorTypeCounts: TIntArray;
begin
  Result := FErrType;
end;

function TTraceSnapshot.GetCommitCount: Integer;
begin
  Result := FCommit;
end;

function TTraceSnapshot.GetRollbackCount: Integer;
begin
  Result := FRoll;
end;

function TTraceSnapshot.GetRecordCount: Int64;
begin
  Result := FRec;
end;

function TTraceSnapshot.GetIndexUsage: TIndexStatDTOArray;
begin
  Result := FIdx;
end;

function TTraceSnapshot.GetSlowList: TSlowStmtDTOArray;
begin
  Result := FSlow;
end;

procedure TTraceSnapshot.SetFingerprint(const V: TFileFingerprintDTO);
begin
  FFp := V;
end;

procedure TTraceSnapshot.SetSlowThreshold(const V: Double);
begin
  FSlowThr := V;
end;

procedure TTraceSnapshot.SetCounts(const Sel, Ins, Upd, Del: Integer);
begin
  FCnt[0] := Sel;
  FCnt[1] := Ins;
  FCnt[2] := Upd;
  FCnt[3] := Del;
end;

procedure TTraceSnapshot.SetTotals(const Sel, Ins, Upd, Del: Double);
begin
  FTot[0] := Sel;
  FTot[1] := Ins;
  FTot[2] := Upd;
  FTot[3] := Del;
end;

procedure TTraceSnapshot.SetMaxima(const Sel, Ins, Upd, Del: Double);
begin
  FMax[0] := Sel;
  FMax[1] := Ins;
  FMax[2] := Upd;
  FMax[3] := Del;
end;

procedure TTraceSnapshot.SetErrMeta(const ErrCnt, CommitCnt, RollCnt: Integer;
  const RecCnt: Int64);
begin
  FErrCnt := ErrCnt;
  FCommit := CommitCnt;
  FRoll := RollCnt;
  FRec := RecCnt;
end;

procedure TTraceSnapshot.SetErrTypeCounts(const Arr: array of Integer);
var
  I, N: Integer;
begin
  N := Length(Arr);
  SetLength(FErrType, N);
  for I := 0 to N - 1 do
    FErrType[I] := Arr[I];
end;

procedure TTraceSnapshot.SetIndexUsage(const Arr: array of TIndexStatDTO);
var
  I, N: Integer;
begin
  N := Length(Arr);
  SetLength(FIdx, N);
  for I := 0 to N - 1 do
    FIdx[I] := Arr[I];
end;

procedure TTraceSnapshot.SetSlowList(const Arr: array of TSlowStmtDTO);
var
  I, N: Integer;
begin
  N := Length(Arr);
  SetLength(FSlow, N);
  for I := 0 to N - 1 do
    FSlow[I] := Arr[I];
end;

end.
