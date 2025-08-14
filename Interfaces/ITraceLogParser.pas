unit ITraceLogParser;

interface

type
  TFileFingerprint = packed record
    Size: Int64;
    WriteTimeLow: Cardinal;
    WriteTimeHigh: Cardinal;
  end;

  TProgressProc = reference to procedure(const APos, ATotal: Int64);

  TErrorType = (etDeadlock, etForeignKey, etUniqueKey, etNotNull,
    etLockConflict, etGeneral);

  TIndexStat = record
    Name: string;
    Count: Integer;
  end;

  TStringArray = array of string;
  TIndexStatArray = array of TIndexStat;

type
  TSlowStmt = record
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

  TSlowArray = array of TSlowStmt;

type
  TParamInfo = record
    Id: Integer;
    DataType: string;
    AsText: string;
  end;

implementation

end.
