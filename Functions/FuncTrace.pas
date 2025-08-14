unit FuncTrace;

interface

uses
  SysUtils, Classes, cxVGrid, cxClasses, cxEdit, cxVariants, cxDataStorage,
  Generics.Collections, Generics.Defaults, ITraceLogParser;

function FormatTimeMsToHMS(timeMs: Int64): string;

procedure FillDictToVGrid(AGrid: TcxVerticalGrid;
  const ADict: TDictionary<string, Integer>; const ACaption: string;
  const ASortByValue: Boolean = False);

procedure FillArrayToVGrid(AGrid: TcxVerticalGrid; const AData: TStringArray;
  const ACaption: string);

implementation

function FormatTimeMsToHMS(timeMs: Int64): string;
var
  totalSec, sec, min, hour: Int64;
begin
  totalSec := timeMs div 1000;
  sec := totalSec mod 60;
  min := (totalSec div 60) mod 60;
  hour := totalSec div 3600;
  Result := Format('%.2d:%.2d:%.2d', [hour, min, sec]);
end;

procedure FillDictToVGrid(AGrid: TcxVerticalGrid;
  const ADict: TDictionary<string, Integer>; const ACaption: string;
  const ASortByValue: Boolean);
var
  CatRow: TcxCategoryRow;
  Row: TcxEditorRow;
  Pair: TPair<string, Integer>;
  List: TList < TPair < string, Integer >> ;
begin
  AGrid.BeginUpdate;
  try
    AGrid.ClearRows;
    CatRow := TcxCategoryRow(AGrid.Add(TcxCategoryRow));
    CatRow.Properties.Caption := ACaption;

    List := TList < TPair < string, Integer >> .Create;
    try
      for Pair in ADict do
        List.Add(Pair);

      List.Sort(TComparer < TPair < string,
        Integer >> .Construct( function(const L, R: TPair<string,
            Integer>): Integer begin if ASortByValue then Result :=
            R.Value - L.Value else Result := CompareText(L.Key, R.Key); end));

      for Pair in List do
      begin
        Row := TcxEditorRow(AGrid.AddChild(CatRow, TcxEditorRow));
        Row.Properties.Caption := Pair.Key;
        Row.Properties.DataBinding.ValueTypeClass := TcxIntegerValueType;
        Row.Properties.Value := Pair.Value;
      end;
    finally
      List.Free;
    end;
  finally
    AGrid.EndUpdate;
  end;
end;

procedure FillArrayToVGrid(AGrid: TcxVerticalGrid; const AData: TStringArray;
  const ACaption: string);
var
  CatRow: TcxCategoryRow;
  Row: TcxEditorRow;
  I: Integer;
begin
  AGrid.BeginUpdate;
  try
    AGrid.ClearRows;
    CatRow := TcxCategoryRow(AGrid.Add(TcxCategoryRow));
    CatRow.Properties.Caption := ACaption;

    for I := 0 to High(AData) do
    begin
      Row := TcxEditorRow(AGrid.AddChild(CatRow, TcxEditorRow));
      Row.Properties.Caption := Format('Елемент %d', [I + 1]);
      Row.Properties.DataBinding.ValueTypeClass := TcxStringValueType;
      Row.Properties.Value := AData[I];
    end;
  finally
    AGrid.EndUpdate;
  end;
end;

end.
