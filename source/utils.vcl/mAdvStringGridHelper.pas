unit mAdvStringGridHelper;

interface

uses
  AdvGrid,

  mStringGridHelper,

  System.SysUtils, System.Classes, System.Math, System.UITypes, Vcl.Controls
  ;

type
  TAdvStringGridHelper = class helper(TStringGridHelper) for TAdvStringGrid
    procedure Merge(c,r, x, y: Integer; Text: String); overload;
    procedure MergeColNextRowAssign(c, r: Integer; Text: String; ARows: TArray<String>);
    procedure MergeRows(c, r, x: Integer; Text: String; ARows: TArray<String>);
    procedure MergeColSubTitle(c, r, x, y: Integer; Text: String; AColTitles: TArray<String>); overload;
    procedure MergeColTitle(c, r, x, y: Integer; Text: String; AColTitles: TArray<String>; AColor: TColor; AFontStyles: TFontStyles); overload;
    procedure MergeColRowTitle(c, r, x, y: Integer; Text: String; ARowTitles: TArray<String>); overload;
    procedure AssignRows(c, AStartOfRow: Integer; ARows: TArray<String>); overload;
    procedure AssignCols(AStartOfCol, r: Integer; ACols: TArray<String>); overload;
    procedure AssignCols(AStartOfCol, r: Integer; ACols: TArray<String>; AColor: TColor; AFontStyles: TFontStyles); overload;
    procedure AssignCols(AStartOfCol, r: Integer; AColor: TColor); overload;
    procedure AssignCols(AStartOfCol, r: Integer; ACtrls: TArray<TControl>; AssignTag: Boolean = True); overload;
    procedure AssignCols<T: TControl>(AStartOfCol, r: Integer; ACtrls: TArray<T>; AssignTag: Boolean = False); overload;
    procedure AssignColWidths(AWidths: TArray<Integer>);
    function ToColInts(const ACol, AStartRow: Integer): TArray<Integer>;
    function GetColInts(const ACol, AStartRow, ALength: Integer): TArray<Integer>;
    procedure SetColInts(const ACol, AStartRow: Integer; Values: TArray<Integer>);
    function GetColFloats(const ACol, AStartRow, ALength: Integer): TArray<Double>;
    procedure SetColFloats(const ACol, AStartRow: Integer; Values: TArray<Double>);
  end;


implementation

{ TAdvStringGridHelper }

procedure TAdvStringGridHelper.AssignCols(AStartOfCol, r: Integer;
  AColor: TColor);
var
  c: Integer;
begin
  for c := 0 to ColCount - AStartOfCol -1  do
    Colors[c+AStartOfCol, r] := AColor;
end;

procedure TAdvStringGridHelper.AssignCols(AStartOfCol, r: Integer;
  ACols: TArray<String>; AColor: TColor; AFontStyles: TFontStyles);
var
  c: Integer;
begin
  for c := 0 to Min(ColCount, Length(ACols)) - 1 do
  begin
    Cells[c+AStartOfCol, r] := ACols[c];
    Colors[c+AStartOfCol, r] := AColor;
    FontStyles[c+AStartOfCol, r] := AFontStyles;
  end;
end;

procedure TAdvStringGridHelper.AssignCols(AStartOfCol, r: Integer;
  ACtrls: TArray<TControl>; AssignTag: Boolean);
var
  c: Integer;
begin
  for c := 0 to Min(ColCount, Length(ACtrls)) - 1 do
  begin
    if AssignTag then
      ACtrls[c].Tag := c;
    CellControls[c+AStartOfCol, r] := ACtrls[c];
  end;
end;

procedure TAdvStringGridHelper.AssignCols<T>(AStartOfCol, r: Integer;
  ACtrls: TArray<T>; AssignTag: Boolean);
var
  c: Integer;
begin
  for c := 0 to Min(ColCount, Length(ACtrls)) - 1 do
  begin
    if AssignTag then
      ACtrls[c].Tag := c;
    CellControls[c+AStartOfCol, r] := ACtrls[c];
  end;
end;

procedure TAdvStringGridHelper.AssignColWidths(AWidths: TArray<Integer>);
var
  c: Integer;
begin
  for c := 0 to Min(ColCount, Length(AWidths)) -1 do
    ColWidths[c] := AWidths[c];
end;

procedure TAdvStringGridHelper.AssignRows(c, AStartOfRow: Integer;
  ARows: TArray<String>);
var
  r: Integer;
begin
  for r := 0 to Min(RowCount, Length(ARows)) - 1 do
    Cells[c, AStartOfRow + r] := ARows[r];
end;

procedure TAdvStringGridHelper.AssignCols(AStartOfCol, r: Integer; ACols: TArray<String>);
var
  c: Integer;
begin
  for c := 0 to Min(ColCount, Length(ACols)) - 1 do
    Cells[c+AStartOfCol, r] := ACols[c];
end;

procedure TAdvStringGridHelper.MergeRows(c, r, x: Integer; Text: String;
  ARows: TArray<String>);
begin
  Merge(c, r, x, Length(ARows), Text);
  AssignRows(c +1, r, ARows);
end;

procedure TAdvStringGridHelper.Merge(c, r, x, y: Integer; Text: String);
begin
  Cells[c, r] := Text;
  MergeCells(c, r, x, y);
end;

procedure TAdvStringGridHelper.MergeColSubTitle(c, r, x, y: Integer; Text: String; AColTitles: TArray<String>);
begin
  Merge(c, r, x, y, Text);
  AssignCols(c + x, r, AColTitles);
end;

procedure TAdvStringGridHelper.MergeColNextRowAssign(c, r: Integer; Text: String; ARows: TArray<String>);
begin
  Merge(c, r, Length(ARows), 1, Text);
  AssignCols(c, r + 1, ARows);
end;

procedure TAdvStringGridHelper.MergeColRowTitle(c, r, x, y: Integer; Text: String; ARowTitles: TArray<String>);
begin
  Merge(c, r, x, y, Text);
  AssignRows(c + 1, r, ARowTitles);
end;

procedure TAdvStringGridHelper.MergeColTitle(c, r, x, y: Integer; Text: String;
  AColTitles: TArray<String>; AColor: TColor; AFontStyles: TFontStyles);
begin
  Merge(c, r, x, y, Text);
  AssignCols(c + y + 1, r, AColTitles, AColor, AFontStyles);
end;

procedure TAdvStringGridHelper.SetColFloats(const ACol, AStartRow: Integer;
  Values: TArray<Double>);
var
  r: Integer;
begin
  for r := 0 to Min(RowCount, AStartRow + Length(Values)) -1 do
    Floats[ACol, AStartRow + r] := Values[r];
end;

procedure TAdvStringGridHelper.SetColInts(const ACol, AStartRow: Integer;
  Values: TArray<Integer>);
var
  r: Integer;
begin
  for r := 0 to Length(Values) -1 do
    Ints[ACol, AStartRow + r] := Values[r];
end;

function TAdvStringGridHelper.GetColFloats(const ACol, AStartRow,
  ALength: Integer): TArray<Double>;
var
  i: Integer;
begin
  for i := AStartRow to ALength do
    Result := Result + [Floats[ACol, i]];
end;

function TAdvStringGridHelper.GetColInts(const ACol, AStartRow,
  ALength: Integer): TArray<Integer>;
var
  i: Integer;
begin
  for i := AStartRow to ALength do
    Result := Result + [Ints[ACol, i]];
end;

function TAdvStringGridHelper.ToColInts(
  const ACol, AStartRow: Integer): TArray<Integer>;
begin
  Result := GetColInts(ACol, AStartRow, RowCount - AStartRow - 1);
end;

end.
