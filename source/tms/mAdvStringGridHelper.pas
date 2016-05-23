unit mAdvStringGridHelper;

interface

uses
  AdvGrid,

  mStringGridHelper,

  System.SysUtils, System.Classes, System.Math, System.UITypes, Vcl.Controls
  ;

type
  TAdvStringGridHelper = class helper(TStringGridHelper) for TAdvStringGrid
    procedure Merge(c,r, x, y: Integer; Text: String);
    procedure ColumsAssign(AStartOfCol, r: Integer; ACols: TArray<String>); overload;
    procedure ColumsAssign(AStartOfCol, r: Integer; ACols: TArray<String>; AColor: TColor; AFontStyles: TFontStyles); overload;
    procedure ColumsAssign(AStartOfCol, r: Integer; AColor: TColor); overload;
    procedure ColumsAssign(AStartOfCol, r: Integer; ACtrls: TArray<TControl>; AssignTag: Boolean = True); overload;
  end;


implementation

{ TAdvStringGridHelper }

procedure TAdvStringGridHelper.ColumsAssign(AStartOfCol, r: Integer;
  AColor: TColor);
var
  c: Integer;
begin
  for c := 0 to ColCount -1  do
    Colors[c+AStartOfCol, r] := AColor;
end;

procedure TAdvStringGridHelper.ColumsAssign(AStartOfCol, r: Integer;
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

procedure TAdvStringGridHelper.ColumsAssign(AStartOfCol, r: Integer;
  ACtrls: TArray<TControl>; AssignTag: Boolean);
var
  c: Integer;
begin
  for c := 0 to Min(ColCount, Length(ACtrls)) - 1 do
  begin
    if AssignTag then
      ACtrls[c].Tag := c+AStartOfCol;
    CellControls[c+AStartOfCol, r] := ACtrls[c];
  end;
end;

procedure TAdvStringGridHelper.Merge(c, r, x, y: Integer; Text: String);
begin
  Cells[c, r] := Text;
  MergeCells(c, r, x, y);
end;

procedure TAdvStringGridHelper.ColumsAssign(AStartOfCol, r: Integer; ACols: TArray<String>);
var
  c: Integer;
begin
  for c := 0 to Min(ColCount, Length(ACols)) - 1 do
    Cells[c+AStartOfCol, r] := ACols[c];
end;

end.
