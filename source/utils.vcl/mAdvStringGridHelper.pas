unit mAdvStringGridHelper;

interface

uses
  mStringGridHelper,

  AdvObj, BaseGrid, AdvGrid
  ;

type
  TAdvStringGridHelper = class helper (TStringGridHelper) for TAdvStringGrid
    procedure MergeCells(c, r, spanx, spany: Integer; AText: String; const ASubItems: TArray<String>); overload;
  end;

implementation

{ TAdvStringGridHelper }

procedure TAdvStringGridHelper.MergeCells(c, r, spanx, spany: Integer;
  AText: String; const ASubItems: TArray<String>);
var
  i: Integer;
begin
  MergeCells(c, r, spanx, spany);
  Cells[c, r] := AText;

  for i := 0 to spany -1 do
    Cells[c+spanx, r+i] := ASubItems[i]
end;

end.
