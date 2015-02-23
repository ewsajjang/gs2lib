unit mStringGridHelper;

interface

uses
  System.SysUtils, System.Classes, Vcl.Grids
  ;

type
  TStringGridHelper = class Helper for TStringGrid
    procedure AutoSizeCol(const AColIdx: Integer);
    procedure AutoSizeCols;
  end;

implementation

{ TStringGridHelper }

procedure TStringGridHelper.AutoSizeCol(const AColIdx: Integer);
const
  MARGIN = 15;
var
  i, LWidth, LMaxWidth: Integer;
begin
  LMaxWidth := 0;
  for i := 0 to RowCount - 1 do
  begin
    LWidth:= Canvas.TextWidth(Cells[AColIdx, i]);
    if LWidth> LMaxWidth then
      LMaxWidth := LWidth
  end;
  ColWidths[AColIdx] := LMaxWidth + MARGIN;
end;

procedure TStringGridHelper.AutoSizeCols;
var
  i: Integer;
begin
  for i := 0 to ColCount - 1 do
    AutoSizeCol(i);
end;

end.