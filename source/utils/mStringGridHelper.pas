unit mStringGridHelper;

interface

uses
  System.SysUtils, System.Classes, Vcl.Grids
  ;

type
  TStringGridHelper = class Helper for TStringGrid
    procedure AutoSizeCol(const AColIdx: Integer);
    procedure AutoSizeCols;
    procedure Clear;
    procedure ScrollBy(const ARow: Integer);
    procedure ScrollToBottom;
  end;

implementation

uses
  System.Math
  ;

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

procedure TStringGridHelper.Clear;
var
  i: Integer;
begin
  for i:= 0 to ColCount - 1 do
    Cols[i].Clear;
end;

procedure TStringGridHelper.ScrollBy(const ARow: Integer);
begin
  if ARow < TopRow then
    TopRow := ARow
  else
    TopRow := Max(0, ARow - VisibleRowCount -1);
end;

procedure TStringGridHelper.ScrollToBottom;
begin
  ScrollBy(RowCount -1);
end;

end.
