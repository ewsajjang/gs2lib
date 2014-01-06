unit mRGPHelper;

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.Types,

  WTGrids.Common.Sprites, WTGrids.Common,
  WTGrids, WTGrids.RealGrid, WTGrids.Data, WTGrids.Data.RealGrid,
  WTGrids.Model, Vcl.Menus, WTGrids.Editors, WTGrids.Internals;

type
  EWTRealGridHelper = Exception;

  TWTRealGridHelper = class helper for TWTRealGrid
    function EditedAndCanCommit(const AItem: TWTGridItem): Boolean;
    function Selected: Boolean;
    function SelectedIdx: Integer;
    function SelectedValue(const AFiled: String): Variant;
    function CellToPoint(const ACellRect: TRect): TPoint;
  end;

  TWTGridItemHelper = class helper for TWTGridItem
    function Assigned: Boolean;
    function Selected: Boolean;
  end;

  TWTRealGridDataProviderHelper = class helper for TWTRealGridDataProvider
    procedure Value(const Index: Integer; const AFieldName, AValue: String); overload;
    procedure Value(const Index: Integer; const AFieldName: String; AValue: Integer); overload;
  end;

  TWTGridCellIndexHelper = record helper for TWTGridCellIndex
    function Selected: Boolean;
    function FieldName: String;
    function FieldNameContaion(const AFieldNames: array of String): Boolean;
    class function Empty: TWTGridCellIndex; static;
  end;

implementation

uses
  mConsts;

{ TWTGridItemHelper }

function TWTGridItemHelper.Assigned: Boolean;
begin
  Result := System.Assigned(Self);
end;

function TWTGridItemHelper.Selected: Boolean;
begin
  Result := DataRow > VAL_NOT_ASSIGNED;
end;

{ TWTRealGridHelper }

function TWTRealGridHelper.CellToPoint(const ACellRect: TRect): TPoint;
begin
  Result := ClientToScreen(Point(ACellRect.Left, ACellRect.Bottom));
end;

function TWTRealGridHelper.EditedAndCanCommit(const AItem: TWTGridItem): Boolean;
begin
  Result := IsItemEdited and CanCommit(AItem);
end;

function TWTRealGridHelper.Selected: Boolean;
begin
  Result := Current.ItemIndex > VAL_NOT_ASSIGNED;
end;

function TWTRealGridHelper.SelectedIdx: Integer;
begin
  Result := VAL_NOT_ASSIGNED;
  if Assigned(Current) then
    Result := Current.DataRow;
end;

function TWTRealGridHelper.SelectedValue(const AFiled: String): Variant;
begin
//  if SelectedIdx > IDX_NOT_ASSIGNED then
//    raise EWTRealGridHelper.Create('Can not find selected values');

  Result := Current.Values[AFiled];
end;

{ TWTRealGridDataProviderHelper }

procedure TWTRealGridDataProviderHelper.Value(const Index: Integer;
  const AFieldName, AValue: String);
begin
  if not CellByName[Index, AFieldName].AsString.Equals(AValue) then
    CellByName[Index, AFieldName].AsString := AValue;
end;

procedure TWTRealGridDataProviderHelper.Value(const Index: Integer;
  const AFieldName: String; AValue: Integer);
begin
  if not CellByName[Index, AFieldName].AsInt <> AValue then
    CellByName[Index, AFieldName].AsInt := AValue;
end;

{ TWTGridCellIndexHelper }

class function TWTGridCellIndexHelper.Empty: TWTGridCellIndex;
begin
  Result.Grid := nil;
  Result.ItemIndex := -1;
  Result.Column := nil;
end;

function TWTGridCellIndexHelper.FieldName: String;
begin
  Result := EmptyStr;
  if Selected then
    Result := GetDataColumn.FieldName;
end;

function TWTGridCellIndexHelper.FieldNameContaion(
  const AFieldNames: array of String): Boolean;
var
  LName: String;
begin
  Result := False;
  for LName in AFieldNames do
  begin
    Result := FieldName.Equals(LName);
    if Result then
      Break;
  end;
end;

function TWTGridCellIndexHelper.Selected: Boolean;
begin
  Result := (ItemIndex > -1) and Assigned(Column);
end;

end.
