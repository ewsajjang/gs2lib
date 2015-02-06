unit mRGP.Utils;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Rtti,

  WTGrids.Common.Sprites, WTGrids.Common, WTGrids, WTGrids.RealGrid,
  WTGrids.Model, WTGrids.Data, WTGrids.Data.RealGrid, WTGrids.Loaders,
  WTGrids.Renderers, WTGrids.Internals;

type
  IRealGridPlusUtil = interface
  ['{424FC675-6F8F-48D1-BACA-96E075112B2D}']
    function GetB(Row: Integer; Field: String): Boolean;
    function GetD(Row: Integer; Field: String): TDateTime;
    function GetI(Row: Integer; Field: String): Integer;
    function GetS(Row: Integer; Field: String): String;
    procedure SetB(Row: Integer; Field: String; const Value: Boolean);
    procedure SetD(Row: Integer; Field: String; const Value: TDateTime);
    procedure SetI(Row: Integer; Field: String; const Value: Integer);
    procedure SetS(Row: Integer; Field: String; const Value: String);
    function GetSearchIdx: Integer;
    function GetActiveCell: TWTGridCellIndex;
    procedure SetActiveCell(const Value: TWTGridCellIndex);
    procedure SetSearchIdx(const Value: Integer);

    procedure ActveCellClear;
    procedure ActveCellClearBy(const ARow: Integer);
    procedure IconFieldsAssign(const AFields: array of String);
    function IdxFormMouse(const X, Y: Integer; var AIdx: TWTGridCellIndex): Boolean;
    procedure MouseOver(const X, Y: Integer);
    function Search(AFields: array of String; AValues: array of Variant): Boolean;
    procedure StrechColumn(const AColumn: TWTGridDataColumn; AMinWidth: Integer = 100);
    procedure CompareDateTimeStrField(const AFieldName: String; const AFmt: TFormatSettings);

    property ActiveCell: TWTGridCellIndex read GetActiveCell write SetActiveCell;
    property SearchIdx: Integer read GetSearchIdx write SetSearchIdx;
    property I[Row: Integer; Field: String]: Integer read GetI write SetI;
    property S[Row: Integer; Field: String]: String read GetS write SetS;
    property B[Row: Integer; Field: String]: Boolean read GetB write SetB;
    property D[Row: Integer; Field: String]: TDateTime read GetD write SetD;
  end;

  TSimpleGridUtil = class(TInterfacedObject, IRealGridPlusUtil)
  private
    FFmt: TFormatSettings;
    FActiveCell: TWTGridCellIndex;
    FGrid: TWTRealGrid;
    FProvider: TWTRealGridDataProvider;
    FSearchIdx: Integer;
    function CompareDateTimeField(Field: Integer; const Value1, Value2: Variant): Integer;
    function GetB(Row: Integer; Field: String): Boolean;
    function GetD(Row: Integer; Field: String): TDateTime;
    function GetI(Row: Integer; Field: String): Integer;
    function GetS(Row: Integer; Field: String): String;
    procedure SetB(Row: Integer; Field: String; const Value: Boolean);
    procedure SetD(Row: Integer; Field: String; const Value: TDateTime);
    procedure SetI(Row: Integer; Field: String; const Value: Integer);
    procedure SetS(Row: Integer; Field: String; const Value: String);
    function GetSearchIdx: Integer;
    procedure SetSearchIdx(const Value: Integer);
    function GetActiveCell: TWTGridCellIndex;
    procedure SetActiveCell(const Value: TWTGridCellIndex);
  protected
    FIconFields: TStringList;
  public
    constructor Create(const AGrid: TWTRealGrid);
    destructor Destroy; override;

    procedure StrechColumn(const AColumn: TWTGridDataColumn; AMinWidth: Integer = 100);
    procedure ActveCellClear;
    procedure ActveCellClearBy(const ARow: Integer);
    procedure IconFieldsAssign(const AFields: array of String);
    function IdxFormMouse(const X, Y: Integer; var AIdx: TWTGridCellIndex): Boolean;
    procedure MouseOver(const X, Y: Integer);
    function Search(AFields: array of String; AValues: array of Variant): Boolean;
    procedure CompareDateTimeStrField(const AFieldName: String; const AFmt: TFormatSettings);

    property ActiveCell: TWTGridCellIndex read GetActiveCell write SetActiveCell;
    property SearchIdx: Integer read GetSearchIdx;
    property I[Row: Integer; Field: String]: Integer read GetI write SetI;
    property S[Row: Integer; Field: String]: String read GetS write SetS;
    property B[Row: Integer; Field: String]: Boolean read GetB write SetB;
    property D[Row: Integer; Field: String]: TDateTime read GetD write SetD;
  end;

  TwtrGridPopupAlign = (pdaLeft, pdaRight);
  TwtRGridPopupInfos = record
  private
    FResult: TValue;
  public
    FieldName: String;
    Rect: TRect;
    Point: TPoint;
    RowIndex: Integer;
    Align: TwtrGridPopupAlign;
    Event: String;
    CloseProc: TProc;
    function ResultAssigned: Boolean;
    procedure ResultClear;
    procedure ResultAssign<T>(AValue: T); overload;
    function AssignedResult<T>: T; overload;
  end;

implementation

uses
  mRGPHelper,

  mConsts, mDateTimeHelper,
  System.DateUtils, System.Math, mLog.CodeSite
  ;

{ TSimpleGrdUtil }

procedure TSimpleGridUtil.ActveCellClear;
begin
  FActiveCell := TWTGridCellIndex.Empty;
end;

procedure TSimpleGridUtil.ActveCellClearBy(const ARow: Integer);
begin
  if FActiveCell.GetDataRow = ARow then
    ActveCellClear;
end;

function TSimpleGridUtil.CompareDateTimeField(Field: Integer; const Value1,
  Value2: Variant): Integer;
var
  ADay1 : TDateTime;
  ADay2 : TDateTime;
begin
  ADay1 := TDateTime.FmtSettingsStr(Value1, FFmt);
  ADay2 := TDateTime.FmtSettingsStr(Value2, FFmt);

  Result := CompareValue(ADay1, ADay2);
end;

procedure TSimpleGridUtil.CompareDateTimeStrField(const AFieldName: String;
  const AFmt: TFormatSettings);
begin
  FFmt := AFmt;
  FProvider.SetComparer(AFieldName, CompareDateTimeField);
end;

constructor TSimpleGridUtil.Create(const AGrid: TWTRealGrid);
begin
  FIconFields := TStringList.Create;

  FActiveCell := TWTGridCellIndex.Empty;

  FGrid := TWTRealGrid(AGrid);
  if Assigned(FGrid.DataProvider) then
    FProvider := TWTRealGridDataProvider(FGrid.DataProvider);
end;

destructor TSimpleGridUtil.Destroy;
begin
  FreeAndNil(FIconFields);

  inherited;
end;

function TSimpleGridUtil.GetActiveCell: TWTGridCellIndex;
begin
  Result := FActiveCell;
end;

function TSimpleGridUtil.GetB(Row: Integer; Field: String): Boolean;
begin
  Result := FProvider.CellByName[Row, Field].AsBool;
end;

function TSimpleGridUtil.GetD(Row: Integer; Field: String): TDateTime;
begin
  Result := FProvider.CellByName[Row, Field].AsDateTime;
end;

function TSimpleGridUtil.GetI(Row: Integer; Field: String): Integer;
begin
  Result := FProvider.CellByName[Row, Field].AsInt;
end;

function TSimpleGridUtil.GetS(Row: Integer; Field: String): String;
begin
  Result := FProvider.CellByName[Row, Field].AsString;
end;

function TSimpleGridUtil.GetSearchIdx: Integer;
begin
  Result := FSearchIdx;
end;

function TSimpleGridUtil.Search(AFields: array of String;
  AValues: array of Variant): Boolean;
begin
  FSearchIdx := FGrid.Search(AFields, AValues, [SearchCaseSensitive, SearchPartialMatch], FSearchIdx + 1);
  Result := FSearchIdx > VAL_NOT_ASSIGNED;
end;

procedure TSimpleGridUtil.IconFieldsAssign(const AFields: array of String);
var
  LItem: String;
begin
  FIconFields.Clear;
  for LItem in AFields do
    if FIconFields.IndexOf(LItem) = VAL_NOT_ASSIGNED then
      FIconFields.Add(LItem);
end;

function TSimpleGridUtil.IdxFormMouse(const X, Y: Integer;
  var AIdx: TWTGridCellIndex): Boolean;
begin
  Result := False;
  if not FGrid.Selected or not FGrid.IsFocused then
    Exit;

  AIdx := FGrid.MouseToIndex(X, Y);
//  Log.Msg('CellIndex: %d, DataRow: %d Selected: %s', [AIdx.ItemIndex, AIdx.DataRow, BoolToStr(AIdx.Selected, True)]);
//  if AIdx.ItemIndex > VAL_NOT_ASSIGNED then
//    if AIdx.DataRow > VAL_NOT_ASSIGNED then
//      Result := FIconFields.IndexOf(AIdx.FieldName) > -1;
  if AIdx.Selected then
    Result := FIconFields.IndexOf(AIdx.FieldName) > -1;
end;

procedure TSimpleGridUtil.MouseOver(const X, Y: Integer);
var
  LIdx: TWTGridCellIndex;
begin
  if FGrid.CanFocus and not FGrid.Focused then
  begin
    FGrid.SetFocus;
    if IdxFormMouse(X, Y, LIdx) then
    begin
      if FActiveCell.Selected then
        I[FActiveCell.GetDataRow, FActiveCell.FieldName] := -1;
      FActiveCell := LIdx;
      I[FActiveCell.GetDataRow, FActiveCell.FieldName] := 0;
    end
    else if FActiveCell.Selected then
    begin
      I[FActiveCell.GetDataRow, FActiveCell.FieldName] := -1;
      FActiveCell := TWTGridCellIndex.Empty;
    end;
  end;
end;

procedure TSimpleGridUtil.SetActiveCell(const Value: TWTGridCellIndex);
begin
  FActiveCell := Value;
end;

procedure TSimpleGridUtil.SetB(Row: Integer; Field: String;
  const Value: Boolean);
begin
  if B[Row, Field] <> Value then
    FProvider.CellByName[Row, Field].AsBool := Value;
end;

procedure TSimpleGridUtil.SetD(Row: Integer; Field: String;
  const Value: TDateTime);
begin
  if not D[Row, Field].Equals(Value) then
    FProvider.CellByName[Row, Field].AsDateTime := Value;
end;

procedure TSimpleGridUtil.SetI(Row: Integer; Field: String;
  const Value: Integer);
begin
  if I[Row, Field] <> Value then
    FProvider.CellByName[Row, Field].AsInt:= Value;
end;

procedure TSimpleGridUtil.SetS(Row: Integer; Field: String; const Value: String);
begin
  if S[Row, Field] <> Value then
    FProvider.CellByName[Row, Field].AsString := Value;
end;

procedure TSimpleGridUtil.SetSearchIdx(const Value: Integer);
begin
  FSearchIdx := Value;
end;

procedure TSimpleGridUtil.StrechColumn(const AColumn: TWTGridDataColumn; AMinWidth: Integer);
const
  SCROLLBAR_WIDTH = 30;
  GROOUP_WIDTH = 20;
var
  i: Integer;
  LWidth: Integer;
  LGroupCnt: Integer;
begin
  LWidth := ScrollBar_Width;
  if FGrid.Indicator.Visible then
    Inc(LWidth, 35);

  LGroupCnt := Length(FGrid.GetGroupByFields);
  if LGroupCnt > 0 then
    Inc(LWidth, Grooup_Width * LGroupCnt);

  for i := 0 to FGrid.ColumnCount - 1 do
    if FGrid.Columns[i].Visible then
      if AColumn <> FGrid.Columns[i] then
        Inc(LWidth, FGrid.Columns[i].Width);
  AColumn.Width := Max(AMinWidth, FGrid.Width - LWidth);
end;

{ TwtRGridPopupInfos }

procedure TwtRGridPopupInfos.ResultAssign<T>(AValue: T);
begin
  FResult := TValue.From<T>(AValue);
end;

procedure TwtRGridPopupInfos.ResultClear;
begin
  FResult := TValue.Empty;
end;

function TwtRGridPopupInfos.AssignedResult<T>: T;
begin
  Result := FResult.AsType<T>;
end;

function TwtRGridPopupInfos.ResultAssigned: Boolean;
begin
  Result := not FResult.IsEmpty;
end;

end.
