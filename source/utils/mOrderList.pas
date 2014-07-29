unit mRepeator;

interface

uses
  System.Classes, System.SysUtils,
  System.Generics.Collections;

type
  TSimpleRepeator<T> = class
  private
    FPosByIdx: Integer;
    FItemIsClass: Boolean;
    FItemClass: TClass;
    FList: TList<T>;
    procedure OnNotify(Sender: TObject; const Item: T; Action: TCollectionNotification);
  private
    FOwnsObjects: Boolean;
    function GetCount: Integer;
    procedure SetPosByIdx(const Value: Integer);
    function GetCurrent: T;
    function GetPosBy: T;
    procedure SetPosBy(const Value: T);
  public
    constructor Create(AOwnsObjects: Boolean = True);
    destructor Destroy; override;

    procedure Init(const AItems: array of T);
    function Add(const AItem: T): Integer;
    function Remove(const AItem: T): Integer;
    function Exists(const AItem: T): Boolean;
    function TryPosBy(const AItem: T): Boolean;
    function Next: T;
    function Eof: Boolean;

    property Count: Integer read GetCount;
    property Current: T read GetCurrent;
    property PosByIdx: Integer read FPosByIdx write SetPosByIdx;
    property PosBy: T read GetPosBy write SetPosBy;

    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

implementation

uses
  mConsts,
  System.RTTI, System.TypInfo, System.Math;

{ TSimpleRepeator<T> }

function TSimpleRepeator<T>.Add(const AItem: T): Integer;
begin
  if not Exists(AItem) then
    Result := FList.Add(AItem)
  else
    Result := FList.Count - 1;
end;

constructor TSimpleRepeator<T>.Create(AOwnsObjects: Boolean);
begin
  FOwnsObjects := AOwnsObjects;

  FList := TList<T>.Create;
  FList.OnNotify := OnNotify;
end;

destructor TSimpleRepeator<T>.Destroy;
begin
  FreeAndNil(FList);

  inherited;
end;

function TSimpleRepeator<T>.Eof: Boolean;
begin
  Result := FPosByIdx = Count - 1;
end;

function TSimpleRepeator<T>.Exists(const AItem: T): Boolean;
begin
  Result := FList.IndexOf(AItem) > VAL_NOT_ASSIGNED;
end;

function TSimpleRepeator<T>.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TSimpleRepeator<T>.GetCurrent: T;
begin
  Result := FList[FPosByIdx];
end;

function TSimpleRepeator<T>.GetPosBy: T;
begin
  Result := Current;
end;

procedure TSimpleRepeator<T>.Init(const AItems: array of T);
var
  LInfo: PTypeInfo;
  LItem: T;
begin
  FList.Clear;
  if FOwnsObjects then
  begin
    LInfo := TypeInfo(T);
    FItemIsClass := LInfo.Kind = tkClass;
    if FItemIsClass then
      FItemClass := LInfo.TypeData.ClassType;
  end;

  for LItem in AItems do
    FList.Add(LItem);

  FPosByIdx := 0;
end;

function TSimpleRepeator<T>.Next: T;
begin
  if FPosByIdx < Count then
    Inc(FPosByIdx);

  Result := FList[FPosByIdx];
end;

procedure TSimpleRepeator<T>.OnNotify(Sender: TObject; const Item: T;
  Action: TCollectionNotification);
begin
  if FOwnsObjects and (Action = cnRemoved) then
    if FItemIsClass then
      (Item as FItemClass).Free;
end;

function TSimpleRepeator<T>.Remove(const AItem: T): Integer;
begin
  Result := FList.Remove(AItem)
end;

procedure TSimpleRepeator<T>.SetPosBy(const Value: T);
var
  LIdx: Integer;
begin
  LIdx := FList.IndexOf(Value);
  if InRange(LIdx, 0, FList.Count - 1) then
    FPosByIdx := LIdx;
end;

procedure TSimpleRepeator<T>.SetPosByIdx(const Value: Integer);
begin
  if Value < Count then
    FPosByIdx := Value;
end;

function TSimpleRepeator<T>.TryPosBy(const AItem: T): Boolean;
begin
  Result := Exists(AItem);
  if Result then
    PosBy := AItem
end;

end.
