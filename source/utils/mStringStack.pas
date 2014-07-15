unit mStringStack;

interface

uses
  System.Classes, System.SysUtils,
  System.Generics.Collections
  ;

type
  TSimpleStackBuffer<T> = class
  private
    FStack: TList<T>;
    FIdx: Integer;
    function GetValues(Index: Integer): T;
  public
    constructor Create(const ACapacity: Integer = 32);
    destructor Destroy; override;

    function Add(const AValue: T): Integer;
    function Previous: Boolean;
    function Next: Boolean;
    function PreviousValue: T;
    function NextValue: T;

    property Values[Index: Integer]: T read GetValues;
  end;

implementation

uses
  System.Math
  ;

{ TSimpleStackBuffer<T> }

function TSimpleStackBuffer<T>.Add(const AValue: T): Integer;
begin
  if FStack.Count >= FStack.Capacity then
    FStack.Delete(0);

  FIdx := FStack.Add(AValue);
  Result := FIdx;
end;

constructor TSimpleStackBuffer<T>.Create(const ACapacity: Integer);
begin
  FStack := TList<T>.Create;
  FStack.Capacity := ACapacity;

  FIdx := 0;
end;

destructor TSimpleStackBuffer<T>.Destroy;
begin
  FreeAndNil(FStack);

  inherited;
end;

function TSimpleStackBuffer<T>.GetValues(Index: Integer): T;
begin
  Result := FStack[Index];
end;

function TSimpleStackBuffer<T>.Next: Boolean;
begin
  Result := InRange(FIdx + 1, 0, FStack.Count -1);
end;

function TSimpleStackBuffer<T>.NextValue: T;
begin
  if InRange(FIdx, 0, FStack.Count -2) then
    Inc(FIdx);
  Result := FStack[FIdx];
end;

function TSimpleStackBuffer<T>.Previous: Boolean;
begin
  Result := InRange(FIdx, 0, FStack.Count -1);
end;

function TSimpleStackBuffer<T>.PreviousValue: T;
begin
  Result := FStack[FIdx];
  if InRange(FIdx, 1, FStack.Count) then
    Dec(FIdx);
end;

end.
