unit mRepeator;

interface

uses
  System.Classes, System.SysUtils,
  System.Generics.Collections;

type
  TSimpleRepeator<T> = class
  private
    FPosition: Integer;
    FItemIsClass: Boolean;
    FItemClass: TClass;
    FList: TList<T>;
    procedure OnNotify(Sender: TObject; const Item: T; Action: TCollectionNotification);
  private
    FOwnsObjects: Boolean;
    function GetCount: Integer;
    procedure SetPosition(const Value: Integer);
    function GetCurrent: T;
  public
    constructor Create(AOwnsObjects: Boolean = True);
    destructor Destroy; override;

    procedure Init(AItems: array of T);
    function Next: T;
    function Eof: Boolean;

    property Count: Integer read GetCount;
    property Current: T read GetCurrent;
    property Position: Integer read FPosition write SetPosition;

    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

implementation

uses
  mConsts,
  System.RTTI, System.TypInfo;

{ TSimpleRepeator<T> }

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
  Result := FPosition = Count - 1;
end;

function TSimpleRepeator<T>.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TSimpleRepeator<T>.GetCurrent: T;
begin
  Result := FList[FPosition];
end;

procedure TSimpleRepeator<T>.Init(AItems: array of T);
var
  LInfo: PTypeInfo;
  LItem: T;
begin
  if FOwnsObjects then
  begin
    LInfo := TypeInfo(T);
    FItemIsClass := LInfo.Kind = tkClass;
    if FItemIsClass then
      FItemClass := LInfo.TypeData.ClassType;
  end;

  for LItem in AItems do
    FList.Add(LItem);

  FPosition := 0;
end;

function TSimpleRepeator<T>.Next: T;
begin
  if FPosition < Count then
    Inc(FPosition);

  Result := FList[FPosition];
end;

procedure TSimpleRepeator<T>.OnNotify(Sender: TObject; const Item: T;
  Action: TCollectionNotification);
begin
  if FOwnsObjects and (Action = cnRemoved) then
    if FItemIsClass then
      (Item as FItemClass).Free;
end;

procedure TSimpleRepeator<T>.SetPosition(const Value: Integer);
begin
  if Value < Count then
    FPosition := Value;
end;

end.
