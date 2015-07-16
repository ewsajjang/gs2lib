unit mLinkedList;

interface

uses
  System.Classes, System.SysUtils,

  System.Generics.Collections
  ;

type
  IEnumerator<T> = interface
    function GetCurrent: T;
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

  TEnumerator<T> = class(TInterfacedObject, IEnumerator<T>)
    function GetCurrent: T; virtual; abstract;
    function MoveNext: Boolean; virtual; abstract;
    property Current: T read GetCurrent;
  end;

  IEnumerable<T> = interface
    function GetEnumerator: IEnumerator<T>;
  end;

  TEnumerable<T> = class(TInterfacedObject, IEnumerable<T>)
    function GetEnumerator: IEnumerator<T>; virtual; abstract;
  end;

  TLinkedList<T> = class;

  TLinkedNode<T> = class
  private
    FValue: T;
    FPrev: TLinkedNode<T>;
    FNext: TLinkedNode<T>;
    FList: TLinkedList<T>;
    FOwnsObject: Boolean;
    function GetEoE: Boolean;
    function GetSoE: Boolean;
  public
    constructor Create(AValue: T);
    destructor Destroy; override;

    property Next: TLinkedNode<T> read FNext;
    property Prev: TLinkedNode<T> read FPrev;

    property SoE: Boolean read GetSoE;
    property EoE: Boolean read GetEoE;

    property List: TLinkedList<T> read FList;
    property Value: T read FValue write FValue;
  end;

  ILinkedList<T> = interface(IEnumerable<T>)
    ['{9D43E8BC-311F-425A-B6E0-FA26340197F9}']
    function GetCount: NativeUInt;
    function GetFirstNode: TLinkedNode<T>;
    function GetLastNode: TLinkedNode<T>;

    procedure Clear;

    procedure AddFirst(const AValue: T); overload;
    procedure AddFirst(const ANode: TLinkedNode<T>); overload;
    procedure Add(const AValue: T); overload;
    procedure Add(const ANode: TLinkedNode<T>); overload;
    function First: T;
    function Last: T;
    function ToArray: TArray<T>;
    function ToNodeArray: TArray<TLinkedNode<T>>;

    property Count: NativeUInt read GetCount;
    property FirstNode: TLinkedNode<T> read GetFirstNode;
    property LastNode: TLinkedNode<T> read GetLastNode;
  end;

  TLinkedList<T> = class(TEnumerable<T>, ILinkedList<T>)
  type
    TEnumerator = class(TEnumerator<T>)
    private
      FList: TLinkedList<T>;
      FCurrentNode: TLinkedNode<T>;
    public
      constructor Create(const AList: TLinkedList<T>);

      function GetCurrent: T; override;
      function MoveNext: Boolean; override;
    end;
    function GetEnumerator: IEnumerator<T>; override;
  private
    FFirstNode: TLinkedNode<T>;
    FLastNode: TLinkedNode<T>;
    FCount: NativeUInt;
    function GetCount: NativeUInt;
    function GetFirstNode: TLinkedNode<T>;
    function GetLastNode: TLinkedNode<T>;
  protected
    procedure DoNodeAssigned(var AEmlement: TLinkedNode<T>); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure AddFirst(const AValue: T); overload;
    procedure AddFirst(const ANode: TLinkedNode<T>); overload;
    procedure Add(const AValue: T); overload;
    procedure Add(const ANode: TLinkedNode<T>); overload;
    function First: T;
    function Last: T;
    function ToArray: TArray<T>;
    function ToNodeArray: TArray<TLinkedNode<T>>;

    property Count: NativeUInt read GetCount;
    property FirstNode: TLinkedNode<T> read GetFirstNode;
    property LastNode: TLinkedNode<T> read GetLastNode;
  end;

  TObjectLinkedList<T: class> = class(TLinkedList<T>)
  private
    FNodeOwnsObject: Boolean;
  protected
    procedure DoNodeAssigned(var ANode: TLinkedNode<T>); override;
  public
    constructor Create(const AOwnsNode: Boolean = True);

    property OwnsNode: Boolean read FNodeOwnsObject;
  end;

implementation

{ TLinkedListNode<T> }

constructor TLinkedNode<T>.Create(AValue: T);
begin
  FValue := AValue;

  FList := nil;
  FPrev := nil;
  FNext := nil;
end;

destructor TLinkedNode<T>.Destroy;
begin
  if Assigned(FPrev) then
  begin
    FPrev.FNext := FNext;
    if Assigned(FNext) and Assigned(FList) then
      FList.FLastNode := FPrev;
  end
  else if Assigned(FList) then
    FList.FFirstNode := FNext;

  if Assigned(FNext) then
    FNext.FPrev := FPrev;

  if Assigned(FList) then
  begin
    if FOwnsObject and (GetTypeKind(T) in [tkClass]) then
      PObject(@FValue)^.Free;

    Dec(FList.FCount);
  end;

  if FList.Count = 0 then
    FList.FLastNode := nil;

  inherited;
end;

function TLinkedNode<T>.GetEoE: Boolean;
begin
  Result := not Assigned(FNext);
end;

function TLinkedNode<T>.GetSoE: Boolean;
begin
  Result := not Assigned(FPrev);
end;

{ TLinkedList<T>.TEnumerator }

constructor TLinkedList<T>.TEnumerator.Create(const AList: TLinkedList<T>);
begin
  FList := AList;
  FCurrentNode := nil;
end;

function TLinkedList<T>.TEnumerator.GetCurrent: T;
begin
  if FCurrentNode <> nil then
    Result := FCurrentNode.Value
  else
    Result := default(T);
end;

function TLinkedList<T>.TEnumerator.MoveNext: Boolean;
begin
  if not Assigned(FCurrentNode) then
    FCurrentNode := FList.FirstNode
  else
    FCurrentNode := FCurrentNode.FNext;

  Result := Assigned(FCurrentNode);
end;

{ TLinkedList<T> }

procedure TLinkedList<T>.Add(const AValue: T);
begin
  Add(TLinkedNode<T>.Create(AValue));
end;

procedure TLinkedList<T>.AddFirst(const AValue: T);
begin
  AddFirst(TLinkedNode<T>.Create(AValue));
end;

procedure TLinkedList<T>.AddFirst(const ANode: TLinkedNode<T>);
begin
  if not Assigned(ANode) then
    raise Exception.Create('TLinkedList<T> Argument nil error');

  if Assigned(ANode.FList) then
    raise Exception.Create('TLinkedList<T> Node alrady part of collection error');

  ANode.FNext := FFirstNode;
  if Assigned(FFirstNode) then
    FFirstNode.FPrev := ANode;


  FFirstNode := ANode;
  DoNodeAssigned(FFirstNode);

  if not Assigned(FLastNode) then
    FLastNode := FFirstNode;

  ANode.FList := Self;
  Inc(FCount);
end;

procedure TLinkedList<T>.Add(const ANode: TLinkedNode<T>);
begin
  if not Assigned(ANode) then
    raise Exception.Create('TLinkedList<T> Argument nil error');

  if Assigned(ANode.FList) then
    raise Exception.Create('TLinkedList<T> Node alrady part of collection error');

  ANode.FPrev := FLastNode;
  if Assigned(FLastNode) then
    FLastNode.FNext := ANode;

  FLastNode := ANode;
  DoNodeAssigned(FLastNode);

  if not Assigned(FFirstNode) then
    FFirstNode := FLastNode;

  ANode.FList := Self;
  Inc(FCount);
end;

procedure TLinkedList<T>.Clear;
begin
  while Assigned(FFirstNode) do
    FFirstNode.Free;
end;

constructor TLinkedList<T>.Create;
begin
  FFirstNode := nil;
  FLastNode := nil;
  FCount := 0;
end;

destructor TLinkedList<T>.Destroy;
begin
  Clear;

  inherited;
end;

procedure TLinkedList<T>.DoNodeAssigned(var AEmlement: TLinkedNode<T>);
begin
end;

function TLinkedList<T>.First: T;
begin
  if FCount = 0 then
    raise Exception.Create('TLinkedList<T> is Empty Error');

  Result := FFirstNode.FValue
end;

function TLinkedList<T>.GetCount: NativeUInt;
begin
  Result := FCount;
end;

function TLinkedList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TLinkedList<T>.GetFirstNode: TLinkedNode<T>;
begin
  Result := FFirstNode;
end;

function TLinkedList<T>.GetLastNode: TLinkedNode<T>;
begin
  Result := FLastNode;
end;

function TLinkedList<T>.Last: T;
begin
  if FCount = 0 then
    raise Exception.Create('TLinkedList<T> is Empty Error');

  Result := FLastNode.FValue;
end;

function TLinkedList<T>.ToArray: TArray<T>;
var
  LNode: TLinkedNode<T>;
  LIdx: Integer;
begin
  SetLength(Result, FCount);
  if not Assigned(FFirstNode) then
    Exit;

  LIdx := 0;
  LNode := FFirstNode;
  repeat
    Result[LIdx] := LNode.Value;
    LNode := LNode.FNext;
    Inc(LIdx);
  until not Assigned(LNode);
end;

function TLinkedList<T>.ToNodeArray: TArray<TLinkedNode<T>>;
var
  LNode: TLinkedNode<T>;
  LIdx: Integer;
begin
  SetLength(Result, FCount);
  if not Assigned(FFirstNode) then
    Exit;

  LIdx := 0;
  LNode := FFirstNode;
  repeat
    Result[LIdx] := LNode;
    LNode := LNode.FNext;
    Inc(LIdx);
  until not Assigned(LNode);
end;

{ TObjectLinkedList<T> }

constructor TObjectLinkedList<T>.Create(const AOwnsNode: Boolean);
begin
  FNodeOwnsObject := AOwnsNode;
end;

procedure TObjectLinkedList<T>.DoNodeAssigned(
  var ANode: TLinkedNode<T>);
begin
  inherited;

  ANode.FOwnsObject := FNodeOwnsObject
end;

end.
