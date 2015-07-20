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
    function GetEoN: Boolean;
    function GetSoN: Boolean;
  public
    constructor Create(AValue: T);
    destructor Destroy; override;

    property Next: TLinkedNode<T> read FNext;
    property Prev: TLinkedNode<T> read FPrev;

    property SoN: Boolean read GetSoN;
    property EoN: Boolean read GetEoN;

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

  TLinkedElement<T> = class;
  ILinkedElement<T> = interface(IEnumerable<T>)
    function GetCount: Integer;
    function GetEoE: Boolean;
    function GetSoE: Boolean;
    function GetValue: T;

    procedure Add(const AValue: T);

    function FirstElement: TLinkedElement<T>;
    function LastElement: TLinkedElement<T>;
    function NextElement: TLinkedElement<T>;
    function PrevElement: TLinkedElement<T>;

    function First: T;
    function Last: T;
    function Next: T;
    function Prev: T;

    function ToArray: TArray<T>;

    property Count: Integer read GetCount;
    property Value: T read GetValue;
    property SoE: Boolean read GetSoE;
    property EoE: Boolean read GetEoE;
  end;

  TLinkedElement<T> = class(TEnumerable<T>, ILinkedElement<T>)
  type
    TEnumerator = class(TEnumerator<T>)
    private
      FIdx: Integer;
      FElementValues: TArray<T>;
    public
      constructor Create(const AElement: TArray<T>);

      function GetCurrent: T; override;
      function MoveNext: Boolean; override;
    end;
    function GetEnumerator: IEnumerator<T>; override;
  private
    FCount: Integer;
    FValue: T;
    FCurrent, FPrev, FNext: TLinkedElement<T>;
    function GetCount: Integer;
    function GetEoE: Boolean;
    function GetSoE: Boolean;
    function GetValue: T;
  protected
    function DoCreateElelement: TLinkedElement<T>; virtual;
  public
    constructor Create(const AValue: T); overload;
    constructor Create; overload;
    destructor Destroy; override;

    procedure Add(const AValue: T);

    function FirstElement: TLinkedElement<T>;
    function LastElement: TLinkedElement<T>;
    function NextElement: TLinkedElement<T>;
    function PrevElement: TLinkedElement<T>;

    function First: T;
    function Last: T;
    function Next: T;
    function Prev: T;

    function ToArray: TArray<T>;

    property Count: Integer read GetCount;
    property Value: T read GetValue;
    property SoE: Boolean read GetSoE;
    property EoE: Boolean read GetEoE;
  end;

  TObjectLinkedElement<T: class> = class(TLinkedElement<T>)
  private
    FOwnsElement: Boolean;
  protected
    function DoCreateElelement: TLinkedElement<T>; override;
  public
    destructor Destroy; override;

    constructor Create(const AValue: T; const AOwnsElement: Boolean = True); overload;
    constructor Create(const AOwnsElement: Boolean = True); overload;
  end;

implementation

uses
  System.Math
  ;

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

function TLinkedNode<T>.GetEoN: Boolean;
begin
  Result := not Assigned(FNext);
end;

function TLinkedNode<T>.GetSoN: Boolean;
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

{ TLinkedElement<T> }

procedure TLinkedElement<T>.Add(const AValue: T);
var
  LLast, LElement: TLinkedElement<T>;
begin
  LElement:= DoCreateElelement;
  LElement.FValue := AValue;
  if Assigned(FCurrent) then
  begin
    LLast := LastElement;
    LElement.FPrev := LLast;
    LLast.FNext := LElement;
  end;
  FCurrent := LElement;
  Inc(FCount);
end;

constructor TLinkedElement<T>.Create(const AValue: T);
begin
  Create;
  Add(AValue);
end;

constructor TLinkedElement<T>.Create;
begin
  FCount := 0;
  FPrev := nil;
  FNext := nil;
  FCurrent := nil;
end;

destructor TLinkedElement<T>.Destroy;
begin
  if Assigned(FPrev) then
    FPrev.FNext := FNext
  else if Assigned(FNext) then
    FNext.FPrev := FPrev;

  if Assigned(FPrev) then
    FreeAndNil(FPrev)
  else if Assigned(FNext) then
    FreeAndNil(FNext);

  if Assigned(FCurrent) then
    FCurrent.Free;

  inherited;
end;

function TLinkedElement<T>.DoCreateElelement: TLinkedElement<T>;
begin
  Result := TLinkedElement<T>.Create;
end;

function TLinkedElement<T>.First: T;
begin
  Result := FirstElement.FValue;
end;

function TLinkedElement<T>.FirstElement: TLinkedElement<T>;
var
  LElement: TLinkedElement<T>;
begin
  LElement := FCurrent;
  while Assigned(LElement.FPrev) do
    LElement:= LElement.FPrev;
  FCurrent := LElement;
  Result := FCurrent;
end;

function TLinkedElement<T>.GetCount: Integer;
begin
  Result := FCount;
end;

function TLinkedElement<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(ToArray);
end;

function TLinkedElement<T>.GetEoE: Boolean;
begin
  Result := not Assigned(FCurrent.FNext);
end;

function TLinkedElement<T>.GetSoE: Boolean;
begin
  Result := not Assigned(FCurrent.FPrev);
end;

function TLinkedElement<T>.GetValue: T;
begin
  Result := FCurrent.FValue
end;

function TLinkedElement<T>.Last: T;
begin
  Result := LastElement.FValue;
end;

function TLinkedElement<T>.LastElement: TLinkedElement<T>;
var
  LElement: TLinkedElement<T>;
begin
  LElement := FCurrent;
  while Assigned(LElement.FNext) do
    LElement:= LElement.FNext;
  FCurrent := LElement;
  Result := FCurrent;
end;

function TLinkedElement<T>.Next: T;
begin
  Result := NextElement.FValue;
end;

function TLinkedElement<T>.NextElement: TLinkedElement<T>;
begin
  FCurrent := FCurrent.FNext;
end;

function TLinkedElement<T>.Prev: T;
begin
  Result := PrevElement.FValue;
end;

function TLinkedElement<T>.PrevElement: TLinkedElement<T>;
begin
  FCurrent:= FCurrent.FPrev
end;

function TLinkedElement<T>.ToArray: TArray<T>;
var
  LElement: TLinkedElement<T>;
begin
  LElement := FirstElement;
  repeat
    Result := Result + [LElement.FValue];
    LElement := LElement.FNext;
  until not Assigned(LElement);
end;

{ TLinkedElement<T>.TEnumerator }

constructor TLinkedElement<T>.TEnumerator.Create(
  const AElement: TArray<T>);
begin
  FElementValues := AElement;
  FIdx := 0;
end;

function TLinkedElement<T>.TEnumerator.GetCurrent: T;
begin
  Result := FElementValues[FIdx];
  if InRange(FIdx, 0, Length(FElementValues) -1) then
    Inc(FIdx);
end;

function TLinkedElement<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := InRange(FIdx, 0, Length(FElementValues) -1);
end;

{ TObjectLinkedElement<T> }

constructor TObjectLinkedElement<T>.Create(const AValue: T;
  const AOwnsElement: Boolean);
begin
  Create(AOwnsElement);
end;

constructor TObjectLinkedElement<T>.Create(const AOwnsElement: Boolean);
begin
  inherited Create;

  FOwnsElement := AOwnsElement;
end;

destructor TObjectLinkedElement<T>.Destroy;
begin
  if FOwnsElement and (GetTypeKind(T) in [tkClass]) then
    FreeAndNil(FValue);

  inherited;
end;

function TObjectLinkedElement<T>.DoCreateElelement: TLinkedElement<T>;
begin
  Result := TObjectLinkedElement<T>.Create;
end;

end.
