unit mLinkedCollections;

interface

uses
  System.Classes, System.SysUtils, System.Rtti, mEnumerator,

  System.Generics.Collections
  ;

type
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

  TLinkedList<T> = class(mEnumerator.TEnumerable<T>, ILinkedList<T>)
  type
    TEnumerator = class(mEnumerator.TEnumerator<T>)
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

  ILinkedElement<T> = interface(IEnumerable<T>)
    function GetEoE: Boolean;
    function GetSoE: Boolean;
    function GetLength: Integer;
    function GetElements(Index: Integer): T;
    procedure Clear;
    procedure Add(AValue: T); overload;
    function Add: T; overload;
    function Add(const AParams: array of TValue): T; overload;

    function MoveNext(var AValue: T): Boolean; overload;
    function MoveNext: Boolean; overload;
    function MovePrev(var AValue: T): Boolean; overload;
    function MovePrev: Boolean; overload;
    function First: T;
    function Prev: T;
    function Next: T;
    function Last: T;

    property Length: Integer read GetLength;
    property EoE: Boolean read GetEoE;
    property SoE: Boolean read GetSoE;
    property Elements[Index: Integer]: T read GetElements;
  end;

  TLinkedElement<T> = class(mEnumerator.TEnumerable<T>, ILinkedElement<T>)
  private type
    TEnumerator = class(mEnumerator.TEnumerator<T>)
    private
      FList: TObjectList<TLinkedElement<T>>;
      FIdx: Integer;
    public
      constructor Create(const AList: TObjectList<TLinkedElement<T>>);
      function GetCurrent: T; override;
      function MoveNext: Boolean; override;
    end;
  public
    // IEnurator implementation
    function GetEnumerator: IEnumerator<T>; override;
  private
    FElements: TObjectList<TLinkedElement<T>>;
    function GetEoE: Boolean;
    function GetSoE: Boolean;
    function GetLength: Integer;
    function GetValue: T;
    procedure SetValue(const Value: T);
    function GetElements(Index: Integer): T;
  protected
    FOwner, FCurrent, FPrev, FNext: TLinkedElement<T>;
    FValue: T;
    FOwnsObject: Boolean;
  protected
    function IsRootElement: Boolean; virtual;
  public
    constructor Create(const AOwnsObject: Boolean = True); virtual;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(AValue: T); overload;
    function Add: T; overload;
    function Add(const AParams: array of TValue): T; overload;
    function Add(const AValue: Boolean): T; overload;

    function MoveNext(var AValue: T): Boolean; overload;
    function MoveNext: Boolean; overload;
    function MovePrev(var AValue: T): Boolean; overload;
    function MovePrev: Boolean; overload;
    function First: T;
    function Prev: T;
    function Next: T;
    function Last: T;

    property Length: Integer read GetLength;
    property EoE: Boolean read GetEoE;
    property SoE: Boolean read GetSoE;
    property Value: T read GetValue write SetValue;
    property Elements[Index: Integer]: T read GetElements; default;
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

procedure TLinkedElement<T>.Add(AValue: T);
var
  LElement: TLinkedElement<T>;
begin
  LElement := TLinkedElement<T>.Create;
  LElement.FOwner := Self;
  LElement.FValue := AValue;
  if Assigned(FCurrent) then
  begin
    LElement.FPrev := FCurrent;
    FCurrent.FNext := LElement;
  end;
  FElements.Add(LElement);
  FCurrent := LElement;
end;

function TLinkedElement<T>.Add: T;
begin
  Result := Add([]);
end;

function TLinkedElement<T>.Add(const AParams: array of TValue): T;
var
  LValue: TValue;
  LCtx: TRttiContext;
  LRType: TRttiType;
  LCreator: TRttiMethod;
  LParamLen: Integer;
  LInstanceType: TRttiInstanceType;
begin
  LCtx := TRttiContext.Create;
  LRType := LCtx.GetType(TypeInfo(T));
  for LCreator in LRType.GetMethods do
    if LCreator.IsConstructor then
      if System.Length(LCreator.GetParameters) = System.Length(AParams) then
      begin
        LInstanceType := LRType.AsInstance;
        LValue := LCreator.Invoke(LInstanceType.MetaclassType, AParams);
        Result := LValue.AsType<T>;
        Add(Result);
        Exit;
      end;
end;

function TLinkedElement<T>.Add(const AValue: Boolean): T;
begin
  Result := Add([AValue]);
end;

procedure TLinkedElement<T>.Clear;
begin
  if not IsRootElement and FOwnsObject and (GetTypeKind(T) in [tkClass]) then
    PObject(@FValue)^.Free
  else if IsManagedType(T) then
    FValue := Default(T);

  if IsRootElement then
    FElements.Clear;

  FCurrent := nil;
  FPrev := nil;
  FNext := nil;
end;

constructor TLinkedElement<T>.Create(const AOwnsObject: Boolean = True);
begin
  FOwnsObject := AOwnsObject;
  FElements := TObjectList<TLinkedElement<T>>.Create(True);
end;

destructor TLinkedElement<T>.Destroy;
begin
  Clear;
  FreeAndNil(FElements);

  inherited;
end;

function TLinkedElement<T>.GetElements(Index: Integer): T;
begin
  if IsRootElement then
    Result := FElements[Index].FValue
  else
    Result := FValue
end;

function TLinkedElement<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(FElements);
end;

function TLinkedElement<T>.GetEoE: Boolean;
begin
  Result := not Assigned(FCurrent.FNext);
end;

function TLinkedElement<T>.GetLength: Integer;
begin
  Result := FElements.Count;
end;

function TLinkedElement<T>.GetSoE: Boolean;
begin
  Result := not Assigned(FCurrent.FPrev);
end;

function TLinkedElement<T>.GetValue: T;
begin
  if IsRootElement then
    Result := FCurrent.FValue
  else
    Result := FValue
end;

function TLinkedElement<T>.IsRootElement: Boolean;
begin
  Result := Assigned(FCurrent) and not Assigned(FOwner);
end;

function TLinkedElement<T>.First: T;
begin
  FCurrent := FElements.First;
  Result := FCurrent.FValue;
end;

function TLinkedElement<T>.Last: T;
begin
  FCurrent := FElements.Last;
  Result := FCurrent.FValue;
end;

function TLinkedElement<T>.Next: T;
begin
  if MoveNext then
    Result := FCurrent.FValue
  else
    raise Exception.Create('Element postion is out of range');
end;

function TLinkedElement<T>.Prev: T;
begin
  if MovePrev then
    Result := FCurrent.FValue
  else
    raise Exception.Create('Element postion is out of range');
end;

procedure TLinkedElement<T>.SetValue(const Value: T);
begin
  if IsRootElement then
    FCurrent.FValue := Value
  else
    FValue := Value
end;

function TLinkedElement<T>.MoveNext: Boolean;
begin
  Result := Assigned(FCurrent.FNext);
  if Result then
    FCurrent := FCurrent.FNext;
end;

function TLinkedElement<T>.MoveNext(var AValue: T): Boolean;
begin
  Result := MoveNext;
  if Result then
    AValue := FCurrent.FValue;
end;

function TLinkedElement<T>.MovePrev: Boolean;
begin
  Result := Assigned(FCurrent.FPrev);
  if Result then
    FCurrent := FCurrent.FPrev;
end;

function TLinkedElement<T>.MovePrev(var AValue: T): Boolean;
begin
  Result := MovePrev;
  if Result then
    AValue := FCurrent.FValue;
end;

{ TLinkedElement<T>.TEnumerator }

constructor TLinkedElement<T>.TEnumerator.Create(
  const AList: TObjectList<TLinkedElement<T>>);
begin
  FList := AList;
  FIdx := 0;
end;

function TLinkedElement<T>.TEnumerator.GetCurrent: T;
begin
  Result := FList[FIdx].FValue;
  Inc(FIdx);
end;

function TLinkedElement<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := InRange(FIdx, 0, FList.Count -1);
end;

end.
