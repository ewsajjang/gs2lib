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

  TLinkedElement<T> = class
  private
    FValue: T;
    FPrev: TLinkedElement<T>;
    FNext: TLinkedElement<T>;
    FList: TLinkedList<T>;
    FOwnsObject: Boolean;
    function GetEoE: Boolean;
    function GetSoE: Boolean;
  public
    constructor Create(AValue: T);
    destructor Destroy; override;

    property Next: TLinkedElement<T> read FNext;
    property Prev: TLinkedElement<T> read FPrev;

    property SoE: Boolean read GetSoE;
    property EoE: Boolean read GetEoE;

    property List: TLinkedList<T> read FList;
    property Value: T read FValue write FValue;
  end;

  ILinkedList<T> = interface(IEnumerable<T>)
    ['{9D43E8BC-311F-425A-B6E0-FA26340197F9}']
    function GetCount: NativeUInt;
    function GetFirstElement: TLinkedElement<T>;
    function GetLastElement: TLinkedElement<T>;

    procedure Clear;

    procedure AddFirst(const AValue: T); overload;
    procedure AddFirst(const AElement: TLinkedElement<T>); overload;
    procedure Add(const AValue: T); overload;
    procedure Add(const AElement: TLinkedElement<T>); overload;
    function First: T;
    function Last: T;
    function ToArray: TArray<T>;
    function ToElementArray: TArray<TLinkedElement<T>>;

    property Count: NativeUInt read GetCount;
    property FirstElement: TLinkedElement<T> read GetFirstElement;
    property LastElement: TLinkedElement<T> read GetLastElement;
  end;

  TLinkedList<T> = class(TEnumerable<T>, ILinkedList<T>)
  type
    TEnumerator = class(TEnumerator<T>)
    private
      FList: TLinkedList<T>;
      FCurrentElement: TLinkedElement<T>;
    public
      constructor Create(const AList: TLinkedList<T>);

      function GetCurrent: T; override;
      function MoveNext: Boolean; override;
    end;
    function GetEnumerator: IEnumerator<T>; override;
  private
    FFirstElement: TLinkedElement<T>;
    FLastElement: TLinkedElement<T>;
    FCount: NativeUInt;
    function GetCount: NativeUInt;
    function GetFirstElement: TLinkedElement<T>;
    function GetLastElement: TLinkedElement<T>;
  protected
    procedure DoElementAssigned(var AEmlement: TLinkedElement<T>); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure AddFirst(const AValue: T); overload;
    procedure AddFirst(const AElement: TLinkedElement<T>); overload;
    procedure Add(const AValue: T); overload;
    procedure Add(const AElement: TLinkedElement<T>); overload;
    function First: T;
    function Last: T;
    function ToArray: TArray<T>;
    function ToElementArray: TArray<TLinkedElement<T>>;

    property Count: NativeUInt read GetCount;
    property FirstElement: TLinkedElement<T> read GetFirstElement;
    property LastElement: TLinkedElement<T> read GetLastElement;
  end;

  TObjectLinkedList<T: class> = class(TLinkedList<T>)
  private
    FElementOwnsObject: Boolean;
  protected
    procedure DoElementAssigned(var AEmlement: TLinkedElement<T>); override;
  public
    constructor Create(const AOwnsElement: Boolean = True);

    property OwnsElement: Boolean read FElementOwnsObject;
  end;

implementation

{ TLinkedListNode<T> }

constructor TLinkedElement<T>.Create(AValue: T);
begin
  FValue := AValue;

  FList := nil;
  FPrev := nil;
  FNext := nil;
end;

destructor TLinkedElement<T>.Destroy;
begin
  if Assigned(FPrev) then
  begin
    FPrev.FNext := FNext;
    if Assigned(FNext) and Assigned(FList) then
      FList.FLastElement := FPrev;
  end
  else if Assigned(FList) then
    FList.FFirstElement := FNext;

  if Assigned(FNext) then
    FNext.FPrev := FPrev;

  if Assigned(FList) then
  begin
    if FOwnsObject and (GetTypeKind(T) in [tkClass]) then
      PObject(@FValue)^.Free;

    Dec(FList.FCount);
  end;

  if FList.Count = 0 then
    FList.FLastElement := nil;

  inherited;
end;

function TLinkedElement<T>.GetEoE: Boolean;
begin
  Result := not Assigned(FNext);
end;

function TLinkedElement<T>.GetSoE: Boolean;
begin
  Result := not Assigned(FPrev);
end;

{ TLinkedList<T>.TEnumerator }

constructor TLinkedList<T>.TEnumerator.Create(const AList: TLinkedList<T>);
begin
  FList := AList;
  FCurrentElement := nil;
end;

function TLinkedList<T>.TEnumerator.GetCurrent: T;
begin
  if FCurrentElement <> nil then
    Result := FCurrentElement.Value
  else
    Result := default(T);
end;

function TLinkedList<T>.TEnumerator.MoveNext: Boolean;
begin
  if not Assigned(FCurrentElement) then
    FCurrentElement := FList.FirstElement
  else
    FCurrentElement := FCurrentElement.FNext;

  Result := Assigned(FCurrentElement);
end;

{ TLinkedList<T> }

procedure TLinkedList<T>.Add(const AValue: T);
begin
  Add(TLinkedElement<T>.Create(AValue));
end;

procedure TLinkedList<T>.AddFirst(const AValue: T);
begin
  AddFirst(TLinkedElement<T>.Create(AValue));
end;

procedure TLinkedList<T>.AddFirst(const AElement: TLinkedElement<T>);
begin
  if not Assigned(AElement) then
    raise Exception.Create('TLinkedList<T> Argument nil error');

  if Assigned(AElement.FList) then
    raise Exception.Create('TLinkedList<T> Element alrady part of collection error');

  AElement.FNext := FFirstElement;
  if Assigned(FFirstElement) then
    FFirstElement.FPrev := AElement;


  FFirstElement := AElement;
  DoElementAssigned(FFirstElement);

  if not Assigned(FLastElement) then
    FLastElement := FFirstElement;

  AElement.FList := Self;
  Inc(FCount);
end;

procedure TLinkedList<T>.Add(const AElement: TLinkedElement<T>);
begin
  if not Assigned(AElement) then
    raise Exception.Create('TLinkedList<T> Argument nil error');

  if Assigned(AElement.FList) then
    raise Exception.Create('TLinkedList<T> Element alrady part of collection error');

  AElement.FPrev := FLastElement;
  if Assigned(FLastElement) then
    FLastElement.FNext := AElement;

  FLastElement := AElement;
  DoElementAssigned(FLastElement);

  if not Assigned(FFirstElement) then
    FFirstElement := FLastElement;

  AElement.FList := Self;
  Inc(FCount);
end;

procedure TLinkedList<T>.Clear;
begin
  while Assigned(FFirstElement) do
    FFirstElement.Free;
end;

constructor TLinkedList<T>.Create;
begin
  FFirstElement := nil;
  FLastElement := nil;
  FCount := 0;
end;

destructor TLinkedList<T>.Destroy;
begin
  Clear;

  inherited;
end;

procedure TLinkedList<T>.DoElementAssigned(var AEmlement: TLinkedElement<T>);
begin
end;

function TLinkedList<T>.First: T;
begin
  if FCount = 0 then
    raise Exception.Create('TLinkedList<T> is Empty Error');

  Result := FFirstElement.FValue
end;

function TLinkedList<T>.GetCount: NativeUInt;
begin
  Result := FCount;
end;

function TLinkedList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TLinkedList<T>.GetFirstElement: TLinkedElement<T>;
begin
  Result := FFirstElement;
end;

function TLinkedList<T>.GetLastElement: TLinkedElement<T>;
begin
  Result := FLastElement;
end;

function TLinkedList<T>.Last: T;
begin
  if FCount = 0 then
    raise Exception.Create('TLinkedList<T> is Empty Error');

  Result := FLastElement.FValue;
end;

function TLinkedList<T>.ToArray: TArray<T>;
var
  LElement: TLinkedElement<T>;
  LIdx: Integer;
begin
  SetLength(Result, FCount);
  if not Assigned(FFirstElement) then
    Exit;

  LIdx := 0;
  LElement := FFirstElement;
  repeat
    Result[LIdx] := LElement.Value;
    LElement := LElement.FNext;
    Inc(LIdx);
  until not Assigned(LElement);
end;

function TLinkedList<T>.ToElementArray: TArray<TLinkedElement<T>>;
var
  LElement: TLinkedElement<T>;
  LIdx: Integer;
begin
  SetLength(Result, FCount);
  if not Assigned(FFirstElement) then
    Exit;

  LIdx := 0;
  LElement := FFirstElement;
  repeat
    Result[LIdx] := LElement;
    LElement := LElement.FNext;
    Inc(LIdx);
  until not Assigned(LElement);
end;

{ TObjectLinkedList<T> }

constructor TObjectLinkedList<T>.Create(const AOwnsElement: Boolean);
begin
  FElementOwnsObject := AOwnsElement;
end;

procedure TObjectLinkedList<T>.DoElementAssigned(
  var AEmlement: TLinkedElement<T>);
begin
  inherited;

  AEmlement.FOwnsObject := FElementOwnsObject
end;

end.
