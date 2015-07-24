unit TestmLinkedCollections;

interface

uses
  System.Classes, System.SysUtils, mLinkedCollections,

  DUnitX.TestFramework;

type
  [TestFixture]
  TLinkedListTest = class
  private
    type
      TItem = class
        ID: Integer;
        constructor Create(const AID: Integer); reintroduce;
        destructor Destroy; override;
      end;
    var
      FList: ILinkedList<TItem>;
  private
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [TestCase]
    procedure AddTest;
    [TestCase]
    procedure AddFirstTest;
    [TestCase]
    procedure ToArrayTest;
    [TestCase]
    procedure ToFieldArrayTest;
    [TestCase]
    procedure FirstLastTest;
    [TestCase]
    procedure FirstLastNodeTest;
    [TestCase]
    procedure NodeNextPrev;
  end;

  TLinkedElementTest = class
  private type
    TObjElement = class(TLinkedElement<TObjElement>)
    private
      FID: Integer;
        function GetID: Integer;
        procedure SetID(const Value: Integer);
    public
      constructor Create(AID: Integer); overload;
      property ID: Integer read GetID write SetID;
    end;
  private
    FElement: TLinkedElement<Integer>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [TestCase]
    procedure ManagedTypeTest;
    [TestCase]
    procedure ClassTypeTest;
  end;

implementation

var
  FCnt: Integer;
{ LinkedListTest.TItem }

constructor TLinkedListTest.TItem.Create(const AID: Integer);
begin
  inherited Create;

  ID := AID;
  Inc(FCnt);
end;

destructor TLinkedListTest.TItem.Destroy;
begin
  Dec(FCnt);

  inherited;
end;

{ LinkedListTest }

procedure TLinkedListTest.AddFirstTest;
var
  LNode: TLinkedNode<TItem>;
begin
  FList.Clear;
  FList.AddFirst(TItem.Create(0));
  FList.AddFirst(TItem.Create(1));
  FList.AddFirst(TItem.Create(2));

  LNode := FList.FirstNode;
  Assert.AreEqual(2, LNode.Value.ID);
  LNode := LNode.Next;
  Assert.AreEqual(1, LNode.Value.ID);
  LNode := LNode.Next;
  Assert.AreEqual(0, LNode.Value.ID);
end;

procedure TLinkedListTest.Setup;
begin
  FList := TObjectLinkedList<TItem>.Create;
  FCnt := 0;
end;

procedure TLinkedListTest.TearDown;
begin
  FList := nil;
  Assert.AreEqual(0, FCnt);
end;

procedure TLinkedListTest.ToArrayTest;
var
  LItems: array[0..3] of TItem;
  LArray: TArray<TItem>;
begin
  LItems[0] := TItem.Create(0);
  LItems[1] := TItem.Create(1);
  LItems[2] := TItem.Create(2);
  FList.Add(LItems[0]);
  FList.Add(LItems[1]);
  FList.Add(LItems[2]);
  LArray := FList.ToArray;
  Assert.AreEqual(3, Length(LArray));
  Assert.AreSame(LItems[0], LArray[0]);
  Assert.AreSame(LItems[1], LArray[1]);
  Assert.AreSame(LItems[2], LArray[2]);
end;

procedure TLinkedListTest.ToFieldArrayTest;
var
  LNodes: array[0..3] of TLinkedNode<TItem>;
  LArray: TArray<TLinkedNode<TItem>>;
begin
  LNodes[0] := TLinkedNode<TItem>.Create(TItem.Create(0));
  LNodes[1] := TLinkedNode<TItem>.Create(TItem.Create(1));
  LNodes[2] := TLinkedNode<TItem>.Create(TItem.Create(2));
  FList.Add(LNodes[0]);
  FList.Add(LNodes[1]);
  FList.Add(LNodes[2]);
  LArray := FList.ToNodeArray;
  Assert.AreEqual(3, Length(LArray));
  Assert.AreSame(LNodes[0], LArray[0]);
  Assert.AreSame(LNodes[1], LArray[1]);
  Assert.AreSame(LNodes[2], LArray[2]);
end;

procedure TLinkedListTest.AddTest;
var
  LItem: TItem;
  LNode: TLinkedNode<TItem>;
begin
  FList.Add(TItem.Create(0));
  FList.Add(TItem.Create(1));
  FList.Add(TItem.Create(2));
  Assert.AreEqual<NativeUInt>(3, FList.Count);

  LNode := FList.FirstNode;
  LItem := LNode.Value;
  Assert.AreEqual(0, LItem.ID);
  Assert.IsTrue(LNode.SoN);
  Assert.IsFalse(LNode.EoN);

  LNode := LNode.Next;
  LItem := LNode.Value;
  Assert.AreEqual(1, LItem.ID);

  LNode := LNode.Next;
  LItem := LNode.Value;
  Assert.AreEqual(2, LItem.ID);
  Assert.IsFalse(LNode.SoN);
  Assert.IsTrue(LNode.EoN);
end;

procedure TLinkedListTest.NodeNextPrev;
var
  LNode: TLinkedNode<TItem>;
begin
  FList.Add(TItem.Create(0));
  FList.Add(TItem.Create(1));
  FList.Add(TItem.Create(2));
  LNode := FList.FirstNode;
  LNode := LNode.Next;
  Assert.AreEqual(1, LNode.Value.ID);
  LNode := LNode.Prev;
  Assert.AreEqual(0, LNode.Value.ID);
  LNode := LNode.Next;
  Assert.AreEqual(1, LNode.Value.ID);
  LNode := LNode.Next;
  Assert.AreEqual(2, LNode.Value.ID);
  LNode := LNode.Prev;
  Assert.AreEqual(1, LNode.Value.ID);
  LNode := LNode.Prev;
  Assert.AreEqual(0, LNode.Value.ID);
end;

procedure TLinkedListTest.FirstLastNodeTest;
begin
  FList.Add(TItem.Create(0));
  FList.Add(TItem.Create(1));
  FList.Add(TItem.Create(2));
  Assert.AreEqual(0, FList.FirstNode.Value.ID);
  Assert.AreEqual(2, FList.LastNode.Value.ID);
end;

procedure TLinkedListTest.FirstLastTest;
begin
  FList.Add(TItem.Create(0));
  FList.Add(TItem.Create(1));
  FList.Add(TItem.Create(2));
  Assert.AreEqual(0, FList.First.ID);
  Assert.AreEqual(2, FList.Last.ID);
end;

{ TLinkedElementTest }

procedure TLinkedElementTest.Setup;
begin
  FElement := TLinkedElement<Integer>.Create;
end;

procedure TLinkedElementTest.TearDown;
begin
  FElement.Free;
  FElement := nil;
end;

procedure TLinkedElementTest.ClassTypeTest;
var
  LObj, LItem: TObjElement;
begin
  LObj := TObjElement.Create;
  try
    Assert.AreEqual(0, LObj.Length);
    LObj.Add([0]);
    LObj.Add([1]);
    LObj.Add([2]);
    LObj.Add([3]);
    Assert.AreEqual(4, LObj.Length);
    LItem := LObj.First;
    Assert.AreEqual(0, LItem.ID);
    Assert.AreEqual(0, LObj.ID);
    Assert.AreEqual(1, LObj.Next.ID);
    LObj.MoveNext(LItem);
    Assert.AreEqual(2, LItem.ID);
    Assert.AreEqual(3, LObj.Next.ID);
    Assert.AreEqual(0, LObj.First.ID);
    Assert.IsTrue(LObj.SoE);
    Assert.IsFalse(LObj.EoE);
    Assert.AreEqual(3, LObj.Last.ID);
    Assert.IsFalse(LObj.SoE);
    Assert.IsTrue(LObj.EoE);
    LObj.Prev;
    Assert.AreEqual(2, LObj.ID);
    LObj.Prev;
    Assert.AreEqual(1, LObj.ID);
    LObj.ID := 11;
    Assert.AreEqual(11, LObj.ID);
    LObj.Prev;
    Assert.AreEqual(0, LObj.ID);
    LObj.Next;
    Assert.AreEqual(11, LObj.ID);
    LObj.Next;
    Assert.AreEqual(2, LObj.ID);
    Assert.AreEqual(11, LObj[1].ID, 'property Elements error');
    LObj.Clear;
    Assert.AreEqual(0, LObj.Length);
  finally
    FreeAndNil(LObj);
  end;
end;

procedure TLinkedElementTest.ManagedTypeTest;
var
  LExpected, LValue: Integer;
begin
  FElement.Add(1);
  Assert.AreEqual(1, FElement.Value);
  Assert.AreEqual(1, FElement.Length);
  FElement.Add(2);
  Assert.AreEqual(2, FElement.Value);
  Assert.AreEqual(2, FElement.Length);
  FElement.Add(3);
  Assert.AreEqual(3, FElement.Value);
  Assert.AreEqual(3, FElement.Length);
  LExpected := 1;
  for LValue in FElement do
  begin
    Assert.AreEqual(LExpected, LValue);
    Inc(LExpected);
  end;
  FElement.First;
  Assert.AreEqual(1, FElement.Value);
  Assert.IsTrue(FElement.SoE);
  Assert.IsFalse(FElement.EoE);
  FElement.Next;
  Assert.AreEqual(2, FElement.Value);
  Assert.IsFalse(FElement.SoE);
  Assert.IsFalse(FElement.EoE);
  FElement.Next;
  Assert.AreEqual(3, FElement.Value);
  Assert.IsFalse(FElement.SoE);
  Assert.IsTrue(FElement.EoE);
  FElement.Prev;
  Assert.AreEqual(2, FElement.Value);
  Assert.IsFalse(FElement.SoE);
  Assert.IsFalse(FElement.EoE);
  FElement.Prev;
  Assert.AreEqual(1, FElement.Value);
  Assert.IsFalse(FElement.EoE);
  Assert.IsTrue(FElement.SoE);

  LExpected := 1;
  for LValue in FElement do
  begin
    Assert.AreEqual(LExpected, LValue);
    Inc(LExpected);
  end;
end;

{ TLinkedElementTest.TObjElement }

constructor TLinkedElementTest.TObjElement.Create(AID: Integer);
begin
  FID := AID;
end;

function TLinkedElementTest.TObjElement.GetID: Integer;
begin
  if IsRootElement then
    Result := FCurrent.FValue.FID
  else
    Result := FID;
end;

procedure TLinkedElementTest.TObjElement.SetID(const Value: Integer);
begin
  if IsRootElement then
    FCurrent.FValue.FID := Value
  else
    FID := Value
end;

initialization
  TDUnitX.RegisterTestFixture(TLinkedListTest);
  TDUnitX.RegisterTestFixture(TLinkedElementTest);

end.
