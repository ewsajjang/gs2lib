unit TestmLinkedList;

interface

uses
  System.Classes, System.SysUtils, mLinkedList,

  DUnitX.TestFramework;

type
  [TestFixture]
  TObjectLinkedListTest = class
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

  [IgnoreMemoryLeaks(False)]
  TLinkedElementTest = class
  private
    FElement: TLinkedElement<Integer>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [TestCase]
    [IgnoreMemoryLeaks(False)]
    procedure Test;
  end;

  [IgnoreMemoryLeaks(False)]
  TObjectLinkedElementTest = class
  private type
    TItem = class
    public
      class var FCnt: Integer;
    public
      ID: Integer;
      constructor Create(AID: Integer);
      destructor Destroy; override;
    end;
  private
    FElement: ILinkedElement<TItem>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [TestCase]
    procedure Test;
  end;

implementation

var
  FCnt: Integer;
{ LinkedListTest.TItem }

constructor TObjectLinkedListTest.TItem.Create(const AID: Integer);
begin
  inherited Create;

  ID := AID;
  Inc(FCnt);
end;

destructor TObjectLinkedListTest.TItem.Destroy;
begin
  Dec(FCnt);

  inherited;
end;

{ LinkedListTest }

procedure TObjectLinkedListTest.AddFirstTest;
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

procedure TObjectLinkedListTest.Setup;
begin
  FList := TObjectLinkedList<TItem>.Create;
  FCnt := 0;
end;

procedure TObjectLinkedListTest.TearDown;
begin
  FList := nil;
  Assert.AreEqual(0, FCnt);
end;

procedure TObjectLinkedListTest.ToArrayTest;
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

procedure TObjectLinkedListTest.ToFieldArrayTest;
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

procedure TObjectLinkedListTest.AddTest;
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

procedure TObjectLinkedListTest.NodeNextPrev;
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

procedure TObjectLinkedListTest.FirstLastNodeTest;
begin
  FList.Add(TItem.Create(0));
  FList.Add(TItem.Create(1));
  FList.Add(TItem.Create(2));
  Assert.AreEqual(0, FList.FirstNode.Value.ID);
  Assert.AreEqual(2, FList.LastNode.Value.ID);
end;

procedure TObjectLinkedListTest.FirstLastTest;
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

procedure TLinkedElementTest.Test;
var
  LExpected, LValue: Integer;
begin
  FElement.Add(1);
  Assert.AreEqual(1, FElement.Value);
  Assert.AreEqual(1, FElement.Count);
  FElement.Add(2);
  Assert.AreEqual(2, FElement.Value);
  Assert.AreEqual(2, FElement.Count);
  FElement.Add(3);
  Assert.AreEqual(3, FElement.Value);
  Assert.AreEqual(3, FElement.Count);
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
  for LValue in FElement.ToArray do
  begin
    Assert.AreEqual(LExpected, LValue);
    Inc(LExpected);
  end;
end;

{ TObjectLinkedElementTest.TItem }

constructor TObjectLinkedElementTest.TItem.Create(AID: Integer);
begin
  ID := AID;
  Inc(FCnt);
end;

destructor TObjectLinkedElementTest.TItem.Destroy;
begin
  Dec(FCnt);

  inherited
end;

{ TObjectLinkedElementTest }

procedure TObjectLinkedElementTest.Setup;
begin
  TItem.FCnt := 0;
  FElement := TObjectLinkedElement<TItem>.Create(True);
end;

procedure TObjectLinkedElementTest.TearDown;
begin
//  FreeAndNil(FElement);
  FElement := nil;
  Assert.AreEqual(0, TItem.FCnt);
end;

procedure TObjectLinkedElementTest.Test;
var
  LExpected: Integer;
  LValue: TItem;
begin
  FElement.Add(TItem.Create(1));
  Assert.AreEqual(1, FElement.Value.ID);
  Assert.AreEqual(1, FElement.Count);
  FElement.Add(TItem.Create(2));
  Assert.AreEqual(2, FElement.Value.ID);
  Assert.AreEqual(2, FElement.Count);
  FElement.Add(TItem.Create(3));
  Assert.AreEqual(3, FElement.Value.ID);
  Assert.AreEqual(3, FElement.Count);
  LExpected := 1;
  for LValue in FElement do
  begin
    Assert.AreEqual(LExpected, LValue.ID);
    Inc(LExpected);
  end;
  FElement.First;
  Assert.AreEqual(1, FElement.Value.ID);
  Assert.IsTrue(FElement.SoE);
  Assert.IsFalse(FElement.EoE);
  FElement.Next;
  Assert.AreEqual(2, FElement.Value.ID);
  Assert.IsFalse(FElement.SoE);
  Assert.IsFalse(FElement.EoE);
  FElement.Next;
  Assert.AreEqual(3, FElement.Value.ID);
  Assert.IsFalse(FElement.SoE);
  Assert.IsTrue(FElement.EoE);
  FElement.Prev;
  Assert.AreEqual(2, FElement.Value.ID);
  Assert.IsFalse(FElement.SoE);
  Assert.IsFalse(FElement.EoE);
  FElement.Prev;
  Assert.AreEqual(1, FElement.Value.ID);
  Assert.IsFalse(FElement.EoE);
  Assert.IsTrue(FElement.SoE);

  LExpected := 1;
  for LValue in FElement.ToArray do
  begin
    Assert.AreEqual(LExpected, LValue.ID);
    Inc(LExpected);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TObjectLinkedListTest);
  TDUnitX.RegisterTestFixture(TLinkedElementTest);
  TDUnitX.RegisterTestFixture(TObjectLinkedElementTest);

end.
