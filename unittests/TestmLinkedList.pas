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
      FList: TObjectLinkedList<TItem>;
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
    procedure FirstLastElementTest;
    [TestCase]
    procedure ElementNextPrev;
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
  LElement: TLinkedElement<TItem>;
begin
  FList.Clear;
  FList.AddFirst(TItem.Create(0));
  FList.AddFirst(TItem.Create(1));
  FList.AddFirst(TItem.Create(2));

  LElement := FList.FirstElement;
  Assert.AreEqual(2, LElement.Value.ID);
  LElement := LElement.Next;
  Assert.AreEqual(1, LElement.Value.ID);
  LElement := LElement.Next;
  Assert.AreEqual(0, LElement.Value.ID);
end;

procedure TObjectLinkedListTest.Setup;
begin
  FList := TObjectLinkedList<TItem>.Create;
  FCnt := 0;
end;

procedure TObjectLinkedListTest.TearDown;
begin
  FList.Free;
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
  LElements: array[0..3] of TLinkedElement<TItem>;
  LArray: TArray<TLinkedElement<TItem>>;
begin
  LElements[0] := TLinkedElement<TItem>.Create(TItem.Create(0));
  LElements[1] := TLinkedElement<TItem>.Create(TItem.Create(1));
  LElements[2] := TLinkedElement<TItem>.Create(TItem.Create(2));
  FList.Add(LElements[0]);
  FList.Add(LElements[1]);
  FList.Add(LElements[2]);
  LArray := FList.ToElementArray;
  Assert.AreEqual(3, Length(LArray));
  Assert.AreSame(LElements[0], LArray[0]);
  Assert.AreSame(LElements[1], LArray[1]);
  Assert.AreSame(LElements[2], LArray[2]);
end;

procedure TObjectLinkedListTest.AddTest;
var
  LItem: TItem;
  LElement: TLinkedElement<TItem>;
begin
  FList.Add(TItem.Create(0));
  FList.Add(TItem.Create(1));
  FList.Add(TItem.Create(2));
  Assert.AreEqual<NativeUInt>(3, FList.Count);

  LElement := FList.FirstElement;
  LItem := LElement.Value;
  Assert.AreEqual(0, LItem.ID);
  Assert.IsTrue(LElement.SoE);
  Assert.IsFalse(LElement.EoE);

  LElement := LElement.Next;
  LItem := LElement.Value;
  Assert.AreEqual(1, LItem.ID);

  LElement := LElement.Next;
  LItem := LElement.Value;
  Assert.AreEqual(2, LItem.ID);
  Assert.IsFalse(LElement.SoE);
  Assert.IsTrue(LElement.EoE);
end;

procedure TObjectLinkedListTest.ElementNextPrev;
var
  LElement: TLinkedElement<TItem>;
begin
  FList.Add(TItem.Create(0));
  FList.Add(TItem.Create(1));
  FList.Add(TItem.Create(2));
  LElement := FList.FirstElement;
  LElement := LElement.Next;
  Assert.AreEqual(1, LElement.Value.ID);
  LElement := LElement.Prev;
  Assert.AreEqual(0, LElement.Value.ID);
  LElement := LElement.Next;
  Assert.AreEqual(1, LElement.Value.ID);
  LElement := LElement.Next;
  Assert.AreEqual(2, LElement.Value.ID);
  LElement := LElement.Prev;
  Assert.AreEqual(1, LElement.Value.ID);
  LElement := LElement.Prev;
  Assert.AreEqual(0, LElement.Value.ID);
end;

procedure TObjectLinkedListTest.FirstLastElementTest;
begin
  FList.Add(TItem.Create(0));
  FList.Add(TItem.Create(1));
  FList.Add(TItem.Create(2));
  Assert.AreEqual(0, FList.FirstElement.Value.ID);
  Assert.AreEqual(2, FList.LastElement.Value.ID);
end;

procedure TObjectLinkedListTest.FirstLastTest;
begin
  FList.Add(TItem.Create(0));
  FList.Add(TItem.Create(1));
  FList.Add(TItem.Create(2));
  Assert.AreEqual(0, FList.First.ID);
  Assert.AreEqual(2, FList.Last.ID);
end;

initialization
  TDUnitX.RegisterTestFixture(TObjectLinkedListTest);

end.
