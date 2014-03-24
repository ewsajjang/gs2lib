unit TestmGenericValueList;

interface

uses
  mGenericValueList,
  System.Classes, System.Generics.Collections, System.SysUtils,
  DUnitX.TestFramework;

type
  TestGenericSimpleDictionary = class
  private
    FList: TGenericValueList;
  public
    [setup]
    procedure setup;
    [tearDown]
    procedure tearDown;
  public
    [test]
    procedure TestAddNonObject;
    [test]
    procedure TestAddObject;
    [test]
    procedure TestAddObjectFreeExcute;
    [test]
    procedure TestRemoveThenContatins;
    [test]
    procedure TestContains;
    [test]
    procedure TestCntOfSameNames;
    [test]
    procedure TestCnt;
    [test]
    procedure TestAddAndRemoveCnt;
  end;

implementation

{ TestGenericSimpleDictionary }

procedure TestGenericSimpleDictionary.TestRemoveThenContatins;
begin
  FList.Add<TObject>('abc', TObject.Create);
  FList.Remove('abc');
  Assert.IsFalse(FList.ContainsName('abc'));
end;

procedure TestGenericSimpleDictionary.TestAddNonObject;
begin
  FList.Add<Integer>('a', 1);
  Assert.AreEqual(1, FList.Get<Integer>('a'));
end;

procedure TestGenericSimpleDictionary.TestAddObject;
begin
  FList.Add<TObject>('a', TObject.Create);
  Assert.IsNotNull( FList.Get<TObject>('a'));
end;

type
  TObj = class
  public class var
    DestroyExcute: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TObj }

constructor TObj.Create;
begin
  DestroyExcute := False;
end;

destructor TObj.Destroy;
begin
  DestroyExcute := True;

  inherited;
end;

procedure TestGenericSimpleDictionary.TestAddObjectFreeExcute;
begin
  FList.Add<TObj>('abc', TObj.Create);
  FList.Remove('abc');
  Assert.IsTrue(TObj.DestroyExcute);
end;

procedure TestGenericSimpleDictionary.setup;
begin
  FList := TGenericValueList.Create;
end;

procedure TestGenericSimpleDictionary.tearDown;
begin
  FList.Free;
  FList := nil;
end;

procedure TestGenericSimpleDictionary.TestAddAndRemoveCnt;
begin
  FList.Add<TObject>('abc', TObject.Create);
  FList.Add<TObject>('abcd', TObj.Create);
  FList.Add<TObject>('abcdf', TObj.Create);
  FList.Remove('abc');
  Assert.AreEqual(2, FList.Count);
  Assert.IsFalse(FList.ContainsName('abc'));
end;

procedure TestGenericSimpleDictionary.TestCnt;
begin
  FList.Add<TObject>('abc', TObject.Create);
  FList.Add<TObject>('abcd', TObj.Create);
  Assert.AreEqual(2, FList.Count);
end;

procedure TestGenericSimpleDictionary.TestCntOfSameNames;
begin
  FList.Add<TObject>('abc', TObject.Create);
  FList.Add<TObject>('abc', TObj.Create);
  Assert.AreEqual(1, FList.Count);
end;

procedure TestGenericSimpleDictionary.TestContains;
begin
  FList.Add<TObject>('abc', TObject.Create);
  Assert.IsTrue(FList.ContainsName('abc'));
end;

initialization
  TDUnitX.RegisterTestFixture(TestGenericSimpleDictionary);

end.
