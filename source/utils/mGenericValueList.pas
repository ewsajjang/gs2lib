unit mGenericValueList;

interface

uses
  System.Classes, System.SysUtils, System.Rtti,
  System.Generics.Collections
  ;

type
  EGenericSimpleDictionary = class(Exception);
    EMatchedValueNotExists = class(EGenericSimpleDictionary);
  TGenericValueList = class
  private
    FDic: TDictionary<String, TValue>;
    FOwnsClassInstance: Boolean;
    function GetCount: Integer;
    procedure OnValueNotify(Sender: TObject; const Item: TValue;
      Action: TCollectionNotification);
  public
    constructor Create(const AOwnsClassInstance: Boolean = True);
    destructor Destroy; override;

    function Get<T>(const AName: String): T; overload;
    procedure Add<T>(const AName: String; const AValue: T); overload;
    procedure Remove(const AName: String);
    function ContainsName(Name: String): Boolean;

    property Count: Integer read GetCount;
  end;

implementation

{ TGenericSimpleDictionary }

procedure TGenericValueList.Add<T>(const AName: String; const AValue: T);
begin
  FDic.AddOrSetValue(AName, TValue.From<T>(AValue));
end;

function TGenericValueList.Get<T>(const AName: String): T;
var
  LValue: TValue;
begin
  if FDic.TryGetValue(AName, LValue) then
    Result := LValue.AsType<T>
  else
    raise EMatchedValueNotExists.Create('Error Message');
end;

constructor TGenericValueList.Create(const AOwnsClassInstance: Boolean);
begin
  FOwnsClassInstance := AOwnsClassInstance;

  FDic := TDictionary<String, TValue>.Create;
  FDic.OnValueNotify := OnValueNotify;
end;

destructor TGenericValueList.Destroy;
begin
  FreeAndNil(FDic);

  inherited;
end;

function TGenericValueList.GetCount: Integer;
begin
  Result := FDic.Count;
end;

function TGenericValueList.ContainsName(Name: String): Boolean;
begin
  Result := FDic.ContainsKey(Name);
end;

procedure TGenericValueList.OnValueNotify(Sender: TObject; const Item: TValue;
  Action: TCollectionNotification);
begin
  if FOwnsClassInstance and (Action = cnRemoved) then
    if Item.IsObject then
      Item.AsObject.Free;
end;

procedure TGenericValueList.Remove(const AName: String);
begin
  FDic.Remove(AName);
end;

end.
