unit mGenericClassList;

interface

uses
  System.SysUtils, System.Classes,
  System.Generics.Collections;

type
  EGenericAlreadyRegistered = class(Exception);
  EGenricItemNotRegistered = class(Exception);
  TGenericClassList = class
  private
    FList: TDictionary<String, TObject>;

    function Key<T: class>: String;
    procedure OnValueNotify(Sender: TObject; const Item: TObject; Action: TCollectionNotification);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(AGenricInstance: TObject);
    procedure Remove<T: class>;
    function Get<T: class>: T;
    function Exists<T: class>: Boolean;
  end;

implementation

uses
  System.Rtti, System.TypInfo, vcl.Forms;

{ TGenericClassList }

procedure TGenericClassList.Add(AGenricInstance: TObject);
var
  LKey: String;
begin
  if not Assigned(AGenricInstance) then
    raise EArgumentNilException.Create('Cannot add a nil instance');

  LKey := AGenricInstance.ClassName;
  if FList.ContainsKey(LKey) then
    raise EGenericAlreadyRegistered.Create('Genric Item already registered');

  FList.Add(LKey, AGenricInstance);
end;

function TGenericClassList.Exists<T>: Boolean;
begin
  Result := FList.ContainsKey(Key<T>);
end;

function TGenericClassList.Get<T>: T;
var
  LKey: String;
  LObj: TObject;
begin
  LKey := Key<T>;
  if FList.TryGetValue(LKey, LObj) then
    Result := T(LObj)
  else
    raise EGenricItemNotRegistered.Create(Format('Genric Item %s not registered', [LKey]));
end;

function TGenericClassList.Key<T>: String;
var
  LTypeInfo: PTypeInfo;
  LRttiContext: TRttiContext;
  LType: TRttiType;
begin
  LTypeInfo := TypeInfo(T);
  LType := LRttiContext.GetType(LTypeInfo);
  Result := Format('%s', [LType.Name]);
end;

constructor TGenericClassList.Create;
begin
  FList := TDictionary<String, TObject>.Create;
  FList.OnValueNotify := OnValueNotify;
end;

destructor TGenericClassList.Destroy;
begin
  FreeAndNil(FList);

  inherited;
end;

procedure TGenericClassList.OnValueNotify(Sender: TObject; const Item: TObject;
  Action: TCollectionNotification);
var
  LInf: IInterface;
begin
  if Assigned(Self) then

  if Action = cnRemoved then
    if Assigned(Item) then
      if Supports(Item, IInterface, LInf) then
        LInf := nil
      else if not (Item is TDataModule) then
        Item.Free;
end;

procedure TGenericClassList.Remove<T>;
var
  LKey: String;
begin
  LKey := Key<T>;
  if FList.ContainsKey(LKey) then
    FList.Remove(LKey);
end;

end.
