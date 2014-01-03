unit vm.List;

interface

uses
  System.SysUtils, System.Classes,
  System.Generics.Collections;

type
  EGenericAlreadyRegistered = class(Exception);
  EGenricItemNotRegistered = class(Exception);
  TvmList = class
  private
    FList: TDictionary<String, TObject>;

    constructor Create;

    function Key<T: class>: String;
    procedure OnValueNotify(Sender: TObject; const Item: TObject; Action: TCollectionNotification);
  public
    destructor Destroy; override;

    procedure Add(GenricInstance: TObject);
    procedure Remove<T: class>;
    function Get<T: class>: T;
    function Exists<T: class>: Boolean;
  end;

var
  vmList: TvmList = nil;

implementation

uses
  System.Rtti, System.TypInfo;

{ TvmList }

procedure TvmList.Add(GenricInstance: TObject);
var
  LKey: String;
begin
  if not Assigned(GenricInstance) then
    raise EArgumentNilException.Create('Cannot add a nil instance');

  LKey := GenricInstance.ClassName;
  if FList.ContainsKey(LKey) then
    raise EGenericAlreadyRegistered.Create('Genric Item already registered');

  FList.Add(LKey, GenricInstance);
end;

function TvmList.Exists<T>: Boolean;
begin
  Result := FList.ContainsKey(Key<T>);
end;

function TvmList.Get<T>: T;
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

function TvmList.Key<T>: String;
var
  LTypeInfo: PTypeInfo;
  LRttiContext: TRttiContext;
  LType: TRttiType;
begin
  LTypeInfo := TypeInfo(T);
  LType := LRttiContext.GetType(LTypeInfo);
  Result := Format('%s', [LType.Name]);
end;

constructor TvmList.Create;
begin
  FList := TDictionary<String, TObject>.Create;
  FList.OnValueNotify := OnValueNotify;
end;

destructor TvmList.Destroy;
begin
  FreeAndNil(FList);

  inherited;
end;

procedure TvmList.OnValueNotify(Sender: TObject; const Item: TObject;
  Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    if Assigned(Item) then
      Item.Free;
end;

procedure TvmList.Remove<T>;
var
  LKey: String;
begin
  LKey := Key<T>;
  if FList.ContainsKey(LKey) then
    FList.Remove(LKey);
end;

initialization
  if not Assigned(vmList) then
    vmList := TvmList.Create;

finalization
  if Assigned(vmList) then
  begin
    vmList.Free;
    vmList := nil;
  end;

end.
