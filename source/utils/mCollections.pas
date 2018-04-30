unit mCollections;

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults
  ;

type
  TExecuteProc<TValue> = reference to procedure(const AValue: TValue);
  TThreadedDictionary<TKey, TValue> = class
  private
    FDic: TDictionary<TKey, TValue>;
    FGuard: TObject;
    function GetCount: Integer;
    function GetItem(const Key: TKey): TValue;
    procedure SetItem(const Key: TKey; const Value: TValue);
    function GetKeys: TArray<TKey>;
    function GetValues: TArray<TValue>;
  protected
    procedure Lock;
    procedure Unlock;
    function GetOnKeyNotify: TCollectionNotifyEvent<TKey>; virtual;
    procedure SetOnKeyNotify(const Value: TCollectionNotifyEvent<TKey>); virtual;
    function GetOnValueNotify: TCollectionNotifyEvent<TValue>; virtual;
    procedure SetOnValueNotify(const Value: TCollectionNotifyEvent<TValue>); virtual;
  public
    constructor Create(ACapacity: Integer = 0); overload;
    constructor Create(const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(ACapacity: Integer; const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(const Collection: TEnumerable<TPair<TKey,TValue>>); overload;
    constructor Create(const Collection: TEnumerable<TPair<TKey,TValue>>; const AComparer: IEqualityComparer<TKey>); overload;

    destructor Destroy; override;

    procedure Execute(const AKey: TKey; AExeProc: TExecuteProc<TValue>);
    procedure Add(const Key: TKey; const Value: TValue);
    procedure Remove(const Key: TKey);
    function ExtractPair(const Key: TKey): TPair<TKey,TValue>;
    procedure Clear;
    procedure TrimExcess;
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    procedure AddOrSetValue(const Key: TKey; const Value: TValue);
    function ContainsKey(const Key: TKey): Boolean;
    function ContainsValue(const Value: TValue): Boolean;
    function ToArray: TArray<TPair<TKey,TValue>>;

    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
    property Count: Integer read GetCount;

    property Keys: TArray<TKey> read GetKeys;
    property Values: TArray<TValue> read GetValues;
    property OnKeyNotify: TCollectionNotifyEvent<TKey> read GetOnKeyNotify write SetOnKeyNotify;
    property OnValueNotify: TCollectionNotifyEvent<TValue> read GetOnValueNotify write SetOnValueNotify;

    function GetEnumerator: TEnumerator<TPair<TKey,TValue>>; reintroduce;
  end;

  TThreadedObjectDictionary<TKey, TValue> = class(TThreadedDictionary<TKey, TValue>)
  private
    FOwnerships: TDictionaryOwnerships;
    FOnKeyNotify: TCollectionNotifyEvent<TKey>;
    FOnValueNotify: TCollectionNotifyEvent<TValue>;
    procedure KeyNotify(Sender: TObject; const Key: TKey; Action: TCollectionNotification);
    procedure ValueNotify(Sender: TObject; const Value: TValue; Action: TCollectionNotification);
  protected
    function GetOnKeyNotify: TCollectionNotifyEvent<TKey>; override;
    procedure SetOnKeyNotify(const Value: TCollectionNotifyEvent<TKey>); override;
    function GetOnValueNotify: TCollectionNotifyEvent<TValue>; override;
    procedure SetOnValueNotify(const Value: TCollectionNotifyEvent<TValue>); override;
  public
    constructor Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer = 0); overload;
    constructor Create(Ownerships: TDictionaryOwnerships; const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer; const AComparer: IEqualityComparer<TKey>); overload;
  end;

implementation

uses
  Rtti, TypInfo, SysConst
  ;

{ TThreadedDictionary<TKey, TValue> }

constructor TThreadedDictionary<TKey, TValue>.Create(ACapacity: Integer);
begin
  Create(ACapacity, nil);
end;

constructor TThreadedDictionary<TKey, TValue>.Create(const AComparer: IEqualityComparer<TKey>);
begin
  Create(0, AComparer);
end;

constructor TThreadedDictionary<TKey, TValue>.Create(ACapacity: Integer; const AComparer: IEqualityComparer<TKey>);
begin
  FGuard := TObject.Create;
  FDic := TDictionary<TKey, TValue>.Create(ACapacity, AComparer);
end;

procedure TThreadedDictionary<TKey, TValue>.Add(const Key: TKey; const Value: TValue);
begin
  Lock;
  try
    FDic.Add(Key, Value);
  finally
    Unlock;
  end;
end;

procedure TThreadedDictionary<TKey, TValue>.AddOrSetValue(const Key: TKey; const Value: TValue);
begin
  Lock;
  try
    FDic.AddOrSetValue(Key, Value);
  finally
    Unlock;
  end;
end;

procedure TThreadedDictionary<TKey, TValue>.Clear;
begin
  Lock;
  try
    FDic.Clear;
  finally
    Unlock;
  end;
end;

function TThreadedDictionary<TKey, TValue>.ContainsKey(const Key: TKey): Boolean;
begin
  Lock;
  try
    Result := FDic.ContainsKey(Key);
  finally
    Unlock;
  end;
end;

function TThreadedDictionary<TKey, TValue>.ContainsValue(const Value: TValue): Boolean;
begin
  Lock;
  try
    Result := FDic.ContainsValue(Value);
  finally
    Unlock;
  end;
end;

constructor TThreadedDictionary<TKey, TValue>.Create(const Collection: TEnumerable<TPair<TKey, TValue>>;
  const AComparer: IEqualityComparer<TKey>);
var
  LItem: TPair<TKey,TValue>;
begin
  Create(0, AComparer);
  Lock;
  try
    for LItem in Collection do
      FDic.AddOrSetValue(LItem.Key, LItem.Value);
  finally
    Unlock;
  end;
end;

constructor TThreadedDictionary<TKey, TValue>.Create(const Collection: TEnumerable<TPair<TKey, TValue>>);
var
  LItem: TPair<TKey,TValue>;
begin
  Create(0, nil);
  Lock;
  try
    for LItem in Collection do
      FDic.AddOrSetValue(LItem.Key, LItem.Value);
  finally
    Unlock;
  end;
end;

destructor TThreadedDictionary<TKey, TValue>.Destroy;
begin
  FreeAndNil(FDic);
  FreeAndNil(FGuard);

  inherited;
end;

function TThreadedDictionary<TKey, TValue>.GetEnumerator: TEnumerator<TPair<TKey, TValue>>;
begin
  Result := FDic.GetEnumerator
end;

procedure TThreadedDictionary<TKey, TValue>.Execute(const AKey: TKey; AExeProc: TExecuteProc<TValue>);
begin
  Lock;
  try
    if FDic.ContainsKey(AKey) then
      AExeProc(FDic[AKey]);
  finally
    Unlock;
  end;
end;

function TThreadedDictionary<TKey, TValue>.ExtractPair(const Key: TKey): TPair<TKey, TValue>;
begin
  Lock;
  try
    Result := FDic.ExtractPair(Key);
  finally
    Unlock;
  end;
end;

function TThreadedDictionary<TKey, TValue>.GetCount: Integer;
begin
  Lock;
  try
    Result := FDic.Count;
  finally
    Unlock;
  end;
end;

function TThreadedDictionary<TKey, TValue>.GetItem(const Key: TKey): TValue;
begin
  Lock;
  try
    Result := FDic[Key];
  finally
    Unlock;
  end;
end;

function TThreadedDictionary<TKey, TValue>.GetKeys: TArray<TKey>;
var
  i: Integer;
  LItem: TPair<TKey,TValue>;
begin
  Lock;
  try
    SetLength(Result, Count);
    i := 0;
    for LItem in FDic do
    begin
      Result[i] := LItem.Key;
      Inc(i);
    end;
  finally
    Unlock;
  end;
end;

function TThreadedDictionary<TKey, TValue>.GetOnKeyNotify: TCollectionNotifyEvent<TKey>;
begin
  Lock;
  try
    Result := FDic.OnKeyNotify;
  finally
    Unlock;
  end;
end;

function TThreadedDictionary<TKey, TValue>.GetOnValueNotify: TCollectionNotifyEvent<TValue>;
begin
  Lock;
  try
    Result := FDic.OnValueNotify;
  finally
    Unlock;
  end;
end;

function TThreadedDictionary<TKey, TValue>.GetValues: TArray<TValue>;
var
  i: Integer;
  LItem: TPair<TKey,TValue>;
begin
  Lock;
  try
    SetLength(Result, Count);
    i := 0;
    for LItem in FDic do
    begin
      Result[i] := LItem.Value;
      Inc(i);
    end;
  finally
    Unlock;
  end;
end;

procedure TThreadedDictionary<TKey, TValue>.Lock;
begin
  TMonitor.Enter(FGuard);
end;

procedure TThreadedDictionary<TKey, TValue>.Remove(const Key: TKey);
begin
  Lock;
  try
    FDic.Remove(Key);
  finally
    Unlock;
  end;
end;

procedure TThreadedDictionary<TKey, TValue>.SetItem(const Key: TKey; const Value: TValue);
begin
  Lock;
  try
    FDic[Key] := Value;
  finally
    Unlock;
  end;
end;

procedure TThreadedDictionary<TKey, TValue>.SetOnKeyNotify(const Value: TCollectionNotifyEvent<TKey>);
begin
  Lock;
  try
    FDic.OnKeyNotify := Value;
  finally
    Unlock;
  end;
end;

procedure TThreadedDictionary<TKey, TValue>.SetOnValueNotify(const Value: TCollectionNotifyEvent<TValue>);
begin
  Lock;
  try
    FDic.OnValueNotify := Value;
  finally
    Unlock;
  end;
end;

function TThreadedDictionary<TKey, TValue>.ToArray: TArray<TPair<TKey, TValue>>;
begin
  Lock;
  try
    Result := FDic.ToArray;
  finally
    Unlock;
  end;
end;

procedure TThreadedDictionary<TKey, TValue>.TrimExcess;
begin
  Lock;
  try
    FDic.TrimExcess;
  finally
    Unlock;
  end;
end;

function TThreadedDictionary<TKey, TValue>.TryGetValue(const Key: TKey; out Value: TValue): Boolean;
begin
  Lock;
  try
    Result := FDic.TryGetValue(Key, Value);
  finally
    Unlock;
  end;
end;

procedure TThreadedDictionary<TKey, TValue>.Unlock;
begin
  TMonitor.Exit(FGuard);
end;

{ TThreadedObjectDictionary<TKey, TValue> }

constructor TThreadedObjectDictionary<TKey, TValue>.Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer);
begin
  Create(Ownerships, ACapacity, nil);
end;

constructor TThreadedObjectDictionary<TKey, TValue>.Create(Ownerships: TDictionaryOwnerships;
  const AComparer: IEqualityComparer<TKey>);
begin
  Create(Ownerships, 0, AComparer);
end;

constructor TThreadedObjectDictionary<TKey, TValue>.Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer;
  const AComparer: IEqualityComparer<TKey>);
begin
  inherited Create(ACapacity, AComparer);

  if doOwnsKeys in Ownerships then
  begin
    if (TypeInfo(TKey) = nil) or (PTypeInfo(TypeInfo(TKey))^.Kind <> tkClass) then
      raise EInvalidCast.CreateRes(@SInvalidCast);
  end;

  if doOwnsValues in Ownerships then
  begin
    if (TypeInfo(TValue) = nil) or (PTypeInfo(TypeInfo(TValue))^.Kind <> tkClass) then
      raise EInvalidCast.CreateRes(@SInvalidCast);
  end;
  FOwnerships := Ownerships;
  FDic.OnKeyNotify := KeyNotify;
  FDic.OnValueNotify := ValueNotify;
end;

function TThreadedObjectDictionary<TKey, TValue>.GetOnKeyNotify: TCollectionNotifyEvent<TKey>;
begin
  Lock;
  try
    Result := FOnKeyNotify;
  finally
    Unlock;
  end;
end;

function TThreadedObjectDictionary<TKey, TValue>.GetOnValueNotify: TCollectionNotifyEvent<TValue>;
begin
  Lock;
  try
    Result := FOnValueNotify;
  finally
    Unlock;
  end;
end;

procedure TThreadedObjectDictionary<TKey, TValue>.KeyNotify(Sender: TObject; const Key: TKey; Action: TCollectionNotification);
begin
  if Assigned(FOnKeyNotify) then
    FOnKeyNotify(Self, Key, Action);

  if (Action = cnRemoved) and (doOwnsKeys in FOwnerships) then
    PObject(@Key)^.DisposeOf;
end;

procedure TThreadedObjectDictionary<TKey, TValue>.SetOnKeyNotify(const Value: TCollectionNotifyEvent<TKey>);
begin
  Lock;
  try
    FOnKeyNotify := Value;
  finally
    Unlock;
  end;
end;

procedure TThreadedObjectDictionary<TKey, TValue>.SetOnValueNotify(const Value: TCollectionNotifyEvent<TValue>);
begin
  Lock;
  try
    FOnValueNotify := Value;
  finally
    Unlock;
  end;
end;

procedure TThreadedObjectDictionary<TKey, TValue>.ValueNotify(Sender: TObject; const Value: TValue; Action: TCollectionNotification);
begin
  if Assigned(FOnValueNotify) then
    FOnValueNotify(Self, Value, Action);

  if (Action = cnRemoved) and (doOwnsValues in FOwnerships) then
    PObject(@Value)^.DisposeOf;
end;

end.

