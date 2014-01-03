unit mEvents;

interface

uses
  System.SysUtils, System.Classes, System.Rtti,
  System.Generics.Collections;

type
  TEventNotify = String;
  EEvent = class(Exception);
    EEventQueryItemNotExists = class(EEvent);
    EEventQueryFuncNotAssigned = class(EEvent);
  TEvent = class
  private
    FValue: TValue;
    FValue2: TValue;
    FList: TDictionary<TEventNotify, TList<TProc>>;
    FBoolList: TDictionary<TEventNotify, TFunc<Boolean>>;
    FGenericList: TDictionary<TEventNotify, TFunc<TValue>>;

    constructor Create;
    procedure OnNotify(Sender: TObject; const Item: TList<TProc>; Action: TCollectionNotification);
  public
    destructor Destroy; override;

    procedure Clear;

    procedure Notify(en: TEventNotify); overload;
    procedure Notify<T>(en: TEventNotify; const Data: T); overload;
    procedure Notify<T, T2>(en: TEventNotify; const Data: T; Data2: T2); overload;
    procedure Handler(en: TEventNotify; Proc: TProc); overload;
    procedure Handler(enArray: array of TEventNotify; Proc: TProc); overload;
    procedure RemoveHandler(en: TEventNotify; Proc: TProc); overload;

    function Excute(en: TEventNotify): Boolean; overload;
    function Excute<T>(en: TEventNotify; const Data: T): Boolean; overload;
    procedure Handler(en: TEventNotify; Func: TFunc<Boolean>); overload;
    procedure RemoveHandler(en: TEventNotify); overload;

    function Data<T>: T; overload;
    function Data2<T>: T; overload;
    procedure Data<T>(AValue: T); overload;
    procedure Data2<T>(AValue: T); overload;

    function Query<TResult>(en: TEventNotify; var Value: TResult): Boolean; overload;
    function Query<TResult>(en: TEventNotify): TResult; overload;
    procedure Feed<TResult>(en: TEventNotify; Func: TFunc<TResult>); overload;
  end;

var
  Event: TEvent = nil;

implementation

procedure TEvent.Handler(en: TEventNotify; Proc: TProc);
var
  LList: TList<TProc>;
begin
  if not FList.ContainsKey(en) then
    FList.Add(en, TList<TProc>.Create);
  LList := FList.Items[en];
  if LList.IndexOf(Proc) = -1 then
    LList.Add(Proc);
end;

procedure TEvent.Clear;
begin
  FGenericList.Clear;
  FBoolList.Clear;
  FList.Clear;
end;

constructor TEvent.Create;
begin
  FList := TDictionary<TEventNotify, TList<TProc>>.Create;
  FList.OnValueNotify := OnNotify;

  FBoolList := TDictionary<TEventNotify, TFunc<Boolean>>.Create;

  FGenericList := TDictionary<TEventNotify, TFunc<TValue>>.Create;
end;

function TEvent.Data<T>: T;
begin
  Result := FValue.AsType<T>;
end;

function TEvent.Data2<T>: T;
begin
  Result := FValue2.AsType<T>;
end;

procedure TEvent.Data2<T>(AValue: T);
begin
  FValue2 := TValue.From<T>(AValue);
end;

procedure TEvent.Data<T>(AValue: T);
begin
  FValue := TValue.From<T>(AValue);
end;

destructor TEvent.Destroy;
begin
  FreeAndNil(FGenericList);
  FreeAndNil(FBoolList);
  FreeAndNil(FList);

  inherited;
end;

procedure TEvent.Handler(en: TEventNotify; Func: TFunc<Boolean>);
begin
  if FBoolList.ContainsKey(en) then
    raise EEvent.CreateFmt('[%s]EventItem is already exists', [en]);

  FBoolList.Add(en, Func);
end;

procedure TEvent.Feed<TResult>(en: TEventNotify; Func: TFunc<TResult>);
begin
  if FGenericList.ContainsKey(en) then
    raise EEvent.CreateFmt('[%s]Item is already exists', [en]);


  if not Assigned(Func) then
    raise EEventQueryFuncNotAssigned.CreateFmt('[%s]Item query function does not assigned', [en]);

  FGenericList.Add(en,
    function: TValue
    begin
      Result := TValue.From<TResult>(Func);
    end);
end;

procedure TEvent.Handler(enArray: array of TEventNotify; Proc: TProc);
var
  Len: TEventNotify;
begin
  for Len in enArray do
    Handler(Len, Proc);
end;

procedure TEvent.Notify(en: TEventNotify);
var
  LList: TList<TProc>;
  LProc: TProc;
begin
  if FList.TryGetValue(en, LList) then
    for LProc in LList do
      if Assigned(LProc) then
        LProc();
end;

procedure TEvent.Notify<T, T2>(en: TEventNotify; const Data: T; Data2: T2);
begin
  FValue := TValue.From<T>(Data);
  FValue2 := TValue.From<T2>(Data2);
  Notify(en);
end;

procedure TEvent.Notify<T>(en: TEventNotify; const Data: T);
begin
  FValue := TValue.From<T>(Data);
  Notify(en);
end;

function TEvent.Excute(en: TEventNotify): Boolean;
var
  LFunc: TFunc<Boolean>;
begin
  Result := False;

  if FBoolList.TryGetValue(en, LFunc) then
    if Assigned(LFunc) then
      Result := LFunc();
end;

function TEvent.Excute<T>(en: TEventNotify; const Data: T): Boolean;
begin
  FValue := TValue.From<T>(Data);
  Result := Excute(en);
end;

procedure TEvent.RemoveHandler(en: TEventNotify);
begin
  if FBoolList.ContainsKey(en) then
    FBoolList.Remove(en);
end;

procedure TEvent.OnNotify(Sender: TObject; const Item: TList<TProc>;
  Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    Item.Free;
end;

function TEvent.Query<TResult>(en: TEventNotify): TResult;
var
  LFunc: TFunc<TValue>;
begin
  if FGenericList.TryGetValue(en, LFunc) then
  begin
    if Assigned(LFunc) then
      Result := LFunc.AsType<TResult>
  end
  else
    raise EEventQueryItemNotExists.CreateFmt('[%s]Item does not exists', [en]);
end;

function TEvent.Query<TResult>(en: TEventNotify; var Value: TResult): Boolean;
var
  LFunc: TFunc<TValue>;
begin
  Result := False;
  if FGenericList.TryGetValue(en, LFunc) then
    if Assigned(LFunc) then
    begin
      Value := LFunc.AsType<TResult>;
      Result := True;
    end;
end;

procedure TEvent.RemoveHandler(en: TEventNotify; Proc: TProc);
var
  LList: TList<TProc>;
begin
  if FList.TryGetValue(en, LList) then
    LList.Remove(Proc);
end;

initialization
  if not Assigned(Event) then
    Event := TEvent.Create;

finalization
  if Assigned(Event) then
  begin
    Event.Free;
    Event := nil;
  end;

end.
