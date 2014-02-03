unit mRouter;

interface

uses
  System.SysUtils, System.Classes, System.Rtti,
  System.Generics.Collections;

type
  ERouter = class(Exception);
    ERouterMethodIDAlreadyExists = class(ERouter);
    ERouterMethodIDNotExists = class(ERouter);
    ERouterMethodNotAssigned = class(ERouter);

  TRouter<TID> = class
  private const
    FMT_METHOD_ID_ALREADY_EXISTS = '[%s]Method ID is already exists';
    FMT_METHOD_ID_NOT_EXISTS = '[%s]Method ID does not exists';
    FMT_METHOD_NOT_ASSIGNED = '[%s]Method does not assigned';
  private
    FValue: TValue;
    FValue2: TValue;
    FNotifyList: TDictionary<TID, TList<TProc>>;
    FExcuteList: TDictionary<TID, TFunc<Boolean>>;
    FGenericList: TDictionary<TID, TFunc<TValue>>;

    procedure OnNotify(Sender: TObject; const Item: TList<TProc>; Action: TCollectionNotification);
    function RouterKeyToStr(AID: TID): String;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure Notify(AID: TID); overload;
    procedure Notify<T>(AID: TID; const Data: T); overload;
    procedure Notify<T, T2>(AID: TID; const Data: T; Data2: T2); overload;
    procedure Handler(AID: TID; Proc: TProc); overload;
    procedure Handler(enArray: array of TID; Proc: TProc); overload;
    procedure RemoveHandler(AID: TID; Proc: TProc); overload;

    function Excute(AID: TID): Boolean; overload;
    function Excute<T>(AID: TID; const Data: T): Boolean; overload;
    procedure Handler(AID: TID; Func: TFunc<Boolean>); overload;
    procedure RemoveHandler(AID: TID); overload;

    function Data<T>: T; overload;
    function Data2<T>: T; overload;
    procedure Data<T>(AValue: T); overload;
    procedure Data2<T>(AValue: T); overload;

    function Query<T>(AID: TID; var Value: T): Boolean; overload;
    function Query<T>(AID: TID): T; overload;
    procedure Feed<T>(AID: TID; Func: TFunc<T>); overload;
  end;

implementation

uses
  System.TypInfo;

{ TRouter }

procedure TRouter<TID>.Handler(AID: TID; Proc: TProc);
var
  LList: TList<TProc>;
begin
  if not FNotifyList.ContainsKey(AID) then
    FNotifyList.Add(AID, TList<TProc>.Create);
  LList := FNotifyList.Items[AID];
  if LList.IndexOf(Proc) = -1 then
    LList.Add(Proc);
end;

procedure TRouter<TID>.Clear;
begin
  FGenericList.Clear;
  FExcuteList.Clear;
  FNotifyList.Clear;
end;

constructor TRouter<TID>.Create;
begin
  FNotifyList := TDictionary<TID, TList<TProc>>.Create;
  FNotifyList.OnValueNotify := OnNotify;

  FExcuteList := TDictionary<TID, TFunc<Boolean>>.Create;

  FGenericList := TDictionary<TID, TFunc<TValue>>.Create;
end;

function TRouter<TID>.Data<T>: T;
begin
  Result := FValue.AsType<T>;
end;

function TRouter<TID>.Data2<T>: T;
begin
  Result := FValue2.AsType<T>;
end;

procedure TRouter<TID>.Data2<T>(AValue: T);
begin
  FValue2 := TValue.From<T>(AValue);
end;

procedure TRouter<TID>.Data<T>(AValue: T);
begin
  FValue := TValue.From<T>(AValue);
end;

destructor TRouter<TID>.Destroy;
begin
  FreeAndNil(FGenericList);
  FreeAndNil(FExcuteList);
  FreeAndNil(FNotifyList);

  inherited;
end;

procedure TRouter<TID>.Handler(AID: TID; Func: TFunc<Boolean>);
begin
  if FExcuteList.ContainsKey(AID) then
    raise ERouterMethodIDAlreadyExists.CreateFmt(FMT_METHOD_ID_ALREADY_EXISTS, [RouterKeyToStr(AID)]);

  FExcuteList.Add(AID, Func);
end;

procedure TRouter<TID>.Feed<T>(AID: TID; Func: TFunc<T>);
begin
  if FGenericList.ContainsKey(AID) then
    raise ERouterMethodIDAlreadyExists .CreateFmt(FMT_METHOD_ID_ALREADY_EXISTS, [RouterKeyToStr(AID)]);

  if not Assigned(Func) then
    raise ERouterMethodNotAssigned.CreateFmt(FMT_METHOD_NOT_ASSIGNED, [RouterKeyToStr(AID)]);

  FGenericList.Add(AID,
    function: TValue
    begin
      Result := TValue.From<T>(Func);
    end);
end;

procedure TRouter<TID>.Handler(enArray: array of TID; Proc: TProc);
var
  Len: TID;
begin
  for Len in enArray do
    Handler(Len, Proc);
end;

procedure TRouter<TID>.Notify(AID: TID);
var
  LList: TList<TProc>;
  LProc: TProc;
begin
  if FNotifyList.TryGetValue(AID, LList) then
    for LProc in LList do
      if Assigned(LProc) then
        LProc();
end;

procedure TRouter<TID>.Notify<T, T2>(AID: TID; const Data: T; Data2: T2);
begin
  FValue := TValue.From<T>(Data);
  FValue2 := TValue.From<T2>(Data2);
  Notify(AID);
end;

procedure TRouter<TID>.Notify<T>(AID: TID; const Data: T);
begin
  FValue := TValue.From<T>(Data);
  Notify(AID);
end;

function TRouter<TID>.Excute(AID: TID): Boolean;
var
  LFunc: TFunc<Boolean>;
begin
  Result := False;

  if FExcuteList.TryGetValue(AID, LFunc) then
    if Assigned(LFunc) then
      Result := LFunc();
end;

function TRouter<TID>.Excute<T>(AID: TID; const Data: T): Boolean;
begin
  FValue := TValue.From<T>(Data);
  Result := Excute(AID);
end;

procedure TRouter<TID>.RemoveHandler(AID: TID);
begin
  if FExcuteList.ContainsKey(AID) then
    FExcuteList.Remove(AID);
end;

function TRouter<TID>.RouterKeyToStr(AID: TID): String;
begin
  Result := TValue.From<TID>(AID).ToString
end;

procedure TRouter<TID>.OnNotify(Sender: TObject; const Item: TList<TProc>;
  Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    Item.Free;
end;

function TRouter<TID>.Query<T>(AID: TID): T;
var
  LFunc: TFunc<TValue>;
begin
  if FGenericList.TryGetValue(AID, LFunc) then
  begin
    if Assigned(LFunc) then
      Result := LFunc.AsType<T>
  end
  else
    raise ERouterMethodIDNotExists.CreateFmt(FMT_METHOD_ID_NOT_EXISTS, [RouterKeyToStr(AID)]);
end;

function TRouter<TID>.Query<T>(AID: TID; var Value: T): Boolean;
var
  LFunc: TFunc<TValue>;
begin
  Result := False;
  if FGenericList.TryGetValue(AID, LFunc) then
    if Assigned(LFunc) then
    begin
      Value := LFunc.AsType<T>;
      Result := True;
    end;
end;

procedure TRouter<TID>.RemoveHandler(AID: TID; Proc: TProc);
var
  LList: TList<TProc>;
begin
  if FNotifyList.TryGetValue(AID, LList) then
    LList.Remove(Proc);
end;

end.
