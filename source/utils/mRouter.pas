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

  TRouter<TMethodID> = class
  private const
    FMT_METHOD_ID_ALREADY_EXISTS = '[%s]Method ID is already exists';
    FMT_METHOD_ID_NOT_EXISTS = '[%s]Method ID does not exists';
    FMT_METHOD_NOT_ASSIGNED = '[%s]Method does not assigned';
  private
    FValue: TValue;
    FValue2: TValue;
    FList: TDictionary<TMethodID, TList<TProc>>;
    FBoolList: TDictionary<TMethodID, TFunc<Boolean>>;
    FGenericList: TDictionary<TMethodID, TFunc<TValue>>;

    procedure OnNotify(Sender: TObject; const Item: TList<TProc>; Action: TCollectionNotification);
    function RouterKeyToStr(AID: TMethodID): String;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure Notify(AID: TMethodID); overload;
    procedure Notify<T>(AID: TMethodID; const Data: T); overload;
    procedure Notify<T, T2>(AID: TMethodID; const Data: T; Data2: T2); overload;
    procedure Handler(AID: TMethodID; Proc: TProc); overload;
    procedure Handler(enArray: array of TMethodID; Proc: TProc); overload;
    procedure RemoveHandler(AID: TMethodID; Proc: TProc); overload;

    function Excute(AID: TMethodID): Boolean; overload;
    function Excute<T>(AID: TMethodID; const Data: T): Boolean; overload;
    procedure Handler(AID: TMethodID; Func: TFunc<Boolean>); overload;
    procedure RemoveHandler(AID: TMethodID); overload;

    function Data<T>: T; overload;
    function Data2<T>: T; overload;
    procedure Data<T>(AValue: T); overload;
    procedure Data2<T>(AValue: T); overload;

    function Query<TResult>(AID: TMethodID; var Value: TResult): Boolean; overload;
    function Query<TResult>(AID: TMethodID): TResult; overload;
    procedure Feed<TResult>(AID: TMethodID; Func: TFunc<TResult>); overload;
  end;

implementation

uses
  System.TypInfo;

{ TRouter }

procedure TRouter<TMethodID>.Handler(AID: TMethodID; Proc: TProc);
var
  LList: TList<TProc>;
begin
  if not FList.ContainsKey(AID) then
    FList.Add(AID, TList<TProc>.Create);
  LList := FList.Items[AID];
  if LList.IndexOf(Proc) = -1 then
    LList.Add(Proc);
end;

procedure TRouter<TMethodID>.Clear;
begin
  FGenericList.Clear;
  FBoolList.Clear;
  FList.Clear;
end;

constructor TRouter<TMethodID>.Create;
begin
  FList := TDictionary<TMethodID, TList<TProc>>.Create;
  FList.OnValueNotify := OnNotify;

  FBoolList := TDictionary<TMethodID, TFunc<Boolean>>.Create;

  FGenericList := TDictionary<TMethodID, TFunc<TValue>>.Create;
end;

function TRouter<TMethodID>.Data<T>: T;
begin
  Result := FValue.AsType<T>;
end;

function TRouter<TMethodID>.Data2<T>: T;
begin
  Result := FValue2.AsType<T>;
end;

procedure TRouter<TMethodID>.Data2<T>(AValue: T);
begin
  FValue2 := TValue.From<T>(AValue);
end;

procedure TRouter<TMethodID>.Data<T>(AValue: T);
begin
  FValue := TValue.From<T>(AValue);
end;

destructor TRouter<TMethodID>.Destroy;
begin
  FreeAndNil(FGenericList);
  FreeAndNil(FBoolList);
  FreeAndNil(FList);

  inherited;
end;

procedure TRouter<TMethodID>.Handler(AID: TMethodID; Func: TFunc<Boolean>);
begin
  if FBoolList.ContainsKey(AID) then
    raise ERouterMethodIDAlreadyExists.CreateFmt(FMT_METHOD_ID_ALREADY_EXISTS, [RouterKeyToStr(AID)]);

  FBoolList.Add(AID, Func);
end;

procedure TRouter<TMethodID>.Feed<TResult>(AID: TMethodID; Func: TFunc<TResult>);
begin
  if FGenericList.ContainsKey(AID) then
    raise ERouterMethodIDAlreadyExists .CreateFmt(FMT_METHOD_ID_ALREADY_EXISTS, [RouterKeyToStr(AID)]);

  if not Assigned(Func) then
    raise ERouterMethodNotAssigned.CreateFmt(FMT_METHOD_NOT_ASSIGNED, [RouterKeyToStr(AID)]);

  FGenericList.Add(AID,
    function: TValue
    begin
      Result := TValue.From<TResult>(Func);
    end);
end;

procedure TRouter<TMethodID>.Handler(enArray: array of TMethodID; Proc: TProc);
var
  Len: TMethodID;
begin
  for Len in enArray do
    Handler(Len, Proc);
end;

procedure TRouter<TMethodID>.Notify(AID: TMethodID);
var
  LList: TList<TProc>;
  LProc: TProc;
begin
  if FList.TryGetValue(AID, LList) then
    for LProc in LList do
      if Assigned(LProc) then
        LProc();
end;

procedure TRouter<TMethodID>.Notify<T, T2>(AID: TMethodID; const Data: T; Data2: T2);
begin
  FValue := TValue.From<T>(Data);
  FValue2 := TValue.From<T2>(Data2);
  Notify(AID);
end;

procedure TRouter<TMethodID>.Notify<T>(AID: TMethodID; const Data: T);
begin
  FValue := TValue.From<T>(Data);
  Notify(AID);
end;

function TRouter<TMethodID>.Excute(AID: TMethodID): Boolean;
var
  LFunc: TFunc<Boolean>;
begin
  Result := False;

  if FBoolList.TryGetValue(AID, LFunc) then
    if Assigned(LFunc) then
      Result := LFunc();
end;

function TRouter<TMethodID>.Excute<T>(AID: TMethodID; const Data: T): Boolean;
begin
  FValue := TValue.From<T>(Data);
  Result := Excute(AID);
end;

procedure TRouter<TMethodID>.RemoveHandler(AID: TMethodID);
begin
  if FBoolList.ContainsKey(AID) then
    FBoolList.Remove(AID);
end;

function TRouter<TMethodID>.RouterKeyToStr(AID: TMethodID): String;
begin
  Result := TValue.From<TMethodID>(AID).ToString
end;

procedure TRouter<TMethodID>.OnNotify(Sender: TObject; const Item: TList<TProc>;
  Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    Item.Free;
end;

function TRouter<TMethodID>.Query<TResult>(AID: TMethodID): TResult;
var
  LFunc: TFunc<TValue>;
begin
  if FGenericList.TryGetValue(AID, LFunc) then
  begin
    if Assigned(LFunc) then
      Result := LFunc.AsType<TResult>
  end
  else
    raise ERouterMethodIDNotExists.CreateFmt(FMT_METHOD_ID_NOT_EXISTS, [RouterKeyToStr(AID)]);
end;

function TRouter<TMethodID>.Query<TResult>(AID: TMethodID; var Value: TResult): Boolean;
var
  LFunc: TFunc<TValue>;
begin
  Result := False;
  if FGenericList.TryGetValue(AID, LFunc) then
    if Assigned(LFunc) then
    begin
      Value := LFunc.AsType<TResult>;
      Result := True;
    end;
end;

procedure TRouter<TMethodID>.RemoveHandler(AID: TMethodID; Proc: TProc);
var
  LList: TList<TProc>;
begin
  if FList.TryGetValue(AID, LList) then
    LList.Remove(Proc);
end;

end.
