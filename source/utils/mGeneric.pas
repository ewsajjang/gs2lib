unit mGeneric;

interface

uses
  System.Classes, System.SysUtils, System.Rtti
  ;

type
  TGeneric = class
  private
    class function ValueToLog(const AValue: TValue): String;
  public
    class function CreateInstance<T>(const AArgs: TArray<TValue>): T;
    class function ToString<T>: String; reintroduce;
    class function IfThen<T>(AValue: Boolean; const ATrue: T): T; overload;
    class function IfThen<T>(AValue: Boolean; const ATrue, AFalse: T): T; overload;
    class function HasIdxProperty(const ASrc: TObject; const APropName: String): Boolean;
    class function TryGetIdxProperty<T>(const ASrc: TObject; const APropName: String;
      const Args: array of TValue; var AValue: T): Boolean;
    class function TrySetIdxProperty<T>(const ASrc: TObject; const APropName: String;
      const Args: array of TValue; const AValue: T): Boolean;
    class function ToLog<T>(const AValue: TArray<T>): String; overload; static;
    class function ToLog<T>(const AValue: array of T): String; overload; static;
    class function ToLog<T>(const AValue: T): String; overload; static;
  end;

implementation

uses
  System.TypInfo
  ;

{ TGeneric }

class function TGeneric.CreateInstance<T>(const AArgs: TArray<TValue>): T;
var
  LValue: TValue;
  LCtx: TRttiContext;
  LType: TRttiType;
  LMethod: TRttiMethod;
  LInstanceType: TRttiInstanceType;
begin
  LCtx := TRttiContext.Create;
  LType := LCtx.GetType(TypeInfo(T));
  for LMethod in LType.GetMethods do
    if LMethod.IsConstructor and (Length(LMethod.GetParameters) = Length(AArgs)) then
    begin
      LInstanceType := LType.AsInstance;
      LValue := LMethod.Invoke(LInstanceType.MetaclassType, AArgs);
      Exit(LValue.AsType<T>);
    end;
  Exit(Default(T));
end;

class function TGeneric.IfThen<T>(AValue: Boolean; const ATrue: T): T;
begin
  Result := IfThen<T>(AValue, ATrue, Default(T));
end;

class function TGeneric.HasIdxProperty(const ASrc: TObject;
  const APropName: String): Boolean;
var
  LCtx: TRttiContext;
  LRttiType: TRttiType;
begin
  LCtx := TRttiContext.Create;
  try
    LRttiType := LCtx.GetType(ASrc.ClassInfo) as TRttiInstanceType;
    Result := Assigned(LRttiType.GetIndexedProperty(APropName));
  except on E: Exception do
    Exit(False);
  end;
end;

class function TGeneric.IfThen<T>(AValue: Boolean; const ATrue, AFalse: T): T;
begin
  if AValue then
    Exit(ATrue)
  else
    Exit(AFalse);
end;

class function TGeneric.ToLog<T>(const AValue: TArray<T>): String;
var
  LItem: T;
  LBuf: TStringList;
  LValue: TValue;
begin
  LBuf := TStringList.Create;
  try
    for LItem in AValue do
    begin
      LValue := TValue.From<T>(LItem);
      LBuf.Add(ValueToLog(LValue) + LValue.ToString);
    end;
    Result := LBuf.CommaText;
  finally
    FreeAndNil(LBuf);
  end;
end;

class function TGeneric.ToLog<T>(const AValue: T): String;
var
  LValue: TValue;
begin
  LValue := TValue.From<T>(AValue);
  Result := Format('%s', [ValueToLog(LValue) + LValue.ToString]);
end;

class function TGeneric.ToLog<T>(const AValue: array of T): String;
var
  LItem: T;
  LBuf: TStringList;
  LValue: TValue;
begin
  LBuf := TStringList.Create;
  try
    for LItem in AValue do
    begin
      LValue := TValue.From<T>(LItem);
      LBuf.Add(Format('%s', [ValueToLog(LValue) + LValue.ToString]));
    end;
    Result := LBuf.CommaText;
  finally
    FreeAndNil(LBuf);
  end;
end;

class function TGeneric.ToString<T>: String;
var
  LTypeInfo: PTypeInfo;
  LContext: TRttiContext;
  LType: TRttiType;
begin
  LTypeInfo := TypeInfo(T);
  LType := LContext.GetType(LTypeInfo);
  Result := LType.Name;
end;

class function TGeneric.TryGetIdxProperty<T>(const ASrc: TObject;
  const APropName: String; const Args: array of TValue; var AValue: T): Boolean;
var
  LCtx: TRttiContext;
  LRttiType: TRttiType;
  LRttiProp: TRttiIndexedProperty;
  LValue: TValue;
begin
  Result := False;
  LCtx := TRttiContext.Create;
  try
    LRttiType := LCtx.GetType(ASrc.ClassInfo) as TRttiInstanceType;
    LRttiProp := LRttiType.GetIndexedProperty(APropName);
    if Assigned(LRttiProp) then
    begin
      LValue := LRttiProp.GetValue(ASrc, Args);
      Result := not LValue.IsEmpty;
      AValue := LValue.AsType<T>;
    end;
  except on E: Exception do
  end;
  Result := True;
end;

class function TGeneric.TrySetIdxProperty<T>(const ASrc: TObject;
  const APropName: String; const Args: array of TValue;
  const AValue: T): Boolean;
var
  LCtx: TRttiContext;
  LRttiType: TRttiType;
  LRttiProp: TRttiIndexedProperty;
  LValue: TValue;
begin
  Result := False;
  LCtx := TRttiContext.Create;
  try
    LRttiType := LCtx.GetType(ASrc.ClassInfo) as TRttiInstanceType;
    LValue := TValue.From(AValue);
    LRttiProp := LRttiType.GetIndexedProperty(APropName);
    if Assigned(LRttiProp) then
    begin
      LRttiProp.SetValue(ASrc, Args, LValue);
      Result := True;
    end;
  except on E: Exception do
  end;
  Result := True;
end;

class function TGeneric.ValueToLog(const AValue: TValue): String;
begin
  Result := '';
  case AValue.Kind of
    tkInteger: Result := AValue.AsInteger.ToHexString(AValue.DataSize * 2);
    tkChar: Result := AValue.AsInteger.ToHexString(AValue.DataSize * 2);
    tkEnumeration: ;

    tkFloat: ;

    tkString ,
    tkLString,
    tkWString,
    tkUString: ;

    tkSet: ;
    tkClass: ;
    tkMethod: ;
    tkWChar: ;
    tkVariant: ;
    tkArray: ;
    tkRecord: ;
    tkInterface: ;
    tkInt64:
      if AValue.TypeInfo = System.TypeInfo(Int64) then
        Result := AValue.AsInt64.ToHexString(AValue.DataSize * 2)
      else if AValue.TypeInfo = System.TypeInfo(UInt64) then
        Result := AValue.AsUInt64.ToHexString(AValue.DataSize * 2);
    tkDynArray: ;
    tkClassRef: ;
    tkPointer: Result := Format('%p', [AValue.AsType<Pointer>]);
    tkProcedure: ;
  end;
  if not Result.IsEmpty then
    Result := '[$' + Result + ']';
end;

end.
