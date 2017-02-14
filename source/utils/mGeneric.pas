unit mGeneric;

interface

uses
  System.Classes, System.SysUtils, System.Rtti
  ;

type
  TGeneric = class
    class function CreateInstance<T>(const AArgs: TArray<TValue>): T;
    class function ToString<T>: String; reintroduce;
    class function IfThen<T>(AValue: Boolean; const ATrue: T): T; overload;
    class function IfThen<T>(AValue: Boolean; const ATrue, AFalse: T): T; overload;
    class function HasIdxProperty(const ASrc: TObject; const APropName: String): Boolean;
    class function TryGetIdxProperty<T>(const ASrc: TObject; const APropName: String;
      const Args: array of TValue; var AValue: T): Boolean;
    class function TrySetIdxProperty<T>(const ASrc: TObject; const APropName: String;
      const Args: array of TValue; const AValue: T): Boolean;

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

end.
