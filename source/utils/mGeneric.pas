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

end.
