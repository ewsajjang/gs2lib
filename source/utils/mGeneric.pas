unit mGeneric;

interface

uses
  System.Classes, System.SysUtils, System.Rtti
  ;

type
  TGeneric = class
    class function CreateInstance<T>(const AArgs: TArray<TValue>): T;
  end;

implementation

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

end.
