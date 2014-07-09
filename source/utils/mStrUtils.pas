unit mStrUtils;

interface

function EmailValidate(const Value: String): Boolean;
function FloatValidate(const AValue: String): Boolean;
function IntegerValidate(const AValue: String): Boolean;
function NonEmptyStr(const AValue1, AValue2: String): String;

implementation

uses
  System.SysUtils, System.TypInfo, System.RegularExpressions
  ;

function EmailValidate(const Value: String): Boolean;
const
  REG_EXP_EMAIL = '[a-z0-9!#$%&''*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&''*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?';
begin
  Result := TRegEx.IsMatch(Value, REG_EXP_EMAIL);
end;

function FloatValidate(const AValue: String): Boolean;
begin
  Result := TRegEx.IsMatch(AValue, '[0-9]*\.?[0-9]+$')
end;

function IntegerValidate(const AValue: String): Boolean;
begin
  Result := TRegEx.IsMatch(AValue, '^\d+$')
end;

function NonEmptyStr(const AValue1, AValue2: String): String;
begin
  if AValue1.IsEmpty then
    Result := AValue2
  else
    Result := AValue1
end;

end.
