unit mStrUtils;

interface

function EmailValidate(const Value: String): Boolean;
function FloatValidate(const AValue: String): Boolean;
function IntegerValidate(const AValue: String): Boolean;
function NonEmptyStr(const AValue1, AValue2: String): String;
function IsNumeric(const AValue: String): Boolean;
function IsInteger(const AValue: String): Boolean;
function ValidateIPv4(const AValue: string): Boolean;

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
  Result := TRegEx.IsMatch(AValue, '[0-9]*\'+FormatSettings.DecimalSeparator+'?[0-9]+$')
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

function IsNumeric(const AValue: String): Boolean;
var
  LValue: Extended;
begin
  Result := TryStrToFloat(AValue, LValue);
end;

function IsInteger(const AValue: String): Boolean;
var
  LValue: Int64;
begin
  Result := TryStrToInt64(AValue, LValue);
end;

function ValidateIPv4(const AValue: string): Boolean;
const
  SRegExp = '^(?=\d+\.\d+\.\d+\.\d+($|\/))(([1-9]?\d|1\d\d|2[0-4]\d|25[0-5])\.?){4}(([0-9]|[1-2][0-9]|3[0-2]))?$';
begin
  Result := TRegEx.IsMatch(AValue, SRegExp);
end;


end.
