unit mRegularExpressionsHelper;

interface

uses
  System.RegularExpressions,
  System.Classes, System.SysUtils
  ;

type
  TRegExHelper = record Helper for TRegEx
    class function EmailValidate(const AValue: String): Boolean; static;
    class function IsNumber(const AValue: String): Boolean; overload; static;
    class function IsNumber(const AValue: String; const ADecimalSeparator: Char): Boolean; overload; static;
    class function IsFloat(const AValue: String; const APrecision: Integer = -1): Boolean; static;
  end;

implementation

uses
  System.StrUtils
  ;

{ TRegExHelper }

class function TRegExHelper.EmailValidate(const AValue: String): Boolean;
const
  SPattern = '[a-z0-9!#$%&''*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&''*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?';
begin
  Result := TRegEx.IsMatch(AValue, SPattern);
end;

class function TRegExHelper.IsNumber(const AValue: String): Boolean;
const
  SPattern = '^(?=.)([+-]?([0-9]*)(\.([0-9]+))?)$';
begin
  Result := TRegEx.IsMatch(AValue, SPattern);

end;

class function TRegExHelper.IsFloat(const AValue: String; const APrecision: Integer): Boolean;
const
  SPattern: array[False .. True] of String = ('\d*\.\d', '\d+|\d*\.\d{%d}');
begin
  Result := TRegEx.IsMatch(AValue, Format(SPattern[APrecision > 0], [APrecision]));
end;

class function TRegExHelper.IsNumber(const AValue: String; const ADecimalSeparator: Char): Boolean;
const
  SPattern = '^(?=.)([+-]?([0-9]*)(\.([0-9]+))?)$';
var
  LPattern: String;
begin
  if ADecimalSeparator = '.' then
    Exit(IsNumber(AValue));

  LPattern := SPattern.Replace('.', ADecimalSeparator, [rfReplaceAll]);
  Result := TRegEx.IsMatch(AValue, LPattern);
end;

end.
