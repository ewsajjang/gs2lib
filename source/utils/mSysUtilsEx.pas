unit mSysUtilsEx;

interface

uses
  System.SysUtils, System.Types;

function BytesToHexStr(const AValue: TBytes): String; overload;
function BytesToHexStr(const AValue: TBytes; const StrIdx, ALength: Integer): String; overload;
function BytesToStr(const AValue: TBytes): String;
function AnsiBytesToStr(const AValue: TBytes): String; overload;
function AnsiBytesToStr(const AValue: TBytes; const StrIdx, ALength: Integer): String; overload;

function StrToHexStr(const Str: String): String;
procedure HexStrToBytes(const Source: String; var Dest: TBytes);
function HexStrToStr(const Source: String): String;

// Numbers Utils
function MinimumCount(const ADividend, ADivisor: Integer): Integer;
function SwapByte(Value: DWord): DWord; overload;
function SwapByte(Value: Word): Word; overload;

// email validate
function EmailValidate(const Value: String): Boolean;

implementation

uses
  System.TypInfo, System.RegularExpressions, System.Math
  ;

function MinimumCount(const ADividend, ADivisor: Integer): Integer;
var
  LRemainder: Integer;
begin
  LRemainder := ADividend mod ADivisor;
  Result := ADividend div ADivisor;
  if LRemainder > 0 then
    Inc(Result);
end;

function BytesToHexStr(const AValue: TBytes): String; overload;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := 0 to Length(AValue) - 1 do
    Result := Result + Format('%.2x', [AValue[i]]);
end;

function BytesToHexStr(const AValue: TBytes; const StrIdx, ALength: Integer): String; overload;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := StrIdx to StrIdx + ALength - 1 do
    Result := Result + Format('%.2x', [AValue[i]]);
end;

function BytesToStr(const AValue: TBytes): String;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := 0 to Length(AValue) - 1 do
    if InRange(AValue[i], $21, $7E) then
      Result := Result + Chr(AValue[i])
    else
      Result := Result + '.'
end;

procedure HexStrToBytes(const Source: String; var Dest: TBytes);
const
  SOURCE_LEN_MIN = 2;
var
  i, LIdxOfDest, LBytesLen: Integer;
begin
  if Source.Length < SOURCE_LEN_MIN then
    SetLength(Dest, 0)
  else
  begin
    LBytesLen := Source.Length div 2;
    SetLength(Dest, LBytesLen);

    LIdxOfDest := 0;
    for i := 0 to Source.Length - 1 do
    begin
      if i mod 2 = 0 then
        Continue;

      Dest[LIdxOfDest] := Byte(Char(StrToInt('$' + Source[i] + Source[i+1])));
      Inc(LIdxOfDest);
    end;
  end;
end;

function HexStrToStr(const Source: String): String;
var
  LBytes: TBytes;
begin
  HexStrToBytes(Source, LBytes);
  SetString(Result, PAnsiChar(@LBytes[0]), Length(LBytes));
end;

function StrToHexStr(const Str: String): String;
var
  i: Integer;
  AStr: String;
begin
  Result := EmptyStr;
  AStr := Str;
  for i := 1 to System.Length(AStr) do
    Result := Result + Format('%.2x', [Byte(AStr[i])]);
end;

function SwapByte(Value: DWord): DWord;
type
  Bytes = packed array[0..3] of Byte;
begin
  Bytes(Result)[0]:= Bytes(Value)[3];
  Bytes(Result)[1]:= Bytes(Value)[2];
  Bytes(Result)[2]:= Bytes(Value)[1];
  Bytes(Result)[3]:= Bytes(Value)[0];
end;

function SwapByte(Value: Word): Word;
type
  Bytes = packed array[0..1] of Byte;
begin
  Bytes(Result)[0]:= Bytes(Value)[1];
  Bytes(Result)[1]:= Bytes(Value)[0];
end;

function AnsiBytesToStr(const AValue: TBytes): String; overload;
begin
  SetString(Result, PAnsiChar(@AValue[0]), Length(AValue));
end;

function AnsiBytesToStr(const AValue: TBytes; const StrIdx, ALength: Integer): String;
begin
  SetString(Result, PAnsiChar(@AValue[StrIdx]), ALength);
end;

function EmailValidate(const Value: String): Boolean;
const
  REG_EXP_EMAIL = '[a-z0-9!#$%&''*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&''*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?';
begin
  Result := TRegEx.IsMatch(Value, REG_EXP_EMAIL);
end;

end.
