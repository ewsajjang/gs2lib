unit mSysUtilsEx;

interface

uses
  System.SysUtils, System.Types;

function BytesToHexStr(const AValue: TBytes): String; overload;
function BytesToHexStr(const AValue: TBytes; const StrIdx, ALength: Integer): String; overload;
function AnsiBytesToStr(const AValue: TBytes; const StrIdx, ALength: Integer): String;

function StrToHexStr(const Str: String): String;
procedure HexStrToBytes(const Source: String; var Dest: TBytes);
function HexStrToStr(const Source: String): String;

// Numbers Utils
function MinimumCount(const ADividend, ADivisor: Integer): Integer;
function SwapByte(Value: DWord): DWord; overload;
function SwapByte(Value: Word): Word; overload;

implementation

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

procedure HexStrToBytes(const Source: String; var Dest: TBytes);
var
  i, LIdxOfDest, LBytesLen: Integer;
begin
  LBytesLen := Length(Source) div 2;
  SetLength(Dest, LBytesLen);

  LIdxOfDest := 0;
  for i := 0 to Length(Source) - 1 do
  begin
    if i mod 2 = 0 then
      Continue;

    Dest[LIdxOfDest] := Byte(Char(StrToInt('$' + Source[i] + Source[i+1])));
    Inc(LIdxOfDest);
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

function AnsiBytesToStr(const AValue: TBytes; const StrIdx, ALength: Integer): String;
begin
  SetString(Result, PAnsiChar(@AValue[StrIdx]), ALength);
end;

end.
