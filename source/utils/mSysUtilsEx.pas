unit mSysUtilsEx;

interface

uses
  System.SysUtils, System.Types;

function BufferToHexStr(const ABuffer: Pointer; const ALength: Integer): String;
function ByteToHex(const AValue: Byte): String; overload;
function ByteToHex(const AValue: TBytes): String; overload;
function BytesToHexStr(const ABuffer: TBytes): String; overload;
function BytesToHexStr(const AValue: array of Byte): String; overload;
function BytesToHexStr(const AValue: TBytes; const StrIdx, ALength: Integer): String; overload;
function BytesToStr(const AValue: TBytes): String; overload;
function BytesToStr(const AValue: TBytes; const ALength: Int64): String; overload;
function BytesToRawBytesString(const AValues: TBytes): RawByteString;
function BytesToLog(const AValue: TArray<TBytes>): String; overload;
function BytesToLog(const AValue: TBytes): String; overload;

function StrToHexStr(const Str: String): String;
procedure HexStrToBytes(const Source: String; var Dest: TBytes);
function HexStrToStr(const Source: String): String;
function HexStrToInt(const Source: String): Integer;

// Numbers Utils
function MinimumCount(const ADividend, ADivisor: Integer): Integer;
function SwapByte(Value: UInt64): UInt64; overload;
function SwapByte(Value: DWord): DWord; overload;
function SwapByte(Value: Word): Word; overload;
function SwapByte(Value: Single): Single; overload;
procedure ReverseBytes(Source, Dest: Pointer; Size: UInt64);

type
  TFloat = class
    class function ArrayLog<T>(const AValue: TArray<T>): String; static; deprecated 'Uses mGeneric.TGneric.ToLog<T>';
  end;

// email validate
function EmailValidate(const Value: String): Boolean; deprecated 'Use mRegularExpressionsHelper.TRegExHelper.EmailValidate';

implementation

uses
  System.TypInfo, System.RegularExpressions, System.Math, System.Rtti, System.Classes
  ;

function BufferToHexStr(const ABuffer: Pointer; const ALength: Integer): String;
var
  LBuffer: TBytes;
begin
  SetLength(LBuffer, ALength);
  Move(ABuffer^, LBuffer[0], ALength);
  Result := BytesToHexStr(LBuffer);
end;

function MinimumCount(const ADividend, ADivisor: Integer): Integer;
var
  LRemainder: Integer;
begin
  LRemainder := ADividend mod ADivisor;
  Result := ADividend div ADivisor;
  if LRemainder > 0 then
    Inc(Result);
end;

function ByteToHex(const AValue: TBytes): String; overload;
begin
  SetLength(Result, Length(AValue) * 2);
  BinToHex(@AValue[0], PChar(Result), Result.Length);
end;

function ByteToHex(const AValue: Byte): String;
const
  CHexDigits: array [0..15] of Char = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'); {do not localize}
begin
  SetLength(Result, 2);

  {$IFDEF WIN32, WIN64}
    Result[1] := CHexDigits[(AValue and $F0) shr 4]; // 1111 0000
    Result[2] := CHexDigits[AValue and $F];          // 0000 1111
  {$ELSE}
    Result[0] := CHexDigits[(AValue and $F0) shr 4]; // 1111 0000
    Result[1] := CHexDigits[AValue and $F];          // 0000 1111
  {$ENDIF}
end;

function BytesToHexStr(const ABuffer: TBytes): String; overload;
var
  LByte: Byte;
  LBuilder: TStringBuilder;
begin
  LBuilder := TStringBuilder.Create(Length(ABuffer));
  try
    for LByte in ABuffer do
      LBuilder.Append(ByteToHex(LByte));
    Result := LBuilder.ToString;
  finally
    FreeAndNil(LBuilder);
  end;
end;

function BytesToHexStr(const AValue: array of Byte): String; overload;
var
  LByte: Byte;
begin
  Result := EmptyStr;
  for LByte in AValue do
    Result := Result + ByteToHex(LByte);//.ToHexString(2);
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
begin
  Result := BytesToStr(AValue, Length(AValue));
end;

function BytesToStr(const AValue: TBytes; const ALength: Int64): String;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := 0 to ALength - 1 do
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
      if i mod 2 <> 0 then
        Continue;

      Dest[LIdxOfDest] := Byte.Parse('$'+Source.Substring(i, 2));
      Inc(LIdxOfDest);
    end;
  end;
end;

function HexStrToStr(const Source: String): String;
var
  LBytes: TBytes;
begin
  HexStrToBytes(Source, LBytes);
  SetString(Result, PChar(@LBytes[0]), Length(LBytes));
end;

function HexStrToInt(const Source: String): Integer;
begin
  Result := StrToIntDef('$'+Source, 0);
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

function SwapByte(Value: UInt64): UInt64; overload;
type
  Bytes = packed array[0..7] of Byte;
begin
  Bytes(Result)[0]:= Bytes(Value)[7];
  Bytes(Result)[1]:= Bytes(Value)[6];
  Bytes(Result)[2]:= Bytes(Value)[5];
  Bytes(Result)[3]:= Bytes(Value)[4];
  Bytes(Result)[4]:= Bytes(Value)[3];
  Bytes(Result)[5]:= Bytes(Value)[2];
  Bytes(Result)[6]:= Bytes(Value)[1];
  Bytes(Result)[7]:= Bytes(Value)[0];
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

function SwapByte(Value: Single): Single; overload;
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

function BytesToRawBytesString(const AValues: TBytes): RawByteString;
var
  LValue: Byte;
begin
  Result := '';
  for LValue in AValues do
    Result := Result + RawByteString(Char(LValue));
end;

function BytesToLog(const AValue: TArray<TBytes>): String; overload;
var
  LItem: TBytes;
  LBuf: TStringList;
begin
  if Length(AValue) = 0 then
    Exit('');

  LBuf := TStringList.Create;
  try
    for LItem in AValue do
      LBuf.Add(Format('[%s]', [BytesToLog(LItem)]));
    Result := LBuf.CommaText.Trim;
  finally
    FreeAndNil(LBuf);
  end;
end;

function BytesToLog(const AValue: TBytes): String; overload;
var
  LItem: Byte;
  LBuf: TStringList;
begin
  if Length(AValue) = 0 then
    Exit('');

  LBuf := TStringList.Create;
  try
    for LItem in AValue do
      LBuf.Add(LItem.ToHexString(2));
    Result := LBuf.CommaText.Trim;
  finally
    FreeAndNil(LBuf);
  end;
end;

procedure ReverseBytes(Source, Dest: Pointer; Size: UInt64);
begin
  Dest := PByte(NativeUInt(Dest) + Size -1);
  while (Size > 0) do
  begin
    PByte(Dest)^ := PByte(Source)^;
    Inc(PByte(Source));
    Dec(PByte(Dest));
    Dec(Size);
  end;
end;

function DoubleArrayToString(const AValues: TArray<Double>): String;
var
  LValue: Double;
begin
  Result := '';
  for LValue in AValues do
    if Result.IsEmpty then
      Result := LValue.ToString
    else
      Result := Result + ',' + LValue.ToString;
end;

function EmailValidate(const Value: String): Boolean;
const
  REG_EXP_EMAIL = '[a-z0-9!#$%&''*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&''*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?';
begin
  Result := TRegEx.IsMatch(Value, REG_EXP_EMAIL);
end;

{ TFloat }

class function TFloat.ArrayLog<T>(const AValue: TArray<T>): String;
var
  LTypInfo: PTypeInfo;
  LItem: T;
  LValue: String;
begin
  Result := '';

  LTypInfo := TypeInfo(T);
  if LTypInfo.Kind <> tkFloat then
    Exit;

  for LItem in AValue do
  begin
    LValue := TValue.From(LItem).AsExtended.ToString;
    if Result.IsEmpty then
      Result := LValue
    else
      Result := Result + ',' + LValue;
  end;
end;

end.
