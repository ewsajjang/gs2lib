unit mBitMask;

interface

uses
  System.SysUtils, System.Classes,
  System.Generics.Collections;

type
  EBitFlag = class(Exception);
    ENotSupportedType = class(EBitFlag);

  TBitMask<T> = record
  private const
    BoolStrs: array[False..True] of Char = ('0', '1');
    BoolInts: array[False..True] of Integer = (0, 1);
  private
    FSize: Integer;
    FValue: T;
    function GetB(Index: Integer): Boolean;
    function GetStr: String;
    function GetBitCnt: Integer;
    function GetI(Index: Integer): Integer;
  public
    procedure Load(const AValue: T);

    property B[Index: Integer]: Boolean read GetB; default;
    property I[Index: Integer]: Integer read GetI;
    property Size: Integer read FSize;
    property BitCnt: Integer read GetBitCnt;
    property Str: String read GetStr;
  end;

implementation

uses
  System.Rtti, System.TypInfo;

{ TBitFlag<T> }

function TBitMask<T>.GetB(Index: Integer): Boolean;
var
  LMask: UInt64;
begin
  LMask := 1 shl Index;
  Result := (TValue.From<T>(FValue).AsType<UInt64> and LMask) = LMask;
end;

function TBitMask<T>.GetBitCnt: Integer;
begin
  Result := Size * 8;
end;

function TBitMask<T>.GetI(Index: Integer): Integer;
begin
  Result := BoolInts[B[Index]]
end;

function TBitMask<T>.GetStr: String;
var
  i: Integer;
begin
  SetLength(Result, BitCnt);
  for i := 1 to BitCnt do
    Result[i] := BoolStrs[B[BitCnt - i]];
end;

procedure TBitMask<T>.Load(const AValue: T);
var
  LValue: TValue;
  LSupported: Boolean;
  LIsClass: Boolean;
begin
  LValue := TValue.From<T>(AValue);
  LIsClass := LValue.IsClass;
  if not LIsClass then
    LSupported :=
      LValue.IsType<Integer > or
      LValue.IsType<Cardinal> or
      LValue.IsType<ShortInt> or
      LValue.IsType<SmallInt> or
      LValue.IsType<LongInt > or
      LValue.IsType<Int64   > or
      LValue.IsType<Byte    > or
      LValue.IsType<Word    > or
      LValue.IsType<LongWord> or
      LValue.IsType<UInt64  > ;
  if not LSupported or LIsClass then
    raise ENotSupportedType.CreateFmt('TBitFlag can not created by %s', [LValue.TypeInfo.Name]);

  FValue := AValue;
  FSize := SizeOf(FValue);
end;

end.
