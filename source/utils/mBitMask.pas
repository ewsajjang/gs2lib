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
    function GetB(ABitIdx: Integer): Boolean;
    function GetStr: String;
    function GetBitCnt: Integer;
    function GetI(ABitIdx: Integer): Integer;
  public
    procedure Load(const AValue: T);

    property B[ABitIdx: Integer]: Boolean read GetB; default;
    property I[ABitIdx: Integer]: Integer read GetI;
    property Size: Integer read FSize;
    property BitCnt: Integer read GetBitCnt;
    property Str: String read GetStr;
  end;

  TCustomBitsHandler = class
  protected const
    BIT_CNT_OF_BYTE = 8;
  protected
    FBuffer: TBytes;
    FBufferLength: Byte;
    procedure SetBuffer(const Value: TBytes);
    function GetAssigned: Boolean;
    procedure DoAfterAssignBuffer; virtual;
  public
    procedure Empty;
    property Assigned: Boolean read GetAssigned;
    property Buffers: TBytes read FBuffer write SetBuffer;
    property BufferLength: Byte read FBufferLength;
  end;

  TBitFlagHandler = class(TCustomBitsHandler)
  private
    function GetAsIndexes: TArray<Byte>;
    function GetAsBitBools(ABitIdx: Byte): Boolean;
    procedure SetAsBitBools(ABitIdx: Byte; const Value: Boolean);
    function GetByte: Byte;
    function GetWord: Word;
  protected
    function GetAsArray: TArray<Cardinal>;
  public
    property AsByte: Byte read GetByte;
    property AsWord: Word read GetWord;
    property AsArray: TArray<Cardinal> read GetAsArray;
    property AsIndxes: TArray<Byte> read GetAsIndexes;
    property AsBitBools[ABitIdx: Byte]: Boolean read GetAsBitBools write SetAsBitBools;
  end;

  TBitValueHandler = class(TCustomBitsHandler)
  private
    function GetNibble(const Index: Integer): Byte;
  protected
    function GetByte(const Index: Integer): Byte;
    function GetInt8(const Index: Integer): ShortInt;
    function GetWord(const Index: Integer): Word;
    function GetInt16(const Index: Integer): SmallInt;
    function GetCardinal(const Index: Integer): Cardinal;
    function GetInteger(const Index: Integer): Integer;
  public
    function AsSingle(const AByteOffSet: Byte = 0): Single; overload;
    function AsDouble(const AByteOffSet: Byte = 0): Double; overload;
    function AsDateTime(const AByteOffSet: Byte = 0): TDateTime;
    // !! The following index value is combination of bit-offset(first 2 digit) and bit-length(last 2 digit).
    property AsNibbleStx:Byte     index $0004 read GetNibble;
    property AsNibbleEtx:Byte     index $0404 read GetNibble;
    property AsByte     :Byte     index $0008 read GetByte;
    property AsInt8     :ShortInt index $0008 read GetInt8;
    property AsWord     :Word     index $0010 read GetWord;
    property AsInt16    :SmallInt index $0010 read GetInt16;
    property AsCardinal :Cardinal index $0020 read GetCardinal;
    property AsInteger  :Integer  index $0020 read GetInteger;
  end;

  TBit = class
  private
    class var FEnum: TBitValueHandler;
    class var FFlag: TBitFlagHandler;
    class procedure InitInstance(const AInstance: TCustomBitsHandler; const ABuffer: TBytes);
  public
    class function Value(const ABuffer: TBytes): TBitValueHandler; overload;
    class function Value(const ABuffer: TBytes; const AIndex, ACount: Integer): TBitValueHandler; overload;
    class function Flag(const AValue: Byte): TBitFlagHandler; overload;
    class function Flag(const ABuffer: TBytes): TBitFlagHandler; overload;
    class function Flag(const ABuffer: TBytes; const AIndex, ACount: Integer): TBitFlagHandler; overload;
  end;

implementation

uses
  System.Rtti, System.TypInfo, System.Math;

{ TBitFlag<T> }

function TBitMask<T>.GetB(ABitIdx: Integer): Boolean;
var
  LMask: UInt64;
begin
  LMask := 1 shl ABitIdx;
  Result := (TValue.From<T>(FValue).AsType<UInt64> and LMask) = LMask;
end;

function TBitMask<T>.GetBitCnt: Integer;
begin
  Result := Size * 8;
end;

function TBitMask<T>.GetI(ABitIdx: Integer): Integer;
begin
  Result := BoolInts[B[ABitIdx]]
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

{ TCustomBitsHandler }

procedure TCustomBitsHandler.DoAfterAssignBuffer;
begin
end;

procedure TCustomBitsHandler.Empty;
begin
  FBuffer := nil;
end;

function TCustomBitsHandler.GetAssigned: Boolean;
begin
  Result := FBufferLength > 0;
end;

procedure TCustomBitsHandler.SetBuffer(const Value: TBytes);
begin
  FBufferLength := Length(Value);
  SetLength(FBuffer, FBufferLength);
  FBuffer := Copy(Value);

  DoAfterAssignBuffer;
end;

{ TBitFlagHandler }

function TBitFlagHandler.GetAsArray: TArray<Cardinal>;
const
  MASK = $1;
var
  i: Integer;
  j: Integer;
  LIdx: Integer;
begin
  Result := [];
  LIdx := 0;
  for i := 0 to FBufferLength -1 do
  begin
    for j := 0 to BIT_CNT_OF_BYTE -1 do
    begin
      if ((FBuffer[i] shr j) and MASK) = MASK then
        Result := Result + [1 shl (LIdx -i)];
      Inc(LIdx);
    end;
    Inc(LIdx);
  end;
end;

function TBitFlagHandler.GetAsBitBools(ABitIdx: Byte): Boolean;
begin
  Result := ((FBuffer[0] shr ABitIdx) and $1 ) = 1;
end;

function TBitFlagHandler.GetAsIndexes: TArray<Byte>;
const
  MASK = $1;
var
  i: Integer;
  j: Integer;
  LIdx: Integer;
begin
  Result := [];
  LIdx := 0;
  for i := 0 to FBufferLength -1 do
  begin
    for j := 0 to BIT_CNT_OF_BYTE -1 do
    begin
      if ((FBuffer[i] shr j) and MASK) = MASK then
        Result := Result + [LIdx -i];
      Inc(LIdx);
    end;
    Inc(LIdx);
  end;
end;

function TBitFlagHandler.GetByte: Byte;
begin
  Result := FBuffer[0];
end;

function TBitFlagHandler.GetWord: Word;
begin
  Result := PWord(@FBuffer[0])^
end;

procedure TBitFlagHandler.SetAsBitBools(ABitIdx: Byte; const Value: Boolean);
begin
  if Value and ((FBuffer[0] shr ABitIdx) = 0) then
      FBuffer[0] := FBuffer[0] + (1 shl ABitIdx)
  else if (FBuffer[0] shr ABitIdx) = 1 then
    FBuffer[0] := FBuffer[0] - (1 shl ABitIdx)
end;

{ TBitEnumHandler }

function TBitValueHandler.AsDateTime(const AByteOffSet: Byte): TDateTime;
begin
  Result := AsDouble(AByteOffSet);
end;

function TBitValueHandler.AsSingle(const AByteOffSet: Byte): Single;
begin
  Result := PSingle(@FBuffer[AByteOffSet])^;
end;

function TBitValueHandler.AsDouble(const AByteOffSet: Byte): Double;
begin
  Result := PDouble(@FBuffer[AByteOffSet])^;
end;

function TBitValueHandler.GetByte(const Index: Integer): Byte;
begin
  if not Assigned then
    Exit(0);
  Result := (FBuffer[0] shr (Index shr BIT_CNT_OF_BYTE)) // Assign OffSet
            and ((1 shl Byte(Index)) -1); // Assign Mask
end;

function TBitValueHandler.GetCardinal(const Index: Integer): Cardinal;
begin
  if not Assigned then
    Exit(0);
  Result := (PCardinal(@FBuffer[0])^ shr (Index shr BIT_CNT_OF_BYTE)) // Assign OffSet
            and ((UInt64(1) shl Cardinal(Index)) -1); // Assign Mask
end;

function TBitValueHandler.GetInteger(const Index: Integer): Integer;
begin
  if not Assigned then
    Exit(0);
  Result := (PInteger(@FBuffer[0])^ shr (Index shr BIT_CNT_OF_BYTE)) // Assign OffSet
            and ((Int64(1) shl Byte(Index)) -1); // Assign Mask
end;

function TBitValueHandler.GetNibble(const Index: Integer): Byte;
begin
  if not Assigned then
    Exit(0);
  Result := (FBuffer[0] shr (Index shr BIT_CNT_OF_BYTE)) // Assign OffSet
            and ((1 shl Byte(Index)) -1); // Assign Mask
end;

function TBitValueHandler.GetInt8(const Index: Integer): ShortInt;
begin
  if not Assigned then
    Exit(0);
  Result := (PShortInt(@FBuffer[0])^ shr (Index shr BIT_CNT_OF_BYTE)) // Assign OffSet
            and ((1 shl Byte(Index)) -1); // Assign Mask
end;

function TBitValueHandler.GetInt16(const Index: Integer): SmallInt;
begin
  if not Assigned then
    Exit(0);
  Result := (PSmallInt(@FBuffer[0])^ shr (Index shr BIT_CNT_OF_BYTE)) // Assign OffSet
            and ((1 shl Byte(Index)) -1); // Assign Mask
end;

function TBitValueHandler.GetWord(const Index: Integer): Word;
begin
  if not Assigned then
    Exit(0);
  Result := (PWord(@FBuffer[0])^ shr (Index shr BIT_CNT_OF_BYTE)) // Assign OffSet
            and ((1 shl Byte(Index)) -1); // Assign Mask
end;

{ TBit }

class function TBit.Value(const ABuffer: TBytes): TBitValueHandler;
begin
  if not Assigned(FEnum) then
    FEnum := TBitValueHandler.Create;

  InitInstance(FEnum, ABuffer);
  Result := FEnum;
end;

class function TBit.Flag(const ABuffer: TBytes): TBitFlagHandler;
begin
  if not Assigned(FFlag) then
    FFlag := TBitFlagHandler.Create;

  InitInstance(FFlag, ABuffer);
  Result := FFlag;
end;

class function TBit.Flag(const ABuffer: TBytes; const AIndex,
  ACount: Integer): TBitFlagHandler;
var
  LBuffer: TBytes;
begin
  SetLength(LBuffer, ACount);
  LBuffer := Copy(ABuffer, AIndex, ACount);
  Result := Flag(LBuffer);
end;

class function TBit.Flag(const AValue: Byte): TBitFlagHandler;
var
  LBuffer: TBytes;
begin
  LBuffer := [AValue];
  Result := Flag(LBuffer);
end;

class procedure TBit.InitInstance(const AInstance: TCustomBitsHandler; const ABuffer: TBytes);
begin
  AInstance.Empty;
  AInstance.Buffers := ABuffer;
end;

class function TBit.Value(const ABuffer: TBytes; const AIndex,
  ACount: Integer): TBitValueHandler;
var
  LBuffer: TBytes;
begin
  SetLength(LBuffer, ACount);
  LBuffer := Copy(ABuffer, AIndex, ACount);
  Result := Value(LBuffer);
end;

initialization

finalization
  if Assigned(TBit.FEnum) then
    TBit.FEnum.Free;
  if Assigned(TBit.FFlag) then
    TBit.FFlag.Free;

end.
