unit mRecordUtils;

interface

uses
  System.SysUtils, System.Classes
  ;

type
  TRecordIndexer = class
  private
    function GetAssigned: Boolean;
    function GetBufferLength: Int64;
  protected
    const BIT_CNT = 8;
  protected
    FBuffer: TBytes;
    procedure DoAfterAssignBuffer; virtual;
    function DoCalculatesOffset(const Index: Integer): Integer; virtual;
  protected
    function GetBuffer(const Index: Integer): TBytes;
    function GetDateTime(const Index: Integer): TDateTime;
    function GetFloat32(const Index: Integer): Single;
    function GetFloat16(const Index: Integer): Single;
    function GetInt8(const Index: Integer): Int8;
    function GetInt16(const Index: Integer): Int16;
    function GetInteger(const Index: Integer): Integer;
    function GetInt64(const Index: Integer): Int64;
    function GetByte(const Index: Integer): Byte;
    function GetWord(const Index: Integer): Word;
    function GetCardinal(const Index: Integer): Cardinal;
    function GetUInt64(const Index: Integer): UInt64;
    function GetNibbleTail(const Index: Integer): Byte;
    function GetNibbleHead(const Index: Integer): Byte;
  public
    procedure Empty;
    procedure Assign(const ABuffer: TBytes; const ABufferLen: Int64; AReverse: Boolean = False); overload;
    procedure Assign(const ABuffer: TBytes; AReverse: Boolean = False); overload;

    property Assigned: Boolean read GetAssigned;
    property Buffer: TBytes read FBuffer;
    property BufferLength: Int64 read GetBufferLength;
  end;

implementation

uses
  mSysUtilsEx, System.Math, System.DateUtils
  ;

{ TRecordIndexer }

procedure TRecordIndexer.Assign(const ABuffer: TBytes;
  const ABufferLen: Int64; AReverse: Boolean);
begin

  if AReverse then
  begin
    SetLength(FBuffer, ABufferLen);
    ReverseBytes(ABuffer, FBuffer, ABufferLen);
  end
  else
    FBuffer := Copy(ABuffer);
  DoAfterAssignBuffer;
end;

procedure TRecordIndexer.Assign(const ABuffer: TBytes; AReverse: Boolean);
begin
  Assign(ABuffer, Length(ABuffer), AReverse);
end;

procedure TRecordIndexer.DoAfterAssignBuffer;
begin
end;

function TRecordIndexer.DoCalculatesOffset(const Index: Integer): Integer;
begin
  Result := 0;
end;

procedure TRecordIndexer.Empty;
begin
  FBuffer := nil;
end;

function TRecordIndexer.GetAssigned: Boolean;
begin
  Result := BufferLength > 0
end;

function TRecordIndexer.GetBuffer(const Index: Integer): TBytes;
begin
  Result := Copy(FBuffer, Index, Length(FBuffer) - Index);
end;

function TRecordIndexer.GetBufferLength: Int64;
begin
  if System.Assigned(FBuffer) then
    Result := Length(FBuffer)
  else
    Result := 0;
end;

function TRecordIndexer.GetByte(const Index: Integer): Byte;
var
  LIdx: Integer;
begin
  LIdx := Index + DoCalculatesOffset(Index);
  if LIdx < BufferLength then
    Result := FBuffer[Index + DoCalculatesOffset(Index)]
  else
    Result := 0;
end;

function TRecordIndexer.GetCardinal(const Index: Integer): Cardinal;
var
  LIdx: Integer;
begin
  LIdx := Index + DoCalculatesOffset(Index);
  if LIdx < BufferLength then
    Result := PCardinal(@FBuffer[Index + DoCalculatesOffset(Index)])^
  else
    Result := 0
end;

function TRecordIndexer.GetDateTime(const Index: Integer): TDateTime;
var
  LIdx: Integer;
begin
  LIdx := Index + DoCalculatesOffset(Index);
  if LIdx < BufferLength then
    Result := PDateTime(@FBuffer[Index + DoCalculatesOffset(Index)])^
  else
    Result := MinDateTime
end;

function TRecordIndexer.GetFloat16(const Index: Integer): Single;
var
  LBuffer: TBytes;
  LIdx: Integer;
begin
  LIdx := Index + DoCalculatesOffset(Index);
  if LIdx < BufferLength then
  begin
    LBuffer := Copy(FBuffer, Index + DoCalculatesOffset(Index), 2);
    Result := PSingle(@LBuffer[0])^;
  end
  else
    Result := 0
end;

function TRecordIndexer.GetFloat32(const Index: Integer): Single;
var
  LIdx: Integer;
begin
  LIdx := Index + DoCalculatesOffset(Index);
  if LIdx < BufferLength then
    Result := PSingle(@FBuffer[Index + DoCalculatesOffset(Index)])^
  else
    Result := 0
end;

function TRecordIndexer.GetInt16(const Index: Integer): Int16;
var
  LIdx: Integer;
begin
  LIdx := Index + DoCalculatesOffset(Index);
  if LIdx < BufferLength then
    Result := PSmallInt(@FBuffer[Index + DoCalculatesOffset(Index)])^
  else
    Result := 0
end;

function TRecordIndexer.GetInt64(const Index: Integer): Int64;
var
  LIdx: Integer;
begin
  LIdx := Index + DoCalculatesOffset(Index);
  if LIdx < BufferLength then
    Result := PInt64(@FBuffer[Index + DoCalculatesOffset(Index)])^
  else
    Result := 0
end;

function TRecordIndexer.GetInt8(const Index: Integer): Int8;
var
  LIdx: Integer;
begin
  LIdx := Index + DoCalculatesOffset(Index);
  if LIdx < BufferLength then
    Result := PShortInt(@FBuffer[Index + DoCalculatesOffset(Index)])^
  else
    Result := 0
end;

function TRecordIndexer.GetInteger(const Index: Integer): Integer;
var
  LIdx: Integer;
begin
  LIdx := Index + DoCalculatesOffset(Index);
  if LIdx < BufferLength then
    Result := PInteger(@FBuffer[Index + DoCalculatesOffset(Index)])^
  else
    Result := 0
end;

function TRecordIndexer.GetNibbleTail(const Index: Integer): Byte;
var
  LIdx: Integer;
begin
  LIdx := Index + DoCalculatesOffset(Index);
  if LIdx < BufferLength then
    Result := GetByte(Index + DoCalculatesOffset(Index)) and $F
  else
    Result := 0
end;

function TRecordIndexer.GetUInt64(const Index: Integer): UInt64;
var
  LIdx: Integer;
begin
  LIdx := Index + DoCalculatesOffset(Index);
  if LIdx < BufferLength then
    Result := PUInt64(@FBuffer[Index + DoCalculatesOffset(Index)])^
  else
    Result := 0
end;

function TRecordIndexer.GetNibbleHead(const Index: Integer): Byte;
var
  LIdx: Integer;
begin
  LIdx := Index + DoCalculatesOffset(Index);
  if LIdx < BufferLength then
    Result := GetByte(Index + DoCalculatesOffset(Index)) shr 4
  else
    Result := 0
end;

function TRecordIndexer.GetWord(const Index: Integer): Word;
var
  LIdx: Integer;
begin
  LIdx := Index + DoCalculatesOffset(Index);
  if LIdx < BufferLength then
    Result := PWord(@FBuffer[Index + DoCalculatesOffset(Index)])^
  else
    Result := 0
end;

end.
