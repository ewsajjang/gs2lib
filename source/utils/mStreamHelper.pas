unit mStreamHelper;

interface

uses
  System.Classes, System.SysUtils
  ;

type
  TBytesStreamHelper = class helper for TBytesStream
  private
  public
    function PosOfStart(const ATarget: TArray<Byte>; const AStartPos: Integer = 0; const AExclusive: Boolean = True): Integer; overload;
    function PosOfStart(const ATarget: TArray<Byte>; const AStartPos: Integer; var AFindIdx: Integer; const AExclusive: Boolean = True): Boolean; overload;
    function PosOfStart(const ATargets: TArray<TArray<Byte>>; const AStartPos: Integer; var AFindIdx: Integer; const AExclusive: Boolean = True): Boolean; overload;
    function PosOfStart(const ATargets: TArray<TArray<Byte>>; const AStartPos: Integer = 0; const AExclusive: Boolean = True): Integer; overload;
    function PosOfEnd(const AEtx: TArray<Byte>; var AEtxIdx: Integer; const AExclusive: Boolean = True): Boolean; overload;
    function PosOfEnd(const ATarget: TArray<Byte>; const AExclusive: Boolean = True): Integer; overload;
    function PosOfEnd(const ATargets: TArray<TArray<Byte>>; var AEtxIdx: Integer; const AExclusive: Boolean = True): Boolean; overload;
    function PosOfEnd(const ATargets: TArray<TArray<Byte>>; const AExclusive: Boolean = True): Integer; overload;
  end;

implementation

uses
  System.Math
  ;

{ TBytesStreamHelper }

function TBytesStreamHelper.PosOfStart(const ATarget: TArray<Byte>;
  const AStartPos: Integer; const AExclusive: Boolean): Integer;
begin
  if not PosOfStart(ATarget, AStartPos, Result, AExclusive) then
    Result := -1;
end;

function TBytesStreamHelper.PosOfEnd(const AEtx: TArray<Byte>;
  var AEtxIdx: Integer; const AExclusive: Boolean): Boolean;
var
  LLenOfEtx, i: Integer;
  LPosOfEnd: Int64;
begin
  Result := False;
  LLenOfEtx := Length(AEtx);
  LPosOfEnd := Size;
  AEtxIdx := LPosOfEnd;// - LLenOfEtx;
  while InRange(AEtxIdx, 0, LPosOfEnd{ -LLenOfEtx}) do
  begin
    i := 0;
    while (i < LLenOfEtx) and (Bytes[AEtxIdx + i] = AEtx[i]) do
      Inc(i);
    Result := i = LLenOfEtx;
    if Result then
    begin
      Inc(AEtxIdx, IfThen(Not AExclusive, LLenOfEtx));
      Break;
    end;
    Dec(AEtxIdx);
  end;
end;

function TBytesStreamHelper.PosOfEnd(const ATarget: TArray<Byte>; const AExclusive: Boolean): Integer;
begin
  if not PosOfEnd(ATarget, Result, AExclusive) then
    Result := -1;
end;

function TBytesStreamHelper.PosOfEnd(const ATargets: TArray<TArray<Byte>>; var AEtxIdx: Integer;
  const AExclusive: Boolean): Boolean;
var
  LTarget: TBytes;
  LTargetLen, i: Integer;
begin
  Result := False;
  AEtxIdx := Size -1;
  while AEtxIdx > -1 do
  begin
    for LTarget in ATargets do
    begin
      LTargetLen := Length(LTarget);
      if InRange(AEtxIdx, 0, Size -1) then
      begin
        i := 0;
        while (i < LTargetLen) and (Bytes[AEtxIdx + i] = LTarget[i]) do
          Inc(i);
        if i = LTargetLen then
        begin
          Inc(AEtxIdx, IfThen(not AExclusive, LTargetLen));
          Exit(True);
        end;
      end;
    end;
    Dec(AEtxIdx);
  end;
end;

function TBytesStreamHelper.PosOfEnd(const ATargets: TArray<TArray<Byte>>; const AExclusive: Boolean): Integer;
var
  LIdx: Integer;
begin
  if PosOfEnd(ATargets, LIdx, AExclusive) then
    Result := LIdx
  else
    Result := -1;
end;

function TBytesStreamHelper.PosOfStart(
  const ATargets: TArray<TArray<Byte>>; const AStartPos: Integer;
  const AExclusive: Boolean): Integer;
begin
  if not PosOfStart(ATargets, AStartPos, Result, AExclusive) then
    Result := -1;
end;

function TBytesStreamHelper.PosOfStart(const ATargets: TArray<TArray<Byte>>;
  const AStartPos: Integer; var AFindIdx: Integer;
  const AExclusive: Boolean): Boolean;
var
  i: Integer;
  LTarget: TBytes;
  LTargetLen: Integer;
begin
  Result := False;
  AFindIdx:= AStartPos;
  while AFindIdx <= Size do
  begin
    for LTarget in ATargets do
    begin
      LTargetLen := Length(LTarget);
      if (AFindIdx + LTargetLen > Size) then
        Break;

      i := 0;
      while (i < LTargetLen) and (Bytes[AFindIdx + i] = LTarget[i]) do
        Inc(i);
      if i = LTargetLen then
      begin
        Inc(AFindIdx, IfThen(AExclusive, LTargetLen));
        Exit(True);
      end
    end;
    Inc(AFindIdx);
  end;
end;

function TBytesStreamHelper.PosOfStart(const ATarget: TArray<Byte>;
  const AStartPos: Integer; var AFindIdx: Integer;
  const AExclusive: Boolean): Boolean;
var
  i: Integer;
begin
  Result := False;
  AFindIdx:= AStartPos;
  while (AFindIdx + Length(ATarget) <= Size) do
  begin
    i := 0;
    while (i < Length(ATarget)) and (Bytes[AFindIdx + i] = ATarget[i]) do
      Inc(i);
    Result := i = Length(ATarget);
    if Result then
    begin
      Inc(AFindIdx, IfThen(AExclusive, Length(ATarget)));
      Break;
    end;
    Inc(AFindIdx);
  end;
end;

end.
