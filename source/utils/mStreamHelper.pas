unit mStreamHelper;

interface

uses
  System.Classes, System.SysUtils
  ;

type
  TBytesStreamHelper = class helper for TBytesStream
  private
  public
    function PosOfStart(const ATarget: TBytes; const AStartPos: Integer = 0; const AExclusive: Boolean = True): Integer; overload;
    function PosOfStart(const ATarget: TBytes; const AStartPos: Integer; var AFindIdx: Integer; const AExclusive: Boolean = True): Boolean; overload;
    function PosOfEnd(const ATarget: TBytes; var AFindIdx: Integer; const AExclusive: Boolean = True): Boolean; overload;
    function PosOfEnd(const ATarget: TBytes; const AExclusive: Boolean = True): Integer; overload;
  end;

implementation

uses
  System.Math
  ;

{ TBytesStreamHelper }

function TBytesStreamHelper.PosOfStart(const ATarget: TBytes;
  const AStartPos: Integer; const AExclusive: Boolean): Integer;
begin
  if not PosOfStart(ATarget, AStartPos, Result, AExclusive) then
    Result := -1;
end;

function TBytesStreamHelper.PosOfEnd(const ATarget: TBytes;
  var AFindIdx: Integer; const AExclusive: Boolean): Boolean;
var
  LEnd, LTargetLen, i: Integer;
begin
  Result := False;
  LTargetLen := Length(ATarget);
  LEnd := Size;
  AFindIdx := LEnd -LTargetLen;
  while InRange(AFindIdx, 0, LEnd -LTargetLen) do
  begin
    i := 0;
    while (i < LTargetLen) and (Bytes[AFindIdx + i] = ATarget[i]) do
      Inc(i);
    Result := i = LTargetLen;
    if Result then
    begin
      Inc(AFindIdx, IfThen(Not AExclusive, LTargetLen));
      Break;
    end;
    Dec(AFindIdx);
  end;
end;

function TBytesStreamHelper.PosOfEnd(const ATarget: TBytes; const AExclusive: Boolean): Integer;
begin
  if not PosOfEnd(ATarget, Result, AExclusive) then
    Result := -1;
end;

function TBytesStreamHelper.PosOfStart(const ATarget: TBytes;
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
