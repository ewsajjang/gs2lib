unit mStreamHelper;

interface

uses
  System.Classes, System.SysUtils
  ;

type
  TBytesStreamHelper = class helper for TBytesStream
  public
    function PosOfStart(const ATarget: TBytes; const AStartPos: Integer = 0): Integer; overload;
    function PosOfStart(const ATarget: TBytes; const AStartPos: Integer; var AFindIdx: Integer): Boolean; overload;
    function PosOfEnd(const ATarget: TBytes; var AFindIdx: Integer): Boolean; overload;
    function PosOfEnd(const ATarget: TBytes): Integer; overload;
  end;

implementation

uses
  System.Math
  ;

{ TBytesStreamHelper }

function TBytesStreamHelper.PosOfStart(const ATarget: TBytes; const AStartPos: Integer): Integer;
begin
  if not PosOfStart(ATarget, AStartPos, Result) then
    Result := -1;
end;

function TBytesStreamHelper.PosOfEnd(const ATarget: TBytes;
  var AFindIdx: Integer): Boolean;
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
      Break;
    Dec(AFindIdx);
  end;
end;

function TBytesStreamHelper.PosOfEnd(const ATarget: TBytes): Integer;
begin
  if not PosOfEnd(ATarget, Result) then
    Result := -1;
end;

function TBytesStreamHelper.PosOfStart(const ATarget: TBytes;
  const AStartPos: Integer; var AFindIdx: Integer): Boolean;
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
      Break;
    Inc(AFindIdx);
  end;
end;

end.
