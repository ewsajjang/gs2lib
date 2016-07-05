unit mStreamHelper;

interface

uses
  System.Classes, System.SysUtils
  ;

type
  TBytesStreamHelper = class helper for TBytesStream
  public
    function PositionOf(const ATarget: TBytes; const AStartPos: Integer = 0): Integer; overload;
    function PositionOf(const ATarget: TBytes; const AStartPos: Integer; var AFindIdx: Integer): Boolean; overload;
  end;

implementation

{ TBytesStreamHelper }

function TBytesStreamHelper.PositionOf(const ATarget: TBytes; const AStartPos: Integer): Integer;
begin
  if not PositionOf(ATarget, AStartPos, Result) then
    Result := -1;
end;

function TBytesStreamHelper.PositionOf(const ATarget: TBytes;
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
