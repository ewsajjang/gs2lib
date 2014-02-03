unit mIntervalCounter;

interface

uses
  System.SysUtils, System.Classes, System.Math;

type
  IIntervalCounter = interface
    ['{4B4E3216-7DF7-4AD0-B244-FF2669C9A001}']
    function GetRange: Integer;
    function GetCount: Integer;
    function GetStrIdx(Interval: Integer): Integer;
    function GetEndIdx(Interval: Integer): Integer;
    function GetIntervalRange(Interval: Integer): Integer;

    procedure Init(const ABegin, AEnd, AInterval: Integer);

    property Range: Integer read GetRange;
    property Count: Integer read GetCount;
    property StrIdx[Interval: Integer]: Integer read GetStrIdx;
    property EndIdx[Interval: Integer]: Integer read GetEndIdx;
    property IntervalRange[Interval: Integer]: Integer read GetIntervalRange;
  end;

  EIntervalCounter = class(Exception);
    EIntervalAssignedByZero = class(EIntervalCounter);
    EIntervalAssignedByNegative = class(EIntervalCounter);
    ERangeParamsAssignedByZero = class(EIntervalCounter);
    ERangeParamsAssignedByNegative = class(EIntervalCounter);
    ERangeParamsOutOfRange = class(EIntervalCounter);
  TIntervalCounter = class(TInterfacedObject, IIntervalCounter)
  private
    FGetIntervalCnt: Integer;
    FInterval, FMin, FMax: Integer;

    function GetRange: Integer;
    function GetCount: Integer;
    function GetStrIdx(Interval: Integer): Integer;
    function GetEndIdx(Interval: Integer): Integer;
    function GetIntervalRange(Interval: Integer): Integer;
  public
    procedure Init(const ABegin, AEnd, AInterval: Integer);

    property Range: Integer read GetRange;
    property Count: Integer read GetCount;
    property StrIdx[Interval: Integer]: Integer read GetStrIdx;
    property EndIdx[Interval: Integer]: Integer read GetEndIdx;
    property IntervalRange[Interval: Integer]: Integer read GetIntervalRange;
  end;

implementation

uses
  mSysUtilsEx;

{ TIntervalIndexer }

function TIntervalCounter.GetIntervalRange(Interval: Integer): Integer;
begin
  Result := EndIdx[Interval] - StrIdx[Interval];
  Inc(Result);
end;

function TIntervalCounter.GetCount: Integer;
begin
  Result := FGetIntervalCnt;
end;

function TIntervalCounter.GetRange: Integer;
begin
  Result := FMax - FMin;
  Inc(Result);
end;

function TIntervalCounter.GetEndIdx(Interval: Integer): Integer;
begin
  if Interval < Count - 1 then
    Result := FMin + (FInterval * (Interval + 1) ) - 1
  else
    Result := Min(FMax, FMin + (FInterval * (Interval + 1) ) );
end;

function TIntervalCounter.GetStrIdx(Interval: Integer): Integer;
begin
  if Interval = 0 then
    Result := FMin
  else
    Result := FMin + (FInterval * Interval);
end;

procedure TIntervalCounter.Init(const ABegin, AEnd, AInterval: Integer);
begin
  if (ABegin = 0) or (AEnd = 0) then
    raise ERangeParamsAssignedByZero.Create('ABegin or AEnd params can not assigned by zero');
  if (ABegin < 0) or (AEnd < 0) then
    raise ERangeParamsAssignedByNegative.Create('ABegin or AEnd params can not assigned by negative');
  if ABegin > AEnd then
    raise ERangeParamsOutOfRange.Create('AEnd parameter is always greater then or equal to ABegin parameter');
  if AInterval = 0 then
    raise EIntervalAssignedByZero.Create('AInterval can not assigned by zero');
  if AInterval < 0 then
    raise EIntervalAssignedByNegative.Create('AInterval cannot assigned by negative');

  FInterval := AInterval;
  FMin := ABegin;
  FMax := AEnd;
  FGetIntervalCnt := MinimumCount(AEnd - ABegin + 1, AInterval);
end;

end.
