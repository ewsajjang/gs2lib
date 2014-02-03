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

    procedure Init(const AMin, AMax, AInterval: Integer);

    property Range: Integer read GetRange;
    property Count: Integer read GetCount;
    property StrIdx[Interval: Integer]: Integer read GetStrIdx;
    property EndIdx[Interval: Integer]: Integer read GetEndIdx;
    property IntervalRange[Interval: Integer]: Integer read GetIntervalRange;
  end;

  EIntervalIndexer = Exception;
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
    procedure Init(const AMin, AMax, AInterval: Integer);

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

procedure TIntervalCounter.Init(const AMin, AMax, AInterval: Integer);
begin
  FInterval := AInterval;
  FMin := AMin;
  FMax := AMax;
  FGetIntervalCnt := MinimumCount(AMax - AMin + 1, AInterval);
end;

end.
