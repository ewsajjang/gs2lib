unit mMath;

interface

function CompensationTrunc(const AValue: Single): Single; overload;
function CompensationTrunc(const AValue: Double): Double; overload;
function CompensationTrunc(const AValue: Extended): Extended; overload;

implementation

function CompensationTrunc(const AValue: Single): Single; overload;
begin
  Result := Trunc(AValue + 0.000001);
end;

function CompensationTrunc(const AValue: Double): Double; overload;
begin
  Result := Trunc(AValue + 0.000001);
end;

function CompensationTrunc(const AValue: Extended): Extended; overload;
begin
  Result := Trunc(AValue + 0.000001);
end;

end.
