unit mTypes;

interface

uses
  System.SysUtils, System.Classes, System.DateUtils,
  System.Generics.Collections;

type
  TNotifyBoolean = procedure(Sender: TObject; Value: Boolean) of object;
  TNotifyPointer = procedure(Sender: TObject; Value: Pointer; Length: Int64) of object;
  TNotifyBytes   = procedure(Sender: TObject; Value: TBytes) of object;
  TNotifyError   = procedure(Sender: TObject; ErCode: Integer; ErMsg: String) of object;
  TNotifyProgress= procedure(Sender: TObject; Total: Boolean; Value: Int64) of object;
  TNotifyInteger = procedure(Sender: TObject; Value: Integer) of object;
  TNotifyString  = procedure(Sender: TObject; Value: String) of object;

  TProcBoolean = reference to procedure(const Value: Boolean);
  TProcPointer = reference to procedure(const Sender: TObject; const Value: Pointer; const Length: Int64);
  TProcBytes = reference to procedure(const Value: TBytes);
  TProcError = reference to procedure(const Sender: TObject; const ErCode: Integer; const ErMsg: String);
  TProcProgress= reference to procedure(const Total: Boolean; const Value: Int64);
  TProcInteger = reference to procedure(const Sender: TObject; const Value: Integer);
  TProcStr = reference to procedure(const Value: String);

  TBytesList = TList<TBytes>;

  TProgressInfo = record
    Total: Boolean;
    Value: Int64;
    constructor Create(const ATotal: Boolean; AValue: Int64);
  end;

  TPeriod = record
    Min, Max: TDateTime;
    function Equal(const AValue: TPeriod): Boolean;
    function Contain(const AValue: TDateTime): Boolean;
    class function Create(AMin, AMax: TDateTime): TPeriod; static;
  end;

implementation

uses
  mDateTimeHelper;

{ TProgressInfo }

constructor TProgressInfo.Create(const ATotal: Boolean; AValue: Int64);
begin
  Total := ATotal;
  Value := Value;
end;

{ TPeriod }

function TPeriod.Contain(const AValue: TDateTime): Boolean;
begin
  Result := AValue.InRange(Min.StartOfDay, Max.EndOfDay);
end;

class function TPeriod.Create(AMin, AMax: TDateTime): TPeriod;
begin
  Result.Min := AMin;
  Result.Max := AMax;
end;

function TPeriod.Equal(const AValue: TPeriod): Boolean;
begin
  Result := Self.Min.Equals(AValue.Min) and
    Self.Max.Equals(AValue.Max)
end;

end.
