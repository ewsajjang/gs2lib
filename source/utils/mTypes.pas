unit mTypes;

interface

uses
  System.SysUtils, System.Classes, System.DateUtils, System.Types, System.Rtti,
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

  TBytesList = class(TList<TBytes>)
    function Copy: TBytesList;
  end;

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

  TResult<T, T1> = record
    Value: T;
    Param: T1;
    constructor Create(const AValue: T; const AParam: T1);
  end;

  TResult<T, T1, T2> = record
    Value: T;
    Param1: T1;
    Param2: T2;
    constructor Create(const AValue: T; const AParam1: T1; const AParam2: T2);
  end;

implementation

uses
  mDateTimeHelper;

{ TProgressInfo }

constructor TProgressInfo.Create(const ATotal: Boolean; AValue: Int64);
begin
  Total := ATotal;
  Value := AValue;
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

{ TBytesList }

function TBytesList.Copy: TBytesList;
var
  i: Integer;
  LItem: TBytes;
  j: Integer;
begin
  Result := TBytesList.Create;
  for i := 0 to Count - 1 do
  begin
    SetLength(LItem, Length(Items[i]));
    for j := Low(Items[i]) to High(Items[i]) do
      LItem[j] := ITems[i][j];
    //CopyArray(@LItem[0], @(Items[i][0]), TypeInfo(TBytes), Length(Items[i]));
    Result.Add(LItem);
  end;
end;

{ TResult<T, T1> }

{ TResult<T, T1> }

constructor TResult<T, T1>.Create(const AValue: T; const AParam: T1);
begin
  Value := AValue;
  Param := AParam;
end;

{ TResult<T, T1, T2> }

constructor TResult<T, T1, T2>.Create(const AValue: T; const AParam1: T1;
  const AParam2: T2);
begin
  Value := AValue;
  Param1 := AParam1;
  Param2 := AParam2;
end;

end.
