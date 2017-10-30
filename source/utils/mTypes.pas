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

  TNotifySimple = procedure of object;

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

  TBytesArray = TArray<TBytes>;
  TBytesArrayHelper = record helper for TBytesArray
    class function Parse(const AValue: String): TBytesArray; overload; static;
    class function ToString(const AValue: TBytesArray): String; overload; static;
    function ToString: String; overload;
  end;

  TBytesHelper = record helper for TBytes
    class function Parse(const AValue: String): TBytes; overload; static;
    class function ToString(const AValue: TBytes): String; overload; static;
    function ToString: String; overload;
  end;

  TStringArray = TArray<String>;

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

//function EncodeBytesArray(const ASrc: String): TArray<TBytes>;

implementation

uses
  mDateTimeHelper, mTypes.bytesarray;

{ TBytesArrayHelper }

class function TBytesArrayHelper.Parse(const AValue: String): TBytesArray;
begin
  Result := TBytesArrayEncoder.Execute(AValue)
end;

class function TBytesArrayHelper.ToString(const AValue: TBytesArray): String;
begin
  Result := TBytesArrayDecoder.Execute(AValue)
end;

function TBytesArrayHelper.ToString: String;
begin
  Result := TBytesArrayDecoder.Execute(Self)
end;

{ TBytesHelper }

class function TBytesHelper.Parse(const AValue: String): TBytes;
begin
  Result := TBytesEncoder.Execute(AValue)
end;

class function TBytesHelper.ToString(const AValue: TBytes): String;
begin
  Result := TBytesDecoder.Execute(AValue)
end;

function TBytesHelper.ToString: String;
begin
  Result := TBytesDecoder.Execute(Self)
end;

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
