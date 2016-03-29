unit mStringListHelper;

interface

uses
  System.Classes, System.SysUtils, System.UITypes, System.Types;

type
  TSequentialIdx = (siExist, siAdd, siInsert);

  TStringsHelper = class helper for TStrings
  private
    function GetB(Name: String): Boolean;
    function GetD(Name: String): Double;
    function GetDT(Name: String): TDateTime;
    function GetI(Name: String): Integer;
    function GetS(Name: String): String;
    procedure SetB(Name: String; const Value: Boolean);
    procedure SetD(Name: String; const Value: Double);
    procedure SetDT(Name: String; const Value: TDateTime);
    procedure SetI(Name: String; const Value: Integer);
    procedure SetS(Name: String; const Value: String);
    function GetC(Name: String): TColor;
    procedure SetC(Name: String; const Value: TColor);
    function GetGUID(Name: String): TGUID;
    procedure SetGUID(Name: String; const Value: TGUID);

  public
    procedure PriorityS(const AName, AValue: String);

    function SequentialIdx(S: String; var AIdx: Integer): TSequentialIdx;
    function MergeString: String;

    function Integers(Name: String; Defualt: Integer = 0): Integer;
    function Exists(const AKey: String): Boolean;
    function KeysExists(const AKeys: array of String; out ANotExistKeys: TArray<string>): Boolean; overload;
    function KeysExists(const AKeys: array of String; out ANotExistKeys: String): Boolean; overload;
    function KeysExists(const AKeys: array of String): Boolean; overload;
    function KeyExists(const AKey: String): Boolean; overload;
    function DelimitedValues(const ADelimiter: Char): String;

    function DelimitedKeys(const ADelimiter: Char): String;
    function CommaKeys: String;
    function CommaValues: String;

    function O<T: Class>(const AIdx: Integer): T; overload;
    function OExtract<T: Class>(const AIdx: Integer): T; overload;

    property S[Name: String]: String read GetS write SetS;
    property I[Name: String]: Integer read GetI write SetI;
    property B[Name: String]: Boolean read GetB write SetB;
    property D[Name: String]: Double read GetD write SetD;
    property DT[Name: String]: TDateTime read GetDT write SetDT;
    property C[Name: String]: TColor read GetC write SetC;
    property GUID[Name: String]: TGUID read GetGUID write SetGUID;
  end;

  TStringListHelper = class helper(TStringsHelper) for TStringList
  private
  public
    procedure TryDelete(const AValue: String);
    function AddFmt(const S: string; const Args: array of const): Integer;

    function O<T: Class>(const AName: String): T; overload;
    function O(const AName: String; AObj: TObject): Integer; overload;
    function OSwap(const AName: String; const AObj: TObject; AFree: Boolean = True): Integer;

    function OExtract<T: Class>(const AName: String): T; overload;
  end;

implementation

uses
  mConsts, mDateTimeHelper, DateUtils,
  System.UIConsts;

function TStringsHelper.DelimitedKeys(const ADelimiter: Char): String;
var
  LValue: String;
  i: Integer;
begin
  for i := 0 to Count -1 do
  begin
    LValue := Names[i];
    if not LValue.IsEmpty then
    begin
      if Result.IsEmpty then
        Result := LValue
      else
        Result := LValue + ADelimiter;
      Result := Result + LValue
    end;
  end;
end;

function TStringsHelper.DelimitedValues(const ADelimiter: Char): String;
var
  LValue: String;
  i: Integer;
begin
  for i := 0 to Count -1 do
  begin
    LValue := ValueFromIndex[i];
    if not LValue.IsEmpty then
    begin
      if Result.IsEmpty then
        Result := LValue
      else
        Result := LValue + ADelimiter;
      Result := Result + LValue
    end;
  end;
end;

function TStringsHelper.Exists(const AKey: String): Boolean;
begin
  Result := IndexOfName(AKey) <> -1;
end;

function TStringsHelper.GetB(Name: String): Boolean;
begin
  Result := Values[Name] = BoolStr[True];
end;

function TStringsHelper.GetC(Name: String): TColor;
begin
  Result := StringToColor(Values[Name]);
end;

function TStringsHelper.GetD(Name: String): Double;
begin
  Result:= StrToFloatDef(Values[Name], 0);
end;

function TStringsHelper.GetDT(Name: String): TDateTime;
begin
  Result := TDateTime.FmtISO8601(Values[Name]);
end;

function TStringsHelper.GetGUID(Name: String): TGUID;
begin
  try
    Result := StringToGUID(Values[Name]);
  except on E: Exception do
    Result := TGUID.Empty;
  end;
end;

function TStringsHelper.GetI(Name: String): Integer;
begin
  Result := StrToIntDef(Values[Name], 0);
end;

function TStringsHelper.GetS(Name: String): String;
begin
  Result := Values[Name];
end;

function TStringsHelper.Integers(Name: String; Defualt: Integer): Integer;
begin
  if Values[Name] = EmptyStr then
    Result := Defualt
  else
    Result := StrToIntDef(Values[Name], Defualt);
end;

function TStringsHelper.KeysExists(const AKeys: array of String; out ANotExistKeys: TArray<string>): Boolean;
const
  SEPERATOR: Char = ',';
var
  LKey: String;
  LNotExistsKeys: String;
begin
  for LKey in AKeys do
    if not KeyExists(LKey) then
      LNotExistsKeys := LNotExistsKeys.Join(SEPERATOR, [LKey]);
  Result := LNotExistsKeys.IsEmpty;
  ANotExistKeys := LNotExistsKeys.Split(SEPERATOR);
end;

function TStringsHelper.KeyExists(const AKey: String): Boolean;
begin
  Result := Exists(AKey);
end;

function TStringsHelper.KeysExists(const AKeys: array of String): Boolean;
var
  LKey: String;
begin
  for LKey in AKeys do
    if not KeyExists(LKey) then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

function TStringsHelper.KeysExists(const AKeys: array of String;
  out ANotExistKeys: String): Boolean;
const
  SEPERATOR: Char = ',';
var
  LKey: String;
begin
  ANotExistKeys := EmptyStr;
  for LKey in AKeys do
    if not KeyExists(LKey) then
      ANotExistKeys := ANotExistKeys.Join(SEPERATOR, [LKey]);
  Result := ANotExistKeys.IsEmpty;
end;

function TStringsHelper.MergeString: String;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := 0 to Count - 1 do
    Result := Result + Strings[i];
end;

procedure TStringsHelper.SetB(Name: String; const Value: Boolean);
begin
  Values[Name] := BoolStr[Value];
end;

procedure TStringsHelper.SetC(Name: String; const Value: TColor);
begin
  Values[Name] := ColorToString(Value);
end;

procedure TStringsHelper.SetD(Name: String; const Value: Double);
begin
  Values[Name]:= FloatToStr(Value);
end;

procedure TStringsHelper.SetDT(Name: String; const Value: TDateTime);
begin
  Values[Name] := Value.ToISO8601Str;
end;

procedure TStringsHelper.SetGUID(Name: String; const Value: TGUID);
begin
  Values[NAME] := Value.ToString;
end;

procedure TStringsHelper.SetI(Name: String; const Value: Integer);
begin
  Values[Name] := IntToStr(Value);
end;

procedure TStringsHelper.SetS(Name: String; const Value: String);
begin
  Values[Name] := Value;
end;

procedure TStringsHelper.PriorityS(const AName, AValue: String);
begin
  S[AName] := AValue;
  if AValue.IsEmpty and not Exists(AName) then
    Add(AName + NameValueSeparator);
end;

function TStringsHelper.SequentialIdx(S: String;
  var AIdx: Integer): TSequentialIdx;
var
  L, H, C: Integer;
begin
  AIdx := 0;
  if Count = 0 then
    Result := siAdd
  else
  begin
    L := 0;
    H := Count - 1;
    while L <= H do
    begin
      AIdx := (L + H) shr 1;
      C := CompareStr(Strings[AIdx], S);
      if C < 0 then
        L := AIdx + 1
      else if C > 0 then
        H := AIdx - 1
      else
        Break;
    end;

    C := CompareStr(Strings[AIdx], S);
    if C < 0 then
    begin
      AIdx := AIdx + 1;
      Result := siAdd;
    end
    else if C = 0 then
      Result := siExist
    else
      Result := siInsert;
  end;
end;

function TStringsHelper.CommaKeys: String;
begin
  Result := DelimitedKeys(',')
end;

function TStringsHelper.CommaValues: String;
begin
  Result := DelimitedValues(',');
end;

function TStringsHelper.O<T>(const AIdx: Integer): T;
begin
  Result := Objects[AIdx] as T
end;

function TStringsHelper.OExtract<T>(const AIdx: Integer): T;
begin
  Result := O<T>(AIdx);
  Delete(AIdx);
end;

{ TStringListHelper }

procedure TStringListHelper.TryDelete(const AValue: String);
var
  LIdx: Integer;
begin
  LIdx := IndexOf(AValue);
  if LIdx > -1 then
    Delete(LIdx);
end;

function TStringListHelper.AddFmt(const S: string; const Args: array of const): Integer;
begin
  Result := Add(Format(S, Args));
end;

function TStringListHelper.O(const AName: String; AObj: TObject): Integer;
begin
  Result := AddObject(AName, AObj);
end;

function TStringListHelper.OExtract<T>(const AName: String): T;
var
  LIdx: Integer;
begin
  LIdx := IndexOf(AName);
  Result := O<T>(AName);
  Delete(LIdx);
end;

function TStringListHelper.OSwap(const AName: String; const AObj: TObject;
  AFree: Boolean): Integer;
var
  LIdx: Integer;
begin
  LIdx := IndexOfName(AName);
  if LIdx > VAL_NOT_ASSIGNED then
    Delete(LIdx);
  Result := AddObject(AName, AObj);
end;

function TStringListHelper.O<T>(const AName: String): T;
begin
  Result := Objects[IndexOf(AName)] as T
end;


end.
