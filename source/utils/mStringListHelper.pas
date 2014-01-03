unit mStringListHelper;

interface

uses
  System.Classes, System.SysUtils, System.UITypes, System.Types;

type
  TSequentialIdx = (siExist, siAdd, siInsert);

  TStringListHelper = class helper for TStringList
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

  public
    procedure TryDeleteByS(const AValue: String);
    procedure PriorityS(const AName, AValue: String);

    function SequentialIdx(S: String; var AIdx: Integer): TSequentialIdx;
    function MergeString: String;
    function AddFmt(const S: string; const Args: array of const): Integer;
    function Integers(Name: String): Integer;
    function Exists(const AKey: String): Boolean;
    function KeysExists(const AKeys: array of String; out ANotExistKeys: TArray<string>): Boolean; overload;
    function KeysExists(const AKeys: array of String): Boolean; overload;
    function KeyExists(const AKey: String): Boolean; overload;

//    procedure O(Index: Integer; AObj: TObject); overload;
//    function O<T: class>(Index: Integer): T; overload;

    property S[Name: String]: String read GetS write SetS;
    property I[Name: String]: Integer read GetI write SetI;
    property B[Name: String]: Boolean read GetB write SetB;
    property D[Name: String]: Double read GetD write SetD;
    property DT[Name: String]: TDateTime read GetDT write SetDT;
    property C[Name: String]: TColor read GetC write SetC;
  end;

implementation

uses
  mConsts, mDateTimeHelper, DateUtils,
  System.UIConsts;

{ TStringListHelper }

function TStringListHelper.AddFmt(const S: string; const Args: array of const): Integer;
begin
  Result := Add(Format(S, Args));
end;

function TStringListHelper.Exists(const AKey: String): Boolean;
begin
  Result := IndexOfName(AKey) <> -1;
end;

function TStringListHelper.GetB(Name: String): Boolean;
begin
  Result := Values[Name] = BoolStr[True];
end;

function TStringListHelper.GetC(Name: String): TColor;
begin
  Result := StringToColor(Values[Name]);
end;

function TStringListHelper.GetD(Name: String): Double;
begin
  Result:= StrToFloatDef(Values[Name], 0);
end;

function TStringListHelper.GetDT(Name: String): TDateTime;
begin
  Result := TDateTime.FmtISO8601(Values[Name]);
end;

function TStringListHelper.GetI(Name: String): Integer;
begin
  Result := StrToIntDef(Values[Name], 0);
end;

function TStringListHelper.GetS(Name: String): String;
begin
  Result := Values[Name];
end;

function TStringListHelper.Integers(Name: String): Integer;
begin
  if Values[Name] = EmptyStr then
    Result := 0
  else
    Result := StrToIntDef(Values[Name], 0);
end;

function TStringListHelper.KeysExists(const AKeys: array of String; out ANotExistKeys: TArray<string>): Boolean;
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

function TStringListHelper.KeyExists(const AKey: String): Boolean;
begin
  Result := Exists(AKey);
end;

function TStringListHelper.KeysExists(const AKeys: array of String): Boolean;
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

function TStringListHelper.MergeString: String;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := 0 to Count - 1 do
    Result := Result + Strings[i];
end;

function TStringListHelper.SequentialIdx(S: String;
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

procedure TStringListHelper.SetB(Name: String; const Value: Boolean);
begin
  Values[Name] := BoolStr[Value];
end;

procedure TStringListHelper.SetC(Name: String; const Value: TColor);
begin
  Values[Name] := ColorToString(Value);
end;

procedure TStringListHelper.SetD(Name: String; const Value: Double);
begin
  Values[Name]:= FloatToStr(Value);
end;

procedure TStringListHelper.SetDT(Name: String; const Value: TDateTime);
begin
  Values[Name] := Value.ToISO8601Str;
end;

procedure TStringListHelper.SetI(Name: String; const Value: Integer);
begin
  Values[Name] := IntToStr(Value);
end;

procedure TStringListHelper.SetS(Name: String; const Value: String);
begin
  Values[Name] := Value;
end;

procedure TStringListHelper.TryDeleteByS(const AValue: String);
var
  LIdx: Integer;
begin
  LIdx := IndexOf(AValue);
  if LIdx > -1 then
    Delete(LIdx);
end;

procedure TStringListHelper.PriorityS(const AName, AValue: String);
begin
  S[AName] := AValue;
  if AValue.IsEmpty and not Exists(AName) then
    Add(AName + NameValueSeparator);
end;

end.
