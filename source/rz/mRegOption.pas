unit mRegOption;

interface

uses
  mPreferences,

  RzCommon,

  System.Classes, System.SysUtils
  ;

type
  TRegOption = class(TDataModule)
  private
    FReg: TRzRegIniFile;
  protected
    FIdx: TOptionIndexer;
    procedure DoCreate; override;
    procedure DoDestroy; override;

    function GetRegIniFile(var APath: String): TRzRegIniFile; virtual; abstract;

    function GetString(const Index: Integer): String;
    procedure SetString(const Index: Integer; const Value: String);
    function GetInteger(const Index: Integer): Integer;
    function GetInt64(const Index: Integer): Int64;
    function GetUInt64(const Index: Integer): UInt64;
    procedure SetInteger(const Index: Integer; const Value: Integer);
    procedure SetInt64(const Index: Integer; const Value: Int64);
    procedure SetUInt64(const Index: Integer; const Value: UInt64);
    function GetFloat(const Index: Integer): Double;
    procedure SetFloat(const Index: Integer; const Value: Double);
    function GetBool(const Index: Integer): Boolean;
    procedure SetBool(const Index: Integer; const Value: Boolean);
    function  GetStringArray(const Index: Integer): TArray<string>;
    procedure SetStringArray(const Index: Integer; const AValues: TArray<string>);
    function GetIntegerArray(const Index: Integer): TArray<Integer>;
    procedure SetIntegerArray(const Index: Integer; const AValues: TArray<Integer>);

    function mmSecFromSec(const Index: Integer): Integer;
    function mmSecFromMin(const Index: Integer): Integer;
    procedure SecToMmSec(const Index, Value: Integer);
    procedure MinToMmSec(const Index, Value: Integer);

    procedure Add(const ASection: String; const ANameDefault: TArray<TVariantArray>); virtual;
  public const
    NSec = 1000;
    NMin = 60 * NSec;
  public
    function CreateRzRegOption(const APath: String; AOwner: TComponent = nil): TRzRegIniFile;
    procedure EraseRegPath;

    property Reg: TRzRegIniFile read FReg;
  end;

implementation

uses
  Variants, Registry
  ;

{ TRegOption }

procedure TRegOption.Add(const ASection: String; const ANameDefault: TArray<TVariantArray>);
begin
  FIdx.Add(ASection, ANameDefault);
end;

function TRegOption.CreateRzRegOption(const APath: String; AOwner: TComponent = nil): TRzRegIniFile;
begin
  if not Assigned(AOwner) then
    AOwner := Self;
  Result := TRzRegIniFile.Create(AOwner);
  Result.PathType := ptRegistry;
end;

procedure TRegOption.DoCreate;
var
  LPath: String;
begin
  FIdx := TOptionIndexer.Create;
  FReg := GetRegIniFile(LPath);
  if not LPath.IsEmpty then
    FReg.Path := LPath;

  inherited;
end;

procedure TRegOption.DoDestroy;
begin
  inherited;

  FreeAndNil(FIdx);
end;

procedure TRegOption.EraseRegPath;
var
  LReg: TRegistry;
begin
  LReg := TRegistry.Create;
  try
    LReg.DeleteKey(FReg.Path);
  finally
    FreeAndNil(LReg);
  end;
end;

function TRegOption.GetInt64(const Index: Integer): Int64;
begin
  Result := GetString(Index).ToInt64;
end;

function TRegOption.GetInteger(const Index: Integer): Integer;
begin
  Result := Reg.ReadInteger(FIdx.Section[Index], FIdx.Name[Index], FIdx.Default[Index]);
end;

function TRegOption.GetIntegerArray(const Index: Integer): TArray<Integer>;
var
  LBuf: TArray<string>;
  LRet, i: Integer;
begin
  Result := [];
  LBuf := GetStringArray(Index);
  if Length(LBuf) = 0 then
    Exit;

  for i := 0 to Length(LBuf) -1 do
    if TryStrToInt(LBuf[i], LRet) then
      Result := Result + [LRet];
end;

function TRegOption.GetString(const Index: Integer): String;
begin
  Result := Reg.ReadString(FIdx.Section[Index], FIdx.Name[Index], FIdx.Default[Index]);
end;

function TRegOption.GetStringArray(const Index: Integer): TArray<string>;
var
  LVarArray: Variant;
  LCnt, lo, hi, dim: Integer;
  LBuf: string;
begin
  Result := [];
  if Reg.ValueExists(FIdx.Section[Index], FIdx.Name[Index]) then
  begin
    LBuf := Reg.ReadString(FIdx.Section[Index], FIdx.Name[Index], '');
    if not LBuf.IsEmpty then
      Result := LBuf.Split([','])
  end
  else
  begin
    LVarArray := FIdx.Default[Index];
    dim := VarArrayDimCount(LVarArray);
    lo := VarArrayLowBound(LVarArray, dim);
    hi := VarArrayHighBound(LVarArray, dim);
    LCnt := hi - lo +1;
    if LCnt > 0 then
    begin
      SetLength(Result, LCnt);
      while lo < LCnt do
      begin
        Result[lo] := LVarArray[lo];
        Inc(lo);
      end;
    end;
  end;
end;

function TRegOption.GetUInt64(const Index: Integer): UInt64;
begin
  Result := StrToUInt64(GetString(Index));
end;

procedure TRegOption.SetInt64(const Index: Integer; const Value: Int64);
begin
  SetString(Index, Value.ToString);
end;

procedure TRegOption.SetInteger(const Index, Value: Integer);
begin
  Reg.WriteInteger(FIdx.Section[Index], FIdx.Name[Index], Value)
end;

procedure TRegOption.SetIntegerArray(const Index: Integer; const AValues: TArray<Integer>);
var
  LBuf: TStringList;
  i: Integer;
begin
  LBuf := TStringList.Create;
  try
    LBuf.Delimiter := ',';
    LBuf.QuoteChar := #0;
    LBuf.StrictDelimiter := True;
    for i in AValues do
      LBuf.Add(i.ToString);
    SetString(Index, LBuf.DelimitedText);
  finally
    FreeAndNil(LBuf);
  end;
end;

procedure TRegOption.SetString(const Index: Integer; const Value: String);
begin
  Reg.WriteString(FIdx.Section[Index], FIdx.Name[Index], Value)
end;

procedure TRegOption.SetStringArray(const Index: Integer; const AValues: TArray<string>);
var
  LBuf: TStringList;
begin
  LBuf := TStringList.Create;
  try
    LBuf.Delimiter := ',';
    LBuf.QuoteChar := #0;
    LBuf.StrictDelimiter := True;
    LBuf.AddStrings(AValues);
    SetString(Index, LBuf.DelimitedText);
  finally
    FreeAndNil(LBuf);
  end;
end;

procedure TRegOption.SetUInt64(const Index: Integer; const Value: UInt64);
begin
  SetString(Index, Value.ToString);
end;

function TRegOption.GetFloat(const Index: Integer): Double;
begin
  Result := Reg.ReadFloat(FIdx.Section[Index], FIdx.Name[Index], FIdx.Default[Index].ToDouble)
end;

procedure TRegOption.SetFloat(const Index: Integer; const Value: Double);
begin
  Reg.WriteFloat(FIdx.Section[Index], FIdx.Name[Index], Value)
end;

function TRegOption.GetBool(const Index: Integer): Boolean;
begin
  Result := Reg.ReadBool(FIdx.Section[Index], FIdx.Name[Index], FIdx.Default[Index])
end;

procedure TRegOption.SetBool(const Index: Integer; const Value: Boolean);
begin
  Reg.WriteBool(FIdx.Section[Index], FIdx.Name[Index], Value)
end;

function TRegOption.mmSecFromSec(const Index: Integer): Integer;
begin
  Result := GetInteger(Index) div NSec;
end;

function TRegOption.mmSecFromMin(const Index: Integer): Integer;
begin
  Result := GetInteger(Index) div NMin;
end;


procedure TRegOption.SecToMmSec(const Index, Value: Integer);
begin
  SetInteger(Index, Value * NSec);
end;

procedure TRegOption.MinToMmSec(const Index, Value: Integer);
begin
  SetInteger(Index, Value * NMin);
end;

end.
