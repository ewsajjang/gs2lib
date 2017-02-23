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
    FIdx: TOptionIndexer;
  protected
    procedure DoCreate; override;
    procedure DoDestroy; override;

    function GetRegIniFile(var APath: String): TRzRegIniFile; virtual; abstract;

    function GetString(const Index: Integer): String;
    procedure SetString(const Index: Integer; const Value: String);
    function GetInteger(const Index: Integer): Integer;
    procedure SetInteger(const Index, Value: Integer);
    function GetFloat(const Index: Integer): Double;
    procedure SetFloat(const Index: Integer; const Value: Double);
    function GetBool(const Index: Integer): Boolean;
    procedure SetBool(const Index: Integer; const Value: Boolean);

    function mmSecFromSec(const Index: Integer): Integer;
    function mmSecFromMin(const Index: Integer): Integer;
    procedure SecToMmSec(const Index, Value: Integer);
    procedure MinToMmSec(const Index, Value: Integer);

    procedure Add(const ASection: String; const ANameDefault: TArray<TArray<String>>); virtual;
  public const
    NSec = 1000;
    NMin = 60 * NSec;
  public
    property Reg: TRzRegIniFile read FReg;
  end;

implementation

{ TRegOption }

procedure TRegOption.Add(const ASection: String; const ANameDefault: TArray<TArray<String>>);
begin
  FIdx.Add(ASection, ANameDefault);
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

function TRegOption.GetInteger(const Index: Integer): Integer;
begin
  Result := GetString(Index).ToInteger;
end;

function TRegOption.GetString(const Index: Integer): String;
begin
  Result := Reg.ReadString(FIdx.Section[Index], FIdx.Name[Index], FIdx.Default[Index]);
end;

procedure TRegOption.SetInteger(const Index, Value: Integer);
begin
  SetString(Index, Value.ToString);
end;

procedure TRegOption.SetString(const Index: Integer; const Value: String);
begin
  Reg.WriteString(FIdx.Section[Index], FIdx.Name[Index], Value)
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
  Result := Reg.ReadBool(FIdx.Section[Index], FIdx.Name[Index], FIdx.Default[Index].ToBoolean)
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
