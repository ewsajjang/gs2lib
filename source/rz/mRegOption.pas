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

    function GetRegIniFile: TRzRegIniFile; virtual; abstract;

    function GetString(const Index: Integer): String;
    procedure SetString(const Index: Integer; const Value: String);
    function GetInteger(const Index: Integer): Integer;
    procedure SetInteger(const Index, Value: Integer);
    function GetFloat(const Index: Integer): Double;
    procedure SetFloat(const Index: Integer; const Value: Double);
    function GetBool(const Index: Integer): Boolean;
    procedure SetBool(const Index: Integer; const Value: Boolean);

    procedure Add(const ASection: String; const ANameDefault: TArray<TArray<String>>); virtual;
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
begin
  FIdx := TOptionIndexer.Create;
  FReg := GetRegIniFile;

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

end.
