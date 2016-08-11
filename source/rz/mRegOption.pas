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

    procedure Add(const ASection: String; const ANameDefault: TArray<TArray<String>>); virtual;

    function GetString(const Index: Integer): String;
    procedure SetString(const Index: Integer; const Value: String);
    function GetInteger(const Index: Integer): Integer;
    procedure SetInteger(const Index, Value: Integer);
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
  SetString(Index, Value.ToString)
end;

procedure TRegOption.SetString(const Index: Integer; const Value: String);
begin
  Reg.WriteString(FIdx.Section[Index], FIdx.Name[Index], Value)
end;

end.
