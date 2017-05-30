unit mPreferences;

interface

uses
  System.Classes, System.SysUtils, System.IniFiles, System.Rtti, RzCommon,
  System.Generics.Collections
  ;

const
  NIdxIdent = 0;
  NIdxDefualt = 1;

type
  TIdentAndDefualt = array[NIdxIdent..NIdxDefualt] of String;

  ECustomPreferences = class(Exception);
    EInitFailed = class(ECustomPreferences);

  TStringArray = TArray<String>;
  IPreferences = interface
    ['{019E078F-BC8E-459F-9E8B-8A93E4EED77A}']

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

    procedure Add(const ASection: String; const ANameDefault: TArray<TStringArray>);
  end;

  TCustomPreferences = class
  protected
    FIni: TIniFile;
    FSections: TArray<String>;
    FIdentAndDefualt: TArray<TIdentAndDefualt>;
    procedure DoCreateIniFile(AIniFile: TIniFile = nil); virtual;
    procedure DoInitSecIdentDefualt; virtual; abstract;

    function Section(const AIndex: Integer): String;
    function Ident(const AIndex: Integer): String;
    function Defualt(const AIndex: Integer): String;
  protected
    function GetInteger(const Index: Integer): Integer;
    function GetString(const Index: Integer): String;
    function GetBool(const Index: Integer): Boolean;
    function GetFloat(const Index: Integer): Double;
    function GetCardinal(const Index: Integer): Cardinal;

    procedure SetInteger(const Index, Value: Integer);
    procedure SetString(const Index: Integer; const Value: String);
    procedure SetBool(const Index: Integer; const Value: Boolean);
    procedure SetFloat(const Index: Integer; const Value: Double);
    procedure SetCardinal(const Index: Integer; const Value: Cardinal);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TOptionIndexer = class
  strict private
    const
      NName= 0;
      NDefualt = 1;
    type
      TNameDefault = TArray<String>;
    var FSections: TArray<String>;
    var FSecNameDefaults: TDictionary<String, TArray<TNameDefault>>;
    function GetDefault(AIdx: Integer): String;
    function GetName(AIdx: Integer): String;
    function GetSection(AIdx: Integer): String;
  private
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const ASection: String; const ANameDefault: TArray<TNameDefault>);
    procedure Clear;

    property Section[AIdx: Integer]: String read GetSection;
    property Name[AIdx: Integer]: String read GetName;
    property Default[AIdx: Integer]: String read GetDefault;
  end;

  TPreferences = class(TInterfacedObject, IPreferences)
  protected
    FSrc: TRzRegIniFile;
    FIdx: TOptionIndexer;
    function GetString(const Index: Integer): String;
    procedure SetString(const Index: Integer; const Value: String);
    function GetBoolStr(const Index: Integer): Boolean;
    procedure SetBoolStr(const Index: Integer; const Value: Boolean);
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
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure ClearIdx; virtual;
    procedure Add(const ASection: String; const ANameDefault: TArray<TStringArray>); virtual;

    function LoadFromIni(const APath: String; const ACreateNotExists: Boolean = False): Boolean;
    function LoadFromReg(const APath: String; const AKey: TRzRegKey = hkeyCurrentUser; const AAccess: TRzRegAccess = [keyAllAccess]): Boolean;
  end;

implementation

uses
  mIniFileHelper, System.IOUtils, mConsts
  ;

const
  NBitCntOfByte = 8;

function IdxL(const Index: Integer): Integer;
begin
  Result := (Index shr NBitCntOfByte) and $FF;
end;

function IdxR(const Index: Integer): Integer;
begin
  Result := Index and $FF;
end;

constructor TCustomPreferences.Create;
begin
  DoCreateIniFile;
  if not Assigned(FIni) then
  begin
    if not TIniFile.FileExist then
      TIniFile.CreateFile;
    FIni := TIniFile.Create(TIniFile.FileName);
  end;

  DoInitSecIdentDefualt;
  if Length(FSections) = 0 then
    raise ECustomPreferences.Create('Array of Section does not assigned');

  if Length(FIdentAndDefualt) = 0 then
    raise ECustomPreferences.Create('Array of Ident does not assigned');
end;

function TCustomPreferences.Defualt(const AIndex: Integer): String;
begin
  Result := FIdentAndDefualt[IdxR(AIndex)][1];
end;

destructor TCustomPreferences.Destroy;
begin
  FreeAndNil(FIni);

  inherited;
end;

procedure TCustomPreferences.DoCreateIniFile(AIniFile: TIniFile);
begin
end;

function TCustomPreferences.GetBool(const Index: Integer): Boolean;
begin
  Result := FIni.ReadBool(Section(Index), Ident(Index), Defualt(Index).ToBoolean);
end;

function TCustomPreferences.GetCardinal(const Index: Integer): Cardinal;
begin
  Result := GetInteger(Index);
end;

function TCustomPreferences.GetFloat(const Index: Integer): Double;
begin
  Result := FIni.ReadFloat(Section(Index), Ident(Index), Defualt(Index).ToDouble);
end;

function TCustomPreferences.GetInteger(const Index: Integer): Integer;
begin
  Result := FIni.ReadInteger(Section(Index), Ident(Index), Defualt(Index).ToInteger);
end;

function TCustomPreferences.GetString(const Index: Integer): String;
begin
  Result := FIni.ReadString(Section(Index), Ident(Index), Defualt(Index));
end;

function TCustomPreferences.Ident(const AIndex: Integer): String;
begin
  Result := FIdentAndDefualt[IdxR(AIndex)][0];
end;

function TCustomPreferences.Section(const AIndex: Integer): String;
begin
  Result := FSections[IdxL(AIndex)];
end;

procedure TCustomPreferences.SetBool(const Index: Integer;
  const Value: Boolean);
begin
  FIni.WriteBool(Section(Index), Ident(Index), Value);
end;

procedure TCustomPreferences.SetCardinal(const Index: Integer;
  const Value: Cardinal);
begin
  SetInteger(Index, Value);
end;

procedure TCustomPreferences.SetFloat(const Index: Integer;
  const Value: Double);
begin
  FIni.WriteFloat(Section(Index), Ident(Index), Value);
end;

procedure TCustomPreferences.SetInteger(const Index, Value: Integer);
begin
  FIni.WriteInteger(Section(Index), Ident(Index), Value);
end;

procedure TCustomPreferences.SetString(const Index: Integer; const Value: String);
begin
  FIni.WriteString(Section(Index), Ident(Index), Value);
end;

{ TOptionIndexer }

procedure TOptionIndexer.Add(const ASection: String;
  const ANameDefault: TArray<TNameDefault>);
begin
  Assert(not FSecNameDefaults.ContainsKey(ASection), Format('The %s is alreay exists', [ASection]));

  FSections := FSections + [ASection];
  FSecNameDefaults.Add(ASection, ANameDefault);
end;

destructor TOptionIndexer.Destroy;
begin
  if Assigned(FSecNameDefaults) then
    FreeAndNil(FSecNameDefaults);
end;

function TOptionIndexer.GetDefault(AIdx: Integer): String;
begin
  Result := FSecNameDefaults[Section[AIdx]][IdxR(AIdx)][NIdxDefualt]
end;

function TOptionIndexer.GetName(AIdx: Integer): String;
begin
  Result := FSecNameDefaults[Section[AIdx]][IdxR(AIdx)][NIdxIdent]
end;

function TOptionIndexer.GetSection(AIdx: Integer): String;
begin
  Result := FSections[IdxL(AIdx)]
end;

procedure TOptionIndexer.Clear;
begin
  FSecNameDefaults.Clear;
  FSections := [];
end;

constructor TOptionIndexer.Create;
begin
  FSecNameDefaults := TDictionary<String, TArray<TNameDefault>>.Create;
end;

{ TPreferences }

procedure TPreferences.Add(const ASection: String; const ANameDefault: TArray<TStringArray>);
begin
  FIdx.Add(ASection, ANameDefault);
end;

procedure TPreferences.ClearIdx;
begin
  FIdx.Clear;
end;

constructor TPreferences.Create;
begin
  FIdx := TOptionIndexer.Create;
  FSrc := TRzRegIniFile.Create(nil);
end;

destructor TPreferences.Destroy;
begin
  FreeAndNil(FSrc);
  FreeAndNil(FIdx);

  inherited;
end;

function TPreferences.GetBool(const Index: Integer): Boolean;
begin
  Result := FSrc.ReadBool(FIdx.Section[Index], FIdx.Name[Index], FIdx.Default[Index].ToBoolean)
end;

function TPreferences.GetFloat(const Index: Integer): Double;
begin
  Result := FSrc.ReadFloat(FIdx.Section[Index], FIdx.Name[Index], FIdx.Default[Index].ToDouble)
end;

function TPreferences.GetInt64(const Index: Integer): Int64;
begin
  Result := GetString(Index).ToInt64;
end;

function TPreferences.GetInteger(const Index: Integer): Integer;
begin
  Result := GetString(Index).ToInteger;
end;

function TPreferences.GetBoolStr(const Index: Integer): Boolean;
begin
  Result := GetString(Index) = BoolStr[True];
end;

function TPreferences.GetString(const Index: Integer): String;
begin
  Result := FSrc.ReadString(FIdx.Section[Index], FIdx.Name[Index], FIdx.Default[Index]);
end;

function TPreferences.GetUInt64(const Index: Integer): UInt64;
begin
  Result := StrToUInt64(GetString(Index));
end;

function TPreferences.LoadFromIni(const APath: String; const ACreateNotExists: Boolean): Boolean;
begin
  if not TFile.Exists(APath) then
    if ACreateNotExists then
      TFile.Create(APath);

  Result := TFile.Exists(APath);
  if Result then
  begin
    FSrc.PathType := ptIniFile;
    FSrc.Path := APath;
  end;
end;

function TPreferences.LoadFromReg(const APath: String; const AKey: TRzRegKey; const AAccess: TRzRegAccess): Boolean;
begin
  FSrc.PathType := ptRegistry;
  FSrc.RegAccess := AAccess;
  FSrc.RegKey := AKey;
  FSrc.Path := APath;
  Result := True;
end;

procedure TPreferences.SetBool(const Index: Integer; const Value: Boolean);
begin
  FSrc.WriteBool(FIdx.Section[Index], FIdx.Name[Index], Value)
end;

procedure TPreferences.SetFloat(const Index: Integer; const Value: Double);
begin
  FSrc.WriteFloat(FIdx.Section[Index], FIdx.Name[Index], Value)
end;

procedure TPreferences.SetInt64(const Index: Integer; const Value: Int64);
begin
  SetString(Index, Value.ToString);
end;

procedure TPreferences.SetInteger(const Index, Value: Integer);
begin
  SetString(Index, Value.ToString);
end;

procedure TPreferences.SetBoolStr(const Index: Integer; const Value: Boolean);
begin
  SetString(Index, BoolStr[Value]);
end;

procedure TPreferences.SetString(const Index: Integer; const Value: String);
begin
  FSrc.WriteString(FIdx.Section[Index], FIdx.Name[Index], Value)
end;

procedure TPreferences.SetUInt64(const Index: Integer; const Value: UInt64);
begin
  SetString(Index, Value.ToString);
end;

end.
