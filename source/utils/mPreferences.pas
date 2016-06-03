unit mPreferences;

interface

uses
  System.Classes, System.SysUtils, System.IniFiles
  ;

const
  NIdxIdent = 0;
  NIdxDefualt = 1;

type
  TIdentAndDefualt = array[NIdxIdent..NIdxDefualt] of String;

  ECustomPreferences = class(Exception);
    EInitFailed = class(ECustomPreferences);
  TCustomPreferences = class
  protected
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

implementation

uses
  mIniFileHelper
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


end.
