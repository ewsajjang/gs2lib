unit mIniFileHelper;

interface

uses
  System.Classes, System.SysUtils, System.IniFiles;

type
  TIniFileHelper = class helper for TIniFile
  private
    const DEFAULT_FIEL_EXT = '.ini';
  private
    class function GetFileName: String; static;

    function GetS(Section, Ident: String): String;
    procedure SetS(Section, Ident: String; const Value: String);
    function GetI(Section, Ident: String): Integer;
    procedure SetI(Section, Ident: String; const Value: Integer);
    procedure SetB(Section, Ident: String; const Value: Boolean);
  public
    class function FileExist: Boolean;
    class procedure CreateFile;

    class property FileName: String read GetFileName;

    property S[Section: String; Ident: String]: String read GetS write SetS;
    property I[Section: String; Ident: String]: Integer read GetI write SetI;
    property B[Section: String; Ident: String]: Boolean write SetB;
  end;

implementation

uses
  System.IOUtils;

var
  FFileName: String = '';

{ TIniFileHelper }

class procedure TIniFileHelper.CreateFile;
begin
  TFile.Create(FileName).Free;
end;

class function TIniFileHelper.FileExist: Boolean;
begin
  Result := TFile.Exists(FileName);
end;

class function TIniFileHelper.GetFileName: String;
begin
  if FFileName.IsEmpty then
    FFileName := TPath.ChangeExtension(ParamStr(0), DEFAULT_FIEL_EXT);

  Result := FFileName;
end;

function TIniFileHelper.GetI(Section, Ident: String): Integer;
begin
  Result := ReadInteger(Section, Ident, 0);
end;

function TIniFileHelper.GetS(Section, Ident: String): String;
begin
  Result := ReadString(Section, Ident, EmptyStr)
end;

procedure TIniFileHelper.SetB(Section, Ident: String; const Value: Boolean);
begin
  WriteBool(Section, Ident, Value);
end;

procedure TIniFileHelper.SetI(Section, Ident: String; const Value: Integer);
begin
  WriteInteger(Section, Ident, Value);
end;

procedure TIniFileHelper.SetS(Section, Ident: String; const Value: String);
begin
  WriteString(Section, Ident, Value);
end;

initialization
  FFileName := EmptyStr;

finalization

end.
