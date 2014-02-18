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
  public
    class function FileExist: Boolean;
    class procedure CreateFile;

    class property FileName: String read GetFileName;
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

initialization
  FFileName := EmptyStr;

finalization

end.
