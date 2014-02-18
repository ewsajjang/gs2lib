unit mParams;

interface

type
  TParams = class
  private class var
    class function GetItems(Index: Integer): String; static;
    class function GetCount: Integer; static;
  public
    class function ExePath: String;
    class function ExeName: String;

    class property Items[Index: Integer]: String read GetItems;
    class property Count: Integer read GetCount;
  end;

implementation

uses
  System.Classes, System.SysUtils, System.IOUtils;

{ TParamInfo }

class function TParams.ExeName: String;
begin
  Result := ExtractFileName(ParamStr(0));
end;

class function TParams.ExePath: String;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

class function TParams.GetCount: Integer;
begin
  Result := ParamCount;
end;

class function TParams.GetItems(Index: Integer): String;
begin
  Result := ParamStr(Index);
end;

end.
