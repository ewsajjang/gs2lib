unit mStringBuilderHelper;

interface

uses
  System.SysUtils, System.Classes
  ;

type
  TStringBuilderHelper = class helper for TStringBuilder
    function Append(const ATrue: Boolean; const Value: string): TStringBuilder; overload;
    function AppendFormat(const ATrue: Boolean; const Format: string; const Args: array of const): TStringBuilder; overload;
  end;

implementation

{ TStringBuilderHelper }

function TStringBuilderHelper.Append(const ATrue: Boolean;
  const Value: string): TStringBuilder;
begin
  if not ATrue then
    Result := Self
  else
    Result := Append(Value);
end;

function TStringBuilderHelper.AppendFormat(const ATrue: Boolean;
  const Format: string; const Args: array of const): TStringBuilder;
begin
  if not True then
    Result := Self
  else
    Result := AppendFormat(Format, Args);
end;

end.
