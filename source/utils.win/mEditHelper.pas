unit mEditHelper;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.StdCtrls
  ;

type
  TEditHelper = class helper for TEdit
    function Str: String;
    function StrHint: String;
    function IsEmpty: Boolean;
  end;

implementation

{ TEditHelper }

function TEditHelper.StrHint: String;
begin
  Result := TextHint;
end;

function TEditHelper.IsEmpty: Boolean;
begin
  Result := Str.IsEmpty;
end;

function TEditHelper.Str: String;
begin
  Result := Text;
end;

end.
