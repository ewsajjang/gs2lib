unit mActnListHelper;

interface

uses
  System.Classes, System.SysUtils, Vcl.ActnList
  ;

type
  TActionHelper = class helper for TAction
    function ReaderableString: String;
  end;

implementation

uses
  mConsts
  ;

{ TActionHelper }

function TActionHelper.ReaderableString: String;
var
  LPos: Integer;
begin
  LPos := Caption.IndexOf('(&');
  if LPos > VAL_NOT_ASSIGNED then
    Result := Caption.Substring(0, LPos)
  else
    Result := Caption;
end;

end.
