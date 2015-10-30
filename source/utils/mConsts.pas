unit mConsts;

interface

const
	FALSE_STR = 'False';
  TRUE_STR = 'True';
  BoolStr: array[False..True] of string = (FALSE_STR, TRUE_STR);
  VAL_NOT_ASSIGNED = -1;
  VAL_NOT_ASSIGNED_STR = 'not assigned';

function StrToBool(const AValue: String): Boolean;

implementation

function StrToBool(const AValue: String): Boolean;
begin
  if AValue = FALSE_STR then
    Result := False
  else
    Result := True;
end;

end.
