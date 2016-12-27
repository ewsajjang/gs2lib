unit mConsts;

interface

const
	FALSE_STR = 'False';
  TRUE_STR = 'True';
  BoolStr: array[False..True] of string = (FALSE_STR, TRUE_STR);
  VAL_NOT_ASSIGNED = -1 deprecated 'use NNotAssigned';
  VAL_NOT_ASSIGNED_STR = 'not assigned' deprecated 'use SNotAssigned';

  NNotAssigned = -1;
  SNotAssigned = 'not assigned';

  NSec = 1000;
  NMin = NSec * 60;
  NHour = NMin * 60;

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
