unit mForms;

interface

uses
  Forms
  ;

function IfThen(const ACondition: Boolean; const ATrue, AFalse: TBorderStyle): TBorderStyle; overload;

implementation

function IfThen(const ACondition: Boolean; const ATrue, AFalse: TBorderStyle): TBorderStyle;
begin
  if ACondition then
    Exit(ATrue)
  else
    Exit(AFalse);
end;

end.
