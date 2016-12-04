unit mListBoxHelper;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.StdCtrls
  ;

type
  TListBoxHelper = class helper for TListBox
    function ItemSelected: Boolean;
    function ItemText: String;
  end;

implementation

uses
  mConsts;

{ TListBoxHelper }

function TListBoxHelper.ItemSelected: Boolean;
begin
  Result := ItemIndex > NNotAssigned;
end;

function TListBoxHelper.ItemText: String;
begin
  if ItemSelected then
    Result := Items[ItemIndex]
  else
    Result := EmptyStr;
end;

end.

