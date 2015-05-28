unit mComboBoxHelper;

interface

uses
  FMX.ListBox,

  System.Classes, System.SysUtils
  ;

type
  TComboBoxHelper = class Helper for TComboBox
    function Add(const AValue: String): Integer;
    function AddFmt(const AValue: String; const Args: array of const): Integer;

    function ItemCount: Integer;
    function ItemSelected: Boolean;
    function ItemText: String;
  end;

implementation

uses
  mConsts
  ;

{ TComboBoxHelper }

function TComboBoxHelper.Add(const AValue: String): Integer;
begin
  Result := Items.Add(AValue)
end;

function TComboBoxHelper.AddFmt(const AValue: String;
  const Args: array of const): Integer;
begin
  Result := Items.Add(Format(AValue, Args));
end;

function TComboBoxHelper.ItemCount: Integer;
begin
  Result := Items.Count;
end;

function TComboBoxHelper.ItemSelected: Boolean;
begin
  Result := ItemIndex > VAL_NOT_ASSIGNED
end;

function TComboBoxHelper.ItemText: String;
begin
  if ItemSelected then
    Result := Selected.Text
  else
    Result := EmptyStr;
end;

end.
