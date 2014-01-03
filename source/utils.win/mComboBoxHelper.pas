unit mComboBoxHelper;

interface

uses
  System.SysUtils,
  Vcl.StdCtrls;

type
  TComboBoxHelper = class helper for TComboBox
    procedure IndexBy(const AValue: String);
  end;

implementation

{ TComboBoxHelper }

procedure TComboBoxHelper.IndexBy(const AValue: String);
var
  LSuccess: Boolean;
  i: Integer;
begin
  LSuccess := False;
  for i := 0 to Items.Count - 1 do
  begin
    LSuccess := Items[i].Equals(AValue);
    if LSuccess then
    begin
      ItemIndex := i;
      Break;
    end;
  end;

  if not LSuccess then
    ItemIndex := -1;
end;

end.
