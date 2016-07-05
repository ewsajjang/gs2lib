unit mRadioGroupHelper;

interface

uses
  System.Classes, System.SysUtils, Vcl.ExtCtrls
  ;

type
  TRadioGroupHelper = class helper for TRadioGroup
  private
    function GetItemSelected: Boolean;
    function GetItemText: String;
  public
    procedure ItemIndexBy(const AValue: String; const AFireOnClick: Boolean = True);

    property ItemSelected: Boolean read GetItemSelected;
    property ItemText: String read GetItemText;
  end;

implementation

{ TRadioGroupHelper }

function TRadioGroupHelper.GetItemSelected: Boolean;
begin
  Result := ItemIndex > -1;
end;

function TRadioGroupHelper.GetItemText: String;
begin
  if ItemSelected then
    Result := Items[ItemIndex]
  else
    Result := '';
end;

procedure TRadioGroupHelper.ItemIndexBy(const AValue: String; const AFireOnClick: Boolean);
var
  LIdx: Integer;
begin
  LIdx := Items.IndexOf(AValue);
  ItemIndex := LIdx;
  if Assigned(OnClick) then
    OnClick(Self);
end;

end.
