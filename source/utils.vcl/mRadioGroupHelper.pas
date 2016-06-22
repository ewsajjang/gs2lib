unit mRadioGroupHelper;

interface

uses
  System.Classes, System.SysUtils, Vcl.ExtCtrls
  ;

type
  TRadioGroupHelper = class helper for TRadioGroup
  private
    function GetItemSelected: Boolean;
  public
    property ItemSelected: Boolean read GetItemSelected;
  end;

implementation

{ TRadioGroupHelper }

function TRadioGroupHelper.GetItemSelected: Boolean;
begin
  Result := ItemIndex > -1;
end;

end.
