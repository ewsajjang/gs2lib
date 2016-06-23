unit mToggleSwitchHelper;

interface

uses
  Vcl.WinXCtrls,
  System.Classes, System.SysUtils
  ;

type
  TToggleSwitchHelper = class helper for TToggleSwitch
  private
    function GetSwitchOn: Boolean;
  public
    property SwitchOn: Boolean read GetSwitchOn;
  end;

implementation

{ TToggleSwitchHelper }

function TToggleSwitchHelper.GetSwitchOn: Boolean;
begin
  Result := State = tssOn;
end;

end.
