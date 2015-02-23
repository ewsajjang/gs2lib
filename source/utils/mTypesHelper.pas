unit mTypesHelper;

interface

uses
  System.SysUtils, System.Classes, System.TypInfo, System.Types
  ;

type
  TRectHelper = record helper for TRect
    function ToString: String;
  end;

implementation

{ TRectHelper }

function TRectHelper.ToString: String;
begin
  Result := Format('Top:%d,Left:%d,Bottom:%d,Right:%d', [Left, Top, Bottom, Right]);
end;

end.
