unit mValueListEditorHelper;

interface

uses
  Vcl.Grids, Vcl.ValEdit,
  System.Classes, System.SysUtils
  ;

type
  TValueListEditorHelper = class helper for TValueListEditor
    function CalcRow(const ARow: Integer): Integer;
  end;

implementation

uses
  System.Math
  ;

{ TValueListEditorHelper }

function TValueListEditorHelper.CalcRow(const ARow: Integer): Integer;
begin
  Result := ARow - IfThen(doColumnTitles in DisplayOptions, 1)
end;

end.
