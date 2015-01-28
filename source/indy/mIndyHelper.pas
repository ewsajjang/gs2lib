unit mIndyHelper;

interface

uses
  IdComponent,
  System.TypInfo
  ;


type
  TWorkModeHelper = record helper for TWorkMode
    function Str: String;
  end;

implementation

{ TWorkModeHelper }

function TWorkModeHelper.Str: String;
begin
  Result := GetEnumName(TypeInfo(TWorkMode), Integer(Self));
end;
end.
