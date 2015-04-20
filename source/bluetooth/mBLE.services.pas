unit mBLE.services;

interface

uses
  System.SysUtils, System.Classes;

type
  TbleService = class
    class function GetServiceName(const AServiceUUID: TGUID): string; static;
  end;

implementation

uses
  System.Bluetooth
  ;


{ TbleService }

class function TbleService.GetServiceName(const AServiceUUID: TGUID): string;
type
  TServiceNames = array [0..2] of TBluetoothService;
const
  ServiceNames: TServiceNames = (
    (Name: 'Glucose Service'; UUID:'{00001808-0000-1000-8000-00805F9B34FB}'),
    (Name: 'Current Time Service'; UUID:'{00001805-0000-1000-8000-00805F9B34FB}'),
    (Name: 'The Apple Notification Center Service'; UUID:'{7905F431-B5CE-4E99-A40F-4B1E122D00D0}')
  );
var
  i: Integer;
begin
  Result := 'Not Assigend';
  for I := Low(ServiceNames) to High(ServiceNames) do
    if ServiceNames[I].UUID = AServiceUUID then
      Exit(ServiceNames[I].Name);
end;

end.
