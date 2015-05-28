unit mBluetoothLEHelper;

interface

uses
  System.Bluetooth, System.Bluetooth.Components
  ;

type
  TBluetoothLEHelper = class helper for TBluetoothLE
    function ServiceByID(const ADevice: TBluetoothLEDevice;
      const AId: TBluetoothUUID;
      var AService: TBluetoothGattService): Boolean;
    function CharacteristicById(const ADevice: TBluetoothLEDevice;
      const AServiceId, ACharacteristicId: TBluetoothUUID;
      var ACharacteristic: TBluetoothGattCharacteristic): Boolean; overload;
    function CharacteristicById(const ADevice: TBluetoothLEDevice;
      const AServiceId, ACharacteristicId: TBluetoothUUID;
      var AService: TBluetoothGattService; var ACharacteristic: TBluetoothGattCharacteristic): Boolean; overload;

    function Connected: Boolean;
  end;

implementation

{ TBluetoothLEHelper }

function TBluetoothLEHelper.CharacteristicById(
  const ADevice: TBluetoothLEDevice; const AServiceId,
  ACharacteristicId: TBluetoothUUID;
  var ACharacteristic: TBluetoothGattCharacteristic): Boolean;
var
  LService: TBluetoothGattService;
begin
  if not Assigned(ADevice) then
    Exit(False);
  Result := CharacteristicById(ADevice, AServiceId, ACharacteristicId, LService, ACharacteristic)
end;

function TBluetoothLEHelper.CharacteristicById(
  const ADevice: TBluetoothLEDevice; const AServiceId,
  ACharacteristicId: TBluetoothUUID; var AService: TBluetoothGattService;
  var ACharacteristic: TBluetoothGattCharacteristic): Boolean;
begin
  if not ServiceByID(ADevice, AServiceId, AService) then
    Exit(False);

  ACharacteristic := GetCharacteristic(AService, ACharacteristicId);
  Result := Assigned(ACharacteristic);
end;

function TBluetoothLEHelper.Connected: Boolean;
begin
  Result := Self.ConnectionState = TBluetoothConnectionState.Connected
end;

function TBluetoothLEHelper.ServiceByID(const ADevice: TBluetoothLEDevice;
  const AId: TBluetoothUUID; var AService: TBluetoothGattService): Boolean;
begin
  if not Assigned(ADevice) then
    Exit(False);

  AService := GetService(ADevice, AId);
  Result := Assigned(AService);
end;

end.
