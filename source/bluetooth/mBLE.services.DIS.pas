unit mBLE.services.DIS;

interface

uses
  mBLE.characteristics,

  System.Bluetooth, System.BluetoothConsts,

  System.Classes, System.SysUtils,
  System.Generics.Collections
  ;

type
  TDeviceInformationService = class(TBluetoothGattService)

  end;

implementation

end.
