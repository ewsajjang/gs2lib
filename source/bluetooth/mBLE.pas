unit mBLE;

interface

uses
  System.Bluetooth, System.BluetoothConsts, System.Bluetooth.Components,

  System.Classes, System.SysUtils,
  System.Generics.Collections
  ;

type
  // Exceptions
  //  Characteristic Exception
  ENotImplementsMandatoryCharacteristic = class(EBluetoothLECharacteristicException);
  ENotMatchedCharacteristicUUID = class(EBluetoothLECharacteristicException);

  //  Service Exception
  ENotImplementsMandatoryService = class(EBluetoothLEServiceException);
  ENotMatchedServiceUUID = class(EBluetoothLEServiceException);

  //  Descriptor Exception
  ENotImplementsMandatoryDescriptor = class(EBluetoothLEServiceException);

  //  Profile Exception
  EProfileException = class(EBluetoothException);



implementation

end.
