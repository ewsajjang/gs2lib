unit mBLE.services.GLS;

interface

uses
  mBLE.characteristics,

  System.Bluetooth, System.BluetoothConsts,

  System.Classes, System.SysUtils,
  System.Generics.Collections
  ;

type
  TGlucoseService = class(TBluetoothGattService)

  end;

implementation

end.
