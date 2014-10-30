unit mBluetooth;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo,
  System.Bluetooth, System.Bluetooth.Components;

type
  TBluetoothAdapterStateHelper = record helper for TBluetoothAdapterState
    function Str: String;
  end;

  TBluetoothScanModeHelper = record helper for TBluetoothScanMode
    function Str: String;
  end;

  TBluetoothConnectionStateHelper = record helper for TBluetoothConnectionState
    function Str: String;
  end;

  TBluetoothPropertyHelper = record helper for TBluetoothProperty
    function Str: String;
  end;

  TBluetoothPropertyFlagsHelper = record helper for TBluetoothPropertyFlags
    function Str(const ADelimiter: Char = ','): String;
  end;

  TBluetoothGattStatusHelper = record helper for TBluetoothGattStatus
    function Str: String;
  end;

  TBluetoothDescriptorKindHelper = record helper for TBluetoothDescriptorKind
    function Str: String;
  end;

implementation

{ TBluetoothAdapterStateHelper }

function TBluetoothAdapterStateHelper.Str: String;
begin
  Result := GetEnumName(TypeInfo(TBluetoothAdapterState), Integer(Self))
end;

{ TBluetoothConnectionStateHelper }

function TBluetoothConnectionStateHelper.Str: String;
begin
  Result := GetEnumName(TypeInfo(TBluetoothConnectionState), Integer(Self))
end;

{ TBluetoothPropertyHelper }

function TBluetoothPropertyHelper.Str: String;
begin
  Result := GetEnumName(TypeInfo(TBluetoothProperty), Integer(Self));
end;

{ TBluetoothPropertyFlagsHelper }

function TBluetoothPropertyFlagsHelper.Str(const ADelimiter: Char): String;
var
  LProperty: TBluetoothProperty;
begin
  Result := EmptyStr;
  for LProperty in Self do
    if Result.IsEmpty then
      Result := LProperty.Str
    else
      Result := Result + ADelimiter + LProperty.Str
end;

{ TBluetoothGattStatusHelper }

function TBluetoothGattStatusHelper.Str: String;
begin
  case Self of
    TBluetoothGattStatus.Success                   : Result := 'Success';
    TBluetoothGattStatus.ReadNotPermitted          : Result := 'ReadNotPermitted';
    TBluetoothGattStatus.WriteNotPermitted         : Result := 'WriteNotPermitted';
    TBluetoothGattStatus.InsufficientAutentication : Result := 'InsufficientAutentication';
    TBluetoothGattStatus.RequestNotSupported       : Result := 'RequestNotSupported';
    TBluetoothGattStatus.InvalidOffset             : Result := 'InvalidOffset';
    TBluetoothGattStatus.InvalidAttributeLength    : Result := 'InvalidAttributeLength';
    TBluetoothGattStatus.InsufficientEncryption    : Result := 'InsufficientEncryption';
    TBluetoothGattStatus.Failure                   : Result := 'Failure';
  end;
end;

{ TBluetoothDescriptorKindHelper }

function TBluetoothDescriptorKindHelper.Str: String;
begin
  Result := GetEnumName(TypeInfo(TBluetoothDescriptorKind), Integer(Self));
end;

{ TBluetoothScanModeHelper }

function TBluetoothScanModeHelper.Str: String;
begin
  Result := GetEnumName(TypeInfo(TBluetoothScanMode), Integer(Self));
end;

end.
