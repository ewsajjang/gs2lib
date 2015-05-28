unit mBluetoothHelper;

interface

uses
  System.Bluetooth,
  System.Classes, System.SysUtils
  ;

type
  TBluetoothGattCharacteristicListHelper = class helper for TBluetoothGattCharacteristicList
    function Extract(const AUUID: TBluetoothUUID; var ACharacteristic: TBluetoothGattCharacteristic): Boolean;
  end;

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
    class function Create(const AValue: String): TBluetoothProperty; static;
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

  TScanResponseKeyHelper = record helper for TScanResponseKey
    function Str: String;
  end;

implementation

uses
  System.TypInfo
  ;

{ TBluetoothGattCharacteristicListHelper }

function TBluetoothGattCharacteristicListHelper.Extract(
  const AUUID: TBluetoothUUID;
  var ACharacteristic: TBluetoothGattCharacteristic): Boolean;
var
  LItem: TBluetoothGattCharacteristic;
begin
  Result := False;
  ACharacteristic := nil;
  for LItem in Self do
  begin
    Result := LItem.UUID = AUUID;
    if Result then
    begin
      ACharacteristic := LItem;
      Break;
    end;
  end;
end;

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

class function TBluetoothPropertyHelper.Create(
  const AValue: String): TBluetoothProperty;
var
  LProperty: TBluetoothProperty;
  LFind: Boolean;
begin
  LFind := False;
  Result := TBluetoothProperty.Broadcast;
  for LProperty := Low(TBluetoothProperty) to High(TBluetoothProperty) do
    if LProperty.Str.Equals(AVAlue) then
    begin
      Result := LProperty;
      LFind := True;
      Break;
    end;
  if not LFind then
    raise Exception.Create('Can not find matched TBluetoothProperty values');
end;

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

{ TScanResponseKeyHelper }

function TScanResponseKeyHelper.Str: String;
begin
  case Self of
    TScanResponseKey.Flags                            : Result := 'Flags';
    TScanResponseKey.IncompleteList16SCUUID           : Result := 'IncompleteList16SCUUID';
    TScanResponseKey.CompleteList16SCUUID             : Result := 'CompleteList16SCUUID';
    TScanResponseKey.IncompleteList32SCUUID           : Result := 'IncompleteList32SCUUID';
    TScanResponseKey.CompleteList32SCUUID             : Result := 'CompleteList32SCUUID';
    TScanResponseKey.IncompleteList128SCUUID          : Result := 'IncompleteList128SCUUID';
    TScanResponseKey.CompleteList128SCUUID            : Result := 'CompleteList128SCUUID';
    TScanResponseKey.ShortenedLocalName               : Result := 'ShortenedLocalName';
    TScanResponseKey.CompleteLocalName                : Result := 'CompleteLocalName';
    TScanResponseKey.TxPowerLevel                     : Result := 'TxPowerLevel';
    TScanResponseKey.ClassOfDevice                    : Result := 'ClassOfDevice';
    TScanResponseKey.SimplePairingHashC               : Result := 'SimplePairingHashC';
    TScanResponseKey.SimplePairingRAndomizerR         : Result := 'SimplePairingRAndomizerR';
    TScanResponseKey.DeviceID                         : Result := 'DeviceID';
    TScanResponseKey.SecurityManagerOutOfBandFlags    : Result := 'SecurityManagerOutOfBandFlags';
    TScanResponseKey.SlaveConnectionIntervalRange     : Result := 'SlaveConnectionIntervalRange';
    TScanResponseKey.List16bServiceSolicitationUUIDs  : Result := 'List16bServiceSolicitationUUIDs';
    TScanResponseKey.List32bServiceSolicitationUUIDs  : Result := 'List32bServiceSolicitationUUIDs';
    TScanResponseKey.List128bServiceSolicitationUUIDs : Result := 'List128bServiceSolicitationUUIDs';
    //TScanResponseKey.ServiceData                      ,
    TScanResponseKey.ServiceData16b                   : Result := 'ServiceData16b';
    TScanResponseKey.ServiceData32b                   : Result := 'ServiceData32b';
    TScanResponseKey.ServiceData128b                  : Result := 'ServiceData128b';
    TScanResponseKey.PublicTargetAddress              : Result := 'PublicTargetAddress';
    TScanResponseKey.RandomTargetAddress              : Result := 'RandomTargetAddress';
    TScanResponseKey.Appearance                       : Result := 'Appearance';
    TScanResponseKey.AdvertisingInterval              : Result := 'AdvertisingInterval';
    TScanResponseKey.LEBluetoothDeviceAddress         : Result := 'LEBluetoothDeviceAddress';
    TScanResponseKey.LERole                           : Result := 'LERole';
    TScanResponseKey.SimplePairingHashc256            : Result := 'SimplePairingHashc256';
    TScanResponseKey.SimplePairingRAndomizerR256      : Result := 'SimplePairingRAndomizerR256';
    TScanResponseKey._3DInformationData               : Result := '_3DInformationData';
    TScanResponseKey.ManufacturerSpecificData         : Result := 'ManufacturerSpecificData';
  end;
end;

end.
