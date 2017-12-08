unit mDevice.Windows;

interface

uses
  mDevice.Windows.Common,

  SetupApi,

  System.SysUtils, WinApi.Windows;

type
  TWinDevice = class
  private
    FDeviceInfoData: SP_DEVINFO_DATA;
    FDeviceListHandle: HDEVINFO;

    function GetBinary(PropertyCode: Integer; pData: Pointer; dwSize: DWORD): Boolean; virtual;
    function GetDWORD(PropertyCode: Integer): DWORD; virtual;
    function GetGuid(PropertyCode: Integer): TGUID; virtual;
    function GetString(PropertyCode: Integer): String; virtual;
    function GetPolicy(PropertyCode: Integer): String; virtual;
  public
    function Capabilities: String;
    function Characteristics: String;
    function ConfigFlags: String;
    function DeviceClassDescription: String; overload;
    function DeviceClassDescription(DeviceTypeGUID: TGUID): String; overload;
    function InstallState: String;
    function PowerData: String;
    function LegacyBusType: String;

    property Address: DWORD index SPDRP_ADDRESS read GetDWORD;
    property BusTypeGUID: TGUID index SPDRP_BUSTYPEGUID read GetGuid;
    property BusNumber: DWORD index SPDRP_BUSNUMBER read GetDWORD;
    property ClassGUID: TGUID index SPDRP_CLASSGUID read GetGuid;
    property CompatibleIDS: String index SPDRP_COMPATIBLEIDS read GetString;

    property DeviceClassName: String index SPDRP_CLASS read GetString;
    property DriverName: String index SPDRP_DRIVER read GetString;
    property Description: String index SPDRP_DEVICEDESC read GetString;
    property DeviceType: String index SPCRP_DEVTYPE read GetString;

    property Enumerator: String index SPDRP_ENUMERATOR_NAME read GetString;

    property FriendlyName: String index SPDRP_FRIENDLYNAME read GetString;
    property HardwareID: String index SPDRP_HARDWAREID read GetString;

    property Service: String index SPDRP_SERVICE read GetString;

    property Location: String index SPDRP_LOCATION_INFORMATION read GetString;
    property LowerFilters: String index SPDRP_LOWERFILTERS read GetString;
    property Manufacturer: String index SPDRP_MFG read GetString;
    property PhisicalDriverName: String index SPDRP_PHYSICAL_DEVICE_OBJECT_NAME read GetString;

    property RemovalPolicy: String index SPDRP_REMOVAL_POLICY read GetPolicy;
    property RemovalPolicyHWDefault: String index SPDRP_REMOVAL_POLICY_HW_DEFAULT read GetPolicy;
    property RemovalPolicyOverride: String index SPDRP_REMOVAL_POLICY_OVERRIDE read GetPolicy;

    property UINumber: DWORD index SPDRP_UI_NUMBER read GetDWORD;
    property UINumberDecription: String index SPDRP_UI_NUMBER_DESC_FORMAT read GetString;
    property UpperFilters: String index SPDRP_UPPERFILTERS read GetString;
  public
    property DeviceInfoData: SP_DEVINFO_DATA read FDeviceInfoData write FDeviceInfoData;
    property DeviceListHandle: HDEVINFO read FDeviceListHandle write FDeviceListHandle;
  end;

implementation

{ TWinDevice }

function TWinDevice.Capabilities: String;
var
  i: Integer;
  dwCapabilities: DWORD;
begin
  Result := EmptyStr;

  dwCapabilities := GetDWORD(SPDRP_CAPABILITIES);
  for i := 0 to 9 do
    if HasFlag(dwCapabilities, CapabilitiesRelationships[i].Flag) then
      AddToResult(Result, CapabilitiesRelationships[i].Desc);
end;

function TWinDevice.Characteristics: String;
var
  dwCharacteristics: DWORD;
begin
  dwCharacteristics := GetDWORD(SPDRP_CHARACTERISTICS);
  if dwCharacteristics <> 0 then
    System.SysUtils.Beep;
end;

function TWinDevice.ConfigFlags: String;
var
  i: Integer;
  dwConfigFlags: DWORD;
begin
  Result := EmptyStr;

  dwConfigFlags := GetDWORD(SPDRP_CONFIGFLAGS);
  for i := 0 to 15 do
    if HasFlag(dwConfigFlags, ConfigFlagRelationships[i].Flag) then
      AddToResult(Result, ConfigFlagRelationships[i].Desc);
end;

function TWinDevice.DeviceClassDescription(DeviceTypeGUID: TGUID): String;
var
  dwRequiredSize: DWORD;
begin
  Result := EmptyStr;

  dwRequiredSize := 0;
  SetupDiGetClassDescription(DeviceTypeGUID, nil, 0, dwRequiredSize);
  if GetLastError = ERROR_INSUFFICIENT_BUFFER then
  begin
    SetLength(Result, dwRequiredSize);
    SetupDiGetClassDescription(DeviceTypeGUID,
      @Result[1], dwRequiredSize, dwRequiredSize);
  end;
  Result := PChar(Result);
end;

function TWinDevice.DeviceClassDescription: String;
var
  LGUID: TGUID;
begin
  LGUID := ClassGUID;
  Result := DeviceClassDescription(LGUID);
end;

function TWinDevice.GetBinary(PropertyCode: Integer; pData: Pointer;
  dwSize: DWORD): Boolean;
var
  dwPropertyRegDataType, dwRequiredSize: DWORD;
begin
  dwRequiredSize := 0;
  dwPropertyRegDataType := REG_BINARY;
  Result := SetupDiGetDeviceRegistryProperty(DeviceListHandle, DeviceInfoData,
    PropertyCode, dwPropertyRegDataType, pData,
    dwSize, dwRequiredSize);
end;

function TWinDevice.GetDWORD(PropertyCode: Integer): DWORD;
var
  dwPropertyRegDataType, dwRequiredSize: DWORD;
begin
  Result := 0;
  dwRequiredSize := 4;
  dwPropertyRegDataType := REG_DWORD;
  SetupDiGetDeviceRegistryProperty(DeviceListHandle, DeviceInfoData,
    PropertyCode, dwPropertyRegDataType, @Result,
    dwRequiredSize, dwRequiredSize);
end;

function TWinDevice.GetGuid(PropertyCode: Integer): TGUID;
var
  dwPropertyRegDataType, dwRequiredSize: DWORD;
  LGUIDStr: String;
begin
  ZeroMemory(@Result, SizeOf(TGUID));
  LGUIDStr := GetString(PropertyCode);
  if LGUIDStr.IsEmpty then
  begin
    dwRequiredSize := 0;
    dwPropertyRegDataType := REG_BINARY;
    SetupDiGetDeviceRegistryProperty(DeviceListHandle, DeviceInfoData,
      PropertyCode, dwPropertyRegDataType, nil, 0, dwRequiredSize);
    if GetLastError = ERROR_INSUFFICIENT_BUFFER then
    begin
      SetupDiGetDeviceRegistryProperty(DeviceListHandle, DeviceInfoData,
        PropertyCode, dwPropertyRegDataType, @Result,
        dwRequiredSize, dwRequiredSize);
    end;
  end
  else
    Result := StringToGUID(LGUIDStr);
end;

function TWinDevice.GetPolicy(PropertyCode: Integer): String;
var
  dwPolicy: DWORD;
begin
  dwPolicy := GetDWORD(PropertyCode);
  if dwPolicy > 0 then
    case dwPolicy of
      CM_REMOVAL_POLICY_EXPECT_NO_REMOVAL:
        Result := 'CM_REMOVAL_POLICY_EXPECT_NO_REMOVAL';
      CM_REMOVAL_POLICY_EXPECT_ORDERLY_REMOVAL:
        Result := 'CM_REMOVAL_POLICY_EXPECT_ORDERLY_REMOVAL';
      CM_REMOVAL_POLICY_EXPECT_SURPRISE_REMOVAL:
        Result := 'CM_REMOVAL_POLICY_EXPECT_SURPRISE_REMOVAL';
    else
      Result := 'unknown 0x' + IntToHex(dwPolicy, 8);
    end;
end;

function TWinDevice.GetString(PropertyCode: Integer): String;
var
  dwPropertyRegDataType, dwRequiredSize: DWORD;
begin
  Result := EmptyStr;

  dwRequiredSize := 0;
  dwPropertyRegDataType := REG_SZ;
  SetupDiGetDeviceRegistryProperty(DeviceListHandle, DeviceInfoData,
    PropertyCode, dwPropertyRegDataType, nil, 0, dwRequiredSize);
  if not (dwPropertyRegDataType in [REG_SZ, REG_MULTI_SZ]) then Exit;
  if GetLastError = ERROR_INSUFFICIENT_BUFFER then
  begin
    SetLength(Result, dwRequiredSize);
    SetupDiGetDeviceRegistryProperty(DeviceListHandle, DeviceInfoData,
      PropertyCode, dwPropertyRegDataType, @Result[1],
      dwRequiredSize, dwRequiredSize);
  end;
  case dwPropertyRegDataType of
    REG_SZ: Result := PChar(Result);
    REG_MULTI_SZ: Result := ExtractMultiString(Result);
  end;
end;

function TWinDevice.InstallState: String;
var
  dwInstallState: DWORD;
begin
  dwInstallState := GetDWORD(SDRP_INSTALL_STATE);
  case dwInstallState of
    CM_INSTALL_STATE_INSTALLED:
      Result := 'CM_INSTALL_STATE_INSTALLED';
    CM_INSTALL_STATE_NEEDS_REINSTALL:
      Result := 'CM_INSTALL_STATE_NEEDS_REINSTALL';
    CM_INSTALL_STATE_FAILED_INSTALL:
      Result := 'CM_INSTALL_STATE_FAILED_INSTALL';
    CM_INSTALL_STATE_FINISH_INSTALL:
      Result := 'CM_INSTALL_STATE_FINISH_INSTALL';
  else
    Result := 'unknown 0x' + IntToHex(dwInstallState, 8);
  end;
end;

function TWinDevice.LegacyBusType: String;
var
  BusType: Integer;
begin
  BusType := Integer(GetDWORD(SPDRP_LEGACYBUSTYPE));
  case BusType of
    -1: Result := 'InterfaceTypeUndefined';
    00: Result := 'Internal';
    01: Result := 'Isa';
    02: Result := 'Eisa';
    03: Result := 'MicroChannel';
    04: Result := 'TurboChannel';
    05: Result := 'PCIBus';
    06: Result := 'VMEBus';
    07: Result := 'NuBus';
    08: Result := 'PCMCIABus';
    09: Result := 'CBus';
    10: Result := 'MPIBus';
    11: Result := 'MPSABus';
    12: Result := 'ProcessorInternal';
    13: Result := 'InternalPowerBus';
    14: Result := 'PNPISABus';
    15: Result := 'PNPBus';
    16: Result := 'MaximumInterfaceType';
  else
    Result := 'unknown 0x' + IntToHex(BusType, 8);
  end;
end;

function TWinDevice.PowerData: String;
var
  i: Integer;
  pPowerData: TCM_Power_Data;
begin
  Result := EmptyStr;
  if GetBinary(SPDRP_DEVICE_POWER_DATA, @pPowerData, SizeOf(TCM_Power_Data)) then
    for i := 0 to 8 do
      if HasFlag(pPowerData.PD_Capabilities, PDCAPRelationships[i].Flag) then
        AddToResult(Result, PDCAPRelationships[i].Desc);
end;

end.
