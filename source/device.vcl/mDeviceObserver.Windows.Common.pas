unit mDeviceObserver.Windows.Common;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.TypInfo, WinApi.Windows;

const
  //RegisterDeviceNotification function
  // DEVICE_NOTIFY_WINDOW_HANDLE       = $00000000; Define by Winapi.Windows.pas
  DEVICE_NOTIFY_SERVICE_HANDLE        = $00000001;
  DEVICE_NOTIFY_ALL_INTERFACE_CLASSES = $00000004;

const
  // DEV_BROADCAST_HDR structure
  DBT_DEVTYP_OEM              = $00000000;  // oem-defined device type
  DBT_DEVTYP_DEVNODE          = $00000001;  // devnode number
  DBT_DEVTYP_VOLUME           = $00000002;  // logical volume
  DBT_DEVTYP_PORT             = $00000003;  // serial, parallel
  DBT_DEVTYP_NET              = $00000004;  // network resource
  DBT_DEVTYP_DEVICEINTERFACE  = $00000005;  // device interface class
  DBT_DEVTYP_HANDLE           = $00000006;  // file system handle

const
  // WM_DEVICECHANGE message
  DBT_CONFIGCHANGECANCELED    = $00019   ;  // A request to change the current configuration (dock or undock) has been canceled.
  DBT_CONFIGCHANGED           = $00018   ;  // The current configuration has changed, due to a dock or undock.
  DBT_CUSTOMEVENT             = $08006   ;  // A custom event has occurred.
  DBT_DEVICEARRIVAL           = $08000   ;  // A device or piece of media has been inserted and is now available.
  DBT_DEVICEQUERYREMOVE       = $08001   ;  // Permission is requested to remove a device or piece of media. Any application can deny this request and cancel the removal.
  DBT_DEVICEQUERYREMOVEFAILED = $08002   ;  // A request to remove a device or piece of media has been canceled.
  DBT_DEVICEREMOVECOMPLETE    = $08004   ;  // A device or piece of media has been removed.
  DBT_DEVICEREMOVEPENDING     = $08003   ;  // A device or piece of media is about to be removed. Cannot be denied.
  DBT_DEVICETYPESPECIFIC      = $08005   ;  // A device-specific event has occurred.
  DBT_DEVNODES_CHANGED        = $00007   ;  // A device has been added to or removed from the system.
  DBT_QUERYCHANGECONFIG       = $00017   ;  // Permission is requested to change the current configuration (dock or undock).
  DBT_USERDEFINED             = $0FFFF   ;  // The meaning of this message is user-defined.

const
  // more infos
  //  http://pcsupport.about.com/od/driverssupport/a/device-class-guid.htm
  //  http://msdn.microsoft.com/en-us/library/windows/hardware/ff553426(v=vs.85).aspx
  //    System-Defined Device Setup Classes Available to Vendors (Windows Drivers)
  //Class	           GUID	                                        Device Description
  GUID_CDROM      : TGUID = '{4D36E965-E325-11CE-BFC1-08002BE10318}';  // CD/DVD/Blu-ray drives
  GUID_DiskDrive  : TGUID = '{4D36E967-E325-11CE-BFC1-08002BE10318}';  // Hard drives
  GUID_Display    : TGUID = '{4D36E968-E325-11CE-BFC1-08002BE10318}';  // Video adapters
  GUID_FDC        : TGUID = '{4D36E969-E325-11CE-BFC1-08002BE10318}';  // Floppy controllers
  GUID_FloppyDisk : TGUID = '{4D36E980-E325-11CE-BFC1-08002BE10318}';  // Floppy drives
  GUID_HDC        : TGUID = '{4D36E96A-E325-11CE-BFC1-08002BE10318}';  // Hard drive controllers
  GUID_HIDClass   : TGUID = '{745A17A0-74D3-11D0-B6FE-00A0C90F57DA}';  // Some USB devices
  GUID_1394       : TGUID = '{6BDD1FC1-810F-11D0-BEC7-08002BE2092F}';  // IEEE 1394 host controller
  GUID_Image      : TGUID = '{6BDD1FC6-810F-11D0-BEC7-08002BE2092F}';  // Cameras and scanners
  GUID_Keyboard   : TGUID = '{4D36E96B-E325-11CE-BFC1-08002BE10318}';  // Keyboards
  GUID_Modem      : TGUID = '{4D36E96D-E325-11CE-BFC1-08002BE10318}';  // Modems
  GUID_Mouse      : TGUID = '{4D36E96F-E325-11CE-BFC1-08002BE10318}';  // Mice and pointing devices
  GUID_Media      : TGUID = '{4D36E96C-E325-11CE-BFC1-08002BE10318}';  // Audio and video devices
  GUID_Net        : TGUID = '{4D36E972-E325-11CE-BFC1-08002BE10318}';  // Network adapters
  GUID_Ports      : TGUID = '{4D36E978-E325-11CE-BFC1-08002BE10318}';  // Serial and parallel ports
  GUID_SCSIAdapter: TGUID = '{4D36E97B-E325-11CE-BFC1-08002BE10318}';  // SCSI and RAID controllers
  GUID_System     : TGUID = '{4D36E97D-E325-11CE-BFC1-08002BE10318}';  // System buses, bridges, etc.
  GUID_USB        : TGUID = '{36FC9E60-C465-11CF-8056-444553540000}';  // USB host controllers and hubs

const
  // System-Defined Device Interface Classes
  //  MoreInfo: http://msdn.microsoft.com/en-us/library/windows/hardware/ff553412(v=vs.85).aspx

  // http://msdn.microsoft.com/en-us/library/windows/hardware/ff545972(v=vs.85).aspx
  //  The GUID_DEVINTERFACE_USB_DEVICE device interface class is defined for USB devices that are attached to a USB hub.
  GUID_DEVINTERFACE_USB_DEVICE: TGUID = '{A5DCBF10-6530-11D2-901F-00C04FB951ED}';
  // http://msdn.microsoft.com/en-us/library/windows/hardware/ff545821(v=vs.85).aspx
  //  The GUID_DEVINTERFACE_COMPORT device interface class is defined for COM ports.
  GUID_DEVINTERFACE_COMPORT: TGUID = '{86E0D1E0-8089-11D0-9CE4-08003E301F73}';

type
  DEV_BROADCAST_DEVICEINTERFACE = record
    dbcc_size       : DWORD;
    dbcc_devicetype : DWORD;
    dbcc_reserved   : DWORD;
    dbcc_classguid  : TGUID;
    dbcc_name       : Pointer;
  end;
  TDEV_BROADCAST_DEVICEINTERFACE = DEV_BROADCAST_DEVICEINTERFACE;
  PDEV_BROADCAST_DEVICEINTERFACE = ^TDEV_BROADCAST_DEVICEINTERFACE;

  TNotifyDevBroadcastDeviceInterface = procedure(Sender: TObject; Value: PDEV_BROADCAST_DEVICEINTERFACE) of object;
  TNotifyDevBroadcastDeviceInterfaceProc = reference to procedure(Value: PDEV_BROADCAST_DEVICEINTERFACE);

  DEV_BROADCAST_DEVICEINTERFACE_Helper = record helper for DEV_BROADCAST_DEVICEINTERFACE
    function dbcc_nameToStr: String;
    function dbcc_nameToRegPath: String;
    function dbcc_nameToDevIntefaceGUID: TGUID;
    function dbcc_NameExist: Boolean;
  end;

  DEV_BROADCAST_PORT = packed record
    dbcp_size       : DWORD;
    dbcp_devicetype : DWORD;
    dbcp_reserved   : DWORD;
  end;
  TDEV_BROADCAST_PORT = DEV_BROADCAST_PORT;
  PDEV_BROADCAST_PORT = ^TDEV_BROADCAST_PORT;

  TGUIDHelper = record helper for TGUID
    function ToString: String;
  end;

const
  STR_PORT = 'Port';
  STR_USB = 'USB';

type
  TDeviceType = (dtNone, dtComport, dtUSB);

  TDeviceTypeHelper = record helper for TDeviceType
    function Equals(const AValue: TDeviceType): Boolean;
    function Str: String;
    function GUID: TGUID;
    class function Create(const AValue: String): TDeviceType; static;
  end;

  TDvcDirection = (ddArrived, ddRemoved);
  EDvcDirectionHelperCreate = class(Exception);
  TDvcDirectionHelper = record Helper for TDvcDirection
    function Str: String;
    function Equal(const AValue: TDvcDirection): Boolean;
    class function Create(const AValue: String): TDvcDirection; static;
  end;

  TDbDiTo = record
  private
  public
    class function DeviceType(const Value: PDEV_BROADCAST_DEVICEINTERFACE): TDeviceType; static;
    class function DeviceDesc(const Value: PDEV_BROADCAST_DEVICEINTERFACE): String; static;
    class function FriendlyName(const Value: PDEV_BROADCAST_DEVICEINTERFACE): String; static;
    class function Infos(const Value: PDEV_BROADCAST_DEVICEINTERFACE): TStringList; overload; static;
    class function Infos(const ADirection: TDvcDirection; const Value: PDEV_BROADCAST_DEVICEINTERFACE): TStringList; overload; static;
  end;

function FriendlyNameToComport(const AFirendlyName: String): String;
function DeviceGUIDs: String;

function RegDeviceNotification(const AHandle: THandle;
                     const ADeviceInterface: TGUID;
                     AFlag: DWORD = DEVICE_NOTIFY_WINDOW_HANDLE): PHandle;
function UnRegDeviceNotification(const AHandle: PHandle): Boolean;

implementation

uses
  System.RegularExpressions, mConsts,
  mUtils.Windows, mStringListHelper;

function FriendlyNameToComport(const AFirendlyName: String): String;
const
  REG_EXP_COMPORT = 'COM\d*';
begin
  Result := EmptyStr;

  with TRegEx.Match(AFirendlyName, REG_EXP_COMPORT) do
    if Success then
      Exit(Value);
end;

function DeviceGUIDs: String;
begin
  Result := EmptyStr;
  Result := Result + Format('GUID_CDROM=%s'#13#10, [GUID_CDROM.ToString]);
  Result := Result + Format('GUID_DiskDrive=%s'#13#10, [GUID_DiskDrive.ToString]);
  Result := Result + Format('GUID_Display=%s'#13#10, [GUID_Display.ToString]);
  Result := Result + Format('GUID_FDC=%s'#13#10, [GUID_FDC.ToString]);
  Result := Result + Format('GUID_FloppyDisk=%s'#13#10, [GUID_FloppyDisk.ToString]);
  Result := Result + Format('GUID_HDC=%s'#13#10, [GUID_HDC.ToString]);
  Result := Result + Format('GUID_HIDClass=%s'#13#10, [GUID_HIDClass.ToString]);
  Result := Result + Format('GUID_1394=%s'#13#10, [GUID_1394.ToString]);
  Result := Result + Format('GUID_Image=%s'#13#10, [GUID_Image.ToString]);
  Result := Result + Format('GUID_Keyboard=%s'#13#10, [GUID_Keyboard.ToString]);
  Result := Result + Format('GUID_Modem=%s'#13#10, [GUID_Modem.ToString]);
  Result := Result + Format('GUID_Mouse=%s'#13#10, [GUID_Mouse.ToString]);
  Result := Result + Format('GUID_Media=%s'#13#10, [GUID_Media.ToString]);
  Result := Result + Format('GUID_Net=%s'#13#10, [GUID_Net.ToString]);
  Result := Result + Format('GUID_Ports=%s'#13#10, [GUID_Ports.ToString]);
  Result := Result + Format('GUID_SCSIAdapter=%s'#13#10, [GUID_SCSIAdapter.ToString]);
  Result := Result + Format('GUID_System=%s'#13#10, [GUID_System.ToString]);
  Result := Result + Format('GUID_USB=%s'#13#10, [GUID_USB.ToString]);
end;

function RegDeviceNotification(const AHandle: THandle; const ADeviceInterface: TGUID; AFlag: DWORD): PHandle;
var
  LDbi: DEV_BROADCAST_DEVICEINTERFACE;
begin
  ZeroMemory(@LDbi, SizeOf(DEV_BROADCAST_DEVICEINTERFACE));
  LDbi.dbcc_size := SizeOf(DEV_BROADCAST_DEVICEINTERFACE);
  LDbi.dbcc_devicetype := DBT_DEVTYP_DEVICEINTERFACE;
  LDbi.dbcc_reserved := 0;
  LDbi.dbcc_classguid := ADeviceInterface;
  LDbi.dbcc_name := nil;

  Result := RegisterDeviceNotification(AHandle, @LDbi, AFlag);
end;

function UnRegDeviceNotification(const AHandle: PHandle): Boolean;
begin
  Result := UnregisterDeviceNotification(AHandle);
end;

const
  REG_NAME_CLASS_GUID    = 'ClassGUID';
  REG_NAME_DEVICE_DESC   = 'DeviceDesc';
  REG_NAME_FRIENDLY_NAME = 'FriendlyName';

function CompareRegClassGUID(AKey: HKEY; const AGUID: TGUID;
  const ARegKey, AName: String): Boolean;
var
  LGUID: String;
begin
  Result := False;
  LGUID := ReadRegNameString(AKey, ARegKey, AName);
  if not LGUID.IsEmpty then
    Result := AGUID = StringToGUID(LGUID);
end;

{ TGUIDHelper }

function TGUIDHelper.ToString: String;
begin
  Result := GUIDToString(Self)
end;

{ DEV_BROADCAST_DEVICEINTERFACE_Helper }

function DEV_BROADCAST_DEVICEINTERFACE_Helper.dbcc_nameToDevIntefaceGUID: TGUID;
const
  IDX_DEV_INTERFACE = 3;
var
  LdbccName: String;
  LStrs: TArray<String>;
begin
  LdbccName := dbcc_nameToStr;
  if LdbccName = EmptyStr then
    Exit;

  LStrs := LdbccName.Split(['#']);
  if Length(LStrs) = 4 then
    Result := StringToGUID(LStrs[IDX_DEV_INTERFACE]);
end;

function DEV_BROADCAST_DEVICEINTERFACE_Helper.dbcc_nameToRegPath: String;
const
  FMT_REG_USB = 'SYSTEM\CurrentControlSet\Enum\USB\%s\%s\';

  IDX_VID_PID    = 1;
  IDX_SUB_FOLDER = 2;
var
  LdbccName: String;
  LStrs: TArray<String>;
begin
  Result := EmptyStr;
  LdbccName := dbcc_nameToStr;
  if LdbccName = EmptyStr then
    Exit;

  LStrs := LdbccName.Split(['#']);
  if Length(LStrs) = 4 then
    Result := Format(FMT_REG_USB, [LStrs[IDX_VID_PID], LStrs[IDX_SUB_FOLDER]]);
end;

function DEV_BROADCAST_DEVICEINTERFACE_Helper.dbcc_nameToStr: String;
begin
  Result := EmptyStr;
  if dbcc_size = 0 then
    Exit;

  Result := PChar(@Self.dbcc_name);
end;

function DEV_BROADCAST_DEVICEINTERFACE_Helper.dbcc_NameExist: Boolean;
begin
  Result := (dbcc_size > 0) and (Length(dbcc_nameToStr.Split(['#'])) = 4);
end;

{ TDeviceInfo }

class function TDbDiTo.DeviceDesc(
  const Value: PDEV_BROADCAST_DEVICEINTERFACE): String;
begin
  Result := EmptyStr;
  if Assigned(Value) and Value.dbcc_nameExist then
    Result := ReadRegNameString(HKEY_LOCAL_MACHINE, Value.dbcc_nameToRegPath, REG_NAME_DEVICE_DESC);
end;

class function TDbDiTo.DeviceType(
  const Value: PDEV_BROADCAST_DEVICEINTERFACE): TDeviceType;
const
  GUID_DRAGON: TGUID = '{9D7DEBBC-C85D-11D1-9EB4-006008C3A19A}';
begin
  Result := dtNone;
  if Assigned(Value) and Value.dbcc_nameExist then
    if CompareRegClassGUID(HKEY_LOCAL_MACHINE, GUID_Ports, Value.dbcc_nameToRegPath, REG_NAME_CLASS_GUID) then
      Result := dtComport
    else if CompareRegClassGUID(HKEY_LOCAL_MACHINE, GUID_USB, Value.dbcc_nameToRegPath, REG_NAME_CLASS_GUID) then
      Result := dtUSB;
end;

class function TDbDiTo.FriendlyName(
  const Value: PDEV_BROADCAST_DEVICEINTERFACE): String;
begin
  Result := EmptyStr;
  if Assigned(Value) and Value.dbcc_nameExist then
    Result := ReadRegNameString(HKEY_LOCAL_MACHINE, Value.dbcc_nameToRegPath, REG_NAME_FRIENDLY_NAME);
end;

class function TDbDiTo.Infos(const ADirection: TDvcDirection;
  const Value: PDEV_BROADCAST_DEVICEINTERFACE): TStringList;
begin
  Result := Infos(Value);
  Result.S['Direction'] := ADirection.Str;
end;

class function TDbDiTo.Infos(
  const Value: PDEV_BROADCAST_DEVICEINTERFACE): TStringList;
begin
  Result := TStringList.Create;

  Result.GUID['classguid'] := Value.dbcc_classguid;
  Result.S['DeviceType'] :=   DeviceType(Value).Str;
  Result.S['DeviceDesc'] :=   DeviceDesc(Value);
  Result.S['FriendlyName'] := FriendlyName(Value);
  Result.S['Comport'] := FriendlyNameToComport(Result.S['FriendlyName'])
end;

{ TDeviceTypeHelper }

function TDeviceTypeHelper.Equals(const AValue: TDeviceType): Boolean;
begin
  Result := Self = AValue;
end;

class function TDeviceTypeHelper.Create(const AValue: String): TDeviceType;
begin
  Result := dtNone;
  for Result := Low(TDeviceType) to High(TDeviceType) do
    if Result.Str.ToLower.Equals(AValue.ToLower) then
      Break;
end;

function TDeviceTypeHelper.GUID: TGUID;
begin
  case Self of
    dtNone: Result := TGUID.Empty;
    dtComport: Result := GUID_Ports;
    dtUSB: Result := GUID_USB;
  end;
end;

function TDeviceTypeHelper.Str: String;
begin
  Result := GetEnumName(TypeInfo(TDeviceType), Integer(Self));
end;

{ TDvcDirectionHelper }

class function TDvcDirectionHelper.Create(const AValue: String): TDvcDirection;
var
  LResult: Integer;
begin
  LResult := VAL_NOT_ASSIGNED;
  for Result := Low(TDvcDirection) to High(TDvcDirection) do
    if Result.Str.ToLower.Equals(AValue.ToLower) then
    begin
      LResult := Integer(Result);
      Break;
    end;
  if LResult = VAL_NOT_ASSIGNED then
    raise EDvcDirectionHelperCreate.CreateFmt('%s is Can''t matched to TDvcDirection', [AValue]);
end;

function TDvcDirectionHelper.Equal(const AValue: TDvcDirection): Boolean;
begin
  Result := Self = AValue;
end;

function TDvcDirectionHelper.Str: String;
begin
  Result := GetEnumName(TypeInfo(TDvcDirection), Integer(Self));
end;

end.
