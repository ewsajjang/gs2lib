unit mDeviceList.Windows;

interface

uses
  mDevice.Windows,
  mDeviceList.Windows.Common,

  SetupApi,

  System.SysUtils, System.Classes, WinApi.Windows, System.Generics.Collections;

type
  TWinDeviceList = class(TInterfacedObject, IDeviceList)
  private
    FGUIDs: array of TGUID;
    FList: TList<TItem>;

    FhDevice: HDEVINFO;
    FCurrentDevice: TWinDevice;

    { IDeviceList }
    function GetDevices(Index: Integer): TWinDevice;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    { IDeviceList }
    procedure ClearAndSearch(const AGUIDs: array of TGUID);
    function Exists(AFunc: TFuncExistDevice): Boolean; overload;
    function Exists(const AGUID: TGUID; AFunc: TFuncExistDevice): Boolean; overload;
    procedure Clear;

    { IDeviceList }
    property Devices[Index: Integer]: TWinDevice read GetDevices; default;
    property Count: Integer read GetCount;
  end;

implementation

{ TWinDeviceList }

procedure TWinDeviceList.Clear;
begin
  FList.Clear;
end;

constructor TWinDeviceList.Create;
begin
  if not LoadSetupApi then
    RaiseLastOSError;

  FCurrentDevice := TWinDevice.Create;

  FList := TList<TItem>.Create;

  FhDevice := INVALID_HANDLE_VALUE;
end;

destructor TWinDeviceList.Destroy;
begin
  FreeAndNil(FList);
  FCurrentDevice.Free;
  if FhDevice <> INVALID_HANDLE_VALUE then
    SetupDiDestroyDeviceInfoList(FhDevice);

  inherited;
end;

function TWinDeviceList.Exists(AFunc: TFuncExistDevice): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to FList.Count - 1 do
  begin
    Result:= AFunc(Devices[i], i);
    if Result then
      Break;
  end;
end;

function TWinDeviceList.Exists(const AGUID: TGUID; AFunc: TFuncExistDevice): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to FList.Count - 1 do
    if FList[i].Key.GUID = AGUID then
    begin
      Result := AFunc(Devices[i], i);
      if Result then
        Break;
    end;
end;

function TWinDeviceList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TWinDeviceList.GetDevices(Index: Integer): TWinDevice;
var
  LDeviceInfoData: SP_DEVINFO_DATA;
begin
  FCurrentDevice.DeviceListHandle := FList.Items[Index].Key.HDevList;

  ZeroMemory(@LDeviceInfoData, SizeOf(SP_DEVINFO_DATA));
  LDeviceInfoData.cbSize := SizeOf(SP_DEVINFO_DATA);
  if not SetupDiEnumDeviceInfo(FCurrentDevice.DeviceListHandle, FList.Items[Index].Key.IdxOfList, LDeviceInfoData) then
    Exit(nil);

  FCurrentDevice.DeviceInfoData := LDeviceInfoData;
  Result := FCurrentDevice;
end;

procedure TWinDeviceList.ClearAndSearch(const AGUIDs: array of TGUID);
var
  i: Integer;
  LIdx: DWORD;
  LDeviceInfoData: SP_DEVINFO_DATA;
  LDeviceName: String;
  LKey: TKey;
begin
  FList.Clear;
  SetLength(FGUIDs, Length(AGuids));
  for i := Low(AGUIDs) to High(AGUIDs) do
  begin
    FGUIDs[i] := AGUIDs[i];

    FhDevice := SetupDiGetClassDevsEx(@FGUIDs[i], nil, 0, DIGCF_PRESENT, 0, nil, nil);
    if FhDevice = INVALID_HANDLE_VALUE then
      RaiseLastOSError;
    FCurrentDevice.DeviceListHandle := FhDevice;

    LIdx := 0;
    ZeroMemory(@LDeviceInfoData, SizeOf(SP_DEVINFO_DATA));
    LDeviceInfoData.cbSize := SizeOf(SP_DEVINFO_DATA);
    while SetupDiEnumDeviceInfo(FhDevice, LIdx, LDeviceInfoData) do
    begin

      FCurrentDevice.DeviceInfoData := LDeviceInfoData;
      LDeviceName := FCurrentDevice.FriendlyName;
      if LDeviceName = EmptyStr then
        LDeviceName := FCurrentDevice.Description;

      LKey := TKey.Create(FCurrentDevice.DeviceListHandle, FGUIDs[i], LIdx);
      FList.Add(TItem.Create(LKey, LDeviceName));

      Inc(LIdx);
    end;
  end;
end;

end.
