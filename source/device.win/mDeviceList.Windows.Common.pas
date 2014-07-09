unit mDeviceList.Windows.Common;

interface

uses
  mDevice.Windows,

  System.Generics.Collections;

type
  TFuncExistDevice = reference to function(AInfo: TWinDevice; AFindIdx: Integer): Boolean;
  TNotifyDevice = procedure(Sender: TObject; GUID: TGUID) of object;

  IDeviceList = interface
    ['{9F77C1E9-0380-41AE-AB19-0C4ADB034379}']
    function GetDevices(Index: Integer): TWinDevice;
    function GetCount: Integer;

    function Exists(AFunc: TFuncExistDevice): Boolean; overload;
    function Exists(const AGUID: TGUID; AFunc: TFuncExistDevice): Boolean; overload;
    procedure ClearAndSearch(const AGUIDs: array of TGUID);
    procedure Clear;

    property Devices[Index: Integer]: TWinDevice read GetDevices; default;
    property Count: Integer read GetCount;
  end;

  HDEVINFO = THandle;
  TWinDvcItemKey = record
    HDevList: HDEVINFO;
    IdxOfList: Integer;
    GUID: TGUID;
    constructor Create(const AHDevList: HDEVINFO; const AGUID: TGUID; const AIdxOfList: Integer);
  end;

  TWinDvcItem = TPair<TWinDvcItemKey, String>;

implementation

{ TKey }

constructor TWinDvcItemKey.Create(const AHDevList: HDEVINFO; const AGUID: TGUID; const AIdxOfList: Integer);
begin
  HDevList := AHDevList;
  GUID := AGUID;
  IdxOfList := AIdxOfList;
end;

end.
