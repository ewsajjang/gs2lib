unit mWclHelper;

interface

uses
  wclBluetoothDiscovery, wclAPI;

type
  TwclBluetoothRadioHelper = class helper for TwclBluetoothRadio
  private
    function _GetConnectable: Boolean;
    function _GetDiscoverable: Boolean;
    function _GetName: String;
    function _GetAPIStr: String;
    function _GetAddr: String;
    function _GetEnabled: Boolean;
  public
    class function Clone(const ASrc: TwclBluetoothRadio): TwclBluetoothRadio; static;

    property Name: String read _GetName;
    property APIStr: String read _GetAPIStr;
    property Addr: String read _GetAddr;
    property Discoverable: Boolean read _GetDiscoverable;
    property Connectable: Boolean read _GetConnectable;
    property Enabled: Boolean read _GetEnabled;
  end;

  TwclBluetoothDeviceHelper = class helper for TwclBluetoothDevice
  private
  public
    function Name(const ARadio: TwclBluetoothRadio): String;
    function Addr: String;
    function Paired(const ARadio: TwclBluetoothRadio): Boolean;
    function COD(const ARadio: TwclBluetoothRadio): String;
  end;

  TwclBluetoothAPIHeper = record helper for TWclBluetoothAPI
    function Str: String;
  end;

implementation

uses
  wclErrors, wclUUIDs,
  System.SysUtils, System.TypInfo
  ;

{ TwclBluetoothRadioHelper }

function TwclBluetoothRadioHelper._GetEnabled: Boolean;
begin
  Result := Connectable and Discoverable;
end;

class function TwclBluetoothRadioHelper.Clone(
  const ASrc: TwclBluetoothRadio): TwclBluetoothRadio;
begin
  Result := TwclBluetoothRadio.Create;
  Result.Assign(ASrc);
end;

function TwclBluetoothRadioHelper._GetAddr: String;
var
  LValue: WideString;
begin
  Result := EmptyStr;
  if GetAddress(LValue) = WCL_E_SUCCESS then
    Result := LValue;
end;

function TwclBluetoothRadioHelper._GetAPIStr: String;
begin
  Result := API.Str;
end;

function TwclBluetoothRadioHelper._GetConnectable: Boolean;
begin
  if GetConnectable(Result) <> WCL_E_SUCCESS then
    Result := False;
end;

function TwclBluetoothRadioHelper._GetDiscoverable: Boolean;
begin
  if GetDiscoverable(Result) <> WCL_E_SUCCESS then
    Result := False;
end;

function TwclBluetoothRadioHelper._GetName: String;
var
  LValue: WideString;
begin
  Result := EmptyStr;
  if GetName(LValue) = WCL_E_SUCCESS then
    Result := LValue;
end;

{ TwclBluetoothDeviceHelper }

function TwclBluetoothDeviceHelper.Addr: String;
begin
  Result := Address;
end;

function TwclBluetoothDeviceHelper.COD(
  const ARadio: TwclBluetoothRadio): String;
var
  LCOD: Cardinal;
begin
  Result := EmptyStr;
  if GetClassOfDevice(ARadio, LCOD) = WCL_E_SUCCESS then
    Result := Format('%.6x', [LCOD]);
end;

function TwclBluetoothDeviceHelper.Name(const ARadio: TwclBluetoothRadio): String;
var
  LValue: WideString;
begin
  Result := EmptyStr;
  if GetName(ARadio, LValue) = WCL_E_SUCCESS then
    Result := LValue;
end;

function TwclBluetoothDeviceHelper.Paired(const ARadio: TwclBluetoothRadio): Boolean;
var
  LPaired: Boolean;
begin
  Result := False;
  if GetPaired(ARadio, LPaired) = WCL_E_SUCCESS then
    Result := LPaired
end;

{ TwclBluetoothAPIHeper }

function TwclBluetoothAPIHeper.Str: String;
begin
  Result := GetEnumName(TypeInfo(TWclBluetoothAPI), Integer(Self)).Substring(2);
end;

end.
