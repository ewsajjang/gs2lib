unit mWclHelper;

interface

uses
  wclBluetooth,
  Classes, SysUtils
  ;

const
  BT_STATUS: array [False .. True] of string = ('[-]', '[+]');
  BT_DEVICE_HA10B = 'SDB.HA10B';

type
  TwclBluetoothRadioHelper = class helper for TwclBluetoothRadio
    function Name: string;
    function Connectable: Boolean;
    function Address: Int64;
    function AsAddrString: string;
    function RemoteName(const Address: Int64): string;
  end;

function wclExeLog(const ASuccess: Integer; const AStr: string): Boolean;
function wclErrorMsg(const AError: Integer): string;
function wclAddressToString(const AAddress: Int64): string;

implementation

uses
  wclErrors,
  mLog.codeSite
  ;

function wclAddressToString(const AAddress: Int64): string;
var
  LHex: string;
begin
  if AAddress = 0 then
    Exit('');

  LHex := AAddress.ToHexString(12);
  Result := Format('(%s:%s:%s:%s:%s:%s)', [
    LHex.Substring(0, 2), LHex.Substring(2, 2), LHex.Substring(4, 2),
    LHex.Substring(6, 2), LHex.Substring(8, 2), LHex.Substring(10, 2)]);
end;

function wclErrorString(const AError: Integer): string;
begin
  case AError of
    WCL_E_SUCCESS: Result := '';

    //Common error codes.
    //WCL_E_BASE             : Result := 'WCL_E_BASE';
    WCL_E_INVALID_ARGUMENT : Result := 'WCL_E_INVALID_ARGUMENT';
    WCL_E_OUT_OF_MEMORY    : Result := 'WCL_E_OUT_OF_MEMORY';

    //Message recevier error codes
    //WCL_E_MR_BASE                     : Result := 'WCL_E_MR_BASE';
    WCL_E_MR_CLOSED                   : Result := 'WCL_E_MR_CLOSED';
    WCL_E_MR_OPENED                   : Result := 'WCL_E_MR_OPENED';
    WCL_E_MR_NOT_OPENED               : Result := 'WCL_E_MR_NOT_OPENED';
    WCL_E_MR_UNABLE_SYNCHRONIZE       : Result := 'WCL_E_MR_UNABLE_SYNCHRONIZE';
    WCL_E_MR_UNABLE_REGISTER_SYNC_OBJ : Result := 'WCL_E_MR_UNABLE_REGISTER_SYNC_OBJ';
    WCL_E_MR_UNABLE_CREATE_SYNC_OBJ   : Result := 'WCL_E_MR_UNABLE_CREATE_SYNC_OBJ';
    WCL_E_MR_SYNC_OBJ_NOT_CREATED     : Result := 'WCL_E_MR_SYNC_OBJ_NOT_CREATED';

    //Message broadcaster error codes
    //WCL_E_MB_BASE                        : Result := 'WCL_E_MB_BASE';
    WCL_E_MB_RECEIVER_ALREADY_SUBSCRIBED : Result := 'WCL_E_MB_RECEIVER_ALREADY_SUBSCRIBED';
    WCL_E_MB_RECEIVER_NOT_SUBSCRIBED     : Result := 'WCL_E_MB_RECEIVER_NOT_SUBSCRIBED';
    WCL_E_MB_CAN_NOT_INIT_HW_THREAD      : Result := 'WCL_E_MB_CAN_NOT_INIT_HW_THREAD';
    WCL_E_MB_CAN_NOT_START_HW_THREAD     : Result := 'WCL_E_MB_CAN_NOT_START_HW_THREAD';
    WCL_E_MB_UNABLE_CREATE_HW_OBJ        : Result := 'WCL_E_MB_UNABLE_CREATE_HW_OBJ';
    WCL_E_MB_UNABLE_REGISTER_HW_NOTIFY   : Result := 'WCL_E_MB_UNABLE_REGISTER_HW_NOTIFY';
    WCL_E_MB_UNABLE_CREATE_MUTEX         : Result := 'WCL_E_MB_UNABLE_CREATE_MUTEX';
    WCL_E_MB_NOT_CREATED                 : Result := 'WCL_E_MB_NOT_CREATED';
    WCL_E_MB_REF_COUNT_ERROR             : Result := 'WCL_E_MB_REF_COUNT_ERROR';

    //WinRT subsystem error codes.
    //WCL_E_WINRT_BASE                     : Result := 'WCL_E_WINRT_BASE';
    WCL_E_WINRT_UNABLE_CREATE_MUTEX      : Result := 'WCL_E_WINRT_UNABLE_CREATE_MUTEX';
    WCL_E_WINRT_UNABLE_LOAD_CORE_DLL     : Result := 'WCL_E_WINRT_UNABLE_LOAD_CORE_DLL';
    WCL_E_WINRT_UNABLE_LOAD_STRING_DLL   : Result := 'WCL_E_WINRT_UNABLE_LOAD_STRING_DLL';
    WCL_E_WINRT_INIT_FAILED              : Result := 'WCL_E_WINRT_INIT_FAILED';
    WCL_E_WINRT_UNABLE_CREATE_STRING     : Result := 'WCL_E_WINRT_UNABLE_CREATE_STRING';
    WCL_E_WINRT_UNABLE_ACTIVATE_INSTANCE : Result := 'WCL_E_WINRT_UNABLE_ACTIVATE_INSTANCE';
    WCL_E_WINRT_UNABLE_CREATE_INTERFACE  : Result := 'WCL_E_WINRT_UNABLE_CREATE_INTERFACE';
  end;
end;

function wclErrorMsg(const AError: Integer): string;
begin
  Result := '';
  case AError of
    WCL_E_SUCCESS: Result := 'Operation completed with success.';

    //Common error codes.
    //WCL_E_BASE            : Result := 'The base error code for the WCL system/common errors.';
    WCL_E_INVALID_ARGUMENT: Result := 'One or more arguments passed into the method or function are invalid';
    WCL_E_OUT_OF_MEMORY   : Result := 'Out of system memory.';

    //Message recevier error codes
    //WCL_E_MR_BASE                    : Result := 'The base error code for the message receiver.';
    WCL_E_MR_CLOSED                  : Result := 'A message receiver has already been closed or not opened.';
    WCL_E_MR_OPENED                  : Result := 'A message receiver has already been opened.';
    WCL_E_MR_NOT_OPENED              : Result := 'A message receiver was not opened yet.';
    WCL_E_MR_UNABLE_SYNCHRONIZE      : Result := 'A message receiver can not synchronize threads and notify a "main" thread about new received message.';
    WCL_E_MR_UNABLE_REGISTER_SYNC_OBJ: Result := 'A message receiver can not register for threads synchronization.';
    WCL_E_MR_UNABLE_CREATE_SYNC_OBJ  : Result := 'A message receiver can not create a thread synchronization object.';
    WCL_E_MR_SYNC_OBJ_NOT_CREATED    : Result := 'A thread synchronization object has not been created.';

    //Message broadcaster error codes
    //WCL_E_MB_BASE                       : Result := 'The base error code for the message broadcaster.';
    WCL_E_MB_RECEIVER_ALREADY_SUBSCRIBED: Result := 'A message receiver has already been subscribed.';
    WCL_E_MB_RECEIVER_NOT_SUBSCRIBED    : Result := 'A message receiver has not been subscribed.';
    WCL_E_MB_CAN_NOT_INIT_HW_THREAD     : Result := 'The message broadcaster can not initialize the hardware monitoring thread.';
    WCL_E_MB_CAN_NOT_START_HW_THREAD    : Result := 'The message broadcaster can not start the hardware monitoring thread.';
    WCL_E_MB_UNABLE_CREATE_HW_OBJ       : Result := 'The message broadcaster can not create the hardware monitoring object.';
    WCL_E_MB_UNABLE_REGISTER_HW_NOTIFY  : Result := 'The message broadcaster can not register for hardware changes notifications.';
    WCL_E_MB_UNABLE_CREATE_MUTEX        : Result := 'The message broadcaster can not create a mutex synchronization object.';
    WCL_E_MB_NOT_CREATED                : Result := 'The message broadcaster was not created.';
    WCL_E_MB_REF_COUNT_ERROR            : Result := 'There is the error with the references counter.';

    //WinRT subsystem error codes.
    //WCL_E_WINRT_BASE                    : Result := 'The base WinRT subsystem error code.';
    WCL_E_WINRT_UNABLE_CREATE_MUTEX     : Result := 'Unable to create synchronization mutex for WinRT control.';
    WCL_E_WINRT_UNABLE_LOAD_CORE_DLL    : Result := 'unable to load WinRT core DLL.';
    WCL_E_WINRT_UNABLE_LOAD_STRING_DLL  : Result := 'unable to load WinRT string DLL.';
    WCL_E_WINRT_INIT_FAILED             : Result := 'WinRT initialization failed.';
    WCL_E_WINRT_UNABLE_CREATE_STRING    : Result := 'Unable to create string.';
    WCL_E_WINRT_UNABLE_ACTIVATE_INSTANCE: Result := 'Unable to active instance of the required class.';
    WCL_E_WINRT_UNABLE_CREATE_INTERFACE : Result := 'Unable to create interface of the required class.';
  end;

  if not Result.IsEmpty then
    Result := Format('[%s]%s', [wclErrorString(AError), Result]);
end;

function wclExeLog(const ASuccess: Integer; const AStr: string): Boolean;
begin
  Result := ASuccess = WCL_E_SUCCESS;
  Log.Msg(Result, '%s: [%.12x]%s', [AStr, ASuccess, wclErrorMsg(ASuccess)]);
end;

{ TwclBluetoothRadioHelper }

function TwclBluetoothRadioHelper.Address: Int64;
begin
  Result := GetAddress(Result);
end;

function TwclBluetoothRadioHelper.AsAddrString: string;
var
  LAddr: Int64;
begin
  if GetAddress(LAddr) <> WCL_E_SUCCESS then
    Result := ''
  else
    Result := wclAddressToString(LAddr);
end;

function TwclBluetoothRadioHelper.Connectable: Boolean;
begin
  GetConnectable(Result);
end;

function TwclBluetoothRadioHelper.Name: string;
begin
  GetName(Result);
end;

function TwclBluetoothRadioHelper.RemoteName(const Address: Int64): string;
begin
  GetRemoteName(Address, Result);
end;

end.
