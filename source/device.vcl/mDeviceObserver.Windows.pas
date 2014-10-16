unit mDeviceObserver.Windows;

interface

uses
  mTypes,
  mDeviceObserver.Windows.Common,

  System.SysUtils, System.Classes,

  WinApi.Messages, WinAPI.Windows,

  System.Generics.Collections;

type
  TWinDeviceObserver = class(TComponent)
  private
    FRegHandles: TDictionary<TGUID, PHandle>;
    FHandle: THandle;
    FOnArrived: TNotifyDevBroadcastDeviceInterface;
    FOnRemoved: TNotifyDevBroadcastDeviceInterface;
    FOnNodeChange: TNotifyInteger;
    FOnArrivedProc: TNotifyDevBroadcastDeviceInterfaceProc;
    FOnRemovedProc: TNotifyDevBroadcastDeviceInterfaceProc;
    procedure WmDeviceChange(var Msg : TMessage); message WM_DEVICECHANGE;
  protected
    procedure do_WndProc(var Message:TMessage); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddNotifycation(const ADeviceInterface: TGUID): Boolean;
    procedure RemoveNotification(const ADeviceInterface: TGUID);

    property OnNodeChange: TNotifyInteger read FOnNodeChange write FOnNodeChange;
    property OnArrived: TNotifyDevBroadcastDeviceInterface read FOnArrived write FOnArrived;
    property OnArrivedProc: TNotifyDevBroadcastDeviceInterfaceProc read FOnArrivedProc write FOnArrivedProc;
    property OnRemoved: TNotifyDevBroadcastDeviceInterface read FOnRemoved write FOnRemoved;
    property OnRemovedProc: TNotifyDevBroadcastDeviceInterfaceProc read FOnRemovedProc write FOnRemovedProc;
  end;

implementation

{ TWinDeviceObserver }

constructor TWinDeviceObserver.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FHandle := AllocateHWND(do_WndProc);
  FRegHandles := TDictionary<TGUID, PHandle>.Create;
end;

destructor TWinDeviceObserver.Destroy;
var
  LHandle: PHandle;
begin
  for LHandle in FRegHandles.Values do
    UnRegDeviceNotification(LHandle);
  DeAllocateHWND(FHandle);
  FreeAndNil(FRegHandles);

  inherited;
end;

procedure TWinDeviceObserver.do_WndProc(var Message: TMessage);
begin
  Dispatch(Message);
end;

procedure TWinDeviceObserver.RemoveNotification(
  const ADeviceInterface: TGUID);
var
  LHandle: PHandle;
begin
  if FRegHandles.TryGetValue(ADeviceInterface, LHandle) then
    if UnRegDeviceNotification(LHandle) then
      FRegHandles.Remove(ADeviceInterface);
end;

function TWinDeviceObserver.AddNotifycation(const ADeviceInterface: TGUID): Boolean;
var
  LHandle: PHandle;
begin
  LHandle := RegDeviceNotification(FHandle, ADeviceInterface);
  Result := Assigned(LHandle) and (LHandle^ <> INVALID_HANDLE_VALUE);
  if Result then
    FRegHandles.AddOrSetValue(ADeviceInterface, LHandle);
end;

procedure TWinDeviceObserver.WmDeviceChange(var Msg: TMessage);
var
  LDbDi: PDEV_BROADCAST_DEVICEINTERFACE;
begin
  case Msg.WParam of
    DBT_DEVNODES_CHANGED :
      if Assigned(FOnNodeChange) then FOnNodeChange(Self, Msg.WParam);

    DBT_DEVICEARRIVAL       ,
    DBT_DEVICEREMOVECOMPLETE:
      begin
        LDbDi := Pointer(Msg.LParam);
        case Msg.WParam of
          DBT_DEVICEARRIVAL:
          begin
            if Assigned(FOnArrived) then
              FOnArrived(Self, LDbDi);
            if Assigned(FOnArrivedProc) then
              FOnArrivedProc(LDbDi);
          end;

          DBT_DEVICEREMOVECOMPLETE:
          begin
            if Assigned(FOnRemoved) then
              FOnRemoved(Self, LDbDi);
            if Assigned(FOnRemovedProc) then
              FOnRemovedProc(LDbDi);
          end;
        end;
      end;

  end;
end;

end.
