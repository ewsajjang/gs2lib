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
    FRegHandles: TList<PHandle>;
    FHandle: THandle;
    FOnArrive: TNotifyDevBroadcastDeviceInterface;
    FOnRemoved: TNotifyDevBroadcastDeviceInterface;
    FOnNodeChange: TNotifyInteger;
    procedure WmDeviceChange(var Msg : TMessage); message WM_DEVICECHANGE;
    procedure UnRegGUID;
  protected
    procedure do_WndProc(var Message:TMessage); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddNotifycation(const ADeviceInterface: TGUID): Boolean;

    property OnNodeChange: TNotifyInteger read FOnNodeChange write FOnNodeChange;
    property OnArrived: TNotifyDevBroadcastDeviceInterface read FOnArrive write FOnArrive;
    property OnRemoved: TNotifyDevBroadcastDeviceInterface read FOnRemoved write FOnRemoved;
  end;

implementation

{ TWinDeviceObserver }

constructor TWinDeviceObserver.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FHandle := AllocateHWND(do_WndProc);
  FRegHandles := TList<PHandle>.Create;
end;

destructor TWinDeviceObserver.Destroy;
begin
  UnRegGUID;
  DeAllocateHWND(FHandle);
  FreeAndNil(FRegHandles);

  inherited;
end;

procedure TWinDeviceObserver.do_WndProc(var Message: TMessage);
begin
  Dispatch(Message);
end;

function TWinDeviceObserver.AddNotifycation(const ADeviceInterface: TGUID): Boolean;
var
  LHandle: PHandle;
begin
  LHandle := RegDeviceNotification(FHandle, ADeviceInterface);
  Result := Assigned(LHandle) and (LHandle^ <> INVALID_HANDLE_VALUE);
  if Result then
    FRegHandles.Add(LHandle);
end;

procedure TWinDeviceObserver.UnRegGUID;
var
  LHandle: PHandle;
begin
  for LHandle in FRegHandles do
    UnRegDeviceNotification(LHandle);
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
            if Assigned(FOnArrive) then
              FOnArrive(Self, LDbDi);

          DBT_DEVICEREMOVECOMPLETE:
            if Assigned(FOnRemoved) then
              FOnRemoved(Self, LDbDi);
        end;
      end;

  end;
end;

end.
