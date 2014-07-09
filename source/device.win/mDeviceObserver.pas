unit mDeviceObserver;

interface

uses
  mDevice.Common, mDevice.Windows,
  mDeviceList.Windows.Common, mDeviceList.Windows,
  mDeviceObserver.Windows, mDeviceObserver.Windows.Common,

  mHandleComponent.Windows,

  System.Classes, System.SysUtils
  ;

type
  TdvcObserver = class(THandleComponent)
  private
    FDvcName: String;
    FDvcType: TDeviceType;
    FDvcList: IDeviceList;
    FObserver: TWinDeviceObserver;

    { TDeviceObserver EventHandler }
    procedure OnObserverArrived(Sender: TObject; Value: PDEV_BROADCAST_DEVICEINTERFACE);
    procedure OnObserverRemoved(Sender: TObject; Value: PDEV_BROADCAST_DEVICEINTERFACE);
  private
    FComportName: String;
    FOnRemvoed: TProc;
    FOnArrived: TProc;

    function GetExists: Boolean;
  public
    constructor Create(const ADvcType: TDeviceType; const ADvcName: String); reintroduce;
    destructor Destroy; override;

    property ComportName: String read FComportName;
    property Exists: Boolean read GetExists;

    property OnArrived: TProc read FOnArrived write FOnArrived;
    property OnRemvoed: TProc read FOnRemvoed write FOnRemvoed;
  end;

implementation

{ TdvcObserver }

function TdvcObserver.GetExists: Boolean;
begin
  Result := FDvcList.Exists(
    function (AInfo: TWinDevice; AFindIdx: Integer): Boolean
    begin
      Result := AInfo.Description.Contains(FDvcName);
      if Result and FComportName.IsEmpty then
        FComportName := FriendlyNameToComport(AInfo.FriendlyName);
    end);
end;

constructor TdvcObserver.Create(const ADvcType: TDeviceType; const ADvcName: String);
begin
  inherited Create(nil);

  FDvcName := ADvcName;
  FDvcType := ADvcType;

  FObserver := TWinDeviceObserver.Create(Self);
  FObserver.AddNotifycation(GUID_DEVINTERFACE_COMPORT);
  FObserver.OnArrived := OnObserverArrived;
  FObserver.OnRemoved := OnObserverRemoved;

  FDvcList := TWinDeviceList.Create;
  FDvcList.ClearAndSearch([ADvcType.GUID]);
end;

destructor TdvcObserver.Destroy;
begin
  FreeAndNil(FObserver);

  inherited;
end;

procedure TdvcObserver.OnObserverArrived(Sender: TObject;
  Value: PDEV_BROADCAST_DEVICEINTERFACE);
var
  LDvcType: TDeviceType;
  LDvcDesc: String;
begin
  LDvcType := TDbDiTo.DeviceType(Value);
  LDvcDesc := TDbdiTo.DeviceDesc(Value);
  if LDVCtype.Equals(FDvcType) and LDvcDesc.Contains(FDvcName) then
  begin
    if FComportName.IsEmpty then
      FComportName := FriendlyNameToComport(TDbDiTo.FriendlyName(Value));
    if Assigned(FOnArrived) then
      FOnArrived;
  end
end;

procedure TdvcObserver.OnObserverRemoved(Sender: TObject;
  Value: PDEV_BROADCAST_DEVICEINTERFACE);
var
  LDvcType: TDeviceType;
  LDvcDesc: String;
begin
  LDvcType := TDbDiTo.DeviceType(Value);
  LDvcDesc := TDbdiTo.DeviceDesc(Value);
  if LDVCtype.Equals(FDvcType) and LDvcDesc.Contains(FDvcName) then
    if not FComportName.IsEmpty and FComportName.Equals(FriendlyNameToComport(TDbDiTo.FriendlyName(Value))) then
    begin
      FComportName := EmptyStr;
      if Assigned(FOnRemvoed) then
        FOnRemvoed;
    end;
end;

end.
