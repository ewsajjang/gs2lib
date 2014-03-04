unit _v.Main;

interface

uses
  mDeviceObserver.Windows.Common, mDeviceObserver.Windows,
  mDeviceList.Windows, mDeviceList.Windows.Common, mDevice.Windows,

  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TvMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDvcOb: TWinDeviceObserver;
    procedure OnNodeChange(Sender: TObject; Node: Integer);
    procedure OnArrived(Sender: TObject; Value: PDEV_BROADCAST_DEVICEINTERFACE);
    procedure OnRemoved(Sender: TObject; Value: PDEV_BROADCAST_DEVICEINTERFACE);
  public
  end;

var
  vMain: TvMain;

implementation

{$R *.dfm}

uses
  mLog.CodeSite

  ;

procedure TvMain.FormCreate(Sender: TObject);
begin
  FDvcOb := TWinDeviceObserver.Create(Self);
  FDvcOb.AddNotifycation(GUID_Ports);
  FDvcOb.AddNotifycation(GUID_USB);
  FDvcOb.AddNotifycation(GUID_Net);
  FDvcOb.OnNodeChange := OnNodeChange;
  FDvcOb.OnArrived := OnArrived;
  FDvcOb.OnRemoved := OnRemoved;
end;

procedure TvMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDvcOb);
end;

procedure TvMain.OnArrived(Sender: TObject;
  Value: PDEV_BROADCAST_DEVICEINTERFACE);
var
  LInfos: TStringList;
  LItem: String;
begin
  Log.Enter('OnArrived');
  LInfos := TDbDiTo.Infos(Value);
  try
    for LItem in LInfos do
      Log.Msg(LItem);
  finally
    FreeAndNil(LInfos);
  end;
  Log.Exit('OnArrived');
end;

procedure TvMain.OnNodeChange(Sender: TObject; Node: Integer);
begin
  Log.Msg('OnNodeChange.Node %d', [Node]);
end;

procedure TvMain.OnRemoved(Sender: TObject;
  Value: PDEV_BROADCAST_DEVICEINTERFACE);
var
  LInfos: TStringList;
  LItem: String;
begin
  Log.Enter('OnRemoved');
  LInfos := TDbDiTo.Infos(Value);
  try
    for LItem in LInfos do
      Log.Msg(LItem);
  finally
    FreeAndNil(LInfos);
  end;
  Log.Exit('OnRemoved');
end;

end.
