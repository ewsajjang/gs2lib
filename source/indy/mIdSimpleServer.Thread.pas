unit mIdSimpleServer.Thread;

interface

uses
  mIdSimpleServer,

  IdGlobal, IdTCPConnection, IdSimpleServer, IdExceptionCore, IdException,
  IdStack, IdSync,

  System.Classes, System.SysUtils,
  System.Generics.Collections, System.SyncObjs
  ;

type
  TIdSimpleSvrTh = class(TThread)
  private const
    NAME_SVR_PORT = 'ServerPort';
    NAME_SVR_LOCAL_IP = 'ServerLocalIP';
    NAME_CLIENT_TIMEOUT = 'ClientTimeOut';
    NAME_CLIENT_CONNECTED = 'ClientConnected';
    NAME_CLIENT_PEER_PORT = 'ClientPeerPort';
    NAME_CLIENT_PEER_IP = 'ClientPeerIP';
    SEC = 1000;
  private
    FLock: TIdCriticalSection;
    FSvr: TIdSimpleServerEx;
    FServerInfos: TStringList;
    FClientInfos: TStringList;
    FStatus: TSvrStatus;
    procedure OnBeforeBind(Sender: TObject);
    procedure OnAfterBind(Sender: TObject);
    procedure OnSvrDisconnected(Sender: TObject);
    procedure DoListen;
    procedure DoClientConnected;
    procedure DoClientDisconnect;
    function CheckForDataOnSource(const ATimeOut: Integer): Boolean;
    function WaitQueuePopupItem(var AItem: TBytesStream): Boolean;
  private
    FQueue: TThreadedQueue<TBytesStream>;
    FOnData: TProc<TIdBytes>;
    FOnError: TProc<Integer>;
    FOnEndListen: TProc<TSvrStatus>;
    FOnListen: TProc<TSvrStatus, TStringList>;
    FOnConnected: TProc<TSvrStatus, TStringList>;
    FOnDisconnected: TProc<TSvrStatus, TStringList>;
    function GetClientTimeout: Integer;
    function GetPort: Integer;
    class function GetLocalIP: String; static;
    function GetStatus: TSvrStatus;
  protected
    procedure TerminatedSet; override;
    procedure Execute; override;
  public
    constructor Create(const APort: Integer = 65501; const AClientTimeout: Integer = 15 * SEC); reintroduce;
    destructor Destroy; override;

    procedure Abort;
    procedure Send(const ABytes: TBytes);

    property ClientTimeout: Integer read GetClientTimeout default 15 * SEC;
    property Port: Integer read GetPort default 65501;
    class property LocalIP: String read GetLocalIP;
    property Status: TSvrStatus read GetStatus;

    property OnData: TProc<TIdBytes> read FOnData write FOnData;
    property OnListen: TProc<TSvrStatus, TStringList> read FOnListen write FOnListen;
    property OnEndListen: TProc<TSvrStatus> read FOnEndListen write FOnEndListen;
    property OnConnected: TProc<TSvrStatus, TStringList> read FOnConnected write FOnConnected;
    property OnDisconnected: TProc<TSvrStatus, TStringList> read FOnDisconnected write FOnDisconnected;
    property OnError: TProc<Integer> read FOnError write FOnError;
  end;

implementation

uses
  CodeSiteLogging, mCodeSiteHelper, mStringListHelper, System.TypInfo,
  mSysUtilsEx, mDateTimeHelper, System.DateUtils
  ;

{ TIdSimpleSvrTh }

procedure TIdSimpleSvrTh.OnAfterBind(Sender: TObject);
begin
  CodeSite.Send('OnAfterBind');
end;

procedure TIdSimpleSvrTh.OnBeforeBind(Sender: TObject);
begin
  CodeSite.Send('OnBeforeBind');
end;

procedure TIdSimpleSvrTh.OnSvrDisconnected(Sender: TObject);
begin
  FServerInfos.B[NAME_CLIENT_CONNECTED] := False;
  FStatus := ssListenDisconnected;
  CodeSite.Send('Client Disconnected');
    if Assigned(FOnDisconnected) then
      FOnDisconnected(FStatus, FClientInfos);
  FClientInfos.Clear;
end;

procedure TIdSimpleSvrTh.Abort;
begin
  if not FSvr.Aborted then
    try
      FSvr.Abort;
    except on E: Exception do
      CodeSite.SendError('Abort %s, %s', [E.ClassName, E.Message]);
    end;
end;

function TIdSimpleSvrTh.CheckForDataOnSource(const ATimeOut: Integer): Boolean;
var
  LStart: TDateTime;
begin
  Result := False;
  LStart := Now;
  while (LStart.MilliSecondsBetween(Now) < ATimeOut) and not Terminated do
  begin
    Result := FSvr.IOHandler.CheckForDataOnSource(3);
    if Result then
      Break;
  end;
end;

constructor TIdSimpleSvrTh.Create(const APort: Integer; const AClientTimeout: Integer);
begin
  inherited Create(True);

  FreeOnTerminate := False;

  FLock := TIdCriticalSection.Create;

  FClientInfos := TStringList.Create;

  FServerInfos := TStringList.Create;
  FServerInfos.I[NAME_SVR_PORT] := APort;
  FServerInfos.I[NAME_CLIENT_TIMEOUT] := AClientTimeout;
  FServerInfos.B[NAME_CLIENT_CONNECTED] := False;
  FServerInfos.S[NAME_SVR_LOCAL_IP] := LocalIP;

  FQueue := TThreadedQueue<TBytesStream>.Create;

  FSvr := TIdSimpleServerEx.Create(nil);
  FSvr.OnBeforeBind := OnBeforeBind;
  FSvr.OnAfterBind := OnAfterBind;
  FSvr.OnDisconnected := OnSvrDisconnected;
  FSvr.OnListen := procedure
  begin
    TIdNotify.NotifyMethod(DoListen);
  end;
  FSvr.OnEndListen := procedure
  begin
    CodeSite.Send('OnEndListen');
      if Assigned(FOnEndListen) then
        FOnEndListen(ssEndListen);
  end;
end;

destructor TIdSimpleSvrTh.Destroy;
begin
  if not FQueue.ShutDown then
    FQueue.DoShutDown;
  FreeAndNil(FQueue);
  FSvr.EndListen;
  FreeAndNil(FSvr);
  FreeAndNil(FServerInfos);
  FreeAndNil(FClientInfos);
  FreeAndNil(FLock);

  inherited;
end;

procedure TIdSimpleSvrTh.DoClientConnected;
begin
  FStatus := ssListenConnected;
  if Assigned(FOnConnected) then
    FOnConnected(FStatus, FClientInfos);
end;

procedure TIdSimpleSvrTh.DoClientDisconnect;
begin
  if Assigned(FOnDisconnected) then
    FOnDisconnected(ssListenDisconnected, FClientInfos);
end;

procedure TIdSimpleSvrTh.DoListen;
begin
  CodeSite.Send('OnListen');
  FStatus := ssListenDisconnected;
  if Assigned(FOnListen) then
    FOnListen(FStatus, FServerInfos);
end;

procedure TIdSimpleSvrTh.TerminatedSet;
begin
  inherited;

  Abort;
  if FSvr.Connected and FSvr.IOHandler.Connected then
    if FSVr.IOHandler.Opened then
      FSVr.IOHandler.Close;
end;

function TIdSimpleSvrTh.WaitQueuePopupItem(var AItem: TBytesStream): Boolean;
var
  LStart: TDateTime;
begin
  Result := False;
  LStart := Now;
  while (LStart.MinutesBetween(Now) < SEC) and not Terminated do
  begin
    Result := FQueue.QueueSize > 0;
    if Result then
    begin
      AItem := FQueue.PopItem;
      Break;
    end
    else
      IndySleep(3);
  end;
end;

procedure TIdSimpleSvrTh.Execute;
var
  LRcvBuffer: TIdBytes;
  LRcvSize: Integer;
  LSndBuffer: TBytesStream;
  LWaitSnd: Boolean;
  procedure DisconnectAndFlushQueue;
  begin
    try
      if FSVr.Connected then
        FSvr.Disconnect;
    except on E: Exception do
      CodeSite.SendError('TIdSimpleSvrTh.Execute.DisconnectAndFlushQueue -> %s %s', [E.ClassName, E.Message]);
    end;
    FSvr.CheckForGracefulDisconnect(False);
    TIdNotify.NotifyMethod(DoClientDisconnect);
    while FQueue.QueueSize > 0 do
      FQueue.PopItem.Free;
  end;
begin
  FSvr.BoundPort := Port;
  CodeSite.Send('TIdSimpleSvrTh Excute Port: %d, ClientTimeOut: %d', [
    Port,
    ClientTimeOut
  ]);
  while not Terminated do
  try
    FSvr.BeginListen;
    FSvr.Listen;
    try
      CodeSite.Send('Client Connected');
      FServerInfos.B[NAME_CLIENT_CONNECTED] := True;
      FClientInfos.Clear;
      FClientInfos.S[NAME_CLIENT_PEER_IP] := FSvr.Binding.PeerIP;
      FClientInfos.I[NAME_CLIENT_PEER_PORT] := FSvr.Binding.PeerPort;
      FClientInfos.I[NAME_SVR_PORT] := FSvr.Binding.Port;
      TIdNotify.NotifyMethod(DoClientConnected);
      while FSvr.Connected and not Terminated do
      begin
        if not CheckForDataOnSource(ClientTimeout) then
          DisconnectAndFlushQueue
        else if not FSvr.IOHandler.InputBufferIsEmpty then
        begin
          FSvr.IOHandler.ReadTimeout := 5;
          LRcvSize := FSvr.IOHandler.InputBuffer.Size;
          FSvr.IOHandler.ReadBytes(LRcvBuffer, LRcvSize, False);
          SetLength(LRcvBuffer, LRcvSize);
          LWaitSnd := not BytesToHexStr(LRcvBuffer).Contains('4004107060AA');
          TRcvNotify.Execute(FOnData, LRcvBuffer);
          if LWaitSnd and WaitQueuePopupItem(LSndBuffer) then
          begin
            IndySleep(50);
            LSndBuffer.Seek(0, soFromBeginning);
            FSvr.IOHandler.WriteBufferClear;
            FSvr.IOHandler.Write(LSndBuffer);
            FreeAndNil(LSndBuffer);
          end
          else
          begin
            DisconnectAndFlushQueue;
            Break;
          end;
        end;
        IndySleep(1);
        FSvr.CheckForGracefulDisconnect;
      end;
      IndySleep(1);
    finally
      DisconnectAndFlushQueue;
    end;
  except
    on E: Exception do
    begin
      if E is EIdAcceptTimeout then
      begin
        CodeSite.SendError('%s, %s', [E.ClassName, E.Message]);
        FSvr.EndListen;
      end
      else if E is EIdCouldNotBindSocket then
      begin
        CodeSite.SendError('%s, %s', [E.ClassName, E.Message]);
        raise E
      end
      else
        CodeSite.SendError('SimpleSvrTh Exception %s, %s', [E.ClassName, E.Message]);
    end;
  end;
end;

function TIdSimpleSvrTh.GetClientTimeout: Integer;
begin
  Result := FServerInfos.I[NAME_CLIENT_TIMEOUT];
end;

class function TIdSimpleSvrTh.GetLocalIP: String;
begin
  TIdStack.IncUsage;
  try
    Result := GStack.LocalAddress
  finally
    TIdStack.DecUsage;
  end;
end;

function TIdSimpleSvrTh.GetPort: Integer;
begin
  FLock.Enter;
  try
    Result := FServerInfos.I[NAME_SVR_PORT];
  finally
    FLock.Leave;
  end;
end;

function TIdSimpleSvrTh.GetStatus: TSvrStatus;
begin
  FLock.Enter;
  try
    Result := FStatus;
  finally
    FLock.Leave;
  end;
end;

procedure TIdSimpleSvrTh.Send(const ABytes: TBytes);
var
  LBuffer: TBytesStream;
begin
  if FSvr.Connected and FServerInfos.B[NAME_CLIENT_CONNECTED] then
  begin
    LBuffer := TBytesStream.Create(ABytes);
    CodeSite.Send('Snd', ABytes);
    FQueue.PushItem(LBuffer);
  end;
end;

end.
