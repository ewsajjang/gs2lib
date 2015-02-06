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
    NAME_PORT = 'PORT';
    NAME_LOCAL_IP = 'LocalIP';
    NAME_CLIENT_TIMEOUT = 'ClientTimeOut';
    NAME_CLIENT_CONNECTED = 'ClientConnected';
    SEC = 1000;
  private
    FSvr: TIdSimpleServerEx;
    FServerInfos: TStringList;
    FClientInfos: TStringList;
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
    function GetLocalIP: String;
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
    property LocalIP: String read GetLocalIP;

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
  CodeSite.Send('Client Disconnected');
    if Assigned(FOnDisconnected) then
      FOnDisconnected(ssListenDisconnected, FClientInfos);
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

  FClientInfos := TStringList.Create;

  FServerInfos := TStringList.Create;
  FServerInfos.I[NAME_PORT] := APort;
  FServerInfos.I[NAME_CLIENT_TIMEOUT] := AClientTimeout;
  FServerInfos.B[NAME_CLIENT_CONNECTED] := False;
  TIdStack.IncUsage;
  try
    FServerInfos.S[NAME_LOCAL_IP] := GStack.LocalAddress
  finally
    TIdStack.DecUsage;
  end;

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
  FreeAndNil(FSvr);
  FreeAndNil(FServerInfos);
  FreeAndNil(FClientInfos);

  inherited;
end;

procedure TIdSimpleSvrTh.DoClientConnected;
begin
  if Assigned(FOnConnected) then
    FOnConnected(ssListenConnected, FClientInfos);
end;

procedure TIdSimpleSvrTh.DoClientDisconnect;
begin
  if Assigned(FOnDisconnected) then
    FOnDisconnected(ssListenDisconnected, FClientInfos);
end;

procedure TIdSimpleSvrTh.DoListen;
begin
  CodeSite.Send('OnListen');
  if Assigned(FOnListen) then
    FOnListen(ssListenDisconnected, FServerInfos);
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
  while not Terminated do
  try
    FSvr.BeginListen;
    FSvr.Listen;
    try
      CodeSite.Send('Client Connected');
      FServerInfos.B[NAME_CLIENT_CONNECTED] := True;
      FClientInfos.Clear;
      FClientInfos.S['PeerIP'] := FSvr.Binding.PeerIP;
      FClientInfos.I['PeerPort'] := FSvr.Binding.PeerPort;
      FClientInfos.I['Port'] := FSvr.Binding.Port;
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

function TIdSimpleSvrTh.GetLocalIP: String;
begin
  Result := FServerInfos.S[NAME_LOCAL_IP]
end;

function TIdSimpleSvrTh.GetPort: Integer;
begin
  Result := FServerInfos.I[NAME_PORT]
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
