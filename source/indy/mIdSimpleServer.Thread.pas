unit mIdSimpleServerThread;

interface

uses
  IdGlobal, IdTCPConnection, IdSimpleServer, IdExceptionCore, IdException,
  IdStack,
  System.Classes, System.SysUtils,
  System.Generics.Collections, System.SyncObjs
  ;

type
  TIdSimpleServerEx = class(TIdSimpleServer)
  private
    FOnListen: TProc;
    FOnEndListen: TProc;
  public
    procedure EndListen; override;
    procedure Listen(ATimeout: Integer = IdTimeoutDefault); override;

    property Aborted: Boolean read FAbortedRequested;

    property OnListen: TProc read FOnListen write FOnListen;
    property OnEndListen: TProc read FOnEndListen write FOnEndListen;
  end;

  TSvrStatus = (ssEndListen, ssListenDisconnected, ssListenConnected);
  TSvrStatusHelper = record helper for TSvrStatus
    function Str: String;
  end;

  TIdSimpleSvrTh = class(TThread)
  private
    FLock: TIdCriticalSection;
    FSvr: TIdSimpleServerEx;
    FClientInfos: TStringList;
    procedure OnBeforeBind(Sender: TObject);
    procedure OnAfterBind(Sender: TObject);
    procedure OnSvrDisconnected(Sender: TObject);
  private
    FClientConnected: Boolean;
    FQueue: TThreadedQueue<TStream>;
    FOnData: TProc<TIdBytes>;
    FOnError: TProc<Integer>;
    FPort: Integer;
    FOnEndListen: TProc<TSvrStatus>;
    FOnListen: TProc<TSvrStatus, String>;
    FOnConnected: TProc<TSvrStatus, TStringList>;
    FOnDisconnected: TProc<TSvrStatus, TStringList>;
    FClientTimeout: Integer;
    procedure DoClientConnected;
  protected
    procedure TerminatedSet; override;
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure Abort;
    procedure Send(const ABytes: TBytes);

    property ClientTimeout: Integer read FClientTimeout write FClientTimeout;
    property Port: Integer read FPort write FPort default 65501;

    property OnData: TProc<TIdBytes> read FOnData write FOnData;
    property OnListen: TProc<TSvrStatus, String> read FOnListen write FOnListen;
    property OnEndListen: TProc<TSvrStatus> read FOnEndListen write FOnEndListen;
    property OnConnected: TProc<TSvrStatus, TStringList> read FOnConnected write FOnConnected;
    property OnDisconnected: TProc<TSvrStatus, TStringList> read FOnDisconnected write FOnDisconnected;
    property OnError: TProc<Integer> read FOnError write FOnError;
  end;

implementation

uses
  CodeSiteLogging, mCodeSiteHelper, mStringListHelper, System.TypInfo
  ;

{ TIdSimpleServerEx }

procedure TIdSimpleServerEx.EndListen;
var
  LIsListen: Boolean;
begin
  LIsListen := FListening;

  inherited EndListen;

  if LIsListen then
    if Assigned(FOnEndListen) then
      FOnEndListen;
end;

procedure TIdSimpleServerEx.Listen(ATimeout: Integer);
begin
  if Assigned(FOnListen) then
    FOnListen;

  inherited Listen(ATimeOut);
end;

{ TSvrStatusHelper }

function TSvrStatusHelper.Str: String;
begin
  Result := GetEnumName(TypeInfo(TSvrStatus), Integer(Self));
end;

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
  FClientConnected := False;
  CodeSite.Send('Client Disconnected');
  FLock.Enter;
  try
    if Assigned(FOnDisconnected) then
      FOnDisconnected(ssListenDisconnected, FClientInfos);
  finally
    FLock.Leave;
  end;
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

constructor TIdSimpleSvrTh.Create;
const
  SEC = 1000;
begin
  inherited Create(True);

  FreeOnTerminate := False;

  FLock := TIdCriticalSection.Create;

  FClientInfos := TStringList.Create;

  FPort := 65501;
  FClientTimeOut := 15 * SEC;
  FSvr := TIdSimpleServerEx.Create(nil);
  FSvr.OnBeforeBind := OnBeforeBind;
  FSvr.OnAfterBind := OnAfterBind;
  FSvr.OnDisconnected := OnSvrDisconnected;
  FSvr.OnListen := procedure
  var
    LIPAddr: String;
  begin
    CodeSite.Send('OnListen');
    TIdStack.IncUsage;
    try
      LIPAddr := GStack.LocalAddress
    finally
      TIdStack.DecUsage;
    end;
    FLock.Enter;
    try
      if Assigned(FOnListen) then
        FOnListen(ssListenDisconnected, LIPAddr + ':' + FPort.ToString);
    finally
      FLock.Leave;
    end;
  end;
  FSvr.OnEndListen := procedure
  begin
    CodeSite.Send('OnEndListen');
    FLock.Enter;
    try
      if Assigned(FOnEndListen) then
        FOnEndListen(ssEndListen);
    finally
      FLock.Leave;
    end;
  end;

  FQueue := TThreadedQueue<TStream>.Create;

  FClientConnected := False;
end;

destructor TIdSimpleSvrTh.Destroy;
begin
  if not FQueue.ShutDown then
    FQueue.DoShutDown;
  FreeAndNil(FQueue);
  FreeAndNil(FSvr);
  FreeAndNil(FClientInfos);
  FreeAndNil(FLock);

  inherited;
end;

procedure TIdSimpleSvrTh.DoClientConnected;
begin
  FLock.Enter;
  try
    if Assigned(FOnConnected) then
      FOnConnected(ssListenConnected, FClientInfos);
  finally
    FLock.Leave;
  end;
end;

procedure TIdSimpleSvrTh.TerminatedSet;
begin
  inherited;

  Abort;
  if FSvr.Connected and FSvr.IOHandler.Connected then
    if FSVr.IOHandler.Opened then
      FSVr.IOHandler.Close;
end;

procedure TIdSimpleSvrTh.Execute;
var
  LRcv: TIdBytes;
  LSndBuffer: TStream;
  procedure DisconnectClientAndFlushQueue;
  begin
    FSvr.Disconnect;
    while FQueue.QueueSize > 0 do
      FQueue.PopItem.Free;
    FLock.Enter;
    try
      if Assigned(FOnDisconnected) then
        FOnDisconnected(ssListenDisconnected, FClientInfos);
    finally
      FLock.Leave;
    end;
  end;
begin
  FSvr.BoundPort := FPort;
  while not Terminated do
  try
    FSvr.BeginListen;
    FSvr.Listen;
    try
      CodeSite.Send('Client Connected');
      FClientConnected := True;
      FClientInfos.Clear;
      FClientInfos.S['PeerIP'] := FSvr.Binding.PeerIP;
      FClientInfos.I['PeerPort'] := FSvr.Binding.PeerPort;
      FClientInfos.I['Port'] := FSvr.Binding.Port;
      DoClientConnected;
      while FSvr.Connected and not Terminated do
      begin
        FSvr.IOHandler.CheckForDisconnect;
        FSvr.IOHandler.CheckForDataOnSource(FClientTimeOut);
        if FSvr.IOHandler.InputBufferIsEmpty then
          DisconnectClientAndFlushQueue
        else
        begin
          FSvr.IOHandler.InputBuffer.ExtractToBytes(LRcv);
          if Assigned(FOnData) then
          begin
            FLock.Enter;
            try
              FOnData(LRcv);
            finally
              FLock.Leave;
            end;
          end;
          SetLength(LRcv, 0);
          FSvr.IOHandler.InputBuffer.Clear;
        end;
        IndySleep(1);
        if FSvr.Connected and (FQueue.QueueSize > 0) then
        begin
          LSndBuffer := FQueue.PopItem;
          LSndBuffer.Seek(0, soFromBeginning);
          FSvr.IOHandler.WriteBufferClear;
          FSvr.IOHandler.Write(LSndBuffer);
          FreeAndNil(LSndBuffer);
        end;
      end;
      IndySleep(1);
    finally
      DisconnectClientAndFlushQueue;
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

procedure TIdSimpleSvrTh.Send(const ABytes: TBytes);
var
  LBuffer: TBytesStream;
begin
  if FSvr.Connected then
    if FClientConnected then
    begin
      LBuffer := TBytesStream.Create(ABytes);
      CodeSite.Send('Snd', ABytes);
      FQueue.PushItem(LBuffer);
    end;
end;

end.
