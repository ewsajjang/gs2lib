unit mIdSimpleServerThread;

interface

uses
  IdGlobal, IdTCPConnection, IdSimpleServer, IdExceptionCore, IdException,
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

    property Abroted: Boolean read FAbortedRequested;

    property OnListen: TProc read FOnListen write FOnListen;
    property OnEndListen: TProc read FOnEndListen write FOnEndListen;
  end;

  TIdSimpleSvrTh = class(TThread)
  private
    FLock: TIdCriticalSection;
    FSvr: TIdSimpleServerEx;
    procedure OnBeforeBind(Sender: TObject);
    procedure OnAfterBind(Sender: TObject);
    procedure OnDisconnected(Sender: TObject);
  private
    FClientConnected: Boolean;
    FQueue: TThreadedQueue<TStream>;
    FOnData: TProc<TIdBytes>;
    FOnError: TProc<Integer>;
    FPort: Integer;
    FOnEndListen: TProc;
    FOnListen: TProc;
  protected
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure Abort;
    procedure Send(const ABytes: TBytes);

    property Port: Integer read FPort write FPort default 65501;

    property OnData: TProc<TIdBytes> read FOnData write FOnData;
    property OnListen: TProc read FOnListen write FOnListen;
    property OnEndListen: TProc read FOnEndListen write FOnEndListen;
    property OnError: TProc<Integer> read FOnError write FOnError;
  end;

implementation

uses
  CodeSiteLogging, mCodeSiteHelper
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

{ TIdSimpleSvrTh }

procedure TIdSimpleSvrTh.OnAfterBind(Sender: TObject);
begin
  CodeSite.Send('OnAfterBind');
end;

procedure TIdSimpleSvrTh.OnBeforeBind(Sender: TObject);
begin
  CodeSite.Send('OnBeforeBind');
end;

procedure TIdSimpleSvrTh.OnDisconnected(Sender: TObject);
begin
  FClientConnected := False;
  CodeSite.Send('Client Disconnected');
end;

procedure TIdSimpleSvrTh.Abort;
begin
  if not FSvr.Abroted then
    try
      FSvr.Abort;
    except on E: Exception do
      CodeSite.SendError('Abort %s, %s', [E.ClassName, E.Message]);
    end;
end;

constructor TIdSimpleSvrTh.Create;
begin
  inherited Create(True);

  FreeOnTerminate := False;

  FLock := TIdCriticalSection.Create;

  FSvr := TIdSimpleServerEx.Create(nil);
  FSvr.BoundPort := FPort;
  FSvr.OnBeforeBind := OnBeforeBind;
  FSvr.OnAfterBind := OnAfterBind;
  FSvr.OnDisconnected := OnDisconnected;
  FSvr.OnListen := procedure
  begin
    CodeSite.Send('OnListen');
    FLock.Enter;
    try
      if Assigned(FOnListen) then
        FOnListen;
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
        FOnEndListen;
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
  FreeAndNil(FLock);

  inherited;
end;

procedure TIdSimpleSvrTh.Execute;
const
  SEC = 1000;
var
  LRcv: TIdBytes;
  LSndBuffer: TStream;
  procedure DisconnectClientAndFlushQueue;
  begin
    FSvr.Disconnect;
    while FQueue.QueueSize > 0 do
      FQueue.PopItem.Free;
  end;
begin
  while not Terminated do
  try
    FSvr.BeginListen;
    FSvr.Listen;
    try
      CodeSite.Send('Client Connected');
      FClientConnected := True;
      while FSvr.Connected and not Terminated do
      begin
        FSvr.IOHandler.CheckForDisconnect;
        FSvr.IOHandler.CheckForDataOnSource(2 * SEC);
        if FSvr.IOHandler.InputBufferIsEmpty then
//          DisconnectClientAndFlushQueue
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
