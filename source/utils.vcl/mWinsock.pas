unit mWinsock;

interface

uses
  System.Classes, System.SysUtils,
  Winapi.Winsock2, Winapi.Winsock, Winapi.windows, Winapi.Messages,
  System.Generics.Collections
  ;

type
  EWinsocket = class(Exception);

  TWinsock = record
  private
    class var FErCode: Integer;
    class var FErMsg: String;
    class var FData: TWSAData;
    class var FMaxSocket: Word;
    class var FActive: Boolean;
    class var FMaxUdpDg: Word;
    class function GetDesription: String; static;
    class function GetHighVersion: String; static;
    class function GetStatus: String; static;
    class function GetVersion: String; static;
    class function GetErMsg: String; static;
  public
    class function Startup: Boolean; static;
    class procedure Cleanup; static;

    class function IsPortOpen(const AHost: String; const APort: Integer): Boolean; static;

    class function Excuete(const AErCode: Integer; const AOperation: String; const ARaise: Boolean = True): Boolean; static;

    class property Active: Boolean read FActive;
    class property ErMsg: String read GetErMsg;

    class property Version: String read GetVersion;
    class property HighVersion: String read GetHighVersion;
    class property Description: String read GetDesription;
    class property Status: String read GetStatus;
    class property MaxSocket: Word read FMaxSocket;
    class property MaxUdpDg: Word read FMaxUdpDg;
  end;

  EClient = class(Exception);

  TCustomSocket = class
  private
    procedure WndProc(var Message: TMessage);
  protected type
    TShutodownHow = (shRcv = 0, shSnd);
    TShutodownHowSet = set of TShutodownHow;
  protected
    FSocket: TSocket;
    FShudownStatus: TShutodownHowSet;
    FPort: Integer;
    FHost: String;
    FAddr: TSockAddrIn;
    FHandle: THandle;
    function InitAddr(const AHost: String; const APort: Integer; const AClient: Boolean = True): TSockAddrIn;
    procedure InitSocket;
    procedure ShutdownSend;
    procedure ShutdownRecv;
    procedure ShutdownBoth;
    procedure CleanUp;
    procedure SendAndFree(AStream: TStream);
    function GetHandle: THandle;
  public
    constructor Create;
    destructor Destroy; override;

    property Handle: THandle read GetHandle;
  end;

  TCustomClientSocket = class(TCustomSocket)
  public
    function CanRead(const ATimeOut: Integer = 0): Boolean;
    procedure Send(AFunc: TFunc<TStream>);
    function ReceiveUntilDrop: TStringStream; overload;
  end;

  TBlockClientSocket = class(TCustomClientSocket)
  private
  strict private
    FConnected: Boolean;
  protected
    procedure DoAfterConnect; virtual;
    procedure DoShutdown; virtual;
    procedure DoAfterDisconnect; virtual;
  public
    procedure Open(const AHost: String; const APort: Integer); virtual;
    procedure Close; virtual;
    function SendThenReceiveDrop(AFunc: TFunc<TStream>): TStringStream; virtual;

    property Connected: Boolean read FConnected;
  end;

const
  WM_WINSOCK_ASYNC = WM_USER + $0001;

type
  TAsyncClientSocket = class(TBlockClientSocket)
  private
    FOnConnected: TProc;
    FOnDisconnect: TProc;
    FOnReceive: TProc<TStringStream>;
    FOnSend: TProc;
    procedure wmWinSockAsync(var AMsg: TMessage); message WM_WINSOCK_ASYNC;
  protected
    procedure DoAfterConnect; override;
    procedure DoAfterDisconnect; override;
  public
    procedure Open(const AHost: String; const APort: Integer); override;
    procedure Close; override;
    procedure SendThenReceiveDrop(AFunc: TFunc<TStream>); reintroduce;

    property OnConnected: TProc read FOnConnected write FOnConnected;
    property OnDisconnect: TProc read FOnDisconnect write FOnDisconnect;
    property OnReceive: TProc<TStringStream> read FOnReceive write FOnReceive;
    property OnSend: TProc read FOnSend write FOnSend;
  end;

type
  TServerCustomClientSocket = class(TCustomClientSocket)
  public
    constructor Create(const ASocket: TSocket); reintroduce;
  end;

  TServerSocket = class(TCustomSocket)
  strict private
    FClients: TObjectDictionary<TSocket, TServerCustomClientSocket>;
    FOnGetClientSocket: TFunc<TServerCustomClientSocket>;
    FOnRead: TProc<TServerCustomClientSocket, TStringStream>;
    FOnWrite: TProc<TServerCustomClientSocket>;
    procedure wmWinSockAsync(var AMsg: TMessage); message WM_WINSOCK_ASYNC;
  private
    FOnClientConnected: TProc<TServerCustomClientSocket>;
    FOnClientDisconnected: TProc<TServerCustomClientSocket>;
  protected
    FPort: Integer;
    procedure AcceptClient(const AClientSocket: TSocket);
    procedure ReadClient(const AClientSocket: TSocket); virtual;
    procedure CloseClient(const AClientSocket: TSocket);
    procedure CloseServer;
    procedure WriteClient(const AClientSocket: TSocket);
  public
    procedure Listen(const APort: Integer);
    procedure Close;
    procedure Send(const AClientSocket: TServerCustomClientSocket; AFunc: TFunc<TStream>);

    property OnGetClientSocket: TFunc<TServerCustomClientSocket> read FOnGetClientSocket write FOnGetClientSocket;
    property OnRead: TProc<TServerCustomClientSocket, TStringStream> read FOnRead write FOnRead;
    property OnWrite: TProc<TServerCustomClientSocket> read FOnWrite write FOnWrite;
    property OnClientConnected: TProc<TServerCustomClientSocket> read FOnClientConnected write FOnClientConnected;
    property OnClientDisconnected: TProc<TServerCustomClientSocket> read FOnClientDisconnected write FOnClientDisconnected;
  end;

function GetKeepAliveStatus(Socket: NativeInt; OptVal: PInteger): Boolean;
function SetKeepAliveStatus(Socket: NativeInt; OptVal: Integer): Boolean;
function SetKeepAliveValue(Socket: NativeInt; OnOff, Time, Interval: Integer): Boolean;


implementation

uses
  System.RTLConsts
  ;

threadvar
  SocketErrorProc: TProc<Integer, String>;

function GetKeepAliveStatus(Socket: NativeInt; OptVal: PInteger): Boolean;
var
  Len: Integer;
begin
  Len:= SizeOf(OptVal^);
  Result := GetSockOpt(Socket, SOL_SOCKET, SO_KEEPALIVE,  @(OptVal^), Len) = 0;
end;

function SetKeepAliveStatus(Socket: NativeInt; OptVal: Integer): Boolean;
begin
  Result := SetSockOpt(Socket, SOL_SOCKET, SO_KEEPALIVE,  @OptVal, SizeOf(OptVal)) = 0;
end;

function SetKeepAliveValue(Socket: NativeInt; OnOff, Time, Interval: Integer): Boolean;
const
  SIO_KEEPALIVE_VALS = IOC_IN or IOC_VENDOR or 4;
type
  TTcpKeepAlive= packed record
    OnOff,
    KeepAliveTime,
    KeepAliveInterval: Cardinal;
  end;
var
  KeepAliveIn: TTcpKeepAlive;
  BytesReturned: Cardinal;
begin
  FillChar(KeepAliveIn, SizeOf(KeepAliveIn), 0);
  KeepAliveIn.OnOff:= OnOff;
  KeepAliveIn.KeepAliveTime:= Time;
  KeepAliveIn.KeepAliveInterval:= Interval;
  Result := WSAIoctl(Socket, SIO_KEEPALIVE_VALS,  @KeepAliveIn, SizeOf(KeepAliveIn), nil, 0, &BytesReturned, nil, nil) = 0;
end;

{ TWinsock }

class procedure TWinsock.Cleanup;
begin
  if FActive then
  begin
    FErCode := WSACleanUp;
    if FErcode <> 0 then
      FErMsg := Format(sWindowsSocketError, [SysErrorMessage(FErCode), FErCode, 'WSACleanup']);
    FActive := False;
  end;
end;

class function TWinsock.Excuete(const AErCode: Integer;
  const AOperation: String; const ARaise: Boolean): Boolean;
begin
  Result := AErCode = 0;
  if not Result then
  begin
    FErCode := WSAGetLastError;
    if FErCode = 0 then
      Exit(True);
    if FErCode <> WSAEWOULDBLOCK then
      FErMsg := Format(sWindowsSocketError, [SysErrorMessage(FErCode), FErCode, AOperation]);
    if Assigned(SocketErrorProc) then
      SocketErrorProc(FErCode, FErMsg)
    else if ARaise then
      raise Exception.Create(FErMsg);
  end;
end;

class function TWinsock.GetDesription: String;
begin
  Result := String(FData.szDescription);
end;

class function TWinsock.GetErMsg: String;
begin
  if FErCode = 0 then
    Result := ''
  else
    Result := FErMsg;
end;

class function TWinsock.GetHighVersion: String;
begin
  Result := Format('%d.%d', [(FData.wHighVersion shr 8) and  $FF, FData.wHighVersion and $FF]);
end;

class function TWinsock.GetStatus: String;
begin
  Result := String(FData.szSystemStatus);
end;

class function TWinsock.GetVersion: String;
begin
  Result := Format('%d.%d', [(FData.wVersion shr 8) and  $FF, FData.wVersion and $FF]);
end;

class function TWinsock.IsPortOpen(const AHost: String;
  const APort: Integer): Boolean;
var
  LSocket: TBlockClientSocket;
begin
  Result := False;
  if StartUp then
  try
    LSocket := TBlockClientSocket.Create;
    try
      try
        LSocket.Open(AHost, APort);
      except on E: Exception do
        Exit(False);
      end;
      Result := LSocket.Connected;
      if Result then
        LSocket.Close;
    finally
      FreeAndNil(LSocket);
    end;
  finally
    CleanUp;
  end;
end;

class function TWinsock.Startup: Boolean;
begin
  FErCode := WSAStartup($0101, FData);
  FActive := FErCode = 0;
  if not FActive then
  begin
    FErMsg := Format(sWindowsSocketError, [SysErrorMessage(FErCode), FErCode, 'WSAStartup']);
    FillChar(FData, SizeOf(FData), 0);
  end
  else
  begin
    FMaxSocket := FData.iMaxSockets;
    FMaxUdpDg := FData.iMaxUdpDg;
  end;
  Result := FActive;
end;

{ TCustomSocket }

procedure TCustomSocket.SendAndFree(AStream: TStream);
var
  LBuf: array[0..4096] of Byte;
  LBufLen: Integer;
  LSndTotal, LSnd: Integer;
begin
  try
    LSndTotal := 0;
    while LSndTotal < AStream.Size do
    begin
      AStream.Position := LSndTotal;
      LBufLen := AStream.Read(LBuf, SizeOf(LBuf));
      LSnd := Winapi.WinSock.send(FSocket, LBuf, LBufLen, 0);
      if LSnd > 0 then
        Inc(LSndTotal, LSnd)
      else
        raise Exception.CreateFmt('Send failed: %d', [WSAGetLastError]);
    end;
  finally
    FreeAndNil(AStream);
  end;
end;

procedure TCustomSocket.CleanUp;
begin
  FSocket := INVALID_SOCKET;
  FAddr.sin_family := PF_INET;
  FAddr.sin_addr.s_addr := INADDR_ANY;
  FAddr.sin_port := 0;
end;

constructor TCustomSocket.Create;
begin
  FHandle := 0;
end;

destructor TCustomSocket.Destroy;
begin
  if FHandle <> 0 then
    DeallocateHWnd(FHandle);

  inherited;
end;

function TCustomSocket.GetHandle: THandle;
begin
  if FHandle = 0 then
    FHandle := AllocateHWnd(WndProc);
  Result := FHandle;
end;

function TCustomSocket.InitAddr(const AHost: String; const APort: Integer; const AClient: Boolean): TSockAddrIn;
  function LookupName(const Name: string; var InAddr: TInAddr): Boolean;
  var
    LHostEnt: PHostEnt;
  begin
    LHostEnt := gethostbyname(PAnsiChar(AnsiString(Name)));
    Result := Assigned(LHostEnt);
    if Result then
    begin
      InAddr.S_un_b.s_b1 := LHostEnt^.h_addr^[0];
      InAddr.S_un_b.s_b2 := LHostEnt^.h_addr^[1];
      InAddr.S_un_b.s_b3 := LHostEnt^.h_addr^[2];
      InAddr.S_un_b.s_b4 := LHostEnt^.h_addr^[3];
    end;
  end;
var
  LInAddr: TInAddr;
begin
  FHost := AHost;
  FPort := APort;
  Result.sin_family := PF_INET;
  Result.sin_port := hToNs(APort);
  if not AClient then
    Result.sin_addr.S_addr := INADDR_ANY
  else if LookupName(AHost, LInAddr) then
    Result.sin_addr := LInAddr
  else
    Result.sin_addr.S_addr := inet_addr(PAnsiChar(AnsiString(AHost)));
end;

procedure TCustomSocket.InitSocket;
begin
  FSocket := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
  if FSocket = INVALID_SOCKET then
    raise EClient.CreateRes(@sCannotCreateSocket);
  FShudownStatus := [];
end;

procedure TCustomSocket.ShutdownBoth;
begin
  TWinsock.Excuete(shutdown(FSocket, SD_BOTH), 'shutdown');
end;

procedure TCustomSocket.ShutdownRecv;
begin
  TWinsock.Excuete(shutdown(FSocket, SD_RECEIVE), 'shutdown');
end;

procedure TCustomSocket.ShutdownSend;
begin
  TWinsock.Excuete(shutdown(FSocket, SD_SEND), 'shutdown');
end;

procedure TCustomSocket.WndProc(var Message: TMessage);
begin
  try
    Dispatch(Message);
  except
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(Self);
  end;
end;

{ TCustomClientSocket }

function TCustomClientSocket.ReceiveUntilDrop: TStringStream;
var
  LBuf: array[0..4096] of Byte;
  LRcv: Integer;
begin
  Result := TStringStream.Create;
  FillChar(LBuf, SizeOf(LBuf), 0);
  repeat
    LRcv := recv(FSocket, LBuf, SizeOf(LBuf), 0);
    if LRcv > 0 then
      Result.Write(LBuf, LRcv)
    else if LRcv = SOCKET_ERROR then
      raise Exception.CreateFmt('Receive failed: %d', [WSAGetLastError]);
  until not CanRead;
end;

function TCustomClientSocket.CanRead(const ATimeOut: Integer): Boolean;
var
  LFDSet: TFDSet;
  LTimeVal: TTimeVal;
begin
  FD_ZERO(LFDSet);
  FD_SET(FSocket, LFDSet);
  LTimeVal.tv_sec := ATimeout div 1000;
  LTimeVal.tv_usec := (ATimeout mod 1000) * 1000;
  Result := select(0, @LFDSet, nil, nil, @LTimeVal) > 0;
end;

procedure TCustomClientSocket.Send(AFunc: TFunc<TStream>);
begin
  SendAndFree(AFunc);
end;

{ TClient }

procedure TBlockClientSocket.DoAfterConnect;
begin
  FConnected := True;
end;

procedure TBlockClientSocket.DoAfterDisconnect;
begin
  CleanUp;
  FConnected := False;
end;

procedure TBlockClientSocket.DoShutdown;
begin
  if (FSocket <> INVALID_SOCKET) and FConnected then
  begin
    if FShudownStatus = [] then ShutdownBoth
    else if shSnd in FShudownStatus then ShutdownRecv
    else ShutdownSend;
  end;
end;

procedure TBlockClientSocket.Close;
begin
  DoShutdown;
  if TWinsock.Excuete(closesocket(FSocket), 'closesocket') then
    DoAfterDisconnect;
end;

procedure TBlockClientSocket.Open(const AHost: String; const APort: Integer);
begin
  InitSocket;
  FAddr := InitAddr(AHost, APort);
  if TWinsock.Excuete(connect(FSocket, FAddr, SizeOf(FAddr)), 'connect') then
    DoAfterConnect;
end;

function TBlockClientSocket.SendThenReceiveDrop(AFunc: TFunc<TStream>): TStringStream;
begin
  SendAndFree(AFunc);
  ShutdownSend;
  Result := ReceiveUntilDrop;
  Close;
end;

{ TAsyncClientSocket }

procedure TAsyncClientSocket.DoAfterConnect;
begin
  inherited;
  if Assigned(FOnConnected) then
    FOnConnected;
end;

procedure TAsyncClientSocket.DoAfterDisconnect;
begin
  inherited ;

  if Assigned(FOnDisconnect) then
    FOnDisconnect;
end;

procedure TAsyncClientSocket.Close;
begin
  closesocket(FSocket);
end;

procedure TAsyncClientSocket.Open(const AHost: String; const APort: Integer);
begin
  InitSocket;
  WSAAsyncSelect(FSocket, Handle, WM_WINSOCK_ASYNC, FD_CONNECT + FD_READ + FD_WRITE + FD_CLOSE);
  FAddr := InitAddr(AHost, APort);
  connect(FSocket, FAddr, SizeOf(FAddr));
end;

procedure TAsyncClientSocket.SendThenReceiveDrop(
  AFunc: TFunc<TStream>);
begin
  SendAndFree(AFunc);
  ShutdownSend;
end;

procedure TAsyncClientSocket.wmWinSockAsync(var AMsg: TMessage);
var
  LError: Word;
  LEvent: Word;
begin
  LError := WSAGetLastError;
  LEvent := WSAGetSelectEvent(AMsg.LParam);

  if LError <= WSABASEERR then
    case LEvent of
      FD_CONNECT: DoAfterConnect;
      FD_READ   : if Assigned(FOnReceive) then FOnReceive(ReceiveUntilDrop);
      FD_WRITE  : if Assigned(FOnSend) then FOnSend;
      FD_CLOSE  :
        if Connected then
        begin
          DoShutdown;
          Close;
          DoAfterDisconnect;
        end;
    end
  else
  begin
    if LError <> WSAEWOULDBLOCK then
  end;
end;

{ TServerCustomClientSocket }

constructor TServerCustomClientSocket.Create(const ASocket: TSocket);
begin
  inherited Create;

  FSocket := ASocket;
end;

{ TServerSocket }

procedure TServerSocket.AcceptClient(const AClientSocket: TSocket);
var
  LAddr: tSockAddrIn;
  LAddrSize: Integer;
  LClientSocket: tSocket;
  LClient: TServerCustomClientSocket;
begin
  LAddrSize := SizeOf(LAddr);
  LClientSocket := accept(AClientSocket, @LAddr, @LAddrSize);
  if LClientSocket <> INVALID_SOCKET then
  begin
    LClient := nil;
    if Assigned(FOnGetClientSocket) then
      LClient := FOnGetClientSocket;
    if not Assigned(LClient) then
      LClient := TServerCustomClientSocket.Create(LClientSocket);
    FClients.Add(LClientSocket, LClient);
    if Assigned(FOnClientConnected) then
      FOnClientConnected(LClient);
  end;
end;

procedure TServerSocket.Close;
begin

end;

procedure TServerSocket.CloseClient(const AClientSocket: TSocket);
var
  LClient: TServerCustomClientSocket;
begin
  if FClients.TryGetValue(AClientSocket, LClient) then
  begin
    LClient.ShutdownBoth;
    TWinsock.Excuete(closesocket(LClient.FSocket), 'closesocket');
    if ASsigned(FOnClientDisconnected) then
      FOnClientDisconnected(LClient);
    LClient.CleanUp;
    FClients.Remove(AClientSocket);
  end;
end;

procedure TServerSocket.CloseServer;
begin
  ShutdownBoth;
  TWinsock.Excuete(closesocket(FSocket), 'closesocket');
  Cleanup;
  FreeAndNil(FClients);
end;

procedure TServerSocket.Listen(const APort: Integer);
begin
  InitSocket;
  TWinsock.Excuete(WSAAsyncSelect(FSocket, Handle, WM_WINSOCK_ASYNC, FD_ACCEPT or FD_READ or FD_WRITE or FD_CLOSE), 'WSAAsyncSelect');
  FAddr := InitAddr('', APort, False);
  TWinsock.Excuete(bind(FSocket, FAddr, SizeOf(FAddr)), 'bind');
  TWinsock.Excuete(Winapi.Winsock.listen(FSocket, SOMAXCONN), 'listen');
  FClients := TObjectDictionary<TSocket, TServerCustomClientSocket>.Create([doOwnsValues]);
end;

procedure TServerSocket.ReadClient(const AClientSocket: TSocket);
var
  LClient: TServerCustomClientSocket;
  LRcv: TStringStream;
begin
  if FClients.TryGetValue(AClientSocket, LClient) then
    if Assigned(FOnRead) then
    begin
      LRcv := LClient.ReceiveUntilDrop;
      FOnRead(LClient, LRcv);
      LClient.Send(function: TStream begin Result := LRcv; end);
    end;
end;

procedure TServerSocket.wmWinSockAsync(var AMsg: TMessage);
var
  LError: Word;
  LEvent: Word;
  LSocket: TSocket;
begin
  LError := WSAGetLastError;
  LSocket := AMsg.WParam;
  LEvent := WSAGetSelectEvent(AMsg.LParam);

  if LError <= WSABASEERR then
    case LEvent of
      FD_ACCEPT : AcceptClient(LSocket);
      FD_READ   : ReadClient(LSocket);
      FD_WRITE  : WriteClient(LSocket);
      FD_CLOSE  :
      begin
        if WSAIsBlocking then
          WSACancelBlockingCall;
        if LSocket = FSocket then
          CloseServer
        else
          CloseClient(LSocket);
      end;
    end
  else
  begin
    if LError <> WSAEWOULDBLOCK then
  end;
end;

procedure TServerSocket.Send(const AClientSocket: TServerCustomClientSocket;
  AFunc: TFunc<TStream>);
begin
  AClientSocket.Send(AFunc);
end;

procedure TServerSocket.WriteClient(const AClientSocket: TSocket);
var
  LClient: TServerCustomClientSocket;
begin
  if Assigned(FOnWrite) then
    if FClients.TryGetValue(AClientSocket, LClient) then
      FOnWrite(LClient);
end;

initialization

finalization

end.
