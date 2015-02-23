unit mIdSimpleServer;

interface

uses
  IdGlobal, IdTCPConnection, IdSimpleServer, IdExceptionCore, IdException,
  IdStack, IdSync,
  System.Classes, System.SysUtils,
  System.Generics.Collections, System.SyncObjs;

type
  TIdSimpleServerEx = class(TIdSimpleServer)
  private
    FOnListen: TProc;
    FOnEndListen: TProc;
    FAbortedRequested: Boolean;
    FListening: Boolean;
  public
    procedure EndListen; override;
    procedure Listen(ATimeout: Integer = IdTimeoutDefault); override;

    property Aborted: Boolean read FAbortedRequested;
    property Listening: Boolean read FListening;

    property OnListen: TProc read FOnListen write FOnListen;
    property OnEndListen: TProc read FOnEndListen write FOnEndListen;
  end;

  TSvrStatus = (ssEndListen, ssListenDisconnected, ssListenConnected);
  TSvrStatusHelper = record helper for TSvrStatus
    function Str: String;
  end;

  TRcvNotify = class(TIdNotify)
  private
    FEventProc: TProc<TIdBytes>;
    FBuffer: TIdBytes;
  protected
    procedure DoNotify; override;
  public
    constructor Create(const AEventProc: TProc<TIdBytes>; const ABuffer: TIdBytes); reintroduce;

    class procedure Execute(const AEventProc: TProc<TIdBytes>; const ABuffer: TIdBytes);
  end;


implementation

uses
  System.TypInfo
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
  FListening := True;
  if Assigned(FOnListen) then
    FOnListen;

  inherited Listen(ATimeOut);

  CheckConnected;
  if not Connected then
  begin
    if not Socket.BindingAllocated then
    begin
      Socket.Close;
      Socket.Open;
    end;
    if not IOHandler.Opened then
    begin
      IOHandler.Close;
      IOHandler.Open;
      CreateBinding;
    end;
  end;
end;

{ TSvrStatusHelper }

function TSvrStatusHelper.Str: String;
begin
  Result := GetEnumName(TypeInfo(TSvrStatus), Integer(Self));
end;

{ TRcvNotify }

class procedure TRcvNotify.Execute(const AEventProc: TProc<TIdBytes>; const ABuffer: TIdBytes);
begin
  TRcvNotify.Create(AEventProc, ABuffer).Notify;
end;

constructor TRcvNotify.Create(const AEventProc: TProc<TIdBytes>; const ABuffer: TIdBytes);
begin
  inherited Create;

  FBuffer := ABuffer;
  FEventProc := AEventProc;
end;

procedure TRcvNotify.DoNotify;
begin
  inherited;

  if Assigned(FEventProc) then
    FEventProc(FBuffer);
end;

end.
