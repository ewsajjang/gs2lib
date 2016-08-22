unit mServerSocket;

interface

uses
  mCodeSite, CodeSiteLogging,

  System.Classes, System.SysUtils, Vcl.ComCtrls,
  System.Win.ScktComp, Winapi.winsock
  ;

type
  TSvrClient = class(TServerClientWinSocket, IInterface, ICodeSiteLog)
  private
    FLogger: TCodeSiteLogger;
  {$REGION 'Implements IInterface'}
    protected
      FRefCount: Integer;
      function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
  {$ENDREGION}
  {$REGION 'Implements ICodeSiteLog'}
    protected
      function GetLogger: TCodeSiteLogger;
      procedure SetLogger(AValue : TCodeSiteLogger);
  {$ENDREGION}
  protected
    FRcvBuffer: TBytesStream;
    procedure DoAfterCreate; virtual;
    procedure DoBeforeReadData; virtual;
    procedure DoAfterReadData; virtual;
  public
    constructor Create(Socket: TSocket; ServerWinSocket: TServerWinSocket);
    destructor Destroy; override;

    function ReadData: Integer;
    procedure SendText(const AValue: String); overload;
    procedure SendText(const AValue: String; const AEncoding: TEncoding); overload;

    property Logger: TCodeSiteLogger read GetLogger write SetLogger;
  end;

implementation

uses
  System.SyncObjs, mCodeSiteHelper
  ;

{ THL7SvrClient }

constructor TSvrClient.Create(Socket: TSocket;
  ServerWinSocket: TServerWinSocket);
begin
  inherited Create(Socket, ServerWinSocket);

  FRcvBuffer := TBytesStream.Create;
  DoAfterCreate;
end;

destructor TSvrClient.Destroy;
begin
  if Assigned(FRcvBuffer) then
    FreeAndNil(FRcvBuffer);

  inherited;
end;

procedure TSvrClient.DoAfterReadData;
begin
end;

procedure TSvrClient.DoBeforeReadData;
begin
end;

procedure TSvrClient.DoAfterCreate;
begin
end;

function TSvrClient.GetLogger: TCodeSiteLogger;
begin
  if Assigned(FLogger) then
    Result := FLogger
  else
    Result := CodeSite;
end;

function TSvrClient.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TSvrClient.ReadData: Integer;
var
  LBuffer: Pointer;
  LBufLen: Integer;
begin
  DoBeforeReadData;
  Logger.EnterMethod('TSvrClient.ReadData');
  if ReceiveLength = 0 then
    Exit(0);
  GetMem(LBuffer, ReceiveLength);
  try
    LBufLen := ReceiveBuf(LBuffer^, ReceiveLength);
    Logger.Send('Reading', LBufLen);
    if LBufLen > 0 then
      FRcvBuffer.Write(LBuffer^, LBufLen);
    Result := FRcvBuffer.Size;
  finally
    FreeMem(LBuffer);
  end;
  Logger.Send('Read[%d]', [LBufLen], FRcvBuffer.Bytes, LBufLen);
  Logger.ExitMethod('TSvrClient.ReadData');
  FRcvBuffer.Position := 0;
  DoAfterReadData;
end;

procedure TSvrClient.SendText(const AValue: String);
begin
  SendText(AValue, TEncoding.UTF8);
end;

procedure TSvrClient.SendText(const AValue: String; const AEncoding: TEncoding);
var
  LStream: TStringStream;
  LBuf: array[0..1024] of byte;
  LBufLen, LSendTotal, LSendSize: Integer;
begin
  Logger.EnterMethod('TSvrClient.SendText');
  LStream := TStringStream.Create(AValue, AEncoding);
  try
    LSendTotal := 0;
    while LSendTotal < LStream.Size do
    begin
      LStream.Position := LSendTotal;
      LBufLen := LStream.Read(LBuf, SizeOf(LBuf));
      if LBufLen > 0 then
      begin
        LSendSize := SendBuf(LBuf, LBufLen);
        if LSendSize > 0 then
          Inc(LSendTotal, LSendSize);
      end;
    end;
    Logger.Send('SendTotal: %d, SendBufferSize: %d', [LSendTotal, LStream.Size]);
  finally
    FreeAndNil(LStream);
  end;
  Logger.ExitMethod('TSvrClient.SendText');
end;

procedure TSvrClient.SetLogger(AValue: TCodeSiteLogger);
begin
  FLogger := AValue;
end;

function TSvrClient._AddRef: Integer;
begin
  Result := TInterlocked.Increment(FRefCount);
end;

function TSvrClient._Release: Integer;
begin
  Result := TInterlocked.Decrement(FRefCount);
end;

end.
