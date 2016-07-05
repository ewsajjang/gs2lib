unit mWinsockHelper;

interface

uses
  System.Classes, System.SysUtils, Winapi.WinSock, System.Win.ScktComp
  ;

type
  TClientSocketHelper = class helper for TClientSocket
    function ShutdownSnd: Boolean;
  end;

  TErrorEventHelper = record helper for TErrorEvent
    function ToString: String;
    function ToDesc: String;
  end;

implementation

uses
  mWinsock,
  Spring.SystemUtils
  ;

{ TClientSocketHelper }

function TClientSocketHelper.ShutdownSnd: Boolean;
begin
  try
    Result := TWinsock.Excuete( shutdown(Socket.SocketHandle, SD_SEND), 'shutdown SD_SEND' );
  except
    on E: Exception do
    begin
      Result := False;
    end;
  end;
end;

function TErrorEventHelper.ToDesc: String;
begin
  case Self of
    eeGeneral    : Result := 'Errors that do not belongs to the following categories are marked as eeGeneral.';
    eeSend       : Result := 'An error occurred when trying to write to socket connection.';
    eeReceive    : Result := 'An error occurred when trying to read from socket connection.';
    eeConnect    : Result := 'A connection request that was already accepted colud not be completed.';
    eeDisconnect : Result := 'An error occurred when trying to close connection.';
    eeAccept     : Result := 'A problem occurred when trying to accept a client connection request.';
    eeLookup     : Result := 'An error occurred when trying to lookup from server.';
  end;
end;

function TErrorEventHelper.ToString: String;
begin
  Result := TEnum.GetName<TErrorEvent>(Self);
end;


end.
