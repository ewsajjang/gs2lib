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
  end;

implementation

{ TClientSocketHelper }

function TClientSocketHelper.ShutdownSnd: Boolean;
begin
  try
    TWinsock.Excuete( shutdown(Socket.SocketHandle, SD_SEND) );
  except
    on E: Exception do
    begin
      CodeSite.SendException('ShutdownSnd', E);
      Result := False;
    end;
  end;

end;


end.
