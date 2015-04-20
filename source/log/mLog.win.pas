unit mLog.win;

interface

uses
  mLogX,

  {$IFDEF MSWINDOWS}
  Winapi.Windows
  ,
  {$ENDIF}

  System.SysUtils, System.Classes,
  System.Rtti

  ;

type
  TWinLogger = class(TLogger)
  private
  protected
    procedure Write(const AValue: String); override;
  public
  end;


  TPlatformLoggerManager = class(TLoggerManager)
  protected
    class function PlatformLogger: TLogger; override;
  public
  end;

implementation

{ TgsLoggerWin }

procedure TWinLogger.Write(const AValue: String);
begin
  {$IFDEF MSWINDOWS}
//  if DebugHook <> 0 then
//    OutputDebugString(PChar(AValue));

  AllocConsole;
  if FAppTitleEnabled and not AppTitle.IsEmpty then
    WriteLn(Format('%s', [AValue]))
  else
    WriteLn(AValue);
  {$ENDIF}
end;

{ TPlatformLoggerManager }

class function TPlatformLoggerManager.PlatformLogger: TLogger;
begin
  Result := TWinLogger.Create;
end;

end.
