unit mLog.osx;

interface

uses
  mLogX,
  {$IFDEF MACOS}
  Macapi.ObjectiveC, Macapi.Helpers, Macapi.Foundation, FMX.Platform
  ,
  {$ENDIF}
  System.SysUtils, System.Classes,
  System.Rtti

  ;

type
  TOSXLogger = class(TLogger)
  private
  protected
    procedure Write(const AValue: String); override;
  public
    constructor Create;
  end;

  TPlatformLoggerManager = class(TLoggerManager)
  protected
    class function ApplicationTitle: String;
    class function PlatformLogger: TLogger; override;
  public
  end;

implementation

{ TgsLoggerWin }

constructor TOSXLogger.Create;
var
  LAppSvc: IFMXApplicationService;
begin
  TPlatformServices.Current.SupportsPlatformService(IFMXApplicationService, IInterface(LAppSvc));
  FAppTitle := LAppSvc.GetDefaultTitle;
end;

procedure TOSXLogger.Write(const AValue: String);
begin
  {$IFDEF MACOS or iOS}
  if FAppTitleEnabled and not AppTitle.IsEmpty then
    WriteLn(Format('%s'#9'%s', [AppTitle, AValue]))
  else
    WriteLn(AValue);
  {$ENDIF}
end;

{ TPlatformLoggerManager }

class function TPlatformLoggerManager.ApplicationTitle: String;
var
  LAppSvc: IFMXApplicationService;
begin
  TPlatformServices.Current.SupportsPlatformService(IFMXApplicationService, IInterface(LAppSvc));
end;

class function TPlatformLoggerManager.PlatformLogger: TLogger;
begin
  Result := TOSXLogger.Create;
end;

end.
