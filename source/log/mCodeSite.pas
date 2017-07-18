unit mCodeSite;

interface

uses
  CodeSiteLogging, Spring.Utils,

  System.Classes, System.SysUtils, System.UITypes, System.SyncObjs,
  System.Generics.Collections
  ;

type
  ICodeSiteLog = interface
    ['{13253586-A52D-4846-816C-DEA6B6AFF535}']
    function GetLogger: TCodeSiteLogger;
    procedure SetLogger(AValue : TCodeSiteLogger);

    property Log: TCodeSiteLogger read GetLogger write SetLogger;
  end;

  TCodeSiteLogClass = class(TInterfacedObject, ICodeSiteLog)
  private
  protected
    FLogger: TCodeSiteLogger;
    function GetLogger: TCodeSiteLogger; virtual;
    procedure SetLogger(Value: TCodeSiteLogger); virtual;
  public
    property Log: TCodeSiteLogger read GetLogger write SetLogger;
  end;

  TCodeSiteLogThread = class(TInterfacedThread, ICodeSiteLog)
  protected
    FLogger: TCodeSiteLogger;
    function GetLogger: TCodeSiteLogger; virtual;
    procedure SetLogger(AValue: TCodeSiteLogger); virtual;
  public
    property Log: TCodeSiteLogger read GetLogger write SetLogger;
  end;

  TCodeSiteLogDataModule = class(TDataModule, ICodeSiteLog)
  private
    FLogger: TCodeSiteLogger;
    function GetLogger: TCodeSiteLogger; virtual;
    procedure SetLogger(AValue: TCodeSiteLogger); virtual;
  public
    property Log: TCodeSiteLogger read GetLogger write SetLogger;
  end;

  TCodeSiteLoggerFactory = class
  private
    class var FDic: TObjectDictionary<String, TCodeSiteLogger>;
    class procedure Initialize;
    class procedure Finalize;
    class function GetLoggers(Category: String): TCodeSiteLogger; static;
  public
    class function CreateOrSelectLogger(const ACategory: String; ABgColor: TColor; AFontColor: TColor = TColorRec.Black): TCodeSiteLogger;
    class function CreateCodeSiteLogger(const ACategory: String): TCodeSiteLogger; overload;
    class function CreateCodeSiteLogger(const ACategory: String; ABgColor: TColor; AFontColor: TColor = TColorRec.Black): TCodeSiteLogger; overload;

    class property Loggers[Category: String]: TCodeSiteLogger read GetLoggers;
  end;

implementation

{ TCodeSiteLoggerFactory }

class function TCodeSiteLoggerFactory.CreateCodeSiteLogger(
  const ACategory: String): TCodeSiteLogger;
begin
  Result := TCodeSiteLogger.Create(nil);
  Result.Category := ACategory;
  FDic.AddOrSetValue(ACategory, Result);
end;

class function TCodeSiteLoggerFactory.CreateCodeSiteLogger(const ACategory: String;
  ABgColor, AFontColor: TColor): TCodeSiteLogger;
var
  i: Integer;
  LName: String;
begin
  i := 0;
  LName := ACategory;
  while FDic.ContainsKey(LName) do
  begin
    Inc(i);
    LName := Format('%s[%d]', [ACategory, i]);
  end;
  Result := CreateCodeSiteLogger(LName);
  Result.CategoryColor := ABgColor;
  Result.CategoryFontColor := AFontColor;
end;

class function TCodeSiteLoggerFactory.CreateOrSelectLogger(const ACategory: String; ABgColor,
  AFontColor: TColor): TCodeSiteLogger;
begin
  if FDic.ContainsKey(ACategory) then
    Result := FDic[ACategory]
  else
    Result := CreateCodeSiteLogger(ACategory, ABgColor, AFontColor);
end;

class procedure TCodeSiteLoggerFactory.Finalize;
begin
  if Assigned(FDic) then
    FreeAndNil(FDic);
end;

class function TCodeSiteLoggerFactory.GetLoggers(Category: String): TCodeSiteLogger;
begin
  Result := FDic[Category];
end;

class procedure TCodeSiteLoggerFactory.Initialize;
begin
  if not Assigned(FDic) then
    FDic := TObjectDictionary<String, TCodeSiteLogger>.Create([doOwnsValues]);
end;

{ TCodeSiteLogClass }

function TCodeSiteLogClass.GetLogger: TCodeSiteLogger;
begin
  if Assigned(FLogger) then
    Result := FLogger
  else
    Result := CodeSite;
end;

procedure TCodeSiteLogClass.SetLogger(Value: TCodeSiteLogger);
begin
  FLogger := Value;
end;

{ TCodeSiteLogThreadObject }

function TCodeSiteLogThread.GetLogger: TCodeSiteLogger;
begin
  if Assigned(FLogger) then
    Result := FLogger
  else
    Result := CodeSiteLogging.CodeSite
end;

procedure TCodeSiteLogThread.SetLogger(AValue: TCodeSiteLogger);
begin
  FLogger := AValue;
end;

{ TCodeSiteLogDataModule }

function TCodeSiteLogDataModule.GetLogger: TCodeSiteLogger;
begin
  if Assigned(FLogger) then
    Result := FLogger
  else
    Result := CodeSite;
end;

procedure TCodeSiteLogDataModule.SetLogger(AValue: TCodeSiteLogger);
begin
  FLogger := AValue;
end;

initialization
  TCodeSiteLoggerFactory.Initialize;

finalization
  TCodeSiteLoggerFactory.Finalize;

end.
