unit mWatchInterval;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs
  ;

type
  TwatchInterval = class(TThread)
  private
    FEnabled: Boolean;
    FInterval: Integer;
    FOnTimeout: TProc;
    FPause, FWait: TSimpleEvent;
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Integer);
  protected
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create(const AInterval: Integer); reintroduce;
    destructor Destroy; override;

    procedure Alive;

    property Interval: Integer read FInterval write SetInterval;
    property Enabled: Boolean read GetEnabled write SetEnabled;

    property OnTimeOut: TProc read FOnTimeOut write FOnTimeOut;
  end;

implementation

{ TwatchInterval }

procedure TwatchInterval.Alive;
begin
  FWait.SetEvent;
end;

constructor TwatchInterval.Create(const AInterval: Integer);
begin
  inherited Create(False);

  FInterval := AInterval;

  FWait := TSimpleEvent.Create;
  FPause := TSimpleEvent.Create;
end;

destructor TwatchInterval.Destroy;
begin
  FreeAndNil(FPause);
  FreeAndNil(FWait);

  inherited;
end;

procedure TwatchInterval.Execute;
begin
  while not Terminated do
  begin
    if not Enabled then
      FPause.Acquire;
    FWait.ResetEvent;
    if (FWait.WaitFor(FInterval) = wrSignaled) and Enabled then
      Continue
    else if Enabled and Assigned(FOnTimeOut) then
    begin
      Enabled := False;
      Synchronize(
        procedure
        begin
          FOnTimeOut;
        end);
    end;
  end;
end;

function TwatchInterval.GetEnabled: Boolean;
begin
  TMonitor.Enter(Self);
  try
    Result := FEnabled;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TwatchInterval.SetEnabled(const Value: Boolean);
begin
  System.TMonitor.Enter(Self);
  try
    if FEnabled <> Value then
      FEnabled := Value;
  finally
    System.TMonitor.Exit(Self);
  end;
  if Enabled then
    FPause.SetEvent;
end;

procedure TwatchInterval.SetInterval(const Value: Integer);
begin
  if not Started then
    FInterval := Value;
end;

procedure TwatchInterval.TerminatedSet;
begin
  FOnTimeout := nil;
  if not Enabled then
    FPause.SetEvent;
end;

end.
