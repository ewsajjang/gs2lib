unit mLog.Threads;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  ELogStringThreadQueue = class(Exception);
    EQueueDepthOverFlow = class(ELogStringThreadQueue);
  TLogStringThreadQueue = class(TThread)
  private const
    MAX_QUEUE_DEPTH = 16;
  private
    FQueue: TThreadedQueue<String>;
    FOnData: TProc<String>;
  protected
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure Add(const AValue: String); overload;
    procedure Add(const AValue: String; Args: array of const); overload;
    procedure Clear;

    property OnData: TProc<String> read FOnData write FOnData;
  end;

implementation

{ TLogStringThreadQueue }

procedure TLogStringThreadQueue.Add(const AValue: String);
begin
  if FQueue.QueueSize < MAX_QUEUE_DEPTH then
    FQueue.PushItem(AValue)
  else
    raise EQueueDepthOverFlow.Create('The Queue max depth is overflowed.');
end;

procedure TLogStringThreadQueue.Add(const AValue: String; Args: array of const);
begin
  Add(Format(AValue, Args));
end;

procedure TLogStringThreadQueue.Clear;
begin
  FQueue.Grow(0);
end;

constructor TLogStringThreadQueue.Create;
begin
  inherited Create(False);

  FQueue := TThreadedQueue<String>.Create(MAX_QUEUE_DEPTH);
end;

destructor TLogStringThreadQueue.Destroy;
begin
  FQueue.DoShutDown;
  FreeAndNil(FQueue);

  inherited;
end;

procedure TLogStringThreadQueue.Execute;
var
  LLog: String;
begin
  NameThreadForDebugging('TLogStringThreadQueue');
  while not Terminated do
  begin
    LLog := FQueue.PopItem;
    if Assigned(FOnData) then
      Synchronize(procedure begin FOnData(LLog) end);
  end;
end;

end.
