unit mDataQueue;

interface

uses
  System.Classes, System.SysUtils,
  System.Generics.Defaults, System.Generics.Collections
  ;

type
  TPopItemProcessFunc<T, T2> = reference to function(AItem: T): T2;
  TDataQueue<TPush: Class; TData> = class(TThread)
  private const
    MAX_QUEUE_DEPTH = 64;
  private
    FPopItemProcessFunc: TPopItemProcessFunc<TPush, TData>;
    FOnData: TProc<TPush, TData>;
    FQueue: TThreadedQueue<TPush>;
  protected
    procedure Execute; override;
  public
    constructor Create(const APopItemProcess: TPopItemProcessFunc<TPush, TData>); reintroduce;
    destructor Destroy; override;

    procedure Add(const AItem: TPush);

    property OnData: TProc<TPush, TData> read FOnData write FOnData;
  end;

implementation

{ TDvcDataQueue }

procedure TDataQueue<TPush,TData>.Add(const AItem: TPush);
begin
  FQueue.PushItem(AItem);
end;

constructor TDataQueue<TPush,TData>.Create(const APopItemProcess: TPopItemProcessFunc<TPush,TData>);
begin
  inherited Create(False);

  FPopItemProcessFunc := APopItemProcess;
  FQueue := TThreadedQueue<TPush>.Create(MAX_QUEUE_DEPTH);
  FreeOnTerminate := False;
end;

destructor TDataQueue<TPush,TData>.Destroy;
begin
  FQueue.DoShutDown;
  Terminate;
  Waitfor;
  FreeAndNil(FQueue);

  inherited;
end;

procedure TDataQueue<TPush,TData>.Execute;
var
  LItem: TPush;
  LData: TData;
  LSize: Integer;
begin
  NameThreadForDebugging(ClassName);
  while not Terminated do
  begin
    LItem := FQueue.PopItem;
    if not Terminated and not FQueue.ShutDown and Assigned(LItem) then
    begin
      if Assigned(FPopItemProcessFunc) and Assigned(FOnData) then
      begin
        LData := FPopItemProcessFunc(LItem);
        Synchronize(
          procedure
          begin
            FOnData(LItem, LData);
          end);
      end;
      FreeAndNil(LItem);
    end;
  end;
end;

end.
