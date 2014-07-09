unit mAsyncWorker;

interface

uses
  System.SysUtils, System.Classes
  ;

type
  TSimpleWorkerFunc<T: class> = class(TThread)
  private
    FAsyncFunc: TFunc<T>;
    FOnResult: TProc<T>;

    constructor Create(const AFunc: TFunc<T>); reintroduce;

    property OnBeforeDie: TProc<T> read FOnResult write FOnResult;
  protected
    procedure Execute; override;
  public
    class function Run(const AFunc: TFunc<T>): T;
  end;

  TSimpleWorkerProc = class(TThread)
  private
    FVclProc: TProc;
    FOnBeforeDie: TProc;
    constructor Create(const AProc: TProc); reintroduce;
  protected
    procedure Execute; override;
  public
    class procedure Run(const AProc: TProc);
  end;

implementation

uses
  Vcl.Forms
  ;

{ TAsyncSimpleWorker<T> }

constructor TSimpleWorkerFunc<T>.Create(const AFunc: TFunc<T>);
begin
  inherited Create(True);

  FAsyncFunc := AFunc;
end;

procedure TSimpleWorkerFunc<T>.Execute;
var
  LResult: T;
begin
  inherited;

  LResult := FAsyncFunc;
  Sleep(1000);
  Synchronize(procedure
    begin
      if Assigned(FOnResult) then
        FOnResult(LResult);
    end);
end;

class function TSimpleWorkerFunc<T>.Run(const AFunc: TFunc<T>): T;
var
  LAsyncer: TSimpleWorkerFunc<T>;
  LReult: T;
  LTerminated: Boolean;
begin
  LAsyncer := TSimpleWorkerFunc<T>.Create(AFunc);
  LAsyncer.FreeOnTerminate := True;
  LAsyncer.OnBeforeDie := procedure(AValue: T)
  begin
    LReult := AValue;
    LTerminated := True;
  end;
  LAsyncer.Start;
  LTerminated := False;
  repeat
    Application.HandleMessage;
  until LTerminated;
  Result := LReult;
end;

{ TSimpleVclAsyncProc }

constructor TSimpleWorkerProc.Create(const AProc: TProc);
begin
  inherited Create(True);

  FVclProc := AProc;
end;

procedure TSimpleWorkerProc.Execute;
begin
  inherited;

  FVclProc;
  Synchronize(
    procedure
    begin
      if Assigned(FOnBeforeDie) then
        FOnBeforeDie;
    end);
end;

class procedure TSimpleWorkerProc.Run(const AProc: TProc);
var
  LAsyncer: TSimpleWorkerProc;
  LTerminated: Boolean;
begin
  LAsyncer := TSimpleWorkerProc.Create(AProc);
  LAsyncer.FreeOnTerminate := True;
  LAsyncer.FOnBeforeDie := procedure
  begin
    LTerminated := True;
  end;
  LAsyncer.Start;
  LTerminated := False;
  repeat
    Application.HandleMessage;
  until LTerminated;
end;

end.
