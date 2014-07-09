unit mLog.TMemo;

interface

uses
  mLog, mLog.Threads, mLog.Common,

  System.Classes, System.SysUtils, System.Math,
  Vcl.StdCtrls;

type
  TLogMemo = class(TLog)
  private const
    MAX_MEMO_LINE_COUNT = MaxInt;
  private
    FQueue: TLogStringThreadQueue;
    procedure OnMemoChange(Sender: TObject);
  private
    FMemo: TMemo;
    FAutoScroll: Boolean;
    FShowDateTime: Boolean;
    FDateTimeFmt: String;
    procedure SetMemo(const Value: TMemo);
    procedure SetAutoScroll(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;

    procedure Enter(const AValue: String); override;
    procedure Exit(const AValue: String); override;

    procedure BeginUpdate; override;
    procedure EndUpdate; override;

    procedure Snd(const APacket: TBytes); override;
    procedure Snd(const AMsg: String; const APacket: TBytes); override;
    procedure Snd(const AErCondition: Boolean; const APacket: TBytes); override;
    procedure Snd(const AErCondition: Boolean; const AMsg: String; const APacket: TBytes); override;
    procedure Snd(const AErCondition: Boolean; const AMsg: String; const Args: array of const; const APacket: TBytes); override;

    procedure Rcv(const APacket: TBytes); override;
    procedure Rcv(const AMsg: String; const APacket: TBytes); override;
    procedure Rcv(const AErCondition: Boolean; const APacket: TBytes); override;
    procedure Rcv(const AErCondition: Boolean; const AMsg: String; const APacket: TBytes); override;
    procedure Rcv(const AErCondition: Boolean; const AMsg: String; const Args: array of const; const APacket: TBytes); override;

    procedure Msg(const AMsg: String); override;
    procedure Msg(const APacket: TBytes); override;
    procedure Msg(const AMsg: String; const APacket: TBytes); override;
    procedure Msg(const AMsg: String; const Args: array of const; const APacket: TBytes); override;
    procedure Msg(const AErCondition: Boolean; const AMsg: String); override;

    procedure Error(const AMsg: String); override;
    procedure Error(const AMsg: String; const APacket: TBytes); override;
    procedure Error(const Sender: TObject; const AMsg: String); override;
    procedure Error(const Sender: TObject; const AMsg: String; const Args: array of const); override;
    procedure Error(const Sender: TObject; const AMsg: String; const APacket: TBytes); override;

    property Memo: TMemo read FMemo write SetMemo;
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll;
    property ShowDateTime: Boolean read FShowDateTime write FShowDateTime;
    property DateTimeFmt: String read FDateTimeFmt write FDateTimeFmt;
  end;

function Log: TLogMemo;

implementation

uses
  mSysUtilsEx, mDateTimeHelper, mFontHelper,
  Vcl.Forms, Winapi.Windows, Winapi.Messages;
  
function Log: TLogMemo;
begin
  if not Assigned(mLog.Log) then
    mLog.Log := TLogMemo.Create;

  Result := TLogMemo(mLog.Log);
end;

{ TLogMemo }

procedure TLogMemo.BeginUpdate;
begin
  if Assigned(FMemo) then
    FMemo.Lines.BeginUpdate;
end;

procedure TLogMemo.Clear;
begin
  BeginUpdate;
  try
    FMemo.Clear;
  finally
    EndUpdate;
  end;
end;

constructor TLogMemo.Create;
begin
  FAutoScroll := True;

  FQueue := TLogStringThreadQueue.Create;
  FQueue.OnData := procedure(ALog: String)
  begin
    if Assigned(FMemo) then
    begin
      if FMemo.Lines.Count = MAX_MEMO_LINE_COUNT - 1 then
        Clear;
      BeginUpdate;
      try
        if not FShowDateTime then
          FMemo.Lines.Add(ALog)
        else if FDateTimeFmt.IsEmpty then
          FMemo.Lines.Add(Now.ToISO8601Str + #9 + ALog)
        else
          FMemo.Lines.Add(Now.ToString(FDateTimeFmt) + #9 + ALog)
      finally
        EndUpdate;
      end;
    end;
  end;
end;

destructor TLogMemo.Destroy;
begin
  if Assigned(FMemo) then
    FMemo.OnChange := nil;

  FQueue.Terminate;
  FQueue.DoShutDown;
  FQueue.WaitFor;
  FreeAndNil(FQueue);

  inherited;
end;

procedure TLogMemo.EndUpdate;
begin
  if Assigned(FMemo) then
    FMemo.Lines.EndUpdate;
end;

procedure TLogMemo.Enter(const AValue: String);
begin
  FQueue.Add('%s%s', [lkEnter.Str, AValue]);
end;

procedure TLogMemo.Error(const Sender: TObject; const AMsg: String;
  const Args: array of const);
begin
  FQueue.Add('%s%s', [lkErr.Str(Sender, AMsg, Args)]);
end;

procedure TLogMemo.Error(const Sender: TObject; const AMsg: String;
  const APacket: TBytes);
begin
  FQueue.Add('%s%s', [lkErr.Str(Sender), AMsg]);
  FQueue.Add('%s[%s]', [lkErr.Str(Sender), BytesToHexStr(APacket)]);
end;

procedure TLogMemo.Error(const AMsg: String);
begin
  FQueue.Add('%s', [lkErr.Str(AMsg)]);
end;

procedure TLogMemo.Error(const AMsg: String; const APacket: TBytes);
begin
  FQueue.Add('%s', [lkErr.Str(AMsg, APacket)]);
end;

procedure TLogMemo.Error(const Sender: TObject; const AMsg: String);
begin
  FQueue.Add('%s%s', [lkErr.Str(Sender, AMsg)]);
end;

procedure TLogMemo.Exit(const AValue: String);
begin
  FQueue.Add('%s%s', [lkExit.Str, AValue]);
end;

procedure TLogMemo.Msg(const AMsg: String; const Args: array of const;
  const APacket: TBytes);
begin
  FQueue.Add('%s', [lkMsg.Str(AMsg, Args, APacket)]);
end;

procedure TLogMemo.Msg(const AErCondition: Boolean; const AMsg: String);
begin
  if AErCondition then
    Msg(AMsg)
  else
    FQueue.Add('%s', [lkMsgEr.Str(AMsg)]);
end;

procedure TLogMemo.OnMemoChange(Sender: TObject);
begin
  if FAutoScroll then
    PostMessage(FMemo.Handle, EM_LINESCROLL, 0, FMemo.Lines.Count);
end;

procedure TLogMemo.Msg(const AMsg: String);
begin
  FQueue.Add('%s', [lkMsg.Str(AMsg)]);
end;

procedure TLogMemo.Msg(const APacket: TBytes);
begin
  FQueue.Add('%s', [lkMsg.Str(APacket)]);
end;

procedure TLogMemo.Msg(const AMsg: String; const APacket: TBytes);
begin
  FQueue.Add('%s', [lkMsg.Str(AMsg, APacket)]);
end;

procedure TLogMemo.Rcv(const AErCondition: Boolean; const APacket: TBytes);
begin
  if AErCondition then
    Rcv(APacket)
  else
    FQueue.Add('%s', [lkRcvEr.Str(APacket)]);
end;

procedure TLogMemo.Rcv(const AMsg: String; const APacket: TBytes);
begin
  FQueue.Add('%s', [lkRcv.Str(AMsg, APacket)]);
end;

procedure TLogMemo.Rcv(const APacket: TBytes);
begin
  FQueue.Add('%s', [lkRcv.Str(APacket)]);
end;

procedure TLogMemo.Rcv(const AErCondition: Boolean; const AMsg: String;
  const Args: array of const; const APacket: TBytes);
begin
  if AErCondition then
    Snd(AMsg, Args, APacket)
  else
    FQueue.Add('%s', [lkRcvEr.Str(AMsg, Args, APacket)]);
end;

procedure TLogMemo.Rcv(const AErCondition: Boolean; const AMsg: String;
  const APacket: TBytes);
begin
  if AErCondition then
    Rcv(AMsg, APacket)
  else
    FQueue.Add('%s', [lkRcvEr.Str(AMsg, APacket)]);
end;

procedure TLogMemo.Snd(const APacket: TBytes);
begin
  FQueue.Add('%s', [lkSnd.Str(APacket)]);
end;

procedure TLogMemo.SetAutoScroll(const Value: Boolean);
begin
  if FAutoScroll <> Value then
    FAutoScroll := Value;
end;

procedure TLogMemo.SetMemo(const Value: TMemo);
begin
  FMemo := Value;
  FMemo.OnChange := OnMemoChange;
  if not TFontUtil.IsFixedWidthFont(FMemo.Font.Name) then
    FMemo.Font.Name := TFontUtil.SystemFixedWidthFontName;
end;

procedure TLogMemo.Snd(const AErCondition: Boolean; const AMsg: String;
  const Args: array of const; const APacket: TBytes);
begin
  if AErCondition then
    Snd(AMsg, Args, APacket)
  else
    FQueue.Add('%s', [lkSndEr.Str(AMsg, Args, APacket)]);
end;

procedure TLogMemo.Snd(const AErCondition: Boolean; const AMsg: String;
  const APacket: TBytes);
begin
  if AErCondition then
    Snd(AMsg, APacket)
  else
    FQueue.Add('%s', [lkSndEr.Str(AMsg, APacket)]);
end;

procedure TLogMemo.Snd(const AMsg: String; const APacket: TBytes);
begin
  FQueue.Add('%s', [lkSnd.Str(AMsg, APacket)]);
end;

procedure TLogMemo.Snd(const AErCondition: Boolean; const APacket: TBytes);
begin
  if AErCondition then
    Snd(APacket)
  else
    FQueue.Add('%s', [lkSndEr.Str(APacket)]);
end;

initialization

finalization
  if Assigned(mLog.Log) then
    mLog.Log := nil;

end.
