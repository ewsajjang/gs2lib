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
  end;

var
  Log: TLogMemo;

implementation

uses
  mSysUtilsEx,
  Vcl.Forms, Winapi.Windows, Winapi.Messages;

type
  TFontHelper = class
  private const
    FONTS_FIXED_WIDTH: array[0..5] of String = ('Consolas', 'Courier', 'Lucida Console', 'Terminal', 'FixedSys', 'Hyperfont');
  public
    class function IsFixedWidthFont(const AFontName: String): Boolean;
    class function SystemFixedWidthFontName: String;
  end;

{ TFontHelper }

class function TFontHelper.IsFixedWidthFont(const AFontName: String): Boolean;
var
  LName: String;
begin
  Result := False;
  for LName in FONTS_FIXED_WIDTH do
    if LName.Equals(AFontName) then
    begin
      Result := True;
      Break;
    end;
end;

class function TFontHelper.SystemFixedWidthFontName: String;
var
  LName: String;
begin
  Result := EmptyStr;
  for LName in FONTS_FIXED_WIDTH do
    if Screen.Fonts.IndexOf(LName) > -1 then
    begin
      Result := LName;
      Break;
    end;
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
        FMemo.Lines.Add(ALog);
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
    FQueue.Add('%s', [lkErr.Str(APacket)]);
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
    FQueue.Add('%s', [lkErr.Str(AMsg, Args, APacket)]);
end;

procedure TLogMemo.Rcv(const AErCondition: Boolean; const AMsg: String;
  const APacket: TBytes);
begin
  if AErCondition then
    Rcv(AMsg, APacket)
  else
    FQueue.Add('%s', [lkErr.Str(AMsg, APacket)]);
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
  if not TFontHelper.IsFixedWidthFont(FMemo.Font.Name) then
    FMemo.Font.Name := TFontHelper.SystemFixedWidthFontName;
end;

procedure TLogMemo.Snd(const AErCondition: Boolean; const AMsg: String;
  const Args: array of const; const APacket: TBytes);
begin
  if AErCondition then
    Snd(AMsg, Args, APacket)
  else
    FQueue.Add('%s', [lkErr.Str(AMsg, Args, APacket)]);
end;

procedure TLogMemo.Snd(const AErCondition: Boolean; const AMsg: String;
  const APacket: TBytes);
begin
  if AErCondition then
    Snd(AMsg, APacket)
  else
    FQueue.Add('%s', [lkErr.Str(AMsg, APacket)]);
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
    FQueue.Add('%s', [lkErr.Str(APacket)]);
end;

initialization
  if not Assigned(Log) then
    Log := TLogMemo.Create;

finalization
  if Assigned(Log) then
  begin
    Log.Free;
    Log := nil;
  end;

end.
