unit mLogX;

interface

uses
  FMX.Memo,

  System.SysUtils, System.Classes,
  System.Rtti
  ;

type
  TLogger = class
  public type
    TKind = (
      lkMsg,
      lkSnd,
      lkErSnd,
      lkRcv,
      lkErRcv,
      lkError
    );
    TKindHelper = record helper for TKind
      function Str: String;
    end;
  private
    FMemo: TMemo;
    FMemoEnable: Boolean;
    FConsoleLog: Boolean;
    function ValueToStr(const AValue: TValue): String;
    procedure Write(const AKind: TKind; const AValues: array of TValue); overload;
  protected
    FAppTitle: String;
    FAppTitleEnabled: Boolean;
    procedure Write(const AValue: String); overload; virtual; abstract;
  public
    function Msg(const ACondition: Boolean; const AValues: array of TValue): Boolean; overload;
    function Msg(const ACondition: Boolean; const AValue: String; const Arg: array of const): Boolean; overload;
    function Msg(const ACondition: Boolean; const ABuffer: TBytes): Boolean; overload;
    procedure Msg(const AValue: String; const ABuffer: TBytes); overload;
    procedure Msg(const ABuffer: TBytes); overload;
    procedure Msg(const AValue: TValue); overload;
    procedure Msg(const AValues: array of TValue); overload;
    procedure Msg(const AValue: String; const Arg: array of const); overload;
    procedure Msg(const AValue: String; const Arg: array of const; const ABuffer: TBytes); overload;

    function Snd(const ACondition: Boolean; const AValues: array of TValue): Boolean; overload;
    function Snd(const ACondition: Boolean; const ABuffer: TBytes): Boolean; overload;
    function Snd(const ACondition: Boolean; const AValue: String; const Arg: array of const): Boolean; overload;
    function Snd(const ACondition: Boolean; const AValue: String; const Arg: array of const; const ABuffer: TBytes): Boolean; overload;
    procedure Snd(const ABuffer: TBytes); overload;
    procedure Snd(const AValue: TValue); overload;
    procedure Snd(const AValue: String; const ABuffer: TBytes); overload;
    procedure Snd(const AValues: array of TValue); overload;
    procedure Snd(const AValue: String; const Arg: array of const); overload;
    procedure Snd(const Arg: TStrings); overload;
    procedure Snd(const AValue: String; const Arg: TStrings); overload;
    procedure Snd(const AValue: String; const Arg: array of const; const ABuffer: TBytes); overload;

    function Rcv(const ACondition: Boolean; const AValues: array of TValue): Boolean; overload;
    function Rcv(const ACondition: Boolean; const ABuffer: TBytes): Boolean; overload;
    function Rcv(const ACondition: Boolean; const AValue: String; const Arg: array of const): Boolean; overload;
    function Rcv(const ACondition: Boolean; const AValue: String; const Arg: array of const; const ABuffer: TBytes): Boolean; overload;
    procedure Rcv(const ABuffer: TBytes); overload;
    procedure Rcv(const AValue: TValue); overload;
    procedure Rcv(const Arg: TStrings); overload;
    procedure Rcv(const AValue: String; const ABuffer: TBytes); overload;
    procedure Rcv(const AValues: array of TValue); overload;
    procedure Rcv(const AValue: String; const Arg: array of const); overload;
    procedure Rcv(const AValue: String; const Arg: TStrings); overload;
    procedure Rcv(const AValue: String; const Arg: array of const; const ABuffer: TBytes); overload;

    procedure E(const AValue: TValue); overload;
    procedure E(const AValues: array of TValue); overload;
    procedure E(const AValue: String; const Arg: array of const); overload;

    property ConsoleLog: Boolean read FConsoleLog write FConsoleLog;
    property AppTitle: String read FAppTitle;
    property AppTitleEnabled: Boolean read FAppTitleEnabled write FAppTitleEnabled;
    property Memo: TMemo read FMemo write FMemo;
    property MemoEnable: Boolean read FMemoEnable write FMemoEnable;
  end;

type
  TLoggerManager = class
  protected type
    TLoggerManagerClass = class of TLoggerManager;
  protected class var
    FLogger: TLogger;
    FLoggerClass: TLoggerManagerClass;
  protected
    class function PlatformLogger: TLogger; virtual; abstract;
    class function GetLogger: TLogger; static;
  public
    class property Current: TLogger read GetLogger;
  end;

function Log: TLogger;

implementation

uses
  mSysUtilsEx,
  {$IFDEF MACOS or iOS}
    mLog.osx,
  {$ELSE}
    {$IFDEF MSWINDOWS}
      mLog.win,
    {$ENDIF}
  {$ENDIF}
  System.TypInfo
  ;

function Log: TLogger;
begin
  Result := TLoggerManager.Current;
end;

{ TgsLogger }

{ TgsLogger.TKindHelper }

function TLogger.TKindHelper.Str: String;
begin
  Result := GetEnumName(TypeInfo(TLogger.TKind), Integer(Self));
end;

procedure TLogger.E(const AValues: array of TValue);
begin
  Write(lkError, AValues);
end;

procedure TLogger.Msg(const AValues: array of TValue);
begin
  Write(lkMsg, AValues);
end;

function TLogger.Msg(const ACondition: Boolean; const AValues: array of TValue): Boolean;
begin
  if ACondition then
    Msg(AValues)
  else
    E(AValues);

  Result := ACondition;
end;

procedure TLogger.Msg(const AValue: TValue);
begin
  Write(lkMSG, [AValue]);
end;

procedure TLogger.Rcv(const AValues: array of TValue);
begin
  Write(lkRcv, AValues);
end;

function TLogger.Snd(const ACondition: Boolean; const AValues: array of TValue): Boolean;
begin
  if ACondition then
    Snd(AValues)
  else
    Write(lkErSnd, AValues);
  Result := ACondition;
end;

procedure TLogger.Snd(const AValue: TValue);
begin
  Write(lkSnd, [AValue]);
end;

procedure TLogger.Snd(const AValue: String; const Arg: array of const);
begin
  Snd(Format(AValue, Arg));
end;

procedure TLogger.Snd(const AValues: array of TValue);
begin
  Write(lkSnd, AValues);
end;

function TLogger.ValueToStr(const AValue: TValue): String;
var
  LTryAsBool: Boolean;
begin
  Result := EmptyStr;

  case AValue.Kind of
    tkChar   ,
    tkString ,
    tkWChar  ,
    tkWString,
    tkUString,
    tkLString: Result := AValue.AsString;

    tkInteger: Result := AValue.AsInteger.ToString;
    tkInt64: Result := AValue.AsInt64.ToString;

    tkFloat: Result := AValue.AsExtended.ToString;

    tkEnumeration:
      if AValue.TryAsType<Boolean>(LTryAsBool) then
        Result := AValue.AsBoolean.ToString
      else
        Result := AValue.AsOrdinal.ToString;

    tkClass: Result := AValue.AsObject.ClassName;
    tkClassRef: Result := AValue.AsObject.ClassName;

    tkUnknown  ,
    tkSet      ,
    tkMethod   ,
    tkVariant  ,
    tkArray    ,
    tkRecord   ,
    tkInterface,
    tkDynArray ,
    tkPointer  ,
    tkProcedure:
      if not AValue.TryAsType<String>(Result) then
        Result := String(#191);
  end;
end;

procedure TLogger.Write(const AKind: TKind; const AValues: array of TValue);
var
  LValue: TValue;
  LBuilder: TStringBuilder;
  LLog: String;
begin
  LBuilder := TStringBuilder.Create;
  try
    LBuilder.AppendFormat('%-8s%'#9, [AKind.Str.Substring(2)]);
    for LValue in AValues do
    begin
      LBuilder.Append(ValueToStr(LValue));
      LBuilder.Append(' ');
    end;
    LLog := LBuilder.ToString;
    if FConsoleLog then
      Write(LLog);
    if FMemoEnable and Assigned(FMemo) then
      FMemo.Lines.Add(LLog);
  finally
    FreeAndNil(LBuilder);
  end;
end;

{ TLoggerManager }

class function TLoggerManager.GetLogger: TLogger;
begin
  if not Assigned(FLoggerClass) then
    FLoggerClass := TPlatformLoggerManager;

  if not Assigned(FLogger) then
    FLogger := FLoggerClass.PlatformLogger;

  Result := FLogger
end;

procedure TLogger.E(const AValue: TValue);
begin
  Write(lkError, [AValue]);
end;

procedure TLogger.E(const AValue: String; const Arg: array of const);
begin
  E(Format(AValue, Arg));
end;

procedure TLogger.Msg(const AValue: String; const Arg: array of const;
  const ABuffer: TBytes);
begin
  Msg([Format(AValue, Arg), BytesToHexStr(ABuffer)])
end;

procedure TLogger.Msg(const AValue: String; const ABuffer: TBytes);
begin
  Msg([AValue, BytesToHexStr(ABuffer)])
end;

function TLogger.Msg(const ACondition: Boolean; const AValue: String;
  const Arg: array of const): Boolean;
begin
  if ACondition then
    Msg(AValue, Arg)
  else
    E(AValue, Arg);
  Result := ACondition;
end;

function TLogger.Msg(const ACondition: Boolean; const ABuffer: TBytes): Boolean;
begin
  if ACondition then
    Msg(ABuffer)
  else
    write(lkError, [BytesToHexStr(ABuffer)]);
  Result := ACondition;
end;

procedure TLogger.Msg(const ABuffer: TBytes);
begin
  Msg(BytesToHexStr(ABuffer));
end;

procedure TLogger.Msg(const AValue: String; const Arg: array of const);
begin
  write(lkMsg, [Format(AValue, Arg)]);
end;

function TLogger.Rcv(const ACondition: Boolean; const AValues: array of TValue): Boolean;
begin
  if ACondition then
    Rcv(AValues)
  else
    Write(lkErRcv, AValues);
  Result := ACondition;
end;

procedure TLogger.Rcv(const AValue: TValue);
begin
  Write(lkRcv, [AValue]);
end;

procedure TLogger.Rcv(const AValue: String; const Arg: array of const);
begin
  Rcv(Format(AValue, Arg));
end;

procedure TLogger.Snd(const AValue: String; const Arg: array of const;
  const ABuffer: TBytes);
begin
  Snd([Format(AValue, Arg), BytesToHexStr(ABuffer)])
end;

procedure TLogger.Snd(const Arg: TStrings);
var
  LItem: String;
begin
  for LItem in Arg do
    Snd(#9 + LItem);
end;

procedure TLogger.Snd(const AValue: String; const Arg: TStrings);
begin
  Snd(AValue);
  Snd(Arg);
end;

function TLogger.Snd(const ACondition: Boolean; const AValue: String;
  const Arg: array of const): Boolean;
begin
  if ACondition then
    Snd([Format(AValue, Arg)])
  else
    write(lkErSnd, [Format(AValue, Arg)]);
  Result := ACondition;
end;

function TLogger.Snd(const ACondition: Boolean; const AValue: String;
  const Arg: array of const; const ABuffer: TBytes): Boolean;
begin
  if ACondition then
    Snd([Format(AValue, Arg), BytesToHexStr(ABuffer)])
  else
    write(lkErSnd, [Format(AValue, Arg), BytesToHexStr(ABuffer)]);
  Result := ACondition;
end;

procedure TLogger.Snd(const AValue: String; const ABuffer: TBytes);
begin
  Snd([AValue, BytesToHexStr(ABuffer)])
end;

procedure TLogger.Snd(const ABuffer: TBytes);
begin
  Snd(BytesToHexStr(ABuffer));
end;

function TLogger.Snd(const ACondition: Boolean; const ABuffer: TBytes): Boolean;
begin
  if ACondition then
    Snd(ABuffer)
  else
    write(lkErSnd, [BytesToHexStr(ABuffer)]);
  Result := ACondition;
end;

function TLogger.Rcv(const ACondition: Boolean; const ABuffer: TBytes): Boolean;
begin
  if ACondition then
    Rcv(ABuffer)
  else
    write(lkErRcv, [BytesToHexStr(ABuffer)]);
  Result := ACondition;
end;

procedure TLogger.Rcv(const ABuffer: TBytes);
begin
  Rcv(BytesToHexStr(ABuffer));
end;

procedure TLogger.Rcv(const AValue: String; const Arg: array of const;
  const ABuffer: TBytes);
begin
  Rcv([Format(AValue, Arg), BytesToHexStr(ABuffer)])
end;

procedure TLogger.Rcv(const Arg: TStrings);
var
  LItem: String;
begin
  for LItem in Arg do
    Rcv(#9 + LItem);
end;

procedure TLogger.Rcv(const AValue: String; const Arg: TStrings);
begin
  Rcv(AValue);
  Rcv(Arg);
end;

function TLogger.Rcv(const ACondition: Boolean; const AValue: String;
  const Arg: array of const): Boolean;
begin
  if ACondition then
    Rcv([Format(AValue, Arg)])
  else
    write(lkErRcv, [Format(AValue, Arg)]);
  Result := ACondition;
end;

function TLogger.Rcv(const ACondition: Boolean; const AValue: String;
  const Arg: array of const; const ABuffer: TBytes): Boolean;
begin
  if ACondition then
    Rcv([Format(AValue, Arg), BytesToHexStr(ABuffer)])
  else
    write(lkErRcv, [Format(AValue, Arg), BytesToHexStr(ABuffer)]);
  Result := ACondition;
end;

procedure TLogger.Rcv(const AValue: String; const ABuffer: TBytes);
begin
  Rcv([AValue, BytesToHexStr(ABuffer)])
end;

end.
