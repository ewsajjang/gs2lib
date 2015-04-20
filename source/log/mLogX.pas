unit mLogX;

interface

uses
  System.SysUtils, System.Classes,
  System.Rtti
  ;

type
  TLogger = class
  public type
    TKind = (
      lkMsg,
      lkSnd,
      lkSndEr,
      lkRcv,
      lkRcvEr,
      lkError
    );
    TKindHelper = record helper for TKind
      function Str: String;
    end;
  private
    function ValueToStr(const AValue: TValue): String;
    procedure Write(const AKind: TKind; const AValues: array of TValue); overload;
  protected
    FAppTitle: String;
    FAppTitleEnabled: Boolean;
    procedure Write(const AValue: String); overload; virtual; abstract;
  public
    procedure Msg(const ACondition: Boolean; const AValues: array of TValue); overload;
    procedure Msg(const ABuffer: TBytes); overload;
    procedure Msg(const ACondition: Boolean; const ABuffer: TBytes); overload;
    procedure Msg(const AValue: TValue); overload;
    procedure Msg(const AValues: array of TValue); overload;
    procedure Msg(const AValue: String; const Arg: array of const); overload;
    procedure Msg(const AValue: String; const Arg: array of const; const ABuffer: TBytes); overload;

    procedure Snd(const ACondition: Boolean; const AValues: array of TValue); overload;
    procedure Snd(const ACondition: Boolean; const ABuffer: TBytes); overload;
    procedure Snd(const ABuffer: TBytes); overload;
    procedure Snd(const AValue: TValue); overload;
    procedure Snd(const AValues: array of TValue); overload;
    procedure Snd(const AValue: String; const Arg: array of const); overload;
    procedure Snd(const AValue: String; const Arg: array of const; const ABuffer: TBytes); overload;

    procedure Rcv(const ACondition: Boolean; const AValues: array of TValue); overload;
    procedure Rcv(const ACondition: Boolean; const ABuffer: TBytes); overload;
    procedure Rcv(const ABuffer: TBytes); overload;
    procedure Rcv(const AValue: TValue); overload;
    procedure Rcv(const AValues: array of TValue); overload;
    procedure Rcv(const AValue: String; const Arg: array of const); overload;
    procedure Rcv(const AValue: String; const Arg: array of const; const ABuffer: TBytes); overload;

    procedure e(const AValue: TValue); overload;
    procedure e(const AValues: array of TValue); overload;
    procedure e(const AValue: String; const Arg: array of const); overload;

    property AppTitle: String read FAppTitle;
    property AppTitleEnabled: Boolean read FAppTitleEnabled write FAppTitleEnabled;
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

procedure TLogger.e(const AValues: array of TValue);
begin
  Write(lkError, AValues);
end;

procedure TLogger.Msg(const AValues: array of TValue);
begin
  Write(lkMsg, AValues);
end;

procedure TLogger.Msg(const ACondition: Boolean; const AValues: array of TValue);
begin
  if ACondition then
    Msg(AValues)
  else
    e(AValues)
end;

procedure TLogger.Msg(const AValue: TValue);
begin
  Write(lkMSG, [AValue]);
end;

procedure TLogger.Rcv(const AValues: array of TValue);
begin
  Write(lkRcv, AValues);
end;

procedure TLogger.Snd(const ACondition: Boolean; const AValues: array of TValue);
begin
  if ACondition then
    Snd(AValues)
  else
    Write(lkSndEr, AValues);
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
begin
  LBuilder := TStringBuilder.Create;
  try
    LBuilder.Append(AKind.Str + #9);
    for LValue in AValues do
    begin
      LBuilder.Append(ValueToStr(LValue));
      LBuilder.Append(' ');
    end;
    Write(LBuilder.ToString);
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

procedure TLogger.e(const AValue: TValue);
begin
  Write(lkError, [AValue]);
end;

procedure TLogger.e(const AValue: String; const Arg: array of const);
begin
  e(Format(AValue, Arg));
end;

procedure TLogger.Msg(const AValue: String; const Arg: array of const;
  const ABuffer: TBytes);
begin
  Msg([Format(AValue, Arg), BytesToHexStr(ABuffer)])
end;

procedure TLogger.Msg(const ACondition: Boolean; const ABuffer: TBytes);
begin
  if ACondition then
    Msg(ABuffer)
  else
    write(lkError, [BytesToHexStr(ABuffer)]);
end;

procedure TLogger.Msg(const ABuffer: TBytes);
begin
  Msg(BytesToHexStr(ABuffer));
end;

procedure TLogger.Msg(const AValue: String; const Arg: array of const);
begin
  write(Format(AValue, Arg));
end;

procedure TLogger.Rcv(const ACondition: Boolean; const AValues: array of TValue);
begin
  if ACondition then
    Rcv(AValues)
  else
    Write(lkRcvEr, AValues)
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

procedure TLogger.Snd(const ABuffer: TBytes);
begin
  Snd(BytesToHexStr(ABuffer));
end;

procedure TLogger.Snd(const ACondition: Boolean; const ABuffer: TBytes);
begin
  if ACondition then
    Snd(ABuffer)
  else
    write(lkSndEr, [BytesToHexStr(ABuffer)]);
end;

procedure TLogger.Rcv(const ACondition: Boolean; const ABuffer: TBytes);
begin
  if ACondition then
    Rcv(ABuffer)
  else
    write(lkRcvEr, [BytesToHexStr(ABuffer)]);
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

end.
