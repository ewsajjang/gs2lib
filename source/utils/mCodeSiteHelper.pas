unit mCodeSiteHelper;

interface

uses
  CodeSiteLogging,
  System.SysUtils
  ;

type
  TCodeSiteLoggerHelper = class helper for TCodeSiteLogger
    procedure Send(Expression: Boolean; Msg: String); overload;
    procedure Send(Expression: Boolean; Fmt: String; Args: array of const); overload;

    procedure Send(const APacket: TBytes); overload;
    procedure Send(const AMsg: String; const APacket: TBytes); overload;
    procedure Send(const AMsg: String; const APacket: array of Byte); overload;
    procedure Send(const AMsg: String; Args: array of const; const APacket: TBytes); overload;
    procedure Send(const AErCondition: Boolean; const APacket: TBytes); overload;
    procedure Send(const AErCondition: Boolean; const AMsg: String; const APacket: TBytes); overload;
    procedure Send(const AErCondition: Boolean; const AMsg: String; const Args: array of const; const APacket: TBytes); overload;
  end;

implementation

uses
  mSysUtilsEx
  ;

{ TCodeSiteLoggerHelper }

procedure TCodeSiteLoggerHelper.Send(Expression: Boolean; Msg: String);
begin
  if not Expression then
    CodeSite.SendError(Msg)
  else
    CodeSite.Send(Msg);
end;

procedure TCodeSiteLoggerHelper.Send(Expression: Boolean; Fmt: String;
  Args: array of const);
begin
  if not Expression then
    CodeSite.SendError(Fmt, Args)
  else
    CodeSite.Send(Fmt, Args);
end;

procedure TCodeSiteLoggerHelper.Send(const AErCondition: Boolean;
  const AMsg: String; const APacket: TBytes);
begin
  if AErCondition then
    CodeSite.Send(AMsg, BytesToHexStr(APacket))
  else
    CodeSite.SendError('[%s]%s', [AMsg, BytesToHexStr(APacket)])
end;

procedure TCodeSiteLoggerHelper.Send(const AErCondition: Boolean;
  const AMsg: String; const Args: array of const; const APacket: TBytes);
begin
  if AErCondition then
    CodeSite.Send(Format(AMsg, Args), BytesToHexStr(APacket))
  else
    CodeSite.SendError('[%s]%s', [Format(AMsg, Args), BytesToHexStr(APacket)])
end;

procedure TCodeSiteLoggerHelper.Send(const AMsg: String;
  const APacket: array of Byte);
begin
  CodeSite.Send(AMsg, BytesToHexStr(APacket))
end;

procedure TCodeSiteLoggerHelper.Send(const AMsg: String; Args: array of const;
  const APacket: TBytes);
begin
  CodeSite.Send(Format(AMsg, Args), BytesToHexStr(APacket))
end;

procedure TCodeSiteLoggerHelper.Send(const APacket: TBytes);
begin
  CodeSite.Send(BytesToHexStr(APacket))
end;

procedure TCodeSiteLoggerHelper.Send(const AErCondition: Boolean;
  const APacket: TBytes);
begin
  if AErCondition then
    CodeSite.Send(BytesToHexStr(APacket))
  else
    CodeSite.SendError(BytesToHexStr(APacket))
end;

procedure TCodeSiteLoggerHelper.Send(const AMsg: String; const APacket: TBytes);
begin
  CodeSite.Send(AMsg, BytesToHexStr(APacket))
end;

end.
