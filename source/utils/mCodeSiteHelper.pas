unit mCodeSiteHelper;

interface

uses
  CodeSiteLogging,
  System.Classes, System.SysUtils, System.Types
  ;

type
  TCodeSiteLoggerHelper = class helper for TCodeSiteLogger
    procedure Send(const List: TStringList); overload;

    procedure Send(const Fmt: String; Args: array of const; List: TStrings); overload;

    procedure Send(const AMsg: String; const ARect: TRect); overload;
    procedure Send(const AMsg: String; Args: array of const; const ARect: TRect); overload;

    procedure Send(const APacket: TBytes); overload;
    procedure Send(const AMsg: String; const APacket: TBytes); overload;
    procedure Send(const AMsg: String; const APacket: array of Byte); overload;
    procedure Send(const AMsg: String; Args: array of const; const APacket: TBytes); overload;

    function Send(const Expression: Boolean; Msg: String): Boolean; overload;
    function Send(const Expression: Boolean; Fmt: String; Args: array of const): Boolean; overload;
    function Send(const Expression: Boolean; const APacket: TBytes): Boolean; overload;
    function Send(const Expression: Boolean; const AMsg: String; const AValue: String): Boolean; overload;
    function Send(const Expression: Boolean; const AMsg: String; const APacket: TBytes): Boolean; overload;
    function Send(const Expression: Boolean; const AMsg: String; const Args: array of const; const APacket: TBytes): Boolean; overload;
  end;

implementation

uses
  mSysUtilsEx, mTypesHelper, System.StrUtils, System.SysConst
  ;

procedure CodeSiteAssertErrorHandler(const Message, Filename: String; LineNumber: Integer; ErrorAddr: Pointer);
const
  SAssertionFmt = 'AssertionFiled - File: %s, Line: %d, Addr: %p, Msg: %s';
var
  LMsg: String;
begin
  LMsg := IfThen(not Message.IsEmpty, Message, SAssertionFailed);
  CodeSite.SendError(SAssertionFmt,
    [ExtractFileName(Filename), LineNumber, ErrorAddr, LMsg]);

  raise EAssertionFailed.CreateFmt(SAssertionFmt,
    [ExtractFileName(Filename), LineNumber, ErrorAddr, LMsg])
end;

{ TCodeSiteLoggerHelper }

function TCodeSiteLoggerHelper.Send(const Expression: Boolean; Msg: String): Boolean;
begin
  Result := Expression;
  if not Result then
    CodeSite.Send(Msg)
  else
    CodeSite.SendError(Msg);
end;

function TCodeSiteLoggerHelper.Send(const Expression: Boolean; Fmt: String;
  Args: array of const): Boolean;
begin
  Result := Expression;
  if Result then
    CodeSite.Send(Fmt, Args)
  else
    CodeSite.SendError(Fmt, Args);
end;

function TCodeSiteLoggerHelper.Send(const Expression: Boolean;
  const AMsg: String; const APacket: TBytes): Boolean;
begin
  Result := Expression;
  if Result then
    CodeSite.Send(AMsg, BytesToHexStr(APacket))
  else
    CodeSite.SendError('[%s]%s', [AMsg, BytesToHexStr(APacket)])
end;

function TCodeSiteLoggerHelper.Send(const Expression: Boolean;
  const AMsg: String; const Args: array of const; const APacket: TBytes): Boolean;
begin
  Result := Expression;
  if Result then
    CodeSite.Send(Format(AMsg, Args), BytesToHexStr(APacket))
  else
    CodeSite.SendError('[%s]%s', [Format(AMsg, Args), BytesToHexStr(APacket)])
end;

function TCodeSiteLoggerHelper.Send(const Expression: Boolean; const AMsg,
  AValue: String): Boolean;
begin
  Result := Expression;
  if Result then
    CodeSite.Send(AMsg, AValue)
  else
    CodeSite.SendError('%s = %s', [AMsg, AValue])
end;

procedure TCodeSiteLoggerHelper.Send(const AMsg: String; const ARect: TRect);
begin
  CodeSite.Send(AMsg, ARect.ToString);
end;

procedure TCodeSiteLoggerHelper.Send(const AMsg: String; Args: array of const;
  const ARect: TRect);
begin
  CodeSite.Send(Format(AMsg, Args), ARect.ToString);
end;

procedure TCodeSiteLoggerHelper.Send(const List: TStringList);
begin
  CodeSite.Send('', List);
end;

procedure TCodeSiteLoggerHelper.Send(const Fmt: String; Args: array of const;
  List: TStrings);
begin
  CodeSite.Send(Format(Fmt, Args), List);
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

function TCodeSiteLoggerHelper.Send(const Expression: Boolean;
  const APacket: TBytes): Boolean;
begin
  Result := Expression;
  if Result then
    CodeSite.Send(BytesToHexStr(APacket))
  else
    CodeSite.SendError(BytesToHexStr(APacket))
end;

procedure TCodeSiteLoggerHelper.Send(const AMsg: String; const APacket: TBytes);
begin
  CodeSite.Send(AMsg, BytesToHexStr(APacket))
end;

initialization
  AssertErrorProc := CodeSiteAssertErrorHandler;

end.
