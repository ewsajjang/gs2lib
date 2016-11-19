unit mCodeSiteHelper;

interface

uses
  CodeSiteLogging,
  System.Classes, System.SysUtils, System.Types
  ;

type
  TCodeSiteLoggerHelper = class helper for TCodeSiteLogger

    procedure EnterMethod(const AMethodName: String; Args: array of const); overload;
    procedure ExitMethod(const AMethodName: String; Args: array of const); overload;
    procedure Send(const List: TStringList); overload;

    procedure Send(const Fmt: String; Args: array of const; List: TStrings); overload;

    procedure Send(const AMsg: String; const ARect: TRect); overload;
    procedure Send(const AMsg: String; Args: array of const; const ARect: TRect); overload;


    procedure Send(const APacket: TBytes); overload;
    procedure Send(const AMsg: String; APacket: TBytes); overload;
    procedure Send(const APacket: TBytes; const ALength: Integer); overload;
    procedure Send(const AMsg: String; const APacket: TBytes; const ALength: Integer); overload;

    procedure Send(const ABuffer: Pointer; const ALength: Integer); overload;
    procedure Send(const AMsg: String; const ABuffer: Pointer; const ALength: Integer); overload;
    procedure Send(const AMsg: String; Args: array of const; const ABuffer: Pointer; const ALength: Integer); overload;

    //procedure Send(const AMsg: String; APacket: array of Byte); overload;
    procedure Send(const AMsg: String; Args: array of const; const APacket: TBytes); overload;

    function Send(const Expression: Boolean; Msg: String): Boolean; overload;
    function Send(const Expression: Boolean; Fmt: String; Args: array of const): Boolean; overload;
    function Send(const Expression: Boolean; const APacket: TBytes): Boolean; overload;
    function Send(const Expression: Boolean; const AMsg: String; const AValue: String): Boolean; overload;
    function Send(const Expression: Boolean; const AMsg: String; const APacket: TBytes): Boolean; overload;
    function Send(const Expression: Boolean; const AMsg: String; const Args: array of const; const APacket: TBytes): Boolean; overload;

    procedure SendError(const AMsg: String; const Args: array of const; const APacket: TBytes); overload;
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
  if Result then
    Send(Msg)
  else
    SendError(Msg);
end;

function TCodeSiteLoggerHelper.Send(const Expression: Boolean; Fmt: String;
  Args: array of const): Boolean;
begin
  Result := Expression;
  if Result then
    Send(Fmt, Args)
  else
    SendError(Fmt, Args);
end;

function TCodeSiteLoggerHelper.Send(const Expression: Boolean;
  const AMsg: String; const APacket: TBytes): Boolean;
begin
  Result := Expression;
  if Result then
    Send(AMsg, BytesToHexStr(APacket))
  else
    SendError('[%s]%s', [AMsg, BytesToHexStr(APacket)])
end;

procedure TCodeSiteLoggerHelper.EnterMethod(const AMethodName: String;
  Args: array of const);
begin
  EnterMethod(Format(AMethodName, Args));
end;

procedure TCodeSiteLoggerHelper.ExitMethod(const AMethodName: String;
  Args: array of const);
begin
  ExitMethod(Format(AMethodName, Args))
end;

function TCodeSiteLoggerHelper.Send(const Expression: Boolean;
  const AMsg: String; const Args: array of const; const APacket: TBytes): Boolean;
begin
  Result := Expression;
  if Result then
    Send(Format(AMsg, Args), BytesToHexStr(APacket))
  else
    SendError('[%s]%s', [Format(AMsg, Args), BytesToHexStr(APacket)])
end;

procedure TCodeSiteLoggerHelper.Send(const AMsg: String; Args: array of const;
  const ABuffer: Pointer; const ALength: Integer);
begin
  Send(Format(AMsg, Args), ABuffer, ALength);
end;

procedure TCodeSiteLoggerHelper.Send(const APacket: TBytes;
  const ALength: Integer);
begin
  Send(BytesToHexStr(APacket, 0, ALength));
end;

procedure TCodeSiteLoggerHelper.Send(const AMsg: String; const ABuffer: Pointer;
  const ALength: Integer);
var
  LBuffer: TBytes;
begin
  SetLength(LBuffer, ALength);
  Move(ABuffer^, LBuffer[0], ALength);
  Send(AMsg, LBuffer);
end;

procedure TCodeSiteLoggerHelper.Send(const AMsg: String; const APacket: TBytes;
  const ALength: Integer);
begin
  Send(AMsg, BytesToHexStr(APacket, 0, ALength));
end;

procedure TCodeSiteLoggerHelper.Send(const ABuffer: Pointer;
  const ALength: Integer);
var
  LBuffer: TBytes;
begin
  SetLength(LBuffer, ALength);
  Move(ABuffer^, LBuffer[0], ALength);
  Send(LBuffer);
end;

procedure TCodeSiteLoggerHelper.SendError(const AMsg: String;
  const Args: array of const; const APacket: TBytes);
begin
  SendError('[%s]%s', [Format(AMsg, Args), BytesToHexStr(APacket)])
end;

function TCodeSiteLoggerHelper.Send(const Expression: Boolean; const AMsg,
  AValue: String): Boolean;
begin
  Result := Expression;
  if Result then
    Send(AMsg, AValue)
  else
    SendError('%s = %s', [AMsg, AValue])
end;

procedure TCodeSiteLoggerHelper.Send(const AMsg: String; const ARect: TRect);
begin
  Send(AMsg, ARect.ToString);
end;

procedure TCodeSiteLoggerHelper.Send(const AMsg: String; Args: array of const;
  const ARect: TRect);
begin
  Send(Format(AMsg, Args), ARect.ToString);
end;

procedure TCodeSiteLoggerHelper.Send(const List: TStringList);
begin
  Send('', List);
end;

procedure TCodeSiteLoggerHelper.Send(const Fmt: String; Args: array of const;
  List: TStrings);
begin
  Send(Format(Fmt, Args), List);
end;

//procedure TCodeSiteLoggerHelper.Send(const AMsg: String;
//  APacket: array of Byte);
//begin
//  Send(AMsg, BytesToHexStr(APacket))
//end;

procedure TCodeSiteLoggerHelper.Send(const AMsg: String; Args: array of const;
  const APacket: TBytes);
begin
  Send(Format(AMsg, Args), BytesToHexStr(APacket))
end;

procedure TCodeSiteLoggerHelper.Send(const APacket: TBytes);
begin
  Send(BytesToHexStr(APacket))
end;

function TCodeSiteLoggerHelper.Send(const Expression: Boolean;
  const APacket: TBytes): Boolean;
begin
  Result := Expression;
  if Result then
    Send(BytesToHexStr(APacket))
  else
    SendError(BytesToHexStr(APacket))
end;

procedure TCodeSiteLoggerHelper.Send(const AMsg: String; APacket: TBytes);
begin
  Send(AMsg, BytesToHexStr(APacket))
end;

initialization
  AssertErrorProc := CodeSiteAssertErrorHandler;

end.
