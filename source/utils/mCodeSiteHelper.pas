unit mCodeSiteHelper;

interface

uses
  CodeSiteLogging,
  System.Classes, System.SysUtils, System.Types
  ;

type
  TCodeSiteLoggerHelper = class helper for TCodeSiteLogger
    procedure Send(const List: TStringList); overload;
    procedure Send(const Expression: Boolean; Msg: String); overload;
    procedure Send(const Expression: Boolean; Fmt: String; Args: array of const); overload;
    procedure Send(const Fmt: String; Args: array of const; List: TStrings); overload;

    procedure Send(const AMsg: String; const ARect: TRect); overload;
    procedure Send(const AMsg: String; Args: array of const; const ARect: TRect); overload;

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
  mSysUtilsEx, mTypesHelper
  ;

{ TCodeSiteLoggerHelper }

procedure TCodeSiteLoggerHelper.Send(const Expression: Boolean; Msg: String);
begin
  if not Expression then
    CodeSite.SendError(Msg)
  else
    CodeSite.Send(Msg);
end;

procedure TCodeSiteLoggerHelper.Send(const Expression: Boolean; Fmt: String;
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
