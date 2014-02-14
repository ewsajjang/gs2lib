unit mLog.Common;

interface

uses
  System.SysUtils;

type
  TLogKind = (lkEnter, lkExit, lkSnd, lkRcv, lkMsg, lkMsgEr, lkErr);
  TLogKindHelper = record helper for TLogKind
  private const
    LOG_HEADERS: array[lkEnter..lkErr] of String = (
      '-->> ',
      '<<-- ',
      'snd',
      'rcv',
      'msg',
      'ErMsg',
      'Error'
    );
  private
    class var MAX_STR_LEN: Integer;
    class procedure Init; static;
  public
    function Str: String; overload;
    function Str(const AMsg: String): String; overload;
    function Str(const AMsg: String; const Args: array of const; const APacket: TBytes): String; overload;
    function Str(const APacket: TBytes): String; overload;
    function Str(const AMsg: String; const APacket: TBytes): String; overload;
    function Str(const AObj: TObject): String; overload;
    function Str(const AObj: TObject; const AMsg: String): String; overload;
    function Str(const AObj: TObject; const AMsg: String; const Args: array of const): String; overload;
  end;

implementation

uses
  mSysUtilsEx;

{ TLogKindHelper }

class procedure TLogKindHelper.Init;
var
  LStr: String;
  LLen: Integer;
begin
  MAX_STR_LEN := 0;
  for LStr in LOG_HEADERS do
  begin
    LLen := LStr.Length;
    if LLen > MAX_STR_LEN then
      MAX_STR_LEN := LLen;
  end;
  MAX_STR_LEN := LLen + 2;
end;

function TLogKindHelper.Str: String;
begin
  Result := Format('%-'+IntToStr(MAX_STR_LEN)+'s ', [LOG_HEADERS[Self]]);
end;

function TLogKindHelper.Str(const AMsg: String): String;
begin
  Result := Format('%-'+IntToStr(MAX_STR_LEN)+'s,%s', [LOG_HEADERS[Self], AMsg]);
end;

function TLogKindHelper.Str(const AObj: TObject): String;
begin
  Result := Format('%-'+IntToStr(MAX_STR_LEN)+'s,%s', [LOG_HEADERS[Self], AObj.ClassName]);
end;

function TLogKindHelper.Str(const APacket: TBytes): String;
begin
  Result := Format('%-'+IntToStr(MAX_STR_LEN)+'s,%s', [LOG_HEADERS[Self], BytesToHexStr(APacket)]);
end;

function TLogKindHelper.Str(const AMsg: String; const APacket: TBytes): String;
begin
  Result := Format('%-'+IntToStr(MAX_STR_LEN)+'s,%s,%s', [LOG_HEADERS[Self], AMsg, BytesToHexStr(APacket)]);
end;

function TLogKindHelper.Str(const AObj: TObject; const AMsg: String): String;
begin
  Result := Format('%-'+IntToStr(MAX_STR_LEN)+'s,%s,%s', [LOG_HEADERS[Self], AObj.ClassName, AMsg]);
end;

function TLogKindHelper.Str(const AMsg: String;
  const Args: array of const; const APacket: TBytes): String;
begin
  Result := Format('%-'+IntToStr(MAX_STR_LEN)+'s,%s,%s', [LOG_HEADERS[Self], Format(AMsg, Args), BytesToHexStr(APacket)]);
end;

function TLogKindHelper.Str(const AObj: TObject; const AMsg: String;
  const Args: array of const): String;
begin
  Result := Format('%-'+IntToStr(MAX_STR_LEN)+'s,%s,%s', [LOG_HEADERS[Self], Format(AMsg, Args), AObj.ClassName]);
end;

initialization
  TLogKind.Init;

finalization

end.
