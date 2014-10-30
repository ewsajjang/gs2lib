unit mLog;

interface

uses
  System.Classes, System.SysUtils;

type
  ILog = interface
    ['{A25E6427-6C8D-4E72-8BD5-2AEFD2EA8286}']
    procedure Clear;

    procedure Enter(const AValue: String);
    procedure Exit(const AValue: String);

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Snd(const APacket: TBytes); overload;
    procedure Snd(const AMsg: String; const APacket: TBytes); overload;
    procedure Snd(const AMsg: String; const Args: array of const; const APacket: TBytes); overload;
    procedure Snd(const AErCondition: Boolean; const APacket: TBytes); overload;
    procedure Snd(const AErCondition: Boolean; const AMsg: String; const APacket: TBytes); overload;
    procedure Snd(const AErCondition: Boolean; const AMsg: String; const Args: array of const; const APacket: TBytes); overload;

    procedure Rcv(const APacket: TBytes); overload;
    procedure Rcv(const AMsg: String; const APacket: TBytes); overload;
    procedure Rcv(const AMsg: String; const Args: array of const; const APacket: TBytes); overload;
    procedure Rcv(const AErCondition: Boolean; const APacket: TBytes); overload;
    procedure Rcv(const AErCondition: Boolean; const AMsg: String; const APacket: TBytes); overload;
    procedure Rcv(const AErCondition: Boolean; const AMsg: String; const Args: array of const; const APacket: TBytes); overload;

    procedure Msg(const AMsgs: TStringList); overload;
    procedure Msg(const AMsg: String); overload;
    procedure Msg(const AMsg: String; const Args: array of const); overload;
    procedure Msg(const APacket: TBytes); overload;
    procedure Msg(const AMsg: String; const APacket: TBytes); overload;
    procedure Msg(const AMsg: String; const Args: array of const; const APacket: TBytes; const ASize: Int64); overload;
    procedure Msg(const AMsg: String; const Args: array of const; const APacket: TBytes); overload;
    function Msg(const AErCondition: Boolean; const AMsg: String): Boolean; overload;
    function Msg(const AErCondition: Boolean; const AMsg: String; const Args: array of const): Boolean; overload;

    procedure Error(const AMsg: String); overload;
    procedure Error(const AMsg: String; const Args: array of const); overload;
    procedure Error(const AMsg: String; const APacket: TBytes); overload;
    procedure Error(const AMsg: String; const Args: array of const; const APacket: TBytes); overload;
    procedure Error(const Sender: TObject; const AMsg: String); overload;
    procedure Error(const Sender: TObject; const AMsg: String; const Args: array of const); overload;
    procedure Error(const Sender: TObject; const AMsg: String; const APacket: TBytes); overload;
    procedure Error(const Sender: TObject; const AMsg: String; const Args: array of const; const APacket: TBytes); overload;
  end;

  TLog = class(TInterfacedObject, ILog)

    procedure Clear; virtual; abstract;

    procedure Enter(const AValue: String); virtual; abstract;
    procedure Exit(const AValue: String); virtual; abstract;

    procedure BeginUpdate; virtual; abstract;
    procedure EndUpdate; virtual; abstract;

    procedure Snd(const APacket: TBytes); overload; virtual; abstract;
    procedure Snd(const AMsg: String; const APacket: TBytes); overload; virtual; abstract;
    procedure Snd(const AMsg: String; const Args: array of const; const APacket: TBytes); overload;
    procedure Snd(const AErCondition: Boolean; const APacket: TBytes); overload; virtual; abstract;
    procedure Snd(const AErCondition: Boolean; const AMsg: String; const APacket: TBytes); overload; virtual; abstract;
    procedure Snd(const AErCondition: Boolean; const AMsg: String; const Args: array of const; const APacket: TBytes); overload; virtual; abstract;

    procedure Rcv(const APacket: TBytes); overload; virtual; abstract;
    procedure Rcv(const AMsg: String; const APacket: TBytes); overload; virtual; abstract;
    procedure Rcv(const AMsg: String; const Args: array of const; const APacket: TBytes); overload;
    procedure Rcv(const AErCondition: Boolean; const APacket: TBytes); overload; virtual; abstract;
    procedure Rcv(const AErCondition: Boolean; const AMsg: String; const APacket: TBytes); overload; virtual; abstract;
    procedure Rcv(const AErCondition: Boolean; const AMsg: String; const Args: array of const; const APacket: TBytes); overload; virtual; abstract;

    procedure Msg(const AMsgs: TStringList); overload;
    procedure Msg(const AMsg: String); overload; virtual; abstract;
    procedure Msg(const AMsg: String; const Args: array of const); overload;
    procedure Msg(const APacket: TBytes); overload; virtual; abstract;
    procedure Msg(const AMsg: String; const APacket: TBytes); overload; virtual; abstract;
    procedure Msg(const AMsg: String; const Args: array of const; const APacket: TBytes; const ASize: Int64); overload; virtual; abstract;
    procedure Msg(const AMsg: String; const Args: array of const; const APacket: TBytes); overload; virtual; abstract;

    function Msg(const AErCondition: Boolean; const AMsg: String): Boolean; overload; virtual; abstract;
    function Msg(const AErCondition: Boolean; const AMsg: String; const Args: array of const): Boolean; overload;

    procedure Error(const AMsg: String); overload; virtual; abstract;
    procedure Error(const AMsg: String; const Args: array of const); overload;
    procedure Error(const AMsg: String; const APacket: TBytes); overload; virtual; abstract;
    procedure Error(const AMsg: String; const Args: array of const; const APacket: TBytes); overload;

    procedure Error(const Sender: TObject; const AMsg: String); overload; virtual; abstract;
    procedure Error(const Sender: TObject; const AMsg: String; const Args: array of const); overload; virtual; abstract;
    procedure Error(const Sender: TObject; const AMsg: String; const APacket: TBytes); overload; virtual; abstract;
    procedure Error(const Sender: TObject; const AMsg: String; const Args: array of const; const APacket: TBytes); overload;
  end;

var
  Log: ILog;

implementation

{ TLog }

procedure TLog.Snd(const AMsg: String; const Args: array of const;
  const APacket: TBytes);
begin
  Snd(Format(AMsg, Args), APacket);
end;

procedure TLog.Rcv(const AMsg: String; const Args: array of const;
  const APacket: TBytes);
begin
  Rcv(Format(AMsg, Args), APacket);
end;

procedure TLog.Msg(const AMsg: String; const Args: array of const);
begin
  Msg(Format(AMsg, Args));
end;

procedure TLog.Error(const AMsg: String; const Args: array of const);
begin
  Error(Format(AMsg, Args));
end;

procedure TLog.Error(const AMsg: String; const Args: array of const;
  const APacket: TBytes);
begin
  Error(Format(AMsg, Args), APacket);
end;

procedure TLog.Error(const Sender: TObject; const AMsg: String;
  const Args: array of const; const APacket: TBytes);
begin
  Error(Sender, Format(AMsg, Args), APacket);
end;

procedure TLog.Msg(const AMsgs: TStringList);
var
  LMsg: String;
begin
  if Assigned(AMsgs) and not AMsgs.Text.IsEmpty then
    for LMsg in AMsgs do
      Msg(#9+LMsg);
end;

function TLog.Msg(const AErCondition: Boolean; const AMsg: String;
  const Args: array of const): Boolean;
begin
  Result := Msg(AErCondition, Format(AMsg, Args));
end;

end.
