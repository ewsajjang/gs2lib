unit mLog.CodeSite;

interface

uses
  CodeSiteLogging,
  System.Classes, System.SysUtils;

type
  TLogCodeSite = class
  private
    FCodeSiteDest: TCodeSiteDestination;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure Enter(const AValue: String);
    procedure Exit(const AValue: String);

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

    procedure Msg(const AMsg: String); overload;
    procedure Msg(const AMsg: String; const Args: array of const); overload;

    procedure Msg(const AErCondition: Boolean; const AMsg: String); overload;
    procedure Msg(const AErCondition: Boolean; const AMsg: String; const Args: array of const); overload;

    procedure Error(const AMsg: String); overload;
    procedure Error(const AMsg: String; const Args: array of const); overload;
    procedure Error(const AMsg: String; const APacket: TBytes); overload;
    procedure Error(const AMsg: String; const Args: array of const; const APacket: TBytes); overload;

    procedure Error(const Sender: TObject; const AMsg: String); overload;
    procedure Error(const Sender: TObject; const AMsg: String; const Args: array of const); overload;
    procedure Error(const Sender: TObject; const AMsg: String; const APacket: TBytes); overload;
    procedure Error(const Sender: TObject; const AMsg: String; const Args: array of const; const APacket: TBytes); overload;
  end;

{IFDEF DEBUG}

var
  Log: TLogCodeSite = nil;

{ENDIF DEBUG}

implementation

uses
  mSysUtilsEx;

const
  STR_ERROR = 'ERROR';

{ TLogCodeSite }

//csmInfo	              $0
//csmWarning	          $1
//csmError	            $2
//csmCheckPoint	        $3
//csmReminder	          $17
//csmSeparator	        $b
//csmInactiveSeparator	$c
//csmLevel1, csmRed	    $18
//csmLevel2, csmOrange	$19
//csmLevel3, csmYellow	$1a
//csmLevel4, csmGreen	  $1b
//csmLevel5, csmBlue	  $1c
//csmLevel6, csmIndigo	$1d
//csmLevel7, csmViolet	$1e
  cmsSuccess = csmYellow;
  csmSnd = csmIndigo;
  csmRcv = csmGreen;

procedure TLogCodeSite.Clear;
begin
  CodeSite.Clear;
end;

constructor TLogCodeSite.Create;
begin
  FCodeSiteDest := TCodeSiteDestination.Create(nil);
  FCodeSiteDest.Viewer.Active := CodeSite.Installed;
  FCodeSiteDest.LogFile.Active := False;
  CodeSite.Destination := FCodeSiteDest;
  CodeSite.Clear;
end;

destructor TLogCodeSite.Destroy;
begin
  FreeAndNil(FCodeSiteDest);

  inherited;
end;

procedure TLogCodeSite.Error(const AMsg: String; const Args: array of const;
  const APacket: TBytes);
begin
  Error(Format(AMsg, Args), APacket);
end;

procedure TLogCodeSite.Error(const Sender: TObject; const AMsg: String);
begin
  CodeSite.Send(csmError, AMsg, Sender);
end;

procedure TLogCodeSite.Error(const AMsg: String; const APacket: TBytes);
begin
  CodeSite.Send(csmError, AMsg);
  CodeSite.Send(csmError, BytesToHexStr(APacket));
end;

procedure TLogCodeSite.Error(const AMsg: String);
begin
  CodeSite.Send(csmError, AMsg);
end;

procedure TLogCodeSite.Error(const AMsg: String; const Args: array of const);
begin
  Error(Format(AMsg, Args));
end;

procedure TLogCodeSite.Enter(const AValue: String);
begin
  CodeSite.EnterMethod(AValue);
end;

procedure TLogCodeSite.Error(const Sender: TObject; const AMsg: String;
  const Args: array of const; const APacket: TBytes);
begin
  Error(Sender, Format(AMsg, Args), APacket);
end;

procedure TLogCodeSite.Exit(const AValue: String);
begin
  CodeSite.ExitMethod(AValue);
end;

procedure TLogCodeSite.Error(const Sender: TObject; const AMsg: String;
  const APacket: TBytes);
begin
  CodeSite.Send(csmError, AMsg, Sender);
  CodeSite.Send(csmError, BytesToHexStr(APacket));
end;

procedure TLogCodeSite.Error(const Sender: TObject; const AMsg: String;
  const Args: array of const);
begin
  CodeSite.Send(csmError, Format(AMsg, Args), Sender);
end;

procedure TLogCodeSite.Msg(const AErCondition: Boolean; const AMsg: String);
begin
  if AErCondition then
    CodeSite.SendMsg(cmsSuccess, AMsg)
  else
    Error(AMsg);
end;

procedure TLogCodeSite.Msg(const AErCondition: Boolean; const AMsg: String;
  const Args: array of const);
begin
  Msg(AErCondition, Format(AMsg, Args));
end;

procedure TLogCodeSite.Rcv(const AErCondition: Boolean; const APacket: TBytes);
begin
  if AErCondition then
    Rcv(APacket)
  else
    CodeSite.Send(csmError, '[%s]%s', ['RCV', BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.Rcv(const AErCondition: Boolean; const AMsg: String;
  const APacket: TBytes);
begin
  if AErCondition then
    Rcv(AMsg, APacket)
  else
    CodeSite.Send(csmError, '[%s]%s', ['RCV', BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.Rcv(const AErCondition: Boolean; const AMsg: String;
  const Args: array of const; const APacket: TBytes);
begin
  if AErCondition then
    Rcv(AMsg, Args, APacket)
  else
    CodeSite.Send(csmError, '[%s%s]%s', ['RCV', Format(AMsg, Args), BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.Msg(const AMsg: String);
begin
  CodeSite.SendMsg(AMsg);
end;

procedure TLogCodeSite.Msg(const AMsg: String; const Args: array of const);
begin
  Msg(Format(AMsg, Args));
end;

procedure TLogCodeSite.Rcv(const AMsg: String; const Args: array of const;
  const APacket: TBytes);
begin
  Rcv(Format(AMsg, Args), APacket);
end;

procedure TLogCodeSite.Rcv(const AMsg: String; const APacket: TBytes);
begin
  CodeSite.Send(csmRcv, '[%s]%s', [AMsg, BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.Rcv(const APacket: TBytes);
begin
  CodeSite.Send(csmRcv, '[%s]%s', ['RCV', BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.Snd(const APacket: TBytes);
begin
  CodeSite.Send(csmSnd, '[%s]%s', ['SND', BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.Snd(const AMsg: String; const APacket: TBytes);
begin
  CodeSite.Send(csmSnd, '[%s]%s', [AMsg, BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.Snd(const AMsg: String; const Args: array of const;
  const APacket: TBytes);
begin
  Snd(Format(AMsg, Args), APacket);
end;

{IFDEF DEBUG}

procedure TLogCodeSite.Snd(const AErCondition: Boolean; const APacket: TBytes);
begin
  if AErCondition then
    Snd(APacket)
  else
    CodeSite.Send(csmError, '[%s]%s', ['Snd', BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.Snd(const AErCondition: Boolean; const AMsg: String;
  const APacket: TBytes);
begin
  if AErCondition then
    Snd(AMsg, APacket)
  else
    CodeSite.Send(csmError, '[%s]%s', ['Snd', BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.Snd(const AErCondition: Boolean; const AMsg: String;
  const Args: array of const; const APacket: TBytes);
begin
  if AErCondition then
    Snd(AMsg, Args, APacket)
  else
    CodeSite.Send(csmError, '[%s%s]%s', ['Snd', Format(AMsg, Args), BytesToHexStr(APacket)]);
end;

initialization
  if not Assigned(Log) then
    Log := TLogCodeSite.Create;

finalization
  if Assigned(Log) then
    FreeAndNil(Log);

{ENDIF DEBUG}

end.
