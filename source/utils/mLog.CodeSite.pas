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

    procedure Enter(const AValue: String);
    procedure Exit(const AValue: String);

    procedure Snd(const APacket: TBytes); overload;
    procedure Snd(const AMsg: String; const APacket: TBytes); overload;
    procedure Snd(const AMsg: String; const Args: array of const; const APacket: TBytes); overload;

    procedure Rcv(const APacket: TBytes); overload;
    procedure Rcv(const AMsg: String; const APacket: TBytes); overload;
    procedure Rcv(const AMsg: String; const Args: array of const; const APacket: TBytes); overload;

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
  csmSnd = csmOrange;
  csmRcv = csmGreen;

constructor TLogCodeSite.Create;
begin
  FCodeSiteDest := TCodeSiteDestination.Create(nil);
  FCodeSiteDest.Viewer.Active := True;
  FCodeSiteDest.LogFile.Active := False;
  CodeSite.Destination := FCodeSiteDest;
  CodeSite.Send(ClassName + '.Create');
end;

destructor TLogCodeSite.Destroy;
begin
  CodeSite.Send(TLogCodeSite.ClassName + '.Destory');
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
    Msg(AMsg)
  else
    Error(AMsg);
end;

procedure TLogCodeSite.Msg(const AErCondition: Boolean; const AMsg: String;
  const Args: array of const);
begin
  Msg(AErCondition, Format(AMsg, Args));
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
  CodeSite.Send(csmRcv, BytesToHexStr(APacket));
end;

procedure TLogCodeSite.Rcv(const APacket: TBytes);
begin
  CodeSite.Send(csmRcv, BytesToHexStr(APacket));
end;

procedure TLogCodeSite.Snd(const APacket: TBytes);
begin
  CodeSite.Send(csmSnd, BytesToHexStr(APacket));
end;

procedure TLogCodeSite.Snd(const AMsg: String; const APacket: TBytes);
begin
  CodeSite.Send(csmSnd, BytesToHexStr(APacket));
end;

procedure TLogCodeSite.Snd(const AMsg: String; const Args: array of const;
  const APacket: TBytes);
begin
  Snd(Format(AMsg, Args), APacket);
end;

end.
