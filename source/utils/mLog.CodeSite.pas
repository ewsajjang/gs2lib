unit mLog.CodeSite;

interface

uses
  mLog,
  CodeSiteLogging,
  System.Classes, System.SysUtils;

type
  TLogCodeSite = class(TLog)
  private
    FCodeSiteDest: TCodeSiteDestination;
    FViewActive: Boolean;
    procedure SetViewActive(const Value: Boolean);
    function GetInstalled: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;

    procedure Enter(const AValue: String); override;
    procedure Exit(const AValue: String); override;

    procedure BeginUpdate; override;
    procedure EndUpdate; override;

    procedure Snd(const APacket: TBytes); override;
    procedure Snd(const AMsg: String; const APacket: TBytes); override;
    procedure Snd(const AErCondition: Boolean; const APacket: TBytes); override;
    procedure Snd(const AErCondition: Boolean; const AMsg: String; const APacket: TBytes); override;
    procedure Snd(const AErCondition: Boolean; const AMsg: String; const Args: array of const; const APacket: TBytes); override;

    procedure Rcv(const APacket: TBytes); override;
    procedure Rcv(const AMsg: String; const APacket: TBytes); override;
    procedure Rcv(const AErCondition: Boolean; const APacket: TBytes); override;
    procedure Rcv(const AErCondition: Boolean; const AMsg: String; const APacket: TBytes); override;
    procedure Rcv(const AErCondition: Boolean; const AMsg: String; const Args: array of const; const APacket: TBytes); override;

    procedure Msg(const AMsg: String); override;
    procedure Msg(const APacket: TBytes); override;
    procedure Msg(const AMsg: String; const APacket: TBytes); override;
    procedure Msg(const AMsg: String; const Args: array of const; const APacket: TBytes); override;
    procedure Msg(const AMsg: String; const Args: array of const; const APacket: TBytes; const ASize: Int64); override;
    function Msg(const AErCondition: Boolean; const AMsg: String): Boolean; override;

    procedure Error(const AMsg: String); override;
    procedure Error(const AMsg: String; const APacket: TBytes); override;
    procedure Error(const Sender: TObject; const AMsg: String); override;
    procedure Error(const Sender: TObject; const AMsg: String; const Args: array of const); override;
    procedure Error(const Sender: TObject; const AMsg: String; const APacket: TBytes); override;

    property ViewActivate: Boolean read FViewActive write SetViewActive;
    property Installed: Boolean read GetInstalled;
  end;

function Log: TLogCodeSite;

var
  ShowCodeSiteLogOnApplicationRun: Boolean = False;

implementation

uses
  mSysUtilsEx, Vcl.Dialogs;

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
  cmsSuccess = csmGreen;
  csmSnd = csmViolet;
  csmRcv = csmYellow;
  csmMsgError = csmRed;

function Log: TLogCodeSite;
begin
  if not Assigned(mLog.Log) then
    mLog.Log := TLogCodeSite.Create;

  Result := mLog.Log as TLogCodeSite;
end;

procedure TLogCodeSite.BeginUpdate;
begin
  FCodeSiteDest.BeginUpdate;
end;

procedure TLogCodeSite.Clear;
begin
  CodeSite.Clear;
end;

constructor TLogCodeSite.Create;
begin
  FCodeSiteDest := TCodeSiteDestination.Create(nil);
{$IFDEF DEBUG}
  FCodeSiteDest.Viewer.Active := CodeSite.Installed;
{$ELSE}
  FCodeSiteDest.Viewer.Active := ShowCodeSiteLogOnApplicationRun and CodeSite.Installed;
{$ENDIF}
  FCodeSiteDest.LogFile.Active := False;
  CodeSite.Destination := FCodeSiteDest;
  //CodeSite.Clear;



end;

destructor TLogCodeSite.Destroy;
begin
  FreeAndNil(FCodeSiteDest);

  inherited;
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

procedure TLogCodeSite.EndUpdate;
begin
  FCodeSiteDest.EndUpdate;
end;

procedure TLogCodeSite.Enter(const AValue: String);
begin
  CodeSite.EnterMethod(AValue);
end;

procedure TLogCodeSite.Exit(const AValue: String);
begin
  CodeSite.ExitMethod(AValue);
end;

function TLogCodeSite.GetInstalled: Boolean;
begin
  Result := CodeSite.Installed;
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

function TLogCodeSite.Msg(const AErCondition: Boolean; const AMsg: String): Boolean;
begin
  Result := AErCondition;
  if AErCondition then
    CodeSite.SendMsg(cmsSuccess, AMsg)
  else
    CodeSite.SendMsg(csmMsgError, AMsg);
end;

procedure TLogCodeSite.Msg(const AMsg: String; const Args: array of const;
  const APacket: TBytes; const ASize: Int64);
begin
  CodeSite.Send('[%s]%s', [Format(AMsg, Args), BytesToHexStr(APacket, 0, ASize)]);
end;

procedure TLogCodeSite.Msg(const AMsg: String; const APacket: TBytes);
begin
  CodeSite.Send('[%s]%s', [AMsg, BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.Msg(const AMsg: String; const Args: array of const;
  const APacket: TBytes);
begin
  CodeSite.Send('[%s]%s', [Format(AMsg, Args), BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.Msg(const APacket: TBytes);
begin
  CodeSite.Send(BytesToHexStr(APacket));
end;

procedure TLogCodeSite.Rcv(const AErCondition: Boolean; const APacket: TBytes);
begin
  if AErCondition then
    Rcv(APacket)
  else
    CodeSite.Send(csmError, '[rcv]%s', [BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.Rcv(const AErCondition: Boolean; const AMsg: String;
  const APacket: TBytes);
begin
  if AErCondition then
    Rcv(AMsg, APacket)
  else
    CodeSite.Send(csmError, '[rcv|%s]%s', [AMsg, BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.Rcv(const AErCondition: Boolean; const AMsg: String;
  const Args: array of const; const APacket: TBytes);
begin
  if AErCondition then
    Rcv(AMsg, Args, APacket)
  else
    CodeSite.Send(csmError, '[rcv|%s]%s', [Format(AMsg, Args), BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.Msg(const AMsg: String);
begin
  CodeSite.SendMsg(AMsg);
end;

procedure TLogCodeSite.Rcv(const AMsg: String; const APacket: TBytes);
begin
  CodeSite.Send(csmRcv, '[rcv|%s]%s', [AMsg, BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.Rcv(const APacket: TBytes);
begin
  CodeSite.Send(csmRcv, '[rcv]%s', [BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.Snd(const APacket: TBytes);
begin
  CodeSite.Send(csmSnd, '[snd]%s', [BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.Snd(const AMsg: String; const APacket: TBytes);
begin
  CodeSite.Send(csmSnd, '[snd|%s]%s', [AMsg, BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.Snd(const AErCondition: Boolean; const APacket: TBytes);
begin
  if AErCondition then
    Snd(APacket)
  else
    CodeSite.Send(csmError, '[snd]%s', [BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.Snd(const AErCondition: Boolean; const AMsg: String;
  const APacket: TBytes);
begin
  if AErCondition then
    Snd(AMsg, APacket)
  else
    CodeSite.Send(csmError, '[snd|%s] %s', [AMsg, BytesToHexStr(APacket)]);
end;

procedure TLogCodeSite.SetViewActive(const Value: Boolean);
begin
  FViewActive := Value;
  CodeSite.Destination.Viewer.Active := FViewActive and CodeSite.Installed;
end;

procedure TLogCodeSite.Snd(const AErCondition: Boolean; const AMsg: String;
  const Args: array of const; const APacket: TBytes);
begin
  if AErCondition then
    Snd(AMsg, Args, APacket)
  else
    CodeSite.Send(csmError, '[snd|%s]%s', [Format(AMsg, Args), BytesToHexStr(APacket)]);
end;

initialization
  {$IFDEF DEBUG}
    ShowCodeSiteLogOnApplicationRun := True;
  {$ENDIF}

finalization
  if Assigned(mLog.Log) then
    mLog.Log := nil;

end.
