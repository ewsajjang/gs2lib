unit mLog.Windows;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  Vcl.StdCtrls, Vcl.Forms, Vcl.ComCtrls, System.StrUtils,
  WinApi.Windows


  ;

procedure LogInit(const AFileLog: Boolean; const ADateTimeFormat: String = '[YYYY-MM-DD HH:NN]');
procedure wl(const ALog: string); overload;
procedure wl(const ALog: string; const Args: array of const); overload;

var
  MemoTarget: TMemo = nil;
  RichEditTarget: TRichEdit = nil;
  RichEditTarger: TRichEdit = nil;
  MemoMaxLineCount: Integer = 10000;
  DbgStr: Boolean = False;
  FileName: String = '';
  DateTimeFormat: String = '';
  LogAppName: Boolean = False;

implementation

uses
  mDateTimeHelper, mUtils.Windows, System.IOUtils,
  System.Generics.Collections
  ;

type
  TSimpleFileLogThread = class(TThread)
  private
    FFileName : String;
    FLogQueue : TThreadedQueue<String>;
  protected
    procedure Execute; override;
  public
    constructor Create(const FileName: String);

    property LogQueue: TThreadedQueue<String> read FLogQueue;
  end;

var
  LogFileExists: Boolean = False;
  LogFileName: String = '';
  FLogTh: TSimpleFileLogThread = nil;
  FileLog: Boolean = False;

procedure MemoLog(S: String);
begin
  if Assigned(MemoTarget) then
  begin
    if Assigned(MemoTarget) and Assigned(MemoTarget.Owner) then
    begin
      if MemoTarget.Lines.Count > MemoMaxLineCount then
        MemoTarget.Clear;
      MemoTarget.Lines.Add(S);
    end;
  end
  else if Assigned(RichEditTarget) then
  begin
    if Assigned(RichEditTarget) and Assigned(RichEditTarget.Owner) then
    begin
      if RichEditTarget.Lines.Count > MemoMaxLineCount then
        RichEditTarget.Clear;
      RichEditTarget.Lines.Add(S);
    end;
  end;
end;

function SystemInfoStr: String;
var
  LFile: String;
begin
  LFile := TFile.ModuleFileName;
  try
    Result := Format('--------------------------------------------------'#13#10+
                     //'VM: %s'#13#10 +
                     '%s'#13#10+            //Windows Version, 32/64bit
                     '%s(%s)'#13#10+            //ExeName (Version)
                     'FileName: %s'#13#10+      //FileName:
                     'FileSize: %d'#13#10+      //Size:
                     'CreationTime: %s'#13#10+
                     'ModifiedTime: %s'#13#10+
                     '--------------------------------------------------',
                     [//VirtualMachineStr,
                      TOSVersion.ToString,
                      ExtractFileName(LFile), ExeVersion(LFile),
                      LFile,
                      TFile.Size(LFile),
                      TFile.GetCreationTime(LFile).ToISO8601Str,
                      TFile.GetLastWriteTime(LFile).ToISO8601Str]);
  except
  end;
end;

procedure wl(const ALog: string); overload;

var
  Log: String;
  AppName: String;
begin
  if LogAppName then
    AppName := Format('%s:: ', [ExtractFileName(TFile.ModuleFileName)])
  else
    AppName := EmptyStr;

  Log := IfThen(not AppName.IsEmpty, Format('%s%s', [AppName, ALog]), ALog);
  if DbgStr then
    OutputDebugString(PChar(Log));
  MemoLog(Log);

  if FileLog then
  begin
    if not DateTimeFormat.IsEmpty then
      Log := Format('%s %s'#13#10, [FormatDateTime(DateTimeFormat, Now), Log]);
    FLogTh.LogQueue.PushItem(Log);
  end;
end;

procedure wl(const ALog: string; const Args: array of const); overload;
begin
  wl(Format('%s', [Format(ALog, Args)]));
end;

function DefulatLogFileName: String;
begin
  Result := ChangeFileExt(TFile.ModuleFileName, '.log');
end;

procedure LogInit(const AFileLog: Boolean; const ADateTimeFormat: String);
begin
  FileLog := AFileLog;
  DateTimeFormat := ADateTimeFormat;
  if FileName <> EmptyStr then
    LogFileName := FileName
  else
    LogFileName := DefulatLogFileName;
  if FileLog then
  begin
    if FLogTh = nil then
      FLogTh := TSimpleFileLogThread.Create(LogFileName);
    FLogTh.LogQueue.PushItem(SystemInfoStr);
  end;
end;

{ TSimpleFileLogThread }

constructor TSimpleFileLogThread.Create(const FileName: String);
begin
  inherited Create(False);

  FFileName := FileName;
  FreeOnTerminate := True;
end;

procedure TSimpleFileLogThread.execute;
var
  LLogFile : TFileStream;
  LFileMode : Word;
  LItem : String;
  LQueueSize: Integer;
begin
  NameThreadForDebugging('TSimpleFileLogThread');

  if FileExists(FFileName) then
    LFileMode := fmOpenWrite or fmShareDenyWrite
  else
    LFileMode := fmCreate or fmShareDenyWrite;

  FLogQueue := TThreadedQueue<String>.Create;
  LLogFile := TFileStream.Create(FFileName, LFileMode);
  try
    while not Terminated do
      if FLogQueue.PopItem(LQueueSize, LItem) = wrSignaled then
        if not Terminated then
        begin
          if not LItem.EndsWith(#13#10) then
            LItem := LItem + #13#10;
          LLogFile.Write(LItem[1], Length(LItem) * SizeOf(Char))
        end;
  finally
    LLogFile.Free;
    FLogQueue.Free;
  end;
end;

initialization

finalization
  if FLogTh <> nil then
    FLogTh.Terminate;

end.
