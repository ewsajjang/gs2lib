program mLogMemoSample;

uses
  Vcl.Forms,
  _vMain in '_vMain.pas' {vMain},
  mLog.TMemo in '..\..\source\utils.win\mLog.TMemo.pas',
  mLog in '..\..\source\utils\mLog.pas',
  mSysUtilsEx in '..\..\source\utils\mSysUtilsEx.pas',
  mLog.Threads in '..\..\source\utils\mLog.Threads.pas',
  mLog.Common in '..\..\source\utils\mLog.Common.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TvMain, vMain);
  Application.Run;
end.
