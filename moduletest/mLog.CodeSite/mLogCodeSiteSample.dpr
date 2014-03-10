program mLogCodeSiteSample;

uses
  Vcl.Forms,
  _vMain in '_vMain.pas' {vMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TvMain, vMain);
  Application.Run;
end.
