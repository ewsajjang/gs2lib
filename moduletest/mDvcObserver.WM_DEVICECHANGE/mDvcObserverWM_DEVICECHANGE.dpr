program mDvcObserverWM_DEVICECHANGE;

uses
  Vcl.Forms,
  _v.Main in '_v.Main.pas' {vMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TvMain, vMain);
  Application.Run;
end.
