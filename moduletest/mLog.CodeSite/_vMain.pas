unit _vMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TvMain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
  public
  end;

var
  vMain: TvMain;

implementation

{$R *.dfm}

uses
  mLog, mLog.CodeSite;

procedure TvMain.Button1Click(Sender: TObject);
begin
  Log.Clear;
end;

procedure TvMain.Button2Click(Sender: TObject);
var
  LBytes: TBytes;
begin
  Log.Msg('test');
  Log.Enter('FontSize');
  Log.Msg(False, 'FontSize: %s', [Font.Name]);

  SetLength(LBytes, 3);
  LBytes[0] := $10;
  LBytes[1] := $13;
  LBytes[2] := $09;

  Log.Snd(False, 'FontSize: %s', [Font.Name], LBytes);
  Log.Rcv(True, 'FontSize: %s', [Font.Name], LBytes);
  Log.Exit('FontSize');
  Log.Rcv(True, LBytes);
  Log.Error('error log', LBytes);
end;

end.
