unit _vMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TvMain = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
  public
  end;

var
  vMain: TvMain;

implementation

{$R *.dfm}

uses
  mLog, mLog.TMemo;

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
  Log.Msg(False, 'FontSize: %s', [Memo1.Font.Name]);

  SetLength(LBytes, 3);
  LBytes[0] := $10;
  LBytes[1] := $13;
  LBytes[2] := $09;

  Log.Snd(False, 'FontSize: %s', [Memo1.Font.Name], LBytes);
  Log.Rcv(True, 'FontSize: %s', [Memo1.Font.Name], LBytes);
  Log.Exit('FontSize');
  Log.Rcv(True, LBytes);
  Log.Rcv(False, LBytes);
  Log.Error('error log', LBytes);
end;

procedure TvMain.CheckBox1Click(Sender: TObject);
var
  LCheckBox: TCheckBox absolute Sender;
begin
  (Log as TLogMemo).AutoScroll := LCheckBox.Checked;
end;

procedure TvMain.FormCreate(Sender: TObject);
begin
  (Log as TLogMemo).Memo := Memo1;
end;

end.
