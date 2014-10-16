unit mvw.vForm.helper;

interface

uses
  mvw.vForm,
  System.Classes, System.SysUtils, System.Generics.Collections,
  Vcl.Forms, Vcl.Controls,  Vcl.StdCtrls;

type
  TvFormHelper = class helper for TvForm
  private
  protected
    procedure OnEditDoubleKey(Sender: TObject; var Key: Char);
    procedure OnEditIntegerKey(Sender: TObject; var Key: Char);
  public
    procedure TabOrderAlign;
    procedure FontAssign(const AFontName: String);
  end;

implementation

uses
  mFormUtils;

{ TFormHelper }

procedure TvFormHelper.TabOrderAlign;
begin
  mFormUtils.TabOrderAlign(Self);
end;

procedure TvFormHelper.FontAssign(const AFontName: String);
begin
  mFormUtils.FontAssign(Self, AFontName);
end;

procedure TvFormHelper.OnEditDoubleKey(Sender: TObject; var Key: Char);
var
  LText: String;
begin
  LText := (Sender as TEdit).Text;
  if (not CharInSet(Key, [#8, '0'..'9', FormatSettings.DecimalSeparator])) or
     (Key = FormatSettings.DecimalSeparator) and LText.Contains(FormatSettings.DecimalSeparator) then
    Key := #0;
end;

procedure TvFormHelper.OnEditIntegerKey(Sender: TObject; var Key: Char);
var
  LText: String;
begin
  LText := (Sender as TEdit).Text;
  if (not CharInSet(Key, [#8, '0'..'9'])) or
     (Key = FormatSettings.DecimalSeparator) and LText.Contains(FormatSettings.DecimalSeparator) then
    Key := #0;
end;

end.
