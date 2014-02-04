unit mvw.vForm.helper;

interface

uses
  mvw.vForm,
  System.Classes, System.SysUtils, System.Generics.Collections,
  Vcl.Forms, Vcl.Controls;

type
  TvFormHelper = class helper for TvForm
  private
  public
    procedure TabOrderAlign;
    procedure PlaceOn(const ASource: TForm; ATarget: TWinControl);
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

procedure TvFormHelper.PlaceOn(const ASource: TForm; ATarget: TWinControl);
begin
  ASource.Parent := ATarget;
  ASource.Align := alClient;
  ASource.Show;
end;

end.
