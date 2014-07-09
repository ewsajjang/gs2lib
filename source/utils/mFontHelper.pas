unit mFontHelper;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Forms, Vcl.Controls
  ;

type
  TFontUtil = class
  private class var
    FIXED_WIDTH_FONT_NAME: String;
  private const
    FONTS_FIXED_WIDTH: array[0..5] of String = ('Consolas', 'Courier', 'Lucida Console', 'Terminal', 'FixedSys', 'Hyperfont');
  public
    class function IsFixedWidthFont(const AFontName: String): Boolean;
    class function SystemFixedWidthFontName: String;
    class procedure FixedFontAssing(const ACtrl: TControl);
  end;

implementation

type
  TControltFontHelper = class (TControl)
  private
    function GetFontName: String;
    procedure SetFontName(const AName: String);
  public
    property FontName: String read GetFontName write SetFontName;
  end;

{ TControltTmp }

function TControltFontHelper.GetFontName: String;
begin
  Result := Font.Name;
end;

procedure TControltFontHelper.SetFontName(const AName: String);
begin
  Font.Name := AName;
end;

{ TFontUtil }

class procedure TFontUtil.FixedFontAssing(const ACtrl: TControl);
var
  LCtrl: TControltFontHelper;
begin
  LCtrl := TControltFontHelper(ACtrl);
  if not TFontUtil.IsFixedWidthFont(LCtrl.FontName) then
    LCtrl.FontName := FIXED_WIDTH_FONT_NAME;
end;

class function TFontUtil.IsFixedWidthFont(const AFontName: String): Boolean;
var
  LName: String;
begin
  Result := False;
  for LName in FONTS_FIXED_WIDTH do
    if LName.Equals(AFontName) then
    begin
      Result := True;
      Break;
    end;
end;

class function TFontUtil.SystemFixedWidthFontName: String;
var
  LName: String;
begin
  Result := EmptyStr;
  for LName in FONTS_FIXED_WIDTH do
    if Screen.Fonts.IndexOf(LName) > -1 then
    begin
      Result := LName;
      Break;
    end;
end;


initialization
  TFontUtil.FIXED_WIDTH_FONT_NAME := TFontUtil.SystemFixedWidthFontName;

finalization

end.
