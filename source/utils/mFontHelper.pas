unit mFontHelper;

interface

uses
  System.SysUtils, System.Classes,

  {$IFDEF MACOS or iOS}
    mLog.osx
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Vcl.Forms, Vcl.Controls
    {$ENDIF}
  {$ENDIF}
  ;

type
  TFixedWidthFont = class
  private class var
    FFIXED_WIDTH_FONT_NAME: String;
    FFixedFontExists: Boolean;
    class function SystemFixedWidthFontName: String;
  private const
    FONTS_FIXED_WIDTH: array[0..6] of String = ('Source Code Pro', 'Consolas', 'Courier', 'Lucida Console', 'Terminal', 'FixedSys', 'Hyperfont');
  public
    class function IsFixedWidthFont(const AFontName: String): Boolean;
    class procedure AssingToCtrl(const ACtrl: TControl);
    class procedure AssingToCtrls(const ACtrls: array of TControl);

    class property Exists: Boolean read FFixedFontExists;
    class property Name: String read FFIXED_WIDTH_FONT_NAME;
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

class procedure TFixedWidthFont.AssingToCtrl(const ACtrl: TControl);
var
  LCtrl: TControltFontHelper;
begin
  LCtrl := TControltFontHelper(ACtrl);
  if not TFixedWidthFont.IsFixedWidthFont(LCtrl.FontName) then
    LCtrl.FontName := FFIXED_WIDTH_FONT_NAME;
end;

class procedure TFixedWidthFont.AssingToCtrls(const ACtrls: array of TControl);
var
  LCtrl: TControl;
begin
  for LCtrl in ACtrls do
    AssingToCtrl(LCtrl);
end;

class function TFixedWidthFont.IsFixedWidthFont(const AFontName: String): Boolean;
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

class function TFixedWidthFont.SystemFixedWidthFontName: String;
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
  TFixedWidthFont.FFIXED_WIDTH_FONT_NAME := TFixedWidthFont.SystemFixedWidthFontName;
  TFixedWidthFont.FFixedFontExists := not TFixedWidthFont.FFIXED_WIDTH_FONT_NAME.IsEmpty;

finalization

end.
