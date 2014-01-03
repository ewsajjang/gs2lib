unit v.Windows.helper;

interface

uses
  v.Windows,
  System.Classes, System.SysUtils,
  System.Generics.Collections,
  vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.Graphics;

type
  TvHelper = class helper for TvForm
    procedure AlignTabOrder;
    procedure PlaceOn(const ASource: TForm; ATarget: TWinControl);
    procedure AssignFontTo(const AFontName: String);
  end;

const
  NanumGothic_NAME = '³ª´®°íµñ';

function EmailValidate(const Value: String): Boolean;

implementation

uses
  System.TypInfo, System.RegularExpressions
  ,Winapi.Windows, Winapi.Messages
  ;

const
  RES_FONTS: array[0..2] of String = ('NanumGothic.ttf', 'NanumGothicBold.ttf', 'NanumGothicExtraBold.ttf');
  _NanumGothic_GRID_NAME = '³ª´®°íµñ,10';

var
  NanumGothicExists: Boolean = False;
  InsertedFonts: Boolean = False;

function ExtractExePath: String;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result);
end;

procedure InsertFonts;
var
  LFont: String;
  i: Integer;
begin
  for i := Low(RES_FONTS) to High(RES_FONTS) do
  begin
    LFont := RES_FONTS[i];
    if FileExists(ExtractExePath + LFont) then
      AddFontResource(PChar(ExtractExePath + LFont));
  end;
  InsertedFonts := True;
end;

procedure RemoveFonts;
var
  LFont: String;
begin
  if InsertedFonts then
  begin
    for LFont in RES_FONTS do
      if FileExists(ExtractExePath + LFont) then
        RemoveFontResource(PChar(ExtractExePath + LFont));
    SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
    InsertedFonts := False;
  end;
end;

function EmailValidate(const Value: String): Boolean;
const
  REG_EXP_EMAIL = '[a-z0-9!#$%&''*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&''*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?';
begin
  Result := TRegEx.IsMatch(Value, REG_EXP_EMAIL);
end;

{ TFormHelper }

procedure _AlignTaborders(const Parent: TWinControl) ;
  procedure FixTabOrder(const Parent: TWinControl) ;
  var
    i, j: Integer;
    LFixCtrls: TList<TControl>;
    LChild: TControl;
    LLeftSided, LLower: Boolean;
  begin
    LFixCtrls := TList<TControl>.Create;
    try
      for i := 0 to Parent.ControlCount - 1 do
      begin
        if not (Parent.Controls[i] is TWinControl) then
          Continue;

        j := 0;
        if LFixCtrls.Count > 0 then
        begin
          LChild := Parent.Controls[i];
          for j := 0 to LFixCtrls.Count - 1 do
          begin
            LLower := (LChild.Top < LFixCtrls[j].Top);
            LLeftSided := (LChild.Top = LFixCtrls[j].Top) and (LChild.Left < LFixCtrls[j].Left);
            if LLower or LLeftSided then
              Break;
          end;
        end;
        LFixCtrls.Insert(j, Parent.Controls[i]) ;
        FixTabOrder(TWinControl(Parent.Controls[i])) ;
      end;
      for i := 0 to LFixCtrls.Count - 1 do
        TWinControl(LFixCtrls[i]).TabOrder := i;
    finally
      LFixCtrls.Free;
    end;
  end;
begin
  FixTabOrder(Parent);
end;

procedure _AssignFontTo(ACtrl: TWinControl; const AFontName: String);
  procedure AssignFont(ACtrl: TControl; const AFontName: String);
  var
    LFont: TFont;
  begin
    if IsPublishedProp(ACtrl, 'ParentFont')
      and (GetOrdProp(ACtrl, 'ParentFont') = Ord(False))
      and IsPublishedProp(ACtrl, 'Font') then
    begin
      LFont := TFont(GetObjectProp(ACtrl, 'Font', TFont));
      LFont.Name := AFontName;
    end;
  end;
var
  i: Integer;
begin
  AssignFont(ACtrl, AFontName);
  for i := 0 to ACtrl.ControlCount - 1 do
    if ACtrl.controls[i] is Twincontrol then
      _AssignFontTo(TWincontrol(ACtrl.Controls[i]), AFontName)
    else
      AssignFont(ACtrl.Controls[i], AFontName);
end;

procedure TvHelper.AlignTabOrder;
begin
  _AlignTaborders(Self);
end;

procedure TvHelper.AssignFontTo(const AFontName: String);
begin
  _AssignFontTo(Self, AFontName);
end;

procedure TvHelper.PlaceOn(const ASource: TForm; ATarget: TWinControl);
begin
  ASource.Parent := ATarget;
  ASource.Align := alClient;
  ASource.Show;
end;

initialization
  if not InsertedFonts then
    InsertFonts;

finalization
  if InsertedFonts then
    RemoveFonts;

end.
