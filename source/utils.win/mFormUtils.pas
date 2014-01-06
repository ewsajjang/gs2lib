unit mFormUtils;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.Graphics;

procedure TaborderAlign(const Parent: TWinControl) ;
procedure FontAssign(ACtrl: TWinControl; const AFontName: String);

implementation

uses
  System.TypInfo;

procedure TaborderAlign(const Parent: TWinControl) ;
  procedure TabOrderFix(const Parent: TWinControl) ;
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
        TabOrderFix(TWinControl(Parent.Controls[i])) ;
      end;
      for i := 0 to LFixCtrls.Count - 1 do
        TWinControl(LFixCtrls[i]).TabOrder := i;
    finally
      LFixCtrls.Free;
    end;
  end;
begin
  TabOrderFix(Parent);
end;

procedure FontAssign(ACtrl: TWinControl; const AFontName: String);
  procedure _FontAssign(ACtrl: TControl; const AFontName: String);
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
  _FontAssign(ACtrl, AFontName);
  for i := 0 to ACtrl.ControlCount - 1 do
    if ACtrl.controls[i] is Twincontrol then
      FontAssign(TWincontrol(ACtrl.Controls[i]), AFontName)
    else
      _FontAssign(ACtrl.Controls[i], AFontName);
end;

end.
