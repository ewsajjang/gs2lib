unit mImageHelper;

interface

uses
  Vcl.ExtCtrls, Vcl.Graphics, Vcl.StdCtrls, Vcl.GraphUtil,
  System.Classes, System.SysUtils
  ;

type
  TImageHelper = class helper for TImage
    procedure IfEmptyThenAssignBmp(const w: Integer = -1; const h: Integer = -1);
  end;

implementation

uses
  System.Math
  ;

{ TImageHelper }

procedure TImageHelper.IfEmptyThenAssignBmp(const w, h: Integer);
var
  LBmp: TBitmap;
begin
  if not Assigned(Picture.Bitmap) then
  begin
    LBmp := TBitmap.Create;
    LBmp.Width := IfThen(w = -1, Width, w);
    LBmp.Height := IfThen(h = -1, Height, h);
    Picture.Bitmap := LBmp;
  end;
end;

end.
