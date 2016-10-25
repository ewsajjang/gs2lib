unit mWinCtrlGraphicHelper;

interface

uses
  WinApi.Windows, Vcl.Graphics, Vcl.Controls, Vcl.ExtCtrls,
  System.Classes, System.SysUtils
  ;

type
  TWinControlHelper = class helper for TWinControl
    procedure SaveToBitmap(out ABitmap: TBitmap);
    procedure LoadFromBitmap(const ABitmap: TBitmap);
  end;

  TPanelHelper = class helper (TWinControlHelper) for TPanel
  end;


implementation

{ TWinControlHelper }

procedure TWinControlHelper.LoadFromBitmap(const ABitmap: TBitmap);
var
  LDc: HDC;
begin
  if not Assigned(ABitmap) then Exit;

  LDC :=GetWindowDC(Handle);
  try
    BitBlt(LDC, 0, 0, ABitmap.Width, ABitmap.Height, ABitmap.Canvas.Handle, 0, 0, SrcCopy);
  finally
    ReleaseDC(Handle, LDc);
  end;
end;

procedure TWinControlHelper.SaveToBitmap(out ABitmap: TBitmap);
var
  LDc: HDC;
begin
  if not Assigned(ABitmap) then Exit;

  LDC :=GetWindowDC(Handle);
  try
    ABitmap.Width  := Width;
    ABitmap.Height := Height;
    BitBlt(ABitmap.Canvas.Handle, 0, 0, Width, Height, LDC, 0, 0, SrcCopy);
  finally
    ReleaseDC(Handle, LDC);
  end;
end;

end.
