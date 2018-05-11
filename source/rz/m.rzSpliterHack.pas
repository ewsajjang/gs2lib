unit m.rzSpliterHack;

interface

uses
  RzSplit, RzCommon,
  Classes, SysUtils, Windows, UITypes, Graphics
  ;

type
  THackRzSplitter = class(TRzSplitter)
  protected
    procedure DrawSplitterBar; override;
  end;
  TOutlookStyleRzSplitter = THackRzSplitter;

implementation

{ THackRzSplitter }

procedure THackRzSplitter.DrawSplitterBar;
var
  FBarRect, R: TRect;
  W: Integer;
begin
  inherited DrawSplitterBar;;

  R := GetClientRect;
  W := R.Right - R.Left;
  case Orientation of
    orVertical:
      FBarRect := Bounds( R.Left, R.Top + Position, W, SplitterWidth );

    orHorizontal:
      FBarRect := Bounds( R.Left + Position, R.Top, SplitterWidth, R.Height );

  end;

  Canvas.Pen.Style := psClear;
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.Rectangle( FBarRect.Left, FBarRect.Top, FBarRect.Right + 1, FBarRect.Bottom + 1 );

  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := $00e5e5e5;
  Canvas.Pen.Width := 1;
  case Orientation of
    orVertical:
    begin
      Canvas.MoveTo(FBarRect.Left +5, FBarRect.Top + (FBarRect.Height div 2));
      Canvas.LineTo(FBarRect.Left +5 + W -5, FBarRect.Top + (FBarRect.Height div 2));
    end;

    orHorizontal:
    begin
      Canvas.MoveTo(FBarRect.Left + FBarRect.Width div 2, FBarRect.Top );
      Canvas.LineTo(FBarRect.Left + FBarRect.Width div 2, FBarRect.Bottom);
    end;

  end;
end;

end.
