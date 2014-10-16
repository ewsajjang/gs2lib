unit mProgressBarHook;

interface

uses
  System.Types, System.SysUtils, System.Math,
  Vcl.Styles, Vcl.ComCtrls, Vcl.Controls, Vcl.ExtCtrls, Vcl.Themes,
  Vcl.Graphics,
  Winapi.CommCtrl, Winapi.Windows;

type
  TProgressBarStyleHookMarquee = class(TProgressBarStyleHook)
  private
    FStep: Integer;
    FTimer: TTimer;
    procedure OnTimer(Sender: TObject);
    function ProgressBarControl: TProgressBar;
  protected
    procedure PaintBar(Canvas: TCanvas); override;
  public
    constructor Create(AControl: TWinControl); override;
    destructor Destroy; override;
  end;

implementation

function TProgressBarStyleHookMarquee.ProgressBarControl: TProgressBar;
begin
  Result := TProgressBar(Control);
end;

constructor TProgressBarStyleHookMarquee.Create(AControl: TWinControl);
begin
  inherited;

  FStep := 0;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := ProgressBarControl.MarqueeInterval;
  FTimer.OnTimer := OnTimer;
  FTimer.Enabled := ProgressBarControl.Style = pbstMarquee;
end;

destructor TProgressBarStyleHookMarquee.Destroy;
begin
  FreeAndNil(FTimer);

  inherited;
end;

procedure TProgressBarStyleHookMarquee.PaintBar(Canvas: TCanvas);
var
  LFillR, LR: TRect;
  LW, LPos: Integer;
  LDetails: TThemedElementDetails;
begin
  if (ProgressBarControl.Style = pbstMarquee) and StyleServices.Available then
  begin
    LR := BarRect;
    InflateRect(LR, -1, -1);
    if Orientation = pbHorizontal then
      LW := LR.Width
    else
      LW := LR.Height;

    LPos := Round(LW * 0.1);
    LFillR := LR;
    if Orientation = pbHorizontal then
    begin
      LFillR.Right := LFillR.Left -1 + LPos;
//      LFillR.Right := System.Math.Max(LFillR.Width div 5, LFillR.Left) + LPos;
      LDetails := StyleServices.GetElementDetails(tpChunk);
    end
    else
    begin
      LFillR.Top := LFillR.Bottom - LPos;
      LDetails := StyleServices.GetElementDetails(tpChunkVert);
    end;

    LFillR.SetLocation(FStep * LFillR.Width, LFillR.Top);
    StyleServices.DrawElement(Canvas.Handle, LDetails, LFillR);
    Inc(FStep, 1);
    if FStep mod 10 = 0 then
      FStep := 0;
  end
  else
    inherited;
end;

procedure TProgressBarStyleHookMarquee.OnTimer(Sender: TObject);
var
  LCanvas: TCanvas;
begin
  if StyleServices.Available and (ProgressBarControl.Style = pbstMarquee) and Control.Visible then
  begin
    FTimer.Enabled := False;
    LCanvas := TCanvas.Create;
    try
      LCanvas.Handle := GetWindowDC(Control.Handle);
      PaintFrame(LCanvas);
      PaintBar(LCanvas);
    finally
      ReleaseDC(Handle, LCanvas.Handle);
      LCanvas.Handle := 0;
      FreeAndNil(LCanvas);
      FTimer.Enabled := True;
    end;
  end
  else
    FTimer.Enabled := False;
end;

initialization
  TStyleManager.Engine.RegisterStyleHook(TProgressBar, TProgressBarStyleHookMarquee);

end.
