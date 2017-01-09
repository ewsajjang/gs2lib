unit mvw.vform;

interface

uses
  FMX.Forms, FMX.Types,

  System.Classes, System.SysUtils,
  System.Generics.Collections
  ;

type
  TFmxObjectEnumProc<T: class> = reference to procedure(ACtrl: T; ACtrlName: String);
  TFmxObjectEnumProc = reference to procedure(ACtrl: TFmxObject; ACtrlName: String);
  TvForm = class(TForm)
  private
  protected
    FPlaceOnProc: TProc;
  public
    class function PlaceOn<T: TForm>(AContainer: TFmxObject): T; overload;
    procedure PlaceOn(const AForm: TvForm; AContainer: TFmxObject); overload;
    procedure FmxObjectEnum<T: class>(AProc: TFmxObjectEnumProc<T>; const AContainer: TFmxObject = nil); overload;
    procedure FmxObjectEnum(AProc: TFmxObjectEnumProc; const AContainer: TFmxObject = nil); overload;
  end;

implementation

{ TvForm }

procedure TvForm.FmxObjectEnum(AProc: TFmxObjectEnumProc;
  const AContainer: TFmxObject);
var
  i: Integer;
begin
  for i := 0 to ChildrenCount -1 do
    if Children[i].ChildrenCount > 0 then
      FmxObjectEnum(AProc, Children[i])
    else if Assigned(AProc) then
      AProc(Children[i], Children[i].Name)
end;

procedure TvForm.FmxObjectEnum<T>(AProc: TFmxObjectEnumProc<T>;
  const AContainer: TFmxObject);
var
  i: Integer;
begin
  FmxObjectEnum(
    procedure(ACtrl: TFmxObject; ACtrlName: String)
    begin
      if ACtrl is T then
        if Assigned(AProc) then
          AProc(ACtrl as T, ACtrl.Name)
    end,
    AContainer);
end;

procedure TvForm.PlaceOn(const AForm: TvForm; AContainer: TFmxObject);
begin
  if AForm.ChildrenCount > 0 then
  begin
    AForm.Children[0].Parent := AContainer;
    AForm.BorderStyle := TFmxFormBorderStyle.None;
    AForm.BringToFront;
    if Assigned(AForm.FPlaceOnProc) then
      FPlaceOnProc;
  end;
end;

class function TvForm.PlaceOn<T>(AContainer: TFmxObject): T;
begin
  Application.CreateForm(T, Result);
  Result.BorderStyle := TFmxFormBorderStyle.None;
  while Result.ChildrenCount>0 do
    Result.Children[0].Parent := AContainer;
end;

end.
