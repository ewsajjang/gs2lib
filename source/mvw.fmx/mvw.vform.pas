unit mvw.vform;

interface

uses
  FMX.Forms, FMX.Types,

  System.Classes, System.SysUtils, System.Rtti,
  System.Generics.Collections
  ;

type
  TEnumCtrlNameProc<T: class> = reference to procedure(const ACtrl: T; const ACtrlName: String);
  TEnumCtrlNameProc = reference to procedure(const ACtrl: TFmxObject; const ACtrlName: String);
  TEnumCtrlProc<T: class> = reference to procedure(const ACtrl: T);
  TEnumCtrlProc = reference to procedure(const ACtrl: TFmxObject);
  TvForm = class(TForm)
  protected
  public
    class function PlaceOn<T: TForm>(const AContainer: TFmxObject; AArgs: TArray<TValue>): T; overload;
    class function PlaceOn<T: TForm>(const AContainer: TFmxObject): T; overload;
    class procedure PlaceOn(const AForm: TForm; const AContainer: TFmxObject); overload;

    procedure PlaceOn(const AContainer: TFmxObject); overload;

    procedure Init; virtual;

    procedure EnumCtrls<T: class>(const AContainer: TFmxObject; const AProc: TEnumCtrlNameProc<T>); overload;
    procedure EnumCtrls(const AContainer: TFmxObject; AProc: TEnumCtrlNameProc); overload;

    procedure EnumCtrls<T: class>(const AContainer: TFmxObject; const AProc: TEnumCtrlProc<T>); overload;
    procedure EnumCtrls(const AContainer: TFmxObject; AProc: TEnumCtrlProc); overload;
  end;

implementation

uses
  mGeneric
  ;

{ TvForm }

procedure TvForm.EnumCtrls(const AContainer: TFmxObject; AProc: TEnumCtrlNameProc);
var
  i: Integer;
begin
  for i := 0 to AContainer.ChildrenCount -1 do
    if AContainer.Children[i].ChildrenCount > 0 then
      EnumCtrls(AContainer.Children[i], AProc)
    else if Assigned(AProc) then
      AProc(AContainer.Children[i], Children[i].Name)
end;

procedure TvForm.EnumCtrls<T>(const AContainer: TFmxObject; const AProc: TEnumCtrlNameProc<T>);
var
  i: Integer;
begin
  EnumCtrls(
    AContainer,
    procedure(const ACtrl: TFmxObject; const ACtrlName: String)
    begin
      if ACtrl is T then
        if Assigned(AProc) then
          AProc(ACtrl as T, ACtrl.Name)
    end);
end;

procedure TvForm.EnumCtrls<T>(const AContainer: TFmxObject; const AProc: TEnumCtrlProc<T>);
var
  i: Integer;
begin
  EnumCtrls(
    AContainer,
    procedure(const ACtrl: TFmxObject)
    begin
      if ACtrl is T then
        if Assigned(AProc) then
          AProc(ACtrl as T)
    end);
end;

procedure TvForm.Init;
begin
end;

class procedure TvForm.PlaceOn(const AForm: TForm; const AContainer: TFmxObject);
begin
  while AForm.ChildrenCount > 0 do
    AForm.Children[0].Parent := AContainer;
  AForm.BorderStyle := TFmxFormBorderStyle.None;
  //AForm.BringToFront;
end;

procedure TvForm.PlaceOn(const AContainer: TFmxObject);
begin
  PlaceOn(Self, AContainer);
end;

class function TvForm.PlaceOn<T>(const AContainer: TFmxObject): T;
begin
  Result := PlaceOn<T>(AContainer, [Application]);
end;

class function TvForm.PlaceOn<T>(const AContainer: TFmxObject; AArgs: TArray<TValue>): T;
var
  LContainApp: Boolean;
  LItem: TValue;
begin
  LContainApp := False;
  for LItem in AArgs do
  begin
    LContainApp := LItem.IsType<TApplication> ;//and Assigned(LItem.AsObject);
    if LContainApp then
      Break;
  end;
  if not LContainApp then
    AArgs := [Application] + AArgs;
  Result := TGeneric.CreateInstance<T>(AArgs);

  PlaceOn(Result, AContainer);
end;

procedure TvForm.EnumCtrls(const AContainer: TFmxObject; AProc: TEnumCtrlProc);
var
  i: Integer;
begin
  for i := 0 to AContainer.ChildrenCount -1 do
    if AContainer.Children[i].ChildrenCount > 0 then
      EnumCtrls(AContainer.Children[i], AProc)
    else if Assigned(AProc) then
      AProc(AContainer.Children[i])
end;

initialization

finalization

end.
