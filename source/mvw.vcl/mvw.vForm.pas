unit mvw.vForm;

interface

uses
  System.Classes, System.SysUtils,

  Vcl.ActnList, vcl.Forms, Vcl.Menus, Vcl.Controls,
  System.Generics.Collections
  ;

type
  TvFormClass = class of TvForm;
  TvForm = class(TForm)
  private class var
    FDic: TDictionary<String, TvForm>;
  private
    function GetFormsFromClass(AClass: TvFormClass): TvForm;
    function GetFormsFormName(Name: String): TvForm;
    function GetFormsCnt: Integer;
  protected
    FOnPlaceOn: TProc;
    FOnPlaceOnParent: TProc;
    procedure OnPlaceOnParentNotify; virtual;

    function FindTargetWinControl(const AName: String): TWinControl;
  public
    class procedure PlaceOn(const AChild: TvForm; ATarget: TWinControl);

    constructor Create(AOwner: TComponent); override;

    procedure PlaceOnParent(const ATarget: TWinControl = nil); overload;
    procedure PlaceOnParent(const AParent: TvForm); overload;
    procedure PlaceOnParent(const AParent: TvFormClass); overload;
    procedure PlaceOnParent(const AParent: String); overload;
    procedure ComponentsEnum<T: class>(AProc: TProc<T, String>); overload;
    procedure ComponentsEnum<T: class>(AProc: TProc<T>); overload;
    procedure ControlsEnum<T: class>(const AContainer: TWinControl; AProc: TProc<T, String>); overload;
    procedure ControlsEnum<T: class>(const AContainer: TWinControl; AProc: TProc<T>); overload;
    procedure ControlsEnum(const AContainer: TWinControl; AProc: TProc<TControl, String>); overload;

    function ExistsForms(const AvFormName: String): Boolean; overload;
    function ExistsForms(const AvFormClass: TvFormClass): Boolean; overload;

    property vCnt: Integer read GetFormsCnt;
    property vNames[Name: String]: TvForm read GetFormsFormName;
    property vClasses[AClass: TvFormClass]: TvForm read GetFormsFromClass;
  end;

  TvDlg = class(TvForm)
  private
    FCloseAction: TAction;
    FActionList: TActionList;
    procedure OnCloseActionExecute(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  Tv<Tvm: class> = class(TvForm)
  private
    function Getvm: Tvm;
  protected
    function Query<Tv: class>: TvForm;
  public
    property vm: Tvm read Getvm;
  end;

  TvDlg<Tvm: class> = class(Tv<Tvm>)
  private
    FCloseAction: TAction;
    FActionList: TActionList;
    procedure OnCloseActionExecute(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  mvw.Services, mvw.vForm.Helper,
  System.Actions, System.Generics.Defaults
  ;

{ Tv<Tvm> }

function Tv<Tvm>.Getvm: Tvm;
begin
  Result := vmList.Get<Tvm>;
end;

function Tv<Tvm>.Query<Tv>: TvForm;
begin
  Result := FDic.Items[Tv.ClassName];
end;

{ TvDlg<Tvm> }

constructor TvDlg<Tvm>.Create(AOwner: TComponent);
begin
  inherited;

  FActionList := TActionList.Create(Self);
  FActionList.Name := '_ActionList_';
  FCloseAction := TACtion.Create(FActionList);
  FCloseAction.ShortCut := TextToShortCut('Esc');
  FCloseAction.OnExecute := OnCloseActionExecute;
  FCloseAction.ActionList := FActionList;
end;

procedure TvDlg<Tvm>.OnCloseActionExecute(Sender: TObject);
begin
  Close;
end;

{ TvForm }

function TvForm.ExistsForms(const AvFormName: String): Boolean;
begin
  Result := FDic.ContainsKey(AvFormName);
end;

procedure TvForm.ComponentsEnum<T>(AProc: TProc<T, String>);
var
	i: Integer;
begin
	for i := 0 to ComponentCount -1 do
  	if Components[i] is T then
    	if Assigned(AProc) then
      	AProc(Components[i] as T, Components[i].Name);
end;

procedure TvForm.ComponentsEnum<T>(AProc: TProc<T>);
var
	i: Integer;
begin
	for i := 0 to ComponentCount -1 do
  	if Components[i] is T then
    	if Assigned(AProc) then
      	AProc(Components[i] as T);
end;

procedure TvForm.ControlsEnum(const AContainer: TWinControl;
  AProc: TProc<TControl, String>);
var
	i: Integer;
begin
	for i := 0 to AContainer.ControlCount -1 do
  begin
    if Assigned(AProc) then
      AProc(AContainer.Controls[i], AContainer.Controls[i].Name);
    if AContainer.Controls[i] is TWinControl then
      ControlsEnum(TWinControl(AContainer.Controls[i]), AProc);
  end;
end;

procedure TvForm.ControlsEnum<T>(const AContainer: TWinControl;
  AProc: TProc<T>);
var
	i: Integer;
begin
	for i := 0 to AContainer.ControlCount -1 do
  	if AContainer.Controls[i] is T then
    begin
    	if Assigned(AProc) then
      	AProc(AContainer.Controls[i] as T);
    end
    else if AContainer.Controls[i] is TWinControl then
    	ControlsEnum<T>(TWinControl(AContainer.Controls[i]), AProc);
end;

procedure TvForm.ControlsEnum<T>(const AContainer: TWinControl;
	AProc: TProc<T, String>);
var
	i: Integer;
begin
	for i := 0 to AContainer.ControlCount -1 do
  	if AContainer.Controls[i] is T then
    begin
    	if Assigned(AProc) then
      	AProc(AContainer.Controls[i] as T, AContainer.Controls[i].Name);
    end
    else if AContainer.Controls[i] is TWinControl then
    	ControlsEnum<T>(TWinControl(AContainer.Controls[i]), AProc);
end;

constructor TvForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  DoubleBuffered := True;
  Scaled := False;
  TabOrderAlign;

  FDic.AddOrSetValue(Self.ClassName, Self);
end;

function TvForm.ExistsForms(const AvFormClass: TvFormClass): Boolean;
begin
  Result := ExistsForms(AvFormClass.ClassName);
end;

function TvForm.FindTargetWinControl(const AName: String): TWinControl;
var
  i: Integer;
  LComponentName: String;
begin
  Result := nil;
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TWinControl then
    begin
      LComponentName := Components[i].Name;
      if LComponentName.EndsWith(AName) then
      begin
        Result := TWinControl(Components[i]);
        Break;
      end;
    end;
end;

function TvForm.GetFormsFromClass(AClass: TvFormClass): TvForm;
begin
  Result := vNames[AClass.ClassName];
end;

class procedure TvForm.PlaceOn(const AChild: TvForm; ATarget: TWinControl);
begin
  AChild.Parent := ATarget;
  AChild.Align := alClient;
  AChild.Show;
  AChild.BringToFront;
  if Assigned(AChild.FOnPlaceOn) then
    AChild.FOnPlaceOn();
end;

function TvForm.GetFormsCnt: Integer;
begin
  Result := FDic.Count;
end;

function TvForm.GetFormsFormName(Name: String): TvForm;
begin
  Result := FDic.Items[Name];
end;

procedure TvForm.PlaceOnParent(const AParent: TvFormClass);
begin
  PlaceOnParent(vClasses[AParent]);
end;

procedure TvForm.OnPlaceOnParentNotify;
begin
  if Assigned(FOnPlaceOnParent) then
    FOnPlaceOnParent;
end;

procedure TvForm.PlaceOnParent(const AParent: String);
var
  LParent: TvForm;
begin
  LParent := vNames[AParent];
  if Assigned(LParent) then
  begin
    PlaceOn(Self, FindTargetWinControl(LParent.Name));
    OnPlaceOnParentNotify;
  end;
end;

procedure TvForm.PlaceOnParent(const ATarget: TWinControl);
begin
  PlaceOn(Self, ATarget);
  OnPlaceOnParentNotify;
end;

procedure TvForm.PlaceOnParent(const AParent: TvForm);
begin
  PlaceOn(Self, FindTargetWinControl(AParent.Name));
  OnPlaceOnParentNotify;
end;

{ TvDlg }

constructor TvDlg.Create(AOwner: TComponent);
begin
  inherited;

  FActionList := TActionList.Create(Self);
  FActionList.Name := '_ActionList_';
  FCloseAction := TACtion.Create(FActionList);
  FCloseAction.ShortCut := TextToShortCut('Esc');
  FCloseAction.OnExecute := OnCloseActionExecute;
  FCloseAction.ActionList := FActionList;
end;

procedure TvDlg.OnCloseActionExecute(Sender: TObject);
begin
  Close;
end;

initialization
  if not Assigned(TvForm.FDic) then
    TvForm.FDic := TDictionary<String, TvForm>.Create;

finalization
  if Assigned(TvForm.FDic) then
    FreeAndNil(TvForm.FDic);

end.
