unit mvw.vForm;

interface

uses
  System.Classes, System.SysUtils,

  Vcl.ActnList, vcl.Forms, Vcl.Menus, Vcl.Controls, WinAPI.Windows, WinApi.Messages,
  System.Generics.Collections
  ;

type
  TvFormClass = class of TvForm;
  TvForm = class(TForm)
  private class var
    FDic: TDictionary<String, TvForm>;
  private
    FLockWindowsUpdate: Boolean;
    FEnableDropdownFiles: Boolean;
    FDropDownFileExt: String;
    function GetVClasses(AClass: TvFormClass): TvForm;
    function GetVNames(Name: String): TvForm;
    function GetVCnt: Integer;
    procedure SetEnableDragNDrop(const Value: Boolean);
    procedure WmDropFiles(var msg: TWMDropFiles); message WM_DROPFILES;
  protected
    FOnPlaceOn: TProc;
    FOnPlaceOnParent: TProc;
    procedure OnPlaceOnParentNotify; virtual;

    function FindTargetWinControl(const AName: String): TWinControl;

    procedure DoCreate; override;
    procedure DoDropFile(const ACnt: Integer; const AFiles: TStringList); virtual;
  public
    class procedure PlaceOn(const AChild: TvForm; ATarget: TWinControl); overload;
    class function PlaceOn(const AChildClass: TFormClass; ATarget: TWinControl; AOwner: TComponent = nil): TForm; overload;
    class function PlaceOn<T: TForm>(ATarget: TWinControl; AOwner: TComponent = nil): T; overload;

    constructor Create(AOwner: TComponent); override;

    function IsShortCut(var Message: TWMKey): Boolean; override;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure PlaceOnParent(const ATarget: TWinControl = nil); overload;
    procedure PlaceOnParent(const AParent: TvForm); overload;
    procedure PlaceOnParent(const AParent: TvFormClass); overload;
    procedure PlaceOnParent(const AParent: String); overload;
    procedure EnumComponents<T: class>(AProc: TProc<T, String>); overload;
    procedure EnumComponents<T: class>(AProc: TProc<T>); overload;
    procedure EnumControls<T: class>(const AContainer: TWinControl; AProc: TProc<T, String>); overload;
    procedure EnumControls<T: class>(const AContainer: TWinControl; AProc: TProc<T>); overload;
    function Controls<T: class>(const AContainer: TWinControl): TArray<T>;
    procedure EnumControls(const AContainer: TWinControl; AProc: TProc<TControl, String>); overload;

    function ExistsForms(const AvFormName: String): Boolean; overload;
    function ExistsForms(const AvFormClass: TvFormClass): Boolean; overload;

    property EnableDropdownFiles: Boolean read FEnableDropdownFiles write SetEnableDragNDrop;
    property DropDownFileExt: String read FDropDownFileExt write FDropDownFileExt;
    property vCnt: Integer read GetVCnt;
    property vNames[Name: String]: TvForm read GetVNames;
    property vClasses[AClass: TvFormClass]: TvForm read GetVClasses;
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
  System.Actions, System.Generics.Defaults, Winapi.ShellAPI, System.IOUtils
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

procedure TvForm.EnumComponents<T>(AProc: TProc<T, String>);
var
	i: Integer;
begin
	for i := 0 to ComponentCount -1 do
  	if Components[i] is T then
    	if Assigned(AProc) then
      	AProc(Components[i] as T, Components[i].Name);
end;

procedure TvForm.EndUpdate;
begin
  if FLockWindowsUpdate then
    LockWindowUpdate(0);
end;

procedure TvForm.EnumComponents<T>(AProc: TProc<T>);
var
	i: Integer;
begin
	for i := 0 to ComponentCount -1 do
  	if Components[i] is T then
    	if Assigned(AProc) then
      	AProc(Components[i] as T);
end;

procedure TvForm.EnumControls(const AContainer: TWinControl;
  AProc: TProc<TControl, String>);
var
	i: Integer;
begin
	for i := 0 to AContainer.ControlCount -1 do
  begin
    if Assigned(AProc) then
      AProc(AContainer.Controls[i], AContainer.Controls[i].Name);
    if AContainer.Controls[i] is TWinControl then
      EnumControls(TWinControl(AContainer.Controls[i]), AProc);
  end;
end;

procedure TvForm.EnumControls<T>(const AContainer: TWinControl;
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
    	EnumControls<T>(TWinControl(AContainer.Controls[i]), AProc);
end;

procedure TvForm.EnumControls<T>(const AContainer: TWinControl;
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
    	EnumControls<T>(TWinControl(AContainer.Controls[i]), AProc);
end;

procedure TvForm.BeginUpdate;
begin
  if CanFocus then
    FLockWindowsUpdate := LockWindowUpdate(Handle);
end;

function TvForm.Controls<T>(const AContainer: TWinControl): TArray<T>;
var
	i: Integer;
begin
	for i := 0 to AContainer.ControlCount -1 do
  	if AContainer.Controls[i] is T then
    	Result := Result + [AContainer.Controls[i] as T]
    else if AContainer.Controls[i] is TWinControl then
    	Result := Controls<T>(AContainer.Controls[i] as TWinControl);
end;

constructor TvForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEnableDropdownFiles := False;
end;

procedure TvForm.DoCreate;
begin
  DoubleBuffered := True;
  Scaled := False;
  TabOrderAlign;
  FDropDownFileExt := '';

  FDic.AddOrSetValue(Self.ClassName, Self);

  inherited DoCreate;
end;

procedure TvForm.DoDropFile(const ACnt: Integer;
  const AFiles: TStringList);
begin
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

function TvForm.GetVClasses(AClass: TvFormClass): TvForm;
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

function TvForm.GetVCnt: Integer;
begin
  Result := FDic.Count;
end;

function TvForm.GetVNames(Name: String): TvForm;
begin
  Result := FDic.Items[Name];
end;

function TvForm.IsShortCut(var Message: TWMKey): Boolean;
var
  LForm: TvForm;
begin
  Result := inherited;
  if not Result then
    for LForm in Controls<TvForm>(Self) do
    begin
      Result := LForm.Visible and LForm.IsShortCut(Message);
      if Result then
        Break;
    end;
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

class function TvForm.PlaceOn(const AChildClass: TFormClass;
  ATarget: TWinControl; AOwner: TComponent): TForm;
begin
  Result := AChildClass.Create(AOwner);
  Result.Parent := ATarget;
  if Assigned(Result.Parent) then
    Result.Align := alClient;
  Result.Show;
  Result.BringToFront;
  if Result is TvForm then
    if Assigned((Result as TvForm).FOnPlaceOn) then
      (Result as TvForm).FOnPlaceOn();
end;

class function TvForm.PlaceOn<T>(ATarget: TWinControl; AOwner: TComponent): T;
begin
  Result := PlaceOn(T, ATarget, AOwner) as T;
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

procedure TvForm.SetEnableDragNDrop(const Value: Boolean);
begin
  if FEnableDropdownFiles <> Value then
  begin
    FEnableDropdownFiles := Value;
    DragAcceptFiles(Handle, FEnableDropdownFiles);
  end;
end;

procedure TvForm.WmDropFiles(var msg: TWMDropFiles);
const
  NMaxFileLen = 255;
var
  i, LFileCnt: Integer;
  LFileName: array [0 .. NMaxFileLen] of Char;
  LBuf: TStringList;
begin
  LFileCnt := DragQueryFile(msg.Drop, $FFFFFFFF, LFileName, NMaxFileLen);
  LBuf := TStringList.Create;
  try
    LBuf.Sorted := True;
    for i := 0 to LFileCnt - 1 do
    begin
      DragQueryFile(msg.Drop, i, LFileName, NMaxFileLen);
      if FDropDownFileExt.IsEmpty then
        LBuf.Add(LFileName)
      else if TPath.GetExtension(LFileName).Equals(FDropDownFileExt) then
      begin
        LBuf.Add(LFileName)
      end;
    end;
    //LBuf.Sort;
    DoDropFile(LBuf.Count, LBuf);
  finally
    FreeAndNil(LBuf);
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

{class function TvForm.PlaceOn<T>(const AChildClass: TFormClass;
  ATarget: TWinControl; AOwner: TComponent): T;
begin

end;

 TvDlg }

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
