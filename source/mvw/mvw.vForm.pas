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
  private
    class var
      FDic: TDictionary<String, TvForm>;
  protected
  public
  end;

  Tv<Tvm: class> = class(TvForm)
  private
    function Getvm: Tvm;
  protected
    function Query<Tv: class>: TvForm;
  public
    constructor Create(AOwner: TComponent); override;

    property vm: Tvm read Getvm;
  end;

  TvModule<Tvm: class> = class(Tv<Tvm>)
  private
    class var
      FPlacedForm: TvForm;
  public
    destructor Destroy; override;

    class procedure PlaceOnTarget(const AOwner: TComponent; ATarget: TWinControl);
  end;

  TvDlg<Tvm: class> = class(Tv<Tvm>)
  private
    FCloseAction: TAction;
    FActionList: TActionList;
    procedure OnCloseActionExecute(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TvDlgModule<Tvm: class> = class(Tv<Tvm>)
  private
  public
  end;

implementation

uses
  mvw.Services, mvw.vForm.Helper,
  System.Actions
  ;

{ TvForm }

{ Tfm<Tvm> }

constructor Tv<Tvm>.Create(AOwner: TComponent);
begin
  inherited;

  FDic.AddOrSetValue(Self.ClassName, Self);

  DoubleBuffered := True;

  Scaled := False;

  TabOrderAlign;
end;

function Tv<Tvm>.Getvm: Tvm;
begin
  Result := vmList.Get<Tvm>;
end;

function Tv<Tvm>.Query<Tv>: TvForm;
begin
  Result := FDic.Items[Tv.ClassName];
end;

{ TvModule<Tvm> }

destructor TvModule<Tvm>.Destroy;
begin
  inherited;

  FPlacedForm := nil;
end;

class procedure TvModule<Tvm>.PlaceOnTarget(const AOwner: TComponent;
  ATarget: TWinControl);
begin
  if not Assigned(FPlacedForm) then
  begin
    FPlacedForm := Create(AOwner);
  end;
  PlaceOn(FPlacedForm, ATarget);
end;

{ TvDlg<Tvm> }

constructor TvDlg<Tvm>.Create(AOwner: TComponent);
begin
  inherited;

  FActionList := TActionList.Create(Self);
  FCloseAction := TACtion.Create(FActionList);
  FCloseAction.ShortCut := TextToShortCut('Esc');
  FCloseAction.OnExecute := OnCloseActionExecute;
  FCloseAction.ActionList := FActionList;
end;

procedure TvDlg<Tvm>.OnCloseActionExecute(Sender: TObject);
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
