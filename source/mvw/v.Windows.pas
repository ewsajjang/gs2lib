unit v.Windows;

interface

uses
  System.Classes, System.SysUtils

  , Vcl.ActnList, vcl.Forms, Vcl.Menus, Vcl.Controls, Vcl.StdCtrls
  , System.Generics.Collections
  ;

type
  TvForm = class(TForm)
  private
    class var FDic: TDictionary<String, TvForm>;
  protected
  public
  end;

  Tv<Tvm: class> = class(TvForm)
  private
    function Getvm: Tvm;
  protected
    function Query<Tv: class>: TvForm;

    procedure OnEditDoubleKey(Sender: TObject; var Key: Char);
    procedure OnEditIntegerKey(Sender: TObject; var Key: Char);
  public
    constructor Create(AOwner: TComponent); override;

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
  mServices, v.Windows.Helper,
  System.Actions
  ;

{ Tfm<Tvm> }

constructor Tv<Tvm>.Create(AOwner: TComponent);
begin
  inherited;

  FDic.AddOrSetValue(Self.ClassName, Self);

  DoubleBuffered := True;

  Scaled := False;

  AlignTabOrder;
end;

function Tv<Tvm>.Getvm: Tvm;
begin
  Result := vmList.Get<Tvm>;
end;

procedure Tv<Tvm>.OnEditDoubleKey(Sender: TObject; var Key: Char);
var
  LText: String;
begin
  LText := (Sender as TEdit).Text;
  if (not CharInSet(Key, [#8, '0'..'9', FormatSettings.DecimalSeparator])) or
     (Key = FormatSettings.DecimalSeparator) and LText.Contains(FormatSettings.DecimalSeparator) then
    Key := #0;
end;

procedure Tv<Tvm>.OnEditIntegerKey(Sender: TObject; var Key: Char);
var
  LText: String;
begin
  LText := (Sender as TEdit).Text;
  if (not CharInSet(Key, [#8, '0'..'9'])) or
     (Key = FormatSettings.DecimalSeparator) and LText.Contains(FormatSettings.DecimalSeparator) then
    Key := #0;
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
