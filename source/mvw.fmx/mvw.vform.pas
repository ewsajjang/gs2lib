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
  private class var
    FDic: TDictionary<String, TvForm>;
  private
    function GetVCnt: Integer;
    function GetVName(AClassName: String): TvForm;
  protected
    FPlaceOnProc: TProc;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PlaceOn(const AForm: TvForm; AContainer: TFmxObject);
    procedure FmxObjectEnum<T: class>(AProc: TFmxObjectEnumProc<T>; const AContainer: TFmxObject = nil); overload;
    procedure FmxObjectEnum(AProc: TFmxObjectEnumProc; const AContainer: TFmxObject = nil); overload;
    function vExists(const AClassName: String): Boolean;


    property vCnt: Integer read GetVCnt;
    property vNames[AClassName: String]: TvForm read GetVName;
  end;

  Tv<Tvm: class> = class(TvForm)
  private
    function Getvm: Tvm;
  protected
    function Query<Tv: class>: TvForm;
  public
    property vm: Tvm read Getvm;
  end;

implementation

uses
  mvw.Services
  ;

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

constructor TvForm.Create(AOwner: TComponent);
begin
  inherited;

  if not Assigned(FDic) then
    FDic := TDictionary<String, TvForm>.Create;
  FDic.Add(ClassName, Self);
end;

destructor TvForm.Destroy;
begin
  FDic.Remove(ClassName);
  if FDic.Count = 0 then
    FreeAndNil(FDic);

  inherited;
end;

function TvForm.GetVCnt: Integer;
begin
  Result := FDic.Count;
end;

function TvForm.GetVName(AClassName: String): TvForm;
begin
  if not FDic.TryGetValue(AClassName, Result) then
    Result := nil;
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

function TvForm.vExists(const AClassName: String): Boolean;
begin
  Result := FDic.ContainsKey(AClassName)
end;

{ Tv<Tvm> }

function Tv<Tvm>.Getvm: Tvm;
begin
  Result := vmList.Get<Tvm>;
end;

function Tv<Tvm>.Query<Tv>: TvForm;
begin
  Result := FDic.Items[Tv.ClassName];
end;

end.
