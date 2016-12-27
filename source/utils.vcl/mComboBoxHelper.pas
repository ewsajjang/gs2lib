unit mComboBoxHelper;

interface

uses
  System.SysUtils,
  Vcl.StdCtrls;

type
  TComboBoxHelper = class helper for TComboBox
  private
    function GetItemText: String;
    procedure SetItemText(const Value: String);

  public
    procedure AddItems(const AValues: TArray<String>);
    procedure AddFmt(const AStr: String; const Arg: array of const);
    procedure IndexBy(const AValue: String; const AFireOnChange: Boolean = True); overload;
    procedure IndexBy(const AIndex: Integer; const AFireOnChange: Boolean = True); overload;
    procedure IndexByContainStr(const AValue: String);
    function IndexOfContainStr(const AValue: String): Integer;
    procedure ContainsBy(const AValue: String);
    function ItemSelected: Boolean;
    function ItemCount: Integer;
    function ItemObj<T: class>: T;
    procedure DropdownListAutoWidth;
    procedure ClearItemsObjects;
    procedure SetDropdownCount(const AMin: Integer = 8);

    property ItemText: String read GetItemText write SetItemText;
  end;

implementation

uses
	mconsts,
  Winapi.Windows, Winapi.Messages, System.Math;

{ TComboBoxHelper }

procedure TComboBoxHelper.AddFmt(const AStr: String; const Arg: array of const);
begin
  Items.Add(Format(AStr, Arg));
end;

procedure TComboBoxHelper.AddItems(const AValues: TArray<String>);
var
  LValue: String;
begin
  Items.BeginUpdate;
  try
    for LValue in AValues do
      Items.Add(LValue);
  finally
    Items.EndUpdate;
  end;
end;

procedure TComboBoxHelper.ClearItemsObjects;
var
  i: Integer;
begin
  for i := 0 to ItemCount - 1 do
    Items.Objects[i].Free;
  Clear;
end;

procedure TComboBoxHelper.ContainsBy(const AValue: String);
var
  LSuccess: Boolean;
  i: Integer;
begin
  LSuccess := False;
  if not AValue.IsEmpty then
    for i := 0 to Items.Count - 1 do
    begin
      LSuccess := Items[i].Contains(AValue);
      if LSuccess then
      begin
        ItemIndex := i;
        Break;
      end;
    end;

  if not LSuccess then
    ItemIndex := -1;
end;

procedure TComboBoxHelper.SetDropdownCount(const AMin: Integer);
begin
  DropDownCount := Min(Items.Count, AMin);
end;

procedure TComboBoxHelper.DropdownListAutoWidth;
const
  HORIZONTAL_PADDING = 4;
var
  LMaxWidth: Integer;
  i: integer;
  LItemWidth: Integer;
begin
  LMaxWidth := 0;
  Canvas.Font.Assign(Font);
  for i := 0 to Items.Count-1 do
  begin
    LItemWidth := Canvas.TextWidth(Items[i]);
    Inc(LItemWidth, HORIZONTAL_PADDING * 2);
    if (LItemWidth > LMaxWidth) then
      LMaxWidth := LItemWidth;
  end;
  if (LMaxWidth > Width) then
  begin
    if DropDownCount < Items.Count then
      LMaxWidth := LMaxWidth + GetSystemMetrics(SM_CXVSCROLL);
    SendMessage(Handle, CB_SETDROPPEDWIDTH, LMaxWidth, 0);
  end;
end;

function TComboBoxHelper.GetItemText: String;
begin
  Result := Text;
end;

procedure TComboBoxHelper.IndexBy(const AValue: String; const AFireOnChange: Boolean);
var
  LSuccess: Boolean;
  i: Integer;
begin
  LSuccess := False;
  for i := 0 to Items.Count - 1 do
  begin
    LSuccess := Items[i].Equals(AValue);
    if LSuccess then
    begin
      ItemIndex := i;
      if AFireOnChange and Assigned(OnChange) then
        OnChange(Self);
      Break;
    end;
  end;

  if not LSuccess then
    ItemIndex := -1;
end;

procedure TComboBoxHelper.IndexBy(const AIndex: Integer;
  const AFireOnChange: Boolean);
begin
  ItemIndex := AIndex;
  if InRange(AIndex, 0, Items.Count -1) and (ItemIndex > -1) then
    if AFireOnChange and Assigned(OnChange) then
      OnChange(Self);
end;

procedure TComboBoxHelper.IndexByContainStr(const AValue: String);
begin
	ItemIndex := IndexOfContainStr(AValue);
end;

function TComboBoxHelper.IndexOfContainStr(const AValue: String): Integer;
var
  i: Integer;
begin
	Result := NNotAssigned;
  for i := 0 to ItemCount -1 do
    if Items[i].Contains(AValue) then
    begin
      Result := i;
      Break;
    end;
end;

function TComboBoxHelper.ItemCount: Integer;
begin
  Result := Items.Count;
end;

function TComboBoxHelper.ItemSelected: Boolean;
begin
  Result := ItemIndex > -1;
end;

procedure TComboBoxHelper.SetItemText(const Value: String);
begin
  if ItemSelected then
    Items[ItemIndex] := Value;
end;

function TComboBoxHelper.ItemObj<T>: T;
begin
  if ItemSelected then
    Result := Items.Objects[ItemIndex] as T
  else
    Result := nil;
end;

end.
