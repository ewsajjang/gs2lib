unit mComboBoxHelper;

interface

uses
  System.SysUtils,
  Vcl.StdCtrls;

type
  TComboBoxHelper = class helper for TComboBox
  private
    function GetSelectedStr: String;
    procedure SetSelectedStr(const Value: String);
  public
    procedure IndexBy(const AValue: String);
    procedure ContainsBy(const AValue: String);
    function ItemSelected: Boolean;
    function ItemCount: Integer;
    function SelectedObj<T: class>: T;
    property SelectedStr: String read GetSelectedStr write SetSelectedStr;
    procedure DropdownListAutoWidth;
    procedure ClearItemsObjects;
  end;

implementation

uses
  Winapi.Windows, Winapi.Messages;

{ TComboBoxHelper }

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

function TComboBoxHelper.GetSelectedStr: String;
begin
  Result := Text;
end;

procedure TComboBoxHelper.IndexBy(const AValue: String);
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
      Break;
    end;
  end;

  if not LSuccess then
    ItemIndex := -1;
end;

function TComboBoxHelper.ItemCount: Integer;
begin
  Result := Items.Count;
end;

function TComboBoxHelper.ItemSelected: Boolean;
begin
  Result := ItemIndex > -1;
end;

procedure TComboBoxHelper.SetSelectedStr(const Value: String);
begin
  if ItemSelected then
    Items[ItemIndex] := Value;
end;

function TComboBoxHelper.SelectedObj<T>: T;
begin
  if ItemSelected then
    Result := Items.Objects[ItemIndex] as T
  else
    Result := nil;
end;

end.
