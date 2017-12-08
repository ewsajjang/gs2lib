unit mListBoxHelper;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.StdCtrls, Vcl.Controls, vcl.CheckLst, System.Generics.Collections, Vcl.Forms
  ;

type
  TCustomListBoxHelper = class helper for TCustomListBox
  private type
    TStEx = record
      Chk: TCheckBoxState;
      Enabled: Boolean;
      constructor Create(const AChk: TCheckBoxState; const AEnabled: Boolean);
    end;
  private
    function GetItemText: String;
    procedure OnItemDragOver(Sender, Source: TObject; X, Y: Integer; State:  TDragState; var Accept: Boolean) ;
    procedure OnItemsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SetItemText(const Value: String);
  public
    procedure Click;
    function ItemSelected: Boolean;
    function GroupSelected: Boolean;
    procedure ItemUp;
    procedure ItemDown;
    procedure DragAndDropEnabled(const AMultiSelect: Boolean = True);
    procedure ItemIndexBy(const AValue: String; const AFireOnClick: Boolean = True);
    function SelectedIdxs: TArray<Integer>;
    function SelectedStrs: TArray<String>;
    function SelectedText(const ADelimiter: Char = ','): String;

    property ItemText: String read GetItemText write SetItemText;
  end;

  TListBoxEventHelper = class
    class procedure OnItemsDragDrop(Sender, Source: TObject; X, Y: Integer); static;
  end;

  TCheckListBoxHelper = class helper (TCustomListBoxHelper) for TCheckListBox
  end;

implementation

uses
  mConsts, System.Types, mGeneric;

{ TCustomListBoxHelper }

procedure TCustomListBoxHelper.Click;
begin
  if ItemIndex > -1 then
    Selected[ItemIndex] := True;

  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TCustomListBoxHelper.DragAndDropEnabled(const AMultiSelect: Boolean);
begin
  DragMode := dmAutomatic;
  MultiSelect := AMultiSelect;
  OnDragOver := OnItemDragOver;
  OnDragDrop := OnItemsDragDrop;
end;

function TCustomListBoxHelper.GroupSelected: Boolean;
begin
  Result := ItemSelected and GetItemText.StartsWith('//')
end;

procedure TCustomListBoxHelper.ItemDown;
var
  LCurrent: Integer;
begin
  LCurrent := ItemIndex;
  if ItemIndex > -1 then
    if LCurrent + 1 < Items.Count then
    begin
      Items.BeginUpdate;
      try
        Items.Move(ItemIndex, LCurrent + 1);
        ItemIndex := LCurrent + 1;
      finally
        Items.EndUpdate;
      end;
    end;
end;

procedure TCustomListBoxHelper.ItemIndexBy(const AValue: String; const AFireOnClick: Boolean);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].Equals(AValue) then
    begin
      ItemIndex := i;
      if AFireOnClick then
        Click;
      Break;
    end;
end;

function TCustomListBoxHelper.ItemSelected: Boolean;
begin
  Result := ItemIndex > NNotAssigned;
end;

function TCustomListBoxHelper.GetItemText: String;
begin
  if ItemSelected then
    Result := Items[ItemIndex]
  else
    Result := EmptyStr;
end;

procedure TCustomListBoxHelper.ItemUp;
var
  LCurrent: Integer;
begin
  if ItemIndex > 0 then
  begin
    Items.BeginUpdate;
    try
      LCurrent := ItemIndex;
      Items.Move(ItemIndex, (LCurrent - 1));
      ItemIndex := LCurrent - 1;
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TCustomListBoxHelper.OnItemDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Sender is Self.ClassType;
end;

procedure TCustomListBoxHelper.OnItemsDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  LListBox: TListBox absolute Sender;
  LChkBoxs: TList<TStEx>;

  i, LPos: Integer;
  LItems: TStringList;
  LChkItems: TList<TStEx>;

  LChkSt: TCheckBoxState;
  LEnabled: Boolean;
  LHasItemState: Boolean;
begin
  Assert(Source=Sender);
  LHasItemState := TGeneric.HasIdxProperty(Sender, 'ItemState');
  LPos := LListBox.ItemAtPos(Point(X, Y), False);
  if LPos > NNotAssigned then
  begin
    LChkBoxs := TList<TStEx>.Create;
    LChkItems := TList<TStEx>.Create;
    LItems := TStringList.Create;
    LListBox.Items.BeginUpdate;
    try
      for i := 0 to LListBox.Items.Count -1 do
      begin
        if LHasItemState then
        begin
          TGeneric.TryGetIdxProperty<TCheckBoxState>(Sender, 'ItemState', [i], LChkSt);
          TGeneric.TryGetIdxProperty<Boolean>(Sender, 'ItemEnabled', [i], LEnabled);
          LChkBoxs.Add(TStEx.Create(LChkSt, LEnabled));
        end;
      end;

      for i := LListBox.Items.Count -1 downto 0 do
      begin
        if LListBox.Selected[i] then
        begin
          if LHasItemState then
          begin
            TGeneric.TryGetIdxProperty<TCheckBoxState>(Sender, 'ItemState', [i], LChkSt);
            TGeneric.TryGetIdxProperty<Boolean>(Sender, 'ItemEnabled', [i], LEnabled);
            LChkItems.Add(TStEx.Create(LChkSt, LEnabled));
          end;
          LItems.AddObject(LListBox.Items[i], LListBox.Items.Objects[i]);

          LListBox.Items.Delete(i);
          if LHasItemState then
            LChkBoxs.Delete(i);
          if i < LPos then
            Dec(LPos);
        end;
      end;
      for i := LItems.Count -1 downto 0 do
      begin
        LListBox.Items.InsertObject(LPos, LItems[i], LItems.Objects[i]);
        LListBox.Selected[LPos] := True;
        if LHasItemState then
          LChkBoxs.Insert(LPos, LChkItems[i]);
        Inc(LPos);
      end;
      for i := 0 to LChkBoxs.Count -1 do
      begin
        TGeneric.TrySetIdxProperty<TCheckBoxState>(LListBox, 'ItemState', [i], LChkBoxs[i].Chk);
        TGeneric.TrySetIdxProperty<Boolean>(LListBox, 'ItemEnabled', [i], LChkBoxs[i].Enabled);
      end;
    finally
      LListBox.Items.EndUpdate;
      FreeAndNil(LChkBoxs);
      FreeAndNil(LChkItems);
      FreeAndNil(LItems);
    end;
  end;
end;

function TCustomListBoxHelper.SelectedIdxs: TArray<Integer>;
var
  i: Integer;
begin
  Result := [];
  for i := 0 to Count -1 do
    if Selected[i] then
      Result := Result + [i];
end;

function TCustomListBoxHelper.SelectedStrs: TArray<String>;
var
  i: Integer;
begin
  Result := [];
  for i := 0 to Count -1 do
    if Selected[i] then
      Result := Result + [Items[i]];
end;

function TCustomListBoxHelper.SelectedText(const ADelimiter: Char): String;
var
  LBuf: TStringList;
  i: Integer;
begin
  LBuf := TStringList.Create;
  try
    LBuf.Delimiter := ADelimiter;
    for i := 0 to Count -1 do
      if Selected[i] then
        LBuf.Add(Items[i]);
    Result := LBuf.DelimitedText;
  finally
    FreeAndNil(LBuf);
  end;
end;

procedure TCustomListBoxHelper.SetItemText(const Value: String);
begin
  if ItemSelected then
    Items[ItemIndex] := Value;
end;

{ TListBoxEventHelper }

class procedure TListBoxEventHelper.OnItemsDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  LListBox: TListBox absolute Sender;
  i, LPos: Integer;
  LItems: TStringList;
begin
  Assert(Source=Sender);
  LPos := LListBox.ItemAtPos(Point(X, Y), False);
  if LPos > NNotAssigned then
  begin
    LItems := TStringList.Create;
    LListBox.Items.BeginUpdate;
    try
      for i := LListBox.Items.Count -1 downto 0 do
        if LListBox.Selected[i] then
        begin
          LItems.AddObject(LListBox.Items[i], LListBox.Items.Objects[i]);
          LListBox.Items.Delete(i);
          if i<LPos then
            Dec(LPos);
        end;

      for i := LItems.Count-1 downto 0 do
      begin
        LListBox.Items.InsertObject(LPos, LItems[i], LItems.Objects[i]);
        LListBox.Selected[LPos] := True;
        Inc(LPos);
      end;
    finally
      LListBox.Items.EndUpdate;
      LItems.Free;
    end;
  end;
end;

{ TCustomListBoxHelper.TStEx }

constructor TCustomListBoxHelper.TStEx.Create(const AChk: TCheckBoxState;
  const AEnabled: Boolean);
begin
  Enabled := AEnabled;
  Chk := AChk;
end;

end.
