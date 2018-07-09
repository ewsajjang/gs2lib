unit mListViewHelper;

interface

uses
  Classes, SysUtils, ComCtrls, CommCtrl, AdvListV
  ;

type
  TListGroupsHelper = class helper for TListGroups
    function FindItemByGroupID(GroupID: Integer): TListGroup;
  end;
  TListViewHelper = class helper for TListView
    procedure SortWithGroups(const ACompareFunc: TFNLVGroupCompare = nil);
  end;

  TAdvListViewHelper = class helper (TListViewHelper) for TAdvListView
  end;


implementation

uses
  Winapi.Messages, Windows
  ;

{ TListGroupsHelper }

function TListGroupsHelper.FindItemByGroupID(GroupID: Integer): TListGroup;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Items[i];
    if Result.GroupID = GroupID then
      Exit;
  end;
  Result := nil;
end;

{ TListViewHelper }

procedure TListViewHelper.SortWithGroups(const ACompareFunc: TFNLVGroupCompare);
  function LVGroupCompare(Group1_ID, Group2_ID: Integer; pvData: Pointer): Integer; stdcall;
  var
    LGroup1, LGroup2: TListGroup;
  begin
    Result := 0;
    LGroup1 := TListGroups(pvData).FindItemByGroupID(Group1_ID);
    LGroup2 := TListGroups(pvData).FindItemByGroupID(Group2_ID);
    if Assigned(LGroup1) and Assigned(LGroup2) then
      Result := CompareText(LGroup1.Header, LGroup2.Header)
  end;
begin
  if not Assigned(ACompareFunc) then
    ListView_SortGroups(Handle, @LVGroupCompare, Groups)
  else
    ListView_SortGroups(Handle, ACompareFunc, Groups)
end;

end.
