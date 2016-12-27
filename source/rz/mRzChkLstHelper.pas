unit mRzChkLstHelper;

interface

uses
  mListBoxHelper, RzChkLst, Spring
  ;

type
  TRzCheckListHelper = class helper (TCustomListBoxHelper) for TRzCheckList
    procedure SaveFile(AFileName: Nullable<String>); overload;
    procedure LoadFile(AFileName: Nullable<String>); overload;
  end;

implementation

uses
  System.SysUtils
  ;

{ TRzCheckListHelper }

procedure TRzCheckListHelper.LoadFile(AFileName: Nullable<String>);
begin
  Assert(AFileName.HasValue, 'FileName is not assigned');
  if FileExists(AFileName) then
    LoadFromFile(AFileName);
end;

procedure TRzCheckListHelper.SaveFile(AFileName: Nullable<String>);
begin
  Assert(AFileName.HasValue, 'FileName is not assigned');
  SaveToFile(AFileName);
end;

end.
