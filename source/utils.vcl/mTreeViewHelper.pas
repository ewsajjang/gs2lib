unit mTreeViewHelper;

interface

uses
  System.Classes, System.SysUtils, Vcl.ComCtrls
  ;

type
  TTreeViewHelper = class helper for TTreeView
  private
    procedure SetExplorerTheme(const Value: Boolean);
  public
    property ExplorerTheme: Boolean write SetExplorerTheme;
  end;
  TTreeNodeHelper = class helper for TTreeNodes
    function AddChildFmt(Parent: TTreeNode; const S: string; const Args: array of const): TTreeNode;
    function AddChildObjectFmt(Parent: TTreeNode; const S: string; const Args: array of const; Ptr: Pointer): TTreeNode;
  end;

implementation

uses
  Vcl.Themes, Winapi.UxTheme
  ;

{ TTreeNodeHelper }

function TTreeNodeHelper.AddChildFmt(Parent: TTreeNode; const S: string;
  const Args: array of const): TTreeNode;
begin
  Result := AddChild(Parent, Format(S, Args));
end;

function TTreeNodeHelper.AddChildObjectFmt(Parent: TTreeNode; const S: string;
  const Args: array of const; Ptr: Pointer): TTreeNode;
begin
  Result := AddChildObject(Parent, Format(S, Args), Ptr);
end;

{ TTreeViewHelper }

procedure TTreeViewHelper.SetExplorerTheme(const Value: Boolean);
begin
  // fork from - https://stackoverflow.com/a/8408192/1174572
  if ThemeServices.ThemesEnabled and CheckWin32Version(6, 0) then
    if Value then
      SetWindowTheme(Handle, 'Explorer', nil)
    else
      SetWindowTheme(Handle, nil, nil);
end;

end.
