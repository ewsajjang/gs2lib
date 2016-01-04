unit mTreeViewHelper;

interface

uses
  System.Classes, System.SysUtils, Vcl.ComCtrls
  ;

type
  TTreeNodeHelper = class helper for TTreeNodes
    function AddChildFmt(Parent: TTreeNode; const S: string; const Args: array of const): TTreeNode;
    function AddChildObjectFmt(Parent: TTreeNode; const S: string; const Args: array of const; Ptr: Pointer): TTreeNode;
  end;

implementation

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

end.
