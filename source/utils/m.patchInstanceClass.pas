unit m.patchInstanceClass;

interface

procedure PatchInstanceClass(Instance: TObject; NewClass: TClass);

implementation

procedure PatchInstanceClass(Instance: TObject; NewClass: TClass);
// see - https://stackoverflow.com/a/9887684/1174572
type
  PClass = ^TClass;
begin
  if Assigned(Instance) and Assigned(NewClass)
    and NewClass.InheritsFrom(Instance.ClassType)
    and (NewClass.InstanceSize = Instance.InstanceSize) then
  begin
    PClass(Instance)^ := NewClass;
  end;
end;

end.
