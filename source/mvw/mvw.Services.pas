unit mvw.Services;

interface

uses
  mGenericClassList, mRouter,
  System.Classes, System.SysUtils;

var
  router: TRouter<String> = nil;
  vmList: TGenericClassList = nil;
  svcList: TGenericClassList = nil;

implementation

initialization

  if not Assigned(router) then
    router := TRouter<String>.Create;

  if not Assigned(vmList) then
    vmList := TGenericClassList.Create;

  if not Assigned(svcList) then
    svcList := TGenericClassList.Create;

finalization
  if Assigned(svcList) then
  begin
    svcList.Free;
    svcList := nil;
  end;

  if Assigned(vmList) then
  begin
    vmList.Free;
    vmList := nil;
  end;

  if Assigned(router) then
  begin
    router.Free;
    router := nil;
  end;

end.
