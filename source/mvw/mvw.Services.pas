unit mvw.Services;

interface

uses
  mGenericClassList, mRouter,
  System.Classes, System.SysUtils;

var
  EventRouter: TRouter<String> = nil;
  vmList: TGenericClassList = nil;
  svcList: TGenericClassList = nil;

implementation

initialization

  if not Assigned(EventRouter) then
    EventRouter := TRouter<String>.Create;

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

  if Assigned(EventRouter) then
  begin
    EventRouter.Free;
    EventRouter := nil;
  end;

end.
