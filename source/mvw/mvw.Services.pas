unit mvw.Services;

interface

uses
  mGenericClassList, mMsgRouter,
  System.Classes, System.SysUtils;

var
  msgRouter: TMsgRouter<String> = nil;
  vmList: TGenericClassList = nil;
  svcList: TGenericClassList = nil;

implementation

initialization

  if not Assigned(msgRouter) then
    msgRouter := TMsgRouter<String>.Create;

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

  if Assigned(msgRouter) then
  begin
    msgRouter.Free;
    msgRouter := nil;
  end;

end.
