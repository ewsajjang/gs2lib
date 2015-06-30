unit mComportList;

interface

uses
  Classes, SysUtils, Windows, System.Generics.Collections;

type
  EComportList = class(Exception);
  TComPortList = class(TStringList)
  private
  public
    procedure Search;
  end;

implementation

uses
  System.Win.Registry
  ;

{ TComPortList }

procedure TComPortList.Search;
const
  RegPath = 'HARDWARE\DEVICEMAP\SerialComm';
var
  Reg: TRegistry;
  SL: TStringList;
  i: Integer;
begin
  Reg := TRegistry.Create(KEY_READ);
  SL := TStringList.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if not Reg.KeyExists(RegPath) then Exit;
    if not Reg.OpenKey(RegPath, False) then
      raise EComportList.Create('Search failed - Cannot open RegKey ');

    Reg.GetValueNames(SL);
    for i := 0 to SL.Count - 1 do
      Values[Reg.ReadString(SL[i])] := SL[i];
    Reg.CloseKey;
  finally
    FreeAndNil(SL);
    FreeAndNil(Reg);
  end;
end;

end.
