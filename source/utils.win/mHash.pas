unit mHash;

interface

uses
  System.Classes, System.SysUtils;

type
  TSHA256 = class
    class function Encrypt(const Key, Value: String): String;
    class function Decrypt(const Key, Value: String): String;
  end;

implementation

uses
  // see - http://www.cityinthesky.co.uk/opensource/dcpcrypt/
  DCPcrypt2, DCPsha256, DCPblockciphers, DCPrijndael;

{ TSHA256 }
var
  DCP_rijndael: TDCP_rijndael;
  DCP_sha256: TDCP_sha256;

procedure CreateSHA256;
begin
  DCP_rijndael := TDCP_rijndael.Create(nil);

  DCP_sha256 := TDCP_sha256.Create(nil);
  DCP_sha256.Algorithm := 'SHA256';
  DCP_sha256.HashSize := 256;
  DCP_sha256.Id := 28;
end;

procedure DestorySHA256;
begin
  FreeAndNil(DCP_sha256);
  FreeAndNil(DCP_rijndael);
end;

class function TSHA256.Decrypt(const Key, Value: String): String;
begin
  DCP_rijndael.InitStr(Key, TDCP_sha256);
  Result := String(DCP_rijndael.DecryptString(Value));
end;

class function TSHA256.Encrypt(const Key, Value: String): String;
begin
  DCP_rijndael.InitStr(Key, TDCP_sha256);
  Result := String(DCP_rijndael.EncryptString(Value));
end;

initialization
  CreateSHA256;

finalization
  DestorySHA256;

end.
