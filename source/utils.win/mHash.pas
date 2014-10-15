unit mHash;

interface

uses
  System.Classes, System.SysUtils;

type
  TSHA256 = class
    class function Encrypt(const Key, Value: String): String;
    class function Decrypt(const Key, Value: String): String;
  end;

  // see - http://stackoverflow.com/a/3690631
  TMurmurHash2 = class
    class function Value(const S: TCharArray; const Seed: LongWord = $9747b28c): Longword;
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

{ TMurmurHash }

class function TMurmurHash2.Value(const S: TCharArray;
  const Seed: LongWord): Longword;
const
  // 'm' and 'r' are mixing constants generated offline.
  // They're not really 'magic', they just happen to work well.
  m = $5bd1e995;
  r = 24;
var
  h: LongWord;
  len: LongWord;
  k: LongWord;
  i: Integer;
begin
  len := Length(S);

  //The default seed, $9747b28c, is from the original C library
  // Initialize the hash to a 'random' value
  h := seed xor len;
  // Mix 4 bytes at a time into the hash
  i := 0;
  while(len >= 4) do
  begin
    k := PLongWord(@S[i])^;

    k := k*m;
    k := k xor (k shr r);
    k := k* m;

    h := h*m;
    h := h xor k;

    i := i+4;
    len := len-4;
  end;

  {   Handle the last few bytes of the input array
          S: ... $69 $18 $2f
  }
  Assert(len <= 3);
  if len = 3 then
    h := h xor (LongWord(s[i+2]) shl 16);
  if len >= 2 then
    h := h xor (LongWord(s[i+1]) shl 8);
  if len >= 1 then
  begin
    h := h xor (LongWord(s[i]));
    h := h * m;
  end;

  // Do a few final mixes of the hash to ensure the last few
  // bytes are well-incorporated.
  h := h xor (h shr 13);
  h := h * m;
  h := h xor (h shr 15);

  Result := h;
end;

initialization
  CreateSHA256;

finalization
  DestorySHA256;

end.
