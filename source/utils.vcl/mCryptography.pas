unit mCryptography;

interface

uses
  jwaWinCrypt, jwaWinBase, jwaWinType,
  Classes, SysUtils
  ;

type
  // See: https://stackoverflow.com/a/13146105/1174572
  TCryptography = record
    // http://msdn.microsoft.com/en-us/library/windows/desktop/aa380261(v=vs.85).aspx
    class function ProtectData(ASrc: TBytes; out ADst: TBytes): Boolean; overload; static;
    class function ProtectData(ASrc: TBytes; out ADst: TBytes; out ADstBinHex: string): Boolean; overload; static;
    class function ProtectData(ASrc: TBytes; out ADstBinHex: string): Boolean; overload; static;
    // http://msdn.microsoft.com/en-us/library/windows/desktop/aa380882%28v=vs.85%29.aspx
    class function UnprotectData(ASrc: TBytes; out ADst: TBytes): Boolean; overload; static;
    class function UnprotectData(ASrc: string; out ADst: string): Boolean; overload; static;
  end;

implementation

class function TCryptography.ProtectData(ASrc: TBytes; out ADst: TBytes): Boolean;
var
  LInData, LOutData: TDataBlob;
begin
  LOutData := Default(TDataBlob);
  LInData.cbData := Length(ASrc);
  LInData.pbData := @ASrc[0];
  Result := CryptProtectData(@LInData, nil, nil, nil, nil, 0, @LOutData);
  if Result then
  begin
    SetLength(ADst, LOutData.cbData);
    Move(LOutData.pbData^, ADst[0], LOutData.cbData);
    LocalFree(HLOCAL(LOutData.pbData));
  end;
end;

class function TCryptography.ProtectData(ASrc: TBytes; out ADst: TBytes; out ADstBinHex: string): Boolean;
var
  LText: TBytes;
  LLen: Integer;
begin
  Result := ProtectData(ASrc, ADst);
  if Result then
  begin
    LLen := Length(ADst) * 2 +1;
    SetLength(LText, LLen);
    BinToHex(ADst, 0, LText, 0, LLen);
    ADstBinHex := TEncoding.UTF8.GetString(LText);
  end;
end;

class function TCryptography.ProtectData(ASrc: TBytes; out ADstBinHex: string): Boolean;
var
  LEncrypt: TBytes;
begin
  Result := ProtectData(ASrc, LEncrypt, ADstBinHex);
end;

class function TCryptography.UnprotectData(ASrc: string; out ADst: string): Boolean;
var
  LEncrypt, LSrc, LDecrypt: TBytes;
  LLen: Integer;
begin
  LEncrypt := TEncoding.UTF8.GetBytes(ASrc);
  LLen := Length(LEncrypt) div 2;
  SetLength(LSrc, LLen);
  HexToBin(LEncrypt, 0, LSrc, 0, LLen);
  Result := UnprotectData(LSrc, LDecrypt);
  if Result then
    ADst := TEncoding.UTF8.GetString(LDecrypt);
end;

class function TCryptography.UnprotectData(ASrc: TBytes; out ADst: TBytes): Boolean;
var
  LInData, LOutData: TDataBlob;
begin
  LOutData := Default(TDataBlob);
  LInData.cbData := Length(ASrc);
  LInData.pbData := @ASrc[0];
  Result := CryptUnprotectData(@LInData, nil, nil, nil, nil, 0, @LOutData);
  if Result then
  begin
    SetLength(ADst, LOutData.cbData);
    Move(LOutData.pbData^, ADst[0], LOutData.cbData);
    LocalFree(HLOCAL(LOutData.pbData));
  end
end;

end.
