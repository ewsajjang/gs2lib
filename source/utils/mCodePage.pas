unit mCodePage;

interface

// The following codepage array is place at System.SysUtils.CharSetNameToCodePage

type
  TCodePage = record
    type
      TCodePageInfo = record
        Name: string;
        CodePage: Cardinal;
      end;
    const
      ItemCnt = 35;
    type
      TCodePageArray = array[0..ItemCnt -1] of TCodePage.TCodePageInfo;
    const
      Items: TCodePageArray = (
        (Name: 'Latin-US (DOS)'; CodePage: 437),                          { 00 }
        (Name: 'Western (DOS Latin 1)'; CodePage: 850),                   { 01 }
        (Name: 'Thai (Windows, DOS)'; CodePage: 874),                     { 02 }
        (Name: 'Japanese (Windows, DOS)'; CodePage: 932),                 { 03 }
        (Name: 'Simplified Chinese (Windows, DOS)'; CodePage: 936),       { 04 }
        (Name: 'Korean (Windows, DOS)'; CodePage: 949),                   { 05 }
        (Name: 'Traditional Chinese (Windows, DOS)'; CodePage: 950),      { 06 }
        (Name: 'Unicode (UTF-16)'; CodePage: 1200),                       { 07 }
        (Name: 'Unicode (UTF-16LE)'; CodePage: 1200),                     { 08 }
        (Name: 'Unicode (UTF-16BE)'; CodePage: 1201),                     { 09 }
        (Name: 'Central European (Windows Latin 2)'; CodePage: 1250),     { 10 }
        (Name: 'Cyrillic (Windows)'; CodePage: 1251),                     { 11 }
        (Name: 'Western (Windows Latin 1)'; CodePage: 1252),              { 12 }
        (Name: 'Greek (Windows)'; CodePage: 1253),                        { 13 }
        (Name: 'Turkish (Windows Latin 5)'; CodePage: 1254),              { 14 }
        (Name: 'Hebrew (Windows)'; CodePage: 1255),                       { 15 }
        (Name: 'Arabic (Windows)'; CodePage: 1256),                       { 16 }
        (Name: 'Baltic (Windows)'; CodePage: 1257),                       { 17 }
        (Name: 'Vietnamese (Windows)'; CodePage: 1258),                   { 18 }
        (Name: 'Western (ASCII)'; CodePage: 20127),                       { 19 }
        (Name: 'Unicode (UTF-7)'; CodePage: CP_UTF7),                     { 20 }
        (Name: 'Unicode (UTF-8)'; CodePage: CP_UTF8),                     { 21 }
        // Windows code pages...
        (Name: 'Windows-1252'; CodePage: 1252),                           { 22 }
        (Name: 'US-ASCII'; CodePage: 20127),                              { 23 }
        (Name: 'UTF-7'; CodePage: CP_UTF7),                               { 24 }
        (Name: 'UTF-8'; CodePage: CP_UTF8),                               { 25 }
        (Name: 'UTF-16'; CodePage: 1200),                                 { 26 }
        (Name: 'UTF-16BE'; CodePage: 1201),                               { 27 }
        (Name: 'UTF-16LE'; CodePage: 1200),                               { 28 }
        (Name: 'SHIFT-JIS'; CodePage: 932),                               { 29 }
        (Name: 'ISO-8859-1'; CodePage: 28591),                            { 30 }
        (Name: 'iso-8859-1'; CodePage: 28591),                            { 31 }
        (Name: 'MACCROATIAN'; CodePage: 10082),                           { 32 }
        (Name: 'ASCII'; CodePage: 20127),                                 { 33 }
        (Name: ''; CodePage: 0)                                           { 34 }
      );
    class function Validate(const AValue: Integer): Boolean; static;
    class function Unicode: Integer; static;
    class function EucKr: Integer; static;
  end;


implementation

{ TCodePage }

class function TCodePage.EucKr: Integer;
begin
  Result := Items[05].CodePage
end;

class function TCodePage.Unicode: Integer;
begin
  Result := Items[07].CodePage;
end;

class function TCodePage.Validate(const AValue: Integer): Boolean;
var
  i: Integer;
begin
  for i := 0 to ItemCnt -1 do
  begin
    Result := Items[i].CodePage = Cardinal(AValue);
    if Result then
      Break;
  end;
end;

end.
