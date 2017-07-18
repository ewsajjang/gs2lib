unit mFileInfo;

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils
  ;

type
  TFileInfo = record
  public type
    TLanguage = (
      {$REGION 'Language Enums'}
      flArabic,
      flBulgarian,
      flCatalan,
      flTraditionalChinese,
      flCzech,
      flDanish,
      flGerman,
      flGreek,
      flUSEnglish,
      flCastilianSpanish,
      flFinnish,
      flFrench,
      flHebrew,
      flHungarian,
      flIcelandic,
      flItalian,
      flJapanese,
      flKorean,
      flDutch,
      flNorwegianBokmal,
      flSwissItalian,
      flBelgianDutch,
      flNorwegianNynorsk,
      flPolish,
      flPortugueseBrazil,
      flRhaetoRomanic,
      flRomanian,
      flRussian,
      flCroatoSerbianLatin,
      flSlovak,
      flAlbanian,
      flSwedish,
      flThai,
      flTurkish,
      flUrdu,
      flBahasa,
      flSimplifiedChinese,
      flSwissGerman,
      flUKEnglish,
      flSpanishMexico,
      flBelgianFrench,
      flCanadianFrench,
      flSwissFrench,
      flPortuguesePortugal,
      flSerboCroatianCyrillic
      {$ENDREGION}
    );
    TLanguageHelper = record helper for TLanguage
      function ToWord: Word;
      class function Create(const AValue: Word): TLanguage; static;
    end;
    TCodePage = (
      fcASCII7bit,
      fcJapanShiftJISX0208,
      fcKoreaShiftKSC5601,
      fcTaiwanBig5,
      fcUnicode,
      fcLatin2EasternEuropean,
      fcCyrillic,
      fcMultilingual,
      fcGreek,
      fcTurkish,
      fcHebrew,
      fcArabic
    );
    TCodePageHelper = record helper for TCodePage
      function ToWord: Word;
      class function Create(const AValue: Word): TCodePage; static;
    end;
    TTransItemIdx = 0..9;
    TTranslation = record
    private
      FLanguage: TLanguage;
      FCodePage: TCodePage;
      function GetwCodePage: Word;
      function GetwLanguage: Word;
    public
      constructor Create(const AValue: Cardinal);
      class function Default: TTranslation; static;
      function ToHexStr: String;
      function Equals(const ALanguage: TLanguage; ACodePage: TCodePage): Boolean; overload;
      function Equals(const ALanguage: TLanguage): Boolean; overload;
      property wLanguage: Word read GetwLanguage;
      property wCodePage: Word read GetwCodePage;
    end;
    TVersion = record
    private
      FVersionMS, FVersionLS: Cardinal;
      function GetVersion(const Index: Integer): Integer;
    public
      constructor Create(const AVersionMS, AVersionLS: Cardinal);

      function ToString(const AFmt: String = ''): String;

      property Major: Integer index 0 read GetVersion;
      property Minor: Integer index 1 read GetVersion;
      property Release: Integer index 2 read GetVersion;
      property Build: Integer index 3 read GetVersion;
    end;
    TBuildFlag = (
      bfRelease,
      bfDebug,
      bfPreRelease,
      bfPrivate
    );
    TBuildFlags = set of TBuildFlag;
    TBuildFlagsHelper = record helper for TBuildFlags
      class function Create(const AValue: Cardinal): TBuildFlags; static;
    end;
  private
    FTransIdx: Integer;
    FTrans: TArray<TTranslation>;
    FFileVer: TVersion;
    FProductVer: TVersion;
    FFileInfos: TArray<array[TTransItemIdx] of String>;
    FBuildFlags: TBuildFlags;
    FFileFullPath: String;
    function GetFileInfos(const Index: Integer): String;
    function GetTransCnt: Integer;
    function GetTranslation: TTranslation;
    function GetDirectory: String;
    function GetFileName: String;
  public
    function Load(const AFileName: String): Boolean;
    function TryAssignTranslation(const ALang: TLanguage; const ACodePage: TCodePage): Boolean; overload;
    function TryAssignTranslation(const ALang: TLanguage): Boolean; overload;

    property FileFullPath: String read FFileFullPath;
    property Directory: String read GetDirectory;
    property FileName: String read GetFileName;
    property BuildFlags: TBuildFlags read FBuildFlags;
    property FileVersion: TVersion read FFileVer;
    property ProductVersion: TVersion read FProductVer;

    property Translation: TTranslation read GetTranslation;
    property TransItemCnt: Integer read GetTransCnt;

    property Comments         : String index 0 read GetFileInfos;
    property CompanyName      : String index 1 read GetFileInfos;
    property FileDescription  : String index 2 read GetFileInfos;
    property InternalName     : String index 3 read GetFileInfos;
    property LegalCopyright   : String index 4 read GetFileInfos;
    property LegalTrademarks  : String index 5 read GetFileInfos;
    property OriginalFilename : String index 6 read GetFileInfos;
    property PrivateBuild     : String index 7 read GetFileInfos;
    property ProductName      : String index 8 read GetFileInfos;
    property SpecialBuild     : String index 9 read GetFileInfos;
  end;

function ModuleFileName: String;

implementation

uses
  System.IOUtils, System.Math
  ;

function ModuleFileName: String;
var
  LBuffer: array [0..MAX_PATH] of Char;
begin
  SetString(Result, LBuffer, GetModuleFileName(0, LBuffer, Length(LBuffer)))
end;

{ TFileInfo.TTranslation }

constructor TFileInfo.TTranslation.Create(const AValue: Cardinal);
begin
  FLanguage := TLanguage.Create(AValue and $FFFF);          // Lo
  FCodePage := TCodePage.Create((AValue shr 16) and $FFFF); // Hi
end;

class function TFileInfo.TTranslation.Default: TTranslation;
begin
  Result := TTranslation.Create(
    (flUSEnglish.ToWord shl 16) or
    fcUnicode.ToWord
  );
end;

function TFileInfo.TTranslation.Equals(const ALanguage: TLanguage): Boolean;
begin
  Result := FLanguage = ALanguage;
end;

function TFileInfo.TTranslation.Equals(const ALanguage: TLanguage; ACodePage: TCodePage): Boolean;
begin
  Result := (FLanguage = ALanguage) and (FCodePage = ACodePage)
end;

function TFileInfo.TTranslation.GetwCodePage: Word;
begin
  Result := FCodePage.ToWord;
end;

function TFileInfo.TTranslation.GetwLanguage: Word;
begin
  Result := FLanguage.ToWord;
end;

function TFileInfo.TTranslation.ToHexStr: String;
begin
  Result := wLanguage.ToHexString(4) + wCodePage.ToHexString(4);
end;

{ TFileInfo.TVersion }

constructor TFileInfo.TVersion.Create(const AVersionMS, AVersionLS: Cardinal);
begin
  FVersionMS := AVersionMS;
  FVersionLS := AVersionLS;
end;

function TFileInfo.TVersion.GetVersion(const Index: Integer): Integer;
begin
  Result := 0;
  case Index of
    0: Result := HiWord(FVersionMS);
    1: Result := LoWord(FVersionMS);
    2: Result := HiWord(FVersionLS);
    3: Result := LoWord(FVersionLS);
  end;
end;

function TFileInfo.TVersion.ToString(const AFmt: String): String;
begin
  if AFmt.IsEmpty then
    Result := Format('%d.%d.%d.%d', [Major, Minor, Release, Build])
  else
    try
      Result := Format(AFmt, [Major, Minor, Release, Build])
    except on E: Exception do
      Result := ToString;
    end;
end;

{ TFileInfo.TBuildFlagsHelper }

class function TFileInfo.TBuildFlagsHelper.Create(const AValue: Cardinal): TBuildFlags;
begin
  Result := [];
  if AValue = 0 then
    Exit([bfRelease]);

  if (AValue and VS_FF_DEBUG) <> 0 then
    Result := Result + [bfDebug];

  if (AValue and VS_FF_PRERELEASE) <> 0 then
    Result := Result + [bfPreRelease];

  if (AValue and VS_FF_PRIVATEBUILD) <> 0 then
    Result := Result + [bfPrivate];
end;

{ TFileInfo }

function TFileInfo.GetDirectory: String;
begin
  Result := TDirectory.GetParent(FFileFullPath);
end;

function TFileInfo.GetFileInfos(const Index: Integer): String;
begin
  Result := FFileInfos[FTransIdx][Index]
end;

function TFileInfo.GetFileName: String;
begin
  Result := TPath.GetFileName(FFileFullPath);
end;

function TFileInfo.GetTransCnt: Integer;
begin
  Result := Length(FTrans);
end;

function TFileInfo.GetTranslation: TTranslation;
begin
  Result := FTrans[FTransIdx];
end;

function TFileInfo.Load(const AFileName: String): Boolean;
const
  SSlash: String = '\';
  STrans: String = '\VarFileInfo\Translation';
  SFmtFileVers: String = '\StringFileInfo\%s\FileVersion';
  SFmtInfo: String = '\StringFileInfo\%s\%s';
  SInfoPaths: array[0..9] of String = (
    'Comments',
    'CompanyName',
    'FileDescription',
    //'FileVersion',
    'InternalName',
    'LegalCopyright',
    'LegalTrademarks',
    'OriginalFilename',
    'PrivateBuild',
    'ProductName',
    //'ProductVersion',
    'SpecialBuild'
  );
type
  TLanguageCodePage = record
    wLanguage: WORD;
    wCodePage: WORD;
  end;
var
  LWnd, LInfoSz, LBufLen: DWORD;
  LInfo: Pointer;
  LLangCodePage: TArray<TLanguageCodePage>;
  LBuf: PChar;
  LFileInfo: ^TVSFixedFileInfo;
  i, t: Integer;
  LVer: String;
begin
  Result := False;
  FBuildFlags := [];
  FFileInfos := [];
  if not TFile.Exists(AFileName) then
    Exit;

  FFileFullPath := AFileName;
  try
    LInfoSz := GetFileVersionInfoSize(PChar(AFileName), LWnd);
    if LInfoSz <= 0 then
      Exit;

    GetMem(LInfo, LInfoSz);
    try
      if GetFileVersionInfo(PChar(AFileName), LWnd, LInfoSz, LInfo) then
      begin
        if VerQueryValue(LInfo, PChar(SSlash), Pointer(LFileInfo), LBufLen) then
        begin
          FBuildFlags := TBuildFlags.Create(LFileInfo.dwFileFlags);
          FFileVer := TVersion.Create(LFileInfo^.dwFileVersionMS, LFileInfo^.dwFileVersionLS);
          FProductVer := TVersion.Create(LFileInfo^.dwProductVersionMS, LFileInfo^.dwProductVersionLS);
        end;

        FTrans := [];
        if VerQueryValue(LInfo, PChar(STrans), Pointer(LBuf), LBufLen) then
        begin
          SetLength(FTrans, LBufLen div SizeOf(TLanguageCodePage));

          SetLength(LLangCodePage, TransItemCnt);
          Move(LBuf^, LLangCodePage[0], LBufLen);

          SetLength(FFileInfos, TransItemCnt);

          FTransIdx := IfThen(TransItemCnt > 1, -1, 0);
          for t := 0 to TransItemCnt -1 do
          begin
            FTrans[t] := TTranslation.Create(PCardinal(@LLangCodePage[t])^);
            if FTransIdx = -1 then
              if VerQueryValue(LInfo, PChar(Format(SFmtFileVers, [FTrans[t].ToHexStr])), Pointer(LBuf), LBufLen) then
              begin
                LVer := LBuf;
                if LVer.StartsWith('v') or LVer.StartsWith('V') then
                  LVer := LVer.Substring(1);
                if FFileVer.ToString.Equals(LVer) then
                  FTransIdx := t;
              end;

            for i := 0 to Length(SInfoPaths) -1 do
              if VerQueryValue(LInfo, PChar(Format(SFmtInfo, [FTrans[t].ToHexStr, SInfoPaths[i]])), Pointer(LBuf), LBufLen) then
                FFileInfos[t][i] := LBuf
              else
                FFileInfos[t][i] := '';
          end;
          if FTransIdx = -1 then
            for t := 0 to TransItemCnt -1 do
              if FTrans[t].Equals(flUSEnglish) then
              begin
                FTransIdx := t;
                Break;
              end;
          if FTransIdx = -1 then
            FTransIdx := TransItemCnt -1;
        end;
        Exit(True);
      end;
    finally
      FreeMem(LInfo, LInfoSz);
    end;
  except on E: Exception do
    Result := False;
  end;
end;

function TFileInfo.TryAssignTranslation(const ALang: TLanguage): Boolean;
var
  t: Integer;
begin
  Result := False;
  for t := 0 to TransItemCnt -1 do
    if FTrans[t].Equals(ALang) then
    begin
      FTransIdx := t;
      Exit(True);
    end;
end;

function TFileInfo.TryAssignTranslation(const ALang: TLanguage; const ACodePage: TCodePage): Boolean;
var
  t: Integer;
begin
  Result := False;
  for t := 0 to TransItemCnt -1 do
    if FTrans[t].Equals(ALang, ACodePage) then
    begin
      FTransIdx := t;
      Exit(True);
    end;
end;

{ TFileInfo.TLanguageHelper }

class function TFileInfo.TLanguageHelper.Create(const AValue: Word): TLanguage;
var
  LLang: TLanguage;
begin
  Result := flUSEnglish;
  for LLang := Low(TLanguage) to High(TLanguage) do
    if LLang.ToWord = AValue then
    begin
      Result := LLang;
      Break;
    end;
end;

function TFileInfo.TLanguageHelper.ToWord: Word;
begin
  case Self of
    flArabic                : Result := $0401;
    flBulgarian             : Result := $0402;
    flCatalan               : Result := $0403;
    flTraditionalChinese    : Result := $0404;
    flCzech                 : Result := $0405;
    flDanish                : Result := $0406;
    flGerman                : Result := $0407;
    flGreek                 : Result := $0408;
    flUSEnglish             : Result := $0409;
    flCastilianSpanish      : Result := $040A;
    flFinnish               : Result := $040B;
    flFrench                : Result := $040C;
    flHebrew                : Result := $040D;
    flHungarian             : Result := $040E;
    flIcelandic             : Result := $040F;
    flItalian               : Result := $0410;
    flJapanese              : Result := $0411;
    flKorean                : Result := $0412;
    flDutch                 : Result := $0413;
    flNorwegianBokmal       : Result := $0414;
    flSwissItalian          : Result := $0810;
    flBelgianDutch          : Result := $0813;
    flNorwegianNynorsk      : Result := $0814;
    flPolish                : Result := $0415;
    flPortugueseBrazil      : Result := $0416;
    flRhaetoRomanic         : Result := $0417;
    flRomanian              : Result := $0418;
    flRussian               : Result := $0419;
    flCroatoSerbianLatin    : Result := $041A;
    flSlovak                : Result := $041B;
    flAlbanian              : Result := $041C;
    flSwedish               : Result := $041D;
    flThai                  : Result := $041E;
    flTurkish               : Result := $041F;
    flUrdu                  : Result := $0420;
    flBahasa                : Result := $0421;
    flSimplifiedChinese     : Result := $0804;
    flSwissGerman           : Result := $0807;
    flUKEnglish             : Result := $0809;
    flSpanishMexico         : Result := $080A;
    flBelgianFrench         : Result := $080C;
    flCanadianFrench        : Result := $0C0C;
    flSwissFrench           : Result := $100C;
    flPortuguesePortugal    : Result := $0816;
    flSerboCroatianCyrillic : Result := $081A;
  else
    raise Exception.Create('Handled code not exists');
  end;
end;

{ TFileInfo.TCodePageHelper }

class function TFileInfo.TCodePageHelper.Create(const AValue: Word): TCodePage;
var
  LCodePage: TCodePage;
begin
  Result := fcUnicode;
  for LCodePage := Low(TCodePage) to High(TCodePage) do
    if LCodePage.ToWord = AValue then
    begin
      Result := LCodePage;
      Break;
    end;
end;

function TFileInfo.TCodePageHelper.ToWord: Word;
begin
  case Self of
    fcASCII7bit             : Result := 0;
    fcJapanShiftJISX0208    : Result := 932;
    fcKoreaShiftKSC5601     : Result := 949;
    fcTaiwanBig5            : Result := 950;
    fcUnicode               : Result := 1200;
    fcLatin2EasternEuropean : Result := 1250;
    fcCyrillic              : Result := 1251;
    fcMultilingual          : Result := 1252;
    fcGreek                 : Result := 1253;
    fcTurkish               : Result := 1254;
    fcHebrew                : Result := 1255;
    fcArabic                : Result := 1256;
  else
    raise Exception.Create('Handled code not exists');
  end;
end;

//var
//  LInfo: TFileInfo;

initialization
  //LInfo.Load(TPath.GetFullPath('.\MultiVer.exe'));
  //LInfo.Load(ParamStr(0));

end.
