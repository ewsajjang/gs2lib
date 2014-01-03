unit mUtils.Windows;

interface

uses
  System.SysUtils, System.Classes,
  System.Win.Registry, System.IOUtils,

  WinApi.Windows, Winapi.ShlObj;

function ReadRegNameString(const AKey: HKEY; const ARegKey, AName: String): String;
function WriteRegString(const AKey: HKEY; const ARegKey, AName, AValue: String): Boolean;
function AppDataPath(AOrgName: String = ''): String;
function GetSpecialFolder(CSIDL_VALUE: Integer): string;
function ShellExecuteFile(const FileName: String; Parameters: String = ''; Directory: String = ''): Integer;

function ComObjExists(ClassName: string): Boolean;
function ComObjRunning(ClassName: string): Boolean;
function ComObjRequest(ClassName: String; var AComObj: OLEVariant): Boolean;

function MakeUniqueFileName(const APath, AFileName: string): string;
procedure MyDocumentFile(var AFileName: String);
procedure EmailByOutlook(const Subject, Body, FileName: String);

function ExeVersion(FileName: String = ''): String;
function ExtractExePath: String;

type
  TFileHelper = record helper for TFile
    class function Size(const AFileName: String): Int64; static;
    class function ModuleFileName: String; static;
  end;

const
  CLASS_NAME_OFICE_WORLD  = 'Word.Application';
  CLASS_NAME_OFICE_EXCEL  = 'Excel.Application';
  CLASS_NAME_OFICE_OUTLOOK= 'Outlook.Application';
  CLASS_NAME_OFICE_ACCESS = 'Access.Application';
  CLASS_NAME_OFICE_PPT    = 'Powerpoint.Application';

implementation

uses
  System.Win.ComObj, System.Variants, Winapi.ActiveX, Winapi.ShellAPI;

function ComObjExists(ClassName: string): Boolean;
var
  LClassID: TCLSID;
begin
  Result := Succeeded(CLSIDFromProgID(PWideChar(ClassName), LClassID));
end;

function ComObjRunning(ClassName: string): Boolean;
var
  ClassID: TCLSID;
  Unknown: IUnknown;
begin
  try
    ClassID := ProgIDToClassID(ClassName);
    Result  := GetActiveObject(ClassID, nil, Unknown) = S_OK;
  except
    Result := False;
  end;
end;

function ComObjRequest(ClassName: String; var AComObj: OLEVariant): Boolean;
begin
  try
    if ComObjExists(ClassName) then
  begin
    if ComObjRunning(ClassName) then
      AComObj := GetActiveOleObject(ClassName)
    else
      AComObj := CreateOleObject(ClassName);
  end;
  except
  end;

  Result := not VarIsClear(AComObj);
end;

function ReadRegNameString(const AKey: HKEY; const ARegKey, AName: String): String;
var
  LReg: TRegistry;
begin
  Result := EmptyStr;

  LReg := TRegistry.Create;
  try
    LReg.RootKey := AKey;
    if LReg.KeyExists(ARegKey) then if LReg.OpenKeyReadOnly(ARegKey) then
    begin
      if LReg.ValueExists(AName) then
        Result := LReg.ReadString(AName);
      LReg.CloseKey;
    end;
  finally
    FreeAndNil(LReg);
  end;
end;

function WriteRegString(const AKey: HKEY; const ARegKey, AName, AValue: String): Boolean;
var
  LReg: TRegistry;
begin
  LReg := TRegistry.Create;
  try
    LReg.RootKey := AKey;
    try
      if LReg.KeyExists(ARegKey) then if LReg.OpenKeyReadOnly(ARegKey) then
      begin
        if LReg.ValueExists(AName) then
          LReg.WriteString(AName, AValue);
        LReg.CloseKey;
      end;
    except on E: Exception do
    end;
  finally
    FreeAndNil(LReg);
  end;
  Result := True;
end;

function GetSpecialFolder(CSIDL_VALUE: Integer): string;
var
  PIDL: PItemIDList;
begin
  SetLength(Result, MAX_PATH);
  SHGetSpecialFolderLocation(0, CSIDL_VALUE, PIDL);
  SHGetPathFromIDList(PIDL, PChar(Result));
  SetLength(Result, StrLen(PChar(Result)));
  Result := IncludeTrailingPathDelimiter(Result);
end;

function AppDataPath(AOrgName: String): String;
begin
  Result := GetSpecialFolder(CSIDL_COMMON_APPDATA) + AOrgName;
  Result := IncludeTrailingPathDelimiter(Result);
  ForceDirectories(Result);
end;

function ShellExecuteFile(const FileName: String; Parameters: String = ''; Directory: String = ''): Integer;
begin
  Result := ShellExecute(0, 'open', PChar(FileName), PChar(Parameters),
    PChar(Directory), SW_SHOWNORMAL);
end;

function MakeUniqueFileName(const APath, AFileName: string): string;
var
  UniqueName: array[0..MAX_PATH-1] of Char;
begin
  Result := IncludeTrailingPathDelimiter(APath) + AFileName;

  if FileExists( Result ) then
    if PathMakeUniqueName( UniqueName, Length(UniqueName), PChar(AFileName), nil, PChar(APath) ) then
      Result := UniqueName;
end;

procedure MyDocumentFile(var AFileName: String);
var
  LPath: String;
begin
  LPath := GetSpecialFolder(CSIDL_COMMON_DOCUMENTS);
  AFileName := MakeUniqueFileName(LPath, AFileName);
end;

procedure EmailByOutlook(const Subject, Body, FileName: String);
const
  olMailItem = 0;
  olByValue = 1;
var
  LOutlook, LMailItem, LAttachments: OLEVariant;
  LHandle: THandle;
begin
  if ComObjRequest(CLASS_NAME_OFICE_OUTLOOK, LOutlook) then
  begin
    LMailItem := LOutlook.CreateItem(olMailItem);
    try
      LMailItem.Subject := Subject;
      LMailItem.Body    := Body;
      LAttachments      := LMailItem.Attachments;
      LAttachments.Add(FileName, olByValue, 1, ExtractFileName(FileName));
      LMailItem.Display(False);
      LHandle := FindWindow('rctrl_renwnd32', nil);
      SetForegroundWindow(LHandle);
    finally
      LAttachments := VarNull;
      LOutlook    := VarNull;
    end;
  end;
end;

function ExeVersion(FileName: String): String;
const
  InfoNum = 9;
  InfoStr: array [0 .. InfoNum] of string = ('CompanyName', 'FileDescription',
    'FileVersion', 'InternalName', 'LegalCopyright', 'LegalTradeMarks',
    'OriginalFileName', 'ProductName', 'ProductVersion', 'Comments');
type
  TExeFileVerInfo = record
    Translation,
    CompanyName,
    FileDescription,
    FileVersion,
    InternalName,
    LegalCopyright,
    LegalTradeMarks,
    OriginalFileName,
    ProductName,
    ProductVersion,
    Comments: string;
  end;

  LANGANDCODEPAGE = record
    wLanguage: WORD;
    wCodePage: WORD;
  end;
var
  Lang: string;
  FileVersionInfoSize, Len, j, i, nSize: DWORD;
  Buffer: Pointer;
  Value: PChar;
  lcpage: array of LANGANDCODEPAGE;
  VerInfo: TExeFileVerInfo;
begin
  if FileName = EmptyStr then
    FileName := TFile.ModuleFileName;
  Result := EmptyStr;
  if not FileExists(FileName) then
    Exit;

  FileVersionInfoSize := GetFileVersionInfoSize(PChar(FileName), FileVersionInfoSize);
  if FileVersionInfoSize > 0 then
  begin
    Buffer := AllocMem(FileVersionInfoSize);
    try
      if not GetFileVersionInfo(PChar(FileName), 0, FileVersionInfoSize, Buffer) then
        GetLastError();

      if VerQueryValue(Buffer, PChar('\VarFileInfo\Translation'), Pointer(Value), Len) then
      begin
        nSize := Len div SizeOf(LANGANDCODEPAGE);
        SetLength(lcpage, nSize);
        Move(Value^, lcpage[0], Len);
      end
      else
        Exit;

      for i := 0 to nSize - 1 do
      begin
        Lang := Format('%.4x%.4x', [lcpage[i].wLanguage, lcpage[i].wCodePage]);
        VerInfo.Translation := Lang;
        for j := Low(InfoStr) to High(InfoStr) do
          if VerQueryValue(Buffer, PChar('\StringFileInfo\' + Lang + '\' + InfoStr[j]), Pointer(Value), Len) then
          begin
            case j of
              0: VerInfo.CompanyName := Value;
              1: VerInfo.FileDescription := Value;
              2: VerInfo.FileVersion := Value;
              3: VerInfo.InternalName := Value;
              4: VerInfo.LegalCopyright := Value;
              5: VerInfo.LegalTradeMarks := Value;
              6: VerInfo.OriginalFileName := Value;
              7: VerInfo.ProductName := Value;
              8: VerInfo.ProductVersion := Value;
              9: VerInfo.Comments := Value;
            end;
          end;
      end;
    finally
      FreeMem(Buffer, FileVersionInfoSize);
    end;
    Result := VerInfo.FileVersion;
  end;
end;

function ExtractExePath: String;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result);
end;

{ TFileHelper }

class function TFileHelper.ModuleFileName: String;
var
  LBuffer: array [0 .. MAX_PATH - 1] of Char;
begin
  GetModuleFileName(HInstance, LBuffer, SizeOf(LBuffer));
  Result := LBuffer;
end;

class function TFileHelper.Size(const AFileName: String): Int64;
var
  LSr: TSearchRec;
begin
  Result := -1;
  if AFileName.IsEmpty or not FileExists(AFileName) then
    Exit;

  FindFirst(ModuleFileName, faAnyFile, LSr);
  Result := LSr.Size;
end;

end.
