{ Fork from https://gist.github.com/freeonterminate/e2316f0f829115851358 }

unit mIOUtils;

interface

uses
  System.Classes, System.SysUtils, System.IOUtils
  ;

type
  TFileHelper = record helper for System.IOUtils.TFile
  public
    class function GetSize(const AFileName: String): Int64; static;
    class function GetSizeStr(const AFileName: String): String; static;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows
  {$ENDIF}

  {$IFDEF POSIX}
  Posix.SysStat
  {$ENDIF}
  ;

{ TFileHelper }

class function TFileHelper.GetSize(const AFileName: String): Int64;
{$IFDEF MSWINDOWS}
var
  LHandle: THandle;
  LRec: TWin32FindData;
  LErrMode: Integer;
begin
  Result := -1;
  if not TFile.Exists(AFileName) then
    Exit;

  LErrMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    LHandle := FindFirstFile(PChar(AFileName), LRec);
    try
      if (LHandle <> INVALID_HANDLE_VALUE) and ((LRec.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0) then
        Result := (Int64(LRec.nFileSizeHigh) shl 32) + LRec.nFileSizeLow;
    finally
      FindClose(LHandle);
    end;
  finally
    SetErrorMode(LErrMode);
  end;
end;
{$ENDIF}

{$IFDEF POSIX}
var
  LRec: _stat;
  LMashaller: TMarshaller;
begin
  Result := -1;
  if not TFile.Exists(iFileName) then
    Exit;

  FillChar(LRec, SizeOf(LRec), 0);
  if stat(LMashaller.AsAnsi(iFileName).ToPointer, LRec) = 0 then
    Result := LRec.st_size;
end;
{$ENDIF}

class function TFileHelper.GetSizeStr(const AFileName: String): String;
const
  NKb = Int64(1024);
  NMb = NKb * NKb;
  NGb = NKb * NMb;
  NTb = NKb * NGb;
var
  LSize: Int64;
begin
  LSize := GetSize(AFileName);
  if LSize < NKb then
    Result := Format('%d bytes', [LSize + 0.])
  else if LSize < NMb then
    Result := Format('%.0n KB', [LSize / NKb])
  else if LSize < NGb then
    Result := Format('%.0n MB', [LSize / NMb])
  else if LSize < NTb then
    Result := Format('%.0n GB', [LSize / NGb])
  else
    Result := Format('%.0n TB', [LSize / NTb]);
end;

end.
