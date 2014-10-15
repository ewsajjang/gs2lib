unit mUtils.Windows.Printer;

interface

uses
	Vcl.Printers,
  Winapi.WinSpool, Winapi.Windows;

type
  TPrinterDlg = class
  private const
    PRINTER_FDEFAULTS: TPrinterDefaults = (
      pDatatype: nil;
      pDevMode: nil;
      DesiredAccess: STANDARD_RIGHTS_REQUIRED or PRINTER_ACCESS_USE);
    BUFFER_LEN = 256;
  private class var
    FDeviceName: array [0..BUFFER_LEN - 1] of Char;
    FDriverName: array [0..BUFFER_LEN - 1] of Char;
    FPort: array [0..BUFFER_LEN - 1] of Char;
    FDeviceHandle: THandle;
    FHandle: THandle;
    FPrinterInfoLen: Cardinal;
  private
    class procedure InitValues(const APrinterIdx: Integer);
  public
    class procedure PropertiesOpen(APrinterIdx: Integer = -1);
    class procedure DocumentPropertiesOpen(APrinterIdx: Integer = -1);
  end;

  TPrinterHelper = class helper for TPrinter
  private
    function GetCounter: Integer;
    function GetNames(Index: Integer): String;
    function GetReadableNames(Index: Integer): String;
    function GetDefaultName: String;
    function GetDefaultIndex: Integer;
  public
  	function IdxOfName(const APrinterName: String): Integer;

    property DefaultIndex: Integer read GetDefaultIndex;
    property DefaultName: String read GetDefaultName;
  	property Count: Integer read GetCounter;
    property Names[Index: Integer]: String read GetNames;
    property ReadableNames[Index: Integer]: String read GetReadableNames;
  end;

implementation

uses
	mConsts,
  Winapi.ShlObj, Vcl.Forms, System.SysUtils;

{ TPrinterPropertiesDlg }

class procedure TPrinterDlg.InitValues(const APrinterIdx: Integer);
begin
  Printer.PrinterIndex := APrinterIdx;

  ZeroMemory(@FDeviceName[0], BUFFER_LEN);
  ZeroMemory(@FDriverName[0], BUFFER_LEN);
  ZeroMemory(@FPort[0], BUFFER_LEN);
  FDeviceHandle := INVALID_HANDLE_VALUE;
  FHandle := INVALID_HANDLE_VALUE;
end;

class procedure TPrinterDlg.PropertiesOpen(APrinterIdx: Integer);
begin
  InitValues(APrinterIdx);
  Printer.GetPrinter(FDeviceName, FDriverName, FPort, FDeviceHandle);
  if OpenPrinter(@FDeviceName, FHandle, @PRINTER_FDEFAULTS) then
    try
      PrinterProperties(Application.MainForm.Handle, FHandle);
    finally
      ClosePrinter(FHandle);
    end;
end;

class procedure TPrinterDlg.DocumentPropertiesOpen(
  APrinterIdx: Integer);
var
  LPrinterInfo2: PPrinterInfo2;
  LDlgResult: Integer;
begin
  InitValues(APrinterIdx);
  Printer.GetPrinter(FDeviceName, FDriverName, FPort, FDeviceHandle);
  if OpenPrinter(@FDeviceName, FHandle, @PRINTER_FDEFAULTS) then
    try
      SetLastError(0);
      // See - http://msdn.microsoft.com/en-us/library/windows/desktop/dd144911(v=vs.85).aspx or Search 'msdn GetPrinter' in google :)
      if not GetPrinter(FHandle, 2, nil, 0, @FPrinterInfoLen) then
      begin
        LPrinterInfo2 := AllocMem(FPrinterInfoLen);
        try
          if GetPrinter(FHandle, 2, LPrinterInfo2, FPrinterInfoLen, @FPrinterInfoLen) then
          begin
            LDlgResult := DocumentProperties(
              Application.MainForm.Handle,
              FHandle,
              @FDeviceName,
              LPrinterInfo2.pDevMode^,
              LPrinterInfo2.pDevMode^,
              DM_IN_PROMPT or DM_IN_BUFFER or DM_OUT_BUFFER);
            if LDlgResult = IDOK then
              Winapi.WinSpool.SetPrinter(FHandle, 2, LPrinterInfo2, 0);
          end;
        finally
          FreeMem(LPrinterInfo2, FPrinterInfoLen);
        end;
      end;
    finally
      ClosePrinter(FHandle);
    end;
end;

var
	FDefaultPrinterIdx: Integer = VAL_NOT_ASSIGNED;

{ TPrinter }

function TPrinterHelper.GetCounter: Integer;
begin
	Result := Printers.Count;
end;

function TPrinterHelper.GetDefaultIndex: Integer;
begin
	Result := FDefaultPrinterIdx;
end;

function TPrinterHelper.GetDefaultName: String;
begin
	Result := Printers[FDefaultPrinterIdx];
end;

function TPrinterHelper.GetNames(Index: Integer): String;
begin
	Result := Printers[Index];
end;

function TPrinterHelper.IdxOfName(const APrinterName: String): Integer;
var
  i: Integer;
begin
	Result := VAL_NOT_ASSIGNED;
  for i := 0 to Count -1 do
		if ReadableNames[i].Contains(APrinterName) then
    	Result := i;
end;

function TPrinterHelper.GetReadableNames(Index: Integer): String;
begin
	Result := Printers[Index].Replace('&', EmptyStr, [rfReplaceAll])
end;

initialization
	FDefaultPrinterIdx := Printer.PrinterIndex;

finalization

end.
