unit mPrinter;

interface

uses
  Winapi.WinSpool, Winapi.Windows
  ;

type
  TPrinterProperties = class
  private const
    PRINTER_FDEFAULTS: TPrinterDefaults = (
      pDatatype: nil;
      pDevMode: nil;
      DesiredAccess: STANDARD_RIGHTS_REQUIRED or PRINTER_ACCESS_USE);
    NBufLen = 256;
  private class var
    FDeviceName: array [0..NBufLen -1] of Char;
    FDriverName: array [0..NBufLen -1] of Char;
    FPort: array [0..NBufLen -1] of Char;
    FDeviceHandle: THandle;
    FHandle: THandle;
    FPrinterInfoLen: Cardinal;
  private
    class procedure InitValues(const APrinterIdx: Integer);
  public
    class procedure Open(const APrinterIdx: Integer = -1);
    class procedure DocOpen(const APrinterIdx: Integer = -1);
  end;

implementation

uses
  Vcl.Printers, Winapi.ShlObj, Vcl.Forms
  ;

{ TPrinterPropertiesDlg }

class procedure TPrinterProperties.InitValues(const APrinterIdx: Integer);
begin
  Printer.PrinterIndex := APrinterIdx;

  ZeroMemory(@FDeviceName[0], NBufLen);
  ZeroMemory(@FDriverName[0], NBufLen);
  ZeroMemory(@FPort[0], NBufLen);
  FDeviceHandle := INVALID_HANDLE_VALUE;
  FHandle := INVALID_HANDLE_VALUE;
end;

class procedure TPrinterProperties.Open(const APrinterIdx: Integer);
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

class procedure TPrinterProperties.DocOpen(const APrinterIdx: Integer);
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

end.
