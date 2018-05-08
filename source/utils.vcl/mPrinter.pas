unit mPrinter;

interface

uses
  Winapi.WinSpool, Winapi.Windows
  ;

type
  TPrinterDlg = class
  private const
    PRINTER_FDEFAULTS: TPrinterDefaults = (
      pDatatype: nil;
      pDevMode: nil;
      DesiredAccess: {STANDARD_RIGHTS_REQUIRED or }PRINTER_ACCESS_USE);
    NBufLen = 256;
  private class var
    FDeviceName: string;
    FDriverName: string;
    FPort: string;
    FDeviceHandle: THandle;
    FHandle: THandle;
    FPrinterInfoLen: Cardinal;
  private
    class procedure InitValues(const APrinterIdx: Integer);
  public
    class procedure OpenProperties(const APrinterIdx: Integer = -1);
    class procedure OpenDocProperties(const APrinterIdx: Integer = -1);
  end;

implementation

uses
  Vcl.Printers, Winapi.ShlObj, Vcl.Forms, SysUtils
  ;

{ TPrinterPropertiesDlg }

class procedure TPrinterDlg.InitValues(const APrinterIdx: Integer);
begin
  Printer.PrinterIndex := APrinterIdx;
  SetLength(FDeviceName, NBufLen);
  SetLength(FDriverName, NBufLen);
  SetLength(FPort, NBufLen);
  FDeviceHandle := INVALID_HANDLE_VALUE;
  FHandle := INVALID_HANDLE_VALUE;
end;

class procedure TPrinterDlg.OpenProperties(const APrinterIdx: Integer);
begin
  InitValues(APrinterIdx);
  Printer.GetPrinter(PChar(FDeviceName), PChar(FDriverName), PChar(FPort), FDeviceHandle);
  if not OpenPrinter(PChar(FDeviceName), FHandle, @PRINTER_FDEFAULTS) then
    raise EInvalidOp.Create(GetLastError.ToString)
  else
    try
      PrinterProperties(Application.MainForm.Handle, FHandle);
    finally
      ClosePrinter(FHandle);
    end;
end;

class procedure TPrinterDlg.OpenDocProperties(const APrinterIdx: Integer);
var
  LPrinterInfo2: PPrinterInfo2;
  LDlgResult: Integer;
begin
  InitValues(APrinterIdx);
  Printer.GetPrinter(PChar(FDeviceName), PChar(FDriverName), PChar(FPort), FDeviceHandle);
  if not OpenPrinter(PChar(FDeviceName), FHandle, @PRINTER_FDEFAULTS) then
    raise EInvalidOp.Create(GetLastError.ToString)
  else
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
              PChar(FDeviceName),
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
