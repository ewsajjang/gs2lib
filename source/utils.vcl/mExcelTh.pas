unit mExcelTh;

interface

uses
  Excel2000, Vcl.OleServer,

  System.Classes, System.SysUtils, System.SyncObjs, System.Variants,
  WinApi.Windows, WinApi.ActiveX
  ;

type
  TExcelTh = class(TThread)
  public const
    SWindowName: PChar = 'XLMAIN';
  private
    FEvent: TEvent;
    FAppLock: TCriticalSection;
    ExcelApp: TExcelApplication;
    WorkSheet: TExcelWorksheet;
    WorkBook: TExcelWorkbook;
    FSheetLock: TCriticalSection;
    FSheets: TStringList;
    procedure BuildSheetList;
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
    function GetVersion: String;
    function GetRanges(c, r: OleVariant): OleVariant;
    procedure SetRanges(c, r: OleVariant; const Value: OleVariant);
    function GetFormulas(c, r: OleVariant): Olevariant;
    procedure SetFormulas(c, r: OleVariant; const Value: Olevariant);
    function GetIdxs(Idx: OleVariant): OleVariant;
    procedure SetIdxs(Idx: OleVariant; const Value: OleVariant);
    function GetCells(c, r: OleVariant): OleVariant;
    procedure SetCells(c, r: OleVariant; const Value: OleVariant);
  protected
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure OepmEmptySheet(const AVisible: Boolean = True);
    procedure OpenFile(const AFileName: String; const AVisible: Boolean = True);
    procedure Save;
    procedure Close(const ASaveChanges: Boolean = False);
    procedure GetSheets(var LCollection: TStrings);
    function Find(const AWhat: String; var c, r: Integer): Boolean;

    procedure Clear;
    function TryLock: Boolean;
    procedure EndLock;

    property Version: String read GetVersion;
    property Visible: Boolean read GetVisible write SetVisible;

    property Idxs[Idx: OleVariant]: OleVariant read GetIdxs write SetIdxs;
    property Cells[c, r: OleVariant]: OleVariant read GetCells write SetCells;
    property Ranges[c, r: OleVariant]: OleVariant read GetRanges write SetRanges;
    property Formulas[c, r: OleVariant]: Olevariant read GetFormulas write SetFormulas;
  end;

implementation

const
  LCID: Cardinal = LOCALE_SYSTEM_DEFAULT;

{ TExcelTh }

procedure TExcelTh.Clear;
begin
  WorkSheet.Cells.Clear;
end;

procedure TExcelTh.Close(const ASaveChanges: Boolean);
var
  i: Integer;
  LWorkbook: OleVariant;
begin
  FAppLock.Acquire;
  try
    if Assigned(ExcelApp) then
      for i := ExcelApp.Workbooks.Count downto 1 do
      begin
        LWorkbook := ExcelApp.Workbooks[i];
        try
          LWorkbook.Close(ASaveChanges);//, EmptyParam, EmptyParam, LCID);
        except on E: Exception do
        end;
      end;

    if Assigned(WorkSheet) then
      WorkSheet.Disconnect;
    if Assigned(WorkBook) then
      WorkBook.Disconnect;
    if Assigned(ExcelApp) then
    begin
      ExcelApp.Disconnect;
      ExcelApp.Quit;
    end;
  finally
    FAppLock.Release;
  end;
end;

constructor TExcelTh.Create;
begin
  inherited Create(True);

  FAppLock := TCriticalSection.Create;
  FEvent := TEvent.Create;

  ExcelApp := TExcelApplication.Create(nil);
  WorkSheet := TExcelWorksheet.Create(nil);
  WorkBook := TExcelWorkbook.Create(nil);

  ExcelApp.AutoConnect := False;
  WorkSheet.AutoConnect := False;
  WorkBook.AutoConnect := False;

  ExcelApp.ConnectKind := ckRunningOrNew;//ckNewInstance;
  WorkSheet.ConnectKind := ckRunningOrNew;//ckNewInstance;
  WorkBook.ConnectKind := ckRunningOrNew;//ckNewInstance;

  ExcelApp.ScreenUpdating[LCID] := False;
  ExcelApp.Disconnect;
  ExcelApp.Quit;

  FSheetLock := TCriticalSection.Create;
  FSheets := TStringList.Create;
end;

destructor TExcelTh.Destroy;
begin
  FreeAndNil(FSheets);
  FreeAndNil(FSheetLock);

  FreeAndNil(WorkSheet);
  FreeAndNil(WorkBook);
  FreeAndNil(ExcelApp);

  FreeAndNil(FEvent);
  FreeAndNil(FAppLock);

  inherited;
end;

procedure TExcelTh.OepmEmptySheet(const AVisible: Boolean);
begin
  FAppLock.Acquire;
  try
    ExcelApp.ScreenUpdating[LCID] := False;
    if ExcelApp.Visible[LCID] then
    begin
      WorkSheet.Disconnect;
      WorkBook.Close(False, EmptyParam, False, LCID);
      WorkBook.Disconnect;
    end;

    ExcelApp.ConnectKind := ckRunningOrNew;
    ExcelApp.Workbooks.Add(EmptyParam, LCID);
    Workbook.ConnectTo(ExcelApp.ActiveWorkbook as ExcelWorkbook);
    Worksheet.ConnectTo(Workbook.Worksheets[1] as ExcelWorkSheet);
    WorkSheet.Activate;
    ExcelApp.DisplayAlerts[LCID] := False;
    WorkSheet.Range['A1', 'A1'].Select;
  finally
    FAppLock.Release;
  end;
  BuildSheetList;
  Visible := AVisible;
end;

function TExcelTh.GetRanges(c, r: OleVariant): OleVariant;
begin
  Result := WorkSheet.Range[c, r].Value;
end;

function TExcelTh.GetIdxs(Idx: OleVariant): OleVariant;
begin
  FAppLock.Acquire;
  try
    Result := WorkSheet.Range[Idx, Idx].Value
  finally
    FAppLock.Release;
  end;
end;

function TExcelTh.GetCells(c, r: OleVariant): OleVariant;
begin
  Result := WorkSheet.Cells.Item[r, c]
end;

function TExcelTh.GetFormulas(c, r: OleVariant): Olevariant;
begin
  Result := WorkSheet.Range[c, r].Formula
end;

procedure TExcelTh.GetSheets(var LCollection: TStrings);
begin
  FSheetLock.Acquire;
  try
    LCollection.Assign(FSheets);
  finally
    FSheetLock.Release;
  end;
end;

function TExcelTh.GetVersion: String;
begin
  FAppLock.Acquire;
  try
    Result := ExcelApp.Version[LCID];
  finally
    FAppLock.Release;
  end;
end;

function TExcelTh.GetVisible: Boolean;
begin
  FAppLock.Acquire;
  try
    Result := ExcelApp.Visible[LCID];
  finally
    FAppLock.Release;
  end;
end;

function TExcelTh.TryLock: Boolean;
begin
  Result := FAppLock.TryEnter;
end;

procedure TExcelTh.BuildSheetList;
var
  i: Integer;
  LSheetName: OleVariant;
begin
  FSheetLock.Acquire;
  try
    FSheets.Clear;
    for i := 1 to WorkBook.Sheets.Count do
    begin
      LSheetName := Workbook.Sheets[i];
      FSheets.Add(LSheetName.Name);
    end;
  finally
    FSheetLock.Release;
  end;
end;

procedure TExcelTh.EndLock;
begin
  FAppLock.Release;
end;

procedure TExcelTh.Execute;
begin
  NameThreadForDebugging('gs2.excelTh');
  while not Terminated do
  begin
    FEVent.WaitFor(INFINITE);
  end;
end;

function TExcelTh.Find(const AWhat: String; var c, r: Integer): Boolean;
var
  LRange: ExcelRange;
  LRange1: OleVariant;
  LRange2: OleVariant;
  LAfter: OleVariant;
  LSearchOrder: XlSearchOrder;
  LSearchDir: XlSearchDirection;
begin
  WorkSheet.Range['A1', 'A1'].EntireRow.Select;
  LRange1 := 'A1';
  LRange2 := WorkSheet.UsedRange[LCID];

  LAfter := ExcelApp.ActiveCell;
  LSearchOrder := xlByRows;
  LSearchDir := xlNext;
  LRange := WorkSheet.Range[LRange1, LRange2].Find(
                             AWhat,        //검색 데이터
                             LAfter,       //검색 시작 셀
                             EmptyParam,   //검색 데이터 유형
                             EmptyParam,   //부분 또는 전체 일치
                             LSearchOrder, //행렬의 방향
                             LSearchDir,   //검색 방향
                             EmptyParam,   //대소 문자 구분
                             EmptyParam ); //최대 전각의 구별
  Result := not VarIsEmpty(LRange) and Assigned(LRange);
  if Result then
  begin
    LRange.Select;
    c := LRange.Column;
    r := LRange.Row;
  end;
end;

procedure TExcelTh.OpenFile(const AFileName: String; const AVisible: Boolean);
var
  LExcel: OleVariant;
begin
  FAppLock.Acquire;
  try
    ExcelApp.ScreenUpdating[LCID] := False;
    if ExcelApp.Visible[LCID] then
    begin
      WorkSheet.Disconnect;
      WorkBook.Close(False, EmptyParam, False, LCID);
      WorkBook.Disconnect;
    end;

    ExcelApp.ConnectKind := ckRunningOrNew;
    LExcel := ExcelApp.Application;
    LExcel.Workbooks.Open(FileName := AFileName);

    Workbook.ConnectTo(ExcelApp.ActiveWorkbook as ExcelWorkbook);
    Worksheet.ConnectTo(Workbook.Worksheets[1] as ExcelWorkSheet);
    WorkSheet.Activate;
    ExcelApp.DisplayAlerts[LCID] := False;
  finally
    FAppLock.Release;
  end;
  Visible := AVisible;
end;

procedure TExcelTh.SetRanges(c, r: OleVariant; const Value: OleVariant);
begin
  WorkSheet.Range[c, r].Value := Value;
end;

procedure TExcelTh.Save;
begin
  ExcelApp.Save;
end;

procedure TExcelTh.SetIdxs(Idx: OleVariant; const Value: OleVariant);
begin
  FAppLock.Acquire;
  try
    WorkSheet.Range[Idx, Idx].Value := Value;
  finally
    FAppLock.Release;
  end;
end;

procedure TExcelTh.SetCells(c, r: OleVariant; const Value: OleVariant);
begin
  WorkSheet.Cells.Item[r, c] := Value;
end;

procedure TExcelTh.SetFormulas(c, r: OleVariant; const Value: Olevariant);
begin
  FAppLock.Acquire;
  try
    WorkSheet.Range[c, r].Formula := Value;
  finally
    FAppLock.Release;
  end;
end;

procedure TExcelTh.SetVisible(const Value: Boolean);
var
  LHandle: THandle;
begin
  FAppLock.Acquire;
  try
    if ExcelApp.Visible[LCID] = Value then
      Exit;

    if Value then
    begin
      LHandle := FindWindow(SWindowName, nil);
      SetWindowPos(LHandle, HWND_TOP, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_SHOWWINDOW);
      ExcelApp.ScreenUpdating[LCID] := True;
    end;
    ExcelApp.Visible[LCID] := Value;
  finally
    FAppLock.Release;
  end;
end;

procedure TExcelTh.TerminatedSet;
begin
  inherited TerminatedSet;

  Close;
  FEvent.SetEvent;
end;

initialization
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

finalization
  CoUninitialize;

end.
