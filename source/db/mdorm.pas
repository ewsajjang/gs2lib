unit mdorm;

interface

uses
	mConsts,
  dorm,
  dorm.Mappings,
  dorm.Commons,
  dorm.ObjectStatus,

  System.SysUtils, System.Classes,
  System.Generics.Collections;

const
  SQLITE_BOOL: array[False..True] of Integer = (0, 1);

type
	TSQLiteBoolStr = (sbNull, sbFalse, sbTrue);
  TSQLiteBoolStrHelper = record helper for TSQLiteBoolStr
  	class function New(const AValue: String): TSQLiteBoolStr; overload; static;
  	class function New(const AFalseCondition, ATrueCondition: Boolean): TSQLiteBoolStr; overload; static;
    function Str: String;
  end;
const
  SQLITE_BSTR: array[sbNull .. sbTrue] of String = ('', FALSE_STR, TRUE_STR);

const
  SQLITE_IDX_START        = 1;
  SQLITE_IDX_NOT_ASSIGNED = 0;

type
  TDBModel = class
  private
  protected
    FId: Integer;
    FCreatedAt: String;
    FReserved: String;
    FObjStatus: TdormObjectStatus;
  public
    property ID: Integer read FId write FId;
    property CreatedAt: String read FCreatedAt write FCreatedAt;
    property Reserved: String read FReserved write FReserved;

    [Transient]
    property ObjStatus: TdormObjectStatus read FObjStatus write FObjStatus;
  end;

  Tmdorm = class
  private
  protected
    FSession: TSession;

    procedure Open;
    procedure Close;
    procedure ColumnAppend(const ADBName, ATable, AColName, AColType: String);
    procedure IndexAppend(const ADBName, AIdxName, ATableName, AColName: String; AUnique: Boolean = False; AAsc: Boolean = True);
    procedure LogFileDelete(AFileName: String = '');
  public
  	procedure Persist(AValue: TDBModel);
    procedure Update(AValue: TDBModel);
    procedure Delete(AValue: TDBModel);
    procedure Insert(AValue: TDBModel);
    procedure UpdateList(AList: TObject);

    function Load<T: class>(const ADBId: Integer; ALoadRelations: Boolean = False): T;
    function LoadList<T: class>(ALoadRelations: Boolean = False): TObjectList<T>; overload;
    procedure LoadList<T: class>(AList: TObjectList<T>; ALoadRelations: Boolean = False); overload;
  end;

implementation

uses
  mSQLiteUtils, mParams,
  System.IOUtils, System.TypInfo, System.Types;

{ Tmdorm }

procedure Tmdorm.Close;
begin
  if Assigned(FSession) then
  begin
    FSession.Commit;
    FreeAndNil(FSession);
  end;
end;

procedure Tmdorm.ColumnAppend(const ADBName, ATable, AColName, AColType: String);
begin
  if not TSQLiteUtil.ColumnExists(ADBName, ATable, AColName) then
    TSQLiteUtil.ColumnAdd(ADBName, ATable, AColName, AColType);
end;

procedure Tmdorm.Delete(AValue: TDBModel);
begin
  Open;
  try
    AValue.ObjStatus := osDeleted;
    FSession.Persist(AValue);
  finally
    Close;
  end;
end;

procedure Tmdorm.IndexAppend(const ADBName, AIdxName, ATableName, AColName: String;
  AUnique, AAsc: Boolean);
begin
  TSQLiteUtil.IndexAdd(ADBName, AIdxName, ATableName, AColName, AAsc, AUnique);
end;

procedure Tmdorm.Insert(AValue: TDBModel);
begin
  Open;
  try
    FSession.Insert(AValue);
  finally
    Close;
  end;
end;

function Tmdorm.Load<T>(const ADBId: Integer; ALoadRelations: Boolean): T;
begin
  Open;
  try
    Result := FSession.Load<T>(ADBId);
    if ALoadRelations then
      FSession.LoadRelations(Result);
  finally
    Close;
  end;
end;

procedure Tmdorm.LoadList<T>(AList: TObjectList<T>; ALoadRelations: Boolean);
begin
  Open;
  try
    FSession.LoadList<T>(nil, AList);
    if ALoadRelations then
      FSession.LoadRelationsForEachElement(AList);
  finally
    Close;
  end;
end;

function Tmdorm.LoadList<T>(ALoadRelations: Boolean): TObjectList<T>;
begin
  Open;
  try
    Result := FSession.LoadList<T>;
    if ALoadRelations then
      FSession.LoadRelationsForEachElement(Result);
  finally
    Close;
  end;
end;

procedure Tmdorm.LogFileDelete(AFileName: String);
var
  LFiles: TStringDynArray;
  LFile: String;
begin
  LFiles := TDirectory.GetFiles(TParams.ExePath, '*.log', TSearchOption.soTopDirectoryOnly);
  for LFile in LFiles do
    if FileExists(LFile) then
      TFile.Delete(LFile);
end;

procedure Tmdorm.Open;
begin
  FSession := TSession.CreateConfigured(
    TStreamReader.Create('db.conf')
    , TStreamReader.Create('db.mapping')
    ,
      {$IFDEF DEBUG}
        deTest
      {$ELSE}
        deRelease
      {$ENDIF}
    );
  FSession.StartTransaction;
end;

procedure Tmdorm.Persist(AValue: TDBModel);
begin
  Open;
  try
    FSession.Persist(AValue);
  finally
    Close;
  end;
end;

procedure Tmdorm.Update(AValue: TDBModel);
begin
  Open;
  try
    AValue.ObjStatus := osDirty;
    FSession.Persist(AValue);
  finally
    Close;
  end;
end;

procedure Tmdorm.UpdateList(AList: TObject);
begin
  Open;
  try
    FSession.PersistCollection(AList)
  finally
    Close;
  end;
end;

{ TSQLiteBoolStrHelper }

class function TSQLiteBoolStrHelper.New(const AValue: String): TSQLiteBoolStr;
var
	LItem: TSqliteBoolStr;
begin
	Result := sbNull;
	for LItem := Low(TSqliteBoolStr) to High(TSqliteBoolStr) do
  	if LItem.Str.Contains(AValue) then
    begin
      Result := LItem;
      Break;
    end;
end;

class function TSQLiteBoolStrHelper.New(const AFalseCondition,
  ATrueCondition: Boolean): TSQLiteBoolStr;
begin
	if not AFalseCondition and not ATrueCondition then
  	Result := sbNull
  else if ATrueCondition then
  	Result := sbTrue
  else
  	Result := sbFalse
end;

function TSQLiteBoolStrHelper.Str: String;
begin
	Result := SQLITE_BSTR[Self];
end;

end.
