unit mdorm;

interface

uses
  dorm,
  dorm.Mappings,
  dorm.Commons,
  dorm.ObjectStatus,

  System.SysUtils, System.Classes,
  System.Generics.Collections;

const
  SQLITE_BOOL: array[False..True] of Integer = (0, 1);
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
    procedure LogFileDelete(AFileName: String = '');
  public
    procedure Update(AValue: TDBModel);
    procedure Delete(AValue: TDBModel);

    function Load<T: class>(const ADBId: Integer; ALoadRelations: Boolean = False): T;
    function LoadList<T: class>(ALoadRelations: Boolean = False): TObjectList<T>;
  end;

implementation

uses
  mSQLiteUtils, mParams,
  System.IOUtils;

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
//const
//  LOG_FILE_NAME = 'GlucoNaviiDMS_dormFileLog.log';
var
  LFileName: String;
begin
  if AFileName.IsEmpty then
    LFileName := TParams.ExePath + TPath.GetFileNameWithoutExtension(TParams.ExeName) + '_dormFileLog.log';

  if FileExists(LFileName) then
    TFile.Delete(LFileName);
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

end.
