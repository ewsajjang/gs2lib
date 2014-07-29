unit mSQLiteUtils;

interface

uses
  System.Classes, System.SysUtils
  ;

type
  TSQLiteUtil = class
  private
  public
    class function ColumnExists(const ADBPath, ATable, ACol: String): Boolean;
    class function ColumnAdd(const ADBPath, ATable, ACol, AColType: String; IsNull: Boolean = True): Boolean;
    class function TableNames(const ADBPath: String; Contain_sqlite_: Boolean = False): TArray<String>; static;
    class function IndexAdd(const ADBPath, AIdxName, ATableName, AColName: String; AUnique: Boolean = False; AAsc: Boolean = True): Boolean;
  end;

implementation

uses
  Sqlite3, Sqlite3udf, SQLiteWrap
  , System.RegularExpressions

  ;

{ TSqlLiteUtil }

class function TSQLiteUtil.ColumnAdd(const ADBPath, ATable, ACol,
  AColType: String; IsNull: Boolean): Boolean;
var
  DB: TSqliteDatabase;
  LSQL: String;
begin
  DB := TSqliteDatabase.Create(ADBPath);
  try
    LSQL := Format('ALTER TABLE [%s] ADD COLUMN [%s] %s', [ATable, ACol, AColType.ToUpper]);
    try
      DB.ExecSQL(LSQL);
    except on E: Exception do
      Result := False;
    end;
    Result := True;
  finally
    FreeAndNil(DB);
  end;
end;

class function TSQLiteUtil.ColumnExists(const ADBPath, ATable,
  ACol: String): Boolean;
var
  DB: TSqliteDatabase;
  LResult, LRegExp, LSQL: String;
  Table: TSqliteTable;
begin
  DB := TSqliteDatabase.Create(ADBPath);
  try
    LSQL := Format('SELECT * FROM sqlite_master WHERE type="table" and name = "%s"', [ATable]);
    Table := DB.GetTable(LSQL);
    try
      while not Table.EOF do
      begin
        LResult := LResult + #13#10 + Table.FieldAsString(Table.FieldIndex['sql']);
        Table.Next;
      end;
      LRegExp := Format('\[%s\]', [ACol]);
      Result := TRegEx.Match(LResult, LRegExp).Success;
    finally
      FreeAndNil(Table);
    end;
  finally
    FreeAndNil(DB);
  end;
end;

class function TSQLiteUtil.IndexAdd(const ADBPath, AIdxName, ATableName, AColName: String;
  AUnique, AAsc: Boolean): Boolean;
const
  UNIQUE_STR: array[False..True] of String = ('', 'UNIQUE');
  ORDER_DIRECTION: array[False..True] of String = ('DESC', 'ASC');
var
  DB: TSqliteDatabase;
  LSQL: String;
begin
  DB := TSqliteDatabase.Create(ADBPath);
  try
    LSQL := Format('CREATE %s INDEX IF NOT EXISTS [%s] ON [%s] ([%s] %s)',
      [UNIQUE_STR[AUnique], AIdxName, ATableName, AColName, ORDER_DIRECTION[AAsc]]);
    try
      DB.ExecSQL(LSQL);
    except on E: Exception do
      Result := False;
    end;
    Result := True;
  finally
    FreeAndNil(DB);
  end;
end;

class function TSQLiteUtil.TableNames(const ADBPath: String; Contain_sqlite_: Boolean = False): TArray<String>;
const
  system_table = 'sqlite_';
var
  DB: TSqliteDatabase;
  Table: TSqliteTable;
  LName: String;
  LList: TStringList;
begin
  LList := TStringList.Create;
  DB := TSqliteDatabase.Create(ADBPath);
  try
    Table := DB.GetTable('SELECT name FROM sqlite_master');
    try
      while not Table.EOF do
      begin
        LName := Table.FieldAsString(0);
        if Contain_sqlite_ then
          LList.Add(LName)
        else if not LName.Contains(system_table) then
          LList.Add(LName);
        Table.Next;
      end;
      Result := LList.ToStringArray;
    finally
      FreeAndNil(Table);
    end;
  finally
    FreeAndNil(DB);
    FreeAndNil(LList);
  end;
end;

end.