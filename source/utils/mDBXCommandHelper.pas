unit mDBXCommandHelper;

interface

uses
  Data.SqlExpr, Data.DB, Data.DBXCommon, Data.SqlTimSt,
  System.Classes, System.SysUtils;

type
  // see http://docwiki.embarcadero.com/RADStudio/XE3/en/DbExpress_Data_Type_Mapping_for_Supported_Databases
  TDBXCommandHelper = class helper for TDBXCommand
  private
  public
    procedure Init;
    procedure AddParam(AType: TDBXType; AValue: String); overload;
    procedure AddParam(AType: TDBXType; AValue: TDateTime); overload;
    procedure AddParam(AType: TDBXType; AValue: Integer); overload;
    procedure AddParam(AType: TDBXType; AValue: Double); overload;
    procedure AddParam(AType: TDBXType; AValue: TStream); overload;
  end;

implementation

{ TDBXCommandHelper }
const
  DBTYPE_CONVERT_ASSERT = 'DataType converter is not ready. plz add handling Code';

procedure TDBXCommandHelper.AddParam(AType: TDBXType; AValue: String);
var
  Param: TDBXParameter;
begin
  Assert(AType in [TDBXDataTypes.WideStringType, TDBXDataTypes.AnsiStringType], DBTYPE_CONVERT_ASSERT);

  Param := CreateParameter;
  Param.DataType := AType;
  case AType of
    TDBXDataTypes.WideStringType: Param.Value.SetWideString(AValue);
    TDBXDataTypes.AnsiStringType: Param.Value.SetAnsiString(AnsiString(AValue));
  end;
  Parameters.AddParameter(Param);
end;

procedure TDBXCommandHelper.AddParam(AType: TDBXType; AValue: TDateTime);
var
  Param: TDBXParameter;
begin
  Assert(AType in [TDBXDataTypes.TimeStampType], DBTYPE_CONVERT_ASSERT);

  Param := CreateParameter;
  Param.DataType := AType;

  Param.Value.SetTimeStamp(DateTimeToSQLTimeStamp(AValue));

  Parameters.AddParameter(Param);
end;

procedure TDBXCommandHelper.AddParam(AType: TDBXType; AValue: Integer);
var
  Param: TDBXParameter;
begin
  with TDBXDataTypes do
    Assert(AType in [Int32Type, Int16Type], DBTYPE_CONVERT_ASSERT);

  Param := CreateParameter;
  Param.DataType := AType;
  case AType of
    TDBXDataTypes.Int32Type: Param.Value.SetInt32(AValue);
    TDBXDataTypes.Int16Type: Param.Value.SetInt16(AValue);
  end;
  Parameters.AddParameter(Param);
end;

procedure TDBXCommandHelper.AddParam(AType: TDBXType; AValue: Double);
var
  Param: TDBXParameter;
begin
  Assert(AType in [TDBXDataTypes.DoubleType], DBTYPE_CONVERT_ASSERT);
  Param := CreateParameter;
  Param.DataType := AType;

  Param.Value.SetDouble(AValue);

  Parameters.AddParameter(Param);
end;

procedure TDBXCommandHelper.AddParam(AType: TDBXType; AValue: TStream);
var
  Param: TDBXParameter;
begin
  Assert(AType in [TDBXDataTypes.BlobType], DBTYPE_CONVERT_ASSERT);
  Param := CreateParameter;
  Param.DataType := AType;
  Param.SubType := TDBXSubDataTypes.BinarySubType;
  Param.Value.SetStream(AValue, False);
  Param.Value.ValueType.ValueTypeFlags := Param.Value.ValueType.ValueTypeFlags or TDBXValueTypeFlags.ExtendedType;
  Parameters.AddParameter(Param);
end;

procedure TDBXCommandHelper.Init;
begin
  Parameters.ClearParameters;
  Close;
  Text := EmptyStr;
end;

end.
