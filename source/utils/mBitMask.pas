unit mBitMask;

interface

uses
  System.SysUtils, System.Classes,
  System.Generics.Collections;

type
  EBitFlag = class(Exception);
    ENotSupportedType = class(EBitFlag);

  TBitMask<T> = record
  private
    FSize: Integer;
    FValue: T;
    function GetB(Index: Integer): Boolean;
  public
    procedure Load(const AValue: T);

    property B[Index: Integer]: Boolean read GetB;
    property Size: Integer read FSize;
  end;

implementation

uses
  System.Rtti, System.TypInfo;

{ TBitFlag<T> }

function TBitMask<T>.GetB(Index: Integer): Boolean;
begin

end;

procedure TBitMask<T>.Load(const AValue: T);
var
  LValue: TValue;
  LSupported: Boolean;
  LIsClass: Boolean;
begin
  LValue := TValue.From<T>(AValue);
  LIsClass := LValue.IsClass;
  if not LIsClass then
    LSupported :=
      LValue.IsType<Integer  > or
      LValue.IsType<Cardinal > or
      LValue.IsType<ShortInt > or
      LValue.IsType<SmallInt > or
      LValue.IsType<LongInt  > or
      LValue.IsType<Int64    > or
      LValue.IsType<Byte     > or
      LValue.IsType<Word     > or
      LValue.IsType<LongWord > or
      LValue.IsType<UInt64   > ;
  if not LSupported or LIsClass then
    raise ENotSupportedType.CreateFmt('TBitFlag class can not created by %s', [LValue.TypeInfo.Name]);

  FValue := AValue;
  FSize := SizeOf(FValue);
end;

end.
