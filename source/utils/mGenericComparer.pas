unit mGenericComparer;

interface

uses
  System.Classes, System.SysUtils,
  System.Generics.Defaults, System.Generics.Collections;

type
  TExtendedComparer = class(TComparer<Extended>)
    function Compare(const Left, Right: Extended): Integer; override;
  end;
  TDoubleComparer = class(TComparer<Double>)
    function Compare(const Left, Right: Double): Integer; override;
  end;
  TSingleComparer = class(TComparer<Single>)
    function Compare(const Left, Right: Single): Integer; override;
  end;
  TIntegerComparer = class(TComparer<Integer>)
    function Compare(const Left, Right: Integer): Integer; override;
  end;
  TInt64Comparer = class(TComparer<Int64>)
    function Compare(const Left, Right: Int64): Integer; override;
  end;
  TUInt64Comparer = class(TComparer<UInt64>)
    function Compare(const Left, Right: UInt64): Integer; override;
  end;

implementation

uses
  System.Math, System.Types;

{ TExtendedComparer }

function TExtendedComparer.Compare(const Left, Right: Extended): Integer;
begin
  Result := CompareValue(Left, Right)
end;

{ TDoubleComparer }

function TDoubleComparer.Compare(const Left, Right: Double): Integer;
begin
  Result := CompareValue(Left, Right)
end;

{ TSingleComparer }

function TSingleComparer.Compare(const Left, Right: Single): Integer;
begin
  Result := CompareValue(Left, Right)
end;

{ TIntegerComparer }

function TIntegerComparer.Compare(const Left, Right: Integer): Integer;
begin
  Result := CompareValue(Left, Right)
end;

{ TInt64Comparer }

function TInt64Comparer.Compare(const Left, Right: Int64): Integer;
begin
  Result := CompareValue(Left, Right)
end;

{ TUInt64Comparer }

function TUInt64Comparer.Compare(const Left, Right: UInt64): Integer;
begin
  Result := CompareValue(Left, Right)
end;

end.
