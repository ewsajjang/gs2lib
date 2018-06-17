unit mEvent;

interface

uses
  Classes, SysUtils,
  Generics.Collections, SyncObjs
  ;

type
  TEvent<T> = record
  strict private
    FList: TArray<T>;
    function GetCount: Integer;
  public
    procedure Subscribe(AListener: T);
    procedure Unsubscribe(AListener: T);

    property Listeners: TArray<T> read FList;
    property Count: Integer read GetCount;
  end;

implementation

uses
  Generics.Defaults, Types
  ;

{ TEvent<T> }

function TEvent<T>.GetCount: Integer;
begin
  Result := Length(FList);
end;

procedure TEvent<T>.Subscribe(AListener: T);
begin
  FList := FList + [AListener];
end;

procedure TEvent<T>.Unsubscribe(AListener: T);
var
  i, j: Integer;
  LComparer: IComparer<T>;
begin
  LComparer := TComparer<T>.Default;
  for i := Count -1 downto 0 do
    if LComparer.Compare(FList[i], AListener) = EqualsValue then
    begin
      for j := i to Count -2 do
        FList[j] := FList[j +1];
      SetLength(FList, Count -1);
      Exit;
    end;
end;

end.
