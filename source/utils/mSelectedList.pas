unit mSelectedList;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  ESelectedList = class(Exception);

  TSelectedList<T> = class(TList<T>)
  private
    FSelectedIdx: Integer;
    FOnSelected: TProc<T>;
    function GetSelectedItem: T;
    function GetSelected: Boolean;
  public
    constructor Create; overload;

    procedure Clear;
    procedure Select(const AIndex: Integer);

    property SelectedItem: T read GetSelectedItem;
    property SelectedIdx: Integer read FSelectedIdx;
    property Selected: Boolean read GetSelected;

    property OnSelected: TProc<T> read FOnSelected write FOnSelected;
  end;

implementation

{ TSelectedList<T: class> }

procedure TSelectedList<T>.Clear;
begin
  inherited Clear;

  FSelectedIdx := -1;
end;

constructor TSelectedList<T>.Create;
begin
  inherited Create;

  FSelectedIdx := -1;
end;

function TSelectedList<T>.GetSelected: Boolean;
begin
  Result := FSelectedIdx > -1;
end;

function TSelectedList<T>.GetSelectedItem: T;
begin
  if FSelectedIdx > -1 then
    Result := Items[FSelectedIdx]
  else
    raise ESelectedList.Create('Can not find selected item index.');
end;

procedure TSelectedList<T>.Select(const AIndex: Integer);
begin
  FSelectedIdx := AIndex;
  if Assigned(FOnSelected) then
    FOnSelected(Items[FSelectedIdx]);
end;

end.