unit mValueListEditorHelper;

interface

uses
  Vcl.Grids, Vcl.ValEdit,
  System.Classes, System.SysUtils
  ;

type
  TValueListEditorHelper = class helper for TValueListEditor
  private
    function GetItemValue: String;
    procedure SetItemValue(const Value: String);
    function GetItemKeys(Index: Integer): String;
    function GetItemValues(Index: Integer): String;
    function GetItemCount: Integer;
  public
    function CalcRow(const ARow: Integer): Integer;
    function ItemRow: Integer;
    function ItemKey: String;

    property ItemValue: String read GetItemValue write SetItemValue;
    property ItemCount: Integer read GetItemCount;
    property ItemKeys[Index: Integer]: String read GetItemKeys;
    property ItemValues[Index: Integer]: String read GetItemValues;
  end;

implementation

uses
  System.Math
  ;

{ TValueListEditorHelper }

function TValueListEditorHelper.ItemRow: Integer;
begin
  Result := Row -IfThen(doColumnTitles in DisplayOptions, 1);
end;

function TValueListEditorHelper.GetItemCount: Integer;
begin
  Result := Strings.Count;
end;

function TValueListEditorHelper.GetItemKeys(Index: Integer): String;
begin
  Result := Cells[0, Index +IfThen(doColumnTitles in DisplayOptions, 1)];
end;

function TValueListEditorHelper.GetItemValues(Index: Integer): String;
begin
  Result := Cells[1, Index +IfThen(doColumnTitles in DisplayOptions, 1)];
end;

function TValueListEditorHelper.ItemKey: String;
begin
  Result := '';
  if ItemRow > -1 then
    Result := Strings.Names[ItemRow];
end;

procedure TValueListEditorHelper.SetItemValue(const Value: String);
begin
  if ItemRow > -1 then
    Strings.ValueFromIndex[ItemRow] := Value;
end;

function TValueListEditorHelper.CalcRow(const ARow: Integer): Integer;
begin
  Result := ARow - IfThen(doColumnTitles in DisplayOptions, 1)
end;

function TValueListEditorHelper.GetItemValue: String;
begin
  Result := '';
  if ItemRow > -1 then
    Result := Strings.ValueFromIndex[ItemRow];
end;

end.
