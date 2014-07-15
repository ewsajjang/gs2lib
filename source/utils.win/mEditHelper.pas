unit mEditHelper;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.StdCtrls
  ;

type
  TEditHelper = class helper for TEdit
  private
    procedure SetStrWithCursor(const Value: String);
  public
    function Str: String;
    function StrHint: String;
    function IsEmpty: Boolean;
    procedure EndOfCursor;

    property StrWithCursor: String write SetStrWithCursor;
  end;

implementation

uses
  Winapi.Windows, Winapi.Messages;

{ TEditHelper }

function TEditHelper.StrHint: String;
begin
  Result := TextHint;
end;

procedure TEditHelper.EndOfCursor;
begin
  PostMessage(Handle, WM_KEYDOWN, VK_END, 0);
  PostMessage(Handle, WM_KEYUP, VK_END, 0);
end;

function TEditHelper.IsEmpty: Boolean;
begin
  Result := Str.IsEmpty;
end;

procedure TEditHelper.SetStrWithCursor(const Value: String);
begin
  Text := Value;
  EndOfCursor;
end;

function TEditHelper.Str: String;
begin
  Result := Text;
end;

end.
