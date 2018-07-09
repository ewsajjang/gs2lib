unit mMemoHelper;

interface

uses
  StdCtrls, Windows,
  SysUtils
  ;

type
  TMemoHelper = class helper for TMemo
    procedure InsertCaretPos(const AValue: String);
    procedure MoveCaret(const x, y: Integer);
    procedure InsertCurrentCarret(const AValue: string);
    procedure MoveCaretLineEnd;
  end;

implementation

{ TMemoHelper }

procedure TMemoHelper.InsertCurrentCarret(const AValue: string);
var
  LPos: TPoint;
  LLine: string;
begin
  LPos := CaretPos;
  LLine := Lines[LPos.Y];
  Lines[LPos.Y] := LLine.Insert(LPos.X, AValue);
end;

procedure TMemoHelper.InsertCaretPos(const AValue: String);
var
  LPos: TPoint;
  LLine: String;
begin
  LPos := CaretPos;
  LLine := Lines[LPos.Y];
  Lines[LPos.Y] := LLine.Substring(0, LPos.X) + AValue + LLine.Substring(LPos.X);
  LPos.Offset(AValue.Length, 0);
  CaretPos := LPos;
end;

procedure TMemoHelper.MoveCaret(const x, y: Integer);
var
  LPos: TPoint;
begin
  LPos := CaretPos;
  LPos.Offset(x, y);
  CaretPos := LPos;
end;

procedure TMemoHelper.MoveCaretLineEnd;
var
  LPos: TPoint;
begin
  LPos := CaretPos;
  LPos.X := Lines[LPos.Y].Length;
  CaretPos := LPos;
end;

end.
