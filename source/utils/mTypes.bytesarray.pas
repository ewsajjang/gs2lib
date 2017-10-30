unit mTypes.bytesarray;

interface

uses
  mTypes,

  System.Classes, System.SysUtils, Spring.Collections
  ;

type
  TToken = record
  private
    FPosition: Integer;
    FLength: Integer;
    function GetIdx: Integer;
  public

    constructor Create(const APosition: Integer);

    procedure Forward(const AStep: Integer = 1);
    procedure Initialize;

    property Idx: Integer read GetIdx;
    property Position: Integer read FPosition;
    property Length: Integer read FLength;
  end;

  TBytesArrayState = class;
  TBytesArrayStateClass = class of TBytesArrayState;

  TBytesEncoder = class
  // ---------------------------------------------------------------------------------------
  // 상태 전이의 책임은 본 TBytesArrayDecoderContext 클래스가 가진다.
  //  즉 ConcreteState(child of the TBytesArrayState)는 다음 상태의 전이를 하지 않는다.
  //    목적. ConcreteState클래스를 다른 Parser/Decoder에서 재활용하고자함.
  //    단점. 각 상태전이에 따른 인스턴스가 생성된기 때문에 성능상의 저하가 올 수 있다.
  // ---------------------------------------------------------------------------------------
  // BNF
  //  <Source>       ::= <ArrayOfBytes> | <Bytes>
  //  <ArrayOfBytes> ::= <SquareBraketBegin> <Bytes> <SquareBracketEnd>
  //  <Bytes>        ::= <SquareBraketBegin> <ArrayItems> <SquareBracketEnd>
  //  <ArrayItems>   ::= <NumberValue> | <Comma> <NumberValue>
  //  <NumberValue>  ::= <HexaDecimalValue> | <DecimalValue>
  // BNF로 부터 다음의 ConcreteState를 추출한다.
  //  TSquareBracketBegin, TSquareBracketEnd
  //  TComma
  //  TNumberValue
  protected
    class function StartWith(const ASrc, ACompare: String; AToken: TToken): Boolean;
  public
    class function Execute(const ASrc: String; out ABuf: TBytes): Boolean; overload;
    class function Execute(const ASrc: String; var AToken: TToken; out ABuf: TBytes): Boolean; overload;
    class function Execute(const ASrc: String): TBytes; overload;
  end;

  TBytesArrayEncoder = class(TBytesEncoder)
  public
    class function Execute(const ASrc: String; out ABuf: TBytesArray): Boolean; overload;
    class function Execute(const ASrc: String): TBytesArray; overload;
  end;

  TBytesArrayState = class
  public
    procedure Execute(const ASrc: String; var AToken: TToken); virtual;
  end;

  TCumsumeSpace = class(TBytesArrayState)
    procedure Execute(const ASrc: String; var AToken: TToken); override;
  end;

  TSquareBracketBegin = class(TBytesArrayState)
    procedure Execute(const ASrc: String; var AToken: TToken); override;
  end;

  TSquareBracketEnd = class(TBytesArrayState)
    procedure Execute(const ASrc: String; var AToken: TToken); override;
  end;

  TNumberValue = class(TBytesArrayState)
    procedure Execute(const ASrc: String; var AToken: TToken); override;
  end;

  TComma = class(TBytesArrayState)
    procedure Execute(const ASrc: String; var AToken: TToken); override;
  end;

  TBytesDecoder = class
    class function Execute(const AValues: TBytes; const AAddCommaSpace: Boolean = True; const AContainsBracket: Boolean = True; const AHexStr: Boolean = True): String;
  end;

  TBytesArrayDecoder = class(TBytesDecoder)
    class function Execute(const AValues: TBytesArray; const AAddCommaSpace: Boolean = True; const AContainsBracket: Boolean = False; const AHexStr: Boolean = True): String;
  end;

implementation

uses
  System.StrUtils
  ;

const
  SOutOfBoundToken = 'The index of Token is out of bound. The index is greater than a source length.';

{ TToken }

constructor TToken.Create(const APosition: Integer);
begin
  FPosition := APosition;
  FLength := 0;
end;

procedure TToken.Forward(const AStep: Integer);
begin
  Inc(FPosition, AStep);
  Inc(FLength, AStep);
end;

function TToken.GetIdx: Integer;
begin
  Result := Position - FLength;
end;

procedure TToken.Initialize;
begin
  FLength := 0;
end;

{ TBytesArrayDecoder }

class function TBytesEncoder.Execute(const ASrc: String; out ABuf: TBytes): Boolean;
var
  t: TToken;
begin
  t := Default(TToken);
  Result := Execute(ASrc, t, ABuf);
end;

class function TBytesEncoder.Execute(const ASrc: String): TBytes;
begin
  Result := [];
  Execute(ASrc, Result);
end;

class function TBytesEncoder.Execute(const ASrc: String; var AToken: TToken; out ABuf: TBytes): Boolean;
var
  s: TBytesArrayState;
  function NewState(const AType: TBytesArrayStateClass): TBytesArrayState;
  begin
    if Assigned(s) then
      FreeAndNil(s);

    s := AType.Create;
    s.Execute(ASrc, AToken);
    Result := s;
  end;
begin
  //  <Bytes>        ::= <SquareBraketBegin> <ArrayItems> <SquareBracketEnd>
  //  <ArrayItems>   ::= <NumberValue> | <Comma> <NumberValue>
  //  <NumberValue>  ::= <HexaDecimalValue> | <DecimalValue>

  if ASrc.IsEmpty then
  begin
    ABuf := [];
    Exit(True);
  end;

  if ASrc.Equals('[]') then
  begin
    ABuf := [];
    Exit(True);
  end;

  s := nil;
  try
    s := NewState(TCumsumeSpace);
    if ASrc.Chars[AToken.Position] = '[' then
    begin
      s := NewState(TSquareBracketBegin);
      s := NewState(TCumsumeSpace);
    end;
    ABuf := [];
    while AToken.Position < ASrc.Length do
    begin
      if CharInSet(ASrc.Chars[AToken.Position], ['$', '0'..'9', 'a'..'f', 'A'..'F']) then
      begin
        s := NewState(TNumberValue);
        ABuf := ABuf + [ASrc.Substring(AToken.Idx, AToken.Length).ToInteger];
        s := NewState(TCumsumeSpace);
      end
      else
        case ASrc.Chars[AToken.Position] of
          ',':
            s := NewState(TComma);

          ']':
          begin
            s := NewState(TSquareBracketEnd);
            break;
          end;

          ' ':
            s := NewState(TCumsumeSpace);

          else
            Exit(False);
        end;
    end;
    Result := True;
  finally
    FreeAndNil(s);
  end;
end;

class function TBytesEncoder.StartWith(const ASrc, ACompare: String; AToken: TToken): Boolean;
begin
  if ASrc.Length < ACompare.Length then
    Exit(False);

  if ASrc.Length < AToken.Position then
    Exit(False);

  Result := ASrc.Substring(AToken.Position).StartsWith(ACompare);
end;

{ TBytesArrayDecoder }

class function TBytesArrayEncoder.Execute(const ASrc: String; out ABuf: TBytesArray): Boolean;
var
  s: TBytesArrayState;
  t: TToken;
  LItem: TBytes;
  function NewState(const AType: TBytesArrayStateClass): TBytesArrayState;
  begin
    if Assigned(s) then
      FreeAndNil(s);

    s := AType.Create;
    s.Execute(ASrc, t);
    Result := s;
  end;
begin
  //  <Source>       ::= <ArrayOfBytes> | <Bytes>
  //  <ArrayOfBytes> ::= <SquareBraketBegin> <Bytes> <SquareBracketEnd>
  //  <Bytes>        ::= <SquareBraketBegin> <ArrayItems> <SquareBracketEnd>
  //  <ArrayItems>   ::= <NumberValue> | <Comma> <NumberValue>
  //  <NumberValue>  ::= <HexaDecimalValue> | <DecimalValue>

  if ASrc.IsEmpty then
  begin
    ABuf := [];
    Exit(True);
  end;

  if ASrc.Equals('[]') then
  begin
    ABuf := [];
    Exit(True);
  end;

  t := Default(TToken);
  s := nil;
  s := NewState(TCumsumeSpace);
  try
    if StartWith(ASrc, '[[', t) then
      s := NewState(TSquareBracketBegin);

    if not StartWith(ASrc, '[', t) then
      raise EArgumentException.Create('Invalid argument!!');

    while t.Position < ASrc.Length do
    begin
      s := NewState(TCumsumeSpace);
      if Execute(ASrc, t, LItem) then
        ABuf := ABuf + [LItem];

      case ASrc.Chars[t.Position] of
          ',':
          begin
            s := NewState(TComma);
            Continue;
          end;

          ']':
            s := NewState(TSquareBracketEnd);

      end;
      s := NewState(TCumsumeSpace);
    end;
    Result := True;
  finally
    FreeAndNil(s);
  end;
end;

class function TBytesArrayEncoder.Execute(const ASrc: String): TBytesArray;
begin
  Result := [];
  Execute(ASrc, Result);
end;

{ TBytesArrayState }

procedure TBytesArrayState.Execute(const ASrc: String; var AToken: TToken);
begin
  if AToken.Position >= ASrc.Length then
    raise EArgumentException.Create(SOutOfBoundToken);

  AToken.Initialize;
end;

{ TCumsumeSpace }

procedure TCumsumeSpace.Execute(const ASrc: String; var AToken: TToken);
begin
  if AToken.Position > ASrc.Length then
    raise EInvalidOperation.Create(SOutOfBoundToken);

  AToken.Initialize;
  while AToken.Position < ASrc.Length do
    case ASrc.Chars[AToken.Position] of
      ' ',
      #13,
      #10:
        AToken.Forward;
      else
        Break;
    end;
end;

{ TSquareBraket }

procedure TSquareBracketBegin.Execute(const ASrc: String; var AToken: TToken);
begin
  inherited ;

  if ASrc.Chars[AToken.Position] = '[' then
    AToken.Forward
  else
    raise EArgumentException.Create('Cannot find the square-bracket for the begin.');
end;

{ TSquareBracketArrayEnd }

procedure TSquareBracketEnd.Execute(const ASrc: String; var AToken: TToken);
begin
  inherited ;

  if ASrc.Chars[AToken.Position] = ']' then
    AToken.Forward
  else
    raise EArgumentException.Create('Cannot find the square-bracket for the end.');
end;

{ TArrayItem }

procedure TNumberValue.Execute(const ASrc: String; var AToken: TToken);
var
  LHexLen: Integer;
begin
  inherited;

  while AToken.Position < ASrc.Length do
    case ASrc.Chars[AToken.Position] of
      '$':
      begin
        AToken.Forward;
        LHexLen := 1;
        while AToken.Position < ASrc.Length do
          if CharInSet(ASrc.Chars[AToken.Position], ['0'..'9', 'a'..'f', 'A'..'F']) then
          begin
            AToken.Forward;
            Inc(LHexLen);
          end
          else
          begin
            if LHexLen = 1 then
              raise EArgumentException.Create('Cannot find the source that is a hexadecimal character which will be used for an item of the array.');

            Exit;
          end;
      end;

      '0'..'9':
        while AToken.Position < ASrc.Length do
          if CharInSet(ASrc.Chars[AToken.Position], ['0'..'9']) then
            AToken.Forward
          else
            Exit;
      else
        raise EArgumentException.Create('Cannot find any character for the TNumberValue');
    end;
end;

{ TComma }

procedure TComma.Execute(const ASrc: String; var AToken: TToken);
begin
  inherited ;

  if ASrc.Chars[AToken.Position] = ',' then
    AToken.Forward
  else
    raise EArgumentException.Create('Cannot find any character for the ' + ClassName);
end;

{ TBytesEncoder }

class function TBytesDecoder.Execute(const AValues: TBytes; const AAddCommaSpace: Boolean; const AContainsBracket: Boolean;
  const AHexStr: Boolean): String;
var
  i: Integer;
  function ConcatTail: String;
  begin
    Result := IfThen(i < High(AValues), ',' +IfThen(AAddCommaSpace, ' '))
  end;
begin
  Result := '';
  for i := Low(AValues) to High(AValues) do
    if AHexStr then
      Result := Result + '$' +AValues[i].ToHexString(2) +ConcatTail
    else
      Result := Result + AValues[i].ToString +ConcatTail;

  if AContainsBracket then
    Result := '[' + Result + ']';
end;

{ TBytesArrayEncoder }

class function TBytesArrayDecoder.Execute(const AValues: TBytesArray; const AAddCommaSpace: Boolean;
  const AContainsBracket: Boolean; const AHexStr: Boolean): String;
var
  LBuf: TStringList;
  i: Integer;
begin
  LBuf := TStringList.Create;
  try
    for i := Low(AValues) to High(AValues) do
      Result := Result + inherited Execute(AValues[i], AAddCommaSpace, True, AHexStr) + IfThen(i < High(AValues), ',' +IfThen(AAddCommaSpace, ' '));

    if AContainsBracket then
      Result := '[' + Result + ']';
  finally
    FreeAndNil(LBuf);
  end;
end;

end.
