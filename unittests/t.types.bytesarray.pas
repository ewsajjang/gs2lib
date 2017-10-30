unit t.types.bytesarray;

interface

uses
  mTypes, mTypes.bytesarray,

  System.Classes, System.SysUtils,
  DUnitX.TestFramework
  ;

type
  [TestFixture]
  TBytesArrayStateTest = class
  protected
    FToken: TToken;
  public
    [Setup] procedure Setup; virtual;
  end;

  TCumsumeSpaceTest = class(TBytesArrayStateTest)
  private
    FState: TCumsumeSpace;
  public
    [Setup] procedure Setup; override;
    [TearDown] procedure TearDown;

    [Test] procedure TestSpaceOnlyExecute;
    [Test] procedure TestEmptyStr;
    [Test] procedure TestSingleChar;
  end;

  TSquareBracketBeginTest = class(TBytesArrayStateTest)
  private
    FState: TSquareBracketBegin;
  public
    [Setup] procedure Setup; override;
    [TearDown] procedure TearDown;

    [Test] procedure TestExecute;
    [Test] procedure TestNoneBracket;
    [Test] procedure TestEmptyStr;
  end;

  TSquareBracketEndTest = class(TBytesArrayStateTest)
  private
    FState: TSquareBracketEnd;
  public
    [Setup] procedure Setup; override;
    [TearDown] procedure TearDown;

    [Test] procedure TestExecute;
    [Test] procedure TestNoneBracket;
    [Test] procedure TestEmptyStr;
  end;

  TNumberValueTest = class(TBytesArrayStateTest)
  private
    FState: TNumberValue;
  public
    [Setup] procedure Setup; override;
    [TearDown] procedure TearDown;

    [Test] procedure TestEmptyStr;
    [Test] procedure TestHexUpper;
    [Test] procedure TestHexUpperSpace;
    [Test] procedure TestHexLower;
    [Test] procedure TestHexLowerSpace;
    [Test] procedure TestDec;
  end;

  TCommaTest = class(TBytesArrayStateTest)
  private
    FState: TComma;
  public
    [Setup] procedure Setup; override;
    [TearDown] procedure TearDown;

    [Test] procedure TestEmptyStr;
    [Test] procedure TestExecute;
    [Test] procedure TestNoneComma;
  end;

  TBytesEncoderTest = class
  public
    [Test] procedure TestNormal;
    [Test] procedure TestSpaceArray;
    [Test] procedure TestNoSpaceArray;
    [Test] procedure TestInvalidHexStrArray;
    [Test] procedure TestInvalidStrArray;
    [Test] procedure TestNoBracketStr;
    [Test] procedure TestNoBracketStrWithSpace;
  end;

  TBytesArrayEncoderTest = class
  public
    [Test] procedure TestNormal;
    [Test] procedure TestSpaceArray;
    [Test] procedure TestNoSpaceArray;
    [Test] procedure TestNoBytesArrayBracket;
    [Test] procedure TestInvalidHexStrArray;
    [Test] procedure TestSingleBytesArray;
    [Test] procedure TestInvalidBytesArrayBracket;
  end;

  TBytesDecoderTest = class
  private
  public
    [Test] procedure TestBracketOnly;
    [Test] procedure TestBracketAndSpace;
    [Test] procedure TestSpaceOnly;
    [Test] procedure TestDecimal;
  end;

  TBytesArrayDecoderTest = class
  public
    [Test] procedure TestBraketOnly;
    [Test] procedure TestBracketAndSpace;
    [Test] procedure TestSpaceOnly;
    [Test] procedure TestDecimal;
  end;

implementation

{ TBytesArrayStateTest }

procedure TBytesArrayStateTest.Setup;
begin
  FToken := Default(TToken);
end;

procedure TCumsumeSpaceTest.Setup;
begin
  FState := TCumsumeSpace.Create;
  FToken := Default(TToken);
end;

procedure TCumsumeSpaceTest.TearDown;
begin
  FreeAndNil(FState);
end;

procedure TCumsumeSpaceTest.TestEmptyStr;
begin
  Assert.WillNotRaiseAny(procedure
    begin
      FState.Execute('', FToken);
      Assert.AreEqual(0, FToken.Position);
      Assert.AreEqual(0, FToken.Length);
      Assert.AreEqual(0, FToken.Idx);
    end);
end;

procedure TCumsumeSpaceTest.TestSingleChar;
begin
  FState.Execute('a', FToken);
  assert.AreEqual(0, FToken.Position);
  assert.AreEqual(0, FToken.Length);
  Assert.AreEqual(0, FToken.Idx);
end;

procedure TCumsumeSpaceTest.TestSpaceOnlyExecute;
begin
  FState.Execute('    ', FToken);
  assert.AreEqual(4, FToken.Position);
  assert.AreEqual(4, FToken.Length);
  Assert.AreEqual(0, FToken.Idx);
end;

{ TSquareBracketArrayBeginTest }

procedure TSquareBracketBeginTest.Setup;
begin
  FState := TSquareBracketBegin.Create;
end;

procedure TSquareBracketBeginTest.TearDown;
begin
  FreeAndNil(FState);
end;

procedure TSquareBracketBeginTest.TestEmptyStr;
begin
  Assert.WillRaise(procedure
    begin FState.Execute('', FToken); end);
end;

procedure TSquareBracketBeginTest.TestExecute;
begin
  FState.Execute('[', FToken);
  Assert.AreEqual(1, FToken.Position);
  Assert.AreEqual(1, FToken.Length);
  Assert.AreEqual(0, FToken.Idx);
end;

procedure TSquareBracketBeginTest.TestNoneBracket;
begin
  Assert.WillRaise(procedure
    begin FState.Execute(']', FToken); end);
end;

{ TSquareBracketArrayEndTest }

procedure TSquareBracketEndTest.Setup;
begin
  FState := TSquareBracketEnd.Create;
end;

procedure TSquareBracketEndTest.TearDown;
begin
  FreeAndNil(FState);
end;

procedure TSquareBracketEndTest.TestEmptyStr;
begin
  Assert.WillRaise(procedure
    begin FState.Execute('', FToken); end);
end;

procedure TSquareBracketEndTest.TestExecute;
begin
  FState.Execute(']]', FToken);
  Assert.AreEqual(1, FToken.Position);
  Assert.AreEqual(1, FToken.Length);
  Assert.AreEqual(0, FToken.Idx);
end;

procedure TSquareBracketEndTest.TestNoneBracket;
begin
  Assert.WillRaise(procedure
    begin FState.Execute('a', FToken); end);
end;

{ TNumberValueTest }

procedure TNumberValueTest.Setup;
begin
  inherited;

  FState := TNumberValue.Create;
end;

procedure TNumberValueTest.TearDown;
begin
  FreeAndNil(FState);
end;

procedure TNumberValueTest.TestDec;
begin
  FState.Execute('99', FToken);
  Assert.AreEqual(2, FToken.Length);
  Assert.AreEqual(2, FToken.Position);
  Assert.AreEqual(0, FToken.Idx);

  FToken := Default(TToken);
  FState.Execute('0', FToken);
  Assert.AreEqual(1, FToken.Length);
  Assert.AreEqual(1, FToken.Position);
  Assert.AreEqual(0, FToken.Idx);
end;

procedure TNumberValueTest.TestEmptyStr;
begin
  Assert.WillRaise(procedure
    begin FState.Execute('', FToken); end);
end;

procedure TNumberValueTest.TestHexLower;
begin
  FState.Execute('$ff', FToken);
  Assert.AreEqual(3, FToken.Length);
  Assert.AreEqual(3, FToken.Position);
  Assert.AreEqual(0, FToken.Idx);
end;

procedure TNumberValueTest.TestHexLowerSpace;
begin
  FState.Execute('$ff ', FToken);
  Assert.AreEqual(3, FToken.Length);
  Assert.AreEqual(3, FToken.Position);
  Assert.AreEqual(0, FToken.Idx);
end;

procedure TNumberValueTest.TestHexUpper;
begin
  FState.Execute('$FF', FToken);
  Assert.AreEqual(3, FToken.Length);
  Assert.AreEqual(3, FToken.Position);
  Assert.AreEqual(0, FToken.Idx);
end;

procedure TNumberValueTest.TestHexUpperSpace;
begin
  FState.Execute('$FF ', FToken);
  Assert.AreEqual(3, FToken.Length);
  Assert.AreEqual(3, FToken.Position);
  Assert.AreEqual(0, FToken.Idx);
end;

{ TCommaTest }

procedure TCommaTest.Setup;
begin
  inherited;

  FState := TComma.Create;
end;

procedure TCommaTest.TearDown;
begin
  FreeAndNil(FState);
end;

procedure TCommaTest.TestEmptyStr;
begin
  Assert.WillRaise(procedure
    begin FState.Execute('', FToken); end);
end;

procedure TCommaTest.TestExecute;
begin
  FState.Execute(',', FToken);
  Assert.AreEqual(1, FToken.Length);
  Assert.AreEqual(1, FToken.Position);
  Assert.AreEqual(0, FToken.Idx);
end;

procedure TCommaTest.TestNoneComma;
begin
  Assert.WillRaise(procedure
    begin FState.Execute('d', FToken); end);
end;

{ TBytesDecoderTest }

procedure TBytesEncoderTest.TestInvalidHexStrArray;
var
  LBuf: TBytes;
begin
  Assert.WillRaise(procedure
    begin Assert.IsFalse(TBytesEncoder.Execute('[00, $ 01]', LBuf)); end);
end;

procedure TBytesEncoderTest.TestInvalidStrArray;
var
  LBuf: TBytes;
begin
  Assert.WillRaise(procedure
    begin Assert.IsFalse(TBytesEncoder.Execute('[0 0, $01]', LBuf)); end);
end;

procedure TBytesEncoderTest.TestNoBracketStr;
var
  LBuf: TBytes;
begin
  Assert.IsTrue(TBytesEncoder.Execute('99, $FF', LBuf));
  Assert.AreEqual<TBytes>([99, $FF], LBuf);
  Assert.AreEqual<TBytes>([99, $FF], TBytesEncoder.Execute('99, $FF'));
end;

procedure TBytesEncoderTest.TestNoBracketStrWithSpace;
var
  LBuf: TBytes;
begin
  Assert.IsTrue(TBytesEncoder.Execute(' 99 , $FF ', LBuf));
  Assert.AreEqual<TBytes>([99, $FF], LBuf);
  Assert.AreEqual<TBytes>([99, $FF], TBytesEncoder.Execute(' 99 , $FF '));
end;

procedure TBytesEncoderTest.TestNormal;
var
  LBuf: TBytes;
begin
  Assert.IsTrue(TBytesEncoder.Execute('[00, $01]', LBuf));
  Assert.AreEqual<TBytes>([00, 01], LBuf);
  Assert.AreEqual<TBytes>([00, 01], TBytesEncoder.Execute('[00, $01]'));
end;

procedure TBytesEncoderTest.TestNoSpaceArray;
var
  LBuf: TBytes;
begin
  Assert.IsTrue(TBytesEncoder.Execute(' [$00,01]', LBuf));
  Assert.AreEqual<TBytes>([00, 01], LBuf);
  Assert.AreEqual<TBytes>([00, 01], TBytesEncoder.Execute(' [$00,01]'));
end;

procedure TBytesEncoderTest.TestSpaceArray;
var
  LBuf: TBytes;
begin
  Assert.IsTrue(TBytesEncoder.Execute(' [ $00 , 01 ] ', LBuf));
  Assert.AreEqual<TBytes>([00, 01], LBuf);
  Assert.AreEqual<TBytes>([00, 01], TBytesEncoder.Execute(' [ $00 , 01 ] '));
end;

{ TBytesArrayDecoderTest }

procedure TBytesArrayEncoderTest.TestInvalidBytesArrayBracket;
var
  LBuf: TBytesArray;
begin
  Assert.WillRaise(procedure
    begin TBytesArrayEncoder.Execute('00, $01], 254, $FF]', LBuf); end);
end;

procedure TBytesArrayEncoderTest.TestInvalidHexStrArray;
var
  LBuf: TBytesArray;
begin
  Assert.WillRaise(procedure
    begin TBytesArrayEncoder.Execute('[[00, $ 01, [254, $FF', LBuf); end);
end;

procedure TBytesArrayEncoderTest.TestSingleBytesArray;
var
  LExpect, LBuf: TBytesArray;
  i: Integer;
begin
  LExpect := TBytesArray.Create(TBytes.Create(00, $01, 254, $FF));
  Assert.IsTrue(TBytesArrayEncoder.Execute('[00, $01, 254, $FF]', LBuf));
  for i := 0 to Length(LBuf) -1 do
    Assert.AreEqual<TBytes>(LExpect[0], LBuf[0], i.ToString + 'is not equals');
end;

procedure TBytesArrayEncoderTest.TestNoBytesArrayBracket;
var
  LExpect, LBuf: TBytesArray;
  i: Integer;
begin
  LExpect := TBytesArray.Create(TBytes.Create(00, $01), TBytes.Create(254, $FF));
  Assert.IsTrue(TBytesArrayEncoder.Execute('[00, $01], [254, $FF]', LBuf));
  for i := 0 to Length(LBuf) -1 do
    Assert.AreEqual<TBytes>(LExpect[0], LBuf[0], i.ToString + 'is not equals');
end;

procedure TBytesArrayEncoderTest.TestNormal;
var
  LExpect, LBuf: TBytesArray;
  i: Integer;
begin
  LExpect := TBytesArray.Create(TBytes.Create(00, $01), TBytes.Create(254, $FF));
  Assert.IsTrue(TBytesArrayEncoder.Execute('[[00, $01], [254, $FF]]', LBuf));
  for i := 0 to Length(LBuf) -1 do
    Assert.AreEqual<TBytes>(LExpect[0], LBuf[0], i.ToString + 'is not equals');
end;

procedure TBytesArrayEncoderTest.TestNoSpaceArray;
var
  LExpect, LBuf: TBytesArray;
  i: Integer;
begin
  LExpect := TBytesArray.Create(TBytes.Create(00, $01), TBytes.Create(254, $FF));
  Assert.IsTrue(TBytesArrayEncoder.Execute('[[00,$01],[254,$FF]]', LBuf));
  for i := 0 to Length(LBuf) -1 do
    Assert.AreEqual<TBytes>(LExpect[0], LBuf[0], i.ToString + 'is not equals');
end;

procedure TBytesArrayEncoderTest.TestSpaceArray;
var
  LExpect, LBuf: TBytesArray;
  i: Integer;
begin
  LExpect := TBytesArray.Create(TBytes.Create(00, $01), TBytes.Create(254, $FF));
  Assert.IsTrue(TBytesArrayEncoder.Execute('[ [ 00 , $01 ], [ 254 , $FF ]  ]', LBuf));
  for i := 0 to Length(LBuf) -1 do
    Assert.AreEqual<TBytes>(LExpect[0], LBuf[0], i.ToString + 'is not equals');
end;

{ TBytesEncoderTest }

procedure TBytesDecoderTest.TestBracketAndSpace;
begin
  Assert.AreEqual('[$01]', TBytesDecoder.Execute([$01]));
  Assert.AreEqual('[$01, $FF]', TBytesDecoder.Execute([$01, $FF]));
  Assert.AreEqual('[$01, $7F, $FF]', TBytesDecoder.Execute([$01, $7F, $FF]));
end;

procedure TBytesDecoderTest.TestBracketOnly;
begin
  Assert.AreEqual('[$01]', TBytesDecoder.Execute([$01], False));
  Assert.AreEqual('[$01,$FF]', TBytesDecoder.Execute([$01, $FF], False));
  Assert.AreEqual('[$01,$7F,$FF]', TBytesDecoder.Execute([$01, $7F, $FF], False));
end;

procedure TBytesDecoderTest.TestDecimal;
begin
  Assert.AreEqual('[1]', TBytesDecoder.Execute([$01], True, True, False));
  Assert.AreEqual('[1, 255]', TBytesDecoder.Execute([$01, $FF], True, True, False));
  Assert.AreEqual('[1, 127, 255]', TBytesDecoder.Execute([$01, $7F, $FF], True, True, False));
end;

procedure TBytesDecoderTest.TestSpaceOnly;
begin
  Assert.AreEqual('$01', TBytesDecoder.Execute([$01], True, False));
  Assert.AreEqual('$01, $FF', TBytesDecoder.Execute([$01, $FF], True, False));
  Assert.AreEqual('$01, $7F, $FF', TBytesDecoder.Execute([$01, $7F, $FF], True, False));
end;

{ TBytesArrayEncoderTest }

procedure TBytesArrayDecoderTest.TestBracketAndSpace;
begin
  Assert.AreEqual('[[$00]]', TBytesArrayDecoder.Execute([[$00]], True, True));
  Assert.AreEqual('[[$00, $01]]', TBytesArrayDecoder.Execute([[$00, $01]], True, True));
  Assert.AreEqual('[[$00, $01], [$7F, $FF]]', TBytesArrayDecoder.Execute([[$00, $01],[$7F, $FF]], True, True));
end;

procedure TBytesArrayDecoderTest.TestBraketOnly;
begin
  Assert.AreEqual('[[$00]]', TBytesArrayDecoder.Execute([[$00]], False, True));
  Assert.AreEqual('[[$00,$01]]', TBytesArrayDecoder.Execute([[$00, $01]], False, True));
  Assert.AreEqual('[[$00,$01],[$7F,$FF]]', TBytesArrayDecoder.Execute([[$00, $01],[$7F, $FF]], False, True));
end;

procedure TBytesArrayDecoderTest.TestDecimal;
begin
  Assert.AreEqual('[[0]]', TBytesArrayDecoder.Execute([[$00]], True, True, False));
  Assert.AreEqual('[[0, 1]]', TBytesArrayDecoder.Execute([[$00, $01]], True, True, False));
  Assert.AreEqual('[[0, 1], [127, 255]]', TBytesArrayDecoder.Execute([[$00, $01],[$7F, $FF]], True, True, False));
end;

procedure TBytesArrayDecoderTest.TestSpaceOnly;
begin
  Assert.AreEqual('[$00]', TBytesArrayDecoder.Execute([[$00]]));
  Assert.AreEqual('[$00, $01]', TBytesArrayDecoder.Execute([[$00, $01]]));
  Assert.AreEqual('[$00, $01], [$7F, $FF]', TBytesArrayDecoder.Execute([[$00, $01], [$7F, $FF]]));
end;

initialization
  TDUnitX.RegisterTestFixture(TCumsumeSpaceTest);
  TDUnitX.RegisterTestFixture(TSquareBracketBeginTest);
  TDUnitX.RegisterTestFixture(TSquareBracketEndTest);
  TDUnitX.RegisterTestFixture(TNumberValueTest);
  TDUnitX.RegisterTestFixture(TCommaTest);
  TDUnitX.RegisterTestFixture(TBytesEncoderTest);
  TDUnitX.RegisterTestFixture(TBytesArrayEncoderTest);
  TDUnitX.RegisterTestFixture(TBytesDecoderTest);
  TDUnitX.RegisterTestFixture(TBytesArrayDecoderTest);

end.
