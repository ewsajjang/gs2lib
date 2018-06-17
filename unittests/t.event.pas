unit t.event;

interface

uses
  mEvent,

  System.Classes, System.SysUtils,
  DUnitX.TestFramework
  ;

type
  [TestFixture]
  TestEvent = class
  private
    FEvent: TEvent<TProc>;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;

    [Test] procedure TestSubscribe;
  end;


implementation

{ TestEvent }

procedure TestEvent.Setup;
begin
end;

procedure TestEvent.TearDown;
begin
end;

procedure TestEvent.TestSubscribe;
var
  LEvent: TProc;
  LExecute: Boolean;
begin
  LExecute := False;
  FEvent.Subscribe(procedure begin LExecute := True end);
  for LEvent in FEvent.Listeners do
    LEvent;
  Assert.IsTrue(LExecute);
end;

initialization
  TDUnitX.RegisterTestFixture(TestEvent);

end.
