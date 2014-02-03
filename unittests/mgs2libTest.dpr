program mgs2libTest;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  mBitMask in '..\source\utils\mBitMask.pas',
  TestmBitMask in 'TestmBitMask.pas',
  TestmIntervalCounter in 'TestmIntervalCounter.pas',
  mSysUtilsEx in '..\source\utils\mSysUtilsEx.pas',
  mIntervalCounter in '..\source\utils\mIntervalCounter.pas';

{R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

