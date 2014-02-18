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
  TestmBitMask in 'TestmBitMask.pas',
  TestmIntervalCounter in 'TestmIntervalCounter.pas',
  TestmGenericClassList in 'TestmGenericClassList.pas',
  TestmRouter in 'TestmRouter.pas',
  TestmRepeatorRecord in 'TestmRepeatorRecord.pas',
  mBitMask in '..\source\utils\mBitMask.pas',
  mGenericClassList in '..\source\utils\mGenericClassList.pas',
  mIntervalCounter in '..\source\utils\mIntervalCounter.pas',
  mRepeator in '..\source\utils\mRepeator.pas',
  mRouter in '..\source\utils\mRouter.pas';

{R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

