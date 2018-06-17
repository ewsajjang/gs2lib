program mgs2libTest;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}

uses
  SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.AutoDetect.Console,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestRunner,
  DUnitX.TestFramework,
  TestmRouter in 'TestmRouter.pas',
  TestmBitMask in 'TestmBitMask.pas',
  TestmIntervalCounter in 'TestmIntervalCounter.pas',
  TestmGenericClassList in 'TestmGenericClassList.pas',
  TestmRepeatorRecord in 'TestmRepeatorRecord.pas',
  mMsgRouter in '..\source\utils\mMsgRouter.pas',
  mBitMask in '..\source\utils\mBitMask.pas',
  mGenericClassList in '..\source\utils\mGenericClassList.pas',
  mIntervalCounter in '..\source\utils\mIntervalCounter.pas',
  mRepeator in '..\source\utils\mRepeator.pas',
  TestmGenericValueList in 'TestmGenericValueList.pas',
  mGenericValueList in '..\source\utils\mGenericValueList.pas',
  mLinkedCollections in '..\source\utils\mLinkedCollections.pas',
  TestmLinkedCollections in 'TestmLinkedCollections.pas',
  t.types in 't.types.pas',
  mTypes in '..\source\utils\mTypes.pas',
  mTypesHelper in '..\source\utils\mTypesHelper.pas',
  mTypes.bytesarray in '..\source\utils\mTypes.bytesarray.pas',
  t.types.bytesarray in 't.types.bytesarray.pas',
  mEvent in '..\source\utils\mEvent.pas',
  t.event in 't.event.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
{$ENDIF}
  ReportMemoryLeaksOnShutdown := True;
  try
    //Create the runner
    runner := TDUnitX.CreateRunner;
    runner.UseRTTI := True;
    //runner.FailsOnNoAsserts := True; //Assertions must be made during tests;
    //tell the runner how we will log things
    logger := TDUnitXConsoleLogger.Create(true);
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create;
    runner.AddLogger(logger);
    runner.AddLogger(nunitLogger);

    //Run tests
    results := runner.Execute;

    {$IFNDEF CI}
      //We don't want this happening when running under CI.
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.

