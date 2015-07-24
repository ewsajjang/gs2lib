program mgs2libTest;


{$APPTYPE CONSOLE}

uses
  SysUtils,
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
  TestmLinkedCollections in 'TestmLinkedCollections.pas';

{R *.RES}

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
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

