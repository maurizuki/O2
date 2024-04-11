program O2TestProject;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,
  DCPbase64 in '..\DCPcrypt2\DCPbase64.pas',
  DCPblockciphers in '..\DCPcrypt2\DCPblockciphers.pas',
  DCPconst in '..\DCPcrypt2\DCPconst.pas',
  DCPcrypt2 in '..\DCPcrypt2\DCPcrypt2.pas',
  DCPblowfish in '..\DCPcrypt2\Ciphers\DCPblowfish.pas',
  DCPcast128 in '..\DCPcrypt2\Ciphers\DCPcast128.pas',
  DCPcast256 in '..\DCPcrypt2\Ciphers\DCPcast256.pas',
  DCPdes in '..\DCPcrypt2\Ciphers\DCPdes.pas',
  DCPice in '..\DCPcrypt2\Ciphers\DCPice.pas',
  DCPidea in '..\DCPcrypt2\Ciphers\DCPidea.pas',
  DCPmars in '..\DCPcrypt2\Ciphers\DCPmars.pas',
  DCPmisty1 in '..\DCPcrypt2\Ciphers\DCPmisty1.pas',
  DCPrc2 in '..\DCPcrypt2\Ciphers\DCPrc2.pas',
  DCPrc4 in '..\DCPcrypt2\Ciphers\DCPrc4.pas',
  DCPrc5 in '..\DCPcrypt2\Ciphers\DCPrc5.pas',
  DCPrc6 in '..\DCPcrypt2\Ciphers\DCPrc6.pas',
  DCPrijndael in '..\DCPcrypt2\Ciphers\DCPrijndael.pas',
  DCPserpent in '..\DCPcrypt2\Ciphers\DCPserpent.pas',
  DCPtea in '..\DCPcrypt2\Ciphers\DCPtea.pas',
  DCPtwofish in '..\DCPcrypt2\Ciphers\DCPtwofish.pas',
  DCPhaval in '..\DCPcrypt2\Hashes\DCPhaval.pas',
  DCPmd4 in '..\DCPcrypt2\Hashes\DCPmd4.pas',
  DCPmd5 in '..\DCPcrypt2\Hashes\DCPmd5.pas',
  DCPripemd128 in '..\DCPcrypt2\Hashes\DCPripemd128.pas',
  DCPripemd160 in '..\DCPcrypt2\Hashes\DCPripemd160.pas',
  DCPsha1 in '..\DCPcrypt2\Hashes\DCPsha1.pas',
  DCPsha256 in '..\DCPcrypt2\Hashes\DCPsha256.pas',
  DCPsha512 in '..\DCPcrypt2\Hashes\DCPsha512.pas',
  DCPtiger in '..\DCPcrypt2\Hashes\DCPtiger.pas',
  TestCiphers in 'TestCiphers.pas',
  TestHashes in 'TestHashes.pas';

{$IFNDEF TESTINSIGHT}
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger : ITestLogger;
{$ENDIF}
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //When true, Assertions must be made during tests;
    runner.FailsOnNoAsserts := False;

    //tell the runner how we will log things
    //Log to the console window if desired
    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      runner.AddLogger(logger);
    end;
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
{$ENDIF}
end.
