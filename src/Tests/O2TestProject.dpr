program O2TestProject;

{$R ..\O2\Dictionaries.res}

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
  SZCRC32 in '..\O2\Utils\SZCRC32.pas',
  MarkdownCommonMark in '..\Markdown\MarkdownCommonMark.pas',
  MarkdownHTMLEntities in '..\Markdown\MarkdownHTMLEntities.pas',
  MarkdownProcessor in '..\Markdown\MarkdownProcessor.pas',
  MarkdownUnicodeUtils in '..\Markdown\MarkdownUnicodeUtils.pas',
  Zxcvbn.DateMatcher in '..\Zxcvbn\src\Zxcvbn.DateMatcher.pas',
  Zxcvbn.DefaultMatcherFactory in '..\Zxcvbn\src\Zxcvbn.DefaultMatcherFactory.pas',
  Zxcvbn.DictionaryMatcher in '..\Zxcvbn\src\Zxcvbn.DictionaryMatcher.pas',
  Zxcvbn.L33tMatcher in '..\Zxcvbn\src\Zxcvbn.L33tMatcher.pas',
  Zxcvbn.Matcher in '..\Zxcvbn\src\Zxcvbn.Matcher.pas',
  Zxcvbn.MatcherFactory in '..\Zxcvbn\src\Zxcvbn.MatcherFactory.pas',
  Zxcvbn in '..\Zxcvbn\src\Zxcvbn.pas',
  Zxcvbn.PasswordScoring in '..\Zxcvbn\src\Zxcvbn.PasswordScoring.pas',
  Zxcvbn.RegexMatcher in '..\Zxcvbn\src\Zxcvbn.RegexMatcher.pas',
  Zxcvbn.RepeatMatcher in '..\Zxcvbn\src\Zxcvbn.RepeatMatcher.pas',
  Zxcvbn.Result in '..\Zxcvbn\src\Zxcvbn.Result.pas',
  Zxcvbn.SequenceMatcher in '..\Zxcvbn\src\Zxcvbn.SequenceMatcher.pas',
  Zxcvbn.SpatialMatcher in '..\Zxcvbn\src\Zxcvbn.SpatialMatcher.pas',
  Zxcvbn.Utility in '..\Zxcvbn\src\Zxcvbn.Utility.pas',
  uO2Classes in '..\O2\DataModel\uO2Classes.pas',
  uO2Defs in '..\O2\DataModel\uO2Defs.pas',
  uO2File in '..\O2\DataModel\uO2File.pas',
  uO2Objects in '..\O2\DataModel\uO2Objects.pas',
  uO2Relations in '..\O2\DataModel\uO2Relations.pas',
  uO2Rules in '..\O2\DataModel\uO2Rules.pas',
  uO2Utils in '..\O2\DataModel\uO2Utils.pas',
  uO2Xml in '..\O2\DataModel\uO2Xml.pas',
  uServices in '..\O2\uServices.pas',
  uGlobal in '..\O2\uGlobal.pas',
  uO2ObjectsUtils in '..\O2\Utils\uO2ObjectsUtils.pas',
  uUtils in '..\O2\Utils\uUtils.pas',
  uHTMLHelper in '..\O2\Utils\uHTMLHelper.pas',
  uEncryptionPropsModel in '..\O2\ViewModels\uEncryptionPropsModel.pas',
  uEventFilters in '..\O2\ViewModels\uEventFilters.pas',
  uFileManager in '..\O2\ViewModels\uFileManager.pas',
  uFilePropsModel in '..\O2\ViewModels\uFilePropsModel.pas',
  uHTMLExportModel in '..\O2\ViewModels\uHTMLExportModel.pas',
  uObjectModels in '..\O2\ViewModels\uObjectModels.pas',
  uPasswordStrengthInfo in '..\O2\ViewModels\uPasswordStrengthInfo.pas',
  uPrintModel in '..\O2\ViewModels\uPrintModel.pas',
  uRelationModels in '..\O2\ViewModels\uRelationModels.pas',
  uReplaceOperations in '..\O2\ViewModels\uReplaceOperations.pas',
  uRuleModels in '..\O2\ViewModels\uRuleModels.pas',
  Testing.Cryptography in 'Testing.Cryptography.pas',
  Testing.FilePropsModel in 'Testing.FilePropsModel.pas',
  Testing.ObjectModels in 'Testing.ObjectModels.pas';

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
    runner.FailsOnNoAsserts := True;

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
