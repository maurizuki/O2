{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2024 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit Testing.MarkdownLogger;

interface

uses
  DUnitX.TestFramework, DUnitX.Loggers.Null;

type
  TMarkdownLogger = class(TDUnitXNullLogger)
  private
    FFileName: string;
  protected
    procedure OnTestingEnds(const RunResults: IRunResults); override;
  public
    constructor Create(const AFileName: string = '');
  end;

implementation

uses
  Classes, TypInfo, SysUtils;

{ TMarkdownLogger }

constructor TMarkdownLogger.Create(const AFileName: string);
begin
  if AFileName = '' then
    FFileName := ExtractFilePath(ParamStr(0)) + 'test-results.md'
  else
    FFileName := AFileName;
end;

procedure TMarkdownLogger.OnTestingEnds(const RunResults: IRunResults);
var
  StreamWriter: TStreamWriter;
  FixtureResult: IFixtureResult;

procedure WriteFixtureResult(const FixtureResult : IFixtureResult);
var
  Child: IFixtureResult;
  TestResult: ITestResult;
  ResultType: string;
begin
    StreamWriter.WriteLine;

    if FixtureResult.Fixture.TestClass.ClassNameIs('TObject') then
      StreamWriter.WriteLine('## Namespace ' + FixtureResult.Fixture.Name)
    else
    begin
      StreamWriter.WriteLine('### Fixture ' + FixtureResult.Fixture.Name);
      StreamWriter.WriteLine;
      StreamWriter.WriteLine('Test case|Result|Success');
      StreamWriter.WriteLine('---------|------|-------');
      for TestResult in FixtureResult.TestResults do
      begin
        ResultType := GetEnumName(TypeInfo(TTestResultType),
          Ord(TestResult.ResultType));

        if TestResult.ResultType = TTestResultType.Pass then
          StreamWriter.WriteLine('%s|%s|✔', [TestResult.Test.Name, ResultType])
        else
          StreamWriter.WriteLine('**%s**|**%s**|❌', [TestResult.Test.Name,
            ResultType]);
      end;
    end;

    for Child in FixtureResult.Children do WriteFixtureResult(Child);
end;

begin
  StreamWriter := TStreamWriter.Create(FFileName, False, TEncoding.UTF8);
  try
    StreamWriter.WriteLine('# TEST RESULTS');
    StreamWriter.WriteLine;
    StreamWriter.WriteLine('Total    |Failures |Errors   |Ignored  |Date');
    StreamWriter.WriteLine('---------|---------|---------|---------|-------------------');
    StreamWriter.WriteLine('%-9d|%-9d|%-9d|%-9d|%s', [RunResults.TestCount,
      RunResults.FailureCount, RunResults.ErrorCount, RunResults.IgnoredCount,
      FormatDateTime('yyyy"-"mm"-"dd" "hh":"nn":"ss', RunResults.StartTime)]);

    for FixtureResult in RunResults.FixtureResults do
      WriteFixtureResult(FixtureResult);
  finally
    StreamWriter.Free;
  end;
end;

end.
