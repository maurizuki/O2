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
  Classes, Generics.Collections, Generics.Defaults, TypInfo, SysUtils;

function GetSortedFixtureResults(
  const Collection: IEnumerable<IFixtureResult>): TList<IFixtureResult>;
var
  Comparer: IComparer<IFixtureResult>;
begin
  Comparer := TDelegatedComparer<IFixtureResult>.Create(
    function(const Left, Right: IFixtureResult): Integer
    begin
      Result := CompareStr(Left.Fixture.Name, Right.Fixture.Name);
    end);
  Result := TList<IFixtureResult>.Create(Comparer);
  Result.AddRange(Collection);
  Result.Sort;
end;

function GetSortedTestResults(
  const Collection: IEnumerable<ITestResult>): TList<ITestResult>;
var
  Comparer: IComparer<ITestResult>;
begin
  Comparer := TDelegatedComparer<ITestResult>.Create(
    function(const Left, Right: ITestResult): Integer
    begin
      Result := CompareStr(Left.Test.Name, Right.test.Name);
    end);
  Result := TList<ITestResult>.Create(Comparer);
  Result.AddRange(Collection);
  Result.Sort;
end;

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
  SortedFixtureResults: TList<IFixtureResult>;
  FixtureResult: IFixtureResult;

procedure WriteFixtureResult(const FixtureResult : IFixtureResult);
var
  SortedTestResults: TList<ITestResult>;
  TestResult: ITestResult;
  ResultTypeString: string;
  SortedChildren: TList<IFixtureResult>;
  Child: IFixtureResult;
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

      SortedTestResults := GetSortedTestResults(FixtureResult.TestResults);
      try
        for TestResult in SortedTestResults do
        begin
          ResultTypeString := GetEnumName(TypeInfo(TTestResultType),
            Ord(TestResult.ResultType));

          if TestResult.ResultType = TTestResultType.Pass then
            StreamWriter.WriteLine('%s|%s|✔', [TestResult.Test.Name,
              ResultTypeString])
          else
            StreamWriter.WriteLine('**%s**|**%s**|❌', [TestResult.Test.Name,
              ResultTypeString]);
        end;
      finally
        SortedTestResults.Free;
      end;
    end;

    SortedChildren := GetSortedFixtureResults(FixtureResult.Children);
    try
      for Child in SortedChildren do WriteFixtureResult(Child);
    finally
      SortedChildren.Free;
    end;
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

    SortedFixtureResults := GetSortedFixtureResults(RunResults.FixtureResults);
    try
      for FixtureResult in SortedFixtureResults do
        WriteFixtureResult(FixtureResult);
    finally
      SortedFixtureResults.Free;
    end;
  finally
    StreamWriter.Free;
  end;
end;

end.
