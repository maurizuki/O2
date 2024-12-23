{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2025 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit Testing.MarkdownLogger;

interface

uses
  Generics.Collections, Generics.Defaults,
  DUnitX.TestFramework, DUnitX.Loggers.Null;

type
  TMarkdownLogger = class(TDUnitXNullLogger)
  private
    FFileName: string;

    class function GetSortedList<T>(const ACollection: IEnumerable<T>;
      const ACompare: TComparison<T>): TList<T>;
  protected
    procedure OnTestingEnds(const RunResults: IRunResults); override;
  public
    constructor Create(const AFileName: string = '');
  end;

implementation

uses
  Classes, TypInfo, SysUtils, DateUtils;

function CompareFixtureResults(const Left, Right: IFixtureResult): Integer;
begin
  Result := CompareStr(Left.Fixture.Name, Right.Fixture.Name);
end;

function CompareTestResults(const Left, Right: ITestResult): Integer;
begin
  Result := CompareStr(Left.Test.Name, Right.Test.Name);
end;

{ TMarkdownLogger }

constructor TMarkdownLogger.Create(const AFileName: string);
begin
  if AFileName = '' then
    FFileName := ExtractFilePath(ParamStr(0)) + 'test-results.md'
  else
    FFileName := AFileName;
end;

class function TMarkdownLogger.GetSortedList<T>(
  const ACollection: IEnumerable<T>; const ACompare: TComparison<T>): TList<T>;
var
  AComparer: IComparer<T>;
begin
  AComparer := TDelegatedComparer<T>.Create(ACompare);
  Result := TList<T>.Create(AComparer);
  Result.AddRange(ACollection);
  Result.Sort;
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

      SortedTestResults := GetSortedList<ITestResult>(FixtureResult.TestResults,
        CompareTestResults);
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

    SortedChildren := GetSortedList<IFixtureResult>(FixtureResult.Children,
      CompareFixtureResults);
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
    StreamWriter.WriteLine('---------|---------|---------|---------|-----------------------------');
    StreamWriter.WriteLine('%-9d|%-9d|%-9d|%-9d|%s', [RunResults.TestCount,
      RunResults.FailureCount, RunResults.ErrorCount, RunResults.IgnoredCount,
      DateToISO8601(RunResults.StartTime, False)]);

    SortedFixtureResults := GetSortedList<IFixtureResult>(
      RunResults.FixtureResults, CompareFixtureResults);
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
