unit Testing.FileManager;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TFileManagerTests = class
  public
    [Test]
    procedure LoadTags;

    [Test]
    procedure FilterByName;

    [Test]
    [TestCase('All'        , '0,12,All')]
    [TestCase('AllEvents'  , '1,11,AllEvents')]
    [TestCase('Custom'     , '2,2,Custom')]
    [TestCase('Today'      , '3,1,Today')]
    [TestCase('Tomorrow'   , '4,1,Tomorrow')]
    { TODO -omaurizuki : TestCase ThisWeek }
    { TODO -omaurizuki : TestCase ThisMonth }
    { TODO -omaurizuki : TestCase ThisYear }
    [TestCase('Next7days'  , '8,3,Next7days')]
    [TestCase('Next15days' , '9,4,Next15days')]
    [TestCase('Next30days' , '10,5,Next30days')]
    [TestCase('Next60days' , '11,6,Next60days')]
    [TestCase('Next90days' , '12,7,Next90days')]
    [TestCase('Next180days', '13,8,Next180days')]
    [TestCase('Next365days', '14,9,Next365days')]
    procedure FilterByEvent(EventFilterIndex, Count: Integer;
      const Tag: string);
  end;

implementation

uses
  SysUtils, uServices, uFileManager, uO2Objects, uO2Rules;

{ TFileManagerTests }

procedure TFileManagerTests.LoadTags;
var
  Model: IFileManager;
begin
  Model := TFileManager.Create(nil, nil);

  Model.O2File.Objects.AddObject('Object 1').Tag := 'Tag 1,Tag 2';
  Model.O2File.Objects.AddObject('Object 2').Tag := 'tag 2,Tag 3';

  Assert.AreEqual(4, Model.Tags.Count);
  Assert.Contains(Model.Tags, 'Tag 1');
  Assert.Contains(Model.Tags, 'Tag 2');
  Assert.Contains(Model.Tags, 'Tag 3');
end;

procedure TFileManagerTests.FilterByName;
var
  Model: IFileManager;
  AObject: TO2Object;
begin
  Model := TFileManager.Create(nil, nil);

  Model.O2File.Objects.AddObject('A matching object');
  Model.O2File.Objects.AddObject('Another object');

  Model.ObjectName := 'Matching';

  for AObject in Model.GetObjects do
    Assert.AreEqual('A matching object', AObject.Name);
end;

procedure TFileManagerTests.FilterByEvent(EventFilterIndex, Count: Integer;
  const Tag: string);
var
  Model: IFileManager;
  AObject: TO2Object;
  ActualCount: Integer;

function ToStr(ADate: TDate): string;
var
  Year, Month, Day: Word;
begin
  DecodeDate(ADate, Year, Month, Day);
  Result := Format('%.4d-%.2d-%.2d', [Year, Month, Day]);
end;

begin
  Model := TFileManager.Create(nil, nil);

  Model.O2File.Objects.AddObject('Object 1').Tag := '(All)';

  with Model.O2File.Objects.AddObject('Object 2') do
  begin
    Tag := '(All),(AllEvents)';
    Fields.AddField('Field 1').FieldValue := '1970-01-01';
  end;

  with Model.O2File.Objects.AddObject('Object 3') do
  begin
    Tag := '(All),(AllEvents),(Custom)';
    Fields.AddField('Field 1').FieldValue := ToStr(Date - 10);
  end;

  with Model.O2File.Objects.AddObject('Object 4') do
  begin
    Tag := '(All),(AllEvents),(Custom),(Today),(Next7days),(Next15days),(Next30days),(Next60days),(Next90days),(Next180days),(Next365days)';
    Fields.AddField('Field 1').FieldValue := ToStr(Date);
  end;

  with Model.O2File.Objects.AddObject('Object 5') do
  begin
    Tag := '(All),(AllEvents),(Tomorrow),(Next7days),(Next15days),(Next30days),(Next60days),(Next90days),(Next180days),(Next365days)';
    Fields.AddField('Field 1').FieldValue := ToStr(Date + 1);
  end;

  with Model.O2File.Objects.AddObject('Object 6') do
  begin
    Tag := '(All),(AllEvents),(Next7days),(Next15days),(Next30days),(Next60days),(Next90days),(Next180days),(Next365days)';
    Fields.AddField('Field 1').FieldValue := ToStr(Date + 7);
  end;

  with Model.O2File.Objects.AddObject('Object 7') do
  begin
    Tag := '(All),(AllEvents),(Next15days),(Next30days),(Next60days),(Next90days),(Next180days),(Next365days)';
    Fields.AddField('Field 1').FieldValue := ToStr(Date + 15);
  end;

  with Model.O2File.Objects.AddObject('Object 8') do
  begin
    Tag := '(All),(AllEvents),(Next30days),(Next60days),(Next90days),(Next180days),(Next365days)';
    Fields.AddField('Field 1').FieldValue := ToStr(Date + 30);
  end;

  with Model.O2File.Objects.AddObject('Object 9') do
  begin
    Tag := '(All),(AllEvents),(Next60days),(Next90days),(Next180days),(Next365days)';
    Fields.AddField('Field 1').FieldValue := ToStr(Date + 60);
  end;

  with Model.O2File.Objects.AddObject('Object 10') do
  begin
    Tag := '(All),(AllEvents),(Next90days),(Next180days),(Next365days)';
    Fields.AddField('Field 1').FieldValue := ToStr(Date + 90);
  end;

  with Model.O2File.Objects.AddObject('Object 11') do
  begin
    Tag := '(All),(AllEvents),(Next180days),(Next365days)';
    Fields.AddField('Field 1').FieldValue := ToStr(Date + 180);
  end;

  with Model.O2File.Objects.AddObject('Object 12') do
  begin
    Tag := '(All),(AllEvents),(Next365days)';
    Fields.AddField('Field 1').FieldValue := ToStr(Date + 365);
  end;

  with Model.O2File.Rules.AddRule('Rule 1') do
  begin
    Active := True;
    RuleType := rtExpirationDate;
    FieldName := '*';
    FieldValue := '*';
    Params.AddParam(DateSeparatorParam).ParamValue := '-';
    Params.AddParam(ShortDateFormatParam).ParamValue := 'yyyy/mm/dd';
    Params.AddParam(DaysBeforeParam).ParamValue := '10';
    Params.AddParam(DaysAfterParam).ParamValue := '0';
  end;

  Model.EventFilterIndex := EventFilterIndex;

  ActualCount := 0;
  for AObject in Model.GetObjects do
  begin
    Assert.Contains(AObject.Tag, '(' + Tag + ')', 'Object: ' + AObject.Name);
    Inc(ActualCount);
  end;

  Assert.AreEqual(Count, ActualCount);
end;

end.
