unit Testing.FileManager;

interface

uses
  DUnitX.TestFramework, uServices, uO2Rules;

type
  [TestFixture]
  TFileManagerTests = class
  public
    [Test]
    procedure LoadTags;

    [Test]
    procedure FilterByName;

    [Test]
    [TestCase('All.ExpirationDate'            , '0,2000,1,1,rtExpirationDate,0,0,')]
    [TestCase('AllEvents.ExpirationDate'      , '1,2000,1,1,rtExpirationDate,0,0,2000-01-01')]
    [TestCase('Custom.ExpirationDate.Start'   , '2,2000,1,15,rtExpirationDate,5,0,2000-01-10')]
    [TestCase('Custom.ExpirationDate.End'     , '2,2000,1,15,rtExpirationDate,0,5,2000-01-20')]
    [TestCase('Today.ExpirationDate'          , '3,2000,1,1,rtExpirationDate,0,0,2000-01-01')]
    [TestCase('Tomorrow.ExpirationDate'       , '4,2000,1,1,rtExpirationDate,0,0,2000-01-02')]
    [TestCase('ThisWeek.ExpirationDate.Start' , '5,2000,1,9,rtExpirationDate,0,0,2000-01-03')]
    [TestCase('ThisWeek.ExpirationDate.End'   , '5,2000,1,10,rtExpirationDate,0,0,2000-01-16')]
    [TestCase('ThisMonth.ExpirationDate.Start', '6,2000,1,31,rtExpirationDate,0,0,2000-01-01')]
    [TestCase('ThisMonth.ExpirationDate.End'  , '6,2000,2,1,rtExpirationDate,0,0,2000-02-29')]
    [TestCase('ThisYear.ExpirationDate.Start' , '7,2000,12,31,rtExpirationDate,0,0,2000-01-01')]
    [TestCase('ThisYear.ExpirationDate.End'   , '7,2001,1,1,rtExpirationDate,0,0,2001-12-31')]
    [TestCase('Next7days.ExpirationDate'      , '8,2000,1,1,rtExpirationDate,0,0,2000-01-07')]
    [TestCase('Next15days.ExpirationDate'     , '9,2000,1,1,rtExpirationDate,0,0,2000-01-15')]
    [TestCase('Next30days.ExpirationDate'     , '10,2000,1,1,rtExpirationDate,0,0,2000-01-30')]
    [TestCase('Next60days.ExpirationDate'     , '11,2000,1,1,rtExpirationDate,0,0,2000-03-01')]
    [TestCase('Next90days.ExpirationDate'     , '12,2000,1,1,rtExpirationDate,0,0,2000-03-31')]
    [TestCase('Next180days.ExpirationDate'    , '13,2000,1,1,rtExpirationDate,0,0,2000-06-29')]
    [TestCase('Next365days.ExpirationDate'    , '14,2000,1,1,rtExpirationDate,0,0,2000-12-31')]
    [TestCase('All.Recurrence'                , '0,2000,1,1,rtRecurrence,0,0,')]
    [TestCase('AllEvents.Recurrence'          , '1,2001,1,1,rtRecurrence,0,0,2000-01-01')]
    [TestCase('Custom.Recurrence.Start'       , '2,2001,1,15,rtRecurrence,5,0,2000-01-10')]
    [TestCase('Custom.Recurrence.End'         , '2,2001,1,15,rtRecurrence,0,5,2000-01-20')]
    [TestCase('Today.Recurrence'              , '3,2001,1,1,rtRecurrence,0,0,2000-01-01')]
    [TestCase('Tomorrow.Recurrence'           , '4,2001,1,1,rtRecurrence,0,0,2000-01-02')]
    [TestCase('ThisWeek.Recurrence.Start'     , '5,2001,1,7,rtRecurrence,0,0,2000-01-01')]
    [TestCase('ThisWeek.Recurrence.End'       , '5,2001,1,8,rtRecurrence,0,0,2000-01-14')]
    [TestCase('ThisMonth.Recurrence.Start'    , '6,2001,1,31,rtRecurrence,0,0,2000-01-01')]
    [TestCase('ThisMonth.Recurrence.End'      , '6,2001,2,1,rtRecurrence,0,0,2000-02-29')]
    [TestCase('ThisYear.Recurrence.Start'     , '7,2001,12,31,rtRecurrence,0,0,2000-01-01')]
    [TestCase('ThisYear.Recurrence.End'       , '7,2002,1,1,rtRecurrence,0,0,2001-12-31')]
    [TestCase('Next7days.Recurrence'          , '8,2001,1,1,rtRecurrence,0,0,2000-01-07')]
    [TestCase('Next15days.Recurrence'         , '9,2001,1,1,rtRecurrence,0,0,2000-01-15')]
    [TestCase('Next30days.Recurrence'         , '10,2001,1,1,rtRecurrence,0,0,2000-01-30')]
    [TestCase('Next60days.Recurrence'         , '11,2001,1,1,rtRecurrence,0,0,2000-03-01')]
    [TestCase('Next90days.Recurrence'         , '12,2001,1,1,rtRecurrence,0,0,2000-03-31')]
    [TestCase('Next180days.Recurrence'        , '13,2001,1,1,rtRecurrence,0,0,2000-06-29')]
    [TestCase('Next365days.Recurrence'        , '14,2001,1,1,rtRecurrence,0,0,2000-12-31')]
    procedure FilterByEvent(EventFilterIndex, AYear, AMonth, ADay: Integer;
      ARuleType: TO2RuleType; const DaysBefore, DaysAfter, FieldValue: string);

    [Test]
    procedure FilterByTag;

    [Test]
    procedure FilterUntagged;

    [Test]
    procedure FilterByRule;

    { TODO -omaurizuki : Test GetNextEvent }

    { TODO -omaurizuki : Test GetHighlight }

    { TODO -omaurizuki : Test GetDisplayText }

    { TODO -omaurizuki : Test GetHyperLink }

    [Test]
    [TestCase('None'          , 'rtNone,False')]
    [TestCase('HyperLink'     , 'rtHyperLink,True')]
    [TestCase('Email'         , 'rtEmail,True')]
    [TestCase('Password'      , 'rtPassword,False')]
    [TestCase('ExpirationDate', 'rtExpirationDate,False')]
    [TestCase('Recurrence'    , 'rtRecurrence,False')]
    [TestCase('Highlight'     , 'rtHighlight,False')]
    procedure IsHyperlinkOrEmail(ARuleType: TO2RuleType; Expected: Boolean);

    [Test]
    [TestCase('None'          , 'rtNone,False')]
    [TestCase('HyperLink'     , 'rtHyperLink,True')]
    [TestCase('Email'         , 'rtEmail,False')]
    [TestCase('Password'      , 'rtPassword,False')]
    [TestCase('ExpirationDate', 'rtExpirationDate,False')]
    [TestCase('Recurrence'    , 'rtRecurrence,False')]
    [TestCase('Highlight'     , 'rtHighlight,False')]
    procedure IsHyperlink(ARuleType: TO2RuleType; Expected: Boolean);

    [Test]
    [TestCase('None'          , 'rtNone,False')]
    [TestCase('HyperLink'     , 'rtHyperLink,False')]
    [TestCase('Email'         , 'rtEmail,True')]
    [TestCase('Password'      , 'rtPassword,False')]
    [TestCase('ExpirationDate', 'rtExpirationDate,False')]
    [TestCase('Recurrence'    , 'rtRecurrence,False')]
    [TestCase('Highlight'     , 'rtHighlight,False')]
    procedure IsEmail(ARuleType: TO2RuleType; Expected: Boolean);
  end;

  TCustomDateProvider = class(TInterfacedObject, IDateProvider)
  private
    FDate: TDateTime;
  public
    constructor Create(ADate: TDateTime);
    function GetDate: TDateTime;
  end;

implementation

uses
  SysUtils, DateUtils, uFileManager, uO2Objects;

{ TFileManagerTests }

procedure TFileManagerTests.LoadTags;
var
  Model: IFileManager;
begin
  Model := TFileManager.Create(nil, nil, nil);

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
  Model := TFileManager.Create(nil, nil, nil);

  Model.O2File.Objects.AddObject('A matching object');
  Model.O2File.Objects.AddObject('Another object');

  Model.ObjectName := 'Matching';

  for AObject in Model.GetObjects do
    Assert.AreEqual('A matching object', AObject.Name);
end;

procedure TFileManagerTests.FilterByEvent(EventFilterIndex, AYear, AMonth,
  ADay: Integer; ARuleType: TO2RuleType; const DaysBefore, DaysAfter,
  FieldValue: string);
var
  Model: IFileManager;
  AObject: TO2Object;
begin
  Model := TFileManager.Create(TCustomDateProvider.Create(EncodeDate(AYear,
    AMonth, ADay)), nil, nil);

  Model.O2File.Objects.AddObject('Object 1').Fields.AddField('Field 1')
    .FieldValue := FieldValue;

  with Model.O2File.Rules.AddRule('Rule 1') do
  begin
    Active := True;
    RuleType := ARuleType;
    FieldName := '*';
    FieldValue := '*';
    Params.AddParam(DateSeparatorParam).ParamValue := '-';
    Params.AddParam(ShortDateFormatParam).ParamValue := 'yyyy/mm/dd';
    Params.AddParam(DaysBeforeParam).ParamValue := DaysBefore;
    Params.AddParam(DaysAfterParam).ParamValue := DaysAfter;
  end;

  Model.EventFilterIndex := EventFilterIndex;

  for AObject in Model.GetObjects do
    Assert.AreEqual('Object 1', AObject.Name);
end;

procedure TFileManagerTests.FilterByTag;
var
  Model: IFileManager;
  AObject: TO2Object;
begin
  Model := TFileManager.Create(nil, nil, nil);

  Model.O2File.Objects.AddObject('Object 1').Tag := 'Tag 1';
  Model.O2File.Objects.AddObject('Object 2').Tag := 'Tag 1,Tag 2';
  Model.O2File.Objects.AddObject('Object 3');

  Model.ObjectTags.Add('tag 2');
  Model.IncludeUntagged := False;

  for AObject in Model.GetObjects do
    Assert.AreEqual('Object 2', AObject.Name);
end;

procedure TFileManagerTests.FilterUntagged;
var
  Model: IFileManager;
  AObject: TO2Object;
begin
  Model := TFileManager.Create(nil, nil, nil);

  Model.O2File.Objects.AddObject('Object 1').Tag := 'Tag 1';
  Model.O2File.Objects.AddObject('Object 2').Tag := 'Tag 1,Tag 2';
  Model.O2File.Objects.AddObject('Object 3');

  Model.IncludeUntagged := True;

  for AObject in Model.GetObjects do
    Assert.AreEqual('Object 3', AObject.Name);
end;

procedure TFileManagerTests.FilterByRule;
var
  Model: IFileManager;
  AObject: TO2Object;
  ARule: TO2Rule;
begin
  Model := TFileManager.Create(nil, nil, nil);

  Model.O2File.Objects.AddObject('Object 1').Fields.AddField('Field 1')
    .FieldValue := 'No match';
  Model.O2File.Objects.AddObject('Object 2').Fields.AddField('Field 1')
    .FieldValue := 'Match';
  Model.O2File.Objects.AddObject('Object 3');

  ARule := Model.O2File.Rules.AddRule('Rule 1');
  ARule.Active := True;
  ARule.RuleType := rtHighlight;
  ARule.FieldName := '*';
  ARule.FieldValue := 'match';

  Model.ObjectRules.Add(ARule);

  for AObject in Model.GetObjects do
    Assert.AreEqual('Object 2', AObject.Name);
end;

procedure TFileManagerTests.IsHyperlinkOrEmail(ARuleType: TO2RuleType;
  Expected: Boolean);
var
  Model: IFileManager;
begin
  Model := TFileManager.Create(nil, nil, nil);

  Model.O2File.Objects.AddObject('Object 1').Fields.AddField('Field 1');

  with Model.O2File.Rules.AddRule('Rule 1') do
  begin
    Active := True;
    RuleType := ARuleType;
    FieldName := '*';
    FieldValue := '*';
  end;

  Assert.AreEqual(Expected,
    Model.IsHyperlinkOrEmail(Model.O2File.Objects[0].Fields[0]));
end;

procedure TFileManagerTests.IsHyperlink(ARuleType: TO2RuleType;
  Expected: Boolean);
var
  Model: IFileManager;
begin
  Model := TFileManager.Create(nil, nil, nil);

  Model.O2File.Objects.AddObject('Object 1').Fields.AddField('Field 1');

  with Model.O2File.Rules.AddRule('Rule 1') do
  begin
    Active := True;
    RuleType := ARuleType;
    FieldName := '*';
    FieldValue := '*';
  end;

  Assert.AreEqual(Expected,
    Model.IsHyperlink(Model.O2File.Objects[0].Fields[0]));
end;

procedure TFileManagerTests.IsEmail(ARuleType: TO2RuleType; Expected: Boolean);
var
  Model: IFileManager;
begin
  Model := TFileManager.Create(nil, nil, nil);

  Model.O2File.Objects.AddObject('Object 1').Fields.AddField('Field 1');

  with Model.O2File.Rules.AddRule('Rule 1') do
  begin
    Active := True;
    RuleType := ARuleType;
    FieldName := '*';
    FieldValue := '*';
  end;

  Assert.AreEqual(Expected, Model.IsEmail(Model.O2File.Objects[0].Fields[0]));
end;

{ TCustomDateProvider }

constructor TCustomDateProvider.Create(ADate: TDateTime);
begin
  FDate := ADate;
end;

function TCustomDateProvider.GetDate: TDateTime;
begin
  Result := FDate;
end;

end.
