unit Testing.RuleModels;

interface

uses
  DUnitX.TestFramework, uO2File, uO2Rules, uServices;

type
  TRulePropsModelTests = class
  protected
    FO2File: TO2File;

    function CreateModel: IRuleProps; virtual; abstract;
  public
    [Setup]
    procedure Setup; virtual;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure LoadRuleTypes;

    [Test]
    procedure LoadDateFormats;

    [Test]
    procedure SaveRuleName;

    [Test]
    [TestCase('HyperLink'     , '0,rtHyperLink')]
    [TestCase('Email'         , '1,rtEmail')]
    [TestCase('Password'      , '2,rtPassword')]
    [TestCase('ExpirationDate', '3,rtExpirationDate')]
    [TestCase('Recurrence'    , '4,rtRecurrence')]
    [TestCase('Highlight'     , '5,rtHighlight')]
    procedure SaveRuleTypeIndex(Index: Integer; Expected: TO2RuleType);

    [Test]
    procedure SaveFieldNameMask;

    [Test]
    procedure SaveFieldValueMask;

    [Test]
    [TestCase('HyperLink'     , '0,New hyperlink mask,New hyperlink mask')]
    [TestCase('Email'         , '1,New hyperlink mask,')]
    [TestCase('Password'      , '2,New hyperlink mask,')]
    [TestCase('ExpirationDate', '3,New hyperlink mask,')]
    [TestCase('Recurrence'    , '4,New hyperlink mask,')]
    [TestCase('Highlight'     , '5,New hyperlink mask,')]
    procedure SaveHyperLinkMask(RuleTypeIndex: Integer; const Mask,
      Expected: string);

    [Test]
    [TestCase('HyperLink'     , '0,False,')]
    [TestCase('Email'         , '1,False,')]
    [TestCase('Password.False', '2,False,False')]
    [TestCase('Password.True' , '2,True,True')]
    [TestCase('ExpirationDate', '3,False,')]
    [TestCase('Recurrence'    , '4,False,')]
    [TestCase('Highlight'     , '5,False,')]
    procedure SaveDisplayPasswordStrength(RuleTypeIndex: Integer;
      DisplayPasswordStrength: Boolean; const Expected: string);

    [Test]
    [TestCase('HyperLink'     , '0,New display mask,')]
    [TestCase('Email'         , '1,New display mask,')]
    [TestCase('Password'      , '2,New display mask,')]
    [TestCase('ExpirationDate', '3,New display mask,New display mask')]
    [TestCase('Recurrence'    , '4,New display mask,New display mask')]
    [TestCase('Highlight'     , '5,New display mask,')]
    procedure SaveDisplayMask(RuleTypeIndex: Integer; const Mask,
      Expected: string);

    [Test]
    [TestCase('HyperLink'         , '0,0,')]
    [TestCase('Email'             , '1,0,')]
    [TestCase('Password'          , '2,0,')]
    [TestCase('ExpirationDate.YMD', '3,0,yyyy/mm/dd')]
    [TestCase('ExpirationDate.MDY', '3,1,mm/dd/yyyy')]
    [TestCase('ExpirationDate.DMY', '3,2,dd/mm/yyyy')]
    [TestCase('Recurrence.YMD'    , '4,0,yyyy/mm/dd')]
    [TestCase('Recurrence.MDY'    , '4,1,mm/dd/yyyy')]
    [TestCase('Recurrence.DMY'    , '4,2,dd/mm/yyyy')]
    [TestCase('Highlight'         , '5,0,')]
    procedure SaveDateFormatIndex(RuleTypeIndex, DateFormatIndex: Integer;
      const Expected: string);

    [Test]
    [TestCase('HyperLink'     , '0,New date separator,')]
    [TestCase('Email'         , '1,New date separator,')]
    [TestCase('Password'      , '2,New date separator,')]
    [TestCase('ExpirationDate', '3,New date separator,New date separator')]
    [TestCase('Recurrence'    , '4,New date separator,New date separator')]
    [TestCase('Highlight'     , '5,New date separator,')]
    procedure SaveDateSeparator(RuleTypeIndex: Integer; const DateSeparator,
      Expected: string);

    [Test]
    [TestCase('HyperLink'     , '0,42,')]
    [TestCase('Email'         , '1,42,')]
    [TestCase('Password'      , '2,42,')]
    [TestCase('ExpirationDate', '3,42,42')]
    [TestCase('Recurrence'    , '4,42,42')]
    [TestCase('Highlight'     , '5,42,')]
    procedure SaveDaysBefore(RuleTypeIndex, Days: Integer;
      const Expected: string);

    [Test]
    [TestCase('HyperLink'     , '0,42,')]
    [TestCase('Email'         , '1,42,')]
    [TestCase('Password'      , '2,42,')]
    [TestCase('ExpirationDate', '3,42,42')]
    [TestCase('Recurrence'    , '4,42,42')]
    [TestCase('Highlight'     , '5,42,')]
    procedure SaveDaysAfter(RuleTypeIndex, Days: Integer;
      const Expected: string);
  end;

  [TestFixture]
  TNewRuleModelTests = class(TRulePropsModelTests)
  protected
    function CreateModel: IRuleProps; override;
  public
    [Test]
    procedure LoadRuleName;

    [Test]
    procedure LoadRuleTypeIndex;

    [Test]
    procedure LoadFieldNameMask;

    [Test]
    procedure LoadFieldValueMask;

    [Test]
    procedure LoadHyperLinkMask;

    [Test]
    procedure LoadDisplayPasswordStrength;

    [Test]
    procedure LoadDisplayMask;

    [Test]
    procedure LoadDateFormatIndex;

    [Test]
    procedure LoadDateSeparator;

    [Test]
    procedure LoadDaysBefore;

    [Test]
    procedure LoadDaysAfter;
  end;

  TDuplicateEditRuleModelTests = class(TRulePropsModelTests)
  protected
    FO2Rule: TO2Rule;
  public
    [Setup]
    procedure Setup; override;

    [Test]
    [TestCase('HyperLink'     , 'rtHyperLink,0')]
    [TestCase('Email'         , 'rtEmail,1')]
    [TestCase('Password'      , 'rtPassword,2')]
    [TestCase('ExpirationDate', 'rtExpirationDate,3')]
    [TestCase('Recurrence'    , 'rtRecurrence,4')]
    [TestCase('Highlight'     , 'rtHighlight,5')]
    procedure LoadRuleTypeIndex(RuleType: TO2RuleType; Expected: Integer);

    [Test]
    procedure LoadFieldNameMask;

    [Test]
    procedure LoadFieldValueMask;

    [Test]
    procedure LoadHyperLinkMask;

    [Test]
    [TestCase('False', 'False,False')]
    [TestCase('True' , 'True,True')]
    procedure LoadDisplayPasswordStrength(const Value: string;
      Expected: Boolean);

    [Test]
    procedure LoadDisplayMask;

    [Test]
    [TestCase('YMD', 'yyyy/mm/dd,0')]
    [TestCase('MDY', 'mm/dd/yyyy,1')]
    [TestCase('DMY', 'dd/mm/yyyy,2')]
    procedure LoadDateFormatIndex(const DateFormat: string; Expected: Integer);

    [Test]
    procedure LoadDateSeparator;

    [Test]
    procedure LoadDaysBefore;

    [Test]
    procedure LoadDaysAfter;
  end;

  [TestFixture]
  TDuplicateRuleModelTests = class(TDuplicateEditRuleModelTests)
  protected
    function CreateModel: IRuleProps; override;
  public
    [Test]
    procedure LoadRuleName;
  end;

  [TestFixture]
  TEditRuleModelTests = class(TDuplicateEditRuleModelTests)
  protected
    function CreateModel: IRuleProps; override;
  public
    [Test]
    procedure LoadRuleName;
  end;

implementation

uses
  Classes, SysUtils, uRuleModels;

{ TRulePropsModelTests }

procedure TRulePropsModelTests.Setup;
begin
  FO2File := TO2File.Create;
end;

procedure TRulePropsModelTests.TearDown;
begin
  FO2File.Free;
end;

procedure TRulePropsModelTests.LoadRuleTypes;
var
  Expected: TStrings;
  Model: IRuleProps;
begin
  Model := CreateModel;

  Expected := TStringList.Create;
  try
    Expected.Add('Internet link');
    Expected.Add('E-mail address');
    Expected.Add('Password');
    Expected.Add('Expiration date');
    Expected.Add('Recurrence');
    Expected.Add('Highlight');

    Assert.AreEqual(Expected, Model.RuleTypes);
  finally
    Expected.Free;
  end;
end;

procedure TRulePropsModelTests.LoadDateFormats;
var
  Expected: TStrings;
  Model: IRuleProps;
begin
  Model := CreateModel;

  Expected := TStringList.Create;
  try
    Expected.Add('Year, month, day');
    Expected.Add('Month, day, year');
    Expected.Add('Day, month, year');

    Assert.AreEqual(Expected, Model.DateFormats);
  finally
    Expected.Free;
  end;
end;

procedure TRulePropsModelTests.SaveRuleName;
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Model.RuleName := 'New rule name';
  Model.ApplyChanges;

  Assert.AreEqual('New rule name', Model.Rule.Name);
end;

procedure TRulePropsModelTests.SaveRuleTypeIndex(Index: Integer;
  Expected: TO2RuleType);
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Model.RuleTypeIndex := Index;
  Model.ApplyChanges;

  Assert.AreEqual(Expected, Model.Rule.RuleType);
end;

procedure TRulePropsModelTests.SaveFieldNameMask;
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Model.FieldNameMask := 'New field name mask';
  Model.ApplyChanges;

  Assert.AreEqual('New field name mask', Model.Rule.FieldName);
end;

procedure TRulePropsModelTests.SaveFieldValueMask;
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Model.FieldValueMask := 'New field value mask';
  Model.ApplyChanges;

  Assert.AreEqual('New field value mask', Model.Rule.FieldValue);
end;

procedure TRulePropsModelTests.SaveHyperLinkMask(RuleTypeIndex: Integer;
  const Mask, Expected: string);
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Model.RuleTypeIndex := RuleTypeIndex;
  Model.HyperLinkMask := Mask;
  Model.ApplyChanges;

  Assert.AreEqual(Expected, Model.Rule.Params.Values['Mask']);
end;

procedure TRulePropsModelTests.SaveDisplayPasswordStrength(
  RuleTypeIndex: Integer; DisplayPasswordStrength: Boolean;
  const Expected: string);
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Model.RuleTypeIndex := RuleTypeIndex;
  Model.DisplayPasswordStrength := DisplayPasswordStrength;
  Model.ApplyChanges;

  Assert.AreEqual(Expected,
    Model.Rule.Params.Values['DisplayPasswordStrength']);
end;

procedure TRulePropsModelTests.SaveDisplayMask(RuleTypeIndex: Integer;
  const Mask, Expected: string);
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Model.RuleTypeIndex := RuleTypeIndex;
  Model.DisplayMask := Mask;
  Model.ApplyChanges;

  Assert.AreEqual(Expected, Model.Rule.Params.Values['DisplayMask']);
end;

procedure TRulePropsModelTests.SaveDateFormatIndex(RuleTypeIndex,
  DateFormatIndex: Integer; const Expected: string);
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Model.RuleTypeIndex := RuleTypeIndex;
  Model.DateFormatIndex := DateFormatIndex;
  Model.ApplyChanges;

  Assert.AreEqual(Expected, Model.Rule.Params.Values['ShortDateFormat']);
end;

procedure TRulePropsModelTests.SaveDateSeparator(RuleTypeIndex: Integer;
  const DateSeparator, Expected: string);
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Model.RuleTypeIndex := RuleTypeIndex;
  Model.DateSeparator := DateSeparator;
  Model.ApplyChanges;

  Assert.AreEqual(Expected, Model.Rule.Params.Values['DateSeparator']);
end;

procedure TRulePropsModelTests.SaveDaysBefore(RuleTypeIndex, Days: Integer;
  const Expected: string);
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Model.RuleTypeIndex := RuleTypeIndex;
  Model.DaysBefore := Days;
  Model.ApplyChanges;

  Assert.AreEqual(Expected, Model.Rule.Params.Values['DaysBefore']);
end;

procedure TRulePropsModelTests.SaveDaysAfter(RuleTypeIndex, Days: Integer;
  const Expected: string);
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Model.RuleTypeIndex := RuleTypeIndex;
  Model.DaysAfter := Days;
  Model.ApplyChanges;

  Assert.AreEqual(Expected, Model.Rule.Params.Values['DaysAfter']);
end;

{ TNewRuleModelTests }

function TNewRuleModelTests.CreateModel: IRuleProps;
begin
  Result := TNewRuleModel.Create(FO2File);
end;

procedure TNewRuleModelTests.LoadRuleName;
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Assert.IsEmpty(Model.RuleName);
end;

procedure TNewRuleModelTests.LoadRuleTypeIndex;
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Assert.AreEqual(0, Model.RuleTypeIndex);
end;

procedure TNewRuleModelTests.LoadFieldNameMask;
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Assert.IsEmpty(Model.FieldNameMask);
end;

procedure TNewRuleModelTests.LoadFieldValueMask;
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Assert.IsEmpty(Model.FieldValueMask);
end;

procedure TNewRuleModelTests.LoadHyperLinkMask;
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Assert.IsEmpty(Model.HyperLinkMask);
end;

procedure TNewRuleModelTests.LoadDisplayPasswordStrength;
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Assert.IsFalse(Model.DisplayPasswordStrength);
end;

procedure TNewRuleModelTests.LoadDisplayMask;
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Assert.IsEmpty(Model.DisplayMask);
end;

procedure TNewRuleModelTests.LoadDateFormatIndex;
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Assert.AreEqual(0, Model.DateFormatIndex);
end;

procedure TNewRuleModelTests.LoadDateSeparator;
var
  FormatSettings: TFormatSettings;
  Model: IRuleProps;
begin
  Model := CreateModel;

  FormatSettings := TFormatSettings.Create;
  Assert.AreEqual(FormatSettings.DateSeparator, Model.DateSeparator);
end;

procedure TNewRuleModelTests.LoadDaysBefore;
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Assert.AreEqual(15, Model.DaysBefore);
end;

procedure TNewRuleModelTests.LoadDaysAfter;
var
  Model: IRuleProps;
begin
  Model := CreateModel;

  Assert.AreEqual(60, Model.DaysAfter);
end;

{ TDuplicateEditRuleModelTests }

procedure TDuplicateEditRuleModelTests.Setup;
begin
  inherited;
  FO2Rule := FO2File.Rules.AddRule('');
end;

procedure TDuplicateEditRuleModelTests.LoadRuleTypeIndex(RuleType: TO2RuleType;
  Expected: Integer);
var
  Model: IRuleProps;
begin
  FO2Rule.RuleType := RuleType;

  Model := CreateModel;

  Assert.AreEqual(Expected, Model.RuleTypeIndex);
end;

procedure TDuplicateEditRuleModelTests.LoadFieldNameMask;
var
  Model: IRuleProps;
begin
  FO2Rule.FieldName := 'Original field name mask';

  Model := CreateModel;

  Assert.AreEqual('Original field name mask', Model.FieldNameMask);
end;

procedure TDuplicateEditRuleModelTests.LoadFieldValueMask;
var
  Model: IRuleProps;
begin
  FO2Rule.FieldValue := 'Original field value mask';

  Model := CreateModel;

  Assert.AreEqual('Original field value mask', Model.FieldValueMask);
end;

procedure TDuplicateEditRuleModelTests.LoadHyperLinkMask;
var
  Model: IRuleProps;
begin
  FO2Rule.Params.Values['Mask'] := 'Original hyperlink mask';

  Model := CreateModel;

  Assert.AreEqual('Original hyperlink mask', Model.HyperLinkMask);
end;

procedure TDuplicateEditRuleModelTests.LoadDisplayPasswordStrength(
  const Value: string; Expected: Boolean);
var
  Model: IRuleProps;
begin
  FO2Rule.Params.Values['DisplayPasswordStrength'] := Value;

  Model := CreateModel;

  Assert.AreEqual(Expected, Model.DisplayPasswordStrength);
end;

procedure TDuplicateEditRuleModelTests.LoadDisplayMask;
var
  Model: IRuleProps;
begin
  FO2Rule.Params.Values['DisplayMask'] := 'Original display mask';

  Model := CreateModel;

  Assert.AreEqual('Original display mask', Model.DisplayMask);
end;

procedure TDuplicateEditRuleModelTests.LoadDateFormatIndex(
  const DateFormat: string; Expected: Integer);
var
  Model: IRuleProps;
begin
  FO2Rule.Params.Values['ShortDateFormat'] := DateFormat;

  Model := CreateModel;

  Assert.AreEqual(Expected, Model.DateFormatIndex);
end;

procedure TDuplicateEditRuleModelTests.LoadDateSeparator;
var
  Model: IRuleProps;
begin
  FO2Rule.Params.Values['DateSeparator'] := 'Original date separator';

  Model := CreateModel;

  Assert.AreEqual('Original date separator', Model.DateSeparator);
end;

procedure TDuplicateEditRuleModelTests.LoadDaysBefore;
var
  Model: IRuleProps;
begin
  FO2Rule.Params.Values['DaysBefore'] := '42';

  Model := CreateModel;

  Assert.AreEqual(42, Model.DaysBefore);
end;

procedure TDuplicateEditRuleModelTests.LoadDaysAfter;
var
  Model: IRuleProps;
begin
  FO2Rule.Params.Values['DaysAfter'] := '42';

  Model := CreateModel;

  Assert.AreEqual(42, Model.DaysAfter);
end;

{ TDuplicateRuleModelTests }

function TDuplicateRuleModelTests.CreateModel: IRuleProps;
begin
  Result := TDuplicateRuleModel.Create(FO2File, FO2Rule);
end;

procedure TDuplicateRuleModelTests.LoadRuleName;
var
  Model: IRuleProps;
begin
  FO2Rule.Name := 'Original rule name';

  Model := CreateModel;

  Assert.IsEmpty(Model.RuleName);
end;

{ TEditRuleModelTests }

function TEditRuleModelTests.CreateModel: IRuleProps;
begin
  Result := TEditRuleModel.Create(FO2Rule);
end;

procedure TEditRuleModelTests.LoadRuleName;
var
  Model: IRuleProps;
begin
  FO2Rule.Name := 'Original rule name';

  Model := CreateModel;

  Assert.AreEqual('Original rule name', Model.RuleName);
end;

end.
