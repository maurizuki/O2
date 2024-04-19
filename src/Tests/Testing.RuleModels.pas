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
  end;

  [TestFixture]
  TNewRuleModelTests = class(TRulePropsModelTests)
  protected
    function CreateModel: IRuleProps; override;
  public
    [Test]
    procedure LoadRuleName;

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
  end;

  TDuplicateEditRuleModelTests = class(TRulePropsModelTests)
  protected
    FO2Rule: TO2Rule;
  public
    [Setup]
    procedure Setup; override;

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
  uRuleModels;

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
  Model: IRuleProps;
begin
  Model := CreateModel;

  Assert.AreEqual(6, Model.RuleTypes.Count);
  Assert.Contains(Model.RuleTypes, 'Internet link');
  Assert.Contains(Model.RuleTypes, 'E-mail address');
  Assert.Contains(Model.RuleTypes, 'Password');
  Assert.Contains(Model.RuleTypes, 'Expiration date');
  Assert.Contains(Model.RuleTypes, 'Recurrence');
  Assert.Contains(Model.RuleTypes, 'Highlight');
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

{ TDuplicateEditRuleModelTests }

procedure TDuplicateEditRuleModelTests.Setup;
begin
  inherited;
  FO2Rule := FO2File.Rules.AddRule('');
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
