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
  end;

  [TestFixture]
  TNewRuleModelTests = class(TRulePropsModelTests)
  protected
    function CreateModel: IRuleProps; override;
  public
    [Test]
    procedure LoadRuleName;
  end;

  TDuplicateEditRuleModelTests = class(TRulePropsModelTests)
  protected
    FO2Rule: TO2Rule;
  public
    [Setup]
    procedure Setup; override;
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

{ TDuplicateEditRuleModelTests }

procedure TDuplicateEditRuleModelTests.Setup;
begin
  inherited;
  FO2Rule := FO2File.Rules.AddRule('');
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
