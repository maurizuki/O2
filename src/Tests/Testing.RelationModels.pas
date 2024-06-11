unit Testing.RelationModels;

interface

uses
  DUnitX.TestFramework, uO2File, uO2Relations, uServices;

type
  TRelationPropsModelTests = class
  protected
    FO2File: TO2File;

    function CreateModel: IRelationProps; virtual; abstract;
  public
    [Setup]
    procedure Setup; virtual;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure LoadObjectName1;

    [Test]
    procedure LoadObjectName2;

    [Test]
    procedure LoadRoles;

    [Test]
    procedure SaveRole1;

    [Test]
    procedure SaveRole2;
  end;

  [TestFixture]
  TNewRelationModelTests = class(TRelationPropsModelTests)
  protected
    function CreateModel: IRelationProps; override;
  public
    [Test]
    procedure LoadRole1;

    [Test]
    procedure LoadRole2;
  end;

  [TestFixture]
  TEditRelationModelTests = class(TRelationPropsModelTests)
  protected
    FO2Relation: TO2Relation;

    function CreateModel: IRelationProps; override;
  public
    [Setup]
    procedure Setup; override;

    [Test]
    procedure LoadRole1;

    [Test]
    procedure LoadRole2;
  end;

implementation

uses
  uRelationModels;

{ TRelationPropsModelTests }

procedure TRelationPropsModelTests.Setup;
begin
  FO2File := TO2File.Create;
  FO2File.Objects.AddObject('Object 1');
  FO2File.Objects.AddObject('Object 2');
end;

procedure TRelationPropsModelTests.TearDown;
begin
  FO2File.Free;
end;

procedure TRelationPropsModelTests.LoadObjectName1;
var
  Model: IRelationProps;
begin
  Model := CreateModel;

  Assert.AreEqual('Object 1', Model.ObjectName1);
end;

procedure TRelationPropsModelTests.LoadObjectName2;
var
  Model: IRelationProps;
begin
  Model := CreateModel;

  Assert.AreEqual('Object 2', Model.ObjectName2);
end;

procedure TRelationPropsModelTests.LoadRoles;
var
  Model: IRelationProps;
begin
  FO2File.Objects.AddObject('Object 3');
  with FO2File.Relations.AddRelation do
  begin
    ObjectID1 := FO2File.Objects[0].ObjectID;
    Role1 := 'Role 1';
    ObjectID2 := FO2File.Objects[2].ObjectID;
    Role2 := 'Role 3';
  end;
  with FO2File.Relations.AddRelation do
  begin
    ObjectID1 := FO2File.Objects[1].ObjectID;
    Role1 := 'role 2';
    ObjectID2 := FO2File.Objects[2].ObjectID;
    Role2 := 'Role 3';
  end;

  Model := CreateModel;

  Assert.AreEqual(3, Model.Roles.Count);
  Assert.Contains(Model.Roles, 'Role 1');
  Assert.Contains(Model.Roles, 'Role 2');
  Assert.Contains(Model.Roles, 'Role 3');
end;

procedure TRelationPropsModelTests.SaveRole1;
var
  Model: IRelationProps;
begin
  Model := CreateModel;

  Model.Role1 := 'New role';
  Model.ApplyChanges;

  Assert.AreEqual('New role', Model.Relation.Role1);
end;

procedure TRelationPropsModelTests.SaveRole2;
var
  Model: IRelationProps;
begin
  Model := CreateModel;

  Model.Role2 := 'New role';
  Model.ApplyChanges;

  Assert.AreEqual('New role', Model.Relation.Role2);
end;

{ TNewRelationModelTests }

function TNewRelationModelTests.CreateModel: IRelationProps;
begin
  Result := TNewRelationModel.Create(FO2File, FO2File.Objects.ToEnumerable);
end;

procedure TNewRelationModelTests.LoadRole1;
var
  Model: IRelationProps;
begin
  Model := CreateModel;

  Assert.IsEmpty(Model.Role1);
end;

procedure TNewRelationModelTests.LoadRole2;
var
  Model: IRelationProps;
begin
  Model := CreateModel;

  Assert.IsEmpty(Model.Role1);
end;

{ TEditRelationModelTests }

function TEditRelationModelTests.CreateModel: IRelationProps;
begin
  Result := TEditRelationModel.Create(FO2File, FO2Relation);
end;

procedure TEditRelationModelTests.Setup;
begin
  inherited;
  FO2Relation := FO2File.Relations.AddRelation;
  FO2Relation.ObjectID1 := FO2File.Objects[0].ObjectID;
  FO2Relation.Role1 := 'Role 1';
  FO2Relation.ObjectID2 := FO2File.Objects[1].ObjectID;
  FO2Relation.Role2 := 'Role 2';
end;

procedure TEditRelationModelTests.LoadRole1;
var
  Model: IRelationProps;
begin
  Model := CreateModel;

  Assert.AreEqual('Role 1', Model.Role1);
end;

procedure TEditRelationModelTests.LoadRole2;
var
  Model: IRelationProps;
begin
  Model := CreateModel;

  Assert.AreEqual('Role 2', Model.Role2);
end;

end.
