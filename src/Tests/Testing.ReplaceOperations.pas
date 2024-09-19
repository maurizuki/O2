unit Testing.ReplaceOperations;

interface

uses
  DUnitX.TestFramework, uO2File;

type
  TReplaceOperationTests = class
  protected
    FO2File: TO2File;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;
  end;

  [TestFixture]
  TReplaceTagModelTests = class(TReplaceOperationTests)
  public
    [Test]
    procedure LoadSearchList;

    [Test]
    procedure LoadReplaceList;

    [Test]
    procedure Valid;

    [Test]
    procedure Replace;
  end;

  [TestFixture]
  TReplaceFieldNameModelTests = class(TReplaceOperationTests)
  public
    [Test]
    procedure LoadSearchList;

    [Test]
    procedure LoadReplaceList;

    [Test]
    procedure Valid;

    [Test]
    procedure Replace;
  end;

  [TestFixture]
  TReplaceFieldValueModelTests = class(TReplaceOperationTests)
  public
    [Test]
    procedure LoadSearchList;

    [Test]
    procedure LoadReplaceList;

    [Test]
    procedure Valid;

    [Test]
    procedure Replace;
  end;

  [TestFixture]
  TReplaceRoleModelTests = class(TReplaceOperationTests)
  public
    [Test]
    procedure LoadSearchList;

    [Test]
    procedure LoadReplaceList;

    [Test]
    procedure Valid;

    [Test]
    procedure Replace;
  end;

implementation

uses
  uServices, uReplaceOperations;

{ TReplaceOperationsTests }

procedure TReplaceOperationTests.Setup;
begin
  FO2File := TO2File.Create;
end;

procedure TReplaceOperationTests.TearDown;
begin
  FO2File.Free;
end;

{ TReplaceTagModelTests }

procedure TReplaceTagModelTests.LoadSearchList;
var
  Model: IReplaceOperation;
begin
  FO2File.Objects.AddObject('Object 1').Tag	:= 'Tag 1,Tag 2';
  FO2File.Objects.AddObject('Object 2').Tag	:= 'tag 2,Tag 3';

  Model := TReplaceTagModel.Create(FO2File, FO2File.Objects.ToEnumerable);

  Assert.AreEqual(3, Model.SearchList.Count);
  Assert.Contains(Model.SearchList, 'Tag 1');
  Assert.Contains(Model.SearchList, 'Tag 2');
  Assert.Contains(Model.SearchList, 'Tag 3');
end;

procedure TReplaceTagModelTests.LoadReplaceList;
var
  Model: IReplaceOperation;
begin
  FO2File.Objects.AddObject('Object 1').Tag	:= 'Tag 1,Tag 2';
  FO2File.Objects.AddObject('Object 2').Tag	:= 'tag 2,Tag 3';

  Model := TReplaceTagModel.Create(FO2File, FO2File.Objects.ToEnumerable);

  Assert.AreEqual(3, Model.ReplaceList.Count);
  Assert.Contains(Model.ReplaceList, 'Tag 1');
  Assert.Contains(Model.ReplaceList, 'Tag 2');
  Assert.Contains(Model.ReplaceList, 'Tag 3');
end;

procedure TReplaceTagModelTests.Valid;
var
  Model: IReplaceOperation;
begin
  FO2File.Objects.AddObject().Tag	:= 'Old tag';

  Model := TReplaceTagModel.Create(FO2File, FO2File.Objects.ToEnumerable);

  Model.SearchValue := 'Old tag';
  Model.ReplaceValue := 'New tag';

  Assert.IsTrue(Model.Valid);
end;

procedure TReplaceTagModelTests.Replace;
var
  Model: IReplaceOperation;
begin
  FO2File.Objects.AddObject('Object 1').Tag	:= 'Tag 1,Old tag';
  FO2File.Objects.AddObject('Object 2').Tag	:= 'old tag';

  Model := TReplaceTagModel.Create(FO2File, FO2File.Objects.ToEnumerable);

  Model.SearchValue := 'Old tag';
  Model.ReplaceValue := 'New tag';
  Model.Replace;

  Assert.AreEqual('New tag,Tag 1', FO2File.Objects.FindObject('Object 1').Tag);
  Assert.AreEqual('New tag', FO2File.Objects.FindObject('Object 2').Tag);
end;

{ TReplaceFieldNameModelTests }

procedure TReplaceFieldNameModelTests.LoadSearchList;
var
  Model: IReplaceOperation;
begin
  with FO2File.Objects.AddObject('Object 1') do
  begin
    Fields.AddField('Field 1');
    Fields.AddField('Field 2');
  end;
  with FO2File.Objects.AddObject('Object 2') do
  begin
    Fields.AddField('field 2');
    Fields.AddField('Field 3');
  end;

  Model := TReplaceFieldNameModel.Create(FO2File, FO2File.Objects.ToEnumerable);

  Assert.AreEqual(3, Model.SearchList.Count);
  Assert.Contains(Model.SearchList, 'Field 1');
  Assert.Contains(Model.SearchList, 'Field 2');
  Assert.Contains(Model.SearchList, 'Field 3');
end;

procedure TReplaceFieldNameModelTests.LoadReplaceList;
var
  Model: IReplaceOperation;
begin
  with FO2File.Objects.AddObject('Object 1') do
  begin
    Fields.AddField('Field 1');
    Fields.AddField('Field 2');
  end;
  with FO2File.Objects.AddObject('Object 2') do
  begin
    Fields.AddField('field 2');
    Fields.AddField('Field 3');
  end;

  Model := TReplaceFieldNameModel.Create(FO2File, FO2File.Objects.ToEnumerable);

  Assert.AreEqual(3, Model.ReplaceList.Count);
  Assert.Contains(Model.ReplaceList, 'Field 1');
  Assert.Contains(Model.ReplaceList, 'Field 2');
  Assert.Contains(Model.ReplaceList, 'Field 3');
end;

procedure TReplaceFieldNameModelTests.Valid;
var
  Model: IReplaceOperation;
begin
  FO2File.Objects.AddObject().Fields.AddField('Old field');

  Model := TReplaceFieldNameModel.Create(FO2File, FO2File.Objects.ToEnumerable);

  Model.SearchValue := 'Old field';
  Model.ReplaceValue := 'New field';

  Assert.IsTrue(Model.Valid);
end;

procedure TReplaceFieldNameModelTests.Replace;
var
  Model: IReplaceOperation;
begin
  with FO2File.Objects.AddObject('Object 1') do
  begin
    Fields.AddField('Field 1');
    Fields.AddField('Old field');
  end;
  FO2File.Objects.AddObject('Object 2').Fields.AddField('old field');

  Model := TReplaceFieldNameModel.Create(FO2File, FO2File.Objects.ToEnumerable);

  Model.SearchValue := 'Old field';
  Model.ReplaceValue := 'New field';
  Model.Replace;

  Assert.AreEqual('Field 1',
    FO2File.Objects.FindObject('Object 1').Fields[0].FieldName);
  Assert.AreEqual('New field',
    FO2File.Objects.FindObject('Object 1').Fields[1].FieldName);
  Assert.AreEqual('New field',
    FO2File.Objects.FindObject('Object 2').Fields[0].FieldName);
end;

{ TReplaceFieldValueModelTests }

procedure TReplaceFieldValueModelTests.LoadSearchList;
var
  Model: IReplaceOperation;
begin
  with FO2File.Objects.AddObject('Object 1') do
  begin
    Fields.AddField('Field 1');
    Fields.AddField('Field 2');
  end;
  with FO2File.Objects.AddObject('Object 2') do
  begin
    Fields.AddField('field 2');
    Fields.AddField('Field 3');
  end;

  Model := TReplaceFieldValueModel.Create(FO2File,
    FO2File.Objects.ToEnumerable);

  Assert.AreEqual(3, Model.SearchList.Count);
  Assert.Contains(Model.SearchList, 'Field 1');
  Assert.Contains(Model.SearchList, 'Field 2');
  Assert.Contains(Model.SearchList, 'Field 3');
end;

procedure TReplaceFieldValueModelTests.LoadReplaceList;
var
  Model: IReplaceOperation;
begin
  with FO2File.Objects.AddObject('Object 1') do
  begin
    Fields.AddField('Field 1').FieldValue := 'Value 1';
    Fields.AddField('Field 2').FieldValue := 'Value 2';
  end;
  with FO2File.Objects.AddObject('Object 2') do
  begin
    Fields.AddField('Field 1').FieldValue := 'value 2';
    Fields.AddField('Field 2').FieldValue := 'Value 3';
  end;

  Model := TReplaceFieldValueModel.Create(FO2File,
    FO2File.Objects.ToEnumerable);

  Assert.AreEqual(3, Model.ReplaceList.Count);
  Assert.Contains(Model.ReplaceList, 'Value 1');
  Assert.Contains(Model.ReplaceList, 'Value 2');
  Assert.Contains(Model.ReplaceList, 'Value 3');
end;

procedure TReplaceFieldValueModelTests.Valid;
var
  Model: IReplaceOperation;
begin
  FO2File.Objects.AddObject().Fields.AddField('Target field');

  Model := TReplaceFieldValueModel.Create(FO2File,
    FO2File.Objects.ToEnumerable);

  Model.SearchValue := 'Target field';
  Model.ReplaceValue := 'New value';

  Assert.IsTrue(Model.Valid);
end;

procedure TReplaceFieldValueModelTests.Replace;
var
  Model: IReplaceOperation;
begin
  with FO2File.Objects.AddObject('Object 1') do
  begin
    Fields.AddField('Field 1').FieldValue := 'Value 1';
    Fields.AddField('Target field').FieldValue := 'Old value';
  end;
  FO2File.Objects.AddObject('Object 2').Fields.AddField('target field')
    .FieldValue := 'Old value';

  Model := TReplaceFieldValueModel.Create(FO2File,
    FO2File.Objects.ToEnumerable);

  Model.SearchValue := 'Target field';
  Model.ReplaceValue := 'New value';
  Model.Replace;

  Assert.AreEqual('Value 1',
    FO2File.Objects.FindObject('Object 1').Fields[0].FieldValue);
  Assert.AreEqual('New value',
    FO2File.Objects.FindObject('Object 1').Fields[1].FieldValue);
  Assert.AreEqual('New value',
    FO2File.Objects.FindObject('Object 2').Fields[0].FieldValue);
end;

{ TReplaceRoleModelTests }

procedure TReplaceRoleModelTests.LoadSearchList;
var
  Model: IReplaceOperation;
begin
  FO2File.Objects.AddObject('Object 1');
  FO2File.Objects.AddObject('Object 2');
  FO2File.Objects.AddObject('Object 3');
  with FO2File.Relations.AddRelation do
  begin
    ObjectID1 := FO2File.Objects.FindObject('Object 1').ObjectID;
    Role1 := 'Role 1';
    ObjectID2 := FO2File.Objects.FindObject('Object 2').ObjectID;
    Role2 := 'Role 2';
  end;
  with FO2File.Relations.AddRelation do
  begin
    ObjectID1 := FO2File.Objects.FindObject('Object 2').ObjectID;
    Role1 := 'role 2';
    ObjectID2 := FO2File.Objects.FindObject('Object 3').ObjectID;
    Role2 := 'Role 3';
  end;

  Model := TReplaceRoleModel.Create(FO2File, FO2File.Objects.ToEnumerable);

  Assert.AreEqual(3, Model.SearchList.Count);
  Assert.Contains(Model.SearchList, 'Role 1');
  Assert.Contains(Model.SearchList, 'Role 2');
  Assert.Contains(Model.SearchList, 'Role 3');
end;

procedure TReplaceRoleModelTests.LoadReplaceList;
var
  Model: IReplaceOperation;
begin
  FO2File.Objects.AddObject('Object 1');
  FO2File.Objects.AddObject('Object 2');
  FO2File.Objects.AddObject('Object 3');
  with FO2File.Relations.AddRelation do
  begin
    ObjectID1 := FO2File.Objects.FindObject('Object 1').ObjectID;
    Role1 := 'Role 1';
    ObjectID2 := FO2File.Objects.FindObject('Object 2').ObjectID;
    Role2 := 'Role 2';
  end;
  with FO2File.Relations.AddRelation do
  begin
    ObjectID1 := FO2File.Objects.FindObject('Object 2').ObjectID;
    Role1 := 'role 2';
    ObjectID2 := FO2File.Objects.FindObject('Object 3').ObjectID;
    Role2 := 'Role 3';
  end;

  Model := TReplaceRoleModel.Create(FO2File, FO2File.Objects.ToEnumerable);

  Assert.AreEqual(3, Model.ReplaceList.Count);
  Assert.Contains(Model.ReplaceList, 'Role 1');
  Assert.Contains(Model.ReplaceList, 'Role 2');
  Assert.Contains(Model.ReplaceList, 'Role 3');
end;

procedure TReplaceRoleModelTests.Valid;
var
  Model: IReplaceOperation;
begin
  FO2File.Objects.AddObject('Object 1');
  FO2File.Objects.AddObject('Object 2');
  with FO2File.Relations.AddRelation do
  begin
    ObjectID1 := FO2File.Objects.FindObject('Object 1').ObjectID;
    Role1 := 'Old role';
    ObjectID2 := FO2File.Objects.FindObject('Object 2').ObjectID;
    Role2 := 'Role 2';
  end;

  Model := TReplaceRoleModel.Create(FO2File, FO2File.Objects.ToEnumerable);

  Model.SearchValue := 'Old role';
  Model.ReplaceValue := 'New role';

  Assert.IsTrue(Model.Valid);
end;

procedure TReplaceRoleModelTests.Replace;
var
  Model: IReplaceOperation;
begin
  FO2File.Objects.AddObject('Object 1');
  FO2File.Objects.AddObject('Object 2');
  FO2File.Objects.AddObject('Object 3');
  with FO2File.Relations.AddRelation do
  begin
    ObjectID1 := FO2File.Objects.FindObject('Object 1').ObjectID;
    Role1 := 'Role 1';
    ObjectID2 := FO2File.Objects.FindObject('Object 2').ObjectID;
    Role2 := 'Old role';
  end;
  with FO2File.Relations.AddRelation do
  begin
    ObjectID1 := FO2File.Objects.FindObject('Object 2').ObjectID;
    Role1 := 'Old role';
    ObjectID2 := FO2File.Objects.FindObject('Object 3').ObjectID;
    Role2 := 'Role 2';
  end;

  Model := TReplaceRoleModel.Create(FO2File, FO2File.Objects.ToEnumerable);

  Model.SearchValue := 'Old role';
  Model.ReplaceValue := 'New role';
  Model.Replace;

  Assert.AreEqual('Role 1', FO2File.Relations[0].Role1);
  Assert.AreEqual('New role', FO2File.Relations[0].Role2);
  Assert.AreEqual('New role', FO2File.Relations[1].Role1);
  Assert.AreEqual('Role 2', FO2File.Relations[1].Role2);
end;

end.
