unit Testing.ObjectModels;

interface

uses
  DUnitX.TestFramework, uO2File, uO2Objects, uServices;

type
  TObjectPropsModelTests = class
  protected
    FO2File: TO2File;
    FO2Object: TO2Object;

    function CreateModel: IObjectProps; virtual; abstract;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure LoadTags;

    [Test]
    procedure SaveObjectName;

    [Test]
    procedure SaveObjectTags;
  end;

  [TestFixture]
  TNewObjectModelTests = class(TObjectPropsModelTests)
  protected
    function CreateModel: IObjectProps; override;
  public
    [Test]
    procedure LoadObjectName;

    [Test]
    procedure LoadObjectTags;
  end;

  TDuplicateEditObjectModelTests = class(TObjectPropsModelTests)
  public
    [Test]
    procedure LoadObjectTags;
  end;

  [TestFixture]
  TDuplicateObjectModelTests = class(TDuplicateEditObjectModelTests)
  protected
    function CreateModel: IObjectProps; override;
  public
    [Test]
    procedure LoadObjectName;
  end;

  [TestFixture]
  TEditObjectModelTests = class(TDuplicateEditObjectModelTests)
  protected
    function CreateModel: IObjectProps; override;
  public
    [Test]
    procedure LoadObjectName;
  end;

implementation

uses
  uObjectModels;

{ TObjectPropsModelTests }

procedure TObjectPropsModelTests.Setup;
begin
  FO2File := TO2File.Create;
  FO2Object := FO2File.Objects.AddObject();
end;

procedure TObjectPropsModelTests.TearDown;
begin
  FO2File.Free;
end;

procedure TObjectPropsModelTests.LoadTags;
var
  Model: IObjectProps;
begin
  FO2File.Objects.AddObject.Tag := 'Tag 1';
  FO2File.Objects.AddObject.Tag := 'tag 1,Tag 2';
  FO2File.Objects.AddObject.Tag := 'Tag 2,Tag 3';

  Model := CreateModel;

  Assert.AreEqual(3, Model.Tags.Count);
  Assert.Contains(Model.Tags, 'Tag 1');
  Assert.Contains(Model.Tags, 'Tag 2');
  Assert.Contains(Model.Tags, 'Tag 3');
end;

procedure TObjectPropsModelTests.SaveObjectName;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Model.ObjectName := 'New object name';
  Model.ApplyChanges;

  Assert.AreEqual('New object name', Model.O2Object.Name);
end;

procedure TObjectPropsModelTests.SaveObjectTags;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Model.ObjectTags.Add('Tag 1');
  Model.ObjectTags.Add('Tag 2');
  Model.ObjectTags.Add('Tag 3');
  Model.ApplyChanges;

  Assert.AreEqual('Tag 1,Tag 2,Tag 3', Model.O2Object.Tag);
end;

{ TNewObjectModelTests }

function TNewObjectModelTests.CreateModel: IObjectProps;
begin
  Result := TNewObjectModel.Create(FO2File);
end;

procedure TNewObjectModelTests.LoadObjectName;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Assert.IsEmpty(Model.ObjectName);
end;

procedure TNewObjectModelTests.LoadObjectTags;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Assert.IsEmpty(Model.ObjectTags);
end;

{ TDuplicateEditObjectModelTests }

procedure TDuplicateEditObjectModelTests.LoadObjectTags;
var
  Model: IObjectProps;
begin
  FO2Object.Tag := 'Tag 1,Tag 2,Tag 3';

  Model := CreateModel;

  Assert.AreEqual(3, Model.ObjectTags.Count);
  Assert.Contains(Model.ObjectTags, 'Tag 1');
  Assert.Contains(Model.ObjectTags, 'Tag 2');
  Assert.Contains(Model.ObjectTags, 'Tag 3');
end;

{ TDuplicateObjectModelTests }

function TDuplicateObjectModelTests.CreateModel: IObjectProps;
begin
  Result := TDuplicateObjectModel.Create(FO2File, FO2Object);
end;

procedure TDuplicateObjectModelTests.LoadObjectName;
var
  Model: IObjectProps;
begin
  FO2Object.Name := 'Original object name';

  Model := CreateModel;

  Assert.IsEmpty(Model.ObjectName);
end;

{ TEditObjectModelTests }

function TEditObjectModelTests.CreateModel: IObjectProps;
begin
  Result := TEditObjectModel.Create(FO2File, FO2Object);
end;

procedure TEditObjectModelTests.LoadObjectName;
var
  Model: IObjectProps;
begin
  FO2Object.Name := 'Original object name';

  Model := CreateModel;

  Assert.AreEqual('Original object name', Model.ObjectName);
end;

end.
