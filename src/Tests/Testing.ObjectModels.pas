unit Testing.ObjectModels;

interface

uses
  DUnitX.TestFramework, uO2File, uO2Objects, uServices;

{ TODO -omaurizuki -cTest : Test AddField method }
{ TODO -omaurizuki -cTest : Test ReplaceField method }
{ TODO -omaurizuki -cTest : Test DeleteField method }
{ TODO -omaurizuki -cTest : Test SwapFields method }

type
  TObjectPropsModelTests = class
  protected
    FO2File: TO2File;

    function CreateModel: IObjectProps; virtual; abstract;
  public
    [Setup]
    procedure Setup; virtual;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure LoadTags;

    [Test]
    procedure LoadFieldNames;

    [Test]
    procedure LoadFieldValues;

    [Test]
    procedure SaveObjectName;

    [Test]
    procedure SaveObjectTags;

    [Test]
    procedure SaveObjectFieldNames;

    [Test]
    procedure SaveObjectFieldValues;

    [Test]
    procedure SaveObjectNotes;

    [Test]
    [TestCase('False', 'False,ttPlainText')]
    [TestCase('True' , 'True,ttCommonMark')]
    procedure SaveMarkdown(Markdown: Boolean; Expected: TO2TextType);

    [Test]
    procedure CanAddField;

    [Test]
    procedure CannotAddFieldEmptyFieldName;

    [Test]
    procedure CannotAddFieldDuplicatedFieldName;

    [Test]
    procedure CanReplaceField;

    [Test]
    procedure CannotReplaceFieldEmptyFieldName;

    [Test]
    procedure CannotReplaceFieldDuplicatedFieldName;

    [Test]
    procedure CanDeleteField;

    [Test]
    procedure CannotDeleteFieldEmptyFields;

    [Test]
    procedure Valid;

    [Test]
    procedure NotValidEmptyObjectName;
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

    [Test]
    procedure LoadObjectFieldNames;

    [Test]
    procedure LoadObjectFieldValues;

    [Test]
    procedure LoadObjectNotes;

    [Test]
    procedure LoadMarkdown;
  end;

  TDuplicateEditObjectModelTests = class(TObjectPropsModelTests)
  protected
    FO2Object: TO2Object;
  public
    [Setup]
    procedure Setup; override;

    [Test]
    procedure LoadObjectTags;

    [Test]
    procedure LoadObjectFieldNames;

    [Test]
    procedure LoadObjectFieldValues;

    [Test]
    procedure LoadObjectNotes;

    [Test]
    [TestCase('False', 'ttPlainText,False')]
    [TestCase('True' , 'ttCommonMark,True')]
    procedure LoadMarkdown(TextType: TO2TextType; Expected: Boolean);
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
  Classes, uObjectModels;

{ TObjectPropsModelTests }

procedure TObjectPropsModelTests.Setup;
begin
  FO2File := TO2File.Create;
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

procedure TObjectPropsModelTests.LoadFieldNames;
var
  Model: IObjectProps;
begin
  with FO2File.Objects.AddObject do
    Fields.AddField('Field 1');
  with FO2File.Objects.AddObject do
  begin
    Fields.AddField('field 1');
    Fields.AddField('Field 2');
  end;
  with FO2File.Objects.AddObject do
  begin
    Fields.AddField('Field 2');
    Fields.AddField('Field 3');
  end;

  Model := CreateModel;

  Assert.AreEqual(3, Model.FieldNames.Count);
  Assert.Contains(Model.FieldNames, 'Field 1');
  Assert.Contains(Model.FieldNames, 'Field 2');
  Assert.Contains(Model.FieldNames, 'Field 3');
end;

procedure TObjectPropsModelTests.LoadFieldValues;
var
  Model: IObjectProps;
begin
  with FO2File.Objects.AddObject do
    Fields.AddField('Field 1').FieldValue := 'Value 1';
  with FO2File.Objects.AddObject do
  begin
    Fields.AddField('Field 1').FieldValue := 'value 1';
    Fields.AddField('Field 2').FieldValue := 'Value 2';
  end;
  with FO2File.Objects.AddObject do
  begin
    Fields.AddField('Field 1').FieldValue := 'Value 2';
    Fields.AddField('Field 2').FieldValue := 'Value 3';
  end;

  Model := CreateModel;

  Assert.AreEqual(3, Model.FieldValues.Count);
  Assert.Contains(Model.FieldValues, 'Value 1');
  Assert.Contains(Model.FieldValues, 'Value 2');
  Assert.Contains(Model.FieldValues, 'Value 3');
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

  Assert.Contains(Model.O2Object.Tag, 'Tag 1');
  Assert.Contains(Model.O2Object.Tag, 'Tag 2');
  Assert.Contains(Model.O2Object.Tag, 'Tag 3');
end;

procedure TObjectPropsModelTests.SaveObjectFieldNames;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Model.FieldName := 'Field 1';
  Model.AddField;
  Model.FieldName := 'Field 2';
  Model.AddField;
  Model.FieldName := 'Field 3';
  Model.AddField;
  Model.ApplyChanges;

  Assert.AreEqual(3, Model.O2Object.Fields.Count);
  Assert.AreEqual('Field 1', Model.O2Object.Fields[0].FieldName);
  Assert.AreEqual('Field 2', Model.O2Object.Fields[1].FieldName);
  Assert.AreEqual('Field 3', Model.O2Object.Fields[2].FieldName);
end;

procedure TObjectPropsModelTests.SaveObjectFieldValues;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Model.FieldValue := 'Value 1';
  Model.AddField;
  Model.FieldValue := 'Value 2';
  Model.AddField;
  Model.FieldValue := 'Value 3';
  Model.AddField;
  Model.ApplyChanges;

  Assert.AreEqual(3, Model.O2Object.Fields.Count);
  Assert.AreEqual('Value 1', Model.O2Object.Fields[0].FieldValue);
  Assert.AreEqual('Value 2', Model.O2Object.Fields[1].FieldValue);
  Assert.AreEqual('Value 3', Model.O2Object.Fields[2].FieldValue);
end;

procedure TObjectPropsModelTests.SaveObjectNotes;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Model.ObjectNotes.Text := 'New object notes';
  Model.ApplyChanges;

  Assert.AreEqual('New object notes'#13#10, Model.O2Object.Text.Text);
end;

procedure TObjectPropsModelTests.SaveMarkdown(Markdown: Boolean;
  Expected: TO2TextType);
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Model.Markdown := Markdown;
  Model.ApplyChanges;

  Assert.AreEqual(Expected, Model.O2Object.TextType);
end;

procedure TObjectPropsModelTests.CanAddField;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Model.FieldName := 'Valid field name';

  Assert.IsTrue(Model.CanAddField);
end;

procedure TObjectPropsModelTests.CannotAddFieldEmptyFieldName;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Model.FieldName := '';

  Assert.IsFalse(Model.CanAddField);
end;

procedure TObjectPropsModelTests.CannotAddFieldDuplicatedFieldName;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Model.FieldName := 'Valid field name';
  Model.AddField;
  Model.FieldName := 'Valid field name';

  Assert.IsFalse(Model.CanAddField);
end;

procedure TObjectPropsModelTests.CanReplaceField;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Model.FieldName := 'Valid field name';
  Model.AddField;
  Model.FieldName := 'Another field name';

  Assert.IsTrue(Model.CanReplaceField);
end;

procedure TObjectPropsModelTests.CannotReplaceFieldEmptyFieldName;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Model.FieldName := '';

  Assert.IsFalse(Model.CanReplaceField);
end;

procedure TObjectPropsModelTests.CannotReplaceFieldDuplicatedFieldName;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Model.FieldName := 'Valid field name';
  Model.AddField;
  Model.FieldName := 'Another field name';
  Model.AddField;
  Model.FieldName := 'Valid field name';

  Assert.IsFalse(Model.CanReplaceField);
end;

procedure TObjectPropsModelTests.CanDeleteField;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Model.FieldName := 'Valid field name';
  Model.AddField;

  Assert.IsTrue(Model.CanDeleteField);
end;

procedure TObjectPropsModelTests.CannotDeleteFieldEmptyFields;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Assert.IsFalse(Model.CanDeleteField);
end;

procedure TObjectPropsModelTests.Valid;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Model.ObjectName := 'Valid object name';

  Assert.IsTrue(Model.Valid);
end;

procedure TObjectPropsModelTests.NotValidEmptyObjectName;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Model.ObjectName := '';

  Assert.IsFalse(Model.Valid);
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

procedure TNewObjectModelTests.LoadObjectFieldNames;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Assert.AreEqual(0, Model.FieldCount);
end;

procedure TNewObjectModelTests.LoadObjectFieldValues;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Assert.AreEqual(0, Model.FieldCount);
end;

procedure TNewObjectModelTests.LoadObjectNotes;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Assert.IsEmpty(Model.ObjectNotes);
end;

procedure TNewObjectModelTests.LoadMarkdown;
var
  Model: IObjectProps;
begin
  Model := CreateModel;

  Assert.IsFalse(Model.Markdown);
end;

{ TDuplicateEditObjectModelTests }

procedure TDuplicateEditObjectModelTests.Setup;
begin
  inherited;
  FO2Object := FO2File.Objects.AddObject();
end;

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

procedure TDuplicateEditObjectModelTests.LoadObjectFieldNames;
var
  Model: IObjectProps;
begin
  FO2Object.Fields.AddField('Field 1');
  FO2Object.Fields.AddField('Field 2');
  FO2Object.Fields.AddField('Field 3');

  Model := CreateModel;

  Assert.AreEqual(3, Model.FieldCount);
  Assert.AreEqual('Field 1', Model.ObjectFieldNames[0]);
  Assert.AreEqual('Field 2', Model.ObjectFieldNames[1]);
  Assert.AreEqual('Field 3', Model.ObjectFieldNames[2]);
end;

procedure TDuplicateEditObjectModelTests.LoadObjectFieldValues;
var
  Model: IObjectProps;
begin
  FO2Object.Fields.AddField('Field 1').FieldValue := 'Value 1';
  FO2Object.Fields.AddField('Field 2').FieldValue := 'Value 2';
  FO2Object.Fields.AddField('Field 3').FieldValue := 'Value 3';

  Model := CreateModel;

  Assert.AreEqual(3, Model.FieldCount);
  Assert.AreEqual('Value 1', Model.ObjectFieldValues[0]);
  Assert.AreEqual('Value 2', Model.ObjectFieldValues[1]);
  Assert.AreEqual('Value 3', Model.ObjectFieldValues[2]);
end;

procedure TDuplicateEditObjectModelTests.LoadObjectNotes;
var
  Model: IObjectProps;
begin
  FO2Object.Text.Text := 'Original object notes';

  Model := CreateModel;

  Assert.AreEqual('Original object notes'#13#10, Model.ObjectNotes.Text);
end;

procedure TDuplicateEditObjectModelTests.LoadMarkdown(TextType: TO2TextType;
  Expected: Boolean);
var
  Model: IObjectProps;
begin
  FO2Object.TextType := TextType;

  Model := CreateModel;

  Assert.AreEqual(Expected, Model.Markdown);
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
