unit Testing.PrintModel;

interface

uses
  DUnitX.TestFramework, uO2File;

type
  [TestFixture]
  TPrintModelTests = class
  protected
    FO2File: TO2File;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('NotEmpty', ',Original filename')]
    [TestCase('Empty'   , 'Original title,Original title')]
    procedure LoadTitle(const Title, Expected: string);

    [Test]
    [TestCase('False', 'False')]
    [TestCase('True' , 'True')]
    procedure LoadIncludeTags(Value: Boolean);

    [Test]
    [TestCase('False', 'False')]
    [TestCase('True' , 'True')]
    procedure LoadIncludeNotes(Value: Boolean);

    [Test]
    [TestCase('False', 'False')]
    [TestCase('True' , 'True')]
    procedure LoadIncludeRelations(Value: Boolean);

    [Test]
    [TestCase('False', 'False')]
    [TestCase('True' , 'True')]
    procedure LoadIncludePasswords(Value: Boolean);

    [Test]
    [TestCase('False', 'False')]
    [TestCase('True' , 'True')]
    procedure SaveIncludeTags(Value: Boolean);

    [Test]
    [TestCase('False', 'False')]
    [TestCase('True' , 'True')]
    procedure SaveIncludeNotes(Value: Boolean);

    [Test]
    [TestCase('False', 'False')]
    [TestCase('True' , 'True')]
    procedure SaveIncludeRelations(Value: Boolean);

    [Test]
    [TestCase('False', 'False')]
    [TestCase('True' , 'True')]
    procedure SaveIncludePasswords(Value: Boolean);
  end;

implementation

uses
  uServices, uXmlStorage, uPrintModel;

{ TPrintModelTests }

procedure TPrintModelTests.Setup;
begin
  FO2File := TO2File.Create;
end;

procedure TPrintModelTests.TearDown;
begin
  FO2File.Free;
end;

procedure TPrintModelTests.LoadTitle(const Title, Expected: string);
var
  Storage: IStorage;
  Model: IPrint;
begin
  FO2File.FileName := 'c:\Original filename.o2';
  FO2File.Title := Title;

  Storage := TXmlStorage.Create;

  Model := TPrintModel.Create(FO2File, FO2File.Objects.ToEnumerable, Storage);

  Assert.AreEqual(Expected, Model.Title);
end;

procedure TPrintModelTests.LoadIncludeTags(Value: Boolean);
var
  Storage: IStorage;
  Model: IPrint;
begin
  Storage := TXmlStorage.Create;
  Storage.WriteBoolean('Print.Include.Tags', Value);

  Model := TPrintModel.Create(FO2File, FO2File.Objects.ToEnumerable, Storage);

  Assert.AreEqual(Value, Model.IncludeTags);
end;

procedure TPrintModelTests.LoadIncludeNotes(Value: Boolean);
var
  Storage: IStorage;
  Model: IPrint;
begin
  Storage := TXmlStorage.Create;
  Storage.WriteBoolean('Print.Include.Notes', Value);

  Model := TPrintModel.Create(FO2File, FO2File.Objects.ToEnumerable, Storage);

  Assert.AreEqual(Value, Model.IncludeNotes);
end;

procedure TPrintModelTests.LoadIncludeRelations(Value: Boolean);
var
  Storage: IStorage;
  Model: IPrint;
begin
  Storage := TXmlStorage.Create;
  Storage.WriteBoolean('Print.Include.Relations', Value);

  Model := TPrintModel.Create(FO2File, FO2File.Objects.ToEnumerable, Storage);

  Assert.AreEqual(Value, Model.IncludeRelations);
end;

procedure TPrintModelTests.LoadIncludePasswords(Value: Boolean);
var
  Storage: IStorage;
  Model: IPrint;
begin
  Storage := TXmlStorage.Create;
  Storage.WriteBoolean('Print.Include.Passwords', Value);

  Model := TPrintModel.Create(FO2File, FO2File.Objects.ToEnumerable, Storage);

  Assert.AreEqual(Value, Model.IncludePasswords);
end;

procedure TPrintModelTests.SaveIncludeTags(Value: Boolean);
var
  Storage: IStorage;
  Model: IPrint;
begin
  Storage := TXmlStorage.Create;

  Model := TPrintModel.Create(FO2File, FO2File.Objects.ToEnumerable, Storage);

  Model.IncludeTags := Value;
  Model.StoreSettings;

  Assert.AreEqual(Value, Storage.ReadBoolean('Print.Include.Tags'));
end;

procedure TPrintModelTests.SaveIncludeNotes(Value: Boolean);
var
  Storage: IStorage;
  Model: IPrint;
begin
  Storage := TXmlStorage.Create;

  Model := TPrintModel.Create(FO2File, FO2File.Objects.ToEnumerable, Storage);

  Model.IncludeNotes := Value;
  Model.StoreSettings;

  Assert.AreEqual(Value, Storage.ReadBoolean('Print.Include.Notes'));
end;

procedure TPrintModelTests.SaveIncludeRelations(Value: Boolean);
var
  Storage: IStorage;
  Model: IPrint;
begin
  Storage := TXmlStorage.Create;

  Model := TPrintModel.Create(FO2File, FO2File.Objects.ToEnumerable, Storage);

  Model.IncludeRelations := Value;
  Model.StoreSettings;

  Assert.AreEqual(Value, Storage.ReadBoolean('Print.Include.Relations'));
end;

procedure TPrintModelTests.SaveIncludePasswords(Value: Boolean);
var
  Storage: IStorage;
  Model: IPrint;
begin
  Storage := TXmlStorage.Create;

  Model := TPrintModel.Create(FO2File, FO2File.Objects.ToEnumerable, Storage);

  Model.IncludePasswords := Value;
  Model.StoreSettings;

  Assert.AreEqual(Value, Storage.ReadBoolean('Print.Include.Passwords'));
end;

end.
