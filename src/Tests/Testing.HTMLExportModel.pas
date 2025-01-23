unit Testing.HTMLExportModel;

interface

uses
  DUnitX.TestFramework, uUtils, uO2File;

type
  [TestFixture]
  THTMLExportModelTests = class
  private
    FAppVersionInfo: TAppVersionInfo;
    FO2File: TO2File;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('False', 'False')]
    [TestCase('True' , 'True')]
    procedure LoadIncludeIndex(Value: Boolean);

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
    procedure SaveIncludeIndex(Value: Boolean);

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
  uServices, uAppFiles, uXmlStorage, uHTMLExportModel;

{ THTMLExportModelTests }

procedure THTMLExportModelTests.Setup;
begin
  FO2File := TO2File.Create;
end;

procedure THTMLExportModelTests.TearDown;
begin
  FO2File.Free;
end;

procedure THTMLExportModelTests.LoadIncludeIndex(Value: Boolean);
var
  Storage: IStorage;
  Model: IHTMLExport;
begin
  Storage := TXmlStorage.Create;
  Storage.WriteBoolean('ExportToHTML.Include.Index', Value);

  Model := THTMLExportModel.Create(FO2File, FO2File.Objects.ToEnumerable,
    FAppVersionInfo, TAppFiles.Create, Storage);

  Assert.AreEqual(Value, Model.IncludeIndex);
end;

procedure THTMLExportModelTests.LoadIncludeTags(Value: Boolean);
var
  Storage: IStorage;
  Model: IHTMLExport;
begin
  Storage := TXmlStorage.Create;
  Storage.WriteBoolean('ExportToHTML.Include.Tags', Value);

  Model := THTMLExportModel.Create(FO2File, FO2File.Objects.ToEnumerable,
    FAppVersionInfo, TAppFiles.Create, Storage);

  Assert.AreEqual(Value, Model.IncludeTags);
end;

procedure THTMLExportModelTests.LoadIncludeNotes(Value: Boolean);
var
  Storage: IStorage;
  Model: IHTMLExport;
begin
  Storage := TXmlStorage.Create;
  Storage.WriteBoolean('ExportToHTML.Include.Notes', Value);

  Model := THTMLExportModel.Create(FO2File, FO2File.Objects.ToEnumerable,
    FAppVersionInfo, TAppFiles.Create, Storage);

  Assert.AreEqual(Value, Model.IncludeNotes);
end;

procedure THTMLExportModelTests.LoadIncludeRelations(Value: Boolean);
var
  Storage: IStorage;
  Model: IHTMLExport;
begin
  Storage := TXmlStorage.Create;
  Storage.WriteBoolean('ExportToHTML.Include.Relations', Value);

  Model := THTMLExportModel.Create(FO2File, FO2File.Objects.ToEnumerable,
    FAppVersionInfo, TAppFiles.Create, Storage);

  Assert.AreEqual(Value, Model.IncludeRelations);
end;

procedure THTMLExportModelTests.LoadIncludePasswords(Value: Boolean);
var
  Storage: IStorage;
  Model: IHTMLExport;
begin
  Storage := TXmlStorage.Create;
  Storage.WriteBoolean('ExportToHTML.Include.Passwords', Value);

  Model := THTMLExportModel.Create(FO2File, FO2File.Objects.ToEnumerable,
    FAppVersionInfo, TAppFiles.Create, Storage);

  Assert.AreEqual(Value, Model.IncludePasswords);
end;

procedure THTMLExportModelTests.SaveIncludeIndex(Value: Boolean);
var
  Storage: IStorage;
  Model: IHTMLExport;
begin
  Storage := TXmlStorage.Create;

  Model := THTMLExportModel.Create(FO2File, FO2File.Objects.ToEnumerable,
    FAppVersionInfo, TAppFiles.Create, Storage);

  Model.IncludeIndex := Value;
  Model.StoreSettings;

  Assert.AreEqual(Value, Storage.ReadBoolean('ExportToHTML.Include.Index'));
end;

procedure THTMLExportModelTests.SaveIncludeTags(Value: Boolean);
var
  Storage: IStorage;
  Model: IHTMLExport;
begin
  Storage := TXmlStorage.Create;

  Model := THTMLExportModel.Create(FO2File, FO2File.Objects.ToEnumerable,
    FAppVersionInfo, TAppFiles.Create, Storage);

  Model.IncludeTags := Value;
  Model.StoreSettings;

  Assert.AreEqual(Value, Storage.ReadBoolean('ExportToHTML.Include.Tags'));
end;

procedure THTMLExportModelTests.SaveIncludeNotes(Value: Boolean);
var
  Storage: IStorage;
  Model: IHTMLExport;
begin
  Storage := TXmlStorage.Create;

  Model := THTMLExportModel.Create(FO2File, FO2File.Objects.ToEnumerable,
    FAppVersionInfo, TAppFiles.Create, Storage);

  Model.IncludeNotes := Value;
  Model.StoreSettings;

  Assert.AreEqual(Value, Storage.ReadBoolean('ExportToHTML.Include.Notes'));
end;

procedure THTMLExportModelTests.SaveIncludeRelations(Value: Boolean);
var
  Storage: IStorage;
  Model: IHTMLExport;
begin
  Storage := TXmlStorage.Create;

  Model := THTMLExportModel.Create(FO2File, FO2File.Objects.ToEnumerable,
    FAppVersionInfo, TAppFiles.Create, Storage);

  Model.IncludeRelations := Value;
  Model.StoreSettings;

  Assert.AreEqual(Value, Storage.ReadBoolean('ExportToHTML.Include.Relations'));
end;

procedure THTMLExportModelTests.SaveIncludePasswords(Value: Boolean);
var
  Storage: IStorage;
  Model: IHTMLExport;
begin
  Storage := TXmlStorage.Create;

  Model := THTMLExportModel.Create(FO2File, FO2File.Objects.ToEnumerable,
    FAppVersionInfo, TAppFiles.Create, Storage);

  Model.IncludePasswords := Value;
  Model.StoreSettings;

  Assert.AreEqual(Value, Storage.ReadBoolean('ExportToHTML.Include.Passwords'));
end;

end.
