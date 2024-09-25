unit Testing.XmlStorage;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TXmlStorageTests = class
  public
    [Test]
    [TestCase('False', 'False')]
    [TestCase('True' , 'True')]
    procedure RetrieveDefaultBooleanValue(Value: Boolean);

    [Test]
    [TestCase('False', 'False')]
    [TestCase('True' , 'True')]
    procedure StoreAndRetrieveBooleanValue(Value: Boolean);

    [Test]
    procedure RetrieveDefaultIntegerValue;

    [Test]
    procedure StoreAndRetrieveIntegerValue;

    [Test]
    procedure RetrieveDefaultFloatValue;

    [Test]
    procedure StoreAndRetrieveFloatValue;

    [Test]
    procedure RetrieveDefaultStringValue;

    [Test]
    procedure StoreAndRetrieveStringValue;
  end;

implementation

uses
  uServices, uXmlStorage;

{ TXmlStorageTests }

procedure TXmlStorageTests.RetrieveDefaultBooleanValue(Value: Boolean);
var
  Storage: IStorage;
begin
  Storage := TXmlStorage.Create;

  Assert.AreEqual(Value, Storage.ReadBoolean('Boolean value', Value));
end;

procedure TXmlStorageTests.StoreAndRetrieveBooleanValue(Value: Boolean);
var
  Storage: IStorage;
begin
  Storage := TXmlStorage.Create;

  Storage.WriteBoolean('Boolean value', Value);

  Assert.AreEqual(Value, Storage.ReadBoolean('Boolean value'));
end;

procedure TXmlStorageTests.RetrieveDefaultIntegerValue;
var
  Storage: IStorage;
begin
  Storage := TXmlStorage.Create;

  Assert.AreEqual(42, Storage.ReadInteger('Integer value', 42));
end;

procedure TXmlStorageTests.StoreAndRetrieveIntegerValue;
var
  Storage: IStorage;
begin
  Storage := TXmlStorage.Create;

  Storage.WriteInteger('Integer value', 42);

  Assert.AreEqual(42, Storage.ReadInteger('Integer value'));
end;

procedure TXmlStorageTests.RetrieveDefaultFloatValue;
var
  Storage: IStorage;
begin
  Storage := TXmlStorage.Create;

  Assert.AreEqual(Double(1.21), Storage.ReadFloat('Float value', 1.21));
end;

procedure TXmlStorageTests.StoreAndRetrieveFloatValue;
var
  Storage: IStorage;
begin
  Storage := TXmlStorage.Create;

  Storage.WriteFloat('Float value', 1.21);

  Assert.AreEqual(Double(1.21), Storage.ReadFloat('Float value'));
end;

procedure TXmlStorageTests.RetrieveDefaultStringValue;
var
  Storage: IStorage;
begin
  Storage := TXmlStorage.Create;

  Assert.AreEqual('Allons-y!', Storage.ReadString('String value', 'Allons-y!'));
end;

procedure TXmlStorageTests.StoreAndRetrieveStringValue;
var
  Storage: IStorage;
begin
  Storage := TXmlStorage.Create;

  Storage.WriteString('String value', 'Allons-y!');

  Assert.AreEqual('Allons-y!', Storage.ReadString('String value'));
end;

end.
