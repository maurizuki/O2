unit Testing.FilePropsModel;

interface

uses
  DUnitX.TestFramework, uO2Defs, uO2File;

type
  [TestFixture]
  TFilePropsModelTests = class
  private
    FO2File: TO2File;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure LoadTitle;

    [Test]
    procedure LoadDescription;

    [Test]
    procedure LoadAuthor;

    [Test]
    [TestCase('None'     , '0,(none)')]
    [TestCase('Blowfish' , '1,Blowfish [deprecated]')]
    [TestCase('CAST128'  , '2,Cast-128')]
    [TestCase('CAST256'  , '3,Cast-256')]
    [TestCase('DES'      , '4,DES [deprecated]')]
    [TestCase('TripleDES', '5,3DES')]
    [TestCase('ICE'      , '6,Ice [deprecated]')]
    [TestCase('ThinICE'  , '7,Thin Ice [deprecated]')]
    [TestCase('ICE2'     , '8,Ice 2')]
    [TestCase('IDEA'     , '9,IDEA')]
    [TestCase('MARS'     , '10,MARS')]
    [TestCase('MISTY1'   , '11,Misty1 [deprecated]')]
    [TestCase('RC2'      , '12,RC2 [deprecated]')]
    [TestCase('RC4'      , '13,RC4 [deprecated]')]
    [TestCase('RC5'      , '14,RC5')]
    [TestCase('RC6'      , '15,RC6')]
    [TestCase('Rijndael' , '16,Rijndael (AES)')]
    [TestCase('Serpent'  , '17,Serpent')]
    [TestCase('TEA'      , '18,TEA [deprecated]')]
    [TestCase('Twofish'  , '19,Twofish')]
    procedure LoadCipher(Cipher: TO2Cipher; const Expected: string);

    [Test]
    [TestCase('None'     , '0,(none)')]
    [TestCase('HAVAL'    , '1,Haval')]
    [TestCase('MD4'      , '2,MD4')]
    [TestCase('MD5'      , '3,MD5 [deprecated]')]
    [TestCase('RIPEMD128', '4,RipeMD-128')]
    [TestCase('RIPEMD160', '5,RipeMD-160')]
    [TestCase('SHA1'     , '6,SHA-1 [deprecated]')]
    [TestCase('SHA256'   , '7,SHA-256')]
    [TestCase('SHA384'   , '8,SHA-384')]
    [TestCase('SHA512'   , '9,SHA-512')]
    [TestCase('Tiger'    , '10,Tiger')]
    procedure LoadHash(Hash: TO2Hash; const Expected: string);

    [Test]
    procedure SaveTitle;

    [Test]
    procedure SaveDescription;

    [Test]
    procedure SaveAuthor;
  end;

implementation

uses
  uServices, uFilePropsModel;

{ TFilePropsModelTests }

procedure TFilePropsModelTests.Setup;
begin
  FO2File := TO2File.Create;
end;

procedure TFilePropsModelTests.TearDown;
begin
  FO2File.Free;
end;

procedure TFilePropsModelTests.LoadTitle;
var
  Model: IFileProps;
begin
  FO2File.Title := 'Original title';

  Model := TFilePropsModel.Create(FO2File);

  Assert.AreEqual('Original title', Model.Title);
end;

procedure TFilePropsModelTests.LoadDescription;
var
  Model: IFileProps;
begin
  FO2File.Description := 'Original description';

  Model := TFilePropsModel.Create(FO2File);

  Assert.AreEqual('Original description', Model.Description);
end;

procedure TFilePropsModelTests.LoadAuthor;
var
  Model: IFileProps;
begin
  FO2File.Author := 'Original author';

  Model := TFilePropsModel.Create(FO2File);

  Assert.AreEqual('Original author', Model.Author);
end;

procedure TFilePropsModelTests.LoadCipher(Cipher: TO2Cipher;
  const Expected: string);
var
  Model: IFileProps;
begin
  FO2File.Encrypted := True;
  FO2File.Cipher := Cipher;

  Model := TFilePropsModel.Create(FO2File);

  Assert.AreEqual(Expected, Model.Cipher);
end;

procedure TFilePropsModelTests.LoadHash(Hash: TO2Hash; const Expected: string);
var
  Model: IFileProps;
begin
  FO2File.Encrypted := True;
  FO2File.Hash := Hash;

  Model := TFilePropsModel.Create(FO2File);

  Assert.AreEqual(Expected, Model.Hash);
end;

procedure TFilePropsModelTests.SaveTitle;
var
  Model: IFileProps;
begin
  Model := TFilePropsModel.Create(FO2File);

  Model.Title := 'New title';
  Model.ApplyChanges;

  Assert.AreEqual('New title', FO2File.Title);
end;

procedure TFilePropsModelTests.SaveDescription;
var
  Model: IFileProps;
begin
  Model := TFilePropsModel.Create(FO2File);

  Model.Description := 'New description';
  Model.ApplyChanges;

  Assert.AreEqual('New description', FO2File.Description);
end;

procedure TFilePropsModelTests.SaveAuthor;
var
  Model: IFileProps;
begin
  Model := TFilePropsModel.Create(FO2File);

  Model.Author := 'New author';
  Model.ApplyChanges;

  Assert.AreEqual('New author', FO2File.Author);
end;

initialization
  TDUnitX.RegisterTestFixture(TFilePropsModelTests);

end.

