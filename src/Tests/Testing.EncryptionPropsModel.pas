unit Testing.EncryptionPropsModel;

interface

uses
  DUnitX.TestFramework, uO2Defs, uO2File;

type
  [TestFixture]
  TEncryptionPropsModelTests = class
  private
    FO2File: TO2File;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('NotEncrypted', 'False,0,0')]
    [TestCase('None'        , 'True,0,0')]
    [TestCase('Blowfish'    , 'True,1,1')]
    [TestCase('CAST128'     , 'True,2,2')]
    [TestCase('CAST256'     , 'True,3,3')]
    [TestCase('DES'         , 'True,4,4')]
    [TestCase('TripleDES'   , 'True,5,5')]
    [TestCase('ICE'         , 'True,6,6')]
    [TestCase('ThinICE'     , 'True,7,7')]
    [TestCase('ICE2'        , 'True,8,8')]
    [TestCase('IDEA'        , 'True,9,9')]
    [TestCase('MARS'        , 'True,10,10')]
    [TestCase('MISTY1'      , 'True,11,11')]
    [TestCase('RC2'         , 'True,12,12')]
    [TestCase('RC4'         , 'True,13,13')]
    [TestCase('RC5'         , 'True,14,14')]
    [TestCase('RC6'         , 'True,15,15')]
    [TestCase('Rijndael'    , 'True,16,16')]
    [TestCase('Serpent'     , 'True,17,17')]
    [TestCase('TEA'         , 'True,18,18')]
    [TestCase('Twofish'     , 'True,19,19')]
    procedure LoadCipherIndex(IsEncrypted: Boolean; Cipher: TO2Cipher;
      Expected: Integer);

    [Test]
    [TestCase('None'     , '0,0')]
    [TestCase('HAVAL'    , '1,0')]
    [TestCase('MD4'      , '2,1')]
    [TestCase('MD5'      , '3,2')]
    [TestCase('RIPEMD128', '4,3')]
    [TestCase('RIPEMD160', '5,4')]
    [TestCase('SHA1'     , '6,5')]
    [TestCase('SHA256'   , '7,6')]
    [TestCase('SHA384'   , '8,7')]
    [TestCase('SHA512'   , '9,8')]
    [TestCase('Tiger'    , '10,9')]
    procedure LoadHashIndex(Hash: TO2Hash; Expected: Integer);

    [Test]
    procedure LoadPassword;

    [Test]
    procedure LoadPasswordConfirmation;

    [Test]
    [TestCase('None'     , '0,False')]
    [TestCase('Blowfish' , '1,True')]
    [TestCase('CAST128'  , '2,True')]
    [TestCase('CAST256'  , '3,True')]
    [TestCase('DES'      , '4,True')]
    [TestCase('TripleDES', '5,True')]
    [TestCase('ICE'      , '6,True')]
    [TestCase('ThinICE'  , '7,True')]
    [TestCase('ICE2'     , '8,True')]
    [TestCase('IDEA'     , '9,True')]
    [TestCase('MARS'     , '10,True')]
    [TestCase('MISTY1'   , '11,True')]
    [TestCase('RC2'      , '12,True')]
    [TestCase('RC4'      , '13,True')]
    [TestCase('RC5'      , '14,True')]
    [TestCase('RC6'      , '15,True')]
    [TestCase('Rijndael' , '16,True')]
    [TestCase('Serpent'  , '17,True')]
    [TestCase('TEA'      , '18,True')]
    [TestCase('Twofish'  , '19,True')]
    procedure SaveEncrypted(CipherIndex: Integer; Expected: Boolean);

    [Test]
    [TestCase('None'     , '0,0')]
    [TestCase('Blowfish' , '1,1')]
    [TestCase('CAST128'  , '2,2')]
    [TestCase('CAST256'  , '3,3')]
    [TestCase('DES'      , '4,4')]
    [TestCase('TripleDES', '5,5')]
    [TestCase('ICE'      , '6,6')]
    [TestCase('ThinICE'  , '7,7')]
    [TestCase('ICE2'     , '8,8')]
    [TestCase('IDEA'     , '9,9')]
    [TestCase('MARS'     , '10,10')]
    [TestCase('MISTY1'   , '11,11')]
    [TestCase('RC2'      , '12,12')]
    [TestCase('RC4'      , '13,13')]
    [TestCase('RC5'      , '14,14')]
    [TestCase('RC6'      , '15,15')]
    [TestCase('Rijndael' , '16,16')]
    [TestCase('Serpent'  , '17,17')]
    [TestCase('TEA'      , '18,18')]
    [TestCase('Twofish'  , '19,19')]
    procedure SaveCipher(CipherIndex: Integer; Expected: TO2Cipher);

    [Test]
    [TestCase('HAVAL'    , '0,1')]
    [TestCase('MD4'      , '1,2')]
    [TestCase('MD5'      , '2,3')]
    [TestCase('RIPEMD128', '3,4')]
    [TestCase('RIPEMD160', '4,5')]
    [TestCase('SHA1'     , '5,6')]
    [TestCase('SHA256'   , '6,7')]
    [TestCase('SHA384'   , '7,8')]
    [TestCase('SHA512'   , '8,9')]
    [TestCase('Tiger'    , '9,10')]
    procedure SaveHash(HashIndex: Integer; Expected: TO2Hash);

    [Test]
    procedure SavePassword;

    [Test]
    [TestCase('None'     , '0,False')]
    [TestCase('Blowfish' , '1,True')]
    [TestCase('CAST128'  , '2,True')]
    [TestCase('CAST256'  , '3,True')]
    [TestCase('DES'      , '4,True')]
    [TestCase('TripleDES', '5,True')]
    [TestCase('ICE'      , '6,True')]
    [TestCase('ThinICE'  , '7,True')]
    [TestCase('ICE2'     , '8,True')]
    [TestCase('IDEA'     , '9,True')]
    [TestCase('MARS'     , '10,True')]
    [TestCase('MISTY1'   , '11,True')]
    [TestCase('RC2'      , '12,True')]
    [TestCase('RC4'      , '13,True')]
    [TestCase('RC5'      , '14,True')]
    [TestCase('RC6'      , '15,True')]
    [TestCase('Rijndael' , '16,True')]
    [TestCase('Serpent'  , '17,True')]
    [TestCase('TEA'      , '18,True')]
    [TestCase('Twofish'  , '19,True')]
    procedure IsEncrypted(CipherIndex: Integer; Expected: Boolean);

    [Test]
    [TestCase('None'     , '0,True')]
    [TestCase('Blowfish' , '1,False')]
    [TestCase('CAST128'  , '2,True')]
    [TestCase('CAST256'  , '3,True')]
    [TestCase('DES'      , '4,False')]
    [TestCase('TripleDES', '5,True')]
    [TestCase('ICE'      , '6,False')]
    [TestCase('ThinICE'  , '7,False')]
    [TestCase('ICE2'     , '8,True')]
    [TestCase('IDEA'     , '9,True')]
    [TestCase('MARS'     , '10,True')]
    [TestCase('MISTY1'   , '11,False')]
    [TestCase('RC2'      , '12,False')]
    [TestCase('RC4'      , '13,False')]
    [TestCase('RC5'      , '14,True')]
    [TestCase('RC6'      , '15,True')]
    [TestCase('Rijndael' , '16,True')]
    [TestCase('Serpent'  , '17,True')]
    [TestCase('TEA'      , '18,False')]
    [TestCase('Twofish'  , '19,True')]
    procedure ValidCipher(CipherIndex: Integer; Expected: Boolean);

    [Test]
    [TestCase('HAVAL'    , '0,True')]
    [TestCase('MD4'      , '1,True')]
    [TestCase('MD5'      , '2,False')]
    [TestCase('RIPEMD128', '3,True')]
    [TestCase('RIPEMD160', '4,True')]
    [TestCase('SHA1'     , '5,False')]
    [TestCase('SHA256'   , '6,True')]
    [TestCase('SHA384'   , '7,True')]
    [TestCase('SHA512'   , '8,True')]
    [TestCase('Tiger'    , '9,True')]
    procedure ValidHash(HashIndex: Integer; Expected: Boolean);

    [Test]
    [TestCase('NotValid', '1234,False')]
    [TestCase('Valid',    '12345,True')]
    procedure ValidPassword(const Password: string; Expected: Boolean);

    [Test]
    procedure NotValidPasswordMismatch;
  end;

implementation

uses
  uServices, uEncryptionPropsModel;

{ TEncryptionPropsModelTests }

procedure TEncryptionPropsModelTests.Setup;
begin
  FO2File := TO2File.Create;
end;

procedure TEncryptionPropsModelTests.TearDown;
begin
  FO2File.Free;
end;

procedure TEncryptionPropsModelTests.LoadCipherIndex(IsEncrypted: Boolean;
  Cipher: TO2Cipher; Expected: Integer);
var
  Model: IEncryptionProps;
begin
  FO2File.Encrypted := IsEncrypted;
  FO2File.Cipher := Cipher;

  Model := TEncryptionPropsModel.Create(FO2File);

  Assert.AreEqual(Expected, Model.CipherIndex);
end;

procedure TEncryptionPropsModelTests.LoadHashIndex(Hash: TO2Hash;
  Expected: Integer);
var
  Model: IEncryptionProps;
begin
  FO2File.Hash := Hash;

  Model := TEncryptionPropsModel.Create(FO2File);

  Assert.AreEqual(Expected, Model.HashIndex);
end;

procedure TEncryptionPropsModelTests.LoadPassword;
var
  Model: IEncryptionProps;
begin
  FO2File.Password := 'Original password';

  Model := TEncryptionPropsModel.Create(FO2File);

  Assert.AreEqual('Original password', Model.Password);
end;

procedure TEncryptionPropsModelTests.LoadPasswordConfirmation;
var
  Model: IEncryptionProps;
begin
  FO2File.Password := 'Original password';

  Model := TEncryptionPropsModel.Create(FO2File);

  Assert.AreEqual('Original password', Model.PasswordConfirmation);
end;

procedure TEncryptionPropsModelTests.SaveEncrypted(CipherIndex: Integer;
  Expected: Boolean);
var
  Model: IEncryptionProps;
begin
  Model := TEncryptionPropsModel.Create(FO2File);

  Model.CipherIndex := CipherIndex;
  Model.ApplyChanges;

  Assert.AreEqual(Expected, FO2File.Encrypted);
end;

procedure TEncryptionPropsModelTests.SaveCipher(CipherIndex: Integer;
  Expected: TO2Cipher);
var
  Model: IEncryptionProps;
begin
  Model := TEncryptionPropsModel.Create(FO2File);

  Model.CipherIndex := CipherIndex;
  Model.ApplyChanges;

  Assert.AreEqual(Expected, FO2File.Cipher);
end;

procedure TEncryptionPropsModelTests.SaveHash(HashIndex: Integer;
  Expected: TO2Hash);
var
  Model: IEncryptionProps;
begin
  Model := TEncryptionPropsModel.Create(FO2File);

  Model.HashIndex := HashIndex;
  Model.ApplyChanges;

  Assert.AreEqual(Expected, FO2File.Hash);
end;

procedure TEncryptionPropsModelTests.SavePassword;
var
  Model: IEncryptionProps;
begin
  Model := TEncryptionPropsModel.Create(FO2File);

  Model.Password := 'New password';
  Model.ApplyChanges;

  Assert.AreEqual('New password', FO2File.Password);
end;

procedure TEncryptionPropsModelTests.IsEncrypted(CipherIndex: Integer;
  Expected: Boolean);
var
  Model: IEncryptionProps;
begin
  Model := TEncryptionPropsModel.Create(FO2File);

  Model.CipherIndex := CipherIndex;

  Assert.AreEqual(Expected, Model.IsEncrypted);
end;

procedure TEncryptionPropsModelTests.ValidCipher(CipherIndex: Integer;
  Expected: Boolean);
var
  Model: IEncryptionProps;
begin
  Model := TEncryptionPropsModel.Create(FO2File);

  Model.CipherIndex := CipherIndex;
  Model.Password := 'Valid password';
  Model.PasswordConfirmation := 'Valid password';

  Assert.AreEqual(Expected, Model.Valid);
end;

procedure TEncryptionPropsModelTests.ValidHash(HashIndex: Integer;
  Expected: Boolean);
var
  Model: IEncryptionProps;
begin
  Model := TEncryptionPropsModel.Create(FO2File);

  Model.CipherIndex := 16;
  Model.HashIndex := HashIndex;
  Model.Password := 'Valid password';
  Model.PasswordConfirmation := 'Valid password';

  Assert.AreEqual(Expected, Model.Valid);
end;

procedure TEncryptionPropsModelTests.ValidPassword(const Password: string;
  Expected: Boolean);
var
  Model: IEncryptionProps;
begin
  Model := TEncryptionPropsModel.Create(FO2File);

  Model.CipherIndex := 16;
  Model.Password := Password;
  Model.PasswordConfirmation := Password;

  Assert.AreEqual(Expected, Model.Valid);
end;

procedure TEncryptionPropsModelTests.NotValidPasswordMismatch;
var
  Model: IEncryptionProps;
begin
  Model := TEncryptionPropsModel.Create(FO2File);

  Model.CipherIndex := 16;
  Model.Password := 'Valid password';
  Model.PasswordConfirmation := 'Another password';

  Assert.IsFalse(Model.Valid);
end;

end.
