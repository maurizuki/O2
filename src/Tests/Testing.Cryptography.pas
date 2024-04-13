unit Testing.Cryptography;

interface

uses
  SysUtils, DUnitX.TestFramework, DCPcrypt2;

type
  [TestFixture]
  TCipherAlgorithms = class
  private
    procedure TestCipher(CipherClass: TDCP_cipherclass);
  public
    [Test]
    procedure Blowfish;
    [Test]
    procedure CAST128;
    [Test]
    procedure CAST256;
    [Test]
    procedure DES;
    [Test]
    procedure TripleDES;
    [Test]
    procedure ICE;
    [Test]
    procedure ThinICE;
    [Test]
    procedure ICE2;
    [Test]
    procedure IDEA;
    [Test]
    procedure MARS;
    [Test]
    procedure MISTY1;
    [Test]
    procedure RC2;
    [Test]
    procedure RC4;
    [Test]
    procedure RC5;
    [Test]
    procedure RC6;
    [Test]
    procedure Rijndael;
    [Test]
    procedure Serpent;
    [Test]
    procedure TEA;
    [Test]
    procedure Twofish;
  end;

  [TestFixture]
  THashAlgorithms = class
  private
    procedure TestHash(HashClass: TDCP_hashclass);
  public
    [Test]
    procedure HAVAL;
    [Test]
    procedure MD4;
    [Test]
    procedure MD5;
    [Test]
    procedure RIPEMD128;
    [Test]
    procedure RIPEMD160;
    [Test]
    procedure SHA1;
    [Test]
    procedure SHA256;
    [Test]
    procedure SHA384;
    [Test]
    procedure SHA512;
    [Test]
    procedure Tiger;
  end;

implementation

uses
  DCPblowfish, DCPcast128, DCPcast256, DCPdes, DCPice, DCPidea, DCPmars,
  DCPmisty1, DCPrc2, DCPrc4, DCPrc5, DCPrc6, DCPrijndael, DCPserpent, DCPtea,
  DCPtwofish, DCPhaval, DCPmd4, DCPmd5, DCPripemd128, DCPripemd160, DCPsha1,
  DCPsha256, DCPsha512, DCPtiger;

{ TCipherAlgorithms }

procedure TCipherAlgorithms.TestCipher(CipherClass: TDCP_cipherclass);
var
  ACipher: TDCP_cipher;
begin
  ACipher := CipherClass.Create;
  try
    Assert.IsTrue(ACipher.SelfTest,
      Format('Cipher algorithm "%s" self-test', [ACipher.GetAlgorithm]));
  finally
    ACipher.Free;
  end;
end;

procedure TCipherAlgorithms.TripleDES;
begin
  TestCipher(TDCP_3des);
end;

procedure TCipherAlgorithms.Blowfish;
begin
  TestCipher(TDCP_blowfish);
end;

procedure TCipherAlgorithms.CAST128;
begin
  TestCipher(TDCP_cast128);
end;

procedure TCipherAlgorithms.CAST256;
begin
  TestCipher(TDCP_cast256);
end;

procedure TCipherAlgorithms.DES;
begin
  TestCipher(TDCP_des);
end;

procedure TCipherAlgorithms.ICE;
begin
  TestCipher(TDCP_ice);
end;

procedure TCipherAlgorithms.ICE2;
begin
  TestCipher(TDCP_ice2);
end;

procedure TCipherAlgorithms.IDEA;
begin
  TestCipher(TDCP_idea);
end;

procedure TCipherAlgorithms.MARS;
begin
  TestCipher(TDCP_mars);
end;

procedure TCipherAlgorithms.MISTY1;
begin
  TestCipher(TDCP_misty1);
end;

procedure TCipherAlgorithms.RC2;
begin
  TestCipher(TDCP_rc2);
end;

procedure TCipherAlgorithms.RC4;
begin
  TestCipher(TDCP_rc4);
end;

procedure TCipherAlgorithms.RC5;
begin
  TestCipher(TDCP_rc5);
end;

procedure TCipherAlgorithms.RC6;
begin
  TestCipher(TDCP_rc6);
end;

procedure TCipherAlgorithms.Rijndael;
begin
  TestCipher(TDCP_rijndael);
end;

procedure TCipherAlgorithms.Serpent;
begin
  TestCipher(TDCP_serpent);
end;

procedure TCipherAlgorithms.TEA;
begin
  TestCipher(TDCP_tea);
end;

procedure TCipherAlgorithms.ThinICE;
begin
  TestCipher(TDCP_thinice);
end;

procedure TCipherAlgorithms.Twofish;
begin
  TestCipher(TDCP_twofish);
end;

{ THashAlgorithms }

procedure THashAlgorithms.TestHash(HashClass: TDCP_hashclass);
var
  AHash: TDCP_hash;
begin
  AHash := HashClass.Create;
  try
    Assert.IsTrue(AHash.SelfTest,
      Format('Hash algorithm "%s" self-test', [AHash.GetAlgorithm]));
  finally
    AHash.Free;
  end;
end;

procedure THashAlgorithms.HAVAL;
begin
  TestHash(TDCP_haval);
end;

procedure THashAlgorithms.MD4;
begin
  TestHash(TDCP_md4);
end;

procedure THashAlgorithms.MD5;
begin
  TestHash(TDCP_md5);
end;

procedure THashAlgorithms.RIPEMD128;
begin
  TestHash(TDCP_ripemd128);
end;

procedure THashAlgorithms.RIPEMD160;
begin
  TestHash(TDCP_ripemd160);
end;

procedure THashAlgorithms.SHA1;
begin
  TestHash(TDCP_sha1);
end;

procedure THashAlgorithms.SHA256;
begin
  TestHash(TDCP_sha256);
end;

procedure THashAlgorithms.SHA384;
begin
  TestHash(TDCP_sha384);
end;

procedure THashAlgorithms.SHA512;
begin
  TestHash(TDCP_sha512);
end;

procedure THashAlgorithms.Tiger;
begin
  TestHash(TDCP_tiger);
end;

initialization
  TDUnitX.RegisterTestFixture(TCipherAlgorithms);
  TDUnitX.RegisterTestFixture(THashAlgorithms);

end.

