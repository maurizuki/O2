unit TestCiphers;

interface

uses
  DUnitX.TestFramework, DCPcrypt2;

type
  [TestFixture]
  TTestCiphers = class
  private
    procedure TestCipher(CipherClass: TDCP_cipherclass);
  public
    [Test]
    procedure TestBlowfish;
    [Test]
    procedure TestCast128;
    [Test]
    procedure TestCast256;
    [Test]
    procedure TestDES;
    [Test]
    procedure Test3DES;
    [Test]
    procedure TestICE;
    [Test]
    procedure TestThinICE;
    [Test]
    procedure TestICE2;
    [Test]
    procedure TestIDEA;
    [Test]
    procedure TestMARS;
    [Test]
    procedure TestMisty1;
    [Test]
    procedure TestRC2;
    [Test]
    procedure TestRC4;
    [Test]
    procedure TestRC5;
    [Test]
    procedure TestRC6;
    [Test]
    procedure TestRijndael;
    [Test]
    procedure TestSerpent;
    [Test]
    procedure TestTEA;
    [Test]
    procedure TestTwofish;
  end;

implementation

uses
  DCPblowfish, DCPcast128, DCPcast256, DCPdes, DCPice, DCPidea, DCPmars,
  DCPmisty1, DCPrc2, DCPrc4, DCPrc5, DCPrc6, DCPrijndael, DCPserpent, DCPtea,
  DCPtwofish;

procedure TTestCiphers.TestCipher(CipherClass: TDCP_cipherclass);
var
  ACipher: TDCP_cipher;
begin
  ACipher := CipherClass.Create;
  try
    Assert.IsTrue(ACipher.SelfTest);
  finally
    ACipher.Free;
  end;
end;

procedure TTestCiphers.Test3DES;
begin
  TestCipher(TDCP_3des);
end;

procedure TTestCiphers.TestBlowfish;
begin
  TestCipher(TDCP_blowfish);
end;

procedure TTestCiphers.TestCast128;
begin
  TestCipher(TDCP_cast128);
end;

procedure TTestCiphers.TestCast256;
begin
  TestCipher(TDCP_cast256);
end;

procedure TTestCiphers.TestDES;
begin
  TestCipher(TDCP_des);
end;

procedure TTestCiphers.TestICE;
begin
  TestCipher(TDCP_ice);
end;

procedure TTestCiphers.TestICE2;
begin
  TestCipher(TDCP_ice2);
end;

procedure TTestCiphers.TestIDEA;
begin
  TestCipher(TDCP_idea);
end;

procedure TTestCiphers.TestMARS;
begin
  TestCipher(TDCP_mars);
end;

procedure TTestCiphers.TestMisty1;
begin
  TestCipher(TDCP_misty1);
end;

procedure TTestCiphers.TestRC2;
begin
  TestCipher(TDCP_rc2);
end;

procedure TTestCiphers.TestRC4;
begin
  TestCipher(TDCP_rc4);
end;

procedure TTestCiphers.TestRC5;
begin
  TestCipher(TDCP_rc5);
end;

procedure TTestCiphers.TestRC6;
begin
  TestCipher(TDCP_rc6);
end;

procedure TTestCiphers.TestRijndael;
begin
  TestCipher(TDCP_rijndael);
end;

procedure TTestCiphers.TestSerpent;
begin
  TestCipher(TDCP_serpent);
end;

procedure TTestCiphers.TestTEA;
begin
  TestCipher(TDCP_tea);
end;

procedure TTestCiphers.TestThinICE;
begin
  TestCipher(TDCP_thinice);
end;

procedure TTestCiphers.TestTwofish;
begin
  TestCipher(TDCP_twofish);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestCiphers);

end.

