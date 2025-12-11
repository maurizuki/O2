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

procedure THashAlgorithms.HAVAL;
const
  Test1Out: array[0..31] of byte=
    ($EA,$B9,$2E,$C2,$9E,$FC,$CB,$E0,$19,$D8,$31,$F2,$AB,$03,$0B,$10,
     $7E,$A9,$B8,$4B,$76,$86,$47,$33,$3B,$2D,$8C,$A5,$53,$1F,$01,$12);
  Test2Out: array[0..31] of byte=
    ($17,$22,$C5,$DC,$88,$D7,$77,$08,$D4,$78,$7D,$28,$3D,$7E,$26,$38,
     $57,$E9,$84,$AE,$5A,$6F,$9A,$18,$54,$96,$39,$06,$34,$3A,$BB,$F8);
var
  TestHash: TDCP_haval;
  TestOut: array[0..31] of byte;
begin
  TestHash:= TDCP_haval.Create;
  try
    TestHash.Init;
    TestHash.UpdateStr('abc');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test1Out,Sizeof(Test1Out),'Test 1');
    TestHash.Init;
    TestHash.UpdateStr('abcdefghijklmnopqrstuvwxyz');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test2Out,Sizeof(Test2Out),'Test 2');
  finally
    TestHash.Free;
  end;
end;

procedure THashAlgorithms.MD4;
const
  Test1Out: array[0..15] of byte=
    ($E0,$FB,$A3,$82,$68,$D0,$EC,$66,$EF,$1C,$B4,$52,$D5,$88,$5E,$53);
  Test2Out: array[0..15] of byte=
    ($0B,$D6,$31,$85,$F3,$48,$4B,$B0,$00,$28,$6C,$85,$91,$7D,$C1,$2E);
var
  TestHash: TDCP_md4;
  TestOut: array[0..19] of byte;
begin
  TestHash:= TDCP_md4.Create;
  try
    TestHash.Init;
    TestHash.UpdateStr('abc');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test1Out,Sizeof(Test1Out),'Test 1');
    TestHash.Init;
    TestHash.UpdateStr('abcdefghijklmnopqrstuvwxyz');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test2Out,Sizeof(Test2Out),'Test 2');
  finally
    TestHash.Free;
  end;
end;

procedure THashAlgorithms.MD5;
const
  Test1Out: array[0..15] of byte=
    ($CE,$14,$73,$CF,$80,$C6,$B3,$FD,$A8,$E3,$DF,$C0,$06,$AD,$C3,$15);
  Test2Out: array[0..15] of byte=
    ($35,$02,$0D,$67,$A5,$2D,$8E,$91,$53,$30,$F0,$A7,$7F,$67,$6B,$BF);
var
  TestHash: TDCP_md5;
  TestOut: array[0..19] of byte;
begin
  TestHash:= TDCP_md5.Create;
  try
    TestHash.Init;
    TestHash.UpdateStr('abc');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test1Out,Sizeof(Test1Out),'Test 1');
    TestHash.Init;
    TestHash.UpdateStr('abcdefghijklmnopqrstuvwxyz');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test2Out,Sizeof(Test2Out),'Test 2');
  finally
    TestHash.Free;
  end;
end;

procedure THashAlgorithms.RIPEMD128;
const
  Test1Out: array[0..15] of byte=
    ($46,$41,$76,$F1,$8E,$DC,$59,$CB,$59,$F7,$B0,$8E,$2A,$6E,$40,$4F);
  Test2Out: array[0..15] of byte=
    ($7B,$84,$1D,$A3,$C9,$BD,$29,$23,$AF,$E8,$B8,$C9,$1F,$70,$36,$AE);
var
  TestHash: TDCP_ripemd128;
  TestOut: array[0..15] of byte;
begin
  TestHash:= TDCP_ripemd128.Create;
  try
    TestHash.Init;
    TestHash.UpdateStr('abc');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test1Out,Sizeof(Test1Out),'Test 1');
    TestHash.Init;
    TestHash.UpdateStr('abcdefghijklmnopqrstuvwxyz');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test2Out,Sizeof(Test2Out),'Test 2');
  finally
    TestHash.Free;
  end;
end;

procedure THashAlgorithms.RIPEMD160;
const
  Test1Out: array[0..19] of byte=
    ($44,$BF,$C4,$96,$5C,$B1,$40,$DC,$9B,$EA,$1F,$84,$2A,$9D,$EA,$BD,$AE,$0B,$E4,$53);
  Test2Out: array[0..19] of byte=
    ($24,$71,$79,$9A,$D0,$EE,$46,$96,$8F,$A5,$BD,$32,$BE,$AC,$4E,$86,$DA,$2E,$95,$6D);
var
  TestHash: TDCP_ripemd160;
  TestOut: array[0..19] of byte;
begin
  TestHash:= TDCP_ripemd160.Create;
  try
    TestHash.Init;
    TestHash.UpdateStr('abc');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test1Out,Sizeof(Test1Out),'Test 1');
    TestHash.Init;
    TestHash.UpdateStr('abcdefghijklmnopqrstuvwxyz');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test2Out,Sizeof(Test2Out),'Test 2');
  finally
    TestHash.Free;
  end;
end;

procedure THashAlgorithms.SHA1;
const
  Test1Out: array[0..19] of byte=
    ($9F,$04,$F4,$1A,$84,$85,$14,$16,$20,$50,$E3,$D6,$8C,$1A,$7A,$BB,$44,$1D,$C2,$B5);
  Test2Out: array[0..19] of byte=
    ($D6,$44,$F8,$B7,$C1,$50,$2B,$17,$39,$2F,$84,$C0,$C9,$D2,$5A,$2C,$8F,$97,$66,$4B);
var
  TestHash: TDCP_sha1;
  TestOut: array[0..19] of byte;
begin
  TestHash:= TDCP_sha1.Create;
  try
    TestHash.Init;
    TestHash.UpdateStr('abc');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test1Out,Sizeof(Test1Out),'Test 1');
    TestHash.Init;
    TestHash.UpdateStr('abcdefghijklmnopqrstuvwxyz');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test2Out,Sizeof(Test2Out),'Test 2');
  finally
    TestHash.Free;
  end;
end;

procedure THashAlgorithms.SHA256;
const
  Test1Out: array[0..31] of byte=
    ($13,$E2,$28,$56,$7E,$82,$49,$FC,$E5,$33,$37,$F2,$5D,$79,$70,$DE,
     $3B,$D6,$8A,$B2,$65,$34,$24,$C7,$B8,$F9,$FD,$05,$E3,$3C,$AE,$DF);
  Test2Out: array[0..31] of byte=
    ($D7,$9C,$A8,$CA,$68,$CA,$C4,$C2,$D2,$9A,$41,$67,$29,$53,$03,$D2,
     $A7,$CF,$6C,$AA,$E1,$8D,$1D,$71,$BC,$75,$66,$FC,$B2,$9B,$51,$52);
var
  TestHash: TDCP_sha256;
  TestOut: array[0..31] of byte;
begin
  TestHash:= TDCP_sha256.Create;
  try
    TestHash.Init;
    TestHash.UpdateStr('abc');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test1Out,Sizeof(Test1Out),'Test 1');
    TestHash.Init;
    TestHash.UpdateStr('abcdefghijklmnopqrstuvwxyz');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test2Out,Sizeof(Test2Out),'Test 2');
  finally
    TestHash.Free;
  end;
end;

procedure THashAlgorithms.SHA384;
const
  Test1Out: array[0..47] of byte=
    ($9B,$7C,$E7,$C7,$AF,$46,$E4,$00,$A3,$7C,$80,$99,$CB,$4B,$BB,$5D,
     $04,$08,$06,$1D,$D7,$4C,$DB,$5D,$AC,$76,$61,$BE,$D1,$E5,$37,$24,
     $BD,$07,$F2,$99,$E2,$65,$F4,$00,$80,$2A,$48,$D2,$E0,$B2,$09,$2C);
  Test2Out: array[0..47] of byte=
    ($88,$60,$A0,$9A,$1B,$A1,$71,$34,$B1,$C7,$9A,$93,$32,$F8,$67,$F6,
     $35,$4E,$48,$CA,$97,$40,$73,$20,$5F,$AA,$C5,$D8,$3D,$66,$F4,$E6,
     $FD,$36,$2F,$BC,$EA,$2A,$CE,$9A,$ED,$07,$88,$14,$AE,$9F,$41,$E4);
var
  TestHash: TDCP_sha384;
  TestOut: array[0..47] of byte;
begin
  TestHash:= TDCP_sha384.Create;
  try
    TestHash.Init;
    TestHash.UpdateStr('abc');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test1Out,Sizeof(Test1Out),'Test 1');
    TestHash.Init;
    TestHash.UpdateStr('abcdefghijklmnopqrstuvwxyz');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test2Out,Sizeof(Test2Out),'Test 2');
  finally
    TestHash.Free;
  end;
end;

procedure THashAlgorithms.SHA512;
const
  Test1Out: array[0..63] of byte=
    ($AD,$D8,$B8,$15,$4D,$F7,$A7,$34,$D2,$94,$7A,$98,$1F,$4E,$61,$C5,
     $36,$67,$10,$D6,$10,$04,$0E,$5B,$54,$89,$4D,$10,$06,$E8,$92,$83,
     $CB,$A0,$82,$28,$7E,$D5,$DD,$4C,$25,$CD,$AA,$5A,$F5,$6D,$24,$AB,
     $9F,$BE,$DC,$56,$89,$71,$30,$B0,$B5,$F3,$E5,$0C,$7F,$9E,$E6,$DF);
  Test2Out: array[0..63] of byte=
    ($13,$44,$90,$32,$54,$C8,$92,$2B,$4B,$B4,$3A,$7E,$CD,$A8,$5D,$37,
     $39,$BE,$35,$3B,$74,$C3,$8A,$AE,$48,$BB,$06,$34,$9F,$75,$31,$35,
     $FF,$0E,$6D,$47,$0B,$F9,$2B,$A3,$5F,$B6,$B7,$33,$7F,$1A,$AA,$1F,
     $10,$80,$6C,$51,$AF,$71,$21,$B8,$51,$8C,$8B,$C2,$2D,$4E,$57,$AD);
var
  TestHash: TDCP_sha512;
  TestOut: array[0..63] of byte;
begin
  TestHash:= TDCP_sha512.Create;
  try
    TestHash.Init;
    TestHash.UpdateStr('abc');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test1Out,Sizeof(Test1Out),'Test 1');
    TestHash.Init;
    TestHash.UpdateStr('abcdefghijklmnopqrstuvwxyz');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test2Out,Sizeof(Test2Out),'Test 2');
  finally
    TestHash.Free;
  end;
end;

procedure THashAlgorithms.Tiger;
const
  Test1Out: array[0..23] of byte=
    ($70,$19,$81,$91,$F5,$B6,$E9,$01,
     $C8,$84,$A5,$E6,$1A,$8F,$16,$EA,
     $0E,$CE,$41,$96,$92,$89,$21,$0E);
  Test2Out: array[0..23] of byte=
    ($97,$FB,$5A,$ED,$48,$23,$9F,$D2,
     $48,$74,$22,$F4,$28,$9C,$A2,$77,
     $4F,$CC,$39,$B1,$01,$9B,$6C,$04);
var
  TestHash: TDCP_tiger;
  TestOut: array[0..2] of int64;
begin
  TestHash:= TDCP_tiger.Create;
  try
    TestHash.Init;
    TestHash.UpdateStr('abc');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test1Out,Sizeof(Test1Out),'Test 1');
    TestHash.Init;
    TestHash.UpdateStr('abcdefghijklmnopqrstuvwxyz');
    TestHash.Final(TestOut);
    Assert.AreEqualMemory(@TestOut,@Test2Out,Sizeof(Test2Out),'Test 2');
  finally
    TestHash.Free;
  end;
end;

end.

