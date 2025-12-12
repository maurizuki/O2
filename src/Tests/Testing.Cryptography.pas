unit Testing.Cryptography;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TCipherAlgorithms = class
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
  DCPsha256, DCPsha512, DCPtiger, DCPcrypt2;

{ TCipherAlgorithms }

procedure TCipherAlgorithms.TripleDES;
const
  Key: array[0..23] of byte=
    ($01,$23,$45,$67,$89,$ab,$cd,$ef,$fe,$dc,$ba,$98,
     $76,$54,$32,$10,$89,$ab,$cd,$ef,$01,$23,$45,$67);
  PlainText: array[0..7] of byte=
    ($01,$23,$45,$67,$89,$ab,$cd,$e7);
  CipherText: array[0..7] of byte=
    ($de,$0b,$7c,$06,$ae,$5e,$0e,$d5);
var
  Cipher: TDCP_3des;
  Block: array[0..7] of byte;
begin
  Cipher:= TDCP_3des.Create;
  try
    Cipher.Init(Key,Sizeof(Key)*8,nil);
    Cipher.EncryptECB(PlainText,Block);
    Assert.AreEqualMemory(@Block,@CipherText,Sizeof(CipherText),'Test 1 - Encrypt');
    Cipher.DecryptECB(Block,Block);
    Assert.AreEqualMemory(@Block,@PlainText,Sizeof(PlainText),'Test 1 - Decrypt');
  finally
    Cipher.Free;
  end;
end;

procedure TCipherAlgorithms.Blowfish;
const
  Key1: array[0..7] of byte= ($00,$00,$00,$00,$00,$00,$00,$00);
  Key2: array[0..7] of byte= ($7C,$A1,$10,$45,$4A,$1A,$6E,$57);
  InData1: array[0..7] of byte= ($00,$00,$00,$00,$00,$00,$00,$00);
  InData2: array[0..7] of byte= ($01,$A1,$D6,$D0,$39,$77,$67,$42);
  OutData1: array[0..7] of byte= ($4E,$F9,$97,$45,$61,$98,$DD,$78);
  OutData2: array[0..7] of byte= ($59,$C6,$82,$45,$EB,$05,$28,$2B);
var
  Cipher: TDCP_blowfish;
  Data: array[0..7] of byte;
begin
  Cipher:= TDCP_blowfish.Create;
  try
    Cipher.Init(Key1,Sizeof(Key1)*8,nil);
    Cipher.EncryptECB(InData1,Data);
    Assert.AreEqualMemory(@Data,@OutData1,Sizeof(Data),'Test 1 - Encrypt');
    Cipher.Reset;
    Cipher.DecryptECB(Data,Data);
    Assert.AreEqualMemory(@Data,@InData1,Sizeof(Data),'Test 1 - Decrypt');
    Cipher.Burn;
    Cipher.Init(Key2,Sizeof(Key2)*8,nil);
    Cipher.EncryptECB(InData2,Data);
    Assert.AreEqualMemory(@Data,@OutData2,Sizeof(Data),'Test 2 - Encrypt');
    Cipher.Reset;
    Cipher.DecryptECB(Data,Data);
    Assert.AreEqualMemory(@Data,@InData2,Sizeof(Data),'Test 2 - Decrypt');
    Cipher.Burn;
  finally
    Cipher.Free;
  end;
end;

procedure TCipherAlgorithms.CAST128;
const
  Key: array[0..15] of byte=
    ($01,$23,$45,$67,$12,$34,$56,$78,$23,$45,$67,$89,$34,$56,$78,$9A);
  InBlock: array[0..7] of byte=
    ($01,$23,$45,$67,$89,$AB,$CD,$EF);
  Out128: array[0..7] of byte=
    ($23,$8B,$4F,$E5,$84,$7E,$44,$B2);
  Out80: array[0..7] of byte=
    ($EB,$6A,$71,$1A,$2C,$02,$27,$1B);
  Out40: array[0..7] of byte=
    ($7A,$C8,$16,$D1,$6E,$9B,$30,$2E);
var
  Block: array[0..7] of byte;
  Cipher: TDCP_cast128;
begin
  Cipher:= TDCP_cast128.Create;
  try
    Cipher.Init(Key,128,nil);
    Cipher.EncryptECB(InBlock,Block);
    Assert.AreEqualMemory(@Block,@Out128,8,'Test 1 - Encrypt');
    Cipher.DecryptECB(Block,Block);
    Assert.AreEqualMemory(@Block,@InBlock,8,'Test 1 - Decrypt');
    Cipher.Burn;
    Cipher.Init(Key,80,nil);
    Cipher.EncryptECB(InBlock,Block);
    Assert.AreEqualMemory(@Block,@Out80,8,'Test 2 - Encrypt');
    Cipher.DecryptECB(Block,Block);
    Assert.AreEqualMemory(@Block,@InBlock,8,'Test 2 - Decrypt');
    Cipher.Burn;
    Cipher.Init(Key,40,nil);
    Cipher.EncryptECB(InBlock,Block);
    Assert.AreEqualMemory(@Block,@Out40,8,'Test 3 - Encrypt');
    Cipher.DecryptECB(Block,Block);
    Assert.AreEqualMemory(@Block,@InBlock,8,'Test 3 - Decrypt');
    Cipher.Burn;
  finally
    Cipher.Free;
  end;
end;

procedure TCipherAlgorithms.CAST256;
const
  Key1: array[0..15] of byte=
    ($23,$42,$bb,$9e,$fa,$38,$54,$2c,$0a,$f7,$56,$47,$f2,$9f,$61,$5d);
  InBlock1: array[0..15] of byte=
    ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0c,$9b,$28,$07);
  OutBlock1: array[0..15] of byte=
    ($96,$3a,$8a,$50,$ce,$b5,$4d,$08,$e0,$de,$e0,$f1,$d0,$41,$3d,$cf);
  Key2: array[0..23] of byte=
    ($23,$42,$bb,$9e,$fa,$38,$54,$2c,$be,$d0,$ac,$83,$94,$0a,$c2,$98,$ba,$c7,$7a,$77,$17,$94,$28,$63);
  InBlock2: array[0..15] of byte=
    ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$de,$25,$5a,$ff);
  OutBlock2: array[0..15] of byte=
    ($2b,$c1,$92,$9f,$30,$13,$47,$a9,$9d,$3f,$3e,$45,$ad,$34,$01,$e8);
  Key3: array[0..31] of byte=
    ($23,$42,$bb,$9e,$fa,$38,$54,$2c,$be,$d0,$ac,$83,$94,$0a,$c2,$98,$8d,$7c,$47,$ce,$26,$49,$08,$46,$1c,$c1,$b5,$13,$7a,$e6,$b6,$04);
  InBlock3: array[0..15] of byte=
    ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$c5,$fc,$eb,$19);
  OutBlock3: array[0..15] of byte=
    ($1e,$2e,$bc,$6c,$9f,$2e,$43,$8e,$1d,$90,$d9,$b9,$c6,$85,$32,$86);
var
  Block: array[0..15] of byte;
  Cipher: TDCP_cast256;
begin
  Cipher:= TDCP_cast256.Create;
  try
    Cipher.Init(Key1,Sizeof(Key1)*8,nil);
    Cipher.EncryptECB(InBlock1,Block);
    Assert.AreEqualMemory(@Block,@OutBlock1,8,'Test 1 - Encrypt');
    Cipher.DecryptECB(Block,Block);
    Assert.AreEqualMemory(@Block,@InBlock1,16,'Test 1 - Decrypt');
    Cipher.Burn;
    Cipher.Init(Key2,Sizeof(Key2)*8,nil);
    Cipher.EncryptECB(InBlock2,Block);
    Assert.AreEqualMemory(@Block,@OutBlock2,8,'Test 2 - Encrypt');
    Cipher.DecryptECB(Block,Block);
    Assert.AreEqualMemory(@Block,@InBlock2,16,'Test 2 - Decrypt');
    Cipher.Burn;
    Cipher.Init(Key3,Sizeof(Key3)*8,nil);
    Cipher.EncryptECB(InBlock3,Block);
    Assert.AreEqualMemory(@Block,@OutBlock3,8,'Test 3 - Encrypt');
    Cipher.DecryptECB(Block,Block);
    Assert.AreEqualMemory(@Block,@InBlock3,16,'Test 3 - Decrypt');
    Cipher.Burn;
  finally
    Cipher.Free;
  end;
end;

procedure TCipherAlgorithms.DES;
const
  InData1: array[0..7] of byte=
    ($07,$56,$D8,$E0,$77,$47,$61,$D2);
  OutData1: array[0..7] of byte=
    ($0C,$D3,$DA,$02,$00,$21,$DC,$09);
  Key1: array[0..7] of byte=
    ($01,$70,$F1,$75,$46,$8F,$B5,$E6);
  InData2: array[0..7] of byte=
    ($48,$0D,$39,$00,$6E,$E7,$62,$F2);
  OutData2: array[0..7] of byte=
    ($A1,$F9,$91,$55,$41,$02,$0B,$56);
  Key2: array[0..7] of byte=
    ($02,$58,$16,$16,$46,$29,$B0,$07);
var
  Cipher: TDCP_des;
  Data: array[0..7] of byte;
begin
  Cipher:= TDCP_des.Create;
  try
    Cipher.Init(Key1,Sizeof(Key1)*8,nil);
    Cipher.EncryptECB(InData1,Data);
    Assert.AreEqualMemory(@Data,@OutData1,Sizeof(Data),'Test 1 - Encrypt');
    Cipher.DecryptECB(Data,Data);
    Assert.AreEqualMemory(@Data,@InData1,Sizeof(Data),'Test 1 - Decrypt');
    Cipher.Burn;
    Cipher.Init(Key2,Sizeof(Key2)*8,nil);
    Cipher.EncryptECB(InData2,Data);
    Assert.AreEqualMemory(@Data,@OutData2,Sizeof(Data),'Test 2 - Encrypt');
    Cipher.DecryptECB(Data,Data);
    Assert.AreEqualMemory(@Data,@InData2,Sizeof(Data),'Test 2 - Decrypt');
    Cipher.Burn;
  finally
    Cipher.Free;
  end;
end;

procedure TCipherAlgorithms.ICE;
const
  Key1: array[0..7] of byte= ($de,$ad,$be,$ef,$01,$23,$45,$67);
  InData1: array[0..7] of byte= ($fe,$dc,$ba,$98,$76,$54,$32,$10);
  OutData1: array[0..7] of byte= ($7d,$6e,$f1,$ef,$30,$d4,$7a,$96);
var
  Cipher: TDCP_ice;
  Data: array[0..7] of byte;
begin
  Cipher:= TDCP_ice.Create;
  try
    Cipher.Init(Key1,Sizeof(Key1)*8,nil);
    Cipher.EncryptECB(InData1,Data);
    Assert.AreEqualMemory(@Data,@OutData1,Sizeof(Data),'Test 1 - Encrypt');
    Cipher.Reset;
    Cipher.DecryptECB(Data,Data);
    Assert.AreEqualMemory(@Data,@InData1,Sizeof(Data),'Test 1 - Decrypt');
    Cipher.Burn;
  finally
    Cipher.Free;
  end;
end;

procedure TCipherAlgorithms.ICE2;
const
  Key1: array[0..15] of byte=
    ($00,$11,$22,$33,$44,$55,$66,$77,$88,$99,$aa,$bb,$cc,$dd,$ee,$ff);
  InData1: array[0..7] of byte= ($fe,$dc,$ba,$98,$76,$54,$32,$10);
  OutData1: array[0..7] of byte= ($f9,$48,$40,$d8,$69,$72,$f2,$1c);
var
  Cipher: TDCP_ice2;
  Data: array[0..7] of byte;
begin
  Cipher:= TDCP_ice2.Create;
  try
    Cipher.Init(Key1,Sizeof(Key1)*8,nil);
    Cipher.EncryptECB(InData1,Data);
    Assert.AreEqualMemory(@Data,@OutData1,Sizeof(Data),'Test 1 - Encrypt');
    Cipher.Reset;
    Cipher.DecryptECB(Data,Data);
    Assert.AreEqualMemory(@Data,@InData1,Sizeof(Data),'Test 1 - Decrypt');
    Cipher.Burn;
  finally
    Cipher.Free;
  end;
end;

procedure TCipherAlgorithms.IDEA;
const
  Key1: array[0..15] of byte=
    ($3A,$98,$4E,$20,$00,$19,$5D,$B3,$2E,$E5,$01,$C8,$C4,$7C,$EA,$60);
  InData1: array[0..7] of byte=
    ($01,$02,$03,$04,$05,$06,$07,$08);
  OutData1: array[0..7] of byte=
    ($97,$BC,$D8,$20,$07,$80,$DA,$86);
  Key2: array[0..15] of byte=
    ($00,$64,$00,$C8,$01,$2C,$01,$90,$01,$F4,$02,$58,$02,$BC,$03,$20);
  InData2: array[0..7] of byte=
    ($05,$32,$0A,$64,$14,$C8,$19,$FA);
  OutData2: array[0..7] of byte=
    ($65,$BE,$87,$E7,$A2,$53,$8A,$ED);
var
  Cipher: TDCP_idea;
  Data: array[0..7] of byte;
begin
  Cipher:= TDCP_idea.Create;
  try
    Cipher.Init(Key1,Sizeof(Key1)*8,nil);
    Cipher.EncryptECB(InData1,Data);
    Assert.AreEqualMemory(@Data,@OutData1,Sizeof(Data),'Test 1 - Encrypt');
    Cipher.DecryptECB(Data,Data);
    Assert.AreEqualMemory(@Data,@InData1,Sizeof(Data),'Test 1 - Decrypt');
    Cipher.Burn;
    Cipher.Init(Key2,Sizeof(Key2)*8,nil);
    Cipher.EncryptECB(InData2,Data);
    Assert.AreEqualMemory(@Data,@OutData2,Sizeof(Data),'Test 2 - Encrypt');
    Cipher.DecryptECB(Data,Data);
    Assert.AreEqualMemory(@Data,@InData2,Sizeof(Data),'Test 2 - Decrypt');
    Cipher.Burn;
  finally
    Cipher.Free;
  end;
end;

procedure TCipherAlgorithms.MARS;
const
  Key1: array[0..3] of dword=
    ($deb35132,$83c296de,$39069e6b,$994c2438);
  Key2: array[0..5] of dword=
    ($a5391779,$1a58048b,$a853a993,$1d41102c,$088658d1,$954d8738);
  Key3: array[0..7] of dword=
    ($9867a1fb,$22ef7a3e,$8ce27c31,$a3e1aa02,$3ccce5e8,$2aa8beed,$9ac3db99,$27725ed6);
  Plain1: array[0..3] of dword= ($deb35132,$83c296de,$39069e6b,$994c2438);
  Plain2: array[0..3] of dword= ($2dc46167,$d242613e,$adbf4fa8,$8f1583b3);
  Plain3: array[0..3] of dword= ($a4ab4413,$0847c4d3,$1621a7a8,$8493f4d4);
  Cipher1: array[0..3] of dword= ($a91245f9,$4e032db4,$042279c4,$9ba608d7);
  Cipher2: array[0..3] of dword= ($260334cb,$6d587f45,$e0d2bd54,$bd191c57);
  Cipher3: array[0..3] of dword= ($67a1acdd,$be3163e3,$5f9f1c2c,$b8a48fe3);
var
  Cipher: TDCP_mars;
  Block: array[0..3] of dword;
begin
  Cipher:= TDCP_mars.Create;
  try
    Cipher.Init(Key1,Sizeof(Key1)*8,nil);
    Cipher.EncryptECB(Plain1,Block);
    Assert.AreEqualMemory(@Cipher1,@Block,Sizeof(Block),'Test 1 - Encrypt');
    Cipher.DecryptECB(Block,Block);
    Assert.AreEqualMemory(@Plain1,@Block,Sizeof(Block),'Test 1 - Decrypt');
    Cipher.Burn;
    Cipher.Init(Key2,Sizeof(Key2)*8,nil);
    Cipher.EncryptECB(Plain2,Block);
    Assert.AreEqualMemory(@Cipher2,@Block,Sizeof(Block),'Test 2 - Encrypt');
    Cipher.DecryptECB(Block,Block);
    Assert.AreEqualMemory(@Plain2,@Block,Sizeof(Block),'Test 2 - Decrypt');
    Cipher.Burn;
    Cipher.Init(Key3,Sizeof(Key3)*8,nil);
    Cipher.EncryptECB(Plain3,Block);
    Assert.AreEqualMemory(@Cipher3,@Block,Sizeof(Block),'Test 3 - Encrypt');
    Cipher.DecryptECB(Block,Block);
    Assert.AreEqualMemory(@Plain3,@Block,Sizeof(Block),'Test 3 - Decrypt');
    Cipher.Burn;
  finally
    Cipher.Free;
  end;
end;

procedure TCipherAlgorithms.MISTY1;
const
  Key: array[0..15] of byte=
    ($00,$11,$22,$33,$44,$55,$66,$77,$88,$99,$aa,$bb,$cc,$dd,$ee,$ff);
  Plain1: array[0..7] of byte= ($01,$23,$45,$67,$89,$ab,$cd,$ef);
  Plain2: array[0..7] of byte= ($fe,$dc,$ba,$98,$76,$54,$32,$10);
  Cipher1: array[0..7] of byte= ($8b,$1d,$a5,$f5,$6a,$b3,$d0,$7c);
  Cipher2: array[0..7] of byte= ($04,$b6,$82,$40,$b1,$3b,$e9,$5d);
var
  Cipher: TDCP_misty1;
  Block: array[0..7] of byte;
begin
  Cipher:= TDCP_misty1.Create;
  try
    Cipher.Init(Key,Sizeof(Key)*8,nil);
    Cipher.EncryptECB(Plain1,Block);
    Assert.AreEqualMemory(@Cipher1,@Block,Sizeof(Block),'Test 1 - Encrypt');
    Cipher.DecryptECB(Block,Block);
    Assert.AreEqualMemory(@Plain1,@Block,Sizeof(Block),'Test 1 - Decrypt');
    Cipher.EncryptECB(Plain2,Block);
    Assert.AreEqualMemory(@Cipher2,@Block,Sizeof(Block),'Test 2 - Encrypt');
    Cipher.DecryptECB(Block,Block);
    Assert.AreEqualMemory(@Plain2,@Block,Sizeof(Block),'Test 2 - Decrypt');
    Cipher.Burn;
  finally
    Cipher.Free;
  end;
end;

procedure TCipherAlgorithms.RC2;
const
  Key1: array[0..15] of byte=
    ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F);
  InData1: array[0..7] of byte=
    ($00,$00,$00,$00,$00,$00,$00,$00);
  OutData1: array[0..7] of byte=
    ($50,$DC,$01,$62,$BD,$75,$7F,$31);
  Key2: array[0..15] of byte=
    ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01);
  InData2: array[0..7] of byte=
    ($00,$00,$00,$00,$00,$00,$00,$00);
  OutData2: array[0..7] of byte=
    ($21,$82,$9C,$78,$A9,$F9,$C0,$74);
var
  Cipher: TDCP_rc2;
  Data: array[0..7] of byte;
begin
  Cipher:= TDCP_rc2.Create;
  try
    Cipher.Init(Key1,Sizeof(Key1)*8,nil);
    Cipher.EncryptECB(InData1,Data);
    Assert.AreEqualMemory(@Data,@OutData1,Sizeof(Data),'Test 1 - Encrypt');
    Cipher.DecryptECB(Data,Data);
    Assert.AreEqualMemory(@Data,@InData1,Sizeof(Data),'Test 1 - Decrypt');
    Cipher.Burn;
    Cipher.Init(Key2,Sizeof(Key2)*8,nil);
    Cipher.EncryptECB(InData2,Data);
    Assert.AreEqualMemory(@Data,@OutData2,Sizeof(Data),'Test 2 - Encrypt');
    Cipher.DecryptECB(Data,Data);
    Assert.AreEqualMemory(@Data,@InData2,Sizeof(Data),'Test 2 - Decrypt');
    Cipher.Burn;
  finally
    Cipher.Free;
  end;
end;

procedure TCipherAlgorithms.RC4;
const
  Key1: array[0..4] of byte= ($61,$8A,$63,$D2,$FB);
  InData1: array[0..4] of byte= ($DC,$EE,$4C,$F9,$2C);
  OutData1: array[0..4] of byte= ($F1,$38,$29,$C9,$DE);
var
  Cipher: TDCP_rc4;
  Data: array[0..4] of byte;
begin
  Cipher:= TDCP_rc4.Create;
  try
    Cipher.Init(Key1,Sizeof(Key1)*8,nil);
    Cipher.Encrypt(InData1,Data,Sizeof(Data));
    Assert.AreEqualMemory(@Data,@OutData1,Sizeof(Data),'Test 1 - Encrypt');
    Cipher.Reset;
    Cipher.Decrypt(Data,Data,Sizeof(Data));
    Assert.AreEqualMemory(@Data,@InData1,Sizeof(Data),'Test 1 - Decrypt');
    Cipher.Burn;
  finally
    Cipher.Free;
  end;
end;

procedure TCipherAlgorithms.RC5;
const
  Key1: array[0..15] of byte=
    ($DC,$49,$DB,$13,$75,$A5,$58,$4F,$64,$85,$B4,$13,$B5,$F1,$2B,$AF);
  Plain1: array[0..1] of dword=
    ($B7B3422F,$92FC6903);
  Cipher1: array[0..1] of dword=
    ($B278C165,$CC97D184);
  Key2: array[0..15] of byte=
    ($52,$69,$F1,$49,$D4,$1B,$A0,$15,$24,$97,$57,$4D,$7F,$15,$31,$25);
  Plain2: array[0..1] of dword=
    ($B278C165,$CC97D184);
  Cipher2: array[0..1] of dword=
    ($15E444EB,$249831DA);
var
  Cipher: TDCP_rc5;
  Data: array[0..1] of dword;
begin
  Cipher:= TDCP_rc5.Create;
  try
    Cipher.Init(Key1,Sizeof(Key1)*8,nil);
    Cipher.EncryptECB(Plain1,Data);
    Assert.AreEqualMemory(@Data,@Cipher1,Sizeof(Data),'Test 1 - Encrypt');
    Cipher.DecryptECB(Data,Data);
    Assert.AreEqualMemory(@Data,@Plain1,Sizeof(Data),'Test 1 - Decrypt');
    Cipher.Burn;
    Cipher.Init(Key2,Sizeof(Key2)*8,nil);
    Cipher.EncryptECB(Plain2,Data);
    Assert.AreEqualMemory(@Data,@Cipher2,Sizeof(Data),'Test 2 - Encrypt');
    Cipher.DecryptECB(Data,Data);
    Assert.AreEqualMemory(@Data,@Plain2,Sizeof(Data),'Test 2 - Decrypt');
    Cipher.Burn;
  finally
    Cipher.Free;
  end;
end;

procedure TCipherAlgorithms.RC6;
const
  Key1: array[0..15] of byte=
    ($01,$23,$45,$67,$89,$ab,$cd,$ef,$01,$12,$23,$34,$45,$56,$67,$78);
  Plain1: array[0..15] of byte=
    ($02,$13,$24,$35,$46,$57,$68,$79,$8a,$9b,$ac,$bd,$ce,$df,$e0,$f1);
  Cipher1: array[0..15] of byte=
    ($52,$4e,$19,$2f,$47,$15,$c6,$23,$1f,$51,$f6,$36,$7e,$a4,$3f,$18);
  Key2: array[0..31] of byte=
    ($01,$23,$45,$67,$89,$ab,$cd,$ef,$01,$12,$23,$34,$45,$56,$67,$78,
     $89,$9a,$ab,$bc,$cd,$de,$ef,$f0,$10,$32,$54,$76,$98,$ba,$dc,$fe);
  Plain2: array[0..15] of byte=
    ($02,$13,$24,$35,$46,$57,$68,$79,$8a,$9b,$ac,$bd,$ce,$df,$e0,$f1);
  Cipher2: array[0..15] of byte=
    ($c8,$24,$18,$16,$f0,$d7,$e4,$89,$20,$ad,$16,$a1,$67,$4e,$5d,$48);
var
  Cipher: TDCP_rc6;
  Data: array[0..15] of byte;
begin
  Cipher:= TDCP_rc6.Create;
  try
    Cipher.Init(Key1,Sizeof(Key1)*8,nil);
    Cipher.EncryptECB(Plain1,Data);
    Assert.AreEqualMemory(@Data,@Cipher1,Sizeof(Data),'Test 1 - Encrypt');
    Cipher.DecryptECB(Data,Data);
    Assert.AreEqualMemory(@Data,@Plain1,Sizeof(Data),'Test 1 - Decrypt');
    Cipher.Burn;
    Cipher.Init(Key2,Sizeof(Key2)*8,nil);
    Cipher.EncryptECB(Plain2,Data);
    Assert.AreEqualMemory(@Data,@Cipher2,Sizeof(Data),'Test 2 - Encrypt');
    Cipher.DecryptECB(Data,Data);
    Assert.AreEqualMemory(@Data,@Plain2,Sizeof(Data),'Test 2 - Decrypt');
    Cipher.Burn;
  finally
    Cipher.Free;
  end;
end;

procedure TCipherAlgorithms.Rijndael;
const
  Key1: array[0..15] of byte=
    ($00,$01,$02,$03,$05,$06,$07,$08,$0A,$0B,$0C,$0D,$0F,$10,$11,$12);
  InData1: array[0..15] of byte=
    ($50,$68,$12,$A4,$5F,$08,$C8,$89,$B9,$7F,$59,$80,$03,$8B,$83,$59);
  OutData1: array[0..15] of byte=
    ($D8,$F5,$32,$53,$82,$89,$EF,$7D,$06,$B5,$06,$A4,$FD,$5B,$E9,$C9);
  Key2: array[0..23] of byte=
    ($A0,$A1,$A2,$A3,$A5,$A6,$A7,$A8,$AA,$AB,$AC,$AD,$AF,$B0,$B1,$B2,
     $B4,$B5,$B6,$B7,$B9,$BA,$BB,$BC);
  InData2: array[0..15] of byte=
    ($4F,$1C,$76,$9D,$1E,$5B,$05,$52,$C7,$EC,$A8,$4D,$EA,$26,$A5,$49);
  OutData2: array[0..15] of byte=
    ($F3,$84,$72,$10,$D5,$39,$1E,$23,$60,$60,$8E,$5A,$CB,$56,$05,$81);
  Key3: array[0..31] of byte=
    ($00,$01,$02,$03,$05,$06,$07,$08,$0A,$0B,$0C,$0D,$0F,$10,$11,$12,
     $14,$15,$16,$17,$19,$1A,$1B,$1C,$1E,$1F,$20,$21,$23,$24,$25,$26);
  InData3: array[0..15] of byte=
    ($5E,$25,$CA,$78,$F0,$DE,$55,$80,$25,$24,$D3,$8D,$A3,$FE,$44,$56);
  OutData3: array[0..15] of byte=
    ($E8,$B7,$2B,$4E,$8B,$E2,$43,$43,$8C,$9F,$FF,$1F,$0E,$20,$58,$72);
var
  Block: array[0..15] of byte;
  Cipher: TDCP_rijndael;
begin
  Cipher:= TDCP_rijndael.Create;
  try
    Cipher.Init(Key1,Sizeof(Key1)*8,nil);
    Cipher.EncryptECB(InData1,Block);
    Assert.AreEqualMemory(@Block,@OutData1,16,'Test 1 - Encrypt');
    Cipher.DecryptECB(Block,Block);
    Cipher.Burn;
    Assert.AreEqualMemory(@Block,@InData1,16,'Test 1 - Decrypt');
    Cipher.Init(Key2,Sizeof(Key2)*8,nil);
    Cipher.EncryptECB(InData2,Block);
    Assert.AreEqualMemory(@Block,@OutData2,16,'Test 2 - Encrypt');
    Cipher.DecryptECB(Block,Block);
    Cipher.Burn;
    Assert.AreEqualMemory(@Block,@InData2,16,'Test 2 - Decrypt');
    Cipher.Init(Key3,Sizeof(Key3)*8,nil);
    Cipher.EncryptECB(InData3,Block);
    Assert.AreEqualMemory(@Block,@OutData3,16,'Test 3 - Encrypt');
    Cipher.DecryptECB(Block,Block);
    Cipher.Burn;
    Assert.AreEqualMemory(@Block,@InData3,16,'Test 3 - Decrypt');
  finally
    Cipher.Free;
  end;
end;

procedure TCipherAlgorithms.Serpent;
const
  Key1: array[0..15] of byte=
    ($ff,$ee,$dd,$cc,$bb,$aa,$99,$88,$77,$66,$55,$44,$33,$22,$11,$00);
  InData1: array[0..15] of byte=
    ($10,$32,$54,$76,$98,$ba,$dc,$fe,$ef,$cd,$ab,$89,$67,$45,$23,$01);
  OutData1: array[0..15] of byte=
    ($d5,$ba,$a0,$0a,$4b,$b9,$d8,$a7,$c9,$81,$c8,$dc,$90,$d8,$9d,$92);
  Key2: array[0..23] of byte=
    ($88,$99,$aa,$bb,$cc,$dd,$ee,$ff,$ff,$ee,$dd,$cc,$bb,$aa,$99,$88,
     $77,$66,$55,$44,$33,$22,$11,$00);
  InData2: array[0..15] of byte=
    ($10,$32,$54,$76,$98,$ba,$dc,$fe,$ef,$cd,$ab,$89,$67,$45,$23,$01);
  OutData2: array[0..15] of byte=
    ($da,$86,$08,$42,$b7,$20,$80,$2b,$f4,$04,$a4,$c7,$10,$34,$87,$9a);
  Key3: array[0..31] of byte=
    ($00,$11,$22,$33,$44,$55,$66,$77,$88,$99,$aa,$bb,$cc,$dd,$ee,$ff,
     $ff,$ee,$dd,$cc,$bb,$aa,$99,$88,$77,$66,$55,$44,$33,$22,$11,$00);
  InData3: array[0..15] of byte=
    ($10,$32,$54,$76,$98,$ba,$dc,$fe,$ef,$cd,$ab,$89,$67,$45,$23,$01);
  OutData3: array[0..15] of byte=
    ($93,$df,$9a,$3c,$af,$e3,$87,$bd,$99,$9e,$eb,$e3,$93,$a1,$7f,$ca);
var
  Block: array[0..15] of byte;
  Cipher: TDCP_serpent;
begin
  Cipher:= TDCP_serpent.Create;
  try
    Cipher.Init(Key1,Sizeof(Key1)*8,nil);
    Cipher.EncryptECB(InData1,Block);
    Assert.AreEqualMemory(@Block,@OutData1,16,'Test 1 - Encrypt');
    Cipher.DecryptECB(Block,Block);
    Cipher.Burn;
    Assert.AreEqualMemory(@Block,@InData1,16,'Test 1 - Decrypt');
    Cipher.Init(Key2,Sizeof(Key2)*8,nil);
    Cipher.EncryptECB(InData2,Block);
    Assert.AreEqualMemory(@Block,@OutData2,16,'Test 2 - Encrypt');
    Cipher.DecryptECB(Block,Block);
    Cipher.Burn;
    Assert.AreEqualMemory(@Block,@InData2,16,'Test 2 - Decrypt');
    Cipher.Init(Key3,Sizeof(Key3)*8,nil);
    Cipher.EncryptECB(InData3,Block);
    Assert.AreEqualMemory(@Block,@OutData3,16,'Test 3 - Encrypt');
    Cipher.DecryptECB(Block,Block);
    Cipher.Burn;
    Assert.AreEqualMemory(@Block,@InData3,16,'Test 3 - Decrypt');
  finally
    Cipher.Free;
  end;
end;

procedure TCipherAlgorithms.TEA;
const
  Key: array[0..3] of dword= ($12345678,$9ABCDEF0,$0FEDCBA9,$87654321);
  PT: array[0..1] of dword= ($12345678,$9ABCDEF0);
var
  Data: array[0..1] of dword;
  Cipher: TDCP_tea;
begin
  Cipher:= TDCP_tea.Create;
  try
    Cipher.Init(Key,Sizeof(Key)*8,nil);
    Cipher.EncryptECB(PT,Data);
    Assert.AreNotEqualMemory(@Data,@PT,Sizeof(PT),'Test 1 - Encrypt');
    Cipher.DecryptECB(Data,Data);
    Assert.AreEqualMemory(@Data,@PT,Sizeof(PT),'Test 1 - Decrypt');
    Cipher.Burn;
  finally
    Cipher.Free;
  end;
end;

procedure TCipherAlgorithms.ThinICE;
const
  Key1: array[0..7] of byte= ($de,$ad,$be,$ef,$01,$23,$45,$67);
  InData1: array[0..7] of byte= ($fe,$dc,$ba,$98,$76,$54,$32,$10);
  OutData1: array[0..7] of byte= ($de,$24,$0d,$83,$a0,$0a,$9c,$c0);
var
  Cipher: TDCP_thinice;
  Data: array[0..7] of byte;
begin
  Cipher:= TDCP_thinice.Create;
  try
    Cipher.Init(Key1,Sizeof(Key1)*8,nil);
    Cipher.EncryptECB(InData1,Data);
    Assert.AreEqualMemory(@Data,@OutData1,Sizeof(Data),'Test 1 - Encrypt');
    Cipher.Reset;
    Cipher.DecryptECB(Data,Data);
    Assert.AreEqualMemory(@Data,@InData1,Sizeof(Data),'Test 1 - Decrypt');
    Cipher.Burn;
  finally
    Cipher.Free;
  end;
end;

procedure TCipherAlgorithms.Twofish;
const
  Out128: array[0..15] of byte=
    ($5D,$9D,$4E,$EF,$FA,$91,$51,$57,$55,$24,$F1,$15,$81,$5A,$12,$E0);
  Out192: array[0..15] of byte=
    ($E7,$54,$49,$21,$2B,$EE,$F9,$F4,$A3,$90,$BD,$86,$0A,$64,$09,$41);
  Out256: array[0..15] of byte=
    ($37,$FE,$26,$FF,$1C,$F6,$61,$75,$F5,$DD,$F4,$C3,$3B,$97,$A2,$05);
var
  i: integer;
  Key: array[0..31] of byte;
  Block: array[0..15] of byte;
  Cipher: TDCP_twofish;
begin
  Cipher:= TDCP_twofish.Create;
  try
    FillChar(Key,Sizeof(Key),0);
    FillChar(Block,Sizeof(Block),0);
    for i:= 1 to 49 do
    begin
      Cipher.Init(Key,128,nil);
      Move(Block,Key,16);
      Cipher.EncryptECB(Block,Block);
      Cipher.Burn;
    end;
    Assert.AreEqualMemory(@Block,@Out128,16,'Test 1 - Encrypt');
    FillChar(Key,Sizeof(Key),0);
    FillChar(Block,Sizeof(Block),0);
    for i:= 1 to 49 do
    begin
      Cipher.Init(Key,192,nil);
      Move(Key[0],Key[16],8);
      Move(Block,Key,16);
      Cipher.EncryptECB(Block,Block);
      Cipher.Burn;
    end;
    Assert.AreEqualMemory(@Block,@Out192,16,'Test 2 - Encrypt');
    FillChar(Key,Sizeof(Key),0);
    FillChar(Block,Sizeof(Block),0);
    for i:= 1 to 49 do
    begin
      Cipher.Init(Key,256,nil);
      Move(Key[0],Key[16],16);
      Move(Block,Key,16);
      Cipher.EncryptECB(Block,Block);
      Cipher.Burn;
    end;
    Assert.AreEqualMemory(@Block,@Out256,16,'Test 3 - Encrypt');
    Cipher.Burn;
  finally
    Cipher.Free;
  end;
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

