unit TestHashes;

interface

uses
  DUnitX.TestFramework, DCPcrypt2;

type
  [TestFixture]
  TTestHashes = class
  private
    procedure TestHash(HashClass: TDCP_hashclass);
  public
    [Test]
    procedure TestHaval;
    [Test]
    procedure TestMD4;
    [Test]
    procedure TestMD5;
    [Test]
    procedure TestRipeMD128;
    [Test]
    procedure TestRipeMD160;
    [Test]
    procedure TestSHA1;
    [Test]
    procedure TestSHA256;
    [Test]
    procedure TestSHA384;
    [Test]
    procedure TestSHA512;
    [Test]
    procedure TestTiger;
  end;

implementation

uses
  DCPhaval, DCPmd4, DCPmd5, DCPripemd128, DCPripemd160, DCPsha1, DCPsha256,
  DCPsha512, DCPtiger;

{ TTestHashes }

procedure TTestHashes.TestHash(HashClass: TDCP_hashclass);
var
  AHash: TDCP_hash;
begin
  AHash := HashClass.Create;
  try
    Assert.IsTrue(AHash.SelfTest);
  finally
    AHash.Free;
  end;
end;

procedure TTestHashes.TestHaval;
begin
  TestHash(TDCP_haval);
end;

procedure TTestHashes.TestMD4;
begin
  TestHash(TDCP_md4);
end;

procedure TTestHashes.TestMD5;
begin
  TestHash(TDCP_md5);
end;

procedure TTestHashes.TestRipeMD128;
begin
  TestHash(TDCP_ripemd128);
end;

procedure TTestHashes.TestRipeMD160;
begin
  TestHash(TDCP_ripemd160);
end;

procedure TTestHashes.TestSHA1;
begin
  TestHash(TDCP_sha1);
end;

procedure TTestHashes.TestSHA256;
begin
  TestHash(TDCP_sha256);
end;

procedure TTestHashes.TestSHA384;
begin
  TestHash(TDCP_sha384);
end;

procedure TTestHashes.TestSHA512;
begin
  TestHash(TDCP_sha512);
end;

procedure TTestHashes.TestTiger;
begin
  TestHash(TDCP_tiger);
end;

end.
