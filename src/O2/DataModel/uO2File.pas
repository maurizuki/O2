{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2024 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uO2File;

interface

uses
  Classes, SysUtils, DCPcrypt2, uO2Defs, uO2Classes, uO2Objects, uO2Relations,
  uO2Rules;

type
  IPasswordProvider = interface
    function TryGetPassword(var Password: string): Boolean;
  end;

  TO2File = class(TO2Persistent)
  private
    FFileName: string;
    FVersion: WordRec;
    FEncrypted: Boolean;
    FCipher: TO2Cipher;
    FHash: TO2Hash;
    FIV: array[0..15] of Byte;
    FCRC32: Longword;
    FPassword: string;
    FModified: Boolean;
    FTitle: string;
    FDescription: string;
    FAuthor: string;
    FObjects: TO2Objects;
    FRelations: TO2Relations;
    FRules: TO2Rules;

    procedure SetTitle(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetAuthor(const Value: string);
    procedure SetObjects(const Value: TO2Objects);
    procedure SetRelations(const Value: TO2Relations);
    procedure SetRules(const Value: TO2Rules);

    procedure OutputDebugNotifyChanges(Item: TO2CollectionItem;
      Action: TO2Notification);
    procedure OutputDebugMetadata(const Name, Value: string); inline;
    procedure ReadMetadata(const Stream: TStream);
    procedure WriteMetadata(const Stream: TStream);
    function GetCipher(IV: Pointer): TDCP_cipher;
    function GetHashClass: TDCP_hashclass;
    procedure Compress(InputStream, OutputStream: TStream);
    procedure Decompress(InputStream, OutputStream: TStream);
    procedure Encrypt(InputStream, OutputStream: TStream);
    procedure Decrypt(InputStream, OutputStream: TStream);
    function UseCompression: Boolean;
    function UseIV: Boolean;
  protected
    procedure NotifyChanges(Item: TO2CollectionItem;
      Action: TO2Notification); override;
  public
    constructor Create;
    destructor Destroy; override;

    function GetObjectRelation(const AObject: TO2Object;
      const ARelation: TO2Relation): TO2ObjRelation;
    function GetObjectRelations(const AObject: TO2Object): TO2ObjRelations;

    procedure Load(PasswordProvider: IPasswordProvider);
    procedure Save;

    property FileName: string read FFileName write FFileName;
    property Encrypted: Boolean read FEncrypted write FEncrypted;
    property Cipher: TO2Cipher read FCipher write FCipher;
    property Hash: TO2Hash read FHash write FHash;
    property Password: string read FPassword write FPassword;
    property Modified: Boolean read FModified write FModified;
  published
    property Title: string read FTitle write SetTitle;
    property Description: string read FDescription write SetDescription;
    property Author: string read FAuthor write SetAuthor;
    property Objects: TO2Objects read FObjects write SetObjects;
    property Relations: TO2Relations read FRelations write SetRelations;
    property Rules: TO2Rules read FRules write SetRules;
  end;

implementation

uses
  Windows, TypInfo, ZLib, SZCRC32,
  DCPblowfish, DCPcast128, DCPcast256, DCPdes, DCPice,
  DCPidea, DCPmars, DCPmisty1, DCPrc2, DCPrc4, DCPrc5,
  DCPrc6, DCPrijndael, DCPserpent, DCPtea, DCPtwofish,
  DCPhaval, DCPmd4, DCPmd5, DCPripemd128, DCPripemd160,
  DCPsha1, DCPsha256, DCPsha512, DCPtiger,
  uXmlSerialization;

resourcestring
  SUnsupportedFileType = 'File type not supported.';
  SUnsupportedFileVersion = 'File version not supported (%d.%d).';
  SUnsupportedCipher = 'Cipher not supported ($%.2x).';
  SUnsupportedHash = 'Hash algorithm not supported ($%.2x).';
  SWrongPassword = 'Wrong password.';

{ TO2File }

constructor TO2File.Create;
begin
  inherited Create;
  FFileName := '';
  FEncrypted := False;
  FCipher := ocDefault;
  FHash := ohDefault;
  FPassword := '';
  FModified := False;
  FTitle := '';
  FDescription := '';
  FAuthor := '';
  FObjects := TO2Objects.Create(Self);
  FRelations := TO2Relations.Create(Self);
  FRules := TO2Rules.Create(Self);
end;

destructor TO2File.Destroy;
begin
  FRules.Free;
  FRelations.Free;
  FObjects.Free;
  inherited Destroy;
end;

function TO2File.GetObjectRelation(const AObject: TO2Object;
  const ARelation: TO2Relation): TO2ObjRelation;
begin
  if SameText(ARelation.ObjectID1, AObject.ObjectID) then
    Exit(TO2ObjRelation.Create(ARelation,
      Objects.FindObjectByID(ARelation.ObjectID2), ARelation.Role2));

  if SameText(ARelation.ObjectID2, AObject.ObjectID) then
    Exit(TO2ObjRelation.Create(ARelation,
      Objects.FindObjectByID(ARelation.ObjectID1), ARelation.Role1));

  Result := nil;
end;

function TO2File.GetObjectRelations(const AObject: TO2Object): TO2ObjRelations;
var
  ARelation: TO2Relation;
  AObjRelation: TO2ObjRelation;
begin
  Result := TO2ObjRelations.Create;

  for ARelation in Relations do
  begin
    AObjRelation := GetObjectRelation(AObject, ARelation);
    if Assigned(AObjRelation) then Result.Add(AObjRelation);
  end;
end;

procedure TO2File.Load(PasswordProvider: IPasswordProvider);
var
  XmlReader: TXmlReader;
  XmlStream, RawStream: TMemoryStream;
  InputStream: TFileStream;
  APassword: string;
begin
  InputStream := TFileStream.Create(FFileName, fmOpenRead);
  try
    ReadMetadata(InputStream);

    if FEncrypted and not PasswordProvider.TryGetPassword(APassword) then Abort;
    FPassword := APassword;

    XmlStream := TMemoryStream.Create;
    try
      RawStream := TMemoryStream.Create;
      try
        if FEncrypted then
        begin
          Decrypt(InputStream, RawStream);

          RawStream.Position := 0;
          if FCRC32 <> SZCRC32FullStream(RawStream) then
            raise Exception.Create(SWrongPassword);
        end
        else
          RawStream.CopyFrom(InputStream,
            InputStream.Size - InputStream.Position);

        if UseCompression then
          Decompress(RawStream, XmlStream)
        else
          XmlStream.LoadFromStream(RawStream);
      finally
        RawStream.Free;
      end;

      XmlStream.Position := 0;
      XmlReader := TXmlReader.Create(Self);
      try
        XmlReader.LoadFromStream(XmlStream);
      finally
        XmlReader.Free;
      end;
    finally
      XmlStream.Free;
    end;
  finally
    InputStream.Free;
  end;
  FModified := False;
end;

procedure TO2File.Save;
var
  XmlWriter: TXmlWriter;
  XmlStream, RawStream: TMemoryStream;
  OutputStream: TFileStream;
  I: Integer;
begin
  RawStream := TMemoryStream.Create;
  try
    XmlStream := TMemoryStream.Create;
    try
      XmlWriter := TXmlWriter.Create(Self, O2FileSchemaLocation);
      try
        XmlWriter.SaveToStream(XmlStream);
      finally
        XmlWriter.Free;
      end;

      Compress(XmlStream, RawStream);
    finally
      XmlStream.Free;
    end;

    if FEncrypted then
    begin
      for I := Low(FIV) to High(FIV) do FIV[I] := Random(256);

      RawStream.Position := 0;
      FCRC32 := SZCRC32FullStream(RawStream);
    end;

    OutputStream := TFileStream.Create(FFileName, fmCreate);
    try
      WriteMetadata(OutputStream);

      RawStream.Position := 0;
      if FEncrypted then
        Encrypt(RawStream, OutputStream)
      else
        RawStream.SaveToStream(OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    RawStream.Free;
  end;
end;

procedure TO2File.OutputDebugNotifyChanges(Item: TO2CollectionItem;
  Action: TO2Notification);
const
  DebugOutputFmt = 'O2 File: %s. Item Class: %s. Action Type: %s.';
var
  DebugOutput, ItemClass: string;
begin
  if Assigned(Item) then
    ItemClass := Item.ClassName
  else
    ItemClass := ClassName;

  DebugOutput := Format(DebugOutputFmt, [FFileName, ItemClass,
    GetEnumName(TypeInfo(TO2Notification), Integer(Action))]);

  OutputDebugString(PChar(DebugOutput));
end;

procedure TO2File.NotifyChanges(Item: TO2CollectionItem;
  Action: TO2Notification);
begin
  FModified := True;

  OutputDebugNotifyChanges(Item, Action);

  if (Action in [onExtracting, onDeleting])
    and Assigned(Item) and (Item is TO2Object) then
    Relations.DeleteObjectRelations(TO2Object(Item));
end;

procedure TO2File.OutputDebugMetadata(const Name, Value: string);
begin
  OutputDebugString(PChar(Format('O2 File: %s. %s: %s.',
    [FFileName, Name, Value])));
end;

procedure TO2File.ReadMetadata(const Stream: TStream);
var
  ContentType: TGUID;
  DebugStr: string;
  I: Integer;
begin
  Stream.Read(ContentType, SizeOf(ContentType));
  OutputDebugMetadata('Content Type', GUIDToString(ContentType));

  if not IsEqualGUID(O2FileGUID, ContentType) then
    raise Exception.Create(SUnsupportedFileType);

  Stream.Read(FVersion, SizeOf(FVersion));
  OutputDebugMetadata('Version', Format('%d.%d', [FVersion.Hi, FVersion.Lo]));

  if Word(FVersion) > Word(O2FileVersion) then
    raise Exception.CreateFmt(SUnsupportedFileVersion,
      [FVersion.Hi, FVersion.Lo]);

  Stream.Read(FEncrypted, SizeOf(FEncrypted));
  OutputDebugMetadata('Encrypted', BoolToStr(FEncrypted, True));

  Stream.Read(FCipher, SizeOf(FCipher));
  if not CipherToIdent(FCipher, DebugStr) then
    DebugStr := Format('%s%.2x', [HexDisplayPrefix, FCipher]);
  OutputDebugMetadata('Cipher', DebugStr);

  Stream.Read(FHash, SizeOf(FHash));
  if not HashToIdent(FHash, DebugStr) then
    DebugStr := Format('%s%.2x', [HexDisplayPrefix, FHash]);
  OutputDebugMetadata('Hash', DebugStr);

  if UseIV then
  begin
    Stream.Read(FIV, SizeOf(FIV));

    DebugStr := '';
    for I := Low(FIV) to High(FIV) do
      DebugStr := Format('%s %s%.2x', [DebugStr, HexDisplayPrefix, FIV[I]]);
    OutputDebugMetadata('IV', TrimLeft(DebugStr));
  end;

  Stream.Read(FCRC32, SizeOf(FCRC32));
  OutputDebugMetadata('CRC32', Format('%s%.8x', [HexDisplayPrefix, FCRC32]));
end;

procedure TO2File.WriteMetadata(const Stream: TStream);
begin
  Stream.Write(O2FileGUID, SizeOf(O2FileGUID));
  Stream.Write(O2FileVersion, SizeOf(O2FileVersion));
  Stream.Write(FEncrypted, SizeOf(FEncrypted));
  Stream.Write(FCipher, SizeOf(FCipher));
  Stream.Write(FHash, SizeOf(FHash));
  Stream.Write(FIV, SizeOf(FIV));
  Stream.Write(FCRC32, SizeOf(FCRC32));
end;

function TO2File.GetCipher(IV: Pointer): TDCP_cipher;
var
  HashClass: TDCP_hashclass;
  Hash: TDCP_hash;
  Digest: Pointer;
begin
  case FCipher of
    ocBlowfish: Result := TDCP_blowfish.Create;
    ocCast128:  Result := TDCP_cast128.Create;
    ocCast256:  Result := TDCP_cast256.Create;
    ocDES:      Result := TDCP_des.Create;
    oc3DES:     Result := TDCP_3des.Create;
    ocIce:      Result := TDCP_ice.Create;
    ocThinIce:  Result := TDCP_thinice.Create;
    ocIce2:     Result := TDCP_ice2.Create;
    ocIDEA:     Result := TDCP_idea.Create;
    ocMARS:     Result := TDCP_mars.Create;
    ocMisty1:   Result := TDCP_misty1.Create;
    ocRC2:      Result := TDCP_rc2.Create;
    ocRC4:      Result := TDCP_rc4.Create;
    ocRC5:      Result := TDCP_rc5.Create;
    ocRC6:      Result := TDCP_rc6.Create;
    ocRijndael: Result := TDCP_rijndael.Create;
    ocSerpent:  Result := TDCP_serpent.Create;
    ocTEA:      Result := TDCP_tea.Create;
    ocTwofish:  Result := TDCP_twofish.Create;
    else raise Exception.CreateFmt(SUnsupportedCipher, [Cipher]);
  end;

  HashClass := GetHashClass;

  GetMem(Digest, HashClass.GetHashSize div 8);
  try
    Hash:= HashClass.Create;
    try
      Hash.Init;
      Hash.UpdateStr(AnsiString(FPassword));
      Hash.Final(Digest^);
    finally
      Hash.Free;
    end;

    if Result.MaxKeySize < HashClass.GetHashSize then
      Result.Init(Digest^, Result.MaxKeySize, IV)
    else
      Result.Init(Digest^, HashClass.GetHashSize, IV);

    ZeroMemory(Digest, HashClass.GetHashSize div 8);
  finally
    FreeMem(Digest);
  end;
end;

function TO2File.GetHashClass: TDCP_hashclass;
begin
  case FHash of
    ohHaval:     Result := TDCP_haval;
    ohMD4:       Result := TDCP_md4;
    ohMD5:       Result := TDCP_md5;
    ohRipeMD128: Result := TDCP_ripemd128;
    ohRipeMD160: Result := TDCP_ripemd160;
    ohSHA1:      Result := TDCP_sha1;
    ohSHA256:    Result := TDCP_sha256;
    ohSHA384:    Result := TDCP_sha384;
    ohSHA512:    Result := TDCP_sha512;
    ohTiger:     Result := TDCP_tiger;
    else raise Exception.CreateFmt(SUnsupportedHash, [Hash]);
  end;
end;

procedure TO2File.Compress(InputStream, OutputStream: TStream);
var
  CompressionStream: TCompressionStream;
begin
  CompressionStream := TCompressionStream.Create(clDefault, OutputStream);
  try
    CompressionStream.CopyFrom(InputStream, 0);
  finally
    CompressionStream.Free;
  end;
end;

procedure TO2File.Decompress(InputStream, OutputStream: TStream);
var
  DecompressionStream: TDecompressionStream;
  Bytes: Longint;
  Buffer: Byte;
begin
  InputStream.Position := 0;
  DecompressionStream := TDecompressionStream.Create(InputStream);
  try
    repeat
      Bytes := DecompressionStream.Read(Buffer, SizeOf(Buffer));
      if Bytes > 0 then
        OutputStream.Write(Buffer, SizeOf(Buffer));
    until Bytes = 0;
  finally
    DecompressionStream.Free;
  end;
end;

procedure TO2File.Encrypt(InputStream, OutputStream: TStream);
var
  Cipher: TDCP_cipher;
begin
  Cipher := GetCipher(@FIV);
  try
    Cipher.EncryptStream(InputStream, OutputStream,
      InputStream.Size - InputStream.Position);
    Cipher.Burn;
  finally
    Cipher.Free;
  end;
end;

procedure TO2File.Decrypt(InputStream, OutputStream: TStream);
var
  Cipher: TDCP_cipher;
begin
  if UseIV then
    Cipher := GetCipher(@FIV)
  else
    Cipher := GetCipher(nil);
  try
    Cipher.DecryptStream(InputStream, OutputStream,
      InputStream.Size - InputStream.Position);
    Cipher.Burn;
  finally
    Cipher.Free;
  end;
end;

function TO2File.UseCompression: Boolean;
const
  MinVer: WordRec = (Lo: 0; Hi: 2);
begin
  Result := Word(FVersion) >= Word(MinVer);
end;

function TO2File.UseIV: Boolean;
const
  MinVer: WordRec = (Lo: 0; Hi: 3);
begin
  Result := Word(FVersion) >= Word(MinVer);
end;

procedure TO2File.SetTitle(const Value: string);
begin
  if FTitle <> Value then
  begin
    FTitle := Value;
    NotifyChanges(nil, onPropertyChanged);
  end;
end;

procedure TO2File.SetDescription(const Value: string);
begin
  if FDescription <> Value then
  begin
    FDescription := Value;
    NotifyChanges(nil, onPropertyChanged);
  end;
end;

procedure TO2File.SetAuthor(const Value: string);
begin
  if FAuthor <> Value then
  begin
    FAuthor := Value;
    NotifyChanges(nil, onPropertyChanged);
  end;
end;

procedure TO2File.SetObjects(const Value: TO2Objects);
begin
  FObjects.Assign(Value);
end;

procedure TO2File.SetRelations(const Value: TO2Relations);
begin
  FRelations.Assign(Value);
end;

procedure TO2File.SetRules(const Value: TO2Rules);
begin
  FRules.Assign(Value);
end;

end.
