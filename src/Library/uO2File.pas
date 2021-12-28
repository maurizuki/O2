{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2022 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uO2File;

interface

uses
  Classes, ZLib, DCPcrypt2,
  uO2Defs, uO2Classes, uO2Objects, uO2Relations, uO2Rules;

type
  TPasswordQueryEvent = procedure(Sender: TObject; var APassword: string;
    var Acknowledge: Boolean) of object;

  TO2File = class(TO2Persistent)
  private
    FFileName: string;
    FEncrypted: Boolean;
    FCipher: TO2Cipher;
    FHash: TO2Hash;
    FCRC32: Longword;
    FPassword: string;
    FModified: Boolean;
    FOnPasswordQuery: TPasswordQueryEvent;
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
  protected
    procedure OutputDebugNotifyChanges(Item: TO2CollectionItem;
      Action: TO2Notification); virtual;
    procedure NotifyChanges(Item: TO2CollectionItem;
      Action: TO2Notification); override;
    procedure CheckContentType(ContentType: TGUID); virtual;
    procedure CheckVersion(Version: Word); virtual;
    procedure OutputDebugHeader(Header: TO2FileHeader); virtual;
    function ReadHeader(const Stream: TStream): Word; virtual;
    function GetContentType: TGUID; virtual;
    function GetVersion: Word; virtual;
    procedure WriteHeader(const Stream: TStream); virtual;
    function GetCipherClass: TDCP_cipherclass; virtual;
    function GetHashClass: TDCP_hashclass; virtual;
    procedure Compress(InputStream, OutputStream: TStream); virtual;
    procedure Decompress(InputStream, OutputStream: TStream); virtual;
    procedure Encrypt(InputStream, OutputStream: TStream); virtual;
    procedure Decrypt(InputStream, OutputStream: TStream); virtual;
    function PasswordQuery: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load; virtual;
    procedure Save(KeepModified: Boolean = False); virtual;
    property FileName: string read FFileName write FFileName;
    property Encrypted: Boolean read FEncrypted write FEncrypted;
    property Cipher: TO2Cipher read FCipher write FCipher;
    property Hash: TO2Hash read FHash write FHash;
    property CRC32: Longword read FCRC32 write FCRC32;
    property Password: string read FPassword write FPassword;
    property Modified: Boolean read FModified write FModified;
    property OnPasswordQuery: TPasswordQueryEvent read FOnPasswordQuery
      write FOnPasswordQuery;
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
  Windows, SysUtils, Types, TypInfo,
  DCPblowfish, DCPcast128, DCPcast256, DCPdes, DCPice,
  DCPidea, DCPmars, DCPmisty1, DCPrc2, DCPrc4, DCPrc5,
  DCPrc6, DCPrijndael, DCPserpent, DCPtea, DCPtwofish,
  DCPhaval, DCPmd4, DCPmd5, DCPripemd128, DCPripemd160,
  DCPsha1, DCPsha256, DCPsha512, DCPtiger,
  uO2CRC32, uO2Xml;

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
  FOnPasswordQuery := nil;
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

procedure TO2File.Load;
const
  Version2_0: WordRec = (Lo: 0; Hi: 2);
var
  XmlReader: TO2XmlReader;
  XmlStream, RawStream: TMemoryStream;
  InputStream: TFileStream;
  FileVersion: Word;
begin
  InputStream := TFileStream.Create(FileName, fmOpenRead);
  try
    FileVersion := ReadHeader(InputStream);

    if Encrypted and not PasswordQuery then Abort;

    XmlStream := TMemoryStream.Create;
    try
      RawStream := TMemoryStream.Create;
      try
        if Encrypted then
        begin
          Decrypt(InputStream, RawStream);

          RawStream.Position := 0;
          if CRC32 <> SZCRC32FullStream(RawStream) then
            raise Exception.Create(SWrongPassword);
        end
        else
          RawStream.CopyFrom(InputStream,
            InputStream.Size - InputStream.Position);

        if FileVersion < Word(Version2_0) then
          XmlStream.LoadFromStream(RawStream)
        else
          Decompress(RawStream, XmlStream);
      finally
        RawStream.Free;
      end;

      XmlStream.Position := 0;
      XmlReader := TO2XmlReader.Create(Self);
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
  Modified := False;
end;

procedure TO2File.Save(KeepModified: Boolean = False);
var
  XmlWriter: TO2XmlWriter;
  XmlStream, RawStream: TMemoryStream;
  OutputStream: TFileStream;
begin
  OutputStream := TFileStream.Create(FileName, fmCreate);
  try
    RawStream := TMemoryStream.Create;
    try
      XmlStream := TMemoryStream.Create;
      try
        XmlWriter := TO2XmlWriter.Create(Self);
        try
          XmlWriter.SaveToStream(XmlStream);
        finally
          XmlWriter.Free;
        end;
        
        Compress(XmlStream, RawStream);
      finally
        XmlStream.Free;
      end;

      if Encrypted then
      begin
        RawStream.Position := 0;
        CRC32 := SZCRC32FullStream(RawStream);
      end;

      WriteHeader(OutputStream);
      RawStream.Position := 0;
      if Encrypted then
        Encrypt(RawStream, OutputStream)
      else
        RawStream.SaveToStream(OutputStream);
    finally
      RawStream.Free;
    end;
  finally
    OutputStream.Free;
  end;
  if not KeepModified then Modified := False;
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

  DebugOutput := Format(DebugOutputFmt, [FileName, ItemClass,
    GetEnumName(TypeInfo(TO2Notification), Integer(Action))]);

  OutputDebugString(PChar(DebugOutput));
end;

procedure TO2File.NotifyChanges(Item: TO2CollectionItem;
  Action: TO2Notification);
begin
  Modified := True;

  OutputDebugNotifyChanges(Item, Action);

  if (Action in [onExtracting, onDeleting])
    and Assigned(Item) and (Item is TO2Object) then
    Relations.DeleteObjectRelations(TO2Object(Item));
end;

procedure TO2File.CheckContentType(ContentType: TGUID);
begin
  if not IsEqualGUID(O2FileGUID, ContentType) then
    raise Exception.Create(SUnsupportedFileType);
end;

procedure TO2File.CheckVersion(Version: Word);
begin
  if Version > Word(O2FileVersion) then
    raise Exception.CreateFmt(SUnsupportedFileVersion,
      [WordRec(Version).Hi, WordRec(Version).Lo]);
end;

function TO2File.GetContentType: TGUID;
begin
  Result := O2FileGUID;
end;

function TO2File.GetVersion: Word;
begin
  Result := Word(O2FileVersion);
end;

procedure TO2File.OutputDebugHeader(Header: TO2FileHeader);
const
  DebugOutputFmt = 'O2 File: %s. Content Type: %s. Version: %d.%d. '
    + 'Encrypted: %s. Cipher: %s. Hash: %s. CRC32: %s%.8x.';
var
  DebugOutput, CipherIdent, HashIdent: string;
begin
  if not CipherToIdent(Header.Cipher, CipherIdent) then
    CipherIdent := Format('%s%.2x', [HexDisplayPrefix, Header.Cipher]);

  if not HashToIdent(Header.Hash, HashIdent) then
    HashIdent := Format('%s%.2x', [HexDisplayPrefix, Header.Hash]);

  DebugOutput := Format(DebugOutputFmt, [FileName,
    GUIDToString(Header.ContentType),
    WordRec(Header.Version).Hi, WordRec(Header.Version).Lo,
    BoolToStr(Header.Encrypted, True),
    CipherIdent,
    HashIdent,
    HexDisplayPrefix, Header.CRC32]);

  OutputDebugString(PChar(DebugOutput));
end;

function  TO2File.ReadHeader(const Stream: TStream): Word;
var
  Header: TO2FileHeader;
begin
  Stream.Read(Header, SizeOf(Header));

  OutputDebugHeader(Header);

  CheckContentType(Header.ContentType);
  CheckVersion(Header.Version);

  Encrypted := Header.Encrypted;
  Cipher := Header.Cipher;
  Hash := Header.Hash;
  CRC32 := Header.CRC32;

  Result := Header.Version;
end;

procedure TO2File.WriteHeader(const Stream: TStream);
var
  Header: TO2FileHeader;
begin
  FillChar(Header, SizeOf(Header), 0);

  Header.ContentType := GetContentType;
  Header.Version := GetVersion;
  Header.Encrypted := Encrypted;
  if Encrypted then
  begin
    Header.Cipher := Cipher;
    Header.Hash := Hash;
    Header.CRC32 := CRC32;
  end;

  Stream.Write(Header, SizeOf(Header));
end;

function TO2File.GetCipherClass: TDCP_cipherclass;
begin
  case Cipher of
    ocBlowfish: Result := TDCP_blowfish;
    ocCast128:  Result := TDCP_cast128;
    ocCast256:  Result := TDCP_cast256;
    ocDES:      Result := TDCP_des;
    oc3DES:     Result := TDCP_3des;
    ocIce:      Result := TDCP_ice;
    ocThinIce:  Result := TDCP_thinice;
    ocIce2:     Result := TDCP_ice2;
    ocIDEA:     Result := TDCP_idea;
    ocMARS:     Result := TDCP_mars;
    ocMisty1:   Result := TDCP_misty1;
    ocRC2:      Result := TDCP_rc2;
    ocRC4:      Result := TDCP_rc4;
    ocRC5:      Result := TDCP_rc5;
    ocRC6:      Result := TDCP_rc6;
    ocRijndael: Result := TDCP_rijndael;
    ocSerpent:  Result := TDCP_serpent;
    ocTEA:      Result := TDCP_tea;
    ocTwofish:  Result := TDCP_twofish;
    else raise Exception.CreateFmt(SUnsupportedCipher, [Cipher]);
  end;
end;

function TO2File.GetHashClass: TDCP_hashclass;
begin
  case Hash of
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
  Cipher := GetCipherClass.Create(nil);
  try
    Cipher.InitStr(AnsiString(Password), GetHashClass);
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
  Cipher := GetCipherClass.Create(nil);
  try
    Cipher.InitStr(AnsiString(Password), GetHashClass);
    Cipher.DecryptStream(InputStream, OutputStream,
      InputStream.Size - InputStream.Position);
    Cipher.Burn;
  finally
    Cipher.Free;
  end;
end;

function TO2File.PasswordQuery: Boolean;
var
  APassword: string;
begin
  Result := True;
  APassword := Password;
  if Assigned(OnPasswordQuery) then
  begin
    OnPasswordQuery(Self, APassword, Result);
    if Result then Password := APassword;
  end;
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
