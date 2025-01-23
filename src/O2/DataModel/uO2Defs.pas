{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2025 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uO2Defs;

interface

uses
  SysUtils;

const
  O2FileGUID: TGUID = '{ABBB4FE2-9C21-450E-80B0-469DFD8A8BFC}';
  O2FileVersion: WordRec = (Lo: 0; Hi: 3); // 3.0

type
  TO2Cipher = Byte;
  TO2Hash = Byte;

const

{ Supported ciphers IDs }

  ocNone     = TO2Cipher($00);
  ocBlowfish = TO2Cipher($01);
  ocCast128  = TO2Cipher($02);
  ocCast256  = TO2Cipher($03);
  ocDES      = TO2Cipher($04);
  oc3DES     = TO2Cipher($05);
  ocIce      = TO2Cipher($06);
  ocThinIce  = TO2Cipher($07);
  ocIce2     = TO2Cipher($08);
  ocIDEA     = TO2Cipher($09);
  ocMARS     = TO2Cipher($0A);
  ocMisty1   = TO2Cipher($0B);
  ocRC2      = TO2Cipher($0C);
  ocRC4      = TO2Cipher($0D);
  ocRC5      = TO2Cipher($0E);
  ocRC6      = TO2Cipher($0F);
  ocRijndael = TO2Cipher($10);
  ocSerpent  = TO2Cipher($11);
  ocTEA      = TO2Cipher($12);
  ocTwofish  = TO2Cipher($13);
  ocDefault  = ocBlowfish;

  DeprecatedCiphers: set of TO2Cipher = [
    ocBlowfish,
    ocDES,
    ocIce,
    ocThinIce,
    ocMisty1,
    ocRC2,
    ocRC4,
    ocTEA
  ];

{ Supported hash algorithms IDs }

  ohNone      = TO2Hash($00);
  ohHaval     = TO2Hash($01);
  ohMD4       = TO2Hash($02);
  ohMD5       = TO2Hash($03);
  ohRipeMD128 = TO2Hash($04);
  ohRipeMD160 = TO2Hash($05);
  ohSHA1      = TO2Hash($06);
  ohSHA256    = TO2Hash($07);
  ohSHA384    = TO2Hash($08);
  ohSHA512    = TO2Hash($09);
  ohTiger     = TO2Hash($0A);
  ohDefault   = ohSHA256;

  DeprecatedHashes: set of TO2Hash = [
    ohMD5,
    ohSHA1
  ];

var
  O2FileSchemaLocation: string;

function CipherToIdent(Cipher: Longint; var Ident: string): Boolean;
function HashToIdent(Hash: Longint; var Ident: string): Boolean;

implementation

uses
  Classes;

const
  Ciphers: array[0..19] of TIdentMapEntry = (
    (Value: ocNone;     Name: 'ocNone'),
    (Value: ocBlowfish; Name: 'ocBlowfish'),
    (Value: ocCast128;  Name: 'ocCast128'),
    (Value: ocCast256;  Name: 'ocCast256'),
    (Value: ocDES;      Name: 'ocDES'),
    (Value: oc3DES;     Name: 'oc3DES'),
    (Value: ocIce;      Name: 'ocIce'),
    (Value: ocThinIce;  Name: 'ocThinIce'),
    (Value: ocIce2;     Name: 'ocIce2'),
    (Value: ocIDEA;     Name: 'ocIDEA'),
    (Value: ocMARS;     Name: 'ocMARS'),
    (Value: ocMisty1;   Name: 'ocMisty1'),
    (Value: ocRC2;      Name: 'ocRC2'),
    (Value: ocRC4;      Name: 'ocRC4'),
    (Value: ocRC5;      Name: 'ocRC5'),
    (Value: ocRC6;      Name: 'ocRC6'),
    (Value: ocRijndael; Name: 'ocRijndael'),
    (Value: ocSerpent;  Name: 'ocSerpent'),
    (Value: ocTEA;      Name: 'ocTEA'),
    (Value: ocTwofish;  Name: 'ocTwofish'));

  Hashes: array[0..10] of TIdentMapEntry = (
    (Value: ohNone;      Name: 'ohNone'),
    (Value: ohHaval;     Name: 'ohHaval'),
    (Value: ohMD4;       Name: 'ohMD4'),
    (Value: ohMD5;       Name: 'ohMD5'),
    (Value: ohRipeMD128; Name: 'ohRipeMD128'),
    (Value: ohRipeMD160; Name: 'ohRipeMD160'),
    (Value: ohSHA1;      Name: 'ohSHA1'),
    (Value: ohSHA256;    Name: 'ohSHA256'),
    (Value: ohSHA384;    Name: 'ohSHA384'),
    (Value: ohSHA512;    Name: 'ohSHA512'),
    (Value: ohTiger;     Name: 'ohTiger'));

function CipherToIdent(Cipher: Longint; var Ident: string): Boolean;
begin
  Result := IntToIdent(Cipher, Ident, Ciphers);
end;

function HashToIdent(Hash: Longint; var Ident: string): Boolean;
begin
  Result := IntToIdent(Hash, Ident, Hashes);
end;

initialization
  O2FileSchemaLocation := Format(
    'https://maurizuki.github.io/O2/xml/O2File/%d%d.xsd',
    [O2FileVersion.Hi, O2FileVersion.Lo]);

end.
