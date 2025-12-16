{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2026 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uEncryptionPropsModel;

interface

uses
  Classes, uServices, uPasswordStrengthInfo, uO2File;

type
  TEncryptionPropsModel = class(TPasswordStrengthInfo, IEncryptionProps)
  private
    FO2File: TO2File;
    FCiphers: TStrings;
    FCipherIndex: Integer;
    FHashes: TStrings;
    FHashIndex: Integer;
    FPassword: string;
    FPasswordConfirmation: string;

    function GetCiphers: TStrings;
    function GetCipherIndex: Integer;
    function GetHashes: TStrings;
    function GetHashIndex: Integer;
    function GetPassword: string;
    function GetPasswordConfirmation: string;
    function GetValid: Boolean;
    procedure SetCipherIndex(const Value: Integer);
    procedure SetHashIndex(const Value: Integer);
    procedure SetPassword(const Value: string);
    procedure SetPasswordConfirmation(const Value: string);
  protected
    procedure EvaluatePasswordStrength(const APassword: string); override;
  public
    constructor Create(const O2File: TO2File);
    destructor Destroy; override;

    function IsEncrypted: Boolean;

    procedure ApplyChanges;

    property Ciphers: TStrings read GetCiphers;
    property CipherIndex: Integer read GetCipherIndex write SetCipherIndex;
    property Hashes: TStrings read GetHashes;
    property HashIndex: Integer read GetHashIndex write SetHashIndex;
    property Password: string read GetPassword write SetPassword;
    property PasswordConfirmation: string read GetPasswordConfirmation
      write SetPasswordConfirmation;
    property PasswordScore;
    property PasswordStrengthInfo;

    property Valid: Boolean read GetValid;
  end;

implementation

uses
  uGlobal, uO2Defs;

type
  TCipherRange = 0..19;
  THashRange = 0..9;

const
  CipherValues: array[TCipherRange] of TO2Cipher = (
    ocNone,
    ocBlowfish,
    ocCast128,
    ocCast256,
    ocDES,
    oc3DES,
    ocIce,
    ocThinIce,
    ocIce2,
    ocIDEA,
    ocMARS,
    ocMisty1,
    ocRC2,
    ocRC4,
    ocRC5,
    ocRC6,
    ocRijndael,
    ocSerpent,
    ocTEA,
    ocTwofish
    );

  CipherDescriptions: array[TCipherRange] of string = (
    SCipherNone,
    SCipherBlowfish,
    SCipherCast128,
    SCipherCast256,
    SCipherDES,
    SCipher3DES,
    SCipherIce,
    SCipherThinIce,
    SCipherIce2,
    SCipherIDEA,
    SCipherMARS,
    SCipherMisty1,
    SCipherRC2,
    SCipherRC4,
    SCipherRC5,
    SCipherRC6,
    SCipherRijndael,
    SCipherSerpent,
    SCipherTEA,
    SCipherTwofish
    );

  HashValues: array[THashRange] of TO2Hash = (
    ohHaval,
    ohMD4,
    ohMD5,
    ohRipeMD128,
    ohRipeMD160,
    ohSHA1,
    ohSHA256,
    ohSHA384,
    ohSHA512,
    ohTiger
    );

  HashDescriptions: array[THashRange] of string = (
    SHashHaval,
    SHashMD4,
    SHashMD5,
    SHashRipeMD128,
    SHashRipeMD160,
    SHashSHA1,
    SHashSHA256,
    SHashSHA384,
    SHashSHA512,
    SHashTiger
    );

{ TEncryptionPropsModel }

constructor TEncryptionPropsModel.Create(const O2File: TO2File);
begin
  inherited Create;
  FO2File := O2File;

  if FO2File.Encrypted then
  begin
    FCipherIndex := High(CipherValues);
    while (FCipherIndex > Low(CipherValues))
      and (CipherValues[FCipherIndex] <> FO2File.Cipher) do
      Dec(FCipherIndex);
  end;

  EvaluatePasswordStrength(FO2File.Password);

  FHashIndex := High(HashValues);
  while (FHashIndex > Low(HashValues))
    and (HashValues[FHashIndex] <> FO2File.Hash) do
    Dec(FHashIndex);

  FCiphers := TStringList.Create;
  FCiphers.AddStrings(CipherDescriptions);

  FHashes := TStringList.Create;
  FHashes.AddStrings(HashDescriptions);

  FPassword := FO2File.Password;
  FPasswordConfirmation := FO2File.Password;
end;

destructor TEncryptionPropsModel.Destroy;
begin
  FCiphers.Free;
  FHashes.Free;
  inherited;
end;

procedure TEncryptionPropsModel.EvaluatePasswordStrength(
  const APassword: string);
begin
  if not IsEncrypted then
  begin
    FPasswordScore := -1;
    FPasswordStrengthInfo := '';

    Exit;
  end;

  inherited;
end;

procedure TEncryptionPropsModel.ApplyChanges;
begin
  FO2File.Encrypted := IsEncrypted;
  FO2File.Cipher := CipherValues[FCipherIndex];
  FO2File.Hash := HashValues[FHashIndex];
  FO2File.Password := FPassword;
end;

function TEncryptionPropsModel.GetCipherIndex: Integer;
begin
  Result := FCipherIndex;
end;

function TEncryptionPropsModel.GetCiphers: TStrings;
begin
  Result := FCiphers;
end;

function TEncryptionPropsModel.GetHashes: TStrings;
begin
  Result := FHashes;
end;

function TEncryptionPropsModel.GetHashIndex: Integer;
begin
  Result := FHashIndex;
end;

function TEncryptionPropsModel.GetPassword: string;
begin
  Result := FPassword;
end;

function TEncryptionPropsModel.GetPasswordConfirmation: string;
begin
  Result := FPasswordConfirmation;
end;

function TEncryptionPropsModel.GetValid: Boolean;
begin
  Result := not IsEncrypted or (FPassword = FPasswordConfirmation)
    and (Length(FPassword) >= MinPasswordLength)
    and not (CipherValues[FCipherIndex] in DeprecatedCiphers)
    and not (HashValues[FHashIndex] in DeprecatedHashes);
end;

function TEncryptionPropsModel.IsEncrypted: Boolean;
begin
  Result := CipherValues[FCipherIndex] > ocNone;
end;

procedure TEncryptionPropsModel.SetCipherIndex(const Value: Integer);
begin
  if FCipherIndex <> Value then
  begin
    FCipherIndex := Value;

    EvaluatePasswordStrength(FPassword);
  end;
end;

procedure TEncryptionPropsModel.SetHashIndex(const Value: Integer);
begin
  FHashIndex := Value;
end;

procedure TEncryptionPropsModel.SetPassword(const Value: string);
begin
  if FPassword <> Value then
  begin
    FPassword := Value;

    EvaluatePasswordStrength(FPassword);
  end;
end;

procedure TEncryptionPropsModel.SetPasswordConfirmation(const Value: string);
begin
  FPasswordConfirmation := Value;
end;

end.
