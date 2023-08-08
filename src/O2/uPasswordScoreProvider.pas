{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2023 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uPasswordScoreProvider;

interface

uses
  System.Generics.Collections, DCPcrypt2, Zxcvbn, uO2File, uO2Rules;

type
  TPasswordScoreProvider = class(TInterfacedObject, IPasswordScoreProvider)
  private
    FPasswordScores: TDictionary<string, Integer>;
    FHash: TDCP_hash;
    FZxcvbn: TZxcvbn;
    function GetHash(const s: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    function TryGet(const Password: string; var Score: Integer): Boolean;
    procedure Update(const O2File: TO2File); overload;
    procedure Update(const O2File: TO2File; ObjectIndex: Integer); overload;
  end;

implementation

uses
  System.Classes, DCPsha256, Zxcvbn.Result, uO2Objects;

{ PasswordScoreProvider }

constructor TPasswordScoreProvider.Create;
begin
  FPasswordScores := TDictionary<string, Integer>.Create;
  FHash := TDCP_sha256.Create;
  FZxcvbn := TZxcvbn.Create;
end;

destructor TPasswordScoreProvider.Destroy;
begin
  FPasswordScores.Free;
  FHash.Free;
  FZxcvbn.Free;
end;

function TPasswordScoreProvider.GetHash(const s: string): string;
var
  Digest: array[0..31] of byte;
begin
  FHash.Init;
  FHash.UpdateStr(s);
  FHash.Final(Digest);
  SetString(Result, PAnsiChar(@Digest[0]), Length(Digest));
end;

function TPasswordScoreProvider.TryGet(const Password: string;
  var Score: Integer): Boolean;
begin
  Result := FPasswordScores.TryGetValue(GetHash(Password), Score);
end;

procedure TPasswordScoreProvider.Update(const O2File: TO2File);
var
  I: Integer;
begin
  for I := 0 to O2File.Objects.Count - 1 do Update(O2File, I);
end;

procedure TPasswordScoreProvider.Update(const O2File: TO2File;
  ObjectIndex: Integer);
var
  ZxcvbnResult: TZxcvbnResult;
  AField: TO2Field;
  ARule: TO2Rule;
begin
  for AField in O2File.Objects[ObjectIndex].Fields do
    for ARule in O2File.Rules do
      if (ARule.RuleType = rtPassword) and ARule.Matches(AField) then
      begin
          ZxcvbnResult := FZxcvbn.EvaluatePassword(AField.FieldValue);
          try
            FPasswordScores.TryAdd(GetHash(AField.FieldValue),
              ZxcvbnResult.Score);
          finally
            ZxcvbnResult.Free;
          end;
      end;
end;

end.
