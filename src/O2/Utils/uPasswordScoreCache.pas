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

unit uPasswordScoreCache;

interface

uses
  Generics.Collections, DCPcrypt2, Zxcvbn, uO2File, uServices;

type
  TPasswordScoreCache = class(TInterfacedObject, IPasswordScoreCache)
  private
    FPasswordScores: TDictionary<string, Integer>;
    FHash: TDCP_hash;
    FZxcvbn: TZxcvbn;

    function CalculateHash(const S: string): string;
    function EvaluatePasswordScore(const Password: string): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function TryGetPasswordScore(const Password: string;
      var Score: Integer): Boolean;

    procedure UpdateCache(const O2File: TO2File);
  end;

implementation

uses
  Classes, DCPsha256, Zxcvbn.Result, uO2Objects, uO2Rules;

{ TPasswordScoreCache }

constructor TPasswordScoreCache.Create;
begin
  FPasswordScores := TDictionary<string, Integer>.Create;
  FHash := TDCP_sha256.Create;
  FZxcvbn := TZxcvbn.Create;
end;

destructor TPasswordScoreCache.Destroy;
begin
  FPasswordScores.Free;
  FHash.Free;
  FZxcvbn.Free;
  inherited;
end;

function TPasswordScoreCache.CalculateHash(const S: string): string;
var
  Digest: array[0..31] of byte;
begin
  FHash.Init;
  FHash.UpdateStr(S);
  FHash.Final(Digest);
  SetString(Result, PAnsiChar(@Digest[0]), Length(Digest));
end;

function TPasswordScoreCache.EvaluatePasswordScore(
  const Password: string): Integer;
var
  ZxcvbnResult: TZxcvbnResult;
begin
  ZxcvbnResult := FZxcvbn.EvaluatePassword(Password);
  try
    Result := ZxcvbnResult.Score;
  finally
    ZxcvbnResult.Free;
  end;
end;

function TPasswordScoreCache.TryGetPasswordScore(const Password: string;
  var Score: Integer): Boolean;
var
  Hash: string;
begin
  Result := True;
  Hash := CalculateHash(Password);
  if FPasswordScores.TryGetValue(Hash, Score) then Exit;
  Score := EvaluatePasswordScore(Password);
  FPasswordScores.Add(Hash, Score);
end;

procedure TPasswordScoreCache.UpdateCache(const O2File: TO2File);
var
  AObject: TO2Object;
  AField: TO2Field;
  ARule: TO2Rule;
  Score: Integer;
begin
  for ARule in O2File.Rules do
    if ARule.Active and (ARule.RuleType = rtPassword)
      and ARule.DisplayPasswordStrength then
      for AObject in O2File.Objects do
        for AField in AObject.Fields do
          if ARule.Matches(AField) then
            TryGetPasswordScore(AField.FieldValue, Score);
end;

end.
