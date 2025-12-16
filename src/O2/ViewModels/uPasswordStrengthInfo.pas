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

unit uPasswordStrengthInfo;

interface

uses
  Zxcvbn, uServices;

type
  TPasswordStrengthInfo = class(TInterfacedObject, IPasswordStrengthInfo)
  private
    FZxcvbn: TZxcvbn;
    function GetPasswordScore: Integer;
    function GetPasswordStrengthInfo: string;
  protected
    FPasswordScore: Integer;
    FPasswordStrengthInfo: string;

    constructor Create;

    procedure EvaluatePasswordStrength(const APassword: string); virtual;

    property PasswordScore: Integer read GetPasswordScore;
    property PasswordStrengthInfo: string read GetPasswordStrengthInfo;
  public
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, Zxcvbn.Result, Zxcvbn.Utility;

{ TPasswordStrengthInfo }

constructor TPasswordStrengthInfo.Create;
begin
  FZxcvbn := TZxcvbn.Create;
end;

destructor TPasswordStrengthInfo.Destroy;
begin
  FZxcvbn.Free;
  inherited;
end;

procedure TPasswordStrengthInfo.EvaluatePasswordStrength(
  const APassword: string);
var
  ZxcvbnResult: TZxcvbnResult;
  ASuggestion: TZxcvbnSuggestion;
  PasswordStrengthInfoBuilder: TStringBuilder;
begin
  ZxcvbnResult := FZxcvbn.EvaluatePassword(APassword);
  try
    FPasswordScore := ZxcvbnResult.Score;

    PasswordStrengthInfoBuilder := TStringBuilder.Create;
    try
      if ZxcvbnResult.Warning <> zwDefault then
      begin
        PasswordStrengthInfoBuilder.AppendLine(
          GetWarning(ZxcvbnResult.Warning));

        PasswordStrengthInfoBuilder.AppendLine();
      end;

      for ASuggestion in ZxcvbnResult.Suggestions do
        PasswordStrengthInfoBuilder.AppendLine(GetSuggestion(ASuggestion));

      FPasswordStrengthInfo := Trim(PasswordStrengthInfoBuilder.ToString);
    finally
      PasswordStrengthInfoBuilder.Free;
    end;
  finally
    ZxcvbnResult.Free;
  end;
end;

function TPasswordStrengthInfo.GetPasswordScore: Integer;
begin
  Result := FPasswordScore;
end;

function TPasswordStrengthInfo.GetPasswordStrengthInfo: string;
begin
  Result := FPasswordStrengthInfo;
end;

end.
