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

unit uFileManager;

interface

uses
  uServices, uO2File, uO2Objects, uO2Rules;

type
  TFileManager = class(TInterfacedObject, IFileManager)
  private
    FFile: TO2File;

    FPasswordScoreCache: IPasswordScoreCache;

    function GetFile: TO2File;
    procedure SetFile(const Value: TO2File);
  public
    constructor Create(PasswordScoreCache: IPasswordScoreCache);
    destructor Destroy; override;

    function GetHighlight(const AObject: TO2Object): THighlight; overload;
    function GetHighlight(const AField: TO2Field): THighlight; overload;
    function IsHyperlink(const AField: TO2Field): Boolean;

    property O2File: TO2File read GetFile write SetFile;
  end;

implementation

{ TFileManager }

constructor TFileManager.Create(PasswordScoreCache: IPasswordScoreCache);
begin
  FPasswordScoreCache := PasswordScoreCache;
end;

destructor TFileManager.Destroy;
begin
  if Assigned(FFile) then FFile.Free;
  inherited;
end;

function TFileManager.GetFile: TO2File;
begin
  if FFile = nil then FFile := TO2File.Create;
  Result := FFile;
end;

function TFileManager.GetHighlight(const AField: TO2Field): THighlight;
begin
  Result := O2File.Rules.GetHighlightColors(AField, FPasswordScoreCache);
end;

function TFileManager.GetHighlight(const AObject: TO2Object): THighlight;
begin
  Result := O2File.Rules.GetHighlightColors(AObject, FPasswordScoreCache);
end;

function TFileManager.IsHyperlink(const AField: TO2Field): Boolean;
begin
  Result := Assigned(O2File.Rules.FindFirstRule(AField, HyperlinkRules));
end;

procedure TFileManager.SetFile(const Value: TO2File);
begin
  FFile := Value;
end;

end.
