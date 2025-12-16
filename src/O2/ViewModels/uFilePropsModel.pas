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

unit uFilePropsModel;

interface

uses
  uServices, uO2File;

type
  TFilePropsModel = class(TInterfacedObject, IFileProps)
  private
    FO2File: TO2File;
    FTitle: string;
    FDescription: string;
    FAuthor: string;
    function GetTitle: string;
    function GetDescription: string;
    function GetAuthor: string;
    function GetCipher: string;
    function GetHash: string;
    procedure SetTitle(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetAuthor(const Value: string);
  public
    constructor Create(const O2File: TO2File);

    procedure ApplyChanges;

    property Title: string read GetTitle write SetTitle;
    property Description: string read GetDescription write SetDescription;
    property Author: string read GetAuthor write SetAuthor;
    property Cipher: string read GetCipher;
    property Hash: string read GetHash;
  end;

implementation

uses
  uGlobal;

{ TFilePropsModel }

constructor TFilePropsModel.Create(const O2File: TO2File);
begin
  FO2File := O2File;
  FTitle := O2File.Title;
  FDescription := O2File.Description;
  FAuthor := O2File.Author;
end;

procedure TFilePropsModel.ApplyChanges;
begin
  FO2File.Title := FTitle;
  FO2File.Description := FDescription;
  FO2File.Author := FAuthor;
end;

function TFilePropsModel.GetAuthor: string;
begin
  Result := FAuthor;
end;

function TFilePropsModel.GetCipher: string;
begin
  if FO2File.Encrypted then
    Result := Ciphers[FO2File.Cipher]
  else
    Result := SCipherNone;
end;

function TFilePropsModel.GetDescription: string;
begin
  Result := FDescription;
end;

function TFilePropsModel.GetHash: string;
begin
  if FO2File.Encrypted then
    Result := Hashes[FO2File.Hash]
  else
    Result := SHashNone;
end;

function TFilePropsModel.GetTitle: string;
begin
  Result := FTitle;
end;

procedure TFilePropsModel.SetAuthor(const Value: string);
begin
  FAuthor := Value;
end;

procedure TFilePropsModel.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TFilePropsModel.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

end.
