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

unit uStorageUtils;

interface

uses
  Classes, uServices;

function ReadIntIdent(AStorage: IStorage; const Name: string;
  const Map: array of TIdentMapEntry; Default: Integer = 0): Integer;

procedure WriteIntIdent(AStorage: IStorage; const Name: string;
  const Map: array of TIdentMapEntry; Value: Integer);

implementation

function ReadIntIdent(AStorage: IStorage; const Name: string;
  const Map: array of TIdentMapEntry; Default: Integer = 0): Integer;
begin
  if not IdentToInt(AStorage.ReadString(Name), Result, Map) then
    Result := Default;
end;

procedure WriteIntIdent(AStorage: IStorage; const Name: string;
  const Map: array of TIdentMapEntry; Value: Integer);
var
  Ident: string;
begin
  if IntToIdent(Value, Ident, Map) then
    AStorage.WriteString(Name, Ident);
end;

end.
