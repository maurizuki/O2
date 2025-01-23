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

unit uFileOperation;

interface

uses
  uO2File, uServices;

type
  TFileOperation = class(TInterfacedObject, IFileOperation)
  private
    FO2File: TO2File;
  protected
    property O2File: TO2File read FO2File;
  public
    constructor Create(const O2File: TO2File);
    procedure Execute(const FileName: string); virtual; abstract;
  end;

implementation

{ TImportExport }

constructor TFileOperation.Create(const O2File: TO2File);
begin
  FO2File := O2File;
end;

end.
