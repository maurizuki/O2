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

unit uXmlImportExport;

interface

uses
  uFileOperation;

type
  TXmlImport = class(TFileOperation)
  public
    procedure Execute(const FileName: string); override;
  end;

  TXmlExport = class(TFileOperation)
  public
    procedure Execute(const FileName: string); override;
  end;

implementation

uses
  uXmlFiler, uO2Defs;

{ TXmlImport }

procedure TXmlImport.Execute(const FileName: string);
var
  XmlReader: TXmlReader;
begin
  XmlReader := TXmlReader.Create(O2File, O2FileSchemaLocation);
  try
    XmlReader.LoadFromFile(FileName);
  finally
    XmlReader.Free;
  end;
end;

{ TXmlExport }

procedure TXmlExport.Execute(const FileName: string);
var
  XmlWriter: TXmlWriter;
begin
  XmlWriter := TXmlWriter.Create(O2File, O2FileSchemaLocation);
  try
    XmlWriter.SaveToFile(FileName);
  finally
    XmlWriter.Free;
  end;
end;

end.
