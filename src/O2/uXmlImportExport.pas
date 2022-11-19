{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2022 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uXmlImportExport;

interface

uses
  uImportExport;

type
  TXmlImport = class(TImportExport)
  public
    procedure Execute(const FileName: string); override;
  end;

  TXmlExport = class(TImportExport)
  public
    procedure Execute(const FileName: string); override;
  end;

implementation

uses
  uO2Xml;

{ TXmlImport }

procedure TXmlImport.Execute(const FileName: string);
var
  XmlReader: TO2XmlReader;
begin
  XmlReader := TO2XmlReader.Create(O2File);
  try
    XmlReader.LoadFromFile(FileName);
  finally
    XmlReader.Free;
  end;
end;

{ TXmlExport }

procedure TXmlExport.Execute(const FileName: string);
var
  XmlWriter: TO2XmlWriter;
begin
  XmlWriter := TO2XmlWriter.Create(O2File);
  try
    XmlWriter.SaveToFile(FileName);
  finally
    XmlWriter.Free;
  end;
end;

end.
