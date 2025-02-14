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

unit uXmlImportExport;

interface

uses
  uO2ImportExport;

type
  TXmlImport = class(TO2CustomImport)
  public
    procedure Execute(const FileName: string); override;
  end;

  TXmlExport = class(TO2CustomExport)
  public
    procedure Execute(const FileName: string); override;
  end;

implementation

uses
  uXmlSerialization, uO2Defs, uO2File;

{ TXmlImport }

procedure TXmlImport.Execute(const FileName: string);
var
  InputFile: TO2File;
  XmlReader: TXmlReader;
begin
  InputFile := TO2File.Create;
  try
    XmlReader := TXmlReader.Create(InputFile);
    try
      XmlReader.LoadFromFile(FileName);
    finally
      XmlReader.Free;
    end;
    ImportFrom(InputFile);
  finally
    InputFile.Free;
  end;
end;

{ TXmlExport }

procedure TXmlExport.Execute(const FileName: string);
var
  OutputFile: TO2File;
  XmlWriter: TXmlWriter;
begin
  OutputFile := TO2File.Create;
  try
    ExportTo(OutputFile);
    XmlWriter := TXmlWriter.Create(OutputFile, O2FileSchemaLocation);
    try
      XmlWriter.SaveToFile(FileName);
    finally
      XmlWriter.Free;
    end;
  finally
    OutputFile.Free;
  end;
end;

end.
