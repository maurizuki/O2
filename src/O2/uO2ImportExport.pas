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

unit uO2ImportExport;

interface

uses
  uImportExport, uO2File, uO2Objects;

type
  TO2Import = class(TImportExport)
  private
    FPasswordQuery: TPasswordQueryEvent;
  public
    constructor Create(const O2File: TO2File;
      const PasswordQuery: TPasswordQueryEvent); overload;
    procedure Execute(const FileName: string); override;
    property PasswordQuery: TPasswordQueryEvent read FPasswordQuery;
  end;

  TO2Export = class(TImportExport)
  private
    FSelection: TO2ObjectList;
  public
    constructor Create(const O2File: TO2File;
      const Selection: TO2ObjectList); overload;
    procedure Execute(const FileName: string); override;
    property Selection: TO2ObjectList read FSelection;
  end;

implementation

uses
  Classes, uO2Rules, uO2Relations;

{ TO2Import }

constructor TO2Import.Create(const O2File: TO2File;
  const PasswordQuery: TPasswordQueryEvent);
begin
  inherited Create(O2File);
  FPasswordQuery := PasswordQuery;
end;

procedure TO2Import.Execute(const FileName: string);
var
  InputFile: TO2File;
  AObject: TO2Object;
  ARelation: TO2Relation;
  ARule: TO2Rule;
begin
  InputFile := TO2File.Create;
  try
    InputFile.OnPasswordQuery := PasswordQuery;
    InputFile.FileName := FileName;
    InputFile.Load;
    for AObject in InputFile.Objects do
      O2File.Objects.ImportObject(AObject);
    for ARelation in InputFile.Relations do
      O2File.Relations.ImportRelation(ARelation);
    for ARule in InputFile.Rules do
      O2File.Rules.ImportRule(ARule);
  finally
    InputFile.Free;
  end;
end;

{ TO2Export }

constructor TO2Export.Create(const O2File: TO2File;
  const Selection: TO2ObjectList);
begin
  inherited Create(O2File);
  FSelection := Selection;
end;

procedure TO2Export.Execute(const FileName: string);
var
  OutputFile: TO2File;
  AObject, NewObject: TO2Object;
  ARelation, NewRelation: TO2Relation;
begin
  OutputFile := TO2File.Create;
  try
    for AObject in Selection do
    begin
      NewObject := OutputFile.Objects.AddObject;
      NewObject.Assign(AObject);
    end;
    for ARelation in O2File.Relations do
      if Assigned(OutputFile.Objects.FindObjectByID(ARelation.ObjectID1))
        and Assigned(OutputFile.Objects.FindObjectByID(ARelation.ObjectID2))
      then begin
        NewRelation := OutputFile.Relations.AddRelation;
        NewRelation.Assign(ARelation);
      end;
    OutputFile.Rules := O2File.Rules;
    OutputFile.FileName := FileName;
    OutputFile.Save;
  finally
    OutputFile.Free;
  end;
end;

end.
