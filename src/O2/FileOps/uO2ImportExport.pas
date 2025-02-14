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

unit uO2ImportExport;

interface

uses
  uFileOperation, uO2File, uO2Objects, uO2Relations, uO2Rules;

type
  TO2CustomImport = class(TFileOperation)
  private
    function ImportObject(const AObject: TO2Object): TO2Object;
    function ImportRelation(const ARelation: TO2Relation): TO2Relation;
    function ImportRule(const ARule: TO2Rule): TO2Rule;
  protected
    procedure ImportFrom(const InputFile: TO2File);
  end;

  TO2Import = class(TO2CustomImport)
  private
    FPasswordProvider: IPasswordProvider;
  public
    constructor Create(const O2File: TO2File;
      PasswordProvider: IPasswordProvider); overload;
    procedure Execute(const FileName: string); override;
  end;

  TO2CustomExport = class(TFileOperation)
  private
    FSelectedObjects: IEnumerable<TO2Object>;
  protected
    procedure ExportTo(const OutputFile: TO2File);
  public
    constructor Create(const O2File: TO2File;
      SelectedObjects: IEnumerable<TO2Object>); overload;
  end;

  TO2Export = class(TO2CustomExport)
  public
    procedure Execute(const FileName: string); override;
  end;

implementation

uses
  Classes;

{ TO2CustomImport }

function TO2CustomImport.ImportObject(const AObject: TO2Object): TO2Object;
begin
  Result := FO2File.Objects.FindObjectByID(AObject.ObjectID);
  if Result = nil then Result := FO2File.Objects.AddObject(AObject.Name);
  Result.Assign(AObject);
end;

function TO2CustomImport.ImportRelation(
  const ARelation: TO2Relation): TO2Relation;
begin
  Result := FO2File.Relations.FindRelationByID(ARelation.RelationID);
  if Result = nil then Result := FO2File.Relations.AddRelation;
  Result.Assign(ARelation);
end;

function TO2CustomImport.ImportRule(const ARule: TO2Rule): TO2Rule;
begin
  Result := FO2File.Rules.FindRule(ARule.Name);
  if Result = nil then Result := FO2File.Rules.AddRule(ARule.Name);
  Result.Assign(ARule);
end;

procedure TO2CustomImport.ImportFrom(const InputFile: TO2File);
var
  AObject: TO2Object;
  ARelation: TO2Relation;
  ARule: TO2Rule;
begin
  if FO2File.Title = '' then FO2File.Title := InputFile.Title;
  if FO2File.Description = '' then FO2File.Description := InputFile.Description;
  if FO2File.Author = '' then FO2File.Author := InputFile.Author;
  for AObject in InputFile.Objects do ImportObject(AObject);
  for ARelation in InputFile.Relations do ImportRelation(ARelation);
  for ARule in InputFile.Rules do ImportRule(ARule);
end;

{ TO2Import }

constructor TO2Import.Create(const O2File: TO2File;
  PasswordProvider: IPasswordProvider);
begin
  inherited Create(O2File);
  FPasswordProvider := PasswordProvider;
end;

procedure TO2Import.Execute(const FileName: string);
var
  InputFile: TO2File;
begin
  InputFile := TO2File.Create;
  try
    InputFile.FileName := FileName;
    InputFile.Load(FPasswordProvider);
    ImportFrom(InputFile);
  finally
    InputFile.Free;
  end;
end;

{ TO2CustomExport }

constructor TO2CustomExport.Create(const O2File: TO2File;
  SelectedObjects: IEnumerable<TO2Object>);
begin
  inherited Create(O2File);
  FSelectedObjects := SelectedObjects;
end;

procedure TO2CustomExport.ExportTo(const OutputFile: TO2File);
var
  AObject, NewObject: TO2Object;
  ARelation, NewRelation: TO2Relation;
begin
  OutputFile.Title := FO2File.Title;
  OutputFile.Description := FO2File.Description;
  OutputFile.Author := FO2File.Author;
  for AObject in FSelectedObjects do
  begin
    NewObject := OutputFile.Objects.AddObject;
    NewObject.Assign(AObject);
  end;
  for ARelation in FO2File.Relations do
    if Assigned(OutputFile.Objects.FindObjectByID(ARelation.ObjectID1))
      and Assigned(OutputFile.Objects.FindObjectByID(ARelation.ObjectID2))
    then begin
      NewRelation := OutputFile.Relations.AddRelation;
      NewRelation.Assign(ARelation);
    end;
  OutputFile.Rules := FO2File.Rules;
end;

{ TO2Export }

procedure TO2Export.Execute(const FileName: string);
var
  OutputFile: TO2File;
begin
  OutputFile := TO2File.Create;
  try
    ExportTo(OutputFile);
    OutputFile.FileName := FileName;
    OutputFile.Save;
  finally
    OutputFile.Free;
  end;
end;

end.
