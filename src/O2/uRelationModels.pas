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

unit uRelationModels;

interface

uses
  Classes, uServices, uO2File, uO2Objects, uO2Relations;

type
  TRelationPropsModel = class(TInterfacedObject, IRelationProps)
  private
    FRoles: TStrings;
    function GetObjectName1: string;
    function GetObjectName2: string;
    function GetRoles: TStrings;
    function GetRole1: string;
    function GetRole2: string;
    function GetRelation: TO2Relation;
    procedure SetRole1(const Value: string);
    procedure SetRole2(const Value: string);
  protected
    FO2File: TO2File;
    FRelation: TO2Relation;
    FObjectName1: string;
    FObjectName2: string;
    FRole1: string;
    FRole2: string;
  public
    constructor Create(const O2File: TO2File);
    destructor Destroy; override;

    procedure ApplyChanges; virtual; abstract;

    property ObjectName1: string read GetObjectName1;
    property ObjectName2: string read GetObjectName2;
    property Roles: TStrings read GetRoles;
    property Role1: string read GetRole1 write SetRole1;
    property Role2: string read GetRole2 write SetRole2;
    property Relation: TO2Relation read GetRelation;
  end;

  TNewRelationModel = class(TRelationPropsModel)
  private
    FObjectID1: string;
    FObjectID2: string;
  public
    constructor Create(const O2File: TO2File;
      SelectedObjects: IEnumerable<TO2Object>);

    procedure ApplyChanges; override;
  end;

  TEditRelationModel = class(TRelationPropsModel)
  public
    constructor Create(const O2File: TO2File; Relation: TO2Relation);

    procedure ApplyChanges; override;
  end;

implementation

{ TRelationPropsModel }

constructor TRelationPropsModel.Create(const O2File: TO2File);
begin
  FO2File := O2File;
  FRoles := TStringList.Create;
  O2File.Relations.GetRoles(FRoles);
end;

destructor TRelationPropsModel.Destroy;
begin
  FRoles.Free;
  inherited;
end;

function TRelationPropsModel.GetObjectName1: string;
begin
  Result := FObjectName1;
end;

function TRelationPropsModel.GetObjectName2: string;
begin
  Result := FObjectName2;
end;

function TRelationPropsModel.GetRelation: TO2Relation;
begin
  Result := FRelation;
end;

function TRelationPropsModel.GetRole1: string;
begin
  Result := FRole1;
end;

function TRelationPropsModel.GetRole2: string;
begin
  Result := FRole2;
end;

function TRelationPropsModel.GetRoles: TStrings;
begin
  Result := FRoles;
end;

procedure TRelationPropsModel.SetRole1(const Value: string);
begin
  FRole1 := Value;
end;

procedure TRelationPropsModel.SetRole2(const Value: string);
begin
  FRole2 := Value;
end;

{ TNewRelationModel }

constructor TNewRelationModel.Create(const O2File: TO2File;
  SelectedObjects: IEnumerable<TO2Object>);
var
  ObjEnum: IEnumerator<TO2Object>;
  AObject: TO2Object;
begin
  inherited Create(O2File);

  ObjEnum := SelectedObjects.GetEnumerator;

  if not ObjEnum.MoveNext then Exit;
  AObject := ObjEnum.Current;
  FObjectID1 := AObject.ObjectID;
  FObjectName1 := AObject.Name;

  if not ObjEnum.MoveNext then Exit;
  AObject := ObjEnum.Current;
  FObjectID2 := AObject.ObjectID;
  FObjectName2 := AObject.Name;
end;

procedure TNewRelationModel.ApplyChanges;
begin
  if FRelation = nil then
  begin
    FRelation := FO2File.Relations.AddRelation;
    FRelation.ObjectID1 := FObjectID1;
    FRelation.ObjectID2 := FObjectID2;
  end;
  FRelation.Role1 := FRole1;
  FRelation.Role2 := FRole2;
end;

{ TEditRelationModel }

constructor TEditRelationModel.Create(const O2File: TO2File;
  Relation: TO2Relation);
begin
  inherited Create(O2File);

  FRelation := Relation;
  FObjectName1 := O2File.Objects.FindObjectByID(Relation.ObjectID1).Name;
  FObjectName2 := O2File.Objects.FindObjectByID(Relation.ObjectID2).Name;
  FRole1 := Relation.Role1;
  FRole2 := Relation.Role2;
end;

procedure TEditRelationModel.ApplyChanges;
begin
  FRelation.Role1 := FRole1;
  FRelation.Role2 := FRole2;
end;

end.
