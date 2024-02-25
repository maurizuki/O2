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

unit uO2Relations;

interface

uses
  Classes, Contnrs, SysUtils, Types, System.Generics.Collections,
  System.Generics.Defaults, uO2Classes, uO2Objects;

type
  TO2ObjRelation = class;

  TO2Relation = class(TO2CollectionItem)
  private
    FRelationID: string;
    FObjectID1: string;
    FObjectID2: string;
    FRole1: string;
    FRole2: string;
    procedure SetRelationID(const Value: string);
    procedure SetObjectID1(const Value: string);
    procedure SetObjectID2(const Value: string);
    procedure SetRole1(const Value: string);
    procedure SetRole2(const Value: string);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function GetObjectRelation(const AObject: TO2Object): TO2ObjRelation;
  published
    property RelationID: string read FRelationID write SetRelationID;
    property ObjectID1: string read FObjectID1 write SetObjectID1;
    property ObjectID2: string read FObjectID2 write SetObjectID2;
    property Role1: string read FRole1 write SetRole1;
    property Role2: string read FRole2 write SetRole2;
  end;

  TO2RelationsEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TO2Relation;
    property Current: TO2Relation read GetCurrent;
  end;

  TO2ObjRelations = class;

  TO2Relations = class(TO2Collection)
  private
    function GetRelations(Index: Integer): TO2Relation;
  public
    constructor Create(AOwner: TPersistent);
    function GetEnumerator: TO2RelationsEnumerator;
    function FindRelationByID(const RelationID: string): TO2Relation;
    function GetObjectRelations(const AObject: TO2Object): TO2ObjRelations;
    procedure DeleteObjectRelations(const AObject: TO2Object);
    function AddRelation: TO2Relation;
    function ImportRelation(const ARelation: TO2Relation): TO2Relation;
    procedure GetRoles(const List: TStrings); overload;
    procedure GetRoles(const Objects: TO2ObjectList; const List: TStrings); overload;
    procedure ReplaceRole(const Objects: TO2ObjectList; const Role, NewRole: string);
    property Relations[Index: Integer]: TO2Relation read GetRelations; default;
  end;

  TO2ObjRelation = class
  private
    FRelation: TO2Relation;
    FObject: TO2Object;
    FRole: string;
  public
    constructor Create;
    property Relation: TO2Relation read FRelation write FRelation;
    property Obj: TO2Object read FObject write FObject;
    property Role: string read FRole write FRole;
  end;

  TO2ObjRelations = class(TObjectList<TO2ObjRelation>)
  public
    procedure SortByObjName;
    procedure SortByRole;
  end;

  TCompareObjRelationByObjName = class(TComparer<TO2ObjRelation>)
  public
    function Compare(const Left, Right: TO2ObjRelation): Integer; override;
  end;

  TCompareObjRelationByRole = class(TComparer<TO2ObjRelation>)
  public
    function Compare(const Left, Right: TO2ObjRelation): Integer; override;
  end;

implementation

resourcestring
  SRelationAlreadyExists = 'Relation %s already exists.';

{ TO2Relation }

constructor TO2Relation.Create(Collection: TCollection);
var
  GUID: TGUID;
begin
  inherited Create(Collection);
  CreateGUID(GUID);
  FRelationID := GUIDToString(GUID);
  FObjectID1 := '';
  FObjectID2 := '';
  FRole1 := '';
  FRole2 := '';
end;

procedure TO2Relation.Assign(Source: TPersistent);
begin
  if Source is TO2Relation then
  begin
    RelationID := TO2Relation(Source).RelationID;
    ObjectID1 := TO2Relation(Source).ObjectID1;
    ObjectID2 := TO2Relation(Source).ObjectID2;
    Role1 := TO2Relation(Source).Role1;
    Role2 := TO2Relation(Source).Role2;
  end
  else
    inherited Assign(Source);
end;

function TO2Relation.GetObjectRelation(
  const AObject: TO2Object): TO2ObjRelation;
begin
  Result := TO2ObjRelation.Create;
  try
    Result.Relation := Self;
    if SameText(ObjectID1, AObject.ObjectID) then
    begin
      Result.Obj := TO2Objects(AObject.Collection).FindObjectByID(ObjectID2);
      Result.Role := Role2;
    end
    else if SameText(ObjectID2, AObject.ObjectID) then
    begin
      Result.Obj := TO2Objects(AObject.Collection).FindObjectByID(ObjectID1);
      Result.Role := Role1;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TO2Relation.SetRelationID(const Value: string);
var
  ARelation: TO2Relation;
begin
  if FRelationID <> Value then
  begin
    ARelation := TO2Relations(Collection).FindRelationByID(Value);
    if Assigned(ARelation) and (ARelation <> Self) then
      raise Exception.CreateFmt(SRelationAlreadyExists, [Value]);
    FRelationID := Value;
    NotifyChanges(Self, onPropertyChanged);
  end;
end;

procedure TO2Relation.SetObjectID1(const Value: string);
begin
  if FObjectID1 <> Value then
  begin
    FObjectID1 := Value;
    NotifyChanges(Self, onPropertyChanged);
  end;
end;

procedure TO2Relation.SetObjectID2(const Value: string);
begin
  if FObjectID2 <> Value then
  begin
    FObjectID2 := Value;
    NotifyChanges(Self, onPropertyChanged);
  end;
end;

procedure TO2Relation.SetRole1(const Value: string);
begin
  if FRole1 <> Value then
  begin
    FRole1 := Value;
    NotifyChanges(Self, onPropertyChanged);
  end;
end;

procedure TO2Relation.SetRole2(const Value: string);
begin
  if FRole2 <> Value then
  begin
    FRole2 := Value;
    NotifyChanges(Self, onPropertyChanged);
  end;
end;

{ TO2RelationsEnumerator }

function TO2RelationsEnumerator.GetCurrent: TO2Relation;
begin
  Result := TO2Relation(inherited GetCurrent);
end;

{ TO2Relations }

constructor TO2Relations.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TO2Relation);
end;

function TO2Relations.GetEnumerator: TO2RelationsEnumerator;
begin
  Result := TO2RelationsEnumerator.Create(Self);
end;

function TO2Relations.FindRelationByID(
  const RelationID: string): TO2Relation;
var
  ARelation: TO2Relation;
begin
  Result := nil;
  for ARelation in Self do
    if SameText(ARelation.RelationID, RelationID) then Exit(ARelation);
end;

function TO2Relations.GetObjectRelations(
  const AObject: TO2Object): TO2ObjRelations;
var
  ARelation: TO2Relation;
begin
  Result := TO2ObjRelations.Create;
  try
    for ARelation in Self do
      if SameText(ARelation.ObjectID1, AObject.ObjectID)
        or SameText(ARelation.ObjectID2, AObject.ObjectID) then
        Result.Add(ARelation.GetObjectRelation(AObject));
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TO2Relations.DeleteObjectRelations(const AObject: TO2Object);
var
  I: Integer;
begin
  I := 0;
  while I < Count do
    if SameText(Relations[I].ObjectID1, AObject.ObjectID)
      or SameText(Relations[I].ObjectID2, AObject.ObjectID) then
      Delete(I)
    else
      Inc(I);
end;

function TO2Relations.AddRelation: TO2Relation;
begin
  Result := TO2Relation(Add);
end;

function TO2Relations.ImportRelation(
  const ARelation: TO2Relation): TO2Relation;
begin
  Result := FindRelationByID(ARelation.RelationID);
  if Result = nil then Result := AddRelation;
  Result.Assign(ARelation);
end;

procedure TO2Relations.GetRoles(const List: TStrings);
var
  AList: TStringList;
  ARelation: TO2Relation;
begin
  AList := TStringList.Create;
  try
    AList.CaseSensitive := False;
    AList.Duplicates := dupIgnore;
    AList.Sorted := True;
    for ARelation in Self do
    begin
      AList.Add(ARelation.Role1);
      AList.Add(ARelation.Role2);
    end;
    List.AddStrings(AList);
  finally
    AList.Free;
  end;
end;

procedure TO2Relations.GetRoles(const Objects: TO2ObjectList;
  const List: TStrings);
var
  AList: TStringList;
  AObject: TO2Object;
  ARelation: TO2Relation;
begin
  AList := TStringList.Create;
  try
    AList.CaseSensitive := False;
    AList.Duplicates := dupIgnore;
    AList.Sorted := True;
    for AObject in Objects do
      for ARelation in Self do
      begin
        if SameText(ARelation.ObjectID1, AObject.ObjectID) then
          AList.Add(ARelation.Role1);
        if SameText(ARelation.ObjectID2, AObject.ObjectID) then
          AList.Add(ARelation.Role2);
      end;
    List.AddStrings(AList);
  finally
    AList.Free;
  end;
end;

procedure TO2Relations.ReplaceRole(const Objects: TO2ObjectList; const Role,
  NewRole: string);
var
  AObject: TO2Object;
  ARelation: TO2Relation;
begin
  for AObject in Objects do
    for ARelation in Self do
    begin
      if SameText(ARelation.ObjectID1, AObject.ObjectID) and SameText(ARelation.Role1, Role) then
        ARelation.Role1 := NewRole;
      if SameText(ARelation.ObjectID2, AObject.ObjectID) and SameText(ARelation.Role2, Role) then
        ARelation.Role2 := NewRole;
    end;
end;

function TO2Relations.GetRelations(Index: Integer): TO2Relation;
begin
  Result := TO2Relation(Items[Index]);
end;

{ TO2ObjRelation }

constructor TO2ObjRelation.Create;
begin
  inherited Create;
  FRelation := nil;
  FObject := nil;
  FRole := '';
end;

{ TO2ObjRelations }

procedure TO2ObjRelations.SortByObjName;
var
  AComparer: TCompareObjRelationByObjName;
begin
  AComparer := TCompareObjRelationByObjName.Create;
  try
    Sort(AComparer);
  finally
    AComparer.Free;
  end;
end;

procedure TO2ObjRelations.SortByRole;
var
  AComparer: TCompareObjRelationByRole;
begin
  AComparer := TCompareObjRelationByRole.Create;
  try
    Sort(AComparer);
  finally
    AComparer.Free;
  end;
end;

{ TCompareObjRelationByObjName }

function TCompareObjRelationByObjName.Compare(const Left,
  Right: TO2ObjRelation): Integer;
var
  NameLeft, NameRight: string;
begin
  if Assigned(TO2ObjRelation(Left).Obj) then
    NameLeft := TO2ObjRelation(Left).Obj.Name
  else
    NameLeft := '';
  if Assigned(TO2ObjRelation(Right).Obj) then
    NameRight := TO2ObjRelation(Right).Obj.Name
  else
    NameRight := '';
  Result := CompareText(NameLeft, NameRight);
end;

{ TCompareObjRelationByRole }

function TCompareObjRelationByRole.Compare(const Left,
  Right: TO2ObjRelation): Integer;
begin
  Result := CompareText(TO2ObjRelation(Left).Role, TO2ObjRelation(Right).Role);
end;

end.
