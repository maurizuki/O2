{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2015 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uO2Objects;

interface

uses
  Classes, Contnrs, SysUtils, uO2Classes;

type
  TO2Field = class(TO2CollectionItem)
  private
    FFieldName: string;
    FFieldValue: string;
    procedure SetFieldName(const Value: string);
    procedure SetFieldValue(const Value: string);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property FieldName: string read FFieldName write SetFieldName;
    property FieldValue: string read FFieldValue write SetFieldValue;
  end;

  TO2FieldsEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TO2Field;
    property Current: TO2Field read GetCurrent;
  end;

  TO2Fields = class(TO2Collection)
  private
    function GetFields(Index: Integer): TO2Field;
  public
    constructor Create(AOwner: TPersistent);
    function GetEnumerator: TO2FieldsEnumerator;
    function FindField(const FieldName: string): TO2Field;
    function FieldExists(const FieldName: string): Boolean;
    function AddField(const FieldName: string): TO2Field;
    procedure DeleteField(const FieldName: string);
    procedure ReadFieldValues(const StringList: TStrings);
    procedure WriteFieldValues(const StringList: TStrings);
    property Fields[Index: Integer]: TO2Field read GetFields; default;
  end;

  TO2Object = class(TO2CollectionItem)
  private
    FObjectID: string;
    FName: string;
    FTag: string;
    FFields: TO2Fields;
    FText: TStrings;
    procedure SetObjectID(const Value: string);
    procedure SetName(const Value: string);
    procedure SetTag(const Value: string);
    procedure SetFields(const Value: TO2Fields);
    procedure SetText(const Value: TStrings);
    procedure TextChange(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure GetTags(const List: TStrings);
    procedure SetTags(const List: TStrings);
    procedure AddTag(const Tag: string);
    procedure DeleteTag(const Tag: string);
  published
    property ObjectID: string read FObjectID write SetObjectID;
    property Name: string read FName write SetName;
    property Tag: string read FTag write SetTag;
    property Fields: TO2Fields read FFields write SetFields;
    property Text: TStrings read FText write SetText;
  end;

  TO2ObjectsEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TO2Object;
    property Current: TO2Object read GetCurrent;
  end;

  TO2Objects = class(TO2Collection)
  private
    function GetObjects(Index: Integer): TO2Object;
  public
    constructor Create(AOwner: TPersistent);
    function GetEnumerator: TO2ObjectsEnumerator;
    function FindObjectByID(const ObjectID: string): TO2Object;
    function FindObject(const Name: string): TO2Object;
    function ObjectExists(const Name: string): Boolean;
    function AddObject(const Name: string = ''): TO2Object;
    procedure DeleteObject(const Name: string);
    function ImportObject(const AObject: TO2Object): TO2Object;
    procedure GetFieldNames(const List: TStrings);
    procedure GetFieldValues(const FieldName: string; const List: TStrings);
    procedure GetTags(const List: TStrings);
    property Objects[Index: Integer]: TO2Object read GetObjects; default;
  end;

  TO2ObjectListEnumerator = class(TListEnumerator)
  public
    function GetCurrent: TO2Object;
    property Current: TO2Object read GetCurrent;
  end;

  TO2ObjectList = class(TObjectList)
  private
    function GetItem(Index: Integer): TO2Object;
    procedure SetItem(Index: Integer; AObject: TO2Object);
  public
    constructor Create;
    function Add(AObject: TO2Object): Integer;
    function Extract(AObject: TO2Object): TO2Object;
    function Remove(AObject: TO2Object): Integer;
    function GetEnumerator: TO2ObjectListEnumerator;
    function IndexOf(AObject: TO2Object): Integer;
    procedure Insert(Index: Integer; AObject: TO2Object);
    function First: TO2Object;
    function Last: TO2Object;
    procedure GetFieldNames(const List: TStrings);
    procedure GetTags(const List: TStrings);
    procedure AddTag(const Tag: string);
    procedure DeleteTag(const Tag: string);
    function ReplaceFieldName(const FieldName, NewFieldName: string): Integer;
    procedure ReplaceFieldValue(const FieldName, NewFieldValue: string);
    procedure ReplaceTag(const Tag, NewTag: string);
    property Items[Index: Integer]: TO2Object
      read GetItem write SetItem; default;
  end;

implementation

uses
  StrUtils, uO2File;

resourcestring
  SObjectAlreadyExists = 'Object %s already exists.';
  SObjectNameAlreadyExists = 'An object named "%s" already exists.';
  SFieldAlreadyExists = 'A field named "%s" already exists.';

{ TO2Field }

constructor TO2Field.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FFieldName := '';
  FFieldValue := '';
end;

procedure TO2Field.Assign(Source: TPersistent);
begin
  if Source is TO2Field then
  begin
    FieldName := TO2Field(Source).FieldName;
    FieldValue := TO2Field(Source).FieldValue;
  end
  else
    inherited Assign(Source);
end;

procedure TO2Field.SetFieldName(const Value: string);
var
  AField: TO2Field;
begin
  if FFieldName <> Value then
  begin
    AField := TO2Fields(Collection).FindField(Value);
    if Assigned(AField) and (AField <> Self) then
      raise Exception.CreateFmt(SFieldAlreadyExists, [Value]);
    FFieldName := Value;
    NotifyChanges(Self, onPropertyChanged);
  end;
end;

procedure TO2Field.SetFieldValue(const Value: string);
begin
  if FFieldValue <> Value then
  begin
    FFieldValue := Value;
    NotifyChanges(Self, onPropertyChanged);
  end;
end;

{ TO2FieldsEnumerator }

function TO2FieldsEnumerator.GetCurrent: TO2Field;
begin
  Result := TO2Field(inherited GetCurrent);
end;

{ TO2Fields }

constructor TO2Fields.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TO2Field);
end;

function TO2Fields.GetEnumerator: TO2FieldsEnumerator;
begin
  Result := TO2FieldsEnumerator.Create(Self);
end;

function TO2Fields.FindField(const FieldName: string): TO2Field;
var
  AField: TO2Field;
begin
  Result := nil;
  for AField in Self do
    if SameText(AField.FieldName, FieldName) then
    begin
      Result := AField;
      Break;
    end;
end;

function TO2Fields.FieldExists(const FieldName: string): Boolean;
begin
  Result := Assigned(FindField(FieldName));
end;

function TO2Fields.AddField(const FieldName: string): TO2Field;
begin
  Result := TO2Field(Add);
  try
    Result.FieldName := FieldName;
  except
    Delete(Result.Index);
    raise;
  end;
end;

procedure TO2Fields.DeleteField(const FieldName: string);
var
  AField: TO2Field;
begin
  AField := FindField(FieldName);
  if Assigned(AField) then
    Delete(AField.Index);
end;

function TO2Fields.GetFields(Index: Integer): TO2Field;
begin
  Result := TO2Field(Items[Index]);
end;

procedure TO2Fields.ReadFieldValues(const StringList: TStrings);
var
  AField: TO2Field;
begin
  StringList.BeginUpdate;
  try
    StringList.Clear;
    for AField in Self do
      StringList.Values[AField.FieldName] := AField.FieldValue;
  finally
    StringList.EndUpdate;
  end;
end;

procedure TO2Fields.WriteFieldValues(const StringList: TStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    Clear;
    for I := 0 to Pred(StringList.Count) do
      with AddField(StringList.Names[I]) do
        FieldValue := StringList.Values[FieldName];
  finally
    EndUpdate;
  end;
end;

{ TO2Object }

constructor TO2Object.Create(Collection: TCollection);
var
  GUID: TGUID;
begin
  inherited Create(Collection);
  CreateGUID(GUID);
  FObjectID := GUIDToString(GUID);
  FName := '';
  FFields := TO2Fields.Create(Self);
  FText := TStringList.Create;
  TStringList(Text).OnChange := TextChange;
end;

destructor TO2Object.Destroy;
begin
  FFields.Free;
  FText.Free;
  inherited Destroy;
end;

procedure TO2Object.Assign(Source: TPersistent);
begin
  if Source is TO2Object then
  begin
    ObjectID := TO2Object(Source).ObjectID;
    Name := TO2Object(Source).Name;
    Tag := TO2Object(Source).Tag;
    Fields := TO2Object(Source).Fields;
    Text := TO2Object(Source).Text;
  end
  else
    inherited Assign(Source);
end;

procedure TO2Object.GetTags(const List: TStrings);
var
  AList: TStringList;
begin
  AList := TStringList.Create;
  try
    AList.CaseSensitive := False;
    AList.Duplicates := dupIgnore;
    AList.Sorted := True;
    ExtractStrings([','], [' '], PChar(Tag), AList);
    List.AddStrings(AList);
  finally
    AList.Free;
  end;
end;

procedure TO2Object.SetTags(const List: TStrings);
var
  S, Item: string;
begin
  S := '';
  for Item in List do
    if TrimLeft(Item) <> '' then
      S := S + TrimLeft(Item) + ',';
  if IsDelimiter(',', S, Length(S)) then
    SetLength(S, Length(S) - 1);
  Tag := S;
end;

procedure TO2Object.AddTag(const Tag: string);
var
  AList: TStringList;
begin
  AList := TStringList.Create;
  try
    AList.CaseSensitive := False;
    AList.Duplicates := dupIgnore;
    AList.Sorted := True;
    GetTags(AList);
    AList.Add(Tag);
    SetTags(AList);
  finally
    Alist.Free;
  end;
end;

procedure TO2Object.DeleteTag(const Tag: string);
var
  AList: TStrings;
  Index: Integer;
begin
  AList := TStringList.Create;
  try
    GetTags(AList);
    Index := AList.IndexOf(Tag);
    if Index <> -1 then
    begin
      AList.Delete(Index);
      SetTags(AList);
    end;
  finally
    Alist.Free;
  end;
end;

procedure TO2Object.TextChange(Sender: TObject);
begin
  NotifyChanges(Self, onPropertyChanged);
end;

procedure TO2Object.SetObjectID(const Value: string);
var
  AObject: TO2Object;
begin
  if FObjectID <> Value then
  begin
    AObject := TO2Objects(Collection).FindObjectByID(Value);
    if Assigned(AObject) and (AObject <> Self) then
      raise Exception.CreateFmt(SObjectAlreadyExists, [Value]);
    FObjectID := Value;
    NotifyChanges(Self, onPropertyChanged);
  end;
end;

procedure TO2Object.SetName(const Value: string);
var
  AObject: TO2Object;
begin
  if FName <> Value then
  begin
    AObject := TO2Objects(Collection).FindObject(Value);
    if Assigned(AObject) and (AObject <> Self) then
      raise Exception.CreateFmt(SObjectNameAlreadyExists, [Value]);
    FName := Value;
    NotifyChanges(Self, onPropertyChanged);
  end;
end;

procedure TO2Object.SetTag(const Value: string);
begin
  if FTag <> Value then
  begin
    FTag := Value;
    NotifyChanges(Self, onPropertyChanged);
  end;
end;

procedure TO2Object.SetFields(const Value: TO2Fields);
begin
  FFields.Assign(Value);
end;

procedure TO2Object.SetText(const Value: TStrings);
begin
  FText.Assign(Value);
end;

{ TO2ObjectsEnumerator }

function TO2ObjectsEnumerator.GetCurrent: TO2Object;
begin
  Result := TO2Object(inherited GetCurrent);
end;

{ TO2Objects }

constructor TO2Objects.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TO2Object);
end;

function TO2Objects.GetEnumerator: TO2ObjectsEnumerator;
begin
  Result := TO2ObjectsEnumerator.Create(Self);
end;

function TO2Objects.GetObjects(Index: Integer): TO2Object;
begin
  Result := TO2Object(Items[Index]);
end;

function TO2Objects.FindObject(const Name: string): TO2Object;
var
  AObject: TO2Object;
begin
  Result := nil;
  for AObject in Self do
    if SameText(AObject.Name, Name) then
    begin
      Result := AObject;
      Break;
    end;
end;

function TO2Objects.FindObjectByID(const ObjectID: string): TO2Object;
var
  AObject: TO2Object;
begin
  Result := nil;
  for AObject in Self do
    if SameText(AObject.ObjectID, ObjectID) then
    begin
      Result := AObject;
      Break;
    end;
end;

function TO2Objects.ObjectExists(const Name: string): Boolean;
begin
  Result := Assigned(FindObject(Name));
end;

function TO2Objects.AddObject(const Name: string): TO2Object;
begin
  Result := TO2Object(Add);
  try
    Result.Name := Name;
  except
    Delete(Result.Index);
    raise;
  end;
end;

procedure TO2Objects.DeleteObject(const Name: string);
var
  AObject: TO2Object;
begin
  AObject := FindObject(Name);
  if Assigned(AObject) then
    Delete(AObject.Index);
end;

function TO2Objects.ImportObject(const AObject: TO2Object): TO2Object;
begin
  Result := FindObjectByID(AObject.ObjectID);
  if Result = nil then Result := AddObject(AObject.Name);
  Result.Assign(AObject);
end;

procedure TO2Objects.GetFieldNames(const List: TStrings);
var
  AList: TStringList;
  AObject: TO2Object;
  AField: TO2Field;
begin
  AList := TStringList.Create;
  try
    AList.CaseSensitive := False;
    AList.Duplicates := dupIgnore;
    AList.Sorted := True;
    for AObject in Self do
      for AField in AObject.Fields do
        AList.Add(AField.FieldName);
    List.AddStrings(AList);
  finally
    AList.Free;
  end;
end;

procedure TO2Objects.GetFieldValues(const FieldName: string;
  const List: TStrings);
var
  AList: TStringList;
  AObject: TO2Object;
  AField: TO2Field;
begin
  AList := TStringList.Create;
  try
    AList.CaseSensitive := False;
    AList.Duplicates := dupIgnore;
    AList.Sorted := True;
    for AObject in Self do
      for AField in AObject.Fields do
        if (FieldName = '') or SameText(AField.FieldName, FieldName) then
          AList.Add(AField.FieldValue);
    List.AddStrings(AList);
  finally
    AList.Free;
  end;
end;

procedure TO2Objects.GetTags(const List: TStrings);
var
  AList: TStringList;
  AObject: TO2Object;
begin
  AList := TStringList.Create;
  try
    AList.CaseSensitive := False;
    AList.Duplicates := dupIgnore;
    AList.Sorted := True;
    for AObject in Self do
      AObject.GetTags(AList);
    List.AddStrings(AList);
  finally
    AList.Free;
  end;
end;

{ TO2ObjectListEnumerator }

function TO2ObjectListEnumerator.GetCurrent: TO2Object;
begin
  Result := inherited GetCurrent;
end;

{ TO2ObjectList }

constructor TO2ObjectList.Create;
begin
  inherited Create(False);
end;

function TO2ObjectList.GetItem(Index: Integer): TO2Object;
begin
  Result := TO2Object(inherited Items[Index]);
end;

procedure TO2ObjectList.SetItem(Index: Integer; AObject: TO2Object);
begin
  inherited Items[Index] := AObject;
end;

function TO2ObjectList.Add(AObject: TO2Object): Integer;
begin
  Result := inherited Add(AObject);
end;

function TO2ObjectList.Extract(AObject: TO2Object): TO2Object;
begin
  Result := TO2Object(inherited Extract(AObject));
end;

function TO2ObjectList.Remove(AObject: TO2Object): Integer;
begin
  Result := inherited Remove(AObject);
end;

function TO2ObjectList.GetEnumerator: TO2ObjectListEnumerator;
begin
  Result := TO2ObjectListEnumerator.Create(Self);
end;

function TO2ObjectList.IndexOf(AObject: TO2Object): Integer;
begin
  Result := inherited IndexOf(AObject);
end;

procedure TO2ObjectList.Insert(Index: Integer; AObject: TO2Object);
begin
  inherited Insert(Index, AObject);
end;

function TO2ObjectList.First: TO2Object;
begin
  Result := TO2Object(inherited First);
end;

function TO2ObjectList.Last: TO2Object;
begin
  Result := TO2Object(inherited Last);
end;

procedure TO2ObjectList.GetFieldNames(const List: TStrings);
var
  AList: TStringList;
  AObject: TO2Object;
  AField: TO2Field;
begin
  AList := TStringList.Create;
  try
    AList.CaseSensitive := False;
    AList.Duplicates := dupIgnore;
    AList.Sorted := True;
    for AObject in Self do
      for AField in AObject.Fields do
        AList.Add(AField.FieldName);
    List.AddStrings(AList);
  finally
    AList.Free;
  end;
end;

procedure TO2ObjectList.GetTags(const List: TStrings);
var
  AList: TStringList;
  AObject: TO2Object;
begin
  AList := TStringList.Create;
  try
    AList.CaseSensitive := False;
    AList.Duplicates := dupIgnore;
    AList.Sorted := True;
    for AObject in Self do
      AObject.GetTags(AList);
    List.AddStrings(AList);
  finally
    AList.Free;
  end;
end;

procedure TO2ObjectList.AddTag(const Tag: string);
var
  AObject: TO2Object;
begin
  for AObject in Self do
    AObject.AddTag(Tag);
end;

procedure TO2ObjectList.DeleteTag(const Tag: string);
var
  AObject: TO2Object;
begin
  for AObject in Self do
    AObject.DeleteTag(Tag);
end;

function TO2ObjectList.ReplaceFieldName(const FieldName,
  NewFieldName: string): Integer;
var
  AObject: TO2Object;
  AField: TO2Field;
begin
  Result := 0;
  for AObject in Self do
  begin
    AField := AObject.Fields.FindField(FieldName);
    if Assigned(AField) then
    try
      AField.FieldName := NewFieldName;
    except
      Inc(Result);
    end;
  end;
end;

procedure TO2ObjectList.ReplaceFieldValue(const FieldName,
  NewFieldValue: string);
var
  AObject: TO2Object;
  AField: TO2Field;
begin
  for AObject in Self do
  begin
    AField := AObject.Fields.FindField(FieldName);
    if Assigned(AField) then
      AField.FieldValue := NewFieldValue;
  end;
end;

procedure TO2ObjectList.ReplaceTag(const Tag, NewTag: string);
var
  AObject: TO2Object;
  List: TStringList;
  Index: Integer;
begin
  List := TStringList.Create;
  try
    for AObject in Self do
    begin
      List.Clear;
      AObject.GetTags(List);
      Index := List.IndexOf(Tag);
      if Index > -1 then
        List[Index] := NewTag;
      AObject.SetTags(List);
    end;
  finally
    List.Free;
  end;
end;

end.
