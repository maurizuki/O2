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

unit uO2Objects;

interface

uses
  Classes, uO2Classes;

type
  TO2TextType = (ttPlainText, ttCommonMark);

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
    function AddField(const FieldName: string): TO2Field;

    property Fields[Index: Integer]: TO2Field read GetFields; default;
  end;

  TO2Object = class(TO2CollectionItem)
  private
    FObjectID: string;
    FName: string;
    FTag: string;
    FFields: TO2Fields;
    FText: TStrings;
    FTextType: TO2TextType;

    procedure SetObjectID(const Value: string);
    procedure SetName(const Value: string);
    procedure SetTag(const Value: string);
    procedure SetFields(const Value: TO2Fields);
    procedure SetText(const Value: TStrings);
    procedure SetTextType(const Value: TO2TextType);

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
    property TextType: TO2TextType read FTextType write SetTextType;
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
    function AddObject(const Name: string = ''): TO2Object;

    function ToEnumerable: IEnumerable<TO2Object>;

    property Objects[Index: Integer]: TO2Object read GetObjects; default;
  end;

implementation

uses
  SysUtils;

resourcestring
  SObjectAlreadyExists = 'Object %s already exists.';
  SObjectNameAlreadyExists = 'An object named "%s" already exists.';
  SFieldAlreadyExists = 'A field named "%s" already exists.';

type
  TO2ObjectCollectionEnumerator = class(TInterfacedObject,
    IEnumerator<TO2Object>)
  private
    FCollection: TO2Objects;
    FIndex: Integer;
  public
    constructor Create(const Collection: TO2Objects);

    function GetCurrent: TObject;
    function GetCurrentT: TO2Object;
    function IEnumerator<TO2Object>.GetCurrent = GetCurrentT;
    function MoveNext: Boolean;
    procedure Reset;
  end;

  TO2ObjectCollectionEnumerable = class(TInterfacedObject,
    IEnumerable<TO2Object>)
  private
    FCollection: TO2Objects;
  public
    constructor Create(const Collection: TO2Objects);

    function GetEnumerator: IEnumerator;
    function GetEnumeratorT: IEnumerator<TO2Object>;
    function IEnumerable<TO2Object>.GetEnumerator = GetEnumeratorT;
  end;

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
    if SameText(AField.FieldName, FieldName) then Exit(AField);
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

function TO2Fields.GetFields(Index: Integer): TO2Field;
begin
  Result := TO2Field(Items[Index]);
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

procedure TO2Object.SetTextType(const Value: TO2TextType);
begin
  if FTextType <> Value then
  begin
    FTextType := Value;
    NotifyChanges(Self, onPropertyChanged);
  end;
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
    if SameText(AObject.Name, Name) then Exit(AObject);
end;

function TO2Objects.FindObjectByID(const ObjectID: string): TO2Object;
var
  AObject: TO2Object;
begin
  Result := nil;
  for AObject in Self do
    if SameText(AObject.ObjectID, ObjectID) then Exit(AObject);
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

function TO2Objects.ToEnumerable: IEnumerable<TO2Object>;
begin
  Result := TO2ObjectCollectionEnumerable.Create(Self);
end;

{ TO2ObjectCollectionEnumerator }

constructor TO2ObjectCollectionEnumerator.Create(const Collection: TO2Objects);
begin
  FCollection := Collection;
  FIndex := -1;
end;

function TO2ObjectCollectionEnumerator.GetCurrent: TObject;
begin
  Result := GetCurrentT;
end;

function TO2ObjectCollectionEnumerator.GetCurrentT: TO2Object;
begin
  Result := FCollection[FIndex];
end;

function TO2ObjectCollectionEnumerator.MoveNext: Boolean;
begin
  Result := True;
  if FIndex < FCollection.Count - 1 then
    Inc(FIndex)
  else
    Result := False;
end;

procedure TO2ObjectCollectionEnumerator.Reset;
begin
  FIndex := -1;
end;

{ TO2ObjectCollectionEnumerable }

constructor TO2ObjectCollectionEnumerable.Create(const Collection: TO2Objects);
begin
  FCollection := Collection;
end;

function TO2ObjectCollectionEnumerable.GetEnumerator: IEnumerator;
begin
  Result := GetEnumeratorT;
end;

function TO2ObjectCollectionEnumerable.GetEnumeratorT: IEnumerator<TO2Object>;
begin
  Result := TO2ObjectCollectionEnumerator.Create(FCollection);
end;

end.
