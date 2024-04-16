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

unit uObjectModels;

interface

uses
  Classes, uServices, uPasswordStrengthInfo, uO2File, uO2Objects;

type
  TObjectPropsModel = class(TPasswordStrengthInfo, IObjectProps)
  private
    function GetObjectName: string;
    function GetTags: TStrings;
    function GetObjectTags: TStrings;
    function GetFieldNames: TStrings;
    function GetFieldValues: TStrings;
    function GetObjectFieldNames(Index: Integer): string;
    function GetObjectFieldValues(Index: Integer): string;
    function GetFieldCount: Integer;
    function GetFieldIndex: Integer;
    function GetFieldName: string;
    function GetFieldValue: string;
    function GetObjectNotes: TStrings;
    function GetMarkdown: Boolean;
    function GetO2Object: TO2Object;
    function GetValid: Boolean;
    procedure SetObjectName(const Value: string);
    procedure SetObjectTags(const Value: TStrings);
    procedure SetFieldIndex(const Value: Integer);
    procedure SetFieldName(const Value: string);
    procedure SetFieldValue(const Value: string);
    procedure SetObjectNotes(const Value: TStrings);
    procedure SetMarkdown(const Value: Boolean);
  protected
    FO2File: TO2File;
    FO2Object: TO2Object;
    FObjectName: string;
    FTags: TStrings;
    FObjectTags: TStrings;
    FFieldNames: TStrings;
    FFieldValues: TStrings;
    FObjectFieldNames: TStrings;
    FObjectFieldValues: TStrings;
    FFieldIndex: Integer;
    FFieldName: string;
    FFieldValue: string;
    FObjectNotes: TStrings;
    FMarkdown: Boolean;
    procedure Build;
    procedure EnsureAssigned;
  public
    constructor Create(const O2File: TO2File);
    destructor Destroy; override;

    procedure EvaluatePasswordStrength(const APassword: string); override;

    function CanAddField: Boolean;
    procedure AddField;

    function CanReplaceField: Boolean;
    procedure ReplaceField;

    function CanDeleteField: Boolean;
    procedure DeleteField;

    procedure SwapFields(OtherIndex: Integer);

    procedure ApplyChanges; virtual;

    property ObjectName: string read GetObjectName write SetObjectName;
    property Tags: TStrings read GetTags;
    property ObjectTags: TStrings read GetObjectTags write SetObjectTags;
    property FieldNames: TStrings read GetFieldNames;
    property FieldValues: TStrings read GetFieldValues;
    property ObjectFieldNames[Index: Integer]: string
      read GetObjectFieldNames;
    property ObjectFieldValues[Index: Integer]: string
      read GetObjectFieldValues;
    property FieldCount: Integer read GetFieldCount;
    property FieldIndex: Integer read GetFieldIndex write SetFieldIndex;
    property FieldName: string read GetFieldName write SetFieldName;
    property FieldValue: string read GetFieldValue write SetFieldValue;
    property ObjectNotes: TStrings read GetObjectNotes write SetObjectNotes;
    property Markdown: Boolean read GetMarkdown write SetMarkdown;

    property O2Object: TO2Object read GetO2Object;

    property Valid: Boolean read GetValid;
  end;

  TNewObjectModel = class(TObjectPropsModel)
  public
    procedure ApplyChanges; override;
  end;

  TDuplicateObjectModel = class(TObjectPropsModel)
  public
    constructor Create(const O2File: TO2File; const Source: TO2Object);

    procedure ApplyChanges; override;
  end;

  TEditObjectModel = class(TObjectPropsModel)
  public
    constructor Create(const O2File: TO2File; const O2Object: TO2Object);
  end;

implementation

uses
  uO2Rules, uO2ObjectsUtils;

{ TObjectPropsModel }

constructor TObjectPropsModel.Create(const O2File: TO2File);
begin
  inherited Create;
  FO2File := O2File;

  FTags := TStringList.Create;
  AppendTagsToList(FO2File.Objects.ToEnumerable, FTags);

  FObjectTags := TStringList.Create;

  FFieldNames := TStringList.Create;
  AppendFieldNamesToList(FO2File.Objects.ToEnumerable, FFieldNames);

  FFieldValues := TStringList.Create;
  AppendFieldValuesToList(FO2File.Objects.ToEnumerable, FFieldValues);

  FObjectFieldNames := TStringList.Create;
  TStringList(FObjectFieldNames).CaseSensitive := False;

  FObjectFieldValues := TStringList.Create;

  FFieldIndex := -1;
  FPasswordScore := -1;

  FObjectNotes := TStringList.Create;
end;

destructor TObjectPropsModel.Destroy;
begin
  FTags.Free;
  FObjectTags.Free;
  FFieldNames.Free;
  FFieldValues.Free;
  FObjectFieldNames.Free;
  FObjectFieldValues.Free;
  FObjectNotes.Free;
  inherited;
end;

procedure TObjectPropsModel.AddField;
begin
  Inc(FFieldIndex);
  FObjectFieldNames.Insert(FFieldIndex, FFieldName);
  FObjectFieldValues.Insert(FFieldIndex, FFieldValue);
end;

procedure TObjectPropsModel.ApplyChanges;
var
  I: Integer;
const
  TextTypes: array[Boolean] of TO2TextType = (ttPlainText, ttCommonMark);
begin
  FO2Object.Name := FObjectName;

  FO2Object.SetTags(FObjectTags);

  FO2Object.Fields.Clear;
  for I := 0 to FObjectFieldNames.Count - 1 do
    with FO2Object.Fields.AddField(FObjectFieldNames[I]) do
      FieldValue := FObjectFieldValues[I];

  FO2Object.Text := FObjectNotes;
  FO2Object.TextType := TextTypes[FMarkdown];
end;

procedure TObjectPropsModel.Build;
var
  AField: TO2Field;
begin
  FObjectName := FO2Object.Name;

  FO2Object.GetTags(FObjectTags);

  for AField in FO2Object.Fields do
  begin
    FObjectFieldNames.Add(AField.FieldName);
    FObjectFieldValues.Add(AField.FieldValue);
  end;

  FObjectNotes.Assign(FO2Object.Text);
  FMarkdown := FO2Object.TextType = ttCommonMark;
end;

function TObjectPropsModel.CanAddField: Boolean;
begin
  Result := (FFieldName <> '')
    and (FFieldIndex <= FObjectFieldValues.Count)
    and (FObjectFieldNames.IndexOf(FFieldName) = -1);
end;

function TObjectPropsModel.CanDeleteField: Boolean;
begin
  Result :=(FFieldIndex >= 0) and (FFieldIndex < FObjectFieldValues.Count);
end;

function TObjectPropsModel.CanReplaceField: Boolean;
var
  FieldNameIndex: Integer;
begin
  FieldNameIndex := FObjectFieldNames.IndexOf(FFieldName);
  Result := (FFieldName <> '')
    and (FFieldIndex >= 0) and (FFieldIndex < FObjectFieldValues.Count)
    and ((FieldNameIndex = -1) or (FieldNameIndex = FFieldIndex));
end;

procedure TObjectPropsModel.DeleteField;
begin
  FObjectFieldNames.Delete(FFieldIndex);
  FObjectFieldValues.Delete(FFieldIndex);

  if FFieldIndex >= FObjectFieldNames.Count then
    FFieldIndex := FObjectFieldNames.Count - 1;
end;

procedure TObjectPropsModel.EnsureAssigned;
begin
  if Assigned(FO2Object) then Exit;
  FO2Object := FO2File.Objects.AddObject(FObjectName);
end;

procedure TObjectPropsModel.EvaluatePasswordStrength(const APassword: string);
var
  ARule: TO2Rule;
begin
  for ARule in FO2File.Rules do
    if (ARule.RuleType = rtPassword) and ARule.DisplayPasswordStrength
      and ARule.Matches(FFieldName, FFieldValue) then
    begin
      inherited;
      Exit;
    end;

  FPasswordScore := -1;
  FPasswordStrengthInfo := '';
end;

function TObjectPropsModel.GetFieldCount: Integer;
begin
  Result := FObjectFieldNames.Count;
end;

function TObjectPropsModel.GetFieldIndex: Integer;
begin
  Result := FFieldIndex;
end;

function TObjectPropsModel.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TObjectPropsModel.GetFieldNames: TStrings;
begin
  Result := FFieldNames;
end;

function TObjectPropsModel.GetObjectFieldNames(Index: Integer): string;
begin
  Result := FObjectFieldNames[Index];
end;

function TObjectPropsModel.GetFieldValue: string;
begin
  Result := FFieldValue;
end;

function TObjectPropsModel.GetFieldValues: TStrings;
begin
  Result := FFieldValues;
end;

function TObjectPropsModel.GetObjectFieldValues(Index: Integer): string;
begin
  Result := FObjectFieldValues[Index];
end;

function TObjectPropsModel.GetMarkdown: Boolean;
begin
  Result := FMarkdown;
end;

function TObjectPropsModel.GetO2Object: TO2Object;
begin
  Result := FO2Object;
end;

function TObjectPropsModel.GetObjectName: string;
begin
  Result := FObjectName;
end;

function TObjectPropsModel.GetObjectNotes: TStrings;
begin
  Result := FObjectNotes;
end;

function TObjectPropsModel.GetObjectTags: TStrings;
begin
  Result := FObjectTags;
end;

function TObjectPropsModel.GetTags: TStrings;
begin
  Result := FTags;
end;

function TObjectPropsModel.GetValid: Boolean;
begin
  Result := FObjectName <> '';
end;

procedure TObjectPropsModel.ReplaceField;
begin
  FObjectFieldNames[FFieldIndex] := FFieldName;
  FObjectFieldValues[FFieldIndex] := FFieldValue;
end;

procedure TObjectPropsModel.SetFieldIndex(const Value: Integer);
begin
  if FFieldIndex <> Value then
  begin
    FFieldIndex := Value;
    
    FFieldName := FObjectFieldNames[FFieldIndex];
    FFieldValue := FObjectFieldValues[FFieldIndex];

    EvaluatePasswordStrength(FFieldValue);

    FFieldValues.Clear;
    AppendFieldValuesToList(
      FO2File.Objects.ToEnumerable, FFieldName, FFieldValues);
  end;
end;

procedure TObjectPropsModel.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then
  begin
    FFieldName := Value;

    EvaluatePasswordStrength(FFieldValue);

    FFieldValues.Clear;
    AppendFieldValuesToList(
      FO2File.Objects.ToEnumerable, FFieldName, FFieldValues);
  end;
end;

procedure TObjectPropsModel.SetFieldValue(const Value: string);
begin
  if FFieldValue <> Value then
  begin
    FFieldValue := Value;

    EvaluatePasswordStrength(FFieldValue);
  end;
end;

procedure TObjectPropsModel.SetMarkdown(const Value: Boolean);
begin
  FMarkdown := Value;
end;

procedure TObjectPropsModel.SetObjectName(const Value: string);
begin
  FObjectName := Value;
end;

procedure TObjectPropsModel.SetObjectNotes(const Value: TStrings);
begin
  FObjectNotes.Assign(Value);
end;

procedure TObjectPropsModel.SetObjectTags(const Value: TStrings);
begin
  FObjectTags.Assign(Value);
end;

procedure TObjectPropsModel.SwapFields(OtherIndex: Integer);
begin
  FObjectFieldNames.Exchange(FFieldIndex, OtherIndex);
  FObjectFieldValues.Exchange(FFieldIndex, OtherIndex);
  FFieldIndex := OtherIndex;
end;

{ TNewObjectModel }

procedure TNewObjectModel.ApplyChanges;
begin
  EnsureAssigned;
  inherited;
end;

{ TDuplicateObjectModel }

constructor TDuplicateObjectModel.Create(const O2File: TO2File;
  const Source: TO2Object);
begin
  inherited Create(O2File);
  FO2Object := Source;
  Build;
  FO2Object := nil;
  FObjectName := '';
end;

procedure TDuplicateObjectModel.ApplyChanges;
begin
  EnsureAssigned;
  inherited;
end;

{ TEditObjectModel }

constructor TEditObjectModel.Create(const O2File: TO2File;
  const O2Object: TO2Object);
begin
  inherited Create(O2File);
  FO2Object := O2Object;
  Build;
end;

end.
