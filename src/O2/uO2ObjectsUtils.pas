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

unit uO2ObjectsUtils;

interface

uses
  Classes, uO2Objects;

procedure AppendFieldNamesToList(Objects: IEnumerable<TO2Object>;
  const FieldNames: TStrings);

function ReplaceObjectsFieldName(Objects: IEnumerable<TO2Object>;
  const FieldName, NewFieldName: string): Integer;

procedure AppendFieldValuesToList(Objects: IEnumerable<TO2Object>;
  const FieldName: string; const FieldValues: TStrings);

procedure ReplaceObjectsFieldValue(Objects: IEnumerable<TO2Object>;
  const FieldName, NewFieldValue: string);

procedure AppendTagsToList(Objects: IEnumerable<TO2Object>;
  const Tags: TStrings);

procedure ReplaceObjectsTag(Objects: IEnumerable<TO2Object>;
  const Tag, NewTag: string);

implementation

uses
  SysUtils;

procedure AppendFieldNamesToList(Objects: IEnumerable<TO2Object>;
  const FieldNames: TStrings);
var
  TempList: TStringList;
  AObject: TO2Object;
  AField: TO2Field;
begin
  TempList := TStringList.Create;
  try
    TempList.CaseSensitive := False;
    TempList.Duplicates := dupIgnore;
    TempList.Sorted := True;
    for AObject in Objects do
      for AField in AObject.Fields do
        TempList.Add(AField.FieldName);
    FieldNames.AddStrings(TempList);
  finally
    TempList.Free;
  end;
end;

function ReplaceObjectsFieldName(Objects: IEnumerable<TO2Object>;
  const FieldName, NewFieldName: string): Integer;
var
  AObject: TO2Object;
  AField: TO2Field;
begin
  Result := 0;
  for AObject in Objects do
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

procedure AppendFieldValuesToList(Objects: IEnumerable<TO2Object>;
  const FieldName: string; const FieldValues: TStrings);
var
  TempList: TStringList;
  AObject: TO2Object;
  AField: TO2Field;
begin
  TempList := TStringList.Create;
  try
    TempList.CaseSensitive := False;
    TempList.Duplicates := dupIgnore;
    TempList.Sorted := True;
    for AObject in Objects do
      for AField in AObject.Fields do
        if (FieldName = '') or SameText(AField.FieldName, FieldName) then
          TempList.Add(AField.FieldValue);
    FieldValues.AddStrings(TempList);
  finally
    TempList.Free;
  end;
end;

procedure ReplaceObjectsFieldValue(Objects: IEnumerable<TO2Object>;
  const FieldName, NewFieldValue: string);
var
  AObject: TO2Object;
  AField: TO2Field;
begin
  for AObject in Objects do
  begin
    AField := AObject.Fields.FindField(FieldName);
    if Assigned(AField) then
      AField.FieldValue := NewFieldValue;
  end;
end;

procedure AppendTagsToList(Objects: IEnumerable<TO2Object>;
  const Tags: TStrings);
var
  TempList: TStringList;
  AObject: TO2Object;
begin
  TempList := TStringList.Create;
  try
    TempList.CaseSensitive := False;
    TempList.Duplicates := dupIgnore;
    TempList.Sorted := True;
    for AObject in Objects do
      AObject.GetTags(TempList);
    Tags.AddStrings(TempList);
  finally
    TempList.Free;
  end;
end;

procedure ReplaceObjectsTag(Objects: IEnumerable<TO2Object>;
  const Tag, NewTag: string);
var
  TempList: TStringList;
  AObject: TO2Object;
  Index: Integer;
begin
  TempList := TStringList.Create;
  try
    for AObject in Objects do
    begin
      TempList.Clear;
      AObject.GetTags(TempList);
      Index := TempList.IndexOf(Tag);
      if Index > -1 then
        TempList[Index] := NewTag;
      AObject.SetTags(TempList);
    end;
  finally
    TempList.Free;
  end;
end;

end.
