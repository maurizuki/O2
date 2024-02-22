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
  Classes, ComCtrls, uO2Objects;

type
  TO2ObjectListViewEnumerator = class(TInterfacedObject,
    IEnumerator<TO2Object>)
  private
    FListView: TCustomListView;
    FListItem: TListItem;
  public
    constructor Create(const ListView: TCustomListView);
    function GetCurrent: TObject;
    function GetCurrentT: TO2Object;
    function IEnumerator<TO2Object>.GetCurrent = GetCurrentT;
    function MoveNext: Boolean;
    procedure Reset;
  end;

  TO2ObjectListViewEnumerable = class(TInterfacedObject,
    IEnumerable<TO2Object>)
  private
    FListView: TCustomListView;
  public
    constructor Create(const ListView: TCustomListView);
    function GetEnumerator: IEnumerator;
    function GetEnumeratorT: IEnumerator<TO2Object>;
    function IEnumerable<TO2Object>.GetEnumerator = GetEnumeratorT;
  end;

function ObjectExists(Objects: IEnumerable<TO2Object>;
  const Obj: TO2Object): Boolean;

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

function ObjectExists(Objects: IEnumerable<TO2Object>;
  const Obj: TO2Object): Boolean;
var
  AObject: TO2Object;
begin
  Result := False;
  for AObject in Objects do if AObject = Obj then Exit(True);
end;

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

{ TO2ObjectListViewEnumerator }

constructor TO2ObjectListViewEnumerator.Create(
  const ListView: TCustomListView);
begin
  FListView := ListView;
  FListItem := nil;
end;

function TO2ObjectListViewEnumerator.GetCurrent: TObject;
begin
  Result := GetCurrentT;
end;

function TO2ObjectListViewEnumerator.GetCurrentT: TO2Object;
begin
  Result := FListItem.Data;
end;

function TO2ObjectListViewEnumerator.MoveNext: Boolean;
begin
  Result := True;
  if FListItem = nil then
    if FListView.Selected = nil then
      if FListView.Items.Count = 0 then
        Result := False
      else
        FListItem := FListView.Items[0]
    else
      FListItem := FListView.Selected
  else
    if FListView.Selected = nil then
      if FListItem.Index < FListView.Items.Count - 1 then
        FListItem := FListView.Items[FListItem.Index + 1]
      else
        Result := False
    else
    begin
      FListItem := FListView.GetNextItem(FListItem, sdAll, [isSelected]);
      Result := Assigned(FListItem);
    end;
end;

procedure TO2ObjectListViewEnumerator.Reset;
begin
  FListItem := nil;
end;

{ TO2ObjectListViewEnumerable }

constructor TO2ObjectListViewEnumerable.Create(
  const ListView: TCustomListView);
begin
  FListView := ListView;
end;

function TO2ObjectListViewEnumerable.GetEnumerator: IEnumerator;
begin
  Result := GetEnumeratorT;
end;

function TO2ObjectListViewEnumerable.GetEnumeratorT:
  IEnumerator<TO2Object>;
begin
  Result := TO2ObjectListViewEnumerator.Create(FListView);
end;

end.
