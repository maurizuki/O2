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

unit uReplaceOperations;

interface

uses
  Classes, uO2File, uO2Objects, uServices;

type
  TReplaceOperation = class(TInterfacedObject, IReplaceOperation)
  private
    FSearchList: TStrings;
    FReplaceList: TStrings;
    FSearchValue: string;
    FReplaceValue: string;
    function GetSearchList: TStrings;
    function GetReplaceList: TStrings;
    function GetSearchValue: string;
    function GetValid: Boolean;
    procedure SetSearchValue(const Value: string);
    function GetReplaceValue: string;
    procedure SetReplaceValue(const Value: string);
  protected
    FO2File: TO2File;
    FSelectedObjects: IEnumerable<TO2Object>;
    function GetTitle: string; virtual; abstract;
    function GetSearchValueLabel: string; virtual; abstract;
    function GetReplaceValueLabel: string; virtual; abstract;
    procedure Build; virtual; abstract;
  public
    constructor Create(const O2File: TO2File;
      SelectedObjects: IEnumerable<TO2Object>);
    destructor Destroy; override;
    procedure Replace; virtual; abstract;

    property Title: string read GetTitle;
    property SearchValueLabel: string read GetSearchValueLabel;
    property ReplaceValueLabel: string read GetReplaceValueLabel;
    property SearchList: TStrings read GetSearchList;
    property ReplaceList: TStrings read GetReplaceList;
    property SearchValue: string read GetSearchValue write SetSearchValue;
    property ReplaceValue: string read GetReplaceValue write SetReplaceValue;
    property Valid: Boolean read GetValid;
  end;

  TReplaceTagModel = class(TReplaceOperation)
  protected
    function GetTitle: string; override;
    function GetSearchValueLabel: string; override;
    function GetReplaceValueLabel: string; override;
    procedure Build; override;
  public
    procedure Replace; override;
  end;

  TReplaceFieldNameModel = class(TReplaceOperation)
  protected
    function GetTitle: string; override;
    function GetSearchValueLabel: string; override;
    function GetReplaceValueLabel: string; override;
    procedure Build; override;
  public
    procedure Replace; override;
  end;

  TReplaceFieldValueModel = class(TReplaceOperation)
  protected
    function GetTitle: string; override;
    function GetSearchValueLabel: string; override;
    function GetReplaceValueLabel: string; override;
    procedure Build; override;
  public
    procedure Replace; override;
  end;

  TReplaceRoleModel = class(TReplaceOperation)
  protected
    function GetTitle: string; override;
    function GetSearchValueLabel: string; override;
    function GetReplaceValueLabel: string; override;
    procedure Build; override;
  public
    procedure Replace; override;
  end;

implementation

uses
  SysUtils, uGlobal, uO2Relations, uO2ObjectsUtils;

{ TReplaceOperation }

constructor TReplaceOperation.Create(const O2File: TO2File;
  SelectedObjects: IEnumerable<TO2Object>);
begin
  FO2File := O2File;
  FSelectedObjects := SelectedObjects;
  FSearchList := TStringList.Create;
  FReplaceList := TStringList.Create;
  Build;
end;

destructor TReplaceOperation.Destroy;
begin
  FSearchList.Free;
  FReplaceList.Free;
  inherited;
end;

function TReplaceOperation.GetReplaceList: TStrings;
begin
  Result := FReplaceList;
end;

function TReplaceOperation.GetReplaceValue: string;
begin
  Result := FReplaceValue;
end;

function TReplaceOperation.GetSearchList: TStrings;
begin
  Result := FSearchList;
end;

function TReplaceOperation.GetSearchValue: string;
begin
  Result := FSearchValue;
end;

function TReplaceOperation.GetValid: Boolean;
begin
  Result := (FSearchValue <> '') and (FReplaceValue <> '')
    and (FSearchList.IndexOf(FSearchValue) >= 0);
end;

procedure TReplaceOperation.SetReplaceValue(const Value: string);
begin
  FReplaceValue := Value;
end;

procedure TReplaceOperation.SetSearchValue(const Value: string);
begin
  FSearchValue := Value;
end;

{ TReplaceTagModel }

procedure TReplaceTagModel.Build;
begin
  AppendTagsToList(FSelectedObjects, SearchList);
  AppendTagsToList(FO2File.Objects.ToEnumerable, ReplaceList);
end;

function TReplaceTagModel.GetReplaceValueLabel: string;
begin
  Result := SReplaceTagDlgReplaceLabel;
end;

function TReplaceTagModel.GetSearchValueLabel: string;
begin
  Result := SReplaceTagDlgSearchLabel;
end;

function TReplaceTagModel.GetTitle: string;
begin
  Result := SReplaceTagDlgTitle;
end;

procedure TReplaceTagModel.Replace;
var
  TempList: TStringList;
  AObject: TO2Object;
  Index: Integer;
begin
  TempList := TStringList.Create;
  try
    for AObject in FSelectedObjects do
    begin
      TempList.Clear;
      AObject.GetTags(TempList);
      Index := TempList.IndexOf(SearchValue);
      if Index > -1 then
        TempList[Index] := ReplaceValue;
      AObject.SetTags(TempList);
    end;
  finally
    TempList.Free;
  end;
end;

{ TReplaceFieldNameModel }

procedure TReplaceFieldNameModel.Build;
begin
  AppendFieldNamesToList(FSelectedObjects, SearchList);
  AppendFieldNamesToList(FO2File.Objects.ToEnumerable, ReplaceList);
end;

function TReplaceFieldNameModel.GetReplaceValueLabel: string;
begin
  Result := SReplaceFieldNameDlgReplaceLabel;
end;

function TReplaceFieldNameModel.GetSearchValueLabel: string;
begin
  Result := SReplaceFieldNameDlgSearchLabel;
end;

function TReplaceFieldNameModel.GetTitle: string;
begin
  Result := SReplaceFieldNameDlgTitle;
end;

procedure TReplaceFieldNameModel.Replace;
var
  AObject: TO2Object;
  AField: TO2Field;
begin
  for AObject in FSelectedObjects do
  begin
    AField := AObject.Fields.FindField(SearchValue);
    if Assigned(AField) then
    try
      AField.FieldName := ReplaceValue;
    except
    end;
  end;
end;

{ TReplaceFieldValueModel }

procedure TReplaceFieldValueModel.Build;
begin
  AppendFieldNamesToList(FSelectedObjects, SearchList);
  AppendFieldValuesToList(FO2File.Objects.ToEnumerable, '', ReplaceList);
end;

function TReplaceFieldValueModel.GetReplaceValueLabel: string;
begin
  Result := SReplaceFieldValueDlgReplaceLabel;
end;

function TReplaceFieldValueModel.GetSearchValueLabel: string;
begin
  Result := SReplaceFieldValueDlgSearchLabel;
end;

function TReplaceFieldValueModel.GetTitle: string;
begin
  Result := SReplaceFieldValueDlgTitle;
end;

procedure TReplaceFieldValueModel.Replace;
var
  AObject: TO2Object;
  AField: TO2Field;
begin
  for AObject in FSelectedObjects do
  begin
    AField := AObject.Fields.FindField(SearchValue);
    if Assigned(AField) then AField.FieldValue := ReplaceValue;
  end;
end;

{ TReplaceRoleModel }

procedure TReplaceRoleModel.Build;
begin
  FO2File.Relations.GetRoles(FSelectedObjects, SearchList);
  FO2File.Relations.GetRoles(ReplaceList);
end;

function TReplaceRoleModel.GetReplaceValueLabel: string;
begin
  Result := SReplaceRoleDlgReplaceLabel;
end;

function TReplaceRoleModel.GetSearchValueLabel: string;
begin
  Result := SReplaceRoleDlgSearchLabel;
end;

function TReplaceRoleModel.GetTitle: string;
begin
  Result := SReplaceRoleDlgTitle;
end;

procedure TReplaceRoleModel.Replace;
var
  AObject: TO2Object;
  ARelation: TO2Relation;
begin
  for AObject in FSelectedObjects do
    for ARelation in FO2File.Relations do
    begin
      if SameText(ARelation.ObjectID1, AObject.ObjectID)
        and SameText(ARelation.Role1, SearchValue) then
        ARelation.Role1 := ReplaceValue;
      if SameText(ARelation.ObjectID2, AObject.ObjectID)
        and SameText(ARelation.Role2, SearchValue) then
        ARelation.Role2 := ReplaceValue;
    end;
end;

end.
