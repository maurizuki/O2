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

unit uHTMLExportModel;

interface

uses
  Windows, System.Generics.Collections, SysUtils, uO2File, uO2Objects,
  uServices;

type
  THTMLExportModel = class
  private
    FTitle: string;
    FO2File: TO2File;
    FSelectedObjects: TO2ObjectList;
    FStorage: IStorage;
    FIncludeIndex: Boolean;
    FIncludeTags: Boolean;
    FIncludeNotes: Boolean;
    FIncludeRelations: Boolean;
    FIncludePasswords: Boolean;
    FStyleIndex: Integer;
    FStyles: TList<string>;
    FBuilder: TStringBuilder;
    procedure AppendObjectIndex;
    procedure AppendObjectList;
    procedure AppendTagList(const Obj: TO2Object);
    procedure AppendFieldList(const Obj: TO2Object);
    procedure AppendRelationList(const Obj: TO2Object);
  public
    constructor Create(const O2File: TO2File;
      const SelectedObjects: TO2ObjectList; Storage: IStorage);
    destructor Destroy; override;
    procedure StoreSettings;
    function AddStyle(const Style: string): Integer;
    function ExportToHTML: string; overload;
    procedure ExportToHTML(const FileName: string); overload;
    property IncludeIndex: Boolean read FIncludeIndex write FIncludeIndex;
    property IncludeTags: Boolean read FIncludeTags write FIncludeTags;
    property IncludeNotes: Boolean read FIncludeNotes write FIncludeNotes;
    property IncludeRelations: Boolean read FIncludeRelations
      write FIncludeRelations;
    property IncludePasswords: Boolean read FIncludePasswords
      write FIncludePasswords;
    property StyleIndex: Integer read FStyleIndex write FStyleIndex;
  end;

implementation

uses
  Classes, JclFileUtils, uGlobal, uAppFiles, uHTMLHelper, uO2Relations,
  uO2Rules;

{ THTMLExportModel }

constructor THTMLExportModel.Create(const O2File: TO2File;
  const SelectedObjects: TO2ObjectList; Storage: IStorage);
begin
  if O2File.Title = '' then
    FTitle := ChangeFileExt(ExtractFileName(O2File.FileName), '')
  else
    FTitle := O2File.Title;
  FO2File := O2File;
  FSelectedObjects := SelectedObjects;
  FStorage := Storage;
  FIncludeIndex := FStorage.ReadBoolean(IdHTMLExportIncludeIndex, True);
  FIncludeTags := FStorage.ReadBoolean(IdHTMLExportIncludeTags, True);
  FIncludeNotes := FStorage.ReadBoolean(IdHTMLExportIncludeNotes, True);
  FIncludeRelations := FStorage.ReadBoolean(IdHTMLExportIncludeRelations, True);
  FIncludePasswords := FStorage.ReadBoolean(IdHTMLExportIncludePasswords, True);
  FStyleIndex := 0;
  FStyles := TList<string>.Create;
  FBuilder := TStringBuilder.Create;
end;

destructor THTMLExportModel.Destroy;
begin
  FStyles.Free;
  FBuilder.Free;
  inherited;
end;

procedure THTMLExportModel.StoreSettings;
begin
  FStorage.WriteBoolean(IdHTMLExportIncludeIndex, FIncludeIndex);
  FStorage.WriteBoolean(IdHTMLExportIncludeTags, FIncludeTags);
  FStorage.WriteBoolean(IdHTMLExportIncludeNotes, FIncludeNotes);
  FStorage.WriteBoolean(IdHTMLExportIncludeRelations, FIncludeRelations);
  FStorage.WriteBoolean(IdHTMLExportIncludePasswords, FIncludePasswords);
end;

function THTMLExportModel.AddStyle(const Style: string): Integer;
begin
  Result := FStyles.Add(Style);
end;

function THTMLExportModel.ExportToHTML: string;
var
  VersionInfo: TJclFileVersionInfo;
begin
  FBuilder.Clear;

  FBuilder.AppendLine('<!DOCTYPE html>')
    .AppendLine('<html>')
    .AppendLine('<head>');

  VersionInfo := TJclFileVersionInfo.Create(AppFiles.FullPath[IdAppExe]);
  try
    FBuilder.AppendFormat('<meta name="generator" content="%s %s" />',
        [VersionInfo.ProductName, VersionInfo.BinFileVersion]);
  finally
    VersionInfo.Free;
  end;

  FBuilder.AppendFormat('<meta name="description" content="%s" />',
      [FO2File.Description])
    .AppendFormat('<meta name="author" content="%s" />', [FO2File.Author])
    .AppendLine('<style>')
    .AppendLine(FStyles[FStyleIndex])
    .AppendLine('</style>')
    .Append('<title>').AppendHTML(FTitle).AppendLine('</title>')
    .AppendLine('</head>')
    .Append('<body><div class="container">')
    .Append('<h1>').AppendHTML(FTitle).Append('</h1>');

  if FIncludeIndex then AppendObjectIndex;

  AppendObjectList;

  FBuilder.AppendLine('</div></body>').Append('</html>');

  Result := FBuilder.ToString;
end;

procedure THTMLExportModel.ExportToHTML(const FileName: string);
var
  Writer: TTextWriter;
begin
  Writer := TStreamWriter.Create(FileName);
  try
    Writer.Write(ExportToHTML);
  finally
    Writer.Free;
  end;
end;

procedure THTMLExportModel.AppendObjectIndex;
var
  AObject: TO2Object;
begin
  FBuilder.Append('<div class="object-index"><ul>');

  for AObject in FSelectedObjects do
    FBuilder.AppendFormat('<li><a href="#%d">', [AObject.Index])
      .AppendHTML(AObject.Name).Append('</a></li>');

  FBuilder.Append('</ul></div>');
end;

procedure THTMLExportModel.AppendObjectList;
var
  I: Integer;
begin
    FBuilder.Append('<div class="object-list">');

    for I := 0 to FSelectedObjects.Count - 1 do
    begin
      FBuilder.AppendFormat('<a name="%d"></a>', [FSelectedObjects[I].Index])
        .Append('<div class="object-item"><h2>')
        .AppendHTML(FSelectedObjects[I].Name).Append('</h2>');

      if FIncludeTags then AppendTagList(FSelectedObjects[I]);

      AppendFieldList(FSelectedObjects[I]);

      if FIncludeRelations then AppendRelationList(FSelectedObjects[I]);

      if FIncludeNotes and (FSelectedObjects[I].Text.Count > 0) then
        FBuilder.Append('<div class="notes">')
          .AppendHTML(FSelectedObjects[I].Text, FSelectedObjects[I].TextType)
          .Append('</div>');

      FBuilder.Append('</div>');
    end;

    FBuilder.Append('</div>');
end;

procedure THTMLExportModel.AppendTagList(const Obj: TO2Object);
var
  Tags: TStrings;
  ATag: string;
begin
  Tags := TStringList.Create;
  try
    Obj.GetTags(Tags);

    if Tags.Count > 0 then
    begin
      FBuilder.Append('<div class="tag-list">');

      for ATag in Tags do
        FBuilder.Append('<div class="tag-item">')
          .AppendHTML(ATag).Append('</div>');

      FBuilder.Append('</div>');
    end;
  finally
    Tags.Free;
  end;
end;

procedure THTMLExportModel.AppendFieldList(const Obj: TO2Object);
var
  Fields: TList<TO2Field>;
  AField: TO2Field;
  ARule: TO2Rule;
begin
  Fields := TList<TO2Field>.Create;
  try
    for AField in Obj.Fields do
      if FIncludePasswords
        or not Assigned(FO2File.Rules.FindFirstRule(AField, [rtPassword])) then
        Fields.Add(AField);

    if Fields.Count > 0 then
    begin
      FBuilder.Append('<div class="field-list">');

      for AField in Fields do
      begin
        FBuilder.Append('<div class="field-item"><div class="field-name">')
          .AppendHTML(AField.FieldName)
          .Append('</div><div class="field-value">');

        ARule := FO2File.Rules.FindFirstRule(AField, [rtHyperLink, rtEmail]);
        if Assigned(ARule) then
          case ARule.RuleType of
            rtHyperLink:
              FBuilder.AppendFormat('<a href="%s" target="_blank">',
                [ARule.GetHyperLink(AField)])
                .AppendHTML(AField.FieldValue).Append('</a>');
            rtEmail:
              FBuilder.AppendFormat('<a href="mailto:%s">', [AField.FieldValue])
                .AppendHTML(AField.FieldValue).Append('</a>');
          end
        else
          FBuilder.AppendHTML(AField.FieldValue);

        FBuilder.Append('</div></div>');
      end;

      FBuilder.Append('</div>');
    end;
  finally
    Fields.Free;
  end;
end;

procedure THTMLExportModel.AppendRelationList(const Obj: TO2Object);
var
  AObjRelations: TO2ObjRelations;
  AObjRelation: TO2ObjRelation;
begin
  AObjRelations := FO2File.Relations.GetObjectRelations(Obj);
  try
    if AObjRelations.Count > 0 then
    begin
      FBuilder.Append('<div class="relation-list">');

      AObjRelations.SortByObjName;
      for AObjRelation in AObjRelations do
        if Assigned(AObjRelation.Obj) then
        begin
          FBuilder
            .Append('<div class="relation-item"><div class="relation-object">');

          if FSelectedObjects.Contains(AObjRelation.Obj) then
            FBuilder.AppendFormat('<a href="#%d">', [AObjRelation.Obj.Index])
              .AppendHTML(AObjRelation.Obj.Name).Append('</a>')
          else
            FBuilder.AppendHTML(AObjRelation.Obj.Name);

          FBuilder.Append('</div><div class="relation-role">')
            .AppendHTML(AObjRelation.Role).Append('</div></div>');
        end;

      FBuilder.Append('</div>');
    end;
  finally
    AObjRelations.Free;
  end;
end;

end.
