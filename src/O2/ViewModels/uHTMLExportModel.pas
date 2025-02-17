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

unit uHTMLExportModel;

interface

uses
  Windows, Generics.Collections, SysUtils, uO2File, uO2Objects, uServices,
  uAppFiles, uUtils;

type
  THTMLExportModel = class(TInterfacedObject, IHTMLExport)
  private
    FTitle: string;
    FO2File: TO2File;
    FSelectedObjects: IEnumerable<TO2Object>;
    FAppVersionInfo: TAppVersionInfo;
    FAppFiles: IAppFiles;
    FStorage: IStorage;
    FIncludeIndex: Boolean;
    FIncludeTags: Boolean;
    FIncludeNotes: Boolean;
    FIncludeRelations: Boolean;
    FIncludePasswords: Boolean;
    FStyleIndex: Integer;
    FStyles: TList<string>;
    FBuilder: TStringBuilder;
    function GetIncludeIndex: Boolean;
    function GetIncludeTags: Boolean;
    function GetIncludeNotes: Boolean;
    function GetIncludeRelations: Boolean;
    function GetIncludePasswords: Boolean;
    function GetStyleIndex: Integer;
    procedure SetIncludeIndex(Value: Boolean);
    procedure SetIncludeTags(Value: Boolean);
    procedure SetIncludeNotes(Value: Boolean);
    procedure SetIncludeRelations(Value: Boolean);
    procedure SetIncludePasswords(Value: Boolean);
    procedure SetStyleIndex(Value: Integer);
    procedure AppendObjectIndex;
    procedure AppendObjectList;
    procedure AppendTagList(const Obj: TO2Object);
    procedure AppendFieldList(const Obj: TO2Object);
    procedure AppendRelationList(const Obj: TO2Object);
  public
    constructor Create(const O2File: TO2File;
      SelectedObjects: IEnumerable<TO2Object>; AppVersionInfo: TAppVersionInfo;
      AppFiles: IAppFiles; Storage: IStorage);
    destructor Destroy; override;

    procedure StoreSettings;

    function TryGetStyleFileName(Index: Integer; out FileName: string): Boolean;
    function AddStyle(const Style: string): Integer;

    function ExportToHTML(Preview: Boolean): string; overload;
    procedure ExportToHTML(const FileName: string); overload;

    property IncludeIndex: Boolean read GetIncludeIndex write SetIncludeIndex;
    property IncludeTags: Boolean read GetIncludeTags write SetIncludeTags;
    property IncludeNotes: Boolean read GetIncludeNotes write SetIncludeNotes;
    property IncludeRelations: Boolean read GetIncludeRelations
      write SetIncludeRelations;
    property IncludePasswords: Boolean read GetIncludePasswords
      write SetIncludePasswords;
    property StyleIndex: Integer read GetStyleIndex write SetStyleIndex;
  end;

implementation

uses
  Classes, uGlobal, uHTMLHelper, uO2Relations, uO2Rules, uO2ObjectsUtils,
  uO2RulesUtils;

{ THTMLExportModel }

constructor THTMLExportModel.Create(const O2File: TO2File;
  SelectedObjects: IEnumerable<TO2Object>; AppVersionInfo: TAppVersionInfo;
  AppFiles: IAppFiles; Storage: IStorage);
begin
  if O2File.Title = '' then
    FTitle := ChangeFileExt(ExtractFileName(O2File.FileName), '')
  else
    FTitle := O2File.Title;
  FO2File := O2File;
  FSelectedObjects := SelectedObjects;
  FAppVersionInfo := AppVersionInfo;
  FAppFiles := AppFiles;
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

procedure THTMLExportModel.SetIncludeIndex(Value: Boolean);
begin
  FIncludeIndex := Value;
end;

procedure THTMLExportModel.SetIncludeNotes(Value: Boolean);
begin
  FIncludeNotes := Value;
end;

procedure THTMLExportModel.SetIncludePasswords(Value: Boolean);
begin
  FIncludePasswords := Value;
end;

procedure THTMLExportModel.SetIncludeRelations(Value: Boolean);
begin
  FIncludeRelations := Value;
end;

procedure THTMLExportModel.SetIncludeTags(Value: Boolean);
begin
  FIncludeTags := Value;
end;

procedure THTMLExportModel.SetStyleIndex(Value: Integer);
begin
  FStyleIndex := Value;
end;

procedure THTMLExportModel.StoreSettings;
begin
  FStorage.WriteBoolean(IdHTMLExportIncludeIndex, FIncludeIndex);
  FStorage.WriteBoolean(IdHTMLExportIncludeTags, FIncludeTags);
  FStorage.WriteBoolean(IdHTMLExportIncludeNotes, FIncludeNotes);
  FStorage.WriteBoolean(IdHTMLExportIncludeRelations, FIncludeRelations);
  FStorage.WriteBoolean(IdHTMLExportIncludePasswords, FIncludePasswords);
end;

function THTMLExportModel.TryGetStyleFileName(Index: Integer;
  out FileName: string): Boolean;
var
  Id: string;
begin
  Id := IdHTMLStyle + IntToStr(Index);
  Result := FAppFiles.FileExists(Id);
  if Result then FileName := FAppFiles.FullPaths[Id];
end;

function THTMLExportModel.AddStyle(const Style: string): Integer;
begin
  Result := FStyles.Add(Style);
end;

function THTMLExportModel.ExportToHTML(Preview: Boolean): string;
begin
  FBuilder.Clear;

  FBuilder
    .AppendLine('<!DOCTYPE html>')
    .AppendLine('<html>');

  if Preview then FBuilder.AppendContextMenuBlockerScript;

  FBuilder
    .AppendLine('<head>')
    .AppendFormat('<meta name="generator" content="%s %s" />',
      [FAppVersionInfo.AppName, FAppVersionInfo.DisplayVersion])
    .AppendFormat('<meta name="description" content="%s" />',
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
    Writer.Write(ExportToHTML(False));
  finally
    Writer.Free;
  end;
end;

function THTMLExportModel.GetIncludeIndex: Boolean;
begin
  Result := FIncludeIndex;
end;

function THTMLExportModel.GetIncludeNotes: Boolean;
begin
  Result := FIncludeNotes;
end;

function THTMLExportModel.GetIncludePasswords: Boolean;
begin
  Result := FIncludePasswords;
end;

function THTMLExportModel.GetIncludeRelations: Boolean;
begin
  Result := FIncludeRelations;
end;

function THTMLExportModel.GetIncludeTags: Boolean;
begin
  Result := FIncludeTags;
end;

function THTMLExportModel.GetStyleIndex: Integer;
begin
  Result := FStyleIndex;
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
const
  TextTypeClasses: array [TO2TextType] of string = (
    'notes-plaintext', 'notes-commonmark');
var
  AObject: TO2Object;
begin
    FBuilder.Append('<div class="object-list">');

    for AObject in FSelectedObjects do
    begin
      FBuilder.AppendFormat('<div id="%d" class="object-item"><h2>',
        [AObject.Index]).AppendHTML(AObject.Name).Append('</h2>');

      if FIncludeTags then AppendTagList(AObject);

      AppendFieldList(AObject);

      if FIncludeRelations then AppendRelationList(AObject);

      if FIncludeNotes and (AObject.Text.Count > 0) then
        FBuilder.Append('<div class="notes ')
          .Append(TextTypeClasses[AObject.TextType]).Append('">')
          .AppendHTML(AObject.Text, AObject.TextType).Append('</div>');

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
                [GetHyperLinkAddress(AField, ARule)])
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
  AObjRelations := FO2File.GetObjectRelations(Obj);
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

          if ObjectExists(FSelectedObjects, AObjRelation.Obj) then
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
