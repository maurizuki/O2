{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2022 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uHTMLExport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, Menus, ImgList, ActnList, OleCtrls, SHDocVw,
  JvStringHolder, uO2File, uO2Objects, uO2Rules, uO2Relations, System.ImageList,
  System.Actions;

type
  TStringBuilderHelper = class helper for TStringBuilder
  private
    class function EncodeHTML(const S: string): string;
  public
    function AppendHTML(const S: string): TStringBuilder; overload;
    function AppendHTML(const Lines: TStrings): TStringBuilder; overload;
  end;

  TExportToHTMLOption = (xoIncludeIndex, xoIncludeTags, xoIncludeNotes,
    xoIncludeRelations, xoIncludePasswords);
  TExportToHTMLOptions = set of TExportToHTMLOption;

  THTMLExport = class(TForm)
    ActionList: TActionList;
    ExportFile: TAction;
    ExportOptions: TAction;
    IncludeIndex: TAction;
    IncludeTags: TAction;
    IncludeNotes: TAction;
    IncludeRelations: TAction;
    IncludePasswords: TAction;
    ToolBarImages: TImageList;
    ToolBarImagesH: TImageList;
    ToolBarImagesD: TImageList;
    OptionsMenu: TPopupMenu;
    Includeindex1: TMenuItem;
    N1: TMenuItem;
    Includetags1: TMenuItem;
    Includenotes1: TMenuItem;
    Includerelations1: TMenuItem;
    N2: TMenuItem;
    Includepasswords1: TMenuItem;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    WebBrowser: TWebBrowser;
    DefaultStyle: TJvStrHolder;
    ExportDialog: TSaveDialog;
    ToolButton3: TToolButton;
    StyleMenu: TPopupMenu;
    HTMLStyle: TAction;
    BlueWaterStyle: TAction;
    MatchaStyle: TAction;
    SakuraStyle: TAction;
    BlueWater1: TMenuItem;
    Matcha1: TMenuItem;
    Sakura1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
    procedure ExportFileExecute(Sender: TObject);
    procedure OptionExecute(Sender: TObject);
    procedure StyleExecute(Sender: TObject);
    procedure StyleUpdate(Sender: TObject);
    procedure ExportDialogCanClose(Sender: TObject; var CanClose: Boolean);
  private
    FFileName: string;
    FTitle: string;
    FO2File: TO2File;
    FObjects: TO2ObjectList;
    FStyleIndex: Integer;
    procedure SetStyleIndex(const Value: Integer);
    procedure SetStyleCaption(Action: TCustomAction);
  protected
    procedure AppendObjectIndex(const SB: TStringBuilder);
    procedure AppendObjectList(const SB: TStringBuilder);
    procedure AppendTagList(const SB: TStringBuilder; const Obj: TO2Object);
    procedure AppendFieldList(const SB: TStringBuilder; const Obj: TO2Object);
    procedure AppendRelationList(const SB: TStringBuilder;
      const Obj: TO2Object);
    function ExportToHTML: string;
    procedure RefreshPreview;
    property StyleIndex: Integer read FStyleIndex write SetStyleIndex;
  public
    class procedure IncludeOption(var Options: TExportToHTMLOptions;
      AOption: TExportToHTMLOption; Include: Boolean);
    class procedure Execute(AOwner: TComponent; const FileName: string;
      const O2File: TO2File; const Selection: TO2ObjectList;
      var Options: TExportToHTMLOptions);
    property FileName: string read FFileName write FFileName;
    property Title: string read FTitle write FTitle;
    property O2File: TO2File read FO2File write FO2File;
    property Objects: TO2ObjectList read FObjects write FObjects;
  end;

var
  HTMLExport: THTMLExport;

implementation

uses
  System.Generics.Collections, JclFileUtils, uGlobal, uAppFiles, uStuffHTML,
  uUtils;

{$R *.dfm}

type
  TDefaultStylePalette = record
    LinkColor: string;
    BorderColor: string;
    AltBgColor: string;
  end;

const
  Palettes: array [0..2] of TDefaultStylePalette = (
    ( LinkColor: '#0d6efd'; BorderColor: '#9ec5fe'; AltBgColor: '#f4f8ff' ),
    ( LinkColor: '#a1952e'; BorderColor: '#d5e3c0'; AltBgColor: '#f1f6ea' ),
    ( LinkColor: '#c3829e'; BorderColor: '#fcc9b9'; AltBgColor: '#fff5f2' )
  );

{ TStringBuilderHelper }

class function TStringBuilderHelper.EncodeHTML(const S: string): string;
begin
  Result := StringReplace(S, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
end;

function TStringBuilderHelper.AppendHTML(const S: string): TStringBuilder;
begin
  Result := Self.Append(EncodeHTML(S));
end;

function TStringBuilderHelper.AppendHTML(const Lines: TStrings): TStringBuilder;
var
  S: string;
begin
  Self.Append('<pre>');
  for S in Lines do Self.Append(EncodeHTML(S)).Append('<br>');
  Result := Self.Append('</pre>');
end;

{ THTMLExport }

class procedure THTMLExport.IncludeOption(var Options: TExportToHTMLOptions;
  AOption: TExportToHTMLOption; Include: Boolean);
begin
  if Include then
    System.Include(Options, AOption)
  else
    System.Exclude(Options, AOption);
end;

class procedure THTMLExport.Execute(AOwner: TComponent; const FileName: string;
  const O2File: TO2File; const Selection: TO2ObjectList;
  var Options: TExportToHTMLOptions);
var
  Form: THTMLExport;
begin
  Form := THTMLExport.Create(AOwner);
  try
    Form.FileName := FileName;
    if O2File.Title = '' then
      Form.Title := ChangeFileExt(ExtractFileName(FileName), '')
    else
      Form.Title := O2File.Title;
    Form.O2File := O2File;
    Form.Objects := Selection;

    Form.IncludeIndex.Checked := xoIncludeIndex in Options;
    Form.IncludeTags.Checked := xoIncludeTags in Options;
    Form.IncludeNotes.Checked := xoIncludeNotes in Options;
    Form.IncludeRelations.Checked := xoIncludeRelations in Options;
    Form.IncludePasswords.Checked := xoIncludePasswords in Options;

    Form.RefreshPreview;
    Form.BlueWaterStyle.Execute;
    Form.ShowModal;

    THTMLExport.IncludeOption(Options, xoIncludeIndex,
      Form.IncludeIndex.Checked);
    THTMLExport.IncludeOption(Options, xoIncludeTags,
      Form.IncludeTags.Checked);
    THTMLExport.IncludeOption(Options, xoIncludeNotes,
      Form.IncludeNotes.Checked);
    THTMLExport.IncludeOption(Options, xoIncludeRelations,
      Form.IncludeRelations.Checked);
    THTMLExport.IncludeOption(Options, xoIncludePasswords,
      Form.IncludePasswords.Checked);
  finally
    Form.Free;
  end;
end;

procedure THTMLExport.RefreshPreview;
begin
  StuffHTML(WebBrowser.DefaultInterface, ExportToHTML);
end;

procedure THTMLExport.SetStyleIndex(const Value: Integer);
begin
  if FStyleIndex <> Value then
  begin
    FStyleIndex := Value;
    RefreshPreview;
  end;
end;

procedure THTMLExport.SetStyleCaption(Action: TCustomAction);
begin
  HTMLStyle.Caption := Format('%s (%s)', [SHTMLExportStyle, Action.Caption]);
end;

procedure THTMLExport.AppendObjectIndex(const SB: TStringBuilder);
var
  I: Integer;
begin
  SB.Append('<div class="object-index"><ul>');

  for I := 0 to Objects.Count - 1 do
  begin
    if I mod 2 = 0 then
      SB.Append('<li>')
    else
      SB.Append('<li class="alt-bg">');

    SB.AppendFormat('<a href="#%d">', [Objects[I].Index])
      .AppendHTML(Objects[I].Name).Append('</a>');

    SB.Append('</li>');
  end;

  SB.Append('</ul></div>');
end;

procedure THTMLExport.AppendObjectList(const SB: TStringBuilder);
var
  I: Integer;
begin
    SB.Append('<div class="object-list">');

    for I := 0 to Objects.Count - 1 do
    begin
      SB.AppendFormat('<a name="%d"></a>', [Objects[I].Index])
        .Append('<div class="object-item"><h2>')
        .AppendHTML(Objects[I].Name).Append('</h2>');

      if IncludeTags.Checked then AppendTagList(SB, Objects[I]);

      AppendFieldList(SB, Objects[I]);

      if IncludeRelations.Checked then AppendRelationList(SB, Objects[I]);

      if IncludeNotes.Checked and (Objects[I].Text.Count > 0) then
        SB.Append('<div class="notes">')
          .AppendHTML(Objects[I].Text).Append('</div>');

      SB.Append('</div>');
    end;

    SB.Append('</div>');
end;

procedure THTMLExport.AppendTagList(const SB: TStringBuilder;
  const Obj: TO2Object);
var
  Tags: TStringList;
  ATag: string;
begin
  Tags := TStringList.Create;
  try
    Obj.GetTags(Tags);

    if Tags.Count > 0 then
    begin
      SB.Append('<div class="tag-list">');

      for ATag in Tags do
        SB.Append('<div class="tag-item">')
          .AppendHTML(ATag).Append('</div>');

      SB.Append('</div>');
    end;
  finally
    Tags.Free;
  end;
end;

procedure THTMLExport.AppendFieldList(const SB: TStringBuilder;
  const Obj: TO2Object);
var
  Fields: TList<TO2Field>;
  AField: TO2Field;
  ARule: TO2Rule;
  I: Integer;
begin
  Fields := TList<TO2Field>.Create;
  try
    for AField in Obj.Fields do
      if IncludePasswords.Checked
        or not Assigned(O2File.Rules.FindFirstRule(AField, [rtPassword])) then
        Fields.Add(AField);

    if Fields.Count > 0 then
    begin
      SB.Append('<div class="field-list">');

      for I := 0 to Fields.Count - 1 do
      begin
        if I mod 2 = 0 then
          SB.Append('<div class="field-item">')
        else
          SB.Append('<div class="field-item alt-bg">');

        SB.Append('<div class="field-name">')
          .AppendHTML(Fields[I].FieldName).Append('</div>');

        SB.Append('<div class="field-value">');
        ARule := O2File.Rules.FindFirstRule(Fields[I], [rtHyperLink, rtEmail]);
        if Assigned(ARule) then
          case ARule.RuleType of
            rtHyperLink:
              SB.AppendFormat('<a href="%s">', [ARule.GetHyperLink(Fields[I])])
                .AppendHTML(Fields[I].FieldValue).Append('</a>');
            rtEmail:
              SB.AppendFormat('<a href="mailto:%s">', [Fields[I].FieldValue])
                .AppendHTML(Fields[I].FieldValue).Append('</a>');
          end
        else
          SB.AppendHTML(Fields[I].FieldValue);
        SB.Append('</div>');

        SB.Append('</div>');
      end;

      SB.Append('</div>');
    end;
  finally
    Fields.Free;
  end;
end;

procedure THTMLExport.AppendRelationList(const SB: TStringBuilder;
  const Obj: TO2Object);
var
  AObjRelations: TO2ObjRelations;
  I: Integer;
begin
  AObjRelations := O2File.Relations.GetObjectRelations(Obj);
  try
    if AObjRelations.Count > 0 then
    begin
      SB.Append('<div class="relation-list">');

      AObjRelations.SortByObjName;
      for I := 0 to AObjRelations.Count - 1 do
        if Assigned(AObjRelations[I].Obj) then
        begin
          if I mod 2 = 0 then
            SB.Append('<div class="relation-item">')
          else
            SB.Append('<div class="relation-item alt-bg">');

          SB.Append('<div class="relation-object">')
            .AppendFormat('<a href="#%d">', [AObjRelations[I].Obj.Index])
            .AppendHTML(AObjRelations[I].Obj.Name).Append('</a></div>');

          SB.Append('<div class="relation-role">')
            .AppendHTML(AObjRelations[I].Role).Append('</div>');

          SB.Append('</div>');
        end;

      SB.Append('</div>');
    end;
  finally
    AObjRelations.Free;
  end;
end;

function THTMLExport.ExportToHTML: string;
var
  VersionInfo: TJclFileVersionInfo;
  SB: TStringBuilder;
begin
  DefaultStyle.MacroByName('link-color').Value :=
    Palettes[StyleIndex].LinkColor;
  DefaultStyle.MacroByName('border-color').Value :=
    Palettes[StyleIndex].BorderColor;
  DefaultStyle.MacroByName('alt-bg-color').Value :=
    Palettes[StyleIndex].AltBgColor;

  VersionInfo := TJclFileVersionInfo.Create(AppFiles.FullPath[IdAppExe]);
  try
    SB := TStringBuilder.Create;
    try
      SB.AppendLine('<!DOCTYPE html>')
        .AppendLine('<html>')
        .AppendLine('<head>')
        .AppendFormat('<meta name="generator" content="%s %s">',
          [VersionInfo.ProductName, VersionInfo.BinFileVersion])
        .AppendFormat('<meta name="description" content="%s">',
          [O2File.Description])
        .AppendFormat('<meta name="author" content="%s">', [O2File.Author])
        .AppendLine('<style>')
        .AppendLine(DefaultStyle.ExpandMacros)
        .AppendLine('</style>')
        .Append('<title>').AppendHTML(Title).AppendLine('</title>')
        .AppendLine('</head>')
        .Append('<body><div class="container">')
        .Append('<h1>').AppendHTML(Title).Append('</h1>');

      if IncludeIndex.Checked then AppendObjectIndex(SB);

      AppendObjectList(SB);

      SB.AppendLine('</div></body>').Append('</html>');

      Result := SB.ToString;
    finally
      SB.Free;
    end;
  finally
    VersionInfo.Free;
  end;
end;

procedure THTMLExport.FormCreate(Sender: TObject);
var
  WorkArea: TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @WorkArea, 0);
  SetBounds(WorkArea.Left, WorkArea.Top,
    WorkArea.Right - WorkArea.Left, WorkArea.Bottom - WorkArea.Top);
end;

procedure THTMLExport.ActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure THTMLExport.ExportFileExecute(Sender: TObject);
var
  Writer: TTextWriter;
begin
  ExportDialog.FileName := '';
  if ExportDialog.Execute then
  begin
    Writer := TStreamWriter.Create(ExportDialog.FileName);
    try
      Writer.Write(ExportToHTML);
    finally
      Writer.Free;
    end;
  end;
end;

procedure THTMLExport.OptionExecute(Sender: TObject);
begin
  TAction(Sender).Checked := not TAction(Sender).Checked;
  RefreshPreview;
end;

procedure THTMLExport.StyleExecute(Sender: TObject);
begin
  StyleIndex := TComponent(Sender).Tag;
  SetStyleCaption(TCustomAction(Sender));
end;

procedure THTMLExport.StyleUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := StyleIndex = TComponent(Sender).Tag;
end;

procedure THTMLExport.ExportDialogCanClose(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := not FileExists(ExportDialog.FileName)
    or YesNoBox(SFileOverwriteQuery);
end;

end.
