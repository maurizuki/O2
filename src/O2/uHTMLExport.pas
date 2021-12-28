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
    HTMLPage: TJvStrHolder;
    HTMLObject: TJvStrHolder;
    ExportDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
    procedure ExportFileExecute(Sender: TObject);
    procedure OptionExecute(Sender: TObject);
  private
    FFileName: string;
    FTitle: string;
    FO2File: TO2File;
    FObjects: TO2ObjectList;
  protected
    function ObjectToHTML(const AObject: TO2Object;
      const O2File: TO2File): string;
    function ExportToHTML: string;
    procedure RefreshPreview;
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
  JclFileUtils, uGlobal, uAppFiles, uStuffHTML;

{$R *.dfm}

const
  HTMLIndexItem = '<div class="O2IndexItem"><a href="#i%d">%s</a></div>'#13#10;
  HTMLObjectField = '<tr><td class="O2FieldName">%s</td><td class="O2FieldValue" colspan="2">%s</td></tr>'#13#10;
  HTMLObjectRelation = '<tr><td class="O2RelationObject" colspan="2"><a href="#i%d">%s</a></td><td class="O2RelationRole">%s</td></tr>'#13#10;
  HTMLObjectText = '<tr><td class="O2ObjectNotes" colspan="3">%s</td></tr>'#13#10;

function EncodeHTML(const S: string): string;
begin
  Result := StringReplace(S, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
end;

function TextToHTML(const Text: TStrings): string;
var
  S: string;
begin
  Result := '';
  for S in Text do
    Result := Result + EncodeHTML(S) + '<br>';
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

function THTMLExport.ObjectToHTML(const AObject: TO2Object;
  const O2File: TO2File): string;
var
  ObjFields, ObjRelations, FieldValue: string;
  AObjRelations: TO2ObjRelations;
  AObjRelation: TO2ObjRelation;
  AField: TO2Field;
  ARule: TO2Rule;
begin
  ObjFields := '';
  for AField in AObject.Fields do
  begin
    if IncludePasswords.Checked
      or not Assigned(O2File.Rules.FindFirstRule(AField, [rtPassword])) then
    begin
      ARule := O2File.Rules.FindFirstRule(AField, [rtHyperLink, rtEmail]);
      if Assigned(ARule) then
        case ARule.RuleType of
          rtHyperLink:
            FieldValue := Format('<a href="%s">%s</a>',
              [ARule.GetHyperLink(AField), EncodeHTML(AField.FieldValue)]);
          rtEmail:
            FieldValue := Format('<a href="mailto:%s">%s</a>',
              [AField.FieldValue, EncodeHTML(AField.FieldValue)]);
        end
      else
        FieldValue := EncodeHTML(AField.FieldValue);

      ObjFields := ObjFields + Format(HTMLObjectField,
        [EncodeHTML(AField.FieldName), FieldValue]);
    end;
  end;

  ObjRelations := '';
  if IncludeRelations.Checked then
  begin
    AObjRelations := O2File.Relations.GetObjectRelations(AObject);
    try
      AObjRelations.SortByObjName;
      for AObjRelation in AObjRelations do
        if Assigned(AObjRelation.Obj) then
          ObjRelations := ObjRelations + Format(HTMLObjectRelation,
            [AObjRelation.Obj.Index, EncodeHTML(AObjRelation.Obj.Name),
            EncodeHTML(EncodeHTML(AObjRelation.Role))]);
    finally
      AObjRelations.Free;
    end;
  end;

  HTMLObject.MacroByName('objIndex').Value := Format('"i%d"', [AObject.Index]);
  HTMLObject.MacroByName('objName').Value := EncodeHTML(AObject.Name);

  if IncludeTags.Checked and (AObject.Tag <> '') then
    HTMLObject.MacroByName('objTags').Value :=
      Format(HTMLObjectField, [STags, EncodeHTML(AObject.Tag)])
  else
    HTMLObject.MacroByName('objTags').Value := '';

  HTMLObject.MacroByName('objFields').Value := ObjFields;
  HTMLObject.MacroByName('objRelations').Value := ObjRelations;

  if IncludeNotes.Checked and (AObject.Text.Count > 0) then
    HTMLObject.MacroByName('objText').Value :=
      Format(HTMLObjectText, [TextToHTML(AObject.Text)])
  else
    HTMLObject.MacroByName('objText').Value := '';

  Result := HTMLObject.ExpandMacros;
end;

function THTMLExport.ExportToHTML: string;
var
  VersionInfo: TJclFileVersionInfo;
  IndexHTML, ObjectsHTML: string;
  AObject: TO2Object;
begin
  IndexHTML := '';
  ObjectsHTML := '';
  for AObject in Objects do
  begin
    if IncludeIndex.Checked then
      IndexHTML := IndexHTML +
        Format(HTMLIndexItem, [AObject.Index, EncodeHTML(AObject.Name)]);
    ObjectsHTML := ObjectsHTML + ObjectToHTML(AObject, O2File);
  end;

  HTMLPage.MacroByName('docTitle').Value := Title;
  HTMLPage.MacroByName('description').Value := '"' + O2File.Description + '"';
  HTMLPage.MacroByName('author').Value := '"' + O2File.Author + '"';
  HTMLPage.MacroByName('docIndex').Value := IndexHTML;
  HTMLPage.MacroByName('objects').Value := ObjectsHTML;

  VersionInfo := TJclFileVersionInfo.Create(AppFiles.FullPath[IdAppExe]);
  try
    HTMLPage.MacroByName('generator').Value := '"' + VersionInfo.ProductName
      + ' ' + VersionInfo.BinFileVersion + '"';
  finally
    VersionInfo.Free;
  end;

  Result := HTMLPage.ExpandMacros;
end;

procedure THTMLExport.RefreshPreview;
begin
  StuffHTML(WebBrowser.DefaultInterface, ExportToHTML);
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
  HTML: TStrings;
begin
  ExportDialog.FileName := '';
  if ExportDialog.Execute then
  begin
    HTML := TStringList.Create;
    try
      HTML.Text := ExportToHTML;
      HTML.SaveToFile(ExportDialog.FileName, TEncoding.UTF8);
    finally
      HTML.Free;
    end;
  end;
end;

procedure THTMLExport.OptionExecute(Sender: TObject);
begin
  TAction(Sender).Checked := not TAction(Sender).Checked;
  RefreshPreview;
end;

end.
