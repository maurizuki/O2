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

unit uHTMLExport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, Menus, ImgList, ActnList, OleCtrls, SHDocVw,
  System.ImageList, System.Actions, JvStringHolder, uO2File, uHTMLExportModel;

type
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
    procedure FormDestroy(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
    procedure ExportFileExecute(Sender: TObject);
    procedure IncludeIndexExecute(Sender: TObject);
    procedure IncludeIndexUpdate(Sender: TObject);
    procedure IncludeTagsExecute(Sender: TObject);
    procedure IncludeTagsUpdate(Sender: TObject);
    procedure IncludeNotesExecute(Sender: TObject);
    procedure IncludeNotesUpdate(Sender: TObject);
    procedure IncludeRelationsExecute(Sender: TObject);
    procedure IncludeRelationsUpdate(Sender: TObject);
    procedure IncludePasswordsExecute(Sender: TObject);
    procedure IncludePasswordsUpdate(Sender: TObject);
    procedure StyleExecute(Sender: TObject);
    procedure StyleUpdate(Sender: TObject);
    procedure ExportDialogCanClose(Sender: TObject; var CanClose: Boolean);
  private
    FModel: THTMLExportModel;
    OriginalApplicationMessage: TMessageEvent;
    procedure ApplicationMessage(var Msg: TMsg; var Handled: Boolean);
    procedure SetModel(const Value: THTMLExportModel);
    procedure SetStyleCaption(Action: TCustomAction);
    procedure RefreshPreview;
  public
    class procedure Execute(AOwner: TComponent; Model: THTMLExportModel);
    property Model: THTMLExportModel read FModel write SetModel;
  end;

var
  HTMLExport: THTMLExport;

implementation

uses
  uGlobal, uStuffHTML, uUtils;

{$R *.dfm}

{ THTMLExport }

class procedure THTMLExport.Execute(AOwner: TComponent;
  Model: THTMLExportModel);
var
  Form: THTMLExport;
begin
  Form := THTMLExport.Create(AOwner);
  try
    Form.Model := Model;
    Form.ShowModal;
    Model.StoreSettings;
  finally
    Form.Free;
  end;
end;

procedure THTMLExport.ApplicationMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if ((Msg.Message = WM_RBUTTONDOWN) or (Msg.Message = WM_RBUTTONDBLCLK))
    and IsChild(WebBrowser.Handle, Msg.hwnd) then
    Handled := True
  else
    OriginalApplicationMessage(Msg, Handled);
end;

procedure THTMLExport.SetModel(const Value: THTMLExportModel);
begin
  if FModel <> Value then
  begin
    FModel := Value;

    DefaultStyle.MacroByName('link-color').Value := '#0d6efd';
    DefaultStyle.MacroByName('border-color').Value := '#9ec5fe';
    DefaultStyle.MacroByName('alt-bg-color').Value := '#f4f8ff';
    BlueWaterStyle.Tag := FModel.AddStyle(DefaultStyle.ExpandMacros);

    DefaultStyle.MacroByName('link-color').Value := '#a1952e';
    DefaultStyle.MacroByName('border-color').Value := '#d5e3c0';
    DefaultStyle.MacroByName('alt-bg-color').Value := '#f1f6ea';
    MatchaStyle.Tag := FModel.AddStyle(DefaultStyle.ExpandMacros);

    DefaultStyle.MacroByName('link-color').Value := '#c3829e';
    DefaultStyle.MacroByName('border-color').Value := '#fcc9b9';
    DefaultStyle.MacroByName('alt-bg-color').Value := '#fff5f2';
    SakuraStyle.Tag := FModel.AddStyle(DefaultStyle.ExpandMacros);

    BlueWaterStyle.Execute;
  end;
end;

procedure THTMLExport.SetStyleCaption(Action: TCustomAction);
begin
  HTMLStyle.Caption := Format('%s (%s)', [SHTMLExportStyle, Action.Caption]);
end;

procedure THTMLExport.RefreshPreview;
begin
  StuffHTML(WebBrowser.DefaultInterface, FModel.ExportToHTML);
end;

procedure THTMLExport.FormCreate(Sender: TObject);
var
  WorkArea: TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @WorkArea, 0);
  SetBounds(WorkArea.Left, WorkArea.Top,
    WorkArea.Right - WorkArea.Left, WorkArea.Bottom - WorkArea.Top);

  OriginalApplicationMessage := Application.OnMessage;
  Application.OnMessage := ApplicationMessage;
end;

procedure THTMLExport.FormDestroy(Sender: TObject);
begin
  Application.OnMessage := OriginalApplicationMessage;
end;

procedure THTMLExport.ActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure THTMLExport.ExportFileExecute(Sender: TObject);
begin
  ExportDialog.FileName := '';
  if ExportDialog.Execute then FModel.ExportToHTML(ExportDialog.FileName);
end;

procedure THTMLExport.IncludeIndexExecute(Sender: TObject);
begin
  FModel.IncludeIndex := not FModel.IncludeIndex;
  RefreshPreview;
end;

procedure THTMLExport.IncludeIndexUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FModel.IncludeIndex;
end;

procedure THTMLExport.IncludeTagsExecute(Sender: TObject);
begin
  FModel.IncludeTags := not FModel.IncludeTags;
  RefreshPreview;
end;

procedure THTMLExport.IncludeTagsUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FModel.IncludeTags;
end;

procedure THTMLExport.IncludeNotesExecute(Sender: TObject);
begin
  FModel.IncludeNotes := not FModel.IncludeNotes;
  RefreshPreview;
end;

procedure THTMLExport.IncludeNotesUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FModel.IncludeNotes;
end;

procedure THTMLExport.IncludeRelationsExecute(Sender: TObject);
begin
  FModel.IncludeRelations := not FModel.IncludeRelations;
  RefreshPreview;
end;

procedure THTMLExport.IncludeRelationsUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FModel.IncludeRelations;
end;

procedure THTMLExport.IncludePasswordsExecute(Sender: TObject);
begin
  FModel.IncludePasswords := not FModel.IncludePasswords;
  RefreshPreview;
end;

procedure THTMLExport.IncludePasswordsUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FModel.IncludePasswords;
end;

procedure THTMLExport.StyleExecute(Sender: TObject);
begin
  FModel.StyleIndex := TComponent(Sender).Tag;
  SetStyleCaption(TCustomAction(Sender));
  RefreshPreview;
end;

procedure THTMLExport.StyleUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FModel.StyleIndex = TComponent(Sender).Tag;
end;

procedure THTMLExport.ExportDialogCanClose(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := not FileExists(ExportDialog.FileName)
    or YesNoBox(SFileOverwriteQuery);
end;

end.
