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

unit uPrintPreview;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvComponentBase, JvExControls, JvPrvwRender, JvPrvwDoc, ToolWin,
  ComCtrls, ActnList, ImgList, Menus, System.ImageList, System.Actions,
  uServices;

type
  TPrintPreview = class(TForm)
    PreviewControl: TJvPreviewControl;
    ToolBar: TToolBar;
    ActionList: TActionList;
    ToolBarImages: TImageList;
    PrintFile: TAction;
    ToolButton1: TToolButton;
    ToolBarImagesH: TImageList;
    ToolBarImagesD: TImageList;
    ToolButton2: TToolButton;
    PreviewZoom: TAction;
    PrintDialog: TPrintDialog;
    PreviewPrinter: TJvPreviewPrinter;
    ZoomWholePage: TAction;
    ZoomPageWidth: TAction;
    ZoomMenu: TPopupMenu;
    Print1: TMenuItem;
    Pagewidth1: TMenuItem;
    Zoom25: TAction;
    N251: TMenuItem;
    Zoom50: TAction;
    Zoom75: TAction;
    Zoom100: TAction;
    Zoom150: TAction;
    Zoom200: TAction;
    Zoom400: TAction;
    N501: TMenuItem;
    N751: TMenuItem;
    N1001: TMenuItem;
    N1501: TMenuItem;
    N2001: TMenuItem;
    N4001: TMenuItem;
    N1: TMenuItem;
    StatusBar: TStatusBar;
    PrintOptions: TAction;
    ToolButton3: TToolButton;
    OptionsMenu: TPopupMenu;
    IncludeTags: TAction;
    IncludeNotes: TAction;
    IncludeRelations: TAction;
    IncludePasswords: TAction;
    Includetags1: TMenuItem;
    Includenotes1: TMenuItem;
    Includerelations1: TMenuItem;
    N2: TMenuItem;
    Includepasswords1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
    procedure PrintFileExecute(Sender: TObject);
    procedure IncludeTagsExecute(Sender: TObject);
    procedure IncludeTagsUpdate(Sender: TObject);
    procedure IncludeNotesExecute(Sender: TObject);
    procedure IncludeNotesUpdate(Sender: TObject);
    procedure IncludeRelationsExecute(Sender: TObject);
    procedure IncludeRelationsUpdate(Sender: TObject);
    procedure IncludePasswordsExecute(Sender: TObject);
    procedure IncludePasswordsUpdate(Sender: TObject);
    procedure ZoomWholePageExecute(Sender: TObject);
    procedure ZoomWholePageUpdate(Sender: TObject);
    procedure ZoomPageWidthExecute(Sender: TObject);
    procedure ZoomPageWidthUpdate(Sender: TObject);
    procedure ZoomExecute(Sender: TObject);
    procedure ZoomUpdate(Sender: TObject);
    procedure PreviewControlAddPage(Sender: TObject; PageIndex: Integer;
      Canvas: TCanvas; PageRect, PrintRect: TRect; var NeedMorePages: Boolean);
    procedure PreviewControlChange(Sender: TObject);
  private
    FModel: IPrint;
    procedure SetModel(Value: IPrint);
    procedure SetZoomCaption(Action: TCustomAction);
    procedure RefreshPreview;
  public
    class procedure Execute(Model: IPrint);
    property Model: IPrint read FModel write SetModel;
  end;

var
  PrintPreview: TPrintPreview;

implementation

uses
  Printers, Math, uGlobal;

{$R *.dfm}

{ TPrintPreview }

class procedure TPrintPreview.Execute(Model: IPrint);
var
  Form: TPrintPreview;
begin
  Form := TPrintPreview.Create(Application);
  try
    Form.Model := Model;
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

procedure TPrintPreview.SetModel(Value: IPrint);
begin
  if FModel <> Value then
  begin
    FModel := Value;
    RefreshPreview;
    ZoomWholePage.Execute;
  end;
end;

procedure TPrintPreview.SetZoomCaption(Action: TCustomAction);
begin
  PreviewZoom.Caption := Format('%s (%s)', [SPrintPreviewZoom, Action.Caption]);
end;

procedure TPrintPreview.RefreshPreview;
begin
  PreviewControl.Clear;
  PreviewControl.Add;
end;

procedure TPrintPreview.FormCreate(Sender: TObject);
var
  WorkArea: TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @WorkArea, 0);
  SetBounds(WorkArea.Left, WorkArea.Top,
    WorkArea.Right - WorkArea.Left, WorkArea.Bottom - WorkArea.Top);

  with PreviewControl.DeviceInfo do
  begin
    ReferenceHandle := Printer.Handle;
    OffsetLeft := Max(MMToXPx(10), OffsetLeft);
    OffsetRight := Max(MMToXPx(10), OffsetRight);
    OffsetTop := Max(MMToYPx(10), OffsetTop);
    OffsetBottom := Max(MMToYPx(10), OffsetBottom);
  end;
end;

procedure TPrintPreview.FormDestroy(Sender: TObject);
begin
  FModel.StoreSettings;
end;

procedure TPrintPreview.PrintFileExecute(Sender: TObject);
begin
  PrintDialog.PrintRange := prAllPages;
  if PreviewControl.PageCount < 2 then
    PrintDialog.Options := PrintDialog.Options - [poPageNums]
  else
  begin
    PrintDialog.Options := PrintDialog.Options + [poPageNums];

    PrintDialog.FromPage := 1;
    PrintDialog.MinPage := PrintDialog.FromPage;

    PrintDialog.ToPage := PreviewControl.PageCount;
    PrintDialog.MaxPage := PrintDialog.ToPage;
  end;
  if PrintDialog.Execute then
  begin
    PreviewPrinter.Assign(PrintDialog);
    PreviewPrinter.Printer := Printer;
    PreviewPrinter.Title := FModel.Title;
    PreviewPrinter.Print;
  end;
end;

procedure TPrintPreview.IncludeTagsExecute(Sender: TObject);
begin
  FModel.IncludeTags := not FModel.IncludeTags;
  RefreshPreview;
end;

procedure TPrintPreview.IncludeTagsUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FModel.IncludeTags;
end;

procedure TPrintPreview.IncludeNotesExecute(Sender: TObject);
begin
  FModel.IncludeNotes := not FModel.IncludeNotes;
  RefreshPreview;
end;

procedure TPrintPreview.IncludeNotesUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FModel.IncludeNotes;
end;

procedure TPrintPreview.IncludeRelationsExecute(Sender: TObject);
begin
  FModel.IncludeRelations := not FModel.IncludeRelations;
  RefreshPreview;
end;

procedure TPrintPreview.IncludeRelationsUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FModel.IncludeRelations;
end;

procedure TPrintPreview.IncludePasswordsExecute(Sender: TObject);
begin
  FModel.IncludePasswords := not FModel.IncludePasswords;
  RefreshPreview;
end;

procedure TPrintPreview.IncludePasswordsUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FModel.IncludePasswords;
end;

procedure TPrintPreview.ActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure TPrintPreview.ZoomWholePageExecute(Sender: TObject);
begin
  PreviewControl.Options.ScaleMode := smFullPage;
  SetZoomCaption(TCustomControlAction(Sender));
end;

procedure TPrintPreview.ZoomWholePageUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := PreviewControl.Options.ScaleMode = smFullPage;
end;

procedure TPrintPreview.ZoomPageWidthExecute(Sender: TObject);
begin
  PreviewControl.Options.ScaleMode := smPageWidth;
  SetZoomCaption(TCustomControlAction(Sender));
end;

procedure TPrintPreview.ZoomPageWidthUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := PreviewControl.Options.ScaleMode = smPageWidth;
end;

procedure TPrintPreview.ZoomExecute(Sender: TObject);
begin
  PreviewControl.Options.ScaleMode := smScale;
  PreviewControl.Options.Scale := TAction(Sender).Tag;
  SetZoomCaption(TCustomControlAction(Sender));
end;

procedure TPrintPreview.ZoomUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := (PreviewControl.Options.ScaleMode = smScale)
    and (TAction(Sender).Tag = Integer(PreviewControl.Options.Scale));
end;

procedure TPrintPreview.PreviewControlAddPage(Sender: TObject;
  PageIndex: Integer; Canvas: TCanvas; PageRect, PrintRect: TRect;
  var NeedMorePages: Boolean);
begin
  NeedMorePages := FModel.DrawNextPage(Canvas, PageRect, PrintRect,
    PageIndex);
end;

procedure TPrintPreview.PreviewControlChange(Sender: TObject);
begin
  StatusBar.SimpleText := Format(SStatusPagesCount, [PreviewControl.PageCount]);
end;

end.
