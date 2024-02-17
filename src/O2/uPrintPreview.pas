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

unit uPrintPreview;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvComponentBase, JvExControls, JvPrvwRender, JvPrvwDoc, ToolWin,
  ComCtrls, ActnList, ImgList, Menus, System.ImageList, System.Actions, uPrint;

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
    procedure ActionUpdate(Sender: TObject);
    procedure PrintFileExecute(Sender: TObject);
    procedure OptionExecute(Sender: TObject);
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
    FPrintDocument: TPrintDocument;
    procedure SetPrintDocument(const Value: TPrintDocument);
    procedure SetZoomCaption(Action: TCustomAction);
    procedure RefreshPreview;
  public
    class procedure Execute(AOwner: TComponent; PrintDocument: TPrintDocument);
    property PrintDocument: TPrintDocument read FPrintDocument
      write SetPrintDocument;
  end;

var
  PrintPreview: TPrintPreview;

implementation

uses
  Printers, Math, uGlobal;

{$R *.dfm}

{ TPrintPreview }

class procedure TPrintPreview.Execute(AOwner: TComponent;
  PrintDocument: TPrintDocument);
var
  Form: TPrintPreview;
begin
  Form := TPrintPreview.Create(AOwner);
  try
    Form.PrintDocument := PrintDocument;
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

procedure TPrintPreview.SetPrintDocument(const Value: TPrintDocument);
begin
  if FPrintDocument <> Value then
  begin
    FPrintDocument := Value;

    IncludeTags.Checked := poIncludeTags in FPrintDocument.Options;
    IncludeNotes.Checked := poIncludeNotes in FPrintDocument.Options;
    IncludeRelations.Checked := poIncludeRelations in FPrintDocument.Options;
    IncludePasswords.Checked := poIncludePasswords in FPrintDocument.Options;

    with PreviewControl.DeviceInfo do
    begin
      ReferenceHandle := Printer.Handle;
      OffsetLeft := Max(MMToXPx(10), OffsetLeft);
      OffsetRight := Max(MMToXPx(10), OffsetRight);
      OffsetTop := Max(MMToYPx(10), OffsetTop);
      OffsetBottom := Max(MMToYPx(10), OffsetBottom);
    end;

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
  FPrintDocument.Reset;
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
end;

procedure TPrintPreview.ActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
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
    PreviewPrinter.Title := FPrintDocument.Title;
    PreviewPrinter.Print;
  end;
end;

procedure TPrintPreview.OptionExecute(Sender: TObject);
var
  Options: TPrintOptions;
begin
  TAction(Sender).Checked := not TAction(Sender).Checked;

  Options := [];
  if IncludeTags.Checked then Include(Options, poIncludeTags);
  if IncludeNotes.Checked then Include(Options, poIncludeNotes);
  if IncludeRelations.Checked then Include(Options, poIncludeRelations);
  if IncludePasswords.Checked then Include(Options, poIncludePasswords);
  FPrintDocument.Options := Options;

  RefreshPreview;
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
  NeedMorePages := FPrintDocument.DrawNextPage(Canvas, PageRect, PrintRect,
    PageIndex);
end;

procedure TPrintPreview.PreviewControlChange(Sender: TObject);
begin
  StatusBar.SimpleText := Format(SStatusPagesCount, [PreviewControl.PageCount]);
end;

end.
