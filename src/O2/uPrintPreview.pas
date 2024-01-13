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
  ComCtrls, ActnList, ImgList, Menus, uO2File, uO2Objects, uO2Relations,
  System.ImageList, System.Actions;

type
  TPrintOption = (poIncludeTags, poIncludeNotes, poIncludeRelations,
    poIncludePasswords);
  TPrintOptions = set of TPrintOption;

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
    FFileName: string;
    FTitle: string;
    FO2File: TO2File;
    FObjects: TO2ObjectList;
    ObjRelations: TO2ObjRelations;
    ObjectIndex: Integer;
    FieldIndex: Integer;
    ObjRelationIndex: Integer;
    ItemIndex: Integer;
    procedure SetZoomCaption(Action: TCustomAction);
  protected
    procedure RefreshPreview;
  public
    class procedure IncludeOption(var Options: TPrintOptions;
      AOption: TPrintOption; Include: Boolean);
    class procedure Execute(AOwner: TComponent; const FileName: string;
      const O2File: TO2File; const Selection: TO2ObjectList;
      var Options: TPrintOptions);
    property FileName: string read FFileName write FFileName;
    property Title: string read FTitle write FTitle;
    property O2File: TO2File read FO2File write FO2File;
    property Objects: TO2ObjectList read FObjects write FObjects;
  end;

var
  PrintPreview: TPrintPreview;

implementation

uses
  Printers, Math, uGlobal, uO2Rules;

{$R *.dfm}

{ TPrintPreview }

class procedure TPrintPreview.IncludeOption(var Options: TPrintOptions;
  AOption: TPrintOption; Include: Boolean);
begin
  if Include then
    System.Include(Options, AOption)
  else
    System.Exclude(Options, AOption);
end;

class procedure TPrintPreview.Execute(AOwner: TComponent;
  const FileName: string; const O2File: TO2File;
  const Selection: TO2ObjectList; var Options: TPrintOptions);
var
  Form: TPrintPreview;
begin
  Form := TPrintPreview.Create(AOwner);
  try
    Form.FileName := FileName;
    if O2File.Title = '' then
      Form.Title := ChangeFileExt(ExtractFileName(FileName), '')
    else
      Form.Title := O2File.Title;
    Form.O2File := O2File;
    Form.Objects := Selection;

    Form.IncludeTags.Checked := poIncludeTags in Options;
    Form.IncludeNotes.Checked := poIncludeNotes in Options;
    Form.IncludeRelations.Checked := poIncludeRelations in Options;
    Form.IncludePasswords.Checked := poIncludePasswords in Options;

    with Form.PreviewControl.DeviceInfo do
    begin
      ReferenceHandle := Printer.Handle;
      OffsetLeft := Max(MMToXPx(10), OffsetLeft);
      OffsetRight := Max(MMToXPx(10), OffsetRight);
      OffsetTop := Max(MMToYPx(10), OffsetTop);
      OffsetBottom := Max(MMToYPx(10), OffsetBottom);
    end;

    Form.RefreshPreview;
    Form.ZoomWholePage.Execute;
    Form.ShowModal;

    TPrintPreview.IncludeOption(Options, poIncludeTags,
      Form.IncludeTags.Checked);
    TPrintPreview.IncludeOption(Options, poIncludeNotes,
      Form.IncludeNotes.Checked);
    TPrintPreview.IncludeOption(Options, poIncludeRelations,
      Form.IncludeRelations.Checked);
    TPrintPreview.IncludeOption(Options, poIncludePasswords,
      Form.IncludePasswords.Checked);
  finally
    Form.Free;
  end;
end;

procedure TPrintPreview.RefreshPreview;
begin
  ObjectIndex := 0;
  FieldIndex := 0;
  ObjRelationIndex := 0;
  ItemIndex := 0;

  PreviewControl.Clear;
  PreviewControl.Add;
end;

procedure TPrintPreview.SetZoomCaption(Action: TCustomAction);
begin
  PreviewZoom.Caption := Format('%s (%s)', [SPrintPreviewZoom, Action.Caption]);
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
    PreviewPrinter.Title := Title;
    PreviewPrinter.Print;
  end;
end;

procedure TPrintPreview.OptionExecute(Sender: TObject);
begin
  TAction(Sender).Checked := not TAction(Sender).Checked;
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
var
  AObject: TO2Object;
  AField: TO2Field;
  TextMetric: TTextMetric;
  Rect, Rect2: TRect;
  W, H: Integer;
  S: string;
begin
  NeedMorePages := True;

  with TJvPreviewControl(Sender).DeviceInfo do
  begin
    Canvas.Font.Name := 'Arial';
    Canvas.Font.Size := 8;
    GetTextMetrics(Canvas.Handle, TextMetric);
    W := (PrintRect.Right - PrintRect.Left) div 2;
    H := TextMetric.tmHeight + TextMetric.tmInternalLeading
      + TextMetric.tmExternalLeading;

    S := Title;
    Rect.Left := PrintRect.Left;
    Rect.Top := PrintRect.Top;
    Rect.Right := PrintRect.Left + W;
    Rect.Bottom := PrintRect.Top + H;
    DrawText(Canvas.Handle, PChar(S), Length(S), Rect,
      DT_NOPREFIX or DT_LEFT or DT_TOP);

    S := FileName;
    Rect.Left := PrintRect.Right - W;
    Rect.Right := PrintRect.Right;
    DrawText(Canvas.Handle, PChar(S), Length(S), Rect,
      DT_NOPREFIX or DT_RIGHT or DT_TOP);

    S := DateTimeToStr(Now);
    Rect.Left := PrintRect.Left;
    Rect.Top := PrintRect.Bottom - H;
    Rect.Right := PrintRect.Left + W;
    Rect.Bottom := PrintRect.Bottom;
    DrawText(Canvas.Handle, PChar(S), Length(S), Rect,
      DT_NOPREFIX or DT_LEFT or DT_BOTTOM);

    S := IntToStr(PageIndex + 1);
    Rect.Left := PrintRect.Right - W;
    Rect.Right := PrintRect.Right;
    DrawText(Canvas.Handle, PChar(S), Length(S), Rect,
      DT_NOPREFIX or DT_RIGHT or DT_BOTTOM);

    Inc(PrintRect.Top, H + MMToYPx(5));
    Dec(PrintRect.Bottom, H + MMToYPx(5));

    Rect.Top := PrintRect.Top;

    if PageIndex = 0 then
    begin
      Canvas.Font.Size := 24;
      GetTextMetrics(Canvas.Handle, TextMetric);
      H := TextMetric.tmHeight + TextMetric.tmInternalLeading
        + TextMetric.tmExternalLeading;

      S := Title;
      Rect.Left := PrintRect.Left;
      Rect.Right := PrintRect.Right;
      Rect.Bottom := Rect.Top + H;
      DrawText(Canvas.Handle, PChar(S), Length(S), Rect,
        DT_NOPREFIX or DT_LEFT or DT_TOP);

      Inc(Rect.Top, H + MMToYPx(5));
    end;

    { Objects }

    while ObjectIndex < Objects.Count do
    begin
      Canvas.Font.Name := 'Arial';
      Canvas.Font.Size := 12;
      GetTextMetrics(Canvas.Handle, TextMetric);
      H := TextMetric.tmHeight + TextMetric.tmInternalLeading
        + TextMetric.tmExternalLeading;

      if Rect.Top + H + MMToYPx(3) > PrintRect.Bottom then Exit;

      S := Objects[ObjectIndex].Name;
      W := Canvas.TextWidth(S);
      Rect.Left := PrintRect.Left;
      Rect.Right := PrintRect.Right;
      Rect.Bottom := Rect.Top + H;
      DrawText(Canvas.Handle, PChar(S), Length(S), Rect,
        DT_NOPREFIX or DT_LEFT or DT_TOP);

      Canvas.Pen.Width := 1;
      Canvas.Pen.Style := psSolid;
      Canvas.MoveTo(Rect.Left, Rect.Bottom);
      Canvas.LineTo(Rect.Left + W, Rect.Bottom);

      Inc(Rect.Top, H + MMToYPx(3));

      { Fields }

      Canvas.Font.Name := 'Arial';
      Canvas.Font.Size := 10;
      GetTextMetrics(Canvas.Handle, TextMetric);
      W := Round((PrintRect.Right - PrintRect.Left) * 0.3);
      H := TextMetric.tmHeight + TextMetric.tmInternalLeading
        + TextMetric.tmExternalLeading;

      Rect.Left := PrintRect.Left;
      Rect.Right := PrintRect.Left + W;
      Rect.Bottom := Rect.Top + H;

      Rect2.Top := Rect.Top;
      Rect2.Left := PrintRect.Left + W;
      Rect2.Right := PrintRect.Right;
      Rect2.Bottom := Rect2.Top + H;

      if IncludeTags.Checked and (FieldIndex = 0)
        and (Objects[ObjectIndex].Tag <> '') then
      begin
        if Rect.Top + H > PrintRect.Bottom then Exit;

        S := STags;
        DrawText(Canvas.Handle, PChar(S), Length(S), Rect,
          DT_NOPREFIX or DT_LEFT or DT_TOP);

        S := Objects[ObjectIndex].Tag;
        DrawText(Canvas.Handle, PChar(S), Length(S), Rect2,
          DT_NOPREFIX or DT_LEFT or DT_TOP);

        OffsetRect(Rect, 0, H);
        OffsetRect(Rect2, 0, H);
      end;

      while FieldIndex < Objects[ObjectIndex].Fields.Count do
      begin
        AField := Objects[ObjectIndex].Fields[FieldIndex];

        if IncludePasswords.Checked
          or not Assigned(O2File.Rules.FindFirstRule(AField, [rtPassword])) then
        begin
          if Rect.Top + H > PrintRect.Bottom then Exit;

          S := AField.FieldName;
          DrawText(Canvas.Handle, PChar(S), Length(S), Rect,
            DT_NOPREFIX or DT_LEFT or DT_TOP);

          S := AField.FieldValue;
          DrawText(Canvas.Handle, PChar(S), Length(S), Rect2,
            DT_NOPREFIX or DT_LEFT or DT_TOP);

          OffsetRect(Rect, 0, H);
          OffsetRect(Rect2, 0, H);
        end;

        Inc(FieldIndex);
      end;

      { Relations }

      if IncludeRelations.Checked then
      begin
        if ObjRelations = nil then
        begin
          ObjRelations :=
            O2File.Relations.GetObjectRelations(Objects[ObjectIndex]);
          ObjRelations.SortByObjName;
        end;

        Canvas.Font.Name := 'Arial';
        Canvas.Font.Size := 10;
        GetTextMetrics(Canvas.Handle, TextMetric);
        H := TextMetric.tmHeight + TextMetric.tmInternalLeading
          + TextMetric.tmExternalLeading;

        if (ObjRelationIndex = 0) and (ObjRelations.Count > 0) then
        begin
          Inc(Rect.Top, MMToYPx(1));

          Canvas.Pen.Width := 1;
          Canvas.Pen.Style := psDot;
          Canvas.MoveTo(PrintRect.Left, Rect.Top);
          Canvas.LineTo(PrintRect.Right, Rect.Top);

          Inc(Rect.Top, MMToYPx(1));
        end;

        Rect.Left := PrintRect.Left;
        Rect.Right := PrintRect.Right - W;
        Rect.Bottom := Rect.Top + H;

        Rect2.Top := Rect.Top;
        Rect2.Left := PrintRect.Right - W;
        Rect2.Right := PrintRect.Right;
        Rect2.Bottom := Rect2.Top + H;

        while ObjRelationIndex < ObjRelations.Count do
        begin
          if Rect.Top + H > PrintRect.Bottom then Exit;

          if Assigned(ObjRelations[ObjRelationIndex].Obj) then
            S := ObjRelations[ObjRelationIndex].Obj.Name
          else
            S := '';
          DrawText(Canvas.Handle, PChar(S), Length(S), Rect,
            DT_NOPREFIX or DT_LEFT or DT_TOP);

          S := ObjRelations[ObjRelationIndex].Role;
          DrawText(Canvas.Handle, PChar(S), Length(S), Rect2,
            DT_NOPREFIX or DT_LEFT or DT_TOP);

          OffsetRect(Rect, 0, H);
          OffsetRect(Rect2, 0, H);
          Inc(ObjRelationIndex);
        end;
      end;

      { Text }

      if IncludeNotes.Checked then
      begin
        Canvas.Font.Name := 'Courier New';
        Canvas.Font.Size := 10;
        GetTextMetrics(Canvas.Handle, TextMetric);
        H := TextMetric.tmHeight + TextMetric.tmInternalLeading
          + TextMetric.tmExternalLeading;

        if (ItemIndex = 0) and (Objects[ObjectIndex].Text.Count > 0) then
        begin
          Inc(Rect.Top, MMToYPx(1));

          Canvas.Pen.Width := 1;
          Canvas.Pen.Style := psDot;
          Canvas.MoveTo(PrintRect.Left, Rect.Top);
          Canvas.LineTo(PrintRect.Right, Rect.Top);

          Inc(Rect.Top, MMToYPx(1));
        end;

        Rect.Left := PrintRect.Left;
        Rect.Bottom := Rect.Top + H;

        while ItemIndex < Objects[ObjectIndex].Text.Count do
        begin
          Rect.Right := PrintRect.Right;

          S := Objects[ObjectIndex].Text[ItemIndex];
          H := DrawText(Canvas.Handle, PChar(S), Length(S), Rect,
            DT_CALCRECT or DT_NOPREFIX or DT_EXPANDTABS or DT_WORDBREAK
            or DT_LEFT or DT_TOP);

          if Rect.Right > PrintRect.Right then
          begin
            Rect.Right := PrintRect.Right;
            Insert(#13#10, S, Length(S) div 2);
            H := DrawText(Canvas.Handle, PChar(S), Length(S), Rect,
              DT_CALCRECT or DT_NOPREFIX or DT_EXPANDTABS or DT_WORDBREAK
              or DT_LEFT or DT_TOP);
          end;

          if Rect.Bottom > PrintRect.Bottom then Exit;

          DrawText(Canvas.Handle, PChar(S), Length(S), Rect,
            DT_NOPREFIX or DT_EXPANDTABS or DT_WORDBREAK or DT_LEFT or DT_TOP);

          OffsetRect(Rect, 0, H);
          Inc(ItemIndex);
        end;
      end;

      Inc(Rect.Top, MMToYPx(5));
      Inc(ObjectIndex);

      FieldIndex := 0;
      ObjRelationIndex := 0;
      ItemIndex := 0;

      FreeAndNil(ObjRelations);
    end;
  end;

  NeedMorePages := False;
end;

procedure TPrintPreview.PreviewControlChange(Sender: TObject);
begin
  StatusBar.SimpleText := Format(SStatusPagesCount, [PreviewControl.PageCount]);
end;

end.
