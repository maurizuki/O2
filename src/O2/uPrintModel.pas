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

unit uPrintModel;

interface

uses
  Windows, Graphics, uO2File, uO2Objects, uO2Relations, uServices;

type
  TPrintModel = class(TInterfacedObject, IPrint)
  private
    FTitle: string;
    FO2File: TO2File;
    FStorage: IStorage;
    FIncludeTags: Boolean;
    FIncludeNotes: Boolean;
    FIncludeRelations: Boolean;
    FIncludePasswords: Boolean;
    FObjectEnumerator: IEnumerator<TO2Object>;
    FFieldIndex: Integer;
    FObjRelations: TO2ObjRelations;
    FObjRelationIndex: Integer;
    FNoteLineIndex: Integer;
    FPrintDate: string;
    function GetTitle: string;
    function GetIncludeTags: Boolean;
    function GetIncludeNotes: Boolean;
    function GetIncludeRelations: Boolean;
    function GetIncludePasswords: Boolean;
    procedure SetIncludeTags(Value: Boolean);
    procedure SetIncludeNotes(Value: Boolean);
    procedure SetIncludeRelations(Value: Boolean);
    procedure SetIncludePasswords(Value: Boolean);
  public
    constructor Create(const O2File: TO2File;
      SelectedObjects: IEnumerable<TO2Object>; Storage: IStorage);

    procedure StoreSettings;

    function DrawNextPage(const Canvas: TCanvas; PageRect, PrintRect: TRect;
      PageIndex: Integer): Boolean;

    property Title: string read GetTitle;
    property IncludeTags: Boolean read GetIncludeTags write SetIncludeTags;
    property IncludeNotes: Boolean read GetIncludeNotes write SetIncludeNotes;
    property IncludeRelations: Boolean read GetIncludeRelations
      write SetIncludeRelations;
    property IncludePasswords: Boolean read GetIncludePasswords
      write SetIncludePasswords;
  end;

implementation

uses
  Classes, SysUtils, uGlobal, uO2Rules;

{ TPrintModel }

constructor TPrintModel.Create(const O2File: TO2File;
  SelectedObjects: IEnumerable<TO2Object>; Storage: IStorage);
begin
  if O2File.Title = '' then
    FTitle := ChangeFileExt(ExtractFileName(O2File.FileName), '')
  else
    FTitle := O2File.Title;
  FO2File := O2File;
  FObjectEnumerator := SelectedObjects.GetEnumerator;
  FStorage := Storage;
  FIncludeTags := FStorage.ReadBoolean(IdPrintIncludeTags, True);
  FIncludeNotes := FStorage.ReadBoolean(IdPrintIncludeNotes, True);
  FIncludeRelations := FStorage.ReadBoolean(IdPrintIncludeRelations, True);
  FIncludePasswords := FStorage.ReadBoolean(IdPrintIncludePasswords, True);
end;

procedure TPrintModel.SetIncludeNotes(Value: Boolean);
begin
  FIncludeNotes := Value;
end;

procedure TPrintModel.SetIncludePasswords(Value: Boolean);
begin
  FIncludePasswords := Value;
end;

procedure TPrintModel.SetIncludeRelations(Value: Boolean);
begin
  FIncludeRelations := Value;
end;

procedure TPrintModel.SetIncludeTags(Value: Boolean);
begin
  FIncludeTags := Value;
end;

procedure TPrintModel.StoreSettings;
begin
  FStorage.WriteBoolean(IdPrintIncludeTags, FIncludeTags);
  FStorage.WriteBoolean(IdPrintIncludeNotes, FIncludeNotes);
  FStorage.WriteBoolean(IdPrintIncludeRelations, FIncludeRelations);
  FStorage.WriteBoolean(IdPrintIncludePasswords, FIncludePasswords);
end;

function TPrintModel.DrawNextPage(const Canvas: TCanvas; PageRect,
  PrintRect: TRect; PageIndex: Integer): Boolean;
var
  AObject: TO2Object;
  AField: TO2Field;
  TextMetric: TTextMetric;
  LogPixelsY: Integer;
  Rect, Rect2: TRect;
  W, H: Integer;
  S: string;

function MillimetersToPixelsY(Value: Single): Integer;
begin
  Result := Round((Value / 25.4) * LogPixelsY);
end;

begin
  Result := True;

  if PageIndex = 0 then
  begin
    FObjectEnumerator.Reset;
    if not FObjectEnumerator.MoveNext then Exit(False);

    FFieldIndex := 0;
    FObjRelations := nil;
    FObjRelationIndex := 0;
    FNoteLineIndex := 0;
    FPrintDate := DateTimeToStr(Now);
  end;

  LogPixelsY := GetDeviceCaps(Canvas.Handle, Windows.LOGPIXELSY);

  Canvas.Font.Name := 'Arial';
  Canvas.Font.Size := 8;
  GetTextMetrics(Canvas.Handle, TextMetric);
  W := (PrintRect.Right - PrintRect.Left) div 2;
  H := TextMetric.tmHeight + TextMetric.tmInternalLeading
    + TextMetric.tmExternalLeading;

  S := FTitle;
  Rect.Left := PrintRect.Left;
  Rect.Top := PrintRect.Top;
  Rect.Right := PrintRect.Left + W;
  Rect.Bottom := PrintRect.Top + H;
  DrawText(Canvas.Handle, PChar(S), Length(S), Rect,
    DT_NOPREFIX or DT_LEFT or DT_TOP);

  S := FO2File.FileName;
  Rect.Left := PrintRect.Right - W;
  Rect.Right := PrintRect.Right;
  DrawText(Canvas.Handle, PChar(S), Length(S), Rect,
    DT_NOPREFIX or DT_RIGHT or DT_TOP);

  S := FPrintDate;
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

  Inc(PrintRect.Top, H + MillimetersToPixelsY(5));
  Dec(PrintRect.Bottom, H + MillimetersToPixelsY(5));

  Rect.Top := PrintRect.Top;

  if PageIndex = 0 then
  begin
    Canvas.Font.Size := 24;
    GetTextMetrics(Canvas.Handle, TextMetric);
    H := TextMetric.tmHeight + TextMetric.tmInternalLeading
      + TextMetric.tmExternalLeading;

    S := FTitle;
    Rect.Left := PrintRect.Left;
    Rect.Right := PrintRect.Right;
    Rect.Bottom := Rect.Top + H;
    DrawText(Canvas.Handle, PChar(S), Length(S), Rect,
      DT_NOPREFIX or DT_LEFT or DT_TOP);

    Inc(Rect.Top, H + MillimetersToPixelsY(5));
  end;

  { Objects }

  repeat
    AObject := FObjectEnumerator.Current;

    Canvas.Font.Name := 'Arial';
    Canvas.Font.Size := 12;
    GetTextMetrics(Canvas.Handle, TextMetric);
    H := TextMetric.tmHeight + TextMetric.tmInternalLeading
      + TextMetric.tmExternalLeading;

    if Rect.Top + H + MillimetersToPixelsY(3) > PrintRect.Bottom then Exit;

    S := AObject.Name;
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

    Inc(Rect.Top, H + MillimetersToPixelsY(3));

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

    if FIncludeTags and (FFieldIndex = 0) and (AObject.Tag <> '') then
    begin
      if Rect.Top + H > PrintRect.Bottom then Exit;

      S := STags;
      DrawText(Canvas.Handle, PChar(S), Length(S), Rect,
        DT_NOPREFIX or DT_LEFT or DT_TOP);

      S := AObject.Tag;
      DrawText(Canvas.Handle, PChar(S), Length(S), Rect2,
        DT_NOPREFIX or DT_LEFT or DT_TOP);

      OffsetRect(Rect, 0, H);
      OffsetRect(Rect2, 0, H);
    end;

    while FFieldIndex < AObject.Fields.Count do
    begin
      AField := AObject.Fields[FFieldIndex];

      if FIncludePasswords
        or not Assigned(FO2File.Rules.FindFirstRule(AField, [rtPassword])) then
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

      Inc(FFieldIndex);
    end;

    { Relations }

    if FIncludeRelations then
    begin
      if FObjRelations = nil then
      begin
        FObjRelations := FO2File.Relations.GetObjectRelations(AObject);
        FObjRelations.SortByObjName;
      end;

      Canvas.Font.Name := 'Arial';
      Canvas.Font.Size := 10;
      GetTextMetrics(Canvas.Handle, TextMetric);
      H := TextMetric.tmHeight + TextMetric.tmInternalLeading
        + TextMetric.tmExternalLeading;

      if (FObjRelationIndex = 0) and (FObjRelations.Count > 0) then
      begin
        Inc(Rect.Top, MillimetersToPixelsY(1));

        Canvas.Pen.Width := 1;
        Canvas.Pen.Style := psDot;
        Canvas.MoveTo(PrintRect.Left, Rect.Top);
        Canvas.LineTo(PrintRect.Right, Rect.Top);

        Inc(Rect.Top, MillimetersToPixelsY(1));
      end;

      Rect.Left := PrintRect.Left;
      Rect.Right := PrintRect.Right - W;
      Rect.Bottom := Rect.Top + H;

      Rect2.Top := Rect.Top;
      Rect2.Left := PrintRect.Right - W;
      Rect2.Right := PrintRect.Right;
      Rect2.Bottom := Rect2.Top + H;

      while FObjRelationIndex < FObjRelations.Count do
      begin
        if Rect.Top + H > PrintRect.Bottom then Exit;

        if Assigned(FObjRelations[FObjRelationIndex].Obj) then
          S := FObjRelations[FObjRelationIndex].Obj.Name
        else
          S := '';
        DrawText(Canvas.Handle, PChar(S), Length(S), Rect,
          DT_NOPREFIX or DT_LEFT or DT_TOP);

        S := FObjRelations[FObjRelationIndex].Role;
        DrawText(Canvas.Handle, PChar(S), Length(S), Rect2,
          DT_NOPREFIX or DT_LEFT or DT_TOP);

        OffsetRect(Rect, 0, H);
        OffsetRect(Rect2, 0, H);
        Inc(FObjRelationIndex);
      end;
    end;

    { Text }

    if FIncludeNotes then
    begin
      Canvas.Font.Name := 'Courier New';
      Canvas.Font.Size := 10;
      GetTextMetrics(Canvas.Handle, TextMetric);
      H := TextMetric.tmHeight + TextMetric.tmInternalLeading
        + TextMetric.tmExternalLeading;

      if (FNoteLineIndex = 0) and (AObject.Text.Count > 0) then
      begin
        Inc(Rect.Top, MillimetersToPixelsY(1));

        Canvas.Pen.Width := 1;
        Canvas.Pen.Style := psDot;
        Canvas.MoveTo(PrintRect.Left, Rect.Top);
        Canvas.LineTo(PrintRect.Right, Rect.Top);

        Inc(Rect.Top, MillimetersToPixelsY(1));
      end;

      Rect.Left := PrintRect.Left;
      Rect.Bottom := Rect.Top + H;

      while FNoteLineIndex < AObject.Text.Count do
      begin
        Rect.Right := PrintRect.Right;

        S := AObject.Text[FNoteLineIndex];
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
        Inc(FNoteLineIndex);
      end;
    end;

    Inc(Rect.Top, MillimetersToPixelsY(5));

    FFieldIndex := 0;
    FObjRelationIndex := 0;
    FNoteLineIndex := 0;

    FreeAndNil(FObjRelations);

  until not FObjectEnumerator.MoveNext;

  Result := False;
end;

function TPrintModel.GetIncludeNotes: Boolean;
begin
  Result := FIncludeNotes;
end;

function TPrintModel.GetIncludePasswords: Boolean;
begin
  Result := FIncludePasswords;
end;

function TPrintModel.GetIncludeRelations: Boolean;
begin
  Result := FIncludeRelations;
end;

function TPrintModel.GetIncludeTags: Boolean;
begin
  Result := FIncludeTags;
end;

function TPrintModel.GetTitle: string;
begin
  Result := FTitle;
end;

end.
