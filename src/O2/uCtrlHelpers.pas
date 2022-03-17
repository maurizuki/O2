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

unit uCtrlHelpers;

interface

uses
  Classes, Controls, ComCtrls, Menus;

type
  TControlHelper = class helper for TControl
  public
    procedure DropDown(APopupMenu: TPopupMenu);
  end;

  TListViewHelper = class helper for TCustomListView
  public
    procedure InvertSelection;
    procedure FreeSelectedItemsData;
    function ListSelectedItemsData: TList;
    procedure SelectItemsByData(const List: TList);
    procedure ResizeColumns; overload;
    procedure ResizeColumns(ColumnIndex: Integer); overload;
  end;

implementation

uses
  Types, Windows, CommCtrl;

{ TControlHelper }

procedure TControlHelper.DropDown(APopupMenu: TPopupMenu);
var
  P: TPoint;
begin
  P := Self.Parent.ClientToScreen(Point(Self.Left, Self.Top + Self.Height));
  APopupMenu.Popup(P.X, P.Y);
end;

{ TListViewHelper }

procedure TListViewHelper.InvertSelection;
var
  Item: TListItem;
begin
  Self.Items.BeginUpdate;
  try
    for Item in Self.Items do
      Item.Selected := not Item.Selected;
  finally
    Self.Items.EndUpdate;
  end;
end;

procedure TListViewHelper.FreeSelectedItemsData;
var
  Item: TListItem;
begin
  for Item in Self.Items do
    if Item.Selected then
    begin
      TObject(Item.Data).Free;
      Item.Data := nil;
    end;
end;

function TListViewHelper.ListSelectedItemsData: TList;
var
  Item: TListItem;
begin
  Result := TList.Create;
  for Item in Self.Items do
    if Item.Selected then
      Result.Add(Item.Data);
end;

procedure TListViewHelper.SelectItemsByData(const List: TList);
var
  Item: TListItem;
  Data: Pointer;
begin
  for Data in List do
  begin
    Item := Self.FindData(0, Data, True, False);
    if Assigned(Item) then Item.Selected := True;
  end;
end;

procedure TListViewHelper.ResizeColumns;
var
  I, Width, Count, ColumnWidth: Integer;
begin
  Width := Self.ClientWidth;
  Count := Self.Columns.Count;
  ColumnWidth := Width div Count;
  for I := 0 to Count - 2 do
  begin
    TListColumn(Self.Columns[I]).Width := ColumnWidth;
    Dec(Width, ColumnWidth);
  end;
  TListColumn(Self.Columns[Count - 1]).Width := Width;
end;

procedure TListViewHelper.ResizeColumns(ColumnIndex: Integer);
var
  I, Width, Count: Integer;
begin
  Width := Self.ClientWidth;
  Count := Self.Columns.Count;
  for I := 0 to Count - 1 do
    if I <> ColumnIndex then
      Dec(Width, SendMessage(Self.Handle, LVM_GETCOLUMNWIDTH, I, 0));
  TListColumn(Self.Columns[ColumnIndex]).Width := Width;
end;

end.
