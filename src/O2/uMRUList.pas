{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2021 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uMRUList;

interface

uses
  Classes, Contnrs;

type
  TMRUItem = class(TObject)
  private
    FItem: string;
    FCount: Integer;
  public
    constructor Create(const AItem: string; ACount: Integer = 1);
    property Item: string read FItem write FItem;
    property Count: Integer read FCount write FCount;
  end;

  TMRUListEnumerator = class(TListEnumerator)
  public
    function GetCurrent: TMRUItem;
    property Current: TMRUItem read GetCurrent;
  end;

  TMRUList = class(TObjectList)
  private
    function GetItem(Index: Integer): TMRUItem;
    procedure SetItem(Index: Integer; AObject: TMRUItem);
  public
    function AddItem(Item: string): Integer;
    function GetEnumerator: TMRUListEnumerator;
    property Items[Index: Integer]: TMRUItem
      read GetItem write SetItem; default;
  end;

implementation

uses
  SysUtils;

{ TMRUItem }

constructor TMRUItem.Create(const AItem: string; ACount: Integer);
begin
  FItem := AItem;
  FCount := ACount;
end;

{ TMRUListEnumerator }

function TMRUListEnumerator.GetCurrent: TMRUItem;
begin
  Result := inherited GetCurrent;
end;

{ TMRUList }

function TMRUList.GetItem(Index: Integer): TMRUItem;
begin
  Result := TMRUItem(inherited Items[Index]);
end;

procedure TMRUList.SetItem(Index: Integer; AObject: TMRUItem);
begin
  inherited Items[Index] := AObject;
end;

function TMRUList.AddItem(Item: string): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to Count - 1 do
    if SameText(Items[I].Item, Item) then
    begin
      Items[I].Count := Items[I].Count + 1;
      Result := I;
      Break;
    end;

  if Result = -1 then Result := Add(TMRUItem.Create(Item));

  while (Result > 0) and (Items[Pred(Result)].Count <= Items[Result].Count) do
  begin
    Move(Result, Pred(Result));
    Dec(Result);
  end;
end;

function TMRUList.GetEnumerator: TMRUListEnumerator;
begin
  Result := TMRUListEnumerator.Create(Self);
end;

end.
