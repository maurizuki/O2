{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2023 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uMRUList;

interface

uses
  Classes, System.Generics.Collections;

type
  TMRUItem = class
  private
    FItem: string;
    FCount: Integer;
  public
    constructor Create(const AItem: string; ACount: Integer = 1);
    property Item: string read FItem write FItem;
    property Count: Integer read FCount write FCount;
  end;

  TMRUList = class(TObjectList<TMRUItem>)
  public
    function AddItem(Item: string): Integer;
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

{ TMRUList }

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

end.
