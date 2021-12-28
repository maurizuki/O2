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

unit uLookupHelper;

interface

uses
  SysUtils, StdCtrls;

type
  PLookupMapEntry = ^TLookupMapEntry;
  TLookupMapEntry = record
    Value: Integer;
    Item: string;
  end;

  TLookupHelper = class
  protected
    class procedure GetMapBounds(out LowerBound, UpperBound: Integer);
      virtual; abstract;
    class function GetMapEntry(Index: Integer): PLookupMapEntry;
      virtual; abstract;
  public
    class procedure Fill(const Combo: TCustomCombo);
    class procedure Select(const Combo: TCustomCombo; Value: Integer);
    class function SelectedValue(const Combo: TCustomCombo;
      Default: Integer = -1): Integer;
    class function Lookup(Value: Integer): string;
  end;

implementation

{ TLookupHelper }

class procedure TLookupHelper.Fill(const Combo: TCustomCombo);
var
  I, LowerBound, UpperBound: Integer;
  P: PLookupMapEntry;
begin
  GetMapBounds(LowerBound, UpperBound);
  for I := LowerBound to UpperBound do
  begin
    P := GetMapEntry(I);
    Combo.Items.AddObject(P^.Item, TObject(P));
  end;
end;

class procedure TLookupHelper.Select(const Combo: TCustomCombo; Value: Integer);
var
  I: Integer;
begin
  for I := 0 to Combo.Items.Count - 1 do
    if Assigned(Combo.Items.Objects[I])
      and (PLookupMapEntry(Combo.Items.Objects[I])^.Value = Value) then
    begin
      Combo.ItemIndex := I;
      Break;
    end;
end;

class function TLookupHelper.SelectedValue(const Combo: TCustomCombo;
  Default: Integer): Integer;
begin
  if (Combo.ItemIndex > -1) and
    Assigned(Combo.Items.Objects[Combo.ItemIndex]) then
    Result := PLookupMapEntry(Combo.Items.Objects[Combo.ItemIndex])^.Value
  else
    Result := Default;
end;

class function TLookupHelper.Lookup(Value: Integer): string;
var
  I, LowerBound, UpperBound: Integer;
  P: PLookupMapEntry;
begin
  Result := '';
  GetMapBounds(LowerBound, UpperBound);
  for I := LowerBound to UpperBound do
  begin
    P := GetMapEntry(I);
    if P^.Value = Value then
    begin
      Result := P^.Item;
      Break;
    end;
  end;
end;

end.
