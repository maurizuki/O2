{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2026 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uClipbrdUtils;

interface

uses
  SysUtils;

procedure CopyToClipboardAsTable(HeaderSelector: TFunc<Integer, string>;
  CellSelector: TFunc<Integer, Integer, string>;
  RowCount, ColumnCount: Integer);

implementation

uses
  Clipbrd;

function EscapePipe(const S: string): string;
begin
  Result := StringReplace(S, '|', '&#124;', [rfReplaceAll]);
end;

procedure CopyToClipboardAsTable(HeaderSelector: TFunc<Integer, string>;
  CellSelector: TFunc<Integer, Integer, string>;
  RowCount, ColumnCount: Integer);
var
  Row, Column: Integer;
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    for Column := 0 to ColumnCount - 1 do
    begin
      SB.Append(EscapePipe(HeaderSelector(Column)));
      if Column < ColumnCount - 1 then SB.Append('|');
    end;

    SB.AppendLine;

    for Column := 0 to ColumnCount - 1 do
    begin
      SB.Append('---');
      if Column < ColumnCount - 1 then SB.Append('|');
    end;

    SB.AppendLine;

    for Row := 0 to RowCount - 1 do
    begin
      for Column := 0 to ColumnCount - 1 do
      begin
        SB.Append(EscapePipe(CellSelector(Row, Column)));
        if Column < ColumnCount - 1 then SB.Append('|');
      end;

      SB.AppendLine;
    end;

    Clipboard.AsText := SB.ToString;
  finally
    SB.Free;
  end;
end;

end.
