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
  Windows, Clipbrd, uHTMLHelper;

var
  HTMLFormat: UINT;

  function EscapePipe(const S: string): string;
begin
  Result := StringReplace(S, '|', '&#124;', [rfReplaceAll]);
end;

function ToPipeTable(HeaderSelector: TFunc<Integer, string>;
  CellSelector: TFunc<Integer, Integer, string>;
  RowCount, ColumnCount: Integer): string;
var
  Row, Column: Integer;
  SB: TStringBuilder;
begin
  if ColumnCount <= 0 then Exit('');

  SB := TStringBuilder.Create;
  try
    SB.Append('|');
    for Column := 0 to ColumnCount - 1 do
    begin
      SB.Append(' ')
        .Append(EscapePipe(HeaderSelector(Column)))
        .Append(' |');
    end;
    SB.AppendLine;

    SB.Append('|');
    for Column := 0 to ColumnCount - 1 do SB.Append(' --- |');
    SB.AppendLine;

    for Row := 0 to RowCount - 1 do
    begin
      SB.Append('|');
      for Column := 0 to ColumnCount - 1 do
      begin
        SB.Append(' ')
          .Append(EscapePipe(CellSelector(Row, Column)))
          .Append(' |');
      end;
      SB.AppendLine;
    end;

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function ToHTMLTable(HeaderSelector: TFunc<Integer, string>;

  CellSelector: TFunc<Integer, Integer, string>;
  RowCount, ColumnCount: Integer): string;
var
  Row, Column: Integer;
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create('<table><thead><tr>');
  try
    for Column := 0 to ColumnCount - 1 do
      SB.Append('<th>')
        .AppendHTML(HeaderSelector(Column))
        .Append('</th>');

    SB.Append('</tr></thead><tbody>');

    for Row := 0 to RowCount - 1 do
    begin
      SB.Append('<tr>');

      for Column := 0 to ColumnCount - 1 do
        SB.Append('<td>')
          .AppendHTML(CellSelector(Row, Column))
          .Append('</td>');

      SB.Append('</tr>');
    end;

    SB.Append('</tbody></table>');

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function ToTextFormat(const S: string): HGLOBAL;
var
  Ptr: Pointer;
begin
  Result := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT,
    (Length(S) + 1) * SizeOf(Char));
  if Result <> 0 then
  begin
    Ptr := GlobalLock(Result);
    Move(S[1], Ptr^, Length(S) * SizeOf(Char));
    GlobalUnlock(Result);
  end;
end;

function ToHTMLFormat(const S: string): HGLOBAL;
var
  Ptr: Pointer;
  HTMLStr: string;
  HTMLUTF8Str: UTF8String;
begin
  HTMLStr :=
    'Version:0.9'#13#10 +
    'StartHTML:0000000105'#13#10 +
    'EndHTML:XXXXXXXXXX'#13#10 +
    'StartFragment:0000000125'#13#10 +
    'EndFragment:YYYYYYYYYY'#13#10 +
    '<html><body><!--StartFragment-->' + S + '<!--EndFragment--></body></html>';

  HTMLStr := StringReplace(HTMLStr, 'XXXXXXXXXX',
    Format('%.10d', [Length(HTMLStr)]), []);
  HTMLStr := StringReplace(HTMLStr, 'YYYYYYYYYY',
    Format('%.10d', [Length(HTMLStr) - 36]), []);
  HTMLUTF8Str := UTF8String(HTMLStr);

  Result := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT,
    Length(HTMLUTF8Str) + 1);
  if Result <> 0 then
  begin
    Ptr := GlobalLock(Result);
    Move(HTMLUTF8Str[1], Ptr^, Length(HTMLUTF8Str));
    GlobalUnlock(Result);
  end;
end;

procedure CopyToClipboardAsTable(HeaderSelector: TFunc<Integer, string>;
  CellSelector: TFunc<Integer, Integer, string>;
  RowCount, ColumnCount: Integer);
var
  HtmlHandle, TextHandle: HGLOBAL;
begin
  TextHandle := ToTextFormat(ToPipeTable(HeaderSelector, CellSelector,
    RowCount, ColumnCount));

  HtmlHandle := ToHTMLFormat(ToHTMLTable(HeaderSelector, CellSelector,
    RowCount, ColumnCount));

  Clipboard.Clear;
  Clipboard.Open;
  try
    if HtmlHandle <> 0 then
      Clipboard.SetAsHandle(HTMLFormat, HtmlHandle);

    if TextHandle <> 0 then
      Clipboard.SetAsHandle(CF_UNICODETEXT, TextHandle);
  finally
    Clipboard.Close;
  end;
end;

initialization
  HTMLFormat := RegisterClipboardFormat('HTML Format');

end.
