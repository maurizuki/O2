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

unit uHTMLHelper;

interface

uses
  Classes, SysUtils;

type
  THTMLHelper = class helper for TStringBuilder
  private
    class function EncodeHTML(const S: string): string;
  public
    function AppendHTML(const S: string): TStringBuilder; overload;
    function AppendHTML(const Lines: TStrings;
      Markdown: Boolean): TStringBuilder; overload;
  end;

implementation

uses
  MarkdownCommonMark;

{ THTMLHelper }

class function THTMLHelper.EncodeHTML(const S: string): string;
begin
  Result := StringReplace(S, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
end;

function THTMLHelper.AppendHTML(const S: string): TStringBuilder;
begin
  Result := Self.Append(EncodeHTML(S));
end;

function THTMLHelper.AppendHTML(const Lines: TStrings;
  Markdown: Boolean): TStringBuilder;
var
  MarkdownProcessor: TCommonMarkProcessor;
  S: string;
begin
  if Markdown then
  begin
    MarkdownProcessor := TCommonMarkProcessor.Create;
    try
      MarkdownProcessor.AllowUnsafe := False;
      Result := Self.Append(MarkdownProcessor.process(Lines.Text));
    finally
      MarkdownProcessor.Free;
    end;
  end
  else
  begin
    Self.Append('<pre>');
    for S in Lines do Self.Append(EncodeHTML(S)).Append('<br />');
    Result := Self.Append('</pre>');
  end;
end;

end.
