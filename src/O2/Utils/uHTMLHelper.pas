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

unit uHTMLHelper;

interface

uses
  Classes, SysUtils, uO2Objects;

type
  THTMLHelper = class helper for TStringBuilder
  private
    class function EncodeHTML(const S: string): string;
    class function ProcessMarkdown(const S: string): string;
  public
    function AppendHTML(const S: string): TStringBuilder; overload;
    function AppendHTML(const Lines: TStrings;
      TextType: TO2TextType): TStringBuilder; overload;
    function AppendContextMenuBlockerScript: TStringBuilder;
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

class function THTMLHelper.ProcessMarkdown(const S: string): string;
var
  MarkdownProcessor: TCommonMarkProcessor;
begin
  MarkdownProcessor := TCommonMarkProcessor.Create;
  try
    MarkdownProcessor.AllowUnsafe := False;
    MarkdownProcessor.OpenLinksInNewWindow := True;
    Result := MarkdownProcessor.process(S);
  finally
    MarkdownProcessor.Free;
  end;
end;

function THTMLHelper.AppendHTML(const S: string): TStringBuilder;
begin
  Result := Self.Append(EncodeHTML(S));
end;

function THTMLHelper.AppendHTML(const Lines: TStrings;
  TextType: TO2TextType): TStringBuilder;
var
  S: string;
begin
  if TextType = ttCommonMark then
    Exit(Self.Append(ProcessMarkdown(Lines.Text)));

  for S in Lines do Self.Append(EncodeHTML(S)).Append('<br />');
  Result := Self;
end;

function THTMLHelper.AppendContextMenuBlockerScript: TStringBuilder;
begin
  Result := Self.Append('<script type="text/javascript">document.addEventListener("contextmenu", event => event.preventDefault(), true)</script>');
end;

end.
