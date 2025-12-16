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

program O2Portable;

uses
  Windows, SysUtils, ShellApi;

{$R *.res}

var
  Directory, FileName, Parameters: string;

begin
  Directory := ExtractFileDir(ParamStr(0)) + '\App\O2';
  FileName := Directory + '\o2.exe';
  Parameters := 'portable' + ' '
    + AnsiQuotedStr(ExtractFilePath(ParamStr(0)), '"');
  if ParamCount > 0 then
    Parameters := Parameters + ' ' + AnsiQuotedStr(ParamStr(1), '"');
  ShellExecute(0, 'open', PChar(FileName), PChar(Parameters),
    PChar(Directory), SW_SHOWNORMAL);
end.
