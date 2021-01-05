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

program O2Portable;

uses
  Windows, Forms, SysUtils, ShellApi;

{$R *.res}

var
  Directory, FileName, Parameters: string;

begin
  Directory := ExtractFilePath(Application.ExeName) + '\App\O2';
  FileName := Directory + '\o2.exe';
  Parameters := 'portable' + ' '
    + AnsiQuotedStr(ExtractFilePath(Application.ExeName), '"');
  if ParamCount > 0 then
    Parameters := Parameters + ' ' + AnsiQuotedStr(ParamStr(1), '"');
  ShellExecute(Application.Handle, 'open', PChar(FileName), PChar(Parameters),
    PChar(Directory), SW_SHOWNORMAL);
end.
