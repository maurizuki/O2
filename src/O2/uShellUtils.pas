{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2024 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uShellUtils;

interface

type
  TShellFolders = class
  private
    class function GetShellFolder(const ShellFolder: string): string;
  public
    class function AdminTools: string;
    class function AppData: string;
    class function Cache: string;
    class function Cookies: string;
    class function Desktop: string;
    class function Favorites: string;
    class function Fonts: string;
    class function History: string;
    class function LocalAppData: string;
    class function MyMusic: string;
    class function MyPictures: string;
    class function NetHood: string;
    class function Personal: string;
    class function PrintHood: string;
    class function Programs: string;
    class function Recent: string;
    class function SendTo: string;
    class function StartMenu: string;
    class function Startup: string;
    class function Templates: string;
  end;

procedure ShellOpen(const FileName: string; const Parameters: string = '');
procedure ShellMailTo(const Recipient: string; const Subject: string = '');

implementation

uses
  Windows, Forms, SysUtils, ShellApi, RegStr, uO2Utils;

procedure ShellOpen(const FileName, Parameters: string);
begin
  ShellExecute(Application.Handle, 'open', PChar(FileName),
    PChar(Parameters), nil, SW_SHOW);
end;

procedure ShellMailTo(const Recipient, Subject: string);
var
  S: string;
begin
  S := 'mailto:' + UrlEscape(Recipient);
  if Subject <> '' then S := S + '?subject=' + UrlEscape(Subject);
  ShellOpen(S);
end;

{ TShellFolders }

class function TShellFolders.GetShellFolder(const ShellFolder: string): string;
var
  Key: HKEY;
  Size: DWORD;
begin
  Result := '';
  if RegOpenKeyEx(HKEY_CURRENT_USER,
    PChar(REGSTR_PATH_EXPLORER + '\Shell Folders'), 0, KEY_READ, Key) = 0 then
  try
    if RegQueryValueEx(Key, PChar(ShellFolder), nil, nil, nil, @Size) = 0 then
    begin
      SetString(Result, nil, Size div SizeOf(Char));
      RegQueryValueEx(Key, PChar(ShellFolder), nil, nil, PByte(Result), @Size);
      SetLength(Result, StrLen(PChar(Result)));
    end;
  finally
    RegCloseKey(Key);
  end;
end;

class function TShellFolders.AdminTools: string;
begin
  Result := GetShellFolder('Administrative Tools');
end;

class function TShellFolders.AppData: string;
begin
  Result := GetShellFolder('AppData');
end;

class function TShellFolders.Cache: string;
begin
  Result := GetShellFolder('Cache');
end;

class function TShellFolders.Cookies: string;
begin
  Result := GetShellFolder('Cookies');
end;

class function TShellFolders.Desktop: string;
begin
  Result := GetShellFolder('Desktop');
end;

class function TShellFolders.Favorites: string;
begin
  Result := GetShellFolder('Favorites');
end;

class function TShellFolders.Fonts: string;
begin
  Result := GetShellFolder('Fonts');
end;

class function TShellFolders.History: string;
begin
  Result := GetShellFolder('History');
end;

class function TShellFolders.LocalAppData: string;
begin
  Result := GetShellFolder('Local AppData');
end;

class function TShellFolders.MyMusic: string;
begin
  Result := GetShellFolder('My Music');
end;

class function TShellFolders.MyPictures: string;
begin
  Result := GetShellFolder('My Pictures');
end;

class function TShellFolders.NetHood: string;
begin
  Result := GetShellFolder('NetHood');
end;

class function TShellFolders.Personal: string;
begin
  Result := GetShellFolder('Personal');
end;

class function TShellFolders.PrintHood: string;
begin
  Result := GetShellFolder('PrintHood');
end;

class function TShellFolders.Programs: string;
begin
  Result := GetShellFolder('Programs');
end;

class function TShellFolders.Recent: string;
begin
  Result := GetShellFolder('Recent');
end;

class function TShellFolders.SendTo: string;
begin
  Result := GetShellFolder('SendTo');
end;

class function TShellFolders.StartMenu: string;
begin
  Result := GetShellFolder('Start Menu');
end;

class function TShellFolders.Startup: string;
begin
  Result := GetShellFolder('Startup');
end;

class function TShellFolders.Templates: string;
begin
  Result := GetShellFolder('Templates');
end;

end.
