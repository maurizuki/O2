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

unit uBrowserEmulation;

interface

type
  TBrowserEmulation = Integer;

const

{ Internet Explorer 11. Webpages are displayed in IE11 edge mode, regardless of
  the !DOCTYPE directive. }

  IE11 = TBrowserEmulation(11001);

{ Internet Explorer 11. Webpages containing standards-based !DOCTYPE directives
  are displayed in IE11 edge mode. Default value for IE11. }

  IE11Default = TBrowserEmulation(11000);

{ Internet Explorer 10. Webpages are displayed in IE10 Standards mode,
  regardless of the !DOCTYPE directive. }

  IE10 = TBrowserEmulation(10001);

{ Internet Explorer 10. Webpages containing standards-based !DOCTYPE directives
  are displayed in IE10 Standards mode. Default value for Internet Explorer
  10. }

  IE10Default = TBrowserEmulation(10000);

{ Internet Explorer 9. Webpages are displayed in IE9 Standards mode, regardless
  of the !DOCTYPE directive. }

  IE9 = TBrowserEmulation(9999);

{ Internet Explorer 9. Webpages containing standards-based !DOCTYPE directives
  are displayed in IE9 mode. Default value for Internet Explorer 9. }

  IE9Default = TBrowserEmulation(9000);

{ Internet Explorer 8. Webpages are displayed in IE8 Standards mode, regardless
  of the !DOCTYPE directive. }

  IE8 = TBrowserEmulation(8888);

{ Internet Explorer 8. Webpages containing standards-based !DOCTYPE directives
  are displayed in IE8 mode. Default value for Internet Explorer 8. }

  IE8Default = TBrowserEmulation(8000);

{ Internet Explorer 7. Webpages containing standards-based !DOCTYPE directives
  are displayed in IE7 Standards mode. Default value for applications hosting
  the WebBrowser Control. }

  IE7Default = TBrowserEmulation(7000);

procedure SetBrowserEmulation(const ExeName: string; Value: TBrowserEmulation);

implementation

uses
  Registry, Windows;

const
  BrowserEmulationKey = 'SOFTWARE\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION';

procedure SetBrowserEmulation(const ExeName: string; Value: TBrowserEmulation);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    if Reg.OpenKey(BrowserEmulationKey, True) then
      Reg.WriteInteger(ExeName, Value);

    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey(BrowserEmulationKey, True) then
      Reg.WriteInteger(ExeName, Value);
  finally
    Reg.Free;
  end;
end;

end.
