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

unit uUtils;

interface

uses
  Types, Graphics, JSON;

type
  TVersion = record
    MajorVersion, MinorVersion, Release, Build: Word;
    function Compare(Other: TVersion): TValueRelationship;
  end;

  TAppVersionInfo = record
    AppName, DisplayVersion: string;
    Version: TVersion;
  end;

  TAppUpdateInfo = record
    Version: TVersion;
    DownloadURL: string;
    constructor Create(const JSON: TJSONValue);
  end;

function MsgBox(const Text: string; Flags: Integer): Integer; inline;
procedure InfoBox(const Text: string); inline;
procedure ErrorBox(const Text: string); inline;
procedure WarningBox(const Text: string); inline;
function YesNoBox(const Text: string): Boolean; inline;
function YesNoWarningBox(const Text: string): Boolean; inline;
function YesNoCancelBox(const Text: string): Integer; inline;
function AbortRetryIgnoreBox(const Text: string): Integer; inline;

function CombinePath(const S1, S2: string): string; inline;

function GetLanguageName(LangId: Word): string;

procedure SetLocaleOverride(const FileName, LocaleId: string);
procedure DeleteLocaleOverride(const FileName: string);

function GetSettingsOverride(const FileName: string; out Path: string): Boolean;

implementation

uses
  Windows, Forms, SysUtils, Math, Registry, RegularExpressions;

const
  LocaleOverrideKey = 'SOFTWARE\Embarcadero\Locales';
  SettingsOverrideKey = 'SOFTWARE\O2\Settings';

function MsgBox(const Text: string; Flags: Integer): Integer;
begin
  Result :=
    Application.MessageBox(PChar(Text), PChar(Application.Title), Flags);
end;

procedure InfoBox(const Text: string);
begin
  MsgBox(Text, MB_ICONINFORMATION or MB_OK);
end;

procedure ErrorBox(const Text: string);
begin
  MsgBox(Text, MB_ICONERROR or MB_OK);
end;

procedure WarningBox(const Text: string);
begin
  MsgBox(Text, MB_ICONWARNING or MB_OK);
end;

function YesNoBox(const Text: string): Boolean;
begin
  Result := MsgBox(Text, MB_ICONQUESTION or MB_YESNO) = ID_YES;
end;

function YesNoWarningBox(const Text: string): Boolean;
begin
  Result := MsgBox(Text, MB_ICONWARNING or MB_YESNO) = ID_YES;
end;

function YesNoCancelBox(const Text: string): Integer;
begin
  Result := MsgBox(Text, MB_ICONQUESTION or MB_YESNOCANCEL);
end;

function AbortRetryIgnoreBox(const Text: string): Integer;
begin
  Result := MsgBox(Text, MB_ICONWARNING or MB_ABORTRETRYIGNORE);
end;

function CombinePath(const S1, S2: string): string;
begin
  Result := IncludeTrailingPathDelimiter(S1) + S2;
end;

function GetLanguageName(LangId: Word): string;
var
  Size: DWORD;
begin
  SetLength(Result, MAX_PATH);
  Size := VerLanguageName(LangId, PChar(Result), MAX_PATH);
  SetLength(Result, Size);
end;

procedure SetLocaleOverride(const FileName, LocaleId: string);
var
  Key: HKEY;
  Disposition: DWORD;
begin
  if RegCreateKeyEx(HKEY_CURRENT_USER, LocaleOverrideKey, 0, nil,
    REG_OPTION_NON_VOLATILE, KEY_WRITE, nil, Key, @Disposition) = 0 then
  try
    RegSetValueEx(Key, PChar(FileName), 0, REG_SZ,
      PChar(LocaleId), (Length(LocaleId) + 1) * SizeOf(Char));
  finally
    RegCloseKey(Key);
  end;
end;

procedure DeleteLocaleOverride(const FileName: string);
var
  Key: HKEY;
begin
  if RegOpenKeyEx(HKEY_CURRENT_USER,
    LocaleOverrideKey, 0, KEY_WRITE, Key) = 0 then
  try
    RegDeleteValue(Key, PChar(FileName));
  finally
    RegCloseKey(Key);
  end;
end;

function GetSettingsOverride(const FileName: string; out Path: string): Boolean;
var
  Reg: TRegistry;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    if Reg.OpenKey(SettingsOverrideKey, False) then
      if Reg.ValueExists(FileName) then
      begin
        Path := Reg.ReadString(FileName);
        Result := True;
      end;
  finally
    Reg.Free;
  end;
end;

{ TVersion }

function TVersion.Compare(Other: TVersion): TValueRelationship;
begin
  Result := CompareValue(MajorVersion, Other.MajorVersion);
  if (Result <> EqualsValue) then Exit;
  Result := CompareValue(MinorVersion, Other.MinorVersion);
  if (Result <> EqualsValue) then Exit;
  Result := CompareValue(Release, Other.Release);
  if (Result <> EqualsValue) then Exit;
  Result := CompareValue(Build, Other.Build);
end;

{ TAppUpdateInfo }

constructor TAppUpdateInfo.Create(const JSON: TJSONValue);
const
  AppVersionPattern =
    '^v(?<MajorVersion>[0-9]+)\.(?<MinorVersion>[0-9]+)(?:\.(?<Release>[0-9]+))?$';
var
  RegEx: TRegEx;
  Match: TMatch;
begin
  RegEx := TRegEx.Create(AppVersionPattern);
  Match := RegEx.Match(JSON.GetValue<string>('tag_name'));
  Version.MajorVersion := StrToInt(Match.Groups['MajorVersion'].Value);
  Version.MinorVersion := StrToInt(Match.Groups['MinorVersion'].Value);
  try
    Version.Release := StrToIntDef(Match.Groups['Release'].Value, 0)
  except
    Version.Release := 0;
  end;
  Version.Build := 0;

  DownloadURL := JSON.GetValue<string>('html_url');
end;

end.
