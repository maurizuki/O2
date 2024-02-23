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
  Classes, Types, Graphics, JSON;

type
  TAppVersionInfo = record
    ProductName, Version: string;
    MajorVersion, MinorVersion, Release, Build: Word;
  end;

  TAppVersion = class(TPersistent)
  private
    FMajorVersion: Integer;
    FMinorVersion: Integer;
    FRelease: Integer;
    FBuild: Integer;
  public
    const Unused = Integer(-1);
    constructor Create; overload;
    constructor Create(AMajorVersion, AMinorVersion, ARelease,
      ABuild: Integer); overload;
    procedure Assign(Source: TPersistent); override;
    function Compare(AMajorVersion: Integer;
      AMinorVersion: Integer = TAppVersion.Unused;
      ARelease: Integer = TAppVersion.Unused;
      ABuild: Integer = TAppVersion.Unused): TValueRelationship; overload;
    function Compare(const AVersion: TAppVersion): TValueRelationship; overload;
  published
    property MajorVersion: Integer read FMajorVersion write FMajorVersion;
    property MinorVersion: Integer read FMinorVersion write FMinorVersion;
    property Release: Integer read FRelease write FRelease;
    property Build: Integer read FBuild write FBuild;
  end;

  TAppUpdate = class(TPersistent)
  private
    FAppName: string;
    FAppVersion: TAppVersion;
    FDownloadURL: string;
    procedure SetAppVersion(const Value: TAppVersion);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromJSON(const JSON: TJSONValue);
  published
    property AppName: string read FAppName write FAppName;
    property AppVersion: TAppVersion read FAppVersion write SetAppVersion;
    property DownloadURL: string read FDownloadURL write FDownloadURL;
  end;

function MsgBox(const Text: string; Flags: Integer): Integer; inline;
procedure InfoBox(const Text: string); inline;
procedure ErrorBox(const Text: string); inline;
procedure WarningBox(const Text: string); inline;
function YesNoBox(const Text: string): Boolean; inline;
function YesNoWarningBox(const Text: string): Boolean; inline;
function YesNoCancelBox(const Text: string): Integer; inline;
function AbortRetryIgnoreBox(const Text: string): Integer; inline;

procedure DrawHIndicator(const ACanvas: TCanvas; ARect: TRect; AColor: TColor;
  Ratio: Double);

function GetFileSize(const FileName: string): Integer;

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

procedure DrawHIndicator(const ACanvas: TCanvas; ARect: TRect; AColor: TColor;
  Ratio: Double);
begin
  ACanvas.Brush.Color := clWhite;

  ACanvas.FillRect(ARect);
  ACanvas.Rectangle(ARect);

  if Ratio >= 0 then
  begin
    ACanvas.Brush.Color := AColor;

    ARect.Right := Round(ARect.Left	+ ARect.Width * Ratio);
    ARect.Inflate(-1, -1);
    ACanvas.FillRect(ARect);
  end;
end;

function GetFileSize(const FileName: string): Integer;
var
  F: TSearchRec;
begin
  if FindFirst(FileName, faAnyFile, F) = 0 then
    Result := F.Size
  else
    Result := -1;
  FindClose(F);
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

{ TAppVersion }

constructor TAppVersion.Create;
begin
  inherited Create;
  FMajorVersion := 0;
  FMinorVersion := 0;
  FRelease := 0;
  FBuild := 0;
end;

constructor TAppVersion.Create(AMajorVersion, AMinorVersion, ARelease,
  ABuild: Integer);
begin
  inherited Create;
  FMajorVersion := AMajorVersion;
  FMinorVersion := AMinorVersion;
  FRelease := ARelease;
  FBuild := ABuild;
end;

procedure TAppVersion.Assign(Source: TPersistent);
begin
  if Source is TAppVersion then
  begin
    MajorVersion := TAppVersion(Source).MajorVersion;
    MinorVersion := TAppVersion(Source).MinorVersion;
    Release := TAppVersion(Source).Release;
    Build := TAppVersion(Source).Build;
  end
  else
    inherited Assign(Source);
end;

function TAppVersion.Compare(AMajorVersion, AMinorVersion, ARelease,
  ABuild: Integer): TValueRelationship;
begin
  Result := CompareValue(MajorVersion, AMajorVersion);
  if (Result <> EqualsValue) or (AMinorVersion = Unused) then Exit;
  Result := CompareValue(MinorVersion, AMinorVersion);
  if (Result <> EqualsValue) or (ARelease = Unused) then Exit;
  Result := CompareValue(Release, ARelease);
  if (Result <> EqualsValue) or (ABuild = Unused) then Exit;
  Result := CompareValue(Build, ABuild);
end;

function TAppVersion.Compare(const AVersion: TAppVersion): TValueRelationship;
begin
  Result := Compare(AVersion.MajorVersion, AVersion.MinorVersion,
    AVersion.FRelease, AVersion.Build);
end;

{ TAppUpdate }

constructor TAppUpdate.Create;
begin
  inherited Create;
  FAppName := '';
  FAppVersion := TAppVersion.Create;
  FDownloadURL := '';
end;

destructor TAppUpdate.Destroy;
begin
  FAppVersion.Free;
  inherited Destroy;
end;

procedure TAppUpdate.SetAppVersion(const Value: TAppVersion);
begin
  FAppVersion.Assign(Value);
end;

procedure TAppUpdate.LoadFromJSON(const JSON: TJSONValue);
const
  AppVersionPattern =
    '^v(?<MajorVersion>[0-9]+)\.(?<MinorVersion>[0-9]+)(?:\.(?<Release>[0-9]+))?$';
var
  RegEx: TRegEx;
  Match: TMatch;
begin
    RegEx := TRegEx.Create(AppVersionPattern);
    Match := RegEx.Match(JSON.GetValue<string>('tag_name'));
    AppVersion.MajorVersion := StrToInt(Match.Groups['MajorVersion'].Value);
    AppVersion.MinorVersion := StrToInt(Match.Groups['MinorVersion'].Value);
    try
      AppVersion.Release := StrToIntDef(Match.Groups['Release'].Value, 0)
    except
      AppVersion.Release := 0;
    end;
    AppVersion.Build := 0;
    DownloadURL := JSON.GetValue<string>('html_url');
end;

end.
