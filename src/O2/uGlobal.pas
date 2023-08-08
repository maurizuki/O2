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

unit uGlobal;

interface

uses
  Windows, Classes, Graphics, ComCtrls, StdCtrls, uO2Defs, uO2Rules,
  uLookupHelper;

const
  DefaultFileExt = 'o2';

{ Application file names and paths }

  AppInfoFile = 'appinfo.ini';
  AppIconFile = 'appicon.ico';
  AppIcon16File = 'appicon_16.png';
  AppIcon32File = 'appicon_32.png';
  FileTypeIconFile = 'o2.ico';
  FileTypeIcon16File = 'o2_16.png';
  FileTypeIcon32File = 'o2_32.png';
  LauncherFile = 'O2Portable.exe';
  SettingsFile = 'O2.xml';
  HTMLHelpFile = 'help.html';
  LicenseFile = 'License.rtf';
  ReadMeFile = 'ReadMe.rtf';

  LocalSettingsDir = 'O2';

  PortableLauncherPath = '';
  PortableAppPath = 'App\O2';
  PortableAppInfoPath = 'App\AppInfo';
  PortableSettingsPath = 'Data\Settings';
  PortableFileTypeIconsPath = 'App\AppInfo\FileTypeIcons';

{ Application file list IDs }

  IdAppExe = 'AppExe';
  IdAppInfo = 'AppInfo';
  IdAppIcon = 'AppIcon';
  IdAppIcon16 = 'AppIcon16';
  IdAppIcon32 = 'AppIcon32';
  IdFileTypeIcon = 'FileTypeIcon';
  IdFileTypeIcon16 = 'FileTypeIcon16';
  IdFileTypeIcon32 = 'FileTypeIcon32';
  IdLauncher = 'Launcher';
  IdSettings = 'Settings';
  IdHelp = 'Help';
  IdLicense = 'License';
  IdReadMe = 'ReadMe';
  IdResourceModule = 'ResourceModule';

{ Configuration file IDs }

  IdMRUList = 'Application.MRU';
  IdStayOnTop = 'Application.StayOnTop';
  IdTransparency = 'Application.Transparency';
  IdTransparencyOnlyIfDeactivated = 'Application.Transparency.OnlyIfDeactivated';
  IdAutoCheckForUpdates = 'Application.AutoCheckForUpdates';
  IdLastCheckForUpdates = 'Application.AutoCheckForUpdates.LastCheck';
  IdViewStyle = 'MainWindow.ObjectView.ViewStyle';
  IdSortColumn = 'MainWindow.ObjectView.Sort.Column';
  IdSortAscending = 'MainWindow.ObjectView.Sort.Ascending';
  IdHTMLExportIncludeIndex = 'ExportToHTML.Include.Index';
  IdHTMLExportIncludeTags = 'ExportToHTML.Include.Tags';
  IdHTMLExportIncludeNotes = 'ExportToHTML.Include.Notes';
  IdHTMLExportIncludeRelations = 'ExportToHTML.Include.Relations';
  IdHTMLExportIncludePasswords = 'ExportToHTML.Include.Passwords';
  IdPrintIncludeTags = 'Print.Include.Tags';
  IdPrintIncludeNotes = 'Print.Include.Notes';
  IdPrintIncludeRelations = 'Print.Include.Relations';
  IdPrintIncludePasswords = 'Print.Include.Passwords';

{ UI localization languages }

type
  TLanguageMapEntry = record
    LangId: Word;
    Language: string;
  end;

const
  Languages: array[1..2] of TLanguageMapEntry = (
    (LangId: (SUBLANG_ENGLISH_US shl 10) or LANG_ENGLISH; Language: 'ENU'),
    (LangId: (SUBLANG_ITALIAN shl 10) or LANG_ITALIAN;    Language: 'ITA'));

resourcestring
  STags = 'Tags';
  STagsNone = '(none)';
  SStatusItemsCount = 'Items: %d';
  SStatusSelectedItemsCount = 'Selected items: %d';
  SStatusObject = '%s Tags: %s';
  SStatusPagesCount = 'Pages: %d';
  SFieldsViewHint = 'Double-click: copy field value';
  SFieldsViewHint2 = 'Ctrl+Click: open link';
  SRelationsViewHint = 'Double-click: edit properties';
  SRelationsViewHint2 = 'Ctrl+Click: go to object';
  SOpenFileFilter = 'O2 file|*.o2|Any file|*.*';
  SImportFileFilter = 'O2 file|*.o2|XML file|*.xml';
  SImportSettingsFileFilter = 'O2 settings file|*.xml|Any file|*.*';
  SSaveFileFilter = 'O2 file|*.o2|Any file|*.*';
  SExportFileFilter = 'O2 file|*.o2|XML file|*.xml|iCalendar file|*.ics';
  SExportSettingsFileFilter = 'O2 settings file|*.xml|Any file|*.*';
  SDeleteObjectsQuery = 'Delete selected objects?';
  SRemoveFromMRUListQuery = 'File not found. Remove from the recent files list?';
  SSaveChangesQuery = 'Save changes?';
  SFileOverwriteQuery = 'File already exists. Overwrite?';
  SObjPropsDlgTitle = 'Object properties';
  SRulePropsDlgTitle = 'Rule properties';
  SDeleteRelationQuery = 'Delete selected relation?';
  SDeleteRuleQuery = 'Delete selected rules?';
  SInstallOnRemovableMediaPrompt = 'O2 can install itself onto a removable disk, USB stick or flash memory card. The media will need %.1f MB of free space.';
  SInstallOnRemovableMediaFolderPrompt = 'Select the installation folder:';
  SSettingsOverwriteQuery = 'Settings file already exists. Overwrite?';
  SReplaceTagDlgTitle = 'Replace tag';
  SReplaceTagDlgSearchLabel = '&Tag:';
  SReplaceTagDlgReplaceLabel = '&New tag:';
  SReplaceFieldNameDlgTitle = 'Replace field name';
  SReplaceFieldNameDlgSearchLabel = '&Field name:';
  SReplaceFieldNameDlgReplaceLabel = '&New field name:';
  SReplaceFieldValueDlgTitle = 'Replace field value';
  SReplaceFieldValueDlgSearchLabel = '&Field name:';
  SReplaceFieldValueDlgReplaceLabel = '&New field value:';
  SReplaceRoleDlgTitle = 'Replace role';
  SReplaceRoleDlgSearchLabel = '&Role:';
  SReplaceRoleDlgReplaceLabel = '&New role:';
  SCannotCheckForUpdates = 'Cannot check for updates.';
  SNoAvailableUpdates = 'No available updates.';
  SDownloadUpdatesQuery = 'Version %d.%d.%d is available. Download?';
  SApplyAtNextStartup = 'Change will take effect at next application startup.';
  SPrintPreviewZoom = 'Zoom';
  SHTMLExportStyle = 'Style';

{ Ciphers }

  SCipherNone     = '(none)';
  SCipherBlowfish = 'Blowfish [deprecated]';
  SCipherCast128  = 'Cast-128';
  SCipherCast256  = 'Cast-256';
  SCipherDES      = 'DES [deprecated]';
  SCipher3DES     = '3DES';
  SCipherIce      = 'Ice [deprecated]';
  SCipherThinIce  = 'Thin Ice [deprecated]';
  SCipherIce2     = 'Ice 2';
  SCipherIDEA     = 'IDEA';
  SCipherMARS     = 'MARS';
  SCipherMisty1   = 'Misty1 [deprecated]';
  SCipherRC2      = 'RC2 [deprecated]';
  SCipherRC4      = 'RC4 [deprecated]';
  SCipherRC5      = 'RC5';
  SCipherRC6      = 'RC6';
  SCipherRijndael = 'Rijndael (AES)';
  SCipherSerpent  = 'Serpent';
  SCipherTEA      = 'TEA [deprecated]';
  SCipherTwofish  = 'Twofish';

{ Hash algorithms }

  SHashNone      = '(none)';
  SHashHaval     = 'Haval';
  SHashMD4       = 'MD4';
  SHashMD5       = 'MD5 [deprecated]';
  SHashRipeMD128 = 'RipeMD-128';
  SHashRipeMD160 = 'RipeMD-160';
  SHashSHA1      = 'SHA-1 [deprecated]';
  SHashSHA256    = 'SHA-256';
  SHashSHA384    = 'SHA-384';
  SHashSHA512    = 'SHA-512';
  SHashTiger     = 'Tiger';

{ Rule types }

  SRuleHyperLink      = 'Internet link';
  SRuleEmail          = 'E-mail address';
  SRulePassword       = 'Password';
  SRuleExpirationDate = 'Expiration date';
  SRuleRecurrence     = 'Recurrence';
  SRuleHighlight      = 'Highlight';

{ Event filters }

  SEventAll         = 'All';
  SEventAllEvents   = 'All events';
  SEventCustom      = 'Custom';
  SEventToday       = 'Today';
  SEventTomorrow    = 'Tomorrow';
  SEventThisWeek    = 'This week';
  SEventThisMonth   = 'This month';
  SEventThisYear    = 'This year';
  SEventNext7days   = 'Next 7 days';
  SEventNext15days  = 'Next 15 days';
  SEventNext30days  = 'Next 30 days';
  SEventNext60days  = 'Next 60 days';
  SEventNext90days  = 'Next 90 days';
  SEventNext180days = 'Next 180 days';
  SEventNext365days = 'Next 365 days';

{ Date formats }

  SYMD = 'Year, month, day';
  SMDY = 'Month, day, year';
  SDMY = 'Day, month, year';

type
  TEventFilter = (
    efAll,
    efAllEvents,
    efCustom,
    efToday,
    efTomorrow,
    efThisWeek,
    efThisMonth,
    efThisYear,
    efNext7days,
    efNext15days,
    efNext30days,
    efNext60days,
    efNext90days,
    efNext180days,
    efNext365days);

  TDateFormat = (dfYMD, dfMDY, dfDMY);

  TCipherLookup = class(TLookupHelper)
  protected
    class procedure GetMapBounds(out LowerBound, UpperBound: Integer); override;
    class function GetMapEntry(Index: Integer): PLookupMapEntry; override;
  end;

  THashLookup = class(TLookupHelper)
  protected
    class procedure GetMapBounds(out LowerBound, UpperBound: Integer); override;
    class function GetMapEntry(Index: Integer): PLookupMapEntry; override;
  end;

  TRuleTypeLookup = class(TLookupHelper)
  protected
    class procedure GetMapBounds(out LowerBound, UpperBound: Integer); override;
    class function GetMapEntry(Index: Integer): PLookupMapEntry; override;
  end;

  TEventFilterLookup = class(TLookupHelper)
  protected
    class procedure GetMapBounds(out LowerBound, UpperBound: Integer); override;
    class function GetMapEntry(Index: Integer): PLookupMapEntry; override;
  end;

  TDateFormatLookup = class(TLookupHelper)
  protected
    class procedure GetMapBounds(out LowerBound, UpperBound: Integer); override;
    class function GetMapEntry(Index: Integer): PLookupMapEntry; override;
  public
    class procedure Select(const Combo: TCustomCombo; Value: string); overload;
    class function SelectedValue(const Combo: TCustomCombo): string; overload;
  end;

implementation

uses
  SysUtils;

const
  Ciphers: array[0..19] of TLookupMapEntry = (
    (Value: ocNone;     Item: SCipherNone),
    (Value: ocBlowfish; Item: SCipherBlowfish),
    (Value: ocCast128;  Item: SCipherCast128),
    (Value: ocCast256;  Item: SCipherCast256),
    (Value: ocDES;      Item: SCipherDES),
    (Value: oc3DES;     Item: SCipher3DES),
    (Value: ocIce;      Item: SCipherIce),
    (Value: ocThinIce;  Item: SCipherThinIce),
    (Value: ocIce2;     Item: SCipherIce2),
    (Value: ocIDEA;     Item: SCipherIDEA),
    (Value: ocMARS;     Item: SCipherMARS),
    (Value: ocMisty1;   Item: SCipherMisty1),
    (Value: ocRC2;      Item: SCipherRC2),
    (Value: ocRC4;      Item: SCipherRC4),
    (Value: ocRC5;      Item: SCipherRC5),
    (Value: ocRC6;      Item: SCipherRC6),
    (Value: ocRijndael; Item: SCipherRijndael),
    (Value: ocSerpent;  Item: SCipherSerpent),
    (Value: ocTEA;      Item: SCipherTEA),
    (Value: ocTwofish;  Item: SCipherTwofish));

  Hashes: array[0..9] of TLookupMapEntry = (
    (Value: ohHaval;     Item: SHashHaval),
    (Value: ohMD4;       Item: SHashMD4),
    (Value: ohMD5;       Item: SHashMD5),
    (Value: ohRipeMD128; Item: SHashRipeMD128),
    (Value: ohRipeMD160; Item: SHashRipeMD128),
    (Value: ohSHA1;      Item: SHashSHA1),
    (Value: ohSHA256;    Item: SHashSHA256),
    (Value: ohSHA384;    Item: SHashSHA384),
    (Value: ohSHA512;    Item: SHashSHA512),
    (Value: ohTiger;     Item: SHashTiger));

  RuleTypes: array[0..5] of TLookupMapEntry = (
    (Value: Integer(rtHyperLink);      Item: SRuleHyperLink),
    (Value: Integer(rtEmail);          Item: SRuleEmail),
    (Value: Integer(rtPassword);       Item: SRulePassword),
    (Value: Integer(rtExpirationDate); Item: SRuleExpirationDate),
    (Value: Integer(rtRecurrence);     Item: SRuleRecurrence),
    (Value: Integer(rtHighlight);      Item: SRuleHighlight));

  EventFilters: array[0..14] of TLookupMapEntry = (
    (Value: Integer(efAll);         Item: SEventAll),
    (Value: Integer(efAllEvents);   Item: SEventAllEvents),
    (Value: Integer(efCustom);      Item: SEventCustom),
    (Value: Integer(efToday);       Item: SEventToday),
    (Value: Integer(efTomorrow);    Item: SEventTomorrow),
    (Value: Integer(efThisWeek);    Item: SEventThisWeek),
    (Value: Integer(efThisMonth);   Item: SEventThisMonth),
    (Value: Integer(efThisYear);    Item: SEventThisYear),
    (Value: Integer(efNext7days);   Item: SEventNext7days),
    (Value: Integer(efNext15days);  Item: SEventNext15days),
    (Value: Integer(efNext30days);  Item: SEventNext30days),
    (Value: Integer(efNext60days);  Item: SEventNext60days),
    (Value: Integer(efNext90days);  Item: SEventNext90days),
    (Value: Integer(efNext180days); Item: SEventNext180days),
    (Value: Integer(efNext365days); Item: SEventNext365days));

  DateFormats: array[0..2] of TLookupMapEntry = (
    (Value: Integer(dfYMD); Item: SYMD),
    (Value: Integer(dfMDY); Item: SMDY),
    (Value: Integer(dfDMY); Item: SDMY));

{ TCipherLookup }

class procedure TCipherLookup.GetMapBounds(out LowerBound,
  UpperBound: Integer);
begin
  LowerBound := Low(Ciphers);
  UpperBound := High(Ciphers);
end;

class function TCipherLookup.GetMapEntry(Index: Integer): PLookupMapEntry;
begin
  Result := @Ciphers[Index];
end;

{ THashLookup }

class procedure THashLookup.GetMapBounds(out LowerBound,
  UpperBound: Integer);
begin
  LowerBound := Low(Hashes);
  UpperBound := High(Hashes);
end;

class function THashLookup.GetMapEntry(Index: Integer): PLookupMapEntry;
begin
  Result := @Hashes[Index];
end;

{ TRuleTypeLookup }

class procedure TRuleTypeLookup.GetMapBounds(out LowerBound,
  UpperBound: Integer);
begin
  LowerBound := Low(RuleTypes);
  UpperBound := High(RuleTypes);
end;

class function TRuleTypeLookup.GetMapEntry(Index: Integer): PLookupMapEntry;
begin
  Result := @RuleTypes[Index];
end;

{ TEventFilterLookup }

class procedure TEventFilterLookup.GetMapBounds(out LowerBound,
  UpperBound: Integer);
begin
  LowerBound := Low(EventFilters);
  UpperBound := High(EventFilters);
end;

class function TEventFilterLookup.GetMapEntry(Index: Integer): PLookupMapEntry;
begin
  Result := @EventFilters[Index];
end;

{ TDateFormatLookup }

class procedure TDateFormatLookup.GetMapBounds(out LowerBound,
  UpperBound: Integer);
begin
  LowerBound := Low(DateFormats);
  UpperBound := High(DateFormats);
end;

class function TDateFormatLookup.GetMapEntry(Index: Integer): PLookupMapEntry;
begin
  Result := @DateFormats[Index];
end;

class procedure TDateFormatLookup.Select(const Combo: TCustomCombo;
  Value: string);
var
  Y, M, D: Integer;
begin
  Y := Pos('y', LowerCase(Value));
  M := Pos('m', LowerCase(Value));
  D := Pos('d', LowerCase(Value));

  if (Y < M) and (M < D) then
    Select(Combo, Integer(dfYMD))
  else if (M < D) and (D < Y) then
    Select(Combo, Integer(dfMDY))
  else if (D < M) and (M < Y) then
    Select(Combo, Integer(dfDMY));
end;

class function TDateFormatLookup.SelectedValue(
  const Combo: TCustomCombo): string;
begin
  case SelectedValue(Combo, -1) of
    Integer(dfYMD): Result := 'yyyy/mm/dd';
    Integer(dfMDY): Result := 'mm/dd/yyyy';
    Integer(dfDMY): Result := 'dd/mm/yyyy';
  else
    Result := '';
  end;
end;

end.
