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

unit uGlobal;

interface

{$R Dictionaries.res}

uses
  Windows, Classes, Graphics, ComCtrls, uO2Defs, uO2Rules;

{$I PAFConsts.inc}

const
  MinPasswordLength = 5;

  DefaultFileExt = 'o2';

{ Service names }

  NewObjectService = 'NewObject';
  DuplicateObjectService = 'DuplicateObject';
  EditObjectService = 'EditObject';
  NewRelationService = 'NewRelation';
  EditRelationService = 'EditRelation';
  NewRuleService = 'NewRule';
  DuplicateRuleService = 'DuplicateRule';
  EditRuleService = 'EditRule';
  ImportFromO2FileService = 'ImportFromO2File';
  ImportFromXmlFileService = 'ImportFromXmlFile';
  ExportToO2FileService = 'ExportToO2File';
  ExportToXmlFileService = 'ExportToXmlFile';
  ExportToIcsFileService = 'ExportToIcsFile';
  ReplaceTagService = 'ReplaceTag';
  ReplaceFieldNameService = 'ReplaceFieldName';
  ReplaceFieldValueService = 'ReplaceFieldValue';
  ReplaceRoleService = 'ReplaceRole';

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
  WebView2LoaderFile = 'WebView2Loader.dll';

  LocalSettingsPath = 'O2';
  LocalWebDataPath = 'O2\WebView2';
  LocalHTMLStylesPath = 'Styles';

  PortableLauncherPath = '';
  PortableAppPath = 'App\O2';
  PortableAppInfoPath = 'App\AppInfo';
  PortableSettingsPath = 'Data\Settings';
  PortableFileTypeIconsPath = 'App\AppInfo\FileTypeIcons';
  PortableWebDataPath = 'Data\WebView2';
  PortableHTMLStylesPath = 'App\O2\Styles';

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
  IdWebView2Loader = 'WebView2Loader';
  IdHTMLStyle = 'IdHTMLStyle';

{ Configuration file IDs }

  IdMRUList = 'Application.MRU';
  IdMRUListItemFmt = IdMRUList + '.%d';
  IdMRUListItemCntFmt = IdMRUList + '.%d.Count';
  IdStayOnTop = 'Application.StayOnTop';
  IdTransparency = 'Application.Transparency';
  IdTransparencyOnlyIfDeactivated = 'Application.Transparency.OnlyIfDeactivated';
  IdAutoCheckForUpdates = 'Application.AutoCheckForUpdates';
  IdLastCheckForUpdates = 'Application.AutoCheckForUpdates.LastCheck';
  IdViewStyle = 'MainWindow.ObjectView.ViewStyle';
  IdSortKind = 'MainWindow.ObjectView.Sort.Column';
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

{ Password score colors }

  PasswordScoreColors: array [0..4] of TColor = (
    $00241CED, $00277FFF, $000EC9FF, $00E8A200, $004CB122);

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

const
  RuleTypes: array[TO2RuleType] of string = (
    '',
    SRuleHyperLink,
    SRuleEmail,
    SRulePassword,
    SRuleExpirationDate,
    SRuleRecurrence,
    SRuleHighlight);

  Ciphers: array[ocNone..ocTwofish] of string = (
    SCipherNone,
    SCipherBlowfish,
    SCipherCast128,
    SCipherCast256,
    SCipherDES,
    SCipher3DES,
    SCipherIce,
    SCipherThinIce,
    SCipherIce2,
    SCipherIDEA,
    SCipherMARS,
    SCipherMisty1,
    SCipherRC2,
    SCipherRC4,
    SCipherRC5,
    SCipherRC6,
    SCipherRijndael,
    SCipherSerpent,
    SCipherTEA,
    SCipherTwofish);

  Hashes: array[ohNone..ohTiger] of string = (
    SHashNone,
    SHashHaval,
    SHashMD4,
    SHashMD5,
    SHashRipeMD128,
    SHashRipeMD160,
    SHashSHA1,
    SHashSHA256,
    SHashSHA384,
    SHashSHA512,
    SHashTiger);

type
  TObjectSortKind = (osName, osTags, osNextEvent);

const
  SortKinds: array[0..2] of TIdentMapEntry = (
    (Value: Integer(osName);      Name: 'Name'),
    (Value: Integer(osTags);      Name: 'Tags'),
    (Value: Integer(osNextEvent); Name: 'NextEvent'));

  ViewStyles: array[0..3] of TIdentMapEntry = (
    (Value: Integer(vsIcon);      Name: 'Icons'),
    (Value: Integer(vsSmallIcon); Name: 'SmallIcons'),
    (Value: Integer(vsList);      Name: 'List'),
    (Value: Integer(vsReport);    Name: 'Report'));

var
  OpenFileName, PortablePath, WebDataPath: string;

implementation

uses
  SysUtils, uShellUtils, uUtils;

procedure GetCommandLineParams(out OpenFileName, PortablePath: string);
var
  I: Integer;
begin
  OpenFileName := '';
  PortablePath := '';
  I := 1;
  while I <= ParamCount do
    if SameText(ParamStr(I), 'portable') and (ParamStr(I + 1) <> '') then
    begin
      PortablePath := ParamStr(I + 1);
      Inc(I, 2);
    end
    else
    begin
      OpenFileName := ParamStr(I);
      Inc(I);
    end;
end;

initialization
  GetCommandLineParams(OpenFileName, PortablePath);

  if PortablePath <> '' then
    WebDataPath := CombinePath(PortablePath, PortableWebDataPath)
  else
    WebDataPath := CombinePath(TShellFolders.AppData, LocalWebDataPath);

end.
