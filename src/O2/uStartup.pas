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

unit uStartup;

interface

uses
  uServices, uUtils;

var
  AppVersionInfo: TAppVersionInfo;
  AppFiles: IAppFiles;
  Storage: IStorage;
  PasswordScoreCache: IPasswordScoreCache;

procedure GetCommandLineParams(out OpenFileName, PortablePath: string);
procedure ConfigureServices;

implementation

uses
  Forms, ComCtrls, SysUtils, TypInfo, Variants, XMLDoc, XMLIntf, xmldom,
  msxmldom, JclFileUtils, uGlobal, uShellUtils, uAppFiles, uXmlStorage,
  uStorageUtils, uPasswordScoreCache, uMain;

function MigrateConfiguration(XmlStorage: IStorage;
  XML: IXMLDocument): IXMLDocument;
var
  ANode: IXMLNode;
  AValue: Variant;
  I, Count: Integer;
begin
  if not SameText(XML.DocumentElement.NodeName, 'Configuration') then
    Exit(XML);

  ANode := XML.DocumentElement.ChildNodes.FindNode('MRUList');
  if Assigned(ANode) then
  begin
    AValue := ANode.ChildValues['Count'];
    if not VarIsNull(AValue) then
    begin
      Count := StrToIntDef(AValue, 0);
      Storage.WriteInteger(IdMRUList, Count);

      for I := 0 to Count - 1 do
      begin
        AValue := ANode.ChildValues['Item' + IntToStr(I)];
        if not VarIsNull(AValue) then
          Storage.WriteString(Format(IdMRUListItemFmt, [I]), AValue);
      end;
    end;
  end;

  AValue := XML.DocumentElement.ChildValues['StayOnTop'];
  if not VarIsNull(AValue) then
    Storage.WriteBoolean(IdStayOnTop, StrToBoolDef(AValue, False));

  AValue := XML.DocumentElement.ChildValues['Transparency'];
  if not VarIsNull(AValue) then
    Storage.WriteInteger(IdTransparency, StrToIntDef(AValue, 0));

  AValue := XML.DocumentElement.ChildValues['AutoCheckForUpdates'];
  if not VarIsNull(AValue) then
    Storage.WriteBoolean(IdAutoCheckForUpdates,
      StrToBoolDef(AValue, True));

  AValue := XML.DocumentElement.ChildValues['ViewStyle'];
  if not VarIsNull(AValue) then
    WriteIntIdent(Storage, IdViewStyle, ViewStyles,
      GetEnumValue(TypeInfo(TViewStyle), AValue));

  AValue := XML.DocumentElement.ChildValues['SortColumn'];
  if not VarIsNull(AValue) then
    WriteIntIdent(Storage, IdSortColumn, SortColumns,
      GetEnumValue(TypeInfo(TObjectViewColumn), AValue));

  AValue := XML.DocumentElement.ChildValues['SortSign'];
  if not VarIsNull(AValue) then
    Storage.WriteBoolean(IdSortAscending, StrToIntDef(AValue, 1) > 0);

  ANode := XML.DocumentElement.ChildNodes.FindNode('Print');
  if Assigned(ANode) then
  begin
    AValue := ANode.ChildValues['IncludeTags'];
    if not VarIsNull(AValue) then
      Storage.WriteBoolean(IdPrintIncludeTags,
        StrToBoolDef(AValue, True));

    AValue := ANode.ChildValues['IncludeNotes'];
    if not VarIsNull(AValue) then
      Storage.WriteBoolean(IdPrintIncludeNotes,
        StrToBoolDef(AValue, True));

    AValue := ANode.ChildValues['IncludeRelations'];
    if not VarIsNull(AValue) then
      Storage.WriteBoolean(IdPrintIncludeRelations,
        StrToBoolDef(AValue, True));

    AValue := ANode.ChildValues['IncludePasswords'];
    if not VarIsNull(AValue) then
      Storage.WriteBoolean(IdPrintIncludePasswords,
        StrToBoolDef(AValue, True));
  end;

  Result := nil;
end;

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

procedure ConfigureServices;
var
  OpenFileName, AppPath, SettingsPath, LauncherPath, PortablePath, AppInfo,
  LanguageModule: string;
  ExeVersionInfo: TJclFileVersionInfo;
  AppInfoBuilder: TStringBuilder;
  I: Integer;
begin
  ExeVersionInfo := TJclFileVersionInfo.Create(Application.ExeName);
  try
    AppVersionInfo.AppName := ExeVersionInfo.ProductName;
    AppVersionInfo.DisplayVersion := ExeVersionInfo.BinFileVersion;
    VersionExtractFileInfo(ExeVersionInfo.FixedInfo,
      AppVersionInfo.Version.MajorVersion,
      AppVersionInfo.Version.MinorVersion,
      AppVersionInfo.Version.Release,
      AppVersionInfo.Version.Build);

    AppInfoBuilder := TStringBuilder.Create;
    try
      AppInfo := AppInfoBuilder
        .AppendLine('[' + PAF_FormatSection + ']')
        .AppendLine(PAF_FormatTypeId + '=' + PAF_FormatType)
        .AppendLine(PAF_FormatVersionId + '=' + PAF_FormatVersion)
        .AppendLine
        .AppendLine('[' + PAF_DetailsSection + ']')
        .AppendLine(PAF_AppNameId + '='
          + ExeVersionInfo.ProductName + ' Portable')
        .AppendLine(PAF_AppIDId + '=' + ExeVersionInfo.ProductName)
        .AppendLine(PAF_PublisherId + '=' + ExeVersionInfo.CompanyName)
        .AppendLine(PAF_HomepageId + '='
          + ExeVersionInfo.GetCustomFieldValue('Homepage'))
        .AppendLine(PAF_CategoryId + '=' + PAF_CategorySecurity)
        .AppendLine(PAF_DescriptionId + '=' + ExeVersionInfo.Comments)
        .AppendLine(PAF_LanguageId + '=' + PAF_LanguageMultilingual)
        .AppendLine
        .AppendLine('[' + PAF_LicenseSection + ']')
        .AppendLine(PAF_ShareableId + '=' + BoolToStr(True, True))
        .AppendLine(PAF_OpenSourceId + '=' + BoolToStr(True, True))
        .AppendLine(PAF_FreewareId + '=' + BoolToStr(True, True))
        .AppendLine(PAF_CommercialUseId + '=' + BoolToStr(True, True))
        .AppendLine
        .AppendLine('[' + PAF_VersionSection + ']')
        .AppendLine(PAF_PackageVersionId + '=' + ExeVersionInfo.BinFileVersion)
        .AppendLine(PAF_DisplayVersionId + '=' + ExeVersionInfo.BinFileVersion)
        .AppendLine
        .AppendLine('[' + PAF_ControlSection + ']')
        .AppendLine(PAF_IconsId + '=1')
        .AppendLine(PAF_StartId + '=' + LauncherFile)
        .AppendLine
        .AppendLine('[' + PAF_AssociationsSection + ']')
        .AppendLine(PAF_FileTypesId + '=' + DefaultFileExt)
        .AppendLine
        .AppendLine('[' + PAF_FileTypeIconsSection + ']')
        .AppendLine(DefaultFileExt + '=' + PAF_FileTypeIconCustom)
        .ToString;
    finally
      AppInfoBuilder.Free;
    end;
  finally
    ExeVersionInfo.Free;
  end;

  GetCommandLineParams(OpenFileName, PortablePath);

  if PortablePath <> '' then
  begin
    AppPath :=
      IncludeTrailingPathDelimiter(PortablePath) + PortableAppPath;
    if not GetSettingsOverride(Application.ExeName, SettingsPath) then
      SettingsPath :=
        IncludeTrailingPathDelimiter(PortablePath) + PortableSettingsPath;
    LauncherPath :=
      IncludeTrailingPathDelimiter(PortablePath) + PortableLauncherPath;
  end
  else
  begin
    AppPath := ExtractFilePath(Application.ExeName);
    if not GetSettingsOverride(Application.ExeName, SettingsPath) then
      SettingsPath :=
        IncludeTrailingPathDelimiter(TShellFolders.AppData) + LocalSettingsDir;
    LauncherPath := AppPath;
  end;

  AppFiles := TAppFiles.Create
    .Add(IdAppExe, ExtractFileName(Application.ExeName), AppPath,
      PortableAppPath)
    .AddInMemory(IdAppInfo, AppInfoFile, AppPath, PortableAppInfoPath, AppInfo)
    .Add(IdAppIcon, AppIconFile, AppPath, PortableAppInfoPath)
    .Add(IdAppIcon16, AppIcon16File, AppPath, PortableAppInfoPath)
    .Add(IdAppIcon32, AppIcon32File, AppPath, PortableAppInfoPath)
    .Add(IdFileTypeIcon, FileTypeIconFile, AppPath, PortableFileTypeIconsPath)
    .Add(IdFileTypeIcon16, FileTypeIcon16File, AppPath,
      PortableFileTypeIconsPath)
    .Add(IdFileTypeIcon32, FileTypeIcon32File, AppPath,
      PortableFileTypeIconsPath)
    .Add(IdLauncher, LauncherFile, LauncherPath, PortableLauncherPath)
    .Add(IdSettings, SettingsFile, SettingsPath, PortableSettingsPath,
      SSettingsOverwriteQuery)
    .Add(IdHelp, HTMLHelpFile, AppPath, PortableLauncherPath)
    .Add(IdLicense, LicenseFile, AppPath, PortableAppPath)
    .Add(IdReadMe, ReadMeFile, AppPath, PortableAppPath);

  for I := Low(Languages) to High(Languages) do
  begin
    LanguageModule := ChangeFileExt(AppFiles.FullPath[IdAppExe],
      '.' + Languages[I].Language);
    if FileExists(LanguageModule) then
      TAppFiles(AppFiles).Add(IdResourceModule + Languages[I].Language,
        ExtractFileName(LanguageModule), ExtractFilePath(LanguageModule),
        PortableAppPath);
  end;

  Storage := TXmlStorage.Create(MigrateConfiguration);
  PasswordScoreCache := TPasswordScoreCache.Create;
end;

end.
