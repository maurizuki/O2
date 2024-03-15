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
  Spring.Container, uServices, uUtils;

var
  ServiceContainer: TContainer;

procedure GetCommandLineParams(out OpenFileName, PortablePath: string);
procedure ConfigureServices;

implementation

uses
  Forms, ComCtrls, SysUtils, TypInfo, Variants, XMLDoc, XMLIntf, xmldom,
  msxmldom, JclFileUtils, uMain, uGlobal, uShellUtils, uAppFiles, uXmlStorage,
  uStorageUtils, uPasswordScoreCache, uO2File, uO2Objects, uO2Relations,
  uO2Rules, uFilePropsModel, uEncryptionPropsModel, uObjectModels,
  uRelationModels, uRuleModels, uO2ImportExport, uXmlImportExport,
  uiCalendarExport, uHTMLExportModel, uPrintModel;

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
      XmlStorage.WriteInteger(IdMRUList, Count);

      for I := 0 to Count - 1 do
      begin
        AValue := ANode.ChildValues['Item' + IntToStr(I)];
        if not VarIsNull(AValue) then
          XmlStorage.WriteString(Format(IdMRUListItemFmt, [I]), AValue);
      end;
    end;
  end;

  AValue := XML.DocumentElement.ChildValues['StayOnTop'];
  if not VarIsNull(AValue) then
    XmlStorage.WriteBoolean(IdStayOnTop, StrToBoolDef(AValue, False));

  AValue := XML.DocumentElement.ChildValues['Transparency'];
  if not VarIsNull(AValue) then
    XmlStorage.WriteInteger(IdTransparency, StrToIntDef(AValue, 0));

  AValue := XML.DocumentElement.ChildValues['AutoCheckForUpdates'];
  if not VarIsNull(AValue) then
    XmlStorage.WriteBoolean(IdAutoCheckForUpdates,
      StrToBoolDef(AValue, True));

  AValue := XML.DocumentElement.ChildValues['ViewStyle'];
  if not VarIsNull(AValue) then
    WriteIntIdent(XmlStorage, IdViewStyle, ViewStyles,
      GetEnumValue(TypeInfo(TViewStyle), AValue));

  AValue := XML.DocumentElement.ChildValues['SortColumn'];
  if not VarIsNull(AValue) then
    WriteIntIdent(XmlStorage, IdSortKind, SortKinds,
      GetEnumValue(TypeInfo(TObjectSortKind), AValue));

  AValue := XML.DocumentElement.ChildValues['SortSign'];
  if not VarIsNull(AValue) then
    XmlStorage.WriteBoolean(IdSortAscending, StrToIntDef(AValue, 1) > 0);

  ANode := XML.DocumentElement.ChildNodes.FindNode('Print');
  if Assigned(ANode) then
  begin
    AValue := ANode.ChildValues['IncludeTags'];
    if not VarIsNull(AValue) then
      XmlStorage.WriteBoolean(IdPrintIncludeTags,
        StrToBoolDef(AValue, True));

    AValue := ANode.ChildValues['IncludeNotes'];
    if not VarIsNull(AValue) then
      XmlStorage.WriteBoolean(IdPrintIncludeNotes,
        StrToBoolDef(AValue, True));

    AValue := ANode.ChildValues['IncludeRelations'];
    if not VarIsNull(AValue) then
      XmlStorage.WriteBoolean(IdPrintIncludeRelations,
        StrToBoolDef(AValue, True));

    AValue := ANode.ChildValues['IncludePasswords'];
    if not VarIsNull(AValue) then
      XmlStorage.WriteBoolean(IdPrintIncludePasswords,
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
  ExeVersionInfo: TJclFileVersionInfo;
  AppVersionInfo: TAppVersionInfo;
  AppInfoBuilder: TStringBuilder;
  AppInfoBytes: TBytes;
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
      AppInfoBytes := TEncoding.UTF8.GetBytes(AppInfoBuilder
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
        .ToString);
    finally
      AppInfoBuilder.Free;
    end;
  finally
    ExeVersionInfo.Free;
  end;

  ServiceContainer
    .RegisterInstance<TAppVersionInfo>(AppVersionInfo)
    .AsSingleton;

  ServiceContainer
    .RegisterType<TAppFiles>(
      function: TAppFiles
      var
        OpenFileName, AppPath, SettingsPath, LauncherPath, PortablePath: string;
        LanguageModule: string;
        I: Integer;
      begin
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
            SettingsPath := IncludeTrailingPathDelimiter(TShellFolders.AppData)
              + LocalSettingsDir;
          LauncherPath := AppPath;
        end;

        Result := TAppFiles.Create
          .Add(IdAppExe, ExtractFileName(Application.ExeName), AppPath,
            PortableAppPath)
          .Add(IdAppInfo, AppInfoFile, AppPath, PortableAppInfoPath,
            AppInfoBytes)
          .Add(IdAppIcon, AppIconFile, AppPath, PortableAppInfoPath)
          .Add(IdAppIcon16, AppIcon16File, AppPath, PortableAppInfoPath)
          .Add(IdAppIcon32, AppIcon32File, AppPath, PortableAppInfoPath)
          .Add(IdFileTypeIcon, FileTypeIconFile, AppPath,
            PortableFileTypeIconsPath)
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
          LanguageModule := ChangeFileExt(Result.FullPaths[IdAppExe],
            '.' + Languages[I].Language);
          if FileExists(LanguageModule) then
            Result.Add(IdResourceModule + Languages[I].Language,
              ExtractFileName(LanguageModule), ExtractFilePath(LanguageModule),
              PortableAppPath);
        end;
      end)
    .Implements<IAppFiles>
    .AsSingleton;

  ServiceContainer
    .RegisterInstance<IStorage>(TXmlStorage.Create(MigrateConfiguration))
    .AsSingleton;

  ServiceContainer
    .RegisterType<TPasswordScoreCache>
    .Implements<IPasswordScoreCache>
    .AsSingleton;

  ServiceContainer
    .RegisterType<IPasswordProvider>(
      function: IPasswordProvider
      begin
        Result := MainForm;
      end)
    .AsSingleton;

  ServiceContainer
    .RegisterType<TO2File>(
      function: TO2File
      begin
        Result := MainForm.O2File;
      end)
    .AsTransient;

  ServiceContainer
    .RegisterType<IEnumerable<TO2Object>>(
      function: IEnumerable<TO2Object>
      begin
        Result := MainForm.SelectedObjects;
      end)
    .AsSingleton;

  ServiceContainer
    .RegisterType<TO2Object>(
      function: TO2Object
      begin
        Result := MainForm.SelectedObject;
      end)
    .AsTransient;

  ServiceContainer
    .RegisterType<TO2Relation>(
      function: TO2Relation
      begin
        Result := MainForm.SelectedRelation;
      end)
    .AsTransient;

  ServiceContainer
    .RegisterType<TO2Rule>(
      function: TO2Rule
      begin
        Result := MainForm.SelectedRule;
      end)
    .AsTransient;

  ServiceContainer
    .RegisterType<TFilePropsModel>
    .Implements<IFileProps>
    .AsTransient;

  ServiceContainer
    .RegisterType<TEncryptionPropsModel>
    .Implements<IEncryptionProps>
    .AsTransient;

  {$REGION 'IObjectProps'}

  ServiceContainer
    .RegisterType<TNewObjectModel>(NewObjectService)
    .Implements<IObjectProps>
    .AsTransient;

  ServiceContainer
    .RegisterType<TDuplicateObjectModel>(DuplicateObjectService)
    .Implements<IObjectProps>
    .AsTransient;

  ServiceContainer
    .RegisterType<TEditObjectModel>(EditObjectService)
    .Implements<IObjectProps>
    .AsTransient;

  {$ENDREGION}

  {$REGION 'IRelationProps'}

  ServiceContainer
    .RegisterType<TNewRelationModel>(NewRelationService)
    .Implements<IRelationProps>
    .AsTransient;

  ServiceContainer
    .RegisterType<TEditRelationModel>(EditRelationService)
    .Implements<IRelationProps>
    .AsTransient;

  {$ENDREGION}

  {$REGION 'IRuleProps'}

  ServiceContainer
    .RegisterType<TNewRuleModel>(NewRuleService)
    .Implements<IRuleProps>
    .AsTransient;

  ServiceContainer
    .RegisterType<TDuplicateRuleModel>(DuplicateRuleService)
    .Implements<IRuleProps>
    .AsTransient;

  ServiceContainer
    .RegisterType<TEditRuleModel>(EditRuleService)
    .Implements<IRuleProps>
    .AsTransient;

  {$ENDREGION}

  {$REGION 'IFileOperation'}

  ServiceContainer
    .RegisterType<TO2Import>(ImportFromO2FileService)
    .Implements<IFileOperation>
    .AsTransient;

  ServiceContainer
    .RegisterType<TXmlImport>(ImportFromXmlFileService)
    .Implements<IFileOperation>
    .AsTransient;

  ServiceContainer
    .RegisterType<TO2Export>(ExportToO2FileService)
    .Implements<IFileOperation>
    .AsTransient;

  ServiceContainer
    .RegisterType<TXmlExport>(ExportToXmlFileService)
    .Implements<IFileOperation>
    .AsTransient;

  ServiceContainer
    .RegisterType<TiCalendarExport>(ExportToIcsFileService)
    .Implements<IFileOperation>
    .AsTransient;

  {$ENDREGION}

  ServiceContainer
    .RegisterType<THTMLExportModel>
    .Implements<IHTMLExport>
    .AsTransient;

  ServiceContainer
    .RegisterType<TPrintModel>
    .Implements<IPrint>
    .AsTransient;

  { TODO : Register IReplaceOperation }

  ServiceContainer.Build;
end;

initialization
  ServiceContainer := TContainer.Create;

finalization
  ServiceContainer.Free;

end.
