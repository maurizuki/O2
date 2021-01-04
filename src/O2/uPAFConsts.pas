{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2015 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

{ ******************************** }
{                                  }
{   PortableApps.com Format        }
{                                  }
{   Copyright (c) John T. Haller   }
{                                  }
{   http://portableapps.com        }
{                                  }
{ ******************************** }

unit uPAFConsts;

interface

const

{ PortableApps.com Format current version }

  PAF_FormatType = 'PortableApps.comFormat';
  PAF_FormatVersion = '3.0';

{ PortableApps.com Format application categories }

  PAF_CategoryAccessibility = 'Accessibility';
  PAF_CategoryDevelopment = 'Development';
  PAF_CategoryEducation = 'Education';
  PAF_CategoryGames = 'Games';
  PAF_CategoryGraphicsAndPictures = 'Graphics & Pictures';
  PAF_CategoryInternet = 'Internet';
  PAF_CategoryMusicAndVideo = 'Music & Video';
  PAF_CategoryOffice = 'Office';
  PAF_CategorySecurity = 'Security';
  PAF_CategoryUtilities = 'Utilities';

{ PortableApps.com Format application languages }

  PAF_LanguageMultilingual = 'Multilingual';

  PAF_LanguageAfrikaans = 'Afrikaans';
  PAF_LanguageAlbanian = 'Albanian';
  PAF_LanguageArabic = 'Arabic';
  PAF_LanguageArmenian = 'Armenian';
  PAF_LanguageBasque = 'Basque';
  PAF_LanguageBelarusian = 'Belarusian';
  PAF_LanguageBosnian = 'Bosnian';
  PAF_LanguageBreton = 'Breton';
  PAF_LanguageBulgarian = 'Bulgarian';
  PAF_LanguageCatalan = 'Catalan';
  PAF_LanguageCibemba = 'Cibemba';
  PAF_LanguageCroatian = 'Croatian';
  PAF_LanguageCzech = 'Czech';
  PAF_LanguageDanish = 'Danish';
  PAF_LanguageDutch = 'Dutch';
  PAF_LanguageEfik = 'Efik';
  PAF_LanguageEnglish = 'English';
  PAF_LanguageEnglishGB = 'EnglishGB';
  PAF_LanguageEsperanto = 'Esperanto';
  PAF_LanguageEstonian = 'Estonian';
  PAF_LanguageFarsi = 'Farsi';
  PAF_LanguageFinnish = 'Finnish';
  PAF_LanguageFrench = 'French';
  PAF_LanguageGalician = 'Galician';
  PAF_LanguageGeorgian = 'Georgian';
  PAF_LanguageGerman = 'German';
  PAF_LanguageGreek = 'Greek';
  PAF_LanguageHebrew = 'Hebrew';
  PAF_LanguageHungarian = 'Hungarian';
  PAF_LanguageIcelandic = 'Icelandic';
  PAF_LanguageIgbo = 'Igbo';
  PAF_LanguageIndonesian = 'Indonesian';
  PAF_LanguageIrish = 'Irish';
  PAF_LanguageItalian = 'Italian';
  PAF_LanguageJapanese = 'Japanese';
  PAF_LanguageKhmer = 'Khmer';
  PAF_LanguageKorean = 'Korean';
  PAF_LanguageKurdish = 'Kurdish';
  PAF_LanguageLatvian = 'Latvian';
  PAF_LanguageLithuanian = 'Lithuanian';
  PAF_LanguageLuxembourgish = 'Luxembourgish';
  PAF_LanguageMacedonian = 'Macedonian';
  PAF_LanguageMalagasy = 'Malagasy';
  PAF_LanguageMalay = 'Malay';
  PAF_LanguageMongolian = 'Mongolian';
  PAF_LanguageNorwegian = 'Norwegian';
  PAF_LanguageNorwegianNynorsk = 'NorwegianNynorsk';
  PAF_LanguagePashto = 'Pashto';
  PAF_LanguagePolish = 'Polish';
  PAF_LanguagePortuguese = 'Portuguese';
  PAF_LanguagePortugueseBR = 'PortugueseBR';
  PAF_LanguageRomanian = 'Romanian';
  PAF_LanguageRussian = 'Russian';
  PAF_LanguageSerbian = 'Serbian';
  PAF_LanguageSerbianLatin = 'SerbianLatin';
  PAF_LanguageSimpChinese = 'SimpChinese';
  PAF_LanguageSlovak = 'Slovak';
  PAF_LanguageSlovenian = 'Slovenian';
  PAF_LanguageSpanish = 'Spanish';
  PAF_LanguageSpanishInternational = 'SpanishInternational';
  PAF_LanguageSwahili = 'Swahili';
  PAF_LanguageSwedish = 'Swedish';
  PAF_LanguageThai = 'Thai';
  PAF_LanguageTradChinese = 'TradChinese';
  PAF_LanguageTurkish = 'Turkish';
  PAF_LanguageUkrainian = 'Ukrainian';
  PAF_LanguageUzbek = 'Uzbek';
  PAF_LanguageValencian = 'Valencian';
  PAF_LanguageVietnamese = 'Vietnamese';
  PAF_LanguageWelsh = 'Welsh';
  PAF_LanguageYoruba = 'Yoruba';

{ PortableApps.com Format file type icons }

  PAF_FileTypeIconApp = 'app';
  PAF_FileTypeIconCustom = 'custom';

  PAF_FileTypeIconArchive = 'archive';
  PAF_FileTypeIconAudio = 'audio';
  PAF_FileTypeIconCalendar = 'calendar';
  PAF_FileTypeIconChart = 'chart';
  PAF_FileTypeIconCode = 'code';
  PAF_FileTypeIconContact = 'contact';
  PAF_FileTypeIconDatabase = 'database';
  PAF_FileTypeIconDiskimage = 'diskimage';
  PAF_FileTypeIconDrawing = 'drawing';
  PAF_FileTypeIconDocument = 'document';
  PAF_FileTypeIconEbook = 'ebook';
  PAF_FileTypeIconFont = 'font';
  PAF_FileTypeIconImage = 'image';
  PAF_FileTypeIconJava = 'java';
  PAF_FileTypeIconPresentation = 'presentation';
  PAF_FileTypeIconSpreadsheet = 'spreadsheet';
  PAF_FileTypeIconText = 'text';
  PAF_FileTypeIconTorrent = 'torrent';
  PAF_FileTypeIconVideo = 'video';
  PAF_FileTypeIconWebpage = 'webpage';

{ PortableApps.com Format sections }

  PAF_FormatSection = 'Format';
  PAF_DetailsSection = 'Details';
  PAF_LicenseSection = 'License';
  PAF_VersionSection = 'Version';
  PAF_SpecialPathsSection = 'SpecialPaths';
  PAF_DependenciesSection = 'Dependencies';
  PAF_ControlSection = 'Control';
  PAF_AssociationsSection = 'Associations';
  PAF_FileTypeIconsSection = 'FileTypeIcons';

{ PortableApps.com Format "Format" section IDs }

  PAF_FormatTypeId = 'Type';
  PAF_FormatVersionId = 'Version';

{ PortableApps.com Format "Details" section IDs }

  PAF_AppNameId = 'Name';
  PAF_AppIDId = 'AppID';
  PAF_PublisherId = 'Publisher';
  PAF_HomepageId = 'Homepage';
  PAF_CategoryId = 'Category';
  PAF_DescriptionId = 'Description';
  PAF_LanguageId = 'Language';
  PAF_TrademarksId = 'Trademarks';
  PAF_InstallTypeId = 'InstallType';

{ PortableApps.com Format "License" section IDs }

  PAF_ShareableId = 'Shareable';
  PAF_OpenSourceId = 'OpenSource';
  PAF_FreewareId = 'Freeware';
  PAF_CommercialUseId = 'CommercialUse';
  PAF_EULAVersionId = 'EULAVersion';

{ PortableApps.com Format "Version" section IDs }

  PAF_PackageVersionId = 'PackageVersion';
  PAF_DisplayVersionId = 'DisplayVersion';

{ PortableApps.com Format "SpecialPaths" section IDs }

  PAF_PluginsId = 'Plugins';

{ PortableApps.com Format "Dependencies" section IDs }

  PAF_UsesJavaId = 'UsesJava';
  PAF_UsesDotNetVersionId = 'UsesDotNetVersion';

{ PortableApps.com Format "Control" section IDs }

  PAF_IconsId = 'Icons';
  PAF_StartId = 'Start';
  PAF_ExtractIconId = 'ExtractIcon';

{ PortableApps.com Format "Associations" section IDs }

  PAF_FileTypesId = 'FileTypes';
  PAF_FileTypeCommandLineId = 'FileTypeCommandLine';
  PAF_ProtocolsId = 'Protocols';
  PAF_ProtocolCommandLineId = 'ProtocolCommandLine';
  PAF_SendToId = 'SendTo';
  PAF_SendToCommandLineId = 'SendToCommandLine';
  PAF_ShellId = 'Shell';
  PAF_ShellCommandId = 'ShellCommand';

{ PortableApps.com Format "FileTypeIcons" section IDs }

  PAF_AllOtherIconsId = 'AllOtherIcons';

implementation

end.
