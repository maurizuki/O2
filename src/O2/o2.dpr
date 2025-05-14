{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2025 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

program o2;

uses
  Forms,
  DCPbase64 in '..\DCPcrypt2\DCPbase64.pas',
  DCPblockciphers in '..\DCPcrypt2\DCPblockciphers.pas',
  DCPconst in '..\DCPcrypt2\DCPconst.pas',
  DCPcrypt2 in '..\DCPcrypt2\DCPcrypt2.pas',
  DCPblowfish in '..\DCPcrypt2\Ciphers\DCPblowfish.pas',
  DCPcast128 in '..\DCPcrypt2\Ciphers\DCPcast128.pas',
  DCPcast256 in '..\DCPcrypt2\Ciphers\DCPcast256.pas',
  DCPdes in '..\DCPcrypt2\Ciphers\DCPdes.pas',
  DCPice in '..\DCPcrypt2\Ciphers\DCPice.pas',
  DCPidea in '..\DCPcrypt2\Ciphers\DCPidea.pas',
  DCPmars in '..\DCPcrypt2\Ciphers\DCPmars.pas',
  DCPmisty1 in '..\DCPcrypt2\Ciphers\DCPmisty1.pas',
  DCPrc2 in '..\DCPcrypt2\Ciphers\DCPrc2.pas',
  DCPrc4 in '..\DCPcrypt2\Ciphers\DCPrc4.pas',
  DCPrc5 in '..\DCPcrypt2\Ciphers\DCPrc5.pas',
  DCPrc6 in '..\DCPcrypt2\Ciphers\DCPrc6.pas',
  DCPrijndael in '..\DCPcrypt2\Ciphers\DCPrijndael.pas',
  DCPserpent in '..\DCPcrypt2\Ciphers\DCPserpent.pas',
  DCPtea in '..\DCPcrypt2\Ciphers\DCPtea.pas',
  DCPtwofish in '..\DCPcrypt2\Ciphers\DCPtwofish.pas',
  DCPhaval in '..\DCPcrypt2\Hashes\DCPhaval.pas',
  DCPmd4 in '..\DCPcrypt2\Hashes\DCPmd4.pas',
  DCPmd5 in '..\DCPcrypt2\Hashes\DCPmd5.pas',
  DCPripemd128 in '..\DCPcrypt2\Hashes\DCPripemd128.pas',
  DCPripemd160 in '..\DCPcrypt2\Hashes\DCPripemd160.pas',
  DCPsha1 in '..\DCPcrypt2\Hashes\DCPsha1.pas',
  DCPsha256 in '..\DCPcrypt2\Hashes\DCPsha256.pas',
  DCPsha512 in '..\DCPcrypt2\Hashes\DCPsha512.pas',
  DCPtiger in '..\DCPcrypt2\Hashes\DCPtiger.pas',
  MarkdownCommonMark in '..\Markdown\MarkdownCommonMark.pas',
  MarkdownHTMLEntities in '..\Markdown\MarkdownHTMLEntities.pas',
  MarkdownProcessor in '..\Markdown\MarkdownProcessor.pas',
  MarkdownUnicodeUtils in '..\Markdown\MarkdownUnicodeUtils.pas',
  Zxcvbn.DateMatcher in '..\Zxcvbn\src\Zxcvbn.DateMatcher.pas',
  Zxcvbn.DefaultMatcherFactory in '..\Zxcvbn\src\Zxcvbn.DefaultMatcherFactory.pas',
  Zxcvbn.DictionaryMatcher in '..\Zxcvbn\src\Zxcvbn.DictionaryMatcher.pas',
  Zxcvbn.L33tMatcher in '..\Zxcvbn\src\Zxcvbn.L33tMatcher.pas',
  Zxcvbn.Matcher in '..\Zxcvbn\src\Zxcvbn.Matcher.pas',
  Zxcvbn.MatcherFactory in '..\Zxcvbn\src\Zxcvbn.MatcherFactory.pas',
  Zxcvbn in '..\Zxcvbn\src\Zxcvbn.pas',
  Zxcvbn.PasswordScoring in '..\Zxcvbn\src\Zxcvbn.PasswordScoring.pas',
  Zxcvbn.RegexMatcher in '..\Zxcvbn\src\Zxcvbn.RegexMatcher.pas',
  Zxcvbn.RepeatMatcher in '..\Zxcvbn\src\Zxcvbn.RepeatMatcher.pas',
  Zxcvbn.Result in '..\Zxcvbn\src\Zxcvbn.Result.pas',
  Zxcvbn.SequenceMatcher in '..\Zxcvbn\src\Zxcvbn.SequenceMatcher.pas',
  Zxcvbn.SpatialMatcher in '..\Zxcvbn\src\Zxcvbn.SpatialMatcher.pas',
  Zxcvbn.Utility in '..\Zxcvbn\src\Zxcvbn.Utility.pas',
  SZCRC32 in 'Utils\SZCRC32.pas',
  uO2Classes in 'DataModel\uO2Classes.pas',
  uO2Defs in 'DataModel\uO2Defs.pas',
  uO2File in 'DataModel\uO2File.pas',
  uO2Objects in 'DataModel\uO2Objects.pas',
  uO2Relations in 'DataModel\uO2Relations.pas',
  uO2Rules in 'DataModel\uO2Rules.pas',
  uXmlSerialization in 'Utils\uXmlSerialization.pas',
  uAppFiles in 'Utils\uAppFiles.pas',
  uCtrlHelpers in 'Utils\uCtrlHelpers.pas',
  uHTMLHelper in 'Utils\uHTMLHelper.pas',
  uMRUList in 'Utils\uMRUList.pas',
  uPasswordScoreCache in 'Utils\uPasswordScoreCache.pas',
  uShellUtils in 'Utils\uShellUtils.pas',
  uStorageUtils in 'Utils\uStorageUtils.pas',
  uUtils in 'Utils\uUtils.pas',
  uXmlStorage in 'Utils\uXmlStorage.pas',
  uFileOperation in 'FileOps\uFileOperation.pas',
  uiCalendarExport in 'FileOps\uiCalendarExport.pas',
  uO2ImportExport in 'FileOps\uO2ImportExport.pas',
  uXmlImportExport in 'FileOps\uXmlImportExport.pas',
  uO2ObjectsUtils in 'Utils\uO2ObjectsUtils.pas',
  uO2RulesUtils in 'Utils\uO2RulesUtils.pas',
  uEncryptionPropsModel in 'ViewModels\uEncryptionPropsModel.pas',
  uEventFilters in 'ViewModels\uEventFilters.pas',
  uFileManager in 'ViewModels\uFileManager.pas',
  uFilePropsModel in 'ViewModels\uFilePropsModel.pas',
  uHTMLExportModel in 'ViewModels\uHTMLExportModel.pas',
  uObjectModels in 'ViewModels\uObjectModels.pas',
  uPasswordStrengthInfo in 'ViewModels\uPasswordStrengthInfo.pas',
  uPrintModel in 'ViewModels\uPrintModel.pas',
  uRelationModels in 'ViewModels\uRelationModels.pas',
  uReplaceOperations in 'ViewModels\uReplaceOperations.pas',
  uRuleModels in 'ViewModels\uRuleModels.pas',
  uMain in 'uMain.pas' {MainForm},
  uGlobal in 'uGlobal.pas',
  uObjPropsDlg in 'uObjPropsDlg.pas' {ObjPropsDlg},
  uRelationPropsDlg in 'uRelationPropsDlg.pas' {RelationPropsDlg},
  uRulePropsDlg in 'uRulePropsDlg.pas' {RulePropsDlg},
  uGetPassword in 'uGetPassword.pas' {GetPasswordDlg},
  uSetPassword in 'uSetPassword.pas' {SetPasswordDlg},
  uAbout in 'uAbout.pas' {AboutForm},
  uFilePropsDlg in 'uFilePropsDlg.pas' {FilePropsDlg},
  uReplaceDlg in 'uReplaceDlg.pas' {ReplaceDlg},
  uPrintPreview in 'uPrintPreview.pas' {PrintPreview},
  uHTMLExport in 'uHTMLExport.pas' {HTMLExport},
  uRTFViewer in 'uRTFViewer.pas' {RTFViewer},
  uPasswordStrengthIndicator in 'uPasswordStrengthIndicator.pas' {PasswordStrengthIndicator: TFrame},
  uServices in 'uServices.pas',
  uStartup in 'uStartup.pas';

{$R *.res}

begin
  Randomize;
  DeleteLocaleOverride(Application.ExeName);
  Application.Initialize;
  Application.Title := 'O2';
  ConfigureServices;
  Application.CreateForm(TMainForm, MainForm);
  MainForm.ServiceContainer := ServiceContainer;
  Application.Run;
end.
