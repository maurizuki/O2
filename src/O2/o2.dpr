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

program o2;

uses
  Forms,
  uMain in 'uMain.pas' {MainForm},
  uUtils in 'uUtils.pas',
  uShellUtils in 'uShellUtils.pas',
  uGlobal in 'uGlobal.pas',
  uAppFiles in 'uAppFiles.pas',
  uObjPropsDlg in 'uObjPropsDlg.pas' {ObjPropsDlg},
  uRelationPropsDlg in 'uRelationPropsDlg.pas' {RelationPropsDlg},
  uRulePropsDlg in 'uRulePropsDlg.pas' {RulePropsDlg},
  uGetPassword in 'uGetPassword.pas' {GetPasswordDlg},
  uSetPassword in 'uSetPassword.pas' {SetPasswordDlg},
  uAbout in 'uAbout.pas' {AboutForm},
  uFilePropsDlg in 'uFilePropsDlg.pas' {FilePropsDlg},
  uReplaceDlg in 'uReplaceDlg.pas' {ReplaceDlg},
  uPrintPreview in 'uPrintPreview.pas' {PrintPreview},
  uLookupHelper in 'uLookupHelper.pas',
  uXmlStorage in 'uXmlStorage.pas',
  uHTMLExport in 'uHTMLExport.pas' {HTMLExport},
  uStuffHTML in 'uStuffHTML.pas',
  uMRUList in 'uMRUList.pas',
  uBrowserEmulation in 'uBrowserEmulation.pas',
  uCtrlHelpers in 'uCtrlHelpers.pas',
  uFileOperation in 'uFileOperation.pas',
  uXmlImportExport in 'uXmlImportExport.pas',
  uO2ImportExport in 'uO2ImportExport.pas',
  uiCalendarExport in 'uiCalendarExport.pas',
  uPasswordScoreCache in 'uPasswordScoreCache.pas',
  uRTFViewer in 'uRTFViewer.pas' {RTFViewer},
  uHTMLHelper in 'uHTMLHelper.pas',
  uO2ObjectsUtils in 'uO2ObjectsUtils.pas',
  uPrintModel in 'uPrintModel.pas',
  uServices in 'uServices.pas',
  uHTMLExportModel in 'uHTMLExportModel.pas',
  uStartup in 'uStartup.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'O2';
  ConfigureServices;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
