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
  uPAFConsts in 'uPAFConsts.pas',
  uXmlStorage in 'uXmlStorage.pas',
  uHTMLExport in 'uHTMLExport.pas' {HTMLExport},
  uStuffHTML in 'uStuffHTML.pas',
  uMRUList in 'uMRUList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'O2';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
