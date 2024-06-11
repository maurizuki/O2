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

unit uAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, uServices, uUtils;

type
  TAboutForm = class(TForm)
    Image1: TImage;
    lbVersion: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    GroupBox2: TGroupBox;
    btOk: TButton;
    btReadMe: TButton;
    Memo1: TMemo;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LinkClick(Sender: TObject);
    procedure btReadMeClick(Sender: TObject);
  public
    FAppFiles: IAppFiles;
    class procedure Execute(AppVersionInfo: TAppVersionInfo;
      AppFiles: IAppFiles);
  end;

var
  AboutForm: TAboutForm;

implementation

uses
  DateUtils, JclBase, JVCLVer, uGlobal, uShellUtils, uRTFViewer, uO2Defs;

{$R *.dfm}

class procedure TAboutForm.Execute(AppVersionInfo: TAppVersionInfo;
  AppFiles: IAppFiles);
var
  Form: TAboutForm;
begin
  Form := TAboutForm.Create(Application);
  try
    Form.FAppFiles := AppFiles;
    Form.lbVersion.Caption := AppVersionInfo.DisplayVersion;
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

procedure TAboutForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F1) and (ssCtrl in Shift) and (ssShift in Shift) then
  begin
    GroupBox2.Caption := 'Tech info';

    Memo1.Lines.BeginUpdate;
    Memo1.Clear;
    Memo1.Lines.Add('Build timestamp: '
      + DateToISO8601(PImageNtHeaders(HInstance
      + Cardinal(PImageDosHeader(HInstance)^._lfanew))^.FileHeader.TimeDateStamp
      / SecsPerDay + UnixDateDelta));
    Memo1.Lines.Add(Format('Compiler version: %d.%d', [Trunc(CompilerVersion),
      Trunc(Frac(CompilerVersion) * 10)]));
    Memo1.Lines.Add('');
    Memo1.Lines.Add(Format('JCL version: %d.%d.%d.%d', [JclVersionMajor,
      JclVersionMinor, JclVersionRelease, JclVersionBuild]));
    Memo1.Lines.Add(Format('JVCL version: %d.%d.%d.%d', [JVCLVersionMajor,
      JVCLVersionMinor, JVCLVersionRelease, JVCLVersionBuild]));
    Memo1.Lines.Add('');
    Memo1.Lines.Add(Format('O2 file version: %d.%d', [O2FileVersion.Hi,
      O2FileVersion.Lo]));
    Memo1.Lines.Add('O2 file identifier: ' + GUIDToString(O2FileGUID));
    Memo1.Lines.Add('');
    Memo1.Lines.Add('EXE path: ' + Application.ExeName);
    Memo1.Lines.Add('Settings path: ' + FAppFiles.FullPaths[IdSettings]);
    Memo1.Lines.EndUpdate;
  end;
end;

procedure TAboutForm.LinkClick(Sender: TObject);
begin
  ShellOpen(TControl(Sender).Hint);
end;

procedure TAboutForm.btReadMeClick(Sender: TObject);
begin
  TRTFViewer.Execute(Application, FAppFiles.FullPaths[IdReadMe]);
end;

end.
