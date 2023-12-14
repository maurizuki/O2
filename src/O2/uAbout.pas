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

unit uAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

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
    procedure FormCreate(Sender: TObject);
    procedure LinkClick(Sender: TObject);
    procedure btReadMeClick(Sender: TObject);
  end;

var
  AboutForm: TAboutForm;

implementation

uses
  JclFileUtils, uGlobal, uAppFiles, uShellUtils, uRTFViewer;

{$R *.dfm}

procedure TAboutForm.btReadMeClick(Sender: TObject);
begin
  TRTFViewer.Execute(Application, AppFiles.FullPath[IdReadMe]);
end;

procedure TAboutForm.FormCreate(Sender: TObject);
var
  VersionInfo: TJclFileVersionInfo;
begin
  VersionInfo := TJclFileVersionInfo.Create(AppFiles.FullPath[IdAppExe]);
  try
    lbVersion.Caption := VersionInfo.BinFileVersion;
  finally
    VersionInfo.Free;
  end;
end;

procedure TAboutForm.LinkClick(Sender: TObject);
begin
  ShellOpen(TControl(Sender).Hint);
end;

end.
