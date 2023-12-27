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

unit uFilePropsDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uO2File, StdCtrls;

type
  TFilePropsDlg = class(TForm)
    Label1: TLabel;
    edTitle: TEdit;
    Label2: TLabel;
    edDescription: TEdit;
    Label3: TLabel;
    edAuthor: TEdit;
    Label4: TLabel;
    edCipher: TEdit;
    Label5: TLabel;
    edHash: TEdit;
    btOk: TButton;
    btCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure btOkClick(Sender: TObject);
  private
    FO2File: TO2File;
  public
    class function Execute(AOwner: TComponent; O2File: TO2File): Boolean;
    property O2File: TO2File read FO2File write FO2File;
  end;

var
  FilePropsDlg: TFilePropsDlg;

implementation

uses
  uGlobal;

{$R *.dfm}

class function TFilePropsDlg.Execute(AOwner: TComponent;
  O2File: TO2File): Boolean;
var
  Form: TFilePropsDlg;
begin
  Form := TFilePropsDlg.Create(AOwner);
  try
    Form.O2File := O2File;
    Result := Form.ShowModal = mrOk;
  finally
    Form.Free;
  end;
end;

procedure TFilePropsDlg.FormShow(Sender: TObject);
begin
  edTitle.Text := O2File.Title;
  edDescription.Text := O2File.Description;
  edAuthor.Text := O2File.Author;

  if O2File.Encrypted then
  begin
    edCipher.Text := TCipherLookup.Lookup(O2File.Cipher);
    edHash.Text := THashLookup.Lookup(O2File.Hash);
  end
  else
  begin
    edCipher.Text := SCipherNone;
    edHash.Text := SHashNone;
  end
end;

procedure TFilePropsDlg.btOkClick(Sender: TObject);
begin
  O2File.Title := edTitle.Text;
  O2File.Description := edDescription.Text;
  O2File.Author := edAuthor.Text;
  ModalResult := mrOk;
end;

end.
