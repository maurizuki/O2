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

unit uFilePropsDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uServices;

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
    procedure edTitleChange(Sender: TObject);
    procedure edDescriptionChange(Sender: TObject);
    procedure edAuthorChange(Sender: TObject);
    procedure btOkClick(Sender: TObject);
  private
    FModel: IFileProps;
    procedure SetModel(const Value: IFileProps);
  public
    class function Execute(Model: IFileProps): Boolean;
    property Model: IFileProps read FModel write SetModel;
  end;

var
  FilePropsDlg: TFilePropsDlg;

implementation

{$R *.dfm}

procedure TFilePropsDlg.edAuthorChange(Sender: TObject);
begin
  FModel.Author := edAuthor.Text;
end;

procedure TFilePropsDlg.edDescriptionChange(Sender: TObject);
begin
  FModel.Description := edDescription.Text;
end;

procedure TFilePropsDlg.edTitleChange(Sender: TObject);
begin
  FModel.Title := edTitle.Text;
end;

class function TFilePropsDlg.Execute(Model: IFileProps): Boolean;
var
  Form: TFilePropsDlg;
begin
  Form := TFilePropsDlg.Create(Application);
  try
    Form.Model := Model;
    Result := Form.ShowModal = mrOk;
  finally
    Form.Free;
  end;
end;

procedure TFilePropsDlg.SetModel(const Value: IFileProps);
begin
  if FModel <> Value then
  begin
    FModel := Value;

    edTitle.Text := FModel.Title;
    edDescription.Text := FModel.Description;
    edAuthor.Text := FModel.Author;
    edCipher.Text := FModel.Cipher;
    edHash.Text := FModel.Hash;
  end;
end;

procedure TFilePropsDlg.btOkClick(Sender: TObject);
begin
  FModel.ApplyChanges;
end;

end.
