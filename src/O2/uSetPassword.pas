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

unit uSetPassword;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Vcl.ExtCtrls, JvExStdCtrls, JvEdit, uServices,
  uPasswordStrengthIndicator;

type
  TSetPasswordDlg = class(TForm)
    lbPassword: TLabel;
    edPassword: TJvEdit;
    lbConfPassword: TLabel;
    edConfPassword: TJvEdit;
    btOk: TButton;
    btCancel: TButton;
    Label1: TLabel;
    cbEncryption: TComboBox;
    lbHash: TLabel;
    cbHash: TComboBox;
    gbPasswordStrength: TGroupBox;
    PasswordStrengthMemo: TMemo;
    PasswordStrengthIndicator: TPasswordStrengthIndicator;
    procedure cbEncryptionChange(Sender: TObject);
    procedure cbHashChange(Sender: TObject);
    procedure edPasswordChange(Sender: TObject);
    procedure edConfPasswordChange(Sender: TObject);
    procedure btOkClick(Sender: TObject);
  private
    FModel: IEncryptionProps;
    procedure SetModel(const Value: IEncryptionProps);
  public
    class function Execute(Model: IEncryptionProps): Boolean;
    property Model: IEncryptionProps read FModel write SetModel;
  end;

var
  SetPasswordDlg: TSetPasswordDlg;

implementation

{$R *.dfm}

class function TSetPasswordDlg.Execute(Model: IEncryptionProps): Boolean;
var
  Form: TSetPasswordDlg;
begin
  Form := TSetPasswordDlg.Create(Application);
  try
    Form.Model := Model;
    Result := Form.ShowModal = mrOk;
  finally
    Form.Free;
  end;
end;

procedure TSetPasswordDlg.btOkClick(Sender: TObject);
begin
  FModel.ApplyChanges;
end;

procedure TSetPasswordDlg.cbEncryptionChange(Sender: TObject);
var
  IsEncrypted: Boolean;
begin
  FModel.CipherIndex := cbEncryption.ItemIndex;

  IsEncrypted := FModel.IsEncrypted;
  lbHash.Enabled := IsEncrypted;
  cbHash.Enabled := IsEncrypted;
  lbPassword.Enabled := IsEncrypted;
  edPassword.Enabled := IsEncrypted;
  lbConfPassword.Enabled := IsEncrypted;
  edConfPassword.Enabled := IsEncrypted;

  PasswordStrengthIndicator.PasswordScore := FModel.PasswordScore;
  PasswordStrengthMemo.Text := FModel.PasswordStrengthInfo;

  btOk.Enabled := FModel.Valid;
end;

procedure TSetPasswordDlg.cbHashChange(Sender: TObject);
begin
  FModel.HashIndex := cbHash.ItemIndex;

  btOk.Enabled := FModel.Valid;
end;

procedure TSetPasswordDlg.edConfPasswordChange(Sender: TObject);
begin
  FModel.PasswordConfirmation := edConfPassword.Text;

  btOk.Enabled := FModel.Valid;
end;

procedure TSetPasswordDlg.edPasswordChange(Sender: TObject);
begin
  FModel.Password := edPassword.Text;

  PasswordStrengthIndicator.PasswordScore := FModel.PasswordScore;
  PasswordStrengthMemo.Text := FModel.PasswordStrengthInfo;

  btOk.Enabled := FModel.Valid;
end;

procedure TSetPasswordDlg.SetModel(const Value: IEncryptionProps);
var
  IsEncrypted: Boolean;
begin
  if FModel <> Value then
  begin
    FModel := Value;

    cbEncryption.Items := FModel.Ciphers;
    cbEncryption.ItemIndex := FModel.CipherIndex;
    cbHash.Items := FModel.Hashes;
    cbHash.ItemIndex := FModel.HashIndex;
    edPassword.Text := FModel.Password;
    edConfPassword.Text := FModel.PasswordConfirmation;

    IsEncrypted := FModel.IsEncrypted;
    lbHash.Enabled := IsEncrypted;
    cbHash.Enabled := IsEncrypted;
    lbPassword.Enabled := IsEncrypted;
    edPassword.Enabled := IsEncrypted;
    lbConfPassword.Enabled := IsEncrypted;
    edConfPassword.Enabled := IsEncrypted;

    PasswordStrengthIndicator.PasswordScore := FModel.PasswordScore;
    PasswordStrengthMemo.Text := FModel.PasswordStrengthInfo;

    btOk.Enabled := FModel.Valid;
  end;
end;

end.
