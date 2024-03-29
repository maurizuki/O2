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

unit uSetPassword;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Vcl.ExtCtrls, JvExStdCtrls, JvEdit, Zxcvbn;

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
    pbPasswordStrength: TPaintBox;
    PasswordStrengthMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbEncryptionChange(Sender: TObject);
    procedure cbHashChange(Sender: TObject);
    procedure edPasswordChange(Sender: TObject);
    procedure pbPasswordStrengthPaint(Sender: TObject);
  private
    FZxcvbn: TZxcvbn;
    FPasswordScore: Integer;
    procedure EnableControls;
    procedure UpdatePasswordStrengthInfo;
  public
    class function Execute(AOwner: TComponent; var Encrypt: Boolean;
      var Cipher, Hash: Byte; var Password: string): Boolean;
  end;

var
  SetPasswordDlg: TSetPasswordDlg;

implementation

uses
  uO2Defs, uO2Rules, uGlobal, uUtils, Zxcvbn.Result, Zxcvbn.Utility;

{$R *.dfm}

class function TSetPasswordDlg.Execute(AOwner: TComponent;
  var Encrypt: Boolean; var Cipher, Hash: Byte; var Password: string): Boolean;
var
  Form: TSetPasswordDlg;
begin
  Form := TSetPasswordDlg.Create(AOwner);
  try
    if Encrypt then
    begin
      TCipherLookup.Select(Form.cbEncryption, Cipher);
      THashLookup.Select(Form.cbHash, Hash);
    end
    else
    begin
      TCipherLookup.Select(Form.cbEncryption, ocNone);
      THashLookup.Select(Form.cbHash, ohDefault);
    end;
    Form.edPassword.Text := Password;
    Form.edConfPassword.Text := Password;

    Result := Form.ShowModal = mrOk;

    if Result then
    begin
      Encrypt := Form.cbEncryption.ItemIndex <> 0;
      Cipher := TCipherLookup.SelectedValue(Form.cbEncryption);
      Hash := THashLookup.SelectedValue(Form.cbHash);
      Password := Form.edPassword.Text;
    end;
  finally
    Form.Free;
  end;
end;

procedure TSetPasswordDlg.FormCreate(Sender: TObject);
begin
  FZxcvbn := TZxcvbn.Create;
  FPasswordScore := 0;
  TCipherLookup.Fill(cbEncryption);
  THashLookup.Fill(cbHash);
end;

procedure TSetPasswordDlg.FormDestroy(Sender: TObject);
begin
  FZxcvbn.Free;
end;

procedure TSetPasswordDlg.FormShow(Sender: TObject);
begin
  EnableControls;
end;

procedure TSetPasswordDlg.cbEncryptionChange(Sender: TObject);
begin
  EnableControls;
  UpdatePasswordStrengthInfo;
end;

procedure TSetPasswordDlg.cbHashChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TSetPasswordDlg.edPasswordChange(Sender: TObject);
begin
  EnableControls;
  UpdatePasswordStrengthInfo;
end;

procedure TSetPasswordDlg.pbPasswordStrengthPaint(Sender: TObject);
begin
  if cbEncryption.ItemIndex <> 0 then
    DrawHIndicator(pbPasswordStrength.Canvas, pbPasswordStrength.ClientRect,
      PasswordScoreColors[FPasswordScore], (FPasswordScore + 1) / 5)
  else
    DrawHIndicator(pbPasswordStrength.Canvas, pbPasswordStrength.ClientRect,
      0, 0);
end;

procedure TSetPasswordDlg.EnableControls;
begin
  lbHash.Enabled := cbEncryption.ItemIndex <> 0;
  cbHash.Enabled := cbEncryption.ItemIndex <> 0;
  lbPassword.Enabled := cbEncryption.ItemIndex <> 0;
  edPassword.Enabled := cbEncryption.ItemIndex <> 0;
  lbConfPassword.Enabled := cbEncryption.ItemIndex <> 0;
  edConfPassword.Enabled := cbEncryption.ItemIndex <> 0;
  btOk.Enabled := (Length(edPassword.Text) >= 5)
    and (edPassword.Text = edConfPassword.Text)
    and not (TCipherLookup.SelectedValue(cbEncryption) in DeprecatedCiphers)
    and not (THashLookup.SelectedValue(cbHash) in DeprecatedHashes)
    or (cbEncryption.ItemIndex = 0);
end;

procedure TSetPasswordDlg.UpdatePasswordStrengthInfo;
var
  ZxcvbnResult: TZxcvbnResult;
  ASuggestion: TZxcvbnSuggestion;
begin
  PasswordStrengthMemo.Clear;

  if cbEncryption.ItemIndex <> 0 then
  begin
    ZxcvbnResult := FZxcvbn.EvaluatePassword(edPassword.Text);
    try
      FPasswordScore := ZxcvbnResult.Score;

      if ZxcvbnResult.Warning <> zwDefault then
      begin
        PasswordStrengthMemo.Lines.Add(GetWarning(ZxcvbnResult.Warning));
        PasswordStrengthMemo.Lines.Add('');
      end;
      for ASuggestion in ZxcvbnResult.Suggestions do
        PasswordStrengthMemo.Lines.Add(GetSuggestion(ASuggestion));
    finally
      ZxcvbnResult.Free;
    end;
  end;

  pbPasswordStrength.Invalidate;
end;

end.
