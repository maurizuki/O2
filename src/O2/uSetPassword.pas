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

unit uSetPassword;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvExStdCtrls, JvEdit, Vcl.ExtCtrls;

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
    procedure FormShow(Sender: TObject);
    procedure cbEncryptionChange(Sender: TObject);
    procedure cbHashChange(Sender: TObject);
    procedure edPasswordChange(Sender: TObject);
    procedure pbPasswordStrengthPaint(Sender: TObject);
  private
    FPasswordScore: Integer;
    procedure EnableControls;
  public
    class function Execute(AOwner: TComponent; var Encrypt: Boolean;
      var Cipher, Hash: Byte; var Password: string): Boolean;
  end;

var
  SetPasswordDlg: TSetPasswordDlg;

implementation

uses
  uO2Defs, uGlobal, Zxcvbn, Zxcvbn.Result, Zxcvbn.Utility;

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
  FPasswordScore := 0;
  TCipherLookup.Fill(cbEncryption);
  THashLookup.Fill(cbHash);
end;

procedure TSetPasswordDlg.FormShow(Sender: TObject);
begin
  EnableControls;
end;

procedure TSetPasswordDlg.cbEncryptionChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TSetPasswordDlg.cbHashChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TSetPasswordDlg.edPasswordChange(Sender: TObject);
var
  Zxcvbn: TZxcvbn;
  ZxcvbnResult: TZxcvbnResult;
begin
  EnableControls;

  Zxcvbn := TZxcvbn.Create;
  try
    ZxcvbnResult := Zxcvbn.EvaluatePassword(edPassword.Text);
    try
      FPasswordScore := ZxcvbnResult.Score;
      pbPasswordStrength.Invalidate;

      PasswordStrengthMemo.Clear;
      PasswordStrengthMemo.Lines.Add('Warning:');
      PasswordStrengthMemo.Lines.Add(GetWarning(ZxcvbnResult.Warning));
      PasswordStrengthMemo.Lines.Add('');
      PasswordStrengthMemo.Lines.Add('Suggestions:');
      PasswordStrengthMemo.Lines.Add(GetSuggestions(ZxcvbnResult.Suggestions));
    finally
      ZxcvbnResult.Free;
    end;
  finally
    Zxcvbn.Free;
  end;
end;

procedure TSetPasswordDlg.pbPasswordStrengthPaint(Sender: TObject);
begin
  pbPasswordStrength.Canvas.Brush.Color := clWhite;
  pbPasswordStrength.Canvas.FillRect(Rect(0, 0,
    pbPasswordStrength.Width, pbPasswordStrength.Height));
  pbPasswordStrength.Canvas.Rectangle(0, 0,
    pbPasswordStrength.Width, pbPasswordStrength.Height);

  if cbEncryption.ItemIndex <> 0 then
    case FPasswordScore of
      0:
        begin
          pbPasswordStrength.Canvas.Brush.Color := $00241CED;
          pbPasswordStrength.Canvas.FillRect(Rect(1, 1,
            1 * (pbPasswordStrength.Width div 5) - 1,
            pbPasswordStrength.Height - 1));
        end;

      1:
        begin
          pbPasswordStrength.Canvas.Brush.Color := $00277FFF;
          pbPasswordStrength.Canvas.FillRect(Rect(1, 1,
          2 * (pbPasswordStrength.Width div 5) - 1,
          pbPasswordStrength.Height - 1));
        end;

      2:
        begin
          pbPasswordStrength.Canvas.Brush.Color := $000EC9FF;
          pbPasswordStrength.Canvas.FillRect(Rect(1, 1,
          3 * (pbPasswordStrength.Width div 5) - 1,
          pbPasswordStrength.Height - 1));
        end;

      3:
        begin
          pbPasswordStrength.Canvas.Brush.Color := $00E8A200;
          pbPasswordStrength.Canvas.FillRect(Rect(1, 1,
          4 * (pbPasswordStrength.Width div 5) - 1,
          pbPasswordStrength.Height - 1));
        end;

      4:
        begin
          pbPasswordStrength.Canvas.Brush.Color := $004CB122;
          pbPasswordStrength.Canvas.FillRect(Rect(1, 1,
          pbPasswordStrength.Width - 1, pbPasswordStrength.Height - 1));
        end;
    end;
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

end.
