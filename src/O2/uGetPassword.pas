{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2021 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uGetPassword;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvExStdCtrls, JvEdit;

type
  TGetPasswordDlg = class(TForm)
    edPassword: TJvEdit;
    btOk: TButton;
    btCancel: TButton;
    procedure edPasswordChange(Sender: TObject);
  public
    class function Execute(AOwner: TComponent; var Password: string): Boolean;
  end;

var
  GetPasswordDlg: TGetPasswordDlg;

implementation

{$R *.dfm}

class function TGetPasswordDlg.Execute(AOwner: TComponent;
  var Password: string): Boolean;
var
  Form: TGetPasswordDlg;
begin
  Form := TGetPasswordDlg.Create(AOwner);
  try
    Form.edPassword.Text := Password;
    Result := Form.ShowModal = mrOk;
    if Result then
      Password := Form.edPassword.Text;
  finally
    Form.Free;
  end;
end;

procedure TGetPasswordDlg.edPasswordChange(Sender: TObject);
begin
  btOk.Enabled := Length(edPassword.Text) >= 5;
end;

end.
