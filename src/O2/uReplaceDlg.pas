{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2025 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uReplaceDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uServices;

type
  TReplaceDlg = class(TForm)
    lbReplaceValue: TLabel;
    btOk: TButton;
    btCancel: TButton;
    lbSearchValue: TLabel;
    cbSearchValue: TComboBox;
    cbReplaceValue: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure cbSearchValueChange(Sender: TObject);
    procedure cbReplaceValueChange(Sender: TObject);
    procedure btOkClick(Sender: TObject);
  private
    FModel: IReplaceOperation;
    procedure SetModel(const Value: IReplaceOperation);
  public
    class function Execute(Model: IReplaceOperation): Boolean;
    property Model: IReplaceOperation read FModel write SetModel;
  end;

var
  ReplaceDlg: TReplaceDlg;

implementation

{$R *.dfm}

class function TReplaceDlg.Execute(Model: IReplaceOperation): Boolean;
var
  Form: TReplaceDlg;
begin
  Form := TReplaceDlg.Create(Application);
  try
    Form.Model := Model;
    Result := Form.ShowModal = mrOk;
  finally
    Form.Free;
  end;
end;

procedure TReplaceDlg.FormShow(Sender: TObject);
begin
  btOk.Enabled := FModel.Valid;
end;

procedure TReplaceDlg.SetModel(const Value: IReplaceOperation);
begin
  if FModel <> Value then
  begin
    FModel := Value;

    Caption := FModel.Title;
    lbSearchValue.Caption := FModel.SearchValueLabel;
    lbReplaceValue.Caption := FModel.ReplaceValueLabel;
    cbSearchValue.Items := FModel.SearchList;
    cbSearchValue.Text := FModel.SearchValue;
    cbReplaceValue.Items := FModel.ReplaceList;
    cbReplaceValue.Text := FModel.ReplaceValue;
  end;
end;

procedure TReplaceDlg.cbSearchValueChange(Sender: TObject);
begin
  FModel.SearchValue := cbSearchValue.Text;
  btOk.Enabled := FModel.Valid;
end;

procedure TReplaceDlg.cbReplaceValueChange(Sender: TObject);
begin
  FModel.ReplaceValue := cbReplaceValue.Text;
  btOk.Enabled := FModel.Valid;
end;

procedure TReplaceDlg.btOkClick(Sender: TObject);
begin
  FModel.Replace;
end;

end.
