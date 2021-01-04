{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2015 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uReplaceDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TReplaceDlgAction = (acReplaceTag, acReplaceFieldName, acReplaceFieldValue, acReplaceRole);

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
  private
    procedure EnableControls;
  public
    class function Execute(AOwner: TComponent; Action: TReplaceDlgAction;
      SearchList, ReplaceList: TStrings;
      out SearchValue, ReplaceValue: string): Boolean;
  end;

var
  ReplaceDlg: TReplaceDlg;

implementation

uses
  uGlobal;

{$R *.dfm}

class function TReplaceDlg.Execute(AOwner: TComponent;
  Action: TReplaceDlgAction; SearchList, ReplaceList: TStrings;
  out SearchValue, ReplaceValue: string): Boolean;
var
  Form: TReplaceDlg;
begin
  Form := TReplaceDlg.Create(AOwner);
  try
    case Action of
      acReplaceTag:
      begin
        Form.Caption := SReplaceTagDlgTitle;
        Form.lbSearchValue.Caption := SReplaceTagDlgSearchLabel;
        Form.lbReplaceValue.Caption := SReplaceTagDlgReplaceLabel;
      end;
      acReplaceFieldName:
      begin
        Form.Caption := SReplaceFieldNameDlgTitle;
        Form.lbSearchValue.Caption := SReplaceFieldNameDlgSearchLabel;
        Form.lbReplaceValue.Caption := SReplaceFieldNameDlgReplaceLabel;
      end;
      acReplaceFieldValue:
      begin
        Form.Caption := SReplaceFieldValueDlgTitle;
        Form.lbSearchValue.Caption := SReplaceFieldValueDlgSearchLabel;
        Form.lbReplaceValue.Caption := SReplaceFieldValueDlgReplaceLabel;
      end;
      acReplaceRole:
      begin
        Form.Caption := SReplaceRoleDlgTitle;
        Form.lbSearchValue.Caption := SReplaceRoleDlgSearchLabel;
        Form.lbReplaceValue.Caption := SReplaceRoleDlgReplaceLabel;
      end;
    end;
    Form.cbSearchValue.Items := SearchList;
    Form.cbReplaceValue.Items := ReplaceList;

    Result := Form.ShowModal = mrOk;

    if Result then
    begin
      SearchValue := Form.cbSearchValue.Text;
      ReplaceValue := Form.cbReplaceValue.Text;
    end;
  finally
    Form.Free;
  end;
end;

procedure TReplaceDlg.FormShow(Sender: TObject);
begin
  EnableControls;
end;

procedure TReplaceDlg.cbSearchValueChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TReplaceDlg.cbReplaceValueChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TReplaceDlg.EnableControls;
begin
  btOk.Enabled := (cbSearchValue.ItemIndex <> -1)
    and (cbReplaceValue.Text <> '');
end;

end.
