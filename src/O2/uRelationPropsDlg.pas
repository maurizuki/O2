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

unit uRelationPropsDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uServices;

type
  TRelationPropsDlg = class(TForm)
    Label1: TLabel;
    edObject1: TEdit;
    Label2: TLabel;
    edObject2: TEdit;
    Label3: TLabel;
    cbRole1: TComboBox;
    Label4: TLabel;
    cbRole2: TComboBox;
    btOk: TButton;
    btCancel: TButton;
    procedure cbRole1Change(Sender: TObject);
    procedure cbRole2Change(Sender: TObject);
    procedure btOkClick(Sender: TObject);
  private
    FModel: IRelationProps;
    procedure SetModel(const Value: IRelationProps);
  public
    class function Execute(Model: IRelationProps): Boolean;
    property Model: IRelationProps read FModel write SetModel;
  end;

var
  RelationPropsDlg: TRelationPropsDlg;

implementation

{$R *.dfm}

{ TRelationPropsDlg }

class function TRelationPropsDlg.Execute(Model: IRelationProps): Boolean;
var
  Form: TRelationPropsDlg;
begin
  Form := TRelationPropsDlg.Create(Application);
  try
    Form.Model := Model;
    Result := Form.ShowModal = mrOk;
  finally
    Form.Free;
  end;
end;

procedure TRelationPropsDlg.SetModel(const Value: IRelationProps);
begin
  if FModel <> Value then
  begin
    FModel := Value;

    edObject1.Text := FModel.ObjectName1;
    edObject2.Text := FModel.ObjectName2;
    cbRole1.Items := FModel.Roles;
    cbRole1.Text := FModel.Role1;
    cbRole2.Items := FModel.Roles;
    cbRole2.Text := FModel.Role2;
  end;
end;

procedure TRelationPropsDlg.cbRole1Change(Sender: TObject);
begin
  FModel.Role1 := cbRole1.Text;
end;

procedure TRelationPropsDlg.cbRole2Change(Sender: TObject);
begin
  FModel.Role2 := cbRole2.Text;
end;

procedure TRelationPropsDlg.btOkClick(Sender: TObject);
begin
  FModel.ApplyChanges;
end;

end.
