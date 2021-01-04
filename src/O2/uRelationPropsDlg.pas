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

unit uRelationPropsDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uO2File, uO2Objects, uO2Relations;

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
    procedure FormShow(Sender: TObject);
    procedure btOkClick(Sender: TObject);
  private
    FO2File: TO2File;
    FRelation: TO2Relation;
    FObject1: TO2Object;
    FObject2: TO2Object;
  protected
    property O2File: TO2File read FO2File write FO2File;
    property Relation: TO2Relation read FRelation write FRelation;
    property Object1: TO2Object read FObject1 write FObject1;
    property Object2: TO2Object read FObject2 write FObject2;
  public
    class function Execute(const AOwner: TComponent; const O2File: TO2File;
      const Object1, Object2: TO2Object; var Relation: TO2Relation): Boolean;
  end;

var
  RelationPropsDlg: TRelationPropsDlg;

implementation

{$R *.dfm}

{ TRelationPropsDlg }

class function TRelationPropsDlg.Execute(const AOwner: TComponent;
  const O2File: TO2File; const Object1, Object2: TO2Object;
  var Relation: TO2Relation): Boolean;
var
  Form: TRelationPropsDlg;
begin
  Form := TRelationPropsDlg.Create(AOwner);
  try
    Form.O2File := O2File;
    Form.Relation := Relation;
    Form.Object1 := Object1;
    Form.Object2 := Object2;
    Result := Form.ShowModal = mrOk;
    if Result then Relation := Form.Relation;
  finally
    Form.Free;
  end;
end;

procedure TRelationPropsDlg.FormShow(Sender: TObject);
begin
  O2File.Relations.GetRoles(cbRole1.Items);
  cbRole2.Items := cbRole1.Items; 
  if Assigned(Relation) then
  begin
    Object1 := O2File.Objects.FindObjectByID(Relation.ObjectID1);
    Object2 := O2File.Objects.FindObjectByID(Relation.ObjectID2);
    cbRole1.Text := Relation.Role1;
    cbRole2.Text := Relation.Role2;
  end;
  if Assigned(Object1) then
    edObject1.Text := Object1.Name;
  if Assigned(Object2) then
    edObject2.Text := Object2.Name;
end;

procedure TRelationPropsDlg.btOkClick(Sender: TObject);
begin
  if Relation = nil then
  begin
    Relation := O2File.Relations.AddRelation;
    Relation.ObjectID1 := Object1.ObjectID;
    Relation.ObjectID2 := Object2.ObjectID;
  end;
  Relation.Role1 := cbRole1.Text;
  Relation.Role2 := cbRole2.Text;
  ModalResult := mrOk;
end;

end.
