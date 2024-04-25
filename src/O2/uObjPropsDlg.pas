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

unit uObjPropsDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Grids, ValEdit, ExtCtrls, ActnList, Menus,
  Actions, uServices, uPasswordStrengthIndicator;

type
  TObjPropsDlgPage = (pgGeneral, pgGeneralTags, pgFields, pgNotes);
  
  TObjPropsDlg = class(TForm)
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    btOk: TButton;
    btCancel: TButton;
    edName: TEdit;
    TabSheet3: TTabSheet;
    Memo: TMemo;
    Label1: TLabel;
    ActionList: TActionList;
    OK: TAction;
    MoveUp: TAction;
    MoveDown: TAction;
    AddField: TAction;
    ReplaceField: TAction;
    DeleteField: TAction;
    FieldsView: TListView;
    cbFieldName: TComboBox;
    cbFieldValue: TComboBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    AddTag: TAction;
    Button6: TButton;
    Button7: TButton;
    ckMarkdown: TCheckBox;
    Label3: TLabel;
    PasswordStrengthIndicator: TPasswordStrengthIndicator;
    GroupBox1: TGroupBox;
    edTag: TEdit;
    lbxTags: TListBox;
    Button4: TButton;
    procedure edNameChange(Sender: TObject);
    procedure lbxTagsClick(Sender: TObject);
    procedure AddTagExecute(Sender: TObject);
    procedure AddTagUpdate(Sender: TObject);
    procedure FieldsViewResize(Sender: TObject);
    procedure FieldsViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure AddFieldExecute(Sender: TObject);
    procedure AddFieldUpdate(Sender: TObject);
    procedure ReplaceFieldExecute(Sender: TObject);
    procedure ReplaceFieldUpdate(Sender: TObject);
    procedure DeleteFieldExecute(Sender: TObject);
    procedure DeleteFieldUpdate(Sender: TObject);
    procedure MoveUpExecute(Sender: TObject);
    procedure MoveUpUpdate(Sender: TObject);
    procedure MoveDownExecute(Sender: TObject);
    procedure MoveDownUpdate(Sender: TObject);
    procedure cbFieldNameChange(Sender: TObject);
    procedure cbFieldValueEnter(Sender: TObject);
    procedure cbFieldValueChange(Sender: TObject);
    procedure ckMarkdownClick(Sender: TObject);
    procedure LinkClick(Sender: TObject);
    procedure OKExecute(Sender: TObject);
    procedure OKUpdate(Sender: TObject);
  private
    FModel: IObjectProps;
    procedure SetModel(const Value: IObjectProps);

    procedure ResizeFieldsViewColumns;
    procedure MoveField(Offset: Integer);
    procedure UpdatePasswordStrengthInfo;
  public
    class function Execute(Model: IObjectProps;
      Page: TObjPropsDlgPage): Boolean;

    property Model: IObjectProps read FModel write SetModel;
  end;

var
  ObjPropsDlg: TObjPropsDlg;

implementation

uses
  uGlobal, uCtrlHelpers, uShellUtils;

{$R *.dfm}

class function TObjPropsDlg.Execute(Model: IObjectProps;
  Page: TObjPropsDlgPage): Boolean;
var
  Form: TObjPropsDlg;
begin
  Form := TObjPropsDlg.Create(Application);
  try
    Form.Model := Model;

    case Page of
      pgGeneral:
      begin
        Form.PageControl.ActivePage := Form.TabSheet1;
        Form.ActiveControl := Form.edName;
      end;
      pgGeneralTags:
      begin
        Form.PageControl.ActivePage := Form.TabSheet1;
        Form.ActiveControl := Form.edTag;
      end;
      pgFields:
      begin
        Form.PageControl.ActivePage := Form.TabSheet2;
        Form.ActiveControl := Form.cbFieldName;
      end;
      pgNotes:
      begin
        Form.PageControl.ActivePage := Form.TabSheet3;
        Form.ActiveControl := Form.Memo;
      end;
    end;

    Result := Form.ShowModal = mrOk;
  finally
    Form.Free;
  end;
end;

procedure TObjPropsDlg.edNameChange(Sender: TObject);
begin
  if edName.Text <> '' then
    Caption := SObjPropsDlgTitle + ' - ' + edName.Text
  else
    Caption := SObjPropsDlgTitle;

  FModel.ObjectName := edName.Text;
end;

procedure TObjPropsDlg.FieldsViewResize(Sender: TObject);
begin
  ResizeFieldsViewColumns;
end;

procedure TObjPropsDlg.FieldsViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if not Selected then Exit;
  FModel.FieldIndex := Item.Index;
  cbFieldName.Text := FModel.FieldName;
  cbFieldValue.Text := FModel.FieldValue;
  UpdatePasswordStrengthInfo;
end;

procedure TObjPropsDlg.lbxTagsClick(Sender: TObject);
var
  I: Integer;
begin
  FModel.ObjectTags.Clear;
  for I := 0 to lbxTags.Count - 1 do
    if lbxTags.Selected[I] then
      FModel.ObjectTags.Add(lbxTags.Items[i]);
end;

procedure TObjPropsDlg.AddTagExecute(Sender: TObject);
begin
  lbxTags.Selected[lbxTags.Items.Add(edTag.Text)] := True;
  FModel.ObjectTags.Add(edTag.Text);
end;

procedure TObjPropsDlg.AddTagUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (edTag.Text <> '')
    and (lbxTags.Items.IndexOf(edTag.Text) = -1);
end;

procedure TObjPropsDlg.AddFieldExecute(Sender: TObject);
begin
  FModel.AddField;

  FieldsView.Items.BeginUpdate;
  try
    with FieldsView.Items.Insert(FModel.FieldIndex) do
    begin
      Caption := FModel.FieldName;
      SubItems.Add(FModel.FieldValue);
    end;

    FieldsView.ItemIndex := FModel.FieldIndex;
    FieldsView.Selected.Focused := True;
    FieldsView.Selected.MakeVisible(False);
  finally
    FieldsView.Items.EndUpdate;
  end;

  cbFieldName.SetFocus;
end;

procedure TObjPropsDlg.AddFieldUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FModel.CanAddField;
end;

procedure TObjPropsDlg.ReplaceFieldExecute(Sender: TObject);
begin
  FModel.ReplaceField;

  FieldsView.Items.BeginUpdate;
  try
    with FieldsView.Selected do
    begin
      Caption := FModel.FieldName;
      SubItems[0] := FModel.FieldValue;
    end;
  finally
    FieldsView.Items.EndUpdate
  end;

  cbFieldName.SetFocus;
end;

procedure TObjPropsDlg.ReplaceFieldUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FModel.CanReplaceField;
end;

procedure TObjPropsDlg.DeleteFieldExecute(Sender: TObject);
begin
  FModel.DeleteField;

  FieldsView.Items.BeginUpdate;
  try
    FieldsView.DeleteSelected;
    FieldsView.ItemIndex := FModel.FieldIndex;
    FieldsView.Selected.Focused := True;
    FieldsView.Selected.MakeVisible(False);
  finally
    FieldsView.Items.EndUpdate;
  end;
end;

procedure TObjPropsDlg.DeleteFieldUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FModel.CanDeleteField;
end;

procedure TObjPropsDlg.MoveUpExecute(Sender: TObject);
begin
  MoveField(-1);
end;

procedure TObjPropsDlg.MoveUpUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FModel.FieldIndex > 0;
end;

procedure TObjPropsDlg.MoveDownExecute(Sender: TObject);
begin
  MoveField(1);
end;

procedure TObjPropsDlg.MoveDownUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FModel.FieldIndex < FModel.FieldCount - 1;
end;

procedure TObjPropsDlg.cbFieldValueEnter(Sender: TObject);
begin
  TCustomComboBox(Sender).Items := FModel.FieldValues;
end;

procedure TObjPropsDlg.cbFieldNameChange(Sender: TObject);
begin
  FModel.FieldName := cbFieldName.Text;
  UpdatePasswordStrengthInfo;
end;

procedure TObjPropsDlg.cbFieldValueChange(Sender: TObject);
begin
  FModel.FieldValue := cbFieldValue.Text;
  UpdatePasswordStrengthInfo;
end;

procedure TObjPropsDlg.ckMarkdownClick(Sender: TObject);
begin
  FModel.Markdown := ckMarkdown.Checked;
end;

procedure TObjPropsDlg.LinkClick(Sender: TObject);
begin
  ShellOpen(TControl(Sender).Hint);
end;

procedure TObjPropsDlg.OKExecute(Sender: TObject);
begin
  Memo.WordWrap := False;
  FModel.ObjectNotes := Memo.Lines;

  FModel.ApplyChanges;
end;

procedure TObjPropsDlg.OKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FModel.Valid;
end;

procedure TObjPropsDlg.SetModel(const Value: IObjectProps);
var
  I: Integer;
begin
  if FModel <> Value then
  begin
    FModel := Value;

    edName.Text := FModel.ObjectName;

    lbxTags.Items := FModel.Tags;
    for I := 0 to lbxTags.Count - 1 do
      lbxTags.Selected[I] := FModel.ObjectTags.IndexOf(lbxTags.Items[I]) <> -1;
    lbxTags.ItemIndex := 0;

    FieldsView.Items.BeginUpdate;
    try
      for I := 0 to FModel.FieldCount - 1 do
        with FieldsView.Items.Add do
        begin
          Caption := FModel.ObjectFieldNames[I];
          SubItems.Add(FModel.ObjectFieldValues[I]);
        end;

      ResizeFieldsViewColumns;
    finally
      FieldsView.Items.EndUpdate;
    end;

    cbFieldName.Items := FModel.FieldNames;
    cbFieldValue.Items := FModel.FieldValues;

    UpdatePasswordStrengthInfo;

    Memo.Lines := FModel.ObjectNotes;
    Memo.SelStart := 0;

    ckMarkdown.Checked := FModel.Markdown;
  end;
end;

procedure TObjPropsDlg.MoveField(Offset: Integer);
begin
  FModel.SwapFields(FModel.FieldIndex + Offset);

  FieldsView.Items.BeginUpdate;
  try
    with FieldsView.Items[FModel.FieldIndex] do
    begin
      Caption := FModel.FieldName;
      SubItems[0] := FModel.FieldValue;
    end;

    with FieldsView.Items[FModel.FieldIndex - Offset] do
    begin
      Caption := FModel.ObjectFieldNames[FModel.FieldIndex - Offset];
      SubItems[0] := FModel.ObjectFieldValues[FModel.FieldIndex - Offset];
    end;

    FieldsView.ItemIndex := FModel.FieldIndex;
    FieldsView.Selected.Focused := True;
    FieldsView.Selected.MakeVisible(False);
  finally
    FieldsView.Items.EndUpdate;
  end;
end;

procedure TObjPropsDlg.ResizeFieldsViewColumns;
begin
  FieldsView.ResizeColumns(1);
end;

procedure TObjPropsDlg.UpdatePasswordStrengthInfo;
begin
  PasswordStrengthIndicator.PasswordScore := FModel.PasswordScore;
  PasswordStrengthIndicator.PasswordStrengthInfo := FModel.PasswordStrengthInfo;
end;

end.
