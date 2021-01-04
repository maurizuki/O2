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

unit uObjPropsDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Grids, ValEdit, ExtCtrls, uO2Objects, uUtils,
  ActnList, Menus, System.Actions;

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
    Label2: TLabel;
    cbTag: TComboBox;
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
    lbxTags: TListBox;
    Button4: TButton;
    Button5: TButton;
    AddTag: TAction;
    DeleteTag: TAction;
    Button6: TButton;
    Button7: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure FieldsViewResize(Sender: TObject);
    procedure FieldsViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure AddTagExecute(Sender: TObject);
    procedure AddTagUpdate(Sender: TObject);
    procedure DeleteTagExecute(Sender: TObject);
    procedure DeleteTagUpdate(Sender: TObject);
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
    procedure cbFieldValueEnter(Sender: TObject);
    procedure OKExecute(Sender: TObject);
    procedure OKUpdate(Sender: TObject);
  private
    FObjects: TO2Objects;
    FObjectIndex: Integer;
    FDuplicate: Boolean;
    FFields: TO2Fields;
    procedure SetFields(const Value: TO2Fields);
    function GetSelectedField: TO2Field;
    function FieldToListItem(const AField: TO2Field;
      const Item: TListItem; Index: Integer = -1): TListItem;
    procedure SetFocusAndMakeVisible;
    procedure MoveSelectedField(Offset: Integer);
    procedure ResizeFieldsViewColumns;
    procedure UpdateFieldsView;
  protected
    property Fields: TO2Fields read FFields write SetFields;
    property SelectedField: TO2Field read GetSelectedField;
  public
    class function Execute(AOwner: TComponent; const Objects: TO2Objects;
      var ObjectIndex: Integer; Duplicate: Boolean;
      Page: TObjPropsDlgPage): Boolean;
    property Objects: TO2Objects read FObjects write FObjects;
    property ObjectIndex: Integer read FObjectIndex write FObjectIndex;
    property Duplicate: Boolean read FDuplicate write FDuplicate;
  end;

var
  ObjPropsDlg: TObjPropsDlg;

implementation

uses
  uGlobal;

{$R *.dfm}

class function TObjPropsDlg.Execute(AOwner: TComponent;
  const Objects: TO2Objects; var ObjectIndex: Integer; Duplicate: Boolean;
  Page: TObjPropsDlgPage): Boolean;
var
  Form: TObjPropsDlg;
begin
  Form := TObjPropsDlg.Create(AOwner);
  try
    Form.Objects := Objects;
    Form.ObjectIndex := ObjectIndex;
    Form.Duplicate := Duplicate;
    case Page of
      pgGeneral:
      begin
        Form.PageControl.ActivePage := Form.TabSheet1;
        Form.ActiveControl := Form.edName;
      end;
      pgGeneralTags:
      begin
        Form.PageControl.ActivePage := Form.TabSheet1;
        Form.ActiveControl := Form.cbTag;
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
    if Result then ObjectIndex := Form.ObjectIndex;
  finally
    Form.Free;
  end;
end;

procedure TObjPropsDlg.FormCreate(Sender: TObject);
begin
  FFields := TO2Fields.Create(nil);
end;

procedure TObjPropsDlg.FormDestroy(Sender: TObject);
begin
  Fields.Free;
end;

procedure TObjPropsDlg.FormShow(Sender: TObject);
var
  AObject: TO2Object;
begin
  if ObjectIndex <> -1 then
  begin
    AObject := Objects[ObjectIndex];
    if not Duplicate then
      edName.Text := AObject.Name;
    AObject.GetTags(lbxTags.Items);
    Fields := AObject.Fields;
    Memo.Lines := AObject.Text;
    Memo.SelStart := 0;
  end;
  UpdateFieldsView;
  Objects.GetTags(cbTag.Items);
  Objects.GetFieldNames(cbFieldName.Items);

  if Duplicate then ObjectIndex := -1;
end;

procedure TObjPropsDlg.edNameChange(Sender: TObject);
begin
  if edName.Text <> '' then
    Caption := SObjPropsDlgTitle + ' - ' + edName.Text
  else
    Caption := SObjPropsDlgTitle;
end;

procedure TObjPropsDlg.FieldsViewResize(Sender: TObject);
begin
  ResizeFieldsViewColumns;
end;

procedure TObjPropsDlg.FieldsViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
  begin
    cbFieldName.Text := TO2Field(Item.Data).FieldName;
    cbFieldValue.Text := TO2Field(Item.Data).FieldValue;
  end;
end;

procedure TObjPropsDlg.AddTagExecute(Sender: TObject);
begin
  lbxTags.Items.Add(cbTag.Text);
  cbTag.Text := '';
  cbTag.SetFocus;
end;

procedure TObjPropsDlg.AddTagUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (cbTag.Text <> '')
    and (lbxTags.Items.IndexOf(cbTag.Text) = -1);
end;

procedure TObjPropsDlg.DeleteTagExecute(Sender: TObject);
begin
  lbxTags.DeleteSelected;
end;

procedure TObjPropsDlg.DeleteTagUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := lbxTags.ItemIndex <> -1;
end;

procedure TObjPropsDlg.AddFieldExecute(Sender: TObject);
var
  AField: TO2Field;
  Index: Integer;
begin
  AField := Fields.AddField(cbFieldName.Text);
  AField.FieldValue := cbFieldValue.Text;
  if Assigned(FieldsView.Selected) then
    AField.Index := FieldsView.Selected.Index + 1;
  FieldsView.ClearSelection;
  UpdateFieldsView;
  FieldsView.Selected := FieldsView.FindData(0, AField, True, False);
  SetFocusAndMakeVisible;
  cbFieldName.SetFocus;
end;

procedure TObjPropsDlg.AddFieldUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := cbFieldName.Text <> '';
end;

procedure TObjPropsDlg.ReplaceFieldExecute(Sender: TObject);
begin
  SelectedField.FieldName := cbFieldName.Text;
  SelectedField.FieldValue := cbFieldValue.Text;
  FieldToListItem(SelectedField, FieldsView.Selected);
  cbFieldName.SetFocus;
end;

procedure TObjPropsDlg.ReplaceFieldUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(SelectedField)
    and (cbFieldName.Text <> '');
end;

procedure TObjPropsDlg.DeleteFieldExecute(Sender: TObject);
var
  Index: Integer;
begin
  Index := FieldsView.Selected.Index;
  LVFreeSelectedItemsData(FieldsView);
  UpdateFieldsView;
  if Index >= FieldsView.Items.Count then
    Index := FieldsView.Items.Count - 1;
  if Index >= 0 then
  begin
    FieldsView.Selected := FieldsView.Items[Index];
    SetFocusAndMakeVisible;
  end;
end;

procedure TObjPropsDlg.DeleteFieldUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(SelectedField);
end;

procedure TObjPropsDlg.MoveUpExecute(Sender: TObject);
begin
  MoveSelectedField(-1);
end;

procedure TObjPropsDlg.MoveUpUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(FieldsView.Selected)
    and (FieldsView.Selected.Index > 0);
end;

procedure TObjPropsDlg.MoveDownExecute(Sender: TObject);
begin
  MoveSelectedField(1);
end;

procedure TObjPropsDlg.MoveDownUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(FieldsView.Selected)
    and (FieldsView.Selected.Index < FieldsView.Items.Count - 1);
end;

procedure TObjPropsDlg.cbFieldValueEnter(Sender: TObject);
begin
  TComboBox(Sender).Items.Clear;
  Objects.GetFieldValues(cbFieldName.Text, TComboBox(Sender).Items);
end;

procedure TObjPropsDlg.OKExecute(Sender: TObject);
var
  AObject: TO2Object;
begin
  if ObjectIndex = -1 then
    AObject := Objects.AddObject(edName.Text)
  else
  begin
    AObject := Objects[ObjectIndex];
    AObject.Name := edName.Text;
  end;
  AObject.SetTags(lbxTags.Items);
  AObject.Fields := Fields;
  Memo.WordWrap := False;
  AObject.Text := Memo.Lines;
  ObjectIndex := AObject.Index;

  ModalResult := mrOk;
end;

procedure TObjPropsDlg.OKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := edName.Text <> '';
end;

procedure TObjPropsDlg.SetFields(const Value: TO2Fields);
begin
  FFields.Assign(Value);
end;

function TObjPropsDlg.GetSelectedField: TO2Field;
begin
  if Assigned(FieldsView.Selected) then
    Result := TO2Field(FieldsView.Selected.Data)
  else
    Result := nil;
end;

function TObjPropsDlg.FieldToListItem(const AField: TO2Field;
  const Item: TListItem; Index: Integer): TListItem;
begin
  if Assigned(Item) then
    Result := Item
  else
    Result := FieldsView.Items.AddItem(nil, Index);
  Result.Caption := AField.FieldName;
  Result.SubItems.Clear;
  Result.SubItems.Add(AField.FieldValue);
  Result.Data := AField;
end;

procedure TObjPropsDlg.SetFocusAndMakeVisible;
begin
  FieldsView.Selected.Focused := True;
  FieldsView.Selected.MakeVisible(False);
end;

procedure TObjPropsDlg.MoveSelectedField(Offset: Integer);
var
  AField: TO2Field;
begin
  if Offset = 0 then Exit;
  AField := SelectedField;
  AField.Index := AField.Index + Offset;
  UpdateFieldsView;
  SetFocusAndMakeVisible;
end;

procedure TObjPropsDlg.ResizeFieldsViewColumns;
begin
  LVResizeColumns(FieldsView, 1);
end;

procedure TObjPropsDlg.UpdateFieldsView;
var
  AField: TO2Field;
  Selection: TList;
begin
  Selection := LVListSelectedItemsData(FieldsView);
  try
    FieldsView.Items.BeginUpdate;
    try
      FieldsView.Clear;
      for AField in Fields do FieldToListItem(AField, nil);
      ResizeFieldsViewColumns;
      LVSelectItemsByData(FieldsView, Selection);
    finally
      FieldsView.Items.EndUpdate;
    end;
  finally
    Selection.Free;
  end;
end;

end.
