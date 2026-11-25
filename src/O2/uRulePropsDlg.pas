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

unit uRulePropsDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, ComCtrls, ExtCtrls, Mask, JvExMask, JvSpin,
  Menus, System.Actions, uServices;

type
  TRulePropsDlg = class(TForm)
    btOk: TButton;
    btCancel: TButton;
    ActionList: TActionList;
    OK: TAction;
    PageControl1: TPageControl;
    tsGeneral: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edName: TEdit;
    cbType: TComboBox;
    edFieldName: TEdit;
    edFieldValue: TEdit;
    tsHighlight: TTabSheet;
    Label5: TLabel;
    cbHighlightColor: TColorBox;
    Label6: TLabel;
    cbHighlightTextColor: TColorBox;
    tsHyperLink: TTabSheet;
    Label7: TLabel;
    edMask: TEdit;
    HyperLinkFieldNameMacro: TAction;
    HyperLinkFieldValueMacro: TAction;
    tsDateFormat: TTabSheet;
    Label8: TLabel;
    Label9: TLabel;
    edDateSeparator: TEdit;
    tsCustomFilter: TTabSheet;
    Label10: TLabel;
    Label11: TLabel;
    edDaysBefore: TJvSpinEdit;
    edDaysAfter: TJvSpinEdit;
    btHyperLinkMacros: TButton;
    HyperLinkMacrosMenu: TPopupMenu;
    Fieldname1: TMenuItem;
    Fieldvalue1: TMenuItem;
    tsDisplay: TTabSheet;
    edDisplayMask: TEdit;
    Label12: TLabel;
    btDisplayMacros: TButton;
    DisplayFieldNameMacro: TAction;
    DisplayFieldValueMacro: TAction;
    DisplayMacrosMenu: TPopupMenu;
    DisplayYearsMacro: TAction;
    DisplayMonthsOfYearMacro: TAction;
    DisplayDaysOfMonthMacro: TAction;
    DisplayMonthsMacro: TAction;
    DisplayDaysMacro: TAction;
    Fieldname2: TMenuItem;
    Fieldvalue2: TMenuItem;
    N1: TMenuItem;
    Years1: TMenuItem;
    Monthsofyear1: TMenuItem;
    Daysofmonth1: TMenuItem;
    Months1: TMenuItem;
    Days1: TMenuItem;
    cbDateFormat: TComboBox;
    tsPassword: TTabSheet;
    ckDisplayPasswordStrength: TCheckBox;
    Help: TAction;
    btHelp: TButton;
    procedure FormShow(Sender: TObject);
    procedure HelpExecute(Sender: TObject);
    procedure OKExecute(Sender: TObject);
    procedure OKUpdate(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure cbTypeChange(Sender: TObject);
    procedure edFieldNameChange(Sender: TObject);
    procedure edFieldValueChange(Sender: TObject);
    procedure edDisplayMaskChange(Sender: TObject);
    procedure btDisplayMacrosClick(Sender: TObject);
    procedure DisplayFieldNameMacroExecute(Sender: TObject);
    procedure DisplayFieldValueMacroExecute(Sender: TObject);
    procedure DisplayYearsMacroExecute(Sender: TObject);
    procedure DisplayMonthsOfYearMacroExecute(Sender: TObject);
    procedure DisplayDaysOfMonthMacroExecute(Sender: TObject);
    procedure DisplayMonthsMacroExecute(Sender: TObject);
    procedure DisplayDaysMacroExecute(Sender: TObject);
    procedure edMaskChange(Sender: TObject);
    procedure btHyperLinkMacrosClick(Sender: TObject);
    procedure HyperLinkFieldValueMacroExecute(Sender: TObject);
    procedure HyperLinkFieldNameMacroExecute(Sender: TObject);
    procedure cbDateFormatChange(Sender: TObject);
    procedure edDateSeparatorChange(Sender: TObject);
    procedure edDaysBeforeChange(Sender: TObject);
    procedure edDaysAfterChange(Sender: TObject);
    procedure cbHighlightColorChange(Sender: TObject);
    procedure cbHighlightTextColorChange(Sender: TObject);
    procedure ckDisplayPasswordStrengthClick(Sender: TObject);
  private
    FModel: IRuleProps;
    procedure SetModel(const Value: IRuleProps);
    procedure AddMacro(const AEdit: TCustomEdit; const Macro: string);
    procedure DisplayTabs;
  public
    class function Execute(Model: IRuleProps): Boolean;
    property Model: IRuleProps read FModel write SetModel;
  end;

var
  RulePropsDlg: TRulePropsDlg;

implementation

uses
  uGlobal, uCtrlHelpers, uShellUtils, uO2Rules;

{$R *.dfm}

class function TRulePropsDlg.Execute(Model: IRuleProps): Boolean;
var
  Form: TRulePropsDlg;
begin
  Form := TRulePropsDlg.Create(Application);
  try
    Form.Model := Model;
    Result := Form.ShowModal = mrOk;
  finally
    Form.Free;
  end;
end;

procedure TRulePropsDlg.SetModel(const Value: IRuleProps);
begin
  if FModel <> Value then
  begin
    FModel := Value;

    edName.Text := FModel.RuleName;
    cbType.Items := FModel.RuleTypes;
    cbType.ItemIndex := FModel.RuleTypeIndex;
    edFieldName.Text := FModel.FieldNameMask;
    edFieldValue.Text := FModel.FieldValueMask;
    edMask.Text := FModel.HyperLinkMask;
    cbHighlightColor.Selected := FModel.HighlightColor;
    cbHighlightTextColor.Selected := FModel.HighlightTextColor;
    ckDisplayPasswordStrength.Checked := FModel.DisplayPasswordStrength;
    edDisplayMask.Text := FModel.DisplayMask;
    cbDateFormat.Items := FModel.DateFormats;
    cbDateFormat.ItemIndex := FModel.DateFormatIndex;
    edDateSeparator.Text := FModel.DateSeparator;
    edDaysBefore.Value := FModel.DaysBefore;
    edDaysAfter.Value := FModel.DaysAfter;
  end;
end;

procedure TRulePropsDlg.AddMacro(const AEdit: TCustomEdit; const Macro: string);
begin
  AEdit.SelText := MacroStartDelimiter + Macro + MacroEndDelimiter;
end;

procedure TRulePropsDlg.DisplayTabs;
var
  IsEvent: Boolean;
begin
  IsEvent := FModel.IsEvent;
  tsDisplay.TabVisible := IsEvent;
  tsHyperLink.TabVisible := FModel.IsHyperLink;
  tsDateFormat.TabVisible := IsEvent;
  tsCustomFilter.TabVisible := IsEvent;
  tsHighlight.TabVisible := FModel.IsHighlight;
  tsPassword.TabVisible := FModel.IsPassword;
end;

procedure TRulePropsDlg.FormShow(Sender: TObject);
begin
  DisplayTabs;
  if edName.CanFocus then edName.SetFocus;
end;

procedure TRulePropsDlg.HelpExecute(Sender: TObject);
begin
  ShellOpen(TAction(Sender).Hint);
end;

procedure TRulePropsDlg.OKExecute(Sender: TObject);
begin
  FModel.ApplyChanges;
  ModalResult := mrOk;
end;

procedure TRulePropsDlg.OKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FModel.Valid;
end;

procedure TRulePropsDlg.DisplayFieldNameMacroExecute(Sender: TObject);
begin
  AddMacro(edDisplayMask, FieldNameMacro);
end;

procedure TRulePropsDlg.DisplayFieldValueMacroExecute(Sender: TObject);
begin
  AddMacro(edDisplayMask, FieldValueMacro);
end;

procedure TRulePropsDlg.DisplayYearsMacroExecute(Sender: TObject);
begin
  AddMacro(edDisplayMask, YearsMacro);
end;

procedure TRulePropsDlg.DisplayMonthsOfYearMacroExecute(Sender: TObject);
begin
  AddMacro(edDisplayMask, MonthsOfYearMacro);
end;

procedure TRulePropsDlg.DisplayDaysOfMonthMacroExecute(Sender: TObject);
begin
  AddMacro(edDisplayMask, DaysOfMonthMacro);
end;

procedure TRulePropsDlg.DisplayMonthsMacroExecute(Sender: TObject);
begin
  AddMacro(edDisplayMask, MonthsMacro);
end;

procedure TRulePropsDlg.DisplayDaysMacroExecute(Sender: TObject);
begin
  AddMacro(edDisplayMask, DaysMacro);
end;

procedure TRulePropsDlg.HyperLinkFieldNameMacroExecute(Sender: TObject);
begin
  AddMacro(edMask, FieldNameMacro);
end;

procedure TRulePropsDlg.HyperLinkFieldValueMacroExecute(Sender: TObject);
begin
  AddMacro(edMask, FieldValueMacro);
end;

procedure TRulePropsDlg.edDateSeparatorChange(Sender: TObject);
begin
  FModel.DateSeparator := edDateSeparator.Text;
end;

procedure TRulePropsDlg.edDaysAfterChange(Sender: TObject);
begin
  FModel.DaysAfter	:= Trunc(edDaysAfter.Value);
end;

procedure TRulePropsDlg.edDaysBeforeChange(Sender: TObject);
begin
  FModel.DaysBefore	:= Trunc(edDaysBefore.Value);
end;

procedure TRulePropsDlg.edDisplayMaskChange(Sender: TObject);
begin
  FModel.DisplayMask := edDisplayMask.Text;
end;

procedure TRulePropsDlg.edFieldNameChange(Sender: TObject);
begin
  FModel.FieldNameMask := edFieldName.Text;
end;

procedure TRulePropsDlg.edFieldValueChange(Sender: TObject);
begin
  FModel.FieldValueMask := edFieldValue.Text;
end;

procedure TRulePropsDlg.edMaskChange(Sender: TObject);
begin
  FModel.HyperLinkMask := edMask.Text;
end;

procedure TRulePropsDlg.edNameChange(Sender: TObject);
begin
  FModel.RuleName := edName.Text;

  if edName.Text <> '' then
    Caption := SRulePropsDlgTitle + ' - ' + edName.Text
  else
    Caption := SRulePropsDlgTitle;
end;

procedure TRulePropsDlg.cbDateFormatChange(Sender: TObject);
begin
  FModel.DateFormatIndex := cbDateFormat.ItemIndex;
end;

procedure TRulePropsDlg.cbHighlightColorChange(Sender: TObject);
begin
  FModel.HighlightColor := cbHighlightColor.Selected;
end;

procedure TRulePropsDlg.cbHighlightTextColorChange(Sender: TObject);
begin
  FModel.HighlightTextColor := cbHighlightTextColor.Selected;
end;

procedure TRulePropsDlg.cbTypeChange(Sender: TObject);
begin
  FModel.RuleTypeIndex := cbType.ItemIndex;
  DisplayTabs;
end;

procedure TRulePropsDlg.ckDisplayPasswordStrengthClick(Sender: TObject);
begin
  FModel.DisplayPasswordStrength := ckDisplayPasswordStrength.Checked;
end;

procedure TRulePropsDlg.btDisplayMacrosClick(Sender: TObject);
begin
  btDisplayMacros.DropDown(DisplayMacrosMenu);
end;

procedure TRulePropsDlg.btHyperLinkMacrosClick(Sender: TObject);
begin
  btHyperLinkMacros.DropDown(HyperLinkMacrosMenu);
end;

end.
