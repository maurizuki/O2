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

unit uRulePropsDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, ComCtrls, ExtCtrls, Mask, JvExMask, JvSpin,
  uO2Rules, Menus, System.Actions;

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
    MaskHelpMemo: TMemo;
    cbDateFormat: TComboBox;
    tsPassword: TTabSheet;
    ckDisplayPasswordStrength: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKExecute(Sender: TObject);
    procedure OKUpdate(Sender: TObject);
    procedure DisplayFieldNameMacroExecute(Sender: TObject);
    procedure DisplayFieldValueMacroExecute(Sender: TObject);
    procedure HyperLinkFieldValueMacroExecute(Sender: TObject);
    procedure HyperLinkFieldNameMacroExecute(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure cbTypeChange(Sender: TObject);
    procedure btDisplayMacrosClick(Sender: TObject);
    procedure btHyperLinkMacrosClick(Sender: TObject);
    procedure DisplayYearsMacroExecute(Sender: TObject);
    procedure DisplayMonthsOfYearMacroExecute(Sender: TObject);
    procedure DisplayDaysOfMonthMacroExecute(Sender: TObject);
    procedure DisplayMonthsMacroExecute(Sender: TObject);
    procedure DisplayDaysMacroExecute(Sender: TObject);
  private
    FRules: TO2Rules;
    FRuleIndex: Integer;
    FDuplicate: Boolean;
    procedure AddMacro(const AEdit: TCustomEdit; const Macro: string);
  public
    class function Execute(AOwner: TComponent; Rules: TO2Rules;
      var RuleIndex: Integer; Duplicate: Boolean): Boolean;
    property Rules: TO2Rules read FRules write FRules;
    property RuleIndex: Integer read FRuleIndex write FRuleIndex;
    property Duplicate: Boolean read FDuplicate write FDuplicate;
  end;

var
  RulePropsDlg: TRulePropsDlg;

implementation

uses
  uGlobal, uCtrlHelpers;

{$R *.dfm}

resourcestring
  SMaskHelp01 = 'Valid field name mask and field value mask consist of literal characters, sets, and wildcards.';
  SMaskHelp02 = 'Each literal character must match a single character in the field name or value.';
  SMaskHelp03 = 'The comparison to literal characters is case-insensitive.';
  SMaskHelp04 = 'Each set begins with an opening bracket ([) and ends with a closing bracket (]).';
  SMaskHelp05 = 'Between the brackets are the elements of the set.';
  SMaskHelp06 = 'Each element is a literal character or a range.';
  SMaskHelp07 = 'Ranges are specified by an initial value, a dash (-), and a final value.';
  SMaskHelp08 = 'Do not use spaces or commas to separate the elements of the set.';
  SMaskHelp09 = 'A set must match a single character in the field name or value.';
  SMaskHelp10 = 'The character matches the set if it is the same as one of the literal characters in the set, or if it is in one of the ranges in the set.';
  SMaskHelp11 = 'A character is in a range if it matches the initial value, the final value, or falls between the two values.';
  SMaskHelp12 = 'All comparisons are case-insensitive.';
  SMaskHelp13 = 'If the first character after the opening bracket of a set is an exclamation point (!), then the set matches any character that is not in the set.';
  SMaskHelp14 = 'Wildcards are asterisks (*) or question marks (?).';
  SMaskHelp15 = 'An asterisk matches any number of characters.';
  SMaskHelp16 = 'A question mark matches a single arbitrary character.';

class function TRulePropsDlg.Execute(AOwner: TComponent; Rules: TO2Rules;
  var RuleIndex: Integer; Duplicate: Boolean): Boolean;
var
  Form: TRulePropsDlg;
begin
  Form := TRulePropsDlg.Create(AOwner);
  try
    Form.Rules := Rules;
    Form.RuleIndex := RuleIndex;
    Form.Duplicate := Duplicate;
    Result := Form.ShowModal = mrOk;
    if Result then RuleIndex := Form.RuleIndex;
  finally
    Form.Free;
  end;
end;

procedure TRulePropsDlg.AddMacro(const AEdit: TCustomEdit; const Macro: string);
begin
  AEdit.SelText := MacroStartDelimiter + Macro + MacroEndDelimiter;
end;

procedure TRulePropsDlg.FormCreate(Sender: TObject);
begin
  TRuleTypeLookup.Fill(cbType);
  TDateFormatLookup.Fill(cbDateFormat);

  MaskHelpMemo.Lines.Clear;
  MaskHelpMemo.Lines.Add(SMaskHelp01);
  MaskHelpMemo.Lines.Add('');
  MaskHelpMemo.Lines.Add(SMaskHelp02 + ' ' + SMaskHelp03);
  MaskHelpMemo.Lines.Add('');
  MaskHelpMemo.Lines.Add(SMaskHelp04 + ' ' + SMaskHelp05
    + ' ' + SMaskHelp06 + ' ' + SMaskHelp07 + ' ' + SMaskHelp08
    + ' ' + SMaskHelp09 + ' ' + SMaskHelp10 + ' ' + SMaskHelp11
    + ' ' + SMaskHelp12 + ' ' + SMaskHelp13);
  MaskHelpMemo.Lines.Add('');
  MaskHelpMemo.Lines.Add(SMaskHelp14 + ' ' + SMaskHelp15 + ' ' + SMaskHelp16);
end;

procedure TRulePropsDlg.FormShow(Sender: TObject);
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := TFormatSettings.Create;
  if RuleIndex <> -1 then
    with Rules[RuleIndex] do
    begin
      if not Duplicate then edName.Text := Name;
      TRuleTypeLookup.Select(cbType, Integer(RuleType));
      edFieldName.Text := FieldName;
      edFieldValue.Text := FieldValue;
      edDisplayMask.Text := Params.Values[DisplayMaskParam];
      edMask.Text := Params.Values[HyperLinkMaskParam];
      edDateSeparator.Text := Params.StrValue(DateSeparatorParam,
        FormatSettings.DateSeparator);
      TDateFormatLookup.Select(cbDateFormat,
        Params.StrValue(ShortDateFormatParam, FormatSettings.ShortDateFormat));
      edDaysBefore.Value := Params.IntValue(DaysBeforeParam, DefaultDaysBefore);
      edDaysAfter.Value := Params.IntValue(DaysAfterParam, DefaultDaysAfter);
      cbHighlightColor.Selected :=
        Params.IntValue(HighlightColorParam, DefaultHighlightColor);
      cbHighlightTextColor.Selected :=
        Params.IntValue(HighlightTextColorParam, DefaultHighlightTextColor);
      ckDisplayPasswordStrength.Checked := Params.BoolValue(
        DisplayPasswordStrengthParam, DefaultDisplayPasswordStrength);
    end
  else
  begin
    edDateSeparator.Text := FormatSettings.DateSeparator;
    TDateFormatLookup.Select(cbDateFormat, FormatSettings.ShortDateFormat);
    edDaysBefore.Value := DefaultDaysBefore;
    edDaysAfter.Value := DefaultDaysAfter;
    cbHighlightColor.Selected := DefaultHighlightColor;
    cbHighlightTextColor.Selected := DefaultHighlightTextColor;
  end;

  if Duplicate then RuleIndex := -1;
  cbTypeChange(cbType);
  if edName.CanFocus then edName.SetFocus;
end;

procedure TRulePropsDlg.OKExecute(Sender: TObject);
var
  ARule: TO2Rule;
begin
  if RuleIndex = -1 then
  begin
    ARule := Rules.AddRule(edName.Text);
    ARule.Active := True;
  end
  else
  begin
    ARule := Rules[RuleIndex];
    ARule.Name := edName.Text;
  end;
  ARule.RuleType := TO2RuleType(TRuleTypeLookup.SelectedValue(cbType));
  ARule.FieldName := edFieldName.Text;
  ARule.FieldValue := edFieldValue.Text;
  if ARule.RuleType = rtHyperLink then
    ARule.Params.Values[HyperLinkMaskParam] := edMask.Text;
  if ARule.RuleType in EventRules then
  begin
    ARule.Params.Values[DisplayMaskParam] := edDisplayMask.Text;
    ARule.Params.Values[DateSeparatorParam] := edDateSeparator.Text;
    ARule.Params.Values[ShortDateFormatParam] :=
      TDateFormatLookup.SelectedValue(cbDateFormat);
    ARule.Params.Values[DaysBeforeParam] := edDaysBefore.Text;
    ARule.Params.Values[DaysAfterParam] := edDaysAfter.Text;
  end;
  if ARule.RuleType in HighlightRules then
  begin
    ARule.Params.Values[HighlightColorParam] :=
      IntToStr(cbHighlightColor.Selected);
    ARule.Params.Values[HighlightTextColorParam] :=
      IntToStr(cbHighlightTextColor.Selected);
  end;
  if ARule.RuleType = rtPassword then
    ARule.Params.Values[DisplayPasswordStrengthParam] :=
      BoolToStr(ckDisplayPasswordStrength.Checked);
  RuleIndex := ARule.Index;

  ModalResult := mrOk;
end;

procedure TRulePropsDlg.OKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (edName.Text <> '') and (cbType.ItemIndex <> -1);
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

procedure TRulePropsDlg.edNameChange(Sender: TObject);
begin
  if edName.Text <> '' then
    Caption := SRulePropsDlgTitle + ' - ' + edName.Text
  else
    Caption := SRulePropsDlgTitle;
end;

procedure TRulePropsDlg.cbTypeChange(Sender: TObject);
var
  RuleType: TO2RuleType;
begin
  RuleType := TO2RuleType(TRuleTypeLookup.SelectedValue(cbType));
  tsDisplay.TabVisible := RuleType in EventRules;
  tsHyperLink.TabVisible := RuleType = rtHyperLink;
  tsDateFormat.TabVisible := RuleType in EventRules;
  tsCustomFilter.TabVisible := RuleType in EventRules;
  tsHighlight.TabVisible := RuleType in HighlightRules;
  tsPassword.TabVisible := RuleType = rtPassword;
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
