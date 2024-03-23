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

unit uRuleModels;

interface

uses
  Classes, Graphics, uServices, uO2File, uO2Rules;

type
  TRulePropsModel = class(TInterfacedObject, IRuleProps)
  private
    FRuleTypes: TStrings;
    FDateFormats: TStrings;
    function GetRuleName: string;
    function GetRuleTypes: TStrings;
    function GetRuleTypeIndex: Integer;
    function GetFieldNameMask: string;
    function GetFieldValueMask: string;
    function GetHyperLinkMask: string;
    function GetDisplayPasswordStrength: Boolean;
    function GetDisplayMask: string;
    function GetDateFormats: TStrings;
    function GetDateFormatIndex: Integer;
    function GetDateSeparator: string;
    function GetDaysBefore: Integer;
    function GetDaysAfter: Integer;
    function GetHighlightColor: TColor;
    function GetHighlightTextColor: TColor;
    function GetRule: TO2Rule;
    function GetValid: Boolean;
    procedure SetRuleName(const Value: string);
    procedure SetRuleTypeIndex(const Value: Integer);
    procedure SetFieldNameMask(const Value: string);
    procedure SetFieldValueMask(const Value: string);
    procedure SetHyperLinkMask(const Value: string);
    procedure SetDisplayPasswordStrength(const Value: Boolean);
    procedure SetDisplayMask(const Value: string);
    procedure SetDateFormatIndex(Value: Integer);
    procedure SetDateSeparator(const Value: string);
    procedure SetDaysBefore(const Value: Integer);
    procedure SetDaysAfter(const Value: Integer);
    procedure SetHighlightColor(const Value: TColor);
    procedure SetHighlightTextColor(const Value: TColor);
  protected
    FO2File: TO2File;
    FRule: TO2Rule;
    FRuleName: string;
    FRuleTypeIndex: Integer;
    FFieldNameMask: string;
    FFieldValueMask: string;
    FHyperLinkMask: string;
    FDisplayPasswordStrength: Boolean;
    FDisplayMask: string;
    FDateFormatIndex: Integer;
    FDateSeparator: string;
    FDaysBefore: Integer;
    FDaysAfter: Integer;
    FHighlightColor: TColor;
    FHighlightTextColor: TColor;
    procedure Build;
    procedure EnsureAssigned;
  public
    constructor Create;
    destructor Destroy; override;

    function IsHyperLink: Boolean;
    function IsPassword: Boolean;
    function IsEvent: Boolean;
    function IsHighlight: Boolean;

    procedure ApplyChanges; virtual;

    property RuleName: string read GetRuleName write SetRuleName;
    property RuleTypes: TStrings read GetRuleTypes;
    property RuleTypeIndex: Integer read GetRuleTypeIndex
      write SetRuleTypeIndex;
    property FieldNameMask: string read GetFieldNameMask
      write SetFieldNameMask;
    property FieldValueMask: string read GetFieldValueMask
      write SetFieldValueMask;
    property HyperLinkMask: string read GetHyperLinkMask
      write SetHyperLinkMask;
    property DisplayPasswordStrength: Boolean read GetDisplayPasswordStrength
      write SetDisplayPasswordStrength;
    property DisplayMask: string read GetDisplayMask write SetDisplayMask;
    property DateFormats: TStrings read GetDateFormats;
    property DateFormatIndex: Integer read GetDateFormatIndex
      write SetDateFormatIndex;
    property DateSeparator: string read GetDateSeparator write SetDateSeparator;
    property DaysBefore: Integer read GetDaysBefore write SetDaysBefore;
    property DaysAfter: Integer read GetDaysAfter write SetDaysAfter;
    property HighlightColor: TColor read GetHighlightColor
      write SetHighlightColor;
    property HighlightTextColor: TColor read GetHighlightTextColor
      write SetHighlightTextColor;

    property Rule: TO2Rule read GetRule;

    property Valid: Boolean read GetValid;
  end;

  TNewRuleModel = class(TRulePropsModel)
  public
    constructor Create(const O2File: TO2File);

    procedure ApplyChanges; override;
  end;

  TDuplicateRuleModel = class(TRulePropsModel)
  public
    constructor Create(const O2File: TO2File; const Source: TO2Rule);

    procedure ApplyChanges; override;
  end;

  TEditRuleModel = class(TRulePropsModel)
  public
    constructor Create(const Rule: TO2Rule);
  end;

implementation

uses
  SysUtils, uGlobal;

type
  TRuleTypeRange = 0..5;
  TDateFormatRange = 0..2;

const
  RuleTypeValues: array[TRuleTypeRange] of TO2RuleType = (
    rtHyperLink,
    rtEmail,
    rtPassword,
    rtExpirationDate,
    rtRecurrence,
    rtHighlight
    );

  RuleTypeDescriptions: array[TRuleTypeRange] of string = (
    SRuleHyperLink,
    SRuleEmail,
    SRulePassword,
    SRuleExpirationDate,
    SRuleRecurrence,
    SRuleHighlight
    );

  DateFormatValues: array[TDateFormatRange] of string = (
    'yyyy/mm/dd', 'mm/dd/yyyy', 'dd/mm/yyyy');

  DateFormatDescriptions: array[TDateFormatRange] of string = (
    SYMD, SMDY, SDMY);

{ TRulePropsModel }

procedure TRulePropsModel.Build;
var
  FormatSettings: TFormatSettings;
  DateOrder: string;

function GetDateOrder(const S: string): string;
var
  C: Char;
begin
  Result := '';
  for C in S do
    if CharInSet(C, ['y', 'm', 'd']) and (Pos(C, Result) = 0) then
      Result := Result + C;
end;

begin
  FRuleName := FRule.Name;

  FRuleTypeIndex := High(RuleTypeValues);
  while (FRuleTypeIndex > Low(RuleTypeValues))
    and (RuleTypeValues[FRuleTypeIndex] <> FRule.RuleType) do
    Dec(FRuleTypeIndex);

  FFieldNameMask := FRule.FieldName;
  FFieldValueMask := FRule.FieldValue;

  FHyperLinkMask := FRule.Params.ReadString(HyperLinkMaskParam);

  FDisplayPasswordStrength := FRule.Params.ReadBoolean(
    DisplayPasswordStrengthParam, DefaultDisplayPasswordStrength);

  FDisplayMask := FRule.Params.ReadString(DisplayMaskParam);

  FormatSettings := TFormatSettings.Create;

  DateOrder := GetDateOrder(LowerCase(FRule.Params.ReadString(
    ShortDateFormatParam, FormatSettings.ShortDateFormat)));
  FDateFormatIndex := High(DateFormatValues);
  while (FDateFormatIndex > Low(DateFormatValues))
    and (GetDateOrder(DateFormatValues[FDateFormatIndex]) <> DateOrder)
    do Dec(FDateFormatIndex);

  FDateSeparator := FRule.Params.ReadString(DateSeparatorParam,
    FormatSettings.DateSeparator);

  FDaysBefore := FRule.Params.ReadInteger(DaysBeforeParam, DefaultDaysBefore);
  FDaysAfter := FRule.Params.ReadInteger(DaysAfterParam, DefaultDaysAfter);

  FHighlightColor := FRule.Params.ReadInteger(HighlightColorParam,
    DefaultHighlightColor);
  FHighlightTextColor := FRule.Params.ReadInteger(HighlightTextColorParam,
    DefaultHighlightTextColor);
end;

constructor TRulePropsModel.Create;
begin
  FRuleTypes := TStringList.Create;
  FRuleTypes.AddStrings(RuleTypeDescriptions);

  FDateFormats := TStringList.Create;
  FDateFormats.AddStrings(DateFormatDescriptions);
end;

destructor TRulePropsModel.Destroy;
begin
  FRuleTypes.Free;
  inherited;
end;

procedure TRulePropsModel.EnsureAssigned;
begin
  if Assigned(FRule) then Exit;
  FRule := FO2File.Rules.AddRule(FRuleName);
  FRule.Active := True;
end;

procedure TRulePropsModel.ApplyChanges;
begin
  FRule.Name := FRuleName;
  FRule.RuleType := RuleTypeValues[FRuleTypeIndex];
  FRule.FieldName := FFieldNameMask;
  FRule.FieldValue := FFieldValueMask;

  if FRule.RuleType = rtHyperLink then
    FRule.Params.Values[HyperLinkMaskParam] := FHyperLinkMask
  else
    FRule.Params.DeleteParam(HyperLinkMaskParam);

  if FRule.RuleType = rtPassword then
    FRule.Params.Values[DisplayPasswordStrengthParam] :=
      BoolToStr(FDisplayPasswordStrength)
  else
    FRule.Params.DeleteParam(DisplayPasswordStrengthParam);

  if FRule.RuleType in EventRules then
  begin
    FRule.Params.Values[DisplayMaskParam] := FDisplayMask;
    FRule.Params.Values[ShortDateFormatParam] :=
      DateFormatValues[FDateFormatIndex];
    FRule.Params.Values[DateSeparatorParam] := FDateSeparator;
    FRule.Params.Values[DaysBeforeParam] := IntToStr(FDaysBefore);
    FRule.Params.Values[DaysAfterParam] := IntToStr(FDaysAfter);
  end
  else
  begin
    FRule.Params.DeleteParam(DisplayMaskParam);
    FRule.Params.DeleteParam(ShortDateFormatParam);
    FRule.Params.DeleteParam(DateSeparatorParam);
    FRule.Params.DeleteParam(DaysBeforeParam);
    FRule.Params.DeleteParam(DaysAfterParam);
  end;

  if FRule.RuleType in HighlightRules then
  begin
    FRule.Params.Values[HighlightColorParam] := IntToStr(FHighlightColor);
    FRule.Params.Values[HighlightTextColorParam] :=
      IntToStr(FHighlightTextColor);
  end
  else
  begin
    FRule.Params.DeleteParam(HighlightColorParam);
    FRule.Params.DeleteParam(HighlightTextColorParam);
  end;
end;

function TRulePropsModel.GetDateFormatIndex: Integer;
begin
  Result := FDateFormatIndex;
end;

function TRulePropsModel.GetDateFormats: TStrings;
begin
  Result := FDateFormats;
end;

function TRulePropsModel.GetDateSeparator: string;
begin
  Result := FDateSeparator;
end;

function TRulePropsModel.GetDaysAfter: Integer;
begin
  Result := FDaysAfter;
end;

function TRulePropsModel.GetDaysBefore: Integer;
begin
  Result := FDaysBefore;
end;

function TRulePropsModel.GetDisplayMask: string;
begin
  Result := FDisplayMask;
end;

function TRulePropsModel.GetDisplayPasswordStrength: Boolean;
begin
  Result := FDisplayPasswordStrength;
end;

function TRulePropsModel.GetFieldNameMask: string;
begin
  Result := FFieldNameMask;
end;

function TRulePropsModel.GetFieldValueMask: string;
begin
  Result := FFieldValueMask;
end;

function TRulePropsModel.GetHighlightColor: TColor;
begin
  Result := FHighlightColor;
end;

function TRulePropsModel.GetHighlightTextColor: TColor;
begin
  Result := FHighlightTextColor;
end;

function TRulePropsModel.GetHyperLinkMask: string;
begin
  Result := FHyperLinkMask;
end;

function TRulePropsModel.GetRule: TO2Rule;
begin
  Result := FRule;
end;

function TRulePropsModel.GetRuleName: string;
begin
  Result := FRuleName;
end;

function TRulePropsModel.GetRuleTypeIndex: Integer;
begin
  Result := FRuleTypeIndex;
end;

function TRulePropsModel.GetRuleTypes: TStrings;
begin
  Result := FRuleTypes;
end;

function TRulePropsModel.GetValid: Boolean;
begin
  Result := RuleName <> '';
end;

function TRulePropsModel.IsEvent: Boolean;
begin
  Result := RuleTypeValues[FRuleTypeIndex] in EventRules;
end;

function TRulePropsModel.IsHighlight: Boolean;
begin
  Result := RuleTypeValues[FRuleTypeIndex] in HighlightRules;
end;

function TRulePropsModel.IsHyperLink: Boolean;
begin
  Result := RuleTypeValues[FRuleTypeIndex] = rtHyperLink;
end;

function TRulePropsModel.IsPassword: Boolean;
begin
  Result := RuleTypeValues[FRuleTypeIndex] = rtPassword;
end;

procedure TRulePropsModel.SetDateFormatIndex(Value: Integer);
begin
  FDateFormatIndex := Value;
end;

procedure TRulePropsModel.SetDateSeparator(const Value: string);
begin
  FDateSeparator := Value;
end;

procedure TRulePropsModel.SetDaysAfter(const Value: Integer);
begin
  FDaysAfter := Value;
end;

procedure TRulePropsModel.SetDaysBefore(const Value: Integer);
begin
  FDaysBefore := Value;
end;

procedure TRulePropsModel.SetDisplayMask(const Value: string);
begin
  FDisplayMask := Value;
end;

procedure TRulePropsModel.SetDisplayPasswordStrength(const Value: Boolean);
begin
  FDisplayPasswordStrength := Value;
end;

procedure TRulePropsModel.SetFieldNameMask(const Value: string);
begin
  FFieldNameMask := Value;
end;

procedure TRulePropsModel.SetFieldValueMask(const Value: string);
begin
  FFieldValueMask := Value;
end;

procedure TRulePropsModel.SetHighlightColor(const Value: TColor);
begin
  FHighlightColor := Value;
end;

procedure TRulePropsModel.SetHighlightTextColor(const Value: TColor);
begin
  FHighlightTextColor := Value;
end;

procedure TRulePropsModel.SetHyperLinkMask(const Value: string);
begin
  FHyperLinkMask := Value;
end;

procedure TRulePropsModel.SetRuleName(const Value: string);
begin
  FRuleName := Value;
end;

procedure TRulePropsModel.SetRuleTypeIndex(const Value: Integer);
begin
  FRuleTypeIndex := Value;
end;

{ TNewRuleModel }

constructor TNewRuleModel.Create(const O2File: TO2File);
var
  FormatSettings: TFormatSettings;
begin
  inherited Create;
  FO2File := O2File;
  FDisplayPasswordStrength := DefaultDisplayPasswordStrength;
  FormatSettings := TFormatSettings.Create;
  FDateSeparator := FormatSettings.DateSeparator;
  FDaysBefore := DefaultDaysBefore;
  FDaysAfter := DefaultDaysAfter;
  FHighlightColor := DefaultHighlightColor;
  FHighlightTextColor := DefaultHighlightTextColor;
end;

procedure TNewRuleModel.ApplyChanges;
begin
  EnsureAssigned;
  inherited;
end;

{ TDuplicateRuleModel }

constructor TDuplicateRuleModel.Create(const O2File: TO2File;
  const Source: TO2Rule);
begin
  inherited Create;
  FO2File := O2File;
  FRule := Source;
  Build;
  FRule := nil;
  FRuleName := '';
end;

procedure TDuplicateRuleModel.ApplyChanges;
begin
  EnsureAssigned;
  inherited;
end;

{ TEditRuleModel }

constructor TEditRuleModel.Create(const Rule: TO2Rule);
begin
  inherited Create;
  FRule := Rule;
  Build;
end;

end.
