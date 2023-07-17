﻿{ ******************************************************************** }
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

unit uO2Rules;

interface

uses
  Classes, Windows, SysUtils, Graphics, System.Generics.Collections,
  uO2Classes, uO2Objects;

const
  PasswordChar = '●';

{ Rule param names }

  HyperLinkMaskParam = 'Mask';
  DisplayMaskParam = 'DisplayMask';
  DateSeparatorParam = 'DateSeparator';
  ShortDateFormatParam = 'ShortDateFormat';
  DaysBeforeParam = 'DaysBefore';
  DaysAfterParam = 'DaysAfter';
  HighlightColorParam = 'Color';
  HighlightTextColorParam = 'TextColor';

{ Macro symbols for mask strings }

  LegacyMacroStartDelimiter = '<';
  LegacyMacroEndDelimiter = '>';

  LegacyFieldNameMacro = 'fieldname';
  LegacyFieldValueMacro = 'fieldvalue';

  MacroStartDelimiter = '{';
  MacroEndDelimiter = '}';

  FieldNameMacro = 'fn';
  FieldValueMacro = 'fv';
  YearsMacro = 'years';
  MonthsOfYearMacro = 'monthsof';
  DaysOfMonthMacro = 'daysof';
  MonthsMacro = 'months';
  DaysMacro = 'days';

{ Rule param defaults }

  DefaultDaysBefore = 15;
  DefaultDaysAfter = 60;
  DefaultHighlightColor = clYellow;
  DefaultHighlightTextColor = clBlack;
  DefaultExpirationDateMask =
    MacroStartDelimiter + FieldValueMacro + MacroEndDelimiter + #32
    + '(' + MacroStartDelimiter + DaysMacro + MacroEndDelimiter + ')';
  DefaultRecurrenceMask =
    MacroStartDelimiter + FieldValueMacro + MacroEndDelimiter + #32
    + '(' + MacroStartDelimiter + YearsMacro + MacroEndDelimiter + ')';

{ Password score colors }

  PasswordScore0Color = TColor($00241CED);
  PasswordScore0TextColor = clBlack;
  PasswordScore1Color = TColor($00277FFF);
  PasswordScore1TextColor = clBlack;
  PasswordScore2Color = TColor($000EC9FF);
  PasswordScore2TextColor = clBlack;
  PasswordScore3Color = TColor($00E8A200);
  PasswordScore3TextColor = clBlack;
  PasswordScore4Color = TColor($004CB122);
  PasswordScore4TextColor = clBlack;

type

{ Rule types }

  TO2RuleType = (
    rtNone,
    rtHyperLink,
    rtEmail,
    rtPassword,
    rtExpirationDate,
    rtRecurrence,
    rtHighlight
    );

  TO2RuleTypes = set of TO2RuleType;

const

{ Rule sets }

  HyperlinkRules = [rtHyperLink, rtEmail];
  EventRules = [rtExpirationDate, rtRecurrence];
  HighlightRules = [rtExpirationDate, rtRecurrence, rtHighlight];

type
  TO2Param = class(TO2CollectionItem)
  private
    FParamName: string;
    FParamValue: string;
    procedure SetParamName(const Value: string);
    procedure SetParamValue(const Value: string);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ParamName: string read FParamName write SetParamName;
    property ParamValue: string read FParamValue write SetParamValue;
  end;

  TO2ParamsEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TO2Param;
    property Current: TO2Param read GetCurrent;
  end;

  TO2Params = class(TO2Collection)
  private
    function GetParams(Index: Integer): TO2Param;
    function GetValues(Name: string): string;
    procedure SetValues(Name: string; const Value: string);
  public
    constructor Create(AOwner: TPersistent);
    function GetEnumerator: TO2ParamsEnumerator;
    function FindParam(const ParamName: string): TO2Param;
    function ParamExists(const ParamName: string): Boolean;
    function AddParam(const ParamName: string): TO2Param;
    procedure DeleteParam(const ParamName: string);
    function IntValue(const ParamName: string; DefaultValue: Integer): Integer;
    function StrValue(const ParamName, DefaultValue: string): string;
    property Params[Index: Integer]: TO2Param read GetParams; default;
    property Values[Name: string]: string read GetValues write SetValues;
  end;

  TO2Rule = class(TO2CollectionItem)
  private
    FName: string;
    FRuleType: TO2RuleType;
    FFieldName: string;
    FFieldValue: string;
    FParams: TO2Params;
    FActive: Boolean;
    procedure SetName(const Value: string);
    procedure SetRuleType(const Value: TO2RuleType);
    procedure SetFieldName(const Value: string);
    procedure SetFieldValue(const Value: string);
    procedure SetParams(const Value: TO2Params);
    procedure SetActive(const Value: Boolean);
  protected
    function GetFormatSettings: TFormatSettings;
    function GetEventDisplayText(const AField: TO2Field): string;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Matches(const AField: TO2Field): Boolean;
    function GetDisplayText(const AField: TO2Field;
      ShowPasswords: Boolean): string;
    function GetHyperLink(const AField: TO2Field): string;
    function CheckEvents(const AField: TO2Field; Date1,
      Date2: TDateTime; UseParams: Boolean = False): Boolean;
    function GetFirstEvent(const AField: TO2Field;
      out FirstDate: TDateTime): Boolean;
    function GetNextEvent(const AField: TO2Field; StartDate: TDateTime;
      out NextDate: TDateTime; UseParams: Boolean = False): Boolean;
    function GetHighlightColors(const AField: TO2Field;
      const PasswordScores: TDictionary<string, Integer>;
      out Color, TextColor: TColor): Boolean;
  published
    property Name: string read FName write SetName;
    property RuleType: TO2RuleType read FRuleType write SetRuleType;
    property FieldName: string read FFieldName write SetFieldName;
    property FieldValue: string read FFieldValue write SetFieldValue;
    property Params: TO2Params read FParams write SetParams;
    property Active: Boolean read FActive write SetActive;
  end;

  TO2RulesEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TO2Rule;
    property Current: TO2Rule read GetCurrent;
  end;

  TO2Rules = class(TO2Collection)
  private
    function GetRules(Index: Integer): TO2Rule;
  public
    constructor Create(AOwner: TPersistent);
    function GetEnumerator: TO2RulesEnumerator;
    function FindRule(const Name: string): TO2Rule;
    function FindFirstRule(const AField: TO2Field;
      RuleTypes: TO2RuleTypes): TO2Rule;
    function RuleExists(const Name: string): Boolean;
    function AddRule(const Name: string): TO2Rule;
    procedure DeleteRule(const Name: string);
    function ImportRule(const ARule: TO2Rule): TO2Rule;
    function GetDisplayText(const AField: TO2Field;
      ShowPasswords: Boolean): string;
    function GetHyperLink(const AField: TO2Field): string;
    function CheckEvents(const AField: TO2Field; Date1,
      Date2: TDateTime; UseParams: Boolean = False): Boolean; overload;
    function CheckEvents(const AObject: TO2Object; Date1,
      Date2: TDateTime; UseParams: Boolean = False): Boolean; overload;
    function GetNextEvent(const AField: TO2Field; StartDate: TDateTime;
      out NextDate: TDateTime; UseParams: Boolean = False): Boolean; overload;
    function GetNextEvent(const AObject: TO2Object; StartDate: TDateTime;
      out NextDate: TDateTime; UseParams: Boolean = False): Boolean; overload;
    function GetHighlightColors(const AField: TO2Field;
      const PasswordScores: TDictionary<string, Integer>;
      out Color, TextColor: TColor): Boolean; overload;
    function GetHighlightColors(const AObject: TO2Object;
      const PasswordScores: TDictionary<string, Integer>;
      out Color, TextColor: TColor): Boolean; overload;
    property Rules[Index: Integer]: TO2Rule read GetRules; default;
  end;

implementation

uses
  Masks, DateUtils, uO2File, uO2Utils;

resourcestring
  SRuleAlreadyExists = 'A rule named "%s" already exists.';
  SParamAlreadyExists = 'A parameter named "%s" already exists.';

{ TO2Param }

constructor TO2Param.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FParamName := '';
  FParamValue := '';
end;

procedure TO2Param.Assign(Source: TPersistent);
begin
  if Source is TO2Param then
  begin
    ParamName := TO2Param(Source).ParamName;
    ParamValue := TO2Param(Source).ParamValue;
  end
  else
    inherited Assign(Source);
end;

procedure TO2Param.SetParamName(const Value: string);
var
  AParam: TO2Param;
begin
  if FParamName <> Value then
  begin
    AParam := TO2Params(Collection).FindParam(Value);
    if Assigned(AParam) and (AParam <> Self) then
      raise Exception.CreateFmt(SParamAlreadyExists, [Value]);
    FParamName := Value;
    NotifyChanges(Self, onPropertyChanged);
  end;
end;

procedure TO2Param.SetParamValue(const Value: string);
begin
  if FParamValue <> Value then
  begin
    FParamValue := Value;
    NotifyChanges(Self, onPropertyChanged);
  end;
end;

{ TO2ParamsEnumerator }

function TO2ParamsEnumerator.GetCurrent: TO2Param;
begin
  Result := TO2Param(inherited GetCurrent);
end;

{ TO2Params }

constructor TO2Params.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TO2Param);
end;

function TO2Params.GetEnumerator: TO2ParamsEnumerator;
begin
  Result := TO2ParamsEnumerator.Create(Self);
end;

function TO2Params.FindParam(const ParamName: string): TO2Param;
var
  AParam: TO2Param;
begin
  Result := nil;
  for AParam in Self do
    if SameText(AParam.ParamName, ParamName) then
    begin
      Result := AParam;
      Break;
    end;
end;

function TO2Params.ParamExists(const ParamName: string): Boolean;
begin
  Result := Assigned(FindParam(ParamName));
end;

function TO2Params.AddParam(const ParamName: string): TO2Param;
begin
  Result := TO2Param(Add);
  try
    Result.ParamName := ParamName;
  except
    Delete(Result.Index);
    raise;
  end;
end;

procedure TO2Params.DeleteParam(const ParamName: string);
var
  AParam: TO2Param;
begin
  AParam := FindParam(ParamName);
  if Assigned(AParam) then
    Delete(AParam.Index);
end;

function TO2Params.IntValue(const ParamName: string;
  DefaultValue: Integer): Integer;
var
  AParam: TO2Param;
begin
  AParam := FindParam(ParamName);
  if Assigned(AParam) then
    Result := StrToIntDef(AParam.ParamValue, DefaultValue)
  else
    Result := DefaultValue;
end;

function TO2Params.StrValue(const ParamName, DefaultValue: string): string;
var
  AParam: TO2Param;
begin
  AParam := FindParam(ParamName);
  if Assigned(AParam) then
    Result := AParam.ParamValue
  else
    Result := DefaultValue;
end;

function TO2Params.GetParams(Index: Integer): TO2Param;
begin
  Result := TO2Param(Items[Index]);
end;

function TO2Params.GetValues(Name: string): string;
var
  AParam: TO2Param;
begin
  AParam := FindParam(Name);
  if Assigned(AParam) then
    Result := AParam.ParamValue
  else
    Result := '';
end;

procedure TO2Params.SetValues(Name: string; const Value: string);
var
  AParam: TO2Param;
begin
  AParam := FindParam(Name);
  if AParam = nil then AParam := AddParam(Name);
  AParam.ParamValue := Value;
end;

{ TO2Rule }

constructor TO2Rule.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FName := '';
  FRuleType := rtNone;
  FFieldName := '';
  FFieldValue := '';
  FParams := TO2Params.Create(Self);
  FActive := False;
end;

destructor TO2Rule.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

procedure TO2Rule.Assign(Source: TPersistent);
begin
  if Source is TO2Rule then
  begin
    Name := TO2Rule(Source).Name;
    RuleType := TO2Rule(Source).RuleType;
    FieldName := TO2Rule(Source).FieldName;
    FieldValue := TO2Rule(Source).FieldValue;
    Params := TO2Rule(Source).Params;
    Active := TO2Rule(Source).Active;
  end
  else
    inherited Assign(Source);
end;

function TO2Rule.Matches(const AField: TO2Field): Boolean;
begin
  try
    Result := Active
      and MatchesMask(AField.FieldName, FieldName)
      and MatchesMask(AField.FieldValue, FieldValue);
  except
    Result := False;
  end;
end;

function TO2Rule.GetEventDisplayText(const AField: TO2Field): string;
var
  MacroProcessor: TMacroProcessor;
  Years, MonthsOfYear, DaysOfMonth: Word;
  Months, Days: Integer;
  ADate: TDateTime;
begin
  if TryStrToDate(AField.FieldValue, ADate, GetFormatSettings)
    and ((RuleType = rtExpirationDate) and (ADate >= Date)
    or (RuleType = rtRecurrence) and (ADate <= Date)) then
  begin
    DateSpan(Date, ADate, Years, MonthsOfYear, DaysOfMonth);
    Months := MonthsBetween(Date, ADate);
    Days := DaysBetween(Date, ADate);
    MacroProcessor := TMacroProcessor.Create;
    try
      MacroProcessor.StartDelimiter := MacroStartDelimiter;
      MacroProcessor.EndDelimiter := MacroEndDelimiter;
      MacroProcessor.Macros.Values[FieldNameMacro] := AField.FieldName;
      MacroProcessor.Macros.Values[FieldValueMacro] := AField.FieldValue;
      MacroProcessor.Macros.Values[YearsMacro] := IntToStr(Years);
      MacroProcessor.Macros.Values[MonthsOfYearMacro] := IntToStr(MonthsOfYear);
      MacroProcessor.Macros.Values[DaysOfMonthMacro] := IntToStr(DaysOfMonth);
      MacroProcessor.Macros.Values[MonthsMacro] := IntToStr(Months);
      MacroProcessor.Macros.Values[DaysMacro] := IntToStr(Days);
      if Params.Values[DisplayMaskParam] = '' then
        case RuleType of
          rtExpirationDate:
            Result := MacroProcessor.ExpandMacros(DefaultExpirationDateMask);
          rtRecurrence:
            Result := MacroProcessor.ExpandMacros(DefaultRecurrenceMask);
        end
      else
        Result := MacroProcessor.ExpandMacros(Params.Values[DisplayMaskParam]);
    finally
      MacroProcessor.Free;
    end;
  end
  else
    Result := AField.FieldValue;
end;

function TO2Rule.GetDisplayText(const AField: TO2Field;
  ShowPasswords: Boolean): string;
begin
  case RuleType of
    rtPassword:
      if ShowPasswords then
        Result := AField.FieldValue
      else
        Result := StringOfChar(PasswordChar, Length(AField.FieldValue));
    rtExpirationDate, rtRecurrence:
      Result := GetEventDisplayText(AField);
    else
      Result := AField.FieldValue;
  end;
end;

function TO2Rule.GetHyperLink(const AField: TO2Field): string;
var
  EncodedFieldName, EncodedFieldValue: string;
  MacroProcessor: TMacroProcessor;
begin
  if Params.Values[HyperLinkMaskParam] = '' then
    Result := AField.FieldValue
  else
  begin
    Result := Params.Values[HyperLinkMaskParam];
    EncodedFieldName := UrlEscape(AField.FieldName);
    EncodedFieldValue := UrlEscape(AField.FieldValue);
    MacroProcessor := TMacroProcessor.Create;
    try
      MacroProcessor.StartDelimiter := LegacyMacroStartDelimiter;
      MacroProcessor.EndDelimiter := LegacyMacroEndDelimiter;
      MacroProcessor.Macros.Values[LegacyFieldNameMacro] := EncodedFieldName;
      MacroProcessor.Macros.Values[LegacyFieldValueMacro] := EncodedFieldValue;
      Result := MacroProcessor.ExpandMacros(Result);

      MacroProcessor.StartDelimiter := MacroStartDelimiter;
      MacroProcessor.EndDelimiter := MacroEndDelimiter;
      MacroProcessor.Macros.Clear;
      MacroProcessor.Macros.Values[FieldNameMacro] := EncodedFieldName;
      MacroProcessor.Macros.Values[FieldValueMacro] := EncodedFieldValue;
      Result := MacroProcessor.ExpandMacros(Result);
    finally
      MacroProcessor.Free;
    end;
  end;
end;

function TO2Rule.CheckEvents(const AField: TO2Field; Date1,
  Date2: TDateTime; UseParams: Boolean): Boolean;
var
  DateValue, DateMin, DateMax: TDateTime;
begin
  Result := False;
  if (RuleType in EventRules) and Matches(AField)
    and TryStrToDate(AField.FieldValue, DateValue, GetFormatSettings) then
  begin
    if UseParams then
    begin
      Date1 := Date - Params.IntValue(DaysBeforeParam, DefaultDaysBefore);
      Date2 := Date + Params.IntValue(DaysAfterParam, DefaultDaysAfter);
    end;

    case RuleType of
      rtExpirationDate:
        Result := (DateValue >= Date1) and (DateValue <= Date2);

      rtRecurrence:
      begin
        DateMin := SafeRecodeYear(DateValue, YearOf(Date1));
        DateMax := SafeRecodeYear(DateValue, YearOf(Date2));
        Result := (DateMin >= Date1) and (DateMin <= Date2)
          or (DateMax >= Date1) and (DateMax <= Date2);
      end;
    end;
  end;
end;

function TO2Rule.GetFirstEvent(const AField: TO2Field;
  out FirstDate: TDateTime): Boolean;
begin
  Result := (RuleType in EventRules) and Matches(AField)
    and TryStrToDate(AField.FieldValue, FirstDate, GetFormatSettings);
end;

function TO2Rule.GetNextEvent(const AField: TO2Field; StartDate: TDateTime;
  out NextDate: TDateTime; UseParams: Boolean): Boolean;
var
  FirstDate: TDateTime;
begin
  Result := False;
  if GetFirstEvent(AField, FirstDate) then
  begin
    case RuleType of
      rtExpirationDate:
        NextDate := FirstDate;

      rtRecurrence:
      begin
        if UseParams then
          StartDate := Date - Params.IntValue(DaysBeforeParam,
            DefaultDaysBefore);

        NextDate := SafeRecodeYear(FirstDate, YearOf(StartDate));

        if NextDate < StartDate then
          NextDate := SafeRecodeYear(FirstDate, YearOf(IncYear(StartDate)));
      end;
    end;

    Result := True;
  end;
end;

function TO2Rule.GetHighlightColors(const AField: TO2Field;
  const PasswordScores: TDictionary<string, Integer>;
  out Color, TextColor: TColor): Boolean;
const
  PasswordScoreColors: array [0..4] of TColor = (
    PasswordScore0Color,
    PasswordScore1Color,
    PasswordScore2Color,
    PasswordScore3Color,
    PasswordScore4Color
  );
  PasswordScoreTextColors: array [0..4] of TColor = (
    PasswordScore0TextColor,
    PasswordScore1TextColor,
    PasswordScore2TextColor,
    PasswordScore3TextColor,
    PasswordScore4TextColor
  );
var
  PasswordScore: Integer;
begin
  if (RuleType = rtPassword) and Matches(AField) then
  begin
    Result := PasswordScores.TryGetValue(AField.FieldValue, PasswordScore);
    if Result then
    begin
      Color := PasswordScoreColors[PasswordScore];
      TextColor := PasswordScoreTextColors[PasswordScore];
    end;
  end
  else
    if (RuleType = rtHighlight) and Matches(AField)
      or CheckEvents(AField, 0, 0, True) then
    begin
      Color := Params.IntValue(HighlightColorParam, DefaultHighlightColor);
      TextColor :=
        Params.IntValue(HighlightTextColorParam, DefaultHighlightTextColor);
      Result := True;
    end
    else
      Result := False;
end;

function TO2Rule.GetFormatSettings: TFormatSettings;
begin
  Result := TFormatSettings.Create;
  Result.DateSeparator := Params.StrValue(DateSeparatorParam,
    Result.DateSeparator)[1];
  Result.ShortDateFormat := Params.StrValue(ShortDateFormatParam,
    Result.ShortDateFormat);
end;

procedure TO2Rule.SetName(const Value: string);
var
  ARule: TO2Rule;
begin
  if FName <> Value then
  begin
    ARule := TO2Rules(Collection).FindRule(Value);
    if Assigned(ARule) and (ARule <> Self) then
      raise Exception.CreateFmt(SRuleAlreadyExists, [Value]);
    FName := Value;
    NotifyChanges(Self, onPropertyChanged);
  end;
end;

procedure TO2Rule.SetRuleType(const Value: TO2RuleType);
begin
  if FRuleType <> Value then
  begin
    FRuleType := Value;
    NotifyChanges(Self, onPropertyChanged);
  end;
end;

procedure TO2Rule.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then
  begin
    FFieldName := Value;
    NotifyChanges(Self, onPropertyChanged);
  end;
end;

procedure TO2Rule.SetFieldValue(const Value: string);
begin
  if FFieldValue <> Value then
  begin
    FFieldValue := Value;
    NotifyChanges(Self, onPropertyChanged);
  end;
end;

procedure TO2Rule.SetParams(const Value: TO2Params);
begin
  FParams.Assign(Value);
end;

procedure TO2Rule.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    NotifyChanges(Self, onPropertyChanged);
  end;
end;

{ TO2RulesEnumerator }

function TO2RulesEnumerator.GetCurrent: TO2Rule;
begin
  Result := TO2Rule(inherited GetCurrent);
end;

{ TO2Rules }

constructor TO2Rules.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TO2Rule);
end;

function TO2Rules.GetEnumerator: TO2RulesEnumerator;
begin
  Result := TO2RulesEnumerator.Create(Self);
end;

function TO2Rules.GetRules(Index: Integer): TO2Rule;
begin
  Result := TO2Rule(Items[Index]);
end;

function TO2Rules.FindRule(const Name: string): TO2Rule;
var
  ARule: TO2Rule;
begin
  Result := nil;
  for ARule in Self do
    if SameText(ARule.Name, Name) then
    begin
      Result := ARule;
      Break;
    end;
end;

function TO2Rules.FindFirstRule(const AField: TO2Field;
  RuleTypes: TO2RuleTypes): TO2Rule;
var
  ARule: TO2Rule;
begin
  Result := nil;
  for ARule in Self do
    if (ARule.RuleType in RuleTypes) and ARule.Matches(AField) then
    begin
      Result := ARule;
      Break;
    end;
end;

function TO2Rules.RuleExists(const Name: string): Boolean;
begin
  Result := Assigned(FindRule(Name));
end;

function TO2Rules.AddRule(const Name: string): TO2Rule;
begin
  Result := TO2Rule(Add);
  try
    Result.Name := Name;
  except
    Delete(Result.Index);
    raise;
  end;
end;

procedure TO2Rules.DeleteRule(const Name: string);
var
  ARule: TO2Rule;
begin
  ARule := FindRule(Name);
  if Assigned(ARule) then
    Delete(ARule.Index);
end;

function TO2Rules.ImportRule(const ARule: TO2Rule): TO2Rule;
begin
  Result := FindRule(ARule.Name);
  if Result = nil then Result := AddRule(ARule.Name);
  Result.Assign(ARule);
end;

function TO2Rules.GetDisplayText(const AField: TO2Field;
  ShowPasswords: Boolean): string;
var
  ARule: TO2Rule;
begin
  Result := AField.FieldValue;
  for ARule in Self do
    if ARule.Matches(AField) then
    begin
      Result := ARule.GetDisplayText(AField, ShowPasswords);
      if Result <> AField.FieldValue then Break;
    end;
end;

function TO2Rules.GetHyperLink(const AField: TO2Field): string;
var
  ARule: TO2Rule;
begin
  Result := AField.FieldValue;
  ARule := FindFirstRule(AField, [rtHyperLink]);
  if Assigned(ARule) then
    Result := ARule.GetHyperLink(AField);
end;

function TO2Rules.CheckEvents(const AField: TO2Field; Date1,
  Date2: TDateTime; UseParams: Boolean): Boolean;
var
  ARule: TO2Rule;
begin
  Result := False;
  for ARule in Self do
    if ARule.CheckEvents(AField, Date1, Date2, UseParams) then
    begin
      Result := True;
      Break;
    end;
end;

function TO2Rules.CheckEvents(const AObject: TO2Object; Date1,
  Date2: TDateTime; UseParams: Boolean): Boolean;
var
  AField: TO2Field;
begin
  Result := False;
  for AField in AObject.Fields do
    if CheckEvents(AField, Date1, Date2, UseParams) then
    begin
      Result := True;
      Break;
    end;
end;

function TO2Rules.GetNextEvent(const AField: TO2Field; StartDate: TDateTime;
  out NextDate: TDateTime; UseParams: Boolean): Boolean;
var
  ANextDate: TDateTime;
  ARule: TO2Rule;
begin
  Result := False;
  for ARule in Self do
    if ARule.GetNextEvent(AField, StartDate, ANextDate, UseParams) then
    begin
      if not Result or (ANextDate < NextDate) then
        NextDate := ANextDate;
      Result := True;
    end;
end;

function TO2Rules.GetNextEvent(const AObject: TO2Object; StartDate: TDateTime;
  out NextDate: TDateTime; UseParams: Boolean): Boolean;
var
  ANextDate: TDateTime;
  AField: TO2Field;
begin
  Result := False;
  for AField in AObject.Fields do
    if GetNextEvent(AField, StartDate, ANextDate, UseParams) then
    begin
      if not Result or (ANextDate < NextDate) then
        NextDate := ANextDate;
      Result := True;
    end;
end;

function TO2Rules.GetHighlightColors(const AField: TO2Field;
  const PasswordScores: TDictionary<string, Integer>;
  out Color, TextColor: TColor): Boolean;
var
  ARule: TO2Rule;
begin
  Result := False;
  for ARule in Self do
    if ARule.GetHighlightColors(AField, PasswordScores, Color, TextColor) then
    begin
      Result := True;
      Break;
    end;
end;

function TO2Rules.GetHighlightColors(const AObject: TO2Object;
  const PasswordScores: TDictionary<string, Integer>;
  out Color, TextColor: TColor): Boolean;
var
  AColor, ATextColor: TColor;
  RuleIndex: Integer;
  AField: TO2Field;
  ARule: TO2Rule;
begin
  Result := False;
  RuleIndex := Count;
  for AField in AObject.Fields do
    for ARule in Self do
      if (ARule.Index < RuleIndex)
        and ARule.GetHighlightColors(AField, PasswordScores,
          AColor, ATextColor) then
      begin
        Color := AColor;
        TextColor := ATextColor;
        RuleIndex := ARule.Index;
        Result := True;
      end;
end;

end.
