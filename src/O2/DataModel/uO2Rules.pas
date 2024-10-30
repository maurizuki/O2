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

unit uO2Rules;

interface

uses
  Classes, SysUtils, Graphics, Masks, uO2Classes, uO2Objects;

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
  DisplayPasswordStrengthParam = 'DisplayPasswordStrength';

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
  DefaultDisplayPasswordStrength = False;

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
    function AddParam(const ParamName: string): TO2Param;
    procedure DeleteParam(const ParamName: string);

    function ReadBoolean(const ParamName: string;
      DefaultValue: Boolean = False): Boolean;
    function ReadInteger(const ParamName: string;
      DefaultValue: Integer = 0): Integer;
    function ReadString(const ParamName: string;
      const DefaultValue: string = ''): string;

    property Params[Index: Integer]: TO2Param read GetParams; default;
    property Values[Name: string]: string read GetValues write SetValues;
  end;

  TO2Rule = class(TO2CollectionItem)
  private
    FName: string;
    FRuleType: TO2RuleType;
    FFieldName: string;
    FFieldNameMask: TMask;
    FFieldValue: string;
    FFieldValueMask: TMask;
    FParams: TO2Params;
    FActive: Boolean;

    function GetFieldNameMask: TMask;
    function GetFieldValueMask: TMask;
    procedure SetName(const Value: string);
    procedure SetRuleType(const Value: TO2RuleType);
    procedure SetFieldName(const Value: string);
    procedure SetFieldValue(const Value: string);
    procedure SetParams(const Value: TO2Params);
    procedure SetActive(const Value: Boolean);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function Matches(const AFieldName, AFieldValue: string): Boolean; overload;
    function Matches(const AField: TO2Field): Boolean; overload; inline;
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
  public
    constructor Create(AOwner: TPersistent);

    function GetEnumerator: TO2RulesEnumerator;

    function FindRule(const Name: string): TO2Rule;
    function FindFirstRule(const AField: TO2Field;
      RuleTypes: TO2RuleTypes): TO2Rule;
    function AddRule(const Name: string): TO2Rule;
  end;

implementation

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
    if SameText(AParam.ParamName, ParamName) then Exit(AParam);
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

function TO2Params.ReadBoolean(const ParamName: string;
  DefaultValue: Boolean): Boolean;
var
  AParam: TO2Param;
begin
  AParam := FindParam(ParamName);
  if Assigned(AParam) then
    Result := StrToBoolDef(AParam.ParamValue, DefaultValue)
  else
    Result := DefaultValue;
end;

function TO2Params.ReadInteger(const ParamName: string;
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

function TO2Params.ReadString(const ParamName, DefaultValue: string): string;
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
  FFieldNameMask := nil;
  FFieldValue := '';
  FFieldValueMask := nil;
  FParams := TO2Params.Create(Self);
  FActive := False;
end;

destructor TO2Rule.Destroy;
begin
  if Assigned(FFieldNameMask) then FFieldNameMask.Free;
  if Assigned(FFieldValueMask) then FFieldValueMask.Free;
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

function TO2Rule.Matches(const AFieldName, AFieldValue: string): Boolean;
begin
  try
    Result := GetFieldNameMask.Matches(AFieldName)
      and GetFieldValueMask.Matches(AFieldValue);
  except
    Result := False;
  end;
end;

function TO2Rule.Matches(const AField: TO2Field): Boolean;
begin
  Result := Matches(AField.FieldName, AField.FieldValue);
end;

function TO2Rule.GetFieldNameMask: TMask;
begin
  if FFieldNameMask = nil then
    FFieldNameMask := TMask.Create(FFieldName);
  Result := FFieldNameMask;
end;

function TO2Rule.GetFieldValueMask: TMask;
begin
  if FFieldValueMask = nil then
    FFieldValueMask := TMask.Create(FFieldValue);
  Result := FFieldValueMask;
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
    if Assigned(FFieldNameMask) then FreeAndNil(FFieldNameMask);
    NotifyChanges(Self, onPropertyChanged);
  end;
end;

procedure TO2Rule.SetFieldValue(const Value: string);
begin
  if FFieldValue <> Value then
  begin
    FFieldValue := Value;
    if Assigned(FFieldValueMask) then FreeAndNil(FFieldValueMask);
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

function TO2Rules.FindRule(const Name: string): TO2Rule;
var
  ARule: TO2Rule;
begin
  Result := nil;
  for ARule in Self do
    if SameText(ARule.Name, Name) then Exit(ARule);
end;

function TO2Rules.FindFirstRule(const AField: TO2Field;
  RuleTypes: TO2RuleTypes): TO2Rule;
var
  ARule: TO2Rule;
begin
  Result := nil;
  for ARule in Self do
    if ARule.Active and (ARule.RuleType in RuleTypes)
      and ARule.Matches(AField) then Exit(ARule);
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

end.
