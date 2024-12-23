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

unit uO2RulesUtils;

interface

uses
  uO2Objects, uO2Rules;

function TryParseDate(const AField: TO2Field; const ARule: TO2Rule;
  out Value: TDateTime): Boolean;

function GetHyperLinkAddress(const AField: TO2Field;
  const ARule: TO2Rule): string;

implementation

uses
  SysUtils, uUtils;

function TryParseDate(const AField: TO2Field; const ARule: TO2Rule;
  out Value: TDateTime): Boolean;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := TFormatSettings.Create;
  FormatSettings.DateSeparator := ARule.Params.ReadString(
    DateSeparatorParam, FormatSettings.DateSeparator)[1];
  FormatSettings.ShortDateFormat := ARule.Params.ReadString(
    ShortDateFormatParam, FormatSettings.ShortDateFormat);

  Result := TryStrToDate(AField.FieldValue, Value, FormatSettings);
end;

function GetHyperLinkAddress(const AField: TO2Field;
  const ARule: TO2Rule): string;
var
  EncodedFieldName, EncodedFieldValue: string;
  LegacyMacroProcessor, MacroProcessor: TMacroProcessor;
begin
  if ARule.Params.Values[HyperLinkMaskParam] = '' then
    Result := AField.FieldValue
  else
  begin
    EncodedFieldName := UrlEscape(AField.FieldName);
    EncodedFieldValue := UrlEscape(AField.FieldValue);

    LegacyMacroProcessor := TMacroProcessor.Create(
      ARule.Params.Values[HyperLinkMaskParam], LegacyMacroStartDelimiter,
      LegacyMacroEndDelimiter);
    try
      MacroProcessor := TMacroProcessor.Create(LegacyMacroProcessor
        .Macro(LegacyFieldNameMacro, EncodedFieldName)
        .Macro(LegacyFieldValueMacro, EncodedFieldValue)
        .ToString, MacroStartDelimiter, MacroEndDelimiter);
      try
        Result := MacroProcessor
          .Macro(FieldNameMacro, EncodedFieldName)
          .Macro(FieldValueMacro, EncodedFieldValue)
          .ToString;
      finally
        MacroProcessor.Free;
      end;
    finally
      LegacyMacroProcessor.Free;
    end;
  end;
end;

end.
