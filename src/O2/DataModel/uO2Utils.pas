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

unit uO2Utils;

interface

uses
  Classes;

type
  TMacroProcessor = class
  private
    FMask: string;
    FStartDelimiter: Char;
    FEndDelimiter: Char;
    FMacros: TStrings;
  public
    constructor Create(const Mask: string; StartDelimiter, EndDelimiter: Char);
    destructor Destroy; override;
    function Macro(const Name, Value: string): TMacroProcessor; overload;
    function Macro(const Name: string; Value: Integer): TMacroProcessor;
      overload;
    function ToString: string; override;
  end;

procedure DateSpan(ANow, AThen: TDateTime; var Years, Months, Days: Word);
function SafeRecodeYear(const AValue: TDateTime; const AYear: Word): TDateTime;

function UrlEscape(const S: string): string;

implementation

uses
  SysUtils, StrUtils, DateUtils;

procedure DateSpan(ANow, AThen: TDateTime; var Years, Months, Days: Word);
var
  X: Double;
begin
  X := DaySpan(ANow, AThen);
  Years := Trunc(X / ApproxDaysPerYear);
  X := X - Years * ApproxDaysPerYear;
  Months := Trunc(X / ApproxDaysPerMonth);
  X := X - Months * ApproxDaysPerMonth;
  Days := Trunc(X);
end;

function SafeRecodeYear(const AValue: TDateTime; const AYear: Word): TDateTime;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  DecodeDateTime(AValue, Year, Month, Day, Hour, Min, Sec, MSec);
  if (Month = 2) and (Day = 29) and not IsLeapYear(AYear) then Day := 28;
  Result := EncodeDateTime(AYear, Month, Day, Hour, Min, Sec, MSec);
end;

function UrlEscape(const S: string): string;
const
  SpecialChars: TSysCharSet = ['!', '"', '#', '$', '%', '&',
    '''', '(', ')', '+', ',', '/', ':', ';', '<', '=', '>',
    '?', '[', '\', ']', '^', '`', '{', '\', '}', '~'];
var
  C: AnsiChar;
begin
  Result := '';
  for C in UTF8Encode(S) do
    if (C <= #$20) or (C > #$7F) or CharInSet(C, SpecialChars) then
      Result := Result + '%' + IntToHex(Ord(C), 2)
    else
      Result := Result + WideChar(C);
end;

{ TMacroProcessor }

constructor TMacroProcessor.Create(const Mask: string; StartDelimiter,
  EndDelimiter: Char);
begin
  inherited Create;
  FMask := Mask;
  FStartDelimiter := StartDelimiter;
  FEndDelimiter := EndDelimiter;
  FMacros := TStringList.Create;
end;

destructor TMacroProcessor.Destroy;
begin
  FMacros.Free;
  inherited Destroy;
end;

function TMacroProcessor.Macro(const Name, Value: string): TMacroProcessor;
begin
  FMacros.Values[Name] := Value;
  Result := Self;
end;

function TMacroProcessor.Macro(const Name: string;
  Value: Integer): TMacroProcessor;
begin
  FMacros.Values[Name] := IntToStr(Value);
  Result := Self;
end;

function TMacroProcessor.ToString: string;
var
  I, P: Integer;
begin
  I := 1;
  Result := '';
  while I <= Length(FMask) do
  begin
    if FMask[I] = FStartDelimiter then
      P := PosEx(FEndDelimiter, FMask, I + 1)
    else
      P := 0;

    if P > 0 then
    begin
      Result := Result + FMacros.Values[Copy(FMask, I + 1, P - I - 1)];
      I := P + 1;
    end
    else
    begin
      Result := Result + FMask[I];
      Inc(I);
    end;
  end;
end;

end.
