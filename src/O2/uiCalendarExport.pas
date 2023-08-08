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

{ ******************************************************************** }
{                                                                      }
{ Request for Comments: 5545                                           }
{                                                                      }
{    Internet Calendaring and Scheduling Core Object Specification     }
{                             (iCalendar)                              }
{                                                                      }
{ https://www.rfc-editor.org/rfc/rfc5545.html                          }
{                                                                      }
{ ******************************************************************** }

unit uiCalendarExport;

interface

uses
  Classes, uImportExport, uO2File, uO2Objects;

type
  TiCalendarExport = class(TImportExport)
  private
    FSelection: TO2ObjectList;
  public
    constructor Create(const O2File: TO2File;
      const Selection: TO2ObjectList); overload;
    procedure Execute(const FileName: string); override;
    property Selection: TO2ObjectList read FSelection;
  end;

  TiCalendarClassification = (clPublic, clPrivate, clConfidential);
  TiCalendarTimeTransparency = (ttOpaque, ttTransparent);

  TiCalendarWriter = class
  private
    FStream: TStream;
  public
    constructor Create(Stream: TStream);
    procedure WriteLines(const Value: string);
  end;

implementation

uses
  SysUtils, uO2Rules, DateUtils;

function EscapeText(const Value: string): string;
begin
  Result := StringReplace(Value, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, ';', '\;', [rfReplaceAll]);
  Result := StringReplace(Result, ',', '\,', [rfReplaceAll]);
  Result := StringReplace(Result, #13#10, '\n', [rfReplaceAll]);
end;

{ TiCalendarExport }

constructor TiCalendarExport.Create(const O2File: TO2File;
  const Selection: TO2ObjectList);
begin
  inherited Create(O2File);
  FSelection := Selection;
end;

procedure TiCalendarExport.Execute(const FileName: string);
var
  OutputStream: TFileStream;
  Writer: TiCalendarWriter;
  EventDate: TDateTime;
  AObject: TO2Object;
  AField: TO2Field;
  ARule: TO2Rule;
  UID: TGUID;
begin
  OutputStream := TFileStream.Create(FileName, fmCreate);
  try
    Writer := TiCalendarWriter.Create(OutputStream);
    try
      Writer.WriteLines('BEGIN:VCALENDAR');
      Writer.WriteLines('PRODID:-//O2//iCalendar Export//EN');
      Writer.WriteLines('VERSION:2.0');
      Writer.WriteLines('CALSCALE:GREGORIAN');

      for AObject in Selection do
        for AField in AObject.Fields do
        begin
          ARule := O2File.Rules.FindFirstRule(AField, EventRules);

          if Assigned(ARule) then
          begin
            CreateGUID(UID);
            ARule.GetFirstEvent(AField, EventDate);

            Writer.WriteLines('BEGIN:VEVENT');
            Writer.WriteLines(Format(
              'UID:%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x',
              [UID.D1, UID.D2, UID.D3, UID.D4[0], UID.D4[1], UID.D4[2],
              UID.D4[3], UID.D4[4], UID.D4[5], UID.D4[6], UID.D4[7]]));
            Writer.WriteLines(FormatDateTime('"DTSTAMP:"yyyymmdd"T"hhnnss"Z"',
              TTimeZone.Local.ToUniversalTime(Now)));
            Writer.WriteLines(FormatDateTime('"DTSTART;VALUE=DATE:"yyyymmdd',
              EventDate));
            Writer.WriteLines('SUMMARY:' +
              EscapeText(AObject.Name + ' - ' + AField.FieldName));
            Writer.WriteLines('CLASS:PRIVATE');
            Writer.WriteLines('TRANSP:TRANSPARENT');
            Writer.WriteLines('DESCRIPTION:' +
              EscapeText(AObject.Name + #13#10 + AField.FieldName));

            if ARule.RuleType = rtRecurrence then
              Writer.WriteLines('RRULE:FREQ=YEARLY');

            Writer.WriteLines('END:VEVENT');;
          end;
        end;

      Writer.WriteLines('END:VCALENDAR');
    finally
      Writer.Free;
    end;
  finally
    OutputStream.Free;
  end;
end;

{ TiCalendarWriter }

constructor TiCalendarWriter.Create(Stream: TStream);
begin
  FStream := Stream;
end;

procedure TiCalendarWriter.WriteLines(const Value: string);
const
  NewLine: array [0..1] of Byte = (13, 10);
  LineFolding: Byte = 9;
var
  Bytes: TBytes;
  ByteIndex, MaxWriteCount, WriteCount: Integer;
begin
  Bytes := TEncoding.UTF8.GetBytes(Value);
  ByteIndex := 0;
  MaxWriteCount := 75;

  while ByteIndex < Length(Bytes) do
  begin
    WriteCount := Length(Bytes) - ByteIndex;
    if WriteCount > MaxWriteCount then WriteCount := MaxWriteCount;

    if ByteIndex = 0 then
      Dec(MaxWriteCount)
    else
      FStream.Write(LineFolding, SizeOf(LineFolding));

    FStream.Write(Bytes, ByteIndex, WriteCount);
    FStream.Write(NewLine, SizeOf(NewLine));

    Inc(ByteIndex, WriteCount);
  end;
end;

end.
