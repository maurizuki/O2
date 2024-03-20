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

unit uFileManager;

interface

uses
  Classes, uServices, uEventFilters, uO2File, uO2Objects, uO2Rules;

type
  TFileManager = class(TInterfacedObject, IFileManager)
  private
    FFile: TO2File;
    FObjectName: string;
    FEventFilters: TStrings;
    FEventFilterIndex: Integer;
    FEventFilter: TEventFilter;

    FPasswordScoreCache: IPasswordScoreCache;

    function GetFile: TO2File;
    function GetObjectName: string;
    function GetEventFilters: TStrings;
    function GetEventFilterIndex: Integer;
    procedure SetFile(const Value: TO2File);
    procedure SetObjectName(const Value: string);
    procedure SetEventFilterIndex(const Value: Integer);
  public
    constructor Create(PasswordScoreCache: IPasswordScoreCache);
    destructor Destroy; override;

    function GetObjects: IEnumerable<TO2Object>;

    function GetNextEvent(const AObject: TO2Object;
      out NextDate: TDateTime): Boolean;
    function GetHighlight(const AObject: TO2Object): THighlight; overload;
    function GetHighlight(const AField: TO2Field): THighlight; overload;
    function IsHyperlink(const AField: TO2Field): Boolean;

    property O2File: TO2File read GetFile write SetFile;

    property ObjectName: string read GetObjectName write SetObjectName;
    property EventFilters: TStrings read GetEventFilters;
    property EventFilterIndex: Integer read GetEventFilterIndex
      write SetEventFilterIndex;
    property EventFilter: TEventFilter read FEventFilter;
  end;

implementation

uses
  SysUtils, StrUtils, uGlobal;

type
  TO2ObjectFilteredEnumerator = class(TInterfacedObject,
    IEnumerator<TO2Object>)
  private
    FFileManager: TFileManager;
    FIndex: Integer;

    function CheckName: Boolean;
    function CheckTags: Boolean;
    function CheckEvents: Boolean;
    function CheckRules: Boolean;
  public
    constructor Create(FileManager: TFileManager);
    function GetCurrent: TObject;
    function GetCurrentT: TO2Object;
    function IEnumerator<TO2Object>.GetCurrent = GetCurrentT;
    function MoveNext: Boolean;
    procedure Reset;
  end;

  TO2ObjectFilteredEnumerable = class(TInterfacedObject,
    IEnumerable<TO2Object>)
  private
    FFileManager: TFileManager;
  public
    constructor Create(FileManager: TFileManager);
    function GetEnumerator: IEnumerator;
    function GetEnumeratorT: IEnumerator<TO2Object>;
    function IEnumerable<TO2Object>.GetEnumerator = GetEnumeratorT;
  end;

  TEventFilterRange = 0..14;

const
  EventFilterClasses: array[TEventFilterRange] of TEventFilterClass = (
    TEventFilter,
    TEventFilterAllEvents,
    TEventFilterCustom,
    TEventFilterToday,
    TEventFilterTomorrow,
    TEventFilterThisWeek,
    TEventFilterThisMonth,
    TEventFilterThisYear,
    TEventFilterNext7days,
    TEventFilterNext15days,
    TEventFilterNext30days,
    TEventFilterNext60days,
    TEventFilterNext90days,
    TEventFilterNext180days,
    TEventFilterNext365days
    );

  EventFilterDescriptions: array[TEventFilterRange] of string = (
    SEventAll,
    SEventAllEvents,
    SEventCustom,
    SEventToday,
    SEventTomorrow,
    SEventThisWeek,
    SEventThisMonth,
    SEventThisYear,
    SEventNext7days,
    SEventNext15days,
    SEventNext30days,
    SEventNext60days,
    SEventNext90days,
    SEventNext180days,
    SEventNext365days
    );

{ TFileManager }

constructor TFileManager.Create(PasswordScoreCache: IPasswordScoreCache);
begin
  FPasswordScoreCache := PasswordScoreCache;
  FEventFilters := TStringList.Create;
  FEventFilters.AddStrings(EventFilterDescriptions);
  FEventFilter := EventFilterClasses[FEventFilterIndex].Create;
end;

destructor TFileManager.Destroy;
begin
  if Assigned(FFile) then FFile.Free;
  FEventFilters.Free;
  FEventFilter.Free;
  inherited;
end;

function TFileManager.GetEventFilterIndex: Integer;
begin
  Result := FEventFilterIndex;
end;

function TFileManager.GetEventFilters: TStrings;
begin
  Result := FEventFilters;
end;

function TFileManager.GetFile: TO2File;
begin
  if FFile = nil then FFile := TO2File.Create;
  Result := FFile;
end;

function TFileManager.GetHighlight(const AField: TO2Field): THighlight;
begin
  Result := O2File.Rules.GetHighlightColors(AField, FPasswordScoreCache);
end;

function TFileManager.GetNextEvent(const AObject: TO2Object;
  out NextDate: TDateTime): Boolean;
begin
  Result := O2File.Rules.GetNextEvent(
    AObject, FEventFilter.Date1, NextDate, FEventFilter.UseParamsForNextEvent);
end;

function TFileManager.GetObjectName: string;
begin
  Result := FObjectName;
end;

function TFileManager.GetObjects: IEnumerable<TO2Object>;
begin
  Result := TO2ObjectFilteredEnumerable.Create(Self);
end;

function TFileManager.GetHighlight(const AObject: TO2Object): THighlight;
begin
  Result := O2File.Rules.GetHighlightColors(AObject, FPasswordScoreCache);
end;

function TFileManager.IsHyperlink(const AField: TO2Field): Boolean;
begin
  Result := Assigned(O2File.Rules.FindFirstRule(AField, HyperlinkRules));
end;

procedure TFileManager.SetEventFilterIndex(const Value: Integer);
begin
  if FEventFilterIndex <> Value then
  begin
    FEventFilterIndex := Value;

    FEventFilter.Free;
    FEventFilter := EventFilterClasses[FEventFilterIndex].Create;
  end;
end;

procedure TFileManager.SetFile(const Value: TO2File);
begin
  FFile := Value;
end;

procedure TFileManager.SetObjectName(const Value: string);
begin
  FObjectName := Value;
end;

{ TO2ObjectFilteredEnumerator }

constructor TO2ObjectFilteredEnumerator.Create(FileManager: TFileManager);
begin
  FFileManager := FileManager;
  FIndex := -1;
end;

function TO2ObjectFilteredEnumerator.GetCurrent: TObject;
begin
  Result := GetCurrentT;
end;

function TO2ObjectFilteredEnumerator.GetCurrentT: TO2Object;
begin
  Result := FFileManager.O2File.Objects[FIndex];
end;

function TO2ObjectFilteredEnumerator.CheckName: Boolean;
begin
  Result := (FFileManager.ObjectName = '')
    or ContainsText(FFileManager.O2File.Objects[FIndex].Name,
      FFileManager.ObjectName);
end;

function TO2ObjectFilteredEnumerator.CheckTags: Boolean;
begin
  Result := True;
end;

function TO2ObjectFilteredEnumerator.CheckEvents: Boolean;
begin
  Result := FFileManager.EventFilter.All
    or FFileManager.O2File.Rules.CheckEvents(
      FFileManager.O2File.Objects[FIndex],
      FFileManager.EventFilter.Date1, FFileManager.EventFilter.Date2,
      FFileManager.EventFilter.UseParams);
end;

function TO2ObjectFilteredEnumerator.CheckRules: Boolean;
begin
  Result := True;
end;

function TO2ObjectFilteredEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);

  while FIndex < FFileManager.O2File.Objects.Count do
  begin
    if CheckName and CheckTags and CheckEvents and CheckRules then
      Exit(True);

    Inc(FIndex);
  end;

  Result := False;
end;

procedure TO2ObjectFilteredEnumerator.Reset;
begin
  FIndex := -1;
end;

{ TO2ObjectFilteredEnumerable }

constructor TO2ObjectFilteredEnumerable.Create(FileManager: TFileManager);
begin
  FFileManager := FileManager;
end;

function TO2ObjectFilteredEnumerable.GetEnumerator: IEnumerator;
begin
  Result := GetEnumeratorT;
end;

function TO2ObjectFilteredEnumerable.GetEnumeratorT: IEnumerator<TO2Object>;
begin
  Result := TO2ObjectFilteredEnumerator.Create(FFileManager);
end;

end.
