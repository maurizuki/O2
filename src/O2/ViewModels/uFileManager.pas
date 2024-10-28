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
  Classes, Generics.Collections, uServices, uEventFilters, uO2File, uO2Objects, 
  uO2Rules;

type
  TFileManager = class(TInterfacedObject, IFileManager)
  private
    FDateProvider: IDateProvider;
    FPasswordProvider: IPasswordProvider;
    FPasswordScoreCache: IPasswordScoreCache;

    FFile: TO2File;
    FObjectName: string;
    FEventFilters: TStrings;
    FEventFilterIndex: Integer;
    FEventFilter: TEventFilter;
    FTags: TStrings;
    FObjectTags: TStrings;
    FIncludeUntagged: Boolean;
    FObjectRules: TList<TO2Rule>;

    function GetFile: TO2File;
    function GetObjectName: string;
    function GetEventFilters: TStrings;
    function GetEventFilterIndex: Integer;
    function GetTags: TStrings;
    function GetObjectTags: TStrings;
    function GetIncludeUntagged: Boolean;
    function GetObjectRules: TList<TO2Rule>;
    procedure SetObjectName(const Value: string);
    procedure SetEventFilterIndex(const Value: Integer);
    procedure SetObjectTags(const Value: TStrings);
    procedure SetIncludeUntagged(const Value: Boolean);
  public
    constructor Create(DateProvider: IDateProvider;
      PasswordProvider: IPasswordProvider;
      PasswordScoreCache: IPasswordScoreCache);
    destructor Destroy; override;

    procedure NewFile;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string; KeepModified: Boolean);

    function GetObjects: IEnumerable<TO2Object>;

    function GetNextEvent(const AObject: TO2Object;
      out NextDate: TDateTime): Boolean;
    function GetHighlight(const AObject: TO2Object): THighlight; overload;
    function GetHighlight(const AField: TO2Field): THighlight; overload;
    function GetDisplayText(const AField: TO2Field;
      ShowPasswords: Boolean): string;
    function GetHyperLink(const AField: TO2Field): string;
    function IsHyperlinkOrEmail(const AField: TO2Field): Boolean;
    function IsHyperlink(const AField: TO2Field): Boolean;
    function IsEmail(const AField: TO2Field): Boolean;

    property O2File: TO2File read GetFile;

    property ObjectName: string read GetObjectName write SetObjectName;
    property EventFilters: TStrings read GetEventFilters;
    property EventFilterIndex: Integer read GetEventFilterIndex
      write SetEventFilterIndex;
    property EventFilter: TEventFilter read FEventFilter;
    property Tags: TStrings read GetTags;
    property ObjectTags: TStrings read GetObjectTags write SetObjectTags;
    property IncludeUntagged: Boolean read GetIncludeUntagged
      write SetIncludeUntagged;
    property ObjectRules: TList<TO2Rule> read GetObjectRules;
  end;

implementation

uses
  SysUtils, StrUtils, uGlobal, uO2ObjectsUtils;

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

constructor TFileManager.Create(DateProvider: IDateProvider;
  PasswordProvider: IPasswordProvider;
  PasswordScoreCache: IPasswordScoreCache);
begin
  FDateProvider := DateProvider;
  FPasswordProvider := PasswordProvider;
  FPasswordScoreCache := PasswordScoreCache;

  FEventFilters := TStringList.Create;
  FEventFilters.AddStrings(EventFilterDescriptions);

  FEventFilter := EventFilterClasses[FEventFilterIndex].Create(FDateProvider);

  FTags := TStringList.Create;
  FObjectTags := TStringList.Create;
  FObjectRules := TList<TO2Rule>.Create;
end;

destructor TFileManager.Destroy;
begin
  if Assigned(FFile) then FFile.Free;
  FEventFilters.Free;
  FEventFilter.Free;
  FTags.Free;
  FObjectTags.Free;
  FObjectRules.Free;
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

function TFileManager.GetHighlight(const AObject: TO2Object): THighlight;
var
  AHighlight: THighlight;
  RuleIndex: Integer;
  AField: TO2Field;
  ARule: TO2Rule;
begin
  Result.Highlight := htNone;
  RuleIndex := O2File.Rules.Count;
  for AField in AObject.Fields do
    for ARule in O2File.Rules do
      if ARule.Index < RuleIndex then
      begin
        AHighlight := ARule.GetHighlightColors(AField, FDateProvider,
          FPasswordScoreCache);
        if AHighlight.Highlight <> htNone then
        begin
          Result := AHighlight;
          RuleIndex := ARule.Index;
        end;
      end;
end;

function TFileManager.GetHighlight(const AField: TO2Field): THighlight;
var
  ARule: TO2Rule;
begin
  Result.Highlight := htNone;
  for ARule in O2File.Rules do
  begin
    Result := ARule.GetHighlightColors(AField, FDateProvider,
      FPasswordScoreCache);
    if Result.Highlight <> htNone then Break;
  end;
end;

function TFileManager.GetDisplayText(const AField: TO2Field;
  ShowPasswords: Boolean): string;
var
  ARule: TO2Rule;
begin
  Result := AField.FieldValue;
  for ARule in O2File.Rules do
    if ARule.Active and ARule.Matches(AField) then
    begin
      Result := ARule.GetDisplayText(AField, FDateProvider, ShowPasswords);
      if Result <> AField.FieldValue then Break;
    end;
end;

function TFileManager.GetHyperLink(const AField: TO2Field): string;
var
  ARule: TO2Rule;
begin
  Result := AField.FieldValue;
  ARule := O2File.Rules.FindFirstRule(AField, [rtHyperLink]);
  if Assigned(ARule) then Result := ARule.GetHyperLink(AField);
end;

function TFileManager.GetIncludeUntagged: Boolean;
begin
  Result := FIncludeUntagged;
end;

function TFileManager.GetNextEvent(const AObject: TO2Object;
  out NextDate: TDateTime): Boolean;
var
  ANextDate: TDateTime;
  AField: TO2Field;
  ARule: TO2Rule;
begin
  Result := False;
  for AField in AObject.Fields do
    for ARule in O2File.Rules do
      if ARule.GetNextEvent(AField, FDateProvider, FEventFilter.StartDate,
        ANextDate, FEventFilter.UseParamsForNextEvent) then
      begin
        if not Result or (ANextDate < NextDate) then NextDate := ANextDate;
        Result := True;
      end;
end;

function TFileManager.GetObjectName: string;
begin
  Result := FObjectName;
end;

function TFileManager.GetObjectRules: TList<TO2Rule>;
begin
  Result := FObjectRules;
end;

function TFileManager.GetObjects: IEnumerable<TO2Object>;
begin
  Result := TO2ObjectFilteredEnumerable.Create(Self);
end;

function TFileManager.GetObjectTags: TStrings;
begin
  Result := FObjectTags;
end;

function TFileManager.GetTags: TStrings;
begin
  FTags.Text := STagsNone;
  AppendTagsToList(O2File.Objects.ToEnumerable, FTags);

  Result := FTags;
end;

function TFileManager.IsEmail(const AField: TO2Field): Boolean;
begin
  Result := Assigned(O2File.Rules.FindFirstRule(AField, [rtEmail]));
end;

function TFileManager.IsHyperlink(const AField: TO2Field): Boolean;
begin
  Result := Assigned(O2File.Rules.FindFirstRule(AField, [rtHyperLink]));
end;

function TFileManager.IsHyperlinkOrEmail(const AField: TO2Field): Boolean;
begin
  Result := Assigned(O2File.Rules.FindFirstRule(AField, HyperlinkRules));
end;

procedure TFileManager.LoadFromFile(const FileName: string);
var
  NewFile: TO2File;
begin
  NewFile := TO2File.Create;
  try
    NewFile.FileName := FileName;
    NewFile.Load(FPasswordProvider);
    FPasswordScoreCache.UpdateCache(NewFile);
    O2File.Free;
    FFile := NewFile;
  except
    NewFile.Free;
    raise;
  end;
end;

procedure TFileManager.NewFile;
begin
  FreeAndNil(FFile);
end;

procedure TFileManager.SaveToFile(const FileName: string;
  KeepModified: Boolean);
begin
  O2File.FileName := FileName;
  FFile.Save;
  if not KeepModified then FFile.Modified := False;
end;

procedure TFileManager.SetEventFilterIndex(const Value: Integer);
begin
  if FEventFilterIndex <> Value then
  begin
    FEventFilterIndex := Value;

    FEventFilter.Free;
    FEventFilter := EventFilterClasses[FEventFilterIndex].Create(FDateProvider);
  end;
end;

procedure TFileManager.SetIncludeUntagged(const Value: Boolean);
begin
  FIncludeUntagged := Value;
end;

procedure TFileManager.SetObjectName(const Value: string);
begin
  FObjectName := Value;
end;

procedure TFileManager.SetObjectTags(const Value: TStrings);
begin
  FObjectTags.Assign(Value);
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
var
  Tags: TStringList;
  ATag: string;
begin
  if not FFileManager.IncludeUntagged and (FFileManager.ObjectTags.Count = 0)
    or FFileManager.IncludeUntagged
    and (FFileManager.O2File.Objects[FIndex].Tag = '') then Exit(True);

  Tags := TStringList.Create;
  try
    FFileManager.O2File.Objects[FIndex].GetTags(Tags);
    
    for ATag in FFileManager.ObjectTags do
      if Tags.IndexOf(Atag) <> -1 then Exit(True);
  finally
    Tags.Free;
  end;
  
  Result := False;
end;

function TO2ObjectFilteredEnumerator.CheckEvents: Boolean;
var
  AField: TO2Field;
  ARule: TO2Rule;
begin
  if FFileManager.EventFilter.All then Exit(True);

  for AField in FFileManager.O2File.Objects[FIndex].Fields do
    for ARule in FFileManager.O2File.Rules do
      if ARule.HasEventInWindow(AField, FFileManager.FDateProvider,
        FFileManager.EventFilter.StartDate,
        FFileManager.EventFilter.EndDate,
        FFileManager.EventFilter.UseParams) then
        Exit(True);

  Result := False;
end;

function TO2ObjectFilteredEnumerator.CheckRules: Boolean;
var
  AField: TO2Field;
  ARule: TO2Rule;
begin
  if FFileManager.ObjectRules.Count = 0 then Exit(True);

  for ARule in FFileManager.ObjectRules do
    for AField in FFileManager.O2File.Objects[FIndex].Fields do
      if ARule.Active and not (ARule.RuleType in EventRules)
        and ARule.Matches(AField)
        or ARule.HasEventInWindow(AField, FFileManager.FDateProvider) then
        Exit(True);

  Result := False;
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
