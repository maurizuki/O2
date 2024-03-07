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

unit uServices;

interface

uses
  Classes, Graphics, Windows, uO2File, uO2Relations, uO2Rules;

type
  IAppFiles = interface
    function FileExists(const Name: string): Boolean;
    function GetPortableFilesTotalSize: Int64;
    procedure InstallPortable(const Path: string);

    function GetFullPath(IndexOrName: Variant): string;
    property FullPath[IndexOrName: Variant]: string read GetFullPath;
  end;

  IStorage = interface
    function ReadBoolean(const Name: string; Default: Boolean = False): Boolean;
    function ReadInteger(const Name: string; Default: Integer = 0): Integer;
    function ReadFloat(const Name: string; Default: Double = 0): Double;
    function ReadString(const Name: string; Default: string = ''): string;

    procedure WriteBoolean(const Name: string; Value: Boolean);
    procedure WriteInteger(const Name: string; Value: Integer);
    procedure WriteFloat(const Name: string; Value: Double);
    procedure WriteString(const Name, Value: string);

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
  end;

  IPasswordScoreCache = interface(IPasswordScoreProvider)
    procedure Update(const O2File: TO2File); overload;
    procedure Update(const O2File: TO2File; ObjectIndex: Integer); overload;
  end;

  IFileProps = interface
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    property Title: string read GetTitle write SetTitle;

    function GetDescription: string;
    procedure SetDescription(const Value: string);
    property Description: string read GetDescription write SetDescription;

    function GetAuthor: string;
    procedure SetAuthor(const Value: string);
    property Author: string read GetAuthor write SetAuthor;

    function GetCipher: string;
    property Cipher: string read GetCipher;

    function GetHash: string;
    property Hash: string read GetHash;

    procedure ApplyChanges;
  end;

  IRelationProps = interface
    function GetObjectName1: string;
    property ObjectName1: string read GetObjectName1;

    function GetObjectName2: string;
    property ObjectName2: string read GetObjectName2;

    function GetRoles: TStrings;
    property Roles: TStrings read GetRoles;

    function GetRole1: string;
    procedure SetRole1(const Value: string);
    property Role1: string read GetRole1 write SetRole1;

    function GetRole2: string;
    procedure SetRole2(const Value: string);
    property Role2: string read GetRole2 write SetRole2;

    function GetRelation: TO2Relation;
    property Relation: TO2Relation read GetRelation;

    procedure ApplyChanges;
  end;

  IRuleProps = interface
    function GetRuleName: string;
    procedure SetRuleName(const Value: string);
    property RuleName: string read GetRuleName write SetRuleName;

    function GetRuleTypes: TStrings;
    property RuleTypes: TStrings read GetRuleTypes;

    function GetRuleTypeIndex: Integer;
    procedure SetRuleTypeIndex(const Value: Integer);
    property RuleTypeIndex: Integer read GetRuleTypeIndex
      write SetRuleTypeIndex;

    function GetFieldNameMask: string;
    procedure SetFieldNameMask(const Value: string);
    property FieldNameMask: string read GetFieldNameMask
      write SetFieldNameMask;

    function GetFieldValueMask: string;
    procedure SetFieldValueMask(const Value: string);
    property FieldValueMask: string read GetFieldValueMask
      write SetFieldValueMask;

    function GetHyperLinkMask: string;
    procedure SetHyperLinkMask(const Value: string);
    property HyperLinkMask: string read GetHyperLinkMask
      write SetHyperLinkMask;

    function GetDisplayPasswordStrength: Boolean;
    procedure SetDisplayPasswordStrength(const Value: Boolean);
    property DisplayPasswordStrength: Boolean read GetDisplayPasswordStrength
      write SetDisplayPasswordStrength;

    function GetDisplayMask: string;
    procedure SetDisplayMask(const Value: string);
    property DisplayMask: string read GetDisplayMask write SetDisplayMask;

    function GetDateFormats: TStrings;
    property DateFormats: TStrings read GetDateFormats;

    function GetDateFormatIndex: Integer;
    procedure SetDateFormatIndex(Value: Integer);
    property DateFormatIndex: Integer read GetDateFormatIndex
      write SetDateFormatIndex;

    function GetDateSeparator: string;
    procedure SetDateSeparator(const Value: string);
    property DateSeparator: string read GetDateSeparator write SetDateSeparator;

    function GetDaysBefore: Integer;
    procedure SetDaysBefore(const Value: Integer);
    property DaysBefore: Integer read GetDaysBefore write SetDaysBefore;

    function GetDaysAfter: Integer;
    procedure SetDaysAfter(const Value: Integer);
    property DaysAfter: Integer read GetDaysAfter write SetDaysAfter;

    function GetHighlightColor: TColor;
    procedure SetHighlightColor(const Value: TColor);
    property HighlightColor: TColor read GetHighlightColor
      write SetHighlightColor;

    function GetHighlightTextColor: TColor;
    procedure SetHighlightTextColor(const Value: TColor);
    property HighlightTextColor: TColor read GetHighlightTextColor
      write SetHighlightTextColor;

    function IsHyperLink: Boolean;
    function IsPassword: Boolean;
    function IsEvent: Boolean;
    function IsHighlight: Boolean;

    function GetRule: TO2Rule;
    property Rule: TO2Rule read GetRule;

    function GetValid: Boolean;
    property Valid: Boolean read GetValid;

    procedure ApplyChanges;
  end;

  IFileOperation = interface
    procedure Execute(const FileName: string);
  end;

  IHTMLExport = interface
    procedure StoreSettings;

    function AddStyle(const Style: string): Integer;

    function ExportToHTML: string; overload;
    procedure ExportToHTML(const FileName: string); overload;

    function GetIncludeIndex: Boolean;
    procedure SetIncludeIndex(Value: Boolean);
    property IncludeIndex: Boolean read GetIncludeIndex write SetIncludeIndex;

    function GetIncludeTags: Boolean;
    procedure SetIncludeTags(Value: Boolean);
    property IncludeTags: Boolean read GetIncludeTags write SetIncludeTags;

    function GetIncludeNotes: Boolean;
    procedure SetIncludeNotes(Value: Boolean);
    property IncludeNotes: Boolean read GetIncludeNotes write SetIncludeNotes;

    function GetIncludeRelations: Boolean;
    procedure SetIncludeRelations(Value: Boolean);
    property IncludeRelations: Boolean read GetIncludeRelations
      write SetIncludeRelations;

    function GetIncludePasswords: Boolean;
    procedure SetIncludePasswords(Value: Boolean);
    property IncludePasswords: Boolean read GetIncludePasswords
      write SetIncludePasswords;

    function GetStyleIndex: Integer;
    procedure SetStyleIndex(Value: Integer);
    property StyleIndex: Integer read GetStyleIndex write SetStyleIndex;
  end;

  IPrint = interface
    procedure StoreSettings;

    function DrawNextPage(const Canvas: TCanvas; PageRect, PrintRect: TRect;
      PageIndex: Integer): Boolean;

    function GetTitle: string;
    property Title: string read GetTitle;

    function GetIncludeTags: Boolean;
    procedure SetIncludeTags(Value: Boolean);
    property IncludeTags: Boolean read GetIncludeTags write SetIncludeTags;

    function GetIncludeNotes: Boolean;
    procedure SetIncludeNotes(Value: Boolean);
    property IncludeNotes: Boolean read GetIncludeNotes write SetIncludeNotes;

    function GetIncludeRelations: Boolean;
    procedure SetIncludeRelations(Value: Boolean);
    property IncludeRelations: Boolean read GetIncludeRelations
      write SetIncludeRelations;

    function GetIncludePasswords: Boolean;
    procedure SetIncludePasswords(Value: Boolean);
    property IncludePasswords: Boolean read GetIncludePasswords
      write SetIncludePasswords;
  end;

  IReplaceOperation = interface
    function GetTitle: string;
    property Title: string read GetTitle;

    function GetSearchValueLabel: string;
    property SearchValueLabel: string read GetSearchValueLabel;

    function GetReplaceValueLabel: string;
    property ReplaceValueLabel: string read GetReplaceValueLabel;

    function GetSearchList: TStrings;
    property SearchList: TStrings read GetSearchList;

    function GetReplaceList: TStrings;
    property ReplaceList: TStrings read GetReplaceList;

    function GetSearchValue: string;
    procedure SetSearchValue(const Value: string);
    property SearchValue: string read GetSearchValue write SetSearchValue;

    function GetReplaceValue: string;
    procedure SetReplaceValue(const Value: string);
    property ReplaceValue: string read GetReplaceValue write SetReplaceValue;

    function GetValid: Boolean;
    property Valid: Boolean read GetValid;

    procedure Replace;
  end;

implementation

end.
