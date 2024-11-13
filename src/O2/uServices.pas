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
  Classes, Generics.Collections, Graphics, Windows, uO2File, uO2Objects,
  uO2Relations, uO2Rules;

type
  IDateProvider = interface
    function GetDate: TDateTime;
  end;

  IAppFiles = interface
    function FileExists(const Name: string): Boolean;
    function GetTotalSize: Int64;
    procedure InstallPortable(const Path: string);

    function GetFullPaths(IndexOrName: Variant): string;
    property FullPaths[IndexOrName: Variant]: string read GetFullPaths;
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

  IPasswordScoreProvider = interface
    function TryGetPasswordScore(const Password: string;
      var Score: Integer): Boolean;
  end;

  IPasswordScoreCache = interface(IPasswordScoreProvider)
    procedure UpdateCache(const O2File: TO2File);
  end;

  IFileManager = interface
    function GetFile: TO2File;
    property O2File: TO2File read GetFile;

    procedure NewFile;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string; KeepModified: Boolean);

    {$REGION 'Object enumeration'}

    function GetObjectName: string;
    procedure SetObjectName(const Value: string);
    property ObjectName: string read GetObjectName write SetObjectName;

    function GetEventFilters: TStrings;
    property EventFilters: TStrings read GetEventFilters;

    function GetEventFilterIndex: Integer;
    procedure SetEventFilterIndex(const Value: Integer);
    property EventFilterIndex: Integer read GetEventFilterIndex
      write SetEventFilterIndex;

    function GetTags: TStrings;
    property Tags: TStrings read GetTags;

    function GetObjectTags: TStrings;
    procedure SetObjectTags(const Value: TStrings);
    property ObjectTags: TStrings read GetObjectTags write SetObjectTags;

    function GetIncludeUntagged: Boolean;
    procedure SetIncludeUntagged(const Value: Boolean);
    property IncludeUntagged: Boolean read GetIncludeUntagged
      write SetIncludeUntagged;

    function GetObjectRules: TList<TO2Rule>;
    property ObjectRules: TList<TO2Rule> read GetObjectRules;

    function GetObjects: IEnumerable<TO2Object>;

    {$ENDREGION}

    function TryGetNextEvent(const AObject: TO2Object;
      out NextDate: TDateTime): Boolean;
    function TryGetHighlightColors(const AObject: TO2Object; out Color,
      TextColor: TColor): Boolean; overload;
    function TryGetHighlightColors(const AField: TO2Field; out Color,
      TextColor: TColor): Boolean; overload;
    function GetDisplayText(const AField: TO2Field;
      ShowPasswords: Boolean): string;
    function GetHyperLink(const AField: TO2Field): string;
    function IsHyperlinkOrEmail(const AField: TO2Field): Boolean;
    function IsHyperlink(const AField: TO2Field): Boolean;
    function IsEmail(const AField: TO2Field): Boolean;
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

  IPasswordStrengthInfo = interface
    function GetPasswordScore: Integer;
    property PasswordScore: Integer read GetPasswordScore;

    function GetPasswordStrengthInfo: string;
    property PasswordStrengthInfo: string read GetPasswordStrengthInfo;
  end;

  IEncryptionProps = interface(IPasswordStrengthInfo)
    function GetCiphers: TStrings;
    property Ciphers: TStrings read GetCiphers;

    function GetCipherIndex: Integer;
    procedure SetCipherIndex(const Value: Integer);
    property CipherIndex: Integer read GetCipherIndex write SetCipherIndex;

    function GetHashes: TStrings;
    property Hashes: TStrings read GetHashes;

    function GetHashIndex: Integer;
    procedure SetHashIndex(const Value: Integer);
    property HashIndex: Integer read GetHashIndex write SetHashIndex;

    function GetPassword: string;
    procedure SetPassword(const Value: string);
    property Password: string read GetPassword write SetPassword;

    function GetPasswordConfirmation: string;
    procedure SetPasswordConfirmation(const Value: string);
    property PasswordConfirmation: string read GetPasswordConfirmation
      write SetPasswordConfirmation;

    function IsEncrypted: Boolean;

    function GetValid: Boolean;
    property Valid: Boolean read GetValid;

    procedure ApplyChanges;
  end;

  IObjectProps = interface(IPasswordStrengthInfo)
    function GetObjectName: string;
    procedure SetObjectName(const Value: string);
    property ObjectName: string read GetObjectName write SetObjectName;

    {$REGION 'Object tags'}

    function GetTags: TStrings;
    property Tags: TStrings read GetTags;

    function GetObjectTags: TStrings;
    procedure SetObjectTags(const Value: TStrings);
    property ObjectTags: TStrings read GetObjectTags write SetObjectTags;

    {$ENDREGION}

    {$REGION 'Object fields'}

    function GetFieldNames: TStrings;
    property FieldNames: TStrings read GetFieldNames;

    function GetFieldValues: TStrings;
    property FieldValues: TStrings read GetFieldValues;

    function GetObjectFieldNames(Index: Integer): string;
    property ObjectFieldNames[Index: Integer]: string
      read GetObjectFieldNames;

    function GetObjectFieldValues(Index: Integer): string;
    property ObjectFieldValues[Index: Integer]: string
      read GetObjectFieldValues;

    function GetFieldCount: Integer;
    property FieldCount: Integer read GetFieldCount;

    function GetFieldIndex: Integer;
    procedure SetFieldIndex(const Value: Integer);
    property FieldIndex: Integer read GetFieldIndex write SetFieldIndex;

    function GetFieldName: string;
    procedure SetFieldName(const Value: string);
    property FieldName: string read GetFieldName write SetFieldName;

    function GetFieldValue: string;
    procedure SetFieldValue(const Value: string);
    property FieldValue: string read GetFieldValue write SetFieldValue;

    function CanAddField: Boolean;
    procedure AddField;

    function CanReplaceField: Boolean;
    procedure ReplaceField;

    function CanDeleteField: Boolean;
    procedure DeleteField;

    procedure SwapFields(OtherIndex: Integer);

    {$ENDREGION}

    {$REGION 'Object notes'}

    function GetObjectNotes: TStrings;
    procedure SetObjectNotes(const Value: TStrings);
    property ObjectNotes: TStrings read GetObjectNotes write SetObjectNotes;

    function GetMarkdown: Boolean;
    procedure SetMarkdown(const Value: Boolean);
    property Markdown: Boolean read GetMarkdown write SetMarkdown;

    {$ENDREGION}

    function GetO2Object: TO2Object;
    property O2Object: TO2Object read GetO2Object;

    function GetValid: Boolean;
    property Valid: Boolean read GetValid;

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

    function ExportToHTML(Preview: Boolean): string; overload;
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
