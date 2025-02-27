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

unit uXmlStorage;

interface

uses
  SysUtils, XMLDoc, XMLIntf, xmldom, msxmldom, uServices;

type
  TXmlStorage = class(TInterfacedObject, IStorage)
  private
    FXML: IXMLDocument;
    FMigrationHandler: TFunc<IStorage, IXMLDocument, IXMLDocument>;

    const SchemaLocation = 'https://maurizuki.github.io/O2/xml/settings.xsd';
    const DocumentElementIdent = 'O2';
    const SettingsIdent = 'Settings';
    const SettingIdent = 'setting';
    const SettingNameIdent = 'name';
    const SettingTypeIdent = 'type';
    const BooleanTypeIdent = 'boolean';
    const IntegerTypeIdent = 'integer';
    const FloatTypeIdent = 'float';
    const StringTypeIdent = 'string';

    function GetXML: IXMLDocument;
    function GetSettingsNode(CanCreate: Boolean): IXMLNode;
    function GetSettingNode(const Name: string; CanCreate: Boolean): IXMLNode;
    function NodeIsSetting(Node: IXMLNode; const Name: string): Boolean;
    function ReadValue(const Name: string; Default: Variant): Variant;
    procedure WriteValue(const Name, ValueType: string; Value: Variant);
  protected
    property XML: IXMLDocument read GetXML write FXML;
  public
    constructor Create(const MigrationHandler:
      TFunc<IStorage, IXMLDocument, IXMLDocument> = nil);

    function Exists(const Name: string): Boolean;
    procedure Delete(const Name: string);

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

implementation

uses
  Variants;

{ TXmlStorage }

constructor TXmlStorage.Create(const MigrationHandler:
  TFunc<IStorage, IXMLDocument, IXMLDocument>);
begin
  inherited Create;
  FMigrationHandler := MigrationHandler;
  FXML := nil;
end;

function TXmlStorage.GetXML: IXMLDocument;
begin
  if FXML = nil then
  begin
    FXML := TXMLDocument.Create(nil);
    FXML.Active := True;
    FXML.Encoding := 'utf-8';
    FXML.StandAlone := 'yes';
    FXML.DocumentElement := FXML.CreateNode(DocumentElementIdent);
  end;
  Result := FXML;
end;

function TXmlStorage.GetSettingsNode(CanCreate: Boolean): IXMLNode;
begin
  Result := XML.DocumentElement.ChildNodes.FindNode(SettingsIdent);
  if (Result = nil) and CanCreate then
    Result := XML.DocumentElement.AddChild(SettingsIdent)
end;

function TXmlStorage.GetSettingNode(const Name: string;
  CanCreate: Boolean): IXMLNode;
var
  Settings: IXMLNode;
  I: Integer;
begin
  Result := nil;
  Settings := GetSettingsNode(CanCreate);
  if Settings = nil then Exit;
  for I := 0 to Settings.ChildNodes.Count - 1 do
    if NodeIsSetting(Settings.ChildNodes[I], Name) then
    begin
      Result := Settings.ChildNodes[I];
      Exit;
    end;
  if not CanCreate then Exit;
  Result := Settings.AddChild(SettingIdent);
  Result.Attributes[SettingNameIdent] := Name;
end;

function TXmlStorage.NodeIsSetting(Node: IXMLNode;
  const Name: string): Boolean;
begin
  Result := SameText(Node.NodeName, SettingIdent)
    and not VarIsNull(Node.Attributes[SettingNameIdent])
    and SameText(Node.Attributes[SettingNameIdent], Name);
end;

function TXmlStorage.Exists(const Name: string): Boolean;
begin
  Result := Assigned(GetSettingNode(Name, False));
end;

procedure TXmlStorage.Delete(const Name: string);
var
  Settings: IXMLNode;
  I: Integer;
begin
  Settings := GetSettingsNode(False);
  if Settings = nil then Exit;
  for I := 0 to Settings.ChildNodes.Count - 1 do
    if NodeIsSetting(Settings.ChildNodes[I], Name) then
      Settings.ChildNodes.Delete(I);
end;

function TXmlStorage.ReadValue(const Name: string; Default: Variant): Variant;
var
  Setting: IXMLNode;
begin
  Setting := GetSettingNode(Name, False);
  if Assigned(Setting) and not VarIsNull(Setting.NodeValue) then
    Result := Setting.NodeValue
  else
    Result := Default;
end;

function TXmlStorage.ReadBoolean(const Name: string; Default: Boolean): Boolean;
begin
  Result := StrToBoolDef(ReadValue(Name, Default), Default);
end;

function TXmlStorage.ReadInteger(const Name: string; Default: Integer): Integer;
begin
  Result := StrToIntDef(ReadValue(Name, Default), Default);
end;

function TXmlStorage.ReadFloat(const Name: string; Default: Double): Double;
var
  SaveSeparator: Char;
begin
  SaveSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  try
    Result := StrToFloatDef(ReadValue(Name, Default), Default);
  finally
    FormatSettings.DecimalSeparator := SaveSeparator;
  end;
end;

function TXmlStorage.ReadString(const Name: string; Default: string): string;
begin
  Result := ReadValue(Name, Default);
end;

procedure TXmlStorage.WriteValue(const Name, ValueType: string; Value: Variant);
var
  Setting: IXMLNode;
begin
  Setting := GetSettingNode(Name, True);
  Setting.Attributes[SettingTypeIdent] := ValueType;
  Setting.NodeValue := Value;
end;

procedure TXmlStorage.WriteBoolean(const Name: string; Value: Boolean);
begin
  WriteValue(Name, BooleanTypeIdent, Value);
end;

procedure TXmlStorage.WriteInteger(const Name: string; Value: Integer);
begin
  WriteValue(Name, IntegerTypeIdent, Value);
end;

procedure TXmlStorage.WriteFloat(const Name: string; Value: Double);
var
  SaveSeparator: Char;
begin
  SaveSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  try
    WriteValue(Name, FloatTypeIdent, Value);
  finally
    FormatSettings.DecimalSeparator := SaveSeparator;
  end;
end;

procedure TXmlStorage.WriteString(const Name, Value: string);
begin
  WriteValue(Name, StringTypeIdent, Value);
end;

procedure TXmlStorage.LoadFromFile(const FileName: string);
var
  TempXML: IXMLDocument;
begin
  if FileExists(FileName) then
  begin
    XML.LoadFromFile(FileName);

    if Assigned(FMigrationHandler) then
    begin
      TempXML := XML; XML := nil;
      TempXML := FMigrationHandler(Self, TempXML);
      if Assigned(TempXML) then XML := TempXML;
    end;
  end
  else
    XML := nil;
end;

procedure TXmlStorage.SaveToFile(const FileName: string);
begin
  if not XML.DocumentElement.HasAttribute('xmlns:xsi') then
  begin
    XML.DocumentElement
      .Attributes['xmlns:xsi'] := 'http://www.w3.org/2001/XMLSchema-instance';
    XML.DocumentElement
      .Attributes['xsi:noNamespaceSchemaLocation'] := SchemaLocation;
  end;
  XML.SaveToFile(FileName);
end;

end.
