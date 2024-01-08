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

unit uO2Xml;

interface

uses
  Classes, XMLDoc, XMLIntf, xmldom, msxmldom;

type
  TO2XmlFiler = class
  private
    FInstance: TPersistent;
  protected
    const SchemaLocationFmt =
      'https://maurizuki.github.io/O2/xml/O2File/%d%d.xsd';
    const ItemIdent = 'item';
    function GetPropCount(AInstance: TPersistent): Integer;
    function GetPropName(AInstance: TPersistent; Index: Integer): string;
    function CreateXMLDocument: IXMLDocument;
  public
    constructor Create(AInstance: TPersistent); virtual;
    property Instance: TPersistent read FInstance;
  end;

  TO2XmlReader = class(TO2XmlFiler)
  private
    function ReadValue(const Node: IXMLNode;
      const Default: Variant): Variant;
    function ReadEnumeration(const Node: IXMLNode;
      const Default: Variant): Variant;
    procedure ReadStringList(const Node: IXMLNode;
      const AStringList: TStrings);
    procedure ReadCollection(const Node: IXMLNode;
      const ACollection: TCollection);
    procedure ReadProperty(const Node: IXMLNode;
      const PropName: string; const AInstance: TPersistent);
    procedure ReadPersistent(const Node: IXMLNode;
      const AInstance: TPersistent);
  public
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromString(const S: string);
  end;

  TO2XmlWriter = class(TO2XmlFiler)
  private
    procedure WriteValue(const Node: IXMLNode;
      const Value: Variant);
    procedure WriteStringList(const Node: IXMLNode;
      const AStringList: TStrings);
    procedure WriteCollection(const Node: IXMLNode;
      const ACollection: TCollection);
    procedure WriteProperty(const Node: IXMLNode;
      const PropName: string; const AInstance: TPersistent);
    procedure WritePersistent(const Node: IXMLNode;
      const AInstance: TPersistent);
  public
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    function SaveToString: string;
  end;

implementation

uses
  SysUtils, TypInfo, Variants, uO2Defs;

{ TO2XmlFiler }

constructor TO2XmlFiler.Create(AInstance: TPersistent);
begin
  inherited Create;
  FInstance := AInstance;
end;

function TO2XmlFiler.GetPropCount(AInstance: TPersistent): Integer;
begin
  Result := GetTypeData(AInstance.ClassInfo)^.PropCount;
end;

function TO2XmlFiler.GetPropName(AInstance: TPersistent;
  Index: Integer): string;
var
  PropList: PPropList;
  PropCount: Integer;
begin
  Result := '';
  PropCount := GetPropList(AInstance, PropList);
  if PropCount > 0 then
  try
    Result := TypInfo.GetPropName(PropList^[Index]);
  finally
    FreeMem(PropList, PropCount * SizeOf(PPropInfo));
  end;
end;

function TO2XmlFiler.CreateXMLDocument: IXMLDocument;
begin
  Result := TXMLDocument.Create(nil);
  Result.Active := True;
  Result.Encoding := 'utf-8';
  Result.StandAlone := 'yes';
  Result.DocumentElement := Result.CreateNode(FInstance.ClassName);
  Result.DocumentElement.Attributes['xmlns:xsi'] :=
    'http://www.w3.org/2001/XMLSchema-instance';
  Result.DocumentElement.Attributes['xsi:noNamespaceSchemaLocation'] :=
    Format(SchemaLocationFmt, [O2FileVersion.Hi, O2FileVersion.Lo]);
end;

{ TO2XmlReader }

function TO2XmlReader.ReadValue(const Node: IXMLNode;
  const Default: Variant): Variant;
begin
  if Assigned(Node) and not VarIsNull(Node.NodeValue) then
    Result := Node.NodeValue
  else
    Result := Default;
end;

function TO2XmlReader.ReadEnumeration(const Node: IXMLNode;
  const Default: Variant): Variant;
var
  IntValue: Integer;
begin
  if Assigned(Node) and not VarIsNull(Node.NodeValue) then
    if ((VarType(Node.NodeValue) = varString)
      or (VarType(Node.NodeValue) = varOleStr)
      or (VarType(Node.NodeValue) = varUString))
      and TryStrToInt(Node.NodeValue, IntValue) then
      Result := IntValue
    else
      Result := Node.NodeValue
  else
    Result := Default;
end;

procedure TO2XmlReader.ReadStringList(const Node: IXMLNode;
  const AStringList: TStrings);
var
  I: Integer;
begin
  AStringList.BeginUpdate;
  try
    AStringList.Clear;
    if not Assigned(Node) then Exit;
    for I := 0 to Node.ChildNodes.Count - 1 do
      AStringList.Add(ReadValue(Node.ChildNodes[I], ''));
  finally
    AStringList.EndUpdate;
  end;
end;

procedure TO2XmlReader.ReadCollection(const Node: IXMLNode;
  const ACollection: TCollection);
var
  I: Integer;
begin
  ACollection.BeginUpdate;
  try
    ACollection.Clear;
    if not Assigned(Node) then Exit;
    for I := 0 to Node.ChildNodes.Count - 1 do
      ReadPersistent(Node.ChildNodes[I], ACollection.Add);
  finally
    ACollection.EndUpdate;
  end;
end;

procedure TO2XmlReader.ReadProperty(const Node: IXMLNode;
  const PropName: string; const AInstance: TPersistent);
var
  AObject: TObject;
begin
  if (Node = nil) or (AInstance = nil) then Exit;
  case PropType(AInstance, PropName) of
    tkClass:
    begin
      AObject := GetObjectProp(AInstance, PropName);
      if AObject is TStrings then
        ReadStringList(Node, AObject as TStrings)
      else if AObject is TCollection then
        ReadCollection(Node, AObject as TCollection)
      else
        ReadPersistent(Node, AObject as TPersistent);
    end;
    tkEnumeration:
      SetPropValue(AInstance, PropName,
        ReadEnumeration(Node, GetPropValue(AInstance, PropName)));
    else
      SetPropValue(AInstance, PropName,
        ReadValue(Node, GetPropValue(AInstance, PropName)));
  end;
end;

procedure TO2XmlReader.ReadPersistent(const Node: IXMLNode;
  const AInstance: TPersistent);
var
  PropName: string;
  I: Integer;
begin
  if (Node = nil) or (AInstance = nil) then Exit;
  for I := 0 to GetPropCount(AInstance) - 1 do
  begin
    PropName := GetPropName(AInstance, I);
    ReadProperty(Node.ChildNodes.FindNode(PropName), PropName, AInstance);
  end;
end;

procedure TO2XmlReader.LoadFromStream(Stream: TStream);
var
  XML: IXMLDocument;
begin
  XML := CreateXMLDocument;
  XML.LoadFromStream(Stream);
  ReadPersistent(XML.DocumentElement, Instance);
end;

procedure TO2XmlReader.LoadFromFile(const FileName: string);
var
  XML: IXMLDocument;
begin
  XML := CreateXMLDocument;
  XML.LoadFromFile(FileName);
  ReadPersistent(XML.DocumentElement, Instance);
end;

procedure TO2XmlReader.LoadFromString(const S: string);
var
  XML: IXMLDocument;
begin
  XML := CreateXMLDocument;
  XML.LoadFromXML(S);
  ReadPersistent(XML.DocumentElement, Instance);
end;

{ TO2XmlWriter }

procedure TO2XmlWriter.WriteValue(const Node: IXMLNode; const Value: Variant);
begin
  Node.NodeValue := Value;
end;

procedure TO2XmlWriter.WriteStringList(const Node: IXMLNode;
  const AStringList: TStrings);
var
  I: Integer;
begin
  for I := 0 to AStringList.Count - 1 do
    WriteValue(Node.AddChild(ItemIdent), AStringList[I]);
end;

procedure TO2XmlWriter.WriteCollection(const Node: IXMLNode;
  const ACollection: TCollection);
var
  I: Integer;
begin
  for I := 0 to ACollection.Count - 1 do
    WritePersistent(Node.AddChild(ItemIdent), ACollection.Items[I]);
end;

procedure TO2XmlWriter.WriteProperty(const Node: IXMLNode;
  const PropName: string; const AInstance: TPersistent);
var
  AObject: TObject;
begin
  if (Node = nil) or (AInstance = nil) then Exit;
  if PropType(AInstance, PropName) = tkClass then
  begin
    AObject := GetObjectProp(AInstance, PropName);
    if AObject is TStrings then
      WriteStringList(Node, AObject as TStrings)
    else if AObject is TCollection then
      WriteCollection(Node, AObject as TCollection)
    else
      WritePersistent(Node, AObject as TPersistent);
  end
  else
    WriteValue(Node, GetPropValue(AInstance, PropName));
end;

procedure TO2XmlWriter.WritePersistent(const Node: IXMLNode;
  const AInstance: TPersistent);
var
  PropName: string;
  I: Integer;
begin
  if (Node = nil) or (AInstance = nil) then Exit;
  for I := 0 to GetPropCount(AInstance) - 1 do
  begin
    PropName := GetPropName(AInstance, I);
    WriteProperty(Node.AddChild(PropName), PropName, AInstance);
  end;
end;

procedure TO2XmlWriter.SaveToStream(Stream: TStream);
var
  XML: IXMLDocument;
begin
  XML := CreateXMLDocument;
  WritePersistent(XML.DocumentElement, Instance);
  XML.SaveToStream(Stream);
end;

procedure TO2XmlWriter.SaveToFile(const FileName: string);
var
  XML: IXMLDocument;
begin
  XML := CreateXMLDocument;
  WritePersistent(XML.DocumentElement, Instance);
  XML.SaveToFile(FileName);
end;

function TO2XmlWriter.SaveToString: string;
var
  XML: IXMLDocument;
begin
  XML := CreateXMLDocument;
  WritePersistent(XML.DocumentElement, Instance);
  XML.SaveToXML(Result);
end;

end.
