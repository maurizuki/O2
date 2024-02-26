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
  Classes, uO2File, uO2Rules;

type
  IAppFiles = interface
    function FileExists(const Name: string): Boolean;
    function GetPortableFilesTotalSize: Int64;
    procedure InstallPortable(const Path: string);
    function GetFullPath(IndexOrName: Variant): string;
    property FullPath[IndexOrName: Variant]: string read GetFullPath;
  end;

  IStorage = interface
    function Exists(const Name: string): Boolean;
    procedure Delete(const Name: string);
    function ReadBoolean(const Name: string; Default: Boolean = False): Boolean;
    function ReadInteger(const Name: string; Default: Integer = 0): Integer;
    function ReadIntIdent(const Name: string;
      const Map: array of TIdentMapEntry; Default: Integer = 0): Integer;
    function ReadFloat(const Name: string; Default: Double = 0): Double;
    function ReadString(const Name: string; Default: string = ''): string;
    procedure ReadStringList(const Name: string; const AStringList: TStrings);
    procedure WriteBoolean(const Name: string; Value: Boolean);
    procedure WriteInteger(const Name: string; Value: Integer);
    procedure WriteIntIdent(const Name: string;
      const Map: array of TIdentMapEntry; Value: Integer);
    procedure WriteFloat(const Name: string; Value: Double);
    procedure WriteString(const Name, Value: string);
    procedure WriteStringList(const Name: string; const AStringList: TStrings);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
  end;

  IPasswordScoreCache = interface(IPasswordScoreProvider)
    procedure Update(const O2File: TO2File); overload;
    procedure Update(const O2File: TO2File; ObjectIndex: Integer); overload;
  end;

  IFileOperation = interface
    procedure Execute(const FileName: string);
  end;

implementation

end.
