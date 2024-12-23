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

unit uAppFiles;

interface

uses
  Classes, SysUtils, uServices;

type
  TAppFile = class
  protected
    FFileName: string;
    FPortablePath: string;
    FOverwritePrompt: string;

    function GetFullPath: string; virtual; abstract;
    function GetSize: Int64; virtual; abstract;

    procedure CopyTo(NewFileName: string); virtual; abstract;
  public
    constructor Create(const FileName, PortablePath: string);

    procedure InstallPortable(InstallPath: string);

    property FullPath: string read GetFullPath;
    property Size: Int64 read GetSize;
    property OverwritePrompt: string read FOverwritePrompt
      write FOverwritePrompt;
  end;

  TAppFileInMemory = class(TAppFile)
  private
    FBytes: TBytes;
  protected
    function GetFullPath: string; override;
    function GetSize: Int64; override;

    procedure CopyTo(NewFileName: string); override;
  public
    constructor Create(const FileName, PortablePath: string; Bytes: TBytes);
  end;

  TAppFileOnDisk = class(TAppFile)
  private
    FPath: string;
  protected
    function GetFullPath: string; override;
    function GetSize: Int64; override;

    procedure CopyTo(NewFileName: string); override;
  public
    constructor Create(const FileName, Path, PortablePath: string);
  end;

  TAppFiles = class(TInterfacedObject, IAppFiles)
  private
    FFiles: TStrings;

    function GetFiles(IndexOrName: Variant): TAppFile;
    function GetFullPaths(IndexOrName: Variant): string;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const Name, FileName, Path, PortablePath: string): TAppFiles;
      overload;
    function Add(const Name, FileName, Path, PortablePath,
      OverwritePrompt: string): TAppFiles; overload;
    function Add(const Name, FileName, Path, PortablePath: string;
      Bytes: TBytes): TAppFiles; overload;

    function FileExists(const Name: string): Boolean;
    function GetTotalSize: Int64;
    procedure InstallPortable(const Path: string);

    property Count: Integer read GetCount;
    property Files[IndexOrName: Variant]: TAppFile read GetFiles;
    property FullPaths[IndexOrName: Variant]: string read GetFullPaths;
  end;

implementation

uses
  Windows, Forms, Variants, uUtils;

{ TAppFile }

constructor TAppFile.Create(const FileName, PortablePath: string);
begin
  FFileName := FileName;
  FPortablePath := PortablePath;
end;

procedure TAppFile.InstallPortable(InstallPath: string);
begin
  InstallPath := IncludeTrailingPathDelimiter(InstallPath)
    + IncludeTrailingPathDelimiter(FPortablePath);

  if (OverwritePrompt = '')
    or not FileExists(InstallPath + FFileName)
    or YesNoBox(OverwritePrompt) then
  begin
    ForceDirectories(InstallPath);
    CopyTo(InstallPath + FFileName);
  end;
end;

{ TAppFileInMemory }

constructor TAppFileInMemory.Create(const FileName, PortablePath: string;
  Bytes: TBytes);
begin
  inherited Create(FileName, PortablePath);
  FBytes := Bytes;
end;

function TAppFileInMemory.GetFullPath: string;
begin
  Result := FFileName;
end;

function TAppFileInMemory.GetSize: Int64;
begin
  Result := SizeOf(FBytes);
end;

procedure TAppFileInMemory.CopyTo(NewFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(NewFileName, fmCreate);
  try
    F.Write(FBytes, Length(FBytes));
  finally
    F.Free;
  end;
end;

{ TAppFileOnDisk }

constructor TAppFileOnDisk.Create(const FileName, Path, PortablePath: string);
begin
  inherited Create(FileName, PortablePath);
  FPath := Path;
end;

function TAppFileOnDisk.GetFullPath: string;
begin
  Result := IncludeTrailingPathDelimiter(FPath) + FFileName;
end;

function TAppFileOnDisk.GetSize: Int64;
var
  SearchResult: TSearchRec;
begin
  if FindFirst(FullPath, faAnyFile, SearchResult) = 0 then
    Result := SearchResult.Size
  else
    Result := -1;

  SysUtils.FindClose(SearchResult);
end;

procedure TAppFileOnDisk.CopyTo(NewFileName: string);
begin
  CopyFile(PWideChar(FullPath), PWideChar(NewFileName), False);
end;

{ TAppFiles }

constructor TAppFiles.Create;
begin
  inherited Create;
  FFiles := TStringList.Create;
end;

destructor TAppFiles.Destroy;
var
  I: Integer;
begin
  for I := 0 to FFiles.Count - 1 do FFiles.Objects[I].Free;
  FFiles.Free;
  inherited Destroy;
end;

function TAppFiles.GetCount: Integer;
begin
  Result := FFiles.Count;
end;

function TAppFiles.GetFiles(IndexOrName: Variant): TAppFile;
begin
  if VarIsOrdinal(IndexOrName) then
    Result := TAppFile(FFiles.Objects[IndexOrName])
  else
    Result := TAppFile(FFiles.Objects[FFiles.IndexOf(IndexOrName)]);
end;

function TAppFiles.GetFullPaths(IndexOrName: Variant): string;
begin
  Result := Files[IndexOrName].FullPath;
end;

function TAppFiles.Add(const Name, FileName, Path,
  PortablePath: string): TAppFiles;
begin
  FFiles.AddObject(Name, TAppFileOnDisk.Create(FileName, Path, PortablePath));
  Result := Self;
end;

function TAppFiles.Add(const Name, FileName, Path, PortablePath,
  OverwritePrompt: string): TAppFiles;
var
  AppFile: TAppFile;
begin
  AppFile := TAppFileOnDisk.Create(FileName, Path, PortablePath);
  AppFile.OverwritePrompt := OverwritePrompt;
  FFiles.AddObject(Name, AppFile);
  Result := Self;
end;

function TAppFiles.Add(const Name, FileName, Path, PortablePath: string;
  Bytes: TBytes): TAppFiles;
begin
  FFiles.AddObject(Name,
    TAppFileInMemory.Create(FileName, PortablePath, Bytes));
  Result := Self;
end;

function TAppFiles.FileExists(const Name: string): Boolean;
var
  Index: Integer;
begin
  Index := FFiles.IndexOf(Name);
  if Index = -1 then Exit(False);
  Result := SysUtils.FileExists(FullPaths[Index]);
end;

function TAppFiles.GetTotalSize: Int64;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do Inc(Result, Files[I].Size);
end;

procedure TAppFiles.InstallPortable(const Path: string);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do Files[I].InstallPortable(Path);
end;

end.
