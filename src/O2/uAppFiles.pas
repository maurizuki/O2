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

unit uAppFiles;

interface

uses
  Classes, uServices;

type
  TAppFile = class
  private
    FFileName: string;
    FPath: string;
    function GetFullPath: string;
  protected
    function GetSize: Int64;
  public
    constructor Create(const FileName, Path: string);
    property FileName: string read FFileName;
    property Path: string read FPath;
    property FullPath: string read GetFullPath;
    property Size: Int64 read GetSize;
  end;

  TPortableAppFile = class(TAppFile)
  private
    FPortablePath: string;
    FOverwritePrompt: string;
    FContent: string;
  protected
    function GetSize: Int64;
  public
    constructor Create(const FileName, Path, PortablePath: string);
    procedure InstallPortable(InstallPath: string);
    property PortablePath: string read FPortablePath;
    property OverwritePrompt: string read FOverwritePrompt
      write FOverwritePrompt;
    property Content: string read FContent write FContent;
  end;

  TAppFiles = class(TInterfacedObject, IAppFiles)
  private
    FFiles: TStrings;
    function GetFiles(IndexOrName: Variant): TAppFile;
    function GetFullPath(IndexOrName: Variant): string;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const Name, FileName, Path, PortablePath: string): TAppFiles;
      overload;
    function Add(const Name, FileName, Path, PortablePath,
      OverwritePrompt: string): TAppFiles; overload;
    function AddInMemory(const Name, FileName, Path, PortablePath,
      Content: string): TAppFiles;
    function FileExists(const Name: string): Boolean;
    function GetPortableFilesTotalSize: Int64;
    procedure InstallPortable(const Path: string);
    property Count: Integer read GetCount;
    property Files[IndexOrName: Variant]: TAppFile read GetFiles;
    property FullPath[IndexOrName: Variant]: string read GetFullPath;
  end;

implementation

uses
  Windows, Forms, SysUtils, Variants, uUtils;

{ TAppFile }

constructor TAppFile.Create(const FileName, Path: string);
begin
  FFileName := FileName;
  FPath := Path;
end;

function TAppFile.GetFullPath: string;
begin
  Result := IncludeTrailingPathDelimiter(Path) + FileName;
end;

function TAppFile.GetSize: Int64;
begin
  Result := GetFileSize(FullPath);
end;

{ TPortableAppFile }

constructor TPortableAppFile.Create(const FileName, Path, PortablePath: string);
begin
  inherited Create(FileName, Path);
  FPortablePath := PortablePath;
end;

function TPortableAppFile.GetSize: Int64;
var
  Writer: TTextWriter;
  F: TMemoryStream;
begin
  if FContent <> '' then
  begin
    F := TMemoryStream.Create;
    try
      Writer := TStreamWriter.Create(F);
      try
        Writer.Write(FContent);
      finally
        Writer.Free;
      end;

      Result := F.Size;
    finally
      F.Free;
    end;
  end
  else
    Result := inherited GetSize;
end;

procedure TPortableAppFile.InstallPortable(InstallPath: string);
var
  Writer: TTextWriter;
  F: TFileStream;
begin
  InstallPath := IncludeTrailingPathDelimiter(InstallPath)
    + IncludeTrailingPathDelimiter(PortablePath);
  if (OverwritePrompt = '')
    or not FileExists(InstallPath + FileName)
    or YesNoBox(OverwritePrompt) then
  begin
    ForceDirectories(InstallPath);
    if FContent <> '' then
    begin
      F := TFileStream.Create(InstallPath + FileName, fmCreate);
      try
        Writer := TStreamWriter.Create(F);
        try
          Writer.Write(FContent);
        finally
          Writer.Free;
        end;
      finally
        F.Free;
      end;
    end
    else
      CopyFile(PWideChar(FullPath), PWideChar(InstallPath + FileName), False);
  end;
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

function TAppFiles.GetFullPath(IndexOrName: Variant): string;
begin
  Result := Files[IndexOrName].FullPath;
end;

function TAppFiles.Add(const Name, FileName, Path,
  PortablePath: string): TAppFiles;
begin
  FFiles.AddObject(Name, TPortableAppFile.Create(FileName, Path, PortablePath));
  Result := Self;
end;

function TAppFiles.Add(const Name, FileName, Path, PortablePath,
  OverwritePrompt: string): TAppFiles;
var
  AppFile: TPortableAppFile;
begin
  AppFile := TPortableAppFile.Create(FileName, Path, PortablePath);
  AppFile.OverwritePrompt := OverwritePrompt;
  FFiles.AddObject(Name, AppFile);
  Result := Self;
end;

function TAppFiles.AddInMemory(const Name, FileName, Path, PortablePath,
  Content: string): TAppFiles;
var
  AppFile: TPortableAppFile;
begin
  AppFile := TPortableAppFile.Create(FileName, Path, PortablePath);
  AppFile.Content := Content;
  FFiles.AddObject(Name, AppFile);
  Result := Self;
end;

function TAppFiles.FileExists(const Name: string): Boolean;
var
  Index: Integer;
begin
  Index := FFiles.IndexOf(Name);
  if Index >= 0 then
    Result := SysUtils.FileExists(FullPath[Index])
  else
    Result := False;
end;

function TAppFiles.GetPortableFilesTotalSize: Int64;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Pred(Count) do
    if Files[I] is TPortableAppFile then
      Inc(Result, TPortableAppFile(Files[I]).Size);
end;

procedure TAppFiles.InstallPortable(const Path: string);
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
    if Files[I] is TPortableAppFile then
      TPortableAppFile(Files[I]).InstallPortable(Path);
end;

end.
