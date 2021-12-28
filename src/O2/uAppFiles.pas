{ ******************************************************************** }
{                                                                      }
{ This Source Code Form is subject to the terms of the Mozilla Public  }
{ License, v. 2.0. If a copy of the MPL was not distributed with this  }
{ file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                      }
{ The initial Contributor is Maurizio Basaglia.                        }
{                                                                      }
{ Portions created by the initial Contributor are Copyright (C)        }
{ 2004-2022 the initial Contributor. All rights reserved.              }
{                                                                      }
{ Contributor(s):                                                      }
{                                                                      }
{ ******************************************************************** }

unit uAppFiles;

interface

uses
  Classes;

type
  TAppFile = class(TObject)
  private
    FFileName: string;
    FPath: string;
    function GetFullPath: string;
    procedure SetPath(const Value: string);
  protected
    function GetSize: Int64;
  public
    constructor Create(const FileName: string);
    property FileName: string read FFileName;
    property Path: string read FPath write SetPath;
    property FullPath: string read GetFullPath;
    property Size: Int64 read GetSize;
  end;

  TGetDataEvent = procedure(Sender: TObject; F: TStream) of object;

  TPortableAppFile = class(TAppFile)
  private
    FPortablePath: string;
    FOverwritePrompt: string;
    FOnGetData: TGetDataEvent;
    procedure SetPortablePath(const Value: string);
    procedure SetOverwritePrompt(const Value: string);
  protected
    function GetSize: Int64;
  public
    procedure InstallPortable(InstallPath: string);
    property PortablePath: string read FPortablePath write SetPortablePath;
    property OverwritePrompt: string read FOverwritePrompt
      write SetOverwritePrompt;
    property OnGetData: TGetDataEvent read FOnGetData write FOnGetData;
  end;

  TAppFiles = class(TObject)
  private
    FFiles: TStrings;
    function GetFiles(IndexOrName: Variant): TAppFile;
    function GetFullPath(IndexOrName: Variant): string;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const Name: string; const AppFile: TAppFile): Integer;
    function Exists(const Name: string): Boolean;
    function InstallPortableSpaceRequired: Int64;
    procedure InstallPortable(const Path: string);
    property Count: Integer read GetCount;
    property Files[IndexOrName: Variant]: TAppFile read GetFiles;
    property FullPath[IndexOrName: Variant]: string read GetFullPath;
  end;

function AppFiles: TAppFiles;

implementation

uses
  Windows, Forms, SysUtils, Variants, uUtils;

var
  FAppFiles: TAppFiles = nil;

function AppFiles: TAppFiles;
begin
  if FAppFiles = nil then FAppFiles := TAppFiles.Create;
  Result := FAppFiles;
end;

{ TAppFile }

constructor TAppFile.Create(const FileName: string);
begin
  FFileName := FileName;
end;

function TAppFile.GetFullPath: string;
begin
  Result := IncludeTrailingPathDelimiter(Path) + FileName;
end;

function TAppFile.GetSize: Int64;
begin
  Result := GetFileSize(FullPath);
end;

procedure TAppFile.SetPath(const Value: string);
begin
  FPath := Value;
end;

{ TPortableAppFile }

procedure TPortableAppFile.SetPortablePath(const Value: string);
begin
  FPortablePath := Value;
end;

procedure TPortableAppFile.SetOverwritePrompt(const Value: string);
begin
  FOverwritePrompt := Value;
end;

function TPortableAppFile.GetSize: Int64;
var
  F: TMemoryStream;
begin
  if Assigned(OnGetData) then
  begin
    F := TMemoryStream.Create;
    try
      OnGetData(Self, F);
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
  F: TFileStream;
begin
  InstallPath := IncludeTrailingPathDelimiter(InstallPath)
    + IncludeTrailingPathDelimiter(PortablePath);
  if (OverwritePrompt = '')
    or not FileExists(InstallPath + FileName)
    or YesNoBox(OverwritePrompt) then
  begin
    ForceDirectories(InstallPath);
    if Assigned(OnGetData) then
    begin
      F := TFileStream.Create(InstallPath + FileName, fmCreate);
      try
        OnGetData(Self, F);
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
  for I := 0 to FFiles.Count - 1 do
    FFiles.Objects[I].Free;
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

function TAppFiles.Add(const Name: string; const AppFile: TAppFile): Integer;
begin
  Result := FFiles.AddObject(Name, AppFile);
end;

function TAppFiles.Exists(const Name: string): Boolean;
begin
  Result := FileExists(FullPath[Name]);
end;

function TAppFiles.InstallPortableSpaceRequired: Int64;
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

initialization

finalization
  if Assigned(FAppFiles) then FAppFiles.Free;

end.
