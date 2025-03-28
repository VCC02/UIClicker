{
    Copyright (C) 2025 VCC
    creation date: 16 Jan 2025
    initial release date: 16 Jan 2025

    author: VCC
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:
    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
    DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
    OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}


unit ClickerPluginInMemFileSystem;

{$mode Delphi}

interface

uses
  Classes, SysUtils, {InMemFileSystem,} ClickerActionPlugins;

type
  TPluginInMemFileSystem = class
  private
    FContent: string;
    FPluginReference: Pointer;

    procedure AssertPluginReference;
    procedure HandleOnFileContent(AStreamContent: Pointer; AStreamSize: Int64); cdecl;

  public
    constructor Create;

    procedure LoadFileFromMem(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS_Obj; AName: string; AContent: Pointer; AvailableIndex: Integer = -1); overload;
    procedure SaveFileToMem(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS_Obj; AName: string; AContent: Pointer; ASize: Int64); overload;
    function GetFileSize(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS_Obj; AName: string): Int64; overload;
    function ListMemFilesAsString(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS_Obj): string; overload;
    function FileExistsInMem(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS_Obj; AFileName: string): Boolean; overload;
    procedure DeleteFileFromMem(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS_Obj; AFileName: string); overload;
    function DuplicateFile(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS_Obj; AFileName: string): string; overload;
    procedure RenameFile(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS_Obj; AFileName, NewFileName: string); overload;

    //Same set of functions as above, but AOnActionPlugin_InMemFS is of type TOnActionPlugin_InMemFS instead of TOnActionPlugin_InMemFS_Obj.
    procedure LoadFileFromMem(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; AName: string; AContent: Pointer; AvailableIndex: Integer = -1); overload;
    procedure SaveFileToMem(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; AName: string; AContent: Pointer; ASize: Int64); overload;
    function GetFileSize(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; AName: string): Int64; overload;
    function ListMemFilesAsString(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS): string; overload;
    function FileExistsInMem(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; AFileName: string): Boolean; overload;
    procedure DeleteFileFromMem(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; AFileName: string); overload;
    function DuplicateFile(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; AFileName: string): string; overload;
    procedure RenameFile(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; AFileName, NewFileName: string); overload;

    property PluginReference: Pointer write FPluginReference;
  end;


implementation


constructor TPluginInMemFileSystem.Create;
begin
  inherited Create;
  FPluginReference := nil;
  FContent := '';
end;


procedure TPluginInMemFileSystem.AssertPluginReference;
begin
  if FPluginReference = nil then
    raise Exception.Create('PluginReference not set in TPluginInMemFileSystem');
end;


procedure TPluginInMemFileSystem.HandleOnFileContent(AStreamContent: Pointer; AStreamSize: Int64); cdecl;
begin
  SetLength(FContent, AStreamSize);
  Move(AStreamContent^, FContent[1], AStreamSize);
end;


procedure TPluginInMemFileSystem.LoadFileFromMem(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS_Obj; AName: string; AContent: Pointer; AvailableIndex: Integer = -1);
begin
  AOnActionPlugin_InMemFS(CPluginInMemFSFunc_LoadFileFromMem, @AName[1], AContent, {Length(AName)} 0, 0, HandleOnFileContent);
end;


procedure TPluginInMemFileSystem.SaveFileToMem(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS_Obj; AName: string; AContent: Pointer; ASize: Int64);
begin
  AOnActionPlugin_InMemFS(CPluginInMemFSFunc_SaveFileToMem, @AName[1], AContent, {Length(AName)} 0, ASize, HandleOnFileContent);
end;


function TPluginInMemFileSystem.GetFileSize(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS_Obj; AName: string): Int64;
begin
  Result := AOnActionPlugin_InMemFS(CPluginInMemFSFunc_GetFileSize, @AName[1], nil, {Length(AName)} 0, 0, HandleOnFileContent);
end;


function TPluginInMemFileSystem.ListMemFilesAsString(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS_Obj): string;
begin
  AOnActionPlugin_InMemFS(CPluginInMemFSFunc_ListMemFiles, nil, nil, 0, 0, HandleOnFileContent);
  Result := FContent;
end;


function TPluginInMemFileSystem.FileExistsInMem(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS_Obj; AFileName: string): Boolean;
begin
  Result := Boolean(AOnActionPlugin_InMemFS(CPluginInMemFSFunc_FileExistsInMem, @AFileName[1], nil, 0, 0, HandleOnFileContent));
end;


procedure TPluginInMemFileSystem.DeleteFileFromMem(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS_Obj; AFileName: string);
begin
  AOnActionPlugin_InMemFS(CPluginInMemFSFunc_DeleteFileFromMem, @AFileName[1], nil, 0, 0, HandleOnFileContent);
end;


function TPluginInMemFileSystem.DuplicateFile(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS_Obj; AFileName: string): string;
begin
  AOnActionPlugin_InMemFS(CPluginInMemFSFunc_DuplicateFile, @AFileName[1], nil, 0, 0, HandleOnFileContent);  //returns length of FContent
  Result := FContent;
end;


procedure TPluginInMemFileSystem.RenameFile(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS_Obj; AFileName, NewFileName: string);
var
  ExRes: Integer;
begin
  ExRes := AOnActionPlugin_InMemFS(CPluginInMemFSFunc_DeleteFileFromMem, @AFileName[1], @NewFileName[1], 0, 0, HandleOnFileContent);
  case ExRes of
    -1: raise Exception.Create('New file already exists on renaming. "' + AFileName + '"');
    -2: raise Exception.Create('Can''t properly rename file. "' + AFileName + '"' + #13#10 + 'AV');
    -3: raise Exception.Create('Can''t find file to rename. "' + AFileName + '"');
    else
      ;
  end;
end;



//Overloads
procedure TPluginInMemFileSystem.LoadFileFromMem(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; AName: string; AContent: Pointer; AvailableIndex: Integer = -1);
begin
  AssertPluginReference;
  AOnActionPlugin_InMemFS(FPluginReference, CPluginInMemFSFunc_LoadFileFromMem, @AName[1], AContent, {Length(AName)} 0, 0, HandleOnFileContent);
end;


procedure TPluginInMemFileSystem.SaveFileToMem(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; AName: string; AContent: Pointer; ASize: Int64);
begin
  AssertPluginReference;
  AOnActionPlugin_InMemFS(FPluginReference, CPluginInMemFSFunc_SaveFileToMem, @AName[1], AContent, {Length(AName)} 0, ASize, HandleOnFileContent);
end;


function TPluginInMemFileSystem.GetFileSize(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; AName: string): Int64;
begin
  AssertPluginReference;
  Result := AOnActionPlugin_InMemFS(FPluginReference, CPluginInMemFSFunc_GetFileSize, @AName[1], nil, {Length(AName)} 0, 0, HandleOnFileContent);
end;


function TPluginInMemFileSystem.ListMemFilesAsString(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS): string;
begin
  AssertPluginReference;
  AOnActionPlugin_InMemFS(FPluginReference, CPluginInMemFSFunc_ListMemFiles, nil, nil, 0, 0, HandleOnFileContent);
  Result := FContent;
end;


function TPluginInMemFileSystem.FileExistsInMem(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; AFileName: string): Boolean;
begin
  AssertPluginReference;
  Result := Boolean(AOnActionPlugin_InMemFS(FPluginReference, CPluginInMemFSFunc_FileExistsInMem, @AFileName[1], nil, 0, 0, HandleOnFileContent));
end;


procedure TPluginInMemFileSystem.DeleteFileFromMem(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; AFileName: string);
begin
  AssertPluginReference;
  AOnActionPlugin_InMemFS(FPluginReference, CPluginInMemFSFunc_DeleteFileFromMem, @AFileName[1], nil, 0, 0, HandleOnFileContent);
end;


function TPluginInMemFileSystem.DuplicateFile(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; AFileName: string): string;
begin
  AssertPluginReference;
  AOnActionPlugin_InMemFS(FPluginReference, CPluginInMemFSFunc_DuplicateFile, @AFileName[1], nil, 0, 0, HandleOnFileContent);  //returns length of FContent
  Result := FContent;
end;


procedure TPluginInMemFileSystem.RenameFile(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; AFileName, NewFileName: string);
var
  ExRes: Integer;
begin
  AssertPluginReference;

  ExRes := AOnActionPlugin_InMemFS(FPluginReference, CPluginInMemFSFunc_DeleteFileFromMem, @AFileName[1], @NewFileName[1], 0, 0, HandleOnFileContent);
  case ExRes of
    -1: raise Exception.Create('New file already exists on renaming. "' + AFileName + '"');
    -2: raise Exception.Create('Can''t properly rename file. "' + AFileName + '"' + #13#10 + 'AV');
    -3: raise Exception.Create('Can''t find file to rename. "' + AFileName + '"');
    else
      ;
  end;
end;


end.

