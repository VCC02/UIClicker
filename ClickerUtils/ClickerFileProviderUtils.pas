{
    Copyright (C) 2024 VCC
    creation date: 02 Apr 2024
    initial release date: 03 Apr 2024

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


unit ClickerFileProviderUtils;

{$mode Delphi}

interface

uses
  Classes, SysUtils;

type
  TFileProvider = class
  private
    FListOfAccessibleDirs: TStringList;
    FListOfAccessibleFileExtensions: TStringList;
    FFullTemplatesDir: string;
  public
    constructor Create;
    destructor Destroy; override;
    function FileIsAllowed(AFileName: string; out ADenyReason: string): Boolean;

    procedure AddListOfAccessibleDirs(AList: TStrings); overload;
    procedure AddListOfAccessibleFileExtensions(AList: TStrings); overload;
    procedure AddListOfAccessibleDirs(AListOfDirsStr: string); overload;
    procedure AddListOfAccessibleFileExtensions(AListOfExtensionsStr: string); overload;

    property FullTemplatesDir: string read FFullTemplatesDir write FFullTemplatesDir;
  end;


implementation


{TFileProvider}

constructor TFileProvider.Create;
begin
  inherited Create;
  FListOfAccessibleDirs := TStringList.Create;
  FListOfAccessibleFileExtensions := TStringList.Create;
  FListOfAccessibleDirs.LineBreak := #13#10;
  FListOfAccessibleFileExtensions.LineBreak := #13#10;
  FFullTemplatesDir := '';
end;


destructor TFileProvider.Destroy;
begin
  FreeAndNil(FListOfAccessibleDirs);
  FreeAndNil(FListOfAccessibleFileExtensions);
  inherited Destroy;
end;


function TFileProvider.FileIsAllowed(AFileName: string; out ADenyReason: string): Boolean;
const
  CFileExtensionNotAllowed = 'File extension is not allowed.';
  CFileOutOfAllowedDirs = 'File is outside of allowed directories.';
  CFileFromUpperDirs = 'Also, paths are not allowed to contain ".." directories.';
var
  FileExt, FilePath: string;
  i: Integer;
  FoundExt, FoundDir: Boolean;
  CurrentItem: string;
  TempFullTemplatesDir: string;
begin
  Result := False;
  ADenyReason := '';

  AFileName := UpperCase(AFileName);
  FileExt := ExtractFileExt(AFileName);

  CurrentItem := '.CLKTMPL';
  FoundExt := False;
  for i := 0 to FListOfAccessibleFileExtensions.Count - 1 do
  begin
    CurrentItem := FListOfAccessibleFileExtensions.Strings[i];
    if CurrentItem = FileExt then
    begin
      FoundExt := True;
      Break;
    end;
  end;

  if not FoundExt then
  begin
    ADenyReason := CFileExtensionNotAllowed;
    Exit;
  end;

  TempFullTemplatesDir := UpperCase(StringReplace(UpperCase(FFullTemplatesDir), '$APPDIR$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]));

  if (AFileName <> '') and (AFileName[1] = PathDelim) then   //resolve relative paths to TemplatesDir
    AFileName := UpperCase(TempFullTemplatesDir + AFileName);

  AFileName := UpperCase(StringReplace(AFileName, '$APPDIR$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]));
  AFileName := UpperCase(StringReplace(AFileName, '$TEMPLATEDIR$', TempFullTemplatesDir, [rfReplaceAll]));

  if ExtractFileName(AFileName) = AFileName then //files without paths are expected to be found in $AppDir$\ActionTemplates
    AFileName := UpperCase(TempFullTemplatesDir + PathDelim + AFileName);

  FoundDir := False;
  FilePath := ExtractFilePath(AFileName);

  for i := 0 to FListOfAccessibleDirs.Count - 1 do
  begin
    CurrentItem := FListOfAccessibleDirs.Strings[i];  //these are already UpperCase, as set from outside
    if (CurrentItem > '') and (CurrentItem[Length(CurrentItem)] <> PathDelim) then
      CurrentItem := CurrentItem + PathDelim;  //make sure there are consistent results, regardless of the last '\' existence

    if ((Pos(CurrentItem, FilePath) = 1) or (Pos(ExtractFileDir(AFileName), CurrentItem) > 1)) and
       (Pos('..', CurrentItem) = 0) then
    begin
      FoundDir := True;
      Break;
    end;
  end;

  if not FoundDir then
  begin
    ADenyReason := CFileOutOfAllowedDirs;

    if Pos('..', CurrentItem) > 0 then
      ADenyReason := ADenyReason + ' ' + CFileFromUpperDirs;

    //AddUniqueMessageToLog('Denied: "' + AFileName + '" from "' + FastReplace_ReturnTo45(FListOfAccessibleDirs.Text) + '"');  //for debugging
    Exit;
  end;

  Result := True;
end;


procedure TFileProvider.AddListOfAccessibleDirs(AList: TStrings); overload;
var
  i: Integer;
  CurrentItem, ExeDir: string;
begin
  ExeDir := ExtractFileDir(ParamStr(0));

  for i := 0 to AList.Count - 1 do
  begin
    CurrentItem := AList.Strings[i];
    CurrentItem := StringReplace(CurrentItem, '$AppDir$', ExeDir, [rfReplaceAll]);
    FListOfAccessibleDirs.Add(UpperCase(CurrentItem));
  end;
end;


procedure TFileProvider.AddListOfAccessibleFileExtensions(AList: TStrings); overload;
var
  i: Integer;
begin
  for i := 0 to AList.Count - 1 do
    FListOfAccessibleFileExtensions.Add(UpperCase(AList.Strings[i]));
end;


procedure TFileProvider.AddListOfAccessibleDirs(AListOfDirsStr: string); overload;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.LineBreak := #13#10;
    List.Text := AListOfDirsStr;
    AddListOfAccessibleDirs(List);
  finally
    List.Free;
  end;
end;


procedure TFileProvider.AddListOfAccessibleFileExtensions(AListOfExtensionsStr: string); overload;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.LineBreak := #13#10;
    List.Text := AListOfExtensionsStr;
    AddListOfAccessibleFileExtensions(List);
  finally
    List.Free;
  end;
end;

end.

