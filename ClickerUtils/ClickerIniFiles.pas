{
    Copyright (C) 2025 VCC
    creation date: Dec 2019
    initial release date: 26 Jul 2022

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

unit ClickerIniFiles;

interface

uses
  SysUtils, Classes;

type
  TClkIniReadonlyFile = class(TObject)  //an implementation without trimming the values
  private
    FSections: TStringList;    //ToDo  see if TStringHash has better speed results
    FSectionContents: array of TStringList; //not sure if this is the best way to store the contents, but it is easy to understand

    procedure LoadSections(ASourceContent: TStringList); overload;
    procedure LoadSections(FileName: string); overload;
  public
    constructor Create(ASourceContent: TStringList); overload;
    constructor Create(ASourceContent: TMemoryStream); overload;
    constructor Create(const FileName: string); overload;
    constructor Create; overload;
    destructor Destroy; override;

    function GetSectionCount: Integer;
    function GetSectionIndex(Section: string): Integer;
    function GetSectionAtIndex(AIndex: Integer): string;

    function ReadString(const Section, Ident, Default: string): string; overload;
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint; overload;
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; overload;

    function ReadString(SectionIndex: Integer; const Ident, Default: string): string; overload;
    function ReadInteger(SectionIndex: Integer; const Ident: string; Default: Longint): Longint; overload;
    function ReadBool(SectionIndex: Integer; const Ident: string; Default: Boolean): Boolean; overload;

    procedure ReadSection(SectionIndex: Integer; Dest: TStringList);
  end;


  TClkIniFileOpenMode = (omStringList, omMemoryStream, omFileName);
  TClkIniFileWriteSectionMode = (smAddValues, smReplaceSection);  //smAddValues will add new values, without verifying if their keys already exist.  smReplaceSection discards existing section content.
                                                                  //ToDo: add smUpdateValues, which merges the new content with the existing one

  TClkIniFile = class(TClkIniReadonlyFile)     //This class does not support comments. They will be discarded/lost when calling UpdateFile.
  private
    FClkIniFileOpenMode: TClkIniFileOpenMode;
    FFileName: string;

    procedure CreateSection(ANewSectionName: string);
  public
    constructor Create(ASourceContent: TStringList); overload;
    constructor Create(ASourceContent: TMemoryStream); overload;
    constructor Create(const FileName: string); overload;
    constructor Create; overload;
    destructor Destroy; override;

    procedure GetFileContent(AContent: TStringList); overload;
    procedure GetFileContent(AContent: TMemoryStream); overload;
    procedure UpdateFile;

    procedure WriteString(const Section, Ident, Value: string); overload;
    procedure WriteInteger(const Section, Ident: string; Value: Longint); overload;
    procedure WriteBool(const Section, Ident: string; Value: Boolean); overload;

    procedure WriteString(SectionIndex: Integer; const Ident, Value: string; ANewSectionName: string = ''); overload;  //If SectionIndex is negative or out of range, the new section is created. That also requires that ANewSectionName is a non-empty.
    procedure WriteInteger(SectionIndex: Integer; const Ident: string; Value: Longint; ANewSectionName: string = ''); overload;   //If SectionIndex is negative or out of range, the new section is created. That also requires that ANewSectionName is a non-empty.
    procedure WriteBool(SectionIndex: Integer; const Ident: string; Value: Boolean; ANewSectionName: string = ''); overload;   //If SectionIndex is negative or out of range, the new section is created. That also requires that ANewSectionName is a non-empty.

    procedure WriteSection(SectionIndex: Integer; Src: TStringList; ANewSectionName: string = ''; AWriteSectionMode: TClkIniFileWriteSectionMode = smReplaceSection); //If SectionIndex is negative or out of range, the new section is created. That also requires that ANewSectionName is a non-empty.
  end;

  
implementation


constructor TClkIniReadonlyFile.Create(ASourceContent: TStringList);
begin
  inherited Create;
  FSections := TStringList.Create;
  LoadSections(ASourceContent);
end;


constructor TClkIniReadonlyFile.Create(ASourceContent: TMemoryStream);
var
  TempStringList: TStringList;
begin
  inherited Create;
  FSections := TStringList.Create;

  TempStringList := TStringList.Create;
  try
    TempStringList.LoadFromStream(ASourceContent);
    LoadSections(TempStringList);
  finally
    TempStringList.Free;
  end;
end;


constructor TClkIniReadonlyFile.Create(const FileName: string);
begin
  inherited Create;
  FSections := TStringList.Create;
  LoadSections(FileName);
end;


constructor TClkIniReadonlyFile.Create;
begin
  raise Exception.Create('IniReadonlyFile: This constructor was implemented to let you know that one of the other constructors should be called (those with an argument).');
end;


destructor TClkIniReadonlyFile.Destroy;
var
  i: Integer;
begin
  FSections.Free;

  for i := 0 to Length(FSectionContents) - 1 do
    FSectionContents[i].Free;

  SetLength(FSectionContents, 0);
  inherited Destroy;
end;


procedure TClkIniReadonlyFile.LoadSections(ASourceContent: TStringList);
var
  i, n: Integer;
  s, SectionName, TrimmedS: string;
begin
  SetLength(FSectionContents, 0);

  SectionName := '';
  n := -1;

  for i := 0 to ASourceContent.Count - 1 do
  begin
    s := ASourceContent.Strings[i];
    TrimmedS := Trim(s);

    if (TrimmedS <> '') and (TrimmedS[1] <> ';') then
    begin
      if (s[1] = '[') and (s[Length(s)] = ']') then
      begin
        SectionName := Copy(s, 2, Length(s) - 2);

        if SectionName > '' then
        begin
          FSections.Add(SectionName);

          n := Length(FSectionContents);
          SetLength(FSectionContents, n + 1);
          FSectionContents[n] := TStringList.Create;
        end;
      end
      else
        if n > -1 then
          FSectionContents[n].Add(s);  //this assumes the file is in a good format (section name before contents)
    end;
  end;
end;


procedure TClkIniReadonlyFile.LoadSections(FileName: string);
var
  FileContent: TStringList;
begin
  if not FileExists(FileName) then   //this may happen when saving a new ini file
    Exit;

  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FileName);
    LoadSections(FileContent);
  finally
    FileContent.Free;
  end;
end;


function TClkIniReadonlyFile.GetSectionCount: Integer;
begin
  Result := FSections.Count;
end;


function TClkIniReadonlyFile.GetSectionIndex(Section: string): Integer;
begin
  Result := FSections.IndexOf(Section);
end;


function TClkIniReadonlyFile.GetSectionAtIndex(AIndex: Integer): string;
begin
  if (AIndex < 0) or (AIndex > FSections.Count - 1) then
    raise Exception.Create('Section index out of bounds: ' + IntToStr(AIndex))
  else
    Result := FSections.Strings[AIndex];
end;


function GetIndentPairValue(ASection: TStringList; Ident: string; out Value: string): Integer;   //returns line index if found
var
  i, PosEqual: Integer;
  KeyValue, Key: string;
begin
  Result := -1;
  for i := 0 to ASection.Count - 1 do
  begin
    KeyValue := ASection.Strings[i];
    PosEqual := Pos('=', KeyValue);

    Key := Copy(KeyValue, 1, PosEqual - 1);
    if Key = Ident then
    begin
      Value := Copy(KeyValue, PosEqual + 1, MaxInt);
      Result := i;
      Exit;
    end;
  end;
end;


function TClkIniReadonlyFile.ReadString(const Section, Ident, Default: string): string;
var
  SectionIndex: Integer;
  Value: string;
begin
  SectionIndex := GetSectionIndex(Section);
  if SectionIndex = -1 then
  begin
    Result := Default;
    Exit;
  end;

  if GetIndentPairValue(FSectionContents[SectionIndex], Ident, Value) > -1 then
    Result := Value
  else
    Result := Default;
end;


function TClkIniReadonlyFile.ReadInteger(const Section, Ident: string; Default: Longint): Longint;
begin
  Result := StrToIntDef(ReadString(Section, Ident, ''), Default);
end;


function TClkIniReadonlyFile.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
begin
  Result := ReadInteger(Section, Ident, Ord(Default)) = 1;
end;


function TClkIniReadonlyFile.ReadString(SectionIndex: Integer; const Ident, Default: string): string;
var
  Value: string;
begin
  if (SectionIndex < 0) or (SectionIndex > Length(FSectionContents) - 1) then
  begin
    Result := Default;
    Exit;
  end;
  
  if GetIndentPairValue(FSectionContents[SectionIndex], Ident, Value) > -1 then
    Result := Value
  else
    Result := Default;
end;


function TClkIniReadonlyFile.ReadInteger(SectionIndex: Integer; const Ident: string; Default: Longint): Longint;
begin
  Result := StrToIntDef(ReadString(SectionIndex, Ident, ''), Default);
end;


function TClkIniReadonlyFile.ReadBool(SectionIndex: Integer; const Ident: string; Default: Boolean): Boolean;
begin
  Result := ReadInteger(SectionIndex, Ident, Ord(Default)) = 1;
end;


procedure TClkIniReadonlyFile.ReadSection(SectionIndex: Integer; Dest: TStringList);
begin
  if (SectionIndex < 0) or (SectionIndex > Length(FSectionContents) - 1) then
    raise Exception.Create('Section index out of bounds: ' + IntToStr(SectionIndex) + ' in ReadSection.');

  Dest.AddStrings(FSectionContents[SectionIndex]);
end;


{TClkIniFile}


constructor TClkIniFile.Create(ASourceContent: TStringList);
begin
  inherited Create(ASourceContent);
  FClkIniFileOpenMode := omStringList;
  FFileName := '';
end;


constructor TClkIniFile.Create(ASourceContent: TMemoryStream);
begin
  inherited Create(ASourceContent);
  FClkIniFileOpenMode := omMemoryStream;
  FFileName := '';
end;


constructor TClkIniFile.Create(const FileName: string);
begin
  inherited Create(FileName);
  FClkIniFileOpenMode := omFileName;
  FFileName := FileName;
end;


constructor TClkIniFile.Create;
begin
  inherited Create;
end;


destructor TClkIniFile.Destroy;
begin
  inherited Destroy;
end;


procedure TClkIniFile.GetFileContent(AContent: TStringList);
var
  i, j: Integer;
begin
  for i := 0 to Length(FSectionContents) - 1 do
  begin
    AContent.Add('[' + FSections.Strings[i] + ']');

    for j := 0 to FSectionContents[i].Count - 1 do
      AContent.Add(FSectionContents[i].Strings[j]);

    AContent.Add('');
  end;
end;


procedure TClkIniFile.GetFileContent(AContent: TMemoryStream);
var
  FileContent: TStringList;
begin
  FileContent := TStringList.Create;
  try
    GetFileContent(FileContent);
    FileContent.SaveToStream(AContent);
  finally
    FileContent.Free;
  end;
end;


procedure TClkIniFile.UpdateFile;
var
  FileContent: TStringList;
begin
  if FClkIniFileOpenMode = omFileName then
  begin
    FileContent := TStringList.Create;
    try
      GetFileContent(FileContent);
      FileContent.SaveToFile(FFileName);
    finally
      FileContent.Free;
    end;
  end;
end;


procedure TClkIniFile.CreateSection(ANewSectionName: string);
begin
  FSections.Add(ANewSectionName);
  SetLength(FSectionContents, Length(FSectionContents) + 1);
  FSectionContents[Length(FSectionContents) - 1] := TStringList.Create;
end;


procedure TClkIniFile.WriteString(const Section, Ident, Value: string);
var
  SectionIndex, LineIndex: Integer;
  OldValue: string;
begin
  SectionIndex := GetSectionIndex(Section);
  if SectionIndex = -1 then
  begin
    FSections.Add(Section);
    SetLength(FSectionContents, Length(FSectionContents) + 1);

    FSectionContents[Length(FSectionContents) - 1] := TStringList.Create;
    SectionIndex := FSections.Count - 1;
  end;

  LineIndex := GetIndentPairValue(FSectionContents[SectionIndex], Ident, OldValue);
  if LineIndex > -1 then
    FSectionContents[SectionIndex].Strings[LineIndex] := Ident + '=' + Value
  else
    FSectionContents[SectionIndex].Add(Ident + '=' + Value);
end;


procedure TClkIniFile.WriteInteger(const Section, Ident: string; Value: Longint);
begin
  WriteString(Section, Ident, IntToStr(Value));
end;


procedure TClkIniFile.WriteBool(const Section, Ident: string; Value: Boolean);
begin
  WriteString(Section, Ident, IntToStr(Ord(Value)));
end;


procedure TClkIniFile.WriteString(SectionIndex: Integer; const Ident, Value: string; ANewSectionName: string = '');
var
  LineIndex: Integer;
  OldValue: string;
begin
  if (SectionIndex < 0) or (SectionIndex > Length(FSectionContents) - 1) then
  begin
    if ANewSectionName = '' then
      raise Exception.Create('Section name cannot be empty when writing a value.');

    CreateSection(ANewSectionName);
    SectionIndex := Length(FSectionContents) - 1;
  end;

  LineIndex := GetIndentPairValue(FSectionContents[SectionIndex], Ident, OldValue);
  if LineIndex > -1 then
    FSectionContents[SectionIndex].Strings[LineIndex] := Ident + '=' + Value
  else
    FSectionContents[SectionIndex].Add(Ident + '=' + Value);
end;


procedure TClkIniFile.WriteInteger(SectionIndex: Integer; const Ident: string; Value: Longint; ANewSectionName: string = '');
begin
  WriteString(SectionIndex, Ident, IntToStr(Value), ANewSectionName);
end;


procedure TClkIniFile.WriteBool(SectionIndex: Integer; const Ident: string; Value: Boolean; ANewSectionName: string = '');
begin
  WriteString(SectionIndex, Ident, IntToStr(Ord(Value)), ANewSectionName);
end;


procedure TClkIniFile.WriteSection(SectionIndex: Integer; Src: TStringList; ANewSectionName: string = ''; AWriteSectionMode: TClkIniFileWriteSectionMode = smReplaceSection);
begin
  if (SectionIndex < 0) or (SectionIndex > Length(FSectionContents) - 1) then
  begin
    if ANewSectionName = '' then
      raise Exception.Create('Section name cannot be empty when creating a new section.');

    CreateSection(ANewSectionName);
    SectionIndex := Length(FSectionContents) - 1;
  end;

  if AWriteSectionMode = smReplaceSection then
    FSectionContents[SectionIndex].Clear;

  FSectionContents[SectionIndex].AddStrings(Src);
end;


end.
