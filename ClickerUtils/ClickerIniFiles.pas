{
    Copyright (C) 2022 VCC
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
    FSections, FFileContent: TStringList;    //ToDo  see if TStringHash has better speed results
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
begin
  FFileContent := TStringList.Create;
  try
    FFileContent.LoadFromFile(FileName);
    LoadSections(FFileContent);
  finally
    FFileContent.Free;
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


function GetIndentPairValue(ASection: TStringList; Ident: string; out Value: string): Boolean;   //returns True if found
var
  i, PosEqual: Integer;
  KeyValue, Key: string;
begin
  Result := False;
  for i := 0 to ASection.Count - 1 do
  begin
    KeyValue := ASection.Strings[i];
    PosEqual := Pos('=', KeyValue);

    Key := Copy(KeyValue, 1, PosEqual - 1);
    if Key = Ident then
    begin
      Value := Copy(KeyValue, PosEqual + 1, MaxInt);
      Result := True;
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

  if GetIndentPairValue(FSectionContents[SectionIndex], Ident, Value) then
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
  
  if GetIndentPairValue(FSectionContents[SectionIndex], Ident, Value) then
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

end.
