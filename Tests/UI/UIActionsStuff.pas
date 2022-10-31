{
    Copyright (C) 2022 VCC
    creation date: Oct 2022
    initial release date: 30 Oct 2022

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


unit UIActionsStuff;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ClickerUtils, InMemFileSystem;


procedure GenerateExecAppOptionsForTestDriver(var AExecAppOptions: TClkExecAppOptions; AExeArgs: string);
procedure LoadTemplateFromDiskIntoInMemFS(ATemplateName: string; AInMemFS: TInMemFileSystem);


implementation


procedure GenerateExecAppOptionsForTestDriver(var AExecAppOptions: TClkExecAppOptions; AExeArgs: string);
begin
  AExecAppOptions.PathToApp := ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\UIClicker.exe';
  AExecAppOptions.ListOfParams := AExeArgs;
  AExecAppOptions.CurrentDir := ExtractFileDir(AExecAppOptions.PathToApp);
  AExecAppOptions.UseInheritHandles := uihNo;
  AExecAppOptions.WaitForApp := False;
  AExecAppOptions.NoConsole := True;
end;


procedure LoadTemplateFromDiskIntoInMemFS(ATemplateName: string; AInMemFS: TInMemFileSystem);
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    MemStream.LoadFromFile(ATemplateName);
    AInMemFS.SaveFileToMem(ATemplateName, MemStream.Memory, MemStream.Size);
  finally
    MemStream.Free;
  end;
end;

end.

