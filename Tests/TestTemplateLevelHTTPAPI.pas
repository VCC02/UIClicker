{
    Copyright (C) 2022 VCC
    creation date: Aug 2022
    initial release date: 25 Aug 2022

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


unit TestTemplateLevelHTTPAPI;

{$mode ObjFPC}{$H+}

interface

uses
  LCLIntf, Classes, SysUtils, TestHTTPAPI, fpcunit, testregistry, Expectations;

type

  TTestTemplateLevelHTTPAPI = class(TTestHTTPAPI)
  public
    constructor Create; override;
  published
    procedure BeforeAll_AlwaysExecute;

    procedure Test_ExecuteCommandAtIndex_HappyFlow;
    procedure Test_ExecuteCommandAtIndex_EmptyTemplate;
    procedure Test_ExecuteCommandAtIndex_BadActionIndex;
    procedure Test_ExecuteCommandAtIndex_BadStackIndex;

    procedure Test_GetAllReplacementVars_HappyFlow;
    procedure Test_GetAllReplacementVars_BadStackLevel;

    procedure Test_SendFileToServer;

    procedure Test_GetFileExistenceOnServer_OneFileNoHash;
    procedure Test_GetFileExistenceOnServer_OneFileWithHash;
    procedure Test_GetFileExistenceOnServer_MultipleFilesNoHash;
    procedure Test_GetFileExistenceOnServer_MultipleFilesWithHash;

    procedure Test_ClearInMemFileSystem;
    procedure Test_GetListOfWaitingFiles_MissingTemplate;

    procedure AfterAll_AlwaysExecute;
  end;


implementation


uses
  ClickerActionsClient, ClickerUtils, Controls, InMemFileSystem, ClickerFileProviderClient,
  AsyncProcess, UITestUtils;


var
  TestUIClicker_Proc: TAsyncProcess;


constructor TTestTemplateLevelHTTPAPI.Create;
begin
  inherited Create;
  TestServerAddress := CTestServerAddress;
end;


procedure TTestTemplateLevelHTTPAPI.BeforeAll_AlwaysExecute;
var
  PathToTestUIClicker: string;
  Response: string;
  CallTemplateOptions: TClkCallTemplateOptions;
begin
  PathToTestUIClicker := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\UIClicker.exe');
  TestUIClicker_Proc := CreateUIClickerProcess(PathToTestUIClicker, '--SetExecMode Server --ServerPort 5444');
end;


procedure TTestTemplateLevelHTTPAPI.AfterAll_AlwaysExecute;
begin
  if TestUIClicker_Proc <> nil then
  begin
    TestUIClicker_Proc.Terminate(0);
    TestUIClicker_Proc.Free;
  end;
end;


procedure TTestTemplateLevelHTTPAPI.Test_ExecuteCommandAtIndex_HappyFlow;
var
  Response: string;
  ListOfVars: TStringList;
begin
  CreateTestTemplateInMem;
  SendTemplateFromInMemToServerThenLoad(CTestTemplateFileName);

  Response := FastReplace_87ToReturn(Send_ExecuteCommandAtIndex_ToServer(0, 0));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    Expect(ListOfVars).WithItem('$LastAction_Status$').DifferentThanValue(CActionStatusStr[asFailed]);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestTemplateLevelHTTPAPI.Test_ExecuteCommandAtIndex_EmptyTemplate;
var
  Response: string;
  ListOfVars: TStringList;
begin
  //SetVariable(CTestServerAddress, '$LastAction_Status$', '', 0);
  SendEmptyTemplateToServerThenLoad;
  Response := FastReplace_87ToReturn(Send_ExecuteCommandAtIndex_ToServer(0, 0));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    Expect(ListOfVars).WithItem('$LastAction_Status$').OfValue(CActionStatusStr[asFailed]);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestTemplateLevelHTTPAPI.Test_ExecuteCommandAtIndex_BadActionIndex;
const
  CExpectedErr: string = 'Cannot select action, to load values at index 10.  It seems that no template is loaded.';
var
  Response: string;
  ListOfVars: TStringList;
begin
  SendEmptyTemplateToServerThenLoad;
  Response := FastReplace_87ToReturn(Send_ExecuteCommandAtIndex_ToServer(10, 0));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    Expect(ListOfVars).WithItem('$LastAction_Status$').OfValue(CActionStatusStr[asFailed]);
    Expect(ListOfVars).WithItem('$DbgCurrentAction$').OfValue(CExpectedErr);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestTemplateLevelHTTPAPI.Test_ExecuteCommandAtIndex_BadStackIndex;
begin
  Expect(Send_ExecuteCommandAtIndex_ToServer(0, 10)).ToBe(CExpectedBadStackLevelResponse);
end;


procedure TTestTemplateLevelHTTPAPI.Test_GetAllReplacementVars_HappyFlow;
var
  Response: string;
  ListOfVars: TStringList;
begin
  Response := FastReplace_87ToReturn(GetAllReplacementVars(TestServerAddress, 0));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    Expect(ListOfVars).WithItem('$Control_Handle$');
    Expect(ListOfVars).WithItem('$OSVer$');
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestTemplateLevelHTTPAPI.Test_GetAllReplacementVars_BadStackLevel;
begin
  Expect(GetAllReplacementVars(TestServerAddress, 10)).ToBe(CExpectedBadStackLevelResponse);
end;


procedure TTestTemplateLevelHTTPAPI.Test_SendFileToServer;
const
  CFileName: string = 'Test_SendFileToServer.txt';
  CExpectedHash = 'BCF036B6F33E182D4705F4F5B1AF13AC';
begin
  Expect(ClearInMemFileSystem(TestServerAddress)).ToBe(CREResp_Done);
  SendTestFileToServer(TestServerAddress, CFileName);

  Expect(GetFileExistenceOnServer(TestServerAddress, CFileName + CDefaultInMemFileNameHashSeparator + CExpectedHash, True)).ToBe(True);
end;


procedure TTestTemplateLevelHTTPAPI.Test_GetFileExistenceOnServer_OneFileNoHash;
const
  CFileName: string = 'Test_GetFileExistenceOnServer_OneFileNoHash.txt';
begin
  Expect(ClearInMemFileSystem(TestServerAddress)).ToBe(CREResp_Done);
  SendTestFileToServer(TestServerAddress, CFileName, 'SomeContentForNoHash');

  Expect(GetFileExistenceOnServer(TestServerAddress, CFileName, False)).ToBe(True);
end;


procedure TTestTemplateLevelHTTPAPI.Test_GetFileExistenceOnServer_OneFileWithHash;
const
  CFileName: string = 'Test_GetFileExistenceOnServer_OneFileWithHash.txt';
  CExpectedHash = 'E51AAA0C74A8390EE612C2451F40CAB9';
begin
  Expect(ClearInMemFileSystem(TestServerAddress)).ToBe(CREResp_Done);
  SendTestFileToServer(TestServerAddress, CFileName, 'SomeContentForComputingHash');

  Expect(GetFileExistenceOnServer(TestServerAddress, CFileName + CDefaultInMemFileNameHashSeparator + CExpectedHash, True)).ToBe(True);
end;


procedure TTestTemplateLevelHTTPAPI.Test_GetFileExistenceOnServer_MultipleFilesNoHash;
const
  CFileName1 = 'Test_GetFileExistenceOnServer_OneFileNoHash_1.txt';
  CFileName2 = 'Test_GetFileExistenceOnServer_OneFileNoHash_2.txt';
  CFileName3 = 'Test_GetFileExistenceOnServer_OneFileNoHash_3.txt';
  CFileNames: array[0..2] of string = (CFileName1, CFileName2, CFileName3);
  CExpectedExistence: array[0..3] of string = ('1', '1', '1', '0');
var
  ListOfFiles, ListOfResults: TStringList;
  Response: string;
begin
  Expect(ClearInMemFileSystem(TestServerAddress)).ToBe(CREResp_Done);
  SendMultipleTestFilesToServer(@CFileNames, Length(CFileNames), 'SomeContentForNoHash');

  ListOfFiles := TStringList.Create;
  ListOfResults := TStringList.Create;
  try
    ListOfFiles.Add(CFileName1);
    ListOfFiles.Add(CFileName2);
    ListOfFiles.Add(CFileName3);
    ListOfFiles.Add('non-existent file');

    Response := GetFileExistenceOnServer(TestServerAddress, ListOfFiles, ListOfResults, False);

    Expect(Response).ToBe('');
    Expect(ListOfResults).ToMatchContentOfStringArray(@CExpectedExistence, 4, 'File existence on server.');
  finally
    ListOfFiles.Free;
    ListOfResults.Free;
  end;
end;


procedure TTestTemplateLevelHTTPAPI.Test_GetFileExistenceOnServer_MultipleFilesWithHash;
const
  CFileName1 = 'Test_GetFileExistenceOnServer_OneFileWithHash_1.txt';
  CFileName2 = 'Test_GetFileExistenceOnServer_OneFileWithHash_2.txt';
  CFileName3 = 'Test_GetFileExistenceOnServer_OneFileWithHash_3.txt';
  CFileNames: array[0..2] of string = (CFileName1, CFileName2, CFileName3);
  CExpectedExistence: array[0..3] of string = ('1', '0', '1', '0');
var
  ListOfFiles, ListOfResults: TStringList;
  Response: string;
begin
  Expect(ClearInMemFileSystem(TestServerAddress)).ToBe(CREResp_Done);
  SendMultipleTestFilesToServer(@CFileNames, Length(CFileNames), 'SomeContentForWithHash');

  ListOfFiles := TStringList.Create;
  ListOfResults := TStringList.Create;
  try
    ListOfFiles.Add(CFileName1 + CDefaultInMemFileNameHashSeparator + '01A883F23CCAFE1CB87AE35BABA90B91');
    ListOfFiles.Add(CFileName2 + CDefaultInMemFileNameHashSeparator + 'bad hash');  //60AFF80200964FA6EFE893DE0F4E2352
    ListOfFiles.Add(CFileName3 + CDefaultInMemFileNameHashSeparator + 'B924828E25CC6B02FC1D84D8129E599F');
    ListOfFiles.Add('non-existent file' + CDefaultInMemFileNameHashSeparator + 'no hash');

    Response := GetFileExistenceOnServer(TestServerAddress, ListOfFiles, ListOfResults, True);

    Expect(Response).ToBe('');
    Expect(ListOfResults).ToMatchContentOfStringArray(@CExpectedExistence, 4, 'File existence on server.');
  finally
    ListOfFiles.Free;
    ListOfResults.Free;
  end;
end;


procedure TTestTemplateLevelHTTPAPI.Test_ClearInMemFileSystem;
const
  CFileName: string = 'Test_ClearInMemFileSystem.txt';
var
  Response: string;
begin
  SendTestFileToServer(TestServerAddress, CFileName);

  Response := ClearInMemFileSystem(TestServerAddress);
  Expect(Response).ToBe(CREResp_Done);
  Expect(GetFileExistenceOnServer(TestServerAddress, CFileName, False)).ToBe(False);
end;


procedure TTestTemplateLevelHTTPAPI.Test_GetListOfWaitingFiles_MissingTemplate;
const
  CFileName: string = 'CallMissingTemplate.clktmpl';
  CDirName: string = 'MemDir';
var
  Response: string;
  ListOfVars: TStringList;
  MissingTemplateName: string;
  FileProvider: TPollForMissingServerFiles;
begin
  Expect(ClearInMemFileSystem(TestServerAddress)).ToBe(CREResp_Done);

  Exit; /////////////////////////////////////////////////////////////////////////////
  /////  the test is disabled for now, because the file provider from this test, is not closed properly, so another test will fail

  MissingTemplateName := CDirName + '\MissingTemplate.clktmpl';
  CreateCallableTestTemplateInMem(MissingTemplateName, '$VarFromCalledTemplate$', 'DefaultValue');
  CreateCallableTestTemplateInMem_WithCallTemplate(CFileName, '$DummyVar$', '$DummyValue$', MissingTemplateName, '', False);

  SendTemplateFromInMemToServerThenLoad(CFileName);
  Expect(GetFileExistenceOnServer(TestServerAddress, CFileName, False)).ToBe(True);

  FileProvider := CreateFileProvider(CDirName, '.clktmpl'#13#10'.bmp', @HandleOnFileExists_Mem, @HandleOnLoadMissingFileContent_Mem);
  try
    Response := FastReplace_87ToReturn(Send_ExecuteCommandAtIndex_ToServer(2, 0));

    ListOfVars := TStringList.Create;
    try
      ListOfVars.Text := Response;
      Expect(ListOfVars).WithItem('$LastAction_Status$').DifferentThanValue(CActionStatusStr[asFailed]);
    finally
      ListOfVars.Free;
    end;
  finally
    DestroyFileProvider(FileProvider);
  end;
end;


initialization

  RegisterTest(TTestTemplateLevelHTTPAPI);
  TestUIClicker_Proc := nil;
end.

