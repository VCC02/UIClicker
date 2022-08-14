{
    Copyright (C) 2022 VCC
    creation date: Aug 2022
    initial release date: 14 Aug 2022

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


unit TestHTTPAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TTestHTTPAPI = class(TTestCase)
  private
    function SendTestFileToServer(ARemoteAddress, AFileName: string; AFileContent: string = 'Dummy'): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_TestConnection;
    procedure Test_GetAllReplacementVars_HappyFlow;
    procedure Test_GetAllReplacementVars_BadStackLevel;
    procedure Test_SendFileToServer;
    procedure Test_GetFileExistenceOnServer_OneFileNoHash;
    procedure Test_GetFileExistenceOnServer_OneFileWithHash;
    procedure Test_GetFileExistenceOnServer_MultipleFilesNoHash;
    procedure Test_GetFileExistenceOnServer_MultipleFilesWithHash;
    procedure Test_ClearInMemFileSystem;
  end;


implementation

uses
  ClickerActionsClient, ClickerUtils, InMemFileSystem;


const
  CTestServerAddress = 'http://127.0.0.1:5444/';


procedure TTestHTTPAPI.SetUp;
begin

end;


procedure TTestHTTPAPI.TearDown;
begin

end;


function TTestHTTPAPI.SendTestFileToServer(ARemoteAddress, AFileName: string; AFileContent: string = 'Dummy'): string;
var
  Content: TMemoryStream;
  Link: string;
begin
  Link := ARemoteAddress + CRECmd_SendFileToServer + '?' +
                           CREParam_FileName + '=' + AFileName;

  Content := TMemoryStream.Create;
  try
    Content.Write(AFileContent[1], Length(AFileContent));
    Result := SendFileToServer(Link, Content);
  finally
    Content.Free;
  end;

  AssertEquals('Received file: "' + AFileName + '"  of ' + IntToStr(Length(AFileContent)) + ' bytes in size.', Result);
  AssertEquals(True, GetFileExistenceOnServer(CTestServerAddress, AFileName, False));
end;


procedure TTestHTTPAPI.Test_TestConnection;
var
  Response: string;
begin
  Response := TestConnection(CTestServerAddress);
  AssertEquals(CREResp_ConnectionOK, Response);
end;


procedure TTestHTTPAPI.Test_GetAllReplacementVars_HappyFlow;
var
  Response: string;
  ListOfVars: TStringList;
begin
  Response := FastReplace_87ToReturn(GetAllReplacementVars(CTestServerAddress, 0));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    AssertEquals(True, ListOfVars.IndexOfName('$Control_Handle$') > -1);
    AssertEquals(True, ListOfVars.IndexOfName('$OSVer$') > -1);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestHTTPAPI.Test_GetAllReplacementVars_BadStackLevel;
const
  CExpectedResponse: string = '[Server error] Stack level out of bounds: 10. This happens when there is no template loaded in a new tab (with the requested stack level), as a result of "call template" action. It is also possible that the template is loaded, then exited before being executed.';
var
  Response: string;
begin
  Response := GetAllReplacementVars(CTestServerAddress, 10);
  AssertEquals(CExpectedResponse, Response);
end;


procedure TTestHTTPAPI.Test_SendFileToServer;
const
  CFileName: string = 'Test_SendFileToServer.txt';
  CExpectedHash = 'BCF036B6F33E182D4705F4F5B1AF13AC';
begin
  AssertEquals(CREResp_Done, ClearInMemFileSystem(CTestServerAddress));
  SendTestFileToServer(CTestServerAddress, CFileName);

  AssertEquals(True, GetFileExistenceOnServer(CTestServerAddress, CFileName + CDefaultInMemFileNameHashSeparator + CExpectedHash, True));
end;


procedure TTestHTTPAPI.Test_GetFileExistenceOnServer_OneFileNoHash;
const
  CFileName: string = 'Test_GetFileExistenceOnServer_OneFileNoHash.txt';
begin
  AssertEquals(CREResp_Done, ClearInMemFileSystem(CTestServerAddress));
  SendTestFileToServer(CTestServerAddress, CFileName, 'SomeContentForNoHash');

  AssertEquals(True, GetFileExistenceOnServer(CTestServerAddress, CFileName, False));
end;


procedure TTestHTTPAPI.Test_GetFileExistenceOnServer_OneFileWithHash;
const
  CFileName: string = 'Test_GetFileExistenceOnServer_OneFileWithHash.txt';
  CExpectedHash = 'E51AAA0C74A8390EE612C2451F40CAB9';
begin
  AssertEquals(CREResp_Done, ClearInMemFileSystem(CTestServerAddress));
  SendTestFileToServer(CTestServerAddress, CFileName, 'SomeContentForComputingHash');

  AssertEquals(True, GetFileExistenceOnServer(CTestServerAddress, CFileName + CDefaultInMemFileNameHashSeparator + CExpectedHash, True));
end;


procedure TTestHTTPAPI.Test_GetFileExistenceOnServer_MultipleFilesNoHash;
const
  CFileName1: string = 'Test_GetFileExistenceOnServer_OneFileNoHash_1.txt';
  CFileName2: string = 'Test_GetFileExistenceOnServer_OneFileNoHash_2.txt';
  CFileName3: string = 'Test_GetFileExistenceOnServer_OneFileNoHash_3.txt';
var
  ListOfFiles, ListOfResults: TStringList;
begin
  AssertEquals(CREResp_Done, ClearInMemFileSystem(CTestServerAddress));
  SendTestFileToServer(CTestServerAddress, CFileName1, 'SomeContentForNoHash1');
  SendTestFileToServer(CTestServerAddress, CFileName2, 'SomeContentForNoHash2');
  SendTestFileToServer(CTestServerAddress, CFileName3, 'SomeContentForNoHash3');

  ListOfFiles := TStringList.Create;
  ListOfResults := TStringList.Create;
  try
    ListOfFiles.Add(CFileName1);
    ListOfFiles.Add(CFileName2);
    ListOfFiles.Add(CFileName3);
    ListOfFiles.Add('non-existent file');

    AssertEquals('', GetFileExistenceOnServer(CTestServerAddress, ListOfFiles, ListOfResults, False));
    AssertEquals('1', ListOfResults.Strings[0]);
    AssertEquals('1', ListOfResults.Strings[1]);
    AssertEquals('1', ListOfResults.Strings[2]);
    AssertEquals('0', ListOfResults.Strings[3]);
  finally
    ListOfFiles.Free;
    ListOfResults.Free;
  end;
end;


procedure TTestHTTPAPI.Test_GetFileExistenceOnServer_MultipleFilesWithHash;
const
  CFileName1: string = 'Test_GetFileExistenceOnServer_OneFileWithHash_1.txt';
  CFileName2: string = 'Test_GetFileExistenceOnServer_OneFileWithHash_2.txt';
  CFileName3: string = 'Test_GetFileExistenceOnServer_OneFileWithHash_3.txt';
var
  ListOfFiles, ListOfResults: TStringList;
begin
  AssertEquals(CREResp_Done, ClearInMemFileSystem(CTestServerAddress));
  SendTestFileToServer(CTestServerAddress, CFileName1, 'SomeContentForWithHash1');
  SendTestFileToServer(CTestServerAddress, CFileName2, 'SomeContentForWithHash2');
  SendTestFileToServer(CTestServerAddress, CFileName3, 'SomeContentForWithHash3');

  ListOfFiles := TStringList.Create;
  ListOfResults := TStringList.Create;
  try
    ListOfFiles.Add(CFileName1 + CDefaultInMemFileNameHashSeparator + '01A883F23CCAFE1CB87AE35BABA90B91');
    ListOfFiles.Add(CFileName2 + CDefaultInMemFileNameHashSeparator + 'bad hash');  //60AFF80200964FA6EFE893DE0F4E2352
    ListOfFiles.Add(CFileName3 + CDefaultInMemFileNameHashSeparator + 'B924828E25CC6B02FC1D84D8129E599F');
    ListOfFiles.Add('non-existent file');

    AssertEquals('', GetFileExistenceOnServer(CTestServerAddress, ListOfFiles, ListOfResults, True));
    AssertEquals('1', ListOfResults.Strings[0]);
    AssertEquals('0', ListOfResults.Strings[1]);
    AssertEquals('1', ListOfResults.Strings[2]);
    AssertEquals('0', ListOfResults.Strings[3]);
  finally
    ListOfFiles.Free;
    ListOfResults.Free;
  end;
end;


procedure TTestHTTPAPI.Test_ClearInMemFileSystem;
const
  CFileName: string = 'Test_ClearInMemFileSystem.txt';
var
  Response: string;
begin
  SendTestFileToServer(CTestServerAddress, CFileName);

  Response := ClearInMemFileSystem(CTestServerAddress);
  AssertEquals(CREResp_Done, Response);
  AssertEquals(False, GetFileExistenceOnServer(CTestServerAddress, CFileName, False));
end;





initialization

  RegisterTest(TTestHTTPAPI);
end.

