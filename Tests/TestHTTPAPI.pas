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
  LCLIntf, Classes, SysUtils, fpcunit, testregistry, InMemFileSystem;

type

  TTestHTTPAPI = class(TTestCase)
  private
    FInMemFS: TInMemFileSystem;
    function SendTestFileToServer(ARemoteAddress, AFileName: string; AFileContent: string = 'Dummy'): string;
    function SendTemplateToServer(ARemoteAddress, AFileName: string; AFileContent: TMemoryStream): string;
    procedure CreateTestTemplateInMem;
    function Send_ExecuteCommandAtIndex_ToServer(AActionIdx, AStackLevel: Integer): string;
    procedure SetupTargetWindowFor_FindSubControl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_TestConnection;
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

    procedure Test_SetVariable_HappyFlow;
    procedure Test_SetVariable_BadStackLevel;

    procedure Test_ExecuteClickAction_LeaveMouse;
    procedure Test_ExecuteExecAppAction_IPConfig;
    procedure Test_ExecuteExecAppAction_IPConfig_NoInheritHandles;

    procedure Test_ExecuteFindControlAction_UIClickerMain;
    procedure Test_ExecuteFindControlAction_UIClickerMain_WrongClass;
    procedure Test_ExecuteFindControlAction_UIClickerMain_WrongClassAllowToFail;

    procedure Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabel;
  end;


implementation

uses
  ClickerActionsClient, ClickerUtils, ActionsStuff, Controls;


const
  CTestServerAddress = 'http://127.0.0.1:5444/';
  CTestTemplateFileName = 'TestTemplate.clktmpl';
  CEmptyTestTemplateFileName = 'EmptyTestTemplate.clktmpl';
  CExpectedBadStackLevelResponse: string = '[Server error] Stack level out of bounds: 10. This happens when there is no template loaded in a new tab (with the requested stack level), as a result of "call template" action. It is also possible that the template is loaded, then exited before being executed.';


procedure TTestHTTPAPI.SetUp;
begin
  FInMemFS := TInMemFileSystem.Create;
end;


procedure TTestHTTPAPI.TearDown;
begin
  FreeAndNil(FInMemFS);
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


function TTestHTTPAPI.SendTemplateToServer(ARemoteAddress, AFileName: string; AFileContent: TMemoryStream): string;
var
  Link: string;
begin
  Link := ARemoteAddress + CRECmd_SendFileToServer + '?' +
                           CREParam_FileName + '=' + AFileName;

  Result := SendFileToServer(Link, AFileContent);

  AssertEquals('Received file: "' + AFileName + '"  of ' + IntToStr(AFileContent.Size) + ' bytes in size.', Result);
  AssertEquals(True, GetFileExistenceOnServer(CTestServerAddress, AFileName, False));
end;


procedure TTestHTTPAPI.CreateTestTemplateInMem;
var
  ClickOptions: TClkClickOptions;
begin
  GetDefaultClickOptions(ClickOptions);
  ClickOptions.XClickPointReference := xrefVar;
  ClickOptions.YClickPointReference := yrefVar;
  ClickOptions.XClickPointVar := '$ObjLeft$';
  ClickOptions.YClickPointVar := '$ObjTop$';
  ClickOptions.MoveWithoutClick := True;

  AddClickActionToTemplate(CTestTemplateFileName, '1', 0, True, '', ClickOptions, FInMemFS);
end;


function TTestHTTPAPI.Send_ExecuteCommandAtIndex_ToServer(AActionIdx, AStackLevel: Integer): string;
var
  Link: string;
begin
  Link := CTestServerAddress + CRECmd_ExecuteCommandAtIndex + '?' +
                               CREParam_ActionIdx + '=' + IntToStr(AActionIdx) + '&' +
                               CREParam_StackLevel + '=' + IntToStr(AStackLevel) + '&' +
                               CREParam_IsDebugging + '=' + IntToStr(0) + '&' +
                               CREParam_FileLocation + '= ' + CREParam_FileLocation_ValueMem;

  Result := SendTextRequestToServer(Link);
end;


procedure TTestHTTPAPI.SetupTargetWindowFor_FindSubControl;
var
  WindowOperationsOptions: TClkWindowOperationsOptions;
  FindControlOptions: TClkFindControlOptions;
  UIClickerMainHandle: THandle;
  Response: string;
  ListOfVars: TStringList;
begin
  GenerateFindControlOptionsForMainUIClickerWindow(FindControlOptions, False);
  Response := FastReplace_87ToReturn(ExecuteFindControlAction(CTestServerAddress, FindControlOptions, 'Setup UIClicker Main', 1000, CREParam_FileLocation_ValueMem));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    AssertEquals(True, ListOfVars.Values['$LastAction_Status$'] <> CActionStatusStr[asFailed]);
    UIClickerMainHandle := StrToIntDef(ListOfVars.Values['$Control_Handle$'], 0);
  finally
    ListOfVars.Free;
  end;

  //Can't use ExecuteWindowOperationsAction(woBringToFront), because the target window belongs to the same process (UIClicker). See: https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-setforegroundwindow
  //GenerateWindowOperationsOptionsForFindControlSetup(WindowOperationsOptions, woBringToFront);
  //ExecuteWindowOperationsAction(CTestServerAddress, WindowOperationsOptions);
  SetForegroundWindow(UIClickerMainHandle);

  GenerateWindowOperationsOptionsForFindControlSetup(WindowOperationsOptions, woMoveResize);
  ExecuteWindowOperationsAction(CTestServerAddress, WindowOperationsOptions);
end;


procedure TTestHTTPAPI.Test_TestConnection;
var
  Response: string;
begin
  Response := TestConnection(CTestServerAddress);
  AssertEquals(CREResp_ConnectionOK, Response);
end;


//ToDo: some refactoring, then use some test utils for assertions.
procedure TTestHTTPAPI.Test_ExecuteCommandAtIndex_HappyFlow;
var
  Content: TMemoryStream;
  Response: string;
  ListOfVars: TStringList;
begin
  CreateTestTemplateInMem;

  Content := TMemoryStream.Create;
  try
    FInMemFS.LoadFileFromMemToStream(CTestTemplateFileName, Content);
    SendTemplateToServer(CTestServerAddress, CTestTemplateFileName, Content);
  finally
    Content.Free;
  end;

  AssertEquals(CREResp_TemplateLoaded, SendLoadTemplateInExecListRequest(CTestServerAddress, CTestTemplateFileName, 0));

  Response := FastReplace_87ToReturn(Send_ExecuteCommandAtIndex_ToServer(0, 0));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    AssertEquals(True, ListOfVars.Values['$LastAction_Status$'] <> CActionStatusStr[asFailed]);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestHTTPAPI.Test_ExecuteCommandAtIndex_EmptyTemplate;
var
  Content: TMemoryStream;
  Response: string;
  ListOfVars: TStringList;
begin
  //SetVariable(CTestServerAddress, '$LastAction_Status$', '', 0);

  Content := TMemoryStream.Create;
  try
    SendTemplateToServer(CTestServerAddress, CEmptyTestTemplateFileName, Content);
  finally
    Content.Free;
  end;

  AssertEquals(CREResp_TemplateLoaded, SendLoadTemplateInExecListRequest(CTestServerAddress, CEmptyTestTemplateFileName, 0));

  Response := FastReplace_87ToReturn(Send_ExecuteCommandAtIndex_ToServer(0, 0));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    AssertEquals(CActionStatusStr[asFailed], ListOfVars.Values['$LastAction_Status$']);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestHTTPAPI.Test_ExecuteCommandAtIndex_BadActionIndex;
const
  CExpectedErr: string = 'Cannot select action, to load values at index 10.  It seems that no template is loaded.';
var
  Response: string;
  ListOfVars: TStringList;
begin
  Response := FastReplace_87ToReturn(Send_ExecuteCommandAtIndex_ToServer(10, 0));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    AssertEquals(CActionStatusStr[asFailed], ListOfVars.Values['$LastAction_Status$']);
    AssertEquals(CExpectedErr, ListOfVars.Values['$DbgCurrentAction$']);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestHTTPAPI.Test_ExecuteCommandAtIndex_BadStackIndex;
var
  Response: string;
begin
  Response := Send_ExecuteCommandAtIndex_ToServer(0, 10);
  AssertEquals(CExpectedBadStackLevelResponse, Response);
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
var
  Response: string;
begin
  Response := GetAllReplacementVars(CTestServerAddress, 10);
  AssertEquals(CExpectedBadStackLevelResponse, Response);
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
    ListOfFiles.Add('non-existent file' + CDefaultInMemFileNameHashSeparator + 'no hash');

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


procedure TTestHTTPAPI.Test_SetVariable_HappyFlow;
var
  Response: string;
begin
  Response := SetVariable(CTestServerAddress, '$MyVar$', 'some value', 0);
  AssertEquals(CREResp_Done, Response);
end;


procedure TTestHTTPAPI.Test_SetVariable_BadStackLevel;
const
  CBadValue = 'some bad value';
var
  Response: string;
  ListOfVars: TStringList;
begin
  Response := SetVariable(CTestServerAddress, '$MyVar$', CBadValue, 10);
  AssertEquals(CExpectedBadStackLevelResponse, Response);

  Response := FastReplace_87ToReturn(GetAllReplacementVars(CTestServerAddress, 0));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    AssertEquals(False, ListOfVars.Values['$MyVar$'] = CBadValue);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestHTTPAPI.Test_ExecuteClickAction_LeaveMouse;
const
  CX: Integer = 20;
  CY: Integer = 30;
var
  Response: string;
  ListOfVars: TStringList;
  ClickOptions: TClkClickOptions;
  tp: TPoint;
begin
  GenerateClickOptionsForLeaveMouse(CX, CY, ClickOptions);

  Response := FastReplace_87ToReturn(ExecuteClickAction(CTestServerAddress, ClickOptions));

  GetCursorPos(tp);
  AssertEquals(tp.X, CX);
  AssertEquals(tp.Y, CY);

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    AssertEquals('', ListOfVars.Values['$ExecAction_Err$']);
    AssertEquals('1', ListOfVars.Values[CREResp_RemoteExecResponseVar]);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestHTTPAPI.Test_ExecuteExecAppAction_IPConfig;
var
  Response: string;
  ListOfVars: TStringList;
  ExecAppOptions: TClkExecAppOptions;
begin
  GenerateExecAppOptionsForIPConfig(ExecAppOptions);
  ExecAppOptions.UseInheritHandles := uihYes;

  Response := FastReplace_87ToReturn(ExecuteExecAppAction(CTestServerAddress, ExecAppOptions, 'TestExec', 1000));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    AssertEquals('', ListOfVars.Values['$ExecAction_Err$']);
    AssertEquals('1', ListOfVars.Values[CREResp_RemoteExecResponseVar]);
    AssertEquals(True, Pos('Windows IP Configuration', ListOfVars.Values['$ExecAction_StdOut$']) > 0);
    AssertEquals(True, Pos('Host Name', ListOfVars.Values['$ExecAction_StdOut$']) > 0);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestHTTPAPI.Test_ExecuteExecAppAction_IPConfig_NoInheritHandles;
var
  Response: string;
  ListOfVars: TStringList;
  ExecAppOptions: TClkExecAppOptions;
begin
  GenerateExecAppOptionsForIPConfig(ExecAppOptions);
  Response := FastReplace_87ToReturn(ExecuteExecAppAction(CTestServerAddress, ExecAppOptions, 'TestExec', 1000));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    AssertEquals('', ListOfVars.Values['$ExecAction_Err$']);
    AssertEquals('1', ListOfVars.Values[CREResp_RemoteExecResponseVar]);
    AssertEquals('', ListOfVars.Values['$ExecAction_StdOut$']);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestHTTPAPI.Test_ExecuteFindControlAction_UIClickerMain;
var
  Response: string;
  ListOfVars: TStringList;
  FindControlOptions: TClkFindControlOptions;
begin
  GenerateFindControlOptionsForMainUIClickerWindow(FindControlOptions, False);
  Response := FastReplace_87ToReturn(ExecuteFindControlAction(CTestServerAddress, FindControlOptions, 'TestFind UIClicker Main', 1000, CREParam_FileLocation_ValueMem));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    AssertEquals('', ListOfVars.Values['$ExecAction_Err$']);
    AssertEquals('1', ListOfVars.Values[CREResp_RemoteExecResponseVar]);
    AssertEquals('Successful', ListOfVars.Values['$LastAction_Status$']);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestHTTPAPI.Test_ExecuteFindControlAction_UIClickerMain_WrongClass;
var
  Response: string;
  ListOfVars: TStringList;
  FindControlOptions: TClkFindControlOptions;
begin
  GenerateFindControlOptionsForMainUIClickerWindow(FindControlOptions, False);
  FindControlOptions.MatchClassName := 'non-existent name';
  Response := FastReplace_87ToReturn(ExecuteFindControlAction(CTestServerAddress, FindControlOptions, 'TestFind UIClicker Main', 1000, CREParam_FileLocation_ValueMem));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    AssertEquals(True, Pos('Timeout at "TestFind UIClicker Main" in ', ListOfVars.Values['$ExecAction_Err$']) > 0);
    AssertEquals('0', ListOfVars.Values[CREResp_RemoteExecResponseVar]);
    AssertEquals('Failed', ListOfVars.Values['$LastAction_Status$']);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestHTTPAPI.Test_ExecuteFindControlAction_UIClickerMain_WrongClassAllowToFail;
var
  Response: string;
  ListOfVars: TStringList;
  FindControlOptions: TClkFindControlOptions;
begin
  GenerateFindControlOptionsForMainUIClickerWindow(FindControlOptions, True);
  FindControlOptions.MatchClassName := 'non-existent name';
  Response := FastReplace_87ToReturn(ExecuteFindControlAction(CTestServerAddress, FindControlOptions, 'TestFind UIClicker Main', 1000, CREParam_FileLocation_ValueMem));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    AssertEquals('', ListOfVars.Values['$ExecAction_Err$']);
    AssertEquals('0', ListOfVars.Values[CREResp_RemoteExecResponseVar]);
    AssertEquals('Allowed Failed', ListOfVars.Values['$LastAction_Status$']);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabel;
var
  Response: string;
  ListOfVars: TStringList;
  FindSubControlOptions: TClkFindControlOptions;
begin
  SetupTargetWindowFor_FindSubControl;
  GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness(FindSubControlOptions, False);
  Response := FastReplace_87ToReturn(ExecuteFindSubControlAction(CTestServerAddress, FindSubControlOptions, 'Test Find Bitness on UIClicker Main', 3000, CREParam_FileLocation_ValueMem));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    AssertEquals('', ListOfVars.Values['$ExecAction_Err$']);
    AssertEquals('1', ListOfVars.Values[CREResp_RemoteExecResponseVar]);
    AssertEquals('Successful', ListOfVars.Values['$LastAction_Status$']);
  finally
    ListOfVars.Free;
  end;
end;


initialization

  RegisterTest(TTestHTTPAPI);
end.

