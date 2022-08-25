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
  LCLIntf, Classes, SysUtils, fpcunit, testregistry, InMemFileSystem, Expectations;

type

  TTestHTTPAPI = class(TTestCase)
  private
    FInMemFS: TInMemFileSystem;
    function SendTestFileToServer(ARemoteAddress, AFileName: string; AFileContent: string = 'Dummy'): string;
    function SendTemplateToServer(ARemoteAddress, AFileName: string; AFileContent: TMemoryStream): string;
    procedure SendEmptyTemplateToServerThenLoad;
    procedure SendTemplateFromInMemToServerThenLoad(AFileName: string);
    procedure SendMultipleTestFilesToServer(AFileNames: PStrArr; AFileNamesLength: Integer; ABaseContent: string);
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
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_WindowInterpreterButton_Disk;
  end;


implementation

uses
  ClickerActionsClient, ClickerUtils, ActionsStuff, Controls;


const
  CTestServerAddress = 'http://127.0.0.1:5444/';
  CTestTemplateFileName = 'TestTemplate.clktmpl';
  CEmptyTestTemplateFileName = 'EmptyTestTemplate.clktmpl';
  CExpectedBadStackLevelResponse: string = '[Server error] Stack level out of bounds: 10. This happens when there is no template loaded in a new tab (with the requested stack level), as a result of "call template" action. It is also possible that the template is loaded, then exited before being executed.';


procedure ExpectSuccessfulAction(AListOfVars: TStringList);
begin
  Expect(AListOfVars).WithItem('$ExecAction_Err$', 'list of vars').OfValue('', 'No error Allowed.');
  Expect(AListOfVars).WithItem(CREResp_RemoteExecResponseVar, 'list of vars').OfValue('1', 'Expected successful action.');
  Expect(AListOfVars).WithItem('$LastAction_Status$').OfValue('Successful');
end;


procedure ExpectFailedAction(AListOfVars: TStringList; APartOfErrorMessage: string);
begin
  Expect(AListOfVars).WithItem('$ExecAction_Err$', 'list of vars').ToContain(APartOfErrorMessage, 'An error is expected.');
  Expect(AListOfVars).WithItem(CREResp_RemoteExecResponseVar, 'list of vars').OfValue('0', 'Expected failed action.');
  Expect(AListOfVars).WithItem('$LastAction_Status$').OfValue('Failed');
end;


procedure ExpectAllowedFailedAction(AListOfVars: TStringList);
begin
  Expect(AListOfVars).WithItem('$ExecAction_Err$', 'list of vars').OfValue('');
  Expect(AListOfVars).WithItem(CREResp_RemoteExecResponseVar, 'list of vars').OfValue('0', 'Expected allowed failed action.');
  Expect(AListOfVars).WithItem('$LastAction_Status$').OfValue('Allowed Failed');
end;


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

  Expect(Result).ToBe('Received file: "' + AFileName + '"  of ' + IntToStr(Length(AFileContent)) + ' bytes in size.');
  Expect(GetFileExistenceOnServer(CTestServerAddress, AFileName, False)).ToBe(True);
end;


function TTestHTTPAPI.SendTemplateToServer(ARemoteAddress, AFileName: string; AFileContent: TMemoryStream): string;
var
  Link: string;
begin
  Link := ARemoteAddress + CRECmd_SendFileToServer + '?' +
                           CREParam_FileName + '=' + AFileName;

  Result := SendFileToServer(Link, AFileContent);

  Expect(Result).ToBe('Received file: "' + AFileName + '"  of ' + IntToStr(AFileContent.Size) + ' bytes in size.');
  Expect(GetFileExistenceOnServer(CTestServerAddress, AFileName, False)).ToBe(True);
end;


procedure TTestHTTPAPI.SendEmptyTemplateToServerThenLoad;
var
  Content: TMemoryStream;
begin
  Content := TMemoryStream.Create;
  try
    SendTemplateToServer(CTestServerAddress, CEmptyTestTemplateFileName, Content);
  finally
    Content.Free;
  end;

  Expect(SendLoadTemplateInExecListRequest(CTestServerAddress, CEmptyTestTemplateFileName, 0)).ToBe(CREResp_TemplateLoaded);
end;


procedure TTestHTTPAPI.SendTemplateFromInMemToServerThenLoad(AFileName: string);
var
  Content: TMemoryStream;
begin
  Content := TMemoryStream.Create;
  try
    FInMemFS.LoadFileFromMemToStream(AFileName, Content);
    SendTemplateToServer(CTestServerAddress, AFileName, Content);
  finally
    Content.Free;
  end;

  Expect(SendLoadTemplateInExecListRequest(CTestServerAddress, AFileName, 0)).ToBe(CREResp_TemplateLoaded);
end;


procedure TTestHTTPAPI.SendMultipleTestFilesToServer(AFileNames: PStrArr; AFileNamesLength: Integer; ABaseContent: string);
var
  i: Integer;
begin
  for i := 0 to AFileNamesLength - 1 do
    SendTestFileToServer(CTestServerAddress, AFileNames^[i], ABaseContent + IntToStr(i + 1));  //i + 1, to match the already existing hashes
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
    Expect(ListOfVars).WithItem('$LastAction_Status$').DifferentThanValue(CActionStatusStr[asFailed]);
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


//====================================================================================


procedure TTestHTTPAPI.Test_TestConnection;
begin
  Expect(TestConnection(CTestServerAddress)).ToBe(CREResp_ConnectionOK);
end;


procedure TTestHTTPAPI.Test_ExecuteCommandAtIndex_HappyFlow;
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


procedure TTestHTTPAPI.Test_ExecuteCommandAtIndex_EmptyTemplate;
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


procedure TTestHTTPAPI.Test_ExecuteCommandAtIndex_BadActionIndex;
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


procedure TTestHTTPAPI.Test_ExecuteCommandAtIndex_BadStackIndex;
begin
  Expect(Send_ExecuteCommandAtIndex_ToServer(0, 10)).ToBe(CExpectedBadStackLevelResponse);
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
    Expect(ListOfVars).WithItem('$Control_Handle$');
    Expect(ListOfVars).WithItem('$OSVer$');
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestHTTPAPI.Test_GetAllReplacementVars_BadStackLevel;
begin
  Expect(GetAllReplacementVars(CTestServerAddress, 10)).ToBe(CExpectedBadStackLevelResponse);
end;


procedure TTestHTTPAPI.Test_SendFileToServer;
const
  CFileName: string = 'Test_SendFileToServer.txt';
  CExpectedHash = 'BCF036B6F33E182D4705F4F5B1AF13AC';
begin
  Expect(ClearInMemFileSystem(CTestServerAddress)).ToBe(CREResp_Done);
  SendTestFileToServer(CTestServerAddress, CFileName);

  Expect(GetFileExistenceOnServer(CTestServerAddress, CFileName + CDefaultInMemFileNameHashSeparator + CExpectedHash, True)).ToBe(True);
end;


procedure TTestHTTPAPI.Test_GetFileExistenceOnServer_OneFileNoHash;
const
  CFileName: string = 'Test_GetFileExistenceOnServer_OneFileNoHash.txt';
begin
  Expect(ClearInMemFileSystem(CTestServerAddress)).ToBe(CREResp_Done);
  SendTestFileToServer(CTestServerAddress, CFileName, 'SomeContentForNoHash');

  Expect(GetFileExistenceOnServer(CTestServerAddress, CFileName, False)).ToBe(True);
end;


procedure TTestHTTPAPI.Test_GetFileExistenceOnServer_OneFileWithHash;
const
  CFileName: string = 'Test_GetFileExistenceOnServer_OneFileWithHash.txt';
  CExpectedHash = 'E51AAA0C74A8390EE612C2451F40CAB9';
begin
  Expect(ClearInMemFileSystem(CTestServerAddress)).ToBe(CREResp_Done);
  SendTestFileToServer(CTestServerAddress, CFileName, 'SomeContentForComputingHash');

  Expect(GetFileExistenceOnServer(CTestServerAddress, CFileName + CDefaultInMemFileNameHashSeparator + CExpectedHash, True)).ToBe(True);
end;


procedure TTestHTTPAPI.Test_GetFileExistenceOnServer_MultipleFilesNoHash;
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
  Expect(ClearInMemFileSystem(CTestServerAddress)).ToBe(CREResp_Done);
  SendMultipleTestFilesToServer(@CFileNames, Length(CFileNames), 'SomeContentForNoHash');

  ListOfFiles := TStringList.Create;
  ListOfResults := TStringList.Create;
  try
    ListOfFiles.Add(CFileName1);
    ListOfFiles.Add(CFileName2);
    ListOfFiles.Add(CFileName3);
    ListOfFiles.Add('non-existent file');

    Response := GetFileExistenceOnServer(CTestServerAddress, ListOfFiles, ListOfResults, False);

    Expect(Response).ToBe('');
    Expect(ListOfResults).ToMatchContentOfStringArray(@CExpectedExistence, 4, 'File existence on server.');
  finally
    ListOfFiles.Free;
    ListOfResults.Free;
  end;
end;


procedure TTestHTTPAPI.Test_GetFileExistenceOnServer_MultipleFilesWithHash;
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
  Expect(ClearInMemFileSystem(CTestServerAddress)).ToBe(CREResp_Done);
  SendMultipleTestFilesToServer(@CFileNames, Length(CFileNames), 'SomeContentForWithHash');

  ListOfFiles := TStringList.Create;
  ListOfResults := TStringList.Create;
  try
    ListOfFiles.Add(CFileName1 + CDefaultInMemFileNameHashSeparator + '01A883F23CCAFE1CB87AE35BABA90B91');
    ListOfFiles.Add(CFileName2 + CDefaultInMemFileNameHashSeparator + 'bad hash');  //60AFF80200964FA6EFE893DE0F4E2352
    ListOfFiles.Add(CFileName3 + CDefaultInMemFileNameHashSeparator + 'B924828E25CC6B02FC1D84D8129E599F');
    ListOfFiles.Add('non-existent file' + CDefaultInMemFileNameHashSeparator + 'no hash');

    Response := GetFileExistenceOnServer(CTestServerAddress, ListOfFiles, ListOfResults, True);

    Expect(Response).ToBe('');
    Expect(ListOfResults).ToMatchContentOfStringArray(@CExpectedExistence, 4, 'File existence on server.');
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
  Expect(Response).ToBe(CREResp_Done);
  Expect(GetFileExistenceOnServer(CTestServerAddress, CFileName, False)).ToBe(False);
end;


procedure TTestHTTPAPI.Test_SetVariable_HappyFlow;
begin
  Expect(SetVariable(CTestServerAddress, '$MyVar$', 'some value', 0)).ToBe(CREResp_Done);
end;


procedure TTestHTTPAPI.Test_SetVariable_BadStackLevel;
const
  CBadValue = 'some bad value';
var
  Response: string;
  ListOfVars: TStringList;
begin
  Expect(SetVariable(CTestServerAddress, '$MyVar$', CBadValue, 10)).ToBe(CExpectedBadStackLevelResponse);

  Response := FastReplace_87ToReturn(GetAllReplacementVars(CTestServerAddress, 0));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    Expect(ListOfVars).WithItem('$MyVar$').DifferentThanValue(CBadValue);
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
  tp, InitialTp: TPoint;
begin
  GenerateClickOptionsForLeaveMouse(CX, CY, ClickOptions);
  GetCursorPos(InitialTp);

  Response := FastReplace_87ToReturn(ExecuteClickAction(CTestServerAddress, ClickOptions));

  GetCursorPos(tp);
  Expect(tp.X).ToBe(CX, 'mouse pos');
  Expect(tp.Y).ToBe(CY, 'mouse pos');

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    ExpectSuccessfulAction(ListOfVars);
  finally
    ListOfVars.Free;
    SetCursorPos(InitialTp.X, InitialTp.Y);
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
    ExpectSuccessfulAction(ListOfVars);
    Expect(ListOfVars).WithItem('$ExecAction_StdOut$').ToContain('Windows IP Configuration');
    Expect(ListOfVars).WithItem('$ExecAction_StdOut$').ToContain('Host Name');
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
    ExpectSuccessfulAction(ListOfVars);
    Expect(ListOfVars).WithItem('$ExecAction_StdOut$').OfValue('');
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
    ExpectSuccessfulAction(ListOfVars);
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
    ExpectFailedAction(ListOfVars, 'Timeout at "TestFind UIClicker Main" in ');
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
    ExpectAllowedFailedAction(ListOfVars);
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
    ExpectSuccessfulAction(ListOfVars);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_WindowInterpreterButton_Disk;
var
  Response: string;
  ListOfVars: TStringList;
  FindSubControlOptions: TClkFindControlOptions;
begin
  SetupTargetWindowFor_FindSubControl;
  GenerateFindSubControlOptionsForMainUIClickerWindow_WinInterpBtn(FindSubControlOptions, False);
  Response := FastReplace_87ToReturn(ExecuteFindSubControlAction(CTestServerAddress, FindSubControlOptions, 'Test Find WindowInterpreterButton_Disk on UIClicker Main', 3000, CREParam_FileLocation_ValueDisk));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    ExpectSuccessfulAction(ListOfVars);
  finally
    ListOfVars.Free;
  end;
end;


initialization

  RegisterTest(TTestHTTPAPI);
end.

