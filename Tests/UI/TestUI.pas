{
    Copyright (C) 2025 VCC
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


unit TestUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestHTTPAPI, fpcunit, testregistry, UIActionsStuff;

type

  TTestUI = class(TTestHTTPAPI)
  protected
    //procedure SetUp; override;
    //procedure TearDown; override;

    procedure StartAllUIClickerInstances;
    procedure ExecuteTemplateOnTestDriver(ATemplatePath, AFileLocation: string; AAdditionalExpectedVar: string = ''; AAdditionalExpectedValue: string = '');
    procedure ArrangeMainUIClickerWindows;
    procedure ArrangeUIClickerActionWindows;

    procedure LoadTestTemplateInClickerUnderTest(ATestTemplate: string);
    procedure LoadTestTemplateInClickerUnderTest_FullPath(ATestTemplate: string);

    procedure RunTestTemplateInClickerUnderTest(ATestTemplate: string);
    procedure RunTestTemplateInClickerUnderTestWithDebugging(ATestTemplate: string);
    procedure RunTestTemplateInClickerUnderTestWithDebuggingAndStepInto(ATestTemplate, AExpectedStepOverCount: string; AStopAfterSteppingInto: string = '0');
    procedure RunTestTemplateInClickerUnderTest_FullPath(ATestTemplate: string);

    procedure PrepareClickerUnderTestToReadItsVars;
    procedure PrepareClickerUnderTestToLocalMode;
    procedure PrepareClickerUnderTestToClientMode;

    procedure ExpectVarFromClientUnderTest(AVarName, AExpectedValue: string; AExtraComment: string = '');

    procedure DragAction_FromPosToPos(ASrcIdx, ADestIdx: Integer; const AActIdx: array of Byte);
    procedure AutoCompleteTextInConsoleEditBox(ATypedEditBoxContent, AFunctionNameToDoubleClick, AExpectedResult: string);
    procedure AddActionButton(AActionNameToSelect: string);

    procedure VerifyOIDefaultValues(AActionToDrag: string; var AProperties: TOIInteractionDataArr);
    procedure VerifyOIDifferentThanDefaultValues(AActionToDrag: string; var AProperties: TOIInteractionDataArr);
    procedure VerifyPermissionsOnSendingFiles;
    procedure EditOIPropertiesFromMenu(AActionToDrag, APropertyToEdit, ASubPropertyToEdit, AScrollCount, AMenuItemCaption, ANewPropertyValue: string);
  public
    constructor Create; override;
  published
    procedure BeforeAll_AlwaysExecute;

    procedure Test_ExecuteTemplateRemotely;
    procedure Test_ExecuteTemplateRemotely_WithLoopedCall;
    procedure Test_ExecuteTemplateRemotelyWithDebugging;
    procedure Test_ExecuteTemplateRemotelyWithDebugging_WithLoopedCall;
    procedure Test_ExecuteTemplateRemotelyWithDebugging_AndStepInto;
    procedure Test_ExecuteTemplateRemotelyWithDebugging_WithLoopedCall_AndStepInto;
    procedure Test_ExecuteTemplateRemotelyWithDebugging_AndStepIntoThenStop;
    //procedure Test_ExecuteTemplateRemotelyWithDebugging_WithLoopedCall_AndStepIntoThenStop;

    procedure Test_FindColorErrorFromGradientOnServer_ErrorLevel_SuccessVerbose_NoUpdate;
    procedure Test_FindColorErrorFromGradientOnServer_ErrorCount_SuccessVerbose_NoUpdate;

          //remote execution is disabled for these tests, because the action is not updated, on the server side, by the searching algorithm
          //when/if implemented, these tests should pass
    //procedure Test_ExecuteTemplateRemotely_FindColorErrorFromGradientOnServer_ErrorLevel_SuccessVerbose;
    //procedure Test_ExecuteTemplateRemotely_FindColorErrorFromGradientOnServer_ErrorCount_SuccessVerbose;

    procedure Test_DragAction_FromPos0ToPos4;
    procedure Test_DragAction_FromPos4ToPos0;
    procedure Test_DragAction_FromPos1ToPos4;
    procedure Test_DragAction_FromPos4ToPos1;

    procedure Test_ExecuteTemplateRemotelyWithDebugging_WithLoopedCall_AndStepIntoThenStop;
    procedure Test_ExecuteTemplateRemotelyWithDebugging_ToVerifyIfCallTemplateWorksAfterStopping;

    procedure TestAutoComplete_EmptyEditBox;
    procedure TestAutoComplete_PartialFunctionNameOnly;
    procedure TestAutoComplete_FullFunctionNameOnly;
    procedure TestAutoComplete_VarAssignmentToPartialFunctionNameOnly;
    procedure TestAutoComplete_VarAssignmentToFullFunctionNameOnly;
    procedure TestAutoComplete_VarAssignmentToEmpty;
    procedure TestAutoComplete_VarAssignmentToNumericConstantAndThenDifferentSelection;
    procedure TestAutoComplete_VarAssignmentAndBlanksToPartialFunctionNameOnly;
    procedure TestAutoComplete_VarAssignmentAndBlanksToFullFunctionNameOnly;

    procedure TestAddActionButton_HappyFlow;

    procedure TestVerifyOIDefaultValues_Click;
    procedure TestVerifyOIDefaultValues_ExecApp;
    procedure TestVerifyOIDefaultValues_FindControl;
    procedure TestVerifyOIDefaultValues_FindSubControl;
    procedure TestVerifyOIDefaultValues_SetControlText;
    procedure TestVerifyOIDefaultValues_CallTemplate;
    procedure TestVerifyOIDefaultValues_Sleep;
    procedure TestVerifyOIDefaultValues_SetVar;
    procedure TestVerifyOIDefaultValues_WindowOperations;
    procedure TestVerifyOIDefaultValues_LoadSetVarFromFile;
    procedure TestVerifyOIDefaultValues_SaveSetVarToFile;
    procedure TestVerifyOIDefaultValues_Plugin;
    procedure TestVerifyOIDefaultValues_EditTemplate;

    procedure TestVerifyOIDifferentThanDefaultValues_Click;
    procedure TestVerifyOIDifferentThanDefaultValues_ExecApp;
    procedure TestVerifyOIDifferentThanDefaultValues_FindControl;
    procedure TestVerifyOIDifferentThanDefaultValues_FindSubControl;
    procedure TestVerifyOIDifferentThanDefaultValues_FindSubControl_SubProperties;
    procedure TestVerifyOIDifferentThanDefaultValues_SetControlText;
    procedure TestVerifyOIDifferentThanDefaultValues_CallTemplate;
    procedure TestVerifyOIDifferentThanDefaultValues_Sleep;
    procedure TestVerifyOIDifferentThanDefaultValues_SetVar;
    procedure TestVerifyOIDifferentThanDefaultValues_WindowOperations;
    procedure TestVerifyOIDifferentThanDefaultValues_LoadSetVarFromFile;
    procedure TestVerifyOIDifferentThanDefaultValues_SaveSetVarToFile;
    procedure TestVerifyOIDifferentThanDefaultValues_Plugin;
    procedure TestVerifyOIDifferentThanDefaultValues_EditTemplate;

    procedure TestEditOIProperty_FindControl_InitialRectangle_Left_SetToValueFromMenu;
    procedure TestEditOIProperty_FindControl_InitialRectangle_Top_SetToValueFromMenu;
    procedure TestEditOIProperty_FindControl_InitialRectangle_Right_SetToValueFromMenu;
    procedure TestEditOIProperty_FindControl_InitialRectangle_Bottom_SetToValueFromMenu;

    procedure TestEditOIProperty_FindSubControl_InitialRectangle_Left_SetToValueFromMenu;
    procedure TestEditOIProperty_FindSubControl_InitialRectangle_Top_SetToValueFromMenu;
    procedure TestEditOIProperty_FindSubControl_InitialRectangle_Right_SetToValueFromMenu;
    procedure TestEditOIProperty_FindSubControl_InitialRectangle_Bottom_SetToValueFromMenu;

    procedure TestEditOIProperty_FindSubControl_MatchBitmapText_ForegroundColor_SetToValueFromMenu;
    procedure TestEditOIProperty_FindSubControl_MatchBitmapText_BackgroundColor_SetToValueFromMenu;

    procedure TestVerifyPermissionsOnSendingFiles_SendingFileFromDeniedTestFiles;
    procedure TestVerifyPermissionsOnSendingFiles_ReSendingModifiedFileByEditTemplate;

    procedure AfterAll_AlwaysExecute;
  end;

{
  This test application uses a second instance of UIClicker, called TestDriver, from TestDriver directory,
  which clicks and types on UIClicker-under-test, to test its UI.
  Depending on desired features, TestDriver may have to be up to date, but nonetheless, in an working state.
  When testing client-server mode, a second pair of UIClicker instances will be required.
  Depending on test, a single TestDriver may be enough to drive two UIClicker-under-test instances.

  This test app is expected to run from UIClicker\Tests\UI
  Other requirements:
  - The UIClicker-Under-Test (client) should allow sending files from \TestDriver\ActionTemplates dir
  - The UIClicker-Under-Test (client) should be configured to connect to 127.0.0.1:5444

  Possible connection diagram:
                             +---- connects to ---------------------------------------------------------+ (read results)
                             |                                                                          |
                             |                                                                         \|/  (temporary in server mode 35444, after executing test template, to provide its vars to UIClickerUITest)
  UIClickerUITest (this app)-+---- connects to ---> TestDriver_1(in server mode) ----- clicks on ----> UIClicker-Under-Test(in client mode or local mode)
                             |                      25444                                                  |
                             |                                                                             | connects to
                             |                                                                             |
                             |                                                                            \|/
                             +---- connects to ---> TestDriver_2(in server mode) ----- clicks on ----> UIClicker-Under-Test(in server mode)
                             |                       15444                                              5444    /|\
                             |                                                                                   |
                             +---. connects to --.---.---.---.---.---.---.---.---.---.---.---.---.---.---.---.---+ (read results)
}


const
  CTestDriver_ServerPort_ForServerUnderTest = '15444';
  CTestDriver_ServerPort_ForClientUnderTest = '25444';
  CServerUnderTestServerPort = '5444';
  CClientUnderTestServerPort = '35444'; //this is a temporary mode, while UIClickerUITest reads the test results, then sets it back to client mode

  CTestServerAddress = 'http://127.0.0.1:' + CServerUnderTestServerPort + '/';                                //UIClicker-under-test server
  CTestClientAddress = 'http://127.0.0.1:' + CClientUnderTestServerPort + '/';                                //UIClicker-under-test client in server mode
  CTestDriverServerAddress_Server = 'http://127.0.0.1:' + CTestDriver_ServerPort_ForServerUnderTest + '/';    //UIClicker driver  (a different instance of UIClicker, which drives UIClicker-under-test)
  CTestDriverServerAddress_Client = 'http://127.0.0.1:' + CTestDriver_ServerPort_ForClientUnderTest + '/';    //UIClicker driver

  CMinErrorLevelKeyword = 'level';
  CMinErrorCountKeyword = 'count';
  CMinErrorMsgBoxBtn_NoUpdate = 'False';
  CMinErrorMsgBoxBtn_YesUpdate = 'True';


implementation


uses
  ClickerActionsClient, ClickerUtils, AsyncProcess, Process, Expectations, Forms, IniFiles,
  ObjectInspectorFrame, ClickerActionProperties, ClickerActionValues, ActionsStuff;


var  //using global vars instead of class fields, to avoid recreating the objects on every test
  FTestDriverForServer_Proc, FTestDriverForClient_Proc, FServerAppUnderTest_Proc, FClientAppUnderTest_Proc: TAsyncProcess;
  FTemplatesDir: string;
  FIsWine: Boolean;


constructor TTestUI.Create;
begin
  inherited Create;
  TestServerAddress := CTestServerAddress;
end;


//procedure TTestUI.SetUp;
//begin
//  inherited;
//end;
//
//procedure TTestUI.TearDown;
//begin
//  inherited;
//end;


function CreateUIClickerProcess(AExe, AParams: string): TAsyncProcess;
begin
  Result := TAsyncProcess.Create(nil);
  Result.Executable := AExe;
  Result.Parameters.Text := StringReplace(AParams, #32, #13#10, [rfReplaceAll]);
  Result.CurrentDirectory := ExtractFileDir(AExe);
  Result.InheritHandles := False;
  Result.Execute;
end;


procedure SetUIClickerWindowPosition(APathToIni: string; AMainLeft, AMainTop, AActionsLeft, AActionsTop: Integer);  //useful, to preview the interaction
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(APathToIni);
  try
    Ini.WriteInteger('MainWindow', 'Left', AMainLeft);
    Ini.WriteInteger('MainWindow', 'Top', AMainTop);
    Ini.WriteInteger('ActionsWindow', 'Left', AActionsLeft);
    Ini.WriteInteger('ActionsWindow', 'Top', AActionsTop);
  finally
    Ini.Free;
  end;
end;


procedure TTestUI.StartAllUIClickerInstances;
const
  CDisplayTabsOptions: string = ' --AutoSwitchToExecTab Yes --AutoEnableSwitchTabsOnDebugging Yes';
  CSkipSavingSettings: string = ' --SkipSavingSettings Yes';
var
  PathToTestDriver, PathToAppUnderTest: string;
  ServerParams, ClientParams, AppUnderTestServerParams, AppUnderTestClientParams: string;
begin
  PathToTestDriver := ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\UIClicker.exe'; //this should be a stable version of UIClicker
  PathToAppUnderTest := ExtractFilePath(ParamStr(0)) + '..\..\UIClicker.exe';

  {$IFDEF UNIX}
    FIsWine := False;
  {$ELSE}
    FIsWine := {DirectoryExists('Z:\home') and} DirectoryExists('Z:\media') and DirectoryExists('Z:\etc'); //assume this is running on Wine
  {$ENDIF}

  ServerParams := '--SetExecMode Server --ExtraCaption Driver.Server --ServerPort ' + CTestDriver_ServerPort_ForServerUnderTest + CDisplayTabsOptions;
  ClientParams := '--SetExecMode Server --ExtraCaption Driver.Client --ServerPort ' + CTestDriver_ServerPort_ForClientUnderTest + CDisplayTabsOptions;
  AppUnderTestServerParams := '--SetExecMode Server --ExtraCaption ServerUnderTest --ServerPort ' + CServerUnderTestServerPort + CSkipSavingSettings + CDisplayTabsOptions;
  AppUnderTestClientParams := '--SetExecMode Client --ExtraCaption ClientUnderTest --ConnectsTo http://127.0.0.1:' + CServerUnderTestServerPort + '/' + CSkipSavingSettings + CDisplayTabsOptions;

  if FIsWine then
  begin
    SetUIClickerWindowPosition(ExtractFilePath(PathToTestDriver) + 'Clicker.ini', 700, 50, 970, 270);
    Sleep(100);
    FTestDriverForServer_Proc := CreateUIClickerProcess(PathToTestDriver, ServerParams + ' --UseWideStringsOnGetControlText Yes');
    Sleep(1000);

    SetUIClickerWindowPosition(ExtractFilePath(PathToTestDriver) + 'Clicker.ini', 1040, 50, 1000, 300);
    Sleep(100);
    FTestDriverForClient_Proc := CreateUIClickerProcess(PathToTestDriver, ClientParams + ' --UseWideStringsOnGetControlText Yes');
    Sleep(1000);

    SetUIClickerWindowPosition(ExtractFilePath(PathToAppUnderTest) + 'Clicker.ini', 10, 50, 10, 330);
    Sleep(100);
    FServerAppUnderTest_Proc := CreateUIClickerProcess(PathToAppUnderTest, AppUnderTestServerParams + ' --UseWideStringsOnGetControlText Yes');
    Sleep(1000);

    SetUIClickerWindowPosition(ExtractFilePath(PathToAppUnderTest) + 'Clicker.ini', 360, 50, 30, 490);
    Sleep(100);
    FClientAppUnderTest_Proc := CreateUIClickerProcess(PathToAppUnderTest, AppUnderTestClientParams + ' --UseWideStringsOnGetControlText Yes');
    Sleep(1000);
  end
  else
  begin
    FTestDriverForServer_Proc := CreateUIClickerProcess(PathToTestDriver, ServerParams);
    FTestDriverForClient_Proc := CreateUIClickerProcess(PathToTestDriver, ClientParams);
    FServerAppUnderTest_Proc := CreateUIClickerProcess(PathToAppUnderTest, AppUnderTestServerParams);
    FClientAppUnderTest_Proc := CreateUIClickerProcess(PathToAppUnderTest, AppUnderTestClientParams);
  end;
end;


procedure TTestUI.ExecuteTemplateOnTestDriver(ATemplatePath, AFileLocation: string; AAdditionalExpectedVar: string = ''; AAdditionalExpectedValue: string = '');
const
  CSecondExpectedErrMsg = 'Action condition is false.';
var
  Response: string;
  ListOfVars: TStringList;
  CallTemplateOptions: TClkCallTemplateOptions;
begin
  CallTemplateOptions.TemplateFileName := ATemplatePath;
  CallTemplateOptions.ListOfCustomVarsAndValues := '';
  CallTemplateOptions.EvaluateBeforeCalling := False;
  CallTemplateOptions.CallTemplateLoop.Enabled := False;
  CallTemplateOptions.CallTemplateLoop.Direction := ldInc;
  CallTemplateOptions.CallTemplateLoop.EvalBreakPosition := lebpAfterContent;

  Response := FastReplace_87ToReturn(ExecuteCallTemplateAction(CTestDriverServerAddress_Client, CallTemplateOptions, False, False, AFileLocation));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    try
      Expect(ListOfVars).WithItem('$ExecAction_Err$').OfValue('', 'No error Allowed in test driver... $ExecAction_Err$ is ' + ListOfVars.Values['$ExecAction_Err$'] + '   $ExitCode$ is ' + ListOfVars.Values['$ExitCode$']);
    except
      on E: Exception do
        Expect(ListOfVars).WithItem('$ExecAction_Err$').OfValue(CSecondExpectedErrMsg, 'No error Allowed in test driver. $ExecAction_Err$ is ' + ListOfVars.Values['$ExecAction_Err$'] + '   $ExitCode$ is ' + ListOfVars.Values['$ExitCode$']);
    end;

    if AAdditionalExpectedVar <> '' then
      Expect(ListOfVars).WithItem(AAdditionalExpectedVar).OfValue(AAdditionalExpectedValue, 'Additional variable mismatch.');
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestUI.ArrangeMainUIClickerWindows;  //These "arrange windows" calls will have to be replaced with modifying the UIClicker ini files, before executing. This will be required to run tests on Wine.
begin
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\ArrangeMainUIClickerWindows.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestUI.ArrangeUIClickerActionWindows;
begin
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\ArrangeActionWindowsUnderTest.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure SetVariableOnTestDriverClient(AVarName, AVarValue: string; AEvalVarBefore: Boolean = False);
var
  SetVarOptions: TClkSetVarOptions;
  SetVarResult: TStringList;
  RawResult: string;
begin
  SetVarOptions.ListOfVarNames := AVarName;
  SetVarOptions.ListOfVarValues := AVarValue;
  SetVarOptions.ListOfVarEvalBefore := Chr(48 + Ord(AEvalVarBefore));

  SetVarResult := TStringList.Create;
  try
    RawResult := ExecuteSetVarAction(CTestDriverServerAddress_Client, SetVarOptions);
    SetVarResult.Text := FastReplace_87ToReturn(RawResult); //this is the test "driver" for client 25444
    Expect(SetVarResult).WithItem(AVarName).OfValue(AVarValue, 'Expected a particular var value.');
  finally
    SetVarResult.Free;
  end;
end;


procedure WaitForDriverStartup;
var
  TestResult: string;
  tk: QWord;
begin
  tk := GetTickCount64;
  repeat
    TestResult := TestConnection(CTestDriverServerAddress_Client, False);
  until (TestResult = CREResp_ConnectionOK) or (GetTickCount64 - tk > 3000);
end;


procedure TTestUI.BeforeAll_AlwaysExecute;
begin
  StartAllUIClickerInstances;

  WaitForDriverStartup;

  try
    if FIsWine then
    begin
      GeneralConnectTimeout := 10000;
      SetVariableOnTestDriverClient('$IsAdminOnWine$', '  [Is admin]');
      Application.MainForm.Caption := Application.MainForm.Caption  + '  $IsAdminOnWine$';
    end
    else
      SetVariableOnTestDriverClient('$IsAdminOnWine$', ''); // a single #13#10 results in an empty string item in a TStringList. Still, better send '', to allow the expectation to match ''. UIClicker should convert this one '', into a new line.
  except
    on E: Exception do
      raise Exception.Create('Please verify if UIClicker is built for testing (including the test driver). ' + E.Message);
  end;

  ArrangeMainUIClickerWindows;      //Setting window position from ini file, works on Wine. Setting from UIClicker does not (yet).
  Sleep(500);                       //these sleep calls should be replaced by some waiting loops
  ArrangeUIClickerActionWindows;
  Sleep(500);

  FTemplatesDir := ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\';
end;


procedure TTestUI.LoadTestTemplateInClickerUnderTest(ATestTemplate: string);
begin
  SetVariableOnTestDriverClient('$TemplateToLoad$', FTemplatesDir + ATestTemplate);
  ExecuteTemplateOnTestDriver(FTemplatesDir + 'LoadCallerTemplateIntoAppUnderTest.clktmpl', CREParam_FileLocation_ValueDisk); //this loads BasicCaller into test client (the one which is in client mode)
end;


procedure TTestUI.LoadTestTemplateInClickerUnderTest_FullPath(ATestTemplate: string);
begin
  SetVariableOnTestDriverClient('$TemplateToLoad$', ATestTemplate);
  ExecuteTemplateOnTestDriver(FTemplatesDir + 'LoadCallerTemplateIntoAppUnderTest.clktmpl', CREParam_FileLocation_ValueDisk); //this loads BasicCaller into test client (the one which is in client mode)
end;


procedure TTestUI.RunTestTemplateInClickerUnderTest(ATestTemplate: string);
begin
  LoadTestTemplateInClickerUnderTest(ATestTemplate);
  ExecuteTemplateOnTestDriver(FTemplatesDir + 'PlayAllActionsFromAppUnderTest.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestUI.RunTestTemplateInClickerUnderTestWithDebugging(ATestTemplate: string);
begin
  LoadTestTemplateInClickerUnderTest(ATestTemplate);
  ExecuteTemplateOnTestDriver(FTemplatesDir + 'PlayAllActionsFromAppUnderTestWithDebugging.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestUI.RunTestTemplateInClickerUnderTestWithDebuggingAndStepInto(ATestTemplate, AExpectedStepOverCount: string; AStopAfterSteppingInto: string = '0');
begin
  SetVariableOnTestDriverClient('$ExpectedStepOverCount$', AExpectedStepOverCount); //how many times to click "Step Over" in the called template
  SetVariableOnTestDriverClient('$StopAfterSteppingInto$', AStopAfterSteppingInto);
  LoadTestTemplateInClickerUnderTest(ATestTemplate);
  ExecuteTemplateOnTestDriver(FTemplatesDir + 'PlayAllActionsFromAppUnderTestWithDebuggingAndSteppingInto.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestUI.RunTestTemplateInClickerUnderTest_FullPath(ATestTemplate: string);
begin
  LoadTestTemplateInClickerUnderTest_FullPath(ATestTemplate);
  ExecuteTemplateOnTestDriver(FTemplatesDir + 'PlayAllActionsFromAppUnderTest.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestUI.PrepareClickerUnderTestToReadItsVars;
begin
  //Set $ExtraCaption$ variable in Client Driver, to 'ClientUnderTest', which is required by SetExecutionModeOnAppUnderTest.clktmpl
  SetVariableOnTestDriverClient('$ExtraCaption$', 'ClientUnderTest');    //even after executing SetExecModeToServer.clktmpl, the caption should stay 'ClientUnderTest'
  SetVariableOnTestDriverClient('$ListeningPort$', CClientUnderTestServerPort);
  ExecuteTemplateOnTestDriver(FTemplatesDir + 'SetExecModeToServer.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestUI.PrepareClickerUnderTestToLocalMode;
begin
  //Set $ExtraCaption$ variable in Client Driver, to 'ClientUnderTest', which is required by SetExecutionModeOnAppUnderTest.clktmpl
  SetVariableOnTestDriverClient('$ExtraCaption$', 'ClientUnderTest');    //even after executing SetExecModeToServer.clktmpl, the caption should stay 'ClientUnderTest'
  ExecuteTemplateOnTestDriver(FTemplatesDir + 'SetExecModeToLocal.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestUI.PrepareClickerUnderTestToClientMode;
begin
  //Set $ExtraCaption$ variable in Client Driver, to 'ClientUnderTest', which is required by SetExecutionModeOnAppUnderTest.clktmpl
  SetVariableOnTestDriverClient('$ExtraCaption$', 'ClientUnderTest');    //even after executing SetExecModeToServer.clktmpl, the caption should stay 'ClientUnderTest'
  ExecuteTemplateOnTestDriver(FTemplatesDir + 'SetExecModeToClient.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestUI.ExpectVarFromClientUnderTest(AVarName, AExpectedValue: string; AExtraComment: string = '');
begin
  TestServerAddress := CTestClientAddress;
  try
    //connect to ClientUnderTest instance, which is now running in server mode and read its variables
    Expect(GetVarValueFromServer(AVarName)).ToBe(AExpectedValue, AExtraComment);
  finally
    TestServerAddress := CTestDriverServerAddress_Client; //restore
  end;
end;


procedure TTestUI.Test_ExecuteTemplateRemotely;
begin
  PrepareClickerUnderTestToClientMode;

  TestServerAddress := CTestDriverServerAddress_Client;
  RunTestTemplateInClickerUnderTest('BasicCaller.clktmpl');
  PrepareClickerUnderTestToReadItsVars;

  Sleep(500);
  ExpectVarFromClientUnderTest('$VarToBeIncremented$', '1', 'This is VarToBeIncremented.');
end;


procedure TTestUI.Test_ExecuteTemplateRemotely_WithLoopedCall;
begin
  PrepareClickerUnderTestToClientMode;

  TestServerAddress := CTestDriverServerAddress_Client;
  RunTestTemplateInClickerUnderTest('BasicLoopedCaller.clktmpl');
  PrepareClickerUnderTestToReadItsVars;

  Sleep(500);
  ExpectVarFromClientUnderTest('$VarToBeIncremented$', '10');  //this var is initialized to 0 in BasicCaller.clktmpl
end;


procedure TTestUI.Test_ExecuteTemplateRemotelyWithDebugging;
begin
  PrepareClickerUnderTestToClientMode;

  TestServerAddress := CTestDriverServerAddress_Client;
  RunTestTemplateInClickerUnderTestWithDebugging('BasicCaller.clktmpl');
  PrepareClickerUnderTestToReadItsVars;

  Sleep(500);
  ExpectVarFromClientUnderTest('$VarToBeIncremented$', '1');  //this var is initialized to 0 in BasicCaller.clktmpl
end;


procedure TTestUI.Test_ExecuteTemplateRemotelyWithDebugging_WithLoopedCall;
begin
  PrepareClickerUnderTestToClientMode;

  TestServerAddress := CTestDriverServerAddress_Client;
  RunTestTemplateInClickerUnderTestWithDebugging('BasicLoopedCaller.clktmpl');
  PrepareClickerUnderTestToReadItsVars;

  Sleep(500);
  ExpectVarFromClientUnderTest('$VarToBeIncremented$', '10');  //this var is initialized to 0 in BasicCaller.clktmpl
end;


procedure TTestUI.Test_ExecuteTemplateRemotelyWithDebugging_AndStepInto;
begin
  PrepareClickerUnderTestToClientMode;

  TestServerAddress := CTestDriverServerAddress_Client;
  RunTestTemplateInClickerUnderTestWithDebuggingAndStepInto('BasicCaller.clktmpl', '1');
  PrepareClickerUnderTestToReadItsVars;

  Sleep(500);
  ExpectVarFromClientUnderTest('$VarToBeIncremented$', '1'); //this var is initialized to 0 in BasicCaller.clktmpl
end;


procedure TTestUI.Test_ExecuteTemplateRemotelyWithDebugging_WithLoopedCall_AndStepInto;
begin
  PrepareClickerUnderTestToClientMode;

  TestServerAddress := CTestDriverServerAddress_Client;
  RunTestTemplateInClickerUnderTestWithDebuggingAndStepInto('BasicLoopedCaller.clktmpl', '10');
  PrepareClickerUnderTestToReadItsVars;

  Sleep(500);
  ExpectVarFromClientUnderTest('$VarToBeIncremented$', '10');  //this var is initialized to 0 in BasicCaller.clktmpl
end;


procedure TTestUI.Test_ExecuteTemplateRemotelyWithDebugging_AndStepIntoThenStop;
begin
  PrepareClickerUnderTestToClientMode;

  TestServerAddress := CTestDriverServerAddress_Client;
  RunTestTemplateInClickerUnderTestWithDebuggingAndStepInto('BasicCaller.clktmpl', '1', '1');
  PrepareClickerUnderTestToReadItsVars;

  Sleep(500);
  ExpectVarFromClientUnderTest('$VarToBeIncremented$', '0');  //this var is initialized to 0 in BasicCaller.clktmpl
end;


procedure TTestUI.Test_ExecuteTemplateRemotelyWithDebugging_WithLoopedCall_AndStepIntoThenStop;
begin
  PrepareClickerUnderTestToClientMode;

  TestServerAddress := CTestDriverServerAddress_Client;
  RunTestTemplateInClickerUnderTestWithDebuggingAndStepInto('BasicLoopedCaller.clktmpl', '10', '1');
  PrepareClickerUnderTestToReadItsVars;

  Sleep(500);
  ExpectVarFromClientUnderTest('$VarToBeIncremented$', '0', '$VarToBeIncremented$ should be reset to 0.  $DbgPlayAllActions$ is ' + GetVarValueFromServer('$DbgPlayAllActions$')); //this var is initialized to 0 in BasicCaller.clktmpl
end;


procedure TTestUI.Test_ExecuteTemplateRemotelyWithDebugging_ToVerifyIfCallTemplateWorksAfterStopping;  //this is a combination of above AndStepIntoThenStop and WithLoopedCall_AndStepIntoThenStop
begin
  PrepareClickerUnderTestToClientMode;

  TestServerAddress := CTestDriverServerAddress_Client;
  RunTestTemplateInClickerUnderTestWithDebuggingAndStepInto('BasicCaller.clktmpl', '1', '1');
  RunTestTemplateInClickerUnderTestWithDebuggingAndStepInto('BasicLoopedCaller.clktmpl', '10', '1');
  PrepareClickerUnderTestToReadItsVars;

  Sleep(500);
  ExpectVarFromClientUnderTest('$VarToBeIncremented$', '0', '$VarToBeIncremented$ should be reset to 0.  $DbgPlayAllActions$ is ' + GetVarValueFromServer('$DbgPlayAllActions$'));  //this var is initialized to 0 in BasicCaller.clktmpl
end;


procedure TTestUI.Test_FindColorErrorFromGradientOnServer_ErrorLevel_SuccessVerbose_NoUpdate;
begin
  TestServerAddress := CTestDriverServerAddress_Client;

  PrepareClickerUnderTestToLocalMode;
  RunTestTemplateInClickerUnderTest('FindColorErrorFromGradientOnServer.clktmpl');

  SetVariableOnTestDriverClient('$SearchResult$', 'False');
  SetVariableOnTestDriverClient('$CalcItem$', 'Calculate minimum color error ' + CMinErrorLevelKeyword + ' to match bitmap...');
  SetVariableOnTestDriverClient('$ExpectedColorErrorMsg$', 'A color error ' + CMinErrorLevelKeyword + ' found');
  SetVariableOnTestDriverClient('$MsgBoxButtonEval$', CMinErrorMsgBoxBtn_NoUpdate);

  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\FindMinErrorLevel.clktmpl',
                              CREParam_FileLocation_ValueDisk,
                              '$SearchResult$',
                              'True'
                              );
  PrepareClickerUnderTestToReadItsVars;
end;


procedure TTestUI.Test_FindColorErrorFromGradientOnServer_ErrorCount_SuccessVerbose_NoUpdate;
begin
  TestServerAddress := CTestDriverServerAddress_Client;

  PrepareClickerUnderTestToLocalMode;
  RunTestTemplateInClickerUnderTest('FindColorErrorFromGradientOnServer.clktmpl');

  SetVariableOnTestDriverClient('$SearchResult$', 'False');
  SetVariableOnTestDriverClient('$CalcItem$', 'Calculate minimum color error ' + CMinErrorCountKeyword + ' to match bitmap...');
  SetVariableOnTestDriverClient('$ExpectedColorErrorMsg$', 'A color error ' + CMinErrorCountKeyword + ' found');
  SetVariableOnTestDriverClient('$MsgBoxButtonEval$', CMinErrorMsgBoxBtn_NoUpdate);

  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\FindMinErrorLevel.clktmpl',
                              CREParam_FileLocation_ValueDisk,
                              '$SearchResult$',
                              'True'
                              );
  PrepareClickerUnderTestToReadItsVars;
end;


//procedure TTestUI.Test_ExecuteTemplateRemotely_FindColorErrorFromGradientOnServer_ErrorLevel_SuccessVerbose_NoUpdate;
//begin
//  TestServerAddress := CTestDriverServerAddress_Client;
//  PrepareClickerUnderTestToClientMode;
//  RunTestTemplateInClickerUnderTest('FindColorErrorFromGradientOnServer.clktmpl');
//
//  SetVariableOnTestDriverClient('$SearchResult$', 'False');
//  SetVariableOnTestDriverClient('$CalcItem$', 'Calculate minimum color error ' + CMinErrorLevelKeyword + ' to match bitmap...');
//  SetVariableOnTestDriverClient('$ExpectedColorErrorMsg$', 'A color error ' + CMinErrorLevelKeyword + ' found');
//  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\FindMinErrorLevel.clktmpl',
                              // CREParam_FileLocation_ValueDisk,
                              //'$SearchResult$',
                              //'True'
                              //);
//
//  SetVariableOnTestDriverClient('$MsgBoxButtonEval$', CMinErrorMsgBoxBtn_NoUpdate);
//
//  PrepareClickerUnderTestToReadItsVars;
//end;
//
//
//procedure TTestUI.Test_ExecuteTemplateRemotely_FindColorErrorFromGradientOnServer_ErrorCount_SuccessVerbose_NoUpdate;
//begin
//  TestServerAddress := CTestDriverServerAddress_Client;
//  PrepareClickerUnderTestToClientMode;
//  RunTestTemplateInClickerUnderTest('FindColorErrorFromGradientOnServer.clktmpl');
//
//  SetVariableOnTestDriverClient('$SearchResult$', 'False');
//  SetVariableOnTestDriverClient('$CalcItem$', 'Calculate minimum color error ' + CMinErrorCountKeyword + ' to match bitmap...');
//  SetVariableOnTestDriverClient('$ExpectedColorErrorMsg$', 'A color error ' + CMinErrorCountKeyword + ' found');
//  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\FindMinErrorLevel.clktmpl',
                            //  CREParam_FileLocation_ValueDisk,
                            //'$SearchResult$',
                            //'True'
                            //);
//
//  SetVariableOnTestDriverClient('$MsgBoxButtonEval$', CMinErrorMsgBoxBtn_NoUpdate);
//
//  PrepareClickerUnderTestToReadItsVars;
//end;


procedure TTestUI.DragAction_FromPosToPos(ASrcIdx, ADestIdx: Integer; const AActIdx: array of Byte);
const
  CActionNamesToDrag: array[0..4] of string = ('"ExecApp"', '"FindControl"', '"FindSubControl"', '"SetControlText"', '"Sleep"');
var
  i: Integer;
begin
  TestServerAddress := CTestDriverServerAddress_Client;
  LoadTestTemplateInClickerUnderTest('ActionsToDrag.clktmpl');

  PrepareClickerUnderTestToLocalMode;
  SetVariableOnTestDriverClient('$SearchResult$', 'False');
  SetVariableOnTestDriverClient('$SourceAction$', CActionNamesToDrag[ASrcIdx]);
  SetVariableOnTestDriverClient('$DestinationAction$', CActionNamesToDrag[ADestIdx]);

  for i := 0 to 4 do
    SetVariableOnTestDriverClient('$Act' +  IntToStr(i) + '$', CActionNamesToDrag[AActIdx[i]]);

  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\DragActionOnAppUnderTest.clktmpl',
                              CREParam_FileLocation_ValueDisk,
                              '$SearchResult$',
                              'True'
                              );
  PrepareClickerUnderTestToReadItsVars;
end;


procedure TTestUI.Test_DragAction_FromPos0ToPos4;
begin
  DragAction_FromPosToPos(0, 4, [1, 2, 3, 4, 0]);
end;


procedure TTestUI.Test_DragAction_FromPos4ToPos0;
begin
  DragAction_FromPosToPos(4, 0, [4, 0, 1, 2, 3]);
end;


procedure TTestUI.Test_DragAction_FromPos1ToPos4;
begin
  DragAction_FromPosToPos(1, 4, [0, 2, 3, 4, 1]);
end;


procedure TTestUI.Test_DragAction_FromPos4ToPos1;
begin
  DragAction_FromPosToPos(4, 1, [0, 4, 1, 2, 3]);
end;


procedure TTestUI.AutoCompleteTextInConsoleEditBox(ATypedEditBoxContent, AFunctionNameToDoubleClick, AExpectedResult: string);
begin
  TestServerAddress := CTestDriverServerAddress_Client;

  PrepareClickerUnderTestToLocalMode;

  if Pos('$', ATypedEditBoxContent) > 0 then
  begin
    SetVariableOnTestDriverClient('$AutoCompleteTextToType_Part1$', Copy(ATypedEditBoxContent, 1, Pos('$', ATypedEditBoxContent)));
    SetVariableOnTestDriverClient('$AutoCompleteTextToType_Part2$', Copy(ATypedEditBoxContent, Pos('$', ATypedEditBoxContent) + 1, MaxInt));
  end
  else
  begin
    SetVariableOnTestDriverClient('$AutoCompleteTextToType_Part2$', '');
    SetVariableOnTestDriverClient('$AutoCompleteTextToType_Part1$', ATypedEditBoxContent);
  end;

  SetVariableOnTestDriverClient('$FuncNameToDoubleClick$', AFunctionNameToDoubleClick);

  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\AutoCompleteTextOnAppUnderTest.clktmpl',
                              CREParam_FileLocation_ValueDisk,
                              '$Control_Text$',
                              AExpectedResult
                              );
  PrepareClickerUnderTestToReadItsVars;
end;


procedure TTestUI.TestAutoComplete_EmptyEditBox;
begin
  AutoCompleteTextInConsoleEditBox('', 'ExtractFileName(<PathToFile>)', '$ExtractFileName(<PathToFile>)$');
end;


procedure TTestUI.TestAutoComplete_PartialFunctionNameOnly;
begin
  AutoCompleteTextInConsoleEditBox('Get', 'GetActionProperties()', '$GetActionProperties()$');
end;


procedure TTestUI.TestAutoComplete_FullFunctionNameOnly;
begin
  AutoCompleteTextInConsoleEditBox('GetActionProperties', 'GetActionProperties()', '$GetActionProperties()$');
end;


procedure TTestUI.TestAutoComplete_VarAssignmentToPartialFunctionNameOnly;
begin
  AutoCompleteTextInConsoleEditBox('$a$=$Get', 'GetActionProperties()', '$a$=$GetActionProperties()$');
end;


procedure TTestUI.TestAutoComplete_VarAssignmentToFullFunctionNameOnly;
begin
  AutoCompleteTextInConsoleEditBox('$a$=$GetActionProperties', 'GetActionProperties()', '$a$=$GetActionProperties()$');
end;


procedure TTestUI.TestAutoComplete_VarAssignmentToEmpty;
begin
  AutoCompleteTextInConsoleEditBox('$a$=', 'GetActionProperties()', '$a$=$GetActionProperties()$');
end;


procedure TTestUI.TestAutoComplete_VarAssignmentToNumericConstantAndThenDifferentSelection;
begin
  AutoCompleteTextInConsoleEditBox('$Desktop_Width$=1920', 'Screen_Width$=1920', '$Desktop_Width$=1920$Screen_Width$=1920');
end;


procedure TTestUI.TestAutoComplete_VarAssignmentAndBlanksToPartialFunctionNameOnly;
begin
  AutoCompleteTextInConsoleEditBox('$a$ = $Get', 'GetActionProperties()', '$a$ = $GetActionProperties()$');
end;


procedure TTestUI.TestAutoComplete_VarAssignmentAndBlanksToFullFunctionNameOnly;
begin
  AutoCompleteTextInConsoleEditBox('$a$ = $GetActionProperties', 'GetActionProperties()', '$a$ = $GetActionProperties()$');
end;


procedure TTestUI.AddActionButton(AActionNameToSelect: string);
begin
  TestServerAddress := CTestDriverServerAddress_Client;
  LoadTestTemplateInClickerUnderTest('DuplicateActionFromAddActionButton.clktmpl');

  PrepareClickerUnderTestToLocalMode;
  SetVariableOnTestDriverClient('$ActionNameToSelect$', AActionNameToSelect);

  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\AddActionButtonOnAppUnderTest.clktmpl',
                              CREParam_FileLocation_ValueDisk,
                              '$LastAction_Status$',
                              'Successful'
                              );
  PrepareClickerUnderTestToReadItsVars;
end;


procedure TTestUI.TestAddActionButton_HappyFlow;
begin
  AddActionButton('GenerateAndSaveTree');
end;


procedure TTestUI.VerifyOIDefaultValues(AActionToDrag: string; var AProperties: TOIInteractionDataArr);
var
  i: Integer;
begin
  TestServerAddress := CTestDriverServerAddress_Client;

  PrepareClickerUnderTestToReadItsVars;  //This is server mode. It is suitable to these tests only, because the actions do not have to be executed.

  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\PrepareClientActionsWindowForInteraction.clktmpl',
                              CREParam_FileLocation_ValueDisk);
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\DeleteAllActionsFromList.clktmpl', //this template depends on caching from above
                              CREParam_FileLocation_ValueDisk);

  SetVariableOnTestDriverClient('$ActionToDrag$', AActionToDrag);
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\DragActionToListOnAppUnderTest.clktmpl',
                              CREParam_FileLocation_ValueDisk,
                              '', //'$Control_Text$',
                              '' //AExpectedResult
                              );

  SetVariableOnTestDriverClient('$PropertyCount$', IntToStr(Length(AProperties)));
  for i := 0 to Length(AProperties) - 1 do
  begin
    SetVariableOnTestDriverClient('$PropertyName$', AProperties[i].BasicPropertyInfo.Name);
    SetVariableOnTestDriverClient('$PropertyNameToExpand$', AProperties[i].PropertyNamesToExpand);
    SetVariableOnTestDriverClient('$PropertyValue$', AProperties[i].PropertyValue);
    SetVariableOnTestDriverClient('$EditorType$', COIEditorTypeStr[AProperties[i].BasicPropertyInfo.EditorType]);
    SetVariableOnTestDriverClient('$PropertyIndex$', IntToStr(i));

    ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\VerifyOIDefaultValuesOnAppUnderTest.clktmpl',
                                CREParam_FileLocation_ValueDisk,
                                '$ValueFound$',
                                'Successful'
                                );
  end;
end;


procedure TTestUI.TestVerifyOIDefaultValues_Click;
var
  Properties: TOIInteractionDataArr;
  ClickOptions: TClkClickOptions;
begin
  GetDefaultPropertyValues_Click(ClickOptions);
  ListOfSerializedPropertiesToOIInteractionData(GetClickActionProperties(ClickOptions), @CClickProperties, CPropIsExp[acClick], CPropCount_Click, Properties);

  VerifyOIDefaultValues(CClkActionStr[acClick], Properties);
end;


procedure TTestUI.TestVerifyOIDefaultValues_ExecApp;
var
  Properties: TOIInteractionDataArr;
  ExecAppOptions: TClkExecAppOptions;
begin
  GetDefaultPropertyValues_ExecApp(ExecAppOptions);
  ListOfSerializedPropertiesToOIInteractionData(GetExecAppActionProperties(ExecAppOptions), @CExecAppProperties, CPropIsExp[acExecApp], CPropCount_ExecApp, Properties);

  VerifyOIDefaultValues(CClkActionStr[acExecApp], Properties);
end;


procedure TTestUI.TestVerifyOIDefaultValues_FindControl;
var
  Properties: TOIInteractionDataArr;
  FindControlOptions: TClkFindControlOptions;
begin
  GetDefaultPropertyValues_FindControl(FindControlOptions);
  ListOfSerializedPropertiesToOIInteractionData(GetFindControlActionProperties(FindControlOptions), @CFindControlProperties, CPropIsExp[acFindControl], CPropCount_FindControl, Properties);

  VerifyOIDefaultValues(CClkActionStr[acFindControl], Properties);
end;


procedure TTestUI.TestVerifyOIDefaultValues_FindSubControl;
var
  Properties: TOIInteractionDataArr;
  FindSubControlOptions: TClkFindSubControlOptions;
begin
  GetDefaultPropertyValues_FindSubControl(FindSubControlOptions);
  ListOfSerializedPropertiesToOIInteractionData(GetFindSubControlActionProperties(FindSubControlOptions), @CFindSubControlProperties, CPropIsExp[acFindSubControl], CPropCount_FindControl, Properties);

  VerifyOIDefaultValues(CClkActionStr[acFindSubControl], Properties);
end;


procedure TTestUI.TestVerifyOIDefaultValues_SetControlText;
var
  Properties: TOIInteractionDataArr;
  SetControlTextOptions: TClkSetTextOptions;
begin
  GetDefaultPropertyValues_SetControlText(SetControlTextOptions);
  ListOfSerializedPropertiesToOIInteractionData(GetSetControlTextActionProperties(SetControlTextOptions), @CSetTextProperties, CPropIsExp[acSetControlText], CPropCount_SetText, Properties);

  VerifyOIDefaultValues(CClkActionStr[acSetControlText], Properties);
end;


procedure TTestUI.TestVerifyOIDefaultValues_CallTemplate;
var
  Properties: TOIInteractionDataArr;
  CallTemplateOptions: TClkCallTemplateOptions;
begin
  GetDefaultPropertyValues_CallTemplate(CallTemplateOptions);
  ListOfSerializedPropertiesToOIInteractionData(GetCallTemplateActionProperties(CallTemplateOptions), @CCallTemplateProperties, CPropIsExp[acCallTemplate], CPropCount_CallTemplate, Properties);

  VerifyOIDefaultValues(CClkActionStr[acCallTemplate], Properties);
end;


procedure TTestUI.TestVerifyOIDefaultValues_Sleep;
var
  Properties: TOIInteractionDataArr;
  SleepOptions: TClkSleepOptions;
begin
  GetDefaultPropertyValues_Sleep(SleepOptions);
  ListOfSerializedPropertiesToOIInteractionData(GetSleepActionProperties(SleepOptions), @CSleepProperties, CPropIsExp[acSleep], CPropCount_Sleep, Properties);

  VerifyOIDefaultValues(CClkActionStr[acSleep], Properties);
end;


procedure TTestUI.TestVerifyOIDefaultValues_SetVar;
var
  Properties: TOIInteractionDataArr;
  SetVarOptions: TClkSetVarOptions;
begin
  GetDefaultPropertyValues_SetVar(SetVarOptions);
  ListOfSerializedPropertiesToOIInteractionData(GetSetVarActionProperties(SetVarOptions), @CSetVarProperties, CPropIsExp[acSetVar], CPropCount_SetVar, Properties);

  VerifyOIDefaultValues(CClkActionStr[acSetVar], Properties);
end;


procedure TTestUI.TestVerifyOIDefaultValues_WindowOperations;
var
  Properties: TOIInteractionDataArr;
  WindowOperationsOptions: TClkWindowOperationsOptions;
begin
  GetDefaultPropertyValues_WindowOperations(WindowOperationsOptions);
  ListOfSerializedPropertiesToOIInteractionData(GetWindowOperationsActionProperties(WindowOperationsOptions), @CWindowOperationsProperties, CPropIsExp[acWindowOperations], CPropCount_WindowOperations, Properties);

  VerifyOIDefaultValues(CClkActionStr[acWindowOperations], Properties);
end;


procedure TTestUI.TestVerifyOIDefaultValues_LoadSetVarFromFile;
var
  Properties: TOIInteractionDataArr;
  LoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions;
begin
  GetDefaultPropertyValues_LoadSetVarFromFile(LoadSetVarFromFileOptions);
  ListOfSerializedPropertiesToOIInteractionData(GetLoadSetVarFromFileActionProperties(LoadSetVarFromFileOptions), @CLoadSetVarFromFileProperties, CPropIsExp[acLoadSetVarFromFile], CPropCount_LoadSetVarFromFile, Properties);

  VerifyOIDefaultValues(CClkActionStr[acLoadSetVarFromFile], Properties);
end;


procedure TTestUI.TestVerifyOIDefaultValues_SaveSetVarToFile;
var
  Properties: TOIInteractionDataArr;
  SaveSetVarToFileOptions: TClkSaveSetVarToFileOptions;
begin
  GetDefaultPropertyValues_SaveSetVarToFile(SaveSetVarToFileOptions);
  ListOfSerializedPropertiesToOIInteractionData(GetSaveSetVarToFileActionProperties(SaveSetVarToFileOptions), @CSaveSetVarToFileProperties, CPropIsExp[acSaveSetVarToFile], CPropCount_SaveSetVarToFile, Properties);

  VerifyOIDefaultValues(CClkActionStr[acSaveSetVarToFile], Properties);
end;


procedure TTestUI.TestVerifyOIDefaultValues_Plugin;
var
  Properties: TOIInteractionDataArr;
  PluginOptions: TClkPluginOptions;
begin
  GetDefaultPropertyValues_Plugin(PluginOptions);
  ListOfSerializedPropertiesToOIInteractionData(GetPluginActionProperties(PluginOptions), @CPluginProperties, CPropIsExp[acPlugin], CPropCount_Plugin, Properties);

  VerifyOIDefaultValues(CClkActionStr[acPlugin], Properties);
end;


procedure TTestUI.TestVerifyOIDefaultValues_EditTemplate;
var
  Properties: TOIInteractionDataArr;
  EditTemplateOptions: TClkEditTemplateOptions;
begin
  GetDefaultPropertyValues_EditTemplate(EditTemplateOptions);
  ListOfSerializedPropertiesToOIInteractionData(GetEditTemplateActionProperties(EditTemplateOptions), @CEditTemplateProperties, CPropIsExp[acEditTemplate], CPropCount_EditTemplate, Properties);

  VerifyOIDefaultValues(CClkActionStr[acEditTemplate], Properties);
end;


///////////////////////////////


function CreatePyProcess(AExe, AParams, ADir: string): TProcess;
begin
  Result := TProcess.Create(nil);
  Result.Executable := AExe;
  Result.Parameters.Text := StringReplace(AParams, #32, #13#10, [rfReplaceAll]);
  Result.CurrentDirectory := ADir;
  Result.InheritHandles := True;
  Result.StartupOptions := Result.StartupOptions + [suoUseShowWindow];
  Result.Options := [poUsePipes, poStderrToOutPut{, poPassInput}]; //- [poNoConsole];
  Result.Options := Result.Options + [poNoConsole];

  Result.Execute;
end;



procedure TTestUI.VerifyOIDifferentThanDefaultValues(AActionToDrag: string; var AProperties: TOIInteractionDataArr);
var
  i: Integer;
  PyProc: TProcess;
  tk: QWord;
  PyOut: string;
begin
  TestServerAddress := CTestDriverServerAddress_Client;

  PrepareClickerUnderTestToReadItsVars;  //This is server mode. It is suitable for these tests only, because the actions do not have to be executed.

  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\PrepareClientActionsWindowForInteraction.clktmpl',
                              CREParam_FileLocation_ValueDisk);
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\DeleteAllActionsFromList.clktmpl', //this template depends on caching from above
                              CREParam_FileLocation_ValueDisk);

  SetVariableOnTestDriverClient('$ActionToDrag$', AActionToDrag);
  SetVariableOnTestDriverClient('$ExtraCaption2$', ' - ClientUnderTest'); //using $ExtraCaption2$, instead of $ExtraCaption$ in VerifyOIDefaultValuesOnAppUnderTest.clktmpl
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\PrepareOIForReadingValues.clktmpl',
                              CREParam_FileLocation_ValueDisk,
                              '', //'$Control_Text$',
                              '' //AExpectedResult
                              );

  //These tests do not call DragActionToListOnAppUnderTest.clktmpl. Instead, a python script sends a request to create the action.

  //run python with arg, which executes an action in debugging mode with "different than default" values for all properties
  PyProc := CreatePyProcess('Python'{.exe'}, '..\..\py\Tests\RunExecute' + AActionToDrag + 'Action.py', ExtractFileDir(ParamStr(0)));
  try
    try
      SetVariableOnTestDriverClient('$PropertyCount$', IntToStr(Length(AProperties)));
      for i := 0 to Length(AProperties) - 1 do
      begin
        SetVariableOnTestDriverClient('$PropertyName$', AProperties[i].BasicPropertyInfo.Name);
        SetVariableOnTestDriverClient('$PropertyNameToExpand$', AProperties[i].PropertyNamesToExpand);
        SetVariableOnTestDriverClient('$PropertyValue$', AProperties[i].PropertyValue);
        SetVariableOnTestDriverClient('$EditorType$', COIEditorTypeStr[AProperties[i].BasicPropertyInfo.EditorType]);
        SetVariableOnTestDriverClient('$PropertyIndex$', IntToStr(i));

        ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\VerifyOIDefaultValuesOnAppUnderTest.clktmpl',
                                    CREParam_FileLocation_ValueDisk,
                                    '$ValueFound$',
                                    'Successful'
                                    );
      end;
    finally
      ExecuteTemplateOnTestDriver('$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl', CREParam_FileLocation_ValueDisk);

      PyOut := 'No output';

      tk := GetTickCount64;
      repeat
        Application.ProcessMessages;
        Sleep(1);
      until not PyProc.Active or (GetTickCount64 - tk > 2000);  //the process may freeze, regardless of timeout

      if (PyProc.Output <> nil) and (PyProc.Output.NumBytesAvailable > 0) then
      begin
        SetLength(PyOut, PyProc.Output.NumBytesAvailable);
        PyProc.Output.Read(PyOut[1], Length(PyOut));
      end;

      PyProc.Terminate(0);
      //MessageBoxFunction(PChar(PyOut), 'PyOut', 0);  //ideally, this should end up in a log
      Expect(DWord(Pos('Traceback (most recent call last)', PyOut))).ToBe(0, 'No py error allowed.');
    end;
  finally
    try
      Expect(PyProc.Active).ToBe(False, 'py should close gracefully, not by timeout.');
    finally
      PyProc.Free;
    end;
  end;
end;


procedure TTestUI.TestVerifyOIDifferentThanDefaultValues_Click;
var
  Properties: TOIInteractionDataArr;
begin
  ListOfSerializedPropertiesToOIInteractionData(GenerateDifferentThanDefault_ClickStr, @CClickProperties, CPropIsExp[acClick], CPropCount_Click, Properties);

  VerifyOIDifferentThanDefaultValues(CClkActionStr[acClick], Properties);
end;


procedure TTestUI.TestVerifyOIDifferentThanDefaultValues_ExecApp;
var
  Properties: TOIInteractionDataArr;
begin
  ListOfSerializedPropertiesToOIInteractionData(GenerateDifferentThanDefault_ExecAppStr, @CExecAppProperties, CPropIsExp[acExecApp], CPropCount_ExecApp, Properties);

  VerifyOIDifferentThanDefaultValues(CClkActionStr[acExecApp], Properties);
end;


procedure TTestUI.TestVerifyOIDifferentThanDefaultValues_FindControl;
var
  Properties: TOIInteractionDataArr;
begin
  ListOfSerializedPropertiesToOIInteractionData(GenerateDifferentThanDefault_FindControlStr, @CFindControlProperties, CPropIsExp[acFindControl], CPropCount_FindControl, Properties);

  VerifyOIDifferentThanDefaultValues(CClkActionStr[acFindControl], Properties);
end;


procedure TTestUI.TestVerifyOIDifferentThanDefaultValues_FindSubControl;
var
  Properties: TOIInteractionDataArr;
begin
  ListOfSerializedPropertiesToOIInteractionData(GenerateDifferentThanDefault_FindSubControlStr, @CFindSubControlProperties, CPropIsExp[acFindSubControl], CPropCount_FindSubControl, Properties);

  VerifyOIDifferentThanDefaultValues(CClkActionStr[acFindSubControl], Properties);
end;


procedure TTestUI.TestVerifyOIDifferentThanDefaultValues_FindSubControl_SubProperties;
var
  Properties: TOIInteractionDataArr;
  FindSubControlOptions: TClkFindSubControlOptions;
begin
  GenerateDifferentThanDefault_FindSubControl(FindSubControlOptions);
  ListOfSerializedPropertiesToOIInteractionData(GetFindSubControlActionProperties(FindSubControlOptions), @CFindSubControlProperties, CPropIsExp[acFindSubControl], CPropCount_FindControl, Properties);

  //Get only the properties from structures and arrays

  //VerifyOIDifferentThanDefaultValues(CClkActionStr[acFindSubControl], Properties);
end;


procedure TTestUI.TestVerifyOIDifferentThanDefaultValues_SetControlText;
var
  Properties: TOIInteractionDataArr;
begin
  ListOfSerializedPropertiesToOIInteractionData(GenerateDifferentThanDefault_SetControlTextStr, @CSetTextProperties, CPropIsExp[acSetControlText], CPropCount_SetText, Properties);

  VerifyOIDifferentThanDefaultValues(CClkActionStr[acSetControlText], Properties);
end;


procedure TTestUI.TestVerifyOIDifferentThanDefaultValues_CallTemplate;
var
  Properties: TOIInteractionDataArr;
begin
  ListOfSerializedPropertiesToOIInteractionData(GenerateDifferentThanDefault_CallTemplateStr, @CCallTemplateProperties, CPropIsExp[acCallTemplate], CPropCount_CallTemplate, Properties);

  VerifyOIDifferentThanDefaultValues(CClkActionStr[acCallTemplate], Properties);
end;


procedure TTestUI.TestVerifyOIDifferentThanDefaultValues_Sleep;
var
  Properties: TOIInteractionDataArr;
begin
  ListOfSerializedPropertiesToOIInteractionData(GenerateDifferentThanDefault_SleepStr, @CSleepProperties, CPropIsExp[acSleep], CPropCount_Sleep, Properties);

  VerifyOIDifferentThanDefaultValues(CClkActionStr[acSleep], Properties);
end;


procedure TTestUI.TestVerifyOIDifferentThanDefaultValues_SetVar;
var
  Properties: TOIInteractionDataArr;
begin
  ListOfSerializedPropertiesToOIInteractionData(GenerateDifferentThanDefault_SetVarStr, @CSetVarProperties, CPropIsExp[acSetVar], CPropCount_SetVar, Properties);

  VerifyOIDifferentThanDefaultValues(CClkActionStr[acSetVar], Properties);
end;


procedure TTestUI.TestVerifyOIDifferentThanDefaultValues_WindowOperations;
var
  Properties: TOIInteractionDataArr;
begin
  ListOfSerializedPropertiesToOIInteractionData(GenerateDifferentThanDefault_WindowOperationsStr, @CWindowOperationsProperties, CPropIsExp[acWindowOperations], CPropCount_WindowOperations, Properties);

  VerifyOIDifferentThanDefaultValues(CClkActionStr[acWindowOperations], Properties);
end;


procedure TTestUI.TestVerifyOIDifferentThanDefaultValues_LoadSetVarFromFile;
var
  Properties: TOIInteractionDataArr;
begin
  ListOfSerializedPropertiesToOIInteractionData(GenerateDifferentThanDefault_LoadSetVarFromFileStr, @CLoadSetVarFromFileProperties, CPropIsExp[acLoadSetVarFromFile], CPropCount_LoadSetVarFromFile, Properties);

  VerifyOIDifferentThanDefaultValues(CClkActionStr[acLoadSetVarFromFile], Properties);
end;


procedure TTestUI.TestVerifyOIDifferentThanDefaultValues_SaveSetVarToFile;
var
  Properties: TOIInteractionDataArr;
begin
  ListOfSerializedPropertiesToOIInteractionData(GenerateDifferentThanDefault_SaveSetVarToFileStr, @CSaveSetVarToFileProperties, CPropIsExp[acSaveSetVarToFile], CPropCount_SaveSetVarToFile, Properties);

  VerifyOIDifferentThanDefaultValues(CClkActionStr[acSaveSetVarToFile], Properties);
end;


procedure TTestUI.TestVerifyOIDifferentThanDefaultValues_Plugin;
var
  Properties: TOIInteractionDataArr;
begin
  ListOfSerializedPropertiesToOIInteractionData(GenerateDifferentThanDefault_PluginStr, @CPluginProperties, CPropIsExp[acPlugin], CPropCount_Plugin, Properties);

  VerifyOIDifferentThanDefaultValues(CClkActionStr[acPlugin], Properties);
end;


procedure TTestUI.TestVerifyOIDifferentThanDefaultValues_EditTemplate;
var
  Properties: TOIInteractionDataArr;
begin
  ListOfSerializedPropertiesToOIInteractionData(GenerateDifferentThanDefault_EditTemplateStr, @CEditTemplateProperties, CPropIsExp[acEditTemplate], CPropCount_EditTemplate, Properties);

  VerifyOIDifferentThanDefaultValues(CClkActionStr[acEditTemplate], Properties);
end;


procedure TTestUI.EditOIPropertiesFromMenu(AActionToDrag, APropertyToEdit, ASubPropertyToEdit, AScrollCount, AMenuItemCaption, ANewPropertyValue: string);
var
  i: Integer;
begin
  TestServerAddress := CTestDriverServerAddress_Client;

  PrepareClickerUnderTestToReadItsVars;  //This is server mode. It is suitable to these tests only, because the actions do not have to be executed.

  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\PrepareClientActionsWindowForInteraction.clktmpl',
                              CREParam_FileLocation_ValueDisk);
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\DeleteAllActionsFromList.clktmpl', //this template depends on caching from above
                              CREParam_FileLocation_ValueDisk);

  SetVariableOnTestDriverClient('$ActionToDrag$', AActionToDrag);
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\DragActionToListOnAppUnderTest.clktmpl',
                              CREParam_FileLocation_ValueDisk,
                              '', //'$Control_Text$',
                              '' //AExpectedResult
                              );

  SetVariableOnTestDriverClient('$PropertyToEdit$', APropertyToEdit);
  SetVariableOnTestDriverClient('$SubPropertyToEdit$', ASubPropertyToEdit);
  SetVariableOnTestDriverClient('$ScrollCount$', AScrollCount);          //to get to $ActionToDrag$
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\EditProperty.clktmpl',
                              CREParam_FileLocation_ValueDisk,
                              '', //'$Control_Text$',
                              '' //AExpectedResult
                              );

  SetVariableOnTestDriverClient('$MenuItem$', AMenuItemCaption);
  SetVariableOnTestDriverClient('$NewPropertyValue$', ANewPropertyValue);
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\OpenOIEditorPopUpMenuFromRightClick.clktmpl',
                              CREParam_FileLocation_ValueDisk,
                              '', //'$Control_Text$',
                              '' //AExpectedResult
                              );
end;


procedure TTestUI.TestEditOIProperty_FindControl_InitialRectangle_Left_SetToValueFromMenu;
begin
  EditOIPropertiesFromMenu('FindControl', 'InitialRectangle', 'Left', '-5', '$Control_Right$', '$Control_Right$');
end;


procedure TTestUI.TestEditOIProperty_FindControl_InitialRectangle_Top_SetToValueFromMenu;
begin
  EditOIPropertiesFromMenu('FindControl', 'InitialRectangle', 'Top', '-5', '$Control_Bottom$', '$Control_Bottom$');
end;


procedure TTestUI.TestEditOIProperty_FindControl_InitialRectangle_Right_SetToValueFromMenu;
begin
  EditOIPropertiesFromMenu('FindControl', 'InitialRectangle', 'Right', '-5', '$Control_Left$', '$Control_Left$');
end;


procedure TTestUI.TestEditOIProperty_FindControl_InitialRectangle_Bottom_SetToValueFromMenu;
begin
  EditOIPropertiesFromMenu('FindControl', 'InitialRectangle', 'Bottom', '-5', '$Control_Top$', '$Control_Top$');
end;


procedure TTestUI.TestEditOIProperty_FindSubControl_InitialRectangle_Left_SetToValueFromMenu;
begin
  EditOIPropertiesFromMenu('FindSubControl', 'InitialRectangle', 'Left', '-5', '$Control_Right$', '$Control_Right$');
end;


procedure TTestUI.TestEditOIProperty_FindSubControl_InitialRectangle_Top_SetToValueFromMenu;
begin
  EditOIPropertiesFromMenu('FindSubControl', 'InitialRectangle', 'Top', '-5', '$Control_Bottom$', '$Control_Bottom$');
end;


procedure TTestUI.TestEditOIProperty_FindSubControl_InitialRectangle_Right_SetToValueFromMenu;
begin
  EditOIPropertiesFromMenu('FindSubControl', 'InitialRectangle', 'Right', '-5', '$Control_Left$', '$Control_Left$');
end;


procedure TTestUI.TestEditOIProperty_FindSubControl_InitialRectangle_Bottom_SetToValueFromMenu;
begin
  EditOIPropertiesFromMenu('FindSubControl', 'InitialRectangle', 'Bottom', '-5', '$Control_Top$', '$Control_Top$');
end;


procedure TTestUI.TestEditOIProperty_FindSubControl_MatchBitmapText_ForegroundColor_SetToValueFromMenu;
begin
  EditOIPropertiesFromMenu('FindSubControl', 'MatchBitmapText [0..0]', 'ForegroundColor', '-3', '$Color_ActiveCaption$', '$Color_ActiveCaption$');
end;


procedure TTestUI.TestEditOIProperty_FindSubControl_MatchBitmapText_BackgroundColor_SetToValueFromMenu;
begin
  EditOIPropertiesFromMenu('FindSubControl', 'MatchBitmapText [0..0]', 'BackgroundColor', '-3', '$Color_WindowFrame$', '$Color_WindowFrame$');
end;

///////////////////////////////

procedure TTestUI.VerifyPermissionsOnSendingFiles;
var
  i: Integer;
  AllowedExtensions, AllowedDirs: string;
  ListOfAllowedExtensions, ListOfAllowedDirs: TStringList;
begin
  TestServerAddress := CTestDriverServerAddress_Client;

  PrepareClickerUnderTestToReadItsVars;  //This is server mode. It is suitable to these tests only, because the actions do not have to be executed.

  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\PrepareClientActionsWindowForEditingSettings.clktmpl', CREParam_FileLocation_ValueDisk);
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\GetAllowedFileExtensionsFromSettings.clktmpl', CREParam_FileLocation_ValueDisk);

  AllowedExtensions := GetVarValueFromServer('$Control_Text$', 0);
  ListOfAllowedExtensions := TStringList.Create;
  try
    ListOfAllowedExtensions.Text := FastReplace_68ToReturn(AllowedExtensions);
    for i := 0 to ListOfAllowedExtensions.Count - 1 do
      ListOfAllowedExtensions.Strings[i] := ListOfAllowedExtensions.Strings[i] + '='; //convert items to keys, so they can be evaluated by WithItem

    Expect(ListOfAllowedExtensions).WithItem('.clktmpl');
    Expect(ListOfAllowedExtensions).WithItem('.bmp');
    Expect(ListOfAllowedExtensions).WithItem('.pmtv');
  finally
    ListOfAllowedExtensions.Free;
  end;

  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\PrepareClientActionsWindowForEditingSettings.clktmpl', CREParam_FileLocation_ValueDisk);
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\GetAllowedDirsFromSettings.clktmpl', CREParam_FileLocation_ValueDisk);

  AllowedDirs := GetVarValueFromServer('$Control_Text$', 0);
  ListOfAllowedDirs := TStringList.Create;
  try
    ListOfAllowedDirs.Text := FastReplace_68ToReturn(AllowedDirs);
    Expect(ListOfAllowedDirs.IndexOf('$AppDir$\Tests\DeniedTestFiles')).ToBe(-1);
  finally
    ListOfAllowedDirs.Free;
  end;

  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\PrepareClientActionsWindowForInteraction.clktmpl', CREParam_FileLocation_ValueDisk);
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\DeleteAllActionsFromList.clktmpl', CREParam_FileLocation_ValueDisk); //this template depends on caching from above
end;


procedure TTestUI.TestVerifyPermissionsOnSendingFiles_SendingFileFromDeniedTestFiles;
begin
  VerifyPermissionsOnSendingFiles;

  PrepareClickerUnderTestToClientMode;
  RunTestTemplateInClickerUnderTest_FullPath(ExtractFilePath(ParamStr(0)) + '..\TestFiles\ResetDeniedFileExecutedVar.clktmpl'); //this resets the var on both client and server
  PrepareClickerUnderTestToReadItsVars;

  ExpectVarFromClientUnderTest('$DeniedFileExecuted$', 'False', 'The variable should be reset.');

  PrepareClickerUnderTestToClientMode;
  RunTestTemplateInClickerUnderTest_FullPath(ExtractFilePath(ParamStr(0)) + '..\DeniedTestFiles\ADeniedFile.clktmpl');
  PrepareClickerUnderTestToReadItsVars;

  ExpectVarFromClientUnderTest('$DeniedFileExecuted$', 'False', 'The template should not be executed on server side.');
  ExpectVarFromClientUnderTest('$ExecAction_Err$', 'empty template', 'A second confirmation of attempting to execute an empty template.'); //this error message comes from server under test
end;


procedure TTestUI.TestVerifyPermissionsOnSendingFiles_ReSendingModifiedFileByEditTemplate;
begin
  VerifyPermissionsOnSendingFiles;

  PrepareClickerUnderTestToClientMode;
  RunTestTemplateInClickerUnderTest_FullPath(ExtractFilePath(ParamStr(0)) + '..\TestFiles\TestEditTemplateWithVars.clktmpl');
  PrepareClickerUnderTestToReadItsVars;

  ExpectVarFromClientUnderTest('$LastAction_Status$', 'Successful', 'The first run should be successful and leave the file modified by itself.');

  //Run again. This time, the template should stop, because the file is modified by itself, but it should not be reloaded automatically.

  PrepareClickerUnderTestToClientMode;
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\PrepareClientActionsWindowForInteraction.clktmpl', CREParam_FileLocation_ValueDisk);

  ExecuteTemplateOnTestDriver(FTemplatesDir + 'PlayAllActionsFromAppUnderTest.clktmpl', CREParam_FileLocation_ValueDisk); //run without reloading
  PrepareClickerUnderTestToReadItsVars;

  ExpectVarFromClientUnderTest('$LastAction_Status$', 'Failed', 'The second run should stop at first EditAction.');

  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\PrepareClientActionsWindowForInteraction.clktmpl', CREParam_FileLocation_ValueDisk);
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\DeleteAllActionsFromList.clktmpl', CREParam_FileLocation_ValueDisk); //this template depends on caching from above
end;


procedure TTestUI.AfterAll_AlwaysExecute;
begin
  //the following instances should be terminated in this specific order:
  FClientAppUnderTest_Proc.Terminate(0);
  FServerAppUnderTest_Proc.Terminate(0);
  FTestDriverForClient_Proc.Terminate(0);
  FTestDriverForServer_Proc.Terminate(0);

  FreeAndNil(FClientAppUnderTest_Proc);
  FreeAndNil(FServerAppUnderTest_Proc);
  FreeAndNil(FTestDriverForClient_Proc);
  FreeAndNil(FTestDriverForServer_Proc);
end;


initialization

  RegisterTest(TTestUI);
end.

