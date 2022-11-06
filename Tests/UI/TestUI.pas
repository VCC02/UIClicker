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


unit TestUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestHTTPAPI, fpcunit, testregistry;

type

  TTestUI = class(TTestHTTPAPI)
  protected
    //procedure SetUp; override;
    //procedure TearDown; override;

    procedure StartAllUIClickerInstances;
    procedure ExecuteTemplateOnTestDriver(ATemplatePath, AFileLocation: string);
    procedure ArrangeMainUIClickerWindows;
    procedure ArrangeUIClickerActionWindows;
    procedure RunTestTemplateInClickerUnderTest(ATestTemplate: string);
    procedure RunTestTemplateInClickerUnderTestWithDebugging(ATestTemplate: string);
    procedure PrepareClickerUnderTestToReadItsVars;
  public
    constructor Create; override;
  published
    procedure BeforeAll_AlwaysExecute;

    procedure Test_ExecuteTemplateRemotely;
    procedure Test_ExecuteTemplateRemotely_WithLoopedCall;
    procedure Test_ExecuteTemplateRemotelyWithDebugging;
    procedure Test_ExecuteTemplateRemotelyWithDebugging_WithLoopedCall;

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


implementation


uses
  ClickerActionsClient, ClickerUtils, UIActionsStuff, AsyncProcess, Expectations, Forms;


var  //using global vars instead of class fields, to avoid recreating the objects on every test
  FTestDriverForServer_Proc, FTestDriverForClient_Proc, FServerAppUnderTest_Proc, FClientAppUnderTest_Proc: TAsyncProcess;
  FTemplatesDir: string;


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


procedure TTestUI.StartAllUIClickerInstances;
var
  PathToTestDriver, PathToAppUnderTest: string;
  ServerParams, ClientParams, AppUnderTestServerParams, AppUnderTestClientParams: string;
begin
  PathToTestDriver := ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\UIClicker.exe'; //this should be a stable version of UIClicker
  PathToAppUnderTest := ExtractFilePath(ParamStr(0)) + '..\..\UIClicker.exe';

  ServerParams := '--SetExecMode Server --ExtraCaption Driver.Server --ServerPort ' + CTestDriver_ServerPort_ForServerUnderTest;
  ClientParams := '--SetExecMode Server --ExtraCaption Driver.Client --ServerPort ' + CTestDriver_ServerPort_ForClientUnderTest;
  AppUnderTestServerParams := '--SetExecMode Server --ExtraCaption ServerUnderTest --ServerPort ' + CServerUnderTestServerPort;
  AppUnderTestClientParams := '--SetExecMode Client --ExtraCaption ClientUnderTest --ConnectsTo http://127.0.0.1:' + CServerUnderTestServerPort + '/ --SkipSavingSettings Yes';

  FTestDriverForServer_Proc := CreateUIClickerProcess(PathToTestDriver, ServerParams);
  FTestDriverForClient_Proc := CreateUIClickerProcess(PathToTestDriver, ClientParams);
  FServerAppUnderTest_Proc := CreateUIClickerProcess(PathToAppUnderTest, AppUnderTestServerParams);
  FClientAppUnderTest_Proc := CreateUIClickerProcess(PathToAppUnderTest, AppUnderTestClientParams);
end;


procedure TTestUI.ExecuteTemplateOnTestDriver(ATemplatePath, AFileLocation: string);
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

  Response := FastReplace_87ToReturn(ExecuteCallTemplateAction(CTestDriverServerAddress_Client, CallTemplateOptions, False, AFileLocation));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    try
      Expect(ListOfVars).WithItem('$ExecAction_Err$').OfValue('', 'No error Allowed in test driver.');
    except
      on E: Exception do
        Expect(ListOfVars).WithItem('$ExecAction_Err$').OfValue(CSecondExpectedErrMsg, 'No error Allowed in test driver.');
    end;
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
begin
  SetVarOptions.ListOfVarNames := AVarName;
  SetVarOptions.ListOfVarValues := AVarValue;
  SetVarOptions.ListOfVarEvalBefore := Chr(48 + Ord(AEvalVarBefore));
  ExecuteSetVarAction(CTestDriverServerAddress_Client, SetVarOptions);    //this is driver for client 25444
end;


procedure TTestUI.BeforeAll_AlwaysExecute;
begin
  StartAllUIClickerInstances;
  ArrangeMainUIClickerWindows;
  ArrangeUIClickerActionWindows;

  FTemplatesDir := ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\';
end;


procedure TTestUI.RunTestTemplateInClickerUnderTest(ATestTemplate: string);
begin
  SetVariableOnTestDriverClient('$TemplateToLoad$', FTemplatesDir + ATestTemplate);
  ExecuteTemplateOnTestDriver(FTemplatesDir + 'LoadCallerTemplateIntoAppUnderTest.clktmpl', CREParam_FileLocation_ValueDisk); //this loads BasicCaller into test client (the one which is in client mode)
  ExecuteTemplateOnTestDriver(FTemplatesDir + 'PlayAllActionsFromAppUnderTest.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestUI.RunTestTemplateInClickerUnderTestWithDebugging(ATestTemplate: string);
begin
  SetVariableOnTestDriverClient('$TemplateToLoad$', FTemplatesDir + ATestTemplate);
  ExecuteTemplateOnTestDriver(FTemplatesDir + 'LoadCallerTemplateIntoAppUnderTest.clktmpl', CREParam_FileLocation_ValueDisk); //this loads BasicCaller into test client (the one which is in client mode)
  ExecuteTemplateOnTestDriver(FTemplatesDir + 'PlayAllActionsFromAppUnderTestWithDebugging.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestUI.PrepareClickerUnderTestToReadItsVars;
begin
  //Set $ExtraCaption$ variable in Client Driver, to 'ClientUnderTest', which is required by SetExecutionModeOnAppUnderTest.clktmpl
  SetVariableOnTestDriverClient('$ExtraCaption$', 'ClientUnderTest');    //even after executing SetExecModeToServer.clktmpl, the caption should stay 'ClientUnderTest'
  SetVariableOnTestDriverClient('$ListeningPort$', CClientUnderTestServerPort);
  ExecuteTemplateOnTestDriver(FTemplatesDir + 'SetExecModeToServer.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestUI.Test_ExecuteTemplateRemotely;
begin
  TestServerAddress := CTestDriverServerAddress_Client;
  RunTestTemplateInClickerUnderTest('BasicCaller.clktmpl');
  PrepareClickerUnderTestToReadItsVars;

  Sleep(500);

  TestServerAddress := CTestClientAddress;
  try
    //connect to ClientUnderTest instance, which is now running in server mode and read its variables
    Expect(GetVarValueFromServer('$VarToBeIncremented$')).ToBe('1');  //this var is initialized to 0 in BasicCaller.clktmpl
  finally
    TestServerAddress := CTestDriverServerAddress_Client; //restore
    ExecuteTemplateOnTestDriver(FTemplatesDir + 'SetExecModeToClient.clktmpl', CREParam_FileLocation_ValueDisk);
  end;
end;


procedure TTestUI.Test_ExecuteTemplateRemotely_WithLoopedCall;
begin
  TestServerAddress := CTestDriverServerAddress_Client;
  RunTestTemplateInClickerUnderTest('BasicLoopedCaller.clktmpl');
  PrepareClickerUnderTestToReadItsVars;

  Sleep(500);

  TestServerAddress := CTestClientAddress;
  try
    //connect to ClientUnderTest instance, which is now running in server mode and read its variables
    Expect(GetVarValueFromServer('$VarToBeIncremented$')).ToBe('10');  //this var is initialized to 0 in BasicCaller.clktmpl
  finally
    TestServerAddress := CTestDriverServerAddress_Client; //restore
    ExecuteTemplateOnTestDriver(FTemplatesDir + 'SetExecModeToClient.clktmpl', CREParam_FileLocation_ValueDisk);
  end;
end;


procedure TTestUI.Test_ExecuteTemplateRemotelyWithDebugging;
begin
  TestServerAddress := CTestDriverServerAddress_Client;
  RunTestTemplateInClickerUnderTestWithDebugging('BasicCaller.clktmpl');
  PrepareClickerUnderTestToReadItsVars;

  Sleep(500);

  TestServerAddress := CTestClientAddress;
  try
    //connect to ClientUnderTest instance, which is now running in server mode and read its variables
    Expect(GetVarValueFromServer('$VarToBeIncremented$')).ToBe('1');  //this var is initialized to 0 in BasicCaller.clktmpl
  finally
    TestServerAddress := CTestDriverServerAddress_Client; //restore
    ExecuteTemplateOnTestDriver(FTemplatesDir + 'SetExecModeToClient.clktmpl', CREParam_FileLocation_ValueDisk);
  end;
end;


procedure TTestUI.Test_ExecuteTemplateRemotelyWithDebugging_WithLoopedCall;
begin
  TestServerAddress := CTestDriverServerAddress_Client;
  RunTestTemplateInClickerUnderTestWithDebugging('BasicLoopedCaller.clktmpl');
  PrepareClickerUnderTestToReadItsVars;

  Sleep(500);

  TestServerAddress := CTestClientAddress;
  try
    //connect to ClientUnderTest instance, which is now running in server mode and read its variables
    Expect(GetVarValueFromServer('$VarToBeIncremented$')).ToBe('10');  //this var is initialized to 0 in BasicCaller.clktmpl
  finally
    TestServerAddress := CTestDriverServerAddress_Client; //restore
    ExecuteTemplateOnTestDriver(FTemplatesDir + 'SetExecModeToClient.clktmpl', CREParam_FileLocation_ValueDisk);
  end;
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

