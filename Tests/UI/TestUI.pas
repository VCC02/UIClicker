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
  public
    constructor Create; override;
  published
    procedure BeforeAll_AlwaysExecute;

    procedure Test_ExecuteTemplateRemotely;

    procedure AfterAll_AlwaysExecute;
  end;

{
  This test application uses a second instance of UIClicker, called TestDriver, from TestDriver directory,
  which clicks and types on UIClicker-under-test, to test its UI.
  Depending on desired features, TestDriver may have to be up to date, but nonetheless, in an working state.
  When testing client-server mode, a second pair of UIClicker instances will be required.
  Depending on test, a single TestDriver may be enough to drive two UIClicker-under-test instances.

  This test app is expected to run from UIClicker\Tests\UI

  Possible connection diagram:

  UClickerUITest (this app)-+---- connects to ---> TestDriver_1(in server mode) ----- clicks on ----> UIClicker-Under-Test(in client mode or local mode)
                            |                      25444                                                  |
                            |                                                                             | connects to
                            |                                                                             |
                            |                                                                            \|/
                            +---- connects to ---> TestDriver_2(in server mode) ----- clicks on ----> UIClicker-Under-Test(in server mode)
                                                   15444                                              5444
}


const
  CTestDriver_ServerPort_ForServerUnderTest = '15444';
  CTestDriver_ServerPort_ForClientUnderTest = '25444';
  CServerUnderTestServerPort = '5444';

  CTestServerAddress = 'http://127.0.0.1:' + CServerUnderTestServerPort + '/';                                //UIClicker-under-test
  CTestDriverServerAddress_Server = 'http://127.0.0.1:' + CTestDriver_ServerPort_ForServerUnderTest + '/';    //UIClicker driver  (a different instance of UIClicker, which drives UIClicker-under-test)
  CTestDriverServerAddress_Client = 'http://127.0.0.1:' + CTestDriver_ServerPort_ForClientUnderTest + '/';    //UIClicker driver


implementation


uses
  ClickerActionsClient, ClickerUtils, UIActionsStuff, AsyncProcess, Expectations, Forms;


var  //using global vars instead of class fields, to avoid recreating the objects on every test
  FTestDriverForServer_Proc, FTestDriverForClient_Proc, FServerAppUnderTest_Proc, FClientAppUnderTest_Proc: TAsyncProcess;


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
  PathToTestDriver, TestDriverDir, PathToAppUnderTest: string;
  ServerParams, ClientParams, AppUnderTestServerParams, AppUnderTestClientParams: string;
begin
  PathToTestDriver := ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\UIClicker.exe'; //this should be a stable version of UIClicker
  TestDriverDir := ExtractFileDir(PathToTestDriver);
  PathToAppUnderTest := ExtractFilePath(ParamStr(0)) + '..\..\UIClicker.exe';

  ServerParams := '--SetExecMode Server --ExtraCaption Driver.Server --ServerPort ' + CTestDriver_ServerPort_ForServerUnderTest;
  ClientParams := '--SetExecMode Server --ExtraCaption Driver.Client --ServerPort ' + CTestDriver_ServerPort_ForClientUnderTest;
  AppUnderTestServerParams := '--SetExecMode Server --ExtraCaption ServerUnderTest --ServerPort ' + CServerUnderTestServerPort;
  AppUnderTestClientParams := '--SetExecMode Client --ExtraCaption ClientUnderTest';

  FTestDriverForServer_Proc := CreateUIClickerProcess(PathToTestDriver, ServerParams);
  FTestDriverForClient_Proc := CreateUIClickerProcess(PathToTestDriver, ClientParams);
  FServerAppUnderTest_Proc := CreateUIClickerProcess(PathToAppUnderTest, AppUnderTestServerParams);
  FClientAppUnderTest_Proc := CreateUIClickerProcess(PathToAppUnderTest, AppUnderTestClientParams);
end;


procedure TTestUI.ExecuteTemplateOnTestDriver(ATemplatePath, AFileLocation: string);
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
    Expect(ListOfVars).WithItem('$ExecAction_Err$').OfValue('', 'No error Allowed.');
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestUI.ArrangeMainUIClickerWindows;
begin
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\ArrangeMainUIClickerWindows.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestUI.ArrangeUIClickerActionWindows;
begin
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\ArrangeActionWindowsUnderTest.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestUI.BeforeAll_AlwaysExecute;
begin
  StartAllUIClickerInstances;
  ArrangeMainUIClickerWindows;
  ArrangeUIClickerActionWindows;
end;


procedure TTestUI.Test_ExecuteTemplateRemotely;
var
  Fnm: string;
  tk: QWord;
begin
  TestServerAddress := CTestDriverServerAddress_Client;
  //Fnm := ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\TemplateToBeCalled.clktmpl';
  //LoadTemplateFromDiskIntoInMemFS(Fnm, InMemFS);
  //SendTemplateFromInMemToServer(Fnm);

  //Fnm := ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\BasicCaller.clktmpl';   ///////////// template to load BasicCaller into test client
  //LoadTemplateFromDiskIntoInMemFS(Fnm, InMemFS);
  //SendTemplateFromInMemToServerThenLoad(Fnm);

  //Sleep(25000);
  tk := GetTickCount64;
  repeat
    Sleep(10);
    Application.ProcessMessages;
  until GetTickCount64 - tk > 25000;
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

