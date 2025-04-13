{
    Copyright (C) 2025 VCC
    creation date: Apr 2025
    initial release date: 13 Apr 2025

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


unit TestDistPlugin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TestHTTPAPI, fpcunit, testregistry, UIActionsStuff,
  TestUI;

type
  TTestDistPlugin = class(TTestHTTPAPI)
  protected
    procedure StartAllUIClickerInstances;
    procedure StartAllWorkerInstances(AReportedOS: string = 'Win+Lin'; AReportedFonts: string = '');  //at least those from this machine
    procedure ExecuteTemplateOnTestDriver(ATemplatePath, AFileLocation: string; AAdditionalExpectedVar: string = ''; AAdditionalExpectedValue: string = '');

    procedure PrepareClickerUnderTestToReadItsVars;
    procedure PrepareClickerUnderTestToLocalMode;
    procedure LoadTestTemplateInClickerUnderTest_FullPath(ATestTemplate: string);
  public
    constructor Create; override;

  published
    procedure BeforeAll_AlwaysExecute;

    procedure Test_AllocationOfTwoFontProfiles_WinFontsOnly;

    procedure AfterAll_AlwaysExecute;
  end;

implementation


uses
  UITestUtils, AsyncProcess, Forms, ClickerActionsClient;


const
  CTestDriver_ServerPort_ForClientUnderTest = '25444';
  CClientUnderTestServerPort = '35444'; //this is a temporary mode, while UIClickerUITest reads the test results, then sets it back to client mode

  CWorkerClickerServerPort1 = '34444';
  CWorkerClickerServerPort2 = '44444';
  CWorkerClickerServerPort3 = '54444';
  CWorkerClickerServerPort4 = '24444';

  //CTestClientAddress = 'http://127.0.0.1:' + CClientUnderTestServerPort + '/';                                //UIClicker-under-test client in server mode
  CTestDriverServerAddress_Client = 'http://127.0.0.1:' + CTestDriver_ServerPort_ForClientUnderTest + '/';    //UIClicker driver

  //CWorkerClickerServerAddress1 = 'http://127.0.0.1:' + CWorkerClickerServerPort1 + '/';
  //CWorkerClickerServerAddress2 = 'http://127.0.0.1:' + CWorkerClickerServerPort2 + '/';
  //CWorkerClickerServerAddress3 = 'http://127.0.0.1:' + CWorkerClickerServerPort3 + '/';
  //CWorkerClickerServerAddress4 = 'http://127.0.0.1:' + CWorkerClickerServerPort4 + '/';


  CSkipSavingWorkerSettings: string = ' --SkipSavingIni Yes';

var
  FIsWine: Boolean;
  FTestDriverForClient_Proc, FClientAppUnderTest_Proc: TAsyncProcess;
  FWorker1_Proc, FWorker2_Proc, FWorker3_Proc, FWorker4_Proc: TAsyncProcess;
  FTemplatesDir: string;


constructor TTestDistPlugin.Create;
begin
  inherited Create;
  TestServerAddress := CTestServerAddress;
end;


procedure TTestDistPlugin.StartAllUIClickerInstances;
const
  CDisplayTabsOptions: string = ' --AutoSwitchToExecTab Yes --AutoEnableSwitchTabsOnDebugging Yes';
var
  PathToTestDriver, PathToAppUnderTest: string;
  DriverParams, AppUnderTestClientParams: string;
begin
  PathToTestDriver := ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\UIClicker.exe'; //this should be a stable version of UIClicker
  PathToAppUnderTest := ExtractFilePath(ParamStr(0)) + '..\..\UIClicker.exe';

  {$IFDEF UNIX}
    FIsWine := False;
  {$ELSE}
    FIsWine := {DirectoryExists('Z:\home') and} DirectoryExists('Z:\media') and DirectoryExists('Z:\etc'); //assume this is running on Wine
  {$ENDIF}

  DriverParams := '--SetExecMode Server --ExtraCaption Driver.Client --ServerPort ' + CTestDriver_ServerPort_ForClientUnderTest + CDisplayTabsOptions;
  AppUnderTestClientParams := '--SetExecMode Local --ExtraCaption ClientUnderTest' + CSkipSavingSettings + CDisplayTabsOptions;

  if FIsWine then
  begin
    SetUIClickerWindowPosition(ExtractFilePath(PathToTestDriver) + 'Clicker.ini', 1040, 50, 1000, 300);
    Sleep(100);
    FTestDriverForClient_Proc := CreateUIClickerProcess(PathToTestDriver, DriverParams + ' --UseWideStringsOnGetControlText Yes');
    Sleep(1000);

    SetUIClickerWindowPosition(ExtractFilePath(PathToAppUnderTest) + 'Clicker.ini', 360, 50, 30, 490);
    Sleep(100);
    FClientAppUnderTest_Proc := CreateUIClickerProcess(PathToAppUnderTest, AppUnderTestClientParams + ' --UseWideStringsOnGetControlText Yes');
    Sleep(1000);
  end
  else
  begin
    FTestDriverForClient_Proc := CreateUIClickerProcess(PathToTestDriver, DriverParams);
    FClientAppUnderTest_Proc := CreateUIClickerProcess(PathToAppUnderTest, AppUnderTestClientParams);
  end;
end;


procedure TTestDistPlugin.StartAllWorkerInstances(AReportedOS: string = 'Win+Lin'; AReportedFonts: string = '');  //at least those from this machine
var
  PathToDistWorker: string;
begin
  //Other params: '--SetWorkerExtraName', '--SetWorkerExtraCaption', '--SetBrokerCredFile', '--SetBrokerAddress', '--SetBrokerPort'
  PathToDistWorker := ExtractFilePath(ParamStr(0)) + '..\..\..\UIClickerDistFindSubControlPlugin\Worker\FindSubControlWorker.exe';

  FWorker1_Proc := CreateUIClickerProcess(PathToDistWorker, '--SetReportedOS ' + AReportedOS + CSkipSavingWorkerSettings + ' --SetUIClickerPort ' + CWorkerClickerServerPort1);
  Sleep(500);
  FWorker2_Proc := CreateUIClickerProcess(PathToDistWorker, '--SetReportedOS ' + AReportedOS + CSkipSavingWorkerSettings + ' --SetUIClickerPort ' + CWorkerClickerServerPort2);
  Sleep(500);
  FWorker3_Proc := CreateUIClickerProcess(PathToDistWorker, '--SetReportedOS ' + AReportedOS + CSkipSavingWorkerSettings + ' --SetUIClickerPort ' + CWorkerClickerServerPort3);
  Sleep(500);
  FWorker4_Proc := CreateUIClickerProcess(PathToDistWorker, '--SetReportedOS ' + AReportedOS + CSkipSavingWorkerSettings + ' --SetUIClickerPort ' + CWorkerClickerServerPort4);
  Sleep(500);
end;


procedure TTestDistPlugin.ExecuteTemplateOnTestDriver(ATemplatePath, AFileLocation: string; AAdditionalExpectedVar: string = ''; AAdditionalExpectedValue: string = '');
begin
  ExecuteTemplateOnCustomTestDriver(CTestDriverServerAddress_Client, ATemplatePath, AFileLocation, AAdditionalExpectedVar, AAdditionalExpectedValue);
end;


//ArrangeMainUIClickerWindows
//ArrangeUIClickerActionWindows


procedure SetVariableOnTestDriverClient(AVarName, AVarValue: string; AEvalVarBefore: Boolean = False);
begin
  SetVariableOnCustomTestDriverClient(CTestDriverServerAddress_Client, AVarName, AVarValue, AEvalVarBefore);
end;


procedure WaitForDriverStartup;
begin
  WaitForCustomDriverStartup(CTestDriverServerAddress_Client);
end;


procedure TTestDistPlugin.PrepareClickerUnderTestToReadItsVars;
begin
  PrepareCustomClickerUnderTestToReadItsVars(CTestDriverServerAddress_Client, CClientUnderTestServerPort, FTemplatesDir);
end;


procedure TTestDistPlugin.PrepareClickerUnderTestToLocalMode;
begin
  PrepareCustomClickerUnderTestToLocalMode(CTestDriverServerAddress_Client, FTemplatesDir);
end;


procedure TTestDistPlugin.LoadTestTemplateInClickerUnderTest_FullPath(ATestTemplate: string);
begin
  SetVariableOnTestDriverClient('$TemplateToLoad$', ATestTemplate);
  ExecuteTemplateOnTestDriver(FTemplatesDir + 'LoadCallerTemplateIntoAppUnderTest.clktmpl', CREParam_FileLocation_ValueDisk); //this loads BasicCaller into test client (the one which is in client mode)
end;


procedure TTestDistPlugin.BeforeAll_AlwaysExecute;
begin
  StartAllUIClickerInstances;
  StartAllWorkerInstances;

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

  //ArrangeMainUIClickerWindows;      //Setting window position from ini file, works on Wine. Setting from UIClicker does not (yet).
  //Sleep(500);                       //these sleep calls should be replaced by some waiting loops
  //ArrangeUIClickerActionWindows;
  //Sleep(500);

  FTemplatesDir := ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\';
end;


procedure TTestDistPlugin.Test_AllocationOfTwoFontProfiles_WinFontsOnly;
begin
  PrepareClickerUnderTestToLocalMode;

  TestServerAddress := CTestDriverServerAddress_Client;
  LoadTestTemplateInClickerUnderTest_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\AllocateTwoFontProfiles.clktmpl');
  PrepareClickerUnderTestToReadItsVars;
end;


procedure TTestDistPlugin.AfterAll_AlwaysExecute;
begin
  //the following instances should be terminated in this specific order:
  FClientAppUnderTest_Proc.Terminate(0);
  FTestDriverForClient_Proc.Terminate(0);

  FWorker1_Proc.Terminate(0);
  FWorker2_Proc.Terminate(0);
  FWorker3_Proc.Terminate(0);
  FWorker4_Proc.Terminate(0);

  FreeAndNil(FClientAppUnderTest_Proc);
  FreeAndNil(FTestDriverForClient_Proc);

  FreeAndNil(FWorker1_Proc);
  FreeAndNil(FWorker2_Proc);
  FreeAndNil(FWorker3_Proc);
  FreeAndNil(FWorker4_Proc);
end;


initialization

  RegisterTest(TTestDistPlugin);

end.

