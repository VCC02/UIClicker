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
    procedure StartTestUtilities;
    procedure ExecuteTemplateOnTestDriver(ATemplatePath, AFileLocation: string; AAdditionalExpectedVar: string = ''; AAdditionalExpectedValue: string = '');
    procedure ArrangeMainUIClickerWindows;
    procedure ArrangeUIClickerActionWindows;
    procedure ArrangeWorkerWindows;

    procedure PrepareClickerUnderTestToReadItsVars;
    procedure PrepareClickerUnderTestToLocalMode;
    procedure LoadTestTemplateInClickerUnderTest_FullPath(ATestTemplate: string);
    procedure ExpectVarFromClientUnderTest(AVarName, AExpectedValue: string; AExtraComment: string = '');
    procedure ExpectWorkAtWorkerSide(const AWork: array of string; AExpectedUnreceivedWorkCount: Integer);
    procedure ExpectWorkAtPluginSide(const AWork: array of string; AExpectedUnreceivedWorkCount: Integer);
    procedure ExecutePluginTestTemplate_FullPath(ATemplatePath: string);
  public
    constructor Create; override;

  published
    procedure BeforeAll_AlwaysExecute;

    procedure Test_AllocationOfZeroFontProfiles_WinFontsOnly;
    procedure Test_AllocationOfOneFontProfile_WinFontsOnly;
    procedure Test_AllocationOfTwoFontProfiles_WinFontsOnly;
    procedure Test_AllocationOfThreeFontProfiles_WinFontsOnly;
    procedure Test_AllocationOfFourFontProfiles_WinFontsOnly;

    procedure AfterAll_AlwaysExecute;
  end;

implementation


uses
  UITestUtils, AsyncProcess, Forms, ClickerActionsClient, Expectations;


const
  CTestDriver_ServerPort_ForClientUnderTest = '25444';
  CClientUnderTestServerPort = '35444'; //this is a temporary mode, while UIClickerUITest reads the test results, then sets it back to client mode

  CWorkerClickerServerPort1 = '34444';
  CWorkerClickerServerPort2 = '44444';
  CWorkerClickerServerPort3 = '54444';
  CWorkerClickerServerPort4 = '24444';

  CTestClientAddress = 'http://127.0.0.1:' + CClientUnderTestServerPort + '/';                                //UIClicker-under-test client in server mode
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
  FServerForWorker1_Proc, FServerForWorker2_Proc, FServerForWorker3_Proc, FServerForWorker4_Proc: TAsyncProcess;
  CommonFonts_Proc: TAsyncProcess;
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
  DriverParams, AppUnderTestClientParams, ServerForWorkerParams: string;
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
  ServerForWorkerParams := '--SetExecMode Server Worker --ServerPort ';

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

    FServerForWorker1_Proc := CreateUIClickerProcess(PathToAppUnderTest, ServerForWorkerParams + CWorkerClickerServerPort1 + ' --ExtraCaption Worker1');
    FServerForWorker2_Proc := CreateUIClickerProcess(PathToAppUnderTest, ServerForWorkerParams + CWorkerClickerServerPort2 + ' --ExtraCaption Worker2');
    FServerForWorker3_Proc := CreateUIClickerProcess(PathToAppUnderTest, ServerForWorkerParams + CWorkerClickerServerPort3 + ' --ExtraCaption Worker3');
    FServerForWorker4_Proc := CreateUIClickerProcess(PathToAppUnderTest, ServerForWorkerParams + CWorkerClickerServerPort4 + ' --ExtraCaption Worker4');
  end;
end;


procedure TTestDistPlugin.StartAllWorkerInstances(AReportedOS: string = 'Win+Lin'; AReportedFonts: string = '');  //at least those from this machine
var
  PathToDistWorker: string;
begin
  //Other params: '--SetWorkerExtraName', '--SetWorkerExtraCaption', '--SetBrokerCredFile', '--SetBrokerAddress', '--SetBrokerPort'
  PathToDistWorker := ExtractFilePath(ParamStr(0)) + '..\..\..\UIClickerDistFindSubControlPlugin\Worker\FindSubControlWorker.exe';

  FWorker1_Proc := CreateUIClickerProcess(PathToDistWorker, '--SetReportedOS ' + AReportedOS + CSkipSavingWorkerSettings + ' --SetUIClickerPort ' + CWorkerClickerServerPort1 + ' --SetWorkerExtraName First --SetWorkerExtraCaption First');
  Sleep(500);
  FWorker2_Proc := CreateUIClickerProcess(PathToDistWorker, '--SetReportedOS ' + AReportedOS + CSkipSavingWorkerSettings + ' --SetUIClickerPort ' + CWorkerClickerServerPort2 + ' --SetWorkerExtraName Second --SetWorkerExtraCaption Second');
  Sleep(500);
  FWorker3_Proc := CreateUIClickerProcess(PathToDistWorker, '--SetReportedOS ' + AReportedOS + CSkipSavingWorkerSettings + ' --SetUIClickerPort ' + CWorkerClickerServerPort3 + ' --SetWorkerExtraName Third --SetWorkerExtraCaption Third');
  Sleep(500);
  FWorker4_Proc := CreateUIClickerProcess(PathToDistWorker, '--SetReportedOS ' + AReportedOS + CSkipSavingWorkerSettings + ' --SetUIClickerPort ' + CWorkerClickerServerPort4 + ' --SetWorkerExtraName Fourth --SetWorkerExtraCaption Fourth');
  Sleep(500);
end;


procedure TTestDistPlugin.StartTestUtilities;
var
  PathToCommonFonts: string;
begin
  PathToCommonFonts := ExtractFilePath(ParamStr(0)) + '..\..\..\UIClickerDistFindSubControlPlugin\Tests\CommonFonts\CommonFonts.exe';
  CommonFonts_Proc := CreateUIClickerProcess(PathToCommonFonts, '');
  Sleep(500);
end;


procedure TTestDistPlugin.ExecuteTemplateOnTestDriver(ATemplatePath, AFileLocation: string; AAdditionalExpectedVar: string = ''; AAdditionalExpectedValue: string = '');
begin
  ExecuteTemplateOnCustomTestDriver(CTestDriverServerAddress_Client, ATemplatePath, AFileLocation, AAdditionalExpectedVar, AAdditionalExpectedValue);
end;


procedure TTestDistPlugin.ArrangeMainUIClickerWindows;
begin
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\..\UIClickerDistFindSubControlPlugin\Tests\ArrangeMainUIClickerWindows.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestDistPlugin.ArrangeUIClickerActionWindows;
begin
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\..\UIClickerDistFindSubControlPlugin\Tests\ArrangeActionWindows.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestDistPlugin.ArrangeWorkerWindows;
begin
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + '..\..\..\UIClickerDistFindSubControlPlugin\Tests\ArrangeWorkerWindows.clktmpl', CREParam_FileLocation_ValueDisk);
end;


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


procedure TTestDistPlugin.ExpectVarFromClientUnderTest(AVarName, AExpectedValue: string; AExtraComment: string = '');
begin
  TestServerAddress := CTestClientAddress;
  try
    //connect to ClientUnderTest instance, which is now running in server mode and read its variables
    Expect(GetVarValueFromServer(AVarName)).ToBe(AExpectedValue, AExtraComment);
  finally
    TestServerAddress := CTestDriverServerAddress_Client; //restore
  end;
end;


type
  TBooleanArr = array of Boolean;

const
  CEmptyWorkTask = 'TxtCnt=0&BmpCnt=0&PmtvCnt=0&';

procedure ExpectWork(var AWorkersDbgInfo: TStringArray; const AWork: array of string; AExpectedUnreceivedWorkCount: Integer);
var
  FoundArr: TBooleanArr;
  FoundUnAllocatedCount, i, j: Integer;
begin
  SetLength(FoundArr, Length(AWork));
  for i := 0 to Length(FoundArr) - 1 do
    FoundArr[i] := False;

  FoundUnAllocatedCount := 0;
  for i := 0 to Length(AWorkersDbgInfo) - 1 do
  begin
    for j := 0 to Length(AWork) - 1 do
      if AWorkersDbgInfo[i] = AWork[j] then
        FoundArr[j] := True;

    if AWorkersDbgInfo[i] = CEmptyWorkTask then
      Inc(FoundUnAllocatedCount);
  end;

  Expect(FoundUnAllocatedCount).ToBe(AExpectedUnreceivedWorkCount, 'The number of workers, which received work, does not match the expected count: ' + IntToStr(AExpectedUnreceivedWorkCount) + '.');

  for j := 0 to Length(FoundArr) - 1 do
    Expect(FoundArr[j]).ToBe(True, 'A worker should get work for the task [' + IntToStr(j) + '].');
end;


procedure TTestDistPlugin.ExpectWorkAtWorkerSide(const AWork: array of string; AExpectedUnreceivedWorkCount: Integer);
var
  WorkersDbgInfo: TStringArray;
begin
  ExecuteTemplateOnTestDriver(FTemplatesDir + '..\..\..\UIClickerDistFindSubControlPlugin\Tests\GetAllocatedWorkFromAllWorkers.clktmpl', CREParam_FileLocation_ValueDisk);

  SetLength(WorkersDbgInfo, 4);
  WorkersDbgInfo[0] := GetVarValueFromServer('$Worker_First$');       //TestServerAddress is already set to CTestDriverServerAddress_Client;
  WorkersDbgInfo[1] := GetVarValueFromServer('$Worker_Second$');
  WorkersDbgInfo[2] := GetVarValueFromServer('$Worker_Third$');
  WorkersDbgInfo[3] := GetVarValueFromServer('$Worker_Fourth$');

  ExpectWork(WorkersDbgInfo, AWork, AExpectedUnreceivedWorkCount);
end;


procedure TTestDistPlugin.ExpectWorkAtPluginSide(const AWork: array of string; AExpectedUnreceivedWorkCount: Integer);
var
  WorkersDbgInfo: TStringArray;
  i: Integer;
begin
  TestServerAddress := CTestClientAddress;
  try
    SetLength(WorkersDbgInfo, 4); //4 workers
    for i := 0 to Length(WorkersDbgInfo) - 1 do
      WorkersDbgInfo[i] := GetVarValueFromServer('$Worker[' + IntToStr(i) + '].WorkerSpecificTask$');
  finally
    TestServerAddress := CTestDriverServerAddress_Client; //restore
  end;

  ExpectWork(WorkersDbgInfo, AWork, AExpectedUnreceivedWorkCount);
end;


procedure TTestDistPlugin.BeforeAll_AlwaysExecute;
begin
  StartAllUIClickerInstances;
  StartAllWorkerInstances;
  StartTestUtilities;

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
  ArrangeWorkerWindows;
  Sleep(500);

  FTemplatesDir := ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\ActionTemplates\';
end;


procedure TTestDistPlugin.ExecutePluginTestTemplate_FullPath(ATemplatePath: string);
begin
  PrepareClickerUnderTestToLocalMode;

  TestServerAddress := CTestDriverServerAddress_Client;
  LoadTestTemplateInClickerUnderTest_FullPath(ATemplatePath);
  ExecuteTemplateOnTestDriver(FTemplatesDir + 'PlayAllActionsFromAppUnderTest.clktmpl', CREParam_FileLocation_ValueDisk);
  PrepareClickerUnderTestToReadItsVars;
end;


procedure TTestDistPlugin.Test_AllocationOfZeroFontProfiles_WinFontsOnly;
begin
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\AllocateZeroFontProfiles.clktmpl');
  ExpectWorkAtPluginSide([], 4);
  ExpectWorkAtWorkerSide([], 4);
end;


procedure TTestDistPlugin.Test_AllocationOfOneFontProfile_WinFontsOnly;
begin
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\AllocateOneFontProfile.clktmpl');
  ExpectWorkAtPluginSide(['TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_0=1&'], 3);
  ExpectWorkAtWorkerSide(['TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_0=1&'], 3);
end;


procedure TTestDistPlugin.Test_AllocationOfTwoFontProfiles_WinFontsOnly;
begin
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\AllocateTwoFontProfiles.clktmpl');
  ExpectWorkAtPluginSide(['TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_0=1&', 'TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_1=1&'], 2);
  ExpectWorkAtWorkerSide(['TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_0=1&', 'TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_1=1&'], 2);
end;


procedure TTestDistPlugin.Test_AllocationOfThreeFontProfiles_WinFontsOnly;
begin
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\AllocateThreeFontProfiles.clktmpl');
  ExpectWorkAtPluginSide(['TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_0=1&', 'TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_1=1&', 'TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_2=1&'], 1);
  ExpectWorkAtWorkerSide(['TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_0=1&', 'TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_1=1&', 'TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_2=1&'], 1);
end;


procedure TTestDistPlugin.Test_AllocationOfFourFontProfiles_WinFontsOnly;
begin
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\AllocateFourFontProfiles.clktmpl');
  ExpectWorkAtPluginSide(['TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_0=1&', 'TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_1=1&', 'TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_2=1&', 'TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_3=1&'], 0);
  ExpectWorkAtWorkerSide(['TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_0=1&', 'TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_1=1&', 'TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_2=1&', 'TxtCnt=1&BmpCnt=0&PmtvCnt=0&Txt_3=1&'], 0);
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

  FServerForWorker1_Proc.Terminate(0);
  FServerForWorker2_Proc.Terminate(0);
  FServerForWorker3_Proc.Terminate(0);
  FServerForWorker4_Proc.Terminate(0);

  CommonFonts_Proc.Terminate(0);

  FreeAndNil(FClientAppUnderTest_Proc);
  FreeAndNil(FTestDriverForClient_Proc);

  FreeAndNil(FWorker1_Proc);
  FreeAndNil(FWorker2_Proc);
  FreeAndNil(FWorker3_Proc);
  FreeAndNil(FWorker4_Proc);

  FreeAndNil(FServerForWorker1_Proc);
  FreeAndNil(FServerForWorker2_Proc);
  FreeAndNil(FServerForWorker3_Proc);
  FreeAndNil(FServerForWorker4_Proc);

  FreeAndNil(CommonFonts_Proc);
end;


initialization

  RegisterTest(TTestDistPlugin);

end.

