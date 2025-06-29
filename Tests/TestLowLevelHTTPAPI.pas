{
    Copyright (C) 2025 VCC
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


unit TestLowLevelHTTPAPI;

{$mode ObjFPC}{$H+}

interface

uses
  LCLIntf, Classes, SysUtils, TestHTTPAPI, fpcunit, testregistry, Expectations,
  ClickerUtils;


type

  TTestLowLevelHTTPAPI = class(TTestHTTPAPI)
  private
    procedure Test_FindSubControl_MultiFind(AFnm, AExpectedXOffsets, AExpectedYOffsets: string);
    function GetPluginPath: string;
    procedure Test_ExecutePlugin(APluginVarsAndValues, AExpectedErr: string);
    procedure Test_ExecuteEditTemplate(AActionType: TClkAction; AOperation: TEditTemplateOperation; var AExpectedValues: TStringArray);
    procedure CreateTheSecondPluginAction(var AExpectedValues: TStringArray);
    procedure Create_SaveTemplateButton_WithAndWithoutThreads_TestTemplateInMem(var FindControlOptions: TClkFindControlOptions; var WindowOperationsOptions: TClkWindowOperationsOptions; var FindSubControlOptions: TClkFindSubControlOptions);
    procedure Execute_UIClickerActions_SaveTemplateButton(AThreadCount, AThreadMessage: string; var FindControlOptions: TClkFindControlOptions; var WindowOperationsOptions: TClkWindowOperationsOptions; var FindSubControlOptions: TClkFindSubControlOptions);

    procedure Test_FindSubControl_RenderExternalBackground;
    procedure CloseRenderingServer;
  public
    constructor Create; override;
  published
    procedure Test_ExecuteClickAction_LeaveMouse;
    procedure Test_ExecuteClickAction_MouseWheel;
    procedure Test_ExecuteClickAction_TestApp_ClickDuration;
    procedure Test_ExecuteClickAction_TestApp_DragDuration_WithDraggingAction;
    procedure Test_ExecuteClickAction_TestApp_DragDuration_WithMouseDownMoveUpActions;
    procedure Test_ExecuteExecAppAction_IPConfig;
    procedure Test_ExecuteExecAppAction_IPConfig_NoInheritHandles;

    procedure Test_ExecuteFindControlAction_UIClickerMain;
    procedure Test_ExecuteFindControlAction_UIClickerMain_WrongClass;
    procedure Test_ExecuteFindControlAction_UIClickerMain_WrongClassAllowToFail;

    procedure Test_ExecuteFindControlAction_UIClickerMain_WhileExecutingSleep;
    procedure Test_ExecuteFindControlAction_UIClickerMain_WhileExecutingFindControl_Actions;
    procedure Test_ExecuteFindControlAction_UIClickerMain_WhileExecutingFindControl_Actions_WhileExecutingSleep;

    procedure Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabel;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithCropping;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithBadCropping;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithBadCropping_Width0;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithBadCropping_NegativeWidth;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithBadCropping_Height0;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithBadCropping_NegativeHeight;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabel_WithFastSearch;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabel_WithFastSearchAndSleepySearch;
    procedure Test_ExecuteFindSubControlAction_UIClickerActions_SaveTemplateButton_WithAndWithoutThreads;
    procedure Test_ExecuteFindSubControlAction_UIClickerActions_SaveTemplateButton_WithAndWithoutThreads_AsTemplate;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_CustomLabel_IgnoreBG;

    procedure Test_ExecuteFindSubControlAction_UIClickerMain_WindowInterpreterButton_Disk;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_WindowInterpreterButton_Mem_NoSender;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_PmtvPreviewButton_Disk;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_PmtvPreviewButton_Mem;
    procedure Test_ExecuteFindSubControlAction_RenderingServer_PmtvGradientWithText;
    procedure Test_ExecuteFindSubControlAction_RenderingServer_PmtvGradientOnBrowser;

    procedure Test_ExecuteFindSubControlAction_RenderingServer_MultiText;
    procedure Test_ExecuteFindSubControlAction_RenderingServer_MultiTextWithTwoProfiles;
    procedure Test_ExecuteFindSubControlAction_RenderingServer_MultiBmp;
    procedure Test_ExecuteFindSubControlAction_RenderingServer_MultiBmpWithTwoProfiles;
    procedure Test_ExecuteFindSubControlAction_RenderingServer_MultiPmtv;
    procedure Test_ExecuteFindSubControlAction_RenderingServer_MultiPmtvWithTwoProfiles;
    procedure Test_ExecuteFindSubControlAction_RenderingServer_MultiTextWithTwoProfilesAndOneBmp;
    procedure Test_ExecuteFindSubControlAction_RenderingServer_MultiTextWithTwoProfilesAndOnePmtv;
    procedure Test_ExecuteFindSubControlAction_RenderingServer_MultiTextWithTwoProfilesAndOneBmpAndOnePmtv;
    procedure Test_ExecuteFindSubControlAction_RenderingServer_MultiTextWithTwoProfilesAndTextAndOneBmpAndOnePmtv;
    procedure Test_ExecuteFindSubControlAction_RenderingServer_MultiTextWithTwoProfilesAndTextAndPmtvWithTwoOrders;

    procedure Test_FindSubControl_ExternalBackground_isflDisk;
    procedure Test_FindSubControl_ExternalBackground_isflMem;

    procedure Test_FindSubControl_PrecisionTimeout;
    procedure Test_FindSubControl_CropFromScreenshot;

    procedure Test_ExecuteSetControlTextAction_HappyFlow;
    procedure Test_ExecuteCallTemplate_HappyFlow;
    procedure Test_ExecuteCallTemplateWithSubTemplate_HappyFlow;
    procedure Test_ExecuteLoopedCallTemplate_HappyFlow;

    procedure Test_ExecuteSleep_HappyFlow;
    procedure Test_ExecuteSleep_NegativeValue;
    procedure Test_ExecuteSetVar_HappyFlow_NoEval;
    procedure Test_ExecuteSetVar_HappyFlow_NoEvalWithVarName;
    procedure Test_ExecuteSetVar_HappyFlow_EvalBefore;

    procedure Test_ExecutePlugin_EmptyFileName;
    procedure Test_ExecutePlugin_BadPlugin_ValidExe;
    procedure Test_ExecutePlugin_BadPlugin_InvalidExe;
    procedure Test_ExecutePlugin_EmptyListOfVars_EmptyTemplate;
    procedure Test_ExecutePlugin_EmptyListOfVars_WithDummyActions;
    procedure Test_ExecutePlugin_ValidVarsSetToNonExistentActions_EmptyTemplate;
    procedure Test_ExecutePlugin_ValidVarsSetToNonExistentActions_WithDummyActions;

    procedure Test_ExecuteEditTemplate_UpdateAction_Click_HappyFlow;
    procedure Test_ExecuteEditTemplate_UpdateAction_ExecApp_HappyFlow;
    procedure Test_ExecuteEditTemplate_UpdateAction_FindControl_HappyFlow;
    procedure Test_ExecuteEditTemplate_UpdateAction_FindSubControl_HappyFlow;
    procedure Test_ExecuteEditTemplate_UpdateAction_SetControlText_HappyFlow;
    procedure Test_ExecuteEditTemplate_UpdateAction_CallTemplate_HappyFlow;
    procedure Test_ExecuteEditTemplate_UpdateAction_Sleep_HappyFlow;
    procedure Test_ExecuteEditTemplate_UpdateAction_SetVar_HappyFlow;
    procedure Test_ExecuteEditTemplate_UpdateAction_WindowOperations_HappyFlow;
    procedure Test_ExecuteEditTemplate_UpdateAction_LoadSetVarFromFile_HappyFlow;
    procedure Test_ExecuteEditTemplate_UpdateAction_SaveSetVarToFile_HappyFlow;
    procedure Test_ExecuteEditTemplate_UpdateAction_Plugin_HappyFlow;
    procedure Test_ExecuteEditTemplate_UpdateAction_TwoUpdatedPlugins_HappyFlow;
    procedure Test_ExecuteEditTemplate_UpdateAction_EditTemplate_HappyFlow;
  end;


implementation


uses
  ClickerActionsClient, ActionsStuff, Controls, ClickerFileProviderClient, ClickerActionProperties,
  Graphics;


constructor TTestLowLevelHTTPAPI.Create;
begin
  inherited Create;
  TestServerAddress := CTestServerAddress;
end;


function TTestLowLevelHTTPAPI.GetPluginPath: string;
begin
  Result := '$AppDir$\..\UIClickerFindWindowsPlugin\lib\' + GetPluginBitnessDirName + '\UIClickerFindWindows.dll';
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteClickAction_LeaveMouse;
const
  CX: Integer = 20;
  CY: Integer = 30;
var
  Response: string;
  ClickOptions: TClkClickOptions;
  tp, InitialTp: TPoint;
begin
  GenerateClickOptionsForLeaveMouse(CX, CY, ClickOptions);
  GetCursorPos(InitialTp);

  Response := FastReplace_87ToReturn(ExecuteClickAction(TestServerAddress, ClickOptions));

  GetCursorPos(tp);
  Expect(tp.X).ToBe(CX, 'mouse pos');
  Expect(tp.Y).ToBe(CY, 'mouse pos');

  try
    ExpectSuccessfulAction(Response);
  finally
    SetCursorPos(InitialTp.X, InitialTp.Y);
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteClickAction_MouseWheel;
var
  Response: string;
  CallTemplateOptions: TClkCallTemplateOptions;
begin
  GenerateCallTemplateOptions(CallTemplateOptions, '$AppDir$\Tests\TestFiles\MouseWheelOnWinInterp.clktmpl', '', False);
  Response := FastReplace_87ToReturn(ExecuteCallTemplateAction(TestServerAddress, CallTemplateOptions, False, False, CREParam_FileLocation_ValueDisk));

  ExpectSuccessfulAction(Response);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteClickAction_TestApp_ClickDuration;
begin                                         //This test should be modified, to execute in-mem actions, i.e. Click, via API.
                                              //It should generate some options similar to GenerateClickOptionsForLeaveMouse and modify them for this test.
  ExecTestTemplate(TestServerAddress, '$AppDir$\Tests\TestFiles\ClickMeasurements.clktmpl');
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteClickAction_TestApp_DragDuration_WithDraggingAction;
begin                                         //This test should be modified, to execute in-mem actions, i.e. Click, via API.
                                              //It should generate some options similar to GenerateClickOptionsForLeaveMouse and modify them for this test.
  SetVariable(TestServerAddress, '$DragMouseCursor$', '1', 0);    //using mouse cursor dragging in one action
  ExecTestTemplate(TestServerAddress, '$AppDir$\Tests\TestFiles\DragMeasurements.clktmpl');
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteClickAction_TestApp_DragDuration_WithMouseDownMoveUpActions;
begin                                         //This test should be modified, to execute in-mem actions, i.e. Click, via API.
                                              //It should generate some options similar to GenerateClickOptionsForLeaveMouse and modify them for this test.
  SetVariable(TestServerAddress, '$DragMouseCursor$', '0', 0);    //using mouse cursor dragging in one action
  ExecTestTemplate(TestServerAddress, '$AppDir$\Tests\TestFiles\DragMeasurements.clktmpl');
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteExecAppAction_IPConfig;
var
  Response: string;
  ListOfVars: TStringList;
  ExecAppOptions: TClkExecAppOptions;
begin
  GenerateExecAppOptionsForIPConfig(ExecAppOptions);
  ExecAppOptions.UseInheritHandles := uihYes;

  Response := FastReplace_87ToReturn(ExecuteExecAppAction(TestServerAddress, ExecAppOptions, 'TestExec', 1000));

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


procedure TTestLowLevelHTTPAPI.Test_ExecuteExecAppAction_IPConfig_NoInheritHandles;
var
  Response: string;
  ListOfVars: TStringList;
  ExecAppOptions: TClkExecAppOptions;
begin
  GenerateExecAppOptionsForIPConfig(ExecAppOptions);
  Response := FastReplace_87ToReturn(ExecuteExecAppAction(TestServerAddress, ExecAppOptions, 'TestExec', 1000));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    ExpectSuccessfulAction(ListOfVars);
    Expect(ListOfVars).WithItem('$ExecAction_StdOut$').OfValue('');
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindControlAction_UIClickerMain;
var
  Response: string;
  FindControlOptions: TClkFindControlOptions;
begin
  GenerateFindControlOptionsForMainUIClickerWindow(FindControlOptions, False);
  Response := FastReplace_87ToReturn(ExecuteFindControlAction(TestServerAddress, FindControlOptions, 'TestFind UIClicker Main', 1000, CREParam_FileLocation_ValueMem));

  ExpectSuccessfulAction(Response);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindControlAction_UIClickerMain_WrongClass;
var
  Response: string;
  ListOfVars: TStringList;
  FindControlOptions: TClkFindControlOptions;
begin
  GenerateFindControlOptionsForMainUIClickerWindow(FindControlOptions, False);
  FindControlOptions.MatchClassName := 'non-existent name';
  Response := FastReplace_87ToReturn(ExecuteFindControlAction(TestServerAddress, FindControlOptions, 'TestFind UIClicker Main', 1000, CREParam_FileLocation_ValueMem));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    ExpectFailedAction(ListOfVars, 'Timeout at "TestFind UIClicker Main" in ');
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindControlAction_UIClickerMain_WrongClassAllowToFail;
var
  Response: string;
  ListOfVars: TStringList;
  FindControlOptions: TClkFindControlOptions;
begin
  GenerateFindControlOptionsForMainUIClickerWindow(FindControlOptions, True);
  FindControlOptions.MatchClassName := 'non-existent name';
  Response := FastReplace_87ToReturn(ExecuteFindControlAction(TestServerAddress, FindControlOptions, 'TestFind UIClicker Main', 1000, CREParam_FileLocation_ValueMem));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    ExpectAllowedFailedAction(ListOfVars);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindControlAction_UIClickerMain_WhileExecutingSleep;
var
  Response: string;
  MainFindControlOptions: TClkFindControlOptions;
  SleepOptions: TClkSleepOptions;
  Th: TClientThread;
begin
  GenerateFindControlOptionsForMainUIClickerWindow(MainFindControlOptions, False);
  SleepOptions.Value := '2000';

  Th := AsyncExecuteSleepAction(TestServerAddress, SleepOptions, 'Long sleep', True, False);
  try
    Response := FastReplace_87ToReturn(ExecuteFindControlAction(TestServerAddress, MainFindControlOptions, 'TestFind UIClicker Main', 2000, CREParam_FileLocation_ValueMem));
    ExpectSuccessfulAction(Response);

    WaitForServerResponse(Th, True);
    ExpectSuccessfulAction(FastReplace_87ToReturn(Th.Result));
  finally
    Th.Free;
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindControlAction_UIClickerMain_WhileExecutingFindControl_Actions;
var                                                     //This test was used to verify a failed action. Hhowever, there are race conditions on setting exec results in UIClicker.
  Response: string;
  MainFindControlOptions: TClkFindControlOptions;
  ActionsFindControlOptions: TClkFindControlOptions;
  Th: TClientThread;
begin
  GenerateFindControlOptionsForMainUIClickerWindow(MainFindControlOptions, False);
  GetDefaultPropertyValues_FindControl(ActionsFindControlOptions);
  ActionsFindControlOptions.MatchText := 'UI Clicker Actions';
  ActionsFindControlOptions.MatchClassName := 'Window';

  Th := AsyncExecuteFindControlAction(TestServerAddress, ActionsFindControlOptions, 'Actions FindControl', 3000, CREParam_FileLocation_ValueMem, True, False);
  try
    Sleep(500); //this should not be needed after implementing exec serialization
    Response := FastReplace_87ToReturn(ExecuteFindControlAction(TestServerAddress, MainFindControlOptions, 'TestFind UIClicker Main', 2000, CREParam_FileLocation_ValueMem));
    ExpectSuccessfulAction(Response);

    WaitForServerResponse(Th, True);
    ExpectSuccessfulAction(FastReplace_87ToReturn(Th.Result), 'Actions');
  finally
    Th.Free;
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindControlAction_UIClickerMain_WhileExecutingFindControl_Actions_WhileExecutingSleep;
var                                                //Still flaky. It depends on how fast the first ExecuteFindControlAction finds the window.
  Response: string;                                //The test should be fine after implementing execution serialization.
  MainFindControlOptions: TClkFindControlOptions;
  ActionsFindControlOptions: TClkFindControlOptions;
  SleepOptions: TClkSleepOptions;
  Th, SleepTh: TClientThread;
begin
  GenerateFindControlOptionsForMainUIClickerWindow(MainFindControlOptions, False);
  GetDefaultPropertyValues_FindControl(ActionsFindControlOptions);
  ActionsFindControlOptions.MatchText := 'UI Clicker Actions';
  ActionsFindControlOptions.MatchClassName := 'Window';
  SleepOptions.Value := '5000';

  SleepTh := AsyncExecuteSleepAction(TestServerAddress, SleepOptions, 'Long sleep', True, False);
  try
    Th := AsyncExecuteFindControlAction(TestServerAddress, ActionsFindControlOptions, 'Actions FindControl', 4000, CREParam_FileLocation_ValueMem, True, False);
    try
      Sleep(500);  //this should not be needed after implementing exec serialization
      Response := FastReplace_87ToReturn(ExecuteFindControlAction(TestServerAddress, MainFindControlOptions, 'TestFind UIClicker Main', 3000, CREParam_FileLocation_ValueMem));
      ExpectSuccessfulAction(Response, '.Find.Main');

      WaitForServerResponse(Th, True);
      ExpectSuccessfulAction(FastReplace_87ToReturn(Th.Result), '.Find.Actions');    //the error status might be cleared, so a failed action may not be expected
    finally
      Th.Free;
    end;

    WaitForServerResponse(SleepTh, True);
    ExpectSuccessfulAction(FastReplace_87ToReturn(SleepTh.Result), '.Sleep.');
  finally
    SleepTh.Free;
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabel;
var
  Response: string;
  FindSubControlOptions: TClkFindSubControlOptions;
begin
  SetupTargetWindowFor_FindSubControl;
  GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness(FindSubControlOptions, False);
  Response := FastReplace_87ToReturn(ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'Test Find Bitness on UIClicker Main', 3000, CREParam_FileLocation_ValueMem));

  ExpectSuccessfulAction(Response);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithCropping;
var
  Response: string;
  FindSubControlOptions: TClkFindSubControlOptions;
begin
  SetupTargetWindowFor_FindSubControl;
  GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness(FindSubControlOptions, False);
  FindSubControlOptions.MatchBitmapText[0].CropLeft := '3';
  FindSubControlOptions.MatchBitmapText[0].CropTop := '1';
  FindSubControlOptions.MatchBitmapText[0].CropRight := '4';
  FindSubControlOptions.MatchBitmapText[0].CropBottom := '2';

  Response := FastReplace_87ToReturn(ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'Test Find Bitness on UIClicker Main', 3000, CREParam_FileLocation_ValueMem));
  ExpectSuccessfulAction(Response);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithBadCropping;
var
  Response: string;
  ListOfVars: TStringList;
  FindSubControlOptions: TClkFindSubControlOptions;
begin
  SetupTargetWindowFor_FindSubControl; //this should set the execution status to "Successful", then the next call, should set it to "Failed".
  GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness(FindSubControlOptions, False);
  FindSubControlOptions.MatchBitmapText[0].CropLeft := '3';
  FindSubControlOptions.MatchBitmapText[0].CropTop := '-14';
  FindSubControlOptions.MatchBitmapText[0].CropRight := '4';
  FindSubControlOptions.MatchBitmapText[0].CropBottom := '2';

  Response := FastReplace_87ToReturn(ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'Test Find Bitness on UIClicker Main', 3000, CREParam_FileLocation_ValueMem));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    ExpectFailedAction(ListOfVars, 'MatchBitmapText[0].CropTop is out of range.');
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithBadCropping_Width0;
var
  Response: string;
  FindSubControlOptions: TClkFindSubControlOptions;
begin
  SetupTargetWindowFor_FindSubControl; //this should set the execution status to "Successful", then the next call, should set it to "Failed".
  GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness(FindSubControlOptions, False);
  FindSubControlOptions.MatchBitmapText[0].CropLeft := '9';
  FindSubControlOptions.MatchBitmapText[0].CropTop := '0';
  FindSubControlOptions.MatchBitmapText[0].CropRight := '10';
  FindSubControlOptions.MatchBitmapText[0].CropBottom := '0';

  Response := FastReplace_87ToReturn(ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'Test Find Bitness on UIClicker Main', 3000, CREParam_FileLocation_ValueMem));

  Expect(Response).ToBe('ProcessServerCommand exception: The text width, after cropping, is 0.  Profile[0]: "' + FindSubControlOptions.MatchBitmapText[0].ProfileName + '".   Searched text: "-bit"');
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithBadCropping_NegativeWidth;
var
  Response: string;
  FindSubControlOptions: TClkFindSubControlOptions;
begin
  SetupTargetWindowFor_FindSubControl; //this should set the execution status to "Successful", then the next call, should set it to "Failed".
  GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness(FindSubControlOptions, False);
  FindSubControlOptions.MatchBitmapText[0].CropLeft := '15';
  FindSubControlOptions.MatchBitmapText[0].CropTop := '0';
  FindSubControlOptions.MatchBitmapText[0].CropRight := '16';
  FindSubControlOptions.MatchBitmapText[0].CropBottom := '0';

  Response := FastReplace_87ToReturn(ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'Test Find Bitness on UIClicker Main', 3000, CREParam_FileLocation_ValueMem));

  Expect(Response).ToBe('ProcessServerCommand exception: The text width, after cropping, is negative.  Profile[0]: "' + FindSubControlOptions.MatchBitmapText[0].ProfileName + '".   Searched text: "-bit"');
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithBadCropping_Height0;
var
  Response: string;
  FindSubControlOptions: TClkFindSubControlOptions;
begin
  SetupTargetWindowFor_FindSubControl; //this should set the execution status to "Successful", then the next call, should set it to "Failed".
  GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness(FindSubControlOptions, False);
  FindSubControlOptions.MatchBitmapText[0].CropLeft := '0';
  FindSubControlOptions.MatchBitmapText[0].CropTop := '8';
  FindSubControlOptions.MatchBitmapText[0].CropRight := '0';
  FindSubControlOptions.MatchBitmapText[0].CropBottom := '7';

  Response := FastReplace_87ToReturn(ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'Test Find Bitness on UIClicker Main', 3000, CREParam_FileLocation_ValueMem));

  Expect(Response).ToBe('ProcessServerCommand exception: The text height, after cropping, is 0.  Profile[0]: "' + FindSubControlOptions.MatchBitmapText[0].ProfileName + '".   Searched text: "-bit"');
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithBadCropping_NegativeHeight;
var
  Response: string;
  FindSubControlOptions: TClkFindSubControlOptions;
begin
  SetupTargetWindowFor_FindSubControl; //this should set the execution status to "Successful", then the next call, should set it to "Failed".
  GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness(FindSubControlOptions, False);
  FindSubControlOptions.MatchBitmapText[0].CropLeft := '0';
  FindSubControlOptions.MatchBitmapText[0].CropTop := '9';
  FindSubControlOptions.MatchBitmapText[0].CropRight := '0';
  FindSubControlOptions.MatchBitmapText[0].CropBottom := '9';

  Response := FastReplace_87ToReturn(ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'Test Find Bitness on UIClicker Main', 3000, CREParam_FileLocation_ValueMem));

  Expect(Response).ToBe('ProcessServerCommand exception: The text height, after cropping, is negative.  Profile[0]: "' + FindSubControlOptions.MatchBitmapText[0].ProfileName + '".   Searched text: "-bit"');
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabel_WithFastSearch;
const
  CExpectedMultiplier = 0.5; //without SleepySearch, there is not much difference
var
  tk: QWord;
  FirstDuration, SecondDuration: QWord;
begin                                         //This test should be modified, to execute in-mem actions, i.e. FindSubControl, via API. Only the FindSubControl execution time should be measured.
  tk := GetTickCount64;                       //It should generate some options with GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness and modify them for this test.
  ExecTestTemplate(TestServerAddress, '$AppDir$\Tests\TestFiles\FindBitOnMainNoFastSearch.clktmpl');
  FirstDuration := GetTickCount64 - tk;

  tk := GetTickCount64;
  ExecTestTemplate(TestServerAddress, '$AppDir$\Tests\TestFiles\FindBitOnMainWithFastSearch.clktmpl');
  SecondDuration := GetTickCount64 - tk;

  Expect(DWord(FirstDuration)).ToBeGreaterThan(Round(CExpectedMultiplier * SecondDuration), 'Expecting some performance gain.  ' + IntToStr(FirstDuration) + ' vs. ' + FloatToStr(CExpectedMultiplier) + ' * ' + IntToStr(SecondDuration));
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabel_WithFastSearchAndSleepySearch;
const
  CExpectedMultiplier = 16;
var
  tk: QWord;
  FirstDuration, SecondDuration: QWord;
begin                                         //This test should be modified, to execute in-mem actions, i.e. FindSubControl, via API. Only the FindSubControl execution time should be measured.
  tk := GetTickCount64;                       //It should generate some options with GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness and modify them for this test.
  ExecTestTemplate(TestServerAddress, '$AppDir$\Tests\TestFiles\FindBitOnMainNoFastSearchWithSleepySearch.clktmpl');
  FirstDuration := GetTickCount64 - tk;

  tk := GetTickCount64;
  ExecTestTemplate(TestServerAddress, '$AppDir$\Tests\TestFiles\FindBitOnMainWithFastSearchWithSleepySearch.clktmpl');
  SecondDuration := GetTickCount64 - tk;

  Expect(DWord(FirstDuration)).ToBeGreaterThan(CExpectedMultiplier * SecondDuration, 'Expecting some performance gain.  ' + IntToStr(FirstDuration) + ' vs. ' + IntToStr(CExpectedMultiplier) + ' * ' + IntToStr(SecondDuration));
end;


const
  //CTestFileName_FindSaveTemplateButtonAsSubControl = 'FindSaveTemplateButtonAsSubControl.clktmpl';
  CThreadCount_VarName = '$ThreadCount$';
  CFindSubControlActionDuration_VarName = '$ActionExecDuration$';

procedure TTestLowLevelHTTPAPI.Create_SaveTemplateButton_WithAndWithoutThreads_TestTemplateInMem(var FindControlOptions: TClkFindControlOptions; var WindowOperationsOptions: TClkWindowOperationsOptions; var FindSubControlOptions: TClkFindSubControlOptions);
begin
  GetDefaultPropertyValues_FindControl(FindControlOptions);
  FindControlOptions.MatchText := 'UI Clicker Actions';
  FindControlOptions.MatchClassName := 'Window';

  GetDefaultPropertyValues_WindowOperations(WindowOperationsOptions);

  GetDefaultPropertyValues_FindSubControl(FindSubControlOptions);
  FindSubControlOptions.MatchText := 'Save Template'; //looking for the Save Template button
  SetLength(FindSubControlOptions.MatchBitmapText, 3);

  FindSubControlOptions.MatchBitmapText[0].ForegroundColor := '$Color_WindowText$';
  FindSubControlOptions.MatchBitmapText[0].BackgroundColor := 'EAEAEA';
  FindSubControlOptions.MatchBitmapText[0].FontName := 'Tahoma';
  FindSubControlOptions.MatchBitmapText[0].FontSize := 8;
  FindSubControlOptions.MatchBitmapText[0].FontQuality := fqNonAntialiased;
  FindSubControlOptions.MatchBitmapText[0].IgnoreBackgroundColor := True;

  GetDefaultPropertyValues_FindControl_MatchBitmapText(FindSubControlOptions.MatchBitmapText[1]);
  GetDefaultPropertyValues_FindControl_MatchBitmapText(FindSubControlOptions.MatchBitmapText[2]);
  FindSubControlOptions.MatchBitmapText[1].FontQuality := fqAntialiased;
  FindSubControlOptions.MatchBitmapText[2].FontQuality := fqCleartype;
  FindSubControlOptions.MatchBitmapText[1].ForegroundColor := '$Color_WindowText$';
  FindSubControlOptions.MatchBitmapText[2].ForegroundColor := '$Color_WindowText$';
  FindSubControlOptions.MatchBitmapText[1].BackgroundColor := 'EAEAEA';
  FindSubControlOptions.MatchBitmapText[2].BackgroundColor := 'EAEAEA';
  FindSubControlOptions.MatchBitmapText[1].IgnoreBackgroundColor := True;
  FindSubControlOptions.MatchBitmapText[2].IgnoreBackgroundColor := True;

  //FindSubControlOptions.InitialRectangle.LeftOffset := '464';     //limit the search area, to save time (for debugging only, because multiple results are expected)
  //FindSubControlOptions.InitialRectangle.TopOffset := '284';
  //FindSubControlOptions.InitialRectangle.RightOffset := '-635';
  //FindSubControlOptions.InitialRectangle.BottomOffset := '-469';

  FindSubControlOptions.ColorError := '16';
  FindSubControlOptions.AllowedColorErrorCount := '30';

  FindSubControlOptions.GetAllControls := True;   //there will be about 3 texts like this (the actual button, the list of actions and the log)
  FindSubControlOptions.ThreadCount := CThreadCount_VarName;
end;


procedure TTestLowLevelHTTPAPI.Execute_UIClickerActions_SaveTemplateButton(AThreadCount, AThreadMessage: string; var FindControlOptions: TClkFindControlOptions; var WindowOperationsOptions: TClkWindowOperationsOptions; var FindSubControlOptions: TClkFindSubControlOptions);
begin
  Expect(ExecuteFindControlAction(CTestServerAddress, FindControlOptions, 'Find this window (UIClicker Actions)', 3000, CREParam_FileLocation_ValueMem)).ToContain('$LastAction_Status$=Successful', 'Should find window');
  Expect(ExecuteWindowOperationsAction(CTestServerAddress, WindowOperationsOptions)).ToContain('$LastAction_Status$=Successful', 'Should bring window to front');

  SetVariable(TestServerAddress, CThreadCount_VarName, AThreadCount, 0);
  Expect(ExecuteFindSubControlAction(CTestServerAddress, FindSubControlOptions, 'Find "Save Template"', 2000, CREParam_FileLocation_ValueMem)).ToContain('$LastAction_Status$=Successful', AThreadMessage);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerActions_SaveTemplateButton_WithAndWithoutThreads;
var
  UIThreadDuration, OneThreadDuration, TwoThreadsDuration: QWord;

  FindControlOptions: TClkFindControlOptions;
  WindowOperationsOptions: TClkWindowOperationsOptions;
  FindSubControlOptions: TClkFindSubControlOptions;
begin
  Create_SaveTemplateButton_WithAndWithoutThreads_TestTemplateInMem(FindControlOptions, WindowOperationsOptions, FindSubControlOptions);

  Execute_UIClickerActions_SaveTemplateButton('0', 'UI thread', FindControlOptions, WindowOperationsOptions, FindSubControlOptions);
  UIThreadDuration := StrToIntDef(GetVarValueFromServer(CFindSubControlActionDuration_VarName), MaxInt);

  Execute_UIClickerActions_SaveTemplateButton('1', 'One separate thread', FindControlOptions, WindowOperationsOptions, FindSubControlOptions);
  OneThreadDuration := StrToIntDef(GetVarValueFromServer(CFindSubControlActionDuration_VarName), MaxInt);

  Execute_UIClickerActions_SaveTemplateButton('2', 'Two separate threads', FindControlOptions, WindowOperationsOptions, FindSubControlOptions);
  TwoThreadsDuration := StrToIntDef(GetVarValueFromServer(CFindSubControlActionDuration_VarName), MaxInt);

  Expect(DWord(OneThreadDuration)).ToBeLessThan(UIThreadDuration, 'Expecting some performance gain from one separate thread over the UI thread.  ' + IntToStr(OneThreadDuration) + ' should be less than ' + IntToStr(UIThreadDuration));
  Expect(DWord(TwoThreadsDuration)).ToBeLessThan(OneThreadDuration, 'Expecting some performance gain from two separate threads over one separate thread.  ' + IntToStr(TwoThreadsDuration) + ' should be less than ' + IntToStr(OneThreadDuration));
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerActions_SaveTemplateButton_WithAndWithoutThreads_AsTemplate;
var
  tk: QWord;
  UIThreadDuration, OneThreadDuration, TwoThreadsDuration: QWord;
begin                                             //same as above test, but instead of executing actions via API, it executes a template from disk
  SetVariable(TestServerAddress, '$ThreadCount$', '0', 0);
  tk := GetTickCount64;
  ExecTestTemplate(TestServerAddress, '$AppDir$\Tests\TestFiles\FindSaveTemplateButtonAsSubControl.clktmpl');
  UIThreadDuration := GetTickCount64 - tk;

  SetVariable(TestServerAddress, '$ThreadCount$', '1', 0);
  tk := GetTickCount64;
  ExecTestTemplate(TestServerAddress, '$AppDir$\Tests\TestFiles\FindSaveTemplateButtonAsSubControl.clktmpl');
  OneThreadDuration := GetTickCount64 - tk;

  SetVariable(TestServerAddress, '$ThreadCount$', '2', 0);
  tk := GetTickCount64;
  ExecTestTemplate(TestServerAddress, '$AppDir$\Tests\TestFiles\FindSaveTemplateButtonAsSubControl.clktmpl');
  TwoThreadsDuration := GetTickCount64 - tk;

  Expect(DWord(OneThreadDuration)).ToBeLessThan(UIThreadDuration, 'Expecting some performance gain from one separate thread over the UI thread.  ' + IntToStr(OneThreadDuration) + ' vs. ' + IntToStr(UIThreadDuration));
  Expect(DWord(TwoThreadsDuration)).ToBeLessThan(OneThreadDuration, 'Expecting some performance gain from two separate threads over one separate thread.  ' + IntToStr(TwoThreadsDuration) + ' vs. ' + IntToStr(OneThreadDuration));
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_CustomLabel_IgnoreBG;
begin                                         //This test should be modified, to execute in-mem actions, i.e. FindSubControl, via API. Only the FindSubControl execution time should be measured.
                                              //It should generate some options with GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness and modify them for this test.
  ExecTestTemplate(TestServerAddress, '$AppDir$\Tests\TestFiles\FindTextOnAppIgnoreBG.clktmpl');
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_WindowInterpreterButton_Disk;
var
  Response: string;
  FindSubControlOptions: TClkFindSubControlOptions;
begin
  SetupTargetWindowFor_FindSubControl;
  GenerateFindSubControlOptionsForMainUIClickerWindow_WinInterpBtn(FindSubControlOptions, False);
  Response := FastReplace_87ToReturn(ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'Test Find WindowInterpreterButton_Disk on UIClicker Main', 3000, CREParam_FileLocation_ValueDisk));

  ExpectSuccessfulAction(Response);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_WindowInterpreterButton_Mem_NoSender;
const
  CExpectedErr_Part1: string = 'Timeout at "Test Find WindowInterpreterButton_Mem_NoSender on UIClicker Main" in ';
  CExpectedErr_Part2: string = '  File not found: "py\bmps\ShowActionsWindow_Focused.bmp" File not found: "py\bmps\ShowActionsWindow_FocusedHighlighted.bmp" File not found: "py\bmps\ShowActionsWindow_Unfocused.bmp"';
var
  Response: string;
  ListOfVars: TStringList;
  FindSubControlOptions: TClkFindSubControlOptions;
begin
  SetupTargetWindowFor_FindSubControl;
  GenerateFindSubControlOptionsForMainUIClickerWindow_WinInterpBtn(FindSubControlOptions, False);

  SendTerminateWaitingForFileAvailabilityRequest(CREParam_TerminateWaitingLoop_ValueAll, 3000); //send request after 3s, and let the test continue
  Response := FastReplace_87ToReturn(ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'Test Find WindowInterpreterButton_Mem_NoSender on UIClicker Main', 3000, CREParam_FileLocation_ValueMem));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    ExpectFailedAction(ListOfVars, CExpectedErr_Part1);
    ExpectFailedAction(ListOfVars, CExpectedErr_Part2);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_PmtvPreviewButton_Disk;
var
  Response: string;
  FindSubControlOptions: TClkFindSubControlOptions;
begin
  SetupTargetWindowFor_FindSubControl;
  GenerateFindSubControlOptionsForMainUIClickerWindow_PmtvPreviewBtn(FindSubControlOptions, False);
  Response := FastReplace_87ToReturn(ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'Test Find PreviewButton_Disk on UIClicker Main with pmtv', 3000, CREParam_FileLocation_ValueDisk));

  ExpectSuccessfulAction(Response);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_PmtvPreviewButton_Mem;
const
  CAllowedDirs = '$AppDir$\TestFiles\';
  CAllowedExts = '.clktmpl'#13#10'.bmp'#13#10'.pmtv';
  CPmtvFiles = 'TestFiles\PreviewButtonIcon.pmtv'#13#10'TestFiles\PreviewButtonIcon64.pmtv'#13#10 +
               '$AppDir$\TestFiles\PreviewButtonIcon.pmtv'#13#10'$AppDir$\TestFiles\PreviewButtonIcon64.pmtv';
var
  Response: string;
  FindSubControlOptions: TClkFindSubControlOptions;
  FileProvider: TPollForMissingServerFiles;
begin
  SetupTargetWindowFor_FindSubControl;
  GenerateFindSubControlOptionsForMainUIClickerWindow_PmtvPreviewBtn(FindSubControlOptions, False);

  FindSubControlOptions.MatchPrimitiveFiles := CPmtvFiles;

  Expect(ClearInMemFileSystem(TestServerAddress)).ToBe(CREResp_Done);
  CopyMultipleFilesFromDiskToInMemFS(FindSubControlOptions.MatchPrimitiveFiles);

  FileProvider := CreateFileProvider(CAllowedDirs, CAllowedExts, @HandleOnFileExists_Mem, @HandleOnLoadMissingFileContent_Mem);
  try
    Response := FastReplace_87ToReturn(Send_ExecuteCommandAtIndex_ToServer(2, 0));
    Response := FastReplace_87ToReturn(ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'Test Find PreviewButton_Mem on UIClicker Main with pmtv', 3000, CREParam_FileLocation_ValueMem));

    ExpectSuccessfulAction(Response);
  finally
    DestroyFileProvider(FileProvider);
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_RenderingServer_PmtvGradientWithText;
begin                                         //This test should be modified, to execute in-mem actions, i.e. FindSubControl, via API. Only the FindSubControl execution time should be measured.
                                              //It should generate some options with GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness and modify them for this test.
  ExecTestTemplate(TestServerAddress, '$AppDir$\Tests\TestFiles\RenderGradientOnServer.clktmpl');
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_RenderingServer_PmtvGradientOnBrowser;
begin                                         //This test should be modified, to execute in-mem actions, i.e. FindSubControl, via API. Only the FindSubControl execution time should be measured.
                                              //It should generate some options with GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness and modify them for this test.
  ExecTestTemplate(TestServerAddress, '$AppDir$\Tests\TestFiles\RenderGradientOnBrowser.clktmpl');
end;


procedure TTestLowLevelHTTPAPI.Test_FindSubControl_MultiFind(AFnm, AExpectedXOffsets, AExpectedYOffsets: string);
var
  Res: TStringList;
begin                                         //This test should be modified, to execute in-mem actions, i.e. FindSubControl, via API. Only the FindSubControl execution time should be measured.
                                              //It should generate some options with GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness and modify them for this test.
  SetVariable(TestServerAddress, '$AllControl_XOffsets$', 'Erased', 0);
  SetVariable(TestServerAddress, '$AllControl_YOffsets$', 'Erased', 0);

  Res := TStringList.Create;
  try
    Res.Text := ExecTestTemplate(TestServerAddress, '$AppDir$\Tests\TestFiles\' + AFnm);
    Expect(Res).WithItem('$AllControl_XOffsets$').ToContain(AExpectedXOffsets);
    Expect(Res).WithItem('$AllControl_YOffsets$').ToContain(AExpectedYOffsets);
  finally
    Res.Free;
  end;
end;


const
  CExpected_X_Profile0 = '5829858298';
  CExpected_Y_Profile0 = '9191121121';
  CExpected_X_Profile1 = '181421181421';
  CExpected_Y_Profile1 = '116116146146';
  CExpected_X_Profile01 = CExpected_X_Profile0 + CExpected_X_Profile1;
  CExpected_Y_Profile01 = CExpected_Y_Profile0 + CExpected_Y_Profile1;
  CExpected_X_ExtProfile01 = '108368238498' + CExpected_X_Profile01;
  CExpected_Y_ExtProfile01 = '56566161' + CExpected_Y_Profile01;
  CExpected_X_Profile10 = CExpected_X_Profile1 + CExpected_X_Profile0;
  CExpected_Y_Profile10 = CExpected_Y_Profile1 + CExpected_Y_Profile0;
  CExpected_X_ExtProfile10 = '108368238498' + CExpected_X_Profile10;
  CExpected_Y_ExtProfile10 = '56566161' + CExpected_Y_Profile10;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_RenderingServer_MultiText;
begin
  Test_FindSubControl_MultiFind('RenderMultiTextOnServer.clktmpl',
                                CExpected_X_Profile0,
                                CExpected_Y_Profile0);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_RenderingServer_MultiTextWithTwoProfiles;
begin
  Test_FindSubControl_MultiFind('RenderMultiTextOnServerWithTwoProfiles.clktmpl',
                                CExpected_X_Profile01,
                                CExpected_Y_Profile01);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_RenderingServer_MultiBmp;
begin
  Test_FindSubControl_MultiFind('RenderMultiBmpOnServer.clktmpl',
                                CExpected_X_Profile0,
                                CExpected_Y_Profile0);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_RenderingServer_MultiBmpWithTwoProfiles;
begin
  Test_FindSubControl_MultiFind('RenderMultiBmpOnServerWithTwoProfiles.clktmpl',
                                CExpected_X_Profile01,
                                CExpected_Y_Profile01);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_RenderingServer_MultiPmtv;
begin                                         //This test should be modified, to execute in-mem actions, i.e. FindSubControl, via API. Only the FindSubControl execution time should be measured.
  Test_FindSubControl_MultiFind('RenderMultiPmtvOnServer.clktmpl',
                                CExpected_X_Profile0,
                                CExpected_Y_Profile0);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_RenderingServer_MultiPmtvWithTwoProfiles;
begin
  Test_FindSubControl_MultiFind('RenderMultiPmtvOnServerWithTwoProfiles.clktmpl',
                                CExpected_X_Profile01,
                                CExpected_Y_Profile01);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_RenderingServer_MultiTextWithTwoProfilesAndOneBmp;
begin
  Test_FindSubControl_MultiFind('RenderMultiTextOnServerWithTwoProfilesAndOneBmp.clktmpl',
                                CExpected_X_Profile01,
                                CExpected_Y_Profile01);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_RenderingServer_MultiTextWithTwoProfilesAndOnePmtv;
begin
  Test_FindSubControl_MultiFind('RenderMultiTextOnServerWithTwoProfilesAndOnePmtv.clktmpl',
                                CExpected_X_Profile01,
                                CExpected_Y_Profile01);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_RenderingServer_MultiTextWithTwoProfilesAndOneBmpAndOnePmtv;
begin
  Test_FindSubControl_MultiFind('RenderMultiTextOnServerWithTwoProfilesAndOneBmpAndOnePmtv.clktmpl',
                                CExpected_X_Profile01,
                                CExpected_Y_Profile01);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_RenderingServer_MultiTextWithTwoProfilesAndTextAndOneBmpAndOnePmtv;
begin
  Test_FindSubControl_MultiFind('RenderMultiTextOnServerWithTwoProfilesAndTextAndOneBmpAndOnePmtv.clktmpl',
                                CExpected_X_ExtProfile01,
                                CExpected_Y_ExtProfile01);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_RenderingServer_MultiTextWithTwoProfilesAndTextAndPmtvWithTwoOrders;
begin
  Test_FindSubControl_MultiFind('RenderMultiTextOnServerWithTwoProfilesAndTextAndPmtvWithTwoOrders.clktmpl',
                                CExpected_X_ExtProfile10,
                                CExpected_Y_ExtProfile10);
end;


procedure TTestLowLevelHTTPAPI.Test_FindSubControl_RenderExternalBackground;
const
  CPathToExternalRenderingServer = '$AppDir$\Tests\TestFiles\StartRenderingTestServer.clktmpl';
  CRenderBmpCmd = 'SrvAddrPort=http://127.0.0.1:53444$#4#5$Cmd=GetGradientImage$#4#5$Filename=I:\TheResult.bmp$#4#5$Params=IncludeTimestamp%3DYes&TextCount%3D1';
var
  CallTemplateOptions: TClkCallTemplateOptions;
begin
  //Expect(SendLoadTemplateInExecListRequest(TestServerAddress, CPathToExternalRenderingServer, 0)).ToBe(CREResp_TemplateLoaded);  //This command tells UIClicker to load the template from InMem FS, but the file is on disk. See ExecuteCallTemplateAction below.
  GenerateCallTemplateOptions(CallTemplateOptions, CPathToExternalRenderingServer, '', False);
  ExpectSuccessfulAction(FastReplace_87ToReturn(ExecuteCallTemplateAction(TestServerAddress, CallTemplateOptions, False, False, CREParam_FileLocation_ValueDisk, True)));

  CreateCallableTestTemplateInMem('ExtBk.clktmpl', '$RenderBmpExternally()$', CRenderBmpCmd, '0', True, True);
  SendTemplateFromInMemToServerThenLoad('ExtBk.clktmpl');

  ExpectSuccessfulAction(FastReplace_87ToReturn(Send_ExecuteCommandAtIndex_ToServer(0, 0)));  // $RenderBmpExternally()$

  Expect(GetVarValueFromServer('$ExternallyRenderedBmpResult$', 0)).ToBe('', 'The rendering error message should be empty.');
end;


procedure TTestLowLevelHTTPAPI.CloseRenderingServer;
const
  CPathToCloseExternalRenderingServer = '$AppDir$\Tests\TestFiles\CloseRenderingServer.clktmpl';
var
  CallTemplateOptions: TClkCallTemplateOptions;
begin
  GenerateCallTemplateOptions(CallTemplateOptions, CPathToCloseExternalRenderingServer, '', False);
  ExpectSuccessfulAction(FastReplace_87ToReturn(ExecuteCallTemplateAction(TestServerAddress, CallTemplateOptions, False, False, CREParam_FileLocation_ValueDisk, True)));
end;


procedure TTestLowLevelHTTPAPI.Test_FindSubControl_ExternalBackground_isflDisk;
const
  CExtBk = '$AppDir$\Tests\TestFiles\MyLongText_GreenBlue.bmp';  //used with isflDisk
var
  FindSubControlOptions: TClkFindSubControlOptions;
begin
  GenerateFindSubControlOptionsForLoadedBackgroundBmp(FindSubControlOptions, False, CExtBk);
  ExpectSuccessfulAction(FastReplace_87ToReturn(ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'FindExtBmpFileFromDisk', 1000, CREParam_FileLocation_ValueDisk, True)));
end;


procedure TTestLowLevelHTTPAPI.Test_FindSubControl_ExternalBackground_isflMem;
var
  FindSubControlOptions: TClkFindSubControlOptions;
begin
  Test_FindSubControl_RenderExternalBackground;
  GenerateFindSubControlOptionsForExtRenderingText(FindSubControlOptions, False, 'I:\TheResult.bmp');

  try
    ExpectSuccessfulAction(FastReplace_87ToReturn(ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'FindExtBmp', 1000, CREParam_FileLocation_ValueDisk, True)));
  finally
    CloseRenderingServer;  //close it anyway, so that other tests can run
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_FindSubControl_PrecisionTimeout;
var
  FindSubControlOptions: TClkFindSubControlOptions;
  tk1, tk2: QWord;
  Response: string;
begin
  GenerateFindSubControlOptionsForFullScreenshot(FindSubControlOptions, False);
  FindSubControlOptions.UseFastSearch := False;
  SetLength(FindSubControlOptions.MatchBitmapText, 4);
  FindSubControlOptions.MatchBitmapText[1] := FindSubControlOptions.MatchBitmapText[0];
  FindSubControlOptions.MatchBitmapText[2] := FindSubControlOptions.MatchBitmapText[0];
  FindSubControlOptions.MatchBitmapText[3] := FindSubControlOptions.MatchBitmapText[0];
  FindSubControlOptions.MatchBitmapText[1].ProfileName := 'two';
  FindSubControlOptions.MatchBitmapText[2].ProfileName := 'three';
  FindSubControlOptions.MatchBitmapText[3].ProfileName := 'four';
  FindSubControlOptions.MatchBitmapText[1].BackgroundColor := '00FFFF'; //yellow
  FindSubControlOptions.MatchBitmapText[2].BackgroundColor := '00FF88'; //green
  FindSubControlOptions.MatchBitmapText[3].BackgroundColor := '44AADD'; //orange

  tk1 := GetTickCount64;
  Response := ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'FindTxtOnDesktop_FullDuration', 2000, CREParam_FileLocation_ValueDisk, True);
  ExpectFailedAction(FastReplace_87ToReturn(Response), 'Timeout at "FindTxtOnDesktop_FullDuration" in');
  tk1 := GetTickCount64 - tk1;

  FindSubControlOptions.PrecisionTimeout := True;
  tk2 := GetTickCount64;
  Response := ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'FindTxtOnDesktop_FastTimeout', 2000, CREParam_FileLocation_ValueDisk, True);
  ExpectFailedAction(FastReplace_87ToReturn(Response), 'Timeout');
  tk2 := GetTickCount64 - tk2;

  Expect(DWord(tk1)).ToBeGreaterThan(DWord(tk2 + 100), 'Without PrecisionTimeout, the duration should be greater.');
end;


procedure TTestLowLevelHTTPAPI.Test_FindSubControl_CropFromScreenshot;
begin
  ExecTestTemplate(TestServerAddress, '$AppDir$\Tests\TestFiles\TestFindSubControlInWinOverlayWithCropFromScreenshot.clktmpl');
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteSetControlTextAction_HappyFlow;
const
  OldCaption = 'UI Clicker Main';
  NewCaption = 'UI Clicker Main [Modified]';
begin
  SetupTargetWindowFor_FindSubControl(OldCaption, sfcmFindWindow);  //this is required, because the window might be covered by other windows (e.g. the browser from a previous test)

  try
    ExecuteSetControlTextActionWithMainUIClickerWindow(OldCaption, NewCaption);
  finally
    ExecuteSetControlTextActionWithMainUIClickerWindow(NewCaption, OldCaption);  //restore
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteCallTemplate_HappyFlow;
const
  CVarName = '$IncVar$';
var
  CurrentValue: Integer;
  Response: string;
  ListOfVars: TStringList;
  CallTemplateOptions: TClkCallTemplateOptions;
begin
  CurrentValue := StrToIntDef(GetVarValueFromServer(CVarName), 0);
  CreateCallableTestTemplateInMem(CTestTemplateFileName, CVarName, IntToStr(CurrentValue + 1), '30');
  SendTemplateFromInMemToServer(CTestTemplateFileName);

  CallTemplateOptions.TemplateFileName := CTestTemplateFileName;
  CallTemplateOptions.ListOfCustomVarsAndValues := '';
  CallTemplateOptions.EvaluateBeforeCalling := False;
  CallTemplateOptions.CallTemplateLoop.Enabled := False;
  CallTemplateOptions.CallTemplateLoop.Direction := ldInc;
  CallTemplateOptions.CallTemplateLoop.EvalBreakPosition := lebpAfterContent;

  Response := FastReplace_87ToReturn(ExecuteCallTemplateAction(TestServerAddress, CallTemplateOptions, False, False, CREParam_FileLocation_ValueMem));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    Expect(ListOfVars).WithItem('$ExecAction_Err$').OfValue('', 'No error Allowed.');
    Expect(ListOfVars).WithItem(CVarName).OfValue(IntToStr(CurrentValue + 1));
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteCallTemplateWithSubTemplate_HappyFlow;
const
  CVarName = '$IncVar$';
  CCalledTemplate = 'CalledTemplate.clktmpl';
var
  CurrentValue: Integer;
  Response: string;
  ListOfVars: TStringList;
  CallTemplateOptions: TClkCallTemplateOptions;
begin
  CurrentValue := StrToIntDef(GetVarValueFromServer(CVarName), 0);
  CreateCallableTestTemplateInMem(CCalledTemplate, CVarName, IntToStr(CurrentValue + 1), '30');
  SendTemplateFromInMemToServer(CCalledTemplate);

  CreateCallableTestTemplateInMem_WithCallTemplate(CTestTemplateFileName, CVarName, IntToStr(CurrentValue + 1), CCalledTemplate, '', False, '40');
  SendTemplateFromInMemToServer(CTestTemplateFileName);

  CallTemplateOptions.TemplateFileName := CTestTemplateFileName;
  CallTemplateOptions.ListOfCustomVarsAndValues := '';
  CallTemplateOptions.EvaluateBeforeCalling := False;
  CallTemplateOptions.CallTemplateLoop.Enabled := False;
  CallTemplateOptions.CallTemplateLoop.Direction := ldInc;
  CallTemplateOptions.CallTemplateLoop.EvalBreakPosition := lebpAfterContent;

  Response := FastReplace_87ToReturn(ExecuteCallTemplateAction(TestServerAddress, CallTemplateOptions, False, False, CREParam_FileLocation_ValueMem));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    Expect(ListOfVars).WithItem('$ExecAction_Err$').OfValue('', 'No error Allowed.');
    Expect(ListOfVars).WithItem(CVarName).OfValue(IntToStr(CurrentValue + 1));
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteLoopedCallTemplate_HappyFlow;
const
  CVarName = '$IncVar$';
var
  CurrentValue: Integer;
  Response: string;
  ListOfVars: TStringList;
  CallTemplateOptions: TClkCallTemplateOptions;
begin
  CurrentValue := StrToIntDef(GetVarValueFromServer(CVarName), 0);
  CreateCallableTestTemplateInMem(CTestTemplateFileName, CVarName, '$Sum(' + CVarName + ', 1)$', '30', True);
  SendTemplateFromInMemToServer(CTestTemplateFileName);

  CallTemplateOptions.TemplateFileName := CTestTemplateFileName;
  CallTemplateOptions.ListOfCustomVarsAndValues := '';
  CallTemplateOptions.EvaluateBeforeCalling := False;
  CallTemplateOptions.CallTemplateLoop.Enabled := True;
  CallTemplateOptions.CallTemplateLoop.InitValue := '0';
  CallTemplateOptions.CallTemplateLoop.EndValue := '3';
  CallTemplateOptions.CallTemplateLoop.Counter := '$i$';
  CallTemplateOptions.CallTemplateLoop.Direction := ldInc;
  CallTemplateOptions.CallTemplateLoop.BreakCondition := '';
  CallTemplateOptions.CallTemplateLoop.EvalBreakPosition := lebpAfterContent;

  Response := FastReplace_87ToReturn(ExecuteCallTemplateAction(TestServerAddress, CallTemplateOptions, False, False, CREParam_FileLocation_ValueMem));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    Expect(ListOfVars).WithItem('$ExecAction_Err$').OfValue('', 'No error Allowed.');
    Expect(ListOfVars).WithItem(CVarName).OfValue(IntToStr(CurrentValue + 4));
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteSleep_HappyFlow;
var
  SleepOptions: TClkSleepOptions;
  tk: QWord;
  Response: string;
  Diff: QWord;
begin
  GenerateSleepOptions(SleepOptions, '5000');
  tk := GetTickCount64;
  Response := FastReplace_87ToReturn(ExecuteSleepAction(TestServerAddress, SleepOptions, 'Sleep a bit'));
  Diff := GetTickCount64 - tk;

  Expect(Integer(Diff)).ToBeGreaterThanOrEqualTo(5000);
  ExpectSuccessfulAction(Response);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteSleep_NegativeValue;
var
  SleepOptions: TClkSleepOptions;
  Response: string;
  ListOfVars: TStringList;
begin
  GenerateSleepOptions(SleepOptions, '-3');
  Response := FastReplace_87ToReturn(ExecuteSleepAction(TestServerAddress, SleepOptions, 'Sleep a bit'));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    ExpectFailedAction(ListOfVars, 'Invalid sleep value: -3');
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteSetVar_HappyFlow_NoEval;
const
  CVarName = '$MyVar$';
  CVarInitValue = 'init';
  CVarNewValue = 'some value';
var
  SetVarOptions: TClkSetVarOptions;
  Response: string;
begin
  Expect(SetVariable(TestServerAddress, CVarName, CVarInitValue, 0)).ToBe(CREResp_Done);
  Expect(GetVarValueFromServer(CVarName)).ToBe(CVarInitValue);
  GenerateSetVarOptions_OneVar(SetVarOptions, CVarName, CVarNewValue);

  Response := FastReplace_87ToReturn(ExecuteSetVarAction(TestServerAddress, SetVarOptions));

  ExpectSuccessfulAction(Response);
  Expect(GetVarValueFromServer(CVarName)).ToBe(CVarNewValue);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteSetVar_HappyFlow_NoEvalWithVarName;
const
  CVarName = '$MyVar$';
  CVarInitValue = 'init';
  CVarNewValue = '$SomeVar$';
var
  SetVarOptions: TClkSetVarOptions;
  Response: string;
begin
  Expect(SetVariable(TestServerAddress, CVarName, CVarInitValue, 0)).ToBe(CREResp_Done);
  Expect(SetVariable(TestServerAddress, CVarNewValue, 'unknown', 0)).ToBe(CREResp_Done);
  Expect(GetVarValueFromServer(CVarName)).ToBe(CVarInitValue);
  GenerateSetVarOptions_OneVar(SetVarOptions, CVarName, CVarNewValue); //this should not be evaluated to 'unknown'

  Response := FastReplace_87ToReturn(ExecuteSetVarAction(TestServerAddress, SetVarOptions));

  ExpectSuccessfulAction(Response);
  Expect(GetVarValueFromServer(CVarName)).ToBe(CVarNewValue);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteSetVar_HappyFlow_EvalBefore;
const
  CVarName = '$MyVar$';
  CSecondVarName = '$MySecondVar$';
  CVarInitValue = 'init';
  CVarNewValue = 'some value';
var
  SetVarOptions: TClkSetVarOptions;
  Response: string;
begin
  Expect(SetVariable(TestServerAddress, CVarName, CVarInitValue, 0)).ToBe(CREResp_Done);
  Expect(SetVariable(TestServerAddress, CSecondVarName, CVarNewValue, 0)).ToBe(CREResp_Done);
  Expect(GetVarValueFromServer(CVarName)).ToBe(CVarInitValue);
  Expect(GetVarValueFromServer(CSecondVarName)).ToBe(CVarNewValue);
  GenerateSetVarOptions_OneVar(SetVarOptions, CVarName, CSecondVarName, True);

  Response := FastReplace_87ToReturn(ExecuteSetVarAction(TestServerAddress, SetVarOptions));

  ExpectSuccessfulAction(Response);
  Expect(GetVarValueFromServer(CVarName)).ToBe(CVarNewValue);
end;


const
  CPluginVarsAndValues = 'FindSubControlTopLeftCorner=aFindSubControlBotLeftCorner=bFindSubControlTopRightCorner=cFindSubControlBotRightCorner=dFindSubControlLeftEdge=eFindSubControlTopEdge=fFindSubControlRightEdge=gFindSubControlBottomEdge=h';
  CExpectedErr_RunOnEmptyTemplate = 'This plugin does not run on an empty template.';
  CExpectedErr_WrongNumberOfProperties = 'The plugin is configured with a wrong number of properties: 0, instead of the expected';

procedure TTestLowLevelHTTPAPI.Test_ExecutePlugin_EmptyFileName;
const
  CVarName = '$ExecAction_Err$';
  CVarNewValue = 'Plugin not found at: "".';
  CVarInitValue = 'dummy';
var
  PluginOptions: TClkPluginOptions;
begin
  Expect(SetVariable(TestServerAddress, CVarName, CVarInitValue, 0)).ToBe(CREResp_Done);
  Expect(GetVarValueFromServer(CVarName)).ToBe(CVarInitValue);
  GeneratePluginOptions(PluginOptions, '', '');

  ExecutePluginAction(TestServerAddress, PluginOptions);
  Expect(GetVarValueFromServer(CVarName)).ToBe(CVarNewValue);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecutePlugin_BadPlugin_ValidExe;
const
  CVarName = '$ExecAction_Err$';
  CVarNewValue = 'Cannot get address of ExecutePlugin.';  //Plugin not found at: "$AppDir$\UIClicker.exe".
  CVarInitValue = 'dummy';
var
  PluginOptions: TClkPluginOptions;
begin
  Expect(SetVariable(TestServerAddress, CVarName, CVarInitValue, 0)).ToBe(CREResp_Done);
  Expect(GetVarValueFromServer(CVarName)).ToBe(CVarInitValue);
  GeneratePluginOptions(PluginOptions, '$AppDir$\UIClicker.exe', '');

  ExecutePluginAction(TestServerAddress, PluginOptions);
  Expect(GetVarValueFromServer(CVarName)).ToBe(CVarNewValue);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecutePlugin_BadPlugin_InvalidExe;
//const
//  CVarName = '$ExecAction_Err$';
//  CVarNewValue = 'Invalid plugin at: ';
//  CVarInitValue = 'dummy';
//var
//  PluginOptions: TClkPluginOptions;
//  IniPath: string;
begin
  //if GetVarValueFromServer('$OSBitness$') = 'win32' then
  //begin
  //  Expect(SetVariable(TestServerAddress, CVarName, CVarInitValue, 0)).ToBe(CREResp_Done);
  //  Expect(GetVarValueFromServer(CVarName)).ToBe(CVarInitValue);
  //  GeneratePluginOptions(PluginOptions, '$AppDir$\Clicker.ini', '');
  //
  //  IniPath := ExtractFilePath(ExtractFileDir(ParamStr(0))) + 'Clicker.ini';
  //  ExecutePluginAction(TestServerAddress, PluginOptions);
  //  Expect(GetVarValueFromServer(CVarName)).ToBe(CVarNewValue + '"' + IniPath + '".');
  //end
  //else
    Ignore('Disabled test on 64-bit, because UIClicker displays a system pop-up.');
    //Nothing on 64-bit, because UIClicker will show a MessageBox (with system-releated message), which doesn't seem to be displayed on 32-bit. Maybe this test can be moved to UI tests, where it can expect a pop-up.
end;


procedure TTestLowLevelHTTPAPI.Test_ExecutePlugin(APluginVarsAndValues, AExpectedErr: string);
const
  CVarName = '$PluginError$';
  CVarName2 = '$ExecAction_Err$';
  CVarInitValue = '';
var
  PluginOptions: TClkPluginOptions;
begin
  Expect(SetVariable(TestServerAddress, CVarName, CVarInitValue, 0)).ToBe(CREResp_Done);
  Expect(GetVarValueFromServer(CVarName)).ToBe(CVarInitValue);

  GeneratePluginOptions(PluginOptions, GetPluginPath, '');
  ExecutePluginAction(TestServerAddress, PluginOptions);

  Expect(FastReplace_ReturnTo45(FastReplace_68ToReturn(GetVarValueFromServer(CVarName)))).ToContain(AExpectedErr);
  Expect(GetVarValueFromServer(CVarName2)).ToBe('');
end;


procedure TTestLowLevelHTTPAPI.Test_ExecutePlugin_EmptyListOfVars_EmptyTemplate;
begin
  SendEmptyTemplateToServerThenLoad;
  Test_ExecutePlugin('', CExpectedErr_RunOnEmptyTemplate);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecutePlugin_EmptyListOfVars_WithDummyActions;
begin
  CreateTestTemplateInMem;
  SendTemplateFromInMemToServerThenLoad(CTestTemplateFileName);
  Test_ExecutePlugin('', CExpectedErr_WrongNumberOfProperties);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecutePlugin_ValidVarsSetToNonExistentActions_EmptyTemplate;
begin
  SendEmptyTemplateToServerThenLoad;
  Test_ExecutePlugin(CPluginVarsAndValues, CExpectedErr_RunOnEmptyTemplate);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecutePlugin_ValidVarsSetToNonExistentActions_WithDummyActions;
begin
  CreateTestTemplateInMem;
  SendTemplateFromInMemToServerThenLoad(CTestTemplateFileName);
  Test_ExecutePlugin(CPluginVarsAndValues, CExpectedErr_WrongNumberOfProperties);
end;


const
  CTestEditTemplateFileName = 'TestEditTemplateAction.clktmpl';
  //CTestEditTemplateFileName = '$AppDir$\ActionTemplates\TestEditTemplateAction.clktmpl';  //not needed to have a path

procedure TTestLowLevelHTTPAPI.Test_ExecuteEditTemplate(AActionType: TClkAction; AOperation: TEditTemplateOperation; var AExpectedValues: TStringArray);
var
  EditTemplateOptions: TClkEditTemplateOptions;
  TempListOfEnabledProperties: TStringList;
  i: Integer;
  VarName: string;
begin
  GenerateEditTemplateOptions(EditTemplateOptions, AActionType, AOperation, etwtOther, CTestEditTemplateFileName);
  TempListOfEnabledProperties := TStringList.Create;
  try
    TempListOfEnabledProperties.Text := EditTemplateOptions.ListOfEnabledProperties;
    for i := 0 to TempListOfEnabledProperties.Count - 1 do
    begin
      VarName := '$Property_' + TempListOfEnabledProperties.Strings[i] + '_Value$';
      Expect(SetVariable(TestServerAddress, VarName, '__', 0)).ToBe(CREResp_Done);
      Expect(GetVarValueFromServer(VarName)).ToBe('__');
    end;

    ExecuteEditTemplateAction(TestServerAddress, EditTemplateOptions);
    Expect(SendLoadTemplateInExecListRequest(TestServerAddress, CTestEditTemplateFileName, 0)).ToBe(CREResp_TemplateLoaded);//for debugging only

    //Execute a get property, which should set the variables.
    GenerateEditTemplateOptions(EditTemplateOptions, AActionType, etoGetProperty, etwtOther, CTestEditTemplateFileName);
    ExecuteEditTemplateAction(TestServerAddress, EditTemplateOptions);

    Expect(TempListOfEnabledProperties.Count).ToBe(DWord(Length(AExpectedValues)), 'The number of properties mismatches. Please update.');
    for i := 0 to TempListOfEnabledProperties.Count - 1 do
    begin
      VarName := '$Property_' + TempListOfEnabledProperties.Strings[i] + '_Value$';
      Expect(GetVarValueFromServer(VarName)).ToBe(AExpectedValues[i]);
    end;
  finally
    TempListOfEnabledProperties.Free;
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteEditTemplate_UpdateAction_Click_HappyFlow;
var
  ExpectedValues: TStringArray;
begin
  CreateTestTemplateWithAllActionsInMem(CTestEditTemplateFileName);
  SendTemplateFromInMemToServer(CTestEditTemplateFileName);

  SetLength(ExpectedValues, 3);
  ExpectedValues[0] := '2';
  ExpectedValues[1] := '$JustAnotherVar$';
  ExpectedValues[2] := '77';
  Test_ExecuteEditTemplate(acClick, etoUpdateAction, ExpectedValues);  //@['2', '$JustAnotherVar$', '77']
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteEditTemplate_UpdateAction_ExecApp_HappyFlow;
var
  ExpectedValues: TStringArray;
begin
  CreateTestTemplateWithAllActionsInMem(CTestEditTemplateFileName);
  SendTemplateFromInMemToServer(CTestEditTemplateFileName);

  SetLength(ExpectedValues, 2);
  ExpectedValues[0] := 'path to destination';
  ExpectedValues[1] := 'something typed into app console';
  Test_ExecuteEditTemplate(acExecApp, etoUpdateAction, ExpectedValues);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteEditTemplate_UpdateAction_FindControl_HappyFlow;
var
  ExpectedValues: TStringArray;
begin
  CreateTestTemplateWithAllActionsInMem(CTestEditTemplateFileName);
  SendTemplateFromInMemToServer(CTestEditTemplateFileName);

  SetLength(ExpectedValues, 5);
  ExpectedValues[0] := '1';
  ExpectedValues[1] := '1';
  ExpectedValues[2] := 'text to be matched';
  ExpectedValues[3] := 'some window';
  ExpectedValues[4] := '1';
  Test_ExecuteEditTemplate(acFindControl, etoUpdateAction, ExpectedValues);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteEditTemplate_UpdateAction_FindSubControl_HappyFlow;
var
  ExpectedValues: TStringArray;
begin
  CreateTestTemplateWithAllActionsInMem(CTestEditTemplateFileName);
  SendTemplateFromInMemToServer(CTestEditTemplateFileName);

  SetLength(ExpectedValues, 7);
  ExpectedValues[0] := '1';
  ExpectedValues[1] := 'bmp text to be matched';
  ExpectedValues[2] := '$FGCol$';
  ExpectedValues[3] := '$BGCol$';
  ExpectedValues[4] := 'Mono';
  ExpectedValues[5] := '19';
  ExpectedValues[6] := '0';
  Test_ExecuteEditTemplate(acFindSubControl, etoUpdateAction, ExpectedValues);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteEditTemplate_UpdateAction_SetControlText_HappyFlow;
var
  ExpectedValues: TStringArray;
begin
  CreateTestTemplateWithAllActionsInMem(CTestEditTemplateFileName);
  SendTemplateFromInMemToServer(CTestEditTemplateFileName);

  SetLength(ExpectedValues, 3);
  ExpectedValues[0] := 'Type this text.';
  ExpectedValues[1] := '2';
  ExpectedValues[2] := '$cnt$';
  Test_ExecuteEditTemplate(acSetControlText, etoUpdateAction, ExpectedValues);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteEditTemplate_UpdateAction_CallTemplate_HappyFlow;
var
  ExpectedValues: TStringArray;
begin
  CreateTestTemplateWithAllActionsInMem(CTestEditTemplateFileName);
  SendTemplateFromInMemToServer(CTestEditTemplateFileName);

  SetLength(ExpectedValues, 3);
  ExpectedValues[0] := 'Template to be called.';
  ExpectedValues[1] := '$a$=a$b$=b';
  ExpectedValues[2] := '$ii$';
  Test_ExecuteEditTemplate(acCallTemplate, etoUpdateAction, ExpectedValues);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteEditTemplate_UpdateAction_Sleep_HappyFlow;
var
  ExpectedValues: TStringArray;
begin
  CreateTestTemplateWithAllActionsInMem(CTestEditTemplateFileName);
  SendTemplateFromInMemToServer(CTestEditTemplateFileName);

  SetLength(ExpectedValues, 1);
  ExpectedValues[0] := '456';
  Test_ExecuteEditTemplate(acSleep, etoUpdateAction, ExpectedValues);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteEditTemplate_UpdateAction_SetVar_HappyFlow;
var
  ExpectedValues: TStringArray;
begin
  CreateTestTemplateWithAllActionsInMem(CTestEditTemplateFileName);
  SendTemplateFromInMemToServer(CTestEditTemplateFileName);

  SetLength(ExpectedValues, 2);
  ExpectedValues[0] := '__'; //'ListOfVarNamesValuesAndEvalBefore'; //This action does not get the value of ListOfVarNamesValuesAndEvalBefore.
  ExpectedValues[1] := '1'; //'FailOnException';

  Test_ExecuteEditTemplate(acSetVar, etoUpdateAction, ExpectedValues);

  //Because of the special encoding, Test_ExecuteEditTemplate is not compatible with this action type, so the values have to be verified here.
  Expect(GetVarValueFromServer('$Property_ListOfVarNames_Value$')).ToBe('$abc$$def$');
  Expect(GetVarValueFromServer('$Property_ListOfVarValues_Value$')).ToBe('ab');
  Expect(GetVarValueFromServer('$Property_ListOfVarEvalBefore_Value$')).ToBe('11');
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteEditTemplate_UpdateAction_WindowOperations_HappyFlow;
var
  ExpectedValues: TStringArray;
begin
  CreateTestTemplateWithAllActionsInMem(CTestEditTemplateFileName);
  SendTemplateFromInMemToServer(CTestEditTemplateFileName);

  SetLength(ExpectedValues, 2);
  ExpectedValues[0] := 'X300';
  ExpectedValues[1] := 'Y400';
  Test_ExecuteEditTemplate(acWindowOperations, etoUpdateAction, ExpectedValues);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteEditTemplate_UpdateAction_LoadSetVarFromFile_HappyFlow;
var
  ExpectedValues: TStringArray;
begin
  CreateTestTemplateWithAllActionsInMem(CTestEditTemplateFileName);
  SendTemplateFromInMemToServer(CTestEditTemplateFileName);

  SetLength(ExpectedValues, 2);
  ExpectedValues[0] := '$PathToFile$';
  ExpectedValues[1] := '$Action$';
  Test_ExecuteEditTemplate(acLoadSetVarFromFile, etoUpdateAction, ExpectedValues);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteEditTemplate_UpdateAction_SaveSetVarToFile_HappyFlow;
var
  ExpectedValues: TStringArray;
begin
  CreateTestTemplateWithAllActionsInMem(CTestEditTemplateFileName);
  SendTemplateFromInMemToServer(CTestEditTemplateFileName);

  SetLength(ExpectedValues, 2);
  ExpectedValues[0] := '$PathToAnotherFile$';
  ExpectedValues[1] := '$AnotherAction$';
  Test_ExecuteEditTemplate(acSaveSetVarToFile, etoUpdateAction, ExpectedValues);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteEditTemplate_UpdateAction_Plugin_HappyFlow;
var
  ExpectedValues: TStringArray;
begin
  CreateTestTemplateWithAllActionsInMem(CTestEditTemplateFileName);
  SendTemplateFromInMemToServer(CTestEditTemplateFileName);

  SetLength(ExpectedValues, 4);
  ExpectedValues[0] := '$AppDir$\..\UIClickerFindWindowsPlugin\lib\i386-win32\UIClickerFindWindows.dll';
  ExpectedValues[1] := '30';
  ExpectedValues[2] := '40';
  ExpectedValues[3] := '7';
  Test_ExecuteEditTemplate(acPlugin, etoUpdateAction, ExpectedValues);
end;


procedure TTestLowLevelHTTPAPI.CreateTheSecondPluginAction(var AExpectedValues: TStringArray);  //This has to be refactored into multiple sections
var
  TempEditTemplate: TClkEditTemplateOptions;
  DestPluginOptions: TClkPluginOptions;
  TempListOfEnabledProperties: TStringList;
  i: Integer;
  VarName: string;
begin
  TempEditTemplate.ListOfEnabledProperties := 'FileName' + #13#10 +
                                              'WhatToBecomeVarName' + #13#10 +
                                              'CurrentTextVarName' + #13#10 +
                                              'LowLevelTypewriterAction';

  //reset all involved vars
  TempListOfEnabledProperties := TStringList.Create;
  try
    TempListOfEnabledProperties.Text := TempEditTemplate.ListOfEnabledProperties;
    TempListOfEnabledProperties.Text := TempListOfEnabledProperties.Text + #13#10 +
                                        'FindSubControlTopLeftCorner' + #13#10 +
                                        'FindSubControlBotLeftCorner' + #13#10 +
                                        'BorderThickness';

    for i := 0 to TempListOfEnabledProperties.Count - 1 do
    begin
      VarName := '$Property_' + TempListOfEnabledProperties.Strings[i] + '_Value$';
      Expect(SetVariable(TestServerAddress, VarName, '__', 0)).ToBe(CREResp_Done);
      Expect(GetVarValueFromServer(VarName)).ToBe('__');
    end;
  finally
    TempListOfEnabledProperties.Free;
  end;

  DestPluginOptions.FileName := '$AppDir$\..\UIClickerTypewriterPlugin\lib\i386-win32\UIClickerTypewriter.dll';
  DestPluginOptions.ListOfPropertiesAndValues := 'WhatToBecomeVarName=$Become$CurrentTextVarName=$CurrentText$LowLevelTypewriterAction=MyAction';
  GenerateEditTemplateOptions(TempEditTemplate, acPlugin, etoNewAction, etwtOther, CTestEditTemplateFileName);  //this call resets ListOfEnabledProperties

  TempEditTemplate.Operation := etoNewAction;
  TempEditTemplate.EditedActionTimeout := 3333;
  TempEditTemplate.ListOfEditedProperties := GetPluginActionProperties(DestPluginOptions);
  TempEditTemplate.EditedActionName := 'SecondPlugin';

  ExecuteEditTemplateAction(TestServerAddress, TempEditTemplate); //create the plugin action

  //modify the plugin action
  TempEditTemplate.Operation := etoUpdateAction;
  TempEditTemplate.EditedActionTimeout := 4444;
  DestPluginOptions.ListOfPropertiesAndValues := 'WhatToBecomeVarName=$Done$CurrentTextVarName=$EditedText$LowLevelTypewriterAction=SameAction';
  TempEditTemplate.ListOfEditedProperties := GetPluginActionProperties(DestPluginOptions);
  TempEditTemplate.ListOfEnabledProperties := 'FileName' + #13#10 +
                                              'WhatToBecomeVarName' + #13#10 +
                                              'CurrentTextVarName' + #13#10 +
                                              'LowLevelTypewriterAction';

  ExecuteEditTemplateAction(TestServerAddress, TempEditTemplate); //update the plugin action

  Expect(SendLoadTemplateInExecListRequest(TestServerAddress, CTestEditTemplateFileName, 0)).ToBe(CREResp_TemplateLoaded);//for debugging only

  //Execute a GetProperty, which should set the variables.
  TempEditTemplate.Operation := etoGetProperty;
  DestPluginOptions.FileName := 'bad path';   //init to some unlikely values
  DestPluginOptions.ListOfPropertiesAndValues := 'WhatToBecomeVarName=M_valCurrentTextVarName=N_valLowLevelTypewriterAction=27'; //'unknown values';
  TempEditTemplate.ListOfEditedProperties := GetPluginActionProperties(DestPluginOptions);
  ExecuteEditTemplateAction(TestServerAddress, TempEditTemplate);

  //verify updated values
  TempListOfEnabledProperties := TStringList.Create;
  try
    TempListOfEnabledProperties.Text := TempEditTemplate.ListOfEnabledProperties;
    Expect(TempListOfEnabledProperties.Count).ToBe(DWord(Length(AExpectedValues)), 'The number of properties mismatches. Please update.');

    for i := 0 to TempListOfEnabledProperties.Count - 1 do
    begin
      VarName := '$Property_' + TempListOfEnabledProperties.Strings[i] + '_Value$';
      Expect(GetVarValueFromServer(VarName)).ToBe(AExpectedValues[i]);
    end;
  finally
    TempListOfEnabledProperties.Free;
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteEditTemplate_UpdateAction_TwoUpdatedPlugins_HappyFlow;
var
  ExpectedValues: TStringArray;
begin
  //This test relies on the fact that TypeWriter plugin has fewer properties than FindWindows plugin.
  //If there are no bugs, the lists of properties should be properly updated.
  //Also, the two plugins should already exist on disk.

  //The double plugin test should use an EditTemplate action, to create a second Plugin action.
  //This Plugin action has to be configured to point to a second plugin.
  //Ths test verifies if modifying the two plugins, one after the other, will both update and read proper values.

  CreateTestTemplateWithAllActionsInMem(CTestEditTemplateFileName);
  SendTemplateFromInMemToServer(CTestEditTemplateFileName);

  SetLength(ExpectedValues, 4);
  ExpectedValues[0] := '$AppDir$\..\UIClickerFindWindowsPlugin\lib\i386-win32\UIClickerFindWindows.dll';
  ExpectedValues[1] := '30';
  ExpectedValues[2] := '40';
  ExpectedValues[3] := '7';
  Test_ExecuteEditTemplate(acPlugin, etoUpdateAction, ExpectedValues);

  SetLength(ExpectedValues, 4);
  ExpectedValues[0] := '$AppDir$\..\UIClickerTypewriterPlugin\lib\i386-win32\UIClickerTypewriter.dll';
  ExpectedValues[1] := '$Done$';
  ExpectedValues[2] := '$EditedText$';
  ExpectedValues[3] := 'SameAction';
  CreateTheSecondPluginAction(ExpectedValues);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteEditTemplate_UpdateAction_EditTemplate_HappyFlow;
var
  ExpectedValues: TStringArray;
begin
  CreateTestTemplateWithAllActionsInMem(CTestEditTemplateFileName);
  SendTemplateFromInMemToServer(CTestEditTemplateFileName);

  SetLength(ExpectedValues, 4);
  ExpectedValues[0] := IntToStr(Ord(etoDuplicateAction));
  ExpectedValues[1] := IntToStr(Ord(etwtSelf));
  ExpectedValues[2] := '$PathToTemplate$';
  ExpectedValues[3] := 'Act';
  Test_ExecuteEditTemplate(acEditTemplate, etoUpdateAction, ExpectedValues);
end;


initialization

  RegisterTest(TTestLowLevelHTTPAPI);
end.

