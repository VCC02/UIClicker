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


unit TestLowLevelHTTPAPI;

{$mode ObjFPC}{$H+}

interface

uses
  LCLIntf, Classes, SysUtils, TestHTTPAPI, fpcunit, testregistry, Expectations;


type

  TTestLowLevelHTTPAPI = class(TTestHTTPAPI)
  private
    procedure Test_FindSubControl_MultiFind(AFnm, AExpectedXOffsets, AExpectedYOffsets: string);
    function GetPluginPath: string;
    procedure Test_ExecutePlugin(APluginVarsAndValues, AExpectedErr: string);
  public
    constructor Create; override;
  published
    procedure Test_ExecuteClickAction_LeaveMouse;
    procedure Test_ExecuteClickAction_MouseWheel;
    procedure Test_ExecuteClickAction_TestApp_ClickDuration;
    procedure Test_ExecuteClickAction_TestApp_DragDuration;
    procedure Test_ExecuteExecAppAction_IPConfig;
    procedure Test_ExecuteExecAppAction_IPConfig_NoInheritHandles;

    procedure Test_ExecuteFindControlAction_UIClickerMain;
    procedure Test_ExecuteFindControlAction_UIClickerMain_WrongClass;
    procedure Test_ExecuteFindControlAction_UIClickerMain_WrongClassAllowToFail;

    procedure Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabel;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithCropping;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithBadCropping;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithBadCropping_Width0;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithBadCropping_NegativeWidth;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithBadCropping_Height0;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithBadCropping_NegativeHeight;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabel_WithFastSearch;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabel_WithFastSearchAndSleepySearch;
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
  end;


implementation


uses
  ClickerActionsClient, ClickerUtils, ActionsStuff, Controls, ClickerFileProviderClient;


function ExecTestTemplate(ATestServerAddress, ATemplateName: string): string;
var
  CallTemplateOptions: TClkCallTemplateOptions;
begin
  CallTemplateOptions.TemplateFileName := ATemplateName;
  CallTemplateOptions.ListOfCustomVarsAndValues := '';
  CallTemplateOptions.EvaluateBeforeCalling := False;
  CallTemplateOptions.CallTemplateLoop.Enabled := False;
  CallTemplateOptions.CallTemplateLoop.Direction := ldInc;
  CallTemplateOptions.CallTemplateLoop.EvalBreakPosition := lebpAfterContent;

  Result := FastReplace_87ToReturn(ExecuteCallTemplateAction(ATestServerAddress, CallTemplateOptions, False, False, CREParam_FileLocation_ValueDisk));
  ExpectSuccessfulAction(Result);
end;


constructor TTestLowLevelHTTPAPI.Create;
begin
  inherited Create;
  TestServerAddress := CTestServerAddress;
end;


function TTestLowLevelHTTPAPI.GetPluginPath: string;
var
  AppBitness: string;
  OSBitness: string;
begin
  AppBitness := GetVarValueFromServer('$AppBitness$');
  OSBitness := GetVarValueFromServer('$OSBitness$');
  Result := '$AppDir$\..\UIClickerFindWindowsPlugin\lib\' + AppBitness + '-' + OSBitness + '\UIClickerFindWindows.dll';
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


procedure TTestLowLevelHTTPAPI.Test_ExecuteClickAction_TestApp_DragDuration;
begin                                         //This test should be modified, to execute in-mem actions, i.e. Click, via API.
                                              //It should generate some options similar to GenerateClickOptionsForLeaveMouse and modify them for this test.
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


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabel;
var
  Response: string;
  FindSubControlOptions: TClkFindControlOptions;
begin
  SetupTargetWindowFor_FindSubControl;
  GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness(FindSubControlOptions, False);
  Response := FastReplace_87ToReturn(ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'Test Find Bitness on UIClicker Main', 3000, CREParam_FileLocation_ValueMem));

  ExpectSuccessfulAction(Response);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithCropping;
var
  Response: string;
  FindSubControlOptions: TClkFindControlOptions;
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
  FindSubControlOptions: TClkFindControlOptions;
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
  FindSubControlOptions: TClkFindControlOptions;
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
  FindSubControlOptions: TClkFindControlOptions;
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
  FindSubControlOptions: TClkFindControlOptions;
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
  FindSubControlOptions: TClkFindControlOptions;
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


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_CustomLabel_IgnoreBG;
begin                                         //This test should be modified, to execute in-mem actions, i.e. FindSubControl, via API. Only the FindSubControl execution time should be measured.
                                              //It should generate some options with GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness and modify them for this test.
  ExecTestTemplate(TestServerAddress, '$AppDir$\Tests\TestFiles\FindTextOnAppIgnoreBG.clktmpl');
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_WindowInterpreterButton_Disk;
var
  Response: string;
  FindSubControlOptions: TClkFindControlOptions;
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
  FindSubControlOptions: TClkFindControlOptions;
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
  FindSubControlOptions: TClkFindControlOptions;
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
  FindSubControlOptions: TClkFindControlOptions;
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
const
  CVarName = '$ExecAction_Err$';
  CVarNewValue = 'Invalid plugin at: ';
  CVarInitValue = 'dummy';
var
  PluginOptions: TClkPluginOptions;
  IniPath: string;
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


initialization

  RegisterTest(TTestLowLevelHTTPAPI);
end.

