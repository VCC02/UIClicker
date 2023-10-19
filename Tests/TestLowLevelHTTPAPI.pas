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
  public
    constructor Create; override;
  published
    procedure Test_ExecuteClickAction_LeaveMouse;
    procedure Test_ExecuteClickAction_MouseWheel;
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
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_CustomLabel_IgnoreBG;

    procedure Test_ExecuteFindSubControlAction_UIClickerMain_WindowInterpreterButton_Disk;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_WindowInterpreterButton_Mem_NoSender;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_PmtvPreviewButton_Disk;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_PmtvPreviewButton_Mem;

    procedure Test_ExecuteSetControlTextAction_HappyFlow;
    procedure Test_ExecuteCallTemplate_HappyFlow;
    procedure Test_ExecuteCallTemplateWithSubTemplate_HappyFlow;
    procedure Test_ExecuteLoopedCallTemplate_HappyFlow;

    procedure Test_ExecuteSleep_HappyFlow;
    procedure Test_ExecuteSleep_NegativeValue;
    procedure Test_ExecuteSetVar_HappyFlow_NoEval;
    procedure Test_ExecuteSetVar_HappyFlow_NoEvalWithVarName;
    procedure Test_ExecuteSetVar_HappyFlow_EvalBefore;
  end;


implementation


uses
  ClickerActionsClient, ClickerUtils, ActionsStuff, Controls, ClickerFileProviderClient;


constructor TTestLowLevelHTTPAPI.Create;
begin
  inherited Create;
  TestServerAddress := CTestServerAddress;
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


procedure ExecFastSearchTestTemplate(ATestServerAddress, ATemplateName: string);
var
  Response: string;
  CallTemplateOptions: TClkCallTemplateOptions;
begin
  CallTemplateOptions.TemplateFileName := ATemplateName;
  CallTemplateOptions.ListOfCustomVarsAndValues := '';
  CallTemplateOptions.EvaluateBeforeCalling := False;
  CallTemplateOptions.CallTemplateLoop.Enabled := False;
  CallTemplateOptions.CallTemplateLoop.Direction := ldInc;
  CallTemplateOptions.CallTemplateLoop.EvalBreakPosition := lebpAfterContent;

  Response := FastReplace_87ToReturn(ExecuteCallTemplateAction(ATestServerAddress, CallTemplateOptions, False, False, CREParam_FileLocation_ValueDisk));
  ExpectSuccessfulAction(Response);
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabel_WithFastSearch;
const
  CExpectedMultiplier = 25;
var
  tk: QWord;
  FirstDuration, SecondDuration: QWord;
begin                                         //This test should be modified, to execute in-mem actions, i.e. FindSubControl, via API. Only the FindSubControl execution time should be measured.
  tk := GetTickCount64;                       //It should generate some options with GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness and modify them for this test.
  ExecFastSearchTestTemplate(TestServerAddress, '$AppDir$\Tests\TestFiles\FindBitOnMainNoFastSearch.clktmpl');
  FirstDuration := GetTickCount64 - tk;

  tk := GetTickCount64;
  ExecFastSearchTestTemplate(TestServerAddress, '$AppDir$\Tests\TestFiles\FindBitOnMainWithFastSearch.clktmpl');
  SecondDuration := GetTickCount64 - tk;

  Expect(DWord(FirstDuration)).ToBeGreaterThan(CExpectedMultiplier * SecondDuration, 'Expecting some performance gain.  ' + IntToStr(FirstDuration) + ' vs. ' + IntToStr(CExpectedMultiplier) + ' * ' + IntToStr(SecondDuration));
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_CustomLabel_IgnoreBG;
begin                                         //This test should be modified, to execute in-mem actions, i.e. FindSubControl, via API. Only the FindSubControl execution time should be measured.
                                              //It should generate some options with GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness and modify them for this test.
  ExecFastSearchTestTemplate(TestServerAddress, '$AppDir$\Tests\TestFiles\FindTextOnAppIgnoreBG.clktmpl');
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
  CExpectedErr_Part2: string = '  AttemptCount=1  File not found: "py\bmps\ShowActionsWindow_Focused.bmp" File not found: "py\bmps\ShowActionsWindow_FocusedHighlighted.bmp" File not found: "py\bmps\ShowActionsWindow_Unfocused.bmp"';
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


procedure TTestLowLevelHTTPAPI.Test_ExecuteSetControlTextAction_HappyFlow;
const
  OldCaption = 'UI Clicker Main';
  NewCaption = 'UI Clicker Main [Modified]';
begin
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


initialization

  RegisterTest(TTestLowLevelHTTPAPI);
end.

