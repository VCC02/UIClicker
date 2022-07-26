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

    procedure Test_ExecuteFindSubControlAction_UIClickerMain_WindowInterpreterButton_Disk;
    procedure Test_ExecuteFindSubControlAction_UIClickerMain_WindowInterpreterButton_Mem_NoSender;

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
  ClickerActionsClient, ClickerUtils, ActionsStuff, Controls;


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
  ListOfVars: TStringList;
  ClickOptions: TClkClickOptions;
  tp, InitialTp: TPoint;
begin
  GenerateClickOptionsForLeaveMouse(CX, CY, ClickOptions);
  GetCursorPos(InitialTp);

  Response := FastReplace_87ToReturn(ExecuteClickAction(TestServerAddress, ClickOptions));

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
  ListOfVars: TStringList;
  FindControlOptions: TClkFindControlOptions;
begin
  GenerateFindControlOptionsForMainUIClickerWindow(FindControlOptions, False);
  Response := FastReplace_87ToReturn(ExecuteFindControlAction(TestServerAddress, FindControlOptions, 'TestFind UIClicker Main', 1000, CREParam_FileLocation_ValueMem));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    ExpectSuccessfulAction(ListOfVars);
  finally
    ListOfVars.Free;
  end;
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
  ListOfVars: TStringList;
  FindSubControlOptions: TClkFindControlOptions;
begin
  SetupTargetWindowFor_FindSubControl;
  GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness(FindSubControlOptions, False);
  Response := FastReplace_87ToReturn(ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'Test Find Bitness on UIClicker Main', 3000, CREParam_FileLocation_ValueMem));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    ExpectSuccessfulAction(ListOfVars);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabelWithCropping;
var
  Response: string;
  ListOfVars: TStringList;
  FindSubControlOptions: TClkFindControlOptions;
begin
  SetupTargetWindowFor_FindSubControl;
  GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness(FindSubControlOptions, False);
  FindSubControlOptions.MatchBitmapText[0].CropLeft := '3';
  FindSubControlOptions.MatchBitmapText[0].CropTop := '1';
  FindSubControlOptions.MatchBitmapText[0].CropRight := '4';
  FindSubControlOptions.MatchBitmapText[0].CropBottom := '2';

  Response := FastReplace_87ToReturn(ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'Test Find Bitness on UIClicker Main', 3000, CREParam_FileLocation_ValueMem));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    ExpectSuccessfulAction(ListOfVars);
  finally
    ListOfVars.Free;
  end;
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

  Expect(Response).ToBe('ProcessServerCommand exception: The text width, after cropping, is 0.  Profile[0]: "frClickerBMPText".   Searched text: "-bit"');
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

  Expect(Response).ToBe('ProcessServerCommand exception: The text width, after cropping, is negative.  Profile[0]: "frClickerBMPText".   Searched text: "-bit"');
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

  Expect(Response).ToBe('ProcessServerCommand exception: The text height, after cropping, is 0.  Profile[0]: "frClickerBMPText".   Searched text: "-bit"');
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

  Expect(Response).ToBe('ProcessServerCommand exception: The text height, after cropping, is negative.  Profile[0]: "frClickerBMPText".   Searched text: "-bit"');
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_WindowInterpreterButton_Disk;
var
  Response: string;
  ListOfVars: TStringList;
  FindSubControlOptions: TClkFindControlOptions;
begin
  SetupTargetWindowFor_FindSubControl;
  GenerateFindSubControlOptionsForMainUIClickerWindow_WinInterpBtn(FindSubControlOptions, False);
  Response := FastReplace_87ToReturn(ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'Test Find WindowInterpreterButton_Disk on UIClicker Main', 3000, CREParam_FileLocation_ValueDisk));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    ExpectSuccessfulAction(ListOfVars);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_WindowInterpreterButton_Mem_NoSender;
const
  CExpectedErr: string = 'Timeout at "Test Find WindowInterpreterButton_Mem_NoSender on UIClicker Main" in   AttemptCount=1  File not found: "py\bmps\ShowActionsWindow_Focused.bmp" File not found: "py\bmps\ShowActionsWindow_FocusedHighlighted.bmp" File not found: "py\bmps\ShowActionsWindow_Unfocused.bmp"';
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
    ExpectFailedAction(ListOfVars, CExpectedErr);
  finally
    ListOfVars.Free;
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
  ListOfVars: TStringList;
  Diff: QWord;
begin
  GenerateSleepOptions(SleepOptions, '5000');
  tk := GetTickCount64;
  Response := FastReplace_87ToReturn(ExecuteSleepAction(TestServerAddress, SleepOptions, 'Sleep a bit'));
  Diff := GetTickCount64 - tk;

  Expect(Integer(Diff)).ToBeGreaterThanOrEqualTo(5000);
  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    ExpectSuccessfulAction(ListOfVars);
  finally
    ListOfVars.Free;
  end;
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
  ListOfVars: TStringList;
begin
  Expect(SetVariable(TestServerAddress, CVarName, CVarInitValue, 0)).ToBe(CREResp_Done);
  Expect(GetVarValueFromServer(CVarName)).ToBe(CVarInitValue);
  GenerateSetVarOptions_OneVar(SetVarOptions, CVarName, CVarNewValue);

  Response := FastReplace_87ToReturn(ExecuteSetVarAction(TestServerAddress, SetVarOptions));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    ExpectSuccessfulAction(ListOfVars);
    Expect(GetVarValueFromServer(CVarName)).ToBe(CVarNewValue);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestLowLevelHTTPAPI.Test_ExecuteSetVar_HappyFlow_NoEvalWithVarName;
const
  CVarName = '$MyVar$';
  CVarInitValue = 'init';
  CVarNewValue = '$SomeVar$';
var
  SetVarOptions: TClkSetVarOptions;
  Response: string;
  ListOfVars: TStringList;
begin
  Expect(SetVariable(TestServerAddress, CVarName, CVarInitValue, 0)).ToBe(CREResp_Done);
  Expect(SetVariable(TestServerAddress, CVarNewValue, 'unknown', 0)).ToBe(CREResp_Done);
  Expect(GetVarValueFromServer(CVarName)).ToBe(CVarInitValue);
  GenerateSetVarOptions_OneVar(SetVarOptions, CVarName, CVarNewValue); //this should not be evaluated to 'unknown'

  Response := FastReplace_87ToReturn(ExecuteSetVarAction(TestServerAddress, SetVarOptions));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    ExpectSuccessfulAction(ListOfVars);
    Expect(GetVarValueFromServer(CVarName)).ToBe(CVarNewValue);
  finally
    ListOfVars.Free;
  end;
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
  ListOfVars: TStringList;
begin
  Expect(SetVariable(TestServerAddress, CVarName, CVarInitValue, 0)).ToBe(CREResp_Done);
  Expect(SetVariable(TestServerAddress, CSecondVarName, CVarNewValue, 0)).ToBe(CREResp_Done);
  Expect(GetVarValueFromServer(CVarName)).ToBe(CVarInitValue);
  Expect(GetVarValueFromServer(CSecondVarName)).ToBe(CVarNewValue);
  GenerateSetVarOptions_OneVar(SetVarOptions, CVarName, CSecondVarName, True);

  Response := FastReplace_87ToReturn(ExecuteSetVarAction(TestServerAddress, SetVarOptions));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    ExpectSuccessfulAction(ListOfVars);
    Expect(GetVarValueFromServer(CVarName)).ToBe(CVarNewValue);
  finally
    ListOfVars.Free;
  end;
end;


initialization

  RegisterTest(TTestLowLevelHTTPAPI);
end.

