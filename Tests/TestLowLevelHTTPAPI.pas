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
  published
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


procedure TTestLowLevelHTTPAPI.Test_ExecuteExecAppAction_IPConfig;
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


procedure TTestLowLevelHTTPAPI.Test_ExecuteExecAppAction_IPConfig_NoInheritHandles;
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


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindControlAction_UIClickerMain;
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


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindControlAction_UIClickerMain_WrongClass;
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


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindControlAction_UIClickerMain_WrongClassAllowToFail;
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


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_BitnessLabel;
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


procedure TTestLowLevelHTTPAPI.Test_ExecuteFindSubControlAction_UIClickerMain_WindowInterpreterButton_Disk;
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

  RegisterTest(TTestLowLevelHTTPAPI);
end.

