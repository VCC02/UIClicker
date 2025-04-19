{
    Copyright (C) 2025 VCC
    creation date: 13 Apr 2025
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


unit UITestUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, AsyncProcess;


function CreateUIClickerProcess(AExe, AParams: string): TAsyncProcess;
procedure SetUIClickerWindowPosition(APathToIni: string; AMainLeft, AMainTop, AActionsLeft, AActionsTop: Integer);  //useful, to preview the interaction
procedure ExecuteTemplateOnCustomTestDriver(ATestDriverAddr, ATemplatePath, AFileLocation: string; AAdditionalExpectedVar: string = ''; AAdditionalExpectedValue: string = '');
procedure SetVariableOnCustomTestDriverClient(ATestDriverAddr, AVarName, AVarValue: string; AEvalVarBefore: Boolean = False);
procedure WaitForCustomDriverStartup(ATestDriverAddr: string);


procedure PrepareCustomClickerUnderTestToReadItsVars(ATestDriverAddr, AClientUnderTestServerPort, ATemplatesDir: string);
procedure PrepareCustomClickerUnderTestToLocalMode(ATestDriverAddr, ATemplatesDir: string);
procedure PrepareCustomClickerUnderTestToClientMode(ATestDriverAddr, ATemplatesDir: string);


const
  CSkipSavingSettings: string = ' --SkipSavingSettings Yes';


implementation


uses
  IniFiles, ClickerUtils, ClickerActionsClient, Expectations;


function CreateUIClickerProcess(AExe, AParams: string): TAsyncProcess;
begin
  Expect(FileExists(AExe)).ToBe(True, 'The executable file must exist at this path: "' + AExe + '"');
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


procedure ExecuteTemplateOnCustomTestDriver(ATestDriverAddr, ATemplatePath, AFileLocation: string; AAdditionalExpectedVar: string = ''; AAdditionalExpectedValue: string = '');
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

  Response := FastReplace_87ToReturn(ExecuteCallTemplateAction(ATestDriverAddr, CallTemplateOptions, False, False, AFileLocation));

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


procedure SetVariableOnCustomTestDriverClient(ATestDriverAddr, AVarName, AVarValue: string; AEvalVarBefore: Boolean = False);
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
    RawResult := ExecuteSetVarAction(ATestDriverAddr, SetVarOptions);
    SetVarResult.Text := FastReplace_87ToReturn(RawResult); //this is the test "driver" for client 25444
    Expect(SetVarResult).WithItem(AVarName).OfValue(AVarValue, 'Expected a particular var value.');
  finally
    SetVarResult.Free;
  end;
end;


procedure WaitForCustomDriverStartup(ATestDriverAddr: string);
var
  TestResult: string;
  tk: QWord;
begin
  tk := GetTickCount64;
  repeat
    TestResult := TestConnection(ATestDriverAddr, False);
  until (TestResult = CREResp_ConnectionOK) or (GetTickCount64 - tk > 3000);
end;


procedure PrepareCustomClickerUnderTestToReadItsVars(ATestDriverAddr, AClientUnderTestServerPort, ATemplatesDir: string);
begin
  //Set $ExtraCaption$ variable in Client Driver, to 'ClientUnderTest', which is required by SetExecutionModeOnAppUnderTest.clktmpl
  SetVariableOnCustomTestDriverClient(ATestDriverAddr, '$ExtraCaption$', 'ClientUnderTest');    //even after executing SetExecModeToServer.clktmpl, the caption should stay 'ClientUnderTest'
  SetVariableOnCustomTestDriverClient(ATestDriverAddr, '$ListeningPort$', AClientUnderTestServerPort);
  ExecuteTemplateOnCustomTestDriver(ATestDriverAddr, ATemplatesDir + 'SetExecModeToServer.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure PrepareCustomClickerUnderTestToLocalMode(ATestDriverAddr, ATemplatesDir: string);
begin
  //Set $ExtraCaption$ variable in Client Driver, to 'ClientUnderTest', which is required by SetExecutionModeOnAppUnderTest.clktmpl
  SetVariableOnCustomTestDriverClient(ATestDriverAddr, '$ExtraCaption$', 'ClientUnderTest');    //even after executing SetExecModeToServer.clktmpl, the caption should stay 'ClientUnderTest'
  ExecuteTemplateOnCustomTestDriver(ATestDriverAddr, ATemplatesDir + 'SetExecModeToLocal.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure PrepareCustomClickerUnderTestToClientMode(ATestDriverAddr, ATemplatesDir: string);
begin
  //Set $ExtraCaption$ variable in Client Driver, to 'ClientUnderTest', which is required by SetExecutionModeOnAppUnderTest.clktmpl
  SetVariableOnCustomTestDriverClient(ATestDriverAddr, '$ExtraCaption$', 'ClientUnderTest');    //even after executing SetExecModeToServer.clktmpl, the caption should stay 'ClientUnderTest'
  ExecuteTemplateOnCustomTestDriver(ATestDriverAddr, ATemplatesDir + 'SetExecModeToClient.clktmpl', CREParam_FileLocation_ValueDisk);
end;


end.

