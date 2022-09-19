{
    Copyright (C) 2022 VCC
    creation date: Aug 2022
    initial release date: 14 Aug 2022

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


unit TestHTTPAPI;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, Classes, SysUtils, fpcunit, testregistry, InMemFileSystem, Expectations;

type
  TTerminateWaitingForFileAvailabilityRequestTh = class(TThread)
  private
    FDone: Boolean;
    FDelayBeforeRequest: Integer;
    FLoopType: string;
    FResult: string;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean;
                       const StackSize: SizeUInt = DefaultStackSize);
  end;


  TTestHTTPAPI = class(TTestCase)
  private
    FInMemFS: TInMemFileSystem;

    function SendTemplateToServer(ARemoteAddress, AFileName: string; AFileContent: TMemoryStream): string;

  protected
    function SendTestFileToServer(ARemoteAddress, AFileName: string; AFileContent: string = 'Dummy'): string;

    procedure SetUp; override;
    procedure TearDown; override;

    procedure SendEmptyTemplateToServerThenLoad;

    procedure SendTemplateFromInMemToServer(AFileName: string);
    procedure SendTemplateFromInMemToServerThenLoad(AFileName: string);
    procedure SendMultipleTestFilesToServer(AFileNames: PStrArr; AFileNamesLength: Integer; ABaseContent: string);
    function Send_ExecuteCommandAtIndex_ToServer(AActionIdx, AStackLevel: Integer): string;
    procedure CreateTestTemplateInMem;
    procedure CreateCallableTestTemplateInMem(ATestTemplateFileName, AVarName, AVarValue: string; ASleepValue: string = '0'; AEvalBefore: Boolean = False);
    procedure CreateCallableTestTemplateInMem_WithCallTemplate(ATestTemplateFileName, AVarName, AVarValue: string; ACalledTemplateName, AListOfVarsAndValues: string; AEvalBefore: Boolean; ASleepValue: string = '0');
    procedure SetupTargetWindowFor_FindSubControl(ACustomFormCaption: string = 'UI Clicker Main');
    procedure SendTerminateWaitingForFileAvailabilityRequest(ALoopType: string; ADelayBeforeRequest: Integer);
    procedure ExecuteSetControlTextActionWithMainUIClickerWindow(ASearchedCaption, ASetCaption: string);

    function GetVarValueFromServer(AVarName: string): string;
  end;


const
  CTestServerAddress = 'http://127.0.0.1:5444/';
  CTestTemplateFileName = 'TestTemplate.clktmpl';
  CEmptyTestTemplateFileName = 'EmptyTestTemplate.clktmpl';
  CExpectedBadStackLevelResponse: string = '[Server error] Stack level out of bounds: 10. This happens when there is no template loaded in a new tab (with the requested stack level), as a result of "call template" action. It is also possible that the template is loaded, then exited before being executed.';


procedure ExpectSuccessfulAction(AListOfVars: TStringList);
procedure ExpectFailedAction(AListOfVars: TStringList; APartOfErrorMessage: string);
procedure ExpectAllowedFailedAction(AListOfVars: TStringList);


implementation


uses
  ClickerActionsClient, ClickerUtils, ActionsStuff, Controls, Forms;


procedure ExpectSuccessfulAction(AListOfVars: TStringList);
begin
  Expect(AListOfVars).WithItem('$ExecAction_Err$', 'list of vars').OfValue('', 'No error Allowed.');
  Expect(AListOfVars).WithItem(CREResp_RemoteExecResponseVar, 'list of vars').OfValue('1', 'Expected successful action.');
  Expect(AListOfVars).WithItem('$LastAction_Status$').OfValue('Successful');
end;


procedure ExpectFailedAction(AListOfVars: TStringList; APartOfErrorMessage: string);
begin
  Expect(AListOfVars).WithItem('$ExecAction_Err$', 'list of vars').ToContain(APartOfErrorMessage, 'An error is expected.');
  Expect(AListOfVars).WithItem(CREResp_RemoteExecResponseVar, 'list of vars').OfValue('0', 'Expected failed action.');
  Expect(AListOfVars).WithItem('$LastAction_Status$').OfValue('Failed');
end;


procedure ExpectAllowedFailedAction(AListOfVars: TStringList);
begin
  Expect(AListOfVars).WithItem('$ExecAction_Err$', 'list of vars').OfValue('');
  Expect(AListOfVars).WithItem(CREResp_RemoteExecResponseVar, 'list of vars').OfValue('0', 'Expected allowed failed action.');
  Expect(AListOfVars).WithItem('$LastAction_Status$').OfValue('Allowed Failed');
end;


constructor TTerminateWaitingForFileAvailabilityRequestTh.Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
begin
  inherited Create(CreateSuspended, StackSize);
  FDone := False;
end;


procedure TTerminateWaitingForFileAvailabilityRequestTh.Execute;
var
  tk: QWord;
begin
  try
    tk := GetTickCount64;
    repeat
      Sleep(2);
    until GetTickCount64 - tk > FDelayBeforeRequest;

    try
      FResult := TerminateWaitingForFileAvailability(CTestServerAddress, FLoopType, False);

      if FResult <> CREResp_Done then
        raise Exception.Create(FResult);
    finally
      Free;
    end;
  except
    on E: Exception do
      FResult := E.Message;
  end;
end;


procedure TTestHTTPAPI.SetUp;
begin
  FInMemFS := TInMemFileSystem.Create;
end;


procedure TTestHTTPAPI.TearDown;
begin
  FreeAndNil(FInMemFS);
end;


function TTestHTTPAPI.SendTestFileToServer(ARemoteAddress, AFileName: string; AFileContent: string = 'Dummy'): string;
var
  Content: TMemoryStream;
  Link: string;
begin
  Link := ARemoteAddress + CRECmd_SendFileToServer + '?' +
                           CREParam_FileName + '=' + AFileName;

  Content := TMemoryStream.Create;
  try
    Content.Write(AFileContent[1], Length(AFileContent));
    Result := SendFileToServer(Link, Content);
  finally
    Content.Free;
  end;

  Expect(Result).ToBe('Received file: "' + AFileName + '"  of ' + IntToStr(Length(AFileContent)) + ' bytes in size.');
  Expect(GetFileExistenceOnServer(CTestServerAddress, AFileName, False)).ToBe(True);
end;


function TTestHTTPAPI.SendTemplateToServer(ARemoteAddress, AFileName: string; AFileContent: TMemoryStream): string;
var
  Link: string;
begin
  Link := ARemoteAddress + CRECmd_SendFileToServer + '?' +
                           CREParam_FileName + '=' + AFileName;

  Result := SendFileToServer(Link, AFileContent);

  Expect(Result).ToBe('Received file: "' + AFileName + '"  of ' + IntToStr(AFileContent.Size) + ' bytes in size.');
  Expect(GetFileExistenceOnServer(CTestServerAddress, AFileName, False)).ToBe(True);
end;


procedure TTestHTTPAPI.SendEmptyTemplateToServerThenLoad;
var
  Content: TMemoryStream;
begin
  Content := TMemoryStream.Create;
  try
    SendTemplateToServer(CTestServerAddress, CEmptyTestTemplateFileName, Content);
  finally
    Content.Free;
  end;

  Expect(SendLoadTemplateInExecListRequest(CTestServerAddress, CEmptyTestTemplateFileName, 0)).ToBe(CREResp_TemplateLoaded);
end;


procedure TTestHTTPAPI.SendTemplateFromInMemToServer(AFileName: string);
var
  Content: TMemoryStream;
begin
  Content := TMemoryStream.Create;
  try
    FInMemFS.LoadFileFromMemToStream(AFileName, Content);
    Expect(SendTemplateToServer(CTestServerAddress, AFileName, Content)).ToContain('Received file: "' + AFileName + '"');
  finally
    Content.Free;
  end;
end;


procedure TTestHTTPAPI.SendTemplateFromInMemToServerThenLoad(AFileName: string);
begin
  SendTemplateFromInMemToServer(AFileName);
  Expect(SendLoadTemplateInExecListRequest(CTestServerAddress, AFileName, 0)).ToBe(CREResp_TemplateLoaded);
end;


procedure TTestHTTPAPI.SendMultipleTestFilesToServer(AFileNames: PStrArr; AFileNamesLength: Integer; ABaseContent: string);
var
  i: Integer;
begin
  for i := 0 to AFileNamesLength - 1 do
    SendTestFileToServer(CTestServerAddress, AFileNames^[i], ABaseContent + IntToStr(i + 1));  //i + 1, to match the already existing hashes
end;


procedure TTestHTTPAPI.CreateTestTemplateInMem;
var
  ClickOptions: TClkClickOptions;
begin
  GetDefaultClickOptions(ClickOptions);
  ClickOptions.XClickPointReference := xrefVar;
  ClickOptions.YClickPointReference := yrefVar;
  ClickOptions.XClickPointVar := '$ObjLeft$';
  ClickOptions.YClickPointVar := '$ObjTop$';
  ClickOptions.MoveWithoutClick := True;

  AddClickActionToTemplate(CTestTemplateFileName, '1', 0, True, '', ClickOptions, FInMemFS);
end;


procedure TTestHTTPAPI.CreateCallableTestTemplateInMem(ATestTemplateFileName, AVarName, AVarValue: string; ASleepValue: string = '0'; AEvalBefore: Boolean = False);
var
  SetVarOptions: TClkSetVarOptions;
  SleepOptions: TClkSleepOptions;
begin
  GenerateSetVarOptions_OneVar(SetVarOptions, AVarName, AVarValue, AEvalBefore);
  GenerateSleepOptions(SleepOptions, ASleepValue);
  AddSetVarActionToTemplate(ATestTemplateFileName, '1', 0, True, '', SetVarOptions, FInMemFS);
  AddSleepActionToTemplate(ATestTemplateFileName, 'dbg', 0, True, '', SleepOptions, FInMemFS);
end;


procedure TTestHTTPAPI.CreateCallableTestTemplateInMem_WithCallTemplate(ATestTemplateFileName, AVarName, AVarValue: string; ACalledTemplateName, AListOfVarsAndValues: string; AEvalBefore: Boolean; ASleepValue: string = '0');
var
  CallTemplateOptions: TClkCallTemplateOptions;
begin
  CreateCallableTestTemplateInMem(ATestTemplateFileName, AVarName, AVarValue, ASleepValue);

  GenerateCallTemplateOptions(CallTemplateOptions, ACalledTemplateName, AListOfVarsAndValues, AEvalBefore);
  AddCallTemplateActionToTemplate(ATestTemplateFileName, 'call ' + ATestTemplateFileName, 0, True, '', CallTemplateOptions, FInMemFS);
end;


function TTestHTTPAPI.Send_ExecuteCommandAtIndex_ToServer(AActionIdx, AStackLevel: Integer): string;
var
  Link: string;
begin
  Link := CTestServerAddress + CRECmd_ExecuteCommandAtIndex + '?' +
                               CREParam_ActionIdx + '=' + IntToStr(AActionIdx) + '&' +
                               CREParam_StackLevel + '=' + IntToStr(AStackLevel) + '&' +
                               CREParam_IsDebugging + '=' + IntToStr(0) + '&' +
                               CREParam_FileLocation + '= ' + CREParam_FileLocation_ValueMem;

  Result := SendTextRequestToServer(Link);
end;


procedure TTestHTTPAPI.SetupTargetWindowFor_FindSubControl(ACustomFormCaption: string = 'UI Clicker Main');
var
  WindowOperationsOptions: TClkWindowOperationsOptions;
  FindControlOptions: TClkFindControlOptions;
  UIClickerMainHandle: THandle;
  Response: string;
  ListOfVars: TStringList;
begin
  GenerateFindControlOptionsForMainUIClickerWindow(FindControlOptions, False, ACustomFormCaption);
  Response := FastReplace_87ToReturn(ExecuteFindControlAction(CTestServerAddress, FindControlOptions, 'Setup UIClicker Main', 1000, CREParam_FileLocation_ValueMem));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    Expect(ListOfVars).WithItem('$LastAction_Status$').DifferentThanValue(CActionStatusStr[asFailed]);
    UIClickerMainHandle := StrToIntDef(ListOfVars.Values['$Control_Handle$'], 0);
  finally
    ListOfVars.Free;
  end;

  //Can't use ExecuteWindowOperationsAction(woBringToFront), because the target window belongs to the same process (UIClicker). See: https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-setforegroundwindow
  //GenerateWindowOperationsOptionsForFindControlSetup(WindowOperationsOptions, woBringToFront);
  //ExecuteWindowOperationsAction(CTestServerAddress, WindowOperationsOptions);
  SetForegroundWindow(UIClickerMainHandle);

  GenerateWindowOperationsOptionsForFindControlSetup(WindowOperationsOptions, woMoveResize);
  ExecuteWindowOperationsAction(CTestServerAddress, WindowOperationsOptions);
end;


procedure TTestHTTPAPI.SendTerminateWaitingForFileAvailabilityRequest(ALoopType: string; ADelayBeforeRequest: Integer);
var
  Th: TTerminateWaitingForFileAvailabilityRequestTh;
begin
  Th := TTerminateWaitingForFileAvailabilityRequestTh.Create(True);

  Th.FLoopType := ALoopType;

  if ADelayBeforeRequest < 0 then
    ADelayBeforeRequest := 0;

  Th.FDelayBeforeRequest := ADelayBeforeRequest;
  Th.FreeOnTerminate := False; //True;
  Th.Start;
end;


procedure TTestHTTPAPI.ExecuteSetControlTextActionWithMainUIClickerWindow(ASearchedCaption, ASetCaption: string);
var
  Response: string;
  ListOfVars: TStringList;
  SetTextOptions: TClkSetTextOptions;
begin
  SetupTargetWindowFor_FindSubControl(ASearchedCaption); //find main UIClicker window
  GenerateSetControlTextOptions(SetTextOptions, ASetCaption, stEditBox);

  Response := FastReplace_87ToReturn(ExecuteSetControlTextAction(CTestServerAddress, SetTextOptions));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    ExpectSuccessfulAction(ListOfVars);
  finally
    ListOfVars.Free;
  end;
end;


function TTestHTTPAPI.GetVarValueFromServer(AVarName: string): string;
var
  Response: string;
  ListOfVars: TStringList;
begin
  Response := FastReplace_87ToReturn(GetAllReplacementVars(CTestServerAddress, 0));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    Result := ListOfVars.Values[AVarName];
  finally
    ListOfVars.Free;
  end;
end;

end.

