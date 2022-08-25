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

  TTestHTTPAPI = class(TTestCase)
  private
    FInMemFS: TInMemFileSystem;

    function SendTemplateToServer(ARemoteAddress, AFileName: string; AFileContent: TMemoryStream): string;

  protected
    function SendTestFileToServer(ARemoteAddress, AFileName: string; AFileContent: string = 'Dummy'): string;

    procedure SetUp; override;
    procedure TearDown; override;

    procedure SendEmptyTemplateToServerThenLoad;

    procedure SendTemplateFromInMemToServerThenLoad(AFileName: string);
    procedure SendMultipleTestFilesToServer(AFileNames: PStrArr; AFileNamesLength: Integer; ABaseContent: string);
    function Send_ExecuteCommandAtIndex_ToServer(AActionIdx, AStackLevel: Integer): string;
    procedure CreateTestTemplateInMem;
    procedure SetupTargetWindowFor_FindSubControl;
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
  ClickerActionsClient, ClickerUtils, ActionsStuff, Controls;


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


procedure TTestHTTPAPI.SendTemplateFromInMemToServerThenLoad(AFileName: string);
var
  Content: TMemoryStream;
begin
  Content := TMemoryStream.Create;
  try
    FInMemFS.LoadFileFromMemToStream(AFileName, Content);
    SendTemplateToServer(CTestServerAddress, AFileName, Content);
  finally
    Content.Free;
  end;

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


procedure TTestHTTPAPI.SetupTargetWindowFor_FindSubControl;
var
  WindowOperationsOptions: TClkWindowOperationsOptions;
  FindControlOptions: TClkFindControlOptions;
  UIClickerMainHandle: THandle;
  Response: string;
  ListOfVars: TStringList;
begin
  GenerateFindControlOptionsForMainUIClickerWindow(FindControlOptions, False);
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


end.

