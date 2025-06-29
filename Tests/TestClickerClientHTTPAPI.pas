{
    Copyright (C) 2025 VCC
    creation date: Dec 2024
    initial release date: 14 Dec 2024

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


unit TestClickerClientHTTPAPI;

{$mode Delphi}{$H+}

interface

uses
  LCLIntf, Classes, SysUtils, TestHTTPAPI, fpcunit, testregistry, Expectations,
  ClickerUtils, ClickerActionsClient;

type
  TTestClickerClientHTTPAPI = class(TTestHTTPAPI)
  private
    FLoadClickerClientRes: Boolean;

    procedure WaitForDebuggingActionToFinish(AExecResponse: string; ATh: TClientThread);

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    constructor Create; override;
  published
    procedure Test_TestConnectionToServer;
    procedure StartTestDriver;
    procedure Test_ExecuteClickAction_With_UseServerDebugging;
    procedure Test_ExecuteExecAppAction_With_UseServerDebugging;
    procedure Test_ExecuteFindControlAction_With_UseServerDebugging;
    procedure Test_ExecuteFindSubControlAction_With_UseServerDebugging;
    procedure Test_ExecuteSetControlTextAction_With_UseServerDebugging;
    procedure Test_ExecuteCallTemplateAction_With_UseServerDebugging;
    procedure Test_ExecuteSleepAction_With_UseServerDebugging;
    procedure Test_ExecuteSetVarAction_With_UseServerDebugging;
    procedure Test_ExecuteWindowOperationsAction_With_UseServerDebugging;
    procedure Test_ExecuteLoadSetVarFromFileAction_With_UseServerDebugging;
    procedure Test_ExecuteSaveSetVarToFileAction_With_UseServerDebugging;
    procedure Test_ExecutePluginAction_With_UseServerDebugging;
    procedure Test_ExecuteEditTemplateAction_With_UseServerDebugging;

    procedure Test_AddClickActionToTemplate_HappyFlow;
    procedure Test_AddExecAppActionToTemplate_HappyFlow;
    procedure Test_AddFindControlActionToTemplate_HappyFlow;
    procedure Test_AddFindSubControlActionToTemplate_HappyFlow;
    procedure Test_AddSetControlTextActionToTemplate_HappyFlow;
    procedure Test_AddCallTemplateActionToTemplate_HappyFlow;
    procedure Test_AddSleepActionToTemplate_HappyFlow;
    procedure Test_AddSetVarActionToTemplate_HappyFlow;
    procedure Test_AddWindowOperationsActionToTemplate_HappyFlow;
    procedure Test_AddLoadSetVarFromFileActionToTemplate_HappyFlow;
    procedure Test_AddSaveSetVarToFileActionToTemplate_HappyFlow;
    procedure Test_AddPluginActionToTemplate_HappyFlow;
    procedure Test_AddEditTemplateActionToTemplate_HappyFlow;
  end;


implementation


uses
  Controls, ClickerActionProperties, //DCPsha256, DCPmd5,
  Graphics, DllUtils, ClickerClientAPI, ClickerClientIntf, ShellAPI;


const
  CTestDriverAddress = 'http://127.0.0.1:25444/';


constructor TTestClickerClientHTTPAPI.Create;
begin
  inherited Create;
  TestServerAddress := CTestServerAddress;
  FLoadClickerClientRes := False;
end;


procedure TTestClickerClientHTTPAPI.SetUp;
var
  RecServerAddress: string;
begin
  inherited SetUp;
  FLoadClickerClientRes := LoadClickerClient('..\ClickerClient\ClickerClient.dll');
  Expect(FLoadClickerClientRes).ToBe(True, 'Can''t load ClickerClient.dll');

  InitClickerClient;
  SetServerAddress(@WideString(TestServerAddress)[1]);

  SetLength(RecServerAddress, CMaxSharedStringLength);
  SetLength(RecServerAddress, GetServerAddress(@RecServerAddress[1]));

  Expect(RecServerAddress).ToBe(TestServerAddress, 'The configured server address does not match the expected one in ClickerClient.');
  SetVariable(TestServerAddress, '$ExecAction_Err$', '', 0);  //clear errors  - required if the debugged action should be successful
  SetVariable(CTestDriverAddress, '$ExtraCaption2$', '', 0);  //required for finding action window
end;


procedure TTestClickerClientHTTPAPI.TearDown;
begin
  if FLoadClickerClientRes then
  begin
    try
      DoneClickerClient;
    finally
      UnLoadClickerClient;
    end;
  end;

  inherited TearDown;
end;


procedure TTestClickerClientHTTPAPI.Test_TestConnectionToServer;
var
  Response: string;
begin
  SetLength(Response, CMaxSharedStringLength);
  SetLength(Response, TestConnectionToServer(@Response[1]));
  Expect(Response).ToBe(CREResp_ConnectionOK, 'Make sure UIClicker is running and is listening on the configured port.');
end;


procedure TTestClickerClientHTTPAPI.StartTestDriver;
var
  PathToDriver: string;
begin
  //ToDo use UI tests code for this. Also, create one more method, at the end, to close the process

  PathToDriver := ExtractFilePath(ParamStr(0)) + '..\TestDriver\UIClicker.exe' ;
  ShellExecute(0, 'open', PChar(PathToDriver), '--ExtraCaption MyServer --SetExecMode Server --ServerPort 25444', PChar(ExtractFilePath(PathToDriver)), 5);
end;


procedure TTestClickerClientHTTPAPI.WaitForDebuggingActionToFinish(AExecResponse: string; ATh: TClientThread);
begin
  Expect(AExecResponse).ToContain('$RemoteExecResponse$=', 'Response from the action being debugged.');
  WaitForServerResponse(ATh, True);
  ExpectSuccessfulAction(FastReplace_87ToReturn(ATh.Result));
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteClickAction_With_UseServerDebugging;
var
  ClickOptions: TClkClickOptions;
  ClickOptionsAPI: TClkClickOptionsAPI;
  Response: string;
  Th: TClientThread;
begin
  GetDefaultPropertyValues_Click(ClickOptions);
  SetLength(Response, CMaxSharedStringLength);

  SetVariable(TestServerAddress, '$Control_Left$', '1920', 0);  //prevent clicking on top-left, because the mouse action has '' for these two vars
  SetVariable(TestServerAddress, '$Control_Top$', '1080', 0);

  Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetClickOptionsToAPI(ClickOptions, ClickOptionsAPI);
    ExecuteClickAction(nil, 100, @ClickOptionsAPI, True, @Response[1]);
    WaitForDebuggingActionToFinish(Response, Th);
  finally
    Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteExecAppAction_With_UseServerDebugging;
var
  ExecAppOptions: TClkExecAppOptions;
  ExecAppOptionsAPI: TClkExecAppOptionsAPI;
  ActionName: WideString;
  Response: string;
  Th: TClientThread;
begin
  GetDefaultPropertyValues_ExecApp(ExecAppOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My ExecApp action';

  Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetExecAppOptionsToAPI(ExecAppOptions, ExecAppOptionsAPI);
    ExecuteExecAppAction(@ActionName[1], 100, @ExecAppOptionsAPI, True, @Response[1]);
    WaitForDebuggingActionToFinish(Response, Th);
  finally
    Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteFindControlAction_With_UseServerDebugging;
var
  FindControlOptions: TClkFindControlOptions;
  FindControlOptionsAPI: TClkFindControlOptionsAPI;
  MatchBitmapTextRecAPI: TMatchBitmapTextRecAPI;
  FindControlMatchBitmapTextAPIArr: TClkFindControlMatchBitmapTextAPIArr;
  ActionName, FileLoc: WideString;
  Response: string;
  Th: TClientThread;
begin
  GetDefaultPropertyValues_FindControl(FindControlOptions);
  FindControlOptions.MatchText := 'UI Clicker Main';
  FindControlOptions.MatchClassName := 'Window';

  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My FindControl action';
  FileLoc := CREParam_FileLocation_ValueDisk;

  Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetFindControlOptionsToAPI(FindControlOptions, FindControlOptionsAPI, MatchBitmapTextRecAPI, FindControlMatchBitmapTextAPIArr);
    ExecuteFindControlAction(@ActionName[1], 2000, @FindControlOptionsAPI, True, @FileLoc[1], @Response[1]);
    WaitForDebuggingActionToFinish(Response, Th);
  finally
    Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteFindSubControlAction_With_UseServerDebugging;
var
  FindSubControlOptions: TClkFindSubControlOptions;
  FindSubControlOptionsAPI: TClkFindSubControlOptionsAPI;
  MatchBitmapTextRecAPI: TMatchBitmapTextRecAPI;
  FindControlMatchBitmapTextAPIArr: TClkFindControlMatchBitmapTextAPIArr;
  ActionName, FileLoc: WideString;
  Response: string;
  Th: TClientThread;
begin
  GetDefaultPropertyValues_FindSubControl(FindSubControlOptions);
  FindSubControlOptions.MatchText := '-bit';
  FindSubControlOptions.MatchBitmapFiles := 'FirstDummyFile.bmp' + #4#5 + 'SecondDummyFile.bmp' + #4#5;
  SetLength(FindSubControlOptions.MatchBitmapText, 2);
  FindSubControlOptions.MatchBitmapText[1] := FindSubControlOptions.MatchBitmapText[0];
  FindSubControlOptions.MatchBitmapText[1].ForegroundColor := '00FFFF';
  FindSubControlOptions.MatchBitmapText[1].BackgroundColor := '008000';
  FindSubControlOptions.MatchBitmapText[1].IgnoreBackgroundColor := True;
  FindSubControlOptions.MatchBitmapText[0].ProfileName := 'First';
  FindSubControlOptions.MatchBitmapText[1].ProfileName := 'Second';

  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My FindSubControl action';
  FileLoc := CREParam_FileLocation_ValueDisk;

  Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetFindSubControlOptionsToAPI(FindSubControlOptions, FindSubControlOptionsAPI, MatchBitmapTextRecAPI, FindControlMatchBitmapTextAPIArr);
    ExecuteFindSubControlAction(@ActionName[1], 1000, @FindSubControlOptionsAPI, True, @FileLoc[1], @Response[1]);
    WaitForDebuggingActionToFinish(Response, Th);
  finally
    Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteSetControlTextAction_With_UseServerDebugging;
var
  SetControlTextOptions: TClkSetTextOptions;
  SetControlTextOptionsAPI: TClkSetTextOptionsAPI;
  ActionName: WideString;
  Response: string;
  Th: TClientThread;
  SetVarToResetHandle: TClkSetVarOptions;
begin
  GetDefaultPropertyValues_SetControlText(SetControlTextOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My SetControlText action';
  SetVarToResetHandle.ListOfVarNames := '$Control_Handle$';
  SetVarToResetHandle.ListOfVarValues := '0';
  SetVarToResetHandle.ListOfVarEvalBefore := '0';
  SetVarToResetHandle.FailOnException := True;

  ExpectSuccessfulAction(FastReplace_87ToReturn(ClickerActionsClient.ExecuteSetVarAction(CTestServerAddress, SetVarToResetHandle)));

  Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetSetControlTextOptionsToAPI(SetControlTextOptions, SetControlTextOptionsAPI);
    ExecuteSetControlTextAction(@ActionName[1], 100, @SetControlTextOptionsAPI, True, @Response[1]);
    WaitForDebuggingActionToFinish(Response, Th);
  finally
    Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteCallTemplateAction_With_UseServerDebugging;
var
  CallTemplateOptions: TClkCallTemplateOptions;
  CallTemplateOptionsAPI: TClkCallTemplateOptionsAPI;
  ActionName, FileLoc: WideString;
  Response: string;
  Th: TClientThread;
begin
  GetDefaultPropertyValues_CallTemplate(CallTemplateOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My CallTemplate action';
  FileLoc := CREParam_FileLocation_ValueDisk;

  Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetCallTemplateOptionsToAPI(CallTemplateOptions, CallTemplateOptionsAPI);
    ExecuteCallTemplateAction(@ActionName[1], 100, @CallTemplateOptionsAPI, True, @FileLoc[1], @Response[1]);
    WaitForDebuggingActionToFinish(Response, Th);
  finally
    Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteSleepAction_With_UseServerDebugging;
var
  SleepOptions: TClkSleepOptions;
  SleepOptionsAPI: TClkSleepOptionsAPI;
  ActionName: WideString;
  Response: string;
  Th: TClientThread;
begin
  GetDefaultPropertyValues_Sleep(SleepOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My Sleep action';

  Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetSleepOptionsToAPI(SleepOptions, SleepOptionsAPI);
    ExecuteSleepAction(@ActionName[1], 100, @SleepOptionsAPI, True, @Response[1]);
    WaitForDebuggingActionToFinish(Response, Th);
  finally
    Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteSetVarAction_With_UseServerDebugging;
var
  SetVarOptions: TClkSetVarOptions;
  SetVarOptionsAPI: TClkSetVarOptionsAPI;
  ActionName: WideString;
  Response: string;
  Th: TClientThread;
begin
  GetDefaultPropertyValues_SetVar(SetVarOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My SetVar action';

  Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetSetVarOptionsToAPI(SetVarOptions, SetVarOptionsAPI);
    ExecuteSetVarAction(@ActionName[1], 100, @SetVarOptionsAPI, True, @Response[1]);
    WaitForDebuggingActionToFinish(Response, Th);
  finally
    Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteWindowOperationsAction_With_UseServerDebugging;
var
  WindowOperationsOptions: TClkWindowOperationsOptions;
  WindowOperationsOptionsAPI: TClkWindowOperationsOptionsAPI;
  ActionName: WideString;
  Response: string;
  Th: TClientThread;
begin
  GetDefaultPropertyValues_WindowOperations(WindowOperationsOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My WindowOperations action';

  Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetWindowOperationsOptionsToAPI(WindowOperationsOptions, WindowOperationsOptionsAPI);
    ExecuteWindowOperationsAction(@ActionName[1], 100, @WindowOperationsOptionsAPI, True, @Response[1]);
    WaitForDebuggingActionToFinish(Response, Th);
  finally
    Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteLoadSetVarFromFileAction_With_UseServerDebugging;
var
  LoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions;
  LoadSetVarFromFileOptionsAPI: TClkLoadSetVarFromFileOptionsAPI;
  ActionName: WideString;
  Response: string;
  Th: TClientThread;
begin
  GetDefaultPropertyValues_LoadSetVarFromFile(LoadSetVarFromFileOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My LoadSetVarFromFile action';

  Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetLoadSetVarFromFileOptionsToAPI(LoadSetVarFromFileOptions, LoadSetVarFromFileOptionsAPI);
    ExecuteLoadSetVarFromFileAction(@ActionName[1], 100, @LoadSetVarFromFileOptionsAPI, True, @Response[1]);
    WaitForDebuggingActionToFinish(Response, Th);
  finally
    Th.Free;
  end;
end;



procedure TTestClickerClientHTTPAPI.Test_ExecuteSaveSetVarToFileAction_With_UseServerDebugging;
var
  SaveSetVarToFileOptions: TClkSaveSetVarToFileOptions;
  SaveSetVarToFileOptionsAPI: TClkSaveSetVarToFileOptionsAPI;
  ActionName: WideString;
  Response: string;
  Th: TClientThread;
begin
  GetDefaultPropertyValues_SaveSetVarToFile(SaveSetVarToFileOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My SaveSetVarToFile action';

  Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetSaveSetVarToFileOptionsToAPI(SaveSetVarToFileOptions, SaveSetVarToFileOptionsAPI);
    ExecuteSaveSetVarToFileAction(@ActionName[1], 100, @SaveSetVarToFileOptionsAPI, True, @Response[1]);
    WaitForDebuggingActionToFinish(Response, Th);
  finally
    Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_ExecutePluginAction_With_UseServerDebugging;
var
  PluginOptions: TClkPluginOptions;
  PluginOptionsAPI: TClkPluginOptionsAPI;
  ActionName: WideString;
  Response: string;
  Th: TClientThread;
begin
  GetDefaultPropertyValues_Plugin(PluginOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My Plugin action';

  Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetPluginOptionsToAPI(PluginOptions, PluginOptionsAPI);
    ExecutePluginAction(@ActionName[1], 100, @PluginOptionsAPI, True, True, @Response[1]);
    WaitForDebuggingActionToFinish(Response, Th);
  finally
    Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteEditTemplateAction_With_UseServerDebugging;
var
  EditTemplateOptions: TClkEditTemplateOptions;
  EditTemplateOptionsAPI: TClkEditTemplateOptionsAPI;
  ActionName: WideString;
  Response: string;
  Th: TClientThread;
begin
  GetDefaultPropertyValues_EditTemplate(EditTemplateOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My EditTemplate action';

  Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetEditTemplateOptionsToAPI(EditTemplateOptions, EditTemplateOptionsAPI);
    EditTemplateOptionsAPI.WhichTemplate := Byte(etwtSelf);  //remote execution of EditTemplate, with etwtOther expects a valid file (for 5min) to be sent
    ExecuteEditTemplateAction(@ActionName[1], 100, @EditTemplateOptionsAPI, True, @Response[1]);
    WaitForDebuggingActionToFinish(Response, Th);
  finally
    Th.Free;
  end;
end;


const
  CAddActionsTemplateFnm = 'TestAddActionsViaClickerClient.clktmpl';

function GetStandardResponseFrom_PrepareFilesInServer: string;
begin
  Result := CRECmd_LoadTemplateInExecList + ': ' + CREResp_TemplateLoaded + ',  SendMissingFilesToServer: ';
end;


procedure TTestClickerClientHTTPAPI.Test_AddClickActionToTemplate_HappyFlow;
var
  AddActionsTemplateFnm: WideString;
  ActionName: WideString;
  Response: string;
  ClickOptions: TClkClickOptions;
  ClickOptionsAPI: TClkClickOptionsAPI;
  //Th: TClientThread;
begin
  GetDefaultPropertyValues_Click(ClickOptions);
  SetLength(Response, CMaxSharedStringLength);

  SetVariable(TestServerAddress, '$Control_Left$', '1920', 0);  //prevent clicking on top-left, because the mouse action has '' for these two vars
  SetVariable(TestServerAddress, '$Control_Top$', '1080', 0);

  AddActionsTemplateFnm := CAddActionsTemplateFnm;
  ActionName := 'ClientClick';

  //Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetClickOptionsToAPI(ClickOptions, ClickOptionsAPI);
    Expect(AddClickActionToTemplate(@AddActionsTemplateFnm[1], @ActionName[1], 1000, True, nil, @ClickOptionsAPI)).ToBe(0);

    SetLength(Response, PrepareFilesInServer(@AddActionsTemplateFnm[1], @Response[1]));
    Expect(Response).ToBe(GetStandardResponseFrom_PrepareFilesInServer, 'Response from PrepareFilesInServer');
    //ToDo: execute a template in TestDriver, which clicks the action (to be selected), then starts action execution in debugging mode.
    //WaitForDebuggingActionToFinish('$RemoteExecResponse$=', Th);
  finally
    //Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_AddExecAppActionToTemplate_HappyFlow;
var
  AddActionsTemplateFnm: WideString;
  ActionName: WideString;
  Response: string;
  ExecAppOptions: TClkExecAppOptions;
  ExecAppOptionsAPI: TClkExecAppOptionsAPI;
  //Th: TClientThread;
begin
  GetDefaultPropertyValues_ExecApp(ExecAppOptions);
  SetLength(Response, CMaxSharedStringLength);

  AddActionsTemplateFnm := CAddActionsTemplateFnm;
  ActionName := 'ClientExecApp';

  //Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetExecAppOptionsToAPI(ExecAppOptions, ExecAppOptionsAPI);
    Expect(AddExecAppActionToTemplate(@AddActionsTemplateFnm[1], @ActionName[1], 1000, True, nil, @ExecAppOptionsAPI)).ToBe(0);

    SetLength(Response, PrepareFilesInServer(@AddActionsTemplateFnm[1], @Response[1]));
    Expect(Response).ToBe(GetStandardResponseFrom_PrepareFilesInServer, 'Response from PrepareFilesInServer');
    //ToDo: execute a template in TestDriver, which clicks the action (to be selected), then starts action execution in debugging mode.
    //WaitForDebuggingActionToFinish('$RemoteExecResponse$=', Th);
  finally
    //Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_AddFindControlActionToTemplate_HappyFlow;
var
  AddActionsTemplateFnm: WideString;
  ActionName: WideString;
  Response: string;
  FindControlOptions: TClkFindControlOptions;
  FindControlOptionsAPI: TClkFindControlOptionsAPI;
  MatchBitmapTextRecAPI: TMatchBitmapTextRecAPI;
  FindControlMatchBitmapTextAPIArr: TClkFindControlMatchBitmapTextAPIArr;
  //Th: TClientThread;
begin
  GetDefaultPropertyValues_FindControl(FindControlOptions);
  SetLength(Response, CMaxSharedStringLength);
  FindControlOptions.MatchText := 'UI Clicker Main';
  FindControlOptions.MatchClassName := 'Window';

  AddActionsTemplateFnm := CAddActionsTemplateFnm;
  ActionName := 'ClientFindControl';

  //Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetFindControlOptionsToAPI(FindControlOptions, FindControlOptionsAPI, MatchBitmapTextRecAPI, FindControlMatchBitmapTextAPIArr);
    Expect(AddFindControlActionToTemplate(@AddActionsTemplateFnm[1], @ActionName[1], 1000, True, nil, @FindControlOptionsAPI)).ToBe(0);

    SetLength(Response, PrepareFilesInServer(@AddActionsTemplateFnm[1], @Response[1]));
    Expect(Response).ToBe(GetStandardResponseFrom_PrepareFilesInServer, 'Response from PrepareFilesInServer');
    //ToDo: execute a template in TestDriver, which clicks the action (to be selected), then starts action execution in debugging mode.
    //WaitForDebuggingActionToFinish('$RemoteExecResponse$=', Th);
  finally
    //Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_AddFindSubControlActionToTemplate_HappyFlow;
var
  AddActionsTemplateFnm: WideString;
  ActionName: WideString;
  Response: string;
  FindSubControlOptions: TClkFindSubControlOptions;
  FindSubControlOptionsAPI: TClkFindSubControlOptionsAPI;
  MatchBitmapTextRecAPI: TMatchBitmapTextRecAPI;
  FindSubControlMatchBitmapTextAPIArr: TClkFindControlMatchBitmapTextAPIArr;
  //Th: TClientThread;
begin
  GetDefaultPropertyValues_FindSubControl(FindSubControlOptions);
  SetLength(Response, CMaxSharedStringLength);
  FindSubControlOptions.MatchText := '-bit';
  FindSubControlOptions.MatchBitmapFiles := 'FirstDummyFile.bmp' + #4#5 + 'SecondDummyFile.bmp' + #4#5;
  SetLength(FindSubControlOptions.MatchBitmapText, 2);
  FindSubControlOptions.MatchBitmapText[1] := FindSubControlOptions.MatchBitmapText[0];
  FindSubControlOptions.MatchBitmapText[1].ForegroundColor := '00FFFF';
  FindSubControlOptions.MatchBitmapText[1].BackgroundColor := '008000';
  FindSubControlOptions.MatchBitmapText[1].IgnoreBackgroundColor := True;
  FindSubControlOptions.MatchBitmapText[0].ProfileName := 'First';
  FindSubControlOptions.MatchBitmapText[1].ProfileName := 'Second';

  AddActionsTemplateFnm := CAddActionsTemplateFnm;
  ActionName := 'ClientFindSubControl';

  //Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetFindSubControlOptionsToAPI(FindSubControlOptions, FindSubControlOptionsAPI, MatchBitmapTextRecAPI, FindSubControlMatchBitmapTextAPIArr);
    Expect(AddFindSubControlActionToTemplate(@AddActionsTemplateFnm[1], @ActionName[1], 1000, True, nil, @FindSubControlOptionsAPI)).ToBe(0);

    SetLength(Response, PrepareFilesInServer(@AddActionsTemplateFnm[1], @Response[1]));
    Expect(Response).ToContain(GetStandardResponseFrom_PrepareFilesInServer, 'Response from PrepareFilesInServer');
    //ToDo: execute a template in TestDriver, which clicks the action (to be selected), then starts action execution in debugging mode.
    //WaitForDebuggingActionToFinish('$RemoteExecResponse$=', Th);
  finally
    //Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_AddSetControlTextActionToTemplate_HappyFlow;
var
  AddActionsTemplateFnm: WideString;
  ActionName: WideString;
  Response: string;
  SetControlTextOptions: TClkSetTextOptions;
  SetControlTextOptionsAPI: TClkSetTextOptionsAPI;
  //Th: TClientThread;
begin
  GetDefaultPropertyValues_SetControlText(SetControlTextOptions);
  SetLength(Response, CMaxSharedStringLength);

  AddActionsTemplateFnm := CAddActionsTemplateFnm;
  ActionName := 'ClientSetControlText';

  //Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetSetControlTextOptionsToAPI(SetControlTextOptions, SetControlTextOptionsAPI);
    Expect(AddSetControlTextActionToTemplate(@AddActionsTemplateFnm[1], @ActionName[1], 1000, True, nil, @SetControlTextOptionsAPI)).ToBe(0);

    SetLength(Response, PrepareFilesInServer(@AddActionsTemplateFnm[1], @Response[1]));
    Expect(Response).ToBe(GetStandardResponseFrom_PrepareFilesInServer, 'Response from PrepareFilesInServer');
    //ToDo: execute a template in TestDriver, which clicks the action (to be selected), then starts action execution in debugging mode.
    //WaitForDebuggingActionToFinish('$RemoteExecResponse$=', Th);
  finally
    //Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_AddCallTemplateActionToTemplate_HappyFlow;
var
  AddActionsTemplateFnm: WideString;
  ActionName: WideString;
  Response: string;
  CallTemplateOptions: TClkCallTemplateOptions;
  CallTemplateOptionsAPI: TClkCallTemplateOptionsAPI;
  //Th: TClientThread;
begin
  GetDefaultPropertyValues_CallTemplate(CallTemplateOptions);
  SetLength(Response, CMaxSharedStringLength);

  AddActionsTemplateFnm := CAddActionsTemplateFnm;
  ActionName := 'ClientCallTemplate';

  //Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetCallTemplateOptionsToAPI(CallTemplateOptions, CallTemplateOptionsAPI);
    Expect(AddCallTemplateActionToTemplate(@AddActionsTemplateFnm[1], @ActionName[1], 1000, True, nil, @CallTemplateOptionsAPI)).ToBe(0);

    SetLength(Response, PrepareFilesInServer(@AddActionsTemplateFnm[1], @Response[1]));
    Expect(Response).ToBe(GetStandardResponseFrom_PrepareFilesInServer, 'Response from PrepareFilesInServer');
    //ToDo: execute a template in TestDriver, which clicks the action (to be selected), then starts action execution in debugging mode.
    //WaitForDebuggingActionToFinish('$RemoteExecResponse$=', Th);
  finally
    //Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_AddSleepActionToTemplate_HappyFlow;
var
  AddActionsTemplateFnm: WideString;
  ActionName: WideString;
  Response: string;
  SleepOptions: TClkSleepOptions;
  SleepOptionsAPI: TClkSleepOptionsAPI;
  //Th: TClientThread;
begin
  GetDefaultPropertyValues_Sleep(SleepOptions);
  SetLength(Response, CMaxSharedStringLength);

  AddActionsTemplateFnm := CAddActionsTemplateFnm;
  ActionName := 'ClientSleep';

  //Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetSleepOptionsToAPI(SleepOptions, SleepOptionsAPI);
    Expect(AddSleepActionToTemplate(@AddActionsTemplateFnm[1], @ActionName[1], 1000, True, nil, @SleepOptionsAPI)).ToBe(0);

    SetLength(Response, PrepareFilesInServer(@AddActionsTemplateFnm[1], @Response[1]));
    Expect(Response).ToBe(GetStandardResponseFrom_PrepareFilesInServer, 'Response from PrepareFilesInServer');
    //ToDo: execute a template in TestDriver, which clicks the action (to be selected), then starts action execution in debugging mode.
    //WaitForDebuggingActionToFinish('$RemoteExecResponse$=', Th);
  finally
    //Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_AddSetVarActionToTemplate_HappyFlow;
var
  AddActionsTemplateFnm: WideString;
  ActionName: WideString;
  Response: string;
  SetVarOptions: TClkSetVarOptions;
  SetVarOptionsAPI: TClkSetVarOptionsAPI;
  //Th: TClientThread;
begin
  GetDefaultPropertyValues_SetVar(SetVarOptions);
  SetLength(Response, CMaxSharedStringLength);

  AddActionsTemplateFnm := CAddActionsTemplateFnm;
  ActionName := 'ClientSetVar';

  //Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetSetVarOptionsToAPI(SetVarOptions, SetVarOptionsAPI);
    Expect(AddSetVarActionToTemplate(@AddActionsTemplateFnm[1], @ActionName[1], 1000, True, nil, @SetVarOptionsAPI)).ToBe(0);

    SetLength(Response, PrepareFilesInServer(@AddActionsTemplateFnm[1], @Response[1]));
    Expect(Response).ToBe(GetStandardResponseFrom_PrepareFilesInServer, 'Response from PrepareFilesInServer');
    //ToDo: execute a template in TestDriver, which clicks the action (to be selected), then starts action execution in debugging mode.
    //WaitForDebuggingActionToFinish('$RemoteExecResponse$=', Th);
  finally
    //Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_AddWindowOperationsActionToTemplate_HappyFlow;
var
  AddActionsTemplateFnm: WideString;
  ActionName: WideString;
  Response: string;
  WindowOperationsOptions: TClkWindowOperationsOptions;
  WindowOperationsOptionsAPI: TClkWindowOperationsOptionsAPI;
  //Th: TClientThread;
begin
  GetDefaultPropertyValues_WindowOperations(WindowOperationsOptions);
  SetLength(Response, CMaxSharedStringLength);

  AddActionsTemplateFnm := CAddActionsTemplateFnm;
  ActionName := 'ClientWindowOperations';

  //Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetWindowOperationsOptionsToAPI(WindowOperationsOptions, WindowOperationsOptionsAPI);
    Expect(AddWindowOperationsActionToTemplate(@AddActionsTemplateFnm[1], @ActionName[1], 1000, True, nil, @WindowOperationsOptionsAPI)).ToBe(0);

    SetLength(Response, PrepareFilesInServer(@AddActionsTemplateFnm[1], @Response[1]));
    Expect(Response).ToBe(GetStandardResponseFrom_PrepareFilesInServer, 'Response from PrepareFilesInServer');
    //ToDo: execute a template in TestDriver, which clicks the action (to be selected), then starts action execution in debugging mode.
    //WaitForDebuggingActionToFinish('$RemoteExecResponse$=', Th);
  finally
    //Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_AddLoadSetVarFromFileActionToTemplate_HappyFlow;
var
  AddActionsTemplateFnm: WideString;
  ActionName: WideString;
  Response: string;
  LoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions;
  LoadSetVarFromFileOptionsAPI: TClkLoadSetVarFromFileOptionsAPI;
  //Th: TClientThread;
begin
  GetDefaultPropertyValues_LoadSetVarFromFile(LoadSetVarFromFileOptions);
  SetLength(Response, CMaxSharedStringLength);

  AddActionsTemplateFnm := CAddActionsTemplateFnm;
  ActionName := 'ClientLoadSetVarFromFile';

  //Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetLoadSetVarFromFileOptionsToAPI(LoadSetVarFromFileOptions, LoadSetVarFromFileOptionsAPI);
    Expect(AddLoadSetVarFromFileActionToTemplate(@AddActionsTemplateFnm[1], @ActionName[1], 1000, True, nil, @LoadSetVarFromFileOptionsAPI)).ToBe(0);

    SetLength(Response, PrepareFilesInServer(@AddActionsTemplateFnm[1], @Response[1]));
    Expect(Response).ToBe(GetStandardResponseFrom_PrepareFilesInServer, 'Response from PrepareFilesInServer');
    //ToDo: execute a template in TestDriver, which clicks the action (to be selected), then starts action execution in debugging mode.
    //WaitForDebuggingActionToFinish('$RemoteExecResponse$=', Th);
  finally
    //Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_AddSaveSetVarToFileActionToTemplate_HappyFlow;
var
  AddActionsTemplateFnm: WideString;
  ActionName: WideString;
  Response: string;
  SaveSetVarToFileOptions: TClkSaveSetVarToFileOptions;
  SaveSetVarToFileOptionsAPI: TClkSaveSetVarToFileOptionsAPI;
  //Th: TClientThread;
begin
  GetDefaultPropertyValues_SaveSetVarToFile(SaveSetVarToFileOptions);
  SetLength(Response, CMaxSharedStringLength);

  AddActionsTemplateFnm := CAddActionsTemplateFnm;
  ActionName := 'ClientSaveSetVarToFile';

  //Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetSaveSetVarToFileOptionsToAPI(SaveSetVarToFileOptions, SaveSetVarToFileOptionsAPI);
    Expect(AddSaveSetVarToFileActionToTemplate(@AddActionsTemplateFnm[1], @ActionName[1], 1000, True, nil, @SaveSetVarToFileOptionsAPI)).ToBe(0);

    SetLength(Response, PrepareFilesInServer(@AddActionsTemplateFnm[1], @Response[1]));
    Expect(Response).ToBe(GetStandardResponseFrom_PrepareFilesInServer, 'Response from PrepareFilesInServer');
    //ToDo: execute a template in TestDriver, which clicks the action (to be selected), then starts action execution in debugging mode.
    //WaitForDebuggingActionToFinish('$RemoteExecResponse$=', Th);
  finally
    //Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_AddPluginActionToTemplate_HappyFlow;
var
  AddActionsTemplateFnm: WideString;
  ActionName: WideString;
  Response: string;
  PluginOptions: TClkPluginOptions;
  PluginOptionsAPI: TClkPluginOptionsAPI;
  //Th: TClientThread;
begin
  GetDefaultPropertyValues_Plugin(PluginOptions);
  SetLength(Response, CMaxSharedStringLength);

  AddActionsTemplateFnm := CAddActionsTemplateFnm;
  ActionName := 'ClientPlugin';

  //Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetPluginOptionsToAPI(PluginOptions, PluginOptionsAPI);
    Expect(AddPluginActionToTemplate(@AddActionsTemplateFnm[1], @ActionName[1], 1000, True, nil, @PluginOptionsAPI)).ToBe(0);

    SetLength(Response, PrepareFilesInServer(@AddActionsTemplateFnm[1], @Response[1]));
    Expect(Response).ToBe(GetStandardResponseFrom_PrepareFilesInServer, 'Response from PrepareFilesInServer');
    //ToDo: execute a template in TestDriver, which clicks the action (to be selected), then starts action execution in debugging mode.
    //WaitForDebuggingActionToFinish('$RemoteExecResponse$=', Th);
  finally
    //Th.Free;
  end;
end;


procedure TTestClickerClientHTTPAPI.Test_AddEditTemplateActionToTemplate_HappyFlow;
var
  AddActionsTemplateFnm: WideString;
  ActionName: WideString;
  Response: string;
  EditTemplateOptions: TClkEditTemplateOptions;
  EditTemplateOptionsAPI: TClkEditTemplateOptionsAPI;
  //Th: TClientThread;
begin
  GetDefaultPropertyValues_EditTemplate(EditTemplateOptions);
  SetLength(Response, CMaxSharedStringLength);

  AddActionsTemplateFnm := CAddActionsTemplateFnm;
  ActionName := 'ClientEditTemplate';

  //Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetEditTemplateOptionsToAPI(EditTemplateOptions, EditTemplateOptionsAPI);
    EditTemplateOptionsAPI.WhichTemplate := Byte(etwtSelf);  //remote execution of EditTemplate, with etwtOther expects a valid file (for 5min) to be sent
    Expect(AddEditTemplateActionToTemplate(@AddActionsTemplateFnm[1], @ActionName[1], 1000, True, nil, @EditTemplateOptionsAPI)).ToBe(0);

    SetLength(Response, PrepareFilesInServer(@AddActionsTemplateFnm[1], @Response[1]));
    Expect(Response).ToBe(GetStandardResponseFrom_PrepareFilesInServer, 'Response from PrepareFilesInServer');
    //ToDo: execute a template in TestDriver, which clicks the action (to be selected), then starts action execution in debugging mode.
    //WaitForDebuggingActionToFinish('$RemoteExecResponse$=', Th);
  finally
    //Th.Free;
  end;
end;


initialization

  RegisterTest(TTestClickerClientHTTPAPI);

end.

