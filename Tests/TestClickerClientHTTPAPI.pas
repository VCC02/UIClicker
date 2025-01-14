{
    Copyright (C) 2024 VCC
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
  ClickerUtils, ClickerActionsClient, MemArchive;

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
  end;


implementation


uses
  Controls, ClickerActionProperties, DCPsha256, DCPmd5,
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
  FindControlOptions.MatchBitmapFiles := 'DummyFile.bmp';

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
  FindControlOptions: TClkFindControlOptions;
  FindControlOptionsAPI: TClkFindControlOptionsAPI;
  MatchBitmapTextRecAPI: TMatchBitmapTextRecAPI;
  FindControlMatchBitmapTextAPIArr: TClkFindControlMatchBitmapTextAPIArr;
  ActionName, FileLoc: WideString;
  Response: string;
  Th: TClientThread;
begin
  GetDefaultPropertyValues_FindControl(FindControlOptions, True);
  FindControlOptions.MatchText := '-bit';
  FindControlOptions.MatchBitmapFiles := 'FirstDummyFile.bmp' + #4#5 + 'SecondDummyFile.bmp' + #4#5;
  SetLength(FindControlOptions.MatchBitmapText, 2);
  FindControlOptions.MatchBitmapText[1] := FindControlOptions.MatchBitmapText[0];
  FindControlOptions.MatchBitmapText[1].ForegroundColor := '00FFFF';
  FindControlOptions.MatchBitmapText[1].BackgroundColor := '008000';
  FindControlOptions.MatchBitmapText[1].IgnoreBackgroundColor := True;
  FindControlOptions.MatchBitmapText[0].ProfileName := 'First';
  FindControlOptions.MatchBitmapText[1].ProfileName := 'Second';

  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My FindSubControl action';
  FileLoc := CREParam_FileLocation_ValueDisk;

  Th := AsyncExecTestTemplate(CTestDriverAddress, '$AppDir$\..\Tests\TestFiles\ClickDebuggingButton.clktmpl');
  try
    SetFindControlOptionsToAPI(FindControlOptions, FindControlOptionsAPI, MatchBitmapTextRecAPI, FindControlMatchBitmapTextAPIArr);
    ExecuteFindSubControlAction(@ActionName[1], 1000, @FindControlOptionsAPI, True, @FileLoc[1], @Response[1]);
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
begin
  GetDefaultPropertyValues_SetControlText(SetControlTextOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My SetControlText action';

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

initialization

  RegisterTest(TTestClickerClientHTTPAPI);

end.

