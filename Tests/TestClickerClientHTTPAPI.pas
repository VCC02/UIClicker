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
  ClickerUtils;

type
  TTestClickerClientHTTPAPI = class(TTestHTTPAPI)
  private
    FClickerClientDllHandle: THandle;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    constructor Create; override;
  published
    procedure Test_TestConnectionToServer;
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
  ClickerActionsClient, ActionsStuff, Controls, ClickerFileProviderClient, ClickerActionProperties,
  Graphics, DllUtils, ClickerClientAPI;

type
  TInitClickerClient_Proc = procedure; cdecl;
  TDoneClickerClient_Proc = procedure; cdecl;
  TSetServerAddress_Proc = procedure(AAddress: Pointer); cdecl;
  TGetServerAddress_Proc = function(AResponse: Pointer): LongInt; cdecl;
  TTestConnectionToServer_Proc = function(AResponse: Pointer): LongInt; cdecl;

  TExecuteClickAction_Proc = function(AActionName: Pointer;
                                      AActionTimeout: LongInt; //ms
                                      AClickOptions: PClkClickOptionsAPI;
                                      AUseServerDebugging: Boolean;
                                      AResultStr: Pointer): LongInt; cdecl;

  TExecuteExecAppAction_Proc = function(AActionName: Pointer;
                                        AActionTimeout: LongInt; //ms
                                        AExecAppOptions: PClkExecAppOptionsAPI;
                                        AUseServerDebugging: Boolean;
                                        AResultStr: Pointer): LongInt; cdecl;

  TExecuteFindControlAction_Proc = function(AActionName: Pointer;
                                            AActionTimeout: LongInt; //ms
                                            AFindControlOptions: PClkFindControlOptionsAPI;
                                            AUseServerDebugging: Boolean;
                                            AFileLocation: Pointer;
                                            AResultStr: Pointer): LongInt; cdecl;

  TExecuteSetControlTextAction_Proc = function(AActionName: Pointer;
                                               AActionTimeout: LongInt; //ms
                                               ASetControlTextOptions: PClkSetTextOptionsAPI;
                                               AUseServerDebugging: Boolean;
                                               AResultStr: Pointer): LongInt; cdecl;

  TExecuteCallTemplateAction_Proc = function(AActionName: Pointer;
                                             AActionTimeout: LongInt; //ms
                                             ASetControlTextOptions: PClkCallTemplateOptionsAPI;
                                             AUseServerDebugging: Boolean;
                                             AFileLocation: Pointer;
                                             AResultStr: Pointer): LongInt; cdecl;

  TExecuteSleepAction_Proc = function(AActionName: Pointer;
                                      AActionTimeout: LongInt; //ms
                                      ASleepOptions: PClkSleepOptionsAPI;
                                      AUseServerDebugging: Boolean;
                                      AResultStr: Pointer): LongInt; cdecl;

  TExecuteSetVarAction_Proc = function(AActionName: Pointer;
                                       AActionTimeout: LongInt; //ms
                                       ASetVarOptions: PClkSetVarOptionsAPI;
                                       AUseServerDebugging: Boolean;
                                       AResultStr: Pointer): LongInt; cdecl;

  TExecuteWindowOperationsAction_Proc = function(AActionName: Pointer;
                                                 AActionTimeout: LongInt; //ms
                                                 AWindowOperationsOptions: PClkWindowOperationsOptionsAPI;
                                                 AUseServerDebugging: Boolean;
                                                 AResultStr: Pointer): LongInt; cdecl;

  TExecuteLoadSetVarFromFileAction_Proc = function(AActionName: Pointer;
                                                   AActionTimeout: LongInt; //ms
                                                   ALoadSetVarFromFileOptions: PClkLoadSetVarFromFileOptionsAPI;
                                                   AUseServerDebugging: Boolean;
                                                   AResultStr: Pointer): LongInt; cdecl;

  TExecuteSaveSetVarToFileAction_Proc = function(AActionName: Pointer;
                                                 AActionTimeout: LongInt; //ms
                                                 ASaveSetVarToFileOptions: PClkSaveSetVarToFileOptionsAPI;
                                                 AUseServerDebugging: Boolean;
                                                 AResultStr: Pointer): LongInt; cdecl;

   TExecutePluginAction_Proc = function(AActionName: Pointer;
                                        AActionTimeout: LongInt; //ms
                                        APluginOptions: PClkPluginOptionsAPI;
                                        AUseServerDebugging: Boolean;
                                        AUseStepIntoDebugging: Boolean;
                                        AResultStr: Pointer): LongInt; cdecl;

   TExecuteEditTemplateAction_Proc = function(AActionName: Pointer;
                                              AActionTimeout: LongInt; //ms
                                              AEditTemplateOptions: PClkEditTemplateOptionsAPI;
                                              AUseServerDebugging: Boolean;
                                              AResultStr: Pointer): LongInt; cdecl;

var
  InitClickerClient: TInitClickerClient_Proc;
  DoneClickerClient: TDoneClickerClient_Proc;
  SetServerAddress: TSetServerAddress_Proc;
  GetServerAddress: TGetServerAddress_Proc;
  TestConnectionToServer: TTestConnectionToServer_Proc;

  ExecuteClickAction: TExecuteClickAction_Proc;
  ExecuteExecAppAction: TExecuteExecAppAction_Proc;
  ExecuteFindControlAction: TExecuteFindControlAction_Proc;
  ExecuteFindSubControlAction: TExecuteFindControlAction_Proc;
  ExecuteSetControlTextAction: TExecuteSetControlTextAction_Proc;
  ExecuteCallTemplateAction: TExecuteCallTemplateAction_Proc;
  ExecuteSleepAction: TExecuteSleepAction_Proc;
  ExecuteSetVarAction: TExecuteSetVarAction_Proc;
  ExecuteWindowOperationsAction: TExecuteWindowOperationsAction_Proc;
  ExecuteLoadSetVarFromFileAction: TExecuteLoadSetVarFromFileAction_Proc;
  ExecuteSaveSetVarToFileAction: TExecuteSaveSetVarToFileAction_Proc;
  ExecutePluginAction: TExecutePluginAction_Proc;
  ExecuteEditTemplateAction: TExecuteEditTemplateAction_Proc;


constructor TTestClickerClientHTTPAPI.Create;
begin
  inherited Create;
  TestServerAddress := CTestServerAddress;
end;


procedure TTestClickerClientHTTPAPI.SetUp;
var
  RecServerAddress: string;
begin
  inherited SetUp;
  FClickerClientDllHandle := LoadLibrary('..\ClickerClient\ClickerClient.dll');

  Expect(DWord(FClickerClientDllHandle)).NotToBe(0, 'Can''t load ClickerClient.dll');

  @InitClickerClient := GetProcAddress(FClickerClientDllHandle, 'InitClickerClient');
  @DoneClickerClient := GetProcAddress(FClickerClientDllHandle, 'DoneClickerClient');
  @SetServerAddress := GetProcAddress(FClickerClientDllHandle, 'SetServerAddress');
  @GetServerAddress := GetProcAddress(FClickerClientDllHandle, 'GetServerAddress');
  @TestConnectionToServer := GetProcAddress(FClickerClientDllHandle, 'TestConnectionToServer');

  @ExecuteClickAction := GetProcAddress(FClickerClientDllHandle, 'ExecuteClickAction');
  @ExecuteExecAppAction := GetProcAddress(FClickerClientDllHandle, 'ExecuteExecAppAction');
  @ExecuteFindControlAction := GetProcAddress(FClickerClientDllHandle, 'ExecuteFindControlAction');
  @ExecuteFindSubControlAction := GetProcAddress(FClickerClientDllHandle, 'ExecuteFindSubControlAction');
  @ExecuteSetControlTextAction := GetProcAddress(FClickerClientDllHandle, 'ExecuteSetControlTextAction');
  @ExecuteCallTemplateAction := GetProcAddress(FClickerClientDllHandle, 'ExecuteCallTemplateAction');
  @ExecuteSleepAction := GetProcAddress(FClickerClientDllHandle, 'ExecuteSleepAction');
  @ExecuteSetVarAction := GetProcAddress(FClickerClientDllHandle, 'ExecuteSetVarAction');
  @ExecuteWindowOperationsAction := GetProcAddress(FClickerClientDllHandle, 'ExecuteWindowOperationsAction');
  @ExecuteLoadSetVarFromFileAction := GetProcAddress(FClickerClientDllHandle, 'ExecuteLoadSetVarFromFileAction');
  @ExecuteSaveSetVarToFileAction := GetProcAddress(FClickerClientDllHandle, 'ExecuteSaveSetVarToFileAction');
  @ExecutePluginAction := GetProcAddress(FClickerClientDllHandle, 'ExecutePluginAction');
  @ExecuteEditTemplateAction := GetProcAddress(FClickerClientDllHandle, 'ExecuteEditTemplateAction');

  InitClickerClient;
  SetServerAddress(@WideString(TestServerAddress)[1]);

  SetLength(RecServerAddress, CMaxSharedStringLength);
  SetLength(RecServerAddress, GetServerAddress(@RecServerAddress[1]));

  Expect(RecServerAddress).ToBe(TestServerAddress, 'The configured server address does not match the expected one in ClickerClient.');
end;


procedure TTestClickerClientHTTPAPI.TearDown;
begin
  try
    if FClickerClientDllHandle > 0 then
      DoneClickerClient;
  finally
    FreeLibrary(FClickerClientDllHandle);
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


procedure TTestClickerClientHTTPAPI.Test_ExecuteClickAction_With_UseServerDebugging;
var
  ClickOptions: TClkClickOptions;
  ClickOptionsAPI: TClkClickOptionsAPI;
  Response: string;
begin
  GetDefaultPropertyValues_Click(ClickOptions);
  SetLength(Response, CMaxSharedStringLength);

  SetClickOptionsToAPI(ClickOptions, ClickOptionsAPI);
  ExecuteClickAction(nil, 100, @ClickOptionsAPI, True, @Response[1]);
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteExecAppAction_With_UseServerDebugging;
var
  ExecAppOptions: TClkExecAppOptions;
  ExecAppOptionsAPI: TClkExecAppOptionsAPI;
  ActionName: WideString;
  Response: string;
begin
  GetDefaultPropertyValues_ExecApp(ExecAppOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My ExecApp action';

  //ToDo: start a second thread, which runs a template, capable of stopping or stepping over the current action
  //////////////

  SetExecAppOptionsToAPI(ExecAppOptions, ExecAppOptionsAPI);
  ExecuteExecAppAction(@ActionName[1], 100, @ExecAppOptionsAPI, True, @Response[1]);
  Expect(Response).ToContain('$RemoteExecResponse$=');
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteFindControlAction_With_UseServerDebugging;
var
  FindControlOptions: TClkFindControlOptions;
  FindControlOptionsAPI: TClkFindControlOptionsAPI;
  MatchBitmapTextRecAPI: TMatchBitmapTextRecAPI;
  FindControlMatchBitmapTextAPIArr: TClkFindControlMatchBitmapTextAPIArr;
  ActionName, FileLoc: WideString;
  Response: string;
begin
  GetDefaultPropertyValues_FindControl(FindControlOptions);
  FindControlOptions.MatchText := 'UI Clicker Main';
  FindControlOptions.MatchClassName := 'Window';
  FindControlOptions.MatchBitmapFiles := 'DummyFile.bmp';

  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My FindControl action';
  FileLoc := CREParam_FileLocation_ValueDisk;

  SetFindControlOptionsToAPI(FindControlOptions, FindControlOptionsAPI, MatchBitmapTextRecAPI, FindControlMatchBitmapTextAPIArr);
  ExecuteFindControlAction(@ActionName[1], 2000, @FindControlOptionsAPI, True, @FileLoc[1], @Response[1]);
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteFindSubControlAction_With_UseServerDebugging;
var
  FindControlOptions: TClkFindControlOptions;
  FindControlOptionsAPI: TClkFindControlOptionsAPI;
  MatchBitmapTextRecAPI: TMatchBitmapTextRecAPI;
  FindControlMatchBitmapTextAPIArr: TClkFindControlMatchBitmapTextAPIArr;
  ActionName, FileLoc: WideString;
  Response: string;
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

  SetFindControlOptionsToAPI(FindControlOptions, FindControlOptionsAPI, MatchBitmapTextRecAPI, FindControlMatchBitmapTextAPIArr);
  ExecuteFindSubControlAction(@ActionName[1], 1000, @FindControlOptionsAPI, True, @FileLoc[1], @Response[1]);
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteSetControlTextAction_With_UseServerDebugging;
var
  SetControlTextOptions: TClkSetTextOptions;
  SetControlTextOptionsAPI: TClkSetTextOptionsAPI;
  ActionName: WideString;
  Response: string;
begin
  GetDefaultPropertyValues_SetControlText(SetControlTextOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My SetControlText action';

  SetSetControlTextOptionsToAPI(SetControlTextOptions, SetControlTextOptionsAPI);
  ExecuteSetControlTextAction(@ActionName[1], 100, @SetControlTextOptionsAPI, True, @Response[1]);
  Expect(Response).ToContain('$RemoteExecResponse$=');
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteCallTemplateAction_With_UseServerDebugging;
var
  CallTemplateOptions: TClkCallTemplateOptions;
  CallTemplateOptionsAPI: TClkCallTemplateOptionsAPI;
  ActionName, FileLoc: WideString;
  Response: string;
begin
  GetDefaultPropertyValues_CallTemplate(CallTemplateOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My CallTemplate action';
  FileLoc := CREParam_FileLocation_ValueDisk;

  SetCallTemplateOptionsToAPI(CallTemplateOptions, CallTemplateOptionsAPI);
  ExecuteCallTemplateAction(@ActionName[1], 100, @CallTemplateOptionsAPI, True, @FileLoc[1], @Response[1]);
  Expect(Response).ToContain('$RemoteExecResponse$=');
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteSleepAction_With_UseServerDebugging;
var
  SleepOptions: TClkSleepOptions;
  SleepOptionsAPI: TClkSleepOptionsAPI;
  ActionName: WideString;
  Response: string;
begin
  GetDefaultPropertyValues_Sleep(SleepOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My Sleep action';

  SetSleepOptionsToAPI(SleepOptions, SleepOptionsAPI);
  ExecuteSleepAction(@ActionName[1], 100, @SleepOptionsAPI, True, @Response[1]);
  Expect(Response).ToContain('$RemoteExecResponse$=');
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteSetVarAction_With_UseServerDebugging;
var
  SetVarOptions: TClkSetVarOptions;
  SetVarOptionsAPI: TClkSetVarOptionsAPI;
  ActionName: WideString;
  Response: string;
begin
  GetDefaultPropertyValues_SetVar(SetVarOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My SetVar action';

  SetSetVarOptionsToAPI(SetVarOptions, SetVarOptionsAPI);
  ExecuteSetVarAction(@ActionName[1], 100, @SetVarOptionsAPI, True, @Response[1]);
  Expect(Response).ToContain('$RemoteExecResponse$=');
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteWindowOperationsAction_With_UseServerDebugging;
var
  WindowOperationsOptions: TClkWindowOperationsOptions;
  WindowOperationsOptionsAPI: TClkWindowOperationsOptionsAPI;
  ActionName: WideString;
  Response: string;
begin
  GetDefaultPropertyValues_WindowOperations(WindowOperationsOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My WindowOperations action';

  SetWindowOperationsOptionsToAPI(WindowOperationsOptions, WindowOperationsOptionsAPI);
  ExecuteWindowOperationsAction(@ActionName[1], 100, @WindowOperationsOptionsAPI, True, @Response[1]);
  Expect(Response).ToContain('$RemoteExecResponse$=');
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteLoadSetVarFromFileAction_With_UseServerDebugging;
var
  LoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions;
  LoadSetVarFromFileOptionsAPI: TClkLoadSetVarFromFileOptionsAPI;
  ActionName: WideString;
  Response: string;
begin
  GetDefaultPropertyValues_LoadSetVarFromFile(LoadSetVarFromFileOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My LoadSetVarFromFile action';

  SetLoadSetVarFromFileOptionsToAPI(LoadSetVarFromFileOptions, LoadSetVarFromFileOptionsAPI);
  ExecuteLoadSetVarFromFileAction(@ActionName[1], 100, @LoadSetVarFromFileOptionsAPI, True, @Response[1]);
  Expect(Response).ToContain('$RemoteExecResponse$=');
end;



procedure TTestClickerClientHTTPAPI.Test_ExecuteSaveSetVarToFileAction_With_UseServerDebugging;
var
  SaveSetVarToFileOptions: TClkSaveSetVarToFileOptions;
  SaveSetVarToFileOptionsAPI: TClkSaveSetVarToFileOptionsAPI;
  ActionName: WideString;
  Response: string;
begin
  GetDefaultPropertyValues_SaveSetVarToFile(SaveSetVarToFileOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My SaveSetVarToFile action';

  SetSaveSetVarToFileOptionsToAPI(SaveSetVarToFileOptions, SaveSetVarToFileOptionsAPI);
  ExecuteSaveSetVarToFileAction(@ActionName[1], 100, @SaveSetVarToFileOptionsAPI, True, @Response[1]);
  Expect(Response).ToContain('$RemoteExecResponse$=');
end;


procedure TTestClickerClientHTTPAPI.Test_ExecutePluginAction_With_UseServerDebugging;
var
  PluginOptions: TClkPluginOptions;
  PluginOptionsAPI: TClkPluginOptionsAPI;
  ActionName: WideString;
  Response: string;
begin
  GetDefaultPropertyValues_Plugin(PluginOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My Plugin action';

  SetPluginOptionsToAPI(PluginOptions, PluginOptionsAPI);
  ExecutePluginAction(@ActionName[1], 100, @PluginOptionsAPI, True, True, @Response[1]);
  Expect(Response).ToContain('$RemoteExecResponse$=');
end;


procedure TTestClickerClientHTTPAPI.Test_ExecuteEditTemplateAction_With_UseServerDebugging;
var
  EditTemplateOptions: TClkEditTemplateOptions;
  EditTemplateOptionsAPI: TClkEditTemplateOptionsAPI;
  ActionName: WideString;
  Response: string;
begin
  GetDefaultPropertyValues_EditTemplate(EditTemplateOptions);
  SetLength(Response, CMaxSharedStringLength);

  ActionName := 'My EditTemplate action';

  SetEditTemplateOptionsToAPI(EditTemplateOptions, EditTemplateOptionsAPI);
  ExecuteEditTemplateAction(@ActionName[1], 100, @EditTemplateOptionsAPI, True, @Response[1]);
  Expect(Response).ToContain('$RemoteExecResponse$=');
end;


initialization

  RegisterTest(TTestClickerClientHTTPAPI);

end.

