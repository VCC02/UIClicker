{
    Copyright (C) 2024 VCC
    creation date: 18 Dec 2024
    initial release date: 18 Dec 2024

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


unit ClickerClientIntf;

{$mode Delphi}{$H+}

interface

uses
  LCLIntf, Classes, SysUtils, ClickerClientAPI;


type
  TInitClickerClient_Proc = procedure; cdecl;
  TDoneClickerClient_Proc = procedure; cdecl;
  TSetServerAddress_Proc = procedure(AAddress: Pointer); cdecl;
  TGetServerAddress_Proc = function(AResponse: Pointer): LongInt; cdecl;
  TTestConnectionToServer_Proc = function(AResponse: Pointer): LongInt; cdecl;
  TSendMemPluginFileToServer_Proc = function(AFileName, AFileContent: Pointer; AFileSize: Int64; AResultStr: Pointer): Integer; cdecl;
  TSendMemPluginArchiveFileToServer_Proc = function(AFileName, ADecryptionPluginName, ADecompressionPluginName, AHashingPluginName, AFileContent: Pointer; AFileSize: Int64; ACompressionLevel: Integer; AAdditionalInfo: Pointer; AIsDecDecHash: Boolean; AResultStr: Pointer): Integer; cdecl;

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
  SendMemPluginFileToServer: TSendMemPluginFileToServer_Proc;
  SendMemPluginArchiveFileToServer: TSendMemPluginArchiveFileToServer_Proc;

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


function LoadClickerClient(ADllPath: string): Boolean; //returns True if successful
procedure UnLoadClickerClient;


implementation


var
  FClickerClientDllHandle: THandle;


function LoadClickerClient(ADllPath: string): Boolean; //returns True if successful
begin
  FClickerClientDllHandle := LoadLibrary(ADllPath);
  Result := FClickerClientDllHandle > 0;

  @InitClickerClient := GetProcAddress(FClickerClientDllHandle, 'InitClickerClient');
  @DoneClickerClient := GetProcAddress(FClickerClientDllHandle, 'DoneClickerClient');
  @SetServerAddress := GetProcAddress(FClickerClientDllHandle, 'SetServerAddress');
  @GetServerAddress := GetProcAddress(FClickerClientDllHandle, 'GetServerAddress');
  @TestConnectionToServer := GetProcAddress(FClickerClientDllHandle, 'TestConnectionToServer');
  @SendMemPluginFileToServer := GetProcAddress(FClickerClientDllHandle, 'SendMemPluginFileToServer');
  @SendMemPluginArchiveFileToServer := GetProcAddress(FClickerClientDllHandle, 'SendMemPluginArchiveFileToServer');

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
end;


procedure UnLoadClickerClient;
begin
  if FClickerClientDllHandle > 0 then
  begin
    FreeLibrary(FClickerClientDllHandle);
    FClickerClientDllHandle := 0;
  end;
end;

initialization
  FClickerClientDllHandle := 0;

end.

