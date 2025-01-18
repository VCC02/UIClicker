{
    Copyright (C) 2024 VCC
    creation date: 13 Jan 2024
    initial release date: 15 Jan 2024

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


unit ClickerActionPlugins;

{$H+}
{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, ClickerUtils;


type
  //Callbacks of callbacks:
  TOnFileContent = procedure(ACallReference, AStreamContent: Pointer; AStreamSize: Int64); cdecl;
  TOnFileContentObj = procedure(AStreamContent: Pointer; AStreamSize: Int64) of object; cdecl;

  //Plugin property callbacks:
  TOnActionPlugin_UpdatePropertyIcons = procedure(APluginReference: Pointer; AStreamContent: Pointer; AStreamSize: Int64); cdecl; //A plugin calls this procedure to update the TImageList component in UIClicker, used on displaying plugin properties. This callback will be called for every property. UIClicker has to clear and add fixed icons to the image list, prior to calling TGetListOfProperties. On every call of this callback, UIClicker has to add a new bitmap to the image list.

  //Plugin execution callbacks:
  TOnActionPlugin_GetActionCount = function(APluginReference: Pointer): Integer; cdecl;  //a plugin calls this function, to get the number of actions in the current template from UIClicker
  TOnActionPlugin_GetActionInfoByIndex = procedure(APluginReference: Pointer; AIndex: Integer; AActionName: Pointer; ANameLength, AActionType: PDWord); cdecl; //A plugin calls this function, to get the action name and type of an action.
  TOnActionPlugin_GetActionContentByIndex = procedure(APluginReference: Pointer; AIndex: Integer; AActionContent: Pointer; AContentLength: PDWord); cdecl; //A plugin calls this function, to get the action structure of an action. The content has the same format as the one used to serialize the action using the http API.
  TOnActionPlugin_ExecuteAction = function(APluginReference: Pointer; AActionName: Pointer): Boolean; cdecl;
  TOnActionPlugin_GetAllTemplateVars = procedure(APluginReference: Pointer; AAllTemplateVars: Pointer; AVarsLength: PDWord); cdecl;  //AAllTemplateVars are encoded as CRLF separated key=value strings, ready to be used on a TStringlist
  TOnActionPlugin_SetTemplateVar = procedure(APluginReference: Pointer; AVarName, AVarValue: Pointer); cdecl;
  TOnActionPlugin_DebugPoint = function(APluginReference: Pointer; APointName, ALogMsg: Pointer): Boolean; cdecl; //The handler should return True, to continue execution. If False, the dll should exit ExecutePluginFunc (when users stop the execution).
  TOnActionPlugin_AddToLog = procedure(APluginReference: Pointer; ALogMsg: Pointer); cdecl;
  TOnActionPlugin_SetResultImg = procedure(APluginReference: Pointer; AResultIdx0, AResultIdx1, AResultIdx2: Integer; AStreamContent: Pointer; AStreamSize: Int64; AImgWidth, AImgHeight: Integer); cdecl; //Used when the plugin implements FindSubControl functionality. Then, AResultIdx0, AResultIdx1 and AResultIdx2 are result dimensions (e.g. AResultIdx0 can identify FindSubControl type (BmpTxt, Bmp file, Pmtv file), AResultIdx1 can identiy TextProfile index, or Bmp file index or Pmtv file index, while AResultIdx2 can identify Pmtv composition order).
  TOnActionPlugin_LoadBitmap = function(APluginReference: Pointer; AFileName, ACallReference: Pointer; AFileLocation: Byte; AOnFileContent: TOnFileContent): Boolean; cdecl;  //A plugin calls this function, to get the bitmap content (from disk or rendered-externally-in-mem FS). The function returns False if the file is not found. UIClicker calls AOnFileContent callback to set the file content in plugin if the file is found and it is allowed to be accessed. The ACallReference argument is passed to OnFileContent callback, for keeping track of that call. It can be a pointer to an object.
  TOnActionPlugin_GetAllowedFilesInfo = procedure(APluginReference: Pointer; AFullTemplatesDir, AAllowedFileDirsForServer, AAllowedFileExtensionsForServer: Pointer); cdecl; //Called by plugin when the file to be loaded will be sent to a remote location, so file location/extension "permissions" should be verified. For local files, this is not needed, as this is the behavior of the executable itself.
  TOnActionPlugin_SetBitmap = procedure(APluginReference: Pointer; AFileName: Pointer; AStreamContent: Pointer; AStreamSize: Int64; AImgWidth, AImgHeight: Integer); cdecl; //A plugin may call this function multiple times if it has multiple bitmaps to give back to UIClicker, which stores the bitmap in rendered-externally-in-mem FS.
  TOnActionPlugin_Screenshot = function(APluginReference: Pointer; AActionName: Pointer): Boolean; cdecl;  //A plugin may call this function, to take a screenshot, using the settings from a FindSubControl action (specified by AActionName). Returns False if the cropping settings are wrong (e.g. may result in negative sizes). It may require a FindControl action to be executed before the plugin action, to properly set the search area. If the cropping limits are absolute (i.e. they do not depend on the $Control..$ variables), then the $Control_Handle$ will be set to what is found at global screen coordinates. The screenshot is saved to ExtRendering InMem file system, using the CScreenshotFilename name.
  TOnActionPlugin_CheckStopAllActionsOnDemand = function(APluginReference: Pointer): Boolean; cdecl; //A plugin should call this function in its "main loop", from the ExecutePlugin function, and it should stop the loop if the function returns True.
  TOnActionPlugin_InMemFS = function(APluginReference: Pointer; ACallbackIndex: Integer; AInData1, AInData2: Pointer; AInDataLen1, AInDataLen2: Int64; AOnFileContent: TOnFileContentObj): Int64; cdecl;  //A plugin calls this callback to access the in-mem FS it is part of (where the dll is kept in memory). It is a multi-purpose function, which calls one of the In-Mem FS functions, based on the ACallbackIndex parameter. The API is designed in this way, to avoid adding to many callbacks and to make it forward compatible with future implementations of this function.

  TOnActionPlugin_SetTemplateVar_Obj = procedure(AVarName, AVarValue: Pointer) of object; cdecl;
  TOnActionPlugin_AddToLog_Obj = procedure(ALogMsg: Pointer) of object; cdecl;
  TOnActionPlugin_InMemFS_Obj = function(ACallbackIndex: Integer; AInData1, AInData2: Pointer; AInDataLen1, AInDataLen2: Int64; AOnFileContent: TOnFileContentObj): Int64 of object; cdecl;

  //Plugin procedures / functions:
  TGetAPIVersion = function: DWord; cdecl;
  TGetListOfProperties = procedure(APluginReference: Pointer;                 //UIClicker passes the plugin reference to the plugin, then the plugin calls some callbacks with that reference
                                   AListOfProperties: Pointer;
                                   AListOfPropertiesLen: PDWord;

                                   AOnActionPlugin_UpdatePropertyIcons: TOnActionPlugin_UpdatePropertyIcons); cdecl;
  //TGetListOfDebugPoints = procedure(AListOfDebugPoints: Pointer; AListOfDebugPointsLen: PDWord); cdecl;

  TExecutePlugin = function(APluginReference: Pointer;                 //UIClicker passes the plugin reference to the plugin, then the plugin calls some callbacks with that reference
                            AListOfPluginSettings: Pointer;            //similar to a TStringList.Text content
                            AListOfPluginSettingsLen: PDWord;

                            AOnActionPlugin_GetActionCount: TOnActionPlugin_GetActionCount;
                            AOnActionPlugin_GetActionInfoByIndex: TOnActionPlugin_GetActionInfoByIndex;
                            AOnActionPlugin_GetActionContentByIndex: TOnActionPlugin_GetActionContentByIndex;
                            AOnActionPlugin_ExecuteAction: TOnActionPlugin_ExecuteAction;
                            AOnActionPlugin_GetAllTemplateVars: TOnActionPlugin_GetAllTemplateVars;
                            AOnActionPlugin_SetTemplateVar: TOnActionPlugin_SetTemplateVar;
                            AOnActionPlugin_DebugPoint: TOnActionPlugin_DebugPoint;
                            AOnActionPlugin_AddToLog: TOnActionPlugin_AddToLog;
                            AOnActionPlugin_SetResultImg: TOnActionPlugin_SetResultImg;
                            AOnActionPlugin_LoadBitmap: TOnActionPlugin_LoadBitmap;
                            AOnActionPlugin_GetAllowedFilesInfo: TOnActionPlugin_GetAllowedFilesInfo;
                            AOnActionPlugin_SetBitmap: TOnActionPlugin_SetBitmap;
                            AOnActionPlugin_Screenshot: TOnActionPlugin_Screenshot;
                            AOnActionPlugin_CheckStopAllActionsOnDemand: TOnActionPlugin_CheckStopAllActionsOnDemand;
                            AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS
                            ): Boolean; cdecl;


  TActionPluginFunc = record
    GetAPIVersionFunc: TGetAPIVersion;
    GetListOfPropertiesProc: TGetListOfProperties;
    //GetListOfDebugPoints: TGetListOfDebugPoints;
    ExecutePluginFunc: TExecutePlugin;
  end;


const
  CActionPlugin_APIVersion = 8;
  CActionPlugin_ExecutionResultErrorVar = '$PluginError$';
  CActionPlugin_DebuggingVar = '$PluginDebugging$';
  CBeforePluginExecution_DbgLineContent = 'Before plugin execution.';
  CScreenshotFilename = 'Screenshot.bmp';

  CPluginPropertyAttr_DataType = 'DataType';
  CPluginPropertyAttr_EnumCounts = 'EnumCounts';
  CPluginPropertyAttr_EnumStrings = 'EnumStrings';
  CPluginPropertyAttr_Hint = 'Hint';
  CPluginPropertyAttr_Enabled = 'Enabled';
  CPluginPropertyAttr_DefaultValue = 'DefaultValue';

  CPluginInMemFSFunc_LoadFileFromMem = 0;   //Calls   procedure LoadFileFromMem(AName: string; AContent: Pointer; AvailableIndex: Integer = -1);
  CPluginInMemFSFunc_SaveFileToMem = 1;     //Calls   procedure SaveFileToMem(AName: string; AContent: Pointer; ASize: Int64);
  CPluginInMemFSFunc_GetFileSize = 2;       //Calls   function GetFileSize(AName: string): Int64;
  CPluginInMemFSFunc_ListMemFiles = 3;      //Calls   function ListMemFilesAsString: string
  CPluginInMemFSFunc_FileExistsInMem = 4;   //Calls   function FileExistsInMem(AFileName: string): Boolean;
  CPluginInMemFSFunc_DeleteFileFromMem = 5; //Calls   procedure DeleteFileFromMem(AFileName: string);
  CPluginInMemFSFunc_DuplicateFile = 6;     //Calls   function DuplicateFile(AFileName: string): string;
  CPluginInMemFSFunc_RenameFile = 7;        //Calls   procedure RenameFile(AFileName, NewFileName: string);


var
  DefaultOnActionPlugin_DebugPoint: TOnActionPlugin_DebugPoint;
  DefaultPluginReference: Pointer;

function DbgPoint(APointName, ALogMsg: string): Boolean;

implementation


function DbgPoint(APointName, ALogMsg: string): Boolean;
begin
  if @DefaultOnActionPlugin_DebugPoint <> nil then
    Result := DefaultOnActionPlugin_DebugPoint(DefaultPluginReference, @APointName[1], @ALogMsg[1])
  else
    Result := True;
end;


begin
  DefaultOnActionPlugin_DebugPoint := nil;
  DefaultPluginReference := nil;
end.

