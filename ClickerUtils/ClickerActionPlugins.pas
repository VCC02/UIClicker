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

  //Plugin callbacks:
  TOnActionPlugin_GetActionCount = function(APluginReference: Pointer): Integer; cdecl;  //a plugin calls this function, to get the number of actions in the current template from UIClicker
  TOnActionPlugin_GetActionInfoByIndex = procedure(APluginReference: Pointer; AIndex: Integer; AActionName: Pointer; ANameLength, AActionType: PDWord); cdecl; //a plugin calls this function, to get the action structure of an action
  TOnActionPlugin_ExecuteAction = function(APluginReference: Pointer; AActionName: Pointer): Boolean; cdecl;
  TOnActionPlugin_GetAllTemplateVars = procedure(APluginReference: Pointer; AAllTemplateVars: Pointer; AVarsLength: PDWord); cdecl;  //AAllTemplateVars are encoded as CRLF separated key=value strings, ready to be used on a TStringlist
  TOnActionPlugin_SetTemplateVar = procedure(APluginReference: Pointer; AVarName, AVarValue: Pointer); cdecl;
  TOnActionPlugin_DebugPoint = function(APluginReference: Pointer; APointName, ALogMsg: Pointer): Boolean; cdecl; //The handler should return True, to continue execution. If False, the dll should exit ExecutePluginFunc (when users stop the execution).
  TOnActionPlugin_AddToLog = procedure(APluginReference: Pointer; ALogMsg: Pointer); cdecl;
  TOnActionPlugin_SetResultImg = procedure(APluginReference: Pointer; AResultIdx0, AResultIdx1, AResultIdx2: Integer; AStreamContent: Pointer; AStreamSize: Int64; AImgWidth, AImgHeight: Integer); cdecl; //Used when the plugin implements FindSubControl functionality. Then, AResultIdx0, AResultIdx1 and AResultIdx2 are result dimensions (e.g. AResultIdx0 can identify FindSubControl type (BmpTxt, Bmp file, Pmtv file), AResultIdx1 can identiy TextProfile index, or Bmp file index or Pmtv file index, while AResultIdx2 can identify Pmtv composition order).
  TOnActionPlugin_LoadBitmap = function(APluginReference: Pointer; AFileName, ACallReference: Pointer; AFileLocation: Byte; AOnFileContent: TOnFileContent): Boolean; cdecl;  //a plugin calls this function, to get the bitmap content (from disk or rendered-externally-in-mem FS). The function returns False if the file is not found. UIClicker calls AOnFileContent callback to set the file content in plugin if the file is found and it is allowed to be accessed. The ACallReference argument is passed to OnFileContent callback, for keeping track of that call. It can be a pointer to an object.
  TOnActionPlugin_GetAllowedFilesInfo = procedure(APluginReference: Pointer; AFullTemplatesDir, AAllowedFileDirsForServer, AAllowedFileExtensionsForServer: Pointer); cdecl; //Called by plugin when the file to be loaded will be sent to a remote location, so file location/extension "permissions" should be verified. For local files, this is not needed, as this is the behavior of the executable itself.


  //Plugin procedures / functions:
  TGetAPIVersion = function: DWord; cdecl;
  TGetListOfProperties = procedure(AListOfProperties: Pointer; AListOfPropertiesLen: PDWord); cdecl;
  TGetListOfDebugPoints = procedure(AListOfDebugPoints: Pointer; AListOfDebugPointsLen: PDWord); cdecl;

  TExecutePlugin = function(APluginReference: Pointer;                 //UIClicker passes the plugin reference to the plugin, then the plugin calls some callbacks with that reference
                            AListOfPluginSettings: Pointer;            //similar to a TStringList.Text content
                            AListOfPluginSettingsLen: PDWord;

                            AOnActionPlugin_GetActionCount: TOnActionPlugin_GetActionCount;
                            AOnActionPlugin_GetActionInfoByIndex: TOnActionPlugin_GetActionInfoByIndex;
                            AOnActionPlugin_ExecuteAction: TOnActionPlugin_ExecuteAction;
                            AOnActionPlugin_GetAllTemplateVars: TOnActionPlugin_GetAllTemplateVars;
                            AOnActionPlugin_SetTemplateVar: TOnActionPlugin_SetTemplateVar;
                            AOnActionPlugin_DebugPoint: TOnActionPlugin_DebugPoint;
                            AOnActionPlugin_AddToLog: TOnActionPlugin_AddToLog;
                            AOnActionPlugin_SetResultImg: TOnActionPlugin_SetResultImg;
                            AOnActionPlugin_LoadBitmap: TOnActionPlugin_LoadBitmap;
                            AOnActionPlugin_GetAllowedFilesInfo: TOnActionPlugin_GetAllowedFilesInfo): Boolean; cdecl;


  TActionPluginFunc = record
    GetAPIVersionFunc: TGetAPIVersion;
    GetListOfPropertiesProc: TGetListOfProperties;
    GetListOfDebugPoints: TGetListOfDebugPoints;
    ExecutePluginFunc: TExecutePlugin;
  end;


const
  CActionPlugin_APIVersion = 4;
  CActionPlugin_ExecutionResultErrorVar = '$PluginError$';
  CBeforePluginExecution_DbgLineContent = 'Before plugin execution.';


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

