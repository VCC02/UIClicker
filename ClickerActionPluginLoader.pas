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


unit ClickerActionPluginLoader;

{$H+}
{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, Classes, SysUtils, Graphics, dynlibs,
  ClickerUtils, ClickerActionPlugins, InMemFileSystem;


type
  TActionPlugin = record
  private
    OnLoadPluginFromInMemFS: TOnLoadPluginFromInMemFS;
    OnAddToLog: TOnAddToLog;
    OnExecuteActionByName: TOnExecuteActionByName;
    OnSetVar: TOnSetVar;
    OnSetDebugPoint: TOnSetDebugPoint;
    OnIsAtBreakPoint: TOnIsAtBreakPoint;
    OnLoadBitmap: TOnLoadBitmap;
    OnLoadRenderedBitmap: TOnLoadRenderedBitmap;
    OnSaveFileToExtRenderingInMemFS: TOnSaveFileToExtRenderingInMemFS;
    OnScreenshotByActionName: TOnScreenshotByActionName;
    OnUpdatePropertyIcons: TOnUpdatePropertyIcons;
    //OnUpdatePropertyDetails: TOnUpdatePropertyDetails;

    FIsDebugging, FShouldStopAtBreakPoint: Boolean;
    FStopAllActionsOnDemandFromParent, FPluginContinueAll: PBoolean;
    FStepOver: PBoolean;
    //FIsFirstExecution: Boolean; //Flag, used to stop the execution, to allow the user to set breakpoints
    FContinueAfterBreakPoint: Boolean;

    FFullTemplatesDir: string;
    FAllowedFileDirsForServer: string;
    FAllowedFileExtensionsForServer: string;
    FInMemFS: TInMemFileSystem;

    ResultBmp: TBitmap;
    AllActions: PClkActionsRecArr;
    AllVars: TStringList;

    Func: TActionPluginFunc;
    function DoOnLoadPluginFromInMemFS(APlugin: TMemoryStream; AFileName: string): Boolean;
    procedure DoAddToLog(s: string);
    function DoExecuteActionByName(AActionName: string): Boolean;
    procedure DoSetVar(AVarName, AVarValue: string);
    procedure DoSetDebugPoint(ADebugPoint: string);
    function DoIsAtBreakPoint(ADebugPoint: string): Boolean;
  public
    Path: string;
    Loaded: Boolean;
    Err: string;
    PluginHandle: UInt64;

    function LoadToExecute(APath: string;
                           AOnLoadPluginFromInMemFS: TOnLoadPluginFromInMemFS;
                           AOnAddToLog: TOnAddToLog;
                           AOnExecuteActionByName: TOnExecuteActionByName;
                           AOnSetVar: TOnSetVar;
                           AOnSetDebugPoint: TOnSetDebugPoint;
                           AOnIsAtBreakPoint: TOnIsAtBreakPoint;
                           AOnLoadBitmap: TOnLoadBitmap;
                           AOnLoadRenderedBitmap: TOnLoadRenderedBitmap;
                           AOnSaveFileToExtRenderingInMemFS: TOnSaveFileToExtRenderingInMemFS;
                           AOnScreenshotByActionName: TOnScreenshotByActionName;
                           IsDebugging,
                           AShouldStopAtBreakPoint: Boolean;
                           AStopAllActionsOnDemandFromParent,
                           AStepOver,
                           APluginContinueAll: PBoolean;
                           AResultBmp: TBitmap;
                           AInMemFS: TInMemFileSystem;
                           AFullTemplatesDir,
                           AAllowedFileDirsForServer,
                           AAllowedFileExtensionsForServer: string;
                           AAllActions: PClkActionsRecArr = nil;
                           AAllVars: TStringList = nil): Boolean;

    function LoadToGetProperties(APath: string;
                                 AOnLoadPluginFromInMemFS: TOnLoadPluginFromInMemFS;
                                 AOnUpdatePropertyIcons: TOnUpdatePropertyIcons;
                                 AOnAddToLog: TOnAddToLog): Boolean;
    function Unload(AOnAddToLog: TOnAddToLog): Boolean;

    function GetAPIVersion: DWord;
    function GetListOfProperties: string;
    function ExecutePlugin(AListOfPropertiesAndValues: string): Boolean;
  end;

  PActionPlugin = ^TActionPlugin;


  TPluginInMemFSAccess = class
  private
    FInMemFS: TInMemFileSystem;
  public
    constructor Create;
    function InMemFSFunc(ACallbackIndex: Integer; AInData1, AInData2: Pointer; AInDataLen1, AInDataLen2: Int64; AOnFileContent: TOnFileContentObj): Int64;

    property InMemFS: TInMemFileSystem write FInMemFS;
  end;


function DecodePluginPropertyFromAttribute(AValue, AAttrName: string): string;
function GetPluginAdditionalPropertyAttribute(AListOfPropertiesAndTypes, AAttrName: string; APropertyIndexNoOffset: Integer): string;


{$IFDEF MemPlugins}
  function IsMemPluginPath(APath: string): Boolean;
{$ENDIF}


implementation


uses
  DllUtils, Forms, ClickerActionsClient, ClickerActionProperties
  {$IFDEF MemPlugins}
    , DynMemLib    //search for DynMemLib.pas on GitHub if not found
  {$ENDIF}
  ;


const
  CMustSetPluginFileNameMsg: string = ' Please configure the FileName property to a valid plugin path.';

var
  LoadedPluginsCritSec: TRTLCriticalSection;
  LoadedPluginHandles: TStringList;


{$IFDEF MemPlugins}
  function IsMemPluginPath(APath: string): Boolean;
  begin
    Result := Pos(CMemPluginLocationPrefix, APath) = 1;
  end;
{$ENDIF}


function DecodePluginPropertyFromAttribute(AValue, AAttrName: string): string;
begin
  Result := Copy(AValue, Pos(#8#7 + AAttrName + '=', AValue) + 2, MaxInt);  //at this point, Result contains multiple attributes
  Result := Copy(Result, 1, Pos(#8#7, Result) - 1);  //at this point, Result is AAttrName + '=' + [some datatype]
  Delete(Result, 1, Length(AAttrName + '='));
end;


function GetPluginAdditionalPropertyAttribute(AListOfPropertiesAndTypes, AAttrName: string; APropertyIndexNoOffset: Integer): string;
var
  ListOfProperties: TStringList;
begin
  Result := '';

  ListOfProperties := TStringList.Create;
  try
    ListOfProperties.Text := AListOfPropertiesAndTypes;

    if (APropertyIndexNoOffset > -1) and (APropertyIndexNoOffset < ListOfProperties.Count) and (ListOfProperties.Count > 0) then
    begin
      Result := DecodePluginPropertyFromAttribute(ListOfProperties.ValueFromIndex[APropertyIndexNoOffset], AAttrName);

      //DoOnAddToLog('Plugin prop DataType[' + IntToStr(APropertyIndexNoOffset) + ']: ' + Result);
    end
  finally
    ListOfProperties.Free;
  end;
end;


//APluginReference amd AIndex is used as input param.
function DoOnActionPlugin_GetActionCount_Callback(APluginReference: Pointer): Integer; cdecl;
var
  ActionPlugin: PActionPlugin;
begin
  try
    ActionPlugin := APluginReference;
    Result := Length(ActionPlugin^.AllActions^);
  except
    on E: Exception do
      ActionPlugin^.DoAddToLog('Plugin loader: ' + E.Message);
  end;
end;


//APluginReference and AIndex are used as input params to this callback.
//AActionName, ANameLength and AActionType are used as output params from this callback  - the pointers are not modified though, only the data they point to. The plugin is expected to have a valid allocated variable for each of them.
procedure DoOnActionPlugin_GetActionInfoByIndex_Callback(APluginReference: Pointer; AIndex: Integer; AActionName: Pointer; ANameLength, AActionType: PDWord); cdecl;
var
  ActionPlugin: PActionPlugin;
begin
  try
    ActionPlugin := APluginReference;

    if (AIndex < 0) or (AIndex > Length(ActionPlugin^.AllActions^) - 1) then
    begin
      AActionType^ := $FFFFFFFF;
      ANameLength^ := 0;
      Exit;
    end;

    AActionType^ := Ord(ActionPlugin^.AllActions^[AIndex].ActionOptions.Action);
    ANameLength^ := DWord(SetPointedContentFromString(ActionPlugin^.AllActions^[AIndex].ActionOptions.ActionName, AActionName));
  except
    on E: Exception do
      ActionPlugin^.DoAddToLog('Plugin loader: ' + E.Message);
  end;
end;


procedure DoOnActionPlugin_GetActionContentByIndex_Callback(APluginReference: Pointer; AIndex: Integer; AActionContent: Pointer; AContentLength: PDWord); cdecl; //A plugin calls this function, to get the action structure of an action. The content has the same format as the one used to serialize the action using the http API.
var
  ActionPlugin: PActionPlugin;
  ActionContentStr: string;
begin
  try
    ActionPlugin := APluginReference;

    if (AIndex < 0) or (AIndex > Length(ActionPlugin^.AllActions^) - 1) then
    begin
      AContentLength^ := 0;
      Exit;
    end;

    ActionContentStr := '';
    case ActionPlugin^.AllActions^[AIndex].ActionOptions.Action of
      acClick:
        ActionContentStr := GetClickActionProperties(ActionPlugin^.AllActions^[AIndex].ClickOptions);

      acExecApp:
        ActionContentStr := GetExecAppActionProperties(ActionPlugin^.AllActions^[AIndex].ExecAppOptions);

      acFindControl, acFindSubControl:
        ActionContentStr := GetFindControlActionProperties(ActionPlugin^.AllActions^[AIndex].FindControlOptions);

      acSetControlText:
        ActionContentStr := GetSetControlTextActionProperties(ActionPlugin^.AllActions^[AIndex].SetTextOptions);

      acCallTemplate:
        ActionContentStr := GetCallTemplateActionProperties(ActionPlugin^.AllActions^[AIndex].CallTemplateOptions);

      acSleep:
        ActionContentStr := GetSleepActionProperties(ActionPlugin^.AllActions^[AIndex].SleepOptions);

      acSetVar:
        ActionContentStr := GetSetVarActionProperties(ActionPlugin^.AllActions^[AIndex].SetVarOptions);

      acWindowOperations:
        ActionContentStr := GetWindowOperationsActionProperties(ActionPlugin^.AllActions^[AIndex].WindowOperationsOptions);

      acLoadSetVarFromFile:
        ActionContentStr := GetLoadSetVarFromFileActionProperties(ActionPlugin^.AllActions^[AIndex].LoadSetVarFromFileOptions);

      acSaveSetVarToFile:
        ActionContentStr := GetSaveSetVarToFileActionProperties(ActionPlugin^.AllActions^[AIndex].SaveSetVarToFileOptions);

      acPlugin:
        ActionContentStr := GetPluginActionProperties(ActionPlugin^.AllActions^[AIndex].PluginOptions);

      acEditTemplate:
        ActionContentStr := GetEditTemplateActionProperties(ActionPlugin^.AllActions^[AIndex].EditTemplateOptions);

      else
        ActionContentStr := 'Not implemented';
    end;

    AContentLength^ := SetPointedContentFromString(ActionContentStr, AActionContent);
  except
    on E: Exception do
      ActionPlugin^.DoAddToLog('Plugin loader: ' + E.Message);
  end;
end;


//APluginReference amd AActionName are used as input params.
function DoOnActionPlugin_ExecuteAction_Callback(APluginReference: Pointer; AActionName: Pointer): Boolean; cdecl;
var
  ActionPlugin: PActionPlugin;
  s: string;
begin
  Result := True;
  try
    ActionPlugin := APluginReference;
    SetPointedContentToString(AActionName, s);

    Result := ActionPlugin^.DoExecuteActionByName(s);
  except
    on E: Exception do
      ActionPlugin^.DoAddToLog('Plugin loader: ' + E.Message);
  end;
end;


procedure DoOnActionPlugin_GetAllTemplateVars_Callback(APluginReference: Pointer; AAllTemplateVars: Pointer; AVarsLength: PDWord); cdecl;
var
  AllVarsStr: string;
  ActionPlugin: PActionPlugin;
  Len: DWord;
begin
  try
    ActionPlugin := APluginReference;
    AllVarsStr := ActionPlugin^.AllVars.Text;

    Len := SetPointedContentFromString(AllVarsStr, AAllTemplateVars);
    AVarsLength^ := Len;
  except
    on E: Exception do
      ActionPlugin^.DoAddToLog('Plugin loader: ' + E.Message);
  end;
end;


procedure DoOnActionPlugin_SetTemplateVar_Callback(APluginReference: Pointer; AVarName, AVarValue: Pointer); cdecl;
var
  ActionPlugin: PActionPlugin;
  VarName, VarValue: string;
begin
  try
    ActionPlugin := APluginReference;
    SetPointedContentToString(AVarName, VarName);
    SetPointedContentToString(AVarValue, VarValue);

    ActionPlugin^.DoSetVar(VarName, VarValue);
  except
    on E: Exception do
      ActionPlugin^.DoAddToLog('Plugin loader: ' + E.Message);
  end;
end;


procedure WaitInDebuggingMode(AActionPlugin: PActionPlugin);
begin
  repeat
    Sleep(1);
    Application.ProcessMessages;

    if ((AActionPlugin^.FStepOver <> nil) and AActionPlugin^.FStepOver^) or
       (GetAsyncKeyState(VK_F8) < 0) then
    begin
      if AActionPlugin^.FStepOver <> nil then
        AActionPlugin^.FStepOver^ := False;    //Reset flag

      Sleep(200);
      Break;
    end;

    if ((AActionPlugin^.FPluginContinueAll <> nil) and AActionPlugin^.FPluginContinueAll^) or
       (GetAsyncKeyState(VK_F9) < 0) then
    begin
      //Do not reset the FPluginContinueAll flag here.
      Break;
    end;

    if ((AActionPlugin^.FStopAllActionsOnDemandFromParent <> nil) and AActionPlugin^.FStopAllActionsOnDemandFromParent^) or
       ((GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) and (GetAsyncKeyState(VK_F2) < 0)) then
      Exit;

    if GeneralClosingApp then  //there are other loops which will have to be stopped this way
      Exit;
  until False;
end;


function DoOnActionPlugin_DebugPoint_Callback(APluginReference: Pointer; APointName, ALogMsg: Pointer): Boolean; cdecl; //The handler should return True, to continue execution. If False, the dll should exit ExecutePluginFunc (when users stop the execution).
var
  ActionPlugin: PActionPlugin;
  PointName, LogMsg: string;
  IsAtBreakPoint: Boolean;
  ContinueAll: Boolean;
begin
  Result := False;

  try
    ActionPlugin := APluginReference;
    SetPointedContentToString(APointName, PointName);
    SetPointedContentToString(ALogMsg, LogMsg);

    ActionPlugin^.DoAddToLog('Entering plugin debug point "' + PointName + ':' + LogMsg + '".');
    try
      ActionPlugin^.DoSetDebugPoint(PointName);
      IsAtBreakPoint := ActionPlugin^.DoIsAtBreakPoint(PointName);
      ContinueAll := (ActionPlugin^.FPluginContinueAll <> nil) and ActionPlugin^.FPluginContinueAll^;

      if ContinueAll then
      begin
        ActionPlugin^.FContinueAfterBreakPoint := False;

        if IsAtBreakPoint then
        begin
          ContinueAll := False;
          if ActionPlugin^.FPluginContinueAll <> nil then
            ActionPlugin^.FPluginContinueAll^ := False;   //allow stopping at next breakpoint
        end;
      end;

      if ActionPlugin^.FIsDebugging and (IsAtBreakPoint or ActionPlugin^.FContinueAfterBreakPoint) and not ContinueAll then
      begin
        if IsAtBreakPoint then
          ActionPlugin^.FContinueAfterBreakPoint := True;  //allow entering here after first hitting a breakpoint

        WaitInDebuggingMode(ActionPlugin);
      end;
    finally
      ActionPlugin^.DoAddToLog('Exiting plugin debug point "' + PointName + '".');
    end;
  except
    on E: Exception do
      ActionPlugin^.DoAddToLog('Plugin loader: ' + E.Message);
  end;

  Result := True;
end;


procedure DoOnActionPlugin_AddToLog_Callback(APluginReference: Pointer; ALogMsg: Pointer); cdecl;
var
  ActionPlugin: PActionPlugin;
  LogMsg: string;
begin
  try
    ActionPlugin := APluginReference;
    SetPointedContentToString(ALogMsg, LogMsg);
    ActionPlugin^.DoAddToLog('[Plugin]: ' + LogMsg);
  except
    on E: Exception do
      ActionPlugin^.DoAddToLog('Plugin loader: ' + E.Message);
  end;
end;


procedure DoOnActionPlugin_SetResultImg(APluginReference: Pointer; AResultIdx0, AResultIdx1, AResultIdx2: Integer; AStreamContent: Pointer; AStreamSize: Int64; AImgWidth, AImgHeight: Integer); cdecl;
var
  ActionPlugin: PActionPlugin;
  TempStream: TMemoryStream;
begin
  try
    ActionPlugin := APluginReference;
    ActionPlugin^.DoAddToLog('Plugin called OnActionPlugin_SetResultImg');

    if ActionPlugin^.ResultBmp = nil then
    begin
      ActionPlugin^.DoAddToLog('Plugin called with ResultBmp set to nil.');
      Exit;
    end;

    TempStream := TMemoryStream.Create;
    try
      TempStream.SetSize(AStreamSize);
      Move(AStreamContent^, TempStream.Memory^, AStreamSize);

      TempStream.Position := 0;
      ActionPlugin^.ResultBmp.LoadFromStream(TempStream);

      ActionPlugin^.DoAddToLog('Plugin result image size: ' + IntToStr(ActionPlugin^.ResultBmp.Width) + ':' + IntToStr(ActionPlugin^.ResultBmp.Height) + '  (' + IntToStr(TempStream.Size) + ' bytes).');
    finally
      TempStream.Free;
    end;
  except
    on E: Exception do
      ActionPlugin^.DoAddToLog('Ex when plugin called OnActionPlugin_SetResultImg: ' + E.Message);
  end;
end;


//AFileLocation is expected to match TFileLocation from InMemFileSystem
function DoOnActionPlugin_GetFileContent(APluginReference: Pointer; AFileName, ACallReference: Pointer; AFileLocation: Byte; AOnFileContent: TOnFileContent): Boolean; cdecl;
var
  ActionPlugin: PActionPlugin;
  TempMemSteam: TMemoryStream;
  TempBmp: Graphics.TBitmap;
  Fnm: string;
  Allowed: Boolean;
begin
  ActionPlugin := APluginReference;
  Result := True;

  SetPointedContentToString(AFileName, Fnm);
  Allowed := True; /////////////////////////////////////////////////////////////  verifiy if file is allowed to be read (ext and location permissions)

  if not Allowed then
  begin
    Result := False;
    ActionPlugin.DoAddToLog('Error: a plugin attempts to load a file which is not allowed in ActionPlugin_GetFileContent: "' + Fnm + '".');
    Exit;
  end;

  TempMemSteam := TMemoryStream.Create;
  TempBmp := TBitmap.Create;
  try
    case AFileLocation of
      0: //flDisk
        if not ActionPlugin^.OnLoadBitmap(TempBmp, Fnm) then   //returns True if file loaded, and False if file not found
        begin
          Result := False;
          ActionPlugin.DoAddToLog('Error: file not found in ActionPlugin_GetFileContent (Disk): "' + Fnm + '".');
          Exit;
        end;

      1: //flMem
        if not ActionPlugin^.OnLoadRenderedBitmap(TempBmp, Fnm) then   //returns True if file loaded, and False if file not found
        begin
          Result := False;
          ActionPlugin.DoAddToLog('Error: file not found in ActionPlugin_GetFileContent (Mem): "' + Fnm + '".');
          Exit;
        end;

      else
      begin
        ActionPlugin.DoAddToLog('Error: unhandled bitmap location type: ' + IntToStr(AFileLocation) + '.');
        Result := False;
        Exit;
      end;
    end;  //case

    TempBmp.SaveToStream(TempMemSteam);
    AOnFileContent(ACallReference, TempMemSteam.Memory, TempMemSteam.Size);
  finally
    TempMemSteam.Free;
    TempBmp.Free;
  end;
end;


procedure DoOnActionPlugin_GetAllowedFilesInfo(APluginReference: Pointer; AFullTemplatesDir, AAllowedFileDirsForServer, AAllowedFileExtensionsForServer: Pointer); cdecl;
var
  ActionPlugin: PActionPlugin;
begin
  ActionPlugin := APluginReference;
  SetPointedContentFromString(ActionPlugin^.FFullTemplatesDir, AFullTemplatesDir);
  SetPointedContentFromString(ActionPlugin^.FAllowedFileDirsForServer, AAllowedFileDirsForServer);
  SetPointedContentFromString(ActionPlugin^.FAllowedFileExtensionsForServer, AAllowedFileExtensionsForServer);
end;


procedure DoOnActionPlugin_SetBitmap(APluginReference: Pointer; AFileName: Pointer; AStreamContent: Pointer; AStreamSize: Int64; AImgWidth, AImgHeight: Integer); cdecl; //A plugin may call this function multiple times if it has multiple bitmaps to give back to UIClicker, which stores the bitmap in rendered-externally-in-mem FS.
var
  ActionPlugin: PActionPlugin;
  TempFileName: string;
begin
  ActionPlugin := APluginReference;
  SetPointedContentToString(AFileName, TempFileName);

  if not Assigned(ActionPlugin.OnSaveFileToExtRenderingInMemFS) then
    raise Exception.Create('OnSaveFileToExtRenderingInMemFS not assigned.');

  ActionPlugin.OnSaveFileToExtRenderingInMemFS(TempFileName, AStreamContent, AStreamSize);
end;


function DoOnActionPlugin_Screenshot(APluginReference: Pointer; AActionName: Pointer): Boolean; cdecl;
var
  ActionPlugin: PActionPlugin;
  TempActionName: string;
begin
  ActionPlugin := APluginReference;
  SetPointedContentToString(AActionName, TempActionName);

  if not Assigned(ActionPlugin.OnScreenshotByActionName) then
    raise Exception.Create('OnScreenshotByActionName not assigned.');

  Result := ActionPlugin.OnScreenshotByActionName(TempActionName);
end;


function DoOnActionPlugin_CheckStopAllActionsOnDemand(APluginReference: Pointer): Boolean; cdecl;
var
  ActionPlugin: PActionPlugin;
begin
  ActionPlugin := APluginReference;

  if not Assigned(ActionPlugin.OnScreenshotByActionName) then
    raise Exception.Create('OnScreenshotByActionName not assigned.');

  Result := ActionPlugin.FStopAllActionsOnDemandFromParent^;
end;


function DoOnActionPlugin_InMemFS(APluginReference: Pointer; ACallbackIndex: Integer; AInData1, AInData2: Pointer; AInDataLen1, AInDataLen2: Int64; AOnFileContent: TOnFileContentObj): Int64; cdecl;
var
  ActionPlugin: PActionPlugin;
  InMemFSAccess: TPluginInMemFSAccess;
begin
  ActionPlugin := APluginReference;

  if ActionPlugin.FInMemFS = nil then
  begin
    Result := -1;
    ActionPlugin.DoAddToLog('Plugin InMemFS not available. Please enable the feature and recompile UIClicker.');
    Exit;
  end;

  InMemFSAccess := TPluginInMemFSAccess.Create;
  try
    InMemFSAccess.InMemFS := ActionPlugin.FInMemFS;
    Result := InMemFSAccess.InMemFSFunc(ACallbackIndex, AInData1, AInData2, AInDataLen1, AInDataLen2, AOnFileContent);
  finally
    InMemFSAccess.Free;
  end;
end;


function TActionPlugin.DoOnLoadPluginFromInMemFS(APlugin: TMemoryStream; AFileName: string): Boolean;
begin
  if Assigned(OnLoadPluginFromInMemFS) then
    Result := OnLoadPluginFromInMemFS(APlugin, AFileName)
  else
  begin
    Result := False;
    DoAddToLog('OnLoadPluginFromInMemFS is not assigned.');
  end;
end;


procedure TActionPlugin.DoAddToLog(s: string);
begin
  try
    if Assigned(OnAddToLog) then
      OnAddToLog(s);
  except
    on E: Exception do
      MessageBoxFunction(PChar('Ex: ' + s), 'Plugin loader', 0);
  end;
end;


function TActionPlugin.DoExecuteActionByName(AActionName: string): Boolean;
begin
  if Assigned(OnExecuteActionByName) then
    Result := OnExecuteActionByName(AActionName)
  else
  begin
    Result := False;
    DoAddToLog('OnExecuteActionByName is not assigned.');
  end;
end;


procedure TActionPlugin.DoSetVar(AVarName, AVarValue: string);
begin
  if Assigned(OnSetVar) then
    OnSetVar(AVarName, AVarValue);
end;


procedure TActionPlugin.DoSetDebugPoint(ADebugPoint: string);
begin
  if Assigned(OnSetDebugPoint) then
    OnSetDebugPoint(ADebugPoint);
end;


function TActionPlugin.DoIsAtBreakPoint(ADebugPoint: string): Boolean;
begin
  if Assigned(OnIsAtBreakPoint) then
    Result := OnIsAtBreakPoint(ADebugPoint)
  else
    Result := True; //break if not assigned
end;


procedure AddPluginHandleToList(APluginHandle: UInt64);
var
  PluginHandleStr: string;
begin
  EnterCriticalSection(LoadedPluginsCritSec);
  try
    PluginHandleStr := IntToStr(APluginHandle);
    if LoadedPluginHandles.IndexOf(PluginHandleStr) = -1 then
      LoadedPluginHandles.Add(PluginHandleStr)
    else
      raise Exception.Create('Another plugin instance already exists for the same plugin.');
  finally
    LeaveCriticalSection(LoadedPluginsCritSec);
  end;
end;


procedure RemovePluginHandleFromList(APluginHandle: UInt64);
var
  PluginHandleStr: string;
  Idx: Integer;
begin
  EnterCriticalSection(LoadedPluginsCritSec);
  try
    PluginHandleStr := IntToStr(APluginHandle);
    Idx := LoadedPluginHandles.IndexOf(PluginHandleStr);

    if Idx = -1 then
      raise Exception.Create('Attempting to remove a plugin instance which does not exist: ' + PluginHandleStr)
    else
      LoadedPluginHandles.Delete(Idx);
  finally
    LeaveCriticalSection(LoadedPluginsCritSec);
  end;
end;


function TActionPlugin.LoadToExecute(APath: string;
                                     AOnLoadPluginFromInMemFS: TOnLoadPluginFromInMemFS;
                                     AOnAddToLog: TOnAddToLog;
                                     AOnExecuteActionByName: TOnExecuteActionByName;
                                     AOnSetVar: TOnSetVar;
                                     AOnSetDebugPoint: TOnSetDebugPoint;
                                     AOnIsAtBreakPoint: TOnIsAtBreakPoint;
                                     AOnLoadBitmap: TOnLoadBitmap;
                                     AOnLoadRenderedBitmap: TOnLoadRenderedBitmap;
                                     AOnSaveFileToExtRenderingInMemFS: TOnSaveFileToExtRenderingInMemFS;
                                     AOnScreenshotByActionName: TOnScreenshotByActionName;
                                     IsDebugging,
                                     AShouldStopAtBreakPoint: Boolean;
                                     AStopAllActionsOnDemandFromParent,
                                     AStepOver,
                                     APluginContinueAll: PBoolean;
                                     AResultBmp: TBitmap;
                                     AInMemFS: TInMemFileSystem;
                                     AFullTemplatesDir,
                                     AAllowedFileDirsForServer,
                                     AAllowedFileExtensionsForServer: string;
                                     AAllActions: PClkActionsRecArr = nil;
                                     AAllVars: TStringList = nil): Boolean;
{$IFDEF MemPlugins}
  var
    MemLoader: TDynMemLib;
    LibStream: TMemoryStream;
{$ENDIF}
begin
  Result := False;
  OnLoadPluginFromInMemFS := AOnLoadPluginFromInMemFS;
  OnAddToLog := AOnAddToLog;

  if Loaded then
  begin
    DoAddToLog('Plugin already loaded. Unloading first...');
    Unload(AOnAddToLog);
  end;

  Path := APath;
  OnExecuteActionByName := AOnExecuteActionByName;
  OnSetVar := AOnSetVar;
  OnSetDebugPoint := AOnSetDebugPoint;
  OnIsAtBreakPoint := AOnIsAtBreakPoint;
  OnLoadBitmap := AOnLoadBitmap;
  OnLoadRenderedBitmap := AOnLoadRenderedBitmap;
  OnSaveFileToExtRenderingInMemFS := AOnSaveFileToExtRenderingInMemFS;
  OnScreenshotByActionName := AOnScreenshotByActionName;

  FIsDebugging := IsDebugging;
  FShouldStopAtBreakPoint := AShouldStopAtBreakPoint;
  FStopAllActionsOnDemandFromParent := AStopAllActionsOnDemandFromParent;
  FStepOver := AStepOver;
  FPluginContinueAll := APluginContinueAll;
  FInMemFS := AInMemFS;

  ResultBmp := AResultBmp;
  FFullTemplatesDir := AFullTemplatesDir;
  FAllowedFileDirsForServer := AAllowedFileDirsForServer;
  FAllowedFileExtensionsForServer := AAllowedFileExtensionsForServer;

  AllActions := AAllActions;
  AllVars := AAllVars;

  DoAddToLog('Loading plugin for execute...');

  {$IFDEF MemPlugins}
    if IsMemPluginPath(Path) then
    begin
      MemLoader := TDynMemLib.Create;
      PluginHandle := UInt64(MemLoader);

      LibStream := TMemoryStream.Create;
      try
        if not DoOnLoadPluginFromInMemFS(LibStream, Path) then
        begin
          OnAddToLog('Plugin not found in InMem file system: ' + Path);
          Loaded := False;
        end
        else
        begin
          try
            LibStream.Position := 0;
            Loaded := MemLoader.MemLoadLibrary(LibStream.Memory);
          except
            on E: Exception do
            begin
              Loaded := False;
              OnAddToLog('Cannot load plugin from memory. ' + E.Message);
            end;
          end;
        end;
      finally
        LibStream.Free;
      end;
    end
    else
    begin
      if not FileExists(Path) then
      begin
        Err := 'Plugin not found at: "' + Path + '".';
        Exit;
      end;

      PluginHandle := Windows.LoadLibrary(PChar(Path)); //LoadLibraryA(Path);
      Loaded := PluginHandle > 0;
    end;
  {$ELSE}
    PluginHandle := Windows.LoadLibrary(PChar(Path)); //LoadLibraryA(Path);
    Loaded := PluginHandle > 0;
  {$ENDIF}

  if Loaded then
  begin
    Err := '';
    DoAddToLog('Plugin loaded, with handle ' + IntToStr(PluginHandle));

    {$IFDEF MemPlugins}
      if IsMemPluginPath(Path) then
        Func.ExecutePluginFunc := MemLoader.MemGetProcAddress('ExecutePlugin')
      else
        Func.ExecutePluginFunc := GetProcedureAddress(PluginHandle, 'ExecutePlugin');
    {$ELSE}
      Func.ExecutePluginFunc := GetProcedureAddress(PluginHandle, 'ExecutePlugin');
    {$ENDIF}

    if @Func.ExecutePluginFunc = nil then
    begin
      Err := 'Cannot get address of ExecutePlugin.';
      Unload(AOnAddToLog);
      Exit;
    end;

    {$IFDEF MemPlugins}
      if IsMemPluginPath(Path) then
        Func.GetAPIVersionFunc := MemLoader.MemGetProcAddress('GetAPIVersion')
      else
        Func.GetAPIVersionFunc := GetProcedureAddress(PluginHandle, 'GetAPIVersion');
    {$ELSE}
      Func.GetAPIVersionFunc := GetProcedureAddress(PluginHandle, 'GetAPIVersion');
    {$ENDIF}

    if @Func.GetAPIVersionFunc = nil then
    begin
      Err := 'Cannot get address of GetAPIVersion.';
      Unload(AOnAddToLog);
      Exit;
    end;

    if GetAPIVersion <> CActionPlugin_APIVersion then
    begin
      Err := 'The plugin''s API vesion (' + IntToStr(GetAPIVersion) + ') does not match UIClicker''s API version (' + IntToStr(CActionPlugin_APIVersion) + ').';
      Unload(AOnAddToLog);
      Exit;
    end;

    try
      AddPluginHandleToList(PluginHandle);
      DoAddToLog('Added plugin handle to list: ' + IntToStr(PluginHandle));
    except
      on E: Exception do
      begin
        Err := 'Plugin loaded, but it has the same handle as an existing instance. Multiple instances are not supported. ' + E.Message + ' Unloading...';
        Unload(AOnAddToLog);
        Exit;
      end;
    end;
  end
  else
  begin
    Err := 'Invalid plugin at: "' + Path + '".';
    if Path = '' then
      Err := Err + CMustSetPluginFileNameMsg;

    Exit;
  end;

  Result := True;
end;


function TActionPlugin.LoadToGetProperties(APath: string;
                                           AOnLoadPluginFromInMemFS: TOnLoadPluginFromInMemFS;
                                           AOnUpdatePropertyIcons: TOnUpdatePropertyIcons;
                                           AOnAddToLog: TOnAddToLog): Boolean;
var
  PluginLocationInfo: string;

  {$IFDEF MemPlugins}
    MemLoader: TDynMemLib;
    LibStream: TMemoryStream;
  {$ENDIF}
begin
  Result := False;
  OnLoadPluginFromInMemFS := AOnLoadPluginFromInMemFS;
  OnAddToLog := AOnAddToLog;

  if Loaded then
    Unload(AOnAddToLog);

  Path := APath;
  PluginLocationInfo := '';

  DoAddToLog('Loading plugin for getting properties...');

  {$IFDEF MemPlugins}
    if IsMemPluginPath(Path) then
    begin
      MemLoader := TDynMemLib.Create;
      PluginHandle := UInt64(MemLoader);

      LibStream := TMemoryStream.Create;
      try
        if not DoOnLoadPluginFromInMemFS(LibStream, Path) then
        begin
          OnAddToLog('Plugin not found in InMem file system: ' + Path);
          Loaded := False;
        end
        else
        begin
          try
            LibStream.Position := 0;
            Loaded := MemLoader.MemLoadLibrary(LibStream.Memory);
          except
            on E: Exception do
            begin
              Loaded := False;
              OnAddToLog('Cannot load plugin from memory. ' + E.Message);
            end;
          end;
        end;
      finally
        LibStream.Free;
      end;

      PluginLocationInfo := ' from InMem file system';
    end
    else
    begin
      PluginHandle := Windows.LoadLibrary(PChar(Path));  //LoadLibraryA
      Loaded := PluginHandle > 0;
      PluginLocationInfo := ' from disk';
    end;
  {$ELSE}
    PluginHandle := Windows.LoadLibrary(PChar(Path));  //LoadLibraryA
    Loaded := PluginHandle > 0;
  {$ENDIF}

  if Loaded then
  begin
    Err := '';
    DoAddToLog('Plugin loaded' + PluginLocationInfo + ', with handle ' + IntToStr(PluginHandle) + ' and path: ' + Path);

    {$IFDEF MemPlugins}
      if IsMemPluginPath(APath) then
        Func.GetListOfPropertiesProc := MemLoader.MemGetProcAddress('GetListOfProperties')
      else
        Func.GetListOfPropertiesProc := GetProcedureAddress(PluginHandle, 'GetListOfProperties');
    {$ELSE}
      Func.GetListOfPropertiesProc := GetProcedureAddress(PluginHandle, 'GetListOfProperties');
    {$ENDIF}

    if @Func.GetListOfPropertiesProc = nil then
    begin
      Err := 'Cannot get address of GetListOfProperties';
      Unload(AOnAddToLog);
      Exit;
    end;

    {$IFDEF MemPlugins}
      if IsMemPluginPath(APath) then
        Func.GetAPIVersionFunc := MemLoader.MemGetProcAddress('GetAPIVersion')
      else
        Func.GetAPIVersionFunc := GetProcedureAddress(PluginHandle, 'GetAPIVersion');
    {$ELSE}
      Func.GetAPIVersionFunc := GetProcedureAddress(PluginHandle, 'GetAPIVersion');
    {$ENDIF}

    if @Func.GetAPIVersionFunc = nil then
    begin
      Err := 'Cannot get address of GetAPIVersion.';
      Unload(AOnAddToLog);
      Exit;
    end;

    if GetAPIVersion <> CActionPlugin_APIVersion then
    begin
      Err := 'The plugin''s API vesion (' + IntToStr(GetAPIVersion) + ') does not match UIClicker''s API version (' + IntToStr(CActionPlugin_APIVersion) + ').';
      Unload(AOnAddToLog);
      Exit;
    end;

    OnUpdatePropertyIcons := AOnUpdatePropertyIcons;
  end
  else
  begin
    Err := 'Invalid plugin at: "' + Path + '".';
    if Path = '' then
      Err := Err + CMustSetPluginFileNameMsg;

    Exit;
  end;

  Result := True;
end;


function TActionPlugin.Unload(AOnAddToLog: TOnAddToLog): Boolean;
{$IFDEF MemPlugins}
  var
    MemLoader: TDynMemLib;
{$ENDIF}
begin
  Result := False;
  OnAddToLog := AOnAddToLog;

  if Loaded then
  begin
    DoAddToLog('Unloading plugin with handle ' + IntToStr(PluginHandle) + '...');

    {$IFDEF MemPlugins}
      if IsMemPluginPath(Path) then
      begin
        MemLoader := TDynMemLib(PluginHandle);
        try
          MemLoader.MemFreeLibrary;
        except
          on E: Exception do
          begin
            Err := E.Message;
            Exit;
          end;
        end;
      end
      else
      begin
        //if not UnloadLibrary(PluginHandle) then
        if not Windows.FreeLibrary(PluginHandle) then
        begin
          Err := SysErrorMessage(GetLastOSError);
          Exit;
        end
        else
          DoAddToLog('Plugin unloaded...');
      end;
    {$ELSE}
      //if not UnloadLibrary(PluginHandle) then
      if not Windows.FreeLibrary(PluginHandle) then
      begin
        Err := SysErrorMessage(GetLastOSError);
        Exit;
      end
      else
        DoAddToLog('Plugin unloaded...');
    {$ENDIF}
  end;

  Result := True;
  Loaded := False;
end;


function TActionPlugin.GetAPIVersion: DWord;
begin
  if @Func.GetAPIVersionFunc = nil then
    raise Exception.Create('Plugin function not set: GetAPIVersion');

  Result := Func.GetAPIVersionFunc;
end;


procedure DoOnActionPlugin_UpdatePropertyIcons(APluginReference: Pointer; AStreamContent: Pointer; AStreamSize: Int64); cdecl;
var
  ActionPlugin: PActionPlugin;
begin
  ActionPlugin := APluginReference;
  ActionPlugin.OnUpdatePropertyIcons(AStreamContent, AStreamSize);
end;


function TActionPlugin.GetListOfProperties: string;
var
  Len: DWord;
begin
  if @Func.GetListOfPropertiesProc = nil then
    raise Exception.Create('Plugin function not set: GetListOfProperties');

  SetLength(Result, CMaxSharedStringLength);
  Func.GetListOfPropertiesProc(@Self,
                               @Result[1],
                               @Len,
                               @DoOnActionPlugin_UpdatePropertyIcons
                               );
  SetLength(Result, Len);
end;


function TActionPlugin.ExecutePlugin(AListOfPropertiesAndValues: string): Boolean;
var
  ListOfPluginSettingsLen: DWord;
  ActionPlugin: PActionPlugin;
begin
  if @Func.ExecutePluginFunc = nil then
    raise Exception.Create('Plugin function not set: ExecutePlugin');

  ListOfPluginSettingsLen := Length(AListOfPropertiesAndValues);

  if FPluginContinueAll <> nil then
    FPluginContinueAll^ := False; //reset flag before execution

  ActionPlugin := @Self;

  try
    FContinueAfterBreakPoint := False;  //used at pausing the execution at first debug point, to allow the user to set breakpoints

    if ActionPlugin^.FIsDebugging and ((ActionPlugin^.FPluginContinueAll <> nil) and not ActionPlugin^.FPluginContinueAll^) then
    begin
      ActionPlugin^.DoSetDebugPoint(CBeforePluginExecution_DbgLineContent);
      WaitInDebuggingMode(ActionPlugin);
    end;

    try
      Result := Func.ExecutePluginFunc(ActionPlugin,
                                       @AListOfPropertiesAndValues[1],
                                       @ListOfPluginSettingsLen,
                                       DoOnActionPlugin_GetActionCount_Callback,
                                       DoOnActionPlugin_GetActionInfoByIndex_Callback,
                                       DoOnActionPlugin_GetActionContentByIndex_Callback,
                                       DoOnActionPlugin_ExecuteAction_Callback,
                                       DoOnActionPlugin_GetAllTemplateVars_Callback,
                                       DoOnActionPlugin_SetTemplateVar_Callback,
                                       DoOnActionPlugin_DebugPoint_Callback,
                                       DoOnActionPlugin_AddToLog_Callback,
                                       DoOnActionPlugin_SetResultImg,
                                       DoOnActionPlugin_GetFileContent,
                                       DoOnActionPlugin_GetAllowedFilesInfo,
                                       DoOnActionPlugin_SetBitmap,
                                       DoOnActionPlugin_Screenshot,
                                       DoOnActionPlugin_CheckStopAllActionsOnDemand,
                                       DoOnActionPlugin_InMemFS
                                       );
    finally
      if FPluginContinueAll <> nil then
        FPluginContinueAll^ := False; //reset flag after execution
    end;
  finally
    try
      RemovePluginHandleFromList(PluginHandle);
      DoAddToLog('Removed plugin handle from list: ' + IntToStr(PluginHandle));
    except
      on E: Exception do
        DoAddToLog(E.Message);
    end;
  end;
end;


constructor TPluginInMemFSAccess.Create;
begin
  inherited Create;
  FInMemFS := nil;
end;


function TPluginInMemFSAccess.InMemFSFunc(ACallbackIndex: Integer; AInData1, AInData2: Pointer; AInDataLen1, AInDataLen2: Int64; AOnFileContent: TOnFileContentObj): Int64;
var
  Fnm, NewFnm, ListOfFiles: string;
begin
  if FInMemFS = nil then
    raise Exception.Create('Plugin InMemFS not assigned.');

  Result := 0;

  case ACallbackIndex of
    CPluginInMemFSFunc_LoadFileFromMem:   //Calls   procedure LoadFileFromMem(AName: string; AContent: Pointer; AvailableIndex: Integer = -1);
    begin
      SetPointedContentToString(AInData1, Fnm);
      FInMemFS.LoadFileFromMem(Fnm, AInData2);  //AInData1 points to AName, AInData2 points to AContent. The plugin has to call CPluginInMemFSFunc_GetFileSize in advance and preallocate that amount of memory, then set AInData2 pointer to it.
    end;

    CPluginInMemFSFunc_SaveFileToMem:     //Calls   procedure SaveFileToMem(AName: string; AContent: Pointer; ASize: Int64);
    begin
      SetPointedContentToString(AInData1, Fnm);
      FInMemFS.SaveFileToMem(Fnm, AInData2, AInDataLen2);  //AInData1 points to AName, AInData2 is set to AContent. AInDataLen2 is set to ASize.
    end;

    CPluginInMemFSFunc_GetFileSize:       //Calls   function GetFileSize(AName: string): Int64;
    begin
      SetPointedContentToString(AInData1, Fnm);
      Result := FInMemFS.GetFileSize(Fnm);
    end;

    CPluginInMemFSFunc_ListMemFiles:      //Calls   function ListMemFilesAsString: string
    begin
      ListOfFiles := FInMemFS.ListMemFilesAsString;
      Result := Length(ListOfFiles);
      AOnFileContent(@ListOfFiles[1], Result);
    end;

    CPluginInMemFSFunc_FileExistsInMem:   //Calls   function FileExistsInMem(AFileName: string): Boolean;
    begin
      SetPointedContentToString(AInData1, Fnm);
      Result := Int64(FInMemFS.FileExistsInMem(Fnm));
    end;

    CPluginInMemFSFunc_DeleteFileFromMem: //Calls   procedure DeleteFileFromMem(AFileName: string);
    begin
      SetPointedContentToString(AInData1, Fnm);
      FInMemFS.DeleteFileFromMem(Fnm);
    end;

    CPluginInMemFSFunc_DuplicateFile:     //Calls   function DuplicateFile(AFileName: string): string;
    begin
      SetPointedContentToString(AInData1, Fnm);
      NewFnm := FInMemFS.DuplicateFile(Fnm);
      Result := Length(NewFnm);
      AOnFileContent(@NewFnm[1], Result);
    end;

    CPluginInMemFSFunc_RenameFile:        //Calls   procedure RenameFile(AFileName, NewFileName: string);
    begin
      SetPointedContentToString(AInData1, Fnm);
      SetPointedContentToString(AInData2, NewFnm);

      try
        FInMemFS.RenameFile(Fnm, NewFnm);
      except
        on E: Exception do
        begin
          if Pos('New file already exists on renaming.', E.Message) = 1 then
            Result := -1;

          if Pos('Can''t properly rename file.', E.Message) = 1 then
            Result := -2;

          if Pos('Can''t find file to rename.', E.Message) = 1 then
            Result := -3;
        end;
      end;
    end;

    else
      raise Exception.Create('Plugin InMemFS function not implemented at index ' + IntToStr(ACallbackIndex));
  end;
end;


initialization
  InitCriticalSection(LoadedPluginsCritSec);
  LoadedPluginHandles := TStringList.Create;


finalization
  DoneCriticalSection(LoadedPluginsCritSec);
  LoadedPluginHandles.Free;

end.

