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
  Windows, Classes, SysUtils, ClickerUtils, ClickerActionPlugins, dynlibs;


type
  TActionPlugin = record
    Path: string;
    Loaded: Boolean;
    Err: string;
    PluginHandle: THandle;
    OnAddToLog: TOnAddToLog;
    OnExecuteActionByName: TOnExecuteActionByName;
    OnSetVar: TOnSetVar;
    OnSetDebugPoint: TOnSetDebugPoint;
    OnIsAtBreakPoint: TOnIsAtBreakPoint;

    FIsDebugging, FShouldStopAtBreakPoint: Boolean;
    FStopAllActionsOnDemandFromParent, FPluginContinueAll: PBoolean;
    FStepOver: PBoolean;
    FIsFirstExecution: Boolean; //Flag, used to stop the execution, to allow the user to set breakpoints
    FContinueAfterBreakPoint: Boolean;

    AllActions: PClkActionsRecArr;
    AllVars: TStringList;

    Func: TActionPluginFunc;
    procedure DoAddToLog(s: string);
    function DoExecuteActionByName(AActionName: string): Boolean;
    procedure DoSetVar(AVarName, AVarValue: string);
    procedure DoSetDebugPoint(ADebugPoint: string);
    function DoIsAtBreakPoint(ADebugPoint: string): Boolean;

    function LoadToExecute(APath: string; AOnAddToLog: TOnAddToLog; AOnExecuteActionByName: TOnExecuteActionByName; AOnSetVar: TOnSetVar; AOnSetDebugPoint: TOnSetDebugPoint; AOnIsAtBreakPoint: TOnIsAtBreakPoint; IsDebugging, AShouldStopAtBreakPoint: Boolean; AStopAllActionsOnDemandFromParent, AStepOver, APluginContinueAll: PBoolean; AAllActions: PClkActionsRecArr = nil; AAllVars: TStringList = nil): Boolean;
    function LoadToGetProperties(APath: string): Boolean;
    function Unload: Boolean;

    function GetAPIVersion: DWord;
    function GetListOfProperties: string;
    function ExecutePlugin(AListOfPropertiesAndValues: string): Boolean;
  end;

  PActionPlugin = ^TActionPlugin;


implementation


uses
  DllUtils, Forms, ClickerActionsClient;


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


function DoOnActionPlugin_DebugPoint_Callback(APluginReference: Pointer; APointName, ALogMsg: Pointer): Boolean; //The handler should return True, to continue execution. If False, the dll should exit ExecutePluginFunc (when users stop the execution).
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
      ActionPlugin^.DoAddToLog('Exiting plugin debug point "' + PointName);
    end;
  except
    on E: Exception do
      ActionPlugin^.DoAddToLog('Plugin loader: ' + E.Message);
  end;

  Result := True;
end;


procedure TActionPlugin.DoAddToLog(s: string);
begin
  if Assigned(OnAddToLog) then
    OnAddToLog(s);
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


function TActionPlugin.LoadToExecute(APath: string; AOnAddToLog: TOnAddToLog; AOnExecuteActionByName: TOnExecuteActionByName; AOnSetVar: TOnSetVar; AOnSetDebugPoint: TOnSetDebugPoint; AOnIsAtBreakPoint: TOnIsAtBreakPoint; IsDebugging, AShouldStopAtBreakPoint: Boolean; AStopAllActionsOnDemandFromParent, AStepOver, APluginContinueAll: PBoolean; AAllActions: PClkActionsRecArr = nil; AAllVars: TStringList = nil): Boolean;
begin
  Result := False;

  if Loaded then
    Unload;

  Path := APath;
  OnAddToLog := AOnAddToLog;
  OnExecuteActionByName := AOnExecuteActionByName;
  OnSetVar := AOnSetVar;
  OnSetDebugPoint := AOnSetDebugPoint;
  OnIsAtBreakPoint := AOnIsAtBreakPoint;

  FIsDebugging := IsDebugging;
  FShouldStopAtBreakPoint := AShouldStopAtBreakPoint;
  FStopAllActionsOnDemandFromParent := AStopAllActionsOnDemandFromParent;
  FStepOver := AStepOver;
  FPluginContinueAll := APluginContinueAll;

  AllActions := AAllActions;
  AllVars := AAllVars;

  if not FileExists(Path) then
  begin
    Err := 'Plugin not found at: "' + Path + '".';
    Exit;
  end;

  PluginHandle := LoadLibrary(Path);
  Loaded := PluginHandle > 0;

  if Loaded then
  begin
    Err := '';

    Func.ExecutePluginFunc := GetProcedureAddress(PluginHandle, 'ExecutePlugin');
    if @Func.ExecutePluginFunc = nil then
    begin
      Err := 'Cannot get address of ExecutePlugin.';
      Unload;
      Exit;
    end;

    Func.GetAPIVersionFunc := GetProcedureAddress(PluginHandle, 'GetAPIVersion');
    if @Func.GetAPIVersionFunc = nil then
    begin
      Err := 'Cannot get address of GetAPIVersion.';
      Unload;
      Exit;
    end;

    if GetAPIVersion <> CActionPlugin_APIVersion then
    begin
      Err := 'The plugin''s API vesion (' + IntToStr(GetAPIVersion) + ') does not match UIClicker''s API version (' + IntToStr(CActionPlugin_APIVersion) + ').';
      Unload;
      Exit;
    end;
  end
  else
  begin
    Err := 'Invalid plugin at: "' + Path + '".';
    Exit;
  end;

  Result := True;
end;


function TActionPlugin.LoadToGetProperties(APath: string): Boolean;
begin
  Result := False;

  if Loaded then
    Unload;

  Path := APath;

  PluginHandle := LoadLibrary(Path);
  Loaded := PluginHandle > 0;

  if Loaded then
  begin
    Err := '';

    Func.GetListOfPropertiesProc := GetProcedureAddress(PluginHandle, 'GetListOfProperties');
    if @Func.GetListOfPropertiesProc = nil then
    begin
      Err := 'Cannot get address of GetListOfProperties';
      Unload;
      Exit;
    end;

    Func.GetAPIVersionFunc := GetProcedureAddress(PluginHandle, 'GetAPIVersion');
    if @Func.GetAPIVersionFunc = nil then
    begin
      Err := 'Cannot get address of GetAPIVersion.';
      Unload;
      Exit;
    end;

    if GetAPIVersion <> CActionPlugin_APIVersion then
    begin
      Err := 'The plugin''s API vesion (' + IntToStr(GetAPIVersion) + ') does not match UIClicker''s API version (' + IntToStr(CActionPlugin_APIVersion) + ').';
      Unload;
      Exit;
    end;
  end
  else
  begin
    Err := 'Invalid plugin at: "' + Path + '".';
    Exit;
  end;

  Result := True;
end;


function TActionPlugin.Unload: Boolean;
begin
  Result := False;

  if Loaded then
  begin
    if not UnloadLibrary(PluginHandle) then
    begin
      Err := SysErrorMessage(GetLastOSError);
      Exit;
    end
    else
      ;
  end;

  Result := True;
end;


function TActionPlugin.GetAPIVersion: DWord;
begin
  if @Func.GetAPIVersionFunc = nil then
    raise Exception.Create('Plugin function not set: GetAPIVersion');

  Result := Func.GetAPIVersionFunc;
end;


function TActionPlugin.GetListOfProperties: string;
var
  Len: DWord;
begin
  if @Func.GetListOfPropertiesProc = nil then
    raise Exception.Create('Plugin function not set: GetListOfProperties');

  SetLength(Result, CMaxSharedStringLength);
  Func.GetListOfPropertiesProc(@Result[1], @Len);
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
                                     DoOnActionPlugin_ExecuteAction_Callback,
                                     DoOnActionPlugin_GetAllTemplateVars_Callback,
                                     DoOnActionPlugin_SetTemplateVar_Callback,
                                     DoOnActionPlugin_DebugPoint_Callback);
  finally
    if FPluginContinueAll <> nil then
      FPluginContinueAll^ := False; //reset flag after execution
  end;
end;

end.

