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

    AllActions: PClkActionsRecArr;
    AllVars: TStringList;

    Func: TActionPluginFunc;
    procedure DoAddToLog(s: string);
    function DoExecuteActionByName(AActionName: string): Boolean;
    procedure DoSetVar(AVarName, AVarValue: string);

    function LoadToExecute(APath: string; AOnAddToLog: TOnAddToLog; AOnExecuteActionByName: TOnExecuteActionByName; AOnSetVar: TOnSetVar; AAllActions: PClkActionsRecArr = nil; AAllVars: TStringList = nil): Boolean;
    function LoadToGetProperties(APath: string): Boolean;
    function Unload: Boolean;

    function GetAPIVersion: DWord;
    function GetListOfProperties: string;
    function ExecutePlugin(AListOfPropertiesAndValues: string): Boolean;
  end;

  PActionPlugin = ^TActionPlugin;


implementation


uses
  DllUtils;


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


procedure DoOnActionPlugin_SetTemplateVar(APluginReference: Pointer; AVarName, AVarValue: Pointer); cdecl;
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


function TActionPlugin.LoadToExecute(APath: string; AOnAddToLog: TOnAddToLog; AOnExecuteActionByName: TOnExecuteActionByName; AOnSetVar: TOnSetVar; AAllActions: PClkActionsRecArr = nil; AAllVars: TStringList = nil): Boolean;
begin
  Result := False;

  if Loaded then
    Unload;

  Path := APath;
  OnAddToLog := AOnAddToLog;
  OnExecuteActionByName := AOnExecuteActionByName;
  OnSetVar := AOnSetVar;

  AllActions := AAllActions;
  AllVars := AAllVars;

  PluginHandle := LoadLibrary(Path);
  Loaded := PluginHandle > 0;

  if Loaded then
  begin
    Err := '';

    Func.ExecutePluginFunc := GetProcedureAddress(PluginHandle, 'ExecutePlugin');
    if @Func.ExecutePluginFunc = nil then
    begin
      Err := 'Cannot get address of ExecutePlugin';
      Unload;
      Exit;
    end;
  end
  else
    Err := GetLoadErrorStr + '   ' + SysErrorMessage(GetLastOSError);

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
  end
  else
    Err := GetLoadErrorStr + '   ' + SysErrorMessage(GetLastOSError);

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
      Err := '';
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
begin
  if @Func.ExecutePluginFunc = nil then
    raise Exception.Create('Plugin function not set: ExecutePlugin');

  ListOfPluginSettingsLen := Length(AListOfPropertiesAndValues);

  Result := Func.ExecutePluginFunc(@Self,
                                   @AListOfPropertiesAndValues[1],
                                   @ListOfPluginSettingsLen,
                                   DoOnActionPlugin_GetActionCount_Callback,
                                   DoOnActionPlugin_GetActionInfoByIndex_Callback,
                                   DoOnActionPlugin_ExecuteAction_Callback,
                                   DoOnActionPlugin_GetAllTemplateVars_Callback,
                                   DoOnActionPlugin_SetTemplateVar);
end;

end.

