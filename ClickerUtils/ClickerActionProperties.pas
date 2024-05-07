{
    Copyright (C) 2023 VCC
    creation date: Nov 2023
    initial release date: 19 Nov 2023

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


unit ClickerActionProperties;

{$mode Delphi}

interface

uses
  Classes, SysUtils, ClickerUtils;


function GetClickActionProperties(AClickOptions: TClkClickOptions): string;
function GetExecAppActionProperties(AExecAppOptions: TClkExecAppOptions): string;
function GetFindControlActionProperties(AFindControlOptions: TClkFindControlOptions): string;
function GetSetControlTextActionProperties(ASetTextOptions: TClkSetTextOptions): string;
function GetCallTemplateActionProperties(ACallTemplateOptions: TClkCallTemplateOptions): string;
function GetSleepActionProperties(ASleepOptions: TClkSleepOptions): string;
function GetSetVarActionProperties(ASetVarOptions: TClkSetVarOptions): string;
function GetWindowOperationsActionProperties(AWindowOperationsOptions: TClkWindowOperationsOptions): string;
function GetLoadSetVarFromFileActionProperties(ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions): string;
function GetSaveSetVarToFileActionProperties(ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions): string;
function GetPluginActionProperties(PluginOptions: TClkPluginOptions): string;
//when adding new Get<ActionType>Properties functions, please update DoOnActionPlugin_GetActionContentByIndex_Callback from ClickerActionPluginLoader.pas


//The Set<ActionType>Properties functions return an error if any, or emptry string for success.
function SetClickActionProperties(AListOfClickOptionsParams: TStrings; out AClickOptions: TClkClickOptions): string;
function SetExecAppActionProperties(AListOfExecAppOptionsParams: TStrings; out AExecAppOptions: TClkExecAppOptions; out AActionOptions: TClkActionOptions): string;
function SetFindControlActionProperties(AListOfFindControlOptionsParams: TStrings; AIsSubControl: Boolean; AOnAddToLog: TOnAddToLog; out AFindControlOptions: TClkFindControlOptions; out AActionOptions: TClkActionOptions): string; //AOnAddToLog can be set to nil if not used.
function SetSetControlTextActionProperties(AListOfSetControlTextOptionsParams: TStrings; out ASetTextOptions: TClkSetTextOptions): string;
function SetCallTemplateActionProperties(AListOfCallTemplateOptionsParams: TStrings; out ACallTemplateOptions: TClkCallTemplateOptions): string;
function SetSleepActionProperties(AListOfSleepOptionsParams: TStrings; out ASleepOptions: TClkSleepOptions; out AActionOptions: TClkActionOptions): string;
function SetSetVarActionProperties(AListOfSetVarOptionsParams: TStrings; out ASetVarOptions: TClkSetVarOptions): string;
function SetWindowOperationsActionProperties(AListOfWindowOperationsOptionsParams: TStrings; out AWindowOperationsOptions: TClkWindowOperationsOptions): string;
function SetLoadSetVarFromFileActionProperties(AListOfLoadSetVarOptionsParams: TStrings; out ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions): string;
function SetSaveSetVarToFileActionProperties(AListOfSaveSetVarOptionsParams: TStrings; out ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions): string;
function SetPluginActionProperties(APluginOptionsParams: TStrings; out APluginOptions: TClkPluginOptions): string;


implementation


uses
  Controls, Graphics;


function GetClickActionProperties(AClickOptions: TClkClickOptions): string;
begin
  Result := 'XClickPointReference' + '=' + IntToStr(Ord(AClickOptions.XClickPointReference)) + '&' +
            'YClickPointReference' + '=' + IntToStr(Ord(AClickOptions.YClickPointReference)) + '&' +
            'XClickPointVar' + '=' + AClickOptions.XClickPointVar + '&' +
            'YClickPointVar' + '=' + AClickOptions.YClickPointVar + '&' +
            'XOffset' + '=' + AClickOptions.XOffset + '&' +
            'YOffset' + '=' + AClickOptions.YOffset + '&' +
            'MouseButton' + '=' + IntToStr(Ord(AClickOptions.MouseButton)) + '&' +
            'ClickWithCtrl' + '=' + IntToStr(Ord(AClickOptions.ClickWithCtrl)) + '&' +
            'ClickWithAlt' + '=' + IntToStr(Ord(AClickOptions.ClickWithAlt)) + '&' +
            'ClickWithShift' + '=' + IntToStr(Ord(AClickOptions.ClickWithShift)) + '&' +
            'ClickWithDoubleClick' + '=' + IntToStr(Ord(AClickOptions.ClickWithDoubleClick)) + '&' +
            'Count' + '=' + IntToStr(AClickOptions.Count) + '&' +
            'LeaveMouse' + '=' + IntToStr(Ord(AClickOptions.LeaveMouse)) + '&' +
            'MoveWithoutClick' + '=' + IntToStr(Ord(AClickOptions.MoveWithoutClick)) + '&' +
            'ClickType' + '=' + IntToStr(Ord(AClickOptions.ClickType)) + '&' +
            'XClickPointReferenceDest' + '=' + IntToStr(Ord(AClickOptions.XClickPointReferenceDest)) + '&' +
            'YClickPointReferenceDest' + '=' + IntToStr(Ord(AClickOptions.YClickPointReferenceDest)) + '&' +
            'XClickPointVarDest' + '=' + AClickOptions.XClickPointVarDest + '&' +
            'YClickPointVarDest' + '=' + AClickOptions.YClickPointVarDest + '&' +
            'XOffsetDest' + '=' + AClickOptions.XOffsetDest + '&' +
            'YOffsetDest' + '=' + AClickOptions.YOffsetDest + '&' +
            'MouseWheelType' + '=' + IntToStr(Ord(AClickOptions.MouseWheelType)) + '&' +
            'MouseWheelAmount' + '=' + AClickOptions.MouseWheelAmount + '&' +
            'DelayAfterMovingToDestination' + '=' + AClickOptions.DelayAfterMovingToDestination + '&' +
            'DelayAfterMouseDown' + '=' + AClickOptions.DelayAfterMouseDown + '&' +
            'MoveDuration' + '=' + AClickOptions.MoveDuration;
end;


function GetExecAppActionProperties(AExecAppOptions: TClkExecAppOptions): string;
begin
  Result := 'PathToApp' + '=' + AExecAppOptions.PathToApp + '&' +
            'ListOfParams' + '=' + FastReplace_ReturnTo45(AExecAppOptions.ListOfParams) + '&' +
            'WaitForApp' + '=' + IntToStr(Ord(AExecAppOptions.WaitForApp)) + '&' +
            'AppStdIn' + '=' + AExecAppOptions.AppStdIn + '&' +
            'CurrentDir' + '=' + AExecAppOptions.CurrentDir + '&' +
            'UseInheritHandles' + '=' + IntToStr(Ord(AExecAppOptions.UseInheritHandles)) + '&' +
            'NoConsole' + '=' + IntToStr(Ord(AExecAppOptions.NoConsole));
end;


function GetFindControlActionProperties(AFindControlOptions: TClkFindControlOptions): string;
  function GetMatchBitmapTextContent(var AMatchBitmapText: TClkFindControlMatchBitmapTextArr): string;
  var
    i: Integer;
    Prefix: string;
  begin
    Result := '';
    for i := 0 to Length(AMatchBitmapText) - 1 do
    begin
      Prefix := 'MatchBitmapText[' + IntToStr(i) + '].';
      Result := Result + Prefix + 'ForegroundColor' + '=' + AMatchBitmapText[i].ForegroundColor + '&';
      Result := Result + Prefix + 'BackgroundColor' + '=' + AMatchBitmapText[i].BackgroundColor + '&';
      Result := Result + Prefix + 'FontName' + '=' + AMatchBitmapText[i].FontName + '&';
      Result := Result + Prefix + 'FontSize' + '=' + IntToStr(AMatchBitmapText[i].FontSize) + '&';
      Result := Result + Prefix + 'Bold' + '=' + IntToStr(Ord(AMatchBitmapText[i].Bold)) + '&';
      Result := Result + Prefix + 'Italic' + '=' + IntToStr(Ord(AMatchBitmapText[i].Italic)) + '&';
      Result := Result + Prefix + 'Underline' + '=' + IntToStr(Ord(AMatchBitmapText[i].Underline)) + '&';
      Result := Result + Prefix + 'StrikeOut' + '=' + IntToStr(Ord(AMatchBitmapText[i].StrikeOut)) + '&';
      Result := Result + Prefix + 'FontQuality' + '=' + IntToStr(Ord(AMatchBitmapText[i].FontQuality)) + '&';
      Result := Result + Prefix + 'FontQualityUsesReplacement' + '=' + IntToStr(Ord(AMatchBitmapText[i].FontQualityUsesReplacement)) + '&';
      Result := Result + Prefix + 'FontQualityReplacement' + '=' + AMatchBitmapText[i].FontQualityReplacement + '&';
      Result := Result + Prefix + 'ProfileName' + '=' + AMatchBitmapText[i].ProfileName + '&';
      Result := Result + Prefix + 'CropLeft' + '=' + AMatchBitmapText[i].CropLeft + '&';
      Result := Result + Prefix + 'CropTop' + '=' + AMatchBitmapText[i].CropTop + '&';
      Result := Result + Prefix + 'CropRight' + '=' + AMatchBitmapText[i].CropRight + '&';
      Result := Result + Prefix + 'CropBottom' + '=' + AMatchBitmapText[i].CropBottom + '&';
      Result := Result + Prefix + 'IgnoreBackgroundColor' + '=' + IntToStr(Ord(AMatchBitmapText[i].IgnoreBackgroundColor)) + '&';
    end;
  end;
begin
  Result := 'MatchCriteria.SearchForControlMode' + '=' + IntToStr(Ord(AFindControlOptions.MatchCriteria.SearchForControlMode)) + '&' +
            'MatchCriteria.WillMatchText' + '=' + IntToStr(Ord(AFindControlOptions.MatchCriteria.WillMatchText)) + '&' +
            'MatchCriteria.WillMatchClassName' + '=' + IntToStr(Ord(AFindControlOptions.MatchCriteria.WillMatchClassName)) + '&' +
            'MatchCriteria.WillMatchBitmapText' + '=' + IntToStr(Ord(AFindControlOptions.MatchCriteria.WillMatchBitmapText)) + '&' +
            'MatchCriteria.WillMatchBitmapFiles' + '=' + IntToStr(Ord(AFindControlOptions.MatchCriteria.WillMatchBitmapFiles)) + '&' +
            'MatchCriteria.WillMatchPrimitiveFiles' + '=' + IntToStr(Ord(AFindControlOptions.MatchCriteria.WillMatchPrimitiveFiles)) + '&' +
            'AllowToFail' + '=' + IntToStr(Ord(AFindControlOptions.AllowToFail)) + '&' +

            'MatchText' + '=' + AFindControlOptions.MatchText + '&' +
            'MatchClassName' + '=' + AFindControlOptions.MatchClassName + '&' +
            'MatchTextSeparator' + '=' + AFindControlOptions.MatchTextSeparator + '&' +
            'MatchClassNameSeparator' + '=' + AFindControlOptions.MatchClassNameSeparator + '&' +

            'MatchBitmapText.Count' + '=' + IntToStr(Length(AFindControlOptions.MatchBitmapText)) + '&' +
            GetMatchBitmapTextContent(AFindControlOptions.MatchBitmapText) +
            'MatchBitmapFiles' + '=' + FastReplace_ReturnTo45(AFindControlOptions.MatchBitmapFiles) + '&' +
            'MatchBitmapAlgorithm' + '=' + IntToStr(Ord(AFindControlOptions.MatchBitmapAlgorithm)) + '&' +
            'MatchBitmapAlgorithmSettings.XMultipleOf' + '=' + IntToStr(AFindControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf) + '&' +
            'MatchBitmapAlgorithmSettings.YMultipleOf' + '=' + IntToStr(AFindControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf) + '&' +
            'MatchBitmapAlgorithmSettings.XOffset' + '=' + IntToStr(AFindControlOptions.MatchBitmapAlgorithmSettings.XOffset) + '&' +
            'MatchBitmapAlgorithmSettings.YOffset' + '=' + IntToStr(AFindControlOptions.MatchBitmapAlgorithmSettings.YOffset) + '&' +
            'InitialRectangle.Left' + '=' + AFindControlOptions.InitialRectangle.Left + '&' +
            'InitialRectangle.Top' + '=' + AFindControlOptions.InitialRectangle.Top + '&' +
            'InitialRectangle.Right' + '=' + AFindControlOptions.InitialRectangle.Right + '&' +
            'InitialRectangle.Bottom' + '=' + AFindControlOptions.InitialRectangle.Bottom + '&' +
            'InitialRectangle.LeftOffset' + '=' + AFindControlOptions.InitialRectangle.LeftOffset + '&' +
            'InitialRectangle.TopOffset' + '=' + AFindControlOptions.InitialRectangle.TopOffset + '&' +
            'InitialRectangle.RightOffset' + '=' + AFindControlOptions.InitialRectangle.RightOffset + '&' +
            'InitialRectangle.BottomOffset' + '=' + AFindControlOptions.InitialRectangle.BottomOffset + '&' +
            'UseWholeScreen' + '=' + IntToStr(Ord(AFindControlOptions.UseWholeScreen)) + '&' +
            'ColorError' + '=' + AFindControlOptions.ColorError + '&' +
            'AllowedColorErrorCount' + '=' + AFindControlOptions.AllowedColorErrorCount + '&' +
            'WaitForControlToGoAway' + '=' + IntToStr(Ord(AFindControlOptions.WaitForControlToGoAway)) + '&' +
            'StartSearchingWithCachedControl' + '=' + IntToStr(Ord(AFindControlOptions.StartSearchingWithCachedControl)) + '&' +
            'CachedControlLeft' + '=' + AFindControlOptions.ColorError + '&' +
            'CachedControlTop' + '=' + AFindControlOptions.ColorError + '&' +
            'MatchPrimitiveFiles' + '=' + FastReplace_ReturnTo45(AFindControlOptions.MatchPrimitiveFiles) + '&' +
            'GetAllControls' + '=' + IntToStr(Ord(AFindControlOptions.GetAllControls)) + '&' +
            'UseFastSearch' + '=' + IntToStr(Ord(AFindControlOptions.UseFastSearch)) + '&' +
            'FastSearchAllowedColorErrorCount' + '=' + AFindControlOptions.FastSearchAllowedColorErrorCount + '&' +
            'IgnoredColors' + '=' + AFindControlOptions.IgnoredColors + '&' +
            'SleepySearch' + '=' + IntToStr(Ord(AFindControlOptions.SleepySearch)) + '&' +
            'StopSearchOnMismatch' + '=' + IntToStr(Ord(AFindControlOptions.StopSearchOnMismatch)) + '&' +
            'ImageSource' + '=' + IntToStr(Ord(AFindControlOptions.ImageSource)) + '&' +
            'SourceFileName' + '=' + AFindControlOptions.SourceFileName + '&' +
            'ImageSourceFileNameLocation' + '=' + IntToStr(Ord(AFindControlOptions.ImageSourceFileNameLocation));
end;


function GetSetControlTextActionProperties(ASetTextOptions: TClkSetTextOptions): string;
begin
  Result := 'Text' + '=' + ASetTextOptions.Text + '&' +
            'ControlType' + '=' + IntToStr(Ord(ASetTextOptions.ControlType)) + '&' +
            'DelayBetweenKeyStrokes' + '=' + ASetTextOptions.DelayBetweenKeyStrokes + '&' +
            'Count' + '=' + ASetTextOptions.Count;
end;


function GetCallTemplateActionProperties(ACallTemplateOptions: TClkCallTemplateOptions): string;
begin
  Result := 'TemplateFileName' + '=' + ACallTemplateOptions.TemplateFileName + '&' +
            'ListOfCustomVarsAndValues' + '=' + FastReplace_ReturnTo45(ACallTemplateOptions.ListOfCustomVarsAndValues) + '&' +
            'EvaluateBeforeCalling' + '=' + IntToStr(Ord(ACallTemplateOptions.EvaluateBeforeCalling)) + '&' +

            'Loop.Enabled' + '=' + IntToStr(Ord(ACallTemplateOptions.CallTemplateLoop.Enabled)) + '&' +
            'Loop.Counter' + '=' + ACallTemplateOptions.CallTemplateLoop.Counter + '&' +
            'Loop.InitValue' + '=' + ACallTemplateOptions.CallTemplateLoop.InitValue + '&' +
            'Loop.EndValue' + '=' + ACallTemplateOptions.CallTemplateLoop.EndValue + '&' +
            'Loop.Direction' + '=' + IntToStr(Ord(ACallTemplateOptions.CallTemplateLoop.Direction)) + '&' +
            'Loop.BreakCondition' + '=' + FastReplace_ReturnTo45(ACallTemplateOptions.CallTemplateLoop.BreakCondition) + '&' +
            'Loop.EvalBreakPosition' + '=' + IntToStr(Ord(ACallTemplateOptions.CallTemplateLoop.EvalBreakPosition));
end;


function GetSleepActionProperties(ASleepOptions: TClkSleepOptions): string;
begin
  Result := 'Value' + '=' + ASleepOptions.Value;
end;


function GetSetVarActionProperties(ASetVarOptions: TClkSetVarOptions): string;
begin
  Result := 'ListOfVarNames' + '=' + FastReplace_ReturnTo45(ASetVarOptions.ListOfVarNames) + '&' +
            'ListOfVarValues' + '=' + FastReplace_ReturnTo45(ASetVarOptions.ListOfVarValues) + '&' +
            'ListOfVarEvalBefore' + '=' + FastReplace_ReturnTo45(ASetVarOptions.ListOfVarEvalBefore);
end;


function GetWindowOperationsActionProperties(AWindowOperationsOptions: TClkWindowOperationsOptions): string;
begin
  Result := 'Operation' + '=' + IntToStr(Ord(AWindowOperationsOptions.Operation)) + '&' +
            'NewX' + '=' + AWindowOperationsOptions.NewX + '&' +
            'NewY' + '=' + AWindowOperationsOptions.NewY + '&' +
            'NewWidth' + '=' + AWindowOperationsOptions.NewWidth + '&' +
            'NewHeight' + '=' + AWindowOperationsOptions.NewHeight + '&' +
            'NewPositionEnabled' + '=' + IntToStr(Ord(AWindowOperationsOptions.NewPositionEnabled)) + '&' +
            'NewSizeEnabled' + '=' + IntToStr(Ord(AWindowOperationsOptions.NewSizeEnabled))
end;


function GetLoadSetVarFromFileActionProperties(ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions): string;
begin
  Result := 'FileName' + '=' + ALoadSetVarFromFileOptions.FileName + '&' +
            'SetVarActionName' + '=' + ALoadSetVarFromFileOptions.SetVarActionName;
end;


function GetSaveSetVarToFileActionProperties(ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions): string;
begin
  Result := 'FileName' + '=' + ASaveSetVarToFileOptions.FileName + '&' +
            'SetVarActionName' + '=' + ASaveSetVarToFileOptions.SetVarActionName;
end;


function GetPluginActionProperties(PluginOptions: TClkPluginOptions): string;
begin
  Result := 'FileName' + '=' + PluginOptions.FileName + '&' +
            'ListOfPropertiesAndValues' + '=' + FastReplace_ReturnTo45(PluginOptions.ListOfPropertiesAndValues);
end;



function SetClickActionProperties(AListOfClickOptionsParams: TStrings; out AClickOptions: TClkClickOptions): string; //returns error if any, or emptry string for success
var
  Temp_XClickPointReference: Integer;
  Temp_YClickPointReference: Integer;
  Temp_MouseButton, Temp_MouseWheelType: Integer;
  Temp_XClickPointReferenceDest: Integer;
  Temp_YClickPointReferenceDest: Integer;
  Temp_ClickType: Integer;
  Temp_Count: Int64;
begin
  Result := '';

  Temp_XClickPointReference := StrToIntDef(AListOfClickOptionsParams.Values['XClickPointReference'], 0);
  if (Temp_XClickPointReference < 0) or (Temp_XClickPointReference > Ord(High(TXClickPointReference))) then
  begin
    Result := 'XClickPointReference is out of range.';
    Exit;
  end;

  Temp_YClickPointReference := StrToIntDef(AListOfClickOptionsParams.Values['YClickPointReference'], 0);
  if (Temp_YClickPointReference < 0) or (Temp_YClickPointReference > Ord(High(TYClickPointReference))) then
  begin
    Result := 'YClickPointReference is out of range.';
    Exit;
  end;

  Temp_MouseButton := StrToIntDef(AListOfClickOptionsParams.Values['MouseButton'], Ord(mbLeft));
  if (Temp_MouseButton < 0) or (Temp_MouseButton > Ord(High(TMouseButton))) then
  begin
    Result := 'MouseButton is out of range.';
    Exit;
  end;

  Temp_MouseWheelType := StrToIntDef(AListOfClickOptionsParams.Values['MouseWheelType'], Ord(mwtVert));
  if (Temp_MouseWheelType < 0) or (Temp_MouseWheelType > Ord(High(TMouseWheelType))) then
  begin
    Result := 'MouseWheelType is out of range.';
    Exit;
  end;

  Temp_XClickPointReferenceDest := StrToIntDef(AListOfClickOptionsParams.Values['XClickPointReferenceDest'], 0);
  if (Temp_XClickPointReferenceDest < 0) or (Temp_XClickPointReferenceDest > Ord(High(TXClickPointReference))) then
  begin
    Result := 'XClickPointReferenceDest is out of range.';
    Exit;
  end;

  Temp_YClickPointReferenceDest := StrToIntDef(AListOfClickOptionsParams.Values['YClickPointReferenceDest'], 0);
  if (Temp_YClickPointReferenceDest < 0) or (Temp_YClickPointReferenceDest > Ord(High(TYClickPointReference))) then
  begin
    Result := 'YClickPointReferenceDest is out of range.';
    Exit;
  end;

  Temp_ClickType := StrToIntDef(AListOfClickOptionsParams.Values['ClickType'], CClickType_Click);
  if (Temp_ClickType < 0) or (Temp_ClickType > CClickType_Count - 1) then
  begin
    Result := 'ClickType is out of range.';
    Exit;
  end;

  Temp_Count := StrToInt64Def(AListOfClickOptionsParams.Values['Count'], 1);
  if (Temp_Count < 1) or (Temp_Count > Int64(MaxLongint)) then
  begin
    Result := 'Count is out of range.';
    Exit;
  end;

  AClickOptions.XClickPointReference := TXClickPointReference(Temp_XClickPointReference);
  AClickOptions.YClickPointReference := TYClickPointReference(Temp_YClickPointReference);
  AClickOptions.XClickPointVar := AListOfClickOptionsParams.Values['XClickPointVar'];
  AClickOptions.YClickPointVar := AListOfClickOptionsParams.Values['YClickPointVar'];
  AClickOptions.XOffset := AListOfClickOptionsParams.Values['XOffset'];
  AClickOptions.YOffset := AListOfClickOptionsParams.Values['YOffset'];
  AClickOptions.MouseButton := TMouseButton(Temp_MouseButton);
  AClickOptions.ClickWithCtrl := AListOfClickOptionsParams.Values['ClickWithCtrl'] = '1';
  AClickOptions.ClickWithAlt := AListOfClickOptionsParams.Values['ClickWithAlt'] = '1';
  AClickOptions.ClickWithShift := AListOfClickOptionsParams.Values['ClickWithShift'] = '1';
  AClickOptions.ClickWithDoubleClick := AListOfClickOptionsParams.Values['ClickWithDoubleClick'] = '1';
  AClickOptions.Count := Temp_Count;
  AClickOptions.LeaveMouse := AListOfClickOptionsParams.Values['LeaveMouse'] = '1';
  AClickOptions.MoveWithoutClick := AListOfClickOptionsParams.Values['MoveWithoutClick'] = '1';
  AClickOptions.ClickType := Temp_ClickType;    //see CClickType_Click and CClickType_Drag
  AClickOptions.XClickPointReferenceDest := TXClickPointReference(Temp_XClickPointReferenceDest);
  AClickOptions.YClickPointReferenceDest := TYClickPointReference(Temp_YClickPointReferenceDest);
  AClickOptions.XClickPointVarDest := AListOfClickOptionsParams.Values['XClickPointVarDest'];
  AClickOptions.YClickPointVarDest := AListOfClickOptionsParams.Values['YClickPointVarDest'];
  AClickOptions.XOffsetDest := AListOfClickOptionsParams.Values['XOffsetDest'];
  AClickOptions.YOffsetDest := AListOfClickOptionsParams.Values['YOffsetDest'];
  AClickOptions.MouseWheelType := TMouseWheelType(Temp_MouseWheelType);
  AClickOptions.MouseWheelAmount := AListOfClickOptionsParams.Values['MouseWheelAmount'];
  AClickOptions.DelayAfterMovingToDestination := AListOfClickOptionsParams.Values['DelayAfterMovingToDestination'];
  AClickOptions.DelayAfterMouseDown := AListOfClickOptionsParams.Values['DelayAfterMouseDown'];
  AClickOptions.MoveDuration := AListOfClickOptionsParams.Values['MoveDuration'];
end;


function SetExecAppActionProperties(AListOfExecAppOptionsParams: TStrings; out AExecAppOptions: TClkExecAppOptions; out AActionOptions: TClkActionOptions): string;
var
  Temp_UseInheritHandles: Integer;
  Temp_ActionTimeout: Int64;
begin
  Result := '';

  Temp_UseInheritHandles := StrToIntDef(AListOfExecAppOptionsParams.Values['UseInheritHandles'], 0);
  if (Temp_UseInheritHandles < 0) or (Temp_UseInheritHandles > Ord(High(TExecAppUseInheritHandles))) then
  begin
    Result := 'UseInheritHandles is out of range.';
    Exit;
  end;

  Temp_ActionTimeout := StrToIntDef(AListOfExecAppOptionsParams.Values['ActionTimeout'], 1000);
  if (Temp_ActionTimeout < 0) or (Temp_ActionTimeout > 2147483647) then
  begin
    Result := 'ActionTimeout is out of range.';
    Exit;
  end;

  AExecAppOptions.PathToApp := AListOfExecAppOptionsParams.Values['PathToApp'];
  AExecAppOptions.ListOfParams := FastReplace_45ToReturn(AListOfExecAppOptionsParams.Values['ListOfParams']);
  AExecAppOptions.WaitForApp := AListOfExecAppOptionsParams.Values['WaitForApp'] = '1';
  AExecAppOptions.AppStdIn := AListOfExecAppOptionsParams.Values['AppStdIn'];
  AExecAppOptions.CurrentDir := AListOfExecAppOptionsParams.Values['CurrentDir'];
  AExecAppOptions.UseInheritHandles := TExecAppUseInheritHandles(Temp_UseInheritHandles);
  AExecAppOptions.NoConsole := AListOfExecAppOptionsParams.Values['NoConsole'] = '1';

  AActionOptions.ActionName := AListOfExecAppOptionsParams.Values['ActionName'];
  AActionOptions.ActionTimeout := Temp_ActionTimeout;
  AActionOptions.Action := acExecApp;
end;


function SetFindControlActionProperties(AListOfFindControlOptionsParams: TStrings; AIsSubControl: Boolean; AOnAddToLog: TOnAddToLog; out AFindControlOptions: TClkFindControlOptions; out AActionOptions: TClkActionOptions): string; //AOnAddToLog can be set to nil if not used.
const
  CActionType: array[Boolean] of TClkAction = (acFindControl, acFindSubControl);
var
  Temp_SearchForControlMode: Integer;
  Temp_MatchBitmapTextCount: Integer;
  Temp_MatchBitmapAlgorithm: Integer;
  Temp_ImageSource: Integer;
  Temp_ImageSourceFileNameLocation: Integer;
  Temp_ActionTimeout: Int64;
  Temp_FontSize: Integer;
  Temp_FontQuality: Integer;
  Temp_CropLeft, Temp_CropTop, Temp_CropRight, Temp_CropBottom: string;
  i: Integer;
  Prefix: string;
begin
  Result := '';

  Temp_SearchForControlMode := StrToIntDef(AListOfFindControlOptionsParams.Values['MatchCriteria.SearchForControlMode'], 0);
  if (Temp_SearchForControlMode < 0) or (Temp_SearchForControlMode > Ord(High(TSearchForControlMode))) then
  begin
    Result := 'MatchCriteria.SearchForControlMode is out of range.';
    Exit;
  end;

  Temp_MatchBitmapTextCount := StrToIntDef(AListOfFindControlOptionsParams.Values['MatchBitmapText.Count'], 0);
  if (Temp_MatchBitmapTextCount < 0) or (Temp_MatchBitmapTextCount > 100) then
  begin
    Result := 'MatchBitmapText.Count is out of range.';
    Exit;
  end;

  Temp_MatchBitmapAlgorithm := StrToIntDef(AListOfFindControlOptionsParams.Values['MatchBitmapAlgorithm'], 0);
  if (Temp_MatchBitmapAlgorithm < 0) or (Temp_MatchBitmapAlgorithm > Ord(High(TMatchBitmapAlgorithm))) then
  begin
    Result := 'MatchBitmapAlgorithm is out of range.';
    Exit;
  end;

  Temp_ActionTimeout := StrToIntDef(AListOfFindControlOptionsParams.Values['ActionTimeout'], 1000);
  if (Temp_ActionTimeout < 0) or (Temp_ActionTimeout > 2147483647) then
  begin
    Result := 'ActionTimeout is out of range.';
    Exit;
  end;

  Temp_ImageSource := StrToIntDef(AListOfFindControlOptionsParams.Values['ImageSource'], Ord(isScreenshot));
  if (Temp_ImageSource < 0) or (Temp_ImageSource > Ord(High(TImageSource))) then
  begin
    Result := 'ImageSource is out of range.';

    if Assigned(AOnAddToLog) then
      AOnAddToLog('ImageSource is out of range.  ImageSource = ' + IntToStr(Temp_ImageSource));

    Exit;
  end;

  Temp_ImageSourceFileNameLocation := StrToIntDef(AListOfFindControlOptionsParams.Values['ImageSourceFileNameLocation'], Ord(isflMem));
  if (Temp_ImageSourceFileNameLocation < 0) or (Temp_ImageSourceFileNameLocation > Ord(High(TImageSourceFileNameLocation))) then
  begin
    Result := 'ImageSourceFileNameLocation is out of range.';

    if Assigned(AOnAddToLog) then
      AOnAddToLog('ImageSource is out of range.  ImageSourceFileNameLocation = ' + IntToStr(Temp_ImageSourceFileNameLocation));

    Exit;
  end;

  AFindControlOptions.MatchCriteria.SearchForControlMode := TSearchForControlMode(Temp_SearchForControlMode);
  AFindControlOptions.MatchCriteria.WillMatchText := AListOfFindControlOptionsParams.Values['MatchCriteria.WillMatchText'] = '1';
  AFindControlOptions.MatchCriteria.WillMatchClassName := AListOfFindControlOptionsParams.Values['MatchCriteria.WillMatchClassName'] = '1';
  AFindControlOptions.MatchCriteria.WillMatchBitmapText := AListOfFindControlOptionsParams.Values['MatchCriteria.WillMatchBitmapText'] = '1';
  AFindControlOptions.MatchCriteria.WillMatchBitmapFiles := AListOfFindControlOptionsParams.Values['MatchCriteria.WillMatchBitmapFiles'] = '1';
  AFindControlOptions.MatchCriteria.WillMatchPrimitiveFiles := AListOfFindControlOptionsParams.Values['MatchCriteria.WillMatchPrimitiveFiles'] = '1';

  AFindControlOptions.AllowToFail := AListOfFindControlOptionsParams.Values['AllowToFail'] = '1';
  AFindControlOptions.MatchText := AListOfFindControlOptionsParams.Values['MatchText'];
  AFindControlOptions.MatchClassName := AListOfFindControlOptionsParams.Values['MatchClassName'];
  AFindControlOptions.MatchTextSeparator := AListOfFindControlOptionsParams.Values['MatchTextSeparator'];
  AFindControlOptions.MatchClassNameSeparator := AListOfFindControlOptionsParams.Values['MatchClassNameSeparator'];
  SetLength(AFindControlOptions.MatchBitmapText, Temp_MatchBitmapTextCount);

  for i := 0 to Temp_MatchBitmapTextCount - 1 do
  begin
    Prefix := 'MatchBitmapText[' + IntToStr(i) + '].';

    Temp_FontSize := StrToIntDef(AListOfFindControlOptionsParams.Values[Prefix + 'FontSize'], 8);
    if (Temp_FontSize < 2) or (Temp_FontSize > 200) then
    begin
      Result := Prefix + 'FontSize is out of range.';
      Exit;
    end;

    Temp_FontQuality := StrToIntDef(AListOfFindControlOptionsParams.Values[Prefix + 'FontQuality'], 0);
    if (Temp_FontQuality < 0) or (Temp_FontQuality > Ord(High(TFontQuality))) then
    begin
      Result := Prefix + 'FontQuality is out of range.';
      Exit;
    end;

    Temp_CropLeft := AListOfFindControlOptionsParams.Values[Prefix + 'CropLeft'];
    Temp_CropTop := AListOfFindControlOptionsParams.Values[Prefix + 'CropTop'];
    Temp_CropRight := AListOfFindControlOptionsParams.Values[Prefix + 'CropRight'];
    Temp_CropBottom := AListOfFindControlOptionsParams.Values[Prefix + 'CropBottom'];

    if StrToIntDef(Temp_CropLeft, 0) < 0 then
    begin
      Result := Prefix + 'CropLeft is out of range.';
      Exit;
    end;

    if StrToIntDef(Temp_CropTop, 0) < 0 then
    begin
      Result := Prefix + 'CropTop is out of range.';
      Exit;
    end;

    if StrToIntDef(Temp_CropRight, 0) < 0 then
    begin
      Result := Prefix + 'CropRight is out of range.';
      Exit;
    end;

    if StrToIntDef(Temp_CropBottom, 0) < 0 then
    begin
      Result := Prefix + 'CropBottom is out of range.';
      Exit;
    end;

    AFindControlOptions.MatchBitmapText[i].ForegroundColor := AListOfFindControlOptionsParams.Values[Prefix + 'ForegroundColor'];
    AFindControlOptions.MatchBitmapText[i].BackgroundColor := AListOfFindControlOptionsParams.Values[Prefix + 'BackgroundColor'];
    AFindControlOptions.MatchBitmapText[i].FontName := AListOfFindControlOptionsParams.Values[Prefix + 'FontName'];
    AFindControlOptions.MatchBitmapText[i].FontSize := Temp_FontSize;
    AFindControlOptions.MatchBitmapText[i].Bold := AListOfFindControlOptionsParams.Values[Prefix + 'Bold'] = '1';
    AFindControlOptions.MatchBitmapText[i].Italic := AListOfFindControlOptionsParams.Values[Prefix + 'Italic'] = '1';
    AFindControlOptions.MatchBitmapText[i].Underline := AListOfFindControlOptionsParams.Values[Prefix + 'Underline'] = '1';
    AFindControlOptions.MatchBitmapText[i].StrikeOut := AListOfFindControlOptionsParams.Values[Prefix + 'StrikeOut'] = '1';
    AFindControlOptions.MatchBitmapText[i].FontQuality := TFontQuality(Temp_FontQuality);
    AFindControlOptions.MatchBitmapText[i].FontQualityUsesReplacement := AListOfFindControlOptionsParams.Values[Prefix + 'FontQualityUsesReplacement'] = '1';
    AFindControlOptions.MatchBitmapText[i].FontQualityReplacement := AListOfFindControlOptionsParams.Values[Prefix + 'FontQualityReplacement'];
    AFindControlOptions.MatchBitmapText[i].ProfileName := AListOfFindControlOptionsParams.Values[Prefix + 'ProfileName'];
    AFindControlOptions.MatchBitmapText[i].CropLeft := Temp_CropLeft;
    AFindControlOptions.MatchBitmapText[i].CropTop := Temp_CropTop;
    AFindControlOptions.MatchBitmapText[i].CropRight := Temp_CropRight;
    AFindControlOptions.MatchBitmapText[i].CropBottom := Temp_CropBottom;
    AFindControlOptions.MatchBitmapText[i].IgnoreBackgroundColor := AListOfFindControlOptionsParams.Values[Prefix + 'IgnoreBackgroundColor'] = '1';
  end;

  AFindControlOptions.MatchBitmapFiles := FastReplace_45ToReturn(AListOfFindControlOptionsParams.Values['MatchBitmapFiles']); //ListOfStrings
  AFindControlOptions.MatchBitmapAlgorithm := TMatchBitmapAlgorithm(Temp_MatchBitmapAlgorithm);

  AFindControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf := StrToIntDef(AListOfFindControlOptionsParams.Values['MatchBitmapAlgorithmSettings.XMultipleOf'], 0);
  AFindControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf := StrToIntDef(AListOfFindControlOptionsParams.Values['MatchBitmapAlgorithmSettings.YMultipleOf'], 0);
  AFindControlOptions.MatchBitmapAlgorithmSettings.XOffset := StrToIntDef(AListOfFindControlOptionsParams.Values['MatchBitmapAlgorithmSettings.XOffset'], 0);
  AFindControlOptions.MatchBitmapAlgorithmSettings.YOffset := StrToIntDef(AListOfFindControlOptionsParams.Values['MatchBitmapAlgorithmSettings.YOffset'], 0);

  AFindControlOptions.InitialRectangle.Left := AListOfFindControlOptionsParams.Values['InitialRectangle.Left'];
  AFindControlOptions.InitialRectangle.Top := AListOfFindControlOptionsParams.Values['InitialRectangle.Top'];
  AFindControlOptions.InitialRectangle.Right := AListOfFindControlOptionsParams.Values['InitialRectangle.Right'];
  AFindControlOptions.InitialRectangle.Bottom := AListOfFindControlOptionsParams.Values['InitialRectangle.Bottom'];
  AFindControlOptions.InitialRectangle.LeftOffset := AListOfFindControlOptionsParams.Values['InitialRectangle.LeftOffset'];
  AFindControlOptions.InitialRectangle.TopOffset := AListOfFindControlOptionsParams.Values['InitialRectangle.TopOffset'];
  AFindControlOptions.InitialRectangle.RightOffset := AListOfFindControlOptionsParams.Values['InitialRectangle.RightOffset'];
  AFindControlOptions.InitialRectangle.BottomOffset := AListOfFindControlOptionsParams.Values['InitialRectangle.BottomOffset'];

  AFindControlOptions.UseWholeScreen := AListOfFindControlOptionsParams.Values['UseWholeScreen'] = '1';
  AFindControlOptions.ColorError := AListOfFindControlOptionsParams.Values['ColorError'];  //string, to allow var replacements
  AFindControlOptions.AllowedColorErrorCount := AListOfFindControlOptionsParams.Values['AllowedColorErrorCount'];  //Number of pixels allowed to mismatch
  AFindControlOptions.WaitForControlToGoAway := AListOfFindControlOptionsParams.Values['WaitForControlToGoAway'] = '1';
  AFindControlOptions.StartSearchingWithCachedControl := AListOfFindControlOptionsParams.Values['StartSearchingWithCachedControl'] = '1';
  AFindControlOptions.CachedControlLeft := AListOfFindControlOptionsParams.Values['CachedControlLeft'];
  AFindControlOptions.CachedControlTop := AListOfFindControlOptionsParams.Values['CachedControlTop'];

  AFindControlOptions.MatchPrimitiveFiles := FastReplace_45ToReturn(AListOfFindControlOptionsParams.Values['MatchPrimitiveFiles']); //ListOfStrings
  AFindControlOptions.GetAllControls := AListOfFindControlOptionsParams.Values['GetAllControls'] = '1';

  AFindControlOptions.UseFastSearch := AListOfFindControlOptionsParams.Values['UseFastSearch'] <> '0';
  AFindControlOptions.FastSearchAllowedColorErrorCount := AListOfFindControlOptionsParams.Values['FastSearchAllowedColorErrorCount'];
  AFindControlOptions.IgnoredColors := AListOfFindControlOptionsParams.Values['IgnoredColors'];
  AFindControlOptions.SleepySearch := AListOfFindControlOptionsParams.Values['SleepySearch'] = '1';
  AFindControlOptions.StopSearchOnMismatch := AListOfFindControlOptionsParams.Values['StopSearchOnMismatch'] <> '0';

  AFindControlOptions.ImageSource := TImageSource(Temp_ImageSource);
  AFindControlOptions.SourceFileName := AListOfFindControlOptionsParams.Values['SourceFileName'];
  AFindControlOptions.ImageSourceFileNameLocation := TImageSourceFileNameLocation(Temp_ImageSourceFileNameLocation);

  AActionOptions.ActionName := AListOfFindControlOptionsParams.Values['ActionName'];
  AActionOptions.ActionTimeout := Temp_ActionTimeout;
  AActionOptions.Action := CActionType[AIsSubControl];
end;


function SetSetControlTextActionProperties(AListOfSetControlTextOptionsParams: TStrings; out ASetTextOptions: TClkSetTextOptions): string;
var
  Temp_ControlType: Integer;
begin
  Result := '';

  Temp_ControlType := StrToIntDef(AListOfSetControlTextOptionsParams.Values['ControlType'], 0);
  if (Temp_ControlType < 0) or (Temp_ControlType > Ord(High(TClkSetTextControlType))) then
  begin
    Result := 'ControlType is out of range.';
    Exit;
  end;

  ASetTextOptions.Text := AListOfSetControlTextOptionsParams.Values['Text'];
  ASetTextOptions.ControlType := TClkSetTextControlType(Temp_ControlType);
  ASetTextOptions.DelayBetweenKeyStrokes := AListOfSetControlTextOptionsParams.Values['DelayBetweenKeyStrokes'];
  ASetTextOptions.Count := AListOfSetControlTextOptionsParams.Values['Count'];
end;


function SetCallTemplateActionProperties(AListOfCallTemplateOptionsParams: TStrings; out ACallTemplateOptions: TClkCallTemplateOptions): string;
var
  Temp_LoopDirection: Integer;
  Temp_LoopEvalBreakPosition: Integer;
begin
  Result := '';

  Temp_LoopDirection := StrToIntDef(AListOfCallTemplateOptionsParams.Values['Loop.Direction'], 0);
  if (Temp_LoopDirection < 0) or (Temp_LoopDirection > Ord(High(TLoopDirection))) then
  begin
    Result := 'Loop.Direction is out of range.';
    Exit;
  end;

  Temp_LoopEvalBreakPosition := StrToIntDef(AListOfCallTemplateOptionsParams.Values['Loop.EvalBreakPosition'], 0);
  if (Temp_LoopEvalBreakPosition < 0) or (Temp_LoopEvalBreakPosition > Ord(High(TLoopEvalBreakPosition))) then
  begin
    Result := 'Loop.EvalBreakPosition is out of range.';
    Exit;
  end;

  ACallTemplateOptions.TemplateFileName := AListOfCallTemplateOptionsParams.Values['TemplateFileName'];
  ACallTemplateOptions.ListOfCustomVarsAndValues := AListOfCallTemplateOptionsParams.Values['ListOfCustomVarsAndValues'];
  ACallTemplateOptions.CallOnlyIfCondition := False;       //deprecated - must be set to False, to prevent error messages
  ACallTemplateOptions.CallOnlyIfConditionVarName := '';   //deprecated
  ACallTemplateOptions.CallOnlyIfConditionVarValue := '';  //deprecated
  ACallTemplateOptions.EvaluateBeforeCalling := AListOfCallTemplateOptionsParams.Values['EvaluateBeforeCalling'] = '1';

  ACallTemplateOptions.CallTemplateLoop.Enabled := AListOfCallTemplateOptionsParams.Values['Loop.Enabled'] = '1'; //When False, the CallTemplate action is executed once, as before. Else, it may be executed or not, based on loop settings.
  ACallTemplateOptions.CallTemplateLoop.Counter := AListOfCallTemplateOptionsParams.Values['Loop.Counter'];
  ACallTemplateOptions.CallTemplateLoop.InitValue := AListOfCallTemplateOptionsParams.Values['Loop.InitValue'];
  ACallTemplateOptions.CallTemplateLoop.EndValue := AListOfCallTemplateOptionsParams.Values['Loop.EndValue'];
  ACallTemplateOptions.CallTemplateLoop.Direction := TLoopDirection(Temp_LoopDirection);
  ACallTemplateOptions.CallTemplateLoop.BreakCondition := FastReplace_45ToReturn(AListOfCallTemplateOptionsParams.Values['Loop.BreakCondition']); //uses the same format as TClkActionOptions.ActionCondition
  ACallTemplateOptions.CallTemplateLoop.EvalBreakPosition := TLoopEvalBreakPosition(Temp_LoopEvalBreakPosition);
end;


function SetSleepActionProperties(AListOfSleepOptionsParams: TStrings; out ASleepOptions: TClkSleepOptions; out AActionOptions: TClkActionOptions): string;
begin
  Result := '';

  ASleepOptions.Value := AListOfSleepOptionsParams.Values['Value'];

  AActionOptions.ActionName := AListOfSleepOptionsParams.Values['ActionName'];
  AActionOptions.ActionTimeout := 0;
  AActionOptions.Action := acSleep;
end;


function SetSetVarActionProperties(AListOfSetVarOptionsParams: TStrings; out ASetVarOptions: TClkSetVarOptions): string;
begin
  Result := '';

  ASetVarOptions.ListOfVarNames := FastReplace_45ToReturn(AListOfSetVarOptionsParams.Values['ListOfVarNames']);
  ASetVarOptions.ListOfVarValues := FastReplace_45ToReturn(AListOfSetVarOptionsParams.Values['ListOfVarValues']);
  ASetVarOptions.ListOfVarEvalBefore := FastReplace_45ToReturn(AListOfSetVarOptionsParams.Values['ListOfVarEvalBefore']);
end;


function SetWindowOperationsActionProperties(AListOfWindowOperationsOptionsParams: TStrings; out AWindowOperationsOptions: TClkWindowOperationsOptions): string;
var
  Temp_Operation: Integer;
begin
  Result := '';

  Temp_Operation := StrToIntDef(AListOfWindowOperationsOptionsParams.Values['Operation'], 0);
  if (Temp_Operation < 0) or (Temp_Operation > Ord(High(TWindowOperation))) then
  begin
    Result := 'Operation is out of range.';
    Exit;
  end;

  AWindowOperationsOptions.Operation := TWindowOperation(Temp_Operation);
  AWindowOperationsOptions.NewX := AListOfWindowOperationsOptionsParams.Values['NewX'];
  AWindowOperationsOptions.NewY := AListOfWindowOperationsOptionsParams.Values['NewY'];
  AWindowOperationsOptions.NewWidth := AListOfWindowOperationsOptionsParams.Values['NewWidth'];
  AWindowOperationsOptions.NewHeight := AListOfWindowOperationsOptionsParams.Values['NewHeight'];
  AWindowOperationsOptions.NewPositionEnabled := AListOfWindowOperationsOptionsParams.Values['NewPositionEnabled'] = '1';
  AWindowOperationsOptions.NewSizeEnabled := AListOfWindowOperationsOptionsParams.Values['NewSizeEnabled'] = '1';
end;


function SetLoadSetVarFromFileActionProperties(AListOfLoadSetVarOptionsParams: TStrings; out ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions): string;
begin
  Result := '';

  ALoadSetVarFromFileOptions.FileName := AListOfLoadSetVarOptionsParams.Values['FileName'];
  ALoadSetVarFromFileOptions.SetVarActionName := AListOfLoadSetVarOptionsParams.Values['SetVarActionName'];
end;


function SetSaveSetVarToFileActionProperties(AListOfSaveSetVarOptionsParams: TStrings; out ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions): string;
begin
  Result := '';

  ASaveSetVarToFileOptions.FileName := AListOfSaveSetVarOptionsParams.Values['FileName'];
  ASaveSetVarToFileOptions.SetVarActionName := AListOfSaveSetVarOptionsParams.Values['SetVarActionName'];
end;


function SetPluginActionProperties(APluginOptionsParams: TStrings; out APluginOptions: TClkPluginOptions): string;
begin
  Result := '';

  APluginOptions.FileName := APluginOptionsParams.Values['FileName'];
  APluginOptions.ListOfPropertiesAndValues := FastReplace_45ToReturn(APluginOptionsParams.Values['ListOfPropertiesAndValues']);
end;

end.

