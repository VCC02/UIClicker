{
    Copyright (C) 2025 VCC
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


const
  CPropertyName_ActionName = 'ActionName';
  CPropertyName_ActionTimeout = 'ActionTimeout';


function GetClickActionProperties(AClickOptions: TClkClickOptions): string;
function GetExecAppActionProperties(AExecAppOptions: TClkExecAppOptions): string;
function GetFindControlActionProperties(AFindControlOptions: TClkFindControlOptions): string;
function GetFindSubControlActionProperties(AFindSubControlOptions: TClkFindSubControlOptions): string;
function GetSetControlTextActionProperties(ASetTextOptions: TClkSetTextOptions): string;
function GetCallTemplateActionProperties(ACallTemplateOptions: TClkCallTemplateOptions): string;
function GetSleepActionProperties(ASleepOptions: TClkSleepOptions): string;
function GetSetVarActionProperties(ASetVarOptions: TClkSetVarOptions): string;
function GetWindowOperationsActionProperties(AWindowOperationsOptions: TClkWindowOperationsOptions): string;
function GetLoadSetVarFromFileActionProperties(ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions): string;
function GetSaveSetVarToFileActionProperties(ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions): string;
function GetPluginActionProperties(PluginOptions: TClkPluginOptions): string;
function GetEditTemplateActionProperties(AEditTemplateOptions: TClkEditTemplateOptions; AIncludeListOfEditedProperties: Boolean = False): string;
//when adding new Get<ActionType>Properties functions, please update DoOnActionPlugin_GetActionContentByIndex_Callback from ClickerActionPluginLoader.pas

function GetActionPropertiesByType(var AAction: TClkActionRec; AIncludeSpecialProperties: Boolean = False): string;
function GetDifferentThanDefaultActionPropertiesByType(var AAction: TClkActionRec; AIncludeSpecialProperties: Boolean = False): string;

function GetActionPropertyDataTypesByType(var AAction: TClkActionRec; AIncludeSpecialProperties: Boolean = False): string;
function GetDifferentThanDefaultActionPropertyDataTypesByType(var AAction: TClkActionRec; AIncludeSpecialProperties: Boolean = False): string;

//The Set<ActionType>Properties functions return an error if any, or emptry string for success.
function SetClickActionProperties(AListOfClickOptionsParams: TStrings; out AClickOptions: TClkClickOptions): string;
function SetExecAppActionProperties(AListOfExecAppOptionsParams: TStrings; out AExecAppOptions: TClkExecAppOptions; out AActionOptions: TClkActionOptions): string;
function SetFindControlActionProperties(AListOfFindControlOptionsParams: TStrings; AOnAddToLog: TOnAddToLog; out AFindControlOptions: TClkFindControlOptions; out AActionOptions: TClkActionOptions): string; //AOnAddToLog can be set to nil if not used.
function SetFindSubControlActionProperties(AListOfFindSubControlOptionsParams: TStrings; AOnAddToLog: TOnAddToLog; out AFindSubControlOptions: TClkFindSubControlOptions; out AActionOptions: TClkActionOptions): string; //AOnAddToLog can be set to nil if not used.
function SetSetControlTextActionProperties(AListOfSetControlTextOptionsParams: TStrings; out ASetTextOptions: TClkSetTextOptions): string;
function SetCallTemplateActionProperties(AListOfCallTemplateOptionsParams: TStrings; out ACallTemplateOptions: TClkCallTemplateOptions): string;
function SetSleepActionProperties(AListOfSleepOptionsParams: TStrings; out ASleepOptions: TClkSleepOptions; out AActionOptions: TClkActionOptions): string;
function SetSetVarActionProperties(AListOfSetVarOptionsParams: TStrings; out ASetVarOptions: TClkSetVarOptions): string;
function SetWindowOperationsActionProperties(AListOfWindowOperationsOptionsParams: TStrings; out AWindowOperationsOptions: TClkWindowOperationsOptions): string;
function SetLoadSetVarFromFileActionProperties(AListOfLoadSetVarOptionsParams: TStrings; out ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions): string;
function SetSaveSetVarToFileActionProperties(AListOfSaveSetVarOptionsParams: TStrings; out ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions): string;
function SetPluginActionProperties(APluginOptionsParams: TStrings; out APluginOptions: TClkPluginOptions): string;
function SetEditTemplateActionProperties(AListOfEditTemplateOptionsParams: TStrings; out AEditTemplateOptions: TClkEditTemplateOptions; AIncludeListOfEditedProperties: Boolean = False): string;

function SetActionProperties(AListOfOptionsParams: TStrings; AActionType: TClkAction; var AActionOptions: TClkActionRec): string; overload; //it outputs only the options, without the action metadata
function SetActionProperties(AListOfOptionsParams: string; AActionType: TClkAction; var AActionOptions: TClkActionRec): string; overload; //it outputs only the options, without the action metadata

procedure GetDefaultPropertyValues_Click(var AClickOptions: TClkClickOptions);
procedure GetDefaultPropertyValues_ExecApp(var AExecAppOptions: TClkExecAppOptions);
procedure GetDefaultPropertyValues_FindControl(var AFindControlOptions: TClkFindControlOptions);
procedure GetDefaultPropertyValues_FindSubControl(var AFindSubControlOptions: TClkFindSubControlOptions; AFindSubControlProfilesCount: Integer = 1);
procedure GetDefaultPropertyValues_FindControl_MatchBitmapText(var AMatchBitmapText: TClkFindControlMatchBitmapText);
procedure GetDefaultPropertyValues_SetControlText(var ASetControlTextOptions: TClkSetTextOptions);
procedure GetDefaultPropertyValues_CallTemplate(var ACallTemplateOptions: TClkCallTemplateOptions);
procedure GetDefaultPropertyValues_Sleep(var ASleepOptions: TClkSleepOptions);
procedure GetDefaultPropertyValues_SetVar(var ASetVarOptions: TClkSetVarOptions);
procedure GetDefaultPropertyValues_WindowOperations(var AWindowOperationsOptions: TClkWindowOperationsOptions);
procedure GetDefaultPropertyValues_LoadSetVarFromFile(var ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions);
procedure GetDefaultPropertyValues_SaveSetVarToFile(var ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions);
procedure GetDefaultPropertyValues_Plugin(var APluginOptions: TClkPluginOptions);
procedure GetDefaultPropertyValues_EditTemplate(var AEditTemplateOptions: TClkEditTemplateOptions);
//if adding new action types, please update ClickerActionsFrame and ExecuteEditTemplateAction, which call all of the above

procedure GetDefaultPropertyValuesByType(AActionType: TClkAction; var AAction: TClkActionRec; AFindSubControlProfilesCount: Integer = 1);

//These actions return True if their string properties, which are not '' by default, are now set to ''.
//This will happen after serializing properties from ''.
function IsActionEmpty_Click(var AClickOptions: TClkClickOptions): Boolean;
function IsActionEmpty_ExecApp(var AExecAppOptions: TClkExecAppOptions): Boolean;
function IsActionEmpty_FindControl(var AFindControlOptions: TClkFindControlOptions): Boolean;
function IsActionEmpty_FindSubControl(var AFindSubControlOptions: TClkFindSubControlOptions): Boolean;
function IsActionEmpty_SetControlText(var ASetControlTextOptions: TClkSetTextOptions): Boolean;
function IsActionEmpty_CallTemplate(var ACallTemplateOptions: TClkCallTemplateOptions): Boolean;
function IsActionEmpty_Sleep(var ASleepOptions: TClkSleepOptions): Boolean;
function IsActionEmpty_SetVar(var ASetVarOptions: TClkSetVarOptions): Boolean;
function IsActionEmpty_WindowOperations(var AWindowOperationsOptions: TClkWindowOperationsOptions): Boolean;
function IsActionEmpty_LoadSetVarFromFile(var ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions): Boolean;
function IsActionEmpty_SaveSetVarToFile(var ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions): Boolean;
function IsActionEmpty_Plugin(var APluginOptions: TClkPluginOptions): Boolean;
function IsActionEmpty_EditTemplate(var AEditTemplateOptions: TClkEditTemplateOptions): Boolean;
//if adding new action types, please update ClickerActionsFrame, which calls all of the above

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
            'MoveDuration' + '=' + AClickOptions.MoveDuration + '&' +
            'UseClipCursor' + '=' + IntToStr(Ord(AClickOptions.UseClipCursor));
end;


function GetExecAppActionProperties(AExecAppOptions: TClkExecAppOptions): string;
begin
  Result := 'PathToApp' + '=' + AExecAppOptions.PathToApp + '&' +
            'ListOfParams' + '=' + FastReplace_ReturnTo45(AExecAppOptions.ListOfParams) + '&' +
            'WaitForApp' + '=' + IntToStr(Ord(AExecAppOptions.WaitForApp)) + '&' +
            'AppStdIn' + '=' + AExecAppOptions.AppStdIn + '&' +
            'CurrentDir' + '=' + AExecAppOptions.CurrentDir + '&' +
            'UseInheritHandles' + '=' + IntToStr(Ord(AExecAppOptions.UseInheritHandles)) + '&' +
            'NoConsole' + '=' + IntToStr(Ord(AExecAppOptions.NoConsole)) + '&' +
            'VerifyFileExistence' + '=' + IntToStr(Ord(AExecAppOptions.VerifyFileExistence));
end;


function GetFindControlActionProperties(AFindControlOptions: TClkFindControlOptions): string;
begin
  Result := 'MatchCriteria.SearchForControlMode' + '=' + IntToStr(Ord(AFindControlOptions.MatchCriteria.SearchForControlMode)) + '&' +
            'MatchCriteria.WillMatchText' + '=' + IntToStr(Ord(AFindControlOptions.MatchCriteria.WillMatchText)) + '&' +
            'MatchCriteria.WillMatchClassName' + '=' + IntToStr(Ord(AFindControlOptions.MatchCriteria.WillMatchClassName)) + '&' +
            'AllowToFail' + '=' + IntToStr(Ord(AFindControlOptions.AllowToFail)) + '&' +

            'MatchText' + '=' + AFindControlOptions.MatchText + '&' +
            'MatchClassName' + '=' + AFindControlOptions.MatchClassName + '&' +
            'MatchTextSeparator' + '=' + AFindControlOptions.MatchTextSeparator + '&' +
            'MatchClassNameSeparator' + '=' + AFindControlOptions.MatchClassNameSeparator + '&' +

            'InitialRectangle.Left' + '=' + AFindControlOptions.InitialRectangle.Left + '&' +
            'InitialRectangle.Top' + '=' + AFindControlOptions.InitialRectangle.Top + '&' +
            'InitialRectangle.Right' + '=' + AFindControlOptions.InitialRectangle.Right + '&' +
            'InitialRectangle.Bottom' + '=' + AFindControlOptions.InitialRectangle.Bottom + '&' +
            'InitialRectangle.LeftOffset' + '=' + AFindControlOptions.InitialRectangle.LeftOffset + '&' +
            'InitialRectangle.TopOffset' + '=' + AFindControlOptions.InitialRectangle.TopOffset + '&' +
            'InitialRectangle.RightOffset' + '=' + AFindControlOptions.InitialRectangle.RightOffset + '&' +
            'InitialRectangle.BottomOffset' + '=' + AFindControlOptions.InitialRectangle.BottomOffset + '&' +
            'UseWholeScreen' + '=' + IntToStr(Ord(AFindControlOptions.UseWholeScreen)) + '&' +
            'WaitForControlToGoAway' + '=' + IntToStr(Ord(AFindControlOptions.WaitForControlToGoAway)) + '&' +
            'StartSearchingWithCachedControl' + '=' + IntToStr(Ord(AFindControlOptions.StartSearchingWithCachedControl)) + '&' +
            'CachedControlLeft' + '=' + AFindControlOptions.CachedControlLeft + '&' +
            'CachedControlTop' + '=' + AFindControlOptions.CachedControlTop + '&' +
            'GetAllControls' + '=' + IntToStr(Ord(AFindControlOptions.GetAllControls)) + '&' +

            'PrecisionTimeout' + '=' + IntToStr(Ord(AFindControlOptions.PrecisionTimeout)) + '&' +
            'EvaluateTextCount' + '=' + AFindControlOptions.EvaluateTextCount //+ '&' +
            ;
end;


function GetFindSubControlActionProperties(AFindSubControlOptions: TClkFindSubControlOptions): string;
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
  Result := 'MatchCriteria.WillMatchBitmapText' + '=' + IntToStr(Ord(AFindSubControlOptions.MatchCriteria.WillMatchBitmapText)) + '&' +
            'MatchCriteria.WillMatchBitmapFiles' + '=' + IntToStr(Ord(AFindSubControlOptions.MatchCriteria.WillMatchBitmapFiles)) + '&' +
            'MatchCriteria.WillMatchPrimitiveFiles' + '=' + IntToStr(Ord(AFindSubControlOptions.MatchCriteria.WillMatchPrimitiveFiles)) + '&' +
            'AllowToFail' + '=' + IntToStr(Ord(AFindSubControlOptions.AllowToFail)) + '&' +

            'MatchText' + '=' + AFindSubControlOptions.MatchText + '&' +

            'MatchBitmapText.Count' + '=' + IntToStr(Length(AFindSubControlOptions.MatchBitmapText)) + '&' +
            GetMatchBitmapTextContent(AFindSubControlOptions.MatchBitmapText) +
            'MatchBitmapFiles' + '=' + FastReplace_ReturnTo45(AFindSubControlOptions.MatchBitmapFiles) + '&' +
            'MatchBitmapAlgorithm' + '=' + IntToStr(Ord(AFindSubControlOptions.MatchBitmapAlgorithm)) + '&' +
            'MatchBitmapAlgorithmSettings.XMultipleOf' + '=' + IntToStr(AFindSubControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf) + '&' +
            'MatchBitmapAlgorithmSettings.YMultipleOf' + '=' + IntToStr(AFindSubControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf) + '&' +
            'MatchBitmapAlgorithmSettings.XOffset' + '=' + IntToStr(AFindSubControlOptions.MatchBitmapAlgorithmSettings.XOffset) + '&' +
            'MatchBitmapAlgorithmSettings.YOffset' + '=' + IntToStr(AFindSubControlOptions.MatchBitmapAlgorithmSettings.YOffset) + '&' +
            'InitialRectangle.Left' + '=' + AFindSubControlOptions.InitialRectangle.Left + '&' +
            'InitialRectangle.Top' + '=' + AFindSubControlOptions.InitialRectangle.Top + '&' +
            'InitialRectangle.Right' + '=' + AFindSubControlOptions.InitialRectangle.Right + '&' +
            'InitialRectangle.Bottom' + '=' + AFindSubControlOptions.InitialRectangle.Bottom + '&' +
            'InitialRectangle.LeftOffset' + '=' + AFindSubControlOptions.InitialRectangle.LeftOffset + '&' +
            'InitialRectangle.TopOffset' + '=' + AFindSubControlOptions.InitialRectangle.TopOffset + '&' +
            'InitialRectangle.RightOffset' + '=' + AFindSubControlOptions.InitialRectangle.RightOffset + '&' +
            'InitialRectangle.BottomOffset' + '=' + AFindSubControlOptions.InitialRectangle.BottomOffset + '&' +
            'UseWholeScreen' + '=' + IntToStr(Ord(AFindSubControlOptions.UseWholeScreen)) + '&' +
            'ColorError' + '=' + AFindSubControlOptions.ColorError + '&' +
            'AllowedColorErrorCount' + '=' + AFindSubControlOptions.AllowedColorErrorCount + '&' +
            'WaitForControlToGoAway' + '=' + IntToStr(Ord(AFindSubControlOptions.WaitForControlToGoAway)) + '&' +
            'StartSearchingWithCachedControl' + '=' + IntToStr(Ord(AFindSubControlOptions.StartSearchingWithCachedControl)) + '&' +
            'CachedControlLeft' + '=' + AFindSubControlOptions.CachedControlLeft + '&' +
            'CachedControlTop' + '=' + AFindSubControlOptions.CachedControlTop + '&' +
            'MatchPrimitiveFiles' + '=' + FastReplace_ReturnTo45(AFindSubControlOptions.MatchPrimitiveFiles) + '&' +
            'GetAllControls' + '=' + IntToStr(Ord(AFindSubControlOptions.GetAllControls)) + '&' +
            'UseFastSearch' + '=' + IntToStr(Ord(AFindSubControlOptions.UseFastSearch)) + '&' +
            'FastSearchAllowedColorErrorCount' + '=' + AFindSubControlOptions.FastSearchAllowedColorErrorCount + '&' +
            'IgnoredColors' + '=' + AFindSubControlOptions.IgnoredColors + '&' +
            'SleepySearch' + '=' + IntToStr(Ord(AFindSubControlOptions.SleepySearch)) + '&' +
            'StopSearchOnMismatch' + '=' + IntToStr(Ord(AFindSubControlOptions.StopSearchOnMismatch)) + '&' +
            'ImageSource' + '=' + IntToStr(Ord(AFindSubControlOptions.ImageSource)) + '&' +
            'SourceFileName' + '=' + AFindSubControlOptions.SourceFileName + '&' +
            'ImageSourceFileNameLocation' + '=' + IntToStr(Ord(AFindSubControlOptions.ImageSourceFileNameLocation)) + '&' +
            'PrecisionTimeout' + '=' + IntToStr(Ord(AFindSubControlOptions.PrecisionTimeout)) + '&' +
            'FullBackgroundImageInResult' + '=' + IntToStr(Ord(AFindSubControlOptions.FullBackgroundImageInResult)) + '&' +

            'MatchByHistogramSettings.MinPercentColorMatch' + '=' + AFindSubControlOptions.MatchByHistogramSettings.MinPercentColorMatch + '&' +
            'MatchByHistogramSettings.MostSignificantColorCountInSubBmp' + '=' + AFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInSubBmp + '&' +
            'MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp' + '=' + AFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp + '&' +

            'EvaluateTextCount' + '=' + AFindSubControlOptions.EvaluateTextCount + '&' +
            'CropFromScreenshot' + '=' + IntToStr(Ord(AFindSubControlOptions.CropFromScreenshot)) + '&' +
            'ThreadCount' + '=' + AFindSubControlOptions.ThreadCount + '&' +
            'UseTextRenderingInBrowser' + '=' + IntToStr(Ord(AFindSubControlOptions.UseTextRenderingInBrowser))
            ;
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
            'ListOfVarEvalBefore' + '=' + FastReplace_ReturnTo45(ASetVarOptions.ListOfVarEvalBefore) + '&' +
            'FailOnException' + '=' + IntToStr(Ord(ASetVarOptions.FailOnException));
end;


function GetWindowOperationsActionProperties(AWindowOperationsOptions: TClkWindowOperationsOptions): string;
begin
  Result := 'Operation' + '=' + IntToStr(Ord(AWindowOperationsOptions.Operation)) + '&' +
            'NewX' + '=' + AWindowOperationsOptions.NewX + '&' +
            'NewY' + '=' + AWindowOperationsOptions.NewY + '&' +
            'NewWidth' + '=' + AWindowOperationsOptions.NewWidth + '&' +
            'NewHeight' + '=' + AWindowOperationsOptions.NewHeight + '&' +
            'NewPositionEnabled' + '=' + IntToStr(Ord(AWindowOperationsOptions.NewPositionEnabled)) + '&' +
            'NewSizeEnabled' + '=' + IntToStr(Ord(AWindowOperationsOptions.NewSizeEnabled));
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


function GetEditTemplateActionProperties(AEditTemplateOptions: TClkEditTemplateOptions; AIncludeListOfEditedProperties: Boolean = False): string;
begin
  Result := 'Operation' + '=' + IntToStr(Ord(AEditTemplateOptions.Operation)) + '&' +
            'WhichTemplate' + '=' + IntToStr(Ord(AEditTemplateOptions.WhichTemplate)) + '&' +
            'TemplateFileName' + '=' + AEditTemplateOptions.TemplateFileName + '&';

            if AIncludeListOfEditedProperties then  //this should be true when serializing for http
              Result := Result + 'ListOfEditedProperties' + '=' + StringReplace(FastReplace_1920To45(AEditTemplateOptions.ListOfEditedProperties), CPropSeparatorSer, CPropSeparatorInt, [rfReplaceAll]) + '&'; //ListOfEditedProperties stores a #18 separated list of key=value strings

  Result := Result +
            'ListOfEnabledProperties' + '=' + FastReplace_ReturnTo45(AEditTemplateOptions.ListOfEnabledProperties) + '&' +
            'EditedActionName' + '=' + AEditTemplateOptions.EditedActionName + '&' +
            'EditedActionType' + '=' + IntToStr(Ord(AEditTemplateOptions.EditedActionType)) + '&' +
            'EditedActionCondition' + '=' + AEditTemplateOptions.EditedActionCondition + '&' +
            'EditedActionTimeout' + '=' + IntToStr(AEditTemplateOptions.EditedActionTimeout) + '&' +
            'NewActionName' + '=' + AEditTemplateOptions.NewActionName + '&' +
            'ShouldSaveTemplate' + '=' + IntToStr(Ord(AEditTemplateOptions.ShouldSaveTemplate));
end;


function GetActionPropertiesByType(var AAction: TClkActionRec; AIncludeSpecialProperties: Boolean = False): string;
begin
  Result := '';
  case AAction.ActionOptions.Action of
    acClick: Result := GetClickActionProperties(AAction.ClickOptions);
    acExecApp: Result := GetExecAppActionProperties(AAction.ExecAppOptions);
    acFindControl:    Result := GetFindControlActionProperties(AAction.FindControlOptions);
    acFindSubControl: Result := GetFindSubControlActionProperties(AAction.FindSubControlOptions);
    acSetControlText: Result := GetSetControlTextActionProperties(AAction.SetTextOptions);
    acCallTemplate: Result := GetCallTemplateActionProperties(AAction.CallTemplateOptions);
    acSleep: Result := GetSleepActionProperties(AAction.SleepOptions);
    acSetVar: Result := GetSetVarActionProperties(AAction.SetVarOptions);
    acWindowOperations: Result := GetWindowOperationsActionProperties(AAction.WindowOperationsOptions);
    acLoadSetVarFromFile: Result := GetLoadSetVarFromFileActionProperties(AAction.LoadSetVarFromFileOptions);
    acSaveSetVarToFile: Result := GetSaveSetVarToFileActionProperties(AAction.SaveSetVarToFileOptions);
    acPlugin: Result := GetPluginActionProperties(AAction.PluginOptions);
    acEditTemplate: Result := GetEditTemplateActionProperties(AAction.EditTemplateOptions, AIncludeSpecialProperties);
  end;
end;


function GetDifferentThanDefaultActionPropertiesByType(var AAction: TClkActionRec; AIncludeSpecialProperties: Boolean = False): string;
var
  ActionWithDefaultProperties: TClkActionRec;

  DefaultProperties, AllProperties: string;
  ListOfDefaultProperties, ListOfAllProperties: TStringList;
  i: Integer;
begin
  GetDefaultPropertyValuesByType(AAction.ActionOptions.Action, ActionWithDefaultProperties, Length(AAction.FindSubControlOptions.MatchBitmapText));

  DefaultProperties := GetActionPropertiesByType(ActionWithDefaultProperties, AIncludeSpecialProperties);
  AllProperties := GetActionPropertiesByType(AAction, AIncludeSpecialProperties);

  ListOfDefaultProperties := TStringList.Create;
  ListOfAllProperties := TStringList.Create;
  try
    ListOfDefaultProperties.LineBreak := #13#10;
    ListOfAllProperties.LineBreak := #13#10;

    ListOfDefaultProperties.Text := StringReplace(DefaultProperties, '&', #13#10, [rfReplaceAll]);
    ListOfAllProperties.Text := StringReplace(AllProperties, '&', #13#10, [rfReplaceAll]);

    Result := '';
    for i := 0 to ListOfDefaultProperties.Count - 1 do
      if (ListOfAllProperties.Strings[i] <> ListOfDefaultProperties.Strings[i]) or (ListOfAllProperties.Names[i] = 'MatchBitmapText.Count') then
        Result := Result + ListOfAllProperties.Strings[i] + '&';

    if Result > '' then
      if Result[Length(Result)] = '&' then
        Delete(Result, Length(Result), 1);
  finally
    ListOfDefaultProperties.Free;
    ListOfAllProperties.Free;
  end;
end;

//////

function GetClickActionPropertyDataTypes: string;
begin
  Result := 'XClickPointReference' + '=' + CDTEnum + '.TXClickPointReference' + '&' +
            'YClickPointReference' + '=' + CDTEnum + '.TYClickPointReference' + '&' +
            'XClickPointVar' + '=' + CDTString + '&' +
            'YClickPointVar' + '=' + CDTString + '&' +
            'XOffset' + '=' + CDTString + '&' +
            'YOffset' + '=' + CDTString + '&' +
            'MouseButton' + '=' + CDTEnum + '.TMouseButton' + '&' +
            'ClickWithCtrl' + '=' + CDTBool + '&' +
            'ClickWithAlt' + '=' + CDTBool + '&' +
            'ClickWithShift' + '=' + CDTBool + '&' +
            'ClickWithDoubleClick' + '=' + CDTBool + '&' +  //Deprecated. The code is still generated with it.
            'Count' + '=' + CDTInteger + '&' +
            'LeaveMouse' + '=' + CDTBool + '&' +
            'MoveWithoutClick' + '=' + CDTBool + '&' +
            'ClickType' + '=' + CDTInteger + '&' +   //This is enum in OI only. The field is integer.
            'XClickPointReferenceDest' + '=' + CDTEnum + '.TXClickPointReference' + '&' +
            'YClickPointReferenceDest' + '=' + CDTEnum + '.TYClickPointReference' + '&' +
            'XClickPointVarDest' + '=' + CDTString + '&' +
            'YClickPointVarDest' + '=' + CDTString + '&' +
            'XOffsetDest' + '=' + CDTString + '&' +
            'YOffsetDest' + '=' + CDTString + '&' +
            'MouseWheelType' + '=' + CDTEnum + '.TMouseWheelType' + '&' +
            'MouseWheelAmount' + '=' + CDTString + '&' +
            'DelayAfterMovingToDestination' + '=' + CDTString + '&' +
            'DelayAfterMouseDown' + '=' + CDTString + '&' +
            'MoveDuration' + '=' + CDTString + '&' +
            'UseClipCursor' + '=' + CDTBool;
end;


function GetExecAppActionPropertyDataTypes: string;
begin
  Result := 'PathToApp' + '=' + CDTString + '&' +
            'ListOfParams' + '=' + CDTString + '&' +
            'WaitForApp' + '=' + CDTBool + '&' +
            'AppStdIn' + '=' + CDTString + '&' +
            'CurrentDir' + '=' + CDTString + '&' +
            'UseInheritHandles' + '=' + CDTBool + '&' +
            'NoConsole' + '=' + CDTBool + '&' +
            'VerifyFileExistence' + '=' + CDTBool;
end;


function GetFindControlActionPropertyDataTypes: string;
begin
  Result := 'MatchCriteria.SearchForControlMode' + '=' + CDTEnum + '.TSearchForControlMode' + '&' +
            'MatchCriteria.WillMatchText' + '=' + CDTBool + '&' +
            'MatchCriteria.WillMatchClassName' + '=' + CDTBool + '&' +
            'AllowToFail' + '=' + CDTBool + '&' +

            'MatchText' + '=' + CDTString + '&' +
            'MatchClassName' + '=' + CDTString + '&' +
            'MatchTextSeparator' + '=' + CDTString + '&' +
            'MatchClassNameSeparator' + '=' + CDTString + '&' +

            'InitialRectangle.Left' + '=' + CDTString + '&' +
            'InitialRectangle.Top' + '=' + CDTString + '&' +
            'InitialRectangle.Right' + '=' + CDTString + '&' +
            'InitialRectangle.Bottom' + '=' + CDTString + '&' +
            'InitialRectangle.LeftOffset' + '=' + CDTString + '&' +
            'InitialRectangle.TopOffset' + '=' + CDTString + '&' +
            'InitialRectangle.RightOffset' + '=' + CDTString + '&' +
            'InitialRectangle.BottomOffset' + '=' + CDTString + '&' +
            'UseWholeScreen' + '=' + CDTBool + '&' +
            'WaitForControlToGoAway' + '=' + CDTBool + '&' +
            'StartSearchingWithCachedControl' + '=' + CDTBool + '&' +
            'CachedControlLeft' + '=' + CDTString + '&' +
            'CachedControlTop' + '=' + CDTString + '&' +
            'GetAllControls' + '=' + CDTBool + '&' +

            'PrecisionTimeout' + '=' + CDTBool + '&' +
            'EvaluateTextCount' + '=' + CDTString //+ '&' +
            ;
end;


function GetFindSubControlActionPropertyDataTypes(AMatchBitmapTextLen: Integer): string;
  function GetMatchBitmapTextContent(AMatchBitmapTextLen: Integer): string;
  var
    i: Integer;
    Prefix: string;
  begin
    Result := '';
    for i := 0 to AMatchBitmapTextLen - 1 do
    begin
      Prefix := 'MatchBitmapText[' + IntToStr(i) + '].';
      Result := Result + Prefix + 'ForegroundColor' + '=' + CDTString + '&';
      Result := Result + Prefix + 'BackgroundColor' + '=' + CDTString + '&';
      Result := Result + Prefix + 'FontName' + '=' + CDTString + '&';
      Result := Result + Prefix + 'FontSize' + '=' + CDTInteger + '&';
      Result := Result + Prefix + 'Bold' + '=' + CDTBool + '&';
      Result := Result + Prefix + 'Italic' + '=' + CDTBool + '&';
      Result := Result + Prefix + 'Underline' + '=' + CDTBool + '&';
      Result := Result + Prefix + 'StrikeOut' + '=' + CDTBool + '&';
      Result := Result + Prefix + 'FontQuality' + '=' + CDTEnum + '.TFontQuality' + '&';
      Result := Result + Prefix + 'FontQualityUsesReplacement' + '=' + CDTBool + '&';
      Result := Result + Prefix + 'FontQualityReplacement' + '=' + CDTString + '&';
      Result := Result + Prefix + 'ProfileName' + '=' + CDTString + '&';
      Result := Result + Prefix + 'CropLeft' + '=' + CDTString + '&';
      Result := Result + Prefix + 'CropTop' + '=' + CDTString + '&';
      Result := Result + Prefix + 'CropRight' + '=' + CDTString + '&';
      Result := Result + Prefix + 'CropBottom' + '=' + CDTString + '&';
      Result := Result + Prefix + 'IgnoreBackgroundColor' + '=' + CDTBool + '&';
    end;
  end;
begin
  Result := 'MatchCriteria.WillMatchBitmapText' + '=' + CDTBool + '&' +
            'MatchCriteria.WillMatchBitmapFiles' + '=' + CDTBool + '&' +
            'MatchCriteria.WillMatchPrimitiveFiles' + '=' + CDTBool + '&' +
            'AllowToFail' + '=' + CDTBool + '&' +

            'MatchText' + '=' + CDTString + '&' +

            'MatchBitmapText.Count' + '=' + {IntToStr(Length(AFindSubControlOptions.MatchBitmapText))} CDTInteger + '&' +
            GetMatchBitmapTextContent(AMatchBitmapTextLen) +
            'MatchBitmapFiles' + '=' + CDTString + '&' +
            'MatchBitmapAlgorithm' + '=' + CDTEnum + '.TMatchBitmapAlgorithm' + '&' +
            'MatchBitmapAlgorithmSettings.XMultipleOf' + '=' + CDTInteger + '&' +
            'MatchBitmapAlgorithmSettings.YMultipleOf' + '=' + CDTInteger + '&' +
            'MatchBitmapAlgorithmSettings.XOffset' + '=' + CDTInteger + '&' +
            'MatchBitmapAlgorithmSettings.YOffset' + '=' + CDTInteger + '&' +
            'InitialRectangle.Left' + '=' + CDTString + '&' +
            'InitialRectangle.Top' + '=' + CDTString + '&' +
            'InitialRectangle.Right' + '=' + CDTString + '&' +
            'InitialRectangle.Bottom' + '=' + CDTString + '&' +
            'InitialRectangle.LeftOffset' + '=' + CDTString + '&' +
            'InitialRectangle.TopOffset' + '=' + CDTString + '&' +
            'InitialRectangle.RightOffset' + '=' + CDTString + '&' +
            'InitialRectangle.BottomOffset' + '=' + CDTString + '&' +
            'UseWholeScreen' + '=' + CDTBool + '&' +
            'ColorError' + '=' + CDTString + '&' +
            'AllowedColorErrorCount' + '=' + CDTString + '&' +
            'WaitForControlToGoAway' + '=' + CDTBool + '&' +
            'StartSearchingWithCachedControl' + '=' + CDTBool + '&' +
            'CachedControlLeft' + '=' + CDTString + '&' +
            'CachedControlTop' + '=' + CDTString + '&' +
            'MatchPrimitiveFiles' + '=' + CDTString + '&' +
            'GetAllControls' + '=' + CDTBool + '&' +
            'UseFastSearch' + '=' + CDTBool + '&' +
            'FastSearchAllowedColorErrorCount' + '=' + CDTString + '&' +
            'IgnoredColors' + '=' + CDTString + '&' +
            'SleepySearch' + '=' + CDTBool + '&' +
            'StopSearchOnMismatch' + '=' + CDTBool + '&' +
            'ImageSource' + '=' + CDTEnum + '.TImageSource' + '&' +
            'SourceFileName' + '=' + CDTString + '&' +
            'ImageSourceFileNameLocation' + '=' + CDTEnum + '.TImageSourceFileNameLocation' + '&' +
            'PrecisionTimeout' + '=' + CDTBool + '&' +
            'FullBackgroundImageInResult' + '=' + CDTBool + '&' +

            'MatchByHistogramSettings.MinPercentColorMatch' + '=' + CDTString + '&' +
            'MatchByHistogramSettings.MostSignificantColorCountInSubBmp' + '=' + CDTString + '&' +
            'MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp' + '=' + CDTString + '&' +

            'EvaluateTextCount' + '=' + CDTString + '&' +
            'CropFromScreenshot' + '=' + CDTBool + '&' +
            'ThreadCount' + '=' + CDTString + '&' +
            'UseTextRenderingInBrowser' + '=' + CDTBool
            ;
end;


function GetSetControlTextActionPropertyDataTypes: string;
begin
  Result := 'Text' + '=' + CDTString + '&' +
            'ControlType' + '=' + CDTEnum + '.TClkSetTextControlType' + '&' +
            'DelayBetweenKeyStrokes' + '=' + CDTString + '&' +
            'Count' + '=' + CDTString;
end;


function GetCallTemplateActionPropertyDataTypes: string;
begin
  Result := 'TemplateFileName' + '=' + CDTString + '&' +
            'ListOfCustomVarsAndValues' + '=' + CDTString + '&' +
            'EvaluateBeforeCalling' + '=' + CDTBool + '&' +

            'Loop.Enabled' + '=' + CDTBool + '&' +
            'Loop.Counter' + '=' + CDTString + '&' +
            'Loop.InitValue' + '=' + CDTString + '&' +
            'Loop.EndValue' + '=' + CDTString + '&' +
            'Loop.Direction' + '=' + CDTEnum + '.TLoopDirection' + '&' +
            'Loop.BreakCondition' + '=' + CDTString + '&' +
            'Loop.EvalBreakPosition' + '=' + CDTEnum + '.TLoopEvalBreakPosition';
end;


function GetSleepActionPropertyDataTypes: string;
begin
  Result := 'Value' + '=' + CDTString;
end;


function GetSetVarActionPropertyDataTypes: string;
begin
  Result := 'ListOfVarNames' + '=' + CDTString + '&' +
            'ListOfVarValues' + '=' + CDTString + '&' +
            'ListOfVarEvalBefore' + '=' + CDTString + '&' +
            'FailOnException' + '=' + CDTBool;
end;


function GetWindowOperationsActionPropertyDataTypes: string;
begin
  Result := 'Operation' + '=' + CDTEnum + '.TWindowOperation' + '&' +
            'NewX' + '=' + CDTString + '&' +
            'NewY' + '=' + CDTString + '&' +
            'NewWidth' + '=' + CDTString + '&' +
            'NewHeight' + '=' + CDTString + '&' +
            'NewPositionEnabled' + '=' + CDTBool + '&' +
            'NewSizeEnabled' + '=' + CDTBool;
end;


function GetLoadSetVarFromFileActionPropertyDataTypes: string;
begin
  Result := 'FileName' + '=' + CDTString + '&' +
            'SetVarActionName' + '=' + CDTString;
end;


function GetSaveSetVarToFileActionPropertyDataTypes: string;
begin
  Result := 'FileName' + '=' + CDTString + '&' +
            'SetVarActionName' + '=' + CDTString;
end;


function GetPluginActionPropertyDataTypes: string;
begin
  Result := 'FileName' + '=' + CDTString + '&' +
            'ListOfPropertiesAndValues' + '=' + CDTString;
end;


function GetEditTemplateActionPropertyDataTypes(AIncludeListOfEditedProperties: Boolean = False): string;
begin
  Result := 'Operation' + '=' + CDTEnum + '.TEditTemplateOperation' + '&' +
            'WhichTemplate' + '=' + CDTEnum + '.TEditTemplateWhichTemplate' + '&' +
            'TemplateFileName' + '=' + CDTString + '&';

            if AIncludeListOfEditedProperties then  //this should be true when serializing for http
              Result := Result + 'ListOfEditedProperties' + '=' + CDTString + '&'; //ListOfEditedProperties stores a #18 separated list of key=value strings

  Result := Result +
            'ListOfEnabledProperties' + '=' + CDTString + '&' +
            'EditedActionName' + '=' + CDTString + '&' +
            'EditedActionType' + '=' + CDTEnum + '.TClkAction' + '&' +
            'EditedActionCondition' + '=' + CDTString + '&' +
            'EditedActionTimeout' + '=' + CDTInteger + '&' +
            'NewActionName' + '=' + CDTString + '&' +
            'ShouldSaveTemplate' + '=' + CDTBool;
end;


function GetActionPropertyDataTypesByType(var AAction: TClkActionRec; AIncludeSpecialProperties: Boolean = False): string;
begin
  Result := '';
  case AAction.ActionOptions.Action of
    acClick: Result := GetClickActionPropertyDataTypes;
    acExecApp: Result := GetExecAppActionPropertyDataTypes;
    acFindControl:    Result := GetFindControlActionPropertyDataTypes;
    acFindSubControl: Result := GetFindSubControlActionPropertyDataTypes(Length(AAction.FindSubControlOptions.MatchBitmapText));
    acSetControlText: Result := GetSetControlTextActionPropertyDataTypes;
    acCallTemplate: Result := GetCallTemplateActionPropertyDataTypes;
    acSleep: Result := GetSleepActionPropertyDataTypes;
    acSetVar: Result := GetSetVarActionPropertyDataTypes;
    acWindowOperations: Result := GetWindowOperationsActionPropertyDataTypes;
    acLoadSetVarFromFile: Result := GetLoadSetVarFromFileActionPropertyDataTypes;
    acSaveSetVarToFile: Result := GetSaveSetVarToFileActionPropertyDataTypes;
    acPlugin: Result := GetPluginActionPropertyDataTypes;
    acEditTemplate: Result := GetEditTemplateActionPropertyDataTypes(AIncludeSpecialProperties);
  end;
end;


function GetDifferentThanDefaultActionPropertyDataTypesByType(var AAction: TClkActionRec; AIncludeSpecialProperties: Boolean = False): string;
var
  ActionWithDefaultProperties: TClkActionRec;

  DefaultProperties, AllProperties, AllPropertyDataTypes: string;
  ListOfDefaultProperties, ListOfAllProperties, ListOfAllPropertyDataTypes: TStringList;
  i: Integer;
begin
  GetDefaultPropertyValuesByType(AAction.ActionOptions.Action, ActionWithDefaultProperties, Length(AAction.FindSubControlOptions.MatchBitmapText));

  DefaultProperties := GetActionPropertiesByType(ActionWithDefaultProperties, AIncludeSpecialProperties);
  AllProperties := GetActionPropertiesByType(AAction, AIncludeSpecialProperties);
  AllPropertyDataTypes := GetActionPropertyDataTypesByType(AAction, AIncludeSpecialProperties);

  ListOfDefaultProperties := TStringList.Create;
  ListOfAllProperties := TStringList.Create;
  ListOfAllPropertyDataTypes := TStringList.Create;
  try
    ListOfDefaultProperties.LineBreak := #13#10;
    ListOfAllProperties.LineBreak := #13#10;
    ListOfAllPropertyDataTypes.LineBreak := #13#10;

    ListOfDefaultProperties.Text := StringReplace(DefaultProperties, '&', #13#10, [rfReplaceAll]);
    ListOfAllProperties.Text := StringReplace(AllProperties, '&', #13#10, [rfReplaceAll]);
    ListOfAllPropertyDataTypes.Text := StringReplace(AllPropertyDataTypes, '&', #13#10, [rfReplaceAll]);

    Result := '';
    for i := 0 to ListOfDefaultProperties.Count - 1 do
      if (ListOfAllProperties.Strings[i] <> ListOfDefaultProperties.Strings[i]) or (ListOfAllProperties.Names[i] = 'MatchBitmapText.Count') then
        Result := Result + ListOfAllPropertyDataTypes.Strings[i] + '&';

    if Result > '' then
      if Result[Length(Result)] = '&' then
        Delete(Result, Length(Result), 1);
  finally
    ListOfDefaultProperties.Free;
    ListOfAllProperties.Free;
    ListOfAllPropertyDataTypes.Free;
  end;
end;


//////


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
  AClickOptions.UseClipCursor := AListOfClickOptionsParams.Values['UseClipCursor'] = '1';
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

  Temp_ActionTimeout := StrToIntDef(AListOfExecAppOptionsParams.Values[CPropertyName_ActionTimeout], 1000);
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
  AExecAppOptions.VerifyFileExistence := AListOfExecAppOptionsParams.Values['VerifyFileExistence'] = '1';

  AActionOptions.ActionName := AListOfExecAppOptionsParams.Values[CPropertyName_ActionName];
  AActionOptions.ActionTimeout := Temp_ActionTimeout;
  AActionOptions.Action := acExecApp;
end;


function SetFindControlActionProperties(AListOfFindControlOptionsParams: TStrings; AOnAddToLog: TOnAddToLog; out AFindControlOptions: TClkFindControlOptions; out AActionOptions: TClkActionOptions): string; //AOnAddToLog can be set to nil if not used.
var
  Temp_SearchForControlMode: Integer;
  Temp_ActionTimeout: Int64;
begin
  Result := '';

  Temp_SearchForControlMode := StrToIntDef(AListOfFindControlOptionsParams.Values['MatchCriteria.SearchForControlMode'], 0);
  if (Temp_SearchForControlMode < 0) or (Temp_SearchForControlMode > Ord(High(TSearchForControlMode))) then
  begin
    Result := 'MatchCriteria.SearchForControlMode is out of range.';
    Exit;
  end;

  Temp_ActionTimeout := StrToIntDef(AListOfFindControlOptionsParams.Values[CPropertyName_ActionTimeout], 1000);
  if (Temp_ActionTimeout < 0) or (Temp_ActionTimeout > 2147483647) then
  begin
    Result := 'ActionTimeout is out of range.';
    Exit;
  end;

  AFindControlOptions.MatchCriteria.SearchForControlMode := TSearchForControlMode(Temp_SearchForControlMode);
  AFindControlOptions.MatchCriteria.WillMatchText := AListOfFindControlOptionsParams.Values['MatchCriteria.WillMatchText'] <> '0';
  AFindControlOptions.MatchCriteria.WillMatchClassName := AListOfFindControlOptionsParams.Values['MatchCriteria.WillMatchClassName'] <> '0';

  AFindControlOptions.AllowToFail := AListOfFindControlOptionsParams.Values['AllowToFail'] = '1';
  AFindControlOptions.MatchText := AListOfFindControlOptionsParams.Values['MatchText'];
  AFindControlOptions.MatchClassName := AListOfFindControlOptionsParams.Values['MatchClassName'];
  AFindControlOptions.MatchTextSeparator := AListOfFindControlOptionsParams.Values['MatchTextSeparator'];
  AFindControlOptions.MatchClassNameSeparator := AListOfFindControlOptionsParams.Values['MatchClassNameSeparator'];

  AFindControlOptions.InitialRectangle.Left := AListOfFindControlOptionsParams.Values['InitialRectangle.Left'];
  AFindControlOptions.InitialRectangle.Top := AListOfFindControlOptionsParams.Values['InitialRectangle.Top'];
  AFindControlOptions.InitialRectangle.Right := AListOfFindControlOptionsParams.Values['InitialRectangle.Right'];
  AFindControlOptions.InitialRectangle.Bottom := AListOfFindControlOptionsParams.Values['InitialRectangle.Bottom'];
  AFindControlOptions.InitialRectangle.LeftOffset := AListOfFindControlOptionsParams.Values['InitialRectangle.LeftOffset'];
  AFindControlOptions.InitialRectangle.TopOffset := AListOfFindControlOptionsParams.Values['InitialRectangle.TopOffset'];
  AFindControlOptions.InitialRectangle.RightOffset := AListOfFindControlOptionsParams.Values['InitialRectangle.RightOffset'];
  AFindControlOptions.InitialRectangle.BottomOffset := AListOfFindControlOptionsParams.Values['InitialRectangle.BottomOffset'];

  if AFindControlOptions.InitialRectangle.Left = '' then
    AFindControlOptions.InitialRectangle.Left := '$Control_Left$'; //can still be set to '0' if that's the purpose

  if AFindControlOptions.InitialRectangle.Top = '' then
    AFindControlOptions.InitialRectangle.Top := '$Control_Top$'; //can still be set to '0' if that's the purpose

  if AFindControlOptions.InitialRectangle.Right = '' then
    AFindControlOptions.InitialRectangle.Right := '$Control_Right$'; //can still be set to '0' if that's the purpose

  if AFindControlOptions.InitialRectangle.Bottom = '' then
    AFindControlOptions.InitialRectangle.Bottom := '$Control_Bottom$'; //can still be set to '0' if that's the purpose

  AFindControlOptions.UseWholeScreen := AListOfFindControlOptionsParams.Values['UseWholeScreen'] <> '0';
  AFindControlOptions.WaitForControlToGoAway := AListOfFindControlOptionsParams.Values['WaitForControlToGoAway'] = '1';
  AFindControlOptions.StartSearchingWithCachedControl := AListOfFindControlOptionsParams.Values['StartSearchingWithCachedControl'] = '1';
  AFindControlOptions.CachedControlLeft := AListOfFindControlOptionsParams.Values['CachedControlLeft'];
  AFindControlOptions.CachedControlTop := AListOfFindControlOptionsParams.Values['CachedControlTop'];

  AFindControlOptions.GetAllControls := AListOfFindControlOptionsParams.Values['GetAllControls'] = '1';

  AFindControlOptions.PrecisionTimeout := AListOfFindControlOptionsParams.Values['PrecisionTimeout'] = '1';
  AFindControlOptions.EvaluateTextCount := AListOfFindControlOptionsParams.Values['EvaluateTextCount'];

  AActionOptions.ActionName := AListOfFindControlOptionsParams.Values[CPropertyName_ActionName];
  AActionOptions.ActionTimeout := Temp_ActionTimeout;
  AActionOptions.Action := acFindControl;
end;


function SetFindSubControlActionProperties(AListOfFindSubControlOptionsParams: TStrings; AOnAddToLog: TOnAddToLog; out AFindSubControlOptions: TClkFindSubControlOptions; out AActionOptions: TClkActionOptions): string; //AOnAddToLog can be set to nil if not used.
var
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

  Temp_MatchBitmapTextCount := StrToIntDef(AListOfFindSubControlOptionsParams.Values['MatchBitmapText.Count'], 0);
  if (Temp_MatchBitmapTextCount < 0) or (Temp_MatchBitmapTextCount > 100) then
  begin
    Result := 'MatchBitmapText.Count is out of range.';
    Exit;
  end;

  Temp_MatchBitmapAlgorithm := StrToIntDef(AListOfFindSubControlOptionsParams.Values['MatchBitmapAlgorithm'], 0);
  if (Temp_MatchBitmapAlgorithm < 0) or (Temp_MatchBitmapAlgorithm > Ord(High(TMatchBitmapAlgorithm))) then
  begin
    Result := 'MatchBitmapAlgorithm is out of range.';
    Exit;
  end;

  Temp_ActionTimeout := StrToIntDef(AListOfFindSubControlOptionsParams.Values[CPropertyName_ActionTimeout], 1000);
  if (Temp_ActionTimeout < 0) or (Temp_ActionTimeout > 2147483647) then
  begin
    Result := 'ActionTimeout is out of range.';
    Exit;
  end;

  Temp_ImageSource := StrToIntDef(AListOfFindSubControlOptionsParams.Values['ImageSource'], Ord(isScreenshot));
  if (Temp_ImageSource < 0) or (Temp_ImageSource > Ord(High(TImageSource))) then
  begin
    Result := 'ImageSource is out of range.';

    if Assigned(AOnAddToLog) then
      AOnAddToLog('ImageSource is out of range.  ImageSource = ' + IntToStr(Temp_ImageSource));

    Exit;
  end;

  Temp_ImageSourceFileNameLocation := StrToIntDef(AListOfFindSubControlOptionsParams.Values['ImageSourceFileNameLocation'], Ord(isflMem));
  if (Temp_ImageSourceFileNameLocation < 0) or (Temp_ImageSourceFileNameLocation > Ord(High(TImageSourceFileNameLocation))) then
  begin
    Result := 'ImageSourceFileNameLocation is out of range.';

    if Assigned(AOnAddToLog) then
      AOnAddToLog('ImageSource is out of range.  ImageSourceFileNameLocation = ' + IntToStr(Temp_ImageSourceFileNameLocation));

    Exit;
  end;

  AFindSubControlOptions.MatchCriteria.WillMatchBitmapText := AListOfFindSubControlOptionsParams.Values['MatchCriteria.WillMatchBitmapText'] <> '0';
  AFindSubControlOptions.MatchCriteria.WillMatchBitmapFiles := AListOfFindSubControlOptionsParams.Values['MatchCriteria.WillMatchBitmapFiles'] = '1';
  AFindSubControlOptions.MatchCriteria.WillMatchPrimitiveFiles := AListOfFindSubControlOptionsParams.Values['MatchCriteria.WillMatchPrimitiveFiles'] = '1';

  AFindSubControlOptions.AllowToFail := AListOfFindSubControlOptionsParams.Values['AllowToFail'] = '1';
  AFindSubControlOptions.MatchText := AListOfFindSubControlOptionsParams.Values['MatchText'];
  SetLength(AFindSubControlOptions.MatchBitmapText, Temp_MatchBitmapTextCount);

  for i := 0 to Temp_MatchBitmapTextCount - 1 do
  begin
    Prefix := 'MatchBitmapText[' + IntToStr(i) + '].';

    Temp_FontSize := StrToIntDef(AListOfFindSubControlOptionsParams.Values[Prefix + 'FontSize'], 8);
    if (Temp_FontSize < 2) or (Temp_FontSize > 200) then
    begin
      Result := Prefix + 'FontSize is out of range.';
      Exit;
    end;

    Temp_FontQuality := StrToIntDef(AListOfFindSubControlOptionsParams.Values[Prefix + 'FontQuality'], 0);
    if (Temp_FontQuality < 0) or (Temp_FontQuality > Ord(High(TFontQuality))) then
    begin
      Result := Prefix + 'FontQuality is out of range.';
      Exit;
    end;

    Temp_CropLeft := AListOfFindSubControlOptionsParams.Values[Prefix + 'CropLeft'];
    Temp_CropTop := AListOfFindSubControlOptionsParams.Values[Prefix + 'CropTop'];
    Temp_CropRight := AListOfFindSubControlOptionsParams.Values[Prefix + 'CropRight'];
    Temp_CropBottom := AListOfFindSubControlOptionsParams.Values[Prefix + 'CropBottom'];

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

    AFindSubControlOptions.MatchBitmapText[i].ForegroundColor := AListOfFindSubControlOptionsParams.Values[Prefix + 'ForegroundColor'];
    AFindSubControlOptions.MatchBitmapText[i].BackgroundColor := AListOfFindSubControlOptionsParams.Values[Prefix + 'BackgroundColor'];
    AFindSubControlOptions.MatchBitmapText[i].FontName := AListOfFindSubControlOptionsParams.Values[Prefix + 'FontName'];
    AFindSubControlOptions.MatchBitmapText[i].FontSize := Temp_FontSize;
    AFindSubControlOptions.MatchBitmapText[i].Bold := AListOfFindSubControlOptionsParams.Values[Prefix + 'Bold'] = '1';
    AFindSubControlOptions.MatchBitmapText[i].Italic := AListOfFindSubControlOptionsParams.Values[Prefix + 'Italic'] = '1';
    AFindSubControlOptions.MatchBitmapText[i].Underline := AListOfFindSubControlOptionsParams.Values[Prefix + 'Underline'] = '1';
    AFindSubControlOptions.MatchBitmapText[i].StrikeOut := AListOfFindSubControlOptionsParams.Values[Prefix + 'StrikeOut'] = '1';
    AFindSubControlOptions.MatchBitmapText[i].FontQuality := TFontQuality(Temp_FontQuality);
    AFindSubControlOptions.MatchBitmapText[i].FontQualityUsesReplacement := AListOfFindSubControlOptionsParams.Values[Prefix + 'FontQualityUsesReplacement'] = '1';
    AFindSubControlOptions.MatchBitmapText[i].FontQualityReplacement := AListOfFindSubControlOptionsParams.Values[Prefix + 'FontQualityReplacement'];
    AFindSubControlOptions.MatchBitmapText[i].ProfileName := AListOfFindSubControlOptionsParams.Values[Prefix + 'ProfileName'];
    AFindSubControlOptions.MatchBitmapText[i].CropLeft := Temp_CropLeft;
    AFindSubControlOptions.MatchBitmapText[i].CropTop := Temp_CropTop;
    AFindSubControlOptions.MatchBitmapText[i].CropRight := Temp_CropRight;
    AFindSubControlOptions.MatchBitmapText[i].CropBottom := Temp_CropBottom;
    AFindSubControlOptions.MatchBitmapText[i].IgnoreBackgroundColor := AListOfFindSubControlOptionsParams.Values[Prefix + 'IgnoreBackgroundColor'] = '1';
  end;

  AFindSubControlOptions.MatchBitmapFiles := FastReplace_45ToReturn(AListOfFindSubControlOptionsParams.Values['MatchBitmapFiles']); //ListOfStrings
  AFindSubControlOptions.MatchBitmapAlgorithm := TMatchBitmapAlgorithm(Temp_MatchBitmapAlgorithm);

  AFindSubControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf := StrToIntDef(AListOfFindSubControlOptionsParams.Values['MatchBitmapAlgorithmSettings.XMultipleOf'], 0);
  AFindSubControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf := StrToIntDef(AListOfFindSubControlOptionsParams.Values['MatchBitmapAlgorithmSettings.YMultipleOf'], 0);
  AFindSubControlOptions.MatchBitmapAlgorithmSettings.XOffset := StrToIntDef(AListOfFindSubControlOptionsParams.Values['MatchBitmapAlgorithmSettings.XOffset'], 0);
  AFindSubControlOptions.MatchBitmapAlgorithmSettings.YOffset := StrToIntDef(AListOfFindSubControlOptionsParams.Values['MatchBitmapAlgorithmSettings.YOffset'], 0);

  AFindSubControlOptions.InitialRectangle.Left := AListOfFindSubControlOptionsParams.Values['InitialRectangle.Left'];
  AFindSubControlOptions.InitialRectangle.Top := AListOfFindSubControlOptionsParams.Values['InitialRectangle.Top'];
  AFindSubControlOptions.InitialRectangle.Right := AListOfFindSubControlOptionsParams.Values['InitialRectangle.Right'];
  AFindSubControlOptions.InitialRectangle.Bottom := AListOfFindSubControlOptionsParams.Values['InitialRectangle.Bottom'];
  AFindSubControlOptions.InitialRectangle.LeftOffset := AListOfFindSubControlOptionsParams.Values['InitialRectangle.LeftOffset'];
  AFindSubControlOptions.InitialRectangle.TopOffset := AListOfFindSubControlOptionsParams.Values['InitialRectangle.TopOffset'];
  AFindSubControlOptions.InitialRectangle.RightOffset := AListOfFindSubControlOptionsParams.Values['InitialRectangle.RightOffset'];
  AFindSubControlOptions.InitialRectangle.BottomOffset := AListOfFindSubControlOptionsParams.Values['InitialRectangle.BottomOffset'];

  if AFindSubControlOptions.InitialRectangle.Left = '' then
    AFindSubControlOptions.InitialRectangle.Left := '$Control_Left$'; //can still be set to '0' if that's the purpose

  if AFindSubControlOptions.InitialRectangle.Top = '' then
    AFindSubControlOptions.InitialRectangle.Top := '$Control_Top$'; //can still be set to '0' if that's the purpose

  if AFindSubControlOptions.InitialRectangle.Right = '' then
    AFindSubControlOptions.InitialRectangle.Right := '$Control_Right$'; //can still be set to '0' if that's the purpose

  if AFindSubControlOptions.InitialRectangle.Bottom = '' then
    AFindSubControlOptions.InitialRectangle.Bottom := '$Control_Bottom$'; //can still be set to '0' if that's the purpose

  AFindSubControlOptions.UseWholeScreen := AListOfFindSubControlOptionsParams.Values['UseWholeScreen'] = '1';
  AFindSubControlOptions.ColorError := AListOfFindSubControlOptionsParams.Values['ColorError'];  //string, to allow var replacements
  AFindSubControlOptions.AllowedColorErrorCount := AListOfFindSubControlOptionsParams.Values['AllowedColorErrorCount'];  //Number of pixels allowed to mismatch
  AFindSubControlOptions.WaitForControlToGoAway := AListOfFindSubControlOptionsParams.Values['WaitForControlToGoAway'] = '1';
  AFindSubControlOptions.StartSearchingWithCachedControl := AListOfFindSubControlOptionsParams.Values['StartSearchingWithCachedControl'] = '1';
  AFindSubControlOptions.CachedControlLeft := AListOfFindSubControlOptionsParams.Values['CachedControlLeft'];
  AFindSubControlOptions.CachedControlTop := AListOfFindSubControlOptionsParams.Values['CachedControlTop'];

  AFindSubControlOptions.MatchPrimitiveFiles := FastReplace_45ToReturn(AListOfFindSubControlOptionsParams.Values['MatchPrimitiveFiles']); //ListOfStrings
  AFindSubControlOptions.GetAllControls := AListOfFindSubControlOptionsParams.Values['GetAllControls'] = '1';

  AFindSubControlOptions.UseFastSearch := AListOfFindSubControlOptionsParams.Values['UseFastSearch'] <> '0';
  AFindSubControlOptions.FastSearchAllowedColorErrorCount := AListOfFindSubControlOptionsParams.Values['FastSearchAllowedColorErrorCount'];
  AFindSubControlOptions.IgnoredColors := AListOfFindSubControlOptionsParams.Values['IgnoredColors'];
  AFindSubControlOptions.SleepySearch := AListOfFindSubControlOptionsParams.Values['SleepySearch'] = '1';
  AFindSubControlOptions.StopSearchOnMismatch := AListOfFindSubControlOptionsParams.Values['StopSearchOnMismatch'] <> '0';

  AFindSubControlOptions.ImageSource := TImageSource(Temp_ImageSource);
  AFindSubControlOptions.SourceFileName := AListOfFindSubControlOptionsParams.Values['SourceFileName'];
  AFindSubControlOptions.ImageSourceFileNameLocation := TImageSourceFileNameLocation(Temp_ImageSourceFileNameLocation);

  AFindSubControlOptions.PrecisionTimeout := AListOfFindSubControlOptionsParams.Values['PrecisionTimeout'] = '1';
  AFindSubControlOptions.FullBackgroundImageInResult := AListOfFindSubControlOptionsParams.Values['FullBackgroundImageInResult'] <> '0';

  AFindSubControlOptions.MatchByHistogramSettings.MinPercentColorMatch := AListOfFindSubControlOptionsParams.Values['MatchByHistogramSettings.MinPercentColorMatch'];
  AFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInSubBmp := AListOfFindSubControlOptionsParams.Values['MatchByHistogramSettings.MostSignificantColorCountInSubBmp'];
  AFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp := AListOfFindSubControlOptionsParams.Values['MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp'];

  if AFindSubControlOptions.MatchByHistogramSettings.MinPercentColorMatch = '' then
    AFindSubControlOptions.MatchByHistogramSettings.MinPercentColorMatch := '50';

  if AFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInSubBmp = '' then
    AFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInSubBmp := '10';

  if AFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp = '' then
    AFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp := '15';

  AFindSubControlOptions.EvaluateTextCount := AListOfFindSubControlOptionsParams.Values['EvaluateTextCount'];
  AFindSubControlOptions.CropFromScreenshot := AListOfFindSubControlOptionsParams.Values['CropFromScreenshot'] = '1';
  AFindSubControlOptions.ThreadCount := AListOfFindSubControlOptionsParams.Values['ThreadCount'];
  AFindSubControlOptions.UseTextRenderingInBrowser := AListOfFindSubControlOptionsParams.Values['UseTextRenderingInBrowser'] = '1';

  AActionOptions.ActionName := AListOfFindSubControlOptionsParams.Values[CPropertyName_ActionName];
  AActionOptions.ActionTimeout := Temp_ActionTimeout;
  AActionOptions.Action := acFindSubControl;
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

  AActionOptions.ActionName := AListOfSleepOptionsParams.Values[CPropertyName_ActionName];
  AActionOptions.ActionTimeout := 0;
  AActionOptions.Action := acSleep;
end;


function SetSetVarActionProperties(AListOfSetVarOptionsParams: TStrings; out ASetVarOptions: TClkSetVarOptions): string;
begin
  Result := '';

  ASetVarOptions.ListOfVarNames := FastReplace_45ToReturn(AListOfSetVarOptionsParams.Values['ListOfVarNames']);
  ASetVarOptions.ListOfVarValues := FastReplace_45ToReturn(AListOfSetVarOptionsParams.Values['ListOfVarValues']);
  ASetVarOptions.ListOfVarEvalBefore := FastReplace_45ToReturn(AListOfSetVarOptionsParams.Values['ListOfVarEvalBefore']);
  ASetVarOptions.FailOnException := AListOfSetVarOptionsParams.Values['FailOnException'] = '1';
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


function SetEditTemplateActionProperties(AListOfEditTemplateOptionsParams: TStrings; out AEditTemplateOptions: TClkEditTemplateOptions; AIncludeListOfEditedProperties: Boolean = False): string;
var
  Temp_Operation, Temp_WhichTemplate, Temp_EditedActionType: Integer;
begin
  Result := '';

  Temp_Operation := StrToIntDef(AListOfEditTemplateOptionsParams.Values['Operation'], 0);
  if (Temp_Operation < 0) or (Temp_Operation > Ord(High(TEditTemplateOperation))) then
  begin
    Result := 'Operation is out of range.';
    Exit;
  end;

  Temp_WhichTemplate := StrToIntDef(AListOfEditTemplateOptionsParams.Values['WhichTemplate'], Ord(etwtOther));
  if (Temp_WhichTemplate < 0) or (Temp_WhichTemplate > Ord(High(TEditTemplateWhichTemplate))) then
  begin
    Result := 'WhichTemplate is out of range.';
    Exit;
  end;

  Temp_EditedActionType := StrToIntDef(AListOfEditTemplateOptionsParams.Values['EditedActionType'], 0);
  if (Temp_EditedActionType < 0) or (Temp_EditedActionType > Ord(High(TClkAction))) then
  begin
    Result := 'EditedActionType is out of range.';
    Exit;
  end;

  AEditTemplateOptions.Operation := TEditTemplateOperation(Temp_Operation);
  AEditTemplateOptions.WhichTemplate := TEditTemplateWhichTemplate(Temp_WhichTemplate);
  AEditTemplateOptions.TemplateFileName := AListOfEditTemplateOptionsParams.Values['TemplateFileName'];

  if AIncludeListOfEditedProperties then
    AEditTemplateOptions.ListOfEditedProperties := FastReplace_45To1920(AListOfEditTemplateOptionsParams.Values['ListOfEditedProperties']);

  AEditTemplateOptions.ListOfEnabledProperties := FastReplace_45ToReturn(AListOfEditTemplateOptionsParams.Values['ListOfEnabledProperties']);
  AEditTemplateOptions.EditedActionName := AListOfEditTemplateOptionsParams.Values['EditedActionName'];
  AEditTemplateOptions.EditedActionType := TClkAction(Temp_EditedActionType);
  AEditTemplateOptions.EditedActionCondition := AListOfEditTemplateOptionsParams.Values['EditedActionCondition'];
  AEditTemplateOptions.EditedActionTimeout := StrToIntDef(AListOfEditTemplateOptionsParams.Values['EditedActionTimeout'], 1000);
  AEditTemplateOptions.NewActionName := AListOfEditTemplateOptionsParams.Values['NewActionName'];
  AEditTemplateOptions.ShouldSaveTemplate := AListOfEditTemplateOptionsParams.Values['ShouldSaveTemplate'] <> '0';
end;


function SetActionProperties(AListOfOptionsParams: TStrings; AActionType: TClkAction; var AActionOptions: TClkActionRec): string; overload; //it outputs only the options, without the action metadata
var
  DummyActionOptions: TClkActionOptions;
begin
  case AActionType of
    acClick: Result := SetClickActionProperties(AListOfOptionsParams, AActionOptions.ClickOptions);
    acExecApp: Result := SetExecAppActionProperties(AListOfOptionsParams, AActionOptions.ExecAppOptions, DummyActionOptions);
    acFindControl:    Result := SetFindControlActionProperties(AListOfOptionsParams, nil, AActionOptions.FindControlOptions, DummyActionOptions);
    acFindSubControl: Result := SetFindSubControlActionProperties(AListOfOptionsParams, nil, AActionOptions.FindSubControlOptions, DummyActionOptions);
    acSetControlText: Result := SetSetControlTextActionProperties(AListOfOptionsParams, AActionOptions.SetTextOptions);
    acCallTemplate: Result := SetCallTemplateActionProperties(AListOfOptionsParams, AActionOptions.CallTemplateOptions);
    acSleep: Result := SetSleepActionProperties(AListOfOptionsParams, AActionOptions.SleepOptions, DummyActionOptions);
    acSetVar: Result := SetSetVarActionProperties(AListOfOptionsParams, AActionOptions.SetVarOptions);
    acWindowOperations: Result := SetWindowOperationsActionProperties(AListOfOptionsParams, AActionOptions.WindowOperationsOptions);
    acLoadSetVarFromFile: Result := SetLoadSetVarFromFileActionProperties(AListOfOptionsParams, AActionOptions.LoadSetVarFromFileOptions);
    acSaveSetVarToFile: Result := SetSaveSetVarToFileActionProperties(AListOfOptionsParams, AActionOptions.SaveSetVarToFileOptions);
    acPlugin: Result := SetPluginActionProperties(AListOfOptionsParams, AActionOptions.PluginOptions);
    acEditTemplate: Result := SetEditTemplateActionProperties(AListOfOptionsParams, AActionOptions.EditTemplateOptions, False);
  end;

  AActionOptions.ActionOptions.Action := AActionType;  //uncomment if needed
end;


function SetActionProperties(AListOfOptionsParams: string; AActionType: TClkAction; var AActionOptions: TClkActionRec): string; overload; //it outputs only the options, without the action metadata
var
  SerProperties: TStringList;
begin
  SerProperties := TStringList.Create;
  try
    SerProperties.LineBreak := #13#10;
    AListOfOptionsParams := FastReplace_ReturnTo45(AListOfOptionsParams); //just in case if there are lists with #13#10
    AListOfOptionsParams := FastReplace_1920To45(AListOfOptionsParams);
    SerProperties.Text := StringReplace(AListOfOptionsParams, '&', #13#10, [rfReplaceAll]);
    Result := SetActionProperties(SerProperties, AActionType, AActionOptions);
  finally
    SerProperties.Free;
  end;
end;


procedure GetDefaultPropertyValues_Click(var AClickOptions: TClkClickOptions);
begin
  AClickOptions.XClickPointReference := xrefLeft;
  AClickOptions.YClickPointReference := yrefTop;
  AClickOptions.XClickPointVar := '$Control_Left$';
  AClickOptions.YClickPointVar := '$Control_Top$';
  AClickOptions.XOffset := '4';
  AClickOptions.YOffset := '4';
  AClickOptions.MouseButton := mbLeft;
  AClickOptions.ClickWithCtrl := False;
  AClickOptions.ClickWithAlt := False;
  AClickOptions.ClickWithShift := False;
  AClickOptions.ClickWithDoubleClick := False;  //deprecated, but the code is still active
  AClickOptions.Count := 1;
  AClickOptions.LeaveMouse := False;
  AClickOptions.MoveWithoutClick := False;
  AClickOptions.ClickType := CClickType_Click;
  AClickOptions.XClickPointReferenceDest := xrefLeft;
  AClickOptions.YClickPointReferenceDest := yrefTop;
  AClickOptions.XClickPointVarDest := '$Control_Left$';
  AClickOptions.YClickPointVarDest := '$Control_Top$';
  AClickOptions.XOffsetDest := '7';
  AClickOptions.YOffsetDest := '7';
  AClickOptions.MouseWheelType := mwtVert;
  AClickOptions.MouseWheelAmount := '1';
  AClickOptions.DelayAfterMovingToDestination := '50';
  AClickOptions.DelayAfterMouseDown := '200';
  AClickOptions.MoveDuration := '-1';
  AClickOptions.UseClipCursor := False;
end;


procedure GetDefaultPropertyValues_ExecApp(var AExecAppOptions: TClkExecAppOptions);
begin
  AExecAppOptions.PathToApp := '';
  AExecAppOptions.ListOfParams := '';
  AExecAppOptions.WaitForApp := False;
  AExecAppOptions.AppStdIn := '';
  AExecAppOptions.CurrentDir := '';
  AExecAppOptions.UseInheritHandles := uihNo;
  AExecAppOptions.NoConsole := False;
  AExecAppOptions.VerifyFileExistence := False;
end;


procedure GetDefaultPropertyValues_FindControl_MatchBitmapText(var AMatchBitmapText: TClkFindControlMatchBitmapText);
begin
  AMatchBitmapText.ForegroundColor := '$Color_Window$';
  AMatchBitmapText.BackgroundColor := '$Color_Highlight$';
  AMatchBitmapText.FontName := 'Tahoma';
  AMatchBitmapText.FontSize := 8;
  AMatchBitmapText.FontQualityReplacement := '';
  AMatchBitmapText.FontQuality := fqNonAntialiased;
  AMatchBitmapText.FontQualityUsesReplacement := False;
  AMatchBitmapText.Bold := False;
  AMatchBitmapText.Italic := False;
  AMatchBitmapText.Underline := False;
  AMatchBitmapText.StrikeOut := False;
  AMatchBitmapText.CropLeft := '0';
  AMatchBitmapText.CropTop := '0';
  AMatchBitmapText.CropRight := '0';
  AMatchBitmapText.CropBottom := '0';
  AMatchBitmapText.IgnoreBackgroundColor := False;
  AMatchBitmapText.ProfileName := CDefaultFontProfileName;
end;


procedure GetDefaultPropertyValues_FindControl(var AFindControlOptions: TClkFindControlOptions);
begin
  AFindControlOptions.MatchCriteria.WillMatchText := True; //not AIsSubControl;
  AFindControlOptions.MatchCriteria.WillMatchClassName := True; //not AIsSubControl;
  AFindControlOptions.MatchCriteria.SearchForControlMode := sfcmGenGrid;
  AFindControlOptions.AllowToFail := False;
  AFindControlOptions.MatchText := '';
  AFindControlOptions.MatchClassName := '';
  AFindControlOptions.MatchTextSeparator := '';
  AFindControlOptions.MatchClassNameSeparator := '';

  AFindControlOptions.InitialRectangle.Left := '$Control_Left$';
  AFindControlOptions.InitialRectangle.Top := '$Control_Top$';
  AFindControlOptions.InitialRectangle.Right := '$Control_Right$';
  AFindControlOptions.InitialRectangle.Bottom := '$Control_Bottom$';
  AFindControlOptions.InitialRectangle.LeftOffset := '0';
  AFindControlOptions.InitialRectangle.TopOffset := '0';
  AFindControlOptions.InitialRectangle.RightOffset := '0';
  AFindControlOptions.InitialRectangle.BottomOffset := '0';
  AFindControlOptions.UseWholeScreen := True; //not AIsSubControl;
  AFindControlOptions.WaitForControlToGoAway := False;
  AFindControlOptions.StartSearchingWithCachedControl := False;
  AFindControlOptions.CachedControlLeft := '';
  AFindControlOptions.CachedControlTop := '';
  AFindControlOptions.GetAllControls := False;

  AFindControlOptions.PrecisionTimeout := False;
  AFindControlOptions.EvaluateTextCount := '-1';
end;


procedure GetDefaultPropertyValues_FindSubControl(var AFindSubControlOptions: TClkFindSubControlOptions; AFindSubControlProfilesCount: Integer = 1);
var
  i: Integer;
begin
  AFindSubControlOptions.MatchCriteria.WillMatchBitmapText := True; //AIsSubControl;
  AFindSubControlOptions.MatchCriteria.WillMatchBitmapFiles := False;
  AFindSubControlOptions.MatchCriteria.WillMatchPrimitiveFiles := False;
  AFindSubControlOptions.AllowToFail := False;
  AFindSubControlOptions.MatchText := '';
  AFindSubControlOptions.MatchBitmapFiles := '';
  AFindSubControlOptions.MatchBitmapAlgorithm := mbaBruteForce;
  AFindSubControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf := 1;
  AFindSubControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf := 1;
  AFindSubControlOptions.MatchBitmapAlgorithmSettings.XOffset := 0;
  AFindSubControlOptions.MatchBitmapAlgorithmSettings.YOffset := 0;
  AFindSubControlOptions.InitialRectangle.Left := '$Control_Left$';
  AFindSubControlOptions.InitialRectangle.Top := '$Control_Top$';
  AFindSubControlOptions.InitialRectangle.Right := '$Control_Right$';
  AFindSubControlOptions.InitialRectangle.Bottom := '$Control_Bottom$';
  AFindSubControlOptions.InitialRectangle.LeftOffset := '0';
  AFindSubControlOptions.InitialRectangle.TopOffset := '0';
  AFindSubControlOptions.InitialRectangle.RightOffset := '0';
  AFindSubControlOptions.InitialRectangle.BottomOffset := '0';
  AFindSubControlOptions.UseWholeScreen := False; //not AIsSubControl;
  AFindSubControlOptions.ColorError := '0';
  AFindSubControlOptions.AllowedColorErrorCount := '0';
  AFindSubControlOptions.WaitForControlToGoAway := False;
  AFindSubControlOptions.StartSearchingWithCachedControl := False;
  AFindSubControlOptions.CachedControlLeft := '';
  AFindSubControlOptions.CachedControlTop := '';
  AFindSubControlOptions.MatchPrimitiveFiles := '';
  AFindSubControlOptions.MatchPrimitiveFiles_Modified := '';
  AFindSubControlOptions.GetAllControls := False;
  AFindSubControlOptions.UseFastSearch := True;
  AFindSubControlOptions.FastSearchAllowedColorErrorCount := '10';
  AFindSubControlOptions.IgnoredColors := '';
  AFindSubControlOptions.SleepySearch := False;
  AFindSubControlOptions.StopSearchOnMismatch := True;
  AFindSubControlOptions.ImageSource := isScreenshot;
  AFindSubControlOptions.SourceFileName := '';
  AFindSubControlOptions.ImageSourceFileNameLocation := isflMem;
  AFindSubControlOptions.PrecisionTimeout := False;
  AFindSubControlOptions.FullBackgroundImageInResult := True;
  AFindSubControlOptions.MatchByHistogramSettings.MinPercentColorMatch := '50';
  AFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInSubBmp := '10';
  AFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp := '15';
  AFindSubControlOptions.EvaluateTextCount := '-1';
  AFindSubControlOptions.CropFromScreenshot := False;
  AFindSubControlOptions.ThreadCount := '2';
  AFindSubControlOptions.UseTextRenderingInBrowser := False;

  SetLength(AFindSubControlOptions.MatchBitmapText, AFindSubControlProfilesCount);

  for i := 0 to AFindSubControlProfilesCount - 1 do
    GetDefaultPropertyValues_FindControl_MatchBitmapText(AFindSubControlOptions.MatchBitmapText[i]);
end;


procedure GetDefaultPropertyValues_SetControlText(var ASetControlTextOptions: TClkSetTextOptions);
begin
  ASetControlTextOptions.Text := '';
  ASetControlTextOptions.ControlType := stEditBox;
  ASetControlTextOptions.DelayBetweenKeyStrokes := '0';
  ASetControlTextOptions.Count := '1';
end;


procedure GetDefaultPropertyValues_CallTemplate(var ACallTemplateOptions: TClkCallTemplateOptions);
begin
  ACallTemplateOptions.TemplateFileName := '';
  ACallTemplateOptions.ListOfCustomVarsAndValues := '';
  ACallTemplateOptions.EvaluateBeforeCalling := False;
  ACallTemplateOptions.CallOnlyIfCondition := False; //still required, to prevent a pop-up, until the feature is removed
  ACallTemplateOptions.CallTemplateLoop.Enabled := False;
  ACallTemplateOptions.CallTemplateLoop.Counter := '';
  ACallTemplateOptions.CallTemplateLoop.InitValue := '';
  ACallTemplateOptions.CallTemplateLoop.EndValue := '';
  ACallTemplateOptions.CallTemplateLoop.Direction := ldInc;
  ACallTemplateOptions.CallTemplateLoop.BreakCondition := '';
  ACallTemplateOptions.CallTemplateLoop.EvalBreakPosition := lebpBeforeContent;
end;


procedure GetDefaultPropertyValues_Sleep(var ASleepOptions: TClkSleepOptions);
begin
  ASleepOptions.Value := '1000';
end;


procedure GetDefaultPropertyValues_SetVar(var ASetVarOptions: TClkSetVarOptions);
begin
  ASetVarOptions.ListOfVarNames := '';
  ASetVarOptions.ListOfVarValues := '';
  ASetVarOptions.ListOfVarEvalBefore := '';
  ASetVarOptions.FailOnException := False;
end;


procedure GetDefaultPropertyValues_WindowOperations(var AWindowOperationsOptions: TClkWindowOperationsOptions);
begin
  AWindowOperationsOptions.Operation := woBringToFront;
  AWindowOperationsOptions.NewX := '';
  AWindowOperationsOptions.NewY := '';
  AWindowOperationsOptions.NewWidth := '';
  AWindowOperationsOptions.NewHeight := '';
  AWindowOperationsOptions.NewPositionEnabled := False;
  AWindowOperationsOptions.NewSizeEnabled := False;
end;


procedure GetDefaultPropertyValues_LoadSetVarFromFile(var ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions);
begin
  ALoadSetVarFromFileOptions.FileName := '';
  ALoadSetVarFromFileOptions.SetVarActionName := '';
end;


procedure GetDefaultPropertyValues_SaveSetVarToFile(var ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions);
begin
  ASaveSetVarToFileOptions.FileName := '';
  ASaveSetVarToFileOptions.SetVarActionName := '';
end;


procedure GetDefaultPropertyValues_Plugin(var APluginOptions: TClkPluginOptions);
begin
  APluginOptions.FileName := '';
  APluginOptions.ListOfPropertiesAndValues := '';
  APluginOptions.ListOfPropertiesAndTypes := '';
  APluginOptions.CachedCount := 0;
  APluginOptions.ListOfInitValues := '';
end;


procedure GetDefaultPropertyValues_EditTemplate(var AEditTemplateOptions: TClkEditTemplateOptions);
var
  ClickOptions: TClkClickOptions;
begin
  AEditTemplateOptions.Operation := etoNewAction;
  AEditTemplateOptions.WhichTemplate := etwtOther;
  AEditTemplateOptions.TemplateFileName := '';
  AEditTemplateOptions.ListOfEnabledProperties := '';
  AEditTemplateOptions.EditedActionName := '';
  AEditTemplateOptions.EditedActionType := acClick; //TClkAction(CClkUnsetAction); // acClick;
  AEditTemplateOptions.EditedActionCondition := '';
  AEditTemplateOptions.EditedActionTimeout := 1000;
  AEditTemplateOptions.NewActionName := '';
  AEditTemplateOptions.ShouldSaveTemplate := True;

  AEditTemplateOptions.ListOfEditedProperties_ET := '';
  AEditTemplateOptions.ListOfEnabledProperties_ET := '';

  GetDefaultPropertyValues_Click(ClickOptions);
  AEditTemplateOptions.ListOfEditedProperties := GetClickActionProperties(ClickOptions);
end;


procedure GetDefaultPropertyValuesByType(AActionType: TClkAction; var AAction: TClkActionRec; AFindSubControlProfilesCount: Integer = 1);
begin
  case AActionType of
    acClick: GetDefaultPropertyValues_Click(AAction.ClickOptions);
    acExecApp: GetDefaultPropertyValues_ExecApp(AAction.ExecAppOptions);
    acFindControl: GetDefaultPropertyValues_FindControl(AAction.FindControlOptions);
    acFindSubControl: GetDefaultPropertyValues_FindSubControl(AAction.FindSubControlOptions, AFindSubControlProfilesCount);
    acSetControlText: GetDefaultPropertyValues_SetControlText(AAction.SetTextOptions);
    acCallTemplate: GetDefaultPropertyValues_CallTemplate(AAction.CallTemplateOptions);
    acSleep: GetDefaultPropertyValues_Sleep(AAction.SleepOptions);
    acSetVar: GetDefaultPropertyValues_SetVar(AAction.SetVarOptions);
    acWindowOperations: GetDefaultPropertyValues_WindowOperations(AAction.WindowOperationsOptions);
    acLoadSetVarFromFile: GetDefaultPropertyValues_LoadSetVarFromFile(AAction.LoadSetVarFromFileOptions);
    acSaveSetVarToFile: GetDefaultPropertyValues_SaveSetVarToFile(AAction.SaveSetVarToFileOptions);
    acPlugin: GetDefaultPropertyValues_Plugin(AAction.PluginOptions);
    acEditTemplate: GetDefaultPropertyValues_EditTemplate(AAction.EditTemplateOptions);
  end;

  AAction.ActionOptions.Action := AActionType;
end;


function IsActionEmpty_Click(var AClickOptions: TClkClickOptions): Boolean;
begin
  Result := (AClickOptions.XClickPointVar = '') and (AClickOptions.YClickPointVar = '') and
            (AClickOptions.XOffset = '') and (AClickOptions.YOffset = '') and
            (AClickOptions.XClickPointVarDest = '') and (AClickOptions.YClickPointVarDest = '') and
            (AClickOptions.XOffsetDest = '') and (AClickOptions.YOffsetDest = '') and
            (AClickOptions.MouseWheelAmount = '') and
            (AClickOptions.DelayAfterMovingToDestination = '') and
            (AClickOptions.DelayAfterMouseDown = '') and
            (AClickOptions.MoveDuration = '');
end;


function IsActionEmpty_ExecApp(var AExecAppOptions: TClkExecAppOptions): Boolean;
begin
  Result := (AExecAppOptions.PathToApp = '') and (AExecAppOptions.ListOfParams = '') and
            (AExecAppOptions.AppStdIn = '') and (AExecAppOptions.CurrentDir = '');
end;


function IsActionEmpty_FindControl(var AFindControlOptions: TClkFindControlOptions): Boolean;
begin
  Result := (AFindControlOptions.InitialRectangle.Left = '') and
            (AFindControlOptions.InitialRectangle.Top = '') and
            (AFindControlOptions.InitialRectangle.Right = '') and
            (AFindControlOptions.InitialRectangle.Bottom = '') and
            (AFindControlOptions.InitialRectangle.LeftOffset = '') and
            (AFindControlOptions.InitialRectangle.TopOffset = '') and
            (AFindControlOptions.InitialRectangle.RightOffset = '') and
            (AFindControlOptions.InitialRectangle.BottomOffset = '');
end;


function IsActionEmpty_FindSubControl(var AFindSubControlOptions: TClkFindSubControlOptions): Boolean;
begin
  Result := (AFindSubControlOptions.InitialRectangle.Left = '') and
            (AFindSubControlOptions.InitialRectangle.Top = '') and
            (AFindSubControlOptions.InitialRectangle.Right = '') and
            (AFindSubControlOptions.InitialRectangle.Bottom = '') and
            (AFindSubControlOptions.InitialRectangle.LeftOffset = '') and
            (AFindSubControlOptions.InitialRectangle.TopOffset = '') and
            (AFindSubControlOptions.InitialRectangle.RightOffset = '') and
            (AFindSubControlOptions.InitialRectangle.BottomOffset = '') and
            (AFindSubControlOptions.ColorError = '') and
            (AFindSubControlOptions.AllowedColorErrorCount = '') and
            (AFindSubControlOptions.MatchByHistogramSettings.MinPercentColorMatch = '') and
            (AFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInSubBmp = '') and
            (AFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp = '');
end;


function IsActionEmpty_SetControlText(var ASetControlTextOptions: TClkSetTextOptions): Boolean;
begin
  Result := (ASetControlTextOptions.DelayBetweenKeyStrokes = '') and
            (ASetControlTextOptions.Count = '');
end;


function IsActionEmpty_CallTemplate(var ACallTemplateOptions: TClkCallTemplateOptions): Boolean;
begin
  Result := False; //there is no default value, different than ''
end;


function IsActionEmpty_Sleep(var ASleepOptions: TClkSleepOptions): Boolean;
begin
  Result := ASleepOptions.Value = '';
end;


function IsActionEmpty_SetVar(var ASetVarOptions: TClkSetVarOptions): Boolean;
begin
  Result := False; //there is no default value, different than ''
end;


function IsActionEmpty_WindowOperations(var AWindowOperationsOptions: TClkWindowOperationsOptions): Boolean;
begin
  Result := False; //there is no default value, different than ''
end;


function IsActionEmpty_LoadSetVarFromFile(var ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions): Boolean;
begin
  Result := False; //there is no default value, different than ''
end;


function IsActionEmpty_SaveSetVarToFile(var ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions): Boolean;
begin
  Result := False; //there is no default value, different than ''
end;


function IsActionEmpty_Plugin(var APluginOptions: TClkPluginOptions): Boolean;
begin
  Result := False; //there is no default value, different than ''
end;


function IsActionEmpty_EditTemplate(var AEditTemplateOptions: TClkEditTemplateOptions): Boolean;
begin
  Result := False; //there is no default value, different than ''
end;

end.

