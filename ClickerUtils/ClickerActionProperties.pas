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


implementation


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
            'SleepySearch' + '=' + IntToStr(Ord(AFindControlOptions.UseFastSearch));
end;


function GetSetControlTextActionProperties(ASetTextOptions: TClkSetTextOptions): string;
begin
  Result := 'Text' + '=' + ASetTextOptions.Text + '&' +
            'ControlType' + '=' + IntToStr(Ord(ASetTextOptions.ControlType)) + '&' +
            'DelayBetweenKeyStrokes' + '=' + ASetTextOptions.DelayBetweenKeyStrokes;
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


end.

