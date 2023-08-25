{
    Copyright (C) 2023 VCC
    creation date: Dec 2019
    initial release date: 26 Jul 2022

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

unit ClickerTemplates;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ClickerUtils, ClickerIniFiles, Math;


procedure LoadTemplateToCustomActions_V1(Ini: TClkIniReadonlyFile; var ACustomActions: TClkActionsRecArr);
procedure LoadTemplateToCustomActions_V2(Ini: TClkIniReadonlyFile; var ACustomActions: TClkActionsRecArr; var ANotes: string);

//procedure SaveTemplateWithCustomActions_V1(Fnm: string; ACustomActions: TClkActionsRecArr); //not used anymore
procedure SaveTemplateWithCustomActionsToStringList_V2(AStringList: TStringList; var ACustomActions: TClkActionsRecArr; ANotes: string);
procedure CopyActionContent(ASrc: TClkActionRec; var ADest: TClkActionRec);
procedure GetTemplateContentAsMemoryStream(var ATemplateContent: TClkActionsRecArr; ANotes: string; AFileContentMem: TMemoryStream);
procedure GetTemplateContentFromMemoryStream(var ACustomActions: TClkActionsRecArr; var ANotes: string; AFileContentMem: TMemoryStream);

{
 What V2 does better than V1:
   - Template files (*.clktmpl) can be easily diff'ed as text
   - Only the used action types are saved per action, although the structure contains all action types. This reduces the overall file size.
   - The code is easier to read.
   - V2 allows copy-paste operations using the same ini format as *.clktmpl files.
}


implementation


uses
  Controls, Graphics;


procedure AdjustListOfVarEvalBeforeCount(var ASetVarOptions: TClkSetVarOptions);   //required to update old lists to the new structure
var
  i, n1, n2: Integer;
begin
  n1 := 0;
  n2 := 0;
  for i := 1 to Length(ASetVarOptions.ListOfVarNames) - 1 do    //yes, 1 to n - 1
    if (ASetVarOptions.ListOfVarNames[i] = #13) and
       (ASetVarOptions.ListOfVarNames[i + 1] = #10) then
      Inc(n1);

  for i := 1 to Length(ASetVarOptions.ListOfVarEvalBefore) - 1 do    //yes, 1 to n - 1
    if (ASetVarOptions.ListOfVarEvalBefore[i] = #13) and
       (ASetVarOptions.ListOfVarEvalBefore[i + 1] = #10) then
      Inc(n2);

  if n2 < n1 then
    for i := n2 to n1 - 1 do
      ASetVarOptions.ListOfVarEvalBefore := ASetVarOptions.ListOfVarEvalBefore + '0' + #13#10;
end;


procedure LoadTemplateToCustomActions_V1(Ini: TClkIniReadonlyFile; var ACustomActions: TClkActionsRecArr);
var
  IterationStr: string;
  SectionIndex: Integer;
  i: Integer;
begin
  for i := 0 to Length(ACustomActions) - 1 do
  begin
    IterationStr := IntToStr(i);

    SectionIndex := Ini.GetSectionIndex('Actions.ActionOptions');
    ACustomActions[i].ActionOptions.ActionName := Ini.ReadString(SectionIndex, 'ActionName_' + IterationStr, 'ActionName_' + IterationStr);
    ACustomActions[i].ActionOptions.Action := ActionAsStringToTClkAction(Ini.ReadString(SectionIndex, 'Action_' + IterationStr, CClkActionStr[acClick]));
    ACustomActions[i].ActionOptions.ActionTimeout := Ini.ReadInteger(SectionIndex, 'ActionTimeout_' + IterationStr, 0);
    ACustomActions[i].ActionOptions.ActionEnabled := Ini.ReadBool(SectionIndex, 'ActionEnabled_' + IterationStr, True);
    ACustomActions[i].ActionOptions.ActionCondition := StringReplace(Ini.ReadString(SectionIndex, 'ActionCondition_' + IterationStr, ''), #4#5, #13#10, [rfReplaceAll]);

    SectionIndex := Ini.GetSectionIndex('Actions.ClickOptions');
    ACustomActions[i].ClickOptions.XClickPointReference := TXClickPointReference(Ini.ReadInteger(SectionIndex, 'XOffsetReference_' + IterationStr, Ord(xrefLeft)));
    ACustomActions[i].ClickOptions.YClickPointReference := TYClickPointReference(Ini.ReadInteger(SectionIndex, 'YOffsetReference_' + IterationStr, Ord(yrefTop)));
    ACustomActions[i].ClickOptions.XClickPointVar := Ini.ReadString(SectionIndex, 'XClickPointVar_' + IterationStr, '$Control_Left$');
    ACustomActions[i].ClickOptions.YClickPointVar := Ini.ReadString(SectionIndex, 'YClickPointVar_' + IterationStr, '$Control_Top$');
    ACustomActions[i].ClickOptions.XOffset := Ini.ReadString(SectionIndex, 'XOffset_' + IterationStr, '0');
    ACustomActions[i].ClickOptions.YOffset := Ini.ReadString(SectionIndex, 'YOffset_' + IterationStr, '0');
    ACustomActions[i].ClickOptions.MouseButton := TMouseButton(Ini.ReadInteger(SectionIndex, 'MouseButton_' + IterationStr, 0));
    ACustomActions[i].ClickOptions.ClickWithCtrl := Ini.ReadBool(SectionIndex, 'ClickWithCtrl_' + IterationStr, False);
    ACustomActions[i].ClickOptions.ClickWithAlt := Ini.ReadBool(SectionIndex, 'ClickWithAlt_' + IterationStr, False);
    ACustomActions[i].ClickOptions.ClickWithShift := Ini.ReadBool(SectionIndex, 'ClickWithShift_' + IterationStr, False);
    ACustomActions[i].ClickOptions.ClickWithDoubleClick := Ini.ReadBool(SectionIndex, 'ClickWithDoubleClick_' + IterationStr, False);
    ACustomActions[i].ClickOptions.LeaveMouse := Ini.ReadBool(SectionIndex, 'LeaveMouse_' + IterationStr, False);
    ACustomActions[i].ClickOptions.MoveWithoutClick := Ini.ReadBool(SectionIndex, 'MoveWithoutClick_' + IterationStr, False);
    ACustomActions[i].ClickOptions.Count := Ini.ReadInteger('Actions.MultiClickOptions', 'Count_' + IterationStr, 0);   //this field is saved in a different section
    ACustomActions[i].ClickOptions.ClickType := Ini.ReadInteger(SectionIndex, 'ClickType_' + IterationStr, 0);
    ACustomActions[i].ClickOptions.XClickPointReferenceDest := TXClickPointReference(Ini.ReadInteger(SectionIndex, 'XOffsetReferenceDest_' + IterationStr, Ord(xrefLeft)));
    ACustomActions[i].ClickOptions.YClickPointReferenceDest := TYClickPointReference(Ini.ReadInteger(SectionIndex, 'YOffsetReferenceDest_' + IterationStr, Ord(yrefTop)));
    ACustomActions[i].ClickOptions.XClickPointVarDest := Ini.ReadString(SectionIndex, 'XClickPointVarDest_' + IterationStr, '$Control_Left$');
    ACustomActions[i].ClickOptions.YClickPointVarDest := Ini.ReadString(SectionIndex, 'YClickPointVarDest_' + IterationStr, '$Control_Top$');
    ACustomActions[i].ClickOptions.XOffsetDest := Ini.ReadString(SectionIndex, 'XOffsetDest_' + IterationStr, '0');
    ACustomActions[i].ClickOptions.YOffsetDest := Ini.ReadString(SectionIndex, 'YOffsetDest_' + IterationStr, '0');

    SectionIndex := Ini.GetSectionIndex('Actions.ExecAppOptions');
    ACustomActions[i].ExecAppOptions.PathToApp := Ini.ReadString(SectionIndex, 'PathToApp_' + IterationStr, '');
    ACustomActions[i].ExecAppOptions.ListOfParams := StringReplace(Ini.ReadString(SectionIndex, 'ListOfParams_' + IterationStr, ''), #4#5, #13#10, [rfReplaceAll]);
    ACustomActions[i].ExecAppOptions.WaitForApp := Ini.ReadBool(SectionIndex, 'WaitForApp_' + IterationStr, False);

    SectionIndex := Ini.GetSectionIndex('Actions.FindControlOptions');
    ACustomActions[i].FindControlOptions.MatchCriteria.WillMatchText := Ini.ReadBool(SectionIndex, 'MatchCriteria.WillMatchText_' + IterationStr, True);
    ACustomActions[i].FindControlOptions.MatchCriteria.WillMatchClassName := Ini.ReadBool(SectionIndex, 'MatchCriteria.WillMatchClassName_' + IterationStr, True);
    ACustomActions[i].FindControlOptions.MatchCriteria.WillMatchBitmapText := Ini.ReadBool(SectionIndex, 'MatchCriteria.WillMatchBitmapText_' + IterationStr, False);
    ACustomActions[i].FindControlOptions.MatchCriteria.WillMatchBitmapFiles := Ini.ReadBool(SectionIndex, 'MatchCriteria.WillMatchBitmapFiles_' + IterationStr, False);
    ACustomActions[i].FindControlOptions.MatchCriteria.WillMatchPrimitiveFiles := Ini.ReadBool(SectionIndex, 'MatchCriteria.WillMatchPrimitiveFiles_' + IterationStr, False);
    ACustomActions[i].FindControlOptions.MatchCriteria.SearchForControlMode := TSearchForControlMode(Ini.ReadInteger(SectionIndex, 'MatchCriteria.SearchForControlMode_' + IterationStr, Ord(sfcmGenGrid)));

    ACustomActions[i].FindControlOptions.AllowToFail := Ini.ReadBool(SectionIndex, 'AllowToFail_' + IterationStr, False);
    ACustomActions[i].FindControlOptions.WaitForControlToGoAway := Ini.ReadBool(SectionIndex, 'WaitForControlToGoAway_' + IterationStr, False);

    ACustomActions[i].FindControlOptions.MatchText := Ini.ReadString(SectionIndex, 'MatchText_' + IterationStr, 'ComponentText_' + IterationStr);
    ACustomActions[i].FindControlOptions.MatchClassName := Ini.ReadString(SectionIndex, 'MatchClassName_' + IterationStr, 'ComponentClassName_' + IterationStr);
    ACustomActions[i].FindControlOptions.MatchTextSeparator := Ini.ReadString(SectionIndex, 'MatchTextSeparator_' + IterationStr, '');
    ACustomActions[i].FindControlOptions.MatchClassNameSeparator := Ini.ReadString(SectionIndex, 'MatchClassNameSeparator_' + IterationStr, '');

    if Length(ACustomActions[i].FindControlOptions.MatchBitmapText) = 0 then  //version 1 did not support multiple font settings
      SetLength(ACustomActions[i].FindControlOptions.MatchBitmapText, 1);
    ACustomActions[i].FindControlOptions.MatchBitmapText[0].ForegroundColor := Ini.ReadString(SectionIndex, 'MatchBitmapText.ForegroundColor_' + IterationStr, '000000');
    ACustomActions[i].FindControlOptions.MatchBitmapText[0].BackgroundColor := Ini.ReadString(SectionIndex, 'MatchBitmapText.BackgroundColor_' + IterationStr, 'FFFFFF');
    ACustomActions[i].FindControlOptions.MatchBitmapText[0].FontName := Ini.ReadString(SectionIndex, 'MatchBitmapText.FontName_' + IterationStr, 'Tahoma');
    ACustomActions[i].FindControlOptions.MatchBitmapText[0].FontSize := Ini.ReadInteger(SectionIndex, 'MatchBitmapText.FontSize_' + IterationStr, 8);
    ACustomActions[i].FindControlOptions.MatchBitmapText[0].Bold := Ini.ReadBool(SectionIndex, 'MatchBitmapText.Bold_' + IterationStr, False);
    ACustomActions[i].FindControlOptions.MatchBitmapText[0].Italic := Ini.ReadBool(SectionIndex, 'MatchBitmapText.Italic_' + IterationStr, False);
    ACustomActions[i].FindControlOptions.MatchBitmapText[0].Underline := Ini.ReadBool(SectionIndex, 'MatchBitmapText.Underline_' + IterationStr, False);
    ACustomActions[i].FindControlOptions.MatchBitmapText[0].StrikeOut := Ini.ReadBool(SectionIndex, 'MatchBitmapText.StrikeOut_' + IterationStr, False);
    ACustomActions[i].FindControlOptions.MatchBitmapText[0].FontQuality := TFontQuality(Ini.ReadInteger(SectionIndex, 'MatchBitmapText.FontQuality_' + IterationStr, Integer(fqDefault)));
    ACustomActions[i].FindControlOptions.MatchBitmapText[0].FontQualityUsesReplacement := Ini.ReadBool(SectionIndex, 'MatchBitmapText.FontQualityUsesReplacement_' + IterationStr, False);
    ACustomActions[i].FindControlOptions.MatchBitmapText[0].FontQualityReplacement := Ini.ReadString(SectionIndex, 'MatchBitmapText.FontQualityReplacement_' + IterationStr, '$MyFontQuality$');
    ACustomActions[i].FindControlOptions.MatchBitmapText[0].ProfileName := Ini.ReadString(SectionIndex, 'MatchBitmapText.ProfileName_' + IterationStr, 'Default');

    ACustomActions[i].FindControlOptions.MatchBitmapFiles := StringReplace(Ini.ReadString(SectionIndex, 'MatchBitmapFiles_' + IterationStr, ''), #4#5, #13#10, [rfReplaceAll]);

    ACustomActions[i].FindControlOptions.ColorError := Ini.ReadString(SectionIndex, 'ColorError_' + IterationStr, '0');
    ACustomActions[i].FindControlOptions.AllowedColorErrorCount := Ini.ReadString(SectionIndex, 'AllowedColorErrorCount_' + IterationStr, '0');
    ACustomActions[i].FindControlOptions.MatchBitmapAlgorithm := TMatchBitmapAlgorithm(Min(Ini.ReadInteger(SectionIndex, 'MatchBitmapAlgorithm_' + IterationStr, Integer(mbaBruteForce)), Integer(High(TMatchBitmapAlgorithm))));
    ACustomActions[i].FindControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf := Ini.ReadInteger(SectionIndex, 'MatchBitmapAlgorithm_Grid_XMultipleOf' + IterationStr, 1);
    ACustomActions[i].FindControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf := Ini.ReadInteger(SectionIndex, 'MatchBitmapAlgorithm_Grid_YMultipleOf' + IterationStr, 1);
    ACustomActions[i].FindControlOptions.MatchBitmapAlgorithmSettings.XOffset := Ini.ReadInteger(SectionIndex, 'MatchBitmapAlgorithm_Grid_XOffset' + IterationStr, 0);
    ACustomActions[i].FindControlOptions.MatchBitmapAlgorithmSettings.YOffset := Ini.ReadInteger(SectionIndex, 'MatchBitmapAlgorithm_Grid_YOffset' + IterationStr, 0);

    ACustomActions[i].FindControlOptions.InitialRectangle.Left := Ini.ReadString(SectionIndex, 'InitialRectangle.Left_' + IterationStr, Ini.ReadString(SectionIndex, 'InitialRectange.Left_' + IterationStr, '$Control_Left$'));
    ACustomActions[i].FindControlOptions.InitialRectangle.Top := Ini.ReadString(SectionIndex, 'InitialRectangle.Top_' + IterationStr, Ini.ReadString(SectionIndex, 'InitialRectange.Top_' + IterationStr, '$Control_Top$'));
    ACustomActions[i].FindControlOptions.InitialRectangle.Right := Ini.ReadString(SectionIndex, 'InitialRectangle.Right_' + IterationStr, Ini.ReadString(SectionIndex, 'InitialRectange.Right_' + IterationStr, '$Control_Right$'));
    ACustomActions[i].FindControlOptions.InitialRectangle.Bottom := Ini.ReadString(SectionIndex, 'InitialRectangle.Bottom_' + IterationStr, Ini.ReadString(SectionIndex, 'InitialRectange.Bottom_' + IterationStr, '$Control_Bottom$'));
    ACustomActions[i].FindControlOptions.UseWholeScreen := Ini.ReadBool(SectionIndex, 'UseWholeScreen_' + IterationStr, True);
    ACustomActions[i].FindControlOptions.InitialRectangle.LeftOffset := Ini.ReadString(SectionIndex, 'InitialRectangle.LeftOffset_' + IterationStr, Ini.ReadString(SectionIndex, 'InitialRectange.LeftOffset_' + IterationStr, '0'));
    ACustomActions[i].FindControlOptions.InitialRectangle.TopOffset := Ini.ReadString(SectionIndex, 'InitialRectangle.TopOffset_' + IterationStr, Ini.ReadString(SectionIndex, 'InitialRectange.TopOffset_' + IterationStr, '0'));
    ACustomActions[i].FindControlOptions.InitialRectangle.RightOffset := Ini.ReadString(SectionIndex, 'InitialRectangle.RightOffset_' + IterationStr, Ini.ReadString(SectionIndex, 'InitialRectange.RightOffset_' + IterationStr, '0'));
    ACustomActions[i].FindControlOptions.InitialRectangle.BottomOffset := Ini.ReadString(SectionIndex, 'InitialRectangle.BottomOffset_' + IterationStr, Ini.ReadString(SectionIndex, 'InitialRectange.BottomOffset_' + IterationStr, '0'));

    ACustomActions[i].FindControlOptions.StartSearchingWithCachedControl := Ini.ReadBool(SectionIndex, 'StartSearchingWithCachedControl_' + IterationStr, False);
    ACustomActions[i].FindControlOptions.CachedControlLeft := Ini.ReadString(SectionIndex, 'CachedControlLeft_' + IterationStr, '');
    ACustomActions[i].FindControlOptions.CachedControlTop := Ini.ReadString(SectionIndex, 'CachedControlTop_' + IterationStr, '');

    ACustomActions[i].FindControlOptions.MatchPrimitiveFiles := StringReplace(Ini.ReadString(SectionIndex, 'MatchPrimitiveFiles_' + IterationStr, ''), #4#5, #13#10, [rfReplaceAll]);

    SectionIndex := Ini.GetSectionIndex('Actions.SetTextOptions');
    ACustomActions[i].SetTextOptions.Text := Ini.ReadString(SectionIndex, 'Text_' + IterationStr, '');
    ACustomActions[i].SetTextOptions.ControlType := TClkSetTextControlType(Ini.ReadInteger(SectionIndex, 'ControlType_' + IterationStr, Integer(stEditBox)));

    SectionIndex := Ini.GetSectionIndex('Actions.CallTemplateOptions');
    ACustomActions[i].CallTemplateOptions.TemplateFileName := Ini.ReadString(SectionIndex, 'TemplateFileName_' + IterationStr, '');
    ACustomActions[i].CallTemplateOptions.ListOfCustomVarsAndValues := StringReplace(Ini.ReadString(SectionIndex, 'ListOfCustomVarsAndValues_' + IterationStr, ''), #4#5, #13#10, [rfReplaceAll]);
    ACustomActions[i].CallTemplateOptions.CallOnlyIfCondition := Ini.ReadBool(SectionIndex, 'CallOnlyIfCondition_' + IterationStr, False);
    ACustomActions[i].CallTemplateOptions.CallOnlyIfConditionVarName := Ini.ReadString(SectionIndex, 'CallOnlyIfConditionVarName_' + IterationStr, '');
    ACustomActions[i].CallTemplateOptions.CallOnlyIfConditionVarValue := Ini.ReadString(SectionIndex, 'CallOnlyIfConditionVarValue_' + IterationStr, '');
    ACustomActions[i].CallTemplateOptions.EvaluateBeforeCalling := Ini.ReadBool(SectionIndex, 'EvaluateBeforeCalling_' + IterationStr, False);
    ACustomActions[i].CallTemplateOptions.CallTemplateLoop.Enabled := False; //not implemented in V1

    SectionIndex := Ini.GetSectionIndex('Actions.SleepOptions');
    ACustomActions[i].SleepOptions.Value := Ini.ReadString(SectionIndex, 'Value_' + IterationStr, '1');

    SectionIndex := Ini.GetSectionIndex('Actions.SetVarOptions');
    ACustomActions[i].SetVarOptions.ListOfVarNames := StringReplace(Ini.ReadString(SectionIndex, 'ListOfVarNames_' + IterationStr, ''), #4#5, #13#10, [rfReplaceAll]);
    ACustomActions[i].SetVarOptions.ListOfVarValues := StringReplace(Ini.ReadString(SectionIndex, 'ListOfVarValues_' + IterationStr, ''), #4#5, #13#10, [rfReplaceAll]);
    ACustomActions[i].SetVarOptions.ListOfVarEvalBefore := StringReplace(Ini.ReadString(SectionIndex, 'ListOfVarEvalBefore_' + IterationStr, ''), #4#5, #13#10, [rfReplaceAll]);

    AdjustListOfVarEvalBeforeCount(ACustomActions[i].SetVarOptions);
  end;
end;


procedure LoadAction_Options(Ini: TClkIniReadonlyFile; SectionIndex: Integer; var AActionOptions: TClkActionOptions);
begin
  AActionOptions.ActionName := Ini.ReadString(SectionIndex, 'ActionName', 'ActionName' + IntToStr(SectionIndex));
  AActionOptions.Action := ActionAsStringToTClkAction(Ini.ReadString(SectionIndex, 'Action', CClkActionStr[acClick]));
  AActionOptions.ActionTimeout := Ini.ReadInteger(SectionIndex, 'ActionTimeout', 0);
  AActionOptions.ActionEnabled := Ini.ReadBool(SectionIndex, 'ActionEnabled', True);
  AActionOptions.ActionCondition := FastReplace_45ToReturn(Ini.ReadString(SectionIndex, 'ActionCondition', ''));
end;


procedure LoadAction_Breakpoint(Ini: TClkIniReadonlyFile; SectionIndex: Integer; var AActionBreakPoint: TActionBreakPoint);
begin
  AActionBreakPoint.Exists := Ini.ReadBool(SectionIndex, 'BreakpointExists', False);
  AActionBreakPoint.Enabled := Ini.ReadBool(SectionIndex, 'BreakpointEnabled', True);
  AActionBreakPoint.Condition := FastReplace_45ToReturn(Ini.ReadString(SectionIndex, 'BreakpointCondition', ''));
end;


procedure LoadAction_Click(Ini: TClkIniReadonlyFile; SectionIndex: Integer; var AClickOptions: TClkClickOptions);
begin
  AClickOptions.XClickPointReference := TXClickPointReference(Min(Ini.ReadInteger(SectionIndex, 'XOffsetReference', Ord(xrefLeft)), Integer(High(TXClickPointReference))));
  AClickOptions.YClickPointReference := TYClickPointReference(Min(Ini.ReadInteger(SectionIndex, 'YOffsetReference', Ord(yrefTop)), Integer(High(TYClickPointReference))));
  AClickOptions.XClickPointVar := Ini.ReadString(SectionIndex, 'XClickPointVar', '$Control_Left$');
  AClickOptions.YClickPointVar := Ini.ReadString(SectionIndex, 'YClickPointVar', '$Control_Top$');
  AClickOptions.XOffset := Ini.ReadString(SectionIndex, 'XOffset', '0');
  AClickOptions.YOffset := Ini.ReadString(SectionIndex, 'YOffset', '0');
  AClickOptions.MouseButton := TMouseButton(Min(Ini.ReadInteger(SectionIndex, 'MouseButton', 0), Integer(High(TMouseButton))));
  AClickOptions.ClickWithCtrl := Ini.ReadBool(SectionIndex, 'ClickWithCtrl', False);
  AClickOptions.ClickWithAlt := Ini.ReadBool(SectionIndex, 'ClickWithAlt', False);
  AClickOptions.ClickWithShift := Ini.ReadBool(SectionIndex, 'ClickWithShift', False);
  AClickOptions.ClickWithDoubleClick := Ini.ReadBool(SectionIndex, 'ClickWithDoubleClick', False);
  AClickOptions.LeaveMouse := Ini.ReadBool(SectionIndex, 'LeaveMouse', False);
  AClickOptions.MoveWithoutClick := Ini.ReadBool(SectionIndex, 'MoveWithoutClick', False);
  AClickOptions.Count := Ini.ReadInteger(SectionIndex, 'Count', 0);
  AClickOptions.ClickType := Ini.ReadInteger(SectionIndex, 'ClickType', 0);
  AClickOptions.XClickPointReferenceDest := TXClickPointReference(Min(Ini.ReadInteger(SectionIndex, 'XOffsetReferenceDest', Ord(xrefLeft)), Integer(High(TXClickPointReference)) ));
  AClickOptions.YClickPointReferenceDest := TYClickPointReference(Min(Ini.ReadInteger(SectionIndex, 'YOffsetReferenceDest', Ord(yrefTop)), Integer(High(TYClickPointReference)) ));
  AClickOptions.XClickPointVarDest := Ini.ReadString(SectionIndex, 'XClickPointVarDest', '$Control_Left$');
  AClickOptions.YClickPointVarDest := Ini.ReadString(SectionIndex, 'YClickPointVarDest', '$Control_Top$');
  AClickOptions.XOffsetDest := Ini.ReadString(SectionIndex, 'XOffsetDest', '0');
  AClickOptions.YOffsetDest := Ini.ReadString(SectionIndex, 'YOffsetDest', '0');
  AClickOptions.MouseWheelType := TMouseWheelType(Min(Ini.ReadInteger(SectionIndex, 'MouseWheelType', 0), Integer(High(TMouseWheelType))));
  AClickOptions.MouseWheelAmount := Ini.ReadString(SectionIndex, 'MouseWheelAmount', '0');
end;


procedure LoadAction_ExecApp(Ini: TClkIniReadonlyFile; SectionIndex: Integer; var AExecAppOptions: TClkExecAppOptions);
begin
  AExecAppOptions.PathToApp := Ini.ReadString(SectionIndex, 'PathToApp', '');
  AExecAppOptions.ListOfParams := FastReplace_45ToReturn(Ini.ReadString(SectionIndex, 'ListOfParams', ''));
  AExecAppOptions.WaitForApp := Ini.ReadBool(SectionIndex, 'WaitForApp', False);
  AExecAppOptions.AppStdIn := Ini.ReadString(SectionIndex, 'AppStdIn', '');
  AExecAppOptions.CurrentDir := Ini.ReadString(SectionIndex, 'CurrentDir', '');
  AExecAppOptions.UseInheritHandles := TExecAppUseInheritHandles(Min(Ini.ReadInteger(SectionIndex, 'UseInheritHandles', Ord(uihOnlyWithStdInOut)), Integer(High(TExecAppUseInheritHandles))));
  AExecAppOptions.NoConsole := Ini.ReadBool(SectionIndex, 'NoConsole', False);
end;


procedure LoadAction_FindControl(Ini: TClkIniReadonlyFile; SectionIndex: Integer; var AFindControlOptions: TClkFindControlOptions);
var
  i, n: Integer;
  Indent, s: string;
  ListOfPrimitiveFiles: TStringList;
begin
  AFindControlOptions.MatchCriteria.WillMatchText := Ini.ReadBool(SectionIndex, 'MatchCriteria.WillMatchText', True);
  AFindControlOptions.MatchCriteria.WillMatchClassName := Ini.ReadBool(SectionIndex, 'MatchCriteria.WillMatchClassName', True);
  AFindControlOptions.MatchCriteria.WillMatchBitmapText := Ini.ReadBool(SectionIndex, 'MatchCriteria.WillMatchBitmapText', False);
  AFindControlOptions.MatchCriteria.WillMatchBitmapFiles := Ini.ReadBool(SectionIndex, 'MatchCriteria.WillMatchBitmapFiles', False);
  AFindControlOptions.MatchCriteria.WillMatchPrimitiveFiles := Ini.ReadBool(SectionIndex, 'MatchCriteria.WillMatchPrimitiveFiles', False);
  AFindControlOptions.MatchCriteria.SearchForControlMode := TSearchForControlMode(Min(Ini.ReadInteger(SectionIndex, 'MatchCriteria.SearchForControlMode', Ord(sfcmGenGrid)), Integer(High(TSearchForControlMode))));

  AFindControlOptions.AllowToFail := Ini.ReadBool(SectionIndex, 'AllowToFail', False);
  AFindControlOptions.WaitForControlToGoAway := Ini.ReadBool(SectionIndex, 'WaitForControlToGoAway', False);

  AFindControlOptions.MatchText := Ini.ReadString(SectionIndex, 'MatchText', 'ComponentText');
  AFindControlOptions.MatchClassName := Ini.ReadString(SectionIndex, 'MatchClassName', 'ComponentClassName');
  AFindControlOptions.MatchTextSeparator := Ini.ReadString(SectionIndex, 'MatchTextSeparator', '');
  AFindControlOptions.MatchClassNameSeparator := Ini.ReadString(SectionIndex, 'MatchClassNameSeparator', '');

  if Ini.ReadString(SectionIndex, 'MatchBitmapTextArr', '') = '1' then
  begin
    n := Ini.ReadInteger(SectionIndex, 'MatchBitmapText_Count', 1); //set default length to 1
    SetLength(AFindControlOptions.MatchBitmapText, n);

    for i := 0 to n - 1 do
    begin
      Indent := 'MatchBitmapText[' + IntToStr(i) + '].';

      AFindControlOptions.MatchBitmapText[i].ForegroundColor := Ini.ReadString(SectionIndex, Indent + 'ForegroundColor', '000000');
      AFindControlOptions.MatchBitmapText[i].BackgroundColor := Ini.ReadString(SectionIndex, Indent + 'BackgroundColor', 'FFFFFF');
      AFindControlOptions.MatchBitmapText[i].FontName := Ini.ReadString(SectionIndex, Indent + 'FontName', 'Tahoma');
      AFindControlOptions.MatchBitmapText[i].FontSize := Ini.ReadInteger(SectionIndex, Indent + 'FontSize', 8);
      AFindControlOptions.MatchBitmapText[i].Bold := Ini.ReadBool(SectionIndex, Indent + 'Bold', False);
      AFindControlOptions.MatchBitmapText[i].Italic := Ini.ReadBool(SectionIndex, Indent + 'Italic', False);
      AFindControlOptions.MatchBitmapText[i].Underline := Ini.ReadBool(SectionIndex, Indent + 'Underline', False);
      AFindControlOptions.MatchBitmapText[i].StrikeOut := Ini.ReadBool(SectionIndex, Indent + 'StrikeOut', False);
      AFindControlOptions.MatchBitmapText[i].FontQuality := TFontQuality(Min(Ini.ReadInteger(SectionIndex, Indent + 'FontQuality', Integer(fqDefault)), Integer(High(TFontQuality))));
      AFindControlOptions.MatchBitmapText[i].FontQualityUsesReplacement := Ini.ReadBool(SectionIndex, Indent + 'FontQualityUsesReplacement', False);
      AFindControlOptions.MatchBitmapText[i].FontQualityReplacement := Ini.ReadString(SectionIndex, Indent + 'FontQualityReplacement', '$MyFontQuality$');
      AFindControlOptions.MatchBitmapText[i].ProfileName := Ini.ReadString(SectionIndex, Indent + 'ProfileName', 'Default');
      AFindControlOptions.MatchBitmapText[i].CropLeft := Ini.ReadString(SectionIndex, Indent + 'CropLeft', '0');
      AFindControlOptions.MatchBitmapText[i].CropTop := Ini.ReadString(SectionIndex, Indent + 'CropTop', '0');
      AFindControlOptions.MatchBitmapText[i].CropRight := Ini.ReadString(SectionIndex, Indent + 'CropRight', '0');
      AFindControlOptions.MatchBitmapText[i].CropBottom := Ini.ReadString(SectionIndex, Indent + 'CropBottom', '0');
    end;
  end
  else
  begin  //old BMPText format, where there is only one font setting
    if Length(AFindControlOptions.MatchBitmapText) = 0 then
      SetLength(AFindControlOptions.MatchBitmapText, 1);

    AFindControlOptions.MatchBitmapText[0].ForegroundColor := Ini.ReadString(SectionIndex, 'MatchBitmapText.ForegroundColor', '000000');
    AFindControlOptions.MatchBitmapText[0].BackgroundColor := Ini.ReadString(SectionIndex, 'MatchBitmapText.BackgroundColor', 'FFFFFF');
    AFindControlOptions.MatchBitmapText[0].FontName := Ini.ReadString(SectionIndex, 'MatchBitmapText.FontName', 'Tahoma');
    AFindControlOptions.MatchBitmapText[0].FontSize := Ini.ReadInteger(SectionIndex, 'MatchBitmapText.FontSize', 8);
    AFindControlOptions.MatchBitmapText[0].Bold := Ini.ReadBool(SectionIndex, 'MatchBitmapText.Bold', False);
    AFindControlOptions.MatchBitmapText[0].Italic := Ini.ReadBool(SectionIndex, 'MatchBitmapText.Italic', False);
    AFindControlOptions.MatchBitmapText[0].Underline := Ini.ReadBool(SectionIndex, 'MatchBitmapText.Underline', False);
    AFindControlOptions.MatchBitmapText[0].StrikeOut := Ini.ReadBool(SectionIndex, 'MatchBitmapText.StrikeOut', False);
    AFindControlOptions.MatchBitmapText[0].FontQuality := TFontQuality(Min(Ini.ReadInteger(SectionIndex, 'MatchBitmapText.FontQuality', Integer(fqDefault)), Integer(High(TFontQuality))));
    AFindControlOptions.MatchBitmapText[0].FontQualityUsesReplacement := Ini.ReadBool(SectionIndex, 'MatchBitmapText.FontQualityUsesReplacement', False);
    AFindControlOptions.MatchBitmapText[0].FontQualityReplacement := Ini.ReadString(SectionIndex, 'MatchBitmapText.FontQualityReplacement', '$MyFontQuality$');
    AFindControlOptions.MatchBitmapText[0].ProfileName := Ini.ReadString(SectionIndex, 'MatchBitmapText.ProfileName', 'Default');
    AFindControlOptions.MatchBitmapText[0].CropLeft := Ini.ReadString(SectionIndex, 'MatchBitmapText.CropLeft', '0');
    AFindControlOptions.MatchBitmapText[0].CropTop := Ini.ReadString(SectionIndex, 'MatchBitmapText.CropTop', '0');
    AFindControlOptions.MatchBitmapText[0].CropRight := Ini.ReadString(SectionIndex, 'MatchBitmapText.CropRight', '0');
    AFindControlOptions.MatchBitmapText[0].CropBottom := Ini.ReadString(SectionIndex, 'MatchBitmapText.CropBottom', '0');
  end;

  AFindControlOptions.MatchBitmapFiles := FastReplace_45ToReturn(Ini.ReadString(SectionIndex, 'MatchBitmapFiles', ''));

  AFindControlOptions.ColorError := Ini.ReadString(SectionIndex, 'ColorError', '0');
  AFindControlOptions.AllowedColorErrorCount := Ini.ReadString(SectionIndex, 'AllowedColorErrorCount', '0');
  AFindControlOptions.MatchBitmapAlgorithm := TMatchBitmapAlgorithm(Min(Ini.ReadInteger(SectionIndex, 'MatchBitmapAlgorithm', Integer(mbaBruteForce)), Integer(High(TMatchBitmapAlgorithm))));
  AFindControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf := Ini.ReadInteger(SectionIndex, 'MatchBitmapAlgorithm_Grid_XMultipleOf', 1);
  AFindControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf := Ini.ReadInteger(SectionIndex, 'MatchBitmapAlgorithm_Grid_YMultipleOf', 1);
  AFindControlOptions.MatchBitmapAlgorithmSettings.XOffset := Ini.ReadInteger(SectionIndex, 'MatchBitmapAlgorithm_Grid_XOffset', 0);
  AFindControlOptions.MatchBitmapAlgorithmSettings.YOffset := Ini.ReadInteger(SectionIndex, 'MatchBitmapAlgorithm_Grid_YOffset', 0);

  AFindControlOptions.InitialRectangle.Left := Ini.ReadString(SectionIndex, 'InitialRectangle.Left', Ini.ReadString(SectionIndex, 'InitialRectange.Left', '$Control_Left$'));
  AFindControlOptions.InitialRectangle.Top := Ini.ReadString(SectionIndex, 'InitialRectangle.Top', Ini.ReadString(SectionIndex, 'InitialRectange.Top', '$Control_Top$'));
  AFindControlOptions.InitialRectangle.Right := Ini.ReadString(SectionIndex, 'InitialRectangle.Right', Ini.ReadString(SectionIndex, 'InitialRectange.Right', '$Control_Right$'));
  AFindControlOptions.InitialRectangle.Bottom := Ini.ReadString(SectionIndex, 'InitialRectangle.Bottom', Ini.ReadString(SectionIndex, 'InitialRectange.Bottom', '$Control_Bottom$'));
  AFindControlOptions.UseWholeScreen := Ini.ReadBool(SectionIndex, 'UseWholeScreen', True);
  AFindControlOptions.InitialRectangle.LeftOffset := Ini.ReadString(SectionIndex, 'InitialRectangle.LeftOffset', Ini.ReadString(SectionIndex, 'InitialRectange.LeftOffset', '0'));
  AFindControlOptions.InitialRectangle.TopOffset := Ini.ReadString(SectionIndex, 'InitialRectangle.TopOffset', Ini.ReadString(SectionIndex, 'InitialRectange.TopOffset', '0'));
  AFindControlOptions.InitialRectangle.RightOffset := Ini.ReadString(SectionIndex, 'InitialRectangle.RightOffset', Ini.ReadString(SectionIndex, 'InitialRectange.RightOffset', '0'));
  AFindControlOptions.InitialRectangle.BottomOffset := Ini.ReadString(SectionIndex, 'InitialRectangle.BottomOffset', Ini.ReadString(SectionIndex, 'InitialRectange.BottomOffset', '0'));

  AFindControlOptions.StartSearchingWithCachedControl := Ini.ReadBool(SectionIndex, 'StartSearchingWithCachedControl', False);
  AFindControlOptions.CachedControlLeft := Ini.ReadString(SectionIndex, 'CachedControlLeft', '');
  AFindControlOptions.CachedControlTop := Ini.ReadString(SectionIndex, 'CachedControlTop', '');

  AFindControlOptions.MatchPrimitiveFiles := FastReplace_45ToReturn(Ini.ReadString(SectionIndex, 'MatchPrimitiveFiles', ''));
  ListOfPrimitiveFiles := TStringList.Create;
  try
    ListOfPrimitiveFiles.Text := AFindControlOptions.MatchPrimitiveFiles;
    s := '';
    for i := 0 to ListOfPrimitiveFiles.Count - 1 do
      s := s + '0' + #13#10;

    AFindControlOptions.MatchPrimitiveFiles_Modified := s;
  finally
    ListOfPrimitiveFiles.Free;
  end;
end;


procedure LoadAction_SetControlText(Ini: TClkIniReadonlyFile; SectionIndex: Integer; var ASetTextOptions: TClkSetTextOptions);
begin
  ASetTextOptions.Text := Ini.ReadString(SectionIndex, 'Text', '');
  ASetTextOptions.ControlType := TClkSetTextControlType(Min(Ini.ReadInteger(SectionIndex, 'ControlType', Integer(stEditBox)), Integer(High(TClkSetTextControlType))));
end;


procedure LoadAction_CallTemplate(Ini: TClkIniReadonlyFile; SectionIndex: Integer; var ACallTemplateOptions: TClkCallTemplateOptions);
begin
  ACallTemplateOptions.TemplateFileName := Ini.ReadString(SectionIndex, 'TemplateFileName', '');
  ACallTemplateOptions.ListOfCustomVarsAndValues := FastReplace_45ToReturn(Ini.ReadString(SectionIndex, 'ListOfCustomVarsAndValues', ''));
  ACallTemplateOptions.CallOnlyIfCondition := Ini.ReadBool(SectionIndex, 'CallOnlyIfCondition', False);
  ACallTemplateOptions.CallOnlyIfConditionVarName := Ini.ReadString(SectionIndex, 'CallOnlyIfConditionVarName', '');
  ACallTemplateOptions.CallOnlyIfConditionVarValue := Ini.ReadString(SectionIndex, 'CallOnlyIfConditionVarValue', '');
  ACallTemplateOptions.EvaluateBeforeCalling := Ini.ReadBool(SectionIndex, 'EvaluateBeforeCalling', False);

  ACallTemplateOptions.CallTemplateLoop.Enabled := Ini.ReadBool(SectionIndex, 'Loop.Enabled', False);
  ACallTemplateOptions.CallTemplateLoop.Counter := Ini.ReadString(SectionIndex, 'Loop.Counter', '');
  ACallTemplateOptions.CallTemplateLoop.InitValue := Ini.ReadString(SectionIndex, 'Loop.InitValue', '');
  ACallTemplateOptions.CallTemplateLoop.EndValue := Ini.ReadString(SectionIndex, 'Loop.EndValue', '');
  ACallTemplateOptions.CallTemplateLoop.Direction := TLoopDirection(Min(Ini.ReadInteger(SectionIndex, 'Loop.Direction', Integer(ldInc)), Integer(High(TLoopDirection))));
  ACallTemplateOptions.CallTemplateLoop.BreakCondition := FastReplace_45ToReturn(Ini.ReadString(SectionIndex, 'Loop.BreakCondition', '')); //uses the same format as TClkActionOptions.ActionCondition
  ACallTemplateOptions.CallTemplateLoop.EvalBreakPosition := TLoopEvalBreakPosition(Min(Ini.ReadInteger(SectionIndex, 'Loop.EvalBreakPosition', Integer(lebpAfterContent)), Integer(High(TLoopEvalBreakPosition))));
end;


procedure LoadAction_Sleep(Ini: TClkIniReadonlyFile; SectionIndex: Integer; var ASleepOptions: TClkSleepOptions);
begin
  ASleepOptions.Value := Ini.ReadString(SectionIndex, 'Value', '1');
end;


procedure LoadAction_SetVar(Ini: TClkIniReadonlyFile; SectionIndex: Integer; var ASetVarOptions: TClkSetVarOptions);
begin
  ASetVarOptions.ListOfVarNames := FastReplace_45ToReturn(Ini.ReadString(SectionIndex, 'ListOfVarNames', ''));
  ASetVarOptions.ListOfVarValues := FastReplace_45ToReturn(Ini.ReadString(SectionIndex, 'ListOfVarValues', ''));
  ASetVarOptions.ListOfVarEvalBefore := FastReplace_45ToReturn(Ini.ReadString(SectionIndex, 'ListOfVarEvalBefore', ''));
  AdjustListOfVarEvalBeforeCount(ASetVarOptions);
end;


procedure LoadAction_WindowOperations(Ini: TClkIniReadonlyFile; SectionIndex: Integer; var AWindowOperationsOptions: TClkWindowOperationsOptions);
begin
  AWindowOperationsOptions.Operation := TWindowOperation(Min(Ini.ReadInteger(SectionIndex, 'WindowOperation', Integer(woBringToFront)), Integer(High(TWindowOperation))));
  AWindowOperationsOptions.NewX := Ini.ReadString(SectionIndex, 'NewX', '');
  AWindowOperationsOptions.NewY := Ini.ReadString(SectionIndex, 'NewY', '');
  AWindowOperationsOptions.NewWidth := Ini.ReadString(SectionIndex, 'NewWidth', '');
  AWindowOperationsOptions.NewHeight := Ini.ReadString(SectionIndex, 'NewHeight', '');
  AWindowOperationsOptions.NewPositionEnabled := Ini.ReadBool(SectionIndex, 'NewPositionEnabled', Ini.ReadBool(SectionIndex, 'NewPositionEabled', False));
  AWindowOperationsOptions.NewSizeEnabled := Ini.ReadBool(SectionIndex, 'NewSizeEnabled', Ini.ReadBool(SectionIndex, 'NewSizeEabled', False));
end;


procedure LoadTemplateToCustomActions_V2(Ini: TClkIniReadonlyFile; var ACustomActions: TClkActionsRecArr; var ANotes: string);
var
  IterationStr: string;
  SectionIndex: Integer;
  i, n: Integer;
begin
  n := Ini.ReadInteger('Actions', 'Count', 0);
  SetLength(ACustomActions, n);

  for i := 0 to Length(ACustomActions) - 1 do
  begin
    IterationStr := IntToStr(i);
    SectionIndex := Ini.GetSectionIndex('Action_' + IterationStr);

    LoadAction_Options(Ini, SectionIndex, ACustomActions[i].ActionOptions);
    LoadAction_Breakpoint(Ini, SectionIndex, ACustomActions[i].ActionBreakPoint);

    case ACustomActions[i].ActionOptions.Action of
      acClick: LoadAction_Click(Ini, SectionIndex, ACustomActions[i].ClickOptions);
      acExecApp: LoadAction_ExecApp(Ini, SectionIndex, ACustomActions[i].ExecAppOptions);
      acFindControl:    LoadAction_FindControl(Ini, SectionIndex, ACustomActions[i].FindControlOptions);
      acFindSubControl: LoadAction_FindControl(Ini, SectionIndex, ACustomActions[i].FindControlOptions);
      acSetControlText: LoadAction_SetControlText(Ini, SectionIndex, ACustomActions[i].SetTextOptions);
      acCallTemplate: LoadAction_CallTemplate(Ini, SectionIndex, ACustomActions[i].CallTemplateOptions);
      acSleep: LoadAction_Sleep(Ini, SectionIndex, ACustomActions[i].SleepOptions);
      acSetVar: LoadAction_SetVar(Ini, SectionIndex, ACustomActions[i].SetVarOptions);
      acWindowOperations: LoadAction_WindowOperations(Ini, SectionIndex, ACustomActions[i].WindowOperationsOptions);
    end;
  end;

  ANotes := Ini.ReadString('Notes', 'Content', '');
end;


//This might be fast, but the content is hard to diff. Better use V2.    V1 will not be updated to the new features.
{procedure SaveTemplateWithCustomActions_V1(Fnm: string; ACustomActions: TClkActionsRecArr);
var
  i: Integer;
  AStringList: TStringList;   //much faster than T(Mem)IniFile
  IterationStr: string;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Add('[Actions]');
    AStringList.Add('Count=' + IntToStr(Length(ACustomActions)));

    AStringList.Add('');
    AStringList.Add('[Actions.ActionOptions]');

    for i := 0 to Length(ACustomActions) - 1 do
    begin
      IterationStr := IntToStr(i);
      AStringList.Add('ActionName_' + IterationStr + '=' + ACustomActions[i].ActionOptions.ActionName);
      AStringList.Add('Action_' + IterationStr + '=' + CClkActionStr[ACustomActions[i].ActionOptions.Action]);
      AStringList.Add('ActionTimeout_' + IterationStr + '=' + IntToStr(ACustomActions[i].ActionOptions.ActionTimeout));
      AStringList.Add('ActionEnabled_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].ActionOptions.ActionEnabled)));
      AStringList.Add('ActionCondition_' + IterationStr + '=' + StringReplace(ACustomActions[i].ActionOptions.ActionCondition, #13#10, #4#5, [rfReplaceAll]));
    end;

    AStringList.Add('');
    AStringList.Add('[Actions.ClickOptions]');

    for i := 0 to Length(ACustomActions) - 1 do
    begin
      IterationStr := IntToStr(i);
      AStringList.Add('XOffsetReference_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].ClickOptions.XClickPointReference)));
      AStringList.Add('YOffsetReference_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].ClickOptions.YClickPointReference)));
      AStringList.Add('XClickPointVar_' + IterationStr + '=' + ACustomActions[i].ClickOptions.XClickPointVar);
      AStringList.Add('YClickPointVar_' + IterationStr + '=' + ACustomActions[i].ClickOptions.YClickPointVar);
      AStringList.Add('XOffset_' + IterationStr + '=' + ACustomActions[i].ClickOptions.XOffset);
      AStringList.Add('YOffset_' + IterationStr + '=' + ACustomActions[i].ClickOptions.YOffset);

      AStringList.Add('MouseButton_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].ClickOptions.MouseButton)));
      AStringList.Add('ClickWithCtrl_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].ClickOptions.ClickWithCtrl)));
      AStringList.Add('ClickWithAlt_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].ClickOptions.ClickWithAlt)));
      AStringList.Add('ClickWithShift_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].ClickOptions.ClickWithShift)));
      AStringList.Add('ClickWithDoubleClick_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].ClickOptions.ClickWithDoubleClick)));
      AStringList.Add('LeaveMouse_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].ClickOptions.LeaveMouse)));
      AStringList.Add('MoveWithoutClick_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].ClickOptions.MoveWithoutClick)));

      AStringList.Add('ClickType_' + IterationStr + '=' + IntToStr(ACustomActions[i].ClickOptions.ClickType));
      AStringList.Add('XOffsetReferenceDest_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].ClickOptions.XClickPointReferenceDest)));
      AStringList.Add('YOffsetReferenceDest_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].ClickOptions.YClickPointReferenceDest)));
      AStringList.Add('XClickPointVarDest_' + IterationStr + '=' + ACustomActions[i].ClickOptions.XClickPointVarDest);
      AStringList.Add('YClickPointVarDest_' + IterationStr + '=' + ACustomActions[i].ClickOptions.YClickPointVarDest);
      AStringList.Add('XOffsetDest_' + IterationStr + '=' + ACustomActions[i].ClickOptions.XOffsetDest);
      AStringList.Add('YOffsetDest_' + IterationStr + '=' + ACustomActions[i].ClickOptions.YOffsetDest);
    end;

    AStringList.Add('');
    AStringList.Add('[Actions.MultiClickOptions]');

    for i := 0 to Length(ACustomActions) - 1 do
    begin
      IterationStr := IntToStr(i);
      AStringList.Add('Count_' + IterationStr + '=' + IntToStr(ACustomActions[i].ClickOptions.Count));
    end;

    AStringList.Add('');
    AStringList.Add('[Actions.ExecAppOptions]');

    for i := 0 to Length(ACustomActions) - 1 do
    begin
      IterationStr := IntToStr(i);
      AStringList.Add('PathToApp_' + IterationStr + '=' + ACustomActions[i].ExecAppOptions.PathToApp);
      AStringList.Add('ListOfParams_' + IterationStr + '=' + StringReplace(ACustomActions[i].ExecAppOptions.ListOfParams, #13#10, #4#5, [rfReplaceAll]));
      AStringList.Add('WaitForApp_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].ExecAppOptions.WaitForApp)));
    end;

    AStringList.Add('');
    AStringList.Add('[Actions.FindControlOptions]');

    for i := 0 to Length(ACustomActions) - 1 do
    begin
      IterationStr := IntToStr(i);
      AStringList.Add('MatchCriteria.WillMatchText_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].FindControlOptions.MatchCriteria.WillMatchText)));
      AStringList.Add('MatchCriteria.WillMatchClassName_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].FindControlOptions.MatchCriteria.WillMatchClassName)));
      AStringList.Add('MatchCriteria.WillMatchBitmapText_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].FindControlOptions.MatchCriteria.WillMatchBitmapText)));
      AStringList.Add('MatchCriteria.WillMatchBitmapFiles_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].FindControlOptions.MatchCriteria.WillMatchBitmapFiles)));
      AStringList.Add('MatchCriteria.SearchForControlMode_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].FindControlOptions.MatchCriteria.SearchForControlMode)));
      AStringList.Add('MatchText_' + IterationStr + '=' + ACustomActions[i].FindControlOptions.MatchText);
      AStringList.Add('MatchClassName_' + IterationStr + '=' + ACustomActions[i].FindControlOptions.MatchClassName);
      AStringList.Add('MatchTextSeparator_' + IterationStr + '=' + ACustomActions[i].FindControlOptions.MatchTextSeparator);
      AStringList.Add('MatchClassNameSeparator_' + IterationStr + '=' + ACustomActions[i].FindControlOptions.MatchClassNameSeparator);
      AStringList.Add('AllowToFail_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].FindControlOptions.AllowToFail)));
      AStringList.Add('WaitForControlToGoAway_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].FindControlOptions.WaitForControlToGoAway)));

      AStringList.Add('MatchBitmapText.ForegroundColor_' + IterationStr + '=' + ACustomActions[i].FindControlOptions.MatchBitmapText.ForegroundColor);
      AStringList.Add('MatchBitmapText.BackgroundColor_' + IterationStr + '=' + ACustomActions[i].FindControlOptions.MatchBitmapText.BackgroundColor);
      AStringList.Add('MatchBitmapText.FontName_' + IterationStr + '=' + ACustomActions[i].FindControlOptions.MatchBitmapText.FontName);
      AStringList.Add('MatchBitmapText.FontSize_' + IterationStr + '=' + IntToStr(ACustomActions[i].FindControlOptions.MatchBitmapText.FontSize));
      AStringList.Add('MatchBitmapText.Bold_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].FindControlOptions.MatchBitmapText.Bold)));
      AStringList.Add('MatchBitmapText.Italic_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].FindControlOptions.MatchBitmapText.Italic)));
      AStringList.Add('MatchBitmapText.Underline_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].FindControlOptions.MatchBitmapText.Underline)));
      AStringList.Add('MatchBitmapText.StrikeOut_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].FindControlOptions.MatchBitmapText.StrikeOut)));
      AStringList.Add('MatchBitmapText.FontQuality_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].FindControlOptions.MatchBitmapText.FontQuality)));
      AStringList.Add('MatchBitmapText.FontQualityUsesReplacement_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].FindControlOptions.MatchBitmapText.FontQualityUsesReplacement)));
      AStringList.Add('MatchBitmapText.FontQualityReplacement_' + IterationStr + '=' + ACustomActions[i].FindControlOptions.MatchBitmapText.FontQualityReplacement);

      AStringList.Add('MatchBitmapFiles_' + IterationStr + '=' + StringReplace(ACustomActions[i].FindControlOptions.MatchBitmapFiles, #13#10, #4#5, [rfReplaceAll]));

      AStringList.Add('ColorError_' + IterationStr + '=' + ACustomActions[i].FindControlOptions.ColorError);
      AStringList.Add('AllowedColorErrorCount_' + IterationStr + '=' + ACustomActions[i].FindControlOptions.AllowedColorErrorCount);
      AStringList.Add('MatchBitmapAlgorithm_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].FindControlOptions.MatchBitmapAlgorithm)));
      AStringList.Add('MatchBitmapAlgorithm_Grid_XMultipleOf' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].FindControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf)));
      AStringList.Add('MatchBitmapAlgorithm_Grid_YMultipleOf' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].FindControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf)));
      AStringList.Add('MatchBitmapAlgorithm_Grid_XOffset' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].FindControlOptions.MatchBitmapAlgorithmSettings.XOffset)));
      AStringList.Add('MatchBitmapAlgorithm_Grid_YOffset' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].FindControlOptions.MatchBitmapAlgorithmSettings.YOffset)));

      AStringList.Add('InitialRectangle.Left_' + IterationStr + '=' + ACustomActions[i].FindControlOptions.InitialRectangle.Left);
      AStringList.Add('InitialRectangle.Top_' + IterationStr + '=' + ACustomActions[i].FindControlOptions.InitialRectangle.Top);
      AStringList.Add('InitialRectangle.Right_' + IterationStr + '=' + ACustomActions[i].FindControlOptions.InitialRectangle.Right);
      AStringList.Add('InitialRectangle.Bottom_' + IterationStr + '=' + ACustomActions[i].FindControlOptions.InitialRectangle.Bottom);
      AStringList.Add('UseWholeScreen_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].FindControlOptions.UseWholeScreen)));
      AStringList.Add('InitialRectangle.LeftOffset_' + IterationStr + '=' + ACustomActions[i].FindControlOptions.InitialRectangle.LeftOffset);
      AStringList.Add('InitialRectangle.TopOffset_' + IterationStr + '=' + ACustomActions[i].FindControlOptions.InitialRectangle.TopOffset);
      AStringList.Add('InitialRectangle.RightOffset_' + IterationStr + '=' + ACustomActions[i].FindControlOptions.InitialRectangle.RightOffset);
      AStringList.Add('InitialRectangle.BottomOffset_' + IterationStr + '=' + ACustomActions[i].FindControlOptions.InitialRectangle.BottomOffset);
    end;

    AStringList.Add('');
    AStringList.Add('[Actions.SetTextOptions]');

    for i := 0 to Length(ACustomActions) - 1 do
    begin
      IterationStr := IntToStr(i);
      AStringList.Add('Text_' + IterationStr + '=' + ACustomActions[i].SetTextOptions.Text);
      AStringList.Add('ControlType_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].SetTextOptions.ControlType)));
    end;

    AStringList.Add('');
    AStringList.Add('[Actions.CallTemplateOptions]');

    for i := 0 to Length(ACustomActions) - 1 do
    begin
      IterationStr := IntToStr(i);
      AStringList.Add('TemplateFileName_' + IterationStr + '=' + ACustomActions[i].CallTemplateOptions.TemplateFileName);
      AStringList.Add('ListOfCustomVarsAndValues_' + IterationStr + '=' + StringReplace(ACustomActions[i].CallTemplateOptions.ListOfCustomVarsAndValues, #13#10, #4#5, [rfReplaceAll]));
      AStringList.Add('CallOnlyIfCondition_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].CallTemplateOptions.CallOnlyIfCondition)));
      AStringList.Add('CallOnlyIfConditionVarName_' + IterationStr + '=' + ACustomActions[i].CallTemplateOptions.CallOnlyIfConditionVarName);
      AStringList.Add('CallOnlyIfConditionVarValue_' + IterationStr + '=' + ACustomActions[i].CallTemplateOptions.CallOnlyIfConditionVarValue);
      AStringList.Add('EvaluateBeforeCalling_' + IterationStr + '=' + IntToStr(Ord(ACustomActions[i].CallTemplateOptions.EvaluateBeforeCalling)));
    end;

    AStringList.Add('');
    AStringList.Add('[Actions.SleepOptions]');

    for i := 0 to Length(ACustomActions) - 1 do
    begin
      IterationStr := IntToStr(i);
      AStringList.Add('Value_' + IterationStr + '=' + ACustomActions[i].SleepOptions.Value);
      //
    end;

    AStringList.Add('');
    AStringList.Add('[Actions.SetVarOptions]');

    for i := 0 to Length(ACustomActions) - 1 do
    begin
      IterationStr := IntToStr(i);
      AStringList.Add('ListOfVarNames_' + IterationStr + '=' + StringReplace(ACustomActions[i].SetVarOptions.ListOfVarNames, #13#10, #4#5, [rfReplaceAll]));
      AStringList.Add('ListOfVarValues_' + IterationStr + '=' + StringReplace(ACustomActions[i].SetVarOptions.ListOfVarValues, #13#10, #4#5, [rfReplaceAll]));
      AStringList.Add('ListOfVarEvalBefore_' + IterationStr + '=' + StringReplace(ACustomActions[i].SetVarOptions.ListOfVarEvalBefore, #13#10, #4#5, [rfReplaceAll]));
    end;

    AStringList.SaveToFile(Fnm);
  finally
    AStringList.Free;
  end;
end;}


procedure AddActionOptionsToStringList(var AActionOptions: TClkActionOptions; AStringList: TStringList);
begin
  AStringList.Add('ActionName=' + AActionOptions.ActionName);
  AStringList.Add('Action=' + CClkActionStr[AActionOptions.Action]);
  AStringList.Add('ActionTimeout=' + IntToStr(AActionOptions.ActionTimeout));
  AStringList.Add('ActionEnabled=' + IntToStr(Ord(AActionOptions.ActionEnabled)));
  AStringList.Add('ActionCondition=' + FastReplace_ReturnTo45(AActionOptions.ActionCondition));
end;


procedure AddActionBreakpointToStringList(var AActionBreakpoint: TActionBreakPoint; AStringList: TStringList);
begin
  AStringList.Add('BreakpointExists=' + IntToStr(Ord(AActionBreakpoint.Exists)));
  AStringList.Add('BreakpointEnabled=' + IntToStr(Ord(AActionBreakpoint.Enabled)));
  AStringList.Add('BreakpointCondition=' + FastReplace_ReturnTo45(AActionBreakpoint.Condition));
end;


procedure AddAction_ClickToStringList(var AActionClickOptions: TClkClickOptions; AStringList: TStringList);
begin
  AStringList.Add('XOffsetReference=' + IntToStr(Ord(AActionClickOptions.XClickPointReference)));
  AStringList.Add('YOffsetReference=' + IntToStr(Ord(AActionClickOptions.YClickPointReference)));
  AStringList.Add('XClickPointVar=' + AActionClickOptions.XClickPointVar);
  AStringList.Add('YClickPointVar=' + AActionClickOptions.YClickPointVar);
  AStringList.Add('XOffset=' + AActionClickOptions.XOffset);
  AStringList.Add('YOffset=' + AActionClickOptions.YOffset);

  AStringList.Add('MouseButton=' + IntToStr(Ord(AActionClickOptions.MouseButton)));
  AStringList.Add('ClickWithCtrl=' + IntToStr(Ord(AActionClickOptions.ClickWithCtrl)));
  AStringList.Add('ClickWithAlt=' + IntToStr(Ord(AActionClickOptions.ClickWithAlt)));
  AStringList.Add('ClickWithShift=' + IntToStr(Ord(AActionClickOptions.ClickWithShift)));
  AStringList.Add('ClickWithDoubleClick=' + IntToStr(Ord(AActionClickOptions.ClickWithDoubleClick)));
  AStringList.Add('LeaveMouse=' + IntToStr(Ord(AActionClickOptions.LeaveMouse)));
  AStringList.Add('MoveWithoutClick=' + IntToStr(Ord(AActionClickOptions.MoveWithoutClick)));
  AStringList.Add('Count=' + IntToStr(AActionClickOptions.Count));

  AStringList.Add('ClickType=' + IntToStr(AActionClickOptions.ClickType));
  AStringList.Add('XOffsetReferenceDest=' + IntToStr(Ord(AActionClickOptions.XClickPointReferenceDest)));
  AStringList.Add('YOffsetReferenceDest=' + IntToStr(Ord(AActionClickOptions.YClickPointReferenceDest)));
  AStringList.Add('XClickPointVarDest=' + AActionClickOptions.XClickPointVarDest);
  AStringList.Add('YClickPointVarDest=' + AActionClickOptions.YClickPointVarDest);
  AStringList.Add('XOffsetDest=' + AActionClickOptions.XOffsetDest);
  AStringList.Add('YOffsetDest=' + AActionClickOptions.YOffsetDest);

  AStringList.Add('MouseWheelType=' + IntToStr(Ord(AActionClickOptions.MouseWheelType)));
  AStringList.Add('MouseWheelAmount=' + AActionClickOptions.MouseWheelAmount);
end;


procedure AddAction_ExecAppToStringList(var AActionExecAppOptions: TClkExecAppOptions; AStringList: TStringList);
begin
  AStringList.Add('PathToApp=' + AActionExecAppOptions.PathToApp);
  AStringList.Add('ListOfParams=' + FastReplace_ReturnTo45(AActionExecAppOptions.ListOfParams));
  AStringList.Add('WaitForApp=' + IntToStr(Ord(AActionExecAppOptions.WaitForApp)));
  AStringList.Add('AppStdIn=' + AActionExecAppOptions.AppStdIn);
  AStringList.Add('CurrentDir=' + AActionExecAppOptions.CurrentDir);
  AStringList.Add('UseInheritHandles=' + IntToStr(Ord(AActionExecAppOptions.UseInheritHandles)));
  AStringList.Add('NoConsole=' + IntToStr(Ord(AActionExecAppOptions.NoConsole)));
end;


procedure AddAction_FindControlToStringList(var AActionFindControlOptions: TClkFindControlOptions; AStringList: TStringList);
var
  i: Integer;
  Indent: string;
begin
  AStringList.Add('MatchCriteria.WillMatchText=' + IntToStr(Ord(AActionFindControlOptions.MatchCriteria.WillMatchText)));
  AStringList.Add('MatchCriteria.WillMatchClassName=' + IntToStr(Ord(AActionFindControlOptions.MatchCriteria.WillMatchClassName)));
  AStringList.Add('MatchCriteria.WillMatchBitmapText=' + IntToStr(Ord(AActionFindControlOptions.MatchCriteria.WillMatchBitmapText)));
  AStringList.Add('MatchCriteria.WillMatchBitmapFiles=' + IntToStr(Ord(AActionFindControlOptions.MatchCriteria.WillMatchBitmapFiles)));
  AStringList.Add('MatchCriteria.WillMatchPrimitiveFiles=' + IntToStr(Ord(AActionFindControlOptions.MatchCriteria.WillMatchPrimitiveFiles)));
  AStringList.Add('MatchCriteria.SearchForControlMode=' + IntToStr(Ord(AActionFindControlOptions.MatchCriteria.SearchForControlMode)));
  AStringList.Add('MatchText=' + AActionFindControlOptions.MatchText);
  AStringList.Add('MatchClassName=' + AActionFindControlOptions.MatchClassName);
  AStringList.Add('MatchTextSeparator=' + AActionFindControlOptions.MatchTextSeparator);
  AStringList.Add('MatchClassNameSeparator=' + AActionFindControlOptions.MatchClassNameSeparator);
  AStringList.Add('AllowToFail=' + IntToStr(Ord(AActionFindControlOptions.AllowToFail)));
  AStringList.Add('WaitForControlToGoAway=' + IntToStr(Ord(AActionFindControlOptions.WaitForControlToGoAway)));

  AStringList.Add('MatchBitmapTextArr=1'); //'1' means that MatchBitmapText has array support
  AStringList.Add('MatchBitmapText_Count=' + IntToStr(Length(AActionFindControlOptions.MatchBitmapText)));

  for i := 0 to Length(AActionFindControlOptions.MatchBitmapText) - 1 do
  begin
    Indent := 'MatchBitmapText[' + IntToStr(i) + '].';

    AStringList.Add(Indent + 'ForegroundColor=' + AActionFindControlOptions.MatchBitmapText[i].ForegroundColor);
    AStringList.Add(Indent + 'BackgroundColor=' + AActionFindControlOptions.MatchBitmapText[i].BackgroundColor);
    AStringList.Add(Indent + 'FontName=' + AActionFindControlOptions.MatchBitmapText[i].FontName);
    AStringList.Add(Indent + 'FontSize=' + IntToStr(AActionFindControlOptions.MatchBitmapText[i].FontSize));
    AStringList.Add(Indent + 'Bold=' + IntToStr(Ord(AActionFindControlOptions.MatchBitmapText[i].Bold)));
    AStringList.Add(Indent + 'Italic=' + IntToStr(Ord(AActionFindControlOptions.MatchBitmapText[i].Italic)));
    AStringList.Add(Indent + 'Underline=' + IntToStr(Ord(AActionFindControlOptions.MatchBitmapText[i].Underline)));
    AStringList.Add(Indent + 'StrikeOut=' + IntToStr(Ord(AActionFindControlOptions.MatchBitmapText[i].StrikeOut)));
    AStringList.Add(Indent + 'FontQuality=' + IntToStr(Ord(AActionFindControlOptions.MatchBitmapText[i].FontQuality)));
    AStringList.Add(Indent + 'FontQualityUsesReplacement=' + IntToStr(Ord(AActionFindControlOptions.MatchBitmapText[i].FontQualityUsesReplacement)));
    AStringList.Add(Indent + 'FontQualityReplacement=' + AActionFindControlOptions.MatchBitmapText[i].FontQualityReplacement);
    AStringList.Add(Indent + 'ProfileName=' + AActionFindControlOptions.MatchBitmapText[i].ProfileName);
    AStringList.Add(Indent + 'CropLeft=' + AActionFindControlOptions.MatchBitmapText[i].CropLeft);
    AStringList.Add(Indent + 'CropTop=' + AActionFindControlOptions.MatchBitmapText[i].CropTop);
    AStringList.Add(Indent + 'CropRight=' + AActionFindControlOptions.MatchBitmapText[i].CropRight);
    AStringList.Add(Indent + 'CropBottom=' + AActionFindControlOptions.MatchBitmapText[i].CropBottom);
  end;

  AStringList.Add('MatchBitmapFiles=' + FastReplace_ReturnTo45(AActionFindControlOptions.MatchBitmapFiles));

  AStringList.Add('ColorError=' + AActionFindControlOptions.ColorError);
  AStringList.Add('AllowedColorErrorCount=' + AActionFindControlOptions.AllowedColorErrorCount);
  AStringList.Add('MatchBitmapAlgorithm=' + IntToStr(Ord(AActionFindControlOptions.MatchBitmapAlgorithm)));
  AStringList.Add('MatchBitmapAlgorithm_Grid_XMultipleOf=' + IntToStr(Ord(AActionFindControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf)));
  AStringList.Add('MatchBitmapAlgorithm_Grid_YMultipleOf=' + IntToStr(Ord(AActionFindControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf)));
  AStringList.Add('MatchBitmapAlgorithm_Grid_XOffset=' + IntToStr(Ord(AActionFindControlOptions.MatchBitmapAlgorithmSettings.XOffset)));
  AStringList.Add('MatchBitmapAlgorithm_Grid_YOffset=' + IntToStr(Ord(AActionFindControlOptions.MatchBitmapAlgorithmSettings.YOffset)));

  AStringList.Add('InitialRectangle.Left=' + AActionFindControlOptions.InitialRectangle.Left);
  AStringList.Add('InitialRectangle.Top=' + AActionFindControlOptions.InitialRectangle.Top);
  AStringList.Add('InitialRectangle.Right=' + AActionFindControlOptions.InitialRectangle.Right);
  AStringList.Add('InitialRectangle.Bottom=' + AActionFindControlOptions.InitialRectangle.Bottom);
  AStringList.Add('UseWholeScreen=' + IntToStr(Ord(AActionFindControlOptions.UseWholeScreen)));
  AStringList.Add('InitialRectangle.LeftOffset=' + AActionFindControlOptions.InitialRectangle.LeftOffset);
  AStringList.Add('InitialRectangle.TopOffset=' + AActionFindControlOptions.InitialRectangle.TopOffset);
  AStringList.Add('InitialRectangle.RightOffset=' + AActionFindControlOptions.InitialRectangle.RightOffset);
  AStringList.Add('InitialRectangle.BottomOffset=' + AActionFindControlOptions.InitialRectangle.BottomOffset);

  AStringList.Add('StartSearchingWithCachedControl=' + IntToStr(Ord(AActionFindControlOptions.StartSearchingWithCachedControl)));
  AStringList.Add('CachedControlLeft=' + AActionFindControlOptions.CachedControlLeft);
  AStringList.Add('CachedControlTop=' + AActionFindControlOptions.CachedControlTop);

  AStringList.Add('MatchPrimitiveFiles=' + FastReplace_ReturnTo45(AActionFindControlOptions.MatchPrimitiveFiles));
end;


procedure AddAction_SetControlTextToStringList(var AActionSetTextOptions: TClkSetTextOptions; AStringList: TStringList);
begin
  AStringList.Add('Text=' + AActionSetTextOptions.Text);
  AStringList.Add('ControlType=' + IntToStr(Ord(AActionSetTextOptions.ControlType)));
end;


procedure AddAction_CallTemplateToStringList(var AActionCallTemplateOptions: TClkCallTemplateOptions; AStringList: TStringList);
begin
  AStringList.Add('TemplateFileName=' + AActionCallTemplateOptions.TemplateFileName);
  AStringList.Add('ListOfCustomVarsAndValues=' + FastReplace_ReturnTo45(AActionCallTemplateOptions.ListOfCustomVarsAndValues));
  AStringList.Add('CallOnlyIfCondition=' + IntToStr(Ord(AActionCallTemplateOptions.CallOnlyIfCondition)));
  AStringList.Add('CallOnlyIfConditionVarName=' + AActionCallTemplateOptions.CallOnlyIfConditionVarName);
  AStringList.Add('CallOnlyIfConditionVarValue=' + AActionCallTemplateOptions.CallOnlyIfConditionVarValue);
  AStringList.Add('EvaluateBeforeCalling=' + IntToStr(Ord(AActionCallTemplateOptions.EvaluateBeforeCalling)));

  AStringList.Add('Loop.Enabled=' + IntToStr(Ord(AActionCallTemplateOptions.CallTemplateLoop.Enabled)));
  AStringList.Add('Loop.Counter=' + AActionCallTemplateOptions.CallTemplateLoop.Counter);
  AStringList.Add('Loop.InitValue=' + AActionCallTemplateOptions.CallTemplateLoop.InitValue);
  AStringList.Add('Loop.EndValue=' + AActionCallTemplateOptions.CallTemplateLoop.EndValue);
  AStringList.Add('Loop.Direction=' + IntToStr(Ord(AActionCallTemplateOptions.CallTemplateLoop.Direction)));
  AStringList.Add('Loop.BreakCondition=' + FastReplace_ReturnTo45(AActionCallTemplateOptions.CallTemplateLoop.BreakCondition));
  AStringList.Add('Loop.EvalBreakPosition=' + IntToStr(Ord(AActionCallTemplateOptions.CallTemplateLoop.EvalBreakPosition)));
end;


procedure AddAction_SleepToStringList(var AActionSleepOptions: TClkSleepOptions; AStringList: TStringList);
begin
  AStringList.Add('Value=' + AActionSleepOptions.Value);
end;


procedure AddAction_SetVarToStringList(var AActionSetVarOptions: TClkSetVarOptions; AStringList: TStringList);
begin
  AStringList.Add('ListOfVarNames=' + FastReplace_ReturnTo45(AActionSetVarOptions.ListOfVarNames));
  AStringList.Add('ListOfVarValues=' + FastReplace_ReturnTo45(AActionSetVarOptions.ListOfVarValues));
  AStringList.Add('ListOfVarEvalBefore=' + FastReplace_ReturnTo45(AActionSetVarOptions.ListOfVarEvalBefore));
end;


procedure AddAction_WindowOperationsToStringList(var AActionWindowOperationsOptions: TClkWindowOperationsOptions; AStringList: TStringList);
begin
  AStringList.Add('WindowOperation=' + IntToStr(Ord(AActionWindowOperationsOptions.Operation)));
  AStringList.Add('NewX=' + AActionWindowOperationsOptions.NewX);
  AStringList.Add('NewY=' + AActionWindowOperationsOptions.NewY);
  AStringList.Add('NewWidth=' + AActionWindowOperationsOptions.NewWidth);
  AStringList.Add('NewHeight=' + AActionWindowOperationsOptions.NewHeight);
  AStringList.Add('NewPositionEnabled=' + IntToStr(Ord(AActionWindowOperationsOptions.NewPositionEnabled)));
  AStringList.Add('NewSizeEnabled=' + IntToStr(Ord(AActionWindowOperationsOptions.NewSizeEnabled)));
end;


procedure AddActionContentToStringList(var AAction: TClkActionRec; AStringList: TStringList);
begin
  case AAction.ActionOptions.Action of
    acClick: AddAction_ClickToStringList(AAction.ClickOptions, AStringList);
    acExecApp: AddAction_ExecAppToStringList(AAction.ExecAppOptions, AStringList);
    acFindControl:    AddAction_FindControlToStringList(AAction.FindControlOptions, AStringList);
    acFindSubControl: AddAction_FindControlToStringList(AAction.FindControlOptions, AStringList);
    acSetControlText: AddAction_SetControlTextToStringList(AAction.SetTextOptions, AStringList);
    acCallTemplate: AddAction_CallTemplateToStringList(AAction.CallTemplateOptions, AStringList);
    acSleep: AddAction_SleepToStringList(AAction.SleepOptions, AStringList);
    acSetVar: AddAction_SetVarToStringList(AAction.SetVarOptions, AStringList);
    acWindowOperations: AddAction_WindowOperationsToStringList(AAction.WindowOperationsOptions, AStringList);
  end;
end;


procedure SaveTemplateWithCustomActionsToStringList_V2(AStringList: TStringList; var ACustomActions: TClkActionsRecArr; ANotes: string);
var
  i: Integer;
  IterationStr: string;
begin
  AStringList.Add('[Actions]');
  AStringList.Add('Count=' + IntToStr(Length(ACustomActions)));
  AStringList.Add('Version=2'); // .clktmpl format version

  AStringList.Add('');

  for i := 0 to Length(ACustomActions) - 1 do
  begin
    IterationStr := IntToStr(i);
    AStringList.Add('[Action_' + IterationStr + ']');

    AddActionOptionsToStringList(ACustomActions[i].ActionOptions, AStringList);
    AddActionBreakpointToStringList(ACustomActions[i].ActionBreakPoint, AStringList);
    AddActionContentToStringList(ACustomActions[i], AStringList);

    AStringList.Add('');
  end;

  AStringList.Add('[Notes]');
  AStringList.Add('Content=' + ANotes);

  AStringList.Add('');
end;


procedure CopyActionContent(ASrc: TClkActionRec; var ADest: TClkActionRec);
var
  i: Integer;
begin             //Substructures, which do not contain pointers, can be directly, copied. The others have to be manually copied.
  ADest.ActionDebuggingStatus := ASrc.ActionDebuggingStatus;
  ADest.ActionStatus := ASrc.ActionStatus;
  ADest.ActionSkipped := ASrc.ActionSkipped;
  ADest.ActionOptions := ASrc.ActionOptions;
  ADest.ClickOptions := ASrc.ClickOptions;
  ADest.ExecAppOptions := ASrc.ExecAppOptions;
  //ADest.FindControlOptions := ASrc.FindControlOptions;   //this cannot be directly assigned, because it contains a dynamic array
  ADest.SetTextOptions := ASrc.SetTextOptions;
  ADest.CallTemplateOptions := ASrc.CallTemplateOptions;
  ADest.SleepOptions := ASrc.SleepOptions;
  ADest.SetVarOptions := ASrc.SetVarOptions;
  ADest.WindowOperationsOptions := ASrc.WindowOperationsOptions;

  ADest.FindControlOptions.MatchCriteria := ASrc.FindControlOptions.MatchCriteria;
  ADest.FindControlOptions.AllowToFail := ASrc.FindControlOptions.AllowToFail;
  ADest.FindControlOptions.MatchText := ASrc.FindControlOptions.MatchText;
  ADest.FindControlOptions.MatchClassName := ASrc.FindControlOptions.MatchClassName;
  ADest.FindControlOptions.MatchTextSeparator := ASrc.FindControlOptions.MatchTextSeparator;
  ADest.FindControlOptions.MatchClassNameSeparator := ASrc.FindControlOptions.MatchClassNameSeparator;
  //ADest.FindControlOptions.MatchBitmapText: TClkFindControlMatchBitmapTextArr;      //this cannot be directly assigned, it's an array
  ADest.FindControlOptions.MatchBitmapFiles := ASrc.FindControlOptions.MatchBitmapFiles;
  ADest.FindControlOptions.MatchBitmapAlgorithm := ASrc.FindControlOptions.MatchBitmapAlgorithm;
  ADest.FindControlOptions.MatchBitmapAlgorithmSettings := ASrc.FindControlOptions.MatchBitmapAlgorithmSettings;
  ADest.FindControlOptions.InitialRectangle := ASrc.FindControlOptions.InitialRectangle;
  ADest.FindControlOptions.UseWholeScreen := ASrc.FindControlOptions.UseWholeScreen;
  ADest.FindControlOptions.ColorError := ASrc.FindControlOptions.ColorError;
  ADest.FindControlOptions.AllowedColorErrorCount := ASrc.FindControlOptions.AllowedColorErrorCount;
  ADest.FindControlOptions.WaitForControlToGoAway := ASrc.FindControlOptions.WaitForControlToGoAway;

  ADest.FindControlOptions.StartSearchingWithCachedControl := ASrc.FindControlOptions.StartSearchingWithCachedControl;
  ADest.FindControlOptions.CachedControlLeft := ASrc.FindControlOptions.CachedControlLeft;
  ADest.FindControlOptions.CachedControlTop := ASrc.FindControlOptions.CachedControlTop;

  ADest.FindControlOptions.MatchPrimitiveFiles := ASrc.FindControlOptions.MatchPrimitiveFiles;
  ADest.FindControlOptions.MatchPrimitiveFiles_Modified := ASrc.FindControlOptions.MatchPrimitiveFiles_Modified;

  SetLength(ADest.FindControlOptions.MatchBitmapText, Length(ASrc.FindControlOptions.MatchBitmapText));

  for i := 0 to Length(ADest.FindControlOptions.MatchBitmapText) - 1 do
    ADest.FindControlOptions.MatchBitmapText[i] := ASrc.FindControlOptions.MatchBitmapText[i];
end;


procedure GetTemplateContentAsMemoryStream(var ATemplateContent: TClkActionsRecArr; ANotes: string; AFileContentMem: TMemoryStream);
var
  FileContentStr: TStringList;
begin
  FileContentStr := TStringList.Create;
  try
    SaveTemplateWithCustomActionsToStringList_V2(FileContentStr, ATemplateContent, ANotes);
    FileContentStr.SaveToStream(AFileContentMem);
  finally
    FileContentStr.Free;
  end;
end;


procedure GetTemplateContentFromMemoryStream(var ACustomActions: TClkActionsRecArr; var ANotes: string; AFileContentMem: TMemoryStream);
var
  FileContentStr: TStringList;
  Ini: TClkIniReadonlyFile;
begin
  FileContentStr := TStringList.Create;
  try
    AFileContentMem.Position := 0;
    Ini := TClkIniReadonlyFile.Create(AFileContentMem);
    try
      LoadTemplateToCustomActions_V2(Ini, ACustomActions, ANotes);
    finally
      Ini.Free;
    end;
  finally
    FileContentStr.Free;
  end;
end;

end.

