{
    Copyright (C) 2025 VCC
    creation date: Feb 2023
    initial release date: 02 Feb 2023

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


unit ClickerActionValues;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Controls, Graphics,
  ClickerUtils, ObjectInspectorFrame;


{$DEFINE SubProperties}


//OI stuff
type
  TOIPropDef = record
    Name: string;
    EditorType: TOIEditorType;
    DataType: string;
  end;

const
  CCategoryCount = 2;  //Although there actions with more than two categories, maybe it's better to keep this value as used by most of the actions.

  CCategory_Common = 0;
  CCategory_ActionSpecific = 1;
  CCategory_EditedAction = 2;

  CCategory_Name_Common = 'Common';
  CCategory_Name_ActionSpecific = 'Action specific';
  CCategory_Name_EditedAction = 'Edited action';

  CCategories: array[0..CCategoryCount - 1] of string = (CCategory_Name_Common, CCategory_Name_ActionSpecific);
  CPropCount_Common = 4;  //Action name, Action type, Action Timeout, StopOnError

  //Properties (counts)
  CPropCount_Click = 26;
  CPropCount_ExecApp = 7;
  CPropCount_FindControl = 34;
  CPropCount_FindSubControl = 34;
  CPropCount_SetText = 4;
  CPropCount_CallTemplate = 4;
  CPropCount_Sleep = 1;
  CPropCount_SetVar = 2;
  CPropCount_WindowOperations = 7;
  CPropCount_LoadSetVarFromFile = 2;
  CPropCount_SaveSetVarToFile = 2;
  CPropCount_Plugin = 1;  //Static properties, defined here. A plugin can report additional properties, which are not counted by this constant.
  CPropCount_EditTemplate = 11;

  CMainPropCounts: array[0..Ord(High(TClkAction))] of Integer = (
    CPropCount_Click,
    CPropCount_ExecApp,
    CPropCount_FindControl,
    CPropCount_FindSubControl,
    CPropCount_SetText,
    CPropCount_CallTemplate,
    CPropCount_Sleep,
    CPropCount_SetVar,
    CPropCount_WindowOperations,
    CPropCount_LoadSetVarFromFile,
    CPropCount_SaveSetVarToFile,
    CPropCount_Plugin,
    CPropCount_EditTemplate
  );

  //Sub properties (counts)
  CPropCount_FindControlMatchCriteria = 3;
  CPropCount_FindControlInitialRectangle = 8;

  CPropCount_FindSubControlMatchCriteria = 3;
  CPropCount_FindSubControlMatchBitmapText = 17;
  CPropCount_FindSubControlMatchBitmapAlgorithmSettings = 4;
  CPropCount_FindSubControlInitialRectangle = 8;
  CPropCount_FindSubControlMatchByHistogramSettings = 3;

  CPropCount_CallTemplateLoop = 7;

  //PropIndex consts
  CMain_ActionName_PropIndex = 0; //property index in Action structure
  CMain_ActionTimeout_PropIndex = 2; //property index in Action structure
  CMain_ActionCondition_PropIndex = 3; //property index in Action structure

  CClick_XClickPointVar_PropIndex = 2;
  CClick_YClickPointVar_PropIndex = 3;
  CClick_LeaveMouse_PropIndex = 11;
  CClick_XClickPointReferenceDest_PropIndex = 14;
  CClick_YClickPointReferenceDest_PropIndex = 15;
  CClick_XClickPointVarDest_PropIndex = 16;
  CClick_YClickPointVarDest_PropIndex = 17;
  CClick_XOffsetDest_PropIndex = 18;
  CClick_YOffsetDest_PropIndex = 19;
  CClick_MouseWheelType_PropIndex = 20;
  CClick_MouseWheelAmount_PropIndex = 21;
  CClick_DelayAfterMovingToDestination_PropIndex = 22;
  CClick_DelayAfterMouseDown_PropIndex = 23;
  CClick_MoveDuration_PropIndex = 24;
  CClick_UseClipCursor_PropIndex = 25;

  CExecApp_PathToApp_PropIndex = 0;     //property index in ExecApp structure
  CExecApp_ListOfParams_PropIndex = 1;  //property index in ExecApp structure
  CExecApp_CurrentDir_PropIndex = 4;  //property index in ExecApp structure

  CFindControl_MatchCriteria_PropIndex = 0; //property index in FindControl structure
  CFindControl_AllowToFail_PropIndex = 1;
  CFindControl_MatchText_PropIndex = 2;      //property index in FindControl structure
  CFindControl_MatchClassName_PropIndex = 3;  //property index in FindControl structure
  CFindControl_MatchBitmapText_PropIndex = 6; //property index in FindControl structure         //to be removed
  CFindControl_MatchBitmapFiles_PropIndex = 7; //property index in FindControl structure   - list of files   //to be removed
  CFindControl_MatchBitmapAlgorithm_PropIndex = 8;             //to be removed
  CFindControl_MatchBitmapAlgorithmSettings_PropIndex = 9;     //to be removed
  CFindControl_InitialRectangle_PropIndex = 10;
  CFindControl_UseWholeScreen_PropIndex = 11;
  CFindControl_ColorError_PropIndex = 12;     //to be removed
  CFindControl_AllowedColorErrorCount_PropIndex = 13;  //to be removed
  CFindControl_MatchPrimitiveFiles_PropIndex = 18; //property index in FindControl structure   - list of files   //to be removed
  CFindControl_GetAllControls_PropIndex = 19;
  CFindControl_UseFastSearch_PropIndex = 20;   //to be removed
  CFindControl_FastSearchAllowedColorErrorCount_PropIndex = 21;  //to be removed
  CFindControl_IgnoredColors_PropIndex = 22; //to be removed
  CFindControl_SleepySearch_PropIndex = 23;  //to be removed
  CFindControl_StopSearchOnMismatch_PropIndex = 24;  //to be removed
  CFindControl_ImageSource_PropIndex = 25;  //to be removed
  CFindControl_SourceFileName_PropIndex = 26;  //to be removed
  CFindControl_ImageSourceFileNameLocation_PropIndex = 27;  //to be removed
  CFindControl_PrecisionTimeout_PropIndex = 28;  //to be removed
  CFindControl_FullBackgroundImageInResult_PropIndex = 29;  //to be removed
  CFindControl_MatchByHistogramSettings_PropIndex = 30;  //to be removed
  CFindControl_EvaluateTextCount_PropIndex = 31;
  CFindControl_CropFromScreenshot_PropIndex = 32; //to be removed
  CFindControl_ThreadCount_PropIndex = 33;   //to be removed

  CFindSubControl_MatchCriteria_PropIndex = 0; //property index in FindControl structure
  CFindSubControl_AllowToFail_PropIndex = 1;
  CFindSubControl_MatchText_PropIndex = 2;      //property index in FindControl structure    //to be removed
  CFindSubControl_MatchClassName_PropIndex = 3;  //property index in FindControl structure   //to be removed
  CFindSubControl_MatchBitmapText_PropIndex = 6; //property index in FindControl structure
  CFindSubControl_MatchBitmapFiles_PropIndex = 7; //property index in FindControl structure   - list of files
  CFindSubControl_MatchBitmapAlgorithm_PropIndex = 8;
  CFindSubControl_MatchBitmapAlgorithmSettings_PropIndex = 9;
  CFindSubControl_InitialRectangle_PropIndex = 10;
  CFindSubControl_UseWholeScreen_PropIndex = 11;
  CFindSubControl_ColorError_PropIndex = 12;
  CFindSubControl_AllowedColorErrorCount_PropIndex = 13;
  CFindSubControl_MatchPrimitiveFiles_PropIndex = 18; //property index in FindControl structure   - list of files
  CFindSubControl_GetAllControls_PropIndex = 19;
  CFindSubControl_UseFastSearch_PropIndex = 20;
  CFindSubControl_FastSearchAllowedColorErrorCount_PropIndex = 21;
  CFindSubControl_IgnoredColors_PropIndex = 22;
  CFindSubControl_SleepySearch_PropIndex = 23;
  CFindSubControl_StopSearchOnMismatch_PropIndex = 24;
  CFindSubControl_ImageSource_PropIndex = 25;
  CFindSubControl_SourceFileName_PropIndex = 26;
  CFindSubControl_ImageSourceFileNameLocation_PropIndex = 27;
  CFindSubControl_PrecisionTimeout_PropIndex = 28;
  CFindSubControl_FullBackgroundImageInResult_PropIndex = 29;
  CFindSubControl_MatchByHistogramSettings_PropIndex = 30;
  CFindSubControl_EvaluateTextCount_PropIndex = 31;
  CFindSubControl_CropFromScreenshot_PropIndex = 32;
  CFindSubControl_ThreadCount_PropIndex = 33;

  CCallTemplate_TemplateFileName_PropIndex = 0; //property index in CallTemplate structure
  CCallTemplate_ListOfCustomVarsAndValues_PropIndex = 1;
  CCallTemplate_CallTemplateLoop_PropIndex = 3; //property index in CallTemplate structure

  CCallTemplate_CallTemplateLoopProperties_BreakCondition_PropItemIndex = 5;

  CFindControl_MatchCriteria_WillMatchText_PropItemIndex = 0;
  CFindControl_MatchCriteria_WillMatchClassName_PropItemIndex = 1;
  CFindControl_MatchCriteria_SearchForControlMode_PropItemIndex = 2;

  CFindSubControl_MatchCriteria_WillMatchBitmapText_PropItemIndex = 0;
  CFindSubControl_MatchCriteria_WillMatchBitmapFiles_PropItemIndex = 1;
  CFindSubControl_MatchCriteria_WillMatchPrimitiveFiles_PropItemIndex = 2;

  CFindControl_MatchBitmapText_ForegroundColor_PropItemIndex = 0;   //property index in FindControl.MatchBitmapText structure
  CFindControl_MatchBitmapText_BackgroundColor_PropItemIndex = 1;   //property index in FindControl.MatchBitmapText structure
  CFindControl_MatchBitmapText_FontName_PropItemIndex = 2;   //property index in FindControl.MatchBitmapText structure
  CFindControl_MatchBitmapText_ProfileName_PropItemIndex = 11;   //property index in FindControl.MatchBitmapText structure
  CFindControl_MatchBitmapText_IgnoreBackgroundColor_PropItemIndex = 16;   //property index in FindControl.MatchBitmapText structure

  CFindControl_MatchBitmapText_CropLeft = 12;
  CFindControl_MatchBitmapText_CropTop = 13;
  CFindControl_MatchBitmapText_CropRight = 14;
  CFindControl_MatchBitmapText_CropBottom = 15;

  CFindControl_MatchBitmapAlgorithmSettings_XMultipleOf_PropItemIndex = 0;
  CFindControl_MatchBitmapAlgorithmSettings_YMultipleOf_PropItemIndex = 1;
  CFindControl_MatchBitmapAlgorithmSettings_XOffset_PropItemIndex = 2;
  CFindControl_MatchBitmapAlgorithmSettings_YOffset_PropItemIndex = 3;

  CFindControl_InitialRectangle_Left_PropItemIndex = 0;
  CFindControl_InitialRectangle_Top_PropItemIndex = 1;
  CFindControl_InitialRectangle_Right_PropItemIndex = 2;
  CFindControl_InitialRectangle_Bottom_PropItemIndex = 3;
  CFindControl_InitialRectangle_LeftOffset_PropItemIndex = 4;
  CFindControl_InitialRectangle_TopOffset_PropItemIndex = 5;
  CFindControl_InitialRectangle_RightOffset_PropItemIndex = 6;
  CFindControl_InitialRectangle_BottomOffset_PropItemIndex = 7;

  CFindControl_MatchByHistogramSettings_MinPercentColorMatch_PropItemIndex = 0;
  CFindControl_MatchByHistogramSettings_MostSignificantColorCountInSubBmp_PropItemIndex = 1;
  CFindControl_MatchByHistogramSettings_MostSignificantColorCountInBackgroundBmp_PropItemIndex = 2;

  CSetVar_ListOfVarNamesValuesAndEvalBefore_PropItemIndex = 0;
  CSetVar_FailOnException_PropItemIndex = 1;

  CCallTemplate_CallTemplateLoop_Enabled_PropItemIndex = 0;
  CCallTemplate_CallTemplateLoop_Counter_PropItemIndex = 1;
  CCallTemplate_CallTemplateLoop_InitValue_PropItemIndex = 2;
  CCallTemplate_CallTemplateLoop_EndValue_PropItemIndex = 3;
  CCallTemplate_CallTemplateLoop_Direction_PropItemIndex = 4;
  CCallTemplate_CallTemplateLoop_BreakCondition_PropItemIndex = 5;
  CCallTemplate_CallTemplateLoop_EvalBreakPosition_PropItemIndex = 6;

  CWindowOperations_Operation_PropItemIndex = 0;
  CWindowOperations_NewX_PropItemIndex = 1;
  CWindowOperations_NewY_PropItemIndex = 2;
  CWindowOperations_NewWidth_PropItemIndex = 3;
  CWindowOperations_NewHeight_PropItemIndex = 4;
  CWindowOperations_NewPositionEnabled_PropItemIndex = 5;
  CWindowOperations_NewSizeEnabled_PropItemIndex = 6;

  CLoadSetVarFromFile_FileName_PropIndex = 0;
  CLoadSetVarFromFile_SetVarActionName_PropIndex = 1;

  CPlugin_FileName_PropIndex = 0;

  CEditTemplate_Operation_PropIndex = 0;
  CEditTemplate_WhichTemplate_PropIndex = 1;
  CEditTemplate_TemplateFileName_PropIndex = 2;
  CEditTemplate_EditedActionName_PropIndex = 3;
  CEditTemplate_EditedActionType_PropIndex = 4;
  CEditTemplate_EditedActionCondition_PropIndex = 5;
  CEditTemplate_EditedActionTimeout_PropIndex = 6;
  CEditTemplate_NewActionName_PropIndex = 7;
  CEditTemplate_ListOfEditedProperties_PropIndex = 8;
  CEditTemplate_ListOfEnabledProperties_PropIndex = 9;
  CEditTemplate_ShouldSaveTemplate_PropIndex = 10;

  //Moved to ClickerUtils
  //CDTString = 'String';
  //CDTEnum = 'Enum';
  //CDTBool = 'Boolean';
  //CDTInteger = 'Integer';
  //CDTStructure = 'Structure';
  //CDTArray = 'Array';


  //Properties
  CCommonProperties: array[0..CPropCount_Common - 1] of TOIPropDef = (
    (Name: 'ActionName'; EditorType: etText; DataType: CDTString),
    (Name: 'Action'; EditorType: etEnumCombo; DataType: CDTEnum),    //readonly, to avoid changing the OI tree structure
    (Name: 'ActionTimeout'; EditorType: etTextWithArrow; DataType: CDTInteger),
    (Name: 'ActionCondition'; EditorType: etUserEditor; DataType: CDTString)
  );

  //Action specific properties
  CClickProperties: array[0..CPropCount_Click - 1] of TOIPropDef = (
    (Name: 'XClickPointReference'; EditorType: etEnumCombo; DataType: CDTEnum),
    (Name: 'YClickPointReference'; EditorType: etEnumCombo; DataType: CDTEnum),
    (Name: 'XClickPointVar'; EditorType: etText; DataType: CDTString),                //Description:  The provided variables must be global/screen coordinates. The $Current_Mouse_X$ var/replacement can be used as global mouse X coordinate.
    (Name: 'YClickPointVar'; EditorType: etText; DataType: CDTString),                //Description:  The provided variables must be global/screen coordinates. The $Current_Mouse_Y$ var/replacement can be used as global mouse Y coordinate.
    (Name: 'XOffset'; EditorType: etSpinText; DataType: CDTString),                   //Description:  Replacements are available.  Examples of random value: $Random(50, 100)$   $Random($MMin$, $MMax$)$
    (Name: 'YOffset'; EditorType: etSpinText; DataType: CDTString),                   //Description:  Replacements are available.  Examples of random value: $Random(50, 100)$   $Random($MMin$, $MMax$)$
    (Name: 'MouseButton'; EditorType: etEnumCombo; DataType: CDTEnum),
    (Name: 'ClickWithCtrl'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'ClickWithAlt'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'ClickWithShift'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'Count'; EditorType: etSpinText; DataType: CDTInteger),
    (Name: 'LeaveMouse'; EditorType: etBooleanCombo; DataType: CDTBool),            //Description:  When True, the mouse cursor position is not reset after running the action. It may be required if the click action will open a pop-up menu, to cause the menu to open at that location. This is also useful for debugging, to verify offsets.
    (Name: 'MoveWithoutClick'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'ClickType'; EditorType: etEnumCombo; DataType: CDTEnum),
    (Name: 'XClickPointReferenceDest'; EditorType: etEnumCombo; DataType: CDTEnum),
    (Name: 'YClickPointReferenceDest'; EditorType: etEnumCombo; DataType: CDTEnum),
    (Name: 'XClickPointVarDest'; EditorType: etText; DataType: CDTString),
    (Name: 'YClickPointVarDest'; EditorType: etText; DataType: CDTString),
    (Name: 'XOffsetDest'; EditorType: etSpinText; DataType: CDTString),
    (Name: 'YOffsetDest'; EditorType: etSpinText; DataType: CDTString),
    (Name: 'MouseWheelType'; EditorType: etEnumCombo; DataType: CDTEnum),
    (Name: 'MouseWheelAmount'; EditorType: etSpinText; DataType: CDTString),
    (Name: 'DelayAfterMovingToDestination'; EditorType: etSpinText; DataType: CDTString),
    (Name: 'DelayAfterMouseDown'; EditorType: etSpinText; DataType: CDTString),
    (Name: 'MoveDuration'; EditorType: etSpinText; DataType: CDTString),
    (Name: 'UseClipCursor'; EditorType: etBooleanCombo; DataType: CDTBool)
  );

  CExecAppProperties: array[0..CPropCount_ExecApp - 1] of TOIPropDef = (
    (Name: 'PathToApp'; EditorType: etFilePath; DataType: CDTString),                  //Description:  Full path (without quotes) to executable or other file to be open with associated application. Replacements are available.
    (Name: 'ListOfParams'; EditorType: etUserEditor; DataType: CDTString),          //string items
    (Name: 'WaitForApp'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'AppStdIn'; EditorType: etText; DataType: CDTString),                       //Description:  All #4#5 (a.k.a. 0x4:0x5) occurrences are replaced with CRLF (#13#10) before executing the application. Var/replacements are available. E.g.: $ExecAction_StdIn$  When this parameter is empty string, the executed application can run without inherited handles.
    (Name: 'CurrentDir'; EditorType: etDirPath; DataType: CDTString),                  //Description:  Application current directory.  Replacements are avaialable.  Example: $ExtractFileDir($PathToMyFile$)$
    (Name: 'UseInheritHandles'; EditorType: etEnumCombo; DataType: CDTEnum),         //Description:  Required mostly when passing data through StdIn.
    (Name: 'NoConsole'; EditorType: etBooleanCombo; DataType: CDTBool)               //Description:  When checked, console applications are not displayed in a new window.  UI applications can create and display system consoles. For those applications, this option may cause problems if checked.
  );

  CFindControlProperties: array[0..CPropCount_FindControl - 1] of TOIPropDef = (
    (Name: 'MatchCriteria'; EditorType: etNone; DataType: CDTStructure),               //structure
    (Name: 'AllowToFail'; EditorType: etBooleanCombo; DataType: CDTBool),            //Description:  When checked, the execution flow does not stop if the searched (sub)control is not found.  The "Allowed Failed" response can be used for conditional execution (call action).   When the action is allowed to fail and it fails, $LastAction_Status$ is set to "Allowed Failed".
    (Name: 'MatchText'; EditorType: etTextWithArrow; DataType: CDTString),             //Description:  Wildcards are available ("*"). Variable replacements are available. Used on matching text and BMP text.   For controls, which can have different text values (e.g. a window displaying a different title), these values can be e.g. comma separated. In that case, the text separator is a comma.  For example: a window can display "MyTitle" or "MyTitle (modified)". In that case, the "Match Text" editbox can contain "MyTitle,MyTitle (modified)", without quotes, by using the comma separator.
    (Name: 'MatchClassName'; EditorType: etTextWithArrow; DataType: CDTString),        //Description:  Wildcards are available ("*"). Variable replacements are available.  There are applications which can have one or more of their windows, registered with class name, containing a randomly generated string.
    (Name: 'MatchTextSeparator'; EditorType: etText; DataType: CDTString),
    (Name: 'MatchClassNameSeparator'; EditorType: etText; DataType: CDTString),
    (Name: 'MatchBitmapText'; EditorType: etFilePathWithArrow; DataType: CDTArray),  //array of other structure.  Count should be 0 for FindControl and >0 for FindSubControl
    (Name: 'MatchBitmapFiles'; EditorType: etFilePathWithArrow; DataType: CDTArray),  //Description:  Relative paths can be entered using the following format:' + #13#10 + '$TemplateDir$\<SomeBmp.bmp>
    (Name: 'MatchBitmapAlgorithm'; EditorType: etEnumCombo; DataType: CDTEnum),
    (Name: 'MatchBitmapAlgorithmSettings'; EditorType: etNone; DataType: CDTStructure),    //structure
    (Name: 'InitialRectangle'; EditorType: etNone; DataType: CDTStructure),                 //structure
    (Name: 'UseWholeScreen'; EditorType: etBooleanCombo; DataType: CDTBool),          //Description:  Use the whole screen as search area, if True. Use the current control as search area, if False. The search area is modified by offsets.
    (Name: 'ColorError'; EditorType: etSpinText; DataType: CDTString),                  //Description:  When matching bitmaps, which contain antialiasing pixels (see smooth text), some of those pixels will not match. The "Color Error" represents the difference between the color values for the two compared pixels, for each RGB channel. The "Color Error Count" is the allowed number of mismatching pixels. Variable replacements are available.  If at least one of the three color channels (R, G, B) mismatches by at least ColorError, it counts as an error point.
    (Name: 'AllowedColorErrorCount'; EditorType: etSpinText; DataType: CDTString),      //Description:  When matching bitmaps, which contain antialiasing pixels (see smooth text), some of those pixels will not match. The "Color Error" represents the difference between the color values for the two compared pixels, for each RGB channel. The "Color Error Count" is the allowed number of mismatching pixels. Variable replacements are available.
    (Name: 'WaitForControlToGoAway'; EditorType: etBooleanCombo; DataType: CDTBool),  //Description:     When checked, the action expects to find no control, using the current settings.
    (Name: 'StartSearchingWithCachedControl'; EditorType: etBooleanCombo; DataType: CDTBool),   //Description: When checked, the control is checked at the specified cached $My_Control_Left$ and $My_Control_Top$ var replacements, before using the search grid.  In order to cache the control coordinates, please add a SetVar action after this one, by assigning:  $My_Control_Left$ to $Control_Left$  and  $My_Control_Top$ to $Control_Top$  where $My_Control_Left$ and $My_Control_Top$ are the cached values. The "Eval before" checkboxes have to be set.  Each Find(Sub)Control action, which uses caching, will have to use its own set of $My_Control_Left$ and $My_Control_Top$ vars.  The cached values are global coordinates, so they will become invalid even for a subcontrol if the parent window is moved.
    (Name: 'CachedControlLeft'; EditorType: etText; DataType: CDTString),
    (Name: 'CachedControlTop'; EditorType: etText; DataType: CDTString),
    (Name: 'MatchPrimitiveFiles'; EditorType: etFilePathWithArrow; DataType: CDTArray),
    (Name: 'GetAllControls'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'UseFastSearch'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'FastSearchAllowedColorErrorCount'; EditorType: etText; DataType: CDTString),
    (Name: 'IgnoredColors'; EditorType: etText; DataType: CDTString),
    (Name: 'SleepySearch'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'StopSearchOnMismatch'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'ImageSource'; EditorType: etEnumCombo; DataType: CDTEnum),
    (Name: 'SourceFileName'; EditorType: etTextWithArrow; DataType: CDTString),
    (Name: 'ImageSourceFileNameLocation'; EditorType: etEnumCombo; DataType: CDTEnum),
    (Name: 'PrecisionTimeout'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'FullBackgroundImageInResult'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'MatchByHistogramSettings'; EditorType: etNone; DataType: CDTStructure),
    (Name: 'EvaluateTextCount'; EditorType: etSpinText; DataType: CDTString),
    (Name: 'CropFromScreenshot'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'ThreadCount'; EditorType: etText; DataType: CDTString)
  );

  CFindSubControlProperties: array[0..CPropCount_FindSubControl - 1] of TOIPropDef = (
    (Name: 'MatchCriteria'; EditorType: etNone; DataType: CDTStructure),               //structure
    (Name: 'AllowToFail'; EditorType: etBooleanCombo; DataType: CDTBool),            //Description:  When checked, the execution flow does not stop if the searched (sub)control is not found.  The "Allowed Failed" response can be used for conditional execution (call action).   When the action is allowed to fail and it fails, $LastAction_Status$ is set to "Allowed Failed".
    (Name: 'MatchText'; EditorType: etTextWithArrow; DataType: CDTString),             //Description:  Wildcards are available ("*"). Variable replacements are available. Used on matching text and BMP text.   For controls, which can have different text values (e.g. a window displaying a different title), these values can be e.g. comma separated. In that case, the text separator is a comma.  For example: a window can display "MyTitle" or "MyTitle (modified)". In that case, the "Match Text" editbox can contain "MyTitle,MyTitle (modified)", without quotes, by using the comma separator.
    (Name: 'MatchClassName'; EditorType: etTextWithArrow; DataType: CDTString),        //Description:  Wildcards are available ("*"). Variable replacements are available.  There are applications which can have one or more of their windows, registered with class name, containing a randomly generated string.
    (Name: 'MatchTextSeparator'; EditorType: etText; DataType: CDTString),
    (Name: 'MatchClassNameSeparator'; EditorType: etText; DataType: CDTString),
    (Name: 'MatchBitmapText'; EditorType: etFilePathWithArrow; DataType: CDTArray),  //array of other structure.  Count should be 0 for FindControl and >0 for FindSubControl
    (Name: 'MatchBitmapFiles'; EditorType: etFilePathWithArrow; DataType: CDTArray),  //Description:  Relative paths can be entered using the following format:' + #13#10 + '$TemplateDir$\<SomeBmp.bmp>
    (Name: 'MatchBitmapAlgorithm'; EditorType: etEnumCombo; DataType: CDTEnum),
    (Name: 'MatchBitmapAlgorithmSettings'; EditorType: etNone; DataType: CDTStructure),    //structure
    (Name: 'InitialRectangle'; EditorType: etNone; DataType: CDTStructure),                 //structure
    (Name: 'UseWholeScreen'; EditorType: etBooleanCombo; DataType: CDTBool),          //Description:  Use the whole screen as search area, if True. Use the current control as search area, if False. The search area is modified by offsets.
    (Name: 'ColorError'; EditorType: etSpinText; DataType: CDTString),                  //Description:  When matching bitmaps, which contain antialiasing pixels (see smooth text), some of those pixels will not match. The "Color Error" represents the difference between the color values for the two compared pixels, for each RGB channel. The "Color Error Count" is the allowed number of mismatching pixels. Variable replacements are available.  If at least one of the three color channels (R, G, B) mismatches by at least ColorError, it counts as an error point.
    (Name: 'AllowedColorErrorCount'; EditorType: etSpinText; DataType: CDTString),      //Description:  When matching bitmaps, which contain antialiasing pixels (see smooth text), some of those pixels will not match. The "Color Error" represents the difference between the color values for the two compared pixels, for each RGB channel. The "Color Error Count" is the allowed number of mismatching pixels. Variable replacements are available.
    (Name: 'WaitForControlToGoAway'; EditorType: etBooleanCombo; DataType: CDTBool),  //Description:     When checked, the action expects to find no control, using the current settings.
    (Name: 'StartSearchingWithCachedControl'; EditorType: etBooleanCombo; DataType: CDTBool),   //Description: When checked, the control is checked at the specified cached $My_Control_Left$ and $My_Control_Top$ var replacements, before using the search grid.  In order to cache the control coordinates, please add a SetVar action after this one, by assigning:  $My_Control_Left$ to $Control_Left$  and  $My_Control_Top$ to $Control_Top$  where $My_Control_Left$ and $My_Control_Top$ are the cached values. The "Eval before" checkboxes have to be set.  Each Find(Sub)Control action, which uses caching, will have to use its own set of $My_Control_Left$ and $My_Control_Top$ vars.  The cached values are global coordinates, so they will become invalid even for a subcontrol if the parent window is moved.
    (Name: 'CachedControlLeft'; EditorType: etText; DataType: CDTString),
    (Name: 'CachedControlTop'; EditorType: etText; DataType: CDTString),
    (Name: 'MatchPrimitiveFiles'; EditorType: etFilePathWithArrow; DataType: CDTArray),
    (Name: 'GetAllControls'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'UseFastSearch'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'FastSearchAllowedColorErrorCount'; EditorType: etText; DataType: CDTString),
    (Name: 'IgnoredColors'; EditorType: etText; DataType: CDTString),
    (Name: 'SleepySearch'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'StopSearchOnMismatch'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'ImageSource'; EditorType: etEnumCombo; DataType: CDTEnum),
    (Name: 'SourceFileName'; EditorType: etTextWithArrow; DataType: CDTString),
    (Name: 'ImageSourceFileNameLocation'; EditorType: etEnumCombo; DataType: CDTEnum),
    (Name: 'PrecisionTimeout'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'FullBackgroundImageInResult'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'MatchByHistogramSettings'; EditorType: etNone; DataType: CDTStructure),
    (Name: 'EvaluateTextCount'; EditorType: etSpinText; DataType: CDTString),
    (Name: 'CropFromScreenshot'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'ThreadCount'; EditorType: etText; DataType: CDTString)
  );

  {$IFDEF SubProperties}
    CFindControl_MatchCriteriaProperties: array[0..CPropCount_FindControlMatchCriteria - 1] of TOIPropDef = (
      (Name: 'WillMatchText'; EditorType: etBooleanCombo; DataType: CDTBool),
      (Name: 'WillMatchClassName'; EditorType: etBooleanCombo; DataType: CDTBool),
      (Name: 'SearchForControlMode'; EditorType: etEnumCombo; DataType: CDTEnum)
    );

    CFindSubControl_MatchCriteriaProperties: array[0..CPropCount_FindSubControlMatchCriteria - 1] of TOIPropDef = (
      (Name: 'WillMatchBitmapText'; EditorType: etBooleanCombo; DataType: CDTBool),    //Description:   When selecting FindSubControl action, only bitmaps can be matched (BMP Text or BMP Files).  A SubControl does not have a handle of its own, it is a part of a control.  The $Control_Left$, $Control_Top$, $Control_Width$, $Control_Height$, $Control_Right$, $Control_Bottom$ variables ar set with the subcontrol offset.
      (Name: 'WillMatchBitmapFiles'; EditorType: etBooleanCombo; DataType: CDTBool),
      (Name: 'WillMatchPrimitiveFiles'; EditorType: etBooleanCombo; DataType: CDTBool)//,
    );

    CFindSubControl_MatchBitmapTextProperties: array[0..CPropCount_FindSubControlMatchBitmapText - 1] of TOIPropDef = (
      (Name: 'ForegroundColor'; EditorType: etColorCombo; DataType: CDTString),
      (Name: 'BackgroundColor'; EditorType: etColorCombo; DataType: CDTString),
      (Name: 'FontName'; EditorType: etEnumComboWithBtn; DataType: CDTString),
      (Name: 'FontSize'; EditorType: etSpinText; DataType: CDTInteger),
      (Name: 'Bold'; EditorType: etBooleanCombo; DataType: CDTBool),
      (Name: 'Italic'; EditorType: etBooleanCombo; DataType: CDTBool),
      (Name: 'Underline'; EditorType: etBooleanCombo; DataType: CDTBool),
      (Name: 'StrikeOut'; EditorType: etBooleanCombo; DataType: CDTBool),
      (Name: 'FontQuality'; EditorType: etEnumCombo; DataType: CDTEnum),
      (Name: 'FontQualityUsesReplacement'; EditorType: etBooleanCombo; DataType: CDTBool),
      (Name: 'FontQualityReplacement'; EditorType: etText; DataType: CDTString),
      (Name: 'ProfileName'; EditorType: etText; DataType: CDTString),
      (Name: 'CropLeft'; EditorType: etSpinText; DataType: CDTString),
      (Name: 'CropTop'; EditorType: etSpinText; DataType: CDTString),
      (Name: 'CropRight'; EditorType: etSpinText; DataType: CDTString),
      (Name: 'CropBottom'; EditorType: etSpinText; DataType: CDTString),
      (Name: 'IgnoreBackgroundColor'; EditorType: etBooleanCombo; DataType: CDTBool)
    );

    CFindSubControl_MatchBitmapAlgorithmSettingsProperties: array[0..CPropCount_FindSubControlMatchBitmapAlgorithmSettings - 1] of TOIPropDef = (
      (Name: 'XMultipleOf'; EditorType: etSpinText; DataType: CDTInteger),
      (Name: 'YMultipleOf'; EditorType: etSpinText; DataType: CDTInteger),
      (Name: 'XOffset'; EditorType: etSpinText; DataType: CDTInteger),
      (Name: 'YOffset'; EditorType: etSpinText; DataType: CDTInteger)
    );

    CFindControl_InitialRectangleProperties: array[0..CPropCount_FindControlInitialRectangle - 1] of TOIPropDef = (
      (Name: 'Left'; EditorType: etText; DataType: CDTString),
      (Name: 'Top'; EditorType: etText; DataType: CDTString),
      (Name: 'Right'; EditorType: etText; DataType: CDTString),
      (Name: 'Bottom'; EditorType: etText; DataType: CDTString),
      (Name: 'LeftOffset'; EditorType: etSpinText; DataType: CDTString),
      (Name: 'TopOffset'; EditorType: etSpinText; DataType: CDTString),
      (Name: 'RightOffset'; EditorType: etSpinText; DataType: CDTString),
      (Name: 'BottomOffset'; EditorType: etSpinText; DataType: CDTString)
    );

    CFindSubControl_MatchByHistogramSettingsProperties: array[0..CPropCount_FindSubControlMatchByHistogramSettings - 1] of TOIPropDef = (
      (Name: 'MinPercentColorMatch'; EditorType: etSpinText; DataType: CDTString),
      (Name: 'MostSignificantColorCountInSubBmp'; EditorType: etSpinText; DataType: CDTString),
      (Name: 'MostSignificantColorCountInBackgroundBmp'; EditorType: etSpinText; DataType: CDTString)
    );
  {$ENDIF}

  CSetTextProperties: array[0..CPropCount_SetText - 1] of TOIPropDef = (   //Description:  Most edit boxes and combo boxes can be set, using the first two options.  However, depending on their usage on the target application, this approach might not be enough.  For edit boxes, the action can be configured to use key strokes.  For combo boxes, this action will have to be replaced by multiple actions, to open the box, finding text, selecting etc.
    (Name: 'Text'; EditorType: etText; DataType: CDTString),                                    //Description:  The proper control type has to be selected, for the proper API call. Uses $Control_Handle$ variable.    HTTP calls are available, as var values, using the following format: $http://<server:port>/[params]$
    (Name: 'ControlType'; EditorType: etEnumCombo; DataType: CDTEnum),                          //Description:  Uses WM_SETTEXT or CB_SELECTSTRING messages or emulates keystrokes..
    (Name: 'DelayBetweenKeyStrokes'; EditorType: etText; DataType: CDTString),
    (Name: 'Count'; EditorType: etSpinText; DataType: CDTString)
  );

  CCallTemplateProperties: array[0..CPropCount_CallTemplate - 1] of TOIPropDef = (
    (Name: 'TemplateFileName'; EditorType: etTextWithArrow; DataType: CDTString),           //Description:  Replacements are available    //Description[arrow button]:   Templates from the local dir
    (Name: 'ListOfCustomVarsAndValues'; EditorType: etUserEditor; DataType: CDTString),
    (Name: 'EvaluateBeforeCalling'; EditorType: etBooleanCombo; DataType: CDTBool),           //Description:  If unchecked, the values are passed as strings.
    (Name: 'CallTemplateLoop'; EditorType: etNone; DataType: CDTStructure)       //structure       //Description:  What does not work, is closing subtemplates when remote debugging.  So, do not click the stop button when remote debugging CallTemplate actions with loops.
  );

  {$IFDEF SubProperties}
    CCallTemplate_CallTemplateLoopProperties: array[0..CPropCount_CallTemplateLoop - 1] of TOIPropDef = (
      (Name: 'Enabled'; EditorType: etBooleanCombo; DataType: CDTBool),
      (Name: 'Counter'; EditorType: etText; DataType: CDTString),                                //Description:  Replacements are available
      (Name: 'InitValue'; EditorType: etText; DataType: CDTString),                              //Description:  Replacements are available
      (Name: 'EndValue'; EditorType: etText; DataType: CDTString),                               //Description:  Replacements are available.  If the expression evaluator can't properly parse this string, please use a SetVar action to break down the expression into smaller pieces.  For example, $Diff($ItemCount($RemoteVars$)$,1)$ can be split into the following SetVar items (both set to be evaluated):   $RemoteVarsCount$ = $ItemCount($RemoteVars$)$  $LastItemIndex$ = $Diff($RemoteVarsCount$,1)$
      (Name: 'Direction'; EditorType: etEnumCombo; DataType: CDTEnum),                         //Description:  Loop direction can be:  Inc - when the counter should be incremented.  Dec - when the counter should be decremented.  Auto - when the init and the end values can be anything, so the counter direction has to be evaluated before starting the loop.
      (Name: 'BreakCondition'; EditorType: etUserEditor; DataType: CDTString),
      (Name: 'EvalBreakPosition'; EditorType: etEnumCombo; DataType: CDTEnum)                  //Description:  The "Break" call can be made, before or after the actual "CallTemplate" call.
    );
  {$ENDIF}

  CSleepProperties: array[0..CPropCount_Sleep - 1] of TOIPropDef = (
    (Name: 'Value'; EditorType: etText; DataType: CDTString)
  );

  CSetVarProperties: array[0..CPropCount_SetVar - 1] of TOIPropDef = (
    (Name: 'ListOfVarNamesValuesAndEvalBefore'; EditorType: etUserEditor; DataType: CDTString), //structure   (no sub properties). Using string, to allow the property to be checked when edited by EditTemplate action.
    (Name: 'FailOnException'; EditorType: etBooleanCombo; DataType: CDTBool)
  );

  CWindowOperationsProperties: array[0..CPropCount_WindowOperations - 1] of TOIPropDef = (
    (Name: 'Operation'; EditorType: etEnumCombo; DataType: CDTEnum),
    (Name: 'NewX'; EditorType: etSpinText; DataType: CDTString),
    (Name: 'NewY'; EditorType: etSpinText; DataType: CDTString),
    (Name: 'NewWidth'; EditorType: etSpinText; DataType: CDTString),
    (Name: 'NewHeight'; EditorType: etSpinText; DataType: CDTString),
    (Name: 'NewPositionEnabled'; EditorType: etBooleanCombo; DataType: CDTBool),
    (Name: 'NewSizeEnabled'; EditorType: etBooleanCombo; DataType: CDTBool)
  );

  CLoadSetVarFromFileProperties: array[0..CPropCount_LoadSetVarFromFile - 1] of TOIPropDef = (
    (Name: 'FileName'; EditorType: etTextWithArrow; DataType: CDTString),
    (Name: 'SetVarActionName'; EditorType: etTextWithArrow; DataType: CDTString)
  );

  CSaveSetVarToFileProperties: array[0..CPropCount_SaveSetVarToFile - 1] of TOIPropDef = (
    (Name: 'FileName'; EditorType: etTextWithArrow; DataType: CDTString),
    (Name: 'SetVarActionName'; EditorType: etTextWithArrow; DataType: CDTString)
  );

  CPluginProperties: array[0..CPropCount_Plugin - 1] of TOIPropDef = (
    (Name: 'FileName'; EditorType: etTextWithArrow; DataType: CDTString)
  );

  CEditTemplateProperties: array[0..CPropCount_EditTemplate - 1] of TOIPropDef = (
    (Name: 'Operation'; EditorType: etEnumCombo; DataType: CDTEnum),
    (Name: 'WhichTemplate'; EditorType: etEnumCombo; DataType: CDTEnum),
    (Name: 'TemplateFileName'; EditorType: etTextWithArrow; DataType: CDTString),
    (Name: 'EditedActionName'; EditorType: etTextWithArrow; DataType: CDTString),
    (Name: 'EditedActionType'; EditorType: etEnumCombo; DataType: CDTString),
    (Name: 'EditedActionCondition'; EditorType: etUserEditor; DataType: CDTString),
    (Name: 'EditedActionTimeout'; EditorType: etTextWithArrow; DataType: CDTInteger),
    (Name: 'NewActionName'; EditorType: etTextWithArrow; DataType: CDTString),
    (Name: 'ListOfEditedProperties'; EditorType: etText {etNone}; DataType: CDTString),
    (Name: 'ListOfEnabledProperties'; EditorType: etText {etNone}; DataType: CDTString),
    (Name: 'ShouldSaveTemplate'; EditorType: etBooleanCombo; DataType: CDTBool)
  );

type
  TArrayOfProperties = array[0..0] of TOIPropDef;
  PArrayOfProperties = ^TArrayOfProperties;
  TArrayOfPropertiesArr = array[0..Ord(High(TClkAction))] of PArrayOfProperties;

  TGetActionValueStrFunc = function(AAction: PClkActionRec; APropertyIndex: Integer): string;
  TGetActionValueStrFuncArr = array[TClkAction] of TGetActionValueStrFunc;
  TGetFindControlValueStrFuncArr = array[0..CPropCount_FindControl - 1] of TGetActionValueStrFunc;
  TGetFindSubControlValueStrFuncArr = array[0..CPropCount_FindSubControl - 1] of TGetActionValueStrFunc;
  TGetCallTemplateValueStrFuncArr = array[0..CPropCount_CallTemplate - 1] of TGetActionValueStrFunc;

  TSetActionValueStrProc = procedure(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  TSetActionValueStrProcArr = array[TClkAction] of TSetActionValueStrProc;
  TSetFindControlValueStrProcArr = array[0..CPropCount_FindControl - 1] of TSetActionValueStrProc;
  TSetFindSubControlValueStrProcArr = array[0..CPropCount_FindSubControl - 1] of TSetActionValueStrProc;
  TSetCallTemplateValueStrProcArr = array[0..CPropCount_CallTemplate - 1] of TSetActionValueStrProc;

  TPropHintFunc = function: string;
  TPropHintFuncArr = array[0..0] of TPropHintFunc;
  PPropHintFuncArr = ^TPropHintFuncArr;
  TPropHintFuncActionArr = array[TClkAction] of PPropHintFuncArr;

  TArrayOfEnumCounts = array[0..0] of Integer;
  PArrayOfEnumCounts  = ^TArrayOfEnumCounts;

  TArrayOfString = array[0..0] of string;
  PArrayOfString = ^TArrayOfString;
  TArrayOfEnumStrings = array[0..0] of PArrayOfString;
  PArrayOfEnumStrings = ^TArrayOfEnumStrings;

const
  CMainProperties: TArrayOfPropertiesArr = (
    @CClickProperties,                      //0
    @CExecAppProperties,                    //1
    @CFindControlProperties,                //2
    @CFindSubControlProperties,             //3
    @CSetTextProperties,                    //4
    @CCallTemplateProperties,               //5
    @CSleepProperties,                      //6
    @CSetVarProperties,                     //7
    @CWindowOperationsProperties,           //8
    @CLoadSetVarFromFileProperties,         //9
    @CSaveSetVarToFileProperties,           //10
    @CPluginProperties,                     //11
    @CEditTemplateProperties                //12
  );


function GetActionValueStr_Action(AAction: PClkActionRec; APropertyIndex: Integer): string;

function GetActionValueStr_Click(AAction: PClkActionRec; APropertyIndex: Integer): string;
function GetActionValueStr_ExecApp(AAction: PClkActionRec; APropertyIndex: Integer): string;
function GetActionValueStr_FindControl(AAction: PClkActionRec; APropertyIndex: Integer): string;
function GetActionValueStr_FindSubControl(AAction: PClkActionRec; APropertyIndex: Integer): string;
function GetActionValueStr_SetText(AAction: PClkActionRec; APropertyIndex: Integer): string;
function GetActionValueStr_CallTemplate(AAction: PClkActionRec; APropertyIndex: Integer): string;
function GetActionValueStr_Sleep(AAction: PClkActionRec; APropertyIndex: Integer): string;
function GetActionValueStr_SetVar(AAction: PClkActionRec; APropertyIndex: Integer): string;
function GetActionValueStr_WindowOperations(AAction: PClkActionRec; APropertyIndex: Integer): string;
function GetActionValueStr_LoadSetVarFromFile(AAction: PClkActionRec; APropertyIndex: Integer): string;
function GetActionValueStr_SaveSetVarToFile(AAction: PClkActionRec; APropertyIndex: Integer): string;
function GetActionValueStr_Plugin(AAction: PClkActionRec; APropertyIndex: Integer): string;
function GetActionValueStr_EditTemplate(AAction: PClkActionRec; APropertyIndex: Integer): string;

{$IFDEF SubProperties}
  function GetActionValueStr_FindControl_MatchCriteria(AAction: PClkActionRec; APropertyIndex: Integer): string;
  function GetActionValueStr_FindSubControl_MatchCriteria(AAction: PClkActionRec; APropertyIndex: Integer): string;
  function GetActionValueStr_FindSubControl_MatchBitmapText(AAction: PClkActionRec; APropertyIndex: Integer): string;
  function GetActionValueStr_FindSubControl_MatchBitmapAlgorithmSettings(AAction: PClkActionRec; APropertyIndex: Integer): string;
  function GetActionValueStr_FindControl_InitialRectangle(AAction: PClkActionRec; APropertyIndex: Integer): string;
  function GetActionValueStr_FindSubControl_InitialRectangle(AAction: PClkActionRec; APropertyIndex: Integer): string;
  function GetActionValueStr_FindSubControl_MatchByHistogramSettings(AAction: PClkActionRec; APropertyIndex: Integer): string;

  function GetActionValueStr_CallTemplate_CallTemplateLoop(AAction: PClkActionRec; APropertyIndex: Integer): string;
{$ENDIF}


procedure SetActionValueStr_Action(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);

procedure SetActionValueStr_Click(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
procedure SetActionValueStr_ExecApp(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
procedure SetActionValueStr_FindControl(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
procedure SetActionValueStr_FindSubControl(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
procedure SetActionValueStr_SetText(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
procedure SetActionValueStr_CallTemplate(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
procedure SetActionValueStr_Sleep(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
procedure SetActionValueStr_SetVar(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
procedure SetActionValueStr_WindowOperations(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
procedure SetActionValueStr_LoadSetVarFromFile(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
procedure SetActionValueStr_SaveSetVarToFile(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
procedure SetActionValueStr_Plugin(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
procedure SetActionValueStr_EditTemplate(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);

{$IFDEF SubProperties}
  procedure SetActionValueStr_FindControl_MatchCriteria(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  procedure SetActionValueStr_FindSubControl_MatchCriteria(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  procedure SetActionValueStr_FindSubControl_MatchBitmapText(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  procedure SetActionValueStr_FindSubControl_MatchBitmapAlgorithmSettings(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  procedure SetActionValueStr_FindControl_InitialRectangle(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  procedure SetActionValueStr_FindSubControl_InitialRectangle(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  procedure SetActionValueStr_FindSubControl_MatchByHistogramSettings(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);

  procedure SetActionValueStr_CallTemplate_CallTemplateLoop(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
{$ENDIF}


const
  CMainGetActionValueStrFunctions: TGetActionValueStrFuncArr = (
    GetActionValueStr_Click,
    GetActionValueStr_ExecApp,
    GetActionValueStr_FindControl,
    GetActionValueStr_FindSubControl,
    GetActionValueStr_SetText,
    GetActionValueStr_CallTemplate,
    GetActionValueStr_Sleep,
    GetActionValueStr_SetVar,
    GetActionValueStr_WindowOperations,
    GetActionValueStr_LoadSetVarFromFile,
    GetActionValueStr_SaveSetVarToFile,
    GetActionValueStr_Plugin,
    GetActionValueStr_EditTemplate
  );

  CMainSetActionValueStrFunctions: TSetActionValueStrProcArr = (
    SetActionValueStr_Click,
    SetActionValueStr_ExecApp,
    SetActionValueStr_FindControl,
    SetActionValueStr_FindSubControl,
    SetActionValueStr_SetText,
    SetActionValueStr_CallTemplate,
    SetActionValueStr_Sleep,
    SetActionValueStr_SetVar,
    SetActionValueStr_WindowOperations,
    SetActionValueStr_LoadSetVarFromFile,
    SetActionValueStr_SaveSetVarToFile,
    SetActionValueStr_Plugin,
    SetActionValueStr_EditTemplate
  );

  CFindControlGetActionValueStrFunctions: TGetFindControlValueStrFuncArr = (
    GetActionValueStr_FindControl_MatchCriteria,
    nil, //AllowToFail
    nil, //MatchText
    nil, //MatchClassName
    nil, //MatchTextSeparator
    nil, //MatchClassNameSeparator
    nil, //MatchBitmapText,
    nil, //MatchBitmapFiles
    nil, //MatchBitmapAlgorithm
    nil, //MatchBitmapAlgorithmSettings,
    GetActionValueStr_FindControl_InitialRectangle,
    nil, //UseWholeScreen
    nil, //ColorError
    nil, //AllowedColorErrorCount
    nil, //WaitForControlToGoAway
    nil, //StartSearchingWithCachedControl
    nil, //CachedControlLeft
    nil, //CachedControlTop
    nil, //MatchPrimitiveFiles
    nil, //GetAllControls
    nil, //UseFastSearch
    nil, //FastSearchAllowedColorErrorCount
    nil, //IgnoredColors
    nil, //SleepySearch
    nil, //StopSearchOnMismatch
    nil, //ImageSource
    nil, //SourceFileName
    nil, //ImageSourceFileNameLocation
    nil, //PrecisionTimeout
    nil, //FullBackgroundImageInResult
    nil, //MatchByHistogramSettings
    nil, //EvaluateTextCount
    nil, //CropFromScreenshot
    nil  //ThreadCount
  );

  CFindSubControlGetActionValueStrFunctions: TGetFindSubControlValueStrFuncArr = (
    GetActionValueStr_FindSubControl_MatchCriteria,
    nil, //AllowToFail
    nil, //MatchText
    nil, //MatchClassName
    nil, //MatchTextSeparator
    nil, //MatchClassNameSeparator
    GetActionValueStr_FindSubControl_MatchBitmapText,
    nil, //MatchBitmapFiles
    nil, //MatchBitmapAlgorithm
    GetActionValueStr_FindSubControl_MatchBitmapAlgorithmSettings,
    GetActionValueStr_FindSubControl_InitialRectangle,
    nil, //UseWholeScreen
    nil, //ColorError
    nil, //AllowedColorErrorCount
    nil, //WaitForControlToGoAway
    nil, //StartSearchingWithCachedControl
    nil, //CachedControlLeft
    nil, //CachedControlTop
    nil, //MatchPrimitiveFiles
    nil, //GetAllControls
    nil, //UseFastSearch
    nil, //FastSearchAllowedColorErrorCount
    nil, //IgnoredColors
    nil, //SleepySearch
    nil, //StopSearchOnMismatch
    nil, //ImageSource
    nil, //SourceFileName
    nil, //ImageSourceFileNameLocation
    nil, //PrecisionTimeout
    nil, //FullBackgroundImageInResult
    GetActionValueStr_FindSubControl_MatchByHistogramSettings,  //MatchByHistogramSettings
    nil, //EvaluateTextCount
    nil, //CropFromScreenshot
    nil  //ThreadCount
  );

  CCallTemplateGetActionValueStrFunctions: TGetCallTemplateValueStrFuncArr = (
    nil, //TemplateFileName
    nil, //ListOfCustomVarsAndValues
    nil, //EvaluateBeforeCalling
    GetActionValueStr_CallTemplate_CallTemplateLoop
  );


//Enums
  CActionEnumCounts: array[0..CPropCount_Common - 1] of Integer = (
    0,
    Ord(High(TClkAction)) + 1,
    0,
    0
  );

  CClickEnumCounts: array[0..CPropCount_Click - 1] of Integer = (
    Ord(High(TXClickPointReference)) + 1,
    Ord(High(TYClickPointReference)) + 1,
    0, //XClickPointVar: string;
    0, //YClickPointVar: string;
    0, //XOffset,
    0, //YOffset;
    Ord(High(TMouseButton)) + 1,
    0, //ClickWithCtrl: Boolean;
    0, //ClickWithAlt: Boolean;
    0, //ClickWithShift: Boolean;
    0, //Count: Integer;
    0, //LeaveMouse: Boolean;
    0, //MoveWithoutClick: Boolean;
    CClickType_Count, //ClickType: Integer;
    Ord(High(TXClickPointReference)) + 1,
    Ord(High(TYClickPointReference)) + 1,
    0, //XClickPointVarDest: string;
    0, //YClickPointVarDest: string;
    0, //XOffsetDest
    0, //YOffsetDest
    Ord(High(TMouseWheelType)) + 1,
    0, //MouseWheelAmount: string;
    0, //DelayAfterMovingToDestination: string;
    0, //DelayAfterMouseDown: string;
    0, //MoveDuration: string;
    0  //UseClipCursor
  );

  CExecAppEnumCounts: array[0..CPropCount_ExecApp - 1] of Integer = (
    0, //PathToApp: string;
    0, //ListOfParams: string;
    0, //WaitForApp: Boolean;
    0, //AppStdIn: string;
    0, //CurrentDir: string;
    Ord(High(TExecAppUseInheritHandles)) + 1,
    0 //NoConsole: Boolean;
  );

  CFindControlEnumCounts: array[0..CPropCount_FindControl - 1] of Integer = (
    0, //MatchCriteria: TClkFindControlMatchCriteria;
    0, //AllowToFail: Boolean;
    0, //MatchText: string;
    0, //MatchClassName: string;
    0, //MatchTextSeparator: string;
    0, //MatchClassNameSeparator: string;
    0, //MatchBitmapText: TClkFindControlMatchBitmapTextArr;
    0, //MatchBitmapFiles: string; //ListOfStrings
    Ord(High(TMatchBitmapAlgorithm)) + 1,
    0, //MatchBitmapAlgorithmSettings: TMatchBitmapAlgorithmSettings;
    0, //InitialRectangle: TRectString;
    0, //UseWholeScreen: Boolean;
    0, //ColorError: string;  //string, to allow var replacements
    0, //AllowedColorErrorCount: string;  //Number of pixels allowed to mismatch
    0, //WaitForControlToGoAway: Boolean;
    0, //StartSearchingWithCachedControl: Boolean;
    0, //CachedControlLeft: string;
    0, //CachedControlTop: string;
    0, //MatchPrimitiveFiles
    0, //GetAllControls: Boolean;
    0, //UseFastSearch: Boolean;
    0, //FastSearchAllowedColorErrorCount: Boolean;
    0, //IgnoredColors: string;
    0, //StopSearchOnMismatch: Boolean;
    0, //SleepySearch: Boolean;
    Ord(High(TImageSource)) + 1, //ImageSource: TImageSource;
    0, //SourceFileName: string;
    Ord(High(TImageSourceFileNameLocation)) + 1,  //ImageSourceFileNameLocation: TImageSourceFileNameLocation;
    0, //PrecisionTimeout: Boolean;
    0, //FullBackgroundImageInResult
    0, //MatchByHistogramSettings: TMatchByHistogramSettings;
    0, //EvaluateTextCount
    0, //CropFromScreenshot
    0  //ThreadCount
  );

  CFindSubControlEnumCounts: array[0..CPropCount_FindSubControl - 1] of Integer = (
    0, //MatchCriteria: TClkFindSubControlMatchCriteria;
    0, //AllowToFail: Boolean;
    0, //MatchText: string;
    0, //MatchClassName: string;
    0, //MatchTextSeparator: string;
    0, //MatchClassNameSeparator: string;
    0, //MatchBitmapText: TClkFindControlMatchBitmapTextArr;
    0, //MatchBitmapFiles: string; //ListOfStrings
    Ord(High(TMatchBitmapAlgorithm)) + 1,
    0, //MatchBitmapAlgorithmSettings: TMatchBitmapAlgorithmSettings;
    0, //InitialRectangle: TRectString;
    0, //UseWholeScreen: Boolean;
    0, //ColorError: string;  //string, to allow var replacements
    0, //AllowedColorErrorCount: string;  //Number of pixels allowed to mismatch
    0, //WaitForControlToGoAway: Boolean;
    0, //StartSearchingWithCachedControl: Boolean;
    0, //CachedControlLeft: string;
    0, //CachedControlTop: string;
    0, //MatchPrimitiveFiles
    0, //GetAllControls: Boolean;
    0, //UseFastSearch: Boolean;
    0, //FastSearchAllowedColorErrorCount: Boolean;
    0, //IgnoredColors: string;
    0, //StopSearchOnMismatch: Boolean;
    0, //SleepySearch: Boolean;
    Ord(High(TImageSource)) + 1, //ImageSource: TImageSource;
    0, //SourceFileName: string;
    Ord(High(TImageSourceFileNameLocation)) + 1,  //ImageSourceFileNameLocation: TImageSourceFileNameLocation;
    0, //PrecisionTimeout: Boolean;
    0, //FullBackgroundImageInResult
    0, //MatchByHistogramSettings: TMatchByHistogramSettings;
    0, //EvaluateTextCount
    0, //CropFromScreenshot
    0  //ThreadCount
  );


  CSetTextEnumCounts: array[0..CPropCount_SetText - 1] of Integer = (
    0, //Text: string;
    Ord(High(TClkSetTextControlType)) + 1,
    0, //DelayBetweenKeyStrokes: string;
    0  //Count: string;
  );

  CCallTemplateEnumCounts: array[0..CPropCount_CallTemplate - 1] of Integer = (
    0, //TemplateFileName: string;
    0, //ListOfCustomVarsAndValues: string;
    0, //EvaluateBeforeCalling: Boolean;
    0  //CallTemplateLoop: TClkCallTemplateLoop;
  );

  CSleepEnumCounts: array[0..CPropCount_Sleep - 1] of Integer = (
    0  //Value: string;
  );

  CSetVarEnumCounts: array[0..CPropCount_SetVar - 1] of Integer = (
    0,  //ListOfVarNamesValuesAndEvalBefore
    0   //FailOnException
  );

  CWindowOperationsEnumCounts: array[0..CPropCount_WindowOperations - 1] of Integer = (
    Ord(High(TWindowOperation)) + 1,
    0, //NewX,
    0, //NewY,
    0, //NewWidth,
    0, //NewHeight
    0, //NewPositionEnabled,
    0  //NewSizeEnabled: Boolean;
  );

  CLoadSetVarFromFileEnumCounts: array[0..CPropCount_LoadSetVarFromFile - 1] of Integer = (
    0, //FileName
    0  //SetVarActionName
  );

  CSaveSetVarToFileEnumCounts: array[0..CPropCount_SaveSetVarToFile - 1] of Integer = (
    0, //FileName
    0  //SetVarActionName
  );

  CPluginEnumCounts: array[0..CPropCount_Plugin - 1] of Integer = (
    0  //FileName
  );

  CEditTemplateEnumCounts: array[0..CPropCount_EditTemplate - 1] of Integer = (
    Ord(High(TEditTemplateOperation)) + 1,
    Ord(High(TEditTemplateWhichTemplate)) + 1,
    0,  //TemplateFileName
    0,  //EditedActionName,
    Ord(High(TClkAction)) + 1,
    0, //NewActionName
    0,
    0,
    0,
    0,
    0
  );


  CPropEnumCounts: array[TClkAction] of PArrayOfEnumCounts = (
    @CClickEnumCounts,
    @CExecAppEnumCounts,
    @CFindControlEnumCounts,
    @CFindSubControlEnumCounts,
    @CSetTextEnumCounts,
    @CCallTemplateEnumCounts,
    @CSleepEnumCounts,
    @CSetVarEnumCounts,
    @CWindowOperationsEnumCounts,
    @CLoadSetVarFromFileEnumCounts,
    @CSaveSetVarToFileEnumCounts,
    @CPluginEnumCounts,
    @CEditTemplateEnumCounts
  );


  {$IFDEF SubProperties}
    CFindControl_MatchCriteriaEnumCounts: array[0..CPropCount_FindControlMatchCriteria - 1] of Integer = (
      0, //WillMatchText: Boolean;
      0, //WillMatchClassName: Boolean;
      Ord(High(TSearchForControlMode)) + 1
    );

    CFindSubControl_MatchCriteriaEnumCounts: array[0..CPropCount_FindSubControlMatchCriteria - 1] of Integer = (
      0, //WillMatchBitmapText: Boolean;
      0, //WillMatchBitmapFiles: Boolean;
      0//, //WillMatchPrimitiveFiles: Boolean;
    );

    CFindSubControl_MatchBitmapTextEnumCounts: array[0..CPropCount_FindSubControlMatchBitmapText - 1] of Integer = (
      0, //ForegroundColor: string;
      0, //BackgroundColor: string;
      0, //FontName: string;
      0, //FontSize: Integer;
      0, //Bold: Boolean;
      0, //Italic: Boolean;
      0, //Underline: Boolean;
      0, //StrikeOut: Boolean;
      Ord(High(TFontQuality)) + 1,
      0, //FontQualityUsesReplacement: Boolean;
      0, //FontQualityReplacement: string;
      0, //ProfileName: string;
      0, //CropLeft: string;
      0, //CropTop: string;
      0, //CropRight: string;
      0, //CropBottom: string;
      0  //IgnoreBackgroundColor: Boolean;
    );

    CCallTemplate_CallTemplateLoopEnumCounts: array[0..CPropCount_CallTemplateLoop - 1] of Integer = (
      0, //Enabled: Boolean; //When False, the CallTemplate action is executed once, as before. Else, it may be executed or not, based on loop settings.
      0, //Counter: string;
      0, //InitValue: string;
      0, //EndValue: string;
      Ord(High(TLoopDirection)) + 1,
      0, //BreakCondition: string;
      Ord(High(TLoopEvalBreakPosition)) + 1
    );
  {$ENDIF}


  CActionEnumStrings: array[0..CPropCount_Common - 1] of PArrayOfString = (
    nil,
    @CClkActionStr,
    nil,
    nil
  );

  CClickEnumStrings: array[0..CPropCount_Click - 1] of PArrayOfString = (
    @CXClickPointReferenceStr,
    @CYClickPointReferenceStr,
    nil, //XClickPointVar: string;
    nil, //YClickPointVar: string;
    nil, //XOffset,
    nil, //YOffset;
    @CMouseButtonStr,
    nil, //ClickWithCtrl: Boolean;
    nil, //ClickWithAlt: Boolean;
    nil, //ClickWithShift: Boolean;
    nil, //Count: Integer;
    nil, //LeaveMouse: Boolean;
    nil, //MoveWithoutClick: Boolean;
    @CClickTypeStr, //ClickType: Integer;
    @CXClickPointReferenceStr,
    @CYClickPointReferenceStr,
    nil, //XClickPointVarDest: string;
    nil, //YClickPointVarDest: string;
    nil, //XOffsetDest
    nil, //YOffsetDest
    @CMouseWheelTypeStr, //MouseWheelType: TMouseWheelType;
    nil, //MouseWheelAmount
    nil, //DelayAfterMovingToDestination
    nil, //DelayAfterMouseDown
    nil, //MoveDuration
    nil  //UseClipCursor
  );

  CExecAppEnumStrings: array[0..CPropCount_ExecApp - 1] of PArrayOfString = (
    nil, //PathToApp: string;
    nil, //ListOfParams: string;
    nil, //WaitForApp: Boolean;
    nil, //AppStdIn: string;
    nil, //CurrentDir: string;
    @CExecAppUseInheritHandlesStr,
    nil //NoConsole: Boolean;
  );

  CFindControlEnumStrings: array[0..CPropCount_FindControl - 1] of PArrayOfString = (
    nil, //MatchCriteria: TClkFindControlMatchCriteria;
    nil, //AllowToFail: Boolean;
    nil, //MatchText: string;
    nil, //MatchClassName: string;
    nil, //MatchTextSeparator: string;
    nil, //MatchClassNameSeparator: string;
    nil, //MatchBitmapText: TClkFindControlMatchBitmapTextArr;
    nil, //MatchBitmapFiles: string; //ListOfStrings
    @CMatchBitmapAlgorithmStr,
    nil, //MatchBitmapAlgorithmSettings: TMatchBitmapAlgorithmSettings;
    nil, //InitialRectangle: TRectString;
    nil, //UseWholeScreen: Boolean;
    nil, //ColorError: string;  //string, to allow var replacements
    nil, //AllowedColorErrorCount: string;  //Number of pixels allowed to mismatch
    nil, //WaitForControlToGoAway: Boolean;
    nil, //StartSearchingWithCachedControl: Boolean;
    nil, //CachedControlLeft: string;
    nil, //CachedControlTop: string;
    nil, //Primitives
    nil, //GetAllControls
    nil, //UseFastSearch
    nil, //FastSearchAllowedColorErrorCount
    nil, //IgnoredColors
    nil, //SleepySearch
    nil, //StopSearchOnMismatch
    @CImageSourceStr,
    nil, //SourceFileName
    @CImageSourceFileNameLocationStr,
    nil, //PrecisionTimeout
    nil, //FullBackgroundImageInResult
    nil, //MatchByHistogramSettings
    nil, //EvaluateTextCount
    nil, //CropFromScreenshot
    nil  //ThreadCount
  );

  CFindSubControlEnumStrings: array[0..CPropCount_FindSubControl - 1] of PArrayOfString = (
    nil, //MatchCriteria: TClkFindSubControlMatchCriteria;
    nil, //AllowToFail: Boolean;
    nil, //MatchText: string;
    nil, //MatchClassName: string;
    nil, //MatchTextSeparator: string;
    nil, //MatchClassNameSeparator: string;
    nil, //MatchBitmapText: TClkFindControlMatchBitmapTextArr;
    nil, //MatchBitmapFiles: string; //ListOfStrings
    @CMatchBitmapAlgorithmStr,
    nil, //MatchBitmapAlgorithmSettings: TMatchBitmapAlgorithmSettings;
    nil, //InitialRectangle: TRectString;
    nil, //UseWholeScreen: Boolean;
    nil, //ColorError: string;  //string, to allow var replacements
    nil, //AllowedColorErrorCount: string;  //Number of pixels allowed to mismatch
    nil, //WaitForControlToGoAway: Boolean;
    nil, //StartSearchingWithCachedControl: Boolean;
    nil, //CachedControlLeft: string;
    nil, //CachedControlTop: string;
    nil, //Primitives
    nil, //GetAllControls
    nil, //UseFastSearch
    nil, //FastSearchAllowedColorErrorCount
    nil, //IgnoredColors
    nil, //SleepySearch
    nil, //StopSearchOnMismatch
    @CImageSourceStr,
    nil, //SourceFileName
    @CImageSourceFileNameLocationStr,
    nil, //PrecisionTimeout
    nil, //FullBackgroundImageInResult
    nil, //MatchByHistogramSettings
    nil, //EvaluateTextCount
    nil, //CropFromScreenshot
    nil  //ThreadCount
  );

  CSetTextEnumStrings: array[0..CPropCount_SetText - 1] of PArrayOfString = (
    nil, //Text: string;
    @CClkSetTextControlTypeStr,
    nil, //DelayBetweenKeyStrokes: string;
    nil  //Count: string;
  );

  CCallTemplateEnumStrings: array[0..CPropCount_CallTemplate - 1] of PArrayOfString = (
    nil, //TemplateFileName: string;
    nil, //ListOfCustomVarsAndValues: string;
    nil, //EvaluateBeforeCalling: Boolean;
    nil  //CallTemplateLoop: TClkCallTemplateLoop;
  );

  CSleepEnumStrings: array[0..CPropCount_Sleep - 1] of PArrayOfString = (
    nil  //Value: string;
  );

  CSetVarEnumStrings: array[0..CPropCount_SetVar - 1] of PArrayOfString = (
    nil,  //ListOfVarNamesValuesAndEvalBefore
    nil   //FailOnException
  );

  CWindowOperationsEnumStrings: array[0..CPropCount_WindowOperations - 1] of PArrayOfString = (
    @CWindowOperationStr,
    nil, //NewX,
    nil, //NewY,
    nil, //NewWidth,
    nil, //NewHeight
    nil, //NewPositionEnabled,
    nil  //NewSizeEnabled: Boolean;
  );

  CLoadSetVarFromFileEnumStrings: array[0..CPropCount_LoadSetVarFromFile - 1] of PArrayOfString = (
    nil, //FileName
    nil  //SetVarActionName
  );

  CSaveSetVarToFileEnumStrings: array[0..CPropCount_SaveSetVarToFile - 1] of PArrayOfString = (
    nil, //FileName
    nil  //SetVarActionName
  );

  CPluginEnumStrings: array[0..CPropCount_Plugin - 1] of PArrayOfString = (
    nil  //FileName
  );

  CEditTemplateEnumStrings: array[0..CPropCount_EditTemplate - 1] of PArrayOfString = (
    @CEditTemplateOperationStr,
    @CEditTemplateWhichTemplateStr,
    nil,  //TemplateFileName
    nil,  //EditedActionName
    @CClkActionStr,
    nil,  //NewActionName
    nil,
    nil,
    nil,
    nil,
    nil
  );


  CPropEnumStrings: array[TClkAction] of PArrayOfEnumStrings = (
    @CClickEnumStrings,
    @CExecAppEnumStrings,
    @CFindControlEnumStrings,
    @CFindSubControlEnumStrings,
    @CSetTextEnumStrings,
    @CCallTemplateEnumStrings,
    @CSleepEnumStrings,
    @CSetVarEnumStrings,
    @CWindowOperationsEnumStrings,
    @CLoadSetVarFromFileEnumStrings,
    @CSaveSetVarToFileEnumStrings,
    @CPluginEnumStrings,
    @CEditTemplateEnumStrings
  );

  {$IFDEF SubProperties}
    CFindControl_MatchCriteriaEnumStrings: array[0..CPropCount_FindControlMatchCriteria - 1] of PArrayOfString = (
      nil, //WillMatchText: Boolean;
      nil, //WillMatchClassName: Boolean;
      @CSearchForControlModeStr
    );

    CFindSubControl_MatchCriteriaEnumStrings: array[0..CPropCount_FindSubControlMatchCriteria - 1] of PArrayOfString = (
      nil, //WillMatchBitmapText: Boolean;
      nil, //WillMatchBitmapFiles: Boolean;
      nil//, //WillMatchPrimitiveFiles: Boolean;
    );

    CFindSubControl_MatchBitmapTextEnumStrings: array[0..CPropCount_FindSubControlMatchBitmapText - 1] of PArrayOfString = (
      nil, //ForegroundColor: string;
      nil, //BackgroundColor: string;
      nil, //FontName: string;
      nil, //FontSize: Integer;
      nil, //Bold: Boolean;
      nil, //Italic: Boolean;
      nil, //Underline: Boolean;
      nil, //StrikeOut: Boolean;
      @CFontQualityStr,
      nil, //FontQualityUsesReplacement: Boolean;
      nil, //FontQualityReplacement: string;
      nil, //ProfileName: string;
      nil, //CropLeft: string;
      nil, //CropTop: string;
      nil, //CropRight: string;
      nil, //CropBottom: string;
      nil  //IgnoreBackgroundColor: Boolean;
    );

    CCallTemplate_CallTemplateLoopEnumStrings: array[0..CPropCount_CallTemplateLoop - 1] of PArrayOfString = (
      nil, //Enabled: Boolean; //When False, the CallTemplate action is executed once, as before. Else, it may be executed or not, based on loop settings.
      nil, //Counter: string;
      nil, //InitValue: string;
      nil, //EndValue: string;
      @CLoopDirectionStr,
      nil, //BreakCondition: string;
      @CLoopEvalBreakPositionStr
    );
  {$ENDIF}


/////////////////////////////////////

//IsExp =  property, which can be expanded in OI, i.e. a structure or an array of items/structures.
//0 = cannot be expanded
//1 - can always be expanded, like a structure
//2 - can be expanded, but not always (e.g. an array of files)
  CActionIsExp: array[0..CPropCount_Common - 1] of Integer = (
    0,
    0, //TClkAction
    0,
    0
  );

  CClickIsExp: array[0..CPropCount_Click - 1] of Integer = (
    0, //TXClickPointReference
    0, //TYClickPointReference
    0, //XClickPointVar: string;
    0, //YClickPointVar: string;
    0, //XOffset,
    0, //YOffset;
    0, //TMouseButton
    0, //ClickWithCtrl: Boolean;
    0, //ClickWithAlt: Boolean;
    0, //ClickWithShift: Boolean;
    0, //Count: Integer;
    0, //LeaveMouse: Boolean;
    0, //MoveWithoutClick: Boolean;
    0, //CClickType_Count, //ClickType: Integer;
    0, //TXClickPointReference
    0, //TYClickPointReference
    0, //XClickPointVarDest: string;
    0, //YClickPointVarDest: string;
    0, //XOffsetDest
    0, //YOffsetDest
    0, //TMouseWheelType
    0, //MouseWheelAmount: string;
    0, //DelayAfterMovingToDestination: string;
    0, //DelayAfterMouseDown: string;
    0, //MoveDuration: string;
    0  //UseClipCursor
  );

  CExecAppIsExp: array[0..CPropCount_ExecApp - 1] of Integer = (
    0, //PathToApp: string;
    0, //ListOfParams: string;
    0, //WaitForApp: Boolean;
    0, //AppStdIn: string;
    0, //CurrentDir: string;
    0, //TExecAppUseInheritHandles
    0 //NoConsole: Boolean;
  );

  CFindControlIsExp: array[0..CPropCount_FindControl - 1] of Integer = (
    1, //MatchCriteria: TClkFindControlMatchCriteria;
    0, //AllowToFail: Boolean;
    0, //MatchText: string;
    0, //MatchClassName: string;
    0, //MatchTextSeparator: string;
    0, //MatchClassNameSeparator: string;
    2, //MatchBitmapText: TClkFindControlMatchBitmapTextArr;   //althoug unusable without items, the OI allows removing all items
    2, //MatchBitmapFiles: string; //ListOfStrings
    0, //TMatchBitmapAlgorithm
    1, //MatchBitmapAlgorithmSettings: TMatchBitmapAlgorithmSettings;
    1, //InitialRectangle: TRectString;
    0, //UseWholeScreen: Boolean;
    0, //ColorError: string;  //string, to allow var replacements
    0, //AllowedColorErrorCount: string;  //Number of pixels allowed to mismatch
    0, //WaitForControlToGoAway: Boolean;
    0, //StartSearchingWithCachedControl: Boolean;
    0, //CachedControlLeft: string;
    0, //CachedControlTop: string;
    2, //MatchPrimitiveFiles
    0, //GetAllControls: Boolean;
    0, //UseFastSearch: Boolean;
    0, //FastSearchAllowedColorErrorCount: Boolean;
    0, //IgnoredColors: string;
    0, //StopSearchOnMismatch: Boolean;
    0, //SleepySearch: Boolean;
    0, //ImageSource: TImageSource;
    0, //SourceFileName: string;
    0,  //ImageSourceFileNameLocation: TImageSourceFileNameLocation;
    0, //PrecisionTimeout: Boolean;
    0, //FullBackgroundImageInResult
    1, //MatchByHistogramSettings: TMatchByHistogramSettings;
    0, //EvaluateTextCount
    0, //CropFromScreenshot
    0  //ThreadCount
  );

  CFindSubControlIsExp: array[0..CPropCount_FindSubControl - 1] of Integer = (
    1, //MatchCriteria: TClkFindSubControlMatchCriteria;
    0, //AllowToFail: Boolean;
    0, //MatchText: string;
    0, //MatchClassName: string;
    0, //MatchTextSeparator: string;
    0, //MatchClassNameSeparator: string;
    2, //MatchBitmapText: TClkFindControlMatchBitmapTextArr;   //althoug unusable without items, the OI allows removing all items
    2, //MatchBitmapFiles: string; //ListOfStrings
    0, //TMatchBitmapAlgorithm
    1, //MatchBitmapAlgorithmSettings: TMatchBitmapAlgorithmSettings;
    1, //InitialRectangle: TRectString;
    0, //UseWholeScreen: Boolean;
    0, //ColorError: string;  //string, to allow var replacements
    0, //AllowedColorErrorCount: string;  //Number of pixels allowed to mismatch
    0, //WaitForControlToGoAway: Boolean;
    0, //StartSearchingWithCachedControl: Boolean;
    0, //CachedControlLeft: string;
    0, //CachedControlTop: string;
    2, //MatchPrimitiveFiles
    0, //GetAllControls: Boolean;
    0, //UseFastSearch: Boolean;
    0, //FastSearchAllowedColorErrorCount: Boolean;
    0, //IgnoredColors: string;
    0, //StopSearchOnMismatch: Boolean;
    0, //SleepySearch: Boolean;
    0, //ImageSource: TImageSource;
    0, //SourceFileName: string;
    0,  //ImageSourceFileNameLocation: TImageSourceFileNameLocation;
    0, //PrecisionTimeout: Boolean;
    0, //FullBackgroundImageInResult
    1, //MatchByHistogramSettings: TMatchByHistogramSettings;
    0, //EvaluateTextCount
    0, //CropFromScreenshot
    0  //ThreadCount
  );

  CSetTextIsExp: array[0..CPropCount_SetText - 1] of Integer = (
    0, //Text: string;
    0, //TClkSetTextControlType
    0, //DelayBetweenKeyStrokes: string;
    0  //Count: string;
  );

  CCallTemplateIsExp: array[0..CPropCount_CallTemplate - 1] of Integer = (
    0, //TemplateFileName: string;
    0, //ListOfCustomVarsAndValues: string;
    0, //EvaluateBeforeCalling: Boolean;
    1  //CallTemplateLoop: TClkCallTemplateLoop;
  );

  CSleepIsExp: array[0..CPropCount_Sleep - 1] of Integer = (
    0  //Value: string;
  );

  CSetVarIsExp: array[0..CPropCount_SetVar - 1] of Integer = (
    0,  //ListOfVarNamesValuesAndEvalBefore
    0   //FailOnException
  );

  CWindowOperationsIsExp: array[0..CPropCount_WindowOperations - 1] of Integer = (
    0, //TWindowOperation
    0, //NewX,
    0, //NewY,
    0, //NewWidth,
    0, //NewHeight
    0, //NewPositionEnabled,
    0  //NewSizeEnabled: Boolean;
  );

  CLoadSetVarFromFileIsExp: array[0..CPropCount_LoadSetVarFromFile - 1] of Integer = (
    0, //FileName
    0  //SetVarActionName
  );

  CSaveSetVarToFileIsExp: array[0..CPropCount_SaveSetVarToFile - 1] of Integer = (
    0, //FileName
    0  //SetVarActionName
  );

  CPluginIsExp: array[0..CPropCount_Plugin - 1] of Integer = (
    0  //FileName
  );

  CEditTemplateIsExp: array[0..CPropCount_EditTemplate - 1] of Integer = (
    0,  //TEditTemplateOperation
    0,  //TEditTemplateWhichTemplate
    0,  //TemplateFileName
    0,  //EditedActionName,
    0,  //TClkAction
    0,  //NewActionName
    0,  //EditedActionCondition
    0,  //EditedActionTimeout
    0,  //ListOfEditedProperties
    0,  //ListOfEnabledProperties
    0   //ShouldSaveTemplate
  );


  CPropIsExp: array[TClkAction] of PArrayOfEnumCounts = (
    @CClickIsExp,
    @CExecAppIsExp,
    @CFindControlIsExp,
    @CFindSubControlIsExp,
    @CSetTextIsExp,
    @CCallTemplateIsExp,
    @CSleepIsExp,
    @CSetVarIsExp,
    @CWindowOperationsIsExp,
    @CLoadSetVarFromFileIsExp,
    @CSaveSetVarToFileIsExp,
    @CPluginIsExp,
    @CEditTemplateIsExp
  );


function GetPropertyHintNoHint: string;

function GetPropertyHint_Click_XClickPointVar: string;
function GetPropertyHint_Click_YClickPointVar: string;
function GetPropertyHint_Click_XOffset: string;
function GetPropertyHint_Click_YOffset: string;
function GetPropertyHint_Click_LeaveMouse: string;
function GetPropertyHint_Click_DelayAfterMovingToDestination: string;
function GetPropertyHint_Click_DelayAfterMouseDown: string;
function GetPropertyHint_Click_MouseWheelAmount: string;
function GetPropertyHint_Click_MoveDuration: string;
function GetPropertyHint_Click_UseClipCursor: string;

function GetPropertyHint_ExecApp_PathToApp: string;
function GetPropertyHint_ExecApp_AppStdIn: string;
function GetPropertyHint_ExecApp_CurrentDir: string;
function GetPropertyHint_ExecApp_UseInheritHandles: string;
function GetPropertyHint_ExecApp_NoConsole: string;

function GetPropertyHint_FindControl_AllowToFail: string;
function GetPropertyHint_FindControl_MatchText: string;
function GetPropertyHint_FindControl_MatchClassName: string;
function GetPropertyHint_FindControl_MatchBitmapText: string;
function GetPropertyHint_FindControl_MatchBitmapFiles: string;
function GetPropertyHint_FindControl_MatchBitmapAlgorithm: string;
function GetPropertyHint_FindControl_UseWholeScreen: string;
function GetPropertyHint_FindControl_ColorError: string;
function GetPropertyHint_FindControl_AllowedColorErrorCount: string;
function GetPropertyHint_FindControl_WaitForControlToGoAway: string;
function GetPropertyHint_FindControl_StartSearchingWithCachedControl: string;
function GetPropertyHint_FindControl_CachedControlLeftTop: string;
function GetPropertyHint_FindControl_MatchPrimitiveFiles: string;
function GetPropertyHint_FindControl_GetAllControls: string;
function GetPropertyHint_FindControl_UseFastSearch: string;
function GetPropertyHint_FindControl_FastSearchAllowedColorErrorCount: string;
function GetPropertyHint_FindControl_IgnoredColors: string;
function GetPropertyHint_FindControl_SleepySearch: string;
function GetPropertyHint_FindControl_StopSearchOnMismatch: string;
function GetPropertyHint_FindControl_ImageSource: string;
function GetPropertyHint_FindControl_SourceFileName: string;
function GetPropertyHint_FindControl_ImageSourceFileNameLocation: string;
function GetPropertyHint_FindControl_PrecisionTimeout: string;
function GetPropertyHint_FindControl_FullBackgroundImageInResult: string;
function GetPropertyHint_FindControl_MatchByHistogramSettings: string;
function GetPropertyHint_FindControl_EvaluateTextCount: string;
function GetPropertyHint_FindControl_CropFromScreenshot: string;
function GetPropertyHint_FindControl_ThreadCount: string;

{$IFDEF SubProperties}
  function GetPropertyHint_FindControl_MatchCriteria_WillMatchText: string;
  function GetPropertyHint_FindControl_MatchCriteria_WillMatchBitmapText: string;
  function GetPropertyHint_FindControl_MatchCriteria_SearchForControlMode: string;
{$ENDIF}

{$IFDEF SubProperties}
  function GetPropertyHint_FindControl_InitialRectangle_Left: string;
  function GetPropertyHint_FindControl_InitialRectangle_Top: string;
  function GetPropertyHint_FindControl_InitialRectangle_Right: string;
  function GetPropertyHint_FindControl_InitialRectangle_Bottom: string;
  function GetPropertyHint_FindControl_InitialRectangle_Offsets: string;
{$ENDIF}

{$IFDEF SubProperties}
  function GetPropertyHint_FindSubControl_MatchByHistogramSettings_MinPercentColorMatch: string;
  function GetPropertyHint_FindSubControl_MatchByHistogramSettings_MostSignificantColorCountInSubBmp: string;
  function GetPropertyHint_FindSubControl_MatchByHistogramSettings_MostSignificantColorCountInBackgroundBmp: string;
{$ENDIF}


function GetPropertyHint_SetText: string;

{$IFDEF SubProperties}
  function GetPropertyHint_SetText_Text: string;
  function GetPropertyHint_SetText_ControlType: string;
  function GetPropertyHint_DelayBetweenKeyStrokes: string;
  function GetPropertyHint_Count: string;
{$ENDIF}


function GetPropertyHint_CallTemplate_TemplateFileName: string;
function GetPropertyHint_CallTemplate_EvaluateBeforeCalling: string;
function GetPropertyHint_CallTemplate_CallTemplateLoop: string;


{$IFDEF SubProperties}
  function GetPropertyHint_CallTemplate_CallTemplateLoop_Counter: string;
  function GetPropertyHint_CallTemplate_CallTemplateLoop_InitValue: string;
  function GetPropertyHint_CallTemplate_CallTemplateLoop_EndValue: string;
  function GetPropertyHint_CallTemplate_CallTemplateLoop_Direction: string;
  function GetPropertyHint_CallTemplate_CallTemplateLoop_EvalBreakPosition: string;
{$ENDIF}


function GetPropertyHint_WindowOperations_NewXY: string;
function GetPropertyHint_WindowOperations_NewWidthHeight: string;

function GetPropertyHint_LoadSetVarFromFile_FileName: string;
function GetPropertyHint_LoadSetVarFromFile_SetVarActionName: string;
function GetPropertyHint_SaveSetVarToFile_FileName: string;
function GetPropertyHint_SaveSetVarToFile_SetVarActionName: string;
function GetPropertyHint_Plugin_FileName: string;

function GetPropertyHint_Sleep_Value: string;

function GetPropertyHint_SetVar_FailOnException: string;

function GetPropertyHint_EditTemplate_Operation: string;
function GetPropertyHint_EditTemplate_WhichTemplate: string;
function GetPropertyHint_EditTemplate_TemplateFileName: string;
function GetPropertyHint_EditTemplate_EditedActionName: string;
function GetPropertyHint_EditTemplate_EditedActionType: string;
function GetPropertyHint_EditTemplate_EditedActionCondition: string;
function GetPropertyHint_EditTemplate_EditedActionTimeout: string;
function GetPropertyHint_EditTemplate_NewActionName: string;
function GetPropertyHint_EditTemplate_ListOfEditedProperties: string;
function GetPropertyHint_EditTemplate_ListOfEnabledProperties: string;
function GetPropertyHint_EditTemplate_ShouldSaveTemplate: string;

const
  CGetPropertyHint_Click: array[0..CPropCount_Click - 1] of TPropHintFunc = (
    @GetPropertyHintNoHint, //XClickPointReference: TXClickPointReference;
    @GetPropertyHintNoHint, //YClickPointReference: TYClickPointReference;
    @GetPropertyHint_Click_XClickPointVar,
    @GetPropertyHint_Click_YClickPointVar,
    @GetPropertyHint_Click_XOffset,
    @GetPropertyHint_Click_YOffset,
    @GetPropertyHintNoHint, //MouseButton: TMouseButton;
    @GetPropertyHintNoHint, // ClickWithCtrl: Boolean;
    @GetPropertyHintNoHint, // ClickWithAlt: Boolean;
    @GetPropertyHintNoHint, // ClickWithShift: Boolean;
    @GetPropertyHintNoHint, // Count: Integer;
    @GetPropertyHint_Click_LeaveMouse,
    @GetPropertyHintNoHint, // MoveWithoutClick: Boolean;
    @GetPropertyHintNoHint, // ClickType: Integer;    //see CClickType_Click and CClickType_Drag
    @GetPropertyHintNoHint, // XClickPointReferenceDest: TXClickPointReference;
    @GetPropertyHintNoHint, // YClickPointReferenceDest: TYClickPointReference;
    @GetPropertyHintNoHint, // XClickPointVarDest: string;
    @GetPropertyHintNoHint, // YClickPointVarDest: string;
    @GetPropertyHintNoHint, // XOffsetDest
    @GetPropertyHintNoHint, // YOffsetDest
    @GetPropertyHintNoHint, // MouseWheelType: TMouseWheelType;
    @GetPropertyHint_Click_MouseWheelAmount, // MouseWheelAmount: string;
    @GetPropertyHint_Click_DelayAfterMovingToDestination,
    @GetPropertyHint_Click_DelayAfterMouseDown,
    @GetPropertyHint_Click_MoveDuration,
    @GetPropertyHint_Click_UseClipCursor
  );


  CGetPropertyHint_ExecApp: array[0..CPropCount_ExecApp - 1] of TPropHintFunc = (
    @GetPropertyHint_ExecApp_PathToApp, // PathToApp: string;
    @GetPropertyHintNoHint, // ListOfParams: string;
    @GetPropertyHintNoHint, // WaitForApp: Boolean;
    @GetPropertyHint_ExecApp_AppStdIn, // AppStdIn: string;
    @GetPropertyHint_ExecApp_CurrentDir, // CurrentDir: string;
    @GetPropertyHint_ExecApp_UseInheritHandles, // UseInheritHandles: TExecAppUseInheritHandles;
    @GetPropertyHint_ExecApp_NoConsole // NoConsole: Boolean;
  );


  CGetPropertyHint_FindControl: array[0..CPropCount_FindControl - 1] of TPropHintFunc = (
    @GetPropertyHintNoHint, // MatchCriteria: TClkFindControlMatchCriteria;
    @GetPropertyHint_FindControl_AllowToFail, // AllowToFail: Boolean;
    @GetPropertyHint_FindControl_MatchText, // MatchText: string;
    @GetPropertyHint_FindControl_MatchClassName, // MatchClassName: string;
    @GetPropertyHintNoHint, // MatchTextSeparator: string;
    @GetPropertyHintNoHint, // MatchClassNameSeparator: string;
    @GetPropertyHint_FindControl_MatchBitmapText, // MatchBitmapText: TClkFindControlMatchBitmapTextArr;
    @GetPropertyHint_FindControl_MatchBitmapFiles, // MatchBitmapFiles: string; //ListOfStrings
    @GetPropertyHint_FindControl_MatchBitmapAlgorithm, // MatchBitmapAlgorithm: TMatchBitmapAlgorithm;
    @GetPropertyHintNoHint, // MatchBitmapAlgorithmSettings: TMatchBitmapAlgorithmSettings;
    @GetPropertyHintNoHint, // InitialRectangle: TRectString;
    @GetPropertyHint_FindControl_UseWholeScreen, // UseWholeScreen: Boolean;
    @GetPropertyHint_FindControl_ColorError, // ColorError: string;  //string, to allow var replacements
    @GetPropertyHint_FindControl_AllowedColorErrorCount, // AllowedColorErrorCount: string;  //Number of pixels allowed to mismatch
    @GetPropertyHint_FindControl_WaitForControlToGoAway, // WaitForControlToGoAway: Boolean;
    @GetPropertyHint_FindControl_StartSearchingWithCachedControl, // StartSearchingWithCachedControl: Boolean;
    @GetPropertyHint_FindControl_CachedControlLeftTop, // CachedControlLeft: string;
    @GetPropertyHint_FindControl_CachedControlLeftTop, // CachedControlTop: string;
    @GetPropertyHint_FindControl_MatchPrimitiveFiles, // MatchPrimitiveFiles: string; //ListOfStrings
    @GetPropertyHint_FindControl_GetAllControls, // GetAllControls: Boolean;
    @GetPropertyHint_FindControl_UseFastSearch, // UseFastSearch: Boolean;
    @GetPropertyHint_FindControl_FastSearchAllowedColorErrorCount, // FastSearchAllowedColorErrorCount: string;
    @GetPropertyHint_FindControl_IgnoredColors, // IgnoredColors: string;
    @GetPropertyHint_FindControl_SleepySearch, // SleepySearch: Boolean;
    @GetPropertyHint_FindControl_StopSearchOnMismatch, // StopSearchOnMismatch: Boolean;
    @GetPropertyHint_FindControl_ImageSource, // ImageSource: TImageSource;
    @GetPropertyHint_FindControl_SourceFileName, //SourceFileName: string;
    @GetPropertyHint_FindControl_ImageSourceFileNameLocation, //ImageSourceFileNameLocation: TImageSourceFileLocation;
    @GetPropertyHint_FindControl_PrecisionTimeout, //PrecisionTimeout: Boolean
    @GetPropertyHint_FindControl_FullBackgroundImageInResult,  //FullBackgroundImageInResult: Boolean
    @GetPropertyHint_FindControl_MatchByHistogramSettings, //MatchByHistogramSettings: TMatchByHistogramSettings;
    @GetPropertyHint_FindControl_EvaluateTextCount, //EvaluateTextCount: string;
    @GetPropertyHint_FindControl_CropFromScreenshot, //CropFromScreenshot: Boolean;
    @GetPropertyHint_FindControl_ThreadCount //ThreadCount: string;
  );


  CGetPropertyHint_SetText: array[0..CPropCount_SetText - 1] of TPropHintFunc = (
    @GetPropertyHint_SetText_Text, // Text: string;
    @GetPropertyHint_SetText_ControlType,  // ControlType: TClkSetTextControlType;
    @GetPropertyHint_DelayBetweenKeyStrokes, // DelayBetweenKeyStrokes: string;
    @GetPropertyHint_Count // Count: string;
  );


  CGetPropertyHint_CallTemplate: array[0..CPropCount_CallTemplate - 1] of TPropHintFunc = (
    @GetPropertyHint_CallTemplate_TemplateFileName, // TemplateFileName: string;
    @GetPropertyHintNoHint, // ListOfCustomVarsAndValues: string;
    @GetPropertyHint_CallTemplate_EvaluateBeforeCalling, // EvaluateBeforeCalling: Boolean;
    @GetPropertyHint_CallTemplate_CallTemplateLoop  // CallTemplateLoop: TClkCallTemplateLoop;
  );


  CGetPropertyHint_Sleep: array[0..CPropCount_Sleep - 1] of TPropHintFunc = (
    @GetPropertyHint_Sleep_Value //Value: string;  // [ms]
  );


  CGetPropertyHint_SetVar: array[0..CPropCount_SetVar - 1] of TPropHintFunc = (
    @GetPropertyHintNoHint, //ListOfVarNamesValuesAndEvalBefore
    @GetPropertyHint_SetVar_FailOnException  //FailOnException
  );


  CGetPropertyHint_WindowOperations: array[0..CPropCount_WindowOperations - 1] of TPropHintFunc = (
    @GetPropertyHintNoHint, // Operation: TWindowOperation;
    @GetPropertyHint_WindowOperations_NewXY, // NewX,
    @GetPropertyHint_WindowOperations_NewXY, // NewY
    @GetPropertyHint_WindowOperations_NewWidthHeight, // NewWidth
    @GetPropertyHint_WindowOperations_NewWidthHeight, // NewHeight
    @GetPropertyHintNoHint, // NewPositionEnabled,
    @GetPropertyHintNoHint  // NewSizeEnabled
  );


  CGetPropertyHint_LoadSetVarFromFile: array[0..CPropCount_LoadSetVarFromFile - 1] of TPropHintFunc = (
    @GetPropertyHint_LoadSetVarFromFile_FileName, // FileName,
    @GetPropertyHint_LoadSetVarFromFile_SetVarActionName  // SetVarActionName
  );

  CGetPropertyHint_SaveSetVarToFile: array[0..CPropCount_SaveSetVarToFile - 1] of TPropHintFunc = (
    @GetPropertyHint_SaveSetVarToFile_FileName, // FileName,
    @GetPropertyHint_SaveSetVarToFile_SetVarActionName  // SetVarActionName
  );

  CGetPropertyHint_Plugin: array[0..CPropCount_Plugin - 1] of TPropHintFunc = (
    @GetPropertyHint_Plugin_FileName  // FileName,
  );

  CGetPropertyHint_EditTemplate: array[0..CPropCount_EditTemplate - 1] of TPropHintFunc = (
    @GetPropertyHint_EditTemplate_Operation,  // Operation,
    @GetPropertyHint_EditTemplate_WhichTemplate,  // WhichTemplate,
    @GetPropertyHint_EditTemplate_TemplateFileName,  // TemplateFileName,
    @GetPropertyHint_EditTemplate_EditedActionName,
    @GetPropertyHint_EditTemplate_EditedActionType,
    @GetPropertyHint_EditTemplate_EditedActionCondition,
    @GetPropertyHint_EditTemplate_EditedActionTimeout,
    @GetPropertyHint_EditTemplate_NewActionName,
    @GetPropertyHint_EditTemplate_ListOfEditedProperties,
    @GetPropertyHint_EditTemplate_ListOfEnabledProperties,
    @GetPropertyHint_EditTemplate_ShouldSaveTemplate
  );



  CGetPropertyHint_Actions: TPropHintFuncActionArr = (
    @CGetPropertyHint_Click,
    @CGetPropertyHint_ExecApp,
    @CGetPropertyHint_FindControl,
    @CGetPropertyHint_FindControl,
    @CGetPropertyHint_SetText,
    @CGetPropertyHint_CallTemplate,
    @CGetPropertyHint_Sleep,
    @CGetPropertyHint_SetVar,
    @CGetPropertyHint_WindowOperations,
    @CGetPropertyHint_LoadSetVarFromFile,
    @CGetPropertyHint_SaveSetVarToFile,
    @CGetPropertyHint_Plugin,
    @CGetPropertyHint_EditTemplate
  );


  CGetPropertyHint_FindControlMatchCriteria_Items: array[0..CPropCount_FindControlMatchCriteria - 1] of TPropHintFunc = (
    @GetPropertyHint_FindControl_MatchCriteria_WillMatchText, //WillMatchText: Boolean;
    @GetPropertyHintNoHint, //WillMatchClassName: Boolean;
    @GetPropertyHint_FindControl_MatchCriteria_SearchForControlMode //SearchForControlMode: TSearchForControlMode;
  );

  CGetPropertyHint_FindSubControlMatchCriteria_Items: array[0..CPropCount_FindSubControlMatchCriteria - 1] of TPropHintFunc = (
    @GetPropertyHint_FindControl_MatchCriteria_WillMatchBitmapText, //WillMatchBitmapText: Boolean;
    @GetPropertyHintNoHint, //WillMatchBitmapFiles: Boolean;
    @GetPropertyHintNoHint//, //WillMatchPrimitiveFiles: Boolean;
  );

  CGetPropertyHint_FindControlInitialRectangle_Items: array[0..CPropCount_FindControlInitialRectangle - 1] of TPropHintFunc = (
    @GetPropertyHint_FindControl_InitialRectangle_Left, //Left
    @GetPropertyHint_FindControl_InitialRectangle_Top, //Top
    @GetPropertyHint_FindControl_InitialRectangle_Right, //Right
    @GetPropertyHint_FindControl_InitialRectangle_Bottom, //Bottom
    @GetPropertyHint_FindControl_InitialRectangle_Offsets, //LeftOffset
    @GetPropertyHint_FindControl_InitialRectangle_Offsets, //TopOffset
    @GetPropertyHint_FindControl_InitialRectangle_Offsets, //RightOffset
    @GetPropertyHint_FindControl_InitialRectangle_Offsets  //BottomOffset
  );

  CGetPropertyHint_FindSubControlMatchByHistogramSettings_Items: array[0..CPropCount_FindSubControlMatchByHistogramSettings - 1] of TPropHintFunc = (
    @GetPropertyHint_FindSubControl_MatchByHistogramSettings_MinPercentColorMatch, //MinPercentColorMatch
    @GetPropertyHint_FindSubControl_MatchByHistogramSettings_MostSignificantColorCountInSubBmp,  //MostSignificantColorCountInSubBmp
    @GetPropertyHint_FindSubControl_MatchByHistogramSettings_MostSignificantColorCountInBackgroundBmp  //MostSignificantColorCountInBackgroundBmp
  );


  CGetPropertyHint_CallTemplateLoop_Items: array[0..CPropCount_CallTemplateLoop - 1] of TPropHintFunc = (
    @GetPropertyHintNoHint, //Enabled: Boolean;
    @GetPropertyHint_CallTemplate_CallTemplateLoop_Counter, //Counter: string;
    @GetPropertyHint_CallTemplate_CallTemplateLoop_InitValue, // InitValue: string;
    @GetPropertyHint_CallTemplate_CallTemplateLoop_EndValue, // EndValue: string;
    @GetPropertyHint_CallTemplate_CallTemplateLoop_Direction, // Direction: TLoopDirection;
    @GetPropertyHintNoHint, // BreakCondition: string;
    @GetPropertyHint_CallTemplate_CallTemplateLoop_EvalBreakPosition  // EvalBreakPosition: TLoopEvalBreakPosition;
  );


function FontQuality_AsStringToValue(AFontQualityAsString: string): TFontQuality;


implementation


uses
  Math;


function GetActionValueStr_Action(AAction: PClkActionRec; APropertyIndex: Integer): string;
begin
  if Integer(AAction^.ActionOptions.Action) = CClkUnsetAction then
  begin
    Result := '<Not set>';
    Exit;
  end;

  case APropertyIndex of
    0: Result := AAction^.ActionOptions.ActionName;
    1: Result := CClkActionStr[AAction^.ActionOptions.Action];
    2: Result := IntToStr(AAction^.ActionOptions.ActionTimeout);
    3: Result := AAction^.ActionOptions.ActionCondition;
    else
      Result := 'unknown';
  end;
end;


function GetActionValueStr_Click(AAction: PClkActionRec; APropertyIndex: Integer): string;
begin
  case APropertyIndex of
    0: Result := CXClickPointReferenceStr[AAction^.ClickOptions.XClickPointReference];
    1: Result := CYClickPointReferenceStr[AAction^.ClickOptions.YClickPointReference];
    2: Result := AAction^.ClickOptions.XClickPointVar;
    3: Result := AAction^.ClickOptions.YClickPointVar;
    4: Result := AAction^.ClickOptions.XOffset;
    5: Result := AAction^.ClickOptions.YOffset;
    6: Result := CMouseButtonStr[AAction^.ClickOptions.MouseButton];
    7: Result := BoolToStr(AAction^.ClickOptions.ClickWithCtrl, True);
    8: Result := BoolToStr(AAction^.ClickOptions.ClickWithAlt, True);
    9: Result := BoolToStr(AAction^.ClickOptions.ClickWithShift, True);
    //10: Result := BoolToStr(AAction^.ClickOptions.ClickWithDoubleClick, True);  //deprecated
    10: Result := IntToStr(AAction^.ClickOptions.Count);
    11: Result := BoolToStr(AAction^.ClickOptions.LeaveMouse, True);
    12: Result := BoolToStr(AAction^.ClickOptions.MoveWithoutClick, True);
    13: Result := CClickTypeStr[AAction^.ClickOptions.ClickType];
    14: Result := CXClickPointReferenceStr[AAction^.ClickOptions.XClickPointReferenceDest];
    15: Result := CYClickPointReferenceStr[AAction^.ClickOptions.YClickPointReferenceDest];
    16: Result := AAction^.ClickOptions.XClickPointVarDest;
    17: Result := AAction^.ClickOptions.YClickPointVarDest;
    18: Result := AAction^.ClickOptions.XOffsetDest;
    19: Result := AAction^.ClickOptions.YOffsetDest;
    20: Result := CMouseWheelTypeStr[AAction^.ClickOptions.MouseWheelType];
    21: Result := AAction^.ClickOptions.MouseWheelAmount;
    22: Result := AAction^.ClickOptions.DelayAfterMovingToDestination;
    23: Result := AAction^.ClickOptions.DelayAfterMouseDown;
    24: Result := AAction^.ClickOptions.MoveDuration;
    25: Result := BoolToStr(AAction^.ClickOptions.UseClipCursor, True);
    else
      Result := 'unknown';
  end;
end;


function GetActionValueStr_ExecApp(AAction: PClkActionRec; APropertyIndex: Integer): string;
begin
  case APropertyIndex of
    0: Result := AAction^.ExecAppOptions.PathToApp;
    1: Result := AAction^.ExecAppOptions.ListOfParams;
    2: Result := BoolToStr(AAction^.ExecAppOptions.WaitForApp, True);
    3: Result := AAction^.ExecAppOptions.AppStdIn;
    4: Result := AAction^.ExecAppOptions.CurrentDir;
    5: Result := CExecAppUseInheritHandlesStr[AAction^.ExecAppOptions.UseInheritHandles];
    6: Result := BoolToStr(AAction^.ExecAppOptions.NoConsole, True);
    else
      Result := 'unknown';
  end;
end;


function GetActionValueStr_FindControl(AAction: PClkActionRec; APropertyIndex: Integer): string;
begin
  case APropertyIndex of
    0:
    begin
      Result := '';  //MatchCriteria     Replaced by icons.
      //if AAction^.FindControlOptions.MatchCriteria.WillMatchText then
      //  Result := Result + '[Text] ';
      //
      //if AAction^.FindControlOptions.MatchCriteria.WillMatchClassName then
      //  Result := Result + '[Class] ';
    end;
    1: Result := BoolToStr(AAction^.FindControlOptions.AllowToFail, True);
    2: Result := AAction^.FindControlOptions.MatchText;
    3: Result := AAction^.FindControlOptions.MatchClassName;
    4: Result := AAction^.FindControlOptions.MatchTextSeparator;
    5: Result := AAction^.FindControlOptions.MatchClassNameSeparator;
    6: Result := '';  //MatchBitmapText
    7: Result := '';  //AAction^.FindControlOptions.MatchBitmapFiles; //ListOfStrings
    8: Result := '';  //CMatchBitmapAlgorithmStr[AAction^.FindControlOptions.MatchBitmapAlgorithm];
    9: Result := '';  //MatchBitmapAlgorithmSettings
    10: Result := '';  //InitialRectangle
    11: Result := BoolToStr(AAction^.FindControlOptions.UseWholeScreen, True);
    12: Result := '';  //AAction^.FindControlOptions.ColorError;  //string, to allow var replacements
    13: Result := '';  //AAction^.FindControlOptions.AllowedColorErrorCount;  //Number of pixels allowed to mismatch
    14: Result := BoolToStr(AAction^.FindControlOptions.WaitForControlToGoAway, True);
    15: Result := BoolToStr(AAction^.FindControlOptions.StartSearchingWithCachedControl, True);
    16: Result := AAction^.FindControlOptions.CachedControlLeft;
    17: Result := AAction^.FindControlOptions.CachedControlTop;
    18: Result := '';  //AAction^.FindControlOptions.MatchPrimitiveFiles; //ListOfStrings
    19: Result := BoolToStr(AAction^.FindControlOptions.GetAllControls, True);
    20: Result := '';  //BoolToStr(AAction^.FindControlOptions.UseFastSearch, True);
    21: Result := '';  //AAction^.FindControlOptions.FastSearchAllowedColorErrorCount;
    22: Result := '';  //AAction^.FindControlOptions.IgnoredColors;
    23: Result := '';  //BoolToStr(AAction^.FindControlOptions.SleepySearch, True);
    24: Result := '';  //BoolToStr(AAction^.FindControlOptions.StopSearchOnMismatch, True);
    25: Result := '';  //CImageSourceStr[AAction^.FindControlOptions.ImageSource];
    26: Result := '';  //AAction^.FindControlOptions.SourceFileName;
    27: Result := '';  //CImageSourceFileNameLocationStr[AAction^.FindControlOptions.ImageSourceFileNameLocation];
    28: Result := BoolToStr(AAction^.FindControlOptions.PrecisionTimeout, True);
    29: Result := '';  //BoolToStr(AAction^.FindControlOptions.FullBackgroundImageInResult, True);
    30: Result := '';
    31: Result := AAction^.FindControlOptions.EvaluateTextCount;
    32: Result := '';  //BoolToStr(AAction^.FindControlOptions.CropFromScreenshot, True);
    33: Result := '';  //AAction^.FindControlOptions.ThreadCount;
    else
      Result := 'unknown';
  end;
end;


function GetActionValueStr_FindSubControl(AAction: PClkActionRec; APropertyIndex: Integer): string;
begin
  case APropertyIndex of
    0:
    begin
      Result := '';  //MatchCriteria     Replaced by icons.
      //if AAction^.FindSubControlOptions.MatchCriteria.WillMatchBitmapText then
      //  Result := Result + '[bmptxt] ';
      //
      //if AAction^.FindSubControlOptions.MatchCriteria.WillMatchBitmapFiles then
      //  Result := Result + '[.bmp] ';
      //
      //if AAction^.FindSubControlOptions.MatchCriteria.WillMatchPrimitiveFiles then
      //  Result := Result + '[.pmtv] ';
    end;
    1: Result := BoolToStr(AAction^.FindSubControlOptions.AllowToFail, True);
    2: Result := AAction^.FindSubControlOptions.MatchText;
    3: Result := AAction^.FindSubControlOptions.MatchClassName;
    4: Result := AAction^.FindSubControlOptions.MatchTextSeparator;
    5: Result := AAction^.FindSubControlOptions.MatchClassNameSeparator;
    6: Result := '';  //MatchBitmapText
    7: Result := AAction^.FindSubControlOptions.MatchBitmapFiles; //ListOfStrings
    8: Result := CMatchBitmapAlgorithmStr[AAction^.FindSubControlOptions.MatchBitmapAlgorithm];
    9: Result := '';  //MatchBitmapAlgorithmSettings
    10: Result := '';  //InitialRectangle
    11: Result := BoolToStr(AAction^.FindSubControlOptions.UseWholeScreen, True);
    12: Result := AAction^.FindSubControlOptions.ColorError;  //string, to allow var replacements
    13: Result := AAction^.FindSubControlOptions.AllowedColorErrorCount;  //Number of pixels allowed to mismatch
    14: Result := BoolToStr(AAction^.FindSubControlOptions.WaitForControlToGoAway, True);
    15: Result := BoolToStr(AAction^.FindSubControlOptions.StartSearchingWithCachedControl, True);
    16: Result := AAction^.FindSubControlOptions.CachedControlLeft;
    17: Result := AAction^.FindSubControlOptions.CachedControlTop;
    18: Result := AAction^.FindSubControlOptions.MatchPrimitiveFiles; //ListOfStrings
    19: Result := BoolToStr(AAction^.FindSubControlOptions.GetAllControls, True);
    20: Result := BoolToStr(AAction^.FindSubControlOptions.UseFastSearch, True);
    21: Result := AAction^.FindSubControlOptions.FastSearchAllowedColorErrorCount;
    22: Result := AAction^.FindSubControlOptions.IgnoredColors;
    23: Result := BoolToStr(AAction^.FindSubControlOptions.SleepySearch, True);
    24: Result := BoolToStr(AAction^.FindSubControlOptions.StopSearchOnMismatch, True);
    25: Result := CImageSourceStr[AAction^.FindSubControlOptions.ImageSource];
    26: Result := AAction^.FindSubControlOptions.SourceFileName;
    27: Result := CImageSourceFileNameLocationStr[AAction^.FindSubControlOptions.ImageSourceFileNameLocation];
    28: Result := BoolToStr(AAction^.FindSubControlOptions.PrecisionTimeout, True);
    29: Result := BoolToStr(AAction^.FindSubControlOptions.FullBackgroundImageInResult, True);
    30: Result := '';
    31: Result := AAction^.FindSubControlOptions.EvaluateTextCount;
    32: Result := BoolToStr(AAction^.FindSubControlOptions.CropFromScreenshot, True);
    33: Result := AAction^.FindSubControlOptions.ThreadCount;
    else
      Result := 'unknown';
  end;
end;


{$IFDEF SubProperties}
  function GetActionValueStr_FindControl_MatchCriteria(AAction: PClkActionRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := BoolToStr(AAction^.FindControlOptions.MatchCriteria.WillMatchText, True);
      1: Result := BoolToStr(AAction^.FindControlOptions.MatchCriteria.WillMatchClassName, True);
      2: Result := CSearchForControlModeStr[AAction^.FindControlOptions.MatchCriteria.SearchForControlMode];
      else
        Result := 'unknown';
    end;
  end;

  function GetActionValueStr_FindSubControl_MatchCriteria(AAction: PClkActionRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := BoolToStr(AAction^.FindSubControlOptions.MatchCriteria.WillMatchBitmapText, True);
      1: Result := BoolToStr(AAction^.FindSubControlOptions.MatchCriteria.WillMatchBitmapFiles, True);
      2: Result := BoolToStr(AAction^.FindSubControlOptions.MatchCriteria.WillMatchPrimitiveFiles, True);
      else
        Result := 'unknown';
    end;
  end;

  function GetActionValueStr_FindSubControl_MatchBitmapText(AAction: PClkActionRec; APropertyIndex: Integer): string;
  var
    PropertyIndexMod, PropertyIndexDiv: Integer;
  begin
    if Length(AAction^.FindSubControlOptions.MatchBitmapText) = 0 then
    begin
      Result := 'No font profiles';
      Exit;
    end;

    PropertyIndexMod := APropertyIndex mod CPropCount_FindSubControlMatchBitmapText;
    PropertyIndexDiv := APropertyIndex div CPropCount_FindSubControlMatchBitmapText;

    case PropertyIndexMod of
      0: Result := AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].ForegroundColor;
      1: Result := AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].BackgroundColor;
      2: Result := AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].FontName;
      3: Result := IntToStr(AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].FontSize);
      4: Result := BoolToStr(AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].Bold, True);
      5: Result := BoolToStr(AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].Italic, True);
      6: Result := BoolToStr(AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].Underline, True);
      7: Result := BoolToStr(AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].StrikeOut, True);
      8: Result := CFontQualityStr[AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].FontQuality];
      9: Result := BoolToStr(AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].FontQualityUsesReplacement, True);
      10: Result := AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].FontQualityReplacement;
      11: Result := AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].ProfileName;
      12: Result := AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].CropLeft;
      13: Result := AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].CropTop;
      14: Result := AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].CropRight;
      15: Result := AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].CropBottom;
      16: Result := BoolToStr(AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].IgnoreBackgroundColor, True);
      else
        Result := 'unknown';
    end;
  end;


  function GetActionValueStr_FindSubControl_MatchBitmapAlgorithmSettings(AAction: PClkActionRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := IntToStr(AAction^.FindSubControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf);
      1: Result := IntToStr(AAction^.FindSubControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf);
      2: Result := IntToStr(AAction^.FindSubControlOptions.MatchBitmapAlgorithmSettings.XOffset);
      3: Result := IntToStr(AAction^.FindSubControlOptions.MatchBitmapAlgorithmSettings.YOffset);
      else
        Result := 'unknown';
    end;
  end;


  function GetActionValueStr_FindControl_InitialRectangle(AAction: PClkActionRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := AAction^.FindControlOptions.InitialRectangle.Left;
      1: Result := AAction^.FindControlOptions.InitialRectangle.Top;
      2: Result := AAction^.FindControlOptions.InitialRectangle.Right;
      3: Result := AAction^.FindControlOptions.InitialRectangle.Bottom;
      4: Result := AAction^.FindControlOptions.InitialRectangle.LeftOffset;
      5: Result := AAction^.FindControlOptions.InitialRectangle.TopOffset;
      6: Result := AAction^.FindControlOptions.InitialRectangle.RightOffset;
      7: Result := AAction^.FindControlOptions.InitialRectangle.BottomOffset;
      else
        Result := 'unknown';
    end;
  end;


  function GetActionValueStr_FindSubControl_InitialRectangle(AAction: PClkActionRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := AAction^.FindSubControlOptions.InitialRectangle.Left;
      1: Result := AAction^.FindSubControlOptions.InitialRectangle.Top;
      2: Result := AAction^.FindSubControlOptions.InitialRectangle.Right;
      3: Result := AAction^.FindSubControlOptions.InitialRectangle.Bottom;
      4: Result := AAction^.FindSubControlOptions.InitialRectangle.LeftOffset;
      5: Result := AAction^.FindSubControlOptions.InitialRectangle.TopOffset;
      6: Result := AAction^.FindSubControlOptions.InitialRectangle.RightOffset;
      7: Result := AAction^.FindSubControlOptions.InitialRectangle.BottomOffset;
      else
        Result := 'unknown';
    end;
  end;


  function GetActionValueStr_FindSubControl_MatchByHistogramSettings(AAction: PClkActionRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := AAction^.FindSubControlOptions.MatchByHistogramSettings.MinPercentColorMatch;
      1: Result := AAction^.FindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInSubBmp;
      2: Result := AAction^.FindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp;
      else
        Result := 'unknown';
    end;
  end;
{$ENDIF}


function GetActionValueStr_SetText(AAction: PClkActionRec; APropertyIndex: Integer): string;
begin
  case APropertyIndex of
    0: Result := AAction^.SetTextOptions.Text;
    1: Result := CClkSetTextControlTypeStr[AAction^.SetTextOptions.ControlType];
    2: Result := AAction^.SetTextOptions.DelayBetweenKeyStrokes;
    3: Result := AAction^.SetTextOptions.Count;
    else
      Result := 'unknown';
  end;
end;


function GetActionValueStr_CallTemplate(AAction: PClkActionRec; APropertyIndex: Integer): string;
begin
  case APropertyIndex of
    0: Result := AAction^.CallTemplateOptions.TemplateFileName;
    1: Result := AAction^.CallTemplateOptions.ListOfCustomVarsAndValues;
    2: Result := BoolToStr(AAction^.CallTemplateOptions.EvaluateBeforeCalling, True);
    3: Result := ''; //CallTemplateLoop
    else
      Result := 'unknown';
  end;
end;


{$IFDEF SubProperties}
  function GetActionValueStr_CallTemplate_CallTemplateLoop(AAction: PClkActionRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := BoolToStr(AAction^.CallTemplateOptions.CallTemplateLoop.Enabled, True);
      1: Result := AAction^.CallTemplateOptions.CallTemplateLoop.Counter;
      2: Result := AAction^.CallTemplateOptions.CallTemplateLoop.InitValue;
      3: Result := AAction^.CallTemplateOptions.CallTemplateLoop.EndValue;
      4: Result := CLoopDirectionStr[AAction^.CallTemplateOptions.CallTemplateLoop.Direction];
      5: Result := AAction^.CallTemplateOptions.CallTemplateLoop.BreakCondition;
      6: Result := CLoopEvalBreakPositionStr[AAction^.CallTemplateOptions.CallTemplateLoop.EvalBreakPosition];
      else
        Result := 'unknown';
    end;
  end;
{$ENDIF}


function GetActionValueStr_Sleep(AAction: PClkActionRec; APropertyIndex: Integer): string;
begin
  case APropertyIndex of
    0: Result := AAction^.SleepOptions.Value;
    else
      Result := 'unknown';
  end;
end;


function GetActionValueStr_SetVar(AAction: PClkActionRec; APropertyIndex: Integer): string;
begin
  case APropertyIndex of
    0: Result := '';   //ListOfVarNamesValuesAndEvalBefore  - this requires a custom editor
    1: Result := BoolToStr(AAction^.SetVarOptions.FailOnException, True);
    else
      Result := 'unknown';
  end;
end;


function GetActionValueStr_WindowOperations(AAction: PClkActionRec; APropertyIndex: Integer): string;
begin
  case APropertyIndex of
    0: Result := CWindowOperationStr[AAction^.WindowOperationsOptions.Operation];
    1: Result := AAction^.WindowOperationsOptions.NewX;
    2: Result := AAction^.WindowOperationsOptions.NewY;
    3: Result := AAction^.WindowOperationsOptions.NewWidth;
    4: Result := AAction^.WindowOperationsOptions.NewHeight;
    5: Result := BoolToStr(AAction^.WindowOperationsOptions.NewPositionEnabled, True);
    6: Result := BoolToStr(AAction^.WindowOperationsOptions.NewSizeEnabled, True);
    else
      Result := 'unknown';
  end;
end;


function GetActionValueStr_LoadSetVarFromFile(AAction: PClkActionRec; APropertyIndex: Integer): string;
begin
  case APropertyIndex of
    0: Result := AAction^.LoadSetVarFromFileOptions.FileName;
    1: Result := AAction^.LoadSetVarFromFileOptions.SetVarActionName;
    else
      Result := 'unknown';
  end;
end;


function GetActionValueStr_SaveSetVarToFile(AAction: PClkActionRec; APropertyIndex: Integer): string;
begin
  case APropertyIndex of
    0: Result := AAction^.SaveSetVarToFileOptions.FileName;
    1: Result := AAction^.SaveSetVarToFileOptions.SetVarActionName;
    else
      Result := 'unknown';
  end;
end;


function GetActionValueStr_Plugin(AAction: PClkActionRec; APropertyIndex: Integer): string;
var
  TempStringList: TStringList;
begin
  case APropertyIndex of
    0: Result := AAction^.PluginOptions.FileName;
    else
    begin
      Result := '';
      TempStringList := TStringList.Create;
      try
        TempStringList.Text := AAction^.PluginOptions.ListOfPropertiesAndValues;

        if APropertyIndex - 1 < TempStringList.Count then
          Result := TempStringList.ValueFromIndex[APropertyIndex - 1];
      finally
        TempStringList.Free;
      end;
    end;
  end;
end;


function GetActionValueStr_EditTemplate(AAction: PClkActionRec; APropertyIndex: Integer): string;
begin
  case APropertyIndex of
    0: Result := CEditTemplateOperationStr[AAction^.EditTemplateOptions.Operation];
    1: Result := CEditTemplateWhichTemplateStr[AAction^.EditTemplateOptions.WhichTemplate];
    2: Result := AAction^.EditTemplateOptions.TemplateFileName;
    3: Result := AAction^.EditTemplateOptions.EditedActionName;
    4: Result := CClkActionStr[AAction^.EditTemplateOptions.EditedActionType];
    5: Result := AAction^.EditTemplateOptions.EditedActionCondition;
    6: Result := IntToStr(AAction^.EditTemplateOptions.EditedActionTimeout);
    7: Result := AAction^.EditTemplateOptions.NewActionName;
    8: Result := AAction^.EditTemplateOptions.ListOfEditedProperties;
    9: Result := AAction^.EditTemplateOptions.ListOfEnabledProperties;
    10: Result := BoolToStr(AAction^.EditTemplateOptions.ShouldSaveTemplate, True);
    else
      Result := 'unknown';
  end;
end;


//
function XClickPointReference_AsStringToValue(AXClickPointReferenceAsString: string): TXClickPointReference;
var
  i: TXClickPointReference;
begin
  Result := Low(TXClickPointReference);

  for i := Low(TXClickPointReference) to High(TXClickPointReference) do
    if CXClickPointReferenceStr[i] = AXClickPointReferenceAsString then
    begin
      Result := i;
      Exit;
    end;
end;


function YClickPointReference_AsStringToValue(AYClickPointReferenceAsString: string): TYClickPointReference;
var
  i: TYClickPointReference;
begin
  Result := Low(TYClickPointReference);

  for i := Low(TYClickPointReference) to High(TYClickPointReference) do
    if CYClickPointReferenceStr[i] = AYClickPointReferenceAsString then
    begin
      Result := i;
      Exit;
    end;
end;


function TMouseButton_AsStringToValue(AMouseButtonAsString: string): TMouseButton;
var
  i: TMouseButton;
begin
  Result := Low(TMouseButton);

  for i := Low(TMouseButton) to High(TMouseButton) do
    if CMouseButtonStr[i] = AMouseButtonAsString then
    begin
      Result := i;
      Exit;
    end;
end;


function TMouseWheelType_AsStringToValue(AMouseWheelTypeAsString: string): TMouseWheelType;
var
  i: TMouseWheelType;
begin
  Result := Low(TMouseWheelType);

  for i := Low(TMouseWheelType) to High(TMouseWheelType) do
    if CMouseWheelTypeStr[i] = AMouseWheelTypeAsString then
    begin
      Result := i;
      Exit;
    end;
end;


function StrToBool(ABoolAsString: string): Boolean;
begin
  Result := ABoolAsString = 'True';
end;


function ClickType_AsStringToValue(AClickTypeAsString: string): Integer;
var
  i: Integer;
begin
  Result := CClickType_Click;
  for i := Low(CClickTypeStr) to High(CClickTypeStr) do
    if CClickTypeStr[i] = AClickTypeAsString then
    begin
      Result := i;
      Exit;
    end;
end;


function ExecAppUseInheritHandles_AsStringToValue(AExecAppUseInheritHandlesAsString: string): TExecAppUseInheritHandles;
var
  i: TExecAppUseInheritHandles;
begin
  Result := uihNo;
  for i := Low(TExecAppUseInheritHandles) to High(TExecAppUseInheritHandles) do
    if CExecAppUseInheritHandlesStr[i] = AExecAppUseInheritHandlesAsString then
    begin
      Result := i;
      Exit;
    end;
end;


function MatchBitmapAlgorithm_AsStringToValue(AMatchBitmapAlgorithmAsString: string): TMatchBitmapAlgorithm;
var
  i: TMatchBitmapAlgorithm;
begin
  Result := mbaBruteForce;
  for i := Low(TMatchBitmapAlgorithm) to High(TMatchBitmapAlgorithm) do
    if CMatchBitmapAlgorithmStr[i] = AMatchBitmapAlgorithmAsString then
    begin
      Result := i;
      Exit;
    end;
end;


function SearchForControlMode_AsStringToValue(ASearchForControlModeAsString: string): TSearchForControlMode;
var
  i: TSearchForControlMode;
begin
  Result := sfcmGenGrid;
  for i := Low(TSearchForControlMode) to High(TSearchForControlMode) do
    if CSearchForControlModeStr[i] = ASearchForControlModeAsString then
    begin
      Result := i;
      Exit;
    end;
end;


function ImageSource_AsStringToValue(AImageSourceAsString: string): TImageSource;
var
  i: TImageSource;
begin
  Result := isScreenshot;
  for i := Low(TImageSource) to High(TImageSource) do
    if CImageSourceStr[i] = AImageSourceAsString then
    begin
      Result := i;
      Exit;
    end;
end;


function ImageSourceFileNameLocation_AsStringToValue(AImageSourceFileNameLocationAsString: string): TImageSourceFileNameLocation;
var
  i: TImageSourceFileNameLocation;
begin
  Result := isflDisk;
  for i := Low(TImageSourceFileNameLocation) to High(TImageSourceFileNameLocation) do
    if CImageSourceFileNameLocationStr[i] = AImageSourceFileNameLocationAsString then
    begin
      Result := i;
      Exit;
    end;
end;


function FontQuality_AsStringToValue(AFontQualityAsString: string): TFontQuality;
var
  i: TFontQuality;
begin
  Result := fqCleartype;
  for i := Low(TFontQuality) to High(TFontQuality) do
    if CFontQualityStr[i] = AFontQualityAsString then
    begin
      Result := i;
      Exit;
    end;
end;


function ClkSetTextControlType_AsStringToValue(AClkSetTextControlTypeAsString: string): TClkSetTextControlType;
var
  i: TClkSetTextControlType;
begin
  Result := stEditBox;
  for i := Low(TClkSetTextControlType) to High(TClkSetTextControlType) do
    if CClkSetTextControlTypeStr[i] = AClkSetTextControlTypeAsString then
    begin
      Result := i;
      Exit;
    end;
end;


function LoopDirection_AsStringToValue(ALoopDirectionAsString: string): TLoopDirection;
var
  i: TLoopDirection;
begin
  Result := ldInc;
  for i := Low(TLoopDirection) to High(TLoopDirection) do
    if CLoopDirectionStr[i] = ALoopDirectionAsString then
    begin
      Result := i;
      Exit;
    end;
end;


function LoopEvalBreakPosition_AsStringToValue(ALoopEvalBreakPositionAsString: string): TLoopEvalBreakPosition;
var
  i: TLoopEvalBreakPosition;
begin
  Result := lebpAfterContent;
  for i := Low(TLoopEvalBreakPosition) to High(TLoopEvalBreakPosition) do
    if CLoopEvalBreakPositionStr[i] = ALoopEvalBreakPositionAsString then
    begin
      Result := i;
      Exit;
    end;
end;


function WindowOperation_AsStringToValue(AWindowOperationAsString: string): TWindowOperation;
var
  i: TWindowOperation;
begin
  Result := woBringToFront;
  for i := Low(TWindowOperation) to High(TWindowOperation) do
    if CWindowOperationStr[i] = AWindowOperationAsString then
    begin
      Result := i;
      Exit;
    end;
end;


function EditTemplateOperation_AsStringToValue(AEditTemplateOperationAsString: string): TEditTemplateOperation;
var
  i: TEditTemplateOperation;
begin
  Result := etoNewAction;
  for i := Low(TEditTemplateOperation) to High(TEditTemplateOperation) do
    if CEditTemplateOperationStr[i] = AEditTemplateOperationAsString then
    begin
      Result := i;
      Exit;
    end;
end;


function EditTemplateWhichTemplate_AsStringToValue(AEditTemplateWhichTemplate: string): TEditTemplateWhichTemplate;
var
  i: TEditTemplateWhichTemplate;
begin
  Result := etwtOther;
  for i := Low(TEditTemplateWhichTemplate) to High(TEditTemplateWhichTemplate) do
    if CEditTemplateWhichTemplateStr[i] = AEditTemplateWhichTemplate then
    begin
      Result := i;
      Exit;
    end;
end;


function EditTemplateEditedActionType_AsStringToValue(AEditTemplateEditedActionType: string): TClkAction;
var
  i: TClkAction;
begin
  Result := acClick;
  for i := Low(TClkAction) to High(TClkAction) do
    if CClkActionStr[i] = AEditTemplateEditedActionType then
    begin
      Result := i;
      Exit;
    end;
end;


procedure SetActionValueStr_Action(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
begin
  case APropertyIndex of
    0: AAction^.ActionOptions.ActionName := NewValue;
    1: AAction^.ActionOptions.Action := ActionAsStringToTClkAction(NewValue);
    2: AAction^.ActionOptions.ActionTimeout := StrToIntDef(NewValue, 1000);
    3: AAction^.ActionOptions.ActionCondition := NewValue;
    else
      ;
  end;
end;


procedure SetActionValueStr_Click(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
begin
  case APropertyIndex of
    0: AAction^.ClickOptions.XClickPointReference := XClickPointReference_AsStringToValue(NewValue);
    1: AAction^.ClickOptions.YClickPointReference := YClickPointReference_AsStringToValue(NewValue);
    2: AAction^.ClickOptions.XClickPointVar := NewValue;
    3: AAction^.ClickOptions.YClickPointVar := NewValue;
    4: AAction^.ClickOptions.XOffset := NewValue;
    5: AAction^.ClickOptions.YOffset := NewValue;
    6: AAction^.ClickOptions.MouseButton := TMouseButton_AsStringToValue(NewValue);
    7: AAction^.ClickOptions.ClickWithCtrl := StrToBool(NewValue);
    8: AAction^.ClickOptions.ClickWithAlt := StrToBool(NewValue);
    9: AAction^.ClickOptions.ClickWithShift := StrToBool(NewValue);
    //10:= BoolToStr(AAction^.ClickOptions.ClickWithDoubleClick, True);  //deprecated
    10: AAction^.ClickOptions.Count := Max(1, StrToIntDef(NewValue, 1));
    11: AAction^.ClickOptions.LeaveMouse := StrToBool(NewValue);
    12: AAction^.ClickOptions.MoveWithoutClick := StrToBool(NewValue);
    13: AAction^.ClickOptions.ClickType := ClickType_AsStringToValue(NewValue);
    14: AAction^.ClickOptions.XClickPointReferenceDest := XClickPointReference_AsStringToValue(NewValue);
    15: AAction^.ClickOptions.YClickPointReferenceDest := YClickPointReference_AsStringToValue(NewValue);
    16: AAction^.ClickOptions.XClickPointVarDest := NewValue;
    17: AAction^.ClickOptions.YClickPointVarDest := NewValue;
    18: AAction^.ClickOptions.XOffsetDest := NewValue;
    19: AAction^.ClickOptions.YOffsetDest := NewValue;
    20: AAction^.ClickOptions.MouseWheelType := TMouseWheelType_AsStringToValue(NewValue);
    21: AAction^.ClickOptions.MouseWheelAmount := NewValue;
    22: AAction^.ClickOptions.DelayAfterMovingToDestination := NewValue;
    23: AAction^.ClickOptions.DelayAfterMouseDown := NewValue;
    24: AAction^.ClickOptions.MoveDuration := NewValue;
    25: AAction^.ClickOptions.UseClipCursor := StrToBool(NewValue);
    else
      ;
  end;
end;


procedure SetActionValueStr_ExecApp(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
begin
  case APropertyIndex of
    0: AAction^.ExecAppOptions.PathToApp := NewValue;
    1: AAction^.ExecAppOptions.ListOfParams := NewValue;
    2: AAction^.ExecAppOptions.WaitForApp := StrToBool(NewValue);
    3: AAction^.ExecAppOptions.AppStdIn := NewValue;
    4: AAction^.ExecAppOptions.CurrentDir := NewValue;
    5: AAction^.ExecAppOptions.UseInheritHandles := ExecAppUseInheritHandles_AsStringToValue(NewValue);
    6: AAction^.ExecAppOptions.NoConsole := StrToBool(NewValue);
    else
      ;
  end;
end;


procedure SetActionValueStr_FindControl(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
begin
  case APropertyIndex of
    0: ;  //MatchCriteria
    1: AAction^.FindControlOptions.AllowToFail := StrToBool(NewValue);
    2: AAction^.FindControlOptions.MatchText := NewValue;
    3: AAction^.FindControlOptions.MatchClassName := NewValue;
    4: AAction^.FindControlOptions.MatchTextSeparator := NewValue;
    5: AAction^.FindControlOptions.MatchClassNameSeparator := NewValue;
    6: ;  //MatchBitmapText
    7: ;  //AAction^.FindControlOptions.MatchBitmapFiles := NewValue; //ListOfStrings
    8: ;  //AAction^.FindControlOptions.MatchBitmapAlgorithm := MatchBitmapAlgorithm_AsStringToValue(NewValue);
    9: ;  //MatchBitmapAlgorithmSettings
    10: ;  //InitialRectangle
    11: AAction^.FindControlOptions.UseWholeScreen := StrToBool(NewValue);
    12: ;  //AAction^.FindControlOptions.ColorError := NewValue;  //string, to allow var replacements
    13: ;  //AAction^.FindControlOptions.AllowedColorErrorCount := NewValue;  //Number of pixels allowed to mismatch
    14: AAction^.FindControlOptions.WaitForControlToGoAway := StrToBool(NewValue);
    15: AAction^.FindControlOptions.StartSearchingWithCachedControl := StrToBool(NewValue);
    16: AAction^.FindControlOptions.CachedControlLeft := NewValue;
    17: AAction^.FindControlOptions.CachedControlTop := NewValue;
    18: ;  //AAction^.FindControlOptions.MatchPrimitiveFiles := NewValue; //ListOfStrings
    19: AAction^.FindControlOptions.GetAllControls := StrToBool(NewValue);
    20: ;  //AAction^.FindControlOptions.UseFastSearch := StrToBool(NewValue);
    21: ;  //AAction^.FindControlOptions.FastSearchAllowedColorErrorCount := NewValue;
    22: ;  //AAction^.FindControlOptions.IgnoredColors := NewValue;
    23: ;  //AAction^.FindControlOptions.SleepySearch := StrToBool(NewValue);
    24: ;  //AAction^.FindControlOptions.StopSearchOnMismatch := StrToBool(NewValue);
    25: ;  //AAction^.FindControlOptions.ImageSource := ImageSource_AsStringToValue(NewValue);
    26: ;  //AAction^.FindControlOptions.SourceFileName := NewValue;
    27: ;  //AAction^.FindControlOptions.ImageSourceFileNameLocation := ImageSourceFileNameLocation_AsStringToValue(NewValue);
    28: AAction^.FindControlOptions.PrecisionTimeout := StrToBool(NewValue);
    29: ;  //AAction^.FindControlOptions.FullBackgroundImageInResult := StrToBool(NewValue);
    30: ;  //MatchByHistogramSettings
    31: AAction^.FindControlOptions.EvaluateTextCount := NewValue;
    32: ;  //AAction^.FindControlOptions.CropFromScreenshot := StrToBool(NewValue);
    33: ;  //AAction^.FindControlOptions.ThreadCount := NewValue;
    else
      ;
  end;
end;


procedure SetActionValueStr_FindSubControl(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
begin
  case APropertyIndex of
    0: ;  //MatchCriteria
    1: AAction^.FindSubControlOptions.AllowToFail := StrToBool(NewValue);
    2: AAction^.FindSubControlOptions.MatchText := NewValue;
    3: AAction^.FindSubControlOptions.MatchClassName := NewValue;
    4: AAction^.FindSubControlOptions.MatchTextSeparator := NewValue;
    5: AAction^.FindSubControlOptions.MatchClassNameSeparator := NewValue;
    6: ;  //MatchBitmapText
    7: AAction^.FindSubControlOptions.MatchBitmapFiles := NewValue; //ListOfStrings
    8: AAction^.FindSubControlOptions.MatchBitmapAlgorithm := MatchBitmapAlgorithm_AsStringToValue(NewValue);
    9: ;  //MatchBitmapAlgorithmSettings
    10: ;  //InitialRectangle
    11: AAction^.FindSubControlOptions.UseWholeScreen := StrToBool(NewValue);
    12: AAction^.FindSubControlOptions.ColorError := NewValue;  //string, to allow var replacements
    13: AAction^.FindSubControlOptions.AllowedColorErrorCount := NewValue;  //Number of pixels allowed to mismatch
    14: AAction^.FindSubControlOptions.WaitForControlToGoAway := StrToBool(NewValue);
    15: AAction^.FindSubControlOptions.StartSearchingWithCachedControl := StrToBool(NewValue);
    16: AAction^.FindSubControlOptions.CachedControlLeft := NewValue;
    17: AAction^.FindSubControlOptions.CachedControlTop := NewValue;
    18: AAction^.FindSubControlOptions.MatchPrimitiveFiles := NewValue; //ListOfStrings
    19: AAction^.FindSubControlOptions.GetAllControls := StrToBool(NewValue);
    20: AAction^.FindSubControlOptions.UseFastSearch := StrToBool(NewValue);
    21: AAction^.FindSubControlOptions.FastSearchAllowedColorErrorCount := NewValue;
    22: AAction^.FindSubControlOptions.IgnoredColors := NewValue;
    23: AAction^.FindSubControlOptions.SleepySearch := StrToBool(NewValue);
    24: AAction^.FindSubControlOptions.StopSearchOnMismatch := StrToBool(NewValue);
    25: AAction^.FindSubControlOptions.ImageSource := ImageSource_AsStringToValue(NewValue);
    26: AAction^.FindSubControlOptions.SourceFileName := NewValue;
    27: AAction^.FindSubControlOptions.ImageSourceFileNameLocation := ImageSourceFileNameLocation_AsStringToValue(NewValue);
    28: AAction^.FindSubControlOptions.PrecisionTimeout := StrToBool(NewValue);
    29: AAction^.FindSubControlOptions.FullBackgroundImageInResult := StrToBool(NewValue);
    30: ;  //MatchByHistogramSettings
    31: AAction^.FindSubControlOptions.EvaluateTextCount := NewValue;
    32: AAction^.FindSubControlOptions.CropFromScreenshot := StrToBool(NewValue);
    33: AAction^.FindSubControlOptions.ThreadCount := NewValue;
    else
      ;
  end;
end;


{$IFDEF SubProperties}
  procedure SetActionValueStr_FindControl_MatchCriteria(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  begin
    case APropertyIndex of
      0: AAction^.FindControlOptions.MatchCriteria.WillMatchText := StrToBool(NewValue);
      1: AAction^.FindControlOptions.MatchCriteria.WillMatchClassName := StrToBool(NewValue);
      2: AAction^.FindControlOptions.MatchCriteria.SearchForControlMode := SearchForControlMode_AsStringToValue(NewValue);
      else
        ;
    end;
  end;

  procedure SetActionValueStr_FindSubControl_MatchCriteria(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  begin
    case APropertyIndex of
      0: AAction^.FindSubControlOptions.MatchCriteria.WillMatchBitmapText := StrToBool(NewValue);
      1: AAction^.FindSubControlOptions.MatchCriteria.WillMatchBitmapFiles := StrToBool(NewValue);
      2: AAction^.FindSubControlOptions.MatchCriteria.WillMatchPrimitiveFiles := StrToBool(NewValue);
      else
        ;
    end;
  end;

  procedure SetActionValueStr_FindSubControl_MatchBitmapText(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  var
    PropertyIndexMod, PropertyIndexDiv: Integer;
  begin
    PropertyIndexMod := APropertyIndex mod CPropCount_FindSubControlMatchBitmapText;
    PropertyIndexDiv := APropertyIndex div CPropCount_FindSubControlMatchBitmapText;

    case PropertyIndexMod of
      0: AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].ForegroundColor := NewValue;
      1: AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].BackgroundColor := NewValue;
      2: AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].FontName := NewValue;
      3: AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].FontSize := StrToIntDef(NewValue, 8);
      4: AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].Bold := StrToBool(NewValue);
      5: AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].Italic := StrToBool(NewValue);
      6: AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].Underline := StrToBool(NewValue);
      7: AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].StrikeOut := StrToBool(NewValue);
      8: AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].FontQuality := FontQuality_AsStringToValue(NewValue);
      9: AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].FontQualityUsesReplacement := StrToBool(NewValue);
      10: AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].FontQualityReplacement := NewValue;
      11: AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].ProfileName := NewValue;
      12: AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].CropLeft := NewValue;
      13: AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].CropTop := NewValue;
      14: AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].CropRight := NewValue;
      15: AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].CropBottom := NewValue;
      16: AAction^.FindSubControlOptions.MatchBitmapText[PropertyIndexDiv].IgnoreBackgroundColor := StrToBool(NewValue);
      else
        ;
    end;
  end;


  procedure SetActionValueStr_FindSubControl_MatchBitmapAlgorithmSettings(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  begin
    case APropertyIndex of
      0: AAction^.FindSubControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf := StrToIntDef(NewValue, 1);
      1: AAction^.FindSubControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf := StrToIntDef(NewValue, 1);
      2: AAction^.FindSubControlOptions.MatchBitmapAlgorithmSettings.XOffset := StrToIntDef(NewValue, 0);
      3: AAction^.FindSubControlOptions.MatchBitmapAlgorithmSettings.YOffset := StrToIntDef(NewValue, 0);
      else
        ;
    end;
  end;


  procedure SetActionValueStr_FindControl_InitialRectangle(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  begin
    case APropertyIndex of
      0: AAction^.FindControlOptions.InitialRectangle.Left := NewValue;
      1: AAction^.FindControlOptions.InitialRectangle.Top := NewValue;
      2: AAction^.FindControlOptions.InitialRectangle.Right := NewValue;
      3: AAction^.FindControlOptions.InitialRectangle.Bottom := NewValue;
      4: AAction^.FindControlOptions.InitialRectangle.LeftOffset := NewValue;
      5: AAction^.FindControlOptions.InitialRectangle.TopOffset := NewValue;
      6: AAction^.FindControlOptions.InitialRectangle.RightOffset := NewValue;
      7: AAction^.FindControlOptions.InitialRectangle.BottomOffset := NewValue;
      else
        ;
    end;
  end;


  procedure SetActionValueStr_FindSubControl_InitialRectangle(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  begin
    case APropertyIndex of
      0: AAction^.FindSubControlOptions.InitialRectangle.Left := NewValue;
      1: AAction^.FindSubControlOptions.InitialRectangle.Top := NewValue;
      2: AAction^.FindSubControlOptions.InitialRectangle.Right := NewValue;
      3: AAction^.FindSubControlOptions.InitialRectangle.Bottom := NewValue;
      4: AAction^.FindSubControlOptions.InitialRectangle.LeftOffset := NewValue;
      5: AAction^.FindSubControlOptions.InitialRectangle.TopOffset := NewValue;
      6: AAction^.FindSubControlOptions.InitialRectangle.RightOffset := NewValue;
      7: AAction^.FindSubControlOptions.InitialRectangle.BottomOffset := NewValue;
      else
        ;
    end;
  end;


  procedure SetActionValueStr_FindSubControl_MatchByHistogramSettings(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  begin
    case APropertyIndex of
      0: AAction^.FindSubControlOptions.MatchByHistogramSettings.MinPercentColorMatch := NewValue;
      1: AAction^.FindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInSubBmp := NewValue;
      2: AAction^.FindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp := NewValue;
      else
        ;
    end;
  end;
{$ENDIF}


procedure SetActionValueStr_SetText(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
begin
  case APropertyIndex of
    0: AAction^.SetTextOptions.Text := NewValue;
    1: AAction^.SetTextOptions.ControlType := ClkSetTextControlType_AsStringToValue(NewValue);
    2: AAction^.SetTextOptions.DelayBetweenKeyStrokes := NewValue;
    3: AAction^.SetTextOptions.Count := NewValue;
    else
      ;
  end;
end;


procedure SetActionValueStr_CallTemplate(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
begin
  case APropertyIndex of
    0: AAction^.CallTemplateOptions.TemplateFileName := NewValue;
    1: AAction^.CallTemplateOptions.ListOfCustomVarsAndValues := NewValue;
    2: AAction^.CallTemplateOptions.EvaluateBeforeCalling := StrToBool(NewValue);
    3: ; //CallTemplateLoop
    else
      ;
  end;
end;


{$IFDEF SubProperties}
  procedure SetActionValueStr_CallTemplate_CallTemplateLoop(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  begin
    case APropertyIndex of
      0: AAction^.CallTemplateOptions.CallTemplateLoop.Enabled := StrToBool(NewValue);
      1: AAction^.CallTemplateOptions.CallTemplateLoop.Counter := NewValue;
      2: AAction^.CallTemplateOptions.CallTemplateLoop.InitValue := NewValue;
      3: AAction^.CallTemplateOptions.CallTemplateLoop.EndValue := NewValue;
      4: AAction^.CallTemplateOptions.CallTemplateLoop.Direction := LoopDirection_AsStringToValue(NewValue);
      5: AAction^.CallTemplateOptions.CallTemplateLoop.BreakCondition := NewValue;
      6: AAction^.CallTemplateOptions.CallTemplateLoop.EvalBreakPosition := LoopEvalBreakPosition_AsStringToValue(NewValue);
      else
        ;
    end;
  end;
{$ENDIF}


procedure SetActionValueStr_Sleep(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
begin
  case APropertyIndex of
    0: AAction^.SleepOptions.Value := NewValue;
    else
      ;
  end;
end;


procedure SetActionValueStr_SetVar(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
begin
  case APropertyIndex of
    0: ;   //ListOfVarNamesValuesAndEvalBefore  - this requires a custom editor
    1: AAction^.SetVarOptions.FailOnException := StrToBool(NewValue);
    else
      ;
  end;
end;


procedure SetActionValueStr_WindowOperations(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
begin
  case APropertyIndex of
    0: AAction^.WindowOperationsOptions.Operation := WindowOperation_AsStringToValue(NewValue);
    1: AAction^.WindowOperationsOptions.NewX := NewValue;
    2: AAction^.WindowOperationsOptions.NewY := NewValue;
    3: AAction^.WindowOperationsOptions.NewWidth := NewValue;
    4: AAction^.WindowOperationsOptions.NewHeight := NewValue;
    5: AAction^.WindowOperationsOptions.NewPositionEnabled := StrToBool(NewValue);
    6: AAction^.WindowOperationsOptions.NewSizeEnabled := StrToBool(NewValue);
    else
      ;
  end;
end;


procedure SetActionValueStr_LoadSetVarFromFile(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
begin
  case APropertyIndex of
    0: AAction^.LoadSetVarFromFileOptions.FileName := NewValue;
    1: AAction^.LoadSetVarFromFileOptions.SetVarActionName := NewValue;
    else
      ;
  end;
end;


procedure SetActionValueStr_SaveSetVarToFile(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
begin
  case APropertyIndex of
    0: AAction^.SaveSetVarToFileOptions.FileName := NewValue;
    1: AAction^.SaveSetVarToFileOptions.SetVarActionName := NewValue;
    else
      ;
  end;
end;


procedure SetActionValueStr_Plugin(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
var
  TempStringList: TStringList;
begin
  case APropertyIndex of
    0: AAction^.PluginOptions.FileName := NewValue;
    else
    begin
      Dec(APropertyIndex, CPropCount_Plugin);  //adjust offset

      TempStringList := TStringList.Create;
      try
        TempStringList.Text := AAction^.PluginOptions.ListOfPropertiesAndValues;  //this list does not contain properties defined in this file

        if APropertyIndex < TempStringList.Count then
        begin
          //Do not use: "TempStringList.ValueFromIndex[APropertyIndex] := NewValue" !!!  It removes the entire item from list if NewValue = '':
          TempStringList.Strings[APropertyIndex] := TempStringList.Names[APropertyIndex] + '=' + NewValue;
          AAction^.PluginOptions.ListOfPropertiesAndValues := TempStringList.Text;
        end;
      finally
        TempStringList.Free;
      end;
    end;
  end;
end;


procedure SetActionValueStr_EditTemplate(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
begin
  case APropertyIndex of
    0: AAction^.EditTemplateOptions.Operation := EditTemplateOperation_AsStringToValue(NewValue);
    1: AAction^.EditTemplateOptions.WhichTemplate := EditTemplateWhichTemplate_AsStringToValue(NewValue);
    2: AAction^.EditTemplateOptions.TemplateFileName := NewValue;
    3: AAction^.EditTemplateOptions.EditedActionName := NewValue;
    4: AAction^.EditTemplateOptions.EditedActionType := EditTemplateEditedActionType_AsStringToValue(NewValue);
    5: AAction^.EditTemplateOptions.EditedActionCondition := NewValue;
    6: AAction^.EditTemplateOptions.EditedActionTimeout := StrToIntDef(NewValue, 1000);
    7: AAction^.EditTemplateOptions.NewActionName := NewValue;
    8: AAction^.EditTemplateOptions.ListOfEditedProperties := NewValue;
    9: AAction^.EditTemplateOptions.ListOfEnabledProperties := NewValue;
    10: AAction^.EditTemplateOptions.ShouldSaveTemplate := StrToBool(NewValue);
    else
      ;
  end;
end;


//
function GetPropertyHintNoHint: string;
begin
  Result := '';
end;


function GetPropertyHint_Click_XClickPointVar: string;
begin
  Result := 'The provided variables must be global/screen coordinates.' + #13#10 +
            'The $Current_Mouse_X$ var/replacement can be used as global mouse X coordinate.';
end;


function GetPropertyHint_Click_YClickPointVar: string;
begin
  Result := 'The provided variables must be global/screen coordinates.' + #13#10 +
            'The $Current_Mouse_Y$ var/replacement can be used as global mouse Y coordinate.';
end;


function GetPropertyHint_Click_XOffset: string;
begin
  Result := 'Replacements are available.  Examples of random value: $Random(50, 100)$   $Random($MMin$, $MMax$)$';
end;


function GetPropertyHint_Click_YOffset: string;
begin
  Result := 'Replacements are available.  Examples of random value: $Random(50, 100)$   $Random($MMin$, $MMax$)$';
end;


function GetPropertyHint_Click_LeaveMouse: string;
begin
  Result := 'When True, the mouse cursor position is not reset after running the action.' + #13#10 +
            'It may be required if the click action will open a pop-up menu, to cause the menu to open at that location.' + #13#10 +
            'This is also useful for debugging, to verify offsets.';
end;


function GetPropertyHint_Click_DelayAfterMovingToDestination: string;
begin
  Result := 'Delay in ms, between moving the cursor to click point, and the actual click operation.';
end;


function GetPropertyHint_Click_DelayAfterMouseDown: string;
begin
  Result := 'Delay in ms, between mouse down and mouse up events, of a click operation.';
end;


function GetPropertyHint_Click_MouseWheelAmount: string;
begin
  Result := 'Positive values will scroll up. Negative values will scroll down.';
end;


function GetPropertyHint_Click_MoveDuration: string;
begin
  Result := 'Duration in ms, of how much it takes to move the mouse cursor from its current location to its destination.' + #13#10 +
            'If set to a negative value, no delay is used.';
end;


function GetPropertyHint_Click_UseClipCursor: string;
begin
  Result := 'If set to True, it ensures that the mouse cursor is kept at the specified coordinates, although the physical mouse generates other input.' + #13#10 +
            'This may be required, when the user can accidentally move the mouse, while UIClicker is running and setting other mouse cursor position.';
end;


function GetPropertyHint_ExecApp_PathToApp: string;
begin
  Result := 'Full path (without quotes) to executable or other file to be open with associated application. Replacements are available.';
end;


function GetPropertyHint_ExecApp_AppStdIn: string;
begin
  Result := 'All #4#5 (a.k.a. 0x4:0x5) occurrences are replaced with CRLF (#13#10) before executing the application.' + #13#10 +
            'Var/replacements are available. E.g.: $ExecAction_StdIn$' + #13#10 +
            'When this parameter is empty string, the executed application can run without inherited handles.';
end;


function GetPropertyHint_ExecApp_CurrentDir: string;
begin
  Result := 'Application current directory.  Replacements are avaialable.  Example: $ExtractFileDir($PathToMyFile$)$';
end;


function GetPropertyHint_ExecApp_UseInheritHandles: string;
begin
  Result := 'Required mostly when passing data through StdIn.';
end;


function GetPropertyHint_ExecApp_NoConsole: string;
begin
  Result := 'When True, console applications are not displayed in a new window.' + #13#10 +
            'UI applications can create and display system consoles. For those applications, this option may cause problems if checked.';
end;


function GetPropertyHint_FindControl_AllowToFail: string;
begin
  Result := 'When True, the execution flow does not stop if the searched (sub)control is not found.' + #13#10 +
            'The "Allowed Failed" response can be used for conditional execution (call action).' + #13#10 +
            'When the action is allowed to fail and it fails, $LastAction_Status$ is set to "Allowed Failed".';
end;


function GetPropertyHint_FindControl_MatchText: string;
begin
  Result := 'Wildcards are available ("*"). Variable replacements are available.' + #13#10 +
            'Used on matching text and BMP text.' + #13#10 +
            'For controls, which can have different text values (e.g. a window displaying a different title), these values can be e.g. comma separated. In that case, the text separator is a comma.' + #13#10 +
            'For example: a window can display "MyTitle" or "MyTitle (modified)". In that case, the "Match Text" editbox can contain "MyTitle,MyTitle (modified)", without quotes, by using the comma separator.';
end;


function GetPropertyHint_FindControl_MatchClassName: string;
begin
  Result := 'Wildcards are available ("*"). Variable replacements are available.' + #13#10 +
            'There are applications which can have one or more of their windows, registered with class name, containing a randomly generated string.';
end;


function GetPropertyHint_FindControl_MatchBitmapText: string;
begin
  Result := 'Count should be 0 for FindControl and >0 for FindSubControl';
end;


function GetPropertyHint_FindControl_MatchBitmapFiles: string;
begin
  Result := 'Relative paths can be entered using the following format:' + #13#10 + '$TemplateDir$\<SomeBmp.bmp>';
end;


function GetPropertyHint_FindControl_MatchBitmapAlgorithm: string;
begin
  Result := 'This is the "high-level" algorithm, used for defining the points, where the searched bitmap is compared to an area from the background bitmap.' + #13#10 +
            '- For mbaBruteForce, the searched bitmap is compared on every x:y point, which allows the searched bitmap to fit into the background.' + #13#10 +
            '- When mbaXYMultipleAndOffsets is selected, a grid is generated, which expects the searched bitmap to start (top-left corner) on one of the grid points.' + #13#10 +
            '- The mbaRawHistogramZones algorithm (currently in work) generates a grid, with the resolution, matching the searched bitmap size, then computes the histogram of each resulted zone.' + #13#10 +
            '    If the significant colors from every zone histogram, match the searced bitmap''s significant colors, then the brute-force search is used on those specific zones.';
end;


function GetPropertyHint_FindControl_UseWholeScreen: string;
begin
  Result := 'Use the whole screen as search area, if True.' + #13#10 +
            'Use the current control as search area, if False.' + #13#10 +
            'The search area is modified by offsets.';
end;


function GetPropertyHint_FindControl_ColorError: string;
begin
  Result := 'When matching bitmaps, which contain antialiasing pixels (see smooth text), some of those pixels will not match.' + #13#10 +
            'The "Color Error" represents the difference between the color values for the two compared pixels, for each RGB channel.' + #13#10 +
            'The "Color Error Count" is the allowed number of mismatching pixels.' + #13#10 +
            'Variable replacements are available.' + #13#10 +
            'If at least one of the three color channels (R, G, B) mismatches by at least ColorError, it counts as an error point.' + #13#10 +
            'Used for "BMP Text" and "BMP Files".';
end;


function GetPropertyHint_FindControl_AllowedColorErrorCount: string;
begin
  Result := 'When matching bitmaps, which contain antialiasing pixels (see smooth text), some of those pixels will not match.' + #13#10 +
            'The "Color Error" represents the difference between the color values for the two compared pixels, for each RGB channel.' + #13#10 +
            'The "Color Error Count" is the allowed number of mismatching pixels. Variable replacements are available.' + #13#10 +
            'Used for "BMP Text" and "BMP Files". The bitmap searching algorithm stops on error count.';
end;


function GetPropertyHint_FindControl_WaitForControlToGoAway: string;
begin
  Result := 'When True, the action expects to find no control, using the current settings.';
end;


function GetPropertyHint_FindControl_StartSearchingWithCachedControl: string;
begin
  Result := 'When True, the control is searched at the specified cached $My_Control_Left$ and $My_Control_Top$ var replacements, before using the search grid.' + #13#10 +
            'In order to cache the control coordinates, please add a SetVar action after this one, by assigning:' + #13#10 +
            '$My_Control_Left$ to $Control_Left$' + #13#10 +
            'and' + #13#10 +
            '$My_Control_Top$ to $Control_Top$' + #13#10 +
            'where $My_Control_Left$ and $My_Control_Top$ are the cached values. The "Eval before" checkboxes have to be set.' + #13#10 +
            'Each Find(Sub)Control action, which uses caching, will have to use its own set of $My_Control_Left$ and $My_Control_Top$ vars.' + #13#10 +
            'The cached values are global coordinates, so they will become invalid even for a subcontrol if the parent window is moved.';
end;


function GetPropertyHint_FindControl_CachedControlLeftTop: string;
begin
  Result := 'Caching is more effective when searching for controls with a single font profile.' + #13#10 +
            'Using caching on multpile font profiles may indeed be faster than without it, but for every cache miss, the search defaults to the selected algorithm.';
end;


function GetPropertyHint_FindControl_MatchPrimitiveFiles: string;
begin
  Result := 'Relative paths can be entered using the following format:' + #13#10 + '$TemplateDir$\<SomeFile.pmtv>';
end;


function GetPropertyHint_FindControl_GetAllControls: string;
begin
  Result := 'When set to True, FindControl doesn''t stop at the first result.' + #13#10 +
            'It adds all control handles, as a #4#5 separated list, to $AllControl_Handles$ variable.' + #13#10 +
            'The list items can be extracted with $GetTextItem($AllControl_Handles$,<ItemIndex>)$ function.' + #13#10 +
            'The number of handles can be obtained with $ItemCount($AllControl_Handles$)$.' + #13#10#13#10 +
            'When iterating through all returned handles, the control property variables (like $Control_Left$, $Control_Top$ etc.) can be set by calling $UpdateControlInfo(<Handle>)$.';
end;


function GetPropertyHint_FindControl_UseFastSearch: string;
begin
  Result := 'When set to True, FindSubControl starts searching the bitmap (text, file, primitive etc) by cropping a 5px x 5px area, from its top-left corner.' + #13#10 +
            'If that matches, the algorihm verifies a match at the same position, for the full searched bitmap.' + #13#10 +
            'When set to True, the full bitmap is verified at every valid position. Usually, this is very slow (it gets slower with larger bitmaps).';
end;


function GetPropertyHint_FindControl_FastSearchAllowedColorErrorCount: string;
begin
  Result := 'This is similar to AllowedColorErrorCount, but it is used for the small searched area (e.g. 5px x 5px), when UseFastSearch is True.' + #13#10 +
            'A value of -1, will instruct the algorithm to calculate its own value, as a scaled down version of AllowedColorErrorCount,' + #13#10 +
            'based on the ratio between the small search area (5x5) and the full searched bitmap size. That ratio will often be 0, because of rounding to integer.' + #13#10 +
            'FastSearchAllowedColorErrorCount should have a minimium value, so that the 5x5 area will match (even if at multiple locations).' + #13#10#13#10 +

            'If FastSearchAllowedColorErrorCount is too small, so that no 5x5 area matches, then no full bitmap is searched for.' + #13#10 +
            'Because of that, it is possible that a match would exist for the full searched bitmap, but a too tight color error count, would prevent a 5x5 area match.' + #13#10#13#10 +

            'If the value is too high, there are too many false positives (false matchings), causing a full bitmap search on all these positions, defeating the purpose.';
end;


function GetPropertyHint_FindControl_IgnoredColors: string;
begin
  Result := 'Comma-separated list of colors, from searched bitmap.' + #13#10 +
            'They can be 6-digit hex values (BGR format, without "0x" prefix) or var/replacements, which contain 6-digit hex values.' + #13#10 +
            'When empty, no color is ignored (except for text background color, if configured).';
end;


function GetPropertyHint_FindControl_SleepySearch: string;
begin
  Result := 'When set to True, once in a while, at random times, a call to Sleep(1) is made, to avoid keeping a CPU core to 100%.' + #13#10 +
            'Since thread switching is not that accurate, a call to Sleep(1) may take even 16ms. This will result in slower searches.';
end;


function GetPropertyHint_FindControl_StopSearchOnMismatch: string;
begin
  Result := 'When set to False, the search continues, to get the total pixel error count. The action result will be set to "Successful".' + #13#10 +
            'This is useful when the searched bitmap has the same size (both width and height) as the area it is searched from.' + #13#10 +
            'When set to False, the result (total errored pixel count) is placed into $ResultedErrorCount$.' + #13#10 +
            'If the action fails when set to True, the result is usually greater than the value of AllowedColorErrorCount property.';
end;


function GetPropertyHint_FindControl_ImageSource: string;
begin
  Result := 'When set to isScreenshot, FindSubControl takes screenshot from selected area, where it searches for a bitmap.' + #13#10 +
            'When set to isFile, FindSubControl uses a (background) bmp from disk or "externally-rendered" In-Mem file system.';
end;


function GetPropertyHint_FindControl_SourceFileName: string;
begin
  Result := 'Used when ImageSource is set to isFile.' + #13#10 +
            'This is a full path to a (background) bmp from disk or "externally-rendered" In-Mem file system.';
end;


function GetPropertyHint_FindControl_ImageSourceFileNameLocation: string;
begin
  Result := 'When ImageSource is set to isFile, ImageSourceFileNameLocation selects between disk and "externally-rendered" In-Mem file system.' + #13#10 +
            'This is the (background) bmp, where the txt/bmp/pmtv are searched on.';
end;


function GetPropertyHint_FindControl_PrecisionTimeout: string;
begin
  Result := 'When PrecisionTimeout is set to True, the match bitmap algorithm is stopped by timeout.' + #13#10 +
            'Otherwise, the timeout is verified between whole match attempts only, which usually take longer than the configured timeout.' + #13#10 +
            'When using this option, the bitmap algorithm requires slightly more CPU overhead.' + #13#10 +
            'This option might be required to be set to True, on plugins which implement timeouts on FindSubControl calls.';
end;


function GetPropertyHint_FindControl_FullBackgroundImageInResult: string;
begin
  Result := 'When True, the resulted debugging image contains the initial background image.' + #13#10 +
            'Otherwise, only the debuging info is present, i.e. the searched area or found area, text color if applicable etc.' + #13#10 +
            'This is useful to set to False, when requesting a large resulted image.';
end;


function GetPropertyHint_FindControl_MatchByHistogramSettings: string;
begin
  Result := 'Settings available when matching by histogram.';
end;


function GetPropertyHint_FindControl_EvaluateTextCount: string;
begin
  Result := 'When -1, the searched text is not evaluated either until no more variables are extracted from it, or a hardcoded number of iterations (e.g. 1000) is reached.' + #13#10 +
            'When 0, the searched text is not evaluated. This allows a string, which contains the "$" character to be searched as it is.' + #13#10 +
            'When greater than 0, that number of iterations is used to count how many times the string is evaluated.';
end;


function GetPropertyHint_FindControl_CropFromScreenshot: string;
begin
  Result := 'When False, the screenshot is taken directly from the selected component, using its handle.' + #13#10 +
            'This allows capturing the full "component image", although it may be fully or partially overlapped.' + #13#10 +
            'As a limitation, not all windows/controls support this kind of screenshot.' + #13#10#13#10 +
            'When True, a full screenshot is taken, then it is cropped by the desired coordinates.' + #13#10 +
            'This approach is a bit slower, but it allows capturing windows/controls with different device contexts.';
end;


function GetPropertyHint_FindControl_ThreadCount: string;
begin
  Result := 'This property controls how to execute the bitmap matching algorithm when using brute force searching.' + #13#10 +
            'When 0 (or negative), the algorithm runs in the UI thread.' + #13#10 +
            'When greater than 0 (limited to 255), that number is used to create separate threads, which are started together and search on different sections of the background bitmap.' + #13#10 +
            'These sections are horizontal bands, "cropped" from the background bitmap. The performance gain is visible on large backgrounds.';
end;


{$IFDEF SubProperties}
  function GetPropertyHint_FindControl_MatchCriteria_WillMatchText: string;
  begin
    Result := 'The actual text being matched against.' + #13#10 +
              'It is used by FindControl, when WillMatchText or WillMatchBitmapText properties are True.';
  end;


  function GetPropertyHint_FindControl_MatchCriteria_WillMatchBitmapText: string;
  begin
    Result := 'When selecting FindSubControl action, only bitmaps can be matched (BMP Text or BMP Files).' + #13#10 +
              'A SubControl does not have a handle of its own, it is a part of a control.' + #13#10 +
              'The $Control_Left$, $Control_Top$, $Control_Right$, $Control_Bottom$ variables ar set to the subcontrol offset. $Control_Width$, $Control_Height$ are set by its size.';
  end;


  function GetPropertyHint_FindControl_MatchCriteria_SearchForControlMode: string;
  begin
    Result := 'With "Generate Grid", the application generates a grid of points, where it queries for a window/control.' + #13#10 +
              'With "Enumerate Windows", it lists all top-level windows and matches their caption and/or class.' + #13#10 +
              'With "Find Window", both class and caption have to match and no wildcard is available.'
  end;
{$ENDIF}


{$IFDEF SubProperties}
  const
    CAdditonalInfo_InitialRectangle =  #13#10 +
              'The current selection calculations expect that Left, Top, Right and Bottom subproperties use $Control_Left$, $Control_Top$, $Control_Right$ and $Control_Bottom$ variables.' + #13#10 +
              'If they are not used, then please manually clear the values of these variables in the list (Debugging tab), and then set the area.';
  function GetPropertyHint_FindControl_InitialRectangle(AEdge: string): string;
  begin
    Result := AEdge + ' edge of the search area. Variable replacements are available.' + #13#10 +
              CAdditonalInfo_InitialRectangle;
  end;


  function GetPropertyHint_FindControl_InitialRectangle_Left: string;
  begin
    Result := GetPropertyHint_FindControl_InitialRectangle('Left');
  end;


  function GetPropertyHint_FindControl_InitialRectangle_Top: string;
  begin
    Result := GetPropertyHint_FindControl_InitialRectangle('Top');
  end;


  function GetPropertyHint_FindControl_InitialRectangle_Right: string;
  begin
    Result := GetPropertyHint_FindControl_InitialRectangle('Right');
  end;


  function GetPropertyHint_FindControl_InitialRectangle_Bottom: string;
  begin
    Result := GetPropertyHint_FindControl_InitialRectangle('Bottom');
  end;


  function GetPropertyHint_FindControl_InitialRectangle_Offsets: string;
  begin
    Result := 'Offsets can be used to limit or extend the search area.' + #13#10 +
              'They are relative to the evaluated versions of Left/Top/Right/Bottom edges of search area.' + #13#10#13#10 +
              'These values should be valid (i.e. configured) before executing the action, not after.' + #13#10 +
              'When setting the search area and the offset values, make sure the previous action is the last executed action.' + #13#10 +
              'Also it has to be successfully executed. Otherwise, the reference values will be wrong.' + #13#10 +
              CAdditonalInfo_InitialRectangle;
  end;
{$ENDIF}

{$IFDEF SubProperties}
  function GetPropertyHint_FindSubControl_MatchByHistogramSettings_MinPercentColorMatch: string;
  begin
    Result := 'MinPercentColorMatch sets how much of the searched bitmap histogram data should be found in background bitmap histogram data.' + #13#10 +
              'Only identical colors are compared (the most significant ones). Each of them should match at least this amount.' + #13#10 +
              'The percent is calculated as the ratio between the number of pixels of a particular color, from searched bitmap,' + #13#10 +
              'and the number of pixels of the same color, from the background bitmap, in a zone, defined by the search grid.' + #13#10 +
              'The value is expected to be from 0 to 100 (without the "%" symbol).';
  end;


  function GetPropertyHint_FindSubControl_MatchByHistogramSettings_MostSignificantColorCountInSubBmp: string;
  begin
    Result := 'MostSignificantColorCountInSubBmp sets how many of the most significant colors of the searched bitmap histogram, are compared.' + #13#10 +
              'Both histograms are sorted automatically, so that the most significant colors are compared only (those colors with the most number of pixels).' + #13#10 +
              'If at least one of the configured most significant colors is not found, the algorithm stops (the action fails).' + #13#10 +
              'If all of the comparisons are above the configured MinPercentColorMatch value, the algorithm proceeds with "bruteforce" matching in that particular zone.' + #13#10 +
              'The default value is 10 and should be less than or equal to MostSignificantColorCountInBackgroundBmp.';
  end;


  function GetPropertyHint_FindSubControl_MatchByHistogramSettings_MostSignificantColorCountInBackgroundBmp: string;
  begin
    Result := 'MostSignificantColorCountInBackgroundBmp sets how many of the most significant colors of the background bitmap histograms, are available to be compared.' + #13#10 +
              'Both histograms are sorted automatically, so that the most significant colors are compared only (those colors with the most number of pixels).' + #13#10 +
              'If at least one of the configured most significant colors is not found, the algorithm stops (the action fails).' + #13#10 +
              'If all of the comparisons are above the configured MinPercentColorMatch value, the algorithm proceeds with "bruteforce" matching in that particular zone.' + #13#10 +
              'The default value is 15 and should be greater than or equal to MostSignificantColorCountInSubBmp.' + #13#10 +
              'This value has to be greater than or equal to MostSignificantColorCountInSubBmp, because the zone to be compared may have a histogram with different significant colors.';
  end;
{$ENDIF}


function GetPropertyHint_SetText: string;
begin
  Result := 'Most edit boxes and combo boxes can be set, using the first two options.' + #13#10 +
            'However, depending on their usage on the target application, this approach might not be enough.' + #13#10 +
            'For edit boxes, the action can be configured to use key strokes.' + #13#10 +
            'For combo boxes, this action will have to be replaced by multiple actions, to open the box, finding text, selecting etc.';
end;


function GetPropertyHint_SetText_Text: string;
var
  i: Integer;
begin
  Result := 'The proper control type has to be selected, for the proper API call. Uses $Control_Handle$ variable.' + #13#10 +
            'HTTP calls are available, as var values, using the following format: $http://<server:port>/[params]$' + #13#10 +
            'For special keys, please use from the following available replacements:'+ #13#10#13#10;

  for i := 0 to CSpecialKeyCount - 1 do
  begin
    Result := Result + '$' + CSpecialKeyReplacements[i] + '$';

    if i < CSpecialKeyCount - 1 then
      Result := Result + ', ';

    if i and $F = $F then
      Result := Result + #13#10;
  end;

  Result := Result + #13#10;
  Result := Result + #13#10;
  Result := Result + 'As a limitation, when using special keys, it might be required that the other keys to be also used as special, e.g.: $ControlDown$$Space$$ControlUp$';
end;


function GetPropertyHint_SetText_ControlType: string;
begin
  Result := 'Uses WM_SETTEXT or CB_SELECTSTRING messages or emulates keystrokes.' + #13#10 +
            'When using keystrokes, the target control has to be already focused (e.g. by a click action).';
end;


function GetPropertyHint_DelayBetweenKeyStrokes: string;
begin
  Result := 'For keystrokes only. Measured in ms.'#13#10'A value of 0 or empty string, results in no delay.' + #13#10 +
            'When using keystrokes, the target control has to be already focused (e.g. by a click action).';
end;


function GetPropertyHint_Count: string;
begin
  Result := 'Repeats setting/sending the keystrokes "Count" times.' + #13#10 +
            'Replacements are available.' + #13#10 +
            'Valid range: 0..65535';
end;


function GetPropertyHint_CallTemplate_TemplateFileName: string;
begin
  Result := 'Replacements are available.' + #13#10 +
            'Click the arrow button to browse templates from the local dir.';
end;


function GetPropertyHint_CallTemplate_EvaluateBeforeCalling: string;
begin
  Result := 'If unchecked, the values are passed as strings.';
end;


function GetPropertyHint_CallTemplate_CallTemplateLoop: string;
begin
  Result := 'What does not work, is closing subtemplates when remote debugging.' + #13#10 +
            'So, do not click the stop button when remote debugging CallTemplate actions with loops.';
end;

{$IFDEF SubProperties}

  function GetPropertyHint_CallTemplate_CallTemplateLoop_Counter: string;
  begin
    Result := 'Variable, used as loop counter.' + #13#10 + 'Replacements are available';
  end;


  function GetPropertyHint_CallTemplate_CallTemplateLoop_InitValue: string;
  begin
    Result := 'Initialization value for the loop counter.' + #13#10 + 'Replacements are available';
  end;


  function GetPropertyHint_CallTemplate_CallTemplateLoop_EndValue: string;
  begin
    Result := 'Replacements are available.' + #13#10 +
              'If the expression evaluator can''t properly parse this string, please use a SetVar action to break down the expression into smaller pieces.' + #13#10 +
              'For example, $Diff($ItemCount($RemoteVars$)$,1)$ can be split into the following SetVar items (both set to be evaluated):' + #13#10 +
              '$RemoteVarsCount$ = $ItemCount($RemoteVars$)$  $LastItemIndex$ = $Diff($RemoteVarsCount$,1)$';
  end;


  function GetPropertyHint_CallTemplate_CallTemplateLoop_Direction: string;
  begin
    Result := 'Loop direction can be:' + #13#10#13#10 +
              'Inc - when the counter should be incremented.' + #13#10 +
              'Dec - when the counter should be decremented.' + #13#10 +
              'Auto - when the init and the end values can be anything, so the counter direction has to be evaluated before starting the loop.';
  end;


  function GetPropertyHint_CallTemplate_CallTemplateLoop_EvalBreakPosition: string;
  begin
    Result := 'The "Break" call can be made, before or after the actual "CallTemplate" call.';
  end;
{$ENDIF}


function GetPropertyHint_WindowOperations_NewXY: string;
begin
  Result := 'Evaluates $Control_Left$ and $Control_Top$ variables and updates "New X" and "New Y" to them.';
end;


function GetPropertyHint_WindowOperations_NewWidthHeight: string;
begin
  Result := 'Evaluates $Control_Width$ and $Control_Height$ variables and updates "New Width" and "New Height" to them.';
end;


function GetPropertyHint_LoadSetVarFromFile_FileName: string;
begin
  Result := 'Filename to load var values from, for SetVar action.';
end;


function GetPropertyHint_LoadSetVarFromFile_SetVarActionName: string;
begin
  Result := 'Name of the SetVar action, in the current action template, used for specifying the vars to be loaded from file.' + #13#10 +
            'Only the left column of SetVar action is required.';
end;


function GetPropertyHint_SaveSetVarToFile_FileName: string;
begin
  Result := 'Filename to save var values to, from SetVar action.';
end;


function GetPropertyHint_SaveSetVarToFile_SetVarActionName: string;
begin
  Result := 'Name of the SetVar action, in the current action template, used for specifying the vars to be saved to file.' + #13#10 +
            'Only the left column of SetVar action is required.';
end;


function GetPropertyHint_Plugin_FileName: string;
begin
  Result := 'Filename of the dll.' + #13#10 +
            'When changing value of this property, the dll is loaded to get its list of custom properties.' + #13#10 +
            'That list is also loaded on template loading.';

  {$IFDEF MemPlugins}
    Result := Result + #13#10#13#10 +
              'If this path starts with "Mem:\" (without quotes),' + #13#10 +
              'the plugin and its dbgsym file must exist in the InMem file system for plugins.' + #13#10 +
              'Plugins and their related files can be sent to UIClicker''s InMem FS, via the HTTP API, using the SetMemPluginFile command.' + #13#10#13#10 +

              'It is also possible to send multiple plugins and their files, in a single request, by archiving them and using the SetMemPluginArchiveFile command.' + #13#10 +
              'Sending plugin archives has the advantage of using encryption, compression and verifying the archive integrity with a custom hash algorithm.' + #13#10 +
              'Another advantage is the ability to archive both 32-bit and 64-bit plugins, then let UIClicker extract only the matching ones.' + #13#10 +
              'If a file from an archive, has its path starting with "i386-win32", it is extracted to the InMem FS, by a 32-bit UIClicker.' + #13#10 +
              'If a file from an archive, has its path starting with "x86_64-win64", it is extracted to the InMem FS, by a 64-bit UIClicker.' + #13#10 +
              'A file, from an archive, with a path starting with "i386-win32", is ignored (not extracted), by a 64-bit UIClicker.' + #13#10 +
              'A file, from an archive, with a path starting with "x86_64-win64", is ignored (not extracted), by a 32-bit UIClicker.' + #13#10 +
              'All the files, from the "i386-win32" or "x86_64-win64" directories, are extracted outside these directories (their path will not be prefixed by the directories name).' + #13#10 +
              'All the other files (outside the "i386-win32" and "x86_64-win64" directories), from inside the archive, are extracted as they are.' + #13#10#13#10 +

              'The SetMemPluginArchiveFile command has a parameter, IsDecDecHash, which decides what type of archive is being sent.' + #13#10 +
              'If set to 1 (i.e. True), it means that the archive contains plugin(s) for decryption, decompression and custom hashing.' + #13#10 +
              'These types of plugins, called DecDecHash (and all the other files in the archive) will be extracted in separate InMem file systems (one FS / archive).' + #13#10 +
              'They do not appear in the selection menu, from the arrow button of the FileName property of a Plugin action (the property with this tooltip).' + #13#10 +
              'If set to 0 (i.e. False), all the files (except .dll and .dbgsym) are extracted to the main InMem FS for plugins.' + #13#10 +
              'The .dll files are extracted to their own InMem FSes, while the .dbgsym files, to the first InMem FS.' + #13#10 +
              'Ordinary plugins (i.e. non DecDecHash) can access the main FS for plugins, during action execution (via ExecutePlugin function).' + #13#10 +
              'DecDecHash plugins can access their own FSes, and read configuration files, when loaded for decryption, decompression and custom hashing.' + #13#10 +
              'The reason to extract all .dbgsym files to the same FS, is because a new FS is allocated on every request, so less FSes are used in this way.' + #13#10 +
              'Upon extraction, files are prefixed with the "Mem:\" string automatically. They should not contain this prefix inside the archive.' + #13#10 +
              'An archive uses decryption, decompression and custom hashing, if their respective parameters are set to valid and existing plugin names in SetMemPluginArchiveFile command.' + #13#10 +
              'If these parameters are empty strings, their algorithms are not used. If custom hashing is not used, then the archive is verifyed with MD5.' + #13#10#13#10 +

              'The DecDecHash plugins have the same API (for backwards compatibility) as ordinary action plugins, plus two additional functions.' + #13#10 +
              'One of the functions does the decryption, decompression or custom hashing, and the other, reads configuration files (if any) from the InMem FS of that plugin.' + #13#10 +
              'The DecDecHash plugins are expected to exist in their InMem file systems, in order for an archive to be successfully extracted, if it requires them.' + #13#10 +
              'Unfortunately, the security of the first sent decryption plugin is low, because it will have to be sent in plain (i.e. unencrypted archive).' + #13#10 +
              'Although plugins can access the disk, only plugins from these special InMem file systems can be used for decryption, decompression and custom hashing.' + #13#10 +
              'Unless specifically implemented, all the above mentioned plugins remain in memory, during the existence of the UIClicker process.' + #13#10 +
              'The decryption, decompression and custom hashing are implemented in plugins, while the custom archiving component is provided by UIClicker (MemArchive.pas).' + #13#10 +
              'The tool(s) for creating the archives will have to use this archiving component (or a compatible implementation) and the matching encryption and compression algorithms.'
              ;
  {$ELSE}
    Result := Result + #13#10#13#10 +
              'Memory plugins can be used if UIClicker is built using the MemPlugins compiler directive, present at project level.';
  {$ENDIF}
end;


function GetPropertyHint_Sleep_Value: string;
begin
  Result := 'Sleep values, lower than 1, are ignored.' + #13#10 +
            'Use this action only as a last resort (e.g. blinking or resizing controls/windows).' + #13#10 +
            'Waiting for the proper event or control property, is the right way to solve a race condition. Use the "sleep" action if the event/property is not available.';
end;


function GetPropertyHint_SetVar_FailOnException: string;
begin
  Result := 'If set to True, the action fails on the first exception.' + #13#10 +
            'Otherwise, the action continues with a variable, set to the error message.' + #13#10 +
            'As an example, when calling $RenderBmpExternally()$, the result might be a connection error message. If FailOnException is True, the action fails.';
end;


function GetPropertyHint_EditTemplate_Operation: string;
begin
  Result := 'There are various operations available for EditTemplate. Some of them return an error in $ExecAction_Err$ variable when the action fails.' + #13#10;
            Result := Result +
            'If WhichTemplate is set to etwtSelf, the EditTemplate execution is applied to this template. The actions have to exist in the current template (except when creating a new one).' + #13#10;
            Result := Result +
            '    The template is not automatically saved. It requires another EditTemplate action, set to etoSaveTemplate operation. This allows multiple EditTemplate editings on a single save.' + #13#10#13#10;
            Result := Result +
            'If WhichTemplate is set to etwtOther, the template is automatically loaded, then saved when successfully edited. An example of an operation which does not edit, is etoExecuteAction.' + #13#10#13#10;
            Result := Result +
            'etoNewAction - Creates a new action and places it at the end of the template. The action name should not be in the list, otherwise it returns an error.' + #13#10;
            Result := Result +
            '    All the available properties from this EditTemplate action will be copied to the new action.' + #13#10#13#10;
            Result := Result +
            'etoUpdateAction - Updates an existing action with the properties from this EditTemplate action.' + #13#10;
            Result := Result +
            'etoMoveAction - Moves the action, specified by the EditedActionName property, to the position of the action, specified by the NewActionName property.' + #13#10;
            Result := Result +
            '    If NewActionName is '''', then the action is moved to the end of the list. If the action, specified by NewActionName, does not exist, it returns an error.' + #13#10#13#10;
            Result := Result +
            'etoDeleteAction - Deletes an action, specified by the EditedActionName property, from the template.' + #13#10#13#10;
            Result := Result +
            'etoRenameAction - Renames the action, specified by the EditedActionName property, to the name, specified by the NewActionName property.' + #13#10;
            Result := Result +
            '    If the new name already exists, it returns an error.' + #13#10#13#10;
            Result := Result +
            'etoEnableAction - Enables an action, specified by the EditedActionName property.' + #13#10#13#10;
            Result := Result +
            'etoDisableAction - Disables an action, specified by the EditedActionName property.' + #13#10#13#10;
            Result := Result +
            'etoGetProperty - Returns all the checked properties of an action, specified by the EditedActionName property, as variables, in the form of $Property_<PropertyName>_Value$.' + #13#10#13#10;
            Result := Result +
            'etoSetProperty - Sets all the checked properties of an action, specified by the EditedActionName property.' + #13#10#13#10;
            Result := Result +
            'etoSetCondition - Sets the action condition of an action, specified by the EditedActionName property.' + #13#10#13#10;
            Result := Result +
            'etoSetTimeout - Sets the action timeout of an action, specified by the EditedActionName property.' + #13#10#13#10;
            Result := Result +
            'etoExecuteAction - Executes the action, specified by the EditedActionName property, in the context (list of variables) of the current template.' + #13#10#13#10;
            Result := Result +
            'etoSaveTemplate - Saves the current template. Used only when the WhichTemplate property is set to etwtSelf.';
end;


function GetPropertyHint_EditTemplate_WhichTemplate: string;
begin
  Result := 'The action can edit this template or load an existing template and modify it.';
end;


function GetPropertyHint_EditTemplate_TemplateFileName: string;
begin
  Result := 'Used when WhichTemplate is set to etwtOther.';
end;


function GetPropertyHint_EditTemplate_EditedActionName: string;
begin
  Result := 'Used to identify the action to be created, edited or deleted.';
end;


function GetPropertyHint_EditTemplate_EditedActionType: string;
begin
  Result := 'Used when getting or setting action properties.';
end;


function GetPropertyHint_EditTemplate_EditedActionCondition: string;
begin
  Result := 'Used when getting or setting action condition.';
end;


function GetPropertyHint_EditTemplate_EditedActionTimeout: string;
begin
  Result := 'Used when getting or setting action timeout.';
end;


function GetPropertyHint_EditTemplate_NewActionName: string;
begin
  Result := 'Used when moving, duplicating and renaming an action.';
end;


  function GetReadOnlyHintPart_EditTemplate_Properties: string;
  begin
    Result := 'This property is read-only under the "Action specific" category, but it can be edited under the "' + CCategory_Name_EditedAction + '" category.';
  end;


function GetPropertyHint_EditTemplate_ListOfEditedProperties: string;
begin
  Result := '#18-separated list of PropertyName=PropertyValue items. This is a serialized version of the new/updated action.' + #13#10 +
            'When needed for editing, the #18 characters can be obtained from the "ListOfEditedProperties" property, under the "' + CCategory_Name_ActionSpecific + '" category.' + #13#10 +
            'When editing this property, the content can be copied from the "' + CCategory_Name_ActionSpecific + '" category, then edited using a text editor, then pasted under the "' + CCategory_Name_EditedAction + '" category.' + #13#10 +
            'One of the items is "ListOfEnabledProperties", which contains a #4#5 separated list of property names. All the names, present in this list will control which propertie are going to be checked in the new/updated action.' + #13#10 +
            'Please make sure you don''t set out of range values, because they are not validated and may lead to bad/unusable settings.' + #13#10#13#10 +
            'Example of creating/updating an EditTemplate action, with #18 replaced by #13#10 (i.e. "" replaced by Return):' + #13#10#13#10 +
            'Operation=3' + #13#10 +
            'WhichTemplate=1' + #13#10 +
            'TemplateFileName=second_fnm.clktmpl' + #13#10 +
            'ListOfEnabledProperties=NewActionNameOperationEditedActionNameWhichTemplateTemplateFileNameEditedActionConditionEditedActionTypeListOfEditedPropertiesListOfEnabledPropertiesShouldSaveTemplate' + #13#10 +
            'EditedActionName=CopiedAction' + #13#10 +
            'EditedActionType=11' + #13#10 +
            'EditedActionCondition=$Stat$<>Allowed' + #13#10 +
            'EditedActionTimeout=123' + #13#10 +
            'NewActionName=triple' + #13#10 +
            'ShouldSaveTemplate=0' + #13#10#13#10 +
            GetReadOnlyHintPart_EditTemplate_Properties;
end;


function GetPropertyHint_EditTemplate_ListOfEnabledProperties: string;
begin
  Result := 'CRLF separated list of values. (Currently limited to CRLF, which cannot be properly input.)' + #13#10 +
            GetReadOnlyHintPart_EditTemplate_Properties;
end;


function GetPropertyHint_EditTemplate_ShouldSaveTemplate: string;
begin
  Result := 'When Operation is set to etoSaveTemplate and WhichTemplate is set to etwtSelf, this property control weather to save the template or not.' + #13#10 +
            'If ShouldSaveTemplate is True and the template does not have a name, i.e. has never been saved, the action fails, because it won''t prompt the user for a filename.' + #13#10 +
            'This property does not affect the saving of a template which is already on disk (i.e. used when WhichTemplate is set to etwtOther). That template is attempted to be saved anyway.';
end;

end.

