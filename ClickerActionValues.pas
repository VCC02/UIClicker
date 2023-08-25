{
    Copyright (C) 2022 VCC
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
  CCategoryCount = 2;

  CCategory_Common = 0;
  CCategory_ActionSpecific = 1;

  CCategories: array[0..CCategoryCount - 1] of string = ('Common', 'Action specific');
  CPropCount_Common = 4;  //Action name, Action type, Action Timeout, StopOnError

  //Properties (counts)
  CPropCount_Click = 22;
  CPropCount_ExecApp = 7;
  CPropCount_FindControl = 19;
  CPropCount_FindSubControl = 19;
  CPropCount_SetText = 2;
  CPropCount_CallTemplate = 4;
  CPropCount_Sleep = 1;
  CPropCount_SetVar = 1;
  CPropCount_WindowOperations = 7;

  CMainPropCounts: array[0..Ord(High(TClkAction))] of Integer = (
    CPropCount_Click,
    CPropCount_ExecApp,
    CPropCount_FindControl,
    CPropCount_FindSubControl,
    CPropCount_SetText,
    CPropCount_CallTemplate,
    CPropCount_Sleep,
    CPropCount_SetVar,
    CPropCount_WindowOperations
  );

  //Sub properties (counts)
  CPropCount_FindControlMatchCriteria = 6;
  CPropCount_FindControlMatchBitmapText = 16;
  CPropCount_FindControlMatchBitmapAlgorithmSettings = 4;
  CPropCount_FindControlInitialRectangle = 8;

  CPropCount_CallTemplateLoop = 7;

  //PropIndex consts
  CMain_ActionName_PropIndex = 0; //property index in Action structure
  CMain_ActionTimeout_PropIndex = 2; //property index in Action structure
  CMain_ActionCondition_PropIndex = 3; //property index in Action structure

  CExecApp_PathToApp_PropIndex = 0;     //property index in ExecApp structure
  CExecApp_ListOfParams_PropIndex = 1;  //property index in ExecApp structure

  CFindControl_MatchCriteria_PropIndex = 0; //property index in FindControl structure
  CFindControl_MatchText_PropIndex = 2;      //property index in FindControl structure
  CFindControl_MatchClassName_PropIndex = 3;  //property index in FindControl structure
  CFindControl_MatchBitmapText_PropIndex = 6; //property index in FindControl structure
  CFindControl_MatchBitmapFiles_PropIndex = 7; //property index in FindControl structure   - list of files
  CFindControl_MatchBitmapAlgorithmSettings_PropIndex = 9;
  CFindControl_InitialRectangle_PropIndex = 10;
  CFindControl_UseWholeScreen_PropIndex = 11;
  CFindControl_MatchPrimitiveFiles_PropIndex = 18; //property index in FindControl structure   - list of files

  CCallTemplate_TemplateFileName_PropIndex = 0; //property index in CallTemplate structure
  CCallTemplate_ListOfCustomVarsAndValues_PropIndex = 1;
  CCallTemplate_CallTemplateLoop_PropIndex = 3; //property index in CallTemplate structure

  CCallTemplate_CallTemplateLoopProperties_BreakCondition_PropItemIndex = 5;

  CFindControl_MatchCriteria_WillMatchBitmapText_PropItemIndex = 2;
  CFindControl_MatchCriteria_SearchForControlMode_PropItemIndex = 4;

  CFindControl_MatchBitmapText_ForegroundColor_PropItemIndex = 0;   //property index in FindControl.MatchBitmapText structure
  CFindControl_MatchBitmapText_BackgroundColor_PropItemIndex = 1;   //property index in FindControl.MatchBitmapText structure
  CFindControl_MatchBitmapText_FontName_PropItemIndex = 2;   //property index in FindControl.MatchBitmapText structure
  CFindControl_MatchBitmapText_ProfileName_PropItemIndex = 11;   //property index in FindControl.MatchBitmapText structure

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

  CSetVar_ListOfVarNamesValuesAndEvalBefore = 0;

  CCallTemplate_CallTemplateLoop_Enabled_PropItemIndex = 0;
  CCallTemplate_CallTemplateLoop_Counter_PropItemIndex = 1;
  CCallTemplate_CallTemplateLoop_InitValue_PropItemIndex = 2;
  CCallTemplate_CallTemplateLoop_EndValue_PropItemIndex = 3;
  CCallTemplate_CallTemplateLoop_Direction_PropItemIndex = 4;
  CCallTemplate_CallTemplateLoop_BreakCondition_PropItemIndex = 5;
  CCallTemplate_CallTemplateLoop_EvalBreakPosition_PropItemIndex = 6;

  CWindowOperations_NewX = 1;
  CWindowOperations_NewY = 2;
  CWindowOperations_NewWidth = 3;
  CWindowOperations_NewHeight = 4;

  CDTString = 'String';
  CDTEnum = 'Enum';
  CDTBool = 'Boolean';
  CDTInteger = 'Integer';
  CDTStructure = 'Structure';
  CDTArray = 'Array';


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
    (Name: 'MouseWheelAmount'; EditorType: etSpinText; DataType: CDTString)
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
    (Name: 'MatchPrimitiveFiles'; EditorType: etFilePathWithArrow; DataType: CDTArray)
  );

  {$IFDEF SubProperties}
    CFindControl_MatchCriteriaProperties: array[0..CPropCount_FindControlMatchCriteria - 1] of TOIPropDef = (
      (Name: 'WillMatchText'; EditorType: etBooleanCombo; DataType: CDTBool),
      (Name: 'WillMatchClassName'; EditorType: etBooleanCombo; DataType: CDTBool),
      (Name: 'WillMatchBitmapText'; EditorType: etBooleanCombo; DataType: CDTBool),    //Description:   When selecting FindSubControl action, only bitmaps can be matched (BMP Text or BMP Files).  A SubControl does not have a handle of its own, it is a part of a control.  The $Control_Left$, $Control_Top$, $Control_Width$, $Control_Height$, $Control_Right$, $Control_Bottom$ variables ar set with the subcontrol offset.
      (Name: 'WillMatchBitmapFiles'; EditorType: etBooleanCombo; DataType: CDTBool),
      (Name: 'WillMatchPrimitiveFiles'; EditorType: etBooleanCombo; DataType: CDTBool),
      (Name: 'SearchForControlMode'; EditorType: etEnumCombo; DataType: CDTEnum)
    );

    CFindControl_MatchBitmapTextProperties: array[0..CPropCount_FindControlMatchBitmapText - 1] of TOIPropDef = (
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
      (Name: 'CropBottom'; EditorType: etSpinText; DataType: CDTString)
    );

    CFindControl_MatchBitmapAlgorithmSettingsProperties: array[0..CPropCount_FindControlMatchBitmapAlgorithmSettings - 1] of TOIPropDef = (
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
  {$ENDIF}

  CSetTextProperties: array[0..CPropCount_SetText - 1] of TOIPropDef = (   //Description:  Most edit boxes and combo boxes can be set, using the first two options.  However, depending on their usage on the target application, this approach might not be enough.  For edit boxes, the action can be configured to use key strokes.  For combo boxes, this action will have to be replaced by multiple actions, to open the box, finding text, selecting etc.
    (Name: 'Text'; EditorType: etText; DataType: CDTString),                                    //Description:  The proper control type has to be selected, for the proper API call. Uses $Control_Handle$ variable.    HTTP calls are available, as var values, using the following format: $http://<server:port>/[params]$
    (Name: 'ControlType'; EditorType: etEnumCombo; DataType: CDTEnum)                         //Description:  Uses WM_SETTEXT or CB_SELECTSTRING messages or emulates keystrokes..
  );

  CCallTemplateProperties: array[0..CPropCount_CallTemplate - 1] of TOIPropDef = (
    (Name: 'TemplateFileName'; EditorType: etTextWithArrow; DataType: CDTString),           //Description:  Replacements are available    //Description[arrow button]:   Templates from the local dir
    (Name: 'ListOfCustomVarsAndValues'; EditorType: etUserEditor; DataType: CDTStructure),
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
    (Name: 'ListOfVarNamesValuesAndEvalBefore'; EditorType: etUserEditor; DataType: CDTStructure) //structure   (no sub properties)
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

type
  TArrayOfProperties = array[0..0] of TOIPropDef;
  PArrayOfProperties = ^TArrayOfProperties;
  TArrayOfPropertiesArr = array[0..Ord(High(TClkAction))] of PArrayOfProperties;

  TGetActionValueStrFunc = function(AAction: PClkActionRec; APropertyIndex: Integer): string;
  TGetActionValueStrFuncArr = array[TClkAction] of TGetActionValueStrFunc;
  TGetFindControlValueStrFuncArr = array[0..CPropCount_FindControl - 1] of TGetActionValueStrFunc;
  TGetCallTemplateValueStrFuncArr = array[0..CPropCount_CallTemplate - 1] of TGetActionValueStrFunc;

  TSetActionValueStrProc = procedure(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  TSetActionValueStrProcArr = array[TClkAction] of TSetActionValueStrProc;
  TSetFindControlValueStrProcArr = array[0..CPropCount_FindControl - 1] of TSetActionValueStrProc;
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
    @CClickProperties,
    @CExecAppProperties,
    @CFindControlProperties,
    @CFindControlProperties,
    @CSetTextProperties,
    @CCallTemplateProperties,
    @CSleepProperties,
    @CSetVarProperties,
    @CWindowOperationsProperties
  );


function GetActionValueStr_Action(AAction: PClkActionRec; APropertyIndex: Integer): string;

function GetActionValueStr_Click(AAction: PClkActionRec; APropertyIndex: Integer): string;
function GetActionValueStr_ExecApp(AAction: PClkActionRec; APropertyIndex: Integer): string;
function GetActionValueStr_FindControl(AAction: PClkActionRec; APropertyIndex: Integer): string;  //used also for FindSubControl
function GetActionValueStr_SetText(AAction: PClkActionRec; APropertyIndex: Integer): string;
function GetActionValueStr_CallTemplate(AAction: PClkActionRec; APropertyIndex: Integer): string;
function GetActionValueStr_Sleep(AAction: PClkActionRec; APropertyIndex: Integer): string;
function GetActionValueStr_SetVar(AAction: PClkActionRec; APropertyIndex: Integer): string;
function GetActionValueStr_WindowOperations(AAction: PClkActionRec; APropertyIndex: Integer): string;

{$IFDEF SubProperties}
  function GetActionValueStr_FindControl_MatchCriteria(AAction: PClkActionRec; APropertyIndex: Integer): string;
  function GetActionValueStr_FindControl_MatchBitmapText(AAction: PClkActionRec; APropertyIndex: Integer): string;
  function GetActionValueStr_FindControl_MatchBitmapAlgorithmSettings(AAction: PClkActionRec; APropertyIndex: Integer): string;
  function GetActionValueStr_FindControl_InitialRectangle(AAction: PClkActionRec; APropertyIndex: Integer): string;

  function GetActionValueStr_CallTemplate_CallTemplateLoop(AAction: PClkActionRec; APropertyIndex: Integer): string;
{$ENDIF}


procedure SetActionValueStr_Action(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);

procedure SetActionValueStr_Click(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
procedure SetActionValueStr_ExecApp(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
procedure SetActionValueStr_FindControl(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);  //used also for FindSubControl
procedure SetActionValueStr_SetText(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
procedure SetActionValueStr_CallTemplate(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
procedure SetActionValueStr_Sleep(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
procedure SetActionValueStr_SetVar(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
procedure SetActionValueStr_WindowOperations(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);

{$IFDEF SubProperties}
  procedure SetActionValueStr_FindControl_MatchCriteria(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  procedure SetActionValueStr_FindControl_MatchBitmapText(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  procedure SetActionValueStr_FindControl_MatchBitmapAlgorithmSettings(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  procedure SetActionValueStr_FindControl_InitialRectangle(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);

  procedure SetActionValueStr_CallTemplate_CallTemplateLoop(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
{$ENDIF}


const
  CMainGetActionValueStrFunctions: TGetActionValueStrFuncArr = (
    GetActionValueStr_Click,
    GetActionValueStr_ExecApp,
    GetActionValueStr_FindControl,
    GetActionValueStr_FindControl,
    GetActionValueStr_SetText,
    GetActionValueStr_CallTemplate,
    GetActionValueStr_Sleep,
    GetActionValueStr_SetVar,
    GetActionValueStr_WindowOperations
  );

  CMainSetActionValueStrFunctions: TSetActionValueStrProcArr = (
    SetActionValueStr_Click,
    SetActionValueStr_ExecApp,
    SetActionValueStr_FindControl,
    SetActionValueStr_FindControl,
    SetActionValueStr_SetText,
    SetActionValueStr_CallTemplate,
    SetActionValueStr_Sleep,
    SetActionValueStr_SetVar,
    SetActionValueStr_WindowOperations
  );

  CFindControlGetActionValueStrFunctions: TGetFindControlValueStrFuncArr = (
    GetActionValueStr_FindControl_MatchCriteria,
    nil, //AllowToFail
    nil, //MatchText
    nil, //MatchClassName
    nil, //MatchTextSeparator
    nil, //MatchClassNameSeparator
    GetActionValueStr_FindControl_MatchBitmapText,
    nil, //MatchBitmapFiles
    nil, //MatchBitmapAlgorithm
    GetActionValueStr_FindControl_MatchBitmapAlgorithmSettings,
    GetActionValueStr_FindControl_InitialRectangle,
    nil, //UseWholeScreen
    nil, //ColorError
    nil, //AllowedColorErrorCount
    nil, //WaitForControlToGoAway
    nil, //StartSearchingWithCachedControl
    nil, //CachedControlLeft
    nil, //CachedControlTop
    nil  //MatchPrimitiveFiles
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
    0 //MouseWheelAmount: string;
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
    0  //MatchPrimitiveFiles
  );

  CSetTextEnumCounts: array[0..CPropCount_SetText - 1] of Integer = (
    0, //Text: string;
    Ord(High(TClkSetTextControlType)) + 1
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
    0  //ListOfVarNamesValuesAndEvalBefore
  );

  CWindowOperationsCounts: array[0..CPropCount_WindowOperations - 1] of Integer = (
    Ord(High(TWindowOperation)) + 1,
    0, //NewX,
    0, //NewY,
    0, //NewWidth,
    0, //NewHeight
    0, //NewPositionEnabled,
    0  //NewSizeEnabled: Boolean;
  );


  CPropEnumCounts: array[TClkAction] of PArrayOfEnumCounts = (
    @CClickEnumCounts,
    @CExecAppEnumCounts,
    @CFindControlEnumCounts,
    @CFindControlEnumCounts,
    @CSetTextEnumCounts,
    @CCallTemplateEnumCounts,
    @CSleepEnumCounts,
    @CSetVarEnumCounts,
    @CWindowOperationsCounts
  );


  {$IFDEF SubProperties}
    CFindControl_MatchCriteriaEnumCounts: array[0..CPropCount_FindControlMatchCriteria - 1] of Integer = (
      0, //WillMatchText: Boolean;
      0, //WillMatchClassName: Boolean;
      0, //WillMatchBitmapText: Boolean;
      0, //WillMatchBitmapFiles: Boolean;
      0, //WillMatchPrimitiveFiles: Boolean;
      Ord(High(TSearchForControlMode)) + 1
    );

    CFindControl_MatchBitmapTextEnumCounts: array[0..CPropCount_FindControlMatchBitmapText - 1] of Integer = (
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
      0  //CropBottom: string;
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
    nil  //MouseWheelAmount
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
    nil
  );

  CSetTextEnumStrings: array[0..CPropCount_SetText - 1] of PArrayOfString = (
    nil, //Text: string;
    @CClkSetTextControlTypeStr
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
    nil  //ListOfVarNamesValuesAndEvalBefore
  );

  CWindowOperationsStrings: array[0..CPropCount_WindowOperations - 1] of PArrayOfString = (
    @CWindowOperationStr,
    nil, //NewX,
    nil, //NewY,
    nil, //NewWidth,
    nil, //NewHeight
    nil, //NewPositionEnabled,
    nil  //NewSizeEnabled: Boolean;
  );


  CPropEnumStrings: array[TClkAction] of PArrayOfEnumStrings = (
    @CClickEnumStrings,
    @CExecAppEnumStrings,
    @CFindControlEnumStrings,
    @CFindControlEnumStrings,
    @CSetTextEnumStrings,
    @CCallTemplateEnumStrings,
    @CSleepEnumStrings,
    @CSetVarEnumStrings,
    @CWindowOperationsStrings
  );

  {$IFDEF SubProperties}
    CFindControl_MatchCriteriaEnumStrings: array[0..CPropCount_FindControlMatchCriteria - 1] of PArrayOfString = (
      nil, //WillMatchText: Boolean;
      nil, //WillMatchClassName: Boolean;
      nil, //WillMatchBitmapText: Boolean;
      nil, //WillMatchBitmapFiles: Boolean;
      nil, //WillMatchPrimitiveFiles: Boolean;
      @CSearchForControlModeStr
    );

    CFindControl_MatchBitmapTextEnumStrings: array[0..CPropCount_FindControlMatchBitmapText - 1] of PArrayOfString = (
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
      nil  //CropBottom: string;
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


function GetPropertyHintNoHint: string;

function GetPropertyHint_Click_XClickPointVar: string;
function GetPropertyHint_Click_YClickPointVar: string;
function GetPropertyHint_Click_XOffset: string;
function GetPropertyHint_Click_YOffset: string;
function GetPropertyHint_Click_LeaveMouse: string;

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
function GetPropertyHint_FindControl_UseWholeScreen: string;
function GetPropertyHint_FindControl_ColorError: string;
function GetPropertyHint_FindControl_AllowedColorErrorCount: string;
function GetPropertyHint_FindControl_WaitForControlToGoAway: string;
function GetPropertyHint_FindControl_StartSearchingWithCachedControl: string;
function GetPropertyHint_FindControl_CachedControlLeftTop: string;
function GetPropertyHint_FindControl_MatchPrimitiveFiles: string;

{$IFDEF SubProperties}
  function GetPropertyHint_FindControl_MatchCriteria_MatchBitmapText: string;
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


function GetPropertyHint_SetText: string;

{$IFDEF SubProperties}
  function GetPropertyHint_SetText_Text: string;
  function GetPropertyHint_SetText_ControlType: string;
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

function GetPropertyHint_Sleep_Value: string;


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
    @GetPropertyHintNoHint  // MouseWheelAmount: string;
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
    @GetPropertyHintNoHint, // MatchBitmapAlgorithm: TMatchBitmapAlgorithm;
    @GetPropertyHintNoHint, // MatchBitmapAlgorithmSettings: TMatchBitmapAlgorithmSettings;
    @GetPropertyHintNoHint, // InitialRectangle: TRectString;
    @GetPropertyHint_FindControl_UseWholeScreen, // UseWholeScreen: Boolean;
    @GetPropertyHint_FindControl_ColorError, // ColorError: string;  //string, to allow var replacements
    @GetPropertyHint_FindControl_AllowedColorErrorCount, // AllowedColorErrorCount: string;  //Number of pixels allowed to mismatch
    @GetPropertyHint_FindControl_WaitForControlToGoAway, // WaitForControlToGoAway: Boolean;
    @GetPropertyHint_FindControl_StartSearchingWithCachedControl, // StartSearchingWithCachedControl: Boolean;
    @GetPropertyHint_FindControl_CachedControlLeftTop, // CachedControlLeft: string;
    @GetPropertyHint_FindControl_CachedControlLeftTop, // CachedControlTop: string;
    @GetPropertyHint_FindControl_MatchPrimitiveFiles // MatchPrimitiveFiles: string; //ListOfStrings
  );


  CGetPropertyHint_SetText: array[0..CPropCount_SetText - 1] of TPropHintFunc = (
    @GetPropertyHint_SetText_Text, // Text: string;
    @GetPropertyHint_SetText_ControlType  // ControlType: TClkSetTextControlType;
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
    @GetPropertyHintNoHint //ListOfVarNamesValuesAndEvalBefore
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


  CGetPropertyHint_Actions: TPropHintFuncActionArr = (
    @CGetPropertyHint_Click,
    @CGetPropertyHint_ExecApp,
    @CGetPropertyHint_FindControl,
    @CGetPropertyHint_FindControl,
    @CGetPropertyHint_SetText,
    @CGetPropertyHint_CallTemplate,
    @CGetPropertyHint_Sleep,
    @CGetPropertyHint_SetVar,
    @CGetPropertyHint_WindowOperations
  );


  CGetPropertyHint_FindControlMatchCriteria_Items: array[0..CPropCount_FindControlMatchCriteria - 1] of TPropHintFunc = (
    @GetPropertyHint_FindControl_MatchCriteria_MatchBitmapText, //WillMatchText: Boolean;
    @GetPropertyHintNoHint, //WillMatchClassName: Boolean;
    @GetPropertyHint_FindControl_MatchCriteria_WillMatchBitmapText, //WillMatchBitmapText: Boolean;
    @GetPropertyHintNoHint, //WillMatchBitmapFiles: Boolean;
    @GetPropertyHintNoHint, //WillMatchPrimitiveFiles: Boolean;
    @GetPropertyHint_FindControl_MatchCriteria_SearchForControlMode //SearchForControlMode: TSearchForControlMode;
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
    0: Result := '';  //MatchCriteria
    1: Result := BoolToStr(AAction^.FindControlOptions.AllowToFail, True);
    2: Result := AAction^.FindControlOptions.MatchText;
    3: Result := AAction^.FindControlOptions.MatchClassName;
    4: Result := AAction^.FindControlOptions.MatchTextSeparator;
    5: Result := AAction^.FindControlOptions.MatchClassNameSeparator;
    6: Result := '';  //MatchBitmapText
    7: Result := AAction^.FindControlOptions.MatchBitmapFiles; //ListOfStrings
    8: Result := CMatchBitmapAlgorithmStr[AAction^.FindControlOptions.MatchBitmapAlgorithm];
    9: Result := '';  //MatchBitmapAlgorithmSettings
    10: Result := '';  //InitialRectangle
    11: Result := BoolToStr(AAction^.FindControlOptions.UseWholeScreen, True);
    12: Result := AAction^.FindControlOptions.ColorError;  //string, to allow var replacements
    13: Result := AAction^.FindControlOptions.AllowedColorErrorCount;  //Number of pixels allowed to mismatch
    14: Result := BoolToStr(AAction^.FindControlOptions.WaitForControlToGoAway, True);
    15: Result := BoolToStr(AAction^.FindControlOptions.StartSearchingWithCachedControl, True);
    16: Result := AAction^.FindControlOptions.CachedControlLeft;
    17: Result := AAction^.FindControlOptions.CachedControlTop;
    18: Result := AAction^.FindControlOptions.MatchPrimitiveFiles; //ListOfStrings
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
      2: Result := BoolToStr(AAction^.FindControlOptions.MatchCriteria.WillMatchBitmapText, True);
      3: Result := BoolToStr(AAction^.FindControlOptions.MatchCriteria.WillMatchBitmapFiles, True);
      4: Result := BoolToStr(AAction^.FindControlOptions.MatchCriteria.WillMatchPrimitiveFiles, True);
      5: Result := CSearchForControlModeStr[AAction^.FindControlOptions.MatchCriteria.SearchForControlMode];
      else
        Result := 'unknown';
    end;
  end;

  function GetActionValueStr_FindControl_MatchBitmapText(AAction: PClkActionRec; APropertyIndex: Integer): string;
  var
    PropertyIndexMod, PropertyIndexDiv: Integer;
  begin
    if Length(AAction^.FindControlOptions.MatchBitmapText) = 0 then
    begin
      Result := 'No font profiles';
      Exit;
    end;

    PropertyIndexMod := APropertyIndex mod CPropCount_FindControlMatchBitmapText;
    PropertyIndexDiv := APropertyIndex div CPropCount_FindControlMatchBitmapText;

    case PropertyIndexMod of
      0: Result := AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].ForegroundColor;
      1: Result := AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].BackgroundColor;
      2: Result := AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].FontName;
      3: Result := IntToStr(AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].FontSize);
      4: Result := BoolToStr(AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].Bold, True);
      5: Result := BoolToStr(AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].Italic, True);
      6: Result := BoolToStr(AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].Underline, True);
      7: Result := BoolToStr(AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].StrikeOut, True);
      8: Result := CFontQualityStr[AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].FontQuality];
      9: Result := BoolToStr(AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].FontQualityUsesReplacement, True);
      10: Result := AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].FontQualityReplacement;
      11: Result := AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].ProfileName;
      12: Result := AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].CropLeft;
      13: Result := AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].CropTop;
      14: Result := AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].CropRight;
      15: Result := AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].CropBottom;
      else
        Result := 'unknown';
    end;
  end;


  function GetActionValueStr_FindControl_MatchBitmapAlgorithmSettings(AAction: PClkActionRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := IntToStr(AAction^.FindControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf);
      1: Result := IntToStr(AAction^.FindControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf);
      2: Result := IntToStr(AAction^.FindControlOptions.MatchBitmapAlgorithmSettings.XOffset);
      3: Result := IntToStr(AAction^.FindControlOptions.MatchBitmapAlgorithmSettings.YOffset);
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
{$ENDIF}


function GetActionValueStr_SetText(AAction: PClkActionRec; APropertyIndex: Integer): string;
begin
  case APropertyIndex of
    0: Result := AAction^.SetTextOptions.Text;
    1: Result := CClkSetTextControlTypeStr[AAction^.SetTextOptions.ControlType];
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


//
function XClickPointReference_AsStringToValue(AXClickPointReferenceAsString: string): TXClickPointReference;
var
  i: TXClickPointReference;
begin
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
    7: AAction^.FindControlOptions.MatchBitmapFiles := NewValue; //ListOfStrings
    8: AAction^.FindControlOptions.MatchBitmapAlgorithm := MatchBitmapAlgorithm_AsStringToValue(NewValue);
    9: ;  //MatchBitmapAlgorithmSettings
    10: ;  //InitialRectangle
    11: AAction^.FindControlOptions.UseWholeScreen := StrToBool(NewValue);
    12: AAction^.FindControlOptions.ColorError := NewValue;  //string, to allow var replacements
    13: AAction^.FindControlOptions.AllowedColorErrorCount := NewValue;  //Number of pixels allowed to mismatch
    14: AAction^.FindControlOptions.WaitForControlToGoAway := StrToBool(NewValue);
    15: AAction^.FindControlOptions.StartSearchingWithCachedControl := StrToBool(NewValue);
    16: AAction^.FindControlOptions.CachedControlLeft := NewValue;
    17: AAction^.FindControlOptions.CachedControlTop := NewValue;
    18: AAction^.FindControlOptions.MatchPrimitiveFiles := NewValue; //ListOfStrings
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
      2: AAction^.FindControlOptions.MatchCriteria.WillMatchBitmapText := StrToBool(NewValue);
      3: AAction^.FindControlOptions.MatchCriteria.WillMatchBitmapFiles := StrToBool(NewValue);
      4: AAction^.FindControlOptions.MatchCriteria.WillMatchPrimitiveFiles := StrToBool(NewValue);
      5: AAction^.FindControlOptions.MatchCriteria.SearchForControlMode := SearchForControlMode_AsStringToValue(NewValue);
      else
        ;
    end;
  end;

  procedure SetActionValueStr_FindControl_MatchBitmapText(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  var
    PropertyIndexMod, PropertyIndexDiv: Integer;
  begin
    PropertyIndexMod := APropertyIndex mod CPropCount_FindControlMatchBitmapText;
    PropertyIndexDiv := APropertyIndex div CPropCount_FindControlMatchBitmapText;

    case PropertyIndexMod of
      0: AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].ForegroundColor := NewValue;
      1: AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].BackgroundColor := NewValue;
      2: AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].FontName := NewValue;
      3: AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].FontSize := StrToIntDef(NewValue, 8);
      4: AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].Bold := StrToBool(NewValue);
      5: AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].Italic := StrToBool(NewValue);
      6: AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].Underline := StrToBool(NewValue);
      7: AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].StrikeOut := StrToBool(NewValue);
      8: AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].FontQuality := FontQuality_AsStringToValue(NewValue);
      9: AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].FontQualityUsesReplacement := StrToBool(NewValue);
      10: AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].FontQualityReplacement := NewValue;
      11: AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].ProfileName := NewValue;
      12: AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].CropLeft := NewValue;
      13: AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].CropTop := NewValue;
      14: AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].CropRight := NewValue;
      15: AAction^.FindControlOptions.MatchBitmapText[PropertyIndexDiv].CropBottom := NewValue;
      else
        ;
    end;
  end;


  procedure SetActionValueStr_FindControl_MatchBitmapAlgorithmSettings(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  begin
    case APropertyIndex of
      0: AAction^.FindControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf := StrToIntDef(NewValue, 1);
      1: AAction^.FindControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf := StrToIntDef(NewValue, 1);
      2: AAction^.FindControlOptions.MatchBitmapAlgorithmSettings.XOffset := StrToIntDef(NewValue, 0);
      3: AAction^.FindControlOptions.MatchBitmapAlgorithmSettings.YOffset := StrToIntDef(NewValue, 0);
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
{$ENDIF}


procedure SetActionValueStr_SetText(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
begin
  case APropertyIndex of
    0: AAction^.SetTextOptions.Text := NewValue;
    1: AAction^.SetTextOptions.ControlType := ClkSetTextControlType_AsStringToValue(NewValue);
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
  Result := 'When checked, console applications are not displayed in a new window.' + #13#10 +
            'UI applications can create and display system consoles. For those applications, this option may cause problems if checked.';
end;


function GetPropertyHint_FindControl_AllowToFail: string;
begin
  Result := 'When checked, the execution flow does not stop if the searched (sub)control is not found.' + #13#10 +
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


{$IFDEF SubProperties}
  function GetPropertyHint_FindControl_MatchCriteria_MatchBitmapText: string;
  begin
    Result := 'The actual text being matched against.' + #13#10 +
              'It is used, both for FindControl and FindSubControl, when WillMatchText or WillMatchBitmapText properties are True.';
  end;


  function GetPropertyHint_FindControl_MatchCriteria_WillMatchBitmapText: string;
  begin
    Result := 'When selecting FindSubControl action, only bitmaps can be matched (BMP Text or BMP Files).' + #13#10 +
              'A SubControl does not have a handle of its own, it is a part of a control.' + #13#10 +
              'The $Control_Left$, $Control_Top$, $Control_Width$, $Control_Height$, $Control_Right$, $Control_Bottom$ variables ar set with the subcontrol offset.';
  end;


  function GetPropertyHint_FindControl_MatchCriteria_SearchForControlMode: string;
  begin
    Result := 'With "Generate Grid", the application generates a grid of points, where it queries for a window/control.' + #13#10 +
              'With "Enumerate Windows", it lists all top-level windows and matches their caption and/or class.' + #13#10 +
              'With "Find Window", both class and caption have to match and no wildcard is available.'
  end;
{$ENDIF}


{$IFDEF SubProperties}
  function GetPropertyHint_FindControl_InitialRectangle(AEdge: string): string;
  begin
    Result := AEdge + ' edge of the search area. Variable replacements are available.';
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
              'Also it has to be successfully executed. Otherwise, the reference values will be wrong.';
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
begin
  Result := 'The proper control type has to be selected, for the proper API call. Uses $Control_Handle$ variable.' + #13#10 +
            'HTTP calls are available, as var values, using the following format: $http://<server:port>/[params]$';
end;


function GetPropertyHint_SetText_ControlType: string;
begin
  Result := 'Uses WM_SETTEXT or CB_SELECTSTRING messages or emulates keystrokes.';
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


function GetPropertyHint_Sleep_Value: string;
begin
  Result := 'Sleep values, lower than 1, are ignored.' + #13#10 +
            'Use this action only as a last resort (e.g. blinking or resizing controls/windows).' + #13#10 +
            'Waiting for the proper event or control property, is the right way to solve a race condition. Use the "sleep" action if the event/property is not available.';
end;


end.

