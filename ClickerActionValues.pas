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
  end;

const
  CCategoryCount = 2;

  CCategory_Common = 0;
  CCategory_ActionSpecific = 1;

  CCategories: array[0..CCategoryCount - 1] of string = ('Common', 'Action specific');
  CPropCount_Common = 4;  //Action name, Action type, Action Timeout, StopOnError

  //Properties (counts)
  CPropCount_Click = 20;
  CPropCount_ExecApp = 7;
  CPropCount_FindControl = 18;
  CPropCount_FindSubControl = 18;
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
  CPropCount_FindControlMatchCriteria = 5;
  CPropCount_FindControlMatchBitmapText = 16;
  CPropCount_FindControlMatchBitmapAlgorithmSettings = 4;
  CPropCount_FindControlInitialRectange = 8;

  CPropCount_CallTemplateLoop = 7;

  //PropIndex consts
  CMain_ActionCondition_PropIndex = 3; //property index in Action structure

  CExecApp_PathToApp_PropIndex = 0;     //property index in ExecApp structure
  CExecApp_ListOfParams_PropIndex = 1;  //property index in ExecApp structure

  CFindControl_MatchCriteria_PropIndex = 0; //property index in FindControl structure
  CFindControl_MatchBitmapText_PropIndex = 6; //property index in FindControl structure
  CFindControl_MatchBitmapFiles_PropIndex = 7; //property index in FindControl structure   - list of files
  CFindControl_MatchBitmapAlgorithmSettings_PropIndex = 9;
  CFindControl_InitialRectange = 10;

  CCallTemplate_TemplateFileName_PropIndex = 0; //property index in CallTemplate structure
  CCallTemplate_ListOfCustomVarsAndValues_PropIndex = 1;
  CCallTemplate_CallTemplateLoop_PropIndex = 3; //property index in CallTemplate structure

  CCallTemplate_CallTemplateLoopProperties_BreakCondition_PropItemIndex = 5;

  CFindControl_MatchBitmapText_FontName_PropItemIndex = 2;   //property index in FindControl.MatchBitmapText structure

  CFindControl_InitialRectange_Left_PropItemIndex = 0;
  CFindControl_InitialRectange_Top_PropItemIndex = 1;
  CFindControl_InitialRectange_Right_PropItemIndex = 2;
  CFindControl_InitialRectange_Bottom_PropItemIndex = 3;
  CFindControl_InitialRectange_LeftOffset_PropItemIndex = 4;
  CFindControl_InitialRectange_TopOffset_PropItemIndex = 5;
  CFindControl_InitialRectange_RightOffset_PropItemIndex = 6;
  CFindControl_InitialRectange_BottomOffset_PropItemIndex = 7;

  CSetVar_ListOfVarNamesValuesAndEvalBefore = 0;

  //Properties
  CCommonProperties: array[0..CPropCount_Common - 1] of TOIPropDef = (
    (Name: 'ActionName'; EditorType: etText),
    (Name: 'Action'; EditorType: etEnumCombo),    //readonly, to avoid changing the OI tree structure
    (Name: 'ActionTimeout'; EditorType: etText),
    (Name: 'ActionCondition'; EditorType: etUserEditor)
  );

  //Action specific properties
  CClickProperties: array[0..CPropCount_Click - 1] of TOIPropDef = (
    (Name: 'XClickPointReference'; EditorType: etEnumCombo),
    (Name: 'YClickPointReference'; EditorType: etEnumCombo),
    (Name: 'XClickPointVar'; EditorType: etText),
    (Name: 'YClickPointVar'; EditorType: etText),
    (Name: 'XOffset'; EditorType: etSpinText),
    (Name: 'YOffset'; EditorType: etSpinText),
    (Name: 'MouseButton'; EditorType: etEnumCombo),
    (Name: 'ClickWithCtrl'; EditorType: etBooleanCombo),
    (Name: 'ClickWithAlt'; EditorType: etBooleanCombo),
    (Name: 'ClickWithShift'; EditorType: etBooleanCombo),
    (Name: 'Count'; EditorType: etSpinText),
    (Name: 'LeaveMouse'; EditorType: etBooleanCombo),
    (Name: 'MoveWithoutClick'; EditorType: etBooleanCombo),
    (Name: 'ClickType'; EditorType: etEnumCombo),
    (Name: 'XClickPointReferenceDest'; EditorType: etEnumCombo),
    (Name: 'YClickPointReferenceDest'; EditorType: etEnumCombo),
    (Name: 'XClickPointVarDest'; EditorType: etText),
    (Name: 'YClickPointVarDest'; EditorType: etText),
    (Name: 'XOffsetDest'; EditorType: etSpinText),
    (Name: 'YOffsetDest'; EditorType: etSpinText)
  );

  CExecAppProperties: array[0..CPropCount_ExecApp - 1] of TOIPropDef = (
    (Name: 'PathToApp'; EditorType: etFilePath),
    (Name: 'ListOfParams'; EditorType: etUserEditor),          //string items
    (Name: 'WaitForApp'; EditorType: etBooleanCombo),
    (Name: 'AppStdIn'; EditorType: etText),
    (Name: 'CurrentDir'; EditorType: etDirPath),
    (Name: 'UseInheritHandles'; EditorType: etEnumCombo),
    (Name: 'NoConsole'; EditorType: etBooleanCombo)
  );

  CFindControlProperties: array[0..CPropCount_FindControl - 1] of TOIPropDef = (
    (Name: 'MatchCriteria'; EditorType: etNone),               //structure
    (Name: 'AllowToFail'; EditorType: etBooleanCombo),
    (Name: 'MatchText'; EditorType: etText),
    (Name: 'MatchClassName'; EditorType: etText),
    (Name: 'MatchTextSeparator'; EditorType: etText),
    (Name: 'MatchClassNameSeparator'; EditorType: etText),
    (Name: 'MatchBitmapText'; EditorType: etFilePathWithArrow),  //array of other structure.  Count should be 0 for FindControl and >0 for FindSubControl
    (Name: 'MatchBitmapFiles'; EditorType: etFilePathWithArrow),
    (Name: 'MatchBitmapAlgorithm'; EditorType: etEnumCombo),
    (Name: 'MatchBitmapAlgorithmSettings'; EditorType: etNone),    //structure
    (Name: 'InitialRectange'; EditorType: etNone),                 //structure
    (Name: 'UseWholeScreen'; EditorType: etBooleanCombo),
    (Name: 'ColorError'; EditorType: etSpinText),
    (Name: 'AllowedColorErrorCount'; EditorType: etSpinText),
    (Name: 'WaitForControlToGoAway'; EditorType: etBooleanCombo),
    (Name: 'StartSearchingWithCachedControl'; EditorType: etBooleanCombo),
    (Name: 'CachedControlLeft'; EditorType: etText),
    (Name: 'CachedControlTop'; EditorType: etText)
  );

  {$IFDEF SubProperties}
    CFindControl_MatchCriteriaProperties: array[0..CPropCount_FindControlMatchCriteria - 1] of TOIPropDef = (
      (Name: 'WillMatchText'; EditorType: etBooleanCombo),
      (Name: 'WillMatchClassName'; EditorType: etBooleanCombo),
      (Name: 'WillMatchBitmapText'; EditorType: etBooleanCombo),
      (Name: 'WillMatchBitmapFiles'; EditorType: etBooleanCombo),
      (Name: 'SearchForControlMode'; EditorType: etEnumCombo)
    );

    CFindControl_MatchBitmapTextProperties: array[0..CPropCount_FindControlMatchBitmapText - 1] of TOIPropDef = (
      (Name: 'ForegroundColor'; EditorType: etColorCombo),
      (Name: 'BackgroundColor'; EditorType: etColorCombo),
      (Name: 'FontName'; EditorType: etEnumComboWithBtn),
      (Name: 'FontSize'; EditorType: etSpinText),
      (Name: 'Bold'; EditorType: etBooleanCombo),
      (Name: 'Italic'; EditorType: etBooleanCombo),
      (Name: 'Underline'; EditorType: etBooleanCombo),
      (Name: 'StrikeOut'; EditorType: etBooleanCombo),
      (Name: 'FontQuality'; EditorType: etEnumCombo),
      (Name: 'FontQualityUsesReplacement'; EditorType: etBooleanCombo),
      (Name: 'FontQualityReplacement'; EditorType: etText),
      (Name: 'ProfileName'; EditorType: etText),
      (Name: 'CropLeft'; EditorType: etSpinText),
      (Name: 'CropTop'; EditorType: etSpinText),
      (Name: 'CropRight'; EditorType: etSpinText),
      (Name: 'CropBottom'; EditorType: etSpinText)
    );

    CFindControl_MatchBitmapAlgorithmSettingsProperties: array[0..CPropCount_FindControlMatchBitmapAlgorithmSettings - 1] of TOIPropDef = (
      (Name: 'XMultipleOf'; EditorType: etSpinText),
      (Name: 'YMultipleOf'; EditorType: etSpinText),
      (Name: 'XOffset'; EditorType: etSpinText),
      (Name: 'YOffset'; EditorType: etSpinText)
    );

    CFindControl_InitialRectangeProperties: array[0..CPropCount_FindControlInitialRectange - 1] of TOIPropDef = (
      (Name: 'Left'; EditorType: etText),
      (Name: 'Top'; EditorType: etText),
      (Name: 'Right'; EditorType: etText),
      (Name: 'Bottom'; EditorType: etText),
      (Name: 'LeftOffset'; EditorType: etSpinText),
      (Name: 'TopOffset'; EditorType: etSpinText),
      (Name: 'RightOffset'; EditorType: etSpinText),
      (Name: 'BottomOffset'; EditorType: etSpinText)
    );
  {$ENDIF}

  CSetTextProperties: array[0..CPropCount_SetText - 1] of TOIPropDef = (
    (Name: 'Text'; EditorType: etText),
    (Name: 'ControlType'; EditorType: etEnumCombo)
  );

  CCallTemplateProperties: array[0..CPropCount_CallTemplate - 1] of TOIPropDef = (
    (Name: 'TemplateFileName'; EditorType: etFilePathWithArrow),
    (Name: 'ListOfCustomVarsAndValues'; EditorType: etUserEditor),
    (Name: 'EvaluateBeforeCalling'; EditorType: etBooleanCombo),
    (Name: 'CallTemplateLoop'; EditorType: etNone)       //structure
  );

  {$IFDEF SubProperties}
    CCallTemplate_CallTemplateLoopProperties: array[0..CPropCount_CallTemplateLoop - 1] of TOIPropDef = (
      (Name: 'Enabled'; EditorType: etBooleanCombo),
      (Name: 'Counter'; EditorType: etText),
      (Name: 'InitValue'; EditorType: etText),
      (Name: 'EndValue'; EditorType: etText),
      (Name: 'Direction'; EditorType: etEnumCombo),
      (Name: 'BreakCondition'; EditorType: etUserEditor),
      (Name: 'EvalBreakPosition'; EditorType: etEnumCombo)
    );
  {$ENDIF}

  CSleepProperties: array[0..CPropCount_Sleep - 1] of TOIPropDef = (
    (Name: 'Value'; EditorType: etText)
  );

  CSetVarProperties: array[0..CPropCount_SetVar - 1] of TOIPropDef = (
    (Name: 'ListOfVarNamesValuesAndEvalBefore'; EditorType: etUserEditor) //structure   (no sub properties)
  );

  CWindowOperationsProperties: array[0..CPropCount_WindowOperations - 1] of TOIPropDef = (
    (Name: 'Operation'; EditorType: etEnumCombo),
    (Name: 'NewX'; EditorType: etSpinText),
    (Name: 'NewY'; EditorType: etSpinText),
    (Name: 'NewWidth'; EditorType: etSpinText),
    (Name: 'NewHeight'; EditorType: etSpinText),
    (Name: 'NewPositionEabled'; EditorType: etBooleanCombo),
    (Name: 'NewSizeEabled'; EditorType: etBooleanCombo)
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
  function GetActionValueStr_FindControl_InitialRectange(AAction: PClkActionRec; APropertyIndex: Integer): string;

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
  procedure SetActionValueStr_FindControl_InitialRectange(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);

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
    GetActionValueStr_FindControl_InitialRectange,
    nil, //UseWholeScreen
    nil, //ColorError
    nil, //AllowedColorErrorCount
    nil, //WaitForControlToGoAway
    nil, //StartSearchingWithCachedControl
    nil, //CachedControlLeft
    nil  //CachedControlTop
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
    0 //YOffsetDest
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
    0, //InitialRectange: TRectString;
    0, //UseWholeScreen: Boolean;
    0, //ColorError: string;  //string, to allow var replacements
    0, //AllowedColorErrorCount: string;  //Number of pixels allowed to mismatch
    0, //WaitForControlToGoAway: Boolean;
    0, //StartSearchingWithCachedControl: Boolean;
    0, //CachedControlLeft: string;
    0  //CachedControlTop: string;
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
    0, //NewPositionEabled,
    0  //NewSizeEabled: Boolean;
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
    nil //YOffsetDest
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
    nil, //InitialRectange: TRectString;
    nil, //UseWholeScreen: Boolean;
    nil, //ColorError: string;  //string, to allow var replacements
    nil, //AllowedColorErrorCount: string;  //Number of pixels allowed to mismatch
    nil, //WaitForControlToGoAway: Boolean;
    nil, //StartSearchingWithCachedControl: Boolean;
    nil, //CachedControlLeft: string;
    nil  //CachedControlTop: string;
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
    nil, //NewPositionEabled,
    nil  //NewSizeEabled: Boolean;
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


implementation


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
    10: Result := '';  //InitialRectange
    11: Result := BoolToStr(AAction^.FindControlOptions.UseWholeScreen, True);
    12: Result := AAction^.FindControlOptions.ColorError;  //string, to allow var replacements
    13: Result := AAction^.FindControlOptions.AllowedColorErrorCount;  //Number of pixels allowed to mismatch
    14: Result := BoolToStr(AAction^.FindControlOptions.WaitForControlToGoAway, True);
    15: Result := BoolToStr(AAction^.FindControlOptions.StartSearchingWithCachedControl, True);
    16: Result := AAction^.FindControlOptions.CachedControlLeft;
    17: Result := AAction^.FindControlOptions.CachedControlTop;
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
      4: Result := CSearchForControlModeStr[AAction^.FindControlOptions.MatchCriteria.SearchForControlMode];
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


  function GetActionValueStr_FindControl_InitialRectange(AAction: PClkActionRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := AAction^.FindControlOptions.InitialRectange.Left;
      1: Result := AAction^.FindControlOptions.InitialRectange.Top;
      2: Result := AAction^.FindControlOptions.InitialRectange.Right;
      3: Result := AAction^.FindControlOptions.InitialRectange.Bottom;
      4: Result := AAction^.FindControlOptions.InitialRectange.LeftOffset;
      5: Result := AAction^.FindControlOptions.InitialRectange.TopOffset;
      6: Result := AAction^.FindControlOptions.InitialRectange.RightOffset;
      7: Result := AAction^.FindControlOptions.InitialRectange.BottomOffset;
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
    5: Result := BoolToStr(AAction^.WindowOperationsOptions.NewPositionEabled, True);
    6: Result := BoolToStr(AAction^.WindowOperationsOptions.NewSizeEabled, True);
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


function StrToBool(ABoolAsString: string): Boolean;
begin
  Result := ABoolAsString = 'True';
end;


function ClickType_AsStringToValue(AClickTypeAsString: string): Integer;
var
  i: Integer;
begin
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
    10: IntToStr(AAction^.ClickOptions.Count);
    11: AAction^.ClickOptions.LeaveMouse := StrToBool(NewValue);
    12: AAction^.ClickOptions.MoveWithoutClick := StrToBool(NewValue);
    13: AAction^.ClickOptions.ClickType := ClickType_AsStringToValue(NewValue);
    14: AAction^.ClickOptions.XClickPointReferenceDest := XClickPointReference_AsStringToValue(NewValue);
    15: AAction^.ClickOptions.YClickPointReferenceDest := YClickPointReference_AsStringToValue(NewValue);
    16: AAction^.ClickOptions.XClickPointVarDest := NewValue;
    17: AAction^.ClickOptions.YClickPointVarDest := NewValue;
    18: AAction^.ClickOptions.XOffsetDest := NewValue;
    19: AAction^.ClickOptions.YOffsetDest := NewValue;
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
    10: ;  //InitialRectange
    11: AAction^.FindControlOptions.UseWholeScreen := StrToBool(NewValue);
    12: AAction^.FindControlOptions.ColorError := NewValue;  //string, to allow var replacements
    13: AAction^.FindControlOptions.AllowedColorErrorCount := NewValue;  //Number of pixels allowed to mismatch
    14: AAction^.FindControlOptions.WaitForControlToGoAway := StrToBool(NewValue);
    15: AAction^.FindControlOptions.StartSearchingWithCachedControl := StrToBool(NewValue);
    16: AAction^.FindControlOptions.CachedControlLeft := NewValue;
    17: AAction^.FindControlOptions.CachedControlTop := NewValue;
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
      4: AAction^.FindControlOptions.MatchCriteria.SearchForControlMode := SearchForControlMode_AsStringToValue(NewValue);
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


  procedure SetActionValueStr_FindControl_InitialRectange(AAction: PClkActionRec; NewValue: string; APropertyIndex: Integer);
  begin
    case APropertyIndex of
      0: AAction^.FindControlOptions.InitialRectange.Left := NewValue;
      1: AAction^.FindControlOptions.InitialRectange.Top := NewValue;
      2: AAction^.FindControlOptions.InitialRectange.Right := NewValue;
      3: AAction^.FindControlOptions.InitialRectange.Bottom := NewValue;
      4: AAction^.FindControlOptions.InitialRectange.LeftOffset := NewValue;
      5: AAction^.FindControlOptions.InitialRectange.TopOffset := NewValue;
      6: AAction^.FindControlOptions.InitialRectange.RightOffset := NewValue;
      7: AAction^.FindControlOptions.InitialRectange.BottomOffset := NewValue;
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
    5: AAction^.WindowOperationsOptions.NewPositionEabled := StrToBool(NewValue);
    6: AAction^.WindowOperationsOptions.NewSizeEabled := StrToBool(NewValue);
    else
      ;
  end;
end;

end.

