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


unit ClickerUtils;

{$H+}
{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF Windows}
    Windows,
  {$ELSE}
    LCLIntf, LCLType,
  {$ENDIF}
  SysUtils, Classes, ClickerIniFiles, Graphics, Controls, StdCtrls;


type
  TClkAction = (acClick, acExecApp, acFindControl, acFindSubControl,
                acSetControlText, acCallTemplate, acSleep, acSetVar, acWindowOperations,
                acLoadSetVarFromFile, acSaveSetVarToFile, acPlugin);     //please update CInsertActionOffset constant in ClickerActionsArrFrame, if there will be more than 30 action types

  TClkSetTextControlType = (stEditBox, stComboBox, stKeystrokes);
  TSearchForControlMode = (sfcmGenGrid, sfcmEnumWindows, sfcmFindWindow);
  TWindowOperation = (woBringToFront, woMoveResize, woClose);
                      //SetForegroundWindow, SetWindowPos, WM_CLOSE

  TActionDebuggingStatus = (adsNone, adsPrev, adsCurrent, adsNext);
  TActionStatus = (asNotStarted, asFailed, asSuccessful, asInProgress, asAllowedFailed);

  TLimitLabel = (llLeft, llTop, llRight, llBottom);
  TLimitLabels = set of TLimitLabel;

  TOnAddToLog = procedure(s: string) of object;

  TEvaluateReplacementsFunc = function(s: string; Recursive: Boolean = True): string of object;
  TOnTriggerOnControlsModified = procedure of object;
  TOnEvaluateReplacements = function(s: string): string of object;
  TOnReverseEvaluateReplacements = function(s: string): string of object;
  TOnCopyControlTextAndClassFromMainWindow = procedure(ACompProvider: string; out AControlText, AControlClass: string) of object;
  TOnGetExtraSearchAreaDebuggingImage = function(AExtraBitmap: TBitmap): Boolean of object;
  TOnGetExtraSearchAreaDebuggingImageWithStackLevel = function(AExtraBitmap: TBitmap; AStackLevel: Integer): Boolean of object;
  TOnGetConnectionAddress = function: string of object;
  TOnGetSelectedCompFromRemoteWin = function: THandle of object;
  TOnLoadBitmap = function(ABitmap: TBitmap; AFileName: string): Boolean of object; //returns True if file loaded, and False if file not found
  TOnLoadRenderedBitmap = function(ABitmap: TBitmap; AFileName: string): Boolean of object; //returns True if file loaded, and False if file not found in in-mem FS for received rendered bitmaps
  TOnGetListOfExternallyRenderedImages = procedure(AListOfExternallyRenderedImages: TStringList) of object;
  TOnRenderBmpExternally = function(ARequest: string): string of object; //Returns client result (empty string for no error).
  TOnEditActionCondition = function(var AActionCondition: string): Boolean of object;
  TOnSetMatchTextAndClassToOI = procedure(AMatchText, AMatchClassName: string) of object;
  TOnGetUseWholeScreenAsSearchArea = function: Boolean of object;

  TOnFileExists = function(const AFileName: string): Boolean of object;
  TOnTClkIniReadonlyFileCreate = function(AFileName: string): TClkIniReadonlyFile of object;
  TOnTClkIniFileCreate = function(AFileName: string): TClkIniFile of object;
  TOnSaveTemplateToFile = procedure(AStringList: TStringList; const AFileName: string) of object;
  TOnSaveFileToExtRenderingInMemFS = procedure(AFileName: string; AContent: Pointer; AFileSize: Int64) of object;

  TOnScreenshotByActionName = function(AActionName: string): Boolean of object;

  TOnUpdatePropertyIcons = procedure(AStreamContent: Pointer; AStreamSize: Int64) of object;
  TOnUpdatePropertyDetails = procedure(AStreamContent: Pointer; AStreamSize: Int64) of object;

  TOnSetOpenDialogInitialDir = procedure(AInitialDir: string) of object;
  TOnSetOpenDialogMultiSelect = procedure of object; //the dialog is set to allow multiple files to be selected, then it is restored automatically to single file
  TOnOpenDialogExecute = function(AFilter: string): Boolean of object;
  TOnGetOpenDialogFileName = function: string of object;
  TOnSetOpenDialogFileName = procedure(AFileName: string) of object;

  TOnSetPictureSetOpenDialogMultiSelect = procedure of object;
  TOnSetPictureOpenDialogInitialDir = procedure(AInitialDir: string) of object;
  TOnPictureOpenDialogExecute = function: Boolean of object;
  TOnGetPictureOpenDialogFileName = function: string of object;

  TOnRetrieveRenderedBmpFromServer = procedure(FRemoteAddress, Fnm: string) of object;
  TOnGetListOfAvailableSetVarActions = procedure(AListOfSetVarActions: TStringList) of object;
  TOnGetListOfAvailableActions = procedure(AListOfActions: TStringList) of object;

  TOnExecuteActionByName = function(AActionName: string): Boolean of object;
  TOnSetVar = procedure(AVarName, AVarValue: string) of object;
  TOnSetDebugPoint = procedure(ADebugPoint: string) of object;
  TOnIsAtBreakPoint = function(ADebugPoint: string): Boolean of object;

  TOnGetFullTemplatesDir = function: string of object;
  TOnShowAutoComplete = procedure(AEdit: TEdit) of object;
  TOnUpdateActionScrollIndex = procedure(AActionScrollIndex: string) of object;

const
  CClkActionStr: array[TClkAction] of string = ('Click', 'ExecApp', 'FindControl', 'FindSubControl',
                                                'SetControlText', 'CallTemplate', 'Sleep', 'SetVar', 'WindowOperations',
                                                'LoadSetVarFromFile', 'SaveSetVarToFile', 'Plugin');
  CClkUnsetAction = 255; //TClkAction(255);

  //These constants are used to index an array, similar to enum values.  Please update TClickTypeStr if adding more constants to this "type".
  CClickType_Count = 5; //the number of available modes  (like enum items)

  //The following ClickType constants are also defined in MouseStuff.pas.
  CClickType_Click = 0;
  CClickType_Drag = 1;
  CClickType_ButtonDown = 2;
  CClickType_ButtonUp = 3;
  CClickType_Wheel = 4;

  CClickTypeStr: array[0..CClickType_Count - 1] of string = ('Click', 'Drag', 'ButtonDown', 'ButtonUp', 'Wheel');

  CFuncExVarName = '$FunctionException$';


  CExtBmp_Filename = 'Filename';
  CExtBmp_SrvAddrPort = 'SrvAddrPort';
  CExtBmp_Cmd = 'Cmd';
  CExtBmp_Params = 'Params';
  CExtBmp_IncludeFilenameInRequest = 'IncludeFilenameInRequest';  //can be '0' or '1'
  CExtBmp_FilenameMaxLen = 1024;


type
  TClkActionOptions = record
    ActionName: string;
    Action: TClkAction;
    ActionTimeout: Integer; //ms
    ActionEnabled: Boolean;
    ActionCondition: string;
    ExecutionIndex: string; //Used while executing an action, to identify the action in a list of actions.  When no list is used, this field should stay ''.
  end;

  TXClickPointReference = (xrefLeft, xrefRight, xrefWidth, xrefVar, xrefAbsolute);    //see CXOffsetReference below if modified
  TYClickPointReference = (yrefTop, yrefBottom, yrefHeight, yrefVar, yrefAbsolute);   //see CYOffsetReference below if modified
  TMouseWheelType = (mwtVert, mwtHoriz);

  TClkClickOptions = record
    XClickPointReference: TXClickPointReference;
    YClickPointReference: TYClickPointReference;
    XClickPointVar: string;
    YClickPointVar: string;
    XOffset, YOffset: string;
    MouseButton: TMouseButton;
    ClickWithCtrl: Boolean;
    ClickWithAlt: Boolean;
    ClickWithShift: Boolean;
    ClickWithDoubleClick: Boolean;
    Count: Integer;
    LeaveMouse: Boolean;
    MoveWithoutClick: Boolean;
    ClickType: Integer;    //see CClickType_Click and CClickType_Drag
    XClickPointReferenceDest: TXClickPointReference;
    YClickPointReferenceDest: TYClickPointReference;
    XClickPointVarDest: string;
    YClickPointVarDest: string;
    XOffsetDest, YOffsetDest: string;
    MouseWheelType: TMouseWheelType;
    MouseWheelAmount: string;
    DelayAfterMovingToDestination: string;
    DelayAfterMouseDown: string;
    MoveDuration: string;
  end;

  TExecAppUseInheritHandles = (uihNo, uihYes, uihOnlyWithStdInOut);

  TClkExecAppOptions = record
    PathToApp: string;
    ListOfParams: string;
    WaitForApp: Boolean;
    AppStdIn: string;
    CurrentDir: string;
    UseInheritHandles: TExecAppUseInheritHandles;
    NoConsole: Boolean;
  end;

  TRectString = record
    Left, Top, Right, Bottom: string;
    LeftOffset, TopOffset, RightOffset, BottomOffset: string;
  end;

  TSimpleRectString = record   //similar to TRectString, but only with a single set of values
    Left, Top, Right, Bottom: string;
  end;

  TClkFindControlMatchCriteria = record
    WillMatchText: Boolean;
    WillMatchClassName: Boolean;
    WillMatchBitmapText: Boolean;
    WillMatchBitmapFiles: Boolean;
    WillMatchPrimitiveFiles: Boolean;
    SearchForControlMode: TSearchForControlMode;
  end;

  TClkFindControlMatchBitmapText = record
    ForegroundColor: string;
    BackgroundColor: string;
    FontName: string;
    FontSize: Integer;
    Bold: Boolean;
    Italic: Boolean;
    Underline: Boolean;
    StrikeOut: Boolean;
    FontQuality: TFontQuality;
    FontQualityUsesReplacement: Boolean;
    FontQualityReplacement: string;
    ProfileName: string;
    CropLeft: string;
    CropTop: string;
    CropRight: string;
    CropBottom: string;
    IgnoreBackgroundColor: Boolean;
  end;

  TImageSource = (isScreenshot, isFile);
  TImageSourceFileNameLocation = (isflDisk, isflMem); //A reduced version of TFileLocation from InMemFileSystem. Used when TImageSource is set to isFile.

  PClkFindControlMatchBitmapText = ^TClkFindControlMatchBitmapText;

  TClkFindControlMatchBitmapTextArr = array of TClkFindControlMatchBitmapText;

  //once implemented, do not change the order of these values, because they are saved in files as integers
  TMatchBitmapAlgorithm = (mbaBruteForce, mbaXYMultipleAndOffsets, mbaRawHistogramZones);

  TMatchBitmapAlgorithmSettings = record
    XMultipleOf: Integer;
    YMultipleOf: Integer;
    XOffset: Integer;
    YOffset: Integer;
  end;

  TMatchByHistogramSettings = record
    MinPercentColorMatch: string;
    MostSignificantColorCountInSubBmp: string;
    MostSignificantColorCountInBackgroundBmp: string;
  end;

  TClkFindControlOptions = record
    MatchCriteria: TClkFindControlMatchCriteria;
    AllowToFail: Boolean;
    MatchText: string;
    MatchClassName: string;
    MatchTextSeparator: string;
    MatchClassNameSeparator: string;
    MatchBitmapText: TClkFindControlMatchBitmapTextArr;
    MatchBitmapFiles: string; //ListOfStrings
    MatchBitmapAlgorithm: TMatchBitmapAlgorithm;
    MatchBitmapAlgorithmSettings: TMatchBitmapAlgorithmSettings;
    InitialRectangle: TRectString;
    UseWholeScreen: Boolean;
    ColorError: string;  //string, to allow var replacements
    AllowedColorErrorCount: string;  //Number of pixels allowed to mismatch
    WaitForControlToGoAway: Boolean;
    StartSearchingWithCachedControl: Boolean;
    CachedControlLeft: string;
    CachedControlTop: string;
    MatchPrimitiveFiles: string;  //ListOfStrings
      MatchPrimitiveFiles_Modified: string;  //ListOfStrings  '0's and '1's.  This field is not a separate property in ObjectInspector
    GetAllControls: Boolean; //When True, the FindControl algorithm does not stop on the first found control. It keeps adding found handles to a list.
    UseFastSearch: Boolean; //When True, the FindSubControl algorithm searches a 5px x 5px area first. If that matches, then it goes for full search. Make sure that FastSearchAllowedColorErrorCount has the right value.
    FastSearchAllowedColorErrorCount: string;  //-1 = Use calculated value (scaled down value of AllowedColorErrorCount). Other values are directly converted / used (including 0). Because the 5x5 area is small, a scaled down value, will often end up being 0, which won't allow a proper match.
    IgnoredColors: string; //CSV list of colors, which can be in hex format (6 digits) or var/replacements
    SleepySearch: Boolean; //when True, there are Sleep(1) calls, to prevent keeping a CPU core at 100%. However, because the thread switching takes usually longer than 1ms, a Sleep(1) call may take even 16ms. This results in slower searches.
    StopSearchOnMismatch: Boolean; //Defaults to True. When False, the loops continue, in order to get the total pixel error count.
    ImageSource: TImageSource;  //Defaults to isScreenshot. When set to isScreenshot, FindSubControl calls ScreenShot. When set to isFile, FindSubControl uses a bmp from disk or InMem FS (see ImageSourceFileNameLocation).
    SourceFileName: string; //can be a bmp file from disk or from externally rendered InMem FS
    ImageSourceFileNameLocation: TImageSourceFileNameLocation; //can be disk or externally rendered bmp from InMem FS
    PrecisionTimeout: Boolean;  //When True, the bitmap searching algorithm verifies the timeout. This is a bit of extra overhead, which may slow down the search.
    FullBackgroundImageInResult: Boolean; //When True, the resulted debugging image contains the background image. Defaults to True for backwards compatibility.
    MatchByHistogramSettings: TMatchByHistogramSettings; //used when MatchBitmapAlgorithm is mbaRawHistogramZones
  end;

  TClkSetTextOptions = record
    Text: string;
    ControlType: TClkSetTextControlType;
    DelayBetweenKeyStrokes: string; //Available on stKeystrokes only. It is measured in ms.
    Count: string;
  end;

  TLoopDirection = (ldInc, ldDec, ldAuto);
  TLoopEvalBreakPosition = (lebpAfterContent, lebpBeforeContent);

  TClkCallTemplateLoop = record
    Enabled: Boolean; //When False, the CallTemplate action is executed once, as before. Else, it may be executed or not, based on loop settings.
    Counter: string;
    InitValue: string;
    EndValue: string;
    Direction: TLoopDirection;
    BreakCondition: string; //uses the same format as TClkActionOptions.ActionCondition
    EvalBreakPosition: TLoopEvalBreakPosition;
  end;

  TClkCallTemplateOptions = record
    TemplateFileName: string;
    ListOfCustomVarsAndValues: string;
    CallOnlyIfCondition: Boolean;         //deprecated  - these fields are here for compatibility with some old templates. Action conditions are available to all actions.
    CallOnlyIfConditionVarName: string;   //deprecated  - these fields are here for compatibility with some old templates
    CallOnlyIfConditionVarValue: string;  //deprecated  - these fields are here for compatibility with some old templates
    EvaluateBeforeCalling: Boolean;
    CallTemplateLoop: TClkCallTemplateLoop;
  end;

  TClkSleepOptions = record
    Value: string;  // [ms]
  end;

  TClkSetVarOptions = record
    ListOfVarNames: string;
    ListOfVarValues: string;
    ListOfVarEvalBefore: string;
    FailOnException: Boolean;
    //See ClickerSetVarFrame if adding new fields! All fields have to be assigned manually in getter and setter.
  end;

  TClkWindowOperationsOptions = record
    Operation: TWindowOperation;
    NewX, NewY, NewWidth, NewHeight: string;
    NewPositionEnabled, NewSizeEnabled: Boolean;
  end;

  TClkLoadSetVarFromFileOptions = record
    FileName: string;
    SetVarActionName: string;
  end;

  TClkSaveSetVarToFileOptions = record
    FileName: string;
    SetVarActionName: string;
  end;

  TClkPluginOptions = record
    FileName: string;
    ListOfPropertiesAndValues: string;    //CRLF separated items of key=value format    - displayed on ObjectInspector
    ListOfPropertiesAndTypes: string;     //CRLF separated items of key=value format    - set using <Plugin>.LoadToGetProperties after loading ListOfPropertiesAndValues from template. This field is not saved to template.
    CachedCount: Integer;                 //used to speed up ObjectInspector            - set after loading template, from ListOfPropertiesAndTypes   - not displayed on ObjectInspector
    ListOfInitValues: string;             //CRLF separated list of values
  end;

  TActionBreakPoint = record
    Exists: Boolean; //when False, the action has no breakpoint
    Enabled: Boolean;
    Condition: string;  //Empty string means no condition. Otherwise, it should be a condition which can be evaluated to 0 or 1.
  end;

  TClkActionRec = record
    ActionDebuggingStatus: TActionDebuggingStatus;
    ActionBreakPoint: TActionBreakPoint;
    ActionStatus: TActionStatus;
    ActionSkipped: Boolean; //this should be false before executing an action, and true after the action, only if the condition is false
    ActionOptions: TClkActionOptions;
    ScrollIndex: string; //used by ObjectInspector, not saved

    ClickOptions: TClkClickOptions;
    ExecAppOptions: TClkExecAppOptions;
    FindControlOptions: TClkFindControlOptions;
    SetTextOptions: TClkSetTextOptions;
    CallTemplateOptions: TClkCallTemplateOptions;
    SleepOptions: TClkSleepOptions;
    SetVarOptions: TClkSetVarOptions;
    WindowOperationsOptions: TClkWindowOperationsOptions;
    LoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions;
    SaveSetVarToFileOptions: TClkSaveSetVarToFileOptions;
    PluginOptions: TClkPluginOptions;
  end;

  PClkActionRec = ^TClkActionRec;

  TClkActionsRecArr = array of TClkActionRec;
  PClkActionsRecArr = ^TClkActionsRecArr;

  TIntArr = array of Integer;
  TColorArr = array of TColor;


  TVarRepl = record
    VarName: string;
    VarValue: string;
  end;

  TVarReplArr = array of TVarRepl;

  TCompRec = record          //Please update CompRecsAreEqual function if modifying structure
    Handle: THandle;
    ClassName: string;
    Text: string;
    ComponentRectangle: TRect;  //global on screen
    IsSubControl: Boolean;

    MouseXOffset: Integer;       //relative to the mouee cursor at the time of updating handle
    MouseYOffset: Integer;       //relative to the mouee cursor at the time of updating handle
    XOffsetFromParent: Integer;  //field updated when searching for bitmap
    YOffsetFromParent: Integer;  //field updated when searching for bitmap
    ResultedErrorCount: Integer; //field updated when searching for bitmap
  end;

  TCompRecArr = array of TCompRec;

  PClkFindControlOptions = ^TClkFindControlOptions;


  TBrightnessOperation = (boInc, boDec, boIncR, boIncG, boIncB, boDecR, boDecG, boDecB);
  TDisplayGridLineOption = (loDot, loSolid, loTransparentSolid);
  TSelectionColors = record  //used by Find(Sub)Control preview
    TopLeft_Valid: TColor;    //default: orange
    BotRight_Valid: TColor;   //default: green
    TopLeft_Invalid: TColor;  //default: dark red1
    BotRight_Invalid: TColor; //default: dark red2
  end;


  TFontFinderSettings = record
    ListOfUsedFonts: string; //CRLF separated list of font names  (#4#5 in ini file)
    MinFontSize: Integer;
    MaxFontSize: Integer;
    ShowAllFonts: Boolean;
    WinRect: TRect;
    ColWidths: TIntArr;
  end;


  TPaintedLabel = class(TLabel)
  public
    procedure Paint; override;
  end;


  TOnUpdateSearchAreaLimitsInOIFromDraggingLines = procedure(ALimitLabelsToUpdate: TLimitLabels; var AOffsets: TSimpleRectString) of object;
  TOnUpdateTextCroppingLimitsInOIFromDraggingLines = procedure(ALimitLabelsToUpdate: TLimitLabels; var AOffsets: TSimpleRectString; AFontProfileName: string) of object;
  TOnUpdateTextCroppingLimitsInOIFromDraggingLinesIdx = procedure(ALimitLabelsToUpdate: TLimitLabels; var AOffsets: TSimpleRectString; AFontProfileIndex: Integer) of object;
  TOnGetFindControlOptions = function: PClkFindControlOptions of object;
  TOnGetFindControlMatchBitmapText = function(Sender: TObject): PClkFindControlMatchBitmapText of object;
  TOnGetGridDrawingOption = function: TDisplayGridLineOption of object;
  TOnGetActionProperties = function(AActionName: string): PClkActionRec of object;
  TOnRWFontFinderSettings = procedure(var AFontFinderSettings: TFontFinderSettings) of object;
  TOnModifyPluginProperty = procedure(AAction: PClkActionRec) of object;
  TOnGetAllActions = function: PClkActionsRecArr of object;
  TOnGenerateAndSaveTreeWithWinInterp = function(AHandle: THandle; ATreeFileName: string; AStep: Integer; AUseMouseSwipe: Boolean): Boolean of object;

const
  CActionStatusStr: array[TActionStatus] of string = ('Not Started', 'Failed', 'Successful', 'In Progress', 'Allowed Failed');

  CCompNotEqual = '<>';        //all these comparison operators should be two characters long
  CCompEqual = '==';
  CCompLessThan = '<?';
  CCompGreaterThan = '>?';
  CCompLessThanOrEqual = '<=';
  CCompGreaterThanOrEqual = '>=';
  CComparisonOperators: array[0..5] of string = (CCompNotEqual, CCompEqual, CCompLessThan, CCompGreaterThan, CCompLessThanOrEqual, CCompGreaterThanOrEqual);

  //property types
  CDTString = 'String';
  CDTEnum = 'Enum';
  CDTBool = 'Boolean';
  CDTInteger = 'Integer';
  CDTStructure = 'Structure';
  CDTArray = 'Array';

  CLabel_Orange: TColor = $366FFF;
  CLabel_LightGreen: TColor = $4CC123;

  CDisplayGridLineOptionStr: array[TDisplayGridLineOption] of string = ('Dot', 'Solid', 'TransparentSolid');

  CDefaultFontProfileName = 'Profile [0]';
  CTemplateDialogFilter = 'Clicker Action Templates (*.clktmpl)|*.clktmpl|All Files (*.*)|*.*';
  CPrimitivesDialogFilter = 'Clicker Primitives (*.pmtv)|*.pmtv|All Files (*.*)|*.*';

  CXClickPointReferenceStr: array[TXClickPointReference] of string = ('xrefLeft', 'xrefRight', 'xrefWidth', 'xrefVar', 'xrefAbsolute');
  CYClickPointReferenceStr: array[TYClickPointReference] of string = ('yrefTop', 'yrefBottom', 'yrefHeight', 'yrefVar', 'yrefAbsolute');
  CMouseButtonStr: array[TMouseButton] of string = ('mbLeft', 'mbRight', 'mbMiddle', 'mbExtra1', 'mbExtra2');
  CMouseWheelTypeStr: array[TMouseWheelType] of string = ('mwtVert', 'mwtHoriz');
  CExecAppUseInheritHandlesStr: array[TExecAppUseInheritHandles] of string = ('uihNo', 'uihYes', 'uihOnlyWithStdInOut');
  CMatchBitmapAlgorithmStr: array[TMatchBitmapAlgorithm] of string = ('mbaBruteForce', 'mbaXYMultipleAndOffsets', 'mbaRawHistogramZones');
  CImageSourceStr: array[TImageSource] of string = ('isScreenshot', 'isFile');
  CImageSourceFileNameLocationStr: array[TImageSourceFileNameLocation] of string = ('isflDisk', 'isflMem');
  CClkSetTextControlTypeStr: array[TClkSetTextControlType] of string = ('stEditBox', 'stComboBox', 'stKeystrokes');
  CWindowOperationStr: array[TWindowOperation] of string = ('woBringToFront', 'woMoveResize', 'woClose');
  CSearchForControlModeStr: array[TSearchForControlMode] of string = ('sfcmGenGrid', 'sfcmEnumWindows', 'sfcmFindWindow');
  CFontQualityStr: array[TFontQuality] of string = ('fqDefault', 'fqDraft', 'fqProof', 'fqNonAntialiased', 'fqAntialiased', 'fqCleartype', 'fqCleartypeNatural');
  CLoopDirectionStr: array[TLoopDirection] of string = ('ldInc', 'ldDec', 'ldAuto');
  CLoopEvalBreakPositionStr: array[TLoopEvalBreakPosition] of string  = ('lebpAfterContent', 'lebpBeforeContent');


type
  TKeyAction = (kaDownUp, kaDown, kaUp);

  TKeyType = record
    IsSpecial: Boolean; //True for keys like VK_DELETE, VK_LEFT, VK_RIGHT, VK_F3
    KeyCode: Word;
    Action: TKeyAction;
  end;

  TKeyTypeArr = array of TKeyType;


const
  CSpecialKeyCount = 140;
  CSpecialKeyReplacements: array[0..CSpecialKeyCount - 1] of string = (                     //PgUp and PgDown are added here for convenience
    'Delete', 'Backspace', 'Cancel', 'Insert', 'Home', 'End', 'Left', 'Right', 'Up', 'Down', 'PgUp', 'PgDown', 'Prior', 'Next',
    'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'F10', 'F11', 'F12',
    'F13', 'F14', 'F15', 'F16', 'F17', 'F18', 'F19', 'F20', 'F21', 'F22', 'F23', 'F24',
    'Control', 'LControl', 'RControl', 'Shift', 'LShift', 'RShift', 'Menu', 'LMenu', 'RMenu',
    'ControlUp', 'LControlUp', 'RControlUp', 'ShiftUp', 'LShiftUp', 'RShiftUp', 'MenuUp', 'LMenuUp', 'RMenuUp',
    'ControlDown', 'LControlDown', 'RControlDown', 'ShiftDown', 'LShiftDown', 'RShiftDown', 'MenuDown', 'LMenuDown', 'RMenuDown',
    'Apps', 'LWin', 'RWin', 'LWinUp', 'RWinUp', 'LWinDown', 'RWinDown', 'Tab', 'Clear', 'Return', 'Pause', 'Capital', 'Select', 'Print', 'Execute', 'Snapshot',
    'NumLock', 'NumPad0', 'NumPad1', 'NumPad2', 'NumPad3', 'NumPad4', 'NumPad5', 'NumPad6', 'NumPad7', 'NumPad8', 'NumPad9',
    'Scroll', 'BrowserBack', 'BrowserForward', 'BrowserRefresh', 'BrowserStop', 'BrowserSearch', 'BrowserFavorites', 'BrowserHome',
    'Esc', 'EscUp', 'EscDown', 'Space',
    'Key0', 'Key1', 'Key2', 'Key3', 'Key4', 'Key5', 'Key6', 'Key7', 'Key8', 'Key9',
    'KeyA', 'KeyB', 'KeyC', 'KeyD', 'KeyE', 'KeyF', 'KeyG', 'KeyH', 'KeyI', 'KeyJ', 'KeyK', 'KeyL', 'KeyM', 'KeyN', 'KeyO', 'KeyP', 'KeyQ', 'KeyR', 'KeyS', 'KeyT', 'KeyU', 'KeyV', 'KeyW', 'KeyX', 'KeyY', 'KeyZ'
    //'Key!', 'Key"', 'Key#', 'KeyDollar', 'Key%', 'Key&', 'Key''', 'Key(', 'Key)', 'Key*', 'Key+', 'Key,', 'Key-', 'Key.', 'Key/'  //require more testing, as their ordinal values do not match as they do for other keys
    //Others to be added if required...
  );

  CSpecialKeyCodes: array[0..CSpecialKeyCount - 1] of Word = (
    VK_DELETE, VK_BACK, VK_CANCEL, VK_INSERT, VK_HOME, VK_END, VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT, VK_PRIOR, VK_NEXT,
    VK_F1, VK_F2, VK_F3, VK_F4, VK_F5, VK_F6, VK_F7, VK_F8, VK_F9, VK_F10, VK_F11, VK_F12,
    VK_F13, VK_F14, VK_F15, VK_F16, VK_F17, VK_F18, VK_F19, VK_F20, VK_F21, VK_F22, VK_F23, VK_F24,
    VK_CONTROL, VK_LCONTROL, VK_RCONTROL, VK_SHIFT, VK_LSHIFT, VK_RSHIFT, VK_MENU, VK_LMENU, VK_RMENU,
    VK_CONTROL, VK_LCONTROL, VK_RCONTROL, VK_SHIFT, VK_LSHIFT, VK_RSHIFT, VK_MENU, VK_LMENU, VK_RMENU,
    VK_CONTROL, VK_LCONTROL, VK_RCONTROL, VK_SHIFT, VK_LSHIFT, VK_RSHIFT, VK_MENU, VK_LMENU, VK_RMENU,
    VK_APPS, VK_LWIN, VK_RWIN, VK_LWIN, VK_RWIN, VK_LWIN, VK_RWIN, VK_TAB, VK_CLEAR, VK_RETURN, VK_PAUSE, VK_CAPITAL, VK_SELECT, VK_PRINT, VK_EXECUTE, VK_SNAPSHOT,
    VK_NUMLOCK, VK_NUMPAD0, VK_NUMPAD1, VK_NUMPAD2, VK_NUMPAD3, VK_NUMPAD4, VK_NUMPAD5, VK_NUMPAD6, VK_NUMPAD7, VK_NUMPAD8, VK_NUMPAD9,
    VK_SCROLL, VK_BROWSER_BACK, VK_BROWSER_FORWARD, VK_BROWSER_REFRESH, VK_BROWSER_STOP, VK_BROWSER_SEARCH, VK_BROWSER_FAVORITES, VK_BROWSER_HOME,
    VK_ESCAPE, VK_ESCAPE, VK_ESCAPE, 32,
    Ord(0), Ord(1), Ord(2), Ord(3), Ord(4), Ord(5), Ord(6), Ord(7), Ord(8), Ord(9),
    Ord('A'), Ord('B'), Ord('C'), Ord('D'), Ord('E'), Ord('F'), Ord('G'), Ord('H'), Ord('I'), Ord('J'), Ord('K'), Ord('L'), Ord('M'), Ord('N'), Ord('O'), Ord('P'), Ord('Q'), Ord('R'), Ord('S'), Ord('T'), Ord('U'), Ord('V'), Ord('W'), Ord('X'), Ord('Y'), Ord('Z')
    //Ord('!'), Ord('"'), Ord('#'), Ord('$'), Ord('%'), Ord('&'), Ord(''''), Ord('('), Ord(')'), Ord('*'), Ord('+'), Ord(','), Ord('-'), Ord('.'), Ord('/')
  );

  CSpecialKeyActions: array[0..CSpecialKeyCount - 1] of TKeyAction = (
    kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp,
    kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp,
    kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp,
    kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp,
    kaUp, kaUp, kaUp, kaUp, kaUp, kaUp, kaUp, kaUp, kaUp,
    kaDown, kaDown, kaDown, kaDown, kaDown, kaDown, kaDown, kaDown, kaDown,
    kaDownUp, kaDownUp, kaDownUp, kaUp, kaUp, kaDown, kaDown, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp,
    kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp,
    kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp,
    kaDownUp, kaUp, kaDown, kaDownUp,
    kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp,
    kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp
    //kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp, kaDownUp
  );


function FastReplace_ReturnTo45(s: string): string;
function FastReplace_45ToReturn(s: string): string;
function FastReplace_ReturnTo68(s: string): string; //used for storing CRLF (replaced by #6#8) inside a CRLF separated list of variables
function FastReplace_68ToReturn(s: string): string; //used for storing CRLF (replaced by #6#8) inside a CRLF separated list of variables
function FastReplace_ReturnTo87(s: string): string; //should be used for remote execution only
function FastReplace_87ToReturn(s: string): string; //should be used for remote execution only
function FastReplace_87To45(s: string): string;
function FastReplace_45To87(s: string): string;
function FastReplace_ReturnToCSV(s: string): string;
function FastReplace_0To1(s: string): string;

function GetIsUserAnAdmin: string;

function EvaluateAllReplacements(AListOfVars: TStringList; s: string; Recursive: Boolean = True): string;
function CreateDirWithSubDirs(ADir: string): Boolean;
function HexToInt(s: string): Cardinal;
function StringToHex(AStr: string): string;   //converts 'abc' to '616263'
function HexToString(AHex: string): string;   //converts '616263' to 'abc'

procedure RawExpressionToParts(RawExpression: string; out Op1, Op2, OpEq: string);
function MatchCriteriaToString(Criteria: TClkFindControlMatchCriteria): string;
function EvaluateActionCondition(AActionCondition: string; AEvalReplacementsFunc: TEvaluateReplacementsFunc): Boolean;


function GetControlText(hw: THandle): string;
function GetWindowClassRec(HW: THandle): TCompRec; overload;
function GetWindowClassRec(CrPos: TPoint): TCompRec; overload;

function GetCmdLineOptionValue(AOption: string): string;
function RevPos(const ASubStr, AString: string; AOffset: Integer = 1): Integer;

function ActionAsStringToTClkAction(ActionAsString: string): TClkAction;

function ModifyBrightness(AColor: TColor; AAmount: Byte; ABrightnessOperation: TBrightnessOperation): TColor;
procedure CreateSelectionLabels(AOwner: TComponent; AParent: TWinControl; var ALeftLabel, ATopLabel, ARightLabel, ABottomLabel: TLabel; ALeftColor, ATopColor, ARightColor, ABottomColor: TColor; AShouldBringToFront, ACreateWithPaintedLabel: Boolean);

procedure CopyPartialResultsToFinalResult(var AResultedControlArr, APartialResultedControlArr: TCompRecArr);

function ExtractFileNameNoExt(AFnm: string): string;
function ExtractFullFileNameNoExt(AFnm: string): string;

var
  UseWideStringsOnGetControlText: Boolean = False;


implementation


uses
  {$IFDEF Windows}
    ShellAPI,
  {$ENDIF}
  Forms, Messages, Math;


{#13#10 -> #4#5}
function FastReplace_ReturnTo45(s: string): string;
var
  i, n: Integer;
begin
  n := Pos(#13, s);
  if n = 0 then
  begin
    Result := s;
    Exit;
  end;

  for i := n to Length(s) - 1 do
    if s[i] = #13 then
      if s[i + 1] = #10 then
      begin
        s[i] := #4;
        s[i + 1] := #5;
        Continue;
      end;

  Result := s;
end;


function FastReplace_45ToReturn(s: string): string;
var
  i, n: Integer;
begin
  n := Pos(#4, s);
  if n = 0 then
  begin
    Result := s;
    Exit;
  end;

  for i := n to Length(s) - 1 do
    if s[i] = #4 then
      if s[i + 1] = #5 then
      begin
        s[i] := #13;
        s[i + 1] := #10;
        Continue;
      end;

  Result := s;
end;


{#13#10 -> #6#8}
function FastReplace_ReturnTo68(s: string): string;
var
  i, n: Integer;
begin
  n := Pos(#13, s);
  if n = 0 then
  begin
    Result := s;
    Exit;
  end;

  for i := n to Length(s) - 1 do
    if s[i] = #13 then
      if s[i + 1] = #10 then
      begin
        s[i] := #6;
        s[i + 1] := #8;
        Continue;
      end;

  Result := s;
end;


function FastReplace_68ToReturn(s: string): string;
var
  i, n: Integer;
begin
  n := Pos(#6, s);
  if n = 0 then
  begin
    Result := s;
    Exit;
  end;

  for i := n to Length(s) - 1 do
    if s[i] = #6 then
      if s[i + 1] = #8 then
      begin
        s[i] := #13;
        s[i + 1] := #10;
        Continue;
      end;

  Result := s;
end;


function FastReplace_ReturnTo87(s: string): string; //should be used for remote execution only
var
  i, n: Integer;
begin
  n := Pos(#13, s);
  if n = 0 then
  begin
    Result := s;
    Exit;
  end;

  for i := n to Length(s) - 1 do
    if s[i] = #13 then
      if s[i + 1] = #10 then
      begin
        s[i] := #8;
        s[i + 1] := #7;
        Continue;
      end;

  Result := s;
end;


function FastReplace_87ToReturn(s: string): string; //should be used for remote execution only
var
  i, n: Integer;
begin
  n := Pos(#8, s);
  if n = 0 then
  begin
    Result := s;
    Exit;
  end;

  for i := n to Length(s) - 1 do
    if s[i] = #8 then
      if s[i + 1] = #7 then
      begin
        s[i] := #13;
        s[i + 1] := #10;
        Continue;
      end;

  Result := s;
end;


function FastReplace_87To45(s: string): string;
var
  i, n: Integer;
begin
  n := Pos(#8, s);
  if n = 0 then
  begin
    Result := s;
    Exit;
  end;

  for i := n to Length(s) - 1 do
    if s[i] = #8 then
      if s[i + 1] = #7 then
      begin
        s[i] := #4;
        s[i + 1] := #5;
        Continue;
      end;

  Result := s;
end;


function FastReplace_45To87(s: string): string;
var
  i, n: Integer;
begin
  n := Pos(#4, s);
  if n = 0 then
  begin
    Result := s;
    Exit;
  end;

  for i := n to Length(s) - 1 do
    if s[i] = #4 then
      if s[i + 1] = #5 then
      begin
        s[i] := #8;
        s[i + 1] := #7;
        Continue;
      end;

  Result := s;
end;


function FastReplace_ReturnToCSV(s: string): string;
var
  i, n: Integer;
begin
  n := Pos(#13, s);
  if n = 0 then
  begin
    Result := s;
    Exit;
  end;

  for i := n to Length(s) do
    if s[i] = #13 then
      s[i] := ','
    else
      if s[i] = #10 then
        s[i] := ' ';

  Result := s;
end;


function FastReplace_0To1(s: string): string;
var
  i: Integer;
begin
  Result := s;
  for i := 1 to Length(Result) do
    if Result[i] = #0 then
      Result[i] := #1;
end;


function GetIsUserAnAdmin: string;
type
  TIsUserAnAdmin = function: Boolean;
const
  CResult: array[Boolean] of string = ('', '  [Is admin]');
var
  DllHandle: THandle;
  IsUserAnAdmin: TIsUserAnAdmin;
begin
  Result := '  [admin unknown]';

  {$IFDEF Windows}
    DllHandle := LoadLibrary(shell32);
    if DllHandle <> 0 then
    begin
      {$IFDEF FPC}
        IsUserAnAdmin := TIsUserAnAdmin(GetProcAddress(DllHandle, 'IsUserAnAdmin'));
      {$ELSE}
        @IsUserAnAdmin := GetProcAddress(DllHandle, 'IsUserAnAdmin');
      {$ENDIF}
      Result := CResult[IsUserAnAdmin()];
  
      FreeLibrary(DllHandle);
    end;
  {$ENDIF}
end;


function ReplaceOnce(AListOfVars: TStringList; s: string; AReplaceRandom: Boolean = True): string;   forward;


function RevPos(const ASubStr, AString: string; AOffset: Integer = 1): Integer;
var
  i: Integer;
begin
  Result := 0;
  if ASubStr = '' then
    Exit;

  for i := Length(AString) - Length(ASubStr) + 1 downto AOffset do
    if AString[i] = ASubStr[1] then
      if Copy(AString, i, Length(ASubStr)) = ASubStr then
      begin
        Result := i;
        Exit;
      end;
end;


const
  CRandom_FuncName = '$Random(';
  CSum_FuncName = '$Sum(';
  CDiff_FuncName = '$Diff(';
  CMul_FuncName = '$Mul(';
  CDiv_FuncName = '$Div(';
  CFMul_FuncName = '$FMul(';
  CFDiv_FuncName = '$FDiv(';
  CEFMul_FuncName = '$EFMul(';
  CEFDiv_FuncName = '$EFDiv(';
  CAbs_FuncName = '$Abs(';
  CFAbs_FuncName = '$FAbs(';
  CEFAbs_FuncName = '$EFAbs(';
  CPrefixWithZeros_FuncName = '$PrefixWithZeros(';
  CUpdateControlInfo_FuncName = '$UpdateControlInfo(';
  CExtractFileDir_FuncName = '$ExtractFileDir(';
  CExtractFileName_FuncName = '$ExtractFileName(';
  CExtractFileExt_FuncName = '$ExtractFileExt(';
  CExtractFileNameNoExt_FuncName = '$ExtractFileNameNoExt(';
  CChr_FuncName = '$Chr(';
  CFastReplace_45ToReturn_FuncName = '$FastReplace_45ToReturn(';
  CFastReplace_ReturnTo45_FuncName = '$FastReplace_ReturnTo45(';
  CFastReplace_45To87_FuncName = '$FastReplace_45To87(';
  CFastReplace_87To45_FuncName = '$FastReplace_87To45(';
  CExit_FuncName = '$Exit(';
  CStringContains_FuncName = '$StringContains(';
  CCreateDir_FuncName = '$CreateDir(';
  CLoadTextFile_FuncName = '$LoadTextFile(';
  CItemCount_FuncName = '$ItemCount(';
  CGetTextItem_FuncName = '$GetTextItem(';
  CIncBrightness_FuncName = '$IncBrightness(';
  CDecBrightness_FuncName = '$DecBrightness(';
  CIncBrightnessR_FuncName = '$IncBrightnessR(';
  CIncBrightnessG_FuncName = '$IncBrightnessG(';
  CIncBrightnessB_FuncName = '$IncBrightnessB(';
  CDecBrightnessR_FuncName = '$DecBrightnessR(';
  CDecBrightnessG_FuncName = '$DecBrightnessG(';
  CDecBrightnessB_FuncName = '$DecBrightnessB(';
  CGetSelfHandles_FuncName = '$GetSelfHandles(';
  CGetKeyNameFromPair_FuncName = '$GetKeyNameFromPair(';
  CGetKeyValueFromPair_FuncName = '$GetKeyValueFromPair(';
  CGetWindowLongPtr_FuncName = '$GetWindowLongPtr(';
  CGetWindowProcessId_FuncName = '$GetWindowProcessId(';

  CBuiltInFunctionCount = 42;
  CBuiltInFunctions: array[0..CBuiltInFunctionCount - 1] of string = (
    CRandom_FuncName,
    CSum_FuncName,
    CDiff_FuncName,
    CMul_FuncName,
    CDiv_FuncName,
    CFMul_FuncName,
    CFDiv_FuncName,
    CEFMul_FuncName,
    CEFDiv_FuncName,
    CAbs_FuncName,
    CFAbs_FuncName,
    CEFAbs_FuncName,
    CPrefixWithZeros_FuncName,
    CUpdateControlInfo_FuncName,
    CExtractFileDir_FuncName,
    CExtractFileName_FuncName,
    CExtractFileExt_FuncName,
    CExtractFileNameNoExt_FuncName,
    CChr_FuncName,
    CFastReplace_45ToReturn_FuncName,
    CFastReplace_ReturnTo45_FuncName,
    CFastReplace_45To87_FuncName,
    CFastReplace_87To45_FuncName,
    CExit_FuncName,
    CStringContains_FuncName,
    CCreateDir_FuncName,
    CLoadTextFile_FuncName,
    CItemCount_FuncName,
    CGetTextItem_FuncName,
    CIncBrightness_FuncName,
    CDecBrightness_FuncName,
    CIncBrightnessR_FuncName,
    CIncBrightnessG_FuncName,
    CIncBrightnessB_FuncName,
    CDecBrightnessR_FuncName,
    CDecBrightnessG_FuncName,
    CDecBrightnessB_FuncName,
    CGetSelfHandles_FuncName,
    CGetKeyNameFromPair_FuncName,
    CGetKeyValueFromPair_FuncName,
    CGetWindowLongPtr_FuncName,
    CGetWindowProcessId_FuncName
  );


function AnyBuiltInFunctionExistsAtTheEndOfString(AString: string): Integer;
var
  i: Integer;
  ParamLen, CurrentLen: Integer;
begin
  Result := -1;
  ParamLen := Length(AString);

  for i := 0 to CBuiltInFunctionCount - 1 do
  begin
    CurrentLen := Length(CBuiltInFunctions[i]);

    if CurrentLen < ParamLen then
      if Copy(AString, ParamLen - CurrentLen + 1, CurrentLen) = CBuiltInFunctions[i] then
      begin
        Result := i;
        Break;
      end;
  end;
end;


function AnyBuiltInFunctionExists(AString: string): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to CBuiltInFunctionCount - 1 do
    if Pos(CBuiltInFunctions[i], AString) > 0 then
    begin
      Result := i;
      Break;
    end;
end;


function ExtractFuncArgs(AFuncNameStart, AFuncAndArgs: string): string;
var
  Args: string;
  Count, i: Integer;
  FuncIdx: Integer;
begin
  Result := '';
  Args := Copy(AFuncAndArgs, Pos(AFuncNameStart, AFuncAndArgs) + Length(AFuncNameStart), MaxInt);

  Count := 1; //number of '(' / ')' pairs found
  for i := 1 to Length(Args) do
  begin
    if (Args[i] = '(') and (i > 3) and (AnyBuiltInFunctionExistsAtTheEndOfString(Copy(AFuncAndArgs, 1, i)) > -1) then
      Inc(Count)
    else
      if (Args[i] = ')') and (i < Length(Args)) and (Args[i + 1] = '$') then
      begin
        Dec(Count);
        if Count <= 0 then //can be negative, because it finds ')' without '('
        begin
          Result := Copy(Args, 1, i - 1);

          FuncIdx := AnyBuiltInFunctionExists(Result);      //The inner most function has to be solved first (through recursion).
          if FuncIdx > -1 then
            Result := ExtractFuncArgs(CBuiltInFunctions[FuncIdx], Result + ')$');   //recursion here

          Break;
        end;
      end;
  end;
end;


function ReplaceRandom(AListOfVars: TStringList; s: string): string;
var
  PosComma: Integer;
  RandomArgs, InitialRandomArgs: string;
  RandMin, RandMax: Integer;
  RandMinStr, RandMaxStr: string;
  RandomValueStr: string;
begin
  RandomArgs := ExtractFuncArgs(CRandom_FuncName, s);
  InitialRandomArgs := RandomArgs;

  if RandomArgs = '' then
    RandomValueStr := IntToStr(Random(65536))
  else
  begin
    RandomArgs := ReplaceOnce(AListOfVars, RandomArgs, False);
    RandomArgs := StringReplace(RandomArgs, ' ', '', [rfReplaceAll]);
    PosComma := Pos(',', RandomArgs);

    if PosComma > 0 then
    begin
      RandMinStr := Copy(RandomArgs, 1, PosComma - 1);
      RandMaxStr := Copy(RandomArgs, PosComma + 1, MaxInt);

      RandMin := StrToIntDef(RandMinStr, 0);
      RandMax := StrToIntDef(RandMaxStr, MaxInt);
    end
    else
    begin
      RandMin := 0;
      RandMax := StrToIntDef(RandomArgs, MaxInt);
    end;

    RandomValueStr := IntToStr(RandMin + Random(RandMax - RandMin));
  end;

  Result := StringReplace(s, CRandom_FuncName + InitialRandomArgs + ')$', RandomValueStr, [rfReplaceAll]);
end;


function ReplaceSum(AListOfVars: TStringList; s: string): string;
var
  PosComma: Integer;
  SumArgs, InitialSumArgs: string;
  SumOperand1, SumOperand2: Integer;
  SumOperand1Str, SumOperand2Str: string;
  ResultValueStr: string;
begin
  SumArgs := ExtractFuncArgs(CSum_FuncName, s);
  InitialSumArgs := SumArgs;

  if SumArgs = '' then
    ResultValueStr := '0'
  else
  begin
    SumArgs := ReplaceOnce(AListOfVars, SumArgs, False);
    SumArgs := StringReplace(SumArgs, ' ', '', [rfReplaceAll]);
    PosComma := Pos(',', SumArgs);

    if PosComma > 0 then
    begin
      SumOperand1Str := Copy(SumArgs, 1, PosComma - 1);
      SumOperand2Str := Copy(SumArgs, PosComma + 1, MaxInt);

      SumOperand1 := StrToIntDef(SumOperand1Str, 0);
      SumOperand2 := StrToIntDef(SumOperand2Str, 0);
    end
    else
    begin
      SumOperand1 := 0;
      SumOperand2 := StrToIntDef(SumArgs, 0);
    end;

    ResultValueStr := IntToStr(SumOperand1 + SumOperand2);
  end;

  Result := StringReplace(s, CSum_FuncName + InitialSumArgs + ')$', ResultValueStr, [rfReplaceAll]);
end;


function ReplaceDiff(AListOfVars: TStringList; s: string): string;
var
  PosComma: Integer;
  DiffArgs, InitialDiffArgs: string;
  DiffOperand1, DiffOperand2: Integer;
  DiffOperand1Str, DiffOperand2Str: string;
  ResultValueStr: string;
begin
  DiffArgs := ExtractFuncArgs(CDiff_FuncName, s);
  InitialDiffArgs := DiffArgs;

  if DiffArgs = '' then
    ResultValueStr := '0'
  else
  begin
    DiffArgs := ReplaceOnce(AListOfVars, DiffArgs, False);
    DiffArgs := StringReplace(DiffArgs, ' ', '', [rfReplaceAll]);
    PosComma := Pos(',', DiffArgs);

    if PosComma > 0 then
    begin
      DiffOperand1Str := Copy(DiffArgs, 1, PosComma - 1);
      DiffOperand2Str := Copy(DiffArgs, PosComma + 1, MaxInt);

      DiffOperand1 := StrToIntDef(DiffOperand1Str, 0);
      DiffOperand2 := StrToIntDef(DiffOperand2Str, 0);
    end
    else
    begin
      DiffOperand1 := 0;
      DiffOperand2 := StrToIntDef(DiffArgs, 0);
    end;

    ResultValueStr := IntToStr(DiffOperand1 - DiffOperand2);
  end;

  Result := StringReplace(s, CDiff_FuncName + InitialDiffArgs + ')$', ResultValueStr, [rfReplaceAll]);
end;


function ReplaceMul(AListOfVars: TStringList; s: string): string;
var
  PosComma: Integer;
  MulArgs, InitialMulArgs: string;
  MulOperand1, MulOperand2: Integer;
  MulOperand1Str, MulOperand2Str: string;
  ResultValueStr: string;
begin
  MulArgs := ExtractFuncArgs(CMul_FuncName, s);
  InitialMulArgs := MulArgs;

  if MulArgs = '' then
    ResultValueStr := '0'
  else
  begin
    MulArgs := ReplaceOnce(AListOfVars, MulArgs, False);
    MulArgs := StringReplace(MulArgs, ' ', '', [rfReplaceAll]);
    PosComma := Pos(',', MulArgs);

    if PosComma > 0 then
    begin
      MulOperand1Str := Copy(MulArgs, 1, PosComma - 1);
      MulOperand2Str := Copy(MulArgs, PosComma + 1, MaxInt);

      MulOperand1 := StrToIntDef(MulOperand1Str, 1);
      MulOperand2 := StrToIntDef(MulOperand2Str, 1);
    end
    else
    begin
      MulOperand1 := StrToIntDef(MulArgs, 1);
      MulOperand2 := MulOperand1;
    end;

    ResultValueStr := IntToStr(MulOperand1 * MulOperand2);
  end;

  Result := StringReplace(s, CMul_FuncName + InitialMulArgs + ')$', ResultValueStr, [rfReplaceAll]);
end;


function ReplaceDiv(AListOfVars: TStringList; s: string): string;
var
  PosComma: Integer;
  DivArgs, InitialDivArgs: string;
  DivOperand1, DivOperand2: Integer;
  DivOperand1Str, DivOperand2Str: string;
  ResultValueStr: string;
begin
  DivArgs := ExtractFuncArgs(CDiv_FuncName, s);
  InitialDivArgs := DivArgs;

  if DivArgs = '' then
    ResultValueStr := '0'
  else
  begin
    DivArgs := ReplaceOnce(AListOfVars, DivArgs, False);
    DivArgs := StringReplace(DivArgs, ' ', '', [rfReplaceAll]);
    PosComma := Pos(',', DivArgs);

    if PosComma > 0 then
    begin
      DivOperand1Str := Copy(DivArgs, 1, PosComma - 1);
      DivOperand2Str := Copy(DivArgs, PosComma + 1, MaxInt);

      DivOperand1 := StrToIntDef(DivOperand1Str, 1);
      DivOperand2 := StrToIntDef(DivOperand2Str, 1);

      try
        ResultValueStr := IntToStr(DivOperand1 div DivOperand2);
      except
        ResultValueStr := IntToStr(MaxInt);  //exception on division by 0
      end;
    end
    else
    begin
      DivOperand1 := 0;
      DivOperand2 := StrToIntDef(DivArgs, 1);

      try
        ResultValueStr := IntToStr(Round(Sqrt(DivOperand2)));
      except
        ResultValueStr := IntToStr(MaxInt);  //exception on division by 0
      end;
    end;
  end;

  Result := StringReplace(s, CDiv_FuncName + InitialDivArgs + ')$', ResultValueStr, [rfReplaceAll]);
end;


function ReplaceFMul(AListOfVars: TStringList; s: string): string;
var
  PosComma: Integer;
  MulArgs, InitialMulArgs: string;
  MulOperand1, MulOperand2: Double;
  MulOperand1Str, MulOperand2Str: string;
  ResultValueStr: string;
begin
  MulArgs := ExtractFuncArgs(CFMul_FuncName, s);
  InitialMulArgs := MulArgs;

  if MulArgs = '' then
    ResultValueStr := '0'
  else
  begin
    MulArgs := ReplaceOnce(AListOfVars, MulArgs, False);
    MulArgs := StringReplace(MulArgs, ' ', '', [rfReplaceAll]);
    PosComma := Pos(',', MulArgs);

    if PosComma > 0 then
    begin
      MulOperand1Str := Copy(MulArgs, 1, PosComma - 1);
      MulOperand2Str := Copy(MulArgs, PosComma + 1, MaxInt);

      MulOperand1 := StrToFloatDef(MulOperand1Str, 1);
      MulOperand2 := StrToFloatDef(MulOperand2Str, 1);
    end
    else
    begin
      MulOperand1 := StrToFloatDef(MulArgs, 1);
      MulOperand2 := MulOperand1;
    end;

    ResultValueStr := FloatToStr(MulOperand1 * MulOperand2);
  end;

  Result := StringReplace(s, CFMul_FuncName + InitialMulArgs + ')$', ResultValueStr, [rfReplaceAll]);
end;


function ReplaceFDiv(AListOfVars: TStringList; s: string): string;
var
  PosComma: Integer;
  DivArgs, InitialDivArgs: string;
  DivOperand1, DivOperand2: Double;
  DivOperand1Str, DivOperand2Str: string;
  ResultValueStr: string;
begin
  DivArgs := ExtractFuncArgs(CFDiv_FuncName, s);
  InitialDivArgs := DivArgs;

  if DivArgs = '' then
    ResultValueStr := '0'
  else
  begin
    DivArgs := ReplaceOnce(AListOfVars, DivArgs, False);
    DivArgs := StringReplace(DivArgs, ' ', '', [rfReplaceAll]);
    PosComma := Pos(',', DivArgs);

    if PosComma > 0 then
    begin
      DivOperand1Str := Copy(DivArgs, 1, PosComma - 1);
      DivOperand2Str := Copy(DivArgs, PosComma + 1, MaxInt);

      DivOperand1 := StrToFloatDef(DivOperand1Str, 1);
      DivOperand2 := StrToFloatDef(DivOperand2Str, 1);

      try
        ResultValueStr := FloatToStr(DivOperand1 / DivOperand2);
      except
        ResultValueStr := IntToStr(MaxInt);  //exception on division by 0
      end;
    end
    else
    begin
      DivOperand1 := 0;
      DivOperand2 := StrToFloatDef(DivArgs, 1);

      try
        ResultValueStr := FloatToStr(Sqrt(DivOperand2));
      except
        ResultValueStr := IntToStr(MaxInt);  //exception on division by 0
      end;
    end;
  end;

  Result := StringReplace(s, CFDiv_FuncName + InitialDivArgs + ')$', ResultValueStr, [rfReplaceAll]);
end;


function ReplaceEFMul(AListOfVars: TStringList; s: string): string;
var
  PosComma: Integer;
  MulArgs, InitialMulArgs: string;
  MulOperand1, MulOperand2: Extended;
  MulOperand1Str, MulOperand2Str: string;
  ResultValueStr: string;
begin
  MulArgs := ExtractFuncArgs(CEFMul_FuncName, s);
  InitialMulArgs := MulArgs;

  if MulArgs = '' then
    ResultValueStr := '0'
  else
  begin
    MulArgs := ReplaceOnce(AListOfVars, MulArgs, False);
    MulArgs := StringReplace(MulArgs, ' ', '', [rfReplaceAll]);
    PosComma := Pos(',', MulArgs);

    if PosComma > 0 then
    begin
      MulOperand1Str := Copy(MulArgs, 1, PosComma - 1);
      MulOperand2Str := Copy(MulArgs, PosComma + 1, MaxInt);

      MulOperand1 := StrToFloatDef(MulOperand1Str, 0);
      MulOperand2 := StrToFloatDef(MulOperand2Str, 0);
    end
    else
    begin
      MulOperand1 := StrToFloatDef(MulArgs, 1);
      MulOperand2 := MulOperand1;
    end;

    ResultValueStr := FloatToStr(MulOperand1 * MulOperand2);
  end;

  Result := StringReplace(s, CEFMul_FuncName + InitialMulArgs + ')$', ResultValueStr, [rfReplaceAll]);
end;


function ReplaceEFDiv(AListOfVars: TStringList; s: string): string;
var
  PosComma: Integer;
  DivArgs, InitialDivArgs: string;
  DivOperand1, DivOperand2: Extended;
  DivOperand1Str, DivOperand2Str: string;
  ResultValueStr: string;
begin
  DivArgs := ExtractFuncArgs(CEFDiv_FuncName, s);
  InitialDivArgs := DivArgs;

  if DivArgs = '' then
    ResultValueStr := '0'
  else
  begin
    DivArgs := ReplaceOnce(AListOfVars, DivArgs, False);
    DivArgs := StringReplace(DivArgs, ' ', '', [rfReplaceAll]);
    PosComma := Pos(',', DivArgs);

    if PosComma > 0 then
    begin
      DivOperand1Str := Copy(DivArgs, 1, PosComma - 1);
      DivOperand2Str := Copy(DivArgs, PosComma + 1, MaxInt);

      DivOperand1 := StrToFloatDef(DivOperand1Str, 1);
      DivOperand2 := StrToFloatDef(DivOperand2Str, 1);

      try
        ResultValueStr := FloatToStr(DivOperand1 / DivOperand2);
      except
        ResultValueStr := IntToStr(MaxInt);  //exception on division by 0
      end;
    end
    else
    begin
      DivOperand1 := 0;
      DivOperand2 := StrToFloatDef(DivArgs, 1);

      try
        ResultValueStr := FloatToStr(Sqrt(DivOperand2));
      except
        ResultValueStr := IntToStr(MaxInt);  //exception on invalid FP operation
      end;
    end;
  end;

  Result := StringReplace(s, CEFDiv_FuncName + InitialDivArgs + ')$', ResultValueStr, [rfReplaceAll]);
end;


function ReplaceAbs(AListOfVars: TStringList; s: string): string;
var
  AbsArgs, InitialAbsArgs: string;
  AbsOperand: Integer;
  AbsOperandStr: string;
  ResultValueStr: string;
begin
  AbsArgs := ExtractFuncArgs(CAbs_FuncName, s);
  InitialAbsArgs := AbsArgs;

  if AbsArgs = '' then
    ResultValueStr := '0'
  else
  begin
    AbsArgs := ReplaceOnce(AListOfVars, AbsArgs, False);
    AbsArgs := StringReplace(AbsArgs, ' ', '', [rfReplaceAll]);

    AbsOperandStr := AbsArgs;
    AbsOperand := StrToIntDef(AbsOperandStr, 0);

    ResultValueStr := IntToStr(Abs(AbsOperand));
  end;

  Result := StringReplace(s, CAbs_FuncName + InitialAbsArgs + ')$', ResultValueStr, [rfReplaceAll]);
end;


function ReplaceFAbs(AListOfVars: TStringList; s: string): string;
var
  AbsArgs, InitialAbsArgs: string;
  AbsOperand: Double;
  AbsOperandStr: string;
  ResultValueStr: string;
begin
  AbsArgs := ExtractFuncArgs(CFAbs_FuncName, s);
  InitialAbsArgs := AbsArgs;

  if AbsArgs = '' then
    ResultValueStr := '0'
  else
  begin
    AbsArgs := ReplaceOnce(AListOfVars, AbsArgs, False);
    AbsArgs := StringReplace(AbsArgs, ' ', '', [rfReplaceAll]);

    AbsOperandStr := AbsArgs;
    AbsOperand := StrToFloatDef(AbsOperandStr, 0);

    ResultValueStr := FloatToStr(Abs(AbsOperand));
  end;

  Result := StringReplace(s, CFAbs_FuncName + InitialAbsArgs + ')$', ResultValueStr, [rfReplaceAll]);
end;


function ReplaceEFAbs(AListOfVars: TStringList; s: string): string;
var
  AbsArgs, InitialAbsArgs: string;
  AbsOperand: Double;
  AbsOperandStr: string;
  ResultValueStr: string;
begin
  AbsArgs := ExtractFuncArgs(CEFAbs_FuncName, s);
  InitialAbsArgs := AbsArgs;

  if AbsArgs = '' then
    ResultValueStr := '0'
  else
  begin
    AbsArgs := ReplaceOnce(AListOfVars, AbsArgs, False);
    AbsArgs := StringReplace(AbsArgs, ' ', '', [rfReplaceAll]);

    AbsOperandStr := AbsArgs;
    AbsOperand := StrToFloatDef(AbsOperandStr, 0);

    ResultValueStr := FloatToStr(Abs(AbsOperand));
  end;

  Result := StringReplace(s, CEFAbs_FuncName + InitialAbsArgs + ')$', ResultValueStr, [rfReplaceAll]);
end;


function ReplacePrefixWithZeros(AListOfVars: TStringList; s: string): string;
var
  PosComma: Integer;
  PrefixWithZerosArgs, InitialPrefixWithZerosArgs: string;
  PrefixWithZerosOperand2: Integer;
  PrefixWithZerosOperand1Str, PrefixWithZerosOperand2Str: string;
  ResultValueStr: string;
  i: Integer;
begin
  PrefixWithZerosArgs := ExtractFuncArgs(CPrefixWithZeros_FuncName, s);
  InitialPrefixWithZerosArgs := PrefixWithZerosArgs;

  if PrefixWithZerosArgs = '' then
    ResultValueStr := '0'
  else
  begin
    PrefixWithZerosArgs := ReplaceOnce(AListOfVars, PrefixWithZerosArgs, False);
    PrefixWithZerosArgs := StringReplace(PrefixWithZerosArgs, ' ', '', [rfReplaceAll]);
    PosComma := Pos(',', PrefixWithZerosArgs);

    PrefixWithZerosOperand1Str := '';
    if PosComma > 0 then
    begin
      PrefixWithZerosOperand1Str := Copy(PrefixWithZerosArgs, 1, PosComma - 1);
      PrefixWithZerosOperand2Str := Copy(PrefixWithZerosArgs, PosComma + 1, MaxInt);

      PrefixWithZerosOperand2 := StrToIntDef(PrefixWithZerosOperand2Str, 0);

      ResultValueStr := '';
      for i := Length(PrefixWithZerosOperand1Str) to PrefixWithZerosOperand2 - 1 do
        ResultValueStr := ResultValueStr + '0';

      ResultValueStr := ResultValueStr + PrefixWithZerosOperand1Str;
    end
    else
      ResultValueStr := PrefixWithZerosOperand1Str;
  end;

  Result := StringReplace(s, CPrefixWithZeros_FuncName + InitialPrefixWithZerosArgs + ')$', ResultValueStr, [rfReplaceAll]);
end;


function ReplaceUpdateControlInfo(AListOfVars: TStringList; s: string): string;
var
  Args, InitialArgs: string;
  ControlHandle: THandle;
  CompRec: TCompRec;
begin
  Args := ExtractFuncArgs(CUpdateControlInfo_FuncName, s);
  InitialArgs := Args;
  Args := ReplaceOnce(AListOfVars, Args, False);
  ControlHandle := StrToIntDef(Args, 0);

  CompRec := GetWindowClassRec(ControlHandle);
  if CompRec.Handle > 0 then
  begin
    AListOfVars.Values['$Control_Text$'] := CompRec.Text;
    AListOfVars.Values['$Control_Class$'] := CompRec.ClassName;
    AListOfVars.Values['$Control_Handle$'] := IntToStr(CompRec.Handle);
    AListOfVars.Values['$Control_Left$'] := IntToStr(CompRec.ComponentRectangle.Left);
    AListOfVars.Values['$Control_Top$'] := IntToStr(CompRec.ComponentRectangle.Top);
    AListOfVars.Values['$Control_Right$'] := IntToStr(CompRec.ComponentRectangle.Right);
    AListOfVars.Values['$Control_Bottom$'] := IntToStr(CompRec.ComponentRectangle.Bottom);
    AListOfVars.Values['$Control_Width$'] := IntToStr(CompRec.ComponentRectangle.Width);
    AListOfVars.Values['$Control_Height$'] := IntToStr(CompRec.ComponentRectangle.Height);
    AListOfVars.Values['$Half_Control_Width$'] := IntToStr(CompRec.ComponentRectangle.Width shr 1);
    AListOfVars.Values['$Half_Control_Height$'] := IntToStr(CompRec.ComponentRectangle.Height shr 1);
  end
  else
  begin
    AListOfVars.Values['$Control_Text$'] := '';
    AListOfVars.Values['$Control_Class$'] := '';
    AListOfVars.Values['$Control_Handle$'] := '0';
    AListOfVars.Values['$Control_Left$'] := '0';
    AListOfVars.Values['$Control_Top$'] := '0';
    AListOfVars.Values['$Control_Right$'] := '0';
    AListOfVars.Values['$Control_Bottom$'] := '0';
    AListOfVars.Values['$Control_Width$'] := '0';
    AListOfVars.Values['$Control_Height$'] := '0';
    AListOfVars.Values['$Half_Control_Width$'] := '0';
    AListOfVars.Values['$Half_Control_Height$'] := '0';
  end;

  Result := StringReplace(s, CUpdateControlInfo_FuncName + InitialArgs + ')$', InitialArgs, [rfReplaceAll]);
end;


function ReplaceExtractFileDir(s: string): string;
var
  DirArgs, InitialDirArgs: string;
begin
  DirArgs := ExtractFuncArgs(CExtractFileDir_FuncName, s);
  InitialDirArgs := DirArgs;

  Result := StringReplace(s, CExtractFileDir_FuncName + InitialDirArgs + ')$', ExtractFileDir(DirArgs), [rfReplaceAll]);
end;


function ReplaceExtractFileName(s: string): string;
var
  DirArgs, InitialDirArgs: string;
begin
  DirArgs := ExtractFuncArgs(CExtractFileName_FuncName, s);
  InitialDirArgs := DirArgs;

  Result := StringReplace(s, CExtractFileName_FuncName + InitialDirArgs + ')$', ExtractFileName(DirArgs), [rfReplaceAll]);
end;


function ReplaceExtractFileExt(s: string): string;
var
  DirArgs, InitialDirArgs: string;
begin
  DirArgs := ExtractFuncArgs(CExtractFileExt_FuncName, s);
  InitialDirArgs := DirArgs;

  Result := StringReplace(s, CExtractFileExt_FuncName + InitialDirArgs + ')$', ExtractFileExt(DirArgs), [rfReplaceAll]);
end;


function ReplaceExtractFileNameNoExt(s: string): string;
var
  DirArgs, InitialDirArgs: string;
  FileNameWithExt, FileNameWithoutExt: string;
begin
  DirArgs := ExtractFuncArgs(CExtractFileNameNoExt_FuncName , s);
  InitialDirArgs := DirArgs;

  FileNameWithExt := ExtractFileName(DirArgs);
  FileNameWithoutExt := Copy(FileNameWithExt, 1, Length(FileNameWithExt) - Length(ExtractFileExt(FileNameWithExt)));
  Result := StringReplace(s, CExtractFileNameNoExt_FuncName  + InitialDirArgs + ')$', FileNameWithoutExt, [rfReplaceAll]);
end;


function ReplaceChr(s: string): string;
var
  Args, InitialArgs: string;
  ArgsNum: Integer;
begin
  Args := ExtractFuncArgs(CChr_FuncName, s);
  InitialArgs := Args;
  ArgsNum := StrToIntDef(Args, 65);

  Result := StringReplace(s, CChr_FuncName + InitialArgs + ')$', Chr(ArgsNum), [rfReplaceAll]);
end;


function ReplaceFastReplace_45ToReturn(s: string): string;
var
  Args, InitialArgs: string;
begin
  Args := ExtractFuncArgs(CFastReplace_45ToReturn_FuncName, s);
  InitialArgs := Args;

  Result := StringReplace(s, CFastReplace_45ToReturn_FuncName + InitialArgs + ')$', FastReplace_45ToReturn(Args), [rfReplaceAll]);
end;


function ReplaceFastReplace_ReturnTo45(s: string): string;
var
  Args, InitialArgs: string;
begin
  Args := ExtractFuncArgs(CFastReplace_ReturnTo45_FuncName, s);
  InitialArgs := Args;

  Result := StringReplace(s, CFastReplace_ReturnTo45_FuncName + InitialArgs + ')$', FastReplace_ReturnTo45(Args), [rfReplaceAll]);
end;


function ReplaceFastReplace_45To87(s: string): string;
var
  Args, InitialArgs: string;
begin
  Args := ExtractFuncArgs(CFastReplace_45To87_FuncName, s);
  InitialArgs := Args;

  Result := StringReplace(s, CFastReplace_45To87_FuncName + InitialArgs + ')$', FastReplace_45To87(Args), [rfReplaceAll]);
end;


function ReplaceFastReplace_87To45(s: string): string;
var
  Args, InitialArgs: string;
begin
  Args := ExtractFuncArgs(CFastReplace_87To45_FuncName, s);
  InitialArgs := Args;

  Result := StringReplace(s, CFastReplace_87To45_FuncName + InitialArgs + ')$', FastReplace_87To45(Args), [rfReplaceAll]);
end;


function ReplaceExit(s: string): string;
var
  Args, InitialArgs: string;
begin
  Args := ExtractFuncArgs(CExit_FuncName, s);
  InitialArgs := Args;

  Result := StringReplace(s, CExit_FuncName + InitialArgs + ')$', 'Exit(<ExitCode>) should be called from SetVar action, to stop the template.', [rfReplaceAll]);
end;


function ReplaceStringContains(AListOfVars: TStringList; s: string): string;  //as limitation, the substring should not contain commas
var
  PosComma: Integer;
  Args, InitialArgs: string;
  Operand1Str, Operand2Str: string;
  ResultValueStr: string;
begin
  Args := ExtractFuncArgs(CStringContains_FuncName, s);
  InitialArgs := Args;

  if Args = '' then
    ResultValueStr := '0'
  else
  begin
    Args := ReplaceOnce(AListOfVars, Args, False);
    Args := StringReplace(Args, ' ', '', [rfReplaceAll]);
    PosComma := Pos(',', Args);

    if PosComma > 0 then
    begin
      Operand1Str := Copy(Args, 1, PosComma - 1);
      Operand2Str := Copy(Args, PosComma + 1, MaxInt);
    end
    else
    begin
      Operand1Str := '';
      Operand2Str := Args;
    end;

    ResultValueStr := IntToStr(Ord(Pos(Operand1Str, Operand2Str) > 0));
  end;

  Result := StringReplace(s, CStringContains_FuncName + InitialArgs + ')$', ResultValueStr, [rfReplaceAll]);
end;


function ReplaceCreateDir(s: string): string;
var
  DirArgs, InitialDirArgs: string;
begin
  DirArgs := ExtractFuncArgs(CCreateDir_FuncName, s);
  InitialDirArgs := DirArgs;

  Result := StringReplace(s, CCreateDir_FuncName + InitialDirArgs + ')$', IntToStr(Ord(CreateDirWithSubDirs(DirArgs))), [rfReplaceAll]);
end;


function ReplaceLoadTextFile(AListOfVars: TStringList; s: string): string;
var
  FileArgs, InitialFileArgs: string;
  AStringList: TStringList;
begin
  FileArgs := ExtractFuncArgs(CLoadTextFile_FuncName, s);
  InitialFileArgs := FileArgs;

  FileArgs := StringReplace(FileArgs, '"', '', [rfReplaceAll]);

  Result := '';
  try
    AStringList := TStringList.Create;
    try
      AStringList.LoadFromFile(FileArgs);
      Result := FastReplace_ReturnTo45(AStringList.Text);

      if Result = #4#5 then
        Result := '';
    finally
      AStringList.Free;
    end;

    Result := StringReplace(s, CLoadTextFile_FuncName + InitialFileArgs + ')$', Result, [rfReplaceAll]);
  except
    on E: Exception do
    begin
      Result := '';
      AListOfVars.Values[CFuncExVarName] := E.Message;  //can be file not found, or some permission error
    end;
  end;
end;


function ReplaceItemCount(s: string): string;
var
  ItemArgs, InitialItemArgs: string;
  Count, i: Integer;
begin
  ItemArgs := ExtractFuncArgs(CItemCount_FuncName, s);
  InitialItemArgs := ItemArgs;

  Count := 0;
  for i := 1 to Length(ItemArgs) - 1 do  //yes, from 1 to len -1
    if (ItemArgs[i] = #4) and (ItemArgs[i + 1] = #5) then
    begin
      Inc(Count);
      Continue;
    end;

  Result := StringReplace(s, CItemCount_FuncName + InitialItemArgs + ')$', IntToStr(Count), [rfReplaceAll]);
end;


function ReplaceGetTextItem(AListOfVars: TStringList; s: string): string;
var
  ItemArgs, InitialItemArgs, Content, IndexStr: string;
  CurrentIndex, i, ItemIndex, PosComma, ItemPos, PrevItemPos: Integer;
  Found: Boolean;
begin
  ItemArgs := ExtractFuncArgs(CGetTextItem_FuncName, s);
  InitialItemArgs := ItemArgs;

  if ItemArgs = '' then
  begin
    Result := '';
    AListOfVars.Values[CFuncExVarName] := 'Missing arguments for GetTextItem.';
    Exit;
  end
  else
  begin
    PosComma := Pos(',', ItemArgs);

    if PosComma > 0 then
    begin
      Content := Copy(ItemArgs, 1, PosComma - 1);
      IndexStr := Copy(ItemArgs, PosComma + 1, MaxInt);
    end
    else
    begin
      Content := Copy(ItemArgs, 1, MaxInt);
      IndexStr := '0';
    end;
  end;

  ItemIndex := StrToIntDef(IndexStr, -1);
  if ItemIndex = -1 then
  begin
    Result := '';
    AListOfVars.Values[CFuncExVarName] := 'List index out of bounds: -1';
    Exit;
  end;

  ItemPos := 1;
  PrevItemPos := 1;
                 //this parser should be faster than converting the content to CRLF separated string and assigning it to a TStringList
  CurrentIndex := -1;
  Found := False;
  for i := 1 to Length(Content) - 1 do  //yes, from 1 to len -1
    if (Content[i] = #4) and (Content[i + 1] = #5) then
    begin
      Inc(CurrentIndex);

      if CurrentIndex >= ItemIndex then
      begin
        Found := True;
        ItemPos := i;
        Break;
      end;

      PrevItemPos := i + 2;
      Continue;
    end;

  if not Found then
  begin
    Result := '';
    AListOfVars.Values[CFuncExVarName] := 'List index out of bounds: ' + IntToStr(ItemIndex);
    Exit;
  end;

  Result := Copy(Content, PrevItemPos, ItemPos - PrevItemPos);
  Result := StringReplace(s, CGetTextItem_FuncName + InitialItemArgs + ')$', Result, [rfReplaceAll]);
end;


function ModifyBrightness(AColor: TColor; AAmount: Byte; ABrightnessOperation: TBrightnessOperation): TColor;
var
  R, G, B: Integer;
  Red, Green, Blue: Byte;
begin
  RedGreenBlue(AColor, Red, Green, Blue);

  R := Red;
  G := Green;
  B := Blue;

  case ABrightnessOperation of
    boInc:
    begin
      Inc(R, AAmount);  R := Min(255, R);
      Inc(G, AAmount);  G := Min(255, G);
      Inc(B, AAmount);  B := Min(255, B);
    end;

    boDec:
    begin
      Dec(R, AAmount);  R := Max(0, R);
      Dec(G, AAmount);  G := Max(0, G);
      Dec(B, AAmount);  B := Max(0, B);
    end;

    boIncR:
    begin
      Inc(R, AAmount);  R := Min(255, R);
      //Inc(G, AAmount);  G := Min(255, G);
      //Inc(B, AAmount);  B := Min(255, B);
    end;

    boIncG:
    begin
      //Inc(R, AAmount);  R := Min(255, R);
      Inc(G, AAmount);  G := Min(255, G);
      //Inc(B, AAmount);  B := Min(255, B);
    end;

    boIncB:
    begin
      //Inc(R, AAmount);  R := Min(255, R);
      //Inc(G, AAmount);  G := Min(255, G);
      Inc(B, AAmount);  B := Min(255, B);
    end;

    boDecR:
    begin
      Dec(R, AAmount);  R := Max(0, R);
      //Dec(G, AAmount);  G := Max(0, G);
      //Dec(B, AAmount);  B := Max(0, B);
    end;

    boDecG:
    begin
      //Dec(R, AAmount);  R := Max(0, R);
      Dec(G, AAmount);  G := Max(0, G);
      //Dec(B, AAmount);  B := Max(0, B);
    end;

    boDecB:
    begin
      //Dec(R, AAmount);  R := Max(0, R);
      //Dec(G, AAmount);  G := Max(0, G);
      Dec(B, AAmount);  B := Max(0, B);
    end;
  end; //case

  Result := RGBToColor(R, G, B);
end;


const
  CBrightnessOperationStr: array[TBrightnessOperation] of string = (
    CIncBrightness_FuncName, CDecBrightness_FuncName,
    CIncBrightnessR_FuncName, CIncBrightnessG_FuncName, CIncBrightnessB_FuncName,
    CDecBrightnessR_FuncName, CDecBrightnessG_FuncName, CDecBrightnessB_FuncName);

function ReplaceModifyBrightness(AListOfVars: TStringList; s: string; ABrightnessOperation: TBrightnessOperation): string;
var
  PosComma: Integer;
  Args, InitialArgs: string;
  Operand1Str, Operand2Str: string;
  ResultValueStr: string;
  Amount: Byte;
  ColorToBeModified, ResultValue: TColor;
  OperationStr: string;
begin
  OperationStr := CBrightnessOperationStr[ABrightnessOperation];
  Args := ExtractFuncArgs(OperationStr, s);
  InitialArgs := Args;

  if Args = '' then
    ResultValueStr := '0'
  else
  begin
    Args := ReplaceOnce(AListOfVars, Args, False);
    Args := StringReplace(Args, ' ', '', [rfReplaceAll]);
    PosComma := Pos(',', Args);

    if PosComma > 0 then
    begin
      Operand1Str := Copy(Args, 1, PosComma - 1);
      Operand2Str := Copy(Args, PosComma + 1, MaxInt);
    end
    else
    begin
      Operand1Str := Args;
      Operand2Str := '1';
    end;

    ColorToBeModified := HexToInt(Operand1Str);
    Amount := StrToIntDef(Operand2Str, 1);
    ResultValue := ModifyBrightness(ColorToBeModified, Amount, ABrightnessOperation);

    ResultValueStr := IntToHex(ResultValue, 6);
  end;

  Result := StringReplace(s, OperationStr + InitialArgs + ')$', ResultValueStr, [rfReplaceAll]);
end;


function ReplaceGetSelfHandles(s: string): string;
begin
  Result := StringReplace(s, CGetSelfHandles_FuncName + ')$', 'GetSelfHandles() should be called from SetVar action, to fill in the handles.', [rfReplaceAll]);
end;


function ReplaceGetKeyNameFromPair(s: string): string;
var
  ItemArgs, InitialItemArgs: string;
begin
  ItemArgs := ExtractFuncArgs(CGetKeyNameFromPair_FuncName, s);
  InitialItemArgs := ItemArgs;

  Result := StringReplace(s, CGetKeyNameFromPair_FuncName + InitialItemArgs + ')$', Copy(ItemArgs, 1, Pos('=', ItemArgs) - 1), [rfReplaceAll]);
end;


function ReplaceGetKeyValueFromPair(s: string): string;
var
  ItemArgs, InitialItemArgs: string;
begin
  ItemArgs := ExtractFuncArgs(CGetKeyValueFromPair_FuncName, s);
  InitialItemArgs := ItemArgs;

  Result := StringReplace(s, CGetKeyValueFromPair_FuncName + InitialItemArgs + ')$', Copy(ItemArgs, Pos('=', ItemArgs) + 1, MaxInt), [rfReplaceAll]);
end;


function ReplaceGetWindowLongPtr(AListOfVars: TStringList; s: string): string;
var
  PosComma: Integer;
  GetWindowLongPtrArgs, InitialGetWindowLongPtrArgs: string;
  GetWindowLongPtrOperand1, GetWindowLongPtrOperand2: Integer;
  GetWindowLongPtrOperand1Str, GetWindowLongPtrOperand2Str: string;
  ResultValueStr: string;
begin
  GetWindowLongPtrArgs := ExtractFuncArgs(CGetWindowLongPtr_FuncName, s);
  InitialGetWindowLongPtrArgs := GetWindowLongPtrArgs;

  if GetWindowLongPtrArgs = '' then
    ResultValueStr := '0'
  else
  begin
    GetWindowLongPtrArgs := ReplaceOnce(AListOfVars, GetWindowLongPtrArgs, False);
    GetWindowLongPtrArgs := StringReplace(GetWindowLongPtrArgs, ' ', '', [rfReplaceAll]);
    PosComma := Pos(',', GetWindowLongPtrArgs);

    if PosComma > 0 then
    begin
      GetWindowLongPtrOperand1Str := Copy(GetWindowLongPtrArgs, 1, PosComma - 1);
      GetWindowLongPtrOperand2Str := Copy(GetWindowLongPtrArgs, PosComma + 1, MaxInt);

      GetWindowLongPtrOperand1 := StrToIntDef(GetWindowLongPtrOperand1Str, 0);
      GetWindowLongPtrOperand2 := StrToIntDef(GetWindowLongPtrOperand2Str, 0);
    end
    else
    begin
      GetWindowLongPtrOperand1 := StrToIntDef(ReplaceOnce(AListOfVars, '$Control_Handle$', False), 0);
      GetWindowLongPtrOperand2 := StrToIntDef(GetWindowLongPtrArgs, 0);
    end;

    ResultValueStr := IntToStr(GetWindowLongPtr(GetWindowLongPtrOperand1, GetWindowLongPtrOperand2));
  end;

  Result := StringReplace(s, CGetWindowLongPtr_FuncName + InitialGetWindowLongPtrArgs + ')$', ResultValueStr, [rfReplaceAll]);
end;


function ReplaceGetWindowProcessId(AListOfVars: TStringList; s: string): string;
var
  ItemArgs, InitialItemArgs: string;
  ProcID, Res: DWORD;
begin
  ItemArgs := ExtractFuncArgs(CGetWindowProcessId_FuncName, s);
  InitialItemArgs := ItemArgs;

  Res := GetWindowThreadProcessId(StrToIntDef(ItemArgs, 0), @ProcID);
  if Res > 0 then
    Result := StringReplace(s, CGetWindowProcessId_FuncName + InitialItemArgs + ')$', IntToStr(ProcID), [rfReplaceAll])
  else
  begin
    AListOfVars.Values['$ExecAction_Err$'] := SysErrorMessage(GetLastOSError);
    Result := '0';
  end;
end;


function ReplaceOnce(AListOfVars: TStringList; s: string; AReplaceRandom: Boolean = True): string;
var
  i: Integer;
  ibr: TBrightnessOperation;
  tp: TPoint;
  CurrentName, CurrentValue: string;
begin
  for i := 0 to AListOfVars.Count - 1 do
  begin
    CurrentName := AListOfVars.Names[i];
    CurrentValue := AListOfVars.ValueFromIndex[i];

    if Pos(CurrentName, CurrentValue) = 0 then  //avoid expanding a string into its "superset", because it will lead to infinite recursion (e.g.: $TextToFind$=$TextToFind$=$ProjectName$ )
      s := StringReplace(s, CurrentName, CurrentValue, [rfReplaceAll]);
  end;

  if Pos(CRandom_FuncName, s) > 0 then
  begin
    if Random(7) = 3 then
    begin
      Sleep(3);
      Randomize;
    end;

    s := ReplaceRandom(AListOfVars, s);
  end;

  if Pos(CExtractFileDir_FuncName, s) > 0 then
    s := ReplaceExtractFileDir(s);

  if Pos(CExtractFileName_FuncName, s) > 0 then
    s := ReplaceExtractFileName(s);

  if Pos(CExtractFileExt_FuncName, s) > 0 then
    s := ReplaceExtractFileExt(s);

  if Pos(CExtractFileNameNoExt_FuncName, s) > 0 then
    s := ReplaceExtractFileNameNoExt(s);

  if Pos(CChr_FuncName, s) > 0 then
    s := ReplaceChr(s);

  if Pos('$Current_Mouse_X$', s) > 0 then
  begin
    GetCursorPos(tp);
    s := StringReplace(s, '$Current_Mouse_X$', IntToStr(tp.X), [rfReplaceAll]);
  end;

  if Pos('$Current_Mouse_Y$', s) > 0 then
  begin
    GetCursorPos(tp);
    s := StringReplace(s, '$Current_Mouse_Y$', IntToStr(tp.Y), [rfReplaceAll]);
  end;

  if Pos('$CRLF$', s) > 0 then
    s := StringReplace(s, '$CRLF$', #13#10, [rfReplaceAll]);

  if Pos('$#4#5$', s) > 0 then
    s := StringReplace(s, '$#4#5$', #4#5, [rfReplaceAll]);

  if Pos('$Now$', s) > 0 then
    s := StringReplace(s, '$Now$', DateTimeToStr(Now), [rfReplaceAll]);

  if Pos(CExit_FuncName, s) > 0 then
    s := ReplaceExit(s);

  if Pos(CStringContains_FuncName, s) > 0 then
    s := ReplaceStringContains(AListOfVars, s);

  if Pos(CCreateDir_FuncName, s) > 0 then
    s := ReplaceCreateDir(s);

  if Pos(CLoadTextFile_FuncName, s) > 0 then
    s := ReplaceLoadTextFile(AListOfVars, s);

  if Pos(CItemCount_FuncName, s) > 0 then
    s := ReplaceItemCount(s);

  if Pos(CGetTextItem_FuncName, s) > 0 then
    s := ReplaceGetTextItem(AListOfVars, s);

  if Pos(CFastReplace_45ToReturn_FuncName, s) > 0 then
    s := ReplaceFastReplace_45ToReturn(s);

  if Pos(CFastReplace_ReturnTo45_FuncName, s) > 0 then
    s := ReplaceFastReplace_ReturnTo45(s);

  if Pos(CFastReplace_45To87_FuncName, s) > 0 then
    s := ReplaceFastReplace_45To87(s);

  if Pos(CFastReplace_87To45_FuncName, s) > 0 then
    s := ReplaceFastReplace_87To45(s);

  if Pos(CSum_FuncName, s) > 0 then
    s := ReplaceSum(AListOfVars, s);

  if Pos(CDiff_FuncName, s) > 0 then
    s := ReplaceDiff(AListOfVars, s);

  if Pos(CMul_FuncName, s) > 0 then
    s := ReplaceMul(AListOfVars, s);

  if Pos(CDiv_FuncName, s) > 0 then
    s := ReplaceDiv(AListOfVars, s);

  if Pos(CFMul_FuncName, s) > 0 then
    s := ReplaceFMul(AListOfVars, s);

  if Pos(CFDiv_FuncName, s) > 0 then
    s := ReplaceFDiv(AListOfVars, s);

  if Pos(CEFMul_FuncName, s) > 0 then
    s := ReplaceEFMul(AListOfVars, s);

  if Pos(CEFDiv_FuncName, s) > 0 then
    s := ReplaceEFDiv(AListOfVars, s);

  if Pos(CAbs_FuncName, s) > 0 then
    s := ReplaceAbs(AListOfVars, s);

  if Pos(CFAbs_FuncName, s) > 0 then
    s := ReplaceFAbs(AListOfVars, s);

  if Pos(CEFAbs_FuncName, s) > 0 then
    s := ReplaceEFAbs(AListOfVars, s);

  if Pos(CPrefixWithZeros_FuncName, s) > 0 then
    s := ReplacePrefixWithZeros(AListOfVars, s);

  if Pos(CUpdateControlInfo_FuncName, s) > 0 then
    s := ReplaceUpdateControlInfo(AListOfVars, s);

  for ibr := Low(TBrightnessOperation) to High(TBrightnessOperation) do
    if Pos(CBrightnessOperationStr[ibr], s) > 0 then
      s := ReplaceModifyBrightness(AListOfVars, s, ibr);

  //if Pos(CGetSelfHandles_FuncName + ')$', s) > 0 then
  //  s := ReplaceGetSelfHandles(s);   //this has to be commented, to avoid being evaluated when part of a parameter

  if Pos(CGetKeyNameFromPair_FuncName, s) > 0 then
    s := ReplaceGetKeyNameFromPair(s);

  if Pos(CGetKeyValueFromPair_FuncName, s) > 0 then
    s := ReplaceGetKeyValueFromPair(s);

  if Pos(CGetWindowLongPtr_FuncName, s) > 0 then
    s := ReplaceGetWindowLongPtr(AListOfVars, s);

  if Pos(CGetWindowProcessId_FuncName, s) > 0 then
    s := ReplaceGetWindowProcessId(AListOfVars, s);

  Result := s;
end;


function EvaluateAllReplacements(AListOfVars: TStringList; s: string; Recursive: Boolean = True): string;
var
  temp_s: string;
  i: Integer;
begin
  if s = '' then
  begin
    Result := '';
    Exit;
  end;

  temp_s := ReplaceOnce(AListOfVars, s);

  if Recursive and (Pos('$', temp_s) > 0) then
  begin
    i := 0;
    repeat
      Result := temp_s;
      temp_s := ReplaceOnce(AListOfVars, Result);
      if temp_s = Result then //no replacements found
        Break;

      Inc(i);
    until False or (i > 1000);
  end
  else
    Result := temp_s;
end;


function CreateDirWithSubDirs(ADir: string): Boolean;   //requires absolute paths
var
  ADirTemp, PrevTemp: string;
begin
  Result := True; ///success
  if DirectoryExists(ADir) then
    Exit;

  ADirTemp := ADir;
  repeat
    if ADirTemp = '' then
    begin
      Result := False;
      Exit;
    end;

    if not CreateDir(ADirTemp) then
    begin
      PrevTemp := ADirTemp;
      ADirTemp := ExtractFileDir(ADirTemp);
      if PrevTemp = ADirTemp then
      begin
        Result := False;
        Exit;
      end;
    end
    else
      ADirTemp := ADir;
  until DirectoryExists(ADir);
end;


function Pow16(x: Byte): Cardinal;
var
  i: Byte;
begin
  Result := 1;
  for i := 1 to x do
    Result := Result shl 4;
end;


function HexaDigitToByte(ch: Char): Byte;
begin
  Result := 0;
  Ch := UpCase(Ch);
  if Ch in ['0'..'9'] then
  begin
    Result := Ord(Ch) - 48;
    Exit;
  end;

  if Ch in ['A'..'F'] then
    Result := Ord(Ch) - 65 + 10;
end;


function HexToInt(s: string): Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := Length(s) downto 1 do
    if s[i] in ['0'..'9', 'a'..'f', 'A'..'F'] then
      Result := Result + HexaDigitToByte(s[i]) * Pow16(Length(s) - i);
end;


function StringToHex(AStr: string): string;   //converts 'abc' to '616263'
var
  i, i2: Integer;
  TempHexVal: string;
begin
  if Length(AStr) = 0 then
  begin
    Result := '';
    Exit;
  end;

  SetLength(Result, Length(AStr) shl 1);
  for i := 1 to Length(AStr) do
  begin
    TempHexVal := IntToHex(Ord(Astr[i]), 2);
    i2 := (i - 1) shl 1 + 1;
    Move(TempHexVal[1], Result[i2], 2);
  end;
end;


function HexToString(AHex: string): string;   //converts '616263' to 'abc'
var
  i, i2: Integer;
  TempByte: Byte;
begin
  if Length(AHex) = 0 then
  begin
    Result := '';
    Exit;
  end;

  SetLength(Result, Length(AHex) shr 1);

  for i := 1 to Length(Result) do
  begin
    i2 := (i - 1) shl 1 + 1;
    TempByte := HexToInt(Copy(AHex, i2, 2));
    Result[i] := Chr(TempByte);
  end;
end;


procedure RawExpressionToParts(RawExpression: string; out Op1, Op2, OpEq: string);
var
  EqPos, i: Integer;
begin
  {EqPos := Pos('==', RawExpression);
  if EqPos = 0 then
    EqPos := Pos('<>', RawExpression)
  else}

  EqPos := 0;

  for i := Low(CComparisonOperators) to High(CComparisonOperators) do
  begin
    EqPos := Pos(CComparisonOperators[i], RawExpression);
    if EqPos > 0 then
      Break;
  end;

  if EqPos = 0 then
    raise Exception.Create('Unknown operator in expression:' + RawExpression);

  Op1 := Copy(RawExpression, 1, EqPos - 1);
  Op2 := Copy(RawExpression, EqPos + 2, MaxInt);
  OpEq := Copy(RawExpression, EqPos, 2);
end;


function MatchCriteriaToString(Criteria: TClkFindControlMatchCriteria): string;
begin
  Result := '';
  if Criteria.WillMatchText then
    Result := Result + 'Text';

  if Criteria.WillMatchClassName then
  begin
    if Result > '' then
      Result := Result + ', ';
    Result := Result + 'Class';
  end;

  if Criteria.WillMatchBitmapText then
  begin
    if Result > '' then
      Result := Result + ', ';
    Result := Result + 'Bmp';
  end;

  if Criteria.WillMatchBitmapFiles then
  begin
    if Result > '' then
      Result := Result + ', ';
    Result := Result + 'Bmp files';
  end;
end;


function EvaluateActionCondition(AActionCondition: string; AEvalReplacementsFunc: TEvaluateReplacementsFunc): Boolean;
var
  RawCondition: string;
  AStringList, AConditionPart: TStringList;
  i, j: Integer;
  PartialResult, EvalResult: Boolean;
  Op1, Op2, OpEq: string;
begin
  Result := False;
  EvalResult := False;

  if (AActionCondition = '') or (AActionCondition = #13#10) then
  begin
    Result := True;
    Exit;
  end;

  AStringList := TStringList.Create;
  try
    AStringList.Text := AActionCondition;

    for i := 0 to AStringList.Count - 1 do
    begin
      PartialResult := True;
      AConditionPart := TStringList.Create;
      try
        AConditionPart.Text := StringReplace(AStringList.Strings[i], #5#6, #13#10, [rfReplaceAll]);

        for j := 0 to AConditionPart.Count - 1 do
        begin
          RawCondition := AConditionPart.Strings[j];
          RawExpressionToParts(RawCondition, Op1, Op2, OpEq);

          if OpEq = CCompNotEqual then
            EvalResult := AEvalReplacementsFunc(Op1) <> AEvalReplacementsFunc(Op2)
          else
            if OpEq = CCompEqual then
              EvalResult := AEvalReplacementsFunc(Op1) = AEvalReplacementsFunc(Op2)
            else
              if OpEq = CCompLessThan then
                EvalResult := AEvalReplacementsFunc(Op1) < AEvalReplacementsFunc(Op2)
              else
                if OpEq = CCompGreaterThan then
                  EvalResult := AEvalReplacementsFunc(Op1) > AEvalReplacementsFunc(Op2)
                else
                  if OpEq = CCompLessThanOrEqual then
                    EvalResult := AEvalReplacementsFunc(Op1) <= AEvalReplacementsFunc(Op2)
                  else
                    if OpEq = CCompGreaterThanOrEqual then
                      EvalResult := AEvalReplacementsFunc(Op1) >= AEvalReplacementsFunc(Op2)
                    else
                      raise Exception.Create('operator "' + OpEq + '" not implemented in EvaluateActionCondition.');

          PartialResult := PartialResult and EvalResult;
        end; //for j
      finally
        AConditionPart.Free;
      end;

      Result := Result or PartialResult;
    end;
  finally
    AStringList.Free;
  end;
end;


function GetControlText(HW: THandle): string;
var
  TextLength: Integer;
begin
  Result := '';

  {$IFDEF Windows}
    TextLength := SendMessage(HW, WM_GETTEXTLENGTH, 0, 0);
    if TextLength <> 0 then
    begin
      SetLength(Result, TextLength shl 1 + 2);  //Allocating twice the size, might be extreme, but better safe than crash. Still not sure about UTF8 compatibility for 4-byte chars.
      SendMessage(HW, WM_GETTEXT, TextLength + 1, {%H-}PtrInt(@Result[1]));
      SetLength(Result, TextLength);
    end;
  {$ENDIF}
end;


function GetControlTextW(HW: THandle): string;
var
  TextLength: Integer;
  TempBuffer: array of Char;
  WideTempBuffer: PWideChar;
begin
  Result := '';

  {$IFDEF Windows}
    TextLength := SendMessage(HW, WM_GETTEXTLENGTH, 0, 0);
    if TextLength <> 0 then
    begin
      SetLength(Result, TextLength shl 1 + 2);  //Allocating twice the size, might be extreme, but better safe than crash. Still not sure about UTF8 compatibility for 4-byte chars.

      SetLength(TempBuffer, Length(Result) shl 1);
      SendMessageW(HW, WM_GETTEXT, TextLength + 1, {%H-}PtrInt(@TempBuffer[0]));     // W
      WideTempBuffer := @TempBuffer[0];
      Result := string(WideTempBuffer);

      SetLength(Result, TextLength);
    end;
  {$ENDIF}
end;


function GetWindowClassRec(HW: THandle): TCompRec; overload;
var
  CompName: array[0..1023] of Char;
begin
  Result.Handle := HW;

  try
    {$IFDEF Windows}
      if GetClassName(HW, CompName, Length(CompName) - 1) > 0 then
        Result.ClassName := string(CompName)
      else
    {$ENDIF}
      Result.ClassName := '';
  except
    on E: Exception do
      MessageBox(0, PChar('Ex on GetClassName: ' + E.Message), PChar(Application.Title), MB_ICONERROR);
  end;

  try
    GetWindowRect(HW, Result.ComponentRectangle);
  except
    on E: Exception do
      MessageBox(0, PChar('Ex on GetWindowRect: ' + E.Message), PChar(Application.Title), MB_ICONERROR);
  end;

  try
    if (Result.ComponentRectangle.Left = 0) and
       (Result.ComponentRectangle.Top = 0) and
       (Result.ComponentRectangle.Right = 0) and
       (Result.ComponentRectangle.Bottom = 0) then
      Result.Text := ''
    else
    begin
      if UseWideStringsOnGetControlText then
        Result.Text := GetControlTextW(HW)
      else
        Result.Text := GetControlText(HW);
    end;
  except
    on E: Exception do
    begin
      Result.Text := '';
      MessageBox(0, PChar('Ex on GetControlText: ' + E.Message), PChar(Application.Title), MB_ICONERROR);
    end;
  end;
end;


function GetWindowClassRec(CrPos: TPoint): TCompRec; overload;
begin
  Result := GetWindowClassRec(WindowFromPoint(CrPos));
  Result.MouseXOffset := CrPos.X - Result.ComponentRectangle.Left;
  Result.MouseYOffset := CrPos.Y - Result.ComponentRectangle.Top;
end;


function GetCmdLineOptionValue(AOption: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to ParamCount do
    if ParamStr(i) = AOption then
    begin
      Result := ParamStr(i + 1);
      Exit;
    end;
end;


function ActionAsStringToTClkAction(ActionAsString: string): TClkAction;
var
  i: TClkAction;
begin
  Result := acClick; //default

  for i := Low(TClkAction) to High(TClkAction) do
    if ActionAsString = CClkActionStr[i] then
    begin
      Result := i;
      Exit;
    end;
end;


{ TPaintedLabel }

procedure TPaintedLabel.Paint;
begin
  inherited Paint;

  Canvas.Pen.Style := psDashDot;
  Canvas.Pen.Color := $8888FF;

  if Width = 1 then
    Canvas.Line(0, 0, 0, Height);

  if Height = 1 then
    Canvas.Line(0, 0, Width, 0);
end;


procedure CreateSelectionLabels(AOwner: TComponent; AParent: TWinControl; var ALeftLabel, ATopLabel, ARightLabel, ABottomLabel: TLabel; ALeftColor, ATopColor, ARightColor, ABottomColor: TColor; AShouldBringToFront, ACreateWithPaintedLabel: Boolean);
begin
  if ACreateWithPaintedLabel then
  begin
    ALeftLabel := TPaintedLabel.Create(AOwner);
    ATopLabel := TPaintedLabel.Create(AOwner);
    ARightLabel := TPaintedLabel.Create(AOwner);
    ABottomLabel := TPaintedLabel.Create(AOwner);
  end
  else
  begin
    ALeftLabel := TLabel.Create(AOwner);
    ATopLabel := TLabel.Create(AOwner);
    ARightLabel := TLabel.Create(AOwner);
    ABottomLabel := TLabel.Create(AOwner);
  end;

  ALeftLabel.Parent := AParent;
  ATopLabel.Parent := AParent;
  ARightLabel.Parent := AParent;
  ABottomLabel.Parent := AParent;

  ALeftLabel.AutoSize := False;
  ATopLabel.AutoSize := False;
  ARightLabel.AutoSize := False;
  ABottomLabel.AutoSize := False;

  ALeftLabel.Caption := '';
  ATopLabel.Caption := '';
  ARightLabel.Caption := '';
  ABottomLabel.Caption := '';

  ALeftLabel.Color := ALeftColor;
  ATopLabel.Color := ATopColor;
  ARightLabel.Color := ARightColor;
  ABottomLabel.Color := ABottomColor;

  ALeftLabel.Width := 1;
  ATopLabel.Height := 1;
  ARightLabel.Width := 1;
  ABottomLabel.Height := 1;

  if AShouldBringToFront then
  begin
    ALeftLabel.BringToFront;
    ATopLabel.BringToFront;
    ARightLabel.BringToFront;
    ABottomLabel.BringToFront;
  end;

  ALeftLabel.Transparent := False;
  ATopLabel.Transparent := False;
  ARightLabel.Transparent := False;
  ABottomLabel.Transparent := False;
end;


function CompRecsAreEqual(ARecA, ARecB: TCompRec): Boolean;
begin
  Result := True;
  Result := Result and (ARecA.Handle = ARecB.Handle);
  Result := Result and (ARecA.ClassName = ARecB.ClassName);
  Result := Result and (ARecA.Text = ARecB.Text);
  Result := Result and (ARecA.ComponentRectangle = ARecB.ComponentRectangle);
  Result := Result and (ARecA.IsSubControl = ARecB.IsSubControl);
  Result := Result and (ARecA.MouseXOffset = ARecB.MouseXOffset);
  Result := Result and (ARecA.MouseYOffset = ARecB.MouseYOffset);
  Result := Result and (ARecA.XOffsetFromParent = ARecB.XOffsetFromParent);
  Result := Result and (ARecA.YOffsetFromParent = ARecB.YOffsetFromParent);
end;


function PartialResultExistsInFinalResult(var AResultedControlArr: TCompRecArr; APartialResultedControl: TCompRec): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to Length(AResultedControlArr) - 1 do
    if CompRecsAreEqual(AResultedControlArr[i], APartialResultedControl) then
    begin
      Result := True;
      Exit;
    end;
end;


procedure CopyPartialResultsToFinalResult(var AResultedControlArr, APartialResultedControlArr: TCompRecArr);
var
  i: Integer;
begin
  for i := 0 to Length(APartialResultedControlArr) - 1 do
    if not PartialResultExistsInFinalResult(AResultedControlArr, APartialResultedControlArr[i]) then
    begin
      SetLength(AResultedControlArr, Length(AResultedControlArr) + 1);
      AResultedControlArr[Length(AResultedControlArr) - 1] := APartialResultedControlArr[i];
    end;
end;


function ExtractFileNameNoExt(AFnm: string): string;
var
  FnmWithExt: string;
  PosExt: Integer;
begin
  FnmWithExt := ExtractFileName(AFnm);
  PosExt := Pos(ExtractFileExt(FnmWithExt), FnmWithExt);

  if PosExt > 0 then
    Result := Copy(FnmWithExt, 1, PosExt - 1)
  else
    Result := FnmWithExt;
end;


function ExtractFullFileNameNoExt(AFnm: string): string;
var
  FnmWithExt: string;
  PosExt: Integer;
begin
  FnmWithExt := AFnm;
  PosExt := Pos(ExtractFileExt(FnmWithExt), FnmWithExt);

  if PosExt > 0 then
    Result := Copy(FnmWithExt, 1, PosExt - 1)
  else
    Result := FnmWithExt;
end;

end.
