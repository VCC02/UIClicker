{
    Copyright (C) 2025 VCC
    creation date: Dec 2019
    initial release date: 13 Sep 2022

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


unit ClickerActionExecution;

{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF Windows}
    Windows,
  {$ELSE}
    LCLIntf, LCLType, Types,
  {$ENDIF}
  Classes, SysUtils, Forms, Graphics, ExtCtrls,
  ClickerUtils, ClickerPrimitiveUtils, ClickerActionsFrame, ClickerIniFiles,
  InMemFileSystem, ControlInteraction;


type
  TAllowsSteppingInto = (asiNo, asiYes);

  TOnSetEditorEnabledState = procedure(AEnabled: Boolean) of object;
  TOnSetEditorTimeoutProgressBarMax = procedure(AMaxValue: Integer) of object;
  TOnSetEditorTimeoutProgressBarPosition = procedure(APositionValue: Integer) of object;
  TOnWaitForBitmapsAvailability = procedure(ListOfBitmapFiles: TStringList) of object;
  TOnTerminateWaitForMultipleFilesAvailability = procedure of object;
  TOnCallTemplate = function(Sender: TObject; AFileNameToCall: string; ListOfVariables: TStrings; DebugBitmap: TBitmap; DebugGridImage: TImage; IsDebugging, AShouldStopAtBreakPoint: Boolean; AStackLevel: Integer; AExecutesRemotely: Boolean): Boolean of object;
  TOnSetEditorSleepInfo = procedure(AElapsedTime, ARemainingTime: string) of object;
  TOnGetSelfHandles = procedure(AListOfSelfHandles: TStringList) of object;
  TOnAddDefaultFontProfile = procedure(var AFindSubControlOptions: TClkFindSubControlOptions; var AActionOptions: TClkActionOptions) of object;

  TOnGetSetVarActionByName = function(var AClkSetVarOptions: TClkSetVarOptions; AActionName: string): Boolean of object;  //used before saving the action to file
  TOnUpdateSetVarActionByName = function(AClkSetVarOptions: TClkSetVarOptions; AActionName: string): Boolean of object;   //used after loading the action from file
  TOnBackupVars = procedure(AAllVars: TStringList) of object;
  TOnRestoreVars = procedure(AAllVars: TStringList) of object;

  TOnResolveTemplatePath = function(APath: string; ACustomSelfTemplateDir: string = ''; ACustomAppDir: string = ''): string of object;
  TOnExecuteActionByContent = function(var AAllActions: TClkActionsRecArr; AActionIndex: Integer): Boolean of object;
  TOnLoadTemplateToActions = function(Fnm: string; var AActions: TClkActionsRecArr; AWhichTemplate: TEditTemplateWhichTemplate; out ANotes, AIconPath: string; AWaitForFileAvailability: Boolean = False): string of object;
  TOnSaveCompleteTemplateToFile = function(Fnm: string; var AActions: TClkActionsRecArr; AWhichTemplate: TEditTemplateWhichTemplate; ANotes, AIconPath: string; AUpdateUI, AShouldSaveSelfTemplate: Boolean): string of object;

  TOnWaitInDebuggingMode = procedure(var ADebuggingAction: TClkActionRec; AActionAllowsSteppingInto: TAllowsSteppingInto) of object;

  TBrowserRenderingText = record
    Txt: string;
    RequestID: string;
    FontProfiles: TClkFindControlMatchBitmapTextArr;  //not all properties will be used in browser rendering
    RenderedFileNames: TStringArray; //this should have the same length as FontProfiles
    FontSizeUnit: string;
    Timeout: string;
    CritSec: TRTLCriticalSection;
  end;


  TActionExecution = class
  private
    FClickerVars: TStringList;  //not created here in this class, used from outside
    FStopAllActionsOnDemandFromParent: PBoolean;
    FStopAllActionsOnDemand: PBoolean;
    FPluginStepOver: PBoolean;
    FPluginContinueAll: PBoolean;
    FSelfTemplateFileName: PString;
    FExecutingActionFromRemote: PBoolean;
    FFileLocationOfDepsIsMem: PBoolean;
    FFullTemplatesDir: PString;
    FStackLevel: PInteger;
    FNextStackCall: TActionExecution;
    FExecutesRemotely: PBoolean;
    FOwnerFrame: TObject;

    FAllowedFileDirsForServer: PString;
    FAllowedFileExtensionsForServer: PString;

    FRenderingRequestPageCloseBrowserOnDone: Boolean;
    FExtraLogging_FindControl: Boolean;

    FfrClickerActions: TfrClickerActions;  ///////////////////////// temp

    FBrowserRenderingText: TBrowserRenderingText;

    FOnAddToLog: TOnAddToLog;
    FOnSetEditorEnabledState: TOnSetEditorEnabledState;
    FOnSetEditorTimeoutProgressBarMax: TOnSetEditorTimeoutProgressBarMax;
    FOnSetEditorTimeoutProgressBarPosition: TOnSetEditorTimeoutProgressBarPosition;
    FOnLoadBitmap: TOnLoadBitmap;
    FOnLoadRenderedBitmap: TOnLoadRenderedBitmap;
    FOnSaveRenderedBitmap: TOnSaveRenderedBitmap;
    FOnDeleteRenderedBitmap: TOnDeleteRenderedBitmap;
    FOnRenderBmpExternally: TOnRenderBmpExternally;
    FOnLoadRawPmtv: TOnLoadRawPmtv;
    FOnLoadPluginFromInMemFS: TOnLoadPluginFromInMemFS;
    FOnGetActionProperties: TOnGetActionProperties;
    FOnWaitForBitmapsAvailability: TOnWaitForBitmapsAvailability;
    FOnCallTemplate: TOnCallTemplate;
    FOnSetEditorSleepProgressBarMax: TOnSetEditorTimeoutProgressBarMax;
    FOnSetEditorSleepProgressBarPosition: TOnSetEditorTimeoutProgressBarPosition;
    FOnSetEditorSleepInfo: TOnSetEditorSleepInfo;
    FOnGetSelfHandles: TOnGetSelfHandles;
    FOnAddDefaultFontProfile: TOnAddDefaultFontProfile;
    FOnGetGridDrawingOption: TOnGetGridDrawingOption;
    FOnLoadPrimitivesFile: TOnLoadPrimitivesFile;

    FOnGetSetVarActionByName: TOnGetSetVarActionByName;
    FOnUpdateSetVarActionByName: TOnUpdateSetVarActionByName;
    FOnTClkIniReadonlyFileCreate: TOnTClkIniReadonlyFileCreate;
    FOnSaveStringListToFile: TOnSaveTemplateToFile;
    FOnBackupVars: TOnBackupVars;
    //FOnRestoreVars: TOnRestoreVars;
    FOnExecuteActionByName: TOnExecuteActionByName;
    FOnGetAllActions: TOnGetAllActions;
    FOnResolveTemplatePath: TOnResolveTemplatePath;
    FOnSetDebugPoint: TOnSetDebugPoint;
    FOnIsAtBreakPoint: TOnIsAtBreakPoint;
    FOnFileExists: TOnFileExists;
    FOnSaveTemplateToFile: TOnSaveTemplateToFile;

    FOnSaveFileToExtRenderingInMemFS: TOnSaveFileToExtRenderingInMemFS;
    FOnGenerateAndSaveTreeWithWinInterp: TOnGenerateAndSaveTreeWithWinInterp;
    FOnSetWinInterpOption: TOnSetWinInterpOption;
    FOnExecuteActionByContent: TOnExecuteActionByContent;
    FOnLoadTemplateToActions: TOnLoadTemplateToActions;
    FOnSaveCompleteTemplateToFile: TOnSaveCompleteTemplateToFile;

    FOnWaitInDebuggingMode: TOnWaitInDebuggingMode;
    FOnGetPluginInMemFS: TOnGetPluginInMemFS;
    FOnGetListeningPort: TOnGetListeningPort;

    function GetActionVarValue(VarName: string): string;
    procedure SetActionVarValue(VarName, VarValue: string);
    function EvaluateReplacements(s: string; Recursive: Boolean = True; AEvalTextCount: Integer = -1): string;
    procedure AppendErrorMessageToActionVar(NewErrMsg: string);
    procedure PrependErrorMessageToActionVar(NewErrMsg: string);
    function EvaluateHTTP(AValue: string; out AGeneratedException: Boolean): string;
    procedure PreviewTextOnBmp(var AFindSubControlOptions: TClkFindSubControlOptions; AEvaluatedText: string; AProfileIndex: Integer; ASearchedBmp: TBitmap);
    function GetActionProperties(AActionName: string): string;
    function ResolveAllowedFileDirs(AAllowedFileDirsForServer: string): string;

    procedure AddToLog(s: string);
    procedure OpenCLInfoToVars(s: string);
    function DoOnExecuteActionByName(AActionName: string): Boolean;

    function DoOnGetAllActions: PClkActionsRecArr;
    function DoOnResolveTemplatePath(APath: string; ACustomSelfTemplateDir: string = ''; ACustomAppDir: string = ''): string;
    procedure DoOnSetDebugPoint(ADebugPoint: string);
    function DoOnIsAtBreakPoint(ADebugPoint: string): Boolean;

    procedure SetLastActionStatus(AActionResult, AAlowedToFail: Boolean);
    function CheckManualStopCondition: Boolean;

    procedure ExecuteClickAction(var AClickOptions: TClkClickOptions);
    function ExecuteFindControlAction(var AFindControlOptions: TClkFindControlOptions; var AActionOptions: TClkActionOptions; AOutsideTickCount: QWord): Boolean; //returns True if found
    function ExecuteFindSubControlAction(var AFindSubControlOptions: TClkFindSubControlOptions; var AActionOptions: TClkActionOptions; AOutsideTickCount: QWord): Boolean; //returns True if found
    function FillInFindControlInputData(var AFindControlOptions: TClkFindControlOptions; const AActionOptions: TClkActionOptions; out FindControlInputData: TFindControlInputData; out FontProfilesCount: Integer): Boolean;
    function FillInFindSubControlInputDataForGPU(var AFindSubControlOptions: TClkFindSubControlOptions; var FindControlInputData: TFindControlInputData): Boolean;
    function FillInFindSubControlInputData(var AFindSubControlOptions: TClkFindSubControlOptions; var AActionOptions: TClkActionOptions; out FindControlInputData: TFindControlInputData; out FontProfilesCount: Integer): Boolean;
    procedure UpdateActionVarValuesFromControl(AControl: TCompRec; AUpdate_ResultedErrorCount: Boolean = False);
    procedure UpdateActionVarValuesFromResultedControlArr(var AResultedControlArr: TCompRecArr);
    procedure SetDbgImgPos(var AFindSubControlOptions: TClkFindSubControlOptions; AFindControlInputData: TFindControlInputData; AResultedControl: TCompRec; AFindControlOnScreen_Result: Boolean);
    procedure GPUDbgBufferToVars(var AGPUDbgBuffer: TIntArr);
    procedure SetAllControl_Handles_FromResultedControlArr(var AResultedControlArr: TCompRecArr; AMatchSource, ADetailedMatchSource: string);
    procedure InitFindControlParams(var AActionOptions: TClkActionOptions; AOutsideTickCount: QWord; var AResultedControl: TCompRec; var AInitialTickCount, ATimeout: QWord; var AFindControlInputData: TFindControlInputData; out AStopAllActionsOnDemandAddr: Pointer);

    procedure DoOnSetEditorEnabledState(AEnabled: Boolean);
    procedure DoOnSetEditorTimeoutProgressBarMax(AMaxValue: Integer);
    procedure DoOnSetEditorTimeoutProgressBarPosition(APositionValue: Integer);
    function DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function DoOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    procedure DoOnSaveRenderedBitmap(ABitmap: TBitmap; AFileName: string);
    procedure DoOnDeleteRenderedBitmap(AFileName: string);
    function DoOnRenderBmpExternally(ARequest: string): string;
    function DoOnLoadPluginFromInMemFS(APlugin: TMemoryStream; AFileName: string): Boolean;
    function DoOnGetActionProperties(AActionName: string): PClkActionRec;
    procedure DoOnWaitForBitmapsAvailability(AListOfFiles: TStringList);
    procedure DoOnSetEditorSleepProgressBarMax(AMaxValue: Integer);
    procedure DoOnSetEditorSleepProgressBarPosition(APositionValue: Integer);
    procedure DoOnSetEditorSleepInfo(AElapsedTime, ARemainingTime: string);
    procedure DoOnGetSelfHandles(AListOfSelfHandles: TStringList);
    procedure DoOnAddDefaultFontProfile(var AFindSubControlOptions: TClkFindSubControlOptions; var AActionOptions: TClkActionOptions);
    function DoOnGetGridDrawingOption: TOnGetGridDrawingOption;
    procedure DoOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);

    function DoOnGetSetVarActionByName(var AClkSetVarOptions: TClkSetVarOptions; AActionName: string): Boolean;
    function DoOnUpdateSetVarActionByName(AClkSetVarOptions: TClkSetVarOptions; AActionName: string): Boolean;
    function DoOnTClkIniReadonlyFileCreate(AFileName: string): TClkIniReadonlyFile;
    procedure DoOnSaveStringListToFile(AStringList: TStringList; const AFileName: string);
    procedure DoOnBackupVars(AAllVars: TStringList);
    procedure DoOnSaveFileToExtRenderingInMemFS(AFileName: string; AContent: Pointer; AFileSize: Int64);
    //procedure DoOnRestoreVars(AAllVars: TStringList);
    function DoOnGenerateAndSaveTreeWithWinInterp(AHandle: THandle; ATreeFileName: string; AStep: Integer; AUseMouseSwipe: Boolean): Boolean;
    function DoOnSetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue: string): Boolean;
    function DoOnFileExists(const FileName: string): Boolean;
    procedure DoOnSaveTemplateToFile(AStringList: TStringList; const AFileName: string);
    function DoOnExecuteActionByContent(var AAllActions: TClkActionsRecArr; AActionIndex: Integer): Boolean;
    function DoOnLoadTemplateToActions(Fnm: string; var AActions: TClkActionsRecArr; AWhichTemplate: TEditTemplateWhichTemplate; out ANotes, AIconPath: string; AWaitForFileAvailability: Boolean = False): string;
    function DoOnSaveCompleteTemplateToFile(Fnm: string; var AActions: TClkActionsRecArr; AWhichTemplate: TEditTemplateWhichTemplate; ANotes, AIconPath: string; AUpdateUI, AShouldSaveSelfTemplate: Boolean): string;

    procedure DoOnWaitInDebuggingMode(var ADebuggingAction: TClkActionRec; AActionAllowsSteppingInto: TAllowsSteppingInto);
    function DoOnGetPluginInMemFS: TInMemFileSystem;
    function DoOnGetListeningPort: Word;

    function HandleOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function HandleOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function HandleOnEvaluateReplacements(s: string; Recursive: Boolean = True; AEvalTextCount: Integer = -1): string;

    procedure HandleOnSetVar(AVarName, AVarValue: string);
    procedure HandleOnSetDebugPoint(ADebugPoint: string);
    function HandleOnIsAtBreakPoint(ADebugPoint: string): Boolean;
    procedure HandleOnSaveFileToExtRenderingInMemFS(AFileName: string; AContent: Pointer; AFileSize: Int64);
    function HandleOnScreenshotByActionName(AActionName: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function ExecuteMultiClickAction(var AClickOptions: TClkClickOptions): Boolean;
    function ExecuteExecAppAction(var AExecAppOptions: TClkExecAppOptions; var AActionOptions: TClkActionOptions): Boolean;
    function ExecuteFindControlActionWithTimeout(var AFindControlOptions: TClkFindControlOptions; var AActionOptions: TClkActionOptions): Boolean; //returns True if found
    function ExecuteFindSubControlActionWithTimeout(var AFindSubControlOptions: TClkFindSubControlOptions; var AActionOptions: TClkActionOptions): Boolean; //returns True if found
    function ExecuteSetControlTextAction(var ASetTextOptions: TClkSetTextOptions): Boolean;
    function ExecuteCallTemplateAction(var ACallTemplateOptions: TClkCallTemplateOptions; IsDebugging, AShouldStopAtBreakPoint: Boolean): Boolean; //to be moved to private, after ExecuteFindControlAction header
    function ExecuteLoopedCallTemplateAction(var ACallTemplateOptions: TClkCallTemplateOptions; IsDebugging, AShouldStopAtBreakPoint: Boolean): Boolean;
    function ExecuteSleepAction(var ASleepOptions: TClkSleepOptions; var AActionOptions: TClkActionOptions): Boolean;
    function ExecuteSetVarAction(var ASetVarOptions: TClkSetVarOptions): Boolean;
    function ExecuteWindowOperationsAction(var AWindowOperationsOptions: TClkWindowOperationsOptions): Boolean;
    function ExecuteLoadSetVarFromFileAction(var ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions): Boolean;
    function ExecuteSaveSetVarToFileAction(var ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions): Boolean;
    function ExecutePluginAction(var APluginOptions: TClkPluginOptions; AAllActions: PClkActionsRecArr; AListOfAllVars: TStringList; AResolvedPluginPath: string; IsDebugging, AShouldStopAtBreakPoint: Boolean): Boolean;
    function ExecuteEditTemplateAction(var AEditTemplateOptions: TClkEditTemplateOptions): Boolean;

    function ExecuteClickActionAsString(AListOfClickOptionsParams: TStrings): Boolean;
    function ExecuteExecAppActionAsString(AListOfExecAppOptionsParams: TStrings): Boolean;
    function ExecuteFindControlActionAsString(AListOfFindControlOptionsParams: TStrings): Boolean;
    function ExecuteFindSubControlActionAsString(AListOfFindSubControlOptionsParams: TStrings): Boolean;
    function ExecuteSetControlTextActionAsString(AListOfSetControlTextOptionsParams: TStrings): Boolean;
    function ExecuteCallTemplateActionAsString(AListOfCallTemplateOptionsParams: TStrings): Boolean;
    function ExecuteSleepActionAsString(AListOfSleepOptionsParams: TStrings): Boolean;
    function ExecuteSetVarActionAsString(AListOfSetVarOptionsParams: TStrings): Boolean;
    function ExecuteWindowOperationsActionAsString(AListOfWindowOperationsOptionsParams: TStrings): Boolean;
    function ExecuteLoadSetVarFromFileActionAsString(AListOfLoadSetVarOptionsParams: TStrings): Boolean;
    function ExecuteSaveSetVarToFileActionAsString(AListOfSaveSetVarOptionsParams: TStrings): Boolean;
    function ExecutePluginActionAsString(AListOfPluginOptionsParams: TStrings): Boolean;
    function ExecuteEditTemplateActionAsString(AListOfEditTemplateOptionsParams: TStrings): Boolean;

    function GetTextRenderingPage(AHTTPParams: TStrings): string;

    //using pointers for the following properties, because the values they are pointing to, can be updated later, not when this class is created
    property ClickerVars: TStringList write FClickerVars;  //not created here in this class, used from outside
    property StopAllActionsOnDemandFromParent: PBoolean write FStopAllActionsOnDemandFromParent;
    property StopAllActionsOnDemand: PBoolean write FStopAllActionsOnDemand;
    property PluginStepOver: PBoolean write FPluginStepOver;
    property PluginContinueAll: PBoolean write FPluginContinueAll;

    property SelfTemplateFileName: PString write FSelfTemplateFileName;
    property ExecutingActionFromRemote: PBoolean write FExecutingActionFromRemote;
    property FileLocationOfDepsIsMem: PBoolean write FFileLocationOfDepsIsMem;
    property FullTemplatesDir: PString write FFullTemplatesDir;
    property StackLevel: PInteger read FStackLevel write FStackLevel;
    property NextStackCall: TActionExecution read FNextStackCall write FNextStackCall;
    property ExecutesRemotely: PBoolean write FExecutesRemotely;
    property OwnerFrame: TObject write FOwnerFrame;

    property AllowedFileDirsForServer: PString write FAllowedFileDirsForServer;
    property AllowedFileExtensionsForServer: PString write FAllowedFileExtensionsForServer;

    property RenderingRequestPageCloseBrowserOnDone: Boolean read FRenderingRequestPageCloseBrowserOnDone write FRenderingRequestPageCloseBrowserOnDone;
    property ExtraLogging_FindControl: Boolean read FExtraLogging_FindControl write FExtraLogging_FindControl;

    property frClickerActions: TfrClickerActions read FfrClickerActions write FfrClickerActions;  //not created here in this class, used from outside    ///////////////////////// temp

    property OnAddToLog: TOnAddToLog write FOnAddToLog;
    property OnSetEditorEnabledState: TOnSetEditorEnabledState write FOnSetEditorEnabledState;
    property OnSetEditorTimeoutProgressBarMax: TOnSetEditorTimeoutProgressBarMax write FOnSetEditorTimeoutProgressBarMax;
    property OnSetEditorTimeoutProgressBarPosition: TOnSetEditorTimeoutProgressBarPosition write FOnSetEditorTimeoutProgressBarPosition;
    property OnLoadBitmap: TOnLoadBitmap write FOnLoadBitmap;
    property OnLoadRenderedBitmap: TOnLoadRenderedBitmap write FOnLoadRenderedBitmap;
    property OnSaveRenderedBitmap: TOnSaveRenderedBitmap write FOnSaveRenderedBitmap;
    property OnDeleteRenderedBitmap: TOnDeleteRenderedBitmap write FOnDeleteRenderedBitmap;
    property OnRenderBmpExternally: TOnRenderBmpExternally write FOnRenderBmpExternally;
    property OnLoadRawPmtv: TOnLoadRawPmtv read FOnLoadRawPmtv write FOnLoadRawPmtv;
    property OnLoadPluginFromInMemFS: TOnLoadPluginFromInMemFS write FOnLoadPluginFromInMemFS;
    property OnGetActionProperties: TOnGetActionProperties write FOnGetActionProperties;
    property OnWaitForBitmapsAvailability: TOnWaitForBitmapsAvailability write FOnWaitForBitmapsAvailability;
    property OnCallTemplate: TOnCallTemplate write FOnCallTemplate;
    property OnSetEditorSleepProgressBarMax: TOnSetEditorTimeoutProgressBarMax write FOnSetEditorSleepProgressBarMax;
    property OnSetEditorSleepProgressBarPosition: TOnSetEditorTimeoutProgressBarPosition write FOnSetEditorSleepProgressBarPosition;
    property OnSetEditorSleepInfo: TOnSetEditorSleepInfo write FOnSetEditorSleepInfo;
    property OnGetSelfHandles: TOnGetSelfHandles write FOnGetSelfHandles;
    property OnAddDefaultFontProfile: TOnAddDefaultFontProfile write FOnAddDefaultFontProfile;
    property OnGetGridDrawingOption: TOnGetGridDrawingOption write FOnGetGridDrawingOption;
    property OnLoadPrimitivesFile: TOnLoadPrimitivesFile write FOnLoadPrimitivesFile;

    property OnGetSetVarActionByName: TOnGetSetVarActionByName write FOnGetSetVarActionByName;
    property OnUpdateSetVarActionByName: TOnUpdateSetVarActionByName write FOnUpdateSetVarActionByName;
    property OnTClkIniReadonlyFileCreate: TOnTClkIniReadonlyFileCreate write FOnTClkIniReadonlyFileCreate;
    property OnSaveStringListToFile: TOnSaveTemplateToFile write FOnSaveStringListToFile;
    property OnBackupVars: TOnBackupVars write FOnBackupVars;
    //property OnRestoreVars: TOnRestoreVars write FOnRestoreVars;
    property OnExecuteActionByName: TOnExecuteActionByName write FOnExecuteActionByName;
    property OnGetAllActions: TOnGetAllActions write FOnGetAllActions;
    property OnResolveTemplatePath: TOnResolveTemplatePath write FOnResolveTemplatePath;
    property OnSetDebugPoint: TOnSetDebugPoint write FOnSetDebugPoint;
    property OnIsAtBreakPoint: TOnIsAtBreakPoint write FOnIsAtBreakPoint;
    property OnFileExists: TOnFileExists write FOnFileExists;
    property OnSaveTemplateToFile: TOnSaveTemplateToFile write FOnSaveTemplateToFile;

    property OnSaveFileToExtRenderingInMemFS: TOnSaveFileToExtRenderingInMemFS write FOnSaveFileToExtRenderingInMemFS;
    property OnGenerateAndSaveTreeWithWinInterp: TOnGenerateAndSaveTreeWithWinInterp write FOnGenerateAndSaveTreeWithWinInterp;
    property OnSetWinInterpOption: TOnSetWinInterpOption write FOnSetWinInterpOption;
    property OnExecuteActionByContent: TOnExecuteActionByContent write FOnExecuteActionByContent;
    property OnLoadTemplateToActions: TOnLoadTemplateToActions write FOnLoadTemplateToActions;
    property OnSaveCompleteTemplateToFile: TOnSaveCompleteTemplateToFile write FOnSaveCompleteTemplateToFile;

    property OnWaitInDebuggingMode: TOnWaitInDebuggingMode write FOnWaitInDebuggingMode;
    property OnGetPluginInMemFS: TOnGetPluginInMemFS write FOnGetPluginInMemFS;
    property OnGetListeningPort: TOnGetListeningPort write FOnGetListeningPort;
  end;


implementation


uses
  MouseStuff, Controls,
  {$IFnDEF FPC}
    ShellAPI,
  {$ELSE}
    Process,
  {$ENDIF}
  IdHTTP, ClickerPrimitivesCompositor, ClickerPrimitives, ClickerActionProperties,
  ClickerActionPluginLoader, ClickerActionPlugins, BitmapProcessing,
  ClickerActionsClient, ClickerTemplates, Math, CLHeaders, ClickerCLUtils;


constructor TActionExecution.Create;
begin
  //inherited Create;
  FClickerVars := nil; //not created here in this class, used from outside
  FfrClickerActions := nil;
  FExecutingActionFromRemote := nil;
  FFileLocationOfDepsIsMem := nil;
  FFullTemplatesDir := nil;
  FStackLevel := nil;
  FNextStackCall := nil;
  FExecutesRemotely := nil;
  FOwnerFrame := nil;

  FAllowedFileDirsForServer := nil;
  FAllowedFileExtensionsForServer := nil;

  FRenderingRequestPageCloseBrowserOnDone := True;
  FExtraLogging_FindControl := False;

  FPluginStepOver := nil;
  FPluginContinueAll := nil;

  {$IFDEF Windows}
    InitializeCriticalSection(FBrowserRenderingText.CritSec);
  {$ELSE}
    InitCriticalSection(FBrowserRenderingText.CritSec);
  {$ENDIF}

  FOnAddToLog := nil;
  FOnSetEditorEnabledState := nil;
  FOnSetEditorTimeoutProgressBarMax := nil;
  FOnSetEditorTimeoutProgressBarPosition := nil;
  FOnLoadBitmap := nil;
  FOnLoadRenderedBitmap := nil;
  FOnSaveRenderedBitmap := nil;
  FOnDeleteRenderedBitmap := nil;
  FOnRenderBmpExternally := nil;
  FOnLoadRawPmtv := nil;
  FOnLoadPluginFromInMemFS := nil;
  FOnGetActionProperties := nil;
  FOnWaitForBitmapsAvailability := nil;
  FOnCallTemplate := nil;
  FOnSetEditorSleepProgressBarMax := nil;
  FOnSetEditorSleepProgressBarPosition := nil;
  FOnSetEditorSleepInfo := nil;
  FOnGetSelfHandles := nil;
  FOnAddDefaultFontProfile := nil;
  FOnGetGridDrawingOption := nil;
  FOnLoadPrimitivesFile := nil;

  FOnGetSetVarActionByName := nil;
  FOnUpdateSetVarActionByName := nil;
  FOnTClkIniReadonlyFileCreate := nil;
  FOnSaveStringListToFile := nil;
  FOnBackupVars := nil;
  //FOnRestoreVars := nil;
  FOnExecuteActionByName := nil;
  FOnGetAllActions := nil;
  FOnResolveTemplatePath := nil;
  FOnSetDebugPoint := nil;
  FOnIsAtBreakPoint := nil;
  FOnFileExists := nil;
  FOnSaveTemplateToFile := nil;

  FOnSaveFileToExtRenderingInMemFS := nil;
  FOnGenerateAndSaveTreeWithWinInterp := nil;
  FOnSetWinInterpOption := nil;
  FOnExecuteActionByContent := nil;
  FOnLoadTemplateToActions := nil;
  FOnSaveCompleteTemplateToFile := nil;

  FOnWaitInDebuggingMode := nil;
  FOnGetPluginInMemFS := nil;
  FOnGetListeningPort := nil;
end;


destructor TActionExecution.Destroy;
begin
  FClickerVars := nil;
  DoneCriticalSection(FBrowserRenderingText.CritSec);
  inherited Destroy;
end;


function TActionExecution.GetActionVarValue(VarName: string): string;
begin
  if FClickerVars = nil then
    raise Exception.Create('ClickerVars is not assigned.');

  Result := FClickerVars.Values[VarName];
end;


procedure TActionExecution.SetActionVarValue(VarName, VarValue: string);
begin
  FClickerVars.Values[VarName] := FastReplace_ReturnTo68(VarValue);  //Do not use EvaluateReplacements(VarValue) here, because there are calls which expect the value to be directly assigned !

  if VarName = '$ExecAction_Err$' then
    AddToLog(DateTimeToStr(Now) + '  ' + VarValue);
end;


function TActionExecution.EvaluateReplacements(s: string; Recursive: Boolean = True; AEvalTextCount: Integer = -1): string;
begin
  Result := EvaluateAllReplacements(FClickerVars, s, Recursive, AEvalTextCount);
end;


procedure TActionExecution.AppendErrorMessageToActionVar(NewErrMsg: string);
begin
  SetActionVarValue('$ExecAction_Err$', GetActionVarValue('$ExecAction_Err$') + FastReplace_ReturnTo68(NewErrMsg));
end;


procedure TActionExecution.PrependErrorMessageToActionVar(NewErrMsg: string);
begin
  SetActionVarValue('$ExecAction_Err$', FastReplace_ReturnTo68(NewErrMsg) + GetActionVarValue('$ExecAction_Err$'));
end;


function TActionExecution.EvaluateHTTP(AValue: string; out AGeneratedException: Boolean): string;
var
  TempIdHTTP: TIdHTTP;
begin
  Result := AValue;
  AGeneratedException := False;

  if (Pos('$HTTP://', UpperCase(AValue)) > 0) or (Pos('$HTTPS://', UpperCase(AValue)) > 0) then
    if AValue[Length(AValue)] = '$' then
    begin
      AValue := Copy(AValue, 2, Length(AValue) - 2);
      AValue := EvaluateReplacements(AValue);

      try
        TempIdHTTP := TIdHTTP.Create(nil);
        try
          TempIdHTTP.ConnectTimeout := 1000;    //These values should be increased if using a remote server. However, they will slow down the execution in that case.
          TempIdHTTP.ReadTimeout := 1000;
          Result := TempIdHTTP.Get(AValue);
        finally
          TempIdHTTP.Free;
        end;
      except
        on E: Exception do
        begin
          Result := E.Message;
          AGeneratedException := True;
          AppendErrorMessageToActionVar(Result);
        end;
      end;
    end;
end;


procedure TActionExecution.PreviewTextOnBmp(var AFindSubControlOptions: TClkFindSubControlOptions; AEvaluatedText: string; AProfileIndex: Integer; ASearchedBmp: TBitmap);
const
  CFontQualitiesStr: array[TFontQuality] of string = ('Default', 'Draft', 'Proof', 'NonAntialiased', 'Antialiased', 'Cleartype', 'CleartypeNatural');
var
  TextDimensions: TSize;
  TextToDisplay: string;
  FontQualityReplacement: Integer;
  FontQualityReplacementStr: string;
  EvalFG, EvalBG: string;
  EvalFGCol, EvalBGCol: TColor;
  i: TFontQuality;
  CropLeft, CropTop, CropRight, CropBottom: Integer;
  APreviewBmp: TBitmap;
  TextWidthAfterCropping, TextHeightAfterCropping: Integer;
  WorkingRect: TRect;
begin
  TextToDisplay := AEvaluatedText; //ask once    - it expects an already evaluated text, which is also used somewhere else, so evaluated as less as possible

  if Length(AFindSubControlOptions.MatchBitmapText) = 0 then
  begin
    AddToLog('No font profiles are available when previewing text on bmp.  Text = "' + AEvaluatedText + '".');
    Exit;
  end;

  APreviewBmp := TBitmap.Create;
  try
    APreviewBmp.PixelFormat := pf24bit;

    EvalFG := EvaluateReplacements(AFindSubControlOptions.MatchBitmapText[AProfileIndex].ForegroundColor, True);
    EvalBG := EvaluateReplacements(AFindSubControlOptions.MatchBitmapText[AProfileIndex].BackgroundColor, True);

    EvalFGCol := HexToInt(EvalFG);
    EvalBGCol := HexToInt(EvalBG);

    APreviewBmp.Canvas.Font.Color := EvalFGCol;
    APreviewBmp.Canvas.Font.Name := EvaluateReplacements(AFindSubControlOptions.MatchBitmapText[AProfileIndex].FontName);

    APreviewBmp.Canvas.Font.Size := AFindSubControlOptions.MatchBitmapText[AProfileIndex].FontSize;

    APreviewBmp.Canvas.Font.Style := [];

    if AFindSubControlOptions.MatchBitmapText[AProfileIndex].Bold then
      APreviewBmp.Canvas.Font.Style := APreviewBmp.Canvas.Font.Style + [fsBold];

    if AFindSubControlOptions.MatchBitmapText[AProfileIndex].Italic then
      APreviewBmp.Canvas.Font.Style := APreviewBmp.Canvas.Font.Style + [fsItalic];

    if AFindSubControlOptions.MatchBitmapText[AProfileIndex].Underline then
      APreviewBmp.Canvas.Font.Style := APreviewBmp.Canvas.Font.Style + [fsUnderline];

    if AFindSubControlOptions.MatchBitmapText[AProfileIndex].StrikeOut then
      APreviewBmp.Canvas.Font.Style := APreviewBmp.Canvas.Font.Style + [fsStrikeOut];

    if AFindSubControlOptions.MatchBitmapText[AProfileIndex].FontQualityUsesReplacement then
    begin
      FontQualityReplacementStr := EvaluateReplacements(AFindSubControlOptions.MatchBitmapText[AProfileIndex].FontQualityReplacement);  //should return a string in the following set: 'Default', 'Draft', 'Proof', 'NonAntialiased', 'Antialiased', 'Cleartype', 'CleartypeNatural'
      FontQualityReplacement := -1;
      for i := Low(TFontQuality) to High(TFontQuality) do
        if FontQualityReplacementStr = CFontQualitiesStr[i] then
        begin
          FontQualityReplacement := Ord(i);
          Break;
        end;

      if FontQualityReplacement = -1 then
        FontQualityReplacement := 0;  //default to fqDefault

      APreviewBmp.Canvas.Font.Quality := TFontQuality(FontQualityReplacement);
    end
    else
      APreviewBmp.Canvas.Font.Quality := TFontQuality(AFindSubControlOptions.MatchBitmapText[AProfileIndex].FontQuality);

    APreviewBmp.Canvas.Font.CharSet := AFindSubControlOptions.MatchBitmapText[AProfileIndex].CharSet;
    APreviewBmp.Canvas.Font.Orientation := AFindSubControlOptions.MatchBitmapText[AProfileIndex].Orientation;
    APreviewBmp.Canvas.Font.Pitch := AFindSubControlOptions.MatchBitmapText[AProfileIndex].Pitch;

    APreviewBmp.Canvas.Brush.Color := EvalBGCol;
    APreviewBmp.Canvas.Pen.Color := EvalBGCol;  //yes, BG

    if AFindSubControlOptions.MatchBitmapText[AProfileIndex].Orientation = 0 then
    begin
      TextDimensions := APreviewBmp.Canvas.TextExtent(TextToDisplay);
      APreviewBmp.Width := TextDimensions.cx;
      APreviewBmp.Height := TextDimensions.cy;

      APreviewBmp.Canvas.Rectangle(0, 0, APreviewBmp.Width - 1, APreviewBmp.Height - 1);
      APreviewBmp.Canvas.TextOut(0, 0, TextToDisplay);     //Do not use replacements here. The editbox should already be updated with replaced strings.
    end
    else
    begin
      WorkingRect := GetRotatedDrawingRectangle(APreviewBmp.Canvas, TextToDisplay);
      TextDimensions.cx := WorkingRect.Width;
      TextDimensions.cy := WorkingRect.Height;

      APreviewBmp.Width := TextDimensions.cx;
      APreviewBmp.Height := TextDimensions.cy;

      APreviewBmp.Canvas.Rectangle(0, 0, WorkingRect.Width, WorkingRect.Height);  // -1 ???
      APreviewBmp.Canvas.TextOut(WorkingRect.Left, WorkingRect.Top, TextToDisplay);
    end;


    CropLeft := Max(StrToIntDef(EvaluateReplacements(AFindSubControlOptions.MatchBitmapText[AProfileIndex].CropLeft), 0), 0);
    CropTop := Max(StrToIntDef(EvaluateReplacements(AFindSubControlOptions.MatchBitmapText[AProfileIndex].CropTop), 0), 0);
    CropRight := Max(StrToIntDef(EvaluateReplacements(AFindSubControlOptions.MatchBitmapText[AProfileIndex].CropRight), 0), 0);
    CropBottom := Max(StrToIntDef(EvaluateReplacements(AFindSubControlOptions.MatchBitmapText[AProfileIndex].CropBottom), 0), 0);

    ASearchedBmp.PixelFormat := pf24bit;
    ASearchedBmp.Canvas.Pen.Color := clLime;
    ASearchedBmp.Canvas.Brush.Color := clLime;
    if (CropLeft <> 0) or (CropTop <> 0) or (CropRight <> 0) or (CropBottom <> 0) then
    begin
      TextWidthAfterCropping := TextDimensions.cx - (CropLeft + CropRight); //CropLeft is increased as left -> right (towards the text). CropRight is increased right-> left  (towards the text).
      TextHeightAfterCropping := TextDimensions.cy - (CropTop + CropBottom);

      if TextWidthAfterCropping = 0 then
        raise Exception.Create('The text width, after cropping, is 0.');

      if TextHeightAfterCropping = 0 then
        raise Exception.Create('The text height, after cropping, is 0.');

      if TextWidthAfterCropping < 0 then
        raise Exception.Create('The text width, after cropping, is negative.');

      if TextHeightAfterCropping < 0 then
        raise Exception.Create('The text height, after cropping, is negative.');

      ASearchedBmp.Width := TextWidthAfterCropping;
      ASearchedBmp.Height := TextHeightAfterCropping;

      ASearchedBmp.Canvas.Rectangle(0, 0, ASearchedBmp.Width, ASearchedBmp.Height); //this rectangle is required, for proper content copying
      BitBlt(ASearchedBmp.Canvas.Handle, 0, 0, ASearchedBmp.Width, ASearchedBmp.Height, APreviewBmp.Canvas.Handle, CropLeft, CropTop, SRCCOPY);

      //HDC hdcDest, // handle to destination DC
      //int nXDest,  // x-coord of destination upper-left corner
      //int nYDest,  // y-coord of destination upper-left corner
      //int nWidth,  // width of destination rectangle
      //int nHeight, // height of destination rectangle
      //HDC hdcSrc,  // handle to source DC
      //int nXSrc,   // x-coordinate of source upper-left corner
      //int nYSrc,   // y-coordinate of source upper-left corner
      //DWORD dwRop  // raster operation code
    end
    else
    begin
      ASearchedBmp.Width := TextDimensions.cx;
      ASearchedBmp.Height := TextDimensions.cy;
      ASearchedBmp.Canvas.Rectangle(0, 0, ASearchedBmp.Width, ASearchedBmp.Height); //this rectangle is required, for proper content copying
      BitBlt(ASearchedBmp.Canvas.Handle, 0, 0, ASearchedBmp.Width, ASearchedBmp.Height, APreviewBmp.Canvas.Handle, 0, 0, SRCCOPY);
    end;
  finally
    APreviewBmp.Free;
  end;
end;


function TActionExecution.GetActionProperties(AActionName: string): string;
var
  Action: PClkActionRec;
begin
  Action := DoOnGetActionProperties(AActionName);
  if Action = nil then
  begin
    Result := 'Action name not found.';
    Exit;
  end;

  case Action^.ActionOptions.Action of
    acClick:
      Result := GetClickActionProperties(Action.ClickOptions);

    acExecApp:
      Result := GetExecAppActionProperties(Action.ExecAppOptions);

    acFindControl:
      Result := GetFindControlActionProperties(Action.FindControlOptions);

    acFindSubControl:
      Result := GetFindSubControlActionProperties(Action.FindSubControlOptions);

    acSetControlText:
      Result := GetSetControlTextActionProperties(Action.SetTextOptions);

    acCallTemplate:
      Result := GetCallTemplateActionProperties(Action.CallTemplateOptions);

    acSleep:
      Result := GetSleepActionProperties(Action.SleepOptions);

    acSetVar:
      Result := GetSetVarActionProperties(Action.SetVarOptions);

    acWindowOperations:
      Result := GetWindowOperationsActionProperties(Action.WindowOperationsOptions);

    acLoadSetVarFromFile:
      Result := GetLoadSetVarFromFileActionProperties(Action.LoadSetVarFromFileOptions);

    acSaveSetVarToFile:
      Result := GetSaveSetVarToFileActionProperties(Action.SaveSetVarToFileOptions);

    acPlugin:
      Result := GetPluginActionProperties(Action.PluginOptions);

    acEditTemplate:
      Result := GetEditTemplateActionProperties(Action.EditTemplateOptions);
  end;
end;


procedure TActionExecution.AddToLog(s: string);
begin
  if not Assigned(FOnAddToLog) then
    raise Exception.Create('OnAddToLog not assigned.');

  FOnAddToLog(s);
end;


procedure TActionExecution.OpenCLInfoToVars(s: string);
const
  CSep: string = ': ';
var
  VarName, VarValue: string;
begin
  if Trim(s) = '' then
    Exit;

  VarName := 'CL.' + Trim(Copy(s, 1, Pos(CSep, s) - 1));   //using a prefix, in case there are other vars with similar names (e.g. DeviceCount)
  VarValue := Trim(Copy(s, Pos(CSep, s) + Length(CSep), MaxInt));
  SetActionVarValue('$' + VarName + '$', VarValue);
end;


procedure TActionExecution.SetLastActionStatus(AActionResult, AAlowedToFail: Boolean);
begin
  if AActionResult then
  begin
    SetActionVarValue('$LastAction_Status$', CActionStatusStr[asSuccessful]);
    SetActionVarValue('$ExecAction_Err$', ''); //This might be a bit of a breaking change, but without clearing this var, it makes no sense to still have an error.
  end
  else
  begin
    if AAlowedToFail then
      SetActionVarValue('$LastAction_Status$', CActionStatusStr[asAllowedFailed])
    else
      SetActionVarValue('$LastAction_Status$', CActionStatusStr[asFailed])
  end;
end;


//function HandleOnLoadAllowedBitmap: ;

procedure TActionExecution.DoOnSetEditorEnabledState(AEnabled: Boolean);
begin
  if Assigned(FOnSetEditorEnabledState) then
    FOnSetEditorEnabledState(AEnabled)
  else
    raise Exception.Create('FOnSetEditorEnabledState is not assigned.');
end;


procedure TActionExecution.DoOnSetEditorTimeoutProgressBarMax(AMaxValue: Integer);
begin
  if Assigned(FOnSetEditorTimeoutProgressBarMax) then
    FOnSetEditorTimeoutProgressBarMax(AMaxValue)
  else
    raise Exception.Create('FOnSetEditorTimeoutProgressBarMax is not assigned.');
end;


procedure TActionExecution.DoOnSetEditorTimeoutProgressBarPosition(APositionValue: Integer);
begin
  if Assigned(FOnSetEditorTimeoutProgressBarPosition) then
    FOnSetEditorTimeoutProgressBarPosition(APositionValue)
  else
    raise Exception.Create('FOnSetEditorTimeoutProgressBarPosition is not assigned.');
end;


function TActionExecution.DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if Assigned(FOnLoadBitmap) then
    Result := FOnLoadBitmap(ABitmap, AFileName)
  else
    raise Exception.Create('OnLoadBitmap is not assigned.');
end;


function TActionExecution.DoOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if Assigned(FOnLoadRenderedBitmap) then
    Result := FOnLoadRenderedBitmap(ABitmap, AFileName)
  else
    raise Exception.Create('OnLoadRenderedBitmap is not assigned.');
end;


procedure TActionExecution.DoOnSaveRenderedBitmap(ABitmap: TBitmap; AFileName: string);
begin
  if Assigned(FOnSaveRenderedBitmap) then
    FOnSaveRenderedBitmap(ABitmap, AFileName)
  else
    raise Exception.Create('OnSaveRenderedBitmap is not assigned.');
end;


procedure TActionExecution.DoOnDeleteRenderedBitmap(AFileName: string);
begin
  if Assigned(FOnDeleteRenderedBitmap) then
    FOnDeleteRenderedBitmap(AFileName)
  else
    raise Exception.Create('OnDeleteRenderedBitmap is not assigned.');
end;


function TActionExecution.DoOnRenderBmpExternally(ARequest: string): string;
begin
  if Assigned(FOnRenderBmpExternally) then
    Result := FOnRenderBmpExternally(ARequest)
  else
    raise Exception.Create('OnRenderBmpExternally is not assigned.');
end;


function TActionExecution.DoOnLoadPluginFromInMemFS(APlugin: TMemoryStream; AFileName: string): Boolean;
begin
  if Assigned(FOnLoadPluginFromInMemFS) then
    Result := FOnLoadPluginFromInMemFS(APlugin, AFileName)
  else
    Result := False;
end;


function TActionExecution.DoOnGetActionProperties(AActionName: string): PClkActionRec;
begin
  if Assigned(FOnGetActionProperties) then
    Result := FOnGetActionProperties(AActionName)
  else
    raise Exception.Create('OnGetActionProperties is not assigned.');
end;


procedure TActionExecution.DoOnWaitForBitmapsAvailability(AListOfFiles: TStringList);
begin
  if Assigned(FOnWaitForBitmapsAvailability) then
    FOnWaitForBitmapsAvailability(AListOfFiles)
  else
    raise Exception.Create('OnWaitForBitmapsAvailability is not assigned.');
end;


procedure TActionExecution.DoOnSetEditorSleepProgressBarMax(AMaxValue: Integer);
begin
  if Assigned(FOnSetEditorSleepProgressBarMax) then
    FOnSetEditorSleepProgressBarMax(AMaxValue)
  else
    raise Exception.Create('FOnSetEditorSleepProgressBarMax is not assigned.');
end;


procedure TActionExecution.DoOnSetEditorSleepProgressBarPosition(APositionValue: Integer);
begin
  if Assigned(FOnSetEditorSleepProgressBarPosition) then
    FOnSetEditorSleepProgressBarPosition(APositionValue)
  else
    raise Exception.Create('FOnSetEditorSleepProgressBarPosition is not assigned.');
end;


procedure TActionExecution.DoOnSetEditorSleepInfo(AElapsedTime, ARemainingTime: string);
begin
  if Assigned(FOnSetEditorSleepInfo) then
    FOnSetEditorSleepInfo(AElapsedTime, ARemainingTime)
  else
    raise Exception.Create('FOnSetEditorSleepInfo is not assigned.');
end;


procedure TActionExecution.DoOnGetSelfHandles(AListOfSelfHandles: TStringList);
begin
  if Assigned(FOnGetSelfHandles) then
    FOnGetSelfHandles(AListOfSelfHandles)
  else
    raise Exception.Create('FOnGetSelfHandles is not assigned.');
end;


procedure TActionExecution.DoOnAddDefaultFontProfile(var AFindSubControlOptions: TClkFindSubControlOptions; var AActionOptions: TClkActionOptions);
begin
  if Assigned(FOnAddDefaultFontProfile) then
    FOnAddDefaultFontProfile(AFindSubControlOptions, AActionOptions)
  else
  begin     //this part is required when using ClickerActionExecution without UI
    SetLength(AFindSubControlOptions.MatchBitmapText, 1); //this is required, to execute the next for loop, without font profiles
                                                       //this code will have to be split in FindControl and FindSubControl. Then, this part will have to be deleted.
    AFindSubControlOptions.MatchBitmapText[0].FontName := 'Tahoma';
    AFindSubControlOptions.MatchBitmapText[0].FontSize := 8;
    AFindSubControlOptions.MatchBitmapText[0].ForegroundColor := '000000';
    AFindSubControlOptions.MatchBitmapText[0].BackgroundColor := '0000FF';
    AFindSubControlOptions.MatchBitmapText[0].ProfileName := CDefaultFontProfileName;
    frClickerActions.frClickerFindControl.AddNewFontProfile(AFindSubControlOptions.MatchBitmapText[0]);
  end;
end;


function TActionExecution.DoOnGetGridDrawingOption: TOnGetGridDrawingOption;
begin
  if not Assigned(FOnGetGridDrawingOption) then
    raise Exception.Create('OnGetGridDrawingOption not assigned.')
  else
    Result := FOnGetGridDrawingOption;
end;


procedure TActionExecution.DoOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
begin
  if not Assigned(FOnLoadPrimitivesFile) then
    raise Exception.Create('OnLoadPrimitivesFile not assigned.')
  else
    FOnLoadPrimitivesFile(AFileName, APrimitives, AOrders, ASettings);
end;


function TActionExecution.DoOnGetSetVarActionByName(var AClkSetVarOptions: TClkSetVarOptions; AActionName: string): Boolean;
begin
  if not Assigned(FOnGetSetVarActionByName) then
    raise Exception.Create('OnGetSetVarActionByName not assigned.')
  else
    Result := FOnGetSetVarActionByName(AClkSetVarOptions, AActionName);
end;


function TActionExecution.DoOnUpdateSetVarActionByName(AClkSetVarOptions: TClkSetVarOptions; AActionName: string): Boolean;
begin
  if not Assigned(FOnUpdateSetVarActionByName) then
    raise Exception.Create('OnUpdateSetVarActionByName not assigned.')
  else
    Result := FOnUpdateSetVarActionByName(AClkSetVarOptions, AActionName);
end;


function TActionExecution.DoOnTClkIniReadonlyFileCreate(AFileName: string): TClkIniReadonlyFile;
begin
  if not Assigned(FOnTClkIniReadonlyFileCreate) then
    raise Exception.Create('OnTClkIniReadonlyFileCreate not assigned.')
  else
    Result := FOnTClkIniReadonlyFileCreate(AFileName);
end;


procedure TActionExecution.DoOnSaveStringListToFile(AStringList: TStringList; const AFileName: string);
begin
  if not Assigned(FOnSaveStringListToFile) then
    raise Exception.Create('OnSaveStringListToFile not assigned.')
  else
    FOnSaveStringListToFile(AStringList, AFileName);
end;


procedure TActionExecution.DoOnBackupVars(AAllVars: TStringList);
begin
  if not Assigned(FOnBackupVars) then
    raise Exception.Create('OnBackupVars not assigned.')
  else
    FOnBackupVars(AAllVars);
end;


procedure TActionExecution.DoOnSaveFileToExtRenderingInMemFS(AFileName: string; AContent: Pointer; AFileSize: Int64);
begin
  if not Assigned(FOnSaveFileToExtRenderingInMemFS) then
    raise Exception.Create('OnSaveFileToExtRenderingInMemFS not assigned.')
  else
    FOnSaveFileToExtRenderingInMemFS(AFileName, AContent, AFileSize);
end;


//procedure TActionExecution.DoOnRestoreVars(AAllVars: TStringList);
//begin
//  if not Assigned(FOnRestoreVars) then
//    raise Exception.Create('OnRestoreVars not assigned.')
//  else
//    FOnRestoreVars(AAllVars);
//end;


function TActionExecution.DoOnGenerateAndSaveTreeWithWinInterp(AHandle: THandle; ATreeFileName: string; AStep: Integer; AUseMouseSwipe: Boolean): Boolean;
begin
  if not Assigned(FOnGenerateAndSaveTreeWithWinInterp) then
    raise Exception.Create('OnGenerateAndSaveTreeWithWinInterp not assigned.')
  else
    Result := FOnGenerateAndSaveTreeWithWinInterp(AHandle, ATreeFileName, AStep, AUseMouseSwipe);
end;


function TActionExecution.DoOnSetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue: string): Boolean;
begin
  if not Assigned(FOnSetWinInterpOption) then
    raise Exception.Create('OnSetWinInterpOption not assigned.')
  else
    Result := FOnSetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue);
end;


function TActionExecution.DoOnExecuteActionByName(AActionName: string): Boolean;
begin
  if not Assigned(FOnExecuteActionByName) then
    raise Exception.Create('OnExecuteActionByName not assigned.');

  Result := FOnExecuteActionByName(AActionName);
end;


function TActionExecution.DoOnGetAllActions: PClkActionsRecArr;
begin
  if not Assigned(FOnGetAllActions) then
    raise Exception.Create('OnGetAllActions not assigned.');

  Result := FOnGetAllActions();
end;


function TActionExecution.DoOnResolveTemplatePath(APath: string; ACustomSelfTemplateDir: string = ''; ACustomAppDir: string = ''): string;
begin
  if not Assigned(FOnResolveTemplatePath) then
    raise Exception.Create('OnResolveTemplatePath not assigned.');

  Result := FOnResolveTemplatePath(APath, ACustomSelfTemplateDir, ACustomAppDir);
end;


procedure TActionExecution.DoOnSetDebugPoint(ADebugPoint: string);
begin
  if not Assigned(FOnSetDebugPoint) then
    raise Exception.Create('OnSetDebugPoint not assigned.');

  FOnSetDebugPoint(ADebugPoint);
end;


function TActionExecution.DoOnIsAtBreakPoint(ADebugPoint: string): Boolean;
begin
  if not Assigned(FOnIsAtBreakPoint) then
    raise Exception.Create('OnIsAtBreakPoint not assigned.');

  Result := FOnIsAtBreakPoint(ADebugPoint);
end;


function TActionExecution.DoOnFileExists(const FileName: string): Boolean;
begin
  if not Assigned(FOnFileExists) then
    raise Exception.Create('OnFileExists not assigned.');

  Result := FOnFileExists(FileName);
end;


procedure TActionExecution.DoOnSaveTemplateToFile(AStringList: TStringList; const AFileName: string);
begin
  if not Assigned(FOnSaveTemplateToFile) then
    raise Exception.Create('OnSaveTemplateToFile is not assigned.')
  else
    FOnSaveTemplateToFile(AStringList, AFileName);
end;


function TActionExecution.DoOnExecuteActionByContent(var AAllActions: TClkActionsRecArr; AActionIndex: Integer): Boolean;
begin
  if not Assigned(FOnExecuteActionByContent) then
    raise Exception.Create('OnExecuteActionByContent not assigned.');

  Result := FOnExecuteActionByContent(AAllActions, AActionIndex);
end;


function TActionExecution.DoOnLoadTemplateToActions(Fnm: string; var AActions: TClkActionsRecArr; AWhichTemplate: TEditTemplateWhichTemplate; out ANotes, AIconPath: string; AWaitForFileAvailability: Boolean = False): string;
begin
  if not Assigned(FOnLoadTemplateToActions) then
    raise Exception.Create('OnLoadTemplateToActions is not assigned.')
  else
    Result := FOnLoadTemplateToActions(Fnm, AActions, AWhichTemplate, ANotes, AIconPath, AWaitForFileAvailability);
end;


function TActionExecution.DoOnSaveCompleteTemplateToFile(Fnm: string; var AActions: TClkActionsRecArr; AWhichTemplate: TEditTemplateWhichTemplate; ANotes, AIconPath: string; AUpdateUI, AShouldSaveSelfTemplate: Boolean): string;
begin
  if not Assigned(FOnSaveCompleteTemplateToFile) then
    raise Exception.Create('OnSaveCompleteTemplateToFile is not assigned.')
  else
    Result := FOnSaveCompleteTemplateToFile(Fnm, AActions, AWhichTemplate, ANotes, AIconPath, AUpdateUI, AShouldSaveSelfTemplate);
end;


procedure TActionExecution.DoOnWaitInDebuggingMode(var ADebuggingAction: TClkActionRec; AActionAllowsSteppingInto: TAllowsSteppingInto);
begin
  if not Assigned(FOnWaitInDebuggingMode) then
    raise Exception.Create('OnWaitInDebuggingMode is not assigned.')
  else
    FOnWaitInDebuggingMode(ADebuggingAction, AActionAllowsSteppingInto);
end;


function TActionExecution.DoOnGetPluginInMemFS: TInMemFileSystem;
begin
  if not Assigned(FOnGetPluginInMemFS) then
    raise Exception.Create('OnGetPluginInMemFS is not assigned.')
  else
    Result := FOnGetPluginInMemFS();
end;


function TActionExecution.DoOnGetListeningPort: Word;
begin
  if not Assigned(FOnGetListeningPort) then
    raise Exception.Create('OnGetListeningPort is not assigned.')
  else
    Result := FOnGetListeningPort();
end;


function TActionExecution.ResolveAllowedFileDirs(AAllowedFileDirsForServer: string): string;
var
  ListOfDirs: TStringList;
  i: Integer;
begin
  ListOfDirs := TStringList.Create;
  try
    ListOfDirs.LineBreak := #13#10;
    ListOfDirs.Text := AAllowedFileDirsForServer;

    Result := '';
    for i := 0 to ListOfDirs.Count - 1 do
      Result := Result + DoOnResolveTemplatePath(ListOfDirs.Strings[i]) + #13#10;
  finally
    ListOfDirs.Free;
  end;
end;


procedure TActionExecution.ExecuteClickAction(var AClickOptions: TClkClickOptions);
var
  MouseParams: TStringList;
  XClick, YClick: Integer;
  Control_Left, Control_Top, Control_Width, Control_Height: Integer;
  MXOffset, MYOffset: Integer;
  ShiftStateParam: string;
begin
  MouseParams := TStringList.Create;
  try
    MouseParams.LineBreak := #13#10;
    Control_Left := StrToIntDef(GetActionVarValue('$Control_Left$'), 0);
    Control_Top := StrToIntDef(GetActionVarValue('$Control_Top$'), 0);
    Control_Width := StrToIntDef(GetActionVarValue('$Control_Width$'), 0);
    Control_Height := StrToIntDef(GetActionVarValue('$Control_Height$'), 0);

    XClick := Control_Left; //global in screen
    YClick := Control_Top;

    MXOffset := StrToIntDef(EvaluateReplacements(AClickOptions.XOffset), 0);
    MYOffset := StrToIntDef(EvaluateReplacements(AClickOptions.YOffset), 0);

    case AClickOptions.XClickPointReference of
      xrefLeft: Inc(XClick, MXOffset);
      xrefRight: Inc(XClick, MXOffset + Control_Width);
      xrefWidth: Inc(XClick, MXOffset - Control_Width - Control_Left);    ///????????????????
      xrefVar: XClick := StrToIntDef(EvaluateReplacements(AClickOptions.XClickPointVar), 0) + MXOffset;
      xrefAbsolute: XClick := MXOffset;
    end;

    case AClickOptions.YClickPointReference of
      yrefTop: Inc(YClick, MYOffset);
      yrefBottom: Inc(YClick, MYOffset + Control_Height);
      yrefHeight: Inc(YClick, MYOffset - Control_Height - Control_Top);    ///????????????????
      yrefVar: YClick := StrToIntDef(EvaluateReplacements(AClickOptions.YClickPointVar), 0) + MYOffset;
      yrefAbsolute: YClick := MYOffset;
    end;

    MouseParams.Add(CMouseX + '=' + IntToStr(XClick));
    MouseParams.Add(CMouseY + '=' + IntToStr(YClick));

    case AClickOptions.MouseButton of
      mbLeft: MouseParams.Add(CMouseButton + '=' + CMouseButtonLeft);
      mbRight: MouseParams.Add(CMouseButton + '=' + CMouseButtonRight);
      mbMiddle: MouseParams.Add(CMouseButton + '=' + CMouseButtonMiddle);
      else
      begin
      end;
    end;

    ShiftStateParam := '';
    if AClickOptions.ClickWithCtrl then
      ShiftStateParam := ShiftStateParam + CShiftStateCtrl;

    if AClickOptions.ClickWithAlt then
      ShiftStateParam := ShiftStateParam + ',' + CShiftStateAlt;

    if AClickOptions.ClickWithShift then
      ShiftStateParam := ShiftStateParam + ',' + CShiftStateShift;

    if AClickOptions.ClickWithDoubleClick then
      ShiftStateParam := ShiftStateParam + ',' + CShiftStateDoubleClick;

    MouseParams.Add(CMouseShiftState + '=' + ShiftStateParam);

    MouseParams.Add(CMouseCursorLeaveMouse + '=' + IntToStr(Ord(AClickOptions.LeaveMouse)));
    MouseParams.Add(CMouseMoveWithoutClick + '=' + IntToStr(Ord(AClickOptions.MoveWithoutClick)));

    MouseParams.Add(CMouseClickType + '=' + IntToStr(AClickOptions.ClickType));

    MouseParams.Add(CMouseDelayAfterMovingToDestination + '=' + EvaluateReplacements(AClickOptions.DelayAfterMovingToDestination));
    MouseParams.Add(CMouseDelayAfterMouseDown + '=' + EvaluateReplacements(AClickOptions.DelayAfterMouseDown));
    MouseParams.Add(CMouseMoveDuration + '=' + EvaluateReplacements(AClickOptions.MoveDuration));
    MouseParams.Add(CMouseUseClipCursor + '=' + IntToStr(Ord(AClickOptions.UseClipCursor)));

    if AClickOptions.ClickType = CMouseClickType_Drag then  ///Dest
    begin
      XClick := Control_Left; //global in screen
      YClick := Control_Top;

      MXOffset := StrToIntDef(EvaluateReplacements(AClickOptions.XOffsetDest), 0);
      MYOffset := StrToIntDef(EvaluateReplacements(AClickOptions.YOffsetDest), 0);

      case AClickOptions.XClickPointReferenceDest of
        xrefLeft: Inc(XClick, MXOffset);
        xrefRight: Inc(XClick, MXOffset + Control_Width);
        xrefWidth: Inc(XClick, MXOffset - Control_Width - Control_Left);    ///????????????????
        xrefVar: XClick := StrToIntDef(EvaluateReplacements(AClickOptions.XClickPointVarDest), 0) + MXOffset;
        xrefAbsolute: XClick := MXOffset;
      end;

      case AClickOptions.YClickPointReferenceDest of
        yrefTop: Inc(YClick, MYOffset);
        yrefBottom: Inc(YClick, MYOffset + Control_Height);
        yrefHeight: Inc(YClick, MYOffset - Control_Height - Control_Top);    ///????????????????
        yrefVar: YClick := StrToIntDef(EvaluateReplacements(AClickOptions.YClickPointVarDest), 0) + MYOffset;
        yrefAbsolute: YClick := MYOffset;
      end;

      MouseParams.Add(CMouseXDest + '=' + IntToStr(XClick));
      MouseParams.Add(CMouseYDest + '=' + IntToStr(YClick));
    end; ///Dest

    if AClickOptions.ClickType = CMouseClickType_Wheel then
    begin
      case AClickOptions.MouseWheelType of
        mwtVert:
          MouseParams.Add(CMouseWheelType + '=' + CMouseWheelVertWheel);

        mwtHoriz:
          MouseParams.Add(CMouseWheelType + '=' + CMouseWheelHorizWheel);
      end;

      MouseParams.Add(CMouseWheelAmount + '=' + EvaluateReplacements(AClickOptions.MouseWheelAmount));
    end;

    case AClickOptions.ClickType of
      CMouseClickType_Click, CMouseClickType_Drag:
        ClickTControl(MouseParams);

      CMouseClickType_MouseDown:
        MouseDownTControl(MouseParams);

      CMouseClickType_MouseUp:
        MouseUpTControl(MouseParams);

      CMouseClickType_Wheel:
        MouseWheelTControl(MouseParams);
    end;
  finally
    MouseParams.Free;
  end;
end;


function TActionExecution.ExecuteMultiClickAction(var AClickOptions: TClkClickOptions): Boolean;
var
  i: Integer;
  StopAllActionsOnDemandAddr: PBoolean;
begin
  if FStopAllActionsOnDemandFromParent <> nil then
    StopAllActionsOnDemandAddr := FStopAllActionsOnDemandFromParent
  else
    StopAllActionsOnDemandAddr := FStopAllActionsOnDemand;

  Result := True;

  for i := 0 to AClickOptions.Count - 1 do
  begin
    ExecuteClickAction(AClickOptions);
    Application.ProcessMessages;  //When opening a pop-up menu or a MessageBox, belonging to UIClicker, any call to Application.ProcessMessages will block the application.
    Sleep(3);                     //If this Application.ProcessMessages is commented, then the next call is going to block.

    //memLogErr.Lines.Add('$Current_Mouse_Y$: ' + EvaluateReplacements('$Current_Mouse_Y$'));

    {$IFDEF Windows}
      if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) and (GetAsyncKeyState(VK_F2) < 0) then
    {$ELSE}
      if (GetKeyState(VK_CONTROL) < 0) and (GetKeyState(VK_SHIFT) < 0) and (GetKeyState(VK_F2) < 0) then
    {$ENDIF}
    begin
      if FStopAllActionsOnDemandFromParent <> nil then
        FStopAllActionsOnDemandFromParent^ := True;

      FStopAllActionsOnDemand^ := True;
      Exit;
    end;

    if StopAllActionsOnDemandAddr^ then
    begin
      Result := False;
      Break;
    end;
  end;
end;


function TActionExecution.ExecuteExecAppAction(var AExecAppOptions: TClkExecAppOptions; var AActionOptions: TClkActionOptions): Boolean;
var
  ACmd, ErrMsg: string;
  i: Integer;
  AllParams: TStringList;
  s, SelfAppDir, TemplateDir: string;

  {$IFnDEF FPC}
    hwnd: THandle;
    ShellExecuteRes: Cardinal;
    AParams: string;
  {$ELSE}
    AProcess: TProcess;
    ExeInput, ExeOutput: string;
    TempStringList: TStringList;
    tk: QWord;
    TempBuffer: array of Byte;
    MemStream: TMemoryStream;
    TimeoutForAppRun: Integer;
  {$ENDIF}
begin
  SelfAppDir := ExtractFileDir(ParamStr(0));
  ACmd := AExecAppOptions.PathToApp;

  TemplateDir := '';
  if FSelfTemplateFileName = nil then
    TemplateDir := 'FSelfTemplateFileName not set in ExecApp.'
  else
  begin
    TemplateDir := ExtractFileDir(FSelfTemplateFileName^);
    ACmd := StringReplace(ACmd, '$SelfTemplateDir$', TemplateDir, [rfReplaceAll]);
    ACmd := StringReplace(ACmd, '$TemplateDir$', FFullTemplatesDir^, [rfReplaceAll]);
  end;

  ACmd := StringReplace(ACmd, '$AppDir$', SelfAppDir, [rfReplaceAll]);
  ACmd := EvaluateReplacements(ACmd);  //this call has to stay here, because the $SelfTemplateDir$ replacement is ''

  Result := True;

  if AExecAppOptions.VerifyFileExistence then
    if not FileExists(ACmd) then      //A new property should control this commented section.
    begin
      Result := False;
      SetActionVarValue('$ExecAction_Err$', 'File not found: ' + ACmd);
      Exit;
    end;

  AllParams := TStringList.Create;
  try
    AllParams.LineBreak := #13#10;
    AllParams.Text := AExecAppOptions.ListOfParams;


    {$IFnDEF FPC}   //backwards compatibility with Delphi, where there is no TProcess.  Still, there is CreateProcess and WaitForSingleObject.
      AParams := '';

      for i := 0 to AllParams.Count - 1 do
      begin
        s := EvaluateReplacements(AllParams.Strings[i]);
        s := StringReplace(s, '$AppDir$', SelfAppDir, [rfReplaceAll]);
        AParams := AParams + '"' + s + '" ';
      end;

      if AParams > '' then
        Delete(AParams, Length(AParams), 1); //delete last ' '

      if AExecAppOptions.WaitForApp then
        hwnd := Handle
      else
        hwnd := 0;

      ShellExecuteRes := ShellExecute(hwnd, 'open', PChar(ACmd), PChar(AParams), PChar(ExtractFileDir(ACmd)), SW_SHOW);
      if ShellExecuteRes > 32 then
      begin
        Result := True;
        SetActionVarValue('$ExecAction_Err$', '');
      end
      else
      begin
        Result := False;
        SetActionVarValue('$ExecAction_Err$', 'ShellExecute error code: ' + IntToStr(ShellExecuteRes));
      end;
    {$ELSE}
      try
        AProcess := TProcess.Create(nil);
        MemStream := TMemoryStream.Create;
        try
          AProcess.Executable := ACmd;

          for i := 0 to AllParams.Count - 1 do
          begin
            s := AllParams.Strings[i];
            s := StringReplace(s, '$AppDir$', SelfAppDir, [rfReplaceAll]);
            s := StringReplace(s, '$SelfTemplateDir$', TemplateDir, [rfReplaceAll]);
            s := StringReplace(s, '$TemplateDir$', FFullTemplatesDir^, [rfReplaceAll]);
            s := EvaluateReplacements(s);
            AProcess.Parameters.Add(s);
          end;

          AProcess.Options := [poUsePipes, poStderrToOutPut{, poPassInput}];
          AProcess.StartupOptions := AProcess.StartupOptions + [suoUseShowWindow];

          if AExecAppOptions.NoConsole then
            AProcess.Options := AProcess.Options + [poNoConsole];

          case AExecAppOptions.UseInheritHandles of
            uihNo:
              AProcess.InheritHandles := False;

            uihYes:
              AProcess.InheritHandles := True;

            uihOnlyWithStdInOut:
               AProcess.InheritHandles := AExecAppOptions.WaitForApp or
                                         (AExecAppOptions.AppStdIn > '');
          end;  //when InheritHandles is True, the executed application may keep open a listening socket, after closing Clicker, preventing further Clicker processes to listen

          AProcess.ShowWindow := swoShow;

          AProcess.CurrentDirectory := EvaluateReplacements(AExecAppOptions.CurrentDir);
          if AProcess.CurrentDirectory = '' then
            AProcess.CurrentDirectory := ExtractFileDir(ACmd);

          //if AExecAppOptions.WaitForApp then
          //  AProcess.Options := AppProcess.Options + [poWaitOnExit];  //do not use poWaitOnExit, because it blocks the application

          AProcess.Execute;     //run here

          if AProcess.Input = nil then
          begin
            Result := False;
            SetActionVarValue('$ExecAction_Err$', 'Input stream is nil after exec.');
          end;

          ExeInput := FastReplace_45ToReturn(EvaluateReplacements(FastReplace_45ToReturn(AExecAppOptions.AppStdIn)));

          TempStringList := TStringList.Create;
          try
            TempStringList.LineBreak := #13#10;
            TempStringList.Text := ExeInput;
            TempStringList.SaveToStream(AProcess.Input);
          finally
            TempStringList.Free;
          end;

          TimeoutForAppRun := AActionOptions.ActionTimeout;
          if AExecAppOptions.WaitForApp then
          begin
            DoOnSetEditorEnabledState(False);

            try
              DoOnSetEditorTimeoutProgressBarMax(TimeoutForAppRun);
              tk := GetTickCount64;

              repeat
                Application.ProcessMessages;
                Sleep(100);

                if (AProcess.Output <> nil) and (AProcess.Output.NumBytesAvailable > 0) then
                begin
                  SetLength(TempBuffer, AProcess.Output.NumBytesAvailable);
                  if Length(TempBuffer) > 0 then
                  begin
                    AProcess.Output.Read(TempBuffer[0], Length(TempBuffer));
                    MemStream.Write(TempBuffer[0], Length(TempBuffer));
                  end;
                end;

                DoOnSetEditorTimeoutProgressBarPosition(GetTickCount64 - tk);

                if GetTickCount64 - tk >= TimeoutForAppRun then
                begin
                  if FSelfTemplateFileName = nil then
                    raise Exception.Create('FSelfTemplateFileName not set.');

                  if AExecAppOptions.LeaveRunningAfterTimeout then
                    Break;

                  PrependErrorMessageToActionVar('Timeout at "' + AActionOptions.ActionName + '" in ' + FSelfTemplateFileName^);
                  Result := False;
                  Break;
                end;

                if ((FStopAllActionsOnDemand <> nil) and FStopAllActionsOnDemand^) or
                   ((FStopAllActionsOnDemandFromParent <> nil) and FStopAllActionsOnDemandFromParent^) then
                begin
                  PrependErrorMessageToActionVar('App Execution manually stopped at "' + AActionOptions.ActionName + '" in ' + FSelfTemplateFileName^);
                  Result := False;
                  Break;
                end;
              until not AProcess.Active;

              if Result then
                SetActionVarValue('$ExecAction_Err$', '');
            finally
              DoOnSetEditorEnabledState(True);
            end;
          end;  //WaitForApp

          TempStringList := TStringList.Create;
          try
            TempStringList.LineBreak := #13#10;
            if (AProcess.Output <> nil) and (AProcess.Output.NumBytesAvailable > 0) then     //read once more (in case the timeout stopped the reading)
            begin
              SetLength(TempBuffer, AProcess.Output.NumBytesAvailable);
              AProcess.Output.Read(TempBuffer[0], Length(TempBuffer));
              MemStream.Write(TempBuffer[0], Length(TempBuffer));
            end;

            MemStream.Position := 0;
            TempStringList.LoadFromStream(MemStream);

            ExeOutput := TempStringList.Text;
          finally
            TempStringList.Free;
          end;
        finally
          AProcess.Free;
          MemStream.Free;
        end;

        SetActionVarValue('$ExecAction_StdOut$', FastReplace_ReturnTo45(ExeOutput));
      except
        on E: Exception do
        begin
          Result := False;
          if (Pos('Stream write error', E.Message) > 0) and
             (AExecAppOptions.UseInheritHandles = uihNo) then
            E.Message := E.Message + '  Make sure the UseInheritHandles option is set to "Yes" or "Only with StdIn / StdOut", when using StdIn or StdOut.';

          if FSelfTemplateFileName = nil then
            raise Exception.Create('FSelfTemplateFileName not set.');

          ErrMsg := StringReplace(SysErrorMessage(GetLastOSError), '%1', '"' + ACmd + '"', [rfReplaceAll]);
          SetActionVarValue('$ExecAction_Err$', 'Exception "' + E.Message + '" at "' + AActionOptions.ActionName + '" in ' + FSelfTemplateFileName^ + '   SysMsg: ' + ErrMsg);
        end;
      end;
    {$ENDIF}
  finally
    AllParams.Free;
  end;

  DoOnSetEditorTimeoutProgressBarPosition(0);
end;


function TActionExecution.FillInFindControlInputData(var AFindControlOptions: TClkFindControlOptions; const AActionOptions: TClkActionOptions; out FindControlInputData: TFindControlInputData; out FontProfilesCount: Integer): Boolean;
  procedure IgnoredColorsStrToArr(AIgnoredColorsStr: string; var AIgnoredColorsArr: TColorArr);
  var
    i: Integer;
    ColorsStr: string;
    ListOfIgnoredColors: TStringList;
  begin
    if AIgnoredColorsStr <> '' then
    begin
      ListOfIgnoredColors := TStringList.Create;
      try
        ListOfIgnoredColors.LineBreak := #13#10;
        ListOfIgnoredColors.Text := StringReplace(AIgnoredColorsStr, ',', #13#10, [rfReplaceAll]);

        SetLength(AIgnoredColorsArr, ListOfIgnoredColors.Count);
        for i := 0 to ListOfIgnoredColors.Count - 1 do
        begin
          ColorsStr := Trim(ListOfIgnoredColors.Strings[i]);    //a bit ugly to reuse the variable
          ColorsStr := EvaluateReplacements(ColorsStr);
          AIgnoredColorsArr[i] := HexToInt(ColorsStr);
        end;
      finally
        ListOfIgnoredColors.Free;
      end;
    end;
  end;

var
  TempEvalTextCount: Integer;
begin
  Result := True;

  FindControlInputData.MatchingMethods := [];
  if AFindControlOptions.MatchCriteria.WillMatchText then
    FindControlInputData.MatchingMethods := FindControlInputData.MatchingMethods + [mmText];

  if AFindControlOptions.MatchCriteria.WillMatchClassName then
    FindControlInputData.MatchingMethods := FindControlInputData.MatchingMethods + [mmClass];

  //if AFindControlOptions.MatchCriteria.WillMatchBitmapText then
  //  FindControlInputData.MatchingMethods := FindControlInputData.MatchingMethods + [mmBitmapText];
  //
  //if AFindControlOptions.MatchCriteria.WillMatchBitmapFiles then
  //  FindControlInputData.MatchingMethods := FindControlInputData.MatchingMethods + [mmBitmapFiles];
  //
  //if AFindControlOptions.MatchCriteria.WillMatchPrimitiveFiles then
  //  FindControlInputData.MatchingMethods := FindControlInputData.MatchingMethods + [mmPrimitiveFiles];

  FindControlInputData.SearchAsSubControl := False;
  FindControlInputData.DebugBitmap := nil;

  TempEvalTextCount := StrToIntDef(EvaluateReplacements(AFindControlOptions.EvaluateTextCount), -1);

  FindControlInputData.ClassName := EvaluateReplacements(AFindControlOptions.MatchClassName);
  FindControlInputData.Text := EvaluateReplacements(AFindControlOptions.MatchText, True, TempEvalTextCount);
  FindControlInputData.GetAllHandles := AFindControlOptions.GetAllControls;

  //if AFindControlOptions.MatchCriteria.WillMatchBitmapText then
  //  if FindControlInputData.Text = '' then
  //  begin
  //    SetActionVarValue('$ExecAction_Err$', 'The searched text is empty.');
  //    SetActionVarValue('$DebugVar_BitmapText$', 'Matching an empty string will lead to a false match.');
  //    Result := False;
  //    Exit;
  //  end;

  FindControlInputData.ClassNameSeparator := AFindControlOptions.MatchClassNameSeparator;
  FindControlInputData.TextSeparator := AFindControlOptions.MatchTextSeparator;
  FindControlInputData.DebugTemplateName := FSelfTemplateFileName^;

  //FontProfilesCount := Length(AFindControlOptions.MatchBitmapText);
  //if FontProfilesCount = 0 then
  //begin
  //  AddToLog('Adding default font profile to action: "' + AActionOptions.ActionName + '", of ' + CClkActionStr[AActionOptions.Action] + ' type.');
  //  DoOnAddDefaultFontProfile(AFindControlOptions, AActionOptions); //Currently, both FindControl and FindSubControl require a default font profile, because of the "for j" loop below. Once these two actions are split, only FindSubControl will require it.
  //  FontProfilesCount := Length(AFindControlOptions.MatchBitmapText);
  //end;

  FindControlInputData.StartSearchingWithCachedControl := AFindControlOptions.StartSearchingWithCachedControl;
  FindControlInputData.CachedControlLeft := StrToIntDef(EvaluateReplacements(AFindControlOptions.CachedControlLeft), 0);
  FindControlInputData.CachedControlTop := StrToIntDef(EvaluateReplacements(AFindControlOptions.CachedControlTop), 0);

  if AFindControlOptions.GetAllControls then
    //if not IsSubControl then
      SetActionVarValue('$AllControl_Handles$', '');

  /////////////////////////////Moved section    - because GlobalSearchArea has to stay stable between "for j" iterations
  if AFindControlOptions.UseWholeScreen then
  begin
    FindControlInputData.GlobalSearchArea.Left := 0;
    FindControlInputData.GlobalSearchArea.Top := 0;
    FindControlInputData.GlobalSearchArea.Right := Screen.Width;
    FindControlInputData.GlobalSearchArea.Bottom := Screen.Height;
  end
  else
  begin
    FindControlInputData.GlobalSearchArea.Left := StrToIntDef(EvaluateReplacements(AFindControlOptions.InitialRectangle.Left), 0);
    FindControlInputData.GlobalSearchArea.Top := StrToIntDef(EvaluateReplacements(AFindControlOptions.InitialRectangle.Top), 0);
    FindControlInputData.GlobalSearchArea.Right := StrToIntDef(EvaluateReplacements(AFindControlOptions.InitialRectangle.Right), 0);
    FindControlInputData.GlobalSearchArea.Bottom := StrToIntDef(EvaluateReplacements(AFindControlOptions.InitialRectangle.Bottom), 0);
  end;

  if not AFindControlOptions.WaitForControlToGoAway then  //Do not move this if statement, because GlobalSearchArea is modified below:
  begin
    AddToLog('Find Control with text = "' + FindControlInputData.Text + '"' +
             '    GetAllControls is set to ' + BoolToStr(AFindControlOptions.GetAllControls, True) +
             '    SearchMode: ' + CSearchForControlModeStr[AFindControlOptions.MatchCriteria.SearchForControlMode]);

    if FExtraLogging_FindControl then
    begin
      AddToLog('Raw GlobalSearchArea.Left = ' + IntToStr(FindControlInputData.GlobalSearchArea.Left));
      AddToLog('Raw GlobalSearchArea.Top = ' + IntToStr(FindControlInputData.GlobalSearchArea.Top));
      AddToLog('Raw GlobalSearchArea.Right = ' + IntToStr(FindControlInputData.GlobalSearchArea.Right));
      AddToLog('Raw GlobalSearchArea.Bottom = ' + IntToStr(FindControlInputData.GlobalSearchArea.Bottom));
    end;
  end;

  FindControlInputData.InitialRectangleOffsets.Left := StrToIntDef(EvaluateReplacements(AFindControlOptions.InitialRectangle.LeftOffset), 0);
  FindControlInputData.InitialRectangleOffsets.Top := StrToIntDef(EvaluateReplacements(AFindControlOptions.InitialRectangle.TopOffset), 0);
  FindControlInputData.InitialRectangleOffsets.Right := StrToIntDef(EvaluateReplacements(AFindControlOptions.InitialRectangle.RightOffset), 0);
  FindControlInputData.InitialRectangleOffsets.Bottom := StrToIntDef(EvaluateReplacements(AFindControlOptions.InitialRectangle.BottomOffset), 0);

  Inc(FindControlInputData.GlobalSearchArea.Left, FindControlInputData.InitialRectangleOffsets.Left);
  Inc(FindControlInputData.GlobalSearchArea.Top, FindControlInputData.InitialRectangleOffsets.Top);
  Inc(FindControlInputData.GlobalSearchArea.Right, FindControlInputData.InitialRectangleOffsets.Right);
  Inc(FindControlInputData.GlobalSearchArea.Bottom, FindControlInputData.InitialRectangleOffsets.Bottom);

  if FExtraLogging_FindControl and not AFindControlOptions.WaitForControlToGoAway then
  begin
    AddToLog('(With Offset) GlobalSearchArea.Left = ' + IntToStr(FindControlInputData.GlobalSearchArea.Left));
    AddToLog('(With Offset) GlobalSearchArea.Top = ' + IntToStr(FindControlInputData.GlobalSearchArea.Top));
    AddToLog('(With Offset) GlobalSearchArea.Right = ' + IntToStr(FindControlInputData.GlobalSearchArea.Right));
    AddToLog('(With Offset) GlobalSearchArea.Bottom = ' + IntToStr(FindControlInputData.GlobalSearchArea.Bottom));
  end;
  /////////////////////////////End of moved section

  if (FindControlInputData.GlobalSearchArea.Right - FindControlInputData.GlobalSearchArea.Left < 1) or
     (FindControlInputData.GlobalSearchArea.Bottom - FindControlInputData.GlobalSearchArea.Top < 1) then
  begin
    frClickerActions.imgDebugBmp.Picture.Bitmap.Width := 300;
    frClickerActions.imgDebugBmp.Picture.Bitmap.Height := 300;
    frClickerActions.imgDebugBmp.Canvas.Brush.Color := clWhite;
    frClickerActions.imgDebugBmp.Canvas.Pen.Color := clRed;
    frClickerActions.imgDebugBmp.Canvas.Font.Color := clRed;
    frClickerActions.imgDebugBmp.Canvas.TextOut(0, 0, 'Invalid search area:   ');
    frClickerActions.imgDebugBmp.Canvas.TextOut(0, 15, 'Rectangle width: ' + IntToStr(FindControlInputData.GlobalSearchArea.Right - FindControlInputData.GlobalSearchArea.Left) + '   ');
    frClickerActions.imgDebugBmp.Canvas.TextOut(0, 30, 'Rectangle height: ' + IntToStr(FindControlInputData.GlobalSearchArea.Bottom - FindControlInputData.GlobalSearchArea.Top) + '   ');
    frClickerActions.imgDebugBmp.Canvas.TextOut(0, 45, 'Please verify offsets.   ');

    frClickerActions.imgDebugBmp.Canvas.TextOut(0, 65, 'GlobalRectangle left (with offset): ' + IntToStr(FindControlInputData.GlobalSearchArea.Left) + '   ');
    frClickerActions.imgDebugBmp.Canvas.TextOut(0, 80, 'GlobalRectangle top (with offset): ' + IntToStr(FindControlInputData.GlobalSearchArea.Top) + '   ');
    frClickerActions.imgDebugBmp.Canvas.TextOut(0, 95, 'GlobalRectangle right (with offset): ' + IntToStr(FindControlInputData.GlobalSearchArea.Right) + '   ');
    frClickerActions.imgDebugBmp.Canvas.TextOut(0, 110, 'GlobalRectangle bottom (with offset): ' + IntToStr(FindControlInputData.GlobalSearchArea.Bottom) + '   ');

    frClickerActions.imgDebugBmp.Canvas.TextOut(0, 135, 'Left offset: ' + IntToStr(FindControlInputData.InitialRectangleOffsets.Left) + '   ');
    frClickerActions.imgDebugBmp.Canvas.TextOut(0, 150, 'Top offset: ' + IntToStr(FindControlInputData.InitialRectangleOffsets.Top) + '   ');
    frClickerActions.imgDebugBmp.Canvas.TextOut(0, 165, 'Right offset: ' + IntToStr(FindControlInputData.InitialRectangleOffsets.Right) + '   ');
    frClickerActions.imgDebugBmp.Canvas.TextOut(0, 180, 'Bottom offset: ' + IntToStr(FindControlInputData.InitialRectangleOffsets.Bottom) + '   ');

    frClickerActions.imgDebugBmp.Canvas.TextOut(0, 210, 'FileName: '+ ExtractFileName(FSelfTemplateFileName^));
    frClickerActions.imgDebugBmp.Canvas.TextOut(0, 225, 'Action: "' + AActionOptions.ActionName + '"');

    AddToLog('Exiting find control, because the search area is negative.');
    AddToLog('');

    Result := False;
    Exit;
  end;

  if FindControlInputData.MatchingMethods = [] then   //section moved from above (although useful there), to allow refactoring
  begin
    SetActionVarValue('$ExecAction_Err$', 'No match criteria set. Action: ' + AActionOptions.ActionName);
    AddToLog('No match criteria set.');
    Result := False;
  end;

  FindControlInputData.DebugBitmap := frClickerActions.imgDebugBmp.Picture.Bitmap;
  FindControlInputData.DebugGrid := frClickerActions.imgDebugGrid;
end;


function TActionExecution.FillInFindSubControlInputDataForGPU(var AFindSubControlOptions: TClkFindSubControlOptions; var FindControlInputData: TFindControlInputData): Boolean;
var
  OpenCLDll: TOpenCL;
  i, Error: Integer;
  PlatformCount, DeviceCount: cl_uint;
  PlatformIDs: ^cl_platform_id_arr;
  DeviceIDs: ^cl_device_id_arr;
  PlatformInfo, DeviceInfo: string;
  TargetPlatformStr, TargetDeviceStr: string;
  DevType: cl_device_type; //GPU
  UseAllKernelsEvent, UseEventsInEnqueueKernel, WaitForAllKernelsToBeDone: string;
begin
  Result := True;
  FindControlInputData.OpenCLPath := EvaluateReplacements(AFindSubControlOptions.GPUSettings.OpenCLPath);
  OpenCLDll := TOpenCL.Create;
  try
    if AFindSubControlOptions.GPUSettings.OpenCLPath <> '' then
      if OpenCLDll.ExpectedDllLocation <> FindControlInputData.OpenCLPath then
      begin
        OpenCLDll.ExpectedDllFileName := ExtractFileName(FindControlInputData.OpenCLPath);
        OpenCLDll.ExpectedDllDir := ExtractFileDir(FindControlInputData.OpenCLPath);
        OpenCLDll.LoadOpenCLLibrary;
      end;

    if not OpenCLDll.Loaded then
    begin
      AddToLog('OpenCL not available. The dll is expected to exist at ' + FindControlInputData.OpenCLPath);
      SetActionVarValue('$ExecAction_Err$', 'OpenCL not available at ' + FindControlInputData.OpenCLPath);
      Result := False;
      Exit;
    end;

    Error := OpenCLDll.clGetPlatformIDs(0, nil, @PlatformCount);
    if Error < CL_SUCCESS then
    begin
      SetActionVarValue('$ExecAction_Err$', 'Error getting GPU platforms count: ' + CLErrorToStr(Error));
      Result := False;
      Exit;
    end;

    GetMem(PlatformIDs, PlatformCount * SizeOf(cl_platform_id));
    try
      Error := OpenCLDll.clGetPlatformIDs(PlatformCount, Pcl_platform_id(PlatformIDs), nil);
      if Error < CL_SUCCESS then
      begin
        SetActionVarValue('$ExecAction_Err$', 'Error getting GPU platforms IDs: ' + CLErrorToStr(Error));
        Result := False;
        Exit;
      end;

      TargetPlatformStr := EvaluateReplacements(AFindSubControlOptions.GPUSettings.TargetPlatform);
      TargetDeviceStr := EvaluateReplacements(AFindSubControlOptions.GPUSettings.TargetDevice);
      FindControlInputData.GPUPlatformIndex := -1;
      FindControlInputData.GPUDeviceIndex := -1;
      FindControlInputData.GPUExecutionAvailability := AFindSubControlOptions.GPUSettings.ExecutionAvailability;

      PlatformInfo := '';
      case AFindSubControlOptions.GPUSettings.TargetPlatformIDType of
        tpitIndex:
          FindControlInputData.GPUPlatformIndex := StrToIntDef(TargetPlatformStr, 0);

        tpitFullNameMatchCase:
        begin
          for i := 0 to PlatformCount - 1 do
          begin
            PlatformInfo := GetGPUPlatformInfo(AddToLog, OpenCLDll, PlatformIDs[i], CL_PLATFORM_NAME);
            if TargetPlatformStr = PlatformInfo then
            begin
              FindControlInputData.GPUPlatformIndex := i;
              Break;
            end;
          end;
        end;

        tpitFullNameNoCase:
        begin
          TargetPlatformStr := UpperCase(TargetPlatformStr);
          for i := 0 to PlatformCount - 1 do
          begin
            PlatformInfo := GetGPUPlatformInfo(AddToLog, OpenCLDll, PlatformIDs[i], CL_PLATFORM_NAME);
            if TargetPlatformStr = UpperCase(PlatformInfo) then
            begin
              FindControlInputData.GPUPlatformIndex := i;
              Break;
            end;
          end;
        end;

        tpitPartialNameMatchCase:
        begin
          for i := 0 to PlatformCount - 1 do
          begin
            PlatformInfo := GetGPUPlatformInfo(AddToLog, OpenCLDll, PlatformIDs[i], CL_PLATFORM_NAME);
            if Pos(TargetPlatformStr, PlatformInfo) > 0 then
            begin
              FindControlInputData.GPUPlatformIndex := i;
              Break;
            end;
          end;
        end;

        tpitPartialNameNoCase:
        begin
          TargetPlatformStr := UpperCase(TargetPlatformStr);
          for i := 0 to PlatformCount - 1 do
          begin
            PlatformInfo := GetGPUPlatformInfo(AddToLog, OpenCLDll, PlatformIDs[i], CL_PLATFORM_NAME);
            if Pos(TargetPlatformStr, UpperCase(PlatformInfo)) > 0 then
            begin
              FindControlInputData.GPUPlatformIndex := i;
              Break;
            end;
          end;
        end;
      end; //case

      if (FindControlInputData.GPUPlatformIndex < 0) or (FindControlInputData.GPUPlatformIndex > PlatformCount - 1) then
      begin
        SetActionVarValue('$ExecAction_Err$', 'GPU Platform out of range: ' + IntToStr(FindControlInputData.GPUPlatformIndex) + ' / [0..' + IntToStr(PlatformCount - 1) + ']');
        Result := False;
        Exit;
      end;

      DevType := CL_DEVICE_TYPE_GPU;
      Error := OpenCLDll.clGetDeviceIDs(PlatformIDs[FindControlInputData.GPUPlatformIndex], DevType, 0, nil, @DeviceCount);
      if Error < CL_SUCCESS then
      begin
        SetActionVarValue('$ExecAction_Err$', 'Error getting GPU devices count: ' + CLErrorToStr(Error));
        Result := False;
        Exit;
      end;

      GetMem(DeviceIDs, DeviceCount * SizeOf(cl_device_id));
      try
        Error := OpenCLDll.clGetDeviceIDs(PlatformIDs[FindControlInputData.GPUPlatformIndex], DevType, DeviceCount, Pcl_device_id(DeviceIDs), nil);
        if Error < CL_SUCCESS then
        begin
          SetActionVarValue('$ExecAction_Err$', 'Error getting GPU device IDs: ' + CLErrorToStr(Error));
          Result := False;
          Exit;
        end;

        case AFindSubControlOptions.GPUSettings.TargetDeviceIDType of
          tditIndex:
            FindControlInputData.GPUDeviceIndex := StrToIntDef(TargetDeviceStr, 0);

          tditFullNameMatchCase:
          begin
            for i := 0 to DeviceCount - 1 do
            begin
              DeviceInfo := GetGPUDeviceInfo(AddToLog, OpenCLDll, DeviceIDs[i], CL_DEVICE_NAME);
              if TargetDeviceStr = DeviceInfo then
              begin
                FindControlInputData.GPUDeviceIndex := i;
                Break;
              end;
            end;
          end;

          tditFullNameNoCase:
          begin
            TargetDeviceStr := UpperCase(TargetDeviceStr);
            for i := 0 to DeviceCount - 1 do
            begin
              DeviceInfo := GetGPUDeviceInfo(AddToLog, OpenCLDll, DeviceIDs[i], CL_DEVICE_NAME);
              if TargetDeviceStr = UpperCase(DeviceInfo) then
              begin
                FindControlInputData.GPUDeviceIndex := i;
                Break;
              end;
            end;
          end;

          tditPartialNameMatchCase:
          begin
            for i := 0 to DeviceCount - 1 do
            begin
              DeviceInfo := GetGPUDeviceInfo(AddToLog, OpenCLDll, DeviceIDs[i], CL_DEVICE_NAME);
              if Pos(TargetDeviceStr, DeviceInfo) > 0 then
              begin
                FindControlInputData.GPUDeviceIndex := i;
                Break;
              end;
            end;
          end;

          tditPartialNameNoCase:
          begin
            TargetDeviceStr := UpperCase(TargetDeviceStr);
            for i := 0 to DeviceCount - 1 do
            begin
              DeviceInfo := GetGPUDeviceInfo(AddToLog, OpenCLDll, DeviceIDs[i], CL_DEVICE_NAME);
              if Pos(TargetDeviceStr, UpperCase(DeviceInfo)) > 0 then
              begin
                FindControlInputData.GPUDeviceIndex := i;
                Break;
              end;
            end;
          end;
        end; //case

        if (FindControlInputData.GPUDeviceIndex < 0) or (FindControlInputData.GPUDeviceIndex > DeviceCount - 1) then
        begin
          SetActionVarValue('$ExecAction_Err$', 'GPU Device out of range: ' + IntToStr(FindControlInputData.GPUDeviceIndex) + ' / [0..' + IntToStr(DeviceCount - 1) + ']');
          Result := False;
          Exit;
        end;
      finally
        Freemem(DeviceIDs, DeviceCount * SizeOf(cl_device_id));
      end;
    finally
      Freemem(PlatformIDs, PlatformCount * SizeOf(cl_platform_id));
    end;
  finally
    OpenCLDll.Free;
  end;

  UseAllKernelsEvent := EvaluateReplacements(CGPUDbgVar_GPUUseAllKernelsEvent);
  UseEventsInEnqueueKernel := EvaluateReplacements(CGPUDbgVar_GPUUseEventsInEnqueueKernel);
  WaitForAllKernelsToBeDone := EvaluateReplacements(CGPUDbgVar_GPUWaitForAllKernelsToBeDone);

  FindControlInputData.GPUIncludeDashG := EvaluateReplacements(CGPUDbgVar_GPUIncludeDashG) = 'True';
  FindControlInputData.GPUSlaveQueueFromDevice := EvaluateReplacements(CGPUDbgVar_GPUSlaveQueueFromDevice) = 'True';
  FindControlInputData.GPUUseAllKernelsEvent := (UseAllKernelsEvent = CGPUDbgVar_GPUUseAllKernelsEvent) or (UseAllKernelsEvent = 'True');
  FindControlInputData.GPUNdrangeNoLocalParam := EvaluateReplacements(CGPUDbgVar_GPUNdrangeNoLocalParam) = 'True';
  FindControlInputData.GPUUseEventsInEnqueueKernel := {(UseEventsInEnqueueKernel = CGPUDbgVar_GPUUseEventsInEnqueueKernel) or} (UseEventsInEnqueueKernel = 'True');
  FindControlInputData.GPUWaitForAllKernelsToBeDone := (WaitForAllKernelsToBeDone = CGPUDbgVar_GPUWaitForAllKernelsToBeDone) or (WaitForAllKernelsToBeDone = 'True');
  FindControlInputData.GPUReleaseFinalEventAtKernelEnd := EvaluateReplacements(CGPUDbgVar_GPUReleaseFinalEventAtKernelEnd) = 'True';
  FindControlInputData.GPUIgnoreExecutionAvailability := EvaluateReplacements(CGPUDbgVar_GPUIgnoreExecutionAvailability) = 'True';
end;


function TActionExecution.FillInFindSubControlInputData(var AFindSubControlOptions: TClkFindSubControlOptions; var AActionOptions: TClkActionOptions; out FindControlInputData: TFindControlInputData; out FontProfilesCount: Integer): Boolean;
  procedure IgnoredColorsStrToArr(AIgnoredColorsStr: string; var AIgnoredColorsArr: TColorArr);
  var
    i: Integer;
    ColorsStr: string;
    ListOfIgnoredColors: TStringList;
  begin
    if AIgnoredColorsStr <> '' then
    begin
      ListOfIgnoredColors := TStringList.Create;
      try
        ListOfIgnoredColors.LineBreak := #13#10;
        ListOfIgnoredColors.Text := StringReplace(AIgnoredColorsStr, ',', #13#10, [rfReplaceAll]);

        SetLength(AIgnoredColorsArr, ListOfIgnoredColors.Count);
        for i := 0 to ListOfIgnoredColors.Count - 1 do
        begin
          ColorsStr := Trim(ListOfIgnoredColors.Strings[i]);    //a bit ugly to reuse the variable
          ColorsStr := EvaluateReplacements(ColorsStr);
          AIgnoredColorsArr[i] := HexToInt(ColorsStr);
        end;
      finally
        ListOfIgnoredColors.Free;
      end;
    end;
  end;

var
  LocalFormatSettings: TFormatSettings;
  TempEvalTextCount: Integer;
begin
  Result := True;

  FindControlInputData.MatchingMethods := [];
  //if AFindSubControlOptions.MatchCriteria.WillMatchText then
  //  FindControlInputData.MatchingMethods := FindControlInputData.MatchingMethods + [mmText];
  //
  //if AFindSubControlOptions.MatchCriteria.WillMatchClassName then
  //  FindControlInputData.MatchingMethods := FindControlInputData.MatchingMethods + [mmClass];

  if AFindSubControlOptions.MatchCriteria.WillMatchBitmapText then
    FindControlInputData.MatchingMethods := FindControlInputData.MatchingMethods + [mmBitmapText];

  if AFindSubControlOptions.MatchCriteria.WillMatchBitmapFiles then
    FindControlInputData.MatchingMethods := FindControlInputData.MatchingMethods + [mmBitmapFiles];

  if AFindSubControlOptions.MatchCriteria.WillMatchPrimitiveFiles then
    FindControlInputData.MatchingMethods := FindControlInputData.MatchingMethods + [mmPrimitiveFiles];

  FindControlInputData.SearchAsSubControl := True;
  FindControlInputData.DebugBitmap := nil;

  TempEvalTextCount := StrToIntDef(EvaluateReplacements(AFindSubControlOptions.EvaluateTextCount), -1);

  FindControlInputData.Text := EvaluateReplacements(AFindSubControlOptions.MatchText, True, TempEvalTextCount);
  FindControlInputData.GetAllHandles := AFindSubControlOptions.GetAllControls;

  if AFindSubControlOptions.MatchCriteria.WillMatchBitmapText then
    if FindControlInputData.Text = '' then
    begin
      SetActionVarValue('$ExecAction_Err$', 'The searched text is empty.');
      SetActionVarValue('$DebugVar_BitmapText$', 'Matching an empty string will lead to a false match.');
      Result := False;
      Exit;
    end;

  FindControlInputData.ColorError := StrToIntDef(EvaluateReplacements(AFindSubControlOptions.ColorError), 0);
  FindControlInputData.AllowedColorErrorCount := StrToIntDef(EvaluateReplacements(AFindSubControlOptions.AllowedColorErrorCount), 0);
  FindControlInputData.DebugTemplateName := FSelfTemplateFileName^;

  FontProfilesCount := Length(AFindSubControlOptions.MatchBitmapText);
  if FontProfilesCount = 0 then
  begin
    AddToLog('Adding default font profile to action: "' + AActionOptions.ActionName + '", of ' + CClkActionStr[AActionOptions.Action] + ' type.');
    DoOnAddDefaultFontProfile(AFindSubControlOptions, AActionOptions); //Currently, both FindControl and FindSubControl require a default font profile, because of the "for j" loop below. Once these two actions are split, only FindSubControl will require it.
    FontProfilesCount := Length(AFindSubControlOptions.MatchBitmapText);
  end;

  FindControlInputData.StartSearchingWithCachedControl := AFindSubControlOptions.StartSearchingWithCachedControl;
  FindControlInputData.CachedControlLeft := StrToIntDef(EvaluateReplacements(AFindSubControlOptions.CachedControlLeft), 0);
  FindControlInputData.CachedControlTop := StrToIntDef(EvaluateReplacements(AFindSubControlOptions.CachedControlTop), 0);

  //if AFindSubControlOptions.GetAllControls then
  //  if not IsSubControl then
  //    SetActionVarValue('$AllControl_Handles$', '');

  FindControlInputData.UseFastSearch := AFindSubControlOptions.UseFastSearch;
  if FindControlInputData.UseFastSearch then
    FindControlInputData.FastSearchAllowedColorErrorCount := StrToIntDef(EvaluateReplacements(AFindSubControlOptions.FastSearchAllowedColorErrorCount), -1)
  else
    FindControlInputData.FastSearchAllowedColorErrorCount := 0; //not used anyway

  //if IsSubControl then
    AddToLog('Searching with:  ColorError = ' + IntToStr(FindControlInputData.ColorError) +
             '   AllowedColorErrorCount = ' + IntToStr(FindControlInputData.AllowedColorErrorCount) +
             '   FastSearchAllowedColorErrorCount = ' + IntToStr(FindControlInputData.FastSearchAllowedColorErrorCount) +
             '   PrecisionTimeout = ' + BoolToStr(AFindSubControlOptions.PrecisionTimeout, 'True', 'False'));

  IgnoredColorsStrToArr(AFindSubControlOptions.IgnoredColors, FindControlInputData.IgnoredColorsArr);
  if Length(FindControlInputData.IgnoredColorsArr) > 0 then
    AddToLog('Ignoring colors: ' + AFindSubControlOptions.IgnoredColors);

  FindControlInputData.SleepySearch := 2; //this allows a call to AppProcMsg, but does not use Sleep.
  if AFindSubControlOptions.SleepySearch then
    FindControlInputData.SleepySearch := FindControlInputData.SleepySearch or 1;  //Bit 0 is SleepySearch. Bit 1 is AppProcMsg.

  FindControlInputData.StopSearchOnMismatch := AFindSubControlOptions.StopSearchOnMismatch;

  /////////////////////////////Moved section    - because GlobalSearchArea has to stay stable between "for j" iterations
  if AFindSubControlOptions.UseWholeScreen then
  begin
    FindControlInputData.GlobalSearchArea.Left := 0;
    FindControlInputData.GlobalSearchArea.Top := 0;
    FindControlInputData.GlobalSearchArea.Right := Screen.Width;
    FindControlInputData.GlobalSearchArea.Bottom := Screen.Height;
  end
  else
  begin
    FindControlInputData.GlobalSearchArea.Left := StrToIntDef(EvaluateReplacements(AFindSubControlOptions.InitialRectangle.Left), 0);
    FindControlInputData.GlobalSearchArea.Top := StrToIntDef(EvaluateReplacements(AFindSubControlOptions.InitialRectangle.Top), 0);
    FindControlInputData.GlobalSearchArea.Right := StrToIntDef(EvaluateReplacements(AFindSubControlOptions.InitialRectangle.Right), 0);
    FindControlInputData.GlobalSearchArea.Bottom := StrToIntDef(EvaluateReplacements(AFindSubControlOptions.InitialRectangle.Bottom), 0);
  end;

  if not AFindSubControlOptions.WaitForControlToGoAway then  //Do not move this if statement, because GlobalSearchArea is modified below:
  begin
    AddToLog('Find SubControl with text = "' + FindControlInputData.Text + '"' +
             '    GetAllControls is set to ' + BoolToStr(AFindSubControlOptions.GetAllControls, True) {+
             '    SearchMode: ' + CSearchForControlModeStr[AFindSubControlOptions.MatchCriteria.SearchForControlMode]});

    if FExtraLogging_FindControl then
    begin
      AddToLog('Raw GlobalSearchArea.Left = ' + IntToStr(FindControlInputData.GlobalSearchArea.Left));
      AddToLog('Raw GlobalSearchArea.Top = ' + IntToStr(FindControlInputData.GlobalSearchArea.Top));
      AddToLog('Raw GlobalSearchArea.Right = ' + IntToStr(FindControlInputData.GlobalSearchArea.Right));
      AddToLog('Raw GlobalSearchArea.Bottom = ' + IntToStr(FindControlInputData.GlobalSearchArea.Bottom));
    end;
  end;

  FindControlInputData.InitialRectangleOffsets.Left := StrToIntDef(EvaluateReplacements(AFindSubControlOptions.InitialRectangle.LeftOffset), 0);
  FindControlInputData.InitialRectangleOffsets.Top := StrToIntDef(EvaluateReplacements(AFindSubControlOptions.InitialRectangle.TopOffset), 0);
  FindControlInputData.InitialRectangleOffsets.Right := StrToIntDef(EvaluateReplacements(AFindSubControlOptions.InitialRectangle.RightOffset), 0);
  FindControlInputData.InitialRectangleOffsets.Bottom := StrToIntDef(EvaluateReplacements(AFindSubControlOptions.InitialRectangle.BottomOffset), 0);

  Inc(FindControlInputData.GlobalSearchArea.Left, FindControlInputData.InitialRectangleOffsets.Left);
  Inc(FindControlInputData.GlobalSearchArea.Top, FindControlInputData.InitialRectangleOffsets.Top);
  Inc(FindControlInputData.GlobalSearchArea.Right, FindControlInputData.InitialRectangleOffsets.Right);
  Inc(FindControlInputData.GlobalSearchArea.Bottom, FindControlInputData.InitialRectangleOffsets.Bottom);

  if FExtraLogging_FindControl and not AFindSubControlOptions.WaitForControlToGoAway then
  begin
    AddToLog('(With Offset) GlobalSearchArea.Left = ' + IntToStr(FindControlInputData.GlobalSearchArea.Left));
    AddToLog('(With Offset) GlobalSearchArea.Top = ' + IntToStr(FindControlInputData.GlobalSearchArea.Top));
    AddToLog('(With Offset) GlobalSearchArea.Right = ' + IntToStr(FindControlInputData.GlobalSearchArea.Right));
    AddToLog('(With Offset) GlobalSearchArea.Bottom = ' + IntToStr(FindControlInputData.GlobalSearchArea.Bottom));
  end;
  /////////////////////////////End of moved section

  if AFindSubControlOptions.MatchBitmapAlgorithm <> mbaRenderTextOnly then
    if (FindControlInputData.GlobalSearchArea.Right - FindControlInputData.GlobalSearchArea.Left < 1) or
       (FindControlInputData.GlobalSearchArea.Bottom - FindControlInputData.GlobalSearchArea.Top < 1) then
    begin
      frClickerActions.imgDebugBmp.Picture.Bitmap.Width := 300;
      frClickerActions.imgDebugBmp.Picture.Bitmap.Height := 300;
      frClickerActions.imgDebugBmp.Canvas.Brush.Color := clWhite;
      frClickerActions.imgDebugBmp.Canvas.Pen.Color := clRed;
      frClickerActions.imgDebugBmp.Canvas.Font.Color := clRed;
      frClickerActions.imgDebugBmp.Canvas.TextOut(0, 0, 'Invalid search area:   ');
      frClickerActions.imgDebugBmp.Canvas.TextOut(0, 15, 'Rectangle width: ' + IntToStr(FindControlInputData.GlobalSearchArea.Right - FindControlInputData.GlobalSearchArea.Left) + '   ');
      frClickerActions.imgDebugBmp.Canvas.TextOut(0, 30, 'Rectangle height: ' + IntToStr(FindControlInputData.GlobalSearchArea.Bottom - FindControlInputData.GlobalSearchArea.Top) + '   ');
      frClickerActions.imgDebugBmp.Canvas.TextOut(0, 45, 'Please verify offsets.   ');

      frClickerActions.imgDebugBmp.Canvas.TextOut(0, 65, 'GlobalRectangle left (with offset): ' + IntToStr(FindControlInputData.GlobalSearchArea.Left) + '   ');
      frClickerActions.imgDebugBmp.Canvas.TextOut(0, 80, 'GlobalRectangle top (with offset): ' + IntToStr(FindControlInputData.GlobalSearchArea.Top) + '   ');
      frClickerActions.imgDebugBmp.Canvas.TextOut(0, 95, 'GlobalRectangle right (with offset): ' + IntToStr(FindControlInputData.GlobalSearchArea.Right) + '   ');
      frClickerActions.imgDebugBmp.Canvas.TextOut(0, 110, 'GlobalRectangle bottom (with offset): ' + IntToStr(FindControlInputData.GlobalSearchArea.Bottom) + '   ');

      frClickerActions.imgDebugBmp.Canvas.TextOut(0, 135, 'Left offset: ' + IntToStr(FindControlInputData.InitialRectangleOffsets.Left) + '   ');
      frClickerActions.imgDebugBmp.Canvas.TextOut(0, 150, 'Top offset: ' + IntToStr(FindControlInputData.InitialRectangleOffsets.Top) + '   ');
      frClickerActions.imgDebugBmp.Canvas.TextOut(0, 165, 'Right offset: ' + IntToStr(FindControlInputData.InitialRectangleOffsets.Right) + '   ');
      frClickerActions.imgDebugBmp.Canvas.TextOut(0, 180, 'Bottom offset: ' + IntToStr(FindControlInputData.InitialRectangleOffsets.Bottom) + '   ');

      frClickerActions.imgDebugBmp.Canvas.TextOut(0, 210, 'FileName: '+ ExtractFileName(FSelfTemplateFileName^));
      frClickerActions.imgDebugBmp.Canvas.TextOut(0, 225, 'Action: "' + AActionOptions.ActionName + '"');

      AddToLog('Exiting find control, because the search area is negative.');
      AddToLog('');

      Result := False;
      Exit;
    end;

  if FindControlInputData.MatchingMethods = [] then   //section moved from above (although useful there), to allow refactoring
  begin
    SetActionVarValue('$ExecAction_Err$', 'No match criteria set. Action: ' + AActionOptions.ActionName);
    AddToLog('No match criteria set.');
    Result := False;
  end;

  FindControlInputData.DebugBitmap := frClickerActions.imgDebugBmp.Picture.Bitmap;
  FindControlInputData.DebugGrid := frClickerActions.imgDebugGrid;

  FindControlInputData.ImageSource := AFindSubControlOptions.ImageSource;
  FindControlInputData.FullBackgroundImageInResult := AFindSubControlOptions.FullBackgroundImageInResult;

  LocalFormatSettings := FormatSettings;
  LocalFormatSettings.DecimalSeparator := '.';  //ensure consistency between templates
  FindControlInputData.MatchByHistogramNumericSettings.MinPercentColorMatch := StrToFloatDef(EvaluateReplacements(AFindSubControlOptions.MatchByHistogramSettings.MinPercentColorMatch), 50, LocalFormatSettings);
  FindControlInputData.MatchByHistogramNumericSettings.MostSignificantColorCountInSubBmp := StrToIntDef(EvaluateReplacements(AFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInSubBmp), 10);
  FindControlInputData.MatchByHistogramNumericSettings.MostSignificantColorCountInBackgroundBmp := StrToIntDef(EvaluateReplacements(AFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp), 15);

  FindControlInputData.CropFromScreenshot := AFindSubControlOptions.CropFromScreenshot;
  FindControlInputData.ThreadCount := StrToIntDef(EvaluateReplacements(AFindSubControlOptions.ThreadCount), 0);

  if AFindSubControlOptions.MatchBitmapAlgorithm = mbaBruteForceOnGPU then
    Result := FillInFindSubControlInputDataForGPU(AFindSubControlOptions, FindControlInputData);
end;


procedure TActionExecution.UpdateActionVarValuesFromControl(AControl: TCompRec; AUpdate_ResultedErrorCount: Boolean = False);
var
  Control_Width, Control_Height: Integer;
begin
  Control_Width := AControl.ComponentRectangle.Right - AControl.ComponentRectangle.Left;
  Control_Height := AControl.ComponentRectangle.Bottom - AControl.ComponentRectangle.Top;

  SetActionVarValue('$Control_Text$', AControl.Text);
  SetActionVarValue('$Control_Left$', IntToStr(AControl.ComponentRectangle.Left));
  SetActionVarValue('$Control_Top$', IntToStr(AControl.ComponentRectangle.Top));
  SetActionVarValue('$Control_Right$', IntToStr(AControl.ComponentRectangle.Right));
  SetActionVarValue('$Control_Bottom$', IntToStr(AControl.ComponentRectangle.Bottom));
  SetActionVarValue('$Control_Width$', IntToStr(Control_Width));
  SetActionVarValue('$Control_Height$', IntToStr(Control_Height));
  SetActionVarValue('$Half_Control_Width$', IntToStr(Control_Width shr 1));
  SetActionVarValue('$Half_Control_Height$', IntToStr(Control_Height shr 1));

  SetActionVarValue('$Control_Class$', AControl.ClassName);
  SetActionVarValue('$Control_Handle$', IntToStr(AControl.Handle));
  SetActionVarValue('$DebugVar_SubCnvXOffset$', IntToStr(AControl.XOffsetFromParent));
  SetActionVarValue('$DebugVar_SubCnvYOffset$', IntToStr(AControl.YOffsetFromParent));

  if AUpdate_ResultedErrorCount then
    SetActionVarValue('$ResultedErrorCount$', IntToStr(AControl.ResultedErrorCount));
end;


procedure TActionExecution.UpdateActionVarValuesFromResultedControlArr(var AResultedControlArr: TCompRecArr);
var
  Control_Width, Control_Height: Integer;
  AllControl_Lefts_Str: string;
  AllControl_Tops_Str: string;
  AllControl_Rights_Str: string;
  AllControl_Bottoms_Str: string;
  AllControl_Widths_Str: string;
  AllControl_Heights_Str: string;
  AllHalf_Control_Widths_Str: string;
  AllHalf_Control_Heights_Str: string;
  i: Integer;
begin
  AllControl_Lefts_Str := '';
  AllControl_Tops_Str := '';
  AllControl_Rights_Str := '';
  AllControl_Bottoms_Str := '';
  AllControl_Widths_Str := '';
  AllControl_Heights_Str := '';
  AllHalf_Control_Widths_Str := '';
  AllHalf_Control_Heights_Str := '';

  for i := 0 to Length(AResultedControlArr) - 1 do
  begin
    Control_Width := AResultedControlArr[i].ComponentRectangle.Right - AResultedControlArr[i].ComponentRectangle.Left;
    Control_Height := AResultedControlArr[i].ComponentRectangle.Bottom - AResultedControlArr[i].ComponentRectangle.Top;

    AllControl_Lefts_Str := AllControl_Lefts_Str + IntToStr(AResultedControlArr[i].ComponentRectangle.Left) + #4#5;
    AllControl_Tops_Str := AllControl_Tops_Str + IntToStr(AResultedControlArr[i].ComponentRectangle.Top) + #4#5;
    AllControl_Rights_Str := AllControl_Rights_Str + IntToStr(AResultedControlArr[i].ComponentRectangle.Right) + #4#5;
    AllControl_Bottoms_Str := AllControl_Bottoms_Str + IntToStr(AResultedControlArr[i].ComponentRectangle.Bottom) + #4#5;
    AllControl_Widths_Str := AllControl_Widths_Str + IntToStr(Control_Width) + #4#5;
    AllControl_Heights_Str := AllControl_Heights_Str + IntToStr(Control_Height) + #4#5;
    AllHalf_Control_Widths_Str := AllHalf_Control_Widths_Str + IntToStr(Control_Width shr 1) + #4#5;
    AllHalf_Control_Heights_Str := AllHalf_Control_Heights_Str + IntToStr(Control_Height shr 1) + #4#5;
  end;

  SetActionVarValue('$AllControl_Lefts$', AllControl_Lefts_Str);
  SetActionVarValue('$AllControl_Tops$', AllControl_Tops_Str);
  SetActionVarValue('$AllControl_Rights$', AllControl_Rights_Str);
  SetActionVarValue('$AllControl_Bottoms$', AllControl_Bottoms_Str);
  SetActionVarValue('$AllControl_Widths$', AllControl_Widths_Str);
  SetActionVarValue('$AllControl_Heights$', AllControl_Heights_Str);
  SetActionVarValue('$AllHalf_Control_Widths$', AllHalf_Control_Widths_Str);
  SetActionVarValue('$AllHalf_Control_Heights$', AllHalf_Control_Heights_Str);
end;


procedure TActionExecution.SetDbgImgPos(var AFindSubControlOptions: TClkFindSubControlOptions; AFindControlInputData: TFindControlInputData; AResultedControl: TCompRec; AFindControlOnScreen_Result: Boolean);
begin
  if AFindControlInputData.FullBackgroundImageInResult then
  begin
    case AFindSubControlOptions.MatchBitmapAlgorithm of
      mbaBruteForce, mbaRawHistogramZones, mbaBruteForceOnGPU, mbaRenderTextOnly:
      begin
        frClickerActions.imgDebugGrid.Left := AResultedControl.XOffsetFromParent;
        frClickerActions.imgDebugGrid.Top := AResultedControl.YOffsetFromParent;
      end;

      mbaXYMultipleAndOffsets:
      begin
        frClickerActions.imgDebugGrid.Left := AFindControlInputData.InitialRectangleOffsets.Left;
        frClickerActions.imgDebugGrid.Top := AFindControlInputData.InitialRectangleOffsets.Top;
      end;
    end; //case
  end
  else
  begin
    case AFindSubControlOptions.MatchBitmapAlgorithm of
      mbaBruteForce, mbaRawHistogramZones, mbaBruteForceOnGPU, mbaRenderTextOnly:
      begin
        if AFindSubControlOptions.FullBackgroundImageInResult then
        begin
          frClickerActions.imgDebugGrid.Left := CDebugBitmapBevelWidth - AFindSubControlOptions.MatchBitmapAlgorithmSettings.XOffset; //AResultedControl.XOffsetFromParent;
          frClickerActions.imgDebugGrid.Top := CDebugBitmapBevelHeight - AFindSubControlOptions.MatchBitmapAlgorithmSettings.YOffset; //AResultedControl.YOffsetFromParent;
        end
        else
        begin
          frClickerActions.imgDebugGrid.Left := CDebugBitmapBevelWidth;
          frClickerActions.imgDebugGrid.Top := CDebugBitmapBevelHeight;
        end
      end;

      mbaXYMultipleAndOffsets:
      begin
        if AFindControlOnScreen_Result then
        begin
          frClickerActions.imgDebugGrid.Left := CDebugBitmapBevelWidth - AFindSubControlOptions.MatchBitmapAlgorithmSettings.XOffset;
          frClickerActions.imgDebugGrid.Top := CDebugBitmapBevelHeight - AFindSubControlOptions.MatchBitmapAlgorithmSettings.YOffset;
        end
        else
        begin  //not found
          frClickerActions.imgDebugGrid.Left := AFindControlInputData.InitialRectangleOffsets.Left;
          frClickerActions.imgDebugGrid.Top := AFindControlInputData.InitialRectangleOffsets.Top;
        end
      end;
    end; //case
  end
end;


procedure TActionExecution.GPUDbgBufferToVars(var AGPUDbgBuffer: TIntArr);
var
  i: Integer;
begin
  if GetActionVarValue('$SetGPUDbgBuffer$') = 'True' then
  begin
    SetActionVarValue('$GPUDbgBuffer.Len$', IntToStr(Length(AGPUDbgBuffer)));
    for i := 0 to Length(AGPUDbgBuffer) - 1 do
      SetActionVarValue('$GPUDbgBuffer[' + IntToStr(i) + ']$', IntToStr(AGPUDbgBuffer[i]));
  end;
end;


procedure TActionExecution.SetAllControl_Handles_FromResultedControlArr(var AResultedControlArr: TCompRecArr; AMatchSource, ADetailedMatchSource: string);
var
  i: Integer;
  s, xs, ys, ErrCnts: string;
begin
  s := '';
  xs := '';
  ys := '';
  ErrCnts := '';

  for i := 0 to Length(AResultedControlArr) - 1 do
  begin
    s := s + IntToStr(AResultedControlArr[i].Handle) + #4#5;
    xs := xs + IntToStr(AResultedControlArr[i].XOffsetFromParent) + #4#5;
    ys := ys + IntToStr(AResultedControlArr[i].YOffsetFromParent) + #4#5;
    ErrCnts := ErrCnts + IntToStr(AResultedControlArr[i].ResultedErrorCount) + #4#5;
  end;

  SetActionVarValue('$AllControl_Count$', IntToStr(Length(AResultedControlArr)));

  SetActionVarValue('$AllControl_Handles$', s);
  SetActionVarValue('$AllControl_XOffsets$', xs);
  SetActionVarValue('$AllControl_YOffsets$', ys);

  SetActionVarValue('$AllControl_MatchSource$', AMatchSource);
  SetActionVarValue('$AllControl_DetailedMatchSource$', ADetailedMatchSource);
  SetActionVarValue('$AllControl_ResultedErrorCount$', ErrCnts);
end;


procedure TActionExecution.InitFindControlParams(var AActionOptions: TClkActionOptions; AOutsideTickCount: QWord; var AResultedControl: TCompRec; var AInitialTickCount, ATimeout: QWord; var AFindControlInputData: TFindControlInputData; out AStopAllActionsOnDemandAddr: Pointer);
begin
  AResultedControl.XOffsetFromParent := 0; //init here, in case FindControlOnScreen does not update it
  AResultedControl.YOffsetFromParent := 0; //init here, in case FindControlOnScreen does not update it

  AInitialTickCount := GetTickCount64;
  if AActionOptions.ActionTimeout < 0 then
    ATimeout := 0
  else
    ATimeout := AActionOptions.ActionTimeout;

  AFindControlInputData.OutsideTickCount := AOutsideTickCount;
  AFindControlInputData.PrecisionTimeout := ATimeout;

  if FStopAllActionsOnDemandFromParent <> nil then
  begin
    //MessageBox(Handle, 'Using global stop on demand.', PChar(Caption), MB_ICONINFORMATION);
    AStopAllActionsOnDemandAddr := FStopAllActionsOnDemandFromParent;
  end
  else
  begin
    //MessageBox(Handle, 'Using local stop on demand.', PChar(Caption), MB_ICONINFORMATION);
    AStopAllActionsOnDemandAddr := FStopAllActionsOnDemand;
  end;
end;


function TActionExecution.ExecuteFindControlAction(var AFindControlOptions: TClkFindControlOptions; var AActionOptions: TClkActionOptions; AOutsideTickCount: QWord): Boolean; //returns True if found
var
  FindControlInputData, WorkFindControlInputData: TFindControlInputData;
  BmpTextProfileCount: Integer; //not used in FindControl
  ResultedControl: TCompRec;
  InitialTickCount, Timeout: QWord;
  StopAllActionsOnDemandAddr: Pointer;
  ResultedControlArr, PartialResultedControlArr: TCompRecArr;
  FindControlOnScreen_Result: Boolean;
begin
  Result := False;

  frClickerActions.DebuggingInfoAvailable := False;
  SetActionVarValue('$ExecAction_Err$', '');

  if not FillInFindControlInputData(AFindControlOptions, AActionOptions, FindControlInputData, BmpTextProfileCount) then
    Exit;

  InitFindControlParams(AActionOptions, AOutsideTickCount, ResultedControl, InitialTickCount, Timeout, FindControlInputData, StopAllActionsOnDemandAddr);

  case AFindControlOptions.MatchCriteria.SearchForControlMode of
    sfcmGenGrid:
    begin
      if (mmText in FindControlInputData.MatchingMethods) or
         (mmClass in FindControlInputData.MatchingMethods) then
      begin
        try
          SetLength(PartialResultedControlArr, 0);
          WorkFindControlInputData := FindControlInputData;

          FindControlOnScreen_Result := FindControlOnScreen(WorkFindControlInputData,
                                                            InitialTickCount,
                                                            StopAllActionsOnDemandAddr,
                                                            PartialResultedControlArr,
                                                            DoOnGetGridDrawingOption);
          if FindControlOnScreen_Result then
          begin
            UpdateActionVarValuesFromControl(PartialResultedControlArr[0]);
            //frClickerActions.DebuggingInfoAvailable := True;
            //
            //if AFindControlOptions.GetAllControls then
            //begin
            //  SetAllControl_Handles_FromResultedControlArr(ResultedControlArr);
            //  UpdateActionVarValuesFromResultedControlArr(ResultedControlArr);
            //end;

            CopyPartialResultsToFinalResult(ResultedControlArr, PartialResultedControlArr);
            Result := True;
            AddToLog('Found text: "' + AFindControlOptions.MatchText + '" in ' + IntToStr(GetTickCount64 - InitialTickCount) + 'ms.');

            if AFindControlOptions.GetAllControls then
              AddToLog('Result count: ' + IntToStr(Length(PartialResultedControlArr)));

            if not AFindControlOptions.GetAllControls then
              Exit;  //to prevent further searching for bitmap files, primitives or other text profiles
          end;
        finally
          if Length(PartialResultedControlArr) > 0 then
            ResultedControl := PartialResultedControlArr[0];  //ResultedControl has some fields, initialized before the search. If no result is found, then call SetDbgImgPos with those values.
        end;
      end;
    end;   //sfcmGenGrid

    sfcmEnumWindows:        //Caption OR Class
    begin
      if FindWindowOnScreenByCaptionOrClass(FindControlInputData, StopAllActionsOnDemandAddr, ResultedControlArr) then
      begin
        if Length(ResultedControlArr) > 0 then
          UpdateActionVarValuesFromControl(ResultedControlArr[0]);

        Result := True;
        frClickerActions.DebuggingInfoAvailable := True;

        if AFindControlOptions.GetAllControls then
          SetAllControl_Handles_FromResultedControlArr(ResultedControlArr, '', '');

        Exit;
      end;
    end;

    sfcmFindWindow:         //Caption AND Class
    begin
      if FindWindowOnScreenByCaptionAndClass(FindControlInputData, StopAllActionsOnDemandAddr, ResultedControlArr) then
      begin
        UpdateActionVarValuesFromControl(ResultedControlArr[0]);
        Result := True;
        frClickerActions.DebuggingInfoAvailable := True;

        if AFindControlOptions.GetAllControls then
          SetAllControl_Handles_FromResultedControlArr(ResultedControlArr, '', '');

        Exit;
      end;
    end;
  end; //case

  if Result then
    if Length(ResultedControlArr) > 0 then
    begin
      UpdateActionVarValuesFromControl(ResultedControlArr[0]);
      frClickerActions.DebuggingInfoAvailable := True;

      if AFindControlOptions.GetAllControls then
      begin
        SetAllControl_Handles_FromResultedControlArr(ResultedControlArr, '', '');
        UpdateActionVarValuesFromResultedControlArr(ResultedControlArr);
      end;
    end;
end;


function TActionExecution.ExecuteFindSubControlAction(var AFindSubControlOptions: TClkFindSubControlOptions; var AActionOptions: TClkActionOptions; AOutsideTickCount: QWord): Boolean; //returns True if found
{$IFDEF FPC}
  //const
  //  clSystemColor = $FF000000;
{$ENDIF}
  procedure AddInfoToMatchSource(AMatchSourceInfo, ADetailedMatchSourceInfo: string; ACount: Integer; var AMatchSource, ADetailedMatchSource: string);
  var
    ii: Integer;
  begin
    for ii := 0 to ACount - 1 do
    begin
      AMatchSource := AMatchSource + AMatchSourceInfo + #4#5;                         //see $AllControl_MatchSource$ var
      ADetailedMatchSource := ADetailedMatchSource + ADetailedMatchSourceInfo + #4#5; //see $AllControl_DetailedMatchSource$ var
    end;
  end;

  procedure LoadBitmapToSearchOn(AFindControlInputData: TFindControlInputData);
  var
    Res: Boolean;
    Fnm: string;
  begin
    if AFindSubControlOptions.ImageSource = isFile then
    begin
      Res := True;
      Fnm := DoOnResolveTemplatePath(AFindSubControlOptions.SourceFileName);

      case AFindSubControlOptions.ImageSourceFileNameLocation of
        isflDisk:
          Res := DoOnLoadBitmap(AFindControlInputData.BitmapToSearchOn, Fnm);

        isflMem:
          Res := DoOnLoadRenderedBitmap(AFindControlInputData.BitmapToSearchOn, Fnm);
      end;

      if not Res then
        AddToLog('Cannot load BitmapToSearchOn.  FileNameLocation = ' +
                 CImageSourceFileNameLocationStr[AFindSubControlOptions.ImageSourceFileNameLocation] +
                 '  FileName = "' + Fnm + '"');
    end
    else
      AddToLog('BitmapToSearchOn not used... Using screenshot.');
  end;

  function RunApp(AApp, AParams: string): Boolean;
  var
    ExecAppAction: TClkExecAppOptions;
    ActionOptions: TClkActionOptions;
  begin
    GetDefaultPropertyValues_ExecApp(ExecAppAction);
    ExecAppAction.PathToApp := AApp;
    ExecAppAction.ListOfParams := AParams;
    ExecAppAction.VerifyFileExistence := False;

    ActionOptions.Action := acExecApp;
    ActionOptions.ActionName := 'RunApp';
    ActionOptions.ActionTimeout := 3000;
    Result := ExecuteExecAppAction(ExecAppAction, ActionOptions);
  end;

var
  i, j, k, BmpTextProfileCount: Integer;
  ListOfBitmapFiles, ListOfPrimitiveFiles, ListOfBmpsInPrimitiveFiles: TStringList;
  ResultedControl: TCompRec;
  ResultedControlArr, PartialResultedControlArr: TCompRecArr;
  ResultedControlArr_Text, ResultedControlArr_Bmp, ResultedControlArr_Pmtv: TCompRecArr;
  MatchSource, DetailedMatchSource: string;
  InitialTickCount, Timeout, tk: QWord;
  TimeoutReceivingBitmaps, AllBitmapsReceived: Boolean;
  FindControlInputData, WorkFindControlInputData: TFindControlInputData;
  StopAllActionsOnDemandAddr: Pointer;
  EvalFG, EvalBG: string;
  TemplateDir: string;

  TempPrimitives: TPrimitiveRecArr;
  TempOrders: TCompositionOrderArr;
  TempPrimitiveSettings: TPrimitiveSettings;
  PrimitivesCompositor: TPrimitivesCompositor;
  PrimitiveFound: Boolean;
  FindControlOnScreen_Result: Boolean;
  ExtBmpName: string;
  IsExtBmp: Boolean;
  TempBmp: TBitmap;
  WebLink: string;
begin
  Result := False;

  frClickerActions.DebuggingInfoAvailable := False;
  SetActionVarValue('$ExecAction_Err$', '');

  if not FillInFindSubControlInputData(AFindSubControlOptions, AActionOptions, FindControlInputData, BmpTextProfileCount) then
    Exit;

  InitFindControlParams(AActionOptions, AOutsideTickCount, ResultedControl, InitialTickCount, Timeout, FindControlInputData, StopAllActionsOnDemandAddr);

  SetLength(ResultedControlArr_Text, 0);
  SetLength(ResultedControlArr_Bmp, 0);
  SetLength(ResultedControlArr_Pmtv, 0);

  MatchSource := '';
  DetailedMatchSource := '';
  try
    if AFindSubControlOptions.MatchCriteria.WillMatchBitmapText then
    begin
      if AFindSubControlOptions.UseTextRenderingInBrowser then
        EnterCriticalSection(FBrowserRenderingText.CritSec);
      try
        if AFindSubControlOptions.UseTextRenderingInBrowser then
        begin
          FBrowserRenderingText.Txt := AFindSubControlOptions.MatchText;
          SetLength(FBrowserRenderingText.FontProfiles, Length(AFindSubControlOptions.MatchBitmapText));
          SetLength(FBrowserRenderingText.RenderedFileNames, Length(AFindSubControlOptions.MatchBitmapText));

          for j := 0 to Length(FBrowserRenderingText.RenderedFileNames) - 1 do
          begin
            FBrowserRenderingText.FontProfiles[j] := AFindSubControlOptions.MatchBitmapText[j]; //copy entire profile
            FBrowserRenderingText.FontProfiles[j].ForegroundColor := EvaluateReplacements(AFindSubControlOptions.MatchBitmapText[j].ForegroundColor, True);
            FBrowserRenderingText.FontProfiles[j].BackgroundColor := EvaluateReplacements(AFindSubControlOptions.MatchBitmapText[j].BackgroundColor, True);
            FBrowserRenderingText.RenderedFileNames[j] := 'BrowserTxt_' + IntToStr(j) + '_L' + IntToStr(FStackLevel^) + '.png';  //maybe add a timestamp after index. Make sure the string does not contain quotes or other special characters in JS.
          end;

          FBrowserRenderingText.RequestID := AActionOptions.ActionName + '_' + DateTimeToStr(Now) + '_' + IntToStr(GetTickCount64) + '_L' + IntToStr(FStackLevel^);
          FBrowserRenderingText.RequestID := StringReplace(FBrowserRenderingText.RequestID, '"', '_', [rfReplaceAll]);
          FBrowserRenderingText.RequestID := StringReplace(FBrowserRenderingText.RequestID, '''', '_', [rfReplaceAll]);
          FBrowserRenderingText.RequestID := StringReplace(FBrowserRenderingText.RequestID, '?', '_', [rfReplaceAll]);
          FBrowserRenderingText.RequestID := StringReplace(FBrowserRenderingText.RequestID, ' ', '_', [rfReplaceAll]);
          FBrowserRenderingText.RequestID := StringReplace(FBrowserRenderingText.RequestID, '=', '_', [rfReplaceAll]);  //maybe another validator (a function) is required

          FBrowserRenderingText.FontSizeUnit := CFontSizeUnitNoPrefixStr[AFindSubControlOptions.RenderingInBrowserSettings.FontSizeUnit];
          FBrowserRenderingText.Timeout := IntToStr(AFindSubControlOptions.RenderingInBrowserSettings.ReceivingBitmapsTimeout);

          if AFindSubControlOptions.RenderingInBrowserSettings.RenderingRequestType = rrtShellExecute then
          begin
            WebLink := 'http://127.0.0.1:' + IntToStr(DoOnGetListeningPort) + '/' +
                                                  CRECmd_GetTextRenderingPage + '?' +
                                                  CREParam_StackLevel + '=' + IntToStr(FStackLevel^) + '&' +
                                                  CREParam_ID + '=' + FBrowserRenderingText.RequestID;
            {$IFDEF Windows}
              ShellExecute(0, 'open', PChar(WebLink), '', '', 5);  //SW_SHOW
              //if not RunApp('explorer', '"' + WebLink + '"') then
              //  AddToLog('Can''t run application to open web browser.');
            {$ELSE}
              if not RunApp('xdg-open', '"' + WebLink + '"') then
                AddToLog('Can''t run application to open web browser.');
            {$ENDIF}
          end
          else
          begin  //AFindSubControlOptions
            AddToLog('Sending rendering request via an action, instead of using ShellExecute.  RequestID=' + FBrowserRenderingText.RequestID);
            SetActionVarValue('$StackLevel$', IntToStr(FStackLevel^));
            SetActionVarValue('$RenderingRequestID$', FBrowserRenderingText.RequestID); //used by the called action (ExecApp, CallTemplate, Plugin)
            try
              AddToLog('Rendering with action: "' + AFindSubControlOptions.RenderingInBrowserSettings.ActionForSendingRequest + '", at stack level ' + IntToStr(FStackLevel^) + '.');
              if not DoOnExecuteActionByName(AFindSubControlOptions.RenderingInBrowserSettings.ActionForSendingRequest) then
              begin
                AddToLog('Sending rendering request from action, failed at "' + AFindSubControlOptions.RenderingInBrowserSettings.ActionForSendingRequest + '" action.');
                Result := False;
                Exit;
              end;
            finally
              SetActionVarValue('$RenderingRequestID$', 'outdated');
            end;
          end;

          if AFindSubControlOptions.RenderingInBrowserSettings.UsePluginForReceivingBitmaps then
          begin
            AddToLog('Waiting for rendered bitmaps via a plugin action, instead of using UIClicker''s SetRenderedFileB64 request.');
            if not DoOnExecuteActionByName(AFindSubControlOptions.RenderingInBrowserSettings.PluginActionForReceivingBitmaps) then
              raise Exception.Create('Waiting for rendered bitmaps, failed at "' + AFindSubControlOptions.RenderingInBrowserSettings.PluginActionForReceivingBitmaps + '" action.');
          end;

          TimeoutReceivingBitmaps := False;
          TempBmp := TBitmap.Create;
          try
            tk := GetTickCount64;
            repeat                              // wait for BrowserRendering timeout or all files to be present
              Application.ProcessMessages;
              Sleep(1);

              if GetTickCount64 - tk > AFindSubControlOptions.RenderingInBrowserSettings.ReceivingBitmapsTimeout then
              begin
                TimeoutReceivingBitmaps := True;
                Break;
              end;

              AllBitmapsReceived := True;
              for j := 0 to Length(FBrowserRenderingText.RenderedFileNames) - 1 do
                if not DoOnLoadRenderedBitmap(TempBmp, FBrowserRenderingText.RenderedFileNames[j]) then  //instead of DoOnLoadRenderedBitmap, there should be a verification for file existence
                  AllBitmapsReceived := False;

              if AllBitmapsReceived then
                Break;
            until False;
          finally
            TempBmp.Free;
          end;

          if TimeoutReceivingBitmaps then
          begin
            AddToLog(DateTimeToStr(Now) + '  Timeout receiving all expected rendered bitmaps.');
            if not AFindSubControlOptions.RenderingInBrowserSettings.UsePluginForReceivingBitmaps then
              AddToLog(DateTimeToStr(Now) + '  Please also verify if UIClicker is running in server mode.');
          end;
        end; //AFindSubControlOptions.UseTextRenderingInBrowser

        SetLength(ResultedControlArr, 0);
        for j := 0 to BmpTextProfileCount - 1 do //number of font profiles
        begin
          if j > BmpTextProfileCount - 1 then  //it seems that a FP bug allows "j" to go past n - 1. It may happen on EnumerateWindows only. At best, the memory is overwritten, which causes this behavior.
            Break;

          FindControlInputData.BitmapToSearchFor := TBitmap.Create;
          FindControlInputData.BitmapToSearchOn := TBitmap.Create;
          try
            FindControlInputData.BitmapToSearchFor.PixelFormat := pf24bit;
            LoadBitmapToSearchOn(FindControlInputData);

            //if AFindSubControlOptions.MatchCriteria.WillMatchBitmapText then
            begin
              EvalFG := EvaluateReplacements(AFindSubControlOptions.MatchBitmapText[j].ForegroundColor, True);
              EvalBG := EvaluateReplacements(AFindSubControlOptions.MatchBitmapText[j].BackgroundColor, True);

              if AFindSubControlOptions.MatchBitmapAlgorithm <> mbaRenderTextOnly then
                AddToLog('Searching with text profile[' + IntToStr(j) + ']: ' + AFindSubControlOptions.MatchBitmapText[j].ProfileName)
              else
                AddToLog('Rendering text profile[' + IntToStr(j) + ']: ' + AFindSubControlOptions.MatchBitmapText[j].ProfileName);

              SetActionVarValue('$DebugVar_TextColors$',
                                'FileName=' + FSelfTemplateFileName^ +
                                //' FG=' + IntToHex(frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].FGColor, 8) +
                                //' BG=' + IntToHex(frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].BGColor, 8) +
                                ' Eval(FG)=' + EvaluateReplacements(AFindSubControlOptions.MatchBitmapText[j].ForegroundColor, False) + '=' + EvalFG +
                                ' Eval(BG)=' + EvaluateReplacements(AFindSubControlOptions.MatchBitmapText[j].BackgroundColor, False) + '=' + EvalBG );

              //if frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].FGColor and clSystemColor <> 0 then  //clSystemColor is declared above
              //begin
              //  frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].FGColor := clFuchsia;
              //  AddToLog('System color found on text FG: $' + IntToHex(frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].FGColor, 8));
              //end;
              //
              //if frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].BGColor and clSystemColor <> 0 then  //clSystemColor is declared above
              //begin
              //  frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].BGColor := clLime;
              //  AddToLog('System color found on text BG: $' + IntToHex(frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].BGColor, 8));
              //end;

              SetActionVarValue('$DebugVar_BitmapText$', FindControlInputData.Text);

              // frClickerActions.frClickerFindControl.PreviewText;

              try
                if not AFindSubControlOptions.UseTextRenderingInBrowser then
                  PreviewTextOnBmp(AFindSubControlOptions, FindControlInputData.Text, j, FindControlInputData.BitmapToSearchFor)
                else
                  if not DoOnLoadRenderedBitmap(FindControlInputData.BitmapToSearchFor, FBrowserRenderingText.RenderedFileNames[j]) then
                    raise Exception.Create('Can''t load rendered file from browser: ' + FBrowserRenderingText.RenderedFileNames[j]);
              except
                on E: Exception do
                begin
                  AddToLog('Can''t preview bmp text. Ex: "' + E.Message + '".  Action: "' + AActionOptions.ActionName + '" of ' + CClkActionStr[AActionOptions.Action] + ' type.');
                  // frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].ProfileName  is no longer available, since no UI is updated on remote execution. It is replaced by AFindSubControlOptions.MatchBitmapText[j].ProfileName.
                  raise Exception.Create(E.Message + '  Profile[' + IntToStr(j) + ']: "' + AFindSubControlOptions.MatchBitmapText[j].ProfileName + '".   Searched text: "' + FindControlInputData.Text + '"');
                end;
              end;
              //This is the original code for getting the text from the editor, instead of rendering with PreviewTextOnBmp. It should do the same thing.
              //FindControlInputData.BitmapToSearchFor.Width := frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].PreviewImageBitmap.Width;
              //FindControlInputData.BitmapToSearchFor.Height := frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].PreviewImageBitmap.Height;
              //FindControlInputData.BitmapToSearchFor.Canvas.Draw(0, 0, frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].PreviewImageBitmap);   //updated above by PreviewText
            end;  //WillMatchBitmapText

            //if AFindSubControlOptions.MatchCriteria.WillMatchPrimitiveFiles then   //dbg only
            //begin
            //  FindControlInputData.BitmapToSearchFor.Canvas.Pen.Color := clRed;
            //  FindControlInputData.BitmapToSearchFor.Canvas.Line(20, 30, 60, 70);
            //end;

            //negative area verification - moved above "for j" loop
            if AFindSubControlOptions.MatchBitmapAlgorithm <> mbaRenderTextOnly then
            begin
              FindControlInputData.IgnoreBackgroundColor := AFindSubControlOptions.MatchBitmapText[j].IgnoreBackgroundColor;
              FindControlInputData.BackgroundColor := HexToInt(EvalBG);


              //clear debug image
              frClickerActions.imgDebugBmp.Canvas.Pen.Color := clWhite;
              frClickerActions.imgDebugBmp.Canvas.Brush.Color := clWhite;
              frClickerActions.imgDebugBmp.Canvas.Rectangle(0, 0, frClickerActions.imgDebugBmp.Width, frClickerActions.imgDebugBmp.Height);

              //FindControlInputData.DebugBitmap := frClickerActions.imgDebugBmp.Picture.Bitmap;    //section moved above :for j" loop
              //FindControlInputData.DebugGrid := frClickerActions.imgDebugGrid;

              try
                SetLength(PartialResultedControlArr, 0);
                WorkFindControlInputData := FindControlInputData;
                FindControlOnScreen_Result := FindSubControlOnScreen(AFindSubControlOptions.MatchBitmapAlgorithm,
                                                                     AFindSubControlOptions.MatchBitmapAlgorithmSettings,
                                                                     WorkFindControlInputData,
                                                                     InitialTickCount,
                                                                     StopAllActionsOnDemandAddr,
                                                                     PartialResultedControlArr,
                                                                     DoOnGetGridDrawingOption);

                if FindControlOnScreen_Result or not FindControlInputData.StopSearchOnMismatch then
                begin
                  SetActionVarValue('$ActionExecDuration$', IntToStr(GetTickCount64 - InitialTickCount));
                  if not FindControlOnScreen_Result and not FindControlInputData.StopSearchOnMismatch then
                    AddToLog('Can''t find the subcontrol (text), but the searching went further, to get the error count. See $ResultedErrorCount$.');

                  if Length(PartialResultedControlArr) = 0 then  //it looks like FindControlOnScreen may return with an empty array and a result set to True
                    SetLength(PartialResultedControlArr, 1);

                  UpdateActionVarValuesFromControl(PartialResultedControlArr[0], not FindControlInputData.StopSearchOnMismatch);
                  //frClickerActions.DebuggingInfoAvailable := True;
                  //
                  //if AFindSubControlOptions.GetAllControls then
                  //begin
                  //  SetAllControl_Handles_FromResultedControlArr(ResultedControlArr);
                  //  UpdateActionVarValuesFromResultedControlArr(ResultedControlArr);
                  //end;

                  CopyPartialResultsToFinalResult(ResultedControlArr_Text, PartialResultedControlArr);

                  Result := True;
                  AddToLog('Found text: "' + AFindSubControlOptions.MatchText + '" in ' + IntToStr(GetTickCount64 - InitialTickCount) + 'ms.');

                  if AFindSubControlOptions.GetAllControls then
                  begin
                    AddToLog('Result count: ' + IntToStr(Length(PartialResultedControlArr)));
                    AddInfoToMatchSource('txt[' + IntToStr(j) + ']', 'txt[' + IntToStr(j) + '][0]', Length(PartialResultedControlArr), MatchSource, DetailedMatchSource); //hardcoded to [0] as no other subfeature is implemented
                  end;

                  if not AFindSubControlOptions.GetAllControls then
                    Exit;  //to prevent further searching for bitmap files, primitives or other text profiles
                end;
              finally
                if Length(PartialResultedControlArr) > 0 then
                  ResultedControl := PartialResultedControlArr[0];  //ResultedControl has some fields, initialized before the search. If no result is found, then call SetDbgImgPos with those values.

                SetDbgImgPos(AFindSubControlOptions, WorkFindControlInputData, ResultedControl, FindControlOnScreen_Result);
                GPUDbgBufferToVars(WorkFindControlInputData.GPUDbgBuffer);
              end;
            end
            else  //mbaRenderTextOnly
            begin
              ExtBmpName := CExtBmp_Prefix + PathDelim + AActionOptions.ActionName + '_' + AFindSubControlOptions.MatchBitmapText[j].ProfileName + '.bmp';
              DoOnSaveRenderedBitmap(FindControlInputData.BitmapToSearchFor, ExtBmpName);
              //
            end;
          finally
            FindControlInputData.BitmapToSearchFor.Free;
            FindControlInputData.BitmapToSearchOn.Free;
          end;

          if Result and not AFindSubControlOptions.GetAllControls then
            Break;
        end;  //for j  - font profiles

        if AFindSubControlOptions.MatchBitmapAlgorithm = mbaRenderTextOnly then
        begin
          Result := True;
          Exit;  //Done here
        end;

        if FExtraLogging_FindControl then
        begin
          AddToLog('MatchSource: ' + MatchSource);
          AddToLog('DetailedMatchSource: ' + DetailedMatchSource);
        end;
      finally
        if AFindSubControlOptions.UseTextRenderingInBrowser then
        begin
          AddToLog('Deleting received rendered bitmaps from In-Mem FS.  StackLevel: ' + IntToStr(FStackLevel^));
          for j := 0 to Length(FBrowserRenderingText.RenderedFileNames) - 1 do
            DoOnDeleteRenderedBitmap(FBrowserRenderingText.RenderedFileNames[j]);

          LeaveCriticalSection(FBrowserRenderingText.CritSec);
        end;
      end;
    end; //WillMatchBitmapText


    if mmBitmapFiles in FindControlInputData.MatchingMethods then
    begin
      FindControlInputData.BitmapToSearchFor := TBitmap.Create;
      FindControlInputData.BitmapToSearchOn := TBitmap.Create;
      try
        FindControlInputData.BitmapToSearchFor.PixelFormat := pf24bit;
        LoadBitmapToSearchOn(FindControlInputData);

        if FExtraLogging_FindControl and (AFindSubControlOptions.ImageSource = isFile) then
          AddToLog('Background file: "' + AFindSubControlOptions.SourceFileName +
                   '"  Width = ' + IntToStr(FindControlInputData.BitmapToSearchOn.Width) +
                   '   Height = ' + IntToStr(FindControlInputData.BitmapToSearchOn.Height));

        ListOfBitmapFiles := TStringList.Create;
        try
          ListOfBitmapFiles.LineBreak := #13#10;
          ListOfBitmapFiles.Text := AFindSubControlOptions.MatchBitmapFiles;

          if FExtraLogging_FindControl then
            AddToLog('Bmp file count to search with: ' + IntToStr(ListOfBitmapFiles.Count));

          if FExecutingActionFromRemote = nil then
            raise Exception.Create('FExecutingActionFromRemote is not assigned.');

          if FFileLocationOfDepsIsMem = nil then
            raise Exception.Create('FFileLocationOfDepsIsMem is not assigned.');

          if FSelfTemplateFileName = nil then
            TemplateDir := 'FSelfTemplateFileName not set.'
          else
            TemplateDir := ExtractFileDir(FSelfTemplateFileName^);

          for i := 0 to ListOfBitmapFiles.Count - 1 do
            ListOfBitmapFiles.Strings[i] := StringReplace(ListOfBitmapFiles.Strings[i], '$SelfTemplateDir$', TemplateDir, [rfReplaceAll]);

          for i := 0 to ListOfBitmapFiles.Count - 1 do
            ListOfBitmapFiles.Strings[i] := StringReplace(ListOfBitmapFiles.Strings[i], '$TemplateDir$', FFullTemplatesDir^, [rfReplaceAll]);

          //Leave this section commented, it exists after the DoOnWaitForBitmapsAvailability call!
          //if not FExecutingActionFromRemote^ then
          //begin
          //  for i := 0 to ListOfBitmapFiles.Count - 1 do
          //    ListOfBitmapFiles.Strings[i] := StringReplace(ListOfBitmapFiles.Strings[i], '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);
          //end;

          if FExecutingActionFromRemote^ and FFileLocationOfDepsIsMem^ then
          begin
            AddToLog('Might wait for some bitmap files to be present in memory..');
            DoOnWaitForBitmapsAvailability(ListOfBitmapFiles);
          end;

          //resolving the $AppDir$ replacement after having all files available
          for i := 0 to ListOfBitmapFiles.Count - 1 do
            ListOfBitmapFiles.Strings[i] := StringReplace(ListOfBitmapFiles.Strings[i], '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);

          for i := 0 to ListOfBitmapFiles.Count - 1 do
          begin
            IsExtBmp := Pos(CExtBmp_PrefixUpperCase, UpperCase(ListOfBitmapFiles.Strings[i])) = 1;
            if (IsExtBmp and not DoOnLoadRenderedBitmap(FindControlInputData.BitmapToSearchFor, ListOfBitmapFiles.Strings[i])) or
               (not IsExtBmp and not DoOnLoadBitmap(FindControlInputData.BitmapToSearchFor, ListOfBitmapFiles.Strings[i])) then
            begin
              AppendErrorMessageToActionVar('File not found: "' + ListOfBitmapFiles.Strings[i] + '" ');
              Continue;
            end;

            //memLogErr.Lines.Add('DebugBitmap pixel format: ' + IntToStr(Ord(FindControlInputData.DebugBitmap.PixelFormat))); // [6]  - 24-bit

            InitialTickCount := GetTickCount64;
            if AActionOptions.ActionTimeout < 0 then
              Timeout := 0
            else
              Timeout := AActionOptions.ActionTimeout;

            SetLength(PartialResultedControlArr, 0);
            WorkFindControlInputData := FindControlInputData;
            FindControlOnScreen_Result := FindSubControlOnScreen(AFindSubControlOptions.MatchBitmapAlgorithm,
                                                                 AFindSubControlOptions.MatchBitmapAlgorithmSettings,
                                                                 WorkFindControlInputData,
                                                                 InitialTickCount,
                                                                 StopAllActionsOnDemandAddr,
                                                                 PartialResultedControlArr,
                                                                 DoOnGetGridDrawingOption);

            if FindControlOnScreen_Result or not FindControlInputData.StopSearchOnMismatch then
            begin
              SetActionVarValue('$ActionExecDuration$', IntToStr(GetTickCount64 - InitialTickCount));
              if not FindControlOnScreen_Result and not FindControlInputData.StopSearchOnMismatch then
                AddToLog('Can''t find the subcontrol (bmp), but the searching went further, to get the error count. See $ResultedErrorCount$.');

              if Length(PartialResultedControlArr) = 0 then  //it looks like FindControlOnScreen may return with an empty array and a result set to True
                SetLength(PartialResultedControlArr, 1);

              UpdateActionVarValuesFromControl(PartialResultedControlArr[0], not FindControlInputData.StopSearchOnMismatch);
              frClickerActions.DebuggingInfoAvailable := True;

              CopyPartialResultsToFinalResult(ResultedControlArr_Bmp, PartialResultedControlArr);
              Result := True;

              if AFindSubControlOptions.GetAllControls then
              begin
                //SetAllControl_Handles_FromResultedControlArr(ResultedControlArr);
                //UpdateActionVarValuesFromResultedControlArr(ResultedControlArr);

                if FExtraLogging_FindControl then
                  AddToLog('Result count: ' + IntToStr(Length(PartialResultedControlArr)));

                AddInfoToMatchSource('bmp[' + IntToStr(i) + ']', 'bmp[' + IntToStr(i) + '][0]', Length(PartialResultedControlArr), MatchSource, DetailedMatchSource); //hardcoded to [0] as no other subfeature is implemented
              end
              else
                Exit;  //to prevent further searching for other bitmap files
            end;
          end; //for i
        finally
          ListOfBitmapFiles.Free;
          if Length(PartialResultedControlArr) > 0 then
            ResultedControl := PartialResultedControlArr[0];

          SetDbgImgPos(AFindSubControlOptions, WorkFindControlInputData, ResultedControl, FindControlOnScreen_Result);
          GPUDbgBufferToVars(WorkFindControlInputData.GPUDbgBuffer);
        end;
      finally
        FindControlInputData.BitmapToSearchFor.Free;
        FindControlInputData.BitmapToSearchOn.Free;
      end;

      if FExtraLogging_FindControl then
      begin
        AddToLog('MatchSource: ' + MatchSource);
        AddToLog('DetailedMatchSource: ' + DetailedMatchSource);
      end;
    end; //WillMatchBitmapFiles

    if AFindSubControlOptions.MatchCriteria.WillMatchPrimitiveFiles then
    begin
      FindControlInputData.BitmapToSearchFor := TBitmap.Create;
      FindControlInputData.BitmapToSearchOn := TBitmap.Create;
      try
        FindControlInputData.BitmapToSearchFor.PixelFormat := pf24bit;
        LoadBitmapToSearchOn(FindControlInputData);

        ListOfPrimitiveFiles := TStringList.Create;
        ListOfBmpsInPrimitiveFiles := TStringList.Create;
        try
          ListOfPrimitiveFiles.LineBreak := #13#10;
          ListOfPrimitiveFiles.Text := AFindSubControlOptions.MatchPrimitiveFiles;

          if FExtraLogging_FindControl then
            AddToLog('Pmtv file count to search with: ' + IntToStr(ListOfPrimitiveFiles.Count));

          if FSelfTemplateFileName = nil then
            TemplateDir := 'FSelfTemplateFileName not set.'
          else
            TemplateDir := ExtractFileDir(FSelfTemplateFileName^);

          for i := 0 to ListOfPrimitiveFiles.Count - 1 do
            ListOfPrimitiveFiles.Strings[i] := StringReplace(ListOfPrimitiveFiles.Strings[i], '$SelfTemplateDir$', TemplateDir, [rfReplaceAll]);

          for i := 0 to ListOfPrimitiveFiles.Count - 1 do
            ListOfPrimitiveFiles.Strings[i] := StringReplace(ListOfPrimitiveFiles.Strings[i], '$TemplateDir$', FFullTemplatesDir^, [rfReplaceAll]);

          //Leave this section commented, it exists after the DoOnWaitForBitmapsAvailability call!
          //if not FExecutingActionFromRemote^ then   //files from client will not have the $AppDir$ replacement resolved here, because of requesting them with original name
          //begin
          //  for i := 0 to ListOfPrimitiveFiles.Count - 1 do
          //    ListOfPrimitiveFiles.Strings[i] := StringReplace(ListOfPrimitiveFiles.Strings[i], '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);
          //end;

          if FExecutingActionFromRemote^ and FFileLocationOfDepsIsMem^ then
          begin
            AddToLog('Might wait for some primitives files to be present in memory..');
            DoOnWaitForBitmapsAvailability(ListOfPrimitiveFiles);    //might also work for pmtv files
                                                                     //ComposePrimitive_Image also has to wait for bmp files
          end;

          //resolving the $AppDir$ replacement after having all files available
          for i := 0 to ListOfPrimitiveFiles.Count - 1 do
            ListOfPrimitiveFiles.Strings[i] := StringReplace(ListOfPrimitiveFiles.Strings[i], '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);


          for i := 0 to ListOfPrimitiveFiles.Count - 1 do
          begin
            PrimitiveFound := False;
            DoOnLoadPrimitivesFile(ListOfPrimitiveFiles.Strings[i], TempPrimitives, TempOrders, TempPrimitiveSettings);
            //if not DoOnLoadPrimitivesFile(ListOfPrimitiveFiles.Strings[i], TempPrimitives, TempOrders, TempPrimitiveSettings)then
            //begin
            //  AppendErrorMessageToActionVar('File not found: "' + ListOfPrimitiveFiles.Strings[i] + '" ');
            //  Continue;
            //end;

            if Length(TempPrimitives) = 0 then
            begin
              if FExecutingActionFromRemote^ and (Pos('$AppDir$', ListOfPrimitiveFiles.Strings[i]) > 0) then
                AddToLog('Primitives file: "' + ExtractFileName(ListOfPrimitiveFiles.Strings[i]) + '" has no primitives because is is not loaded. It should have been received from client but it has an illegal path, which contains "$AppDir$".')
              else
                AddToLog('Primitives file: "' + ExtractFileName(ListOfPrimitiveFiles.Strings[i]) + '" has no primitives.');

              Continue;
            end;

            if FExecutingActionFromRemote^ and FFileLocationOfDepsIsMem^ then
            begin
              GetPathsToImagesFromPrimitivesFile(TempPrimitives, ListOfBmpsInPrimitiveFiles);

              AddToLog('Might wait for some bmps from primitives files to be present in memory..');
              DoOnWaitForBitmapsAvailability(ListOfBmpsInPrimitiveFiles);
            end;

            PrimitivesCompositor := TPrimitivesCompositor.Create;
            try
              PrimitivesCompositor.FileIndex := i;
              PrimitivesCompositor.OnEvaluateReplacementsFunc := HandleOnEvaluateReplacements;
              PrimitivesCompositor.OnLoadBitmap := HandleOnLoadBitmap;
              PrimitivesCompositor.OnLoadRenderedBitmap := HandleOnLoadRenderedBitmap;

              FindControlInputData.BitmapToSearchFor.Width := PrimitivesCompositor.GetMaxX(FindControlInputData.BitmapToSearchFor.Canvas, TempPrimitives) + 1;
              FindControlInputData.BitmapToSearchFor.Height := PrimitivesCompositor.GetMaxY(FindControlInputData.BitmapToSearchFor.Canvas, TempPrimitives) + 1;

              if (FindControlInputData.BitmapToSearchFor.Width = 0) or (FindControlInputData.BitmapToSearchFor.Height = 0) then
              begin
                AddToLog('Primitives file: "' + ExtractFileName(ListOfPrimitiveFiles.Strings[i]) + '" has a zero width or height');
                Continue;
              end;

              for k := 0 to Length(TempOrders) - 1 do
              begin
                InitialTickCount := GetTickCount64;
                if AActionOptions.ActionTimeout < 0 then
                  Timeout := 0
                else
                  Timeout := AActionOptions.ActionTimeout;

                //no need to clear the bitmap, it is already implemented in ComposePrimitives
                PrimitivesCompositor.ComposePrimitives(FindControlInputData.BitmapToSearchFor, k, False, TempPrimitives, TempOrders, TempPrimitiveSettings);

                SetLength(PartialResultedControlArr, 0);
                WorkFindControlInputData := FindControlInputData;
                FindControlOnScreen_Result := FindSubControlOnScreen(AFindSubControlOptions.MatchBitmapAlgorithm,
                                                                     AFindSubControlOptions.MatchBitmapAlgorithmSettings,
                                                                     WorkFindControlInputData,
                                                                     InitialTickCount,
                                                                     StopAllActionsOnDemandAddr,
                                                                     PartialResultedControlArr,
                                                                     DoOnGetGridDrawingOption);

                if FindControlOnScreen_Result or not FindControlInputData.StopSearchOnMismatch then
                begin
                  SetActionVarValue('$ActionExecDuration$', IntToStr(GetTickCount64 - InitialTickCount));
                  if not FindControlOnScreen_Result and not FindControlInputData.StopSearchOnMismatch then
                    AddToLog('Can''t find the subcontrol (pmtv), but the searching went further, to get the error count. See $ResultedErrorCount$.');

                  if Length(PartialResultedControlArr) = 0 then  //it looks like FindControlOnScreen may return with an empty array and a result set to True
                    SetLength(PartialResultedControlArr, 1);

                  PrimitiveFound := True;
                  UpdateActionVarValuesFromControl(PartialResultedControlArr[0], not FindControlInputData.StopSearchOnMismatch);
                  frClickerActions.DebuggingInfoAvailable := True;

                  CopyPartialResultsToFinalResult(ResultedControlArr_Pmtv, PartialResultedControlArr);
                  Result := True;

                  if FExtraLogging_FindControl then
                    AddToLog('Matched by primitives file: "' + ExtractFileName(ListOfPrimitiveFiles.Strings[i]) + '"  at order ' + IntToStr(k) + '.  Bmp w/h: ' + IntToStr(FindControlInputData.BitmapToSearchFor.Width) + ' / ' + IntToStr(FindControlInputData.BitmapToSearchFor.Height) + '  Result count: ' + IntToStr(Length(ResultedControlArr)));

                  if AFindSubControlOptions.GetAllControls then
                  begin
                    if FExtraLogging_FindControl then
                      AddToLog('Result count: ' + IntToStr(Length(PartialResultedControlArr)));

                    AddInfoToMatchSource('pmtv[' + IntToStr(i * Length(TempOrders) + k) + ']', 'pmtv[' + IntToStr(i) + '][' + IntToStr(k) + ']', Length(PartialResultedControlArr), MatchSource, DetailedMatchSource);
                  end;

                  if not AFindSubControlOptions.GetAllControls then
                  begin
                    //SetAllControl_Handles_FromResultedControlArr(ResultedControlArr);
                    //UpdateActionVarValuesFromResultedControlArr(ResultedControlArr);
                    //Do not call UpdateActionVarValuesFromResultedControlArr and SetAllControl_Handles_FromResultedControlArr here, because this loop is about primitives orders
                    Break;  //to prevent further searching for other primitive compositions
                  end;
                end;
              end; //for k
            finally
              PrimitivesCompositor.Free;
            end;

            if PrimitiveFound then     //use PrimitiveFound outside of "for k" loop
            begin
              if AFindSubControlOptions.GetAllControls then
              begin
                //SetAllControl_Handles_FromResultedControlArr(ResultedControlArr);
                //UpdateActionVarValuesFromResultedControlArr(ResultedControlArr);
              end
              else
                Exit;  //to prevent further searching for other primitive compositions
            end;
          end; //for i   -  primitives
        finally
          ListOfPrimitiveFiles.Free;
          ListOfBmpsInPrimitiveFiles.Free;
          if Length(PartialResultedControlArr) > 0 then
            ResultedControl := PartialResultedControlArr[0];

          SetDbgImgPos(AFindSubControlOptions, WorkFindControlInputData, ResultedControl, FindControlOnScreen_Result);
          GPUDbgBufferToVars(WorkFindControlInputData.GPUDbgBuffer);
        end;
      finally
        FindControlInputData.BitmapToSearchFor.Free;
        FindControlInputData.BitmapToSearchOn.Free;
      end;

      if FExtraLogging_FindControl then
      begin
        AddToLog('MatchSource: ' + MatchSource);
        AddToLog('DetailedMatchSource: ' + DetailedMatchSource);
      end;
    end; //WillMatchPrimitiveFiles
  finally
    if Result then
    begin
      CopyPartialResultsToFinalResult(ResultedControlArr, ResultedControlArr_Text);
      CopyPartialResultsToFinalResult(ResultedControlArr, ResultedControlArr_Bmp);
      CopyPartialResultsToFinalResult(ResultedControlArr, ResultedControlArr_Pmtv);

      if Length(ResultedControlArr) > 0 then
      begin
        UpdateActionVarValuesFromControl(ResultedControlArr[0]);
        frClickerActions.DebuggingInfoAvailable := True;

        if AFindSubControlOptions.GetAllControls then
        begin
          SetAllControl_Handles_FromResultedControlArr(ResultedControlArr, MatchSource, DetailedMatchSource);
          UpdateActionVarValuesFromResultedControlArr(ResultedControlArr);
        end;
      end;
    end;

    SetLength(ResultedControlArr, 0);
    SetLength(PartialResultedControlArr, 0);
    SetLength(ResultedControlArr_Text, 0);
    SetLength(ResultedControlArr_Bmp, 0);
    SetLength(ResultedControlArr_Pmtv, 0);
  end;
end;


function TActionExecution.ExecuteFindControlActionWithTimeout(var AFindControlOptions: TClkFindControlOptions; var AActionOptions: TClkActionOptions): Boolean; //returns True if found
var
  tk, CurrentActionElapsedTime, OutsideTickCount: QWord;
  AttemptCount: Integer;
  LogMsg: string;
begin
  tk := GetTickCount64;
  frClickerActions.prbTimeout.Max := AActionOptions.ActionTimeout;
  frClickerActions.prbTimeout.Position := 0;
  Result := False;
  AttemptCount := 0;

  if AFindControlOptions.PrecisionTimeout then
    OutsideTickCount := tk
  else
    OutsideTickCount := 0; //0 means "do not use PrecisionTimeout".

  repeat
    {$IFDEF Windows}
      if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) and (GetAsyncKeyState(VK_F2) < 0) then
    {$ELSE}
      if (GetKeyState(VK_CONTROL) < 0) and (GetKeyState(VK_SHIFT) < 0) and (GetKeyState(VK_F2) < 0) then
    {$ENDIF}
    begin
      if FStopAllActionsOnDemandFromParent <> nil then
        FStopAllActionsOnDemandFromParent^ := True;

      if FStopAllActionsOnDemand <> nil then
        FStopAllActionsOnDemand^ := True;
    end;

    if FStopAllActionsOnDemandFromParent <> nil then
      if FStopAllActionsOnDemandFromParent^ then
        if FStopAllActionsOnDemand <> nil then
          FStopAllActionsOnDemand^ := True;

    if FStopAllActionsOnDemand^ then
    begin
      PrependErrorMessageToActionVar('Stopped by user at "' + AActionOptions.ActionName + '" in ' + FSelfTemplateFileName^ + '  ');
      Break;
    end;

    try
      Result := ExecuteFindControlAction(AFindControlOptions, AActionOptions, OutsideTickCount);
      //AddToLog('Find(Sub)Control result at attempt no #' + IntToStr(AttemptCount) + ': ' + BoolToStr(Result, 'True', 'False'));
    except
      on E: EBmpMatchTimeout do
      begin
        Result := False;
        AddToLog(E.Message);
      end;
    end;

    if AFindControlOptions.WaitForControlToGoAway then  //the control should not be found
      Result := not Result;

    if not Result and AFindControlOptions.AllowToFail then
      Break; //do not set result to True, because it is required to be detected where ExecuteFindControlActionWithTimeout is called

    if Result then
      Break;

    CurrentActionElapsedTime := GetTickCount64 - tk;
    frClickerActions.prbTimeout.Position := CurrentActionElapsedTime;
    Application.ProcessMessages;

    Inc(AttemptCount);

    if (frClickerActions.prbTimeout.Max > 0) and (frClickerActions.prbTimeout.Position >= frClickerActions.prbTimeout.Max) then
    begin
      LogMsg := 'Timeout at "' + AActionOptions.ActionName +
                '" in ' + FSelfTemplateFileName^ +
                '  ActionTimeout=' + IntToStr(AActionOptions.ActionTimeout) +
                '  Duration=' + IntToStr(CurrentActionElapsedTime) +
                '  AttemptCount=' + IntToStr(AttemptCount);

      if FExtraLogging_FindControl then
        LogMsg := LogMsg + '  Search: ' +
                           '  $Control_Left$=' + EvaluateReplacements('$Control_Left$') + //same as "global...", but are required here, to be displayed in caller template log
                           '  $Control_Top$=' + EvaluateReplacements('$Control_Top$') +
                           '  $Control_Right$=' + EvaluateReplacements('$Control_Right$') +
                           '  $Control_Bottom$=' + EvaluateReplacements('$Control_Bottom$') +
                           '  $Control_Text$="' + EvaluateReplacements('$Control_Text$') + '"' +
                           '  $Control_Class$="' + EvaluateReplacements('$Control_Class$') + '"' +
                           '  SearchedText="' + EvaluateReplacements(AFindControlOptions.MatchText) + '"' +
                           '  SearchedClass="' + EvaluateReplacements(AFindControlOptions.MatchClassName) + '"' +
                           '  ';
      PrependErrorMessageToActionVar(LogMsg);
      Break;
    end;

    Sleep(2);
  until False;

  frClickerActions.prbTimeout.Position := 0;
end;


function TActionExecution.ExecuteFindSubControlActionWithTimeout(var AFindSubControlOptions: TClkFindSubControlOptions; var AActionOptions: TClkActionOptions): Boolean; //returns True if found
var
  tk, CurrentActionElapsedTime, OutsideTickCount: QWord;
  AttemptCount: Integer;
  LogMsg: string;
begin
  tk := GetTickCount64;
  frClickerActions.prbTimeout.Max := AActionOptions.ActionTimeout;
  frClickerActions.prbTimeout.Position := 0;
  Result := False;
  AttemptCount := 0;

  if AFindSubControlOptions.PrecisionTimeout then
    OutsideTickCount := tk
  else
    OutsideTickCount := 0; //0 means "do not use PrecisionTimeout".

  repeat
    {$IFDEF Windows}
      if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) and (GetAsyncKeyState(VK_F2) < 0) then
    {$ELSE}
      if (GetKeyState(VK_CONTROL) < 0) and (GetKeyState(VK_SHIFT) < 0) and (GetKeyState(VK_F2) < 0) then
    {$ENDIF}
    begin
      if FStopAllActionsOnDemandFromParent <> nil then
        FStopAllActionsOnDemandFromParent^ := True;

      if FStopAllActionsOnDemand <> nil then
        FStopAllActionsOnDemand^ := True;
    end;

    if FStopAllActionsOnDemandFromParent <> nil then
      if FStopAllActionsOnDemandFromParent^ then
        if FStopAllActionsOnDemand <> nil then
          FStopAllActionsOnDemand^ := True;

    if FStopAllActionsOnDemand^ then
    begin
      PrependErrorMessageToActionVar('Stopped by user at "' + AActionOptions.ActionName + '" in ' + FSelfTemplateFileName^ + '  ');
      Break;
    end;

    try
      Result := ExecuteFindSubControlAction(AFindSubControlOptions, AActionOptions, OutsideTickCount);
      //AddToLog('Find(Sub)Control result at attempt no #' + IntToStr(AttemptCount) + ': ' + BoolToStr(Result, 'True', 'False'));
    except
      on E: EBmpMatchTimeout do
      begin
        Result := False;
        AddToLog(E.Message);
      end;
    end;

    if AFindSubControlOptions.WaitForControlToGoAway then  //the control should not be found
      Result := not Result;

    if not Result and AFindSubControlOptions.AllowToFail then
      Break; //do not set result to True, because it is required to be detected where ExecuteFindControlActionWithTimeout is called

    if Result then
      Break;

    CurrentActionElapsedTime := GetTickCount64 - tk;
    frClickerActions.prbTimeout.Position := CurrentActionElapsedTime;
    Application.ProcessMessages;

    Inc(AttemptCount);

    if (frClickerActions.prbTimeout.Max > 0) and (frClickerActions.prbTimeout.Position >= frClickerActions.prbTimeout.Max) then
    begin
      LogMsg := 'Timeout at "' + AActionOptions.ActionName +
                '" in ' + FSelfTemplateFileName^ +
                '  ActionTimeout=' + IntToStr(AActionOptions.ActionTimeout) +
                '  Duration=' + IntToStr(CurrentActionElapsedTime) +
                '  AttemptCount=' + IntToStr(AttemptCount);

      if FExtraLogging_FindControl then
        LogMsg := LogMsg + '  Search: ' +
                           '  $Control_Left$=' + EvaluateReplacements('$Control_Left$') + //same as "global...", but are required here, to be displayed in caller template log
                           '  $Control_Top$=' + EvaluateReplacements('$Control_Top$') +
                           '  $Control_Right$=' + EvaluateReplacements('$Control_Right$') +
                           '  $Control_Bottom$=' + EvaluateReplacements('$Control_Bottom$') +
                           '  $Control_Text$="' + EvaluateReplacements('$Control_Text$') + '"' +
                           '  SearchedText="' + EvaluateReplacements(AFindSubControlOptions.MatchText) + '"' +
                           '  MatchBitmapAlgorithm="' + CMatchBitmapAlgorithmStr[AFindSubControlOptions.MatchBitmapAlgorithm] + '"' +
                           '  ';
      PrependErrorMessageToActionVar(LogMsg);

      if (AFindSubControlOptions.ImageSourceFileNameLocation = isflMem) and (AFindSubControlOptions.ImageSource = isFile) then
        if (AFindSubControlOptions.InitialRectangle.Left = '$Control_Left$') and (AFindSubControlOptions.InitialRectangle.Top = '$Control_Top$') then
        begin
          AddToLog('Warning: Default search area detected for a non-screenshot background. It may be possible that this is why the control is not found.');
          AddToLog('A non-screenshot background may require absolute values, or control depended size like 0, 0, $Control_Width$, $Control_Height$, for Left, Top, Right, Bottom, and 0, 0, 0, 0, for offsets.');
          AddToLog('The above values can be adjusted to a smaller area.');
        end;

      Break;
    end;

    Sleep(2);
  until False;

  frClickerActions.prbTimeout.Position := 0;
end;


function TActionExecution.ExecuteSetControlTextAction(var ASetTextOptions: TClkSetTextOptions): Boolean;
  function PosKeyReplacement(AText: string; out AFoundReplacement: string; out AKeyCode: Word; out AKeyAction: TKeyAction): Integer;
  var
    Start, Stop: Integer;
    s: string;
    i: Integer;
  begin
    Result := 0;

    Start := Pos('$', AText);
    if Start = 0 then
      Exit;

    s := Copy(AText, Start + 1, MaxInt);
    Stop := Pos('$', s);
    if Stop = 0 then
      Exit;

    s := Copy(s, 1, Stop - 1); // s should not contain any '$' at this point

    for i := 0 to CSpecialKeyCount - 1 do
      if s = CSpecialKeyReplacements[i] then
      begin
        AFoundReplacement := '$' + s + '$';
        AKeyCode := CSpecialKeyCodes[i];
        AKeyAction := CSpecialKeyActions[i];
        Result := Start;
      end;
  end;


  procedure ParseSpecialKeys(var ATextToSend: string; var AKeyTypes: TKeyTypeArr);
  var
    i, PosRpl: Integer;
    FoundReplacement: string;
    KeyCode: Word;
    KeyAction: TKeyAction;
  begin
    SetLength(AKeyTypes, Length(ATextToSend));

    for i := 0 to Length(AKeyTypes) - 1 do
      AKeyTypes[i].IsSpecial := False;

    repeat
      PosRpl := PosKeyReplacement(ATextToSend, FoundReplacement, KeyCode, KeyAction);

      if PosRpl > 0 then
      begin
        Delete(ATextToSend, PosRpl, Length(FoundReplacement) - 1);
        ATextToSend[PosRpl] := #14; // 
        Delete(AKeyTypes, PosRpl - 1, Length(FoundReplacement) - 1);  //PosRpl - 1, because dynamic arrays are 0-indexed

        AKeyTypes[PosRpl - 1].IsSpecial := True;
        AKeyTypes[PosRpl - 1].KeyCode := KeyCode;
        AKeyTypes[PosRpl - 1].Action := KeyAction;
      end
      else
        Break;
    until False;
  end;

var
  Control_Handle: THandle;
  i, k, Idx: Integer;
  TextToSend: string;
  //s: string; //for debugging
  {$IFDEF Windows}
    KeyStrokes: array of TINPUT;
  {$ELSE}
    KeyStrokes: array of Pointer;
  {$ENDIF}
  KeyTypes: TKeyTypeArr;
  Err: Integer;
  ErrStr: string;
  DelayBetweenKeyStrokesInt: Integer;
  Count: Integer;
  GeneratedException: Boolean;
  tk, tk2: QWord;
  Pos14_InInitialText: Boolean;
begin
  Result := True;

  Control_Handle := StrToIntDef(GetActionVarValue('$Control_Handle$'), 0);
  TextToSend := EvaluateReplacements(ASetTextOptions.Text);
  TextToSend := EvaluateHTTP(TextToSend, GeneratedException);

  Pos14_InInitialText := Pos(#14, TextToSend) > 0;
  ParseSpecialKeys(TextToSend, KeyTypes);

  if ASetTextOptions.ControlType in [stEditBox, stComboBox] then
    if (Pos(#14, TextToSend) > 0) and not Pos14_InInitialText then
      AddToLog('[SetText] Warning: Special keys are not supported in "EditBox" and "ComboBox" mode. Please use "Keystrokes" mode.');

  if Length(TextToSend) <> Length(KeyTypes) then
  begin
    ErrStr := 'SetText parsing error: ' + IntToStr(Length(TextToSend)) + ' / ' + IntToStr(Length(KeyTypes)) + ' "' + TextToSend + '"';
    AddToLog(ErrStr);
    SetActionVarValue('$ExecAction_Err$', ErrStr);
    Result := False;
  end;

  //AddToLog('[SetText]: "' + TextToSend + '"');    //for debugging
  //s := '';
  //for i := 0 to Length(KeyTypes) - 1 do
  //  s := s + IntToStr(Ord(KeyTypes[i].IsSpecial));
  //AddToLog('[SetText]: "' + s + '"');

  Count := Min(65535, Max(0, StrToIntDef(EvaluateReplacements(ASetTextOptions.Count), 1)));

  if ASetTextOptions.ControlType = stKeystrokes then
    DelayBetweenKeyStrokesInt := StrToIntDef(EvaluateReplacements(ASetTextOptions.DelayBetweenKeyStrokes), 0)
  else
    DelayBetweenKeyStrokesInt := 0;

  for k := 1 to Count do
  begin
    case ASetTextOptions.ControlType of
      stEditBox: SetControlText(Control_Handle, TextToSend);

      stComboBox: SelectComboBoxItem(Control_Handle, 0, TextToSend);

      stKeystrokes:
      begin
        {$IFDEF Windows}
          SetLength(KeyStrokes, Length(TextToSend) shl 1);
          try
            for i := 0 to Length(TextToSend) - 1 do   //string len, not array len
            begin
              Idx := i shl 1;
              KeyStrokes[Idx]._Type := INPUT_KEYBOARD; //not sure if needed
              KeyStrokes[Idx].ki.wVk := 0;  //If this is set to Ord(TextToSend[i + 1]), instead of 0, then capital letters are types as lower case (probably, because it expects the Shift key to come as a separate state). This causes VBox to block its keyboard input (probably some bad state).
              KeyStrokes[Idx].ki.wScan := Ord(TextToSend[i + 1]);
              KeyStrokes[Idx].ki.dwFlags := KEYEVENTF_UNICODE; //0;
              KeyStrokes[Idx].ki.Time := 0;
              KeyStrokes[Idx].ki.ExtraInfo := 0;

              KeyStrokes[Idx + 1]._Type := INPUT_KEYBOARD; //not sure if needed
              KeyStrokes[Idx + 1].ki.wVk := 0;   //this can be set to Ord(TextToSend[i + 1]);  //it looks like a valid combination to avoid blocking VBox input
              KeyStrokes[Idx + 1].ki.wScan := Ord(TextToSend[i + 1]);
              KeyStrokes[Idx + 1].ki.dwFlags := KEYEVENTF_UNICODE or KEYEVENTF_KEYUP;
              KeyStrokes[Idx + 1].ki.Time := 0;
              KeyStrokes[Idx + 1].ki.ExtraInfo := 0;

              if KeyTypes[i].IsSpecial then
              begin
                KeyStrokes[Idx].ki.wScan := 0;
                KeyStrokes[Idx + 1].ki.wScan := 0;

                if KeyTypes[i].Action in [kaDownUp, kaDown] then
                begin
                  KeyStrokes[Idx].ki.wVk := KeyTypes[i].KeyCode;
                  KeyStrokes[Idx].ki.dwFlags := 0;
                end;

                if KeyTypes[i].Action in [kaDownUp, kaUp] then
                begin
                  KeyStrokes[Idx + 1].ki.wVk := KeyTypes[i].KeyCode;
                  KeyStrokes[Idx + 1].ki.dwFlags := 0 or KEYEVENTF_KEYUP;
                end;
              end;
            end;

            SetLastError(0);
            if DelayBetweenKeyStrokesInt = 0 then
            begin
              if Integer(SendInput(Length(KeyStrokes), @KeyStrokes[0], SizeOf(TINPUT))) <> Length(KeyStrokes) then
              begin
                Err := GetLastOSError;
                ErrStr := 'KeyStrokes error: ' + IntToStr(Err) + '  ' + SysErrorMessage(GetLastOSError) + '  Keystrokes count: ' + IntToStr(Length(KeyStrokes));
                SetActionVarValue('$ExecAction_Err$', ErrStr);
                Result := False;
              end;
            end
            else
            begin
              for i := 0 to Length(TextToSend) - 1 do
              begin
                //if KeyTypes[i].Action = kaDown then
                //  keybd_event(KeyTypes[i].KeyCode, 0, 0, 0);    //to be enabled if SendInput doesn't properly simulate key combinations
                try
                  if Integer(SendInput(2, @KeyStrokes[i shl 1], SizeOf(TINPUT))) <> 2 then
                  begin
                    Err := GetLastOSError;
                    ErrStr := 'KeyStrokes error: ' + IntToStr(Err) + '  ' + SysErrorMessage(GetLastOSError) + '  Keystrokes count: ' + IntToStr(Length(KeyStrokes));
                    SetActionVarValue('$ExecAction_Err$', ErrStr);
                    Result := False;
                  end;
                finally
                  //if KeyTypes[i].Action = kaUp then
                  //  keybd_event(KeyTypes[i].KeyCode, 0, KEYEVENTF_KEYUP, 0);  //to be enabled if SendInput doesn't properly simulate key combinations
                end;

                tk2 := GetTickCount64;
                repeat
                  //Sleep(1);
                  Application.ProcessMessages;

                  if CheckManualStopCondition then
                  begin
                    Result := False;
                    Break;     //break for k
                  end;
                until GetTickCount64 - tk2 >= DelayBetweenKeyStrokesInt;

                if CheckManualStopCondition then
                begin
                  Result := False;
                  Break;       //break outer for
                end;
              end;
            end; //using delays
          finally
            SetLength(KeyStrokes, 0);
          end;
        {$ENDIF}
      end;
    end; //case

    Application.ProcessMessages;
    if CheckManualStopCondition then
    begin
      Result := False;
      Break;     //break for k
    end;

    tk := GetTickCount64;
    repeat
      //Sleep(1);
      Application.ProcessMessages;

      if CheckManualStopCondition then
      begin
        Result := False;
        Break;     //break for k
      end;
    until GetTickCount64 - tk >= DelayBetweenKeyStrokesInt;   //it's ok to have DelayBetweenKeyStrokesInt between iterations
  end; //for k
end;


function TActionExecution.CheckManualStopCondition: Boolean;
begin
  Result := False;

  {$IFDEF Windows}
    if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) and (GetAsyncKeyState(VK_F2) < 0) then
  {$ELSE}
    if (GetKeyState(VK_CONTROL) < 0) and (GetKeyState(VK_SHIFT) < 0) and (GetKeyState(VK_F2) < 0) then
  {$ENDIF}
  begin
    Result := True;

    if FStopAllActionsOnDemandFromParent <> nil then
      FStopAllActionsOnDemandFromParent^ := True;

    if FStopAllActionsOnDemand <> nil then
      FStopAllActionsOnDemand^ := True;
  end;

  if FStopAllActionsOnDemandFromParent <> nil then
    if FStopAllActionsOnDemandFromParent^ then
      if FStopAllActionsOnDemand <> nil then
        FStopAllActionsOnDemand^ := True;
end;


function TActionExecution.ExecuteCallTemplateAction(var ACallTemplateOptions: TClkCallTemplateOptions; IsDebugging, AShouldStopAtBreakPoint: Boolean): Boolean;
var
  i: Integer;
  CustomVars: TStringList;
  KeyName, KeyValue, RowString: string;
  Fnm, TemplateDir, LocalSelfTemplateFileName: string;
  GeneratedException: Boolean;
begin
  Result := False;
  if not Assigned(FOnCallTemplate) then
  begin
    AppendErrorMessageToActionVar('OnCallTemplate not assigned');
    Exit;
  end;

  CustomVars := TStringList.Create;
  try
    CustomVars.LineBreak := #13#10;
    CustomVars.Text := FastReplace_45ToReturn(ACallTemplateOptions.ListOfCustomVarsAndValues);

    for i := 0 to CustomVars.Count - 1 do
    begin
      try
        RowString := CustomVars.Strings[i];
        KeyName := Copy(RowString, 1, Pos('=', RowString) - 1);
        KeyValue := Copy(RowString, Pos('=', RowString) + 1, MaxInt);
        KeyValue := EvaluateHTTP(KeyValue, GeneratedException);

        if ACallTemplateOptions.EvaluateBeforeCalling then
          SetActionVarValue(KeyName, EvaluateReplacements(KeyValue))
        else
          SetActionVarValue(KeyName, KeyValue);
      except
        //who knows...
      end;
    end;
  finally
    CustomVars.Free;
  end;

  if ACallTemplateOptions.CallOnlyIfCondition then
  begin
    MessageBox(Application.MainForm.Handle, PChar('Using this condition mechanism is deprecated. Please move this condition to the "Condition" tab.' + #13#10 + 'Filename: ' + FSelfTemplateFileName^), PChar(Application.Title), MB_ICONWARNING);

    KeyName := ACallTemplateOptions.CallOnlyIfConditionVarName;
    KeyValue := ACallTemplateOptions.CallOnlyIfConditionVarValue;

    if GetActionVarValue(KeyName) <> KeyValue then
    begin
      Result := True;  //allow further execution
      SetActionVarValue('$ExecAction_Err$', 'Condition not met: ' + GetActionVarValue(KeyName) + ' <> ' + KeyValue);
      Exit;
    end;
  end;

  Fnm := ACallTemplateOptions.TemplateFileName;
  TemplateDir := '';
  if FSelfTemplateFileName = nil then
  begin
    TemplateDir := 'FSelfTemplateFileName not set in CallTemplate.';
    LocalSelfTemplateFileName := '';
  end
  else
  begin
    TemplateDir := ExtractFileDir(FSelfTemplateFileName^);
    Fnm := StringReplace(Fnm, '$SelfTemplateDir$', TemplateDir, [rfReplaceAll]);
    Fnm := StringReplace(Fnm, '$TemplateDir$', FFullTemplatesDir^, [rfReplaceAll]);
    LocalSelfTemplateFileName := FSelfTemplateFileName^;
  end;

  // the FileExists verification has to be done after checking CallOnlyIfCondition, to allow failing the action based on condition

  Fnm := StringReplace(Fnm, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);
  Fnm := EvaluateReplacements(Fnm);  //this call has to stay here, after the $SelfTemplateDir$, $TemplateDir$ and $AppDir$ replacements, because $SelfTemplateDir$ will be evaluated to ''.

  if not FFileLocationOfDepsIsMem^ then
    if ExtractFileName(Fnm) = Fnm then  //Fnm does not contain a path
    begin
      if FFullTemplatesDir = nil then
        raise Exception.Create('FFullTemplatesDir is not assigned.');

      Fnm := FFullTemplatesDir^ + '\' + Fnm;
    end;

  if FOwnerFrame = nil then
    raise Exception.Create('FOwnerFrame is not assigned.');

  AddToLog('Calling template: "' + Fnm + '",  from "' + LocalSelfTemplateFileName + '".');
  //do not verify here if the file exists or not, because this verification is done by FOnCallTemplate, both for disk and in-mem FS
  Result := FOnCallTemplate(FOwnerFrame, Fnm, FClickerVars, frClickerActions.imgDebugBmp.Picture.Bitmap, frClickerActions.imgDebugGrid, IsDebugging, AShouldStopAtBreakPoint, FStackLevel^, FExecutesRemotely^);

  if GetActionVarValue('$ExecAction_Err$') <> '' then   ////////////////// ToDo:  improve the error logging
    AddToLog(DateTimeToStr(Now) + '  ' + GetActionVarValue('$ExecAction_Err$'));
end;


function TActionExecution.ExecuteLoopedCallTemplateAction(var ACallTemplateOptions: TClkCallTemplateOptions; IsDebugging, AShouldStopAtBreakPoint: Boolean): Boolean;
var
  i: Integer;
  StartValue, StopValue: Integer;
  TempACallTemplateOptions: TClkCallTemplateOptions;
begin
  if not ACallTemplateOptions.CallTemplateLoop.Enabled then
    Result := ExecuteCallTemplateAction(ACallTemplateOptions, IsDebugging, AShouldStopAtBreakPoint)
  else
  begin
    StartValue := StrToIntDef(EvaluateReplacements(ACallTemplateOptions.CallTemplateLoop.InitValue), 0);
    StopValue := StrToIntDef(EvaluateReplacements(ACallTemplateOptions.CallTemplateLoop.EndValue), 0);

    Result := True;
    case ACallTemplateOptions.CallTemplateLoop.Direction of
      ldInc:
      begin
        for i := StartValue to StopValue do
        begin
          SetActionVarValue(ACallTemplateOptions.CallTemplateLoop.Counter, IntToStr(i));

          if ACallTemplateOptions.CallTemplateLoop.BreakCondition <> '' then
            if ACallTemplateOptions.CallTemplateLoop.EvalBreakPosition = lebpBeforeContent then
              if EvaluateActionCondition(ACallTemplateOptions.CallTemplateLoop.BreakCondition, EvaluateReplacements) then
                Break;

          Result := Result and ExecuteCallTemplateAction(ACallTemplateOptions, IsDebugging, AShouldStopAtBreakPoint);

          if ACallTemplateOptions.CallTemplateLoop.BreakCondition <> '' then
            if ACallTemplateOptions.CallTemplateLoop.EvalBreakPosition = lebpAfterContent then
              if EvaluateActionCondition(ACallTemplateOptions.CallTemplateLoop.BreakCondition, EvaluateReplacements) then
                Break;

          CheckManualStopCondition;
          if FStopAllActionsOnDemand <> nil then
            if FStopAllActionsOnDemand^ then
              Break;
        end;
      end;

      ldDec:
      begin
        for i := StartValue downto StopValue do
        begin
          SetActionVarValue(ACallTemplateOptions.CallTemplateLoop.Counter, IntToStr(i));

          if ACallTemplateOptions.CallTemplateLoop.EvalBreakPosition = lebpBeforeContent then
            if EvaluateActionCondition(ACallTemplateOptions.CallTemplateLoop.BreakCondition, EvaluateReplacements) then
              Break;

          Result := Result and ExecuteCallTemplateAction(ACallTemplateOptions, IsDebugging, AShouldStopAtBreakPoint);

          if ACallTemplateOptions.CallTemplateLoop.EvalBreakPosition = lebpBeforeContent then
            if EvaluateActionCondition(ACallTemplateOptions.CallTemplateLoop.BreakCondition, EvaluateReplacements) then
              Break;

          CheckManualStopCondition;
          if FStopAllActionsOnDemand <> nil then
            if FStopAllActionsOnDemand^ then
              Break;
        end;
      end;

      ldAuto:
      begin
        TempACallTemplateOptions := ACallTemplateOptions;
        if StartValue < StopValue then
          TempACallTemplateOptions.CallTemplateLoop.Direction := ldInc
        else
          TempACallTemplateOptions.CallTemplateLoop.Direction := ldDec;

        Result := ExecuteLoopedCallTemplateAction(TempACallTemplateOptions, IsDebugging, AShouldStopAtBreakPoint)
      end;

      else
      begin
        Result := False;
        SetActionVarValue('$ExecAction_Err$', 'Unknown loop direction');
      end;
    end;
  end;
end;


function TActionExecution.ExecuteSleepAction(var ASleepOptions: TClkSleepOptions; var AActionOptions: TClkActionOptions): Boolean;
var
  ValueInt: Integer;
  tk1, tk2, ElapsedTime, RemainingTime: Int64;
begin
  //eventually, implement some fast-forward option, to skip the sleep action while debugging
  ValueInt := StrToIntDef(EvaluateReplacements(ASleepOptions.Value), -1);

  if ValueInt < 0 then
  begin
    Result := False;
    SetActionVarValue('$ExecAction_Err$', 'Invalid sleep value: ' + EvaluateReplacements(ASleepOptions.Value));
    Exit;
  end;

  Result := True;

  if ValueInt < 1 then
    Exit;

  DoOnSetEditorSleepProgressBarMax(ValueInt);

  tk1 := GetTickCount64;
  ElapsedTime := 0;
  repeat
    tk2 := GetTickCount64;

    if tk2 < tk1 then  // In case of a wrap around, call this function again. It will get it right, at the cost of an additional delay.
    begin
      ExecuteSleepAction(ASleepOptions, AActionOptions);
      Break;
    end;

    {$IFDEF Windows}
      if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) and (GetAsyncKeyState(VK_F2) < 0) then
    {$ELSE}
      if (GetKeyState(VK_CONTROL) < 0) and (GetKeyState(VK_SHIFT) < 0) and (GetKeyState(VK_F2) < 0) then
    {$ENDIF}
    begin
      if FStopAllActionsOnDemandFromParent <> nil then
        FStopAllActionsOnDemandFromParent^ := True;

      if FStopAllActionsOnDemand <> nil then
        FStopAllActionsOnDemand^ := True;
    end;

    if FStopAllActionsOnDemandFromParent <> nil then
      if FStopAllActionsOnDemandFromParent^ then
        if FStopAllActionsOnDemand <> nil then
          FStopAllActionsOnDemand^ := True;

    if FStopAllActionsOnDemand^ then
    begin
      PrependErrorMessageToActionVar('Stopped by user at "' + AActionOptions.ActionName + '" in ' + FSelfTemplateFileName^ + '  ');
      Result := False;
      Break;
    end;

    ElapsedTime := tk2 - tk1;
    RemainingTime := ValueInt - ElapsedTime;

    DoOnSetEditorSleepInfo('Elapsed Time [ms]: ' + IntToStr(ElapsedTime), 'Remaining Time [ms]: ' + IntToStr(RemainingTime));
    DoOnSetEditorSleepProgressBarPosition(ElapsedTime);

    Application.ProcessMessages;
    Sleep(1);
  until ElapsedTime >= ValueInt;
end;


function TActionExecution.ExecuteSetVarAction(var ASetVarOptions: TClkSetVarOptions): Boolean;
  function GetHistogramResult(var AHistogramResult: TIntArr; ACount: Integer = 0): string;
  var
    i, CountItems: Integer;
  begin
    Result := '';
    if ACount <= 0 then
      CountItems := Length(AHistogramResult)
    else
      CountItems := Min(ACount, Length(AHistogramResult));

    for i := 0 to CountItems - 1 do
      Result := Result + IntToHex(AHistogramResult[i], 6) + #4#5;
  end;

  function GetHistogramColorCountsResult(var AHistogramResult: TIntArr; ACount: Integer = 0): string;
  var
    i, CountItems: Integer;
  begin
    Result := '';
    if ACount <= 0 then
      CountItems := Length(AHistogramResult)
    else
      CountItems := Min(ACount, Length(AHistogramResult));

    for i := 0 to CountItems - 1 do
      Result := Result + IntToStr(AHistogramResult[i]) + #4#5;
  end;

  procedure GetTreeArgsFromArgsStr(AFuncArgs: string; out ATreePath: string; out AStep: Integer; out AUseMouseSwipe: Boolean);
  var
    ListOfFuncArgs: TStringList;
    UseMouseSwipeStr: string;
  begin
    ATreePath := '$TemplateDir$\DefaultTree.tree';
    AStep := 1;
    AUseMouseSwipe := False;

    ListOfFuncArgs := TStringList.Create;
    try
      ListOfFuncArgs.LineBreak := #13#10;
      ListOfFuncArgs.Text := StringReplace(AFuncArgs, ',', #13#10, [rfReplaceAll]);

      if ListOfFuncArgs.Count > 0 then
        ATreePath := Trim(ListOfFuncArgs.Strings[0]);

      ATreePath := EvaluateReplacements(ATreePath);
      ATreePath := DoOnResolveTemplatePath(ATreePath); //outside of if statement, because it is initialized with $TemplateDir$

      if ListOfFuncArgs.Count > 1 then
        AStep := StrToIntDef(EvaluateReplacements(Trim(ListOfFuncArgs.Strings[1])), 1);

      if ListOfFuncArgs.Count > 2 then
      begin
        UseMouseSwipeStr := Trim(ListOfFuncArgs.Strings[2]);
        AUseMouseSwipe := (UseMouseSwipeStr = '1') or (UpperCase(UseMouseSwipeStr) = 'TRUE');
      end;
    finally
      ListOfFuncArgs.Free;
    end;
  end;


  procedure GetWinInterpOptionArgsFromArgsStr(AFuncArgs: string; out ASetting: string; out AValue: string);
  var
    ListOfFuncArgs: TStringList;
  begin
    ASetting := 'None';
    AValue := '';

    ListOfFuncArgs := TStringList.Create;
    try
      ListOfFuncArgs.LineBreak := #13#10;
      ListOfFuncArgs.Text := StringReplace(AFuncArgs, ',', #13#10, [rfReplaceAll]);

      if ListOfFuncArgs.Count > 0 then
        ASetting := EvaluateReplacements(Trim(ListOfFuncArgs.Strings[0]));

      if ListOfFuncArgs.Count > 1 then
        AValue := EvaluateReplacements(Trim(ListOfFuncArgs.Strings[1]));
    finally
      ListOfFuncArgs.Free;
    end;
  end;

var
  TempListOfSetVarNames: TStringList;
  TempListOfSetVarValues: TStringList;
  TempListOfSetVarEvalBefore: TStringList;
  i, j: Integer;
  VarName, VarValue, FuncArgs: string;
  RenderBmpExternallyResult: string;
  ListOfSelfHandles, ListOfConsoleItems: TStringList;
  GeneratedException: Boolean;
  TempBmp: TBitmap;
  HistogramResult, HistogramColorCountsResult: TIntArr;
  HistItemCount: Integer;
  tk: QWord;
  TemplateDir: string;
  TreePath, WinInterpOptionName, WinInterpOptionValue: string;
  TreeStep: Integer;
  TreeUseMouseSwipe: Boolean;
  ConsoleArgs: string;
begin
  Result := False;
  TempListOfSetVarNames := TStringList.Create;
  TempListOfSetVarValues := TStringList.Create;
  TempListOfSetVarEvalBefore := TStringList.Create;
  try
    TempListOfSetVarNames.LineBreak := #13#10;
    TempListOfSetVarValues.LineBreak := #13#10;
    TempListOfSetVarEvalBefore.LineBreak := #13#10;
    TempListOfSetVarNames.Text := ASetVarOptions.ListOfVarNames;
    TempListOfSetVarValues.Text := ASetVarOptions.ListOfVarValues;
    TempListOfSetVarEvalBefore.Text := ASetVarOptions.ListOfVarEvalBefore;

    if (TempListOfSetVarNames.Count = 1) and (TempListOfSetVarValues.Count = 0) then //the only variable, passed here, is set to ''
      TempListOfSetVarValues.Add('');

    if TempListOfSetVarNames.Count <> TempListOfSetVarValues.Count then
    begin
      SetActionVarValue('$ExecAction_Err$', 'SetVar: The list of var names has a different length than the list of var values.');
      Exit;
    end;

    if TempListOfSetVarEvalBefore.Count <> TempListOfSetVarNames.Count then
    begin
      SetActionVarValue('$ExecAction_Err$', 'SetVar: The list of var eval infos has a different length than the list of var names.');
      Exit;
    end;

    for i := 0 to TempListOfSetVarNames.Count - 1 do
    begin
      VarName := TempListOfSetVarNames.Strings[i];
      VarValue := TempListOfSetVarValues.Strings[i];

      if (Pos('$Exit(', VarName) = 1) and (VarName[Length(VarName)] = '$') and (VarName[Length(VarName) - 1] = ')') then
      begin
        VarValue := Copy(VarName, Pos('(', VarName) + 1, MaxInt);
        VarValue := Copy(VarValue, 1, Length(VarValue) - 2);
        SetActionVarValue('$ExecAction_Err$', 'Terminating template execution on request.');

        if TempListOfSetVarEvalBefore.Strings[i] = '1' then
          VarValue := EvaluateReplacements(VarValue);

        SetActionVarValue('$ExitCode$', VarValue);

        Result := VarValue = '0';
        Exit;
      end;

      if VarName = '$GetSelfHandles()$' then
      begin
        ListOfSelfHandles := TStringList.Create;
        try
          ListOfSelfHandles.LineBreak := #13#10;
          DoOnGetSelfHandles(ListOfSelfHandles);

          for j := 0 to ListOfSelfHandles.Count - 1 do
            SetActionVarValue('$' + ListOfSelfHandles.Names[j] + '$', ListOfSelfHandles.ValueFromIndex[j]);
        finally
          ListOfSelfHandles.Free;
        end;

        Continue; //use this to prevent adding '$GetSelfHandles()$' as a variable
      end;

      if TempListOfSetVarEvalBefore.Strings[i] = '1' then
      begin
        TemplateDir := '';
        if FSelfTemplateFileName = nil then
          TemplateDir := 'FSelfTemplateFileName not set in SetVar.'
        else
        begin
          TemplateDir := ExtractFileDir(FSelfTemplateFileName^);
          VarValue := StringReplace(VarValue, '$SelfTemplateDir$', TemplateDir, [rfReplaceAll]);
          VarValue := StringReplace(VarValue, '$TemplateDir$', FFullTemplatesDir^, [rfReplaceAll]);
        end;

        VarValue := StringReplace(VarValue, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);

        VarValue := EvaluateReplacements(VarValue);
      end;

      VarValue := EvaluateHTTP(VarValue, GeneratedException);
      if GeneratedException then
        if ASetVarOptions.FailOnException then
        begin
          SetActionVarValue('$ExecAction_Err$', VarValue);
          Exit;
        end;

      if (Pos('$RenderBmpExternally(', VarName) = 1) and (VarName[Length(VarName)] = '$') and (VarName[Length(VarName) - 1] = ')') then
      begin
        RenderBmpExternallyResult := DoOnRenderBmpExternally(VarValue);
        SetActionVarValue('$ExternallyRenderedBmpResult$', RenderBmpExternallyResult);
        if Pos(CClientExceptionPrefix, RenderBmpExternallyResult) = 1 then
          if ASetVarOptions.FailOnException then
          begin
            SetActionVarValue('$ExecAction_Err$', RenderBmpExternallyResult);
            Exit;
          end;
      end;

      if (Pos('$GetActionProperties(', VarName) = 1) and (VarName[Length(VarName)] = '$') and (VarName[Length(VarName) - 1] = ')') then
        SetActionVarValue('$ActionPropertiesResult$', GetActionProperties(VarValue));

      if (Pos('$GetImageDimensions(', VarName) = 1) and (VarName[Length(VarName)] = '$') and (VarName[Length(VarName) - 1] = ')') then
      begin
        TempBmp := TBitmap.Create;
        try
          if DoOnLoadBitmap(TempBmp, VarValue) then
          begin
            SetActionVarValue('$ImageWidth$', IntToStr(TempBmp.Width));
            SetActionVarValue('$ImageHeight$', IntToStr(TempBmp.Height));
            Result := True;
          end
          else
            if ASetVarOptions.FailOnException then
            begin
              SetActionVarValue('$ExecAction_Err$', 'File not found: "' + VarValue + '".');
              Result := False;
              Exit;
            end;
        finally
          TempBmp.Free;
        end;
      end;

      if (Pos('$GetExternallyRenderedImageDimensions(', VarName) = 1) and (VarName[Length(VarName)] = '$') and (VarName[Length(VarName) - 1] = ')') then
      begin
        TempBmp := TBitmap.Create;
        try
          if DoOnLoadRenderedBitmap(TempBmp, VarValue) then
          begin
            SetActionVarValue('$ExtImageWidth$', IntToStr(TempBmp.Width));
            SetActionVarValue('$ExtImageHeight$', IntToStr(TempBmp.Height));
            Result := True;
          end
          else
            if ASetVarOptions.FailOnException then
            begin
              SetActionVarValue('$ExecAction_Err$', 'File not found: "' + VarValue + '".');
              Result := False;
              Exit;
            end;
        finally
          TempBmp.Free;
        end;
      end;

      if (Pos('$FullHistogramDisk(', VarName) = 1) and (VarName[Length(VarName)] = '$') and (VarName[Length(VarName) - 1] = ')') then
      begin
        tk := GetTickCount64;
        TempBmp := TBitmap.Create;
        try
          if DoOnLoadBitmap(TempBmp, VarValue) then
          begin
            GetHistogram(TempBmp, HistogramResult, HistogramColorCountsResult);
            SetActionVarValue('$HistogramResult$', GetHistogramResult(HistogramResult));
            SetActionVarValue('$HistogramColorCountsResult$', GetHistogramColorCountsResult(HistogramColorCountsResult));
            Result := True;
          end
          else
            if ASetVarOptions.FailOnException then
            begin
              SetActionVarValue('$ExecAction_Err$', 'File not found: "' + VarValue + '".');
              Result := False;
              Exit;
            end;
        finally
          TempBmp.Free;
          AddToLog('Histogram computed in ' + IntToStr(GetTickCount64 - tk) + 'ms.  Found ' + IntToStr(Length(HistogramResult)) + ' item(s).');
        end;
      end;

      if (Pos('$FullHistogramExternallyRendered(', VarName) = 1) and (VarName[Length(VarName)] = '$') and (VarName[Length(VarName) - 1] = ')') then
      begin
        tk := GetTickCount64;
        TempBmp := TBitmap.Create;
        try
          if DoOnLoadRenderedBitmap(TempBmp, VarValue) then
          begin
            GetHistogram(TempBmp, HistogramResult, HistogramColorCountsResult);
            SetActionVarValue('$HistogramResult$', GetHistogramResult(HistogramResult));
            SetActionVarValue('$HistogramColorCountsResult$', GetHistogramColorCountsResult(HistogramColorCountsResult));
            Result := True;
          end
          else
            if ASetVarOptions.FailOnException then
            begin
              SetActionVarValue('$ExecAction_Err$', 'File not found: "' + VarValue + '".');
              Result := False;
              Exit;
            end;
        finally
          TempBmp.Free;
          AddToLog('Histogram computed in ' + IntToStr(GetTickCount64 - tk) + 'ms.  Found ' + IntToStr(Length(HistogramResult)) + ' item(s).');
        end;
      end;

      if (Pos('$PartialHistogramDisk(', VarName) = 1) and (VarName[Length(VarName)] = '$') and (VarName[Length(VarName) - 1] = ')') then
      begin
        tk := GetTickCount64;
        TempBmp := TBitmap.Create;
        try
          if DoOnLoadBitmap(TempBmp, VarValue) then
          begin
            GetHistogram(TempBmp, HistogramResult, HistogramColorCountsResult);
            HistItemCount := StrToIntDef(GetActionVarValue('$HistogramItemCount$'), 10);
            SetActionVarValue('$HistogramResult$', GetHistogramResult(HistogramResult, HistItemCount));
            SetActionVarValue('$HistogramColorCountsResult$', GetHistogramColorCountsResult(HistogramColorCountsResult, HistItemCount));
            Result := True;
          end
          else
            if ASetVarOptions.FailOnException then
            begin
              SetActionVarValue('$ExecAction_Err$', 'File not found: "' + VarValue + '".');
              Result := False;
              Exit;
            end;
        finally
          TempBmp.Free;
          AddToLog('Histogram computed in ' + IntToStr(GetTickCount64 - tk) + 'ms.  Found ' + IntToStr(Length(HistogramResult)) + ' item(s).');
        end;
      end;

      if (Pos('$PartialHistogramExternallyRendered(', VarName) = 1) and (VarName[Length(VarName)] = '$') and (VarName[Length(VarName) - 1] = ')') then
      begin
        tk := GetTickCount64;
        TempBmp := TBitmap.Create;
        try
          if DoOnLoadRenderedBitmap(TempBmp, VarValue) then
          begin
            GetHistogram(TempBmp, HistogramResult, HistogramColorCountsResult);
            HistItemCount := StrToIntDef(GetActionVarValue('$HistogramItemCount$'), 10);
            SetActionVarValue('$HistogramResult$', GetHistogramResult(HistogramResult, HistItemCount));
            SetActionVarValue('$HistogramColorCountsResult$', GetHistogramColorCountsResult(HistogramColorCountsResult, HistItemCount));
            Result := True;
          end
          else
            if ASetVarOptions.FailOnException then
            begin
              SetActionVarValue('$ExecAction_Err$', 'File not found: "' + VarValue + '".');
              Result := False;
              Exit;
            end;
        finally
          TempBmp.Free;
          AddToLog('Histogram computed in ' + IntToStr(GetTickCount64 - tk) + 'ms.  Found ' + IntToStr(Length(HistogramResult)) + ' item(s).');
        end;
      end;

      if (Pos('$GenerateAndSaveTree(', VarName) = 1) and (VarName[Length(VarName)] = '$') and (VarName[Length(VarName) - 1] = ')') then
      begin
        FuncArgs := Copy(VarName, Pos('(', VarName) + 1, MaxInt);
        FuncArgs := Copy(FuncArgs, 1, Length(FuncArgs) - 2);
        GetTreeArgsFromArgsStr(FuncArgs, TreePath, TreeStep, TreeUseMouseSwipe);

        if DoOnGenerateAndSaveTreeWithWinInterp(StrToIntDef(EvaluateReplacements('$Control_Handle$'), 0), TreePath, TreeStep, TreeUseMouseSwipe) then   //path to .tree file
        begin
          //SetActionVarValue('$Tree$', VarValue);
          Result := True;
        end
        else
          if ASetVarOptions.FailOnException then
          begin
            SetActionVarValue('$ExecAction_Err$', 'File not found: "' + VarValue + '".');
            Result := False;
            Exit;
          end;

        VarName := ''; //Prevent creating a variable, named $GenerateAndSaveTree(...)$
      end;

      if (Pos('$SetWinInterpOption(', VarName) = 1) and (VarName[Length(VarName)] = '$') and (VarName[Length(VarName) - 1] = ')') then
      begin
        FuncArgs := Copy(VarName, Pos('(', VarName) + 1, MaxInt);
        FuncArgs := Copy(FuncArgs, 1, Length(FuncArgs) - 2);
        GetWinInterpOptionArgsFromArgsStr(FuncArgs, WinInterpOptionName, WinInterpOptionValue);

        try
          if DoOnSetWinInterpOption(WinInterpOptionName, WinInterpOptionValue) then   //use one of the CWinInterpOption_<Name> options from ClickeUtils.pas
          begin
            //SetActionVarValue('$Tree$', VarValue);
            Result := True;
          end
          else  //this is not an exception, it's just the result, set to False
            if ASetVarOptions.FailOnException then
            begin
              SetActionVarValue('$ExecAction_Err$', 'Unknown option: "' + WinInterpOptionName + '".');
              Result := False;
              Exit;
            end;
        except
          on E: TGetAvoidedZoneException do
          begin
            SetActionVarValue('$GetAvoidedZoneResult$', E.Message);
            Result := True;
            Exit;
          end;

          on E: Exception do
            if ASetVarOptions.FailOnException then
            begin
              SetActionVarValue('$ExecAction_Err$', E.Message);
              Result := False;
              Exit;
            end;
        end;

        VarName := ''; //Prevent creating a variable, named $SetWinInterpOption(...)$
      end;

      if (Pos('$Console(', VarName) = 1) and (VarName[Length(VarName)] = '$') and (VarName[Length(VarName) - 1] = ')') then
      begin
        if VarValue = '' then
        begin
          FuncArgs := Copy(VarName, Pos('(', VarName) + 1, MaxInt);
          FuncArgs := Copy(FuncArgs, 1, Length(FuncArgs) - 2);
          ConsoleArgs := EvaluateReplacements(FuncArgs);
        end
        else
          ConsoleArgs := EvaluateReplacements(VarValue);

        ListOfConsoleItems := TStringList.Create;
        try
          ListOfConsoleItems.LineBreak := #13#10;
          ListOfConsoleItems.Text := ConsoleArgs;
          for j := 0 to ListOfConsoleItems.Count - 1 do
            AddToLog('Console: ' + ListOfConsoleItems.Strings[j]);
        finally
          ListOfConsoleItems.Free;
        end;

        Continue;
      end;

      if (Pos('$DisplayGPUVars(', VarName) = 1) and (VarName[Length(VarName)] = '$') and (VarName[Length(VarName) - 1] = ')') then
      begin
        VarName := ''; //Prevent creating a variable, named $DisplayGPUVars(...)$
        AddToLog('The following GPU vars have to be set to True, in order to enable their features: ');
        AddToLog(CGPUDbgVar_GPUIncludeDashG + ': ' + EvaluateReplacements(CGPUDbgVar_GPUIncludeDashG));
        AddToLog(CGPUDbgVar_GPUSlaveQueueFromDevice + ': ' + EvaluateReplacements(CGPUDbgVar_GPUSlaveQueueFromDevice));
        AddToLog(CGPUDbgVar_GPUUseAllKernelsEvent + ': ' + EvaluateReplacements(CGPUDbgVar_GPUUseAllKernelsEvent));
        AddToLog(CGPUDbgVar_GPUNdrangeNoLocalParam + ': ' + EvaluateReplacements(CGPUDbgVar_GPUNdrangeNoLocalParam));
        AddToLog(CGPUDbgVar_GPUUseEventsInEnqueueKernel + ': ' + EvaluateReplacements(CGPUDbgVar_GPUUseEventsInEnqueueKernel));
        AddToLog(CGPUDbgVar_GPUWaitForAllKernelsToBeDone + ': ' + EvaluateReplacements(CGPUDbgVar_GPUWaitForAllKernelsToBeDone));
        AddToLog(CGPUDbgVar_GPUReleaseFinalEventAtKernelEnd + ': ' + EvaluateReplacements(CGPUDbgVar_GPUReleaseFinalEventAtKernelEnd));
        AddToLog(CGPUDbgVar_GPUIgnoreExecutionAvailability + ': ' + EvaluateReplacements(CGPUDbgVar_GPUIgnoreExecutionAvailability));
      end;

      if (Pos('$DisplayOpenCLInfo(', VarName) = 1) and (VarName[Length(VarName)] = '$') and (VarName[Length(VarName) - 1] = ')') then
      begin
        if VarValue = '' then
        begin
          FuncArgs := Copy(VarName, Pos('(', VarName) + 1, MaxInt);
          FuncArgs := Copy(FuncArgs, 1, Length(FuncArgs) - 2);
          ConsoleArgs := EvaluateReplacements(FuncArgs);
        end
        else
          ConsoleArgs := EvaluateReplacements(VarValue);

        VarName := ''; //Prevent creating a variable, named $DisplayGPUVars(...)$
        GetOpenCLInfo(AddToLog, ConsoleArgs);
      end;

      if (Pos('$OpenCLInfoToVars(', VarName) = 1) and (VarName[Length(VarName)] = '$') and (VarName[Length(VarName) - 1] = ')') then
      begin
        if VarValue = '' then
        begin
          FuncArgs := Copy(VarName, Pos('(', VarName) + 1, MaxInt);
          FuncArgs := Copy(FuncArgs, 1, Length(FuncArgs) - 2);
          ConsoleArgs := EvaluateReplacements(FuncArgs);
        end
        else
          ConsoleArgs := EvaluateReplacements(VarValue);

        VarName := ''; //Prevent creating a variable, named $OpenCLInfoToVars(...)$
        GetOpenCLInfo(OpenCLInfoToVars, ConsoleArgs);
      end;

      if VarName > '' then
        SetActionVarValue(VarName, VarValue);  //Do not move or delete this line. It is what SetVar does, it updates variables. VarName may be set to '', if a "Left-column" function is called.
    end;  //for i := 0 to TempListOfSetVarNames.Count - 1 do

  finally
    TempListOfSetVarNames.Free;
    TempListOfSetVarValues.Free;
    TempListOfSetVarEvalBefore.Free;
  end;

  Result := True;
end;


function TActionExecution.ExecuteWindowOperationsAction(var AWindowOperationsOptions: TClkWindowOperationsOptions): Boolean;
var
  Hw: THandle;
  Flags: DWord;
  X, Y, cx, cy: LongInt;
  CompRec: TCompRec;
begin
  Result := False;
  Hw := StrToIntDef(EvaluateReplacements('$Control_Handle$'), 0);
  if Hw = 0 then
  begin
    SetActionVarValue('$ExecAction_Err$', 'Cannot execute window operations on invalid handle.');
    Exit;
  end;

  case AWindowOperationsOptions.Operation of
    woBringToFront:
    begin
      SetForegroundWindow(Hw);
      Result := True;
    end;

    woMoveResize:
    begin
      Flags := SWP_ASYNCWINDOWPOS or SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOZORDER;

      if AWindowOperationsOptions.NewPositionEnabled then
      begin
        X := StrToIntDef(EvaluateReplacements(AWindowOperationsOptions.NewX), 0);
        Y := StrToIntDef(EvaluateReplacements(AWindowOperationsOptions.NewY), 0);
      end
      else
        Flags := Flags or SWP_NOMOVE;

      if AWindowOperationsOptions.NewSizeEnabled then
      begin
        cx := StrToIntDef(EvaluateReplacements(AWindowOperationsOptions.NewWidth), 0);
        cy := StrToIntDef(EvaluateReplacements(AWindowOperationsOptions.NewHeight), 0);
      end
      else
        Flags := Flags or SWP_NOSIZE;

      Result := SetWindowPos(Hw, HWND_TOP, X, Y, cx, cy, Flags);

      if not Result then
      {$IFDEF Windows}
        SetActionVarValue('$ExecAction_Err$', SysErrorMessage(GetLastError));
      {$ELSE}
        SetActionVarValue('$ExecAction_Err$', 'Not implemented.');
      {$ENDIF}
    end;

    woClose:
    begin
      {$IFDEF Windows}
        SendMessage(Hw, WM_CLOSE, 0, 0);
      {$ELSE}
        SendMessage(Hw, {WM_CLOSE} $10, 0, 0);
      {$ENDIF}
      Result := True;
    end;

    woFitIntoView:
    begin
      CompRec := GetWindowClassRec(Hw);                //-8 is the offset used on maximized windows
      if (CompRec.ComponentRectangle.Left >= -8) and
         (CompRec.ComponentRectangle.Top >= -8) and
         (CompRec.ComponentRectangle.Right < Screen.Width + 8) and
         (CompRec.ComponentRectangle.Bottom < Screen.Height + 8) then
      begin
        Result := True;
        Exit;
      end;

      Flags := SWP_ASYNCWINDOWPOS or SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOZORDER;
      Flags := Flags or SWP_NOSIZE;  //move only

      X := CompRec.ComponentRectangle.Left;   //set default values
      Y := CompRec.ComponentRectangle.Top;
      cx := CompRec.ComponentRectangle.Width;
      cy := CompRec.ComponentRectangle.Height;

      if (CompRec.ComponentRectangle.Left < -8) or (CompRec.ComponentRectangle.Top < -8) then
      begin
        if CompRec.ComponentRectangle.Left < -8 then
          X := 0;

        if CompRec.ComponentRectangle.Top < -8 then
          Y := 0;

        Result := SetWindowPos(Hw, HWND_TOP, X, Y, cx, cy, Flags);   //after this, left and top edges should be visible
        if not Result then
        begin
          {$IFDEF Windows}
            SetActionVarValue('$ExecAction_Err$', SysErrorMessage(GetLastError));
          {$ELSE}
            SetActionVarValue('$ExecAction_Err$', 'Not implemented.');
          {$ENDIF}
          Exit;
        end;
      end;

      if (CompRec.ComponentRectangle.Right > Screen.Width + 8) or (CompRec.ComponentRectangle.Bottom > Screen.Height + 8) then
      begin
        if CompRec.ComponentRectangle.Width < Screen.Width then
        begin
          if CompRec.ComponentRectangle.Right > Screen.Width + 8 then
            X := Screen.Width - CompRec.ComponentRectangle.Width;  //a simple move is enough
        end
        else
          if Flags and SWP_NOSIZE > 0 then
          begin
            Flags := Flags xor SWP_NOSIZE; //let it resize
            X := 0;
            cx := Screen.Width;
          end;

        if CompRec.ComponentRectangle.Height < Screen.Height then
        begin
          if CompRec.ComponentRectangle.Bottom > Screen.Height + 8 then
            Y := Screen.Height - CompRec.ComponentRectangle.Height;   //a simple move is enough
        end
        else
          if Flags and SWP_NOSIZE > 0 then
          begin
            Flags := Flags xor SWP_NOSIZE; //let it resize
            Y := 0;
            cy := Screen.Height;
          end;

        Result := SetWindowPos(Hw, HWND_TOP, X, Y, cx, cy, Flags);   //after this, right and bottom edges should be visible  (if resizing is allowed)
        if not Result then
        begin
          {$IFDEF Windows}
            SetActionVarValue('$ExecAction_Err$', SysErrorMessage(GetLastError));
          {$ELSE}
            SetActionVarValue('$ExecAction_Err$', 'Not implemented.');
          {$ENDIF}
          Exit;
        end;
      end;   //Right or Bottom are outside of visible area
    end; //woFitIntoView

    else
    begin
      Result := False;
    end;
  end;
end;


//Loads action var values from file and updates action vars (to the list of action vars) mentioned by a SetVar action.
//The SetVar action is used only as a list of var names. This way, the same SetVar action can be used on both loading and saving.
function TActionExecution.ExecuteLoadSetVarFromFileAction(var ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions): Boolean;
var
  Ini: TClkIniReadonlyFile;
  LoadedListOfVarNames, LoadedListOfVarValues, VarNamesToBeUpdated: TStringList;
  i: Integer;
  SetVarActionToBeUpdated: TClkSetVarOptions;
begin
  Result := False;
  if not DoOnGetSetVarActionByName(SetVarActionToBeUpdated, ALoadSetVarFromFileOptions.SetVarActionName) then
  begin
    SetActionVarValue('$ExecAction_Err$', 'Error: SetVar action not found when executing LoadSetVarFromFile: "' + ALoadSetVarFromFileOptions.SetVarActionName + '".');
    Exit;
  end;

  Ini := DoOnTClkIniReadonlyFileCreate(ALoadSetVarFromFileOptions.FileName);
  LoadedListOfVarNames := TStringList.Create;
  LoadedListOfVarValues := TStringList.Create;
  VarNamesToBeUpdated := TStringList.Create;
  try
    LoadedListOfVarNames.LineBreak := #13#10;
    LoadedListOfVarValues.LineBreak := #13#10;
    VarNamesToBeUpdated.LineBreak := #13#10;
    LoadedListOfVarNames.Text := FastReplace_45ToReturn(Ini.ReadString('Vars', 'ListOfVarNames', ''));
    LoadedListOfVarValues.Text := FastReplace_45ToReturn(Ini.ReadString('Vars', 'ListOfVarValues', ''));

    if LoadedListOfVarNames.Count <> LoadedListOfVarValues.Count then
    begin
      SetActionVarValue('$ExecAction_Err$', 'Error: Loaded SetVar action has a different number of var names than var values: ' + IntToStr(LoadedListOfVarNames.Count) + ' vs. ' + IntToStr(LoadedListOfVarValues.Count));
      Exit;
    end;

    VarNamesToBeUpdated.Text := SetVarActionToBeUpdated.ListOfVarNames;

    for i := 0 to LoadedListOfVarNames.Count - 1 do    //this list might not match SetVarActionToBeUpdated.ListOfVarNames;
      if VarNamesToBeUpdated.IndexOf(LoadedListOfVarNames.Strings[i]) <> -1 then  //only mentioned vars should be updated, not everything that comes from file
        SetActionVarValue(LoadedListOfVarNames.Strings[i], LoadedListOfVarValues.Strings[i]);

    Result := True;
  finally
    Ini.Free;
    LoadedListOfVarNames.Free;
    LoadedListOfVarValues.Free;
    VarNamesToBeUpdated.Free;
  end;
end;


//Takes action var values as they are, from the list of action vars, then saves them to file.
//The SetVar action is used only as a list of var names. This way, the same SetVar action can be used on both loading and saving.
function TActionExecution.ExecuteSaveSetVarToFileAction(var ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions): Boolean;
var
  Bkp, FileContent, VarNamesToBeSaved: TStringList;
  SetVarActionToBeSaved: TClkSetVarOptions;
  ListOfVarNames, ListOfVarValues: string;
  i: Integer;
  VarName: string;
begin
  Result := False;
  if not DoOnGetSetVarActionByName(SetVarActionToBeSaved, ASaveSetVarToFileOptions.SetVarActionName) then
  begin
    SetActionVarValue('$ExecAction_Err$', 'Error: SetVar action not found when executing SaveSetVarToFile: "' + ASaveSetVarToFileOptions.SetVarActionName + '".');
    Exit;
  end;

  Bkp := TStringList.Create;
  VarNamesToBeSaved := TStringList.Create;
  try
    Bkp.LineBreak := #13#10;
    VarNamesToBeSaved.LineBreak := #13#10;
    DoOnBackupVars(Bkp);
    VarNamesToBeSaved.Text := SetVarActionToBeSaved.ListOfVarNames;

    ListOfVarNames := '';
    ListOfVarValues := '';
    for i := 0 to VarNamesToBeSaved.Count - 1 do
    begin
      VarName := VarNamesToBeSaved.Strings[i];
      ListOfVarNames := ListOfVarNames + VarName + #4#5;
      ListOfVarValues := ListOfVarValues + Bkp.Values[VarName] + #4#5;
    end;

    FileContent := TStringList.Create;
    try
      FileContent.LineBreak := #13#10;
      FileContent.Add('[Vars]');
      FileContent.Add('ListOfVarNames=' + ListOfVarNames);
      FileContent.Add('ListOfVarValues=' + ListOfVarValues);

      DoOnSaveStringListToFile(FileContent, ASaveSetVarToFileOptions.FileName);
      Result := True;
    finally
      FileContent.Free;
    end;
  finally
    Bkp.Free;
    VarNamesToBeSaved.Free;
  end;
end;


function TActionExecution.ExecutePluginAction(var APluginOptions: TClkPluginOptions; AAllActions: PClkActionsRecArr; AListOfAllVars: TStringList; AResolvedPluginPath: string; IsDebugging, AShouldStopAtBreakPoint: Boolean): Boolean;
var
  ActionPlugin: TActionPlugin;
  tk: Int64;
begin
  Result := False;

  AddToLog('Executing plugin on a template with ' + IntToStr(Length(AAllActions^)) + ' action(s)...');
  if IsDebugging then
    AddToLog('Plugin debugging is active. It can be stepped over using F8 shortcut, or stopped using Ctrl-Shift-F2 shortcut.');

  //clear debug image
  frClickerActions.imgDebugBmp.Width := 300;    //some default values
  frClickerActions.imgDebugBmp.Height := 300;
  frClickerActions.imgDebugBmp.Picture.Bitmap.Width := frClickerActions.imgDebugBmp.Width;
  frClickerActions.imgDebugBmp.Picture.Bitmap.Height := frClickerActions.imgDebugBmp.Height;
  frClickerActions.imgDebugBmp.Canvas.Pen.Color := clWhite;
  frClickerActions.imgDebugBmp.Canvas.Brush.Color := clWhite;
  frClickerActions.imgDebugBmp.Canvas.Rectangle(0, 0, frClickerActions.imgDebugBmp.Width, frClickerActions.imgDebugBmp.Height);

  AResolvedPluginPath := EvaluateReplacements(AResolvedPluginPath);

  tk := GetTickCount64;
  try
    ActionPlugin.Loaded := False;
    if not ActionPlugin.LoadToExecute(AResolvedPluginPath,
                                      FOnLoadPluginFromInMemFS,
                                      AddToLog,
                                      DoOnExecuteActionByName,
                                      HandleOnSetVar,
                                      HandleOnSetDebugPoint,
                                      HandleOnIsAtBreakPoint,
                                      FOnLoadBitmap,
                                      FOnLoadRenderedBitmap,
                                      FOnLoadRawPmtv,
                                      HandleOnSaveFileToExtRenderingInMemFS,
                                      HandleOnScreenshotByActionName,
                                      IsDebugging,
                                      AShouldStopAtBreakPoint,
                                      FStopAllActionsOnDemand{FromParent},
                                      FPluginStepOver,
                                      FPluginContinueAll,
                                      frClickerActions.imgDebugBmp.Picture.Bitmap,
                                      DoOnGetPluginInMemFS,
                                      FFullTemplatesDir^,
                                      FAllowedFileDirsForServer^, //ResolvedAllowedFileDirs,
                                      FAllowedFileExtensionsForServer^,
                                      AAllActions,
                                      AListOfAllVars) then
    begin
      SetActionVarValue('$ExecAction_Err$', ActionPlugin.Err);
      AddToLog(ActionPlugin.Err);
      Exit;
    end;

    try
      SetActionVarValue('$ExecAction_Err$', '');
      SetActionVarValue(CActionPlugin_ExecutionResultErrorVar, '');

      if Pos(CExtBmp_PrefixUpperCase, UpperCase(AResolvedPluginPath)) = 1 then
        SetActionVarValue('$PluginPath$', AResolvedPluginPath)
      else
        SetActionVarValue('$PluginPath$', ExpandFileName(AResolvedPluginPath));

      Result := ActionPlugin.ExecutePlugin(APluginOptions.ListOfPropertiesAndValues);
      if not Result then
        AddToLog('Plugin execution failed with: ' + GetActionVarValue(CActionPlugin_ExecutionResultErrorVar))
      else
        AddToLog('Plugin executed successfully.');
    finally
      if not ActionPlugin.Unload(AddToLog) then
        AddToLog('Error unloading plugin: "' + ActionPlugin.Err + '".')
      else
        AddToLog('Plugin unloaded successfully.');

      ActionPlugin.PluginHandle := 0;
    end;

    frClickerActions.imgDebugBmp.Width := Max(10, Min(frClickerActions.imgDebugBmp.Picture.Bitmap.Width, 7680));   //Limit to 8K resolution for now. Sometimes, imgDebugBmp might not be initialized, causing AVs (div by 0 or heap overflow).
    frClickerActions.imgDebugBmp.Height := Max(10, Min(frClickerActions.imgDebugBmp.Picture.Bitmap.Height, 4320)); //Limit to 8K resolution for now.

    frClickerActions.frClickerPlugin.DoneDebuging; //hide debugging buttons
  finally
    AddToLog('Plugin executed in ' + IntToStr(GetTickCount64 - tk) + 'ms.  Total action count: ' + IntToStr(Length(AAllActions^)) + ' action(s)...');
  end;
end;


procedure UpdateActionProperties(var AEditTemplateOptions: TClkEditTemplateOptions; AEditedClkAction: PClkActionRec);
  function ArrayPropertyChecked(AListOfProperties: TStringList; APropertyName: string): Boolean;  // at least one property from the font profile is checked
  var
    i: Integer;
  begin               //IndexOf with Pos
    Result := False;

    for i := 0 to AListOfProperties.Count - 1 do
    begin
      if Pos(APropertyName, AListOfProperties.Strings[i]) = 1 then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

  function FontProfileChecked(AListOfProperties: TStringList; AProfileIndexStr: string): Boolean;  // at least one property from the font profile is checked
  begin
    Result := ArrayPropertyChecked(AListOfProperties, 'MatchBitmapText[' + AProfileIndexStr + '].');
  end;

  function FileNamesAsString(AListOfProperties, AListOfEnabledProperties: TStringList; APropertyName: string): string;
  var
    i, Idx: Integer;
    EnabledPropertyName: string;
    FilteredResult: TStringList;
    IsFiltered: Boolean;
  begin
    Result := '';

    Idx := AListOfProperties.IndexOfName(APropertyName);
    if Idx = -1 then
      Exit;

    Result := FastReplace_1920ToReturn(AListOfProperties.ValueFromIndex[Idx]);
    //the result has to be filtered by AListOfEnabledProperties
    FilteredResult := TStringList.Create;
    try
      FilteredResult.LineBreak := #13#10;
      FilteredResult.Text := Result;
      IsFiltered := False;

      for i := FilteredResult.Count - 1 downto 0 do
      begin
        EnabledPropertyName := APropertyName + '.File[' + IntToStr(i) + ']';
        if AListOfEnabledProperties.IndexOf(EnabledPropertyName) = -1 then
        begin
          FilteredResult.Delete(i);
          IsFiltered := True;
        end;
      end;

      if IsFiltered then
        Result := FilteredResult.Text;
    finally
      FilteredResult.Free;
    end;
  end;

var
  TempProperties, TempEnabledProperties, TempEditedProperties: TStringList;
  ListOfMatchBitmapFiles, ListOfMatchPrimitiveFiles: TStringList;
  PropertyName, OldPropertyValue, NewPropertyValue, ActionProperties, s: string;
  ListOfOldPluginProperties, ListOfNewPluginProperties: TStringList;
  i, j, n: Integer;
  MaxSrcProfileIndex: Integer;
  Fnm: string;
begin
  TempProperties := TStringList.Create;
  TempEnabledProperties := TStringList.Create;
  TempEditedProperties := TStringList.Create;
  try
    TempProperties.LineBreak := #13#10;
    TempEnabledProperties.LineBreak := #13#10;
    TempEditedProperties.LineBreak := #13#10;
    TempEnabledProperties.Text := FastReplace_45ToReturn(AEditTemplateOptions.ListOfEnabledProperties);
    TempEditedProperties.Text := StringReplace(AEditTemplateOptions.ListOfEditedProperties, CPropSeparatorInt, #13#10, [rfReplaceAll]);

    //Example for FindControl, FindSubControl:
    //TempEditedProperties[0] becomes ''    //TBD
    //TempEditedProperties[1] becomes ''    //TBD
    //
    //TempEnabledProperties[0] becomes 'MatchBitmapText[0].ForegroundColor'
    //TempEnabledProperties[1] becomes 'MatchBitmapText[0].BackgroundColor'
    //TempEnabledProperties[2] becomes 'MatchBitmapText[0].FontName'
    //TempEnabledProperties[3] becomes 'MatchBitmapFiles.File[0]'
    //TempEnabledProperties[4] becomes 'MatchBitmapFiles.File[1]'
    //TempEnabledProperties[5] becomes 'MatchBitmapFiles.File[2]'
    //TempEnabledProperties[6] becomes 'MatchPrimitiveFiles.File[1]'
    //TempEnabledProperties[7] becomes 'MatchPrimitiveFiles.File[0]'
    //TempEnabledProperties[8] becomes 'MatchBitmapText[1].FontName'
    //TempEnabledProperties[9] becomes 'MatchBitmapText[1].BackgroundColor'
    //TempEnabledProperties[10] becomes 'MatchBitmapText[1].ForegroundColor'
    //TempEnabledProperties[11] becomes 'MatchBitmapText[1].FontSize'

    //TempProperties[0] becomes ''          //TBD
    //TempProperties[1] becomes ''          //TBD

    if AEditTemplateOptions.EditedActionType in [acFindSubControl] then
    begin
      MaxSrcProfileIndex := -1;
      for i := 0 to 30 - 1 do  //verifying 30 profiles, since this is a large number
        if FontProfileChecked(TempEditedProperties, IntToStr(i)) then
          MaxSrcProfileIndex := i;

      SetLength(AEditedClkAction^.FindSubControlOptions.MatchBitmapText, 0);  //Without this line, old profiles are kept if their index is above MaxSrcProfileIndex.
                                                                           //However, there is no other way of deleting them, rather than deleting all.
      if Length(AEditedClkAction^.FindSubControlOptions.MatchBitmapText) < MaxSrcProfileIndex + 1 then
      begin
        //Verify if the missing profiles are checked. If yes, add them.

        for i := Length(AEditedClkAction^.FindSubControlOptions.MatchBitmapText) to MaxSrcProfileIndex do
          if FontProfileChecked(TempEditedProperties, IntToStr(i)) then
          begin
            n := Length(AEditedClkAction^.FindSubControlOptions.MatchBitmapText);
            SetLength(AEditedClkAction^.FindSubControlOptions.MatchBitmapText, n + 1);
            GetDefaultPropertyValues_FindControl_MatchBitmapText(AEditedClkAction^.FindSubControlOptions.MatchBitmapText[n]);
          end;
      end;

      ListOfMatchBitmapFiles := TStringList.Create;
      try
        ListOfMatchBitmapFiles.LineBreak := #13#10;
        ListOfMatchBitmapFiles.Text := FastReplace_45ToReturn(AEditedClkAction^.FindSubControlOptions.MatchBitmapFiles);  //this is empty for a new action

        Fnm := FileNamesAsString(TempEditedProperties, TempEnabledProperties, 'MatchBitmapFiles');
        ListOfMatchBitmapFiles.Text := Fnm;

        AEditedClkAction^.FindSubControlOptions.MatchBitmapFiles := FastReplace_ReturnTo45(ListOfMatchBitmapFiles.Text);
      finally
        ListOfMatchBitmapFiles.Free;
      end;

      ListOfMatchPrimitiveFiles := TStringList.Create;
      try
        ListOfMatchPrimitiveFiles.LineBreak := #13#10;
        ListOfMatchPrimitiveFiles.Text := FastReplace_45ToReturn(AEditedClkAction^.FindSubControlOptions.MatchPrimitiveFiles); //this is empty for a new action

        Fnm := FileNamesAsString(TempEditedProperties, TempEnabledProperties, 'MatchPrimitiveFiles');
        ListOfMatchPrimitiveFiles.Text := Fnm;

        AEditedClkAction^.FindSubControlOptions.MatchPrimitiveFiles := FastReplace_ReturnTo45(ListOfMatchPrimitiveFiles.Text);
      finally
        ListOfMatchPrimitiveFiles.Free;
      end;
    end;

    ActionProperties := GetActionPropertiesByType(AEditedClkAction^);
    ActionProperties := StringReplace(ActionProperties, CPropSeparatorSer, #13#10, [rfReplaceAll]);
    TempProperties.Text := StringReplace(ActionProperties, {'&'} CPropSeparatorInt, #13#10, [rfReplaceAll]);

    //Example for plugins:
    //TempEditedProperties[0] becomes 'FileName=$AppDir$\..\UIClickerFindWindowsPlugin\lib\i386-win32\UIClickerFindWindows.dll'
    //TempEditedProperties[1] becomes 'ListOfPropertiesAndValues=FindSubControlTopLeftCorner=ab'#$13#$14'FindSubControlBotLeftCorner=cd'#$13#$14'FindSubControlTopRightCorner=ef'#$13#$14'FindSubControlBotRightCorner=gh'#$13#$14'FindSubControlLeftEdge=ij'#$13#$14'FindSubControlTopEdge=kl'#$13#$14'FindSubControlRightEdge=mn'#$13#$14'FindSubControlBottomEdge=op'#$13#$14'ParentFindControl=qr'#$13#$14'BorderThickness=6'#$13#$14'MatchWindowEdges=True'#$13#$14
    //
    //TempEnabledProperties[0] becomes 'FileName'
    //TempEnabledProperties[1] becomes 'FindSubControlTopLeftCorner'
    //TempEnabledProperties[2] becomes 'FindSubControlBotLeftCorner'
    //TempEnabledProperties[3] becomes 'FindSubControlTopRightCorner'

    //TempProperties[0] becomes 'FileName=$AppDir$\..\UIClickerFindWindowsPlugin\lib\i386-win32\UIClickerFindWindows.dll'
    //TempProperties[1] becomes 'ListOfPropertiesAndValues='

    // For plugin, the ListOfPropertiesAndValues property has to be unpacked (#4#5-> #13#10), updated, then repacked.
    // TempEnabledProperties won't even see the ListOfPropertiesAndValues key, only its content (value, which is a list of key=value)

    //MessageBoxFunction(PChar(TempEnabledProperties.Text), 'TempEnabledProperties', 0); //for debugging only

    for i := 0 to TempProperties.Count - 1 do
    begin
      if i >= TempProperties.Count then
        raise Exception.Create('bad code');  //"i" can go past TempProperties.Count - 1 when setting ValueFromIndex to ''

      PropertyName := TempProperties.Names[i];
      NewPropertyValue := TempEditedProperties.Values[PropertyName];   //ValueFromIndex should work, but it is gets out of sync (for some reason), then a wrong value is returned.

      case AEditTemplateOptions.EditedActionType of
        acPlugin:
        begin
          OldPropertyValue := TempProperties.ValueFromIndex[i];

          //For plugins, if the property is ListOfPropertiesAndValues, its value is of <Property=Value>#19#20<Property=Value>#19#20 format
          if PropertyName = 'ListOfPropertiesAndValues' then
          begin
            ListOfNewPluginProperties := TStringList.Create;
            ListOfOldPluginProperties := TStringList.Create;
            try
              ListOfNewPluginProperties.LineBreak := #13#10;
              ListOfOldPluginProperties.LineBreak := #13#10;
              ListOfOldPluginProperties.Text := FastReplace_45ToReturn(OldPropertyValue);
              ListOfNewPluginProperties.Text := FastReplace_1920ToReturn(FastReplace_45ToReturn(NewPropertyValue)); //it contains only the checked properties

              s := '';
              for j := 0 to ListOfNewPluginProperties.Count - 1 do
                if TempEnabledProperties.IndexOf(ListOfNewPluginProperties.Names[j]) > -1 then //PropertyName enabled
                  s := s + ListOfNewPluginProperties.Names[j] + '=' + ListOfNewPluginProperties.ValueFromIndex[j] + #4#5
                else
                  s := s + ListOfNewPluginProperties.Names[j] + '=' + ListOfOldPluginProperties.Values[ListOfNewPluginProperties.Names[j]] + #4#5;  //using ListOfPluginProperties.Names[j] instead of ValueFromIndex[j], because this list might not contain all plugin properties. It contains only the checked ones.

              //[might not be needed] If there are properties in ListOfOldPluginProperties, which are not in ListOfNewPluginProperties, then they should be added here.  Also, not sure if the property order matters.
              for j := 0 to ListOfOldPluginProperties.Count - 1 do
                if ListOfNewPluginProperties.IndexOfName(ListOfOldPluginProperties.Names[j]) = -1 then
                  s := s + ListOfOldPluginProperties.Strings[j] + #4#5;

              TempProperties.Strings[i] := TempProperties.Names[i] + '=' + s; // TempProperties.ValueFromIndex[i] := s;     //Do not set ValueFromIndex !!!
            finally
              ListOfNewPluginProperties.Free;
              ListOfOldPluginProperties.Free;
            end;
          end  //ListOfPropertiesAndValues
          else  //other property (most likely FileName)
            if TempEnabledProperties.IndexOf(PropertyName) > -1 then //enabled
              TempProperties.Strings[i] := TempProperties.Names[i] + '=' + NewPropertyValue; //TempProperties.ValueFromIndex[i] := NewPropertyValue;  //Do not set ValueFromIndex !!!
        end; //is plugin

        acCallTemplate:
        begin
          if PropertyName = 'ListOfCustomVarsAndValues' then
            NewPropertyValue := FastReplace_1920To45(NewPropertyValue);

          if Pos('Loop.', PropertyName) = 1 then
            PropertyName := 'CallTemplate' + PropertyName;

          if TempEnabledProperties.IndexOf(PropertyName) > -1 then //enabled
            TempProperties.Strings[i] := TempProperties.Names[i] + '=' + NewPropertyValue; //TempProperties.ValueFromIndex[i] := NewPropertyValue;    //Do not set ValueFromIndex !!!
        end;

        acSetVar:
        begin
          if (PropertyName = 'ListOfVarNames') or
             (PropertyName = 'ListOfVarValues') or
             (PropertyName = 'ListOfVarEvalBefore') then
          begin
            PropertyName := 'ListOfVarNamesValuesAndEvalBefore';
            NewPropertyValue := FastReplace_1920To45(NewPropertyValue);
          end;

          if TempEnabledProperties.IndexOf(PropertyName) > -1 then //enabled
            TempProperties.Strings[i] := TempProperties.Names[i] + '=' + NewPropertyValue; //TempProperties.ValueFromIndex[i] := NewPropertyValue;   //Do not set ValueFromIndex !!!
        end;

        else //is some other action type
          if TempEnabledProperties.IndexOf(PropertyName) > -1 then //enabled
            TempProperties.Strings[i] := TempProperties.Names[i] + '=' + NewPropertyValue; //TempProperties.ValueFromIndex[i] := NewPropertyValue;  //Do not set ValueFromIndex !!!
      end; //case
    end; //for

    SetActionProperties(TempProperties, AEditTemplateOptions.EditedActionType, AEditedClkAction^);

    if AEditTemplateOptions.EditedActionType = acEditTemplate then
    begin
      AEditedClkAction^.EditTemplateOptions.ListOfEditedProperties := AEditTemplateOptions.ListOfEditedProperties_ET;
      AEditedClkAction^.EditTemplateOptions.ListOfEnabledProperties := AEditTemplateOptions.ListOfEnabledProperties_ET;
    end;
  finally
    TempProperties.Free;
    TempEnabledProperties.Free;
    TempEditedProperties.Free;
  end;
end;


procedure UpdateActionByEditTemplate(var AEditTemplateOptions: TClkEditTemplateOptions; AClkAction: PClkActionRec);
begin
  AClkAction.ActionOptions.ActionName := AEditTemplateOptions.EditedActionName;
  AClkAction.ActionOptions.Action := AEditTemplateOptions.EditedActionType;
  AClkAction.ActionOptions.ActionCondition := AEditTemplateOptions.EditedActionCondition;
  AClkAction.ActionOptions.ActionTimeout := AEditTemplateOptions.EditedActionTimeout;
  AClkAction.ActionOptions.ActionEnabled := True;
  AClkAction.ActionOptions.ExecutionIndex := '';
  AClkAction.ActionStatus := asNotStarted;
  AClkAction.ActionSkipped := False;
  AClkAction.ActionDebuggingStatus := adsNone;
  AClkAction.ActionBreakPoint.Exists := False;
  AClkAction.ActionBreakPoint.Enabled := False;
  AClkAction.ActionBreakPoint.Condition := '';

  if AEditTemplateOptions.Operation = etoNewAction then
  begin
    GetDefaultPropertyValues_Click(AClkAction.ClickOptions);
    GetDefaultPropertyValues_ExecApp(AClkAction.ExecAppOptions);
    GetDefaultPropertyValues_FindControl(AClkAction.FindControlOptions);
    GetDefaultPropertyValues_FindSubControl(AClkAction.FindSubControlOptions);
    GetDefaultPropertyValues_SetControlText(AClkAction.SetTextOptions);
    GetDefaultPropertyValues_CallTemplate(AClkAction.CallTemplateOptions);
    GetDefaultPropertyValues_Sleep(AClkAction.SleepOptions);
    GetDefaultPropertyValues_SetVar(AClkAction.SetVarOptions);
    GetDefaultPropertyValues_WindowOperations(AClkAction.WindowOperationsOptions);
    GetDefaultPropertyValues_LoadSetVarFromFile(AClkAction.LoadSetVarFromFileOptions);
    GetDefaultPropertyValues_SaveSetVarToFile(AClkAction.SaveSetVarToFileOptions);
    GetDefaultPropertyValues_Plugin(AClkAction.PluginOptions);
    GetDefaultPropertyValues_EditTemplate(AClkAction.EditTemplateOptions);
  end;

  UpdateActionProperties(AEditTemplateOptions, AClkAction);
end;


procedure GetPropertiesForEditTemplate(var AEditTemplateOptions: TClkEditTemplateOptions; AEditedClkAction: PClkActionRec; AListOfNames, AListOfValues: TStringList);
var
  TempProperties, TempEnabledProperties, TempEditedProperties: TStringList;
  ActionProperties, PropertyName, PropertyValue, OldPropertyValue: string;
  i, j: Integer;
  ListOfOldPluginProperties, ListOfNewPluginProperties: TStringList;

  procedure AddFindSubControlFilesFromProperty(APropertyName: string);
  var
    j: Integer;
    SubPropertyName: string;
    ListOfFiles: TStringList;
  begin
    ListOfFiles := TStringList.Create;
    try
      ListOfFiles.LineBreak := #13#10;
      ListOfFiles.Text := FastReplace_45ToReturn(PropertyValue);
      for j := 0 to ListOfFiles.Count - 1 do
      begin
        SubPropertyName := APropertyName + '.File[' + IntToStr(j) + ']';
        if TempEnabledProperties.IndexOf(SubPropertyName) > -1 then
        begin
          AListOfNames.Add(SubPropertyName);
          AListOfValues.Add(ListOfFiles.Strings[j]);
        end;
      end;
    finally
      ListOfFiles.Free;
    end;
  end;

begin
  TempProperties := TStringList.Create;
  TempEnabledProperties := TStringList.Create;
  TempEditedProperties := TStringList.Create;
  try
    TempProperties.LineBreak := #13#10;
    TempEnabledProperties.LineBreak := #13#10;
    TempEditedProperties.LineBreak := #13#10;
    TempEnabledProperties.Text := FastReplace_45ToReturn(AEditTemplateOptions.ListOfEnabledProperties);
    TempEditedProperties.Text := StringReplace(AEditTemplateOptions.ListOfEditedProperties, CPropSeparatorInt, #13#10, [rfReplaceAll]);

    ActionProperties := GetActionPropertiesByType(AEditedClkAction^);
    ActionProperties := StringReplace(ActionProperties, CPropSeparatorSer, #13#10, [rfReplaceAll]);
    TempProperties.Text := StringReplace(ActionProperties, {'&'} CPropSeparatorInt, #13#10, [rfReplaceAll]);

    //MessageBoxFunction(PChar(TempEnabledProperties.Text), 'TempEnabledProperties', 0); //for debugging only

    for i := 0 to TempProperties.Count - 1 do
    begin
      PropertyName := TempProperties.Names[i];
      PropertyValue := TempProperties.ValueFromIndex[i]; //TempEditedProperties.Values[PropertyName];   //ValueFromIndex should work, but it is gets out of sync (for some reason), then a wrong value is returned.

      case AEditTemplateOptions.EditedActionType of
        acFindSubControl:
        begin
          if (PropertyName = 'MatchBitmapFiles') or (PropertyName = 'MatchPrimitiveFiles') then
            AddFindSubControlFilesFromProperty(PropertyName)
          else
            if TempEnabledProperties.IndexOf(PropertyName) > -1 then //enabled
            begin
              AListOfNames.Add(PropertyName);
              AListOfValues.Add(PropertyValue);
            end;
        end;

        acPlugin:
        begin
          OldPropertyValue := TempProperties.ValueFromIndex[i];

          //For plugins, if the property is ListOfPropertiesAndValues, its value is of <Property=Value>#19#20<Property=Value>#19#20 format
          if PropertyName = 'ListOfPropertiesAndValues' then
          begin
            ListOfNewPluginProperties := TStringList.Create;
            ListOfOldPluginProperties := TStringList.Create;
            try
              ListOfNewPluginProperties.LineBreak := #13#10;
              ListOfOldPluginProperties.LineBreak := #13#10;
              ListOfOldPluginProperties.Text := FastReplace_45ToReturn(OldPropertyValue);

              PropertyValue := TempEditedProperties.Values[PropertyName];
              ListOfNewPluginProperties.Text := FastReplace_1920ToReturn(FastReplace_45ToReturn(PropertyValue)); //it contains only the checked properties

              for j := 0 to ListOfNewPluginProperties.Count - 1 do
                if TempEnabledProperties.IndexOf(ListOfNewPluginProperties.Names[j]) > -1 then //PropertyName enabled
                begin
                  AListOfNames.Add(ListOfNewPluginProperties.Names[j]);
                  AListOfValues.Add(ListOfOldPluginProperties.Values[ListOfNewPluginProperties.Names[j]]);  /////////////// bad [j]
                end;
            finally
              ListOfNewPluginProperties.Free;
              ListOfOldPluginProperties.Free;
            end;
          end  //ListOfPropertiesAndValues
          else  //other property (most likely FileName)
          begin
            if TempEnabledProperties.IndexOf(PropertyName) > -1 then //enabled
            begin
              AListOfNames.Add(PropertyName);
              AListOfValues.Add(PropertyValue);
            end;
          end;
        end; //is plugin

        acCallTemplate:
        begin
          if PropertyName = 'ListOfCustomVarsAndValues' then
            PropertyValue := FastReplace_1920To45(PropertyValue);

          if Pos('Loop.', PropertyName) = 1 then
            PropertyName := 'CallTemplate' + PropertyName;

          if TempEnabledProperties.IndexOf(PropertyName) > -1 then //enabled
          begin
            AListOfNames.Add(PropertyName);
            AListOfValues.Add(PropertyValue);
          end;
        end;

        acSetVar:
        begin
          if (PropertyName = 'ListOfVarNames') or
             (PropertyName = 'ListOfVarValues') or
             (PropertyName = 'ListOfVarEvalBefore') then
          begin
            PropertyValue := FastReplace_1920ToReturn(PropertyValue);

            if TempEnabledProperties.IndexOf('ListOfVarNamesValuesAndEvalBefore') > -1 then //enabled
            begin
              AListOfNames.Add(PropertyName);
              AListOfValues.Add(PropertyValue);
            end;
          end
          else
            if TempEnabledProperties.IndexOf(PropertyName) > -1 then //enabled
            begin
              AListOfNames.Add(PropertyName);
              AListOfValues.Add(PropertyValue);
            end;
        end;

        else //it's some other action type
        begin
          if TempEnabledProperties.IndexOf(PropertyName) > -1 then //enabled
          begin
            AListOfNames.Add(PropertyName);
            AListOfValues.Add(PropertyValue);
          end;
        end;
      end; //case  AEditTemplateOptions.EditedActionType
    end; //for i
  finally
    TempProperties.Free;
    TempEnabledProperties.Free;
    TempEditedProperties.Free;
  end;
end;


function TActionExecution.ExecuteEditTemplateAction(var AEditTemplateOptions: TClkEditTemplateOptions): Boolean;
var
  ClkActions: TClkActionsRecArr;
  Notes, IconPath: string;
  i, Idx, DestIdx: Integer;
  LocalListOfPropertyNames, LocalListOfPropertyValues: TStringList;
  ShouldUpdateUI, ShouldSaveSelfTemplate: Boolean;
  SaveTemplateResult: string;
  EvaluatedEditedActionName, EvaluatedNewActionName: string;
  LoadingTemplateErr: string;
begin
  Result := False;
  ShouldUpdateUI := False;
  ShouldSaveSelfTemplate := False;
  SetActionVarValue('$ExecAction_Err$', '');

  try
    LoadingTemplateErr := DoOnLoadTemplateToActions(AEditTemplateOptions.TemplateFileName, ClkActions, AEditTemplateOptions.WhichTemplate, Notes, IconPath, True);
    if LoadingTemplateErr <> '' then
    begin
      SetActionVarValue('$ExecAction_Err$', LoadingTemplateErr);
      Exit;
    end;

    AddToLog('EditAction: loaded template "' + AEditTemplateOptions.TemplateFileName + '".  Operation: ' + CEditTemplateOperationStr[AEditTemplateOptions.Operation]);

    if (Length(ClkActions) = 0) and (AEditTemplateOptions.Operation <> etoNewAction) then
    begin
      AddToLog('Template is empty when executing EditAction.');
      SetActionVarValue('$ExecAction_Err$', '');  //exiting without an error
      Exit;
    end;

    EvaluatedEditedActionName := EvaluateReplacements(AEditTemplateOptions.EditedActionName);
    Idx := GetActionIndexByName(ClkActions, EvaluatedEditedActionName);
    if Idx = -1 then
    begin
      if not (AEditTemplateOptions.Operation in [etoNewAction, etoSaveTemplate]) then   //etoNewAction and etoSaveTemplate are the only operation which allow Idx to be -1.
      begin //the action should already exist
        SetActionVarValue('$ExecAction_Err$', CREResp_ActionNotFound);
        AddToLog('Action "' + EvaluatedEditedActionName + '" cannot be found.');
        Exit;
      end;
    end
    else
      if AEditTemplateOptions.Operation = etoNewAction then
      begin //the action should already exist
        SetActionVarValue('$ExecAction_Err$', CREResp_ActionAlreadyExists);
        Exit;
      end;

    //etoNewAction, etoMoveAction, etoDeleteAction, etoDuplicateAction, etoRenameAction, etoEnableAction, etoDisableAction, etoGetProperty, etoSetProperty, etoSetCondition, etoExecuteAction, etoSaveTemplate
    case AEditTemplateOptions.Operation of
      etoNewAction:  //If action already exists, Idx will point to it. That means the action is updated.
      begin
        SetLength(ClkActions, Length(ClkActions) + 1);
        Idx := Length(ClkActions) - 1;
        UpdateActionByEditTemplate(AEditTemplateOptions, @ClkActions[Idx]);
        ShouldUpdateUI := True;
        Result := True;
      end;

      etoUpdateAction:
      begin
        UpdateActionByEditTemplate(AEditTemplateOptions, @ClkActions[Idx]);
        ShouldUpdateUI := True;
        Result := True;
      end;

      etoMoveAction:
      begin
        EvaluatedNewActionName := '';
        if AEditTemplateOptions.NewActionName = '' then  //this moves the action at the end of the list
          DestIdx := Length(ClkActions) - 1
        else
        begin
          EvaluatedNewActionName := EvaluateReplacements(AEditTemplateOptions.NewActionName);
          DestIdx := GetActionIndexByName(ClkActions, EvaluatedNewActionName);
        end;

        if DestIdx = -1 then
        begin
          SetActionVarValue('$ExecAction_Err$', CREResp_ActionNotFound);
          AddToLog('Action "' + EvaluatedNewActionName + '" cannot be found.');
          Exit;
        end;

        MoveActionInArr(ClkActions, Idx, DestIdx);

        ShouldUpdateUI := True;
        Result := True;
      end;

      etoDeleteAction:
      begin
        RemoveActionFromArr(ClkActions, Idx);
        ShouldUpdateUI := True;
        Result := True;
      end;

      etoDuplicateAction:
      begin
        SetLength(ClkActions, Length(ClkActions) + 1);
        CopyActionContent(ClkActions[Idx], ClkActions[Length(ClkActions) - 1]);  //for duplicating right after the existing action, there should be a another move operation
        ClkActions[Length(ClkActions) - 1].ActionOptions.ActionName := EvaluateReplacements(AEditTemplateOptions.NewActionName);
        ShouldUpdateUI := True;
        Result := True;
      end;

      etoRenameAction:
      begin
        EvaluatedNewActionName := EvaluateReplacements(AEditTemplateOptions.NewActionName);
        DestIdx := GetActionIndexByName(ClkActions, EvaluatedNewActionName);

        if DestIdx > -1 then
        begin
          SetActionVarValue('$ExecAction_Err$', CREResp_ActionAlreadyExists);
          Exit;
        end;

        ClkActions[Idx].ActionOptions.ActionName := EvaluatedNewActionName;
        ShouldUpdateUI := True;
        Result := True;
      end;

      etoEnableAction:
      begin
        ClkActions[Idx].ActionOptions.ActionEnabled := True;
        ShouldUpdateUI := True;
        Result := True;
      end;

      etoDisableAction:
      begin
        ClkActions[Idx].ActionOptions.ActionEnabled := False;
        ShouldUpdateUI := True;
        Result := True;
      end;

      etoGetProperty:
      begin
        //All the properties to be found in AEditTemplateOptions.ListOfEnabledProperties will be returned in $Property_<PropertyName>_Value$ variables.

        LocalListOfPropertyNames := TStringList.Create;
        LocalListOfPropertyValues := TStringList.Create;
        try
          LocalListOfPropertyNames.LineBreak := #13#10;
          LocalListOfPropertyValues.LineBreak := #13#10;
          GetPropertiesForEditTemplate(AEditTemplateOptions, @ClkActions[Idx], LocalListOfPropertyNames, LocalListOfPropertyValues);
          for i := 0 to LocalListOfPropertyNames.Count - 1 do
            SetActionVarValue('$Property_' + LocalListOfPropertyNames.Strings[i] + '_Value$', LocalListOfPropertyValues.Strings[i]);
        finally
          LocalListOfPropertyNames.Free;
          LocalListOfPropertyValues.Free;
        end;

        Result := True;
      end;

      etoSetProperty:
      begin      //read-modify-write
        UpdateActionProperties(AEditTemplateOptions, @ClkActions[Idx]);
        Result := True;
      end;

      etoSetCondition:
      begin
        ClkActions[Idx].ActionOptions.ActionCondition := AEditTemplateOptions.EditedActionCondition;
        Result := True;
      end;

      etoSetTimeout:
      begin
        ClkActions[Idx].ActionOptions.ActionTimeout := AEditTemplateOptions.EditedActionTimeout;
        ShouldUpdateUI := True;
        Result := True;
      end;

      etoExecuteAction:
        Result := DoOnExecuteActionByContent(ClkActions, Idx);

      etoSaveTemplate: //The advantage of having a Save command is that a template can be edited in memory, partially executed then saved (or not).
      begin            //This command makes sense for the currently loaded file (the etwtSelf option).
        if AEditTemplateOptions.ShouldSaveTemplate then
          ShouldSaveSelfTemplate := True;

        if AEditTemplateOptions.WhichTemplate = etwtOther then
          AddToLog('Saving anyway...');

        Result := True;
      end;
    end;  //case

    if Result and (not (AEditTemplateOptions.Operation in [etoExecuteAction, etoGetProperty])) then   //etoExecuteAction is not an editing operation
    begin  //The file is saved almost every time for the etwtOther option (successful editing operations only).
      SaveTemplateResult := DoOnSaveCompleteTemplateToFile(AEditTemplateOptions.TemplateFileName, ClkActions, AEditTemplateOptions.WhichTemplate, Notes, IconPath, ShouldUpdateUI, ShouldSaveSelfTemplate);

      if SaveTemplateResult <> '' then
      begin
        Result := False;
        SetActionVarValue('$ExecAction_Err$', SaveTemplateResult);
      end;
    end;
  except
    on E: Exception do
    begin
      SetActionVarValue('$ExecAction_Err$', E.Message);
      AddToLog(E.Message);
      raise;
    end;
  end;
end;


procedure GetActionOptionsFromParams(AListOfOptionsParams: TStrings; var AAction: TClkActionRec);
begin
  AAction.ActionDebuggingStatus := adsCurrent;
  AAction.ActionBreakPoint.Exists := False;
  AAction.ActionBreakPoint.Enabled := False;
  AAction.ActionStatus := asNotStarted;
  AAction.ActionSkipped := False;

  AAction.ActionOptions.ActionName := AListOfOptionsParams.Values[CPropertyName_ActionName];
  AAction.ActionOptions.ActionTimeout := StrToIntDef(AListOfOptionsParams.Values[CPropertyName_ActionTimeout], 1000);
  AAction.ActionOptions.Action := {%H-}TClkAction(CClkUnsetAction);
  AAction.ActionOptions.ActionEnabled := True;
  AAction.ActionOptions.ActionCondition := '';
  AAction.ActionOptions.ExecutionIndex := '';

  if AAction.ActionOptions.ActionName = '' then
    AAction.ActionOptions.ActionName := 'Debugging action';

  if AAction.ActionOptions.ActionTimeout < 0 then
    AAction.ActionOptions.ActionTimeout := 600000; //10min
end;


function TActionExecution.ExecuteClickActionAsString(AListOfClickOptionsParams: TStrings): Boolean;
var
  Err: string;
  WorkAction: TClkActionRec;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    Err := SetClickActionProperties(AListOfClickOptionsParams, WorkAction.ClickOptions);
    if Err <> '' then
    begin
      SetActionVarValue('$ExecAction_Err$', Err);
      Exit;
    end;

    if AListOfClickOptionsParams.Values[CREParam_UseServerDebugging] = '1' then
    begin
      GetActionOptionsFromParams(AListOfClickOptionsParams, WorkAction);
      WorkAction.ActionOptions.Action := acClick;
      DoOnWaitInDebuggingMode(WorkAction, asiNo);
    end;

    Result := ExecuteMultiClickAction(WorkAction.ClickOptions);
  finally
    SetLastActionStatus(Result, False);
  end;
end;


function TActionExecution.ExecuteExecAppActionAsString(AListOfExecAppOptionsParams: TStrings): Boolean;
var
  WorkAction: TClkActionRec;
  Err: string;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    Err := SetExecAppActionProperties(AListOfExecAppOptionsParams, WorkAction.ExecAppOptions, WorkAction.ActionOptions);
    if Err <> '' then
    begin
      SetActionVarValue('$ExecAction_Err$', Err);
      Exit;
    end;

    if AListOfExecAppOptionsParams.Values[CREParam_UseServerDebugging] = '1' then
    begin
      GetActionOptionsFromParams(AListOfExecAppOptionsParams, WorkAction);
      WorkAction.ActionOptions.Action := acExecApp;
      DoOnWaitInDebuggingMode(WorkAction, asiNo);
    end;

    Result := ExecuteExecAppAction(WorkAction.ExecAppOptions, WorkAction.ActionOptions);
  finally
    SetLastActionStatus(Result, False);
  end;
end;


function TActionExecution.ExecuteFindControlActionAsString(AListOfFindControlOptionsParams: TStrings): Boolean;
var
  WorkAction: TClkActionRec;
  Err: string;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    Err := SetFindControlActionProperties(AListOfFindControlOptionsParams, AddToLog, WorkAction.FindControlOptions, WorkAction.ActionOptions);
    if Err <> '' then
    begin
      SetActionVarValue('$ExecAction_Err$', Err);
      Exit;
    end;

    if AListOfFindControlOptionsParams.Values[CREParam_UseServerDebugging] = '1' then
    begin
      GetActionOptionsFromParams(AListOfFindControlOptionsParams, WorkAction);
      WorkAction.ActionOptions.Action := acFindControl;

      DoOnWaitInDebuggingMode(WorkAction, asiNo);
    end;

    Result := ExecuteFindControlActionWithTimeout(WorkAction.FindControlOptions, WorkAction.ActionOptions);
  finally
    SetLastActionStatus(Result, WorkAction.FindControlOptions.AllowToFail);
  end;
end;


function TActionExecution.ExecuteFindSubControlActionAsString(AListOfFindSubControlOptionsParams: TStrings): Boolean;
var
  WorkAction: TClkActionRec;
  Err: string;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    Err := SetFindSubControlActionProperties(AListOfFindSubControlOptionsParams, AddToLog, WorkAction.FindSubControlOptions, WorkAction.ActionOptions);
    if Err <> '' then
    begin
      SetActionVarValue('$ExecAction_Err$', Err);
      Exit;
    end;

    if AListOfFindSubControlOptionsParams.Values[CREParam_UseServerDebugging] = '1' then
    begin
      GetActionOptionsFromParams(AListOfFindSubControlOptionsParams, WorkAction);
      WorkAction.ActionOptions.Action := acFindSubControl;

      DoOnWaitInDebuggingMode(WorkAction, asiNo);
    end;

    Result := ExecuteFindSubControlActionWithTimeout(WorkAction.FindSubControlOptions, WorkAction.ActionOptions);
  finally
    SetLastActionStatus(Result, WorkAction.FindSubControlOptions.AllowToFail);
  end;
end;


function TActionExecution.ExecuteSetControlTextActionAsString(AListOfSetControlTextOptionsParams: TStrings): Boolean;
var
  WorkAction: TClkActionRec;
  Err: string;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    Err := SetSetControlTextActionProperties(AListOfSetControlTextOptionsParams, WorkAction.SetTextOptions);
    if Err <> '' then
    begin
      SetActionVarValue('$ExecAction_Err$', Err);
      Exit;
    end;

    if AListOfSetControlTextOptionsParams.Values[CREParam_UseServerDebugging] = '1' then
    begin
      GetActionOptionsFromParams(AListOfSetControlTextOptionsParams, WorkAction);
      WorkAction.ActionOptions.Action := acSetControlText;
      DoOnWaitInDebuggingMode(WorkAction, asiNo);
    end;

    Result := ExecuteSetControlTextAction(WorkAction.SetTextOptions);
  finally
    SetLastActionStatus(Result, False);
  end;
end;


function TActionExecution.ExecuteCallTemplateActionAsString(AListOfCallTemplateOptionsParams: TStrings): Boolean;
var
  WorkAction: TClkActionRec;
  IsDebugging: Boolean;
  Err: string;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    IsDebugging := AListOfCallTemplateOptionsParams.Values[CREParam_IsDebugging] = '1';

    Err := SetCallTemplateActionProperties(AListOfCallTemplateOptionsParams, WorkAction.CallTemplateOptions);
    if Err <> '' then
    begin
      SetActionVarValue('$ExecAction_Err$', Err);
      Exit;
    end;

    if AListOfCallTemplateOptionsParams.Values[CREParam_UseServerDebugging] = '1' then
    begin
      IsDebugging := True; //this allows the debugger to step into a CallTemplate action
      //FUseLocalDebugger := True; is set to True in main window (HTTP handler) if CREParam_UseServerDebugging is set

      GetActionOptionsFromParams(AListOfCallTemplateOptionsParams, WorkAction);
      WorkAction.ActionOptions.Action := acCallTemplate;
      DoOnWaitInDebuggingMode(WorkAction, asiYes);
    end;

    Result := ExecuteLoopedCallTemplateAction(WorkAction.CallTemplateOptions, IsDebugging, IsDebugging); //not sure if AShouldStopAtBreakPoint should be the same as IsDebugging or if it should be another http param

    if not Result then
      AddToLog(DateTimeToStr(Now) + '  /ExecuteCallTemplateAction is False. $ExecAction_Err$: ' + EvaluateReplacements('$ExecAction_Err$'))
    else
      AddToLog(DateTimeToStr(Now) + '  /ExecuteCallTemplateAction is True.');
  finally
    //SetLastActionStatus(Result, False);  //leave the action status as set by the called template
  end;
end;


function TActionExecution.ExecuteSleepActionAsString(AListOfSleepOptionsParams: TStrings): Boolean;
var
  WorkAction: TClkActionRec;
  Err: string;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    Err := SetSleepActionProperties(AListOfSleepOptionsParams, WorkAction.SleepOptions, WorkAction.ActionOptions);
    if Err <> '' then
    begin
      SetActionVarValue('$ExecAction_Err$', Err);
      Exit;
    end;

    if AListOfSleepOptionsParams.Values[CREParam_UseServerDebugging] = '1' then
    begin
      GetActionOptionsFromParams(AListOfSleepOptionsParams, WorkAction);
      WorkAction.ActionOptions.Action := acSleep;
      DoOnWaitInDebuggingMode(WorkAction, asiNo);
    end;

    Result := ExecuteSleepAction(WorkAction.SleepOptions, WorkAction.ActionOptions);
  finally
    SetLastActionStatus(Result, False);
  end;
end;


function TActionExecution.ExecuteSetVarActionAsString(AListOfSetVarOptionsParams: TStrings): Boolean;
var
  WorkAction: TClkActionRec;
  Err: string;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    Err := SetSetVarActionProperties(AListOfSetVarOptionsParams, WorkAction.SetVarOptions);
    if Err <> '' then
    begin
      SetActionVarValue('$ExecAction_Err$', Err);
      Exit;
    end;

    if AListOfSetVarOptionsParams.Values[CREParam_UseServerDebugging] = '1' then
    begin
      GetActionOptionsFromParams(AListOfSetVarOptionsParams, WorkAction);
      WorkAction.ActionOptions.Action := acSetVar;
      DoOnWaitInDebuggingMode(WorkAction, asiNo);
    end;

    Result := ExecuteSetVarAction(WorkAction.SetVarOptions);
  finally
    SetLastActionStatus(Result, False);
  end;
end;


function TActionExecution.ExecuteWindowOperationsActionAsString(AListOfWindowOperationsOptionsParams: TStrings): Boolean;
var
  WorkAction: TClkActionRec;
  Err: string;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    Err := SetWindowOperationsActionProperties(AListOfWindowOperationsOptionsParams, WorkAction.WindowOperationsOptions);
    if Err <> '' then
    begin
      SetActionVarValue('$ExecAction_Err$', Err);
      Exit;
    end;

    if AListOfWindowOperationsOptionsParams.Values[CREParam_UseServerDebugging] = '1' then
    begin
      GetActionOptionsFromParams(AListOfWindowOperationsOptionsParams, WorkAction);
      WorkAction.ActionOptions.Action := acWindowOperations;
      DoOnWaitInDebuggingMode(WorkAction, asiNo);
    end;

    Result := ExecuteWindowOperationsAction(WorkAction.WindowOperationsOptions);
  finally
    SetLastActionStatus(Result, False);
  end;
end;


function TActionExecution.ExecuteLoadSetVarFromFileActionAsString(AListOfLoadSetVarOptionsParams: TStrings): Boolean;
var
  WorkAction: TClkActionRec;
  Err: string;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    Err := SetLoadSetVarFromFileActionProperties(AListOfLoadSetVarOptionsParams, WorkAction.LoadSetVarFromFileOptions);
    if Err <> '' then
    begin
      SetActionVarValue('$ExecAction_Err$', Err);
      Exit;
    end;

    if AListOfLoadSetVarOptionsParams.Values[CREParam_UseServerDebugging] = '1' then
    begin
      GetActionOptionsFromParams(AListOfLoadSetVarOptionsParams, WorkAction);
      WorkAction.ActionOptions.Action := acLoadSetVarFromFile;
      DoOnWaitInDebuggingMode(WorkAction, asiNo);
    end;

    Result := ExecuteLoadSetVarFromFileAction(WorkAction.LoadSetVarFromFileOptions);
  finally
    SetLastActionStatus(Result, False);
  end;
end;


function TActionExecution.ExecuteSaveSetVarToFileActionAsString(AListOfSaveSetVarOptionsParams: TStrings): Boolean;
var
  WorkAction: TClkActionRec;
  Err: string;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    Err := SetSaveSetVarToFileActionProperties(AListOfSaveSetVarOptionsParams, WorkAction.SaveSetVarToFileOptions);
    if Err <> '' then
    begin
      SetActionVarValue('$ExecAction_Err$', Err);
      Exit;
    end;

    if AListOfSaveSetVarOptionsParams.Values[CREParam_UseServerDebugging] = '1' then
    begin
      GetActionOptionsFromParams(AListOfSaveSetVarOptionsParams, WorkAction);
      WorkAction.ActionOptions.Action := acSaveSetVarToFile;
      DoOnWaitInDebuggingMode(WorkAction, asiNo);
    end;

    Result := ExecuteSaveSetVarToFileAction(WorkAction.SaveSetVarToFileOptions);
  finally
    SetLastActionStatus(Result, False);
  end;
end;


function TActionExecution.ExecutePluginActionAsString(AListOfPluginOptionsParams: TStrings): Boolean;
var
  WorkAction: TClkActionRec;
  TempAllActions: PClkActionsRecArr;
  TempListOfAllVars: TStringList;
  IsDebugging: Boolean;
  Err: string;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    Err := SetPluginActionProperties(AListOfPluginOptionsParams, WorkAction.PluginOptions);
    if Err <> '' then
    begin
      SetActionVarValue('$ExecAction_Err$', Err);
      Exit;
    end;

    IsDebugging := AListOfPluginOptionsParams.Values[CREParam_IsDebugging] = '1';  //this is plugin debugging, which is different than UseServerDebugging
    TempAllActions := DoOnGetAllActions;

    if IsDebugging then
      AddToLog('IsDebugging flag, used for stepping into, is set for plugin "' + WorkAction.PluginOptions.FileName + '".')
    else
      AddToLog('IsDebugging flag, used for stepping into, is not set for plugin "' + WorkAction.PluginOptions.FileName + '".');

    if AListOfPluginOptionsParams.Values[CREParam_UseServerDebugging] = '1' then
    begin
      //IsDebugging := True; //Do not set for Plugin yet, because there is a waiting loop, which cannot be stopped by StepOver (only by Stop, which is not enabled)
                             //However, it seems that the flag is still needed to debug the plugin.
                             //It seems (for now) that plugin debugging starts after the call to remove the debugging action from list (see DoOnWaitInDebuggingMode), which might be needed for debugging the plugin.
                             //Keeping the action in the list does not fix the issue of "invisible" waiting loop. Plugin debugging buttons do not appear either. At least those would be helpful to stop the loop.
                             //The "Step into" button is enabled by this flag, so the execution should not get to that "invisible" loop.
      GetActionOptionsFromParams(AListOfPluginOptionsParams, WorkAction);
      WorkAction.ActionOptions.Action := acPlugin;
      DoOnWaitInDebuggingMode(WorkAction, TAllowsSteppingInto(Ord(IsDebugging)));
    end;

    TempListOfAllVars := TStringList.Create;
    try
      TempListOfAllVars.LineBreak := #13#10;
      DoOnBackupVars(TempListOfAllVars);
      Result := ExecutePluginAction(WorkAction.PluginOptions, TempAllActions, TempListOfAllVars, DoOnResolveTemplatePath(WorkAction.PluginOptions.FileName), IsDebugging, IsDebugging); //passing two IsDebugging params. ToDo:  review the logic
    finally
      TempListOfAllVars.Free;
    end;
  finally
    SetLastActionStatus(Result, False);
  end;
end;


function TActionExecution.ExecuteEditTemplateActionAsString(AListOfEditTemplateOptionsParams: TStrings): Boolean;
var
  WorkAction: TClkActionRec;
  Err: string;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    Err := SetEditTemplateActionProperties(AListOfEditTemplateOptionsParams, WorkAction.EditTemplateOptions, True);
    if Err <> '' then
    begin
      SetActionVarValue('$ExecAction_Err$', Err);
      Exit;
    end;

    if AListOfEditTemplateOptionsParams.Values[CREParam_UseServerDebugging] = '1' then
    begin
      GetActionOptionsFromParams(AListOfEditTemplateOptionsParams, WorkAction);
      WorkAction.ActionOptions.Action := acEditTemplate;
      DoOnWaitInDebuggingMode(WorkAction, asiNo);
    end;

    Result := ExecuteEditTemplateAction(WorkAction.EditTemplateOptions);
  finally
    SetLastActionStatus(Result, False);
  end;
end;


function SwapRGB(AColor: TColor): TColor;
var
  R, G, B: Byte;
begin
  RedGreenBlue(AColor, R, G, B);
  Result := RGBToColor(B, G, R); //swap here
end;


function TActionExecution.GetTextRenderingPage(AHTTPParams: TStrings): string;
var
  i: Integer;
  ProfilesLen: Integer;
begin
  EnterCriticalSection(FBrowserRenderingText.CritSec);
  try
    if AHTTPParams.Values[CREParam_ID] <> FBrowserRenderingText.RequestID then
      raise Exception.Create('Requested wrong page ID: ' + AHTTPParams.Values[CREParam_ID] + '.  Expected: ' + FBrowserRenderingText.RequestID + '  StackLevel: ' + IntToStr(FStackLevel^));

    Result := '<!DOCTYPE html>'#13#10 +
              '<html lang="en-US">'#13#10 +
              '  <meta charset="utf-8">'#13#10;

    ProfilesLen := Length(FBrowserRenderingText.FontProfiles);

    Result := Result +
              ''#13#10 +
              '  <body>'#13#10 +
              '  <script>'#13#10 +
              //Insert a timer here, to close the tab after a while. It would be nice to wait for all requests to get their responses.

              '    //FontProfile count is ' + IntToStr(ProfilesLen) + #13#10 +
              '    let ForegroundColors = [';
    for i := 0 to ProfilesLen - 1 do
    begin
      Result := Result + '"#' + IntToHex(SwapRGB(HexToInt(FBrowserRenderingText.FontProfiles[i].ForegroundColor)), 6) + '"';
      if i < ProfilesLen - 1 then
        Result := Result + ',';
    end;
    Result := Result + '];'#13#10 +

               '    let BackgroundColors = [';
    for i := 0 to ProfilesLen - 1 do
    begin
      Result := Result + '"#' + IntToHex(SwapRGB(HexToInt(FBrowserRenderingText.FontProfiles[i].BackgroundColor)), 6) + '"';
      if i < ProfilesLen - 1 then
        Result := Result + ',';
    end;
    Result := Result + '];'#13#10 +

               '    let FontSizes = [';
    for i := 0 to ProfilesLen - 1 do
    begin
      Result := Result + {'"' +} IntToStr(FBrowserRenderingText.FontProfiles[i].FontSize) {+ '"'};
      if i < ProfilesLen - 1 then
        Result := Result + ',';
    end;
    Result := Result + '];'#13#10 +

               '    let FontNames = [';
    for i := 0 to ProfilesLen - 1 do
    begin
      Result := Result + '"' + FBrowserRenderingText.FontProfiles[i].FontName + '"';
      if i < ProfilesLen - 1 then
        Result := Result + ',';
    end;
    Result := Result + '];'#13#10 +

               '    let FontBolds = [';
    for i := 0 to ProfilesLen - 1 do
    begin
      Result := Result + '"' + BoolToStr(FBrowserRenderingText.FontProfiles[i].Bold, 'bold ', '') + '"';
      if i < ProfilesLen - 1 then
        Result := Result + ',';
    end;
    Result := Result + '];'#13#10 +

               '    let FontItalics = [';
    for i := 0 to ProfilesLen - 1 do
    begin
      Result := Result + '"' + BoolToStr(FBrowserRenderingText.FontProfiles[i].Italic, 'italic ', '') + '"';
      if i < ProfilesLen - 1 then
        Result := Result + ',';
    end;
    Result := Result + '];'#13#10 +

               '    let FileNames = [';
    for i := 0 to ProfilesLen - 1 do
    begin
      Result := Result + '"' + FBrowserRenderingText.RenderedFileNames[i] + '"';
      if i < ProfilesLen - 1 then
        Result := Result + ',';
    end;
    Result := Result + '];'#13#10 +

    '    let Results = [';
    for i := 0 to ProfilesLen - 1 do
    begin
      Result := Result + 'false';
      if i < ProfilesLen - 1 then
        Result := Result + ',';
    end;
    Result := Result + '];'#13#10;

    if FRenderingRequestPageCloseBrowserOnDone then
      Result := Result +
              ''#13#10 +
              '    function CloseIfAllRequestsAreDone(AResults) {'#13#10 +
              '      let AllResults = true;'#13#10 +
              '      for (let i = 0; i < ' + IntToStr(ProfilesLen) + '; i++) {'#13#10 +
              '        if (!AResults[i]) {'#13#10 +
              '          AllResults = false;'#13#10 +
              '          console.log("Still waiting for response at [" + i + "]..");'#13#10 +
              '          break;'#13#10 +
              '        }'#13#10 +
              '      }'#13#10 +
              '      if (AllResults) {'#13#10 +
              '        console.log("Closing..");'#13#10 +
              '        window.close();'#13#10 +
              '      }'#13#10 +
              '    }'#13#10 +
              ''#13#10
    else
      Result := Result +
              '// The CloseIfAllRequestsAreDone function is not used.'#13#10;

    Result := Result +
              '    for (let i = 0; i < ' + IntToStr(ProfilesLen) + '; i++) {'#13#10;

    Result := Result +

              '      let canvas = document.createElement("canvas");'#13#10 +
              '      canvas.id = "txt" + i;'#13#10 +
              '      document.body.appendChild(canvas);'#13#10 +

              '      let context = canvas.getContext("2d");'#13#10 +
              ''#13#10 +
              '      let fontAsStr = FontBolds[i] + FontItalics[i] + FontSizes[i] + "' + FBrowserRenderingText.FontSizeUnit + ' " + FontNames[i];'#13#10 +
              '      context.font = fontAsStr;'#13#10 +
              '      context.textBaseline = "top";'#13#10 +
              '      context.textAlign = "left";'#13#10 +

              '      let textMeasurement = context.measureText("' + FBrowserRenderingText.Txt + '");'#13#10 +
              '      canvas.width = textMeasurement.width + 3;'#13#10;

    if FBrowserRenderingText.FontSizeUnit = 'pt' then
      Result := Result +
              '      canvas.height = FontSizes[i] * 1.62;'#13#10   //approx 1.62 when using pt, instead of px
    else
      Result := Result +
              '      canvas.height = FontSizes[i];'#13#10;

    Result := Result +
              '      context.font = fontAsStr;'#13#10 +     //set font and its properties again, after setting canvas size
              '      context.textBaseline = "top";'#13#10 +
              '      context.textAlign = "left";'#13#10 +

              ''#13#10 +
              '      context.fillStyle = BackgroundColors[i];'#13#10 +
              '      context.fillRect(0, 0, canvas.width - 1, canvas.height - 1); //background'#13#10 +
              ''#13#10 +
              '      context.fillStyle = ForegroundColors[i];'#13#10 +
              '      context.fillText("' + FBrowserRenderingText.Txt + '", 0, 0);'#13#10 +
              ''#13#10 +
              '      let imgContent = canvas.toDataURL();'#13#10 +
              '      const xhr = new XMLHttpRequest();'#13#10 +
              '      xhr.onload = () => {'#13#10 +
              '        console.log("set image response[" + i + "]: " + xhr.response);'#13#10 +
              '        Results[i] = true;'#13#10;

    if FRenderingRequestPageCloseBrowserOnDone then
      Result := Result +
              '        CloseIfAllRequestsAreDone(Results);'#13#10;

    Result := Result +
              '      }'#13#10 +
              ''#13#10 +
              '      xhr.onerror = () => {'#13#10 +
              '        console.error("set image error. Maybe the request string is too long (> MaxLineLength). (Because the rendered text is too complex.)");'#13#10 +
              '        Results[i] = true;'#13#10;

    if FRenderingRequestPageCloseBrowserOnDone then
      Result := Result +
              '        CloseIfAllRequestsAreDone(Results);'#13#10;  //comment this line if the browser should stay open in case of a failed request

    Result := Result +
              '      }'#13#10 +
              ''#13#10 +

              '      imgContent = imgContent.replaceAll("=", "");'#13#10 +   //the HTTP parameter parser messes things up, because of '='
              '      imgContent = imgContent.replaceAll("+", "");'#13#10 +   //the HTTP parameter parser messes things up, because of '+'
              '      imgContent = imgContent.replaceAll("/", "");'#13#10 +   //the HTTP parameter parser messes things up, because of '/'

              '      xhr.open("GET", "/' + CRECmd_SetRenderedFileB64 + '?StackLevel=' + IntToStr(FStackLevel^) + '&' + CREParam_FileName + '=" + FileNames[i] + "&' + CREParam_Content + '=" + imgContent + "&Dummy=dummy", true);'#13#10 +
              '      xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded");'#13#10 +
              '      xhr.send(null);'#13#10;
    Result := Result +
              '    } //for'#13#10 +
              ''#13#10 +
              '  </script>'#13#10 +
              '  </body>'#13#10 +
              '</html>';
  finally
    LeaveCriticalSection(FBrowserRenderingText.CritSec);
  end;
end;


//some handlers for primitives compositor
function TActionExecution.HandleOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  Result := DoOnLoadBitmap(ABitmap, AFileName)
end;


function TActionExecution.HandleOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  Result := DoOnLoadRenderedBitmap(ABitmap, AFileName);
end;


function TActionExecution.HandleOnEvaluateReplacements(s: string; Recursive: Boolean = True; AEvalTextCount: Integer = -1): string;
begin
  Result := EvaluateReplacements(s, Recursive, AEvalTextCount);
end;


procedure TActionExecution.HandleOnSetVar(AVarName, AVarValue: string);
begin
  SetActionVarValue(AVarName, AVarValue);
end;


procedure TActionExecution.HandleOnSetDebugPoint(ADebugPoint: string);
begin
  DoOnSetDebugPoint(ADebugPoint);
end;


function TActionExecution.HandleOnIsAtBreakPoint(ADebugPoint: string): Boolean;
begin
  Result := DoOnIsAtBreakPoint(ADebugPoint);
end;


procedure TActionExecution.HandleOnSaveFileToExtRenderingInMemFS(AFileName: string; AContent: Pointer; AFileSize: Int64);
begin
  DoOnSaveFileToExtRenderingInMemFS(AFileName, AContent, AFileSize);
end;


function TActionExecution.HandleOnScreenshotByActionName(AActionName: string): Boolean;
var
  ActionContent: PClkActionRec;
  AActionOptions: TClkActionOptions;
  FindControlInputData: TFindControlInputData;
  TxtProfileCount: Integer;
  CompAtPoint: TCompRec;
  tp: TPoint;
  ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height, CompWidth, CompHeight: Integer;
  MemStream: TMemoryStream;
begin
  Result := False;

  // This screenshot works with FindSubControl only (no FindControl). Its input settings should be the values of all variables set by a FindControl action, executed prior to this screenshot.

  ActionContent := DoOnGetActionProperties(AActionName);

  if ActionContent = nil then
  begin
    AddToLog('Action not found (' + AActionName + ') when taking a screenshot.');
    Exit;
  end;

  tp.X := 0; //init here
  tp.Y := 0;

  AActionOptions := ActionContent^.ActionOptions;

  if ActionContent^.ActionOptions.Action = acFindControl then
  begin
    AddToLog('Taking screenshot using FindControl... '); //wrong - bug
    if not FillInFindControlInputData(ActionContent^.FindControlOptions, AActionOptions, FindControlInputData, TxtProfileCount) then
      Exit;
  end;

  if ActionContent^.ActionOptions.Action = acFindSubControl then
    if not FillInFindSubControlInputData(ActionContent^.FindSubControlOptions, AActionOptions, FindControlInputData, TxtProfileCount) then
      Exit;

  tp.X := FindControlInputData.GlobalSearchArea.Left;
  tp.Y := FindControlInputData.GlobalSearchArea.Top;
  AddToLog('Taking screenshot by action: ' + AActionName);

  CompAtPoint := GetWindowClassRec(tp);
  CompAtPoint.XOffsetFromParent := 0;
  CompAtPoint.YOffsetFromParent := 0;

  ComputeScreenshotArea(FindControlInputData, CompAtPoint, ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height, CompWidth, CompHeight);

  if FindControlInputData.DebugBitmap <> nil then
  begin
    if FindControlInputData.CropFromScreenshot then
    begin
      //ScreenShot(CompAtPoint.Handle, FindControlInputData.DebugBitmap, ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height)
      CroppedFullScreenShot(CompAtPoint, FindControlInputData, CompWidth, CompHeight);
    end
    else
      ScreenShot(CompAtPoint.Handle, FindControlInputData.DebugBitmap, 0, 0, CompAtPoint.ComponentRectangle.Width, CompAtPoint.ComponentRectangle.Height);

    MemStream := TMemoryStream.Create;
    try
      FindControlInputData.DebugBitmap.SaveToStream(MemStream);
      DoOnSaveFileToExtRenderingInMemFS(CScreenshotFilename, MemStream.Memory, MemStream.Size);
    finally
      MemStream.Free;
    end;

    AddToLog('ScrShot_Left: ' + IntToStr(ScrShot_Left) + '  ScrShot_Top: ' + IntToStr(ScrShot_Top));
    AddToLog('CompWidth: ' + IntToStr(CompWidth) + '  CompHeight: ' + IntToStr(CompHeight) + '  CompAtPoint.Width: ' + IntToStr(CompAtPoint.ComponentRectangle.Width) + '  CompAtPoint.Height: ' + IntToStr(CompAtPoint.ComponentRectangle.Height));
    AddToLog('CompHandle: ' + IntToStr(CompAtPoint.Handle) + '  ControlClass: ' + CompAtPoint.ClassName + '  CropFromScreenshot: ' + BoolToStr(FindControlInputData.CropFromScreenshot, 'True', 'False'));

    Result := True;
  end
  else
    AddToLog('DebugBitmap is not assigned. No screenshot taken.');
end;


end.

