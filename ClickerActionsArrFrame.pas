{
    Copyright (C) 2023 VCC
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


unit ClickerActionsArrFrame;

{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, {Messages,} SysUtils, Variants, Classes, Graphics, Controls, Forms, Types,
  Dialogs, ClickerActionsFrame, StdCtrls, VirtualTrees, ExtCtrls, Buttons,
  ImgList, Menus, ComCtrls, IdHTTP, ClickerIniFiles, ClickerUtils, InMemFileSystem,
  ClickerActionsPaletteFrame, ClickerActionExecution, PollingFIFO, ClickerPrimitiveUtils;

type
  TOnExecuteRemoteActionAtIndex = function(AActionIndex, AStackLevel: Integer; AVarReplacements: TStringList; AIsDebugging: Boolean): Boolean of object;
  //TOnGetRemoteReplacementVars = procedure(AValLst: TValueListEditor) of object;
  TOnWaitForFileAvailability = procedure(AFileName: string) of object; //called in server mode, to add a filename to FIFO and wait until file exists
  TOnWaitForMultipleFilesAvailability = procedure(AListOfFiles: TStringList) of object;

  { TfrClickerActionsArr }

  TfrClickerActionsArr = class(TFrame)
    chkEnableDebuggerKeys: TCheckBox;
    chkResetVarsOnPlayAll: TCheckBox;
    chkShowActionNumber: TCheckBox;
    edtConsoleCommand: TEdit;
    imgWaitingInDebuggingMode: TImage;
    imglstActionHasCondition: TImageList;
    imglstActionExtraStatus: TImageList;
    imglstCurrentDebuggingActionWithBreakPoint: TImageList;
    imglstCurrentDebuggingActionWithDisabledBreakPoint: TImageList;
    imgWaitingInPreDebuggingMode: TImage;
    lbeSearchAction: TLabeledEdit;
    lblModifiedStatus: TLabel;
    memLogErr: TMemo;
    MenuItem_AddRestoreCachedControlAction: TMenuItem;
    MenuItem_AddCacheControlAction: TMenuItem;
    MenuItem_InMemReceivedAsServer: TMenuItem;
    MenuItem_RecentFiles: TMenuItem;
    MenuItem_UpdateFromOI: TMenuItem;
    MenuItem_SetActionStatusToFailed: TMenuItem;
    MenuItem_SetActionStatusToAllowedFailed: TMenuItem;
    MenuItem_SetActionStatusToSuccessful: TMenuItem;
    MenuItem_SetActionStatusTo: TMenuItem;
    N3: TMenuItem;
    MenuItem_PlayActionAndRestoreVars: TMenuItem;
    MenuItem_AddACallTemplateByFile: TMenuItem;
    N2: TMenuItem;
    MenuItem_EditBreakPoint: TMenuItem;
    MenuItemEnableDisableBreakPoint: TMenuItem;
    MenuItemPasteActionsFromClipboard: TMenuItem;
    MenuItemCopySelectedActionsToClipboard: TMenuItem;
    N1: TMenuItem;
    MenuItem_ReplaceSelectedActionsWithATemplateCall: TMenuItem;
    MenuItem_RefactorSelectedActionsIntoATemplate: TMenuItem;
    pnlPalette: TPanel;
    pnlVertSplitter: TPanel;
    pnlActions: TPanel;
    pnlActionsEditor: TPanel;
    pmVstActions: TPopupMenu;
    pmBreakPoint: TPopupMenu;
    pnlfrClickerActions: TPanel;
    pmExtraRemove: TPopupMenu;
    pnlvstActions: TPanel;
    pmExtraPlayAction: TPopupMenu;
    pmExtraLoad: TPopupMenu;
    Removeallactions1: TMenuItem;
    imglstActionStatus: TImageList;
    pmExtraAdd: TPopupMenu;
    InsertActionBeforeSelected1: TMenuItem;
    InsertActionAfterSelected1: TMenuItem;
    pmExtraSave: TPopupMenu;
    SaveTemplateAs1: TMenuItem;
    pmExtraPlayAll: TPopupMenu;
    PlayAllInDebuggingMode1: TMenuItem;
    spdbtnContinuePlayingAll: TSpeedButton;
    spdbtnExtraAdd: TSpeedButton;
    spdbtnExtraPlayAll: TSpeedButton;
    spdbtnExtraPlayAction: TSpeedButton;
    spdbtnExtraRemove: TSpeedButton;
    spdbtnExtraSave: TSpeedButton;
    spdbtnExtraLoad: TSpeedButton;
    spdbtnLoadTemplate: TSpeedButton;
    spdbtnMoveDown: TSpeedButton;
    spdbtnMoveUp: TSpeedButton;
    spdbtnPlayAllActions: TSpeedButton;
    spdbtnPlaySelectedAction: TSpeedButton;
    spdbtnSaveTemplate: TSpeedButton;
    spdbtnStepInto: TSpeedButton;
    spdbtnStepOver: TSpeedButton;
    spdbtnStopPlaying: TSpeedButton;
    spdbtnUpdateAction: TSpeedButton;
    spdbtnPalette: TSpeedButton;
    spdbtnTemplateNotes: TSpeedButton;
    spdbtnAddAction: TSpeedButton;
    spdbtnRemoveAction: TSpeedButton;
    spdbtnNew: TSpeedButton;
    tmrEditActionsVST: TTimer;
    tmrLogging: TTimer;
    tmrDeleteActions: TTimer;
    tmrExecActionFromSrvModule: TTimer;
    tmrGlowUpdateButton: TTimer;
    tmrDebugKeys: TTimer;
    Removeallactionsandclearfilename1: TMenuItem;
    PlayAllInDebuggingModeStartingAtSelected1: TMenuItem;
    imglstCurrentDebuggingAction: TImageList;
    procedure btnAddActionClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure edtConsoleCommandExit(Sender: TObject);
    procedure edtConsoleCommandKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtConsoleCommandKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FrameResize(Sender: TObject);
    procedure MenuItemCopySelectedActionsToClipboardClick(Sender: TObject);
    procedure MenuItemEnableDisableBreakPointClick(Sender: TObject);
    procedure MenuItemPasteActionsFromClipboardClick(Sender: TObject);
    procedure MenuItem_AddACallTemplateByFileClick(Sender: TObject);
    procedure MenuItem_AddCacheControlActionClick(Sender: TObject);
    procedure MenuItem_AddRestoreCachedControlActionClick(Sender: TObject);
    procedure MenuItem_EditBreakPointClick(Sender: TObject);
    procedure MenuItem_PlayActionAndRestoreVarsClick(Sender: TObject);
    procedure MenuItem_RefactorSelectedActionsIntoATemplateClick(Sender: TObject
      );
    procedure MenuItem_ReplaceSelectedActionsWithATemplateCallClick(Sender: TObject
      );
    procedure MenuItem_SetActionStatusToAllowedFailedClick(Sender: TObject);
    procedure MenuItem_SetActionStatusToFailedClick(Sender: TObject);
    procedure MenuItem_SetActionStatusToSuccessfulClick(Sender: TObject);
    procedure pnlActionsClick(Sender: TObject);
    procedure pnlVertSplitterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlVertSplitterMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlVertSplitterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure spdbtnExtraLoadClick(Sender: TObject);
    procedure spdbtnExtraPlayActionClick(Sender: TObject);
    procedure spdbtnPaletteClick(Sender: TObject);
    procedure spdbtnTemplateNotesClick(Sender: TObject);
    procedure spdbtnUpdateActionClick(Sender: TObject);
    procedure tmrDeleteActionsTimer(Sender: TObject);
    procedure tmrEditActionsVSTTimer(Sender: TObject);
    procedure tmrExecActionFromSrvModuleTimer(Sender: TObject);
    procedure tmrLoggingTimer(Sender: TObject);
    procedure vstActionsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vstActionsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure vstActionsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnRemoveActionClick(Sender: TObject);
    procedure vstActionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: {$IFDEF FPC} string {$ELSE} WideString {$ENDIF});
    procedure vstActionsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
    procedure vstActionsPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure vstActionsBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure spdbtnMoveUpClick(Sender: TObject);
    procedure spdbtnMoveDownClick(Sender: TObject);
    procedure vstActionsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure btnLoadTemplateClick(Sender: TObject);
    procedure btnSaveTemplateClick(Sender: TObject);
    procedure Removeallactions1Click(Sender: TObject);
    procedure spdbtnExtraRemoveClick(Sender: TObject);
    procedure lbeSearchActionChange(Sender: TObject);
    procedure vstActionsGetImageIndexEx(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer;
      var ImageList: TCustomImageList);
    procedure vstActionsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstActionsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstActionsDblClick(Sender: TObject);
    procedure vstActionsEditCancelled(Sender: TBaseVirtualTree; Column: TColumnIndex);
    procedure vstActionsEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vstActionsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure spdbtnExtraAddClick(Sender: TObject);
    procedure InsertActionBeforeSelected1Click(Sender: TObject);
    procedure InsertActionAfterSelected1Click(Sender: TObject);
    procedure spdbtnExtraSaveClick(Sender: TObject);
    procedure SaveTemplateAs1Click(Sender: TObject);
    procedure spdbtnPlaySelectedActionClick(Sender: TObject);
    procedure spdbtnPlayAllActionsClick(Sender: TObject);
    procedure spdbtnStopPlayingClick(Sender: TObject);
    procedure spdbtnExtraPlayAllClick(Sender: TObject);
    procedure PlayAllInDebuggingMode1Click(Sender: TObject);
    procedure spdbtnContinuePlayingAllClick(Sender: TObject);
    procedure spdbtnStepOverClick(Sender: TObject);
    procedure spdbtnStepIntoClick(Sender: TObject);
    procedure tmrGlowUpdateButtonTimer(Sender: TObject);
    procedure vstActionsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chkEnableDebuggerKeysClick(Sender: TObject);
    procedure tmrDebugKeysTimer(Sender: TObject);
    procedure Removeallactionsandclearfilename1Click(Sender: TObject);
    procedure PlayAllInDebuggingModeStartingAtSelected1Click(Sender: TObject);
    procedure chkShowActionNumberClick(Sender: TObject);
  private
    { Private declarations }
    FClkActions: TClkActionsRecArr;
    FTemplateNotes: string;
    FActionExecution: TActionExecution;
    FEditingText: string;

    FModified: Boolean;
    FStopAllActionsOnDemand: Boolean;
    FStopAllActionsOnDemandFromParent: PBoolean;
    //FCallerName: string;
    FOnCallTemplate: TOnCallTemplate;
    FFileName: string;
    FPlaying: Boolean;
    FPlayingAllActions: Boolean;  //similar to FPlaying, but this is set internally (PlayAllActions)
    FDebugging: Boolean;
    FContinuePlayingAll: Boolean;
    FContinuePlayingNext: Boolean;
    FContinuePlayingBySteppingInto: Boolean;
    FShouldStopAtBreakPoint: Boolean;
    FUpdateButtonGlowDirection: Boolean;
    FPreviousSelectedNode: PVirtualNode;
    FActionsHitInfo: THitInfo;
    FActionsHitTimeStamp: QWord; //required, to detect fake double-clicks

    FVarDescriptions: TStringList;
    FFuncDescriptions: TStringList;

    F2_State: Byte;
    F6_State: Byte;
    F7_State: Byte;
    F8_State: Byte;
    F9_State: Byte;

    FFullTemplatesDir: string;
    FCmdConsoleHistory: TStringList;

    FExecutesRemotely: Boolean;
    FStackLevel: Integer;
    FOnExecuteRemoteActionAtIndex: TOnExecuteRemoteActionAtIndex;
    //FOnGetRemoteReplacementVars: TOnGetRemoteReplacementVars;

    FRemoteExActionIndex: Integer;
    FRemoteExCmdResult: Boolean;
    FExecutingActionFromRemote: Boolean;
    FUseLocalDebugger: Boolean;
    FFileLocationOfDepsIsMem: Boolean;
    FClosingTemplate: Boolean;  //set to true by ExitTemplateFromRemote when the template should be closed (after it stays open as called template)

    FRemoteAddress: string; //and port

    FHold: Boolean; //for splitter
    FSplitterMouseDownImagePos: TPoint;
    FSplitterMouseDownGlobalPos: TPoint;

    FOnCopyControlTextAndClassFromMainWindow: TOnCopyControlTextAndClassFromMainWindow;
    FOnGetExtraSearchAreaDebuggingImageWithStackLevel: TOnGetExtraSearchAreaDebuggingImageWithStackLevel;
    FOnWaitForFileAvailability: TOnWaitForFileAvailability;
    FOnWaitForMultipleFilesAvailability: TOnWaitForMultipleFilesAvailability;
    FOnWaitForBitmapsAvailability: TOnWaitForBitmapsAvailability;
    FOnLoadBitmap: TOnLoadBitmap;
    FOnLoadRenderedBitmap: TOnLoadRenderedBitmap;
    FOnRenderBmpExternally: TOnRenderBmpExternally;
    FOnGetListOfExternallyRenderedImages: TOnGetListOfExternallyRenderedImages;
    FOnLoadPrimitivesFile: TOnLoadPrimitivesFile;
    FOnSavePrimitivesFile: TOnSavePrimitivesFile;

    FOnFileExists: TOnFileExists;
    FOnTClkIniReadonlyFileCreate: TOnTClkIniReadonlyFileCreate;
    FOnSaveTemplateToFile: TOnSaveTemplateToFile;

    FOnSetOpenDialogMultiSelect: TOnSetOpenDialogMultiSelect;
    FOnSetOpenDialogInitialDir: TOnSetOpenDialogInitialDir;
    FOnOpenDialogExecute: TOnOpenDialogExecute;
    FOnGetOpenDialogFileName: TOnGetOpenDialogFileName;
    FOnSetSaveDialogInitialDir: TOnSetOpenDialogInitialDir;
    FOnSaveDialogExecute: TOnOpenDialogExecute;
    FOnGetSaveDialogFileName: TOnGetOpenDialogFileName;
    FOnSetSaveDialogFileName: TOnSetOpenDialogFileName;

    FOnSetPictureSetOpenDialogMultiSelect: TOnSetPictureSetOpenDialogMultiSelect;
    FOnSetPictureOpenDialogInitialDir: TOnSetPictureOpenDialogInitialDir;
    FOnPictureOpenDialogExecute: TOnPictureOpenDialogExecute;
    FOnGetPictureOpenDialogFileName: TOnGetPictureOpenDialogFileName;

    FOnGetGridDrawingOption: TOnGetGridDrawingOption;
    FOnGetFontFinderSettings: TOnRWFontFinderSettings;
    FOnSetFontFinderSettings: TOnRWFontFinderSettings;

    FOnRetrieveRenderedBmpFromServer: TOnRetrieveRenderedBmpFromServer;

    vstActions: TVirtualStringTree;
    FPalette: TfrClickerActionsPalette;
    FLoggingFIFO: TPollingFIFO;

    procedure GetListOfUsedFilesFromLoadedTemplate(AListOfFiles: TStringList);

    procedure HandleActionSelection; //does not handle an event

    procedure HandleOnSetEditorEnabledState(AEnabled: Boolean);
    procedure HandleOnSetEditorTimeoutProgressBarMax(AMaxValue: Integer);
    procedure HandleOnSetEditorTimeoutProgressBarPosition(APositionValue: Integer);
    procedure HandleOnWaitForBitmapsAvailability(ListOfBitmapFiles: TStringList);
    function HandleOnCallTemplate(Sender: TObject; AFileNameToCall: string; ListOfVariables: TStrings; DebugBitmap: TBitmap; DebugGridImage: TImage; IsDebugging, AShouldStopAtBreakPoint: Boolean; AStackLevel: Integer; AExecutesRemotely: Boolean): Boolean;
    procedure HandleOnSetEditorSleepProgressBarMax(AMaxValue: Integer);
    procedure HandleOnSetEditorSleepProgressBarPosition(APositionValue: Integer);
    procedure HandleOnSetEditorSleepInfo(AElapsedTime, ARemainingTime: string);
    procedure HandleOnAddDefaultFontProfile(var AFindControlOptions: TClkFindControlOptions; var AActionOptions: TClkActionOptions);
    function HandleOnGetGridDrawingOption: TDisplayGridLineOption;

    function HandleOnEditCallTemplateBreakCondition(var AActionCondition: string): Boolean;
    function HandleOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function HandleOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function HandleOnRenderBmpExternally(AFilename: string): string;
    function HandleOnGetActionProperties(AActionName: string): PClkActionRec;
    procedure HandleOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages: TStringList);
    procedure HandleOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    procedure HandleOnSavePrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    function HandleOnFileExists(const AFileName: string): Boolean;

    procedure HandleOnSetOpenDialogMultiSelect;
    procedure HandleOnSetOpenDialogInitialDir(AInitialDir: string);
    function HandleOnOpenDialogExecute(AFilter: string): Boolean;
    function HandleOnGetOpenDialogFileName: string;
    procedure HandleOnSetSaveDialogInitialDir(AInitialDir: string);
    function HandleOnSaveDialogExecute(AFilter: string): Boolean;
    function HandleOnGetSaveDialogFileName: string;
    procedure HandleOnSetSaveDialogFileName(AFileName: string);

    procedure HandleOnSetPictureSetOpenDialogMultiSelect;
    procedure HandleOnSetPictureOpenDialogInitialDir(AInitialDir: string);
    function HandleOnPictureOpenDialogExecute: Boolean;
    function HandleOnGetPictureOpenDialogFileName: string;

    function HandleOnExecuteFindSubControlAction(AErrorLevel, AErrorCount, AFastSearchErrorCount: Integer; AFontName: string; AFontSize: Integer; out AFoundArea: TRect): Boolean;
    procedure HandleOnAddToLog(s: string);
    procedure HandleOnGetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);
    procedure HandleOnSetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);

    function GetInMemFS: TInMemFileSystem;
    procedure SetInMemFS(Value: TInMemFileSystem);

    procedure SetGridDrawingOption(Value: TDisplayGridLineOption);
    procedure SetPreviewSelectionColors(Value: TSelectionColors);

    procedure FillInWithAllVars(AListOfVars: TStringList);
    procedure FillInWithAllFuncs(AListOfVars: TStringList);
    procedure FillInVarAndFuncDescriptions;
    procedure CreateRemainingUIComponents;

    procedure LoadTemplateWithUIUpdate(AFileName: string; AFileLocation: TFileLocation = flDisk; AInMemFileSystem: TInMemFileSystem = nil);
    procedure SetFullTemplatesDir(Value: string);
    procedure SetModified(Value: Boolean);
    procedure UpdateModifiedLabel;
    procedure SaveTemplateIfModified;

    procedure ResizeFrameSectionsBySplitter(NewTop: Integer);

    procedure UpdateActionsArrFromControls(ActionIndex: Integer);
    procedure UpdateControlsFromActionsArr(ActionIndex: Integer);

    procedure FPaletteVstMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FPaletteVstMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FPaletteVsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure ExtraLoadInMemFileClick(Sender: TObject);

    procedure UpdateNodesCheckStateFromActions;
    procedure RemoveAction(ActionIndex: Integer; AClearSelectionAfterRemoving: Boolean = True);
    procedure RemoveSelectedActions;
    procedure SetAFontFromClkActions(AFont: TFont; ActionIndex: Integer);
    procedure UpdateNodeCheckStateFromAction(Node: PVirtualNode);

    procedure LoadTemplate_V1(Ini: TClkIniReadonlyFile);
    procedure LoadTemplate_V2(Ini: TClkIniReadonlyFile);
    procedure SaveTemplateWithCustomActions_V2(Fnm: string; var ACustomClkActions: TClkActionsRecArr; ANotes: string);
    procedure SaveTemplate(Fnm: string);

    procedure DisplayDefaultEvalConsoleEditBox;
    function EvaluateAssignmentExpression: Boolean;
    function EvaluateActionCondition(ActionIndex: Integer): Boolean;

    procedure PrepareFilesInServer;
    function ExecuteActionAtIndex(AActionIndex: Integer): Boolean; //can be called by a server module (used when Clicker is in server mode)
    function LocalOnExecuteRemoteActionAtIndex(AActionIndex, AStackLevel: Integer; AVarReplacements: TStringList; AIsDebugging: Boolean): Boolean;
    function DoExecuteRemoteActionAtIndex(AActionIndex: Integer): Boolean;
    procedure DoWaitForFileAvailability(AFileName: string);
    procedure DoWaitForMultipleFilesAvailability(AListOfFiles: TStringList);
    procedure DoWaitForBitmapsAvailability(AListOfFiles: TStringList);
    function DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function DoOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function DoOnRenderBmpExternally(AFilename: string): string;
    procedure DoOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages: TStringList);
    procedure DoOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    procedure DoOnSavePrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);

    function DoOnFileExists(const AFileName: string): Boolean;
    function DoOnTClkIniReadonlyFileCreate(AFileName: string): TClkIniReadonlyFile;
    procedure DoOnSaveTemplateToFile(AStringList: TStringList; const AFileName: string);

    procedure DoOnSetOpenDialogMultiSelect;
    procedure DoOnSetOpenDialogInitialDir(AInitialDir: string);
    function DoOnOpenDialogExecute(AFilter: string): Boolean;
    function DoOnGetOpenDialogFileName: string;
    procedure DoOnSetSaveDialogInitialDir(AInitialDir: string);
    function DoOnSaveDialogExecute(AFilter: string): Boolean;
    function DoOnGetSaveDialogFileName: string;
    procedure DoOnSetSaveDialogFileName(AFileName: string);

    procedure DoOnSetPictureSetOpenDialogMultiSelect;
    procedure DoOnSetPictureOpenDialogInitialDir(AInitialDir: string);
    function DoOnPictureOpenDialogExecute: Boolean;
    function DoOnGetPictureOpenDialogFileName: string;

    function DoOnGetGridDrawingOption: TDisplayGridLineOption;
    procedure DoOnGetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);
    procedure DoOnSetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);

    procedure DoOnRetrieveRenderedBmpFromServer(ARemoteAddress, AFnm: string);

    function PlayActionByNode(Node: PVirtualNode): Boolean;
    procedure PlaySelected;
    procedure PlayAllActionsFromButton(IsDebugging: Boolean = False; StartAtSelected: Boolean = False);
    procedure PlaySelectedActionFromButton;
    procedure StopAllActionsFromButton;

    function ValidActionToBeAdded: Boolean;


    procedure CopySelectedActionsToClipboard;
    procedure PasteActionsFromClipboard;

    function EvaluateReplacements(s: string; Recursive: Boolean = True): string;
    function EvaluateHTTP(AValue: string): string;

    function GetNodeByIndex(ANodeIndex: Integer): PVirtualNode;
    procedure HighlightCurrentlyExecutedAction(Node: PVirtualNode);

    procedure ClickerActionsFrameOnControlsModified(Sender: TObject);
    procedure StopGlowingUpdateButton;
    procedure ClearAllActions;

    procedure InsertCallTemplateForActionReplacing(AIndexToInsertAt: Integer; ATemplateFileName: string);
    procedure InsertSetVar(AIndexToInsertAt: Integer; AActionName: string; var ANewAction: TClkSetVarOptions);

    procedure GetSelectedActions(var ActionsToCopy: TClkActionsRecArr); overload;
    function GetSelectedActions(var AIndexArr: TIntegerDynArray; var AActionsArr: TClkActionsRecArr): Integer; overload;

    procedure LoadActionIntoEditorByIndex(AIndex: Integer);
    procedure ReplaceSelectedActionsWithCallTemplate(var AActionIndexArrToReplace: TIntegerDynArray; AFirstSelectedIndex: Integer; ACallTemplateFileName: string);

    procedure HandleOnCopyControlTextAndClassFromMainWindow(ACompProvider: string; out AControlText, AControlClass: string);
    function HandleOnGetExtraSearchAreaDebuggingImage(AExtraBitmap: TBitmap): Boolean;

    procedure ToggleBreakpoint;
  public
    { Public declarations }
    frClickerActions: TfrClickerActions;  //used from outside

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateIniReadonlyFileFromInMemFileSystem(AFnm: string; AInMemFileSystem: TInMemFileSystem): TClkIniReadonlyFile;
    procedure LoadTemplate(Fnm: string; AFileLocation: TFileLocation = flDisk; AInMemFileSystem: TInMemFileSystem = nil);
    procedure SaveTemplateWithDialog;
    procedure SetVariables(ListOfVariables: TStrings);
    procedure SetAllActionsToNotStarted;
    procedure ResetDebuggingStatusOnAllActions;
    function ShouldStopActionAtBreakpoint(AActionBreakPoint: TActionBreakPoint): Boolean;
    function PlayAllActions(IsDebugging: Boolean = False; StartAtSelected: Boolean = False): Boolean; overload;
    function PlayAllActions(CallerName: string; ListOfVariables: TStrings; IsDebugging: Boolean; OverridenValues: TStrings = nil): Boolean; overload;
    procedure WaitInDebuggingMode;

    function GetActionVarValue(VarName: string): string;
    procedure SetActionVarValue(VarName, VarValue: string);
    procedure AppendErrorMessageToActionVar(NewErrMsg: string);
    procedure PrependErrorMessageToActionVar(NewErrMsg: string);

    procedure ExecuteActionFromClient(AIsDebuggingFromClient: Boolean); //expects RemoteExActionIndex to be set, then enables tmrExecActionFromSrvModule
    procedure ExitTemplateFromRemote;
    function SendMissingFilesToServer: string;
    function SetCurrentClientTemplateInServer(ASendFileOnly: Boolean = False): string;

    procedure AddToLog(s: string);

    procedure InitFrame;

    property Modified: Boolean read FModified write SetModified;
    property FileName: string read FFileName write FFileName;
    property StopAllActionsOnDemand: Boolean read FStopAllActionsOnDemand write FStopAllActionsOnDemand;
    property StopAllActionsOnDemandFromParent: PBoolean read FStopAllActionsOnDemandFromParent write FStopAllActionsOnDemandFromParent;
    //property Debugging: Boolean read FDebugging write FDebugging;
    property FullTemplatesDir: string read FFullTemplatesDir write SetFullTemplatesDir;  //no trailing backslash

    property ExecutesRemotely: Boolean read FExecutesRemotely write FExecutesRemotely;  //used in client mode
    property StackLevel: Integer read FStackLevel write FStackLevel;
    property RemoteExActionIndex: Integer read FRemoteExActionIndex write FRemoteExActionIndex;
    property RemoteExCmdResult: Boolean read FRemoteExCmdResult write FRemoteExCmdResult;
    property ExecutingActionFromRemote: Boolean read FExecutingActionFromRemote write FExecutingActionFromRemote; //used in server mode
    property UseLocalDebugger: Boolean read FUseLocalDebugger write FUseLocalDebugger;
    property FileLocationOfDepsIsMem: Boolean read FFileLocationOfDepsIsMem write FFileLocationOfDepsIsMem;
    property RemoteAddress: string read FRemoteAddress write FRemoteAddress;

    property InMemFS: TInMemFileSystem read GetInMemFS write SetInMemFS;
    property ActionExecution: TActionExecution read FActionExecution;
    property ShouldStopAtBreakPoint: Boolean {read FShouldStopAtBreakPoint} write FShouldStopAtBreakPoint;

    property GridDrawingOption: TDisplayGridLineOption write SetGridDrawingOption;
    property PreviewSelectionColors: TSelectionColors write SetPreviewSelectionColors;
    property PlayingAllActions: Boolean read FPlayingAllActions;

    property OnCallTemplate: TOnCallTemplate read FOnCallTemplate write FOnCallTemplate;
    property OnExecuteRemoteActionAtIndex: TOnExecuteRemoteActionAtIndex read FOnExecuteRemoteActionAtIndex write FOnExecuteRemoteActionAtIndex;
    property OnCopyControlTextAndClassFromMainWindow: TOnCopyControlTextAndClassFromMainWindow read FOnCopyControlTextAndClassFromMainWindow write FOnCopyControlTextAndClassFromMainWindow;
    property OnGetExtraSearchAreaDebuggingImageWithStackLevel: TOnGetExtraSearchAreaDebuggingImageWithStackLevel write FOnGetExtraSearchAreaDebuggingImageWithStackLevel;

    property OnWaitForFileAvailability: TOnWaitForFileAvailability read FOnWaitForFileAvailability write FOnWaitForFileAvailability;
    property OnWaitForMultipleFilesAvailability: TOnWaitForMultipleFilesAvailability read FOnWaitForMultipleFilesAvailability write FOnWaitForMultipleFilesAvailability;
    property OnWaitForBitmapsAvailability: TOnWaitForBitmapsAvailability read FOnWaitForBitmapsAvailability write FOnWaitForBitmapsAvailability;
    property OnLoadBitmap: TOnLoadBitmap read FOnLoadBitmap write FOnLoadBitmap;
    property OnLoadRenderedBitmap: TOnLoadRenderedBitmap read FOnLoadRenderedBitmap write FOnLoadRenderedBitmap;
    property OnRenderBmpExternally: TOnRenderBmpExternally read FOnRenderBmpExternally write FOnRenderBmpExternally;
    property OnGetListOfExternallyRenderedImages: TOnGetListOfExternallyRenderedImages write FOnGetListOfExternallyRenderedImages;
    property OnLoadPrimitivesFile: TOnLoadPrimitivesFile write FOnLoadPrimitivesFile;
    property OnSavePrimitivesFile: TOnSavePrimitivesFile write FOnSavePrimitivesFile;

    property OnFileExists: TOnFileExists write FOnFileExists;
    property OnTClkIniReadonlyFileCreate: TOnTClkIniReadonlyFileCreate write FOnTClkIniReadonlyFileCreate;
    property OnSaveTemplateToFile: TOnSaveTemplateToFile write FOnSaveTemplateToFile;

    property OnSetOpenDialogMultiSelect: TOnSetOpenDialogMultiSelect write FOnSetOpenDialogMultiSelect;
    property OnSetOpenDialogInitialDir: TOnSetOpenDialogInitialDir write FOnSetOpenDialogInitialDir;
    property OnOpenDialogExecute: TOnOpenDialogExecute write FOnOpenDialogExecute;
    property OnGetOpenDialogFileName: TOnGetOpenDialogFileName write FOnGetOpenDialogFileName;
    property OnSetSaveDialogInitialDir: TOnSetOpenDialogInitialDir write FOnSetSaveDialogInitialDir;
    property OnSaveDialogExecute: TOnOpenDialogExecute write FOnSaveDialogExecute;
    property OnGetSaveDialogFileName: TOnGetOpenDialogFileName write FOnGetSaveDialogFileName;
    property OnSetSaveDialogFileName: TOnSetOpenDialogFileName write FOnSetSaveDialogFileName;

    property OnSetPictureSetOpenDialogMultiSelect: TOnSetPictureSetOpenDialogMultiSelect write FOnSetPictureSetOpenDialogMultiSelect;
    property OnSetPictureOpenDialogInitialDir: TOnSetPictureOpenDialogInitialDir write FOnSetPictureOpenDialogInitialDir;
    property OnPictureOpenDialogExecute: TOnPictureOpenDialogExecute write FOnPictureOpenDialogExecute;
    property OnGetPictureOpenDialogFileName: TOnGetPictureOpenDialogFileName write FOnGetPictureOpenDialogFileName;

    property OnGetGridDrawingOption: TOnGetGridDrawingOption write FOnGetGridDrawingOption;
    property OnGetFontFinderSettings: TOnRWFontFinderSettings write FOnGetFontFinderSettings;
    property OnSetFontFinderSettings: TOnRWFontFinderSettings write FOnSetFontFinderSettings;

    property OnRetrieveRenderedBmpFromServer: TOnRetrieveRenderedBmpFromServer write FOnRetrieveRenderedBmpFromServer;
  end;


implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.frm}
{$ENDIF}


uses
  ValEdit, Math, ClickerTemplates,
  BitmapProcessing, Clipbrd, ClickerConditionEditorForm, ClickerActionsClient,
  ClickerTemplateNotesForm, AutoCompleteForm;


procedure TfrClickerActionsArr.FillInWithAllVars(AListOfVars: TStringList);
const
  CMissingVars: array[0..7] of string = (
    '$ExitCode$',
    '$frmUIClickerMainForm_Handle$',
    '$frmClickerControlPreview_Handle$',
    '$frmClickerActions_Handle$',
    '$frmClickerWinInterp_Handle$',
    '$frmClickerTemplateCallTree_Handle$',
    '$frmClickerRemoteScreen_Handle$',
    '$frmClickerZoomPreview_Handle$'
  );
var
  i: Integer;
begin
  AListOfVars.AddStrings(frClickerActions.vallstVariables.Cols[0]);
  if AListOfVars.Count > 0 then
    if AListOfVars[0] = 'Variable' then
      AListOfVars.Delete(0);

  if AListOfVars.IndexOf('$AppDir$') = -1 then
    AListOfVars.Insert(0, '$AppDir$');

  for i := 0 to Length(CMissingVars) - 1 do
    if AListOfVars.IndexOf(CMissingVars[i]) = -1 then
      AListOfVars.Add(CMissingVars[i]);
end;


procedure TfrClickerActionsArr.FillInWithAllFuncs(AListOfVars: TStringList);
begin
  AListOfVars.AddStrings(frClickerActions.frClickerSetVar.memAvailableFunctions.Lines);
end;


procedure TfrClickerActionsArr.FillInVarAndFuncDescriptions;
var
  i: Integer;
  TempVarDescriptions: TStringList;
  TempFuncDescriptions: TStringList;
  s: string;
begin
  FVarDescriptions.Clear;
  FFuncDescriptions.Clear;

  TempVarDescriptions := TStringList.Create;
  try
    FillInWithAllVars(FVarDescriptions);

    TempVarDescriptions.Add('$Control_Text$=[String] Current control text. Not all controls have text. Subcontrols do not have a text. This is updated by FindControl action, if the control is found.');
    TempVarDescriptions.Add('$Control_Class$=[String] Current control class. All controls should have a class. Subcontrols do not have a class. This is updated by FindControl action, if the control is found.');
    TempVarDescriptions.Add('$Control_Handle$=[Numeric] Current control handle. All controls have a handle. Subcontrols do not have a handle. This is updated by FindControl action, if the control is found.');
    TempVarDescriptions.Add('$Control_Left$=[Numeric] Current control left edge. This is updated by Find(Sub)Control action, if the control is found.');
    TempVarDescriptions.Add('$Control_Top$=[Numeric] Current control top edge. This is updated by Find(Sub)Control action, if the control is found.');
    TempVarDescriptions.Add('$Control_Right$=[Numeric] Current control right edge. This is updated by Find(Sub)Control action, if the control is found.');
    TempVarDescriptions.Add('$Control_Bottom$=[Numeric] Current control bottom edge. This is updated by Find(Sub)Control action, if the control is found.');
    TempVarDescriptions.Add('$Control_Width$=[Numeric] Current control width. This is updated by Find(Sub)Control action, if the control is found.');
    TempVarDescriptions.Add('$Control_Height$=[Numeric] Current control left edge. This is updated by Find(Sub)Control action, if the control is found.');
    TempVarDescriptions.Add('$Half_Control_Width$=[Numeric] Current control half width. This is updated by Find(Sub)Control action, if the control is found.');
    TempVarDescriptions.Add('$Half_Control_Height$=[Numeric] Current control half height. This is updated by Find(Sub)Control action, if the control is found.');
    TempVarDescriptions.Add('$ExecAction_Err$=[String] Error message, as a result of action execution.');
    TempVarDescriptions.Add('$LastAction_Status$=[String] Action execution status.');
    TempVarDescriptions.Add('$LastAction_Skipped$=[String] Indicator of skipping an action execution, based on action condition.');
    TempVarDescriptions.Add('$Screen_Width$=[Numeric] Current screen width. This value is read once, on application startup.');
    TempVarDescriptions.Add('$Screen_Height$=[Numeric] Current screen height. This value is read once, on application startup.');
    TempVarDescriptions.Add('$Desktop_Width$=[Numeric] Current desktop width. This value is read once, on application startup.');
    TempVarDescriptions.Add('$Desktop_Height$=[Numeric] Current desktop height. This value is read once, on application startup.');
    TempVarDescriptions.Add('$Color_Highlight$=[Numeric] Standard system color (value updated by OS). It is used on e.g. focused and selected listbox items.');
    TempVarDescriptions.Add('$Color_BtnFace$=[Numeric] Standard system color (value updated by OS). It is used mostly as background color on windows and buttons.');
    TempVarDescriptions.Add('$Color_Window$=[Numeric] Standard system color (value updated by OS). It is used as background color on e.g. lists and editboxes.');
    TempVarDescriptions.Add('$Color_WindowText$=[Numeric] Standard system color (value updated by OS). It is used as text color on unselected, focused system components.');
    TempVarDescriptions.Add('$ExitCode$=[Numeric] The argument passed to the $Exit(<ExitCode>)$ function.');
    TempVarDescriptions.Add('$AppDir$=[String] This variable can be used in some file paths. It points to the directory where this UIClicker executable is. For safety reasons, the internal use of $AppDir$ is hardcoded, so that it cannot be overriden from the list of variables.');
    TempVarDescriptions.Add('$TemplateDir$=[String] This variable can be used in some file paths (.bmp and .pmtv files, on executing FindSubControl action). It points to the directory, configured on Actions window (settings page). For safety reasons, the internal use of $TemplateDir$ is hardcoded, so that it cannot be overriden from the list of variables.');
    TempVarDescriptions.Add('$SelfTemplateDir$=[String] This variable can be used in some file paths (.bmp and .pmtv files, on executing FindSubControl action). It points to the directory, where the current template is (during FindSubControl execution only). For safety reasons, the internal use of $SelfTemplateDir$ is hardcoded, so that it cannot be overriden from the list of variables.');
    TempVarDescriptions.Add('$frmUIClickerMainForm_Handle$=[Numeric] The current handle of (this) UIClicker''s main form. This variable is generated by a call to $GetSelfHandles()$.');
    TempVarDescriptions.Add('$frmClickerControlPreview_Handle$=[Numeric] The current handle of (this) UIClicker''s Preview form. This variable is generated by a call to $GetSelfHandles()$.');
    TempVarDescriptions.Add('$frmClickerActions_Handle$=[Numeric] The current handle of (this) UIClicker''s Actions form. This variable is generated by a call to $GetSelfHandles()$.');
    TempVarDescriptions.Add('$frmClickerWinInterp_Handle$=[Numeric] The current handle of (this) UIClicker''s WinInterp form. This variable is generated by a call to $GetSelfHandles()$.');
    TempVarDescriptions.Add('$frmClickerTemplateCallTree_Handle$=[Numeric] The current handle of (this) UIClicker''s TemplateCallTree form. This variable is generated by a call to $GetSelfHandles()$.');
    TempVarDescriptions.Add('$frmClickerRemoteScreen_Handle$=[Numeric] The current handle of (this) UIClicker''s RemoteScreen form. This variable is generated by a call to $GetSelfHandles()$.');
    TempVarDescriptions.Add('$frmClickerZoomPreview_Handle$=[Numeric] The current handle of (this) UIClicker''s ZoomPreview form. This variable is generated by a call to $GetSelfHandles()$.');
    TempVarDescriptions.Add('$DebugVar_SubCnvXOffset$=[Numeric] This is a debugging variable. It is updated by FindSubControl action, with the left edge of the found subcontrol, relative to its parent (sub)control.');
    TempVarDescriptions.Add('$DebugVar_SubCnvYOffset$=[Numeric] This is a debugging variable. It is updated by FindSubControl action, with the top edge of the found subcontrol, relative to its parent (sub)control.');
    TempVarDescriptions.Add('$DebugVar_TextColors$=[String] This is a debugging variable. It is updated by FindSubControl action, with the path to the executing template and the evaluated foreground and background color of the searched bitmap text, from the current font profile. After execution, the variable contains the colors from the last font profile.');
    TempVarDescriptions.Add('$DebugVar_BitmapText$=[String] This is a debugging variable. It is updated by FindSubControl action, with the searched bitmap text. If the text is empty, the variable contains an error message, which helps the user understand why the action fails.');
    TempVarDescriptions.Add('$DbgCurrentAction$=[String] This is a debugging variable. It is updated in server mode with an error message, when attempting to execute an out of index action, or an action from an empty template.');
    TempVarDescriptions.Add('$DbgPlayAllActions$=[String] This is a debugging variable. It is updated in server mode with debugging information about action execution state. It is useful during remote debugging, when the server waits for client to close a template.');

    for i := 0 to FVarDescriptions.Count - 1 do
      FVarDescriptions.Strings[i] := TempVarDescriptions.Values[FVarDescriptions.Strings[i]];
  finally
    TempVarDescriptions.Free;
  end;

  TempFuncDescriptions := TStringList.Create;
  try
    FillInWithAllFuncs(FFuncDescriptions);

    TempFuncDescriptions.Add('$ExtractFileDir(<DirName>)$=Returns the directory up to and not including the last item in the path.');
    TempFuncDescriptions.Add('$Random(<min>, <max>)$=Returns a random number between min and max.');
    TempFuncDescriptions.Add('$Random(<max>)$=Returns a random number, lower than max.');
    TempFuncDescriptions.Add('$Sum(<op1>, <op2>)$=Adds two numbers.');
    TempFuncDescriptions.Add('$Diff(<op1>, <op2>)$=Subtracts two numbers.');
    TempFuncDescriptions.Add('$Mul(<op1>, <op2>)$=Multiplies two integer numbers. If only one operand is passed, the result is Sqr(<op>).');
    TempFuncDescriptions.Add('$Div(<op1>, <op2>)$=Divides two integer numbers. If only one operand is passed, the result is Round(Sqrt(<op>)).');
    TempFuncDescriptions.Add('$FMul(<op1>, <op2>)$=Multiplies two double-precision numbers. If only one operand is passed, the result is Sqr(<op>).');
    TempFuncDescriptions.Add('$FDiv(<op1>, <op2>)$=Divides two double-precision numbers. If only one operand is passed, the result is Sqrt(<op>).');
    TempFuncDescriptions.Add('$EFMul(<op1>, <op2>)$=Multiplies two extended-precision numbers. If only one operand is passed, the result is Sqr(<op>).');
    TempFuncDescriptions.Add('$EFDiv(<op1>, <op2>)$=Divides two extended-precision numbers. If only one operand is passed, the result is Sqrt(<op>).');
    TempFuncDescriptions.Add('$Abs(<op>)$=Returns the absolute value of an integer number.');
    TempFuncDescriptions.Add('$FAbs(<op>)$=Returns the absolute value of a double-precision number.');
    TempFuncDescriptions.Add('$EFAbs(<op>)$=Returns the absolute value of an extended-precision number.');
    TempFuncDescriptions.Add('$PrefixWithZeros(<Number>, <TotalNumberOfDigits>)$=Prefixes a number, or a variable which evaluates to a number, with one or more zeros, until the total string length is at least TotalNumberOfDigits characters long. It is useful on action conditions, where the two operands are compared as strings.');
    TempFuncDescriptions.Add('$http://<server:port>/[params]$=Makes an http GET request to the specified server address and port. Returns the result.');
    TempFuncDescriptions.Add('$FastReplace_45ToReturn(<some_string>)$=Replaces all #4#5 (ASCII_4 and ASCII_5) occurrences with CRLF. Returns the result.');
    TempFuncDescriptions.Add('$FastReplace_ReturnTo45(<some_string>)$=Replaces all CRLF (ASCII_13 and ASCII_10) occurrences with #4#5. Returns the result.');
    TempFuncDescriptions.Add('$FastReplace_45To87(<some_string>)$=Replaces all #4#5 (ASCII_4 and ASCII_5) occurrences with #8#7 (ASCII_8 and ASCII_7). Returns the result.');
    TempFuncDescriptions.Add('$FastReplace_87To45(<some_string>)$=Replaces all #8#7 (ASCII_8 and ASCII_7) occurrences with #4#5 (ASCII_4 and ASCII_5). Returns the result.');
    TempFuncDescriptions.Add('$Exit(<ExitCode>)$=Stops current template execution. If the passed ExitCode argument is 0, the template stops with a "Successful" status, otherwise with "Failed". Returns nothing. Sets the $ExitCode$ variable to the passed argument. It can be executed by SetVar action and must be placed in its "Variable" column.');
    TempFuncDescriptions.Add('$CreateDir(<PathToNewDir>)$=Creates a directory (and its parent directories, if required) by the current path.');
    TempFuncDescriptions.Add('$LoadTextFile(<PathToTextFile>)$=Loads a text file (CRLF-separated lines) and returns its content as #4#5 separated strings.');
    TempFuncDescriptions.Add('$ItemCount($TextFileContent$)$=Returns the number of items in a #4#5-separated list of strings (as returned by $LoadTextFile()$. Even the last item has to be terminated by #4#5.');
    TempFuncDescriptions.Add('$GetTextItem($TextFileContent$,<ItemIndex>)$=Returns the item by index, in a #4#5-separated list of strings (as returned by $LoadTextFile()$. Even the last item has to be terminated by #4#5.');
    TempFuncDescriptions.Add('$StringContains(<SubString>, <String>)$=Return 1, if <SubString> is part of <String>. Otherwise, it returns 0.');
    TempFuncDescriptions.Add('$ExtractFileName(<PathToFile>)$=Returns the last item of a file path.');
    TempFuncDescriptions.Add('$ExtractFileExt(<PathToFile>)$=Returns the extension (including the dot) of a file name.');
    TempFuncDescriptions.Add('$ExtractFileNameNoExt(<PathToFile>)$=Returns the filename, without extension, from a file path');
    TempFuncDescriptions.Add('$UpdateControlInfo(<Handle>)$=Updates several variables (e.g. $Control_Text$, $Control_Class$, $Control_Left$, $Control_Top$ etc.) with details about the control, which matches the handle. Returns the passed handle. If the handle is invalid/unused, the $Control_Class$ variable is empty.');
    TempFuncDescriptions.Add('$GetSelfHandles()$=Updates several variables with their window handles (see $frmUIClickerMainForm_Handle$). Returns nothing. It can be executed by SetVar action and must be placed in its "Variable" column.');
    TempFuncDescriptions.Add('$GetKeyNameFromPair(<key>eq<value>)$=Returns <key> from a <key>eq<value> statement.');
    TempFuncDescriptions.Add('$GetKeyValueFromPair(<key>eq<value>)$=Returns <value> from a <key>eq<value> statement.');
    TempFuncDescriptions.Add('$Chr(<ByteValue>)$=Returns the ASCII character, based on the given index. Not all characters have a visual representation.');
    TempFuncDescriptions.Add('$IncBrightness(<HexColor>[,Amount])$=Returns <HexColor>, increased by Amount, on all RGB channels. If Amount is not passed, <HexColor> is increased by one, on all RGB channels.');
    TempFuncDescriptions.Add('$DecBrightness(<HexColor>[,Amount])$=Returns <HexColor>, decreased by Amount, on all RGB channels. If Amount is not passed, <HexColor> is decreased by one, on all RGB channels.');
    TempFuncDescriptions.Add('$IncBrightnessR(<HexColor>[,Amount])$=Returns <HexColor>, increased by Amount, on R channel only. If Amount is not passed, <HexColor> is increased by one, on R channel only. <HexColor> must be in BBGGRR format.');
    TempFuncDescriptions.Add('$IncBrightnessG(<HexColor>[,Amount])$=Returns <HexColor>, increased by Amount, on G channel only. If Amount is not passed, <HexColor> is increased by one, on G channel only. <HexColor> must be in BBGGRR format.');
    TempFuncDescriptions.Add('$IncBrightnessB(<HexColor>[,Amount])$=Returns <HexColor>, increased by Amount, on B channel only. If Amount is not passed, <HexColor> is increased by one, on B channel only. <HexColor> must be in BBGGRR format.');
    TempFuncDescriptions.Add('$DecBrightnessR(<HexColor>[,Amount])$=Returns <HexColor>, decreased by Amount, on R channel only. If Amount is not passed, <HexColor> is decreased by one, on R channel only. <HexColor> must be in BBGGRR format.');
    TempFuncDescriptions.Add('$DecBrightnessG(<HexColor>[,Amount])$=Returns <HexColor>, decreased by Amount, on G channel only. If Amount is not passed, <HexColor> is decreased by one, on G channel only. <HexColor> must be in BBGGRR format.');
    TempFuncDescriptions.Add('$DecBrightnessB(<HexColor>[,Amount])$=Returns <HexColor>, decreased by Amount, on B channel only. If Amount is not passed, <HexColor> is decreased by one, on B channel only. <HexColor> must be in BBGGRR format.');
    TempFuncDescriptions.Add('$Current_Mouse_X$=Returns the current global X position of the mouse cursor.');
    TempFuncDescriptions.Add('$Current_Mouse_Y$=Returns the current global Y position of the mouse cursor.');
    TempFuncDescriptions.Add('$CRLF$=Returns a CRLF sequence.');
    TempFuncDescriptions.Add('$#4#5$=Returns a ASCII #4#5 sequence.');
    TempFuncDescriptions.Add('$Now$=Returns current datetime.');
    TempFuncDescriptions.Add('$RenderBmpExternally()$=Sends an http request to a server, for rendering a bitmap, using the supplied list of parameters (from right column of a SetVar action). These parameters are encoded as a #4#5 separated <key>eq<value> strings. The required parameters are (without quotes): "' + CExtBmp_SrvAddrPort + '", "' + CExtBmp_Cmd + '", "' + CExtBmp_Filename + '". The optional parameters are (without quotes): "' + CExtBmp_Params + '" and "' + CExtBmp_IncludeFilenameInRequest + '". When provided, "' + CExtBmp_Params + '" are "&"-separated key%3Dvalue pairs. When "' + CExtBmp_IncludeFilenameInRequest + '" is 1, the filename is added to request. The result is placed in $ExternallyRenderedBmpResult$ variable. If successful, the result is set to empty string, otherwise it is set to an error message. The received bitmap is "stored" in an in-mem file system. The function must be called from the left column of SetVar action.');
    TempFuncDescriptions.Add('$GetActionProperties()$=Sets the $ActionPropertiesResult$ variable to an &-separated list of action properties and their values, from the current template. The action is identified by name, and this name has to be provided in the right column of a SetVar action. If the action is not found by name (which is case sensitive), the result is set to an error message. The function must be called from the left column.');

    for i := 0 to FFuncDescriptions.Count - 1 do
    begin
      s := FFuncDescriptions.Strings[i];
      s := StringReplace(s, '<key>=<value>', '<key>eq<value>', [rfReplaceAll]);
      FFuncDescriptions.Strings[i] := StringReplace(TempFuncDescriptions.Values[s], '<key>eq<value>', '<key>=<value>', [rfReplaceAll]);
    end;
  finally
    TempFuncDescriptions.Free;
  end;
end;


procedure TfrClickerActionsArr.CreateRemainingUIComponents;
var
  NewColum: TVirtualTreeColumn;
begin
  frClickerActions := TfrClickerActions.Create(Self);
  frClickerActions.Parent := pnlActionsEditor;
  frClickerActions.Left := 3;
  frClickerActions.Top := pnlfrClickerActions.Top;
  frClickerActions.Width := 686 + 80;
  frClickerActions.Height := 259;
  frClickerActions.Constraints.MinWidth := frClickerActions.Width;
  frClickerActions.Constraints.MinHeight := frClickerActions.Height;
  frClickerActions.Anchors := [akBottom, akLeft, akRight, akTop];
  frClickerActions.Color := clCream;
  frClickerActions.ParentColor := False;
  frClickerActions.TabOrder := 2;
  frClickerActions.TabStop := True;

  frClickerActions.OnCopyControlTextAndClassFromMainWindow := HandleOnCopyControlTextAndClassFromMainWindow;
  frClickerActions.OnGetExtraSearchAreaDebuggingImage := HandleOnGetExtraSearchAreaDebuggingImage;

  frClickerActions.OnEditCallTemplateBreakCondition := HandleOnEditCallTemplateBreakCondition;
  frClickerActions.OnLoadBitmap := HandleOnLoadBitmap; //both ActionExecution and frClickerActions use the same handler
  frClickerActions.OnLoadRenderedBitmap := HandleOnLoadRenderedBitmap;
  frClickerActions.OnGetListOfExternallyRenderedImages := HandleOnGetListOfExternallyRenderedImages;
  frClickerActions.OnLoadPrimitivesFile := HandleOnLoadPrimitivesFile;
  frClickerActions.OnSavePrimitivesFile := HandleOnSavePrimitivesFile;
  frClickerActions.OnFileExists := HandleOnFileExists;

  frClickerActions.OnSetOpenDialogMultiSelect := HandleOnSetOpenDialogMultiSelect;
  frClickerActions.OnSetOpenDialogInitialDir := HandleOnSetOpenDialogInitialDir;
  frClickerActions.OnOpenDialogExecute := HandleOnOpenDialogExecute;
  frClickerActions.OnGetOpenDialogFileName := HandleOnGetOpenDialogFileName;
  frClickerActions.OnSetSaveDialogInitialDir := HandleOnSetSaveDialogInitialDir;
  frClickerActions.OnSaveDialogExecute := HandleOnSaveDialogExecute;
  frClickerActions.OnGetSaveDialogFileName := HandleOnGetSaveDialogFileName;
  frClickerActions.OnSetSaveDialogFileName := HandleOnSetSaveDialogFileName;

  frClickerActions.OnSetPictureSetOpenDialogMultiSelect := HandleOnSetPictureSetOpenDialogMultiSelect;
  frClickerActions.OnSetPictureOpenDialogInitialDir := HandleOnSetPictureOpenDialogInitialDir;
  frClickerActions.OnPictureOpenDialogExecute := HandleOnPictureOpenDialogExecute;
  frClickerActions.OnGetPictureOpenDialogFileName := HandleOnGetPictureOpenDialogFileName;

  frClickerActions.OnExecuteFindSubControlAction := HandleOnExecuteFindSubControlAction;
  frClickerActions.OnAddToLog := HandleOnAddToLog;
  frClickerActions.OnGetFontFinderSettings := HandleOnGetFontFinderSettings;
  frClickerActions.OnSetFontFinderSettings := HandleOnSetFontFinderSettings;

  //frClickerActions.OnControlsModified := ClickerActionsFrameOnControlsModified;   //this is set on frame initialization

  vstActions := TVirtualStringTree.Create(Self);
  vstActions.Parent := pnlActions;

  vstActions.Left := 3;
  vstActions.Top := 3;
  vstActions.Width := pnlvstActions.Width;
  vstActions.Height := pnlvstActions.Height;
  vstActions.CheckImageKind := ckXP;
  vstActions.DefaultNodeHeight := 26;
  vstActions.Anchors := [akLeft, akRight, akTop, akBottom];
  vstActions.Header.AutoSizeIndex := 0;
  vstActions.Header.DefaultHeight := 21;
  vstActions.Header.Font.Charset := DEFAULT_CHARSET;
  vstActions.Header.Font.Color := clWindowText;
  vstActions.Header.Font.Height := -11;
  vstActions.Header.Font.Name := 'Tahoma';
  vstActions.Header.Font.Style := [];
  vstActions.Header.Height := 21;
  vstActions.Header.Options := [hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible];
  vstActions.Header.Style := hsFlatButtons;
  vstActions.Indent := 2;
  //vstActions.PopupMenu := pmVstActions; //the menu will be poped up by code
  vstActions.StateImages := frClickerActions.imglstActions;
  vstActions.TabOrder := 0;
  vstActions.TreeOptions.MiscOptions := [toAcceptOLEDrop, toEditable, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning];
  vstActions.TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages];
  vstActions.TreeOptions.SelectionOptions := [toFullRowSelect, toMiddleClickSelect, {toRightClickSelect,} toMultiSelect];
  vstActions.OnBeforeCellPaint := vstActionsBeforeCellPaint;
  vstActions.OnChecked := vstActionsChecked;
  vstActions.OnDblClick := vstActionsDblClick;
  vstActions.OnEditCancelled := vstActionsEditCancelled;
  vstActions.OnEdited := vstActionsEdited;
  vstActions.OnEditing := vstActionsEditing;
  vstActions.OnGetText := vstActionsGetText;
  vstActions.OnNewText := vstActionsNewText;
  vstActions.OnPaintText := vstActionsPaintText;
  vstActions.OnGetImageIndex := vstActionsGetImageIndex;
  vstActions.OnGetImageIndexEx := vstActionsGetImageIndexEx;
  vstActions.OnInitNode := vstActionsInitNode;
  vstActions.OnMouseDown := vstActionsMouseDown;
  vstActions.OnMouseUp := vstActionsMouseUp;
  vstActions.OnKeyDown := vstActionsKeyDown;
  vstActions.OnKeyUp := vstActionsKeyUp;
  vstActions.Colors.UnfocusedSelectionColor := clGradientInactiveCaption;

  NewColum := vstActions.Header.Columns.Add;
  NewColum.MinWidth := 180;
  NewColum.Position := 1;
  NewColum.Width := 180;
  NewColum.Text := 'Action Name';

  NewColum := vstActions.Header.Columns.Add;
  NewColum.MinWidth := 100;
  NewColum.Position := 2;
  NewColum.Width := 100;
  NewColum.Text := 'Action';

  NewColum := vstActions.Header.Columns.Add;
  NewColum.MinWidth := 80;
  NewColum.Position := 3;
  NewColum.Width := 80;
  NewColum.Text := 'Timeout [ms]';

  NewColum := vstActions.Header.Columns.Add;
  NewColum.MinWidth := 110;
  NewColum.Position := 4;
  NewColum.Width := 110;
  NewColum.Text := 'Condition / Misc';

  NewColum := vstActions.Header.Columns.Add;
  NewColum.MinWidth := 200;
  NewColum.Position := 5;
  NewColum.Width := 200;
  NewColum.Text := 'Text';

  NewColum := vstActions.Header.Columns.Add;
  NewColum.MinWidth := 500;
  NewColum.Position := 6;
  NewColum.Width := 500;
  NewColum.Text := 'Bitmap Files';

  NewColum := vstActions.Header.Columns.Add;
  NewColum.Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coAllowFocus];
  NewColum.MinWidth := 30;
  NewColum.Position := 0;
  NewColum.Width := 30;
  NewColum.Text := '#';

  FVarDescriptions := TStringList.Create;
  FFuncDescriptions := TStringList.Create;
  //FillInVarDescriptions; called when displaying autocomplete
end;


constructor TfrClickerActionsArr.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLoggingFIFO := TPollingFIFO.Create;
  CreateRemainingUIComponents;

  FActionExecution := TActionExecution.Create;
  FActionExecution.ClickerVars := frClickerActions.vallstVariables.Strings;
  FActionExecution.StopAllActionsOnDemand := @FStopAllActionsOnDemand;
  FActionExecution.StopAllActionsOnDemandFromParent := FStopAllActionsOnDemandFromParent;
  FActionExecution.OnAddToLog := AddToLog;
  FActionExecution.SelfTemplateFileName := @FFileName;
  FActionExecution.ExecutingActionFromRemote := @FExecutingActionFromRemote;
  FActionExecution.FileLocationOfDepsIsMem := @FFileLocationOfDepsIsMem;
  FActionExecution.FullTemplatesDir := @FFullTemplatesDir;
  FActionExecution.StackLevel := @FStackLevel;
  FActionExecution.ExecutesRemotely := @FExecutesRemotely;
  FActionExecution.OwnerFrame := Self;
  FActionExecution.frClickerActions := frClickerActions;
  FActionExecution.OnSetEditorEnabledState := HandleOnSetEditorEnabledState;
  FActionExecution.OnSetEditorTimeoutProgressBarMax := HandleOnSetEditorTimeoutProgressBarMax;
  FActionExecution.OnSetEditorTimeoutProgressBarPosition := HandleOnSetEditorTimeoutProgressBarPosition;
  FActionExecution.OnWaitForBitmapsAvailability := HandleOnWaitForBitmapsAvailability;
  FActionExecution.OnLoadBitmap := HandleOnLoadBitmap; //both ActionExecution and frClickerActions use the same handler
  FActionExecution.OnLoadRenderedBitmap := HandleOnLoadRenderedBitmap;
  FActionExecution.OnRenderBmpExternally := HandleOnRenderBmpExternally;
  FActionExecution.OnGetActionProperties := HandleOnGetActionProperties;
  FActionExecution.OnCallTemplate := HandleOnCallTemplate;
  FActionExecution.OnSetEditorSleepProgressBarMax := HandleOnSetEditorSleepProgressBarMax;
  FActionExecution.OnSetEditorSleepProgressBarPosition := HandleOnSetEditorSleepProgressBarPosition;
  FActionExecution.OnSetEditorSleepInfo := HandleOnSetEditorSleepInfo;
  FActionExecution.OnAddDefaultFontProfile := HandleOnAddDefaultFontProfile;
  FActionExecution.OnGetGridDrawingOption := HandleOnGetGridDrawingOption;
  FActionExecution.OnLoadPrimitivesFile := HandleOnLoadPrimitivesFile;

  FCmdConsoleHistory := TStringList.Create;
  FOnExecuteRemoteActionAtIndex := nil;
  FOnCopyControlTextAndClassFromMainWindow := nil;
  FOnGetExtraSearchAreaDebuggingImageWithStackLevel := nil;
  FOnLoadBitmap := nil;
  FOnLoadRenderedBitmap := nil;
  FOnRenderBmpExternally := nil;
  FOnGetListOfExternallyRenderedImages := nil;
  FOnLoadPrimitivesFile := nil;
  FOnSavePrimitivesFile := nil;

  FOnFileExists := nil;
  FOnTClkIniReadonlyFileCreate := nil;
  FOnSaveTemplateToFile := nil;

  FOnSetOpenDialogMultiSelect := nil;
  FOnSetOpenDialogInitialDir := nil;
  FOnOpenDialogExecute := nil;
  FOnGetOpenDialogFileName := nil;
  FOnSetSaveDialogInitialDir := nil;
  FOnSaveDialogExecute := nil;
  FOnGetSaveDialogFileName := nil;
  FOnSetSaveDialogFileName := nil;

  FOnSetPictureSetOpenDialogMultiSelect := nil;
  FOnSetPictureOpenDialogInitialDir := nil;
  FOnPictureOpenDialogExecute := nil;
  FOnGetPictureOpenDialogFileName := nil;

  FOnGetGridDrawingOption := nil;
  FOnGetFontFinderSettings := nil;
  FOnSetFontFinderSettings := nil;

  FOnRetrieveRenderedBmpFromServer := nil;

  FPalette := nil;

  FExecutesRemotely := False;
  FRemoteExActionIndex := -1;
  FRemoteExCmdResult := False;
  FExecutingActionFromRemote := False;
  FUseLocalDebugger := False;
  FFileLocationOfDepsIsMem := False;
  FClosingTemplate := False;
  FRemoteAddress := 'http://127.0.0.1:5444/';
  FShouldStopAtBreakPoint := False;
  FPlayingAllActions := False;

  FEditingText := '';
end;


destructor TfrClickerActionsArr.Destroy;
begin
  FCmdConsoleHistory.Free;
  FActionExecution.Free;
  FLoggingFIFO.Free;
  FVarDescriptions.Free;
  FFuncDescriptions.Free;

  inherited Destroy;
end;


procedure TfrClickerActionsArr.HandleOnSetEditorEnabledState(AEnabled: Boolean);
begin
  frClickerActions.Enabled := AEnabled;
end;


procedure TfrClickerActionsArr.HandleOnSetEditorTimeoutProgressBarMax(AMaxValue: Integer);
begin
  frClickerActions.prbTimeout.Max := AMaxValue;
end;


procedure TfrClickerActionsArr.HandleOnSetEditorTimeoutProgressBarPosition(APositionValue: Integer);
begin
  frClickerActions.prbTimeout.Position := APositionValue;
end;


function TfrClickerActionsArr.HandleOnEditCallTemplateBreakCondition(var AActionCondition: string): Boolean;
begin
  Result := EditActionCondition(AActionCondition);
end;


function TfrClickerActionsArr.HandleOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  Result := DoOnLoadBitmap(ABitmap, AFileName);
end;


function TfrClickerActionsArr.HandleOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  Result := DoOnLoadRenderedBitmap(ABitmap, AFileName);
end;


function TfrClickerActionsArr.HandleOnRenderBmpExternally(AFilename: string): string;
begin
  Result := DoOnRenderBmpExternally(AFilename);
end;


function TfrClickerActionsArr.HandleOnGetActionProperties(AActionName: string): PClkActionRec;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Length(FClkActions) - 1 do
    if FClkActions[i].ActionOptions.ActionName = AActionName then
    begin
      Result := @FClkActions[i];
      Break;
    end;
end;


procedure TfrClickerActionsArr.HandleOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages: TStringList);
begin
  DoOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages);
end;


procedure TfrClickerActionsArr.HandleOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
begin
  DoOnLoadPrimitivesFile(AFileName, APrimitives, AOrders, ASettings);
end;


procedure TfrClickerActionsArr.HandleOnSavePrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
begin
  DoOnSavePrimitivesFile(AFileName, APrimitives, AOrders, ASettings);
end;


function TfrClickerActionsArr.HandleOnFileExists(const AFileName: string): Boolean;
begin
  Result := DoOnFileExists(AFileName);
end;


procedure TfrClickerActionsArr.HandleOnWaitForBitmapsAvailability(ListOfBitmapFiles: TStringList);
begin
  DoWaitForBitmapsAvailability(ListOfBitmapFiles);
end;


function TfrClickerActionsArr.HandleOnCallTemplate(Sender: TObject; AFileNameToCall: string; ListOfVariables: TStrings; DebugBitmap: TBitmap; DebugGridImage: TImage; IsDebugging, AShouldStopAtBreakPoint: Boolean; AStackLevel: Integer; AExecutesRemotely: Boolean): Boolean;
begin
  if not Assigned(OnCallTemplate) then
  begin
    AddToLog('OnCallTemplate not assigned. Can''t call templates. Is this an experimental tab?');
    raise Exception.Create('OnCallTemplate not assigned. Stopping execution...');
  end;

  Result := OnCallTemplate(Sender, AFileNameToCall, ListOfVariables, DebugBitmap, DebugGridImage, IsDebugging, AShouldStopAtBreakPoint, AStackLevel, AExecutesRemotely);
end;


procedure TfrClickerActionsArr.HandleOnSetEditorSleepProgressBarMax(AMaxValue: Integer);
begin
  frClickerActions.frClickerSleep.ProgressBarMax := AMaxValue;
end;


procedure TfrClickerActionsArr.HandleOnSetEditorSleepProgressBarPosition(APositionValue: Integer);
begin
  frClickerActions.frClickerSleep.ProgressBarPosition := APositionValue;
end;


procedure TfrClickerActionsArr.HandleOnSetEditorSleepInfo(AElapsedTime, ARemainingTime: string);
begin
  frClickerActions.frClickerSleep.SetEditorSleepInfo(AElapsedTime, ARemainingTime);
end;


procedure TfrClickerActionsArr.HandleOnAddDefaultFontProfile(var AFindControlOptions: TClkFindControlOptions; var AActionOptions: TClkActionOptions);
var
  ExecIdx: Integer;
begin
  //FindSubControl requires at least one font profile.
  //If the action is part of a list of actions, then the UI can be updated with a new profile.

  SetLength(AFindControlOptions.MatchBitmapText, 1);
  AFindControlOptions.MatchBitmapText[0].ForegroundColor := '$Color_Window$';
  AFindControlOptions.MatchBitmapText[0].BackgroundColor := '$Color_Highlight$';
  AFindControlOptions.MatchBitmapText[0].FontName := 'Tahoma';
  AFindControlOptions.MatchBitmapText[0].FontSize := 8;
  AFindControlOptions.MatchBitmapText[0].FontQualityReplacement := '';
  AFindControlOptions.MatchBitmapText[0].FontQuality := fqNonAntialiased;
  AFindControlOptions.MatchBitmapText[0].FontQualityUsesReplacement := False;
  AFindControlOptions.MatchBitmapText[0].Bold := False;
  AFindControlOptions.MatchBitmapText[0].Italic := False;
  AFindControlOptions.MatchBitmapText[0].Underline := False;
  AFindControlOptions.MatchBitmapText[0].StrikeOut := False;
  AFindControlOptions.MatchBitmapText[0].CropLeft := '0';
  AFindControlOptions.MatchBitmapText[0].CropTop := '0';
  AFindControlOptions.MatchBitmapText[0].CropRight := '0';
  AFindControlOptions.MatchBitmapText[0].CropBottom := '0';
  AFindControlOptions.MatchBitmapText[0].ProfileName := CDefaultFontProfileName;

  ExecIdx := StrToIntDef(AActionOptions.ExecutionIndex, 1);
  if (ExecIdx > -1) and (ExecIdx < Length(FClkActions))  then
  begin
    frClickerActions.ClearControls;
    UpdateControlsFromActionsArr(ExecIdx);
    frClickerActions.UpdatePageControlActionExecutionIcons;
  end;
end;


function TfrClickerActionsArr.HandleOnGetGridDrawingOption: TDisplayGridLineOption;
begin
  Result := DoOnGetGridDrawingOption;
end;


procedure TfrClickerActionsArr.HandleOnSetOpenDialogMultiSelect;
begin
  DoOnSetOpenDialogMultiSelect;
end;


procedure TfrClickerActionsArr.HandleOnSetOpenDialogInitialDir(AInitialDir: string);
begin
  DoOnSetOpenDialogInitialDir(AInitialDir);
end;


function TfrClickerActionsArr.HandleOnOpenDialogExecute(AFilter: string): Boolean;
begin
  Result := DoOnOpenDialogExecute(AFilter);
end;


function TfrClickerActionsArr.HandleOnGetOpenDialogFileName: string;
begin
  Result := DoOnGetOpenDialogFileName;
end;


procedure TfrClickerActionsArr.HandleOnSetSaveDialogInitialDir(AInitialDir: string);
begin
  DoOnSetSaveDialogInitialDir(AInitialDir);
end;


function TfrClickerActionsArr.HandleOnSaveDialogExecute(AFilter: string): Boolean;
begin
  Result := DoOnSaveDialogExecute(AFilter);
end;


function TfrClickerActionsArr.HandleOnGetSaveDialogFileName: string;
begin
  Result := DoOnGetSaveDialogFileName;
end;


procedure TfrClickerActionsArr.HandleOnSetSaveDialogFileName(AFileName: string);
begin
  DoOnSetSaveDialogFileName(AFileName);
end;


procedure TfrClickerActionsArr.HandleOnSetPictureSetOpenDialogMultiSelect;
begin
  DoOnSetPictureSetOpenDialogMultiSelect;
end;


procedure TfrClickerActionsArr.HandleOnSetPictureOpenDialogInitialDir(AInitialDir: string);
begin
  DoOnSetPictureOpenDialogInitialDir(AInitialDir);
end;


function TfrClickerActionsArr.HandleOnPictureOpenDialogExecute: Boolean;
begin
  Result := DoOnPictureOpenDialogExecute;
end;


function TfrClickerActionsArr.HandleOnGetPictureOpenDialogFileName: string;
begin
  Result := DoOnGetPictureOpenDialogFileName;
end;


function TfrClickerActionsArr.HandleOnExecuteFindSubControlAction(AErrorLevel, AErrorCount, AFastSearchErrorCount: Integer; AFontName: string; AFontSize: Integer; out AFoundArea: TRect): Boolean;
var
  VarsBkp: string;
  Node: PVirtualNode;
  BkpErrorLevel, BkpErrorCount, BkpFastSearchErrorCount: string;
  BkpFontNames: TStringArray;
  BkpFontSizes: TIntArr;
  i: Integer;
begin
  Node := vstActions.GetFirstSelected;
  if Node = nil then
    MessageBox(Handle, 'No action is selected. Please select a FindSubControl action.', PChar(Application.Title), MB_ICONERROR);

  VarsBkp := frClickerActions.vallstVariables.Strings.Text;
  BkpErrorLevel := FClkActions[Node^.Index].FindControlOptions.ColorError;
  BkpErrorCount := FClkActions[Node^.Index].FindControlOptions.AllowedColorErrorCount;
  BkpFastSearchErrorCount := FClkActions[Node^.Index].FindControlOptions.FastSearchAllowedColorErrorCount;

  SetLength(BkpFontNames, Length(FClkActions[Node^.Index].FindControlOptions.MatchBitmapText));
  SetLength(BkpFontSizes, Length(FClkActions[Node^.Index].FindControlOptions.MatchBitmapText));
  for i := 0 to Length(BkpFontNames) - 1 do
  begin
    BkpFontNames[i] := FClkActions[Node^.Index].FindControlOptions.MatchBitmapText[i].FontName;
    BkpFontSizes[i] := FClkActions[Node^.Index].FindControlOptions.MatchBitmapText[i].FontSize;
  end;

  try
    FClkActions[Node^.Index].FindControlOptions.ColorError := IntToStr(AErrorLevel);
    FClkActions[Node^.Index].FindControlOptions.AllowedColorErrorCount := IntToStr(AErrorCount);
    FClkActions[Node^.Index].FindControlOptions.FastSearchAllowedColorErrorCount := IntToStr(AFastSearchErrorCount);

    if (AFontName <> '') and (AFontSize <> -1) then
      for i := 0 to Length(BkpFontNames) - 1 do
      begin
        FClkActions[Node^.Index].FindControlOptions.MatchBitmapText[i].FontName := AFontName;
        FClkActions[Node^.Index].FindControlOptions.MatchBitmapText[i].FontSize := AFontSize;
      end;

    FDebugging := False;  /////////////////////// to be verified if this will throw the debugger into a bad state
    PrepareFilesInServer;
    Result := PlayActionByNode(Node);

    if Result then
    begin
      AFoundArea.Left := StrToIntDef(EvaluateReplacements('$DebugVar_SubCnvXOffset$'), -1);
      AFoundArea.Top := StrToIntDef(EvaluateReplacements('$DebugVar_SubCnvYOffset$'), -1);
      AFoundArea.Width := StrToIntDef(EvaluateReplacements('$Control_Width$'), -1);
      AFoundArea.Height := StrToIntDef(EvaluateReplacements('$Control_Height$'), -1);
    end
    else
    begin
      AFoundArea.Left := -1;
      AFoundArea.Top := -1;
      AFoundArea.Width := -1;
      AFoundArea.Height := -1;
    end;
  finally
    frClickerActions.vallstVariables.Strings.Text := VarsBkp;
    FClkActions[Node^.Index].FindControlOptions.ColorError := BkpErrorLevel;
    FClkActions[Node^.Index].FindControlOptions.AllowedColorErrorCount := BkpErrorCount;
    FClkActions[Node^.Index].FindControlOptions.FastSearchAllowedColorErrorCount := BkpFastSearchErrorCount;

    for i := 0 to Length(BkpFontNames) - 1 do
    begin
      FClkActions[Node^.Index].FindControlOptions.MatchBitmapText[i].FontName := BkpFontNames[i];
      FClkActions[Node^.Index].FindControlOptions.MatchBitmapText[i].FontSize := BkpFontSizes[i];
    end;
  end;
end;


procedure TfrClickerActionsArr.HandleOnAddToLog(s: string);
begin
  AddToLog(s);
end;


procedure TfrClickerActionsArr.HandleOnGetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);
begin
  DoOnGetFontFinderSettings(AFontFinderSettings);
end;


procedure TfrClickerActionsArr.HandleOnSetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);
begin
  DoOnSetFontFinderSettings(AFontFinderSettings);
end;


procedure TfrClickerActionsArr.UpdateActionsArrFromControls(ActionIndex: Integer);
//var
//  i: Integer;
begin
  FClkActions[ActionIndex].ActionOptions.ActionCondition := frClickerActions.frClickerConditionEditor.GetActionCondition;

  //the number of items from MatchBitmapText has to match the number of frames from FBMPTextFrames
  SetLength(FClkActions[ActionIndex].FindControlOptions.MatchBitmapText, frClickerActions.frClickerFindControl.GetBMPTextFontProfilesCount);

  //for i := 0 to frClickerActions.frClickerFindControl.GetBMPTextFontProfilesCount - 1 do
  //begin
  //  FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].ForegroundColor := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFGColor;
  //  FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].BackgroundColor := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextBGColor;
  //  FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].FontName := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFontName;
  //  FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].FontSize := StrToIntDef(frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextSize, 8);
  //  FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].Bold := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].Bold;
  //  FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].Italic := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].Italic;
  //  FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].Underline := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].Underline;
  //  FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].StrikeOut := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].StrikeOut;
  //
  //  if frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFontQualityIndex = -1 then
  //    FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].FontQuality := fqDefault
  //  else
  //  begin
  //    FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].FontQuality := TFontQuality(frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFontQualityIndex);
  //
  //    if frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFontQualityIndex = Integer(High(TFontQuality)) + 1 then  //is a replacement
  //    begin
  //      FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].FontQualityReplacement := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].FontQualityReplacement;
  //      FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].FontQualityUsesReplacement := True;
  //    end
  //    else
  //      FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].FontQualityUsesReplacement := False;
  //  end;
  //
  //  FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].ProfileName := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].ProfileName;
  //
  //  FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].CropLeft := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].CropLeft;
  //  FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].CropTop := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].CropTop;
  //  FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].CropRight := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].CropRight;
  //  FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].CropBottom := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].CropBottom;
  //  FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].IgnoreBackgroundColor := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].IgnoreBackgroundColor;
  //end;

  FClkActions[ActionIndex].FindControlOptions.MatchBitmapFiles := frClickerActions.frClickerFindControl.lstMatchBitmapFiles.Items.Text;

  FClkActions[ActionIndex].CallTemplateOptions.ListOfCustomVarsAndValues := FastReplace_ReturnTo45(frClickerActions.ListOfCustomVariables);

  CopyActionContent(frClickerActions.EditingAction^, FClkActions[ActionIndex]);  //uncomment this after removing above code
end;


procedure TfrClickerActionsArr.UpdateControlsFromActionsArr(ActionIndex: Integer);
var
  i: Integer;
  TempProfileName: string;
begin
  frClickerActions.frClickerConditionEditor.DisplayActionCondition(FClkActions[ActionIndex].ActionOptions.ActionCondition);

  /////the new content, instead of editboxes and checkboxes
  CopyActionContent(FClkActions[ActionIndex], frClickerActions.EditingAction^);

  //the number of items from MatchBitmapText has to match the number of frames from FBMPTextFrames
  frClickerActions.frClickerFindControl.CreateBMPTextFrames(Length(FClkActions[ActionIndex].FindControlOptions.MatchBitmapText)); //do not use SetLength(frClickerActions.FBMPTextFrames, Length(FClkActions[ActionIndex].FindControlOptions.MatchBitmapText));

  for i := 0 to frClickerActions.frClickerFindControl.GetBMPTextFontProfilesCount - 1 do  //this part is still required when selecting an action
  begin
    frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFGColor := FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].ForegroundColor;
    frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextBGColor := FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].BackgroundColor;
    //frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].FGColor := HexToInt(EvaluateReplacements(FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].ForegroundColor));
    //frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].BGColor := HexToInt(EvaluateReplacements(FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].BackgroundColor));
    frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFontName := FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].FontName;
    frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextSize := IntToStr(FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].FontSize);
    frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].Bold := FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].Bold;
    frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].Italic:= FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].Italic;
    frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].Underline := FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].Underline;
    frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].StrikeOut := FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].StrikeOut;

    if FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].FontQualityUsesReplacement then
    begin
      frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFontQualityIndex := Integer(High(TFontQuality)) + 1;
      frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].FontQualityReplacement := FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].FontQualityReplacement;
    end
    else
      frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFontQualityIndex := Integer(FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].FontQuality);

    TempProfileName := FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].ProfileName;
    //frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].ProfileName := TempProfileName;
    //frClickerActions.frClickerFindControl.tabctrlBMPText.Tabs.Strings[i] := TempProfileName;     //remove these calls if UpdateFontProfileName works as expected
    frClickerActions.frClickerFindControl.UpdateFontProfileName(i, TempProfileName);

    frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].CropLeft := FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].CropLeft;
    frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].CropTop := FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].CropTop;
    frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].CropRight := FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].CropRight;
    frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].CropBottom := FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].CropBottom;

    frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].IgnoreBackgroundColor := FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i].IgnoreBackgroundColor;

    //frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].UpdateSelectionLabelsFromCropEditBoxes;  //replaced below with other call
  end;

  frClickerActions.frClickerFindControl.SetBMPTextFrameVisibility;

  frClickerActions.frClickerFindControl.UpdateListsOfSearchFiles(FClkActions[ActionIndex].FindControlOptions.MatchBitmapFiles, FClkActions[ActionIndex].FindControlOptions.MatchPrimitiveFiles);
  frClickerActions.frClickerFindControl.UpdateBitmapAlgorithmSettings;

  frClickerActions.LoadListOfAvailableTemplates;

  frClickerActions.ListOfCustomVariables := FastReplace_45ToReturn(FClkActions[ActionIndex].CallTemplateOptions.ListOfCustomVarsAndValues);

  frClickerActions.frClickerFindControl.UpdatePreviewIcons;

  frClickerActions.UpdateControlWidthHeightLabels;
  frClickerActions.UpdateUseWholeScreenLabel(FClkActions[ActionIndex].FindControlOptions.UseWholeScreen);
  frClickerActions.frClickerFindControl.UpdateSearchAreaLabelsFromKeysOnInitRect(FClkActions[ActionIndex].FindControlOptions.InitialRectangle);

  frClickerActions.frClickerExecApp.memExecAppParams.Lines.Text := FClkActions[ActionIndex].ExecAppOptions.ListOfParams;
  frClickerActions.frClickerSetVar.SetListOfSetVars(FClkActions[ActionIndex].SetVarOptions);

  frClickerActions.CurrentlyEditingActionType := TClkAction(FClkActions[ActionIndex].ActionOptions.Action);

  if frClickerActions.CurrentlyEditingActionType = acFindSubControl then
  begin
    frClickerActions.frClickerFindControl.PreviewText;

    for i := 0 to frClickerActions.frClickerFindControl.GetBMPTextFontProfilesCount - 1 do
      frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].UpdateSelectionLabelsFromCropInfo(FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[i]);
  end;
end;


procedure TfrClickerActionsArr.UpdateModifiedLabel;
begin
  if FModified then
  begin
    lblModifiedStatus.Caption := 'Modified: ' + ExtractFileName(FFileName);
    lblModifiedStatus.Font.Color := $000429FF;
    spdbtnSaveTemplate.Font.Color := $00241CED;
  end
  else
  begin
    lblModifiedStatus.Caption := 'Up to date: ' + ExtractFileName(FFileName);
    lblModifiedStatus.Font.Color := $00007500;
    spdbtnSaveTemplate.Font.Color := clWindowText;
  end;

  spdbtnSaveTemplate.Font.Bold := FModified;

  lblModifiedStatus.Repaint;
end;


procedure TfrClickerActionsArr.SetFullTemplatesDir(Value: string);
begin
  if FullTemplatesDir <> Value then
  begin
    Value := StringReplace(Value, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);
    FFullTemplatesDir := Value;
    frClickerActions.FullTemplatesDir := Value; //this is how the frame gets the path to templates
    AddToLog('Setting templates dir to "' + Value + '"');
  end;
end;


procedure TfrClickerActionsArr.SetModified(Value: Boolean);
begin
  if FModified <> Value then
  begin
    FModified := Value;
    UpdateModifiedLabel;
  end;
end;


function TfrClickerActionsArr.EvaluateReplacements(s: string; Recursive: Boolean = True): string;
begin
  Result := frClickerActions.EvaluateReplacements(s, Recursive);
end;


function TfrClickerActionsArr.GetActionVarValue(VarName: string): string;
begin
  Result := frClickerActions.vallstVariables.Values[VarName];
end;


procedure SearchAction(vst: TVirtualStringTree; ActionNameToSearchFor: string; var AClkActions: TClkActionsRecArr);
var
  Node, LastNode, OldNode: PVirtualNode;
  UpperCaseActionNameToSearchFor: string;
begin
  Node := vst.GetFirst;
  if Node = nil then
    Exit;

  UpperCaseActionNameToSearchFor := UpperCase(ActionNameToSearchFor);  

  LastNode := vst.GetLast;
  vst.BeginUpdate;
  try
    repeat
      vst.IsVisible[Node] := (ActionNameToSearchFor = '') or (Pos(UpperCaseActionNameToSearchFor, UpperCase(AClkActions[Node^.Index].ActionOptions.ActionName)) > 0);

      OldNode := Node;
      Node := Node^.NextSibling;
    until OldNode = LastNode;
  finally
    vst.EndUpdate;
  end;
end;


procedure TfrClickerActionsArr.lbeSearchActionChange(Sender: TObject);
begin
  if lbeSearchAction.Text = '' then
    lbeSearchAction.Color := clDefault
  else
    lbeSearchAction.Color := clYellow;

  SearchAction(vstActions, lbeSearchAction.Text, FClkActions);
end;


procedure TfrClickerActionsArr.SetActionVarValue(VarName, VarValue: string);
begin
  frClickerActions.vallstVariables.Values[VarName] := FastReplace_ReturnTo68(VarValue);
  if VarName = '$ExecAction_Err$' then
    AddToLog(DateTimeToStr(Now) + '  ' + VarValue);
end;


procedure TfrClickerActionsArr.AppendErrorMessageToActionVar(NewErrMsg: string);
begin
  SetActionVarValue('$ExecAction_Err$', GetActionVarValue('$ExecAction_Err$') + FastReplace_ReturnTo68(NewErrMsg));
end;


procedure TfrClickerActionsArr.PrependErrorMessageToActionVar(NewErrMsg: string);
begin
  SetActionVarValue('$ExecAction_Err$', FastReplace_ReturnTo68(NewErrMsg) + GetActionVarValue('$ExecAction_Err$'));
end;


function TfrClickerActionsArr.EvaluateHTTP(AValue: string): string;
var
  TempIdHTTP: TIdHTTP;
begin
  Result := AValue;

  if (Pos('$HTTP://', UpperCase(AValue)) > 0) or (Pos('$HTTPS://', UpperCase(AValue)) > 0) then
    if AValue[Length(AValue)] = '$' then
    begin
      AValue := Copy(AValue, 2, Length(AValue) - 2);
      AValue := EvaluateReplacements(AValue);

      try
        TempIdHTTP := TIdHTTP.Create(nil);
        try
          TempIdHTTP.ConnectTimeout := 1000;
          TempIdHTTP.ReadTimeout := 1000;
          Result := TempIdHTTP.Get(AValue);
        finally
          TempIdHTTP.Free;
        end;
      except
        on E: Exception do
        begin
          Result := E.Message;
          AppendErrorMessageToActionVar(Result);
        end;
      end;
    end;
end;


procedure TfrClickerActionsArr.ExecuteActionFromClient(AIsDebuggingFromClient: Boolean); //expects RemoteExActionIndex to be set, then enables tmrExecActionFromSrvModule
begin
  FExecutingActionFromRemote := True;
  FFileLocationOfDepsIsMem := True;
  FDebugging := AIsDebuggingFromClient;  // set a flag that tells the player to wait for next "step over" - the last action of a called template should not wait for debugger
  FContinuePlayingBySteppingInto := AIsDebuggingFromClient;
  tmrExecActionFromSrvModule.Enabled := True;
end;


procedure TfrClickerActionsArr.ExitTemplateFromRemote;
begin
  FClosingTemplate := True;
end;


function TfrClickerActionsArr.EvaluateActionCondition(ActionIndex: Integer): Boolean;
begin
  Result := ClickerUtils.EvaluateActionCondition(FClkActions[ActionIndex].ActionOptions.ActionCondition, EvaluateReplacements);
end;


function TfrClickerActionsArr.ExecuteActionAtIndex(AActionIndex: Integer): Boolean;
begin
  Result := True;

  SetActionVarValue('$ExecAction_Err$', '');

  if (AActionIndex < 0) or (AActionIndex > Length(FClkActions) - 1) then
    raise Exception.Create('ActionIndex out of bounds: ' + IntToStr(AActionIndex));

  case FClkActions[AActionIndex].ActionOptions.Action of
    acClick: Result := FActionExecution.ExecuteMultiClickAction(FClkActions[AActionIndex].ClickOptions);
    acExecApp: Result := FActionExecution.ExecuteExecAppAction(FClkActions[AActionIndex].ExecAppOptions, FClkActions[AActionIndex].ActionOptions);
    acFindControl: Result := FActionExecution.ExecuteFindControlActionWithTimeout(FClkActions[AActionIndex].FindControlOptions, FClkActions[AActionIndex].ActionOptions, False);
    acFindSubControl: Result := FActionExecution.ExecuteFindControlActionWithTimeout(FClkActions[AActionIndex].FindControlOptions, FClkActions[AActionIndex].ActionOptions, True);
    acSetControlText: Result := FActionExecution.ExecuteSetControlTextAction(FClkActions[AActionIndex].SetTextOptions);
    acCallTemplate: Result := FActionExecution.ExecuteLoopedCallTemplateAction(FClkActions[AActionIndex].CallTemplateOptions, FContinuePlayingBySteppingInto, {FShouldStopAtBreakPoint replaced by FDebugging} FDebugging);
    acSleep: Result := FActionExecution.ExecuteSleepAction(FClkActions[AActionIndex].SleepOptions, FClkActions[AActionIndex].ActionOptions);
    acSetVar: Result := FActionExecution.ExecuteSetVarAction(FClkActions[AActionIndex].SetVarOptions);
    acWindowOperations: Result := FActionExecution.ExecuteWindowOperationsAction(FClkActions[AActionIndex].WindowOperationsOptions);
  end;  //case
end;


//called in client mode

//Sends this template and the bitmaps it might use. This is called before the server can report any missing files.
function TfrClickerActionsArr.SendMissingFilesToServer: string;
begin
  Result := ClickerActionsClient.SendMissingFilesToServer(FRemoteAddress, FClkActions);
end;


function TfrClickerActionsArr.SetCurrentClientTemplateInServer(ASendFileOnly: Boolean = False): string;
begin
  Result := SetClientTemplateInServer(FRemoteAddress, FFileName, FClkActions, FStackLevel, ASendFileOnly);
end;


procedure TfrClickerActionsArr.PrepareFilesInServer;
begin
  if not FExecutesRemotely then
    Exit;

  if GetServerFileExpectancy(FRemoteAddress) = CREResp_FileExpectancy_ValueFromClient then
    if FStackLevel = 0 then  //Load only the main file. The others should be automatically handled by server.
    begin
      AddToLog('Should send template and other files to server...');
      AddToLog(SetCurrentClientTemplateInServer);
      AddToLog(SendMissingFilesToServer);
    end;
end;


function TfrClickerActionsArr.LocalOnExecuteRemoteActionAtIndex(AActionIndex, AStackLevel: Integer; AVarReplacements: TStringList; AIsDebugging: Boolean): Boolean;
const
  CUsesInMemFiles: array[Boolean] of string = (CREParam_FileLocation_ValueDisk, CREParam_FileLocation_ValueMem);
var
  Th: TClientThread;
  ServerResponse: string;
  EnteredTemplateResult: Boolean;
begin
  Result := False;

  Th := TClientThread.Create(True);   //without using thread, the client blocks both this UI and the server's UI, because it doesn't read  - some sort of deadlock
  try
    Th.ConnectTimeout := GeneralConnectTimeout;
    Th.Link := FRemoteAddress + CRECmd_ExecuteCommandAtIndex + '?' +
                                CREParam_ActionIdx + '=' + IntToStr(AActionIndex) + '&' +
                                CREParam_StackLevel + '=' + IntToStr(AStackLevel) + '&' +
                                CREParam_IsDebugging + '=' + IntToStr(Ord(AIsDebugging)) + '&' +
                                CREParam_FileLocation + '= ' + CUsesInMemFiles[False]; //this should come from a class field
    Th.Start;

    EnteredTemplateResult := True;

    if AIsDebugging then
      if FClkActions[AActionIndex].ActionOptions.Action = acCallTemplate then
        EnteredTemplateResult := FActionExecution.ExecuteLoopedCallTemplateAction(FClkActions[AActionIndex].CallTemplateOptions, FContinuePlayingBySteppingInto, AIsDebugging);

    WaitForServerResponse(Th);

    ServerResponse := FastReplace_87ToReturn(Th.Result);

    if (Trim(ServerResponse) = '') or (Pos('$Control_Class$', ServerResponse) = 0) then // uses '$Control_Class$' as a way to find out if the result is a valid list of variables
    begin
      AVarReplacements.Values['$ServerResponse$'] := ServerResponse;
      AddToLog(ServerResponse);
    end
    else
      AVarReplacements.Text := ServerResponse;

    Result := (AVarReplacements.Values[CREResp_RemoteExecResponseVar] = '1') and EnteredTemplateResult;
  finally
    Th.Free;
  end;
end;


function TfrClickerActionsArr.DoExecuteRemoteActionAtIndex(AActionIndex: Integer): Boolean;
var
  VarReplacements: TStringList;
  Msg, Fnm, FuncParams: string;
  TempBmp: TBitmap;
  VarNames, VarValues: TStringList;
  Idx, i: Integer;
begin
  VarReplacements := TStringList.Create;
  try
    try
      VarReplacements.AddStrings(frClickerActions.vallstVariables.Strings); //init with something, in case the server can't be reached

      Result := LocalOnExecuteRemoteActionAtIndex(AActionIndex, FStackLevel, VarReplacements, FContinuePlayingBySteppingInto);

      frClickerActions.vallstVariables.Strings.Clear;
      frClickerActions.vallstVariables.Strings.AddStrings(VarReplacements);
    except
      on E: Exception do
      begin
        Msg := E.Message + '  at action index: ' + IntToStr(AActionIndex);
        AddToLog('Remote Ex: ' + Msg);
        SetActionVarValue('$RemoteEx$', Msg);
      end;
    end;
  finally
    VarReplacements.Free;
  end;

  if FClkActions[AActionIndex].ActionOptions.Action = acFindSubControl then
  begin
    TempBmp := TBitmap.Create;
    try
      TempBmp.PixelFormat := pf24bit;

      Msg := 'content not set';
      try
        TempBmp.Width := TempBmp.Canvas.TextWidth(Msg) + 10;
        TempBmp.Height := 15;
        TempBmp.Canvas.Font.Color := clFuchsia;  //something different than clRed
        TempBmp.Canvas.TextOut(5, 1, Msg);
      except
        AddToLog('error setting default bitmap content -----------------------------');
      end;

      Msg := GetDebugImageFromServer(FRemoteAddress, FStackLevel, TempBmp, False);

      if Msg <> '' then
      begin
        AddToLog('Remote DbgImg: ' + Msg);
        SetActionVarValue('$RemoteDbgImg$', Msg);
      end
      else
      begin
        frClickerActions.imgDebugBmp.Canvas.Pen.Color := clWhite;
        frClickerActions.imgDebugBmp.Canvas.Brush.Color := clWhite;
        frClickerActions.imgDebugBmp.Canvas.Rectangle(0, 0, frClickerActions.imgDebugBmp.Width, frClickerActions.imgDebugBmp.Height);
        frClickerActions.imgDebugBmp.Picture.Bitmap.Width := TempBmp.Width;
        frClickerActions.imgDebugBmp.Picture.Bitmap.Height := TempBmp.Height;
        frClickerActions.imgDebugBmp.Picture.Bitmap.Assign(TempBmp);

        MakeImageContentTransparent(frClickerActions.imgDebugBmp);

        AddToLog('Remote DbgImgSize: ' + Msg);
        SetActionVarValue('$RemoteDbgImgSize$', Msg);
      end;
    finally
      TempBmp.Free;
    end;

    Exit;
  end;

  if FClkActions[AActionIndex].ActionOptions.Action = acSetVar then
  begin
    VarNames := TStringList.Create;
    VarValues := TStringList.Create;
    try
      VarNames.Text := FClkActions[AActionIndex].SetVarOptions.ListOfVarNames;
      VarValues.Text := FClkActions[AActionIndex].SetVarOptions.ListOfVarValues;

      Idx := -1;
      for i := 0 to VarNames.Count - 1 do
        if Pos('$RenderBmpExternally(', VarNames.Strings[i]) = 1 then   //this accepts 0 or more arguments to $RenderBmpExternally
        begin
          Idx := i;
          Break;
        end;

      if Idx > -1 then
      begin
        FuncParams := EvaluateReplacements(VarNames.Strings[Idx]);    //this should be a #4#5 separated list of key-value pairs
        VarValues.Text := FastReplace_45ToReturn(FuncParams);   //it's ok to reuse VarValues here
        Fnm := VarValues.Values[CExtBmp_Filename];   //do not use CREParam_FileName here  (it's 'n' vs. 'N')
        Fnm := Copy(Fnm, 1, CExtBmp_FilenameMaxLen);

        if Fnm = '' then
          AddToLog('Cannot get filename from $RenderBmpExternally()$ call, when retrieving bitmap from server.')
        else
          DoOnRetrieveRenderedBmpFromServer(FRemoteAddress, Fnm);
      end;
    finally
      VarNames.Free;
      VarValues.Free;
    end;

    Exit;
  end;
end;


procedure TfrClickerActionsArr.DoWaitForFileAvailability(AFileName: string);
begin
  if Assigned(FOnWaitForFileAvailability) then
    FOnWaitForFileAvailability(AFileName);
end;


procedure TfrClickerActionsArr.DoWaitForMultipleFilesAvailability(AListOfFiles: TStringList);
begin
  if Assigned(FOnWaitForMultipleFilesAvailability) then
    FOnWaitForMultipleFilesAvailability(AListOfFiles);
end;


procedure TfrClickerActionsArr.DoWaitForBitmapsAvailability(AListOfFiles: TStringList);
begin
  if Assigned(FOnWaitForBitmapsAvailability) then
    FOnWaitForBitmapsAvailability(AListOfFiles);
end;


function TfrClickerActionsArr.DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if Assigned(FOnLoadBitmap) then
    Result := FOnLoadBitmap(ABitmap, AFileName)
  else
    Result := False;
end;


function TfrClickerActionsArr.DoOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if Assigned(FOnLoadRenderedBitmap) then
    Result := FOnLoadRenderedBitmap(ABitmap, AFileName)
  else
    Result := False;
end;


function TfrClickerActionsArr.DoOnRenderBmpExternally(AFilename: string): string;
begin
  if Assigned(FOnRenderBmpExternally) then
    Result := FOnRenderBmpExternally(AFileName)
  else
    Result := 'OnRenderBmpExternally not assigned';
end;


procedure TfrClickerActionsArr.DoOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages: TStringList);
begin
  if not Assigned(FOnGetListOfExternallyRenderedImages) then
    raise Exception.Create('OnGetListOfExternallyRenderedImages not assigned.')
  else
    FOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages);
end;


procedure TfrClickerActionsArr.DoOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
begin
  if not Assigned(FOnLoadPrimitivesFile) then
    raise Exception.Create('OnLoadPrimitivesFile not assigned.')
  else
    FOnLoadPrimitivesFile(AFileName, APrimitives, AOrders, ASettings);
end;


procedure TfrClickerActionsArr.DoOnSavePrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
begin
  if not Assigned(FOnSavePrimitivesFile) then
    raise Exception.Create('OnSavePrimitivesFile not assigned.')
  else
    FOnSavePrimitivesFile(AFileName, APrimitives, AOrders, ASettings);
end;


function TfrClickerActionsArr.DoOnFileExists(const AFileName: string): Boolean;
begin
  if not Assigned(FOnFileExists) then
    raise Exception.Create('OnFileExists is not assigned.')
  else
    Result := FOnFileExists(AFileName);
end;


function TfrClickerActionsArr.DoOnTClkIniReadonlyFileCreate(AFileName: string): TClkIniReadonlyFile;
begin
  if not Assigned(FOnTClkIniReadonlyFileCreate) then
    raise Exception.Create('OnTClkIniReadonlyFileCreate is not assigned.')
  else
    Result := FOnTClkIniReadonlyFileCreate(AFileName);
end;


procedure TfrClickerActionsArr.DoOnSaveTemplateToFile(AStringList: TStringList; const AFileName: string);
begin
  if not Assigned(FOnSaveTemplateToFile) then
    raise Exception.Create('OnSaveTemplateToFile is not assigned.')
  else
    FOnSaveTemplateToFile(AStringList, AFileName);
end;


procedure TfrClickerActionsArr.DoOnSetOpenDialogMultiSelect;
begin
  if not Assigned(FOnSetOpenDialogMultiSelect) then
    raise Exception.Create('OnSetOpenDialogMultiSelect is not assigned.')
  else
    FOnSetOpenDialogMultiSelect;
end;


procedure TfrClickerActionsArr.DoOnSetOpenDialogInitialDir(AInitialDir: string);
begin
  if not Assigned(FOnSetOpenDialogInitialDir) then
    raise Exception.Create('OnSetOpenDialogInitialDir is not assigned.')
  else
    FOnSetOpenDialogInitialDir(AInitialDir);
end;


function TfrClickerActionsArr.DoOnOpenDialogExecute(AFilter: string): Boolean;
begin
  if not Assigned(FOnOpenDialogExecute) then
    raise Exception.Create('OnOpenDialogExecute is not assigned.')
  else
    Result := FOnOpenDialogExecute(AFilter);
end;


function TfrClickerActionsArr.DoOnGetOpenDialogFileName: string;
begin
  if not Assigned(FOnGetOpenDialogFileName) then
    raise Exception.Create('OnGetOpenDialogFileName is not assigned.')
  else
    Result := FOnGetOpenDialogFileName;
end;


procedure TfrClickerActionsArr.DoOnSetSaveDialogInitialDir(AInitialDir: string);
begin
  if not Assigned(FOnSetSaveDialogInitialDir) then
    raise Exception.Create('OnSetSaveDialogInitialDir is not assigned.')
  else
    FOnSetSaveDialogInitialDir(AInitialDir);
end;


function TfrClickerActionsArr.DoOnSaveDialogExecute(AFilter: string): Boolean;
begin
  if not Assigned(FOnSaveDialogExecute) then
    raise Exception.Create('OnSaveDialogExecute is not assigned.')
  else
    Result := FOnSaveDialogExecute(AFilter);
end;


function TfrClickerActionsArr.DoOnGetSaveDialogFileName: string;
begin
  if not Assigned(FOnGetSaveDialogFileName) then
    raise Exception.Create('OnGetSaveDialogFileName is not assigned.')
  else
    Result := FOnGetSaveDialogFileName;
end;


procedure TfrClickerActionsArr.DoOnSetSaveDialogFileName(AFileName: string);
begin
  if not Assigned(FOnSetSaveDialogFileName) then
    raise Exception.Create('OnSetSaveDialogFileName is not assigned.')
  else
    FOnSetSaveDialogFileName(AFileName);
end;


procedure TfrClickerActionsArr.DoOnSetPictureSetOpenDialogMultiSelect;
begin
  if not Assigned(FOnSetPictureSetOpenDialogMultiSelect) then
    raise Exception.Create('OnSetPictureSetOpenDialogMultiSelect not assigned.')
  else
    FOnSetPictureSetOpenDialogMultiSelect;
end;


procedure TfrClickerActionsArr.DoOnSetPictureOpenDialogInitialDir(AInitialDir: string);
begin
  if not Assigned(FOnSetPictureOpenDialogInitialDir) then
    raise Exception.Create('OnSetPictureOpenDialogInitialDir not assigned.')
  else
    FOnSetPictureOpenDialogInitialDir(AInitialDir);
end;


function TfrClickerActionsArr.DoOnPictureOpenDialogExecute: Boolean;
begin
  if not Assigned(FOnPictureOpenDialogExecute) then
    raise Exception.Create('OnPictureOpenDialogExecute not assigned.')
  else
    Result := FOnPictureOpenDialogExecute;
end;


function TfrClickerActionsArr.DoOnGetPictureOpenDialogFileName: string;
begin
  if not Assigned(FOnGetPictureOpenDialogFileName) then
    raise Exception.Create('OnGetPictureOpenDialogFileName not assigned.')
  else
    Result := FOnGetPictureOpenDialogFileName;
end;


function TfrClickerActionsArr.DoOnGetGridDrawingOption: TDisplayGridLineOption;
begin
  if not Assigned(FOnGetGridDrawingOption) then
    raise Exception.Create('OnGetGridDrawingOption not assigned.')
  else
    Result := FOnGetGridDrawingOption;
end;


procedure TfrClickerActionsArr.DoOnGetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);
begin
  if not Assigned(FOnGetFontFinderSettings) then
    raise Exception.Create('OnGetFontFinderSettings not assigned.')
  else
    FOnGetFontFinderSettings(AFontFinderSettings);
end;


procedure TfrClickerActionsArr.DoOnSetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);
begin
  if not Assigned(FOnSetFontFinderSettings) then
    raise Exception.Create('OnSetFontFinderSettings not assigned.')
  else
    FOnSetFontFinderSettings(AFontFinderSettings);
end;


procedure TfrClickerActionsArr.DoOnRetrieveRenderedBmpFromServer(ARemoteAddress, AFnm: string);
begin
  if not Assigned(FOnRetrieveRenderedBmpFromServer) then
    raise Exception.Create('OnOnRetrieveRenderedBmpFromServer not assigned.')
  else
    FOnRetrieveRenderedBmpFromServer(ARemoteAddress, AFnm);
end;


function TfrClickerActionsArr.PlayActionByNode(Node: PVirtualNode): Boolean;
var
  ActionIndex: Integer;
begin
  Result := True;

  ActionIndex := Node^.Index;

  FClkActions[ActionIndex].ActionStatus := asInProgress;
  SetActionVarValue('$ExitCode$', '');
  vstActions.RepaintNode(Node);

  try
    FClkActions[ActionIndex].ActionSkipped := False;

    if EvaluateActionCondition(ActionIndex) then
    begin
      if not FExecutesRemotely then
        Result := ExecuteActionAtIndex(ActionIndex)
      else
      begin
        Result := DoExecuteRemoteActionAtIndex(ActionIndex);   //called by client, to send requests
        Result := Result and (ActionStatusStrToActionStatus(GetActionVarValue('$LastAction_Status$')) = asSuccessful);
      end;

      SetActionVarValue('$LastAction_Skipped$', 'No');
    end
    else
    begin
      FClkActions[ActionIndex].ActionSkipped := True;
      SetActionVarValue('$ExecAction_Err$', 'Action condition is false.');
      SetActionVarValue('$LastAction_Skipped$', 'Yes');
      Result := True;
    end;
  except          //added later, because of 'Handle is 0 in component screenshot' exception from screenshot function, to prevent popup
    on E: Exception do
    begin
      AppendErrorMessageToActionVar('Exception: ' + E.Message);
      Result := False;
    end;
  end;

  if Result then
  begin
    if FClkActions[ActionIndex].ActionOptions.Action <> acCallTemplate then
      FClkActions[ActionIndex].ActionStatus := asSuccessful
    else
      FClkActions[ActionIndex].ActionStatus := ActionStatusStrToActionStatus(GetActionVarValue('$LastAction_Status$'));  ///////////// not sure if this is the way to go
  end
  else
  begin
    if {((FClkActions[ActionIndex].ActionOptions.Action = acFindControl) or (FClkActions[ActionIndex].ActionOptions.Action = acFindSubControl)) and}
       not Result and
       FClkActions[ActionIndex].FindControlOptions.AllowToFail then
    begin
      FClkActions[ActionIndex].ActionStatus := asAllowedFailed;
      Result := True; //allow further execution
    end
    else
      if GetActionVarValue('$ExitCode$') <> '0' then
        FClkActions[ActionIndex].ActionStatus := asFailed
      else
        FClkActions[ActionIndex].ActionStatus := asSuccessful;
  end;

  if not FExecutesRemotely then
    SetActionVarValue('$LastAction_Status$', CActionStatusStr[FClkActions[ActionIndex].ActionStatus]);

  vstActions.RepaintNode(Node);
end;


procedure TfrClickerActionsArr.PlaySelected;
var
  Node: PVirtualNode;
begin
  Node := vstActions.GetFirstSelected;
  if Node = nil then
    Exit; //do not show a dialog here

  PlayActionByNode(Node);
end;


procedure TfrClickerActionsArr.WaitInDebuggingMode;
begin
  imgWaitingInDebuggingMode.Show;
  imgWaitingInDebuggingMode.Tag := imgWaitingInDebuggingMode.Tag + 1;
  imgWaitingInDebuggingMode.Hint := 'Waiting in debugging mode.' + #13#10 + 'Call stack level: ' + IntToStr(imgWaitingInDebuggingMode.Tag);
  try
    repeat
      Application.ProcessMessages;

      if FStopAllActionsOnDemandFromParent <> nil then
        if FStopAllActionsOnDemandFromParent^ then
        begin
          FStopAllActionsOnDemand := True;
          Exit;
        end;

      if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) and (GetAsyncKeyState(VK_F2) < 0) then
      begin
        if FStopAllActionsOnDemandFromParent <> nil then
          FStopAllActionsOnDemandFromParent^ := True;

        FStopAllActionsOnDemand := True;
        Break;
      end;

      if FContinuePlayingAll then
        Break;

      if FContinuePlayingNext then
      begin
        FContinuePlayingNext := False;  //reset
        Break;
      end;

      if FContinuePlayingBySteppingInto then
        Break;

      Sleep(20);
    until not FPlaying;  //Stop button
  finally
    imgWaitingInDebuggingMode.Show;
    imgWaitingInDebuggingMode.Tag := imgWaitingInDebuggingMode.Tag - 1;
    imgWaitingInDebuggingMode.Hint := 'Waiting in debugging mode.' + #13#10 + 'Call stack level: ' + IntToStr(imgWaitingInDebuggingMode.Tag);
  end;
end;


procedure TfrClickerActionsArr.ResetDebuggingStatusOnAllActions;
var
  i: Integer;
begin
  for i := 0 to Length(FClkActions) - 1 do
    FClkActions[i].ActionDebuggingStatus := adsNone;

  vstActions.Repaint;
end;


function TfrClickerActionsArr.ShouldStopActionAtBreakpoint(AActionBreakPoint: TActionBreakPoint): Boolean;
begin
  Result := AActionBreakPoint.Exists and
            AActionBreakPoint.Enabled and
            ClickerUtils.EvaluateActionCondition(AActionBreakPoint.Condition, EvaluateReplacements);
end;


function TfrClickerActionsArr.GetNodeByIndex(ANodeIndex: Integer): PVirtualNode;
var
  Node: PVirtualNode;
begin
  Result := nil;
  Node := vstActions.GetFirst;
  if Node = nil then
    Exit;

  repeat
    if Integer(Node^.Index) = ANodeIndex then
    begin
      Result := Node;
      Exit;
    end;

    Node := Node^.NextSibling;
  until Node = nil;
end;


procedure TfrClickerActionsArr.HighlightCurrentlyExecutedAction(Node: PVirtualNode);
begin
  if FExecutingActionFromRemote then
  begin   //some green actions in server mode
    vstActions.Colors.UnfocusedSelectionColor := $00D8FFD8;
    vstActions.Colors.FocusedSelectionColor := $00408823;
  end
  else
  begin
    vstActions.Colors.UnfocusedSelectionColor := clGradientInactiveCaption;
    vstActions.Colors.FocusedSelectionColor := clHighlight;
  end;

  vstActions.ClearSelection;
  vstActions.Selected[Node] := True;
  vstActions.ScrollIntoView(Node, True);
  UpdateControlsFromActionsArr(Node^.Index);
  StopGlowingUpdateButton;
  frClickerActions.UpdatePageControlActionExecutionIcons;

  /////// debugging icons
  FClkActions[Node^.Index].ActionDebuggingStatus := adsNext;
  vstActions.InvalidateNode(Node);
  if Node^.NextSibling <> nil then
  begin
    FClkActions[Node^.Index + 1].ActionDebuggingStatus := adsNone;
    vstActions.InvalidateNode(Node^.NextSibling);
  end;
  /////// debugging icons
end;


function TfrClickerActionsArr.PlayAllActions(IsDebugging: Boolean = False; StartAtSelected: Boolean = False): Boolean;
var
  Node, OldNode, LastNode: PVirtualNode;
  ClosingTemplateResponse: string;
  tk: QWord;
  IsAtBreakPoint: Boolean;
begin
  FPlayingAllActions := True;
  try
    Result := False;

    if IsDebugging and FExecutingActionFromRemote and (FStackLevel > 0) and not FUseLocalDebugger then  //  this loop allows receiving missing files
    begin
      SetActionVarValue('$DbgPlayAllActions$', 'Waiting for ClosingTemplate');

      imgWaitingInPreDebuggingMode.Show;
      tk := GetTickCount64;
      repeat
        Application.ProcessMessages;
        Sleep(2);
      until FClosingTemplate or (GetTickCount64 - tk > 3600000);  //1h
      imgWaitingInPreDebuggingMode.Hide;

      SetActionVarValue('$DbgPlayAllActions$', 'ClosingTemplate at stack level' + IntToStr(FStackLevel));

      Result := True;
      Exit;
    end
    else
      SetActionVarValue('$DbgPlayAllActions$', 'FStackLevel: ' + IntToStr(FStackLevel));

    SetActionVarValue('$DbgPlayAllActions$', GetActionVarValue('$DbgPlayAllActions$') + '  IsDebugging: ' + IntToStr(Ord(IsDebugging)) + '  ExecutingActionFromRemote: ' + IntToStr(Ord(FExecutingActionFromRemote)) + '  ClosingTemplate: ' + IntToStr(Ord(FClosingTemplate)) + '  FileLocationOfDepsIsMem: ' + IntToStr(Ord(FFileLocationOfDepsIsMem)));

    try
      if StartAtSelected then
      begin
        Node := vstActions.GetFirstSelected;
        if Node = nil then
          Node := vstActions.GetFirst;
      end
      else
        Node := vstActions.GetFirst;

      if Node = nil then
        Exit; //do not show a dialog here

      if IsDebugging or FShouldStopAtBreakPoint then
      begin
        spdbtnContinuePlayingAll.Enabled := True;
        spdbtnStepOver.Enabled := True;
        spdbtnStepInto.Enabled := False; //this one is false
      end;

      FContinuePlayingAll := False;
      FContinuePlayingNext := False;
      FContinuePlayingBySteppingInto := False;

      ResetDebuggingStatusOnAllActions;

      try
        LastNode := vstActions.GetLast;
        repeat
          FClkActions[Node^.Index].ActionOptions.ExecutionIndex := IntToStr(Node^.Index);  //required by FindSubControl, to update the UI, when adding a default font profile
          HighlightCurrentlyExecutedAction(Node);

          spdbtnStepInto.Enabled := IsDebugging and (FClkActions[Node^.Index].ActionOptions.Action = acCallTemplate);

          IsAtBreakPoint := ShouldStopActionAtBreakpoint(FClkActions[Node^.Index].ActionBreakPoint);
          if IsAtBreakPoint then
          begin
            FContinuePlayingAll := False;
            spdbtnContinuePlayingAll.Enabled := True;
            spdbtnStepOver.Enabled := True;
          end;

          if (IsDebugging or (FShouldStopAtBreakPoint and IsAtBreakPoint)) and not FContinuePlayingAll then   //pause execution if debugging
          begin
            WaitInDebuggingMode;
            FShouldStopAtBreakPoint := False;
          end;

          /////// debugging icons
          if Node^.PrevSibling <> nil then
          begin
            FClkActions[Node^.Index - 1].ActionDebuggingStatus := adsNone;
            vstActions.InvalidateNode(Node^.PrevSibling);
          end;
          FClkActions[Node^.Index].ActionDebuggingStatus := adsCurrent;
          vstActions.InvalidateNode(Node);
          /////// debugging icons

          if FStopAllActionsOnDemandFromParent <> nil then
            if FStopAllActionsOnDemandFromParent^ then
            begin
              FStopAllActionsOnDemand := True;
              Exit;
            end;

          if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) and (GetAsyncKeyState(VK_F2) < 0) then
          begin
            if FStopAllActionsOnDemandFromParent <> nil then
              FStopAllActionsOnDemandFromParent^ := True;

            FStopAllActionsOnDemand := True;
            Exit;
          end;

          spdbtnContinuePlayingAll.Enabled := False;
          spdbtnStepOver.Enabled := False;
          spdbtnStepInto.Enabled := False; //disable button while executing
          try
            if Node^.CheckState = csCheckedNormal then
              if not PlayActionByNode(Node) then               //    Execution happens here
              begin
                //MessageBox(Handle, PChar('Action[' + IntToStr(Node^.Index) + '] failed...' + #13#10 + 'Err=' + GetActionVarValue('$ExecAction_Err$') + #13#10 + 'Caller=' + FCallerName + #13#10 + 'LoadedFile: '+ FFileName), PChar(Caption), MB_ICONERROR);

                if IsDebugging then
                begin
                  spdbtnContinuePlayingAll.Enabled := True;
                  spdbtnStepOver.Enabled := True;

                  if not FContinuePlayingAll then   //pause execution if debugging
                    WaitInDebuggingMode;
                end;

                if FClkActions[Node^.Index].ActionStatus = asFailed then  //the status can be manually reset while debugging
                  Exit;
              end
              else
                if GetActionVarValue('$ExitCode$') = '0' then
                begin
                  if IsDebugging then
                  begin
                    spdbtnContinuePlayingAll.Enabled := True;
                    spdbtnStepOver.Enabled := True;

                    if not FContinuePlayingAll then   //pause execution if debugging
                      WaitInDebuggingMode;
                  end;

                  Exit;   //exit template
                end;
          finally
            //restore button states
            if IsDebugging then
            begin
              spdbtnContinuePlayingAll.Enabled := True;
              spdbtnStepOver.Enabled := True;
              //spdbtnContinuePlayingInto does not have to be enabled
            end;
          end;

          if FClkActions[Node^.Index].ActionOptions.Action = acCallTemplate then
          begin
            FContinuePlayingBySteppingInto := False; //reset flag for next execution
            //spdbtnContinuePlayingInto.Enabled := False;
          end;

          /////// debugging icons
          FClkActions[Node^.Index].ActionDebuggingStatus := adsPrev;
          vstActions.InvalidateNode(Node);
          /////// debugging icons

          OldNode := Node;
          Node := Node^.NextSibling;
          vstActions.RepaintNode(Node);
          Application.ProcessMessages;
        until (OldNode = LastNode) or not FPlaying;
      finally
        spdbtnContinuePlayingAll.Enabled := False;
        spdbtnStepOver.Enabled := False;
        spdbtnStepInto.Enabled := False;
      end;

    finally
      if (FStackLevel > 0) and FExecutesRemotely then
      begin
        try
          ClosingTemplateResponse := ExitRemoteTemplate(FRemoteAddress, FStackLevel);
          AddToLog(DateTimeToStr(Now) + '  Sent request to close remote template at index ' + IntToStr(FStackLevel) +  '  ' + ClosingTemplateResponse);
        except
          on E: Exception do
            AddToLog(DateTimeToStr(Now) + '  Error closing remote template at index ' + IntToStr(FStackLevel) + '  ' + E.Message);
        end;
      end;
    end;

    Result := True;
  finally
    FPlayingAllActions := False;
  end;
end;


function TfrClickerActionsArr.PlayAllActions(CallerName: string; ListOfVariables: TStrings; IsDebugging: Boolean; OverridenValues: TStrings = nil): Boolean;
var
  i: Integer;
  Key: string;
begin
  //FCallerName := CallerName;
  FDebugging := IsDebugging;
  
  SetVariables(ListOfVariables);

  if OverridenValues <> nil then
  begin
    for i := 0 to OverridenValues.Count - 1 do
    begin
      Key := Copy(OverridenValues.Strings[i], 1, Pos('=', OverridenValues.Strings[i]) - 1);
      frClickerActions.vallstVariables.Values[Key] := Copy(OverridenValues.Strings[i], Pos('=', OverridenValues.Strings[i]) + 1, MaxInt);
    end;
  end;

  FPlaying := True;
  FStopAllActionsOnDemand := False;
  try
    Result := PlayAllActions(IsDebugging);
    ListOfVariables.Text := frClickerActions.vallstVariables.Strings.Text;  //pass all variables on to next template
  finally
    FPlaying := False;
    //FStopAllActionsOnDemand := False;     //not sure if needed
  end;
end;


procedure TfrClickerActionsArr.SetVariables(ListOfVariables: TStrings);
begin
  frClickerActions.vallstVariables.Strings.Clear;
  frClickerActions.vallstVariables.Strings.AddStrings(ListOfVariables);  
end;


procedure TfrClickerActionsArr.LoadTemplate_V1(Ini: TClkIniReadonlyFile);
begin
  LoadTemplateToCustomActions_V1(Ini, FClkActions);
end;


procedure TfrClickerActionsArr.LoadTemplate_V2(Ini: TClkIniReadonlyFile);
begin
  LoadTemplateToCustomActions_V2(Ini, FClkActions, FTemplateNotes);
end;


function TfrClickerActionsArr.CreateIniReadonlyFileFromInMemFileSystem(AFnm: string; AInMemFileSystem: TInMemFileSystem): TClkIniReadonlyFile;
var
  IniContentMemStream: TMemoryStream;
begin
  IniContentMemStream := TMemoryStream.Create;
  try
    AInMemFileSystem.LoadFileFromMemToStream(AFnm, IniContentMemStream);
    IniContentMemStream.Position := 0;
    Result := TClkIniReadonlyFile.Create(IniContentMemStream);
  finally
    IniContentMemStream.Free;
  end;
end;


procedure TfrClickerActionsArr.LoadTemplate(Fnm: string; AFileLocation: TFileLocation = flDisk; AInMemFileSystem: TInMemFileSystem = nil);
var
  Ini: TClkIniReadonlyFile;
  FormatVersion: string;
  ActionCount: Integer;
  ErrMsg: string;
begin
  vstActions.Clear; //to reset the node checkboxes

  Fnm := StringReplace(Fnm, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);

  try
    case AFileLocation of      ///////////////////// ToDo:   refactoring !!!!!!!!!!!!!!
      flDisk:
      begin
        if not DoOnFileExists(Fnm) then
        begin
          ErrMsg := 'Template file not found on executing CallTemplateAction: "' + Fnm + '"...';
          AddToLog(ErrMsg);
          AppendErrorMessageToActionVar(ErrMsg);
          //memLogErr.Repaint;
          //MessageBox(Handle, PChar(ErrMsg), 'Arr', MB_ICONERROR);
          Exit;
        end;

        Ini := DoOnTClkIniReadonlyFileCreate(Fnm);
      end;

      flMem:
      begin
        if not AInMemFileSystem.FileExistsInMem(Fnm) then
        begin
          AddToLog('Waiting for file availability: ' + Fnm);
          DoWaitForFileAvailability(Fnm);
        end;

        Ini := CreateIniReadonlyFileFromInMemFileSystem(Fnm, AInMemFileSystem);
      end;

      flDiskThenMem:
        if DoOnFileExists(Fnm) then
          Ini := DoOnTClkIniReadonlyFileCreate(Fnm)
        else
        begin
          if not AInMemFileSystem.FileExistsInMem(Fnm) then
          begin
            AddToLog('Waiting for file availability: ' + Fnm);
            DoWaitForFileAvailability(Fnm);
          end;

          Ini := CreateIniReadonlyFileFromInMemFileSystem(Fnm, AInMemFileSystem);
        end;

      flMemThenDisk:
      begin
        if not AInMemFileSystem.FileExistsInMem(Fnm) then
        begin
          AddToLog('Waiting for file availability: ' + Fnm);
          DoWaitForFileAvailability(Fnm);
        end;

        if AInMemFileSystem.FileExistsInMem(Fnm) then
          Ini := CreateIniReadonlyFileFromInMemFileSystem(Fnm, AInMemFileSystem)
        else
        begin
          if not DoOnFileExists(Fnm) then
          begin
            AppendErrorMessageToActionVar('Template file not found on executing CallTemplateAction: "' + Fnm + '"...');
            Exit;
          end;

          Ini := DoOnTClkIniReadonlyFileCreate(Fnm);
        end;
      end;
    end;
    try
      SetAllActionsToNotStarted;
      ResetDebuggingStatusOnAllActions;

      ActionCount := Ini.ReadInteger('Actions', 'Count', 0);
      SetLength(FClkActions, ActionCount);
      AddToLog('Loading template: "' + Fnm + '", with ' + IntToStr(Length(FClkActions)) + ' action(s)..');

      FormatVersion := Ini.ReadString('Actions', 'Version', '1');

      if FormatVersion = '1' then
        LoadTemplate_V1(Ini)
      else
        if FormatVersion = '2' then
          LoadTemplate_V2(Ini)
        else
          raise Exception.Create('Unhandled format version: ' + FormatVersion);
    finally
      Ini.Free;
    end;
  except
    on E: Exception do
    begin
      if not FExecutingActionFromRemote then
        MessageBox(Handle, PChar('Exception when loading template: ' + E.Message + #13#10 + 'File location: ' + IntToStr(Ord(AFileLocation))), PChar(Caption), MB_ICONINFORMATION)
      else
        raise Exception.Create('Exception when loading template: ' + E.Message + #13#10 + 'File location: ' + IntToStr(Ord(AFileLocation)));
    end;
  end;

  Modified := True; //trigger a label update
  Modified := False;
  vstActions.RootNodeCount := Length(FClkActions);
  vstActions.Repaint;

  UpdateModifiedLabel;  //required for first loading, when Modified is still False
  StopGlowingUpdateButton; //required here, because LoadTemplate can be called from parent of frame
end;


procedure TfrClickerActionsArr.SaveTemplateWithCustomActions_V2(Fnm: string; var ACustomClkActions: TClkActionsRecArr; ANotes: string);
var
  AStringList: TStringList;   //much faster than T(Mem)IniFile
begin
  AStringList := TStringList.Create;
  try
    SaveTemplateWithCustomActionsToStringList_V2(AStringList, ACustomClkActions, ANotes);
    DoOnSaveTemplateToFile(AStringList, Fnm);
  finally
    AStringList.Free;
  end;

  Modified := False;
  StopGlowingUpdateButton;
end;


procedure TfrClickerActionsArr.SaveTemplate(Fnm: string);
begin
  //SaveTemplateWithCustomActions(Fnm, FClkActions);
  SaveTemplateWithCustomActions_V2(Fnm, FClkActions, FTemplateNotes);
end;


procedure TfrClickerActionsArr.SaveTemplateAs1Click(Sender: TObject);
begin
  SaveTemplateWithDialog;
end;


procedure TfrClickerActionsArr.vstActionsChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  FClkActions[Node^.Index].ActionOptions.ActionEnabled := Node^.CheckState = csCheckedNormal;
  Modified := True;
end;


procedure TfrClickerActionsArr.vstActionsDblClick(Sender: TObject);
begin
  if FActionsHitInfo.HitColumn = 0 then
    tmrEditActionsVST.Enabled := True;
end;


procedure TfrClickerActionsArr.vstActionsEditCancelled(Sender: TBaseVirtualTree; Column: TColumnIndex);
begin

end;


procedure TfrClickerActionsArr.vstActionsEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  FClkActions[Node^.Index].ActionOptions.ActionName := FEditingText;
  HandleActionSelection; // frClickerActions.OIFrame.;
  Modified := True;
  frClickerActions.RefreshActionName;
end;


procedure TfrClickerActionsArr.vstActionsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Column = 0;
  FEditingText := FClkActions[Node^.Index].ActionOptions.ActionName;
end;


procedure TfrClickerActionsArr.vstActionsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
begin
  FEditingText := NewText;
end;


procedure TfrClickerActionsArr.tmrEditActionsVSTTimer(Sender: TObject);
var
  Node: PVirtualNode;
begin
  tmrEditActionsVST.Enabled := False;

  Node := vstActions.GetFirstSelected;
  if Node = nil then
    Exit;

  vstActions.EditNode(Node, 0);
end;


procedure TfrClickerActionsArr.vstActionsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  //dummy handler, to allow proper execution of vstActionsGetImageIndexEx
end;


procedure TfrClickerActionsArr.vstActionsGetImageIndexEx(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
  var ImageList: TCustomImageList);
begin
  case Column of
    0:
    begin
      if not FClkActions[Node^.Index].ActionBreakPoint.Exists then
        ImageList := imglstCurrentDebuggingAction
      else
      begin
        if FClkActions[Node^.Index].ActionBreakPoint.Enabled then
          ImageList := imglstCurrentDebuggingActionWithBreakPoint
        else
          ImageList := imglstCurrentDebuggingActionWithDisabledBreakPoint;
      end;

      //arrows
      //0:   |
      //     |\

      //1:   |\
      //     |/

      //2:   |/
      //     |

      case FClkActions[Node^.Index].ActionDebuggingStatus of
        adsNone:    ImageIndex := 0;   //no arrow
        adsPrev:    ImageIndex := 1;   //lower left arrow
        adsCurrent: ImageIndex := 2;   //full arrow
        adsNext:    ImageIndex := 3;   //upper left arrow
      end;
    end;

    1:
    begin
      ImageList := frClickerActions.imglstActions;
      ImageIndex := Ord(FClkActions[Node^.Index].ActionOptions.Action);
    end;

    2:
    begin
      if FClkActions[Node^.Index].ActionSkipped then
      begin
        ImageList := imglstActionExtraStatus;
        ImageIndex := Ord(FClkActions[Node^.Index].ActionStatus);
      end
      else
      begin
        ImageList := imglstActionStatus;
        ImageIndex := Ord(FClkActions[Node^.Index].ActionStatus);
      end;
    end;

    3:
    begin
      ImageList := imglstActionHasCondition;
      ImageIndex := Ord(FClkActions[Node^.Index].ActionOptions.ActionCondition > '');
    end;
  end;
end;


function GetVSTMiscDisplayedInfoByAction(AActionRec: TClkActionRec): string;
begin
  case AActionRec.ActionOptions.Action of
    acClick: Result := CXClickPointReference[AActionRec.ClickOptions.XClickPointReference] + '+' + AActionRec.ClickOptions.XOffset + ' : ' + CYClickPointReference[AActionRec.ClickOptions.YClickPointReference] + '+' + AActionRec.ClickOptions.YOffset + '  Count=' + IntToStr(AActionRec.ClickOptions.Count);
    acExecApp: Result := '"' + AActionRec.ExecAppOptions.PathToApp + '"' + FastReplace_ReturnTo45(AActionRec.ExecAppOptions.ListOfParams);
    acFindControl: Result := 'Match: ' + MatchCriteriaToString(AActionRec.FindControlOptions.MatchCriteria) + '   Text="' + AActionRec.FindControlOptions.MatchText + '"  Class="' + AActionRec.FindControlOptions.MatchClassName + '"';
    acFindSubControl: Result := 'Match: ' + MatchCriteriaToString(AActionRec.FindControlOptions.MatchCriteria);
    acSetControlText: Result := AActionRec.SetTextOptions.Text;
    acCallTemplate: Result := ExtractFileName(AActionRec.CallTemplateOptions.TemplateFileName);
    acSleep: Result := AActionRec.SleepOptions.Value;
    acSetVar: Result := FastReplace_ReturnTo45(AActionRec.SetVarOptions.ListOfVarNames);
  else
    Result := 'not implemented';
  end;
end;


procedure TfrClickerActionsArr.vstActionsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: {$IFDEF FPC} string {$ELSE} WideString {$ENDIF});
begin
  try
    case Column of
      0: CellText := FClkActions[Node^.Index].ActionOptions.ActionName;
      1:
      begin
        if FClkActions[Node^.Index].ActionOptions.Action <= High(TClkAction) then
          CellText := CClkActionStr[FClkActions[Node^.Index].ActionOptions.Action]
        else
          CellText := '[out of range]';  //bug
      end;

      2: CellText := IntToStr(FClkActions[Node^.Index].ActionOptions.ActionTimeout);
      3: CellText := GetVSTMiscDisplayedInfoByAction(FClkActions[Node^.Index]);
      4:
      begin
        case FClkActions[Node^.Index].ActionOptions.Action of
          acClick: CellText := IntToStr(FClkActions[Node^.Index].ClickOptions.ClickType);
          acExecApp: CellText := ExtractFileName(FClkActions[Node^.Index].ExecAppOptions.PathToApp);
          acFindControl, acFindSubControl: CellText := FClkActions[Node^.Index].FindControlOptions.MatchText + ' / ' + FClkActions[Node^.Index].FindControlOptions.MatchClassName;
          acSetControlText: CellText := FClkActions[Node^.Index].SetTextOptions.Text;
          acCallTemplate: CellText := ExtractFileName(FClkActions[Node^.Index].CallTemplateOptions.TemplateFileName);
          acSleep: CellText := FClkActions[Node^.Index].SleepOptions.Value;
          acSetVar: CellText := FastReplace_ReturnTo45(FClkActions[Node^.Index].SetVarOptions.ListOfVarNames);
          acWindowOperations: CellText := IntToStr(Ord(FClkActions[Node^.Index].WindowOperationsOptions.Operation));
        end;
      end;
      5: CellText := StringReplace(FClkActions[Node^.Index].FindControlOptions.MatchBitmapFiles, #13#10, ', ', [rfReplaceAll]);
      6: CellText := IntToStr(Node^.Index);
    end;
  except
    CellText := 'bug';
  end;
end;


procedure TfrClickerActionsArr.UpdateNodeCheckStateFromAction(Node: PVirtualNode);
const
  CCheckStates: array[Boolean] of TCheckState = (csUnCheckedNormal, csCheckedNormal);
begin
  Node.CheckState := CCheckStates[FClkActions[Node^.Index].ActionOptions.ActionEnabled];
end;


procedure TfrClickerActionsArr.vstActionsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  try
    Node.CheckType := ctCheckBox;
    UpdateNodeCheckStateFromAction(Node);
  except
    Node.CheckState := csMixedNormal;
  end;
end;


procedure TfrClickerActionsArr.spdbtnMoveUpClick(Sender: TObject);
var
  Node: PVirtualNode;
  Ph: TClkActionRec;
begin
  Node := vstActions.GetFirstSelected;
  if (Node = nil) or (Node = vstActions.GetFirst) or (vstActions.RootNodeCount = 1) then
    Exit;

  Ph := FClkActions[Node^.Index - 1];
  FClkActions[Node^.Index - 1] := FClkActions[Node^.Index];      //to be replaced with a copy function
  FClkActions[Node^.Index] := Ph;

  UpdateNodeCheckStateFromAction(Node^.PrevSibling);
  UpdateNodeCheckStateFromAction(Node);

  vstActions.ClearSelection;
  vstActions.Selected[Node^.PrevSibling] := True;
  vstActions.ScrollIntoView(Node, True);
  vstActions.Repaint;
  Modified := True;
end;


procedure TfrClickerActionsArr.spdbtnMoveDownClick(Sender: TObject);
var
  Node: PVirtualNode;
  Ph: TClkActionRec;
begin
  Node := vstActions.GetFirstSelected;
  if (Node = nil) or (Node = vstActions.GetLast) or (vstActions.RootNodeCount = 1) then
    Exit;

  Ph := FClkActions[Node^.Index + 1];
  FClkActions[Node^.Index + 1] := FClkActions[Node^.Index];     //to be replaced with a copy function
  FClkActions[Node^.Index] := Ph;

  UpdateNodeCheckStateFromAction(Node^.NextSibling);
  UpdateNodeCheckStateFromAction(Node);

  vstActions.ClearSelection;
  vstActions.Selected[Node^.NextSibling] := True;
  vstActions.ScrollIntoView(Node, True);
  vstActions.Repaint;
  Modified := True;
end;


procedure TfrClickerActionsArr.spdbtnPlayAllActionsClick(Sender: TObject);
begin
  PrepareFilesInServer;
  PlayAllActionsFromButton(False);
end;


procedure TfrClickerActionsArr.spdbtnPlaySelectedActionClick(Sender: TObject);
begin
  if not FPlaying then
    FDebugging := False;

  PrepareFilesInServer;
  PlaySelectedActionFromButton;
end;


procedure TfrClickerActionsArr.spdbtnStopPlayingClick(Sender: TObject);
begin
  StopAllActionsFromButton;
end;


procedure TfrClickerActionsArr.spdbtnContinuePlayingAllClick(Sender: TObject);
begin
  FContinuePlayingAll := True;
end;


procedure TfrClickerActionsArr.spdbtnStepOverClick(Sender: TObject);
begin
  FContinuePlayingNext := True;
end;


procedure TfrClickerActionsArr.spdbtnStepIntoClick(Sender: TObject);
begin
  FContinuePlayingBySteppingInto := True;
end;


procedure TfrClickerActionsArr.spdbtnExtraAddClick(Sender: TObject);
var
  tp: TPoint;
begin
  GetCursorPos(tp);
  pmExtraAdd.Popup(tp.X, tp.Y);
end;


procedure TfrClickerActionsArr.spdbtnExtraPlayAllClick(Sender: TObject);
var
  tp: TPoint;
begin
  GetCursorPos(tp);
  pmExtraPlayAll.Popup(tp.X, tp.Y);
end;


procedure TfrClickerActionsArr.spdbtnExtraRemoveClick(Sender: TObject);
var
  tp: TPoint;
begin
  GetCursorPos(tp);
  pmExtraRemove.Popup(tp.X, tp.Y);
end;


procedure TfrClickerActionsArr.spdbtnExtraSaveClick(Sender: TObject);
var
  tp: TPoint;
begin
  GetCursorPos(tp);
  pmExtraSave.Popup(tp.X, tp.Y);
end;


procedure TfrClickerActionsArr.SetAFontFromClkActions(AFont: TFont; ActionIndex: Integer);
begin
  if Length(FClkActions[ActionIndex].FindControlOptions.MatchBitmapText) = 0 then
  begin
    AFont.Name := 'Tahoma';
    AFont.Size := 8;
    AFont.Style := [];
    Exit;
  end;

  AFont.Name := EvaluateReplacements(FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[0].FontName);
  AFont.Size := FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[0].FontSize;
  AFont.Style := [];

  if FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[0].Bold then
    AFont.Style := AFont.Style + [fsBold];

  if FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[0].Italic then
    AFont.Style := AFont.Style + [fsItalic];

  if FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[0].Underline then
    AFont.Style := AFont.Style + [fsUnderline];

  if FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[0].StrikeOut then
    AFont.Style := AFont.Style + [fsStrikeOut];

  AFont.Color := HexToInt(EvaluateReplacements(FClkActions[ActionIndex].FindControlOptions.MatchBitmapText[0].ForegroundColor));
end;


procedure TfrClickerActionsArr.vstActionsPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  try
    case Column of
      4:
      begin
        if ((FClkActions[Node^.Index].ActionOptions.Action = acFindControl) or (FClkActions[Node^.Index].ActionOptions.Action = acFindSubControl)) then
        begin
          if (not FClkActions[Node^.Index].FindControlOptions.MatchCriteria.WillMatchBitmapText and not FClkActions[Node^.Index].FindControlOptions.MatchCriteria.WillMatchBitmapFiles) then
          begin
            if vstActions.Selected[Node] then
            begin
              if vstActions.Focused then
                TargetCanvas.Font.Color := clWindow
              else
                TargetCanvas.Font.Color := clWindowText;
            end
            else
              TargetCanvas.Font.Color := clWindowText;
          end
          else
            SetAFontFromClkActions(TargetCanvas.Font, Node^.Index);
        end //control or subcontrol
        else
          begin
            {if vstActions.Selected[Node] then
            begin
              if vstActions.Focused then
                TargetCanvas.Font.Color := clWindow
              else
                TargetCanvas.Font.Color := clWindowText;
            end
            else
              TargetCanvas.Font.Color := clWindowText; }
          end
      end;
    end;
  except
  end;
end;


procedure TfrClickerActionsArr.vstActionsBeforeCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
begin
  case Column of
    4:
    begin
      try
        if ((FClkActions[Node^.Index].ActionOptions.Action = acFindControl) or (FClkActions[Node^.Index].ActionOptions.Action = acFindSubControl)) and
         (FClkActions[Node^.Index].FindControlOptions.MatchCriteria.WillMatchBitmapText or FClkActions[Node^.Index].FindControlOptions.MatchCriteria.WillMatchBitmapFiles) then
        TargetCanvas.Brush.Color := HexToInt(EvaluateReplacements(FClkActions[Node^.Index].FindControlOptions.MatchBitmapText[0].BackgroundColor))
        ;
      except
        TargetCanvas.Brush.Color := clRed;
      end;

      TargetCanvas.Rectangle(CellRect);
    end;
  end;
end;


procedure TfrClickerActionsArr.ToggleBreakpoint;
begin
  FClkActions[FActionsHitInfo.HitNode^.Index].ActionBreakPoint.Exists := not FClkActions[FActionsHitInfo.HitNode^.Index].ActionBreakPoint.Exists;
  if FClkActions[FActionsHitInfo.HitNode^.Index].ActionBreakPoint.Exists then
    FClkActions[FActionsHitInfo.HitNode^.Index].ActionBreakPoint.Enabled := True;

  vstActions.InvalidateNode(FActionsHitInfo.HitNode);
  Modified := True;
end;


procedure TfrClickerActionsArr.vstActionsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  CEnableDisableText: array[Boolean] of string = ('Disable', 'Enable');
var
  tp: TPoint;
  ColumnOffSet: Integer;
begin
  FActionsHitTimeStamp := GetTickCount64; //required on MouseUp
  FPreviousSelectedNode := vstActions.GetFirstSelected;

  vstActions.GetHitTestInfoAt(X, Y, True, FActionsHitInfo);

  ColumnOffSet := vstActions.Header.Columns.Items[6].Width * Ord(coVisible in vstActions.Header.Columns.Items[6].Options);
                                                                             //these constants should be replaced with vst.Indent (or TextSpacing) + imgLst.Width + NodeCheckBox spacing etc
  if (FActionsHitInfo.HitColumn = 0) and (FActionsHitInfo.HitNode <> nil) and (X > 24 + ColumnOffSet) and (X < 50 + ColumnOffSet) then
  begin
    case Button of   //click on breakpoint
      mbLeft:
        ToggleBreakpoint;

      mbRight:
      begin
        MenuItemEnableDisableBreakPoint.Caption := CEnableDisableText[not FClkActions[FActionsHitInfo.HitNode^.Index].ActionBreakPoint.Enabled] + ' breakpoint';
        MenuItemEnableDisableBreakPoint.Enabled := FClkActions[FActionsHitInfo.HitNode^.Index].ActionBreakPoint.Exists;
        MenuItem_EditBreakPoint.Enabled := FClkActions[FActionsHitInfo.HitNode^.Index].ActionBreakPoint.Exists;

        GetCursorPos(tp);
        pmBreakPoint.PopUp(tp.X, tp.Y);
      end;

      else
       ;
    end;
  end
  else
    if Button = mbRight then
    begin
      GetCursorPos(tp);
      pmVstActions.PopUp(tp.X, tp.Y);
    end;
end;


procedure TfrClickerActionsArr.HandleActionSelection;
var
  Node: PVirtualNode;
begin
  Node := vstActions.GetFirstSelected;

  if Node = nil then
    Exit;

  if FActionsHitInfo.HitNode = nil then
    Exit;

  if frClickerActions.ControlsModified then
    if MessageBox(Handle, PChar('There are changed properties for the selected action. By selecting another action, you will discard those changes. You can click Update to record the changes to the action list.'#13#10#13#10'Go back to previous action/content?'), '', MB_ICONWARNING + MB_YESNO) = IDYES then
    begin
      vstActions.ClearSelection;
      vstActions.Selected[FPreviousSelectedNode] := True;
      Exit;
    end;

  if frClickerActions.ModifiedPmtvFiles then
    if MessageBox(Handle, PChar('One or more primitives files are modified for the selected action. By selecting another action, you will discard those changes.'#13#10#13#10'Go back to previous action/content?'), '', MB_ICONWARNING + MB_YESNO) = IDYES then
    begin
      vstActions.ClearSelection;
      vstActions.Selected[FPreviousSelectedNode] := True;
      Exit;
    end
    else
      frClickerActions.ResetAllPmtvModifiedFlags;  //reset the flags, so that next time this action is selected, the files won't appear as modified

  UpdateControlsFromActionsArr(Node^.Index);
  StopGlowingUpdateButton;
  frClickerActions.UpdatePageControlActionExecutionIcons;
end;


procedure TfrClickerActionsArr.vstActionsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if GetTickCount64 - FActionsHitTimeStamp < 500 then  //This check is required, when double-clicking a file, in an OpenDialog, over vstActions.
    HandleActionSelection;                             //The dialog closes at the second MouseDown event of the double-click action, while the second MouseUp triggers this (vstActionsMouseUp) handler.
end;


procedure TfrClickerActionsArr.vstActionsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ssCtrl in Shift then
  begin
    case Key of
      Ord('C'): CopySelectedActionsToClipboard;
      Ord('V'): PasteActionsFromClipboard;
      else
    end;
  end;
end;


procedure TfrClickerActionsArr.vstActionsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key in [VK_UP, VK_DOWN, VK_NEXT, VK_PRIOR, VK_HOME, VK_END] then
    HandleActionSelection;

  if Key = VK_DELETE then
    tmrDeleteActions.Enabled := True; //using timer, because of a race condition
end;


function TfrClickerActionsArr.ValidActionToBeAdded: Boolean;
var
  CurrentAction: TClkAction;
begin
  Result := False;

  if Ord(frClickerActions.CurrentlyEditingActionType) = CClkUnsetAction then
  begin
    MessageBox(Handle, 'Please specify an action type.', PChar(Application.Title), MB_ICONINFORMATION);
    Exit;
  end;

  CurrentAction := frClickerActions.CurrentlyEditingActionType;
  case CurrentAction of
    acClick:
    begin

    end;

    acFindControl:
      if not (frClickerActions.EditingAction^.FindControlOptions.MatchCriteria.WillMatchText or
              frClickerActions.EditingAction^.FindControlOptions.MatchCriteria.WillMatchClassName or
              frClickerActions.EditingAction^.FindControlOptions.MatchCriteria.WillMatchBitmapText or
              frClickerActions.EditingAction^.FindControlOptions.MatchCriteria.WillMatchBitmapFiles) then
      begin
        MessageBox(Handle, 'To find a control, at least one match criterion has to be checked.', PChar(Application.Title), MB_ICONINFORMATION);
        Exit;
      end;

    acSetControlText:
      ;

    acExecApp:
      ;

    else
    begin
    end;
  end;  //case

  Result := True;
end;  


procedure TfrClickerActionsArr.btnAddActionClick(Sender: TObject);
var
  n: Integer;
  Node: PVirtualNode;
begin
  if not ValidActionToBeAdded then
    Exit;

  n := Length(FClkActions);
  SetLength(FClkActions, n + 1);

  UpdateActionsArrFromControls(n);
  FClkActions[n].ActionStatus := asNotStarted;
  FClkActions[n].ActionOptions.ActionEnabled := True;

  vstActions.RootNodeCount := 0;  //to reinit nodes
  vstActions.RootNodeCount := Length(FClkActions);
  vstActions.Repaint;
  Modified := True;

  Node := vstActions.GetLast;
  vstActions.Selected[Node] := True;
  vstActions.ScrollIntoView(Node, True);
  StopGlowingUpdateButton;
  frClickerActions.UpdatePageControlActionExecutionIcons;
end;


procedure TfrClickerActionsArr.btnNewClick(Sender: TObject);
begin
  if MessageBox(Handle, 'Are you sure you want to remove all actions from list and clear all controls?', PChar(Caption), MB_ICONQUESTION + MB_YESNO) = ID_YES then
  begin
    ClearAllActions;
    frClickerActions.ClearControls;
    FTemplateNotes := '';
  end;
end;


procedure TfrClickerActionsArr.edtConsoleCommandExit(Sender: TObject);
begin
  if Assigned(frmAutoComplete) and not frmAutoComplete.Focused then
    CloseAutoComplete;
end;


procedure TfrClickerActionsArr.DisplayDefaultEvalConsoleEditBox;
begin
  AddToLog('>> ' + edtConsoleCommand.Text);
  AddToLog('<- ' + EvaluateReplacements(edtConsoleCommand.Text));
end;


function CharCount(AChar: Char; s: string; AMaxIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Min(AMaxIndex, Length(s)) do
    if s[i] = AChar then
      Inc(Result);
end;


function TfrClickerActionsArr.EvaluateAssignmentExpression: Boolean;
var
  PosEq, PosParanth, PosCrop: Integer;
  PosVar, PosEndVar: Integer;
  s: string;
  LeftSide, RightSide, Value: string;
begin
  Result := False;

  s := edtConsoleCommand.Text;
  PosEq := Pos('=', s);
  PosParanth := Pos('(', s);
  PosCrop := PosEq;

  if PosParanth < PosEq then
  begin
    if CharCount('$', s, PosParanth) = 1 then     //e.g.:  $func(value=337770)$
      PosCrop := PosParanth;
  end
  else
    if PosParanth > PosEq then
      if CharCount('$', s, PosEq) > 1 then        //e.g.   $var$=$func(abc)$
        PosCrop := PosEq;

  LeftSide := Trim(Copy(s, 1, PosCrop - 1));

  PosVar := Pos('$', LeftSide);
  //PosEndVar := Pos('$', LeftSide, PosVar + 1);
  PosEndVar := RevPos('$', LeftSide, PosVar + 1);

  if (PosVar = 0) or (PosEndVar = 0) then
  begin
    DisplayDefaultEvalConsoleEditBox;
    Exit;
  end;

  RightSide := Trim(Copy(s, PosCrop + 1, MaxInt));

  if (Length(RightSide) > 2) and (RightSide[1] = '"') and (RightSide[Length(RightSide)] = '"') then
    Value := Copy(RightSide, 2, Length(RightSide) - 2)
  else
    Value := EvaluateReplacements(RightSide);

  SetActionVarValue(LeftSide, Value);

  AddToLog('>> ' + edtConsoleCommand.Text);
  AddToLog('<- ' + Value);

  Result := True;
end;


procedure TfrClickerActionsArr.edtConsoleCommandKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  tp: TPoint;
begin
  case Key of
    VK_RETURN:
    begin
      if Trim(edtConsoleCommand.Text) = '' then
        Exit;

      if not ((Pos('=', edtConsoleCommand.Text) > 0) and EvaluateAssignmentExpression) then
        DisplayDefaultEvalConsoleEditBox;

      FCmdConsoleHistory.Add(edtConsoleCommand.Text);
      edtConsoleCommand.Text := '';
      edtConsoleCommand.Tag := FCmdConsoleHistory.Count; //without -1, to allow decrementing when pressing VK_UP;
    end;

    VK_UP:
    begin
      if AutoCompleteVisible then
      begin
        frmAutoComplete.SetFocus;
        Exit;
      end;

      edtConsoleCommand.Tag := edtConsoleCommand.Tag - 1;

      if edtConsoleCommand.Tag < 0 then
        edtConsoleCommand.Tag := 0;

      if edtConsoleCommand.Tag > FCmdConsoleHistory.Count - 1 then
        edtConsoleCommand.Text := ''
      else
        edtConsoleCommand.Text := FCmdConsoleHistory.Strings[edtConsoleCommand.Tag];

      tp := edtConsoleCommand.CaretPos;
      tp.X := edtConsoleCommand.ClientRect.Right;
      edtConsoleCommand.CaretPos := tp;

      Key := 0;
    end;

    VK_DOWN:
    begin
      if AutoCompleteVisible then
      begin
        frmAutoComplete.SetFocus;
        Exit;
      end;

      edtConsoleCommand.Tag := edtConsoleCommand.Tag + 1;
      if edtConsoleCommand.Tag > FCmdConsoleHistory.Count then  //without - 1
        edtConsoleCommand.Tag := FCmdConsoleHistory.Count;      //this will make Tag, at most equal to FCmdConsoleHistory.Count (outside of list), to clear the editbox

      if edtConsoleCommand.Tag > FCmdConsoleHistory.Count - 1 then
        edtConsoleCommand.Text := ''
      else
        edtConsoleCommand.Text := FCmdConsoleHistory.Strings[edtConsoleCommand.Tag];

      Key := 0;
    end;
  end; //case

  if Key = VK_SPACE then
    if ssCtrl in Shift then
    begin
      Key := 0;
      Exit;
    end;

  if Key in [VK_RETURN, VK_ESCAPE] then
    if AutoCompleteVisible then
      CloseAutoComplete;
end;


procedure TfrClickerActionsArr.edtConsoleCommandKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  TempVars: TStringList;
  TempFuncs: TStringList;
begin
  if Key = VK_SPACE then
    if ssCtrl in Shift then
    begin
      Key := 0;
      FillInVarAndFuncDescriptions;

      TempVars := TStringList.Create;
      TempFuncs := TStringList.Create;
      try
        FillInWithAllVars(TempVars);
        FillInWithAllFuncs(TempFuncs);
        ShowAutoComplete(edtConsoleCommand, TempVars, TempFuncs, FVarDescriptions, FFuncDescriptions);
      finally
        TempVars.Free;
        TempFuncs.Free;
      end;
    end;

  if AutoCompleteVisible then
    ShowAutoComplete(edtConsoleCommand, nil, nil, FVarDescriptions, FFuncDescriptions);
end;


procedure TfrClickerActionsArr.MenuItemCopySelectedActionsToClipboardClick(
  Sender: TObject);
begin
  CopySelectedActionsToClipboard;
end;


procedure TfrClickerActionsArr.MenuItemPasteActionsFromClipboardClick(
  Sender: TObject);
begin
  PasteActionsFromClipboard;
end;


procedure TfrClickerActionsArr.MenuItem_AddACallTemplateByFileClick(
  Sender: TObject);
begin
  if not DoOnOpenDialogExecute(CTemplateDialogFilter) then
    Exit;

  InsertCallTemplateForActionReplacing(vstActions.RootNodeCount, DoOnGetOpenDialogFileName);

  vstActions.Repaint;

  vstActions.ClearSelection;
  Application.ProcessMessages;

  if not Modified and not frClickerActions.ControlsModified then  //do not reload content if modified
    LoadActionIntoEditorByIndex(vstActions.RootNodeCount - 1);

  Modified := True;
  StopGlowingUpdateButton;
end;


procedure TfrClickerActionsArr.MenuItem_AddCacheControlActionClick(
  Sender: TObject);
var
  Node: PVirtualNode;
  NewAction: TClkSetVarOptions;
  InsertIndex: Integer;
begin
  InsertIndex := -1;
  Node := vstActions.GetFirstSelected;
  if Node <> nil then
    InsertIndex := Node^.Index;

  NewAction.ListOfVarNames := FastReplace_45ToReturn('$My_Left$$My_Top$$My_Right$$My_Bottom$$My_Width$$My_Height$$My_Text$$My_Class$$My_Handle$');
  NewAction.ListOfVarValues := FastReplace_45ToReturn('$Control_Left$$Control_Top$$Control_Right$$Control_Bottom$$Control_Width$$Control_Height$$Control_Text$$Control_Class$$Control_Handle$');
  NewAction.ListOfVarEvalBefore := FastReplace_45ToReturn('111111111');

  InsertSetVar(InsertIndex + 1, 'Cache current control', NewAction);
  UpdateNodesCheckStateFromActions;
end;


procedure TfrClickerActionsArr.MenuItem_AddRestoreCachedControlActionClick(
  Sender: TObject);
var
  Node: PVirtualNode;
  NewAction: TClkSetVarOptions;
  InsertIndex: Integer;
begin
  InsertIndex := -1;
  Node := vstActions.GetFirstSelected;
  if Node <> nil then
    InsertIndex := Node^.Index;

  NewAction.ListOfVarNames := FastReplace_45ToReturn('$Control_Left$$Control_Top$$Control_Right$$Control_Bottom$$Control_Width$$Control_Height$$Control_Text$$Control_Class$$Control_Handle$');
  NewAction.ListOfVarValues := FastReplace_45ToReturn('$My_Left$$My_Top$$My_Right$$My_Bottom$$My_Width$$My_Height$$My_Text$$My_Class$$My_Handle$');
  NewAction.ListOfVarEvalBefore := FastReplace_45ToReturn('111111111');

  InsertSetVar(InsertIndex + 1, 'Restore cached control', NewAction);
  UpdateNodesCheckStateFromActions;
end;


procedure TfrClickerActionsArr.MenuItemEnableDisableBreakPointClick(
  Sender: TObject);
begin
  if FActionsHitInfo.HitNode = nil then
  begin
    MessageBox(Handle, 'Clicked on an invalid breakpoint space.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  FClkActions[FActionsHitInfo.HitNode^.Index].ActionBreakPoint.Enabled := not FClkActions[FActionsHitInfo.HitNode^.Index].ActionBreakPoint.Enabled;
  vstActions.InvalidateNode(FActionsHitInfo.HitNode);
  Modified := True;
end;


procedure TfrClickerActionsArr.MenuItem_EditBreakPointClick(Sender: TObject);
begin
  if FActionsHitInfo.HitNode = nil then
  begin
    MessageBox(Handle, 'Clicked on an invalid breakpoint space.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  if EditActionCondition(FClkActions[FActionsHitInfo.HitNode^.Index].ActionBreakPoint.Condition) then
    Modified := True;
end;


procedure TfrClickerActionsArr.MenuItem_PlayActionAndRestoreVarsClick(
  Sender: TObject);
var
  BackupList: TStringList;
begin
  BackupList := TStringList.Create;
  try
    BackupList.Text := frClickerActions.vallstVariables.Strings.Text;
    try
      PrepareFilesInServer;
      PlaySelected;
    finally
      frClickerActions.vallstVariables.Strings.Text := BackupList.Text;
    end;
  finally
    BackupList.Free;
  end;
end;


procedure TfrClickerActionsArr.InsertCallTemplateForActionReplacing(AIndexToInsertAt: Integer; ATemplateFileName: string);
var
  i, n: Integer;
begin
  n := Length(FClkActions);
  SetLength(FClkActions, n + 1);

  for i := n downto AIndexToInsertAt + 1 do
    FClkActions[i] := FClkActions[i - 1];

  FClkActions[AIndexToInsertAt].ActionDebuggingStatus := adsNone;
  FClkActions[AIndexToInsertAt].ActionOptions.Action := acCallTemplate;
  FClkActions[AIndexToInsertAt].ActionOptions.ActionCondition := '';
  FClkActions[AIndexToInsertAt].ActionOptions.ActionEnabled := True;
  FClkActions[AIndexToInsertAt].ActionOptions.ActionName := 'Call ' + ExtractFileName(ATemplateFileName);
  FClkActions[AIndexToInsertAt].ActionOptions.ActionTimeout := 1000;
  FClkActions[AIndexToInsertAt].CallTemplateOptions.CallOnlyIfCondition := False;
  FClkActions[AIndexToInsertAt].CallTemplateOptions.CallOnlyIfConditionVarName := '';
  FClkActions[AIndexToInsertAt].CallTemplateOptions.CallOnlyIfConditionVarValue := '';
  FClkActions[AIndexToInsertAt].CallTemplateOptions.EvaluateBeforeCalling := False;
  FClkActions[AIndexToInsertAt].CallTemplateOptions.ListOfCustomVarsAndValues := '';

  if FullTemplatesDir + '\' + ExtractFileName(ATemplateFileName) = ATemplateFileName then
    FClkActions[AIndexToInsertAt].CallTemplateOptions.TemplateFileName := ExtractFileName(ATemplateFileName)
  else
    FClkActions[AIndexToInsertAt].CallTemplateOptions.TemplateFileName := ATemplateFileName;

  vstActions.RootNodeCount := Length(FClkActions);
end;


procedure TfrClickerActionsArr.InsertSetVar(AIndexToInsertAt: Integer; AActionName: string; var ANewAction: TClkSetVarOptions);
var
  i, n: Integer;
begin
  n := Length(FClkActions);
  SetLength(FClkActions, n + 1);

  for i := n downto AIndexToInsertAt + 1 do
    FClkActions[i] := FClkActions[i - 1];

  FClkActions[AIndexToInsertAt].ActionDebuggingStatus := adsNone;
  FClkActions[AIndexToInsertAt].ActionOptions.Action := acSetVar;
  FClkActions[AIndexToInsertAt].ActionOptions.ActionCondition := '';
  FClkActions[AIndexToInsertAt].ActionOptions.ActionEnabled := True;
  FClkActions[AIndexToInsertAt].ActionOptions.ActionName := AActionName;
  FClkActions[AIndexToInsertAt].ActionOptions.ActionTimeout := 0;
  FClkActions[AIndexToInsertAt].SetVarOptions := ANewAction;

  vstActions.RootNodeCount := Length(FClkActions);
end;


procedure TfrClickerActionsArr.LoadActionIntoEditorByIndex(AIndex: Integer);
var
  Node: PVirtualNode;
begin
  Node := vstActions.GetFirst;
  repeat
    if Integer(Node^.Index) = AIndex then
    begin;
      vstActions.Selected[Node] := True;
      vstActions.ScrollIntoView(Node, True);
      UpdateControlsFromActionsArr(Node^.Index);
      Break;
    end;

    Node := Node^.NextSibling;
  until Node = nil;
end;


procedure TfrClickerActionsArr.CopySelectedActionsToClipboard;
var
  AStringList: TStringList;   //much faster than T(Mem)IniFile
  ActionsToCopy: TClkActionsRecArr;
begin
  AStringList := TStringList.Create;
  try
    GetSelectedActions(ActionsToCopy);
    try
      SaveTemplateWithCustomActionsToStringList_V2(AStringList, ActionsToCopy, '');
      Clipboard.AsText := AStringList.Text;
    finally
      SetLength(ActionsToCopy, 0);
    end;
  finally
    AStringList.Free;
  end;
end;


procedure TfrClickerActionsArr.PasteActionsFromClipboard;
var
  i: Integer;
  AStringList: TStringList;
  Ini: TClkIniReadonlyFile;
  FormatVersion: string;
  ClipboardClkActions: TClkActionsRecArr;
  Node: PVirtualNode;
  DummyNotes: string;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Text := Clipboard.AsText;

    Ini := TClkIniReadonlyFile.Create(AStringList);
    try
      SetLength(ClipboardClkActions, Ini.ReadInteger('Actions', 'Count', 0));
      try
        FormatVersion := Ini.ReadString('Actions', 'Version', '1');

        if FormatVersion = '2' then
        begin
          LoadTemplateToCustomActions_V2(Ini, ClipboardClkActions, DummyNotes);

          vstActions.ClearSelection;
          Application.ProcessMessages;
          for i := 0 to Length(ClipboardClkActions) - 1 do
          begin
            SetLength(FClkActions, Length(FClkActions) + 1);
            FClkActions[Length(FClkActions) - 1] := ClipboardClkActions[i];    //to be replaced with a copy function
            vstActions.RootNodeCount := Length(FClkActions);
            vstActions.Selected[vstActions.GetLast] := True;
          end;
        end
        else
          MessageBox(Handle, PChar('Unhandled clipboard format version: ' + FormatVersion), PChar(Application.Title), MB_ICONERROR);
      finally
        SetLength(ClipboardClkActions, 0);
      end;
    finally
      Ini.Free;
    end;
  finally
    AStringList.Free;
  end;

  vstActions.Repaint;
  Node := vstActions.GetFirstSelected;
  if Node <> nil then
    vstActions.ScrollIntoView(Node, False);

  Modified := True;
  StopGlowingUpdateButton;
end;


function TfrClickerActionsArr.GetSelectedActions(var AIndexArr: TIntegerDynArray; var AActionsArr: TClkActionsRecArr): Integer;
var
  Node: PVirtualNode;
begin
  Result := 0;
  Node := vstActions.GetFirstSelected;

  if Node = nil then
    Exit;

  repeat
    if vstActions.Selected[Node] then
    begin
      SetLength(AIndexArr, Result + 1);
      SetLength(AActionsArr, Result + 1);

      AIndexArr[Result] := Node^.Index;
      AActionsArr[Result] := FClkActions[Node^.Index];    //to be replaced with a copy function

      Inc(Result);
    end;

    Node := Node^.NextSibling;
  until Node = nil;
end;


procedure TfrClickerActionsArr.GetSelectedActions(var ActionsToCopy: TClkActionsRecArr);
var
  AIndexArr: TIntegerDynArray;
begin
  GetSelectedActions(AIndexArr, ActionsToCopy);
  SetLength(AIndexArr, 0);
end;


procedure TfrClickerActionsArr.ReplaceSelectedActionsWithCallTemplate(var AActionIndexArrToReplace: TIntegerDynArray; AFirstSelectedIndex: Integer; ACallTemplateFileName: string);
var
  i, n: Integer;
begin
  n := Length(AActionIndexArrToReplace);
  //remove selected
  for i := n - 1 downto 0 do
    RemoveAction(AActionIndexArrToReplace[i]);  //use a for to remove actions, because the vst is reset and refreshed at every call, so a  Node := Node^.PrevSibling will not work

  //insert a call action
  InsertCallTemplateForActionReplacing(AFirstSelectedIndex, ACallTemplateFileName);

  vstActions.Repaint;

  vstActions.ClearSelection;
  Application.ProcessMessages;

  LoadActionIntoEditorByIndex(AFirstSelectedIndex);

  Modified := True;
  StopGlowingUpdateButton;
end;


procedure TfrClickerActionsArr.MenuItem_RefactorSelectedActionsIntoATemplateClick
  (Sender: TObject);
var
  Node: PVirtualNode;
  NewClkActions: TClkActionsRecArr;
  FirstSelectedIndex: Integer;
  ActionsToRemove: TIntegerDynArray;
begin
  Node := vstActions.GetFirstSelected;
  if Node = nil then
  begin
    MessageBox(Handle, 'There is no selected action. Please select at least one.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  if not DoOnSaveDialogExecute(CTemplateDialogFilter) then
    Exit;

  if ExtractFileExt(DoOnGetSaveDialogFileName) = '' then
    DoOnSetSaveDialogFileName(DoOnGetSaveDialogFileName + '.clktmpl');

  if DoOnFileExists(DoOnGetSaveDialogFileName) then
    if MessageBox(Handle, 'The selected file already exists. Do you want to overwrite it?', PChar(Caption), MB_ICONQUESTION + MB_YESNO) = IDNO then
      Exit;

  FirstSelectedIndex := Node^.Index;

  SetLength(NewClkActions, 0);
  SetLength(ActionsToRemove, 0);
  try
    GetSelectedActions(ActionsToRemove, NewClkActions);
    SaveTemplateWithCustomActions_V2(DoOnGetSaveDialogFileName, NewClkActions, 'refactored actions');
    ReplaceSelectedActionsWithCallTemplate(ActionsToRemove, FirstSelectedIndex, DoOnGetSaveDialogFileName);
  finally
    SetLength(NewClkActions, 0);
    SetLength(ActionsToRemove, 0);
  end;
end;


//Similar to the "refactor actions" above, but this one uses an already existing template.
procedure TfrClickerActionsArr.MenuItem_ReplaceSelectedActionsWithATemplateCallClick
  (Sender: TObject);
var
  Node: PVirtualNode;
  NewClkActions: TClkActionsRecArr;
  FirstSelectedIndex: Integer;
  ActionsToRemove: TIntegerDynArray;
  Fnm: string;
begin
  Node := vstActions.GetFirstSelected;
  if Node = nil then
  begin
    MessageBox(Handle, 'There is no selected action. Please select at least one.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  if not DoOnOpenDialogExecute(CTemplateDialogFilter) then
    Exit;

  Fnm := DoOnGetOpenDialogFileName;

  if ExtractFileExt(Fnm) = '' then
    Fnm := Fnm + '.clktmpl';

  if not DoOnFileExists(Fnm) then
  begin
    MessageBox(Handle, 'The selected file does not exist.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  FirstSelectedIndex := Node^.Index;

  SetLength(NewClkActions, 0);
  SetLength(ActionsToRemove, 0);
  try
    GetSelectedActions(ActionsToRemove, NewClkActions);
    ReplaceSelectedActionsWithCallTemplate(ActionsToRemove, FirstSelectedIndex, Fnm);
  finally
    SetLength(NewClkActions, 0);
    SetLength(ActionsToRemove, 0);
  end;
end;


procedure TfrClickerActionsArr.MenuItem_SetActionStatusToAllowedFailedClick(
  Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstActions.GetFirstSelected;
  if Node = nil then
    Exit;

  FClkActions[Node^.Index].ActionStatus := asAllowedFailed;
  SetActionVarValue('$LastAction_Status$', CActionStatusStr[FClkActions[Node^.Index].ActionStatus]);
  vstActions.InvalidateNode(Node);
end;


procedure TfrClickerActionsArr.MenuItem_SetActionStatusToFailedClick(
  Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstActions.GetFirstSelected;
  if Node = nil then
    Exit;

  FClkActions[Node^.Index].ActionStatus := asFailed;
  SetActionVarValue('$LastAction_Status$', CActionStatusStr[FClkActions[Node^.Index].ActionStatus]);
  vstActions.InvalidateNode(Node);
end;


procedure TfrClickerActionsArr.MenuItem_SetActionStatusToSuccessfulClick(
  Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstActions.GetFirstSelected;
  if Node = nil then
    Exit;

  FClkActions[Node^.Index].ActionStatus := asSuccessful;
  SetActionVarValue('$LastAction_Status$', CActionStatusStr[FClkActions[Node^.Index].ActionStatus]);
  vstActions.InvalidateNode(Node);
end;


procedure TfrClickerActionsArr.pnlActionsClick(Sender: TObject);
begin
  if pnlPalette.Visible then
  begin
    pnlPalette.Hide;
    FPalette.ResetAutoHideTimer;
  end;
end;


procedure TfrClickerActionsArr.FrameResize(Sender: TObject);
var
  NewTop: Integer;
begin
  NewTop := pnlVertSplitter.Top;

  if NewTop > Height - 230 then
    NewTop := Height - 230;

  ResizeFrameSectionsBySplitter(NewTop);
end;


procedure TfrClickerActionsArr.ResizeFrameSectionsBySplitter(NewTop: Integer);
begin
  if NewTop < pnlActions.Constraints.MinHeight then
    NewTop := pnlActions.Constraints.MinHeight;

  if NewTop > Height - 230 then
    NewTop := Height - 230;

  pnlVertSplitter.Top := NewTop;

  pnlActionsEditor.Top := pnlVertSplitter.Top + pnlVertSplitter.Height;
  pnlActions.Height := pnlVertSplitter.Top;
end;


procedure TfrClickerActionsArr.pnlVertSplitterMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Shift <> [ssLeft] then
    Exit;

  if not FHold then
  begin
    GetCursorPos(FSplitterMouseDownGlobalPos);

    FSplitterMouseDownImagePos.Y := pnlVertSplitter.Top;
    FHold := True;
  end;
end;


procedure TfrClickerActionsArr.pnlVertSplitterMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  tp: TPoint;
  NewTop: Integer;
begin
  if Shift <> [ssLeft] then
    Exit;

  if not FHold then
    Exit;

  GetCursorPos(tp);
  NewTop := FSplitterMouseDownImagePos.Y + tp.Y - FSplitterMouseDownGlobalPos.Y;

  ResizeFrameSectionsBySplitter(NewTop);
end;


procedure TfrClickerActionsArr.pnlVertSplitterMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FHold := False;
end;


procedure TfrClickerActionsArr.ExtraLoadInMemFileClick(Sender: TObject);
var
  Fnm: string;
begin
  Fnm := (Sender as TMenuItem).Caption;
  Fnm := StringReplace(Fnm, '&', '', [rfReplaceAll]);

  LoadTemplateWithUIUpdate(Fnm, flMem, InMemFS);

  SaveTemplateIfModified;

  FFileName := Fnm; //update before loading, to allow properly displaying the label
  lblModifiedStatus.Hint := FFileName;
  LoadTemplate(FFileName, flMem, InMemFS); //load with full path

  frClickerActions.ClearControls;
  StopGlowingUpdateButton;
end;


procedure TfrClickerActionsArr.spdbtnExtraLoadClick(Sender: TObject);
var
  tp: TPoint;
  i: Integer;
  MenuItem: TMenuItem;
  ListOfMemFiles: TStringList;
begin
  pmExtraLoad.Items[1].Clear;

  ListOfMemFiles := TStringList.Create;
  try
    frClickerActions.InMemFS.ListMemFiles(ListOfMemFiles);

    for i := 0 to ListOfMemFiles.Count - 1 do
    begin
      MenuItem := TMenuItem.Create(Self);
      MenuItem.Caption := ListOfMemFiles.Strings[i];
      MenuItem.OnClick := ExtraLoadInMemFileClick;
      pmExtraLoad.Items[1].Add(MenuItem);
    end;

  finally
    ListOfMemFiles.Free;
  end;

  GetCursorPos(tp);
  pmExtraLoad.Popup(tp.X, tp.Y);
end;


procedure TfrClickerActionsArr.spdbtnExtraPlayActionClick(Sender: TObject);
var
  tp: TPoint;
begin
  GetCursorPos(tp);
  pmExtraPlayAction.Popup(tp.X, tp.Y);
end;


procedure TfrClickerActionsArr.FPaletteVstMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FPalette.vstActionsPalette.Tag := 1;
end;


procedure TfrClickerActionsArr.FPaletteVstMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
//  tp: TPoint;
//  DeltaX, DeltaY: Integer;
begin
  FPalette.tmrHide.Tag := 0;

  if FPalette.vstActionsPalette.Tag <> 1 then
    Exit;

  Node := FPalette.vstActionsPalette.GetFirstSelected;
  if Node = nil then
    Exit;

  if ssLeft in Shift then
    Windows.SetCursor(Screen.Cursors[crDrag]);
  //
  //GetCursorPos(tp);
  //DeltaX := Left + pnlDrawingBoard.Left + scrboxScreen.Left + 8;
  //DeltaY := Top + scrboxScreen.Top - 8 + Height - ClientHeight;
  //
end;


procedure TfrClickerActionsArr.FPaletteVsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
  tp: TPoint;
  Comp: TCompRec;
  n: Integer;
  TempControlsModified: Boolean;
begin
  if FPalette.vstActionsPalette.Tag <> 1 then
    Exit;

  FPalette.vstActionsPalette.Tag := 0;

  Windows.SetCursor(Screen.Cursors[crDefault]);

  Node := FPalette.vstActionsPalette.GetFirstSelected;
  if Node = nil then
    Exit;

  GetCursorPos(tp);
  Comp := GetWindowClassRec(tp);

  if Comp.Handle = vstActions.Handle then
  begin
    n := Length(FClkActions);
    SetLength(FClkActions, n + 1);

    UpdateActionsArrFromControls(n); ///////////////////////////////// ToDo  replace this call with some default values

    FClkActions[n].ActionOptions.Action := TClkAction(Node^.Index);
    FClkActions[n].ActionOptions.ActionCondition := '';
    FClkActions[n].ActionOptions.ActionEnabled := True;
    FClkActions[n].ActionOptions.ActionName := '"' + CClkActionStr[FClkActions[n].ActionOptions.Action] + '"';
    FClkActions[n].ActionOptions.ActionTimeout := 0;
    FClkActions[n].ActionStatus := asNotStarted;


    //Important default values    ///////////////////////////////// ToDo  replace these with some default values
    FClkActions[n].ClickOptions.XClickPointReference := xrefLeft;
    FClkActions[n].ClickOptions.YClickPointReference := yrefTop;
    FClkActions[n].ClickOptions.XClickPointVar := '$Control_Left$';
    FClkActions[n].ClickOptions.YClickPointVar := '$Control_Top$';
    FClkActions[n].ClickOptions.XOffset := '4';
    FClkActions[n].ClickOptions.YOffset := '4';
    FClkActions[n].ClickOptions.MouseButton := mbLeft;
    FClkActions[n].ClickOptions.ClickWithCtrl := False;
    FClkActions[n].ClickOptions.ClickWithAlt := False;
    FClkActions[n].ClickOptions.ClickWithShift := False;
    //FClkActions[n].ClickOptions.ClickWithDoubleClick := False;
    FClkActions[n].ClickOptions.Count := 1;
    FClkActions[n].ClickOptions.LeaveMouse := False;
    FClkActions[n].ClickOptions.MoveWithoutClick := False;
    FClkActions[n].ClickOptions.ClickType := CClickType_Click;
    FClkActions[n].ClickOptions.XClickPointReferenceDest := xrefLeft;
    FClkActions[n].ClickOptions.YClickPointReferenceDest := yrefTop;
    FClkActions[n].ClickOptions.XClickPointVarDest := '$Control_Left$';
    FClkActions[n].ClickOptions.YClickPointVarDest := '$Control_Top$';
    FClkActions[n].ClickOptions.XOffsetDest := '7';
    FClkActions[n].ClickOptions.YOffsetDest := '7';
    FClkActions[n].ClickOptions.MouseWheelType := mwtVert;
    FClkActions[n].ClickOptions.MouseWheelAmount := '1';
    FClkActions[n].ClickOptions.DelayAfterMovingToDestination := '50';
    FClkActions[n].ClickOptions.DelayAfterMouseDown := '200';
    FClkActions[n].ClickOptions.MoveDuration := '-1';

    FClkActions[n].ExecAppOptions.PathToApp := '';
    FClkActions[n].ExecAppOptions.ListOfParams := '';
    FClkActions[n].ExecAppOptions.WaitForApp := False;
    FClkActions[n].ExecAppOptions.AppStdIn := '';
    FClkActions[n].ExecAppOptions.CurrentDir := '';
    FClkActions[n].ExecAppOptions.UseInheritHandles := uihNo;
    FClkActions[n].ExecAppOptions.NoConsole := False;

    FClkActions[n].FindControlOptions.MatchCriteria.WillMatchText := FClkActions[n].ActionOptions.Action = acFindControl;
    FClkActions[n].FindControlOptions.MatchCriteria.WillMatchClassName := FClkActions[n].ActionOptions.Action = acFindControl;
    FClkActions[n].FindControlOptions.MatchCriteria.WillMatchBitmapText := FClkActions[n].ActionOptions.Action = acFindSubControl;
    FClkActions[n].FindControlOptions.MatchCriteria.WillMatchBitmapFiles := False;
    FClkActions[n].FindControlOptions.MatchCriteria.SearchForControlMode := sfcmGenGrid;
    FClkActions[n].FindControlOptions.AllowToFail := False;
    FClkActions[n].FindControlOptions.MatchText := '';
    FClkActions[n].FindControlOptions.MatchClassName := '';
    FClkActions[n].FindControlOptions.MatchTextSeparator := '';
    FClkActions[n].FindControlOptions.MatchClassNameSeparator := '';
    FClkActions[n].FindControlOptions.MatchBitmapFiles := '';
    FClkActions[n].FindControlOptions.MatchBitmapAlgorithm := mbaBruteForce;
    FClkActions[n].FindControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf := 1;
    FClkActions[n].FindControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf := 1;
    FClkActions[n].FindControlOptions.MatchBitmapAlgorithmSettings.XOffset := 0;
    FClkActions[n].FindControlOptions.MatchBitmapAlgorithmSettings.YOffset := 0;
    FClkActions[n].FindControlOptions.InitialRectangle.Left := '$Control_Left$';
    FClkActions[n].FindControlOptions.InitialRectangle.Top := '$Control_Top$';
    FClkActions[n].FindControlOptions.InitialRectangle.Right := '$Control_Right$';
    FClkActions[n].FindControlOptions.InitialRectangle.Bottom := '$Control_Bottom$';
    FClkActions[n].FindControlOptions.InitialRectangle.LeftOffset := '0';
    FClkActions[n].FindControlOptions.InitialRectangle.TopOffset := '0';
    FClkActions[n].FindControlOptions.InitialRectangle.RightOffset := '0';
    FClkActions[n].FindControlOptions.InitialRectangle.BottomOffset := '0';
    FClkActions[n].FindControlOptions.UseWholeScreen := FClkActions[n].ActionOptions.Action = acFindControl;
    FClkActions[n].FindControlOptions.ColorError := '0';
    FClkActions[n].FindControlOptions.AllowedColorErrorCount := '0';
    FClkActions[n].FindControlOptions.WaitForControlToGoAway := False;
    FClkActions[n].FindControlOptions.StartSearchingWithCachedControl := False;
    FClkActions[n].FindControlOptions.CachedControlLeft := '';
    FClkActions[n].FindControlOptions.CachedControlTop := '';
    FClkActions[n].FindControlOptions.MatchPrimitiveFiles := '';
    FClkActions[n].FindControlOptions.MatchPrimitiveFiles_Modified := '';
    FClkActions[n].FindControlOptions.GetAllControls := False;
    FClkActions[n].FindControlOptions.UseFastSearch := True;
    FClkActions[n].FindControlOptions.FastSearchAllowedColorErrorCount := '10';
    FClkActions[n].FindControlOptions.IgnoredColors := '';

    SetLength(FClkActions[n].FindControlOptions.MatchBitmapText, 1);
    FClkActions[n].FindControlOptions.MatchBitmapText[0].ForegroundColor := '$Color_Window$';
    FClkActions[n].FindControlOptions.MatchBitmapText[0].BackgroundColor := '$Color_Highlight$';
    FClkActions[n].FindControlOptions.MatchBitmapText[0].FontName := 'Tahoma';
    FClkActions[n].FindControlOptions.MatchBitmapText[0].FontSize := 8;
    FClkActions[n].FindControlOptions.MatchBitmapText[0].FontQualityReplacement := '';
    FClkActions[n].FindControlOptions.MatchBitmapText[0].FontQuality := fqNonAntialiased;
    FClkActions[n].FindControlOptions.MatchBitmapText[0].FontQualityUsesReplacement := False;
    FClkActions[n].FindControlOptions.MatchBitmapText[0].Bold := False;
    FClkActions[n].FindControlOptions.MatchBitmapText[0].Italic := False;
    FClkActions[n].FindControlOptions.MatchBitmapText[0].Underline := False;
    FClkActions[n].FindControlOptions.MatchBitmapText[0].StrikeOut := False;
    FClkActions[n].FindControlOptions.MatchBitmapText[0].CropLeft := '0';
    FClkActions[n].FindControlOptions.MatchBitmapText[0].CropTop := '0';
    FClkActions[n].FindControlOptions.MatchBitmapText[0].CropRight := '0';
    FClkActions[n].FindControlOptions.MatchBitmapText[0].CropBottom := '0';
    FClkActions[n].FindControlOptions.MatchBitmapText[0].IgnoreBackgroundColor := False;
    FClkActions[n].FindControlOptions.MatchBitmapText[0].ProfileName := CDefaultFontProfileName;

    if FClkActions[n].ActionOptions.Action = acFindSubControl then
      FClkActions[n].ActionOptions.ActionTimeout := 1000;

    if FClkActions[n].ActionOptions.Action = acFindControl then
      FClkActions[n].ActionOptions.ActionTimeout := 3000;

    FClkActions[n].SetTextOptions.Text := '';
    FClkActions[n].SetTextOptions.ControlType := stEditBox;
    FClkActions[n].SetTextOptions.DelayBetweenKeyStrokes := '0';

    FClkActions[n].CallTemplateOptions.TemplateFileName := '';
    FClkActions[n].CallTemplateOptions.ListOfCustomVarsAndValues := '';
    FClkActions[n].CallTemplateOptions.EvaluateBeforeCalling := False;
    FClkActions[n].CallTemplateOptions.CallTemplateLoop.Enabled := False;
    FClkActions[n].CallTemplateOptions.CallTemplateLoop.Counter := '';
    FClkActions[n].CallTemplateOptions.CallTemplateLoop.InitValue := '';
    FClkActions[n].CallTemplateOptions.CallTemplateLoop.EndValue := '';
    FClkActions[n].CallTemplateOptions.CallTemplateLoop.Direction := ldInc;
    FClkActions[n].CallTemplateOptions.CallTemplateLoop.BreakCondition := '';
    FClkActions[n].CallTemplateOptions.CallTemplateLoop.EvalBreakPosition := lebpBeforeContent;

    FClkActions[n].SleepOptions.Value := '1000';

    FClkActions[n].SetVarOptions.ListOfVarNames := '';
    FClkActions[n].SetVarOptions.ListOfVarValues := '';
    FClkActions[n].SetVarOptions.ListOfVarEvalBefore := '';

    FClkActions[n].WindowOperationsOptions.Operation := woBringToFront;
    FClkActions[n].WindowOperationsOptions.NewX := '';
    FClkActions[n].WindowOperationsOptions.NewY := '';
    FClkActions[n].WindowOperationsOptions.NewWidth := '';
    FClkActions[n].WindowOperationsOptions.NewHeight := '';
    FClkActions[n].WindowOperationsOptions.NewPositionEnabled := False;
    FClkActions[n].WindowOperationsOptions.NewSizeEnabled := False;

    vstActions.RootNodeCount := Length(FClkActions);

    TempControlsModified := frClickerActions.ControlsModified;
    if not TempControlsModified then
    begin
      frClickerActions.CurrentlyEditingActionType := TClkAction(FClkActions[n].ActionOptions.Action);

      vstActions.ClearSelection;
      vstActions.Selected[vstActions.GetLast] := True;
      vstActions.ScrollIntoView(vstActions.GetLast, False);
      //LoadActionIntoEditorByIndex(n);

      frClickerActions.ClearControls;
      FClkActions[n].ActionOptions.Action := TClkAction(Node^.Index);
      FClkActions[n].ActionOptions.ActionName := '"' + CClkActionStr[FClkActions[n].ActionOptions.Action] + '"';

      frClickerActions.EditingAction^.ActionOptions := FClkActions[n].ActionOptions;   //temp solution, to load action settings

      UpdateControlsFromActionsArr(n); //UpdateActionsArrFromControls(n);
      frClickerActions.UpdatePageControlActionExecutionIcons;
    end;

    vstActions.Repaint;
    Application.ProcessMessages;

    if not TempControlsModified then
      StopGlowingUpdateButton;

    Modified := True;
  end;
end;


procedure TfrClickerActionsArr.spdbtnPaletteClick(Sender: TObject);
var
  ActionNames: TStringList;
  i: TClkAction;
begin
  pnlPalette.Visible := not pnlPalette.Visible;

  pnlPalette.Left := vstActions.Left + vstActions.Width + 2;
  pnlPalette.Top := 0;
  pnlPalette.Caption := '';
  pnlPalette.Height := spdbtnPalette.Top;
  pnlPalette.Anchors := [akRight, akTop];

  if FPalette = nil then
  begin
    FPalette := TfrClickerActionsPalette.Create(Self);
    FPalette.Parent := pnlPalette;
    FPalette.Left := 0;
    FPalette.Top := 0;
    FPalette.Width := pnlPalette.Width;
    FPalette.Height := pnlPalette.Height;

    FPalette.Images := frClickerActions.imglstActions;
    FPalette.OwnerPanel := pnlPalette;

    ActionNames := TStringList.Create;
    try
      for i := Low(TClkAction) to High(TClkAction) do
        ActionNames.Add(CClkActionStr[i]);

      FPalette.SetActionNames(ActionNames);
      FPalette.vstActionsPalette.OnMouseDown := FPaletteVstMouseDown;
      FPalette.vstActionsPalette.OnMouseMove := FPaletteVstMouseMove;
      FPalette.vstActionsPalette.OnMouseUp := FPaletteVsMouseUp;
    finally
      ActionNames.Free;
    end;
  end;

  if pnlPalette.Visible then
    FPalette.StartAutoHide
  else
    FPalette.ResetAutoHideTimer;
end;


procedure TfrClickerActionsArr.spdbtnTemplateNotesClick(Sender: TObject);
var
  Notes: string;
begin
  Notes := FastReplace_45ToReturn(FTemplateNotes);
  if EditTemplateNotes(Notes) then
  begin
    Modified := True;
    FTemplateNotes := FastReplace_ReturnTo45(Notes);
  end;
end;


procedure TfrClickerActionsArr.InsertActionBeforeSelected1Click(Sender: TObject);
var
  i, n: Integer;
  Node: PVirtualNode;
begin
  Node := vstActions.GetFirstSelected;
  if Node = nil then
  begin
    MessageBox(Handle, 'Please select an action in the list.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  if not ValidActionToBeAdded then
    Exit;

  n := Length(FClkActions);
  SetLength(FClkActions, n + 1);

  for i := n downto Node^.Index + 1 do
    FClkActions[i] := FClkActions[i - 1];

  UpdateActionsArrFromControls(Node^.Index);
  FClkActions[Node^.Index].ActionStatus := asNotStarted;
  FClkActions[Node^.Index].ActionOptions.ActionEnabled := True;

  vstActions.RootNodeCount := Length(FClkActions);
  UpdateNodesCheckStateFromActions;

  vstActions.Repaint;
  Modified := True;
  StopGlowingUpdateButton;
  frClickerActions.UpdatePageControlActionExecutionIcons;
end;


procedure TfrClickerActionsArr.InsertActionAfterSelected1Click(Sender: TObject);
var
  i, n, NewIndex: Integer;
  Node: PVirtualNode;
begin
  Node := vstActions.GetFirstSelected;
  if Node = nil then
  begin
    MessageBox(Handle, 'Please select an action in the list.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  if not ValidActionToBeAdded then
    Exit;

  n := Length(FClkActions);
  SetLength(FClkActions, n + 1);

  for i := n downto Node^.Index + 2 do
    FClkActions[i] := FClkActions[i - 1];

  NewIndex := Node^.Index + 1;
  UpdateActionsArrFromControls(NewIndex);
  FClkActions[NewIndex].ActionStatus := asNotStarted;
  FClkActions[NewIndex].ActionOptions.ActionEnabled := True;

  vstActions.RootNodeCount := Length(FClkActions);
  UpdateNodesCheckStateFromActions;

  vstActions.Selected[Node] := False;
  vstActions.Selected[Node^.NextSibling] := True;

  vstActions.Repaint;
  Modified := True;
  StopGlowingUpdateButton;
  frClickerActions.UpdatePageControlActionExecutionIcons;
end;


procedure TfrClickerActionsArr.spdbtnUpdateActionClick(Sender: TObject);
var
  Node: PVirtualNode;
  CurrentAction: TClkAction;
begin
  Node := vstActions.GetFirstSelected;

  if Node = nil then
  begin
    MessageBox(Handle, 'No item selected for updating. Please select an item.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  if Ord(frClickerActions.CurrentlyEditingActionType) = CClkUnsetAction then
  begin
    MessageBox(Handle, 'No action selected. Please select an action.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  CurrentAction := frClickerActions.CurrentlyEditingActionType;
  case CurrentAction of
    acClick:
    begin

    end;

    acFindControl:
      if not (frClickerActions.EditingAction^.FindControlOptions.MatchCriteria.WillMatchText or
              frClickerActions.EditingAction^.FindControlOptions.MatchCriteria.WillMatchClassName or
              frClickerActions.EditingAction^.FindControlOptions.MatchCriteria.WillMatchBitmapText or
              frClickerActions.EditingAction^.FindControlOptions.MatchCriteria.WillMatchBitmapFiles) then
      begin
        MessageBox(Handle, 'To find a control, at least one match criterion has to be checked.', PChar(Caption), MB_ICONINFORMATION);
        Exit;
      end;

    acSetControlText:
      ;

    else
    begin
    end;
  end;  //case

  if FClkActions[Node^.Index].ActionOptions.Action <> frClickerActions.CurrentlyEditingActionType then
    if MessageBox(Handle, 'Are you sure you want to overwrite existing action?', PChar(Caption), MB_ICONWARNING + MB_YESNO) = IDNO then
      Exit;

  UpdateActionsArrFromControls(Node^.Index);         //CopyActionContent(frClickerActions.EditingAction^, FClkActions[ActionIndex]);
  vstActions.Repaint;
  Modified := True;
  StopGlowingUpdateButton;
  frClickerActions.UpdatePageControlActionExecutionIcons;
end;


procedure TfrClickerActionsArr.tmrDeleteActionsTimer(Sender: TObject);
begin
  tmrDeleteActions.Enabled := False;
  RemoveSelectedActions;
end;


procedure TfrClickerActionsArr.SetAllActionsToNotStarted;
var
  i: Integer;
begin
  for i := 0 to Length(FClkActions) - 1 do
  begin
    FClkActions[i].ActionSkipped := False;
    FClkActions[i].ActionStatus := asNotStarted;
  end;

  vstActions.Repaint;
end;


procedure TfrClickerActionsArr.PlayAllActionsFromButton(IsDebugging: Boolean = False; StartAtSelected: Boolean = False);
begin
  SetAllActionsToNotStarted;

  FDebugging := IsDebugging;

  if chkResetVarsOnPlayAll.Checked then
  begin
    //Reset work vars before searching..
    SetActionVarValue('$Control_Text$', 'unknown');
    SetActionVarValue('$Control_Left$', '-1');
    SetActionVarValue('$Control_Top$', '-1');
    SetActionVarValue('$Control_Right$', '-1');
    SetActionVarValue('$Control_Bottom$', '-1');
    SetActionVarValue('$Control_Width$', '-1');
    SetActionVarValue('$Control_Height$', '-1');
    SetActionVarValue('$Half_Control_Width$', '-1');
    SetActionVarValue('$Half_Control_Height$', '-1');
    SetActionVarValue('$Control_Class$', 'unknown');
    SetActionVarValue('$Control_Handle$', '0');
    SetActionVarValue('$ExecAction_Err$', '');
  end;

  spdbtnPlaySelectedAction.Enabled := False;
  spdbtnPlayAllActions.Enabled := False;
  PlayAllInDebuggingMode1.Enabled := False;
  spdbtnStopPlaying.Enabled := True;

  spdbtnPlaySelectedAction.Repaint;
  spdbtnPlayAllActions.Repaint;
  spdbtnStopPlaying.Repaint;
  FStopAllActionsOnDemand := False;
  if FStopAllActionsOnDemandFromParent <> nil then
    FStopAllActionsOnDemandFromParent^ := False; //set this to avoid stopping children instances

  FPlaying := True;
  try
    PlayAllActions(IsDebugging, StartAtSelected);
    Sleep(100);
  finally
    spdbtnPlaySelectedAction.Enabled := True;
    spdbtnPlayAllActions.Enabled := True;
    PlayAllInDebuggingMode1.Enabled := True;
    spdbtnStopPlaying.Enabled := False;
    FPlaying := False;
  end;
end;


procedure TfrClickerActionsArr.PlayAllInDebuggingMode1Click(Sender: TObject);
begin
  PrepareFilesInServer;
  PlayAllActionsFromButton(True);
end;


procedure TfrClickerActionsArr.PlayAllInDebuggingModeStartingAtSelected1Click(
  Sender: TObject);
begin
  PrepareFilesInServer;
  PlayAllActionsFromButton(True, True);
end;


procedure TfrClickerActionsArr.PlaySelectedActionFromButton;
var
  Node: PVirtualNode;
begin
  Node := vstActions.GetFirstSelected;
  if Node = nil then
  begin
    MessageBox(Handle, 'Please select an action to play.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  FDebugging := False;

  spdbtnPlaySelectedAction.Enabled := False;
  spdbtnPlayAllActions.Enabled := False;
  PlayAllInDebuggingMode1.Enabled := False;
  spdbtnStopPlaying.Enabled := True;

  spdbtnPlaySelectedAction.Repaint;
  spdbtnPlayAllActions.Repaint;
  spdbtnStopPlaying.Repaint;
  FStopAllActionsOnDemand := False;
  if FStopAllActionsOnDemandFromParent <> nil then
    FStopAllActionsOnDemandFromParent^ := False; //set this to avoid stopping children instances

  FPlaying := True;
  try
    PlaySelected;
    Sleep(100);
  finally
    spdbtnPlaySelectedAction.Enabled := True;
    spdbtnPlayAllActions.Enabled := True;
    PlayAllInDebuggingMode1.Enabled := True;
    spdbtnStopPlaying.Enabled := False;
    FPlaying := False;
  end;
end;


procedure TfrClickerActionsArr.StopAllActionsFromButton;
begin
  spdbtnPlaySelectedAction.Enabled := True;
  spdbtnPlayAllActions.Enabled := True;
  PlayAllInDebuggingMode1.Enabled := True;
  spdbtnStopPlaying.Enabled := False;
  FStopAllActionsOnDemand := True;
  if FStopAllActionsOnDemandFromParent <> nil then
    FStopAllActionsOnDemandFromParent^ := True; //set this to stop children instances   (it will stop everything, including parents)

  FPlaying := False;
end;


procedure TfrClickerActionsArr.SaveTemplateIfModified;
begin
  if FModified then
  begin
    if MessageBox(Handle, 'The current template is modified. Save before loading a new one?', PChar(Caption), MB_ICONQUESTION + MB_YESNO) = IDYES then
    begin
      DoOnSetSaveDialogInitialDir(FullTemplatesDir);

      if FFileName = '' then
        DoOnSetSaveDialogFileName('')
      else
        DoOnSetSaveDialogFileName(FullTemplatesDir + '\' + FFileName);

      if not DoOnSaveDialogExecute(CTemplateDialogFilter) then
        Exit;

      if LowerCase(ExtractFileExt(DoOnGetSaveDialogFileName)) <> '.clktmpl' then
        DoOnSetSaveDialogFileName(DoOnGetSaveDialogFileName + '.clktmpl'); //required later

      SaveTemplate(DoOnGetSaveDialogFileName);
      lblModifiedStatus.Hint := DoOnGetSaveDialogFileName;
    end;
  end;
end;


procedure TfrClickerActionsArr.LoadTemplateWithUIUpdate(AFileName: string; AFileLocation: TFileLocation = flDisk; AInMemFileSystem: TInMemFileSystem = nil);
begin
  SaveTemplateIfModified;

  FFileName := AFileName; //update before loading, to allow properly displaying the label
  lblModifiedStatus.Hint := FFileName;
  LoadTemplate(FFileName); //load with full path

  frClickerActions.ClearControls;
  StopGlowingUpdateButton;
end;


procedure TfrClickerActionsArr.btnLoadTemplateClick(Sender: TObject);
begin
  DoOnSetOpenDialogInitialDir(FullTemplatesDir);
  CreateDirWithSubDirs(FullTemplatesDir);
  if not DoOnOpenDialogExecute(CTemplateDialogFilter) then
    Exit;

  LoadTemplateWithUIUpdate(DoOnGetOpenDialogFileName);
end;


procedure TfrClickerActionsArr.btnSaveTemplateClick(Sender: TObject);
begin
  if FFileName <> ExtractFileName(FFileName) then  //custom dir    -  FFileName is a full path
  begin
    if not DoOnFileExists(FFileName) then
      SaveTemplateWithDialog
    else
    begin
      if LowerCase(ExtractFileExt(FFileName)) <> '.clktmpl' then
        FFileName := FFileName + '.clktmpl';
      SaveTemplate(FFileName);
    end;
  end
  else
  begin   //local templates dir
    if not DoOnFileExists(FullTemplatesDir + '\' + FFileName) then
      SaveTemplateWithDialog
    else
    begin
      if LowerCase(ExtractFileExt(FFileName)) <> '.clktmpl' then
        FFileName := FFileName + '.clktmpl';
      SaveTemplate(FullTemplatesDir + '\' + FFileName);
    end;
  end;
end;


procedure TfrClickerActionsArr.chkEnableDebuggerKeysClick(Sender: TObject);
begin
  tmrDebugKeys.Enabled := chkEnableDebuggerKeys.Checked;
end;


procedure TfrClickerActionsArr.chkShowActionNumberClick(Sender: TObject);
begin
  //if coVisible in vstActions.Header.Columns.Items[6].Options then
  if chkShowActionNumber.Checked then
    vstActions.Header.Columns.Items[6].Options := vstActions.Header.Columns.Items[6].Options + [coVisible]
  else
    vstActions.Header.Columns.Items[6].Options := vstActions.Header.Columns.Items[6].Options - [coVisible];
end;


procedure TfrClickerActionsArr.SaveTemplateWithDialog;
var
  InitDir: string;
begin
  if ExtractFileName(FFileName) = FFileName then //no path in here
    InitDir := FullTemplatesDir
  else
    InitDir := ExtractFileDir(FFileName);

  DoOnSetSaveDialogInitialDir(InitDir);

  if FFileName = '' then
    DoOnSetSaveDialogFileName('')
  else
  begin
    if ExtractFileName(FFileName) = FFileName then //no path in here
      DoOnSetSaveDialogFileName(InitDir + '\' + FFileName)
    else
      DoOnSetSaveDialogFileName(FFileName);
  end;

  CreateDirWithSubDirs(InitDir);
  if not DoOnSaveDialogExecute(CTemplateDialogFilter) then
    Exit;

  FFileName := DoOnGetSaveDialogFileName;
  if LowerCase(ExtractFileExt(FFileName)) <> '.clktmpl' then
    FFileName := FFileName + '.clktmpl';

  if LowerCase(ExtractFileExt(DoOnGetSaveDialogFileName)) <> '.clktmpl' then
    DoOnSetSaveDialogFileName(DoOnGetSaveDialogFileName + '.clktmpl'); //required later

  lblModifiedStatus.Hint := DoOnGetSaveDialogFileName;

  if DoOnFileExists(DoOnGetSaveDialogFileName) then
    if MessageBox(Handle, PChar('"' + FFileName + '" already exists. Overwrite?' ), PChar(Caption), MB_ICONWARNING + MB_YESNO) = IDNO then
      Exit;

  Modified := True; //to trigger a label update
  SaveTemplate(DoOnGetSaveDialogFileName);  //it is a full path here
end;


procedure TfrClickerActionsArr.UpdateNodesCheckStateFromActions;
const
  CNodeStates: array[Boolean] of TCheckState = (csUncheckedNormal, csCheckedNormal);
var
  Node: PVirtualNode;
begin
  Node := vstActions.GetFirst;
  if Node = nil then
    Exit;

  repeat
    Node^.CheckState := CNodeStates[FClkActions[Node^.Index].ActionOptions.ActionEnabled];
    Node := Node^.NextSibling;
  until Node = nil;
end;


procedure TfrClickerActionsArr.RemoveAction(ActionIndex: Integer; AClearSelectionAfterRemoving: Boolean = True);
var
  i: Integer;
begin
  for i := ActionIndex to Length(FClkActions) - 2 do
    FClkActions[i] := FClkActions[i + 1];

  SetLength(FClkActions, Length(FClkActions) - 1);

  if AClearSelectionAfterRemoving then
    vstActions.RootNodeCount := 0; //to reinit nodes

  vstActions.RootNodeCount := Length(FClkActions);
  UpdateNodesCheckStateFromActions;
end;


procedure TfrClickerActionsArr.RemoveSelectedActions;
var
  Node: PVirtualNode;
begin
  Node := vstActions.GetFirstSelected;

  if Node = nil then
  begin
    MessageBox(Handle, 'No action selected for removing. Please select at least one.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  Node := vstActions.GetLast;
  if MessageBox(Handle, 'Are you sure you want to remove the selected action(s) from list?', PChar(Caption), MB_ICONQUESTION + MB_YESNO) = ID_YES then
  begin
    vstActions.BeginUpdate;
    try
      repeat
        if vstActions.Selected[Node] then
          RemoveAction(Node^.Index, False);

        Node := Node^.PrevSibling;
      until Node = nil;
    finally
      vstActions.EndUpdate;
    end;

    vstActions.Repaint;
    Modified := True;
    StopGlowingUpdateButton;

    vstActions.ClearSelection;
  end;
end;


procedure TfrClickerActionsArr.btnRemoveActionClick(Sender: TObject);
begin
  RemoveSelectedActions;
end;


procedure TfrClickerActionsArr.Removeallactions1Click(Sender: TObject);
begin
  if MessageBox(Handle, 'Are you sure you want to remove all actions from list?', PChar(Caption), MB_ICONQUESTION + MB_YESNO) = ID_YES then
  begin
    SetLength(FClkActions, 0);
    vstActions.RootNodeCount := 0;
    Modified := True;
    StopGlowingUpdateButton;
  end;
end;


procedure TfrClickerActionsArr.Removeallactionsandclearfilename1Click(
  Sender: TObject);
begin
  if MessageBox(Handle, 'Are you sure you want to remove all actions from list?', PChar(Caption), MB_ICONQUESTION + MB_YESNO) = ID_YES then
    ClearAllActions;
end;


procedure TfrClickerActionsArr.ClearAllActions;
var
  UnsetActionValue: Byte;
begin
  SetLength(FClkActions, 0);
  vstActions.RootNodeCount := 0;
  FileName := '';
  Modified := True; //to update displayed FileName
  Modified := False;   //false for clearing filename
  StopGlowingUpdateButton;

  UnsetActionValue := CClkUnsetAction; //Using a var, to get rid of warnings. The code is expected to handle out of range values.
  frClickerActions.CurrentlyEditingActionType := TClkAction(UnsetActionValue);
end;


procedure TfrClickerActionsArr.ClickerActionsFrameOnControlsModified(Sender: TObject);
begin
  tmrGlowUpdateButton.Enabled := True;
  spdbtnUpdateAction.Font.Style := [fsBold];
  spdbtnUpdateAction.Font.Size := 10;
  spdbtnUpdateAction.Hint := 'At least one action property is changed. By clicking update, the new changes will be recorded to the list of actions.';
end;



procedure TfrClickerActionsArr.tmrDebugKeysTimer(Sender: TObject);
var
  AState: TKeyboardState;
begin
  //SendMessage(Handle, WM_MOUSEMOVE, 0, 0);  //GetKeyboardState requires WM_MOUSEMOVE events when the mouse is not hovering this window
  //GetKeyboardState(AState);

  if GetAsyncKeyState(VK_F2) < 0 then
    AState[VK_F2] := $80
  else
    AState[VK_F2] := 0;

  if GetAsyncKeyState(VK_F6) < 0 then
    AState[VK_F6] := $80
  else
    AState[VK_F6] := 0;

  if GetAsyncKeyState(VK_F7) < 0 then
    AState[VK_F7] := $80
  else
    AState[VK_F7] := 0;

  if GetAsyncKeyState(VK_F8) < 0 then
    AState[VK_F8] := $80
  else
    AState[VK_F8] := 0;

  if GetAsyncKeyState(VK_F9) < 0 then
    AState[VK_F9] := $80
  else
    AState[VK_F9] := 0;

  if FDebugging then
  begin
    if (F2_State and $80 = 0) and (AState[VK_F2] and $80 = $80) and
       (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) then  //detected Ctrl-Shift-F2
    begin
      //Application.MainForm.Caption := 'Ctrl-Shift-F2';

      if spdbtnStopPlaying.Enabled then
        spdbtnStopPlaying.Click;
    end;

    if (F6_State and $80 = 0) and (AState[VK_F6] and $80 = $80) then  //detected F6
      frClickerActions.frClickerFindControl.btnDisplaySearchAreaDebuggingImage.Click;  /////////ToDo: call a method, not the handler

    if (F7_State and $80 = 0) and (AState[VK_F7] and $80 = $80) then  //detected F7
    begin
      //Application.MainForm.Caption := 'F7';
      
      if spdbtnStepInto.Enabled then
        FContinuePlayingBySteppingInto := True;
    end;

    if (F8_State and $80 = 0) and (AState[VK_F8] and $80 = $80) then  //detected F8
    begin
      //Application.MainForm.Caption := 'F8';

      if spdbtnStepOver.Enabled then
        FContinuePlayingNext := True;
    end;

    if (F9_State and $80 = 0) and (AState[VK_F9] and $80 = $80) then  //detected F9
    begin
      //Application.MainForm.Caption := 'F9';

      if spdbtnContinuePlayingAll.Enabled then
        FContinuePlayingAll := True;
    end;
  end;

  if (F9_State and $80 = 0) and (AState[VK_F9] and $80 = $80) then  //detected F9
    if not FPlaying then
      PlayAllActionsFromButton(True);

  F2_State := AState[VK_F2];
  F6_State := AState[VK_F6];
  F7_State := AState[VK_F7];
  F8_State := AState[VK_F8];
  F9_State := AState[VK_F9];
end;


procedure TfrClickerActionsArr.tmrGlowUpdateButtonTimer(Sender: TObject);
const
  CGlowDir: array[Boolean] of Integer = (-1, 1);
begin
  if tmrGlowUpdateButton.Tag > 14 then   //> 10 (i.e. 11, should be the limit, to get 255  (11 * 17 + 68 = 255), but allow more samples to be "saturated" at 255
    FUpdateButtonGlowDirection := False;

  if tmrGlowUpdateButton.Tag < 1 then
    FUpdateButtonGlowDirection := True;

  spdbtnUpdateAction.Font.Color := $334444 + Min(tmrGlowUpdateButton.Tag * 17, 187);   // $44 = 68d    255 - 68 = 187   187 / 11 = 17    steps: 68, 85, 102, ... 221, 238, 255

  tmrGlowUpdateButton.Tag := tmrGlowUpdateButton.Tag + CGlowDir[FUpdateButtonGlowDirection]; 
end;


procedure TfrClickerActionsArr.StopGlowingUpdateButton;
begin
  tmrGlowUpdateButton.Enabled := False;
  frClickerActions.ControlsModified := False;
  spdbtnUpdateAction.Font.Color := clWindowText;
  spdbtnUpdateAction.Font.Style := [];
  spdbtnUpdateAction.Font.Size := 8;
  spdbtnUpdateAction.Repaint;
end;


procedure TfrClickerActionsArr.InitFrame;
begin
  FFileName := '';
  FModified := False;
  FDebugging := False;
  FContinuePlayingAll := False;
  FContinuePlayingNext := False;
  FContinuePlayingBySteppingInto := False;

  //frClickerActions.vallstVariables.FixedCols := 1;  let it be editable
  frClickerActions.vallstVariables.ColWidths[1] := 130;

  frClickerActions.OnControlsModified := ClickerActionsFrameOnControlsModified;
  FPreviousSelectedNode := nil;

  F2_State := 0;
  F6_State := 0;
  F7_State := 0;
  F8_State := 0;
  F9_State := 0;

  chkEnableDebuggerKeys.Hint := 'F6 - Press "Display dbg img" button, from "Search Area" in "FindSubControl"' + #13#10 +
                                'F7 - Step Into' + #13#10 +
                                'F8 - Step Over' + #13#10 +
                                'F9 - Continue All  /  Play All in debugging mode' + #13#10 +
                                'Ctrl-Shift-F2 - Stop  (always enabled for certain actions)';
end;


//list of bmp files for now, but it can include all other files, which have to be sent to server
procedure TfrClickerActionsArr.GetListOfUsedFilesFromLoadedTemplate(AListOfFiles: TStringList);
var
  i, j: Integer;
  TempStringList: TStringList;
begin
  for i := 0 to Length(FClkActions) - 1 do
    if FClkActions[i].ActionOptions.Action in [acFindControl, acFindSubControl] then
    begin
      TempStringList := TStringList.Create;
      try
        TempStringList.Text := FClkActions[i].FindControlOptions.MatchBitmapFiles;

        for j := 0 to TempStringList.Count - 1 do
          AListOfFiles.Add(TempStringList.Strings[j]);
      finally
        TempStringList.Free;
      end;
    end;
end;


function TfrClickerActionsArr.GetInMemFS: TInMemFileSystem;
begin
  Result := frClickerActions.InMemFS;
end;


procedure TfrClickerActionsArr.SetInMemFS(Value: TInMemFileSystem);
begin
  frClickerActions.InMemFS := Value;
end;


procedure TfrClickerActionsArr.SetGridDrawingOption(Value: TDisplayGridLineOption);
begin
  frClickerActions.GridDrawingOption := Value;
end;


procedure TfrClickerActionsArr.SetPreviewSelectionColors(Value: TSelectionColors);
begin
  frClickerActions.PreviewSelectionColors := Value;
end;


//called in server mode
procedure TfrClickerActionsArr.tmrExecActionFromSrvModuleTimer(Sender: TObject);
var
  CurrentNode: PVirtualNode;
  Err: string;
begin
  tmrExecActionFromSrvModule.Enabled := False;

  if FStackLevel = 0 then
  begin
    FStopAllActionsOnDemand := False;
    if FStopAllActionsOnDemandFromParent <> nil then
      FStopAllActionsOnDemandFromParent^ := False;
  end;

  AddToLog(DateTimeToStr(Now) + '  [Srv]: Executing action at index ' + IntToStr(FRemoteExActionIndex));
  try
    try
      CurrentNode := GetNodeByIndex(FRemoteExActionIndex);
      if CurrentNode <> nil then
      begin
        try
          HighlightCurrentlyExecutedAction(CurrentNode);
        except
          on E: Exception do
            raise Exception.Create(E.Message + '  in HighlightCurrentlyExecutedAction(' + IntToStr(FRemoteExActionIndex) +')');
        end;                                //good to have to blanks here ('  '), to easily find the message
      end
      else
      begin
        Err := 'Cannot select action, to load values at index ' + IntToStr(FRemoteExActionIndex) + '.';
        if GetNodeByIndex(0) = nil then
        begin
          Err := Err + '  It seems that no template is loaded.';
          SetActionVarValue('$ExecAction_Err$', 'empty template');
        end;

        SetActionVarValue('$DbgCurrentAction$', Err);
        AddToLog(DateTimeToStr(Now) + '  [Srv]: ' + Err);

        FRemoteExCmdResult := False;
        SetActionVarValue('$LastAction_Status$', CActionStatusStr[asFailed]);
        Exit;
      end;

      try
        FClkActions[FRemoteExActionIndex].ActionStatus := asInProgress;

        if CurrentNode <> nil then
          vstActions.RepaintNode(CurrentNode);

        FRemoteExCmdResult := ExecuteActionAtIndex(FRemoteExActionIndex);
      except
        on E: Exception do
          raise Exception.Create(E.Message + '  in ExecuteActionAtIndex(' + IntToStr(FRemoteExActionIndex) +')');
      end;
    except
      on E: Exception do
      begin
        SetActionVarValue('$Exception$', E.Message);
        FRemoteExCmdResult := False;
      end;
    end;

    /////////////////////////////////////////////  copied from local execution
    if FRemoteExCmdResult then
    begin
      if FClkActions[FRemoteExActionIndex].ActionOptions.Action <> acCallTemplate then
        FClkActions[FRemoteExActionIndex].ActionStatus := asSuccessful
      else
        FClkActions[FRemoteExActionIndex].ActionStatus := ActionStatusStrToActionStatus(GetActionVarValue('$LastAction_Status$'));  ///////////// not sure if this is the way to go
    end
    else
    begin
      if {((FClkActions[ActionIndex].ActionOptions.Action = acFindControl) or (FClkActions[ActionIndex].ActionOptions.Action = acFindSubControl)) and}
         not FRemoteExCmdResult and
         FClkActions[FRemoteExActionIndex].FindControlOptions.AllowToFail then
      begin
        FClkActions[FRemoteExActionIndex].ActionStatus := asAllowedFailed;
        FRemoteExCmdResult := True; //allow further execution
      end
      else
        FClkActions[FRemoteExActionIndex].ActionStatus := asFailed;
    end;

    SetActionVarValue('$LastAction_Status$', CActionStatusStr[FClkActions[FRemoteExActionIndex].ActionStatus]);
    ////////////////////////////////////////////////

    if CurrentNode <> nil then
      vstActions.RepaintNode(CurrentNode);
  finally
    AddToLog(DateTimeToStr(Now) + '  [Srv]: Done executing action at index ' + IntToStr(FRemoteExActionIndex));
    FExecutingActionFromRemote := False;
    FFileLocationOfDepsIsMem := False;  //reset here, because the server might switch back to local or client, which uses local files
  end;
end;


procedure TfrClickerActionsArr.HandleOnCopyControlTextAndClassFromMainWindow(ACompProvider: string; out AControlText, AControlClass: string);
begin
  if not Assigned(FOnCopyControlTextAndClassFromMainWindow) then
    raise Exception.Create('OnCopyControlTextAndClass not assigned for ' + Caption)
  else
    FOnCopyControlTextAndClassFromMainWindow(ACompProvider, AControlText, AControlClass);
end;


function TfrClickerActionsArr.HandleOnGetExtraSearchAreaDebuggingImage(AExtraBitmap: TBitmap): Boolean;
begin
  if not Assigned(FOnGetExtraSearchAreaDebuggingImageWithStackLevel) then
    Result := False
  else
    Result := FOnGetExtraSearchAreaDebuggingImageWithStackLevel(AExtraBitmap, FStackLevel);
end;


procedure TfrClickerActionsArr.AddToLog(s: string);
begin
  FLoggingFIFO.Put(s);
end;


procedure TfrClickerActionsArr.tmrLoggingTimer(Sender: TObject);
var
  TempStrings: TStringList;
  i: Integer;
begin
  //FLoggingFIFO.PopAll(memLogErr.Lines); //this resets the memo view to the first line

  try
    TempStrings := TStringList.Create;
    try
      FLoggingFIFO.PopAll(TempStrings);

      for i:= 0 to TempStrings.Count - 1 do
        memLogErr.Lines.Add(TempStrings[i]);  //adding lines, one by one, instead of calling AddStrings, to leave the focus to the last line
    finally
      TempStrings.Free;
    end;
  except
    on E: Exception do
      memLogErr.Lines.Add('Exception on adding to log: ' + E.Message);
  end;
end;

end.
