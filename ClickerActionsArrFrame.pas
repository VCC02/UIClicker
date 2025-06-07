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


unit ClickerActionsArrFrame;

{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF Windows}
    Windows, ActiveX,
  {$ELSE}
    LCLIntf, LCLType, FakeActiveX,
  {$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Types,
  Dialogs, ClickerActionsFrame, StdCtrls, VirtualTrees, ExtCtrls, Buttons, IniFiles,
  ImgList, Menus, ComCtrls, IdHTTP, ClickerIniFiles, ClickerUtils, InMemFileSystem,
  ClickerActionsPaletteFrame, ClickerActionExecution, PollingFIFO, ClickerPrimitiveUtils;


{$IFnDEF Windows}
  {$UNDEF MemPlugins}
{$ENDIF}

type
  TOnExecuteRemoteActionAtIndex = function(AActionIndex, AStackLevel: Integer; AVarReplacements: TStringList; AIsDebugging: Boolean): Boolean of object;
  //TOnGetRemoteReplacementVars = procedure(AValLst: TValueListEditor) of object;
  TOnWaitForFileAvailability = procedure(AFileName: string) of object; //called in server mode, to add a filename to FIFO and wait until file exists
  TOnWaitForMultipleFilesAvailability = procedure(AListOfFiles: TStringList) of object;
  TOnOpenCalledTemplateInExperimentTab = procedure(AExperimentIndex: Integer; ATemplatePath: string) of object;

  TOnAddFileNameToRecent = procedure(AFileName: string) of object;
  TOnGetListOfRecentFiles = procedure(AList: TStringList) of object;

  TActionNodeRec = record
    Action: TClkActionRec;
    FullTemplatePath: string;
  end;

  PActionNodeRec = ^TActionNodeRec;


  //TActionPluginArr = array of TActionPlugin;   //for now, there will be no list of plugins


  { TfrClickerActionsArr }

  TfrClickerActionsArr = class(TFrame)
    chkDisplayCalledTemplates: TCheckBox;
    chkEnableDebuggerKeys: TCheckBox;
    chkResetVarsOnPlayAll: TCheckBox;
    chkShowActionNumber: TCheckBox;
    edtConsoleCommand: TEdit;
    imglstWaitingForFilesAvailability: TImageList;
    imgTemplateIcon: TImage;
    imgWaitingInDebuggingMode: TImage;
    imglstActionHasCondition: TImageList;
    imglstActionExtraStatus: TImageList;
    imglstCurrentDebuggingActionWithBreakPoint: TImageList;
    imglstCurrentDebuggingActionWithDisabledBreakPoint: TImageList;
    imgWaitingInPreDebuggingMode: TImage;
    imgWaitingForFilesAvailability: TImage;
    lbeSearchAction: TLabeledEdit;
    lblModifiedStatus: TLabel;
    memLogErr: TMemo;
    MenuItem_GetHTTPRequestFromActionAllPropertiesWithSrvDbg: TMenuItem;
    MenuItem_GetHTTPRequestFromActionModifiedOnlyWithSrvDbg: TMenuItem;
    MenuItem_GetHTTPRequestFromActionAllProperties: TMenuItem;
    MenuItem_GetHTTPRequestFromActionModifiedOnly: TMenuItem;
    MenuItem_GetHTTPRequestFromAction: TMenuItem;
    N5: TMenuItem;
    MenuItemPasteActionsFromClipboardAfterTheFirstSelected: TMenuItem;
    MenuItem_StopWaitingForFilesAvailability: TMenuItem;
    MenuItem_ResolvePathToAbsolute: TMenuItem;
    MenuItem_OpenCalledTemplateInExperimentTab: TMenuItem;
    MenuItem_InsertActionAfterFirstSelected: TMenuItem;
    MenuItem_InsertActionBeforeFirstSelected: TMenuItem;
    MenuItem_CopyFullFilepathToClipboard: TMenuItem;
    MenuItem_CopyFilenameToClipboard: TMenuItem;
    N4: TMenuItem;
    MenuItem_ReplaceWith_AppDir: TMenuItem;
    MenuItem_ReplaceWith_SelfTemplateDir: TMenuItem;
    MenuItem_ReplaceWith_TemplateDir: TMenuItem;
    MenuItem_BrowseTemplateIcon: TMenuItem;
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
    MenuItemPasteActionsFromClipboardAtTheEnd: TMenuItem;
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
    pmTemplateIcon: TPopupMenu;
    pmTemplateName: TPopupMenu;
    pmExtraStop: TPopupMenu;
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
    spdbtnExtraPlayAll1: TSpeedButton;
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
    tmrWaitingForFilesAvailability: TTimer;
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
    procedure chkDisplayCalledTemplatesChange(Sender: TObject);
    procedure edtConsoleCommandExit(Sender: TObject);
    procedure edtConsoleCommandKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtConsoleCommandKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FrameResize(Sender: TObject);
    procedure MenuItemCopySelectedActionsToClipboardClick(Sender: TObject);
    procedure MenuItemEnableDisableBreakPointClick(Sender: TObject);
    procedure MenuItemPasteActionsFromClipboardAfterTheFirstSelectedClick(
      Sender: TObject);
    procedure MenuItemPasteActionsFromClipboardAtTheEndClick(Sender: TObject);
    procedure MenuItem_AddACallTemplateByFileClick(Sender: TObject);
    procedure MenuItem_AddCacheControlActionClick(Sender: TObject);
    procedure MenuItem_AddRestoreCachedControlActionClick(Sender: TObject);
    procedure MenuItem_BrowseTemplateIconClick(Sender: TObject);
    procedure MenuItem_CopyFilenameToClipboardClick(Sender: TObject);
    procedure MenuItem_CopyFullFilepathToClipboardClick(Sender: TObject);
    procedure MenuItem_EditBreakPointClick(Sender: TObject);
    procedure MenuItem_GetGenericHTTPRequestFromActionClick(
      Sender: TObject);
    procedure MenuItem_GetHTTPRequestFromActionClick(Sender: TObject);
    procedure MenuItem_PlayActionAndRestoreVarsClick(Sender: TObject);
    procedure MenuItem_RefactorSelectedActionsIntoATemplateClick(Sender: TObject
      );
    procedure MenuItem_ReplaceSelectedActionsWithATemplateCallClick(Sender: TObject
      );
    procedure MenuItem_ReplaceWith_AppDirClick(Sender: TObject);
    procedure MenuItem_ReplaceWith_SelfTemplateDirClick(Sender: TObject);
    procedure MenuItem_ReplaceWith_TemplateDirClick(Sender: TObject);
    procedure MenuItem_ResolvePathToAbsoluteClick(Sender: TObject);
    procedure MenuItem_SetActionStatusToAllowedFailedClick(Sender: TObject);
    procedure MenuItem_SetActionStatusToFailedClick(Sender: TObject);
    procedure MenuItem_SetActionStatusToSuccessfulClick(Sender: TObject);
    procedure MenuItem_GenericInsertActionOnFirstSelectedClick(Sender: TObject);
    procedure MenuItem_GenericOpenCalledTemplateInExperimentTabClick(Sender: TObject);
    procedure MenuItem_StopWaitingForFilesAvailabilityClick(Sender: TObject);
    procedure pmVstActionsPopup(Sender: TObject);
    procedure pnlActionsClick(Sender: TObject);
    procedure pnlVertSplitterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlVertSplitterMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlVertSplitterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure spdbtnExtraLoadClick(Sender: TObject);
    procedure spdbtnExtraPlayActionClick(Sender: TObject);
    procedure spdbtnExtraPlayAll1Click(Sender: TObject);
    procedure spdbtnPaletteClick(Sender: TObject);
    procedure spdbtnTemplateNotesClick(Sender: TObject);
    procedure spdbtnUpdateActionClick(Sender: TObject);
    procedure tmrDeleteActionsTimer(Sender: TObject);
    procedure tmrEditActionsVSTTimer(Sender: TObject);
    procedure tmrExecActionFromSrvModuleTimer(Sender: TObject);
    procedure tmrLoggingTimer(Sender: TObject);
    procedure tmrWaitingForFilesAvailabilityTimer(Sender: TObject);
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
    procedure vstActionsChecking(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var NewState: TCheckState; var Allowed: boolean);
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
    procedure vstActionsDragAllowed(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstActionsDragOver(Sender: TBaseVirtualTree;
      Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
      Mode: TDropMode; var Effect: DWORD; var Accept: Boolean);
    procedure vstActionsDragDrop(Sender: TBaseVirtualTree;
      Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
      Shift: TShiftState; const Pt: TPoint; var Effect: DWORD; Mode: TDropMode);
    procedure chkEnableDebuggerKeysClick(Sender: TObject);
    procedure tmrDebugKeysTimer(Sender: TObject);
    procedure Removeallactionsandclearfilename1Click(Sender: TObject);
    procedure PlayAllInDebuggingModeStartingAtSelected1Click(Sender: TObject);
    procedure chkShowActionNumberClick(Sender: TObject);
  private
    { Private declarations }
    FClkActions: TClkActionsRecArr;
    FTemplateNotes: string;
    FTemplateIconPath: string;
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
    FAllowedFileDirsForServer: string;
    FAllowedFileExtensionsForServer: string;
    FCmdConsoleHistory: TStringList;

    FExecutesRemotely: Boolean;
    FStackLevel: Integer;
    FOnExecuteRemoteActionAtIndex: TOnExecuteRemoteActionAtIndex;
    //FOnGetRemoteReplacementVars: TOnGetRemoteReplacementVars;

    FRemoteExActionIndex: Integer;
    FRemoteExCmdResult: Boolean;
    FExecutingActionFromRemote: Boolean;   //Server mode
    FUseLocalDebugger: Boolean;
    FFileLocationOfDepsIsMem: Boolean;
    FClosingTemplate: Boolean;  //set to true by ExitTemplateFromRemote when the template should be closed (after it stays open as called template)
    FPluginStepOver: Boolean;
    FPluginContinueAll: Boolean;

    FRemoteAddress: string; //and port  //used as a client

    {$IFDEF MemPlugins}
      FMemPluginsInMemFS: TInMemFileSystem;
    {$ENDIF}

    FHold: Boolean; //for splitter
    FSplitterMouseDownImagePos: TPoint;
    FSplitterMouseDownGlobalPos: TPoint;

    FOnCopyControlTextAndClassFromMainWindow: TOnCopyControlTextAndClassFromMainWindow;
    FOnGetExtraSearchAreaDebuggingImageWithStackLevel: TOnGetExtraSearchAreaDebuggingImageWithStackLevel;
    FOnWaitForFileAvailability: TOnWaitForFileAvailability;
    FOnWaitForMultipleFilesAvailability: TOnWaitForMultipleFilesAvailability;
    FOnWaitForBitmapsAvailability: TOnWaitForBitmapsAvailability;
    FOnTerminateWaitForMultipleFilesAvailability: TOnTerminateWaitForMultipleFilesAvailability;
    FOnLoadBitmap: TOnLoadBitmap;
    FOnLoadRenderedBitmap: TOnLoadRenderedBitmap;
    FOnSaveRenderedBitmap: TOnSaveRenderedBitmap;
    FOnRenderBmpExternally: TOnRenderBmpExternally;
    FOnLoadRawPmtv: TOnLoadRawPmtv;
    FOnGetListOfExternallyRenderedImages: TOnGetListOfExternallyRenderedImages;

    {$IFDEF MemPlugins}
      FOnGetListOfInMemPlugins: TOnGetListOfInMemPlugins;
      FOnLoadPluginFromDiskToPluginInMemFileSystem: TOnLoadPluginFromDiskToPluginInMemFileSystem;
    {$ENDIF}

    FOnLoadPluginFromInMemFS: TOnLoadPluginFromInMemFS;
    FOnLoadPrimitivesFile: TOnLoadPrimitivesFile;
    FOnSavePrimitivesFile: TOnSavePrimitivesFile;

    FOnFileExists: TOnFileExists;
    FOnTClkIniReadonlyFileCreate: TOnTClkIniReadonlyFileCreate;
    FOnTClkIniFileCreate: TOnTClkIniFileCreate;
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
    FOnOpenCalledTemplateInExperimentTab: TOnOpenCalledTemplateInExperimentTab;
    FOnSaveFileToExtRenderingInMemFS: TOnSaveFileToExtRenderingInMemFS;

    FOnAddFileNameToRecent: TOnAddFileNameToRecent;
    FOnGetListOfRecentFiles: TOnGetListOfRecentFiles;
    FOnGenerateAndSaveTreeWithWinInterp: TOnGenerateAndSaveTreeWithWinInterp;
    FOnSetWinInterpOption: TOnSetWinInterpOption;

    FOnGetListeningPort: TOnGetListeningPort;

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
    procedure HandleOnAddDefaultFontProfile(var AFindSubControlOptions: TClkFindSubControlOptions; var AActionOptions: TClkActionOptions);
    function HandleOnGetGridDrawingOption: TDisplayGridLineOption;

    function HandleOnEditCallTemplateBreakCondition(var AActionCondition: string): Boolean;
    function HandleOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function HandleOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    procedure HandleOnSaveRenderedBitmap(ABitmap: TBitmap; AFileName: string);
    function HandleOnRenderBmpExternally(AFilename: string): string;
    function HandleOnLoadRawPmtv(APmtvFile: TMemoryStream; AFileName: string): Boolean;
    procedure HandleOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages: TStringList; Sender: TObject = nil);

    {$IFDEF MemPlugins}
      procedure HandleOnGetListOfInMemPlugins(AListOfInMemPlugins: TStringList);
      procedure HandleOnLoadPluginFromDiskToPluginInMemFileSystem(APluginPath: string);
    {$ENDIF}

    function HandleOnGetActionProperties(AActionName: string): PClkActionRec;

    function HandleOnLoadPluginFromInMemFS(APlugin: TMemoryStream; AFileName: string): Boolean;

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

    function HandleOnGetSetVarActionByName(var AClkSetVarOptions: TClkSetVarOptions; AActionName: string): Boolean;
    function HandleOnUpdateSetVarActionByName(AClkSetVarOptions: TClkSetVarOptions; AActionName: string): Boolean;
    function HandleOnTClkIniReadonlyFileCreate(AFileName: string): TClkIniReadonlyFile;
    function HandleOnTClkIniFileCreate(AFileName: string): TClkIniFile;
    procedure HandleOnSaveStringListToFile(AStringList: TStringList; const AFileName: string);
    function HandleOnExecuteActionByContent(var AAllActions: TClkActionsRecArr; AActionIndex: Integer): Boolean;
    function HandleOnLoadTemplateToActions(Fnm: string; var AActions: TClkActionsRecArr; AWhichTemplate: TEditTemplateWhichTemplate; out ANotes, AIconPath: string; AWaitForFileAvailability: Boolean = False): string;
    function HandleOnSaveCompleteTemplateToFile(Fnm: string; var AActions: TClkActionsRecArr;  AWhichTemplate: TEditTemplateWhichTemplate; ANotes, AIconPath: string; AUpdateUI, AShouldSaveSelfTemplate: Boolean): string;

    procedure HandleOnBackupVars(AAllVars: TStringList);
    procedure HandleOnGetListOfAvailableSetVarActions(AListOfSetVarActions: TStringList);
    procedure HandleOnGetListOfAvailableActions(AListOfSetVarActions: TStringList);
    function HandleOnExecuteActionByName(AActionName: string): Boolean;
    function HandleOnGetAllActions: PClkActionsRecArr;
    procedure HandleOnModifyPluginProperty(AAction: PClkActionRec);
    function HandleOnResolveTemplatePath(APath: string; ACustomSelfTemplateDir: string = ''; ACustomAppDir: string = ''): string;
    procedure HandleOnSaveFileToExtRenderingInMemFS(AFileName: string; AContent: Pointer; AFileSize: Int64);

    function HandleOnClickerSetVarFrame_OnGetSelfTemplatesDir: string;
    procedure HandleOnClickerSetVarFrame_OnShowAutoComplete(AEdit: TEdit);
    procedure HandleOnUpdateActionScrollIndex(AActionScrollIndex: string);
    function HandleOnGetLoadedTemplateFileName: string;
    procedure HandleOnChangeEditTemplateEditingActionType;

    procedure HandleOnSetDebugPoint(ADebugPoint: string);
    function HandleOnIsAtBreakPoint(ADebugPoint: string): Boolean;

    procedure HandleOnPluginDbgStop;
    procedure HandleOnPluginDbgContinueAll;
    procedure HandleOnPluginDbgStepOver;
    function HandleOnPluginDbgRequestLineNumber(out ALineContent, ADbgSymFile: string): Integer;
    procedure HandleOnPluginDbgSetBreakpoint(ALineIndex, ASelectedSourceFileIndex: Integer; AEnabled: Boolean);

    function HandleOnGenerateAndSaveTreeWithWinInterp(AHandle: THandle; ATreeFileName: string; AStep: Integer; AUseMouseSwipe: Boolean): Boolean;
    function HandleOnSetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue: string): Boolean;

    procedure HandleOnWaitInDebuggingMode(var ADebuggingAction: TClkActionRec; AActionAllowsSteppingInto: TAllowsSteppingInto);
    function HandleOnGetPluginInMemFS: TInMemFileSystem;

    function GetInMemFS: TInMemFileSystem;
    procedure SetInMemFS(Value: TInMemFileSystem);
    function GetExtRenderingInMemFS: TInMemFileSystem;
    procedure SetExtRenderingInMemFS(Value: TInMemFileSystem);

    procedure SetGridDrawingOption(Value: TDisplayGridLineOption);
    procedure SetPreviewSelectionColors(Value: TSelectionColors);

    procedure FillInWithAllVars(AListOfVars: TStringList);
    procedure FillInWithAllFuncs(AListOfVars: TStringList);
    procedure FillInVarAndFuncDescriptions;
    procedure CreateRemainingUIComponents;

    procedure SetFullTemplatesDir(Value: string);
    procedure SetModified(Value: Boolean);
    procedure UpdateModifiedLabel(AFileLocation: TFileLocation = flDisk);
    procedure SaveTemplateIfModified;
    procedure LoadTemplateIcon;
    procedure SetTemplateIconHint;
    procedure DrawDefaultTemplateIcon;
    procedure AddCalledTemplateToNode(ANode: PVirtualNode; ATemplateFileName, ATemplateDir, AAppDir: string; ARecursionLevel: Integer);

    procedure ResizeFrameSectionsBySplitter(NewTop: Integer);
    procedure ShowAutoCompleteWindow(AEdit: TEdit);

    procedure UpdateActionsArrFromControls(ActionIndex: Integer);
    procedure UpdateControlsFromActionsArr(ActionIndex: Integer);
    procedure SetActionToDefault(var AAction: TClkActionRec; ANewActionType: TClkAction);
    procedure OverwriteActionAtIndexWithDefault(AIndex: Integer; ANewActionType: TClkAction);

    procedure FPaletteVstMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FPaletteVstMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FPaletteVsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure ExtraLoadInMemFileClick(Sender: TObject);
    procedure ExtraLoadRecentFileClick(Sender: TObject);

    procedure UpdateNodesCheckStateFromActions;
    procedure RemoveAction(ActionIndex: Integer; AClearSelectionAfterRemoving: Boolean = True; AUpdateRootNodeCount: Boolean = True);
    procedure RemoveSelectedActions;
    procedure SetAFontFromClkActions(AFont: TFont; ActionIndex: Integer);
    procedure UpdateNodeCheckStateFromAction(Node: PVirtualNode);

    procedure SetActionPropertiesFromPlugin(var AAction: TClkActionRec);
    procedure SetPropertiesFromPlugins;

    procedure LoadTemplate_V1(Ini: TClkIniReadonlyFile);
    procedure LoadTemplate_V2(Ini: TClkIniReadonlyFile);
    procedure SaveTemplateWithCustomActions_V2(Fnm: string; var ACustomClkActions: TClkActionsRecArr; ANotes, ATemplateIconPath: string);
    procedure SaveTemplate(Fnm: string);

    procedure DisplayDefaultEvalConsoleEditBox;
    function EvaluateAssignmentExpression: Boolean;
    function EvaluateActionCondition(ActionIndex: Integer): Boolean;
    function EncodeTemplatePath(APath: string): string;

    procedure PrepareFilesInServer;
    function ExecuteActionByContent(var AAllActions: TClkActionsRecArr; AActionIndex: Integer): Boolean;
    function ExecuteActionAtIndex(AActionIndex: Integer): Boolean; //can be called by a server module (used when Clicker is in server mode)
    function LocalOnExecuteRemoteActionAtIndex(AActionIndex, AStackLevel: Integer; AVarReplacements: TStringList; AIsDebugging: Boolean): Boolean;
    function DoExecuteRemoteActionAtIndex(AActionIndex: Integer): Boolean;
    procedure DoWaitForFileAvailability(AFileName: string);
    procedure DoWaitForMultipleFilesAvailability(AListOfFiles: TStringList);
    procedure DoWaitForBitmapsAvailability(AListOfFiles: TStringList);
    procedure DoOnTerminateWaitForMultipleFilesAvailability;
    function DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function DoOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    procedure DoOnSaveRenderedBitmap(ABitmap: TBitmap; AFileName: string);
    function DoOnRenderBmpExternally(AFilename: string): string;
    function DoOnLoadRawPmtv(APmtvFile: TMemoryStream; AFileName: string): Boolean;
    procedure DoOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages: TStringList);

    {$IFDEF MemPlugins}
      procedure DoOnGetListOfInMemPlugins(AListOfInMemPlugins: TStringList);
      procedure DoOnLoadPluginFromDiskToPluginInMemFileSystem(APluginPath: string);
    {$ENDIF}

    function DoOnLoadPluginFromInMemFS(APlugin: TMemoryStream; AFileName: string): Boolean;

    procedure DoOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    procedure DoOnSavePrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);

    function DoOnFileExists(const AFileName: string): Boolean;
    function DoOnTClkIniReadonlyFileCreate(AFileName: string): TClkIniReadonlyFile;
    function DoOnTClkIniFileCreate(AFileName: string): TClkIniFile;
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
    procedure DoOnOpenCalledTemplateInExperimentTab(AExperimentIndex: Integer; ATemplatePath: string);
    procedure DoOnSaveFileToExtRenderingInMemFS(AFileName: string; AContent: Pointer; AFileSize: Int64);

    procedure DoOnUpdatePropertyIcons(AStreamContent: Pointer; AStreamSize: Int64);

    procedure DoOnAddFileNameToRecent(AFileName: string);
    procedure DoOnGetListOfRecentFiles(AList: TStringList);
    function DoOnGenerateAndSaveTreeWithWinInterp(AHandle: THandle; ATreeFileName: string; AStep: Integer; AUseMouseSwipe: Boolean): Boolean;
    function DoOnSetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue: string): Boolean;

    function DoOnGetListeningPort: Word;

    function PlayActionByNode(Node: PVirtualNode): Boolean;
    procedure PlaySelected;
    procedure PlayAllActionsFromButton(IsDebugging: Boolean = False; StartAtSelected: Boolean = False);
    procedure PlaySelectedActionFromButton;
    procedure StopAllActionsFromButton;

    procedure MoveAction(ASrcNode, ADestNode: PVirtualNode);
    function ValidActionToBeAdded: Boolean;


    procedure CopySelectedActionsToClipboard;
    procedure PasteActionsFromClipboard(APasteIndex: Integer);

    function EvaluateReplacements(s: string; Recursive: Boolean = True; AEvalTextCount: Integer = -1): string;
    function EvaluateHTTP(AValue: string): string;

    function GetNodeByIndex(ANodeIndex: Integer): PVirtualNode;
    procedure HighlightCurrentlyExecutedAction(Node: PVirtualNode);

    procedure ClickerActionsFrameOnControlsModified(Sender: TObject);
    procedure StopGlowingUpdateButton;
    procedure ClearAllActions;

    procedure InsertCallTemplateForActionReplacing(AIndexToInsertAt: Integer; ATemplateFileName: string);
    procedure InsertSetVar(AIndexToInsertAt: Integer; AActionName: string; var ANewAction: TClkSetVarOptions);
    procedure InsertAction(AIndexToInsertAt: Integer; var ANewAction: TClkActionRec);

    procedure GetSelectedActions(var ActionsToCopy: TClkActionsRecArr); overload;
    function GetSelectedActions(var AIndexArr: TIntegerDynArray; var AActionsArr: TClkActionsRecArr): Integer; overload;

    procedure LoadActionIntoEditorByIndex(AIndex: Integer);
    procedure ReplaceSelectedActionsWithCallTemplate(var AActionIndexArrToReplace: TIntegerDynArray; AFirstSelectedIndex: Integer; ACallTemplateFileName: string);

    procedure HandleOnCopyControlTextAndClassFromMainWindow(ACompProvider: string; out AControlText, AControlClass: string);
    function HandleOnGetExtraSearchAreaDebuggingImage(AExtraBitmap: TBitmap): Boolean;

    procedure ToggleBreakpoint(ACurrentAction: PClkActionRec);
  public
    { Public declarations }
    frClickerActions: TfrClickerActions;  //used from outside

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadSettings(AIni: TMemIniFile; ASection, AIndentSuffix: string);
    procedure SaveSettings(AIni: TMemIniFile; ASection, AIndentSuffix: string);

    function CreateIniReadonlyFileFromInMemFileSystem(AFnm: string; AInMemFileSystem: TInMemFileSystem): TClkIniReadonlyFile;
    procedure LoadTemplate(Fnm: string; AFileLocation: TFileLocation = flDisk; AInMemFileSystem: TInMemFileSystem = nil);
    function LoadTemplateToActions(Fnm: string; var AActions: TClkActionsRecArr; out ANotes, AIconPath: string; AFileLocation: TFileLocation = flDisk; AInMemFileSystem: TInMemFileSystem = nil; AWaitForFileAvailability: Boolean = False): string;
    procedure LoadTemplateWithUIUpdate(AFileName: string; AFileLocation: TFileLocation = flDisk; AInMemFileSystem: TInMemFileSystem = nil);
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
    function ResolveTemplatePath(APath: string; ACustomSelfTemplateDir: string = ''; ACustomAppDir: string = ''): string;

    procedure InitFrame;

    property Modified: Boolean read FModified write SetModified;
    property FileName: string read FFileName write FFileName;
    property StopAllActionsOnDemand: Boolean read FStopAllActionsOnDemand write FStopAllActionsOnDemand;
    property StopAllActionsOnDemandFromParent: PBoolean read FStopAllActionsOnDemandFromParent write FStopAllActionsOnDemandFromParent;
    //property Debugging: Boolean read FDebugging write FDebugging;
    property FullTemplatesDir: string read FFullTemplatesDir write SetFullTemplatesDir;  //no trailing backslash
    property AllowedFileDirsForServer: string write FAllowedFileDirsForServer;
    property AllowedFileExtensionsForServer: string write FAllowedFileExtensionsForServer;

    property ExecutesRemotely: Boolean read FExecutesRemotely write FExecutesRemotely;  //used in client mode
    property StackLevel: Integer read FStackLevel write FStackLevel;
    property RemoteExActionIndex: Integer read FRemoteExActionIndex write FRemoteExActionIndex;
    property RemoteExCmdResult: Boolean read FRemoteExCmdResult write FRemoteExCmdResult;
    property ExecutingActionFromRemote: Boolean read FExecutingActionFromRemote write FExecutingActionFromRemote; //used in server mode
    property UseLocalDebugger: Boolean read FUseLocalDebugger write FUseLocalDebugger;
    property FileLocationOfDepsIsMem: Boolean read FFileLocationOfDepsIsMem write FFileLocationOfDepsIsMem;
    property RemoteAddress: string read FRemoteAddress write FRemoteAddress;

    property InMemFS: TInMemFileSystem read GetInMemFS write SetInMemFS;
    property ExtRenderingInMemFS: TInMemFileSystem read GetExtRenderingInMemFS write SetExtRenderingInMemFS;
    {$IFDEF MemPlugins}
      property MemPluginsInMemFS: TInMemFileSystem read FMemPluginsInMemFS write FMemPluginsInMemFS;
    {$ENDIF}

    property ActionExecution: TActionExecution read FActionExecution;
    property ShouldStopAtBreakPoint: Boolean {read FShouldStopAtBreakPoint} write FShouldStopAtBreakPoint;

    property GridDrawingOption: TDisplayGridLineOption write SetGridDrawingOption;
    property PreviewSelectionColors: TSelectionColors write SetPreviewSelectionColors;
    property PlayingAllActions: Boolean read FPlayingAllActions;

    property PluginContinueAll: Boolean write FPluginContinueAll;
    property PluginStepOver: Boolean write FPluginStepOver;

    property OnCallTemplate: TOnCallTemplate read FOnCallTemplate write FOnCallTemplate;
    property OnExecuteRemoteActionAtIndex: TOnExecuteRemoteActionAtIndex read FOnExecuteRemoteActionAtIndex write FOnExecuteRemoteActionAtIndex;
    property OnCopyControlTextAndClassFromMainWindow: TOnCopyControlTextAndClassFromMainWindow read FOnCopyControlTextAndClassFromMainWindow write FOnCopyControlTextAndClassFromMainWindow;
    property OnGetExtraSearchAreaDebuggingImageWithStackLevel: TOnGetExtraSearchAreaDebuggingImageWithStackLevel write FOnGetExtraSearchAreaDebuggingImageWithStackLevel;

    property OnWaitForFileAvailability: TOnWaitForFileAvailability read FOnWaitForFileAvailability write FOnWaitForFileAvailability;
    property OnWaitForMultipleFilesAvailability: TOnWaitForMultipleFilesAvailability read FOnWaitForMultipleFilesAvailability write FOnWaitForMultipleFilesAvailability;
    property OnWaitForBitmapsAvailability: TOnWaitForBitmapsAvailability read FOnWaitForBitmapsAvailability write FOnWaitForBitmapsAvailability;
    property OnTerminateWaitForMultipleFilesAvailability: TOnTerminateWaitForMultipleFilesAvailability write FOnTerminateWaitForMultipleFilesAvailability;
    property OnLoadBitmap: TOnLoadBitmap read FOnLoadBitmap write FOnLoadBitmap;
    property OnLoadRenderedBitmap: TOnLoadRenderedBitmap read FOnLoadRenderedBitmap write FOnLoadRenderedBitmap;
    property OnSaveRenderedBitmap: TOnSaveRenderedBitmap write FOnSaveRenderedBitmap;
    property OnRenderBmpExternally: TOnRenderBmpExternally read FOnRenderBmpExternally write FOnRenderBmpExternally;
    property OnLoadRawPmtv: TOnLoadRawPmtv read FOnLoadRawPmtv write FOnLoadRawPmtv;
    property OnGetListOfExternallyRenderedImages: TOnGetListOfExternallyRenderedImages write FOnGetListOfExternallyRenderedImages;

    {$IFDEF MemPlugins}
      property OnGetListOfInMemPlugins: TOnGetListOfInMemPlugins read FOnGetListOfInMemPlugins write FOnGetListOfInMemPlugins;
      property OnLoadPluginFromDiskToPluginInMemFileSystem: TOnLoadPluginFromDiskToPluginInMemFileSystem read FOnLoadPluginFromDiskToPluginInMemFileSystem write FOnLoadPluginFromDiskToPluginInMemFileSystem;
    {$ENDIF}

    property OnLoadPluginFromInMemFS: TOnLoadPluginFromInMemFS read FOnLoadPluginFromInMemFS write FOnLoadPluginFromInMemFS;

    property OnLoadPrimitivesFile: TOnLoadPrimitivesFile write FOnLoadPrimitivesFile;
    property OnSavePrimitivesFile: TOnSavePrimitivesFile write FOnSavePrimitivesFile;

    property OnFileExists: TOnFileExists write FOnFileExists;
    property OnTClkIniReadonlyFileCreate: TOnTClkIniReadonlyFileCreate write FOnTClkIniReadonlyFileCreate;
    property OnTClkIniFileCreate: TOnTClkIniFileCreate write FOnTClkIniFileCreate;
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
    property OnOpenCalledTemplateInExperimentTab: TOnOpenCalledTemplateInExperimentTab write FOnOpenCalledTemplateInExperimentTab;
    property OnSaveFileToExtRenderingInMemFS: TOnSaveFileToExtRenderingInMemFS write FOnSaveFileToExtRenderingInMemFS;

    property OnAddFileNameToRecent: TOnAddFileNameToRecent write FOnAddFileNameToRecent;
    property OnGetListOfRecentFiles: TOnGetListOfRecentFiles write FOnGetListOfRecentFiles;
    property OnGenerateAndSaveTreeWithWinInterp: TOnGenerateAndSaveTreeWithWinInterp write FOnGenerateAndSaveTreeWithWinInterp;
    property OnSetWinInterpOption: TOnSetWinInterpOption write FOnSetWinInterpOption;

    property OnGetListeningPort: TOnGetListeningPort write FOnGetListeningPort;
  end;


implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.frm}
{$ENDIF}


uses
  Math, ClickerTemplates, BitmapConv, BitmapProcessing, Clipbrd,
  ClickerConditionEditorForm, ClickerActionsClient, ClickerFileProviderUtils,
  ClickerTemplateNotesForm, AutoCompleteForm, ClickerVstUtils,
  ClickerActionPluginLoader, ClickerActionPlugins, ClickerActionProperties;


const
  CInsertActionOffset = 30; //this value has to be greater than the current available action types High(TClkAction)
  CErr_CannotSelectActionToLoadValuesAtIndex = 'Cannot select action, to load values at index';
  CErr_EmptyTemplate = 'empty template';


procedure TfrClickerActionsArr.FillInWithAllVars(AListOfVars: TStringList);
const
  CMissingVars: array[0..13] of string = (
    '$ExitCode$',
    '$frmUIClickerMainForm_Handle$',
    '$frmClickerControlPreview_Handle$',
    '$frmClickerActions_Handle$',
    '$frmClickerWinInterp_Handle$',
    '$frmClickerTemplateCallTree_Handle$',
    '$frmClickerRemoteScreen_Handle$',
    '$frmClickerZoomPreview_Handle$',
    '$ImageWidth$',
    '$ImageHeight$',
    '$ExtImageWidth$',
    '$ExtImageHeight$',
    '$SelfActionName$',
    '$SelfActionIndex$'
  );
var
  i: Integer;
begin
  AListOfVars.AddStrings(frClickerActions.ClkVariables);
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
    TempVarDescriptions.LineBreak := #13#10;
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
    TempVarDescriptions.Add('$PluginError$=[String] This is an error message, set by an action plugin, after its failed execution.');
    TempVarDescriptions.Add('$AppBitness$=[String] This variable is automatically set to i386 or x86_64, depending on executable bitness.');
    TempVarDescriptions.Add('$OSBitness$=[String] This variable is automatically set to win32 or win64, depending on OS bitness.');
    TempVarDescriptions.Add('$ResultedErrorCount$=[String] This is the numer of mismatching pixels, after executing a FindSubControl action.');
    TempVarDescriptions.Add('$ImageWidth$=[Numeric] This is the width of a bitmap, loaded with $GetImageDimensions()$ function.');
    TempVarDescriptions.Add('$ImageHeight$=[Numeric] This is the height of a bitmap, loaded with $GetImageDimensions()$ function.');
    TempVarDescriptions.Add('$ExtImageWidth$=[Numeric] This is the width of a bitmap, loaded with GetExternallyRenderedImageDimensions()$ function.');
    TempVarDescriptions.Add('$ExtImageHeight$=[Numeric] This is the height of a bitmap, loaded with GetExternallyRenderedImageDimensions()$ function.');
    TempVarDescriptions.Add('$SelfActionName$=[String] Name of the action, which is currently being executed. A template can have multiple actions with the same name, so make sure they are uniquely named for a proper identification.');
    TempVarDescriptions.Add('$SelfActionIndex$=[Numeric] Index of the action, which is currently being executed.');
    TempVarDescriptions.Add('$PluginPath$=[String] Full path to the plugin, which is currently being executed.');

    for i := 0 to FVarDescriptions.Count - 1 do
      FVarDescriptions.Strings[i] := TempVarDescriptions.Values[FVarDescriptions.Strings[i]];
  finally
    TempVarDescriptions.Free;
  end;

  TempFuncDescriptions := TStringList.Create;
  try
    TempFuncDescriptions.LineBreak := #13#10;
    FillInWithAllFuncs(FFuncDescriptions);

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
    TempFuncDescriptions.Add('$FastReplace_45To68(<some_string>)$=Replaces all #4#5 (ASCII_4 and ASCII_5) occurrences with #6#8 (ASCII_6 and ASCII_8). Returns the result.');
    TempFuncDescriptions.Add('$FastReplace_68To45(<some_string>)$=Replaces all #6#8 (ASCII_6 and ASCII_8) occurrences with #4#5 (ASCII_4 and ASCII_5). Returns the result.');
    TempFuncDescriptions.Add('$StringReplace(<StringToBeSearchedFrom>~^~<OldSubString>~^~<NewSubString>)$=Replaces all occurrences of <OldSubString>, from <StringToBeSearchedFrom>, with <NewSubString>. Returns the result. The argument separator is a "~^~" string, to allow parsing strings, which contain commas.');
    TempFuncDescriptions.Add('$Exit(<ExitCode>)$=Stops current template execution. If the passed ExitCode argument is 0, the template stops with a "Successful" status, otherwise with "Failed". Returns nothing. Sets the $ExitCode$ variable to the passed argument. It can be executed by SetVar action and must be placed in its "Variable" column.');
    TempFuncDescriptions.Add('$CreateDir(<PathToNewDir>)$=Creates a directory (and its parent directories, if required) by the current path.');
    TempFuncDescriptions.Add('$LoadTextFile(<PathToTextFile>)$=Loads a text file (CRLF-separated lines) and returns its content as #4#5 separated strings.');
    TempFuncDescriptions.Add('$ItemCount($TextFileContent$)$=Returns the number of items in a #4#5-separated list of strings (as returned by $LoadTextFile()$. Even the last item has to be terminated by #4#5.');
    TempFuncDescriptions.Add('$GetTextItem($TextFileContent$,<ItemIndex>)$=Returns the item by index, in a #4#5-separated list of strings (as returned by $LoadTextFile()$. Even the last item has to be terminated by #4#5.');
    TempFuncDescriptions.Add('$IndexOfTextItem($TextFileContent$,<Item>)$=Returns the item index of the given item, in a #4#5-separated list of strings (as returned by $LoadTextFile()$.');
    TempFuncDescriptions.Add('$Str0(<some_string>,<CharacterIndex>)$=Returns the character at the given index, in a string. The index is 0-based (e.g. $Str0(ABC,1)$ returns B.');
    TempFuncDescriptions.Add('$Str1(<some_string>,<CharacterIndex>)$=Returns the character at the given index, in a string. The index is 1-based (e.g. $Str1(ABC,1)$ returns A.');
    TempFuncDescriptions.Add('$StrLen(<some_string>)$=Returns the number of characters in a string.');
    TempFuncDescriptions.Add('$StringContains(<SubString>, <String>)$=Returns 1, if <SubString> is part of <String>. Otherwise, it returns 0.');
    TempFuncDescriptions.Add('$ExtractFileDir(<DirName>)$=Returns the directory up to and not including the last item in the path.');
    TempFuncDescriptions.Add('$ExtractFilePath(<DirName>)$=Returns the directory up to and not including the last item in the path. It is terminated by a "' + PathDelim + '".');
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
    TempFuncDescriptions.Add('$Current_Mouse_hCursor$=Returns the current mouse cursor handle. Some applictions may register their own cursors which will get dynamic handles.');
    TempFuncDescriptions.Add('$CRLF$=Returns a CRLF sequence.');
    TempFuncDescriptions.Add('$#4#5$=Returns an ASCII #4#5 sequence.');
    TempFuncDescriptions.Add('$#6#8$=Returns an ASCII #6#8 sequence.');
    TempFuncDescriptions.Add('$Now$=Returns current datetime.');
    TempFuncDescriptions.Add('$RenderBmpExternally()$=Sends an http request to a server, for rendering a bitmap, using the supplied list of parameters (from right column of a SetVar action). These parameters are encoded as a "$#4#5$" separated <key>eq<value> strings. Notice the use of "$#4#5$" variable, not its actual value. The required parameters are (without quotes): "' + CExtBmp_SrvAddrPort + '", "' + CExtBmp_Cmd + '", "' + CExtBmp_Filename + '". The optional parameters are (without quotes): "' + CExtBmp_Params + '" and "' + CExtBmp_IncludeFilenameInRequest + '". When provided, "' + CExtBmp_Params + '" are "&"-separated key%3Dvalue pairs. When "' + CExtBmp_IncludeFilenameInRequest + '" is 1, the filename is added to request. The result is placed in $ExternallyRenderedBmpResult$ variable. If successful, the result is set to empty string, otherwise it is set to an error message. The received bitmap is "stored" in an in-mem file system. The function must be called from the left column of SetVar action. Argument example: SrvAddrPort=http://127.0.0.1:53444$#4#5$Cmd=GetGradientImage$#4#5$Filename=I:\TheResult.bmp$#4#5$Params=IncludeTimestamp%3DYes&TextCount%3D1');
    TempFuncDescriptions.Add('$GetActionProperties()$=Sets the $ActionPropertiesResult$ variable to an &-separated list of action properties and their values, from the current template. The action is identified by name, and this name has to be provided in the right column of a SetVar action. If the action is not found by name (which is case sensitive), the result is set to an error message. The function must be called from the left column.');
    TempFuncDescriptions.Add('$GetImageDimensions()$=Loads a bitmap from disk or the server''s in-mem file system (depending on operation mode) and sets the $ImageWidth$ and $ImageHeight$ variables to the width and height of the loaded image. The function should be called from a SetVar action and the file name has to be in its right column. If the file does not exist, then $ExecAction_Err$ is set to an error message. When FailOnException property is True, and the file is not found, the action fails.');
    TempFuncDescriptions.Add('$GetExternallyRenderedImageDimensions()$=Loads a bitmap from the externally rendered in-mem file system and sets the $ExtImageWidth$ and $ExtImageHeight$ variables to the width and height of the loaded image. The function should be called from a SetVar action and the file name has to be in its right column. If the file does not exist, then $ExecAction_Err$ is set to an error message. When FailOnException property is True, and the file is not found, the action fails.');
    TempFuncDescriptions.Add('$FullHistogramDisk()$=Loads a bitmap from disk and counts the number of pixels for each unique color, then sets the $HistogramResult$ variable to a #4#5-separated list of 6-digit hex values and $HistogramColorCountsResult$ to their color counts. The function should be called from a SetVar action and the file name has to be in its right column. If the file does not exist, then $ExecAction_Err$ is set to an error message. When FailOnException property is True, and the file is not found, the action fails. $ItemCount($HistogramResult$)$ can be used to count the items.');
    TempFuncDescriptions.Add('$FullHistogramExternallyRendered()$=Loads a bitmap from the externally rendered in-mem file system and counts the number of pixels for each unique color, then sets the $HistogramResult$ variable to a #4#5-separated list of 6-digit hex values and $HistogramColorCountsResult$ to their color counts. The function should be called from a SetVar action and the file name has to be in its right column. If the file does not exist, then $ExecAction_Err$ is set to an error message. When FailOnException property is True, and the file is not found, the action fails. $ItemCount($HistogramResult$)$ can be used to count the items.');
    TempFuncDescriptions.Add('$PartialHistogramDisk()$=Loads a bitmap from disk and counts the number of pixels for each unique color, then sets the $HistogramResult$ variable to a #4#5-separated list of 6-digit hex values and $HistogramColorCountsResult$ to their color counts. The number of items is limited to the value of $HistogramItemCount$ variable. The function should be called from a SetVar action and the file name has to be in its right column. If the file does not exist, then $ExecAction_Err$ is set to an error message. When FailOnException property is True, and the file is not found, the action fails.');
    TempFuncDescriptions.Add('$PartialHistogramExternallyRendered()$=Loads a bitmap from the externally rendered in-mem file system and counts the number of pixels for each unique color, then sets the $HistogramResult$ variable to a #4#5-separated list of 6-digit hex values and $HistogramColorCountsResult$ to their color counts. The number of items is limited to the value of $HistogramItemCount$ variable. The function should be called from a SetVar action and the file name has to be in its right column. If the file does not exist, then $ExecAction_Err$ is set to an error message. When FailOnException property is True, and the file is not found, the action fails.');
    TempFuncDescriptions.Add('$GetWindowLongPtr(<ControlHandle>, <Index>)$=Returns info about a control, specified by its handle and the info index. For details, see MSDN. Examples for index values are -4, for WNDPROC, -6 for HINSTANCE, -8 for HWNDPARENT, -21 for USERDATA, -12 for ID etc.');
    TempFuncDescriptions.Add('$GetWindowProcessId(<ControlHandle>)$=Returns the process ID, which owns the control, specified by the handle. Most of the time, the handle belongs to a window. Any other control from that window should return the same process ID. If the handle is invalid, the function returns 0 and sets the $ExecAction_Err$ variable to "Invalid window handle.".');
    TempFuncDescriptions.Add('$GenerateAndSaveTree(<TreePath>[,<Step>[,<UseMouseSwipe>]])$=Calls the built-in WinInterpreter scan function, identified by $Control_Handle$, to generate the tree of controls. Then, the tree is saved to file (provided by <TreePath>) and also it is accompanied by its screenshots in the same directory. Step, if provided should resolve to an integer, greater than 1, which sets the step in pixels for the scanning accuracy. UseMouseSwipe, if provided and is either 1 or True, will use the "MouseSwipe" scanning algorithm (see WinInterp window), which is slower, but may find subcontrols.');
    TempFuncDescriptions.Add('$SetWinInterpOption(<WinInterpOptionName>, <WinInterpOptionValue>)$=Sets an option or executes a command on the WinInterp window. Most options are boolean values and they check or uncheck a checkbox on the window. Other options expect a numeric value (e.g. a color index or a radio button index). Available option names (case insensitive): ShowZoom, ContinuouslyScreenshotByKeys, RecordingStep, MouseCursorPosToScreenshotDelay, HighlightSelectedComponent, UseHCursor, FullScreenScanning, RecordSelectedAreaOnly, RecordWithEdgeExtending, MinimizeWhileRecording, BringTargetToFront, BringTargetToFrontPeriodically, HighlightingLabels, SelectedLayer, LoadTree, LoadAvoidedZones, SaveAvoidedZones, NewAvoidedZone, DeleteAvoidedZone, GetAvoidedZone, SetAvoidedZone, ClearAvoidedZone.');
    TempFuncDescriptions.Add('$Console([<Value>])$=Prints the evaluated value to log. This function must be called from the left column of a SetVar action and uses, with priority, the value from the right column. If that value is empty, it looks for an argument, like $Console(<argument>])$ and prints that on instead. It returns nothing.');
    TempFuncDescriptions.Add('$GetListOfFonts()$=Returns a #4#5-separated list of available (intalled) fonts names. This list may be different on another machine, where UIClicker is running in server mode.');

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

  {$IFDEF MemPlugins}
    frClickerActions.OnGetListOfInMemPlugins := HandleOnGetListOfInMemPlugins;
    frClickerActions.OnLoadPluginFromDiskToPluginInMemFileSystem := HandleOnLoadPluginFromDiskToPluginInMemFileSystem;
    frClickerActions.OnLoadPluginFromInMemFS := HandleOnLoadPluginFromInMemFS;
  {$ENDIF}

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

  frClickerActions.OnGetListOfAvailableSetVarActions := HandleOnGetListOfAvailableSetVarActions;
  frClickerActions.OnGetListOfAvailableActions := HandleOnGetListOfAvailableActions;
  frClickerActions.OnTClkIniReadonlyFileCreate := HandleOnTClkIniReadonlyFileCreate;
  frClickerActions.OnModifyPluginProperty := HandleOnModifyPluginProperty;

  frClickerActions.OnPluginDbgStop := HandleOnPluginDbgStop;
  frClickerActions.OnPluginDbgContinueAll := HandleOnPluginDbgContinueAll;
  frClickerActions.OnPluginDbgStepOver := HandleOnPluginDbgStepOver;
  frClickerActions.OnPluginDbgRequestLineNumber := HandleOnPluginDbgRequestLineNumber;
  frClickerActions.OnPluginDbgSetBreakpoint := HandleOnPluginDbgSetBreakpoint;
  frClickerActions.OnTClkIniFileCreate := HandleOnTClkIniFileCreate;

  frClickerActions.OnGetSelfTemplatesDir := HandleOnClickerSetVarFrame_OnGetSelfTemplatesDir;
  frClickerActions.OnShowAutoComplete := HandleOnClickerSetVarFrame_OnShowAutoComplete;
  frClickerActions.OnUpdateActionScrollIndex := HandleOnUpdateActionScrollIndex;
  frClickerActions.OnGetLoadedTemplateFileName := HandleOnGetLoadedTemplateFileName;
  frClickerActions.OnChangeEditTemplateEditingActionType := HandleOnChangeEditTemplateEditingActionType;

  //frClickerActions.OnControlsModified := ClickerActionsFrameOnControlsModified;   //this is set on frame initialization

  vstActions := TVirtualStringTree.Create(Self);
  vstActions.Parent := pnlActions;

  vstActions.Left := 3;
  vstActions.Top := 3;
  vstActions.Width := pnlvstActions.Width;
  vstActions.Height := pnlvstActions.Height;
  vstActions.CheckImageKind := ckXP;
  vstActions.DefaultNodeHeight := 26;
  vstActions.NodeDataSize := SizeOf(TActionNodeRec);
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
  //vstActions.Indent := 2;
  //vstActions.PopupMenu := pmVstActions; //the menu will be poped up by code
  vstActions.StateImages := frClickerActions.imglstActions;
  vstActions.TabOrder := 0;
  vstActions.TreeOptions.AutoOptions := [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoChangeScale, toDisableAutoscrollOnEdit];
  vstActions.TreeOptions.MiscOptions := [toAcceptOLEDrop, toEditable, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning];
  vstActions.TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toShowTreeLines, toUseBlendedImages];
  vstActions.TreeOptions.SelectionOptions := [toFullRowSelect, toMiddleClickSelect, {toRightClickSelect,} toMultiSelect];
  vstActions.OnBeforeCellPaint := vstActionsBeforeCellPaint;
  vstActions.OnChecking := vstActionsChecking;
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
  vstActions.OnDragAllowed := vstActionsDragAllowed;
  vstActions.OnDragOver := vstActionsDragOver;
  vstActions.OnDragDrop := vstActionsDragDrop;
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
  FVarDescriptions.LineBreak := #13#10;
  FFuncDescriptions.LineBreak := #13#10;
  //FillInVarDescriptions; called when displaying autocomplete
end;


constructor TfrClickerActionsArr.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLoggingFIFO := TPollingFIFO.Create;
  CreateRemainingUIComponents;

  FActionExecution := TActionExecution.Create;
  FActionExecution.ClickerVars := frClickerActions.ClkVariables;
  FActionExecution.StopAllActionsOnDemand := @FStopAllActionsOnDemand;
  FActionExecution.StopAllActionsOnDemandFromParent := FStopAllActionsOnDemandFromParent;
  FActionExecution.OnAddToLog := AddToLog;
  FActionExecution.SelfTemplateFileName := @FFileName;
  FActionExecution.ExecutingActionFromRemote := @FExecutingActionFromRemote;
  FActionExecution.FileLocationOfDepsIsMem := @FFileLocationOfDepsIsMem;
  FActionExecution.FullTemplatesDir := @FFullTemplatesDir;
  FActionExecution.StackLevel := @FStackLevel;
  FActionExecution.ExecutesRemotely := @FExecutesRemotely;
  FActionExecution.PluginStepOver := @FPluginStepOver;
  FActionExecution.PluginContinueAll := @FPluginContinueAll;
  FActionExecution.OwnerFrame := Self;
  FActionExecution.AllowedFileDirsForServer := @FAllowedFileDirsForServer;
  FActionExecution.AllowedFileExtensionsForServer := @FAllowedFileExtensionsForServer;
  FActionExecution.frClickerActions := frClickerActions;
  FActionExecution.OnSetEditorEnabledState := HandleOnSetEditorEnabledState;
  FActionExecution.OnSetEditorTimeoutProgressBarMax := HandleOnSetEditorTimeoutProgressBarMax;
  FActionExecution.OnSetEditorTimeoutProgressBarPosition := HandleOnSetEditorTimeoutProgressBarPosition;
  FActionExecution.OnWaitForBitmapsAvailability := HandleOnWaitForBitmapsAvailability;
  FActionExecution.OnLoadBitmap := HandleOnLoadBitmap; //both ActionExecution and frClickerActions use the same handler
  FActionExecution.OnLoadRenderedBitmap := HandleOnLoadRenderedBitmap;
  FActionExecution.OnSaveRenderedBitmap := HandleOnSaveRenderedBitmap;
  FActionExecution.OnRenderBmpExternally := HandleOnRenderBmpExternally;
  FActionExecution.OnLoadRawPmtv := HandleOnLoadRawPmtv;
  FActionExecution.OnLoadPluginFromInMemFS := HandleOnLoadPluginFromInMemFS;
  FActionExecution.OnGetActionProperties := HandleOnGetActionProperties;
  FActionExecution.OnCallTemplate := HandleOnCallTemplate;
  FActionExecution.OnSetEditorSleepProgressBarMax := HandleOnSetEditorSleepProgressBarMax;
  FActionExecution.OnSetEditorSleepProgressBarPosition := HandleOnSetEditorSleepProgressBarPosition;
  FActionExecution.OnSetEditorSleepInfo := HandleOnSetEditorSleepInfo;
  FActionExecution.OnAddDefaultFontProfile := HandleOnAddDefaultFontProfile;
  FActionExecution.OnGetGridDrawingOption := HandleOnGetGridDrawingOption;
  FActionExecution.OnLoadPrimitivesFile := HandleOnLoadPrimitivesFile;
  FActionExecution.OnGetSetVarActionByName := HandleOnGetSetVarActionByName;
  FActionExecution.OnUpdateSetVarActionByName := HandleOnUpdateSetVarActionByName;
  FActionExecution.OnTClkIniReadonlyFileCreate := HandleOnTClkIniReadonlyFileCreate;
  FActionExecution.OnSaveStringListToFile := HandleOnSaveStringListToFile;
  FActionExecution.OnBackupVars := HandleOnBackupVars;
  FActionExecution.OnExecuteActionByName := HandleOnExecuteActionByName;
  FActionExecution.OnGetAllActions := HandleOnGetAllActions;
  FActionExecution.OnResolveTemplatePath := HandleOnResolveTemplatePath;
  FActionExecution.OnSetDebugPoint := HandleOnSetDebugPoint;
  FActionExecution.OnIsAtBreakPoint := HandleOnIsAtBreakPoint;
  FActionExecution.OnSaveFileToExtRenderingInMemFS := HandleOnSaveFileToExtRenderingInMemFS;
  FActionExecution.OnGenerateAndSaveTreeWithWinInterp := HandleOnGenerateAndSaveTreeWithWinInterp;
  FActionExecution.OnSetWinInterpOption := HandleOnSetWinInterpOption;
  FActionExecution.OnFileExists := HandleOnFileExists;
  FActionExecution.OnSaveTemplateToFile := HandleOnSaveStringListToFile; //HandleOnSaveTemplateToFile;
  FActionExecution.OnExecuteActionByContent := HandleOnExecuteActionByContent;
  FActionExecution.OnLoadTemplateToActions := HandleOnLoadTemplateToActions;
  FActionExecution.OnSaveCompleteTemplateToFile := HandleOnSaveCompleteTemplateToFile;
  FActionExecution.OnWaitInDebuggingMode := HandleOnWaitInDebuggingMode;
  FActionExecution.OnGetPluginInMemFS := HandleOnGetPluginInMemFS;

  FCmdConsoleHistory := TStringList.Create;
  FCmdConsoleHistory.LineBreak := #13#10;
  FOnExecuteRemoteActionAtIndex := nil;
  FOnCopyControlTextAndClassFromMainWindow := nil;
  FOnGetExtraSearchAreaDebuggingImageWithStackLevel := nil;

  FOnWaitForFileAvailability := nil;
  FOnWaitForMultipleFilesAvailability := nil;
  FOnWaitForBitmapsAvailability := nil;
  FOnTerminateWaitForMultipleFilesAvailability := nil;

  FOnLoadBitmap := nil;
  FOnLoadRenderedBitmap := nil;
  FOnSaveRenderedBitmap := nil;
  FOnRenderBmpExternally := nil;
  FOnLoadRawPmtv := nil;
  FOnGetListOfExternallyRenderedImages := nil;

  {$IFDEF MemPlugins}
    FOnGetListOfInMemPlugins := nil;
    FOnLoadPluginFromDiskToPluginInMemFileSystem := nil;
  {$ENDIF}

  FOnLoadPluginFromInMemFS := nil;

  FOnLoadPrimitivesFile := nil;
  FOnSavePrimitivesFile := nil;

  FOnFileExists := nil;
  FOnTClkIniReadonlyFileCreate := nil;
  FOnTClkIniFileCreate := nil;
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
  FOnOpenCalledTemplateInExperimentTab := nil;
  FOnSaveFileToExtRenderingInMemFS := nil;

  FOnAddFileNameToRecent := nil;
  FOnGetListOfRecentFiles := nil;
  FOnGenerateAndSaveTreeWithWinInterp := nil;
  FOnSetWinInterpOption := nil;

  FOnGetListeningPort := nil;

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

  {$IFDEF MemPlugins}
    FMemPluginsInMemFS := nil;
  {$ENDIF}

  imgTemplateIcon.Picture.Bitmap.Width := imgTemplateIcon.Width;
  imgTemplateIcon.Picture.Bitmap.Height := imgTemplateIcon.Height;
  imgTemplateIcon.Picture.Bitmap.PixelFormat := pf24bit;

  DrawDefaultTemplateIcon;

  FEditingText := '';
  FPluginStepOver := False;
  FPluginContinueAll := False;
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


procedure TfrClickerActionsArr.LoadSettings(AIni: TMemIniFile; ASection, AIndentSuffix: string);
var
  i: Integer;
  Indent: string;
  SplitterTop, SplitterLeft: Integer;
begin
  for i := 0 to vstActions.Header.Columns.Count - 1 do
  begin
    Indent := 'ColWidth_' + IntToStr(i) + '.' + AIndentSuffix;
    vstActions.Header.Columns.Items[i].Width := AIni.ReadInteger(ASection, Indent, vstActions.Header.Columns.Items[i].Width);
  end;

  SplitterTop := AIni.ReadInteger(ASection, 'VertSplitterTop.' + AIndentSuffix, pnlVertSplitter.Top);
  ResizeFrameSectionsBySplitter(SplitterTop);

  SplitterLeft := AIni.ReadInteger(ASection, 'HorizSplitterLeft.' + AIndentSuffix, frClickerActions.pnlHorizSplitter.Left);
  frClickerActions.ResizeFrameSectionsBySplitter(SplitterLeft);

  SplitterLeft := AIni.ReadInteger(ASection, 'HorizSplitterLeftResults.' + AIndentSuffix, frClickerActions.pnlHorizSplitterResults.Left);
  frClickerActions.ResizeFrameSectionsBySplitterResults(SplitterLeft);

  frClickerActions.vstVariables.Header.Columns.Items[0].Width := AIni.ReadInteger(ASection, 'Variables_0.' + AIndentSuffix, frClickerActions.vstVariables.Header.Columns.Items[0].Width);
  frClickerActions.vstVariables.Header.Columns.Items[1].Width := AIni.ReadInteger(ASection, 'Variables_1.' + AIndentSuffix, frClickerActions.vstVariables.Header.Columns.Items[1].Width);

  for i := 0 to 20 - 1 do
  begin
    Indent := 'ActionCond.ColWidth_' + IntToStr(i) + '.' + AIndentSuffix;
    frClickerActions.frClickerConditionEditor.ColumnWidths[i] := AIni.ReadInteger(ASection, Indent, 100);
  end;
end;


procedure TfrClickerActionsArr.SaveSettings(AIni: TMemIniFile; ASection, AIndentSuffix: string);
var
  i: Integer;
  Indent: string;
begin
  for i := 0 to vstActions.Header.Columns.Count - 1 do
  begin
    Indent := 'ColWidth_' + IntToStr(i) + '.' + AIndentSuffix;
    AIni.WriteInteger(ASection, Indent, vstActions.Header.Columns.Items[i].Width);
  end;

  AIni.WriteInteger(ASection, 'VertSplitterTop.' + AIndentSuffix, pnlVertSplitter.Top);
  AIni.WriteInteger(ASection, 'HorizSplitterLeft.' + AIndentSuffix, frClickerActions.pnlHorizSplitter.Left);
  AIni.WriteInteger(ASection, 'HorizSplitterLeftResults.' + AIndentSuffix, frClickerActions.pnlHorizSplitterResults.Left);

  AIni.WriteInteger(ASection, 'Variables_0.' + AIndentSuffix, frClickerActions.vstVariables.Header.Columns.Items[0].Width);
  AIni.WriteInteger(ASection, 'Variables_1.' + AIndentSuffix, frClickerActions.vstVariables.Header.Columns.Items[1].Width);

  for i := 0 to 20 - 1 do
  begin
    Indent := 'ActionCond.ColWidth_' + IntToStr(i) + '.' + AIndentSuffix;
    AIni.WriteInteger(ASection, Indent, frClickerActions.frClickerConditionEditor.ColumnWidths[i]);
  end;
end;


procedure TfrClickerActionsArr.DrawDefaultTemplateIcon;
begin
  imgTemplateIcon.Picture.Bitmap.Canvas.Pen.Color := clGreen;
  imgTemplateIcon.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  imgTemplateIcon.Picture.Bitmap.Canvas.Rectangle(0, 0, imgTemplateIcon.Width, imgTemplateIcon.Height);
  imgTemplateIcon.Picture.Bitmap.Canvas.Font.Color := clGreen;
  imgTemplateIcon.Picture.Bitmap.Canvas.Font.Name := 'Tahoma';
  imgTemplateIcon.Picture.Bitmap.Canvas.Font.Size := 8;
  imgTemplateIcon.Picture.Bitmap.Canvas.Font.Quality := fqNonAntialiased;
  imgTemplateIcon.Picture.Bitmap.Canvas.TextOut(3, 3, 'Tmpl');
  imgTemplateIcon.Picture.Bitmap.Canvas.TextOut(3, 17, 'icon');
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
  AFileName := ResolveTemplatePath(AFileName); //////////////////// Added for plugin. Not sure how it affects unresolved path, which may be validated from allowed dirs.
  AFileName := EvaluateReplacements(AFileName);
  Result := DoOnLoadBitmap(ABitmap, AFileName);
end;


function TfrClickerActionsArr.HandleOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  AFileName := ResolveTemplatePath(AFileName); //////////////////// Added for plugin. Not sure how it affects unresolved path, which may be validated from allowed dirs.
  AFileName := EvaluateReplacements(AFileName);
  Result := DoOnLoadRenderedBitmap(ABitmap, AFileName);
end;


procedure TfrClickerActionsArr.HandleOnSaveRenderedBitmap(ABitmap: TBitmap; AFileName: string);
begin
  AFileName := ResolveTemplatePath(AFileName); //////////////////// Added for plugin. Not sure how it affects unresolved path, which may be validated from allowed dirs.
  //AFileName := EvaluateReplacements(AFileName);  //uncomment if really needed
  DoOnSaveRenderedBitmap(ABitmap, AFileName);
end;


function TfrClickerActionsArr.HandleOnRenderBmpExternally(AFilename: string): string;
begin
  Result := DoOnRenderBmpExternally(AFilename);
end;


function TfrClickerActionsArr.HandleOnLoadRawPmtv(APmtvFile: TMemoryStream; AFileName: string): Boolean;
begin
  AFileName := ResolveTemplatePath(AFileName); //////////////////// Added for plugin. Not sure how it affects unresolved path, which may be validated from allowed dirs.
  Result := DoOnLoadRawPmtv(APmtvFile, AFileName);
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


procedure TfrClickerActionsArr.HandleOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages: TStringList; Sender: TObject = nil);
begin
  DoOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages);
end;


{$IFDEF MemPlugins}
  procedure TfrClickerActionsArr.HandleOnGetListOfInMemPlugins(AListOfInMemPlugins: TStringList);
  begin
    DoOnGetListOfInMemPlugins(AListOfInMemPlugins);
  end;


  procedure TfrClickerActionsArr.HandleOnLoadPluginFromDiskToPluginInMemFileSystem(APluginPath: string);
  begin
    DoOnLoadPluginFromDiskToPluginInMemFileSystem(APluginPath);
  end;

{$ENDIF}


function TfrClickerActionsArr.HandleOnLoadPluginFromInMemFS(APlugin: TMemoryStream; AFileName: string): Boolean;
begin
  AFileName := ResolveTemplatePath(AFileName); //////////////////// Added for plugin. Not sure how it affects unresolved path, which may be validated from allowed dirs.
  Result := DoOnLoadPluginFromInMemFS(APlugin, AFileName);
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


procedure TfrClickerActionsArr.HandleOnAddDefaultFontProfile(var AFindSubControlOptions: TClkFindSubControlOptions; var AActionOptions: TClkActionOptions);
var
  ExecIdx: Integer;
begin
  //FindSubControl requires at least one font profile.
  //If the action is part of a list of actions, then the UI can be updated with a new profile.

  SetLength(AFindSubControlOptions.MatchBitmapText, 1);
  AFindSubControlOptions.MatchBitmapText[0].ForegroundColor := '$Color_Window$';
  AFindSubControlOptions.MatchBitmapText[0].BackgroundColor := '$Color_Highlight$';
  AFindSubControlOptions.MatchBitmapText[0].FontName := 'Tahoma';
  AFindSubControlOptions.MatchBitmapText[0].FontSize := 8;
  AFindSubControlOptions.MatchBitmapText[0].FontQualityReplacement := '';
  AFindSubControlOptions.MatchBitmapText[0].FontQuality := fqNonAntialiased;
  AFindSubControlOptions.MatchBitmapText[0].FontQualityUsesReplacement := False;
  AFindSubControlOptions.MatchBitmapText[0].Bold := False;
  AFindSubControlOptions.MatchBitmapText[0].Italic := False;
  AFindSubControlOptions.MatchBitmapText[0].Underline := False;
  AFindSubControlOptions.MatchBitmapText[0].StrikeOut := False;
  AFindSubControlOptions.MatchBitmapText[0].CropLeft := '0';
  AFindSubControlOptions.MatchBitmapText[0].CropTop := '0';
  AFindSubControlOptions.MatchBitmapText[0].CropRight := '0';
  AFindSubControlOptions.MatchBitmapText[0].CropBottom := '0';
  AFindSubControlOptions.MatchBitmapText[0].ProfileName := CDefaultFontProfileName;
  AFindSubControlOptions.MatchBitmapText[0].IgnoreBackgroundColor := False;

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

  VarsBkp := frClickerActions.ClkVariables.Text;
  BkpErrorLevel := FClkActions[Node^.Index].FindSubControlOptions.ColorError;
  BkpErrorCount := FClkActions[Node^.Index].FindSubControlOptions.AllowedColorErrorCount;
  BkpFastSearchErrorCount := FClkActions[Node^.Index].FindSubControlOptions.FastSearchAllowedColorErrorCount;

  SetLength(BkpFontNames, Length(FClkActions[Node^.Index].FindSubControlOptions.MatchBitmapText));
  SetLength(BkpFontSizes, Length(FClkActions[Node^.Index].FindSubControlOptions.MatchBitmapText));
  for i := 0 to Length(BkpFontNames) - 1 do
  begin
    BkpFontNames[i] := FClkActions[Node^.Index].FindSubControlOptions.MatchBitmapText[i].FontName;
    BkpFontSizes[i] := FClkActions[Node^.Index].FindSubControlOptions.MatchBitmapText[i].FontSize;
  end;

  try
    FClkActions[Node^.Index].FindSubControlOptions.ColorError := IntToStr(AErrorLevel);
    FClkActions[Node^.Index].FindSubControlOptions.AllowedColorErrorCount := IntToStr(AErrorCount);
    FClkActions[Node^.Index].FindSubControlOptions.FastSearchAllowedColorErrorCount := IntToStr(AFastSearchErrorCount);

    if (AFontName <> '') and (AFontSize <> -1) then
      for i := 0 to Length(BkpFontNames) - 1 do
      begin
        FClkActions[Node^.Index].FindSubControlOptions.MatchBitmapText[i].FontName := AFontName;
        FClkActions[Node^.Index].FindSubControlOptions.MatchBitmapText[i].FontSize := AFontSize;
      end;

    FDebugging := False;  /////////////////////// to be verified if this will throw the debugger into a bad state
    PrepareFilesInServer;  //not sure if this call requires spdbtnStopPlaying.Enabled := True; and spdbtnStopPlaying.Repaint;
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
    frClickerActions.ClkVariables.Text := VarsBkp;
    FClkActions[Node^.Index].FindSubControlOptions.ColorError := BkpErrorLevel;
    FClkActions[Node^.Index].FindSubControlOptions.AllowedColorErrorCount := BkpErrorCount;
    FClkActions[Node^.Index].FindSubControlOptions.FastSearchAllowedColorErrorCount := BkpFastSearchErrorCount;

    for i := 0 to Length(BkpFontNames) - 1 do
    begin
      FClkActions[Node^.Index].FindSubControlOptions.MatchBitmapText[i].FontName := BkpFontNames[i];
      FClkActions[Node^.Index].FindSubControlOptions.MatchBitmapText[i].FontSize := BkpFontSizes[i];
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


function TfrClickerActionsArr.HandleOnGetSetVarActionByName(var AClkSetVarOptions: TClkSetVarOptions; AActionName: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(FClkActions) - 1 do
    if FClkActions[i].ActionOptions.Action = acSetVar then
      if FClkActions[i].ActionOptions.ActionName = AActionName then
      begin
        AClkSetVarOptions := FClkActions[i].SetVarOptions;
        Result := True;
        Break;
      end;
end;


function TfrClickerActionsArr.HandleOnUpdateSetVarActionByName(AClkSetVarOptions: TClkSetVarOptions; AActionName: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(FClkActions) - 1 do
    if FClkActions[i].ActionOptions.Action = acSetVar then
      if FClkActions[i].ActionOptions.ActionName = AActionName then
      begin
        FClkActions[i].SetVarOptions := AClkSetVarOptions;
        Result := True;
        Break;
      end;
end;


function TfrClickerActionsArr.HandleOnTClkIniReadonlyFileCreate(AFileName: string): TClkIniReadonlyFile;
begin
  Result := DoOnTClkIniReadonlyFileCreate(ResolveTemplatePath(AFileName));
end;


function TfrClickerActionsArr.HandleOnTClkIniFileCreate(AFileName: string): TClkIniFile;
begin
  Result := DoOnTClkIniFileCreate(AFileName);
end;


function TfrClickerActionsArr.HandleOnClickerSetVarFrame_OnGetSelfTemplatesDir: string;
begin
  Result := ExtractFileDir(FFileName);
end;


procedure TfrClickerActionsArr.HandleOnClickerSetVarFrame_OnShowAutoComplete(AEdit: TEdit);
begin
  ShowAutoCompleteWindow(AEdit);
end;


procedure TfrClickerActionsArr.HandleOnUpdateActionScrollIndex(AActionScrollIndex: string);
var
  Node: PVirtualNode;
begin
  Node := vstActions.GetFirstSelected;
  if Node = nil then
    Exit;

  FClkActions[Node^.Index].ScrollIndex := AActionScrollIndex;
end;


function TfrClickerActionsArr.HandleOnGetLoadedTemplateFileName: string;
begin
  Result := FFileName;
end;


procedure TfrClickerActionsArr.HandleOnChangeEditTemplateEditingActionType;
var
  Node: PVirtualNode;
  ActionIndex: Integer;
begin
  Node := vstActions.GetFirstSelected;
  if Node = nil then
    Exit;

  ActionIndex := Node^.Index;

  if FClkActions[ActionIndex].ActionOptions.Action <> acEditTemplate then
    Exit;

  frClickerActions.CurrentlyEditingActionType := {%H-}TClkAction(CClkUnsetAction); //something to cause the setter to be called
  Application.ProcessMessages;
  frClickerActions.CurrentlyEditingActionType := FClkActions[ActionIndex].ActionOptions.Action;
end;


procedure TfrClickerActionsArr.HandleOnSaveStringListToFile(AStringList: TStringList; const AFileName: string);
begin
  DoOnSaveTemplateToFile(AStringList, ResolveTemplatePath(AFileName));
end;


function TfrClickerActionsArr.HandleOnExecuteActionByContent(var AAllActions: TClkActionsRecArr; AActionIndex: Integer): Boolean;
begin
  Result := ExecuteActionByContent(AAllActions, AActionIndex);
end;


function TfrClickerActionsArr.HandleOnLoadTemplateToActions(Fnm: string; var AActions: TClkActionsRecArr; AWhichTemplate: TEditTemplateWhichTemplate; out ANotes, AIconPath: string; AWaitForFileAvailability: Boolean = False): string;
const
  CLoc: array[Boolean] of TFileLocation = (flDisk, flMem);
var
  i: Integer;
begin
  if AWhichTemplate = etwtSelf then
  begin
    SetLength(AActions, Length(FClkActions));

    for i := 0 to Length(FClkActions) - 1 do
      CopyActionContent(FClkActions[i], AActions[i]);

    Result := '';
  end
  else
    Result := LoadTemplateToActions(Fnm, AActions, ANotes, AIconPath, CLoc[FExecutingActionFromRemote], InMemFS, AWaitForFileAvailability);
end;


function TfrClickerActionsArr.HandleOnSaveCompleteTemplateToFile(Fnm: string; var AActions: TClkActionsRecArr; AWhichTemplate: TEditTemplateWhichTemplate; ANotes, AIconPath: string; AUpdateUI, AShouldSaveSelfTemplate: Boolean): string;
var
  TempStringList: TStringList;   //much faster than T(Mem)IniFile
  MemStream: TMemoryStream;
  i: Integer;
  LenModified: Boolean;
begin
  Result := '';

  if AWhichTemplate = etwtSelf then
  begin
    LenModified := Length(AActions) <> Length(FClkActions);
    if LenModified then
      vstActions.RootNodeCount := Length(AActions);

    SetLength(FClkActions, Length(AActions));

    for i := 0 to Length(FClkActions) - 1 do
      CopyActionContent(AActions[i], FClkActions[i]);

    try
      if LenModified or AUpdateUI then
      begin
        UpdateNodesCheckStateFromActions; //required for EnableAction and DisableAction operations only
        vstActions.Repaint;
        SetPropertiesFromPlugins;
      end;
    except
      on E: Exception do
      begin
        AddToLog('Ex on updating self actions from EditTemplate: ' + E.Message);
        Result := E.Message;
      end;
    end;

    Modified := True; //This flag should be set anyway
    if AShouldSaveSelfTemplate then
    begin
      if FFileName <> '' then
        SaveTemplate(FFileName)  //this call resets the Modified flag
      else
        Result := CREResp_TemplateFileNameNotSet;
    end;
  end  //Self
  else
  begin
    TempStringList := TStringList.Create;
    try
      TempStringList.LineBreak := #13#10;
      SaveTemplateWithCustomActionsToStringList_V2(TempStringList, AActions, ANotes, AIconPath);

      Fnm := ResolveTemplatePath(Fnm);
      Fnm := EvaluateReplacements(Fnm);

      if FExecutingActionFromRemote then //save to in-mem
      begin
        MemStream := TMemoryStream.Create;
        try
          TempStringList.SaveToStream(MemStream);
          InMemFS.SaveFileToMem(Fnm, MemStream.Memory, MemStream.Size);
        finally
          MemStream.Free;
        end;
      end
      else //save to disk
        DoOnSaveTemplateToFile(TempStringList, Fnm);
    finally
      TempStringList.Free;
    end;
  end;
end;


procedure TfrClickerActionsArr.HandleOnBackupVars(AAllVars: TStringList);
begin
  AAllVars.AddStrings(frClickerActions.ClkVariables);
end;


function TfrClickerActionsArr.HandleOnExecuteActionByName(AActionName: string): Boolean;
var
  i, ActionIndex: Integer;
  LoadedNewFile: Boolean;
  OldDbgSymFnm: string;
begin
  AddToLog('Executing action "' + AActionName + '" by name, from plugin...');

  ActionIndex := -1;
  for i := 0 to Length(FClkActions) - 1 do
    if FClkActions[i].ActionOptions.ActionName = AActionName then
    begin
      ActionIndex := i;
      Break;
    end;

  if ActionIndex = -1 then
  begin
    AddToLog('Action "' + AActionName + '" not found, when executing by name, from plugin.');
    Result := False;
    Exit;
  end;

  OldDbgSymFnm := frClickerActions.frClickerPlugin.DbgSymFnm;
  LoadedNewFile := False;
  if FExecutingActionFromRemote then
    if frClickerActions.CurrentlyEditingActionType = acPlugin then
    begin
      LoadedNewFile := True;
      AddToLog('Loading debug symbols file into debugger: ' + ExtractFullFileNameNoExt(ResolveTemplatePath(FClkActions[ActionIndex].PluginOptions.FileName)) + '.DbgSym');
      frClickerActions.frClickerPlugin.LoadDebugSymbols(ExtractFullFileNameNoExt(ResolveTemplatePath(FClkActions[ActionIndex].PluginOptions.FileName)) + '.DbgSym');
    end;
  try
    Result := ExecuteActionAtIndex(ActionIndex);
  finally
    if FExecutingActionFromRemote and LoadedNewFile and (OldDbgSymFnm <> '') then
      frClickerActions.frClickerPlugin.LoadDebugSymbols(OldDbgSymFnm);
  end;
end;


function TfrClickerActionsArr.HandleOnGetAllActions: PClkActionsRecArr;
begin
  Result := @FClkActions;
end;


function TfrClickerActionsArr.HandleOnResolveTemplatePath(APath: string; ACustomSelfTemplateDir: string = ''; ACustomAppDir: string = ''): string;
begin
  Result := ResolveTemplatePath(APath, ACustomSelfTemplateDir, ACustomAppDir);
end;


procedure TfrClickerActionsArr.HandleOnSetDebugPoint(ADebugPoint: string);
begin
  frClickerActions.frClickerPlugin.SelectLineByContent(ADebugPoint);
end;


function TfrClickerActionsArr.HandleOnIsAtBreakPoint(ADebugPoint: string): Boolean;
begin
  Result := frClickerActions.frClickerPlugin.IsAtBreakPoint(ADebugPoint);
end;


procedure TfrClickerActionsArr.HandleOnSaveFileToExtRenderingInMemFS(AFileName: string; AContent: Pointer; AFileSize: Int64);
begin
  DoOnSaveFileToExtRenderingInMemFS(AFileName, AContent, AFileSize);
end;


procedure TfrClickerActionsArr.HandleOnGetListOfAvailableSetVarActions(AListOfSetVarActions: TStringList);
var
  i: Integer;
begin
  for i := 0 to Length(FClkActions) - 1 do
    if FClkActions[i].ActionOptions.Action = acSetVar then
      AListOfSetVarActions.Add(FClkActions[i].ActionOptions.ActionName);
end;


procedure TfrClickerActionsArr.HandleOnGetListOfAvailableActions(AListOfSetVarActions: TStringList);
var
  i: Integer;
begin
  for i := 0 to Length(FClkActions) - 1 do
    AListOfSetVarActions.Add(FClkActions[i].ActionOptions.ActionName + #4#5 + IntToStr(Ord(FClkActions[i].ActionOptions.Action)));
end;


procedure TfrClickerActionsArr.HandleOnModifyPluginProperty(AAction: PClkActionRec);
begin
  {$IFNDEF MemPlugins}
    if DoOnFileExists(ResolveTemplatePath(AAction^.PluginOptions.FileName)) then
  {$ENDIF}
      SetActionPropertiesFromPlugin(AAction^);
end;


procedure TfrClickerActionsArr.HandleOnPluginDbgStop;
begin
  if not FExecutesRemotely then
    StopAllActionsFromButton
  else
    StopRemoteTemplateExecution(RemoteAddress, FStackLevel, False);   //FStackLevel may be wrong if the server has multiple loaded templates
end;


procedure TfrClickerActionsArr.HandleOnPluginDbgContinueAll;
begin
  if not FExecutesRemotely then
    FPluginContinueAll := True  //will be reset by plugin TActionPlugin.ExecutePlugin, via pointer
  else
    SendPluginCmd(RemoteAddress, CREParam_Plugin_ContinueAll, FStackLevel, False);
end;


procedure TfrClickerActionsArr.HandleOnPluginDbgStepOver;
begin
  if not FExecutesRemotely then
    FPluginStepOver := True //will be reset by plugin handler, via pointer
  else
    SendPluginCmd(RemoteAddress, CREParam_Plugin_StepOver, FStackLevel, False);
end;


function TfrClickerActionsArr.HandleOnPluginDbgRequestLineNumber(out ALineContent, ADbgSymFile: string): Integer;
var
  Response: string;
begin
  Result := -1;
  if FExecutesRemotely then
  begin
    Response := SendPluginCmd(RemoteAddress, CREParam_Plugin_RequestLineNumber, FStackLevel, False);
    Result := StrToIntDef(Copy(Response, 1, Pos('=', Response) - 1), -1);
    ALineContent := Copy(Response, Pos('=', Response) + 1, MaxInt);
    ADbgSymFile := Copy(ALineContent, Pos(#2, ALineContent) + 1, MaxInt);
    ALineContent := Copy(ALineContent, 1, Pos(#2, ALineContent) - 1);
    //AddToLog('SelectedLine response (client): "' + IntToStr(Result) + '"  : "' + ALineContent + '".'); //for debugging
  end;
end;


procedure TfrClickerActionsArr.HandleOnPluginDbgSetBreakpoint(ALineIndex, ASelectedSourceFileIndex: Integer; AEnabled: Boolean);
var
  BreakPointCmd: string;
begin
  if FExecutesRemotely then
  begin
    BreakPointCmd := CREParam_Plugin_SetBreakpoint_LineIndex + '=' + IntToStr(ALineIndex) + '&' +
                     CREParam_Plugin_SetBreakpoint_SelectedSourceFileIndex + '=' + IntToStr(ASelectedSourceFileIndex) + '&' +
                     CREParam_Plugin_SetBreakpoint_Enabled + '=' + IntToStr(Ord(AEnabled));

    AddToLog('Setting breakpoint in server, at line: ' + IntToStr(ALineIndex) + ', from file index: ' + IntToStr(ASelectedSourceFileIndex) + ', to: ' + BoolToStr(AEnabled, 'Enabled', 'Disabled'));
    AddToLog('Setting result: ' + SendPluginCmd(RemoteAddress, CREParam_Plugin_SetBreakpoint + '&' + BreakPointCmd, FStackLevel, False));
  end;
end;


function TfrClickerActionsArr.HandleOnGenerateAndSaveTreeWithWinInterp(AHandle: THandle; ATreeFileName: string; AStep: Integer; AUseMouseSwipe: Boolean): Boolean;
begin
  Result := DoOnGenerateAndSaveTreeWithWinInterp(AHandle, ATreeFileName, AStep, AUseMouseSwipe);
end;


function TfrClickerActionsArr.HandleOnSetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue: string): Boolean;
begin
  Result := DoOnSetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue);
end;


procedure TfrClickerActionsArr.HandleOnWaitInDebuggingMode(var ADebuggingAction: TClkActionRec; AActionAllowsSteppingInto: TAllowsSteppingInto);
var
  IndexBeforeEditing: Integer;
begin
  FContinuePlayingAll := False;
  FPlaying := True;
  FContinuePlayingBySteppingInto := False;  //without this, stepping into will set this flag to true and any subsequent actions will enter debugging mode

  FStopAllActionsOnDemand := False;
  if FStopAllActionsOnDemandFromParent <> nil then
    FStopAllActionsOnDemandFromParent^ := False; //this flag may remain set after stopping a plugin

  spdbtnContinuePlayingAll.Enabled := True;
  spdbtnStepOver.Enabled := True;
  spdbtnStopPlaying.Enabled := True;
  spdbtnPlaySelectedAction.Enabled := False;
  spdbtnPlayAllActions.Enabled := False;

  spdbtnStepInto.Enabled := False;
  if ADebuggingAction.ActionOptions.Action = acCallTemplate then
    if AActionAllowsSteppingInto = asiYes then
      spdbtnStepInto.Enabled := True;   //Currently, there is a bug, which executes all called templates without debugging.
                                        //FContinuePlayingBySteppingInto will cause the waiting loop to exit, when set by spdbtnStepInto.
                                        //This will require an extra waiting loop.
                                        //Until fixed, please use the other remote debugging feature of CallTemplate (set both debugging parameters to True).

  if ADebuggingAction.ActionOptions.Action = acPlugin then
    if AActionAllowsSteppingInto = asiYes then
      spdbtnStepInto.Enabled := True;

  try
    SetLength(FClkActions, Length(FClkActions) + 1);  //add a debugging action
    vstActions.ClearSelection;

    IndexBeforeEditing := Length(FClkActions) - 1;
    CopyActionContent(ADebuggingAction, FClkActions[IndexBeforeEditing]);

    vstActions.RootNodeCount := Length(FClkActions);
    SelectNodeByIndex(vstActions, IndexBeforeEditing, True);

    UpdateActionsArrFromControls(IndexBeforeEditing);
    CopyActionContent(ADebuggingAction, FClkActions[IndexBeforeEditing]);  //call again, because UpdateActionsArrFromControls

    frClickerActions.UpdatePageControlActionExecutionIcons;
    UpdateControlsFromActionsArr(IndexBeforeEditing); //HandleActionSelection;
    StopGlowingUpdateButton;

    AddToLog('Entering in debugging mode.. Waiting for user to step over or continue debugging..');
    WaitInDebuggingMode;
  finally
    AddToLog('Exiting debugging mode..');
    spdbtnContinuePlayingAll.Enabled := False;
    spdbtnStepOver.Enabled := False;
    spdbtnStopPlaying.Enabled := False;
    spdbtnPlaySelectedAction.Enabled := True;
    spdbtnPlayAllActions.Enabled := True;
    spdbtnStepInto.Enabled := False;
    FPlaying := False;

    AddToLog('Updating back the action..');
    CopyActionContent(FClkActions[IndexBeforeEditing], ADebuggingAction);

    try
      RemoveAction(IndexBeforeEditing);  //delete the debugging action
    except
      AddToLog('Debugging action may have been moved (or at least its index is no longer valid).');
    end;
  end;
end;


function TfrClickerActionsArr.HandleOnGetPluginInMemFS: TInMemFileSystem;
begin
  {$IFDEF MemPlugins}
    Result := FMemPluginsInMemFS;
  {$ELSE}
    AddToLog('MemPluginsInMemFS not available. UIClicker requires MemPlugins compiler directive.');
    Result := nil;
  {$ENDIF}
end;


procedure TfrClickerActionsArr.UpdateActionsArrFromControls(ActionIndex: Integer);
//var
//  i: Integer;
begin
  FClkActions[ActionIndex].ActionOptions.ActionCondition := frClickerActions.frClickerConditionEditor.GetActionCondition;

  //the number of items from MatchBitmapText has to match the number of frames from FBMPTextFrames
  SetLength(FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText, frClickerActions.frClickerFindControl.GetBMPTextFontProfilesCount);

  //for i := 0 to frClickerActions.frClickerFindControl.GetBMPTextFontProfilesCount - 1 do
  //begin
  //  FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].ForegroundColor := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFGColor;
  //  FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].BackgroundColor := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextBGColor;
  //  FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].FontName := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFontName;
  //  FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].FontSize := StrToIntDef(frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextSize, 8);
  //  FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].Bold := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].Bold;
  //  FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].Italic := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].Italic;
  //  FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].Underline := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].Underline;
  //  FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].StrikeOut := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].StrikeOut;
  //
  //  if frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFontQualityIndex = -1 then
  //    FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].FontQuality := fqDefault
  //  else
  //  begin
  //    FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].FontQuality := TFontQuality(frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFontQualityIndex);
  //
  //    if frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFontQualityIndex = Integer(High(TFontQuality)) + 1 then  //is a replacement
  //    begin
  //      FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].FontQualityReplacement := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].FontQualityReplacement;
  //      FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].FontQualityUsesReplacement := True;
  //    end
  //    else
  //      FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].FontQualityUsesReplacement := False;
  //  end;
  //
  //  FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].ProfileName := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].ProfileName;
  //
  //  FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].CropLeft := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].CropLeft;
  //  FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].CropTop := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].CropTop;
  //  FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].CropRight := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].CropRight;
  //  FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].CropBottom := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].CropBottom;
  //  FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].IgnoreBackgroundColor := frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].IgnoreBackgroundColor;
  //end;

  FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapFiles := frClickerActions.frClickerFindControl.lstMatchBitmapFiles.Items.Text;

  FClkActions[ActionIndex].CallTemplateOptions.ListOfCustomVarsAndValues := FastReplace_ReturnTo45(frClickerActions.ListOfCustomVariables);

  frClickerActions.SerializeEditTemplateEditingAction;
  //frClickerActions.EditingAction^.EditTemplateOptions.ListOfEditedProperties := StringReplace(GetActionPropertiesByType(FClkEditedActionByEditTemplate), CPropSeparatorSer, CPropSeparatorInt, [rfReplaceAll]);
  //AddToLog('Updating ListOfEditedProperties to ' + frClickerActions.EditingAction^.EditTemplateOptions.ListOfEditedProperties);

  CopyActionContent(frClickerActions.EditingAction^, FClkActions[ActionIndex]);  //uncomment this after removing above code
end;


procedure TfrClickerActionsArr.UpdateControlsFromActionsArr(ActionIndex: Integer);
var
  NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
  Action_ScrollInfo: TStringList;
begin
  frClickerActions.frClickerConditionEditor.DisplayActionCondition(FClkActions[ActionIndex].ActionOptions.ActionCondition);

  /////the new content, instead of editboxes and checkboxes
  CopyActionContent(FClkActions[ActionIndex], frClickerActions.EditingAction^);

  {if frClickerActions.CurrentlyEditingActionType = acFindSubControl then
  begin
    //the number of items from MatchBitmapText has to match the number of frames from FBMPTextFrames
    frClickerActions.frClickerFindControl.CreateBMPTextFrames(Length(FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText)); //do not use SetLength(frClickerActions.FBMPTextFrames, Length(FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText));

    for i := 0 to frClickerActions.frClickerFindControl.GetBMPTextFontProfilesCount - 1 do  //this part is still required when selecting an action
    begin
      frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFGColor := FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].ForegroundColor;
      frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextBGColor := FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].BackgroundColor;
      //frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].FGColor := HexToInt(EvaluateReplacements(FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].ForegroundColor));
      //frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].BGColor := HexToInt(EvaluateReplacements(FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].BackgroundColor));
      frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFontName := FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].FontName;
      frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextSize := IntToStr(FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].FontSize);
      frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].Bold := FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].Bold;
      frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].Italic:= FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].Italic;
      frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].Underline := FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].Underline;
      frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].StrikeOut := FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].StrikeOut;

      if FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].FontQualityUsesReplacement then
      begin
        frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFontQualityIndex := Integer(High(TFontQuality)) + 1;
        frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].FontQualityReplacement := FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].FontQualityReplacement;
      end
      else
        frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFontQualityIndex := Integer(FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].FontQuality);

      TempProfileName := FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].ProfileName;
      //frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].ProfileName := TempProfileName;
      //frClickerActions.frClickerFindControl.tabctrlBMPText.Tabs.Strings[i] := TempProfileName;     //remove these calls if UpdateFontProfileName works as expected
      frClickerActions.frClickerFindControl.UpdateFontProfileName(i, TempProfileName);

      frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].CropLeft := FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].CropLeft;
      frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].CropTop := FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].CropTop;
      frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].CropRight := FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].CropRight;
      frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].CropBottom := FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].CropBottom;

      frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].IgnoreBackgroundColor := FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i].IgnoreBackgroundColor;

      //frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].UpdateSelectionLabelsFromCropEditBoxes;  //replaced below with other call
    end;

    frClickerActions.frClickerFindControl.SetBMPTextFrameVisibility;

    frClickerActions.frClickerFindControl.UpdateListsOfSearchFiles(FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapFiles, FClkActions[ActionIndex].FindSubControlOptions.MatchPrimitiveFiles);
    frClickerActions.frClickerFindControl.UpdateBitmapAlgorithmSettings;
  end;}

  frClickerActions.LoadListOfAvailableTemplates;

  frClickerActions.ListOfCustomVariables := FastReplace_45ToReturn(FClkActions[ActionIndex].CallTemplateOptions.ListOfCustomVarsAndValues);

  {if frClickerActions.CurrentlyEditingActionType = acFindSubControl then
    frClickerActions.frClickerFindControl.UpdatePreviewIcons;

  frClickerActions.UpdateControlWidthHeightLabels;
  frClickerActions.UpdateUseWholeScreenLabel(FClkActions[ActionIndex].FindSubControlOptions.UseWholeScreen);}

  if frClickerActions.CurrentlyEditingActionType = acFindSubControl then
    frClickerActions.frClickerFindControl.UpdateSearchAreaLabelsFromKeysOnInitRect(FClkActions[ActionIndex].FindSubControlOptions.InitialRectangle);

  frClickerActions.frClickerExecApp.memExecAppParams.Lines.Text := FClkActions[ActionIndex].ExecAppOptions.ListOfParams;
  frClickerActions.frClickerSetVar.SetListOfSetVars(FClkActions[ActionIndex].SetVarOptions);

  //if (FClkActions[ActionIndex].ActionOptions.Action = acEditTemplate) and (frClickerActions.EditingAction <> nil) then
  //  frClickerActions.SerializeEditTemplateEditingAction;   //if called here, before setting CurrentlyEditingActionType, there will be no default values in OI

  frClickerActions.CurrentlyEditingActionType := FClkActions[ActionIndex].ActionOptions.Action;

  {if frClickerActions.CurrentlyEditingActionType = acFindSubControl then
  begin
    frClickerActions.frClickerFindControl.PreviewText;

    for i := 0 to frClickerActions.frClickerFindControl.GetBMPTextFontProfilesCount - 1 do
      frClickerActions.frClickerFindControl.BMPTextFontProfiles[i].UpdateSelectionLabelsFromCropInfo(FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[i]);
  end;}

  if frClickerActions.CurrentlyEditingActionType in [acFindControl, acFindSubControl] then    //both actions
    frClickerActions.UpdateFindSubControlInternalStructuresFromAction(@FClkActions[ActionIndex]);

  if frClickerActions.CurrentlyEditingActionType = acPlugin then
  begin
    frClickerActions.frClickerPlugin.LoadDebugSymbols(ExtractFullFileNameNoExt(ResolveTemplatePath(FClkActions[ActionIndex].PluginOptions.FileName)) + '.DbgSym');
    SetActionPropertiesFromPlugin(FClkActions[ActionIndex]);
  end;

  if (frClickerActions.CurrentlyEditingActionType = acEditTemplate) and (frClickerActions.EditingAction <> nil) then
  begin
    frClickerActions.DeserializeEditTemplateEditingAction;
    frClickerActions.CurrentlyEditingActionType := {%H-}TClkAction(CClkUnsetAction); //something to cause the setter to be called
    frClickerActions.CurrentlyEditingActionType := FClkActions[ActionIndex].ActionOptions.Action;
  end; //it's ugly to reload the OI multiple times, but this is an easy fix

  if FClkActions[ActionIndex].ScrollIndex <> '' then
  begin
    Action_ScrollInfo := TStringList.Create;
    try
      Action_ScrollInfo.LineBreak := #13#10;
      Action_ScrollInfo.Text := FClkActions[ActionIndex].ScrollIndex;

      NodeLevel := StrToIntDef(Action_ScrollInfo.Values[COIScrollInfo_NodeLevel], -1);
      CategoryIndex := StrToIntDef(Action_ScrollInfo.Values[COIScrollInfo_CategoryIndex], -1);
      PropertyIndex := StrToIntDef(Action_ScrollInfo.Values[COIScrollInfo_PropertyIndex], -1);
      PropertyItemIndex := StrToIntDef(Action_ScrollInfo.Values[COIScrollInfo_PropertyItemIndex], -1);

      frClickerActions.BringOIPropertyIntoView(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex);
    finally
      Action_ScrollInfo.Free;
    end;
  end;
end;


procedure TfrClickerActionsArr.UpdateModifiedLabel(AFileLocation: TFileLocation = flDisk);
var
  s: string;
begin
  s := '';
  if AFileLocation = flMem then
    s := ' [Mem]';

  if FModified then
  begin
    lblModifiedStatus.Caption := 'Modified' + s + ': ' + ExtractFileName(FFileName);
    lblModifiedStatus.Font.Color := $000429FF;
    spdbtnSaveTemplate.Font.Color := $00241CED;
  end
  else
  begin
    lblModifiedStatus.Caption := 'Up to date' + s + ': ' + ExtractFileName(FFileName);
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


function TfrClickerActionsArr.EvaluateReplacements(s: string; Recursive: Boolean = True; AEvalTextCount: Integer = -1): string;
begin
  Result := frClickerActions.EvaluateReplacements(s, Recursive, AEvalTextCount);
end;


function TfrClickerActionsArr.GetActionVarValue(VarName: string): string;
begin
  Result := frClickerActions.ClkVariables.Values[VarName];
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
var
  Idx: Integer;
  NewLineStr: string;
begin
  NewLineStr := VarName + '=' + FastReplace_ReturnTo68(VarValue);

  if ThreadID <> MainThreadID then
    raise Exception.Create('ThreadID doesn''t match MainThreadID when setting var: ' + NewLineStr);

  Idx := frClickerActions.ClkVariables.IndexOfName(VarName);

  if Idx > -1 then
    frClickerActions.ClkVariables.Strings[Idx] := NewLineStr   //do not use Values[VarName] := ..
  else
    frClickerActions.ClkVariables.Add(NewLineStr);

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


function TfrClickerActionsArr.ExecuteActionByContent(var AAllActions: TClkActionsRecArr; AActionIndex: Integer): Boolean;
begin
  Result := True;

  SetActionVarValue('$ExecAction_Err$', '');
  SetActionVarValue('$SelfActionName$', AAllActions[AActionIndex].ActionOptions.ActionName);
  SetActionVarValue('$SelfActionIndex$', IntToStr(AActionIndex));

  case AAllActions[AActionIndex].ActionOptions.Action of
    acClick: Result := FActionExecution.ExecuteMultiClickAction(AAllActions[AActionIndex].ClickOptions);
    acExecApp: Result := FActionExecution.ExecuteExecAppAction(AAllActions[AActionIndex].ExecAppOptions, AAllActions[AActionIndex].ActionOptions);
    acFindControl: Result := FActionExecution.ExecuteFindControlActionWithTimeout(AAllActions[AActionIndex].FindControlOptions, AAllActions[AActionIndex].ActionOptions);
    acFindSubControl: Result := FActionExecution.ExecuteFindSubControlActionWithTimeout(AAllActions[AActionIndex].FindSubControlOptions, AAllActions[AActionIndex].ActionOptions);
    acSetControlText: Result := FActionExecution.ExecuteSetControlTextAction(AAllActions[AActionIndex].SetTextOptions);
    acCallTemplate: Result := FActionExecution.ExecuteLoopedCallTemplateAction(AAllActions[AActionIndex].CallTemplateOptions, FContinuePlayingBySteppingInto, {FShouldStopAtBreakPoint replaced by FDebugging} FDebugging);
    acSleep: Result := FActionExecution.ExecuteSleepAction(AAllActions[AActionIndex].SleepOptions, AAllActions[AActionIndex].ActionOptions);
    acSetVar: Result := FActionExecution.ExecuteSetVarAction(AAllActions[AActionIndex].SetVarOptions);
    acWindowOperations: Result := FActionExecution.ExecuteWindowOperationsAction(AAllActions[AActionIndex].WindowOperationsOptions);
    acLoadSetVarFromFile: Result := FActionExecution.ExecuteLoadSetVarFromFileAction(AAllActions[AActionIndex].LoadSetVarFromFileOptions);
    acSaveSetVarToFile: Result := FActionExecution.ExecuteSaveSetVarToFileAction(AAllActions[AActionIndex].SaveSetVarToFileOptions);
    acPlugin: Result := FActionExecution.ExecutePluginAction(AAllActions[AActionIndex].PluginOptions, @AAllActions, frClickerActions.ClkVariables, ResolveTemplatePath(AAllActions[AActionIndex].PluginOptions.FileName), FContinuePlayingBySteppingInto, {FShouldStopAtBreakPoint replaced by FDebugging} FDebugging);
    acEditTemplate: Result := FActionExecution.ExecuteEditTemplateAction(AAllActions[AActionIndex].EditTemplateOptions);
  end;  //case
end;


function TfrClickerActionsArr.ExecuteActionAtIndex(AActionIndex: Integer): Boolean;
begin
  if (AActionIndex < 0) or (AActionIndex > Length(FClkActions) - 1) then
    raise Exception.Create('ActionIndex out of bounds: ' + IntToStr(AActionIndex));

  Result := ExecuteActionByContent(FClkActions, AActionIndex);
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
var
  LocalFileProvider: TFileProvider;
  IsAllowed: Boolean;
  DenyReason: string;
  EmptyTemplate: TClkActionsRecArr;
begin
  if not FExecutesRemotely then
    Exit;

  if GetServerFileExpectancy(FRemoteAddress) = CREResp_FileExpectancy_ValueFromClient then
    if FStackLevel = 0 then  //Load only the main file. The others should be automatically handled by server.
    begin
      AddToLog('Should send template and other files to server...');

      if FFileName = '' then
        AddToLog(SetCurrentClientTemplateInServer) //send the current template (which is not saved to a file (disk or mem))
      else
      begin
        LocalFileProvider := TFileProvider.Create;
        try
          LocalFileProvider.AddListOfAccessibleFileExtensions(FAllowedFileExtensionsForServer);
          LocalFileProvider.AddListOfAccessibleDirs(FAllowedFileDirsForServer);
          LocalFileProvider.FullTemplatesDir := FFullTemplatesDir;

          IsAllowed := LocalFileProvider.FileIsAllowed(FFileName, DenyReason);
          if IsAllowed then
          begin
            //AddToLog(SendLoadTemplateInExecListRequest(FRemoteAddress, FFileName, FStackLevel));   //Do not send the file at all. Let the file provider transfer it.
            AddToLog(SetCurrentClientTemplateInServer);  //this is required, to load the template into editor   - actually, it is ok to load the template into editor, without sending it to server via SendLoadTemplateInExecListRequest. However, SetClientTemplateInServer should verify sending permissions first.
          end
          else
          begin
            AddToLog('Current template is not allowed to be sent to server: ' + DenyReason);
            AddToLog('Clearing current template in server: ' + SetClientTemplateInServer(FRemoteAddress, FFileName, EmptyTemplate, FStackLevel)); //clear, because the current client action will pass if it finds a successful action in server
            Sleep(700); //This is required to provide live visual feeback about running and stopping the template. The test driver requires it to capture the state of the Stop button on ClickerUnderTest. Without this Sleep call, the Stop button stays "On" for a very short period of time.
          end;
        finally
          LocalFileProvider.Free;
        end;
      end;
      //AddToLog(SendMissingFilesToServer);  //keep commented, to let the server request its missing files
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
    VarReplacements.LineBreak := #13#10;
    try
      VarReplacements.AddStrings(frClickerActions.ClkVariables); //init with something, in case the server can't be reached

      if FContinuePlayingBySteppingInto {AIsDebugging} then
        if FClkActions[AActionIndex].ActionOptions.Action = acPlugin then
        begin
          try
            frClickerActions.frClickerPlugin.EnableRequestLineNumber;
            frClickerActions.frClickerPlugin.SelectLineByContent(CBeforePluginExecution_DbgLineContent);
          except
            on E: Exception do
              AddToLog('Ex on displaying plugin content for debugging: ' + E.Message);
          end;
        end;

      try
        Result := LocalOnExecuteRemoteActionAtIndex(AActionIndex, FStackLevel, VarReplacements, FContinuePlayingBySteppingInto);

        frClickerActions.ClkVariables.Clear;
        frClickerActions.ClkVariables.AddStrings(VarReplacements);
      finally
        if FContinuePlayingBySteppingInto {AIsDebugging} then
          if FClkActions[AActionIndex].ActionOptions.Action = acPlugin then
          begin
            try
              frClickerActions.frClickerPlugin.DisableRequestLineNumber;
            except
              on E: Exception do
                AddToLog('Ex on displaying plugin content for debugging: ' + E.Message);
            end;
          end;
      end;  //try
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
      VarNames.LineBreak := #13#10;
      VarValues.LineBreak := #13#10;
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
  begin
    imgWaitingForFilesAvailability.Show;
    tmrWaitingForFilesAvailability.Enabled := True;

    FOnWaitForFileAvailability(AFileName);

    imgWaitingForFilesAvailability.Hide;
    tmrWaitingForFilesAvailability.Enabled := False;
  end;
end;


procedure TfrClickerActionsArr.DoWaitForMultipleFilesAvailability(AListOfFiles: TStringList);
begin
  if Assigned(FOnWaitForMultipleFilesAvailability) then
  begin
    imgWaitingForFilesAvailability.Show;
    tmrWaitingForFilesAvailability.Enabled := True;

    FOnWaitForMultipleFilesAvailability(AListOfFiles);

    imgWaitingForFilesAvailability.Hide;
    tmrWaitingForFilesAvailability.Enabled := False;
  end;
end;


procedure TfrClickerActionsArr.DoWaitForBitmapsAvailability(AListOfFiles: TStringList);
begin
  if Assigned(FOnWaitForBitmapsAvailability) then
  begin
    imgWaitingForFilesAvailability.Show;
    tmrWaitingForFilesAvailability.Enabled := True;

    FOnWaitForBitmapsAvailability(AListOfFiles);

    imgWaitingForFilesAvailability.Hide;
    tmrWaitingForFilesAvailability.Enabled := False;
  end;
end;


procedure TfrClickerActionsArr.DoOnTerminateWaitForMultipleFilesAvailability;
begin
  if Assigned(FOnTerminateWaitForMultipleFilesAvailability) then
  begin
    imgWaitingForFilesAvailability.Show;
    tmrWaitingForFilesAvailability.Enabled := True;

    FOnTerminateWaitForMultipleFilesAvailability();

    imgWaitingForFilesAvailability.Hide;
    tmrWaitingForFilesAvailability.Enabled := False;
  end;
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


procedure TfrClickerActionsArr.DoOnSaveRenderedBitmap(ABitmap: TBitmap; AFileName: string);
begin
  if Assigned(FOnSaveRenderedBitmap) then
    FOnSaveRenderedBitmap(ABitmap, AFileName)
  else
    raise Exception.Create('OnSaveRenderedBitmap is not assigned.');
end;


function TfrClickerActionsArr.DoOnRenderBmpExternally(AFilename: string): string;
begin
  if Assigned(FOnRenderBmpExternally) then
    Result := FOnRenderBmpExternally(AFileName)
  else
    Result := 'OnRenderBmpExternally not assigned';
end;


function TfrClickerActionsArr.DoOnLoadRawPmtv(APmtvFile: TMemoryStream; AFileName: string): Boolean;
begin
  if Assigned(FOnLoadRawPmtv) then
    Result := FOnLoadRawPmtv(APmtvFile, AFileName)
  else
    raise Exception.Create('OnLoadRawPmtv is not assigned.');
end;


procedure TfrClickerActionsArr.DoOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages: TStringList);
begin
  if not Assigned(FOnGetListOfExternallyRenderedImages) then
    raise Exception.Create('OnGetListOfExternallyRenderedImages not assigned.')
  else
    FOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages, Self);
end;


{$IFDEF MemPlugins}
  procedure TfrClickerActionsArr.DoOnGetListOfInMemPlugins(AListOfInMemPlugins: TStringList);
  begin
    if not Assigned(FOnGetListOfInMemPlugins) then
      raise Exception.Create('OnGetListOfInMemPlugins not assigned.')
    else
      FOnGetListOfInMemPlugins(AListOfInMemPlugins);
  end;


  procedure TfrClickerActionsArr.DoOnLoadPluginFromDiskToPluginInMemFileSystem(APluginPath: string);
  begin
    if not Assigned(FOnLoadPluginFromDiskToPluginInMemFileSystem) then
      raise Exception.Create('OnLoadPluginFromDiskToPluginInMemFileSystem not assigned.')
    else
      FOnLoadPluginFromDiskToPluginInMemFileSystem(APluginPath);
  end;
{$ENDIF}


function TfrClickerActionsArr.DoOnLoadPluginFromInMemFS(APlugin: TMemoryStream; AFileName: string): Boolean;
begin
  if Assigned(FOnLoadPluginFromInMemFS) then
    Result := FOnLoadPluginFromInMemFS(APlugin, AFileName)
  else
    Result := False;
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


function TfrClickerActionsArr.DoOnTClkIniFileCreate(AFileName: string): TClkIniFile;
begin
  if not Assigned(FOnTClkIniFileCreate) then
    raise Exception.Create('OnTClkIniFileCreate is not assigned.')
  else
    Result := FOnTClkIniFileCreate(AFileName);
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


procedure TfrClickerActionsArr.DoOnOpenCalledTemplateInExperimentTab(AExperimentIndex: Integer; ATemplatePath: string);
begin
  if not Assigned(FOnOpenCalledTemplateInExperimentTab) then
    raise Exception.Create('OnOpenCalledTemplateInExperimentTab not assigned.')
  else
    FOnOpenCalledTemplateInExperimentTab(AExperimentIndex, ATemplatePath);
end;


procedure TfrClickerActionsArr.DoOnSaveFileToExtRenderingInMemFS(AFileName: string; AContent: Pointer; AFileSize: Int64);
begin
  if not Assigned(FOnSaveFileToExtRenderingInMemFS) then
    raise Exception.Create('OnSaveFileToExtRenderingInMemFS not assigned.')
  else
    FOnSaveFileToExtRenderingInMemFS(AFileName, AContent, AFileSize);
end;


procedure TfrClickerActionsArr.DoOnUpdatePropertyIcons(AStreamContent: Pointer; AStreamSize: Int64);
var
  Bmp: TBitmap;
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  Bmp := TBitmap.Create;
  try
    MemStream.SetSize(AStreamSize);
    Move(AStreamContent^, MemStream.Memory^, AStreamSize);
    MemStream.Position := 0;
    Bmp.LoadFromStream(MemStream, AStreamSize);

    //MessageBox(Handle, PChar('Adding icon' + #13#10 + 'Of size: ' + IntToStr(AStreamSize) + '  ' + IntToStr(Bmp.Width) + ':' + IntToStr(Bmp.Height)), 'Arr', 0);
    frClickerActions.imglstPluginProperties.AddMasked(Bmp, clFuchsia);
  finally
    MemStream.Free;
    Bmp.Free;
  end;
end;


procedure TfrClickerActionsArr.DoOnAddFileNameToRecent(AFileName: string);
begin
  if not Assigned(FOnAddFileNameToRecent) then
    raise Exception.Create('OnAddFileNameToRecent not assigned.')
  else
    FOnAddFileNameToRecent(AFileName);
end;


procedure TfrClickerActionsArr.DoOnGetListOfRecentFiles(AList: TStringList);
begin
  if not Assigned(FOnGetListOfRecentFiles) then
    raise Exception.Create('OnGetListOfRecentFiles not assigned.')
  else
    FOnGetListOfRecentFiles(AList);
end;


function TfrClickerActionsArr.DoOnGenerateAndSaveTreeWithWinInterp(AHandle: THandle; ATreeFileName: string; AStep: Integer; AUseMouseSwipe: Boolean): Boolean;
begin
  if not Assigned(FOnGenerateAndSaveTreeWithWinInterp) then
    raise Exception.Create('OnGenerateAndSaveTreeWithWinInterp not assigned.')
  else
    Result := FOnGenerateAndSaveTreeWithWinInterp(AHandle, ATreeFileName, AStep, AUseMouseSwipe);
end;


function TfrClickerActionsArr.DoOnSetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue: string): Boolean;
begin
  if not Assigned(FOnSetWinInterpOption) then
    raise Exception.Create('OnSetWinInterpOption not assigned.')
  else
    Result := FOnSetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue);
end;


function TfrClickerActionsArr.DoOnGetListeningPort: Word;
begin
  if not Assigned(FOnGetListeningPort) then
    raise Exception.Create('OnGetListeningPort not assigned.')
  else
    Result := FOnGetListeningPort;
end;


function TfrClickerActionsArr.PlayActionByNode(Node: PVirtualNode): Boolean;
var
  ActionIndex: Integer;
  InitialLen: Integer;
  ActionName: string;
  DeletingActionIndex: Integer;  //used only when executing EditTemplate, which deletes actions
  DeletingSelfAction: Boolean;
  MovingActionSrcIndex, MovingActionDestIndex: Integer;    //used only when executing EditTemplate, which moves actions
  DbgCurrentAction: string;
begin
  Result := True;

  ActionIndex := Node^.Index;
  DeletingActionIndex := -1;
  MovingActionSrcIndex := -1;
  MovingActionDestIndex := -1;

  FClkActions[ActionIndex].ActionStatus := asInProgress;
  SetActionVarValue('$ExitCode$', '');
  vstActions.RepaintNode(Node);
  InitialLen := Length(FClkActions);

  try
    FClkActions[ActionIndex].ActionSkipped := False;

    if EvaluateActionCondition(ActionIndex) then
    begin
      if FClkActions[ActionIndex].ActionOptions.Action = acEditTemplate then
        if FClkActions[ActionIndex].EditTemplateOptions.WhichTemplate = etwtSelf then
        begin
          case FClkActions[ActionIndex].EditTemplateOptions.Operation of
            etoDeleteAction:
            begin
              ActionName := FClkActions[ActionIndex].EditTemplateOptions.EditedActionName;
              DeletingActionIndex := GetActionIndexByName(FClkActions, ActionName);
            end;

            etoMoveAction:
            begin
              MovingActionSrcIndex := GetActionIndexByName(FClkActions, FClkActions[ActionIndex].EditTemplateOptions.EditedActionName);
              MovingActionDestIndex := GetActionIndexByName(FClkActions, FClkActions[ActionIndex].EditTemplateOptions.NewActionName);
            end;

            else
              ;
          end; //case
        end;

      if not FExecutesRemotely then
        Result := ExecuteActionAtIndex(ActionIndex)
      else
      begin
        Result := DoExecuteRemoteActionAtIndex(ActionIndex);   //called by client, to send requests
        Result := Result and (ActionStatusStrToActionStatus(GetActionVarValue('$LastAction_Status$')) = asSuccessful);

        if Result then                                                             //As an issue in this case, is that executing local apps, might by unwanted.
        begin
          if FClkActions[ActionIndex].ActionOptions.Action = acEditTemplate then   //The result is not modified by the local execution.
            AddToLog('Executing locally as well, because the action is an EditTemplate action. Result = ' + BoolToStr(ExecuteActionAtIndex(ActionIndex), True));
        end
        else
        begin
          DbgCurrentAction := GetActionVarValue('$DbgCurrentAction$');
          if DbgCurrentAction > '' then
          begin
            AddToLog('$DbgCurrentAction$: ' + DbgCurrentAction);
            if Pos(CErr_CannotSelectActionToLoadValuesAtIndex, DbgCurrentAction) > 0 then
            begin
              if GetActionVarValue('$ExecAction_Err$') = CErr_EmptyTemplate then
                AddToLog('The template might not be sent to server. Please verify the list of allowed directories, on the Settings page.')
              else
                AddToLog('The template might be out of date on server side and a non-existent action is attempted to be executed.');
            end
            //else
            //  AddToLog('No extra info about what went wrong.');
          end;
        end;
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

  if Length(FClkActions) > 0 then
  begin
    DeletingSelfAction := DeletingActionIndex = ActionIndex;

    if InitialLen - Length(FClkActions) = 1 then  //deleted one action
      if (DeletingActionIndex > -1) and (DeletingActionIndex < ActionIndex) then
        Dec(ActionIndex);

    if (MovingActionSrcIndex > -1) and (MovingActionDestIndex > -1) then //Moving an action
    begin
      if ActionIndex = MovingActionSrcIndex then
        ActionIndex := MovingActionDestIndex
      else
        if ActionIndex = MovingActionDestIndex then
          ActionIndex := MovingActionSrcIndex;
    end;

    if (ActionIndex > -1) and (ActionIndex < Length(FClkActions)) and (not ((DeletingActionIndex > -1) and DeletingSelfAction)) then
    begin
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
           not Result and (
           (FClkActions[ActionIndex].ActionOptions.Action = acFindControl) and (FClkActions[ActionIndex].FindControlOptions.AllowToFail) or
           (FClkActions[ActionIndex].ActionOptions.Action = acFindSubControl) and (FClkActions[ActionIndex].FindSubControlOptions.AllowToFail)
                          ) then
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

      if InitialLen <> Length(FClkActions) then
        Node := GetNodeByIndex(ActionIndex);

      if Node <> nil then
        vstActions.RepaintNode(Node);    //Node is no longer valid if the action is deleted

      if (MovingActionSrcIndex > -1) and (MovingActionDestIndex > -1) then //Moving an action
        vstActions.Repaint;
    end;
  end;
end;


procedure TfrClickerActionsArr.PlaySelected;
var
  Node: PVirtualNode;
  TempIdx: Integer;
begin
  Node := vstActions.GetFirstSelected;
  if Node = nil then
    Exit; //do not show a dialog here

  TempIdx := Node^.Index; //using TempIdx, because the action might be deleted by PlayActionByNode
  PlayActionByNode(Node);

  if (TempIdx < -1) and (TempIdx < Length(FClkActions)) then
    if FClkActions[TempIdx].ActionOptions.Action = acPlugin then
      frClickerActions.frClickerPlugin.DoneDebuging;
end;


procedure TfrClickerActionsArr.WaitInDebuggingMode;
begin
  imgWaitingInDebuggingMode.Show;
  imgWaitingInDebuggingMode.Tag := imgWaitingInDebuggingMode.Tag + 1;
  imgWaitingInDebuggingMode.Hint := 'Waiting in debugging mode.' + #13#10 + 'Call stack level: ' + IntToStr(imgWaitingInDebuggingMode.Tag);
  try
    repeat
      if GeneralClosingApp then
        Break;

      Application.ProcessMessages;

      if GeneralClosingApp then
        Break;

      if FStopAllActionsOnDemandFromParent <> nil then
        if FStopAllActionsOnDemandFromParent^ then
        begin
          FStopAllActionsOnDemand := True;
          Exit;
        end;

      {$IFDEF Windows}
        if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) and (GetAsyncKeyState(VK_F2) < 0) then
      {$ELSE}
        if (GetKeyState(VK_CONTROL) < 0) and (GetKeyState(VK_SHIFT) < 0) and (GetKeyState(VK_F2) < 0) then
      {$ENDIF}
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
begin
  Result := ClickerVstUtils.GetNodeByIndex(vstActions, ANodeIndex);
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
  Node: PVirtualNode;
  ClosingTemplateResponse: string;
  tk: QWord;
  IsAtBreakPoint: Boolean;
  LastExecutionActionResult: Boolean;
begin
  FPlayingAllActions := True;
  try
    Result := False;
    LastExecutionActionResult := True;

    if IsDebugging and FExecutingActionFromRemote and (FStackLevel > 0) and not FUseLocalDebugger then  //  this loop allows receiving missing files
    begin
      SetActionVarValue('$DbgPlayAllActions$', 'Waiting for ClosingTemplate');

      imgWaitingInPreDebuggingMode.Show;
      tk := GetTickCount64;
      repeat
        Application.ProcessMessages;
        Sleep(2);

        if FStopAllActionsOnDemandFromParent <> nil then
          if FStopAllActionsOnDemandFromParent^ then
          begin
            FStopAllActionsOnDemandFromParent^ := False; // reset flag, to allow CallTemplate actions to be executed
            FStopAllActionsOnDemand := True;
            Exit;
          end;

        if GeneralClosingApp then
          Break;
      until FClosingTemplate or (GetTickCount64 - tk > 3600000);  //1h    //FClosingTemplate is set to True by client, using CRECmd_ExitTemplate
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
        repeat
          FClkActions[Node^.Index].ActionOptions.ExecutionIndex := IntToStr(Node^.Index);  //required by FindSubControl, to update the UI, when adding a default font profile
          HighlightCurrentlyExecutedAction(Node);

          spdbtnStepInto.Enabled := IsDebugging and (FClkActions[Node^.Index].ActionOptions.Action in [acCallTemplate, acPlugin]);

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

          {$IFDEF Windows}
            if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) and (GetAsyncKeyState(VK_F2) < 0) then
          {$ELSE}
            if (GetKeyState(VK_CONTROL) < 0) and (GetKeyState(VK_SHIFT) < 0) and (GetKeyState(VK_F2) < 0) then
          {$ENDIF}
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
            begin
              LastExecutionActionResult := PlayActionByNode(Node);   //    Execution happens here
              if not LastExecutionActionResult then
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

          if FClkActions[Node^.Index].ActionOptions.Action in [acCallTemplate, acPlugin] then
          begin
            FContinuePlayingBySteppingInto := False; //reset flag for next execution
            //spdbtnContinuePlayingInto.Enabled := False;

            if FClkActions[Node^.Index].ActionOptions.Action = acPlugin then
              frClickerActions.frClickerPlugin.DoneDebuging;
          end;

          /////// debugging icons
          FClkActions[Node^.Index].ActionDebuggingStatus := adsPrev;
          vstActions.InvalidateNode(Node);
          /////// debugging icons

          Node := Node^.NextSibling;
          vstActions.RepaintNode(Node);
          Application.ProcessMessages;
        until (Node = nil) or not FPlaying;
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

          if FStopAllActionsOnDemand or GeneralClosingApp or not LastExecutionActionResult then
          begin
            ClosingTemplateResponse := StopRemoteTemplateExecution(FRemoteAddress, FStackLevel);
            AddToLog(DateTimeToStr(Now) + '  Sent request to stop remote template execution at index ' + IntToStr(FStackLevel) +  '  ' + ClosingTemplateResponse);

            //(if not enough, then) send one more ExitRemoteTemplate request, if another frame is created by a CallTemplate loop at server side
          end;
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
      frClickerActions.ClkVariables.Values[Key] := Copy(OverridenValues.Strings[i], Pos('=', OverridenValues.Strings[i]) + 1, MaxInt);
    end;
  end;

  FPlaying := True;
  FStopAllActionsOnDemand := False;
  try
    Result := PlayAllActions(IsDebugging);
    ListOfVariables.Text := frClickerActions.ClkVariables.Text;  //pass all variables on to next template
  finally
    FPlaying := False;
    //FStopAllActionsOnDemand := False;     //not sure if needed
  end;
end;


procedure TfrClickerActionsArr.SetVariables(ListOfVariables: TStrings);
begin
  frClickerActions.ClkVariables.Clear;
  frClickerActions.ClkVariables.AddStrings(ListOfVariables);
end;


function IsSameListOfProperties(AListOfPropertiesAndValues, AListOfPropertiesAndTypes: TStringList): Boolean;
var
  i: Integer;
begin
  Result := False;
  if AListOfPropertiesAndValues.Count <> AListOfPropertiesAndTypes.Count then
    Exit;

  for i := 0 to AListOfPropertiesAndValues.Count - 1 do
    if AListOfPropertiesAndValues.Names[i] <> AListOfPropertiesAndTypes.Names[i] then
      Exit;

  Result := True;
end;


function RebuildListOfPropertiesAndValuesFromTypes(AInitialListOfPropertiesAndValues, AListOfPropertiesAndTypes: TStringList): string;
var
  i: Integer;
  TempName: string;
begin
  Result := '';

  if AInitialListOfPropertiesAndValues.Count > 0 then
  begin
    for i := 0 to AListOfPropertiesAndTypes.Count - 1 do
    begin
      TempName := AListOfPropertiesAndTypes.Names[i];
      Result := Result + TempName + '=' + AInitialListOfPropertiesAndValues.Values[TempName] + #13#10;
    end;
  end
  else
  begin
    for i := 0 to AListOfPropertiesAndTypes.Count - 1 do
    begin
      TempName := AListOfPropertiesAndTypes.Names[i];
      Result := Result + TempName + '=' + DecodePluginPropertyFromAttribute(AListOfPropertiesAndTypes.ValueFromIndex[i], CPluginPropertyAttr_DefaultValue) + #13#10;
    end;
  end;
end;


procedure TfrClickerActionsArr.SetActionPropertiesFromPlugin(var AAction: TClkActionRec);
var
  ActionPlugin: TActionPlugin;
  ResolvedPluginPath: string;
  ListOfProperties, ListOfPropertiesAndValue_Work: TStringList;
  LoadingResult: Boolean;
begin
  ResolvedPluginPath := ResolveTemplatePath(AAction.PluginOptions.FileName);
  ResolvedPluginPath := EvaluateReplacements(ResolvedPluginPath);

  LoadingResult := False;
  try
    frClickerActions.imglstPluginProperties.Clear;
    frClickerActions.imglstPluginProperties.AddMasked(frClickerActions.imgPluginFileName.Picture.Bitmap, clFuchsia);

    ActionPlugin.Loaded := False;
    LoadingResult := ActionPlugin.LoadToGetProperties(ResolvedPluginPath, DoOnLoadPluginFromInMemFS, DoOnUpdatePropertyIcons, AddToLog);
  except
    on E: Exception do
    begin
      AddToLog('Exception on loading plugin for getting properties: "' + E.Message + '". ' + SysErrorMessage(GetLastOSError));
      frClickerActions.imglstPluginProperties.AddMasked(frClickerActions.imgPlugin.Picture.Bitmap, clFuchsia);
    end;
  end;

  if not LoadingResult then
  begin
    AddToLog('Error loading plugin on getting properties: ' + ActionPlugin.Err);
    Exit;
  end;

  try
    AAction.PluginOptions.ListOfPropertiesAndTypes := ActionPlugin.GetListOfProperties;
  finally
    if not ActionPlugin.Unload(AddToLog) then
      AddToLog('Error unloading plugin on getting properties: ' + ActionPlugin.Err);
  end;

  ListOfProperties := TStringList.Create;
  ListOfPropertiesAndValue_Work := TStringList.Create;
  try
    ListOfProperties.LineBreak := #13#10;
    ListOfPropertiesAndValue_Work.LineBreak := #13#10;
    ListOfProperties.Text := AAction.PluginOptions.ListOfPropertiesAndTypes;
    ListOfPropertiesAndValue_Work.Text := AAction.PluginOptions.ListOfPropertiesAndValues; //values are saved in .clktmpl files
    AAction.PluginOptions.CachedCount := ListOfProperties.Count;

    if not IsSameListOfProperties(ListOfProperties, ListOfPropertiesAndValue_Work) then   //verifies names only
      AAction.PluginOptions.ListOfPropertiesAndValues := RebuildListOfPropertiesAndValuesFromTypes(ListOfPropertiesAndValue_Work, ListOfProperties);
  finally
    ListOfProperties.Free;
    ListOfPropertiesAndValue_Work.Free;
  end;
end;


procedure TfrClickerActionsArr.SetPropertiesFromPlugins;
var
  i: Integer;
begin
  for i := 0 to Length(FClkActions) - 1 do
    if FClkActions[i].ActionOptions.Action = acPlugin then
      SetActionPropertiesFromPlugin(FClkActions[i]);
end;


procedure TfrClickerActionsArr.LoadTemplate_V1(Ini: TClkIniReadonlyFile);
begin
  LoadTemplateToCustomActions_V1(Ini, FClkActions);
end;


procedure TfrClickerActionsArr.LoadTemplate_V2(Ini: TClkIniReadonlyFile);
begin
  LoadTemplateToCustomActions_V2(Ini, FClkActions, FTemplateNotes, FTemplateIconPath);
  SetPropertiesFromPlugins;
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
  ErrMsg, WaitingMsg: string;
begin
  vstActions.Clear; //to reset the node checkboxes

  Fnm := ResolveTemplatePath(Fnm);
  Fnm := EvaluateReplacements(Fnm);

  WaitingMsg := 'Waiting for file availability: ' + Fnm + '   Timeout: ' + IntToStr(CWaitForFileAvailabilityTimeout div 1000) + 's.';
  WaitingMsg := WaitingMsg + #13#10 + 'There is a "stop waiting" button, next to the "Stop action" button.';

  try
    case AFileLocation of      ///////////////////// ToDo:   refactoring !!!!!!!!!!!!!!
      flDisk:
      begin
        if not DoOnFileExists(Fnm) then
        begin
          ErrMsg := 'Template file not found on loading template: "' + Fnm + '"...';
          AddToLog(ErrMsg);
          AppendErrorMessageToActionVar(ErrMsg);
          //memLogErr.Repaint;
          //MessageBox(Handle, PChar(ErrMsg), 'Arr', MB_ICONERROR);
          Exit;
        end;

        Ini := DoOnTClkIniReadonlyFileCreate(Fnm);
        DoOnAddFileNameToRecent(Fnm);
      end;

      flMem:
      begin
        if not AInMemFileSystem.FileExistsInMem(Fnm) then
        begin
          AddToLog(WaitingMsg);
          DoWaitForFileAvailability(Fnm);
        end;

        Ini := CreateIniReadonlyFileFromInMemFileSystem(Fnm, AInMemFileSystem);
      end;

      flDiskThenMem:
        if DoOnFileExists(Fnm) then
        begin
          Ini := DoOnTClkIniReadonlyFileCreate(Fnm);
          DoOnAddFileNameToRecent(Fnm);
        end
        else
        begin
          if not AInMemFileSystem.FileExistsInMem(Fnm) then
          begin
            AddToLog(WaitingMsg);
            DoWaitForFileAvailability(Fnm);
          end;

          Ini := CreateIniReadonlyFileFromInMemFileSystem(Fnm, AInMemFileSystem);
        end;

      flMemThenDisk:
      begin
        if not AInMemFileSystem.FileExistsInMem(Fnm) then
        begin
          AddToLog(WaitingMsg);
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
          DoOnAddFileNameToRecent(Fnm);
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

      FTemplateIconPath := ''; //init here

      if FormatVersion = '1' then
        LoadTemplate_V1(Ini)
      else
        if FormatVersion = '2' then
          LoadTemplate_V2(Ini)
        else
          raise Exception.Create('Unhandled format version: ' + FormatVersion);

      SetTemplateIconHint;
      LoadTemplateIcon;
    finally
      Ini.Free;
    end;
  except
    on E: Exception do
    begin
      if not FExecutingActionFromRemote then
        MessageBox(Handle, PChar('Exception when loading template: ' + E.Message + #13#10 + 'File location: ' + CFileLocationStr[AFileLocation]), PChar(Caption), MB_ICONINFORMATION)
      else
        raise Exception.Create('Exception when loading template: ' + E.Message + #13#10 + 'File location: ' + CFileLocationStr[AFileLocation]);
    end;
  end;

  Modified := True; //trigger a label update
  Modified := False;
  vstActions.RootNodeCount := Length(FClkActions);
  vstActions.Repaint;

  UpdateModifiedLabel(AFileLocation);  //required for first loading, when Modified is still False
  StopGlowingUpdateButton; //required here, because LoadTemplate can be called from parent of frame
end;


function TfrClickerActionsArr.LoadTemplateToActions(Fnm: string; var AActions: TClkActionsRecArr; out ANotes, AIconPath: string; AFileLocation: TFileLocation = flDisk; AInMemFileSystem: TInMemFileSystem = nil; AWaitForFileAvailability: Boolean = False): string;
var
  Ini: TClkIniReadonlyFile;
  FormatVersion: string;
  ActionCount: Integer;
  WaitingMsg: string;
begin
  Result := '';  // '' means no error
  Fnm := ResolveTemplatePath(Fnm);
  Fnm := EvaluateReplacements(Fnm);

  ANotes := '';
  AIconPath := '';

  WaitingMsg := 'Waiting for file availability: ' + Fnm + '   Timeout: ' + IntToStr(CWaitForFileAvailabilityTimeout div 1000) + 's.';
  WaitingMsg := WaitingMsg + #13#10 + 'There is a "stop waiting" button, next to the "Stop action" button.';

  try
    case AFileLocation of      ///////////////////// ToDo:   refactoring !!!!!!!!!!!!!!
      flDisk:
      begin
        if not DoOnFileExists(Fnm) then
        begin
          Result := CREResp_FileNotFound;
          AddToLog(Result + ' ' + Fnm);
          Exit;
        end;

        Ini := DoOnTClkIniReadonlyFileCreate(Fnm);
      end;

      flMem:
      begin
        if not AInMemFileSystem.FileExistsInMem(Fnm) then
        begin
          if AWaitForFileAvailability then
          begin
            AddToLog(WaitingMsg);
            DoWaitForFileAvailability(Fnm);
          end
          else
          begin
            Result := CREResp_FileNotFound;
          AddToLog(Result + ' ' + Fnm);
            Exit;
          end
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
            if AWaitForFileAvailability then
            begin
              AddToLog(WaitingMsg);
              DoWaitForFileAvailability(Fnm);
            end
            else
            begin
              Result := CREResp_FileNotFound;
              AddToLog(Result + ' ' + Fnm);
              Exit;
            end;
          end;

          Ini := CreateIniReadonlyFileFromInMemFileSystem(Fnm, AInMemFileSystem);
        end;

      flMemThenDisk:
      begin
        if not AInMemFileSystem.FileExistsInMem(Fnm) then
          if AWaitForFileAvailability then
          begin
            AddToLog(WaitingMsg);
            DoWaitForFileAvailability(Fnm);
          end;

        if AInMemFileSystem.FileExistsInMem(Fnm) then
          Ini := CreateIniReadonlyFileFromInMemFileSystem(Fnm, AInMemFileSystem)
        else
        begin
          if not DoOnFileExists(Fnm) then
          begin
            Result := CREResp_FileNotFound;
            AddToLog(Result + ' ' + Fnm);
            Exit;
          end;

          Ini := DoOnTClkIniReadonlyFileCreate(Fnm);
        end;
      end;
    end;
    try
      ActionCount := Ini.ReadInteger('Actions', 'Count', 0);
      SetLength(AActions, ActionCount);

      FormatVersion := Ini.ReadString('Actions', 'Version', '1');

      if FormatVersion = '1' then
        LoadTemplateToCustomActions_V1(Ini, AActions)
      else
        if FormatVersion = '2' then
          LoadTemplateToCustomActions_V2(Ini, AActions, ANotes, AIconPath)
        else
          //raise Exception.Create('Unhandled format version: ' + FormatVersion)
          ;
    finally
      Ini.Free;
    end;
  except
    on E: Exception do
    begin
      Result := 'Ex on loading template: ' + E.Message;
      AddToLog(Result);
    end;
  end;
end;


procedure TfrClickerActionsArr.SaveTemplateWithCustomActions_V2(Fnm: string; var ACustomClkActions: TClkActionsRecArr; ANotes, ATemplateIconPath: string);
var
  AStringList: TStringList;   //much faster than T(Mem)IniFile
begin
  AStringList := TStringList.Create;
  try
    AStringList.LineBreak := #13#10;
    SaveTemplateWithCustomActionsToStringList_V2(AStringList, ACustomClkActions, ANotes, ATemplateIconPath);
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
  SaveTemplateWithCustomActions_V2(Fnm, FClkActions, FTemplateNotes, FTemplateIconPath);
end;


procedure TfrClickerActionsArr.SaveTemplateAs1Click(Sender: TObject);
begin
  SaveTemplateWithDialog;
end;


procedure TfrClickerActionsArr.vstActionsChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: boolean);
begin
  Allowed := Node^.Parent = vstActions.RootNode;
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
  if Node^.Parent <> vstActions.RootNode then
  begin
    Allowed := False;
    Exit;
  end;

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
  //changing the vstActions.StateImages here, does nothing
end;


procedure TfrClickerActionsArr.vstActionsGetImageIndexEx(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
  var ImageList: TCustomImageList);
var
  CurrentAction: PClkActionRec;
  NodeData: PActionNodeRec;
begin
  if Node^.Parent = vstActions.RootNode then
    CurrentAction := @FClkActions[Node^.Index]
  else
  begin
    NodeData := vstActions.GetNodeData(Node);
    if not Assigned(NodeData) then
    begin
      ImageIndex := -1;
      Exit;
    end;

    CurrentAction := @NodeData^.Action;
  end;

  //if Column = 0 then         //if changing the vstActions.StateImages here, the UI becomes semi-responsive, because the tree calls Invalidate or InvalidateNode
  //  vstActions.StateImages := imglstCurrentDebuggingAction52x26
  //else
  //  vstActions.StateImages := frClickerActions.imglstActions;

  case Column of
    0:  //action name
    begin
      if not CurrentAction^.ActionBreakPoint.Exists then
        ImageList := imglstCurrentDebuggingAction
      else
      begin
        if CurrentAction^.ActionBreakPoint.Enabled then
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

      case CurrentAction^.ActionDebuggingStatus of
        adsNone:    ImageIndex := 0;   //no arrow
        adsPrev:    ImageIndex := 1;   //lower left arrow
        adsCurrent: ImageIndex := 2;   //full arrow
        adsNext:    ImageIndex := 3;   //upper left arrow
      end;
    end;

    1:  //action type
    begin
      ImageList := frClickerActions.imglstActions;
      ImageIndex := Ord(CurrentAction^.ActionOptions.Action);
    end;

    2:  //status (+ timeout)
    begin
      if CurrentAction^.ActionSkipped then
      begin
        ImageList := imglstActionExtraStatus;
        ImageIndex := Ord(CurrentAction^.ActionStatus);
      end
      else
      begin
        ImageList := imglstActionStatus;
        ImageIndex := Ord(CurrentAction^.ActionStatus);
      end;
    end;

    3:  //condition / misc
    begin
      ImageList := imglstActionHasCondition;
      ImageIndex := Ord(CurrentAction^.ActionOptions.ActionCondition > '');
    end;

    4:  //text
    begin
      if CurrentAction^.ActionOptions.Action = acEditTemplate then
      begin
        ImageList := frClickerActions.imglstActions;
        ImageIndex := Ord(CurrentAction^.EditTemplateOptions.EditedActionType);
      end;
    end;
  end;
end;


function GetVSTMiscDisplayedInfoByAction(AActionRec: TClkActionRec): string;
begin
  case AActionRec.ActionOptions.Action of
    acClick: Result := CXClickPointReference[AActionRec.ClickOptions.XClickPointReference] + '+' + AActionRec.ClickOptions.XOffset + ' : ' + CYClickPointReference[AActionRec.ClickOptions.YClickPointReference] + '+' + AActionRec.ClickOptions.YOffset + '  Count=' + IntToStr(AActionRec.ClickOptions.Count);
    acExecApp: Result := '"' + AActionRec.ExecAppOptions.PathToApp + '"' + FastReplace_ReturnTo45(AActionRec.ExecAppOptions.ListOfParams);
    acFindControl: Result := 'Match: ' + MatchFindControlCriteriaToString(AActionRec.FindControlOptions.MatchCriteria) + '   Text="' + AActionRec.FindControlOptions.MatchText + '"  Class="' + AActionRec.FindControlOptions.MatchClassName + '"';
    acFindSubControl: Result := 'Match: ' + MatchFindSubControlCriteriaToString(AActionRec.FindSubControlOptions.MatchCriteria);
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
var
  CurrentAction: PClkActionRec;
  NodeData: PActionNodeRec;
begin
  try
    if Node^.Parent = vstActions.RootNode then
      CurrentAction := @FClkActions[Node^.Index]
    else
    begin
      NodeData := vstActions.GetNodeData(Node);
      if not Assigned(NodeData) then
      begin
        CellText := 'N/A';
        Exit;
      end;

      CurrentAction := @NodeData^.Action;
    end;

    case Column of
      0: CellText := CurrentAction^.ActionOptions.ActionName;
      1:
      begin
        if CurrentAction.ActionOptions.Action <= High(TClkAction) then
          CellText := CClkActionStr[CurrentAction.ActionOptions.Action]
        else
          CellText := '[out of range]';  //bug
      end;

      2: CellText := IntToStr(CurrentAction.ActionOptions.ActionTimeout);
      3: CellText := GetVSTMiscDisplayedInfoByAction(CurrentAction^);
      4:
      begin
        case CurrentAction.ActionOptions.Action of
          acClick: CellText := IntToStr(CurrentAction.ClickOptions.ClickType);
          acExecApp: CellText := ExtractFileName(CurrentAction.ExecAppOptions.PathToApp);
          acFindControl: CellText := CurrentAction.FindControlOptions.MatchText + ' / ' + CurrentAction.FindControlOptions.MatchClassName + ' / ' + BoolToStr(CurrentAction.FindControlOptions.UseWholeScreen, '[WholeScreen]', '');
          acFindSubControl: CellText := CurrentAction.FindSubControlOptions.MatchText + ' / ' + BoolToStr(CurrentAction.FindSubControlOptions.UseWholeScreen, '[WholeScreen]', '');
          acSetControlText: CellText := CurrentAction.SetTextOptions.Text;
          acCallTemplate: CellText := ExtractFileName(CurrentAction.CallTemplateOptions.TemplateFileName);
          acSleep: CellText := CurrentAction.SleepOptions.Value;
          acSetVar: CellText := FastReplace_ReturnTo45(CurrentAction.SetVarOptions.ListOfVarNames);
          acWindowOperations: CellText := IntToStr(Ord(CurrentAction.WindowOperationsOptions.Operation));
          acLoadSetVarFromFile: CellText := CurrentAction.LoadSetVarFromFileOptions.SetVarActionName + '  from "' + CurrentAction.LoadSetVarFromFileOptions.FileName + '"';
          acSaveSetVarToFile: CellText := CurrentAction.SaveSetVarToFileOptions.SetVarActionName + '  from "' + CurrentAction.SaveSetVarToFileOptions.FileName + '"';
          acPlugin: CellText := CurrentAction.PluginOptions.FileName;
          acEditTemplate: CellText := CEditTemplateOperationStr[CurrentAction.EditTemplateOptions.Operation] + ' (' + CEditTemplateWhichTemplateStr[CurrentAction.EditTemplateOptions.WhichTemplate] + ')';
        end;
      end;
      5: CellText := StringReplace(CurrentAction.FindSubControlOptions.MatchBitmapFiles, #13#10, ', ', [rfReplaceAll]);
      6: CellText := IntToStr(Node^.Index);
    end;
  except
    CellText := 'bug';
  end;
end;


procedure TfrClickerActionsArr.UpdateNodeCheckStateFromAction(Node: PVirtualNode);
const
  CCheckStates: array[Boolean] of TCheckState = (csUnCheckedNormal, csCheckedNormal);
var
  CurrentAction: PClkActionRec;
  NodeData: PActionNodeRec;
begin
  if Node^.Parent = vstActions.RootNode then
    CurrentAction := @FClkActions[Node^.Index]
  else
  begin
    NodeData := vstActions.GetNodeData(Node);
    if not Assigned(NodeData) then
    begin
      Node.CheckState := csMixedPressed;
      Exit;
    end;

    CurrentAction := @NodeData^.Action;
  end;

  Node.CheckState := CCheckStates[CurrentAction^.ActionOptions.ActionEnabled];
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

  CopyActionContent(FClkActions[Node^.Index - 1], Ph);
  CopyActionContent(FClkActions[Node^.Index], FClkActions[Node^.Index - 1]);
  CopyActionContent(Ph, FClkActions[Node^.Index]);

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

  CopyActionContent(FClkActions[Node^.Index + 1], Ph);
  CopyActionContent(FClkActions[Node^.Index], FClkActions[Node^.Index + 1]);
  CopyActionContent(Ph, FClkActions[Node^.Index]);

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
  spdbtnStopPlaying.Enabled := True;  //It's ugly that the button has to be enabled here, but it is required to stay "on" for a little longer, to be captured by the test driver.
  spdbtnStopPlaying.Repaint;
  try
    PrepareFilesInServer;
  finally
    PlayAllActionsFromButton(False);
  end;
end;


procedure TfrClickerActionsArr.spdbtnPlaySelectedActionClick(Sender: TObject);
begin
  if not FPlaying then
    FDebugging := False;

  spdbtnStopPlaying.Enabled := True;  //It's ugly that the button has to be enabled here, but it is required to stay "on" for a little longer, to be captured by the test driver.
  spdbtnStopPlaying.Repaint;
  try
    PrepareFilesInServer;
  finally
    PlaySelectedActionFromButton;
  end;
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
  if Length(FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText) = 0 then
  begin
    AFont.Name := 'Tahoma';
    AFont.Size := 8;
    AFont.Style := [];
    Exit;
  end;

  AFont.Name := EvaluateReplacements(FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[0].FontName);
  AFont.Size := FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[0].FontSize;
  AFont.Style := [];

  if FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[0].Bold then
    AFont.Style := AFont.Style + [fsBold];

  if FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[0].Italic then
    AFont.Style := AFont.Style + [fsItalic];

  if FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[0].Underline then
    AFont.Style := AFont.Style + [fsUnderline];

  if FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[0].StrikeOut then
    AFont.Style := AFont.Style + [fsStrikeOut];

  AFont.Color := HexToInt(EvaluateReplacements(FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText[0].ForegroundColor));
end;


procedure TfrClickerActionsArr.vstActionsPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  try
    case Column of
      4:
      begin
        if FClkActions[Node^.Index].ActionOptions.Action = acFindControl then
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
        end;

        if FClkActions[Node^.Index].ActionOptions.Action = acFindSubControl then
          SetAFontFromClkActions(TargetCanvas.Font, Node^.Index)
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
        if FClkActions[Node^.Index].ActionOptions.Action = acFindControl then
          if FClkActions[Node^.Index].FindControlOptions.UseWholeScreen then
            TargetCanvas.Brush.Color := clLime;

        if FClkActions[Node^.Index].ActionOptions.Action = acFindSubControl then
        begin
          if FClkActions[Node^.Index].FindSubControlOptions.MatchCriteria.WillMatchBitmapText then
            TargetCanvas.Brush.Color := HexToInt(EvaluateReplacements(FClkActions[Node^.Index].FindSubControlOptions.MatchBitmapText[0].BackgroundColor));

          if FClkActions[Node^.Index].FindSubControlOptions.UseWholeScreen then
            TargetCanvas.Brush.Color := clLime;
        end;
      except
        TargetCanvas.Brush.Color := clRed;
      end;

      TargetCanvas.Rectangle(CellRect);
    end;

    else
      if Node^.Parent <> vstActions.RootNode then
      begin
        TargetCanvas.Pen.Color := clWindow;
        TargetCanvas.Brush.Color := cl3DLight;
        TargetCanvas.Rectangle(CellRect);
      end;
  end;
end;


procedure TfrClickerActionsArr.ToggleBreakpoint(ACurrentAction: PClkActionRec);
begin
  ACurrentAction^.ActionBreakPoint.Exists := not ACurrentAction^.ActionBreakPoint.Exists;
  if ACurrentAction^.ActionBreakPoint.Exists then
    ACurrentAction^.ActionBreakPoint.Enabled := True;

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
  MinImgX, MaxImgX: Integer;
  NodeLevel, Indent: Integer;
  CurrentAction: PClkActionRec;
  NodeData: PActionNodeRec;
begin
  FActionsHitTimeStamp := GetTickCount64; //required on MouseUp
  FPreviousSelectedNode := vstActions.GetFirstSelected;

  vstActions.GetHitTestInfoAt(X, Y, True, FActionsHitInfo);

  ColumnOffSet := vstActions.Header.Columns.Items[6].Width * Ord(coVisible in vstActions.Header.Columns.Items[6].Options);

  Indent := vstActions.Indent;
  MinImgX := 24 + ColumnOffSet + Indent;
  MaxImgX := 48 + ColumnOffSet + Indent;
  NodeLevel := 0;
  if FActionsHitInfo.HitNode <> nil then
  begin
    NodeLevel := vstActions.GetNodeLevel(FActionsHitInfo.HitNode);
    Inc(MinImgX, NodeLevel * Indent);
    Inc(MaxImgX, NodeLevel * Indent);
  end;

  if (FActionsHitInfo.HitColumn = 0) and (FActionsHitInfo.HitNode <> nil) and (X >= MinImgX) and (X < MaxImgX) then
  begin
    if FActionsHitInfo.HitNode^.Parent = vstActions.RootNode then
      CurrentAction := @FClkActions[FActionsHitInfo.HitNode^.Index]
    else
    begin
      NodeData := vstActions.GetNodeData(FActionsHitInfo.HitNode);
      if not Assigned(NodeData) then
        Exit;

      CurrentAction := @NodeData^.Action;
    end;

    case Button of   //click on breakpoint
      mbLeft:
        ToggleBreakpoint(CurrentAction);

      mbRight:
      begin
        MenuItemEnableDisableBreakPoint.Caption := CEnableDisableText[not CurrentAction^.ActionBreakPoint.Enabled] + ' breakpoint';
        MenuItemEnableDisableBreakPoint.Enabled := CurrentAction^.ActionBreakPoint.Exists;
        MenuItem_EditBreakPoint.Enabled := CurrentAction^.ActionBreakPoint.Exists;
        MenuItemEnableDisableBreakPoint.Tag := {%H-}PtrInt(CurrentAction);

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
const
  CControlsModifiedMsg = 'There are changed properties for the selected action. By selecting another action, you will discard those changes. You can click Update to record the changes to the action list.'#13#10#13#10'Go back to previous action/content?';
  CModifiedPmtvFilesMsg = 'One or more primitives files are modified for the selected action. By selecting another action, you will discard those changes.'#13#10#13#10'Go back to previous action/content?';

  function ReturnToModifiedGenericAction: Boolean; //returns True if should return
  begin
    Result := False;

    if frClickerActions.ControlsModified then
      if MessageBox(Handle, PChar(CControlsModifiedMsg), '', MB_ICONWARNING + MB_YESNO) = IDYES then   //YES means go back to previous action.
      begin
        vstActions.ClearSelection;
        if FPreviousSelectedNode <> nil then
          vstActions.Selected[FPreviousSelectedNode] := True;

        Result := True;
      end;
  end;

  function ReturnToModifiedFindSubControlWithPmtvAction: Boolean; //returns True if should return
  begin
    Result := False;

    if frClickerActions.ModifiedPmtvFiles then
      if MessageBox(Handle, PChar(CModifiedPmtvFilesMsg), '', MB_ICONWARNING + MB_YESNO) = IDYES then   //YES means go back to previous action.
      begin
        vstActions.ClearSelection;
        if FPreviousSelectedNode <> nil then
          vstActions.Selected[FPreviousSelectedNode] := True;

        Result := True;
      end
      else
        frClickerActions.ResetAllPmtvModifiedFlags;  //reset the flags, so that next time this action is selected, the files won't appear as modified
  end;

var
  Node: PVirtualNode;
begin
  Node := vstActions.GetFirstSelected;

  if Node = nil then  //no action is selected, so verify if an action just got deselected
  begin
    if not ReturnToModifiedGenericAction and not ReturnToModifiedFindSubControlWithPmtvAction then
    begin
      frClickerActions.CurrentlyEditingActionType := {%H-}TClkAction(CClkUnsetAction); //use an invalid value, to hide all editors
      StopGlowingUpdateButton;
      frClickerActions.UpdatePageControlActionExecutionIcons;
    end;

    Exit;
  end;

  if FActionsHitInfo.HitNode = nil then
    Exit;

  if Node^.Parent <> vstActions.RootNode then
    Exit;

  if ReturnToModifiedGenericAction then
    Exit;

  if ReturnToModifiedFindSubControlWithPmtvAction then
    Exit;

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
var
  Node: PVirtualNode;
  PasteIndex: Integer;
begin
  if ssCtrl in Shift then
  begin
    case Key of
      Ord('C'):
        CopySelectedActionsToClipboard;

      Ord('V'):
      begin
        Node := nil;
        PasteIndex := -1;

        if ssShift in Shift then
        begin
          Node := vstActions.GetFirstSelected;
          PasteIndex := Node^.Index;
        end;

        PasteActionsFromClipboard(PasteIndex);
      end
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


procedure TfrClickerActionsArr.vstActionsDragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Node^.Parent = vstActions.RootNode;
end;


procedure TfrClickerActionsArr.vstActionsDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
  Mode: TDropMode; var Effect: DWORD; var Accept: Boolean);
var
  Node, SrcNode: PVirtualNode;
begin
  Accept := False;

  if Sender <> Source then
    Exit;

  Node := (Sender as TVirtualStringTree).DropTargetNode;
  SrcNode := (Source as TVirtualStringTree).FocusedNode;

  Accept := Node <> SrcNode;
end;


procedure TfrClickerActionsArr.vstActionsDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: DWORD; Mode: TDropMode);
var
  Node, SrcNode: PVirtualNode;
  DestIdx: Integer;
begin
  if Sender <> Source then
    Exit;

  Node := (Sender as TVirtualStringTree).DropTargetNode;
  SrcNode := (Source as TVirtualStringTree).FocusedNode;

  if not Assigned(Node) or not Assigned(SrcNode) then
    Exit;

  DestIdx := Node^.Index;

  MoveAction(SrcNode, Node);
  Node := GetNodeByIndex(DestIdx);

  if Node <> nil then
  begin
    UpdateNodesCheckStateFromActions;

    vstActions.ClearSelection;
    vstActions.Selected[Node] := True;
    vstActions.ScrollIntoView(Node, True);
  end;

  vstActions.Repaint;
  Modified := True;
end;


procedure TfrClickerActionsArr.MoveAction(ASrcNode, ADestNode: PVirtualNode);  ///////////// there is a bug, which causes AV
var
  Ph: TClkActionRec;
  SrcIdx, DestIdx: Integer;
begin
  if ASrcNode = ADestNode then
    Exit;

  SrcIdx := ASrcNode^.Index;
  DestIdx := ADestNode^.Index;

  CopyActionContent(FClkActions[SrcIdx], Ph);
  RemoveAction(SrcIdx, False);
  InsertAction(DestIdx, Ph);
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
              frClickerActions.EditingAction^.FindControlOptions.MatchCriteria.WillMatchClassName) then
      begin
        MessageBox(Handle, 'To find a control, at least one match criterion has to be checked.', PChar(Application.Title), MB_ICONINFORMATION);
        Exit;
      end;

    acFindSubControl:
      if not (frClickerActions.EditingAction^.FindSubControlOptions.MatchCriteria.WillMatchBitmapText or
              frClickerActions.EditingAction^.FindSubControlOptions.MatchCriteria.WillMatchBitmapFiles or
              frClickerActions.EditingAction^.FindSubControlOptions.MatchCriteria.WillMatchPrimitiveFiles) then
      begin
        MessageBox(Handle, 'To find a subcontrol, at least one match criterion has to be checked.', PChar(Application.Title), MB_ICONINFORMATION);
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

  FClkActions[n].ActionOptions.Action := acClick; //some default
  UpdateActionsArrFromControls(n);
  FClkActions[n].ActionStatus := asNotStarted;
  FClkActions[n].ActionOptions.ActionEnabled := True;

  vstActions.BeginUpdate;
  try
    vstActions.Clear;  //to reinit nodes
    vstActions.RootNodeCount := Length(FClkActions);
    vstActions.Repaint;
    Modified := True;
  finally
    vstActions.EndUpdate;
  end;

  Node := vstActions.GetLast;
  vstActions.Selected[Node] := True;
  vstActions.ScrollIntoView(Node, True);
  StopGlowingUpdateButton;
  frClickerActions.UpdatePageControlActionExecutionIcons;
end;


procedure TfrClickerActionsArr.btnNewClick(Sender: TObject);
begin
  if MessageBox(Handle, 'Are you sure you want to remove all actions from list and clear all controls?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = ID_YES then
  begin
    ClearAllActions;
    frClickerActions.ClearControls;
    FTemplateNotes := '';
    FTemplateIconPath := '';
    DrawDefaultTemplateIcon;
  end;
end;


procedure TfrClickerActionsArr.AddCalledTemplateToNode(ANode: PVirtualNode; ATemplateFileName, ATemplateDir, AAppDir: string; ARecursionLevel: Integer);
var
  Node: PVirtualNode;
  ChNode: PVirtualNode;
  NodeData: PActionNodeRec;
  ChNodeData: PActionNodeRec;
  CalledTemplateContent: TClkActionsRecArr;
  i: Integer;
  CalledTemplateFileName, SelfFileName: string;
  DummyNotes, DummyIconPath: string;
begin
  NodeData := vstActions.GetNodeData(ANode);
  NodeData^.FullTemplatePath := EvaluateReplacements(ATemplateFileName);
  NodeData^.FullTemplatePath := ResolveTemplatePath(NodeData^.FullTemplatePath, ATemplateDir, AAppDir);

  if NodeData^.FullTemplatePath = ExtractFileName(NodeData^.FullTemplatePath) then //there is no full path
    NodeData^.FullTemplatePath := FFullTemplatesDir + PathDelim + NodeData^.FullTemplatePath;

  SelfFileName := NodeData^.FullTemplatePath;

  LoadTemplateToActions(SelfFileName, CalledTemplateContent, DummyNotes, DummyIconPath);

  for i := 0 to Length(CalledTemplateContent) - 1 do
  begin
    ChNode := vstActions.AddChild(ANode);
    ChNodeData := vstActions.GetNodeData(ChNode);
    CopyActionContent(CalledTemplateContent[i], ChNodeData^.Action);
    UpdateNodeCheckStateFromAction(ChNode);
  end;

  SetLength(CalledTemplateContent, 0);

  if ARecursionLevel > 20 then
    Exit;

  Node := ANode.FirstChild;
  if Node = nil then
    Exit;

  repeat
    NodeData := vstActions.GetNodeData(Node);
    if NodeData^.Action.ActionOptions.Action = acCallTemplate then
    begin
      CalledTemplateFileName := NodeData^.Action.CallTemplateOptions.TemplateFileName;
      CalledTemplateFileName := StringReplace(CalledTemplateFileName, '$SelfTemplateDir$', ExtractFileDir(SelfFileName), [rfReplaceAll]);
      AddCalledTemplateToNode(Node, CalledTemplateFileName, ATemplateDir, AAppDir, ARecursionLevel + 1);
    end;

    Node := Node^.NextSibling;
  until Node = nil;
end;


procedure TfrClickerActionsArr.chkDisplayCalledTemplatesChange(Sender: TObject);
var
  Node: PVirtualNode;
  TemplateDir, AppDir, CalledTemplateFileName: string;
begin
  Node := vstActions.GetFirst;
  if Node = nil then
    Exit;

  repeat
    if Node^.ChildCount > 0 then
      vstActions.DeleteChildren(Node);

    Node := Node^.NextSibling;
  until Node = nil;

  if not chkDisplayCalledTemplates.Checked then
    Exit;

  Node := vstActions.GetFirst;
  {if Pos('$SelfTemplateDir$', FFileName) > 0 then
    TemplateDir := StringReplace(FFileName, '$SelfTemplateDir$', ExtractFileDir(FFileName), [rfReplaceAll])
  else}
    TemplateDir := ExtractFileDir(FFileName);

  AppDir := ExtractFileDir(ParamStr(0));
  repeat
    if FClkActions[Node^.Index].ActionOptions.Action = acCallTemplate then
    begin
      CalledTemplateFileName := FClkActions[Node^.Index].CallTemplateOptions.TemplateFileName;
      CalledTemplateFileName := StringReplace(CalledTemplateFileName, '$SelfTemplateDir$', ExtractFileDir(FFileName), [rfReplaceAll]);
      AddCalledTemplateToNode(Node, CalledTemplateFileName, TemplateDir, AppDir, 0);
    end;

    Node := Node^.NextSibling;
  until Node = nil;
end;


procedure TfrClickerActionsArr.edtConsoleCommandExit(Sender: TObject);
begin
  if Assigned(frmAutoComplete) and not frmAutoComplete.Focused and frmAutoComplete.Visible then
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


procedure TfrClickerActionsArr.ShowAutoCompleteWindow(AEdit: TEdit);
var
  TempVars: TStringList;
  TempFuncs: TStringList;
begin
  FillInVarAndFuncDescriptions;

  TempVars := TStringList.Create;
  TempFuncs := TStringList.Create;
  try
    TempVars.LineBreak := #13#10;
    TempFuncs.LineBreak := #13#10;
    FillInWithAllVars(TempVars);
    FillInWithAllFuncs(TempFuncs);
    ShowAutoComplete(AEdit, TempVars, TempFuncs, FVarDescriptions, FFuncDescriptions);
  finally
    TempVars.Free;
    TempFuncs.Free;
  end;

  if AutoCompleteVisible then
    ShowAutoComplete(AEdit, nil, nil, FVarDescriptions, FFuncDescriptions);
end;


procedure TfrClickerActionsArr.edtConsoleCommandKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_SPACE then
    if ssCtrl in Shift then
    begin
      Key := 0;
      ShowAutoCompleteWindow(edtConsoleCommand);
    end;

  if AutoCompleteVisible then
    UpdateAutoComplete(edtConsoleCommand);
end;


procedure TfrClickerActionsArr.MenuItemCopySelectedActionsToClipboardClick(
  Sender: TObject);
begin
  CopySelectedActionsToClipboard;
end;


procedure TfrClickerActionsArr.MenuItemPasteActionsFromClipboardAtTheEndClick(
  Sender: TObject);
begin
  PasteActionsFromClipboard(-1);
end;


procedure TfrClickerActionsArr.MenuItem_AddACallTemplateByFileClick(
  Sender: TObject);
begin
  if not DoOnOpenDialogExecute(CTemplateDialogFilter) then
    Exit;

  InsertCallTemplateForActionReplacing(vstActions.RootNodeCount, DoOnGetOpenDialogFileName);

  vstActions.Repaint;

  if not frClickerActions.ControlsModified then  //do not reload content if modified
  begin
    vstActions.ClearSelection;
    LoadActionIntoEditorByIndex(vstActions.RootNodeCount - 1);
    StopGlowingUpdateButton;
  end;

  Modified := True;
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


procedure TfrClickerActionsArr.SetTemplateIconHint;
begin
  imgTemplateIcon.Hint := 'Template icon:' + #13#10 + FTemplateIconPath;
end;


procedure TfrClickerActionsArr.LoadTemplateIcon;
var
  ResolvedPath: string;
begin
  ResolvedPath := ResolveTemplatePath(FTemplateIconPath);

  try
    imgTemplateIcon.Picture.Bitmap.Width := imgTemplateIcon.Width;
    imgTemplateIcon.Picture.Bitmap.Height := imgTemplateIcon.Height;

    if DoOnFileExists(ResolvedPath) then
      DoOnLoadBitmap(imgTemplateIcon.Picture.Bitmap, ResolvedPath)
    else
    begin
      if ResolvedPath <> '' then
      begin
        imgTemplateIcon.Picture.Bitmap.Canvas.Font.Color := clGreen;
        imgTemplateIcon.Picture.Bitmap.Canvas.Brush.Color := clYellow;
        imgTemplateIcon.Picture.Bitmap.Canvas.TextOut(1, 1, 'Not');
        imgTemplateIcon.Picture.Bitmap.Canvas.TextOut(1, 16, 'found');
      end
      else
        DrawDefaultTemplateIcon;
    end;
  except
    on E: Exception do
    begin
      imgTemplateIcon.Canvas.Font.Color := clRed;
      imgTemplateIcon.Canvas.Brush.Color := clWhite;
      imgTemplateIcon.Canvas.TextOut(10, 10, 'Error');
      imgTemplateIcon.Hint := imgTemplateIcon.Hint + #13#10 + E.Message;
    end;
  end;
end;


procedure TfrClickerActionsArr.MenuItem_BrowseTemplateIconClick(Sender: TObject);
begin
  if not DoOnOpenDialogExecute('Supported formats (*.exe; *.bmp; *.png; *.jpg; *.jpeg; *.ico)|*.exe; *.bmp; *.png; *.jpg; *.jpeg; *.ico|' +
                               'Executable files (*.exe)|*.exe|' +
                               'Bitmap files (*.bmp)|*.bmp|' +
                               'Png files (*.png)|*.png|' +
                               'Jpeg files (*.jpg; *.jpeg)|*.jpg; *.jpeg|' +
                               'Icons (*.ico)|*.ico|' +
                               'All files (*.*)|*.*') then
    Exit;

  FTemplateIconPath := DoOnGetOpenDialogFileName;
  FTemplateIconPath := ResolveTemplatePath(FTemplateIconPath);  //make sure it is resolved, before encoding
  FTemplateIconPath := EncodeTemplatePath(FTemplateIconPath);

  SetTemplateIconHint;
  LoadTemplateIcon;
  Modified := True;
end;


procedure TfrClickerActionsArr.MenuItem_CopyFilenameToClipboardClick(
  Sender: TObject);
begin
  Clipboard.AsText := ExtractFileName(FFileName);
end;


procedure TfrClickerActionsArr.MenuItem_CopyFullFilepathToClipboardClick(
  Sender: TObject);
begin
  Clipboard.AsText := FFileName;
end;


procedure TfrClickerActionsArr.MenuItemEnableDisableBreakPointClick(
  Sender: TObject);
var
  ClkActionRec: PClkActionRec;
begin
  if FActionsHitInfo.HitNode = nil then
  begin
    MessageBox(Handle, 'Clicked on an invalid breakpoint space.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  ClkActionRec := {%H-}PClkActionRec(MenuItemEnableDisableBreakPoint.Tag);
  if ClkActionRec = nil then
    Exit;

  ClkActionRec^.ActionBreakPoint.Enabled := not ClkActionRec^.ActionBreakPoint.Enabled;
  vstActions.InvalidateNode(FActionsHitInfo.HitNode);
  Modified := True;
end;


procedure TfrClickerActionsArr.MenuItemPasteActionsFromClipboardAfterTheFirstSelectedClick
  (Sender: TObject);
var
  Node: PVirtualNode;
begin
  if vstActions.RootNodeCount = 0 then
  begin
    PasteActionsFromClipboard(-1);
    Exit;
  end;

  Node := vstActions.GetFirstSelected;
  if Node = nil then
  begin
    MessageBox(Handle, 'No action selected. Please select one.', PChar(Application.Title), MB_ICONINFORMATION);
    Exit;
  end;

  PasteActionsFromClipboard(Node^.Index);
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


procedure TfrClickerActionsArr.MenuItem_GetGenericHTTPRequestFromActionClick
  (Sender: TObject);
var
  Node: PVirtualNode;
  ActionType: TClkAction;
  Request, Properties: string;
begin
  Node := vstActions.GetFirstSelected;
  if Node = nil then
    if vstActions.RootNodeCount > 0 then
    begin
      MessageBox(Handle, 'Please select an action first.', PChar(Application.Title), MB_ICONINFORMATION);
      Exit;
    end;

  Request := '';
  repeat
    if vstActions.Selected[Node] then
    begin
      if Request > '' then
        Request := Request + #13#10;  //Add CRLF only if there are multiple selected actions.

      ActionType := FClkActions[Node^.Index].ActionOptions.Action;
      Request := Request + 'http://127.0.0.1:' + IntToStr(DoOnGetListeningPort) + '/' +
                           'Execute' + CClkActionStr[ActionType] + 'Action?' +
                           CREParam_StackLevel + '=0';

      if (Sender = MenuItem_GetHTTPRequestFromActionModifiedOnly) or
         (Sender = MenuItem_GetHTTPRequestFromActionModifiedOnlyWithSrvDbg) then    //different than default properties
      begin
        Properties := GetDifferentThanDefaultActionPropertiesByType(FClkActions[Node^.Index], True);
        if Properties > '' then
          Request := Request + '&' + Properties;
      end;

      if (Sender = MenuItem_GetHTTPRequestFromActionAllProperties) or
         (Sender = MenuItem_GetHTTPRequestFromActionAllPropertiesWithSrvDbg) then    //all properties
        Request := Request + '&' + GetActionPropertiesByType(FClkActions[Node^.Index], True);

      if ActionType in [acFindControl, acFindSubControl, acCallTemplate] then
        Request := Request + '&' + CREParam_FileLocation + '=' + CREParam_FileLocation_ValueDisk;

      if (Sender = MenuItem_GetHTTPRequestFromActionAllPropertiesWithSrvDbg) or
         (Sender = MenuItem_GetHTTPRequestFromActionModifiedOnlyWithSrvDbg) then    //debugging
      begin
        if ActionType in [acPlugin, acCallTemplate] then
          Request := Request + '&' + CREParam_IsDebugging + '=1';  //required by plugin to be able to step into

        if ActionType = acCallTemplate then
          Request := Request + '&' + CREParam_UseLocalDebugger + '=1';  //debugging at server side, instead of being controlled by a client

        Request := Request + '&' + CREParam_UseServerDebugging + '=1';
      end;

      Request := Request + '&' + CPropertyName_ActionName + '=' + FClkActions[Node^.Index].ActionOptions.ActionName; //implemented for some of the actions only
      Request := Request + '&' + CPropertyName_ActionTimeout + '=' + IntToStr(FClkActions[Node^.Index].ActionOptions.ActionTimeout);  //required by a few actions only
    end;

    Node := Node^.NextSibling;
  until Node = nil;

  Clipboard.AsText := Request;
end;


procedure TfrClickerActionsArr.MenuItem_GetHTTPRequestFromActionClick(
  Sender: TObject);
begin
  MenuItem_GetHTTPRequestFromActionModifiedOnlyWithSrvDbg.Bitmap := MenuItem_SetActionStatusTo.Bitmap;
  MenuItem_GetHTTPRequestFromActionAllPropertiesWithSrvDbg.Bitmap := MenuItem_SetActionStatusTo.Bitmap;
end;


procedure TfrClickerActionsArr.MenuItem_PlayActionAndRestoreVarsClick(
  Sender: TObject);
var
  BackupList: TStringList;
begin
  BackupList := TStringList.Create;
  try
    BackupList.LineBreak := #13#10;
    BackupList.Text := frClickerActions.ClkVariables.Text;
    try
      spdbtnStopPlaying.Enabled := True;  //It's ugly that the button has to be enabled here, but it is required to stay "on" for a little longer, to be captured by the test driver.
      spdbtnStopPlaying.Repaint;
      try
        PrepareFilesInServer;
      finally
        PlaySelected;
      end;
    finally
      frClickerActions.ClkVariables.Text := BackupList.Text;
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
    CopyActionContent(FClkActions[i - 1], FClkActions[i]); //FClkActions[i] := FClkActions[i - 1];

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
    CopyActionContent(FClkActions[i - 1], FClkActions[i]); //FClkActions[i] := FClkActions[i - 1];

  FClkActions[AIndexToInsertAt].ActionDebuggingStatus := adsNone;
  FClkActions[AIndexToInsertAt].ActionOptions.Action := acSetVar;
  FClkActions[AIndexToInsertAt].ActionOptions.ActionCondition := '';
  FClkActions[AIndexToInsertAt].ActionOptions.ActionEnabled := True;
  FClkActions[AIndexToInsertAt].ActionOptions.ActionName := AActionName;
  FClkActions[AIndexToInsertAt].ActionOptions.ActionTimeout := 0;
  FClkActions[AIndexToInsertAt].SetVarOptions := ANewAction;

  vstActions.RootNodeCount := Length(FClkActions);
end;


procedure TfrClickerActionsArr.InsertAction(AIndexToInsertAt: Integer; var ANewAction: TClkActionRec);
begin
  InsertActionIntoArr(FClkActions, AIndexToInsertAt, ANewAction);
  vstActions.RootNodeCount := Length(FClkActions);
end;


procedure TfrClickerActionsArr.LoadActionIntoEditorByIndex(AIndex: Integer);
var
  Node: PVirtualNode;
begin
  Node := GetNodeByIndex(AIndex);
  if Node <> nil then
  begin
    vstActions.Selected[Node] := True;
    vstActions.ScrollIntoView(Node, True);
    UpdateControlsFromActionsArr(Node^.Index);
    frClickerActions.UpdatePageControlActionExecutionIcons;
  end;
end;


procedure TfrClickerActionsArr.CopySelectedActionsToClipboard;
var
  AStringList: TStringList;   //much faster than T(Mem)IniFile
  ActionsToCopy: TClkActionsRecArr;
begin
  AStringList := TStringList.Create;
  try
    AStringList.LineBreak := #13#10;
    GetSelectedActions(ActionsToCopy);
    try
      SaveTemplateWithCustomActionsToStringList_V2(AStringList, ActionsToCopy, '', '');
      Clipboard.AsText := AStringList.Text;
    finally
      SetLength(ActionsToCopy, 0);
    end;
  finally
    AStringList.Free;
  end;
end;


procedure TfrClickerActionsArr.PasteActionsFromClipboard(APasteIndex: Integer);
var
  i: Integer;
  AStringList: TStringList;
  Ini: TClkIniReadonlyFile;
  FormatVersion: string;
  ClipboardClkActions: TClkActionsRecArr;
  Node: PVirtualNode;
  DummyNotes, DummyIconPath: string;
begin
  AStringList := TStringList.Create;
  try
    AStringList.LineBreak := #13#10;  //probably this should be set after assigning from Clipboard.AsText
    AStringList.Text := Clipboard.AsText;

    Ini := TClkIniReadonlyFile.Create(AStringList);
    try
      SetLength(ClipboardClkActions, Ini.ReadInteger('Actions', 'Count', 0));
      try
        FormatVersion := Ini.ReadString('Actions', 'Version', '1');

        if FormatVersion = '2' then
        begin
          LoadTemplateToCustomActions_V2(Ini, ClipboardClkActions, DummyNotes, DummyIconPath);

          vstActions.ClearSelection;
          Application.ProcessMessages;

          if APasteIndex > -1 then
          begin
            for i := Length(ClipboardClkActions) - 1 downto 0 do
            begin
              InsertAction(APasteIndex + 1, ClipboardClkActions[i]);

              Node := GetNodeByIndex(APasteIndex + i + 1);
              if Node <> nil then
                vstActions.Selected[Node] := True;
            end;

            vstActions.RootNodeCount := Length(FClkActions);

            if FClkActions[APasteIndex].ActionOptions.Action = acPlugin then
              {$IFnDEF MemPlugins}
                if DoOnFileExists(ResolveTemplatePath(FClkActions[APasteIndex].PluginOptions.FileName)) then
              {$ENDIF}
                SetActionPropertiesFromPlugin(FClkActions[APasteIndex]);
          end
          else
            for i := 0 to Length(ClipboardClkActions) - 1 do
            begin
              SetLength(FClkActions, Length(FClkActions) + 1);
              FClkActions[Length(FClkActions) - 1] := ClipboardClkActions[i];    //to be replaced with a copy function
              vstActions.RootNodeCount := Length(FClkActions);
              vstActions.Selected[vstActions.GetLast] := True;

              if FClkActions[Length(FClkActions) - 1].ActionOptions.Action = acPlugin then
                {$IFnDEF MemPlugins}
                  if DoOnFileExists(ResolveTemplatePath(FClkActions[Length(FClkActions) - 1].PluginOptions.FileName)) then
                {$ENDIF}
                  SetActionPropertiesFromPlugin(FClkActions[Length(FClkActions) - 1]);
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
      CopyActionContent(FClkActions[Node^.Index], AActionsArr[Result]);

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
  vstActions.ClearSelection;
  for i := n - 1 downto 0 do
    RemoveAction(AActionIndexArrToReplace[i]);  //use a for to remove actions, because the vst is reset and refreshed at every call, so a  Node := Node^.PrevSibling will not work

  //insert a call action
  InsertCallTemplateForActionReplacing(AFirstSelectedIndex, ACallTemplateFileName);

  if not frClickerActions.ControlsModified then
  begin
    vstActions.ClearSelection;
    LoadActionIntoEditorByIndex(AFirstSelectedIndex);
    StopGlowingUpdateButton;
  end;

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
    SaveTemplateWithCustomActions_V2(DoOnGetSaveDialogFileName, NewClkActions, 'refactored actions', FTemplateIconPath);
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


function TfrClickerActionsArr.ResolveTemplatePath(APath: string; ACustomSelfTemplateDir: string = ''; ACustomAppDir: string = ''): string;
begin
  Result := StringReplace(APath, '$TemplateDir$', FFullTemplatesDir, [rfReplaceAll]);

  if ACustomSelfTemplateDir = '' then
    Result := StringReplace(Result, '$SelfTemplateDir$', ExtractFileDir(FFileName), [rfReplaceAll])
  else
    Result := StringReplace(Result, '$SelfTemplateDir$', ACustomSelfTemplateDir, [rfReplaceAll]);

  if ACustomAppDir = '' then
    Result := StringReplace(Result, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll])
  else
    Result := StringReplace(Result, '$AppDir$', ACustomAppDir, [rfReplaceAll]);
end;


function TfrClickerActionsArr.EncodeTemplatePath(APath: string): string;
begin
  Result := StringReplace(APath, ExtractFileDir(FFileName), '$SelfTemplateDir$', [rfReplaceAll]);
  Result := StringReplace(Result, FFullTemplatesDir, '$TemplateDir$', [rfReplaceAll]);
  Result := StringReplace(Result, ExtractFileDir(ParamStr(0)), '$AppDir$', [rfReplaceAll])
end;


procedure TfrClickerActionsArr.MenuItem_ReplaceWith_AppDirClick(Sender: TObject);
var
  ResolvedPath: string;
begin
  ResolvedPath := ResolveTemplatePath(FTemplateIconPath);

  FTemplateIconPath := StringReplace(ResolvedPath, ExtractFileDir(ParamStr(0)), '$AppDir$', [rfReplaceAll]);
  SetTemplateIconHint;
  Modified := True;
end;


procedure TfrClickerActionsArr.MenuItem_ReplaceWith_SelfTemplateDirClick(
  Sender: TObject);
var
  ResolvedPath: string;
begin
  ResolvedPath := ResolveTemplatePath(FTemplateIconPath);

  FTemplateIconPath := StringReplace(ResolvedPath, ExtractFileDir(FFileName), '$SelfTemplateDir$', [rfReplaceAll]);
  SetTemplateIconHint;
  Modified := True;
end;


procedure TfrClickerActionsArr.MenuItem_ReplaceWith_TemplateDirClick(
  Sender: TObject);
var
  ResolvedPath: string;
begin
  ResolvedPath := ResolveTemplatePath(FTemplateIconPath);

  FTemplateIconPath := StringReplace(ResolvedPath, FFullTemplatesDir, '$TemplateDir$', [rfReplaceAll]);
  SetTemplateIconHint;
  Modified := True;
end;


procedure TfrClickerActionsArr.MenuItem_ResolvePathToAbsoluteClick(
  Sender: TObject);
begin
  FTemplateIconPath := ResolveTemplatePath(FTemplateIconPath);

  SetTemplateIconHint;
  Modified := True;
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


procedure TfrClickerActionsArr.MenuItem_GenericInsertActionOnFirstSelectedClick(Sender: TObject);
var
  TempMenuItem: TMenuItem;
  NewAction: TClkActionRec;
  Node: PVirtualNode;
  IdxOfSelected: Integer;
  ActionTypeInt: Integer;
begin
  TempMenuItem := Sender as TMenuItem;

  Node := vstActions.GetFirstSelected;
  IdxOfSelected := 0;
  if Node <> nil then
    IdxOfSelected := Node^.Index;

  ActionTypeInt := TempMenuItem.Tag;
  if ActionTypeInt in [0..CInsertActionOffset - 1] then
  begin
    InsertAction(IdxOfSelected, NewAction);
    OverwriteActionAtIndexWithDefault(IdxOfSelected, TClkAction(ActionTypeInt));
    UpdateNodesCheckStateFromActions;

    if not frClickerActions.ControlsModified then
    begin
      vstActions.ClearSelection;
      LoadActionIntoEditorByIndex(IdxOfSelected);
      StopGlowingUpdateButton;
    end;

    Modified := True;
  end;

  if ActionTypeInt in [CInsertActionOffset..CInsertActionOffset shl 1 - 1] then
  begin
    InsertAction(IdxOfSelected + 1, NewAction);
    OverwriteActionAtIndexWithDefault(IdxOfSelected + 1, TClkAction(ActionTypeInt - CInsertActionOffset));
    UpdateNodesCheckStateFromActions;

    if not frClickerActions.ControlsModified then
    begin
      vstActions.ClearSelection;
      LoadActionIntoEditorByIndex(IdxOfSelected + 1);
      StopGlowingUpdateButton;
    end;

    Modified := True;
  end;
end;


procedure TfrClickerActionsArr.MenuItem_GenericOpenCalledTemplateInExperimentTabClick(Sender: TObject);
var
  TempMenuItem: TMenuItem;
  Node: PVirtualNode;
  TemplatePath: string;
begin
  Node := vstActions.GetFirstSelected;
  if (Node = nil) or ((Node <> nil) and (FClkActions[Node^.Index].ActionOptions.Action <> acCallTemplate)) then
  begin
    MessageBox(Handle, 'No CallTemplate action is selected.', PChar(Application.Title), MB_ICONINFORMATION);
    Exit;
  end;

  TemplatePath := FClkActions[Node^.Index].CallTemplateOptions.TemplateFileName;
  TemplatePath := ResolveTemplatePath(TemplatePath); //this uses $SelfTemplateDir$, which is not a standard variable. It will be evaluated to ''.

  if TemplatePath = ExtractFileName(TemplatePath) then //there is no path, only the filename
    TemplatePath := FFullTemplatesDir + PathDelim + TemplatePath;

  TemplatePath := EvaluateReplacements(TemplatePath);

  TempMenuItem := Sender as TMenuItem;
  DoOnOpenCalledTemplateInExperimentTab(TempMenuItem.Tag, TemplatePath);
end;


procedure TfrClickerActionsArr.MenuItem_StopWaitingForFilesAvailabilityClick(
  Sender: TObject);
begin
  DoOnTerminateWaitForMultipleFilesAvailability;
end;


procedure TfrClickerActionsArr.pmVstActionsPopup(Sender: TObject);
type
  TFoundItem = record
    Item: TMenuItem;
    ItemTxt: string;
  end;
var
  i, j: Integer;
  Txt: string;
  FoundItems: array[0..1] of TFoundItem;
  TempMenuItem: TMenuItem;
  k: TClkAction;
  Bmp: TBitmap;
  Node: PVirtualNode;
begin
  FoundItems[0].Item := nil;
  FoundItems[1].Item := nil;
  FoundItems[0].ItemTxt := 'Insert action before first selected';
  FoundItems[1].ItemTxt := 'Insert action after first selected';

  for i := 0 to pmVstActions.Items.Count - 1 do
  begin
    Txt := StringReplace(pmVstActions.{Items.}Items[i].Caption, '&', '', [rfReplaceAll]);

    for j := 0 to Length(FoundItems) - 1 do
      if FoundItems[j].ItemTxt = Txt then
      begin
        FoundItems[j].Item := pmVstActions.Items[i];
        pmVstActions.Items[i].Clear;
      end;
  end;

  for j := 0 to Length(FoundItems) - 1 do
    if FoundItems[j].Item <> nil then //found
      for k := Low(TClkAction) to High(TClkAction) do
      begin
        TempMenuItem := TMenuItem.Create(pmVstActions);
        TempMenuItem.Caption := CClkActionStr[k];
        TempMenuItem.Tag := j * CInsertActionOffset + Integer(k);
        TempMenuItem.OnClick := MenuItem_GenericInsertActionOnFirstSelectedClick;

        Bmp := TBitmap.Create;
        WipeBitmap(Bmp, 16, 16);
        frClickerActions.imglstActions16.Draw(Bmp.Canvas, 0, 0, Integer(k));
        TempMenuItem.Bitmap := Bmp;
        FoundItems[j].Item.Insert(FoundItems[j].Item.Count, TempMenuItem);
      end;

  Node := vstActions.GetFirstSelected;
  MenuItem_OpenCalledTemplateInExperimentTab.Enabled := (Node <> nil) and
                                                        (vstActions.GetNodeLevel(Node) = 0) and
                                                        (FClkActions[Node^.Index].ActionOptions.Action = acCallTemplate);

  if MenuItem_OpenCalledTemplateInExperimentTab.Enabled then
    MenuItem_OpenCalledTemplateInExperimentTab.Bitmap := MenuItem_AddACallTemplateByFile.Bitmap
  else
    MenuItem_OpenCalledTemplateInExperimentTab.Bitmap := nil;

  MenuItem_OpenCalledTemplateInExperimentTab.Clear;
  for j := 0 to 1 do //this should be replaced with an array when experiments are implemented as array
  begin
    TempMenuItem := TMenuItem.Create(pmVstActions);
    TempMenuItem.Caption := 'Experiment ' + IntToStr(j + 1);
    TempMenuItem.Tag := j;
    TempMenuItem.OnClick := MenuItem_GenericOpenCalledTemplateInExperimentTabClick;
    MenuItem_OpenCalledTemplateInExperimentTab.Insert(MenuItem_OpenCalledTemplateInExperimentTab.Count, TempMenuItem);
  end;
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

  pnlVertSplitter.Width := Width - 3;   //these corrections are required, because the width anchors seem to be ignored right after setting top and height
  pnlActionsEditor.Width := Width - 3;
  pnlActions.Width := Width - 3;
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


procedure TfrClickerActionsArr.ExtraLoadRecentFileClick(Sender: TObject); //disk files only
var
  Fnm: string;
begin
  Fnm := (Sender as TMenuItem).Caption;
  Fnm := StringReplace(Fnm, '&', '', [rfReplaceAll]);

  LoadTemplateWithUIUpdate(Fnm, flDisk);

  SaveTemplateIfModified;

  FFileName := Fnm; //update before loading, to allow properly displaying the label
  lblModifiedStatus.Hint := FFileName;
  LoadTemplate(FFileName, flDisk); //load with full path

  frClickerActions.ClearControls;
  StopGlowingUpdateButton;
end;


procedure TfrClickerActionsArr.spdbtnExtraLoadClick(Sender: TObject);
var
  tp: TPoint;
  i: Integer;
  MenuItem: TMenuItem;
  ListOfRecentFiles, ListOfMemFiles: TStringList;
  SelfExePath: string;
begin
  pmExtraLoad.Items[0].Clear;

  ListOfRecentFiles := TStringList.Create;
  try
    ListOfRecentFiles.LineBreak := #13#10;
    DoOnGetListOfRecentFiles(ListOfRecentFiles);
    SelfExePath := ExtractFileDir(ParamStr(0));

    for i := 0 to ListOfRecentFiles.Count - 1 do
    begin
      MenuItem := TMenuItem.Create(Self);
      MenuItem.Caption := StringReplace(ListOfRecentFiles.Strings[i], SelfExePath, '$AppDir$', [rfReplaceAll]);
      MenuItem.OnClick := ExtraLoadRecentFileClick;
      MenuItem.Bitmap := spdbtnPlaySelectedAction.Glyph;
      pmExtraLoad.Items[0].Add(MenuItem);
    end;
  finally
    ListOfRecentFiles.Free;
  end;

  pmExtraLoad.Items[1].Clear;

  ListOfMemFiles := TStringList.Create;
  try
    ListOfMemFiles.LineBreak := #13#10;
    frClickerActions.InMemFS.ListMemFiles(ListOfMemFiles);

    for i := 0 to ListOfMemFiles.Count - 1 do
    begin
      MenuItem := TMenuItem.Create(Self);
      MenuItem.Caption := ListOfMemFiles.Strings[i];
      MenuItem.OnClick := ExtraLoadInMemFileClick;
      MenuItem.Bitmap := spdbtnPlaySelectedAction.Glyph;
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


procedure TfrClickerActionsArr.spdbtnExtraPlayAll1Click(Sender: TObject);
begin
  pmExtraStop.PopUp;
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
    {$IFDEF Windows}
      Windows.SetCursor(Screen.Cursors[crDrag]);
    {$ELSE}
      SetCursor(Screen.Cursors[crDrag]);
    {$ENDIF}
  //
  //GetCursorPos(tp);
  //DeltaX := Left + pnlDrawingBoard.Left + scrboxScreen.Left + 8;
  //DeltaY := Top + scrboxScreen.Top - 8 + Height - ClientHeight;
  //
end;


procedure TfrClickerActionsArr.SetActionToDefault(var AAction: TClkActionRec; ANewActionType: TClkAction);
begin
  AAction.ActionOptions.Action := ANewActionType;
  AAction.ActionOptions.ActionCondition := '';
  AAction.ActionOptions.ActionEnabled := True;
  AAction.ActionOptions.ActionName := '"' + CClkActionStr[AAction.ActionOptions.Action] + '"';
  AAction.ActionOptions.ActionTimeout := 0;
  AAction.ActionOptions.ExecutionIndex := '';
  AAction.ActionStatus := asNotStarted;
  AAction.ActionSkipped := False;
  AAction.ActionBreakPoint.Condition := '';
  AAction.ActionBreakPoint.Enabled := False;
  AAction.ActionBreakPoint.Exists := False;
  AAction.ActionDebuggingStatus := adsNone;

  if AAction.ActionOptions.Action = acFindSubControl then
    AAction.ActionOptions.ActionTimeout := 1000;

  if AAction.ActionOptions.Action = acFindControl then
    AAction.ActionOptions.ActionTimeout := 3000;

  GetDefaultPropertyValues_Click(AAction.ClickOptions);
  GetDefaultPropertyValues_ExecApp(AAction.ExecAppOptions);
  GetDefaultPropertyValues_FindControl(AAction.FindControlOptions);
  GetDefaultPropertyValues_FindSubControl(AAction.FindSubControlOptions);
  GetDefaultPropertyValues_SetControlText(AAction.SetTextOptions);
  GetDefaultPropertyValues_CallTemplate(AAction.CallTemplateOptions);
  GetDefaultPropertyValues_Sleep(AAction.SleepOptions);
  GetDefaultPropertyValues_SetVar(AAction.SetVarOptions);
  GetDefaultPropertyValues_WindowOperations(AAction.WindowOperationsOptions);
  GetDefaultPropertyValues_LoadSetVarFromFile(AAction.LoadSetVarFromFileOptions);
  GetDefaultPropertyValues_SaveSetVarToFile(AAction.SaveSetVarToFileOptions);
  GetDefaultPropertyValues_Plugin(AAction.PluginOptions);
  GetDefaultPropertyValues_EditTemplate(AAction.EditTemplateOptions);
end;


procedure TfrClickerActionsArr.OverwriteActionAtIndexWithDefault(AIndex: Integer; ANewActionType: TClkAction);
begin
  SetActionToDefault(FClkActions[AIndex], ANewActionType);
end;


procedure TfrClickerActionsArr.FPaletteVsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
  tp: TPoint;
  Comp: TCompRec;
  n: Integer;
begin
  if FPalette.vstActionsPalette.Tag <> 1 then
    Exit;

  FPalette.vstActionsPalette.Tag := 0;

  {$IFDEF Windows}
    Windows.SetCursor(Screen.Cursors[crDefault]);
  {$ELSE}
    SetCursor(Screen.Cursors[crDefault]);
  {$ENDIF}

  Node := FPalette.vstActionsPalette.GetFirstSelected;
  if Node = nil then
    Exit;

  GetCursorPos(tp);
  Comp := GetWindowClassRec(tp);

  if Comp.Handle = vstActions.Handle then
  begin
    n := Length(FClkActions);
    SetLength(FClkActions, n + 1);

    OverwriteActionAtIndexWithDefault(n, TClkAction(Node^.Index));

    //UpdateActionsArrFromControls(n); ///////////////////////////////// ToDo  replace this call with some default values     //was used for initializing FindControl's "Match" properties
    //OverwriteActionAtIndexWithDefault(n, TClkAction(Node^.Index));

    if (TClkAction(Node^.Index) = acEditTemplate) and (frClickerActions.EditingAction <> nil) then
      frClickerActions.SerializeEditTemplateEditingAction;

    frClickerActions.CurrentlyEditingActionType := TClkAction(Node^.Index);

    vstActions.RootNodeCount := Length(FClkActions);
    UpdateNodesCheckStateFromActions;

    if not frClickerActions.ControlsModified then
    begin
      vstActions.ClearSelection;
      LoadActionIntoEditorByIndex(n);  //calls UpdateControlsFromActionsArr(Node^.Index);  which copies from FClkActions[ActionIndex] to frClickerActions.EditingAction^
      StopGlowingUpdateButton;
    end;

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
      ActionNames.LineBreak := #13#10;
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
    CopyActionContent(FClkActions[i - 1], FClkActions[i]); //FClkActions[i] := FClkActions[i - 1];

  UpdateActionsArrFromControls(Node^.Index);
  FClkActions[Node^.Index].ActionStatus := asNotStarted;
  FClkActions[Node^.Index].ActionOptions.ActionEnabled := True;

  //if (FClkActions[Node^.Index].ActionOptions.Action = acEditTemplate) and (frClickerActions.EditingAction <> nil) then
  //begin
  //  frClickerActions.EditingAction^.EditTemplateOptions.EditingAction := @FClkEditedActionByEditTemplate;
  //  frClickerActions.EditingAction^.EditTemplateOptions.ListOfEditedProperties := StringReplace(GetActionPropertiesByType(FClkEditedActionByEditTemplate), CPropSeparatorSer, CPropSeparatorInt, [rfReplaceAll]);
  //
  //  FClkActions[Node^.Index].EditTemplateOptions.EditingAction := frClickerActions.EditingAction^.EditTemplateOptions.EditingAction;
  //  FClkActions[Node^.Index].EditTemplateOptions.ListOfEditedProperties := frClickerActions.EditingAction^.EditTemplateOptions.ListOfEditedProperties;
  //end;

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
    CopyActionContent(FClkActions[i - 1], FClkActions[i]); //FClkActions[i] := FClkActions[i - 1];

  NewIndex := Node^.Index + 1;
  UpdateActionsArrFromControls(NewIndex);
  FClkActions[NewIndex].ActionStatus := asNotStarted;
  FClkActions[NewIndex].ActionOptions.ActionEnabled := True;

  //if (FClkActions[Node^.Index].ActionOptions.Action = acEditTemplate) and (frClickerActions.EditingAction <> nil) then
  //begin
  //  frClickerActions.EditingAction^.EditTemplateOptions.EditingAction := @FClkEditedActionByEditTemplate;
  //  frClickerActions.EditingAction^.EditTemplateOptions.ListOfEditedProperties := StringReplace(GetActionPropertiesByType(FClkEditedActionByEditTemplate), CPropSeparatorSer, CPropSeparatorInt, [rfReplaceAll]);
  //
  //  FClkActions[Node^.Index].EditTemplateOptions.EditingAction := frClickerActions.EditingAction^.EditTemplateOptions.EditingAction;
  //  FClkActions[Node^.Index].EditTemplateOptions.ListOfEditedProperties := frClickerActions.EditingAction^.EditTemplateOptions.ListOfEditedProperties;
  //end;

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
              frClickerActions.EditingAction^.FindControlOptions.MatchCriteria.WillMatchClassName) then
      begin
        MessageBox(Handle, 'To find a control, at least one match criterion has to be checked.', PChar(Application.Title), MB_ICONINFORMATION);
        Exit;
      end;

    acFindSubControl:
      if not (frClickerActions.EditingAction^.FindSubControlOptions.MatchCriteria.WillMatchBitmapText or
              frClickerActions.EditingAction^.FindSubControlOptions.MatchCriteria.WillMatchBitmapFiles or
              frClickerActions.EditingAction^.FindSubControlOptions.MatchCriteria.WillMatchPrimitiveFiles) then
      begin
        MessageBox(Handle, 'To find a subcontrol, at least one match criterion has to be checked.', PChar(Application.Title), MB_ICONINFORMATION);
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
  spdbtnStopPlaying.Enabled := True;  //It's ugly that the button has to be enabled here, but it is required to stay "on" for a little longer, to be captured by the test driver.
  spdbtnStopPlaying.Repaint;
  try
    PrepareFilesInServer;
  finally
    PlayAllActionsFromButton(True);
  end;
end;


procedure TfrClickerActionsArr.PlayAllInDebuggingModeStartingAtSelected1Click(
  Sender: TObject);
begin
  spdbtnStopPlaying.Enabled := True;  //It's ugly that the button has to be enabled here, but it is required to stay "on" for a little longer, to be captured by the test driver.
  spdbtnStopPlaying.Repaint;
  try
    PrepareFilesInServer;
  finally
    PlayAllActionsFromButton(True, True);
  end;
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
  CurrentAction: PClkActionRec;
  NodeData: PActionNodeRec;
begin
  Node := vstActions.GetFirst;
  if Node = nil then
    Exit;

  repeat
    if Node^.Parent = vstActions.RootNode then
      CurrentAction := @FClkActions[Node^.Index]
    else
    begin
      NodeData := vstActions.GetNodeData(Node);
      if not Assigned(NodeData) then
      begin
        Node.CheckState := csMixedPressed;
        Exit;
      end;

      CurrentAction := @NodeData^.Action;
    end;

    Node^.CheckState := CNodeStates[CurrentAction^.ActionOptions.ActionEnabled];
    Node := Node^.NextSibling;
  until Node = nil;
end;


procedure TfrClickerActionsArr.RemoveAction(ActionIndex: Integer; AClearSelectionAfterRemoving: Boolean = True; AUpdateRootNodeCount: Boolean = True);
begin
  if Length(FClkActions) = 0 then
    Exit;

  if AClearSelectionAfterRemoving then
    vstActions.Clear; // vstActions.RootNodeCount := 0; //to reinit nodes

  if AUpdateRootNodeCount then
    vstActions.RootNodeCount := Length(FClkActions) - 1;

  RemoveActionFromArr(FClkActions, ActionIndex); //deleting the action after setting VST node count, to avoid AVs

  UpdateNodesCheckStateFromActions;
end;


procedure TfrClickerActionsArr.RemoveSelectedActions;
var
  Node: PVirtualNode;
  IdxArr: TIntArr;
  i: Integer;
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
      SetLength(IdxArr, 0);
      repeat
        if vstActions.Selected[Node] then
        begin
          SetLength(IdxArr, Length(IdxArr) + 1);
          IdxArr[Length(IdxArr) - 1] := Node^.Index;
        end;

        Node := Node^.PrevSibling;
      until Node = nil;
    finally
      vstActions.EndUpdate;
    end;

    if Length(IdxArr) > 0 then
    begin
      vstActions.ClearSelection;  //the selected actions will be deleted, so make sure they are not selected anymore

      vstActions.BeginUpdate;
      try
        for i := 0 to Length(IdxArr) - 1 do
          RemoveAction(IdxArr[i], False, True);

        if Length(FClkActions) > 0 then
          vstActions.RootNodeCount := Length(FClkActions);  //this should not be required if RemoveAction updates RootNodeCount
      finally
        SetLength(IdxArr, 0);
        vstActions.EndUpdate;
      end;

      vstActions.Repaint;
      Modified := True;
      StopGlowingUpdateButton;
      vstActions.ClearSelection;
    end;
  end; //confirmation
end;


procedure TfrClickerActionsArr.btnRemoveActionClick(Sender: TObject);
begin
  RemoveSelectedActions;
end;


procedure TfrClickerActionsArr.Removeallactions1Click(Sender: TObject);
begin
  if MessageBox(Handle, 'Are you sure you want to remove all actions from list?', PChar(Caption), MB_ICONQUESTION + MB_YESNO) = ID_YES then
  begin
    vstActions.Clear; // vstActions.RootNodeCount := 0;
    SetLength(FClkActions, 0);

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
  vstActions.ClearSelection;       //ClearSelection and ..
  vstActions.Clear; // vstActions.RootNodeCount := 0;   //set RootNodeCount before clearing the array, otherwise an AV may happen in VST
  Application.ProcessMessages;
  SetLength(FClkActions, 0);

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

  {$IFDEF Windows}
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
  {$ELSE}
    if GetKeyState(VK_F2) < 0 then
      AState[VK_F2] := $80
    else
      AState[VK_F2] := 0;

    if GetKeyState(VK_F6) < 0 then
      AState[VK_F6] := $80
    else
      AState[VK_F6] := 0;

    if GetKeyState(VK_F7) < 0 then
      AState[VK_F7] := $80
    else
      AState[VK_F7] := 0;

    if GetKeyState(VK_F8) < 0 then
      AState[VK_F8] := $80
    else
      AState[VK_F8] := 0;

    if GetKeyState(VK_F9) < 0 then
      AState[VK_F9] := $80
    else
      AState[VK_F9] := 0;
  {$ENDIF}

  if FDebugging then
  begin
    if (F2_State and $80 = 0) and (AState[VK_F2] and $80 = $80) and
      {$IFDEF Windows}
        (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) then
      {$ELSE}
        (GetKeyState(VK_CONTROL) < 0) and (GetKeyState(VK_SHIFT) < 0) then
      {$ENDIF}  //detected Ctrl-Shift-F2
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
    if FClkActions[i].ActionOptions.Action in [acFindSubControl] then
    begin
      TempStringList := TStringList.Create;
      try
        TempStringList.LineBreak := #13#10;
        TempStringList.Text := FClkActions[i].FindSubControlOptions.MatchBitmapFiles;

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


function TfrClickerActionsArr.GetExtRenderingInMemFS: TInMemFileSystem;
begin
  Result := frClickerActions.ExtRenderingInMemFS;
end;


procedure TfrClickerActionsArr.SetExtRenderingInMemFS(Value: TInMemFileSystem);
begin
  frClickerActions.ExtRenderingInMemFS := Value;
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
        SetActionVarValue('$DbgCurrentAction$', '');
        SetActionVarValue('$ExecAction_Err$', '');

        try
          HighlightCurrentlyExecutedAction(CurrentNode);
        except
          on E: Exception do
            raise Exception.Create(E.Message + '  in HighlightCurrentlyExecutedAction(' + IntToStr(FRemoteExActionIndex) +')');
        end;                                //good to have to blanks here ('  '), to easily find the message
      end
      else
      begin
        Err := CErr_CannotSelectActionToLoadValuesAtIndex + ' ' + IntToStr(FRemoteExActionIndex) + '.';
        if GetNodeByIndex(0) = nil then
        begin
          Err := Err + '  It seems that no template is loaded.';
          SetActionVarValue('$ExecAction_Err$', CErr_EmptyTemplate);
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
         not FRemoteExCmdResult and (
             (FClkActions[FRemoteExActionIndex].ActionOptions.Action = acFindControl) and (FClkActions[FRemoteExActionIndex].FindControlOptions.AllowToFail) or
             (FClkActions[FRemoteExActionIndex].ActionOptions.Action = acFindSubControl) and (FClkActions[FRemoteExActionIndex].FindSubControlOptions.AllowToFail)
                                    ) then
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
      TempStrings.LineBreak := #13#10;
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


procedure TfrClickerActionsArr.tmrWaitingForFilesAvailabilityTimer(
  Sender: TObject);
begin
  tmrWaitingForFilesAvailability.Tag := tmrWaitingForFilesAvailability.Tag + 1;
  if tmrWaitingForFilesAvailability.Tag >= 6 then
    tmrWaitingForFilesAvailability.Tag := 0;

  imglstWaitingForFilesAvailability.Draw(imgWaitingForFilesAvailability.Canvas, 0, 0, tmrWaitingForFilesAvailability.Tag, dsNormal, itImage);
end;


end.
