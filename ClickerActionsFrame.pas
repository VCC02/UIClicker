{
    Copyright (C) 2026 VCC
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


unit ClickerActionsFrame;

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
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  VirtualTrees, ExtCtrls, StdCtrls, ComCtrls, ImgList, Buttons,
  Menus, ClickerUtils, ClickerConditionEditorFrame,
  ClickerFindControlFrame, ClickerExecAppFrame, ClickerSetVarFrame,
  ClickerCallTemplateFrame, ClickerSleepFrame, ClickerPluginFrame,
  Types, InMemFileSystem, ObjectInspectorFrame,
  ClickerPrimitiveUtils, ClickerIniFiles, CLHeaders;

{$IFnDEF Windows}
  {$UNDEF MemPlugins}
{$ENDIF}

type
  TVarNodeRec = record
    VarName, VarValue: string;
  end;

  PVarNodeRec = ^TVarNodeRec;

  TOnGetLoadedTemplateFileName = function: string of object;
  TOnChangeEditTemplateEditingActionType = procedure of object;

  TOnGetPluginInMemFS = function: TInMemFileSystem of object;

  { TfrClickerActions }

  TfrClickerActions = class(TFrame)
    chkDecodeVariables: TCheckBox;
    chkShowDebugGrid: TCheckBox;
    imgPluginFileName: TImage;
    imgDebugBmp: TImage;
    imgDebugGrid: TImage;
    imgFontColorBuffer: TImage;
    imgPlugin: TImage;
    lblBitmaps: TLabel;
    lblDebugBitmapXMouseOffset: TLabel;
    lblDebugBitmapYMouseOffset: TLabel;
    lblMouseOnExecDbgImgBB: TLabel;
    lblMouseOnExecDbgImgGG: TLabel;
    lblMouseOnExecDbgImgRR: TLabel;
    lblMouseOnExecDbgImg: TLabel;
    lblVarReplacements: TLabel;
    MenuItem_ReplaceWithSelfTemplateDir: TMenuItem;
    MenuItem_ReplaceWithTemplateDir: TMenuItem;
    MenuItem_ReplaceWithAppDir: TMenuItem;
    MenuItem_SetFromControlWidthAndHeight: TMenuItem;
    MenuItem_SetFromControlLeftAndTop: TMenuItem;
    MenuItem_AddFilesToPropertyList: TMenuItem;
    MenuItem_AddLastActionStatusEqualsSuccessful: TMenuItem;
    MenuItem_AddLastActionStatusEqualsAllowedFailed: TMenuItem;
    N6: TMenuItem;
    MenuItemColor_WindowFrame: TMenuItem;
    MenuItemColor_3DLight: TMenuItem;
    MenuItemColor_3DDkShadow: TMenuItem;
    MenuItemColor_ScrollBar: TMenuItem;
    MenuItemColor_GradientInactiveCaption: TMenuItem;
    MenuItemColor_GradientActiveCaption: TMenuItem;
    MenuItemColor_GrayText: TMenuItem;
    N5: TMenuItem;
    N4: TMenuItem;
    MenuItem_RemoveSetVar: TMenuItem;
    MenuItem_AddSetVar: TMenuItem;
    MenuItemControl_Bottom: TMenuItem;
    MenuItemControl_Right: TMenuItem;
    MenuItemControl_Top: TMenuItem;
    MenuItemControl_Left: TMenuItem;
    N3: TMenuItem;
    MenuItemPasteRefFromClipboard: TMenuItem;
    MenuItemCopyRefToClipboard: TMenuItem;
    N2: TMenuItem;
    MenuItemPasteColorFromClipboard: TMenuItem;
    MenuItemCopyColorToClipboard: TMenuItem;
    MenuItemColor_WindowText: TMenuItem;
    MenuItemColor_Window: TMenuItem;
    MenuItemColor_InactiveCaption: TMenuItem;
    MenuItemColor_ActiveCaption: TMenuItem;
    MenuItemColor_BtnFace: TMenuItem;
    MenuItemColor_Highlight: TMenuItem;
    MenuItemGreaterThanOrEqual: TMenuItem;
    MenuItemLessThanOrEqual: TMenuItem;
    MenuItemGreaterThan: TMenuItem;
    MenuItemLessThan: TMenuItem;
    MenuItemEqual: TMenuItem;
    MenuItemNotEqual: TMenuItem;
    MenuItemCopySearchAreaAllToClipboard: TMenuItem;
    MenuItemCopySearchAreaSearchBmpImgToClipboard: TMenuItem;
    MenuItemCopySearchAreaBkImgToClipboard: TMenuItem;
    MenuItemGenericLoadBmpToSearchedArea: TMenuItem;
    pnlHorizSplitterResults: TPanel;
    pnlResults: TPanel;
    pmStandardControlRefVars: TPopupMenu;
    pmStandardColorVariables: TPopupMenu;
    pnlActionConditions: TPanel;
    pmWindowOperationsEditors: TPopupMenu;
    pnlCover: TPanel;
    pnlExtra: TPanel;
    pnlHorizSplitter: TPanel;
    pnlVars: TPanel;
    pnlvstOI: TPanel;
    N10001: TMenuItem;
    N100001: TMenuItem;
    N01: TMenuItem;
    N300001: TMenuItem;
    pmPathReplacements: TPopupMenu;
    scrboxDebugBmp: TScrollBox;
    tmrOnChangeEditTemplateEditingActionType: TTimer;
    tmrEditClkVariables: TTimer;
    tmrClkVariables: TTimer;
    tmrDrawZoom: TTimer;
    tmrReloadOIContent: TTimer;
    AddCustomVarRow1: TMenuItem;
    RemoveCustomVarRow1: TMenuItem;
    MenuItemSavePreviewImage: TMenuItem;
    MenuItemCopyPreviewImage: TMenuItem;
    pmDebugVars: TPopupMenu;
    CopyDebugValuesListToClipboard1: TMenuItem;
    PasteDebugValuesListFromClipboard1: TMenuItem;
    PasteDebugValuesListFromMainExecutionList1: TMenuItem;
    MenuItemErasePreviewImage: TMenuItem;
    PageControlActionExecution: TPageControl;
    TabSheetAction: TTabSheet;
    TabSheetCondition: TTabSheet;
    TabSheetDebugging: TTabSheet;
    spdbtnCommonTimeouts: TSpeedButton;
    prbTimeout: TProgressBar;
    pmDebugImage: TPopupMenu;
    MenuItemSaveDebugImage: TMenuItem;
    MenuItemCopyDebugImage: TMenuItem;
    MenuItemEraseDebugImage: TMenuItem;
    MenuItemRemoveExpressionPart: TMenuItem;
    MenuItemRemoveTerm: TMenuItem;
    N1: TMenuItem;
    AddVariable1: TMenuItem;
    RemoveVariable1: TMenuItem;
    vstVariables: TVirtualStringTree;
    procedure chkDecodeVariablesChange(Sender: TObject);
    procedure chkWaitForControlToGoAwayChange(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure lbeFindCachedControlLeftChange(Sender: TObject);
    procedure lbeFindCachedControlTopChange(Sender: TObject);
    procedure MenuItem_ReplaceWithAppDirClick(Sender: TObject);
    procedure MenuItem_ReplaceWithSelfTemplateDirClick(Sender: TObject);
    procedure MenuItem_ReplaceWithTemplateDirClick(Sender: TObject);
    procedure MenuItem_SetFromControlLeftAndTopClick(Sender: TObject);
    procedure MenuItem_SetFromControlWidthAndHeightClick(Sender: TObject);
    procedure pmStandardColorVariablesPopup(Sender: TObject);
    procedure pnlHorizSplitterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlHorizSplitterMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlHorizSplitterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlHorizSplitterResultsMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pnlHorizSplitterResultsMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlHorizSplitterResultsMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure scrboxDebugBmpMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure spdbtnDisplaySearchAreaDbgImgMenuClick(Sender: TObject);
    procedure tmrClkVariablesTimer(Sender: TObject);
    procedure tmrDrawZoomTimer(Sender: TObject);
    procedure tmrEditClkVariablesTimer(Sender: TObject);
    procedure tmrOnChangeEditTemplateEditingActionTypeTimer(Sender: TObject);
    procedure tmrReloadOIContentTimer(Sender: TObject);

    procedure CopyDebugValuesListToClipboard1Click(Sender: TObject);
    procedure PasteDebugValuesListFromClipboard1Click(Sender: TObject);
    procedure PasteDebugValuesListFromMainExecutionList1Click(Sender: TObject);

    procedure imgDebugBmpMouseEnter(Sender: TObject);
    procedure imgDebugBmpMouseLeave(Sender: TObject);
    procedure imgDebugBmpMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure FlblResultSelVertMouseEnter(Sender: TObject);
    procedure FlblResultSelVertMouseLeave(Sender: TObject);
    procedure FlblResultSelVertMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure FlblResultSelHorizMouseEnter(Sender: TObject);
    procedure FlblResultSelHorizMouseLeave(Sender: TObject);
    procedure FlblResultSelHorizMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure chkShowDebugGridClick(Sender: TObject);
    procedure MenuItemSaveDebugImageClick(Sender: TObject);
    procedure MenuItemCopyDebugImageClick(Sender: TObject);
    procedure MenuItemEraseDebugImageClick(Sender: TObject);

    procedure lbeSearchRectLeftChange(Sender: TObject);
    procedure lbeSearchRectTopChange(Sender: TObject);
    procedure lbeSearchRectRightChange(Sender: TObject);
    procedure lbeSearchRectBottomChange(Sender: TObject);
    procedure lbeSearchRectLeftOffsetChange(Sender: TObject);
    procedure lbeSearchRectTopOffsetChange(Sender: TObject);
    procedure lbeSearchRectRightOffsetChange(Sender: TObject);
    procedure lbeSearchRectBottomOffsetChange(Sender: TObject);
    procedure rdgrpSearchForControlModeClick(Sender: TObject);
    procedure vallstVariablesValidate(Sender: TObject; ACol, ARow: Integer;
      const KeyName, KeyValue: string);
    procedure AddVariable1Click(Sender: TObject);
    procedure RemoveVariable1Click(Sender: TObject);
    procedure lbeMatchClassNameChange(Sender: TObject);
    procedure lbeMatchTextSeparatorChange(Sender: TObject);
    procedure lbeMatchClassNameSeparatorChange(Sender: TObject);

    ///////////////////////////// OI
    procedure MenuItem_SetActionTimeoutFromOI(Sender: TObject);
    procedure MenuItem_SetEditedActionTimeoutFromOI(Sender: TObject);

    procedure MenuItem_CopyTextAndClassFromPreviewWindowClick(Sender: TObject);
    procedure MenuItem_CopyTextAndClassFromWinInterpWindowClick(Sender: TObject);
    procedure MenuItem_SetTextAndClassAsSystemMenuClick(Sender: TObject);

    procedure MenuItem_AddBMPFilesToPropertyListClick(Sender: TObject);
    procedure MenuItem_AddExtBMPFilesToPropertyListClick(Sender: TObject);
    procedure MenuItem_RemoveAllBMPFilesFromPropertyListClick(Sender: TObject);
    procedure MenuItem_BrowseBMPFileFromPropertyListClick(Sender: TObject);
    procedure MenuItem_RemoveBMPFileFromPropertyListClick(Sender: TObject);
    procedure MenuItem_MoveBMPFileUpInPropertyListClick(Sender: TObject);
    procedure MenuItem_MoveBMPFileDownInPropertyListClick(Sender: TObject);

    procedure MenuItem_AddExistingPrimitiveFilesToPropertyListClick(Sender: TObject);
    procedure MenuItem_AddNewPrimitiveFilesToPropertyListClick(Sender: TObject);
    procedure MenuItem_RemoveAllPrimitiveFilesFromPropertyListClick(Sender: TObject);
    procedure MenuItem_BrowsePrimitiveFileFromPropertyListClick(Sender: TObject);
    procedure MenuItem_RemovePrimitiveFileFromPropertyListClick(Sender: TObject);
    procedure MenuItem_MovePrimitiveFileUpInPropertyListClick(Sender: TObject);
    procedure MenuItem_MovePrimitiveFileDownInPropertyListClick(Sender: TObject);
    procedure MenuItem_SavePrimitiveFileInPropertyListClick(Sender: TObject);
    procedure MenuItem_SavePrimitiveFileAsInPropertyListClick(Sender: TObject);
    procedure MenuItem_DiscardChangesAndReloadPrimitiveFileInPropertyListClick(Sender: TObject);
    procedure SavePrimitivesFileFromMenu(AFileIndex: Integer; AEditingAction: PClkActionRec);

    procedure MenuItem_BrowseImageSourceFromPropertyListClick(Sender: TObject);
    procedure MenuItem_NoImageSourceInInMemPropertyListClick(Sender: TObject);
    procedure MenuItem_NoPluginInInMemPropertyListClick(Sender: TObject);
    procedure MenuItem_SetFileNameFromInMemPropertyListClick(Sender: TObject);
    procedure MenuItem_SetFileNameFromInMemPropertyListAddToBmpFilesClick(Sender: TObject);
    procedure MenuItem_SetFileNameFromInMemPropertyListUpdateBmpFilesClick(Sender: TObject);
    procedure MenuItem_BrowseFileNameFromInMemPropertyListClick(Sender: TObject);
    procedure MenuItem_BrowseFileNameFromInMemPropertyListAddToBmpFilesClick(Sender: TObject);
    procedure MenuItem_BrowseFileNameFromInMemPropertyListUpdateBmpFilesClick(Sender: TObject);
    procedure MenuItem_SetPluginFileNameFromInMemPropertyListClick(Sender: TObject);
    procedure MenuItem_SetGPUPlatformFromInMemPropertyListClick(Sender: TObject);
    procedure MenuItem_SetGPUDeviceFromInMemPropertyListClick(Sender: TObject);
    procedure MenuItem_BrowseOpenCLFileNameForPropertyListClick(Sender: TObject);

    procedure MenuItem_AddFontProfileToPropertyListClick(Sender: TObject);
    procedure MenuItem_AddFontProfileWithAntialiasedAndClearTypeToPropertyListClick(Sender: TObject);
    procedure MenuItem_AddFontProfileWithNonAntialiasedAndAntialiasedAndClearTypeToPropertyListClick(Sender: TObject);
    procedure MenuItem_RemoveFontProfileFromPropertyListClick(Sender: TObject);
    procedure MenuItem_DuplicateFontProfileClick(Sender: TObject);
    procedure MenuItem_MoveFontProfileUpInPropertyListClick(Sender: TObject);
    procedure MenuItem_MoveFontProfileDownInPropertyListClick(Sender: TObject);
    procedure MenuItem_BrowseSystemFontsClick(Sender: TObject);
    procedure MenuItem_SelectFontFromVarClick(Sender: TObject);

    procedure MenuItem_BrowseSetVarFileInPropertyListClick(Sender: TObject);
    procedure MenuItem_BrowsePluginFileInPropertyListClick(Sender: TObject);
    procedure MenuItem_LoadPluginFromDiskToPluginInMemFSInPropertyListClick(Sender: TObject);
    procedure MenuItem_BrowseEditTemplateFileInPropertyListClick(Sender: TObject);

    procedure MenuItemControl_EdgeRefGenericClick(Sender: TObject);
    procedure MenuItemCopyRefToClipboardClick(Sender: TObject);
    procedure MenuItemPasteRefFromClipboardClick(Sender: TObject);
    procedure vstVariablesCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure vstVariablesEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure vstVariablesEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: boolean);
    procedure vstVariablesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstVariablesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vstVariablesNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: string);
  private
    { Private declarations }
    FBMPsDir: string;
    FEditingText: string;
    FTextEditorEditBox: TEdit;
    FHold: Boolean;
    FHitInfo: THitInfo;
    FHoldResults: Boolean;
    FSplitterMouseDownGlobalPos: TPoint;
    FSplitterMouseDownImagePos: TPoint;

    FOIEditorMenuClosed: Boolean;
    FTempNewItems: string;

    FEditingActionRec: TClkActionRec;
    FEditingAction: PClkActionRec;
    FEditTemplateOptions_EditingAction: PClkActionRec;
    FClkEditedActionByEditTemplate: TClkActionRec; //Used as a working action when setting properties from OI on an EditTemplate action. Otherwise, it just takes memory. It is easier to have this allocated here, because it is deallocated automatically when destroying the whole object.
    FPrevSelectedPrimitiveNode: Integer;

    FOnControlsModified: TNotifyEvent;
    FControlsModified: Boolean;
    FPredefinedVarCount: Integer; //number of predefined variables
    FDebuggingInfoAvailable: Boolean;
    FFullTemplatesDir: string;
    FSetVarContent_Vars: TStringList;
    FSetVarContent_Values: TStringList;
    FSetVarContent_EvalBefore: TStringList;
    FShowDeprecatedControls: Boolean;
    FCurrentMousePosOnPreviewImg: TPoint;

    FSearchAreaScrBox: TScrollBox;
    FSearchAreaSearchedBmpDbgImg: TImage;
    FSearchAreaSearchedTextDbgImg: TImage;
    FSearchAreaDbgImgSearchedBmpMenu: TPopupMenu;

    FCurrentlyEditingActionType: Integer;  //yes integer
    FCurrentlyEditingPrimitiveFileName: string;   //this is updated by OnLoad and OnSave handlers, which have the resolved file name
    FLastClickedTVTEdit: TVTEdit;
    FLastClickedEdit: TEdit;

    FClkVariables: TStringList;

    FlblResultSelLeft: TLabel;
    FlblResultSelTop: TLabel;
    FlblResultSelRight: TLabel;
    FlblResultSelBottom: TLabel;

    FPmLocalTemplates: TPopupMenu;
    FOIFrame: TfrObjectInspector;
    FOIEditorMenu: TPopupMenu;

    FOnCopyControlTextAndClassFromMainWindow: TOnCopyControlTextAndClassFromMainWindow;
    FOnGetExtraSearchAreaDebuggingImage: TOnGetExtraSearchAreaDebuggingImage;
    FOnEditCallTemplateBreakCondition: TOnEditActionCondition;

    FOnLoadBitmap: TOnLoadBitmap;
    FOnLoadRenderedBitmap: TOnLoadRenderedBitmap;
    FOnGetListOfExternallyRenderedImages: TOnGetListOfExternallyRenderedImages;

    {$IFDEF MemPlugins}
      FOnGetListOfInMemPlugins: TOnGetListOfInMemPlugins;
      FOnLoadPluginFromDiskToPluginInMemFileSystem: TOnLoadPluginFromDiskToPluginInMemFileSystem;
      FOnLoadPluginFromInMemFS: TOnLoadPluginFromInMemFS;
    {$ENDIF}
    FOnGetPluginInMemFS: TOnGetPluginInMemFS;

    FOnLoadPrimitivesFile: TOnLoadPrimitivesFile;
    FOnSavePrimitivesFile: TOnSavePrimitivesFile;
    FOnFileExists: TOnFileExists;

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

    FOnExecuteFindSubControlAction: TOnExecuteFindSubControlAction;
    FOnAddToLog: TOnAddToLog;
    FOnGetFontFinderSettings: TOnRWFontFinderSettings;
    FOnSetFontFinderSettings: TOnRWFontFinderSettings;

    FOnGetListOfAvailableSetVarActions: TOnGetListOfAvailableSetVarActions;
    FOnGetListOfAvailableActions: TOnGetListOfAvailableActions;
    FOnTClkIniReadonlyFileCreate: TOnTClkIniReadonlyFileCreate;
    FOnModifyPluginProperty: TOnModifyPluginProperty;

    FOnPluginDbgStop: TOnPluginDbgStop;
    FOnPluginDbgContinueAll: TOnPluginDbgContinueAll;
    FOnPluginDbgStepOver: TOnPluginDbgStepOver;
    FOnPluginDbgRequestLineNumber: TOnPluginDbgRequestLineNumber;
    FOnPluginDbgSetBreakpoint: TOnPluginDbgSetBreakpoint;
    FOnTClkIniFileCreate: TOnTClkIniFileCreate;

    FOnGetSelfTemplatesDir: TOnGetFullTemplatesDir;
    FOnShowAutoComplete: TOnShowAutoComplete;
    FOnUpdateActionScrollIndex: TOnUpdateActionScrollIndex;
    FOnGetLoadedTemplateFileName: TOnGetLoadedTemplateFileName;
    FOnChangeEditTemplateEditingActionType: TOnChangeEditTemplateEditingActionType;

    //function GetListOfSetVarEntries: string;
    //procedure SetListOfSetVarEntries(Value: string);

    function GetListOfCustomVariables: string;
    procedure SetListOfCustomVariables(Value: string);

    procedure CreateRemainingUIComponents;
    procedure SetDebuggingInfoAvailable(Value: Boolean);
    procedure TriggerOnControlsModified(AExtraCondition: Boolean = True);

    function DoOnEditCallTemplateBreakCondition(var AActionCondition: string): Boolean;
    function DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function DoOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    procedure DoOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages: TStringList);

    {$IFDEF MemPlugins}
      procedure DoOnGetListOfInMemPlugins(AListOfInMemPlugins: TStringList);
      procedure DoOnLoadPluginFromDiskToPluginInMemFileSystem(APluginPath: string);
      function DoOnLoadPluginFromInMemFS(APlugin: TMemoryStream; AFileName: string): Boolean;
    {$ENDIF}
    function DoOnGetPluginInMemFS: TInMemFileSystem;

    procedure DoOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    procedure DoOnSavePrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    function DoOnFileExists(const AFileName: string): Boolean;

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

    function DoOnExecuteFindSubControlAction(AErrorLevel, AErrorCount, AFastSearchErrorCount: Integer; AFontName: string; AFontSize: Integer; out AFoundArea: TRect): Boolean;
    procedure DoOnAddToLog(s: string);
    procedure DoOnGetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);
    procedure DoOnSetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);

    procedure DoOnGetListOfAvailableSetVarActions(AListOfSetVarActions: TStringList);
    procedure DoOnGetListOfAvailableActions(AListOfActions: TStringList);
    function DoOnTClkIniReadonlyFileCreate(AFileName: string): TClkIniReadonlyFile;
    procedure DoOnModifyPluginProperty(AAction: PClkActionRec);

    procedure DoOnPluginDbgStop;
    procedure DoOnPluginDbgContinueAll;
    procedure DoOnPluginDbgStepOver;
    function DoOnPluginDbgRequestLineNumber(out ALineContent, ADbgSymFile: string): Integer;
    procedure DoOnPluginDbgSetBreakpoint(ALineIndex, ASelectedSourceFileIndex: Integer; AEnabled: Boolean);
    function DoOnTClkIniFileCreate(AFileName: string): TClkIniFile;

    function DoOnGetSelfTemplatesDir: string;
    procedure DoOnShowAutoComplete(AEdit: TEdit);
    procedure DoOnUpdateActionScrollIndex(AActionScrollIndex: string);
    function DoOnGetLoadedTemplateFileName: string;
    procedure DoOnChangeEditTemplateEditingActionType;

    procedure ClkVariablesOnChange(Sender: TObject);
    procedure AddDecodedVarToNode(ANode: PVirtualNode; ARecursionLevel: Integer);
    procedure CreateSelectionLabelsForResult;
    function GetSubVarByIndex(AMainVarValue: string; ASubVarIndex: Integer): string;
    procedure GetDecodedVarDetails(AParentVarName: string; ASubVarIndex: Integer; out X, Y, W, H: Integer);
    procedure SelectAreaFromDecodedVariable(ANodeData: PVarNodeRec; ASubVarIndex: Integer);

    function GetInMemFS: TInMemFileSystem;
    procedure SetInMemFS(Value: TInMemFileSystem);
    function GetExtRenderingInMemFS: TInMemFileSystem;
    procedure SetExtRenderingInMemFS(Value: TInMemFileSystem);

    procedure SetLabelsFromMouseOverExecDbgImgPixelColor(APixelColor: TColor);

    function GetCurrentlyEditingActionType: TClkAction;
    procedure SetCurrentlyEditingActionType(Value: TClkAction);

    procedure SetGridDrawingOption(Value: TDisplayGridLineOption);
    procedure SetPreviewSelectionColors(Value: TSelectionColors);
    function GetModifiedPmtvFiles: Boolean;
    function GetEditingActionObjectByActionType: PClkActionRec;

    procedure LocalTemplatesClick(Sender: TObject);
    procedure AvailableSetVarClick(Sender: TObject);
    procedure AvailablePluginPropertiesClick(Sender: TObject);
    procedure AvailableEditingActionPropertiesClick_Cat_ActionSpecific(Sender: TObject);
    procedure AvailableEditingActionPropertiesClick_Cat_EditingAction(Sender: TObject);
    procedure AvailableFindSubControlSendRequestActionClick(Sender: TObject);
    procedure AvailableFindSubControlRcvBmpPluginActionClick(Sender: TObject);

    procedure BrowseTemplatesClick(Sender: TObject);
    procedure ClickerConditionEditorControlsModified;
    procedure OverlapGridImgOnDebugImg(ADebugAndGridBitmap: TBitmap);
    procedure CopyTextAndClassFromExternalProvider(AProviderName: string);
    procedure SetActionTimeoutToValue(AValue: Integer);
    function GetIndexOfFirstModifiedPmtvFile(AEditingAction: PClkActionRec): Integer;
    function GetIndexOfCurrentlyEditingPrimitivesFile: Integer;
    procedure UpdateMatchBitmapFilesItem(AEditingAction: PClkActionRec; AFileIndex: Integer; ANewFileName: string);

    function AddFontProfileToActionFromMenu(AForegroundColor, ABackgroundColor, AFontName: string; AFontSize: Integer; AFontQuality: TFontQuality): Integer;
    function GetUniqueProfileName(n: Integer): string;

    class function DummyEvaluateReplacements(VarName: string; Recursive: Boolean = True; AEvalTextCount: Integer = -1): string; //returns VarName

    procedure HandleOnUpdateBitmapAlgorithmSettings;
    procedure HandleOnTriggerOnControlsModified;
    function HandleOnEvaluateReplacements(s: string): string;
    function HandleOnReverseEvaluateReplacements(s: string): string;
    procedure HandleOnCopyControlTextAndClassFromMainWindow(ACompProvider: string; out AControlText, AControlClass: string);
    function HandleOnGetExtraSearchAreaDebuggingImage(AExtraBitmap: TBitmap): Boolean;

    function HandleOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function HandleOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    procedure HandleOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages: TStringList; Sender: TObject = nil);
    function HandleOnFileExists(const AFileName: string): Boolean;
    procedure HandleOnSetPictureOpenDialogInitialDir(AInitialDir: string);
    function HandleOnPictureOpenDialogExecute: Boolean;
    function HandleOnGetPictureOpenDialogFileName: string;

    procedure HandleOnUpdateSearchAreaLimitsInOIFromDraggingLines(ALimitLabelsToUpdate: TLimitLabels; var AOffsets: TSimpleRectString);
    procedure HandleOnUpdateTextCroppingLimitsInOIFromDraggingLines(ALimitLabelsToUpdate: TLimitLabels; var AOffsets: TSimpleRectString; AFontProfileIndex: Integer);
    function HandleOnGetDisplayedText: string;
    procedure HandleOnSetMatchTextAndClassToOI(AMatchText, AMatchClassName: string);
    function HandleOnGetFindControlOptions: PClkFindControlOptions;
    function HandleOnGetFindSubControlOptions: PClkFindSubControlOptions;
    function HandleOnGetIsFindSubControl: Boolean;

    function HandleOnExecuteFindSubControlAction(AErrorLevel, AErrorCount, AFastSearchErrorCount: Integer; AFontName: string; AFontSize: Integer; out AFoundArea: TRect): Boolean;
    procedure HandleOnAddToLog(s: string);

    procedure HandleOnClickerExecAppFrame_OnTriggerOnControlsModified;
    procedure HandleOnClickerSetVarFrame_OnTriggerOnControlsModified;
    function HandleOnClickerSetVarFrame_OnGetFullTemplatesDir: string;
    function HandleOnClickerSetVarFrame_OnGetSelfTemplatesDir: string;
    procedure HandleOnClickerSetVarFrame_OnShowAutoComplete(AEdit: TEdit);
    procedure HandleOnClickerCallTemplateFrame_OnTriggerOnControlsModified;

    function HandleOnEvaluateReplacementsFunc(s: string; Recursive: Boolean = True; AEvalTextCount: Integer = -1): string;
    procedure HandleOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    procedure HandleOnSavePrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    procedure HandleOnPrimitivesTriggerOnControlsModified;
    procedure HandleOnSaveFromMenu(Sender: TObject);
    procedure HandleOnGetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);
    procedure HandleOnSetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);

    procedure HandleOnPluginDbgStop;
    procedure HandleOnPluginDbgContinueAll;
    procedure HandleOnPluginDbgStepOver;
    function HandleOnPluginDbgRequestLineNumber(out ALineContent, ADbgSymFile: string): Integer;
    procedure HandleOnPluginDbgSetBreakpoint(ALineIndex, ASelectedSourceFileIndex: Integer; AEnabled: Boolean);

    function HandleOnTClkIniFileCreate(AFileName: string): TClkIniFile;

    {$IFDEF MemPlugins}
      function HandleOnLoadPluginFromInMemFS(APlugin: TMemoryStream; AFileName: string): Boolean;
    {$ENDIF}

    ///////////////////////////// OI
    function EditFontProperties(AEditingAction: PClkActionRec; AItemIndexDiv: Integer; var ANewItems: string): Boolean;

    procedure FreeOIPopupMenu(Sender: TObject);
    procedure BuildFontColorIconsList;

    function HandleOnOIGetCategoryCount: Integer;
    function HandleOnOIGetCategory(AIndex: Integer): string;
    function HandleOnOIGetPropertyCount(ACategoryIndex: Integer): Integer;
    function HandleOnOIGetPropertyName(ACategoryIndex, APropertyIndex: Integer): string;
    function HandleOnOIGetPropertyValue(ACategoryIndex, APropertyIndex: Integer; var AEditorType: TOIEditorType): string;
    function HandleOnOIGetListPropertyItemCount(ACategoryIndex, APropertyIndex: Integer): Integer;
    function HandleOnOIGetListPropertyItemName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
    function HandleOnOIGetListPropertyItemValue(ACategoryIndex, APropertyIndex, AItemIndex: Integer; var AEditorType: TOIEditorType): string;
    function HandleOnOIGetDataTypeName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
    function HandleOnOIGetExtraInfo(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;

    procedure OIGetImageIndexEx_ActionSpecific(ALiveEditingActionType: TClkAction; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);

    procedure HandleOnOIGetImageIndexEx(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);

    procedure UpdateGPUTargetPlatformByType(AOldText: string; AEditingAction: PClkActionRec; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; ANewText: string);
    procedure UpdateGPUTargetDeviceByType(AOldText: string; AEditingAction: PClkActionRec; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; ANewText: string);
    procedure OIEditedText_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; ANewText: string);
    procedure SetEmptyEditedActionByEditTemplateToDefault;
    procedure HandleOnOIEditedText(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; ANewText: string);

    procedure OpenFontsMenu(AEditingAction: PClkActionRec; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer);
    procedure HandleOIEditorMenuOnClose(Sender: TObject);
    function OIEditItems_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewItems: string): Boolean;
    function HandleOnOIEditItems(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewItems: string): Boolean;

    function HandleOnOIGetColorConstsCount(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer;
    procedure HandleOnOIGetColorConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AColorItemIndex: Integer; var AColorName: string; var AColorValue: Int64);

    function HandleOnOIGetEnumConstsCount(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer;
    procedure HandleOnOIGetEnumConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEnumItemIndex: Integer; var AEnumItemName: string; var AEnumImgItemIndex: Integer; var AImgLst: TImageList);

    procedure HandleOnOIPaintText(ANodeData: TNodeDataPropertyRec; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
      const TargetCanvas: TCanvas; Column: TColumnIndex; var TextType: TVSTTextType);

    procedure HandleOnOIBeforeCellPaint(ANodeData: TNodeDataPropertyRec; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
      TargetCanvas: TCanvas; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);

    procedure HandleOnOIAfterCellPaint(ANodeData: TNodeDataPropertyRec; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
      TargetCanvas: TCanvas; Column: TColumnIndex; const CellRect: TRect);

    procedure TextEditorMouseDown_ActionSpecific(AEditingAction: PClkActionRec; ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HandleOnTextEditorMouseDown(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    function TextEditorMouseMove_ActionSpecific(AEditingAction: PClkActionRec; ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      Sender: TObject; Shift: TShiftState; X, Y: Integer): Boolean;
    function HandleOnTextEditorMouseMove(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      Sender: TObject; Shift: TShiftState; X, Y: Integer): Boolean;

    procedure OITextEditorKeyUp_ActionSpecific(AEditingAction: PClkActionRec; ACategoryIndex, APropertyIndex, AItemIndex: Integer; Sender: TObject);
    procedure HandleOnOITextEditorKeyUp(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure HandleOnOITextEditorKeyDown(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure HandleOnOIEditorKeyDown(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure OIEditorAssignMenuAndTooltip_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      Sender: TObject; var APopupMenu: TPopupMenu; var AHint: string; var AShowHint: Boolean);
    procedure HandleOnOIEditorAssignMenuAndTooltip(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      Sender: TObject; var APopupMenu: TPopupMenu; var AHint: string; var AShowHint: Boolean);

    procedure HandleOnOIGetFileDialogSettings(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var AFilter, AInitDir: string);

    procedure AddGPUPlatformsAsMenuItems(AOIEditorMenu: TPopupMenu; AMenuHandler: TNotifyEvent; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; AEditingAction: PClkActionRec);
    procedure AddGPUDevicesAsMenuItems(AOIEditorMenu: TPopupMenu; AMenuHandler: TNotifyEvent; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; AEditingAction: PClkActionRec);
    procedure CreateMenuWithExtMemBitmaps(ABmpTempMenuItem: TMenuItem; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; AEditingAction: PClkActionRec; ABmpItemHandler, ABrowseWithPreviewHandler: TNotifyEvent; AWithExtMemPrefixOnly: Boolean);
    procedure OIArrowEditorClick_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer);
    procedure HandleOnOIArrowEditorClick(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer);

    function ResolveTemplatePath(APath: string; ACustomSelfTemplateDir: string = ''; ACustomAppDir: string = ''): string;
    procedure OIUserEditorClick_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ARepaintValue: Boolean);
    procedure HandleOnOIUserEditorClick(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ARepaintValue: Boolean);

    function OIBrowseFile_ActionSpecific(ALiveEditingActionType: TClkAction; ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      AFilter, ADialogInitDir: string; var Handled: Boolean; AReturnMultipleFiles: Boolean = False): string;
    function HandleOnOIBrowseFile(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      AFilter, ADialogInitDir: string; var Handled: Boolean; AReturnMultipleFiles: Boolean = False): string;

    procedure OIAfterSpinTextEditorChanging_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewValue: string);
    procedure HandleOnOIAfterSpinTextEditorChanging(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewValue: string);

    procedure HandleOnOISelectedNode_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, Column: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HandleOnOISelectedNode(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, Column: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure OIFirstVisibleNode_ActionSpecific(AEditingAction: PClkActionRec; NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer);
    procedure HandleOnOIFirstVisibleNode(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer);

    function GetPropertyName_ForChecking_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
    procedure HandleOnOIInitNode(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer; var ACheckType: TCheckType; var ACheckState: TCheckState; var ANodeHeight: Word);
    procedure HandleOnOIChecked(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer; ACheckState: TCheckState);
    procedure HandleOnOIChecking(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer; ACheckState: TCheckState; var ANewState: TCheckState; var AAllowed: Boolean);
  public
    { Public declarations }
    frClickerConditionEditor: TfrClickerConditionEditor;  //public, because it is accessed from outside :(
    frClickerFindControl: TfrClickerFindControl;
    frClickerExecApp: TfrClickerExecApp;
    frClickerSetVar: TfrClickerSetVar;
    frClickerCallTemplate: TfrClickerCallTemplate;
    frClickerSleep: TfrClickerSleep;
    frClickerPlugin: TfrClickerPlugin;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function EvaluateReplacements(VarName: string; Recursive: Boolean = True; AEvalTextCount: Integer = -1): string;
    function ReverseEvaluateReplacements(AValue: string): string;

    procedure LoadListOfAvailableTemplates;
    procedure LoadListOfAvailableSetVarActions(AEditingAction: PClkActionRec);
    procedure LoadListOfAvailableActionsForPlugin(APropertyIndexToUpdate: Integer; AEditingAction: PClkActionRec);
    procedure LoadListOfAvailableActionsForEditTemplate(APropertyIndexToUpdate: Integer; AEditingAction: PClkActionRec);
    procedure LoadListOfAvailableActionsForFindSubControlRenderingRequest(APropertyIndexToUpdate: Integer; AEditingAction: PClkActionRec; AAllowedActionTypes: TClkActions; AHandler: TNotifyEvent);

    procedure SetDebugVariablesFromListOfStrings(AListOfStrings: string);
    procedure UpdatePageControlActionExecutionIcons;
    procedure UpdateControlWidthHeightLabels;
    procedure UpdateUseWholeScreenLabel(AUseWholeScreen: Boolean);
    procedure RefreshActionName; //called by action list, when modifying the action name from there
    procedure ResetAllPmtvModifiedFlags; //called when users select a diffeent action from the one with modified pmtv files
    procedure ResizeFrameSectionsBySplitter(NewLeft: Integer);
    procedure ResizeFrameSectionsBySplitterResults(NewLeft: Integer);
    procedure BringOIPropertyIntoView(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer);

    procedure ClearControls;
    procedure UpdateFindSubControlInternalStructuresFromAction(AAction: PClkActionRec);
    procedure SerializeEditTemplateEditingAction;
    procedure DeserializeEditTemplateEditingAction;

    property BMPsDir: string read FBMPsDir write FBMPsDir;  /////////////////////////// to be removed
    property ControlsModified: Boolean read FControlsModified write FControlsModified;
    property OnControlsModified: TNotifyEvent read FOnControlsModified write FOnControlsModified;
    property PredefinedVarCount: Integer read FPredefinedVarCount write FPredefinedVarCount;
    property DebuggingInfoAvailable: Boolean write SetDebuggingInfoAvailable;
    property FullTemplatesDir: string read FFullTemplatesDir write FFullTemplatesDir;  //no trailing backslash
    //property ListOfSetVarEntries: string read GetListOfSetVarEntries write SetListOfSetVarEntries;

    property ListOfCustomVariables: string read GetListOfCustomVariables write SetListOfCustomVariables;
    property InMemFS: TInMemFileSystem read GetInMemFS write SetInMemFS;
    property ExtRenderingInMemFS: TInMemFileSystem read GetExtRenderingInMemFS write SetExtRenderingInMemFS;

    property CurrentlyEditingActionType: TClkAction read GetCurrentlyEditingActionType write SetCurrentlyEditingActionType;
    property EditingAction: PClkActionRec read FEditingAction; //the pointer is not writable from outside, only the content

    property GridDrawingOption: TDisplayGridLineOption write SetGridDrawingOption;
    property PreviewSelectionColors: TSelectionColors write SetPreviewSelectionColors;
    property ModifiedPmtvFiles: Boolean read GetModifiedPmtvFiles;

    property ClkVariables: TStringList read FClkVariables;

    property OnCopyControlTextAndClassFromMainWindow: TOnCopyControlTextAndClassFromMainWindow read FOnCopyControlTextAndClassFromMainWindow write FOnCopyControlTextAndClassFromMainWindow;
    property OnGetExtraSearchAreaDebuggingImage: TOnGetExtraSearchAreaDebuggingImage write FOnGetExtraSearchAreaDebuggingImage;
    property OnEditCallTemplateBreakCondition: TOnEditActionCondition write FOnEditCallTemplateBreakCondition;

    property OnLoadBitmap: TOnLoadBitmap write FOnLoadBitmap;
    property OnLoadRenderedBitmap: TOnLoadRenderedBitmap write FOnLoadRenderedBitmap;
    property OnGetListOfExternallyRenderedImages: TOnGetListOfExternallyRenderedImages write FOnGetListOfExternallyRenderedImages;

    {$IFDEF MemPlugins}
      property OnGetListOfInMemPlugins: TOnGetListOfInMemPlugins write FOnGetListOfInMemPlugins;
      property OnLoadPluginFromDiskToPluginInMemFileSystem: TOnLoadPluginFromDiskToPluginInMemFileSystem write FOnLoadPluginFromDiskToPluginInMemFileSystem;
      property OnLoadPluginFromInMemFS: TOnLoadPluginFromInMemFS write FOnLoadPluginFromInMemFS;
    {$ENDIF}
    property OnGetPluginInMemFS: TOnGetPluginInMemFS write FOnGetPluginInMemFS;

    property OnLoadPrimitivesFile: TOnLoadPrimitivesFile write FOnLoadPrimitivesFile;
    property OnSavePrimitivesFile: TOnSavePrimitivesFile write FOnSavePrimitivesFile;
    property OnFileExists: TOnFileExists write FOnFileExists;

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

    property OnExecuteFindSubControlAction: TOnExecuteFindSubControlAction write FOnExecuteFindSubControlAction;
    property OnAddToLog: TOnAddToLog write FOnAddToLog;
    property OnGetFontFinderSettings: TOnRWFontFinderSettings write FOnGetFontFinderSettings;
    property OnSetFontFinderSettings: TOnRWFontFinderSettings write FOnSetFontFinderSettings;

    property OnGetListOfAvailableSetVarActions: TOnGetListOfAvailableSetVarActions write FOnGetListOfAvailableSetVarActions;
    property OnGetListOfAvailableActions: TOnGetListOfAvailableActions write FOnGetListOfAvailableActions;
    property OnTClkIniReadonlyFileCreate: TOnTClkIniReadonlyFileCreate write FOnTClkIniReadonlyFileCreate;
    property OnModifyPluginProperty: TOnModifyPluginProperty write FOnModifyPluginProperty;

    property OnPluginDbgStop: TOnPluginDbgStop write FOnPluginDbgStop;
    property OnPluginDbgContinueAll: TOnPluginDbgContinueAll write FOnPluginDbgContinueAll;
    property OnPluginDbgStepOver: TOnPluginDbgStepOver write FOnPluginDbgStepOver;
    property OnPluginDbgRequestLineNumber: TOnPluginDbgRequestLineNumber write FOnPluginDbgRequestLineNumber;
    property OnPluginDbgSetBreakpoint: TOnPluginDbgSetBreakpoint write FOnPluginDbgSetBreakpoint;
    property OnTClkIniFileCreate: TOnTClkIniFileCreate write FOnTClkIniFileCreate;

    property OnGetSelfTemplatesDir: TOnGetFullTemplatesDir write FOnGetSelfTemplatesDir;
    property OnShowAutoComplete: TOnShowAutoComplete write FOnShowAutoComplete;
    property OnUpdateActionScrollIndex: TOnUpdateActionScrollIndex write FOnUpdateActionScrollIndex;
    property OnGetLoadedTemplateFileName: TOnGetLoadedTemplateFileName write FOnGetLoadedTemplateFileName;
    property OnChangeEditTemplateEditingActionType: TOnChangeEditTemplateEditingActionType write FOnChangeEditTemplateEditingActionType;
  end;


const
  CXClickPointReference: array[Low(TXClickPointReference)..High(TXClickPointReference)] of string = ('Control Left', 'Control Right', 'Control Width', 'Var', 'Screen Absolute X');
  CYClickPointReference: array[Low(TYClickPointReference)..High(TYClickPointReference)] of string = ('Control Top', 'Control Bottom', 'Control Height', 'Var', 'Screen Absolute Y');
  {$IFDEF FPC}
    ID_YES = IDYES;  //from Delphi
  {$ENDIF}

  COIScrollInfo_NodeLevel = 'NodeLevel';
  COIScrollInfo_CategoryIndex = 'CategoryIndex';
  COIScrollInfo_PropertyIndex = 'PropertyIndex';
  COIScrollInfo_PropertyItemIndex = 'PropertyItemIndex';


function ActionStatusStrToActionStatus(AString: string): TActionStatus;  

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.frm}
{$ENDIF}

uses
  Clipbrd, ClickerActionValues, ClickerOIUtils, ClickerZoomPreviewForm,
  ClickerActionPluginLoader, ClickerActionPlugins, InMemFileSystemBrowserForm,
  ClickerExtraUtils, ClickerActionProperties, ClickerTemplates, Math,
  ClickerCLUtils, ClickerIconsDM;


function ActionStatusStrToActionStatus(AString: string): TActionStatus;
var
  i: TActionStatus;
begin
  Result := asNotStarted;
  for i := Low(TActionStatus) to High(TActionStatus) do
    if CActionStatusStr[i] = AString then
    begin
      Result := i;
      Exit;
    end;
end;


procedure TfrClickerActions.ClickerConditionEditorControlsModified;
begin
  FEditingAction^.ActionOptions.ActionCondition := frClickerConditionEditor.GetActionCondition;
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.CreateRemainingUIComponents;
begin
  frClickerFindControl := TfrClickerFindControl.Create(Self);
  frClickerFindControl.Parent := pnlExtra;

  frClickerFindControl.Left := 3;
  frClickerFindControl.Top := 3;
  frClickerFindControl.Width := 877;
  frClickerFindControl.Height := 201;
  frClickerFindControl.Anchors := [akLeft, akTop, akBottom, akRight];
  frClickerFindControl.OnUpdateBitmapAlgorithmSettings := HandleOnUpdateBitmapAlgorithmSettings;
  frClickerFindControl.OnTriggerOnControlsModified := HandleOnTriggerOnControlsModified;
  frClickerFindControl.OnEvaluateReplacements := HandleOnEvaluateReplacements;
  frClickerFindControl.OnReverseEvaluateReplacements := HandleOnReverseEvaluateReplacements;
  frClickerFindControl.OnCopyControlTextAndClassFromMainWindow := HandleOnCopyControlTextAndClassFromMainWindow;
  frClickerFindControl.OnGetExtraSearchAreaDebuggingImage := HandleOnGetExtraSearchAreaDebuggingImage;
  frClickerFindControl.OnLoadBitmap := HandleOnLoadBitmap;
  //frClickerFindControl.OnLoadRenderedBitmap := HandleOnLoadRenderedBitmap;
  frClickerFindControl.OnFileExists := HandleOnFileExists;
  frClickerFindControl.OnSetPictureOpenDialogInitialDir := HandleOnSetPictureOpenDialogInitialDir;
  frClickerFindControl.OnPictureOpenDialogExecute := HandleOnPictureOpenDialogExecute;
  frClickerFindControl.OnGetPictureOpenDialogFileName := HandleOnGetPictureOpenDialogFileName;
  frClickerFindControl.OnUpdateSearchAreaLimitsInOIFromDraggingLines := HandleOnUpdateSearchAreaLimitsInOIFromDraggingLines;
  frClickerFindControl.OnUpdateTextCroppingLimitsInOIFromDraggingLines := HandleOnUpdateTextCroppingLimitsInOIFromDraggingLines;
  frClickerFindControl.OnGetDisplayedText := HandleOnGetDisplayedText;
  frClickerFindControl.OnSetMatchTextAndClassToOI := HandleOnSetMatchTextAndClassToOI;
  frClickerFindControl.OnGetFindControlOptions := HandleOnGetFindControlOptions;
  frClickerFindControl.OnGetFindSubControlOptions := HandleOnGetFindSubControlOptions;
  frClickerFindControl.OnGetIsFindSubControl := HandleOnGetIsFindSubControl;
  frClickerFindControl.OnExecuteFindSubControlAction := HandleOnExecuteFindSubControlAction;
  frClickerFindControl.OnAddToLog := HandleOnAddToLog;
  frClickerFindControl.OnGetFontFinderSettings := HandleOnGetFontFinderSettings;
  frClickerFindControl.OnSetFontFinderSettings := HandleOnSetFontFinderSettings;

  frClickerFindControl.Visible := False;

  //frClickerFindControl.AddDefaultFontProfile;

  frClickerConditionEditor := TfrClickerConditionEditor.Create(Self);
  frClickerConditionEditor.Parent := pnlActionConditions; //for some reason, using TabSheetCondition leads to a hidden frame

  frClickerConditionEditor.Left := 3;
  frClickerConditionEditor.Top := 3;
  frClickerConditionEditor.Width := pnlActionConditions.Width - 3;
  frClickerConditionEditor.Height := pnlActionConditions.Height - 12;
  frClickerConditionEditor.Anchors := [akBottom, akLeft, akRight, akTop];
  frClickerConditionEditor.OnControlsModified := ClickerConditionEditorControlsModified;
  frClickerConditionEditor.Visible := True;

  frClickerExecApp := TfrClickerExecApp.Create(Self);
  frClickerExecApp.Parent := pnlExtra;
  frClickerExecApp.OnTriggerOnControlsModified := HandleOnClickerExecAppFrame_OnTriggerOnControlsModified;
  frClickerExecApp.Left := 3;
  frClickerExecApp.Top := 3;
  frClickerExecApp.Width := pnlExtra.Width - 3;
  frClickerExecApp.Height := pnlExtra.Height - 3;
  frClickerExecApp.Visible := False;

  frClickerSetVar := TfrClickerSetVar.Create(Self);
  frClickerSetVar.Parent := pnlExtra;
  frClickerSetVar.OnTriggerOnControlsModified := HandleOnClickerSetVarFrame_OnTriggerOnControlsModified;
  frClickerSetVar.OnGetFullTemplatesDir := HandleOnClickerSetVarFrame_OnGetFullTemplatesDir;
  frClickerSetVar.OnGetSelfTemplatesDir := HandleOnClickerSetVarFrame_OnGetSelfTemplatesDir;
  frClickerSetVar.OnShowAutoComplete := HandleOnClickerSetVarFrame_OnShowAutoComplete;
  frClickerSetVar.Left := 3;
  frClickerSetVar.Top := 3;
  frClickerSetVar.Width := pnlExtra.Width - 3;
  frClickerSetVar.Height := pnlExtra.Height - 3;
  frClickerSetVar.Visible := False;

  frClickerCallTemplate := TfrClickerCallTemplate.Create(Self);
  frClickerCallTemplate.Parent := pnlExtra;
  frClickerCallTemplate.OnTriggerOnControlsModified := HandleOnClickerCallTemplateFrame_OnTriggerOnControlsModified;
  frClickerCallTemplate.Left := 3;
  frClickerCallTemplate.Top := 3;
  frClickerCallTemplate.Width := pnlExtra.Width - 3;
  frClickerCallTemplate.Height := pnlExtra.Height - 3;
  frClickerCallTemplate.Visible := False;
  frClickerCallTemplate.Anchors := [akBottom, akLeft, akRight, akTop];

  frClickerSleep := TfrClickerSleep.Create(Self);
  frClickerSleep.Parent := pnlExtra;
  frClickerSleep.Left := 3;
  frClickerSleep.Top := 3;
  frClickerSleep.Width := pnlExtra.Width - 3;
  frClickerSleep.Height := pnlExtra.Height - 3;
  frClickerSleep.Visible := False;

  frClickerPlugin := TfrClickerPlugin.Create(Self);
  frClickerPlugin.Parent := pnlExtra;
  frClickerPlugin.Left := 3;
  frClickerPlugin.Top := 3;
  frClickerPlugin.Width := pnlExtra.Width - 3;
  frClickerPlugin.Height := pnlExtra.Height - 3;
  frClickerPlugin.Anchors := [akLeft, akTop, akBottom, akRight];
  frClickerPlugin.Visible := False;
  frClickerPlugin.OnPluginDbgStop := HandleOnPluginDbgStop;
  frClickerPlugin.OnPluginDbgContinueAll := HandleOnPluginDbgContinueAll;
  frClickerPlugin.OnPluginDbgStepOver := HandleOnPluginDbgStepOver;
  frClickerPlugin.OnPluginDbgRequestLineNumber := HandleOnPluginDbgRequestLineNumber;
  frClickerPlugin.OnPluginDbgSetBreakpoint := HandleOnPluginDbgSetBreakpoint;
  frClickerPlugin.OnTClkIniFileCreate := HandleOnTClkIniFileCreate;

  {$IFDEF MemPlugins}
    frClickerPlugin.OnLoadPluginFromInMemFS := HandleOnLoadPluginFromInMemFS;
  {$ENDIF}

  FPmLocalTemplates := TPopupMenu.Create(Self);

  ////////////////////////////// OI
  FOIFrame := TfrObjectInspector.Create(Self);
  FOIFrame.Parent := pnlvstOI;
  FOIFrame.Left := 0;
  FOIFrame.Top := 0;
  FOIFrame.Width := pnlvstOI.Width;
  FOIFrame.Height := pnlvstOI.Height;
  FOIFrame.Anchors := [akBottom, akLeft, akRight, akTop];

  pnlvstOI.Anchors := [akBottom, akLeft, akRight, akTop];

  FOIFrame.OnOIGetCategoryCount := HandleOnOIGetCategoryCount;
  FOIFrame.OnOIGetCategory := HandleOnOIGetCategory;
  FOIFrame.OnOIGetPropertyCount := HandleOnOIGetPropertyCount;
  FOIFrame.OnOIGetPropertyName := HandleOnOIGetPropertyName;
  FOIFrame.OnOIGetPropertyValue := HandleOnOIGetPropertyValue;
  FOIFrame.OnOIGetListPropertyItemCount := HandleOnOIGetListPropertyItemCount;
  FOIFrame.OnOIGetListPropertyItemName := HandleOnOIGetListPropertyItemName;
  FOIFrame.OnOIGetListPropertyItemValue := HandleOnOIGetListPropertyItemValue;
  FOIFrame.OnOIGetDataTypeName := HandleOnOIGetDataTypeName;
  FOIFrame.OnOIGetExtraInfo := HandleOnOIGetExtraInfo;
  FOIFrame.OnOIGetImageIndexEx := HandleOnOIGetImageIndexEx;
  FOIFrame.OnOIEditedText := HandleOnOIEditedText;
  FOIFrame.OnOIEditItems := HandleOnOIEditItems;
  FOIFrame.OnOIGetColorConstsCount := HandleOnOIGetColorConstsCount;
  FOIFrame.OnOIGetColorConst := HandleOnOIGetColorConst;
  FOIFrame.OnOIGetEnumConstsCount := HandleOnOIGetEnumConstsCount;
  FOIFrame.OnOIGetEnumConst := HandleOnOIGetEnumConst;
  FOIFrame.OnOIPaintText := HandleOnOIPaintText;
  FOIFrame.OnOIBeforeCellPaint := HandleOnOIBeforeCellPaint;
  FOIFrame.OnOIAfterCellPaint := HandleOnOIAfterCellPaint;
  FOIFrame.OnOITextEditorMouseDown := HandleOnTextEditorMouseDown;
  FOIFrame.OnOITextEditorMouseMove := HandleOnTextEditorMouseMove;
  FOIFrame.OnOITextEditorKeyUp := HandleOnOITextEditorKeyUp;
  FOIFrame.OnOITextEditorKeyDown := HandleOnOITextEditorKeyDown;
  FOIFrame.OnOIEditorKeyDown := HandleOnOIEditorKeyDown;
  FOIFrame.OnOIEditorAssignMenuAndTooltip := HandleOnOIEditorAssignMenuAndTooltip;
  FOIFrame.OnOIGetFileDialogSettings := HandleOnOIGetFileDialogSettings;
  FOIFrame.OnOIArrowEditorClick := HandleOnOIArrowEditorClick;
  FOIFrame.OnOIUserEditorClick := HandleOnOIUserEditorClick;
  FOIFrame.OnOIBrowseFile := HandleOnOIBrowseFile;
  FOIFrame.OnOIAfterSpinTextEditorChanging := HandleOnOIAfterSpinTextEditorChanging;
  FOIFrame.OnOISelectedNode := HandleOnOISelectedNode;
  FOIFrame.OnOIFirstVisibleNode := HandleOnOIFirstVisibleNode;
  FOIFrame.OnOIInitNode := HandleOnOIInitNode;
  FOIFrame.OnOIChecked := HandleOnOIChecked;
  FOIFrame.OnOIChecking := HandleOnOIChecking;

  FOIFrame.Visible := True;

  FOIFrame.ListItemsVisible := True;
  FOIFrame.DataTypeVisible := True; //False;
  FOIFrame.ExtraInfoVisible := False;
  FOIFrame.PropertyItemHeight := 22; //50;  //this should be 50 for bitmaps
  FOIFrame.OICaption := 'ActionPropertiesOI';

  //FOIFrame.ReloadContent;  //set by ActionType combobox
  pnlvstOI.Visible := True;
end;



constructor TfrClickerActions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateRemainingUIComponents;

  FFullTemplatesDir := 'Non-existentFolder'; //ExtractFilePath(ParamStr(0)) + 'ActionTemplates'; //init value can be overridden by external wrapper
  FHold := False;
  FHoldResults := False;
  FEditingText := '';

  FSearchAreaScrBox := nil;
  FSearchAreaSearchedBmpDbgImg := nil;
  FSearchAreaSearchedTextDbgImg := nil;
  FSetVarContent_Vars := TStringList.Create;
  FSetVarContent_Values := TStringList.Create;
  FSetVarContent_EvalBefore := TStringList.Create;
  FLastClickedTVTEdit := nil;
  FLastClickedEdit := nil;
  FOIEditorMenu := TPopupMenu.Create(Self);
  FClkVariables := TStringList.Create;
  FClkVariables.OnChange := ClkVariablesOnChange;

  FSetVarContent_Vars.LineBreak := #13#10;
  FSetVarContent_Values.LineBreak := #13#10;
  FSetVarContent_EvalBefore.LineBreak := #13#10;
  FClkVariables.LineBreak := #13#10;

  FlblResultSelLeft := nil;
  FlblResultSelTop := nil;
  FlblResultSelRight := nil;
  FlblResultSelBottom := nil;

  FOnCopyControlTextAndClassFromMainWindow := nil;
  FOnGetExtraSearchAreaDebuggingImage := nil;
  FOnEditCallTemplateBreakCondition := nil;

  FOnLoadBitmap := nil;
  FOnLoadRenderedBitmap := nil;
  FOnGetListOfExternallyRenderedImages := nil;

  {$IFDEF MemPlugins}
    FOnGetListOfInMemPlugins := nil;
    FOnLoadPluginFromDiskToPluginInMemFileSystem := nil;
    FOnLoadPluginFromInMemFS := nil;
  {$ENDIF}
  FOnGetPluginInMemFS := nil;

  FOnLoadPrimitivesFile := nil;
  FOnSavePrimitivesFile := nil;
  FOnFileExists := nil;

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

  FOnExecuteFindSubControlAction := nil;
  FOnAddToLog := nil;
  FOnGetFontFinderSettings := nil;
  FOnSetFontFinderSettings := nil;

  FOnGetListOfAvailableSetVarActions := nil;
  FOnGetListOfAvailableActions := nil;
  FOnTClkIniReadonlyFileCreate := nil;
  FOnModifyPluginProperty := nil;

  FOnPluginDbgStop := nil;
  FOnPluginDbgContinueAll := nil;
  FOnPluginDbgStepOver := nil;
  FOnPluginDbgRequestLineNumber := nil;
  FOnPluginDbgSetBreakpoint := nil;
  FOnTClkIniFileCreate := nil;

  FOnGetSelfTemplatesDir := nil;
  FOnShowAutoComplete := nil;
  FOnUpdateActionScrollIndex := nil;
  FOnGetLoadedTemplateFileName := nil;
  FOnChangeEditTemplateEditingActionType := nil;

  FShowDeprecatedControls := False;
  FEditingAction := @FEditingActionRec;
  FEditTemplateOptions_EditingAction := @FClkEditedActionByEditTemplate;
  FCurrentlyEditingPrimitiveFileName := '';
  FPrevSelectedPrimitiveNode := -1;

  PageControlActionExecution.ActivePageIndex := 0;
  PageControlActionExecution.Caption := 'ActionExecution';
end;


destructor TfrClickerActions.Destroy;
begin
  FSetVarContent_Vars.Free;
  FSetVarContent_Values.Free;
  FSetVarContent_EvalBefore.Free;
  FOIEditorMenu.Free;
  FreeAndNil(FClkVariables);

  inherited Destroy;
end;


function TfrClickerActions.EvaluateReplacements(VarName: string; Recursive: Boolean = True; AEvalTextCount: Integer = -1): string;
begin
  Result := EvaluateAllReplacements(FClkVariables, VarName, Recursive, AEvalTextCount);
end;


function TfrClickerActions.ReverseEvaluateReplacements(AValue: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FClkVariables.Count - 1 do
    if FClkVariables.ValueFromIndex[i] = AValue then
    begin
      Result := FClkVariables.Names[i];
      Break;
    end;
end;


procedure TfrClickerActions.tmrDrawZoomTimer(Sender: TObject);
var
  tp: TPoint;
begin
  tmrDrawZoom.Enabled := false;
  GetCursorPos(tp);

  if Assigned(imgDebugBmp.Picture.Bitmap) then
    SetZoomContent(imgDebugBmp.Picture.Bitmap, FCurrentMousePosOnPreviewImg.X, FCurrentMousePosOnPreviewImg.Y, tp.X + 50, tp.Y + 50);
end;


procedure TfrClickerActions.tmrEditClkVariablesTimer(Sender: TObject);
var
  TempBounds: TRect;
begin
  tmrEditClkVariables.Enabled := False;
  if FHitInfo.HitNode = nil then
    Exit;

  vstVariables.EditNode(FHitInfo.HitNode, FHitInfo.HitColumn);

  if FHitInfo.HitColumn in [0..1] then
    if Assigned(vstVariables.EditLink) then
    begin
      TempBounds := vstVariables.EditLink.GetBounds;
      TempBounds.Left := TempBounds.Left - 2;
      TempBounds.Right := Max(TempBounds.Right, TempBounds.Left + vstVariables.Header.Columns[FHitInfo.HitColumn].MinWidth);
      vstVariables.EditLink.SetBounds(TempBounds);

      FTextEditorEditBox.Height := vstVariables.DefaultNodeHeight;
    end;
end;


procedure TfrClickerActions.tmrOnChangeEditTemplateEditingActionTypeTimer(
  Sender: TObject);
begin
  tmrOnChangeEditTemplateEditingActionType.Enabled := False;
  DoOnChangeEditTemplateEditingActionType;
end;


procedure TfrClickerActions.imgDebugBmpMouseEnter(Sender: TObject);
var
  tp: TPoint;
begin
  imgDebugBmp.ShowHint := False;
  GetCursorPos(tp);
  ShowZoom(tp.X + 50, tp.Y + 50);
end;


procedure TfrClickerActions.imgDebugBmpMouseLeave(Sender: TObject);
begin
  imgDebugBmp.ShowHint := True;
  HideZoom;
end;


procedure TfrClickerActions.imgDebugBmpMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lblDebugBitmapXMouseOffset.Caption := 'mx: ' + IntToStr(X);
  lblDebugBitmapYMouseOffset.Caption := 'my: ' + IntToStr(Y);

  if (X > -1) and (Y > -1) and (X < imgDebugBmp.Picture.Bitmap.Width) and (Y < imgDebugBmp.Picture.Bitmap.Height) then
    SetLabelsFromMouseOverExecDbgImgPixelColor(imgDebugBmp.Canvas.Pixels[X, Y]);

  FCurrentMousePosOnPreviewImg.X := X;
  FCurrentMousePosOnPreviewImg.Y := Y;
  tmrDrawZoom.Enabled := True;
end;


procedure TfrClickerActions.FlblResultSelVertMouseEnter(Sender: TObject);
var
  tp: TPoint;
begin
  imgDebugBmp.ShowHint := False;
  GetCursorPos(tp);
  ShowZoom(tp.X + 50, tp.Y + 50);
end;


procedure TfrClickerActions.FlblResultSelVertMouseLeave(Sender: TObject);
begin
  imgDebugBmp.ShowHint := True;
  HideZoom;
end;


procedure TfrClickerActions.FlblResultSelVertMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  lblDebugBitmapXMouseOffset.Caption := 'mx: ' + IntToStr((Sender as TLabel).Left);
  lblDebugBitmapYMouseOffset.Caption := 'my: ' + IntToStr(Y);

  if (X > -1) and (Y > -1) and (X < imgDebugBmp.Picture.Bitmap.Width) and (Y < imgDebugBmp.Picture.Bitmap.Height) then
    SetLabelsFromMouseOverExecDbgImgPixelColor(imgDebugBmp.Canvas.Pixels[X, Y]);

  FCurrentMousePosOnPreviewImg.X := (Sender as TLabel).Left;
  FCurrentMousePosOnPreviewImg.Y := Y;
  tmrDrawZoom.Enabled := True;
end;


procedure TfrClickerActions.FlblResultSelHorizMouseEnter(Sender: TObject);
var
  tp: TPoint;
begin
  imgDebugBmp.ShowHint := False;
  GetCursorPos(tp);
  ShowZoom(tp.X + 50, tp.Y + 50);
end;


procedure TfrClickerActions.FlblResultSelHorizMouseLeave(Sender: TObject);
begin
  imgDebugBmp.ShowHint := True;
  HideZoom;
end;


procedure TfrClickerActions.FlblResultSelHorizMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  lblDebugBitmapXMouseOffset.Caption := 'mx: ' + IntToStr(X);
  lblDebugBitmapYMouseOffset.Caption := 'my: ' + IntToStr((Sender as TLabel).Top);

  if (X > -1) and (Y > -1) and (X < imgDebugBmp.Picture.Bitmap.Width) and (Y < imgDebugBmp.Picture.Bitmap.Height) then
    SetLabelsFromMouseOverExecDbgImgPixelColor(imgDebugBmp.Canvas.Pixels[X, Y]);

  FCurrentMousePosOnPreviewImg.X := X;
  FCurrentMousePosOnPreviewImg.Y := (Sender as TLabel).Top;
  tmrDrawZoom.Enabled := True;
end;


procedure TfrClickerActions.rdgrpSearchForControlModeClick(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.ClkVariablesOnChange(Sender: TObject);
begin
  tmrClkVariables.Enabled := True;
end;


procedure TfrClickerActions.RemoveVariable1Click(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstVariables.GetFirstSelected;
  if Node = nil then
  begin
    MessageBox(Handle, 'Please select a variable to be removed.', PChar(Application.Title), MB_ICONINFORMATION);
    Exit;
  end;

  if Integer(Node^.Index) < FPredefinedVarCount - 1 then
  begin
    MessageBox(Handle, 'Predefined variables are required.', PChar(Application.Title), MB_ICONINFORMATION);
    Exit;
  end;

  if MessageBox(Handle, PChar('Remove variable?' + #13#10 + FClkVariables.Strings[Node^.Index]), PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  vstVariables.Enabled := False;
  try
    FClkVariables.Delete(Node^.Index);
  finally
    vstVariables.RootNodeCount := vstVariables.RootNodeCount - 1;
    vstVariables.Repaint;
    vstVariables.Enabled := True;
  end;
end;


procedure TfrClickerActions.chkShowDebugGridClick(Sender: TObject);
begin
  imgDebugGrid.Visible := chkShowDebugGrid.Checked;
end;


procedure TfrClickerActions.MenuItemEraseDebugImageClick(Sender: TObject);
begin
  if MessageBox(Handle, 'Are you sure you want to erase the current image?', PChar(Caption), MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  imgDebugBmp.Canvas.Pen.Color := clWhite;
  imgDebugBmp.Canvas.Brush.Color := clWhite;
  imgDebugBmp.Canvas.Brush.Style := bsSolid;

  if imgDebugBmp.Width < 1 then
    imgDebugBmp.Width := 1;

  if imgDebugBmp.Height < 1 then
    imgDebugBmp.Height := 1;

  imgDebugBmp.Canvas.Rectangle(0, 0, imgDebugBmp.Width, imgDebugBmp.Height);
  imgDebugBmp.Repaint;

  //imgDebugGrid does not have to be cleared
end;


procedure TfrClickerActions.SetDebugVariablesFromListOfStrings(AListOfStrings: string);
var
  AStringList: TStringList;
  i: Integer;
  KeyValue, Key, Value: string;
begin
  AStringList := TStringList.Create;
  try
    AStringList.LineBreak := #13#10;
    AStringList.Text := AListOfStrings;
    for i := 0 to AStringList.Count - 1 do
    begin
      KeyValue := AStringList.Strings[i];
      Key := Copy(KeyValue, 1, Pos('=', KeyValue) - 1);
      Value := Copy(KeyValue, Pos('=', KeyValue) + 1, MaxInt);

      FClkVariables.Values[Key] := Value;
    end;  
  finally
    AStringList.Free;
  end;
end;


procedure TfrClickerActions.CopyDebugValuesListToClipboard1Click(
  Sender: TObject);
begin
  Clipboard.AsText := FClkVariables.Text;
end;


procedure TfrClickerActions.PasteDebugValuesListFromClipboard1Click(
  Sender: TObject);
begin
  SetDebugVariablesFromListOfStrings(Clipboard.AsText);
end;


procedure TfrClickerActions.PasteDebugValuesListFromMainExecutionList1Click(
  Sender: TObject);
begin
  // Only a placeholder here. This should be implemented where MainExecutionList is available (in children).
end;


procedure TfrClickerActions.lbeMatchTextSeparatorChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeSearchRectBottomChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeSearchRectBottomOffsetChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeSearchRectLeftChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeSearchRectLeftOffsetChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeSearchRectRightChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeSearchRectRightOffsetChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeSearchRectTopChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeSearchRectTopOffsetChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


function TfrClickerActions.GetListOfCustomVariables: string;
begin
  Result := frClickerCallTemplate.GetListOfCustomVariables;
end;


procedure TfrClickerActions.SetListOfCustomVariables(Value: string);
begin
  frClickerCallTemplate.SetListOfCustomVariables(Value);
end;


procedure TfrClickerActions.lbeMatchClassNameChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeMatchClassNameSeparatorChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.SetLabelsFromMouseOverExecDbgImgPixelColor(APixelColor: TColor);
begin
  lblMouseOnExecDbgImgRR.Caption := IntToHex(APixelColor and $FF, 2);
  lblMouseOnExecDbgImgGG.Caption := IntToHex(APixelColor shr 8 and $FF, 2);
  lblMouseOnExecDbgImgBB.Caption := IntToHex(APixelColor shr 16 and $FF, 2);
  lblMouseOnExecDbgImg.Color := APixelColor;
end;


procedure TfrClickerActions.chkWaitForControlToGoAwayChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.AddDecodedVarToNode(ANode: PVirtualNode; ARecursionLevel: Integer);
var
  Node: PVirtualNode;
  ChNode: PVirtualNode;
  NodeData: PVarNodeRec;
  ChNodeData: PVarNodeRec;
  i: Integer;
  SubVars: TStringList;
begin
  NodeData := vstVariables.GetNodeData(ANode);
  NodeData^.VarName := FClkVariables.Names[ANode^.Index];
  NodeData^.VarValue := FClkVariables.ValueFromIndex[ANode^.Index];

  SubVars := TStringList.Create;
  try
    SubVars.LineBreak := #13#10;
    SubVars.Text := FastReplace_45ToReturn(NodeData^.VarValue);
    for i := 0 to SubVars.Count - 1 do
    begin
      ChNode := vstVariables.AddChild(ANode);
      ChNodeData := vstVariables.GetNodeData(ChNode);
      ChNodeData^.VarName := NodeData^.VarName + '[' + IntToStr(i) + ']';
      ChNodeData^.VarValue := SubVars.Strings[i];
    end;
  finally
    SubVars.Free;
  end;

  if ARecursionLevel > 20 then
    Exit;

  Node := ANode.FirstChild;
  if Node = nil then
    Exit;

  repeat
    NodeData := vstVariables.GetNodeData(Node);

    if Pos(#4#5, NodeData^.VarValue) > 0 then
      AddDecodedVarToNode(Node, ARecursionLevel + 1);

    Node := Node^.NextSibling;
  until Node = nil;
end;


procedure TfrClickerActions.chkDecodeVariablesChange(Sender: TObject);
var
  Node: PVirtualNode;
begin
  if FlblResultSelLeft <> nil then
  begin
    FlblResultSelLeft.Visible := chkDecodeVariables.Checked;
    FlblResultSelTop.Visible := chkDecodeVariables.Checked;
    FlblResultSelRight.Visible := chkDecodeVariables.Checked;
    FlblResultSelBottom.Visible := chkDecodeVariables.Checked;
  end;

  Node := vstVariables.GetFirst;
  if Node = nil then
    Exit;

  repeat
    if Node^.ChildCount > 0 then
      vstVariables.DeleteChildren(Node);

    Node := Node^.NextSibling;
  until Node = nil;

  if not chkDecodeVariables.Checked then
    Exit;

  Node := vstVariables.GetFirst;
  repeat
    if Pos(#4#5, FClkVariables.ValueFromIndex[Node^.Index]) > 0 then
      AddDecodedVarToNode(Node, 0);

    Node := Node^.NextSibling;
  until Node = nil;
end;


procedure TfrClickerActions.CreateSelectionLabelsForResult;
begin
  if FlblResultSelLeft = nil then
  begin
    FlblResultSelLeft := TLabel.Create(Self);
    FlblResultSelTop := TLabel.Create(Self);
    FlblResultSelRight := TLabel.Create(Self);
    FlblResultSelBottom := TLabel.Create(Self);

    FlblResultSelLeft.Parent := scrboxDebugBmp;
    FlblResultSelTop.Parent := scrboxDebugBmp;
    FlblResultSelRight.Parent := scrboxDebugBmp;
    FlblResultSelBottom.Parent := scrboxDebugBmp;

    FlblResultSelLeft.Transparent := False;
    FlblResultSelTop.Transparent := False;
    FlblResultSelRight.Transparent := False;
    FlblResultSelBottom.Transparent := False;

    FlblResultSelLeft.Caption := '';
    FlblResultSelTop.Caption := '';
    FlblResultSelRight.Caption := '';
    FlblResultSelBottom.Caption := '';

    FlblResultSelLeft.AutoSize := False;
    FlblResultSelTop.AutoSize := False;
    FlblResultSelRight.AutoSize := False;
    FlblResultSelBottom.AutoSize := False;

    FlblResultSelLeft.Width := 1;
    FlblResultSelTop.Width := scrboxDebugBmp.Width;
    FlblResultSelRight.Width := 1;
    FlblResultSelBottom.Width := FlblResultSelTop.Width;

    FlblResultSelLeft.Height := scrboxDebugBmp.Height;
    FlblResultSelTop.Height := 1;
    FlblResultSelRight.Height := FlblResultSelLeft.Width;
    FlblResultSelBottom.Height := 1;

    FlblResultSelLeft.Anchors := [akLeft, akTop, akBottom];
    FlblResultSelTop.Anchors := [akLeft, akTop, akRight];
    FlblResultSelRight.Anchors := [akLeft, akTop, akBottom];
    FlblResultSelBottom.Anchors := [akLeft, akTop, akRight];

    FlblResultSelLeft.Color := clGreen;
    FlblResultSelTop.Color := clGreen;
    FlblResultSelRight.Color := clMaroon;
    FlblResultSelBottom.Color := clMaroon;

    FlblResultSelLeft.Visible := True;
    FlblResultSelTop.Visible := True;
    FlblResultSelRight.Visible := True;
    FlblResultSelBottom.Visible := True;

    FlblResultSelLeft.BringToFront;
    FlblResultSelTop.BringToFront;
    FlblResultSelRight.BringToFront;
    FlblResultSelBottom.BringToFront;

    FlblResultSelLeft.OnMouseEnter := FlblResultSelVertMouseEnter;
    FlblResultSelTop.OnMouseEnter := FlblResultSelHorizMouseEnter;
    FlblResultSelRight.OnMouseEnter := FlblResultSelVertMouseEnter;
    FlblResultSelBottom.OnMouseEnter := FlblResultSelHorizMouseEnter;

    FlblResultSelLeft.OnMouseLeave := FlblResultSelVertMouseLeave;
    FlblResultSelTop.OnMouseLeave := FlblResultSelHorizMouseLeave;
    FlblResultSelRight.OnMouseLeave := FlblResultSelVertMouseLeave;
    FlblResultSelBottom.OnMouseLeave := FlblResultSelHorizMouseLeave;

    FlblResultSelLeft.OnMouseMove := FlblResultSelVertMouseMove;
    FlblResultSelTop.OnMouseMove := FlblResultSelHorizMouseMove;
    FlblResultSelRight.OnMouseMove := FlblResultSelVertMouseMove;
    FlblResultSelBottom.OnMouseMove := FlblResultSelHorizMouseMove;
  end;
end;


function TfrClickerActions.GetSubVarByIndex(AMainVarValue: string; ASubVarIndex: Integer): string;
var
  SubVars: TStringList;
begin
  SubVars := TStringList.Create;
  try
    SubVars.LineBreak := #13#10;
    SubVars.Text := FastReplace_45ToReturn(AMainVarValue);

    if ASubVarIndex > SubVars.Count - 1 then
    begin
      Result := '10'; //a valid value, to set the label there
      Exit;
    end;

    Result := SubVars.Strings[ASubVarIndex];
  finally
    SubVars.Free;
  end;
end;


procedure TfrClickerActions.GetDecodedVarDetails(AParentVarName: string; ASubVarIndex: Integer; out X, Y, W, H: Integer);
var
  XVarIdx, YVarIdx, WVarIdx, HVarIdx: Integer;
  XVarValue, YVarValue, WVarValue, HVarValue: string;
begin
  X := -1;
  Y := -1;
  W := -1;
  H := -1;
  XVarIdx := -1;
  YVarIdx := -1;
  WVarIdx := -1;
  HVarIdx := -1;

  if (AParentVarName = '$AllControl_XOffsets$') or
     (AParentVarName = '$AllControl_YOffsets$') or
     (AParentVarName = '$AllControl_Lefts$') or
     (AParentVarName = '$AllControl_Tops$') or
     (AParentVarName = '$AllControl_Rights$') or
     (AParentVarName = '$AllControl_Bottoms$') or
     (AParentVarName = '$AllControl_Widths$') or
     (AParentVarName = '$AllControl_Heights$') then
  begin
    XVarIdx := FClkVariables.IndexOfName('$AllControl_XOffsets$');  //use XOffsets, because the value is relative to parent control
    YVarIdx := FClkVariables.IndexOfName('$AllControl_YOffsets$');  //use YOffsets, because the value is relative to parent control
    WVarIdx := FClkVariables.IndexOfName('$AllControl_Widths$');
    HVarIdx := FClkVariables.IndexOfName('$AllControl_Heights$');
  end;

  if (AParentVarName = '$DecodedWindows_XOffset$') or
     (AParentVarName = '$DecodedWindows_YOffset$') or
     (AParentVarName = '$DecodedWindows_Control_Lefts$') or
     (AParentVarName = '$DecodedWindows_Control_Tops$') or
     (AParentVarName = '$DecodedWindows_Control_Rights$') or
     (AParentVarName = '$DecodedWindows_Control_Bottoms$') or
     (AParentVarName = '$DecodedWindows_Control_Width$') or
     (AParentVarName = '$DecodedWindows_Control_Height$') then
  begin
    XVarIdx := FClkVariables.IndexOfName('$DecodedWindows_XOffset$');  //use XOffsets, because the value is relative to parent control
    YVarIdx := FClkVariables.IndexOfName('$DecodedWindows_YOffset$');  //use YOffsets, because the value is relative to parent control
    WVarIdx := FClkVariables.IndexOfName('$DecodedWindows_Control_Width$');
    HVarIdx := FClkVariables.IndexOfName('$DecodedWindows_Control_Height$');
  end;

  if (AParentVarName = '$DecodedWindows_XOffset_WE$') or
     (AParentVarName = '$DecodedWindows_YOffset_WE$') or
     (AParentVarName = '$DecodedWindows_Control_Lefts_WE$') or
     (AParentVarName = '$DecodedWindows_Control_Tops_WE$') or
     (AParentVarName = '$DecodedWindows_Control_Rights_WE$') or
     (AParentVarName = '$DecodedWindows_Control_Bottoms_WE$') or
     (AParentVarName = '$DecodedWindows_Control_Width_WE$') or
     (AParentVarName = '$DecodedWindows_Control_Height_WE$') then
  begin
    XVarIdx := FClkVariables.IndexOfName('$DecodedWindows_XOffset_WE$');  //use XOffsets, because the value is relative to parent control
    YVarIdx := FClkVariables.IndexOfName('$DecodedWindows_YOffset_WE$');  //use YOffsets, because the value is relative to parent control
    WVarIdx := FClkVariables.IndexOfName('$DecodedWindows_Control_Width_WE$');
    HVarIdx := FClkVariables.IndexOfName('$DecodedWindows_Control_Height_WE$');
  end;

  XVarValue := '';
  YVarValue := '';
  WVarValue := '';
  HVarValue := '';

  if XVarIdx <> - 1 then
    XVarValue := GetSubVarByIndex(FClkVariables.ValueFromIndex[XVarIdx], ASubVarIndex);

  if YVarIdx <> - 1 then
    YVarValue := GetSubVarByIndex(FClkVariables.ValueFromIndex[YVarIdx], ASubVarIndex);

  if WVarIdx <> - 1 then
    WVarValue := GetSubVarByIndex(FClkVariables.ValueFromIndex[WVarIdx], ASubVarIndex);

  if HVarIdx <> - 1 then
    HVarValue := GetSubVarByIndex(FClkVariables.ValueFromIndex[HVarIdx], ASubVarIndex);

  X := StrToIntDef(XVarValue, 4);
  Y := StrToIntDef(YVarValue, 4);
  W := StrToIntDef(WVarValue, 4);
  H := StrToIntDef(HVarValue, 4);
end;


procedure TfrClickerActions.SelectAreaFromDecodedVariable(ANodeData: PVarNodeRec; ASubVarIndex: Integer);
var
  ParentVarName: string;
  X, Y, W, H: Integer;
begin
  CreateSelectionLabelsForResult;

  ParentVarName := Copy(ANodeData^.VarName, 1, Pos('$[', ANodeData^.VarName));

  GetDecodedVarDetails(ParentVarName, ASubVarIndex, X, Y, W, H);
  FlblResultSelLeft.Left := X;
  FlblResultSelTop.Top := Y;
  FlblResultSelRight.Left := X + W;
  FlblResultSelBottom.Top := Y + H;
end;


procedure TfrClickerActions.scrboxDebugBmpMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  Factor: Integer;
begin
  if ssCtrl in Shift then
    Factor := 1
  else
    Factor := 3;

  if ssShift in Shift then
    scrboxDebugBmp.HorzScrollBar.Position := scrboxDebugBmp.HorzScrollBar.Position - WheelDelta div Factor
  else
    scrboxDebugBmp.VertScrollBar.Position := scrboxDebugBmp.VertScrollBar.Position - WheelDelta div Factor;

  Handled := True;
end;


procedure TfrClickerActions.UpdateControlWidthHeightLabels;
begin
  frClickerFindControl.UpdateControlWidthHeightLabels;
end;


procedure TfrClickerActions.UpdateUseWholeScreenLabel(AUseWholeScreen: Boolean);
begin
  frClickerFindControl.UpdateUseWholeScreenLabel(AUseWholeScreen);
end;


procedure TfrClickerActions.spdbtnDisplaySearchAreaDbgImgMenuClick(Sender: TObject);
var
  tp: TPoint;
begin
  GetCursorPos(tp);
  FSearchAreaDbgImgSearchedBmpMenu.PopUp(tp.X, tp.Y);
end;


procedure TfrClickerActions.tmrClkVariablesTimer(Sender: TObject);
begin
  tmrClkVariables.Enabled := False;
  vstVariables.RootNodeCount := FClkVariables.Count;
  vstVariables.Repaint;
end;


procedure TfrClickerActions.lbeFindCachedControlLeftChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeFindCachedControlTopChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.MenuItem_ReplaceWithAppDirClick(Sender: TObject);
var
  PathToFileName: string;
begin
  try
    if Assigned(FLastClickedTVTEdit) then
      PathToFileName := FLastClickedTVTEdit.Text
    else
      if Assigned(FLastClickedEdit) then
        PathToFileName := FLastClickedEdit.Text
      else
        PathToFileName := '';

    if ExtractFileDrive(ParamStr(0)) = ExtractFileDrive(PathToFileName) then
      PathToFileName := '$AppDir$' + PathDelim + ExtractRelativePath(ExtractFilePath(ParamStr(0)), PathToFileName);

    if Assigned(FLastClickedTVTEdit) then
    begin
      FLastClickedTVTEdit.Text := PathToFileName; //StringReplace(FLastClickedTVTEdit.Text, ExtractFileDir(ParamStr(0)), '$AppDir$', [rfReplaceAll]);
      FOIFrame.EditingText := FLastClickedTVTEdit.Text;
    end;

    if Assigned(FLastClickedEdit) then
    begin
      FLastClickedEdit.Text := PathToFileName; //StringReplace(FLastClickedEdit.Text, ExtractFileDir(ParamStr(0)), '$AppDir$', [rfReplaceAll]);
      if Assigned(FLastClickedEdit.OnChange) then
        FLastClickedEdit.OnChange(FLastClickedEdit);
    end;
  except
    on E: Exception do
      MessageBox(Handle, PChar('EditBox is not available.' + #13#10 + E.Message), PChar(Application.MainForm.Caption), MB_ICONERROR);
  end;
end;


procedure TfrClickerActions.MenuItem_ReplaceWithTemplateDirClick(Sender: TObject);
var
  PathToFileName: string;
begin
  try
    if Assigned(FLastClickedTVTEdit) then
      PathToFileName := FLastClickedTVTEdit.Text
    else
      if Assigned(FLastClickedEdit) then
        PathToFileName := FLastClickedEdit.Text
      else
        PathToFileName := '';

    PathToFileName := StringReplace(PathToFileName, FFullTemplatesDir, '$TemplateDir$', [rfReplaceAll]);

    if Assigned(FLastClickedTVTEdit) then
    begin
      FLastClickedTVTEdit.Text := PathToFileName;
      FOIFrame.EditingText := FLastClickedTVTEdit.Text;
    end;

    if Assigned(FLastClickedEdit) then
    begin
      FLastClickedEdit.Text := PathToFileName;
      if Assigned(FLastClickedEdit.OnChange) then
        FLastClickedEdit.OnChange(FLastClickedEdit);
    end;
  except
    on E: Exception do
      MessageBox(Handle, PChar('EditBox is not available.' + #13#10 + E.Message), PChar(Application.MainForm.Caption), MB_ICONERROR);
  end;
end;


procedure TfrClickerActions.MenuItem_ReplaceWithSelfTemplateDirClick(
  Sender: TObject);
var
  PathToFileName: string;
begin
  try
    if Assigned(FLastClickedTVTEdit) then
      PathToFileName := FLastClickedTVTEdit.Text
    else
      if Assigned(FLastClickedEdit) then
        PathToFileName := FLastClickedEdit.Text
      else
        PathToFileName := '';
                                                                    //path to template
    PathToFileName := StringReplace(PathToFileName, ExtractFileDir(DoOnGetLoadedTemplateFileName), '$SelfTemplateDir$', [rfReplaceAll]);

    if Assigned(FLastClickedTVTEdit) then
    begin
      FLastClickedTVTEdit.Text := PathToFileName;
      FOIFrame.EditingText := FLastClickedTVTEdit.Text;
    end;

    if Assigned(FLastClickedEdit) then
    begin
      FLastClickedEdit.Text := PathToFileName;
      if Assigned(FLastClickedEdit.OnChange) then
        FLastClickedEdit.OnChange(FLastClickedEdit);
    end;
  except
    on E: Exception do
      MessageBox(Handle, PChar('EditBox is not available.' + #13#10 + E.Message), PChar(Application.MainForm.Caption), MB_ICONERROR);
  end;
end;


procedure TfrClickerActions.MenuItem_SetFromControlLeftAndTopClick(
  Sender: TObject);
begin
  FOIFrame.CancelCurrentEditing;
  GetEditingActionObjectByActionType^.WindowOperationsOptions.NewX := EvaluateReplacements('$Control_Left$');
  GetEditingActionObjectByActionType^.WindowOperationsOptions.NewY := EvaluateReplacements('$Control_Top$');
  FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_ActionSpecific, CWindowOperations_NewX_PropItemIndex, -1);
  FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_ActionSpecific, CWindowOperations_NewY_PropItemIndex, -1);
end;


procedure TfrClickerActions.MenuItem_SetFromControlWidthAndHeightClick(
  Sender: TObject);
begin
  FOIFrame.CancelCurrentEditing;
  GetEditingActionObjectByActionType^.WindowOperationsOptions.NewWidth := EvaluateReplacements('$Control_Width$');
  GetEditingActionObjectByActionType^.WindowOperationsOptions.NewHeight := EvaluateReplacements('$Control_Height$');
  FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_ActionSpecific, CWindowOperations_NewWidth_PropItemIndex, -1);
  FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_ActionSpecific, CWindowOperations_NewHeight_PropItemIndex, -1);
end;


procedure TfrClickerActions.pmStandardColorVariablesPopup(Sender: TObject);
var
  i: Integer;
  s, evs: string;
  TextColor: TColor;
begin
  for i := 0 to pmStandardColorVariables.Items.Count - 1 do
    if Pos('$', pmStandardColorVariables.Items.Items[i].Caption) > 0 then
    begin
      if pmStandardColorVariables.Items.Items[i].Bitmap <> nil then
        pmStandardColorVariables.Items.Items[i].Bitmap.Free;

      pmStandardColorVariables.Items.Items[i].Bitmap := TBitmap.Create;
      pmStandardColorVariables.Items.Items[i].Bitmap.Width := 16;
      pmStandardColorVariables.Items.Items[i].Bitmap.Height := 16;

      s := pmStandardColorVariables.Items.Items[i].Caption;
      Delete(s, 1, 1); //remove first '$', so that Pos returns the next one
      s := '$' + Copy(s, 1, Pos('$', s));
      evs := EvaluateReplacements(s);
      TextColor := HexToInt(evs);

      pmStandardColorVariables.Items.Items[i].Caption := s + '   (' + evs + ')';

      pmStandardColorVariables.Items.Items[i].Bitmap.Canvas.Pen.Color := 1;  // > 0
      pmStandardColorVariables.Items.Items[i].Bitmap.Canvas.Brush.Color := TextColor;
      pmStandardColorVariables.Items.Items[i].Bitmap.Canvas.Rectangle(0, 0, 16, 16);
    end;
end;


procedure TfrClickerActions.FrameResize(Sender: TObject);
var
  NewLeft: Integer;
begin                                   //this method doesn't seem to be called before showing the owner window/frame
  NewLeft := pnlHorizSplitter.Left;     //that is why Width has its initial (small) value, causing NewLeft to adapt to it

  if NewLeft > Width - 260 then
    NewLeft := Width - 260;

  ResizeFrameSectionsBySplitter(NewLeft);

  NewLeft := pnlHorizSplitterResults.Left;
  if NewLeft > Width - 260 then
    NewLeft := Width - 260;

  ResizeFrameSectionsBySplitterResults(NewLeft);
end;


procedure TfrClickerActions.ResizeFrameSectionsBySplitter(NewLeft: Integer);
begin
  if NewLeft < pnlvstOI.Constraints.MinWidth then
    NewLeft := pnlvstOI.Constraints.MinWidth;

  if NewLeft > Width - 260 then
    NewLeft := Width - 260;

  pnlHorizSplitter.Left := NewLeft;

  pnlExtra.Left := pnlHorizSplitter.Left + pnlHorizSplitter.Width;
  pnlExtra.Width := TabSheetAction.Width - pnlExtra.Left;
  pnlvstOI.Width := pnlHorizSplitter.Left;
  pnlvstOI.Left := 0;

  //Code which should not be here, but it is required on larger OS font settings.
  //There are race-conditions or other issues, which prevent setting the width/height of various components, before/after setting anchors.
  FOIFrame.Left := 0;
  FOIFrame.Top := 0;
  FOIFrame.Width := pnlvstOI.Width;
  FOIFrame.Height := pnlvstOI.Height;
  FOIFrame.SetVSTOISize; //A small improvement by calling SetVSTOISize here, not just in its FrameResize handler.
end;


procedure TfrClickerActions.ResizeFrameSectionsBySplitterResults(NewLeft: Integer);
begin
  if NewLeft < pnlVars.Constraints.MinWidth then
    NewLeft := pnlVars.Constraints.MinWidth;

  if NewLeft > Width - 260 then
    NewLeft := Width - 260;

  pnlHorizSplitterResults.Left := NewLeft;

  pnlResults.Left := pnlHorizSplitterResults.Left + pnlHorizSplitterResults.Width;
  pnlResults.Width := TabSheetDebugging.Width - pnlResults.Left;
  pnlVars.Width := pnlHorizSplitterResults.Left;
end;


procedure TfrClickerActions.BringOIPropertyIntoView(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer);
begin
  FOIFrame.ScrollToNode(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex);
end;


function TfrClickerActions.GetIndexOfFirstModifiedPmtvFile(AEditingAction: PClkActionRec): Integer;
var
  PrimitiveFile_Modified: TStringList;
begin
  PrimitiveFile_Modified := TStringList.Create;
  try
    PrimitiveFile_Modified.LineBreak := #13#10;
    PrimitiveFile_Modified.Text := AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified;
    Result := PrimitiveFile_Modified.IndexOf('1');
  finally
    PrimitiveFile_Modified.Free;
  end;
end;


procedure TfrClickerActions.pnlHorizSplitterMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Shift <> [ssLeft] then
    Exit;

  if not FHold then
  begin
    GetCursorPos(FSplitterMouseDownGlobalPos);

    FSplitterMouseDownImagePos.X := pnlHorizSplitter.Left;
    FHold := True;
  end;
end;


procedure TfrClickerActions.pnlHorizSplitterMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  tp: TPoint;
  NewLeft: Integer;
begin
  if Shift <> [ssLeft] then
    Exit;

  if not FHold then
    Exit;

  GetCursorPos(tp);
  NewLeft := FSplitterMouseDownImagePos.X + tp.X - FSplitterMouseDownGlobalPos.X;

  ResizeFrameSectionsBySplitter(NewLeft);
end;


procedure TfrClickerActions.pnlHorizSplitterMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FHold := False;
end;


procedure TfrClickerActions.pnlHorizSplitterResultsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Shift <> [ssLeft] then
    Exit;

  if not FHoldResults then
  begin
    GetCursorPos(FSplitterMouseDownGlobalPos);

    FSplitterMouseDownImagePos.X := pnlHorizSplitterResults.Left;
    FHoldResults := True;
  end;
end;


procedure TfrClickerActions.pnlHorizSplitterResultsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  tp: TPoint;
  NewLeft: Integer;
begin
  if Shift <> [ssLeft] then
    Exit;

  if not FHoldResults then
    Exit;

  GetCursorPos(tp);
  NewLeft := FSplitterMouseDownImagePos.X + tp.X - FSplitterMouseDownGlobalPos.X;

  ResizeFrameSectionsBySplitterResults(NewLeft);
end;


procedure TfrClickerActions.pnlHorizSplitterResultsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FHoldResults := False;
end;


procedure TfrClickerActions.vallstVariablesValidate(Sender: TObject; ACol,
  ARow: Integer; const KeyName, KeyValue: string);
begin
  if ACol = 0 then
  begin
    if KeyName > '' then
      if {(Length(KeyName) < 2) or} (KeyName[1] <> '$') or (KeyName[Length(KeyName)] <> '$') then
        raise Exception.Create('Variable name must be enclosed by two "$" characters. E.g. "$my_var$" (without double quotes).');

    if ARow - 1 < FPredefinedVarCount then
      raise Exception.Create('Predefined variables should not be edited. Press Esc to revert.');
  end;
end;


const
  CNoTemplatesMsg = 'No local templates available.';
  CNoSetVarActionsMsg = 'No SetVar actions available.';
  CNoActionsMsg = 'No actions available.';

procedure TfrClickerActions.LocalTemplatesClick(Sender: TObject);
var
  Fnm: string;
begin
  Fnm := StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]);

  if Fnm = CNoTemplatesMsg then
  begin
    MessageBox(Handle, 'There are no templates in the local directory, ActionTemplates.', PChar(Application.Title), MB_ICONINFORMATION);
    Exit;
  end;

  GetEditingActionObjectByActionType^.CallTemplateOptions.TemplateFileName := Fnm;
  FOIFrame.CancelCurrentEditing;
  FOIFrame.Repaint;   //ideally, RepaintNodeByLevel
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.AvailableSetVarClick(Sender: TObject);
var
  SetVarName: string;
begin
  SetVarName := StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]);

  if SetVarName = CNoSetVarActionsMsg then
  begin
    MessageBox(Handle, 'There are no available SetVar actions.', PChar(Application.Title), MB_ICONINFORMATION);
    Exit;
  end;

  if (CurrentlyEditingActionType = acLoadSetVarFromFile) or ((CurrentlyEditingActionType = acEditTemplate) and (FEditingAction^.EditTemplateOptions.EditedActionType = acLoadSetVarFromFile)) then
    GetEditingActionObjectByActionType^.LoadSetVarFromFileOptions.SetVarActionName := SetVarName;

  if (CurrentlyEditingActionType = acSaveSetVarToFile) or ((CurrentlyEditingActionType = acEditTemplate) and (FEditingAction^.EditTemplateOptions.EditedActionType = acSaveSetVarToFile)) then
    GetEditingActionObjectByActionType^.SaveSetVarToFileOptions.SetVarActionName := SetVarName;

  FOIFrame.CancelCurrentEditing;
  FOIFrame.Repaint;   //ideally, RepaintNodeByLevel
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.AvailablePluginPropertiesClick(Sender: TObject);
var
  ActionName: string;
  ListOfProperties: TStringList;
  PropertyIndex: Integer;
begin
  ActionName := StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]);
  PropertyIndex := (Sender as TMenuItem).Tag;

  ListOfProperties := TStringList.Create;
  try
    ListOfProperties.LineBreak := #13#10;
    ListOfProperties.Text := GetEditingActionObjectByActionType^.PluginOptions.ListOfPropertiesAndValues;

    //if ActionName <> '' then  //verification required, otherwise the item is deleted from list
      //ListOfProperties.ValueFromIndex[PropertyIndex] := ActionName;      //see next line

    ListOfProperties.Strings[PropertyIndex] := ListOfProperties.Names[PropertyIndex] + '=' + ActionName; //this assignment supports empty string, without deleting the item as done by ValueFromIndex

    GetEditingActionObjectByActionType^.PluginOptions.ListOfPropertiesAndValues := ListOfProperties.Text;
  finally
    ListOfProperties.Free;
  end;

  FOIFrame.CancelCurrentEditing;
  FOIFrame.Repaint;   //ideally, RepaintNodeByLevel
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.AvailableEditingActionPropertiesClick_Cat_ActionSpecific(Sender: TObject);
var
  ActionName: string;
  ActionType: TClkAction;
  OldType: TClkAction;
  ActionTypeInt: Integer;
  PropertyIndex: Integer;
begin
  ActionName := StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]);
  ActionTypeInt := Max(Min((Sender as TMenuItem).Tag shr 16, Ord(High(TClkAction))), Ord(Low(TClkAction)));
  ActionType := TClkAction(ActionTypeInt);
  PropertyIndex := (Sender as TMenuItem).Tag and $FFFF;

  OldType := FEditingAction^.EditTemplateOptions.EditedActionType;

  case PropertyIndex of
    CEditTemplate_EditedActionName_PropIndex:
    begin
      //GetEditingActionObjectByActionType^.EditTemplateOptions.EditedActionName := ActionName; //editing action
      FEditingAction^.EditTemplateOptions.EditedActionName := ActionName;                       //action specific
      FEditingAction^.EditTemplateOptions.EditedActionType := ActionType;                       //type
    end;

    CEditTemplate_NewActionName_PropIndex:
    begin
      //GetEditingActionObjectByActionType^.EditTemplateOptions.NewActionName := ActionName; //editing action
      FEditingAction^.EditTemplateOptions.NewActionName := ActionName;                       //action specific
    end;

    else
      ;
  end;

  FOIFrame.CancelCurrentEditing;

  if ActionType <> OldType then   //updating the OI is not commented, as it is in AvailableEditingActionPropertiesClick_Cat_EditingAction
  begin
    FClkEditedActionByEditTemplate.ActionOptions.Action := ActionType;

    if ActionType = acFindSubControl then
    begin
      if Length(FClkEditedActionByEditTemplate.FindSubControlOptions.MatchBitmapText) = 0 then
        GetDefaultPropertyValues_FindSubControl(FClkEditedActionByEditTemplate.FindSubControlOptions);

      if frClickerFindControl.GetBMPTextFontProfilesCount <> Length(FEditTemplateOptions_EditingAction^.FindSubControlOptions.MatchBitmapText) then
        DeserializeEditTemplateEditingAction; //this should be replaced by something which copies the action content from FindSubControl
    end;

    SerializeEditTemplateEditingAction;
    tmrOnChangeEditTemplateEditingActionType.Enabled := True;
  end;

  FOIFrame.Repaint;   //ideally, RepaintNodeByLevel
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.AvailableEditingActionPropertiesClick_Cat_EditingAction(Sender: TObject);
var
  ActionName: string;
  ActionType: TClkAction;
  //OldType: TClkAction;
  ActionTypeInt: Integer;
  PropertyIndex: Integer;
begin
  ActionName := StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]);
  ActionTypeInt := Max(Min((Sender as TMenuItem).Tag shr 16, Ord(High(TClkAction))), Ord(Low(TClkAction)));
  ActionType := TClkAction(ActionTypeInt);
  PropertyIndex := (Sender as TMenuItem).Tag and $FFFF;

  case PropertyIndex of
    CEditTemplate_EditedActionName_PropIndex:
    begin
      GetEditingActionObjectByActionType^.EditTemplateOptions.EditedActionName := ActionName;  //editing action
      //FEditingAction^.EditTemplateOptions.EditedActionName := ActionName;                    //action specific

      //OldType := GetEditingActionObjectByActionType^.EditTemplateOptions.EditedActionType;
      GetEditingActionObjectByActionType^.EditTemplateOptions.EditedActionType := ActionType;  //type
    end;

    CEditTemplate_NewActionName_PropIndex:
    begin
      GetEditingActionObjectByActionType^.EditTemplateOptions.NewActionName := ActionName;  //editing action
      //FEditingAction^.EditTemplateOptions.NewActionName := ActionName;                    //action specific
    end;

    else
      ;
  end;

  FOIFrame.CancelCurrentEditing;

  //if ActionType <> OldType then
  //begin
  //  SerializeEditTemplateEditingAction;
  //  tmrOnChangeEditTemplateEditingActionType.Enabled := True;
  //end;

  FOIFrame.Repaint;   //ideally, RepaintNodeByLevel
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.AvailableFindSubControlSendRequestActionClick(Sender: TObject);
var
  NewActionName: string;
begin
  NewActionName := StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]);

  if NewActionName = CNoActionsMsg then
  begin
    MessageBox(Handle, 'There are no available actions.', PChar(Application.Title), MB_ICONINFORMATION);
    Exit;
  end;

  GetEditingActionObjectByActionType^.FindSubControlOptions.RenderingInBrowserSettings.ActionForSendingRequest := NewActionName;

  FOIFrame.CancelCurrentEditing;
  FOIFrame.Repaint;   //ideally, RepaintNodeByLevel
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.AvailableFindSubControlRcvBmpPluginActionClick(Sender: TObject);
var
  NewActionName: string;
begin
  NewActionName := StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]);

  if NewActionName = CNoActionsMsg then
  begin
    MessageBox(Handle, 'There are no available actions.', PChar(Application.Title), MB_ICONINFORMATION);
    Exit;
  end;

  GetEditingActionObjectByActionType^.FindSubControlOptions.RenderingInBrowserSettings.PluginActionForReceivingBitmaps := NewActionName;

  FOIFrame.CancelCurrentEditing;
  FOIFrame.Repaint;   //ideally, RepaintNodeByLevel
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.BrowseTemplatesClick(Sender: TObject);
begin
  DoOnSetOpenDialogInitialDir(FFullTemplatesDir);    //this is not the right dir
  if DoOnOpenDialogExecute(CTemplateDialogFilter) then
  begin
    GetEditingActionObjectByActionType^.CallTemplateOptions.TemplateFileName := DoOnGetOpenDialogFileName;
    FOIFrame.CancelCurrentEditing;
    FOIFrame.Repaint;   //ideally, RepaintNodeByLevel
    TriggerOnControlsModified;
  end;
end;


procedure TfrClickerActions.LoadListOfAvailableTemplates;
var
  AvailableTemplates: TStringList;
  ASearchRec: TSearchRec;
  SearchResult: Integer;
  Dir: string;
  TempMenuItem, BaseMenuItem: TMenuItem;
  i: Integer;
begin
  AvailableTemplates := TStringList.Create;
  try
    AvailableTemplates.LineBreak := #13#10;
    Dir := FFullTemplatesDir + '\*.clktmpl';

    SearchResult := FindFirst(Dir, faArchive, ASearchRec);
    try
      while SearchResult = 0 do
      begin
        AvailableTemplates.Add(ASearchRec.Name);
        SearchResult := FindNext(ASearchRec);
      end;
    finally
      FindClose(ASearchRec);
    end;

    FPmLocalTemplates.Items.Clear;

    if AvailableTemplates.Count = 0 then
      AvailableTemplates.Add(CNoTemplatesMsg);

    BaseMenuItem := TMenuItem.Create(Self);
    BaseMenuItem.Caption := 'Browse template...';
    BaseMenuItem.OnClick := BrowseTemplatesClick;
    FPmLocalTemplates.Items.Add(BaseMenuItem);

    BaseMenuItem := TMenuItem.Create(Self);
    BaseMenuItem.Caption := 'Local templates';
    BaseMenuItem.OnClick := nil;
    FPmLocalTemplates.Items.Add(BaseMenuItem);

    for i := 0 to AvailableTemplates.Count - 1 do
    begin
      TempMenuItem := TMenuItem.Create(Self);
      TempMenuItem.Caption := AvailableTemplates.Strings[i];
      TempMenuItem.OnClick := LocalTemplatesClick;
      FPmLocalTemplates.Items[1].Add(TempMenuItem);
    end;
  finally
    AvailableTemplates.Free;
  end;
end;


procedure TfrClickerActions.LoadListOfAvailableSetVarActions(AEditingAction: PClkActionRec);
var
  AvailableSetVarActions: TStringList;
  TempMenuItem, BaseMenuItem: TMenuItem;
  i: Integer;
  Bmp: TBitmap;
  Ini: TClkIniReadonlyFile;
  LocalClkActions: TClkActionsRecArr;
  Notes, IconPath: string;
begin
  AvailableSetVarActions := TStringList.Create;
  try
    AvailableSetVarActions.LineBreak := #13#10;
    if AEditingAction^.EditTemplateOptions.WhichTemplate = etwtSelf then
      DoOnGetListOfAvailableSetVarActions(AvailableSetVarActions)
    else
    begin
      if DoOnFileExists(AEditingAction^.EditTemplateOptions.TemplateFileName) then
      begin
        Ini := DoOnTClkIniReadonlyFileCreate(AEditingAction^.EditTemplateOptions.TemplateFileName);  //LoadTemplate
        try
          LoadTemplateToCustomActions_V2(Ini, LocalClkActions, Notes, IconPath);
          for i := 0 to Length(LocalClkActions) - 1 do
            if LocalClkActions[i].ActionOptions.Action = acSetVar then
              AvailableSetVarActions.Add(LocalClkActions[i].ActionOptions.ActionName);
        finally
          Ini.Free;
        end;
      end;
    end;

    FPmLocalTemplates.Items.Clear;

    if AvailableSetVarActions.Count = 0 then
      AvailableSetVarActions.Add(CNoSetVarActionsMsg);

    BaseMenuItem := TMenuItem.Create(Self);
    BaseMenuItem.Caption := 'Available SetVar actions';
    BaseMenuItem.OnClick := nil;
    FPmLocalTemplates.Items.Add(BaseMenuItem);

    for i := 0 to AvailableSetVarActions.Count - 1 do
    begin
      TempMenuItem := TMenuItem.Create(Self);
      TempMenuItem.Caption := AvailableSetVarActions.Strings[i];
      TempMenuItem.OnClick := AvailableSetVarClick;

      Bmp := TBitmap.Create;
      Bmp.PixelFormat := pf24bit;
      Bmp.Width := 16;
      Bmp.Height := 16;
      Bmp.Canvas.Pen.Color := clWhite;
      Bmp.Canvas.Brush.Color := clWhite;
      Bmp.Canvas.Rectangle(0, 0, 16, 16);
      dmClickerIcons.imglstActions16.Draw(bmp.Canvas, 0, 0, Integer(TClkAction(acSetVar)), dsNormal, itImage);
      TempMenuItem.Bitmap := Bmp;

      FPmLocalTemplates.Items[0].Add(TempMenuItem);
    end;
  finally
    AvailableSetVarActions.Free;
  end;
end;


procedure TfrClickerActions.LoadListOfAvailableActionsForPlugin(APropertyIndexToUpdate: Integer; AEditingAction: PClkActionRec);
var
  AvailableActions: TStringList;
  TempMenuItem, BaseMenuItem: TMenuItem;
  i: Integer;
  Bmp: TBitmap;
  ActionStr: string;
  ActionType: Integer;
  Ini: TClkIniReadonlyFile;
  LocalClkActions: TClkActionsRecArr;
  Notes, IconPath: string;
begin
  AvailableActions := TStringList.Create;
  try
    AvailableActions.LineBreak := #13#10;
    if AEditingAction.ActionOptions.Action = acPlugin then
      DoOnGetListOfAvailableActions(AvailableActions)
    else
      if AEditingAction.ActionOptions.Action = acEditTemplate then
        if AEditingAction^.EditTemplateOptions.WhichTemplate = etwtSelf then
          DoOnGetListOfAvailableActions(AvailableActions)
        else
        begin
          if DoOnFileExists(AEditingAction^.EditTemplateOptions.TemplateFileName) then
          begin
            Ini := DoOnTClkIniReadonlyFileCreate(AEditingAction^.EditTemplateOptions.TemplateFileName);  //LoadTemplate
            try
              LoadTemplateToCustomActions_V2(Ini, LocalClkActions, Notes, IconPath);
              for i := 0 to Length(LocalClkActions) - 1 do
                AvailableActions.Add(LocalClkActions[i].ActionOptions.ActionName + #4#5 + IntToStr(Ord(LocalClkActions[i].ActionOptions.Action)));
            finally
              Ini.Free;
            end;
          end;
        end;

    FPmLocalTemplates.Items.Clear;

    if AvailableActions.Count = 0 then
      AvailableActions.Add(CNoSetVarActionsMsg);

    BaseMenuItem := TMenuItem.Create(Self);
    BaseMenuItem.Caption := 'Available actions';
    BaseMenuItem.OnClick := nil;
    FPmLocalTemplates.Items.Add(BaseMenuItem);

    for i := 0 to AvailableActions.Count - 1 do
    begin
      ActionStr := AvailableActions.Strings[i];
      ActionType := StrToIntDef(Copy(ActionStr, Pos(#4#5, ActionStr) + 2, MaxInt), 0);

      TempMenuItem := TMenuItem.Create(Self);
      TempMenuItem.Caption := Copy(ActionStr, 1, Pos(#4#5, ActionStr) - 1);
      TempMenuItem.OnClick := AvailablePluginPropertiesClick;
      TempMenuItem.Tag := APropertyIndexToUpdate;

      Bmp := TBitmap.Create;
      Bmp.PixelFormat := pf24bit;
      Bmp.Width := 16;
      Bmp.Height := 16;
      Bmp.Canvas.Pen.Color := clWhite;
      Bmp.Canvas.Brush.Color := clWhite;
      Bmp.Canvas.Rectangle(0, 0, 16, 16);
      dmClickerIcons.imglstActions16.Draw(bmp.Canvas, 0, 0, ActionType, dsNormal, itImage);
      TempMenuItem.Bitmap := Bmp;

      FPmLocalTemplates.Items[0].Add(TempMenuItem);
    end;
  finally
    AvailableActions.Free;
  end;
end;


procedure TfrClickerActions.LoadListOfAvailableActionsForEditTemplate(APropertyIndexToUpdate: Integer; AEditingAction: PClkActionRec);
var
  AvailableActions: TStringList;
  TempMenuItem, BaseMenuItem: TMenuItem;
  i: Integer;
  Bmp: TBitmap;
  ActionStr: string;
  ActionType: Integer;
  Ini: TClkIniReadonlyFile;
  LocalClkActions: TClkActionsRecArr;
  Notes, IconPath: string;
begin
  AvailableActions := TStringList.Create;
  try
    AvailableActions.LineBreak := #13#10;
    if AEditingAction^.EditTemplateOptions.WhichTemplate = etwtSelf then
      DoOnGetListOfAvailableActions(AvailableActions)
    else
    begin
      if DoOnFileExists(AEditingAction^.EditTemplateOptions.TemplateFileName) then
      begin
        Ini := DoOnTClkIniReadonlyFileCreate(AEditingAction^.EditTemplateOptions.TemplateFileName);  //LoadTemplate
        try
          LoadTemplateToCustomActions_V2(Ini, LocalClkActions, Notes, IconPath);
          for i := 0 to Length(LocalClkActions) - 1 do
            AvailableActions.Add(LocalClkActions[i].ActionOptions.ActionName + #4#5 + IntToStr(Ord(LocalClkActions[i].ActionOptions.Action)));
        finally
          Ini.Free;
        end;
      end;
    end;

    FPmLocalTemplates.Items.Clear;

    if AvailableActions.Count = 0 then
      AvailableActions.Add(CNoActionsMsg + #4#5 + IntToStr(CClkUnsetAction));

    BaseMenuItem := TMenuItem.Create(Self);
    BaseMenuItem.Caption := 'Available actions';
    BaseMenuItem.OnClick := nil;
    FPmLocalTemplates.Items.Add(BaseMenuItem);

    for i := 0 to AvailableActions.Count - 1 do
    begin
      ActionStr := AvailableActions.Strings[i];
      ActionType := StrToIntDef(Copy(ActionStr, Pos(#4#5, ActionStr) + 2, MaxInt), 0);

      TempMenuItem := TMenuItem.Create(Self);
      TempMenuItem.Caption := Copy(ActionStr, 1, Pos(#4#5, ActionStr) - 1);

      if AEditingAction = FEditingAction then
        TempMenuItem.OnClick := AvailableEditingActionPropertiesClick_Cat_ActionSpecific
      else
        TempMenuItem.OnClick := AvailableEditingActionPropertiesClick_Cat_EditingAction;

      TempMenuItem.Tag := APropertyIndexToUpdate or (ActionType shl 16); //This limits the number of actions and the number of types to 65536.

      Bmp := TBitmap.Create;
      Bmp.PixelFormat := pf24bit;
      Bmp.Width := 16;
      Bmp.Height := 16;
      Bmp.Canvas.Pen.Color := clWhite;
      Bmp.Canvas.Brush.Color := clWhite;
      Bmp.Canvas.Rectangle(0, 0, 16, 16);
      dmClickerIcons.imglstActions16.Draw(bmp.Canvas, 0, 0, ActionType, dsNormal, itImage);
      TempMenuItem.Bitmap := Bmp;

      FPmLocalTemplates.Items[0].Add(TempMenuItem);
    end;
  finally
    AvailableActions.Free;
  end;
end;


procedure TfrClickerActions.LoadListOfAvailableActionsForFindSubControlRenderingRequest(APropertyIndexToUpdate: Integer; AEditingAction: PClkActionRec; AAllowedActionTypes: TClkActions; AHandler: TNotifyEvent);
var
  AvailableActions: TStringList;
  TempMenuItem, BaseMenuItem: TMenuItem;
  i: Integer;
  Bmp: TBitmap;
  ActionStr: string;
  ActionType: Integer;
  Ini: TClkIniReadonlyFile;
  LocalClkActions: TClkActionsRecArr;
  Notes, IconPath: string;
begin
  AvailableActions := TStringList.Create;
  try
    AvailableActions.LineBreak := #13#10;
    if AEditingAction.ActionOptions.Action = acFindSubControl then
      DoOnGetListOfAvailableActions(AvailableActions)
    else
      if AEditingAction.ActionOptions.Action = acEditTemplate then
        if AEditingAction^.EditTemplateOptions.WhichTemplate = etwtSelf then
          DoOnGetListOfAvailableActions(AvailableActions)
        else
        begin
          if DoOnFileExists(AEditingAction^.EditTemplateOptions.TemplateFileName) then
          begin
            Ini := DoOnTClkIniReadonlyFileCreate(AEditingAction^.EditTemplateOptions.TemplateFileName);  //LoadTemplate
            try
              LoadTemplateToCustomActions_V2(Ini, LocalClkActions, Notes, IconPath);
              for i := 0 to Length(LocalClkActions) - 1 do
                AvailableActions.Add(LocalClkActions[i].ActionOptions.ActionName + #4#5 + IntToStr(Ord(LocalClkActions[i].ActionOptions.Action)));
            finally
              Ini.Free;
            end;
          end;
        end;

    FPmLocalTemplates.Items.Clear;

    if AvailableActions.Count = 0 then
      AvailableActions.Add(CNoSetVarActionsMsg);

    BaseMenuItem := TMenuItem.Create(Self);
    BaseMenuItem.Caption := 'Available actions';
    BaseMenuItem.OnClick := nil;
    FPmLocalTemplates.Items.Add(BaseMenuItem);

    for i := 0 to AvailableActions.Count - 1 do
    begin
      ActionStr := AvailableActions.Strings[i];
      ActionType := StrToIntDef(Copy(ActionStr, Pos(#4#5, ActionStr) + 2, MaxInt), 0);

      if TClkAction(ActionType) in AAllowedActionTypes then
      begin
        TempMenuItem := TMenuItem.Create(Self);
        TempMenuItem.Caption := Copy(ActionStr, 1, Pos(#4#5, ActionStr) - 1);
        TempMenuItem.OnClick := AHandler;
        TempMenuItem.Tag := APropertyIndexToUpdate;

        Bmp := TBitmap.Create;
        Bmp.PixelFormat := pf24bit;
        Bmp.Width := 16;
        Bmp.Height := 16;
        Bmp.Canvas.Pen.Color := clWhite;
        Bmp.Canvas.Brush.Color := clWhite;
        Bmp.Canvas.Rectangle(0, 0, 16, 16);
        dmClickerIcons.imglstActions16.Draw(bmp.Canvas, 0, 0, ActionType, dsNormal, itImage);
        TempMenuItem.Bitmap := Bmp;

        FPmLocalTemplates.Items[0].Add(TempMenuItem);
      end;
    end;
  finally
    AvailableActions.Free;
  end;
end;


procedure TfrClickerActions.OverlapGridImgOnDebugImg(ADebugAndGridBitmap: TBitmap);
begin
  ADebugAndGridBitmap.Width := imgDebugBmp.Picture.Bitmap.Width;
  ADebugAndGridBitmap.Height := imgDebugBmp.Picture.Bitmap.Height;
  ADebugAndGridBitmap.Canvas.Draw(0, 0, imgDebugBmp.Picture.Bitmap);
  ADebugAndGridBitmap.Canvas.Draw(imgDebugGrid.Left, imgDebugGrid.Top, imgDebugGrid.Picture.Bitmap);
end;


procedure TfrClickerActions.MenuItemCopyDebugImageClick(Sender: TObject);
var
  DebugAndGridBitmap: TBitmap;
begin
  if (imgDebugBmp.Picture.Bitmap.Width = 0) and (imgDebugBmp.Picture.Bitmap.Height = 0) then
  begin
    MessageBox(Handle, 'Selected image is empty. Nothing to copy.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  if chkShowDebugGrid.Checked then
  begin
    DebugAndGridBitmap := TBitmap.Create;
    try
      OverlapGridImgOnDebugImg(DebugAndGridBitmap);
      Clipboard.Assign(DebugAndGridBitmap);
    finally
      DebugAndGridBitmap.Free;
    end;
  end
  else
    Clipboard.Assign(imgDebugBmp.Picture);
end;


procedure TfrClickerActions.MenuItemSaveDebugImageClick(Sender: TObject);
var
  ASaveDialog: TSaveDialog;
  DebugAndGridBitmap: TBitmap;
begin
  ASaveDialog := TSaveDialog.Create(nil);
  try
    ASaveDialog.Filter := 'Bitmap files (*.bmp)|*.bmp|All files (*.*)|*.*';
    ASaveDialog.InitialDir := FBMPsDir;
    if not ASaveDialog.Execute then
      Exit;

    if UpperCase(ExtractFileExt(ASaveDialog.FileName)) <> '.BMP' then
      ASaveDialog.FileName := ASaveDialog.FileName + '.bmp';

    if FileExists(ASaveDialog.FileName) then
      if MessageBox(Handle, 'File already exists. Replace?', PChar(Caption), MB_ICONWARNING + MB_YESNO) = IDNO then
        Exit;

    if chkShowDebugGrid.Checked then
    begin
      DebugAndGridBitmap := TBitmap.Create;
      try
        OverlapGridImgOnDebugImg(DebugAndGridBitmap);
        DebugAndGridBitmap.SaveToFile(ASaveDialog.FileName);
      finally
        DebugAndGridBitmap.Free;
      end;
    end
    else
      imgDebugBmp.Picture.Bitmap.SaveToFile(ASaveDialog.FileName);
          
    FBMPsDir := ExtractFileDir(ASaveDialog.FileName);
  finally
    ASaveDialog.Free;
  end;
end;


procedure TfrClickerActions.HandleOnUpdateBitmapAlgorithmSettings;
begin
  imgDebugGrid.Visible := chkShowDebugGrid.Checked;
end;


procedure TfrClickerActions.HandleOnTriggerOnControlsModified;
begin
  TriggerOnControlsModified;
  FOIFrame.RepaintOI;
end;


function TfrClickerActions.HandleOnEvaluateReplacements(s: string): string;
begin
  Result := EvaluateReplacements(s);
end;


function TfrClickerActions.HandleOnReverseEvaluateReplacements(s: string): string;
begin
  Result := ReverseEvaluateReplacements(s);
end;


procedure TfrClickerActions.AddVariable1Click(Sender: TObject);
begin
  FClkVariables.Add('');
end;


procedure TfrClickerActions.TriggerOnControlsModified(AExtraCondition: Boolean = True);
begin
  if not AExtraCondition then
    Exit;

  if Assigned(FOnControlsModified) then
  begin
    if not FControlsModified then  //to avoid calling the event every time
    begin
      FControlsModified := True;   //execute before the callback, because inside the callback, FControlsModified may be reset
      FOnControlsModified(Self);
    end;
  end;
end;


procedure TfrClickerActions.ClearControls;
begin
  frClickerConditionEditor.ClearActionConditionPreview;
  frClickerFindControl.ClearControls;

  //clear dynamically created mouse controls

  UpdatePageControlActionExecutionIcons;

  FEditingAction^.ActionOptions.Action := {%H-}TClkAction(CClkUnsetAction); //not set
  FOIFrame.ReloadContent;
end;


procedure TfrClickerActions.SerializeEditTemplateEditingAction;
begin
  FEditingAction^.EditTemplateOptions.ListOfEnabledProperties := FClkEditedActionByEditTemplate.EditTemplateOptions.ListOfEnabledProperties;
  FEditingAction^.EditTemplateOptions.ListOfEditedProperties := StringReplace(GetActionPropertiesByType(FClkEditedActionByEditTemplate), CPropSeparatorSer, CPropSeparatorInt, [rfReplaceAll]);

  //To be verified
  FEditingAction^.EditTemplateOptions.ListOfEnabledProperties_ET := FClkEditedActionByEditTemplate.EditTemplateOptions.ListOfEnabledProperties_ET;
  FEditingAction^.EditTemplateOptions.ListOfEditedProperties_ET := FClkEditedActionByEditTemplate.EditTemplateOptions.ListOfEditedProperties_ET;
end;


procedure TfrClickerActions.UpdateFindSubControlInternalStructuresFromAction(AAction: PClkActionRec);
var
  i: Integer;
  TempProfileName: string;
begin
  //the number of items from MatchBitmapText has to match the number of frames from FBMPTextFrames
  frClickerFindControl.CreateBMPTextFrames(Length(AAction^.FindSubControlOptions.MatchBitmapText)); //do not use SetLength(frClickerActions.FBMPTextFrames, Length(FClkActions[ActionIndex].FindSubControlOptions.MatchBitmapText));

  for i := 0 to frClickerFindControl.GetBMPTextFontProfilesCount - 1 do  //this part is still required when selecting an action
  begin
    frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFGColor := AAction^.FindSubControlOptions.MatchBitmapText[i].ForegroundColor;
    frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextBGColor := AAction^.FindSubControlOptions.MatchBitmapText[i].BackgroundColor;
    //frClickerFindControl.BMPTextFontProfiles[i].FGColor := HexToInt(EvaluateReplacements(AAction^.FindSubControlOptions.MatchBitmapText[i].ForegroundColor));
    //frClickerFindControl.BMPTextFontProfiles[i].BGColor := HexToInt(EvaluateReplacements(AAction^.FindSubControlOptions.MatchBitmapText[i].BackgroundColor));
    frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFontName := AAction^.FindSubControlOptions.MatchBitmapText[i].FontName;
    frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextSize := IntToStr(AAction^.FindSubControlOptions.MatchBitmapText[i].FontSize);
    frClickerFindControl.BMPTextFontProfiles[i].Bold := AAction^.FindSubControlOptions.MatchBitmapText[i].Bold;
    frClickerFindControl.BMPTextFontProfiles[i].Italic:= AAction^.FindSubControlOptions.MatchBitmapText[i].Italic;
    frClickerFindControl.BMPTextFontProfiles[i].Underline := AAction^.FindSubControlOptions.MatchBitmapText[i].Underline;
    frClickerFindControl.BMPTextFontProfiles[i].StrikeOut := AAction^.FindSubControlOptions.MatchBitmapText[i].StrikeOut;

    if AAction^.FindSubControlOptions.MatchBitmapText[i].FontQualityUsesReplacement then
    begin
      frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFontQualityIndex := Integer(High(TFontQuality)) + 1;
      frClickerFindControl.BMPTextFontProfiles[i].FontQualityReplacement := AAction^.FindSubControlOptions.MatchBitmapText[i].FontQualityReplacement;
    end
    else
      frClickerFindControl.BMPTextFontProfiles[i].MatchBitmapTextFontQualityIndex := Integer(AAction^.FindSubControlOptions.MatchBitmapText[i].FontQuality);

    frClickerFindControl.BMPTextFontProfiles[i].CharSet := AAction^.FindSubControlOptions.MatchBitmapText[i].CharSet;
    frClickerFindControl.BMPTextFontProfiles[i].Orientation := AAction^.FindSubControlOptions.MatchBitmapText[i].Orientation;
    frClickerFindControl.BMPTextFontProfiles[i].Pitch := AAction^.FindSubControlOptions.MatchBitmapText[i].Pitch;

    TempProfileName := AAction^.FindSubControlOptions.MatchBitmapText[i].ProfileName;
    //frClickerFindControl.BMPTextFontProfiles[i].ProfileName := TempProfileName;
    //frClickerFindControl.tabctrlBMPText.Tabs.Strings[i] := TempProfileName;     //remove these calls if UpdateFontProfileName works as expected
    frClickerFindControl.UpdateFontProfileName(i, TempProfileName);

    frClickerFindControl.BMPTextFontProfiles[i].CropLeft := AAction^.FindSubControlOptions.MatchBitmapText[i].CropLeft;
    frClickerFindControl.BMPTextFontProfiles[i].CropTop := AAction^.FindSubControlOptions.MatchBitmapText[i].CropTop;
    frClickerFindControl.BMPTextFontProfiles[i].CropRight := AAction^.FindSubControlOptions.MatchBitmapText[i].CropRight;
    frClickerFindControl.BMPTextFontProfiles[i].CropBottom := AAction^.FindSubControlOptions.MatchBitmapText[i].CropBottom;

    frClickerFindControl.BMPTextFontProfiles[i].IgnoreBackgroundColor := AAction^.FindSubControlOptions.MatchBitmapText[i].IgnoreBackgroundColor;

    //frClickerFindControl.BMPTextFontProfiles[i].UpdateSelectionLabelsFromCropEditBoxes;  //replaced below with other call
  end;

  frClickerFindControl.SetBMPTextFrameVisibility;

  frClickerFindControl.UpdateListsOfSearchFiles(AAction^.FindSubControlOptions.MatchBitmapFiles, AAction^.FindSubControlOptions.MatchPrimitiveFiles);
  frClickerFindControl.UpdateBitmapAlgorithmSettings;

  frClickerFindControl.UpdatePreviewIcons;
  UpdateControlWidthHeightLabels; //does not depend on acFindControl or acFindSubControl

  case AAction^.ActionOptions.Action of
    acFindControl:
    begin
      UpdateUseWholeScreenLabel(AAction^.FindControlOptions.UseWholeScreen);
      frClickerFindControl.UpdateSearchAreaLabelsFromKeysOnInitRect(AAction^.FindControlOptions.InitialRectangle);
    end;

    acFindSubControl:
    begin
      UpdateUseWholeScreenLabel(AAction^.FindSubControlOptions.UseWholeScreen);
      frClickerFindControl.UpdateSearchAreaLabelsFromKeysOnInitRect(AAction^.FindSubControlOptions.InitialRectangle);
    end;

    else
      ;
  end;

  frClickerFindControl.PreviewText;

  for i := 0 to frClickerFindControl.GetBMPTextFontProfilesCount - 1 do
    frClickerFindControl.BMPTextFontProfiles[i].UpdateSelectionLabelsFromCropInfo(AAction^.FindSubControlOptions.MatchBitmapText[i]);
end;


procedure TfrClickerActions.DeserializeEditTemplateEditingAction;
var
  SerErr: string;
begin
  FClkEditedActionByEditTemplate.EditTemplateOptions.ListOfEnabledProperties := FEditingAction^.EditTemplateOptions.ListOfEnabledProperties;

  SerErr := SetActionProperties(StringReplace(FEditingAction^.EditTemplateOptions.ListOfEditedProperties, CPropSeparatorInt, CPropSeparatorSer, [rfReplaceAll]),
                                FEditingAction^.EditTemplateOptions.EditedActionType,
                                FClkEditedActionByEditTemplate); //converts from serialized ListOfEditedProperties to structured FClkEditedActionByEditTemplate

  if SerErr <> '' then
    DoOnAddToLog(SerErr);

  case FEditingAction^.EditTemplateOptions.EditedActionType of
    acExecApp:
      frClickerExecApp.memExecAppParams.Lines.Text := FEditTemplateOptions_EditingAction.ExecAppOptions.ListOfParams;

    acFindControl, acFindSubControl:   //both
    begin
      UpdateFindSubControlInternalStructuresFromAction(FEditTemplateOptions_EditingAction);
      FEditTemplateOptions_EditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified := InitListOfZerosByItemCount(FEditTemplateOptions_EditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
    end;

    acCallTemplate:
      ListOfCustomVariables := FastReplace_45ToReturn(FEditTemplateOptions_EditingAction.CallTemplateOptions.ListOfCustomVarsAndValues);

    acSetVar:
      frClickerSetVar.SetListOfSetVars(FEditTemplateOptions_EditingAction.SetVarOptions);

    acPlugin:
      DoOnModifyPluginProperty(FEditTemplateOptions_EditingAction);

    else
      ;
  end;

  //To be verified
  FClkEditedActionByEditTemplate.EditTemplateOptions.ListOfEnabledProperties_ET := FEditingAction^.EditTemplateOptions.ListOfEnabledProperties_ET;
  FClkEditedActionByEditTemplate.EditTemplateOptions.ListOfEditedProperties_ET := FEditingAction^.EditTemplateOptions.ListOfEditedProperties_ET;
end;


procedure TfrClickerActions.UpdatePageControlActionExecutionIcons;
begin
  PageControlActionExecution.Pages[0].ImageIndex := 0 + 3 * Ord(Integer(FEditingAction^.ActionOptions.Action) <> CClkUnsetAction);
  PageControlActionExecution.Pages[1].ImageIndex := 1 + 3 * Ord(frClickerConditionEditor.ConditionsAvailable);
  PageControlActionExecution.Pages[2].ImageIndex := 2 + 3 * Ord(FDebuggingInfoAvailable);
end;


procedure TfrClickerActions.SetDebuggingInfoAvailable(Value: Boolean);
begin
  if FDebuggingInfoAvailable <> Value then
  begin
    FDebuggingInfoAvailable := Value;
    UpdatePageControlActionExecutionIcons;
  end;
end;


function TfrClickerActions.GetInMemFS: TInMemFileSystem;
begin
  Result := frClickerFindControl.InMemFS;
end;


procedure TfrClickerActions.SetInMemFS(Value: TInMemFileSystem);
begin
  frClickerFindControl.InMemFS := Value;
end;


function TfrClickerActions.GetExtRenderingInMemFS: TInMemFileSystem;
begin
  Result := frClickerFindControl.ExtRenderingInMemFS;
end;


procedure TfrClickerActions.SetExtRenderingInMemFS(Value: TInMemFileSystem);
begin
  frClickerFindControl.ExtRenderingInMemFS := Value;
end;


function TfrClickerActions.GetEditingActionObjectByActionType: PClkActionRec;  //used by many handlers
begin
  if FEditingAction.ActionOptions.Action = acEditTemplate then
    Result := FEditTemplateOptions_EditingAction
  else
    Result := FEditingAction;
end;


procedure TfrClickerActions.HandleOnCopyControlTextAndClassFromMainWindow(ACompProvider: string; out AControlText, AControlClass: string);
begin
  if not Assigned(FOnCopyControlTextAndClassFromMainWindow) then
    raise Exception.Create('OnCopyControlTextAndClass not assigned for ' + Caption)
  else
    FOnCopyControlTextAndClassFromMainWindow(ACompProvider, AControlText, AControlClass);
end;


function TfrClickerActions.HandleOnGetExtraSearchAreaDebuggingImage(AExtraBitmap: TBitmap): Boolean;
begin
  if not Assigned(FOnGetExtraSearchAreaDebuggingImage) then
    Result := False
  else
    Result := FOnGetExtraSearchAreaDebuggingImage(AExtraBitmap);
end;


function TfrClickerActions.HandleOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  Result := DoOnLoadBitmap(ABitmap, AFileName);
end;


function TfrClickerActions.HandleOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  Result := DoOnLoadRenderedBitmap(ABitmap, AFileName);
end;


procedure TfrClickerActions.HandleOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages: TStringList; Sender: TObject = nil);
begin
  DoOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages);
end;


function TfrClickerActions.HandleOnFileExists(const AFileName: string): Boolean;
begin
  Result := DoOnFileExists(AFileName);
end;


procedure TfrClickerActions.HandleOnSetPictureOpenDialogInitialDir(AInitialDir: string);
begin
  DoOnSetPictureOpenDialogInitialDir(AInitialDir);
end;


function TfrClickerActions.HandleOnPictureOpenDialogExecute: Boolean;
begin
  Result := DoOnPictureOpenDialogExecute;
end;


function TfrClickerActions.HandleOnGetPictureOpenDialogFileName: string;
begin
  Result := DoOnGetPictureOpenDialogFileName;
end;


procedure TfrClickerActions.HandleOnUpdateSearchAreaLimitsInOIFromDraggingLines(ALimitLabelsToUpdate: TLimitLabels; var AOffsets: TSimpleRectString);
begin
  case GetEditingActionObjectByActionType^.ActionOptions.Action of
    acFindControl:
    begin
      if llLeft in ALimitLabelsToUpdate then
      begin
        GetEditingActionObjectByActionType^.FindControlOptions.InitialRectangle.LeftOffset := AOffsets.Left;
        FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindControl_InitialRectangle_PropIndex, CFindControl_InitialRectangle_LeftOffset_PropItemIndex);
      end;

      if llTop in ALimitLabelsToUpdate then
      begin
        GetEditingActionObjectByActionType^.FindControlOptions.InitialRectangle.TopOffset := AOffsets.Top;
        FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindControl_InitialRectangle_PropIndex, CFindControl_InitialRectangle_TopOffset_PropItemIndex);
      end;

      if llRight in ALimitLabelsToUpdate then
      begin
        GetEditingActionObjectByActionType^.FindControlOptions.InitialRectangle.RightOffset := AOffsets.Right;
        FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindControl_InitialRectangle_PropIndex, CFindControl_InitialRectangle_RightOffset_PropItemIndex);
      end;

      if llBottom in ALimitLabelsToUpdate then
      begin
        GetEditingActionObjectByActionType^.FindControlOptions.InitialRectangle.BottomOffset := AOffsets.Bottom;
        FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindControl_InitialRectangle_PropIndex, CFindControl_InitialRectangle_BottomOffset_PropItemIndex);
      end;
    end;

    acFindSubControl:
    begin
      if llLeft in ALimitLabelsToUpdate then
      begin
        GetEditingActionObjectByActionType^.FindSubControlOptions.InitialRectangle.LeftOffset := AOffsets.Left;
        FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindSubControl_InitialRectangle_PropIndex, CFindSubControl_InitialRectangle_LeftOffset_PropItemIndex);
      end;

      if llTop in ALimitLabelsToUpdate then
      begin
        GetEditingActionObjectByActionType^.FindSubControlOptions.InitialRectangle.TopOffset := AOffsets.Top;
        FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindSubControl_InitialRectangle_PropIndex, CFindSubControl_InitialRectangle_TopOffset_PropItemIndex);
      end;

      if llRight in ALimitLabelsToUpdate then
      begin
        GetEditingActionObjectByActionType^.FindSubControlOptions.InitialRectangle.RightOffset := AOffsets.Right;
        FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindSubControl_InitialRectangle_PropIndex, CFindSubControl_InitialRectangle_RightOffset_PropItemIndex);
      end;

      if llBottom in ALimitLabelsToUpdate then
      begin
        GetEditingActionObjectByActionType^.FindSubControlOptions.InitialRectangle.BottomOffset := AOffsets.Bottom;
        FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindSubControl_InitialRectangle_PropIndex, CFindSubControl_InitialRectangle_BottomOffset_PropItemIndex);
      end;
    end;

    else
      ;
  end; //case
end;


procedure TfrClickerActions.HandleOnUpdateTextCroppingLimitsInOIFromDraggingLines(ALimitLabelsToUpdate: TLimitLabels; var AOffsets: TSimpleRectString; AFontProfileIndex: Integer);
var
  UpdatingNodeIndex: Integer;
begin
  if llLeft in ALimitLabelsToUpdate then
  begin
    UpdatingNodeIndex := AFontProfileIndex * CPropCount_FindSubControlMatchBitmapText + CFindSubControl_MatchBitmapText_CropLeft_PropItemIndex;
    GetEditingActionObjectByActionType^.FindSubControlOptions.MatchBitmapText[AFontProfileIndex].CropLeft := AOffsets.Left;
    FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindSubControl_MatchBitmapText_PropIndex, UpdatingNodeIndex);
  end;

  if llTop in ALimitLabelsToUpdate then
  begin
    UpdatingNodeIndex := AFontProfileIndex * CPropCount_FindSubControlMatchBitmapText + CFindSubControl_MatchBitmapText_CropTop_PropItemIndex;
    GetEditingActionObjectByActionType^.FindSubControlOptions.MatchBitmapText[AFontProfileIndex].CropTop := AOffsets.Top;
    FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindSubControl_MatchBitmapText_PropIndex, UpdatingNodeIndex);
  end;

  if llRight in ALimitLabelsToUpdate then
  begin
    UpdatingNodeIndex := AFontProfileIndex * CPropCount_FindSubControlMatchBitmapText + CFindSubControl_MatchBitmapText_CropRight_PropItemIndex;
    GetEditingActionObjectByActionType^.FindSubControlOptions.MatchBitmapText[AFontProfileIndex].CropRight := AOffsets.Right;
    FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindSubControl_MatchBitmapText_PropIndex, UpdatingNodeIndex);
  end;

  if llBottom in ALimitLabelsToUpdate then
  begin
    UpdatingNodeIndex := AFontProfileIndex * CPropCount_FindSubControlMatchBitmapText + CFindSubControl_MatchBitmapText_CropBottom_PropItemIndex;
    GetEditingActionObjectByActionType^.FindSubControlOptions.MatchBitmapText[AFontProfileIndex].CropBottom := AOffsets.Bottom;
    FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindSubControl_MatchBitmapText_PropIndex, UpdatingNodeIndex);
  end;
end;


function TfrClickerActions.HandleOnGetDisplayedText: string;
begin
  case GetEditingActionObjectByActionType^.ActionOptions.Action of
    acFindControl:
      Result := GetEditingActionObjectByActionType^.FindControlOptions.MatchText;

    acFindSubControl:
      Result := GetEditingActionObjectByActionType^.FindSubControlOptions.MatchText;

    else
      Result := 'undefined action';
  end;
end;


procedure TfrClickerActions.HandleOnSetMatchTextAndClassToOI(AMatchText, AMatchClassName: string);
begin
  TriggerOnControlsModified(GetEditingActionObjectByActionType^.FindControlOptions.MatchText <> AMatchText);
  TriggerOnControlsModified(GetEditingActionObjectByActionType^.FindControlOptions.MatchClassName <> AMatchClassName);
  GetEditingActionObjectByActionType^.FindControlOptions.MatchText := AMatchText;
  GetEditingActionObjectByActionType^.FindControlOptions.MatchClassName := AMatchClassName;

  if CurrentlyEditingActionType = acEditTemplate then
  begin
    FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_EditedAction, CFindControl_MatchText_PropIndex, -1, True);
    FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_EditedAction, CFindControl_MatchClassName_PropIndex, -1, True);
  end
  else
  begin
    FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_ActionSpecific, CFindControl_MatchText_PropIndex, -1, True);
    FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_ActionSpecific, CFindControl_MatchClassName_PropIndex, -1, True);
  end;
end;


function TfrClickerActions.HandleOnGetFindControlOptions: PClkFindControlOptions;
begin
  Result := @GetEditingActionObjectByActionType^.FindControlOptions;
end;


function TfrClickerActions.HandleOnGetFindSubControlOptions: PClkFindSubControlOptions;
begin
  Result := @GetEditingActionObjectByActionType^.FindSubControlOptions;
end;


function TfrClickerActions.HandleOnGetIsFindSubControl: Boolean;
begin
  Result := GetEditingActionObjectByActionType^.ActionOptions.Action = acFindSubControl;
end;


function TfrClickerActions.HandleOnExecuteFindSubControlAction(AErrorLevel, AErrorCount, AFastSearchErrorCount: Integer; AFontName: string; AFontSize: Integer; out AFoundArea: TRect): Boolean;
begin
  Result := DoOnExecuteFindSubControlAction(AErrorLevel, AErrorCount, AFastSearchErrorCount, AFontName, AFontSize, AFoundArea);
end;


procedure TfrClickerActions.HandleOnAddToLog(s: string);
begin
  DoOnAddToLog(s);
end;


procedure TfrClickerActions.HandleOnClickerExecAppFrame_OnTriggerOnControlsModified;
begin
  GetEditingActionObjectByActionType.ExecAppOptions.ListOfParams := frClickerExecApp.memExecAppParams.Lines.Text;
  FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_ActionSpecific, CExecApp_ListOfParams_PropIndex, -1);
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.HandleOnClickerSetVarFrame_OnTriggerOnControlsModified;
begin
  GetEditingActionObjectByActionType.SetVarOptions := frClickerSetVar.GetListOfSetVars;
  TriggerOnControlsModified;
end;


function TfrClickerActions.HandleOnClickerSetVarFrame_OnGetFullTemplatesDir: string;
begin
  Result := FFullTemplatesDir;
end;


function TfrClickerActions.HandleOnClickerSetVarFrame_OnGetSelfTemplatesDir: string;
begin
  Result := DoOnGetSelfTemplatesDir;
end;


procedure TfrClickerActions.HandleOnClickerSetVarFrame_OnShowAutoComplete(AEdit: TEdit);
begin
  DoOnShowAutoComplete(AEdit);
end;


procedure TfrClickerActions.HandleOnClickerCallTemplateFrame_OnTriggerOnControlsModified;
begin
  GetEditingActionObjectByActionType.CallTemplateOptions.ListOfCustomVarsAndValues := frClickerCallTemplate.GetListOfCustomVariables;
  TriggerOnControlsModified;
end;


function TfrClickerActions.HandleOnEvaluateReplacementsFunc(s: string; Recursive: Boolean = True; AEvalTextCount: Integer = -1): string;
begin
  Result := EvaluateReplacements(s, Recursive, AEvalTextCount);
end;


procedure TfrClickerActions.HandleOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
begin
  FCurrentlyEditingPrimitiveFileName := AFileName;
  DoOnLoadPrimitivesFile(AFileName, APrimitives, AOrders, ASettings);
end;


procedure TfrClickerActions.HandleOnSavePrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
begin
  FCurrentlyEditingPrimitiveFileName := AFileName; //required on save as
  DoOnSavePrimitivesFile(AFileName, APrimitives, AOrders, ASettings);
end;


function TfrClickerActions.GetIndexOfCurrentlyEditingPrimitivesFile: Integer;  //based on FCurrentlyEditingPrimitiveFileName
var
  ListOfPrimitiveFiles: TStringList;
  UpperCaseName: string;
  ResolvedFileName: string;
  i: Integer;
begin
  Result := -1;

  ListOfPrimitiveFiles := TStringList.Create;
  try
    ListOfPrimitiveFiles.LineBreak := #13#10;
    ListOfPrimitiveFiles.Text := GetEditingActionObjectByActionType^.FindSubControlOptions.MatchPrimitiveFiles;

    UpperCaseName := UpperCase(FCurrentlyEditingPrimitiveFileName);

    for i := 0 to ListOfPrimitiveFiles.Count - 1 do
    begin
      ResolvedFileName := ListOfPrimitiveFiles.Strings[i];
      ResolvedFileName := StringReplace(ResolvedFileName, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);
      ResolvedFileName := StringReplace(ResolvedFileName, '$TemplateDir$', FFullTemplatesDir, [rfReplaceAll]);
      ResolvedFileName := StringReplace(ResolvedFileName, '$SelfTemplateDir$', ExtractFileDir(DoOnGetLoadedTemplateFileName), [rfReplaceAll]);
      ResolvedFileName := EvaluateReplacements(ResolvedFileName);

      if UpperCase(ResolvedFileName) = UpperCaseName then     //this requires the list to have unique filenames
      begin
        Result := i;
        Break;
      end;
    end;
  finally
    ListOfPrimitiveFiles.Free;
  end;
end;


procedure TfrClickerActions.HandleOnPrimitivesTriggerOnControlsModified;
var
  PrimitiveFileIndex: Integer;
  ListOfPrimitiveFiles_Modified: TStringList;
  TempEditingAction: PClkActionRec;
begin
  PrimitiveFileIndex := GetIndexOfCurrentlyEditingPrimitivesFile;

  if PrimitiveFileIndex <> -1 then
  begin
    ListOfPrimitiveFiles_Modified := TStringList.Create;
    try
      ListOfPrimitiveFiles_Modified.LineBreak := #13#10;
      if GetEditingActionObjectByActionType.ActionOptions.Action = acEditTemplate then
        TempEditingAction := FEditTemplateOptions_EditingAction
      else
        TempEditingAction := GetEditingActionObjectByActionType;

      ListOfPrimitiveFiles_Modified.Text := TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified;
      ListOfPrimitiveFiles_Modified.Strings[PrimitiveFileIndex] := '1';
      TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified := ListOfPrimitiveFiles_Modified.Text;
    finally
      ListOfPrimitiveFiles_Modified.Free;
    end;

    //repaint node
    FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindSubControl_MatchPrimitiveFiles_PropIndex, PrimitiveFileIndex);
  end;
end;


procedure TfrClickerActions.HandleOnSaveFromMenu(Sender: TObject);
var
  CurrentlyEditingPrimitiveFileIndex: Integer;
begin
  CurrentlyEditingPrimitiveFileIndex := GetIndexOfCurrentlyEditingPrimitivesFile;

  if CurrentlyEditingPrimitiveFileIndex <> -1 then
  begin
    SavePrimitivesFileFromMenu(CurrentlyEditingPrimitiveFileIndex, GetEditingActionObjectByActionType);
    FOIFrame.ReloadPropertyItems(CCategory_ActionSpecific, CFindSubControl_MatchPrimitiveFiles_PropIndex);
  end
  else
    MessageBox(Handle, 'Can''t get index of editing primitives filename. Please make sure the owner action is selected.', PChar(Application.Title), MB_ICONERROR);
end;


procedure TfrClickerActions.HandleOnGetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);
begin
  DoOnGetFontFinderSettings(AFontFinderSettings);
end;


procedure TfrClickerActions.HandleOnSetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);
begin
  DoOnSetFontFinderSettings(AFontFinderSettings);
end;


procedure TfrClickerActions.HandleOnPluginDbgStop;
begin
  DoOnPluginDbgStop;
end;


procedure TfrClickerActions.HandleOnPluginDbgContinueAll;
begin
  DoOnPluginDbgContinueAll;
end;


procedure TfrClickerActions.HandleOnPluginDbgStepOver;
begin
  DoOnPluginDbgStepOver;
end;


function TfrClickerActions.HandleOnPluginDbgRequestLineNumber(out ALineContent, ADbgSymFile: string): Integer;
begin
  Result := DoOnPluginDbgRequestLineNumber(ALineContent, ADbgSymFile);
end;


procedure TfrClickerActions.HandleOnPluginDbgSetBreakpoint(ALineIndex, ASelectedSourceFileIndex: Integer; AEnabled: Boolean);
begin
  DoOnPluginDbgSetBreakpoint(ALineIndex, ASelectedSourceFileIndex, AEnabled);
end;


function TfrClickerActions.HandleOnTClkIniFileCreate(AFileName: string): TClkIniFile;
begin
  Result := DoOnTClkIniFileCreate(AFileName);
end;


{$IFDEF MemPlugins}
  function TfrClickerActions.HandleOnLoadPluginFromInMemFS(APlugin: TMemoryStream; AFileName: string): Boolean;
  begin
    Result := DoOnLoadPluginFromInMemFS(APlugin, AFileName);
  end;
{$ENDIF}


function TfrClickerActions.DoOnEditCallTemplateBreakCondition(var AActionCondition: string): Boolean;
begin
  if not Assigned(FOnEditCallTemplateBreakCondition) then
    raise Exception.Create('OnEditCallTemplateBreakCondition not assigned.')
  else
    Result := FOnEditCallTemplateBreakCondition(AActionCondition);
end;


function TfrClickerActions.DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if not Assigned(FOnLoadBitmap) then
    raise Exception.Create('OnLoadBitmap not assigned.')
  else
    Result := FOnLoadBitmap(ABitmap, AFileName);
end;


function TfrClickerActions.DoOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if not Assigned(FOnLoadRenderedBitmap) then
    raise Exception.Create('OnLoadRenderedBitmap not assigned.')
  else
    Result := FOnLoadRenderedBitmap(ABitmap, AFileName);
end;


procedure TfrClickerActions.DoOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages: TStringList);
begin
  if not Assigned(FOnGetListOfExternallyRenderedImages) then
    raise Exception.Create('OnGetListOfExternallyRenderedImages not assigned.')
  else
    FOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages);
end;


{$IFDEF MemPlugins}
  procedure TfrClickerActions.DoOnGetListOfInMemPlugins(AListOfInMemPlugins: TStringList);
  begin
    if not Assigned(FOnGetListOfInMemPlugins) then
      raise Exception.Create('OnGetListOfInMemPlugins not assigned.')
    else
      FOnGetListOfInMemPlugins(AListOfInMemPlugins);
  end;


  procedure TfrClickerActions.DoOnLoadPluginFromDiskToPluginInMemFileSystem(APluginPath: string);
  begin
    if not Assigned(FOnLoadPluginFromDiskToPluginInMemFileSystem) then
      raise Exception.Create('OnLoadPluginFromDiskToPluginInMemFileSystem not assigned.')
    else
      FOnLoadPluginFromDiskToPluginInMemFileSystem(APluginPath);
  end;


  function TfrClickerActions.DoOnLoadPluginFromInMemFS(APlugin: TMemoryStream; AFileName: string): Boolean;
  begin
    if not Assigned(FOnLoadPluginFromInMemFS) then
      raise Exception.Create('OnLoadPluginFromInMemFS not assigned.');

    Result := FOnLoadPluginFromInMemFS(APlugin, AFileName);
  end;
{$ENDIF}


function TfrClickerActions.DoOnGetPluginInMemFS: TInMemFileSystem;
begin
  if not Assigned(FOnGetPluginInMemFS) then
    raise Exception.Create('OnGetPluginInMemFS is not assigned.')
  else
    Result := FOnGetPluginInMemFS();
end;


procedure TfrClickerActions.DoOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
begin
  if not Assigned(FOnLoadPrimitivesFile) then
    raise Exception.Create('OnLoadPrimitivesFile not assigned.')
  else
    FOnLoadPrimitivesFile(AFileName, APrimitives, AOrders, ASettings);
end;


procedure TfrClickerActions.DoOnSavePrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
begin
  if not Assigned(FOnSavePrimitivesFile) then
    raise Exception.Create('OnSavePrimitivesFile not assigned.')
  else
    FOnSavePrimitivesFile(AFileName, APrimitives, AOrders, ASettings);
end;


function TfrClickerActions.DoOnFileExists(const AFileName: string): Boolean;
begin
  if not Assigned(FOnFileExists) then
    raise Exception.Create('OnFileExists is not assigned.')
  else
    Result := FOnFileExists(AFileName);
end;


procedure TfrClickerActions.DoOnSetOpenDialogMultiSelect;
begin
  if not Assigned(FOnSetOpenDialogMultiSelect) then
    raise Exception.Create('OnSetOpenDialogMultiSelect is not assigned.')
  else
    FOnSetOpenDialogMultiSelect;
end;


procedure TfrClickerActions.DoOnSetOpenDialogInitialDir(AInitialDir: string);
begin
  if not Assigned(FOnSetOpenDialogInitialDir) then
    raise Exception.Create('OnSetOpenDialogInitialDir is not assigned.')
  else
    FOnSetOpenDialogInitialDir(AInitialDir);
end;


function TfrClickerActions.DoOnOpenDialogExecute(AFilter: string): Boolean;
begin
  if not Assigned(FOnOpenDialogExecute) then
    raise Exception.Create('OnOpenDialogExecute is not assigned.')
  else
    Result := FOnOpenDialogExecute(AFilter);
end;


function TfrClickerActions.DoOnGetOpenDialogFileName: string;
begin
  if not Assigned(FOnGetOpenDialogFileName) then
    raise Exception.Create('OnGetOpenDialogFileName is not assigned.')
  else
    Result := FOnGetOpenDialogFileName;
end;


procedure TfrClickerActions.DoOnSetSaveDialogInitialDir(AInitialDir: string);
begin
  if not Assigned(FOnSetSaveDialogInitialDir) then
    raise Exception.Create('OnSetSaveDialogInitialDir is not assigned.')
  else
    FOnSetSaveDialogInitialDir(AInitialDir);
end;


function TfrClickerActions.DoOnSaveDialogExecute(AFilter: string): Boolean;
begin
  if not Assigned(FOnSaveDialogExecute) then
    raise Exception.Create('OnSaveDialogExecute is not assigned.')
  else
    Result := FOnSaveDialogExecute(AFilter);
end;


function TfrClickerActions.DoOnGetSaveDialogFileName: string;
begin
  if not Assigned(FOnGetSaveDialogFileName) then
    raise Exception.Create('OnGetSaveDialogFileName is not assigned.')
  else
    Result := FOnGetSaveDialogFileName;
end;


procedure TfrClickerActions.DoOnSetSaveDialogFileName(AFileName: string);
begin
  if not Assigned(FOnSetSaveDialogFileName) then
    raise Exception.Create('OnSetSaveDialogFileName is not assigned.')
  else
    FOnSetSaveDialogFileName(AFileName);
end;


procedure TfrClickerActions.DoOnSetPictureSetOpenDialogMultiSelect;
begin
  if not Assigned(FOnSetPictureSetOpenDialogMultiSelect) then
    raise Exception.Create('OnSetPictureSetOpenDialogMultiSelect is not assigned.')
  else
    FOnSetPictureSetOpenDialogMultiSelect;
end;


procedure TfrClickerActions.DoOnSetPictureOpenDialogInitialDir(AInitialDir: string);
begin
  if not Assigned(FOnSetPictureOpenDialogInitialDir) then
    raise Exception.Create('OnSetPictureOpenDialogInitialDir not assigned.')
  else
    FOnSetPictureOpenDialogInitialDir(AInitialDir);
end;


function TfrClickerActions.DoOnPictureOpenDialogExecute: Boolean;
begin
  if not Assigned(FOnPictureOpenDialogExecute) then
    raise Exception.Create('OnPictureOpenDialogExecute not assigned.')
  else
    Result := FOnPictureOpenDialogExecute;
end;


function TfrClickerActions.DoOnGetPictureOpenDialogFileName: string;
begin
  if not Assigned(FOnGetPictureOpenDialogFileName) then
    raise Exception.Create('OnGetPictureOpenDialogFileName not assigned.')
  else
    Result := FOnGetPictureOpenDialogFileName;
end;


function TfrClickerActions.DoOnExecuteFindSubControlAction(AErrorLevel, AErrorCount, AFastSearchErrorCount: Integer; AFontName: string; AFontSize: Integer; out AFoundArea: TRect): Boolean;
begin
  if not Assigned(FOnExecuteFindSubControlAction) then
    raise Exception.Create('OnExecuteFindSubControlAction not assigned.')
  else
    Result := FOnExecuteFindSubControlAction(AErrorLevel, AErrorCount, AFastSearchErrorCount, AFontName, AFontSize, AFoundArea);
end;


procedure TfrClickerActions.DoOnAddToLog(s: string);
begin
  if not Assigned(FOnAddToLog) then
    raise Exception.Create('OnAddToLog not assigned.')
  else
    FOnAddToLog(s);
end;


procedure TfrClickerActions.DoOnGetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);
begin
  if not Assigned(FOnGetFontFinderSettings) then
    raise Exception.Create('OnGetFontFinderSettings not assigned.')
  else
    FOnGetFontFinderSettings(AFontFinderSettings);
end;


procedure TfrClickerActions.DoOnSetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);
begin
  if not Assigned(FOnSetFontFinderSettings) then
    raise Exception.Create('OnSetFontFinderSettings not assigned.')
  else
    FOnSetFontFinderSettings(AFontFinderSettings);
end;


procedure TfrClickerActions.DoOnGetListOfAvailableSetVarActions(AListOfSetVarActions: TStringList);
begin
  if not Assigned(FOnGetListOfAvailableSetVarActions) then
    raise Exception.Create('OnGetListOfAvailableSetVarActions not assigned.')
  else
    FOnGetListOfAvailableSetVarActions(AListOfSetVarActions);
end;


procedure TfrClickerActions.DoOnGetListOfAvailableActions(AListOfActions: TStringList);
begin
  if not Assigned(FOnGetListOfAvailableActions) then
    raise Exception.Create('OnGetListOfAvailableActions not assigned.')
  else
    FOnGetListOfAvailableActions(AListOfActions);
end;


function TfrClickerActions.DoOnTClkIniReadonlyFileCreate(AFileName: string): TClkIniReadonlyFile;
begin
  if not Assigned(FOnTClkIniReadonlyFileCreate) then
    raise Exception.Create('OnTClkIniReadonlyFileCreate is not assigned.')
  else
    Result := FOnTClkIniReadonlyFileCreate(AFileName);
end;


procedure TfrClickerActions.DoOnModifyPluginProperty(AAction: PClkActionRec);
begin
  if not Assigned(FOnModifyPluginProperty) then
    raise Exception.Create('OnModifyPluginProperty not assigned.')
  else
    FOnModifyPluginProperty(AAction);
end;


procedure TfrClickerActions.DoOnPluginDbgStop;
begin
  if not Assigned(FOnPluginDbgStop) then
    raise Exception.Create('OnPluginDbgStop not assigned.');

  FOnPluginDbgStop();
end;


procedure TfrClickerActions.DoOnPluginDbgContinueAll;
begin
  if not Assigned(FOnPluginDbgContinueAll) then
    raise Exception.Create('OnPluginDbgContinueAll not assigned.');

  FOnPluginDbgContinueAll();
end;


procedure TfrClickerActions.DoOnPluginDbgStepOver;
begin
  if not Assigned(FOnPluginDbgStepOver) then
    raise Exception.Create('OnPluginDbgStepOver not assigned.');

  FOnPluginDbgStepOver();
end;


function TfrClickerActions.DoOnPluginDbgRequestLineNumber(out ALineContent, ADbgSymFile: string): Integer;
begin
  if not Assigned(FOnPluginDbgRequestLineNumber) then
    raise Exception.Create('OnPluginDbgRequestLineNumber not assigned.');

  Result := FOnPluginDbgRequestLineNumber(ALineContent, ADbgSymFile);
end;


procedure TfrClickerActions.DoOnPluginDbgSetBreakpoint(ALineIndex, ASelectedSourceFileIndex: Integer; AEnabled: Boolean);
begin
  if not Assigned(FOnPluginDbgSetBreakpoint) then
    raise Exception.Create('OnPluginDbgSetBreakpoint not assigned.');

  FOnPluginDbgSetBreakpoint(ALineIndex, ASelectedSourceFileIndex, AEnabled);
end;


function TfrClickerActions.DoOnTClkIniFileCreate(AFileName: string): TClkIniFile;
begin
  if not Assigned(FOnTClkIniFileCreate) then
    raise Exception.Create('OnTClkIniFileCreate not assigned.');

  Result := FOnTClkIniFileCreate(AFileName);
end;


function TfrClickerActions.DoOnGetSelfTemplatesDir: string;
begin
  if not Assigned(FOnGetSelfTemplatesDir) then
    raise Exception.Create('OnGetSelfTemplatesDir not assigned.')
  else
    Result := FOnGetSelfTemplatesDir();
end;


procedure TfrClickerActions.DoOnShowAutoComplete(AEdit: TEdit);
begin
  if not Assigned(FOnShowAutoComplete) then
    raise Exception.Create('OnShowAutoComplete not assigned.')
  else
    FOnShowAutoComplete(AEdit);
end;


procedure TfrClickerActions.DoOnUpdateActionScrollIndex(AActionScrollIndex: string);
begin
  if not Assigned(FOnUpdateActionScrollIndex) then
    raise Exception.Create('OnUpdateActionScrollIndex not assigned.')
  else
    FOnUpdateActionScrollIndex(AActionScrollIndex);
end;


function TfrClickerActions.DoOnGetLoadedTemplateFileName: string;
begin
  if not Assigned(FOnGetLoadedTemplateFileName) then
    raise Exception.Create('OnGetLoadedTemplateFileName not assigned.')
  else
    Result := FOnGetLoadedTemplateFileName();
end;


procedure TfrClickerActions.DoOnChangeEditTemplateEditingActionType;
begin
  if not Assigned(FOnChangeEditTemplateEditingActionType) then
    raise Exception.Create('OnChangeEditTemplateEditingActionType not assigned.')
  else
    FOnChangeEditTemplateEditingActionType;
end;


//////////////////////////// OI

function TfrClickerActions.GetCurrentlyEditingActionType: TClkAction;
begin
  Result := TClkAction(FCurrentlyEditingActionType);
end;


procedure TfrClickerActions.SetCurrentlyEditingActionType(Value: TClkAction);
  procedure SwitchEditorFrame(AActionType: TClkAction);
  begin
    case AActionType of
      acExecApp:
      begin
        frClickerExecApp.Show;
        frClickerExecApp.BringToFront;
        frClickerFindControl.Hide;
        frClickerSetVar.Hide;
        frClickerCallTemplate.Hide;
        frClickerSleep.Hide;
        frClickerPlugin.Hide;
      end;

      acFindControl, acFindSubControl:
      begin
        frClickerExecApp.Hide;
        frClickerFindControl.Show;
        frClickerFindControl.BringToFront;
        frClickerSetVar.Hide;
        frClickerCallTemplate.Hide;
        frClickerSleep.Hide;
        frClickerPlugin.Hide;
      end;

      acSetVar:
      begin
        frClickerExecApp.Hide;
        frClickerFindControl.Hide;
        frClickerSetVar.Show;
        frClickerSetVar.BringToFront;
        frClickerCallTemplate.Hide;
        frClickerSleep.Hide;
        frClickerPlugin.Hide;
      end;

      acCallTemplate:
      begin
        frClickerExecApp.Hide;
        frClickerFindControl.Hide;
        frClickerSetVar.Hide;
        frClickerCallTemplate.Show;
        frClickerCallTemplate.BringToFront;
        frClickerSleep.Hide;
        frClickerPlugin.Hide;
      end;

      acSleep:
      begin
        frClickerExecApp.Hide;
        frClickerFindControl.Hide;
        frClickerSetVar.Hide;
        frClickerCallTemplate.Hide;
        frClickerSleep.Show;
        frClickerSleep.BringToFront;
        frClickerPlugin.Hide;
      end;

      acPlugin:
      begin
        frClickerExecApp.Hide;
        frClickerFindControl.Hide;
        frClickerSetVar.Hide;
        frClickerCallTemplate.Hide;
        frClickerSleep.Hide;
        frClickerPlugin.Show;
        frClickerPlugin.BringToFront;
      end;

      else
      begin
        frClickerExecApp.Hide;
        frClickerFindControl.Hide;
        frClickerSetVar.Hide;
        frClickerCallTemplate.Hide;
        frClickerSleep.Hide;
        frClickerPlugin.Hide;

        pnlCover.Left := 0;
        pnlCover.Top := 0;
        pnlCover.Width := pnlExtra.Width;
        pnlCover.Height := pnlExtra.Height;
        pnlCover.Show;
        pnlCover.BringToFront;
      end;
    end;
  end;

begin
  FCurrentlyEditingActionType := Ord(Value);
  BuildFontColorIconsList;
  FOIFrame.ReloadContent;
  pnlvstOI.Visible := True;

  pnlCover.Hide;

  if Value = acEditTemplate then
    SwitchEditorFrame(FEditingAction^.EditTemplateOptions.EditedActionType)
  else
    SwitchEditorFrame(Value);
end;


procedure TfrClickerActions.RefreshActionName;
begin
  FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_Common, CMain_ActionName_PropIndex, -1);
end;


procedure TfrClickerActions.ResetAllPmtvModifiedFlags;
var
  ListOfFiles_Modified: TStringList;
  i: Integer;
  TempEditingAction: PClkActionRec;
begin
  ListOfFiles_Modified := TStringList.Create;
  try
    ListOfFiles_Modified.LineBreak := #13#10;
    TempEditingAction := GetEditingActionObjectByActionType;

    ListOfFiles_Modified.Text := TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified;

    for i := 0 to ListOfFiles_Modified.Count - 1 do
      ListOfFiles_Modified.Strings[i] := '0';

    TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified := ListOfFiles_Modified.Text;
    frClickerFindControl.frClickerPrimitives.ClearContent;
  finally
    ListOfFiles_Modified.Free;
  end;
end;


procedure TfrClickerActions.SetGridDrawingOption(Value: TDisplayGridLineOption);
begin
  frClickerFindControl.GridDrawingOption := Value;
  frClickerFindControl.RefreshGrid;
end;


procedure TfrClickerActions.SetPreviewSelectionColors(Value: TSelectionColors);
begin
  frClickerFindControl.PreviewSelectionColors := Value;
end;


function TfrClickerActions.GetModifiedPmtvFiles: Boolean;
begin
  Result := GetIndexOfFirstModifiedPmtvFile(FEditingAction) > -1;  //for now, it is ok to call with FEditingAction, because it is requested from outside
end;


procedure TfrClickerActions.tmrReloadOIContentTimer(Sender: TObject);
begin
  tmrReloadOIContent.Enabled := False;
  FOIFrame.ReloadContent;
end;


procedure TfrClickerActions.CopyTextAndClassFromExternalProvider(AProviderName: string);
var
  ControlText, ControlClass: string;
begin
  if not Assigned(FOnCopyControlTextAndClassFromMainWindow) then
    raise Exception.Create('OnCopyControlTextAndClass not assigned for ' + Caption)
  else
  begin
    FOIFrame.CancelCurrentEditing;

    FOnCopyControlTextAndClassFromMainWindow(AProviderName, ControlText, ControlClass);

    TriggerOnControlsModified(GetEditingActionObjectByActionType^.FindControlOptions.MatchText <> ControlText);
    TriggerOnControlsModified(GetEditingActionObjectByActionType^.FindControlOptions.MatchClassName <> ControlClass);

    GetEditingActionObjectByActionType^.FindControlOptions.MatchText := ControlText;
    GetEditingActionObjectByActionType^.FindControlOptions.MatchClassName := ControlClass;

    FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_ActionSpecific, CFindControl_MatchText_PropIndex, -1);
    FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_ActionSpecific, CFindControl_MatchClassName_PropIndex, -1);
  end;
end;


procedure TfrClickerActions.SetActionTimeoutToValue(AValue: Integer);
begin
  FOIFrame.CancelCurrentEditing;
  FEditingAction^.ActionOptions.ActionTimeout := AValue;
  FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_Common, CMain_ActionTimeout_PropIndex, -1);
end;


procedure TfrClickerActions.MenuItem_SetActionTimeoutFromOI(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ValueStr: string;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    ValueStr := StringReplace(MenuData^.MenuItemCaption, '&', '', [rfReplaceAll]);
    SetActionTimeoutToValue(StrToIntDef(ValueStr, 0));
    TriggerOnControlsModified;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_SetEditedActionTimeoutFromOI(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ValueStr: string;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    ValueStr := StringReplace(MenuData^.MenuItemCaption, '&', '', [rfReplaceAll]);

    FOIFrame.CancelCurrentEditing;
    MenuData^.TempEditingAction^.EditTemplateOptions.EditedActionTimeout := StrToIntDef(ValueStr, 0);
    FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_ActionSpecific, CEditTemplate_EditedActionTimeout_PropIndex, -1);

    TriggerOnControlsModified;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.FreeOIPopupMenu(Sender: TObject);
var
  MenuData: POIMenuItemData;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  Dispose(MenuData);
end;


procedure TfrClickerActions.MenuItem_CopyTextAndClassFromPreviewWindowClick(Sender: TObject);
begin
  CopyTextAndClassFromExternalProvider(CExtProvPreviewWindow);
  FreeOIPopupMenu(Sender);
end;


procedure TfrClickerActions.MenuItem_CopyTextAndClassFromWinInterpWindowClick(Sender: TObject);
begin
  CopyTextAndClassFromExternalProvider(CExtProvWinInterpWindow);
  FreeOIPopupMenu(Sender);
end;


procedure TfrClickerActions.MenuItem_SetTextAndClassAsSystemMenuClick(Sender: TObject);
begin
  CopyTextAndClassFromExternalProvider(CExtProvSystemMenu);
  FreeOIPopupMenu(Sender);
end;


procedure TfrClickerActions.MenuItem_AddBMPFilesToPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ListOfFiles: TStringList;
begin
  DoOnSetPictureSetOpenDialogMultiSelect;
  if not DoOnPictureOpenDialogExecute then
    Exit;

  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    ListOfFiles := TStringList.Create;
    try
      ListOfFiles.LineBreak := #13#10;
      ListOfFiles.Text := MenuData.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles;
      ListOfFiles.Text := ListOfFiles.Text + DoOnGetPictureOpenDialogFileName;
      MenuData.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles := ListOfFiles.Text;

      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
      TriggerOnControlsModified;
      frClickerFindControl.UpdateListsOfSearchFiles(MenuData.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles, MenuData.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
    finally
      ListOfFiles.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_AddExtBMPFilesToPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ListOfFiles: TStringList;
begin
  DoOnSetPictureSetOpenDialogMultiSelect;
  if not DoOnPictureOpenDialogExecute then
    Exit;

  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    ListOfFiles := TStringList.Create;
    try
      ListOfFiles.LineBreak := #13#10;
      ListOfFiles.Text := MenuData.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles;
      ListOfFiles.Text := ListOfFiles.Text + DoOnGetPictureOpenDialogFileName;
      MenuData.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles := ListOfFiles.Text;

      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
      TriggerOnControlsModified;
      frClickerFindControl.UpdateListsOfSearchFiles(MenuData.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles, MenuData.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
    finally
      ListOfFiles.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_RemoveAllBMPFilesFromPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
begin
  if MessageBox(Handle, 'Are you sure you want to remove all files from this list?', PChar(Application.MainForm.Caption), MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles := '';
    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
    TriggerOnControlsModified;
    frClickerFindControl.UpdateListsOfSearchFiles(EditingAction^.FindSubControlOptions.MatchBitmapFiles, EditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.UpdateMatchBitmapFilesItem(AEditingAction: PClkActionRec; AFileIndex: Integer; ANewFileName: string);
var
  ListOfFiles: TStringList;
begin
  ListOfFiles := TStringList.Create;
  try
    ListOfFiles.LineBreak := #13#10;
    ListOfFiles.Text := AEditingAction^.FindSubControlOptions.MatchBitmapFiles;
    ListOfFiles.Strings[AFileIndex] := ANewFileName;
    AEditingAction^.FindSubControlOptions.MatchBitmapFiles := ListOfFiles.Text;
  finally
    ListOfFiles.Free;
  end;
end;


procedure TfrClickerActions.MenuItem_BrowseBMPFileFromPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    //DoOnSetPictureOpenDialogInitialDir();
    if not DoOnPictureOpenDialogExecute then
      Exit;

    UpdateMatchBitmapFilesItem(MenuData.TempEditingAction, MenuData^.PropertyItemIndex, DoOnGetPictureOpenDialogFileName);

    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex, True);
    TriggerOnControlsModified;
    frClickerFindControl.UpdateListsOfSearchFiles(MenuData.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles, MenuData.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_RemoveBMPFileFromPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ListOfFiles: TStringList;
begin
  if MessageBox(Handle, 'Are you sure you want to remove this file from list?', PChar(Application.MainForm.Caption), MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    ListOfFiles := TStringList.Create;
    try
      ListOfFiles.LineBreak := #13#10;
      ListOfFiles.Text := MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles;
      ListOfFiles.Delete(MenuData^.PropertyItemIndex);
      MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles := ListOfFiles.Text;

      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex, True);

      TriggerOnControlsModified;
      frClickerFindControl.UpdateListsOfSearchFiles(MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles, MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
    finally
      ListOfFiles.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_MoveBMPFileUpInPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ListOfFiles: TStringList;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    ListOfFiles := TStringList.Create;
    try
      ListOfFiles.LineBreak := #13#10;
      if MenuData^.PropertyItemIndex <= 0 then
        Exit;

      ListOfFiles.Text := MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles;
      ListOfFiles.Move(MenuData^.PropertyItemIndex, MenuData^.PropertyItemIndex - 1);
      MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles := ListOfFiles.Text;

      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex, True);

      TriggerOnControlsModified;
      frClickerFindControl.UpdateListsOfSearchFiles(MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles, MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
    finally
      ListOfFiles.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_MoveBMPFileDownInPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ListOfFiles: TStringList;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    ListOfFiles := TStringList.Create;
    try
      ListOfFiles.LineBreak := #13#10;
      ListOfFiles.Text := MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles;
      if MenuData^.PropertyItemIndex >= ListOfFiles.Count - 1 then
        Exit;

      ListOfFiles.Move(MenuData^.PropertyItemIndex, MenuData^.PropertyItemIndex + 1);
      MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles := ListOfFiles.Text;

      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex, True);

      TriggerOnControlsModified;
      frClickerFindControl.UpdateListsOfSearchFiles(MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles, MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
    finally
      ListOfFiles.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_AddExistingPrimitiveFilesToPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ListOfFiles: TStringList;
  ListOfFiles_Modified: TStringList;
  i, OldCount: Integer;
begin
  DoOnSetOpenDialogMultiSelect;
  if not DoOnOpenDialogExecute(CPrimitivesDialogFilter) then
    Exit;

  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    ListOfFiles := TStringList.Create;
    ListOfFiles_Modified := TStringList.Create;
    try
      ListOfFiles.LineBreak := #13#10;
      ListOfFiles_Modified.LineBreak := #13#10;
      ListOfFiles.Text := MenuData.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles;
      OldCount := ListOfFiles.Count;
      ListOfFiles.Text := ListOfFiles.Text + DoOnGetOpenDialogFileName;
      MenuData.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles := ListOfFiles.Text;

      ListOfFiles_Modified.Text := MenuData.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified;

      for i := OldCount to ListOfFiles.Count - 1 do
        ListOfFiles_Modified.Add('0');

      MenuData.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified := ListOfFiles_Modified.Text;

      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
      TriggerOnControlsModified;
      frClickerFindControl.UpdateListsOfSearchFiles(MenuData.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles, MenuData.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
    finally
      ListOfFiles.Free;
      ListOfFiles_Modified.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_AddNewPrimitiveFilesToPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ListOfFiles: TStringList;
  ListOfFiles_Modified: TStringList;
begin
  //DoOnSetOpenDialogMultiSelect; //do not call multiselect, as this is a single file save
  DoOnSetSaveDialogFileName('');
  if not DoOnSaveDialogExecute(CPrimitivesDialogFilter) then
    Exit;

  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    ListOfFiles := TStringList.Create;
    ListOfFiles_Modified := TStringList.Create;
    try
      ListOfFiles.LineBreak := #13#10;
      ListOfFiles_Modified.LineBreak := #13#10;
      ListOfFiles.Text := MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles;
      ListOfFiles.Text := ListOfFiles.Text + DoOnGetSaveDialogFileName;
      MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles := ListOfFiles.Text;

      ListOfFiles_Modified.Text := MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified;
      ListOfFiles_Modified.Add('0');
      MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified := ListOfFiles_Modified.Text;

      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
      TriggerOnControlsModified;
      frClickerFindControl.UpdateListsOfSearchFiles(MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles, MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
    finally
      ListOfFiles.Free;
      ListOfFiles_Modified.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_RemoveAllPrimitiveFilesFromPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
begin
  if MessageBox(Handle, 'Are you sure you want to remove all files from this list?', PChar(Application.MainForm.Caption), MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles := '';
    MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified := '';

    FPrevSelectedPrimitiveNode := -1;
    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);

    TriggerOnControlsModified;
    frClickerFindControl.UpdateListsOfSearchFiles(EditingAction^.FindSubControlOptions.MatchBitmapFiles, EditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_BrowsePrimitiveFileFromPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ListOfFiles: TStringList;
  ListOfFiles_Modified: TStringList;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    //DoOnSetOpenDialogMultiSelect;
    if not DoOnOpenDialogExecute(CPrimitivesDialogFilter) then
      Exit;

    ListOfFiles := TStringList.Create;
    ListOfFiles_Modified := TStringList.Create;
    try
      ListOfFiles.LineBreak := #13#10;
      ListOfFiles_Modified.LineBreak := #13#10;
      ListOfFiles_Modified.Text := MenuData.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified;

      if ListOfFiles_Modified.Strings[MenuData^.PropertyItemIndex] = '1' then
        if MessageBox(Handle, 'The file is modified. Do you want to set this to another file and discard all changes?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = IDNO then
          Exit;

      ListOfFiles.Text := MenuData.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles;
      ListOfFiles.Strings[MenuData^.PropertyItemIndex] := DoOnGetOpenDialogFileName;
      MenuData.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles := ListOfFiles.Text;

      ListOfFiles_Modified.Strings[MenuData^.PropertyItemIndex] := '0';
      MenuData.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified := ListOfFiles_Modified.Text;

      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex, True);

      TriggerOnControlsModified;
      frClickerFindControl.UpdateListsOfSearchFiles(MenuData.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles, MenuData.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
    finally
      ListOfFiles.Free;
      ListOfFiles_Modified.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_RemovePrimitiveFileFromPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ListOfFiles: TStringList;
  ListOfFiles_Modified: TStringList;
begin
  if MessageBox(Handle, 'Are you sure you want to remove this file from list?', PChar(Application.MainForm.Caption), MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    ListOfFiles := TStringList.Create;
    ListOfFiles_Modified := TStringList.Create;
    try
      ListOfFiles.LineBreak := #13#10;
      ListOfFiles_Modified.LineBreak := #13#10;
      ListOfFiles.Text := MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles;
      ListOfFiles.Delete(MenuData^.PropertyItemIndex);
      MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles := ListOfFiles.Text;

      ListOfFiles_Modified.Text := MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified;
      ListOfFiles_Modified.Delete(MenuData^.PropertyItemIndex);
      MenuData^.TempEditingAction.FindSubControlOptions.MatchPrimitiveFiles_Modified := ListOfFiles_Modified.Text;

      FPrevSelectedPrimitiveNode := -1;
      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex, True);

      TriggerOnControlsModified;
      frClickerFindControl.UpdateListsOfSearchFiles(MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles, MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
    finally
      ListOfFiles.Free;
      ListOfFiles_Modified.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_MovePrimitiveFileUpInPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ListOfFiles: TStringList;
  ListOfFiles_Modified: TStringList;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    ListOfFiles := TStringList.Create;
    ListOfFiles_Modified := TStringList.Create;
    try
      ListOfFiles.LineBreak := #13#10;
      ListOfFiles_Modified.LineBreak := #13#10;
      if MenuData^.PropertyItemIndex <= 0 then
        Exit;

      ListOfFiles.Text := MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles;
      ListOfFiles.Move(MenuData^.PropertyItemIndex, MenuData^.PropertyItemIndex - 1);
      MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles := ListOfFiles.Text;

      ListOfFiles_Modified.Text := MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified;
      ListOfFiles_Modified.Move(MenuData^.PropertyItemIndex, MenuData^.PropertyItemIndex - 1);
      MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified := ListOfFiles_Modified.Text;

      Dec(FPrevSelectedPrimitiveNode);
      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex, True);

      TriggerOnControlsModified;
      frClickerFindControl.UpdateListsOfSearchFiles(MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles, MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
    finally
      ListOfFiles.Free;
      ListOfFiles_Modified.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_MovePrimitiveFileDownInPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ListOfFiles: TStringList;
  ListOfFiles_Modified: TStringList;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    ListOfFiles := TStringList.Create;
    ListOfFiles_Modified := TStringList.Create;
    try
      ListOfFiles.LineBreak := #13#10;
      ListOfFiles_Modified.LineBreak := #13#10;
      ListOfFiles.Text := MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles;
      if MenuData^.PropertyItemIndex >= ListOfFiles.Count - 1 then
        Exit;

      ListOfFiles.Move(MenuData^.PropertyItemIndex, MenuData^.PropertyItemIndex + 1);
      MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles := ListOfFiles.Text;

      ListOfFiles_Modified.Text := MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified;
      ListOfFiles_Modified.Move(MenuData^.PropertyItemIndex, MenuData^.PropertyItemIndex + 1);
      MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified := ListOfFiles_Modified.Text;

      Inc(FPrevSelectedPrimitiveNode);
      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex, True);

      TriggerOnControlsModified;
      frClickerFindControl.UpdateListsOfSearchFiles(MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles, MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
    finally
      ListOfFiles.Free;
      ListOfFiles_Modified.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.SavePrimitivesFileFromMenu(AFileIndex: Integer; AEditingAction: PClkActionRec);
var
  ListOfFiles: TStringList;
  ListOfFiles_Modified: TStringList;
  PmtvFnm: string;
begin
  ListOfFiles := TStringList.Create;
  ListOfFiles_Modified := TStringList.Create;
  try
    ListOfFiles.LineBreak := #13#10;
    ListOfFiles_Modified.LineBreak := #13#10;
    ListOfFiles.Text := AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles;

    PmtvFnm := ListOfFiles.Strings[AFileIndex];
    PmtvFnm := StringReplace(PmtvFnm, '$TemplateDir$', FFullTemplatesDir, [rfReplaceAll]);
    PmtvFnm := StringReplace(PmtvFnm, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);
    PmtvFnm := StringReplace(PmtvFnm, '$SelfTemplateDir$', ExtractFileDir(DoOnGetLoadedTemplateFileName), [rfReplaceAll]);
    PmtvFnm := EvaluateReplacements(PmtvFnm);

    DoOnAddToLog('Saving primitives file: "' + PmtvFnm + '"');

    frClickerFindControl.frClickerPrimitives.SaveFile(PmtvFnm);

    //maybe the following three lines, should be moved to the OnSave handler
    ListOfFiles_Modified.Text := AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified;
    ListOfFiles_Modified.Strings[AFileIndex] := '0';
    AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified := ListOfFiles_Modified.Text;
  finally
    ListOfFiles.Free;
    ListOfFiles_Modified.Free;
  end;
end;


procedure TfrClickerActions.MenuItem_SavePrimitiveFileInPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    SavePrimitivesFileFromMenu(MenuData^.PropertyItemIndex, MenuData^.TempEditingAction);
    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex, True);
    //TriggerOnControlsModified;  //commented, because the template is not modified by this action
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_SavePrimitiveFileAsInPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ListOfFiles: TStringList;
  ListOfFiles_Modified: TStringList;
  PmtvFnm: string;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    ListOfFiles := TStringList.Create;
    ListOfFiles_Modified := TStringList.Create;
    try
      ListOfFiles.LineBreak := #13#10;
      ListOfFiles_Modified.LineBreak := #13#10;
      ListOfFiles.Text := MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles;

      PmtvFnm := ListOfFiles.Strings[MenuData^.PropertyItemIndex];
      PmtvFnm := StringReplace(PmtvFnm, '$TemplateDir$', FFullTemplatesDir, [rfReplaceAll]);
      PmtvFnm := StringReplace(PmtvFnm, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);
      PmtvFnm := StringReplace(PmtvFnm, '$SelfTemplateDir$', ExtractFileDir(DoOnGetLoadedTemplateFileName), [rfReplaceAll]);
      PmtvFnm := EvaluateReplacements(PmtvFnm);

      DoOnSetSaveDialogInitialDir(ExtractFileDir(PmtvFnm));
      if not DoOnSaveDialogExecute(CPrimitivesDialogFilter) then
        Exit;

      ListOfFiles.Strings[MenuData^.PropertyItemIndex] := DoOnGetSaveDialogFileName;  //Let the user replace back with $AppDir$ if that's the case. The dialog doesn't know about replacements.

      frClickerFindControl.frClickerPrimitives.SaveFile(ListOfFiles.Strings[MenuData^.PropertyItemIndex]);
      MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles := ListOfFiles.Text;

      //maybe the following three lines, should be moved to the OnSave handler
      ListOfFiles_Modified.Text := MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified;
      ListOfFiles_Modified.Strings[MenuData^.PropertyItemIndex] := '0';
      MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified := ListOfFiles_Modified.Text;

      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex, True);

      TriggerOnControlsModified;
      frClickerFindControl.UpdateListsOfSearchFiles(MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles, MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
    finally
      ListOfFiles.Free;
      ListOfFiles_Modified.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_DiscardChangesAndReloadPrimitiveFileInPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ListOfFiles: TStringList;
  ListOfFiles_Modified: TStringList;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    ListOfFiles := TStringList.Create;
    ListOfFiles_Modified := TStringList.Create;
    try
      ListOfFiles.LineBreak := #13#10;
      ListOfFiles_Modified.LineBreak := #13#10;
      if MessageBox(Handle, 'Discard changes and reload?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = IDNO then
        Exit;

      ListOfFiles.Text := MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles;
      frClickerFindControl.frClickerPrimitives.LoadFile(ListOfFiles.Strings[MenuData^.PropertyItemIndex]);

      ListOfFiles_Modified.Text := MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified;
      ListOfFiles_Modified.Strings[MenuData^.PropertyItemIndex] := '0';
      MenuData^.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified := ListOfFiles_Modified.Text;

      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex, True);
      //TriggerOnControlsModified;  //commented, because the template is not modified by this action
    finally
      ListOfFiles.Free;
      ListOfFiles_Modified.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_BrowseImageSourceFromPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    //DoOnSetPictureOpenDialogInitialDir();
    if not DoOnPictureOpenDialogExecute then
      Exit;

    GetEditingActionObjectByActionType^.FindSubControlOptions.SourceFileName := DoOnGetPictureOpenDialogFileName;
    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex, True);  //this closes the editor
    TriggerOnControlsModified;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_NoImageSourceInInMemPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    MessageBox(Handle, PChar('No files in In-Mem file system. They are usually saved by the $RenderBmpExternally()$ function or by plugins.' + #13#10 + 'They can also be generated by a FindSubControl action, configured to render text only ("ExtMem:" bmp files).'), PChar(Application.Title), MB_ICONINFORMATION);
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_NoPluginInInMemPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    MessageBox(Handle, 'No plugins in In-Mem file system..', PChar(Application.Title), MB_ICONINFORMATION);
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_SetFileNameFromInMemPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  Fnm: string;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    Fnm := StringReplace(MenuData.MenuItemCaption, '&', '', [rfReplaceAll]);
    Fnm := Copy(Fnm, 1, Pos(#8#7, Fnm) - 1);

    MenuData^.TempEditingAction^.FindSubControlOptions.SourceFileName := Fnm;
    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex, True);  //this closes the editor
    TriggerOnControlsModified;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_SetFileNameFromInMemPropertyListAddToBmpFilesClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  Fnm: string;
  ListOfFiles: TStringList;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    Fnm := StringReplace(MenuData.MenuItemCaption, '&', '', [rfReplaceAll]);
    Fnm := Copy(Fnm, 1, Pos(#8#7, Fnm) - 1);

    ListOfFiles := TStringList.Create;
    try
      ListOfFiles.LineBreak := #13#10;
      ListOfFiles.Text := MenuData.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles;
      ListOfFiles.Text := ListOfFiles.Text + Fnm;
      MenuData.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles := ListOfFiles.Text;
    finally
      ListOfFiles.Free;
    end;

    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
    TriggerOnControlsModified;
    frClickerFindControl.UpdateListsOfSearchFiles(MenuData.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles, MenuData.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_SetFileNameFromInMemPropertyListUpdateBmpFilesClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  Fnm: string;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    Fnm := StringReplace(MenuData.MenuItemCaption, '&', '', [rfReplaceAll]);
    Fnm := Copy(Fnm, 1, Pos(#8#7, Fnm) - 1);

    UpdateMatchBitmapFilesItem(MenuData.TempEditingAction, MenuData^.PropertyItemIndex, Fnm);

    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
    TriggerOnControlsModified;
    frClickerFindControl.UpdateListsOfSearchFiles(MenuData.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles, MenuData.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_BrowseFileNameFromInMemPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  Fnm: string;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    Fnm := BrowseInMemFSFile(ExtRenderingInMemFS);
    if Fnm <> '' then
    begin
      MenuData^.TempEditingAction^.FindSubControlOptions.SourceFileName := Fnm;
      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex, True);  //this closes the editor
      TriggerOnControlsModified;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_BrowseFileNameFromInMemPropertyListAddToBmpFilesClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  Fnm: string;
  ListOfFiles: TStringList;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    Fnm := BrowseInMemFSFile(ExtRenderingInMemFS);
    if Fnm <> '' then
    begin
      ListOfFiles := TStringList.Create;
      try
        ListOfFiles.LineBreak := #13#10;
        ListOfFiles.Text := MenuData.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles;
        ListOfFiles.Text := ListOfFiles.Text + Fnm;
        MenuData.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles := ListOfFiles.Text;
      finally
        ListOfFiles.Free;
      end;

      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
      TriggerOnControlsModified;
      frClickerFindControl.UpdateListsOfSearchFiles(MenuData.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles, MenuData.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_BrowseFileNameFromInMemPropertyListUpdateBmpFilesClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  Fnm: string;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    Fnm := BrowseInMemFSFile(ExtRenderingInMemFS);
    if Fnm <> '' then
    begin
      UpdateMatchBitmapFilesItem(MenuData.TempEditingAction, MenuData^.PropertyItemIndex, Fnm);

      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
      TriggerOnControlsModified;
      frClickerFindControl.UpdateListsOfSearchFiles(MenuData.TempEditingAction^.FindSubControlOptions.MatchBitmapFiles, MenuData.TempEditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_SetPluginFileNameFromInMemPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  Fnm: string;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    Fnm := StringReplace(MenuData^.MenuItemCaption, '&', '', [rfReplaceAll]);
    //Fnm := Copy(Fnm, 1, Pos(#8#7, Fnm) - 1); //not used for plugin paths

    MenuData^.TempEditingAction^.PluginOptions.FileName := Fnm;
    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex, True);  //this closes the editor

    FOIFrame.CancelCurrentEditing;
    FOIFrame.Repaint;   //ideally, RepaintNodeByLevel
    TriggerOnControlsModified;

    DoOnModifyPluginProperty(MenuData^.TempEditingAction);
    tmrReloadOIContent.Enabled := True;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_SetGPUPlatformFromInMemPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  PlatformName: string;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    PlatformName := StringReplace(MenuData^.MenuItemCaption, '&', '', [rfReplaceAll]);

    if MenuData^.TempEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatformIDType = tpitIndex then
      MenuData^.TempEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatform := IntToStr(MenuData^.UserDataIndex)
    else
      MenuData^.TempEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatform := PlatformName;

    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex, True);  //this closes the editor

    FOIFrame.CancelCurrentEditing;
    FOIFrame.Repaint;   //ideally, RepaintNodeByLevel
    TriggerOnControlsModified;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_SetGPUDeviceFromInMemPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  DeviceName: string;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    DeviceName := StringReplace(MenuData^.MenuItemCaption, '&', '', [rfReplaceAll]);

    if MenuData^.TempEditingAction^.FindSubControlOptions.GPUSettings.TargetDeviceIDType = tditIndex then
      MenuData^.TempEditingAction^.FindSubControlOptions.GPUSettings.TargetDevice := IntToStr(MenuData^.UserDataIndex)
    else
      MenuData^.TempEditingAction^.FindSubControlOptions.GPUSettings.TargetDevice := DeviceName;

    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex, True);  //this closes the editor

    FOIFrame.CancelCurrentEditing;
    FOIFrame.Repaint;   //ideally, RepaintNodeByLevel
    TriggerOnControlsModified;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_BrowseOpenCLFileNameForPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    if not DoOnOpenDialogExecute({$IFDEF Windows} 'Dll files (*.dll)|*.dll|All files (*.*)|*.*' {$ELSE} 'Dll files (*.so)|*.so|All files (*.*)|*.*' {$ENDIF}) then
      Exit;

    MenuData^.TempEditingAction^.FindSubControlOptions.GPUSettings.OpenCLPath := DoOnGetOpenDialogFileName;
    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex, True);  //this closes the editor

    FOIFrame.CancelCurrentEditing;
    FOIFrame.Repaint;   //ideally, RepaintNodeByLevel
    TriggerOnControlsModified;
  finally
    Dispose(MenuData);
  end;
end;


function TfrClickerActions.GetUniqueProfileName(n: Integer): string;
var
  AttemptCount: Integer;
begin
  Result := 'Profile [' + IntToStr(n) + ']';

  AttemptCount := 0;
  while frClickerFindControl.GetFontProfileIndexByName(Result) <> -1 do
  begin
    Result := Result + 'A';
    Inc(AttemptCount);

    if AttemptCount > 1000 then
      raise Exception.Create('Can''t generate a new font profile name.');
  end;
end;


class function TfrClickerActions.DummyEvaluateReplacements(VarName: string; Recursive: Boolean = True; AEvalTextCount: Integer = -1): string; //returns VarName
begin
  Result := VarName;
end;


//Returns the index of the new item  (i.e. the previous length of MatchBitmapText array.
function TfrClickerActions.AddFontProfileToActionFromMenu(AForegroundColor, ABackgroundColor, AFontName: string; AFontSize: Integer; AFontQuality: TFontQuality): Integer;
var
  n: Integer;
  TempEditingAction: PClkActionRec;
begin
  TempEditingAction := GetEditingActionObjectByActionType;
  n := Length(TempEditingAction^.FindSubControlOptions.MatchBitmapText);
  SetLength(TempEditingAction^.FindSubControlOptions.MatchBitmapText, n + 1);

  TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].ForegroundColor := AForegroundColor;
  TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].BackgroundColor := ABackgroundColor;
  TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].FontName := AFontName;
  TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].FontSize := AFontSize;
  TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].FontQualityReplacement := '';
  TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].FontQuality := AFontQuality;
  TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].FontQualityUsesReplacement := False;
  TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].Bold := False;
  TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].Italic := False;
  TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].Underline := False;
  TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].StrikeOut := False;
  TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].CharSet := DEFAULT_CHARSET;
  TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].Orientation := 0;
  TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].Pitch := fpDefault;
  TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].CropLeft := '0';
  TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].CropTop := '0';
  TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].CropRight := '0';
  TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].CropBottom := '0';
  TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].ProfileName := GetUniqueProfileName(n);

  frClickerFindControl.AddNewFontProfile(TempEditingAction^.FindSubControlOptions.MatchBitmapText[n]);
  BuildFontColorIconsList;

  Result := n;
end;


procedure TfrClickerActions.MenuItem_AddFontProfileToPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  n: Integer;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    n := AddFontProfileToActionFromMenu('$Color_Window$', '$Color_Highlight$', 'Tahoma', 8, fqNonAntialiased);

    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
    TriggerOnControlsModified;

    FOIFrame.SelectNode(CPropertyItemLevel, MenuData^.CategoryIndex, MenuData^.PropertyIndex, n * CPropCount_FindSubControlMatchBitmapText);
    FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, MenuData^.CategoryIndex, MenuData^.PropertyIndex, n * CPropCount_FindSubControlMatchBitmapText, True, True);
    FOIFrame.FocusOI;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_AddFontProfileWithAntialiasedAndClearTypeToPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  n: Integer;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    n := AddFontProfileToActionFromMenu('$Color_WindowText$', '$Color_BtnFace$', 'Segoe UI', 9, fqAntialiased);
    n := AddFontProfileToActionFromMenu('$Color_WindowText$', '$Color_BtnFace$', 'Segoe UI', 9, fqCleartype);

    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
    TriggerOnControlsModified;

    FOIFrame.SelectNode(CPropertyItemLevel, MenuData^.CategoryIndex, MenuData^.PropertyIndex, n * CPropCount_FindSubControlMatchBitmapText);
    FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, MenuData^.CategoryIndex, MenuData^.PropertyIndex, n * CPropCount_FindSubControlMatchBitmapText, True, True);
    FOIFrame.FocusOI;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_AddFontProfileWithNonAntialiasedAndAntialiasedAndClearTypeToPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  n: Integer;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    n := AddFontProfileToActionFromMenu('$Color_WindowText$', '$Color_BtnFace$', 'Segoe UI', 9, fqNonAntialiased);
    n := AddFontProfileToActionFromMenu('$Color_WindowText$', '$Color_BtnFace$', 'Segoe UI', 9, fqAntialiased);
    n := AddFontProfileToActionFromMenu('$Color_WindowText$', '$Color_BtnFace$', 'Segoe UI', 9, fqCleartype);

    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
    TriggerOnControlsModified;

    FOIFrame.SelectNode(CPropertyItemLevel, MenuData^.CategoryIndex, MenuData^.PropertyIndex, n * CPropCount_FindSubControlMatchBitmapText);
    FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, MenuData^.CategoryIndex, MenuData^.PropertyIndex, n * CPropCount_FindSubControlMatchBitmapText, True, True);
    FOIFrame.FocusOI;
  finally
    Dispose(MenuData);
  end;
end;



procedure TfrClickerActions.MenuItem_RemoveFontProfileFromPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  n, i: Integer;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    n := Length(MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText);

    if (n = 1) and (MenuData^.CategoryIndex = CCategory_EditedAction) then //////////////////////////////// ToDo:  fix bug
    begin
      MessageBox(Handle,
                 PChar('Connot leave structure without font profiles (under EditAction). Please create the desired profile first, then delete this one.'),
                 PChar(Application.Title),
                 MB_ICONINFORMATION);
      Exit;
    end;

    if MessageBox(Handle,
                  PChar('Are you sure you want to remove font profile: ' + MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex].ProfileName),
                  PChar(Application.Title),
                  MB_ICONQUESTION + MB_YESNO) = IDNO then
      Exit;

    for i := MenuData^.PropertyItemIndex to n - 2 do
      MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText[i] := MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText[i + 1];

    SetLength(MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText, n - 1);

    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);

    frClickerFindControl.RemoveFontProfileByIndex(MenuData^.PropertyItemIndex);
    BuildFontColorIconsList;
    TriggerOnControlsModified;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_DuplicateFontProfileClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  n: Integer;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    n := Length(MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText);
    SetLength(MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText, n + 1);

    MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText[n] := MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex];
    MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText[n].ProfileName := GetUniqueProfileName(n);

    frClickerFindControl.AddNewFontProfile(MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText[n]); //AV here when using EditTemplate, because there are no font profiles
    BuildFontColorIconsList;

    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
    TriggerOnControlsModified;

    FOIFrame.SelectNode(CPropertyItemLevel, MenuData^.CategoryIndex, MenuData^.PropertyIndex, n * CPropCount_FindSubControlMatchBitmapText);
    FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, MenuData^.CategoryIndex, MenuData^.PropertyIndex, n * CPropCount_FindSubControlMatchBitmapText, True, True);
    FOIFrame.FocusOI;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_MoveFontProfileUpInPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  TempProfile: TClkFindControlMatchBitmapText;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    if MenuData^.PropertyItemIndex <= 0 then
      Exit;

    TempProfile := MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex];
    MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex] :=
      MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex - 1];

    MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex - 1] := TempProfile;

    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
    TriggerOnControlsModified;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_MoveFontProfileDownInPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  TempProfile: TClkFindControlMatchBitmapText;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    if MenuData^.PropertyItemIndex >= Length(MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText) - 1 then
      Exit;

    TempProfile := MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex];
    MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex] :=
      MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex + 1];

    MenuData^.TempEditingAction^.FindSubControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex + 1] := TempProfile;

    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
    TriggerOnControlsModified;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_BrowseSystemFontsClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  TempNewItems: string;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    TempNewItems := MenuData^.TempEditingAction.FindSubControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex].FontName;
    if EditFontProperties(MenuData^.TempEditingAction, MenuData^.PropertyItemIndex{Div}, TempNewItems) then
    begin
      FTempNewItems := TempNewItems;
      FOIFrame.CancelCurrentEditing;
      FOIFrame.Repaint;   //ideally, RepaintNodeByLevel
      TriggerOnControlsModified;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_SelectFontFromVarClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  TempFontName: string;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    TempFontName := StringReplace(MenuData^.MenuItemCaption, '&', '', [rfReplaceAll]);
    MenuData^.TempEditingAction.FindSubControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex].FontName := TempFontName;
    FTempNewItems := TempFontName;

    FOIFrame.CancelCurrentEditing;
    FOIFrame.Repaint;   //ideally, RepaintNodeByLevel
    TriggerOnControlsModified;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_BrowseSetVarFileInPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  PathToFileName: string;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    if not DoOnOpenDialogExecute('*.*') then
      Exit;

    PathToFileName := DoOnGetOpenDialogFileName;

    if ExtractFileDrive(ParamStr(0)) = ExtractFileDrive(PathToFileName) then
      PathToFileName := '$AppDir$\' + ExtractRelativePath(ExtractFilePath(ParamStr(0)), PathToFileName);

    case CurrentlyEditingActionType of
      acLoadSetVarFromFile:
        MenuData^.TempEditingAction^.LoadSetVarFromFileOptions.FileName := PathToFileName;

      acSaveSetVarToFile:
        MenuData^.TempEditingAction^.SaveSetVarToFileOptions.FileName := PathToFileName;

      else
        ;
    end;

    FOIFrame.CancelCurrentEditing;
    FOIFrame.Repaint;   //ideally, RepaintNodeByLevel
    TriggerOnControlsModified;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_BrowsePluginFileInPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  PathToFileName: string;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    if not DoOnOpenDialogExecute('Dll files (*.dll)|*.dll|All files (*.*)|*.*') then
      Exit;

    PathToFileName := DoOnGetOpenDialogFileName;

    if ExtractFileDrive(ParamStr(0)) = ExtractFileDrive(PathToFileName) then
      PathToFileName := '$AppDir$\' + ExtractRelativePath(ExtractFilePath(ParamStr(0)), PathToFileName);

    MenuData^.TempEditingAction^.PluginOptions.FileName := PathToFileName;

    FOIFrame.CancelCurrentEditing;
    FOIFrame.Repaint;   //ideally, RepaintNodeByLevel
    TriggerOnControlsModified;

    DoOnModifyPluginProperty(MenuData^.TempEditingAction);
    tmrReloadOIContent.Enabled := True;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_LoadPluginFromDiskToPluginInMemFSInPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  {$IFDEF MemPlugins}
    PathToFileName: string;
  {$ENDIF}
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    if not DoOnOpenDialogExecute('Dll files (*.dll)|*.dll|All files (*.*)|*.*') then
      Exit;

    {$IFDEF MemPlugins}
      PathToFileName := DoOnGetOpenDialogFileName;
      DoOnLoadPluginFromDiskToPluginInMemFileSystem(PathToFileName);
      DoOnLoadPluginFromDiskToPluginInMemFileSystem(ExtractFullFileNameNoExt(PathToFileName) + '.DbgSym');
    {$ENDIF}
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_BrowseEditTemplateFileInPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  PathToFileName: string;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    if not DoOnOpenDialogExecute('UIClicker template files (*.clktmpl)|*.clktmpl|All files (*.*)|*.*') then
      Exit;

    PathToFileName := DoOnGetOpenDialogFileName;

    if ExtractFileDrive(ParamStr(0)) = ExtractFileDrive(PathToFileName) then
      PathToFileName := '$AppDir$\' + ExtractRelativePath(ExtractFilePath(ParamStr(0)), PathToFileName);

    MenuData^.TempEditingAction^.EditTemplateOptions.TemplateFileName := PathToFileName;

    FOIFrame.CancelCurrentEditing;
    FOIFrame.Repaint;   //ideally, RepaintNodeByLevel
    TriggerOnControlsModified;

    DoOnModifyPluginProperty(MenuData^.TempEditingAction);
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItemControl_EdgeRefGenericClick(Sender: TObject);
var
  s: string;
  Key: Word;
begin
  try
    s := StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]);
    Delete(s, 1, 1); //delete first '$'
    s := '$' + Copy(s, 1, Pos('$', s));

    if Assigned(FLastClickedTVTEdit) then
    begin
      FLastClickedTVTEdit.Text := s;
      FOIFrame.EditingText := FLastClickedTVTEdit.Text;
      if Assigned(FLastClickedTVTEdit.OnChange) then
        FLastClickedTVTEdit.OnChange(FLastClickedTVTEdit);

      Key := Ord('A');
      if Assigned(FLastClickedTVTEdit.OnKeyDown) then
        FLastClickedTVTEdit.OnKeyDown(FLastClickedTVTEdit, Key, []);

      if Assigned(FLastClickedTVTEdit.OnKeyUp) then
        FLastClickedTVTEdit.OnKeyUp(FLastClickedTVTEdit, Key, []);   //this one seems to do it
    end;

    if Assigned(FLastClickedEdit) then
    begin
      FLastClickedEdit.Text := s;
      if Assigned(FLastClickedEdit.OnChange) then
        FLastClickedEdit.OnChange(FLastClickedEdit);
    end;
  except
    on E: Exception do
      MessageBox(Handle, PChar('EditBox is not available.' + #13#10 + E.Message), PChar(Application.MainForm.Caption), MB_ICONERROR);
  end;
end;


procedure TfrClickerActions.MenuItemCopyRefToClipboardClick(Sender: TObject);
begin
  try
    if Assigned(FLastClickedTVTEdit) then
      Clipboard.AsText := FLastClickedTVTEdit.Text;

    if Assigned(FLastClickedEdit) then
      Clipboard.AsText := FLastClickedEdit.Text;
  except
    on E: Exception do
      MessageBox(Handle, PChar('EditBox is not available.' + #13#10 + E.Message), PChar(Application.MainForm.Caption), MB_ICONERROR);
  end;
end;


procedure TfrClickerActions.MenuItemPasteRefFromClipboardClick(Sender: TObject);
begin
  try
    if Assigned(FLastClickedTVTEdit) then
    begin
      FLastClickedTVTEdit.Text := Clipboard.AsText;
      FOIFrame.EditingText := FLastClickedTVTEdit.Text;
    end;

    if Assigned(FLastClickedEdit) then
    begin
      FLastClickedEdit.Text := Clipboard.AsText;
      if Assigned(FLastClickedEdit.OnChange) then
        FLastClickedEdit.OnChange(FLastClickedEdit);
    end;
  except
    on E: Exception do
      MessageBox(Handle, PChar('EditBox is not available.' + #13#10 + E.Message), PChar(Application.MainForm.Caption), MB_ICONERROR);
  end;
end;


procedure TfrClickerActions.vstVariablesCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  TempStringEditLink: TStringEditLink;
begin
  TempStringEditLink := TStringEditLink.Create;
  EditLink := TempStringEditLink;

  FTextEditorEditBox := TEdit(TCustomEdit(TempStringEditLink.Edit));
  FTextEditorEditBox.Font.Name := 'Tahoma';
  FTextEditorEditBox.Font.Size := 8;
  //FTextEditorEditBox.Height := vstVariables.DefaultNodeHeight - 3;  //set again in timer

  FTextEditorEditBox.Show;
end;


procedure TfrClickerActions.vstVariablesEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  case Column of
    0:
      FClkVariables.Strings[Node^.Index] := FEditingText + '=' + FClkVariables.ValueFromIndex[Node^.Index];

    1:
      FClkVariables.Strings[Node^.Index] := FClkVariables.Names[Node^.Index] + '=' + FEditingText;
      //do not use FClkVariables.ValueFromIndex[Node^.Index] := FEditingText;  because it deletes items
  end;
end;


procedure TfrClickerActions.vstVariablesEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: boolean);
begin
  Allowed := True;

  case Column of
    0:
      FEditingText := FClkVariables.Names[Node^.Index];

    1:
      FEditingText := FClkVariables.ValueFromIndex[Node^.Index];
  end;
end;


procedure TfrClickerActions.vstVariablesNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: string);
begin
  FEditingText := NewText;
end;


procedure TfrClickerActions.vstVariablesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  NodeData: PVarNodeRec;
begin
  try
    if Node^.Parent = vstVariables.RootNode then
    begin
      case Column of
        0:
          CellText := FClkVariables.Names[Node^.Index];

        1:
          CellText := FClkVariables.ValueFromIndex[Node^.Index];
      end;
    end
    else
    begin
      NodeData := vstVariables.GetNodeData(Node);
      if not Assigned(NodeData) then
      begin
        CellText := 'N/A';
        Exit;
      end;

      case Column of
        0:
          CellText := NodeData^.VarName;

        1:
          CellText := NodeData^.VarValue;
      end;
    end;
  except
    CellText := '';
    tmrClkVariables.Enabled := True; //Trigger a repaint
  end;
end;


procedure TfrClickerActions.vstVariablesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  NodeData: PVarNodeRec;
begin
  vstVariables.GetHitTestInfoAt(X, Y, True, FHitInfo);

  if FHitInfo.HitColumn in [0..1] then
    if FHitInfo.HitNode^.Parent = vstVariables.RootNode then
      tmrEditClkVariables.Enabled := True
    else
    begin
      if FlblResultSelLeft = nil then
        CreateSelectionLabelsForResult;

      NodeData := vstVariables.GetNodeData(FHitInfo.HitNode);
      if NodeData = nil then
        Exit;

      SelectAreaFromDecodedVariable(NodeData, FHitInfo.HitNode^.Index);
    end;
end;


procedure TfrClickerActions.BuildFontColorIconsList;
begin
  BuildFontColorIcons(dmClickerIcons.imglstFontColorProperties, GetEditingActionObjectByActionType^.FindSubControlOptions, EvaluateReplacements);
end;


function TfrClickerActions.HandleOnOIGetCategoryCount: Integer;
begin  //
  Result := CCategoryCount;

  if FEditingAction^.ActionOptions.Action = acEditTemplate then
    Inc(Result);
end;


const
  CActionIsNil = '[Action is nil]';

function TfrClickerActions.HandleOnOIGetCategory(AIndex: Integer): string;
begin  //
  if FEditingAction = nil then
  begin
    Result := CActionIsNil;
    Exit;
  end;

  if FEditingAction^.ActionOptions.Action = acEditTemplate then
  begin
    if AIndex < CCategoryCount then
      Result := CCategories[AIndex]
    else
      Result := CCategory_Name_EditedAction;
  end
  else
    Result := CCategories[AIndex];
end;


function OIGetPropertyCount_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ACategoryIndex: Integer): Integer;
var
  EditingActionType: Integer;
begin
  if AEditingAction = nil then
  begin
    Result := 0;
    Exit;
  end;

  EditingActionType := Integer(ALiveEditingActionType);
  if EditingActionType = CClkUnsetAction then
    Result := 0 //no action is selected
  else
    Result := CMainPropCounts[EditingActionType];

  if ALiveEditingActionType = acPlugin then
    Result := Result + AEditingAction.PluginOptions.CachedCount;
end;


function TfrClickerActions.HandleOnOIGetPropertyCount(ACategoryIndex: Integer): Integer;
begin
  Result := 0;
  try
    case ACategoryIndex of
      CCategory_Common:
        Result := CPropCount_Common;

      CCategory_ActionSpecific:
        Result := OIGetPropertyCount_ActionSpecific(FEditingAction, CurrentlyEditingActionType, ACategoryIndex);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          Result := OIGetPropertyCount_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, ACategoryIndex);

      else
        Result := 0;
    end;

    if Result > 500 then   //This will hide some bugs (for now). At least it will prevent crashes and memory overuse.
      Result := 500;

    if Result < 0 then
      Result := 0;
  except
    Result := 0; //MessageBox(Handle, 'AV', 'UC HandleOnOIGetPropertyCount', 0);
  end;
end;


function OIGetPropertyName_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; APropertyIndex: Integer): string;
const
  CNotUsedStr = '   [Not used]';
var
  EditingActionType: Integer;
  ListOfProperties: TStringList;
begin
  if AEditingAction = nil then
  begin
    Result := CActionIsNil;
    Exit;
  end;

  EditingActionType := Integer(ALiveEditingActionType);
  if EditingActionType = CClkUnsetAction then
    Result := '?'
  else
  begin
    if (ALiveEditingActionType = acPlugin) and (APropertyIndex > CPlugin_FileName_PropIndex) then  //intially used AEditingAction^.ActionOptions.Action
    begin
      ListOfProperties := TStringList.Create;
      try
        ListOfProperties.LineBreak := #13#10;
        ListOfProperties.Text := AEditingAction^.PluginOptions.ListOfPropertiesAndTypes;
        try
          if (APropertyIndex - CPropCount_Plugin < ListOfProperties.Count) and (ListOfProperties.Count > 0) then
            Result := ListOfProperties.Names[APropertyIndex - CPropCount_Plugin]
          else
            Result := '[Err: Index out of bounds: ' + IntToStr(APropertyIndex - 1) + ']';
        except
          Result := 'bug on getting name';
        end;
      finally
        ListOfProperties.Free;
      end;
    end
    else
    begin
      try
        if APropertyIndex < CMainPropCounts[EditingActionType] then
          Result := CMainProperties[EditingActionType]^[APropertyIndex].Name
        else
          Result := 'bug on getting name. APropertyIndex=' + IntToStr(APropertyIndex) + '  ArrLen[' + IntToStr(EditingActionType) + ']=' + IntToStr(CMainPropCounts[EditingActionType]);
      except
        on E: Exception do
        begin
          Result := 'bug on getting name. APropertyIndex=' + IntToStr(APropertyIndex) + '  ArrLen[' + IntToStr(EditingActionType) + ']=' + IntToStr(CMainPropCounts[EditingActionType]);
          //MessageBox(0, PChar('AV' + #13#10 + Result + #13#10 + E.Message), 'UC OIGetPropertyName', 0);
        end;
      end;
    end;
  end;

  if ALiveEditingActionType = acFindControl then   //intially used AEditingAction^.ActionOptions.Action
  begin
    case APropertyIndex of
      CFindControl_MatchText_PropIndex:
        if not AEditingAction^.FindControlOptions.MatchCriteria.WillMatchText {and
           not AEditingAction^.FindControlOptions.MatchCriteria.WillMatchBitmapText} then
          Result := Result + CNotUsedStr;

      CFindControl_MatchClassName_PropIndex:
        if not AEditingAction^.FindControlOptions.MatchCriteria.WillMatchClassName then
          Result := Result + CNotUsedStr;
    end;
  end;

  if ALiveEditingActionType = acFindSubControl then   //intially used AEditingAction^.ActionOptions.Action
  begin
    if APropertyIndex = CFindSubControl_MatchBitmapText_PropIndex then
      Result := Result + ' [0..' + IntToStr(Length(AEditingAction^.FindSubControlOptions.MatchBitmapText) - 1) + ']';

    case APropertyIndex of
      CFindSubControl_MatchBitmapText_PropIndex:
        if not AEditingAction^.FindSubControlOptions.MatchCriteria.WillMatchBitmapText then
          Result := Result + CNotUsedStr;

      CFindSubControl_MatchBitmapFiles_PropIndex:
        if not AEditingAction^.FindSubControlOptions.MatchCriteria.WillMatchBitmapFiles then
          Result := Result + CNotUsedStr;

      CFindSubControl_MatchPrimitiveFiles_PropIndex:
        if not AEditingAction^.FindSubControlOptions.MatchCriteria.WillMatchPrimitiveFiles then
          Result := Result + CNotUsedStr;
    end;
  end;
end;


function TfrClickerActions.HandleOnOIGetPropertyName(ACategoryIndex, APropertyIndex: Integer): string;
begin
  Result := 'Not set';

  try
    case ACategoryIndex of
      CCategory_Common:
        Result := CCommonProperties[APropertyIndex].Name;

      CCategory_ActionSpecific:
        Result := OIGetPropertyName_ActionSpecific(FEditingAction, CurrentlyEditingActionType, APropertyIndex);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          Result := OIGetPropertyName_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, APropertyIndex);

      else
        Result := '???';
    end;
  except
    on E: Exception do
      Result := 'AV ' + E.Message;//MessageBox(Handle, 'AV', PChar('UC HandleOnOIGetPropertyName' + #13#10 + E.Message), 0);
  end;
end;


function OIGetPropertyValue_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ACategoryIndex, APropertyIndex: Integer; var APropDef: TOIPropDef): string;
var
  EditingActionType: Integer;
  ListOfProperties: TStringList;
  PropDetails: string;
begin
  if AEditingAction = nil then
  begin
    APropDef.EditorType := etNone;
    APropDef.Name := '';
    Result := CActionIsNil;
    Exit;
  end;

  try
    EditingActionType := Integer(ALiveEditingActionType);  //this was CurrentlyEditingActionType;
    if EditingActionType = CClkUnsetAction then
      APropDef.Name := '?'
    else
    begin
      if (ALiveEditingActionType = acPlugin) and (APropertyIndex > CPlugin_FileName_PropIndex) then    //initially used AEditingAction^.ActionOptions.Action
      begin
        ListOfProperties := TStringList.Create;
        try
          ListOfProperties.LineBreak := #13#10;
          ListOfProperties.Text := AEditingAction.PluginOptions.ListOfPropertiesAndTypes;

          if (APropertyIndex - CPropCount_Plugin < ListOfProperties.Count) and (ListOfProperties.Count > 0) then
          begin
            PropDetails := ListOfProperties.ValueFromIndex[APropertyIndex - CPropCount_Plugin];
            APropDef.EditorType := StrToTOIEditorType('et' + Copy(PropDetails, 1, Pos(#8#7, PropDetails) - 1));
          end
          else
            APropDef.EditorType := etUserEditor; //index out of bounds
        finally
          ListOfProperties.Free;
        end;
      end
      else
      begin
        if APropertyIndex < CMainPropCounts[EditingActionType] then
          APropDef := CMainProperties[EditingActionType]^[APropertyIndex];
      end;

      if (APropertyIndex < CMainPropCounts[EditingActionType]) or (ALiveEditingActionType = acPlugin) then    //initially used AEditingAction^.ActionOptions.Action
      begin
        Result := CMainGetActionValueStrFunctions[ALiveEditingActionType](AEditingAction, APropertyIndex);

        //if (AEditingAction^.ActionOptions.Action = acEditTemplate) and (ACategoryIndex = CCategory_EditedAction) then
        //  if APropertyIndex in [CEditTemplate_ListOfEditedProperties_PropIndex, CEditTemplate_ListOfEnabledProperties_PropIndex] then
        //    APropDef.EditorType := etTextWithArrow; //To be enabled when editing those properties work as expected.

        if (AEditingAction^.ActionOptions.Action = acEditTemplate) and (ACategoryIndex = CCategory_EditedAction) then
        begin
          case APropertyIndex of             //it would be nice to have subproperties, like In FindSubControl (Font properties), so that these lists would be properly displayed as lists of properies, instead of plain text
            CEditTemplate_ListOfEditedProperties_PropIndex:
              Result := AEditingAction^.EditTemplateOptions.ListOfEditedProperties_ET;

            CEditTemplate_ListOfEnabledProperties_PropIndex:
              Result := FastReplace_ReturnTo45(AEditingAction^.EditTemplateOptions.ListOfEnabledProperties_ET);

            else
              ;
          end;
        end;
      end
      else
        Result := '[bug. bad init]';
    end; //CClkUnsetAction
  except  //expecting some bugs on uninitialized actions
    APropDef.EditorType := etNone;
    APropDef.Name := '';
    Result := '[bug - bad init]';
  end;
end;


function TfrClickerActions.HandleOnOIGetPropertyValue(ACategoryIndex, APropertyIndex: Integer; var AEditorType: TOIEditorType): string;
var
  PropDef: TOIPropDef;
begin
  PropDef.EditorType := etNone;
  Result := '';
  try
    case ACategoryIndex of
      CCategory_Common:
      begin
        PropDef := CCommonProperties[APropertyIndex];
        Result := GetActionValueStr_Action(FEditingAction, APropertyIndex);
      end;

      CCategory_ActionSpecific:
        Result := OIGetPropertyValue_ActionSpecific(FEditingAction, CurrentlyEditingActionType, ACategoryIndex, APropertyIndex, PropDef);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          Result := OIGetPropertyValue_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, ACategoryIndex, APropertyIndex, PropDef);

      else
        PropDef.Name := '???';
    end;

    AEditorType := PropDef.EditorType;
  except
    on E: Exception do
      Result := 'AV ' + E.Message;//MessageBox(Handle, 'AV', 'UC HandleOnOIGetPropertyValue', 0);
  end;
end;


function OIGetListPropertyItemCount_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ACategoryIndex, APropertyIndex: Integer): Integer;
var
  EditingActionType: Integer;
  TempStringList: TStringList;
begin
  Result := 0;
  if AEditingAction = nil then
    Exit;

  EditingActionType := Integer(ALiveEditingActionType);
  if EditingActionType = CClkUnsetAction then
    Exit;

  case EditingActionType of
    Ord(acFindControl):
    begin
      case APropertyIndex of
        CFindControl_MatchCriteria_PropIndex:
          Result := CPropCount_FindControlMatchCriteria;

        CFindControl_InitialRectangle_PropIndex:
          Result := CPropCount_FindControlInitialRectangle;

        else
          Result := 0;
      end;
    end;

    Ord(acFindSubControl):
    begin
      case APropertyIndex of
        CFindSubControl_MatchCriteria_PropIndex:
          Result := CPropCount_FindControlMatchCriteria;

        CFindSubControl_MatchBitmapText_PropIndex:
          Result := CPropCount_FindSubControlMatchBitmapText * Length(AEditingAction^.FindSubControlOptions.MatchBitmapText);  //frClickerFindControl.GetBMPTextFontProfilesCount;

        CFindSubControl_MatchBitmapFiles_PropIndex:
        begin
          TempStringList := TStringList.Create;
          try
            TempStringList.LineBreak := #13#10;
            TempStringList.Text := AEditingAction^.FindSubControlOptions.MatchBitmapFiles;
            Result := TempStringList.Count;
          finally
            TempStringList.Free;
          end;
        end;

        CFindSubControl_MatchBitmapAlgorithmSettings_PropIndex:
          Result := CPropCount_FindSubControlMatchBitmapAlgorithmSettings;

        CFindSubControl_InitialRectangle_PropIndex:
          Result := CPropCount_FindControlInitialRectangle;

        CFindSubControl_MatchPrimitiveFiles_PropIndex:
        begin
          TempStringList := TStringList.Create;
          try
            TempStringList.LineBreak := #13#10;
            TempStringList.Text := AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles;
            Result := TempStringList.Count;
          finally
            TempStringList.Free;
          end;
        end;

        CFindSubControl_MatchByHistogramSettings_PropIndex:
          Result := CPropCount_FindSubControlMatchByHistogramSettings;

        CFindSubControl_RenderingInBrowserSettings_PropIndex:
          Result := CPropCount_FindSubControlRenderingInBrowserSettings;

        CFindSubControl_GPUSettings_PropIndex:
          Result := CPropCount_FindSubControlGPUSettings;

        CFindSubControl_ImageEffectSettings_PropIndex:
          Result := CPropCount_FindSubControlImageEffectSettings;

        else
          Result := 0;
      end;
    end;

    Ord(acCallTemplate):
    begin
      case APropertyIndex of
        CCallTemplate_CallTemplateLoop_PropIndex:
          Result := CPropCount_CallTemplateLoop;

        else
          Result := 0;
      end;
    end;
  end;   //case EditingActionType
end;


function TfrClickerActions.HandleOnOIGetListPropertyItemCount(ACategoryIndex, APropertyIndex: Integer): Integer;
begin
  Result := 0;
  if ACategoryIndex = CCategory_Common then
    Exit; //no subproperties here

  try
    case ACategoryIndex of
      CCategory_Common:
        ;

      CCategory_ActionSpecific:
        Result := OIGetListPropertyItemCount_ActionSpecific(FEditingAction, CurrentlyEditingActionType, ACategoryIndex, APropertyIndex);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          Result := OIGetListPropertyItemCount_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, ACategoryIndex, APropertyIndex);
    end;
  except
    Result := 0; //MessageBox(Handle, 'AV', 'UC HandleOnOIGetListPropertyItemCount', 0);
  end;
end;


function OIGetListPropertyItemName_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
var
  EditingActionType: Integer;
  ItemIndexMod, ItemIndexDiv: Integer;
  ListOfPrimitiveFiles_Modified: TStringList;
begin
  Result := '';

  if AEditingAction = nil then
  begin
    Result := CActionIsNil;
    Exit;
  end;

  EditingActionType := Integer(ALiveEditingActionType);
  if EditingActionType = CClkUnsetAction then
    Exit;

  case EditingActionType of
    Ord(acFindControl):
    begin
      case APropertyIndex of
        CFindControl_MatchCriteria_PropIndex:
          Result := CFindControl_MatchCriteriaProperties[AItemIndex].Name;

        CFindControl_InitialRectangle_PropIndex:
          Result := CFindControl_InitialRectangleProperties[AItemIndex].Name;
      end;
    end;

    Ord(acFindSubControl):
    begin
      case APropertyIndex of
        CFindSubControl_MatchCriteria_PropIndex:
          Result := CFindSubControl_MatchCriteriaProperties[AItemIndex].Name;

        CFindSubControl_MatchBitmapText_PropIndex:
        begin
          ItemIndexMod := AItemIndex mod CPropCount_FindSubControlMatchBitmapText;
          ItemIndexDiv := AItemIndex div CPropCount_FindSubControlMatchBitmapText;
          Result := '[' + IntToStr(ItemIndexDiv) + ']  ' + CFindSubControl_MatchBitmapTextProperties[ItemIndexMod].Name;
        end;

        CFindSubControl_MatchBitmapFiles_PropIndex:
          Result := 'File[' + IntToStr(AItemIndex) + ']';

        CFindSubControl_MatchBitmapAlgorithmSettings_PropIndex:
          Result := CFindSubControl_MatchBitmapAlgorithmSettingsProperties[AItemIndex].Name;

        CFindSubControl_InitialRectangle_PropIndex:
          Result := CFindSubControl_InitialRectangleProperties[AItemIndex].Name;

        CFindSubControl_MatchPrimitiveFiles_PropIndex:
        begin
          Result := 'File[' + IntToStr(AItemIndex) + ']';

          if AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified = '' then //this happens because this field should have been updated in FEditTemplateOptions_EditingAction, instead of FEditingAction
            Result := Result + '  ("out of sync" bug)'
          else
          begin
            ListOfPrimitiveFiles_Modified := TStringList.Create;
            try
              ListOfPrimitiveFiles_Modified.LineBreak := #13#10;
              ListOfPrimitiveFiles_Modified.Text := AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified;
              try
                if ListOfPrimitiveFiles_Modified.Strings[AItemIndex] = '1' then
                  Result := Result + '  (* Modified)';
              except
                on E: Exception do
                  Result := E.Message + '  ' + IntToStr(AItemIndex) + '   ' + IntToStr(ListOfPrimitiveFiles_Modified.Count - 1);
              end;
            finally
              ListOfPrimitiveFiles_Modified.Free;
            end;
          end;
        end;  //CFindControl_MatchPrimitiveFiles_PropIndex

        CFindSubControl_MatchByHistogramSettings_PropIndex:
          Result := CFindSubControl_MatchByHistogramSettingsProperties[AItemIndex].Name;

        CFindSubControl_RenderingInBrowserSettings_PropIndex:
          Result := CFindSubControl_RenderingInBrowserSettingsProperties[AItemIndex].Name;

        CFindSubControl_GPUSettings_PropIndex:
          Result := CFindSubControl_GPUSettingsProperties[AItemIndex].Name;

        CFindSubControl_ImageEffectSettings_PropIndex:
          Result := CFindSubControl_ImageEffectSettingsProperties[AItemIndex].Name;

        else
          Result := '';
      end;
    end;

    Ord(acCallTemplate):
    begin
      case APropertyIndex of
        CCallTemplate_CallTemplateLoop_PropIndex:
          Result := CCallTemplate_CallTemplateLoopProperties[AItemIndex].Name;

        else
          Result := '';
      end;
    end;

  end;   //case EditingActionType
end;


function TfrClickerActions.HandleOnOIGetListPropertyItemName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
begin
  Result := '';

  try
    case ACategoryIndex of
      CCategory_Common:
        ;

      CCategory_ActionSpecific:
        Result := OIGetListPropertyItemName_ActionSpecific(FEditingAction, CurrentlyEditingActionType, ACategoryIndex, APropertyIndex, AItemIndex);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          Result := OIGetListPropertyItemName_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, ACategoryIndex, APropertyIndex, AItemIndex);
    end;
  except
    Result := 'AV'; //MessageBox(Handle, 'AV', 'UC HandleOnOIGetListPropertyItemName', 0);
  end;
end;


function OIGetListPropertyItemValue_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ACategoryIndex, APropertyIndex, AItemIndex: Integer; {var AEditorType: TOIEditorType;} var APropDef: TOIPropDef): string;
var
  EditingActionType: Integer;
  TempStringList: TStringList;
begin
  Result := '';
  APropDef.EditorType := etNone; //AEditorType := etNone;

  if AEditingAction = nil then
  begin
    Result := CActionIsNil;
    Exit;
  end;

  EditingActionType := Integer(ALiveEditingActionType);
  if EditingActionType = CClkUnsetAction then
    Exit;

  case EditingActionType of
    Ord(acFindControl):
    begin
      case APropertyIndex of
        CFindControl_MatchCriteria_PropIndex:
          APropDef := CFindControl_MatchCriteriaProperties[AItemIndex];

        CFindControl_InitialRectangle_PropIndex:
          APropDef := CFindControl_InitialRectangleProperties[AItemIndex];
      end;

      Result := CFindControlGetActionValueStrFunctions[APropertyIndex](AEditingAction, AItemIndex);
    end;

    Ord(acFindSubControl):
    begin
      case APropertyIndex of
        CFindSubControl_MatchCriteria_PropIndex:
          APropDef := CFindSubControl_MatchCriteriaProperties[AItemIndex];

        CFindSubControl_MatchBitmapText_PropIndex:
          APropDef := CFindSubControl_MatchBitmapTextProperties[AItemIndex mod CPropCount_FindSubControlMatchBitmapText];

        CFindSubControl_MatchBitmapFiles_PropIndex:
        begin
          TempStringList := TStringList.Create;
          try
            TempStringList.LineBreak := #13#10;
            TempStringList.Text := AEditingAction^.FindSubControlOptions.MatchBitmapFiles;
            try
              Result := TempStringList.Strings[AItemIndex];
            except
              on E: Exception do
                Result := E.Message + '  ' + IntToStr(AItemIndex) + '   ' + IntToStr(TempStringList.Count - 1);
            end;
          finally
            TempStringList.Free;
          end;

          APropDef.EditorType := etTextWithArrow; //AEditorType := etTextWithArrow;
          Exit;
        end;

        CFindSubControl_MatchBitmapAlgorithmSettings_PropIndex:
          APropDef := CFindSubControl_MatchBitmapAlgorithmSettingsProperties[AItemIndex];

        CFindSubControl_InitialRectangle_PropIndex:
          APropDef := CFindSubControl_InitialRectangleProperties[AItemIndex];

        CFindSubControl_MatchPrimitiveFiles_PropIndex:
        begin
          TempStringList := TStringList.Create;
          try
            TempStringList.LineBreak := #13#10;
            TempStringList.Text := AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles;
            try
              if AItemIndex < TempStringList.Count then
                Result := TempStringList.Strings[AItemIndex]
              else
                Result := 'The ObjectInspector might be out of sync.';
            except
              on E: Exception do
                Result := E.Message + '  ' + IntToStr(AItemIndex) + '   ' + IntToStr(TempStringList.Count - 1) + '    The ObjectInspector might be out of sync.';
            end;
          finally
            TempStringList.Free;
          end;

          APropDef.EditorType := etTextWithArrow; //AEditorType := etTextWithArrow;
          Exit;
        end;

        CFindSubControl_MatchByHistogramSettings_PropIndex:
          APropDef := CFindSubControl_MatchByHistogramSettingsProperties[AItemIndex];

        CFindSubControl_RenderingInBrowserSettings_PropIndex:
          APropDef := CFindSubControl_RenderingInBrowserSettingsProperties[AItemIndex];

        CFindSubControl_GPUSettings_PropIndex:
          APropDef := CFindSubControl_GPUSettingsProperties[AItemIndex];

        CFindSubControl_ImageEffectSettings_PropIndex:
          APropDef := CFindSubControl_ImageEffectSettingsProperties[AItemIndex];

        else
          ;
      end;

      Result := CFindSubControlGetActionValueStrFunctions[APropertyIndex](AEditingAction, AItemIndex);
    end;

    Ord(acCallTemplate):
    begin
      case APropertyIndex of
        CCallTemplate_CallTemplateLoop_PropIndex:
          APropDef := CCallTemplate_CallTemplateLoopProperties[AItemIndex];

        else
          ;
      end;

      Result := CCallTemplateGetActionValueStrFunctions[APropertyIndex](AEditingAction, AItemIndex);
    end;
  end;   //case EditingActionType
end;


function TfrClickerActions.HandleOnOIGetListPropertyItemValue(ACategoryIndex, APropertyIndex, AItemIndex: Integer; var AEditorType: TOIEditorType): string;
var
  PropDef: TOIPropDef;
begin
  Result := '';
  AEditorType := etNone;

  try
    case ACategoryIndex of
      CCategory_Common:
        ;

      CCategory_ActionSpecific:
        Result := OIGetListPropertyItemValue_ActionSpecific(FEditingAction, CurrentlyEditingActionType, ACategoryIndex, APropertyIndex, AItemIndex, {AEditorType,} PropDef);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          Result := OIGetListPropertyItemValue_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, ACategoryIndex, APropertyIndex, AItemIndex, {AEditorType,} PropDef);
    end;
  except
    //MessageBox(Handle, 'AV', 'UC HandleOnOIGetListPropertyItemValue', 0);
  end;

  AEditorType := PropDef.EditorType;
end;


function GetPluginPropertyAttribute(AListOfPropertiesAndTypes, AAttrName: string; APropertyIndex: Integer): string;
begin
  Result := GetPluginAdditionalPropertyAttribute(AListOfPropertiesAndTypes, AAttrName, APropertyIndex - CPropCount_Plugin);
end;


function OIGetDataTypeName_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
var
  EditingActionType: Integer;
begin
  Result := '';

  if AEditingAction = nil then
  begin
    Result := CActionIsNil;
    Exit;
  end;

  EditingActionType := Integer(ALiveEditingActionType);

  if EditingActionType = CClkUnsetAction then
    Result := '?'
  else
  begin
    if AItemIndex = -1 then
    begin
      if (ALiveEditingActionType = acPlugin) and (APropertyIndex > CPlugin_FileName_PropIndex) then
        Result := GetPluginPropertyAttribute(AEditingAction.PluginOptions.ListOfPropertiesAndTypes, CPluginPropertyAttr_DataType, APropertyIndex)
      else
        Result := CMainProperties[EditingActionType]^[APropertyIndex].DataType
    end
    else
    begin
      case EditingActionType of
        Ord(acFindControl):
        begin
          case APropertyIndex of
            CFindControl_MatchCriteria_PropIndex:
              Result := CFindControl_MatchCriteriaProperties[AItemIndex].DataType;

            CFindControl_InitialRectangle_PropIndex:
              Result := CFindControl_InitialRectangleProperties[AItemIndex].DataType;
          end;
        end;

        Ord(acFindSubControl):
        begin
          case APropertyIndex of
            CFindSubControl_MatchCriteria_PropIndex:
              Result := CFindSubControl_MatchCriteriaProperties[AItemIndex].DataType;

            CFindSubControl_MatchBitmapText_PropIndex:
              Result := CFindSubControl_MatchBitmapTextProperties[AItemIndex mod CPropCount_FindSubControlMatchBitmapText].DataType;

            CFindSubControl_MatchBitmapFiles_PropIndex:
              Result := CDTString;

            CFindSubControl_MatchBitmapAlgorithmSettings_PropIndex:
              Result := CFindSubControl_MatchBitmapAlgorithmSettingsProperties[AItemIndex].DataType;

            CFindSubControl_InitialRectangle_PropIndex:
              Result := CFindSubControl_InitialRectangleProperties[AItemIndex].DataType;

            CFindSubControl_MatchPrimitiveFiles_PropIndex:
              Result := CDTString;

            CFindSubControl_MatchByHistogramSettings_PropIndex:
              Result := CFindSubControl_MatchByHistogramSettingsProperties[AItemIndex].DataType;

            CFindSubControl_RenderingInBrowserSettings_PropIndex:
              Result := CFindSubControl_RenderingInBrowserSettingsProperties[AItemIndex].DataType;

            CFindSubControl_GPUSettings_PropIndex:
              Result := CFindSubControl_GPUSettingsProperties[AItemIndex].DataType;

            CFindSubControl_ImageEffectSettings_PropIndex:
              Result := CFindSubControl_ImageEffectSettingsProperties[AItemIndex].DataType;
          end;
        end;

        Ord(acCallTemplate):
          if APropertyIndex = CCallTemplate_CallTemplateLoop_PropIndex then
            Result := CCallTemplate_CallTemplateLoopProperties[AItemIndex].DataType;
      end;
    end; //else
  end;
end;


function TfrClickerActions.HandleOnOIGetDataTypeName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
begin  //
  Result := '';
  try
    case ACategoryIndex of
      CCategory_Common:
        Result := CCommonProperties[APropertyIndex].DataType;

      CCategory_ActionSpecific:
        Result := OIGetDataTypeName_ActionSpecific(FEditingAction, CurrentlyEditingActionType, ACategoryIndex, APropertyIndex, AItemIndex);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          Result := OIGetDataTypeName_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, ACategoryIndex, APropertyIndex, AItemIndex);

      else
        Result := '???';
    end;
  except
    Result := 'AV'; //MessageBox(Handle, 'AV', 'UC HandleOnOIGetDataTypeName', 0);
  end;
end;


function TfrClickerActions.HandleOnOIGetExtraInfo(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
begin
  Result := 'extra';
end;


procedure TfrClickerActions.OIGetImageIndexEx_ActionSpecific(ALiveEditingActionType: TClkAction; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);
var
  ItemIndexMod, ItemIndexDiv: Integer;
  EditingActionType: Integer;
begin
  EditingActionType := Integer(ALiveEditingActionType);

  if Column = 0 then
  begin
    case ANodeLevel of
      CCategoryLevel:
      begin
        ImageList := dmClickerIcons.imglstActions16;
        ImageIndex := EditingActionType;
      end;

      CPropertyLevel:
      begin
        ImageIndex := APropertyIndex;

        case EditingActionType of
          Ord(acClick):
            ImageList := dmClickerIcons.imglstClickProperties;

          Ord(acExecApp):
            ImageList := dmClickerIcons.imglstExecAppProperties;

          Ord(acFindControl):
            ImageList := dmClickerIcons.imglstFindControlProperties;

          Ord(acFindSubControl):
            ImageList := dmClickerIcons.imglstFindSubControlProperties;

          Ord(acSetControlText):
            ImageList := dmClickerIcons.imglstSetTextProperties;

          Ord(acCallTemplate):
            ImageList := dmClickerIcons.imglstCallTemplateProperties;

          Ord(acSleep):
            ImageList := dmClickerIcons.imglstSleepProperties;

          Ord(acSetVar):
            ImageList := dmClickerIcons.imglstSetVarProperties;

          Ord(acWindowOperations):
            ImageList := dmClickerIcons.imglstWindowOperationsProperties;

          Ord(acLoadSetVarFromFile):
            ImageList := dmClickerIcons.imglstLoadSetVarFromFileProperties;

          Ord(acSaveSetVarToFile):
            ImageList := dmClickerIcons.imglstSaveSetVarToFileProperties;

          Ord(acPlugin):
          begin
            ImageList := dmClickerIcons.imglstPluginProperties;
            //if APropertyIndex > CPlugin_FileName_PropIndex then
            //  ImageIndex := 1;
          end;

          Ord(acEditTemplate):
            ImageList := dmClickerIcons.imglstEditTemplateProperties;
        end;   //case
      end;

      CPropertyItemLevel:
      begin
        ImageIndex := AItemIndex;

        case EditingActionType of
          Ord(acFindControl):
          begin
            case APropertyIndex of
              CFindControl_MatchCriteria_PropIndex:
                ImageList := dmClickerIcons.imglstFCMatchCriteriaProperties;

              CFindControl_InitialRectangle_PropIndex:
                ImageList := dmClickerIcons.imglstInitialRectangleProperties;
            end;
          end;

          Ord(acFindSubControl):
          begin
            case APropertyIndex of
              CFindSubControl_MatchCriteria_PropIndex:
                ImageList := dmClickerIcons.imglstFSCMatchCriteriaProperties;

              CFindSubControl_MatchBitmapText_PropIndex:
              begin
                ItemIndexMod := AItemIndex mod CPropCount_FindSubControlMatchBitmapText;
                ImageIndex := ItemIndexMod;
                ImageList := dmClickerIcons.imglstMatchBitmapTextProperties;
              end;

              CFindSubControl_MatchBitmapAlgorithmSettings_PropIndex:
                ImageList := dmClickerIcons.imglstMatchBitmapAlgorithmSettingsProperties;

              CFindSubControl_InitialRectangle_PropIndex:
                ImageList := dmClickerIcons.imglstInitialRectangleProperties;

              CFindSubControl_MatchByHistogramSettings_PropIndex:
                ImageList := dmClickerIcons.imglstMatchByHistogramSettingsProperties;

              CFindSubControl_RenderingInBrowserSettings_PropIndex:
                ImageList := dmClickerIcons.imglstRenderingInBrowserSettingsProperties;

              CFindSubControl_GPUSettings_PropIndex:
                ImageList := dmClickerIcons.imglstGPUSettingsProperties;

              CFindSubControl_ImageEffectSettings_PropIndex:
                ImageList := dmClickerIcons.imglstImageEffectSettingsProperties;
            end;
          end;

          Ord(acCallTemplate):
            ImageList := dmClickerIcons.imglstCallTemplateLoopProperties;
        end;
      end;
    end; //case
  end; // Column = 0

  if Column = 1 then
  begin
    if ANodeLevel = CPropertyItemLevel then
    begin
      if EditingActionType = Ord(acFindSubControl) then
        if APropertyIndex = CFindSubControl_MatchBitmapText_PropIndex then
        begin
          ItemIndexMod := AItemIndex mod CPropCount_FindSubControlMatchBitmapText;
          ItemIndexDiv := AItemIndex div CPropCount_FindSubControlMatchBitmapText;

          if ItemIndexMod in [CFindSubControl_MatchBitmapText_ForegroundColor_PropItemIndex, CFindSubControl_MatchBitmapText_BackgroundColor_PropItemIndex] then
          begin
            ImageList := dmClickerIcons.imglstFontColorProperties;
            ImageIndex := ItemIndexDiv shl 1 + ItemIndexMod;

            if ImageIndex > dmClickerIcons.imglstFontColorProperties.Count - 1 then
              BuildFontColorIconsList;
          end;
        end;
    end;

    if ANodeLevel = CPropertyLevel then
    begin
      if ((ALiveEditingActionType = acEditTemplate) and (ACategoryIndex = CCategory_ActionSpecific)) or
         (ACategoryIndex = CCategory_EditedAction) then   //works for EditTemplate only
      begin
        case APropertyIndex of
          CEditTemplate_EditedActionType_PropIndex:
          begin
            ImageList := dmClickerIcons.imglstActions16;

            case ACategoryIndex of
              CCategory_ActionSpecific:
                ImageIndex := Ord(FEditingAction^.EditTemplateOptions.EditedActionType);

              CCategory_EditedAction:
                if FEditingAction^.EditTemplateOptions.EditedActionType = acEditTemplate then
                  ImageIndex := Ord(FEditTemplateOptions_EditingAction^.EditTemplateOptions.EditedActionType);

              else
                ;
            end;
          end;

          CEditTemplate_Operation_PropIndex:
          begin
            ImageList := dmClickerIcons.imglstEditTemplateOperationProperties;

            case ACategoryIndex of
              CCategory_ActionSpecific:
                ImageIndex := Ord(FEditingAction^.EditTemplateOptions.Operation);

              CCategory_EditedAction:
                if FEditingAction^.EditTemplateOptions.EditedActionType = acEditTemplate then
                  ImageIndex := Ord(FEditTemplateOptions_EditingAction^.EditTemplateOptions.Operation);

              else
                ;
            end;
          end;

          CEditTemplate_WhichTemplate_PropIndex:
          begin
            ImageList := dmClickerIcons.imglstEditTemplateWhichTemplateProperties;

            case ACategoryIndex of
              CCategory_ActionSpecific:
                ImageIndex := Ord(FEditingAction^.EditTemplateOptions.WhichTemplate);

              CCategory_EditedAction:
                if FEditingAction^.EditTemplateOptions.EditedActionType = acEditTemplate then
                  ImageIndex := Ord(FEditTemplateOptions_EditingAction^.EditTemplateOptions.WhichTemplate);

              else
                ;
            end;
          end;

          else
            ;
        end; //case
      end; /// EditTemplate icons

      if ALiveEditingActionType = acCallTemplate then
        if APropertyIndex = CCallTemplate_CallTemplateLoop_PropIndex then
          if ((ACategoryIndex = CCategory_ActionSpecific) and FEditingAction^.CallTemplateOptions.CallTemplateLoop.Enabled) or
             ((ACategoryIndex = CCategory_EditedAction) and FEditTemplateOptions_EditingAction^.CallTemplateOptions.CallTemplateLoop.Enabled) then
          begin
            ImageList := dmClickerIcons.imglstCallTemplateLoopProperties;
            ImageIndex := CCallTemplate_CallTemplateLoop_Enabled_PropItemIndex;
          end;
    end; //CPropertyLevel

  end; //Column = 1
end;


procedure TfrClickerActions.HandleOnOIGetImageIndexEx(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);
var
  EditingActionType: Integer;
begin  //
  EditingActionType := Integer(CurrentlyEditingActionType);
  try
    case ACategoryIndex of
      CCategory_Common:
        if Column = 0 then
        begin
          ImageList := dmClickerIcons.imglstActionProperties;
          ImageIndex := APropertyIndex;
        end;

      CCategory_ActionSpecific:
      begin
        if EditingActionType = CClkUnsetAction then
          Exit;

        OIGetImageIndexEx_ActionSpecific(CurrentlyEditingActionType, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, Kind, Column, Ghosted, ImageIndex, ImageList);
      end;

      CCategory_EditedAction:
      begin
        if EditingActionType = CClkUnsetAction then
          Exit;

        if FEditTemplateOptions_EditingAction <> nil then
          OIGetImageIndexEx_ActionSpecific(FEditTemplateOptions_EditingAction.ActionOptions.Action, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, Kind, Column, Ghosted, ImageIndex, ImageList);
      end;
    end;
  except
    ImageIndex := -1; //MessageBox(Handle, 'AV', 'UC HandleOnOIGetImageIndexEx', 0);
  end;
end;


procedure TfrClickerActions.UpdateGPUTargetPlatformByType(AOldText: string; AEditingAction: PClkActionRec; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; ANewText: string);
var
  DummyPopupMenu: TPopupMenu;
  TempCaption: string;
  TempPlatformIndex: Integer;
  i: Integer;
begin
  DummyPopupMenu := TPopupMenu.Create(nil);  //use a pop-up menu, to get the list of platforms:
  try
    AddGPUPlatformsAsMenuItems(DummyPopupMenu, nil, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
    if (AOldText <> CTargetPlatformIDTypeStr[tpitIndex]) and
       (AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatformIDType = tpitIndex) then //from text to index
    begin
      DoOnAddToLog('Converting TargetPlatform from text to index..');
      for i := 0 to DummyPopupMenu.Items.Count - 1 do
      begin
        TempCaption := StringReplace(DummyPopupMenu.Items[i].Caption, '&', '', [rfReplaceAll]);

        if ((TempCaption = AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatform) and (AOldText = CTargetPlatformIDTypeStr[tpitFullNameMatchCase])) or
           ((UpperCase(TempCaption) = UpperCase(AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatform)) and (AOldText = CTargetPlatformIDTypeStr[tpitFullNameNoCase])) or
           ((Pos(AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatform, TempCaption) > 0) and (AOldText = CTargetPlatformIDTypeStr[tpitPartialNameMatchCase])) or
           ((Pos(UpperCase(AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatform), UpperCase(TempCaption)) > 0) and (AOldText = CTargetPlatformIDTypeStr[tpitPartialNameNoCase])) then
        begin
          DoOnAddToLog('From ' + AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatform + ' to ' + IntToStr(i));
          AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatform := IntToStr(i);
          Break;
        end;
      end;
    end;  //from text to index

    if (AOldText = CTargetPlatformIDTypeStr[tpitIndex]) and
       (AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatformIDType <> tpitIndex) then //from index to text
    begin
      DoOnAddToLog('Converting TargetPlatform from index to text..');
      TempPlatformIndex := StrToIntDef(AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatform, -1);
      if (TempPlatformIndex < 0) or (TempPlatformIndex > DummyPopupMenu.Items.Count - 1) then
        TempPlatformIndex := -1; //even if Count may be 0

      if (DummyPopupMenu.Items.Count > 0) and (TempPlatformIndex <> -1) then //in case of an empty menu, this will lead to AV
      begin
        TempCaption := StringReplace(DummyPopupMenu.Items[TempPlatformIndex].Caption, '&', '', [rfReplaceAll]);
        DoOnAddToLog('From ' + IntToStr(TempPlatformIndex) + ' to ' + TempCaption);
        AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatform := TempCaption;
      end;
    end;  //from index to text
  finally
    DummyPopupMenu.Free;
  end;
end;


procedure TfrClickerActions.UpdateGPUTargetDeviceByType(AOldText: string; AEditingAction: PClkActionRec; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; ANewText: string);
var
  DummyPopupMenu: TPopupMenu;
  TempCaption: string;
  TempPlatformIndex, TempDeviceIndex: Integer;
  i, j: Integer;
begin
  DummyPopupMenu := TPopupMenu.Create(nil);  //use a pop-up menu, to get the list of platforms:
  try
    AddGPUDevicesAsMenuItems(DummyPopupMenu, nil, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

    //The proper TargetPlatform has to be identified, based on TargetPlatformIDType.
    if AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatformIDType = tpitIndex then
    begin
      TempPlatformIndex := StrToIntDef(AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatform, -1);
      if (TempPlatformIndex < 0) or (TempPlatformIndex > DummyPopupMenu.Items.Count - 1) then
        TempPlatformIndex := -1; //even if Count may be 0
    end
    else
    begin
      TempPlatformIndex := -1;
      for i := 0 to DummyPopupMenu.Items.Count - 1 do
      begin
        TempCaption := StringReplace(DummyPopupMenu.Items[i].Caption, '&', '', [rfReplaceAll]);

        if ((TempCaption = AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatform) and (AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatformIDType = tpitFullNameMatchCase)) or
           ((UpperCase(TempCaption) = UpperCase(AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatform)) and (AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatformIDType = tpitFullNameNoCase)) or
           ((Pos(AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatform, TempCaption) > 0) and (AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatformIDType = tpitPartialNameMatchCase)) or
           ((Pos(UpperCase(AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatform), UpperCase(TempCaption)) > 0) and (AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatformIDType = tpitPartialNameNoCase)) then
        begin
          TempPlatformIndex := i;
          Break;
        end;
      end; //for
    end; //text, not index

    if (DummyPopupMenu.Items.Count > 0) and (TempPlatformIndex <> -1) then //in case of an empty menu, this will lead to AV
    begin
      if (AOldText <> CTargetDeviceIDTypeStr[tditIndex]) and
         (AEditingAction^.FindSubControlOptions.GPUSettings.TargetDeviceIDType = tditIndex) then //from text to index
      begin
        DoOnAddToLog('Converting TargetDevice from text to index..');
        for j := 0 to DummyPopupMenu.Items[TempPlatformIndex].Count - 1 do
        begin
          TempCaption := StringReplace(DummyPopupMenu.Items[TempPlatformIndex].Items[j].Caption, '&', '', [rfReplaceAll]);

          if ((TempCaption = AEditingAction^.FindSubControlOptions.GPUSettings.TargetDevice) and (AOldText = CTargetDeviceIDTypeStr[tditFullNameMatchCase])) or
             ((UpperCase(TempCaption) = UpperCase(AEditingAction^.FindSubControlOptions.GPUSettings.TargetDevice)) and (AOldText = CTargetDeviceIDTypeStr[tditFullNameNoCase])) or
             ((Pos(AEditingAction^.FindSubControlOptions.GPUSettings.TargetDevice, TempCaption) > 0) and (AOldText = CTargetDeviceIDTypeStr[tditPartialNameMatchCase])) or
             ((Pos(UpperCase(AEditingAction^.FindSubControlOptions.GPUSettings.TargetDevice), UpperCase(TempCaption)) > 0) and (AOldText = CTargetDeviceIDTypeStr[tditPartialNameNoCase])) then
          begin
            DoOnAddToLog('From ' + AEditingAction^.FindSubControlOptions.GPUSettings.TargetDevice + ' to ' + IntToStr(j));
            AEditingAction^.FindSubControlOptions.GPUSettings.TargetDevice := IntToStr(j);
            Break;
          end;
        end;
      end;

      if (AOldText = CTargetDeviceIDTypeStr[tditIndex]) and
         (AEditingAction^.FindSubControlOptions.GPUSettings.TargetDeviceIDType <> tditIndex) then //from index to text
      begin
        DoOnAddToLog('Converting TargetDevice from index to text..');
        TempDeviceIndex := StrToIntDef(AEditingAction^.FindSubControlOptions.GPUSettings.TargetDevice, -1);
        if (TempDeviceIndex < 0) or (TempDeviceIndex > DummyPopupMenu.Items[TempPlatformIndex].Count - 1) then
          TempDeviceIndex := -1; //even if Count may be 0

        if (DummyPopupMenu.Items[TempPlatformIndex].Count > 0) and (TempDeviceIndex <> -1) then //in case of an empty menu, this will lead to AV
        begin
          TempCaption := StringReplace(DummyPopupMenu.Items[TempPlatformIndex].Items[TempDeviceIndex].Caption, '&', '', [rfReplaceAll]);
          DoOnAddToLog('From ' + IntToStr(TempDeviceIndex) + ' to ' + TempCaption);
          AEditingAction^.FindSubControlOptions.GPUSettings.TargetDevice := TempCaption;
        end;
      end;
    end; //valid menu
  finally
    DummyPopupMenu.Free;
  end;
end;


procedure TfrClickerActions.OIEditedText_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; ANewText: string);
var
  EditingActionType: Integer;
  TempStringList, ListOfFiles_Modified: TStringList;
  ItemIndexMod, ItemIndexDiv: Integer;
  FoundProfileIndex, i, ImageIndex: Integer;
  OldText: string;
begin
  if AEditingAction = nil then
    Exit;

  EditingActionType := Integer(ALiveEditingActionType);
  if EditingActionType = CClkUnsetAction then
    Exit;

  case EditingActionType of
    Ord(acFindControl):
    begin
      case APropertyIndex of
        CFindControl_MatchCriteria_PropIndex:
        begin
          OldText := GetActionValueStr_FindControl_MatchCriteria(AEditingAction, AItemIndex);
          SetActionValueStr_FindControl_MatchCriteria(AEditingAction, ANewText, AItemIndex);
          TriggerOnControlsModified(ANewText <> OldText);
          Exit;
        end;

        CFindControl_InitialRectangle_PropIndex:
        begin
          OldText := GetActionValueStr_FindControl_InitialRectangle(AEditingAction, AItemIndex);
          SetActionValueStr_FindControl_InitialRectangle(AEditingAction, ANewText, AItemIndex);
          TriggerOnControlsModified(ANewText <> OldText);
          Exit;
        end;
      end;
    end;

    Ord(acFindSubControl):
    begin
      case APropertyIndex of
        CFindSubControl_MatchCriteria_PropIndex:
        begin
          OldText := GetActionValueStr_FindSubControl_MatchCriteria(AEditingAction, AItemIndex);
          SetActionValueStr_FindSubControl_MatchCriteria(AEditingAction, ANewText, AItemIndex);
          TriggerOnControlsModified(ANewText <> OldText);
          Exit;
        end;

        CFindSubControl_MatchText_PropIndex:
          frClickerFindControl.UpdateOnTextPropeties;

        CFindSubControl_MatchBitmapText_PropIndex:
        begin
          OldText := GetActionValueStr_FindSubControl_MatchBitmapText(AEditingAction, AItemIndex {no mod here});

          if ANodeLevel = CPropertyItemLevel then
          begin
            ItemIndexMod := AItemIndex mod CPropCount_FindSubControlMatchBitmapText;
            ItemIndexDiv := AItemIndex div CPropCount_FindSubControlMatchBitmapText;

            case ItemIndexMod of
              CFindSubControl_MatchBitmapText_ForegroundColor_PropItemIndex, CFindSubControl_MatchBitmapText_BackgroundColor_PropItemIndex:
              begin
                imgFontColorBuffer.Canvas.Pen.Color := 1;
                imgFontColorBuffer.Canvas.Brush.Color := HexToInt(EvaluateReplacements(ANewText));
                imgFontColorBuffer.Canvas.Rectangle(0, 0, imgFontColorBuffer.Width, imgFontColorBuffer.Height);

                ImageIndex := ItemIndexDiv shl 1 + ItemIndexMod;    //shl 1 means that there are two items / pair  (FG and BG)
                if ImageIndex > dmClickerIcons.imglstFontColorProperties.Count - 1 then
                  BuildFontColorIconsList;

                dmClickerIcons.imglstFontColorProperties.ReplaceMasked(ImageIndex, imgFontColorBuffer.Picture.Bitmap, 2);
              end;

              CFindSubControl_MatchBitmapText_CropLeft_PropItemIndex:
              begin
                if StrToIntDef(ANewText, 0) < 0 then
                  ANewText := '0';

                AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropLeft := ANewText;
                frClickerFindControl.BMPTextFontProfiles[ItemIndexDiv].UpdateSelectionLabelsFromCropInfo(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv]);
              end;

              CFindSubControl_MatchBitmapText_CropTop_PropItemIndex:
              begin
                if StrToIntDef(ANewText, 0) < 0 then
                  ANewText := '0';

                AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropTop := ANewText;
                frClickerFindControl.BMPTextFontProfiles[ItemIndexDiv].UpdateSelectionLabelsFromCropInfo(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv]);
              end;

              CFindSubControl_MatchBitmapText_CropRight_PropItemIndex:
              begin
                if StrToIntDef(ANewText, 0) < 0 then
                  ANewText := '0';

                AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropRight := ANewText;
                frClickerFindControl.BMPTextFontProfiles[ItemIndexDiv].UpdateSelectionLabelsFromCropInfo(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv]);
              end;

              CFindSubControl_MatchBitmapText_CropBottom_PropItemIndex:
              begin
                if StrToIntDef(ANewText, 0) < 0 then
                  ANewText := '0';

                AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropBottom := ANewText;
                frClickerFindControl.BMPTextFontProfiles[ItemIndexDiv].UpdateSelectionLabelsFromCropInfo(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv]);
              end;

              CFindSubControl_MatchBitmapText_ProfileName_PropItemIndex:
              begin
                FoundProfileIndex := frClickerFindControl.GetFontProfileIndexByName(ANewText);
                if (FoundProfileIndex > -1) and (FoundProfileIndex <> ItemIndexDiv) then
                  raise Exception.Create('Font profile name already exists. Please use a different one.');
              end;
            end; //case  ItemIndexMod
          end;

          SetActionValueStr_FindSubControl_MatchBitmapText(AEditingAction, ANewText, AItemIndex {no mod here});
          frClickerFindControl.PreviewText;

          for i := 0 to Length(AEditingAction^.FindSubControlOptions.MatchBitmapText) - 1 do
            frClickerFindControl.BMPTextFontProfiles[i].UpdateSelectionLabelsFromCropInfo(AEditingAction^.FindSubControlOptions.MatchBitmapText[i]);

          TriggerOnControlsModified(ANewText <> OldText);
          frClickerFindControl.UpdateOnTextPropeties;
          Exit;
        end;  //CFindControl_MatchBitmapText_PropIndex

        CFindSubControl_MatchBitmapFiles_PropIndex:
        begin
          TempStringList := TStringList.Create;
          try
            TempStringList.LineBreak := #13#10;
            case ANodeLevel of
              CPropertyLevel:
              begin
                OldText := AEditingAction^.FindSubControlOptions.MatchBitmapFiles;
                AEditingAction^.FindSubControlOptions.MatchBitmapFiles := ANewText;
                FOIFrame.ReloadPropertyItems(ACategoryIndex, APropertyIndex);

                TriggerOnControlsModified(ANewText <> OldText);
                frClickerFindControl.UpdateListsOfSearchFiles(EditingAction^.FindSubControlOptions.MatchBitmapFiles, EditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
              end;

              CPropertyItemLevel:
              begin
                TempStringList.Text := AEditingAction^.FindSubControlOptions.MatchBitmapFiles;    //read
                OldText := TempStringList.Strings[AItemIndex];
                TempStringList.Strings[AItemIndex] := ANewText;                                //modify
                AEditingAction^.FindSubControlOptions.MatchBitmapFiles := TempStringList.Text;    //write

                TriggerOnControlsModified(ANewText <> OldText);
                frClickerFindControl.UpdateListsOfSearchFiles(EditingAction^.FindSubControlOptions.MatchBitmapFiles, EditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
              end;
            end;
          finally
            TempStringList.Free;
          end;

          Exit;
        end;

        CFindSubControl_MatchBitmapAlgorithmSettings_PropIndex:
        begin
          OldText := GetActionValueStr_FindSubControl_MatchBitmapAlgorithmSettings(AEditingAction, AItemIndex);
          SetActionValueStr_FindSubControl_MatchBitmapAlgorithmSettings(AEditingAction, ANewText, AItemIndex);
          TriggerOnControlsModified(ANewText <> OldText);
          Exit;
        end;

        CFindSubControl_InitialRectangle_PropIndex:
        begin
          OldText := GetActionValueStr_FindSubControl_InitialRectangle(AEditingAction, AItemIndex);
          SetActionValueStr_FindSubControl_InitialRectangle(AEditingAction, ANewText, AItemIndex);
          TriggerOnControlsModified(ANewText <> OldText);
          Exit;
        end;

        CFindSubControl_UseWholeScreen_PropIndex:   //this call will have to take into account, the screen edges or vars as search area limits
        begin
          frClickerFindControl.UpdateSearchAreaLabelsFromKeysOnInitRect(AEditingAction^.FindControlOptions.InitialRectangle);
          frClickerFindControl.UpdateUseWholeScreenLabel(StrToBool(ANewText));
        end;

        CFindSubControl_MatchPrimitiveFiles_PropIndex:
        begin
          TempStringList := TStringList.Create;
          try
            TempStringList.LineBreak := #13#10;
            case ANodeLevel of
              CPropertyLevel:
              begin
                OldText := AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles;
                AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles := ANewText;

                AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified := '';
                ListOfFiles_Modified := TStringList.Create;
                try
                  ListOfFiles_Modified.LineBreak := #13#10;
                  ListOfFiles_Modified.Text := AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles;
                  for i := 0 to ListOfFiles_Modified.Count - 1 do
                    AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified := AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified + '0'#13#10;
                finally
                  ListOfFiles_Modified.Free;
                end;

                FOIFrame.ReloadPropertyItems(ACategoryIndex, APropertyIndex);

                TriggerOnControlsModified(ANewText <> OldText);
                frClickerFindControl.UpdateListsOfSearchFiles(EditingAction^.FindSubControlOptions.MatchBitmapFiles, EditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
              end;

              CPropertyItemLevel:
              begin
                TempStringList.Text := AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles;    //read
                OldText := TempStringList.Strings[AItemIndex];
                TempStringList.Strings[AItemIndex] := ANewText;                                   //modify
                AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles := TempStringList.Text;    //write

                TriggerOnControlsModified(ANewText <> OldText);
                frClickerFindControl.UpdateListsOfSearchFiles(EditingAction^.FindSubControlOptions.MatchBitmapFiles, EditingAction^.FindSubControlOptions.MatchPrimitiveFiles);
              end;
            end;
          finally
            TempStringList.Free;
          end;

          Exit;
        end;

        CFindSubControl_MatchByHistogramSettings_PropIndex:
        begin
          OldText := GetActionValueStr_FindSubControl_MatchByHistogramSettings(AEditingAction, AItemIndex);
          SetActionValueStr_FindSubControl_MatchByHistogramSettings(AEditingAction, ANewText, AItemIndex);
          TriggerOnControlsModified(ANewText <> OldText);
          Exit;
        end;

        CFindSubControl_RenderingInBrowserSettings_PropIndex:
        begin
          OldText := GetActionValueStr_FindSubControl_RenderingInBrowserSettings(AEditingAction, AItemIndex);
          SetActionValueStr_FindSubControl_RenderingInBrowserSettings(AEditingAction, ANewText, AItemIndex);
          TriggerOnControlsModified(ANewText <> OldText);
          Exit;
        end;

        CFindSubControl_GPUSettings_PropIndex:
        begin
          OldText := GetActionValueStr_FindSubControl_GPUSettings(AEditingAction, AItemIndex);
          SetActionValueStr_FindSubControl_GPUSettings(AEditingAction, ANewText, AItemIndex);
          TriggerOnControlsModified(ANewText <> OldText);

          case AItemIndex of
            CFindSubControl_GPUSettings_TargetPlatformIDType_PropItemIndex:
              UpdateGPUTargetPlatformByType(OldText, AEditingAction, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, ANewText);

            CFindSubControl_GPUSettings_TargetDeviceIDType_PropItemIndex:
              UpdateGPUTargetDeviceByType(OldText, AEditingAction, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, ANewText);

            else
              ;
          end; //case AItemIndex

          Exit;
        end;  //CFindSubControl_GPUSettings_PropIndex

        CFindSubControl_ImageEffectSettings_PropIndex:
        begin
          OldText := GetActionValueStr_FindSubControl_ImageEffectSettings(AEditingAction, AItemIndex);
          SetActionValueStr_FindSubControl_ImageEffectSettings(AEditingAction, ANewText, AItemIndex);
          TriggerOnControlsModified(ANewText <> OldText);
        end

        else
          ;
      end;
    end; //FindControl  case

    Ord(acCallTemplate):
    begin
      case APropertyIndex of
        CCallTemplate_CallTemplateLoop_PropIndex:
        begin
          OldText := GetActionValueStr_CallTemplate_CallTemplateLoop(AEditingAction, AItemIndex);
          SetActionValueStr_CallTemplate_CallTemplateLoop(AEditingAction, ANewText, AItemIndex);
          TriggerOnControlsModified(ANewText <> OldText);
          Exit;
        end;

        else
          ;
      end;
    end; //CallTemplate  case

    Ord(acEditTemplate):
    begin
      //if AEditingAction^.ActionOptions.Action = acEditTemplate then
      //if FEditTemplateOptions_EditingAction <> nil then
        if AEditingAction = @FClkEditedActionByEditTemplate then
        begin
          case APropertyIndex of
            CEditTemplate_ListOfEditedProperties_PropIndex:
            begin
              AEditingAction^.EditTemplateOptions.ListOfEditedProperties_ET := ANewText;
              TriggerOnControlsModified(True {ANewText <> OldText});  //OldText is not initialized here
              Exit;
            end;

            CEditTemplate_ListOfEnabledProperties_PropIndex:
            begin
              AEditingAction^.EditTemplateOptions.ListOfEnabledProperties_ET := FastReplace_45ToReturn(ANewText);
              TriggerOnControlsModified(True {ANewText <> OldText});  //OldText is not initialized here
              Exit;
            end;
          end;
        end
        else
          if APropertyIndex in [CEditTemplate_ListOfEditedProperties_PropIndex, CEditTemplate_ListOfEnabledProperties_PropIndex] then
            Exit; //behave like read-only properties
    end;
  end;   //case EditingActionType

  //default handler for main properties
  OldText := CMainGetActionValueStrFunctions[ALiveEditingActionType](AEditingAction, APropertyIndex);
  CMainSetActionValueStrFunctions[ALiveEditingActionType](AEditingAction, ANewText, APropertyIndex);
  TriggerOnControlsModified(ANewText <> OldText);

  if AEditingAction^.ActionOptions.Action = acPlugin then
    if APropertyIndex = CPlugin_FileName_PropIndex then
    begin
      DoOnModifyPluginProperty(AEditingAction);
      tmrReloadOIContent.Enabled := True;
    end;

  if AEditingAction <> @FClkEditedActionByEditTemplate then    //This prevents loading the OI when changing the edited action type.
    if AEditingAction^.ActionOptions.Action = acEditTemplate then       // Only the editing action should be verified here. The edited action should be avoided.
    begin
      if APropertyIndex = CEditTemplate_EditedActionType_PropIndex then // AEditingAction will get both editing and edited.
        if FEditTemplateOptions_EditingAction <> nil then
        begin
          FClkEditedActionByEditTemplate.ActionOptions.Action := AEditingAction^.EditTemplateOptions.EditedActionType;
          if Length(FEditTemplateOptions_EditingAction^.FindSubControlOptions.MatchBitmapText) = 0 then
          begin
            DeserializeEditTemplateEditingAction;  //better start off with an empty list of profiles
            SetLength(FEditTemplateOptions_EditingAction^.FindSubControlOptions.MatchBitmapText, frClickerFindControl.GetBMPTextFontProfilesCount);
            frClickerFindControl.CreateBMPTextFrames(Length(FEditTemplateOptions_EditingAction^.FindSubControlOptions.MatchBitmapText)); //AddNewFontProfile(FEditTemplateOptions_EditingAction^.FindSubControlOptions.MatchBitmapText[n]);
            BuildFontColorIconsList;
          end;

          //These will set the action options to default, discarding user settings.
          if ANewText <> OldText then
          begin
            SetEmptyEditedActionByEditTemplateToDefault;
            FClkEditedActionByEditTemplate.ActionOptions.Action := AEditingAction^.EditTemplateOptions.EditedActionType; //call again after resetting by GetDefaultPropertyValues_EditTemplate
          end;

          SerializeEditTemplateEditingAction;
          tmrOnChangeEditTemplateEditingActionType.Enabled := True;
        end;

      if APropertyIndex = CEditTemplate_EditedActionName_PropIndex then // AEditingAction will get both editing and edited.
        if FEditTemplateOptions_EditingAction <> nil then
          if frClickerFindControl.GetBMPTextFontProfilesCount <> Length(FEditTemplateOptions_EditingAction^.FindSubControlOptions.MatchBitmapText) then
            DeserializeEditTemplateEditingAction;
    end;
end;


procedure TfrClickerActions.SetEmptyEditedActionByEditTemplateToDefault;
begin
  if IsActionEmpty_Click(FClkEditedActionByEditTemplate.ClickOptions) then
    GetDefaultPropertyValues_Click(FClkEditedActionByEditTemplate.ClickOptions);

  if IsActionEmpty_ExecApp(FClkEditedActionByEditTemplate.ExecAppOptions) then
    GetDefaultPropertyValues_ExecApp(FClkEditedActionByEditTemplate.ExecAppOptions);

  if IsActionEmpty_FindControl(FClkEditedActionByEditTemplate.FindControlOptions) then
    GetDefaultPropertyValues_FindControl(FClkEditedActionByEditTemplate.FindControlOptions);

  if IsActionEmpty_FindSubControl(FClkEditedActionByEditTemplate.FindSubControlOptions) then
    GetDefaultPropertyValues_FindSubControl(FClkEditedActionByEditTemplate.FindSubControlOptions);

  if IsActionEmpty_SetControlText(FClkEditedActionByEditTemplate.SetTextOptions) then
    GetDefaultPropertyValues_SetControlText(FClkEditedActionByEditTemplate.SetTextOptions);

  if IsActionEmpty_CallTemplate(FClkEditedActionByEditTemplate.CallTemplateOptions) then
    GetDefaultPropertyValues_CallTemplate(FClkEditedActionByEditTemplate.CallTemplateOptions);

  if IsActionEmpty_Sleep(FClkEditedActionByEditTemplate.SleepOptions) then
    GetDefaultPropertyValues_Sleep(FClkEditedActionByEditTemplate.SleepOptions);

  if IsActionEmpty_SetVar(FClkEditedActionByEditTemplate.SetVarOptions) then
    GetDefaultPropertyValues_SetVar(FClkEditedActionByEditTemplate.SetVarOptions);

  if IsActionEmpty_WindowOperations(FClkEditedActionByEditTemplate.WindowOperationsOptions) then
    GetDefaultPropertyValues_WindowOperations(FClkEditedActionByEditTemplate.WindowOperationsOptions);

  if IsActionEmpty_LoadSetVarFromFile(FClkEditedActionByEditTemplate.LoadSetVarFromFileOptions) then
    GetDefaultPropertyValues_LoadSetVarFromFile(FClkEditedActionByEditTemplate.LoadSetVarFromFileOptions);

  if IsActionEmpty_SaveSetVarToFile(FClkEditedActionByEditTemplate.SaveSetVarToFileOptions) then
    GetDefaultPropertyValues_SaveSetVarToFile(FClkEditedActionByEditTemplate.SaveSetVarToFileOptions);

  if IsActionEmpty_Plugin(FClkEditedActionByEditTemplate.PluginOptions) then
    GetDefaultPropertyValues_Plugin(FClkEditedActionByEditTemplate.PluginOptions);

  if IsActionEmpty_EditTemplate(FClkEditedActionByEditTemplate.EditTemplateOptions) then
    GetDefaultPropertyValues_EditTemplate(FClkEditedActionByEditTemplate.EditTemplateOptions);
end;


procedure TfrClickerActions.HandleOnOIEditedText(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; ANewText: string);
var
  OldText: string;
begin
  try
    case ACategoryIndex of
      CCategory_Common:
      begin
        OldText := GetActionValueStr_Action(FEditingAction, APropertyIndex);
        SetActionValueStr_Action(FEditingAction, ANewText, APropertyIndex);
        FCurrentlyEditingActionType := Ord(FEditingAction^.ActionOptions.Action);

        CurrentlyEditingActionType := FEditingAction^.ActionOptions.Action;

        TriggerOnControlsModified(ANewText <> OldText);
        tmrReloadOIContent.Enabled := True;
      end;

      CCategory_ActionSpecific:
        OIEditedText_ActionSpecific(FEditingAction, CurrentlyEditingActionType, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, ANewText);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
        begin
          OIEditedText_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, ANewText);
          //if FEditingAction^.ActionOptions.Action = acEditTemplate then
          //  FEditingAction^.EditTemplateOptions.ListOfEditedProperties := StringReplace(GetActionPropertiesByType(FEditingAction^), CPropSeparatorSer, CPropSeparatorInt, [rfReplaceAll]);
        end

      else
        ;
    end;
  except
    on E: Exception do
      DoOnAddToLog('Ex on setting property value at: ' + IntToStr(ACategoryIndex) + ' /' +  IntToStr(APropertyIndex) + IntToStr(AItemIndex) + '.  Msg: ' + E.Message);
  end;
end;


function TfrClickerActions.EditFontProperties(AEditingAction: PClkActionRec; AItemIndexDiv: Integer; var ANewItems: string): Boolean;
var
  TempFontDialog: TFontDialog;
begin
  Result := False;

  if AEditingAction = nil then
    Exit;

  TempFontDialog := TFontDialog.Create(nil);
  try
    TempFontDialog.Font.Name := ANewItems;
    TempFontDialog.Font.Size := AEditingAction^.FindSubControlOptions.MatchBitmapText[AItemIndexDiv].FontSize;

    if AEditingAction^.FindSubControlOptions.MatchBitmapText[AItemIndexDiv].Bold then
      TempFontDialog.Font.Style := TempFontDialog.Font.Style + [fsBold];

    if AEditingAction^.FindSubControlOptions.MatchBitmapText[AItemIndexDiv].Italic then
      TempFontDialog.Font.Style := TempFontDialog.Font.Style + [fsItalic];

    if AEditingAction^.FindSubControlOptions.MatchBitmapText[AItemIndexDiv].Underline then
      TempFontDialog.Font.Style := TempFontDialog.Font.Style + [fsUnderline];

    if AEditingAction^.FindSubControlOptions.MatchBitmapText[AItemIndexDiv].StrikeOut then
      TempFontDialog.Font.Style := TempFontDialog.Font.Style + [fsStrikeOut];

    if not TempFontDialog.Execute then
      Exit;

    ANewItems := TempFontDialog.Font.Name;
    Result := True;

    AEditingAction^.FindSubControlOptions.MatchBitmapText[AItemIndexDiv].FontName := ANewItems; //redundant, because the OI will call another handler for the property itself
    AEditingAction^.FindSubControlOptions.MatchBitmapText[AItemIndexDiv].FontSize := TempFontDialog.Font.Size;
    AEditingAction^.FindSubControlOptions.MatchBitmapText[AItemIndexDiv].Bold := fsBold in TempFontDialog.Font.Style;
    AEditingAction^.FindSubControlOptions.MatchBitmapText[AItemIndexDiv].Italic := fsItalic in TempFontDialog.Font.Style;
    AEditingAction^.FindSubControlOptions.MatchBitmapText[AItemIndexDiv].Underline := fsUnderline in TempFontDialog.Font.Style;
    AEditingAction^.FindSubControlOptions.MatchBitmapText[AItemIndexDiv].StrikeOut := fsStrikeOut in TempFontDialog.Font.Style;

    AEditingAction^.FindSubControlOptions.MatchBitmapText[AItemIndexDiv].CharSet := TempFontDialog.Font.CharSet;
    AEditingAction^.FindSubControlOptions.MatchBitmapText[AItemIndexDiv].Orientation := TempFontDialog.Font.Orientation;
    AEditingAction^.FindSubControlOptions.MatchBitmapText[AItemIndexDiv].Pitch := TempFontDialog.Font.Pitch;
  finally
    TempFontDialog.Free;
  end;
end;


procedure TfrClickerActions.OpenFontsMenu(AEditingAction: PClkActionRec; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer);
var
  i, j: Integer;
  VarsWithFonts: TStringList;
  tp: TPoint;
  VarsMenuItem, VarWithFontsMenuItem: TMenuItem;
begin
  FOIEditorMenu.Items.Clear;

  AddMenuItemToPopupMenu(FOIEditorMenu, 'Browse system fonts...', MenuItem_BrowseSystemFontsClick,
    ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

  VarsMenuItem := AddMenuItemToPopupMenu(FOIEditorMenu, 'Select font from "list of fonts" variables', nil,
    ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

  VarsWithFonts := TStringList.Create;
  try
    VarsWithFonts.LineBreak := #13#10;
    for i := 0 to FClkVariables.Count - 1 do
    begin
      if Pos(#4#5, FClkVariables.Strings[i]) > 0 then
      begin
        VarWithFontsMenuItem := AddMenuItemToAnotherMenuItem(FOIEditorMenu, VarsMenuItem, FClkVariables.Names[i], nil,
          ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

        VarsWithFonts.Text := FastReplace_45ToReturn(FClkVariables.ValueFromIndex[i]);
        for j := 0 to VarsWithFonts.Count - 1 do
          AddMenuItemToAnotherMenuItem(FOIEditorMenu, VarWithFontsMenuItem, VarsWithFonts.Strings[j], MenuItem_SelectFontFromVarClick,
            ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
      end;
    end;

    if VarsMenuItem.Count = 0 then
      VarsMenuItem.Enabled := False;
  finally
    VarsWithFonts.Free;
  end;

  GetCursorPos(tp);
  FOIEditorMenu.PopUp(tp.X, tp.Y);
end;


procedure TfrClickerActions.HandleOIEditorMenuOnClose(Sender: TObject);
begin
  FOIEditorMenuClosed := True;
end;


function TfrClickerActions.OIEditItems_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewItems: string): Boolean;
var
  EditingActionType: Integer;
  ItemIndexDiv, ItemIndexMod: Integer;
  tk: QWord;
begin
  Result := False;

  if AEditingAction = nil then
    Exit;

  EditingActionType := Integer(ALiveEditingActionType);
  if EditingActionType = CClkUnsetAction then
    Exit;

  if EditingActionType = Ord(acFindSubControl) then
    if APropertyIndex = CFindSubControl_MatchBitmapText_PropIndex then
    begin
      ItemIndexDiv := AItemIndex div CPropCount_FindSubControlMatchBitmapText;
      ItemIndexMod := AItemIndex mod CPropCount_FindSubControlMatchBitmapText;

      if ItemIndexMod = CFindSubControl_MatchBitmapText_FontName_PropItemIndex then
      begin
        FOIEditorMenuClosed := False;
        FTempNewItems := ANewItems;
        FOIEditorMenu.OnClose := HandleOIEditorMenuOnClose;
        OpenFontsMenu(AEditingAction, ANodeLevel, ACategoryIndex, APropertyIndex, ItemIndexDiv);

        tk := GetTickCount64;
        repeat
          Application.ProcessMessages;
        until FOIEditorMenuClosed or (GetTickCount64 - tk > 30000);

        if FOIEditorMenuClosed then
        begin
          ANewItems := FTempNewItems;
          Result := True;
        end;
      end;
    end;
end;


function TfrClickerActions.HandleOnOIEditItems(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewItems: string): Boolean;
begin
  Result := False;

  try
    case ACategoryIndex of
      CCategory_Common:
        ;

      CCategory_ActionSpecific:
        Result := OIEditItems_ActionSpecific(FEditingAction, CurrentlyEditingActionType, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, ANewItems);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          Result := OIEditItems_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, ANewItems);
    end;
  except
    on E: Exception do
      DoOnAddToLog('Ex on setting subproperty value (items) at: ' + IntToStr(ACategoryIndex) + ' /' +  IntToStr(APropertyIndex) + IntToStr(AItemIndex) + '.  Msg: ' + E.Message);
  end;
end;


function TfrClickerActions.HandleOnOIGetColorConstsCount(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer;
begin
  Result := 0;  //additional user colors
end;


procedure TfrClickerActions.HandleOnOIGetColorConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AColorItemIndex: Integer; var AColorName: string; var AColorValue: Int64);
begin
  //additional user colors
end;


function OIGetEnumConstsCount_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer;
var
  EditingActionType: Integer;
  ItemIndexMod: Integer;
begin
  Result := 0;

  if AEditingAction = nil then
    Exit;

  EditingActionType := Integer(ALiveEditingActionType);
  if EditingActionType = CClkUnsetAction then
    Exit;

  if ALiveEditingActionType = acFindControl then
  begin
    if APropertyIndex = CFindControl_MatchCriteria_PropIndex then
    begin
      Result := CFindControl_MatchCriteriaEnumCounts[AItemIndex];
      Exit;
    end;
  end;

  if ALiveEditingActionType = acFindSubControl then
  begin
    if APropertyIndex = CFindSubControl_MatchCriteria_PropIndex then
    begin
      Result := CFindSubControl_MatchCriteriaEnumCounts[AItemIndex];
      Exit;
    end;

    if APropertyIndex = CFindSubControl_MatchBitmapText_PropIndex then
    begin
      ItemIndexMod := AItemIndex mod CPropCount_FindSubControlMatchBitmapText;
      Result := CFindSubControl_MatchBitmapTextEnumCounts[ItemIndexMod];

      if ItemIndexMod = CFindSubControl_MatchBitmapText_FontName_PropItemIndex then
        Result := Screen.Fonts.Count;

      Exit;
    end;

    if APropertyIndex = CFindSubControl_RenderingInBrowserSettings_PropIndex then
    begin
      Result := CFindSubControl_RenderingInBrowserSettingsEnumCounts[AItemIndex];
      Exit;
    end;

    if APropertyIndex = CFindSubControl_GPUSettings_PropIndex then
    begin
      Result := CFindSubControl_GPUSettingsEnumCounts[AItemIndex];
      Exit;
    end;

    if APropertyIndex = CFindSubControl_ImageEffectSettings_PropIndex then
    begin
      Result := CFindSubControl_ImageEffectSettingsEnumCounts[AItemIndex];
      Exit;
    end;
  end;

  if ALiveEditingActionType = acCallTemplate then
    if APropertyIndex = CCallTemplate_CallTemplateLoop_PropIndex then
    begin
      Result := CCallTemplate_CallTemplateLoopEnumCounts[AItemIndex];
      Exit;
    end;

  if (ALiveEditingActionType = acPlugin) and (APropertyIndex > CPlugin_FileName_PropIndex) then
  begin
    //Result := 0
    Result := StrToIntDef(GetPluginPropertyAttribute(AEditingAction.PluginOptions.ListOfPropertiesAndTypes, CPluginPropertyAttr_EnumCounts, APropertyIndex), 0);
  end
  else
    Result := CPropEnumCounts[ALiveEditingActionType]^[APropertyIndex];
end;


function TfrClickerActions.HandleOnOIGetEnumConstsCount(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer;
begin
  Result := 0;

  try
    case ACategoryIndex of
      CCategory_Common:
        Result := CActionEnumCounts[APropertyIndex];

      CCategory_ActionSpecific:
        Result := OIGetEnumConstsCount_ActionSpecific(FEditingAction, CurrentlyEditingActionType, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          Result := OIGetEnumConstsCount_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

      else
        Result := 0;
    end;
  except
  end;
end;


procedure OIGetEnumConst_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEnumItemIndex: Integer; var AEnumItemName: string; var AEnumImgItemIndex: Integer; var AImgLst: TImageList);
var
  EditingActionType: Integer;
  ItemIndexMod: Integer;
  ListOfEnumValues: TStringList;
begin
  if AEditingAction = nil then
    Exit;

  EditingActionType := Integer(ALiveEditingActionType);
  if EditingActionType = CClkUnsetAction then
    Exit;

  if ALiveEditingActionType = acFindControl then
  begin
    if APropertyIndex = CFindControl_MatchCriteria_PropIndex then
    begin
      AEnumItemName := CFindControl_MatchCriteriaEnumStrings[AItemIndex]^[AEnumItemIndex];
      Exit;
    end;
  end;

  if ALiveEditingActionType = acFindSubControl then
  begin
    if APropertyIndex = CFindSubControl_MatchCriteria_PropIndex then
    begin
      AEnumItemName := CFindSubControl_MatchCriteriaEnumStrings[AItemIndex]^[AEnumItemIndex];
      Exit;
    end;

    if APropertyIndex = CFindSubControl_MatchBitmapText_PropIndex then
    begin
      ItemIndexMod := AItemIndex mod CPropCount_FindSubControlMatchBitmapText;

      case ItemIndexMod of
        CFindSubControl_MatchBitmapText_FontName_PropItemIndex:
          AEnumItemName := Screen.Fonts.Strings[AEnumItemIndex];

        CFindSubControl_MatchBitmapText_CharSet_PropItemIndex:
          AEnumItemName := CharSetToString(AEnumItemIndex);

        else
          AEnumItemName := CFindSubControl_MatchBitmapTextEnumStrings[ItemIndexMod]^[AEnumItemIndex];
      end;

      Exit;
    end;

    if APropertyIndex = CFindSubControl_RenderingInBrowserSettings_PropIndex then
    begin
      AEnumItemName := CFindControl_RenderingInBrowserSettingsEnumStrings[AItemIndex]^[AEnumItemIndex];
      Exit;
    end;

    if APropertyIndex = CFindSubControl_GPUSettings_PropIndex then
    begin
      AEnumItemName := CFindControl_GPUSettingsEnumStrings[AItemIndex]^[AEnumItemIndex];
      Exit;
    end;

    if APropertyIndex = CFindSubControl_ImageEffectSettings_PropIndex then
    begin
      AEnumItemName := CFindControl_ImageEffectSettingsEnumStrings[AItemIndex]^[AEnumItemIndex];
      Exit;
    end;
  end;

  if ALiveEditingActionType = acCallTemplate then
    if APropertyIndex = CCallTemplate_CallTemplateLoop_PropIndex then
    begin
      AEnumItemName := CCallTemplate_CallTemplateLoopEnumStrings[AItemIndex]^[AEnumItemIndex];
      Exit;
    end;

  if (ALiveEditingActionType = acPlugin) and (APropertyIndex > CPlugin_FileName_PropIndex) then
  begin
    AEnumItemName := GetPluginPropertyAttribute(AEditingAction.PluginOptions.ListOfPropertiesAndTypes, CPluginPropertyAttr_EnumStrings, APropertyIndex);
    ListOfEnumValues := TStringList.Create;
    try
      ListOfEnumValues.LineBreak := #13#10;
      ListOfEnumValues.Text := FastReplace_45ToReturn(AEnumItemName);
      try
        AEnumItemName := ListOfEnumValues.Strings[AEnumItemIndex];
      except
        AEnumItemName := 'item out of bounds'
      end;
    finally
      ListOfEnumValues.Free;
    end;
  end
  else
    AEnumItemName := CPropEnumStrings[ALiveEditingActionType]^[APropertyIndex]^[AEnumItemIndex];

  if ALiveEditingActionType = acEditTemplate then
  begin
    AEnumImgItemIndex := AEnumItemIndex;
    case APropertyIndex of
      CEditTemplate_Operation_PropIndex:
        AImgLst := dmClickerIcons.imglstEditTemplateOperationProperties;

      CEditTemplate_WhichTemplate_PropIndex:
        AImgLst := dmClickerIcons.imglstEditTemplateWhichTemplateProperties;

      CEditTemplate_EditedActionType_PropIndex:
        AImgLst := dmClickerIcons.imglstActions16;
    end;
  end;
end;


procedure TfrClickerActions.HandleOnOIGetEnumConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEnumItemIndex: Integer; var AEnumItemName: string; var AEnumImgItemIndex: Integer; var AImgLst: TImageList);
begin
  AEnumItemName := '';

  try
    case ACategoryIndex of
      CCategory_Common:
        AEnumItemName := CActionEnumStrings[APropertyIndex]^[AEnumItemIndex];

      CCategory_ActionSpecific:
        OIGetEnumConst_ActionSpecific(FEditingAction, CurrentlyEditingActionType, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEnumItemIndex, AEnumItemName, AEnumImgItemIndex, AImgLst);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          OIGetEnumConst_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEnumItemIndex, AEnumItemName, AEnumImgItemIndex, AImgLst);

      else
        AEnumItemName := '';
    end;
  except
  end;
end;


function PropertyValueReplace(s, AListOfPropertiesAndValues: string): string; //replaces all 'PropertyValue[<index>]' with the actual value
var
  TempListOfPropertiesAndValues: TStringList;
  i: Integer;
begin
  Result := s;

  TempListOfPropertiesAndValues := TStringList.Create;
  try
    TempListOfPropertiesAndValues.LineBreak := #13#10;
    TempListOfPropertiesAndValues.Text := AListOfPropertiesAndValues;

    for i := 0 to TempListOfPropertiesAndValues.Count - 1 do
      Result := StringReplace(Result, 'PropertyValue[' + IntToStr(i) + ']', TempListOfPropertiesAndValues.ValueFromIndex[i], [rfReplaceAll]);
  finally
    TempListOfPropertiesAndValues.Free;
  end;
end;


procedure OIPaintText_ActionSpecific(AEditingAction, AFEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ANodeData: TNodeDataPropertyRec; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
  const TargetCanvas: TCanvas; Column: TColumnIndex; var TextType: TVSTTextType; AOnAddToLog: TOnAddToLog);
var
  ListOfPrimitiveFiles_Modified: TStringList;
  ClickTypeIsNotDrag: Boolean;
  PluginPropertyEnabled: string;
begin
  if AEditingAction = nil then
    Exit;

  if (ANodeData.Level = CPropertyLevel) and (ALiveEditingActionType = acClick) then
    begin
      ClickTypeIsNotDrag := AEditingAction^.ClickOptions.ClickType <> CClickType_Drag;

      if ((APropertyIndex = CClick_XClickPointVar_PropIndex) and (AEditingAction^.ClickOptions.XClickPointReference <> xrefVar)) or
         ((APropertyIndex = CClick_YClickPointVar_PropIndex) and (AEditingAction^.ClickOptions.YClickPointReference <> yrefVar)) or
         ((APropertyIndex = CClick_XClickPointReferenceDest_PropIndex) and ClickTypeIsNotDrag) or
         ((APropertyIndex = CClick_YClickPointReferenceDest_PropIndex) and ClickTypeIsNotDrag) or
         ((APropertyIndex = CClick_XClickPointVarDest_PropIndex) and ((AEditingAction^.ClickOptions.XClickPointReferenceDest <> xrefVar) or ClickTypeIsNotDrag)) or
         ((APropertyIndex = CClick_YClickPointVarDest_PropIndex) and ((AEditingAction^.ClickOptions.YClickPointReferenceDest <> yrefVar) or ClickTypeIsNotDrag)) or
         ((APropertyIndex = CClick_XOffsetDest_PropIndex) and ClickTypeIsNotDrag) or
         ((APropertyIndex = CClick_YOffsetDest_PropIndex) and ClickTypeIsNotDrag) or
         ((APropertyIndex = CClick_MouseWheelType_PropIndex) and (AEditingAction^.ClickOptions.ClickType <> CClickType_Wheel)) or
         ((APropertyIndex = CClick_MouseWheelAmount_PropIndex) and (AEditingAction^.ClickOptions.ClickType <> CClickType_Wheel)) or
         ((APropertyIndex = CClick_LeaveMouse_PropIndex) and (AEditingAction^.ClickOptions.ClickType in [CClickType_Drag, CClickType_Wheel])) or
         ((APropertyIndex in [CClick_DelayAfterMovingToDestination_PropIndex .. CClick_MoveDuration_PropIndex]) and (AEditingAction^.ClickOptions.ClickType in [CClickType_ButtonDown, CClickType_ButtonUp, CClickType_Wheel])) then
      begin
        TargetCanvas.Font.Color := clGray;
        Exit;
      end;
    end;  //acClick

    if (ANodeData.Level = CPropertyLevel) and (ALiveEditingActionType = acFindSubControl) then
    begin
      if (APropertyIndex in [CFindSubControl_MatchBitmapFiles_PropIndex, CFindSubControl_MatchPrimitiveFiles_PropIndex]) then
      begin
        TargetCanvas.Font.Style := [fsItalic];
        //Exit;
      end;

      if ((APropertyIndex in [CFindSubControl_SourceFileName_PropIndex, CFindSubControl_ImageSourceFileNameLocation_PropIndex])
         and (AEditingAction^.FindSubControlOptions.ImageSource = isScreenshot)) then
      begin
        TargetCanvas.Font.Color := clGray;
        Exit;
      end;
    end;

    if (ANodeData.Level = CPropertyLevel) and (ALiveEditingActionType = acWindowOperations) then
      if APropertyIndex in [CWindowOperations_NewX_PropItemIndex .. CWindowOperations_NewSizeEnabled_PropItemIndex] then
        if (AEditingAction^.WindowOperationsOptions.Operation <> woMoveResize) or
          (not AEditingAction^.WindowOperationsOptions.NewPositionEnabled) and (APropertyIndex in [CWindowOperations_NewX_PropItemIndex, CWindowOperations_NewY_PropItemIndex]) or
          (not AEditingAction^.WindowOperationsOptions.NewSizeEnabled) and (APropertyIndex in [CWindowOperations_NewWidth_PropItemIndex, CWindowOperations_NewHeight_PropItemIndex]) then
        begin
          TargetCanvas.Font.Color := clGray;
          Exit;
        end;

    if (ANodeData.Level = CPropertyItemLevel) and (ALiveEditingActionType = acFindSubControl) then
    begin
      if APropertyIndex = CFindSubControl_MatchPrimitiveFiles_PropIndex then
      begin
        if AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified = '' then //this happens because this field should have been updated in FEditTemplateOptions_EditingAction, instead of FEditingAction
        begin
          if ANodeData.IsSelected then
            TargetCanvas.Font.Color := clLime
          else
            TargetCanvas.Font.Color := clGreen;
        end
        else
        begin
          ListOfPrimitiveFiles_Modified := TStringList.Create;   //instead of parsing this list on every tree paint action, the "modified" flags could be stored in some array of (paths + modified)
          try
            ListOfPrimitiveFiles_Modified.LineBreak := #13#10;
            ListOfPrimitiveFiles_Modified.Text := AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified;

            try
              if APropertyItemIndex < ListOfPrimitiveFiles_Modified.Count then
              begin
                if ListOfPrimitiveFiles_Modified.Strings[APropertyItemIndex] = '1' then
                  TargetCanvas.Font.Color := clRed;
              end
              else
              begin
                TargetCanvas.Font.Color := clWhite;
                TargetCanvas.Brush.Color := clRed;
              end;
            except
              TargetCanvas.Font.Color := clWhite;
              TargetCanvas.Brush.Color := clRed;
            end;

            //Exit;
          finally
            ListOfPrimitiveFiles_Modified.Free;
          end;
        end;
      end; //primitives

      if (APropertyIndex in [CFindSubControl_MatchBitmapAlgorithmSettings_PropIndex]) and
         (AEditingAction^.FindSubControlOptions.MatchBitmapAlgorithm <> mbaXYMultipleAndOffsets) then
      begin
        TargetCanvas.Font.Color := clGray;
        Exit;
      end;

      if (APropertyIndex in [CFindSubControl_MatchByHistogramSettings_PropIndex]) and
         (AEditingAction^.FindSubControlOptions.MatchBitmapAlgorithm <> mbaRawHistogramZones) then
      begin
        TargetCanvas.Font.Color := clGray;
        Exit;
      end;

      if (APropertyIndex in [CFindSubControl_RenderingInBrowserSettings_PropIndex]) and
         not AEditingAction^.FindSubControlOptions.UseTextRenderingInBrowser then
      begin
        TargetCanvas.Font.Color := clGray;
        Exit;
      end;

      if (ANodeData.Level = CPropertyItemLevel) and (APropertyIndex in [CFindSubControl_RenderingInBrowserSettings_PropIndex]) then
      begin
        if APropertyItemIndex = CFindSubControl_RenderingInBrowserSettings_ActionForSendingRequest_PropItemIndex then
          if AEditingAction^.FindSubControlOptions.RenderingInBrowserSettings.RenderingRequestType = rrtShellExecute then
          begin
            TargetCanvas.Font.Color := clGray;
            Exit;
          end;

        if APropertyItemIndex = CFindSubControl_RenderingInBrowserSettings_PluginActionForReceivingBitmaps_PropItemIndex then
          if not AEditingAction^.FindSubControlOptions.RenderingInBrowserSettings.UsePluginForReceivingBitmaps then
          begin
            TargetCanvas.Font.Color := clGray;
            Exit;
          end;
      end;

      if (APropertyIndex in [CFindSubControl_GPUSettings_PropIndex]) and
         (AEditingAction^.FindSubControlOptions.MatchBitmapAlgorithm <> mbaBruteForceOnGPU) then
      begin
        TargetCanvas.Font.Color := clGray;
        Exit;
      end;

      if (APropertyIndex in [CFindSubControl_ImageEffectSettings_PropIndex]) and
         not AEditingAction^.FindSubControlOptions.ImageEffectSettings.UseImageEffects then
        if APropertyItemIndex in [CFindSubControl_ImageEffectSettings_ImageEffect_PropItemIndex, CFindSubControl_ImageEffectSettings_WhereToApply_PropItemIndex] then
      begin
        TargetCanvas.Font.Color := clGray;
        Exit;
      end;
    end; //acFindSubControl

    if (ANodeData.Level = CPropertyItemLevel) and (ALiveEditingActionType = acCallTemplate) then
      if APropertyIndex = CCallTemplate_CallTemplateLoop_PropIndex then
        if APropertyItemIndex in [CCallTemplate_CallTemplateLoop_Counter_PropItemIndex .. CCallTemplate_CallTemplateLoop_EvalBreakPosition_PropItemIndex] then
          if not AEditingAction^.CallTemplateOptions.CallTemplateLoop.Enabled then
          begin
            TargetCanvas.Font.Color := clGray;
            Exit;
          end;

    if (ANodeData.Level = CPropertyLevel) and (ALiveEditingActionType = acPlugin) then
    begin
      PluginPropertyEnabled := GetPluginPropertyAttribute(AEditingAction.PluginOptions.ListOfPropertiesAndTypes, CPluginPropertyAttr_Enabled, APropertyIndex);
      if PluginPropertyEnabled <> '' then
      begin
        try
          PluginPropertyEnabled := PropertyValueReplace(PluginPropertyEnabled, AEditingAction.PluginOptions.ListOfPropertiesAndValues);
        except
          on E: Exception do
            AOnAddToLog('Cannot evaluate plugin property value: ' + E.Message);
        end;
        if not ClickerUtils.EvaluateActionCondition(PluginPropertyEnabled, TfrClickerActions.DummyEvaluateReplacements) then
        begin
          TargetCanvas.Font.Color := clGray;
          Exit;
        end;
      end;
    end;

  if (ACategoryIndex = CCategory_EditedAction) and (AFEditingAction^.ActionOptions.Action = acEditTemplate) then   //do not replace with ALiveEditingActionType!. It is FEditingAction on purpose.
    if not (AFEditingAction^.EditTemplateOptions.Operation in [etoNewAction, etoUpdateAction, etoSetProperty, etoGetProperty]) then
    begin
      TargetCanvas.Font.Color := clGray;
      Exit;
    end;

  if (ANodeData.Level = CPropertyLevel) and (ALiveEditingActionType = acEditTemplate) then
  begin
    case APropertyIndex of
      CEditTemplate_TemplateFileName_PropIndex:
        if AEditingAction^.EditTemplateOptions.WhichTemplate = etwtSelf then
        begin
          TargetCanvas.Font.Color := clGray;
          Exit;
        end;

      CEditTemplate_EditedActionName_PropIndex:
        ;

      CEditTemplate_EditedActionCondition_PropIndex:
        if not (AEditingAction^.EditTemplateOptions.Operation in [etoNewAction, etoUpdateAction, etoSetCondition]) then
        begin
          TargetCanvas.Font.Color := clGray;
          Exit;
        end;

      CEditTemplate_EditedActionTimeout_PropIndex:
        if not (AEditingAction^.EditTemplateOptions.Operation in [etoNewAction, etoUpdateAction, etoSetTimeout]) then
        begin
          TargetCanvas.Font.Color := clGray;
          Exit;
        end;

      CEditTemplate_NewActionName_PropIndex:
        if not (AEditingAction^.EditTemplateOptions.Operation in [etoMoveAction, etoDuplicateAction, etoRenameAction]) then
        begin
          TargetCanvas.Font.Color := clGray;
          Exit;
        end;

      CEditTemplate_ShouldSaveTemplate_PropIndex:
        if not ((AEditingAction^.EditTemplateOptions.Operation in [etoSaveTemplate]) and (AEditingAction^.EditTemplateOptions.WhichTemplate = etwtSelf)) then
        begin
          TargetCanvas.Font.Color := clGray;
          Exit;
        end;

      else
        ;
    end;  //case APropertyIndex
  end;
end;


procedure TfrClickerActions.HandleOnOIPaintText(ANodeData: TNodeDataPropertyRec; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
  const TargetCanvas: TCanvas; Column: TColumnIndex; var TextType: TVSTTextType);
begin  //
  if ANodeData.Level = 0 then
  begin
    TargetCanvas.Font.Style := [fsBold];
    Exit;
  end;

  try
    if Column = 1 then
    begin
      case ACategoryIndex of
        CCategory_Common:
          ;

        CCategory_ActionSpecific:
          OIPaintText_ActionSpecific(FEditingAction, FEditingAction, CurrentlyEditingActionType, ANodeData, ACategoryIndex, APropertyIndex, APropertyItemIndex, TargetCanvas, Column, TextType, DoOnAddToLog);

        CCategory_EditedAction:
          if FEditTemplateOptions_EditingAction <> nil then
            OIPaintText_ActionSpecific(FEditTemplateOptions_EditingAction, FEditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, ANodeData, ACategoryIndex, APropertyIndex, APropertyItemIndex, TargetCanvas, Column, TextType, DoOnAddToLog);
      end;
    end;
  except
  end;
end;


procedure OIBeforeCellPaint_ActionSpecific(ALiveEditingActionType: TClkAction; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
  TargetCanvas: TCanvas; Column: TColumnIndex; CellRect: TRect; ASelectedBMPTextTabIdx: Integer);
var
  ItemIndexDiv: Integer;
  NewColor: TColor;
begin
  if ALiveEditingActionType = acFindSubControl then
    if APropertyIndex = CFindSubControl_MatchBitmapText_PropIndex then
    begin
      ItemIndexDiv := APropertyItemIndex div CPropCount_FindSubControlMatchBitmapText;

      if ItemIndexDiv and 1 = 0 then
        NewColor := $E0FFE0   //light green
      else
        NewColor := $97E0FF;   //light orange

      if ItemIndexDiv = ASelectedBMPTextTabIdx then
        NewColor := ModifyBrightness(NewColor, 30, boDec);

      TargetCanvas.Pen.Color := NewColor;
      TargetCanvas.Brush.Color := NewColor;
      TargetCanvas.Rectangle(CellRect);
    end;
end;


procedure TfrClickerActions.HandleOnOIBeforeCellPaint(ANodeData: TNodeDataPropertyRec; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
  TargetCanvas: TCanvas; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  try
    case ACategoryIndex of
      CCategory_Common:
        ;

      CCategory_ActionSpecific:
        OIBeforeCellPaint_ActionSpecific(CurrentlyEditingActionType, ACategoryIndex, APropertyIndex, APropertyItemIndex, TargetCanvas, Column, CellRect, frClickerFindControl.SelectedBMPTextTab);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          OIBeforeCellPaint_ActionSpecific(FEditTemplateOptions_EditingAction.ActionOptions.Action, ACategoryIndex, APropertyIndex, APropertyItemIndex, TargetCanvas, Column, CellRect, frClickerFindControl.SelectedBMPTextTab);
    end;
  except
  end;
end;


procedure OIAfterCellPaint_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer; TargetCanvas: TCanvas; Column: TColumnIndex; const CellRect: TRect; ASrcImgLst: TImageList);
const
  CIconYOffset = 3;
var
  CurrentIconPos: Integer;
begin
  if AEditingAction = nil then
    Exit;

  if ALiveEditingActionType = acFindControl then
    if (APropertyIndex = CFindControl_MatchCriteria_PropIndex) and (APropertyItemIndex = -1) then
      if Column = 1 then
      begin
        CurrentIconPos := CellRect.Left + 2;

        if AEditingAction^.FindControlOptions.MatchCriteria.WillMatchText then
        begin
          ASrcImgLst.Draw(TargetCanvas, CurrentIconPos, CIconYOffset, CFindControl_MatchCriteria_WillMatchText_PropItemIndex, dsNormal, itImage);
          Inc(CurrentIconPos, 18);
        end;

        if AEditingAction^.FindControlOptions.MatchCriteria.WillMatchClassName then
        begin
          ASrcImgLst.Draw(TargetCanvas, CurrentIconPos, CIconYOffset, CFindControl_MatchCriteria_WillMatchClassName_PropItemIndex, dsNormal, itImage);
          Inc(CurrentIconPos, 18);
        end;
      end;

  if ALiveEditingActionType = acFindSubControl then
    if (APropertyIndex = CFindSubControl_MatchCriteria_PropIndex) and (APropertyItemIndex = -1) then
      if Column = 1 then
      begin
        CurrentIconPos := CellRect.Left + 2;

        if AEditingAction^.FindSubControlOptions.MatchCriteria.WillMatchBitmapText then
        begin
          ASrcImgLst.Draw(TargetCanvas, CurrentIconPos, CIconYOffset, CFindSubControl_MatchCriteria_WillMatchBitmapText_PropItemIndex, dsNormal, itImage);
          Inc(CurrentIconPos, 18);
        end;

        if AEditingAction^.FindSubControlOptions.MatchCriteria.WillMatchBitmapFiles then
        begin
          ASrcImgLst.Draw(TargetCanvas, CurrentIconPos, CIconYOffset, CFindSubControl_MatchCriteria_WillMatchBitmapFiles_PropItemIndex, dsNormal, itImage);
          Inc(CurrentIconPos, 18);
        end;

        if AEditingAction^.FindSubControlOptions.MatchCriteria.WillMatchPrimitiveFiles then
        begin
          ASrcImgLst.Draw(TargetCanvas, CurrentIconPos, CIconYOffset, CFindSubControl_MatchCriteria_WillMatchPrimitiveFiles_PropItemIndex, dsNormal, itImage);
          Inc(CurrentIconPos, 18);
        end;
      end;
end;


procedure TfrClickerActions.HandleOnOIAfterCellPaint(ANodeData: TNodeDataPropertyRec; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
  TargetCanvas: TCanvas; Column: TColumnIndex; const CellRect: TRect);
var
  TempUsedMatchCriteriaIcons: TImageList;
begin
  try
    case ACategoryIndex of
      CCategory_Common:
        ;

      CCategory_ActionSpecific:
      begin
        if CurrentlyEditingActionType = acFindControl then
          TempUsedMatchCriteriaIcons := dmClickerIcons.imglstUsedMatchCriteria
        else
          TempUsedMatchCriteriaIcons := dmClickerIcons.imglstUsedMatchCriteriaSub;

        OIAfterCellPaint_ActionSpecific(FEditingAction, CurrentlyEditingActionType, ACategoryIndex, APropertyIndex, APropertyItemIndex, TargetCanvas, Column, CellRect, TempUsedMatchCriteriaIcons);
      end;

      CCategory_EditedAction:
      begin
        if FEditTemplateOptions_EditingAction.ActionOptions.Action = acFindControl then
          TempUsedMatchCriteriaIcons := dmClickerIcons.imglstUsedMatchCriteria
        else
          TempUsedMatchCriteriaIcons := dmClickerIcons.imglstUsedMatchCriteriaSub;

        if FEditTemplateOptions_EditingAction <> nil then
          OIAfterCellPaint_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, ACategoryIndex, APropertyIndex, APropertyItemIndex, TargetCanvas, Column, CellRect, TempUsedMatchCriteriaIcons);
      end;
    end;
  except
  end;
end;


procedure TfrClickerActions.TextEditorMouseDown_ActionSpecific(AEditingAction: PClkActionRec; ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ItemIndexMod, ItemIndexDiv: Integer;
begin
  if AEditingAction = nil then
    Exit;

  case AEditingAction^.ActionOptions.Action of
    acFindControl:
    begin
      case APropertyIndex of
        CFindControl_InitialRectangle_PropIndex:
        begin
          case AItemIndex of
            CFindControl_InitialRectangle_LeftOffset_PropItemIndex:
              frClickerFindControl.UpdateOnSearchRectLeftOffsetMouseDown(AEditingAction^.FindControlOptions.InitialRectangle, Sender as TVTEdit, Button, Shift, X, Y);

            CFindControl_InitialRectangle_TopOffset_PropItemIndex:
              frClickerFindControl.UpdateOnSearchRectTopOffsetMouseDown(AEditingAction^.FindControlOptions.InitialRectangle, Sender as TVTEdit, Button, Shift, X, Y);

            CFindControl_InitialRectangle_RightOffset_PropItemIndex:
              frClickerFindControl.UpdateOnSearchRectRightOffsetMouseDown(AEditingAction^.FindControlOptions.InitialRectangle, Sender as TVTEdit, Button, Shift, X, Y);

            CFindControl_InitialRectangle_BottomOffset_PropItemIndex:
              frClickerFindControl.UpdateOnSearchRectBottomOffsetMouseDown(AEditingAction^.FindControlOptions.InitialRectangle, Sender as TVTEdit, Button, Shift, X, Y);
          end;
        end;
      end
    end;

    acFindSubControl:
    begin
      case APropertyIndex of
        CFindSubControl_InitialRectangle_PropIndex:
        begin
          case AItemIndex of
            CFindSubControl_InitialRectangle_LeftOffset_PropItemIndex:
              frClickerFindControl.UpdateOnSearchRectLeftOffsetMouseDown(AEditingAction^.FindSubControlOptions.InitialRectangle, Sender as TVTEdit, Button, Shift, X, Y);

            CFindSubControl_InitialRectangle_TopOffset_PropItemIndex:
              frClickerFindControl.UpdateOnSearchRectTopOffsetMouseDown(AEditingAction^.FindSubControlOptions.InitialRectangle, Sender as TVTEdit, Button, Shift, X, Y);

            CFindSubControl_InitialRectangle_RightOffset_PropItemIndex:
              frClickerFindControl.UpdateOnSearchRectRightOffsetMouseDown(AEditingAction^.FindSubControlOptions.InitialRectangle, Sender as TVTEdit, Button, Shift, X, Y);

            CFindSubControl_InitialRectangle_BottomOffset_PropItemIndex:
              frClickerFindControl.UpdateOnSearchRectBottomOffsetMouseDown(AEditingAction^.FindSubControlOptions.InitialRectangle, Sender as TVTEdit, Button, Shift, X, Y);
          end;
        end;

        CFindSubControl_MatchBitmapText_PropIndex:
        begin
          ItemIndexMod := AItemIndex mod CPropCount_FindSubControlMatchBitmapText;
          ItemIndexDiv := AItemIndex div CPropCount_FindSubControlMatchBitmapText;

          case ItemIndexMod of
            CFindSubControl_MatchBitmapText_CropLeft_PropItemIndex:
              frClickerFindControl.UpdateOnTextCroppingLeftMouseDown(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv], Sender as TVTEdit, Button, Shift, X, Y);

            CFindSubControl_MatchBitmapText_CropTop_PropItemIndex:
              frClickerFindControl.UpdateOnTextCroppingTopMouseDown(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv], Sender as TVTEdit, Button, Shift, X, Y);

            CFindSubControl_MatchBitmapText_CropRight_PropItemIndex:
              frClickerFindControl.UpdateOnTextCroppingRightMouseDown(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv], Sender as TVTEdit, Button, Shift, X, Y);

            CFindSubControl_MatchBitmapText_CropBottom_PropItemIndex:
              frClickerFindControl.UpdateOnTextCroppingBottomMouseDown(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv], Sender as TVTEdit, Button, Shift, X, Y);
          end;
        end;

        else
          ;
      end; //case
    end; //FindSubControl

    else
      ;
  end; //case
end;


procedure TfrClickerActions.HandleOnTextEditorMouseDown(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  try
    case ACategoryIndex of
      CCategory_Common:
        ;

      CCategory_ActionSpecific:
        TextEditorMouseDown_ActionSpecific(FEditingAction, ACategoryIndex, APropertyIndex, AItemIndex, Sender, Button, Shift, X, Y);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          TextEditorMouseDown_ActionSpecific(FEditTemplateOptions_EditingAction, ACategoryIndex, APropertyIndex, AItemIndex, Sender, Button, Shift, X, Y);
    end;
  except
  end;
end;


function TfrClickerActions.TextEditorMouseMove_ActionSpecific(AEditingAction: PClkActionRec; ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; Shift: TShiftState; X, Y: Integer): Boolean;
var
  ItemIndexMod, ItemIndexDiv: Integer;
  OldValue: string;
begin
  Result := False;
  if AEditingAction = nil then
    Exit;

  case AEditingAction^.ActionOptions.Action of
    acFindControl:
    begin
      case APropertyIndex of
        CFindControl_InitialRectangle_PropIndex:
        begin
          case AItemIndex of
            CFindControl_InitialRectangle_LeftOffset_PropItemIndex:
            begin
              OldValue := AEditingAction^.FindControlOptions.InitialRectangle.LeftOffset;
              frClickerFindControl.UpdateOnSearchRectLeftOffsetMouseMove(AEditingAction^.FindControlOptions.InitialRectangle, Sender as TVTEdit, Shift, X, Y);
              TriggerOnControlsModified(AEditingAction^.FindControlOptions.InitialRectangle.LeftOffset <> OldValue);
              Result := True;
            end;

            CFindControl_InitialRectangle_TopOffset_PropItemIndex:
            begin
              OldValue := AEditingAction^.FindControlOptions.InitialRectangle.TopOffset;
              frClickerFindControl.UpdateOnSearchRectTopOffsetMouseMove(AEditingAction^.FindControlOptions.InitialRectangle, Sender as TVTEdit, Shift, X, Y);
              TriggerOnControlsModified(AEditingAction^.FindControlOptions.InitialRectangle.TopOffset <> OldValue);
              Result := True;
            end;

            CFindControl_InitialRectangle_RightOffset_PropItemIndex:
            begin
              OldValue := AEditingAction^.FindControlOptions.InitialRectangle.RightOffset;
              frClickerFindControl.UpdateOnSearchRectRightOffsetMouseMove(AEditingAction^.FindControlOptions.InitialRectangle, Sender as TVTEdit, Shift, X, Y);
              TriggerOnControlsModified(AEditingAction^.FindControlOptions.InitialRectangle.RightOffset <> OldValue);
              Result := True;
            end;

            CFindControl_InitialRectangle_BottomOffset_PropItemIndex:
            begin
              OldValue := AEditingAction^.FindControlOptions.InitialRectangle.BottomOffset;
              frClickerFindControl.UpdateOnSearchRectBottomOffsetMouseMove(AEditingAction^.FindControlOptions.InitialRectangle, Sender as TVTEdit, Shift, X, Y);
              TriggerOnControlsModified(AEditingAction^.FindControlOptions.InitialRectangle.BottomOffset <> OldValue);
              Result := True;
            end;
          end;
        end;
      end; //case
    end;

    acFindSubControl:
    begin
      case APropertyIndex of
        CFindSubControl_InitialRectangle_PropIndex:
        begin
          case AItemIndex of
            CFindSubControl_InitialRectangle_LeftOffset_PropItemIndex:
            begin
              OldValue := AEditingAction^.FindSubControlOptions.InitialRectangle.LeftOffset;
              frClickerFindControl.UpdateOnSearchRectLeftOffsetMouseMove(AEditingAction^.FindSubControlOptions.InitialRectangle, Sender as TVTEdit, Shift, X, Y);
              TriggerOnControlsModified(AEditingAction^.FindSubControlOptions.InitialRectangle.LeftOffset <> OldValue);
              Result := True;
            end;

            CFindSubControl_InitialRectangle_TopOffset_PropItemIndex:
            begin
              OldValue := AEditingAction^.FindSubControlOptions.InitialRectangle.TopOffset;
              frClickerFindControl.UpdateOnSearchRectTopOffsetMouseMove(AEditingAction^.FindSubControlOptions.InitialRectangle, Sender as TVTEdit, Shift, X, Y);
              TriggerOnControlsModified(AEditingAction^.FindSubControlOptions.InitialRectangle.TopOffset <> OldValue);
              Result := True;
            end;

            CFindSubControl_InitialRectangle_RightOffset_PropItemIndex:
            begin
              OldValue := AEditingAction^.FindSubControlOptions.InitialRectangle.RightOffset;
              frClickerFindControl.UpdateOnSearchRectRightOffsetMouseMove(AEditingAction^.FindSubControlOptions.InitialRectangle, Sender as TVTEdit, Shift, X, Y);
              TriggerOnControlsModified(AEditingAction^.FindSubControlOptions.InitialRectangle.RightOffset <> OldValue);
              Result := True;
            end;

            CFindSubControl_InitialRectangle_BottomOffset_PropItemIndex:
            begin
              OldValue := AEditingAction^.FindSubControlOptions.InitialRectangle.BottomOffset;
              frClickerFindControl.UpdateOnSearchRectBottomOffsetMouseMove(AEditingAction^.FindSubControlOptions.InitialRectangle, Sender as TVTEdit, Shift, X, Y);
              TriggerOnControlsModified(AEditingAction^.FindSubControlOptions.InitialRectangle.BottomOffset <> OldValue);
              Result := True;
            end;
          end;
        end;

        CFindSubControl_MatchBitmapText_PropIndex:
        begin
          ItemIndexMod := AItemIndex mod CPropCount_FindSubControlMatchBitmapText;
          ItemIndexDiv := AItemIndex div CPropCount_FindSubControlMatchBitmapText;

          case ItemIndexMod of
            CFindSubControl_MatchBitmapText_CropLeft_PropItemIndex:
            begin
              OldValue := AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropLeft;
              frClickerFindControl.UpdateOnTextCroppingLeftMouseMove(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv], Sender as TVTEdit, Shift, X, Y, ItemIndexDiv);
              frClickerFindControl.SelectedBMPTextTab := ItemIndexDiv;
              TriggerOnControlsModified(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropLeft <> OldValue);
              Result := True;
            end;

            CFindSubControl_MatchBitmapText_CropTop_PropItemIndex:
            begin
              OldValue := AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropTop;
              frClickerFindControl.UpdateOnTextCroppingTopMouseMove(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv], Sender as TVTEdit, Shift, X, Y, ItemIndexDiv);
              frClickerFindControl.SelectedBMPTextTab := ItemIndexDiv;
              TriggerOnControlsModified(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropTop <> OldValue);
              Result := True;
            end;

            CFindSubControl_MatchBitmapText_CropRight_PropItemIndex:
            begin
              OldValue := AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropRight;
              frClickerFindControl.UpdateOnTextCroppingRightMouseMove(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv], Sender as TVTEdit, Shift, X, Y, ItemIndexDiv);
              frClickerFindControl.SelectedBMPTextTab := ItemIndexDiv;
              TriggerOnControlsModified(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropRight <> OldValue);
              Result := True;
            end;

            CFindSubControl_MatchBitmapText_CropBottom_PropItemIndex:
            begin
              OldValue := AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropBottom;
              frClickerFindControl.UpdateOnTextCroppingBottomMouseMove(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv], Sender as TVTEdit, Shift, X, Y, ItemIndexDiv);
              frClickerFindControl.SelectedBMPTextTab := ItemIndexDiv;
              TriggerOnControlsModified(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropBottom <> OldValue);
              Result := True;
            end;
          end;
        end;

        else
          ;
      end; //case
    end; //FindSubControl

    else
      ;
  end; //case
end;


function TfrClickerActions.HandleOnTextEditorMouseMove(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;

  try
    case ACategoryIndex of
      CCategory_Common:
        ;

      CCategory_ActionSpecific:
        Result := TextEditorMouseMove_ActionSpecific(FEditingAction, ACategoryIndex, APropertyIndex, AItemIndex, Sender, Shift, X, Y);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          Result := TextEditorMouseMove_ActionSpecific(FEditTemplateOptions_EditingAction, ACategoryIndex, APropertyIndex, AItemIndex, Sender, Shift, X, Y);
    end;
  except
  end;
end;


procedure TfrClickerActions.OITextEditorKeyUp_ActionSpecific(AEditingAction: PClkActionRec; ACategoryIndex, APropertyIndex, AItemIndex: Integer; Sender: TObject);
var
  i: Integer;
  ItemIndexMod, ItemIndexDiv: Integer;
  OldValue, NewValue: string;
begin
  if AEditingAction = nil then
    Exit;

  if AEditingAction^.ActionOptions.Action = acFindControl then
    case APropertyIndex of
      CFindControl_MatchText_PropIndex:
        TriggerOnControlsModified(AEditingAction^.FindControlOptions.MatchText <> TVTEdit(Sender).Text);

      CFindControl_InitialRectangle_PropIndex:
      begin
        OldValue := GetActionValueStr_FindControl_InitialRectangle(AEditingAction, AItemIndex);
        NewValue := TVTEdit(Sender).Text;
        SetActionValueStr_FindControl_InitialRectangle(AEditingAction, NewValue, AItemIndex);
        TriggerOnControlsModified(NewValue <> OldValue);

        frClickerFindControl.UpdateSearchAreaLabelsFromKeysOnInitRect(AEditingAction^.FindControlOptions.InitialRectangle);
      end; //init rect
    end;

  if AEditingAction^.ActionOptions.Action = acFindSubControl then
    case APropertyIndex of
      CFindControl_MatchText_PropIndex:
      begin
        TriggerOnControlsModified(AEditingAction^.FindSubControlOptions.MatchText <> TVTEdit(Sender).Text);
        AEditingAction^.FindSubControlOptions.MatchText := TVTEdit(Sender).Text;

        frClickerFindControl.PreviewText;
        for i := 0 to Length(AEditingAction^.FindSubControlOptions.MatchBitmapText) - 1 do
          frClickerFindControl.BMPTextFontProfiles[i].UpdateSelectionLabelsFromCropInfo(AEditingAction^.FindSubControlOptions.MatchBitmapText[i]);
      end;

      CFindSubControl_MatchBitmapText_PropIndex:
      begin
        ItemIndexMod := AItemIndex mod CPropCount_FindSubControlMatchBitmapText;
        ItemIndexDiv := AItemIndex div CPropCount_FindSubControlMatchBitmapText;

        if ItemIndexMod = CFindSubControl_MatchBitmapText_ProfileName_PropItemIndex then
        begin
          AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].ProfileName := TVTEdit(Sender).Text;
          frClickerFindControl.UpdateFontProfileName(ItemIndexDiv, TVTEdit(Sender).Text);
        end;

        frClickerFindControl.PreviewText;
      end;

      CFindSubControl_MatchBitmapAlgorithmSettings_PropIndex:
      begin
        OldValue := GetActionValueStr_FindSubControl_MatchBitmapAlgorithmSettings(AEditingAction, AItemIndex);
        NewValue := TVTEdit(Sender).Text;
        SetActionValueStr_FindSubControl_MatchBitmapAlgorithmSettings(AEditingAction, NewValue, AItemIndex);
        TriggerOnControlsModified(NewValue <> OldValue);

        frClickerFindControl.UpdateSearchAreaLabelsFromKeysOnInitRect(AEditingAction^.FindSubControlOptions.InitialRectangle); //call this, to update the grid
      end;

      CFindSubControl_InitialRectangle_PropIndex:
      begin
        OldValue := GetActionValueStr_FindSubControl_InitialRectangle(AEditingAction, AItemIndex);
        NewValue := TVTEdit(Sender).Text;
        SetActionValueStr_FindSubControl_InitialRectangle(AEditingAction, NewValue, AItemIndex);
        TriggerOnControlsModified(NewValue <> OldValue);

        frClickerFindControl.UpdateSearchAreaLabelsFromKeysOnInitRect(AEditingAction^.FindSubControlOptions.InitialRectangle);
      end; //init rect
    end; //case

  //if AEditingAction^.ActionOptions.Action = acPlugin then
  //  if APropertyIndex = CPlugin_FileName_PropIndex then
  //  begin
  //    NewValue := TVTEdit(Sender).Text;
  //    if DoOnFileExists(NewValue) then
  //    begin
  //      DoOnModifyPluginProperty(AEditingAction);
  //      tmrReloadOIContent.Enabled := True;   //reloading the OI will discard the new value, unfortunately
  //    end;
  //  end;
end;


procedure TfrClickerActions.HandleOnOITextEditorKeyUp(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  try
    case ACategoryIndex of
      CCategory_Common:
        ;

      CCategory_ActionSpecific:
        OITextEditorKeyUp_ActionSpecific(FEditingAction, ACategoryIndex, APropertyIndex, AItemIndex, Sender);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          OITextEditorKeyUp_ActionSpecific(FEditTemplateOptions_EditingAction, ACategoryIndex, APropertyIndex, AItemIndex, Sender);
    end;
  except
  end;
end;


procedure TfrClickerActions.HandleOnOITextEditorKeyDown(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_SPACE then
    if ssCtrl in Shift then
      ;//open a pop-up window with a list of available variables and functions;
end;


procedure TfrClickerActions.HandleOnOIEditorKeyDown(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; var Key: Word; Shift: TShiftState);
var
  EditingActionType: TClkAction;
begin
  case ACategoryIndex of
    CCategory_Common:
      EditingActionType := acFindSubControl;

    CCategory_ActionSpecific:
      EditingActionType := FEditingAction^.ActionOptions.Action;

    CCategory_EditedAction:
      if FEditTemplateOptions_EditingAction <> nil then
        EditingActionType := FEditTemplateOptions_EditingAction^.ActionOptions.Action;
  end;

  if EditingActionType = acFindSubControl then
    if ANodeLevel = CPropertyItemLevel then
      if APropertyIndex = CFindSubControl_MatchBitmapText_PropIndex then
        if AItemIndex mod CPropCount_FindSubControlMatchBitmapText = CFindSubControl_MatchBitmapText_FontName_PropItemIndex then
        begin
          if (Key = Ord('C')) and (ssCtrl in Shift) then
            Clipboard.AsText := FEditingAction^.FindSubControlOptions.MatchBitmapText[AItemIndex div CPropCount_FindSubControlMatchBitmapText].FontName;

          if (Key = Ord('V')) and (ssCtrl in Shift) then
          begin
            TriggerOnControlsModified(FEditingAction^.FindSubControlOptions.MatchBitmapText[AItemIndex div CPropCount_FindSubControlMatchBitmapText].FontName <> Clipboard.AsText);
            FEditingAction^.FindSubControlOptions.MatchBitmapText[AItemIndex div CPropCount_FindSubControlMatchBitmapText].FontName := Clipboard.AsText;
            FOIFrame.SetEditorValue(Clipboard.AsText); //select item in ComboBox
          end;
        end;
end;


procedure TfrClickerActions.OIEditorAssignMenuAndTooltip_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; var APopupMenu: TPopupMenu; var AHint: string; var AShowHint: Boolean);
var
  TempValue: string;
begin
  APopupMenu := nil;
  AHint := '';
  AShowHint := True;

  if AEditingAction = nil then
    Exit;

  if ANodeLevel = CPropertyLevel then
  begin
    if (ALiveEditingActionType = acPlugin) and (APropertyIndex > CPlugin_FileName_PropIndex) then
      AHint := 'Plugin-specific property'
    else
      AHint := CGetPropertyHint_Actions[ALiveEditingActionType]^[APropertyIndex];
  end;

    case ALiveEditingActionType of
      acExecApp:
      begin
        case APropertyIndex of
          CExecApp_PathToApp_PropIndex, CExecApp_CurrentDir_PropIndex:
          begin
            FLastClickedTVTEdit := nil;
            FLastClickedEdit := nil;

            if Sender is TVTEdit then
              FLastClickedTVTEdit := Sender as TVTEdit
            else
              FLastClickedTVTEdit := nil;

            if Sender is TEdit then
              FLastClickedEdit := Sender as TEdit
            else
              FLastClickedEdit := nil;

            APopupMenu := pmPathReplacements;
            AHint := '$AppDir$ replacement is available';
          end;
        end;
      end;

      acFindControl:
      begin
        case APropertyIndex of
          CFindControl_MatchCriteria_PropIndex:
            AHint := CGetPropertyHint_FindControlMatchCriteria_Items[AItemIndex];

          CFindControl_InitialRectangle_PropIndex:
          begin
            AHint := CGetPropertyHint_FindControlInitialRectangle_Items[AItemIndex];

            if AItemIndex in [CFindControl_InitialRectangle_Left_PropItemIndex .. CFindControl_InitialRectangle_Bottom_PropItemIndex] then
            begin
              case AItemIndex of
                CFindControl_InitialRectangle_Left_PropItemIndex:
                begin
                  FLastClickedTVTEdit := Sender as TVTEdit;
                  FLastClickedEdit := nil;
                  APopupMenu := pmStandardControlRefVars;
                  TempValue := AEditingAction^.FindControlOptions.InitialRectangle.Left;
                  AHint := AHint + #13#10 + TempValue + ' = ' + EvaluateReplacements(TempValue);
                end;

                CFindControl_InitialRectangle_Top_PropItemIndex:
                begin
                  FLastClickedTVTEdit := Sender as TVTEdit;
                  FLastClickedEdit := nil;
                  APopupMenu := pmStandardControlRefVars;
                  TempValue := AEditingAction^.FindControlOptions.InitialRectangle.Top;
                  AHint := AHint + #13#10 + TempValue + ' = ' + EvaluateReplacements(TempValue);
                end;

                CFindControl_InitialRectangle_Right_PropItemIndex:
                begin
                  FLastClickedTVTEdit := Sender as TVTEdit;
                  FLastClickedEdit := nil;
                  APopupMenu := pmStandardControlRefVars;
                  TempValue := AEditingAction^.FindControlOptions.InitialRectangle.Right;
                  AHint := AHint + #13#10 + TempValue + ' = ' + EvaluateReplacements(TempValue);
                end;

                CFindControl_InitialRectangle_Bottom_PropItemIndex:
                begin
                  FLastClickedTVTEdit := Sender as TVTEdit;
                  FLastClickedEdit := nil;
                  APopupMenu := pmStandardControlRefVars;
                  TempValue := AEditingAction^.FindControlOptions.InitialRectangle.Bottom;
                  AHint := AHint + #13#10 + TempValue + ' = ' + EvaluateReplacements(TempValue);
                end;
              end;
            end;
          end; //init rect
        end; //case APropertyIndex
      end;  //acFindControl

      acFindSubControl:
      begin
        case APropertyIndex of
          CFindSubControl_MatchCriteria_PropIndex:
            AHint := CGetPropertyHint_FindSubControlMatchCriteria_Items[AItemIndex];

          CFindSubControl_MatchBitmapText_PropIndex:
            case AItemIndex mod CPropCount_FindSubControlMatchBitmapText of
              CFindSubControl_MatchBitmapText_ForegroundColor_PropItemIndex, CFindSubControl_MatchBitmapText_BackgroundColor_PropItemIndex:
              begin
                FLastClickedTVTEdit := nil;
                FLastClickedEdit := Sender as TEdit;
                APopupMenu := pmStandardColorVariables;
              end;

              CFindSubControl_MatchBitmapText_FontName_PropItemIndex:
                AHint := 'Use the "..." button, to browse system fonts, or fonts mentioned in a variable with #4#5-separated list of fonts.' + #13#10 +
                         'The variable with list of fonts, can be set by the $GetListOfFonts()$ function.' + #13#10 +
                         'This is useful when getting a different list of fonts from another machine, where UIClicker is running in server mode.';

              CFindSubControl_MatchBitmapText_IgnoreBackgroundColor_PropItemIndex:
                AHint := 'When set to True, the pixels, which match the current BackgroundColor, under the configured error level, are ignored.' + #13#10 +
                         'This option is not suitable for antialiased text, if using a color, which is very different than BackgroundColor.' + #13#10 +
                         'It is better to use it for non-antialiased text.';

              else
                ;
            end;

          CFindSubControl_InitialRectangle_PropIndex:
          begin
            AHint := CGetPropertyHint_FindControlInitialRectangle_Items[AItemIndex];

            if AItemIndex in [CFindSubControl_InitialRectangle_Left_PropItemIndex .. CFindSubControl_InitialRectangle_Bottom_PropItemIndex] then
            begin
              case AItemIndex of
                CFindSubControl_InitialRectangle_Left_PropItemIndex:
                begin
                  FLastClickedTVTEdit := Sender as TVTEdit;
                  FLastClickedEdit := nil;
                  APopupMenu := pmStandardControlRefVars;
                  TempValue := AEditingAction^.FindSubControlOptions.InitialRectangle.Left;
                  AHint := AHint + #13#10 + TempValue + ' = ' + EvaluateReplacements(TempValue);
                end;

                CFindSubControl_InitialRectangle_Top_PropItemIndex:
                begin
                  FLastClickedTVTEdit := Sender as TVTEdit;
                  FLastClickedEdit := nil;
                  APopupMenu := pmStandardControlRefVars;
                  TempValue := AEditingAction^.FindSubControlOptions.InitialRectangle.Top;
                  AHint := AHint + #13#10 + TempValue + ' = ' + EvaluateReplacements(TempValue);
                end;

                CFindSubControl_InitialRectangle_Right_PropItemIndex:
                begin
                  FLastClickedTVTEdit := Sender as TVTEdit;
                  FLastClickedEdit := nil;
                  APopupMenu := pmStandardControlRefVars;
                  TempValue := AEditingAction^.FindSubControlOptions.InitialRectangle.Right;
                  AHint := AHint + #13#10 + TempValue + ' = ' + EvaluateReplacements(TempValue);
                end;

                CFindSubControl_InitialRectangle_Bottom_PropItemIndex:
                begin
                  FLastClickedTVTEdit := Sender as TVTEdit;
                  FLastClickedEdit := nil;
                  APopupMenu := pmStandardControlRefVars;
                  TempValue := AEditingAction^.FindSubControlOptions.InitialRectangle.Bottom;
                  AHint := AHint + #13#10 + TempValue + ' = ' + EvaluateReplacements(TempValue);
                end;
              end;
            end;
          end; //init rect

          CFindSubControl_MatchBitmapFiles_PropIndex, CFindSubControl_MatchPrimitiveFiles_PropIndex, CFindSubControl_SourceFileName_PropIndex:
          begin
            if Sender is TVTEdit then
              FLastClickedTVTEdit := Sender as TVTEdit
            else
              FLastClickedTVTEdit := nil;

            FLastClickedEdit := nil;
            APopupMenu := pmPathReplacements;

            if (ANodeLevel = CPropertyItemLevel) and (APropertyIndex = CFindSubControl_MatchBitmapFiles_PropIndex) then
              AHint := 'If a file path starts with "' + CExtBmp_Prefix + '" (no quotes), the file must exist in the externally rendered in-mem file system.';

            AHint := AHint + #13#10 + '$AppDir$ replacement is available';

            if APropertyIndex = CFindSubControl_MatchPrimitiveFiles_PropIndex then
            begin
              AHint := AHint + #13#10;
              AHint := AHint + 'Every .pmtv file has an index in this list.' + #13#10 +
                               'It can be read using the $FileIndex$ variable, from any primitives property.' + #13#10#13#10 +
                               'For example if a primitives file requires two colors, $BorderLineColor$ and $BorderRectColor$,' + #13#10 +
                               'They can be set using a SetVar action as, e.g.:' + #13#10#13#10 +
                               '429419$#4#5$D3D3D3$#4#5$150088$#4#5$' + #13#10 +
                               '56C221$#4#5$EBEBEB$#4#5$277FFF$#4#5$' + #13#10#13#10 +
                               'Then used by the primitives file as:' + #13#10#13#10 +
                               '$GetTextItem($BorderLineColor$,$FileIndex$)$' + #13#10 +
                               '$GetTextItem($BorderRectColor$,$FileIndex$)$';
            end;
          end;

          CFindSubControl_MatchByHistogramSettings_PropIndex:
          begin
            AHint := CGetPropertyHint_FindSubControlMatchByHistogramSettings_Items[AItemIndex];
          end;

          CFindSubControl_RenderingInBrowserSettings_PropIndex:
          begin
            AHint := CGetPropertyHint_FindSubControlRenderingInBrowserSettings_Items[AItemIndex];
          end;

          CFindSubControl_GPUSettings_PropIndex:
          begin
            AHint := CGetPropertyHint_FindSubControlGPUSettings_Items[AItemIndex];
            if ANodeLevel = CPropertyItemLevel then
              if AItemIndex = CFindSubControl_GPUSettings_OpenCLPath_PropItemIndex then
              begin
                if Sender is TVTEdit then
                  FLastClickedTVTEdit := Sender as TVTEdit
                else
                  FLastClickedTVTEdit := nil;

                FLastClickedEdit := nil;
                APopupMenu := pmPathReplacements;
                AHint := AHint + #13#10;
                AHint := AHint + '$AppDir$ replacement is available';
              end;
          end;

          CFindSubControl_ImageEffectSettings_PropIndex:
          begin
            AHint := CGetPropertyHint_FindSubControlImageEffectSettings_Items[AItemIndex];
          end;
        end; //case
      end; //FindSubControl

      acCallTemplate:
      begin
        case APropertyIndex of
          CCallTemplate_TemplateFileName_PropIndex:
          begin
            if Sender is TVTEdit then
              FLastClickedTVTEdit := Sender as TVTEdit
            else
              FLastClickedTVTEdit := nil;

            FLastClickedEdit := nil;
            APopupMenu := pmPathReplacements;
            AHint := '$AppDir$ replacement is available';
          end;

          CCallTemplate_CallTemplateLoop_PropIndex:
            AHint := CGetPropertyHint_CallTemplateLoop_Items[AItemIndex];
        end;
      end;

      acWindowOperations:
      begin
        APopupMenu := pmWindowOperationsEditors;

        case APropertyIndex of
          CWindowOperations_NewX_PropItemIndex, CWindowOperations_NewY_PropItemIndex:
          begin
            MenuItem_SetFromControlLeftAndTop.Enabled := True;
            MenuItem_SetFromControlWidthAndHeight.Enabled := False;
          end;

          CWindowOperations_NewWidth_PropItemIndex, CWindowOperations_NewHeight_PropItemIndex:
          begin
            MenuItem_SetFromControlLeftAndTop.Enabled := False;
            MenuItem_SetFromControlWidthAndHeight.Enabled := True;
          end;

          else
          begin
            MenuItem_SetFromControlLeftAndTop.Enabled := False;
            MenuItem_SetFromControlWidthAndHeight.Enabled := False;
          end;
        end;
      end;

      acPlugin:
        if APropertyIndex > CPlugin_FileName_PropIndex then
          AHint := FastReplace_45ToReturn(GetPluginPropertyAttribute(AEditingAction.PluginOptions.ListOfPropertiesAndTypes, CPluginPropertyAttr_Hint, APropertyIndex));

      else
        ;
    end;  //case ALiveEditingActionType
end;


procedure TfrClickerActions.HandleOnOIEditorAssignMenuAndTooltip(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; var APopupMenu: TPopupMenu; var AHint: string; var AShowHint: Boolean);
begin
  APopupMenu := nil;
  AHint := '';
  AShowHint := True;

  try
    case ACategoryIndex of
      CCategory_Common:
        ;

      CCategory_ActionSpecific:
        OIEditorAssignMenuAndTooltip_ActionSpecific(FEditingAction, CurrentlyEditingActionType, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, Sender, APopupMenu, AHint, AShowHint);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          OIEditorAssignMenuAndTooltip_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, Sender, APopupMenu, AHint, AShowHint);
    end;
  except
  end;
end;


procedure OIGetFileDialogSettings_ActionSpecific(ALiveEditingActionType: TClkAction; ACategoryIndex, APropertyIndex, AItemIndex: Integer; ABMPsDir: string; var AFilter, AInitDir: string);
var
  EditingActionType: Integer;
begin
  EditingActionType := Integer(ALiveEditingActionType);
  if EditingActionType = CClkUnsetAction then
    Exit;

  case ALiveEditingActionType of
    acExecApp:
      if APropertyIndex = CExecApp_PathToApp_PropIndex then
      begin
        AFilter := 'Executable files (*.exe)|*.exe|All files (*.*)|*.*';
        AInitDir := ExtractFileDir(ParamStr(0));
      end;

    acFindSubControl:
    begin
      case APropertyIndex of
        CFindSubControl_MatchBitmapFiles_PropIndex:
        begin
          AFilter := 'Bitmap files (*.bmp)|*.bmp|All files (*.*)|*.*';
          AInitDir := ABMPsDir;
        end;

        CFindSubControl_MatchPrimitiveFiles_PropIndex:
        begin
          AFilter := 'Primitives files (*.pmtv)|*.pmtv|All files (*.*)|*.*';
          AInitDir := ABMPsDir;
        end;
      end;
    end;

    acCallTemplate:
      if APropertyIndex = CCallTemplate_TemplateFileName_PropIndex then
      begin
        AFilter := 'Clicker template files (*.clktmpl)|*.clktmpl|All files (*.*)|*.*';
        AInitDir := ExtractFilePath(ParamStr(0)) + '\ActionTemplates';
      end;

    else
      Exit;
  end;
end;


procedure TfrClickerActions.HandleOnOIGetFileDialogSettings(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var AFilter, AInitDir: string);
begin
  AFilter := '';
  AInitDir := '';

  try
    case ACategoryIndex of
      CCategory_Common:
        ;

      CCategory_ActionSpecific:
        OIGetFileDialogSettings_ActionSpecific(CurrentlyEditingActionType, ACategoryIndex, APropertyIndex, AItemIndex, FBMPsDir, AFilter, AInitDir);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          OIGetFileDialogSettings_ActionSpecific(FEditTemplateOptions_EditingAction.ActionOptions.Action, ACategoryIndex, APropertyIndex, AItemIndex, FBMPsDir, AFilter, AInitDir);
    end;
  except
  end;
end;


procedure TfrClickerActions.AddGPUPlatformsAsMenuItems(AOIEditorMenu: TPopupMenu; AMenuHandler: TNotifyEvent; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; AEditingAction: PClkActionRec);
var
  OpenCLDll: TOpenCL;
  TempMenuItem: TMenuItem;
  i, Error: Integer;
  PlatformCount: cl_uint;
  PlatformIDs: ^cl_platform_id_arr;
  PlatformInfo, PathFromAction: string;
begin
  OpenCLDll := TOpenCL.Create;
  try
    PathFromAction := EvaluateReplacements(ResolveTemplatePath(AEditingAction^.FindSubControlOptions.GPUSettings.OpenCLPath));
    if AEditingAction^.FindSubControlOptions.GPUSettings.OpenCLPath <> '' then
    begin
      if OpenCLDll.ExpectedDllLocation <> PathFromAction then
      begin
        OpenCLDll.ExpectedDllFileName := ExtractFileName(PathFromAction);
        OpenCLDll.ExpectedDllDir := ExtractFileDir(PathFromAction);
        OpenCLDll.LoadOpenCLLibrary;
      end;
    end;

    if not OpenCLDll.Loaded then
    begin
      DoOnAddToLog('OpenCL not available. The dll is expected to exist at ' + PathFromAction);
      TempMenuItem := AddMenuItemToPopupMenu(AOIEditorMenu, 'No platforms available', nil, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
      TempMenuItem.Enabled := False;
      Exit;
    end;

    Error := OpenCLDll.clGetPlatformIDs(0, nil, @PlatformCount);
    if Error < CL_SUCCESS then
    begin
      DoOnAddToLog('Error getting platforms count: ' + CLErrorToStr(Error));
      Exit;
    end;

    GetMem(PlatformIDs, PlatformCount * SizeOf(cl_platform_id));
    try
      Error := OpenCLDll.clGetPlatformIDs(PlatformCount, Pcl_platform_id(PlatformIDs), nil);
      if Error < CL_SUCCESS then
      begin
        DoOnAddToLog('Error getting platforms IDs: ' + CLErrorToStr(Error));
        Exit;
      end;

      for i := 0 to PlatformCount - 1 do
      begin
        PlatformInfo := GetGPUPlatformInfo(DoOnAddToLog, OpenCLDll, PlatformIDs[i], CL_PLATFORM_NAME);
        if (AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatformIDType = tpitIndex) and Assigned(AMenuHandler) then //verify AMenuHandler, because this function is also used to get the list of platforms for something else
          PlatformInfo := '[' + IntToStr(i) + ']  ' + PlatformInfo;

        TempMenuItem := AddMenuItemToPopupMenu(AOIEditorMenu, PlatformInfo, AMenuHandler, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
        POIMenuItemData(TempMenuItem.Tag)^.UserDataIndex := i; //this will be used by menu item handlers
      end;
    finally
      Freemem(PlatformIDs, PlatformCount * SizeOf(cl_platform_id));
    end;
  finally
    OpenCLDll.Free;
  end;
end;


procedure TfrClickerActions.AddGPUDevicesAsMenuItems(AOIEditorMenu: TPopupMenu; AMenuHandler: TNotifyEvent; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; AEditingAction: PClkActionRec);
var
  OpenCLDll: TOpenCL;
  TempPlatformMenuItem, TempDeviceMenuItem: TMenuItem;
  i, j, Error: Integer;
  PlatformCount, DeviceCount: cl_uint;
  PlatformIDs: ^cl_platform_id_arr;
  DeviceIDs: ^cl_device_id_arr;
  PlatformInfo, DeviceInfo, PathFromAction: string;
  DevType: cl_device_type; //GPU
begin
  OpenCLDll := TOpenCL.Create;
  try
    PathFromAction := EvaluateReplacements(ResolveTemplatePath(AEditingAction^.FindSubControlOptions.GPUSettings.OpenCLPath));
    if AEditingAction^.FindSubControlOptions.GPUSettings.OpenCLPath <> '' then
      if OpenCLDll.ExpectedDllLocation <> PathFromAction then
      begin
        OpenCLDll.ExpectedDllFileName := ExtractFileName(PathFromAction);
        OpenCLDll.ExpectedDllDir := ExtractFileDir(PathFromAction);
        OpenCLDll.LoadOpenCLLibrary;
      end;

    if not OpenCLDll.Loaded then
    begin
      DoOnAddToLog('OpenCL not available. The dll is expected to exist at ' + PathFromAction);
      TempPlatformMenuItem := AddMenuItemToPopupMenu(AOIEditorMenu, 'No platforms available', nil, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
      TempPlatformMenuItem.Enabled := False;
      Exit;
    end;

    Error := OpenCLDll.clGetPlatformIDs(0, nil, @PlatformCount);
    if Error < CL_SUCCESS then
    begin
      DoOnAddToLog('Error getting platforms count: ' + CLErrorToStr(Error));
      Exit;
    end;

    GetMem(PlatformIDs, PlatformCount * SizeOf(cl_platform_id));
    try
      Error := OpenCLDll.clGetPlatformIDs(PlatformCount, Pcl_platform_id(PlatformIDs), nil);
      if Error < CL_SUCCESS then
      begin
        DoOnAddToLog('Error getting platforms IDs: ' + CLErrorToStr(Error));
        Exit;
      end;

      for i := 0 to PlatformCount - 1 do
      begin
        PlatformInfo := GetGPUPlatformInfo(DoOnAddToLog, OpenCLDll, PlatformIDs[i], CL_PLATFORM_NAME);
        if (AEditingAction^.FindSubControlOptions.GPUSettings.TargetPlatformIDType = tpitIndex) and Assigned(AMenuHandler) then //verify AMenuHandler, because this function is also used to get the list of platforms for something else
          PlatformInfo := '[' + IntToStr(i) + ']  ' + PlatformInfo;

        TempPlatformMenuItem := AddMenuItemToPopupMenu(AOIEditorMenu, PlatformInfo, nil, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
        POIMenuItemData(TempPlatformMenuItem.Tag)^.UserDataIndex := i; //not used here, because there is not handler for these items

        DevType := CL_DEVICE_TYPE_GPU;
        Error := OpenCLDll.clGetDeviceIDs(PlatformIDs[i], DevType, 0, nil, @DeviceCount);
        if Error < CL_SUCCESS then
        begin
          DoOnAddToLog('Error getting devices count: ' + CLErrorToStr(Error));
          Exit;
        end;

        GetMem(DeviceIDs, DeviceCount * SizeOf(cl_device_id));
        try
          Error := OpenCLDll.clGetDeviceIDs(PlatformIDs[i], DevType, DeviceCount, Pcl_device_id(DeviceIDs), nil);
          if Error < CL_SUCCESS then
          begin
            DoOnAddToLog('Error getting device IDs: ' + CLErrorToStr(Error));
            Exit;
          end;

          for j := 0 to DeviceCount - 1 do
          begin
            DeviceInfo := GetGPUDeviceInfo(DoOnAddToLog, OpenCLDll, DeviceIDs[j], CL_DEVICE_NAME);
            TempDeviceMenuItem := AddMenuItemToAnotherMenuItem(AOIEditorMenu,TempPlatformMenuItem, DeviceInfo, AMenuHandler, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
            POIMenuItemData(TempDeviceMenuItem.Tag)^.UserDataIndex := j; //this will be used by menu item handlers
          end;
        finally
          Freemem(DeviceIDs, DeviceCount * SizeOf(cl_device_id));
        end;
      end;
    finally
      Freemem(PlatformIDs, PlatformCount * SizeOf(cl_platform_id));
    end;
  finally
    OpenCLDll.Free;
  end;
end;


procedure TfrClickerActions.CreateMenuWithExtMemBitmaps(ABmpTempMenuItem: TMenuItem; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; AEditingAction: PClkActionRec; ABmpItemHandler, ABrowseWithPreviewHandler: TNotifyEvent; AWithExtMemPrefixOnly: Boolean);
var
  TempListOfExternallyRenderedImages: TStringList;
  i: Integer;
  BrowseMenuItem, BmpMenuItem: TMenuItem;
begin
  TempListOfExternallyRenderedImages := TStringList.Create;
  try
    TempListOfExternallyRenderedImages.LineBreak := #13#10;
    DoOnGetListOfExternallyRenderedImages(TempListOfExternallyRenderedImages);

    if TempListOfExternallyRenderedImages.Count = 0 then
    begin
      BrowseMenuItem := AddMenuItemToAnotherMenuItem(FOIEditorMenu, ABmpTempMenuItem, 'No files in externally rendered In-Mem file system', MenuItem_NoImageSourceInInMemPropertyListClick, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
      BrowseMenuItem.Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstFindSubControlProperties, CFindSubControl_ImageSourceFileNameLocation_PropIndex);
    end
    else
    begin
      BrowseMenuItem := AddMenuItemToAnotherMenuItem(FOIEditorMenu, ABmpTempMenuItem, 'Browse with preview...', ABrowseWithPreviewHandler,
        ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
      BrowseMenuItem.Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstMatchPrimitiveFilesMenu, 0);

      AddMenuItemToAnotherMenuItem(FOIEditorMenu, ABmpTempMenuItem, '-', nil, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

      for i := 0 to TempListOfExternallyRenderedImages.Count - 1 do
        if not AWithExtMemPrefixOnly or (AWithExtMemPrefixOnly and (Pos(CExtBmp_PrefixUpperCase, UpperCase(TempListOfExternallyRenderedImages.Strings[i])) = 1)) then
        begin
          BmpMenuItem := AddMenuItemToAnotherMenuItem(FOIEditorMenu, ABmpTempMenuItem, TempListOfExternallyRenderedImages.Strings[i], ABmpItemHandler,
            ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
          BmpMenuItem.Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstFindSubControlProperties, CFindSubControl_ImageSourceFileNameLocation_PropIndex);
        end;
    end;
  finally
    TempListOfExternallyRenderedImages.Free;
  end;
end;


procedure TfrClickerActions.OIArrowEditorClick_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer);
var
  tp: TPoint;
  i: Integer;
  s: string;
  BMPTxt: TClkFindControlMatchBitmapText;
  ItemIndexMod, ItemIndexDiv: Integer;
  TempListOfExternallyRenderedImages: TStringList;
  BmpTempMenuItem: TMenuItem;
  {$IFDEF MemPlugins}
    TempListOfMemPlugins: TStringList;
    TempMenuItem: TMenuItem;
    PluginMenuItem: TMenuItem;
    PluginPath: string;
    PluginsFoundInInMemFS: Boolean;
  {$ENDIF}
begin
  if AEditingAction = nil then
    Exit;

  case ALiveEditingActionType of
    acFindControl:
    begin
      case APropertyIndex of
        CFindControl_MatchText_PropIndex, CFindControl_MatchClassName_PropIndex:
        begin
          FOIEditorMenu.Items.Clear;

          AddMenuItemToPopupMenu(FOIEditorMenu, 'Copy values from preview window', MenuItem_CopyTextAndClassFromPreviewWindowClick,
            ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

          AddMenuItemToPopupMenu(FOIEditorMenu, 'Copy values from window interpreter', MenuItem_CopyTextAndClassFromWinInterpWindowClick,
            ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

          AddMenuItemToPopupMenu(FOIEditorMenu, 'Set to system menu', MenuItem_SetTextAndClassAsSystemMenuClick,
            ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

          GetCursorPos(tp);
          FOIEditorMenu.PopUp(tp.X, tp.Y);
        end;

      end; //case
    end;

    acFindSubControl:
    begin
      case APropertyIndex of
        CFindSubControl_MatchBitmapText_PropIndex:
          case ANodeLevel of
            CPropertyLevel:
            begin
              FOIEditorMenu.Items.Clear;

              AddMenuItemToPopupMenu(FOIEditorMenu, 'Add default font profile', MenuItem_AddFontProfileToPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

              AddMenuItemToPopupMenu(FOIEditorMenu, 'Add two commonly used font profiles (with Antialiased and ClearType)', MenuItem_AddFontProfileWithAntialiasedAndClearTypeToPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

              AddMenuItemToPopupMenu(FOIEditorMenu, 'Add three commonly used font profiles (with NonAntialiased, Antialiased and ClearType)', MenuItem_AddFontProfileWithNonAntialiasedAndAntialiasedAndClearTypeToPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

              if Length(AEditingAction^.FindSubControlOptions.MatchBitmapText) > 0 then
                AddMenuItemToPopupMenu(FOIEditorMenu, '-', nil, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

              for i := 0 to Length(AEditingAction^.FindSubControlOptions.MatchBitmapText) - 1 do
              begin
                BMPTxt := AEditingAction^.FindSubControlOptions.MatchBitmapText[i];
                s := '  Name: ' + BMPTxt.ProfileName + '  (' + BMPTxt.FontName + ', ' + IntToStr(BMPTxt.FontSize) + ', ' + BMPTxt.ForegroundColor + ', ' + BMPTxt.BackgroundColor + ')';
                AddMenuItemToPopupMenu(FOIEditorMenu, 'Remove font profile[' + IntToStr(i) + ']  ' + s, MenuItem_RemoveFontProfileFromPropertyListClick,
                  ANodeLevel, ACategoryIndex, APropertyIndex, i, AEditingAction);  //ItemIndex is not the real one. It points to the profile index.
              end;

              if Length(AEditingAction^.FindSubControlOptions.MatchBitmapText) > 0 then
                AddMenuItemToPopupMenu(FOIEditorMenu, '-', nil, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

              for i := 0 to Length(AEditingAction^.FindSubControlOptions.MatchBitmapText) - 1 do
              begin
                BMPTxt := AEditingAction^.FindSubControlOptions.MatchBitmapText[i];
                s := '  Name: ' + BMPTxt.ProfileName + '  (' + BMPTxt.FontName + ', ' + IntToStr(BMPTxt.FontSize) + ', ' + BMPTxt.ForegroundColor + ', ' + BMPTxt.BackgroundColor + ')';
                AddMenuItemToPopupMenu(FOIEditorMenu, 'Duplicate font profile[' + IntToStr(i) + ']  ' + s, MenuItem_DuplicateFontProfileClick,
                  ANodeLevel, ACategoryIndex, APropertyIndex, i, AEditingAction);  //ItemIndex is not the real one. It points to the profile index.
              end;

              GetCursorPos(tp);
              FOIEditorMenu.PopUp(tp.X, tp.Y);
            end;

            CPropertyItemLevel:
            begin
              ItemIndexMod := AItemIndex mod CPropCount_FindSubControlMatchBitmapText;
              ItemIndexDiv := AItemIndex div CPropCount_FindSubControlMatchBitmapText;

              if ItemIndexMod = CFindSubControl_MatchBitmapText_ProfileName_PropItemIndex then
                if Length(AEditingAction^.FindSubControlOptions.MatchBitmapText) > 1 then  //add only if there are at least two profiles
                begin
                  FOIEditorMenu.Items.Clear;

                  AddMenuItemToPopupMenu(FOIEditorMenu, 'Move font profile up', MenuItem_MoveFontProfileUpInPropertyListClick,
                    ANodeLevel, ACategoryIndex, APropertyIndex, ItemIndexDiv, AEditingAction); //sending the profile index through item index arg

                  AddMenuItemToPopupMenu(FOIEditorMenu, 'Move font profile down', MenuItem_MoveFontProfileDownInPropertyListClick,
                    ANodeLevel, ACategoryIndex, APropertyIndex, ItemIndexDiv, AEditingAction); //sending the profile index through item index arg

                  GetCursorPos(tp);
                  FOIEditorMenu.PopUp(tp.X, tp.Y);
                end;
            end;
          end; //case

        CFindSubControl_MatchBitmapFiles_PropIndex:
        begin
          case ANodeLevel of
            CPropertyLevel:
            begin
              FOIEditorMenu.Items.Clear;

              /////////////////////////////////// To add to menu item handlers
              /////////////////////////////////// Add also to browse buttons

              AddMenuItemToPopupMenu(FOIEditorMenu, 'Add file(s) to this list...', MenuItem_AddBMPFilesToPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

              BmpTempMenuItem := AddMenuItemToPopupMenu(FOIEditorMenu, 'Add externally rendered file(s) to this list...', nil {MenuItem_AddExtBMPFilesToPropertyListClick},
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

              AddMenuItemToPopupMenu(FOIEditorMenu, 'Remove all files from this list...', MenuItem_RemoveAllBMPFilesFromPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

              CreateMenuWithExtMemBitmaps(BmpTempMenuItem, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction, MenuItem_SetFileNameFromInMemPropertyListAddToBmpFilesClick, MenuItem_BrowseFileNameFromInMemPropertyListAddToBmpFilesClick, True);

              GetCursorPos(tp);
              FOIEditorMenu.PopUp(tp.X, tp.Y);
            end;

            CPropertyItemLevel:
            begin
              FOIEditorMenu.Items.Clear;

              AddMenuItemToPopupMenu(FOIEditorMenu, 'Browse...', MenuItem_BrowseBMPFileFromPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
              FOIEditorMenu.Items.Items[FOIEditorMenu.Items.Count - 1].Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstMatchPrimitiveFilesMenu, 0);

              BmpTempMenuItem := AddMenuItemToPopupMenu(FOIEditorMenu, 'Browse externally rendered file(s) to this list...', nil {MenuItem_AddExtBMPFilesToPropertyListClick},
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
              CreateMenuWithExtMemBitmaps(BmpTempMenuItem, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction, MenuItem_SetFileNameFromInMemPropertyListUpdateBmpFilesClick, MenuItem_BrowseFileNameFromInMemPropertyListUpdateBmpFilesClick, True);
              FOIEditorMenu.Items.Items[FOIEditorMenu.Items.Count - 1].Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstMatchPrimitiveFilesMenu, 0);

              AddMenuItemToPopupMenu(FOIEditorMenu, 'Remove file from list...', MenuItem_RemoveBMPFileFromPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
              FOIEditorMenu.Items.Items[FOIEditorMenu.Items.Count - 1].Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstMatchPrimitiveFilesMenu, 1);

              AddMenuItemToPopupMenu(FOIEditorMenu, 'Move file up (one position)', MenuItem_MoveBMPFileUpInPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
              FOIEditorMenu.Items.Items[FOIEditorMenu.Items.Count - 1].Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstMatchPrimitiveFilesMenu, 2);

              AddMenuItemToPopupMenu(FOIEditorMenu, 'Move file down (one position)', MenuItem_MoveBMPFileDownInPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
              FOIEditorMenu.Items.Items[FOIEditorMenu.Items.Count - 1].Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstMatchPrimitiveFilesMenu, 3);

              GetCursorPos(tp);
              FOIEditorMenu.PopUp(tp.X, tp.Y);
            end;

            else
              ;
          end;
        end;

        CFindSubControl_MatchPrimitiveFiles_PropIndex:
        begin
          case ANodeLevel of
            CPropertyLevel:
            begin
              FOIEditorMenu.Items.Clear;

              AddMenuItemToPopupMenu(FOIEditorMenu, 'Add existing file(s) to this list...', MenuItem_AddExistingPrimitiveFilesToPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

              AddMenuItemToPopupMenu(FOIEditorMenu, 'Add new file to this list...', MenuItem_AddNewPrimitiveFilesToPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

              AddMenuItemToPopupMenu(FOIEditorMenu, 'Remove all files from this list...', MenuItem_RemoveAllPrimitiveFilesFromPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

              GetCursorPos(tp);
              FOIEditorMenu.PopUp(tp.X, tp.Y);
            end;

            CPropertyItemLevel:
            begin
              FOIEditorMenu.Items.Clear;

              AddMenuItemToPopupMenu(FOIEditorMenu, 'Browse...', MenuItem_BrowsePrimitiveFileFromPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
              FOIEditorMenu.Items.Items[FOIEditorMenu.Items.Count - 1].Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstMatchPrimitiveFilesMenu, 0);

              AddMenuItemToPopupMenu(FOIEditorMenu, 'Remove file from list...', MenuItem_RemovePrimitiveFileFromPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
              FOIEditorMenu.Items.Items[FOIEditorMenu.Items.Count - 1].Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstMatchPrimitiveFilesMenu, 1);

              AddMenuItemToPopupMenu(FOIEditorMenu, 'Move file up (one position)', MenuItem_MovePrimitiveFileUpInPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
              FOIEditorMenu.Items.Items[FOIEditorMenu.Items.Count - 1].Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstMatchPrimitiveFilesMenu, 2);

              AddMenuItemToPopupMenu(FOIEditorMenu, 'Move file down (one position)', MenuItem_MovePrimitiveFileDownInPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
              FOIEditorMenu.Items.Items[FOIEditorMenu.Items.Count - 1].Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstMatchPrimitiveFilesMenu, 3);

              AddMenuItemToPopupMenu(FOIEditorMenu, '-', nil,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

              AddMenuItemToPopupMenu(FOIEditorMenu, 'Save file', MenuItem_SavePrimitiveFileInPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
              FOIEditorMenu.Items.Items[FOIEditorMenu.Items.Count - 1].Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstMatchPrimitiveFilesMenu, 4);

              AddMenuItemToPopupMenu(FOIEditorMenu, 'Save file as...', MenuItem_SavePrimitiveFileAsInPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
              FOIEditorMenu.Items.Items[FOIEditorMenu.Items.Count - 1].Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstMatchPrimitiveFilesMenu, 5);

              AddMenuItemToPopupMenu(FOIEditorMenu, '-', nil,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

              AddMenuItemToPopupMenu(FOIEditorMenu, 'Discard changes and reload file...', MenuItem_DiscardChangesAndReloadPrimitiveFileInPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
              FOIEditorMenu.Items.Items[FOIEditorMenu.Items.Count - 1].Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstMatchPrimitiveFilesMenu, 6);

              GetCursorPos(tp);
              FOIEditorMenu.PopUp(tp.X, tp.Y);
            end;

            else
              ;
          end;
        end;  //CFindSubControl_MatchPrimitiveFiles_PropIndex

        CFindSubControl_SourceFileName_PropIndex:
        begin
          if ANodeLevel = CPropertyLevel then
          begin
            FOIEditorMenu.Items.Clear;

            case AEditingAction^.FindSubControlOptions.ImageSourceFileNameLocation of
              isflDisk:
              begin
                AddMenuItemToPopupMenu(FOIEditorMenu, 'Browse...', MenuItem_BrowseImageSourceFromPropertyListClick,
                  ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
                FOIEditorMenu.Items.Items[FOIEditorMenu.Items.Count - 1].Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstMatchPrimitiveFilesMenu, 0);
              end;

              isflMem:
              begin
                BmpTempMenuItem := AddMenuItemToPopupMenu(FOIEditorMenu, 'Browse externally rendered file(s) to this list...', nil {MenuItem_AddExtBMPFilesToPropertyListClick},
                  ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

                CreateMenuWithExtMemBitmaps(BmpTempMenuItem, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction, MenuItem_SetFileNameFromInMemPropertyListClick, MenuItem_BrowseFileNameFromInMemPropertyListClick, False);
                FOIEditorMenu.Items.Items[FOIEditorMenu.Items.Count - 1].Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstMatchPrimitiveFilesMenu, 0);
              end;
            end;

            GetCursorPos(tp);
            FOIEditorMenu.PopUp(tp.X, tp.Y);
          end;
        end; //SourceFileName

        CFindSubControl_RenderingInBrowserSettings_PropIndex:
          if ANodeLevel = CPropertyItemLevel then
          begin
            case AItemIndex of
              CFindSubControl_RenderingInBrowserSettings_ActionForSendingRequest_PropItemIndex:
                LoadListOfAvailableActionsForFindSubControlRenderingRequest(APropertyIndex, AEditingAction, [acExecApp, acCallTemplate, acPlugin], AvailableFindSubControlSendRequestActionClick);

              CFindSubControl_RenderingInBrowserSettings_PluginActionForReceivingBitmaps_PropItemIndex:
                LoadListOfAvailableActionsForFindSubControlRenderingRequest(APropertyIndex, AEditingAction, [acPlugin], AvailableFindSubControlRcvBmpPluginActionClick);
            end;

            FPmLocalTemplates.PopUp;
          end;

        CFindSubControl_GPUSettings_PropIndex:
          if ANodeLevel = CPropertyItemLevel then
          begin
            case AItemIndex of
              CFindSubControl_GPUSettings_OpenCLPath_PropItemIndex:
              begin
                FOIEditorMenu.Items.Clear;
                AddMenuItemToPopupMenu(FOIEditorMenu, 'Browse for OpenCL.dll...', MenuItem_BrowseOpenCLFileNameForPropertyListClick,
                  ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
              end;

              CFindSubControl_GPUSettings_TargetPlatform_PropItemIndex:
              begin
                FOIEditorMenu.Items.Clear;
                AddGPUPlatformsAsMenuItems(FOIEditorMenu, MenuItem_SetGPUPlatformFromInMemPropertyListClick, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
              end;

              CFindSubControl_GPUSettings_TargetDevice_PropItemIndex:
              begin
                FOIEditorMenu.Items.Clear;
                AddGPUDevicesAsMenuItems(FOIEditorMenu, MenuItem_SetGPUDeviceFromInMemPropertyListClick, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
              end;
            end;

            FOIEditorMenu.PopUp;
          end;

        CFindSubControl_ImageEffectSettings_PropIndex:
          ;
      end; //case APropertyIndex
    end; //FindControl, FindSubControl

    acCallTemplate:
    begin
      case APropertyIndex of
        CCallTemplate_TemplateFileName_PropIndex:
        begin
          LoadListOfAvailableTemplates;
          GetCursorPos(tp);
          FPmLocalTemplates.PopUp(tp.X, tp.Y);
        end;
      end;
    end;

    acLoadSetVarFromFile, acSaveSetVarToFile:
    begin
      case APropertyIndex of
        CLoadSetVarFromFile_FileName_PropIndex:
        begin
          FOIEditorMenu.Items.Clear;
          AddMenuItemToPopupMenu(FOIEditorMenu, 'Browse...', MenuItem_BrowseSetVarFileInPropertyListClick,
              ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

          FOIEditorMenu.PopUp;
        end;

        CLoadSetVarFromFile_SetVarActionName_PropIndex:
        begin
          if AEditingAction = @FClkEditedActionByEditTemplate then
          begin
            AEditingAction^.EditTemplateOptions.WhichTemplate := FEditingAction^.EditTemplateOptions.WhichTemplate;
            AEditingAction^.EditTemplateOptions.TemplateFileName := FEditingAction^.EditTemplateOptions.TemplateFileName;
          end
          else
            AEditingAction^.EditTemplateOptions.WhichTemplate := etwtSelf; //update the action, as this is a LoadSetVar or SaveSetVar action, not an EditTemplate. The flag is used by LoadListOfAvailableSetVarActions.

          LoadListOfAvailableSetVarActions(AEditingAction);
          FPmLocalTemplates.PopUp;
        end;
      end; //case APropertyIndex
    end;

    acPlugin:
    begin
      case APropertyIndex of
        CPlugin_FileName_PropIndex:
        begin
          FOIEditorMenu.Items.Clear;

          {$IFDEF MemPlugins}
            TempMenuItem := AddMenuItemToPopupMenu(FOIEditorMenu, 'Browse from disk...', MenuItem_BrowsePluginFileInPropertyListClick,
              ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
            TempMenuItem.Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstMatchPrimitiveFilesMenu, 0);

            AddMenuItemToPopupMenu(FOIEditorMenu, '-', nil, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

            TempMenuItem := AddMenuItemToPopupMenu(FOIEditorMenu, 'Browse Plugin In-Mem file system...', nil,
              ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
            TempMenuItem.Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstMatchPrimitiveFilesMenu, 0);

            TempListOfMemPlugins := TStringList.Create;
            try
              TempListOfMemPlugins.LineBreak := #13#10;
              DoOnGetListOfInMemPlugins(TempListOfMemPlugins);

              PluginsFoundInInMemFS := False;
              for i := 0 to TempListOfMemPlugins.Count - 1 do
              begin
                PluginPath := Copy(TempListOfMemPlugins.Strings[i], 1, Pos(#8#7, TempListOfMemPlugins.Strings[i]) - 1);

                if UpperCase(ExtractFileExt(PluginPath)) = '.DLL' then
                begin
                  PluginsFoundInInMemFS := True;
                  PluginMenuItem := AddMenuItemToAnotherMenuItem(FOIEditorMenu, TempMenuItem, PluginPath, MenuItem_SetPluginFileNameFromInMemPropertyListClick,
                    ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
                  PluginMenuItem.Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstPluginProperties, CPlugin_FileName_PropIndex);
                end;
              end;

              if not PluginsFoundInInMemFS then
              begin
                PluginMenuItem := AddMenuItemToAnotherMenuItem(FOIEditorMenu, TempMenuItem, 'No plugins in In-Mem file system', MenuItem_NoPluginInInMemPropertyListClick, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
                PluginMenuItem.Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstPluginProperties, CPlugin_FileName_PropIndex);
              end;
            finally
              TempListOfMemPlugins.Free;
            end;

            AddMenuItemToPopupMenu(FOIEditorMenu, '-', nil, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

            TempMenuItem := AddMenuItemToPopupMenu(FOIEditorMenu, 'Load plugin from disk to Plugin In-Mem file system...', MenuItem_LoadPluginFromDiskToPluginInMemFSInPropertyListClick,
              ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
            TempMenuItem.Bitmap := CreateBitmapForMenu(dmClickerIcons.imglstFindSubControlProperties, CFindSubControl_ImageSourceFileNameLocation_PropIndex);
          {$ELSE}
            AddMenuItemToPopupMenu(FOIEditorMenu, 'Browse...', MenuItem_BrowsePluginFileInPropertyListClick,
              ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
          {$ENDIF}

          FOIEditorMenu.PopUp;
        end;

        else
        begin
          if AEditingAction = @FClkEditedActionByEditTemplate then
          begin
            AEditingAction^.EditTemplateOptions.WhichTemplate := FEditingAction^.EditTemplateOptions.WhichTemplate;
            AEditingAction^.EditTemplateOptions.TemplateFileName := FEditingAction^.EditTemplateOptions.TemplateFileName;
          end;

          LoadListOfAvailableActionsForPlugin(APropertyIndex - CPropCount_Plugin, AEditingAction);
          FPmLocalTemplates.PopUp;
        end;
      end; //case APropertyIndex
    end; //plugin

    acEditTemplate:
    begin
      case APropertyIndex of
        CEditTemplate_TemplateFileName_PropIndex:
        begin
          FOIEditorMenu.Items.Clear;
          AddMenuItemToPopupMenu(FOIEditorMenu, 'Browse...', MenuItem_BrowseEditTemplateFileInPropertyListClick,
              ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);

          FOIEditorMenu.PopUp;
        end;

        CEditTemplate_EditedActionName_PropIndex, CEditTemplate_NewActionName_PropIndex:
        begin
          LoadListOfAvailableActionsForEditTemplate(APropertyIndex, AEditingAction);
          FPmLocalTemplates.PopUp;
        end;

        CEditTemplate_EditedActionTimeout_PropIndex:
        begin
          FOIEditorMenu.Items.Clear;

          AddMenuItemToPopupMenu(FOIEditorMenu, '0', MenuItem_SetEditedActionTimeoutFromOI, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, FEditingAction);
          AddMenuItemToPopupMenu(FOIEditorMenu, '1000', MenuItem_SetEditedActionTimeoutFromOI, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, FEditingAction);
          AddMenuItemToPopupMenu(FOIEditorMenu, '10000', MenuItem_SetEditedActionTimeoutFromOI, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, FEditingAction);
          AddMenuItemToPopupMenu(FOIEditorMenu, '30000', MenuItem_SetEditedActionTimeoutFromOI, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, FEditingAction);

          FOIEditorMenu.PopUp;
        end;

        //CEditTemplate_NewActionName_PropIndex:
        //begin
        //
        //end;

        else
          ;
      end; //case APropertyIndex
    end //acEditTemplate

    else
      ;
  end; //case ALiveEditingActionType
end;


procedure TfrClickerActions.HandleOnOIArrowEditorClick(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer);
begin
  try
    case ACategoryIndex of
      CCategory_Common:
        case APropertyIndex of
          CMain_ActionTimeout_PropIndex:
          begin
            FOIEditorMenu.Items.Clear;

            AddMenuItemToPopupMenu(FOIEditorMenu, '0', MenuItem_SetActionTimeoutFromOI, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, FEditingAction);
            AddMenuItemToPopupMenu(FOIEditorMenu, '1000', MenuItem_SetActionTimeoutFromOI, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, FEditingAction);
            AddMenuItemToPopupMenu(FOIEditorMenu, '10000', MenuItem_SetActionTimeoutFromOI, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, FEditingAction);
            AddMenuItemToPopupMenu(FOIEditorMenu, '30000', MenuItem_SetActionTimeoutFromOI, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, FEditingAction);

            FOIEditorMenu.PopUp;
          end
          else
            ;
        end;

      CCategory_ActionSpecific:
        OIArrowEditorClick_ActionSpecific(FEditingAction, CurrentlyEditingActionType, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          OIArrowEditorClick_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);
    end;
  except
  end;
end;


function TfrClickerActions.ResolveTemplatePath(APath: string; ACustomSelfTemplateDir: string = ''; ACustomAppDir: string = ''): string;
begin
  Result := StringReplace(APath, '$TemplateDir$', FFullTemplatesDir, [rfReplaceAll]);

  //if ACustomSelfTemplateDir = '' then
  //  Result := StringReplace(Result, '$SelfTemplateDir$', ExtractFileDir(FFileName), [rfReplaceAll])
  //else
  //  Result := StringReplace(Result, '$SelfTemplateDir$', ACustomSelfTemplateDir, [rfReplaceAll]);

  if ACustomAppDir = '' then
    Result := StringReplace(Result, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll])
  else
    Result := StringReplace(Result, '$AppDir$', ACustomAppDir, [rfReplaceAll]);
end;


procedure TfrClickerActions.OIUserEditorClick_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ARepaintValue: Boolean);
var
  EditingActionType: Integer;
  Condition: string;
  ActionPlugin: TActionPlugin;
  ResolvedPluginPath, CurrentPluginPropertyValue, NewPluginPropertyValue: string;
  ListOfProperties: TStringList;
begin
  if AEditingAction = nil then
    Exit;

  EditingActionType := Integer(ALiveEditingActionType);
  if EditingActionType = CClkUnsetAction then
    Exit;

  case ALiveEditingActionType of
    acExecApp:
      if APropertyIndex = CExecApp_ListOfParams_PropIndex then
      begin
        frClickerExecApp.BringToFront;  //MessageBox(Handle, 'Param list editor', 'Files', MB_ICONINFORMATION);
      end;

    acFindSubControl:
      if APropertyIndex in [CFindSubControl_MatchBitmapFiles_PropIndex, CFindSubControl_MatchPrimitiveFiles_PropIndex] then
      begin
        //MessageBox(Handle, 'File list editor', 'Files', MB_ICONINFORMATION);
        TriggerOnControlsModified;
      end;

    acCallTemplate:
    begin
      case APropertyIndex of
        CCallTemplate_ListOfCustomVarsAndValues_PropIndex:
        begin
          frClickerCallTemplate.SetListOfCustomVariables(AEditingAction^.CallTemplateOptions.ListOfCustomVarsAndValues);
          //TriggerOnControlsModified;
        end;

        CCallTemplate_CallTemplateLoop_PropIndex:
          if AItemIndex = CCallTemplate_CallTemplateLoopProperties_BreakCondition_PropItemIndex then
          begin
            Condition := AEditingAction^.CallTemplateOptions.CallTemplateLoop.BreakCondition;
            if DoOnEditCallTemplateBreakCondition(Condition) then
            begin
              TriggerOnControlsModified(AEditingAction^.CallTemplateOptions.CallTemplateLoop.BreakCondition <> Condition);
              AEditingAction^.CallTemplateOptions.CallTemplateLoop.BreakCondition := Condition;
            end;
          end;
      end;
    end;

    acSetVar:
      if APropertyIndex = CSetVar_ListOfVarNamesValuesAndEvalBefore_PropItemIndex then
      begin
        frClickerSetVar.SetListOfSetVars(AEditingAction^.SetVarOptions);
        frClickerSetVar.BringToFront;
        //MessageBox(Handle, 'SetVar editor', 'Files', MB_ICONINFORMATION);
      end;

    acPlugin:
      if APropertyIndex > CPlugin_FileName_PropIndex then
      begin
        ResolvedPluginPath := ResolveTemplatePath(AEditingAction.PluginOptions.FileName);
        ResolvedPluginPath := EvaluateReplacements(ResolvedPluginPath);

        ActionPlugin.Loaded := False;
        try
          if not ActionPlugin.LoadToEditProperty(ResolvedPluginPath, DoOnGetPluginInMemFS, {$IFDEF MemPlugins} DoOnLoadPluginFromInMemFS {$ELSE} nil {$ENDIF}, FOnAddToLog) then
            DoOnAddToLog('Error loading plugin for editing property.')
          else
          begin
            try
              ListOfProperties := TStringList.Create;
              try
                ListOfProperties.LineBreak := #13#10;
                ListOfProperties.Text := AEditingAction^.PluginOptions.ListOfPropertiesAndValues;
                try
                  if (APropertyIndex - CPropCount_Plugin < ListOfProperties.Count) and (ListOfProperties.Count > 0) then
                  begin
                    CurrentPluginPropertyValue := ListOfProperties.ValueFromIndex[APropertyIndex - CPropCount_Plugin];
                    //DoOnAddToLog('Editing property: ' + ListOfProperties.Names[APropertyIndex - CPropCount_Plugin] + ', with value: ' + CurrentPluginPropertyValue);
                    if ActionPlugin.EditProperty(APropertyIndex - CPropCount_Plugin, CurrentPluginPropertyValue, NewPluginPropertyValue) then
                    begin
                      ListOfProperties.Strings[APropertyIndex - CPropCount_Plugin] := ListOfProperties.Names[APropertyIndex - CPropCount_Plugin] + '=' + NewPluginPropertyValue;
                      //DoOnAddToLog('Editing property: ' + ListOfProperties.Strings[APropertyIndex - CPropCount_Plugin]);
                      AEditingAction^.PluginOptions.ListOfPropertiesAndValues := ListOfProperties.Text;
                      TriggerOnControlsModified(NewPluginPropertyValue <> CurrentPluginPropertyValue);
                    end;
                  end
                  else
                    DoOnAddToLog('[Err: Index out of bounds on editing property: ' + IntToStr(APropertyIndex - 1) + ']');
                except
                  on E: Exception do
                    DoOnAddToLog('bug on editing property [' + IntToStr(APropertyIndex - 1) + ']: ' + E.Message);
                end;
              finally
                ListOfProperties.Free;
              end;

            finally
              ActionPlugin.Unload(FOnAddToLog);
            end;
          end;
        except
          on E: Exception do
            DoOnAddToLog('Ex on editing custom plugin property: ' + E.Message);
        end;
      end;

    acEditTemplate:
      if APropertyIndex = CEditTemplate_EditedActionCondition_PropIndex then
      begin
        Condition := AEditingAction^.EditTemplateOptions.EditedActionCondition;
        if DoOnEditCallTemplateBreakCondition(Condition) then
        begin
          TriggerOnControlsModified(AEditingAction^.EditTemplateOptions.EditedActionCondition <> Condition);
          AEditingAction^.EditTemplateOptions.EditedActionCondition := Condition;
        end;
      end;

    else
      ;
  end;   //case
end;


procedure TfrClickerActions.HandleOnOIUserEditorClick(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ARepaintValue: Boolean);
var
  Condition: string;
begin
  try
    case ACategoryIndex of
      CCategory_Common:
      begin
        if APropertyIndex = CMain_ActionCondition_PropIndex then
        begin
          Condition := FEditingAction^.ActionOptions.ActionCondition;
          if DoOnEditCallTemplateBreakCondition(Condition) then
          begin
            TriggerOnControlsModified(FEditingAction^.ActionOptions.ActionCondition <> Condition);
            FEditingAction^.ActionOptions.ActionCondition := Condition;
          end;
        end;
      end;

      CCategory_ActionSpecific:
        OIUserEditorClick_ActionSpecific(FEditingAction, CurrentlyEditingActionType, ACategoryIndex, APropertyIndex, AItemIndex, ARepaintValue);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          OIUserEditorClick_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, ACategoryIndex, APropertyIndex, AItemIndex, ARepaintValue);
    end; //case
  except
  end;
end;


function TfrClickerActions.OIBrowseFile_ActionSpecific(ALiveEditingActionType: TClkAction; ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  AFilter, ADialogInitDir: string; var Handled: Boolean; AReturnMultipleFiles: Boolean = False): string;
var
  EditingActionType: Integer;
  AOpenDialog: TOpenDialog;
begin
  EditingActionType := Integer(ALiveEditingActionType);
  if EditingActionType = CClkUnsetAction then
    Exit;

  case ALiveEditingActionType of
    acExecApp:
      if APropertyIndex = CExecApp_PathToApp_PropIndex then
      begin
        AOpenDialog := TOpenDialog.Create(nil);
        try
          AOpenDialog.InitialDir := ExtractFileDir(ParamStr(0));
          AOpenDialog.Filter := AFilter;

          if AOpenDialog.Execute then
            Result := AOpenDialog.FileName;

          Handled := True;
        finally
          AOpenDialog.Free;
        end;
      end;

    acFindSubControl:
    begin
      if APropertyIndex = CFindSubControl_MatchBitmapText_PropIndex then
        Handled := True; //do nothing, this is not a file path

      if APropertyIndex = CFindSubControl_MatchBitmapFiles_PropIndex then
      begin
        DoOnSetPictureOpenDialogInitialDir(ADialogInitDir);
        DoOnSetPictureSetOpenDialogMultiSelect;

        if DoOnPictureOpenDialogExecute then
          Result := DoOnGetPictureOpenDialogFileName;

        Handled := True;
      end;

      if APropertyIndex = CFindSubControl_MatchPrimitiveFiles_PropIndex then
      begin
        DoOnSetOpenDialogInitialDir(ADialogInitDir);
        DoOnSetOpenDialogMultiSelect;

        if DoOnOpenDialogExecute(CPrimitivesDialogFilter) then
          Result := DoOnGetOpenDialogFileName;

        Handled := True;
      end;
    end;

    acCallTemplate:
    begin
      case APropertyIndex of
        CCallTemplate_TemplateFileName_PropIndex:
        begin
          DoOnSetOpenDialogInitialDir(ADialogInitDir);
          if DoOnOpenDialogExecute(CTemplateDialogFilter) then
            Result := DoOnGetOpenDialogFileName;

          Handled := True;
        end;

        else
          ;
      end;
    end;

    else
      ;
  end;   //case
end;


function TfrClickerActions.HandleOnOIBrowseFile(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  AFilter, ADialogInitDir: string; var Handled: Boolean; AReturnMultipleFiles: Boolean = False): string;
begin
  Result := '';

  try
    case ACategoryIndex of
      CCategory_Common:
        ;

      CCategory_ActionSpecific:
        Result := OIBrowseFile_ActionSpecific(CurrentlyEditingActionType, ACategoryIndex, APropertyIndex, AItemIndex, AFilter, ADialogInitDir, Handled, AReturnMultipleFiles);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          Result := OIBrowseFile_ActionSpecific(FEditTemplateOptions_EditingAction.ActionOptions.Action, ACategoryIndex, APropertyIndex, AItemIndex, AFilter, ADialogInitDir, Handled, AReturnMultipleFiles);
    end; //case
  except
  end;
end;


procedure TfrClickerActions.OIAfterSpinTextEditorChanging_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewValue: string);
var
  ItemIndexMod, ItemIndexDiv: Integer;
  OldValue: string;
begin
  if AEditingAction = nil then
    Exit;

  if ALiveEditingActionType = acFindControl then
  begin
    case APropertyIndex of
      CFindControl_InitialRectangle_PropIndex:
        if AItemIndex in [CFindControl_InitialRectangle_LeftOffset_PropItemIndex .. CFindControl_InitialRectangle_BottomOffset_PropItemIndex] then
        begin
          OldValue := GetActionValueStr_FindControl_InitialRectangle(AEditingAction, AItemIndex);
          SetActionValueStr_FindControl_InitialRectangle(AEditingAction, ANewValue, AItemIndex);
          TriggerOnControlsModified(ANewValue <> OldValue);

          frClickerFindControl.UpdateSearchAreaLabelsFromKeysOnInitRect(AEditingAction^.FindControlOptions.InitialRectangle);
        end;
    end;
  end;

  if ALiveEditingActionType = acFindSubControl then
  begin
    case APropertyIndex of
      CFindSubControl_InitialRectangle_PropIndex:
        if AItemIndex in [CFindControl_InitialRectangle_LeftOffset_PropItemIndex .. CFindControl_InitialRectangle_BottomOffset_PropItemIndex] then
        begin
          OldValue := GetActionValueStr_FindSubControl_InitialRectangle(AEditingAction, AItemIndex);
          SetActionValueStr_FindSubControl_InitialRectangle(AEditingAction, ANewValue, AItemIndex);
          TriggerOnControlsModified(ANewValue <> OldValue);

          frClickerFindControl.UpdateSearchAreaLabelsFromKeysOnInitRect(AEditingAction^.FindSubControlOptions.InitialRectangle);
        end;

      CFindSubControl_MatchBitmapText_PropIndex:
      begin
        ItemIndexMod := AItemIndex mod CPropCount_FindSubControlMatchBitmapText;
        ItemIndexDiv := AItemIndex div CPropCount_FindSubControlMatchBitmapText;

        case ItemIndexMod of
          CFindSubControl_MatchBitmapText_CropLeft_PropItemIndex:
          begin
            OldValue := AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropLeft;
            AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropLeft := ANewValue;
            if StrToIntDef(ANewValue, 0) < 0 then
              ANewValue := '0';

            frClickerFindControl.BMPTextFontProfiles[ItemIndexDiv].UpdateSelectionLabelsFromCropInfo(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv]);
            TriggerOnControlsModified(ANewValue <> OldValue);
          end;

          CFindSubControl_MatchBitmapText_CropTop_PropItemIndex:
          begin
            OldValue := AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropTop;
            AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropTop := ANewValue;
            if StrToIntDef(ANewValue, 0) < 0 then
              ANewValue := '0';

            frClickerFindControl.BMPTextFontProfiles[ItemIndexDiv].UpdateSelectionLabelsFromCropInfo(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv]);
            TriggerOnControlsModified(ANewValue <> OldValue);
          end;

          CFindSubControl_MatchBitmapText_CropRight_PropItemIndex:
          begin
            OldValue := AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropRight;
            AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropRight := ANewValue;
            if StrToIntDef(ANewValue, 0) < 0 then
              ANewValue := '0';

            frClickerFindControl.BMPTextFontProfiles[ItemIndexDiv].UpdateSelectionLabelsFromCropInfo(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv]);
            TriggerOnControlsModified(ANewValue <> OldValue);
          end;

          CFindSubControl_MatchBitmapText_CropBottom_PropItemIndex:
          begin
            OldValue := AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropBottom;
            AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv].CropBottom := ANewValue;
            if StrToIntDef(ANewValue, 0) < 0 then
              ANewValue := '0';

            frClickerFindControl.BMPTextFontProfiles[ItemIndexDiv].UpdateSelectionLabelsFromCropInfo(AEditingAction^.FindSubControlOptions.MatchBitmapText[ItemIndexDiv]);
            TriggerOnControlsModified(ANewValue <> OldValue);
          end;
        end;
      end; //MatchBitmapText

      CFindSubControl_MatchBitmapAlgorithmSettings_PropIndex:
      begin
        OldValue := GetActionValueStr_FindSubControl_MatchBitmapAlgorithmSettings(AEditingAction, AItemIndex);
        SetActionValueStr_FindSubControl_MatchBitmapAlgorithmSettings(AEditingAction, ANewValue, AItemIndex);
        TriggerOnControlsModified(ANewValue <> OldValue);

        frClickerFindControl.UpdateSearchAreaLabelsFromKeysOnInitRect(AEditingAction^.FindSubControlOptions.InitialRectangle); //call this, to update the grid
      end;

      CFindSubControl_MatchByHistogramSettings_PropIndex:
        if AItemIndex in [CFindSubControl_MatchByHistogramSettings_MinPercentColorMatch_PropItemIndex .. CFindSubControl_MatchByHistogramSettings_MostSignificantColorCountInBackgroundBmp_PropItemIndex] then
        begin
          OldValue := GetActionValueStr_FindSubControl_MatchByHistogramSettings(AEditingAction, AItemIndex);
          SetActionValueStr_FindSubControl_MatchByHistogramSettings(AEditingAction, ANewValue, AItemIndex);
          TriggerOnControlsModified(ANewValue <> OldValue);
        end;

      CFindSubControl_RenderingInBrowserSettings_PropIndex:
        if AItemIndex in [CFindSubControl_RenderingInBrowserSettings_ReceivingBitmapsTimeout_PropItemIndex] then
        begin
          OldValue := GetActionValueStr_FindSubControl_RenderingInBrowserSettings(AEditingAction, AItemIndex);
          SetActionValueStr_FindSubControl_RenderingInBrowserSettings(AEditingAction, ANewValue, AItemIndex);
          TriggerOnControlsModified(ANewValue <> OldValue);
        end;

      CFindSubControl_GPUSettings_PropIndex:
        ;//if AItemIndex in [CFindSubControl_GPUSettings_TargetPlatform_PropItemIndex] then
        //begin
        //  OldValue := GetActionValueStr_FindSubControl_GPUSettings(AEditingAction, AItemIndex);
        //  SetActionValueStr_FindSubControl_GPUSettings(AEditingAction, ANewValue, AItemIndex);
        //  TriggerOnControlsModified(ANewValue <> OldValue);
        //end;

      CFindSubControl_ImageEffectSettings_PropIndex:
        ;
    end; //case
  end;
end;


procedure TfrClickerActions.HandleOnOIAfterSpinTextEditorChanging(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewValue: string);
begin
  try
    case ACategoryIndex of
      CCategory_Common:
        ;

      CCategory_ActionSpecific:
        OIAfterSpinTextEditorChanging_ActionSpecific(FEditingAction, CurrentlyEditingActionType, ACategoryIndex, APropertyIndex, AItemIndex, ANewValue);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          OIAfterSpinTextEditorChanging_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, ACategoryIndex, APropertyIndex, AItemIndex, ANewValue);
    end;
  except
  end;
end;


procedure TfrClickerActions.HandleOnOISelectedNode_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, Column: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PrimitiveFileNames: TStringList;
  PrimitiveFile_Modified: TStringList;
  IndexOfModifiedPmtv: Integer;
  PmtvFnm: string;
begin
  //load primitives frame
  if ALiveEditingActionType in [acFindSubControl] then
    if (NodeLevel = CPropertyItemLevel) and (PropertyIndex = CFindSubControl_MatchPrimitiveFiles_PropIndex) then
    begin
      PrimitiveFileNames := TStringList.Create;
      PrimitiveFile_Modified := TStringList.Create;
      try
        PrimitiveFileNames.LineBreak := #13#10;
        PrimitiveFile_Modified.LineBreak := #13#10;
        PrimitiveFileNames.Text := AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles;
        PrimitiveFile_Modified.Text := AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified;

        IndexOfModifiedPmtv := PrimitiveFile_Modified.IndexOf('1');
        if IndexOfModifiedPmtv > -1 then
        begin
          if IndexOfModifiedPmtv <> PropertyItemIndex then   //found a modified file, which is not this one
          begin
            if MessageBox(Handle, 'The current primitives file is modified. If you select another one, the changes will be lost. Continue?', PChar(Application.Title), MB_ICONWARNING + MB_YESNO) = IDNO then
            begin  //no, go back to the modified file
              FOIFrame.SelectNode(NodeLevel, CategoryIndex, PropertyIndex, FPrevSelectedPrimitiveNode);
              Exit;
            end
            else
            begin //yes, select the new file and reset the flag on the old one
              PrimitiveFile_Modified.Strings[FPrevSelectedPrimitiveNode] := '0'; //reset modified flag
              AEditingAction^.FindSubControlOptions.MatchPrimitiveFiles_Modified := PrimitiveFile_Modified.Text;
              FOIFrame.RepaintNodeByLevel(NodeLevel, CategoryIndex, PropertyIndex, FPrevSelectedPrimitiveNode);
            end;
          end
          else
            Exit; //the same (maodified) file is selected, nothing to do here
        end;

        FPrevSelectedPrimitiveNode := PropertyItemIndex;

        frClickerFindControl.CreateClickerPrimitivesFrame;
        frClickerFindControl.frClickerPrimitives.FileIndex := PropertyItemIndex;
        frClickerFindControl.frClickerPrimitives.OnEvaluateReplacementsFunc := HandleOnEvaluateReplacementsFunc;
        frClickerFindControl.frClickerPrimitives.OnLoadBitmap := HandleOnLoadBitmap;
        frClickerFindControl.frClickerPrimitives.OnLoadRenderedBitmap := HandleOnLoadRenderedBitmap;
        frClickerFindControl.frClickerPrimitives.OnGetListOfExternallyRenderedImages := HandleOnGetListOfExternallyRenderedImages;
        frClickerFindControl.frClickerPrimitives.OnLoadPrimitivesFile := HandleOnLoadPrimitivesFile;
        frClickerFindControl.frClickerPrimitives.OnSavePrimitivesFile := HandleOnSavePrimitivesFile;
        frClickerFindControl.frClickerPrimitives.OnTriggerOnControlsModified := HandleOnPrimitivesTriggerOnControlsModified;
        frClickerFindControl.frClickerPrimitives.OnSaveFromMenu := HandleOnSaveFromMenu;
        frClickerFindControl.frClickerPrimitives.OnPictureOpenDialogExecute := HandleOnPictureOpenDialogExecute;
        frClickerFindControl.frClickerPrimitives.OnGetPictureOpenDialogFileName := HandleOnGetPictureOpenDialogFileName;

        PmtvFnm := PrimitiveFileNames.Strings[PropertyItemIndex];
        PmtvFnm := StringReplace(PmtvFnm, '$TemplateDir$', FFullTemplatesDir, [rfReplaceAll]);
        PmtvFnm := StringReplace(PmtvFnm, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);
        PmtvFnm := StringReplace(PmtvFnm, '$SelfTemplateDir$', ExtractFileDir(DoOnGetLoadedTemplateFileName), [rfReplaceAll]);
        PmtvFnm := EvaluateReplacements(PmtvFnm);

        DoOnAddToLog('Loading primitives file: "' + ExpandFileName(PmtvFnm) + '".');

        frClickerFindControl.frClickerPrimitives.LoadFile(PmtvFnm);
      finally
        PrimitiveFileNames.Free;
        PrimitiveFile_Modified.Free;
      end;
    end;
end;


procedure TfrClickerActions.HandleOnOISelectedNode(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, Column: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  try
    case CategoryIndex of
      CCategory_Common:
        ;

      CCategory_ActionSpecific:
        HandleOnOISelectedNode_ActionSpecific(FEditingAction, CurrentlyEditingActionType, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, Column, Button, Shift, X, Y);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          HandleOnOISelectedNode_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, Column, Button, Shift, X, Y);
    end;
  except
  end;
end;


procedure TfrClickerActions.OIFirstVisibleNode_ActionSpecific(AEditingAction: PClkActionRec; NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer);
begin
  if AEditingAction = nil then
    Exit;

  AEditingAction^.ScrollIndex := COIScrollInfo_NodeLevel + '=' + IntToStr(NodeLevel) + #13#10 +
                                 COIScrollInfo_CategoryIndex + '=' + IntToStr(CategoryIndex) + #13#10 +
                                 COIScrollInfo_PropertyIndex + '=' + IntToStr(PropertyIndex) + #13#10 +
                                 COIScrollInfo_PropertyItemIndex + '=' + IntToStr(PropertyItemIndex);

  DoOnUpdateActionScrollIndex(AEditingAction^.ScrollIndex);
end;


procedure TfrClickerActions.HandleOnOIFirstVisibleNode(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer);
begin
  try
    case CategoryIndex of
      CCategory_Common:
        ;

      CCategory_ActionSpecific:
        OIFirstVisibleNode_ActionSpecific(FEditingAction, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex);

      CCategory_EditedAction:
        if FEditTemplateOptions_EditingAction <> nil then
          OIFirstVisibleNode_ActionSpecific(FEditTemplateOptions_EditingAction, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex);
    end;
  except
  end;
end;


function OIGetPropertyName_ForChecking_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; APropertyIndex: Integer): string;
var
  EditingActionType: Integer;
  ListOfProperties: TStringList;
begin
  if AEditingAction = nil then
  begin
    Result := CActionIsNil;
    Exit;
  end;

  EditingActionType := Integer(ALiveEditingActionType);
  if EditingActionType = CClkUnsetAction then
    Result := '?'
  else
  begin
    if (ALiveEditingActionType = acPlugin) and (APropertyIndex > CPlugin_FileName_PropIndex) then  //intially used AEditingAction^.ActionOptions.Action
    begin
      ListOfProperties := TStringList.Create;
      try
        ListOfProperties.LineBreak := #13#10;
        ListOfProperties.Text := AEditingAction^.PluginOptions.ListOfPropertiesAndTypes;
        try
          if (APropertyIndex - CPropCount_Plugin < ListOfProperties.Count) and (ListOfProperties.Count > 0) then
            Result := ListOfProperties.Names[APropertyIndex - CPropCount_Plugin]
          else
            Result := '[Err: Index out of bounds: ' + IntToStr(APropertyIndex - 1) + ']';
        except
          Result := 'bug on getting name';
        end;
      finally
        ListOfProperties.Free;
      end;
    end
    else
    begin
      try
        if APropertyIndex < CMainPropCounts[EditingActionType] then
          Result := CMainProperties[EditingActionType]^[APropertyIndex].Name
        else
          Result := 'bug on getting name.';
      except
        on E: Exception do
        begin
          Result := 'bug on getting name.';
          //MessageBox(0, PChar('AV' + #13#10 + Result + #13#10 + E.Message), 'UC OIGetPropertyName', 0);
        end;
      end;
    end;
  end;
end;


function OIGetListPropertyItemName_ForChecking_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
var
  EditingActionType: Integer;
  ItemIndexMod, ItemIndexDiv: Integer;
begin
  Result := '';

  if AEditingAction = nil then
  begin
    Result := CActionIsNil;
    Exit;
  end;

  EditingActionType := Integer(ALiveEditingActionType);
  if EditingActionType = CClkUnsetAction then
    Exit;

  case EditingActionType of
    Ord(acFindControl):
    begin
      case APropertyIndex of
        CFindControl_MatchCriteria_PropIndex:
          Result := CFindControl_MatchCriteriaProperties[AItemIndex].Name;

        CFindControl_InitialRectangle_PropIndex:
          Result := CFindControl_InitialRectangleProperties[AItemIndex].Name;
      end;
    end;

    Ord(acFindSubControl):
    begin
      case APropertyIndex of
        CFindSubControl_MatchCriteria_PropIndex:
          Result := CFindSubControl_MatchCriteriaProperties[AItemIndex].Name;

        CFindSubControl_MatchBitmapText_PropIndex:
        begin
          ItemIndexMod := AItemIndex mod CPropCount_FindSubControlMatchBitmapText;
          ItemIndexDiv := AItemIndex div CPropCount_FindSubControlMatchBitmapText;
          Result := '[' + IntToStr(ItemIndexDiv) + '].' + CFindSubControl_MatchBitmapTextProperties[ItemIndexMod].Name;
        end;

        CFindSubControl_MatchBitmapFiles_PropIndex:
          Result := 'File[' + IntToStr(AItemIndex) + ']';

        CFindSubControl_MatchBitmapAlgorithmSettings_PropIndex:
          Result := CFindSubControl_MatchBitmapAlgorithmSettingsProperties[AItemIndex].Name;

        CFindSubControl_InitialRectangle_PropIndex:
          Result := CFindSubControl_InitialRectangleProperties[AItemIndex].Name;

        CFindSubControl_MatchPrimitiveFiles_PropIndex:
        begin
          Result := 'File[' + IntToStr(AItemIndex) + ']';
        end;  //CFindSubControl_MatchPrimitiveFiles_PropIndex

        CFindSubControl_MatchByHistogramSettings_PropIndex:
          Result := CFindSubControl_MatchByHistogramSettingsProperties[AItemIndex].Name;

        CFindSubControl_RenderingInBrowserSettings_PropIndex:
          Result := CFindSubControl_RenderingInBrowserSettingsProperties[AItemIndex].Name;

        CFindSubControl_GPUSettings_PropIndex:
          Result := CFindSubControl_GPUSettingsProperties[AItemIndex].Name;

        CFindSubControl_ImageEffectSettings_PropIndex:
          Result := CFindSubControl_ImageEffectSettingsProperties[AItemIndex].Name;
        else
          Result := '';
      end;
    end;

    Ord(acCallTemplate):
    begin
      case APropertyIndex of
        CCallTemplate_CallTemplateLoop_PropIndex:
          Result := CCallTemplate_CallTemplateLoopProperties[AItemIndex].Name;

        else
          Result := '';
      end;
    end;

  end;   //case EditingActionType
end;


function TfrClickerActions.GetPropertyName_ForChecking_ActionSpecific(AEditingAction: PClkActionRec; ALiveEditingActionType: TClkAction; ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
var
  PropertyNameWithoutDot: Boolean; //those with name and '['
begin
  Result := OIGetPropertyName_ForChecking_ActionSpecific(AEditingAction, ALiveEditingActionType, APropertyIndex);
  if AItemIndex <> -1 then
  begin
    PropertyNameWithoutDot := (ALiveEditingActionType = acFindSubControl) and (APropertyIndex = CFindSubControl_MatchBitmapText_PropIndex);
    if not PropertyNameWithoutDot then
      Result := Result + '.';

    Result := Result + OIGetListPropertyItemName_ForChecking_ActionSpecific(AEditingAction, ALiveEditingActionType, ACategoryIndex, APropertyIndex, AItemIndex);
  end;
end;


procedure TfrClickerActions.HandleOnOIInitNode(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer; var ACheckType: TCheckType; var ACheckState: TCheckState; var ANodeHeight: Word);
var
  DatatypeName: string;
  PropertyName: string;
  ListOfProperties: TStringList;
begin
  try
    if FEditTemplateOptions_EditingAction <> nil then
      if CategoryIndex = CCategory_EditedAction then
        if NodeLevel in [CPropertyLevel, CPropertyItemLevel] then
        begin
          ACheckType := ctCheckBox;
          ACheckState := csUncheckedNormal;

          PropertyName := GetPropertyName_ForChecking_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, CategoryIndex, PropertyIndex, PropertyItemIndex);

          ListOfProperties := TStringList.Create;
          try                             // a bit of overhead to decode the same list for all properties
            ListOfProperties.LineBreak := #13#10;
            ListOfProperties.Text := FastReplace_45ToReturn(FEditTemplateOptions_EditingAction.EditTemplateOptions.ListOfEnabledProperties);

            if ListOfProperties.IndexOf(PropertyName) <> -1 then
              ACheckState := csCheckedNormal;
          finally
            ListOfProperties.Free;
          end;

          DatatypeName := OIGetDataTypeName_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, CategoryIndex, PropertyIndex, PropertyItemIndex);
          if (DatatypeName = 'Structure') or (DatatypeName = 'Array') then
            ACheckState := csMixedPressed;
        end;
  except
    //not sure what, but something crashes here
  end;
end;


procedure TfrClickerActions.HandleOnOIChecked(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer; ACheckState: TCheckState);
var
  PropertyName: string;
  ListOfProperties: TStringList;
  Idx: Integer;
begin
  if CategoryIndex <> CCategory_EditedAction then
    Exit;

  if FEditTemplateOptions_EditingAction = nil then
    Exit;

  PropertyName := GetPropertyName_ForChecking_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, CategoryIndex, PropertyIndex, PropertyItemIndex);

  ListOfProperties := TStringList.Create;
  try
    ListOfProperties.LineBreak := #13#10;
    ListOfProperties.Text := FastReplace_45ToReturn(FEditTemplateOptions_EditingAction^.EditTemplateOptions.ListOfEnabledProperties);

    Idx := ListOfProperties.IndexOf(PropertyName);
    if Idx = -1 then //not in the list
    begin
      if ACheckState = csCheckedNormal then
        ListOfProperties.Add(PropertyName)
      else
        ; //nothing to do
    end
    else
    begin //in the list
      if ACheckState = csUncheckedNormal then
        ListOfProperties.Delete(Idx)
      else
        ; //nothing to do
    end;

    FEditTemplateOptions_EditingAction^.EditTemplateOptions.ListOfEnabledProperties := FastReplace_ReturnTo45(ListOfProperties.Text);

    //DoOnAddToLog('List: ' + FEditTemplateOptions_EditingAction.EditTemplateOptions.ListOfEnabledProperties);
    TriggerOnControlsModified;
  finally
    ListOfProperties.Free;
  end;
end;


procedure TfrClickerActions.HandleOnOIChecking(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer; ACheckState: TCheckState; var ANewState: TCheckState; var AAllowed: Boolean);
var
  DatatypeName: string;
begin
  AAllowed := (CategoryIndex = CCategory_EditedAction) and (NodeLevel in [CPropertyLevel, CPropertyItemLevel]);

  if FEditTemplateOptions_EditingAction <> nil then
  begin
    DatatypeName := OIGetDataTypeName_ActionSpecific(FEditTemplateOptions_EditingAction, FEditTemplateOptions_EditingAction.ActionOptions.Action, CategoryIndex, PropertyIndex, PropertyItemIndex);
    if (DatatypeName = 'Structure') or (DatatypeName = 'Array') then
      AAllowed := False;

    if (FEditTemplateOptions_EditingAction^.ActionOptions.Action = acEditTemplate) and (CategoryIndex = CCategory_EditedAction) then
      if PropertyIndex in [CEditTemplate_ListOfEditedProperties_PropIndex, CEditTemplate_ListOfEnabledProperties_PropIndex] then
        AAllowed := False;
  end;
end;

end.

