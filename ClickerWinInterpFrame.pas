{
    Copyright (C) 2025 VCC
    creation date: Jul 2023 - most content moved from ClickerWinInterpForm.pas
    initial release date: 09 Jul 2023

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


unit ClickerWinInterpFrame;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF Windows}
    Windows,
  {$ELSE}
    LCLIntf, LCLType,
  {$ENDIF}
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls, ColorBox,
  Buttons, Menus, VirtualTrees, Types, Graphics, ClickerUtils, IniFiles;

type
  TColoredHandle = record
    Handle: THandle;
    Color: TColor;
  end;

  PHighlightedCompRec = ^THighlightedCompRec;
  THighlightedCompRec = record
    CompRec: TCompRec;
    LocalX, LocalY: Integer;  //relative to the screenshot image
    LocalX_FromParent, LocalY_FromParent: Integer;  //relative to the parent component
    AssignedColor: TColor;
    ManuallyAdded: Boolean;  //Set to True when added by user. It is usually set for subcomponents.

    Ctrl: TWinControl; //used by drawing board, to sync components  (this is the mount panel)
    ParentCtrl: TWinControl; //used by drawing board, to sync components  (this can be the drawing board or another mount panel)
    CGClassName: string; //code generator class name (e.g. TForm1 or TSpeedButton)
    CompName: string; //used when identifying the component for avoided zones, code generator or something else. These names might contain the component handles as they were recorded, although users should change these names to something meaningful (e.g. frmMyForm, btnOpen, btnClose).
  end;


  PAvoidedZoneRec = ^TAvoidedZoneRec;
  TAvoidedZoneRec = record
    ZRectStr: TRectString;
    ZName: string;
  end;


  TLastFindControlGeneratedAction = (lfcNone, lfcClick, lfcCachePos);

  TColoredHandleArr = array of TColoredHandle;

  //TRectArr = array of TRect;
  TExtendDir = (edLeft, edTop, edRight, edBottom);
  TExtendDirs = set of TExtendDir;

  THandleArr = array of THandle;
  TTRectArr = array of TRect;


  TOnInsertTreeComponent = procedure(ACompData: PHighlightedCompRec) of object;
  TOnClearWinInterp = procedure of object;

  { TfrClickerWinInterp }

  TfrClickerWinInterp = class(TFrame)
    btnExport: TButton;
    btnLoadTree: TButton;
    btnSaveTree: TButton;
    btnStartRec: TButton;
    btnStopRec: TButton;
    btNewZone: TButton;
    btnDeleteZone: TButton;
    btnLoadZones: TButton;
    btnSaveZones: TButton;
    btnClearZones: TButton;
    chkShowZoom: TCheckBox;
    chkContinuouslyScreenshotByKeys: TCheckBox;
    chkBringTargetToFrontPeriodically: TCheckBox;
    chkHighlightSelectedComponent: TCheckBox;
    chkRecordSelectedAreaOnly: TCheckBox;
    chkRecordWithEdgeExtending: TCheckBox;
    chkFullScr: TCheckBox;
    chkMinimizeWhileRecording: TCheckBox;
    chkUseHCursor: TCheckBox;
    chkBringTargetToFront: TCheckBox;
    colboxHighlightingLabels: TColorBox;
    imgAvgScreenshotAndAssignedComp: TImage;
    imgAvgScreenshotAndGreenComp: TImage;
    imgHandleColors: TImage;
    imgLiveScreenshot: TImage;
    imglstSpinner: TImageList;
    imgScannedWindow: TImage;
    imgScannedWindowWithText: TImage;
    imgScannedWindowWithAvoidedZones: TImage;
    imgScreenshot: TImage;
    imgSpinner: TImage;
    imgEnabledPause: TImage;
    imgDisabledPause: TImage;
    imgSpinnerDiff: TImage;
    lbeMouseCursorPosToScreenshotDelay: TLabeledEdit;
    lblAvoidedZones: TLabel;
    lbeStep: TLabeledEdit;
    lblGauge: TLabel;
    lblHighlightingLabels: TLabel;
    memCompInfo: TMemo;
    MenuItem_UpdateTreeValuesFromSelectionToANewComponent: TMenuItem;
    MenuItem_ClearZones: TMenuItem;
    MenuItem_CopyLiveScreenshotToMainScreenshot: TMenuItem;
    MenuItem_HideLiveScreenshot: TMenuItem;
    MenuItem_ShowLiveScreenshot: TMenuItem;
    pmAvoidedZones: TPopupMenu;
    pmVarsEditor: TPopupMenu;
    Separator7: TMenuItem;
    MenuItem_ClearScreenshots: TMenuItem;
    Separator6: TMenuItem;
    MenuItem_DeleteSubComponent: TMenuItem;
    MenuItem_AddSubcomponent: TMenuItem;
    PageControlWinInterp: TPageControl;
    pnlvstComponents: TPanel;
    pnlvstSettings: TPanel;
    prbRecordingWithMouseSwipe: TProgressBar;
    Separator5: TMenuItem;
    Separator4: TMenuItem;
    MenuItem_UpdateTreeValuesFromSelection: TMenuItem;
    Separator3: TMenuItem;
    MenuItem_ConfigureMultiSizeRecording: TMenuItem;
    MenuItem_RecordMultipleSizes: TMenuItem;
    pnlDrag: TPanel;
    pnlFrameBK: TPanel;
    pmComponents: TPopupMenu;
    pmExtraRecording: TPopupMenu;
    pnlHorizSplitter: TPanel;
    pnlMouseCoordsOnScreenshot: TPanel;
    pnlWinInterpSettings: TPanel;
    prbRecording: TProgressBar;
    rdgrpLayers: TRadioGroup;
    scrboxScannedComponents: TScrollBox;
    Separator2: TMenuItem;
    MenuItemCopyFindControlActionsToClipBoard: TMenuItem;
    MenuItemCopyFindControlAndCachePositionActionsToClipBoard: TMenuItem;
    MenuItemCopyFindControlAndClickActionsToClipBoard: TMenuItem;
    MenuItemRecordWithMouseSwipe: TMenuItem;
    MenuItem_CopySelectedComponentToClipboard: TMenuItem;
    MenuItem_CopySelectionToClipboard: TMenuItem;
    MenuItem_SaveSelectedComponentToFile: TMenuItem;
    MenuItem_SaveSelectionToFile: TMenuItem;
    pmScreenshot: TPopupMenu;
    spdbtnExtraRecording: TSpeedButton;
    spdbtnMoveDown: TSpeedButton;
    spdbtnMoveUp: TSpeedButton;
    TabSheet_Components: TTabSheet;
    TabSheet_Settings: TTabSheet;
    tmrDrawZoom: TTimer;
    tmrScanByKeys: TTimer;
    tmrEditSettings: TTimer;
    tmrEditComponents: TTimer;
    tmrScan: TTimer;
    tmrSpinner: TTimer;
    procedure btnClearZonesClick(Sender: TObject);
    procedure btnDeleteZoneClick(Sender: TObject);
    procedure btNewZoneClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure btnLoadTreeClick(Sender: TObject);
    procedure btnLoadZonesClick(Sender: TObject);
    procedure btnSaveTreeClick(Sender: TObject);
    procedure btnSaveZonesClick(Sender: TObject);
    procedure btnStartRecClick(Sender: TObject);
    procedure btnStopRecClick(Sender: TObject);
    procedure chkContinuouslyScreenshotByKeysChange(Sender: TObject);
    procedure chkHighlightSelectedComponentChange(Sender: TObject);
    procedure chkRecordSelectedAreaOnlyChange(Sender: TObject);
    procedure chkRecordWithEdgeExtendingChange(Sender: TObject);
    procedure colboxHighlightingLabelsSelect(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure imgLiveScreenshotMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgLiveScreenshotMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure imgLiveScreenshotMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgScannedWindowMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgScannedWindowMouseEnter(Sender: TObject);
    procedure imgScannedWindowMouseLeave(Sender: TObject);
    procedure imgScannedWindowMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imgScannedWindowWithAvoidedZonesMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgScannedWindowWithAvoidedZonesMouseEnter(Sender: TObject);
    procedure imgScannedWindowWithAvoidedZonesMouseLeave(Sender: TObject);
    procedure imgScannedWindowWithAvoidedZonesMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItemCopyFindControlActionsToClipBoardClick(Sender: TObject);
    procedure MenuItemCopyFindControlAndCachePositionActionsToClipBoardClick(
      Sender: TObject);
    procedure MenuItemCopyFindControlAndClickActionsToClipBoardClick(
      Sender: TObject);
    procedure MenuItemRecordWithMouseSwipeClick(Sender: TObject);
    procedure MenuItem_AddSubcomponentClick(Sender: TObject);
    procedure MenuItem_ClearScreenshotsClick(Sender: TObject);
    procedure MenuItem_ClearZonesClick(Sender: TObject);
    procedure MenuItem_CopyLiveScreenshotToMainScreenshotClick(Sender: TObject);
    procedure MenuItem_CopySelectedComponentToClipboardClick(Sender: TObject);
    procedure MenuItem_CopySelectionToClipboardClick(Sender: TObject);
    procedure MenuItem_DeleteSubComponentClick(Sender: TObject);
    procedure MenuItem_RecordMultipleSizesClick(Sender: TObject);
    procedure MenuItem_SaveSelectedComponentToFileClick(Sender: TObject);
    procedure MenuItem_SaveSelectionToFileClick(Sender: TObject);
    procedure MenuItem_UpdateTreeValuesFromSelectionClick(Sender: TObject);
    procedure MenuItem_UpdateTreeValuesFromSelectionToANewComponentClick(
      Sender: TObject);
    procedure MenuItem_vstAvoidedZonesEditorMenuClick(Sender: TObject);
    procedure PageControlWinInterpChange(Sender: TObject);
    procedure pnlDragMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlDragMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlDragMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlHorizSplitterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlHorizSplitterMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlHorizSplitterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdgrpLayersClick(Sender: TObject);
    procedure scrboxScannedComponentsMouseWheel(Sender: TObject;
      Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure spdbtnExtraRecordingClick(Sender: TObject);
    procedure spdbtnMoveDownClick(Sender: TObject);
    procedure spdbtnMoveUpClick(Sender: TObject);
    procedure tmrDrawZoomTimer(Sender: TObject);
    procedure tmrEditComponentsTimer(Sender: TObject);
    procedure tmrEditSettingsTimer(Sender: TObject);
    procedure tmrScanByKeysTimer(Sender: TObject);
    procedure tmrScanTimer(Sender: TObject);
    procedure tmrSpinnerTimer(Sender: TObject);
    procedure vstComponentsClick(Sender: TObject);
    procedure vstComponentsDblClick(Sender: TObject);
    procedure vstComponentsEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vstComponentsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstComponentsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
    procedure vstComponentsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vstComponentsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure vstComponentsLoadNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Stream: TStream);
    procedure vstComponentsLoadTree(Sender: TBaseVirtualTree; Stream: TStream);
    procedure vstComponentsSaveNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Stream: TStream);
    procedure vstComponentsSaveTree(Sender: TBaseVirtualTree; Stream: TStream);
    procedure vstComponentsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure vstAvoidedZonesDblClick(Sender: TObject);
    procedure vstAvoidedZonesEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vstAvoidedZonesEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstAvoidedZonesNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
    procedure vstAvoidedZonesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vstAvoidedZonesGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure vstAvoidedZonesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure vstAvoidedZonesCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure vstAvoidedZonesPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
  private
    vstComponents: TVirtualStringTree;
    vstAvoidedZones: TVirtualStringTree;

    FDragging: Boolean;

    FInterprettedHandle: THandle;     //used by the dragging panel
    FDoneRec: Boolean;
    clrs: array [0..300] of TColor;
    clrs2: array [0..300] of TColor;
    FColoredHandles: TColoredHandleArr;
    FOldSelectedNode: PVirtualNode;
    FCompTreeVersion: LongInt;  //used on loading, because there are two handlers (one which gets the value, and the other, which uses the value)

    FSelectedComponentLeftLimitLabel: TLabel;
    FSelectedComponentTopLimitLabel: TLabel;
    FSelectedComponentRightLimitLabel: TLabel;
    FSelectedComponentBottomLimitLabel: TLabel;

    FTransparent_SelectedComponentLeftLimitLabel: TLabel;
    FTransparent_SelectedComponentTopLimitLabel: TLabel;
    FTransparent_SelectedComponentRightLimitLabel: TLabel;
    FTransparent_SelectedComponentBottomLimitLabel: TLabel;

    FSelectedZoneLeftLimitLabel: TLabel;
    FSelectedZoneTopLimitLabel: TLabel;
    FSelectedZoneRightLimitLabel: TLabel;
    FSelectedZoneBottomLimitLabel: TLabel;

    FTransparent_SelectedZoneLeftLimitLabel: TLabel;
    FTransparent_SelectedZoneTopLimitLabel: TLabel;
    FTransparent_SelectedZoneRightLimitLabel: TLabel;
    FTransparent_SelectedZoneBottomLimitLabel: TLabel;

    FProgressVertLabel: TLabel;
    FProgressHorizLabel: TLabel;

    FSelectionHold: Boolean;
    FMouseDownGlobalPos: TPoint;
    FMouseDownSelPos: TPoint;
    FMouseUpHitInfo_Components: THitInfo;
    FMouseUpHitInfo_Settings: THitInfo;

    FSelectedComponentText: string;
    FSelectedComponentClassName: string;
    FEditingText: string;
    FUpdatedVstText: Boolean;

    FHold: Boolean;
    FSplitterMouseDownGlobalPos: TPoint;
    FSplitterMouseDownImagePos: TPoint;
    FRecordSelectedAreaOnly: Boolean;
    FRecordWithEdgeExtending: Boolean;
    FDraggingForSelection: Boolean;

    FListOfScannedComponents: TStringList;
    FTextEditorEditBox: TEdit;  //pointer to the built-in editor
    FCurrentMousePosOnPreviewImg: TPoint;

    FOnGetConnectionAddress: TOnGetConnectionAddress;
    FOnInsertTreeComponent: TOnInsertTreeComponent;
    FOnClearWinInterp: TOnClearWinInterp;

    FOnOpenDialogExecute: TOnOpenDialogExecute;
    FOnGetOpenDialogFileName: TOnGetOpenDialogFileName;
    FOnSaveDialogExecute: TOnOpenDialogExecute;
    FOnGetSaveDialogFileName: TOnGetOpenDialogFileName;

    FOnLoadFileFromStream: TOnLoadFileFromStream;
    FOnSaveFileToStream: TOnLoadFileFromStream;
    FOnFileExists: TOnFileExists;

    procedure FTransparent_LeftMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FTransparent_LeftMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure FTransparent_LeftMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure FTransparent_RightMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FTransparent_RightMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure FTransparent_RightMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure FTransparent_TopMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FTransparent_TopMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure FTransparent_TopMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure FTransparent_BottomMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FTransparent_BottomMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure FTransparent_BottomMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure FSelectionsLinesMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    ///

    procedure FTransparentZone_LeftMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FTransparentZone_LeftMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure FTransparentZone_LeftMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure FTransparentZone_RightMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FTransparentZone_RightMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure FTransparentZone_RightMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure FTransparentZone_TopMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FTransparentZone_TopMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure FTransparentZone_TopMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure FTransparentZone_BottomMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FTransparentZone_BottomMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure FTransparentZone_BottomMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    ///

    function GetHighlightingLinesColor: TColor;
    procedure SetHighlightingLinesColor(Value: TColor);

    function GetSelectedLayer: TColor;
    procedure SetSelectedLayer(Value: TColor);

    function GetHighlightSelectedComponent: Boolean;
    procedure SetHighlightSelectedComponent(Value: Boolean);

    function DoOnGetConnectionAddress: string;
    procedure DoOnInsertTreeComponent(ACompData: PHighlightedCompRec);
    procedure DoOnClearWinInterp;

    function DoOnOpenDialogExecute(AFilter: string): Boolean;
    function DoOnGetOpenDialogFileName: string;
    function DoOnSaveDialogExecute(AFilter: string): Boolean;
    function DoOnGetSaveDialogFileName: string;

    procedure DoOnLoadFileFromStream(AFileName: string; AStream: TMemoryStream);
    procedure DoOnSaveFileToStream(AFileName: string; AStream: TMemoryStream);
    function DoOnFileExists(const AFileName: string): Boolean;

    procedure EvaluateScanedComponentsOnChange(Sender: TObject);
    function EvaluateScanedComponents(s: string): string;
    procedure ZoneRectStrToRect(var ARectStr: TRectString; var ADestRect: TRect);
    procedure AddCompInfoToListOfScannedComponents(ANodeData: PHighlightedCompRec);
    procedure UpdateCompInfoToListOfScannedComponents(ANodeData: PHighlightedCompRec);
    procedure UpdateListOfScanedValues;
    procedure FillInAvoidedZonesEditorMenu;

    procedure CreateRemainingComponents;
    procedure AdjustHighlightingLabelsToScreenshot;
    procedure HighlightComponent(Node: PVirtualNode);
    procedure HighlightZone(Node: PVirtualNode);
    procedure CopyFindControlActionsToClipBoard(AIncludeAction: TLastFindControlGeneratedAction);

    procedure ResizeFrameSectionsBySplitter(NewLeft: Integer);

    procedure GetWindowInfo;
    procedure BuildColors;
    procedure DrawAvoidedZones;
    procedure GenerateCompImagesfromTreeContent;
    function GetParentNodeByRectangle(AComp: THighlightedCompRec): PVirtualNode;
    procedure InsertTreeComponent(AParentNode: PVirtualNode; AComp: THighlightedCompRec);
    procedure AddTreeComponent(AComp: THighlightedCompRec);
    procedure SelectTreeNodeByHandle(HW: THandle);
    procedure SelectTreeNodeByImgPoint(X, Y: Integer);
    procedure UpdateTreeValuesFromSelection;

    function GetIndexOfColoredHandleByHandle(HW: THandle): Integer;
    function GetIndexOfColoredHandleByColor(AColor: TColor): Integer;
    procedure AddColoredHandle(HW: THandle; AColor: TColor);
    function AddComponentToList(HW: THandle; ALocalX, ALocalY: Integer): TColor;
    function AddSubComponentToList(HW: THandle; ARect: TCompRec; ALocalX, ALocalY: Integer): TColor;
    procedure PrepareLayers(ARect: TRect);
    procedure UpdateLayersVisibility;
    procedure SetHighlightingLabelsColor;
    procedure CropImageFromScreenshot(ComponentNode: PVirtualNode; CroppedBMP: TBitmap; ComponentOnly: Boolean = True);

    procedure GenerateContent_AvgScreenshotAndGreenComp(Node: PVirtualNode);
    procedure GenerateContent_AvgScreenshotAndGenComp;

    function PointIsInAvoidedZone(X, Y: Integer): Boolean; overload;
    function PointIsInAvoidedZone(X, Y: Integer; var AZones: TTRectArr; var ALastFoundIndex: Integer): Boolean; overload;
    procedure BuildZonesArray(var AZones: TTRectArr);
    function GetMouseSwipePixelCount(var AZones: TTRectArr; var AScanningArea: TRect): Integer;

    procedure RectsToTree(var ADiffRects: TCompRecArr; var ImgMatrix: TColorArr; var ImgHWMatrix: THandleArr);
    procedure LoadScreenshotAsPng(AFileName: string; ADestImage: TImage);
    procedure SaveScreenshotAsPng(AFileName: string; ASrcImage: TImage);
    procedure LoadImages(ABasePath: string);
    procedure ScanTargetControl;

    procedure LoadTree(AFnm: string);
    procedure LoadZones(AFnm: string);
    procedure SaveZones(AFnm: string);
    procedure UpdateZoneByNode(ANode: PVirtualNode; AZoneName, ALeftOffset, ATopOffset, ARightOffset, ABottomOffset, ALeft, ATop, ARight, ABottom: string);
    procedure UpdateZone(AZoneName, ALeftOffset, ATopOffset, ARightOffset, ABottomOffset, ALeft, ATop, ARight, ABottom: string);
    procedure NewZone(AZoneName, ALeftOffset, ATopOffset, ARightOffset, ABottomOffset, ALeft, ATop, ARight, ABottom: string);
    procedure DeleteZone(ANode: PVirtualNode);
    function GetZoneNodeByName(AName: string): PVirtualNode;
    procedure DeleteZoneByName(AName: string);
    function GetZoneByName(AName: string): string;

    property HighlightingLinesColor: TColor read GetHighlightingLinesColor write SetHighlightingLinesColor;
    property SelectedLayer: Integer read GetSelectedLayer write SetSelectedLayer;
    property HighlightSelectedComponent: Boolean read GetHighlightSelectedComponent write SetHighlightSelectedComponent;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadSettings(AIni: TMemIniFile);
    procedure SaveSettings(AIni: TMemIniFile);

    procedure GetTreeContent(AStream: TMemoryStream);
    function SetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue: string): Boolean;
    procedure RecordComponent(AInterprettedHandle: THandle; var ImgMatrix: TColorArr; var ImgHWMatrix: THandleArr; AStep: Integer = 1);
    procedure RecordWithMouseSwipe(AInterprettedHandle: THandle; AStep: Integer = 1);

    procedure GetCurrentlyRecordedScreenShotImage(ABmp: TBitmap);
    procedure SaveImages(ABasePath: string);

    property SelectedComponentText: string read FSelectedComponentText;
    property SelectedComponentClassName: string read FSelectedComponentClassName;

    property OnGetConnectionAddress: TOnGetConnectionAddress read FOnGetConnectionAddress write FOnGetConnectionAddress;
    property OnInsertTreeComponent: TOnInsertTreeComponent read FOnInsertTreeComponent write FOnInsertTreeComponent;
    property OnClearWinInterp: TOnClearWinInterp read FOnClearWinInterp write FOnClearWinInterp;

    property OnOpenDialogExecute: TOnOpenDialogExecute read FOnOpenDialogExecute write FOnOpenDialogExecute;
    property OnGetOpenDialogFileName: TOnGetOpenDialogFileName read FOnGetOpenDialogFileName write FOnGetOpenDialogFileName;
    property OnSaveDialogExecute: TOnOpenDialogExecute read FOnSaveDialogExecute write FOnSaveDialogExecute;
    property OnGetSaveDialogFileName: TOnGetOpenDialogFileName read FOnGetSaveDialogFileName write FOnGetSaveDialogFileName;

    property OnLoadFileFromStream: TOnLoadFileFromStream read FOnLoadFileFromStream write FOnLoadFileFromStream;
    property OnSaveFileToStream: TOnLoadFileFromStream read FOnSaveFileToStream write FOnSaveFileToStream;
    property OnFileExists: TOnFileExists read FOnFileExists write FOnFileExists;
  end;

implementation

{$R *.frm}


uses
  BitmapProcessing, ClickerTemplates, Clipbrd, ClickerActionsClient, ClickerZoomPreviewForm,
  imgList, Dialogs, BitmapConv, ClickerIniFiles, Math;


procedure TfrClickerWinInterp.LoadSettings(AIni: TMemIniFile);
begin
  HighlightingLinesColor := AIni.ReadInteger('WinInterpWindow', 'HighlightingLinesColor', CLabel_Orange);
  SelectedLayer := AIni.ReadInteger('WinInterpWindow', 'SelectedLayer', 1);
  HighlightSelectedComponent := AIni.ReadBool('WinInterpWindow', 'HighlightSelectedComponent', True);
end;


procedure TfrClickerWinInterp.SaveSettings(AIni: TMemIniFile);
begin
  AIni.WriteInteger('WinInterpWindow', 'HighlightingLinesColor', HighlightingLinesColor);
  AIni.WriteInteger('WinInterpWindow', 'SelectedLayer', SelectedLayer);
  AIni.WriteBool('WinInterpWindow', 'HighlightSelectedComponent', HighlightSelectedComponent);
end;


function TfrClickerWinInterp.GetHighlightingLinesColor: TColor;
begin
  Result := colboxHighlightingLabels.Selected;
end;


procedure TfrClickerWinInterp.SetHighlightingLinesColor(Value: TColor);
begin
  colboxHighlightingLabels.Selected := Value;
  SetHighlightingLabelsColor;
end;


function TfrClickerWinInterp.GetSelectedLayer: TColor;
begin
  Result := rdgrpLayers.ItemIndex;
end;


procedure TfrClickerWinInterp.SetSelectedLayer(Value: TColor);
begin
  rdgrpLayers.ItemIndex := Value;
end;


function TfrClickerWinInterp.GetHighlightSelectedComponent: Boolean;
begin
  Result := chkHighlightSelectedComponent.Checked;
end;


procedure TfrClickerWinInterp.SetHighlightSelectedComponent(Value: Boolean);
begin
  chkHighlightSelectedComponent.Checked := Value;
end;


function TfrClickerWinInterp.DoOnGetConnectionAddress: string;
begin
  if not Assigned(FOnGetConnectionAddress) then
  begin
    Result := '';
    Exit;
  end;

  Result := FOnGetConnectionAddress();
end;


procedure TfrClickerWinInterp.DoOnInsertTreeComponent(ACompData: PHighlightedCompRec);
begin
  if Assigned(FOnInsertTreeComponent) then
    FOnInsertTreeComponent(ACompData);
end;


procedure TfrClickerWinInterp.DoOnClearWinInterp;
begin
  if Assigned(FOnClearWinInterp) then
    FOnClearWinInterp;
end;


function TfrClickerWinInterp.DoOnOpenDialogExecute(AFilter: string): Boolean;
begin
  if not Assigned(FOnOpenDialogExecute) then
    raise Exception.Create('OnOpenDialogExecute is not assigned.')
  else
    Result := FOnOpenDialogExecute(AFilter);
end;


function TfrClickerWinInterp.DoOnGetOpenDialogFileName: string;
begin
  if not Assigned(FOnGetOpenDialogFileName) then
    raise Exception.Create('OnGetOpenDialogFileName is not assigned.')
  else
    Result := FOnGetOpenDialogFileName();
end;


function TfrClickerWinInterp.DoOnSaveDialogExecute(AFilter: string): Boolean;
begin
  if not Assigned(FOnSaveDialogExecute) then
    raise Exception.Create('OnSaveDialogExecute is not assigned.')
  else
    Result := FOnSaveDialogExecute(AFilter);
end;


function TfrClickerWinInterp.DoOnGetSaveDialogFileName: string;
begin
  if not Assigned(FOnGetSaveDialogFileName) then
    raise Exception.Create('OnGetSaveDialogFileName is not assigned.')
  else
    Result := FOnGetSaveDialogFileName();
end;


procedure TfrClickerWinInterp.DoOnLoadFileFromStream(AFileName: string; AStream: TMemoryStream);
begin
  if Assigned(FOnLoadFileFromStream) then
    FOnLoadFileFromStream(AFileName, AStream);
end;


procedure TfrClickerWinInterp.DoOnSaveFileToStream(AFileName: string; AStream: TMemoryStream);
begin
  if Assigned(FOnSaveFileToStream) then
    FOnSaveFileToStream(AFileName, AStream);
end;


function TfrClickerWinInterp.DoOnFileExists(const AFileName: string): Boolean;
begin
  if not Assigned(FOnFileExists) then
    raise Exception.Create('OnFileExists is not assigned.')
  else
    Result := FOnFileExists(AFileName);
end;


procedure TfrClickerWinInterp.BuildColors;
var
  r, g, b: Byte;
  step: array [0..110] of TColor;
  i: Integer;
begin {Build the colors}
  for i := 1 to 110 do
    step[i] := 0;

  for i := 0 to 51 do
  begin
    step[i shl 1] := 229 - i shl 2;
    step[i shl 1 + 1] := 27 + i shl 2;
  end;

  i := 0;

  r := 0; //0
  g := 0; //34
  b := 0; //67
  repeat
    inc(i);

    if (i >= 1) and (i <= 100) then
    begin
      r := i mod 100;
      g := (i + 35) mod 100;
      b := (i + 67) mod 100;
    end;

    if (i >= 101) and (i <= 200) then
    begin
      g := i mod 100;
      r := (i + 35) mod 100;
      b := (i + 67) mod 100;
    end;

    if (i >= 201) and (i <= 300) then
    begin
      b := i mod 100;
      g := (i + 35) mod 100;
      r := (i + 67) mod 100;
    end;

    clrs[i] := step[b] shl 16 + step[g] shl 8 + step[r];
  until i = 300;

  for i := 1 to 300 do
    clrs2[i] := clrs[(i * 9) mod 299];
end;


procedure TfrClickerWinInterp.vstComponentsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  NodeData: PHighlightedCompRec;
begin
  NodeData := vstComponents.GetNodeData(Node);
  if NodeData = nil then
  begin
    CellText := 'Data bug';
    Exit;
  end;

  case Column of
    0: CellText := IntToStr(NodeData^.CompRec.Handle);
    1: CellText := NodeData^.CompRec.ClassName;
    2: CellText := NodeData^.CompRec.Text;
    3: CellText := NodeData^.CGClassName; //code generator class name (e.g. TForm1 or TSpeedButton)
    4: CellText := '0x' + IntToHex(NodeData^.AssignedColor, 6);
    5: CellText := IntToStr(NodeData^.CompRec.ComponentRectangle.Left);
    6: CellText := IntToStr(NodeData^.CompRec.ComponentRectangle.Top);
    7: CellText := IntToStr(NodeData^.CompRec.ComponentRectangle.Right);
    8: CellText := IntToStr(NodeData^.CompRec.ComponentRectangle.Bottom);
    9: CellText := IntToStr(NodeData^.CompRec.ComponentRectangle.Width {Right - NodeData^.CompRec.ComponentRectangle.Left});
    10: CellText := IntToStr(NodeData^.CompRec.ComponentRectangle.Height);
    11: CellText := IntToStr(NodeData^.LocalX);
    12: CellText := IntToStr(NodeData^.LocalY);
    13: CellText := IntToStr(NodeData^.LocalX_FromParent);
    14: CellText := IntToStr(NodeData^.LocalY_FromParent);
    15: CellText := BoolToStr(NodeData^.ManuallyAdded, 'Yes', 'No');
    16: CellText := NodeData^.CompName;
  end;
end;


procedure TfrClickerWinInterp.vstAvoidedZonesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  NodeData: PAvoidedZoneRec;
begin
  NodeData := vstAvoidedZones.GetNodeData(Node);
  if NodeData = nil then
  begin
    CellText := 'Data bug';
    Exit;
  end;

  case Column of
    0: CellText := NodeData^.ZRectStr.LeftOffset;
    1: CellText := NodeData^.ZRectStr.TopOffset;
    2: CellText := NodeData^.ZRectStr.RightOffset;
    3: CellText := NodeData^.ZRectStr.BottomOffset;

    4: CellText := NodeData^.ZName;

    5: CellText := NodeData^.ZRectStr.Left;
    6: CellText := NodeData^.ZRectStr.Top;
    7: CellText := NodeData^.ZRectStr.Right;
    8: CellText := NodeData^.ZRectStr.Bottom;
  end;
end;


procedure TfrClickerWinInterp.vstComponentsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    MenuItem_DeleteSubComponent.Click;
end;


procedure TfrClickerWinInterp.vstAvoidedZonesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    btnDeleteZone.Click;
end;


procedure TfrClickerWinInterp.MenuItem_vstAvoidedZonesEditorMenuClick(Sender: TObject);
begin
  FTextEditorEditBox.Text := StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]);
  FEditingText := FTextEditorEditBox.Text;
  FUpdatedVstText := True;
end;


procedure TfrClickerWinInterp.FillInAvoidedZonesEditorMenu;
var
  TempNode: PVirtualNode;
  NodeData: PHighlightedCompRec;
  NewMenuItem, SubMenuItem: TMenuItem;
begin
  pmVarsEditor.Items.Clear;

  TempNode := vstComponents.GetFirst;
  if TempNode <> nil then
  begin
    repeat
      NodeData := vstComponents.GetNodeData(TempNode);
      NewMenuItem := TMenuItem.Create(Self);
      NewMenuItem.Caption := NodeData^.CompName;
      NewMenuItem.OnClick := nil;
      pmVarsEditor.Items.Add(NewMenuItem);

      SubMenuItem := TMenuItem.Create(Self);
      SubMenuItem.Caption := '$' + NodeData^.CompName + '_Left$';
      SubMenuItem.OnClick := @MenuItem_vstAvoidedZonesEditorMenuClick;
      NewMenuItem.Add(SubMenuItem);

      SubMenuItem := TMenuItem.Create(Self);
      SubMenuItem.Caption := '$' + NodeData^.CompName + '_Top$';
      SubMenuItem.OnClick := @MenuItem_vstAvoidedZonesEditorMenuClick;
      NewMenuItem.Add(SubMenuItem);

      SubMenuItem := TMenuItem.Create(Self);
      SubMenuItem.Caption := '$' + NodeData^.CompName + '_Right$';
      SubMenuItem.OnClick := @MenuItem_vstAvoidedZonesEditorMenuClick;
      NewMenuItem.Add(SubMenuItem);

      SubMenuItem := TMenuItem.Create(Self);
      SubMenuItem.Caption := '$' + NodeData^.CompName + '_Bottom$';
      SubMenuItem.OnClick := @MenuItem_vstAvoidedZonesEditorMenuClick;
      NewMenuItem.Add(SubMenuItem);

      TempNode := vstComponents.GetNext(TempNode);
    until TempNode = nil;
  end;
end;


procedure TfrClickerWinInterp.vstAvoidedZonesCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  TempStringEditLink: TStringEditLink;
  i: Integer;
  NodeData: PAvoidedZoneRec;
  EditorValue: string;
begin
  TempStringEditLink := TStringEditLink.Create;
  EditLink := TempStringEditLink;

  FTextEditorEditBox := TEdit(TCustomEdit(TempStringEditLink.Edit));

  FTextEditorEditBox.PopupMenu := pmVarsEditor;
  //FTextEditorEditBox.OnExit := edtTextEditorExit;
  //FTextEditorEditBox.OnKeyDown := edtTextEditorKeyDown;
  //FTextEditorEditBox.OnKeyUp := edtTextEditorKeyUp;

  FTextEditorEditBox.Hint := 'Right-click for available var/replacements.';

  NodeData := vstAvoidedZones.GetNodeData(Node);
  if NodeData <> nil then
  begin
    EditorValue := '';               //FTextEditorEditBox.Text is '', because the editor is created here
    case Column of
      5: EditorValue := NodeData^.ZRectStr.Left;
      6: EditorValue := NodeData^.ZRectStr.Top;
      7: EditorValue := NodeData^.ZRectStr.Right;
      8: EditorValue := NodeData^.ZRectStr.Bottom;
    end;
    FTextEditorEditBox.Hint := FTextEditorEditBox.Hint + #13#10 + EditorValue + ' = ' + EvaluateScanedComponents(EditorValue);

    FTextEditorEditBox.Hint := FTextEditorEditBox.Hint + #13#10#13#10;
    for i := 0 to Min(4 * 15, FListOfScannedComponents.Count) - 1 do
      FTextEditorEditBox.Hint := FTextEditorEditBox.Hint + FListOfScannedComponents[i] + #13#10;

    if FListOfScannedComponents.Count > 4 * 15 then
      FTextEditorEditBox.Hint := FTextEditorEditBox.Hint + #13#10 + '...';
  end;

  FTextEditorEditBox.ShowHint := True;

  FillInAvoidedZonesEditorMenu;
  FTextEditorEditBox.Show;

  Application.ProcessMessages;
  FTextEditorEditBox.SetFocus;
end;


procedure TfrClickerWinInterp.vstComponentsLoadNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
var
  NodeData: PHighlightedCompRec;
  DataStr: string;
  Len: LongInt;
begin
  NodeData := vstComponents.GetNodeData(Node);

  Stream.Read(NodeData^.CompRec.Handle, 4);

  Stream.Read(Len, 4);
  SetLength(DataStr, Len);
  Stream.Read(DataStr[1], Len);
  NodeData^.CompRec.Text := DataStr;

  Stream.Read(Len, 4);
  SetLength(DataStr, Len);
  Stream.Read(DataStr[1], Len);
  NodeData^.CompRec.ClassName := DataStr;

  if FCompTreeVersion > 1 then
  begin
    Stream.Read(Len, 4);
    SetLength(DataStr, Len);
    Stream.Read(DataStr[1], Len);
    NodeData^.CGClassName := DataStr;
  end;

  Stream.Read(NodeData^.CompRec.ComponentRectangle.Left, 4);
  Stream.Read(NodeData^.CompRec.ComponentRectangle.Top, 4);
  Stream.Read(NodeData^.CompRec.ComponentRectangle.Right, 4);
  Stream.Read(NodeData^.CompRec.ComponentRectangle.Bottom, 4);

  Stream.Read(NodeData^.LocalX, 4);
  Stream.Read(NodeData^.LocalY, 4);

  if FCompTreeVersion > 1 then
  begin
    Stream.Read(NodeData^.LocalX_FromParent, 4);
    Stream.Read(NodeData^.LocalY_FromParent, 4);
  end;

  Stream.Read(NodeData^.AssignedColor, 4);
  Stream.Read(NodeData^.ManuallyAdded, 1);

  if FCompTreeVersion > 2 then
  begin
    Stream.Read(Len, 4);
    SetLength(DataStr, Len);
    Stream.Read(DataStr[1], Len);
    NodeData^.CompName := DataStr;
  end;
end;


const
  CTreeHeader = #254#254#254#254 + 'TreeVersion_';

procedure TfrClickerWinInterp.vstComponentsLoadTree(Sender: TBaseVirtualTree;
  Stream: TStream);
//var
//  InitialStreamPos: Int64;
//  DataStr: string;
//  Len: LongInt;
begin                                            //this handler is called to late for setting FCompTreeVersion
  //InitialStreamPos := Stream.Position;
  //try
  //  Stream.Read(Len, 4);
  //  SetLength(DataStr, Len);
  //  Stream.Read(DataStr[1], Len);
  //
  //  if DataStr = CTreeHeader then   //Tree version present. This should be at least v2.
  //    Stream.Read(FCompTreeVersion, 4)
  //  else
  //    FCompTreeVersion := 1; //default
  //finally
  //  Stream.Position := InitialStreamPos;
  //end;
end;


procedure TfrClickerWinInterp.vstComponentsSaveNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
var
  NodeData: PHighlightedCompRec;
  DataStr: string;
  Len: LongInt;
begin
  NodeData := vstComponents.GetNodeData(Node);

  Stream.Write(NodeData^.CompRec.Handle, 4);

  DataStr := NodeData^.CompRec.Text;
  Len := Length(DataStr);
  Stream.Write(Len, 4);
  Stream.Write(DataStr[1], Len);

  DataStr := NodeData^.CompRec.ClassName;
  Len := Length(DataStr);
  Stream.Write(Len, 4);
  Stream.Write(DataStr[1], Len);

  DataStr := NodeData^.CGClassName;
  Len := Length(DataStr);
  Stream.Write(Len, 4);            //since v2
  Stream.Write(DataStr[1], Len);   //since v2

  Stream.Write(NodeData^.CompRec.ComponentRectangle.Left, 4);
  Stream.Write(NodeData^.CompRec.ComponentRectangle.Top, 4);
  Stream.Write(NodeData^.CompRec.ComponentRectangle.Right, 4);
  Stream.Write(NodeData^.CompRec.ComponentRectangle.Bottom, 4);

  Stream.Write(NodeData^.LocalX, 4);
  Stream.Write(NodeData^.LocalY, 4);

  Stream.Write(NodeData^.LocalX_FromParent, 4);  //since v2
  Stream.Write(NodeData^.LocalY_FromParent, 4);  //since v2

  Stream.Write(NodeData^.AssignedColor, 4);
  Stream.Write(NodeData^.ManuallyAdded, 1);       //since v2, although this field is loaded in v1 (which, in theory, adds an offset to Stream.Position)

  DataStr := NodeData^.CompName;
  Len := Length(DataStr);
  Stream.Write(Len, 4);            //since v3
  Stream.Write(DataStr[1], Len);   //since v3
end;


procedure TfrClickerWinInterp.vstComponentsSaveTree(Sender: TBaseVirtualTree;
  Stream: TStream);
var
  DataStr: string;
  Len, {%H-}TreeVersion: LongInt;
begin
  DataStr := CTreeHeader;
  Len := Length(DataStr);
  Stream.Write(Len, 4);
  Stream.Write(DataStr[1], Len);

  TreeVersion := 3; //current version
  Stream.Write(TreeVersion, 4);
end;


procedure TfrClickerWinInterp.ScanTargetControl;
var
  tp: TPoint;
  Comp: TCompRec;
  SrcRect, DestRect: TRect;
  FullScreenBmp: TBitmap;
begin
  GetCursorPos(tp);
  Comp := GetWindowClassRec(tp);

  memCompInfo.Clear;
  memCompInfo.Lines.Add('Handle=' + IntToStr(Comp.Handle));
  memCompInfo.Lines.Add('Class=' + Comp.ClassName);
  memCompInfo.Lines.Add('Text=' + Comp.Text);

  if pnlDrag.Color <> clLime then
    pnlDrag.Color := clLime;

  imgLiveScreenshot.Width := Comp.ComponentRectangle.Width;
  imgLiveScreenshot.Height := Comp.ComponentRectangle.Height;
  imgLiveScreenshot.Picture.Bitmap.Width := imgLiveScreenshot.Width;
  imgLiveScreenshot.Picture.Bitmap.Height := imgLiveScreenshot.Height;

  scrboxScannedComponents.HorzScrollBar.Position := 0;
  scrboxScannedComponents.VertScrollBar.Position := 0;

  if not chkFullScr.Checked then
    ScreenShot(Comp.Handle, imgLiveScreenshot.Picture.Bitmap, 0, 0, Comp.ComponentRectangle.Width, Comp.ComponentRectangle.Height)
  else
  begin
    //WipeBitmap(imgLiveScreenshot.Picture.Bitmap, Comp.ComponentRectangle.Width, Comp.ComponentRectangle.Height);

    SrcRect := Comp.ComponentRectangle;

    DestRect.Left := 0;
    DestRect.Top := 0;
    DestRect.Width := Comp.ComponentRectangle.Width;
    DestRect.Height := Comp.ComponentRectangle.Height;

    FullScreenBmp := TBitmap.Create;
    try
      ScreenShot(0, FullScreenBmp, 0, 0, Screen.Width, Screen.Height);
      imgLiveScreenshot.Picture.Bitmap.Canvas.CopyRect(DestRect, FullScreenBmp.Canvas, SrcRect);

      //BitBlt(imgLiveScreenshot.Picture.Bitmap.Canvas.Handle,  //dest DC
      //  0, //X   x-coord of destination upper-left corner
      //  0, //Y   y-coord of destination upper-left corner
      //  SrcRect.Width,   //src and dest width
      //  SrcRect.Height,  //src and dest height
      //  FullScreenBmp.Canvas.Handle,      //src DC
      //  SrcRect.Left,     //offset for source
      //  SrcRect.Top,     //offset for source
      //  SRCCOPY);
    finally
      FullScreenBmp.Free;
    end;
  end;
end;


procedure TfrClickerWinInterp.pnlDragMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then
    Exit;

  FDragging := True;

  imgLiveScreenshot.Canvas.Pen.Color := clWhite;
  imgLiveScreenshot.Canvas.Brush.Color := clWhite;
  imgLiveScreenshot.Canvas.Rectangle(0, 0, imgLiveScreenshot.Width - 1, imgLiveScreenshot.Height - 1);

  //imgLiveScreenshot.Show;
  imgLiveScreenshot.Tag := rdgrpLayers.ItemIndex;
  rdgrpLayers.ItemIndex := 4;  //LiveScreenshot

  tmrScan.Enabled := True;
end;


procedure TfrClickerWinInterp.pnlDragMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if FDragging then
    //ScanTargetControl;  //called by timer
end;


procedure TfrClickerWinInterp.pnlDragMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  tp: TPoint;
  Comp: TCompRec;
begin
  if FDragging then
  begin
    FDragging := False;
    tmrScan.Enabled := False;

    GetCursorPos(tp);
    Comp := GetWindowClassRec(tp);
    FInterprettedHandle := Comp.Handle;
    GetWindowInfo;
    pnlDrag.Color := clYellow;

    if vstComponents.RootNodeCount > 0 then
    begin
      //imgLiveScreenshot.Hide;  //hidden when recording
      rdgrpLayers.ItemIndex := imgLiveScreenshot.Tag; //restore to previous layer (which may even be LiveScreenshot)
    end;
  end;
end;


procedure TfrClickerWinInterp.pnlHorizSplitterMouseDown(Sender: TObject;
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


procedure TfrClickerWinInterp.pnlHorizSplitterMouseMove(Sender: TObject;
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


procedure TfrClickerWinInterp.pnlHorizSplitterMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FHold := False;
end;


procedure TfrClickerWinInterp.FrameResize(Sender: TObject);
var
  NewLeft: Integer;
begin
  NewLeft := pnlHorizSplitter.Left;

  if NewLeft > Width - 260 then
    NewLeft := Width - 260;

  ResizeFrameSectionsBySplitter(NewLeft);
end;


procedure TfrClickerWinInterp.ResizeFrameSectionsBySplitter(NewLeft: Integer);
begin
  if NewLeft < scrboxScannedComponents.Constraints.MinWidth then
    NewLeft := scrboxScannedComponents.Constraints.MinWidth;

  if NewLeft > Width - 460 - pnlHorizSplitter.Width then
    NewLeft := Width - 460 - pnlHorizSplitter.Width;

  if NewLeft < 424 then
    NewLeft := 424;

  pnlHorizSplitter.Left := NewLeft;

  pnlWinInterpSettings.Left := pnlHorizSplitter.Left + pnlHorizSplitter.Width;
  pnlWinInterpSettings.Width := Width - pnlWinInterpSettings.Left;
  scrboxScannedComponents.Width := pnlHorizSplitter.Left;
end;


procedure TfrClickerWinInterp.CreateRemainingComponents;
var
  NewColum: TVirtualTreeColumn;
begin
  vstComponents := TVirtualStringTree.Create(Self);
  vstComponents.Parent := pnlvstComponents;

  vstComponents.Left := 0;
  vstComponents.Height := pnlvstComponents.Height - 1;
  vstComponents.Top := 0;
  vstComponents.Width := pnlvstComponents.Width - 1;
  vstComponents.Anchors := [akLeft, akTop, akRight, akBottom];
  vstComponents.NodeDataSize := SizeOf(THighlightedCompRec);
  //vstComponents.ButtonStyle := bsTriangle; //bsRectangle; //bsRectangle - to use *.res,     bsTriangle - to draw a triangle in code: line 13162
  //vstComponents.ButtonFillMode := fmShaded; //to use *.res
  vstComponents.Colors.UnfocusedColor := clMedGray;
  vstComponents.Colors.UnfocusedSelectionColor := clGradientInactiveCaption;
  vstComponents.DefaultText := 'Node';
  vstComponents.Header.AutoSizeIndex := 0;
  vstComponents.Header.DefaultHeight := 21;
  vstComponents.Header.Height := 21;
  vstComponents.Header.Options := [hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible];
  vstComponents.Header.Style := hsFlatButtons;
  vstComponents.PopupMenu := pmComponents;
  vstComponents.TabOrder := 1;
  vstComponents.TreeOptions.AutoOptions := [toAutoDropExpand, {toAutoScrollOnExpand,} toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toDisableAutoscrollOnEdit];
  vstComponents.TreeOptions.MiscOptions := [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, {toToggleOnDblClick,} toWheelPanning, toEditOnClick];
  vstComponents.TreeOptions.SelectionOptions := [toFullRowSelect, toMultiSelect];
  vstComponents.OnClick := @vstComponentsClick;
  vstComponents.OnDblClick := @vstComponentsDblClick;
  vstComponents.OnEdited := @vstComponentsEdited;
  vstComponents.OnEditing := @vstComponentsEditing;
  vstComponents.OnNewText := @vstComponentsNewText;
  vstComponents.OnMouseUp := @vstComponentsMouseUp;
  vstComponents.OnGetText := @vstComponentsGetText;
  vstComponents.OnLoadNode := @vstComponentsLoadNode;
  vstComponents.OnLoadTree := @vstComponentsLoadTree;
  vstComponents.OnSaveNode := @vstComponentsSaveNode;
  vstComponents.OnSaveTree := @vstComponentsSaveTree;
  vstComponents.OnKeyDown := @vstComponentsKeyDown;

  NewColum := vstComponents.Header.Columns.Add;
  NewColum.MinWidth := 150;
  NewColum.Position := 0;
  NewColum.Width := 150;
  NewColum.Text := 'Handle';

  NewColum := vstComponents.Header.Columns.Add;
  NewColum.MinWidth := 150;
  NewColum.Position := 1;
  NewColum.Width := 150;
  NewColum.Text := 'Class';

  NewColum := vstComponents.Header.Columns.Add;
  NewColum.MinWidth := 150;
  NewColum.Position := 2;
  NewColum.Width := 150;
  NewColum.Text := 'Text';

  NewColum := vstComponents.Header.Columns.Add;
  NewColum.MinWidth := 150;
  NewColum.Position := 3;
  NewColum.Width := 150;
  NewColum.Text := 'CG Class';

  NewColum := vstComponents.Header.Columns.Add;
  NewColum.MinWidth := 100;
  NewColum.Position := 4;
  NewColum.Width := 100;
  NewColum.Text := 'Assigned Color';

  NewColum := vstComponents.Header.Columns.Add;
  NewColum.MinWidth := 50;
  NewColum.Position := 5;
  NewColum.Width := 50;
  NewColum.Text := 'Left';

  NewColum := vstComponents.Header.Columns.Add;
  NewColum.MinWidth := 50;
  NewColum.Position := 6;
  NewColum.Width := 50;
  NewColum.Text := 'Top';

  NewColum := vstComponents.Header.Columns.Add;
  NewColum.MinWidth := 50;
  NewColum.Position := 7;
  NewColum.Width := 50;
  NewColum.Text := 'Right';

  NewColum := vstComponents.Header.Columns.Add;
  NewColum.MinWidth := 70;
  NewColum.Position := 8;
  NewColum.Width := 70;
  NewColum.Text := 'Bottom';

  NewColum := vstComponents.Header.Columns.Add;
  NewColum.MinWidth := 50;
  NewColum.Position := 9;
  NewColum.Width := 50;
  NewColum.Text := 'Width';

  NewColum := vstComponents.Header.Columns.Add;
  NewColum.MinWidth := 70;
  NewColum.Position := 10;
  NewColum.Width := 70;
  NewColum.Text := 'Height';

  NewColum := vstComponents.Header.Columns.Add;
  NewColum.MinWidth := 70;
  NewColum.Position := 11;
  NewColum.Width := 70;
  NewColum.Text := 'Local Left';

  NewColum := vstComponents.Header.Columns.Add;
  NewColum.MinWidth := 70;
  NewColum.Position := 12;
  NewColum.Width := 70;
  NewColum.Text := 'Local Top';

  NewColum := vstComponents.Header.Columns.Add;
  NewColum.MinWidth := 70;
  NewColum.Position := 13;
  NewColum.Width := 170;
  NewColum.Text := 'Local Left from parent';

  NewColum := vstComponents.Header.Columns.Add;
  NewColum.MinWidth := 70;
  NewColum.Position := 14;
  NewColum.Width := 170;
  NewColum.Text := 'Local Top from parent';

  NewColum := vstComponents.Header.Columns.Add;
  NewColum.MinWidth := 99;
  NewColum.Position := 15;
  NewColum.Width := 99;
  NewColum.Text := 'Manually Added';

  NewColum := vstComponents.Header.Columns.Add;
  NewColum.MinWidth := 150;
  NewColum.Position := 16;
  NewColum.Width := 150;
  NewColum.Text := 'Component Name';

  vstAvoidedZones := TVirtualStringTree.Create(Self);
  vstAvoidedZones.Parent := pnlvstSettings;

  vstAvoidedZones.Left := 0;
  vstAvoidedZones.Height := pnlvstSettings.Height;
  vstAvoidedZones.Top := 0;
  vstAvoidedZones.Width := pnlvstSettings.Width;
  vstAvoidedZones.Indent := 0;
  vstAvoidedZones.Anchors := [akLeft, akTop, akRight, akBottom];
  vstAvoidedZones.NodeDataSize := SizeOf(TAvoidedZoneRec);
  //vstAvoidedZones.ButtonStyle := bsTriangle; //bsRectangle; //bsRectangle - to use *.res,     bsTriangle - to draw a triangle in code: line 13162
  //vstAvoidedZones.ButtonFillMode := fmShaded; //to use *.res
  vstAvoidedZones.Colors.UnfocusedColor := clMedGray;
  vstAvoidedZones.Colors.UnfocusedSelectionColor := clGradientInactiveCaption;
  vstAvoidedZones.DefaultText := 'Node';
  vstAvoidedZones.Header.AutoSizeIndex := 0;
  vstAvoidedZones.Header.DefaultHeight := 21;
  vstAvoidedZones.Header.Height := 21;
  vstAvoidedZones.Header.Options := [hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible];
  vstAvoidedZones.Header.Style := hsFlatButtons;
  vstAvoidedZones.PopupMenu := pmAvoidedZones;
  vstAvoidedZones.TabOrder := 1;
  vstAvoidedZones.TreeOptions.AutoOptions := [toAutoDropExpand, {toAutoScrollOnExpand,} toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toDisableAutoscrollOnEdit];
  vstAvoidedZones.TreeOptions.MiscOptions := [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, {toToggleOnDblClick,} toWheelPanning, toEditOnClick];
  vstAvoidedZones.TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, {toShowTreeLines,} toShowRoot, toThemeAware, toUseBlendedImages];
  vstAvoidedZones.TreeOptions.SelectionOptions := [toFullRowSelect];
  vstAvoidedZones.OnDblClick := @vstAvoidedZonesDblClick;
  vstAvoidedZones.OnEdited := @vstAvoidedZonesEdited;
  vstAvoidedZones.OnEditing := @vstAvoidedZonesEditing;
  vstAvoidedZones.OnNewText := @vstAvoidedZonesNewText;
  vstAvoidedZones.OnMouseUp := @vstAvoidedZonesMouseUp;
  vstAvoidedZones.OnGetText := @vstAvoidedZonesGetText;
  vstAvoidedZones.OnKeyDown := @vstAvoidedZonesKeyDown;
  vstAvoidedZones.OnCreateEditor := @vstAvoidedZonesCreateEditor;
  vstAvoidedZones.OnPaintText := @vstAvoidedZonesPaintText;

  NewColum := vstAvoidedZones.Header.Columns.Add;
  NewColum.MinWidth := 70;
  NewColum.Position := 0;
  NewColum.Width := 70;
  NewColum.Text := 'Left offset';

  NewColum := vstAvoidedZones.Header.Columns.Add;
  NewColum.MinWidth := 70;
  NewColum.Position := 1;
  NewColum.Width := 70;
  NewColum.Text := 'Top offset';

  NewColum := vstAvoidedZones.Header.Columns.Add;
  NewColum.MinWidth := 80;
  NewColum.Position := 2;
  NewColum.Width := 80;
  NewColum.Text := 'Right offset';

  NewColum := vstAvoidedZones.Header.Columns.Add;
  NewColum.MinWidth := 90;
  NewColum.Position := 3;
  NewColum.Width := 90;
  NewColum.Text := 'Bottom offset';

  NewColum := vstAvoidedZones.Header.Columns.Add;
  NewColum.MinWidth := 100;
  NewColum.Position := 4;
  NewColum.Width := 150;
  NewColum.Text := 'Zone name/note';

  NewColum := vstAvoidedZones.Header.Columns.Add;
  NewColum.MinWidth := 150;
  NewColum.Position := 5;
  NewColum.Width := 150;
  NewColum.Text := 'Left';

  NewColum := vstAvoidedZones.Header.Columns.Add;
  NewColum.MinWidth := 150;
  NewColum.Position := 6;
  NewColum.Width := 150;
  NewColum.Text := 'Top';

  NewColum := vstAvoidedZones.Header.Columns.Add;
  NewColum.MinWidth := 150;
  NewColum.Position := 7;
  NewColum.Width := 150;
  NewColum.Text := 'Right';

  NewColum := vstAvoidedZones.Header.Columns.Add;
  NewColum.MinWidth := 150;
  NewColum.Position := 8;
  NewColum.Width := 150;
  NewColum.Text := 'Bottom';

  FSelectedComponentLeftLimitLabel := TLabel.Create(Self);
  FSelectedComponentTopLimitLabel := TLabel.Create(Self);
  FSelectedComponentRightLimitLabel := TLabel.Create(Self);
  FSelectedComponentBottomLimitLabel := TLabel.Create(Self);

  FSelectedComponentLeftLimitLabel.Parent := scrboxScannedComponents;
  FSelectedComponentTopLimitLabel.Parent := scrboxScannedComponents;
  FSelectedComponentRightLimitLabel.Parent := scrboxScannedComponents;
  FSelectedComponentBottomLimitLabel.Parent := scrboxScannedComponents;

  FSelectedComponentLeftLimitLabel.AutoSize := False;
  FSelectedComponentTopLimitLabel.AutoSize := False;
  FSelectedComponentRightLimitLabel.AutoSize := False;
  FSelectedComponentBottomLimitLabel.AutoSize := False;

  FSelectedComponentLeftLimitLabel.Caption := '';
  FSelectedComponentTopLimitLabel.Caption := '';
  FSelectedComponentRightLimitLabel.Caption := '';
  FSelectedComponentBottomLimitLabel.Caption := '';

  FSelectedComponentLeftLimitLabel.Color := CLabel_Orange;
  FSelectedComponentTopLimitLabel.Color := CLabel_Orange;
  FSelectedComponentRightLimitLabel.Color := CLabel_Orange;
  FSelectedComponentBottomLimitLabel.Color := CLabel_Orange;

  FSelectedComponentLeftLimitLabel.Width := 1;
  FSelectedComponentTopLimitLabel.Height := 1;
  FSelectedComponentRightLimitLabel.Width := 1;
  FSelectedComponentBottomLimitLabel.Height := 1;

  FSelectedComponentLeftLimitLabel.Transparent := False;
  FSelectedComponentTopLimitLabel.Transparent := False;
  FSelectedComponentRightLimitLabel.Transparent := False;
  FSelectedComponentBottomLimitLabel.Transparent := False;

  FSelectedComponentLeftLimitLabel.OnMouseUp := @FSelectionsLinesMouseUp;
  FSelectedComponentTopLimitLabel.OnMouseUp := @FSelectionsLinesMouseUp;
  FSelectedComponentRightLimitLabel.OnMouseUp := @FSelectionsLinesMouseUp;
  FSelectedComponentBottomLimitLabel.OnMouseUp := @FSelectionsLinesMouseUp;

  FSelectedComponentLeftLimitLabel.OnMouseEnter := @imgScannedWindowWithAvoidedZonesMouseEnter;
  FSelectedComponentTopLimitLabel.OnMouseEnter := @imgScannedWindowWithAvoidedZonesMouseEnter;
  FSelectedComponentRightLimitLabel.OnMouseEnter := @imgScannedWindowWithAvoidedZonesMouseEnter;
  FSelectedComponentBottomLimitLabel.OnMouseEnter := @imgScannedWindowWithAvoidedZonesMouseEnter;

  FTransparent_SelectedComponentLeftLimitLabel := TLabel.Create(Self);
  FTransparent_SelectedComponentTopLimitLabel := TLabel.Create(Self);
  FTransparent_SelectedComponentRightLimitLabel := TLabel.Create(Self);
  FTransparent_SelectedComponentBottomLimitLabel := TLabel.Create(Self);

  FTransparent_SelectedComponentLeftLimitLabel.Parent := scrboxScannedComponents;
  FTransparent_SelectedComponentTopLimitLabel.Parent := scrboxScannedComponents;
  FTransparent_SelectedComponentRightLimitLabel.Parent := scrboxScannedComponents;
  FTransparent_SelectedComponentBottomLimitLabel.Parent := scrboxScannedComponents;

  FTransparent_SelectedComponentLeftLimitLabel.AutoSize := False;
  FTransparent_SelectedComponentTopLimitLabel.AutoSize := False;
  FTransparent_SelectedComponentRightLimitLabel.AutoSize := False;
  FTransparent_SelectedComponentBottomLimitLabel.AutoSize := False;

  FTransparent_SelectedComponentLeftLimitLabel.Caption := '';
  FTransparent_SelectedComponentTopLimitLabel.Caption := '';
  FTransparent_SelectedComponentRightLimitLabel.Caption := '';
  FTransparent_SelectedComponentBottomLimitLabel.Caption := '';

  FTransparent_SelectedComponentLeftLimitLabel.Color := clDefault;
  FTransparent_SelectedComponentTopLimitLabel.Color := clDefault;
  FTransparent_SelectedComponentRightLimitLabel.Color := clDefault;
  FTransparent_SelectedComponentBottomLimitLabel.Color := clDefault;

  FTransparent_SelectedComponentLeftLimitLabel.Width := 7;
  FTransparent_SelectedComponentTopLimitLabel.Height := 7;
  FTransparent_SelectedComponentRightLimitLabel.Width := 7;
  FTransparent_SelectedComponentBottomLimitLabel.Height := 7;

  FTransparent_SelectedComponentLeftLimitLabel.Transparent := True;
  FTransparent_SelectedComponentTopLimitLabel.Transparent := True;
  FTransparent_SelectedComponentRightLimitLabel.Transparent := True;
  FTransparent_SelectedComponentBottomLimitLabel.Transparent := True;

  FTransparent_SelectedComponentLeftLimitLabel.Cursor := crSizeWE;
  FTransparent_SelectedComponentTopLimitLabel.Cursor := crSizeNS;
  FTransparent_SelectedComponentRightLimitLabel.Cursor := crSizeWE;
  FTransparent_SelectedComponentBottomLimitLabel.Cursor := crSizeNS;

  FTransparent_SelectedComponentLeftLimitLabel.OnMouseDown := @FTransparent_LeftMouseDown;
  FTransparent_SelectedComponentLeftLimitLabel.OnMouseMove := @FTransparent_LeftMouseMove;
  FTransparent_SelectedComponentLeftLimitLabel.OnMouseUp := @FTransparent_LeftMouseUp;

  FTransparent_SelectedComponentRightLimitLabel.OnMouseDown := @FTransparent_RightMouseDown;
  FTransparent_SelectedComponentRightLimitLabel.OnMouseMove := @FTransparent_RightMouseMove;
  FTransparent_SelectedComponentRightLimitLabel.OnMouseUp := @FTransparent_RightMouseUp;

  FTransparent_SelectedComponentTopLimitLabel.OnMouseDown := @FTransparent_TopMouseDown;
  FTransparent_SelectedComponentTopLimitLabel.OnMouseMove := @FTransparent_TopMouseMove;
  FTransparent_SelectedComponentTopLimitLabel.OnMouseUp := @FTransparent_TopMouseUp;

  FTransparent_SelectedComponentBottomLimitLabel.OnMouseDown := @FTransparent_BottomMouseDown;
  FTransparent_SelectedComponentBottomLimitLabel.OnMouseMove := @FTransparent_BottomMouseMove;
  FTransparent_SelectedComponentBottomLimitLabel.OnMouseUp := @FTransparent_BottomMouseUp;

  FTransparent_SelectedComponentLeftLimitLabel.OnMouseEnter := @imgScannedWindowWithAvoidedZonesMouseEnter;
  FTransparent_SelectedComponentTopLimitLabel.OnMouseEnter := @imgScannedWindowWithAvoidedZonesMouseEnter;
  FTransparent_SelectedComponentRightLimitLabel.OnMouseEnter := @imgScannedWindowWithAvoidedZonesMouseEnter;
  FTransparent_SelectedComponentBottomLimitLabel.OnMouseEnter := @imgScannedWindowWithAvoidedZonesMouseEnter;

  ///

  FSelectedZoneLeftLimitLabel := TLabel.Create(Self);
  FSelectedZoneTopLimitLabel := TLabel.Create(Self);
  FSelectedZoneRightLimitLabel := TLabel.Create(Self);
  FSelectedZoneBottomLimitLabel := TLabel.Create(Self);

  FSelectedZoneLeftLimitLabel.Parent := scrboxScannedComponents;
  FSelectedZoneTopLimitLabel.Parent := scrboxScannedComponents;
  FSelectedZoneRightLimitLabel.Parent := scrboxScannedComponents;
  FSelectedZoneBottomLimitLabel.Parent := scrboxScannedComponents;

  FSelectedZoneLeftLimitLabel.AutoSize := False;
  FSelectedZoneTopLimitLabel.AutoSize := False;
  FSelectedZoneRightLimitLabel.AutoSize := False;
  FSelectedZoneBottomLimitLabel.AutoSize := False;

  FSelectedZoneLeftLimitLabel.Caption := '';
  FSelectedZoneTopLimitLabel.Caption := '';
  FSelectedZoneRightLimitLabel.Caption := '';
  FSelectedZoneBottomLimitLabel.Caption := '';

  FSelectedZoneLeftLimitLabel.Color := clRed;
  FSelectedZoneTopLimitLabel.Color := clRed;
  FSelectedZoneRightLimitLabel.Color := clRed;
  FSelectedZoneBottomLimitLabel.Color := clRed;

  FSelectedZoneLeftLimitLabel.Width := 1;
  FSelectedZoneTopLimitLabel.Height := 1;
  FSelectedZoneRightLimitLabel.Width := 1;
  FSelectedZoneBottomLimitLabel.Height := 1;

  FSelectedZoneLeftLimitLabel.Transparent := False;
  FSelectedZoneTopLimitLabel.Transparent := False;
  FSelectedZoneRightLimitLabel.Transparent := False;
  FSelectedZoneBottomLimitLabel.Transparent := False;

  FSelectedZoneLeftLimitLabel.Visible := False; //hidden by default
  FSelectedZoneTopLimitLabel.Visible := False; //hidden by default
  FSelectedZoneRightLimitLabel.Visible := False; //hidden by default
  FSelectedZoneBottomLimitLabel.Visible := False; //hidden by default

  FSelectedZoneLeftLimitLabel.OnMouseEnter := @imgScannedWindowWithAvoidedZonesMouseEnter;
  FSelectedZoneTopLimitLabel.OnMouseEnter := @imgScannedWindowWithAvoidedZonesMouseEnter;
  FSelectedZoneRightLimitLabel.OnMouseEnter := @imgScannedWindowWithAvoidedZonesMouseEnter;
  FSelectedZoneBottomLimitLabel.OnMouseEnter := @imgScannedWindowWithAvoidedZonesMouseEnter;


  FTransparent_SelectedZoneLeftLimitLabel := TLabel.Create(Self);
  FTransparent_SelectedZoneTopLimitLabel := TLabel.Create(Self);
  FTransparent_SelectedZoneRightLimitLabel := TLabel.Create(Self);
  FTransparent_SelectedZoneBottomLimitLabel := TLabel.Create(Self);

  FTransparent_SelectedZoneLeftLimitLabel.Parent := scrboxScannedComponents;
  FTransparent_SelectedZoneTopLimitLabel.Parent := scrboxScannedComponents;
  FTransparent_SelectedZoneRightLimitLabel.Parent := scrboxScannedComponents;
  FTransparent_SelectedZoneBottomLimitLabel.Parent := scrboxScannedComponents;

  FTransparent_SelectedZoneLeftLimitLabel.AutoSize := False;
  FTransparent_SelectedZoneTopLimitLabel.AutoSize := False;
  FTransparent_SelectedZoneRightLimitLabel.AutoSize := False;
  FTransparent_SelectedZoneBottomLimitLabel.AutoSize := False;

  FTransparent_SelectedZoneLeftLimitLabel.Caption := '';
  FTransparent_SelectedZoneTopLimitLabel.Caption := '';
  FTransparent_SelectedZoneRightLimitLabel.Caption := '';
  FTransparent_SelectedZoneBottomLimitLabel.Caption := '';

  FTransparent_SelectedZoneLeftLimitLabel.Color := clDefault;
  FTransparent_SelectedZoneTopLimitLabel.Color := clDefault;
  FTransparent_SelectedZoneRightLimitLabel.Color := clDefault;
  FTransparent_SelectedZoneBottomLimitLabel.Color := clDefault;

  FTransparent_SelectedZoneLeftLimitLabel.Width := 7;
  FTransparent_SelectedZoneTopLimitLabel.Height := 7;
  FTransparent_SelectedZoneRightLimitLabel.Width := 7;
  FTransparent_SelectedZoneBottomLimitLabel.Height := 7;

  FTransparent_SelectedZoneLeftLimitLabel.Transparent := True;
  FTransparent_SelectedZoneTopLimitLabel.Transparent := True;
  FTransparent_SelectedZoneRightLimitLabel.Transparent := True;
  FTransparent_SelectedZoneBottomLimitLabel.Transparent := True;

  FTransparent_SelectedZoneLeftLimitLabel.Visible := False; //hidden by default
  FTransparent_SelectedZoneTopLimitLabel.Visible := False; //hidden by default
  FTransparent_SelectedZoneRightLimitLabel.Visible := False; //hidden by default
  FTransparent_SelectedZoneBottomLimitLabel.Visible := False; //hidden by default

  FTransparent_SelectedZoneLeftLimitLabel.Cursor := crSizeWE;
  FTransparent_SelectedZoneTopLimitLabel.Cursor := crSizeNS;
  FTransparent_SelectedZoneRightLimitLabel.Cursor := crSizeWE;
  FTransparent_SelectedZoneBottomLimitLabel.Cursor := crSizeNS;

  FTransparent_SelectedZoneLeftLimitLabel.OnMouseDown := @FTransparentZone_LeftMouseDown;
  FTransparent_SelectedZoneLeftLimitLabel.OnMouseMove := @FTransparentZone_LeftMouseMove;
  FTransparent_SelectedZoneLeftLimitLabel.OnMouseUp := @FTransparentZone_LeftMouseUp;

  FTransparent_SelectedZoneRightLimitLabel.OnMouseDown := @FTransparentZone_RightMouseDown;
  FTransparent_SelectedZoneRightLimitLabel.OnMouseMove := @FTransparentZone_RightMouseMove;
  FTransparent_SelectedZoneRightLimitLabel.OnMouseUp := @FTransparentZone_RightMouseUp;

  FTransparent_SelectedZoneTopLimitLabel.OnMouseDown := @FTransparentZone_TopMouseDown;
  FTransparent_SelectedZoneTopLimitLabel.OnMouseMove := @FTransparentZone_TopMouseMove;
  FTransparent_SelectedZoneTopLimitLabel.OnMouseUp := @FTransparentZone_TopMouseUp;

  FTransparent_SelectedZoneBottomLimitLabel.OnMouseDown := @FTransparentZone_BottomMouseDown;
  FTransparent_SelectedZoneBottomLimitLabel.OnMouseMove := @FTransparentZone_BottomMouseMove;
  FTransparent_SelectedZoneBottomLimitLabel.OnMouseUp := @FTransparentZone_BottomMouseUp;

  FTransparent_SelectedZoneLeftLimitLabel.OnMouseEnter := @imgScannedWindowWithAvoidedZonesMouseEnter;
  FTransparent_SelectedZoneTopLimitLabel.OnMouseEnter := @imgScannedWindowWithAvoidedZonesMouseEnter;
  FTransparent_SelectedZoneRightLimitLabel.OnMouseEnter := @imgScannedWindowWithAvoidedZonesMouseEnter;
  FTransparent_SelectedZoneBottomLimitLabel.OnMouseEnter := @imgScannedWindowWithAvoidedZonesMouseEnter;

  ///

  FProgressVertLabel := TLabel.Create(Self);
  FProgressHorizLabel := TLabel.Create(Self);

  FProgressVertLabel.Parent := scrboxScannedComponents;
  FProgressHorizLabel.Parent := scrboxScannedComponents;

  FProgressVertLabel.AutoSize := False;
  FProgressHorizLabel.AutoSize := False;

  FProgressVertLabel.Caption := '';
  FProgressHorizLabel.Caption := '';

  FProgressVertLabel.Color := clBlue;
  FProgressHorizLabel.Color := clBlue;

  FProgressVertLabel.Width := 1;
  FProgressHorizLabel.Height := 1;

  FProgressVertLabel.Transparent := False;
  FProgressHorizLabel.Transparent := False;

  FProgressVertLabel.Visible := False;
  FProgressHorizLabel.Visible := False;
end;


constructor TfrClickerWinInterp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FDragging := False;
  FDoneRec := False;
  BuildColors;

  FHold := False;
  FOldSelectedNode := nil;

  FInterprettedHandle := 0;
  FOnGetConnectionAddress := nil;

  FListOfScannedComponents := TStringList.Create;
  FListOfScannedComponents.LineBreak := #13#10;
  FListOfScannedComponents.OnChange := @EvaluateScanedComponentsOnChange;
  FTextEditorEditBox := nil;

  CreateRemainingComponents;

  colboxHighlightingLabels.AddItem('clOrange', TObject({%H-}Pointer(CLabel_Orange)));
  colboxHighlightingLabels.Selected := CLabel_Orange;

  imgLiveScreenshot.Left := 0;
  imgLiveScreenshot.Top := 0;
  FSelectionHold := False;
  FUpdatedVstText := False;
  FRecordSelectedAreaOnly := False;
  FRecordWithEdgeExtending := True;
  FDraggingForSelection := False;

  FSelectedComponentText := 'no selected component';
  FSelectedComponentClassName := 'no selected component';

  FOnGetConnectionAddress := nil;
  FOnInsertTreeComponent := nil;
  FOnClearWinInterp := nil;

  FOnOpenDialogExecute := nil;
  FOnGetOpenDialogFileName := nil;
  FOnSaveDialogExecute := nil;
  FOnGetSaveDialogFileName := nil;

  FOnLoadFileFromStream := nil;
  FOnSaveFileToStream := nil;

  pnlWinInterpSettings.Caption := '';
  PageControlWinInterp.ActivePageIndex := 0;

  imgScannedWindow.Left := 0;
  imgScannedWindow.Top := 0;
  imgScreenshot.Left := 0;
  imgScreenshot.Top := 0;
  imgAvgScreenshotAndGreenComp.Left := 0;
  imgAvgScreenshotAndGreenComp.Top := 0;
  imgAvgScreenshotAndAssignedComp.Left := 0;
  imgAvgScreenshotAndAssignedComp.Top := 0;
  imgLiveScreenshot.Left := 0;
  imgLiveScreenshot.Top := 0;
  imgHandleColors.Left := 0;
  imgHandleColors.Top := 0;
  imgScannedWindowWithText.Left := 0;
  imgScannedWindowWithText.Top := 0;
  imgScannedWindowWithAvoidedZones.Left := 0;
  imgScannedWindowWithAvoidedZones.Top := 0;

  //some default values
  imgScannedWindow.Width := 1920;
  imgScannedWindow.Height := 1080;
  imgScreenshot.Width := 1920;
  imgScreenshot.Height := 1080;
  imgAvgScreenshotAndGreenComp.Width := 1920;
  imgAvgScreenshotAndGreenComp.Height := 1080;
  imgAvgScreenshotAndAssignedComp.Width := 1920;
  imgAvgScreenshotAndAssignedComp.Height := 1080;
  imgLiveScreenshot.Width := 1920;
  imgLiveScreenshot.Height := 1080;
  imgHandleColors.Width := 1920;
  imgHandleColors.Height := 1080;
  imgScannedWindowWithText.Width := 1920;
  imgScannedWindowWithText.Height := 1080;
  imgScannedWindowWithAvoidedZones.Width := 1920;
  imgScannedWindowWithAvoidedZones.Height := 1080;
end;


destructor TfrClickerWinInterp.Destroy;
begin
  FreeAndNil(FListOfScannedComponents);
end;


procedure TfrClickerWinInterp.imgLiveScreenshotMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CurrentLabel: TLabel;
  NewLeft: Integer;
  NewTop: Integer;
begin
  if ssLeft in Shift then
  begin
    FDraggingForSelection := True;

    GetCursorPos(FMouseDownGlobalPos);

    CurrentLabel := FTransparent_SelectedComponentLeftLimitLabel;
    NewLeft := X - 3;
    CurrentLabel.Left := Max(0, Min(imgLiveScreenshot.Width - 1, NewLeft));
    FSelectedComponentLeftLimitLabel.Left := CurrentLabel.Left + 3;

    CurrentLabel := FTransparent_SelectedComponentTopLimitLabel;
    NewTop := Y - 3;
    CurrentLabel.Top := Max(0, Min(imgLiveScreenshot.Height - 1, NewTop));
    FSelectedComponentTopLimitLabel.Top := CurrentLabel.Top + 3;

    FSelectedComponentRightLimitLabel.Left := FSelectedComponentLeftLimitLabel.Left;
    FSelectedComponentBottomLimitLabel.Top := FSelectedComponentTopLimitLabel.Top;
  end;
end;


procedure TfrClickerWinInterp.imgLiveScreenshotMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewLeft: Integer;
  NewTop: Integer;
begin
  pnlMouseCoordsOnScreenshot.Caption := IntToStr(X) + ' : ' + IntToStr(Y);

  if FDraggingForSelection then
  begin
    NewLeft := X - 3;
    NewLeft := Max(FSelectedComponentLeftLimitLabel.Left + 5, Min(imgLiveScreenshot.Width - 2, NewLeft));
    FSelectedComponentRightLimitLabel.Left := NewLeft + 3;

    NewTop := Y - 3;
    NewTop := Max(FSelectedComponentTopLimitLabel.Top + 5, Min(imgLiveScreenshot.Height - 2, NewTop));
    FSelectedComponentBottomLimitLabel.Top := NewTop + 3;
  end;

  FCurrentMousePosOnPreviewImg.X := X;
  FCurrentMousePosOnPreviewImg.Y := Y;
  tmrDrawZoom.Enabled := True;
end;


procedure TfrClickerWinInterp.imgLiveScreenshotMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CurrentLabel: TLabel;
  NewLeft: Integer;
  NewTop: Integer;
begin
  if FDraggingForSelection then
  begin
    FDraggingForSelection := False;

    CurrentLabel := FTransparent_SelectedComponentRightLimitLabel;
    NewLeft := X - 3;
    CurrentLabel.Left := Max(FTransparent_SelectedComponentLeftLimitLabel.Left + 8, Min(imgLiveScreenshot.Width - 2, NewLeft));
    FSelectedComponentRightLimitLabel.Left := CurrentLabel.Left + 3;

    CurrentLabel := FTransparent_SelectedComponentBottomLimitLabel;
    NewTop := Y - 3;
    CurrentLabel.Top := Max(FTransparent_SelectedComponentTopLimitLabel.Top + 8, Min(imgLiveScreenshot.Height - 2, NewTop));
    FSelectedComponentBottomLimitLabel.Top := CurrentLabel.Top + 3;
  end;
end;


procedure TfrClickerWinInterp.AdjustHighlightingLabelsToScreenshot;
begin
  FSelectedComponentLeftLimitLabel.Top := 0;
  FSelectedComponentLeftLimitLabel.Height := imgScannedWindow.Height;

  FSelectedComponentTopLimitLabel.Left := 0;
  FSelectedComponentTopLimitLabel.Width := imgScannedWindow.Width;

  FSelectedComponentRightLimitLabel.Top := 0;
  FSelectedComponentRightLimitLabel.Height := imgScannedWindow.Height;

  FSelectedComponentBottomLimitLabel.Left := 0;
  FSelectedComponentBottomLimitLabel.Width := imgScannedWindow.Width;

  FTransparent_SelectedComponentLeftLimitLabel.Top := 0;
  FTransparent_SelectedComponentLeftLimitLabel.Height := imgScannedWindow.Height;

  FTransparent_SelectedComponentTopLimitLabel.Left := 0;
  FTransparent_SelectedComponentTopLimitLabel.Width := imgScannedWindow.Width;

  FTransparent_SelectedComponentRightLimitLabel.Top := 0;
  FTransparent_SelectedComponentRightLimitLabel.Height := imgScannedWindow.Height;

  FTransparent_SelectedComponentBottomLimitLabel.Left := 0;
  FTransparent_SelectedComponentBottomLimitLabel.Width := imgScannedWindow.Width;

  FSelectedZoneLeftLimitLabel.Top := 0;
  FSelectedZoneLeftLimitLabel.Height := imgScannedWindow.Height;

  FSelectedZoneTopLimitLabel.Left := 0;
  FSelectedZoneTopLimitLabel.Width := imgScannedWindow.Width;

  FSelectedZoneRightLimitLabel.Top := 0;
  FSelectedZoneRightLimitLabel.Height := imgScannedWindow.Height;

  FSelectedZoneBottomLimitLabel.Left := 0;
  FSelectedZoneBottomLimitLabel.Width := imgScannedWindow.Width;

  FTransparent_SelectedZoneLeftLimitLabel.Top := 0;
  FTransparent_SelectedZoneLeftLimitLabel.Height := imgScannedWindow.Height;

  FTransparent_SelectedZoneTopLimitLabel.Left := 0;
  FTransparent_SelectedZoneTopLimitLabel.Width := imgScannedWindow.Width;

  FTransparent_SelectedZoneRightLimitLabel.Top := 0;
  FTransparent_SelectedZoneRightLimitLabel.Height := imgScannedWindow.Height;

  FTransparent_SelectedZoneBottomLimitLabel.Left := 0;
  FTransparent_SelectedZoneBottomLimitLabel.Width := imgScannedWindow.Width;

  FProgressVertLabel.Height := imgScannedWindow.Height;
  FProgressHorizLabel.Width := imgScannedWindow.Width;
end;


procedure TfrClickerWinInterp.HighlightComponent(Node: PVirtualNode);
var
  NodeData: PHighlightedCompRec;
begin
  if Node = nil then
    Exit;

  NodeData := vstComponents.GetNodeData(Node);

  FSelectedComponentLeftLimitLabel.Left := NodeData^.LocalX;
  FSelectedComponentTopLimitLabel.Top := NodeData^.LocalY;
  FSelectedComponentRightLimitLabel.Left := NodeData^.LocalX + NodeData^.CompRec.ComponentRectangle.Width;
  FSelectedComponentBottomLimitLabel.Top := NodeData^.LocalY + NodeData^.CompRec.ComponentRectangle.Height;

  FTransparent_SelectedComponentLeftLimitLabel.Left := FSelectedComponentLeftLimitLabel.Left - 3;
  FTransparent_SelectedComponentTopLimitLabel.Top := FSelectedComponentTopLimitLabel.Top - 3;
  FTransparent_SelectedComponentRightLimitLabel.Left := FSelectedComponentRightLimitLabel.Left - 3;
  FTransparent_SelectedComponentBottomLimitLabel.Top := FSelectedComponentBottomLimitLabel.Top - 3;

  FSelectedComponentText := NodeData^.CompRec.Text;
  FSelectedComponentClassName := NodeData^.CompRec.ClassName;
end;


procedure TfrClickerWinInterp.EvaluateScanedComponentsOnChange(Sender: TObject);
begin
  //nothing here, just have an OnChange handler
end;


function TfrClickerWinInterp.EvaluateScanedComponents(s: string): string;
begin
  Result := EvaluateAllReplacements(FListOfScannedComponents, s);
end;


procedure TfrClickerWinInterp.ZoneRectStrToRect(var ARectStr: TRectString; var ADestRect: TRect);
begin
  ADestRect.Left := StrToIntDef(EvaluateScanedComponents(ARectStr.Left), 0) + StrToIntDef(ARectStr.LeftOffset, 0);
  ADestRect.Top := StrToIntDef(EvaluateScanedComponents(ARectStr.Top), 0) + StrToIntDef(ARectStr.TopOffset, 0);
  ADestRect.Right := StrToIntDef(EvaluateScanedComponents(ARectStr.Right), 0) + StrToIntDef(ARectStr.RightOffset, 0);
  ADestRect.Bottom := StrToIntDef(EvaluateScanedComponents(ARectStr.Bottom), 0) + StrToIntDef(ARectStr.BottomOffset, 0);
end;


procedure TfrClickerWinInterp.AddCompInfoToListOfScannedComponents(ANodeData: PHighlightedCompRec);
begin
  FListOfScannedComponents.Add('$' + ANodeData^.CompName + '_Left$=' + IntToStr(ANodeData^.LocalX_FromParent));
  FListOfScannedComponents.Add('$' + ANodeData^.CompName + '_Top$=' + IntToStr(ANodeData^.LocalY_FromParent));
  FListOfScannedComponents.Add('$' + ANodeData^.CompName + '_Right$=' + IntToStr(ANodeData^.LocalX_FromParent + ANodeData^.CompRec.ComponentRectangle.Width));
  FListOfScannedComponents.Add('$' + ANodeData^.CompName + '_Bottom$=' + IntToStr(ANodeData^.LocalY_FromParent + ANodeData^.CompRec.ComponentRectangle.Height));
end;


procedure TfrClickerWinInterp.UpdateCompInfoToListOfScannedComponents(ANodeData: PHighlightedCompRec);
begin
  FListOfScannedComponents.Values['$' + ANodeData^.CompName + '_Left$'] := IntToStr(ANodeData^.LocalX_FromParent);
  FListOfScannedComponents.Values['$' + ANodeData^.CompName + '_Top$'] := IntToStr(ANodeData^.LocalY_FromParent);
  FListOfScannedComponents.Values['$' + ANodeData^.CompName + '_Right$'] := IntToStr(ANodeData^.LocalX_FromParent + ANodeData^.CompRec.ComponentRectangle.Width);
  FListOfScannedComponents.Values['$' + ANodeData^.CompName + '_Bottom$'] := IntToStr(ANodeData^.LocalY_FromParent + ANodeData^.CompRec.ComponentRectangle.Height);
end;


procedure TfrClickerWinInterp.UpdateListOfScanedValues;
var
  Node: PVirtualNode;
  NodeData: PHighlightedCompRec;
begin
  FListOfScannedComponents.Clear;

  Node := vstComponents.GetFirst;
  if Node = nil then
    Exit;

  repeat
    NodeData := vstComponents.GetNodeData(Node);
    AddCompInfoToListOfScannedComponents(NodeData);

    Node := vstComponents.GetNext(Node);
  until Node = nil;
end;


procedure TfrClickerWinInterp.HighlightZone(Node: PVirtualNode);
var
  NodeData: PAvoidedZoneRec;
  ZRect: TRect;
begin
  if Node = nil then
    Exit;

  NodeData := vstAvoidedZones.GetNodeData(Node);

  ZoneRectStrToRect(NodeData^.ZRectStr, ZRect);
  FSelectedZoneLeftLimitLabel.Left := ZRect.Left;
  FSelectedZoneTopLimitLabel.Top := ZRect.Top;
  FSelectedZoneRightLimitLabel.Left := ZRect.Right;
  FSelectedZoneBottomLimitLabel.Top := ZRect.Bottom;

  FTransparent_SelectedZoneLeftLimitLabel.Left := FSelectedZoneLeftLimitLabel.Left - 3;
  FTransparent_SelectedZoneTopLimitLabel.Top := FSelectedZoneTopLimitLabel.Top - 3;
  FTransparent_SelectedZoneRightLimitLabel.Left := FSelectedZoneRightLimitLabel.Left - 3;
  FTransparent_SelectedZoneBottomLimitLabel.Top := FSelectedZoneBottomLimitLabel.Top - 3;
end;


procedure TfrClickerWinInterp.SelectTreeNodeByHandle(HW: THandle);
var
  Node: PVirtualNode;
  NodeData: PHighlightedCompRec;
begin
  Node := vstComponents.GetFirst;

  if Node = nil then
    Exit;

  repeat
    NodeData := vstComponents.GetNodeData(Node);

    if NodeData^.CompRec.Handle = HW then
    begin
      vstComponents.Selected[Node] := True;
      vstComponents.FocusedNode := Node;
      vstComponents.ScrollIntoView(Node, True);
      Exit;
    end;

    Node := vstComponents.GetNext(Node);
  until Node = nil;
end;


procedure TfrClickerWinInterp.SelectTreeNodeByImgPoint(X, Y: Integer);
var
  HandleIndex: Integer;
begin
  if Length(FColoredHandles) = 0 then
    Exit;

  HandleIndex := GetIndexOfColoredHandleByHandle(imgHandleColors.Canvas.Pixels[X, Y]);

  if HandleIndex > -1 then
  begin
    vstComponents.ClearSelection;
    SelectTreeNodeByHandle(FColoredHandles[HandleIndex].Handle);
  end;
end;


procedure TfrClickerWinInterp.imgScannedWindowMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
begin
  if ssLeft in Shift then
  begin
    SelectTreeNodeByImgPoint(X, Y);

    Node := vstComponents.GetFirstSelected;
    GenerateContent_AvgScreenshotAndGreenComp(Node);
    HighlightComponent(Node);
  end;
end;


procedure TfrClickerWinInterp.imgScannedWindowMouseEnter(Sender: TObject);
var
  tp: TPoint;
begin
  (Sender as TImage).ShowHint := False;

  if chkShowZoom.Checked then
  begin
    GetCursorPos(tp);
    ShowZoom(tp.X + 50, tp.Y + 50);
  end;
end;


procedure TfrClickerWinInterp.imgScannedWindowMouseLeave(Sender: TObject);
begin
  (Sender as TImage).ShowHint := True;
  HideZoom;
end;


procedure TfrClickerWinInterp.imgScannedWindowMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  pnlMouseCoordsOnScreenshot.Caption := IntToStr(X) + ' : ' + IntToStr(Y);

  if ssLeft in Shift then
  begin
    SelectTreeNodeByImgPoint(X, Y);

    if FOldSelectedNode <> vstComponents.GetFirstSelected then
    begin
      FOldSelectedNode := vstComponents.GetFirstSelected;
      GenerateContent_AvgScreenshotAndGreenComp(FOldSelectedNode);
      HighlightComponent(FOldSelectedNode);
    end;
  end;

  FCurrentMousePosOnPreviewImg.X := X;
  FCurrentMousePosOnPreviewImg.Y := Y;
  tmrDrawZoom.Enabled := True;
end;


procedure TfrClickerWinInterp.imgScannedWindowWithAvoidedZonesMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
  NodeData: PAvoidedZoneRec;
  ZRect: TRect;
begin
  Node := vstAvoidedZones.GetFirst;
  if Node = nil then
    Exit;

  repeat
    NodeData := vstAvoidedZones.GetNodeData(Node);
    if NodeData <> nil then
    begin
      ZoneRectStrToRect(NodeData^.ZRectStr, ZRect);

      if (X >= ZRect.Left) and (X <= ZRect.Right) and
         (Y >= ZRect.Top) and (Y <= ZRect.Bottom) then
      begin
        vstAvoidedZones.Selected[Node] := True;
        HighlightZone(Node);
        DrawAvoidedZones;
        Break;
      end;
    end;

    Node := Node^.NextSibling;
  until Node = nil;
end;


procedure TfrClickerWinInterp.imgScannedWindowWithAvoidedZonesMouseEnter(
  Sender: TObject);
var
  tp: TPoint;
begin
  imgScannedWindowWithAvoidedZones.ShowHint := False;

  if chkShowZoom.Checked then
  begin
    GetCursorPos(tp);
    ShowZoom(tp.X + 50, tp.Y + 50);
  end;
end;


procedure TfrClickerWinInterp.imgScannedWindowWithAvoidedZonesMouseLeave(
  Sender: TObject);
begin
  imgScannedWindowWithAvoidedZones.ShowHint := True;
  HideZoom;
end;


procedure TfrClickerWinInterp.imgScannedWindowWithAvoidedZonesMouseMove(
  Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  pnlMouseCoordsOnScreenshot.Caption := IntToStr(X) + ' : ' + IntToStr(Y);

  FCurrentMousePosOnPreviewImg.X := X;
  FCurrentMousePosOnPreviewImg.Y := Y;
  tmrDrawZoom.Enabled := True;
end;


procedure SetActionToFindControlByComponentData(ACompData: PHighlightedCompRec; ASearchWholeScreen: Boolean; var AAction: TClkActionRec);
begin
  AAction.ActionOptions.Action := acFindControl;
  AAction.ActionOptions.ActionEnabled := True;
  AAction.ActionOptions.ActionTimeout := 1000;
  AAction.ActionOptions.ActionCondition := '';
  AAction.ActionOptions.ActionName := 'Find ' + ACompData^.CompRec.Text + ' (' + ACompData^.CompRec.ClassName + ')';

  AAction.FindControlOptions.AllowToFail := False;
  AAction.FindControlOptions.MatchCriteria.SearchForControlMode := sfcmGenGrid;
  AAction.FindControlOptions.MatchCriteria.WillMatchClassName := True;
  AAction.FindControlOptions.MatchCriteria.WillMatchText := True;

  AAction.FindControlOptions.MatchText := ACompData^.CompRec.Text;
  AAction.FindControlOptions.MatchClassName := ACompData^.CompRec.ClassName;
  AAction.FindControlOptions.MatchTextSeparator := '';
  AAction.FindControlOptions.MatchClassNameSeparator := '';

  AAction.FindControlOptions.UseWholeScreen := ASearchWholeScreen;
  AAction.FindControlOptions.InitialRectangle.Left := '$Control_Left$';
  AAction.FindControlOptions.InitialRectangle.Top := '$Control_Top$';
  AAction.FindControlOptions.InitialRectangle.Right := '$Control_Right$';
  AAction.FindControlOptions.InitialRectangle.Bottom := '$Control_Bottom$';

  if ASearchWholeScreen then
  begin
    AAction.FindControlOptions.InitialRectangle.LeftOffset := '0';
    AAction.FindControlOptions.InitialRectangle.TopOffset := '0';
    AAction.FindControlOptions.InitialRectangle.RightOffset := '0';
    AAction.FindControlOptions.InitialRectangle.BottomOffset := '0';
  end
  else
  begin
    AAction.FindControlOptions.InitialRectangle.LeftOffset := '-10';
    AAction.FindControlOptions.InitialRectangle.TopOffset := '-10';
    AAction.FindControlOptions.InitialRectangle.RightOffset := '10';
    AAction.FindControlOptions.InitialRectangle.BottomOffset := '10';
  end;

  AAction.FindControlOptions.WaitForControlToGoAway := False;
end;


procedure SetActionToClick(ACompData: PHighlightedCompRec; var AAction: TClkActionRec);
begin
  AAction.ActionOptions.Action := acClick;
  AAction.ActionOptions.ActionEnabled := True;
  AAction.ActionOptions.ActionTimeout := 0;
  AAction.ActionOptions.ActionCondition := '';
  AAction.ActionOptions.ActionName := 'Click on ' + ACompData^.CompRec.Text + ' (' + ACompData^.CompRec.ClassName + ')';

  AAction.ClickOptions.XClickPointReference := xrefLeft;
  AAction.ClickOptions.YClickPointReference := yrefTop;
  AAction.ClickOptions.XClickPointVar := '$Control_Left$';
  AAction.ClickOptions.YClickPointVar := '$Control_Top$';
  AAction.ClickOptions.XOffset := '3';
  AAction.ClickOptions.YOffset := '3';
  AAction.ClickOptions.MouseButton := mbLeft;
  AAction.ClickOptions.ClickWithCtrl := False;
  AAction.ClickOptions.ClickWithAlt := False;
  AAction.ClickOptions.ClickWithShift := False;
  AAction.ClickOptions.ClickWithDoubleClick := False;
  AAction.ClickOptions.Count := 1;
  AAction.ClickOptions.LeaveMouse := False;
  AAction.ClickOptions.MoveWithoutClick := False;
  AAction.ClickOptions.ClickType := CClickType_Click;
  AAction.ClickOptions.XClickPointReferenceDest := xrefLeft;
  AAction.ClickOptions.YClickPointReferenceDest := yrefTop;
  AAction.ClickOptions.XClickPointVarDest := '$Control_Left$';;
  AAction.ClickOptions.YClickPointVarDest := '$Control_Top$';
  AAction.ClickOptions.XOffsetDest := '0';
  AAction.ClickOptions.YOffsetDest := '0';
  AAction.ClickOptions.MouseWheelType := mwtVert;
  AAction.ClickOptions.MouseWheelAmount := '1';
  AAction.ClickOptions.DelayAfterMovingToDestination := '50';
  AAction.ClickOptions.DelayAfterMouseDown := '200';
  AAction.ClickOptions.MoveDuration := '-1';
end;


procedure SetActionToCachePosition(ACompData: PHighlightedCompRec; var AAction: TClkActionRec);
begin
  AAction.ActionOptions.Action := acSetVar;
  AAction.ActionOptions.ActionEnabled := True;
  AAction.ActionOptions.ActionTimeout := 0;
  AAction.ActionOptions.ActionCondition := '';
  AAction.ActionOptions.ActionName := 'Cache pos ' + ACompData^.CompRec.Text + ' (' + ACompData^.CompRec.ClassName + ')';

  AAction.SetVarOptions.ListOfVarNames := '$MyComp_Left$' + #13#10 + '$MyComp_Top$' + #13#10 + '$MyComp_Right$' + #13#10 + '$MyComp_Bottom$' + #13#10;
  AAction.SetVarOptions.ListOfVarValues := '$Control_Left$' + #13#10 + '$Control_Top$' + #13#10 + '$Control_Right$' + #13#10 + '$Control_Bottom$' + #13#10;
  AAction.SetVarOptions.ListOfVarEvalBefore := '1' + #13#10 + '1' + #13#10 + '1' + #13#10 + '1' + #13#10;
end;


procedure TfrClickerWinInterp.CopyFindControlActionsToClipBoard(AIncludeAction: TLastFindControlGeneratedAction);
var
  Node, OldNode: PVirtualNode;
  ComponentChain: array of PHighlightedCompRec;
  ActionChain: TClkActionsRecArr;
  i, n: Integer;
  GeneratedActions: TStringList;
  FirstAction: Boolean;
begin
  Node := vstComponents.GetFirstSelected;
  if Node = nil then
  begin
    MessageBox(Handle, 'Please select a component.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  vstComponents.RootNode^.Parent := nil;
  SetLength(ComponentChain, 0);
  repeat
    n := Length(ComponentChain);
    SetLength(ComponentChain, n + 1);
    ComponentChain[n] := vstComponents.GetNodeData(Node);

    OldNode := Node;
    Node := Node^.Parent;
  until (Node = nil) or (vstComponents.GetNodeLevel(OldNode) = 0);

  SetLength(ActionChain, Length(ComponentChain));

  for i := 0 to Length(ComponentChain) - 1 do
  begin
    FirstAction := i = Length(ComponentChain) - 1;
    SetActionToFindControlByComponentData(ComponentChain[i], FirstAction, ActionChain[Length(ComponentChain) - i - 1]);
  end;

  case AIncludeAction of
    lfcNone:
    begin
    end;

    lfcClick:
    begin
      SetLength(ActionChain, Length(ActionChain) + 1);
      SetActionToClick(ComponentChain[0], ActionChain[Length(ActionChain) - 1]);
    end;

    lfcCachePos:
    begin
      SetLength(ActionChain, Length(ActionChain) + 1);
      SetActionToCachePosition(ComponentChain[0], ActionChain[Length(ActionChain) - 1]);
    end;
  end;

  GeneratedActions := TStringList.Create;
  try
    GeneratedActions.LineBreak := #13#10;
    SaveTemplateWithCustomActionsToStringList_V2(GeneratedActions, ActionChain, '', '');
    Clipboard.AsText := GeneratedActions.Text;
  finally
    GeneratedActions.Free;
  end;
end;


procedure TfrClickerWinInterp.MenuItemCopyFindControlActionsToClipBoardClick(
  Sender: TObject);
begin
  CopyFindControlActionsToClipBoard(lfcNone);
end;


procedure TfrClickerWinInterp.MenuItemCopyFindControlAndClickActionsToClipBoardClick
  (Sender: TObject);
begin
  CopyFindControlActionsToClipBoard(lfcClick);
end;


//Returns "found" if the searchpoint is 1px outside a rectangle
function GetIndexOfRect(var ADiffRects: TCompRecArr; ASearchPointX, ASearchPointY: Integer; out ExtDirs: TExtendDirs): Integer;
var
  i: Integer;
begin
  Result := -1;
  ExtDirs := [];

  for i := 0 to Length(ADiffRects) - 1 do
    if (ASearchPointX >= ADiffRects[i].ComponentRectangle.Left - 1) and (ASearchPointX <= ADiffRects[i].ComponentRectangle.Right + 1) and
       (ASearchPointY >= ADiffRects[i].ComponentRectangle.Top - 1) and (ASearchPointY <= ADiffRects[i].ComponentRectangle.Bottom + 1) then
    begin
      Result := i;

      if ASearchPointX = ADiffRects[i].ComponentRectangle.Left - 1 then
        ExtDirs := ExtDirs + [edLeft];

      if ASearchPointX = ADiffRects[i].ComponentRectangle.Right + 1 then
        ExtDirs := ExtDirs + [edRight];

      if ASearchPointY = ADiffRects[i].ComponentRectangle.Top - 1 then
        ExtDirs := ExtDirs + [edTop];

      if ASearchPointY = ADiffRects[i].ComponentRectangle.Bottom + 1 then
        ExtDirs := ExtDirs + [edBottom];

      Exit;
    end;
end;


function TfrClickerWinInterp.PointIsInAvoidedZone(X, Y: Integer): Boolean;
var
  Node: PVirtualNode;
  NodeData: PAvoidedZoneRec;
  ZRect: TRect;
begin
  Result := False;

  Node := vstAvoidedZones.GetFirst;
  if Node = nil then
    Exit;

  repeat
    NodeData := vstAvoidedZones.GetNodeData(Node);
    ZoneRectStrToRect(NodeData^.ZRectStr, ZRect);

    if (X >= ZRect.Left) and (X <= ZRect.Right) and
       (Y >= ZRect.Top) and (Y <= ZRect.Bottom) then
    begin
      Result := True;
      Exit;
    end;

    Node := Node^.NextSibling;
  until Node = nil;
end;


function TfrClickerWinInterp.PointIsInAvoidedZone(X, Y: Integer; var AZones: TTRectArr; var ALastFoundIndex: Integer): Boolean;
var            //ALastFoundIndex is an optimization, which allows verifying the last found zone first, instead of starting all over from 0.
  i: Integer;
begin
  Result := False;

  for i := ALastFoundIndex to Length(AZones) - 1 do
    if (X >= AZones[i].Left) and (X <= AZones[i].Right) and
       (Y >= AZones[i].Top) and (Y <= AZones[i].Bottom) then
    begin
      Result := True;
      ALastFoundIndex := i;
      Exit;
    end;

  for i := 0 to ALastFoundIndex - 1 do
    if (X >= AZones[i].Left) and (X <= AZones[i].Right) and
       (Y >= AZones[i].Top) and (Y <= AZones[i].Bottom) then
    begin
      Result := True;
      ALastFoundIndex := i;
      Exit;
    end;
end;


procedure TfrClickerWinInterp.BuildZonesArray(var AZones: TTRectArr);
var
  Node: PVirtualNode;
  NodeData: PAvoidedZoneRec;
  ZRect: TRect;
begin
  SetLength(AZones, 0);
  Node := vstAvoidedZones.GetFirst;
  if Node = nil then
    Exit;

  repeat
    NodeData := vstAvoidedZones.GetNodeData(Node);
    ZoneRectStrToRect(NodeData^.ZRectStr, ZRect);

    SetLength(AZones, Length(AZones) + 1);
    AZones[Length(AZones) - 1] := ZRect;

    Node := Node^.NextSibling;
  until Node = nil;
end;


procedure TfrClickerWinInterp.RectsToTree(var ADiffRects: TCompRecArr; var ImgMatrix: TColorArr; var ImgHWMatrix: THandleArr);
var
  AllocatedColor: TColor;
  i: Integer;
  YLine: Integer;
begin
  for i := 0 to Length(ADiffRects) - 1 do
  begin
    ADiffRects[i].Handle := i + 200;
    AllocatedColor := AddSubComponentToList(ADiffRects[i].Handle, ADiffRects[i], ADiffRects[i].MouseXOffset, ADiffRects[i].MouseYOffset);

    YLine := ADiffRects[i].MouseYOffset * ADiffRects[i].ComponentRectangle.Width;

    ImgMatrix[YLine + ADiffRects[i].MouseXOffset] := AllocatedColor;
    ImgHWMatrix[YLine + ADiffRects[i].MouseXOffset] := ADiffRects[i].Handle;
  end;
end;


function TfrClickerWinInterp.GetMouseSwipePixelCount(var AZones: TTRectArr; var AScanningArea: TRect): Integer;
var
  x, y: Integer;
  LastFoundIndex: Integer;
  LocalWidth, LocalHeight: Integer;
begin
  Result := 0;
  LastFoundIndex := 0;

  LocalWidth := AScanningArea.Width;
  LocalHeight := AScanningArea.Height;

  for y := 0 to LocalHeight - 1 do
    for x := 0 to LocalWidth - 1 do
      if not PointIsInAvoidedZone(x, y, AZones, LastFoundIndex) then
        Inc(Result);
end;


procedure TfrClickerWinInterp.RecordWithMouseSwipe(AInterprettedHandle: THandle; AStep: Integer = 1);  ////////////// Add a wrapper without Zones: TTRectArr;
var
  InitBmp, CurrentBmp, PrevBmp: TBitmap;
  rct: TRect;
  x, y, w, h: Integer;
  CurrentX, CurrentY, YLine, Step: Integer;
  DiffRects: TCompRecArr;
  IndexOfRect, n: Integer;
  ExtDirs: TExtendDirs;
  AppTitle: string;
  ImgMatrix: TColorArr;
  ImgHWMatrix: array of THandle;
  tk, Duration, SleepTk, MouseCursorToScreenshotDelay: QWord;
  UseHCursor, UseFullScreenshot, BringTargetToFrontPeriodically: Boolean;

  {$IFDEF Windows}
    pci: TCursorInfo;
  {$ELSE}
    //pci: TCursorInfo;
  {$ENDIF}

  Res: LongBool;
  SrcRect, DestRect: TRect;
  FullScreenBmp: TBitmap;
  SelectionLeft, SelectionTop: Integer;
  SelectionRight, SelectionBottom: Integer;
begin
  FDoneRec := False;
  {$IFDEF Windows}
    if GetWindowRect(AInterprettedHandle, rct) = False then
  {$ELSE}
  {$ENDIF}
    Exit;

  SelectionLeft := FSelectedComponentLeftLimitLabel.Left;
  SelectionTop := FSelectedComponentTopLimitLabel.Top;

  SelectionRight := FSelectedComponentRightLimitLabel.Left;
  SelectionBottom := FSelectedComponentBottomLimitLabel.Top;

  SetLength(ImgMatrix, 0);
  SetLength(ImgHWMatrix, 0);

  RecordComponent(AInterprettedHandle, ImgMatrix, ImgHWMatrix, AStep);

  if Length(ImgMatrix) = 0 then
    Exit;

  if FDoneRec then
    Exit;

  //no need to call DoOnClearWinInterp here, because it is called internally by RecordComponent

  tk := GetTickCount64;

  w := rct.Width; //rct.Width and rct.Height are functions, so better use some local vars
  h := rct.Height;

  memCompInfo.Lines.Add('Width: ' + IntToStr(w));
  memCompInfo.Lines.Add('Height: ' + IntToStr(h));

  prbRecording.Max := w * h;

  if prbRecording.Max = 0 then
    prbRecording.Max := 1;

  if chkMinimizeWhileRecording.Checked then
    if AInterprettedHandle <> Handle then
    begin
      //WindowState := wsMinimized;
      Application.Minimize;
    end;

  FRecordWithEdgeExtending := chkRecordWithEdgeExtending.Checked;

  MouseCursorToScreenshotDelay := StrToIntDef(lbeMouseCursorPosToScreenshotDelay.Text, 1);
  MouseCursorToScreenshotDelay := Max(0, Min(MouseCursorToScreenshotDelay, 1000));

  imgScannedWindow.Canvas.Lock;
  imgHandleColors.Canvas.Lock;
  imgScannedWindowWithText.Canvas.Lock;

  vstComponents.BeginUpdate;
  btnStartRec.Enabled := False;
  spdbtnExtraRecording.Enabled := False;
  AppTitle := Application.Title;
  FProgressVertLabel.Visible := True;
  FProgressHorizLabel.Visible := True;
  prbRecordingWithMouseSwipe.Visible := True;
  try
    UseHCursor := chkUseHCursor.Checked;
    UseFullScreenshot := chkFullScr.Checked;
    BringTargetToFrontPeriodically := chkBringTargetToFrontPeriodically.Checked;

    SetCursorPos(Screen.Width, Screen.Height);
    InitBmp := TBitmap.Create;
    CurrentBmp := TBitmap.Create;
    PrevBmp := TBitmap.Create;
    try
      if not UseFullScreenshot then
        ScreenShot(AInterprettedHandle, InitBmp, 0, 0, w, h)
      else
      begin
        WipeBitmap(InitBmp, w, h);
        WipeBitmap(CurrentBmp, w, h);
        WipeBitmap(PrevBmp, w, h);

        SrcRect := rct;

        DestRect.Left := 0;
        DestRect.Top := 0;
        DestRect.Width := w;
        DestRect.Height := h;

        FullScreenBmp := TBitmap.Create;
        try
          ScreenShot(0, FullScreenBmp, 0, 0, Screen.Width, Screen.Height);
          InitBmp.Canvas.CopyRect(DestRect, FullScreenBmp.Canvas, SrcRect);
        finally
          FullScreenBmp.Free;
        end;
      end;

      PrevBmp.Assign(InitBmp);

      Step := AStep;
      x := 0; //must be initialized also here, because of UseHCursor
      y := 0;

      if FRecordSelectedAreaOnly then
      begin
        x := SelectionLeft;
        y := SelectionTop;
      end;

      prbRecordingWithMouseSwipe.Max := h - 1;

      //imgScannedWindowWithAvoidedZones.Canvas.Pen.Color := clRed;  ////////////////////////dbg

      repeat
        Inc(y, Step);
        //FProgressHorizLabel.Top := y;
        prbRecordingWithMouseSwipe.Position := y;

        {$IFDEF Windows}
          if BringTargetToFrontPeriodically then
            BringWindowToTop(AInterprettedHandle);
        {$ELSE}
          //
        {$ENDIF}

        x := 0;
        if FRecordSelectedAreaOnly then
          x := SelectionLeft;
        repeat
          Inc(x, Step);
          //FProgressVertLabel.Left := x;

          if not PointIsInAvoidedZone(x, y) then
          begin
            FProgressHorizLabel.Top := y;
            FProgressVertLabel.Left := x;

            CurrentX := x + rct.Left;
            CurrentY := y + rct.Top;

            SetCursorPos(CurrentX, CurrentY);

            SleepTk := GetTickCount64;
            repeat
              //Sleep(1); // GetTickCount64 seems to be more accurate than Sleep
              Application.ProcessMessages;
            until GetTickCount64 - SleepTk >= MouseCursorToScreenshotDelay;

            if not UseFullScreenshot then
              ScreenShot(AInterprettedHandle, CurrentBmp, 0, 0, w, h)
            else
            begin
              FullScreenBmp := TBitmap.Create;    //full screenshot then crop
              try
                ScreenShot(0, FullScreenBmp, 0, 0, Screen.Width, Screen.Height);
                CurrentBmp.Canvas.CopyRect(DestRect, FullScreenBmp.Canvas, SrcRect);
              finally
                FullScreenBmp.Free;
              end;
            end;

            if UseHCursor then
            begin
              {$IFDEF Windows}
                pci.cbSize := SizeOf(TCursorInfo);
                Res := GetCursorInfo(pci);
              {$ELSE}
                Res := False;
              {$ENDIF}
            end;

            if not BitmapsAreEqual(InitBmp, CurrentBmp, w, h) or not BitmapsAreEqual(PrevBmp, CurrentBmp, w, h) or
              {$IFDEF Windows}
                (UseHCursor and Res and (pci.hCursor <> 65539)) then
              {$ELSE}
                (UseHCursor and Res) then
              {$ENDIF}
            begin
              imgSpinnerDiff.Visible := True;

              IndexOfRect := GetIndexOfRect(DiffRects, CurrentX, CurrentY, ExtDirs);
              if (IndexOfRect = -1) and FRecordWithEdgeExtending or not BitmapsAreEqual(PrevBmp, CurrentBmp, w, h) and not FRecordWithEdgeExtending then
              begin
                n := Length(DiffRects);
                SetLength(DiffRects, n + 1);

                DiffRects[n].ComponentRectangle.Left := CurrentX;
                DiffRects[n].ComponentRectangle.Top := CurrentY;
                DiffRects[n].ComponentRectangle.Width := 1;
                DiffRects[n].ComponentRectangle.Height := 1;

                DiffRects[n].MouseXOffset := x;
                DiffRects[n].MouseYOffset := y;

                //imgScannedWindowWithAvoidedZones.Canvas.Line(x - 2, y, x + 3, y);  //dbg
                //imgScannedWindowWithAvoidedZones.Canvas.Line(x, y - 2, x, y + 3);  //dbg
              end
              else
              begin  //extend the current rectangle
                if edLeft in ExtDirs then
                  DiffRects[IndexOfRect].ComponentRectangle.Left := DiffRects[IndexOfRect].ComponentRectangle.Left - 1;

                if edTop in ExtDirs then
                  DiffRects[IndexOfRect].ComponentRectangle.Top := DiffRects[IndexOfRect].ComponentRectangle.Top - 1;

                if edRight in ExtDirs then
                  DiffRects[IndexOfRect].ComponentRectangle.Right := DiffRects[IndexOfRect].ComponentRectangle.Right + 1;

                if edBottom in ExtDirs then
                  DiffRects[IndexOfRect].ComponentRectangle.Bottom := DiffRects[IndexOfRect].ComponentRectangle.Bottom + 1;
              end;
            end
            else
              imgSpinnerDiff.Visible := False;

            PrevBmp.Assign(CurrentBmp);

            Application.ProcessMessages;
            {$IFDEF Windows}
              if GetAsyncKeyState(VK_ESCAPE) < 0 then
            {$ELSE}
              if GetKeyState(VK_ESCAPE) < 0 then
            {$ENDIF}
            begin
              {$IFDEF Windows}
                if GetAsyncKeyState(VK_SHIFT) < 0 then
              {$ELSE}
                if GetKeyState(VK_SHIFT) < 0 then
              {$ENDIF}
              begin //pause
                imgEnabledPause.Hide;
                imgDisabledPause.Show;

                Sleep(500); //cheap debounce
                repeat
                  Application.ProcessMessages;

                  {$IFDEF Windows}
                    if GetAsyncKeyState(VK_ESCAPE) < 0 then
                  {$ELSE}
                    if GetKeyState(VK_ESCAPE) < 0 then
                  {$ENDIF}
                  begin
                    {$IFDEF Windows}
                      if GetAsyncKeyState(VK_SHIFT) < 0 then
                    {$ELSE}
                      if GetKeyState(VK_SHIFT) < 0 then
                    {$ENDIF}
                    begin
                      imgEnabledPause.Show;
                      imgDisabledPause.Hide;
                      Sleep(500); //cheap debounce
                      Break;
                    end
                    else
                      Exit;
                  end;

                  BringTargetToFrontPeriodically := chkBringTargetToFrontPeriodically.Checked; //update setting during pause

                  if GeneralClosingApp then
                    Exit;
                until False;

                imgEnabledPause.Show;
                imgDisabledPause.Hide;
              end
              else    //simple Esc
                Exit;
            end;


            //if x and $FF = $FF then
            //begin
            //  prbRecording.Position := YLine + x;
            //  lblGauge.Caption := IntToStr(prbRecording.Position * 100 div prbRecording.Max) + ' %';
            //  Application.Title := lblGauge.Caption;
            //end;
          end; //PointIsInAvoidedZone

          if FDoneRec then
            Break;

          if FRecordSelectedAreaOnly then
            if x >= SelectionRight - 1 then
              Break;
        until x >= w - 1;

        if FRecordSelectedAreaOnly then
          if y >= SelectionBottom - 1 then
            Break;

        lblGauge.Caption := IntToStr(prbRecordingWithMouseSwipe.Position * 100 div prbRecordingWithMouseSwipe.Max) + ' %';
        Application.Title := lblGauge.Caption;
      until y >= h - 1;
    finally
      InitBmp.Free;
      CurrentBmp.Free;
      PrevBmp.Free;
    end;

    RectsToTree(DiffRects, ImgMatrix, ImgHWMatrix);
    memCompInfo.Lines.Add('Found ' + IntToStr(Length(DiffRects)) + ' subcontrol(s).');
  finally
    Application.Title := AppTitle;

    for y := 0 to h - 1 do
    begin
      YLine := y * w;

      for x := 0 to w - 1 do
      begin
        imgScannedWindow.Canvas.Pixels[x, y] := ImgMatrix[YLine + x];
        imgHandleColors.Canvas.Pixels[x, y] := ImgHWMatrix[YLine + x];
      end;
    end;

    imgScannedWindowWithText.Picture.Bitmap.Canvas.Draw(0, 0, imgScannedWindow.Picture.Bitmap);
    //imgScannedWindowWithAvoidedZones.Picture.Bitmap.Canvas.Draw(0, 0, imgScannedWindow.Picture.Bitmap);

    SetLength(ImgMatrix, 0);
    SetLength(ImgHWMatrix, 0);

    try
      imgScannedWindow.Canvas.Unlock;
      imgHandleColors.Canvas.Unlock;
      imgScannedWindowWithText.Canvas.Unlock;
    finally
      vstComponents.EndUpdate;
      btnStartRec.Enabled := True;
      spdbtnExtraRecording.Enabled := True;

      SetLength(DiffRects, 0);
      imgSpinnerDiff.Visible := False;

      if chkMinimizeWhileRecording.Checked then
        if AInterprettedHandle <> Handle then
        begin
          //WindowState := wsNormal;
          Application.Restore;
        end;

      FProgressVertLabel.Visible := False;
      FProgressHorizLabel.Visible := False;
      prbRecordingWithMouseSwipe.Visible := False;
    end;
  end;

  Duration := GetTickCount64 - tk;
  memCompInfo.Lines.Add('Recording with mouse swipe duration: ' + IntToStr(Duration) + 'ms   ~= ' + FloatToStr(Duration / 60000) + ' min.');
  lblGauge.Caption := '100%';
  prbRecording.Position := 0;
  UpdateListOfScanedValues;
end;


procedure TfrClickerWinInterp.MenuItemRecordWithMouseSwipeClick(Sender: TObject);
var
  rct, DestRect: TRect;
  EstimatedDuration, AvgScreenshotDuration: Double;
  Step: Integer;
  MouseCursorToScreenshotDelay, ScreenshotDuration: QWord;
  EstimatedDurationStr: string;
  UseFullScreenshot: Boolean;
  CurrentBmp, FullScreenBmp: TBitmap;
  i, MeasurementCount, TotalPixelCount: Integer;
  Zones: TTRectArr;
begin
  if FInterprettedHandle = 0 then
  begin
    pnlDrag.Color := clRed;
    MessageBox(Handle, 'Please set the target control (or window) before recording it.', PChar(Caption), MB_ICONINFORMATION);
    pnlDrag.Color := clYellow;
    Exit;
  end;

  {$IFDEF Windows}
    if GetWindowRect(FInterprettedHandle, rct) = False then
  {$ELSE}
  {$ENDIF}
    Exit;

  Step := Max(1, StrToIntDef(lbeStep.Text, 1)); //bug: If Step is greater than 1, the algorithm detects subcontrol edges on every Step number of pixels. I.e. it does not merge areas.
  MouseCursorToScreenshotDelay := StrToIntDef(lbeMouseCursorPosToScreenshotDelay.Text, 1);
  MouseCursorToScreenshotDelay := Max(0, Min(MouseCursorToScreenshotDelay, 1000));
  UseFullScreenshot := chkFullScr.Checked;

  BuildZonesArray(Zones);
  TotalPixelCount := GetMouseSwipePixelCount(Zones, rct);  //width * height - avoided_pixels

  AvgScreenshotDuration := 0;
  MeasurementCount := 20;  //number of screenshot duration measurements

  for i := 1 to MeasurementCount do
  begin
    ScreenshotDuration := GetTickCount64;   //an approximate measurement of how long does it take for a (component) screenshot
    try
      CurrentBmp := TBitmap.Create;
      try
        DestRect.Left := 0;
        DestRect.Top := 0;
        DestRect.Width := rct.Width;
        DestRect.Height := rct.Height;

        WipeBitmap(CurrentBmp, Screen.Width, Screen.Height);
        GetWindowRect(FInterprettedHandle, rct);
        if not UseFullScreenshot then
          ScreenShot(FInterprettedHandle, CurrentBmp, 0, 0, rct.Width, rct.Height)   //not accurate if using a selected area only
        else
        begin
          FullScreenBmp := TBitmap.Create;    //full screenshot then crop
          try
            ScreenShot(0, FullScreenBmp, 0, 0, Screen.Width, Screen.Height);
            CurrentBmp.Canvas.CopyRect(DestRect, FullScreenBmp.Canvas, rct); //SrcRect is the same as rct
          finally
            FullScreenBmp.Free;
          end;
        end;
      finally
        CurrentBmp.Free;
      end;

      //there is also the duration of comparing two bitmaps, twice
    finally
      ScreenshotDuration := GetTickCount64 - ScreenshotDuration;
    end;

    AvgScreenshotDuration := AvgScreenshotDuration + ScreenshotDuration;
    Application.ProcessMessages;
  end;
  AvgScreenshotDuration := AvgScreenshotDuration / MeasurementCount;

  Inc(ScreenshotDuration);  //add 1ms for other processing stuff

  EstimatedDuration := (TotalPixelCount * (MouseCursorToScreenshotDelay + AvgScreenshotDuration)) / Double(Sqr(Step));  //step is squared, because it is applied to both x and y
  EstimatedDurationStr := 'Minimum estimated duration: ' + FloatToStr(EstimatedDuration) + ' ms  (' + FloatToStr(EstimatedDuration / 60000) + ' min).';

  memCompInfo.Lines.Add(EstimatedDurationStr);

  if MessageBox(Handle,
                PChar('Recording with mouse swipe, will take a very long time, while keeping busy at least a CPU core. The scanning can be stopped with the Esc key.' + #13#10 +
                      'Control size: ' + IntToStr(rct.Width) + ' x ' + IntToStr(rct.Height) + #13#10 +
                      'Valid pixel count: ' + IntToStr(TotalPixelCount) + #13#10 +
                      EstimatedDurationStr + #13#10 +
                      'Continue?'),
                PChar(Application.Title),
                MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  RecordWithMouseSwipe(FInterprettedHandle, Step);

  vstComponents.Repaint;
end;


procedure TfrClickerWinInterp.MenuItem_AddSubcomponentClick(Sender: TObject);
var
  Node: PVirtualNode;
  CompData: PHighlightedCompRec;
begin
  Node := vstComponents.GetFirstSelected;
  if Node = nil then
  begin
    MessageBox(Handle, 'Please record a component, first.',  PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  Node := vstComponents.AddChild(Node);

  CompData := vstComponents.GetNodeData(Node);
  if CompData = nil then
  begin
    MessageBox(Handle, 'Can''t get component info.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  CompData^.AssignedColor := clBlack; //maybe a new color should be assigned
  CompData^.ManuallyAdded := True; //at least, use to allow editing
  CompData^.CompRec.ClassName := 'NewClass';
  CompData^.CompRec.Text := 'NewText';

  CompData^.LocalX := 0;
  CompData^.LocalX_FromParent := 0;
  CompData^.LocalY := 0;
  CompData^.LocalY_FromParent := 0;

  CompData^.CompName := 'Comp0';

  vstComponents.ClearSelection;
  vstComponents.Selected[Node] := True;
  vstComponents.Expanded[Node^.Parent] := True;
  AddCompInfoToListOfScannedComponents(CompData);
end;


procedure TfrClickerWinInterp.MenuItem_ClearScreenshotsClick(Sender: TObject);
begin
  WipeImage(imgScannedWindow, scrboxScannedComponents.Width, scrboxScannedComponents.Height);
  WipeImage(imgScreenshot, scrboxScannedComponents.Width, scrboxScannedComponents.Height);
  WipeImage(imgAvgScreenshotAndGreenComp, scrboxScannedComponents.Width, scrboxScannedComponents.Height);
  WipeImage(imgAvgScreenshotAndAssignedComp, scrboxScannedComponents.Width, scrboxScannedComponents.Height);
  WipeImage(imgLiveScreenshot, scrboxScannedComponents.Width, scrboxScannedComponents.Height);
  WipeImage(imgHandleColors, scrboxScannedComponents.Width, scrboxScannedComponents.Height);
  WipeImage(imgScannedWindowWithText, scrboxScannedComponents.Width, scrboxScannedComponents.Height);
  //WipeImage(imgScannedWindowWithAvoidedZones, scrboxScannedComponents.Width, scrboxScannedComponents.Height);
end;


procedure TfrClickerWinInterp.MenuItem_ClearZonesClick(Sender: TObject);
begin
  vstAvoidedZones.Clear;
  DrawAvoidedZones;
end;


procedure TfrClickerWinInterp.MenuItem_CopyLiveScreenshotToMainScreenshotClick(
  Sender: TObject);
begin
  WipeImage(imgScreenshot, imgLiveScreenshot.Width, imgLiveScreenshot.Height);
  imgScreenshot.Picture.Assign(imgLiveScreenshot.Picture.Bitmap);
end;


procedure TfrClickerWinInterp.MenuItemCopyFindControlAndCachePositionActionsToClipBoardClick
  (Sender: TObject);
begin
  CopyFindControlActionsToClipBoard(lfcCachePos);
end;


procedure TfrClickerWinInterp.CropImageFromScreenshot(ComponentNode: PVirtualNode; CroppedBMP: TBitmap; ComponentOnly: Boolean = True);
var
  CompData: PHighlightedCompRec;
  X, Y: Integer;
begin
  if ComponentOnly then
  begin
    CompData := vstComponents.GetNodeData(ComponentNode);
    if CompData = nil then
    begin
      MessageBox(Handle, 'Can''t get component info.', PChar(Caption), MB_ICONERROR);
      Exit;
    end;

    CroppedBMP.Width := CompData^.CompRec.ComponentRectangle.Width;
    CroppedBMP.Height := CompData^.CompRec.ComponentRectangle.Height;
    X := CompData^.LocalX;
    Y := CompData^.LocalY;
  end
  else
  begin
    CroppedBMP.Width := FSelectedComponentRightLimitLabel.Left - FSelectedComponentLeftLimitLabel.Left;
    CroppedBMP.Height := FSelectedComponentBottomLimitLabel.Top - FSelectedComponentTopLimitLabel.Top;
    X := FSelectedComponentLeftLimitLabel.Left;
    Y := FSelectedComponentTopLimitLabel.Top;
  end;

  CroppedBMP.PixelFormat := pf24bit;
  CroppedBMP.Canvas.Pen.Color := clWhite;
  CroppedBMP.Canvas.Brush.Color := clWhite;
  CroppedBMP.Canvas.Rectangle(0, 0, CroppedBMP.Width - 1, CroppedBMP.Height - 1);

  BitBlt(CroppedBMP.Canvas.Handle, 0, 0, CroppedBMP.Width, CroppedBMP.Height, imgScreenshot.Picture.Bitmap.Canvas.Handle, X, Y, SRCCOPY);
end;


procedure TfrClickerWinInterp.MenuItem_CopySelectionToClipboardClick(
  Sender: TObject);
var
  Node: PVirtualNode;
  CroppedBMP: TBitmap;
begin
  Node := vstComponents.GetFirstSelected;

  if Node = nil then
  begin
    MessageBox(Handle, 'Please select at least one component.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  CroppedBMP := TBitmap.Create;
  try
    CropImageFromScreenshot(Node, CroppedBMP, False);
    Clipboard.Assign(CroppedBMP);
  finally
    CroppedBMP.Free;
  end;
end;


procedure TfrClickerWinInterp.MenuItem_DeleteSubComponentClick(Sender: TObject);
var
  Node, PrevNode: PVirtualNode;
begin
  Node := vstComponents.GetFirstSelected;

  if Node = nil then
  begin
    MessageBox(Handle, 'Please select at least one component.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  if MessageBox(Handle, 'Are you sure you want to delete the selected component(s) (and its/their subcomponents)?', PChar(Caption), MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  vstComponents.RootNode^.PrevSibling := nil; //make sure the loop stops
  Node := vstComponents.GetLast;
  repeat
    PrevNode := vstComponents.GetPrevious(Node);

    if vstComponents.Selected[Node] then
      vstComponents.DeleteNode(Node);

    Node := PrevNode;
  until Node = nil;

  UpdateListOfScanedValues;
end;


procedure TfrClickerWinInterp.MenuItem_RecordMultipleSizesClick(Sender: TObject);
var
  ImgMatrix: TColorArr;
  ImgHWMatrix: THandleArr;
  i: Integer;
  Flags: DWord;
  MainCompRec: TCompRec;
  TempSaveDialog: TSaveDialog;
  RecResult: Boolean;
  s: string;
  MainFileName: string;
begin
  MessageBox(Handle,
             PChar('The target window sizes (width/height) are not configurable for now. They will be incresed by 30px for each recording.' + #13#10 +
                   'There will be three recordings, starting with the current target window size.' + #13#10 +
                   'Make sure the window can be resized to at least 60px on both width and height, i.e. the window is not maximized.' + #13#10 +
                   'Window Interpreter will attempt to resize the window automatically, after each recording.' + #13#10#13#10 +
                   'A save dialog will open, to let you browse the location of where the .tree files will be saved.'),
             PChar(Application.Title),
             MB_ICONINFORMATION);

  if FInterprettedHandle = 0 then
  begin
    pnlDrag.Color := clRed;
    MessageBox(Handle, 'Please set the target control (or window) before recording it.', PChar(Caption), MB_ICONINFORMATION);
    pnlDrag.Color := clYellow;
    Exit;
  end;

  TempSaveDialog := TSaveDialog.Create(nil);
  try
    TempSaveDialog.Filter := 'Tree files (*.tree)|*.tree|All files (*.*)|*.*';
    if not TempSaveDialog.Execute then
      Exit;

    if ExtractFileExt(TempSaveDialog.FileName) = '' then
      TempSaveDialog.FileName := TempSaveDialog.FileName + '.tree';

    MainFileName := TempSaveDialog.FileName;
    MainFileName := Copy(MainFileName, 1, Length(MainFileName) - 5);

    s := '';

    Flags := SWP_ASYNCWINDOWPOS or SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOZORDER;
    Flags := Flags or SWP_NOMOVE;

    for i := 0 to 2 do
    begin
      if i = 0 then
        MainCompRec := GetWindowClassRec(FInterprettedHandle);

      try
        RecordComponent(FInterprettedHandle, ImgMatrix, ImgHWMatrix);
      finally
        SetLength(ImgMatrix, 0);
        SetLength(ImgHWMatrix, 0);
      end;

      RecResult := SetWindowPos( FInterprettedHandle,
                                 HWND_TOP,
                                 MainCompRec.ComponentRectangle.Left,
                                 MainCompRec.ComponentRectangle.Top,
                                 MainCompRec.ComponentRectangle.Width + (i + 1) * 30,
                                 MainCompRec.ComponentRectangle.Height + (i + 1) * 30,
                                 Flags);

      if not RecResult then
        {$IFDEF Windows}
          s := s + 'Resizing error: ' + SysErrorMessage(GetLastError) + #13#10;
        {$ELSE}
          s := s + 'Resizing error: ' + 'Not implemented.' + #13#10;
        {$ENDIF}

      try
        vstComponents.SaveToFile(MainFileName + '_' + IntToStr(i) + '.tree');
        SaveImages(MainFileName + '_' + IntToStr(i) + '.tree');
      except
        on E: Exception do
          s := s + 'Recording exception: '  + E.Message;
      end;
    end; //for

    //Restore
    RecResult := SetWindowPos( FInterprettedHandle,
                               HWND_TOP,
                               MainCompRec.ComponentRectangle.Left,
                               MainCompRec.ComponentRectangle.Top,
                               MainCompRec.ComponentRectangle.Width,
                               MainCompRec.ComponentRectangle.Height,
                               Flags);

    if not RecResult then
      {$IFDEF Windows}
        s := s + 'Resizing error: ' + SysErrorMessage(GetLastError) + #13#10;
      {$ELSE}
        s := s + 'Resizing error: ' + 'Not implemented.' + #13#10;
      {$ENDIF}

    if s <> '' then
      s := #13#10 + s;

    MessageBox(Handle, PChar('Done recording.' + s), PChar(Application.Title), MB_ICONINFORMATION);
  finally
    TempSaveDialog.Free;
  end;
end;


procedure TfrClickerWinInterp.MenuItem_SaveSelectionToFileClick(Sender: TObject);
var
  Node: PVirtualNode;
  CroppedBMP: TBitmap;
  SaveDialog: TSaveDialog;
begin
  Node := vstComponents.GetFirstSelected;

  if Node = nil then
  begin
    MessageBox(Handle, 'Please select at least one component.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Filter := 'Bitmap files (*.bmp)|*.bmp|All files (*.*)|*.*';

    if not SaveDialog.Execute then
      Exit;

    if UpperCase(ExtractFileExt(SaveDialog.FileName)) <> '.BMP' then
      SaveDialog.FileName := SaveDialog.FileName + '.bmp';

    CroppedBMP := TBitmap.Create;
    try
      CropImageFromScreenshot(Node, CroppedBMP, False);
      CroppedBMP.SaveToFile(SaveDialog.FileName);
    finally
      CroppedBMP.Free;
    end;
  finally
    SaveDialog.Free;
  end;
end;


procedure TfrClickerWinInterp.UpdateTreeValuesFromSelection;
var
  Node, NextSibling: PVirtualNode;
  NodeData: PHighlightedCompRec;
  DiffX, DiffY, DiffR, DiffB: Integer;
begin
  if vstComponents.RootNodeCount = 0 then
    Exit;

  Node := vstComponents.GetFirstSelected;
  if Node = nil then
  begin
    MessageBox(Handle, 'Please select a component in tree before editing.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  NodeData := vstComponents.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  DiffX := FSelectedComponentLeftLimitLabel.Left - NodeData^.LocalX;
  DiffY := FSelectedComponentTopLimitLabel.Top - NodeData^.LocalY;
  DiffR := FSelectedComponentRightLimitLabel.Left - (NodeData^.LocalX + NodeData^.CompRec.ComponentRectangle.Width);
  DiffB := FSelectedComponentBottomLimitLabel.Top - (NodeData^.LocalY + NodeData^.CompRec.ComponentRectangle.Height);

  NodeData^.LocalX := FSelectedComponentLeftLimitLabel.Left;
  NodeData^.LocalY := FSelectedComponentTopLimitLabel.Top;

  Inc(NodeData^.LocalX_FromParent, DiffX);
  Inc(NodeData^.LocalY_FromParent, DiffY);

  Inc(NodeData^.CompRec.ComponentRectangle.Left, DiffX);
  Inc(NodeData^.CompRec.ComponentRectangle.Top, DiffY);
  Inc(NodeData^.CompRec.ComponentRectangle.Right, DiffR);
  Inc(NodeData^.CompRec.ComponentRectangle.Bottom, DiffB);

  GenerateContent_AvgScreenshotAndGreenComp(Node);
  vstComponents.RepaintNode(Node);

  UpdateCompInfoToListOfScannedComponents(NodeData);

  NextSibling := Node^.NextSibling; // vstComponents.GetNextSibling(Node); //this allows getting all the subnodes    GetNextSibling returns Node, if there is no next sibling (only childnodes)
  repeat
    Node := vstComponents.GetNext(Node);
    if (Node = NextSibling) or (Node = nil) then
      Break;

    NodeData := vstComponents.GetNodeData(Node);
    if NodeData <> nil then                        //update all the subnodes
    begin
      Inc(NodeData^.LocalX, DiffX);
      Inc(NodeData^.LocalY, DiffY);

      Inc(NodeData^.CompRec.ComponentRectangle.Left, DiffX);
      Inc(NodeData^.CompRec.ComponentRectangle.Top, DiffY);
      Inc(NodeData^.CompRec.ComponentRectangle.Right, DiffX);
      Inc(NodeData^.CompRec.ComponentRectangle.Bottom, DiffY);

      UpdateCompInfoToListOfScannedComponents(NodeData);
    end;
  until False;
end;


procedure TfrClickerWinInterp.MenuItem_UpdateTreeValuesFromSelectionClick(
  Sender: TObject);
begin
  UpdateTreeValuesFromSelection;
end;


procedure TfrClickerWinInterp.MenuItem_UpdateTreeValuesFromSelectionToANewComponentClick
  (Sender: TObject);
var
  Node: PVirtualNode;
  CompData: PHighlightedCompRec;
begin
  if vstComponents.RootNodeCount = 0 then
  begin
    Node := vstComponents.InsertNode(vstComponents.RootNode, amInsertAfter);
    vstComponents.Selected[Node] := True;
  end;

  Node := vstComponents.GetFirstSelected;
  if Node = nil then
  begin
    Node := vstComponents.GetFirst;
    vstComponents.Selected[Node] := True;
  end;

  Node := vstComponents.InsertNode(Node, amAddChildLast);

  CompData := vstComponents.GetNodeData(Node);
  if CompData = nil then
  begin
    MessageBox(Handle, 'Can''t get component info.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  CompData^.AssignedColor := clBlack; //maybe a new color should be assigned
  CompData^.ManuallyAdded := True; //at least, use to allow editing
  CompData^.CompRec.ClassName := 'NewClass';
  CompData^.CompRec.Text := 'NewText';

  vstComponents.Selected[Node] := True;
  vstComponents.Expanded[Node^.Parent] := True;

  UpdateTreeValuesFromSelection;
end;


procedure TfrClickerWinInterp.PageControlWinInterpChange(Sender: TObject);
begin
  FSelectedComponentLeftLimitLabel.Visible := PageControlWinInterp.ActivePageIndex = 0;
  FSelectedComponentTopLimitLabel.Visible := PageControlWinInterp.ActivePageIndex = 0;
  FSelectedComponentRightLimitLabel.Visible := PageControlWinInterp.ActivePageIndex = 0;
  FSelectedComponentBottomLimitLabel.Visible := PageControlWinInterp.ActivePageIndex = 0;

  FTransparent_SelectedComponentLeftLimitLabel.Visible := PageControlWinInterp.ActivePageIndex = 0;
  FTransparent_SelectedComponentTopLimitLabel.Visible := PageControlWinInterp.ActivePageIndex = 0;
  FTransparent_SelectedComponentRightLimitLabel.Visible := PageControlWinInterp.ActivePageIndex = 0;
  FTransparent_SelectedComponentBottomLimitLabel.Visible := PageControlWinInterp.ActivePageIndex = 0;

  FSelectedZoneLeftLimitLabel.Visible := PageControlWinInterp.ActivePageIndex = 1;
  FSelectedZoneTopLimitLabel.Visible := PageControlWinInterp.ActivePageIndex = 1;
  FSelectedZoneRightLimitLabel.Visible := PageControlWinInterp.ActivePageIndex = 1;
  FSelectedZoneBottomLimitLabel.Visible := PageControlWinInterp.ActivePageIndex = 1;

  FTransparent_SelectedZoneLeftLimitLabel.Visible := PageControlWinInterp.ActivePageIndex = 1;
  FTransparent_SelectedZoneTopLimitLabel.Visible := PageControlWinInterp.ActivePageIndex = 1;
  FTransparent_SelectedZoneRightLimitLabel.Visible := PageControlWinInterp.ActivePageIndex = 1;
  FTransparent_SelectedZoneBottomLimitLabel.Visible := PageControlWinInterp.ActivePageIndex = 1;

  imgScannedWindowWithAvoidedZones.Visible := PageControlWinInterp.ActivePageIndex = 1;
end;


procedure TfrClickerWinInterp.MenuItem_CopySelectedComponentToClipboardClick(
  Sender: TObject);
var
  Node: PVirtualNode;
  CroppedBMP: TBitmap;
begin
  Node := vstComponents.GetFirstSelected;

  if Node = nil then
  begin
    MessageBox(Handle, 'Please select at least one component.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  CroppedBMP := TBitmap.Create;
  try
    CropImageFromScreenshot(Node, CroppedBMP);
    Clipboard.Assign(CroppedBMP);
  finally
    CroppedBMP.Free;
  end;
end;


procedure TfrClickerWinInterp.MenuItem_SaveSelectedComponentToFileClick(
  Sender: TObject);
var
  Node: PVirtualNode;
  CroppedBMP: TBitmap;
  SaveDialog: TSaveDialog;
begin
  Node := vstComponents.GetFirstSelected;

  if Node = nil then
  begin
    MessageBox(Handle, 'Please select at least one component.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Filter := 'Bitmap files (*.bmp)|*.bmp|All files (*.*)|*.*';

    if not SaveDialog.Execute then
      Exit;

    if UpperCase(ExtractFileExt(SaveDialog.FileName)) <> '.BMP' then
      SaveDialog.FileName := SaveDialog.FileName + '.bmp';

    CroppedBMP := TBitmap.Create;
    try
      CropImageFromScreenshot(Node, CroppedBMP);
      CroppedBMP.SaveToFile(SaveDialog.FileName);
    finally
      CroppedBMP.Free;
    end;
  finally
    SaveDialog.Free;
  end;
end;


function TfrClickerWinInterp.GetParentNodeByRectangle(AComp: THighlightedCompRec): PVirtualNode;
var
  ParentData: PHighlightedCompRec;
  Node: PVirtualNode;
begin
  Result := nil;
  Node := vstComponents.GetFirst;

  if Node = nil then
  begin
    Result := Node;
    Exit;
  end
  else
  begin
    repeat
      ParentData := vstComponents.GetNodeData(Node);

      if (AComp.CompRec.ComponentRectangle.Left >= ParentData^.CompRec.ComponentRectangle.Left) and
         (AComp.CompRec.ComponentRectangle.Right <= ParentData^.CompRec.ComponentRectangle.Right) and
         (AComp.CompRec.ComponentRectangle.Top >= ParentData^.CompRec.ComponentRectangle.Top) and
         (AComp.CompRec.ComponentRectangle.Bottom <= ParentData^.CompRec.ComponentRectangle.Bottom) and
         (AComp.CompRec.ComponentRectangle <> ParentData^.CompRec.ComponentRectangle) then
      begin
        Result := Node;
        //Exit;  //do not exit, because there might be another parent, smaller in size
      end;

      Node := vstComponents.GetNext(Node);
    until Node = nil;
  end;
end;


procedure TfrClickerWinInterp.InsertTreeComponent(AParentNode: PVirtualNode; AComp: THighlightedCompRec);
var
  NewData, ParentData: PHighlightedCompRec;
  Node: PVirtualNode;
begin
  if AParentNode = nil then
  begin
    try
      Node := vstComponents.InsertNode(vstComponents.RootNode, amAddChildLast)
    except  //There is some internal race condition in VirtualTreeView, which might trigger.
      vstComponents.Clear;
      Application.ProcessMessages;
      Sleep(100);

      Node := vstComponents.InsertNode(vstComponents.RootNode, amAddChildLast); //maybe it works this time
    end;
  end
  else
  begin
    Node := vstComponents.InsertNode(AParentNode, amAddChildLast);
    vstComponents.FullExpand(AParentNode);
  end;

  NewData := vstComponents.GetNodeData(Node);
  if NewData = nil then
  begin
    memCompInfo.Lines.Add('Can''t get node data.');
    Exit;
  end;

  NewData^ := AComp;

  if AParentNode = nil then
    NewData^.ParentCtrl := nil
  else
  begin
    ParentData := vstComponents.GetNodeData(AParentNode);
    if ParentData <> nil then
    begin
      NewData^.ParentCtrl := ParentData^.Ctrl;

      NewData^.LocalX_FromParent := NewData^.CompRec.ComponentRectangle.Left - ParentData^.CompRec.ComponentRectangle.Left;
      NewData^.LocalY_FromParent := NewData^.CompRec.ComponentRectangle.Top - ParentData^.CompRec.ComponentRectangle.Top;
    end
    else
      NewData^.ParentCtrl := nil; //not sure about this corner case
  end;

  DoOnInsertTreeComponent(NewData);  //this is used to update an external structure (e.g. DrawingBoard)
end;


procedure TfrClickerWinInterp.AddTreeComponent(AComp: THighlightedCompRec);
var
  ParentNode: PVirtualNode;
begin
  ParentNode := GetParentNodeByRectangle(AComp);
  InsertTreeComponent(ParentNode, AComp);
end;


function TfrClickerWinInterp.GetIndexOfColoredHandleByHandle(HW: THandle): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(FColoredHandles) - 1 do
    if FColoredHandles[i].Handle = HW then
    begin
      Result := i;
      Exit;
    end;
end;


function TfrClickerWinInterp.GetIndexOfColoredHandleByColor(AColor: TColor): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(FColoredHandles) - 1 do
    if FColoredHandles[i].Color = AColor then
    begin
      Result := i;
      Exit;
    end;
end;


procedure TfrClickerWinInterp.AddColoredHandle(HW: THandle; AColor: TColor);
var
  n: Integer;
begin
  n := Length(FColoredHandles);
  SetLength(FColoredHandles, n + 1);
  FColoredHandles[n].Handle := HW;
  FColoredHandles[n].Color := AColor;
end;


function TfrClickerWinInterp.AddComponentToList(HW: THandle; ALocalX, ALocalY: Integer): TColor;
var
  HighlightedCompRec: THighlightedCompRec;
  HandleIndex: Integer;
  Found: Boolean;
begin
  HandleIndex := GetIndexOfColoredHandleByHandle(HW);
  Found := HandleIndex > -1;

  if not Found then
  begin
    HandleIndex := Length(FColoredHandles); //without -1

    Result := clrs2[(HandleIndex + 1) mod Length(clrs2)];  //colors are reused, leading to selection bugs if selection relies on color @ [x, y]
    AddColoredHandle(HW, Result);  //this adds the handle and color (i.e. Result) to FColoredHandles

    HighlightedCompRec.CompRec := GetWindowClassRec(HW);
    HighlightedCompRec.CompRec.IsSubControl := False;
    HighlightedCompRec.AssignedColor := Result;
    HighlightedCompRec.LocalX := ALocalX;
    HighlightedCompRec.LocalY := ALocalY;
    HighlightedCompRec.ManuallyAdded := False;
    HighlightedCompRec.Ctrl := nil; //init here
    HighlightedCompRec.ParentCtrl := nil; //init here
    HighlightedCompRec.LocalX_FromParent := ALocalX;  //init here, although these will have to be updated later by AddTreeComponent, when the values are available from parent
    HighlightedCompRec.LocalY_FromParent := ALocalY;  //init here, although these will have to be updated later by AddTreeComponent, when the values are available from parent
    HighlightedCompRec.CGClassName := '';
    HighlightedCompRec.CompName := 'Comp' + IntToStr(HW);

    try
      AddTreeComponent(HighlightedCompRec);
    except
      //there is a bug in VirtualTreeView, which causes a bad AV when inserting a node.
      //See "if (vsExpanded in Destination.Parent.States) and IsEffectivelyVisible(Node) then"  in VirtualTrees.pas.  Destination.Parent is nil.
      //The tree becomes empty (by calling Clear), but the RootNode is not properly initialized.
    end;
  end
  else
    Result := clrs2[(HandleIndex + 1) mod Length(clrs2)];
end;


function TfrClickerWinInterp.AddSubComponentToList(HW: THandle; ARect: TCompRec; ALocalX, ALocalY: Integer): TColor;
var
  HighlightedCompRec: THighlightedCompRec;
  HandleIndex: Integer;
  Found: Boolean;
begin
  HandleIndex := GetIndexOfColoredHandleByHandle(HW);
  Found := HandleIndex > -1;

  if not Found then
  begin
    HandleIndex := Length(FColoredHandles); //without -1

    Result := clrs2[(HandleIndex + 1) mod Length(clrs2)];  //colors are reused, leading to selection bugs if selection relies on color @ [x, y]
    AddColoredHandle(HW, Result);

    HighlightedCompRec.CompRec := ARect;
    HighlightedCompRec.CompRec.IsSubControl := True;

    HighlightedCompRec.AssignedColor := Result;
    HighlightedCompRec.LocalX := ALocalX;
    HighlightedCompRec.LocalY := ALocalY;

    HighlightedCompRec.ManuallyAdded := False;

    try
      AddTreeComponent(HighlightedCompRec);
    except
      //there is a bug in VirtualTreeView, which causes a bad AV when inserting a node.
      //See "if (vsExpanded in Destination.Parent.States) and IsEffectivelyVisible(Node) then"  in VirtualTrees.pas.  Destination.Parent is nil.
      //The tree becomes empty (by calling Clear), but the RootNode is not properly initialized.
    end;
  end
  else
    Result := clrs2[(HandleIndex + 1) mod Length(clrs2)];
end;


procedure TfrClickerWinInterp.PrepareLayers(ARect: TRect);
begin
  imgScannedWindow.Picture.Clear;
  imgScannedWindow.Left := 0;
  imgScannedWindow.Top := 0;
  imgScannedWindow.Width := ARect.Right - ARect.Left;
  imgScannedWindow.Height := ARect.Bottom - ARect.Top;
  imgScannedWindow.Picture.Bitmap.Width := imgScannedWindow.Width;
  imgScannedWindow.Picture.Bitmap.Height := imgScannedWindow.Height;
  imgScannedWindow.Canvas.Pen.Color := clBlack;
  imgScannedWindow.Canvas.Brush.Color := clBlack;
  imgScannedWindow.Canvas.Rectangle(0, 0, imgScannedWindow.Width - 1, imgScannedWindow.Height - 1);
  imgScannedWindow.Repaint;

  imgScreenshot.Picture.Clear;
  imgScreenshot.Left := 0;
  imgScreenshot.Top := 0;
  imgScreenshot.Width := imgScannedWindow.Width;
  imgScreenshot.Height := imgScannedWindow.Height;
  imgScreenshot.Picture.Bitmap.Width := imgScannedWindow.Width;
  imgScreenshot.Picture.Bitmap.Height := imgScannedWindow.Height;
  imgScreenshot.Canvas.Pen.Color := clBlack;
  imgScreenshot.Canvas.Brush.Color := clBlack;
  imgScreenshot.Canvas.Rectangle(0, 0, imgScreenshot.Width - 1, imgScreenshot.Height - 1);

  imgAvgScreenshotAndGreenComp.Picture.Clear;
  imgAvgScreenshotAndGreenComp.Left := 0;
  imgAvgScreenshotAndGreenComp.Top := 0;
  imgAvgScreenshotAndGreenComp.Width := imgScannedWindow.Width;
  imgAvgScreenshotAndGreenComp.Height := imgScannedWindow.Height;
  imgAvgScreenshotAndGreenComp.Picture.Bitmap.Width := imgScannedWindow.Width;
  imgAvgScreenshotAndGreenComp.Picture.Bitmap.Height := imgScannedWindow.Height;
  imgAvgScreenshotAndGreenComp.Canvas.Pen.Color := clBlack;
  imgAvgScreenshotAndGreenComp.Canvas.Brush.Color := clBlack;
  imgAvgScreenshotAndGreenComp.Canvas.Rectangle(0, 0, imgScreenshot.Width - 1, imgScreenshot.Height - 1);

  imgAvgScreenshotAndAssignedComp.Picture.Clear;
  imgAvgScreenshotAndAssignedComp.Left := 0;
  imgAvgScreenshotAndAssignedComp.Top := 0;
  imgAvgScreenshotAndAssignedComp.Width := imgScannedWindow.Width;
  imgAvgScreenshotAndAssignedComp.Height := imgScannedWindow.Height;
  imgAvgScreenshotAndAssignedComp.Picture.Bitmap.Width := imgScannedWindow.Width;
  imgAvgScreenshotAndAssignedComp.Picture.Bitmap.Height := imgScannedWindow.Height;
  imgAvgScreenshotAndAssignedComp.Canvas.Pen.Color := clBlack;
  imgAvgScreenshotAndAssignedComp.Canvas.Brush.Color := clBlack;
  imgAvgScreenshotAndAssignedComp.Canvas.Rectangle(0, 0, imgScreenshot.Width - 1, imgScreenshot.Height - 1);

  imgHandleColors.Picture.Clear;
  imgHandleColors.Left := 0;
  imgHandleColors.Top := 0;
  imgHandleColors.Width := imgScannedWindow.Width;
  imgHandleColors.Height := imgScannedWindow.Height;
  imgHandleColors.Picture.Bitmap.Width := imgScannedWindow.Width;
  imgHandleColors.Picture.Bitmap.Height := imgScannedWindow.Height;
  imgHandleColors.Canvas.Pen.Color := clBlack;
  imgHandleColors.Canvas.Brush.Color := clBlack;
  imgHandleColors.Canvas.Rectangle(0, 0, imgScreenshot.Width - 1, imgScreenshot.Height - 1);

  imgScannedWindowWithText.Picture.Clear;
  imgScannedWindowWithText.Left := 0;
  imgScannedWindowWithText.Top := 0;
  imgScannedWindowWithText.Width := imgScannedWindowWithText.Width;
  imgScannedWindowWithText.Height := imgScannedWindowWithText.Height;
  imgScannedWindowWithText.Picture.Bitmap.Width := imgScannedWindowWithText.Width;
  imgScannedWindowWithText.Picture.Bitmap.Height := imgScannedWindowWithText.Height;
  imgScannedWindowWithText.Canvas.Pen.Color := clBlack;
  imgScannedWindowWithText.Canvas.Brush.Color := clBlack;
  imgScannedWindowWithText.Canvas.Rectangle(0, 0, imgScreenshot.Width - 1, imgScreenshot.Height - 1);

  //imgScannedWindowWithAvoidedZones.Picture.Clear;
  //imgScannedWindowWithAvoidedZones.Left := 0;
  //imgScannedWindowWithAvoidedZones.Top := 0;
  //imgScannedWindowWithAvoidedZones.Width := imgScannedWindowWithText.Width;
  //imgScannedWindowWithAvoidedZones.Height := imgScannedWindowWithText.Height;
  //imgScannedWindowWithAvoidedZones.Picture.Bitmap.Width := imgScannedWindowWithText.Width;
  //imgScannedWindowWithAvoidedZones.Picture.Bitmap.Height := imgScannedWindowWithText.Height;
  //imgScannedWindowWithAvoidedZones.Canvas.Pen.Color := clBlack;
  //imgScannedWindowWithAvoidedZones.Canvas.Brush.Color := clBlack;
  //imgScannedWindowWithAvoidedZones.Canvas.Rectangle(0, 0, imgScreenshot.Width - 1, imgScreenshot.Height - 1);
end;


procedure TfrClickerWinInterp.GenerateContent_AvgScreenshotAndGreenComp(Node: PVirtualNode);
var
  {$IFDEF UNIX}
    i, j: Integer;
    Color1: TColor;
  {$ENDIF}
  NodeData: PHighlightedCompRec;
  LocX, LocY: Integer;
  CompWidth, CompHeight: Integer;
begin
  if toMultiSelect in vstComponents.TreeOptions.SelectionOptions then
    Node := vstComponents.GetFirstSelected; //override param, since multiple nodes can be selected

  if Node = nil then
    Exit;

  //reset with screenshot
  imgAvgScreenshotAndGreenComp.Canvas.Draw(0, 0, imgScreenshot.Picture.Bitmap);

  repeat
    if vstComponents.Selected[Node] then
    begin
      NodeData := vstComponents.GetNodeData(Node);
      if NodeData = nil then
        Exit;

      LocX := NodeData^.LocalX;
      LocY := NodeData^.LocalY;
      CompWidth := NodeData^.CompRec.ComponentRectangle.Width;
      CompHeight := NodeData^.CompRec.ComponentRectangle.Height;

      //highlight with green

      {$IFDEF UNIX}  //using the slower method, because of ScanLine
        for i := LocY to CompHeight + LocY - 1 do
          for j := LocX to CompWidth + LocX - 1 do
          begin
            Color1 := imgAvgScreenshotAndGreenComp.Canvas.Pixels[j, i];
            imgAvgScreenshotAndGreenComp.Canvas.Pixels[j, i] := AvgTwoTrueColors(Color1, clLime);
          end;
      {$ELSE}
        AvgBitmapWithColor(imgScreenshot.Picture.Bitmap, imgAvgScreenshotAndGreenComp.Picture.Bitmap, clLime, LocX, LocY, CompWidth, CompHeight);
      {$ENDIF}
    end;

    Node := vstComponents.GetNext(Node);
  until Node = nil;
end;


procedure TfrClickerWinInterp.GenerateContent_AvgScreenshotAndGenComp;
{$IFDEF UNIX}
  var
    i, j: Integer;
    Color1, Color2: TColor;
{$ENDIF}
begin
  {$IFDEF UNIX}
  for i := 0 to imgScannedWindow.Height - 1 do
    for j := 0 to imgScannedWindow.Width - 1 do
    begin
      Color1 := imgScannedWindow.Canvas.Pixels[j, i];
      Color2 := imgScreenshot.Canvas.Pixels[j, i];
      imgAvgScreenshotAndAssignedComp.Canvas.Pixels[j, i] := AvgTwoTrueColors(Color1, Color2);
    end;
  {$ELSE}
    AvgBitmapWithBitmap(imgScreenshot.Picture.Bitmap, imgScannedWindow.Picture.Bitmap, imgAvgScreenshotAndAssignedComp.Picture.Bitmap, 0, 0, imgScreenshot.Width, imgScreenshot.Height);
  {$ENDIF}
end;


procedure TfrClickerWinInterp.RecordComponent(AInterprettedHandle: THandle; var ImgMatrix: TColorArr; var ImgHWMatrix: THandleArr; AStep: Integer = 1);
var
  x, y, Step, YLine: Integer;
  tp: TPoint;
  rct: TRect;
  HW{, InitialHW}: THandle;
  AllocatedColor: TColor;
  RectWidth, RectHeight: Integer;
  tk: QWord;
  AppTitle: string;
  Node: PVirtualNode;
  NodeData: PHighlightedCompRec;
  DestRect: TRect;
  FullScreenBmp: TBitmap;
begin
  if chkMinimizeWhileRecording.Checked then
    if AInterprettedHandle <> Handle then
    begin
      //WindowState := wsMinimized;
      Application.Minimize;
    end;

  if chkBringTargetToFront.Checked then
    {$IFDEF Windows}
      BringWindowToTop(AInterprettedHandle);
    {$ELSE}
      begin
      end;
    {$ENDIF}

  tk := GetTickCount64;

  FDoneRec := False;
  {$IFDEF Windows}
    if GetWindowRect(AInterprettedHandle, rct) = False then
  {$ELSE}
  {$ENDIF}
    Exit;

  DoOnClearWinInterp;

  vstComponents.BeginUpdate;
  try
    SetLength(FColoredHandles, 0);
    vstComponents.Clear;
  finally
    vstComponents.EndUpdate;
  end;

  PrepareLayers(rct);

  RectWidth := rct.Width;  //rct.Width and rct.Height are functions, so better use some local vars
  RectHeight := rct.Height;

  memCompInfo.Lines.Add('Width: ' + IntToStr(RectWidth));
  memCompInfo.Lines.Add('Height: ' + IntToStr(RectHeight));

  tp.X := rct.Left;
  tp.Y := rct.Top;
  //InitialHW := WindowFromPoint(tp);

  prbRecording.Max := RectWidth * RectHeight;

  if prbRecording.Max = 0 then
    prbRecording.Max := 1;

  if AStep = 1 then
    Step := AStep
  else
    Step := Max(1, StrToIntDef(lbeStep.Text, 1));

  if Step = 1 then
  begin
    SetLength(ImgMatrix, prbRecording.Max);    //these arrays are used only when Step = 1
    SetLength(ImgHWMatrix, prbRecording.Max);
  end;

  imgScannedWindow.Canvas.Lock;
  imgHandleColors.Canvas.Lock;
  imgScannedWindowWithText.Canvas.Lock;
  imgScannedWindowWithAvoidedZones.Canvas.Lock;

  vstComponents.BeginUpdate;
  btnStartRec.Enabled := False;
  spdbtnExtraRecording.Enabled := False;
  AppTitle := Application.Title;
  try
    y := -1;
    repeat
      Inc(y, Step);
      if y and $F = $F then
        Application.ProcessMessages;

      if chkBringTargetToFrontPeriodically.Checked then
        {$IFDEF Windows}
          BringWindowToTop(AInterprettedHandle);
        {$ELSE}
          begin
          end;
        {$ENDIF}

      YLine := y * RectWidth;
      tp.Y := y + rct.Top;
      x := -1;
      repeat
        Inc(x, Step);
        tp.X := x + rct.Left;
        HW := WindowFromPoint(tp);

        AllocatedColor := AddComponentToList(HW, x, y);

        if Step = 1 then
        begin
          ImgMatrix[YLine + x] := AllocatedColor;
          ImgHWMatrix[YLine + x] := HW;
        end
        else
        begin
          imgScannedWindow.Canvas.Pen.Color := AllocatedColor;
          imgScannedWindow.Canvas.Brush.Color := imgScannedWindow.Canvas.Pen.Color;
          imgScannedWindow.Canvas.Rectangle(x, y, x + Step, y + Step);

          imgHandleColors.Canvas.Pen.Color := HW;
          imgHandleColors.Canvas.Brush.Color := imgHandleColors.Canvas.Pen.Color;
          imgHandleColors.Canvas.Rectangle(x, y, x + Step, y + Step);
        end;

        if x and $FF = $FF then
        begin
          prbRecording.Position := YLine + x;
          lblGauge.Caption := IntToStr(prbRecording.Position * 100 div prbRecording.Max) + ' %';
          Application.Title := lblGauge.Caption;
        end;
      until x >= RectWidth - 1;

      if FDoneRec then
        Break;
    until y >= RectHeight - 1;
  finally
    Application.Title := AppTitle;

    if Step = 1 then
    begin
      for y := 0 to RectHeight - 1 do
      begin
        YLine := y * RectWidth;

        for x := 0 to RectWidth - 1 do
        begin
          imgScannedWindow.Canvas.Pixels[x, y] := ImgMatrix[YLine + x];
          imgHandleColors.Canvas.Pixels[x, y] := ImgHWMatrix[YLine + x];
        end;
      end;
    end;

    imgScannedWindowWithText.Picture.Bitmap.Canvas.Draw(0, 0, imgScannedWindow.Picture.Bitmap);
    //imgScannedWindowWithAvoidedZones.Picture.Bitmap.Canvas.Draw(0, 0, imgScannedWindow.Picture.Bitmap);

    Node := vstComponents.GetFirst;
    if Node <> nil then
    begin
      repeat
        NodeData := vstComponents.GetNodeData(Node);

        if NodeData <> nil then
        begin
          imgScannedWindowWithText.Canvas.Font.Color := clWhite;
          imgScannedWindowWithText.Canvas.Brush.Color := NodeData^.AssignedColor;
          imgScannedWindowWithText.Canvas.TextOut(NodeData^.LocalX, NodeData^.LocalY, IntToStr(NodeData^.CompRec.Handle));
        end;

        Node := vstComponents.GetNext(Node);
      until Node = nil;
    end;

    imgScannedWindow.Canvas.Unlock;
    imgHandleColors.Canvas.Unlock;
    imgScannedWindowWithText.Canvas.Unlock;
    imgScannedWindowWithAvoidedZones.Canvas.Unlock;

    vstComponents.EndUpdate;
    btnStartRec.Enabled := True;
    spdbtnExtraRecording.Enabled := True;
  end;  //try

  if chkMinimizeWhileRecording.Checked then
    if AInterprettedHandle <> Handle then
    begin
      //WindowState := wsNormal;
      Application.Restore;
    end;

  memCompInfo.Lines.Add('Done recording in ' + IntToStr(GetTickCount64 - tk) + 'ms.');

  //ScreenShot(InitialHW, imgScreenshot.Picture.Bitmap, 0, 0, RectWidth, RectHeight);

  DestRect.Left := 0;
  DestRect.Top := 0;
  DestRect.Width := rct.Width;
  DestRect.Height := rct.Height;

  WipeBitmap(imgScreenshot.Picture.Bitmap, rct.Width, rct.Height);
  {$IFDEF Windows}
    GetWindowRect(AInterprettedHandle, rct);  //this was InitialHW
  {$ELSE}
  {$ENDIF}
  if not chkFullScr.Checked then
    ScreenShot(AInterprettedHandle, imgScreenshot.Picture.Bitmap, 0, 0, rct.Width, rct.Height)   //this was InitialHW
  else
  begin
    FullScreenBmp := TBitmap.Create;    //full screenshot then crop
    try
      ScreenShot(0, FullScreenBmp, 0, 0, Screen.Width, Screen.Height);
      imgScreenshot.Picture.Bitmap.Canvas.CopyRect(DestRect, FullScreenBmp.Canvas, rct);
    finally
      FullScreenBmp.Free;
    end;
  end;

  GenerateContent_AvgScreenshotAndGenComp;
  AdjustHighlightingLabelsToScreenshot;
  UpdateLayersVisibility;
  DrawAvoidedZones;

  //MessageBox(Handle, 'Done recording', PChar(Caption), MB_ICONINFORMATION);  //sometimes, this messagebox pops up under a Stay-On-Top window
  lblGauge.Caption := '100%';

  prbRecording.Position := 0;
  vstComponents.Repaint;
  UpdateListOfScanedValues;
end;


procedure TfrClickerWinInterp.GetCurrentlyRecordedScreenShotImage(ABmp: TBitmap);
var
  Msg: string;
begin
  if imgScreenshot.Picture.Bitmap = nil then
  begin
    Msg := 'No component is recorded yet.';
    ABmp.Width := ABmp.Canvas.TextWidth(Msg) + 10;
    ABmp.Height := 20;
    ABmp.Canvas.Pen.Color := clWhite;
    ABmp.Canvas.Brush.Color := clWhite;
    ABmp.Canvas.Font.Color := clRed;

    ABmp.Canvas.Rectangle(0, 0, ABmp.Width - 1, ABmp.Height - 1);
    ABmp.Canvas.TextOut(5, 6, Msg);
  end;

  ABmp.Assign(imgScreenshot.Picture.Bitmap);
end;


procedure TfrClickerWinInterp.btnStartRecClick(Sender: TObject);
var
  ImgMatrix: TColorArr;
  ImgHWMatrix: THandleArr;
begin
  if FInterprettedHandle = 0 then
  begin
    pnlDrag.Color := clRed;
    MessageBox(Handle, 'Please set the target control (or window) before recording it.', PChar(Caption), MB_ICONINFORMATION);
    pnlDrag.Color := clYellow;
    Exit;
  end;

  try
    RecordComponent(FInterprettedHandle, ImgMatrix, ImgHWMatrix);
  finally
    SetLength(ImgMatrix, 0);
    SetLength(ImgHWMatrix, 0);
  end;
end;


procedure TfrClickerWinInterp.LoadScreenshotAsPng(AFileName: string; ADestImage: TImage);
var
  Png: TPNGImage;
  MemStream: TMemoryStream;
begin
  if DoOnFileExists(AFileName) then
  begin
    Png := TPNGImage.Create;
    try
      MemStream := TMemoryStream.Create;
      try
        DoOnLoadFileFromStream(AFileName, MemStream);
        MemStream.Position := 0;
        Png.LoadFromStream(MemStream);
      finally
        MemStream.Free;
      end;

      ADestImage.Picture.Bitmap.LoadFromDevice(Png.Canvas.Handle);
    finally
      Png.Free;
    end;
  end
  else
    memCompInfo.Lines.Add('Image not found: ' + AFileName);
end;


procedure TfrClickerWinInterp.SaveScreenshotAsPng(AFileName: string; ASrcImage: TImage);
var
  Png: TPNGImage;
  MemStream: TMemoryStream;
begin
  Png := TPNGImage.Create;
  try
    Png.LoadFromDevice(ASrcImage.Canvas.Handle);

    MemStream := TMemoryStream.Create;
    try
      Png.SaveToStream(MemStream);
      MemStream.Position := 0;
      DoOnSaveFileToStream(AFileName, MemStream);
    finally
      MemStream.Free;
    end;
  finally
    Png.Free;
  end;
end;


procedure FixScreenshotBasePath(var ABasePath: string);
var
  ExtLen: Integer;
begin
  ExtLen := Length(ExtractFileExt(ABasePath));
  Delete(ABasePath, Length(ABasePath) - ExtLen + 1, ExtLen);

  if ABasePath > '' then
    if ABasePath[Length(ABasePath)] <> PathDelim then
      ABasePath := ABasePath + PathDelim;
end;


procedure TfrClickerWinInterp.LoadImages(ABasePath: string);
begin
  FixScreenshotBasePath(ABasePath);
  LoadScreenshotAsPng(ABasePath + 'ScannedWindow.png', imgScannedWindow);
  LoadScreenshotAsPng(ABasePath + 'Screenshot.png', imgScreenshot);
  LoadScreenshotAsPng(ABasePath + 'HandleColors.png', imgHandleColors);
  LoadScreenshotAsPng(ABasePath + 'ScannedWindowWithText.png', imgScannedWindowWithText);

  //draw hashed avoided zones onto imgScannedWindowWithAvoidedZones, if a .zone file is present near the other screenshots
end;


procedure TfrClickerWinInterp.SaveImages(ABasePath: string);
begin
  FixScreenshotBasePath(ABasePath);
  SaveScreenshotAsPng(ABasePath + 'ScannedWindow.png', imgScannedWindow);
  SaveScreenshotAsPng(ABasePath + 'Screenshot.png', imgScreenshot);
  SaveScreenshotAsPng(ABasePath + 'HandleColors.png', imgHandleColors);
  SaveScreenshotAsPng(ABasePath + 'ScannedWindowWithText.png', imgScannedWindowWithText);

  //save avoided zones (imgScannedWindowWithAvoidedZones) if present and used
end;


procedure TfrClickerWinInterp.btnExportClick(Sender: TObject);
  function MakeBlanks(ACount: Integer): string;
  begin
    SetLength(Result, ACount);
    FillChar(Result[1], ACount, ' ');
  end;

var
  Content: TStringList;
  Node: PVirtualNode;
  NodeData: PHighlightedCompRec;
  Blanks, Fnm: string;
  MemStream: TMemoryStream;
begin
  if not DoOnSaveDialogExecute('Yml files (*.yml)|*.yml|All files (*.*)|*.*') then
    Exit;

  Fnm := DoOnGetSaveDialogFileName;
  if ExtractFileExt(Fnm) = '' then
    Fnm := Fnm + '.yml';

  Content := TStringList.Create;
  try
    Content.LineBreak := #13#10;
    Node := vstComponents.GetFirst;
    try
      if Node = nil then
        Exit;

      repeat
        NodeData := vstComponents.GetNodeData(Node);
        Blanks := MakeBlanks(vstComponents.GetNodeLevel(Node) shl 2);

        if NodeData = nil then
          Content.Add(Blanks + 'Data_bug')
        else
        begin
          Content.Add(Blanks + 'Handle: ' + IntToStr(NodeData^.CompRec.Handle));

          if NodeData^.CompRec.IsSubControl then
            Content.Add(Blanks + '    Type: ' + 'Subcontrol')
          else
            Content.Add(Blanks + '    Type: ' + 'Control');

          Content.Add(Blanks + '    Text: "' + NodeData^.CompRec.Text + '"');
          Content.Add(Blanks + '    Class: "' + NodeData^.CompRec.ClassName + '"');
          Content.Add(Blanks + '    Left: ' + IntToStr(NodeData^.CompRec.ComponentRectangle.Left));
          Content.Add(Blanks + '    Top: ' + IntToStr(NodeData^.CompRec.ComponentRectangle.Top));
          Content.Add(Blanks + '    Right: ' + IntToStr(NodeData^.CompRec.ComponentRectangle.Right));
          Content.Add(Blanks + '    Bottom: ' + IntToStr(NodeData^.CompRec.ComponentRectangle.Bottom));
          Content.Add(Blanks + '    LocalX: ' + IntToStr(NodeData^.LocalX));
          Content.Add(Blanks + '    LocalY: ' + IntToStr(NodeData^.LocalY));
        end;

        Node := vstComponents.GetNext(Node);
      until Node = nil;
    finally
      MemStream := TMemoryStream.Create;
      try
        Content.SaveToStream(MemStream);
        MemStream.Position := 0;
        DoOnSaveFileToStream(Fnm, MemStream);
      finally
        MemStream.Free;
      end;
    end;
  finally
    Content.Free;
  end;

  SaveImages(Fnm);
end;


procedure TfrClickerWinInterp.LoadTree(AFnm: string);
var
  MemStream: TMemoryStream;
  s: string;
  TreeHeaderIdx: Integer;
begin
  DoOnClearWinInterp;
  MemStream := TMemoryStream.Create;
  try
    DoOnLoadFileFromStream(AFnm, MemStream);

    SetLength(s, MemStream.Size);
    Move(MemStream.Memory^, s[1], MemStream.Size);
    TreeHeaderIdx := Pos(CTreeHeader, s);

    if TreeHeaderIdx > 0 then
      Move(s[TreeHeaderIdx + Length(CTreeHeader)], FCompTreeVersion, 4)
    else
      FCompTreeVersion := 1;

    memCompInfo.Lines.Add('Tree version: ' + IntToStr(FCompTreeVersion));

    MemStream.Position := 0;
    vstComponents.LoadFromStream(MemStream);
  finally
    MemStream.Free;
  end;

  GenerateCompImagesfromTreeContent;
  LoadImages(AFnm);
  UpdateListOfScanedValues;
end;


procedure TfrClickerWinInterp.btnLoadTreeClick(Sender: TObject);
var
  Fnm: string;
begin
  if not DoOnOpenDialogExecute('Tree files (*.tree)|*.tree|All files (*.*)|*.*') then
    Exit;

  try
    Fnm := DoOnGetOpenDialogFileName;
    LoadTree(Fnm);
  except
    on E: Exception do
      MessageBox(Handle, PChar(E.Message), PChar(Application.Title), MB_ICONERROR);
  end;
end;


procedure TfrClickerWinInterp.DrawAvoidedZones;
  procedure LoadSelectedImage;
  begin
    imgScannedWindowWithAvoidedZones.Width := imgScreenshot.Width;
    imgScannedWindowWithAvoidedZones.Height := imgScreenshot.Height;
    imgScannedWindowWithAvoidedZones.Picture.Bitmap.SetSize(imgScannedWindowWithAvoidedZones.Width, imgScannedWindowWithAvoidedZones.Height);

    case rdgrpLayers.ItemIndex of
      0: imgScannedWindowWithAvoidedZones.Picture.Assign(imgScreenshot.Picture.Bitmap);
      1: imgScannedWindowWithAvoidedZones.Picture.Assign(imgScannedWindowWithText.Picture.Bitmap);
      2: imgScannedWindowWithAvoidedZones.Picture.Assign(imgAvgScreenshotAndGreenComp.Picture.Bitmap); //GenerateContent_AvgScreenshotAndGreenComp should already be called
      3: imgScannedWindowWithAvoidedZones.Picture.Assign(imgAvgScreenshotAndAssignedComp.Picture.Bitmap);
      4: imgScannedWindowWithAvoidedZones.Picture.Assign(imgLiveScreenshot.Picture.Bitmap);
      else
      begin
        imgScannedWindowWithAvoidedZones.Canvas.Brush.Color := clWhite;
        imgScannedWindowWithAvoidedZones.Canvas.Font.Color := clRed;
        imgScannedWindowWithAvoidedZones.Canvas.TextOut(0, 0, 'Not implemented.');
      end;
    end;
  end;

var
  Node: PVirtualNode;
  NodeData: PAvoidedZoneRec;
  SelectedZone, ZRect: TRect;
begin
  LoadSelectedImage;

  Node := vstAvoidedZones.GetFirst;
  if Node = nil then
    Exit;

  SelectedZone.Left := -300; //some unlikely value
  //imgScannedWindowWithAvoidedZones.Canvas.Lock;
  try
    LoadSelectedImage;

    imgScannedWindowWithAvoidedZones.Canvas.Brush.Style := bsBDiagonal;
    imgScannedWindowWithAvoidedZones.Canvas.Pen.Color := colboxHighlightingLabels.Selected;
    imgScannedWindowWithAvoidedZones.Canvas.Brush.Color := colboxHighlightingLabels.Selected;

    repeat
      NodeData := vstAvoidedZones.GetNodeData(Node);
      if NodeData <> nil then
      begin
        ZoneRectStrToRect(NodeData^.ZRectStr, ZRect);
        imgScannedWindowWithAvoidedZones.Canvas.Rectangle(ZRect);
        if vstAvoidedZones.Selected[Node] then
          SelectedZone := ZRect;
      end;

      Node := Node^.NextSibling;
    until Node = nil;
  finally
    //imgScannedWindowWithAvoidedZones.Canvas.Unlock;
  end;

  if SelectedZone.Left <> -300 then
  begin
    imgScannedWindowWithAvoidedZones.Canvas.Brush.Style := bsDiagCross;
    imgScannedWindowWithAvoidedZones.Canvas.Pen.Color := clMaroon;
    imgScannedWindowWithAvoidedZones.Canvas.Brush.Color := clMaroon;
    imgScannedWindowWithAvoidedZones.Canvas.Rectangle(SelectedZone);
  end;
end;


const
  CZoneFilter = 'WinInterp Avoided Zone (*.zone)|*.zone|All Files (*.*)|*.*';

procedure TfrClickerWinInterp.LoadZones(AFnm: string);
var
  Ini: TClkIniReadonlyFile;
  MemStream: TMemoryStream;
  Prefix: string;
  Node: PVirtualNode;
  NodeData: PAvoidedZoneRec;
  i, n: Integer;
begin
  vstAvoidedZones.Clear;
  vstAvoidedZones.BeginUpdate;
  try
    MemStream := TMemoryStream.Create;
    try
      DoOnLoadFileFromStream(AFnm, MemStream);
      MemStream.Position := 0;

      Ini := TClkIniReadonlyFile.Create(MemStream);
      try
        n := Ini.ReadInteger('Zones', 'Count', 0);

        for i := 0 to n - 1 do
        begin
          Node := vstAvoidedZones.InsertNode(vstAvoidedZones.RootNode, amInsertAfter);
          NodeData := vstAvoidedZones.GetNodeData(Node);

          Prefix := 'Zone_' + IntToStr(i) + '.';
          NodeData^.ZRectStr.Left := Ini.ReadString('Zones', Prefix + 'Left', '0');
          NodeData^.ZRectStr.Top := Ini.ReadString('Zones', Prefix + 'Top', '0');
          NodeData^.ZRectStr.Right := Ini.ReadString('Zones', Prefix + 'Right', '0');
          NodeData^.ZRectStr.Bottom := Ini.ReadString('Zones', Prefix + 'Bottom', '0');

          NodeData^.ZRectStr.LeftOffset := Ini.ReadString('Zones', Prefix + 'LeftOffset', '0');
          NodeData^.ZRectStr.TopOffset := Ini.ReadString('Zones', Prefix + 'TopOffset', '0');
          NodeData^.ZRectStr.RightOffset := Ini.ReadString('Zones', Prefix + 'RightOffset', '0');
          NodeData^.ZRectStr.BottomOffset := Ini.ReadString('Zones', Prefix + 'BottomOffset', '0');

          NodeData^.ZName := Ini.ReadString('Zones', Prefix + 'Name', 'Zone');
        end;
      finally
        Ini.Free;
      end;
    finally
      MemStream.Free;
    end;
  finally
    vstAvoidedZones.EndUpdate;
  end;

  UpdateListOfScanedValues;
  DrawAvoidedZones;
end;


procedure TfrClickerWinInterp.btnLoadZonesClick(Sender: TObject);
var
  ZoneFnm: string;
begin
  if not DoOnOpenDialogExecute(CZoneFilter) then
    Exit;

  ZoneFnm := DoOnGetOpenDialogFileName;
  if not DoOnFileExists(ZoneFnm) then
  begin
    MessageBox(Handle, PChar('File not found: ' + #13#10 + ZoneFnm), PChar(Application.Title), MB_ICONERROR);
    Exit;
  end;

  LoadZones(ZoneFnm);
end;


procedure TfrClickerWinInterp.SaveZones(AFnm: string);
var
  MemStream: TMemoryStream;
  Prefix: string;
  Node: PVirtualNode;
  NodeData: PAvoidedZoneRec;
  TempList: TStringList;
begin
  MemStream := TMemoryStream.Create;
  TempList := TStringList.Create;
  try
    TempList.LineBreak := #13#10;
    TempList.Add('[Zones]');
    TempList.Add('Count=' + IntToStr(vstAvoidedZones.RootNodeCount));

    Node := vstAvoidedZones.GetFirst;
    if Node <> nil then
    begin
      repeat
        NodeData := vstAvoidedZones.GetNodeData(Node);
        Prefix := 'Zone_' + IntToStr(Node^.Index) + '.';

        TempList.Add(Prefix + 'Left' + '=' + NodeData^.ZRectStr.Left);
        TempList.Add(Prefix + 'Top' + '=' + NodeData^.ZRectStr.Top);
        TempList.Add(Prefix + 'Right' + '=' + NodeData^.ZRectStr.Right);
        TempList.Add(Prefix + 'Bottom' + '=' + NodeData^.ZRectStr.Bottom);

        TempList.Add(Prefix + 'LeftOffset' + '=' + NodeData^.ZRectStr.LeftOffset);
        TempList.Add(Prefix + 'TopOffset' + '=' + NodeData^.ZRectStr.TopOffset);
        TempList.Add(Prefix + 'RightOffset' + '=' + NodeData^.ZRectStr.RightOffset);
        TempList.Add(Prefix + 'BottomOffset' + '=' + NodeData^.ZRectStr.BottomOffset);

        TempList.Add(Prefix + 'Name' + '=' + NodeData^.ZName);

        Node := Node^.NextSibling;
      until Node = nil;
    end;

    TempList.SaveToStream(MemStream);
    MemStream.Position := 0;
    DoOnSaveFileToStream(AFnm, MemStream);
  finally
    MemStream.Free;
    TempList.Free;
  end;
end;


procedure TfrClickerWinInterp.btnSaveZonesClick(Sender: TObject);
var
  ZoneFnm: string;
begin
  if not DoOnSaveDialogExecute(CZoneFilter) then
    Exit;

  ZoneFnm := DoOnGetSaveDialogFileName;
  if ExtractFileExt(ZoneFnm) = '' then //if ExtractFileName(ZoneFnm) = ExtractFileNameNoExt(ZoneFnm) then
    ZoneFnm := ZoneFnm + '.zone';

  SaveZones(ZoneFnm);
end;


procedure TfrClickerWinInterp.UpdateZoneByNode(ANode: PVirtualNode; AZoneName, ALeftOffset, ATopOffset, ARightOffset, ABottomOffset, ALeft, ATop, ARight, ABottom: string);
var
  NodeData: PAvoidedZoneRec;
begin
  NodeData := vstAvoidedZones.GetNodeData(ANode);
  if NodeData = nil then
    Exit;

  NodeData^.ZName := AZoneName;
  NodeData^.ZRectStr.LeftOffset := ALeftOffset;
  NodeData^.ZRectStr.TopOffset := ATopOffset;
  NodeData^.ZRectStr.RightOffset := ARightOffset;
  NodeData^.ZRectStr.BottomOffset := ABottomOffset;
  NodeData^.ZRectStr.Left := ALeft; //'$<New>Comp_Left$';
  NodeData^.ZRectStr.Top := ATop; //'$<New>Comp_Top$';
  NodeData^.ZRectStr.Right := ARight; //'$<New>Comp_Left$';  //leave it to Left for now
  NodeData^.ZRectStr.Bottom := ABottom; //'$<New>Comp_Top$';  //leave it to Top for now
end;


procedure TfrClickerWinInterp.UpdateZone(AZoneName, ALeftOffset, ATopOffset, ARightOffset, ABottomOffset, ALeft, ATop, ARight, ABottom: string);
var
  Node: PVirtualNode;
begin
  Node := GetZoneNodeByName(AZoneName);

  if Node = nil then
    raise Exception.Create('Zone not found: ' + AZoneName);

  UpdateZoneByNode(Node, AZoneName, ALeftOffset, ATopOffset, ARightOffset, ABottomOffset, ALeft, ATop, ARight, ABottom);
  DrawAvoidedZones;
end;


procedure TfrClickerWinInterp.NewZone(AZoneName, ALeftOffset, ATopOffset, ARightOffset, ABottomOffset, ALeft, ATop, ARight, ABottom: string);
var
  Node: PVirtualNode;
begin
  Node := vstAvoidedZones.InsertNode(vstAvoidedZones.RootNode, amInsertAfter);
  UpdateZoneByNode(Node, AZoneName, ALeftOffset, ATopOffset, ARightOffset, ABottomOffset, ALeft, ATop, ARight, ABottom);
  vstAvoidedZones.Selected[Node] := True;

  DrawAvoidedZones;
end;


procedure TfrClickerWinInterp.btNewZoneClick(Sender: TObject);
var
  Node: PVirtualNode;
  NodeData: PAvoidedZoneRec;
begin
  Node := vstAvoidedZones.GetFirstSelected;
  if Node <> nil then
  begin
    NodeData := vstAvoidedZones.GetNodeData(Node);
    if NodeData = nil then
      Exit;

    NewZone(NodeData^.ZName,
            NodeData^.ZRectStr.LeftOffset, NodeData^.ZRectStr.TopOffset, NodeData^.ZRectStr.RightOffset, NodeData^.ZRectStr.BottomOffset,
            NodeData^.ZRectStr.Left, NodeData^.ZRectStr.Top, NodeData^.ZRectStr.Right, NodeData^.ZRectStr.Bottom);
  end
  else
    NewZone('Zone', '0', '0', '30', '30', '', '', '', '');
end;



procedure TfrClickerWinInterp.DeleteZone(ANode: PVirtualNode);
begin
  vstAvoidedZones.DeleteNode(ANode);
  DrawAvoidedZones;
end;


function TfrClickerWinInterp.GetZoneNodeByName(AName: string): PVirtualNode;
var
  Node: PVirtualNode;
  NodeData: PAvoidedZoneRec;
begin
  Result := nil;

  Node := vstAvoidedZones.GetFirst;
  if Node = nil then
    Exit;

  repeat
    NodeData := vstAvoidedZones.GetNodeData(Node);
    if NodeData <> nil then
      if NodeData^.ZName = AName then
      begin
        Result := Node;
        Break;
      end;

    Node := Node^.NextSibling;
  until Node = nil;
end;


procedure TfrClickerWinInterp.DeleteZoneByName(AName: string);
var
  Node: PVirtualNode;
begin
  Node := GetZoneNodeByName(AName);

  if Node = nil then
    if vstAvoidedZones.RootNodeCount > 0 then
      raise Exception.Create('Zone not found: ' + AName);

  DeleteZone(Node);
end;


procedure TfrClickerWinInterp.btnDeleteZoneClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  if vstAvoidedZones.RootNodeCount = 0 then
    Exit;

  Node := vstAvoidedZones.GetFirstSelected;
  if Node = nil then
  begin
    MessageBox(Handle, 'No zone is selected to be deleted. Please select a zone.', PChar(Application.Title), MB_ICONINFORMATION);
    Exit;
  end;

  if MessageBox(Handle, 'Are you sure you want to delete the selected zone?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  DeleteZone(Node);
end;


function TfrClickerWinInterp.GetZoneByName(AName: string): string;
var
  Node: PVirtualNode;
  NodeData: PAvoidedZoneRec;
begin
  Node := GetZoneNodeByName(AName);

  if Node = nil then
    if vstAvoidedZones.RootNodeCount > 0 then
      raise Exception.Create('Zone not found: ' + AName);

  NodeData := vstAvoidedZones.GetNodeData(Node);
  if NodeData = nil then
    raise Exception.Create('Zone data not found for zone: ' + AName);

  Result := NodeData^.ZName + #6#8 +
            NodeData^.ZRectStr.LeftOffset + #6#8 +
            NodeData^.ZRectStr.TopOffset + #6#8 +
            NodeData^.ZRectStr.RightOffset + #6#8 +
            NodeData^.ZRectStr.BottomOffset + #6#8 +
            NodeData^.ZRectStr.Left + #6#8 +
            NodeData^.ZRectStr.Top + #6#8 +
            NodeData^.ZRectStr.Right + #6#8 +
            NodeData^.ZRectStr.Bottom + #6#8;
end;


procedure TfrClickerWinInterp.btnClearZonesClick(Sender: TObject);
begin
  if MessageBox(Handle, 'Are you sure you want to clear the list of avoided zones?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  vstAvoidedZones.Clear;
  DrawAvoidedZones;
end;


procedure TfrClickerWinInterp.btnSaveTreeClick(Sender: TObject);
var
  Fnm: string;
  MemStream: TMemoryStream;
begin
  if not DoOnSaveDialogExecute('Tree files (*.tree)|*.tree|All files (*.*)|*.*') then
    Exit;

  Fnm := DoOnGetSaveDialogFileName;
  if ExtractFileExt(Fnm) = '' then
    Fnm := Fnm + '.tree';

  MemStream := TMemoryStream.Create;
  try
    vstComponents.SaveToStream(MemStream);
    MemStream.Position := 0;
    DoOnSaveFileToStream(Fnm, MemStream);
  finally
    MemStream.Free;
  end;
  SaveImages(Fnm);
end;


procedure TfrClickerWinInterp.GenerateCompImagesfromTreeContent;
var
  Node: PVirtualNode;
  NodeData: PHighlightedCompRec;
begin
  Node := vstComponents.GetFirst;  //first component defines the image size
  if Node = nil then
    Exit;

  NodeData := vstComponents.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  vstComponents.BeginUpdate;
  try
    SetLength(FColoredHandles, 0);
  finally
    vstComponents.EndUpdate;
  end;

  imgScannedWindow.Width := NodeData^.CompRec.ComponentRectangle.Width;
  imgScannedWindow.Height := NodeData^.CompRec.ComponentRectangle.Height;

  WipeImage(imgScannedWindow, imgScannedWindow.Width, imgScannedWindow.Height);
  //WipeImage(imgScreenshot, imgScannedWindow.Width, imgScannedWindow.Height);     //expected to be already loaded with a real screenshot
  WipeImage(imgAvgScreenshotAndGreenComp, imgScannedWindow.Width, imgScannedWindow.Height);
  WipeImage(imgAvgScreenshotAndAssignedComp, imgScannedWindow.Width, imgScannedWindow.Height);
  WipeImage(imgLiveScreenshot, imgScannedWindow.Width, imgScannedWindow.Height);
  WipeImage(imgHandleColors, imgScannedWindow.Width, imgScannedWindow.Height);
  WipeImage(imgScannedWindowWithText, imgScannedWindow.Width, imgScannedWindow.Height);
  //WipeImage(imgScannedWindowWithAvoidedZones, imgScannedWindow.Width, imgScannedWindow.Height);


  imgScannedWindow.Canvas.Lock;
  imgHandleColors.Canvas.Lock;
  imgScannedWindowWithText.Canvas.Lock;
  //imgScannedWindowWithAvoidedZones.Canvas.Lock;
  try
    repeat
      NodeData := vstComponents.GetNodeData(Node);

      if NodeData <> nil then
      begin
        imgScannedWindow.Canvas.Pen.Color := NodeData^.AssignedColor;
        imgScannedWindow.Canvas.Brush.Color := NodeData^.AssignedColor;
        imgScannedWindow.Canvas.Rectangle(NodeData^.LocalX,             //LocalX and LocalY are relative to the screenshot image (a.k.a. first component)
                                          NodeData^.LocalY,
                                          NodeData^.LocalX + NodeData^.CompRec.ComponentRectangle.Width - 1,
                                          NodeData^.LocalY + NodeData^.CompRec.ComponentRectangle.Height - 1);

        imgHandleColors.Canvas.Pen.Color := NodeData^.CompRec.Handle;
        imgHandleColors.Canvas.Brush.Color := NodeData^.CompRec.Handle;
        imgHandleColors.Canvas.Rectangle(NodeData^.LocalX,
                                         NodeData^.LocalY,
                                         NodeData^.LocalX + NodeData^.CompRec.ComponentRectangle.Width - 1,
                                         NodeData^.LocalY + NodeData^.CompRec.ComponentRectangle.Height - 1);

        AddColoredHandle(NodeData^.CompRec.Handle, NodeData^.AssignedColor);
      end
      else
        AddColoredHandle(0, 0); //some dummy values

      Node := vstComponents.GetNext(Node);
    until Node = nil;

    imgScannedWindowWithText.Picture.Bitmap.Canvas.Draw(0, 0, imgScannedWindow.Picture.Bitmap);


    repeat
      NodeData := vstComponents.GetNodeData(Node);

      if NodeData <> nil then
      begin
        imgScannedWindowWithText.Canvas.Font.Color := clWhite;
        imgScannedWindowWithText.Canvas.Brush.Color := NodeData^.AssignedColor;
        imgScannedWindowWithText.Canvas.TextOut(NodeData^.LocalX, NodeData^.LocalY, IntToStr(NodeData^.CompRec.Handle));
      end;

      Node := vstComponents.GetNext(Node);
    until Node = nil;

    GenerateContent_AvgScreenshotAndGenComp;   //requires images to have some screenshots, already loaded
    AdjustHighlightingLabelsToScreenshot;
    UpdateLayersVisibility;

    //draw something on imgScannedWindowWithAvoidedZones ???
  finally
    imgScannedWindow.Canvas.Unlock;
    imgHandleColors.Canvas.Unlock;
    imgScannedWindowWithText.Canvas.Unlock;
    //imgScannedWindowWithAvoidedZones.Canvas.Unlock;
  end;
end;


procedure TfrClickerWinInterp.GetTreeContent(AStream: TMemoryStream);
begin
  vstComponents.SaveToStream(AStream);
end;


function TfrClickerWinInterp.SetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue: string): Boolean;
var
  OptionValueAsBool: Boolean;
  OptionValueAsInt: Integer;
  CmdParams: TStringList;
  ZoneName, ZoneLeft, ZoneTop, ZoneRight, ZoneBottom, ZoneLeftOffset, ZoneTopOffset, ZoneRightOffset, ZoneBottomOffset: string;
begin
  Result := False; //unknown option name or command
  AWinInterpOptionName := UpperCase(AWinInterpOptionName);
  OptionValueAsBool := (AWinInterpOptionValue = '1') or (AWinInterpOptionValue = 'True');
  OptionValueAsInt := StrToIntDef(AWinInterpOptionValue, 0);

  if AWinInterpOptionName = UpperCase(CWinInterpOption_ShowZoom) then
  begin
    chkShowZoom.Checked := OptionValueAsBool;
    Result := True;
    Exit;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_ContinuouslyScreenshotByKeys) then
  begin
    chkContinuouslyScreenshotByKeys.Checked := OptionValueAsBool;
    Result := True;
    Exit;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_RecordingStep) then
  begin
    lbeStep.Text := IntToStr(Min(Max(1, OptionValueAsInt), 100));
    Result := True;
    Exit;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_MouseCursorPosToScreenshotDelay) then
  begin
    lbeMouseCursorPosToScreenshotDelay.Text := IntToStr(Min(Max(0, OptionValueAsInt), 1000));
    Result := True;
    Exit;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_HighlightSelectedComponent) then
  begin
    chkHighlightSelectedComponent.Checked := OptionValueAsBool;
    Result := True;
    Exit;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_UseHCursor) then
  begin
    chkUseHCursor.Checked := OptionValueAsBool;
    Result := True;
    Exit;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_FullScreenScanning) then
  begin
    chkFullScr.Checked := OptionValueAsBool;
    Result := True;
    Exit;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_RecordSelectedAreaOnly) then
  begin
    chkRecordSelectedAreaOnly.Checked := OptionValueAsBool;
    Result := True;
    Exit;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_RecordWithEdgeExtending) then
  begin
    chkRecordWithEdgeExtending.Checked := OptionValueAsBool;
    Result := True;
    Exit;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_MinimizeWhileRecording) then
  begin
    chkMinimizeWhileRecording.Checked := OptionValueAsBool;
    Result := True;
    Exit;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_BringTargetToFront) then
  begin
    chkBringTargetToFront.Checked := OptionValueAsBool;
    Result := True;
    Exit;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_BringTargetToFrontPeriodically) then
  begin
    chkBringTargetToFrontPeriodically.Checked := OptionValueAsBool;
    Result := True;
    Exit;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_HighlightingLinesColor) then
  begin
    colboxHighlightingLabels.ItemIndex := Min(Max(0, OptionValueAsInt), colboxHighlightingLabels.Items.Count - 1);
    Result := True;
    Exit;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_SelectedLayer) then
  begin
    rdgrpLayers.ItemIndex := Min(Max(0, OptionValueAsInt), rdgrpLayers.Items.Count - 1);
    Result := True;
    Exit;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_LoadTree) then
  begin
    LoadTree(AWinInterpOptionValue);
    Result := True;
    Exit;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_LoadAvoidedZones) then
  begin
    LoadZones(AWinInterpOptionValue);
    Result := True;
    Exit;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_SaveAvoidedZones) then
  begin
    SaveZones(AWinInterpOptionValue);
    Result := True;
    Exit;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_NewAvoidedZone) then
  begin
    CmdParams := TStringList.Create;
    try
      CmdParams.LineBreak := #13#10;
      CmdParams.Text := StringReplace(AWinInterpOptionValue, '|', #13#10, [rfReplaceAll]);
      if CmdParams.Count = 1 then
        CmdParams.Text := FastReplace_68ToReturn(AWinInterpOptionValue); //maybe #6$8 is used as separator

      try
        ZoneName := CmdParams.Strings[0];
        ZoneLeftOffset := CmdParams.Strings[1];
        ZoneTopOffset := CmdParams.Strings[2];
        ZoneRightOffset := CmdParams.Strings[3];
        ZoneBottomOffset := CmdParams.Strings[4];
        ZoneLeft := CmdParams.Strings[5];
        ZoneTop := CmdParams.Strings[6];
        ZoneRight := CmdParams.Strings[7];
        ZoneBottom := CmdParams.Strings[8];
      except
        Result := False;
        raise Exception.Create('NewAvoidedZone command expects 9 parameters, separated by "|" or $#6#8$, e.g.: MyFirstZone|0|0|30|40|$btnOpen_Left$|$btnOpen_Top$|$btnOpen_Right$|$btnOpen_Bottom$. The Left, Top, Right and Bottom parameters can be empty or they can contain WinInterp-specific variables.');
      end;

      NewZone(ZoneName, ZoneLeftOffset, ZoneTopOffset, ZoneRightOffset, ZoneBottomOffset, ZoneLeft, ZoneTop, ZoneRight, ZoneBottom);
      Result := True;
      Exit;
    finally
      CmdParams.Free;
    end;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_DeleteAvoidedZone) then
  begin
    DeleteZoneByName(AWinInterpOptionValue);
    Result := True;
    Exit;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_GetAvoidedZone) then
  begin
    raise TGetAvoidedZoneException.Create(GetZoneByName(AWinInterpOptionValue));   //using exceptions to pass the result
    Result := True;
    Exit;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_SetAvoidedZone) then
  begin
    CmdParams := TStringList.Create;
    try
      CmdParams.LineBreak := #13#10;
      CmdParams.Text := StringReplace(AWinInterpOptionValue, '|', #13#10, [rfReplaceAll]);
      if CmdParams.Count = 1 then
        CmdParams.Text := FastReplace_68ToReturn(AWinInterpOptionValue); //maybe #6$8 is used as separator

      try
        ZoneName := CmdParams.Strings[0];
        ZoneLeftOffset := CmdParams.Strings[1];
        ZoneTopOffset := CmdParams.Strings[2];
        ZoneRightOffset := CmdParams.Strings[3];
        ZoneBottomOffset := CmdParams.Strings[4];
        ZoneLeft := CmdParams.Strings[5];
        ZoneTop := CmdParams.Strings[6];
        ZoneRight := CmdParams.Strings[7];
        ZoneBottom := CmdParams.Strings[8];
      except
        Result := False;
        raise Exception.Create('SetAvoidedZone command expects 9 parameters, separated by "|" or $#6#8$, e.g.: ExistingZone|0|0|30|40|$btnOpen_Left$|$btnOpen_Top$|$btnOpen_Right$|$btnOpen_Bottom$. The Left, Top, Right and Bottom parameters can be empty or they can contain WinInterp-specific variables.');
      end;

      UpdateZone(ZoneName, ZoneLeftOffset, ZoneTopOffset, ZoneRightOffset, ZoneBottomOffset, ZoneLeft, ZoneTop, ZoneRight, ZoneBottom);
      Result := True;
      Exit;
    finally
      CmdParams.Free;
    end;
  end;

  if AWinInterpOptionName = UpperCase(CWinInterpOption_ClearAvoidedZone) then
  begin
    vstAvoidedZones.Clear;
    DrawAvoidedZones;

    Result := True;
    Exit;
  end;
end;


procedure TfrClickerWinInterp.btnStopRecClick(Sender: TObject);
begin
  FDoneRec := True;
end;


procedure TfrClickerWinInterp.chkContinuouslyScreenshotByKeysChange(
  Sender: TObject);
begin
  tmrScanByKeys.Enabled := True;
end;


procedure TfrClickerWinInterp.chkHighlightSelectedComponentChange(Sender: TObject);
begin
  FSelectedComponentLeftLimitLabel.Visible := chkHighlightSelectedComponent.Checked;
  FSelectedComponentTopLimitLabel.Visible := chkHighlightSelectedComponent.Checked;
  FSelectedComponentRightLimitLabel.Visible := chkHighlightSelectedComponent.Checked;
  FSelectedComponentBottomLimitLabel.Visible := chkHighlightSelectedComponent.Checked;

  FTransparent_SelectedComponentLeftLimitLabel.Visible := chkHighlightSelectedComponent.Checked;
  FTransparent_SelectedComponentTopLimitLabel.Visible := chkHighlightSelectedComponent.Checked;
  FTransparent_SelectedComponentRightLimitLabel.Visible := chkHighlightSelectedComponent.Checked;
  FTransparent_SelectedComponentBottomLimitLabel.Visible := chkHighlightSelectedComponent.Checked;
end;


procedure TfrClickerWinInterp.chkRecordSelectedAreaOnlyChange(Sender: TObject);
begin
  FRecordSelectedAreaOnly := chkRecordSelectedAreaOnly.Checked;
end;


procedure TfrClickerWinInterp.chkRecordWithEdgeExtendingChange(Sender: TObject);
begin
  FRecordWithEdgeExtending := chkRecordWithEdgeExtending.Checked;
end;


procedure TfrClickerWinInterp.SetHighlightingLabelsColor;
begin
  FSelectedComponentLeftLimitLabel.Color := colboxHighlightingLabels.Selected;
  FSelectedComponentTopLimitLabel.Color := colboxHighlightingLabels.Selected;
  FSelectedComponentRightLimitLabel.Color := colboxHighlightingLabels.Selected;
  FSelectedComponentBottomLimitLabel.Color := colboxHighlightingLabels.Selected;
end;


procedure TfrClickerWinInterp.colboxHighlightingLabelsSelect(Sender: TObject);
begin
  SetHighlightingLabelsColor;
end;


procedure TfrClickerWinInterp.GetWindowInfo;
var
  InterprettedRectangle: TRect;
begin
  InterprettedRectangle.Top := 0;
  InterprettedRectangle.Bottom := 0;
  InterprettedRectangle.Left := 0;
  InterprettedRectangle.Right := 0;

  {$IFDEF Windows}
    if GetWindowRect(FInterprettedHandle, InterprettedRectangle) = False then
  {$ELSE}
  {$ENDIF}
    Exit;

  memCompInfo.Lines.Add('Left: ' + IntToStr(InterprettedRectangle.Left) + '   ' +
                        'Top: ' + IntToStr(InterprettedRectangle.Top) + '   ' +
                        'Right: ' + IntToStr(InterprettedRectangle.Right) + '   ' +
                        'Bottom: ' + IntToStr(InterprettedRectangle.Bottom));
end;


procedure TfrClickerWinInterp.UpdateLayersVisibility;
var
  Node: PVirtualNode;
begin
  imgScreenshot.Visible := False;
  imgScannedWindow.Visible := False;
  imgScannedWindowWithText.Visible := False;
  imgAvgScreenshotAndGreenComp.Visible := False;
  imgAvgScreenshotAndAssignedComp.Visible := False;
  imgLiveScreenshot.Visible := False;

  Node := vstComponents.GetFirstSelected;
  if Node = nil then
    Node := vstComponents.GetFirst;

  case rdgrpLayers.ItemIndex of
    0: imgScreenshot.Visible := True;

    1:
    begin
      //imgScannedWindow.Visible := True;
      imgScannedWindowWithText.Visible := True;
    end;

    2:
    begin
      imgAvgScreenshotAndGreenComp.Visible := True;
      GenerateContent_AvgScreenshotAndGreenComp(Node);
    end;

    3: imgAvgScreenshotAndAssignedComp.Visible := True;

    4: imgLiveScreenshot.Visible := True;

    //see also the zoom timer for a similar case structure, if adding new cases
    else
  end;

  HighlightComponent(Node);
end;


procedure TfrClickerWinInterp.rdgrpLayersClick(Sender: TObject);
begin
  UpdateLayersVisibility;
end;


procedure TfrClickerWinInterp.scrboxScannedComponentsMouseWheel(
  Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  Factor: Integer;
begin
  if ssCtrl in Shift then
    Factor := 1
  else
    Factor := 3;

  if ssShift in Shift then
    scrboxScannedComponents.HorzScrollBar.Position := scrboxScannedComponents.HorzScrollBar.Position - WheelDelta div Factor
  else
    scrboxScannedComponents.VertScrollBar.Position := scrboxScannedComponents.VertScrollBar.Position - WheelDelta div Factor;

  Handled := True;
end;


procedure TfrClickerWinInterp.spdbtnExtraRecordingClick(Sender: TObject);
var
  tp: TPoint;
begin
  GetCursorPos(tp);
  pmExtraRecording.PopUp(tp.X, tp.Y);
end;


procedure TfrClickerWinInterp.spdbtnMoveUpClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstAvoidedZones.GetFirstSelected;
  if Node = nil then
    Exit;

  if Node = vstAvoidedZones.GetFirst then
    Exit;

  vstAvoidedZones.MoveTo(Node, Node^.PrevSibling, amInsertBefore, False);
end;


procedure TfrClickerWinInterp.tmrDrawZoomTimer(Sender: TObject);
var
  TempBmp: TBitmap;
  tp: TPoint;
  ZoomImg: TImage;
  LineColor: TColor;
  SelLeft, SelTop, SelRight, SelBottom: Integer;
begin
  tmrDrawZoom.Enabled := False;
  ZoomImg := nil; //display AV if not handled in the case structures below

  case PageControlWinInterp.ActivePageIndex of
    0:
    begin
      case rdgrpLayers.ItemIndex of
        0: ZoomImg := imgScreenshot;
        1: ZoomImg := imgScannedWindowWithText;
        2: ZoomImg := imgAvgScreenshotAndGreenComp;
        3: ZoomImg := imgAvgScreenshotAndAssignedComp;
        4: ZoomImg := imgLiveScreenshot;
        else
      end;

      LineColor := colboxHighlightingLabels.Selected;

      SelLeft := FSelectedComponentLeftLimitLabel.Left;
      SelTop := FSelectedComponentTopLimitLabel.Top;
      SelRight := FSelectedComponentRightLimitLabel.Left;
      SelBottom := FSelectedComponentBottomLimitLabel.Top;
    end;

    1:
    begin
      ZoomImg := imgScannedWindowWithAvoidedZones;
      LineColor := $4444FF; //using $4444FF, instead of red, to avoid having a strong color

      SelLeft := FSelectedZoneLeftLimitLabel.Left;
      SelTop := FSelectedZoneTopLimitLabel.Top;
      SelRight := FSelectedZoneRightLimitLabel.Left;
      SelBottom := FSelectedZoneBottomLimitLabel.Top;
    end;
  end; //ActivePageIndex

  TempBmp := TBitmap.Create;
  try
    TempBmp.Width := ZoomImg.Width;
    TempBmp.Height := ZoomImg.Height;
    TempBmp.Canvas.Draw(0, 0, ZoomImg.Picture.Bitmap);

    TempBmp.Canvas.Pen.Color := LineColor;
    Line(TempBmp.Canvas, SelLeft, 0, SelLeft, TempBmp.Height - 1);

    TempBmp.Canvas.Pen.Color := LineColor;
    Line(TempBmp.Canvas, 0, SelTop, TempBmp.Width - 1, SelTop);

    TempBmp.Canvas.Pen.Color := LineColor;
    Line(TempBmp.Canvas, SelRight, 0, SelRight, TempBmp.Height - 1);

    TempBmp.Canvas.Pen.Color := LineColor;
    Line(TempBmp.Canvas, 0, SelBottom, TempBmp.Width - 1, SelBottom);

    //average TempBmp with ZoomImg.Picture.Bitmap, for transparency
    AvgBitmapWithBitmap(TempBmp, ZoomImg.Picture.Bitmap, TempBmp);

    GetCursorPos(tp);
    SetZoomContent(TempBmp, FCurrentMousePosOnPreviewImg.X, FCurrentMousePosOnPreviewImg.Y, tp.X + 50, tp.Y + 50);
  finally
    TempBmp.Free;
  end;
end;


procedure TfrClickerWinInterp.spdbtnMoveDownClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstAvoidedZones.GetFirstSelected;
  if Node = nil then
    Exit;

  if Node = vstAvoidedZones.GetLast then
    Exit;

  vstAvoidedZones.MoveTo(Node, Node^.NextSibling, amInsertAfter, False);
end;


procedure TfrClickerWinInterp.tmrEditComponentsTimer(Sender: TObject);
begin
  tmrEditComponents.Enabled := False;

  if FMouseUpHitInfo_Components.HitNode = nil then
    Exit;

  vstComponents.EditNode(FMouseUpHitInfo_Components.HitNode, FMouseUpHitInfo_Components.HitColumn);
end;


procedure TfrClickerWinInterp.tmrEditSettingsTimer(Sender: TObject);
begin
  tmrEditSettings.Enabled := False;

  if FMouseUpHitInfo_Settings.HitNode = nil then
    Exit;

  vstAvoidedZones.EditNode(FMouseUpHitInfo_Settings.HitNode, FMouseUpHitInfo_Settings.HitColumn);
end;


procedure TfrClickerWinInterp.tmrScanByKeysTimer(Sender: TObject);
begin
  {$IFDEF Windows}
    if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) and (GetAsyncKeyState(VK_MENU) < 0) then
  {$ELSE}
    if (GetKeyState(VK_CONTROL) < 0) and (GetKeyState(VK_SHIFT) < 0) and (GetKeyState(VK_MENU) < 0) then
  {$ENDIF}
    ScanTargetControl;
end;


procedure TfrClickerWinInterp.tmrScanTimer(Sender: TObject);
begin
  ScanTargetControl;
end;


procedure TfrClickerWinInterp.tmrSpinnerTimer(Sender: TObject);
begin
  //imgSpinner.Picture.PNG;
  imglstSpinner.Draw(imgSpinner.Canvas, 0, 0, tmrSpinner.Tag and $7, dsNormal, itImage);
  tmrSpinner.Tag := tmrSpinner.Tag + 1;
end;


procedure TfrClickerWinInterp.vstComponentsClick(Sender: TObject);
begin
  //cannot use this event, because it is not called on clicking the last node :(
end;


procedure TfrClickerWinInterp.vstComponentsDblClick(Sender: TObject);
begin
  tmrEditComponents.Enabled := True;
end;


procedure TfrClickerWinInterp.vstAvoidedZonesDblClick(Sender: TObject);
begin
  tmrEditSettings.Enabled := True;
end;


procedure TfrClickerWinInterp.vstComponentsEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  NodeData: PHighlightedCompRec;
begin
  if FMouseUpHitInfo_Components.HitNode = nil then
    Exit;

  if not FUpdatedVstText then
    Exit;

  NodeData := vstComponents.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  case Column of
    0: NodeData^.CompRec.Handle := StrToIntDef(FEditingText, 0);
    1: NodeData^.CompRec.ClassName := FEditingText;
    2: NodeData^.CompRec.Text := FEditingText;
    3: NodeData^.CGClassName := FEditingText;

    4: NodeData^.AssignedColor := HexToInt(FEditingText);
    5: NodeData^.CompRec.ComponentRectangle.Left := StrToIntDef(FEditingText, 0);
    6: NodeData^.CompRec.ComponentRectangle.Top := StrToIntDef(FEditingText, 0);
    7: NodeData^.CompRec.ComponentRectangle.Right := StrToIntDef(FEditingText, 0);
    8: NodeData^.CompRec.ComponentRectangle.Bottom := StrToIntDef(FEditingText, 0);

    11: NodeData^.LocalX := StrToIntDef(FEditingText, 0);
    12: NodeData^.LocalY := StrToIntDef(FEditingText, 0);
    13: NodeData^.LocalX_FromParent := StrToIntDef(FEditingText, 0);
    14: NodeData^.LocalY_FromParent := StrToIntDef(FEditingText, 0);

    16: NodeData^.CompName := FEditingText;
  end;

  UpdateListOfScanedValues;
end;


procedure TfrClickerWinInterp.vstAvoidedZonesEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  NodeData: PAvoidedZoneRec;
begin
  if FMouseUpHitInfo_Settings.HitNode = nil then
    Exit;

  if not FUpdatedVstText then
    Exit;

  NodeData := vstAvoidedZones.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  case Column of
    0: NodeData^.ZRectStr.LeftOffset := FEditingText;
    1: NodeData^.ZRectStr.TopOffset := FEditingText;
    2: NodeData^.ZRectStr.RightOffset := FEditingText;
    3: NodeData^.ZRectStr.BottomOffset := FEditingText;
    4: NodeData^.ZName := FEditingText;
    5: NodeData^.ZRectStr.Left := FEditingText;
    6: NodeData^.ZRectStr.Top := FEditingText;
    7: NodeData^.ZRectStr.Right := FEditingText;
    8: NodeData^.ZRectStr.Bottom := FEditingText
  end;
end;


procedure TfrClickerWinInterp.vstComponentsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  NodeData: PHighlightedCompRec;
begin
  Allowed := Column in [0..3, 16];

  NodeData := vstComponents.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  if NodeData^.ManuallyAdded then
    Allowed := Allowed or (Column in [4..8, 11..14]);

  FUpdatedVstText := False;
end;


procedure TfrClickerWinInterp.vstAvoidedZonesEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  NodeData: PAvoidedZoneRec;
begin
  Allowed := True;

  NodeData := vstAvoidedZones.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  FUpdatedVstText := False;
end;


procedure TfrClickerWinInterp.vstComponentsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
begin
  FEditingText := NewText;
  FUpdatedVstText := True;
end;


procedure TfrClickerWinInterp.vstAvoidedZonesNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
begin
  FEditingText := NewText;
  FUpdatedVstText := True;
end;


procedure TfrClickerWinInterp.vstComponentsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
begin
  vstComponents.GetHitTestInfoAt(X, Y, True, FMouseUpHitInfo_Components);
  Node := vstComponents.GetFirstSelected;

  GenerateContent_AvgScreenshotAndGreenComp(Node);
  HighlightComponent(Node);
end;


procedure TfrClickerWinInterp.vstAvoidedZonesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
begin
  vstAvoidedZones.GetHitTestInfoAt(X, Y, True, FMouseUpHitInfo_Settings);
  Node := vstAvoidedZones.GetFirstSelected;

  HighlightZone(Node);
  DrawAvoidedZones;
end;


procedure TfrClickerWinInterp.vstAvoidedZonesPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  NodeData: PAvoidedZoneRec;
  VarName: string;
begin
  if not (Column in [5..8]) then
    Exit;

  NodeData := vstAvoidedZones.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  VarName := '';

  case Column of
    5: VarName := NodeData^.ZRectStr.Left;
    6: VarName := NodeData^.ZRectStr.Top;
    7: VarName := NodeData^.ZRectStr.Right;
    8: VarName := NodeData^.ZRectStr.Bottom;
  end;

  if FListOfScannedComponents.IndexOfName(VarName) = -1 then
    TargetCanvas.Font.Color := $4444FF;
end;


procedure TfrClickerWinInterp.FTransparent_LeftMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;

  GetCursorPos(FMouseDownGlobalPos);

  if not FSelectionHold then
  begin
    FMouseDownSelPos.X := FTransparent_SelectedComponentLeftLimitLabel.Left; //component coordinates on the window
    FSelectionHold := True;
  end;
end;


procedure TfrClickerWinInterp.FTransparent_LeftMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewLeft: Integer;
  CurrentLabel: TLabel;
  tp: TPoint;
begin
  GetCursorPos(tp);
  FCurrentMousePosOnPreviewImg.X := FTransparent_SelectedComponentLeftLimitLabel.Left + X + 3;
  FCurrentMousePosOnPreviewImg.Y := Y;
  tmrDrawZoom.Enabled := True;

  if not FSelectionHold then
    Exit;

  if Sender is TLabel then
  begin
    CurrentLabel := Sender as TLabel;
    NewLeft := FMouseDownSelPos.X + tp.X - FMouseDownGlobalPos.X;
    //if NewLeft <> CurrentLabel.Left then
    //  Modified := True;

    CurrentLabel.Left := Max(0, Min(FTransparent_SelectedComponentRightLimitLabel.Left - 8, NewLeft));
    FSelectedComponentLeftLimitLabel.Left := CurrentLabel.Left + 3;

    FCurrentMousePosOnPreviewImg.X := FSelectedComponentLeftLimitLabel.Left;
    FCurrentMousePosOnPreviewImg.Y := Y;
    tmrDrawZoom.Enabled := True;
  end;
end;


procedure TfrClickerWinInterp.FTransparent_LeftMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectionHold := False;
  FDraggingForSelection := False;
end;


procedure TfrClickerWinInterp.FTransparent_RightMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;

  GetCursorPos(FMouseDownGlobalPos);

  if not FSelectionHold then
  begin
    FMouseDownSelPos.X := FTransparent_SelectedComponentRightLimitLabel.Left; //component coordinates on the window
    FSelectionHold := True;
  end;
end;


procedure TfrClickerWinInterp.FTransparent_RightMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewLeft: Integer;
  CurrentLabel: TLabel;
  tp: TPoint;
begin
  GetCursorPos(tp);
  FCurrentMousePosOnPreviewImg.X := FTransparent_SelectedComponentRightLimitLabel.Left + X + 3;
  FCurrentMousePosOnPreviewImg.Y := Y;
  tmrDrawZoom.Enabled := True;

  if not FSelectionHold then
    Exit;

  if Sender is TLabel then
  begin
    CurrentLabel := Sender as TLabel;
    NewLeft := FMouseDownSelPos.X + tp.X - FMouseDownGlobalPos.X;
    //if NewLeft <> CurrentLabel.Left then
    //  Modified := True;

    CurrentLabel.Left := Max(FTransparent_SelectedComponentLeftLimitLabel.Left + 8, Min(imgLiveScreenshot.Width - 2, NewLeft));
    FSelectedComponentRightLimitLabel.Left := CurrentLabel.Left + 3;

    FCurrentMousePosOnPreviewImg.X := FSelectedComponentRightLimitLabel.Left;
    FCurrentMousePosOnPreviewImg.Y := Y;
    tmrDrawZoom.Enabled := True;
  end;
end;


procedure TfrClickerWinInterp.FTransparent_RightMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectionHold := False;
  FDraggingForSelection := False;
end;


procedure TfrClickerWinInterp.FTransparent_TopMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;

  GetCursorPos(FMouseDownGlobalPos);

  if not FSelectionHold then
  begin
    FMouseDownSelPos.Y := FTransparent_SelectedComponentTopLimitLabel.Top; //component coordinates on the window
    FSelectionHold := True;
  end;
end;


procedure TfrClickerWinInterp.FTransparent_TopMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewTop: Integer;
  CurrentLabel: TLabel;
  tp: TPoint;
begin
  GetCursorPos(tp);
  FCurrentMousePosOnPreviewImg.X := X;
  FCurrentMousePosOnPreviewImg.Y := FTransparent_SelectedComponentTopLimitLabel.Top + Y + 3;
  tmrDrawZoom.Enabled := True;

  if not FSelectionHold then
    Exit;

  if Sender is TLabel then
  begin
    CurrentLabel := Sender as TLabel;
    NewTop := FMouseDownSelPos.Y + tp.Y - FMouseDownGlobalPos.Y;
    //if NewTop <> CurrentLabel.Top then
    //  Modified := True;

    CurrentLabel.Top := Max(0, Min(FTransparent_SelectedComponentBottomLimitLabel.Top - 8, NewTop));
    FSelectedComponentTopLimitLabel.Top := CurrentLabel.Top + 3;

    FCurrentMousePosOnPreviewImg.X := X;
    FCurrentMousePosOnPreviewImg.Y := FSelectedComponentTopLimitLabel.Top;
    tmrDrawZoom.Enabled := True;
  end;
end;


procedure TfrClickerWinInterp.FTransparent_TopMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectionHold := False;
  FDraggingForSelection := False;
end;


procedure TfrClickerWinInterp.FTransparent_BottomMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;

  GetCursorPos(FMouseDownGlobalPos);

  if not FSelectionHold then
  begin
    FMouseDownSelPos.Y := FTransparent_SelectedComponentBottomLimitLabel.Top; //component coordinates on the window
    FSelectionHold := True;
  end;
end;


procedure TfrClickerWinInterp.FTransparent_BottomMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewTop: Integer;
  CurrentLabel: TLabel;
  tp: TPoint;
begin
  GetCursorPos(tp);
  FCurrentMousePosOnPreviewImg.X := X;
  FCurrentMousePosOnPreviewImg.Y := FTransparent_SelectedComponentBottomLimitLabel.Top + Y + 3;
  tmrDrawZoom.Enabled := True;

  if not FSelectionHold then
    Exit;

  if Sender is TLabel then
  begin
    CurrentLabel := Sender as TLabel;
    NewTop := FMouseDownSelPos.Y + tp.Y - FMouseDownGlobalPos.Y;
    //if NewTop <> CurrentLabel.Top then
    //  Modified := True;

    CurrentLabel.Top := Max(FTransparent_SelectedComponentTopLimitLabel.Top + 8, Min(imgLiveScreenshot.Height - 2, NewTop));
    FSelectedComponentBottomLimitLabel.Top := CurrentLabel.Top + 3;

    FCurrentMousePosOnPreviewImg.X := X;
    FCurrentMousePosOnPreviewImg.Y := FSelectedComponentBottomLimitLabel.Top;
    tmrDrawZoom.Enabled := True;
  end;
end;


procedure TfrClickerWinInterp.FTransparent_BottomMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectionHold := False;
  FDraggingForSelection := False;
end;

///


procedure TfrClickerWinInterp.FSelectionsLinesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectionHold := False;
  FDraggingForSelection := False;
end;

///


procedure TfrClickerWinInterp.FTransparentZone_LeftMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;

  GetCursorPos(FMouseDownGlobalPos);

  if not FSelectionHold then
  begin
    FMouseDownSelPos.X := FTransparent_SelectedZoneLeftLimitLabel.Left; //component coordinates on the window
    FSelectionHold := True;
  end;
end;


procedure TfrClickerWinInterp.FTransparentZone_LeftMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewLeft: Integer;
  CurrentLabel: TLabel;
  tp: TPoint;
begin
  GetCursorPos(tp);
  FCurrentMousePosOnPreviewImg.X := FTransparent_SelectedZoneLeftLimitLabel.Left + X + 3;
  FCurrentMousePosOnPreviewImg.Y := Y;
  tmrDrawZoom.Enabled := True;

  if not FSelectionHold then
    Exit;

  if Sender is TLabel then
  begin
    CurrentLabel := Sender as TLabel;
    NewLeft := FMouseDownSelPos.X + tp.X - FMouseDownGlobalPos.X;
    //if NewLeft <> CurrentLabel.Left then
    //  Modified := True;

    CurrentLabel.Left := Max(0, Min(FTransparent_SelectedZoneRightLimitLabel.Left - 4, NewLeft));
    FSelectedZoneLeftLimitLabel.Left := CurrentLabel.Left + 3;

    FCurrentMousePosOnPreviewImg.X := FSelectedZoneLeftLimitLabel.Left;
    FCurrentMousePosOnPreviewImg.Y := Y;
    tmrDrawZoom.Enabled := True;
  end;
end;


procedure TfrClickerWinInterp.FTransparentZone_LeftMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
  NodeData: PAvoidedZoneRec;
begin
  FSelectionHold := False;

  Node := vstAvoidedZones.GetFirstSelected;
  if Node = nil then
    Exit;

  NodeData := vstAvoidedZones.GetNodeData(Node);
  NodeData^.ZRectStr.LeftOffset := IntToStr(FSelectedZoneLeftLimitLabel.Left - StrToIntDef(EvaluateScanedComponents(NodeData^.ZRectStr.Left), 0));
  vstAvoidedZones.RepaintNode(Node);
  DrawAvoidedZones;
end;


procedure TfrClickerWinInterp.FTransparentZone_RightMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;

  GetCursorPos(FMouseDownGlobalPos);

  if not FSelectionHold then
  begin
    FMouseDownSelPos.X := FTransparent_SelectedZoneRightLimitLabel.Left; //component coordinates on the window
    FSelectionHold := True;
  end;
end;


procedure TfrClickerWinInterp.FTransparentZone_RightMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewLeft: Integer;
  CurrentLabel: TLabel;
  tp: TPoint;
begin
  GetCursorPos(tp);
  FCurrentMousePosOnPreviewImg.X := FTransparent_SelectedZoneRightLimitLabel.Left + X + 3;
  FCurrentMousePosOnPreviewImg.Y := Y;
  tmrDrawZoom.Enabled := True;

  if not FSelectionHold then
    Exit;

  if Sender is TLabel then
  begin
    CurrentLabel := Sender as TLabel;
    NewLeft := FMouseDownSelPos.X + tp.X - FMouseDownGlobalPos.X;
    //if NewLeft <> CurrentLabel.Left then
    //  Modified := True;

    CurrentLabel.Left := Max(FTransparent_SelectedZoneLeftLimitLabel.Left + 4, Min(imgLiveScreenshot.Width - 2, NewLeft));
    FSelectedZoneRightLimitLabel.Left := CurrentLabel.Left + 3;

    FCurrentMousePosOnPreviewImg.X := FSelectedZoneRightLimitLabel.Left;
    FCurrentMousePosOnPreviewImg.Y := Y;
    tmrDrawZoom.Enabled := True;
  end;
end;


procedure TfrClickerWinInterp.FTransparentZone_RightMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
  NodeData: PAvoidedZoneRec;
begin
  FSelectionHold := False;

  Node := vstAvoidedZones.GetFirstSelected;
  if Node = nil then
    Exit;

  NodeData := vstAvoidedZones.GetNodeData(Node);
  NodeData^.ZRectStr.RightOffset := IntToStr(FSelectedZoneRightLimitLabel.Left - StrToIntDef(EvaluateScanedComponents(NodeData^.ZRectStr.Right), 0));
  vstAvoidedZones.RepaintNode(Node);
  DrawAvoidedZones;
end;


procedure TfrClickerWinInterp.FTransparentZone_TopMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;

  GetCursorPos(FMouseDownGlobalPos);

  if not FSelectionHold then
  begin
    FMouseDownSelPos.Y := FTransparent_SelectedZoneTopLimitLabel.Top; //component coordinates on the window
    FSelectionHold := True;
  end;
end;


procedure TfrClickerWinInterp.FTransparentZone_TopMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewTop: Integer;
  CurrentLabel: TLabel;
  tp: TPoint;
begin
  GetCursorPos(tp);
  FCurrentMousePosOnPreviewImg.X := X;
  FCurrentMousePosOnPreviewImg.Y := FTransparent_SelectedZoneTopLimitLabel.Top + Y + 3;
  tmrDrawZoom.Enabled := True;

  if not FSelectionHold then
    Exit;

  if Sender is TLabel then
  begin
    CurrentLabel := Sender as TLabel;
    NewTop := FMouseDownSelPos.Y + tp.Y - FMouseDownGlobalPos.Y;
    //if NewTop <> CurrentLabel.Top then
    //  Modified := True;

    CurrentLabel.Top := Max(0, Min(FTransparent_SelectedZoneBottomLimitLabel.Top - 4, NewTop));
    FSelectedZoneTopLimitLabel.Top := CurrentLabel.Top + 3;

    FCurrentMousePosOnPreviewImg.X := X;
    FCurrentMousePosOnPreviewImg.Y := FSelectedZoneTopLimitLabel.Top;
    tmrDrawZoom.Enabled := True;
  end;
end;


procedure TfrClickerWinInterp.FTransparentZone_TopMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
  NodeData: PAvoidedZoneRec;
begin
  FSelectionHold := False;

  Node := vstAvoidedZones.GetFirstSelected;
  if Node = nil then
    Exit;

  NodeData := vstAvoidedZones.GetNodeData(Node);
  NodeData^.ZRectStr.TopOffset := IntToStr(FSelectedZoneTopLimitLabel.Top - StrToIntDef(EvaluateScanedComponents(NodeData^.ZRectStr.Top), 0));
  vstAvoidedZones.RepaintNode(Node);
  DrawAvoidedZones;
end;


procedure TfrClickerWinInterp.FTransparentZone_BottomMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;

  GetCursorPos(FMouseDownGlobalPos);

  if not FSelectionHold then
  begin
    FMouseDownSelPos.Y := FTransparent_SelectedZoneBottomLimitLabel.Top; //component coordinates on the window
    FSelectionHold := True;
  end;
end;


procedure TfrClickerWinInterp.FTransparentZone_BottomMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewTop: Integer;
  CurrentLabel: TLabel;
  tp: TPoint;
begin
  GetCursorPos(tp);
  FCurrentMousePosOnPreviewImg.X := X;
  FCurrentMousePosOnPreviewImg.Y := FTransparent_SelectedZoneBottomLimitLabel.Top + Y + 3;
  tmrDrawZoom.Enabled := True;

  if not FSelectionHold then
    Exit;

  if Sender is TLabel then
  begin
    CurrentLabel := Sender as TLabel;
    NewTop := FMouseDownSelPos.Y + tp.Y - FMouseDownGlobalPos.Y;
    //if NewTop <> CurrentLabel.Top then
    //  Modified := True;

    CurrentLabel.Top := Max(FTransparent_SelectedZoneTopLimitLabel.Top + 4, Min(imgLiveScreenshot.Height - 2, NewTop));
    FSelectedZoneBottomLimitLabel.Top := CurrentLabel.Top + 3;

    FCurrentMousePosOnPreviewImg.X := X;
    FCurrentMousePosOnPreviewImg.Y := FSelectedZoneBottomLimitLabel.Top;
    tmrDrawZoom.Enabled := True;
  end;
end;


procedure TfrClickerWinInterp.FTransparentZone_BottomMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
  NodeData: PAvoidedZoneRec;
begin
  FSelectionHold := False;

  Node := vstAvoidedZones.GetFirstSelected;
  if Node = nil then
    Exit;

  NodeData := vstAvoidedZones.GetNodeData(Node);
  NodeData^.ZRectStr.BottomOffset := IntToStr(FSelectedZoneBottomLimitLabel.Top - StrToIntDef(EvaluateScanedComponents(NodeData^.ZRectStr.Bottom), 0));
  vstAvoidedZones.RepaintNode(Node);
  DrawAvoidedZones;
end;

end.

