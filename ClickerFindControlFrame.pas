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


unit ClickerFindControlFrame;

{$H+}
{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, ComCtrls,
  Menus, Buttons, StdCtrls, ExtCtrls, Graphics,
  ClickerUtils, ClickerBMPTextFrame, ClickerPrimitivesFrame,
  InMemFileSystem, VirtualTrees;

type
  TOnUpdateBitmapAlgorithmSettings = procedure of object;

  //TOnSetPictureOpenDialogFileName = procedure(AFileName: string) of object;

  TOnExecuteFindSubControlAction = function(AErrorLevel, AErrorCount, AFastSearchErrorCount: Integer; out AFoundArea: TRect): Boolean of object; //maybe add some text settings (like list of font names)

  TfrClickerFindControl = class; //forward

  { TFontProfile }     //a wrapper over TfrClickerBMPText, to isolate visual components and expose their values as properties

  TFontProfile = class
  private
    FfrClickerBMPText: TfrClickerBMPText;
    FOwnerEditor: TfrClickerFindControl;
    FFindControlMatchBitmapText: PClkFindControlMatchBitmapText;

    function GetProfileName: string;
    procedure SetProfileName(Value: string);

    function GetEditorVisible: Boolean;
    procedure SetEditorVisible(Value: Boolean);

    function GetPreviewImageBitmap: TBitmap;

    function GetObjectName: string;
    procedure SetObjectName(Value: string);

    function GetMatchBitmapTextFGColor: string;
    procedure SetMatchBitmapTextFGColor(Value: string);

    function GetMatchBitmapTextBGColor: string;
    procedure SetMatchBitmapTextBGColor(Value: string);

    //function GetFGColor: TColor;
    //procedure SetFGColor(Value: TColor);
    //
    //function GetBGColor: TColor;
    //procedure SetBGColor(Value: TColor);

    function GetMatchBitmapTextFontName: string;
    procedure SetMatchBitmapTextFontName(Value: string);

    function GetMatchBitmapTextSize: string;
    procedure SetMatchBitmapTextSize(Value: string);

    function GetFontQualityReplacement: string;
    procedure SetFontQualityReplacement(Value: string);

    function GetMatchBitmapTextFontQualityIndex: Integer;
    procedure SetMatchBitmapTextFontQualityIndex(Value: Integer);

    function GetBold: Boolean;
    procedure SetBold(Value: Boolean);

    function GetItalic: Boolean;
    procedure SetItalic(Value: Boolean);

    function GetUnderline: Boolean;
    procedure SetUnderline(Value: Boolean);

    function GetStrikeOut: Boolean;
    procedure SetStrikeOut(Value: Boolean);

    function GetCropLeft: string;
    procedure SetCropLeft(Value: string);

    function GetCropTop: string;
    procedure SetCropTop(Value: string);

    function GetCropRight: string;
    procedure SetCropRight(Value: string);

    function GetCropBottom: string;
    procedure SetCropBottom(Value: string);

    function GetIgnoreBackgroundColor: Boolean;
    procedure SetIgnoreBackgroundColor(Value: Boolean);

    procedure SetShowCroppingLines(Value: Boolean);

    function CreateBMPTextFrame_NoContent(ANewName: string): TfrClickerBMPText;
  public
    constructor Create(AOwnerEditor: TfrClickerFindControl; ANewProfileName: string);
    destructor Destroy; override;

    procedure PreviewText;
    procedure PreviewTextOnImage(AImg: TImage);
    procedure UpdateSelectionLabelsFromCropEditBoxes;
    procedure UpdateSelectionLabelsFromCropInfo(var ABMPText: TClkFindControlMatchBitmapText);

    property ProfileName: string read GetProfileName write SetProfileName;
    property ObjectName: string read GetObjectName write SetObjectName;
    property EditorVisible: Boolean read GetEditorVisible write SetEditorVisible;
    property PreviewImageBitmap: TBitmap read GetPreviewImageBitmap;

    property MatchBitmapTextFGColor: string read GetMatchBitmapTextFGColor write SetMatchBitmapTextFGColor;
    property MatchBitmapTextBGColor: string read GetMatchBitmapTextBGColor write SetMatchBitmapTextBGColor;
    //property FGColor: TColor read GetFGColor write SetFGColor;   //The evaluated version of MatchBitmapTextFGColor. Kind of redundant, but it's a bit faster this way.
    //property BGColor: TColor read GetBGColor write SetBGColor;   //The evaluated version of MatchBitmapTextBGColor. Kind of redundant, but it's a bit faster this way.
    property MatchBitmapTextFontName: string read GetMatchBitmapTextFontName write SetMatchBitmapTextFontName;
    property MatchBitmapTextSize: string read GetMatchBitmapTextSize write SetMatchBitmapTextSize;
    property FontQualityReplacement: string read GetFontQualityReplacement write SetFontQualityReplacement;  //this can be one of the available font qualities or it can be var/replacement
    property MatchBitmapTextFontQualityIndex: Integer read GetMatchBitmapTextFontQualityIndex write SetMatchBitmapTextFontQualityIndex;
    property Bold: Boolean read GetBold write SetBold;
    property Italic: Boolean read GetItalic write SetItalic;
    property Underline: Boolean read GetUnderline write SetUnderline;
    property StrikeOut: Boolean read GetStrikeOut write SetStrikeOut;

    property CropLeft: string read GetCropLeft write SetCropLeft;
    property CropTop: string read GetCropTop write SetCropTop;
    property CropRight: string read GetCropRight write SetCropRight;
    property CropBottom: string read GetCropBottom write SetCropBottom;
    property IgnoreBackgroundColor: Boolean read GetIgnoreBackgroundColor write SetIgnoreBackgroundColor;

    property ShowCroppingLines: Boolean write SetShowCroppingLines;

    property FindControlMatchBitmapText: PClkFindControlMatchBitmapText write FFindControlMatchBitmapText; //must be set by owner
    property OwnerEditor: TfrClickerFindControl read FOwnerEditor;

    property frClickerBMPText: TfrClickerBMPText read FfrClickerBMPText;
  end;

  TFontProfileArr = array of TFontProfile;


  TPaintedLabel = class(TLabel)
  public
    procedure Paint; override;
  end;


  { TfrClickerFindControl }

  TfrClickerFindControl = class(TFrame)
    btnCopyFoundValues: TButton;
    btnDisplaySearchAreaDebuggingImage: TButton;
    chkIncludeSearchedBmpInZoom: TCheckBox;
    chkAutoCopyValuesToObjectInspector: TCheckBox;
    chkDisplayCroppingLines: TCheckBox;
    chkShowBMPFileDbgImg: TCheckBox;
    chkShowBMPTextDbgImg: TCheckBox;
    chkShowGridOnBMPPreview: TCheckBox;
    edtFoundControlInfo: TEdit;
    grpFindControlDetailsOnWindow: TGroupBox;
    imgCalcMinErrLevel: TImage;
    imgStopCalcMinErrLevel: TImage;
    imgCopySelAreaFromBkImg: TImage;
    imgCopyBMPImg: TImage;
    imgCopyTextImg: TImage;
    imgCopyBkAndBMPImg: TImage;
    imglstFindCriteria: TImageList;
    imglstMatchBitmapFiles: TImageList;
    imgDisplayExpectedFindLocation: TImage;
    imgUpdateLeftTopOffsets: TImage;
    imgCopyBkImg: TImage;
    imgUpdateLeftTopRightBottomOffsets: TImage;
    lblColorUnderCursor: TLabel;
    lblColorUnderCursorPreview: TLabel;
    lblPrimitivesInfo: TLabel;
    lbeFoundControlText: TLabeledEdit;
    lbeFoundControlClass: TLabeledEdit;
    lblMouseOnDbgImg: TLabel;
    lblMouseOnDbgImgBB: TLabel;
    lblMouseOnDbgImgGG: TLabel;
    lblMouseOnDbgImgRR: TLabel;
    lblPreviewControl_Height: TLabel;
    lblPreviewControl_Width: TLabel;
    lblReservedSpaceForDbgImg: TLabel;
    lstMatchBitmapFiles: TListBox;
    lstMatchPrimitiveFiles: TListBox;
    MenuItem_CopyTextAndClassFromRemoteScreenWindow: TMenuItem;
    MenuItem_CopyTextAndClassFromWinInterpWindow: TMenuItem;
    MenuItem_CopyTextAndClassFromPreviewWindow: TMenuItem;
    MenuItemControl_Bottom: TMenuItem;
    MenuItemControl_Left: TMenuItem;
    MenuItemControl_Right: TMenuItem;
    MenuItemControl_Top: TMenuItem;
    MenuItemCopyRefToClipboard: TMenuItem;
    MenuItemPasteRefFromClipboard: TMenuItem;
    N3: TMenuItem;
    PageControlMatch: TPageControl;
    pnlUseWholeScreen: TPanel;
    pnlDrag: TPanel;
    pmExtraCopyValueWindows: TPopupMenu;
    spdbtnDisplaySearchAreaDbgImgMenu: TSpeedButton;
    spdbtnExtraCopyValueWindows: TSpeedButton;
    tabctrlBMPText: TTabControl;
    TabSheetActionFindSubControlPrimitives: TTabSheet;
    TabSheetActionFindSubControlBMPText: TTabSheet;
    TabSheetActionFindSubControlSearchArea: TTabSheet;
    TabSheetActionFindSubControlText: TTabSheet;
    tmrBlinkCalcErrLevel: TTimer;
    tmrDrawZoom: TTimer;
    tmrUpdateGrid: TTimer;
    tmrUpdateSearchAreaOffsetEditBoxes: TTimer;
    procedure btnCopyFoundValuesClick(Sender: TObject);
    procedure chkDisplayCroppingLinesChange(Sender: TObject);
    procedure CopyTextAndClassFromPreviewWindowClick(Sender: TObject);
    procedure CopyTextAndClassFromRemoteScreenWindowClick(Sender: TObject);
    procedure CopyTextAndClassFromWinInterpWindowClick(Sender: TObject);
    procedure btnDisplaySearchAreaDebuggingImageClick(Sender: TObject);
    procedure chkShowBMPFileDbgImgClick(Sender: TObject);
    procedure chkShowBMPTextDbgImgClick(Sender: TObject);
    procedure chkShowGridOnBMPPreviewChange(Sender: TObject);
    procedure PageControlMatchChange(Sender: TObject);
    procedure pnlDragMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlDragMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlDragMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure spdbtnDisplaySearchAreaDbgImgMenuClick(Sender: TObject);
    procedure spdbtnExtraCopyValueWindowsClick(Sender: TObject);
    procedure tabctrlBMPTextChange(Sender: TObject);
    procedure tmrBlinkCalcErrLevelTimer(Sender: TObject);
    procedure tmrDrawZoomTimer(Sender: TObject);
    procedure tmrUpdateGridTimer(Sender: TObject);
    procedure tmrUpdateSearchAreaOffsetEditBoxesTimer(Sender: TObject);

  private
    FBMPsDir: string;
    FLastClickedLbe: TLabeledEdit;
    FDragging: Boolean;  //used for getting control handle

    FlbeSearchRectOffsetMDownInit: Integer;
    FlbeSearchRectOffsetMDownValueInit: Integer;
    FlbeSearchRectOffsetMDownSecondValueInit: Integer;

    FSearchAreaScrBox: TScrollBox;
    FSearchAreaControlDbgImg: TImage;
    FSearchAreaSearchedBmpDbgImg: TImage;
    FSearchAreaSearchedTextDbgImg: TImage;
    FSearchAreaGridImg: TImage;
    FSearchAreaDbgImgSearchedBmpMenu: TPopupMenu;
    FSearchAreaMenu: TPopupMenu;
    FSearchAreaOutOfImgImg: TImage;    //yellow image

    FSkipDrawingGrid: Boolean; //to be reset after use

    FSearchAreaLeftLimitLabel: TLabel;
    FSearchAreaTopLimitLabel: TLabel;
    FSearchAreaRightLimitLabel: TLabel;
    FSearchAreaBottomLimitLabel: TLabel;

    FTransparent_SearchAreaLeftLimitLabel: TLabel;
    FTransparent_SearchAreaTopLimitLabel: TLabel;
    FTransparent_SearchAreaRightLimitLabel: TLabel;
    FTransparent_SearchAreaBottomLimitLabel: TLabel;

    FSearchAreaLeftLimitLabel_ForMinErr: TPaintedLabel;
    FSearchAreaTopLimitLabel_ForMinErr: TPaintedLabel;
    FSearchAreaRightLimitLabel_ForMinErr: TPaintedLabel;
    FSearchAreaBottomLimitLabel_ForMinErr: TPaintedLabel;

    FSelectionHold: Boolean;
    //FMouseDownGlobalPos: TPoint;
    FMouseDownSelPos: TPoint;

    FMouseDownGlobalPos: TPoint;
    FMouseDownComponentPos: TPoint;
    FDbgImgHold: Boolean;
    FCurrentMousePosOnPreviewImg: TPoint;

    FRectangleSelecting: Boolean;
    FSelectingXStart: Integer;
    FSelectingYStart: Integer;

    FRectangleSelectingForMinErr: Boolean;

    FGridDrawingOption: TDisplayGridLineOption;
    FPreviewSelectionColors: TSelectionColors;

    FBMPTextProfiles: TFontProfileArr;
    FInMemFS: TInMemFileSystem; //not created in this unit, set from outside as an existing instance
    FfrClickerPrimitives: TfrClickerPrimitives;
    FGridLineOptionMenu: TPopupMenu;

    FExpectedErrLevel_TopLeft: TPoint;
    FExpectedErrLevel_BotRight: TPoint;
    FManuallyStopCalcErrLevel: Boolean;

    FOnTriggerOnControlsModified: TOnTriggerOnControlsModified;
    FOnEvaluateReplacements: TOnEvaluateReplacements;
    FOnReverseEvaluateReplacements: TOnReverseEvaluateReplacements;
    FOnUpdateBitmapAlgorithmSettings: TOnUpdateBitmapAlgorithmSettings;
    FOnCopyControlTextAndClassFromMainWindow: TOnCopyControlTextAndClassFromMainWindow;
    FOnGetExtraSearchAreaDebuggingImage: TOnGetExtraSearchAreaDebuggingImage;

    FOnLoadBitmap: TOnLoadBitmap;
    FOnFileExists: TOnFileExists;

    FOnSetPictureOpenDialogInitialDir: TOnSetPictureOpenDialogInitialDir;
    FOnPictureOpenDialogExecute: TOnPictureOpenDialogExecute;
    FOnGetPictureOpenDialogFileName: TOnGetPictureOpenDialogFileName;

    FOnUpdateSearchAreaLimitsInOIFromDraggingLines: TOnUpdateSearchAreaLimitsInOIFromDraggingLines;
    FOnUpdateTextCroppingLimitsInOIFromDraggingLines: TOnUpdateTextCroppingLimitsInOIFromDraggingLinesIdx;
    FOnGetDisplayedText: TOnGetDisplayedText;
    FOnSetMatchTextAndClassToOI: TOnSetMatchTextAndClassToOI;
    FOnGetUseWholeScreenAsSearchArea: TOnGetUseWholeScreenAsSearchArea;
    FOnGetFindControlOptions: TOnGetFindControlOptions;

    FOnExecuteFindSubControlAction: TOnExecuteFindSubControlAction;
    FOnAddToLog: TOnAddToLog;

    procedure imgSearchAreaControlDbgMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure imgSearchAreaControlDbgMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgSearchAreaControlDbgMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgSearchAreaControlDbgMouseEnter(Sender: TObject);
    procedure imgSearchAreaControlDbgMouseLeave(Sender: TObject);

    procedure imgSearchAreaSearchedBmpDbgImgMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure imgSearchAreaSearchedBmpDbgImgMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgSearchAreaSearchedBmpDbgImgMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgSearchAreaSearchedBmpDbgMouseEnter(Sender: TObject);
    procedure imgSearchAreaSearchedBmpDbgMouseLeave(Sender: TObject);

    procedure imgSearchAreaSearchedTextDbgImgMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure imgSearchAreaSearchedTextDbgImgMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgSearchAreaSearchedTextDbgImgMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgSearchAreaSearchedTextDbgMouseEnter(Sender: TObject);
    procedure imgSearchAreaSearchedTextDbgMouseLeave(Sender: TObject);

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

    procedure FSearchAreaControlDbgImgResize(Sender: TObject);

    procedure MenuItemCopySearchAreaBkImgToClipboardClick(Sender: TObject);
    procedure MenuItemCopySearchAreaSearchBmpImgToClipboardClick(Sender: TObject);
    procedure MenuItemCopySearchAreaSearchTextImgToClipboardClick(Sender: TObject);

    procedure MenuItemCopySearchAreaAllToClipboardClick(Sender: TObject);
    procedure MenuItemCopySearchAreaSelectedAreaFromBkToClipboardClick(Sender: TObject);
    procedure MenuItemUpdateLeftAndTopOffsetsFromPreviewTextImageToEditboxes(Sender: TObject);
    procedure MenuItemUpdateLeftTopRightBottomOffsetsFromPreviewTextImageToEditboxes(Sender: TObject);
    procedure MenuItemCalculateMinimumErrorLevelToMatchText(Sender: TObject);
    procedure MenuItemCalculateMinimumColorErrorCountToMatchText(Sender: TObject);
    procedure MenuItemStopCalculatingMinimumErrorLevelToMatchText(Sender: TObject);
    procedure MenuItemDisplaySelectionLinesForExpectedFindLocation(Sender: TObject);
    procedure MenuItemHideSelectionLinesForExpectedFindLocation(Sender: TObject);

    procedure MenuItemLoadBmpTextToSearchedAreaClick(Sender: TObject);
    procedure MenuItemUnloadBmpTextFromSearchedAreaClick(Sender: TObject);
    procedure MenuItemGenericLoadBmpToSearchedAreaClick(Sender: TObject);

    procedure FSearchAreaScrBoxMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MenuSetGridType(Sender: TObject);

    function GetFontProfile(Value: Integer): TFontProfile;

    procedure CreateRemainingUIComponents;
    procedure CreateGridLineOptionMenu;
    procedure DoOnTriggerOnControlsModified;
    function DoOnGetExtraSearchAreaDebuggingImage(AExtraBitmap: TBitmap): Boolean;

    function DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function DoOnFileExists(const AFileName: string): Boolean;

    procedure DoOnSetPictureOpenDialogInitialDir(AInitialDir: string);
    function DoOnPictureOpenDialogExecute: Boolean;
    function DoOnGetPictureOpenDialogFileName: string;

    procedure DoOnUpdateSearchAreaLimitsInOIFromDraggingLines(ALimitLabelsToUpdate: TLimitLabels; var AOffsets: TSimpleRectString);
    procedure DoOnUpdateTextCroppingLimitsInOIFromDraggingLines(ALimitLabelsToUpdate: TLimitLabels; var AOffsets: TSimpleRectString; AFontProfileIndex: Integer); //called by a handler for BMPTextFrame
    function DoOnGetDisplayedText: string;
    procedure DoOnSetMatchTextAndClassToOI(AMatchText, AMatchClassName: string);
    function DoOnGetFindControlOptions: PClkFindControlOptions;

    function DoOnExecuteFindSubControlAction(AErrorLevel, AErrorCount, AFastSearchErrorCount: Integer; out AFoundArea: TRect): Boolean;
    procedure DoOnAddToLog(s: string);

    procedure HandleMatchTextClick;
    procedure GeneratePreviewGridContent(ADisplayGridLineOption: TDisplayGridLineOption);

    procedure AddFontProfile(AProfileName: string);

    procedure SelectDbgImgByRectangle(X, Y: Integer);
    procedure SelectDbgImgByRectangleForMinErr(X, Y: Integer);
    procedure DisplaySelectionLinesForExpectedFindLocation;
    procedure HideSelectionLinesForExpectedFindLocation;

    function HandleBMPTextOnGetDisplayedText: string;
    procedure HandleBMPTextOnTriggerOnControlsModified;
    function HandleBMPTextOnEvaluateReplacements(s: string): string;
    procedure HandleBMPTextOnSetCroppingValuesToOtherFontProfiles(ACropLeft, ACropTop, ACropRight, ACropBottom: string; ASkipProfileIndex: Integer);
    function HandleBMPTextOnGetCroppingLinesVisiblity: Boolean;
    procedure HandleOnUpdateTextCroppingLimitsInOIFromDraggingLines(ALimitLabelsToUpdate: TLimitLabels; var AOffsets: TSimpleRectString; AFontProfileName: string);
    function HandleOnGetFindControlMatchBitmapText(Sender: TObject): PClkFindControlMatchBitmapText;

    function GetSearch_EditBoxVar_Ref(AEditBoxValue, AVarName: string): Integer;

    function GetSearch_LeftLeft_Ref: Integer;    //Left
    function GetSearch_RightLeft_Ref: Integer;

    function GetSearch_RightRight_Ref: Integer;  //Right
    function GetSearch_LeftRight_Ref: Integer;

    function GetSearch_TopTop_Ref: Integer;      //Top
    function GetSearch_BottomTop_Ref: Integer;

    function GetSearch_BottomBottom_Ref: Integer;//Bottom
    function GetSearch_TopBottom_Ref: Integer;

    function GetSelectedBMPTextTab: Integer;
    procedure SetSelectedBMPTextTab(Value: Integer);

    procedure SetGridDrawingOption(Value: TDisplayGridLineOption);
    procedure SetPreviewSelectionColors(Value: TSelectionColors);

    ///////OI
    function GetSearch_LeftLeft_Ref_FromInitRect(AInitialRectange: TRectString): Integer;    //Left
    function GetSearch_RightLeft_Ref_FromInitRect(AInitialRectange: TRectString): Integer;

    function GetSearch_RightRight_Ref_FromInitRect(AInitialRectange: TRectString): Integer;  //Right
    function GetSearch_LeftRight_Ref_FromInitRect(AInitialRectange: TRectString): Integer;

    function GetSearch_TopTop_Ref_FromInitRect(AInitialRectange: TRectString): Integer;      //Top
    function GetSearch_BottomTop_Ref_FromInitRect(AInitialRectange: TRectString): Integer;

    function GetSearch_BottomBottom_Ref_FromInitRect(AInitialRectange: TRectString): Integer;//Bottom
    function GetSearch_TopBottom_Ref_FromInitRect(AInitialRectange: TRectString): Integer;

    function EvaluateReplacements(s: string): string;
    function ReverseEvaluateReplacements(s: string): string;

    function GetControlWidthFromReplacement: Integer;
    function GetControlHeightFromReplacement: Integer;

    procedure GetOffsetArea(out CropLeft, CropRight, CropTop, CropBottom: Integer);

    function GetSearchAreaLeftOffsetFromBmpDbgImg: Integer;
    function GetSearchAreaTopOffsetFromBmpDbgImg: Integer;
    function GetSearchAreaRightOffsetFromBmpDbgImg: Integer;
    function GetSearchAreaBottomOffsetFromBmpDbgImg: Integer;

    function GetSearchAreaLeftOffsetFromTxtDbgImg: Integer;
    function GetSearchAreaTopOffsetFromTxtDbgImg: Integer;
    function GetSearchAreaRightOffsetFromTxtDbgImg: Integer;
    function GetSearchAreaBottomOffsetFromTxtDbgImg: Integer;

    function GetSearchAreaLeftOffsetFromSelLabel: Integer;
    function GetSearchAreaTopOffsetFromSelLabel: Integer;
    function GetSearchAreaRightOffsetFromSelLabel: Integer;
    function GetSearchAreaBottomOffsetFromSelLabel: Integer;

    procedure UpdateSearchAreaLabelsFromInitRect(AInitialRectange: TRectString);
    procedure UpdateSearchAreaLabelColorsFromTheirPosition;
    procedure UpdateTransparent_SearchAreaLimitsFromSearchAreaLimits;
    procedure SetLabelsFromMouseOverDbgImgPixelColor(APixelColor: TColor);
    procedure CopyTextAndClassFromExternalProvider(AProviderName: string);

    procedure UpdateAllSelectionLabelsFromCropEditBoxes;
    procedure UpdateSearchAreaSearchedTextAndLabels;
    procedure PopulateDbgImgExtraMenu;
  public
    //FBMPTextFrames: TfrClickerBMPTextArr; //should eventually made private and accesed through functions

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    //procedure AddDefaultFontProfile;
    procedure CreateBMPTextFrames(ACount: Integer);
    function GetBMPTextFontProfilesCount: Integer;
    procedure SetBMPTextFrameVisibility;
    procedure CreateClickerPrimitivesFrame;

    procedure UpdateSearchAreaLabelsFromKeysOnInitRect(AInitialRectange: TRectString);  //must be called on OI Text editor - KeyUp
    procedure UpdateControlWidthHeightLabels;
    procedure UpdateUseWholeScreenLabel(AUseWholeScreen: Boolean);
    procedure ClearControls;
    procedure UpdateBitmapAlgorithmSettings;
    procedure UpdatePreviewIcons;
    procedure UpdateListsOfSearchFiles(AMatchBitmapFiles, AMatchPrimitiveFiles: string);

    procedure CreateTransparentSelectionLabels;
    procedure CreateSelectionLabels(var ALeftLabel, ATopLabel, ARightLabel, ABottomLabel: TLabel; ALeftColor, ATopColor, ARightColor, ABottomColor: TColor; AShouldBringToFront, ACreateWithPaintedLabel: Boolean);
    procedure DisplayDebuggingImage;
    procedure PreviewText; //called by ExecuteAction
    procedure RefreshGrid;

    procedure UpdateOnSearchRectLeftOffsetMouseDown(var InitialRectange: TRectString; AEditBox: TVTEdit; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UpdateOnSearchRectLeftOffsetMouseMove(var InitialRectange: TRectString; AEditBox: TVTEdit; Shift: TShiftState; X, Y: Integer);
    procedure UpdateOnSearchRectTopOffsetMouseDown(var InitialRectange: TRectString; AEditBox: TVTEdit; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UpdateOnSearchRectTopOffsetMouseMove(var InitialRectange: TRectString; AEditBox: TVTEdit; Shift: TShiftState; X, Y: Integer);
    procedure UpdateOnSearchRectRightOffsetMouseDown(var InitialRectange: TRectString; AEditBox: TVTEdit; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UpdateOnSearchRectRightOffsetMouseMove(var InitialRectange: TRectString; AEditBox: TVTEdit; Shift: TShiftState; X, Y: Integer);
    procedure UpdateOnSearchRectBottomOffsetMouseDown(var InitialRectange: TRectString; AEditBox: TVTEdit; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UpdateOnSearchRectBottomOffsetMouseMove(var InitialRectange: TRectString; AEditBox: TVTEdit; Shift: TShiftState; X, Y: Integer);

    procedure UpdateOnTextCroppingLeftMouseDown(var AMatchBMP: TClkFindControlMatchBitmapText; AEditBox: TVTEdit; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UpdateOnTextCroppingLeftMouseMove(var AMatchBMP: TClkFindControlMatchBitmapText; AEditBox: TVTEdit; Shift: TShiftState; X, Y, AProfileIndex: Integer);
    procedure UpdateOnTextCroppingTopMouseDown(var AMatchBMP: TClkFindControlMatchBitmapText; AEditBox: TVTEdit; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UpdateOnTextCroppingTopMouseMove(var AMatchBMP: TClkFindControlMatchBitmapText; AEditBox: TVTEdit; Shift: TShiftState; X, Y, AProfileIndex: Integer);
    procedure UpdateOnTextCroppingRightMouseDown(var AMatchBMP: TClkFindControlMatchBitmapText; AEditBox: TVTEdit; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UpdateOnTextCroppingRightMouseMove(var AMatchBMP: TClkFindControlMatchBitmapText; AEditBox: TVTEdit; Shift: TShiftState; X, Y, AProfileIndex: Integer);
    procedure UpdateOnTextCroppingBottomMouseDown(var AMatchBMP: TClkFindControlMatchBitmapText; AEditBox: TVTEdit; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UpdateOnTextCroppingBottomMouseMove(var AMatchBMP: TClkFindControlMatchBitmapText; AEditBox: TVTEdit; Shift: TShiftState; X, Y, AProfileIndex: Integer);

    procedure AddNewFontProfile(ANewProfile: TClkFindControlMatchBitmapText);
    procedure UpdateFontProfileName(AProfileIndex: Integer; ANewName: string);
    procedure RemoveFontProfileByIndex(AIndex: Integer);
    function GetFontProfileIndexByName(AProfileName: string): Integer;

    property BMPsDir: string read FBMPsDir write FBMPsDir;
    property BMPTextFontProfiles[Index: Integer]: TFontProfile read GetFontProfile;
    property SelectedBMPTextTab: Integer read GetSelectedBMPTextTab write SetSelectedBMPTextTab;

    property InMemFS: TInMemFileSystem read FInMemFS write FInMemFS;
    property SearchAreaControlDbgImg: TImage read FSearchAreaControlDbgImg;
    property frClickerPrimitives: TfrClickerPrimitives read FfrClickerPrimitives;
    property GridDrawingOption: TDisplayGridLineOption read FGridDrawingOption write SetGridDrawingOption;
    property PreviewSelectionColors: TSelectionColors read FPreviewSelectionColors write SetPreviewSelectionColors;

    property OnTriggerOnControlsModified: TOnTriggerOnControlsModified read FOnTriggerOnControlsModified write FOnTriggerOnControlsModified;
    property OnEvaluateReplacements: TOnEvaluateReplacements read FOnEvaluateReplacements write FOnEvaluateReplacements;
    property OnReverseEvaluateReplacements: TOnReverseEvaluateReplacements read FOnReverseEvaluateReplacements write FOnReverseEvaluateReplacements;
    property OnUpdateBitmapAlgorithmSettings: TOnUpdateBitmapAlgorithmSettings read FOnUpdateBitmapAlgorithmSettings write FOnUpdateBitmapAlgorithmSettings;
    property OnCopyControlTextAndClassFromMainWindow: TOnCopyControlTextAndClassFromMainWindow read FOnCopyControlTextAndClassFromMainWindow write FOnCopyControlTextAndClassFromMainWindow;
    property OnGetExtraSearchAreaDebuggingImage: TOnGetExtraSearchAreaDebuggingImage write FOnGetExtraSearchAreaDebuggingImage;

    property OnLoadBitmap: TOnLoadBitmap write FOnLoadBitmap;
    property OnFileExists: TOnFileExists write FOnFileExists;
    property OnSetPictureOpenDialogInitialDir: TOnSetPictureOpenDialogInitialDir write FOnSetPictureOpenDialogInitialDir;
    property OnPictureOpenDialogExecute: TOnPictureOpenDialogExecute write FOnPictureOpenDialogExecute;
    property OnGetPictureOpenDialogFileName: TOnGetPictureOpenDialogFileName write FOnGetPictureOpenDialogFileName;

    property OnUpdateSearchAreaLimitsInOIFromDraggingLines: TOnUpdateSearchAreaLimitsInOIFromDraggingLines write FOnUpdateSearchAreaLimitsInOIFromDraggingLines;
    property OnUpdateTextCroppingLimitsInOIFromDraggingLines: TOnUpdateTextCroppingLimitsInOIFromDraggingLinesIdx write FOnUpdateTextCroppingLimitsInOIFromDraggingLines;
    property OnGetDisplayedText: TOnGetDisplayedText write FOnGetDisplayedText;
    property OnSetMatchTextAndClassToOI: TOnSetMatchTextAndClassToOI write FOnSetMatchTextAndClassToOI;
    property OnGetFindControlOptions: TOnGetFindControlOptions write FOnGetFindControlOptions;

    property OnExecuteFindSubControlAction: TOnExecuteFindSubControlAction write FOnExecuteFindSubControlAction;   //used on finding error level only (not on all FindSubControl actions)
    property OnAddToLog: TOnAddToLog write FOnAddToLog;
  end;

const
  CPreviewWindow = 'Preview';
  CWinInterpWindow = 'WinInterp';
  CRemoteScreenWindow = 'RemoteScreen';


implementation

{$R *.frm}

uses
  BitmapProcessing, Clipbrd, ClickerZoomPreviewForm;


//const
//  {$IFDEF FPC}
//    ID_YES = IDYES;  //from Delphi
//  {$ENDIF}


{ TFontProfile }

constructor TFontProfile.Create(AOwnerEditor: TfrClickerFindControl; ANewProfileName: string);
begin
  inherited Create;
  FOwnerEditor := AOwnerEditor;
  FfrClickerBMPText := CreateBMPTextFrame_NoContent(ANewProfileName);
end;


destructor TFontProfile.Destroy;
begin
  FreeAndNil(FfrClickerBMPText); //it is possible that this call should be made only when FfrClickerBMPText is created without an owner
  inherited Destroy;
end;


function TFontProfile.CreateBMPTextFrame_NoContent(ANewName: string): TfrClickerBMPText;
begin
  Result := TfrClickerBMPText.Create(FOwnerEditor);
  Result.Name := ANewName;

  Result.Left := 2;
  Result.Top := 21;
  Result.Parent := FOwnerEditor.tabctrlBMPText; //TabSheetActionFindSubControlBMPText;
  Result.Visible := False;
  Result.OnGetDisplayedText := FOwnerEditor.HandleBMPTextOnGetDisplayedText;
  Result.OnTriggerOnControlsModified := FOwnerEditor.HandleBMPTextOnTriggerOnControlsModified;
  Result.OnEvaluateReplacements := FOwnerEditor.HandleBMPTextOnEvaluateReplacements;
  Result.OnSetCroppingValuesToOtherFontProfiles := FOwnerEditor.HandleBMPTextOnSetCroppingValuesToOtherFontProfiles;
  Result.OnGetCroppingLinesVisiblity := FOwnerEditor.HandleBMPTextOnGetCroppingLinesVisiblity;
  Result.OnUpdateTextCroppingLimitsInOIFromDraggingLines := FOwnerEditor.HandleOnUpdateTextCroppingLimitsInOIFromDraggingLines;
  Result.OnGetFindControlMatchBitmapText := FOwnerEditor.HandleOnGetFindControlMatchBitmapText;
end;


function TFontProfile.GetProfileName: string;
begin
  Result := FfrClickerBMPText.ProfileName;
end;


procedure TFontProfile.SetProfileName(Value: string);
begin
  FfrClickerBMPText.ProfileName := Value
end;


function TFontProfile.GetEditorVisible: Boolean;
begin
  Result := FfrClickerBMPText.Visible;
end;


procedure TFontProfile.SetEditorVisible(Value: Boolean);
begin
  FfrClickerBMPText.Visible := Value;
end;


function TFontProfile.GetPreviewImageBitmap: TBitmap;
begin
  Result := FfrClickerBMPText.imgPreview.Picture.Bitmap;
end;


function TFontProfile.GetObjectName: string;
begin
  Result := FfrClickerBMPText.Name;
end;


procedure TFontProfile.SetObjectName(Value: string);
begin
  FfrClickerBMPText.Name := Value;
end;


function TFontProfile.GetMatchBitmapTextFGColor: string;
begin
  Result := FFindControlMatchBitmapText^.ForegroundColor;
end;


procedure TFontProfile.SetMatchBitmapTextFGColor(Value: string);
begin
  FFindControlMatchBitmapText^.ForegroundColor := Value;
end;


function TFontProfile.GetMatchBitmapTextBGColor: string;
begin
  Result := FFindControlMatchBitmapText^.BackgroundColor;
end;


procedure TFontProfile.SetMatchBitmapTextBGColor(Value: string);
begin
  FFindControlMatchBitmapText^.BackgroundColor := Value;
end;


//function TFontProfile.GetFGColor: TColor;
//begin
//  Result := FfrClickerBMPText.pnlFG.Color;
//end;
//
//
//procedure TFontProfile.SetFGColor(Value: TColor);
//begin
//  FfrClickerBMPText.pnlFG.Color := Value;
//end;
//
//
//function TFontProfile.GetBGColor: TColor;
//begin
//  Result := FfrClickerBMPText.pnlBG.Color;
//end;
//
//
//procedure TFontProfile.SetBGColor(Value: TColor);
//begin
//  FfrClickerBMPText.pnlBG.Color := Value;
//end;


function TFontProfile.GetMatchBitmapTextFontName: string;
begin
  Result := FFindControlMatchBitmapText^.FontName;
end;


procedure TFontProfile.SetMatchBitmapTextFontName(Value: string);
begin
  FFindControlMatchBitmapText^.FontName := Value;
end;


function TFontProfile.GetMatchBitmapTextSize: string;
begin
  Result := IntToStr(FFindControlMatchBitmapText^.FontSize);
end;


procedure TFontProfile.SetMatchBitmapTextSize(Value: string);
begin
  FFindControlMatchBitmapText^.FontSize := StrToIntDef(Value, 8);
end;


function TFontProfile.GetFontQualityReplacement: string;
begin
  Result := FFindControlMatchBitmapText^.FontQualityReplacement;
end;


procedure TFontProfile.SetFontQualityReplacement(Value: string);
begin
  FFindControlMatchBitmapText^.FontQualityReplacement := Value
end;


function TFontProfile.GetMatchBitmapTextFontQualityIndex: Integer;
begin
  Result := Ord(FFindControlMatchBitmapText^.FontQuality);
end;


procedure TFontProfile.SetMatchBitmapTextFontQualityIndex(Value: Integer);
begin
  if Value > Ord(High(TFontQuality)) then
  begin
    Value := Ord(High(TFontQuality));
    FFindControlMatchBitmapText^.FontQualityUsesReplacement := True;
  end
  else
    FFindControlMatchBitmapText^.FontQualityUsesReplacement := False;
end;


function TFontProfile.GetBold: Boolean;
begin
  Result := FFindControlMatchBitmapText^.Bold;
end;


procedure TFontProfile.SetBold(Value: Boolean);
begin
  FFindControlMatchBitmapText^.Bold := Value;
end;


function TFontProfile.GetItalic: Boolean;
begin
  Result := FFindControlMatchBitmapText^.Italic;
end;


procedure TFontProfile.SetItalic(Value: Boolean);
begin
  FFindControlMatchBitmapText^.Italic := Value;
end;


function TFontProfile.GetUnderline: Boolean;
begin
  Result := FFindControlMatchBitmapText^.Underline;
end;


procedure TFontProfile.SetUnderline(Value: Boolean);
begin
  FFindControlMatchBitmapText^.Underline := Value;
end;


function TFontProfile.GetStrikeOut: Boolean;
begin
  Result := FFindControlMatchBitmapText^.StrikeOut;
end;


procedure TFontProfile.SetStrikeOut(Value: Boolean);
begin
  FFindControlMatchBitmapText^.StrikeOut := Value;
end;


function TFontProfile.GetCropLeft: string;
begin
  Result := FfrClickerBMPText.MatchBitmapTextCropLeft;
end;


procedure TFontProfile.SetCropLeft(Value: string);
begin
  FfrClickerBMPText.MatchBitmapTextCropLeft := Value;
end;


function TFontProfile.GetCropTop: string;
begin
  Result := FfrClickerBMPText.MatchBitmapTextCropTop;
end;


procedure TFontProfile.SetCropTop(Value: string);
begin
  FfrClickerBMPText.MatchBitmapTextCropTop := Value;
end;


function TFontProfile.GetCropRight: string;
begin
  Result := FfrClickerBMPText.MatchBitmapTextCropRight;
end;


procedure TFontProfile.SetCropRight(Value: string);
begin
  FfrClickerBMPText.MatchBitmapTextCropRight := Value;
end;


function TFontProfile.GetCropBottom: string;
begin
  Result := FfrClickerBMPText.MatchBitmapTextCropBottom;
end;


procedure TFontProfile.SetCropBottom(Value: string);
begin
  FfrClickerBMPText.MatchBitmapTextCropBottom := Value;
end;


function TFontProfile.GetIgnoreBackgroundColor: Boolean;
begin
  Result := FFindControlMatchBitmapText^.IgnoreBackgroundColor;
end;


procedure TFontProfile.SetIgnoreBackgroundColor(Value: Boolean);
begin
  FFindControlMatchBitmapText^.IgnoreBackgroundColor := Value;
end;


procedure TFontProfile.SetShowCroppingLines(Value: Boolean);
begin
  FfrClickerBMPText.DisplayCroppingLines(Value);
end;


procedure TFontProfile.PreviewText;
begin
  FfrClickerBMPText.PreviewText;
end;


procedure TFontProfile.PreviewTextOnImage(AImg: TImage);
begin
  FfrClickerBMPText.PreviewTextOnImage(AImg);
end;


procedure TFontProfile.UpdateSelectionLabelsFromCropEditBoxes;
begin
  FfrClickerBMPText.UpdateSelectionLabelsFromCropEditBoxes;
end;


procedure TFontProfile.UpdateSelectionLabelsFromCropInfo(var ABMPText: TClkFindControlMatchBitmapText);
begin
  FfrClickerBMPText.UpdateSelectionLabelsFromCropInfo(ABMPText);
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


{ TfrClickerFindControl }


function TfrClickerFindControl.GetFontProfile(Value: Integer): TFontProfile;
begin
  if (Value < 0) or (Value > Length(FBMPTextProfiles) - 1) then
    raise Exception.Create('Indexing font profile out of bounds: ' + IntToStr(Value));

  Result := FBMPTextProfiles[Value];
end;


procedure TfrClickerFindControl.CreateRemainingUIComponents;
begin

end;


procedure TfrClickerFindControl.CreateGridLineOptionMenu;
var
  i: TDisplayGridLineOption;
  TempMenuItem: TMenuItem;
begin
  FGridLineOptionMenu := TPopupMenu.Create(Self);

  for i := Low(TDisplayGridLineOption) to High(TDisplayGridLineOption) do
  begin
    TempMenuItem := TMenuItem.Create(Self);
    TempMenuItem.Caption := CDisplayGridLineOptionStr[i];
    TempMenuItem.Tag := Ord(i);
    TempMenuItem.OnClick := MenuSetGridType;

    FGridLineOptionMenu.Items.Add(TempMenuItem);
  end;

  chkShowGridOnBMPPreview.PopupMenu := FGridLineOptionMenu;
end;


constructor TfrClickerFindControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOnTriggerOnControlsModified := nil;
  FOnEvaluateReplacements := nil;
  FOnReverseEvaluateReplacements := nil;
  FOnUpdateBitmapAlgorithmSettings := nil;
  FOnCopyControlTextAndClassFromMainWindow := nil;
  FOnGetExtraSearchAreaDebuggingImage := nil;

  FOnLoadBitmap := nil;
  FOnFileExists := nil;
  FOnSetPictureOpenDialogInitialDir := nil;
  FOnPictureOpenDialogExecute := nil;
  FOnGetPictureOpenDialogFileName := nil;

  FOnUpdateSearchAreaLimitsInOIFromDraggingLines := nil;
  FOnUpdateTextCroppingLimitsInOIFromDraggingLines := nil;
  FOnGetDisplayedText := nil;
  FOnSetMatchTextAndClassToOI := nil;
  FOnGetUseWholeScreenAsSearchArea := nil;
  FOnGetFindControlOptions := nil;

  FOnExecuteFindSubControlAction := nil;
  FOnAddToLog := nil;

  CreateRemainingUIComponents; //this should be called after initializing callback properties to nil  (like FOnTriggerOnControlsModified)
  CreateGridLineOptionMenu;
  SetLength(FBMPTextProfiles, 0);
  //CreateSelectionLabels is called where all the other labels are created

  FSearchAreaScrBox := nil;
  FSearchAreaControlDbgImg := nil;
  FSearchAreaSearchedBmpDbgImg := nil;
  FSearchAreaSearchedTextDbgImg := nil;
  FSearchAreaOutOfImgImg := nil;
  FSkipDrawingGrid := False;

  FLastClickedLbe := nil;
  FDbgImgHold := False;
  FRectangleSelecting := False;
  FRectangleSelectingForMinErr := False;
  FDragging := False;
  FInMemFS := nil;

  FGridDrawingOption := loDot;

  FPreviewSelectionColors.TopLeft_Valid := CLabel_Orange;
  FPreviewSelectionColors.BotRight_Valid := CLabel_LightGreen;
  FPreviewSelectionColors.TopLeft_Invalid := clRed;
  FPreviewSelectionColors.BotRight_Invalid := clMaroon;

  FManuallyStopCalcErrLevel := False;
  FExpectedErrLevel_TopLeft.X := -1;
  FExpectedErrLevel_TopLeft.Y := -1;
  FExpectedErrLevel_BotRight.X := -1;
  FExpectedErrLevel_BotRight.Y := -1;

  PageControlMatch.ActivePageIndex := 0;
end;


destructor TfrClickerFindControl.Destroy;
var
  i: Integer;
begin
  {for i := 0 to Length(FBMPTextFrames) - 1 do
    FreeAndNil(FBMPTextFrames[i]);}                //do not call these, because the frames will be automatically destroyed, as they were created with a parent

  for i := 0 to Length(FBMPTextProfiles) - 1 do
    FreeAndNil(FBMPTextProfiles[i]);  //font profiles should always be freed manually, as opposed to  FBMPTextFrames

  SetLength(FBMPTextProfiles, 0);

  inherited Destroy;
end;


//procedure TfrClickerFindControl.AddDefaultFontProfile;
//begin
//  AddFontProfile(CDefaultFontProfileName);
//end;


procedure TfrClickerFindControl.DoOnTriggerOnControlsModified;
begin
  if Assigned(FOnTriggerOnControlsModified) then
    FOnTriggerOnControlsModified()
  else
    raise Exception.Create('OnTriggerOnControlsModified not assigned.');
end;


function TfrClickerFindControl.DoOnGetExtraSearchAreaDebuggingImage(AExtraBitmap: TBitmap): Boolean;
begin
  Result := False;
  if not Assigned(FOnGetExtraSearchAreaDebuggingImage) then
    Exit;

  Result := FOnGetExtraSearchAreaDebuggingImage(AExtraBitmap);
end;


function TfrClickerFindControl.DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if not Assigned(FOnLoadBitmap) then
    raise Exception.Create('OnLoadBitmap not assigned.')
  else
    Result := FOnLoadBitmap(ABitmap, AFileName);
end;


function TfrClickerFindControl.DoOnFileExists(const AFileName: string): Boolean;
begin
  if not Assigned(FOnFileExists) then
    raise Exception.Create('OnFileExists is not assigned.')
  else
    Result := FOnFileExists(AFileName);
end;


procedure TfrClickerFindControl.DoOnSetPictureOpenDialogInitialDir(AInitialDir: string);
begin
  if not Assigned(FOnSetPictureOpenDialogInitialDir) then
    raise Exception.Create('OnSetPictureOpenDialogInitialDir not assigned.')
  else
    FOnSetPictureOpenDialogInitialDir(AInitialDir);
end;


function TfrClickerFindControl.DoOnPictureOpenDialogExecute: Boolean;
begin
  if not Assigned(FOnPictureOpenDialogExecute) then
    raise Exception.Create('OnPictureOpenDialogExecute not assigned.')
  else
    Result := FOnPictureOpenDialogExecute;
end;


function TfrClickerFindControl.DoOnGetPictureOpenDialogFileName: string;
begin
  if not Assigned(FOnGetPictureOpenDialogFileName) then
    raise Exception.Create('OnGetPictureOpenDialogFileName not assigned.')
  else
    Result := FOnGetPictureOpenDialogFileName;
end;


procedure TfrClickerFindControl.DoOnUpdateSearchAreaLimitsInOIFromDraggingLines(ALimitLabelsToUpdate: TLimitLabels; var AOffsets: TSimpleRectString);
begin
  if not Assigned(FOnUpdateSearchAreaLimitsInOIFromDraggingLines) then
    Exit;

  FOnUpdateSearchAreaLimitsInOIFromDraggingLines(ALimitLabelsToUpdate, AOffsets);
end;


procedure TfrClickerFindControl.DoOnUpdateTextCroppingLimitsInOIFromDraggingLines(ALimitLabelsToUpdate: TLimitLabels; var AOffsets: TSimpleRectString; AFontProfileIndex: Integer);
begin
  if not Assigned(FOnUpdateTextCroppingLimitsInOIFromDraggingLines) then
    Exit;

  FOnUpdateTextCroppingLimitsInOIFromDraggingLines(ALimitLabelsToUpdate, AOffsets, AFontProfileIndex);
end;


function TfrClickerFindControl.DoOnGetDisplayedText: string;
begin
  if not Assigned(FOnGetDisplayedText) then
    raise Exception.Create('OnGetDisplayedText not assigned.')
  else
    Result := FOnGetDisplayedText;
end;


procedure TfrClickerFindControl.DoOnSetMatchTextAndClassToOI(AMatchText, AMatchClassName: string);
begin
  if not Assigned(FOnSetMatchTextAndClassToOI) then
    raise Exception.Create('OnSetMatchTextAndClassToOI not assigned.')
  else
    FOnSetMatchTextAndClassToOI(AMatchText, AMatchClassName);
end;


function TfrClickerFindControl.DoOnGetFindControlOptions: PClkFindControlOptions;
begin
  if not Assigned(FOnGetFindControlOptions) then
    raise Exception.Create('OnGetFindControlOptions not assigned.')
  else
    Result := FOnGetFindControlOptions;
end;


function TfrClickerFindControl.DoOnExecuteFindSubControlAction(AErrorLevel, AErrorCount, AFastSearchErrorCount: Integer; out AFoundArea: TRect): Boolean;
begin
  if not Assigned(FOnExecuteFindSubControlAction) then
    raise Exception.Create('OnExecuteFindSubControlAction not assigned.')
  else
    Result := FOnExecuteFindSubControlAction(AErrorLevel, AErrorCount, AFastSearchErrorCount, AFoundArea);
end;


procedure TfrClickerFindControl.DoOnAddToLog(s: string);
begin
  if not Assigned(FOnAddToLog) then
    raise Exception.Create('OnAddToLog not assigned.')
  else
    FOnAddToLog(s);
end;


procedure TfrClickerFindControl.SetBMPTextFrameVisibility;
var
  i: Integer;
begin
  for i := 0 to Length(FBMPTextProfiles) - 1 do
    FBMPTextProfiles[i].EditorVisible := i = tabctrlBMPText.TabIndex;
end;


procedure TfrClickerFindControl.CreateClickerPrimitivesFrame;
begin
  if FfrClickerPrimitives <> nil then
    FfrClickerPrimitives.Free;

  FfrClickerPrimitives := TfrClickerPrimitives.Create(Self);
  FfrClickerPrimitives.Parent := TabSheetActionFindSubControlPrimitives;
  FfrClickerPrimitives.Width := TabSheetActionFindSubControlPrimitives.Width - 320;
  FfrClickerPrimitives.Height := TabSheetActionFindSubControlPrimitives.Height;
  FfrClickerPrimitives.Left := 0;
  FfrClickerPrimitives.Top := 0;
  FfrClickerPrimitives.Anchors := [akLeft, akTop, akRight, akBottom];
  FfrClickerPrimitives.Visible := True;

  //do not set FfrClickerPrimitives handlers here, because they are set in TfrClickerActions.HandleOnOISelectedNode
end;


procedure TfrClickerFindControl.PreviewText;
var
  i: Integer;
begin
  for i := 0 to Length(FBMPTextProfiles) - 1 do
    FBMPTextProfiles[i].PreviewText;
end;


function TfrClickerFindControl.HandleBMPTextOnGetDisplayedText: string;
begin
  Result := DoOnGetDisplayedText; //lbeMatchBitmapText.Text;
end;


procedure TfrClickerFindControl.HandleBMPTextOnTriggerOnControlsModified;
begin
  DoOnTriggerOnControlsModified;
end;


function TfrClickerFindControl.HandleBMPTextOnEvaluateReplacements(s: string): string;
begin
  Result := EvaluateReplacements(s);
end;


procedure TfrClickerFindControl.HandleBMPTextOnSetCroppingValuesToOtherFontProfiles(ACropLeft, ACropTop, ACropRight, ACropBottom: string; ASkipProfileIndex: Integer);
var
  i: Integer;
begin
  for i := 0 to Length(FBMPTextProfiles) - 1 do
    if i <> ASkipProfileIndex then
    begin
      FBMPTextProfiles[i].CropLeft := ACropLeft;
      FBMPTextProfiles[i].CropTop := ACropTop;
      FBMPTextProfiles[i].CropRight := ACropRight;
      FBMPTextProfiles[i].CropBottom := ACropBottom;
      FBMPTextProfiles[i].UpdateSelectionLabelsFromCropEditBoxes;
    end;
end;


function TfrClickerFindControl.HandleBMPTextOnGetCroppingLinesVisiblity: Boolean;
begin
  Result := chkDisplayCroppingLines.Checked;
end;


procedure TfrClickerFindControl.HandleOnUpdateTextCroppingLimitsInOIFromDraggingLines(ALimitLabelsToUpdate: TLimitLabels; var AOffsets: TSimpleRectString; AFontProfileName: string);
var
  ProfileIndex: Integer;
begin
  ProfileIndex := GetFontProfileIndexByName(AFontProfileName);
  if ProfileIndex = -1 then
    Exit;

  DoOnUpdateTextCroppingLimitsInOIFromDraggingLines(ALimitLabelsToUpdate, AOffsets, ProfileIndex);
end;


function TfrClickerFindControl.HandleOnGetFindControlMatchBitmapText(Sender: TObject): PClkFindControlMatchBitmapText;
var
  FindControlOptions: PClkFindControlOptions;
  i: Integer;
  Found: Boolean;
begin
  FindControlOptions := DoOnGetFindControlOptions;

  if Length(FBMPTextProfiles) = 0 then
    raise Exception.Create('No font profile available.');

  Found := False;
  for i := 0 to Length(FBMPTextProfiles) - 1 do
  begin
    if Sender = FBMPTextProfiles[i].frClickerBMPText then
    begin
      Found := True;
      Break;
    end;
  end;

  if Found then
    Result := @FindControlOptions^.MatchBitmapText[i]
  else
    raise Exception.Create('Can''t find font profile by index.');
end;


function TfrClickerFindControl.EvaluateReplacements(s: string): string;
begin
  if Assigned(FOnEvaluateReplacements) then
    Result := FOnEvaluateReplacements(s)
  else
    raise Exception.Create('OnEvaluateReplacements not assigned.');
end;


function TfrClickerFindControl.ReverseEvaluateReplacements(s: string): string;
begin
  if Assigned(FOnReverseEvaluateReplacements) then
    Result := FOnReverseEvaluateReplacements(s)
  else
    raise Exception.Create('OnReverseEvaluateReplacements not assigned.');
end;


function TfrClickerFindControl.GetFontProfileIndexByName(AProfileName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(FBMPTextProfiles) - 1 do
    if FBMPTextProfiles[i].ProfileName = AProfileName then
    begin
      Result := i;
      Exit;
    end;
end;


procedure TfrClickerFindControl.RemoveFontProfileByIndex(AIndex: Integer);
var
  i: Integer;
begin
  if (AIndex < 0) or (AIndex > Length(FBMPTextProfiles) - 1) then
    raise Exception.Create('Index out of bounds when removing font profile.');

  FBMPTextProfiles[AIndex].Free;

  for i := AIndex to Length(FBMPTextProfiles) - 2 do
    FBMPTextProfiles[i] := FBMPTextProfiles[i + 1];

  SetLength(FBMPTextProfiles, Length(FBMPTextProfiles) - 1);
  tabctrlBMPText.Tabs.Delete(AIndex);

  if AIndex <= tabctrlBMPText.Tabs.Count - 1 then
    tabctrlBMPText.TabIndex := AIndex;

  for i := 0 to Length(FBMPTextProfiles) - 1 do
    FBMPTextProfiles[i].ObjectName := FBMPTextProfiles[i].ObjectName + 'a'; //modify names, so they can be renamed below

  for i := 0 to Length(FBMPTextProfiles) - 1 do
    FBMPTextProfiles[i].ObjectName := 'FBMPTextFrames_' + IntToStr(i);

  SetBMPTextFrameVisibility;
end;


procedure TfrClickerFindControl.AddFontProfile(AProfileName: string);
var
  Index, n: Integer;
begin
  if Trim(AProfileName) = '' then
  begin
    MessageBox(Handle, 'Font profile name cannot be empty.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  Index := GetFontProfileIndexByName(AProfileName);
  if Index > -1 then
  begin
    MessageBox(Handle, 'Font profile already exists. Please use a different name.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  tabctrlBMPText.Tabs.Add(AProfileName);
  tabctrlBMPText.TabIndex := tabctrlBMPText.Tabs.Count - 1;

  n := Length(FBMPTextProfiles);
  SetLength(FBMPTextProfiles, n + 1);
  FBMPTextProfiles[n] := TFontProfile.Create(Self, 'FBMPTextFrames_' + IntToStr(n));
  FBMPTextProfiles[n].ProfileName := AProfileName;
  FBMPTextProfiles[n].FindControlMatchBitmapText := @DoOnGetFindControlOptions.MatchBitmapText[n];

  SetBMPTextFrameVisibility;
end;


procedure TfrClickerFindControl.CreateBMPTextFrames(ACount: Integer);
var
  i: Integer;
begin
  tabctrlBMPText.Tabs.Clear;

  for i := 0 to Length(FBMPTextProfiles) - 1 do
    FBMPTextProfiles[i].Free;

  SetLength(FBMPTextProfiles, ACount);

  for i := 0 to ACount - 1 do
  begin
    FBMPTextProfiles[i] := TFontProfile.Create(Self, 'FBMPTextFrames_' + IntToStr(i));
    FBMPTextProfiles[i].EditorVisible := i = tabctrlBMPText.TabIndex;

    FBMPTextProfiles[i].ProfileName := 'not set ' + IntToStr(i);
    FBMPTextProfiles[i].FindControlMatchBitmapText := @DoOnGetFindControlOptions.MatchBitmapText[i];

    tabctrlBMPText.Tabs.Add('no name ' + IntToStr(i));
    tabctrlBMPText.TabIndex := tabctrlBMPText.Tabs.Count - 1;
  end;
end;


function TfrClickerFindControl.GetBMPTextFontProfilesCount: Integer;
begin
  Result := Length(FBMPTextProfiles);
end;


procedure TfrClickerFindControl.spdbtnDisplaySearchAreaDbgImgMenuClick(    //the small arrow button, next to "Display dbg img" button
  Sender: TObject);
var
  tp: TPoint;
begin
  PopulateDbgImgExtraMenu;
  GetCursorPos(tp);
  FSearchAreaDbgImgSearchedBmpMenu.PopUp(tp.X, tp.Y);
end;


procedure TfrClickerFindControl.spdbtnExtraCopyValueWindowsClick(Sender: TObject);
var
  tp: TPoint;
begin
  GetCursorPos(tp);
  pmExtraCopyValueWindows.PopUp(tp.X, tp.Y);
end;


procedure TfrClickerFindControl.tabctrlBMPTextChange(Sender: TObject);
begin
  SetBMPTextFrameVisibility;
end;


procedure TfrClickerFindControl.tmrBlinkCalcErrLevelTimer(Sender: TObject);
begin
  imgCalcMinErrLevel.Visible := not imgCalcMinErrLevel.Visible;
end;


procedure TfrClickerFindControl.tmrDrawZoomTimer(Sender: TObject);
var
  TempBmp: TBitmap;
  tp: TPoint;
begin
  tmrDrawZoom.Enabled := False;

  if FSearchAreaControlDbgImg = nil then
    MessageBox(Handle, 'Debug image is not available.', PChar(Caption), MB_ICONERROR);

  TempBmp := TBitmap.Create;
  try
    TempBmp.Width := FSearchAreaControlDbgImg.Width;
    TempBmp.Height := FSearchAreaControlDbgImg.Height;
    TempBmp.Canvas.Draw(0, 0, FSearchAreaControlDbgImg.Picture.Bitmap);

    TempBmp.Canvas.Pen.Color := FSearchAreaLeftLimitLabel.Color;
    Line(TempBmp.Canvas, FSearchAreaLeftLimitLabel.Left, 0, FSearchAreaLeftLimitLabel.Left, TempBmp.Height - 1);

    TempBmp.Canvas.Pen.Color := FSearchAreaTopLimitLabel.Color;
    Line(TempBmp.Canvas, 0, FSearchAreaTopLimitLabel.Top, TempBmp.Width - 1, FSearchAreaTopLimitLabel.Top);

    TempBmp.Canvas.Pen.Color := FSearchAreaRightLimitLabel.Color;
    Line(TempBmp.Canvas, FSearchAreaRightLimitLabel.Left, 0, FSearchAreaRightLimitLabel.Left, TempBmp.Height - 1);

    TempBmp.Canvas.Pen.Color := FSearchAreaBottomLimitLabel.Color;
    Line(TempBmp.Canvas, 0, FSearchAreaBottomLimitLabel.Top, TempBmp.Width - 1, FSearchAreaBottomLimitLabel.Top);

    if chkIncludeSearchedBmpInZoom.Checked then
    begin
      if Assigned(FSearchAreaSearchedBmpDbgImg) then
        if (FSearchAreaSearchedBmpDbgImg.Width > 0) and (FSearchAreaSearchedBmpDbgImg.Height > 0) then
          TempBmp.Canvas.Draw(FSearchAreaSearchedBmpDbgImg.Left, FSearchAreaSearchedBmpDbgImg.Top, FSearchAreaSearchedBmpDbgImg.Picture.Bitmap);

      if Assigned(FSearchAreaSearchedTextDbgImg) then
        if (FSearchAreaSearchedTextDbgImg.Width > 0) and (FSearchAreaSearchedTextDbgImg.Height > 0) then
          TempBmp.Canvas.Draw(FSearchAreaSearchedTextDbgImg.Left, FSearchAreaSearchedTextDbgImg.Top, FSearchAreaSearchedTextDbgImg.Picture.Bitmap);
    end;

    if chkShowGridOnBMPPreview.Checked then
    begin
      if FSkipDrawingGrid then
        FSkipDrawingGrid := False
      else
        GeneratePreviewGridContent(FGridDrawingOption);

      TempBmp.Canvas.Draw(FSearchAreaGridImg.Left, FSearchAreaGridImg.Top, FSearchAreaGridImg.Picture.Bitmap);
    end;

    GetCursorPos(tp);
    SetZoomContent(TempBmp, FCurrentMousePosOnPreviewImg.X, FCurrentMousePosOnPreviewImg.Y, tp.X + 50, tp.Y + 50);
  finally
    TempBmp.Free;
  end;
end;


procedure TfrClickerFindControl.tmrUpdateGridTimer(Sender: TObject);
begin
  tmrUpdateGrid.Enabled := False;
  GeneratePreviewGridContent(FGridDrawingOption);
end;


procedure TfrClickerFindControl.tmrUpdateSearchAreaOffsetEditBoxesTimer(
  Sender: TObject);
var
  Offsets: TSimpleRectString;
begin
  tmrUpdateSearchAreaOffsetEditBoxes.Enabled := False;

  Offsets.Left := IntToStr(GetSearchAreaLeftOffsetFromSelLabel);
  Offsets.Top := IntToStr(GetSearchAreaTopOffsetFromSelLabel);
  Offsets.Right := IntToStr(GetSearchAreaRightOffsetFromSelLabel - GetControlWidthFromReplacement);
  Offsets.Bottom := IntToStr(GetSearchAreaBottomOffsetFromSelLabel - GetControlHeightFromReplacement);
  DoOnUpdateSearchAreaLimitsInOIFromDraggingLines([llLeft, llTop, llRight, llBottom], Offsets);

  tmrUpdateGrid.Enabled := True;
end;


procedure TfrClickerFindControl.HandleMatchTextClick;
begin
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.chkShowBMPFileDbgImgClick(Sender: TObject);
begin
  if FSearchAreaSearchedBmpDbgImg <> nil then
    FSearchAreaSearchedBmpDbgImg.Visible := chkShowBMPFileDbgImg.Checked;   //BMP file
end;


procedure TfrClickerFindControl.chkShowBMPTextDbgImgClick(Sender: TObject);
begin
  if FSearchAreaSearchedTextDbgImg <> nil then
    FSearchAreaSearchedTextDbgImg.Visible := chkShowBMPTextDbgImg.Checked;  //BMP text
end;


procedure TfrClickerFindControl.GeneratePreviewGridContent(ADisplayGridLineOption: TDisplayGridLineOption);
var
  AlgorithmSettings: TMatchBitmapAlgorithmSettings;
  FindControlOptions: PClkFindControlOptions;
  NoGridAreaImg: TImage;
begin
  if FSearchAreaControlDbgImg = nil then
    Exit;

  FindControlOptions := DoOnGetFindControlOptions;

  AlgorithmSettings.XMultipleOf := FindControlOptions^.MatchBitmapAlgorithmSettings.XMultipleOf;
  AlgorithmSettings.YMultipleOf := FindControlOptions^.MatchBitmapAlgorithmSettings.YMultipleOf;
  AlgorithmSettings.XOffset := FindControlOptions^.MatchBitmapAlgorithmSettings.XOffset;
  AlgorithmSettings.YOffset := FindControlOptions^.MatchBitmapAlgorithmSettings.YOffset;

  FSearchAreaGridImg.Left := AlgorithmSettings.XOffset + FSearchAreaLeftLimitLabel.Left;
  FSearchAreaGridImg.Top := AlgorithmSettings.YOffset + FSearchAreaTopLimitLabel.Top;

  FSearchAreaGridImg.Transparent := True;

  WipeImage(FSearchAreaGridImg, FSearchAreaControlDbgImg.Width, FSearchAreaControlDbgImg.Height);
  FSearchAreaGridImg.Picture.Bitmap.TransparentColor := clWhite;

  if chkShowGridOnBMPPreview.Checked then
  begin
    if ADisplayGridLineOption = loTransparentSolid then
      BitBlt(FSearchAreaGridImg.Canvas.Handle,
             0,
             0,
             FSearchAreaControlDbgImg.Width - FSearchAreaGridImg.Left,
             FSearchAreaControlDbgImg.Height - FSearchAreaGridImg.Top,
             FSearchAreaControlDbgImg.Canvas.Handle,
             FSearchAreaGridImg.Left,
             FSearchAreaGridImg.Top,
             SRCCOPY
      );

      //HDC hdcDest, // handle to destination DC
      //int nXDest,  // x-coord of destination upper-left corner
      //int nYDest,  // y-coord of destination upper-left corner
      //int nWidth,  // width of destination rectangle
      //int nHeight, // height of destination rectangle
      //HDC hdcSrc,  // handle to source DC
      //int nXSrc,   // x-coordinate of source upper-left corner
      //int nYSrc,   // y-coordinate of source upper-left corner
      //DWORD dwRop  // raster operation code

    DrawSearchGrid(FSearchAreaGridImg, AlgorithmSettings, FSearchAreaControlDbgImg.Width, FSearchAreaControlDbgImg.Height, $00C9AEFF, ADisplayGridLineOption);

    if ADisplayGridLineOption = loTransparentSolid then
    begin
      NoGridAreaImg := TImage.Create(Self);
      try
        NoGridAreaImg.Visible := False;
        WipeImage(NoGridAreaImg, FSearchAreaControlDbgImg.Width, FSearchAreaControlDbgImg.Height);   //////////////// this image is too large

        BitBlt(NoGridAreaImg.Canvas.Handle,
             0,
             0,
             FSearchAreaControlDbgImg.Width - FSearchAreaGridImg.Left,
             FSearchAreaControlDbgImg.Height - FSearchAreaGridImg.Top,
             FSearchAreaControlDbgImg.Canvas.Handle,
             FSearchAreaGridImg.Left,
             FSearchAreaGridImg.Top,
             SRCCOPY
        );

        AvgBitmapWithBitmap(FSearchAreaGridImg.Picture.Bitmap,        //image with grid
                            NoGridAreaImg.Picture.Bitmap,             //image without grid
                            FSearchAreaGridImg.Picture.Bitmap,        //destination should have an average grid
                            0,
                            0,
                            FSearchAreaControlDbgImg.Width - FSearchAreaGridImg.Left,
                            FSearchAreaControlDbgImg.Height - FSearchAreaGridImg.Top);
      finally
        NoGridAreaImg.Free;
      end;
    end;
  end;

  MakeImageContentTransparent(FSearchAreaGridImg);
end;


procedure TfrClickerFindControl.RefreshGrid;
begin
  if chkShowGridOnBMPPreview.Checked then
    GeneratePreviewGridContent(FGridDrawingOption);

  if FSearchAreaControlDbgImg <> nil then
    FSearchAreaGridImg.Visible := chkShowGridOnBMPPreview.Checked;
end;


procedure TfrClickerFindControl.chkShowGridOnBMPPreviewChange(Sender: TObject);
begin
  RefreshGrid;
end;


procedure TfrClickerFindControl.MenuSetGridType(Sender: TObject);
begin
  FGridDrawingOption := TDisplayGridLineOption((Sender as TMenuItem).Tag);
  RefreshGrid;
end;


procedure TfrClickerFindControl.PageControlMatchChange(Sender: TObject);
var
  i: Integer;
begin
  if PageControlMatch.ActivePage = TabSheetActionFindSubControlSearchArea then
  begin
    if FSearchAreaSearchedTextDbgImg <> nil then
      for i := 0 to Length(FBMPTextProfiles) - 1 do
        FBMPTextProfiles[i].PreviewTextOnImage(FSearchAreaSearchedTextDbgImg);
  end;
end;


procedure TfrClickerFindControl.pnlDragMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragging := True;
end;


procedure TfrClickerFindControl.pnlDragMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  tp: TPoint;
  Comp: TCompRec;
begin
  if FDragging then
  begin
    GetCursorPos(tp);
    Comp := GetWindowClassRec(tp);

    lbeFoundControlText.Text := Comp.Text;
    lbeFoundControlClass.Text := Comp.ClassName;
    edtFoundControlInfo.Text := 'Control info:  handle = ' + IntToStr(Comp.Handle) +
                                '  x:y = ' + IntToStr(Comp.ComponentRectangle.Left) + ':' + IntToStr(Comp.ComponentRectangle.Top) +
                                '  w:h = ' + IntToStr(Comp.ComponentRectangle.Width) + ':' + IntToStr(Comp.ComponentRectangle.Height);

    if pnlDrag.Color <> clLime then
      pnlDrag.Color := clLime;
  end;
end;


procedure TfrClickerFindControl.pnlDragMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  pnlDrag.Color := clYellow;

  if chkAutoCopyValuesToObjectInspector.Checked then
    DoOnSetMatchTextAndClassToOI(lbeFoundControlText.Text, lbeFoundControlClass.Text);
end;


procedure TfrClickerFindControl.AddNewFontProfile(ANewProfile: TClkFindControlMatchBitmapText);
var
  n: Integer;
begin
  n := tabctrlBMPText.Tabs.Count;
  AddFontProfile(ANewProfile.ProfileName);

  if tabctrlBMPText.Tabs.Count = n then
    Exit;

  FBMPTextProfiles[n].MatchBitmapTextFGColor := ANewProfile.ForegroundColor;
  FBMPTextProfiles[n].MatchBitmapTextBGColor := ANewProfile.BackgroundColor;
  //FBMPTextProfiles[n].FGColor := HexToInt(EvaluateReplacements(ANewProfile.ForegroundColor));
  //FBMPTextProfiles[n].BGColor := HexToInt(EvaluateReplacements(ANewProfile.BackgroundColor));
  FBMPTextProfiles[n].MatchBitmapTextFontName := ANewProfile.FontName;
  FBMPTextProfiles[n].MatchBitmapTextSize := IntToStr(ANewProfile.FontSize);
  FBMPTextProfiles[n].FontQualityReplacement := ANewProfile.FontQualityReplacement;
  FBMPTextProfiles[n].MatchBitmapTextFontQualityIndex := Ord(ANewProfile.FontQuality);
  FBMPTextProfiles[n].Bold := ANewProfile.Bold;
  FBMPTextProfiles[n].Italic := ANewProfile.Italic;
  FBMPTextProfiles[n].Underline := ANewProfile.Underline;
  FBMPTextProfiles[n].StrikeOut := ANewProfile.StrikeOut;
  FBMPTextProfiles[n].CropLeft := ANewProfile.CropLeft;
  FBMPTextProfiles[n].CropTop := ANewProfile.CropTop;
  FBMPTextProfiles[n].CropRight := ANewProfile.CropRight;
  FBMPTextProfiles[n].CropBottom := ANewProfile.CropBottom;

  PreviewText;

  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.UpdateFontProfileName(AProfileIndex: Integer; ANewName: string);
begin
  if (AProfileIndex < 0) or (AProfileIndex > Length(FBMPTextProfiles) - 1) then
    raise Exception.Create('Index out of bounds (' + IntToStr(AProfileIndex) + ') on updating font profile name: ' + ANewName);

  FBMPTextProfiles[AProfileIndex].ProfileName := ANewName;
  tabctrlBMPText.Tabs.Strings[AProfileIndex] := ANewName;
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.btnCopyFoundValuesClick(Sender: TObject);
begin
  DoOnSetMatchTextAndClassToOI(lbeFoundControlText.Text, lbeFoundControlClass.Text);
end;


procedure TfrClickerFindControl.chkDisplayCroppingLinesChange(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Length(FBMPTextProfiles) - 1 do
    FBMPTextProfiles[i].ShowCroppingLines := chkDisplayCroppingLines.Checked;
end;


procedure TfrClickerFindControl.CopyTextAndClassFromExternalProvider(AProviderName: string);
var
  ControlText, ControlClass: string;
begin
  if not Assigned(FOnCopyControlTextAndClassFromMainWindow) then
    raise Exception.Create('OnCopyControlTextAndClass not assigned for ' + Caption)
  else
  begin
    FOnCopyControlTextAndClassFromMainWindow(AProviderName, ControlText, ControlClass);
    DoOnSetMatchTextAndClassToOI(ControlText, ControlClass);
  end;
end;


procedure TfrClickerFindControl.CopyTextAndClassFromPreviewWindowClick(
  Sender: TObject);
begin
  CopyTextAndClassFromExternalProvider(CPreviewWindow);
end;


procedure TfrClickerFindControl.CopyTextAndClassFromRemoteScreenWindowClick(
  Sender: TObject);
begin
  CopyTextAndClassFromExternalProvider(CRemoteScreenWindow);
end;


procedure TfrClickerFindControl.CopyTextAndClassFromWinInterpWindowClick(
  Sender: TObject);
begin
  CopyTextAndClassFromExternalProvider(CWinInterpWindow);
end;


procedure TfrClickerFindControl.FSearchAreaScrBoxMouseWheel(
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
    FSearchAreaScrBox.HorzScrollBar.Position := FSearchAreaScrBox.HorzScrollBar.Position - WheelDelta div Factor
  else
    FSearchAreaScrBox.VertScrollBar.Position := FSearchAreaScrBox.VertScrollBar.Position - WheelDelta div Factor;

  Handled := True;
end;


procedure TfrClickerFindControl.PopulateDbgImgExtraMenu;
var
  MenuItem: TMenuItem;
  i: Integer;
begin
  FSearchAreaDbgImgSearchedBmpMenu.Items.Clear;

  MenuItem := TMenuItem.Create(FSearchAreaDbgImgSearchedBmpMenu);
  MenuItem.Caption := 'Load "Bmp Text" to searched area';
  MenuItem.OnClick := MenuItemLoadBmpTextToSearchedAreaClick;
  FSearchAreaDbgImgSearchedBmpMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FSearchAreaDbgImgSearchedBmpMenu);
  MenuItem.Caption := 'Unload "Bmp Text" from searched area';
  MenuItem.OnClick := MenuItemUnloadBmpTextFromSearchedAreaClick;
  FSearchAreaDbgImgSearchedBmpMenu.Items.Add(MenuItem);

  if lstMatchBitmapFiles.Items.Count > 0 then
  begin
    MenuItem := TMenuItem.Create(FSearchAreaMenu);
    MenuItem.Caption := '-';
    FSearchAreaDbgImgSearchedBmpMenu.Items.Add(MenuItem);

    for i := 0 to lstMatchBitmapFiles.Items.Count - 1 do
    begin
      MenuItem := TMenuItem.Create(FSearchAreaDbgImgSearchedBmpMenu);
      MenuItem.Caption := lstMatchBitmapFiles.Items.Strings[i];
      MenuItem.OnClick := MenuItemGenericLoadBmpToSearchedAreaClick;
      MenuItem.Enabled := DoOnFileExists(lstMatchBitmapFiles.Items.Strings[i]);
      FSearchAreaDbgImgSearchedBmpMenu.Items.Add(MenuItem);
    end;
  end;

  if lstMatchPrimitiveFiles.Items.Count > 0 then
  begin
    MenuItem := TMenuItem.Create(FSearchAreaMenu);
    MenuItem.Caption := '-';
    FSearchAreaDbgImgSearchedBmpMenu.Items.Add(MenuItem);

    for i := 0 to lstMatchPrimitiveFiles.Items.Count - 1 do
    begin
      MenuItem := TMenuItem.Create(FSearchAreaDbgImgSearchedBmpMenu);
      MenuItem.Caption := lstMatchPrimitiveFiles.Items.Strings[i];
      MenuItem.OnClick := MenuItemGenericLoadBmpToSearchedAreaClick;
      MenuItem.Enabled := DoOnFileExists(lstMatchPrimitiveFiles.Items.Strings[i]);
      FSearchAreaDbgImgSearchedBmpMenu.Items.Add(MenuItem);
    end;
  end;
end;


procedure TfrClickerFindControl.CreateSelectionLabels(var ALeftLabel, ATopLabel, ARightLabel, ABottomLabel: TLabel; ALeftColor, ATopColor, ARightColor, ABottomColor: TColor; AShouldBringToFront, ACreateWithPaintedLabel: Boolean);
begin
  if ACreateWithPaintedLabel then
  begin
    ALeftLabel := TPaintedLabel.Create(Self);
    ATopLabel := TPaintedLabel.Create(Self);
    ARightLabel := TPaintedLabel.Create(Self);
    ABottomLabel := TPaintedLabel.Create(Self);
  end
  else
  begin
    ALeftLabel := TLabel.Create(Self);
    ATopLabel := TLabel.Create(Self);
    ARightLabel := TLabel.Create(Self);
    ABottomLabel := TLabel.Create(Self);
  end;

  ALeftLabel.Parent := FSearchAreaScrBox;
  ATopLabel.Parent := FSearchAreaScrBox;
  ARightLabel.Parent := FSearchAreaScrBox;
  ABottomLabel.Parent := FSearchAreaScrBox;

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


procedure TfrClickerFindControl.CreateTransparentSelectionLabels;
begin
  CreateSelectionLabels(FTransparent_SearchAreaLeftLimitLabel,
                        FTransparent_SearchAreaTopLimitLabel,
                        FTransparent_SearchAreaRightLimitLabel,
                        FTransparent_SearchAreaBottomLimitLabel,
                        clDefault,
                        clDefault,
                        clDefault,
                        clDefault,
                        False,
                        False);

  FTransparent_SearchAreaLeftLimitLabel.Width := 3;
  FTransparent_SearchAreaTopLimitLabel.Height := 3;
  FTransparent_SearchAreaRightLimitLabel.Width := 3;
  FTransparent_SearchAreaBottomLimitLabel.Height := 3;

  FTransparent_SearchAreaLeftLimitLabel.Transparent := True;
  FTransparent_SearchAreaTopLimitLabel.Transparent := True;
  FTransparent_SearchAreaRightLimitLabel.Transparent := True;
  FTransparent_SearchAreaBottomLimitLabel.Transparent := True;

  FTransparent_SearchAreaLeftLimitLabel.Cursor := crSizeWE;
  FTransparent_SearchAreaTopLimitLabel.Cursor := crSizeNS;
  FTransparent_SearchAreaRightLimitLabel.Cursor := crSizeWE;
  FTransparent_SearchAreaBottomLimitLabel.Cursor := crSizeNS;

  FTransparent_SearchAreaLeftLimitLabel.OnMouseDown := FTransparent_LeftMouseDown;
  FTransparent_SearchAreaLeftLimitLabel.OnMouseMove := FTransparent_LeftMouseMove;
  FTransparent_SearchAreaLeftLimitLabel.OnMouseUp := FTransparent_LeftMouseUp;
  FTransparent_SearchAreaLeftLimitLabel.OnMouseEnter := imgSearchAreaControlDbgMouseEnter;
  FTransparent_SearchAreaLeftLimitLabel.OnMouseLeave := imgSearchAreaControlDbgMouseLeave;

  FTransparent_SearchAreaRightLimitLabel.OnMouseDown := FTransparent_RightMouseDown;
  FTransparent_SearchAreaRightLimitLabel.OnMouseMove := FTransparent_RightMouseMove;
  FTransparent_SearchAreaRightLimitLabel.OnMouseUp := FTransparent_RightMouseUp;
  FTransparent_SearchAreaRightLimitLabel.OnMouseEnter := imgSearchAreaControlDbgMouseEnter;
  FTransparent_SearchAreaRightLimitLabel.OnMouseLeave := imgSearchAreaControlDbgMouseLeave;

  FTransparent_SearchAreaTopLimitLabel.OnMouseDown := FTransparent_TopMouseDown;
  FTransparent_SearchAreaTopLimitLabel.OnMouseMove := FTransparent_TopMouseMove;
  FTransparent_SearchAreaTopLimitLabel.OnMouseUp := FTransparent_TopMouseUp;
  FTransparent_SearchAreaTopLimitLabel.OnMouseEnter := imgSearchAreaControlDbgMouseEnter;
  FTransparent_SearchAreaTopLimitLabel.OnMouseLeave := imgSearchAreaControlDbgMouseLeave;

  FTransparent_SearchAreaBottomLimitLabel.OnMouseDown := FTransparent_BottomMouseDown;
  FTransparent_SearchAreaBottomLimitLabel.OnMouseMove := FTransparent_BottomMouseMove;
  FTransparent_SearchAreaBottomLimitLabel.OnMouseUp := FTransparent_BottomMouseUp;
  FTransparent_SearchAreaBottomLimitLabel.OnMouseEnter := imgSearchAreaControlDbgMouseEnter;
  FTransparent_SearchAreaBottomLimitLabel.OnMouseLeave := imgSearchAreaControlDbgMouseLeave;
end;


procedure TfrClickerFindControl.DisplayDebuggingImage;
const
  CDbgImgPreviewHint: string = 'Drag to move.' + #13#10 +
                               'Ctrl-Drag, to update Left/Top offsets.' + #13#10 +
                               'Ctrl-Shift-Drag, to update Left/Top/Right/Bottom offsets.';
  CSearchAreaMaxWidth = 2000;
  CSearchAreaMaxHeight = 1200;
var
  SearchAreaControlRect: TRect;
  SearchAreaControlHandle: THandle;
  MenuItem: TMenuItem;
  ControlLeft: Integer;
  ControlTop: Integer;
  FindControlOptions: PClkFindControlOptions;
begin
  try
    if FSearchAreaScrBox = nil then
    begin
      FSearchAreaScrBox := TScrollBox.Create(Self);
      FSearchAreaScrBox.Parent := TabSheetActionFindSubControlSearchArea;
      FSearchAreaScrBox.Width := lblReservedSpaceForDbgImg.Width; // TabSheetActionFindSubControlSearchArea.Width - btnDisplaySearchAreaDebuggingImage.Left - 10; //130;
      FSearchAreaScrBox.Height := TabSheetActionFindSubControlSearchArea.Height - 20 - 30; //120;
      FSearchAreaScrBox.Left := lblReservedSpaceForDbgImg.Left;
      FSearchAreaScrBox.Top := lblReservedSpaceForDbgImg.Top;
      FSearchAreaScrBox.Anchors := [akBottom, akLeft, akRight, akTop];
      FSearchAreaScrBox.Color := clYellow;
      FSearchAreaScrBox.HorzScrollBar.Smooth := True;
      FSearchAreaScrBox.VertScrollBar.Smooth := True;
      FSearchAreaScrBox.HorzScrollBar.Tracking := True;
      FSearchAreaScrBox.VertScrollBar.Tracking := True;
      FSearchAreaScrBox.HorzScrollBar.Visible := True;
      FSearchAreaScrBox.VertScrollBar.Visible := True;
      FSearchAreaScrBox.OnMouseWheel := FSearchAreaScrBoxMouseWheel;
      FSearchAreaScrBox.ShowHint := False;
      FSearchAreaScrBox.ParentShowHint := False;
    end
    else
      btnDisplaySearchAreaDebuggingImage.Caption := 'Re-display dbg img';

    SearchAreaControlRect.Left := 0;//StrToIntDef(EvaluateReplacements('$Control_Left$'), 0);
    SearchAreaControlRect.Top := 0;//StrToIntDef(EvaluateReplacements('$Control_Top$'), 0);
    SearchAreaControlRect.Width := StrToIntDef(EvaluateReplacements('$Control_Width$'), 300);
    SearchAreaControlRect.Height := StrToIntDef(EvaluateReplacements('$Control_Height$'), 300);
    SearchAreaControlHandle := StrToIntDef(EvaluateReplacements('$Control_Handle$'), 0);

    if FSearchAreaOutOfImgImg = nil then
    begin
      FSearchAreaOutOfImgImg := TImage.Create(Self);
      FSearchAreaOutOfImgImg.Parent := FSearchAreaScrBox;
      FSearchAreaOutOfImgImg.AutoSize := False;
      FSearchAreaOutOfImgImg.Transparent := False;
      FSearchAreaOutOfImgImg.Left := 0;
      FSearchAreaOutOfImgImg.Top := 0;
      FSearchAreaOutOfImgImg.Width := CSearchAreaMaxWidth;
      FSearchAreaOutOfImgImg.Height := CSearchAreaMaxHeight;
      FSearchAreaOutOfImgImg.OnMouseMove := imgSearchAreaControlDbgMouseMove;

      FSearchAreaOutOfImgImg.Canvas.Pen.Color := $A0FFFF; //light yellow
      FSearchAreaOutOfImgImg.Canvas.Brush.Style := bsSolid;
      FSearchAreaOutOfImgImg.Canvas.Brush.Color := $A0FFFF; //light yellow
      FSearchAreaOutOfImgImg.Canvas.Rectangle(0, 0, FSearchAreaOutOfImgImg.Width, FSearchAreaOutOfImgImg.Height);

      FSearchAreaOutOfImgImg.Canvas.Pen.Color := $A0FFFF; //light yellow
      FSearchAreaOutOfImgImg.Canvas.Brush.Style := bsFDiagonal;
      FSearchAreaOutOfImgImg.Canvas.Brush.Color := $008888FF;
      FSearchAreaOutOfImgImg.Canvas.Rectangle(0, 0, FSearchAreaOutOfImgImg.Width, FSearchAreaOutOfImgImg.Height);
    end;

    if FSearchAreaControlDbgImg = nil then
    begin
      FSearchAreaControlDbgImg := TImage.Create(Self);
      FSearchAreaControlDbgImg.Parent := FSearchAreaScrBox;
      FSearchAreaControlDbgImg.Left := 0;
      FSearchAreaControlDbgImg.Top := 0;
      FSearchAreaControlDbgImg.OnMouseMove := imgSearchAreaControlDbgMouseMove;
      FSearchAreaControlDbgImg.OnMouseDown := imgSearchAreaControlDbgMouseDown;
      FSearchAreaControlDbgImg.OnMouseUp := imgSearchAreaControlDbgMouseUp;
      FSearchAreaControlDbgImg.OnMouseEnter := imgSearchAreaControlDbgMouseEnter;
      FSearchAreaControlDbgImg.OnMouseLeave := imgSearchAreaControlDbgMouseLeave;
      FSearchAreaControlDbgImg.OnResize := FSearchAreaControlDbgImgResize;
      FSearchAreaControlDbgImg.Hint := 'Current control, where the searched image is matched.';
      FSearchAreaControlDbgImg.ShowHint := True;
      FSearchAreaControlDbgImg.ParentShowHint := False;

      FSearchAreaGridImg := TImage.Create(Self);
      FSearchAreaGridImg.Parent := FSearchAreaScrBox;
      FSearchAreaGridImg.Left := 0;
      FSearchAreaGridImg.Top := 0;
      FSearchAreaGridImg.OnMouseMove := imgSearchAreaControlDbgMouseMove;
      FSearchAreaGridImg.OnMouseDown := imgSearchAreaControlDbgMouseDown;
      FSearchAreaGridImg.OnMouseUp := imgSearchAreaControlDbgMouseUp;
      FSearchAreaGridImg.OnMouseEnter := imgSearchAreaControlDbgMouseEnter;
      FSearchAreaGridImg.OnMouseLeave := imgSearchAreaControlDbgMouseLeave;
      FSearchAreaGridImg.Hint := FSearchAreaControlDbgImg.Hint;
      FSearchAreaGridImg.ShowHint := True;
      FSearchAreaGridImg.ParentShowHint := False;

      FSearchAreaDbgImgSearchedBmpMenu := TPopupMenu.Create(Self);

      CreateSelectionLabels(FSearchAreaLeftLimitLabel,
                            FSearchAreaTopLimitLabel,
                            FSearchAreaRightLimitLabel,
                            FSearchAreaBottomLimitLabel,
                            FPreviewSelectionColors.TopLeft_Valid,
                            FPreviewSelectionColors.TopLeft_Valid,
                            FPreviewSelectionColors.BotRight_Valid,
                            FPreviewSelectionColors.BotRight_Valid,
                            True,
                            False);

      CreateSelectionLabels(TLabel(FSearchAreaLeftLimitLabel_ForMinErr),
                            TLabel(FSearchAreaTopLimitLabel_ForMinErr),
                            TLabel(FSearchAreaRightLimitLabel_ForMinErr),
                            TLabel(FSearchAreaBottomLimitLabel_ForMinErr),
                            clTeal,
                            clTeal,
                            clTeal,
                            clTeal,
                            True,
                            True);

      FSearchAreaLeftLimitLabel_ForMinErr.Visible := False;
      FSearchAreaTopLimitLabel_ForMinErr.Visible := False;
      FSearchAreaRightLimitLabel_ForMinErr.Visible := False;
      FSearchAreaBottomLimitLabel_ForMinErr.Visible := False;

      FSearchAreaSearchedBmpDbgImg := TImage.Create(Self);      //created after labels, to maintain the proper z order
      FSearchAreaSearchedBmpDbgImg.Parent := FSearchAreaScrBox;
      FSearchAreaSearchedBmpDbgImg.OnMouseMove := imgSearchAreaSearchedBmpDbgImgMouseMove;
      FSearchAreaSearchedBmpDbgImg.OnMouseDown := imgSearchAreaSearchedBmpDbgImgMouseDown;
      FSearchAreaSearchedBmpDbgImg.OnMouseUp := imgSearchAreaSearchedBmpDbgImgMouseUp;
      FSearchAreaSearchedBmpDbgImg.OnMouseEnter := imgSearchAreaSearchedBmpDbgMouseEnter;
      FSearchAreaSearchedBmpDbgImg.OnMouseLeave := imgSearchAreaSearchedBmpDbgMouseLeave;
      FSearchAreaSearchedBmpDbgImg.Hint := 'BMP preview.' + #13#10 + CDbgImgPreviewHint;
      FSearchAreaSearchedBmpDbgImg.ShowHint := True;
      FSearchAreaSearchedBmpDbgImg.Width := 1;
      FSearchAreaSearchedBmpDbgImg.Height := 1;

      FSearchAreaSearchedTextDbgImg := TImage.Create(Self);
      FSearchAreaSearchedTextDbgImg.Parent := FSearchAreaScrBox;
      FSearchAreaSearchedTextDbgImg.OnMouseMove := imgSearchAreaSearchedTextDbgImgMouseMove;
      FSearchAreaSearchedTextDbgImg.OnMouseDown := imgSearchAreaSearchedTextDbgImgMouseDown;
      FSearchAreaSearchedTextDbgImg.OnMouseUp := imgSearchAreaSearchedTextDbgImgMouseUp;
      FSearchAreaSearchedTextDbgImg.OnMouseEnter := imgSearchAreaSearchedTextDbgMouseEnter;
      FSearchAreaSearchedTextDbgImg.OnMouseLeave := imgSearchAreaSearchedTextDbgMouseLeave;
      FSearchAreaSearchedTextDbgImg.Hint := 'Text preview.' + #13#10 + CDbgImgPreviewHint;
      FSearchAreaSearchedTextDbgImg.ShowHint := True;
      FSearchAreaSearchedTextDbgImg.Width := 1;
      FSearchAreaSearchedTextDbgImg.Height := 1;

      CreateTransparentSelectionLabels;

      /////////
      FSearchAreaMenu := TPopupMenu.Create(Self);

      MenuItem := TMenuItem.Create(FSearchAreaMenu);
      MenuItem.Caption := 'Copy background image to clipboard';
      MenuItem.OnClick := MenuItemCopySearchAreaBkImgToClipboardClick;
      MenuItem.Bitmap := imgCopyBkImg.Picture.Bitmap;
      FSearchAreaMenu.Items.Add(MenuItem);

      MenuItem := TMenuItem.Create(FSearchAreaMenu);
      MenuItem.Caption := 'Copy searched bitmap to clipboard';
      MenuItem.OnClick := MenuItemCopySearchAreaSearchBmpImgToClipboardClick;
      MenuItem.Bitmap := imgCopyBMPImg.Picture.Bitmap;
      FSearchAreaMenu.Items.Add(MenuItem);

      MenuItem := TMenuItem.Create(FSearchAreaMenu);
      MenuItem.Caption := 'Copy searched bitmap text to clipboard';
      MenuItem.OnClick := MenuItemCopySearchAreaSearchTextImgToClipboardClick;
      MenuItem.Bitmap := imgCopyTextImg.Picture.Bitmap;
      FSearchAreaMenu.Items.Add(MenuItem);

      MenuItem := TMenuItem.Create(FSearchAreaMenu);
      MenuItem.Caption := 'Copy background image and searched bitmap(s) to clipboard';
      MenuItem.OnClick := MenuItemCopySearchAreaAllToClipboardClick;
      MenuItem.Bitmap := imgCopyBkAndBMPImg.Picture.Bitmap;
      FSearchAreaMenu.Items.Add(MenuItem);

      MenuItem := TMenuItem.Create(FSearchAreaMenu);
      MenuItem.Caption := 'Copy selected area from background image to clipboard';
      MenuItem.OnClick := MenuItemCopySearchAreaSelectedAreaFromBkToClipboardClick;
      MenuItem.Bitmap := imgCopySelAreaFromBkImg.Picture.Bitmap;
      FSearchAreaMenu.Items.Add(MenuItem);

      MenuItem := TMenuItem.Create(FSearchAreaMenu);
      MenuItem.Caption := '-';
      FSearchAreaMenu.Items.Add(MenuItem);

      MenuItem := TMenuItem.Create(FSearchAreaMenu);
      MenuItem.Caption := 'Update Left and Top offsets from "Preview" image to editboxes';
      MenuItem.OnClick := MenuItemUpdateLeftAndTopOffsetsFromPreviewTextImageToEditboxes;
      MenuItem.Bitmap := imgUpdateLeftTopOffsets.Picture.Bitmap;
      FSearchAreaMenu.Items.Add(MenuItem);

      MenuItem := TMenuItem.Create(FSearchAreaMenu);
      MenuItem.Caption := 'Update Left, Top, Right and Botttom offsets from "Preview" image to editboxes';
      MenuItem.OnClick := MenuItemUpdateLeftTopRightBottomOffsetsFromPreviewTextImageToEditboxes;
      MenuItem.Bitmap := imgUpdateLeftTopRightBottomOffsets.Picture.Bitmap;
      FSearchAreaMenu.Items.Add(MenuItem);

      MenuItem := TMenuItem.Create(FSearchAreaMenu);
      MenuItem.Caption := '-';
      FSearchAreaMenu.Items.Add(MenuItem);

      MenuItem := TMenuItem.Create(FSearchAreaMenu);
      MenuItem.Caption := 'Calculate minimum color error level to match text...';
      MenuItem.OnClick := MenuItemCalculateMinimumErrorLevelToMatchText;
      MenuItem.Bitmap := imgCalcMinErrLevel.Picture.Bitmap;
      FSearchAreaMenu.Items.Add(MenuItem);

      MenuItem := TMenuItem.Create(FSearchAreaMenu);
      MenuItem.Caption := 'Calculate minimum color error count to match text...';
      MenuItem.OnClick := MenuItemCalculateMinimumColorErrorCountToMatchText;
      MenuItem.Bitmap := imgCalcMinErrLevel.Picture.Bitmap;
      FSearchAreaMenu.Items.Add(MenuItem);

      MenuItem := TMenuItem.Create(FSearchAreaMenu);
      MenuItem.Caption := 'Stop calculating minimum error level';
      MenuItem.OnClick := MenuItemStopCalculatingMinimumErrorLevelToMatchText;
      MenuItem.Bitmap := imgStopCalcMinErrLevel.Picture.Bitmap;
      FSearchAreaMenu.Items.Add(MenuItem);

      MenuItem := TMenuItem.Create(FSearchAreaMenu);
      MenuItem.Caption := 'Display selection lines for expected find location';
      MenuItem.OnClick := MenuItemDisplaySelectionLinesForExpectedFindLocation;
      MenuItem.Bitmap := imgDisplayExpectedFindLocation.Picture.Bitmap;
      FSearchAreaMenu.Items.Add(MenuItem);

      MenuItem := TMenuItem.Create(FSearchAreaMenu);
      MenuItem.Caption := 'Hide selection lines for expected find location';
      MenuItem.OnClick := MenuItemHideSelectionLinesForExpectedFindLocation;
      MenuItem.Bitmap := imgDisplayExpectedFindLocation.Picture.Bitmap;
      FSearchAreaMenu.Items.Add(MenuItem);

      MenuItem := TMenuItem.Create(FSearchAreaMenu);
      MenuItem.Caption := '-';
      FSearchAreaMenu.Items.Add(MenuItem);
    end
    else
      FSearchAreaDbgImgSearchedBmpMenu.Items.Clear;

    FSearchAreaScrBox.PopupMenu := FSearchAreaMenu;
    FSearchAreaControlDbgImg.PopupMenu := FSearchAreaMenu;
    FSearchAreaSearchedBmpDbgImg.PopupMenu := FSearchAreaMenu;
    FSearchAreaSearchedTextDbgImg.PopupMenu := FSearchAreaMenu;
    FSearchAreaGridImg.PopupMenu := FSearchAreaMenu;

    FSearchAreaLeftLimitLabel.Height := CSearchAreaMaxHeight;//FSearchAreaScrBox.Height;
    FSearchAreaTopLimitLabel.Width := CSearchAreaMaxWidth;//FSearchAreaScrBox.Width;
    FSearchAreaRightLimitLabel.Height := CSearchAreaMaxHeight;//FFSearchAreaScrBox.Height;
    FSearchAreaBottomLimitLabel.Width := CSearchAreaMaxWidth;//FSearchAreaScrBox.Width;

    FSearchAreaLeftLimitLabel_ForMinErr.Height := CSearchAreaMaxHeight;//FSearchAreaScrBox.Height;
    FSearchAreaTopLimitLabel_ForMinErr.Width := CSearchAreaMaxWidth;//FSearchAreaScrBox.Width;
    FSearchAreaRightLimitLabel_ForMinErr.Height := CSearchAreaMaxHeight;//FFSearchAreaScrBox.Height;
    FSearchAreaBottomLimitLabel_ForMinErr.Width := CSearchAreaMaxWidth;//FSearchAreaScrBox.Width;

    //
    {FSearchAreaLeftLimitLabel.Anchors := FSearchAreaLeftLimitLabel.Anchors + [akBottom];
    FSearchAreaTopLimitLabel.Anchors := FSearchAreaTopLimitLabel.Anchors + [akRight];
    FSearchAreaRightLimitLabel.Anchors := FSearchAreaRightLimitLabel.Anchors + [akBottom];
    FSearchAreaBottomLimitLabel.Anchors := FSearchAreaBottomLimitLabel.Anchors + [akRight];}
    //

    FindControlOptions := DoOnGetFindControlOptions;

    ControlLeft := StrToIntDef(EvaluateReplacements('$Control_Left$'), 0);
    ControlTop := StrToIntDef(EvaluateReplacements('$Control_Top$'), 0);

    FSearchAreaSearchedBmpDbgImg.Left := StrToIntDef(EvaluateReplacements(FindControlOptions^.InitialRectangle.LeftOffset), 0);
    FSearchAreaSearchedBmpDbgImg.Top := StrToIntDef(EvaluateReplacements(FindControlOptions^.InitialRectangle.TopOffset), 0);

    FSearchAreaSearchedBmpDbgImg.Left := FSearchAreaSearchedBmpDbgImg.Left - (ControlLeft - StrToIntDef(EvaluateReplacements(FindControlOptions^.InitialRectangle.Left), 0)) * Ord(not FindControlOptions^.UseWholeScreen);
    FSearchAreaSearchedBmpDbgImg.Top := FSearchAreaSearchedBmpDbgImg.Top - (ControlTop - StrToIntDef(EvaluateReplacements(FindControlOptions^.InitialRectangle.Top), 0)) * Ord(not FindControlOptions^.UseWholeScreen);

    FSearchAreaSearchedTextDbgImg.Left := FSearchAreaSearchedBmpDbgImg.Left;
    FSearchAreaSearchedTextDbgImg.Top := FSearchAreaSearchedBmpDbgImg.Top;

    FSearchAreaLeftLimitLabel.Left := FSearchAreaSearchedBmpDbgImg.Left;
    FSearchAreaTopLimitLabel.Top := FSearchAreaSearchedBmpDbgImg.Top;

    FTransparent_SearchAreaLeftLimitLabel.Left := FSearchAreaLeftLimitLabel.Left;
    FTransparent_SearchAreaTopLimitLabel.Top := FSearchAreaTopLimitLabel.Top;
    //tmrUpdateGrid.Enabled := True;

    FSearchAreaRightLimitLabel.Left := StrToIntDef(EvaluateReplacements(FindControlOptions^.InitialRectangle.RightOffset), 0);
    FSearchAreaBottomLimitLabel.Top := StrToIntDef(EvaluateReplacements(FindControlOptions^.InitialRectangle.BottomOffset), 0);

    if FindControlOptions^.UseWholeScreen then
    begin
      FSearchAreaRightLimitLabel.Left := FSearchAreaRightLimitLabel.Left + Screen.Width;
      FSearchAreaBottomLimitLabel.Top := FSearchAreaBottomLimitLabel.Top + Screen.Height;
    end
    else
    begin
      FSearchAreaRightLimitLabel.Left := FSearchAreaRightLimitLabel.Left - (ControlLeft - StrToIntDef(EvaluateReplacements(FindControlOptions^.InitialRectangle.Right), 0));
      FSearchAreaBottomLimitLabel.Top := FSearchAreaBottomLimitLabel.Top - (ControlTop - StrToIntDef(EvaluateReplacements(FindControlOptions^.InitialRectangle.Bottom), 0));
    end;

    UpdateTransparent_SearchAreaLimitsFromSearchAreaLimits;
    UpdateSearchAreaLabelColorsFromTheirPosition;
    ///////////////////////

    PopulateDbgImgExtraMenu;

    spdbtnDisplaySearchAreaDbgImgMenu.Enabled := True;

    FSearchAreaControlDbgImg.Width := Max(100, SearchAreaControlRect.Width);
    FSearchAreaControlDbgImg.Height := Max(100, SearchAreaControlRect.Height);
    FSearchAreaControlDbgImg.Picture.Bitmap := TBitmap.Create;
    FSearchAreaControlDbgImg.Picture.Bitmap.Width := FSearchAreaControlDbgImg.Width;
    FSearchAreaControlDbgImg.Picture.Bitmap.Height := FSearchAreaControlDbgImg.Height;

    if SearchAreaControlHandle = 0 then
    begin
      FSearchAreaControlDbgImg.Canvas.Pen.Color := clRed;
      FSearchAreaControlDbgImg.Canvas.Brush.Color := clWhite;
      FSearchAreaControlDbgImg.Canvas.Rectangle(0, 0, FSearchAreaControlDbgImg.Width - 1, FSearchAreaControlDbgImg.Height - 1);

      Line(FSearchAreaControlDbgImg.Canvas, 0, 0, FSearchAreaControlDbgImg.Width, FSearchAreaControlDbgImg.Height);
      FSearchAreaControlDbgImg.Canvas.TextOut(10, 10, 'Invalid bk handle.');
      FSearchAreaControlDbgImg.Canvas.TextOut(10, 30, 'Can''t get bmp.');
    end
    else
      ScreenShot(SearchAreaControlHandle,
                 FSearchAreaControlDbgImg.Picture.Bitmap,
                 SearchAreaControlRect.Left,
                 SearchAreaControlRect.Top,
                 SearchAreaControlRect.Width,
                 SearchAreaControlRect.Height);
  except
    on E: Exception do
      MessageBox(Handle, PChar('Debug img: ' + E.Message), PChar(Caption), MB_ICONERROR);
  end;
end;


procedure TfrClickerFindControl.btnDisplaySearchAreaDebuggingImageClick(Sender: TObject);
var
  TempBmp: TBitmap;
begin
  DisplayDebuggingImage;  //call this, before working with FSearchAreaControlDbgImg, because it might be nil, so it has to be created

  TempBmp := TBitmap.Create;
  try
    if DoOnGetExtraSearchAreaDebuggingImage(TempBmp) then
    begin
      FSearchAreaControlDbgImg.Width := TempBmp.Width;
      FSearchAreaControlDbgImg.Height := TempBmp.Height;

      WipeImage(FSearchAreaControlDbgImg, TempBmp.Width, TempBmp.Height);
      FSearchAreaControlDbgImg.Canvas.Draw(0, 0, TempBmp);

      DisplayDebuggingImage; //call again, to update the content
    end;
  finally
    TempBmp.Free;
  end;
end;


function TfrClickerFindControl.GetSelectedBMPTextTab: Integer;
begin
  Result := tabctrlBMPText.TabIndex;
end;


procedure TfrClickerFindControl.SetSelectedBMPTextTab(Value: Integer);
begin
  if Value > tabctrlBMPText.Tabs.Count - 1 then
    Value := tabctrlBMPText.Tabs.Count - 1;

  if Value < 0 then
    Value := 0;

  tabctrlBMPText.TabIndex := Value;
  SetBMPTextFrameVisibility;
end;


procedure TfrClickerFindControl.SetGridDrawingOption(Value: TDisplayGridLineOption);
begin
  if FGridDrawingOption <> Value then
  begin
    FGridDrawingOption := Value;
    RefreshGrid;
  end;
end;


procedure TfrClickerFindControl.SetPreviewSelectionColors(Value: TSelectionColors);
begin
  if (FPreviewSelectionColors.TopLeft_Valid <> Value.TopLeft_Valid) or
     (FPreviewSelectionColors.BotRight_Valid <> Value.BotRight_Valid) or
     (FPreviewSelectionColors.TopLeft_Invalid <> Value.TopLeft_Invalid) or
     (FPreviewSelectionColors.BotRight_Invalid <> Value.BotRight_Invalid) then
  begin
    FPreviewSelectionColors := Value;
    UpdateSearchAreaLabelColorsFromTheirPosition;
  end;
end;


function TfrClickerFindControl.GetSearch_EditBoxVar_Ref(AEditBoxValue, AVarName: string): Integer;
begin
  Result := 0;
  if AEditBoxValue <> AVarName then
    Result := StrToIntDef(EvaluateReplacements(AVarName), 0) - StrToIntDef(EvaluateReplacements(AEditBoxValue), 0);
end;


function TfrClickerFindControl.GetSearch_BottomBottom_Ref: Integer;
begin
  Result := GetSearch_EditBoxVar_Ref(DoOnGetFindControlOptions^.InitialRectangle.Bottom, '$Control_Bottom$');
end;


function TfrClickerFindControl.GetSearch_TopBottom_Ref: Integer;
begin
  Result := GetSearch_EditBoxVar_Ref(DoOnGetFindControlOptions^.InitialRectangle.Top, '$Control_Bottom$');
end;


function TfrClickerFindControl.GetSearch_LeftLeft_Ref: Integer;
begin
  Result := GetSearch_EditBoxVar_Ref(DoOnGetFindControlOptions^.InitialRectangle.Left, '$Control_Left$');
end;


function TfrClickerFindControl.GetSearch_RightLeft_Ref: Integer;
begin
  Result := GetSearch_EditBoxVar_Ref(DoOnGetFindControlOptions^.InitialRectangle.Right, '$Control_Left$');
end;


function TfrClickerFindControl.GetSearch_RightRight_Ref: Integer;
begin
  Result := GetSearch_EditBoxVar_Ref(DoOnGetFindControlOptions^.InitialRectangle.Right, '$Control_Right$');
end;


function TfrClickerFindControl.GetSearch_LeftRight_Ref: Integer;
begin
  Result := GetSearch_EditBoxVar_Ref(DoOnGetFindControlOptions^.InitialRectangle.Left, '$Control_Right$');
end;


function TfrClickerFindControl.GetSearch_TopTop_Ref: Integer;
begin
  Result := GetSearch_EditBoxVar_Ref(DoOnGetFindControlOptions^.InitialRectangle.Top, '$Control_Top$');
end;


function TfrClickerFindControl.GetSearch_BottomTop_Ref: Integer;
begin
  Result := GetSearch_EditBoxVar_Ref(DoOnGetFindControlOptions^.InitialRectangle.Bottom, '$Control_Top$');
end;


function TfrClickerFindControl.GetControlWidthFromReplacement: Integer;
begin
  Result := StrToIntDef(EvaluateReplacements('$Control_Width$'), 0);
end;


function TfrClickerFindControl.GetControlHeightFromReplacement: Integer;
begin
  Result := StrToIntDef(EvaluateReplacements('$Control_Height$'), 0);
end;


function TfrClickerFindControl.GetSearch_BottomBottom_Ref_FromInitRect(AInitialRectange: TRectString): Integer;
begin
  Result := GetSearch_EditBoxVar_Ref(AInitialRectange.Bottom, '$Control_Bottom$');
end;


function TfrClickerFindControl.GetSearch_TopBottom_Ref_FromInitRect(AInitialRectange: TRectString): Integer;
begin
  Result := GetSearch_EditBoxVar_Ref(AInitialRectange.Top, '$Control_Bottom$');
end;


function TfrClickerFindControl.GetSearch_LeftLeft_Ref_FromInitRect(AInitialRectange: TRectString): Integer;
begin
  Result := GetSearch_EditBoxVar_Ref(AInitialRectange.Left, '$Control_Left$');
end;


function TfrClickerFindControl.GetSearch_RightLeft_Ref_FromInitRect(AInitialRectange: TRectString): Integer;
begin
  Result := GetSearch_EditBoxVar_Ref(AInitialRectange.Right, '$Control_Left$');
end;


function TfrClickerFindControl.GetSearch_RightRight_Ref_FromInitRect(AInitialRectange: TRectString): Integer;
begin
  Result := GetSearch_EditBoxVar_Ref(AInitialRectange.Right, '$Control_Right$');
end;


function TfrClickerFindControl.GetSearch_LeftRight_Ref_FromInitRect(AInitialRectange: TRectString): Integer;
begin
  Result := GetSearch_EditBoxVar_Ref(AInitialRectange.Left, '$Control_Right$');
end;


function TfrClickerFindControl.GetSearch_TopTop_Ref_FromInitRect(AInitialRectange: TRectString): Integer;
begin
  Result := GetSearch_EditBoxVar_Ref(AInitialRectange.Top, '$Control_Top$');
end;


function TfrClickerFindControl.GetSearch_BottomTop_Ref_FromInitRect(AInitialRectange: TRectString): Integer;
begin
  Result := GetSearch_EditBoxVar_Ref(AInitialRectange.Bottom, '$Control_Top$');
end;


procedure TfrClickerFindControl.UpdateSearchAreaLabelsFromInitRect(AInitialRectange: TRectString);
var
  ControlWidth, ControlHeight: Integer;
begin
  if FSearchAreaLeftLimitLabel = nil then
    Exit;   //better exit than swallow exception, because the debugger keeps catching this one, and it becomes unsable

  try
    FSearchAreaLeftLimitLabel.Left := StrToIntDef(EvaluateReplacements(AInitialRectange.LeftOffset), 20) - GetSearch_LeftLeft_Ref_FromInitRect(AInitialRectange);
    FSearchAreaTopLimitLabel.Top := StrToIntDef(EvaluateReplacements(AInitialRectange.TopOffset), 20) - GetSearch_TopTop_Ref_FromInitRect(AInitialRectange);

    ControlWidth := GetControlWidthFromReplacement;
    FSearchAreaRightLimitLabel.Left := StrToIntDef(EvaluateReplacements(AInitialRectange.RightOffset), 20) - GetSearch_RightRight_Ref_FromInitRect(AInitialRectange) + ControlWidth;

    ControlHeight := GetControlHeightFromReplacement;
    FSearchAreaBottomLimitLabel.Top := StrToIntDef(EvaluateReplacements(AInitialRectange.BottomOffset), 20) - GetSearch_BottomBottom_Ref_FromInitRect(AInitialRectange) + ControlHeight;

    UpdateTransparent_SearchAreaLimitsFromSearchAreaLimits;
  except
    //exception when the components are not created yet  (expected)
  end;
end;


procedure TfrClickerFindControl.UpdateSearchAreaLabelsFromKeysOnInitRect(AInitialRectange: TRectString);
begin
  if FSearchAreaSearchedBmpDbgImg = nil then
    Exit;

  UpdateSearchAreaLabelsFromInitRect(AInitialRectange);
  UpdateSearchAreaLabelColorsFromTheirPosition;
  tmrUpdateGrid.Enabled := True;
end;


procedure TfrClickerFindControl.GetOffsetArea(out CropLeft, CropRight, CropTop, CropBottom: Integer);
begin
  CropLeft := FSearchAreaLeftLimitLabel.Left;
  CropRight := FSearchAreaRightLimitLabel.Left;

  CropTop := FSearchAreaTopLimitLabel.Top;
  CropBottom := FSearchAreaBottomLimitLabel.Top;
end;


function TfrClickerFindControl.GetSearchAreaLeftOffsetFromBmpDbgImg: Integer;
var
  Ref: Integer;
begin
  Ref := GetSearch_LeftLeft_Ref;
  Result := FSearchAreaSearchedBmpDbgImg.Left + Ref;
end;


function TfrClickerFindControl.GetSearchAreaTopOffsetFromBmpDbgImg: Integer;
var
  Ref: Integer;
begin
  Ref := GetSearch_TopTop_Ref;
  Result := FSearchAreaSearchedBmpDbgImg.Top + Ref;
end;


function TfrClickerFindControl.GetSearchAreaRightOffsetFromBmpDbgImg: Integer;
var
  Ref: Integer;
begin
  Ref := GetSearch_RightRight_Ref;
  Result := FSearchAreaSearchedBmpDbgImg.Left + FSearchAreaSearchedBmpDbgImg.Width + Ref;
end;


function TfrClickerFindControl.GetSearchAreaBottomOffsetFromBmpDbgImg: Integer;
var
  Ref: Integer;
begin
  Ref := GetSearch_BottomBottom_Ref;
  Result := FSearchAreaSearchedBmpDbgImg.Top + FSearchAreaSearchedBmpDbgImg.Height + Ref;
end;


//===================================


function TfrClickerFindControl.GetSearchAreaLeftOffsetFromTxtDbgImg: Integer;
var
  Ref: Integer;
begin
  Ref := GetSearch_LeftLeft_Ref;
  Result := FSearchAreaSearchedTextDbgImg.Left + Ref;
end;


function TfrClickerFindControl.GetSearchAreaTopOffsetFromTxtDbgImg: Integer;
var
  Ref: Integer;
begin
  Ref := GetSearch_TopTop_Ref;
  Result := FSearchAreaSearchedTextDbgImg.Top + Ref;
end;


function TfrClickerFindControl.GetSearchAreaRightOffsetFromTxtDbgImg: Integer;
var
  Ref: Integer;
begin
  Ref := GetSearch_RightRight_Ref;
  Result := FSearchAreaSearchedTextDbgImg.Left + FSearchAreaSearchedTextDbgImg.Width + Ref;
end;


function TfrClickerFindControl.GetSearchAreaBottomOffsetFromTxtDbgImg: Integer;
var
  Ref: Integer;
begin
  Ref := GetSearch_BottomBottom_Ref;
  Result := FSearchAreaSearchedTextDbgImg.Top + FSearchAreaSearchedTextDbgImg.Height + Ref;
end;


//====================================

function TfrClickerFindControl.GetSearchAreaLeftOffsetFromSelLabel: Integer;
var
  Ref: Integer;
begin
  Ref := GetSearch_LeftLeft_Ref;
  Result := FSearchAreaLeftLimitLabel.Left + Ref;
end;


function TfrClickerFindControl.GetSearchAreaTopOffsetFromSelLabel: Integer;
var
  Ref: Integer;
begin
  Ref := GetSearch_TopTop_Ref;
  Result := FSearchAreaTopLimitLabel.Top + Ref;
end;


function TfrClickerFindControl.GetSearchAreaRightOffsetFromSelLabel: Integer;
var
  Ref: Integer;
begin
  Ref := GetSearch_RightRight_Ref;
  Result := FSearchAreaRightLimitLabel.Left + Ref;
end;


function TfrClickerFindControl.GetSearchAreaBottomOffsetFromSelLabel: Integer;
var
  Ref: Integer;
begin
  Ref := GetSearch_BottomBottom_Ref;
  Result := FSearchAreaBottomLimitLabel.Top + Ref;
end;


//==================================


procedure TfrClickerFindControl.UpdatePreviewIcons;
var
  ABitmap: TBitmap;
  AStream: TMemoryStream;
  i: Integer;
begin
  imglstMatchBitmapFiles.Clear;

  for i := 0 to lstMatchBitmapFiles.Count - 1 do
  begin
    ABitmap := TBitmap.Create;
    try
      if DoOnFileExists(lstMatchBitmapFiles.Items.Strings[i]) then
      begin
        if not DoOnLoadBitmap(ABitmap, lstMatchBitmapFiles.Items.Strings[i]) then
        begin
          ABitmap.Width := 50;
          ABitmap.Height := 50;
          ABitmap.Canvas.Font.Color := clMaroon;
          ABitmap.Canvas.Brush.Color := clWhite;
          ABitmap.Canvas.TextOut(0, 10, 'Not loaded');
        end;

        //set again, to resize
        ABitmap.Width := 50;
        ABitmap.Height := 50;
      end
      else
        if (FInMemFS <> nil) and FInMemFS.FileExistsInMem(lstMatchBitmapFiles.Items.Strings[i]) then
        begin
          AStream := TMemoryStream.Create;
          try
            FInMemFS.LoadFileFromMemToStream(lstMatchBitmapFiles.Items.Strings[i], AStream);
            AStream.Position := 0;
            ABitmap.LoadFromStream(AStream);
          finally
            AStream.Free;
          end;
        end
        else
        begin
          ABitmap.Canvas.Brush.Color := clYellow;
          ABitmap.Canvas.Font.Color := clRed;
          ABitmap.Canvas.TextOut(0, 0, 'Not found');
        end;

      imglstMatchBitmapFiles.AddMasked(ABitmap, 1);
    finally
      ABitmap.Free;
    end;
  end;
end;


procedure TfrClickerFindControl.UpdateListsOfSearchFiles(AMatchBitmapFiles, AMatchPrimitiveFiles: string);
begin
  lstMatchBitmapFiles.Items.Text := AMatchBitmapFiles;
  lstMatchPrimitiveFiles.Items.Text := AMatchPrimitiveFiles;
end;


procedure TfrClickerFindControl.UpdateSearchAreaLabelColorsFromTheirPosition;
begin
  if not Assigned(FSearchAreaLeftLimitLabel) then
    Exit;

  if FSearchAreaLeftLimitLabel.Left < FSearchAreaRightLimitLabel.Left then
  begin
    FSearchAreaLeftLimitLabel.Color := FPreviewSelectionColors.TopLeft_Valid; // CLabel_Orange;
    FSearchAreaRightLimitLabel.Color := FPreviewSelectionColors.BotRight_Valid; // CLabel_LightGreen;
  end
  else
  begin
    FSearchAreaLeftLimitLabel.Color := FPreviewSelectionColors.TopLeft_Invalid; // clRed;
    FSearchAreaRightLimitLabel.Color := FPreviewSelectionColors.BotRight_Invalid; // clMaroon;
  end;

  if FSearchAreaTopLimitLabel.Top < FSearchAreaBottomLimitLabel.Top then
  begin
    FSearchAreaTopLimitLabel.Color := FPreviewSelectionColors.TopLeft_Valid; // CLabel_Orange;
    FSearchAreaBottomLimitLabel.Color := FPreviewSelectionColors.BotRight_Valid; // CLabel_LightGreen;
  end
  else
  begin
    FSearchAreaTopLimitLabel.Color := FPreviewSelectionColors.TopLeft_Invalid; // clRed;
    FSearchAreaBottomLimitLabel.Color := FPreviewSelectionColors.BotRight_Invalid; // clMaroon;
  end;
end;


procedure TfrClickerFindControl.SetLabelsFromMouseOverDbgImgPixelColor(APixelColor: TColor);
var
  ColorStr, RR, GG, BB, VarName: string;
begin
  RR := IntToHex(APixelColor and $FF, 2);
  GG := IntToHex(APixelColor shr 8 and $FF, 2);
  BB := IntToHex(APixelColor shr 16 and $FF, 2);

  lblMouseOnDbgImgRR.Caption := RR;
  lblMouseOnDbgImgGG.Caption := GG;
  lblMouseOnDbgImgBB.Caption := BB;

  ColorStr := BB + GG + RR;
  VarName := ReverseEvaluateReplacements(ColorStr);
  if VarName = '' then
    lblColorUnderCursor.Caption := 'Unknown color'
  else
    lblColorUnderCursor.Caption := VarName;

  lblColorUnderCursorPreview.Color := APixelColor;
end;


procedure TfrClickerFindControl.SelectDbgImgByRectangle(X, Y: Integer);
begin
  if (Abs(X - FSelectingXStart) > 5) and (Abs(Y - FSelectingYStart) > 5) then
  begin
    FSearchAreaLeftLimitLabel.Left := Min(FSelectingXStart, FSearchAreaControlDbgImg.Width);
    FSearchAreaRightLimitLabel.Left := Min(X, FSearchAreaControlDbgImg.Width);
    FSearchAreaTopLimitLabel.Top := Min(FSelectingYStart, FSearchAreaControlDbgImg.Height);
    FSearchAreaBottomLimitLabel.Top := Min(Y, FSearchAreaControlDbgImg.Height);

    UpdateTransparent_SearchAreaLimitsFromSearchAreaLimits;
    UpdateSearchAreaLabelColorsFromTheirPosition;
    tmrUpdateSearchAreaOffsetEditBoxes.Enabled := True;
  end;
end;


procedure TfrClickerFindControl.SelectDbgImgByRectangleForMinErr(X, Y: Integer);
var
  Ph: Integer;
begin
  if (Abs(X - FExpectedErrLevel_TopLeft.X) > 3) and (Abs(Y - FExpectedErrLevel_TopLeft.Y) > 3) then
  begin
    FExpectedErrLevel_BotRight.X := X;
    FExpectedErrLevel_BotRight.Y := Y;

    if FExpectedErrLevel_TopLeft.X > FExpectedErrLevel_BotRight.X then
    begin
      Ph := FExpectedErrLevel_TopLeft.X;
      FExpectedErrLevel_TopLeft.X := FExpectedErrLevel_BotRight.X;
      FExpectedErrLevel_BotRight.X := Ph;
    end;

    if FExpectedErrLevel_TopLeft.Y > FExpectedErrLevel_BotRight.Y then
    begin
      Ph := FExpectedErrLevel_TopLeft.Y;
      FExpectedErrLevel_TopLeft.Y := FExpectedErrLevel_BotRight.Y;
      FExpectedErrLevel_BotRight.Y := Ph;
    end;

    FSearchAreaLeftLimitLabel_ForMinErr.Left := Min(FExpectedErrLevel_TopLeft.X, FSearchAreaControlDbgImg.Width);
    FSearchAreaRightLimitLabel_ForMinErr.Left := Min(X, FSearchAreaControlDbgImg.Width);
    FSearchAreaTopLimitLabel_ForMinErr.Top := Min(FExpectedErrLevel_TopLeft.Y, FSearchAreaControlDbgImg.Height);
    FSearchAreaBottomLimitLabel_ForMinErr.Top := Min(Y, FSearchAreaControlDbgImg.Height);
  end;
end;


procedure TfrClickerFindControl.imgSearchAreaControlDbgMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Sender = FSearchAreaGridImg then
  begin
    Inc(X, FSearchAreaGridImg.Left);
    Inc(Y, FSearchAreaGridImg.Top);
  end;

  lblMouseOnDbgImg.Caption := IntToStr(X) + ' : ' + IntToStr(Y);
  SetLabelsFromMouseOverDbgImgPixelColor(FSearchAreaControlDbgImg.Canvas.Pixels[X, Y]);

  if FRectangleSelectingForMinErr then
    SelectDbgImgByRectangleForMinErr(X, Y)
  else
    if FRectangleSelecting then
      SelectDbgImgByRectangle(X, Y);

  FCurrentMousePosOnPreviewImg.X := X;
  FCurrentMousePosOnPreviewImg.Y := Y;
  FSkipDrawingGrid := True;  //set this, to avoid grid fickering
  tmrDrawZoom.Enabled := True;
end;


procedure TfrClickerFindControl.imgSearchAreaControlDbgMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if Sender = FSearchAreaGridImg then
    begin
      Inc(X, FSearchAreaGridImg.Left);
      Inc(Y, FSearchAreaGridImg.Top);
    end;

    if ssAlt in Shift then
    begin
      FRectangleSelectingForMinErr := True;
      FExpectedErrLevel_TopLeft.X := X;
      FExpectedErrLevel_TopLeft.Y := Y;
      DisplaySelectionLinesForExpectedFindLocation;
    end
    else
    begin
      FRectangleSelecting := True;
      FSelectingXStart := X;
      FSelectingYStart := Y;
    end;
  end;
end;


procedure TfrClickerFindControl.imgSearchAreaControlDbgMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if FRectangleSelecting then
      DoOnTriggerOnControlsModified;

    FRectangleSelectingForMinErr := False; //set both to False, because the user might release the Alt key, while dragging
    FRectangleSelecting := False;
  end;
end;


procedure TfrClickerFindControl.imgSearchAreaControlDbgMouseEnter(Sender: TObject);
var
  tp: TPoint;
begin
  FSearchAreaControlDbgImg.ShowHint := False;
  FSearchAreaGridImg.ShowHint := False;
  GetCursorPos(tp);
  ShowZoom(tp.X + 50, tp.Y + 50);
end;


procedure TfrClickerFindControl.imgSearchAreaControlDbgMouseLeave(Sender: TObject);
begin
  FSearchAreaControlDbgImg.ShowHint := True;
  FSearchAreaGridImg.ShowHint := True;
  HideZoom;
end;


//the small bmp overlayed on top of  imgSearchAreaControlDbg
procedure TfrClickerFindControl.imgSearchAreaSearchedBmpDbgImgMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  tp: TPoint;
  NewLeft, NewTop: Integer;
  Offsets: TSimpleRectString;
begin
  if FDbgImgHold and (ssLeft in Shift) then
  begin
    GetCursorPos(tp);
    NewLeft := FMouseDownComponentPos.X + tp.X - FMouseDownGlobalPos.X;
    NewTop := FMouseDownComponentPos.Y + tp.Y - FMouseDownGlobalPos.Y;

    if (NewLeft >= 0) and (NewLeft <= Screen.Width - FSearchAreaSearchedBmpDbgImg.Width) then
      FSearchAreaSearchedBmpDbgImg.Left := NewLeft;

    if (NewTop >= 0) and (NewTop <= Screen.Height - FSearchAreaSearchedBmpDbgImg.Height) then
      FSearchAreaSearchedBmpDbgImg.Top := NewTop;

    if ssCtrl in Shift then
    begin
      FSearchAreaLeftLimitLabel.Left := FSearchAreaSearchedBmpDbgImg.Left;
      FSearchAreaTopLimitLabel.Top := FSearchAreaSearchedBmpDbgImg.Top;
      tmrUpdateGrid.Enabled := True;

      //lbeSearchRectLeftOffset.Text := IntToStr(GetSearchAreaLeftOffsetFromBmpDbgImg);
      //lbeSearchRectTopOffset.Text := IntToStr(GetSearchAreaTopOffsetFromBmpDbgImg);
      Offsets.Left := IntToStr(GetSearchAreaLeftOffsetFromBmpDbgImg);
      Offsets.Top := IntToStr(GetSearchAreaTopOffsetFromBmpDbgImg);
      DoOnUpdateSearchAreaLimitsInOIFromDraggingLines([llLeft, llTop], Offsets);

      if ssShift in Shift then
      begin
        FSearchAreaRightLimitLabel.Left := FSearchAreaSearchedBmpDbgImg.Left + FSearchAreaSearchedBmpDbgImg.Width;
        FSearchAreaBottomLimitLabel.Top := FSearchAreaSearchedBmpDbgImg.Top + FSearchAreaSearchedBmpDbgImg.Height;

        //lbeSearchRectRightOffset.Text := IntToStr(GetSearchAreaRightOffsetFromBmpDbgImg - GetControlWidthFromReplacement);
        //lbeSearchRectBottomOffset.Text := IntToStr(GetSearchAreaBottomOffsetFromBmpDbgImg - GetControlHeightFromReplacement);
        Offsets.Right := IntToStr(GetSearchAreaRightOffsetFromBmpDbgImg - GetControlWidthFromReplacement);
        Offsets.Bottom := IntToStr(GetSearchAreaBottomOffsetFromBmpDbgImg - GetControlHeightFromReplacement);
        DoOnUpdateSearchAreaLimitsInOIFromDraggingLines([llRight, llBottom], Offsets);
      end;

      UpdateTransparent_SearchAreaLimitsFromSearchAreaLimits;
      DoOnTriggerOnControlsModified;
    end;

    UpdateSearchAreaLabelColorsFromTheirPosition;
  end;

  lblMouseOnDbgImg.Caption := IntToStr(X + FSearchAreaSearchedBmpDbgImg.Left) + ' : ' + IntToStr(Y + FSearchAreaSearchedBmpDbgImg.Top);
  SetLabelsFromMouseOverDbgImgPixelColor(FSearchAreaSearchedBmpDbgImg.Canvas.Pixels[X, Y]);

  FCurrentMousePosOnPreviewImg.X := X + FSearchAreaSearchedBmpDbgImg.Left;
  FCurrentMousePosOnPreviewImg.Y := Y + FSearchAreaSearchedBmpDbgImg.Top;
  tmrDrawZoom.Enabled := True;
end;


procedure TfrClickerFindControl.imgSearchAreaSearchedBmpDbgImgMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GetCursorPos(FMouseDownGlobalPos);  //mouse coordinates
  if not FDbgImgHold then
  begin
    FDbgImgHold := True;
    FMouseDownComponentPos.X := FSearchAreaSearchedBmpDbgImg.Left; //component coordinates on the window
    FMouseDownComponentPos.Y := FSearchAreaSearchedBmpDbgImg.Top;
  end;
end;


procedure TfrClickerFindControl.imgSearchAreaSearchedBmpDbgImgMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDbgImgHold := False;
  FSearchAreaSearchedTextDbgImg.Left := FSearchAreaSearchedBmpDbgImg.Left;
  FSearchAreaSearchedTextDbgImg.Top := FSearchAreaSearchedBmpDbgImg.Top;
end;


procedure TfrClickerFindControl.imgSearchAreaSearchedBmpDbgMouseEnter(Sender: TObject);
var
  tp: TPoint;
begin
  FSearchAreaSearchedBmpDbgImg.ShowHint := False;
  GetCursorPos(tp);
  ShowZoom(tp.X + 50, tp.Y + 50);
end;


procedure TfrClickerFindControl.imgSearchAreaSearchedBmpDbgMouseLeave(Sender: TObject);
begin
  FSearchAreaSearchedBmpDbgImg.ShowHint := True;
end;


//the small bmp (with text) overlayed on top of  imgSearchAreaControlDbg
procedure TfrClickerFindControl.imgSearchAreaSearchedTextDbgImgMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  tp: TPoint;
  NewLeft, NewTop: Integer;
  Offsets: TSimpleRectString;
begin
  if FDbgImgHold and (ssLeft in Shift) then
  begin
    GetCursorPos(tp);
    NewLeft := FMouseDownComponentPos.X + tp.X - FMouseDownGlobalPos.X;
    NewTop := FMouseDownComponentPos.Y + tp.Y - FMouseDownGlobalPos.Y;

    if (NewLeft >= 0) and (NewLeft <= Screen.Width - FSearchAreaSearchedTextDbgImg.Width) then
      FSearchAreaSearchedTextDbgImg.Left := NewLeft;

    if (NewTop >= 0) and (NewTop <= Screen.Height - FSearchAreaSearchedTextDbgImg.Height) then
      FSearchAreaSearchedTextDbgImg.Top := NewTop;

    if ssCtrl in Shift then
    begin
      FSearchAreaLeftLimitLabel.Left := FSearchAreaSearchedTextDbgImg.Left;
      FSearchAreaTopLimitLabel.Top := FSearchAreaSearchedTextDbgImg.Top;
      tmrUpdateGrid.Enabled := True;

      //lbeSearchRectLeftOffset.Text := IntToStr(GetSearchAreaLeftOffsetFromTxtDbgImg);
      //lbeSearchRectTopOffset.Text := IntToStr(GetSearchAreaTopOffsetFromTxtDbgImg);
      Offsets.Left := IntToStr(GetSearchAreaLeftOffsetFromTxtDbgImg);
      Offsets.Top := IntToStr(GetSearchAreaTopOffsetFromTxtDbgImg);
      DoOnUpdateSearchAreaLimitsInOIFromDraggingLines([llLeft, llTop], Offsets);

      if ssShift in Shift then
      begin
        FSearchAreaRightLimitLabel.Left := FSearchAreaSearchedTextDbgImg.Left + FSearchAreaSearchedTextDbgImg.Width;
        FSearchAreaBottomLimitLabel.Top := FSearchAreaSearchedTextDbgImg.Top + FSearchAreaSearchedTextDbgImg.Height;

        //lbeSearchRectRightOffset.Text := IntToStr(GetSearchAreaRightOffsetFromTxtDbgImg - GetControlWidthFromReplacement);
        //lbeSearchRectBottomOffset.Text := IntToStr(GetSearchAreaBottomOffsetFromTxtDbgImg - GetControlHeightFromReplacement);
        Offsets.Right := IntToStr(GetSearchAreaRightOffsetFromTxtDbgImg - GetControlWidthFromReplacement);
        Offsets.Bottom := IntToStr(GetSearchAreaBottomOffsetFromTxtDbgImg - GetControlHeightFromReplacement);
        DoOnUpdateSearchAreaLimitsInOIFromDraggingLines([llRight, llBottom], Offsets);
      end;

      UpdateTransparent_SearchAreaLimitsFromSearchAreaLimits;
      DoOnTriggerOnControlsModified;
    end;

    UpdateSearchAreaLabelColorsFromTheirPosition;
  end;

  lblMouseOnDbgImg.Caption := IntToStr(X + FSearchAreaSearchedTextDbgImg.Left) + ' : ' + IntToStr(Y + FSearchAreaSearchedTextDbgImg.Top);
  SetLabelsFromMouseOverDbgImgPixelColor(FSearchAreaSearchedTextDbgImg.Canvas.Pixels[X, Y]);

  FCurrentMousePosOnPreviewImg.X := X + FSearchAreaSearchedTextDbgImg.Left;
  FCurrentMousePosOnPreviewImg.Y := Y + FSearchAreaSearchedTextDbgImg.Top;
  tmrDrawZoom.Enabled := True;
end;


procedure TfrClickerFindControl.imgSearchAreaSearchedTextDbgImgMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GetCursorPos(FMouseDownGlobalPos);  //mouse coordinates
  if not FDbgImgHold then
  begin
    FDbgImgHold := True;
    FMouseDownComponentPos.X := FSearchAreaSearchedTextDbgImg.Left; //component coordinates on the window
    FMouseDownComponentPos.Y := FSearchAreaSearchedTextDbgImg.Top;
  end;
end;


procedure TfrClickerFindControl.imgSearchAreaSearchedTextDbgImgMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDbgImgHold := False;
  FSearchAreaSearchedBmpDbgImg.Left := FSearchAreaSearchedTextDbgImg.Left;
  FSearchAreaSearchedBmpDbgImg.Top := FSearchAreaSearchedTextDbgImg.Top;
end;


procedure TfrClickerFindControl.imgSearchAreaSearchedTextDbgMouseEnter(Sender: TObject);
var
  tp: TPoint;
begin
  FSearchAreaSearchedTextDbgImg.ShowHint := False;
  GetCursorPos(tp);
  ShowZoom(tp.X + 50, tp.Y + 50);
end;


procedure TfrClickerFindControl.imgSearchAreaSearchedTextDbgMouseLeave(Sender: TObject);
begin
  FSearchAreaSearchedTextDbgImg.ShowHint := True;
end;


procedure TfrClickerFindControl.MenuItemCopySearchAreaBkImgToClipboardClick(Sender: TObject);
var
  TempBmp: TBitmap;
begin
  if FSearchAreaControlDbgImg = nil then
  begin
    MessageBox(Handle, 'Debug image is not available.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  TempBmp := TBitmap.Create;
  try
    TempBmp.Width := FSearchAreaControlDbgImg.Width;
    TempBmp.Height := FSearchAreaControlDbgImg.Height;
    TempBmp.Canvas.Draw(0, 0, FSearchAreaControlDbgImg.Picture.Bitmap);

    if chkShowGridOnBMPPreview.Checked then
    begin
      GeneratePreviewGridContent(FGridDrawingOption);
      TempBmp.Canvas.Draw(FSearchAreaGridImg.Left, FSearchAreaGridImg.Top, FSearchAreaGridImg.Picture.Bitmap);
    end;

    Clipboard.Assign(TempBmp);  //only works with temp bitmap. Maybe, it's because of pixel format (24-bit vs device  or 32-bit).
  finally
    TempBmp.Free;
  end;
  //Clipboard.Assign(FSearchAreaControlDbgImg.Picture.Bitmap);     //does not work :(
end;


procedure TfrClickerFindControl.MenuItemCopySearchAreaSearchBmpImgToClipboardClick(Sender: TObject);
begin
  if FSearchAreaControlDbgImg = nil then
  begin
    MessageBox(Handle, 'Debug image is not available.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  Clipboard.Assign(FSearchAreaSearchedBmpDbgImg.Picture.Bitmap);
end;


procedure TfrClickerFindControl.MenuItemCopySearchAreaSearchTextImgToClipboardClick(Sender: TObject);
begin
  if FSearchAreaControlDbgImg = nil then
  begin
    MessageBox(Handle, 'Debug image (text preview) is not available.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  Clipboard.Assign(FSearchAreaSearchedTextDbgImg.Picture.Bitmap);
end;


procedure TfrClickerFindControl.MenuItemCopySearchAreaAllToClipboardClick(Sender: TObject);
var
  TempBmp: TBitmap;
begin
  if FSearchAreaControlDbgImg = nil then
  begin
    MessageBox(Handle, 'Debug image is not available.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  TempBmp := TBitmap.Create;
  try
    TempBmp.Width := FSearchAreaControlDbgImg.Width;
    TempBmp.Height := FSearchAreaControlDbgImg.Height;
    TempBmp.Canvas.Draw(0, 0, FSearchAreaControlDbgImg.Picture.Bitmap);

    if chkShowGridOnBMPPreview.Checked then
    begin
      GeneratePreviewGridContent(FGridDrawingOption);
      TempBmp.Canvas.Draw(FSearchAreaGridImg.Left, FSearchAreaGridImg.Top, FSearchAreaGridImg.Picture.Bitmap);
    end;

    TempBmp.Canvas.Pen.Color := FSearchAreaLeftLimitLabel.Color;
    Line(TempBmp.Canvas, FSearchAreaLeftLimitLabel.Left, 0, FSearchAreaLeftLimitLabel.Left, TempBmp.Height - 1);

    TempBmp.Canvas.Pen.Color := FSearchAreaTopLimitLabel.Color;
    Line(TempBmp.Canvas, 0, FSearchAreaTopLimitLabel.Top, TempBmp.Width - 1, FSearchAreaTopLimitLabel.Top);

    TempBmp.Canvas.Pen.Color := FSearchAreaRightLimitLabel.Color;
    Line(TempBmp.Canvas, FSearchAreaRightLimitLabel.Left, 0, FSearchAreaRightLimitLabel.Left, TempBmp.Height - 1);

    TempBmp.Canvas.Pen.Color := FSearchAreaBottomLimitLabel.Color;
    Line(TempBmp.Canvas, 0, FSearchAreaBottomLimitLabel.Top, TempBmp.Width - 1, FSearchAreaBottomLimitLabel.Top);

    if chkShowBMPTextDbgImg.Checked then
      TempBmp.Canvas.Draw(FSearchAreaSearchedTextDbgImg.Left, FSearchAreaSearchedTextDbgImg.Top, FSearchAreaSearchedTextDbgImg.Picture.Bitmap);

    if chkShowBMPFileDbgImg.Checked then
      TempBmp.Canvas.Draw(FSearchAreaSearchedBmpDbgImg.Left, FSearchAreaSearchedBmpDbgImg.Top, FSearchAreaSearchedBmpDbgImg.Picture.Bitmap);

    Clipboard.Assign(TempBmp);
  finally
    TempBmp.Free;
  end;
end;


procedure TfrClickerFindControl.MenuItemCopySearchAreaSelectedAreaFromBkToClipboardClick(Sender: TObject);
var
  TempBmp: TBitmap;
  CropLeft, CropRight, CropTop, CropBottom: Integer;
  SrcRect, DestRect: TRect;
begin
  if FSearchAreaControlDbgImg = nil then
  begin
    MessageBox(Handle, 'Debug image is not available.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  GetOffsetArea(CropLeft, CropRight, CropTop, CropBottom);

  TempBmp := TBitmap.Create;
  try
    TempBmp.Width := CropRight - CropLeft;
    TempBmp.Height := CropBottom - CropTop;
    TempBmp.Canvas.Pen.Color := clWhite;
    TempBmp.Canvas.Brush.Color := TempBmp.Canvas.Pen.Color;
    TempBmp.Canvas.Rectangle(0, 0, TempBmp.Width, TempBmp.Height);   // -1

    SrcRect.Left := CropLeft;
    SrcRect.Right := CropRight;
    SrcRect.Top := CropTop;
    SrcRect.Bottom := CropBottom;

    DestRect.Left := 0;
    DestRect.Top := 0;
    DestRect.Width := TempBmp.Width;
    DestRect.Height := TempBmp.Height;

    TempBmp.Canvas.CopyRect(DestRect, FSearchAreaControlDbgImg.Canvas, SrcRect);

    Clipboard.Assign(TempBmp);
  finally
    TempBmp.Free;
  end;
end;


procedure TfrClickerFindControl.MenuItemUpdateLeftAndTopOffsetsFromPreviewTextImageToEditboxes(Sender: TObject);
var
  Offsets: TSimpleRectString;
begin
  try
    FSearchAreaLeftLimitLabel.Left := FSearchAreaSearchedTextDbgImg.Left;
    FSearchAreaTopLimitLabel.Top := FSearchAreaSearchedTextDbgImg.Top;
    UpdateTransparent_SearchAreaLimitsFromSearchAreaLimits;
    tmrUpdateGrid.Enabled := True;

    //lbeSearchRectLeftOffset.Text := IntToStr(GetSearchAreaLeftOffsetFromTxtDbgImg);
    //lbeSearchRectTopOffset.Text := IntToStr(GetSearchAreaTopOffsetFromTxtDbgImg);
    Offsets.Left := IntToStr(GetSearchAreaLeftOffsetFromTxtDbgImg);
    Offsets.Top := IntToStr(GetSearchAreaTopOffsetFromTxtDbgImg);
    DoOnUpdateSearchAreaLimitsInOIFromDraggingLines([llLeft, llTop], Offsets);

    UpdateSearchAreaLabelColorsFromTheirPosition;
  except
    //this will throw AV on uninitialized components or out of bounds values
  end;
end;


procedure TfrClickerFindControl.MenuItemUpdateLeftTopRightBottomOffsetsFromPreviewTextImageToEditboxes(Sender: TObject);
var
  Offsets: TSimpleRectString;
begin
  try
    FSearchAreaLeftLimitLabel.Left := FSearchAreaSearchedBmpDbgImg.Left;
    FSearchAreaTopLimitLabel.Top := FSearchAreaSearchedBmpDbgImg.Top;
    tmrUpdateGrid.Enabled := True;

    //lbeSearchRectLeftOffset.Text := IntToStr(GetSearchAreaLeftOffsetFromBmpDbgImg);
    //lbeSearchRectTopOffset.Text := IntToStr(GetSearchAreaTopOffsetFromBmpDbgImg);
    Offsets.Left := IntToStr(GetSearchAreaLeftOffsetFromBmpDbgImg);
    Offsets.Top := IntToStr(GetSearchAreaTopOffsetFromBmpDbgImg);
    DoOnUpdateSearchAreaLimitsInOIFromDraggingLines([llLeft, llTop], Offsets);

    if Assigned(FSearchAreaSearchedTextDbgImg) and
      (FSearchAreaSearchedTextDbgImg.Width > 0) and
      (FSearchAreaSearchedTextDbgImg.Picture.Bitmap.Width > 0) then
    begin
      FSearchAreaRightLimitLabel.Left := FSearchAreaSearchedTextDbgImg.Left + FSearchAreaSearchedTextDbgImg.Width;
      FSearchAreaBottomLimitLabel.Top := FSearchAreaSearchedTextDbgImg.Top + FSearchAreaSearchedTextDbgImg.Height;
    end
    else
    begin
      FSearchAreaRightLimitLabel.Left := FSearchAreaSearchedBmpDbgImg.Left + FSearchAreaSearchedBmpDbgImg.Width;
      FSearchAreaBottomLimitLabel.Top := FSearchAreaSearchedBmpDbgImg.Top + FSearchAreaSearchedBmpDbgImg.Height;
    end;

    //lbeSearchRectRightOffset.Text := IntToStr(GetSearchAreaRightOffsetFromBmpDbgImg - GetControlWidthFromReplacement);
    //lbeSearchRectBottomOffset.Text := IntToStr(GetSearchAreaBottomOffsetFromBmpDbgImg - GetControlHeightFromReplacement);
    Offsets.Right := IntToStr(GetSearchAreaRightOffsetFromBmpDbgImg - GetControlWidthFromReplacement);
    Offsets.Bottom := IntToStr(GetSearchAreaBottomOffsetFromBmpDbgImg - GetControlHeightFromReplacement);
    DoOnUpdateSearchAreaLimitsInOIFromDraggingLines([llRight, llBottom], Offsets);

    UpdateTransparent_SearchAreaLimitsFromSearchAreaLimits;
    UpdateSearchAreaLabelColorsFromTheirPosition;
  except
    //this will throw AV on uninitialized components or out of bounds values
  end;
end;


procedure TfrClickerFindControl.MenuItemCalculateMinimumErrorLevelToMatchText(Sender: TObject);
var
  MinInterval, MaxInterval: Integer;
  Found: Boolean;
  FindControlOptions: PClkFindControlOptions;
  TestedErrorLevel, LastValidErrorLevel: Integer;
  AllowedColErr, FastSearchAllowedColErr: Integer;
  ExpectedArea: TRect; //Expected area where the text should be found. If found somewhere else, then it's a false positive.
  FoundArea: TRect;
  FoundOutsideOfExpectedArea: Boolean;
begin
  (Sender as TMenuItem).Enabled := False;
  try
    if MessageBox(Handle, 'The algorithm will use the current FindSubControl action settings, to get the minimum color error level from the expected area (defined by holding the Alt key while selecting). It can be stopped with Ctrl-Shift-F2. Continue?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = IDNO then
      Exit;

    FindControlOptions := DoOnGetFindControlOptions;

    MinInterval := 0;
    MaxInterval := 100; //a color error this high would make no sense anyway
    LastValidErrorLevel := -1;

    AllowedColErr := StrToIntDef(EvaluateReplacements(FindControlOptions.AllowedColorErrorCount), 10);
    FastSearchAllowedColErr := StrToIntDef(EvaluateReplacements(FindControlOptions.FastSearchAllowedColorErrorCount), 10);

    DoOnAddToLog('');
    DoOnAddToLog('Searching for minimum color error level...');

    if (FExpectedErrLevel_TopLeft.X < 0) or (FExpectedErrLevel_TopLeft.Y < 0) then
    begin
      FExpectedErrLevel_TopLeft.X := 0;
      FExpectedErrLevel_TopLeft.Y := 0;
      FExpectedErrLevel_BotRight.X := FSearchAreaControlDbgImg.Width;
      FExpectedErrLevel_BotRight.Y := FSearchAreaControlDbgImg.Height;
    end;

    Found := False;
    FManuallyStopCalcErrLevel := False;
    FoundOutsideOfExpectedArea := False;

    tmrBlinkCalcErrLevel.Enabled := True;
    try
      while MaxInterval - MinInterval > 1 do
      begin
        TestedErrorLevel := (MinInterval + MaxInterval) shr 1;

        DoOnAddToLog('TestedErrorLevel: ' + IntToStr(TestedErrorLevel) + '    Interval: [' + IntToStr(MinInterval) + '..' + IntToStr(MaxInterval) + ']');

        Found := DoOnExecuteFindSubControlAction(TestedErrorLevel, AllowedColErr, FastSearchAllowedColErr, FoundArea);

        if Found then
        begin
          ExpectedArea.Left := FExpectedErrLevel_TopLeft.X;
          ExpectedArea.Top := FExpectedErrLevel_TopLeft.Y;
          ExpectedArea.Right := FExpectedErrLevel_BotRight.X;
          ExpectedArea.Bottom := FExpectedErrLevel_BotRight.Y;

          if (FoundArea.Left < ExpectedArea.Left) or
             (FoundArea.Top < ExpectedArea.Top) or
             (FoundArea.Right > ExpectedArea.Right) or
             (FoundArea.Bottom > ExpectedArea.Bottom) then
          begin
            Found := False;
            FoundOutsideOfExpectedArea := True;
          end
          else
            LastValidErrorLevel := TestedErrorLevel;
        end;

        if not Found then
          MinInterval := TestedErrorLevel
        else
          MaxInterval := TestedErrorLevel;

        if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) and(GetAsyncKeyState(VK_F2) < 0) then
          Break;

        Application.ProcessMessages;
        Sleep(10);

        if FManuallyStopCalcErrLevel then
        begin
          DoOnAddToLog('Manually stopping searching for color error level.');
          Break;
        end;

        if not Found and (LastValidErrorLevel <> -1) then //Found at least once.  This may prevent further searching, if the minimum value is somewhere in between TestedErrorLevel and LastValidErrorLevel.
        begin
          Found := True; //accept the current found value
          Break;
        end;
      end; //while
    finally
      tmrBlinkCalcErrLevel.Enabled := False;
      imgCalcMinErrLevel.Visible := False;
    end;

    FManuallyStopCalcErrLevel := False;

    if not Found then
    begin
      MessageBox(Handle, 'No proper color error level found.', PChar(Application.Title), MB_ICONINFORMATION);
      DoOnAddToLog('No proper color error level found.');

      if FoundOutsideOfExpectedArea then
        DoOnAddToLog('The searched text can be found outside of expected area.');
    end
    else
    begin
      if LastValidErrorLevel = -1 then
        DoOnAddToLog('No valid value found.')
      else
      begin
        if TestedErrorLevel <> LastValidErrorLevel then
          TestedErrorLevel := LastValidErrorLevel; //update to the latest valid value

        DoOnAddToLog('Found an error level for color matching: ' + IntToStr(TestedErrorLevel));
        if MessageBox(Handle, 'A color error level found. Do you want to update the action properties?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = IDYES then
        begin
          FindControlOptions^.ColorError := IntToStr(TestedErrorLevel);
          tmrUpdateSearchAreaOffsetEditBoxes.Enabled := True; //an ugly way to trigger the modified flag in parent frames, up to OI
          DoOnTriggerOnControlsModified;
          //update other properties if the algorithm is advanced enough to calculate more than ColorError
        end;
      end;
    end;
  finally
    (Sender as TMenuItem).Enabled := True;
  end;
end;


procedure TfrClickerFindControl.MenuItemCalculateMinimumColorErrorCountToMatchText(Sender: TObject); //These two functions should be refactored, because they have a similar structure.
var
  MinInterval, MaxInterval: Integer;
  Found: Boolean;
  FindControlOptions: PClkFindControlOptions;
  ErrorLevel, TestedAllowedColErr, LastValidAllowedColErr, FastSearchAllowedColErr: Integer;
  ExpectedArea: TRect; //Expected area where the text should be found. If found somewhere else, then it's a false positive.
  FoundArea: TRect;
  FoundOutsideOfExpectedArea: Boolean;
begin
  (Sender as TMenuItem).Enabled := False;
  try
    if MessageBox(Handle, 'The algorithm will use the current FindSubControl action settings, to get the minimum color error count from the expected area (defined by holding the Alt key while selecting). It can be stopped with Ctrl-Shift-F2. Continue?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = IDNO then
      Exit;

    FindControlOptions := DoOnGetFindControlOptions;

    MinInterval := 0;
    MaxInterval := 1000; //depends on text size and length (afterall, the bmp area)
    LastValidAllowedColErr := -1;

    ErrorLevel := StrToIntDef(EvaluateReplacements(FindControlOptions.ColorError), 10);
    FastSearchAllowedColErr := StrToIntDef(EvaluateReplacements(FindControlOptions.FastSearchAllowedColorErrorCount), 10);

    DoOnAddToLog('');
    DoOnAddToLog('Searching for minimum color error count...');

    if (FExpectedErrLevel_TopLeft.X < 0) or (FExpectedErrLevel_TopLeft.Y < 0) then
    begin
      FExpectedErrLevel_TopLeft.X := 0;
      FExpectedErrLevel_TopLeft.Y := 0;
      FExpectedErrLevel_BotRight.X := FSearchAreaControlDbgImg.Width;
      FExpectedErrLevel_BotRight.Y := FSearchAreaControlDbgImg.Height;
    end;

    Found := False;
    FManuallyStopCalcErrLevel := False;
    FoundOutsideOfExpectedArea := False;

    tmrBlinkCalcErrLevel.Enabled := True;
    try
      while MaxInterval - MinInterval > 1 do
      begin
        TestedAllowedColErr := (MinInterval + MaxInterval) shr 1;

        DoOnAddToLog('TestedAllowedColErr: ' + IntToStr(TestedAllowedColErr) + '    Interval: [' + IntToStr(MinInterval) + '..' + IntToStr(MaxInterval) + ']');

        Found := DoOnExecuteFindSubControlAction(ErrorLevel, TestedAllowedColErr, FastSearchAllowedColErr, FoundArea);

        if Found then
        begin
          ExpectedArea.Left := FExpectedErrLevel_TopLeft.X;
          ExpectedArea.Top := FExpectedErrLevel_TopLeft.Y;
          ExpectedArea.Right := FExpectedErrLevel_BotRight.X;
          ExpectedArea.Bottom := FExpectedErrLevel_BotRight.Y;

          if (FoundArea.Left < ExpectedArea.Left) or
             (FoundArea.Top < ExpectedArea.Top) or
             (FoundArea.Right > ExpectedArea.Right) or
             (FoundArea.Bottom > ExpectedArea.Bottom) then
          begin
            Found := False;
            FoundOutsideOfExpectedArea := True;
          end
          else
            LastValidAllowedColErr := TestedAllowedColErr;
        end;

        if not Found then
          MinInterval := TestedAllowedColErr
        else
          MaxInterval := TestedAllowedColErr;

        if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) and(GetAsyncKeyState(VK_F2) < 0) then
          Break;

        Application.ProcessMessages;
        Sleep(10);

        if FManuallyStopCalcErrLevel then
        begin
          DoOnAddToLog('Manually stopping searching for color error count.');
          Break;
        end;

        if not Found and (LastValidAllowedColErr <> -1) then //Found at least once.  This may prevent further searching, if the minimum value is somewhere in between TestedErrorLevel and LastValidErrorLevel.
        begin
          Found := True; //accept the current found value
          Break;
        end;
      end; //while
    finally
      tmrBlinkCalcErrLevel.Enabled := False;
      imgCalcMinErrLevel.Visible := False;
    end;

    FManuallyStopCalcErrLevel := False;

    if not Found then
    begin
      MessageBox(Handle, 'No proper color error count found.', PChar(Application.Title), MB_ICONINFORMATION);
      DoOnAddToLog('No proper color error count found.');

      if FoundOutsideOfExpectedArea then
        DoOnAddToLog('The searched text can be found outside of expected area.');
    end
    else
    begin
      if LastValidAllowedColErr = -1 then
        DoOnAddToLog('No valid value found.')
      else
      begin
        if TestedAllowedColErr <> LastValidAllowedColErr then
          TestedAllowedColErr := LastValidAllowedColErr; //update to the latest valid value

        DoOnAddToLog('Found an error count for color matching: ' + IntToStr(TestedAllowedColErr));
        if MessageBox(Handle, 'A color error count found. Do you want to update the action properties?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = IDYES then
        begin
          FindControlOptions^.AllowedColorErrorCount := IntToStr(TestedAllowedColErr);
          tmrUpdateSearchAreaOffsetEditBoxes.Enabled := True; //an ugly way to trigger the modified flag in parent frames, up to OI
          DoOnTriggerOnControlsModified;
          //update other properties if the algorithm is advanced enough to calculate more than ColorError
        end;
      end;
    end;
  finally
    (Sender as TMenuItem).Enabled := True;
  end;
end;


procedure TfrClickerFindControl.MenuItemStopCalculatingMinimumErrorLevelToMatchText(Sender: TObject);
begin
  FManuallyStopCalcErrLevel := True;
end;


procedure TfrClickerFindControl.DisplaySelectionLinesForExpectedFindLocation;
begin
  FSearchAreaLeftLimitLabel_ForMinErr.Show;
  FSearchAreaTopLimitLabel_ForMinErr.Show;
  FSearchAreaRightLimitLabel_ForMinErr.Show;
  FSearchAreaBottomLimitLabel_ForMinErr.Show;
end;


procedure TfrClickerFindControl.HideSelectionLinesForExpectedFindLocation;
begin
  FSearchAreaLeftLimitLabel_ForMinErr.Hide;
  FSearchAreaTopLimitLabel_ForMinErr.Hide;
  FSearchAreaRightLimitLabel_ForMinErr.Hide;
  FSearchAreaBottomLimitLabel_ForMinErr.Hide;
end;


procedure TfrClickerFindControl.MenuItemDisplaySelectionLinesForExpectedFindLocation(Sender: TObject);
begin
  DisplaySelectionLinesForExpectedFindLocation;
end;


procedure TfrClickerFindControl.MenuItemHideSelectionLinesForExpectedFindLocation(Sender: TObject);
begin
  HideSelectionLinesForExpectedFindLocation;
end;


procedure TfrClickerFindControl.MenuItemLoadBmpTextToSearchedAreaClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Length(FBMPTextProfiles) - 1 do
    FBMPTextProfiles[i].PreviewTextOnImage(FSearchAreaSearchedTextDbgImg);

  FSearchAreaSearchedTextDbgImg.Show;
end;


procedure TfrClickerFindControl.MenuItemUnloadBmpTextFromSearchedAreaClick(Sender: TObject);
begin
  FSearchAreaSearchedTextDbgImg.Picture.Clear;
  FSearchAreaSearchedTextDbgImg.Hide;
end;


procedure TfrClickerFindControl.MenuItemGenericLoadBmpToSearchedAreaClick(Sender: TObject);
var
  BmpPath: string;
begin
  BmpPath := StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]);
  BmpPath := StringReplace(BmpPath, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);

  if DoOnFileExists(BmpPath) then
  begin
    FSearchAreaSearchedBmpDbgImg.AutoSize := True;  //should be set-reset when implementing zoom
    DoOnLoadBitmap(FSearchAreaSearchedBmpDbgImg.Picture.Bitmap, BmpPath);

    FSearchAreaSearchedBmpDbgImg.Width := FSearchAreaSearchedBmpDbgImg.Picture.Bitmap.Width;
    FSearchAreaSearchedBmpDbgImg.Height := FSearchAreaSearchedBmpDbgImg.Picture.Bitmap.Height;
  end
  else
  begin
    FSearchAreaSearchedBmpDbgImg.Canvas.Pen.Color := clRed;
    FSearchAreaSearchedBmpDbgImg.Canvas.Brush.Color := clWhite;
    FSearchAreaSearchedBmpDbgImg.Width := 40;
    FSearchAreaSearchedBmpDbgImg.Height := 40;
    FSearchAreaSearchedBmpDbgImg.Canvas.Rectangle(0, 0, FSearchAreaSearchedBmpDbgImg.Width - 1, FSearchAreaSearchedBmpDbgImg.Height - 1);

    FSearchAreaControlDbgImg.Canvas.TextOut(1, 1, 'Not');
    FSearchAreaControlDbgImg.Canvas.TextOut(1, 15, 'found.');
  end;
end;


procedure TfrClickerFindControl.UpdateBitmapAlgorithmSettings;
begin
  tmrUpdateGrid.Enabled := True;

  if Assigned(FOnUpdateBitmapAlgorithmSettings) then
    FOnUpdateBitmapAlgorithmSettings()
  else
    raise Exception.Create('OnUpdateBitmapAlgorithmSettings not assigned.');
end;


procedure TfrClickerFindControl.UpdateControlWidthHeightLabels;
begin
  lblPreviewControl_Width.Caption := '$Control_Width$: ' + EvaluateReplacements('$Control_Width$');
  lblPreviewControl_Height.Caption := '$Control_Height$: ' + EvaluateReplacements('$Control_Height$');
end;


procedure TfrClickerFindControl.UpdateUseWholeScreenLabel(AUseWholeScreen: Boolean);
var
  FindControlOptions: PClkFindControlOptions;
begin
  pnlUseWholeScreen.Visible := AUseWholeScreen;
  FindControlOptions := DoOnGetFindControlOptions;

  if AUseWholeScreen and
     (FindControlOptions^.MatchCriteria.WillMatchBitmapText or
     FindControlOptions^.MatchCriteria.WillMatchBitmapFiles or
     FindControlOptions^.MatchCriteria.WillMatchPrimitiveFiles) then
    pnlUseWholeScreen.Color := $00B6B6FF  //some light red
  else
    pnlUseWholeScreen.Color := $0053F783; //some green
end;


procedure TfrClickerFindControl.ClearControls;
var
  FindControlOptions: PClkFindControlOptions;
  Offsets: TSimpleRectString;
begin
  CreateBMPTextFrames(0);
  AddFontProfile(CDefaultFontProfileName);

  lstMatchBitmapFiles.Clear;
  lstMatchPrimitiveFiles.Clear;
  FindControlOptions := DoOnGetFindControlOptions;

  FindControlOptions^.MatchBitmapAlgorithm := mbaBruteForce;
  FindControlOptions^.MatchBitmapAlgorithmSettings.XMultipleOf := 1;
  FindControlOptions^.MatchBitmapAlgorithmSettings.YMultipleOf := 1;
  FindControlOptions^.MatchBitmapAlgorithmSettings.XOffset := 0;
  FindControlOptions^.MatchBitmapAlgorithmSettings.YOffset := 0;
  FindControlOptions^.StartSearchingWithCachedControl := False;

  UpdateBitmapAlgorithmSettings;

  FindControlOptions^.InitialRectangle.Left := '$Control_Left$';
  FindControlOptions^.InitialRectangle.Top := '$Control_Top$';
  FindControlOptions^.InitialRectangle.Right := '$Control_Right$';
  FindControlOptions^.InitialRectangle.Bottom := '$Control_Bottom$';
  FindControlOptions^.InitialRectangle.LeftOffset := '0';
  FindControlOptions^.InitialRectangle.TopOffset := '0';
  FindControlOptions^.InitialRectangle.RightOffset := '0';
  FindControlOptions^.InitialRectangle.BottomOffset := '0';

  Offsets.Left := '0';
  Offsets.Top := '0';
  Offsets.Right := '0';
  Offsets.Bottom := '0';
  DoOnUpdateSearchAreaLimitsInOIFromDraggingLines([llLeft, llTop, llRight, llBottom], Offsets);
end;


procedure TfrClickerFindControl.FTransparent_LeftMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;

  GetCursorPos(FMouseDownGlobalPos);

  if not FSelectionHold then
  begin
    FMouseDownSelPos.X := FTransparent_SearchAreaLeftLimitLabel.Left; //component coordinates on the window
    FSelectionHold := True;
  end;
end;


procedure TfrClickerFindControl.FTransparent_LeftMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewLeft: Integer;
  CurrentLabel: TLabel;
  tp: TPoint;
begin
  if not FSelectionHold then
    Exit;

  GetCursorPos(tp);
  if Sender is TLabel then
  begin
    CurrentLabel := Sender as TLabel;
    NewLeft := FMouseDownSelPos.X + tp.X - FMouseDownGlobalPos.X;
    if NewLeft <> CurrentLabel.Left then
      DoOnTriggerOnControlsModified;

    CurrentLabel.Left := Min(NewLeft, FSearchAreaControlDbgImg.Width);
    FSearchAreaLeftLimitLabel.Left := CurrentLabel.Left;

    tmrUpdateSearchAreaOffsetEditBoxes.Enabled := True;
    UpdateSearchAreaLabelColorsFromTheirPosition;

    //ToDo: refactoring, to avoid calling the imgSearchAreaControlDbgMouseMove handler
    imgSearchAreaControlDbgMouseMove(FSearchAreaControlDbgImg, Shift, FTransparent_SearchAreaLeftLimitLabel.Left + FTransparent_SearchAreaLeftLimitLabel.Width shr 1, Y);  //shr 1, because of label width
  end;
end;


procedure TfrClickerFindControl.FTransparent_LeftMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectionHold := False;
end;


procedure TfrClickerFindControl.FTransparent_RightMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;

  GetCursorPos(FMouseDownGlobalPos);

  if not FSelectionHold then
  begin
    FMouseDownSelPos.X := FTransparent_SearchAreaRightLimitLabel.Left; //component coordinates on the window
    FSelectionHold := True;
  end;
end;


procedure TfrClickerFindControl.FTransparent_RightMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewLeft: Integer;
  CurrentLabel: TLabel;
  tp: TPoint;
begin
  if not FSelectionHold then
    Exit;

  GetCursorPos(tp);
  if Sender is TLabel then
  begin
    CurrentLabel := Sender as TLabel;
    NewLeft := FMouseDownSelPos.X + tp.X - FMouseDownGlobalPos.X;
    if NewLeft <> CurrentLabel.Left then
      DoOnTriggerOnControlsModified;

    CurrentLabel.Left := Min(NewLeft, FSearchAreaControlDbgImg.Width);
    FSearchAreaRightLimitLabel.Left := CurrentLabel.Left;

    tmrUpdateSearchAreaOffsetEditBoxes.Enabled := True;
    UpdateSearchAreaLabelColorsFromTheirPosition;

    //ToDo: refactoring, to avoid calling the imgSearchAreaControlDbgMouseMove handler
    imgSearchAreaControlDbgMouseMove(FSearchAreaControlDbgImg, Shift, FTransparent_SearchAreaRightLimitLabel.Left + FTransparent_SearchAreaRightLimitLabel.Width shr 1, Y);  //shr 1, because of label width
  end;
end;


procedure TfrClickerFindControl.FTransparent_RightMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectionHold := False;
end;


procedure TfrClickerFindControl.FTransparent_TopMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;

  GetCursorPos(FMouseDownGlobalPos);

  if not FSelectionHold then
  begin
    FMouseDownSelPos.Y := FTransparent_SearchAreaTopLimitLabel.Top; //component coordinates on the window
    FSelectionHold := True;
  end;
end;


procedure TfrClickerFindControl.FTransparent_TopMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewTop: Integer;
  CurrentLabel: TLabel;
  tp: TPoint;
begin
  if not FSelectionHold then
    Exit;

  GetCursorPos(tp);
  if Sender is TLabel then
  begin
    CurrentLabel := Sender as TLabel;
    NewTop := FMouseDownSelPos.Y + tp.Y - FMouseDownGlobalPos.Y;
    if NewTop <> CurrentLabel.Top then
      DoOnTriggerOnControlsModified;

    CurrentLabel.Top := Min(NewTop, FSearchAreaControlDbgImg.Height);
    FSearchAreaTopLimitLabel.Top := CurrentLabel.Top;

    tmrUpdateSearchAreaOffsetEditBoxes.Enabled := True;
    UpdateSearchAreaLabelColorsFromTheirPosition;

    //ToDo: refactoring, to avoid calling the imgSearchAreaControlDbgMouseMove handler
    imgSearchAreaControlDbgMouseMove(FSearchAreaControlDbgImg, Shift, X, FTransparent_SearchAreaTopLimitLabel.Top + FTransparent_SearchAreaTopLimitLabel.Height shr 1);  //shr 1, because of label height
  end;
end;


procedure TfrClickerFindControl.FTransparent_TopMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectionHold := False;
end;


procedure TfrClickerFindControl.FTransparent_BottomMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;

  GetCursorPos(FMouseDownGlobalPos);

  if not FSelectionHold then
  begin
    FMouseDownSelPos.Y := FTransparent_SearchAreaBottomLimitLabel.Top; //component coordinates on the window
    FSelectionHold := True;
  end;
end;


procedure TfrClickerFindControl.FTransparent_BottomMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewTop: Integer;
  CurrentLabel: TLabel;
  tp: TPoint;
begin
  if not FSelectionHold then
    Exit;

  GetCursorPos(tp);
  if Sender is TLabel then
  begin
    CurrentLabel := Sender as TLabel;
    NewTop := FMouseDownSelPos.Y + tp.Y - FMouseDownGlobalPos.Y;
    if NewTop <> CurrentLabel.Top then
      DoOnTriggerOnControlsModified;

    CurrentLabel.Top := Min(NewTop, FSearchAreaControlDbgImg.Height);
    FSearchAreaBottomLimitLabel.Top := CurrentLabel.Top;

    tmrUpdateSearchAreaOffsetEditBoxes.Enabled := True;
    UpdateSearchAreaLabelColorsFromTheirPosition;

    //ToDo: refactoring, to avoid calling the imgSearchAreaControlDbgMouseMove handler
    imgSearchAreaControlDbgMouseMove(FSearchAreaControlDbgImg, Shift, X, FTransparent_SearchAreaBottomLimitLabel.Top + FTransparent_SearchAreaBottomLimitLabel.Height shr 1);  //shr 1, because of label height
  end;
end;


procedure TfrClickerFindControl.FTransparent_BottomMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectionHold := False;
end;


procedure TfrClickerFindControl.FSearchAreaControlDbgImgResize(Sender: TObject);
begin
  FTransparent_SearchAreaLeftLimitLabel.Height := FSearchAreaLeftLimitLabel.Height;
  FTransparent_SearchAreaTopLimitLabel.Width := FSearchAreaTopLimitLabel.Width;
  FTransparent_SearchAreaRightLimitLabel.Height := FSearchAreaRightLimitLabel.Height;
  FTransparent_SearchAreaBottomLimitLabel.Width := FSearchAreaBottomLimitLabel.Width;
end;


procedure TfrClickerFindControl.UpdateTransparent_SearchAreaLimitsFromSearchAreaLimits;
begin
  FTransparent_SearchAreaLeftLimitLabel.Left := FSearchAreaLeftLimitLabel.Left;
  FTransparent_SearchAreaTopLimitLabel.Top := FSearchAreaTopLimitLabel.Top;
  FTransparent_SearchAreaRightLimitLabel.Left := FSearchAreaRightLimitLabel.Left;
  FTransparent_SearchAreaBottomLimitLabel.Top := FSearchAreaBottomLimitLabel.Top;
end;


procedure TfrClickerFindControl.UpdateAllSelectionLabelsFromCropEditBoxes;
var
  i: Integer;
begin
  for i := 0 to Length(FBMPTextProfiles) - 1 do
    FBMPTextProfiles[i].UpdateSelectionLabelsFromCropEditBoxes;
end;



procedure TfrClickerFindControl.UpdateSearchAreaSearchedTextAndLabels;
begin
  if Assigned(FSearchAreaSearchedTextDbgImg) and
     Assigned(FSearchAreaSearchedBmpDbgImg) then
  begin
    FSearchAreaSearchedTextDbgImg.Left := FSearchAreaSearchedBmpDbgImg.Left;
    FSearchAreaSearchedTextDbgImg.Top := FSearchAreaSearchedBmpDbgImg.Top;
    UpdateSearchAreaLabelColorsFromTheirPosition;
  end;
end;


//////////////////////////////// OI stuff
procedure TfrClickerFindControl.UpdateOnSearchRectLeftOffsetMouseDown(var InitialRectange: TRectString;
  AEditBox: TVTEdit; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Ref: Integer;
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
  begin
    Ref := GetSearch_LeftLeft_Ref_FromInitRect(InitialRectange);

    FlbeSearchRectOffsetMDownInit := Y;  //used to compute offset
    FlbeSearchRectOffsetMDownValueInit := StrToIntDef(EvaluateReplacements(AEditBox.Text), MaxInt) - Ref;  //editbox value on mouse down

    Ref := GetSearch_RightLeft_Ref_FromInitRect(InitialRectange);
    FlbeSearchRectOffsetMDownSecondValueInit := StrToIntDef(EvaluateReplacements(InitialRectange.RightOffset), MaxInt) - Ref;  //editbox value on mouse down
  end;
end;


procedure TfrClickerFindControl.UpdateOnSearchRectLeftOffsetMouseMove(var InitialRectange: TRectString;
  AEditBox: TVTEdit; Shift: TShiftState; X, Y: Integer);
var
  Ref: Integer;
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
    if FlbeSearchRectOffsetMDownValueInit <> MaxInt then  //MaxInt is used as an indicator that a replacement is used, not a numeric value
    begin
      Ref := GetSearch_LeftLeft_Ref_FromInitRect(InitialRectange);
      AEditBox.Text := IntToStr(FlbeSearchRectOffsetMDownValueInit - (Y - FlbeSearchRectOffsetMDownInit) + Ref);
      InitialRectange.LeftOffset := AEditBox.Text;

      if FSearchAreaSearchedBmpDbgImg <> nil then
      begin
        FSearchAreaSearchedBmpDbgImg.Left := StrToIntDef(EvaluateReplacements(AEditBox.Text), 20) - Ref;
        FSearchAreaLeftLimitLabel.Left := FSearchAreaSearchedBmpDbgImg.Left;
        FTransparent_SearchAreaLeftLimitLabel.Left := FSearchAreaLeftLimitLabel.Left;
        tmrUpdateGrid.Enabled := True;
      end;

      if ssShift in Shift then
      begin
        Ref := GetSearch_RightLeft_Ref_FromInitRect(InitialRectange);
        InitialRectange.RightOffset := IntToStr(FlbeSearchRectOffsetMDownSecondValueInit - (Y - FlbeSearchRectOffsetMDownInit) + Ref);

        if FSearchAreaSearchedBmpDbgImg <> nil then
        begin
          //if chkUseWholeScreenAsSearchArea.Checked then
          //  ControlRight := Screen.Width
          //else
          //  ControlRight := StrToIntDef(EvaluateReplacements(InitialRectange.Right), 20) - StrToIntDef(EvaluateReplacements(InitialRectange.Left), 20); //Right edge of the search area.

          FSearchAreaRightLimitLabel.Left := StrToIntDef(InitialRectange.RightOffset, 20) - Ref;   ////////// should be control right + offset (offset can be negative)
          FTransparent_SearchAreaRightLimitLabel.Left := FSearchAreaRightLimitLabel.Left;
        end;
      end;

      UpdateSearchAreaSearchedTextAndLabels;
    end;
end;


procedure TfrClickerFindControl.UpdateOnSearchRectTopOffsetMouseDown(var InitialRectange: TRectString;
  AEditBox: TVTEdit; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Ref: Integer;
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
  begin
    Ref := GetSearch_TopTop_Ref_FromInitRect(InitialRectange);
    FlbeSearchRectOffsetMDownInit := Y;  //used to compute offset
    FlbeSearchRectOffsetMDownValueInit := StrToIntDef(EvaluateReplacements(AEditBox.Text), MaxInt) - Ref;  //editbox value on mouse down

    Ref := GetSearch_BottomTop_Ref_FromInitRect(InitialRectange);
    FlbeSearchRectOffsetMDownSecondValueInit := StrToIntDef(EvaluateReplacements(InitialRectange.BottomOffset), MaxInt) - Ref;  //editbox value on mouse down
  end;
end;


procedure TfrClickerFindControl.UpdateOnSearchRectTopOffsetMouseMove(var InitialRectange: TRectString;
  AEditBox: TVTEdit; Shift: TShiftState; X, Y: Integer);
var
  Ref: Integer;
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
    if FlbeSearchRectOffsetMDownValueInit <> MaxInt then  //MaxInt is used as an indicator that a replacement is used, not a numeric value
    begin
      Ref := GetSearch_TopTop_Ref_FromInitRect(InitialRectange);
      AEditBox.Text := IntToStr(FlbeSearchRectOffsetMDownValueInit - (Y - FlbeSearchRectOffsetMDownInit) + Ref);
      InitialRectange.TopOffset := AEditBox.Text;

      if FSearchAreaSearchedBmpDbgImg <> nil then
      begin
        FSearchAreaSearchedBmpDbgImg.Top := StrToIntDef(EvaluateReplacements(AEditBox.Text), 20) - Ref;
        FTransparent_SearchAreaTopLimitLabel.Top := FSearchAreaTopLimitLabel.Top;
        FSearchAreaTopLimitLabel.Top := FSearchAreaSearchedBmpDbgImg.Top;
        tmrUpdateGrid.Enabled := True;
      end;

      if ssShift in Shift then
      begin
        Ref := GetSearch_BottomTop_Ref_FromInitRect(InitialRectange);
        InitialRectange.BottomOffset := IntToStr(FlbeSearchRectOffsetMDownSecondValueInit - (Y - FlbeSearchRectOffsetMDownInit) + Ref);

        if FSearchAreaSearchedBmpDbgImg <> nil then
        begin
          //if chkUseWholeScreenAsSearchArea.Checked then
          //  ControlBottom := Screen.Height
          //else
          //  ControlBottom := StrToIntDef(EvaluateReplacements(InitialRectange.Bottom), 20) - StrToIntDef(EvaluateReplacements(lbeSearchRectTop.Text), 20); //Bottom edge of the search area.

          FSearchAreaBottomLimitLabel.Top := StrToIntDef(InitialRectange.BottomOffset, 20) - Ref;    ////////// should be control bottom + offset (offset can be negative)
          FTransparent_SearchAreaBottomLimitLabel.Top := FSearchAreaBottomLimitLabel.Top;
        end;
      end;

      UpdateSearchAreaSearchedTextAndLabels;
    end;
end;


procedure TfrClickerFindControl.UpdateOnSearchRectRightOffsetMouseDown(var InitialRectange: TRectString;
  AEditBox: TVTEdit; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Ref: Integer;
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
  begin
    Ref := GetSearch_RightRight_Ref_FromInitRect(InitialRectange);

    FlbeSearchRectOffsetMDownInit := Y;  //used to compute offset
    FlbeSearchRectOffsetMDownValueInit := StrToIntDef(EvaluateReplacements(AEditBox.Text), MaxInt) - Ref;  //editbox value on mouse down

    Ref := GetSearch_LeftRight_Ref_FromInitRect(InitialRectange);
    FlbeSearchRectOffsetMDownSecondValueInit := StrToIntDef(EvaluateReplacements(InitialRectange.LeftOffset), MaxInt) - Ref;  //editbox value on mouse down
  end;
end;


procedure TfrClickerFindControl.UpdateOnSearchRectRightOffsetMouseMove(var InitialRectange: TRectString;
  AEditBox: TVTEdit; Shift: TShiftState; X, Y: Integer);
var
  ControlWidth: Integer;
  Ref: Integer;
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
    if FlbeSearchRectOffsetMDownValueInit <> MaxInt then  //MaxInt is used as an indicator that a replacement is used, not a numeric value
    begin
      Ref := GetSearch_RightRight_Ref_FromInitRect(InitialRectange);
      AEditBox.Text := IntToStr(FlbeSearchRectOffsetMDownValueInit - (Y - FlbeSearchRectOffsetMDownInit) + Ref);
      InitialRectange.RightOffset := AEditBox.Text;

      if FSearchAreaSearchedBmpDbgImg <> nil then
      begin
        ControlWidth := GetControlWidthFromReplacement;
        FSearchAreaRightLimitLabel.Left := StrToIntDef(EvaluateReplacements(AEditBox.Text), 20) - Ref + ControlWidth;
        FTransparent_SearchAreaRightLimitLabel.Left := FSearchAreaRightLimitLabel.Left;
      end;

      if ssShift in Shift then
      begin
        Ref := GetSearch_LeftRight_Ref_FromInitRect(InitialRectange);
        InitialRectange.LeftOffset := IntToStr(FlbeSearchRectOffsetMDownSecondValueInit - (Y - FlbeSearchRectOffsetMDownInit) + Ref);

        if FSearchAreaSearchedBmpDbgImg <> nil then
        begin
          //ControlWidth is initialized above
          FSearchAreaLeftLimitLabel.Left := StrToIntDef(EvaluateReplacements(InitialRectange.LeftOffset), 20) - Ref + ControlWidth;
          FTransparent_SearchAreaLeftLimitLabel.Left := FSearchAreaLeftLimitLabel.Left;
          FSearchAreaSearchedBmpDbgImg.Left := FSearchAreaLeftLimitLabel.Left;
          tmrUpdateGrid.Enabled := True;
        end;
      end;

      UpdateSearchAreaSearchedTextAndLabels;
    end;
end;


procedure TfrClickerFindControl.UpdateOnSearchRectBottomOffsetMouseDown(var InitialRectange: TRectString;
  AEditBox: TVTEdit; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Ref: Integer;
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
  begin
    Ref := GetSearch_BottomBottom_Ref_FromInitRect(InitialRectange);
    FlbeSearchRectOffsetMDownInit := Y;  //used to compute offset
    FlbeSearchRectOffsetMDownValueInit := StrToIntDef(EvaluateReplacements(AEditBox.Text), MaxInt) - Ref;  //editbox value on mouse down

    Ref := GetSearch_TopBottom_Ref_FromInitRect(InitialRectange);
    FlbeSearchRectOffsetMDownSecondValueInit := StrToIntDef(EvaluateReplacements(InitialRectange.TopOffset), MaxInt) - Ref;  //editbox value on mouse down
  end;
end;


procedure TfrClickerFindControl.UpdateOnSearchRectBottomOffsetMouseMove(var InitialRectange: TRectString;
  AEditBox: TVTEdit; Shift: TShiftState; X, Y: Integer);
var
  ControlHeight: Integer;
  Ref: Integer;
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
    if FlbeSearchRectOffsetMDownValueInit <> MaxInt then  //MaxInt is used as an indicator that a replacement is used, not a numeric value
    begin
      Ref := GetSearch_BottomBottom_Ref_FromInitRect(InitialRectange);
      AEditBox.Text := IntToStr(FlbeSearchRectOffsetMDownValueInit - (Y - FlbeSearchRectOffsetMDownInit) + Ref);
      InitialRectange.BottomOffset := AEditBox.Text;

      if FSearchAreaSearchedBmpDbgImg <> nil then
      begin
        ControlHeight := GetControlHeightFromReplacement;
        FSearchAreaBottomLimitLabel.Top := StrToIntDef(EvaluateReplacements(AEditBox.Text), 20) - Ref + ControlHeight;
        FTransparent_SearchAreaBottomLimitLabel.Top := FSearchAreaBottomLimitLabel.Top;
      end;

      if ssShift in Shift then
      begin
        Ref := GetSearch_TopBottom_Ref_FromInitRect(InitialRectange);
        InitialRectange.TopOffset := IntToStr(FlbeSearchRectOffsetMDownSecondValueInit - (Y - FlbeSearchRectOffsetMDownInit) + Ref);

        if FSearchAreaSearchedBmpDbgImg <> nil then
        begin
          //ControlHeight is initialized above
          FSearchAreaTopLimitLabel.Top := StrToIntDef(EvaluateReplacements(InitialRectange.TopOffset), 20) - Ref + ControlHeight;
          FTransparent_SearchAreaTopLimitLabel.Top := FSearchAreaTopLimitLabel.Top;
          FSearchAreaSearchedBmpDbgImg.Top := FSearchAreaTopLimitLabel.Top;
          tmrUpdateGrid.Enabled := True;
        end;
      end;

      UpdateSearchAreaSearchedTextAndLabels;
    end;
end;


/////////////// similar to SearchArea, but these are for text cropping
procedure TfrClickerFindControl.UpdateOnTextCroppingLeftMouseDown(var AMatchBMP: TClkFindControlMatchBitmapText;
  AEditBox: TVTEdit; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
  begin
    FlbeSearchRectOffsetMDownInit := Y;  //used to compute offset
    FlbeSearchRectOffsetMDownValueInit := StrToIntDef(EvaluateReplacements(AEditBox.Text), MaxInt);  //editbox value on mouse down
    FlbeSearchRectOffsetMDownSecondValueInit := StrToIntDef(EvaluateReplacements(AMatchBMP.CropRight), MaxInt);  //editbox value on mouse down
  end;
end;


procedure TfrClickerFindControl.UpdateOnTextCroppingLeftMouseMove(var AMatchBMP: TClkFindControlMatchBitmapText;
  AEditBox: TVTEdit; Shift: TShiftState; X, Y, AProfileIndex: Integer);
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
    if FlbeSearchRectOffsetMDownValueInit <> MaxInt then  //MaxInt is used as an indicator that a replacement is used, not a numeric value
    begin
      AEditBox.Text := IntToStr(Max(0, FlbeSearchRectOffsetMDownValueInit - (Y - FlbeSearchRectOffsetMDownInit)));
      AMatchBMP.CropLeft := AEditBox.Text;

      if ssShift in Shift then
        AMatchBMP.CropRight := IntToStr(FlbeSearchRectOffsetMDownSecondValueInit - (Y - FlbeSearchRectOffsetMDownInit));

      FBMPTextProfiles[AProfileIndex].UpdateSelectionLabelsFromCropInfo(AMatchBMP);
    end;
end;


procedure TfrClickerFindControl.UpdateOnTextCroppingTopMouseDown(var AMatchBMP: TClkFindControlMatchBitmapText;
  AEditBox: TVTEdit; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
  begin
    FlbeSearchRectOffsetMDownInit := Y;  //used to compute offset
    FlbeSearchRectOffsetMDownValueInit := StrToIntDef(EvaluateReplacements(AEditBox.Text), MaxInt);  //editbox value on mouse down
    FlbeSearchRectOffsetMDownSecondValueInit := StrToIntDef(EvaluateReplacements(AMatchBMP.CropBottom), MaxInt);  //editbox value on mouse down
  end;
end;


procedure TfrClickerFindControl.UpdateOnTextCroppingTopMouseMove(var AMatchBMP: TClkFindControlMatchBitmapText;
  AEditBox: TVTEdit; Shift: TShiftState; X, Y, AProfileIndex: Integer);
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
    if FlbeSearchRectOffsetMDownValueInit <> MaxInt then  //MaxInt is used as an indicator that a replacement is used, not a numeric value
    begin
      AEditBox.Text := IntToStr(Max(0, FlbeSearchRectOffsetMDownValueInit - (Y - FlbeSearchRectOffsetMDownInit)));
      AMatchBMP.CropTop := AEditBox.Text;

      if ssShift in Shift then
        AMatchBMP.CropBottom := IntToStr(FlbeSearchRectOffsetMDownSecondValueInit - (Y - FlbeSearchRectOffsetMDownInit));

      FBMPTextProfiles[AProfileIndex].UpdateSelectionLabelsFromCropInfo(AMatchBMP);
    end;
end;


procedure TfrClickerFindControl.UpdateOnTextCroppingRightMouseDown(var AMatchBMP: TClkFindControlMatchBitmapText;
  AEditBox: TVTEdit; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
  begin
    FlbeSearchRectOffsetMDownInit := Y;  //used to compute offset
    FlbeSearchRectOffsetMDownValueInit := StrToIntDef(EvaluateReplacements(AEditBox.Text), MaxInt);  //editbox value on mouse down
    FlbeSearchRectOffsetMDownSecondValueInit := StrToIntDef(EvaluateReplacements(AMatchBMP.CropLeft), MaxInt);  //editbox value on mouse down
  end;
end;


procedure TfrClickerFindControl.UpdateOnTextCroppingRightMouseMove(var AMatchBMP: TClkFindControlMatchBitmapText;
  AEditBox: TVTEdit; Shift: TShiftState; X, Y, AProfileIndex: Integer);
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
    if FlbeSearchRectOffsetMDownValueInit <> MaxInt then  //MaxInt is used as an indicator that a replacement is used, not a numeric value
    begin
      AEditBox.Text := IntToStr(Max(0, FlbeSearchRectOffsetMDownValueInit - (Y - FlbeSearchRectOffsetMDownInit)));
      AMatchBMP.CropRight := AEditBox.Text;

      if ssShift in Shift then
        AMatchBMP.CropLeft := IntToStr(FlbeSearchRectOffsetMDownSecondValueInit - (Y - FlbeSearchRectOffsetMDownInit));

      FBMPTextProfiles[AProfileIndex].UpdateSelectionLabelsFromCropInfo(AMatchBMP);
    end;
end;


procedure TfrClickerFindControl.UpdateOnTextCroppingBottomMouseDown(var AMatchBMP: TClkFindControlMatchBitmapText;
  AEditBox: TVTEdit; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
  begin
    FlbeSearchRectOffsetMDownInit := Y;  //used to compute offset
    FlbeSearchRectOffsetMDownValueInit := StrToIntDef(EvaluateReplacements(AEditBox.Text), MaxInt);  //editbox value on mouse down
    FlbeSearchRectOffsetMDownSecondValueInit := StrToIntDef(EvaluateReplacements(AMatchBMP.CropTop), MaxInt);  //editbox value on mouse down
  end;
end;


procedure TfrClickerFindControl.UpdateOnTextCroppingBottomMouseMove(var AMatchBMP: TClkFindControlMatchBitmapText;
  AEditBox: TVTEdit; Shift: TShiftState; X, Y, AProfileIndex: Integer);
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
    if FlbeSearchRectOffsetMDownValueInit <> MaxInt then  //MaxInt is used as an indicator that a replacement is used, not a numeric value
    begin
      AEditBox.Text := IntToStr(Max(0, FlbeSearchRectOffsetMDownValueInit - (Y - FlbeSearchRectOffsetMDownInit)));
      AMatchBMP.CropBottom := AEditBox.Text;

      if ssShift in Shift then
        AMatchBMP.CropTop := IntToStr(FlbeSearchRectOffsetMDownSecondValueInit - (Y - FlbeSearchRectOffsetMDownInit));

      FBMPTextProfiles[AProfileIndex].UpdateSelectionLabelsFromCropInfo(AMatchBMP);
    end;
end;

end.

