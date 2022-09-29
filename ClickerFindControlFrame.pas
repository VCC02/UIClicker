{
    Copyright (C) 2022 VCC
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
  ClickerUtils, ClickerBMPTextFrame, InMemFileSystem, VirtualTrees;

type
  TOnUpdateBitmapAlgorithmSettings = procedure of object;

  //TOnSetPictureOpenDialogFileName = procedure(AFileName: string) of object;

  TfrClickerFindControl = class; //forward

  { TFontProfile }     //a wrapper over TfrClickerBMPText, to isolate visual components and expose their values as properties

  TFontProfile = class
  private
    FfrClickerBMPText: TfrClickerBMPText;
    FOwner: TfrClickerFindControl;

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

    function GetFGColor: TColor;
    procedure SetFGColor(Value: TColor);

    function GetBGColor: TColor;
    procedure SetBGColor(Value: TColor);

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

    procedure SetShowCroppingLines(Value: Boolean);

    function CreateBMPTextFrame_NoContent(ANewName: string): TfrClickerBMPText;

  public
    constructor Create(AOwner: TfrClickerFindControl; ANewProfileName: string);
    destructor Destroy; override;

    procedure PreviewText;
    procedure PreviewTextOnImage(AImg: TImage);
    procedure UpdateSelectionLabelsFromCropEditBoxes;

    property ProfileName: string read GetProfileName write SetProfileName;
    property ObjectName: string read GetObjectName write SetObjectName;
    property EditorVisible: Boolean read GetEditorVisible write SetEditorVisible;
    property PreviewImageBitmap: TBitmap read GetPreviewImageBitmap;

    property MatchBitmapTextFGColor: string read GetMatchBitmapTextFGColor write SetMatchBitmapTextFGColor;
    property MatchBitmapTextBGColor: string read GetMatchBitmapTextBGColor write SetMatchBitmapTextBGColor;
    property FGColor: TColor read GetFGColor write SetFGColor;   //The evaluated version of MatchBitmapTextFGColor. Kind of redundant, but it's a bit faster this way.
    property BGColor: TColor read GetBGColor write SetBGColor;   //The evaluated version of MatchBitmapTextBGColor. Kind of redundant, but it's a bit faster this way.
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
    property ShowCroppingLines: Boolean write SetShowCroppingLines;
  end;

  TFontProfileArr = array of TFontProfile;


  { TfrClickerFindControl }

  TfrClickerFindControl = class(TFrame)
    bitbtnUpdateFontProfile: TBitBtn;
    btnAddBmpFile: TButton;
    btnAddNewFontProfile: TButton;
    btnBrowseBitmap: TButton;
    btnClear: TButton;
    btnDisplaySearchAreaDebuggingImage: TButton;
    btnRemoveBmpFile: TButton;
    btnRemoveFontProfile: TButton;
    btnUpdateBmpFile: TButton;
    btnCopyFoundValues: TButton;
    chkAutoCopyValuesToMatchEditboxes: TCheckBox;
    chkDisplayCroppingLines: TCheckBox;
    chkShowGridOnBMPPreview: TCheckBox;
    chkAllowToFail: TCheckBox;
    chkMatchBitmapFiles: TCheckBox;
    chkMatchBitmapText: TCheckBox;
    chkMatchClassName: TCheckBox;
    chkMatchText: TCheckBox;
    chkSearchCachedLeftAndTopFirst: TCheckBox;
    chkShowBMPFileDbgImg: TCheckBox;
    chkShowBMPTextDbgImg: TCheckBox;
    chkUseWholeScreenAsSearchArea: TCheckBox;
    chkWaitForControlToGoAway: TCheckBox;
    cmbMatchBitmapTextSearchAlgorithm: TComboBox;
    grpFindControlDetailsOnWindow: TGroupBox;
    imgCopySelAreaFromBkImg: TImage;
    imgCopyBMPImg: TImage;
    imgCopyTextImg: TImage;
    imgCopyBkAndBMPImg: TImage;
    imglstFindCriteria: TImageList;
    imglstMatchBitmapFiles: TImageList;
    imgUpdateLeftTopOffsets: TImage;
    imgCopyBkImg: TImage;
    imgUpdateLeftTopRightBottomOffsets: TImage;
    lblFoundControlInfo: TLabel;
    lbeFoundControlText: TLabeledEdit;
    lbeAllowedColorErrorCount: TLabeledEdit;
    lbeColorError: TLabeledEdit;
    lbeCurrentFontProfileName: TLabeledEdit;
    lbeFindCachedControlLeft: TLabeledEdit;
    lbeFindCachedControlTop: TLabeledEdit;
    lbeFoundControlClass: TLabeledEdit;
    lbeMatchBitmapAlgorithmXMulOf: TLabeledEdit;
    lbeMatchBitmapAlgorithmXOffset: TLabeledEdit;
    lbeMatchBitmapAlgorithmYMulOf: TLabeledEdit;
    lbeMatchBitmapAlgorithmYOffset: TLabeledEdit;
    lbeMatchBitmapFile: TLabeledEdit;
    lbeMatchBitmapText: TLabeledEdit;
    lbeMatchClassName: TLabeledEdit;
    lbeMatchClassNameSeparator: TLabeledEdit;
    lbeMatchText: TLabeledEdit;
    lbeMatchTextSeparator: TLabeledEdit;
    lbeSearchRectBottom: TLabeledEdit;
    lbeSearchRectBottomOffset: TLabeledEdit;
    lbeSearchRectLeft: TLabeledEdit;
    lbeSearchRectLeftOffset: TLabeledEdit;
    lbeSearchRectRight: TLabeledEdit;
    lbeSearchRectRightOffset: TLabeledEdit;
    lbeSearchRectTop: TLabeledEdit;
    lbeSearchRectTopOffset: TLabeledEdit;
    lblBMPFilesMatchingInfo: TLabel;
    lblBMPSettingsInfo: TLabel;
    lblBMPTextMatchingInfo: TLabel;
    lblCachingInfo: TLabel;
    lblInfoFindSubControl: TLabel;
    lblInfoFindSubControl2: TLabel;
    lblInfoFindSubControl3: TLabel;
    lblInitRectOffsetsInfo: TLabel;
    lblMatchBitmapTextSearchAlgorithm: TLabel;
    lblMouseOnDbgImg: TLabel;
    lblMouseOnDbgImgBB: TLabel;
    lblMouseOnDbgImgGG: TLabel;
    lblMouseOnDbgImgRR: TLabel;
    lblPreviewControl_Height: TLabel;
    lblPreviewControl_Width: TLabel;
    lblReservedSpaceForDbgImg: TLabel;
    lblSearchAreaValidValuesInfo: TLabel;
    lblSearchInfo: TLabel;
    lblTextMatchingInfo: TLabel;
    lstMatchBitmapFiles: TListBox;
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
    pmStandardControlRefVars: TPopupMenu;
    pnlDrag: TPanel;
    pnlvstMatchBitmapFiles: TPanel;
    pmExtraCopyValueWindows: TPopupMenu;
    rdgrpSearchForControlMode: TRadioGroup;
    spdbtnDisplaySearchAreaDbgImgMenu: TSpeedButton;
    spdbtnExtraCopyValueWindows: TSpeedButton;
    tabctrlBMPText: TTabControl;
    TabSheetActionFindSubControlBMPFiles: TTabSheet;
    TabSheetActionFindSubControlBMPSettings: TTabSheet;
    TabSheetActionFindSubControlBMPText: TTabSheet;
    TabSheetActionFindSubControlCriteria: TTabSheet;
    TabSheetActionFindSubControlSearchArea: TTabSheet;
    TabSheetActionFindSubControlText: TTabSheet;
    tmrDrawZoom: TTimer;
    tmrUpdateGrid: TTimer;
    tmrUpdateSearchAreaOffsetEditBoxes: TTimer;
    updownXMulOf: TUpDown;
    updownXOffset: TUpDown;
    updownYMulOf: TUpDown;
    updownYOffset: TUpDown;
    procedure btnAddBmpFileClick(Sender: TObject);
    procedure btnAddNewFontProfileClick(Sender: TObject);
    procedure btnBrowseBitmapClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnCopyFoundValuesClick(Sender: TObject);
    procedure chkDisplayCroppingLinesChange(Sender: TObject);
    procedure CopyTextAndClassFromPreviewWindowClick(Sender: TObject);
    procedure CopyTextAndClassFromRemoteScreenWindowClick(Sender: TObject);
    procedure CopyTextAndClassFromWinInterpWindowClick(Sender: TObject);
    procedure btnDisplaySearchAreaDebuggingImageClick(Sender: TObject);
    procedure btnRemoveBmpFileClick(Sender: TObject);
    procedure btnRemoveFontProfileClick(Sender: TObject);
    procedure btnUpdateBmpFileClick(Sender: TObject);
    procedure bitbtnUpdateFontProfileClick(Sender: TObject);
    procedure chkAllowToFailClick(Sender: TObject);
    procedure chkMatchBitmapFilesClick(Sender: TObject);
    procedure chkMatchBitmapTextClick(Sender: TObject);
    procedure chkMatchClassNameClick(Sender: TObject);
    procedure chkMatchTextClick(Sender: TObject);
    procedure chkSearchCachedLeftAndTopFirstClick(Sender: TObject);
    procedure chkShowBMPFileDbgImgClick(Sender: TObject);
    procedure chkShowBMPTextDbgImgClick(Sender: TObject);
    procedure chkShowGridOnBMPPreviewChange(Sender: TObject);
    procedure chkUseWholeScreenAsSearchAreaClick(Sender: TObject);
    procedure chkWaitForControlToGoAwayClick(Sender: TObject);
    procedure cmbMatchBitmapTextSearchAlgorithmChange(Sender: TObject);
    procedure cmbMatchBitmapTextSearchAlgorithmCloseUp(Sender: TObject);
    procedure cmbMatchBitmapTextSearchAlgorithmDropDown(Sender: TObject);
    procedure lbeAllowedColorErrorCountChange(Sender: TObject);
    procedure lbeColorErrorChange(Sender: TObject);
    procedure lbeCurrentFontProfileNameEditingDone(Sender: TObject);
    procedure lbeFindCachedControlLeftChange(Sender: TObject);
    procedure lbeFindCachedControlTopChange(Sender: TObject);
    procedure lbeMatchBitmapAlgorithmXMulOfChange(Sender: TObject);
    procedure lbeMatchBitmapAlgorithmXOffsetChange(Sender: TObject);
    procedure lbeMatchBitmapAlgorithmYMulOfChange(Sender: TObject);
    procedure lbeMatchBitmapAlgorithmYOffsetChange(Sender: TObject);
    procedure lbeMatchBitmapTextKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbeMatchClassNameChange(Sender: TObject);
    procedure lbeMatchClassNameSeparatorChange(Sender: TObject);
    procedure lbeMatchTextChange(Sender: TObject);
    procedure lbeMatchTextSeparatorChange(Sender: TObject);
    procedure lbeSearchRectBottomChange(Sender: TObject);
    procedure lbeSearchRectBottomMouseEnter(Sender: TObject);
    procedure lbeSearchRectBottomMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbeSearchRectBottomOffsetChange(Sender: TObject);
    procedure lbeSearchRectBottomOffsetKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbeSearchRectBottomOffsetMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lbeSearchRectBottomOffsetMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure lbeSearchRectLeftChange(Sender: TObject);
    procedure lbeSearchRectLeftMouseEnter(Sender: TObject);
    procedure lbeSearchRectLeftMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbeSearchRectLeftOffsetChange(Sender: TObject);
    procedure lbeSearchRectLeftOffsetKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbeSearchRectLeftOffsetMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lbeSearchRectLeftOffsetMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure lbeSearchRectRightChange(Sender: TObject);
    procedure lbeSearchRectRightMouseEnter(Sender: TObject);
    procedure lbeSearchRectRightMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbeSearchRectRightOffsetChange(Sender: TObject);
    procedure lbeSearchRectRightOffsetKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbeSearchRectRightOffsetMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lbeSearchRectRightOffsetMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure lbeSearchRectTopChange(Sender: TObject);
    procedure lbeSearchRectTopMouseEnter(Sender: TObject);
    procedure lbeSearchRectTopMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbeSearchRectTopOffsetChange(Sender: TObject);
    procedure lbeSearchRectTopOffsetKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbeSearchRectTopOffsetMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lbeSearchRectTopOffsetMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItemControl_EdgeRefGenericClick(Sender: TObject);
    procedure MenuItemCopyRefToClipboardClick(Sender: TObject);
    procedure MenuItemPasteRefFromClipboardClick(Sender: TObject);
    procedure PageControlMatchChange(Sender: TObject);
    procedure pnlDragMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlDragMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlDragMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdgrpSearchForControlModeClick(Sender: TObject);
    procedure spdbtnDisplaySearchAreaDbgImgMenuClick(Sender: TObject);
    procedure spdbtnExtraCopyValueWindowsClick(Sender: TObject);
    procedure tabctrlBMPTextChange(Sender: TObject);
    procedure tmrDrawZoomTimer(Sender: TObject);
    procedure tmrUpdateGridTimer(Sender: TObject);
    procedure tmrUpdateSearchAreaOffsetEditBoxesTimer(Sender: TObject);
    procedure updownXMulOfChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
    procedure updownXOffsetChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection
      );
    procedure updownYMulOfChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
    procedure updownYOffsetChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection
      );
  private
    vstMatchBitmapFiles: TVirtualStringTree;

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

    FSkipDrawingGrid: Boolean; //to be reset after use

    FSearchAreaLeftLimitLabel: TLabel;
    FSearchAreaTopLimitLabel: TLabel;
    FSearchAreaRightLimitLabel: TLabel;
    FSearchAreaBottomLimitLabel: TLabel;

    FTransparent_SearchAreaLeftLimitLabel: TLabel;
    FTransparent_SearchAreaTopLimitLabel: TLabel;
    FTransparent_SearchAreaRightLimitLabel: TLabel;
    FTransparent_SearchAreaBottomLimitLabel: TLabel;

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

    FBMPTextProfiles: TFontProfileArr;
    FInMemFS: TInMemFileSystem; //not created in this unit, set from outside as an existing instance

    FOnTriggerOnControlsModified: TOnTriggerOnControlsModified;
    FOnEvaluateReplacements: TOnEvaluateReplacements;
    FOnUpdateBitmapAlgorithmSettings: TOnUpdateBitmapAlgorithmSettings;
    FOnCopyControlTextAndClassFromMainWindow: TOnCopyControlTextAndClassFromMainWindow;
    FOnGetExtraSearchAreaDebuggingImage: TOnGetExtraSearchAreaDebuggingImage;

    FOnLoadBitmap: TOnLoadBitmap;
    FOnFileExists: TOnFileExists;

    FOnSetPictureOpenDialogInitialDir: TOnSetPictureOpenDialogInitialDir;
    FOnPictureOpenDialogExecute: TOnPictureOpenDialogExecute;
    FOnGetPictureOpenDialogFileName: TOnGetPictureOpenDialogFileName;

    procedure vstMatchBitmapFilesClick(Sender: TObject);
    procedure vstMatchBitmapFilesGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: {$IFDEF FPC} string {$ELSE} WideString {$ENDIF});
    procedure vstMatchBitmapFilesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstMatchBitmapFilesHeaderMouseUp(Sender: TVTHeader;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

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

    procedure MenuItemLoadBmpTextToSearchedAreaClick(Sender: TObject);
    procedure MenuItemGenericLoadBmpToSearchedAreaClick(Sender: TObject);

    procedure FSearchAreaScrBoxMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    function GetFontProfile(Value: Integer): TFontProfile;

    procedure CreateRemainingUIComponents;
    procedure CreateSelectionLabels;
    procedure DoOnTriggerOnControlsModified;
    function DoOnGetExtraSearchAreaDebuggingImage(AExtraBitmap: TBitmap): Boolean;

    function DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function DoOnFileExists(const AFileName: string): Boolean;

    procedure DoOnSetPictureOpenDialogInitialDir(AInitialDir: string);
    function DoOnPictureOpenDialogExecute: Boolean;
    function DoOnGetPictureOpenDialogFileName: string;

    function BrowseBitmapFile: Boolean;
    procedure HandleMatchTextClick;
    procedure GeneratePreviewGridContent;

    function GetFontProfileIndexByName(AProfileName: string): Integer;
    procedure AddFontProfile(AProfileName: string);
    procedure RemoveFontProfileByIndex(AIndex: Integer);

    procedure SelectDbgImgByRectangle(X, Y: Integer);

    function HandleBMPTextOnGetDisplayedText: string;
    procedure HandleBMPTextOnTriggerOnControlsModified;
    function HandleBMPTextOnEvaluateReplacements(s: string): string;
    procedure HandleBMPTextOnSetCroppingValuesToOtherFontProfiles(ACropLeft, ACropTop, ACropRight, ACropBottom: string; ASkipProfileIndex: Integer);
    function HandleBMPTextOnGetCroppingLinesVisiblity: Boolean;

    function GetSearch_LeftLeft_Ref: Integer;    //Left
    function GetSearch_RightLeft_Ref: Integer;

    function GetSearch_RightRight_Ref: Integer;  //Right
    function GetSearch_LeftRight_Ref: Integer;

    function GetSearch_TopTop_Ref: Integer;      //Top
    function GetSearch_BottomTop_Ref: Integer;

    function GetSearch_BottomBottom_Ref: Integer;//Bottom
    function GetSearch_TopBottom_Ref: Integer;

    function EvaluateReplacements(s: string): string;

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

    procedure UpdateSearchAreaLabelsFromKeysOnEditBoxes;
    procedure UpdateSearchAreaLabelColorsFromTheirPosition;
    procedure UpdateTransparent_SearchAreaLimitsFromSearchAreaLimits;
    procedure SetLabelsFromMouseOverDbgImgPixelColor(APixelColor: TColor);
    procedure CopyTextAndClassFromExternalProvider(AProviderName: string);

    procedure UpdateAllSelectionLabelsFromCropEditBoxes;
  public
    //FBMPTextFrames: TfrClickerBMPTextArr; //should eventually made private and accesed through functions

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetBitmapFilesCount: Integer;
    procedure SetBitmapFilesCount;
    procedure CreateBMPTextFrames(ACount: Integer);
    function GetBMPTextFontProfilesCount: Integer;
    procedure SetBMPTextFrameVisibility;

    procedure UpdateControlWidthHeightLabels;
    procedure ClearControls;
    procedure UpdateMatchCriteriaPageIcons;
    procedure UpdateBitmapAlgorithmSettings;
    procedure UpdatePreviewIcons;
    procedure RepaintBitmapFilesVst;
    procedure UpdateSearchAreaLabelsFromOffsetEditboxes;
    procedure SetSearchRectEnabledState;
    procedure DisplayDebuggingImage;
    procedure PreviewText; //called by ExecuteAction

    property BMPsDir: string read FBMPsDir write FBMPsDir;
    property BMPTextFontProfiles[Index: Integer]: TFontProfile read GetFontProfile;

    property InMemFS: TInMemFileSystem write FInMemFS;
    property SearchAreaControlDbgImg: TImage read FSearchAreaControlDbgImg;

    property OnTriggerOnControlsModified: TOnTriggerOnControlsModified read FOnTriggerOnControlsModified write FOnTriggerOnControlsModified;
    property OnEvaluateReplacements: TOnEvaluateReplacements read FOnEvaluateReplacements write FOnEvaluateReplacements;
    property OnUpdateBitmapAlgorithmSettings: TOnUpdateBitmapAlgorithmSettings read FOnUpdateBitmapAlgorithmSettings write FOnUpdateBitmapAlgorithmSettings;
    property OnCopyControlTextAndClassFromMainWindow: TOnCopyControlTextAndClassFromMainWindow read FOnCopyControlTextAndClassFromMainWindow write FOnCopyControlTextAndClassFromMainWindow;
    property OnGetExtraSearchAreaDebuggingImage: TOnGetExtraSearchAreaDebuggingImage write FOnGetExtraSearchAreaDebuggingImage;

    property OnLoadBitmap: TOnLoadBitmap write FOnLoadBitmap;
    property OnFileExists: TOnFileExists write FOnFileExists;
    property OnSetPictureOpenDialogInitialDir: TOnSetPictureOpenDialogInitialDir write FOnSetPictureOpenDialogInitialDir;
    property OnPictureOpenDialogExecute: TOnPictureOpenDialogExecute write FOnPictureOpenDialogExecute;
    property OnGetPictureOpenDialogFileName: TOnGetPictureOpenDialogFileName write FOnGetPictureOpenDialogFileName;
  end;

const
  CPreviewWindow = 'Preview';
  CWinInterpWindow = 'WinInterp';
  CRemoteScreenWindow = 'RemoteScreen';


implementation

{$R *.frm}

uses
  BitmapProcessing, ControlInteraction, Clipbrd, ClickerZoomPreviewForm;


const
  {$IFDEF FPC}
    ID_YES = IDYES;  //from Delphi
  {$ENDIF}


{ TFontProfile }

constructor TFontProfile.Create(AOwner: TfrClickerFindControl; ANewProfileName: string);
begin
  inherited Create;
  FOwner := AOwner;
  FfrClickerBMPText := CreateBMPTextFrame_NoContent(ANewProfileName);
  FOwner.lbeMatchBitmapText.BringToFront;
end;


destructor TFontProfile.Destroy;
begin
  FreeAndNil(FfrClickerBMPText); //it is possible that this call should be made only when FfrClickerBMPText is created without an owner
  inherited Destroy;
end;


function TFontProfile.CreateBMPTextFrame_NoContent(ANewName: string): TfrClickerBMPText;
begin
  Result := TfrClickerBMPText.Create(FOwner);
  Result.Name := ANewName;

  Result.Left := 2;
  Result.Top := 21;
  Result.Parent := FOwner.tabctrlBMPText; //TabSheetActionFindSubControlBMPText;
  Result.Visible := False;
  Result.OnGetDisplayedText := FOwner.HandleBMPTextOnGetDisplayedText;
  Result.OnTriggerOnControlsModified := FOwner.HandleBMPTextOnTriggerOnControlsModified;
  Result.OnEvaluateReplacements := FOwner.HandleBMPTextOnEvaluateReplacements;
  Result.OnSetCroppingValuesToOtherFontProfiles := FOwner.HandleBMPTextOnSetCroppingValuesToOtherFontProfiles;
  Result.OnGetCroppingLinesVisiblity := FOwner.HandleBMPTextOnGetCroppingLinesVisiblity;
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
  Result := FfrClickerBMPText.lbeMatchBitmapTextFGColor.Text;
end;


procedure TFontProfile.SetMatchBitmapTextFGColor(Value: string);
begin
  FfrClickerBMPText.lbeMatchBitmapTextFGColor.Text := Value;
end;


function TFontProfile.GetMatchBitmapTextBGColor: string;
begin
  Result := FfrClickerBMPText.lbeMatchBitmapTextBGColor.Text;
end;


procedure TFontProfile.SetMatchBitmapTextBGColor(Value: string);
begin
  FfrClickerBMPText.lbeMatchBitmapTextBGColor.Text := Value;
end;


function TFontProfile.GetFGColor: TColor;
begin
  Result := FfrClickerBMPText.pnlFG.Color;
end;


procedure TFontProfile.SetFGColor(Value: TColor);
begin
  FfrClickerBMPText.pnlFG.Color := Value;
end;


function TFontProfile.GetBGColor: TColor;
begin
  Result := FfrClickerBMPText.pnlBG.Color;
end;


procedure TFontProfile.SetBGColor(Value: TColor);
begin
  FfrClickerBMPText.pnlBG.Color := Value;
end;


function TFontProfile.GetMatchBitmapTextFontName: string;
begin
  Result := FfrClickerBMPText.lbeMatchBitmapTextFontName.Text;
end;


procedure TFontProfile.SetMatchBitmapTextFontName(Value: string);
begin
  FfrClickerBMPText.lbeMatchBitmapTextFontName.Text := Value;
end;


function TFontProfile.GetMatchBitmapTextSize: string;
begin
  Result := FfrClickerBMPText.lbeMatchBitmapTextSize.Text;
end;


procedure TFontProfile.SetMatchBitmapTextSize(Value: string);
begin
  FfrClickerBMPText.lbeMatchBitmapTextSize.Text := Value;
end;


function TFontProfile.GetFontQualityReplacement: string;
begin
  Result := FfrClickerBMPText.edtFontQualityReplacement.Text;
end;


procedure TFontProfile.SetFontQualityReplacement(Value: string);
begin
  FfrClickerBMPText.edtFontQualityReplacement.Text := Value
end;


function TFontProfile.GetMatchBitmapTextFontQualityIndex: Integer;
begin
  Result := FfrClickerBMPText.cmbMatchBitmapTextFontQuality.ItemIndex;
end;


procedure TFontProfile.SetMatchBitmapTextFontQualityIndex(Value: Integer);
begin
  FfrClickerBMPText.cmbMatchBitmapTextFontQuality.ItemIndex := Value;
  FfrClickerBMPText.edtFontQualityReplacement.Visible := Value = Integer(High(TFontQuality)) + 1;
end;


function TFontProfile.GetBold: Boolean;
begin
  Result := FfrClickerBMPText.chkBold.Checked;
end;


procedure TFontProfile.SetBold(Value: Boolean);
begin
  FfrClickerBMPText.chkBold.Checked := Value;
end;


function TFontProfile.GetItalic: Boolean;
begin
  Result := FfrClickerBMPText.chkItalic.Checked;
end;


procedure TFontProfile.SetItalic(Value: Boolean);
begin
  FfrClickerBMPText.chkItalic.Checked := Value;
end;


function TFontProfile.GetUnderline: Boolean;
begin
  Result := FfrClickerBMPText.chkUnderline.Checked;
end;


procedure TFontProfile.SetUnderline(Value: Boolean);
begin
  FfrClickerBMPText.chkUnderline.Checked := Value;
end;


function TFontProfile.GetStrikeOut: Boolean;
begin
  Result := FfrClickerBMPText.chkStrikeOut.Checked;
end;


procedure TFontProfile.SetStrikeOut(Value: Boolean);
begin
  FfrClickerBMPText.chkStrikeOut.Checked := Value;
end;


function TFontProfile.GetCropLeft: string;
begin
  Result := FfrClickerBMPText.lbeMatchBitmapTextCropLeft.Text;
end;


procedure TFontProfile.SetCropLeft(Value: string);
begin
  FfrClickerBMPText.lbeMatchBitmapTextCropLeft.Text := Value;
end;


function TFontProfile.GetCropTop: string;
begin
  Result := FfrClickerBMPText.lbeMatchBitmapTextCropTop.Text;
end;


procedure TFontProfile.SetCropTop(Value: string);
begin
  FfrClickerBMPText.lbeMatchBitmapTextCropTop.Text := Value;
end;


function TFontProfile.GetCropRight: string;
begin
  Result := FfrClickerBMPText.lbeMatchBitmapTextCropRight.Text;
end;


procedure TFontProfile.SetCropRight(Value: string);
begin
  FfrClickerBMPText.lbeMatchBitmapTextCropRight.Text := Value;
end;


function TFontProfile.GetCropBottom: string;
begin
  Result := FfrClickerBMPText.lbeMatchBitmapTextCropBottom.Text;
end;


procedure TFontProfile.SetCropBottom(Value: string);
begin
  FfrClickerBMPText.lbeMatchBitmapTextCropBottom.Text := Value;
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


{ TfrClickerFindControl }


function TfrClickerFindControl.GetFontProfile(Value: Integer): TFontProfile;
begin
  if (Value < 0) or (Value > Length(FBMPTextProfiles) - 1) then
    raise Exception.Create('Indexing font profile out of bounds: ' + IntToStr(Value));

  Result := FBMPTextProfiles[Value];
end;


procedure TfrClickerFindControl.CreateRemainingUIComponents;
begin
  vstMatchBitmapFiles := TVirtualStringTree.Create(Self);
  vstMatchBitmapFiles.Parent := TabSheetActionFindSubControlBMPFiles;

  vstMatchBitmapFiles.Left := 3;
  vstMatchBitmapFiles.Top := 3;
  vstMatchBitmapFiles.Width := 396;
  vstMatchBitmapFiles.Height := 123;
  vstMatchBitmapFiles.DefaultNodeHeight := 52;
  vstMatchBitmapFiles.Header.AutoSizeIndex := 0;
  vstMatchBitmapFiles.Header.DefaultHeight := 17;
  vstMatchBitmapFiles.Header.Font.Charset := DEFAULT_CHARSET;
  vstMatchBitmapFiles.Header.Font.Color := clWindowText;
  vstMatchBitmapFiles.Header.Font.Height := -11;
  vstMatchBitmapFiles.Header.Font.Name := 'Tahoma';
  vstMatchBitmapFiles.Header.Font.Style := [];
  vstMatchBitmapFiles.Header.MainColumn := -1;
  vstMatchBitmapFiles.Indent := 2;
  vstMatchBitmapFiles.ScrollBarOptions.AlwaysVisible := True;
  vstMatchBitmapFiles.StateImages := imglstMatchBitmapFiles;
  vstMatchBitmapFiles.TabOrder := 0;
  vstMatchBitmapFiles.TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages];
  vstMatchBitmapFiles.TreeOptions.SelectionOptions := [toFullRowSelect, toMiddleClickSelect, toRightClickSelect];
  vstMatchBitmapFiles.OnClick := vstMatchBitmapFilesClick;
  vstMatchBitmapFiles.OnGetText := vstMatchBitmapFilesGetText;
  vstMatchBitmapFiles.OnGetImageIndex := vstMatchBitmapFilesGetImageIndex;
  vstMatchBitmapFiles.OnHeaderMouseUp := vstMatchBitmapFilesHeaderMouseUp;
  vstMatchBitmapFiles.Colors.UnfocusedSelectionColor := clGradientInactiveCaption;
end;


procedure TfrClickerFindControl.CreateSelectionLabels;
begin
  FTransparent_SearchAreaLeftLimitLabel := TLabel.Create(Self);
  FTransparent_SearchAreaTopLimitLabel := TLabel.Create(Self);
  FTransparent_SearchAreaRightLimitLabel := TLabel.Create(Self);
  FTransparent_SearchAreaBottomLimitLabel := TLabel.Create(Self);

  FTransparent_SearchAreaLeftLimitLabel.Parent := FSearchAreaScrBox;
  FTransparent_SearchAreaTopLimitLabel.Parent := FSearchAreaScrBox;
  FTransparent_SearchAreaRightLimitLabel.Parent := FSearchAreaScrBox;
  FTransparent_SearchAreaBottomLimitLabel.Parent := FSearchAreaScrBox;

  FTransparent_SearchAreaLeftLimitLabel.AutoSize := False;
  FTransparent_SearchAreaTopLimitLabel.AutoSize := False;
  FTransparent_SearchAreaRightLimitLabel.AutoSize := False;
  FTransparent_SearchAreaBottomLimitLabel.AutoSize := False;

  FTransparent_SearchAreaLeftLimitLabel.Caption := '';
  FTransparent_SearchAreaTopLimitLabel.Caption := '';
  FTransparent_SearchAreaRightLimitLabel.Caption := '';
  FTransparent_SearchAreaBottomLimitLabel.Caption := '';

  FTransparent_SearchAreaLeftLimitLabel.Color := clDefault;
  FTransparent_SearchAreaTopLimitLabel.Color := clDefault;
  FTransparent_SearchAreaRightLimitLabel.Color := clDefault;
  FTransparent_SearchAreaBottomLimitLabel.Color := clDefault;

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

  FTransparent_SearchAreaRightLimitLabel.OnMouseDown := FTransparent_RightMouseDown;
  FTransparent_SearchAreaRightLimitLabel.OnMouseMove := FTransparent_RightMouseMove;
  FTransparent_SearchAreaRightLimitLabel.OnMouseUp := FTransparent_RightMouseUp;

  FTransparent_SearchAreaTopLimitLabel.OnMouseDown := FTransparent_TopMouseDown;
  FTransparent_SearchAreaTopLimitLabel.OnMouseMove := FTransparent_TopMouseMove;
  FTransparent_SearchAreaTopLimitLabel.OnMouseUp := FTransparent_TopMouseUp;

  FTransparent_SearchAreaBottomLimitLabel.OnMouseDown := FTransparent_BottomMouseDown;
  FTransparent_SearchAreaBottomLimitLabel.OnMouseMove := FTransparent_BottomMouseMove;
  FTransparent_SearchAreaBottomLimitLabel.OnMouseUp := FTransparent_BottomMouseUp;
end;


constructor TfrClickerFindControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOnTriggerOnControlsModified := nil;
  FOnEvaluateReplacements := nil;
  FOnUpdateBitmapAlgorithmSettings := nil;
  FOnCopyControlTextAndClassFromMainWindow := nil;
  FOnGetExtraSearchAreaDebuggingImage := nil;

  FOnLoadBitmap := nil;
  FOnFileExists := nil;
  FOnSetPictureOpenDialogInitialDir := nil;
  FOnPictureOpenDialogExecute := nil;
  FOnGetPictureOpenDialogFileName := nil;

  CreateRemainingUIComponents; //this should be called after initializing callback properties to nil  (like FOnTriggerOnControlsModified)
  SetLength(FBMPTextProfiles, 0);
  //CreateSelectionLabels is called where all the other labels are created

  FSearchAreaScrBox := nil;
  FSearchAreaControlDbgImg := nil;
  FSearchAreaSearchedBmpDbgImg := nil;
  FSearchAreaSearchedTextDbgImg := nil;
  FSkipDrawingGrid := False;

  FLastClickedLbe := nil;
  FDbgImgHold := False;
  FRectangleSelecting := False;
  FDragging := False;
  FInMemFS := nil;

  PageControlMatch.ActivePageIndex := 0;

  rdgrpSearchForControlMode.Hint := 'With "Generate Grid", the application generates a grid of points, where it queries for a window/control.' + #13#10 +
                                    'With "Enumerate Windows", it lists all top-level windows and matches their caption and/or class.' + #13#10 +
                                    'With "Find Window", both class and caption have to match and no wildcard is available.';

  AddFontProfile('Default');
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


function TfrClickerFindControl.GetBitmapFilesCount: Integer;
begin
  Result := vstMatchBitmapFiles.RootNodeCount;
end;


procedure TfrClickerFindControl.SetBitmapFilesCount;
begin
  vstMatchBitmapFiles.RootNodeCount := lstMatchBitmapFiles.Count;
end;


procedure TfrClickerFindControl.SetBMPTextFrameVisibility;
var
  i: Integer;
begin
  for i := 0 to Length(FBMPTextProfiles) - 1 do
    FBMPTextProfiles[i].EditorVisible := i = tabctrlBMPText.TabIndex;

  if tabctrlBMPText.TabIndex = -1 then
    lbeCurrentFontProfileName.Text := ''
  else
    lbeCurrentFontProfileName.Text := tabctrlBMPText.Tabs.Strings[tabctrlBMPText.TabIndex];
end;


procedure TfrClickerFindControl.UpdateMatchCriteriaPageIcons;
var
  i: Integer;
  Sel: Byte;
begin
  for i := 0 to PageControlMatch.PageCount - 1 do
    PageControlMatch.Pages[i].ImageIndex := 0;

  PageControlMatch.Pages[4].ImageIndex := 2;
  PageControlMatch.Pages[5].ImageIndex := 2;

  if not chkMatchText.Checked and
     not chkMatchClassName.Checked and
     not chkMatchBitmapText.Checked and
     not chkMatchBitmapFiles.Checked then
    Exit;

  if chkMatchText.Checked or chkMatchClassName.Checked then
  begin
    PageControlMatch.Pages[0].ImageIndex := 1;
    PageControlMatch.Pages[1].ImageIndex := 1;
  end;

  if chkMatchBitmapText.Checked then
  begin
    PageControlMatch.Pages[0].ImageIndex := 1;
    PageControlMatch.Pages[2].ImageIndex := 1;
    lblBMPTextMatchingInfo.Caption := 'BMP text matching enabled.';
  end
  else
    lblBMPTextMatchingInfo.Caption := 'BMP text matching disabled.';

  if chkMatchBitmapFiles.Checked then
  begin
    PageControlMatch.Pages[0].ImageIndex := 1;
    PageControlMatch.Pages[3].ImageIndex := 1;
    lblBMPFilesMatchingInfo.Caption := 'BMP files matching enabled.';
  end
  else
    lblBMPFilesMatchingInfo.Caption := 'BMP files matching disabled.';

  Sel := Ord(chkMatchBitmapText.Checked) shl 1 + Ord(chkMatchText.Checked);
  if Sel > 0 then
  begin
    lblTextMatchingInfo.Caption := 'Text matching enabled';

    case Sel of
      1: lblTextMatchingInfo.Caption := lblTextMatchingInfo.Caption + ' (text only).';
      2: lblTextMatchingInfo.Caption := lblTextMatchingInfo.Caption + ' (BMP text only).';
      3: lblTextMatchingInfo.Caption := lblTextMatchingInfo.Caption + ' (text and BMP text).';
    else
    end;
  end
  else
    lblTextMatchingInfo.Caption := 'Text matching disabled.';
end;


function TfrClickerFindControl.BrowseBitmapFile: Boolean;
begin
  Result := False;

  DoOnSetPictureOpenDialogInitialDir(FBMPsDir);
  if not DoOnPictureOpenDialogExecute then
    Exit;

  lbeMatchBitmapFile.Text := DoOnGetPictureOpenDialogFileName;
  FBMPsDir := ExtractFileDir(DoOnGetPictureOpenDialogFileName);

  Result := True;
end;


procedure TfrClickerFindControl.SetSearchRectEnabledState;
begin
  lbeSearchRectTop.Enabled := not chkUseWholeScreenAsSearchArea.Checked;
  lbeSearchRectRight.Enabled := not chkUseWholeScreenAsSearchArea.Checked;
  lbeSearchRectLeft.Enabled := not chkUseWholeScreenAsSearchArea.Checked;
  lbeSearchRectBottom.Enabled := not chkUseWholeScreenAsSearchArea.Checked;
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
  Result := lbeMatchBitmapText.Text;
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


function TfrClickerFindControl.EvaluateReplacements(s: string): string;
begin
  if Assigned(FOnEvaluateReplacements) then
    Result := FOnEvaluateReplacements(s)
  else
    raise Exception.Create('OnEvaluateReplacements not assigned.');
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

  n := Length(FBMPTextProfiles);
  SetLength(FBMPTextProfiles, n + 1);
  FBMPTextProfiles[n] := TFontProfile.Create(Self, 'FBMPTextFrames_' + IntToStr(n));
  FBMPTextProfiles[n].ProfileName := AProfileName;

  tabctrlBMPText.Tabs.Add(AProfileName);
  tabctrlBMPText.TabIndex := tabctrlBMPText.Tabs.Count - 1;

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

    tabctrlBMPText.Tabs.Add('no name ' + IntToStr(i));
    tabctrlBMPText.TabIndex := tabctrlBMPText.Tabs.Count - 1;
    lbeCurrentFontProfileName.Text := tabctrlBMPText.Tabs.Strings[tabctrlBMPText.TabIndex];
  end;
end;


function TfrClickerFindControl.GetBMPTextFontProfilesCount: Integer;
begin
  Result := Length(FBMPTextProfiles);
end;


procedure TfrClickerFindControl.chkUseWholeScreenAsSearchAreaClick(
  Sender: TObject);
begin
  SetSearchRectEnabledState;
  DoOnTriggerOnControlsModified;
  UpdateSearchAreaLabelsFromOffsetEditboxes;
end;


procedure TfrClickerFindControl.rdgrpSearchForControlModeClick(Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.spdbtnDisplaySearchAreaDbgImgMenuClick(
  Sender: TObject);
var
  tp: TPoint;
begin
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
    Line(TempBmp.Canvas, FSearchAreaLeftLimitLabel.Left, 0, FSearchAreaLeftLimitLabel.Left, TempBmp.Width - 1);

    TempBmp.Canvas.Pen.Color := FSearchAreaTopLimitLabel.Color;
    Line(TempBmp.Canvas, 0, FSearchAreaTopLimitLabel.Top, TempBmp.Width - 1, FSearchAreaTopLimitLabel.Top);

    TempBmp.Canvas.Pen.Color := FSearchAreaRightLimitLabel.Color;
    Line(TempBmp.Canvas, FSearchAreaRightLimitLabel.Left, 0, FSearchAreaRightLimitLabel.Left, TempBmp.Width - 1);

    TempBmp.Canvas.Pen.Color := FSearchAreaBottomLimitLabel.Color;
    Line(TempBmp.Canvas, 0, FSearchAreaBottomLimitLabel.Top, TempBmp.Width - 1, FSearchAreaBottomLimitLabel.Top);

    if chkShowGridOnBMPPreview.Checked then
    begin
      if FSkipDrawingGrid then
        FSkipDrawingGrid := False
      else
        GeneratePreviewGridContent;

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
  GeneratePreviewGridContent;
end;


procedure TfrClickerFindControl.tmrUpdateSearchAreaOffsetEditBoxesTimer(
  Sender: TObject);
begin
  tmrUpdateSearchAreaOffsetEditBoxes.Enabled := False;

  lbeSearchRectLeftOffset.Text := IntToStr(GetSearchAreaLeftOffsetFromSelLabel);
  lbeSearchRectTopOffset.Text := IntToStr(GetSearchAreaTopOffsetFromSelLabel);
  lbeSearchRectRightOffset.Text := IntToStr(GetSearchAreaRightOffsetFromSelLabel - GetControlWidthFromReplacement);
  lbeSearchRectBottomOffset.Text := IntToStr(GetSearchAreaBottomOffsetFromSelLabel - GetControlHeightFromReplacement);

  tmrUpdateGrid.Enabled := True;
end;


const
  CDirIncrement: array[TUpDownDirection] of Integer = (0, 1, -1);  //TUpDownDirection = (updNone, updUp, updDown);

procedure TfrClickerFindControl.updownXMulOfChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  lbeMatchBitmapAlgorithmXMulOf.Text := IntToStr(StrToIntDef(lbeMatchBitmapAlgorithmXMulOf.Text, 0) + CDirIncrement[Direction]);
end;


procedure TfrClickerFindControl.updownXOffsetChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  lbeMatchBitmapAlgorithmXOffset.Text := IntToStr(StrToIntDef(lbeMatchBitmapAlgorithmXOffset.Text, 0) + CDirIncrement[Direction]);
end;


procedure TfrClickerFindControl.updownYMulOfChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  lbeMatchBitmapAlgorithmYMulOf.Text := IntToStr(StrToIntDef(lbeMatchBitmapAlgorithmYMulOf.Text, 0) + CDirIncrement[Direction]);
end;


procedure TfrClickerFindControl.updownYOffsetChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  lbeMatchBitmapAlgorithmYOffset.Text := IntToStr(StrToIntDef(lbeMatchBitmapAlgorithmYOffset.Text, 0) + CDirIncrement[Direction]);
end;


procedure TfrClickerFindControl.HandleMatchTextClick;
begin
  UpdateMatchCriteriaPageIcons;
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.chkSearchCachedLeftAndTopFirstClick(
  Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
  lbeFindCachedControlLeft.Enabled := chkSearchCachedLeftAndTopFirst.Checked;
  lbeFindCachedControlTop.Enabled := chkSearchCachedLeftAndTopFirst.Checked;
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


procedure TfrClickerFindControl.GeneratePreviewGridContent;
var
  AlgorithmSettings: TMatchBitmapAlgorithmSettings;
begin
  if FSearchAreaControlDbgImg = nil then
    Exit;

  AlgorithmSettings.XMultipleOf := StrToIntDef(lbeMatchBitmapAlgorithmXMulOf.Text, 20);
  AlgorithmSettings.YMultipleOf := StrToIntDef(lbeMatchBitmapAlgorithmYMulOf.Text, 20);
  AlgorithmSettings.XOffset := StrToIntDef(lbeMatchBitmapAlgorithmXOffset.Text, 0);
  AlgorithmSettings.YOffset := StrToIntDef(lbeMatchBitmapAlgorithmYOffset.Text, 0);

  FSearchAreaGridImg.Left := AlgorithmSettings.XOffset + FSearchAreaLeftLimitLabel.Left;
  FSearchAreaGridImg.Top := AlgorithmSettings.YOffset + FSearchAreaTopLimitLabel.Top;

  FSearchAreaGridImg.Transparent := True;

  WipeImage(FSearchAreaGridImg, FSearchAreaControlDbgImg.Width, FSearchAreaControlDbgImg.Height);
  FSearchAreaGridImg.Picture.Bitmap.TransparentColor := clWhite;

  if chkShowGridOnBMPPreview.Checked then
    DrawSearchGrid(FSearchAreaGridImg, AlgorithmSettings, FSearchAreaControlDbgImg.Width, FSearchAreaControlDbgImg.Height, $00C9AEFF);

  MakeImageContentTransparent(FSearchAreaGridImg);
end;


procedure TfrClickerFindControl.chkShowGridOnBMPPreviewChange(Sender: TObject);
begin
  if chkShowGridOnBMPPreview.Checked then
    GeneratePreviewGridContent;

  if FSearchAreaControlDbgImg <> nil then
    FSearchAreaGridImg.Visible := chkShowGridOnBMPPreview.Checked;
end;


procedure TfrClickerFindControl.chkMatchTextClick(Sender: TObject);
begin
  HandleMatchTextClick;
end;


procedure TfrClickerFindControl.chkMatchClassNameClick(Sender: TObject);
begin
  HandleMatchTextClick;
end;


procedure TfrClickerFindControl.chkMatchBitmapTextClick(Sender: TObject);
begin
  HandleMatchTextClick;
end;


procedure TfrClickerFindControl.chkMatchBitmapFilesClick(Sender: TObject);
begin
  HandleMatchTextClick;
end;


procedure TfrClickerFindControl.chkWaitForControlToGoAwayClick(Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.cmbMatchBitmapTextSearchAlgorithmChange(
  Sender: TObject);
begin
  UpdateBitmapAlgorithmSettings;
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.cmbMatchBitmapTextSearchAlgorithmCloseUp(
  Sender: TObject);
begin
  cmbMatchBitmapTextSearchAlgorithm.Width := cmbMatchBitmapTextSearchAlgorithm.Width - 160;
end;


procedure TfrClickerFindControl.cmbMatchBitmapTextSearchAlgorithmDropDown(
  Sender: TObject);
begin
  cmbMatchBitmapTextSearchAlgorithm.Width := cmbMatchBitmapTextSearchAlgorithm.Width + 160;
end;


procedure TfrClickerFindControl.lbeAllowedColorErrorCountChange(Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.lbeColorErrorChange(Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.lbeCurrentFontProfileNameEditingDone(
  Sender: TObject);
begin
  bitbtnUpdateFontProfile.Font.Color := clRed;
  bitbtnUpdateFontProfile.Font.Style := [fsBold];
end;


procedure TfrClickerFindControl.lbeFindCachedControlLeftChange(Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.lbeFindCachedControlTopChange(Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.lbeMatchBitmapAlgorithmXMulOfChange(
  Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
  GeneratePreviewGridContent;
end;


procedure TfrClickerFindControl.lbeMatchBitmapAlgorithmXOffsetChange(
  Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
  GeneratePreviewGridContent;
end;


procedure TfrClickerFindControl.lbeMatchBitmapAlgorithmYMulOfChange(
  Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
  GeneratePreviewGridContent;
end;


procedure TfrClickerFindControl.lbeMatchBitmapAlgorithmYOffsetChange(
  Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
  GeneratePreviewGridContent;
end;


procedure TfrClickerFindControl.lbeMatchBitmapTextKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  for i := 0 to Length(FBMPTextProfiles) - 1 do
    FBMPTextProfiles[i].PreviewText;
end;


procedure TfrClickerFindControl.lbeMatchClassNameChange(Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.lbeMatchClassNameSeparatorChange(Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.lbeMatchTextChange(Sender: TObject);
begin
  lbeMatchBitmapText.Text := lbeMatchText.Text;
  DoOnTriggerOnControlsModified;
  PreviewText;
  UpdateAllSelectionLabelsFromCropEditBoxes;
end;


procedure TfrClickerFindControl.lbeMatchTextSeparatorChange(Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.lbeSearchRectBottomChange(Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.lbeSearchRectBottomMouseEnter(Sender: TObject);
begin
  lbeSearchRectBottom.Hint := 'Bottom edge of the search area. Variable replacements are available.' + #13#10 +
                             lbeSearchRectBottom.Text + ' = ' + EvaluateReplacements(lbeSearchRectBottom.Text);
end;


procedure TfrClickerFindControl.lbeSearchRectBottomMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbRight then
    Exit;

  FLastClickedLbe := Sender as TLabeledEdit;
end;


procedure TfrClickerFindControl.lbeSearchRectBottomOffsetChange(Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.lbeSearchRectBottomOffsetKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  UpdateSearchAreaLabelsFromKeysOnEditBoxes;
end;


procedure TfrClickerFindControl.lbeSearchRectBottomOffsetMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Ref: Integer;
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
  begin
    Ref := GetSearch_BottomBottom_Ref;
    FlbeSearchRectOffsetMDownInit := Y;  //used to compute offset
    FlbeSearchRectOffsetMDownValueInit := StrToIntDef(EvaluateReplacements(lbeSearchRectBottomOffset.Text), MaxInt) - Ref;  //editbox value on mouse down

    Ref := GetSearch_TopBottom_Ref;
    FlbeSearchRectOffsetMDownSecondValueInit := StrToIntDef(EvaluateReplacements(lbeSearchRectTopOffset.Text), MaxInt) - Ref;  //editbox value on mouse down
  end;
end;


procedure TfrClickerFindControl.lbeSearchRectBottomOffsetMouseMove(
  Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  ControlHeight: Integer;
  Ref: Integer;
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
    if FlbeSearchRectOffsetMDownValueInit <> MaxInt then  //MaxInt is used as an indicator that a replacement is used, not a numeric value
    begin
      Ref := GetSearch_BottomBottom_Ref;
      lbeSearchRectBottomOffset.Text := IntToStr(FlbeSearchRectOffsetMDownValueInit - (Y - FlbeSearchRectOffsetMDownInit) + Ref);

      if FSearchAreaSearchedBmpDbgImg <> nil then
      begin
        ControlHeight := GetControlHeightFromReplacement;
        FSearchAreaBottomLimitLabel.Top := StrToIntDef(EvaluateReplacements(lbeSearchRectBottomOffset.Text), 20) - Ref + ControlHeight;
        FTransparent_SearchAreaBottomLimitLabel.Top := FSearchAreaBottomLimitLabel.Top;
      end;

      if ssShift in Shift then
      begin
        Ref := GetSearch_TopBottom_Ref;
        lbeSearchRectTopOffset.Text := IntToStr(FlbeSearchRectOffsetMDownSecondValueInit - (Y - FlbeSearchRectOffsetMDownInit) + Ref);

        if FSearchAreaSearchedBmpDbgImg <> nil then
        begin
          //ControlHeight is initialized above
          FSearchAreaTopLimitLabel.Top := StrToIntDef(EvaluateReplacements(lbeSearchRectTopOffset.Text), 20) - Ref + ControlHeight;
          FTransparent_SearchAreaTopLimitLabel.Top := FSearchAreaTopLimitLabel.Top;
          FSearchAreaSearchedBmpDbgImg.Top := FSearchAreaTopLimitLabel.Top;
          tmrUpdateGrid.Enabled := True;
        end;
      end;

      if Assigned(FSearchAreaSearchedTextDbgImg) and
         Assigned(FSearchAreaSearchedBmpDbgImg) then
      begin
        FSearchAreaSearchedTextDbgImg.Top := FSearchAreaSearchedBmpDbgImg.Top;
        UpdateSearchAreaLabelColorsFromTheirPosition;
      end;
    end;
end;


procedure TfrClickerFindControl.lbeSearchRectLeftChange(Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.lbeSearchRectLeftMouseEnter(Sender: TObject);
begin
  lbeSearchRectLeft.Hint := 'Left edge of the search area. Variable replacements are available.' + #13#10 +
                            lbeSearchRectLeft.Text + ' = ' + EvaluateReplacements(lbeSearchRectLeft.Text);
end;


procedure TfrClickerFindControl.lbeSearchRectLeftMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbRight then
    Exit;

  FLastClickedLbe := Sender as TLabeledEdit;
end;


procedure TfrClickerFindControl.lbeSearchRectLeftOffsetChange(Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.lbeSearchRectLeftOffsetKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  UpdateSearchAreaLabelsFromKeysOnEditBoxes;
end;


procedure TfrClickerFindControl.lbeSearchRectLeftOffsetMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Ref: Integer;
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
  begin
    Ref := GetSearch_LeftLeft_Ref;

    FlbeSearchRectOffsetMDownInit := Y;  //used to compute offset
    FlbeSearchRectOffsetMDownValueInit := StrToIntDef(EvaluateReplacements(lbeSearchRectLeftOffset.Text), MaxInt) - Ref;  //editbox value on mouse down

    Ref := GetSearch_RightLeft_Ref;
    FlbeSearchRectOffsetMDownSecondValueInit := StrToIntDef(EvaluateReplacements(lbeSearchRectRightOffset.Text), MaxInt) - Ref;  //editbox value on mouse down
  end;
end;


procedure TfrClickerFindControl.lbeSearchRectLeftOffsetMouseMove(
  Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Ref: Integer;
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
    if FlbeSearchRectOffsetMDownValueInit <> MaxInt then  //MaxInt is used as an indicator that a replacement is used, not a numeric value
    begin
      Ref := GetSearch_LeftLeft_Ref;
      lbeSearchRectLeftOffset.Text := IntToStr(FlbeSearchRectOffsetMDownValueInit - (Y - FlbeSearchRectOffsetMDownInit) + Ref);

      if FSearchAreaSearchedBmpDbgImg <> nil then
      begin
        FSearchAreaSearchedBmpDbgImg.Left := StrToIntDef(EvaluateReplacements(lbeSearchRectLeftOffset.Text), 20) - Ref;
        FSearchAreaLeftLimitLabel.Left := FSearchAreaSearchedBmpDbgImg.Left;
        FTransparent_SearchAreaLeftLimitLabel.Left := FSearchAreaLeftLimitLabel.Left;
        tmrUpdateGrid.Enabled := True;
      end;

      if ssShift in Shift then
      begin
        Ref := GetSearch_RightLeft_Ref;
        lbeSearchRectRightOffset.Text := IntToStr(FlbeSearchRectOffsetMDownSecondValueInit - (Y - FlbeSearchRectOffsetMDownInit) + Ref);

        if FSearchAreaSearchedBmpDbgImg <> nil then
        begin
          //if chkUseWholeScreenAsSearchArea.Checked then
          //  ControlRight := Screen.Width
          //else
          //  ControlRight := StrToIntDef(EvaluateReplacements(lbeSearchRectRight.Text), 20) - StrToIntDef(EvaluateReplacements(lbeSearchRectLeft.Text), 20); //Right edge of the search area.

          FSearchAreaRightLimitLabel.Left := StrToIntDef(lbeSearchRectRightOffset.Text, 20) - Ref;   ////////// should be control right + offset (offset can be negative)
          FTransparent_SearchAreaRightLimitLabel.Left := FSearchAreaRightLimitLabel.Left;
        end;
      end;

      if Assigned(FSearchAreaSearchedTextDbgImg) and
         Assigned(FSearchAreaSearchedBmpDbgImg) then
      begin
        FSearchAreaSearchedTextDbgImg.Left := FSearchAreaSearchedBmpDbgImg.Left;
        UpdateSearchAreaLabelColorsFromTheirPosition;
      end;
    end;
end;


procedure TfrClickerFindControl.lbeSearchRectRightChange(Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.lbeSearchRectRightMouseEnter(Sender: TObject);
begin
  lbeSearchRectRight.Hint := 'Right edge of the search area. Variable replacements are available.' + #13#10 +
                             lbeSearchRectRight.Text + ' = ' + EvaluateReplacements(lbeSearchRectRight.Text);
end;


procedure TfrClickerFindControl.lbeSearchRectRightMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbRight then
    Exit;

  FLastClickedLbe := Sender as TLabeledEdit;
end;


procedure TfrClickerFindControl.lbeSearchRectRightOffsetChange(Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.lbeSearchRectRightOffsetKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  UpdateSearchAreaLabelsFromKeysOnEditBoxes;
end;


procedure TfrClickerFindControl.lbeSearchRectRightOffsetMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Ref: Integer;
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
  begin
    Ref := GetSearch_RightRight_Ref;

    FlbeSearchRectOffsetMDownInit := Y;  //used to compute offset
    FlbeSearchRectOffsetMDownValueInit := StrToIntDef(EvaluateReplacements(lbeSearchRectRightOffset.Text), MaxInt) - Ref;  //editbox value on mouse down

    Ref := GetSearch_LeftRight_Ref;
    FlbeSearchRectOffsetMDownSecondValueInit := StrToIntDef(EvaluateReplacements(lbeSearchRectLeftOffset.Text), MaxInt) - Ref;  //editbox value on mouse down
  end;
end;


procedure TfrClickerFindControl.lbeSearchRectRightOffsetMouseMove(
  Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  ControlWidth: Integer;
  Ref: Integer;
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
    if FlbeSearchRectOffsetMDownValueInit <> MaxInt then  //MaxInt is used as an indicator that a replacement is used, not a numeric value
    begin
      Ref := GetSearch_RightRight_Ref;
      lbeSearchRectRightOffset.Text := IntToStr(FlbeSearchRectOffsetMDownValueInit - (Y - FlbeSearchRectOffsetMDownInit) + Ref);

      if FSearchAreaSearchedBmpDbgImg <> nil then
      begin
        ControlWidth := GetControlWidthFromReplacement;
        FSearchAreaRightLimitLabel.Left := StrToIntDef(EvaluateReplacements(lbeSearchRectRightOffset.Text), 20) - Ref + ControlWidth;
        FTransparent_SearchAreaRightLimitLabel.Left := FSearchAreaRightLimitLabel.Left;
      end;

      if ssShift in Shift then
      begin
        Ref := GetSearch_LeftRight_Ref;
        lbeSearchRectLeftOffset.Text := IntToStr(FlbeSearchRectOffsetMDownSecondValueInit - (Y - FlbeSearchRectOffsetMDownInit) + Ref);

        if FSearchAreaSearchedBmpDbgImg <> nil then
        begin
          //ControlWidth is initialized above
          FSearchAreaLeftLimitLabel.Left := StrToIntDef(EvaluateReplacements(lbeSearchRectLeftOffset.Text), 20) - Ref + ControlWidth;
          FTransparent_SearchAreaLeftLimitLabel.Left := FSearchAreaLeftLimitLabel.Left;
          FSearchAreaSearchedBmpDbgImg.Left := FSearchAreaLeftLimitLabel.Left;
          tmrUpdateGrid.Enabled := True;
        end;
      end;

      if Assigned(FSearchAreaSearchedTextDbgImg) and
         Assigned(FSearchAreaSearchedBmpDbgImg) then
      begin
        FSearchAreaSearchedTextDbgImg.Left := FSearchAreaSearchedBmpDbgImg.Left;
        UpdateSearchAreaLabelColorsFromTheirPosition;
      end;
    end;
end;


procedure TfrClickerFindControl.lbeSearchRectTopChange(Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.lbeSearchRectTopMouseEnter(Sender: TObject);
begin
  lbeSearchRectTop.Hint := 'Top edge of the search area. Variable replacements are available.' + #13#10 +
                           lbeSearchRectTop.Text + ' = ' + EvaluateReplacements(lbeSearchRectTop.Text);
end;


procedure TfrClickerFindControl.lbeSearchRectTopMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbRight then
    Exit;

  FLastClickedLbe := Sender as TLabeledEdit;
end;


procedure TfrClickerFindControl.lbeSearchRectTopOffsetChange(Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.lbeSearchRectTopOffsetKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  UpdateSearchAreaLabelsFromKeysOnEditBoxes;
end;


procedure TfrClickerFindControl.lbeSearchRectTopOffsetMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Ref: Integer;
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
  begin
    Ref := GetSearch_TopTop_Ref;
    FlbeSearchRectOffsetMDownInit := Y;  //used to compute offset
    FlbeSearchRectOffsetMDownValueInit := StrToIntDef(EvaluateReplacements(lbeSearchRectTopOffset.Text), MaxInt) - Ref;  //editbox value on mouse down

    Ref := GetSearch_BottomTop_Ref;
    FlbeSearchRectOffsetMDownSecondValueInit := StrToIntDef(EvaluateReplacements(lbeSearchRectBottomOffset.Text), MaxInt) - Ref;  //editbox value on mouse down
  end;
end;


procedure TfrClickerFindControl.lbeSearchRectTopOffsetMouseMove(
  Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Ref: Integer;
begin
  if (ssLeft in Shift) and (ssCtrl in Shift) then
    if FlbeSearchRectOffsetMDownValueInit <> MaxInt then  //MaxInt is used as an indicator that a replacement is used, not a numeric value
    begin
      Ref := GetSearch_TopTop_Ref;
      lbeSearchRectTopOffset.Text := IntToStr(FlbeSearchRectOffsetMDownValueInit - (Y - FlbeSearchRectOffsetMDownInit) + Ref);

      if FSearchAreaSearchedBmpDbgImg <> nil then
      begin
        FSearchAreaSearchedBmpDbgImg.Top := StrToIntDef(EvaluateReplacements(lbeSearchRectTopOffset.Text), 20) - Ref;
        FTransparent_SearchAreaTopLimitLabel.Top := FSearchAreaTopLimitLabel.Top;
        FSearchAreaTopLimitLabel.Top := FSearchAreaSearchedBmpDbgImg.Top;
        tmrUpdateGrid.Enabled := True;
      end;

      if ssShift in Shift then
      begin
        Ref := GetSearch_BottomTop_Ref;
        lbeSearchRectBottomOffset.Text := IntToStr(FlbeSearchRectOffsetMDownSecondValueInit - (Y - FlbeSearchRectOffsetMDownInit) + Ref);

        if FSearchAreaSearchedBmpDbgImg <> nil then
        begin
          //if chkUseWholeScreenAsSearchArea.Checked then
          //  ControlBottom := Screen.Height
          //else
          //  ControlBottom := StrToIntDef(EvaluateReplacements(lbeSearchRectBottom.Text), 20) - StrToIntDef(EvaluateReplacements(lbeSearchRectTop.Text), 20); //Bottom edge of the search area.

          FSearchAreaBottomLimitLabel.Top := StrToIntDef(lbeSearchRectBottomOffset.Text, 20) - Ref;    ////////// should be control bottom + offset (offset can be negative)
          FTransparent_SearchAreaBottomLimitLabel.Top := FSearchAreaBottomLimitLabel.Top;
        end;
      end;

      if Assigned(FSearchAreaSearchedTextDbgImg) and
         Assigned(FSearchAreaSearchedBmpDbgImg) then
      begin
        FSearchAreaSearchedTextDbgImg.Top := FSearchAreaSearchedBmpDbgImg.Top;
        UpdateSearchAreaLabelColorsFromTheirPosition;
      end;
    end;
end;


procedure TfrClickerFindControl.MenuItemControl_EdgeRefGenericClick(Sender: TObject);
begin
  FLastClickedLbe.Text := StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]);
end;


procedure TfrClickerFindControl.MenuItemCopyRefToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := FLastClickedLbe.Text;
end;


procedure TfrClickerFindControl.MenuItemPasteRefFromClipboardClick(Sender: TObject);
begin
  FLastClickedLbe.Text := Clipboard.AsText;
end;


procedure TfrClickerFindControl.PageControlMatchChange(Sender: TObject);
var
  i: Integer;
begin
  if PageControlMatch.ActivePageIndex = 2 then
    lbeMatchBitmapText.Text := lbeMatchText.Text;

  if PageControlMatch.ActivePage = TabSheetActionFindSubControlSearchArea then
  begin
    if FSearchAreaSearchedTextDbgImg <> nil then
      for i := 0 to Length(FBMPTextProfiles) - 1 do
        FBMPTextProfiles[i].PreviewTextOnImage(FSearchAreaSearchedTextDbgImg);

    if Assigned(FSearchAreaScrBox) then
    begin
      FSearchAreaScrBox.Parent := TabSheetActionFindSubControlSearchArea;
      FSearchAreaScrBox.Left := lblReservedSpaceForDbgImg.Left;
    end;

    lblMouseOnDbgImg.Parent := TabSheetActionFindSubControlSearchArea;
    lblMouseOnDbgImgBB.Parent := TabSheetActionFindSubControlSearchArea;
    lblMouseOnDbgImgGG.Parent := TabSheetActionFindSubControlSearchArea;
    lblMouseOnDbgImgRR.Parent := TabSheetActionFindSubControlSearchArea;

    lblMouseOnDbgImg.Left := 296;
    lblMouseOnDbgImg.Top := 42;
    lblMouseOnDbgImgBB.Left := 296;
    lblMouseOnDbgImgBB.Top := 56;
    lblMouseOnDbgImgGG.Left := 312;
    lblMouseOnDbgImgGG.Top := 56;
    lblMouseOnDbgImgRR.Left := 328;
    lblMouseOnDbgImgRR.Top := 56;

    btnDisplaySearchAreaDebuggingImage.Parent := TabSheetActionFindSubControlSearchArea;
    spdbtnDisplaySearchAreaDbgImgMenu.Parent := TabSheetActionFindSubControlSearchArea;

    btnDisplaySearchAreaDebuggingImage.Left := lblReservedSpaceForDbgImg.Left;
    spdbtnDisplaySearchAreaDbgImgMenu.Left := btnDisplaySearchAreaDebuggingImage.Left + btnDisplaySearchAreaDebuggingImage.Width - 1;

    chkShowGridOnBMPPreview.Parent := TabSheetActionFindSubControlSearchArea;
    chkShowGridOnBMPPreview.Left := lbeSearchRectRight.Left;
    chkShowGridOnBMPPreview.Top := chkUseWholeScreenAsSearchArea.Top;
  end;

  if PageControlMatch.ActivePage = TabSheetActionFindSubControlBMPSettings then
  begin
    if Assigned(FSearchAreaScrBox) then
    begin
      FSearchAreaScrBox.Parent := TabSheetActionFindSubControlBMPSettings;
      FSearchAreaScrBox.Left := lbeFindCachedControlLeft.Left + lbeFindCachedControlLeft.Width + 10;
    end;

    lblMouseOnDbgImg.Parent := TabSheetActionFindSubControlBMPSettings;
    lblMouseOnDbgImgBB.Parent := TabSheetActionFindSubControlBMPSettings;
    lblMouseOnDbgImgGG.Parent := TabSheetActionFindSubControlBMPSettings;
    lblMouseOnDbgImgRR.Parent := TabSheetActionFindSubControlBMPSettings;

    lblMouseOnDbgImg.Left := lbeFindCachedControlLeft.Left + lbeFindCachedControlLeft.Width - 70;
    lblMouseOnDbgImg.Top := lblBMPSettingsInfo.Top;
    lblMouseOnDbgImgBB.Left := lblMouseOnDbgImg.Left;
    lblMouseOnDbgImgBB.Top := lblSearchInfo.Top;
    lblMouseOnDbgImgGG.Left := lblMouseOnDbgImgBB.Left + 16;
    lblMouseOnDbgImgGG.Top := lblSearchInfo.Top;
    lblMouseOnDbgImgRR.Left := lblMouseOnDbgImgGG.Left + 16;
    lblMouseOnDbgImgRR.Top := lblSearchInfo.Top;

    btnDisplaySearchAreaDebuggingImage.Parent := TabSheetActionFindSubControlBMPSettings;
    spdbtnDisplaySearchAreaDbgImgMenu.Parent := TabSheetActionFindSubControlBMPSettings;

    btnDisplaySearchAreaDebuggingImage.Left := lbeFindCachedControlLeft.Left + lbeFindCachedControlLeft.Width + 10;
    spdbtnDisplaySearchAreaDbgImgMenu.Left := btnDisplaySearchAreaDebuggingImage.Left + btnDisplaySearchAreaDebuggingImage.Width - 1;

    chkShowGridOnBMPPreview.Parent := TabSheetActionFindSubControlBMPSettings;
    chkShowGridOnBMPPreview.Left := 3;
    chkShowGridOnBMPPreview.Top := 101;
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
    lblFoundControlInfo.Caption := 'Control info:  handle = ' + IntToStr(Comp.Handle) +
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

  if chkAutoCopyValuesToMatchEditboxes.Checked then
  begin
    lbeMatchText.Text := lbeFoundControlText.Text;
    lbeMatchClassName.Text := lbeFoundControlClass.Text;
    DoOnTriggerOnControlsModified;
  end;
end;


procedure TfrClickerFindControl.chkAllowToFailClick(Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.btnBrowseBitmapClick(Sender: TObject);
begin
  BrowseBitmapFile;
end;


procedure TfrClickerFindControl.btnAddNewFontProfileClick(Sender: TObject);
var
  PrevIndex, n: Integer;
begin
  PrevIndex := tabctrlBMPText.TabIndex;
  n := tabctrlBMPText.Tabs.Count;
  AddFontProfile(lbeCurrentFontProfileName.Text);

  if tabctrlBMPText.Tabs.Count = n then
    Exit;

  if PrevIndex <> -1 then
  begin
    FBMPTextProfiles[n].MatchBitmapTextFGColor := FBMPTextProfiles[PrevIndex].MatchBitmapTextFGColor;
    FBMPTextProfiles[n].MatchBitmapTextBGColor := FBMPTextProfiles[PrevIndex].MatchBitmapTextBGColor;
    FBMPTextProfiles[n].FGColor := FBMPTextProfiles[PrevIndex].FGColor;
    FBMPTextProfiles[n].BGColor := FBMPTextProfiles[PrevIndex].BGColor;
    FBMPTextProfiles[n].MatchBitmapTextFontName := FBMPTextProfiles[PrevIndex].MatchBitmapTextFontName;
    FBMPTextProfiles[n].MatchBitmapTextSize := FBMPTextProfiles[PrevIndex].MatchBitmapTextSize;
    FBMPTextProfiles[n].FontQualityReplacement := FBMPTextProfiles[PrevIndex].FontQualityReplacement;
    FBMPTextProfiles[n].MatchBitmapTextFontQualityIndex := FBMPTextProfiles[PrevIndex].MatchBitmapTextFontQualityIndex;
    FBMPTextProfiles[n].Bold := FBMPTextProfiles[PrevIndex].Bold;
    FBMPTextProfiles[n].Italic := FBMPTextProfiles[PrevIndex].Italic;
    FBMPTextProfiles[n].Underline := FBMPTextProfiles[PrevIndex].Underline;
    FBMPTextProfiles[n].StrikeOut := FBMPTextProfiles[PrevIndex].StrikeOut;
    FBMPTextProfiles[n].CropLeft := FBMPTextProfiles[PrevIndex].CropLeft;
    FBMPTextProfiles[n].CropTop := FBMPTextProfiles[PrevIndex].CropTop;
    FBMPTextProfiles[n].CropRight := FBMPTextProfiles[PrevIndex].CropRight;
    FBMPTextProfiles[n].CropBottom := FBMPTextProfiles[PrevIndex].CropBottom;
  end;

  DoOnTriggerOnControlsModified;
  bitbtnUpdateFontProfile.Font.Color := clDefault;
  bitbtnUpdateFontProfile.Font.Style := [];
end;


procedure TfrClickerFindControl.bitbtnUpdateFontProfileClick(Sender: TObject);
begin
  if Trim(lbeCurrentFontProfileName.Text) = '' then
  begin
    MessageBox(Handle, 'Font profile name cannot be empty.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  if tabctrlBMPText.TabIndex = -1 then
  begin
    MessageBox(Handle, 'Please select a profile to be updated.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  FBMPTextProfiles[tabctrlBMPText.TabIndex].ProfileName := lbeCurrentFontProfileName.Text;
  tabctrlBMPText.Tabs.Strings[tabctrlBMPText.TabIndex] := lbeCurrentFontProfileName.Text;

  DoOnTriggerOnControlsModified;
  bitbtnUpdateFontProfile.Font.Color := clDefault;
  bitbtnUpdateFontProfile.Font.Style := [];
end;


procedure TfrClickerFindControl.btnRemoveFontProfileClick(Sender: TObject);
begin
  if Length(FBMPTextProfiles) < 2 then
  begin
    MessageBox(Handle, 'At least one font profile has to exist.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  if MessageBox(Handle, 'Are you sure you want to remove the selected font profile?', PChar(Caption), MB_ICONQUESTION + MB_YESNO) = IDYES then
  begin
    RemoveFontProfileByIndex(tabctrlBMPText.TabIndex);
    DoOnTriggerOnControlsModified;
    bitbtnUpdateFontProfile.Font.Color := clDefault;
    bitbtnUpdateFontProfile.Font.Style := [];
  end;
end;


procedure TfrClickerFindControl.btnAddBmpFileClick(Sender: TObject);
var
  UpperCaseFile: string;
  i: Integer;
begin
  if lbeMatchBitmapFile.Text = '' then
    if not BrowseBitmapFile then
      Exit;

  if not DoOnFileExists(lbeMatchBitmapFile.Text) then
  begin
    MessageBox(Handle, 'Specified file does not exist. Please browse a bitmap file, to fill in the editbox.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  UpperCaseFile := UpperCase(lbeMatchBitmapFile.Text);

  for i := 0 to lstMatchBitmapFiles.Count - 1 do
    if UpperCase(lstMatchBitmapFiles.Items.Strings[i]) = UpperCaseFile then
    begin
      MessageBox(Handle, 'Specified file is already in the list.', PChar(Caption), MB_ICONERROR);
      Exit;
    end;

  lstMatchBitmapFiles.Items.Add(lbeMatchBitmapFile.Text);
  vstMatchBitmapFiles.RootNodeCount := lstMatchBitmapFiles.Count;
  UpdatePreviewIcons;
  vstMatchBitmapFiles.Repaint;
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.btnUpdateBmpFileClick(Sender: TObject);
var
  i: Integer;
  Node: PVirtualNode;
  UpperCaseFile: string;
begin
  if not DoOnFileExists(lbeMatchBitmapFile.Text) then
  begin
    MessageBox(Handle, 'Specified file does not exist. Please browse a bitmap file, to fill in the editbox.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  Node := vstMatchBitmapFiles.GetFirstSelected;

  if Node = nil then
  begin
    MessageBox(Handle, 'No item selected for updating. Please select an item.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  UpperCaseFile := UpperCase(lbeMatchBitmapFile.Text);

  for i := 0 to lstMatchBitmapFiles.Count - 1 do
    if i <> Integer(Node^.Index) then
      if UpperCase(lstMatchBitmapFiles.Items.Strings[i]) = UpperCaseFile then
      begin
        MessageBox(Handle, 'Specified file is already in the list.', PChar(Caption), MB_ICONERROR);
        Exit;
      end;

  lstMatchBitmapFiles.Items.Strings[Node^.Index] := lbeMatchBitmapFile.Text;
  UpdatePreviewIcons;
  vstMatchBitmapFiles.Repaint;
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerFindControl.btnRemoveBmpFileClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstMatchBitmapFiles.GetFirstSelected;

  if Node = nil then
  begin
    MessageBox(Handle, 'No item selected for removing. Please select an item.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  if MessageBox(Handle, 'Are you sure you want to remove the selected file from list?', PChar(Caption), MB_ICONQUESTION + MB_YESNO) = ID_YES then
  begin
    lstMatchBitmapFiles.Items.Delete(Node^.Index);
    vstMatchBitmapFiles.RootNodeCount := lstMatchBitmapFiles.Count;
    UpdatePreviewIcons;
    vstMatchBitmapFiles.Repaint;
    DoOnTriggerOnControlsModified;
  end;
end;


procedure TfrClickerFindControl.btnClearClick(Sender: TObject);
begin
  if MessageBox(Handle, 'Are you sure you want to clear the list of files?', PChar(Caption), MB_ICONQUESTION + MB_YESNO) = ID_YES then
  begin
    lstMatchBitmapFiles.Items.Clear;
    vstMatchBitmapFiles.RootNodeCount := 0;
    imglstMatchBitmapFiles.Clear;
    vstMatchBitmapFiles.Repaint;
    DoOnTriggerOnControlsModified;
  end;
end;


procedure TfrClickerFindControl.btnCopyFoundValuesClick(Sender: TObject);
begin
  lbeMatchText.Text := lbeFoundControlText.Text;
  lbeMatchClassName.Text := lbeFoundControlClass.Text;
  DoOnTriggerOnControlsModified;
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
    lbeMatchText.Text := ControlText;
    lbeMatchClassName.Text := ControlClass;
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


procedure TfrClickerFindControl.DisplayDebuggingImage;
const
  CDbgImgPreviewHint: string = 'Drag to move.' + #13#10 +
                               'Ctrl-Drag, to update Left/Top offsets.' + #13#10 +
                               'Ctrl-Shift-Drag, to update Left/Top/Right/Bottom offsets.';
var
  SearchAreaControlRect: TRect;
  SearchAreaControlHandle: THandle;
  MenuItem: TMenuItem;
  i: Integer;
  ControlRight: Integer;
  ControlBottom: Integer;
begin
  try
    if FSearchAreaScrBox = nil then
    begin
      FSearchAreaScrBox := TScrollBox.Create(Self);
      FSearchAreaScrBox.Parent := TabSheetActionFindSubControlSearchArea;
      FSearchAreaScrBox.Left := lbeSearchRectBottom.Left + lbeSearchRectBottom.Width + 8;
      FSearchAreaScrBox.Top := lbeSearchRectBottom.Top + lbeSearchRectBottom.Height;
      FSearchAreaScrBox.Width := lblReservedSpaceForDbgImg.Width; // TabSheetActionFindSubControlSearchArea.Width - btnDisplaySearchAreaDebuggingImage.Left - 10; //130;
      FSearchAreaScrBox.Height := TabSheetActionFindSubControlSearchArea.Height - lbeSearchRectBottom.Top - 30; //120;
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

      if PageControlMatch.ActivePage = TabSheetActionFindSubControlBMPSettings then
      begin
        FSearchAreaScrBox.Left := lbeFindCachedControlLeft.Left + lbeFindCachedControlLeft.Width + 10;
        FSearchAreaScrBox.Parent := TabSheetActionFindSubControlBMPSettings;
      end;
    end
    else
      btnDisplaySearchAreaDebuggingImage.Caption := 'Re-display dbg img';

    SearchAreaControlRect.Left := 0;//StrToIntDef(EvaluateReplacements('$Control_Left$'), 0);
    SearchAreaControlRect.Top := 0;//StrToIntDef(EvaluateReplacements('$Control_Top$'), 0);
    SearchAreaControlRect.Width := StrToIntDef(EvaluateReplacements('$Control_Width$'), 300);
    SearchAreaControlRect.Height := StrToIntDef(EvaluateReplacements('$Control_Height$'), 300);
    SearchAreaControlHandle := StrToIntDef(EvaluateReplacements('$Control_Handle$'), 0);

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

      FSearchAreaLeftLimitLabel := TLabel.Create(Self);
      FSearchAreaTopLimitLabel := TLabel.Create(Self);
      FSearchAreaRightLimitLabel := TLabel.Create(Self);
      FSearchAreaBottomLimitLabel := TLabel.Create(Self);

      FSearchAreaLeftLimitLabel.Parent := FSearchAreaScrBox;
      FSearchAreaTopLimitLabel.Parent := FSearchAreaScrBox;
      FSearchAreaRightLimitLabel.Parent := FSearchAreaScrBox;
      FSearchAreaBottomLimitLabel.Parent := FSearchAreaScrBox;

      FSearchAreaLeftLimitLabel.AutoSize := False;
      FSearchAreaTopLimitLabel.AutoSize := False;
      FSearchAreaRightLimitLabel.AutoSize := False;
      FSearchAreaBottomLimitLabel.AutoSize := False;

      FSearchAreaLeftLimitLabel.Caption := '';
      FSearchAreaTopLimitLabel.Caption := '';
      FSearchAreaRightLimitLabel.Caption := '';
      FSearchAreaBottomLimitLabel.Caption := '';

      FSearchAreaLeftLimitLabel.Color := CLabel_Orange;
      FSearchAreaTopLimitLabel.Color := CLabel_Orange;
      FSearchAreaRightLimitLabel.Color := CLabel_LightGreen;
      FSearchAreaBottomLimitLabel.Color := CLabel_LightGreen;

      FSearchAreaLeftLimitLabel.Width := 1;
      FSearchAreaTopLimitLabel.Height := 1;
      FSearchAreaRightLimitLabel.Width := 1;
      FSearchAreaBottomLimitLabel.Height := 1;

      FSearchAreaLeftLimitLabel.BringToFront;
      FSearchAreaTopLimitLabel.BringToFront;
      FSearchAreaRightLimitLabel.BringToFront;
      FSearchAreaBottomLimitLabel.BringToFront;

      FSearchAreaLeftLimitLabel.Transparent := False;
      FSearchAreaTopLimitLabel.Transparent := False;
      FSearchAreaRightLimitLabel.Transparent := False;
      FSearchAreaBottomLimitLabel.Transparent := False;

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

      CreateSelectionLabels;

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
      MenuItem.Caption := 'Update Left and Top offsets from PreviewText image to editboxes';
      MenuItem.OnClick := MenuItemUpdateLeftAndTopOffsetsFromPreviewTextImageToEditboxes;
      MenuItem.Bitmap := imgUpdateLeftTopOffsets.Picture.Bitmap;
      FSearchAreaMenu.Items.Add(MenuItem);

      MenuItem := TMenuItem.Create(FSearchAreaMenu);
      MenuItem.Caption := 'Update Left, Top, Right and Botttom offsets from PreviewText image to editboxes';
      MenuItem.OnClick := MenuItemUpdateLeftTopRightBottomOffsetsFromPreviewTextImageToEditboxes;
      MenuItem.Bitmap := imgUpdateLeftTopRightBottomOffsets.Picture.Bitmap;
      FSearchAreaMenu.Items.Add(MenuItem);
    end
    else
      FSearchAreaDbgImgSearchedBmpMenu.Items.Clear;

    FSearchAreaScrBox.PopupMenu := FSearchAreaMenu;
    FSearchAreaControlDbgImg.PopupMenu := FSearchAreaMenu;
    FSearchAreaSearchedBmpDbgImg.PopupMenu := FSearchAreaMenu;
    FSearchAreaSearchedTextDbgImg.PopupMenu := FSearchAreaMenu;
    FSearchAreaGridImg.PopupMenu := FSearchAreaMenu;

    FSearchAreaLeftLimitLabel.Height := 1200;//FSearchAreaScrBox.Height;
    FSearchAreaTopLimitLabel.Width := 2000;//FSearchAreaScrBox.Width;
    FSearchAreaRightLimitLabel.Height := 1200;//FFSearchAreaScrBox.Height;
    FSearchAreaBottomLimitLabel.Width := 2000;//FSearchAreaScrBox.Width;

    //
    {FSearchAreaLeftLimitLabel.Anchors := FSearchAreaLeftLimitLabel.Anchors + [akBottom];
    FSearchAreaTopLimitLabel.Anchors := FSearchAreaTopLimitLabel.Anchors + [akRight];
    FSearchAreaRightLimitLabel.Anchors := FSearchAreaRightLimitLabel.Anchors + [akBottom];
    FSearchAreaBottomLimitLabel.Anchors := FSearchAreaBottomLimitLabel.Anchors + [akRight];}
    //

    FSearchAreaSearchedBmpDbgImg.Left := StrToIntDef(EvaluateReplacements(lbeSearchRectLeftOffset.Text), 20);
    FSearchAreaSearchedBmpDbgImg.Top := StrToIntDef(EvaluateReplacements(lbeSearchRectTopOffset.Text), 20);

    FSearchAreaSearchedTextDbgImg.Left := StrToIntDef(EvaluateReplacements(lbeSearchRectLeftOffset.Text), 20);
    FSearchAreaSearchedTextDbgImg.Top := StrToIntDef(EvaluateReplacements(lbeSearchRectTopOffset.Text), 20);

    FSearchAreaLeftLimitLabel.Left := FSearchAreaSearchedBmpDbgImg.Left;
    FSearchAreaTopLimitLabel.Top := FSearchAreaSearchedBmpDbgImg.Top;

    FTransparent_SearchAreaLeftLimitLabel.Left := FSearchAreaLeftLimitLabel.Left;
    FTransparent_SearchAreaTopLimitLabel.Top := FSearchAreaTopLimitLabel.Top;
    //tmrUpdateGrid.Enabled := True;

    //FSearchAreaRightLimitLabel.Left := StrToIntDef(EvaluateReplacements(lbeSearchRectRightOffset.Text), 20);    //wrong, see below
    //FSearchAreaBottomLimitLabel.Top := StrToIntDef(EvaluateReplacements(lbeSearchRectBottomOffset.Text), 20);   //wrong, see below
    if chkUseWholeScreenAsSearchArea.Checked then
    begin
      ControlRight := Screen.Width;
      ControlBottom := Screen.Height;
    end
    else
    begin
      ControlRight := StrToIntDef(EvaluateReplacements(lbeSearchRectRight.Text), 20) - StrToIntDef(EvaluateReplacements(lbeSearchRectLeft.Text), 20); //Right edge of the search area.
      ControlBottom := StrToIntDef(EvaluateReplacements(lbeSearchRectBottom.Text), 20) - StrToIntDef(EvaluateReplacements(lbeSearchRectTop.Text), 20); //Bottom edge of the search area.
    end;

    FSearchAreaRightLimitLabel.Left := ControlRight + StrToIntDef(EvaluateReplacements(lbeSearchRectRightOffset.Text), 20);   ////////// should be control right + offset (offset can be negative)
    FSearchAreaBottomLimitLabel.Top := ControlBottom + StrToIntDef(EvaluateReplacements(lbeSearchRectBottomOffset.Text), 20);    ////////// should be control bottom + offset (offset can be negative)

    UpdateTransparent_SearchAreaLimitsFromSearchAreaLimits;
    ///////////////////////

    MenuItem := TMenuItem.Create(FSearchAreaDbgImgSearchedBmpMenu);
    MenuItem.Caption := 'Load Bmp Text To Searched Area';
    MenuItem.OnClick := MenuItemLoadBmpTextToSearchedAreaClick;
    FSearchAreaDbgImgSearchedBmpMenu.Items.Add(MenuItem);

    MenuItem := TMenuItem.Create(FSearchAreaMenu);
    MenuItem.Caption := '-';
    FSearchAreaDbgImgSearchedBmpMenu.Items.Add(MenuItem);

    for i := 0 to lstMatchBitmapFiles.Items.Count - 1 do
    begin
      MenuItem := TMenuItem.Create(FSearchAreaDbgImgSearchedBmpMenu);
      MenuItem.Caption := lstMatchBitmapFiles.Items.Strings[i];
      MenuItem.OnClick := MenuItemGenericLoadBmpToSearchedAreaClick;
      FSearchAreaDbgImgSearchedBmpMenu.Items.Add(MenuItem);
    end;

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
  DisplayDebuggingImage;

  TempBmp := TBitmap.Create;
  try
    if DoOnGetExtraSearchAreaDebuggingImage(TempBmp) then
    begin
      FSearchAreaControlDbgImg.Width := TempBmp.Width;
      FSearchAreaControlDbgImg.Height := TempBmp.Height;

      WipeImage(FSearchAreaControlDbgImg, TempBmp.Width, TempBmp.Height);
      FSearchAreaControlDbgImg.Canvas.Draw(0, 0, TempBmp);
    end;
  finally
    TempBmp.Free;
  end;
end;


procedure TfrClickerFindControl.vstMatchBitmapFilesClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstMatchBitmapFiles.GetFirstSelected;
  if Node = nil then
    Exit;

  lbeMatchBitmapFile.Text := lstMatchBitmapFiles.Items.Strings[Node^.Index];
end;


procedure TfrClickerFindControl.vstMatchBitmapFilesGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  try
    ImageIndex := Node^.Index;
  except
    ImageIndex := -1;
  end;
end;


procedure TfrClickerFindControl.vstMatchBitmapFilesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: {$IFDEF FPC} string {$ELSE} WideString {$ENDIF});
begin
  try
    CellText := lstMatchBitmapFiles.Items.Strings[Node^.Index];
  except
    CellText := 'bug';
  end;
end;


procedure TfrClickerFindControl.vstMatchBitmapFilesHeaderMouseUp(Sender: TVTHeader;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
begin
  Node := vstMatchBitmapFiles.GetFirstSelected;

  if Node = nil then
    Exit;

  lbeMatchBitmapFile.Text := lstMatchBitmapFiles.Items.Strings[Node^.Index];
end;


function TfrClickerFindControl.GetSearch_BottomBottom_Ref: Integer;
begin
  Result := 0;
  if lbeSearchRectBottom.Text <> '$Control_Bottom$' then
    Result := StrToIntDef(EvaluateReplacements('$Control_Bottom$'), 0) - StrToIntDef(EvaluateReplacements(lbeSearchRectBottom.Text), 0);
end;


function TfrClickerFindControl.GetSearch_TopBottom_Ref: Integer;
begin
  Result := 0;
  if lbeSearchRectTop.Text <> '$Control_Bottom$' then
    Result := StrToIntDef(EvaluateReplacements('$Control_Bottom$'), 0) - StrToIntDef(EvaluateReplacements(lbeSearchRectTop.Text), 0);
end;


function TfrClickerFindControl.GetSearch_LeftLeft_Ref: Integer;
begin
  Result := 0;
  if lbeSearchRectLeft.Text <> '$Control_Left$' then
    Result := StrToIntDef(EvaluateReplacements('$Control_Left$'), 0) - StrToIntDef(EvaluateReplacements(lbeSearchRectLeft.Text), 0);
end;


function TfrClickerFindControl.GetSearch_RightLeft_Ref: Integer;
begin
  Result := 0;
  if lbeSearchRectRight.Text <> '$Control_Left$' then
    Result := StrToIntDef(EvaluateReplacements('$Control_Left$'), 0) - StrToIntDef(EvaluateReplacements(lbeSearchRectRight.Text), 0);
end;


function TfrClickerFindControl.GetSearch_RightRight_Ref: Integer;
begin
  Result := 0;
  if lbeSearchRectRight.Text <> '$Control_Right$' then
    Result := StrToIntDef(EvaluateReplacements('$Control_Right$'), 0) - StrToIntDef(EvaluateReplacements(lbeSearchRectRight.Text), 0);
end;


function TfrClickerFindControl.GetSearch_LeftRight_Ref: Integer;
begin
  Result := 0;
  if lbeSearchRectLeft.Text <> '$Control_Right$' then
    Result := StrToIntDef(EvaluateReplacements('$Control_Right$'), 0) - StrToIntDef(EvaluateReplacements(lbeSearchRectLeft.Text), 0);
end;


function TfrClickerFindControl.GetSearch_TopTop_Ref: Integer;
begin
  Result := 0;
  if lbeSearchRectTop.Text <> '$Control_Top$' then
    Result := StrToIntDef(EvaluateReplacements('$Control_Top$'), 0) - StrToIntDef(EvaluateReplacements(lbeSearchRectTop.Text), 0);
end;


function TfrClickerFindControl.GetSearch_BottomTop_Ref: Integer;
begin
  Result := 0;
  if lbeSearchRectBottom.Text <> '$Control_Top$' then
    Result := StrToIntDef(EvaluateReplacements('$Control_Top$'), 0) - StrToIntDef(EvaluateReplacements(lbeSearchRectBottom.Text), 0);
end;


function TfrClickerFindControl.GetControlWidthFromReplacement: Integer;
begin
  Result := StrToIntDef(EvaluateReplacements('$Control_Width$'), 0);
end;


function TfrClickerFindControl.GetControlHeightFromReplacement: Integer;
begin
  Result := StrToIntDef(EvaluateReplacements('$Control_Height$'), 0);
end;


procedure TfrClickerFindControl.UpdateSearchAreaLabelsFromOffsetEditboxes;
var
  ControlWidth, ControlHeight: Integer;
begin
  if FSearchAreaLeftLimitLabel = nil then
    Exit;   //better exit than swallow exception, because the debugger keeps catching this one, and it becomes unsable

  try
    FSearchAreaLeftLimitLabel.Left := StrToIntDef(EvaluateReplacements(lbeSearchRectLeftOffset.Text), 20) - GetSearch_LeftLeft_Ref;
    FSearchAreaTopLimitLabel.Top := StrToIntDef(EvaluateReplacements(lbeSearchRectTopOffset.Text), 20) - GetSearch_TopTop_Ref;

    ControlWidth := GetControlWidthFromReplacement;
    FSearchAreaRightLimitLabel.Left := StrToIntDef(EvaluateReplacements(lbeSearchRectRightOffset.Text), 20) - GetSearch_RightRight_Ref + ControlWidth;

    ControlHeight := GetControlHeightFromReplacement;
    FSearchAreaBottomLimitLabel.Top := StrToIntDef(EvaluateReplacements(lbeSearchRectBottomOffset.Text), 20) - GetSearch_BottomBottom_Ref + ControlHeight;

    UpdateTransparent_SearchAreaLimitsFromSearchAreaLimits;
  except
    //exception when the components are not created yet  (expected)
  end;
end;


procedure TfrClickerFindControl.UpdateSearchAreaLabelsFromKeysOnEditBoxes;
begin
  UpdateSearchAreaLabelsFromOffsetEditboxes;
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


procedure TfrClickerFindControl.UpdateSearchAreaLabelColorsFromTheirPosition;
begin
  if FSearchAreaLeftLimitLabel.Left < FSearchAreaRightLimitLabel.Left then
  begin
    FSearchAreaLeftLimitLabel.Color := CLabel_Orange;
    FSearchAreaRightLimitLabel.Color := CLabel_LightGreen;
  end
  else
  begin
    FSearchAreaLeftLimitLabel.Color := clRed;
    FSearchAreaRightLimitLabel.Color := clMaroon;
  end;

  if FSearchAreaTopLimitLabel.Top < FSearchAreaBottomLimitLabel.Top then
  begin
    FSearchAreaTopLimitLabel.Color := CLabel_Orange;
    FSearchAreaBottomLimitLabel.Color := CLabel_LightGreen;
  end
  else
  begin
    FSearchAreaTopLimitLabel.Color := clRed;
    FSearchAreaBottomLimitLabel.Color := clMaroon;
  end;
end;


procedure TfrClickerFindControl.SetLabelsFromMouseOverDbgImgPixelColor(APixelColor: TColor);
begin
  lblMouseOnDbgImgRR.Caption := IntToHex(APixelColor and $FF, 2);
  lblMouseOnDbgImgGG.Caption := IntToHex(APixelColor shr 8 and $FF, 2);
  lblMouseOnDbgImgBB.Caption := IntToHex(APixelColor shr 16 and $FF, 2);
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

    FRectangleSelecting := True;
    FSelectingXStart := X;
    FSelectingYStart := Y;
  end;
end;


procedure TfrClickerFindControl.imgSearchAreaControlDbgMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    FRectangleSelecting := False;
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

      lbeSearchRectLeftOffset.Text := IntToStr(GetSearchAreaLeftOffsetFromBmpDbgImg);
      lbeSearchRectTopOffset.Text := IntToStr(GetSearchAreaTopOffsetFromBmpDbgImg);

      if ssShift in Shift then
      begin
        FSearchAreaRightLimitLabel.Left := FSearchAreaSearchedBmpDbgImg.Left + FSearchAreaSearchedBmpDbgImg.Width;
        FSearchAreaBottomLimitLabel.Top := FSearchAreaSearchedBmpDbgImg.Top + FSearchAreaSearchedBmpDbgImg.Height;

        lbeSearchRectRightOffset.Text := IntToStr(GetSearchAreaRightOffsetFromBmpDbgImg - GetControlWidthFromReplacement);
        lbeSearchRectBottomOffset.Text := IntToStr(GetSearchAreaBottomOffsetFromBmpDbgImg - GetControlHeightFromReplacement);
      end;

      UpdateTransparent_SearchAreaLimitsFromSearchAreaLimits;
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

      lbeSearchRectLeftOffset.Text := IntToStr(GetSearchAreaLeftOffsetFromTxtDbgImg);
      lbeSearchRectTopOffset.Text := IntToStr(GetSearchAreaTopOffsetFromTxtDbgImg);

      if ssShift in Shift then
      begin
        FSearchAreaRightLimitLabel.Left := FSearchAreaSearchedTextDbgImg.Left + FSearchAreaSearchedTextDbgImg.Width;
        FSearchAreaBottomLimitLabel.Top := FSearchAreaSearchedTextDbgImg.Top + FSearchAreaSearchedTextDbgImg.Height;

        lbeSearchRectRightOffset.Text := IntToStr(GetSearchAreaRightOffsetFromTxtDbgImg - GetControlWidthFromReplacement);
        lbeSearchRectBottomOffset.Text := IntToStr(GetSearchAreaBottomOffsetFromTxtDbgImg - GetControlHeightFromReplacement);
      end;

      UpdateTransparent_SearchAreaLimitsFromSearchAreaLimits;
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
      GeneratePreviewGridContent;
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
      GeneratePreviewGridContent;
      TempBmp.Canvas.Draw(FSearchAreaGridImg.Left, FSearchAreaGridImg.Top, FSearchAreaGridImg.Picture.Bitmap);
    end;

    TempBmp.Canvas.Pen.Color := FSearchAreaLeftLimitLabel.Color;
    Line(TempBmp.Canvas, FSearchAreaLeftLimitLabel.Left, 0, FSearchAreaLeftLimitLabel.Left, TempBmp.Width - 1);

    TempBmp.Canvas.Pen.Color := FSearchAreaTopLimitLabel.Color;
    Line(TempBmp.Canvas, 0, FSearchAreaTopLimitLabel.Top, TempBmp.Width - 1, FSearchAreaTopLimitLabel.Top);

    TempBmp.Canvas.Pen.Color := FSearchAreaRightLimitLabel.Color;
    Line(TempBmp.Canvas, FSearchAreaRightLimitLabel.Left, 0, FSearchAreaRightLimitLabel.Left, TempBmp.Width - 1);

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
begin
  try
    FSearchAreaLeftLimitLabel.Left := FSearchAreaSearchedTextDbgImg.Left;
    FSearchAreaTopLimitLabel.Top := FSearchAreaSearchedTextDbgImg.Top;
    UpdateTransparent_SearchAreaLimitsFromSearchAreaLimits;
    tmrUpdateGrid.Enabled := True;

    lbeSearchRectLeftOffset.Text := IntToStr(GetSearchAreaLeftOffsetFromTxtDbgImg);
    lbeSearchRectTopOffset.Text := IntToStr(GetSearchAreaTopOffsetFromTxtDbgImg);

    UpdateSearchAreaLabelColorsFromTheirPosition;
  except
    //this will throw AV on uninitialized components or out of bounds values
  end;
end;


procedure TfrClickerFindControl.MenuItemUpdateLeftTopRightBottomOffsetsFromPreviewTextImageToEditboxes(Sender: TObject);
begin
  try
    FSearchAreaLeftLimitLabel.Left := FSearchAreaSearchedBmpDbgImg.Left;
    FSearchAreaTopLimitLabel.Top := FSearchAreaSearchedBmpDbgImg.Top;
    tmrUpdateGrid.Enabled := True;

    lbeSearchRectLeftOffset.Text := IntToStr(GetSearchAreaLeftOffsetFromBmpDbgImg);
    lbeSearchRectTopOffset.Text := IntToStr(GetSearchAreaTopOffsetFromBmpDbgImg);

    if Assigned(FSearchAreaSearchedTextDbgImg) and (FSearchAreaSearchedTextDbgImg.Width > 0) then
    begin
      FSearchAreaRightLimitLabel.Left := FSearchAreaSearchedTextDbgImg.Left + FSearchAreaSearchedTextDbgImg.Width;
      FSearchAreaBottomLimitLabel.Top := FSearchAreaSearchedTextDbgImg.Top + FSearchAreaSearchedTextDbgImg.Height;
    end
    else
    begin
      FSearchAreaRightLimitLabel.Left := FSearchAreaSearchedBmpDbgImg.Left + FSearchAreaSearchedBmpDbgImg.Width;
      FSearchAreaBottomLimitLabel.Top := FSearchAreaSearchedBmpDbgImg.Top + FSearchAreaSearchedBmpDbgImg.Height;
    end;

    lbeSearchRectRightOffset.Text := IntToStr(GetSearchAreaRightOffsetFromBmpDbgImg - GetControlWidthFromReplacement);
    lbeSearchRectBottomOffset.Text := IntToStr(GetSearchAreaBottomOffsetFromBmpDbgImg - GetControlHeightFromReplacement);

    UpdateTransparent_SearchAreaLimitsFromSearchAreaLimits;
    UpdateSearchAreaLabelColorsFromTheirPosition;
  except
    //this will throw AV on uninitialized components or out of bounds values
  end;
end;


procedure TfrClickerFindControl.MenuItemLoadBmpTextToSearchedAreaClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Length(FBMPTextProfiles) - 1 do
    FBMPTextProfiles[i].PreviewTextOnImage(FSearchAreaSearchedTextDbgImg);

  FSearchAreaSearchedTextDbgImg.Show;
end;


procedure TfrClickerFindControl.MenuItemGenericLoadBmpToSearchedAreaClick(Sender: TObject);
var
  BmpPath: string;
begin
  BmpPath := StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]);
  if DoOnFileExists(BmpPath) then
  begin
    FSearchAreaSearchedBmpDbgImg.AutoSize := True;  //should be set-reset when implementing zoom
    DoOnLoadBitmap(FSearchAreaSearchedBmpDbgImg.Picture.Bitmap, BmpPath);
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
  lbeMatchBitmapAlgorithmXMulOf.Enabled := cmbMatchBitmapTextSearchAlgorithm.ItemIndex = Ord(mbaXYMultipleAndOffsets);
  lbeMatchBitmapAlgorithmYMulOf.Enabled := lbeMatchBitmapAlgorithmXMulOf.Enabled;
  lbeMatchBitmapAlgorithmXOffset.Enabled := lbeMatchBitmapAlgorithmXMulOf.Enabled;
  lbeMatchBitmapAlgorithmyOffset.Enabled := lbeMatchBitmapAlgorithmXMulOf.Enabled;
  lbeFindCachedControlLeft.Enabled := chkSearchCachedLeftAndTopFirst.Checked;
  lbeFindCachedControlTop.Enabled := chkSearchCachedLeftAndTopFirst.Checked;

  updownXMulOf.Enabled := lbeMatchBitmapAlgorithmXMulOf.Enabled;
  updownYMulOf.Enabled := lbeMatchBitmapAlgorithmXMulOf.Enabled;
  updownXOffset.Enabled := lbeMatchBitmapAlgorithmXMulOf.Enabled;
  updownYOffset.Enabled := lbeMatchBitmapAlgorithmXMulOf.Enabled;

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


procedure TfrClickerFindControl.ClearControls;
begin
  chkMatchText.Checked := False;
  chkMatchBitmapText.Checked := False;
  chkMatchBitmapFiles.Checked := False;
  chkMatchClassName.Checked := False;
  rdgrpSearchForControlMode.ItemIndex := Ord(sfcmGenGrid);
  chkWaitForControlToGoAway.Checked := False;

  lbeMatchText.Text := '';
  lbeMatchClassName.Text := '';
  lbeMatchTextSeparator.Text := '';
  lbeMatchClassNameSeparator.Text := '';

  lbeMatchBitmapText.Text := '';

  CreateBMPTextFrames(0);
  AddFontProfile('Default');

  lstMatchBitmapFiles.Clear;

  lbeColorError.Text := '0';
  lbeAllowedColorErrorCount.Text := '0';

  cmbMatchBitmapTextSearchAlgorithm.ItemIndex := Ord(mbaBruteForce);
  lbeMatchBitmapAlgorithmXMulOf.Text := '1';
  lbeMatchBitmapAlgorithmYMulOf.Text := '1';
  lbeMatchBitmapAlgorithmXOffset.Text := '0';
  lbeMatchBitmapAlgorithmYOffset.Text := '0';
  chkSearchCachedLeftAndTopFirst.Checked := False;
  lbeFindCachedControlLeft.Text := '';
  lbeFindCachedControlTop.Text := '';

  UpdateBitmapAlgorithmSettings;

  lbeSearchRectLeft.Text := '$Control_Left$';
  lbeSearchRectTop.Text := '$Control_Top$';
  lbeSearchRectRight.Text := '$Control_Right$';
  lbeSearchRectBottom.Text := '$Control_Bottom$';
  chkUseWholeScreenAsSearchArea.Checked := True;
  SetSearchRectEnabledState;
  lbeSearchRectLeftOffset.Text := '0';
  lbeSearchRectTopOffset.Text := '0';
  lbeSearchRectRightOffset.Text := '0';
  lbeSearchRectBottomOffset.Text := '0';
end;


procedure TfrClickerFindControl.RepaintBitmapFilesVst;
begin
  vstMatchBitmapFiles.Repaint;
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
    //if NewLeft <> CurrentLabel.Left then
    //  Modified := True;

    CurrentLabel.Left := Min(NewLeft, FSearchAreaControlDbgImg.Width);
    FSearchAreaLeftLimitLabel.Left := CurrentLabel.Left;

    tmrUpdateSearchAreaOffsetEditBoxes.Enabled := True;
    UpdateSearchAreaLabelColorsFromTheirPosition;
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
    //if NewLeft <> CurrentLabel.Left then
    //  Modified := True;

    CurrentLabel.Left := Min(NewLeft, FSearchAreaControlDbgImg.Width);
    FSearchAreaRightLimitLabel.Left := CurrentLabel.Left;

    tmrUpdateSearchAreaOffsetEditBoxes.Enabled := True;
    UpdateSearchAreaLabelColorsFromTheirPosition;
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
    //if NewTop <> CurrentLabel.Top then
    //  Modified := True;

    CurrentLabel.Top := Min(NewTop, FSearchAreaControlDbgImg.Height);
    FSearchAreaTopLimitLabel.Top := CurrentLabel.Top;

    tmrUpdateSearchAreaOffsetEditBoxes.Enabled := True;
    UpdateSearchAreaLabelColorsFromTheirPosition;
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
    //if NewTop <> CurrentLabel.Top then
    //  Modified := True;

    CurrentLabel.Top := Min(NewTop, FSearchAreaControlDbgImg.Height);
    FSearchAreaBottomLimitLabel.Top := CurrentLabel.Top;

    tmrUpdateSearchAreaOffsetEditBoxes.Enabled := True;
    UpdateSearchAreaLabelColorsFromTheirPosition;
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

end.

