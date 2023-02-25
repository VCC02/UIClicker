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


unit ClickerActionsFrame;

{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  VirtualTrees, ExtCtrls, StdCtrls, ComCtrls, ImgList, Buttons, Grids, ValEdit,
  Menus, ClickerUtils, ClickerConditionEditorFrame,
  ClickerFindControlFrame, ClickerExecAppFrame, ClickerSetVarFrame,
  ClickerCallTemplateFrame, ClickerSleepFrame,
  Types, InMemFileSystem, ObjectInspectorFrame;

type
  { TfrClickerActions }

  TfrClickerActions = class(TFrame)
    imglstMatchBitmapTextProperties: TImageList;
    imglstMatchCriteriaProperties: TImageList;
    imglstInitialRectangleProperties: TImageList;
    imglstMatchBitmapAlgorithmSettingsProperties: TImageList;
    imglstActionProperties: TImageList;
    imglstCallTemplateLoopProperties: TImageList;
    imglstWindowOperationsProperties: TImageList;
    imglstSleepProperties: TImageList;
    imglstExecAppProperties: TImageList;
    imglstSetTextProperties: TImageList;
    imglstFindControlProperties: TImageList;
    imglstActions16: TImageList;
    imglstClickProperties: TImageList;
    imglstCallTemplateProperties: TImageList;
    imglstSetVarProperties: TImageList;
    lblFeatureInWork1: TLabel;
    lblMouseOnExecDbgImgBB: TLabel;
    lblMouseOnExecDbgImgGG: TLabel;
    lblMouseOnExecDbgImgRR: TLabel;
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
    pmStandardControlRefVars: TPopupMenu;
    pmStandardColorVariables: TPopupMenu;
    pnlActionConditions: TPanel;
    pmWindowOperationsEditors: TPopupMenu;
    pnlCover: TPanel;
    pnlExtra: TPanel;
    pnlvstOI: TPanel;
    N10001: TMenuItem;
    N100001: TMenuItem;
    N01: TMenuItem;
    N300001: TMenuItem;
    tmrReloadOIContent: TTimer;
    AddCustomVarRow1: TMenuItem;
    RemoveCustomVarRow1: TMenuItem;
    vallstVariables: TValueListEditor;
    scrboxDebugBmp: TScrollBox;
    imgDebugBmp: TImage;
    MenuItemSavePreviewImage: TMenuItem;
    MenuItemCopyPreviewImage: TMenuItem;
    imglstActions: TImageList;
    pmDebugVars: TPopupMenu;
    CopyDebugValuesListToClipboard1: TMenuItem;
    PasteDebugValuesListFromClipboard1: TMenuItem;
    PasteDebugValuesListFromMainExecutionList1: TMenuItem;
    lblDebugBitmapXMouseOffset: TLabel;
    lblDebugBitmapYMouseOffset: TLabel;
    MenuItemErasePreviewImage: TMenuItem;
    chkShowDebugGrid: TCheckBox;
    imgDebugGrid: TImage;
    PageControlActionExecution: TPageControl;
    TabSheetAction: TTabSheet;
    TabSheetCondition: TTabSheet;
    TabSheetDebugging: TTabSheet;
    imglstActionExecution: TImageList;
    lblAction: TLabel;
    spdbtnCommonTimeouts: TSpeedButton;
    prbTimeout: TProgressBar;
    spdbtnClear: TSpeedButton;
    chkStopOnError: TCheckBox;
    pmDebugImage: TPopupMenu;
    MenuItemSaveDebugImage: TMenuItem;
    MenuItemCopyDebugImage: TMenuItem;
    MenuItemEraseDebugImage: TMenuItem;
    lblVarReplacements: TLabel;
    lblBitmaps: TLabel;
    MenuItemRemoveExpressionPart: TMenuItem;
    MenuItemRemoveTerm: TMenuItem;
    N1: TMenuItem;
    AddVariable1: TMenuItem;
    RemoveVariable1: TMenuItem;
    procedure chkWaitForControlToGoAwayChange(Sender: TObject);

    procedure lbeFindCachedControlLeftChange(Sender: TObject);
    procedure lbeFindCachedControlTopChange(Sender: TObject);
    procedure MenuItem_SetFromControlLeftAndTopClick(Sender: TObject);
    procedure MenuItem_SetFromControlWidthAndHeightClick(Sender: TObject);
    procedure pmStandardColorVariablesPopup(Sender: TObject);

    procedure scrboxDebugBmpMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure spdbtnDisplaySearchAreaDbgImgMenuClick(Sender: TObject);
    procedure tmrReloadOIContentTimer(Sender: TObject);

    procedure CopyDebugValuesListToClipboard1Click(Sender: TObject);
    procedure PasteDebugValuesListFromClipboard1Click(Sender: TObject);
    procedure PasteDebugValuesListFromMainExecutionList1Click(Sender: TObject);

    procedure imgDebugBmpMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
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
    procedure spdbtnClearClick(Sender: TObject);
    procedure vallstVariablesValidate(Sender: TObject; ACol, ARow: Integer;
      const KeyName, KeyValue: string);
    procedure AddVariable1Click(Sender: TObject);
    procedure RemoveVariable1Click(Sender: TObject);
    procedure lbeMatchClassNameChange(Sender: TObject);
    procedure lbeMatchTextSeparatorChange(Sender: TObject);
    procedure lbeMatchClassNameSeparatorChange(Sender: TObject);

    ///////////////////////////// OI
    procedure MenuItem_SetActionTimeoutFromOI(Sender: TObject);

    procedure MenuItem_CopyTextAndClassFromPreviewWindowClick(Sender: TObject);
    procedure MenuItem_CopyTextAndClassFromWinInterpWindowClick(Sender: TObject);
    procedure MenuItem_CopyTextAndClassFromRemoteScreenWindowClick(Sender: TObject);

    procedure MenuItem_AddFilesToPropertyListClick(Sender: TObject);
    procedure MenuItem_RemoveAllFilesFromPropertyListClick(Sender: TObject);
    procedure MenuItem_RemoveFileFromPropertyListClick(Sender: TObject);
    procedure MenuItem_MoveFileUpInPropertyListClick(Sender: TObject);
    procedure MenuItem_MoveFileDownInPropertyListClick(Sender: TObject);

    procedure MenuItem_AddFontProfileToPropertyListClick(Sender: TObject);
    procedure MenuItem_RemoveFontProfileFromPropertyListClick(Sender: TObject);
    procedure MenuItem_MoveFontProfileUpInPropertyListClick(Sender: TObject);
    procedure MenuItem_MoveFontProfileDownInPropertyListClick(Sender: TObject);

    procedure MenuItemControl_EdgeRefGenericClick(Sender: TObject);
    procedure MenuItemCopyRefToClipboardClick(Sender: TObject);
    procedure MenuItemPasteRefFromClipboardClick(Sender: TObject);
  private
    { Private declarations }
    FBMPsDir: string;

    FEditingActionRec: TClkActionRec;
    FEditingAction: PClkActionRec;

    FOnControlsModified: TNotifyEvent;
    FControlsModified: Boolean;
    FPredefinedVarCount: Integer; //number of predefined variables from memVariables on form
    FDebuggingInfoAvailable: Boolean;
    FFullTemplatesDir: string;
    FSetVarContent_Vars: TStringList;
    FSetVarContent_Values: TStringList;
    FSetVarContent_EvalBefore: TStringList;
    FShowDeprecatedControls: Boolean;

    FSearchAreaScrBox: TScrollBox;
    FSearchAreaSearchedBmpDbgImg: TImage;
    FSearchAreaSearchedTextDbgImg: TImage;
    FSearchAreaDbgImgSearchedBmpMenu: TPopupMenu;

    FCurrentlyEditingActionType: Integer;  //yes integer
    FLastClickedTVTEdit: TVTEdit;
    FLastClickedEdit: TEdit;

    FPmLocalTemplates: TPopupMenu;
    FOIFrame: TfrObjectInspector;

    FOnCopyControlTextAndClassFromMainWindow: TOnCopyControlTextAndClassFromMainWindow;
    FOnGetExtraSearchAreaDebuggingImage: TOnGetExtraSearchAreaDebuggingImage;
    FOnEditCallTemplateBreakCondition: TOnEditActionCondition;

    FOnLoadBitmap: TOnLoadBitmap;
    FOnFileExists: TOnFileExists;

    FOnTemplateOpenSetMultiSelect: TOnTemplateOpenSetMultiSelect;
    FOnSetTemplateOpenDialogInitialDir: TOnSetTemplateOpenDialogInitialDir;
    FOnTemplateOpenDialogExecute: TOnTemplateOpenDialogExecute;
    FOnGetTemplateOpenDialogFileName: TOnGetTemplateOpenDialogFileName;
    FOnSetPictureOpenSetMultiSelect: TOnSetPictureOpenSetMultiSelect;
    FOnSetPictureOpenDialogInitialDir: TOnSetPictureOpenDialogInitialDir;
    FOnPictureOpenDialogExecute: TOnPictureOpenDialogExecute;
    FOnGetPictureOpenDialogFileName: TOnGetPictureOpenDialogFileName;

    //function GetListOfSetVarEntries: string;
    //procedure SetListOfSetVarEntries(Value: string);

    function GetListOfCustomVariables: string;
    procedure SetListOfCustomVariables(Value: string);

    procedure CreateRemainingUIComponents;
    procedure SetDebuggingInfoAvailable(Value: Boolean);
    procedure TriggerOnControlsModified;

    function DoOnEditCallTemplateBreakCondition(var AActionCondition: string): Boolean;
    function DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function DoOnFileExists(const AFileName: string): Boolean;

    procedure DoOnTemplateOpenSetMultiSelect;
    procedure DoOnSetTemplateOpenDialogInitialDir(AInitialDir: string);
    function DoOnTemplateOpenDialogExecute: Boolean;
    function DoOnGetTemplateOpenDialogFileName: string;
    procedure DoOnSetPictureOpenSetMultiSelect;
    procedure DoOnSetPictureOpenDialogInitialDir(AInitialDir: string);
    function DoOnPictureOpenDialogExecute: Boolean;
    function DoOnGetPictureOpenDialogFileName: string;

    procedure SetInMemFS(Value: TInMemFileSystem);

    procedure SetLabelsFromMouseOverExecDbgImgPixelColor(APixelColor: TColor);

    function GetCurrentlyEditingActionType: TClkAction;
    procedure SetCurrentlyEditingActionType(Value: TClkAction);

    procedure LocalTemplatesClick(Sender: TObject);
    procedure ClickerConditionEditorControlsModified;
    procedure OverlapGridImgOnDebugImg(ADebugAndGridBitmap: TBitmap);
    procedure CopyTextAndClassFromExternalProvider(AProviderName: string);
    procedure SetActionTimeoutToValue(AValue: Integer);

    procedure HandleOnUpdateBitmapAlgorithmSettings;
    procedure HandleOnTriggerOnControlsModified;
    function HandleOnEvaluateReplacements(s: string): string;
    procedure HandleOnCopyControlTextAndClassFromMainWindow(ACompProvider: string; out AControlText, AControlClass: string);
    function HandleOnGetExtraSearchAreaDebuggingImage(AExtraBitmap: TBitmap): Boolean;

    function HandleOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function HandleOnFileExists(const AFileName: string): Boolean;
    procedure HandleOnSetPictureOpenDialogInitialDir(AInitialDir: string);
    function HandleOnPictureOpenDialogExecute: Boolean;
    function HandleOnGetPictureOpenDialogFileName: string;

    procedure HandleOnUpdateSearchAreaLimitsInOIFromDraggingLines(ALimitLabelsToUpdate: TLimitLabels; var AOffsets: TSimpleRectString);
    procedure HandleOnUpdateTextCroppingLimitsInOIFromDraggingLines(ALimitLabelsToUpdate: TLimitLabels; var AOffsets: TSimpleRectString; AFontProfileIndex: Integer);
    function HandleOnGetDisplayedText: string;
    procedure HandleOnSetMatchTextAndClassToOI(AMatchText, AMatchClassName: string);
    function HandleOnGetFindControlOptions: PClkFindControlOptions;

    procedure HandleOnClickerExecAppFrame_OnTriggerOnControlsModified;
    procedure HandleOnClickerSetVarFrame_OnTriggerOnControlsModified;
    procedure HandleOnClickerCallTemplateFrame_OnTriggerOnControlsModified;

    ///////////////////////////// OI
    function EditFontProperties(AItemIndexDiv: Integer; var ANewItems: string): Boolean;
    procedure AddMenuItemToPopupMenu(APopupMenu: TPopupMenu; ACaption: TCaption; AHandler: TNotifyEvent;
      ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer);

    procedure FreeOIPopupMenu(Sender: TObject);

    function HandleOnOIGetCategoryCount: Integer;
    function HandleOnOIGetCategory(AIndex: Integer): string;
    function HandleOnOIGetPropertyCount(ACategoryIndex: Integer): Integer;
    function HandleOnOIGetPropertyName(ACategoryIndex, APropertyIndex: Integer): string;
    function HandleOnOIGetPropertyValue(ACategoryIndex, APropertyIndex: Integer; var AEditorType: TOIEditorType): string;
    function HandleOnOIGetListPropertyItemCount(ACategoryIndex, APropertyIndex: Integer): Integer;
    function HandleOnOIGetListPropertyItemName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
    function HandleOnOIGetListPropertyItemValue(ACategoryIndex, APropertyIndex, AItemIndex: Integer; var AEditorType: TOIEditorType): string;
    function HandleOnUIGetDataTypeName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
    function HandleOnUIGetExtraInfo(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;

    procedure HandleOnOIGetImageIndexEx(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);
    procedure HandleOnOIEditedText(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; ANewText: string);
    function HandleOnOIEditItems(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewItems: string): Boolean;

    function HandleOnOIGetColorConstsCount(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer;
    procedure HandleOnOIGetColorConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AColorItemIndex: Integer; var AColorName: string; var AColorValue: Int64);

    function HandleOnOIGetEnumConstsCount(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer;
    procedure HandleOnOIGetEnumConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEnumItemIndex: Integer; var AEnumItemName: string);

    procedure HandleOnOIPaintText(ANodeData: TNodeDataPropertyRec; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
      const TargetCanvas: TCanvas; Column: TColumnIndex; var TextType: TVSTTextType);

    procedure HandleOnTextEditorMouseDown(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    function HandleOnTextEditorMouseMove(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      Sender: TObject; Shift: TShiftState; X, Y: Integer): Boolean;

    procedure HandleOnOITextEditorKeyUp(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure HandleOnOITextEditorKeyDown(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure HandleOnOIEditorAssignMenuAndTooltip(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      Sender: TObject; var APopupMenu: TPopupMenu; var AHint: string; var AShowHint: Boolean);

    procedure HandleOnOIGetFileDialogSettings(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var AFilter, AInitDir: string);
    procedure HandleOnOIArrowEditorClick(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer);
    procedure HandleOnOIUserEditorClick(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ARepaintValue: Boolean);

    function HandleOnOIBrowseFile(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      AFilter, ADialogInitDir: string; var Handled: Boolean; AReturnMultipleFiles: Boolean = False): string;

    procedure HandleOnAfterSpinTextEditorChanging(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewValue: string);
  public
    { Public declarations }
    frClickerConditionEditor: TfrClickerConditionEditor;  //public, because it is accessed from outside :(
    frClickerFindControl: TfrClickerFindControl;
    frClickerExecApp: TfrClickerExecApp;
    frClickerSetVar: TfrClickerSetVar;
    frClickerCallTemplate: TfrClickerCallTemplate;
    frClickerSleep: TfrClickerSleep;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function EvaluateReplacements(VarName: string; Recursive: Boolean = True): string;

    procedure LoadListOfAvailableTemplates;
    procedure SetDebugVariablesFromListOfStrings(AListOfStrings: string);
    procedure UpdatePageControlActionExecutionIcons;
    procedure UpdateControlWidthHeightLabels;

    procedure ClearControls;

    property BMPsDir: string read FBMPsDir write FBMPsDir;  /////////////////////////// to be removed
    property ControlsModified: Boolean read FControlsModified write FControlsModified;
    property OnControlsModified: TNotifyEvent read FOnControlsModified write FOnControlsModified;
    property PredefinedVarCount: Integer read FPredefinedVarCount write FPredefinedVarCount;
    property DebuggingInfoAvailable: Boolean write SetDebuggingInfoAvailable;
    property FullTemplatesDir: string read FFullTemplatesDir write FFullTemplatesDir;  //no trailing backslash
    //property ListOfSetVarEntries: string read GetListOfSetVarEntries write SetListOfSetVarEntries;

    property ListOfCustomVariables: string read GetListOfCustomVariables write SetListOfCustomVariables;
    property InMemFS: TInMemFileSystem write SetInMemFS;

    property CurrentlyEditingActionType: TClkAction read GetCurrentlyEditingActionType write SetCurrentlyEditingActionType;
    property EditingAction: PClkActionRec read FEditingAction; //the pointer is not writable from outside, only the content

    property OnCopyControlTextAndClassFromMainWindow: TOnCopyControlTextAndClassFromMainWindow read FOnCopyControlTextAndClassFromMainWindow write FOnCopyControlTextAndClassFromMainWindow;
    property OnGetExtraSearchAreaDebuggingImage: TOnGetExtraSearchAreaDebuggingImage write FOnGetExtraSearchAreaDebuggingImage;
    property OnEditCallTemplateBreakCondition: TOnEditActionCondition write FOnEditCallTemplateBreakCondition;

    property OnLoadBitmap: TOnLoadBitmap write FOnLoadBitmap;
    property OnFileExists: TOnFileExists write FOnFileExists;

    property OnTemplateOpenSetMultiSelect: TOnTemplateOpenSetMultiSelect write FOnTemplateOpenSetMultiSelect;
    property OnSetTemplateOpenDialogInitialDir: TOnSetTemplateOpenDialogInitialDir write FOnSetTemplateOpenDialogInitialDir;
    property OnTemplateOpenDialogExecute: TOnTemplateOpenDialogExecute write FOnTemplateOpenDialogExecute;
    property OnGetTemplateOpenDialogFileName: TOnGetTemplateOpenDialogFileName write FOnGetTemplateOpenDialogFileName;
    property OnSetPictureOpenSetMultiSelect: TOnSetPictureOpenSetMultiSelect write FOnSetPictureOpenSetMultiSelect;
    property OnSetPictureOpenDialogInitialDir: TOnSetPictureOpenDialogInitialDir write FOnSetPictureOpenDialogInitialDir;
    property OnPictureOpenDialogExecute: TOnPictureOpenDialogExecute write FOnPictureOpenDialogExecute;
    property OnGetPictureOpenDialogFileName: TOnGetPictureOpenDialogFileName write FOnGetPictureOpenDialogFileName;
  end;


const
  CXClickPointReference: array[Low(TXClickPointReference)..High(TXClickPointReference)] of string = ('Control Left', 'Control Right', 'Control Width', 'Var', 'Screen Absolute X');
  CYClickPointReference: array[Low(TYClickPointReference)..High(TYClickPointReference)] of string = ('Control Top', 'Control Bottom', 'Control Height', 'Var', 'Screen Absolute Y');
  {$IFDEF FPC}
    ID_YES = IDYES;  //from Delphi
  {$ENDIF}


function ActionStatusStrToActionStatus(AString: string): TActionStatus;  

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.frm}
{$ENDIF}


uses
  Clipbrd, ClickerActionValues;


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
  frClickerFindControl.OnCopyControlTextAndClassFromMainWindow := HandleOnCopyControlTextAndClassFromMainWindow;
  frClickerFindControl.OnGetExtraSearchAreaDebuggingImage := HandleOnGetExtraSearchAreaDebuggingImage;
  frClickerFindControl.OnLoadBitmap := HandleOnLoadBitmap;
  frClickerFindControl.OnFileExists := HandleOnFileExists;
  frClickerFindControl.OnSetPictureOpenDialogInitialDir := HandleOnSetPictureOpenDialogInitialDir;
  frClickerFindControl.OnPictureOpenDialogExecute := HandleOnPictureOpenDialogExecute;
  frClickerFindControl.OnGetPictureOpenDialogFileName := HandleOnGetPictureOpenDialogFileName;
  frClickerFindControl.OnUpdateSearchAreaLimitsInOIFromDraggingLines := HandleOnUpdateSearchAreaLimitsInOIFromDraggingLines;
  frClickerFindControl.OnUpdateTextCroppingLimitsInOIFromDraggingLines := HandleOnUpdateTextCroppingLimitsInOIFromDraggingLines;
  frClickerFindControl.OnGetDisplayedText := HandleOnGetDisplayedText;
  frClickerFindControl.OnSetMatchTextAndClassToOI := HandleOnSetMatchTextAndClassToOI;
  frClickerFindControl.OnGetFindControlOptions := HandleOnGetFindControlOptions;
  frClickerFindControl.Visible := False;

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

  frClickerSleep := TfrClickerSleep.Create(Self);
  frClickerSleep.Parent := pnlExtra;
  frClickerSleep.Left := 3;
  frClickerSleep.Top := 3;
  frClickerSleep.Width := pnlExtra.Width - 3;
  frClickerSleep.Height := pnlExtra.Height - 3;
  frClickerSleep.Visible := False;



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
  FOIFrame.OnUIGetDataTypeName := HandleOnUIGetDataTypeName;
  FOIFrame.OnUIGetExtraInfo := HandleOnUIGetExtraInfo;
  FOIFrame.OnOIGetImageIndexEx := HandleOnOIGetImageIndexEx;
  FOIFrame.OnOIEditedText := HandleOnOIEditedText;
  FOIFrame.OnOIEditItems := HandleOnOIEditItems;
  FOIFrame.OnOIGetColorConstsCount := HandleOnOIGetColorConstsCount;
  FOIFrame.OnOIGetColorConst := HandleOnOIGetColorConst;
  FOIFrame.OnOIGetEnumConstsCount := HandleOnOIGetEnumConstsCount;
  FOIFrame.OnOIGetEnumConst := HandleOnOIGetEnumConst;
  FOIFrame.OnOIPaintText := HandleOnOIPaintText;
  FOIFrame.OnOITextEditorMouseDown := HandleOnTextEditorMouseDown;
  FOIFrame.OnOITextEditorMouseMove := HandleOnTextEditorMouseMove;
  FOIFrame.OnOITextEditorKeyUp := HandleOnOITextEditorKeyUp;
  FOIFrame.OnOITextEditorKeyDown := HandleOnOITextEditorKeyDown;
  FOIFrame.OnOIEditorAssignMenuAndTooltip := HandleOnOIEditorAssignMenuAndTooltip;
  FOIFrame.OnOIGetFileDialogSettings := HandleOnOIGetFileDialogSettings;
  FOIFrame.OnOIArrowEditorClick := HandleOnOIArrowEditorClick;
  FOIFrame.OnOIUserEditorClick := HandleOnOIUserEditorClick;
  FOIFrame.OnOIBrowseFile := HandleOnOIBrowseFile;
  FOIFrame.OnAfterSpinTextEditorChanging := HandleOnAfterSpinTextEditorChanging;

  FOIFrame.Visible := True;

  FOIFrame.ListItemsVisible := True;
  FOIFrame.DataTypeVisible := True; //False;
  FOIFrame.ExtraInfoVisible := False;
  FOIFrame.PropertyItemHeight := 22; //50;  //this should be 50 for bitmaps

  //FOIFrame.ReloadContent;  //set by ActionType combobox
  pnlvstOI.Visible := True;
end;



constructor TfrClickerActions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateRemainingUIComponents;

  FFullTemplatesDir := 'Non-existentFolder'; //ExtractFilePath(ParamStr(0)) + 'ActionTemplates'; //init value can be overridden by external wrapper

  FSearchAreaScrBox := nil;
  FSearchAreaSearchedBmpDbgImg := nil;
  FSearchAreaSearchedTextDbgImg := nil;
  FSetVarContent_Vars := TStringList.Create;
  FSetVarContent_Values := TStringList.Create;
  FSetVarContent_EvalBefore := TStringList.Create;
  FLastClickedTVTEdit := nil;
  FLastClickedEdit := nil;

  FOnCopyControlTextAndClassFromMainWindow := nil;
  FOnGetExtraSearchAreaDebuggingImage := nil;
  FOnEditCallTemplateBreakCondition := nil;

  FOnLoadBitmap := nil;
  FOnFileExists := nil;

  FOnTemplateOpenSetMultiSelect := nil;
  FOnSetTemplateOpenDialogInitialDir := nil;
  FOnTemplateOpenDialogExecute := nil;
  FOnGetTemplateOpenDialogFileName := nil;
  FOnSetPictureOpenSetMultiSelect := nil;
  FOnSetPictureOpenDialogInitialDir := nil;
  FOnPictureOpenDialogExecute := nil;
  FOnGetPictureOpenDialogFileName := nil;

  FShowDeprecatedControls := False;
  FEditingAction := @FEditingActionRec;

  PageControlActionExecution.ActivePageIndex := 0;
end;


destructor TfrClickerActions.Destroy;
begin
  FSetVarContent_Vars.Free;
  FSetVarContent_Values.Free;
  FSetVarContent_EvalBefore.Free;

  inherited Destroy;
end;


function TfrClickerActions.EvaluateReplacements(VarName: string; Recursive: Boolean = True): string;
begin
  Result := EvaluateAllReplacements(vallstVariables.Strings, VarName, Recursive);
end;


procedure TfrClickerActions.imgDebugBmpMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lblDebugBitmapXMouseOffset.Caption := 'mx: ' + IntToStr(X);
  lblDebugBitmapYMouseOffset.Caption := 'my: ' + IntToStr(Y);
  SetLabelsFromMouseOverExecDbgImgPixelColor(imgDebugBmp.Canvas.Pixels[X, Y]);
end;


procedure TfrClickerActions.rdgrpSearchForControlModeClick(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.spdbtnClearClick(Sender: TObject);
begin
  ClearControls;
end;


procedure TfrClickerActions.RemoveVariable1Click(Sender: TObject);
begin
  if vallstVariables.Selection.Top - 1 < FPredefinedVarCount - 1 then
  begin
    MessageBox(Handle, 'Predefined variables are required.', '', MB_ICONINFORMATION);
    Exit;
  end;

  if MessageBox(Handle, PChar('Remove variable?' + #13#10 + vallstVariables.Strings[vallstVariables.Selection.Top - 1]), 'Selection', MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  vallstVariables.Strings.Delete(vallstVariables.Selection.Top - 1);
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
    AStringList.Text := AListOfStrings;
    for i := 0 to AStringList.Count - 1 do
    begin
      KeyValue := AStringList.Strings[i];
      Key := Copy(KeyValue, 1, Pos('=', KeyValue) - 1);
      Value := Copy(KeyValue, Pos('=', KeyValue) + 1, MaxInt);

      vallstVariables.Values[Key] := Value;
    end;  
  finally
    AStringList.Free;
  end;
end;


procedure TfrClickerActions.CopyDebugValuesListToClipboard1Click(
  Sender: TObject);
begin
  Clipboard.AsText := vallstVariables.Strings.Text;
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
end;


procedure TfrClickerActions.chkWaitForControlToGoAwayChange(Sender: TObject);
begin
  TriggerOnControlsModified;
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


procedure TfrClickerActions.spdbtnDisplaySearchAreaDbgImgMenuClick(Sender: TObject);
var
  tp: TPoint;
begin
  GetCursorPos(tp);
  FSearchAreaDbgImgSearchedBmpMenu.PopUp(tp.X, tp.Y);
end;


procedure TfrClickerActions.lbeFindCachedControlLeftChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeFindCachedControlTopChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.MenuItem_SetFromControlLeftAndTopClick(
  Sender: TObject);
begin
  FOIFrame.CancelCurrentEditing;
  FEditingAction^.WindowOperationsOptions.NewX := EvaluateReplacements('$Control_Left$');
  FEditingAction^.WindowOperationsOptions.NewY := EvaluateReplacements('$Control_Top$');
  FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_ActionSpecific, CWindowOperations_NewX, -1);
  FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_ActionSpecific, CWindowOperations_NewY, -1);
end;


procedure TfrClickerActions.MenuItem_SetFromControlWidthAndHeightClick(
  Sender: TObject);
begin
  FOIFrame.CancelCurrentEditing;
  FEditingAction^.WindowOperationsOptions.NewWidth := EvaluateReplacements('$Control_Width$');
  FEditingAction^.WindowOperationsOptions.NewHeight := EvaluateReplacements('$Control_Height$');
  FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_ActionSpecific, CWindowOperations_NewWidth, -1);
  FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_ActionSpecific, CWindowOperations_NewHeight, -1);
end;


procedure TfrClickerActions.pmStandardColorVariablesPopup(Sender: TObject);
var
  i: Integer;
  s: string;
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
      s := EvaluateReplacements(pmStandardColorVariables.Items.Items[i].Caption);
      TextColor := HexToInt(s);

      pmStandardColorVariables.Items.Items[i].Bitmap.Canvas.Pen.Color := 1;  // > 0
      pmStandardColorVariables.Items.Items[i].Bitmap.Canvas.Brush.Color := TextColor;
      pmStandardColorVariables.Items.Items[i].Bitmap.Canvas.Rectangle(0, 0, 16, 16);
    end;
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


procedure TfrClickerActions.LocalTemplatesClick(Sender: TObject);
var
  Fnm: string;
begin
  Fnm := StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]);
  FEditingAction^.CallTemplateOptions.TemplateFileName := Fnm;
  FOIFrame.Repaint;   //ideally, RepaintNodeByLevel
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.LoadListOfAvailableTemplates;
var
  AvailableTemplates: TStringList;
  ASearchRec: TSearchRec;
  SearchResult: Integer;
  Dir: string;
  //SelectedTemplate: string;
  TempMenuItem: TMenuItem;
  i: Integer;
begin
  AvailableTemplates := TStringList.Create;
  try
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

    for i := 0 to AvailableTemplates.Count - 1 do
    begin
      TempMenuItem := TMenuItem.Create(Self);
      TempMenuItem.Caption := AvailableTemplates.Strings[i];
      TempMenuItem.OnClick := LocalTemplatesClick;
      FPmLocalTemplates.Items.Add(TempMenuItem);
    end;
  finally
    AvailableTemplates.Free;
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
end;


function TfrClickerActions.HandleOnEvaluateReplacements(s: string): string;
begin
  Result := EvaluateReplacements(s);
end;


procedure TfrClickerActions.AddVariable1Click(Sender: TObject);
begin
  vallstVariables.Strings.Add('');
end;


procedure TfrClickerActions.TriggerOnControlsModified;
begin
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


procedure TfrClickerActions.SetInMemFS(Value: TInMemFileSystem);
begin
  frClickerFindControl.InMemFS := Value;
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
  if llLeft in ALimitLabelsToUpdate then
  begin
    FEditingAction^.FindControlOptions.InitialRectangle.LeftOffset := AOffsets.Left;
    FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindControl_InitialRectangle_PropIndex, CFindControl_InitialRectangle_LeftOffset_PropItemIndex);
  end;

  if llTop in ALimitLabelsToUpdate then
  begin
    FEditingAction^.FindControlOptions.InitialRectangle.TopOffset := AOffsets.Top;
    FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindControl_InitialRectangle_PropIndex, CFindControl_InitialRectangle_TopOffset_PropItemIndex);
  end;

  if llRight in ALimitLabelsToUpdate then
  begin
    FEditingAction^.FindControlOptions.InitialRectangle.RightOffset := AOffsets.Right;
    FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindControl_InitialRectangle_PropIndex, CFindControl_InitialRectangle_RightOffset_PropItemIndex);
  end;

  if llBottom in ALimitLabelsToUpdate then
  begin
    FEditingAction^.FindControlOptions.InitialRectangle.BottomOffset := AOffsets.Bottom;
    FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindControl_InitialRectangle_PropIndex, CFindControl_InitialRectangle_BottomOffset_PropItemIndex);
  end;
end;


procedure TfrClickerActions.HandleOnUpdateTextCroppingLimitsInOIFromDraggingLines(ALimitLabelsToUpdate: TLimitLabels; var AOffsets: TSimpleRectString; AFontProfileIndex: Integer);
var
  UpdatingNodeIndex: Integer;
begin
  if llLeft in ALimitLabelsToUpdate then
  begin
    UpdatingNodeIndex := AFontProfileIndex * CPropCount_FindControlMatchBitmapText + CFindControl_MatchBitmapText_CropLeft;
    FEditingAction^.FindControlOptions.MatchBitmapText[AFontProfileIndex].CropLeft := AOffsets.Left;
    FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindControl_MatchBitmapText_PropIndex, UpdatingNodeIndex);
  end;

  if llTop in ALimitLabelsToUpdate then
  begin
    UpdatingNodeIndex := AFontProfileIndex * CPropCount_FindControlMatchBitmapText + CFindControl_MatchBitmapText_CropTop;
    FEditingAction^.FindControlOptions.MatchBitmapText[AFontProfileIndex].CropTop := AOffsets.Top;
    FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindControl_MatchBitmapText_PropIndex, UpdatingNodeIndex);
  end;

  if llRight in ALimitLabelsToUpdate then
  begin
    UpdatingNodeIndex := AFontProfileIndex * CPropCount_FindControlMatchBitmapText + CFindControl_MatchBitmapText_CropRight;
    FEditingAction^.FindControlOptions.MatchBitmapText[AFontProfileIndex].CropRight := AOffsets.Right;
    FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindControl_MatchBitmapText_PropIndex, UpdatingNodeIndex);
  end;

  if llBottom in ALimitLabelsToUpdate then
  begin
    UpdatingNodeIndex := AFontProfileIndex * CPropCount_FindControlMatchBitmapText + CFindControl_MatchBitmapText_CropBottom;
    FEditingAction^.FindControlOptions.MatchBitmapText[AFontProfileIndex].CropBottom := AOffsets.Bottom;
    FOIFrame.RepaintNodeByLevel(CPropertyItemLevel, CCategory_ActionSpecific, CFindControl_MatchBitmapText_PropIndex, UpdatingNodeIndex);
  end;
end;


function TfrClickerActions.HandleOnGetDisplayedText: string;
begin
  Result := FEditingAction^.FindControlOptions.MatchText;
end;


procedure TfrClickerActions.HandleOnSetMatchTextAndClassToOI(AMatchText, AMatchClassName: string);
begin
  FEditingAction^.FindControlOptions.MatchText := AMatchText;
  FEditingAction^.FindControlOptions.MatchClassName := AMatchClassName;
  FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_ActionSpecific, CFindControl_MatchText_PropIndex, -1, True);
  FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_ActionSpecific, CFindControl_MatchClassName_PropIndex, -1, True);
end;


function TfrClickerActions.HandleOnGetFindControlOptions: PClkFindControlOptions;
begin
  Result := @FEditingAction^.FindControlOptions;
end;


procedure TfrClickerActions.HandleOnClickerExecAppFrame_OnTriggerOnControlsModified;
begin
  FEditingAction.ExecAppOptions.ListOfParams := frClickerExecApp.memExecAppParams.Lines.Text;
  FOIFrame.RepaintNodeByLevel(CPropertyLevel, CCategory_ActionSpecific, CExecApp_ListOfParams_PropIndex, -1);
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.HandleOnClickerSetVarFrame_OnTriggerOnControlsModified;
begin
  FEditingAction.SetVarOptions := frClickerSetVar.GetListOfSetVars;
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.HandleOnClickerCallTemplateFrame_OnTriggerOnControlsModified;
begin
  FEditingAction.CallTemplateOptions.ListOfCustomVarsAndValues := frClickerCallTemplate.GetListOfCustomVariables;
  TriggerOnControlsModified;
end;


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


function TfrClickerActions.DoOnFileExists(const AFileName: string): Boolean;
begin
  if not Assigned(FOnFileExists) then
    raise Exception.Create('OnFileExists is not assigned.')
  else
    Result := FOnFileExists(AFileName);
end;


procedure TfrClickerActions.DoOnTemplateOpenSetMultiSelect;
begin
  if not Assigned(FOnTemplateOpenSetMultiSelect) then
    raise Exception.Create('OnTemplateOpenSetMultiSelect is not assigned.')
  else
    FOnTemplateOpenSetMultiSelect;
end;


procedure TfrClickerActions.DoOnSetTemplateOpenDialogInitialDir(AInitialDir: string);
begin
  if not Assigned(FOnSetTemplateOpenDialogInitialDir) then
    raise Exception.Create('OnSetTemplateOpenDialogInitialDir is not assigned.')
  else
    FOnSetTemplateOpenDialogInitialDir(AInitialDir);
end;


function TfrClickerActions.DoOnTemplateOpenDialogExecute: Boolean;
begin
  if not Assigned(FOnTemplateOpenDialogExecute) then
    raise Exception.Create('OnTemplateOpenDialogExecute is not assigned.')
  else
    Result := FOnTemplateOpenDialogExecute;
end;


function TfrClickerActions.DoOnGetTemplateOpenDialogFileName: string;
begin
  if not Assigned(FOnGetTemplateOpenDialogFileName) then
    raise Exception.Create('OnGetTemplateOpenDialogFileName is not assigned.')
  else
    Result := FOnGetTemplateOpenDialogFileName;
end;


procedure TfrClickerActions.DoOnSetPictureOpenSetMultiSelect;
begin
  if not Assigned(FOnSetPictureOpenSetMultiSelect) then
    raise Exception.Create('OnSetPictureOpenSetMultiSelect is not assigned.')
  else
    FOnSetPictureOpenSetMultiSelect;
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


//////////////////////////// OI
type
  TOIMenuItemData = record
    OwnerMenu: TPopupMenu;
    NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
    MenuItemCaption: string;
  end;
  POIMenuItemData = ^TOIMenuItemData;


function TfrClickerActions.GetCurrentlyEditingActionType: TClkAction;
begin
  Result := TClkAction(FCurrentlyEditingActionType);
end;


procedure TfrClickerActions.SetCurrentlyEditingActionType(Value: TClkAction);
begin
  FCurrentlyEditingActionType := Ord(Value);
  FOIFrame.ReloadContent;
  pnlvstOI.Visible := True;

  pnlCover.Hide;

  case Value of
    acExecApp:
    begin
      frClickerExecApp.Show;
      frClickerExecApp.BringToFront;
      frClickerFindControl.Hide;
      frClickerSetVar.Hide;
      frClickerCallTemplate.Hide;
      frClickerSleep.Hide;
    end;

    acFindControl, acFindSubControl:
    begin
      frClickerExecApp.Hide;
      frClickerFindControl.Show;
      frClickerFindControl.BringToFront;
      frClickerSetVar.Hide;
      frClickerCallTemplate.Hide;
      frClickerSleep.Hide;
    end;

    acSetVar:
    begin
      frClickerExecApp.Hide;
      frClickerFindControl.Hide;
      frClickerSetVar.Show;
      frClickerSetVar.BringToFront;
      frClickerCallTemplate.Hide;
      frClickerSleep.Hide;
    end;

    acCallTemplate:
    begin
      frClickerExecApp.Hide;
      frClickerFindControl.Hide;
      frClickerSetVar.Hide;
      frClickerCallTemplate.Show;
      frClickerCallTemplate.BringToFront;
      frClickerSleep.Hide;
    end;

    acSleep:
    begin
      frClickerExecApp.Hide;
      frClickerFindControl.Hide;
      frClickerSetVar.Hide;
      frClickerCallTemplate.Hide;
      frClickerSleep.Show;
      frClickerSleep.BringToFront;
    end;

    else
    begin
      frClickerExecApp.Hide;
      frClickerFindControl.Hide;
      frClickerSetVar.Hide;
      frClickerCallTemplate.Hide;
      frClickerSleep.Hide;

      pnlCover.Left := 0;
      pnlCover.Top := 0;
      pnlCover.Width := pnlExtra.Width;
      pnlCover.Height := pnlExtra.Height;
      pnlCover.Show;
      pnlCover.BringToFront;
    end;
  end;
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
    FEditingAction^.FindControlOptions.MatchText := ControlText;
    FEditingAction^.FindControlOptions.MatchClassName := ControlClass;

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
    try
      ValueStr := StringReplace(MenuData^.MenuItemCaption, '&', '', [rfReplaceAll]);
      SetActionTimeoutToValue(StrToIntDef(ValueStr, 0));
    finally
      MenuData^.OwnerMenu.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.FreeOIPopupMenu(Sender: TObject);
var
  MenuData: POIMenuItemData;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    MenuData^.OwnerMenu.Free;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_CopyTextAndClassFromPreviewWindowClick(Sender: TObject);
begin
  CopyTextAndClassFromExternalProvider(CPreviewWindow);
  FreeOIPopupMenu(Sender);
end;


procedure TfrClickerActions.MenuItem_CopyTextAndClassFromWinInterpWindowClick(Sender: TObject);
begin
  CopyTextAndClassFromExternalProvider(CWinInterpWindow);
  FreeOIPopupMenu(Sender);
end;


procedure TfrClickerActions.MenuItem_CopyTextAndClassFromRemoteScreenWindowClick(Sender: TObject);
begin
  CopyTextAndClassFromExternalProvider(CRemoteScreenWindow);
  FreeOIPopupMenu(Sender);
end;


procedure TfrClickerActions.MenuItem_AddFilesToPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ListOfFiles: TStringList;
begin
  DoOnSetPictureOpenSetMultiSelect;
  if not DoOnPictureOpenDialogExecute then
    Exit;

  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    try
      ListOfFiles := TStringList.Create;
      try
        ListOfFiles.Text := FEditingAction^.FindControlOptions.MatchBitmapFiles;
        ListOfFiles.Text := ListOfFiles.Text + DoOnGetPictureOpenDialogFileName;
        FEditingAction^.FindControlOptions.MatchBitmapFiles := ListOfFiles.Text;

        FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
        TriggerOnControlsModified;
      finally
        ListOfFiles.Free;
      end;
    finally
      MenuData^.OwnerMenu.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_RemoveAllFilesFromPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
begin
  if MessageBox(Handle, 'Are you sure you want to remove all files from this list?', PChar(Application.MainForm.Caption), MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    try
      FEditingAction^.FindControlOptions.MatchBitmapFiles := '';
      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
      TriggerOnControlsModified;
    finally
      MenuData^.OwnerMenu.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_RemoveFileFromPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ListOfFiles: TStringList;
begin
  if MessageBox(Handle, 'Are you sure you want to remove this file from list?', PChar(Application.MainForm.Caption), MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    try
      ListOfFiles := TStringList.Create;
      try
        ListOfFiles.Text := FEditingAction^.FindControlOptions.MatchBitmapFiles;
        ListOfFiles.Delete(MenuData^.PropertyItemIndex);
        FEditingAction^.FindControlOptions.MatchBitmapFiles := ListOfFiles.Text;

        FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
        TriggerOnControlsModified;
      finally
        ListOfFiles.Free;
      end;
    finally
      MenuData^.OwnerMenu.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_MoveFileUpInPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ListOfFiles: TStringList;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    try
      ListOfFiles := TStringList.Create;
      try
        if MenuData^.PropertyItemIndex <= 0 then
          Exit;

        ListOfFiles.Text := FEditingAction^.FindControlOptions.MatchBitmapFiles;
        ListOfFiles.Move(MenuData^.PropertyItemIndex, MenuData^.PropertyItemIndex - 1);
        FEditingAction^.FindControlOptions.MatchBitmapFiles := ListOfFiles.Text;

        FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
        TriggerOnControlsModified;
      finally
        ListOfFiles.Free;
      end;
    finally
      MenuData^.OwnerMenu.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_MoveFileDownInPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ListOfFiles: TStringList;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    try
      ListOfFiles := TStringList.Create;
      try
        ListOfFiles.Text := FEditingAction^.FindControlOptions.MatchBitmapFiles;
        if MenuData^.PropertyItemIndex >= ListOfFiles.Count - 1 then
          Exit;

        ListOfFiles.Move(MenuData^.PropertyItemIndex, MenuData^.PropertyItemIndex + 1);
        FEditingAction^.FindControlOptions.MatchBitmapFiles := ListOfFiles.Text;

        FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
        TriggerOnControlsModified;
      finally
        ListOfFiles.Free;
      end;
    finally
      MenuData^.OwnerMenu.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItem_AddFontProfileToPropertyListClick(Sender: TObject);
var
  MenuData: POIMenuItemData;
  n: Integer;

  function GetUniqueProfileName: string;
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
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    try
      n := Length(FEditingAction^.FindControlOptions.MatchBitmapText);
      SetLength(FEditingAction^.FindControlOptions.MatchBitmapText, n + 1);

      FEditingAction^.FindControlOptions.MatchBitmapText[n].ForegroundColor := '$Color_Window$';
      FEditingAction^.FindControlOptions.MatchBitmapText[n].BackgroundColor := '$Color_Highlight$';
      FEditingAction^.FindControlOptions.MatchBitmapText[n].FontName := 'Tahoma';
      FEditingAction^.FindControlOptions.MatchBitmapText[n].FontSize := 8;
      FEditingAction^.FindControlOptions.MatchBitmapText[n].FontQualityReplacement := '';
      FEditingAction^.FindControlOptions.MatchBitmapText[n].FontQuality := fqNonAntialiased;
      FEditingAction^.FindControlOptions.MatchBitmapText[n].FontQualityUsesReplacement := False;
      FEditingAction^.FindControlOptions.MatchBitmapText[n].Bold := False;
      FEditingAction^.FindControlOptions.MatchBitmapText[n].Italic := False;
      FEditingAction^.FindControlOptions.MatchBitmapText[n].Underline := False;
      FEditingAction^.FindControlOptions.MatchBitmapText[n].StrikeOut := False;
      FEditingAction^.FindControlOptions.MatchBitmapText[n].CropLeft := '0';
      FEditingAction^.FindControlOptions.MatchBitmapText[n].CropTop := '0';
      FEditingAction^.FindControlOptions.MatchBitmapText[n].CropRight := '0';
      FEditingAction^.FindControlOptions.MatchBitmapText[n].CropBottom := '0';
      FEditingAction^.FindControlOptions.MatchBitmapText[n].ProfileName := GetUniqueProfileName;

      frClickerFindControl.AddNewFontProfile(FEditingAction^.FindControlOptions.MatchBitmapText[n]);

      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
      TriggerOnControlsModified;
    finally
      MenuData^.OwnerMenu.Free;
    end;
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
    try
      n := Length(FEditingAction^.FindControlOptions.MatchBitmapText);

      if MessageBox(Handle,
                    PChar('Are you sure you want to remove font profile: ' + FEditingAction^.FindControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex].ProfileName),
                    PChar(Application.Title),
                    MB_ICONQUESTION + MB_YESNO) = IDNO then
        Exit;

      for i := MenuData^.PropertyItemIndex to n - 2 do
        FEditingAction^.FindControlOptions.MatchBitmapText[i] := FEditingAction^.FindControlOptions.MatchBitmapText[i + 1];

      SetLength(FEditingAction^.FindControlOptions.MatchBitmapText, n - 1);

      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);

      frClickerFindControl.RemoveFontProfileByIndex(MenuData^.PropertyItemIndex);
      TriggerOnControlsModified;
    finally
      MenuData^.OwnerMenu.Free;
    end;
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
    try
      if MenuData^.PropertyItemIndex <= 0 then
        Exit;

      TempProfile := FEditingAction^.FindControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex];
      FEditingAction^.FindControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex] :=
        FEditingAction^.FindControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex - 1];

      FEditingAction^.FindControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex - 1] := TempProfile;

      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
      TriggerOnControlsModified;
    finally
      MenuData^.OwnerMenu.Free;
    end;
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
    try
      if MenuData^.PropertyItemIndex >= Length(FEditingAction^.FindControlOptions.MatchBitmapText) - 1 then
        Exit;

      TempProfile := FEditingAction^.FindControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex];
      FEditingAction^.FindControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex] :=
        FEditingAction^.FindControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex + 1];

      FEditingAction^.FindControlOptions.MatchBitmapText[MenuData^.PropertyItemIndex + 1] := TempProfile;

      FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex);
      TriggerOnControlsModified;
    finally
      MenuData^.OwnerMenu.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerActions.MenuItemControl_EdgeRefGenericClick(Sender: TObject);
begin
  try
    if Assigned(FLastClickedTVTEdit) then
    begin
      FLastClickedTVTEdit.Text := StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]);
      FOIFrame.EditingText := FLastClickedTVTEdit.Text;
    end;

    if Assigned(FLastClickedEdit) then
    begin
      FLastClickedEdit.Text := StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]);
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


function TfrClickerActions.HandleOnOIGetCategoryCount: Integer;
begin
  Result := CCategoryCount;
end;


function TfrClickerActions.HandleOnOIGetCategory(AIndex: Integer): string;
begin
  Result := CCategories[AIndex];
end;


function TfrClickerActions.HandleOnOIGetPropertyCount(ACategoryIndex: Integer): Integer;
var
  EditingActionType: Integer;
begin
  case ACategoryIndex of
    CCategory_Common:
      Result := CPropCount_Common;

    CCategory_ActionSpecific:
    begin
      EditingActionType := Integer(CurrentlyEditingActionType);
      if EditingActionType = CClkUnsetAction then
        Result := 0 //no action is selected
      else
        Result := CMainPropCounts[EditingActionType];
    end;

    else
      Result := 0;
  end;

  if Result > 500 then   //This will hide some bugs (for now). At least it will prevent crashes and memory overuse.
    Result := 500;

  if Result < 0 then
    Result := 0;
end;


function TfrClickerActions.HandleOnOIGetPropertyName(ACategoryIndex, APropertyIndex: Integer): string;
var
  EditingActionType: Integer;
begin
  case ACategoryIndex of
    CCategory_Common:
      Result := CCommonProperties[APropertyIndex].Name;

    CCategory_ActionSpecific:
    begin
      EditingActionType := Integer(CurrentlyEditingActionType);
      if EditingActionType = CClkUnsetAction then
        Result := '?'
      else
        Result := CMainProperties[EditingActionType]^[APropertyIndex].Name;

      if CurrentlyEditingActionType in [acFindControl, acFindSubControl] then
        if APropertyIndex = CFindControl_MatchBitmapText_PropIndex then
          Result := Result + ' [0..' + IntToStr(Length(FEditingAction^.FindControlOptions.MatchBitmapText) - 1) + ']';
    end;

    else
      Result := '???';
  end;
end;


function TfrClickerActions.HandleOnOIGetPropertyValue(ACategoryIndex, APropertyIndex: Integer; var AEditorType: TOIEditorType): string;
var
  EditingActionType: Integer;
  PropDef: TOIPropDef;
begin
  PropDef.EditorType := etNone;
  Result := '';

  case ACategoryIndex of
    CCategory_Common:
    begin
      PropDef := CCommonProperties[APropertyIndex];
      Result := GetActionValueStr_Action(FEditingAction, APropertyIndex);
    end;

    CCategory_ActionSpecific:
    begin
      EditingActionType := Integer(CurrentlyEditingActionType);
      if EditingActionType = CClkUnsetAction then
        PropDef.Name := '?'
      else
      begin
        PropDef := CMainProperties[EditingActionType]^[APropertyIndex];
        Result := CMainGetActionValueStrFunctions[CurrentlyEditingActionType](FEditingAction, APropertyIndex);
      end;
    end;

    else
      PropDef.Name := '???';
  end;

  AEditorType := PropDef.EditorType;
end;


function TfrClickerActions.HandleOnOIGetListPropertyItemCount(ACategoryIndex, APropertyIndex: Integer): Integer;
var
  EditingActionType: Integer;
  TempStringList: TStringList;
begin
  Result := 0;
  if ACategoryIndex = CCategory_Common then
    Exit; //no subproperties here

  EditingActionType := Integer(CurrentlyEditingActionType);
  if EditingActionType = CClkUnsetAction then
    Exit;

  case EditingActionType of
    Ord(acFindControl), Ord(acFindSubControl):
    begin
      case APropertyIndex of
        CFindControl_MatchCriteria_PropIndex:
          Result := CPropCount_FindControlMatchCriteria;

        CFindControl_MatchBitmapText_PropIndex:
          Result := CPropCount_FindControlMatchBitmapText * Length(FEditingAction^.FindControlOptions.MatchBitmapText);  //frClickerFindControl.GetBMPTextFontProfilesCount;

        CFindControl_MatchBitmapFiles_PropIndex:
        begin
          TempStringList := TStringList.Create;
          try
            TempStringList.Text := FEditingAction^.FindControlOptions.MatchBitmapFiles;
            Result := TempStringList.Count;
          finally
            TempStringList.Free;
          end;
        end;

        CFindControl_MatchBitmapAlgorithmSettings_PropIndex:
          Result := CPropCount_FindControlMatchBitmapAlgorithmSettings;

        CFindControl_InitialRectangle_PropIndex:
          Result := CPropCount_FindControlInitialRectangle;

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


function TfrClickerActions.HandleOnOIGetListPropertyItemName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
var
  EditingActionType: Integer;
  ItemIndexMod, ItemIndexDiv: Integer;
begin
  Result := '';
  if ACategoryIndex = CCategory_Common then
    Exit;

  EditingActionType := Integer(CurrentlyEditingActionType);
  if EditingActionType = CClkUnsetAction then
    Exit;

  case EditingActionType of
    Ord(acFindControl), Ord(acFindSubControl):
    begin
      case APropertyIndex of
        CFindControl_MatchCriteria_PropIndex:
          Result := CFindControl_MatchCriteriaProperties[AItemIndex].Name;

        CFindControl_MatchBitmapText_PropIndex:
        begin
          ItemIndexMod := AItemIndex mod CPropCount_FindControlMatchBitmapText;
          ItemIndexDiv := AItemIndex div CPropCount_FindControlMatchBitmapText;
          Result := '[' + IntToStr(ItemIndexDiv) + ']  ' + CFindControl_MatchBitmapTextProperties[ItemIndexMod].Name;
        end;

        CFindControl_MatchBitmapFiles_PropIndex:
          Result := 'File[' + IntToStr(AItemIndex) + ']';

        CFindControl_MatchBitmapAlgorithmSettings_PropIndex:
          Result := CFindControl_MatchBitmapAlgorithmSettingsProperties[AItemIndex].Name;

        CFindControl_InitialRectangle_PropIndex:
          Result := CFindControl_InitialRectangleProperties[AItemIndex].Name;

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


function TfrClickerActions.HandleOnOIGetListPropertyItemValue(ACategoryIndex, APropertyIndex, AItemIndex: Integer; var AEditorType: TOIEditorType): string;
var
  EditingActionType: Integer;
  PropDef: TOIPropDef;
  TempStringList: TStringList;
begin
  Result := '';
  AEditorType := etNone;

  if ACategoryIndex = CCategory_Common then
    Exit;

  EditingActionType := Integer(CurrentlyEditingActionType);
  if EditingActionType = CClkUnsetAction then
    Exit;

  case EditingActionType of
    Ord(acFindControl), Ord(acFindSubControl):
    begin
      case APropertyIndex of
        CFindControl_MatchCriteria_PropIndex:
          PropDef := CFindControl_MatchCriteriaProperties[AItemIndex];

        CFindControl_MatchBitmapText_PropIndex:
          PropDef := CFindControl_MatchBitmapTextProperties[AItemIndex mod CPropCount_FindControlMatchBitmapText];

        CFindControl_MatchBitmapFiles_PropIndex:
        begin
          TempStringList := TStringList.Create;
          try
            TempStringList.Text := FEditingAction^.FindControlOptions.MatchBitmapFiles;
            try
              Result := TempStringList.Strings[AItemIndex];
            except
              on E: Exception do
                Result := E.Message + '  ' + IntToStr(AItemIndex) + '   ' + IntToStr(TempStringList.Count - 1);
            end;
          finally
            TempStringList.Free;
          end;

          AEditorType := etFilePathWithArrow;
          Exit;
        end;

        CFindControl_MatchBitmapAlgorithmSettings_PropIndex:
          PropDef := CFindControl_MatchBitmapAlgorithmSettingsProperties[AItemIndex];

        CFindControl_InitialRectangle_PropIndex:
          PropDef := CFindControl_InitialRectangleProperties[AItemIndex];

        else
          ;
      end;

      Result := CFindControlGetActionValueStrFunctions[APropertyIndex](FEditingAction, AItemIndex);
    end;

    Ord(acCallTemplate):
    begin
      case APropertyIndex of
        CCallTemplate_CallTemplateLoop_PropIndex:
          PropDef := CCallTemplate_CallTemplateLoopProperties[AItemIndex];

        else
          ;
      end;

      Result := CCallTemplateGetActionValueStrFunctions[APropertyIndex](FEditingAction, AItemIndex);
    end;
  end;   //case EditingActionType

  AEditorType := PropDef.EditorType;
end;


function TfrClickerActions.HandleOnUIGetDataTypeName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
begin
  Result := 'data';
end;


function TfrClickerActions.HandleOnUIGetExtraInfo(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
begin
  Result := 'extra';
end;


procedure TfrClickerActions.HandleOnOIGetImageIndexEx(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);
var
  EditingActionType: Integer;
  ItemIndexMod: Integer;
begin
  EditingActionType := Integer(CurrentlyEditingActionType);

  case ACategoryIndex of
    CCategory_Common:
      if Column = 0 then
      begin
        ImageList := imglstActionProperties;
        ImageIndex := APropertyIndex;
      end;

    CCategory_ActionSpecific:
    begin
      if EditingActionType = CClkUnsetAction then
        Exit;

      if Column = 0 then
      begin
        case ANodeLevel of
          CCategoryLevel:
          begin
            ImageList := imglstActions16;
            ImageIndex := EditingActionType;
          end;

          CPropertyLevel:
          begin
            ImageIndex := APropertyIndex;

            case EditingActionType of
              Ord(acClick):
                ImageList := imglstClickProperties;

              Ord(acExecApp):
                ImageList := imglstExecAppProperties;

              Ord(acFindControl), Ord(acFindSubControl):
                ImageList := imglstFindControlProperties;

              Ord(acSetControlText):
                ImageList := imglstSetTextProperties;

              Ord(acCallTemplate):
                ImageList := imglstCallTemplateProperties;

              Ord(acSleep):
                ImageList := imglstSleepProperties;

              Ord(acSetVar):
                ImageList := imglstSetVarProperties;

              Ord(acWindowOperations):
                ImageList := imglstWindowOperationsProperties;
            end;   //case
          end;

          CPropertyItemLevel:
          begin
            ImageIndex := AItemIndex;

            case EditingActionType of
              Ord(acFindControl), Ord(acFindSubControl):
              begin
                case APropertyIndex of
                  CFindControl_MatchCriteria_PropIndex:
                    ImageList := imglstMatchCriteriaProperties;

                  CFindControl_MatchBitmapText_PropIndex:
                  begin
                    ItemIndexMod := AItemIndex mod CPropCount_FindControlMatchBitmapText;
                    ImageIndex := ItemIndexMod;
                    ImageList := imglstMatchBitmapTextProperties;
                  end;

                  CFindControl_MatchBitmapAlgorithmSettings_PropIndex:
                    ImageList := imglstMatchBitmapAlgorithmSettingsProperties;

                  CFindControl_InitialRectangle_PropIndex:
                    ImageList := imglstInitialRectangleProperties;
                end;
              end;

              Ord(acCallTemplate):
                ImageList := imglstCallTemplateLoopProperties;
            end;
          end;
        end; //case
      end; // Column = 0
    end;
  end;
end;


procedure TfrClickerActions.HandleOnOIEditedText(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; ANewText: string);
var
  EditingActionType: Integer;
  TempStringList: TStringList;
  ItemIndexMod, ItemIndexDiv: Integer;
  FoundProfileIndex: Integer;
begin
  case ACategoryIndex of
    CCategory_Common:
    begin
      SetActionValueStr_Action(FEditingAction, ANewText, APropertyIndex);
      FCurrentlyEditingActionType := Ord(FEditingAction^.ActionOptions.Action);

      TriggerOnControlsModified;
      CurrentlyEditingActionType := FEditingAction^.ActionOptions.Action;

      tmrReloadOIContent.Enabled := True;
    end;

    CCategory_ActionSpecific:
    begin
      EditingActionType := Integer(CurrentlyEditingActionType);
      if EditingActionType = CClkUnsetAction then
        Exit;

      case EditingActionType of
        Ord(acFindControl), Ord(acFindSubControl):
        begin
          case APropertyIndex of
            CFindControl_MatchCriteria_PropIndex:
            begin
              SetActionValueStr_FindControl_MatchCriteria(FEditingAction, ANewText, AItemIndex);
              TriggerOnControlsModified;
              Exit;
            end;

            CFindControl_MatchBitmapText_PropIndex:
            begin
              if ANodeLevel = CPropertyItemLevel then
              begin
                ItemIndexMod := AItemIndex mod CPropCount_FindControlMatchBitmapText;
                ItemIndexDiv := AItemIndex div CPropCount_FindControlMatchBitmapText;

                case ItemIndexMod of
                  CFindControl_MatchBitmapText_CropLeft:
                  begin
                    if StrToIntDef(ANewText, 0) < 0 then
                      ANewText := '0';

                    FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv].CropLeft := ANewText;
                    frClickerFindControl.BMPTextFontProfiles[ItemIndexDiv].UpdateSelectionLabelsFromCropInfo(FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv]);
                  end;

                  CFindControl_MatchBitmapText_CropTop:
                  begin
                    if StrToIntDef(ANewText, 0) < 0 then
                      ANewText := '0';

                    FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv].CropTop := ANewText;
                    frClickerFindControl.BMPTextFontProfiles[ItemIndexDiv].UpdateSelectionLabelsFromCropInfo(FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv]);
                  end;

                  CFindControl_MatchBitmapText_CropRight:
                  begin
                    if StrToIntDef(ANewText, 0) < 0 then
                      ANewText := '0';

                    FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv].CropRight := ANewText;
                    frClickerFindControl.BMPTextFontProfiles[ItemIndexDiv].UpdateSelectionLabelsFromCropInfo(FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv]);
                  end;

                  CFindControl_MatchBitmapText_CropBottom:
                  begin
                    if StrToIntDef(ANewText, 0) < 0 then
                      ANewText := '0';

                    FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv].CropBottom := ANewText;
                    frClickerFindControl.BMPTextFontProfiles[ItemIndexDiv].UpdateSelectionLabelsFromCropInfo(FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv]);
                  end;

                  CFindControl_MatchBitmapText_ProfileName_PropItemIndex:
                  begin
                    FoundProfileIndex := frClickerFindControl.GetFontProfileIndexByName(ANewText);
                    if (FoundProfileIndex > -1) and (FoundProfileIndex <> ItemIndexDiv) then
                      raise Exception.Create('Font profile name already exists. Please use a different one.');
                  end;
                end; //case  ItemIndexMod
              end;

              SetActionValueStr_FindControl_MatchBitmapText(FEditingAction, ANewText, AItemIndex {no mod here});
              frClickerFindControl.PreviewText;
              TriggerOnControlsModified;
              Exit;
            end;

            CFindControl_MatchBitmapFiles_PropIndex:
            begin
              TempStringList := TStringList.Create;
              try
                case ANodeLevel of
                  CPropertyLevel:
                  begin
                    FEditingAction^.FindControlOptions.MatchBitmapFiles := ANewText;
                    FOIFrame.ReloadPropertyItems(ACategoryIndex, APropertyIndex);
                    TriggerOnControlsModified;
                  end;

                  CPropertyItemLevel:
                  begin
                    TempStringList.Text := FEditingAction^.FindControlOptions.MatchBitmapFiles;    //read
                    TempStringList.Strings[AItemIndex] := ANewText;                                //modify
                    FEditingAction^.FindControlOptions.MatchBitmapFiles := TempStringList.Text;    //write
                    TriggerOnControlsModified;
                  end;
                end;
              finally
                TempStringList.Free;
              end;

              Exit;
            end;

            CFindControl_MatchBitmapAlgorithmSettings_PropIndex:
            begin
              SetActionValueStr_FindControl_MatchBitmapAlgorithmSettings(FEditingAction, ANewText, AItemIndex);
              TriggerOnControlsModified;
              Exit;
            end;

            CFindControl_InitialRectangle_PropIndex:
            begin
              SetActionValueStr_FindControl_InitialRectangle(FEditingAction, ANewText, AItemIndex);
              TriggerOnControlsModified;
              Exit;
            end;

            CFindControl_UseWholeScreen_PropIndex:   //this call will have to take into account, the screen edges or vars as search area limits
              frClickerFindControl.UpdateSearchAreaLabelsFromKeysOnInitRect(FEditingAction^.FindControlOptions.InitialRectangle);

            else
              ;
          end;
        end; //FindControl  case

        Ord(acCallTemplate):
        begin
          case APropertyIndex of
            CCallTemplate_CallTemplateLoop_PropIndex:
            begin
              SetActionValueStr_CallTemplate_CallTemplateLoop(FEditingAction, ANewText, AItemIndex);
              TriggerOnControlsModified;
              Exit;
            end;

            else
              ;
          end;
        end; //CallTemplate  case
      end;   //case EditingActionType

      //default handler for main properties
      CMainSetActionValueStrFunctions[CurrentlyEditingActionType](FEditingAction, ANewText, APropertyIndex);
      TriggerOnControlsModified;
    end;

    else
      ;
  end;
end;


function TfrClickerActions.EditFontProperties(AItemIndexDiv: Integer; var ANewItems: string): Boolean;
var
  TempFontDialog: TFontDialog;
begin
  Result := False;

  TempFontDialog := TFontDialog.Create(nil);
  try
    TempFontDialog.Font.Name := ANewItems;
    TempFontDialog.Font.Size := FEditingAction^.FindControlOptions.MatchBitmapText[AItemIndexDiv].FontSize;

    if FEditingAction^.FindControlOptions.MatchBitmapText[AItemIndexDiv].Bold then
      TempFontDialog.Font.Style := TempFontDialog.Font.Style + [fsBold];

    if FEditingAction^.FindControlOptions.MatchBitmapText[AItemIndexDiv].Italic then
      TempFontDialog.Font.Style := TempFontDialog.Font.Style + [fsItalic];

    if FEditingAction^.FindControlOptions.MatchBitmapText[AItemIndexDiv].Underline then
      TempFontDialog.Font.Style := TempFontDialog.Font.Style + [fsUnderline];

    if FEditingAction^.FindControlOptions.MatchBitmapText[AItemIndexDiv].StrikeOut then
      TempFontDialog.Font.Style := TempFontDialog.Font.Style + [fsStrikeOut];

    if not TempFontDialog.Execute then
      Exit;

    ANewItems := TempFontDialog.Font.Name;
    Result := True;

    FEditingAction^.FindControlOptions.MatchBitmapText[AItemIndexDiv].FontName := ANewItems; //redundant, because the OI will call another handler for the property itself
    FEditingAction^.FindControlOptions.MatchBitmapText[AItemIndexDiv].FontSize := TempFontDialog.Font.Size;
    FEditingAction^.FindControlOptions.MatchBitmapText[AItemIndexDiv].Bold := fsBold in TempFontDialog.Font.Style;
    FEditingAction^.FindControlOptions.MatchBitmapText[AItemIndexDiv].Italic := fsItalic in TempFontDialog.Font.Style;
    FEditingAction^.FindControlOptions.MatchBitmapText[AItemIndexDiv].Underline := fsUnderline in TempFontDialog.Font.Style;
    FEditingAction^.FindControlOptions.MatchBitmapText[AItemIndexDiv].StrikeOut := fsStrikeOut in TempFontDialog.Font.Style;
  finally
    TempFontDialog.Free;
  end;
end;


function TfrClickerActions.HandleOnOIEditItems(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewItems: string): Boolean;
var
  EditingActionType: Integer;
  ItemIndexDiv, ItemIndexMod: Integer;
begin
  Result := False;
  if ACategoryIndex = CCategory_ActionSpecific then
  begin
    EditingActionType := Integer(CurrentlyEditingActionType);
    if EditingActionType = CClkUnsetAction then
      Exit;

    if EditingActionType in [Ord(acFindControl), Ord(acFindSubControl)] then
      if APropertyIndex = CFindControl_MatchBitmapText_PropIndex then
      begin
        ItemIndexDiv := AItemIndex div CPropCount_FindControlMatchBitmapText;
        ItemIndexMod := AItemIndex mod CPropCount_FindControlMatchBitmapText;

        if ItemIndexMod = CFindControl_MatchBitmapText_FontName_PropItemIndex then
        begin
          Result := EditFontProperties(ItemIndexDiv, ANewItems);

          if Result then
            TriggerOnControlsModified;
        end;
      end;
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


function TfrClickerActions.HandleOnOIGetEnumConstsCount(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer;
var
  EditingActionType: Integer;
  ItemIndexMod: Integer;
begin
  Result := 0;

  case ACategoryIndex of
    CCategory_Common:
      Result := CActionEnumCounts[APropertyIndex];

    CCategory_ActionSpecific:
    begin
      EditingActionType := Integer(CurrentlyEditingActionType);
      if EditingActionType = CClkUnsetAction then
        Exit;

      if CurrentlyEditingActionType in [acFindControl, acFindSubControl] then
      begin
        if APropertyIndex = CFindControl_MatchCriteria_PropIndex then
        begin
          Result := CFindControl_MatchCriteriaEnumCounts[AItemIndex];
          Exit;
        end;

        if APropertyIndex = CFindControl_MatchBitmapText_PropIndex then
        begin
          ItemIndexMod := AItemIndex mod CPropCount_FindControlMatchBitmapText;
          Result := CFindControl_MatchBitmapTextEnumCounts[ItemIndexMod];

          if ItemIndexMod = CFindControl_MatchBitmapText_FontName_PropItemIndex then
            Result := Screen.Fonts.Count;

          Exit;
        end;
      end;

      if CurrentlyEditingActionType = acCallTemplate then
        if APropertyIndex = CCallTemplate_CallTemplateLoop_PropIndex then
        begin
          Result := CCallTemplate_CallTemplateLoopEnumCounts[AItemIndex];
          Exit;
        end;

      Result := CPropEnumCounts[CurrentlyEditingActionType]^[APropertyIndex];
    end;

    else
      Result := 0;
  end;
end;


procedure TfrClickerActions.HandleOnOIGetEnumConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEnumItemIndex: Integer; var AEnumItemName: string);
var
  EditingActionType: Integer;
  ItemIndexMod: Integer;
begin
  AEnumItemName := '';

  case ACategoryIndex of
    CCategory_Common:
      AEnumItemName := CActionEnumStrings[APropertyIndex]^[AEnumItemIndex];

    CCategory_ActionSpecific:
    begin
      EditingActionType := Integer(CurrentlyEditingActionType);
      if EditingActionType = CClkUnsetAction then
        Exit;

      if CurrentlyEditingActionType in [acFindControl, acFindSubControl] then
      begin
        if APropertyIndex = CFindControl_MatchCriteria_PropIndex then
        begin
          AEnumItemName := CFindControl_MatchCriteriaEnumStrings[AItemIndex]^[AEnumItemIndex];
          Exit;
        end;

        if APropertyIndex = CFindControl_MatchBitmapText_PropIndex then
        begin
          ItemIndexMod := AItemIndex mod CPropCount_FindControlMatchBitmapText;

          if ItemIndexMod = CFindControl_MatchBitmapText_FontName_PropItemIndex then
            AEnumItemName := Screen.Fonts.Strings[AEnumItemIndex]
          else
            AEnumItemName := CFindControl_MatchBitmapTextEnumStrings[ItemIndexMod]^[AEnumItemIndex];

          Exit;
        end;
      end;

      if CurrentlyEditingActionType = acCallTemplate then
        if APropertyIndex = CCallTemplate_CallTemplateLoop_PropIndex then
        begin
          AEnumItemName := CCallTemplate_CallTemplateLoopEnumStrings[AItemIndex]^[AEnumItemIndex];
          Exit;
        end;

      AEnumItemName := CPropEnumStrings[CurrentlyEditingActionType]^[APropertyIndex]^[AEnumItemIndex];
    end;

    else
      AEnumItemName := '';
  end;
end;


procedure TfrClickerActions.HandleOnOIPaintText(ANodeData: TNodeDataPropertyRec; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
  const TargetCanvas: TCanvas; Column: TColumnIndex; var TextType: TVSTTextType);
begin
  if ANodeData.Level = 0 then
  begin
    TargetCanvas.Font.Style := [fsBold];
    Exit;
  end;

  if (ANodeData.Level = CPropertyLevel) and (Column = 1) and (CurrentlyEditingActionType in [acFindControl, acFindSubControl]) then
    if (ACategoryIndex = CCategory_ActionSpecific) and (APropertyIndex = CFindControl_MatchBitmapFiles_PropIndex) then
    begin
      TargetCanvas.Font.Style := [fsItalic];
      Exit;
    end;
end;


procedure TfrClickerActions.HandleOnTextEditorMouseDown(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ItemIndexMod, ItemIndexDiv: Integer;
begin
  if (ACategoryIndex = CCategory_ActionSpecific) and (APropertyIndex = CFindControl_InitialRectangle_PropIndex) then
  begin
    case AItemIndex of
      CFindControl_InitialRectangle_LeftOffset_PropItemIndex:
        frClickerFindControl.UpdateOnSearchRectLeftOffsetMouseDown(FEditingAction^.FindControlOptions.InitialRectangle, Sender as TVTEdit, Button, Shift, X, Y);

      CFindControl_InitialRectangle_TopOffset_PropItemIndex:
        frClickerFindControl.UpdateOnSearchRectTopOffsetMouseDown(FEditingAction^.FindControlOptions.InitialRectangle, Sender as TVTEdit, Button, Shift, X, Y);

      CFindControl_InitialRectangle_RightOffset_PropItemIndex:
        frClickerFindControl.UpdateOnSearchRectRightOffsetMouseDown(FEditingAction^.FindControlOptions.InitialRectangle, Sender as TVTEdit, Button, Shift, X, Y);

      CFindControl_InitialRectangle_BottomOffset_PropItemIndex:
        frClickerFindControl.UpdateOnSearchRectBottomOffsetMouseDown(FEditingAction^.FindControlOptions.InitialRectangle, Sender as TVTEdit, Button, Shift, X, Y);
    end;
  end;

  if (ACategoryIndex = CCategory_ActionSpecific) and (APropertyIndex = CFindControl_MatchBitmapText_PropIndex) then
  begin
    ItemIndexMod := AItemIndex mod CPropCount_FindControlMatchBitmapText;
    ItemIndexDiv := AItemIndex div CPropCount_FindControlMatchBitmapText;

    case ItemIndexMod of
      CFindControl_MatchBitmapText_CropLeft:
        frClickerFindControl.UpdateOnTextCroppingLeftMouseDown(FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv], Sender as TVTEdit, Button, Shift, X, Y);

      CFindControl_MatchBitmapText_CropTop:
        frClickerFindControl.UpdateOnTextCroppingTopMouseDown(FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv], Sender as TVTEdit, Button, Shift, X, Y);

      CFindControl_MatchBitmapText_CropRight:
        frClickerFindControl.UpdateOnTextCroppingRightMouseDown(FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv], Sender as TVTEdit, Button, Shift, X, Y);

      CFindControl_MatchBitmapText_CropBottom:
        frClickerFindControl.UpdateOnTextCroppingBottomMouseDown(FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv], Sender as TVTEdit, Button, Shift, X, Y);
    end;
  end;
end;


function TfrClickerActions.HandleOnTextEditorMouseMove(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; Shift: TShiftState; X, Y: Integer): Boolean;
var
  ItemIndexMod, ItemIndexDiv: Integer;
begin
  Result := False;

  if (ACategoryIndex = CCategory_ActionSpecific) and (APropertyIndex = CFindControl_InitialRectangle_PropIndex) then
  begin
    case AItemIndex of
      CFindControl_InitialRectangle_LeftOffset_PropItemIndex:
      begin
        frClickerFindControl.UpdateOnSearchRectLeftOffsetMouseMove(FEditingAction^.FindControlOptions.InitialRectangle, Sender as TVTEdit, Shift, X, Y);
        Result := True;
      end;

      CFindControl_InitialRectangle_TopOffset_PropItemIndex:
      begin
        frClickerFindControl.UpdateOnSearchRectTopOffsetMouseMove(FEditingAction^.FindControlOptions.InitialRectangle, Sender as TVTEdit, Shift, X, Y);
        Result := True;
      end;

      CFindControl_InitialRectangle_RightOffset_PropItemIndex:
      begin
        frClickerFindControl.UpdateOnSearchRectRightOffsetMouseMove(FEditingAction^.FindControlOptions.InitialRectangle, Sender as TVTEdit, Shift, X, Y);
        Result := True;
      end;

      CFindControl_InitialRectangle_BottomOffset_PropItemIndex:
      begin
        frClickerFindControl.UpdateOnSearchRectBottomOffsetMouseMove(FEditingAction^.FindControlOptions.InitialRectangle, Sender as TVTEdit, Shift, X, Y);
        Result := True;
      end;
    end;
  end;

  if (ACategoryIndex = CCategory_ActionSpecific) and (APropertyIndex = CFindControl_MatchBitmapText_PropIndex) then
  begin
    ItemIndexMod := AItemIndex mod CPropCount_FindControlMatchBitmapText;
    ItemIndexDiv := AItemIndex div CPropCount_FindControlMatchBitmapText;

    case ItemIndexMod of
      CFindControl_MatchBitmapText_CropLeft:
      begin
        frClickerFindControl.UpdateOnTextCroppingLeftMouseMove(FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv], Sender as TVTEdit, Shift, X, Y, ItemIndexDiv);
        Result := True;
      end;

      CFindControl_MatchBitmapText_CropTop:
      begin
        frClickerFindControl.UpdateOnTextCroppingTopMouseMove(FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv], Sender as TVTEdit, Shift, X, Y, ItemIndexDiv);
        Result := True;
      end;

      CFindControl_MatchBitmapText_CropRight:
      begin
        frClickerFindControl.UpdateOnTextCroppingRightMouseMove(FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv], Sender as TVTEdit, Shift, X, Y, ItemIndexDiv);
        Result := True;
      end;

      CFindControl_MatchBitmapText_CropBottom:
      begin
        frClickerFindControl.UpdateOnTextCroppingBottomMouseMove(FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv], Sender as TVTEdit, Shift, X, Y, ItemIndexDiv);
        Result := True;
      end;
    end;
  end;
end;


procedure TfrClickerActions.HandleOnOITextEditorKeyUp(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  ItemIndexMod, ItemIndexDiv: Integer;
begin
  if ACategoryIndex = CCategory_ActionSpecific then
  begin
    if FEditingAction^.ActionOptions.Action in [acFindControl, acFindSubControl] then
      case APropertyIndex of
        CFindControl_MatchText_PropIndex:
        begin
          FEditingAction^.FindControlOptions.MatchText := TVTEdit(Sender).Text;

          frClickerFindControl.PreviewText;
          for i := 0 to Length(FEditingAction^.FindControlOptions.MatchBitmapText) - 1 do
            frClickerFindControl.BMPTextFontProfiles[i].UpdateSelectionLabelsFromCropInfo(FEditingAction^.FindControlOptions.MatchBitmapText[i]);
        end;

        CFindControl_MatchBitmapText_PropIndex:
        begin
          ItemIndexMod := AItemIndex mod CPropCount_FindControlMatchBitmapText;
          ItemIndexDiv := AItemIndex div CPropCount_FindControlMatchBitmapText;

          if ItemIndexMod = CFindControl_MatchBitmapText_ProfileName_PropItemIndex then
          begin
            FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv].ProfileName := TVTEdit(Sender).Text;
            frClickerFindControl.UpdateFontProfilName(ItemIndexDiv, TVTEdit(Sender).Text);
          end;

          frClickerFindControl.PreviewText;
        end;

        CFindControl_InitialRectangle_PropIndex:
        begin
          if AItemIndex in [CFindControl_InitialRectangle_LeftOffset_PropItemIndex .. CFindControl_InitialRectangle_BottomOffset_PropItemIndex] then
          begin
            case AItemIndex of
              CFindControl_InitialRectangle_LeftOffset_PropItemIndex:
                FEditingAction^.FindControlOptions.InitialRectangle.LeftOffset := (Sender as TVTEdit).Text;

              CFindControl_InitialRectangle_TopOffset_PropItemIndex:
                FEditingAction^.FindControlOptions.InitialRectangle.TopOffset := (Sender as TVTEdit).Text;

              CFindControl_InitialRectangle_RightOffset_PropItemIndex:
                FEditingAction^.FindControlOptions.InitialRectangle.RightOffset := (Sender as TVTEdit).Text;

              CFindControl_InitialRectangle_BottomOffset_PropItemIndex:
                FEditingAction^.FindControlOptions.InitialRectangle.BottomOffset := (Sender as TVTEdit).Text;
            end;

            frClickerFindControl.UpdateSearchAreaLabelsFromKeysOnInitRect(FEditingAction^.FindControlOptions.InitialRectangle);
          end;
        end; //init rect
      end; //case
  end;
end;


procedure TfrClickerActions.HandleOnOITextEditorKeyDown(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_SPACE then
    if ssCtrl in Shift then
      //open a pop-up window with a list of available variables and functions;
end;


procedure TfrClickerActions.HandleOnOIEditorAssignMenuAndTooltip(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; var APopupMenu: TPopupMenu; var AHint: string; var AShowHint: Boolean);
var
  TempValue: string;
begin
  APopupMenu := nil;
  AHint := '';
  AShowHint := True;

  if ACategoryIndex = CCategory_ActionSpecific then
  begin
    case CurrentlyEditingActionType of
      acFindControl, acFindSubControl:
      begin
        case APropertyIndex of
          CFindControl_MatchCriteria_PropIndex:
          begin
            if AItemIndex = CFindControl_MatchCriteria_SearchForControlMode_PropItemIndex then
              AHint := GetPropertyHint_FindControl_MatchCriteria_SearchForControlMode;
          end;

          CFindControl_MatchBitmapText_PropIndex:
            case AItemIndex of
              CFindControl_MatchBitmapText_ForegroundColor_PropItemIndex, CFindControl_MatchBitmapText_BackgroundColor_PropItemIndex:
              begin
                FLastClickedTVTEdit := nil;
                FLastClickedEdit := Sender as TEdit;
                APopupMenu := pmStandardColorVariables;
              end;

              else
                ;
            end;

          CFindControl_InitialRectangle_PropIndex:
            if AItemIndex in [CFindControl_InitialRectangle_Left_PropItemIndex .. CFindControl_InitialRectangle_Bottom_PropItemIndex] then
            begin
              case AItemIndex of
                CFindControl_InitialRectangle_Left_PropItemIndex:
                begin
                  FLastClickedTVTEdit := Sender as TVTEdit;
                  FLastClickedEdit := nil;
                  APopupMenu := pmStandardControlRefVars;
                  TempValue := FEditingAction^.FindControlOptions.InitialRectangle.Left;
                  AHint := GetPropertyHint_FindControl_InitialRectangle_Left(TempValue, EvaluateReplacements(TempValue));
                end;

                CFindControl_InitialRectangle_Top_PropItemIndex:
                begin
                  FLastClickedTVTEdit := Sender as TVTEdit;
                  FLastClickedEdit := nil;
                  APopupMenu := pmStandardControlRefVars;
                  TempValue := FEditingAction^.FindControlOptions.InitialRectangle.Top;
                  AHint := GetPropertyHint_FindControl_InitialRectangle_Top(TempValue, EvaluateReplacements(TempValue));
                end;

                CFindControl_InitialRectangle_Right_PropItemIndex:
                begin
                  FLastClickedTVTEdit := Sender as TVTEdit;
                  FLastClickedEdit := nil;
                  APopupMenu := pmStandardControlRefVars;
                  TempValue := FEditingAction^.FindControlOptions.InitialRectangle.Right;
                  AHint := GetPropertyHint_FindControl_InitialRectangle_Right(TempValue, EvaluateReplacements(TempValue));
                end;

                CFindControl_InitialRectangle_Bottom_PropItemIndex:
                begin
                  FLastClickedTVTEdit := Sender as TVTEdit;
                  FLastClickedEdit := nil;
                  APopupMenu := pmStandardControlRefVars;
                  TempValue := FEditingAction^.FindControlOptions.InitialRectangle.Bottom;
                  AHint := GetPropertyHint_FindControl_InitialRectangle_Bottom(TempValue, EvaluateReplacements(TempValue));
                end;
              end;
            end;
        end; //case
      end; //FindControl

      acWindowOperations:
      begin
        APopupMenu := pmWindowOperationsEditors;

        case APropertyIndex of
          CWindowOperations_NewX, CWindowOperations_NewY:
          begin
            AHint := GetPropertyHint_WindowOperations_NewXY;
            MenuItem_SetFromControlLeftAndTop.Enabled := True;
            MenuItem_SetFromControlWidthAndHeight.Enabled := False;
          end;

          CWindowOperations_NewWidth, CWindowOperations_NewHeight:
          begin
            AHint := GetPropertyHint_WindowOperations_NewWidthHeight;
            MenuItem_SetFromControlLeftAndTop.Enabled := False;
            MenuItem_SetFromControlWidthAndHeight.Enabled := True;
          end

          else
          begin
            MenuItem_SetFromControlLeftAndTop.Enabled := False;
            MenuItem_SetFromControlWidthAndHeight.Enabled := False;
          end ;
        end;
      end;

      acSleep:
        AHint := GetPropertyHint_Sleep;

      else
        ;
    end;  //case CurrentlyEditingActionType
  end;
end;


procedure TfrClickerActions.HandleOnOIGetFileDialogSettings(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var AFilter, AInitDir: string);
var
  EditingActionType: Integer;
begin
  AFilter := '';
  AInitDir := '';

  if ACategoryIndex <> CCategory_ActionSpecific then
    Exit;

  EditingActionType := Integer(CurrentlyEditingActionType);
  if EditingActionType = CClkUnsetAction then
    Exit;

  case CurrentlyEditingActionType of
    acExecApp:
      if APropertyIndex = CExecApp_PathToApp_PropIndex then
      begin
        AFilter := 'Executable files (*.exe)|*.exe|All files (*.*)|*.*';
        AInitDir := ExtractFileDir(ParamStr(0));
      end;

    acFindControl, acFindSubControl:
    begin
      case APropertyIndex of
        CFindControl_MatchBitmapFiles_PropIndex:
        begin
          AFilter := 'Bitmap files (*.bmp)|*.bmp|All files (*.*)|*.*';
          AInitDir := FBMPsDir;
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


procedure TfrClickerActions.AddMenuItemToPopupMenu(APopupMenu: TPopupMenu; ACaption: TCaption; AHandler: TNotifyEvent;
  ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer);
var
  MenuData: POIMenuItemData;
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := ACaption;
  MenuItem.OnClick := AHandler;

  New(MenuData);
  MenuItem.Tag := {%H-}PtrInt(MenuData);
  MenuData^.OwnerMenu := APopupMenu;
  MenuData^.NodeLevel := ANodeLevel;
  MenuData^.CategoryIndex := ACategoryIndex;
  MenuData^.PropertyIndex := APropertyIndex;
  MenuData^.PropertyItemIndex := AItemIndex;
  MenuData^.MenuItemCaption := ACaption;

  APopupMenu.Items.Add(MenuItem);
end;


procedure TfrClickerActions.HandleOnOIArrowEditorClick(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer);
var
  tp: TPoint;
  PropertyMenu: TPopupMenu;
  i: Integer;
  s: string;
  BMPTxt: TClkFindControlMatchBitmapText;
  ItemIndexMod, ItemIndexDiv: Integer;
begin
  case ACategoryIndex of
    CCategory_Common:
      case APropertyIndex of
        CMain_ActionTimeout_PropIndex:
        begin
          PropertyMenu := TPopupMenu.Create(Self);

          AddMenuItemToPopupMenu(PropertyMenu, '0', MenuItem_SetActionTimeoutFromOI, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);
          AddMenuItemToPopupMenu(PropertyMenu, '1000', MenuItem_SetActionTimeoutFromOI, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);
          AddMenuItemToPopupMenu(PropertyMenu, '10000', MenuItem_SetActionTimeoutFromOI, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);
          AddMenuItemToPopupMenu(PropertyMenu, '30000', MenuItem_SetActionTimeoutFromOI, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

          GetCursorPos(tp);
          PropertyMenu.PopUp(tp.X, tp.Y);
        end
        else
          ;
      end;

    CCategory_ActionSpecific:
    begin
      case APropertyIndex of
        CFindControl_MatchText_PropIndex, CFindControl_MatchClassName_PropIndex:
        begin
          PropertyMenu := TPopupMenu.Create(Self);
          AddMenuItemToPopupMenu(PropertyMenu, 'Copy values from preview window', MenuItem_CopyTextAndClassFromPreviewWindowClick,
            ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

          AddMenuItemToPopupMenu(PropertyMenu, 'Copy values from window interpreter', MenuItem_CopyTextAndClassFromWinInterpWindowClick,
            ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

          AddMenuItemToPopupMenu(PropertyMenu, 'Copy values from remote screen', MenuItem_CopyTextAndClassFromRemoteScreenWindowClick,
            ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

          GetCursorPos(tp);
          PropertyMenu.PopUp(tp.X, tp.Y);
        end;

        CFindControl_MatchBitmapText_PropIndex:
          case ANodeLevel of
            CPropertyLevel:
            begin
              PropertyMenu := TPopupMenu.Create(Self);
              AddMenuItemToPopupMenu(PropertyMenu, 'Add font profile', MenuItem_AddFontProfileToPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

              if Length(FEditingAction^.FindControlOptions.MatchBitmapText) > 0 then
                AddMenuItemToPopupMenu(PropertyMenu, '-', nil, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

              for i := 0 to Length(FEditingAction^.FindControlOptions.MatchBitmapText) - 1 do
              begin
                BMPTxt := FEditingAction^.FindControlOptions.MatchBitmapText[i];
                s := '  Name: ' + BMPTxt.ProfileName + '  (' + BMPTxt.FontName + ', ' + IntToStr(BMPTxt.FontSize) + ', ' + BMPTxt.ForegroundColor + ', ' + BMPTxt.BackgroundColor + ')';
                AddMenuItemToPopupMenu(PropertyMenu, 'Remove font profile[' + IntToStr(i) + ']  ' + s, MenuItem_RemoveFontProfileFromPropertyListClick,
                  ANodeLevel, ACategoryIndex, APropertyIndex, i);  //ItemIndex is not the real one. It points to the profile index.
              end;

              GetCursorPos(tp);
              PropertyMenu.PopUp(tp.X, tp.Y);
            end;

            CPropertyItemLevel:
            begin
              ItemIndexMod := AItemIndex mod CPropCount_FindControlMatchBitmapText;
              ItemIndexDiv := AItemIndex div CPropCount_FindControlMatchBitmapText;

              if ItemIndexMod = CFindControl_MatchBitmapText_ProfileName_PropItemIndex then
                if Length(FEditingAction^.FindControlOptions.MatchBitmapText) > 1 then  //add only if there are at least two profiles
                begin
                  PropertyMenu := TPopupMenu.Create(Self);
                  AddMenuItemToPopupMenu(PropertyMenu, 'Move font profile up', MenuItem_MoveFontProfileUpInPropertyListClick,
                    ANodeLevel, ACategoryIndex, APropertyIndex, ItemIndexDiv); //sending the profile index through item index arg

                  AddMenuItemToPopupMenu(PropertyMenu, 'Move font profile down', MenuItem_MoveFontProfileDownInPropertyListClick,
                    ANodeLevel, ACategoryIndex, APropertyIndex, ItemIndexDiv); //sending the profile index through item index arg

                  GetCursorPos(tp);
                  PropertyMenu.PopUp(tp.X, tp.Y);
                end;
            end;
          end; //case

        CFindControl_MatchBitmapFiles_PropIndex:
        begin
          case ANodeLevel of
            CPropertyLevel:
            begin
              PropertyMenu := TPopupMenu.Create(Self);
              AddMenuItemToPopupMenu(PropertyMenu, 'Add file(s) to this list...', MenuItem_AddFilesToPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

              AddMenuItemToPopupMenu(PropertyMenu, 'Remove all files from this list...', MenuItem_RemoveAllFilesFromPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

              GetCursorPos(tp);
              PropertyMenu.PopUp(tp.X, tp.Y);
            end;

            CPropertyItemLevel:
            begin
              PropertyMenu := TPopupMenu.Create(Self);
              AddMenuItemToPopupMenu(PropertyMenu, 'Remove file from list...', MenuItem_RemoveFileFromPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

              AddMenuItemToPopupMenu(PropertyMenu, 'Move file up (one position)', MenuItem_MoveFileUpInPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

              AddMenuItemToPopupMenu(PropertyMenu, 'Move file down (one position)', MenuItem_MoveFileDownInPropertyListClick,
                ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

              GetCursorPos(tp);
              PropertyMenu.PopUp(tp.X, tp.Y);
            end;

            else
              ;
          end;
        end;

        CCallTemplate_TemplateFileName_PropIndex:
        begin
          LoadListOfAvailableTemplates;
          GetCursorPos(tp);
          FPmLocalTemplates.PopUp(tp.X, tp.Y);
        end;
      end;
    end;
  end;
end;


procedure TfrClickerActions.HandleOnOIUserEditorClick(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ARepaintValue: Boolean);
var
  EditingActionType: Integer;
  Condition: string;
begin
  case ACategoryIndex of
    CCategory_Common:
    begin
      if APropertyIndex = CMain_ActionCondition_PropIndex then
      begin
        Condition := FEditingAction^.ActionOptions.ActionCondition;
        if DoOnEditCallTemplateBreakCondition(Condition) then
        begin
          FEditingAction^.ActionOptions.ActionCondition := Condition;
          TriggerOnControlsModified;
        end;
      end;
    end;

    CCategory_ActionSpecific:
    begin
      EditingActionType := Integer(CurrentlyEditingActionType);
      if EditingActionType = CClkUnsetAction then
        Exit;

      case CurrentlyEditingActionType of
        acExecApp:
          if APropertyIndex = CExecApp_ListOfParams_PropIndex then
          begin
            frClickerExecApp.BringToFront;  //MessageBox(Handle, 'Param list editor', 'Files', MB_ICONINFORMATION);
          end;

        acFindControl, acFindSubControl:
          if APropertyIndex = CFindControl_MatchBitmapFiles_PropIndex then
          begin
            //MessageBox(Handle, 'File list editor', 'Files', MB_ICONINFORMATION);
            TriggerOnControlsModified;
          end;

        acCallTemplate:
        begin
          case APropertyIndex of
            CCallTemplate_ListOfCustomVarsAndValues_PropIndex:
            begin
              frClickerCallTemplate.SetListOfCustomVariables(FEditingAction^.CallTemplateOptions.ListOfCustomVarsAndValues);
              //TriggerOnControlsModified;
            end;

            CCallTemplate_CallTemplateLoop_PropIndex:
              if AItemIndex = CCallTemplate_CallTemplateLoopProperties_BreakCondition_PropItemIndex then
              begin
                Condition := FEditingAction^.CallTemplateOptions.CallTemplateLoop.BreakCondition;
                if DoOnEditCallTemplateBreakCondition(Condition) then
                begin
                  FEditingAction^.CallTemplateOptions.CallTemplateLoop.BreakCondition := Condition;
                  TriggerOnControlsModified;
                end;
              end;
          end;
        end;

        acSetVar:
          if APropertyIndex = CSetVar_ListOfVarNamesValuesAndEvalBefore then
          begin
            frClickerSetVar.SetListOfSetVars(FEditingAction^.SetVarOptions);
            frClickerSetVar.BringToFront;
            //MessageBox(Handle, 'SetVar editor', 'Files', MB_ICONINFORMATION);
          end;

        else
          ;
      end;   //case
    end; //CCategory_ActionSpecific
  end; //case
end;


function TfrClickerActions.HandleOnOIBrowseFile(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  AFilter, ADialogInitDir: string; var Handled: Boolean; AReturnMultipleFiles: Boolean = False): string;
var
  EditingActionType: Integer;
  AOpenDialog: TOpenDialog;
begin
  Result := '';

  case ACategoryIndex of
    CCategory_Common:
      ;

    CCategory_ActionSpecific:
    begin
      EditingActionType := Integer(CurrentlyEditingActionType);
      if EditingActionType = CClkUnsetAction then
        Exit;

      case CurrentlyEditingActionType of
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

        acFindControl, acFindSubControl:
        begin
          if APropertyIndex = CFindControl_MatchBitmapText_PropIndex then
            Handled := True; //do nothing, this is not a file path

          if APropertyIndex = CFindControl_MatchBitmapFiles_PropIndex then
          begin
            DoOnSetPictureOpenDialogInitialDir(ADialogInitDir);
            DoOnSetPictureOpenSetMultiSelect;

            if DoOnPictureOpenDialogExecute then
              Result := DoOnGetPictureOpenDialogFileName;

            Handled := True;
          end;
        end;

        acCallTemplate:
        begin
          case APropertyIndex of
            CCallTemplate_TemplateFileName_PropIndex:
            begin
              DoOnSetTemplateOpenDialogInitialDir(ADialogInitDir);
              if DoOnTemplateOpenDialogExecute then
                Result := DoOnGetTemplateOpenDialogFileName;

              Handled := True;
            end;

            else
              ;
          end;
        end;

        else
          ;
      end;   //case
    end; //CCategory_ActionSpecific
  end; //case
end;


procedure TfrClickerActions.HandleOnAfterSpinTextEditorChanging(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewValue: string);
var
  ItemIndexMod, ItemIndexDiv: Integer;
begin
  if (ACategoryIndex = CCategory_ActionSpecific) and (APropertyIndex = CFindControl_InitialRectangle_PropIndex) then
  begin
    case AItemIndex of
      CFindControl_InitialRectangle_LeftOffset_PropItemIndex:
      begin
        FEditingAction^.FindControlOptions.InitialRectangle.LeftOffset := ANewValue;
        frClickerFindControl.UpdateSearchAreaLabelsFromKeysOnInitRect(FEditingAction^.FindControlOptions.InitialRectangle);
      end;

      CFindControl_InitialRectangle_TopOffset_PropItemIndex:
      begin
        FEditingAction^.FindControlOptions.InitialRectangle.TopOffset := ANewValue;
        frClickerFindControl.UpdateSearchAreaLabelsFromKeysOnInitRect(FEditingAction^.FindControlOptions.InitialRectangle);
      end;

      CFindControl_InitialRectangle_RightOffset_PropItemIndex:
      begin
        FEditingAction^.FindControlOptions.InitialRectangle.RightOffset := ANewValue;
        frClickerFindControl.UpdateSearchAreaLabelsFromKeysOnInitRect(FEditingAction^.FindControlOptions.InitialRectangle);
      end;

      CFindControl_InitialRectangle_BottomOffset_PropItemIndex:
      begin
        FEditingAction^.FindControlOptions.InitialRectangle.BottomOffset := ANewValue;
        frClickerFindControl.UpdateSearchAreaLabelsFromKeysOnInitRect(FEditingAction^.FindControlOptions.InitialRectangle);
      end;
    end;
  end;

  if (ACategoryIndex = CCategory_ActionSpecific) and (APropertyIndex = CFindControl_MatchBitmapText_PropIndex) then
  begin
    ItemIndexMod := AItemIndex mod CPropCount_FindControlMatchBitmapText;
    ItemIndexDiv := AItemIndex div CPropCount_FindControlMatchBitmapText;

    case ItemIndexMod of
      CFindControl_MatchBitmapText_CropLeft:
      begin
        FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv].CropLeft := ANewValue;
        if StrToIntDef(ANewValue, 0) < 0 then
          ANewValue := '0';

        frClickerFindControl.BMPTextFontProfiles[ItemIndexDiv].UpdateSelectionLabelsFromCropInfo(FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv]);
      end;

      CFindControl_MatchBitmapText_CropTop:
      begin
        FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv].CropTop := ANewValue;
        if StrToIntDef(ANewValue, 0) < 0 then
          ANewValue := '0';

        frClickerFindControl.BMPTextFontProfiles[ItemIndexDiv].UpdateSelectionLabelsFromCropInfo(FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv]);
      end;

      CFindControl_MatchBitmapText_CropRight:
      begin
        FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv].CropRight := ANewValue;
        if StrToIntDef(ANewValue, 0) < 0 then
          ANewValue := '0';

        frClickerFindControl.BMPTextFontProfiles[ItemIndexDiv].UpdateSelectionLabelsFromCropInfo(FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv]);
      end;

      CFindControl_MatchBitmapText_CropBottom:
      begin
        FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv].CropBottom := ANewValue;
        if StrToIntDef(ANewValue, 0) < 0 then
          ANewValue := '0';

        frClickerFindControl.BMPTextFontProfiles[ItemIndexDiv].UpdateSelectionLabelsFromCropInfo(FEditingAction^.FindControlOptions.MatchBitmapText[ItemIndexDiv]);
      end;
    end;
  end;
end;

end.
