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
  Menus, ClickerUtils, ClickerConditionEditorFrame, ClickerFindControlFrame, Types,
  InMemFileSystem;

type
  { TfrClickerActions }

  TfrClickerActions = class(TFrame)
    btnBrowseOtherTemplates: TButton;
    btnCopyOffsetsFromMain: TButton;
    btnSetFromControlLeftAndTop: TButton;
    btnSetFromControlWidthAndHeight: TButton;
    btnBrowseCallTemplateLoopBreakCondition: TButton;
    chkCallTemplateLoopEnabled: TCheckBox;
    chkNoConsole: TCheckBox;
    chkWindowOperationsEnablePos: TCheckBox;
    chkMoveWithoutClick: TCheckBox;
    chkLeaveMouse: TCheckBox;
    chkWindowOperationsEnableSize: TCheckBox;
    cmbCallTemplateLoopBreakPosition: TComboBox;
    cmbClickType: TComboBox;
    cmbUseInheritHandles: TComboBox;
    cmbWindowOperationsType: TComboBox;
    cmbCallTemplateLoopDirection: TComboBox;
    grpCallTemplateLoop: TGroupBox;
    imglstSetVar: TImageList;
    lblFeatureInWork: TLabel;
    lbeCallTemplateLoopBreakCondition: TLabeledEdit;
    lblCallTemplateLoopDirection: TLabel;
    lbeCallTemplateLoopCounter: TLabeledEdit;
    lbeCallTemplateLoopInitValue: TLabeledEdit;
    lbeCallTemplateLoopEndValue: TLabeledEdit;
    lbeWindowOperationsX: TLabeledEdit;
    lbeWindowOperationsY: TLabeledEdit;
    lbeWindowOperationsWidth: TLabeledEdit;
    lbeWindowOperationsHeight: TLabeledEdit;
    lblCallTemplateLoopBreakPosition: TLabel;
    lblSetTextInfo: TLabel;
    lblWindowOperation: TLabel;
    lblAvailableFunctions: TLabel;
    lblUseInheritHandles: TLabel;
    lbeExecAppStdIn: TLabeledEdit;
    lbeExecAppCurrentDir: TLabeledEdit;
    lblMouseOnExecDbgImgBB: TLabel;
    lblMouseOnExecDbgImgGG: TLabel;
    lblMouseOnExecDbgImgRR: TLabel;
    lblSetVarToHttpInfo2: TLabel;
    lblClickType: TLabel;
    lblSetVarToHttpInfo: TLabel;
    lbeTemplateFileName: TLabeledEdit;
    lblSetVarToHttpInfo1: TLabel;
    memAvailableFunctions: TMemo;
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
    pmStandardColorVariables: TPopupMenu;
    pmSetVars: TPopupMenu;
    pnlActionConditions: TPanel;
    spdbtnBrowseLocalTemplates: TSpeedButton;
    TabSheetActionWindowOperations: TTabSheet;
    TabSheetActionFindControl: TTabSheet;
    TabSheetActionSetVar: TTabSheet;
    PageControlActions: TPageControl;
    TabSheetActionClick: TTabSheet;
    TabSheetActionExecApp: TTabSheet;
    TabSheetActionFindSubControl: TTabSheet;
    TabSheetActionSetText: TTabSheet;
    cmbXClickReference: TComboBox;
    cmbYClickReference: TComboBox;
    lbeClickXOffset: TLabeledEdit;
    lbeClickYOffset: TLabeledEdit;
    lblXClickReference: TLabel;
    lblYClickReference: TLabel;
    lbeSetNewText: TLabeledEdit;
    rdgrpSetTextControlType: TRadioGroup;
    lblInfoSetText: TLabel;
    TabSheetActionCall: TTabSheet;
    rdgrpMouseButton: TRadioGroup;
    chkClickWithCtrl: TCheckBox;
    chkClickWithAlt: TCheckBox;
    chkClickWithShift: TCheckBox;
    chkClickWithDoubleClick: TCheckBox;
    pmCommonTimeouts: TPopupMenu;
    N10001: TMenuItem;
    N100001: TMenuItem;
    N01: TMenuItem;
    N300001: TMenuItem;
    tmrEditCustomVars: TTimer;
    tmrEditSetVars: TTimer;
    vallstCustomVariables: TValueListEditor;
    lblCustomUserVarsBeforeCall: TLabel;
    pmCustomVars: TPopupMenu;
    AddCustomVarRow1: TMenuItem;
    RemoveCustomVarRow1: TMenuItem;
    vallstVariables: TValueListEditor;
    scrboxDebugBmp: TScrollBox;
    imgDebugBmp: TImage;
    chkCallOnlyIfContitionIsTrue: TCheckBox;
    lbeCallOnlyIfContitionVarName: TLabeledEdit;
    lbeCallOnlyIfContitionVarValue: TLabeledEdit;
    lblConditionEquals: TLabel;
    MenuItemSavePreviewImage: TMenuItem;
    MenuItemCopyPreviewImage: TMenuItem;
    lbeMultiClickCount: TLabeledEdit;
    lbeExecAppPath: TLabeledEdit;
    btnBrowseExecApp: TButton;
    chkWaitForApp: TCheckBox;
    memExecAppParams: TMemo;
    lblExecAppParams: TLabel;
    imglstActions: TImageList;
    pmDebugVars: TPopupMenu;
    CopyDebugValuesListToClipboard1: TMenuItem;
    PasteDebugValuesListFromClipboard1: TMenuItem;
    PasteDebugValuesListFromMainExecutionList1: TMenuItem;
    lblDebugBitmapXMouseOffset: TLabel;
    lblDebugBitmapYMouseOffset: TLabel;
    MenuItemErasePreviewImage: TMenuItem;
    btnSelectNoTemplate: TButton;
    chkShowDebugGrid: TCheckBox;
    imgDebugGrid: TImage;
    PageControlActionExecution: TPageControl;
    TabSheetAction: TTabSheet;
    TabSheetCondition: TTabSheet;
    TabSheetDebugging: TTabSheet;
    imglstActionExecution: TImageList;
    lblAction: TLabel;
    spdbtnCommonTimeouts: TSpeedButton;
    lbeActionName: TLabeledEdit;
    cmbActions: TComboBox;
    lbeActionTimeout: TLabeledEdit;
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
    chkEvaluateVarsBeforeCalling: TCheckBox;
    lblDeprecatedCondition: TLabel;
    lbeClickVarX: TLabeledEdit;
    lbeClickVarY: TLabeledEdit;
    TabSheetActionSleep: TTabSheet;
    lbeSleep: TLabeledEdit;
    lblSleepInfo: TLabel;
    lblSleepInfo2: TLabel;
    pnlSleepElapsedTime: TPanel;
    pnlSleepRemainingTime: TPanel;
    prbSleep: TProgressBar;
    vstCustomVariables: TVirtualStringTree;
    vstSetVar: TVirtualStringTree;
    procedure btnBrowseCallTemplateLoopBreakConditionClick(Sender: TObject);
    procedure btnBrowseOtherTemplatesClick(Sender: TObject);
    procedure btnSetFromControlLeftAndTopClick(Sender: TObject);
    procedure btnSetFromControlWidthAndHeightClick(Sender: TObject);
    procedure chkAllowToFailChange(Sender: TObject);
    procedure chkCallTemplateLoopEnabledChange(Sender: TObject);
    procedure chkClickWithDoubleClickChange(Sender: TObject);
    procedure chkLeaveMouseChange(Sender: TObject);
    procedure chkMoveWithoutClickChange(Sender: TObject);
    procedure chkNoConsoleChange(Sender: TObject);
    procedure chkWaitForAppChange(Sender: TObject);
    procedure chkWaitForControlToGoAwayChange(Sender: TObject);
    procedure chkWindowOperationsEnablePosChange(Sender: TObject);
    procedure chkWindowOperationsEnableSizeChange(Sender: TObject);
    procedure cmbCallTemplateLoopBreakPositionChange(Sender: TObject);
    procedure cmbCallTemplateLoopDirectionChange(Sender: TObject);
    procedure cmbClickTypeChange(Sender: TObject);
    procedure cmbUseInheritHandlesChange(Sender: TObject);
    procedure cmbWindowOperationsTypeChange(Sender: TObject);
    procedure lbeCallTemplateLoopBreakConditionChange(Sender: TObject);
    procedure lbeCallTemplateLoopCounterChange(Sender: TObject);
    procedure lbeCallTemplateLoopEndValueChange(Sender: TObject);
    procedure lbeCallTemplateLoopInitValueChange(Sender: TObject);

    procedure lbeExecAppCurrentDirChange(Sender: TObject);
    procedure lbeExecAppStdInChange(Sender: TObject);

    procedure lbeTemplateFileNameChange(Sender: TObject);
    procedure lbeFindCachedControlLeftChange(Sender: TObject);
    procedure lbeFindCachedControlTopChange(Sender: TObject);
    procedure lbeWindowOperationsHeightChange(Sender: TObject);
    procedure lbeWindowOperationsWidthChange(Sender: TObject);
    procedure lbeWindowOperationsXChange(Sender: TObject);
    procedure lbeWindowOperationsYChange(Sender: TObject);

    procedure MenuItem_AddSetVarClick(Sender: TObject);
    procedure MenuItem_RemoveSetVarClick(Sender: TObject);

    procedure PageControlActionsChange(Sender: TObject);
    procedure scrboxDebugBmpMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure spdbtnBrowseLocalTemplatesClick(Sender: TObject);
    procedure spdbtnDisplaySearchAreaDbgImgMenuClick(Sender: TObject);
    procedure tmrEditCustomVarsTimer(Sender: TObject);
    procedure tmrEditSetVarsTimer(Sender: TObject);
    procedure vallstCustomVariablesExit(Sender: TObject);

    procedure spdbtnCommonTimeoutsClick(Sender: TObject);
    procedure N01Click(Sender: TObject);
    procedure AddCustomVarRow1Click(Sender: TObject);
    procedure RemoveCustomVarRow1Click(Sender: TObject);
    procedure chkCallOnlyIfContitionIsTrueClick(Sender: TObject);
    procedure vallstCustomVariablesSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure vallstCustomVariablesValidate(Sender: TObject; ACol,
      ARow: Integer; const KeyName, KeyValue: string);
    procedure btnBrowseExecAppClick(Sender: TObject);
    procedure CopyDebugValuesListToClipboard1Click(Sender: TObject);
    procedure PasteDebugValuesListFromClipboard1Click(Sender: TObject);
    procedure PasteDebugValuesListFromMainExecutionList1Click(Sender: TObject);

    procedure imgDebugBmpMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure btnSelectNoTemplateClick(Sender: TObject);
    procedure chkShowDebugGridClick(Sender: TObject);
    procedure MenuItemSaveDebugImageClick(Sender: TObject);
    procedure MenuItemCopyDebugImageClick(Sender: TObject);
    procedure MenuItemEraseDebugImageClick(Sender: TObject);

    procedure lbeExecAppPathChange(Sender: TObject);
    procedure memExecAppParamsChange(Sender: TObject);
    procedure cmbXClickReferenceChange(Sender: TObject);
    procedure cmbYClickReferenceChange(Sender: TObject);
    procedure lbeClickXOffsetChange(Sender: TObject);
    procedure lbeClickYOffsetChange(Sender: TObject);
    procedure lbeMultiClickCountChange(Sender: TObject);
    procedure chkClickWithCtrlClick(Sender: TObject);
    procedure chkClickWithAltClick(Sender: TObject);
    procedure chkClickWithShiftClick(Sender: TObject);
    procedure rdgrpMouseButtonClick(Sender: TObject);

    procedure lbeColorErrorChange(Sender: TObject);
    procedure lbeAllowedColorErrorCountChange(Sender: TObject);

    procedure lbeSearchRectLeftChange(Sender: TObject);
    procedure lbeSearchRectTopChange(Sender: TObject);
    procedure lbeSearchRectRightChange(Sender: TObject);
    procedure lbeSearchRectBottomChange(Sender: TObject);
    procedure lbeSearchRectLeftOffsetChange(Sender: TObject);
    procedure lbeSearchRectTopOffsetChange(Sender: TObject);
    procedure lbeSearchRectRightOffsetChange(Sender: TObject);
    procedure lbeSearchRectBottomOffsetChange(Sender: TObject);
    procedure rdgrpSearchForControlModeClick(Sender: TObject);
    procedure chkAllowToFailClick(Sender: TObject);
    procedure lbeSetNewTextChange(Sender: TObject);
    procedure rdgrpSetTextControlTypeClick(Sender: TObject);
    procedure lbeCallOnlyIfContitionVarNameChange(Sender: TObject);
    procedure lbeCallOnlyIfContitionVarValueChange(Sender: TObject);
    procedure lbeActionNameChange(Sender: TObject);
    procedure cmbActionsChange(Sender: TObject);
    procedure lbeActionTimeoutChange(Sender: TObject);
    procedure spdbtnClearClick(Sender: TObject);
    procedure vallstVariablesValidate(Sender: TObject; ACol, ARow: Integer;
      const KeyName, KeyValue: string);
    procedure AddVariable1Click(Sender: TObject);
    procedure RemoveVariable1Click(Sender: TObject);
    procedure lbeClickVarXChange(Sender: TObject);
    procedure lbeClickVarYChange(Sender: TObject);
    procedure chkEvaluateVarsBeforeCallingClick(Sender: TObject);
    procedure TabSheetActionCallMouseLeave(Sender: TObject);
    procedure lbeMatchClassNameChange(Sender: TObject);
    procedure lbeMatchTextSeparatorChange(Sender: TObject);
    procedure lbeMatchClassNameSeparatorChange(Sender: TObject);
    procedure lbeSleepChange(Sender: TObject);
    procedure vstCustomVariablesDblClick(Sender: TObject);
    procedure vstCustomVariablesEdited(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vstCustomVariablesEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstCustomVariablesGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure vstCustomVariablesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure vstCustomVariablesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vstCustomVariablesNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
    procedure vstSetVarChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstSetVarChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
    procedure vstSetVarDblClick(Sender: TObject);
    procedure vstSetVarEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure vstSetVarEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure vstSetVarGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstSetVarGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vstSetVarInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstSetVarKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure vstSetVarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vstSetVarNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: String);
  private
    { Private declarations }
    FBMPsDir: string;
    FSetVarMouseUpHitInfo: THitInfo;
    FSetVarEditingText: string;
    FSetVarUpdatedVstText: Boolean;

    FCustomVarsMouseUpHitInfo: THitInfo;
    FCustomVarsEditingText: string;
    FCustomVarsUpdatedVstText: Boolean;

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

    FPmLocalTemplates: TPopupMenu;

    FOnCopyControlTextAndClassFromMainWindow: TOnCopyControlTextAndClassFromMainWindow;
    FOnGetExtraSearchAreaDebuggingImage: TOnGetExtraSearchAreaDebuggingImage;
    FOnEditCallTemplateBreakCondition: TOnEditActionCondition;

    FOnLoadBitmap: TOnLoadBitmap;
    FOnFileExists: TOnFileExists;

    FOnSetTemplateOpenDialogInitialDir: TOnSetTemplateOpenDialogInitialDir;
    FOnTemplateOpenDialogExecute: TOnTemplateOpenDialogExecute;
    FOnGetTemplateOpenDialogFileName: TOnGetTemplateOpenDialogFileName;
    FOnSetPictureOpenDialogInitialDir: TOnSetPictureOpenDialogInitialDir;
    FOnPictureOpenDialogExecute: TOnPictureOpenDialogExecute;
    FOnGetPictureOpenDialogFileName: TOnGetPictureOpenDialogFileName;

    //function GetListOfSetVarEntries: string;
    //procedure SetListOfSetVarEntries(Value: string);

    function GetListOfSetVars: TClkSetVarOptions;
    procedure SetListOfSetVars(Value: TClkSetVarOptions);

    function GetListOfCustomVariables: string;
    procedure SetListOfCustomVariables(Value: string);

    procedure SetShowDeprecatedControls(Value: Boolean);
    procedure CreateRemainingUIComponents;
    procedure SetDebuggingInfoAvailable(Value: Boolean);
    procedure TriggerOnControlsModified;

    function DoOnEditCallTemplateBreakCondition(var AActionCondition: string): Boolean;
    function DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function DoOnFileExists(const AFileName: string): Boolean;

    procedure DoOnSetTemplateOpenDialogInitialDir(AInitialDir: string);
    function DoOnTemplateOpenDialogExecute: Boolean;
    function DoOnGetTemplateOpenDialogFileName: string;
    procedure DoOnSetPictureOpenDialogInitialDir(AInitialDir: string);
    function DoOnPictureOpenDialogExecute: Boolean;
    function DoOnGetPictureOpenDialogFileName: string;

    procedure SetInMemFS(Value: TInMemFileSystem);

    procedure SetLabelsFromMouseOverExecDbgImgPixelColor(APixelColor: TColor);

    procedure LocalTemplatesClick(Sender: TObject);
    procedure ClickerConditionEditorControlsModified;
    procedure OverlapGridImgOnDebugImg(ADebugAndGridBitmap: TBitmap);

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
  public
    { Public declarations }
    frClickerConditionEditor: TfrClickerConditionEditor;  //public, because it is accessed from outside :(
    frClickerFindControl: TfrClickerFindControl;

    FgrpMouseDragControls: TGroupBox;
    FlblXClickReferenceDest: TLabel;
    FcmbXClickReferenceDest: TComboBox;
    FlbeClickVarXDest: TLabeledEdit;
    FlbeClickXOffsetDest: TLabeledEdit;
    FlblYClickReferenceDest: TLabel;
    FcmbYClickReferenceDest: TComboBox;
    FlbeClickVarYDest: TLabeledEdit;
    FlbeClickYOffsetDest: TLabeledEdit;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function EvaluateReplacements(VarName: string; Recursive: Boolean = True): string;

    procedure LoadListOfAvailableTemplates;
    procedure SetDebugVariablesFromListOfStrings(AListOfStrings: string);
    procedure UpdatePageControlActionsHighlighting;
    procedure UpdatePageControlActionExecutionIcons;
    procedure UpdateControlWidthHeightLabels;
    procedure UpdatePageControlActionsOnFindControlTab;

    procedure RefreshClickVarXEnabledState;
    procedure RefreshClickVarYEnabledState;

    procedure ClearControls;

    property BMPsDir: string read FBMPsDir write FBMPsDir;  /////////////////////////// to be removed
    property ControlsModified: Boolean read FControlsModified write FControlsModified;
    property OnControlsModified: TNotifyEvent read FOnControlsModified write FOnControlsModified;
    property PredefinedVarCount: Integer read FPredefinedVarCount write FPredefinedVarCount;
    property DebuggingInfoAvailable: Boolean write SetDebuggingInfoAvailable;
    property FullTemplatesDir: string read FFullTemplatesDir write FFullTemplatesDir;  //no trailing backslash
    //property ListOfSetVarEntries: string read GetListOfSetVarEntries write SetListOfSetVarEntries;
    property ListOfSetVars: TClkSetVarOptions read GetListOfSetVars write SetListOfSetVars;
    property ShowDeprecatedControls: Boolean read FShowDeprecatedControls write SetShowDeprecatedControls;

    property ListOfCustomVariables: string read GetListOfCustomVariables write SetListOfCustomVariables;
    property InMemFS: TInMemFileSystem write SetInMemFS;

    property OnCopyControlTextAndClassFromMainWindow: TOnCopyControlTextAndClassFromMainWindow read FOnCopyControlTextAndClassFromMainWindow write FOnCopyControlTextAndClassFromMainWindow;
    property OnGetExtraSearchAreaDebuggingImage: TOnGetExtraSearchAreaDebuggingImage write FOnGetExtraSearchAreaDebuggingImage;
    property OnEditCallTemplateBreakCondition: TOnEditActionCondition write FOnEditCallTemplateBreakCondition;

    property OnLoadBitmap: TOnLoadBitmap write FOnLoadBitmap;
    property OnFileExists: TOnFileExists write FOnFileExists;

    property OnSetTemplateOpenDialogInitialDir: TOnSetTemplateOpenDialogInitialDir write FOnSetTemplateOpenDialogInitialDir;
    property OnTemplateOpenDialogExecute: TOnTemplateOpenDialogExecute write FOnTemplateOpenDialogExecute;
    property OnGetTemplateOpenDialogFileName: TOnGetTemplateOpenDialogFileName write FOnGetTemplateOpenDialogFileName;
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
  Clipbrd, MouseStuff;


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
  frClickerFindControl.Parent := TabSheetActionFindSubControl;

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

  frClickerConditionEditor := TfrClickerConditionEditor.Create(Self);
  frClickerConditionEditor.Parent := pnlActionConditions; //for some reason, using TabSheetCondition leads to a hidden frame

  frClickerConditionEditor.Left := 3;
  frClickerConditionEditor.Top := 3;
  frClickerConditionEditor.Width := pnlActionConditions.Width - 3;
  frClickerConditionEditor.Height := pnlActionConditions.Height - 12;
  frClickerConditionEditor.Anchors := [akBottom, akLeft, akRight, akTop];
  frClickerConditionEditor.OnControlsModified := ClickerConditionEditorControlsModified;
  frClickerConditionEditor.Visible := True;

  FPmLocalTemplates := TPopupMenu.Create(Self);

  //mouse drag controls
  FgrpMouseDragControls := TGroupBox.Create(Self);
  FgrpMouseDragControls.Parent := TabSheetActionClick;
  FgrpMouseDragControls.Left := 0;
  FgrpMouseDragControls.Top := cmbClickType.Top + cmbClickType.Height + 8;
  FgrpMouseDragControls.Width := lbeMultiClickCount.Left;
  FgrpMouseDragControls.Height := chkClickWithCtrl.Top + 20;
  FgrpMouseDragControls.Name := 'grpMouseDragControls';
  FgrpMouseDragControls.Caption := 'Mouse drag - destination point';
  FgrpMouseDragControls.Visible := False;
  FgrpMouseDragControls.Color := clCream; //does nothing on a themed application

  FlblXClickReferenceDest := TLabel.Create(Self);
  FlblXClickReferenceDest.Parent := FgrpMouseDragControls;
  FlblXClickReferenceDest.Left := lblXClickReference.Left;
  FlblXClickReferenceDest.Top := lblXClickReference.Top;
  FlblXClickReferenceDest.Caption := lblXClickReference.Caption;

  FcmbXClickReferenceDest := TComboBox.Create(Self);
  FcmbXClickReferenceDest.Parent := FgrpMouseDragControls;
  FcmbXClickReferenceDest.Style := csOwnerDrawFixed;
  FcmbXClickReferenceDest.Left := cmbXClickReference.Left;
  FcmbXClickReferenceDest.Top := cmbXClickReference.Top;
  FcmbXClickReferenceDest.Width := cmbXClickReference.Width;
  FcmbXClickReferenceDest.Items.Text := cmbXClickReference.Items.Text;
  FcmbXClickReferenceDest.Hint := cmbXClickReference.Hint;
  FcmbXClickReferenceDest.ShowHint := cmbXClickReference.ShowHint;
  FcmbXClickReferenceDest.OnChange := cmbXClickReference.OnChange;
  FcmbXClickReferenceDest.ItemIndex := 0;

  FlbeClickVarXDest := TLabeledEdit.Create(Self);
  FlbeClickVarXDest.Parent := FgrpMouseDragControls;
  FlbeClickVarXDest.Left := lbeClickVarX.Left;
  FlbeClickVarXDest.Top := lbeClickVarX.Top;
  FlbeClickVarXDest.Width := lbeClickVarX.Width;
  FlbeClickVarXDest.EditLabel.Caption := lbeClickVarX.EditLabel.Caption;
  FlbeClickVarXDest.Hint := lbeClickVarX.Hint;
  FlbeClickVarXDest.ShowHint := lbeClickVarX.ShowHint;
  FlbeClickVarXDest.Text := lbeClickVarX.Text;
  FlbeClickVarXDest.OnChange := lbeClickVarX.OnChange;

  FlbeClickXOffsetDest := TLabeledEdit.Create(Self);
  FlbeClickXOffsetDest.Parent := FgrpMouseDragControls;
  FlbeClickXOffsetDest.Left := lbeClickXOffset.Left;
  FlbeClickXOffsetDest.Top := lbeClickXOffset.Top;
  FlbeClickXOffsetDest.Width := lbeClickXOffset.Width;
  FlbeClickXOffsetDest.EditLabel.Caption := lbeClickXOffset.EditLabel.Caption;
  FlbeClickXOffsetDest.Hint := lbeClickXOffset.Hint;
  FlbeClickXOffsetDest.ShowHint := lbeClickXOffset.ShowHint;
  FlbeClickXOffsetDest.Text := lbeClickXOffset.Text;
  FlbeClickXOffsetDest.OnChange := lbeClickXOffset.OnChange;

  FlblYClickReferenceDest := TLabel.Create(Self);
  FlblYClickReferenceDest.Parent := FgrpMouseDragControls;
  FlblYClickReferenceDest.Left := lblYClickReference.Left;
  FlblYClickReferenceDest.Top := lblYClickReference.Top;
  FlblYClickReferenceDest.Caption := lblYClickReference.Caption;

  FcmbYClickReferenceDest := TComboBox.Create(Self);
  FcmbYClickReferenceDest.Parent := FgrpMouseDragControls;
  FcmbYClickReferenceDest.Style := csOwnerDrawFixed;
  FcmbYClickReferenceDest.Left := cmbYClickReference.Left;
  FcmbYClickReferenceDest.Top := cmbYClickReference.Top;
  FcmbYClickReferenceDest.Width := cmbYClickReference.Width;
  FcmbYClickReferenceDest.Items.Text := cmbYClickReference.Items.Text;
  FcmbYClickReferenceDest.Hint := cmbYClickReference.Hint;
  FcmbYClickReferenceDest.ShowHint := cmbYClickReference.ShowHint;
  FcmbYClickReferenceDest.OnChange := cmbYClickReference.OnChange;
  FcmbYClickReferenceDest.ItemIndex := 0;

  FlbeClickVarYDest := TLabeledEdit.Create(Self);
  FlbeClickVarYDest.Parent := FgrpMouseDragControls;
  FlbeClickVarYDest.Left := lbeClickVarY.Left;
  FlbeClickVarYDest.Top := lbeClickVarY.Top;
  FlbeClickVarYDest.Width := lbeClickVarY.Width;
  FlbeClickVarYDest.EditLabel.Caption := lbeClickVarY.EditLabel.Caption;
  FlbeClickVarYDest.Hint := lbeClickVarY.Hint;
  FlbeClickVarYDest.ShowHint := lbeClickVarY.ShowHint;
  FlbeClickVarYDest.Text := lbeClickVarY.Text;
  FlbeClickVarYDest.OnChange := lbeClickVarY.OnChange;

  FlbeClickYOffsetDest := TLabeledEdit.Create(Self);
  FlbeClickYOffsetDest.Parent := FgrpMouseDragControls;
  FlbeClickYOffsetDest.Left := lbeClickYOffset.Left;
  FlbeClickYOffsetDest.Top := lbeClickYOffset.Top;
  FlbeClickYOffsetDest.Width := lbeClickYOffset.Width;
  FlbeClickYOffsetDest.EditLabel.Caption := lbeClickYOffset.EditLabel.Caption;
  FlbeClickYOffsetDest.Hint := lbeClickYOffset.Hint;
  FlbeClickYOffsetDest.ShowHint := lbeClickYOffset.ShowHint;
  FlbeClickYOffsetDest.Text := lbeClickYOffset.Text;
  FlbeClickYOffsetDest.OnChange := lbeClickYOffset.OnChange;

  lbeClickVarX.Enabled := False;
  lbeClickVarY.Enabled := False;
  FlbeClickVarXDest.Enabled := False;
  FlbeClickVarYDest.Enabled := False;
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

  FOnCopyControlTextAndClassFromMainWindow := nil;
  FOnGetExtraSearchAreaDebuggingImage := nil;
  FOnEditCallTemplateBreakCondition := nil;

  FOnLoadBitmap := nil;
  FOnFileExists := nil;

  FOnSetTemplateOpenDialogInitialDir := nil;
  FOnTemplateOpenDialogExecute := nil;
  FOnGetTemplateOpenDialogFileName := nil;
  FOnSetPictureOpenDialogInitialDir := nil;
  FOnPictureOpenDialogExecute := nil;
  FOnGetPictureOpenDialogFileName := nil;

  FShowDeprecatedControls := False;

  PageControlActions.ActivePageIndex := 0;
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


procedure TfrClickerActions.rdgrpMouseButtonClick(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.rdgrpSearchForControlModeClick(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.rdgrpSetTextControlTypeClick(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.spdbtnClearClick(Sender: TObject);
begin
  ClearControls;
end;


procedure TfrClickerActions.spdbtnCommonTimeoutsClick(Sender: TObject);
var
  tp: TPoint;
begin
  GetCursorPos(tp);
  pmCommonTimeouts.Popup(tp.X, tp.Y);
end;


procedure TfrClickerActions.TabSheetActionCallMouseLeave(Sender: TObject);
{$IFnDEF FPC}
  var
    i: Integer;
{$ENDIF}
begin
  {$IFDEF FPC}
    if vallstCustomVariables.Focused then  ///////////////////////////////////// ToDo  search for a proper editor (if available)
      chkEvaluateVarsBeforeCalling.SetFocus; //make vallst remove focus
  {$ELSE}
    for i := 0 to vallstCustomVariables.ControlCount - 1 do
      if (vallstCustomVariables.Controls[i] is TInplaceEditList) then    //TInplaceEditList is the internal editbox, used for inputting new values
        if (vallstCustomVariables.Controls[i] as TInplaceEditList).Focused then
          chkEvaluateVarsBeforeCalling.SetFocus; //make vallst remove focus
  {$ENDIF}
end;


procedure TfrClickerActions.AddCustomVarRow1Click(Sender: TObject);
begin
  vallstCustomVariables.Strings.Add('');
  vstCustomVariables.RootNodeCount := vallstCustomVariables.Strings.Count;
  vstCustomVariables.Repaint;

  vstCustomVariables.Selected[vstCustomVariables.GetLast] := True;

  TriggerOnControlsModified;
end;


procedure TfrClickerActions.RemoveCustomVarRow1Click(Sender: TObject);
var
  Node: PVirtualNode;
begin
  try
    //if MessageBox(Handle, PChar('Remove variable?' + #13#10 + vallstCustomVariables.Strings[vallstCustomVariables.Selection.Top - 1]), 'Selection', MB_ICONQUESTION + MB_YESNO) = IDNO then
    //  Exit;

    Node := vstCustomVariables.GetFirstSelected;

    if Node = nil then
    begin
      MessageBox(Handle, 'Please select a variable to be deleted.', PChar(Application.Title), MB_ICONINFORMATION);
      Exit;
    end;

    if MessageBox(Handle, PChar('Remove variable?' + #13#10 + vallstCustomVariables.Strings[Node^.Index]), 'Selection', MB_ICONQUESTION + MB_YESNO) = IDNO then
      Exit;

    //vallstCustomVariables.Strings.Delete(vallstCustomVariables.Selection.Top - 1);
    vallstCustomVariables.Strings.Delete(Node^.Index);

    vstCustomVariables.RootNodeCount := vallstCustomVariables.Strings.Count;
    vstCustomVariables.Repaint;
    TriggerOnControlsModified;
  except
  end;
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


procedure TfrClickerActions.chkAllowToFailClick(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.chkCallOnlyIfContitionIsTrueClick(Sender: TObject);
begin
  lbeCallOnlyIfContitionVarName.Enabled := chkCallOnlyIfContitionIsTrue.Checked;
  lbeCallOnlyIfContitionVarValue.Enabled := chkCallOnlyIfContitionIsTrue.Checked;
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.chkClickWithAltClick(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.chkClickWithCtrlClick(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.chkClickWithShiftClick(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.chkEvaluateVarsBeforeCallingClick(Sender: TObject);
begin
  TriggerOnControlsModified;
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


procedure TfrClickerActions.cmbActionsChange(Sender: TObject);
begin
  TriggerOnControlsModified;
  UpdatePageControlActionsHighlighting;
end;


procedure TfrClickerActions.RefreshClickVarXEnabledState;
begin
  lbeClickVarX.Enabled := (cmbXClickReference.ItemIndex > -1) and (cmbXClickReference.Items.Strings[cmbXClickReference.ItemIndex] = 'Var/Replacement');
  FlbeClickVarXDest.Enabled := (FcmbXClickReferenceDest.ItemIndex > -1) and (FcmbXClickReferenceDest.Items.Strings[FcmbXClickReferenceDest.ItemIndex] = 'Var/Replacement');
end;


procedure TfrClickerActions.RefreshClickVarYEnabledState;
begin
  lbeClickVarY.Enabled := (cmbYClickReference.ItemIndex > -1) and (cmbYClickReference.Items.Strings[cmbYClickReference.ItemIndex] = 'Var/Replacement');
  FlbeClickVarYDest.Enabled := (FcmbYClickReferenceDest.ItemIndex > -1) and (FcmbYClickReferenceDest.Items.Strings[FcmbYClickReferenceDest.ItemIndex] = 'Var/Replacement');
end;


procedure TfrClickerActions.cmbXClickReferenceChange(Sender: TObject);
begin
  TriggerOnControlsModified;
  RefreshClickVarXEnabledState;
end;


procedure TfrClickerActions.cmbYClickReferenceChange(Sender: TObject);
begin
  TriggerOnControlsModified;
  RefreshClickVarYEnabledState;
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


procedure TfrClickerActions.lbeMultiClickCountChange(Sender: TObject);
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


procedure TfrClickerActions.lbeSetNewTextChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeSleepChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.vstCustomVariablesDblClick(Sender: TObject);
begin
  tmrEditCustomVars.Enabled := True;
end;


procedure TfrClickerActions.vstCustomVariablesEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  NewLine: string;
begin
  if FCustomVarsMouseUpHitInfo.HitNode = nil then
    Exit;

  if not FCustomVarsUpdatedVstText then
    Exit;

  case Column of
    0:
    begin
      if Trim(FCustomVarsEditingText) = '' then
      begin
        MessageBox(Handle, 'Variable name must not be empty.', PChar(Application.Title), MB_ICONERROR);
        Exit;
      end;

      if (FCustomVarsEditingText[1] <> '$') or (FCustomVarsEditingText[Length(FCustomVarsEditingText)] <> '$') then
      begin
        MessageBox(Handle, 'Variable name must be enclosed by two "$" characters. E.g. "$my_var$" (without double quotes).', PChar(Application.Title), MB_ICONERROR);
        Exit;
      end;

      NewLine := FCustomVarsEditingText + '=' + vallstCustomVariables.Strings.ValueFromIndex[Node^.Index];

      if vallstCustomVariables.Strings.Strings[Node^.Index] <> NewLine then
      begin
        vallstCustomVariables.Strings.Strings[Node^.Index] := NewLine;
        TriggerOnControlsModified;
      end;
    end;

    1:
    begin
      if vallstCustomVariables.Strings.ValueFromIndex[Node^.Index] <> FCustomVarsEditingText then
      begin
        vallstCustomVariables.Strings.ValueFromIndex[Node^.Index] := FCustomVarsEditingText;
        TriggerOnControlsModified;
      end;
    end;
  end;
end;


procedure TfrClickerActions.vstCustomVariablesEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Column < 2;
  FCustomVarsUpdatedVstText := False;
end;


procedure TfrClickerActions.vstCustomVariablesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  try
    case Column of
      0: CellText := vallstCustomVariables.Strings.Names[Node^.Index];
      1: CellText := vallstCustomVariables.Strings.ValueFromIndex[Node^.Index];
    end;
  except
    CellText := 'bug';
  end;
end;


procedure TfrClickerActions.vstCustomVariablesKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    RemoveCustomVarRow1Click(RemoveCustomVarRow1);
end;


procedure TfrClickerActions.vstCustomVariablesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  vstCustomVariables.GetHitTestInfoAt(X, Y, True, FCustomVarsMouseUpHitInfo);
end;


procedure TfrClickerActions.vstCustomVariablesNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
begin
  FCustomVarsEditingText := FastReplace_ReturnTo68(NewText);
  FCustomVarsUpdatedVstText := True;
end;


procedure TfrClickerActions.vstSetVarChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  FSetVarContent_EvalBefore.Strings[Node^.Index] := IntToStr(Ord(Node^.CheckState = csCheckedNormal));
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.vstSetVarChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin
  Allowed := True;
end;


procedure TfrClickerActions.vstSetVarDblClick(Sender: TObject);
begin
  tmrEditSetVars.Enabled := True;
end;


//function TfrClickerActions.GetListOfSetVarEntries: string;
//var
//  i: Integer;
//begin
//  Result := '';
//  for i := 0 to FSetVarContent_Vars.Count - 1 do
//    Result := Result + FSetVarContent_Vars.Strings[i] + '=' + FSetVarContent_Values.Strings[i] + #13#10;
//end;
//
//
//procedure TfrClickerActions.SetListOfSetVarEntries(Value: string);
//var
//  i: Integer;
//  TempList: TStringList;
//  s: string;
//begin
//  FSetVarContent_Vars.Clear;
//  FSetVarContent_Values.Clear;
//
//  TempList := TStringList.Create;
//  try
//    TempList.Text := Value;
//    for i := 0 to TempList.Count - 1 do
//    begin
//      s := TempList.Strings[i];
//
//      FSetVarContent_Vars.Add(Copy(s, 1, Pos('=', s) - 1));
//      FSetVarContent_Values.Add(Copy(s, Pos('=', s) + 1, MaxInt));
//    end;
//
//    //if Integer(vstSetVar.RootNodeCount) <> TempList.Count then   //see below
//    begin
//      vstSetVar.RootNodeCount := TempList.Count;
//      vstSetVar.Repaint;
//      TriggerOnControlsModified;
//    end;
//  finally
//    TempList.Free;
//  end;
//end;


function TfrClickerActions.GetListOfSetVars: TClkSetVarOptions;
begin
  Result.ListOfVarNames := FSetVarContent_Vars.Text;
  Result.ListOfVarValues := FSetVarContent_Values.Text;
  Result.ListOfVarEvalBefore := FSetVarContent_EvalBefore.Text;
end;


procedure TfrClickerActions.SetListOfSetVars(Value: TClkSetVarOptions);
const
  CNodeCheckState: array[Boolean] of TCheckState = (csUncheckedNormal, csCheckedNormal);
var
  Node: PVirtualNode;
begin
  FSetVarContent_Vars.Text := Value.ListOfVarNames;
  FSetVarContent_Values.Text := Value.ListOfVarValues;
  FSetVarContent_EvalBefore.Text := Value.ListOfVarEvalBefore;

  //if Integer(vstSetVar.RootNodeCount) <> FSetVarContent_Vars.Count then  //Leave this commented! The new list might have the same length (with different content), so refresh the vst.
  begin
    vstSetVar.RootNodeCount := FSetVarContent_Vars.Count;

    if vstSetVar.RootNodeCount > 0 then
    begin
      Node := vstSetVar.GetFirst;
      repeat
        vstSetVar.CheckState[Node] := CNodeCheckState[FSetVarContent_EvalBefore.Strings[Node^.Index] = '1'];
        Node := Node^.NextSibling;
      until Node = nil;
    end;

    vstSetVar.Repaint;
    TriggerOnControlsModified;
  end;
end;


function TfrClickerActions.GetListOfCustomVariables: string;
begin
  Result := vallstCustomVariables.Strings.Text;
end;


procedure TfrClickerActions.SetListOfCustomVariables(Value: string);
begin
  //if vallstCustomVariables.Strings.Text <> Value then  //For some reason, this has to stay commented. Otherwise, it won't update the tree.
  begin
    vallstCustomVariables.Strings.Text := Value;
    vstCustomVariables.RootNodeCount := vallstCustomVariables.Strings.Count;
    vstCustomVariables.Repaint;
  end;
end;


procedure TfrClickerActions.SetShowDeprecatedControls(Value: Boolean);
begin
  FShowDeprecatedControls := Value;
  lblDeprecatedCondition.Visible := Value;
  chkCallOnlyIfContitionIsTrue.Visible := Value;
  lbeCallOnlyIfContitionVarName.Visible := Value;
  lblConditionEquals.Visible := Value;
  lbeCallOnlyIfContitionVarValue.Visible := Value;
end;


procedure TfrClickerActions.vstSetVarEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if FSetVarMouseUpHitInfo.HitNode = nil then
    Exit;

  if not FSetVarUpdatedVstText then
    Exit;

  case Column of
    0:
    begin
      //nothing here
    end;

    1:
    begin
      if (Trim(FSetVarEditingText) = '') or
         (Length(FSetVarEditingText) < 3) or
         (FSetVarEditingText[1] <> '$') or
         (FSetVarEditingText[Length(FSetVarEditingText)] <> '$') then
      begin
        MessageBox(Handle, 'The variable name must have the following format: "$<varname>$" .', PChar(Application.Title), MB_ICONINFORMATION);
        Exit;
      end;

      FSetVarContent_Vars.Strings[Node^.Index] := FSetVarEditingText;
      TriggerOnControlsModified;
    end;

    2:
    begin
      FSetVarContent_Values.Strings[Node^.Index] := FSetVarEditingText;
      TriggerOnControlsModified;
    end;

    else
      MessageBox(Handle, 'Editing wrong column (bug)', PChar(Application.Title), MB_ICONERROR);
  end;
end;


procedure TfrClickerActions.vstSetVarEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Column in [1, 2];
  FSetVarUpdatedVstText := False;
end;


procedure TfrClickerActions.vstSetVarGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if Column = 0 then
    ImageIndex := 0;
end;


procedure TfrClickerActions.vstSetVarGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
const
  CEvalState: array[Boolean] of string = ('No', 'Yes');
begin
  try
    case Column of
      0: CellText := CEvalState[FSetVarContent_EvalBefore.Strings[Node^.Index] = '1'];
      1: CellText := FSetVarContent_Vars.Strings[Node^.Index];
      2: CellText := FSetVarContent_Values.Strings[Node^.Index];
    end;
  except
    on E: Exception do
      CellText := 'bug: ' + E.Message;
  end;
end;


procedure TfrClickerActions.vstSetVarInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Node^.CheckType := ctCheckBox;
  Node^.CheckState := csUncheckedNormal;
end;


procedure TfrClickerActions.vstSetVarKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    MenuItem_RemoveSetVarClick(MenuItem_RemoveSetVar);
end;


procedure TfrClickerActions.vstSetVarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  vstSetVar.GetHitTestInfoAt(X, Y, True, FSetVarMouseUpHitInfo);
end;


procedure TfrClickerActions.vstSetVarNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
begin
  FSetVarEditingText := NewText;
  FSetVarUpdatedVstText := True;
end;


procedure TfrClickerActions.N01Click(Sender: TObject);
begin
  lbeActionTimeout.Text := (Sender as TMenuItem).Caption;
  lbeActionTimeout.Text := StringReplace(lbeActionTimeout.Text, '&', '', [rfReplaceAll]);
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeActionNameChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeActionTimeoutChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeAllowedColorErrorCountChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeCallOnlyIfContitionVarNameChange(
  Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeCallOnlyIfContitionVarValueChange(
  Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeClickVarXChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeClickVarYChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeClickXOffsetChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeClickYOffsetChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeColorErrorChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeExecAppPathChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeMatchClassNameChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeMatchClassNameSeparatorChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.btnSelectNoTemplateClick(Sender: TObject);
begin
  lbeTemplateFileName.Text := '';
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.btnBrowseOtherTemplatesClick(Sender: TObject);
begin
  DoOnSetTemplateOpenDialogInitialDir(ExtractFileDir(lbeTemplateFileName.Text));

  if DoOnTemplateOpenDialogExecute then
    lbeTemplateFileName.Text := DoOnGetTemplateOpenDialogFileName;
end;


procedure TfrClickerActions.btnBrowseCallTemplateLoopBreakConditionClick(
  Sender: TObject);
var
  Condition: string;
begin
  Condition := lbeCallTemplateLoopBreakCondition.Text;
  if DoOnEditCallTemplateBreakCondition(Condition) then
    lbeCallTemplateLoopBreakCondition.Text := Condition;
end;


procedure TfrClickerActions.btnSetFromControlLeftAndTopClick(Sender: TObject);
begin
  lbeWindowOperationsX.Text := EvaluateReplacements('$Control_Left$');
  lbeWindowOperationsY.Text := EvaluateReplacements('$Control_Top$');
end;


procedure TfrClickerActions.btnSetFromControlWidthAndHeightClick(Sender: TObject);
begin
  lbeWindowOperationsWidth.Text := EvaluateReplacements('$Control_Width$');
  lbeWindowOperationsHeight.Text := EvaluateReplacements('$Control_Height$');
end;


procedure TfrClickerActions.SetLabelsFromMouseOverExecDbgImgPixelColor(APixelColor: TColor);
begin
  lblMouseOnExecDbgImgRR.Caption := IntToHex(APixelColor and $FF, 2);
  lblMouseOnExecDbgImgGG.Caption := IntToHex(APixelColor shr 8 and $FF, 2);
  lblMouseOnExecDbgImgBB.Caption := IntToHex(APixelColor shr 16 and $FF, 2);
end;


procedure TfrClickerActions.chkAllowToFailChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;

procedure TfrClickerActions.chkCallTemplateLoopEnabledChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.chkClickWithDoubleClickChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.chkLeaveMouseChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.chkMoveWithoutClickChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;

procedure TfrClickerActions.chkNoConsoleChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.chkWaitForAppChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.chkWaitForControlToGoAwayChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.chkWindowOperationsEnablePosChange(Sender: TObject);
begin
  TriggerOnControlsModified;

  lbeWindowOperationsX.Enabled := (cmbWindowOperationsType.ItemIndex = Integer(Ord(woMoveResize))) and chkWindowOperationsEnablePos.Checked;
  lbeWindowOperationsY.Enabled := (cmbWindowOperationsType.ItemIndex = Integer(Ord(woMoveResize))) and chkWindowOperationsEnablePos.Checked;
end;


procedure TfrClickerActions.chkWindowOperationsEnableSizeChange(Sender: TObject);
begin
  TriggerOnControlsModified;

  lbeWindowOperationsWidth.Enabled := (cmbWindowOperationsType.ItemIndex = Integer(Ord(woMoveResize))) and chkWindowOperationsEnableSize.Checked;
  lbeWindowOperationsHeight.Enabled := (cmbWindowOperationsType.ItemIndex = Integer(Ord(woMoveResize))) and chkWindowOperationsEnableSize.Checked;
end;


procedure TfrClickerActions.cmbCallTemplateLoopBreakPositionChange(
  Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.cmbCallTemplateLoopDirectionChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.cmbClickTypeChange(Sender: TObject);
begin
  if Assigned(FgrpMouseDragControls) then
    FgrpMouseDragControls.Visible := cmbClickType.ItemIndex = 1;

  TriggerOnControlsModified;
end;


procedure TfrClickerActions.cmbUseInheritHandlesChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.cmbWindowOperationsTypeChange(Sender: TObject);
begin
  TriggerOnControlsModified;

  lbeWindowOperationsX.Enabled := (cmbWindowOperationsType.ItemIndex = Integer(Ord(woMoveResize))) and chkWindowOperationsEnablePos.Checked;
  lbeWindowOperationsY.Enabled := (cmbWindowOperationsType.ItemIndex = Integer(Ord(woMoveResize))) and chkWindowOperationsEnablePos.Checked;
  lbeWindowOperationsWidth.Enabled := (cmbWindowOperationsType.ItemIndex = Integer(Ord(woMoveResize))) and chkWindowOperationsEnableSize.Checked;
  lbeWindowOperationsHeight.Enabled := (cmbWindowOperationsType.ItemIndex = Integer(Ord(woMoveResize))) and chkWindowOperationsEnableSize.Checked;
end;


procedure TfrClickerActions.lbeCallTemplateLoopBreakConditionChange(
  Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeCallTemplateLoopCounterChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeCallTemplateLoopEndValueChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeCallTemplateLoopInitValueChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeExecAppCurrentDirChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeExecAppStdInChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.MenuItem_AddSetVarClick(Sender: TObject);
begin
  FSetVarContent_Vars.Add('');
  FSetVarContent_Values.Add('');
  FSetVarContent_EvalBefore.Add('');

  vstSetVar.RootNodeCount := FSetVarContent_Vars.Count;
  vstSetVar.Repaint;
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.MenuItem_RemoveSetVarClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstSetVar.GetFirstSelected;
  if Node = nil then
  begin
    MessageBox(Handle, 'Please select an item to be removed.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  if MessageBox(Handle, 'Are you sure you want to remove the selected item?', PChar(Caption), MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  FSetVarContent_Vars.Delete(Node^.Index);
  FSetVarContent_Values.Delete(Node^.Index);
  FSetVarContent_EvalBefore.Delete(Node^.Index);

  vstSetVar.RootNodeCount := FSetVarContent_Vars.Count;
  vstSetVar.Repaint;
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.UpdatePageControlActionsOnFindControlTab;
begin
  case PageControlActions.ActivePageIndex of
    2: frClickerFindControl.Parent := TabSheetActionFindControl;
    3: frClickerFindControl.Parent := TabSheetActionFindSubControl;
  end;
end;


procedure TfrClickerActions.PageControlActionsChange(Sender: TObject);
begin
  UpdatePageControlActionsOnFindControlTab;
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


procedure TfrClickerActions.spdbtnBrowseLocalTemplatesClick(Sender: TObject);
var
  tp: TPoint;
begin
  LoadListOfAvailableTemplates;
  GetCursorPos(tp);
  FPmLocalTemplates.PopUp(tp.X, tp.Y);
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

procedure TfrClickerActions.tmrEditCustomVarsTimer(Sender: TObject);
begin
  tmrEditCustomVars.Enabled := False;

  if FCustomVarsMouseUpHitInfo.HitNode = nil then
    Exit;

  vstCustomVariables.EditNode(FCustomVarsMouseUpHitInfo.HitNode, FCustomVarsMouseUpHitInfo.HitColumn);
end;


procedure TfrClickerActions.tmrEditSetVarsTimer(Sender: TObject);
begin
  tmrEditSetVars.Enabled := False;

  if FSetVarMouseUpHitInfo.HitNode = nil then
    Exit;

  vstSetVar.EditNode(FSetVarMouseUpHitInfo.HitNode, FSetVarMouseUpHitInfo.HitColumn);
end;


procedure TfrClickerActions.vallstCustomVariablesExit(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeTemplateFileNameChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeFindCachedControlLeftChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeFindCachedControlTopChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeWindowOperationsXChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeWindowOperationsYChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeWindowOperationsWidthChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.lbeWindowOperationsHeightChange(Sender: TObject);
begin
  TriggerOnControlsModified;
end;


procedure TfrClickerActions.btnBrowseExecAppClick(Sender: TObject);
var
  AOpenDialog: TOpenDialog;
begin
  AOpenDialog := TOpenDialog.Create(nil);
  try
    AOpenDialog.InitialDir := ExtractFileDir(ParamStr(0));

    if AOpenDialog.Execute then
    begin
      lbeExecAppPath.Text := AOpenDialog.FileName;
      TriggerOnControlsModified;
    end;
  finally
    AOpenDialog.Free;
  end;
end;


procedure TfrClickerActions.vallstCustomVariablesSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  {if ACol = 0 then
  begin
    if Value > '' then
      if (Length(Value) < 2) or (Value[1] <> '$') or (Value[Length(Value)] <> '$') then
        raise Exception.Create('Variable name must be enclosed by two "$" characters. E.g. "$my_var$" (without double quotes).');
  end;}

  (*
  {$IFnDEF FPC}  //this handler executes AFTER modifying the content in FPC, which is different than Delphi, where it executes BEFORE
    if vallstCustomVariables.Cells[ACol, ARow] <> Value then
  {$ENDIF}
      TriggerOnControlsModified;
  *)
end;


procedure TfrClickerActions.vallstCustomVariablesValidate(Sender: TObject; ACol,
  ARow: Integer; const KeyName, KeyValue: string);
begin
  if ACol = 0 then
  begin
    if KeyName > '' then
      if {(Length(KeyName) < 2) or} (KeyName[1] <> '$') or (KeyName[Length(KeyName)] <> '$') then
        raise Exception.Create('Variable name must be enclosed by two "$" characters. E.g. "$my_var$" (without double quotes).');
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
begin
  lbeTemplateFileName.Text := StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]);
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


procedure TfrClickerActions.memExecAppParamsChange(Sender: TObject);
begin
  TriggerOnControlsModified;
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
var
  TempListOfVars: TClkSetVarOptions;
begin
  lbeActionName.Text := '';
  cmbActions.ItemIndex := -1;
  lbeActionTimeout.Text := '0';
  frClickerConditionEditor.ClearActionConditionPreview;
  UpdatePageControlActionsHighlighting;

  cmbXClickReference.ItemIndex := 0;
  cmbYClickReference.ItemIndex := 0;
  lbeClickXOffset.Text := '0';
  lbeClickYOffset.Text := '0';
  lbeClickVarX.Text := '$Control_Left$';
  lbeClickVarY.Text := '$Control_Top$';
  lbeClickVarX.Enabled := False;
  lbeClickVarY.Enabled := False;

  cmbClickType.ItemIndex := CMouseClickType_Click;
  FcmbXClickReferenceDest.ItemIndex := 0;
  FcmbYClickReferenceDest.ItemIndex := 0;
  FlbeClickXOffsetDest.Text := '0';
  FlbeClickYOffsetDest.Text := '0';
  FlbeClickVarXDest.Text := '$Control_Left$';
  FlbeClickVarYDest.Text := '$Control_Top$';
  FgrpMouseDragControls.Hide;
  FlbeClickVarXDest.Enabled := False;
  FlbeClickVarYDest.Enabled := False;
  
  rdgrpMouseButton.ItemIndex := Ord(mbLeft);
  chkClickWithCtrl.Checked := False;
  chkClickWithAlt.Checked := False;
  chkClickWithShift.Checked := False;
  chkClickWithDoubleClick.Checked := False;
  chkLeaveMouse.Checked := False;
  chkMoveWithoutClick.Checked := False;
  lbeMultiClickCount.Text := '1';

  lbeExecAppPath.Text := '';
  memExecAppParams.Text := '';
  chkWaitForApp.Checked := False;
  chkNoConsole.Checked := False;

  frClickerFindControl.ClearControls;

  lbeSetNewText.Text := '';

  lbeTemplateFileName.Text := '';
  chkCallOnlyIfContitionIsTrue.Checked := False;
  lbeCallOnlyIfContitionVarName.Enabled := chkCallOnlyIfContitionIsTrue.Checked;
  lbeCallOnlyIfContitionVarValue.Enabled := chkCallOnlyIfContitionIsTrue.Checked;
  vallstCustomVariables.Strings.Clear;
  chkEvaluateVarsBeforeCalling.Checked := False;
  chkCallTemplateLoopEnabled.Checked := False;
  lbeCallTemplateLoopCounter.Text := '';
  lbeCallTemplateLoopInitValue.Text := '';
  lbeCallTemplateLoopEndValue.Text := '';
  cmbCallTemplateLoopDirection.ItemIndex := 0;
  cmbCallTemplateLoopBreakPosition.ItemIndex := 0;
  lbeCallTemplateLoopBreakCondition.Text := '';

  TempListOfVars.ListOfVarNames := '';
  TempListOfVars.ListOfVarValues := '';
  ListOfSetVars := TempListOfVars;

  cmbWindowOperationsType.ItemIndex := 0;
  lbeWindowOperationsX.Text := '';
  lbeWindowOperationsX.Enabled := False;
  lbeWindowOperationsY.Text := '';
  lbeWindowOperationsY.Enabled := False;
  lbeWindowOperationsWidth.Text := '';
  lbeWindowOperationsWidth.Enabled := False;
  lbeWindowOperationsHeight.Text := '';
  lbeWindowOperationsHeight.Enabled := False;

  //clear dynamically created mouse controls

  UpdatePageControlActionExecutionIcons;
end;



procedure TfrClickerActions.UpdatePageControlActionExecutionIcons;
begin
  PageControlActionExecution.Pages[0].ImageIndex := 0 + 3 * Ord(cmbActions.ItemIndex > -1);
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


procedure TfrClickerActions.UpdatePageControlActionsHighlighting;
var
  i: Integer;
begin
  {$IFnDEF FPC}
    for i := 0 to PageControlActions.PageCount - 1 do
      PageControlActions.Pages[i].Highlighted := cmbActions.ItemIndex = i;  //available in Delphi only
  {$ENDIF}

  for i := 0 to PageControlActions.PageCount - 1 do
    if cmbActions.ItemIndex = i then
      PageControlActions.Pages[i].ImageIndex := i
    else
      PageControlActions.Pages[i].ImageIndex := i + 10;
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

end.
