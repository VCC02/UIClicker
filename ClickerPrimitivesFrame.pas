{
    Copyright (C) 2025 VCC
    creation date: Mar 2023
    initial release date: 11 Mar 2023

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


unit ClickerPrimitivesFrame;

{$H+}
{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, ExtCtrls, ObjectInspectorFrame,
  VirtualTrees, ImgList, Graphics, Menus, ComCtrls, StdCtrls, Types,
  ClickerUtils, ClickerPrimitiveUtils;

type
  TEditingPoint = class(TGraphicControl)
  protected
    procedure Paint; override;
  public
    property Left;
    property Top;
    property Width;
    property Height;
  end;

  TEditingPrimitivePoints = record
    OrderIndex: Integer; //for the current composition order
    SelectedPrimitiveIndex: Integer;   //used for FPrimitives[SelectedPrimitiveIndex].PrimitiveType;
    EditingPoints: array of TEditingPoint;
  end;

  { TfrClickerPrimitives }

  TfrClickerPrimitives = class(TFrame)
    chkShowPrimitiveEdges: TCheckBox;
    chkHighContrast: TCheckBox;
    imgPreviewColorUnderMouse: TImage;
    imglstFontColorProperties: TImageList;
    imglstPreviewPrimitives: TImageList;
    imglstPrimitivesEditorMenu: TImageList;
    imglstPrimitives: TImageList;
    imgFontColorBuffer: TImage;
    lblModified: TLabel;
    lblMouseOnPreviewImg: TLabel;
    lblMouseOnPreviewImgBB: TLabel;
    lblMouseOnPreviewImgGG: TLabel;
    lblMouseOnPreviewImgRR: TLabel;
    Separator2: TMenuItem;
    MenuItem_HighContrastOption2: TMenuItem;
    MenuItem_HighContrastOption1: TMenuItem;
    Separator1: TMenuItem;
    MenuItem_CopyColorUnderMouseCursor: TMenuItem;
    MenuItem_RepaintAllCompositionsFromStaticMenu: TMenuItem;
    N3: TMenuItem;
    MenuItem_EditMode: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    MenuItem_SavePrimitivesFile: TMenuItem;
    MenuItem_CopyToClipboard: TMenuItem;
    PageControlPreview: TPageControl;
    pnlHorizSplitter: TPanel;
    pnlPreview: TPanel;
    pnlvstOI: TPanel;
    pmPreview: TPopupMenu;
    tmrDrawZoom: TTimer;
    tmrReloadOIContent: TTimer;
    procedure chkHighContrastChange(Sender: TObject);
    procedure chkShowPrimitiveEdgesChange(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure MenuItem_CopyColorUnderMouseCursorClick(Sender: TObject);
    procedure MenuItem_CopyToClipboardClick(Sender: TObject);
    procedure MenuItem_EditModeClick(Sender: TObject);
    procedure MenuItem_HighContrastOption1Click(Sender: TObject);
    procedure MenuItem_HighContrastOption2Click(Sender: TObject);
    procedure MenuItem_RepaintAllCompositionsFromStaticMenuClick(Sender: TObject);
    procedure MenuItem_SavePrimitivesFileClick(Sender: TObject);
    procedure PageControlPreviewChange(Sender: TObject);
    procedure pnlHorizSplitterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlHorizSplitterMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlHorizSplitterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tmrDrawZoomTimer(Sender: TObject);
    procedure tmrReloadOIContentTimer(Sender: TObject);
  private
    FOIFrame: TfrObjectInspector;
    FPrimitives: TPrimitiveRecArr;
    FOrders: TCompositionOrderArr; //array of orders  - i.e. array of array of indexes.
    FPrimitiveSettings: TPrimitiveSettings;
    FCurrentMousePosOnPreviewImg: TPoint;
    FFileIndex: Integer; //primitives file index in the list of MatchPrimitiveFiles property
    FimgPreviewTpColor: TColor;
    FEditingPanels: array of TPanel; //not created, just pointers to editing panels from the tabs

    FHold: Boolean;
    FSplitterMouseDownGlobalPos: TPoint;
    FSplitterMouseDownImagePos: TPoint;

    FEditingPrimitivePoints: TEditingPrimitivePoints;

    FSelectionHold: Boolean;
    FMouseDownSelPos: TPoint;
    FMouseDownGlobalPos: TPoint;

    FOIEditorMenu: TPopupMenu;

    FOnLoadBitmap: TOnLoadBitmap;
    FOnLoadRenderedBitmap: TOnLoadRenderedBitmap;
    FOnGetListOfExternallyRenderedImages: TOnGetListOfExternallyRenderedImages;
    FOnLoadPrimitivesFile: TOnLoadPrimitivesFile;
    FOnSavePrimitivesFile: TOnSavePrimitivesFile;
    FOnEvaluateReplacementsFunc: TEvaluateReplacementsFunc;
    FOnTriggerOnControlsModified: TOnTriggerOnControlsModified;
    FOnSaveFromMenu: TNotifyEvent;
    FOnPictureOpenDialogExecute: TOnPictureOpenDialogExecute;
    FOnGetPictureOpenDialogFileName: TOnGetPictureOpenDialogFileName;

    //procedure SetFileIndex(Value: Integer);

    procedure CreateRemainingUIComponents;

    procedure ClearPreviewTabs;
    procedure ClearAllEditingPoints;
    procedure RebuildAllEditingPoints(APointCount: Integer);
    procedure ReassignImagePointers;
    function AddPreviewTabWithImage(ATabName: string): TImage;
    procedure CreateAllPreviewPages;
    procedure BuildImgLstPreviewPrimitives;
    procedure BuildFontColorIconsList;
    procedure ResizeFrameSectionsBySplitter(NewLeft: Integer);

    procedure SetEditPointsFromLimitLabels(APmtvType: Integer; ALeftLimitLabel, ATopLimitLabel, ARightLimitLabel, ABottomLimitLabel: TPaintedLabel);
    procedure SetLimitLabelFromPolygon(APrimitiveIndex: Integer; ALeftLimitLabel, ATopLimitLabel, ARightLimitLabel, ABottomLimitLabel: TPaintedLabel);
    procedure SelectPrimitiveOnPreviewImage(APmtvType, APrimitiveIndex, AOrderIndex: Integer; ALeftLimitLabel, ATopLimitLabel, ARightLimitLabel, ABottomLimitLabel: TPaintedLabel);
    procedure UpdatePrimitiveVertex(AEditingPointIndex, NewX, NewY: Integer);

    procedure GetOrderContentByIndex(AIndex: Integer; var ADestContent: TCompositionOrder);
    procedure DeleteOrderByIndex(AIndex: Integer; ADeleteTab: Boolean = True);
    procedure InsertOrderAtIndex(AIndex: Integer; var ASrcContent: TCompositionOrder);
    procedure MoveOrder(ASrcIndex, ADestIndex: Integer);
    procedure MoveOrderTabContent(ASrcIndex, ADestIndex: Integer);

    function GetOrderItemByIndex(AOrderIndex, AItemIndex: Integer): Integer;
    procedure DeleteOrderItemByIndex(AOrderIndex, AItemIndex: Integer);
    procedure InsertOrderItemAtIndex(AOrderIndex, AItemIndex: Integer; ASrcItem: Integer);
    procedure MoveOrderItem(AOrderIndex, ASrcIndex, ADestIndex: Integer);
    procedure GetSelectionLabelsByOrderIndex(AOrderIndex: Integer; var ALeftLbl, ATopLbl, ARightLbl, ABottomLbl: TLabel);

    procedure MenuItem_RemoveAllPrimitivesFromList(Sender: TObject);
    procedure MenuItem_AddPrimitiveToList(Sender: TObject);
    procedure MenuItem_SetValueFromEnumItem(Sender: TObject);
    procedure MenuItem_SetExternallyRenderedFile(Sender: TObject);
    procedure MenuItem_BrowsePrimitivesImageFromDisk(Sender: TObject);
    procedure MenuItem_RemovePrimitiveFromList(Sender: TObject);
    procedure MenuItem_RemoveAllCompositionOrdersFromList(Sender: TObject);
    procedure MenuItem_AddCompositionOrderToList(Sender: TObject);
    procedure MenuItem_RemoveCompositionOrderFromList(Sender: TObject);
    procedure MenuItem_RepaintAllCompositions(Sender: TObject);

    procedure MenuItem_AddPointToPolygon(Sender: TObject);
    procedure MenuItem_RemoveLastPointFromPolygon(Sender: TObject);

    procedure SetLabelsFromMouseOverPreviewImgPixelColor(APixelColor: TColor);
    procedure ClearPrimitiveSelection;
    function PointOnPrimitive(X, Y: Integer; var APrimitive: TPrimitiveRec; AWorkingImage: TImage): Boolean;
    procedure SelectPrimitiveByOrderIndex(APrimitiveIndex, AOrderIndex: Integer);
    procedure imgPreviewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgPreviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgPreviewMouseEnter(Sender: TObject);
    procedure imgPreviewMouseLeave(Sender: TObject);
    procedure pnlPreviewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    function DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function DoOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    procedure DoOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages: TStringList);
    procedure DoOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    procedure DoOnSavePrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    function DoOnEvaluateReplacementsFunc(s: string; Recursive: Boolean = True; AEvalTextCount: Integer = -1): string;
    procedure DoOnTriggerOnControlsModified;
    procedure DoOnSaveFromMenu;
    function DoOnPictureOpenDialogExecute: Boolean;
    function DoOnGetPictureOpenDialogFileName: string;

    function EditFontProperties(APmtvType, APropertyIndex, AItemIndex: Integer; var ANewItems: string): Boolean;

    function HandleOnOIGetCategoryCount: Integer;
    function HandleOnOIGetCategory(AIndex: Integer): string;
    function HandleOnOIGetCategoryValue(ACategoryIndex: Integer; var AEditorType: TOIEditorType): string;
    function HandleOnOIGetPropertyCount(ACategoryIndex: Integer): Integer;
    function HandleOnOIGetPropertyName(ACategoryIndex, APropertyIndex: Integer): string;
    function HandleOnOIGetPropertyValue(ACategoryIndex, APropertyIndex: Integer; var AEditorType: TOIEditorType): string;
    function HandleOnOIGetListPropertyItemCount(ACategoryIndex, APropertyIndex: Integer): Integer;
    function HandleOnOIGetListPropertyItemName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
    function HandleOnOIGetListPropertyItemValue(ACategoryIndex, APropertyIndex, AItemIndex: Integer; var AEditorType: TOIEditorType): string;
    function HandleOnOIGetDataTypeName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
    function HandleOnOIGetExtraInfo(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;

    procedure HandleOnOIGetImageIndexEx(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);
    procedure HandleOnOIEditedText(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; ANewText: string);
    function HandleOnOIEditItems(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewItems: string): Boolean;

    function HandleOnOIGetColorConstsCount(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer;
    procedure HandleOnOIGetColorConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AColorItemIndex: Integer; var AColorName: string; var AColorValue: Int64);

    function HandleOnOIGetEnumConstsCount(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer;
    procedure HandleOnOIGetEnumConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEnumItemIndex: Integer; var AEnumItemName: string; var AEnumImgItemIndex: Integer; var AImgLst: TImageList);

    procedure HandleOnOIPaintText(ANodeData: TNodeDataPropertyRec; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
      const TargetCanvas: TCanvas; Column: TColumnIndex; var TextType: TVSTTextType);

    procedure HandleOnOIBeforeCellPaint(ANodeData: TNodeDataPropertyRec; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
      TargetCanvas: TCanvas; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);

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

    procedure HandleOnOIAfterSpinTextEditorChanging(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewValue: string);
    procedure HandleOnOISelectedNode(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, Column: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure HandleOnOIDragAllowed(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer; var Allowed: Boolean);
    procedure HandleOnOIDragOver(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, SrcNodeLevel, SrcCategoryIndex, SrcPropertyIndex, SrcPropertyItemIndex: Integer; Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode; var Effect: DWORD; var Accept: Boolean);
    procedure HandleOnOIDragDrop(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, SrcNodeLevel, SrcCategoryIndex, SrcPropertyIndex, SrcPropertyItemIndex: Integer; Shift: TShiftState; const Pt: TPoint; var Effect: DWORD; Mode: TDropMode);

    function HandleOnEvaluateReplacementsFunc(s: string; Recursive: Boolean = True; AEvalTextCount: Integer = -1): string;
    function HandleOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function HandleOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;

    procedure HandleEditingPointsOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HandleEditingPointsOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HandleEditingPointsOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HandleEditingPointsOnMouseEnter(Sender: TObject);
    procedure HandleEditingPointsOnMouseLeave(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFile(AFileName: string);
    procedure SaveFile(AFileName: string);
    procedure ClearContent;
    procedure ComposePrimitives(ABmp: TBitmap; AOrderIndex: Integer);
    procedure RepaintAllCompositions;
    function GetOrderCount: Integer;

    property FileIndex: Integer read FFileIndex write FFileIndex {SetFileIndex}; //primitives file index in the list of MatchPrimitiveFiles property;

    property OnLoadBitmap: TOnLoadBitmap write FOnLoadBitmap;
    property OnLoadRenderedBitmap: TOnLoadRenderedBitmap write FOnLoadRenderedBitmap;
    property OnGetListOfExternallyRenderedImages: TOnGetListOfExternallyRenderedImages write FOnGetListOfExternallyRenderedImages;
    property OnLoadPrimitivesFile: TOnLoadPrimitivesFile write FOnLoadPrimitivesFile; //called by LoadFile
    property OnSavePrimitivesFile: TOnSavePrimitivesFile write FOnSavePrimitivesFile;
    property OnEvaluateReplacementsFunc: TEvaluateReplacementsFunc write FOnEvaluateReplacementsFunc; //called by ComposePrimitives
    property OnTriggerOnControlsModified: TOnTriggerOnControlsModified write FOnTriggerOnControlsModified;
    property OnSaveFromMenu: TNotifyEvent write FOnSaveFromMenu;
    property OnPictureOpenDialogExecute: TOnPictureOpenDialogExecute write FOnPictureOpenDialogExecute;
    property OnGetPictureOpenDialogFileName: TOnGetPictureOpenDialogFileName write FOnGetPictureOpenDialogFileName;
  end;

implementation

{$R *.frm}


uses
  ClickerPrimitiveValues, ClickerOIUtils, ClickerPrimitivesCompositor, ClickerZoomPreviewForm,
  ClickerExtraUtils, FPCanvas, Clipbrd, Dialogs;


const
  CAddPrimitiveMenuPrefix = 'Add ';
  CRemovePrimitiveMenuPrefix = 'Remove ';
  CRemoveOrderMenuPrefix = 'Remove ';

  CLeftLblID = 101;
  CTopLblID = 102;
  CRightLblID = 103;
  CBottomLblID = 104;

  CEditingPointSize = 8; //px
  CEditingPointOffset = CEditingPointSize shr 1;


procedure TEditingPoint.Paint;
begin
  inherited Paint;
  Canvas.Pen.Color := clRed;
  Canvas.MoveTo(1, 1);
  Canvas.LineTo(Width, Height);
  Canvas.MoveTo(0, Height);
  Canvas.LineTo(Width, 0);
end;


procedure TfrClickerPrimitives.CreateRemainingUIComponents;
begin
  ////////////////////////////// OI
  FOIFrame := TfrObjectInspector.Create(Self);
  FOIFrame.Parent := pnlvstOI;
  FOIFrame.Left := 0;
  FOIFrame.Top := 0;
  FOIFrame.Width := pnlvstOI.Width;
  FOIFrame.Height := pnlvstOI.Height;
  FOIFrame.Anchors := [akBottom, akLeft, akRight, akTop];

  pnlvstOI.Anchors := [akBottom, akLeft, {akRight,} akTop];

  FOIFrame.OnOIGetCategoryCount := HandleOnOIGetCategoryCount;
  FOIFrame.OnOIGetCategory := HandleOnOIGetCategory;
  FOIFrame.OnOIGetCategoryValue := HandleOnOIGetCategoryValue;
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
  FOIFrame.OnOITextEditorMouseDown := HandleOnTextEditorMouseDown;
  FOIFrame.OnOITextEditorMouseMove := HandleOnTextEditorMouseMove;
  FOIFrame.OnOITextEditorKeyUp := HandleOnOITextEditorKeyUp;
  FOIFrame.OnOITextEditorKeyDown := HandleOnOITextEditorKeyDown;
  FOIFrame.OnOIEditorAssignMenuAndTooltip := HandleOnOIEditorAssignMenuAndTooltip;
  FOIFrame.OnOIGetFileDialogSettings := HandleOnOIGetFileDialogSettings;
  FOIFrame.OnOIArrowEditorClick := HandleOnOIArrowEditorClick;
  FOIFrame.OnOIUserEditorClick := HandleOnOIUserEditorClick;
  FOIFrame.OnOIBrowseFile := HandleOnOIBrowseFile;
  FOIFrame.OnOIAfterSpinTextEditorChanging := HandleOnOIAfterSpinTextEditorChanging;
  FOIFrame.OnOISelectedNode := HandleOnOISelectedNode;
  FOIFrame.OnOIDragAllowed := HandleOnOIDragAllowed;
  FOIFrame.OnOIDragOver := HandleOnOIDragOver;
  FOIFrame.OnOIDragDrop := HandleOnOIDragDrop;

  FOIFrame.Visible := True;

  FOIFrame.ListItemsVisible := True;
  FOIFrame.DataTypeVisible := False;
  FOIFrame.ExtraInfoVisible := False;
  FOIFrame.PropertyItemHeight := 22; //50;  //this should be 50 for bitmaps
  FOIFrame.ColumnWidths[0] := 144;
  FOIFrame.ColumnWidths[1] := 130;

  //FOIFrame.ReloadContent;  //set by loading
  pnlvstOI.Visible := True;
end;


constructor TfrClickerPrimitives.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateRemainingUIComponents;

  SetLength(FPrimitives, 0);
  SetLength(FOrders, 0);
  SetLength(FEditingPrimitivePoints.EditingPoints, 0);
  FPrimitiveSettings.CompositorDirection := cdTopBot;

  FCurrentMousePosOnPreviewImg.X := Screen.Width - 10;  //init somewhere near the bottom-right corner of the screen
  FCurrentMousePosOnPreviewImg.Y := Screen.Height - 10;
  FHold := False;
  FSelectionHold := False;

  lblModified.Left := lblModified.Left + 20;
  chkHighContrast.Left := lblModified.Width + lblModified.Left + 20;
  chkShowPrimitiveEdges.Left := chkHighContrast.Left + chkHighContrast.Width + 20;
  chkShowPrimitiveEdges.Top := chkHighContrast.Top;
  PageControlPreview.Caption := 'PreviewPrimitives';

  FOIEditorMenu := TPopupMenu.Create(Self);

  FOnLoadBitmap := nil;
  FOnLoadRenderedBitmap := nil;
  FOnGetListOfExternallyRenderedImages := nil;
  FOnLoadPrimitivesFile := nil;
  FOnSavePrimitivesFile := nil;
  FOnEvaluateReplacementsFunc := nil;
  FOnTriggerOnControlsModified := nil;
  FOnSaveFromMenu := nil;
  FOnPictureOpenDialogExecute := nil;
  FOnGetPictureOpenDialogFileName := nil;
end;


destructor TfrClickerPrimitives.Destroy;
begin
  SetLength(FPrimitives, 0);
  SetLength(FOrders, 0);
  FOIEditorMenu.Free;

  inherited Destroy;
end;


procedure TfrClickerPrimitives.ClearPreviewTabs;
var
  i: Integer;
begin
  for i := PageControlPreview.PageCount - 1 downto 0 do
  begin
    PageControlPreview.Pages[i].Free;
    FEditingPanels[i].Free;
  end;

  SetLength(FEditingPanels, 0);
end;


function TfrClickerPrimitives.AddPreviewTabWithImage(ATabName: string): TImage;
var
  TempTabSheet: TTabSheet;
  TempScrollBox: TScrollBox;
  TempPanel: TPanel;
  PreviewImage: TImage;
  FLeftLimitLabel_ForPrimitive: TLabel;
  FTopLimitLabel_ForPrimitive: TLabel;
  FRightLimitLabel_ForPrimitive: TLabel;
  FBottomLimitLabel_ForPrimitive: TLabel;
begin
  TempTabSheet := TTabSheet.Create(PageControlPreview);
  TempTabSheet.Parent := PageControlPreview;
  PageControlPreview.ActivePageIndex := PageControlPreview.PageCount - 1;  //required, otherwise, the tab is not added properly

  TempTabSheet.Caption := ATabName;

  TempScrollBox := TScrollBox.Create(TempTabSheet);  //set TempTabSheet as the owner, for easy finding
  TempScrollBox.Parent := TempTabSheet;
  TempTabSheet.Tag := PtrInt(TempScrollBox); //store a pointer to the scrollbox, so it can be used later

  TempScrollBox.Left := 0;
  TempScrollBox.Top := 0;
  TempScrollBox.Width := 1920 shr 2; //TempTabSheet.Width;
  TempScrollBox.Height := 1080 shr 2; //TempTabSheet.Height;
  TempScrollBox.Anchors := [akLeft, akTop, akRight, akBottom];
  TempScrollBox.HorzScrollBar.Visible := True;
  TempScrollBox.VertScrollBar.Visible := True;
  TempScrollBox.HorzScrollBar.Tracking := True;
  TempScrollBox.VertScrollBar.Tracking := True;
  TempScrollBox.Color := clYellow;
  TempScrollBox.Visible := True;

  TempPanel := TPanel.Create(TempScrollBox);
  TempPanel.Parent := TempScrollBox;
  TempPanel.Left := 0;
  TempPanel.Top := 0;
  TempPanel.Width := TempScrollBox.Width - 20;
  TempPanel.Height := TempScrollBox.Height - 20;
  //TempPanel.Anchors := [akLeft, akTop, akRight, akBottom];
  TempPanel.Color := $88FF88;
  TempPanel.Visible := True;
  TempPanel.ShowHint := True;
  TempPanel.OnMouseMove := pnlPreviewMouseMove;

  SetLength(FEditingPanels, Length(FEditingPanels) + 1);
  FEditingPanels[Length(FEditingPanels) - 1] := TempPanel;

  PreviewImage := TImage.Create(TempPanel);
  PreviewImage.Parent := TempPanel;
  TempScrollBox.Tag := PtrInt(PreviewImage);  //the image can be easily accessed as TImage(TScrollBox(PageControlPreview.Pages[i].Tag).Tag).Canvas.WhatEver;

  PreviewImage.Left := 0;
  PreviewImage.Top := 0;
  PreviewImage.Width := TempPanel.Width;
  PreviewImage.Height := TempPanel.Height;
  //PreviewImage.Anchors := [akLeft, akTop, akRight, akBottom];
  PreviewImage.AutoSize := False;
  PreviewImage.Transparent := False;
  PreviewImage.Visible := True;
  PreviewImage.PopupMenu := pmPreview;
  PreviewImage.Tag := PageControlPreview.ActivePageIndex;

  PreviewImage.Canvas.Pen.Color := clWhite;
  PreviewImage.Canvas.Brush.Color := clWhite;
  PreviewImage.Canvas.Rectangle(0, 0, PreviewImage.Width, PreviewImage.Height);

  TempScrollBox.Width := TempTabSheet.Width;
  TempScrollBox.Height := TempTabSheet.Height;

  //PreviewImage.Canvas.Font.Color := clRed;          //for debugging only
  //PreviewImage.Canvas.TextOut(20, 20, ATabName);    //for debugging only

  PreviewImage.OnMouseMove := imgPreviewMouseMove;
  PreviewImage.OnMouseDown := imgPreviewMouseDown;
  PreviewImage.OnMouseEnter := imgPreviewMouseEnter;
  PreviewImage.OnMouseLeave := imgPreviewMouseLeave;

  Result := PreviewImage;

  CreateSelectionLabels(TempPanel, //Self,
                        TempPanel,
                        TLabel(FLeftLimitLabel_ForPrimitive),
                        TLabel(FTopLimitLabel_ForPrimitive),
                        TLabel(FRightLimitLabel_ForPrimitive),
                        TLabel(FBottomLimitLabel_ForPrimitive),
                        clTeal,
                        clTeal,
                        clTeal,
                        clTeal,
                        True,
                        True);

  FLeftLimitLabel_ForPrimitive.Tag := CLeftLblID;  //to identify the labels later
  FTopLimitLabel_ForPrimitive.Tag := CTopLblID;
  FRightLimitLabel_ForPrimitive.Tag := CRightLblID;
  FBottomLimitLabel_ForPrimitive.Tag := CBottomLblID;

  FLeftLimitLabel_ForPrimitive.Left := 0;
  FTopLimitLabel_ForPrimitive.Top := 0;
  FRightLimitLabel_ForPrimitive.Left := 200;
  FBottomLimitLabel_ForPrimitive.Top := 200;

  FLeftLimitLabel_ForPrimitive.Height := 1080;//TempScrollBox.Height;
  FTopLimitLabel_ForPrimitive.Width := 1920;//TempScrollBox.Width;
  FRightLimitLabel_ForPrimitive.Height := 1080;//TempScrollBox.Height;
  FBottomLimitLabel_ForPrimitive.Width := 1920;//TempScrollBox.Width;

  //FLeftLimitLabel_ForPrimitive.SendToBack;  //for some reason, the labels are sent under the panel, as if they are at the same level :(
  //FTopLimitLabel_ForPrimitive.SendToBack;
  //FRightLimitLabel_ForPrimitive.SendToBack;
  //FBottomLimitLabel_ForPrimitive.SendToBack;

  FLeftLimitLabel_ForPrimitive.Visible := chkShowPrimitiveEdges.Checked;
  FTopLimitLabel_ForPrimitive.Visible := chkShowPrimitiveEdges.Checked;
  FRightLimitLabel_ForPrimitive.Visible := chkShowPrimitiveEdges.Checked;
  FBottomLimitLabel_ForPrimitive.Visible := chkShowPrimitiveEdges.Checked;
end;


procedure TfrClickerPrimitives.ReassignImagePointers;
var
  i, j: Integer;
  TempTabSheet: TTabSheet;
  TempScrollBox: TScrollBox;
  TempPanel: TPanel;
  PreviewImage: TImage;
begin
  for i := 0 to PageControlPreview.PageCount - 1 do
  begin
    TempTabSheet := PageControlPreview.Pages[i];

    TempScrollBox := nil;
    for j := 0 to TempTabSheet.ComponentCount - 1 do
      if TempTabSheet.Components[j] is TScrollBox then
      begin
        TempScrollBox := TempTabSheet.Components[j] as TScrollBox;
        Break;
      end;

    if TempScrollBox = nil then
      raise Exception.Create('TempScrollBox not found.');

    TempScrollBox.Parent := TempTabSheet;
    TempTabSheet.Tag := PtrInt(TempScrollBox);

    TempPanel := nil;
    for j := 0 to TempScrollBox.ComponentCount - 1 do
      if TempScrollBox.Components[j] is TPanel then
      begin
        TempPanel := TempScrollBox.Components[j] as TPanel;
        Break;
      end;

    if TempScrollBox = nil then
      raise Exception.Create('TempPanel not found.');

    TempPanel.Parent := TempScrollBox;
    FEditingPanels[i] := TempPanel;

    PreviewImage := nil;
    for j := 0 to TempPanel.ComponentCount - 1 do
      if TempPanel.Components[j] is TImage then
      begin
        PreviewImage := TempPanel.Components[j] as TImage;
        Break;
      end;

    if PreviewImage = nil then
      raise Exception.Create('PreviewImage not found.');

    PreviewImage.Parent := TempPanel;
    PreviewImage.Tag := i;
    TempScrollBox.Tag := PtrInt(PreviewImage);
  end; //for i
end;


procedure UpdatePreviewImageSizeFromPrimitives(APreviewImage: TImage; APreviewScrollBox: TScrollBox);
var
  TempPanel: TPanel;
begin
  if APreviewImage.Width <> APreviewImage.Picture.Bitmap.Width then
    APreviewImage.Width := APreviewImage.Picture.Bitmap.Width;

  if APreviewImage.Height <> APreviewImage.Picture.Bitmap.Height then
    APreviewImage.Height := APreviewImage.Picture.Bitmap.Height;

  TempPanel := APreviewImage.Parent as TPanel;
  if TempPanel.Width <> APreviewImage.Picture.Bitmap.Width + 10 then
    TempPanel.Width := APreviewImage.Picture.Bitmap.Width + 10;

  if TempPanel.Height <> APreviewImage.Picture.Bitmap.Height + 10 then
    TempPanel.Height := APreviewImage.Picture.Bitmap.Height + 10;

  //APreviewScrollBox.UpdateScrollbars;  //useless
  APreviewScrollBox.HorzScrollBar.Range := TempPanel.Width;
  APreviewScrollBox.VertScrollBar.Range := TempPanel.Height;
end;


procedure TfrClickerPrimitives.CreateAllPreviewPages;
var
  i: Integer;
begin
  ClearPreviewTabs;

  for i := 0 to Length(FOrders) - 1 do
    AddPreviewTabWithImage(FOrders[i].Name);

  RepaintAllCompositions;
  PageControlPreview.ActivePageIndex := 0;
end;


procedure TfrClickerPrimitives.RepaintAllCompositions;
var
  i: Integer;
  PreviewImage: TImage;
  PmtvCompositor: TPrimitivesCompositor;
  UsingHighContrast: Boolean;
  TempScrollBox: TScrollBox;
begin
  PmtvCompositor := TPrimitivesCompositor.Create;
  try
    PmtvCompositor.FileIndex := FFileIndex;
    PmtvCompositor.OnEvaluateReplacementsFunc := HandleOnEvaluateReplacementsFunc;
    PmtvCompositor.OnLoadBitmap := HandleOnLoadBitmap;
    PmtvCompositor.OnLoadRenderedBitmap := HandleOnLoadRenderedBitmap;

    UsingHighContrast := chkHighContrast.Checked;
    BuildImgLstPreviewPrimitives;

    for i := 0 to Length(FOrders) - 1 do
    begin
      TempScrollBox := TScrollBox(PageControlPreview.Pages[i].Tag);
      PreviewImage := TImage(TempScrollBox.Tag);

      PreviewImage.Picture.Bitmap.Width := PmtvCompositor.GetMaxX(PreviewImage.Picture.Bitmap.Canvas, FPrimitives) + 1;
      PreviewImage.Picture.Bitmap.Height := PmtvCompositor.GetMaxY(PreviewImage.Picture.Bitmap.Canvas, FPrimitives) + 1;

      PmtvCompositor.HighContrastOption1 := MenuItem_HighContrastOption1.Checked;
      PmtvCompositor.HighContrastOption2 := MenuItem_HighContrastOption2.Checked;
      PmtvCompositor.ComposePrimitives(PreviewImage.Picture.Bitmap, i, UsingHighContrast, FPrimitives, FOrders, FPrimitiveSettings);
      UpdatePreviewImageSizeFromPrimitives(PreviewImage, TempScrollBox);
    end;
  finally
    PmtvCompositor.Free;
  end;
end;


procedure TfrClickerPrimitives.LoadFile(AFileName: string);
begin
  SetLength(FPrimitives, 0);
  SetLength(FOrders, 0);
  FOIFrame.ReloadContent;  //cleanup here, so the tree won't repaint with dangling pointers

  DoOnLoadPrimitivesFile(AFileName, FPrimitives, FOrders, FPrimitiveSettings);
  FOIFrame.ReloadContent;
  CreateAllPreviewPages;
  RepaintAllCompositions;
  BuildImgLstPreviewPrimitives;
  BuildFontColorIconsList;
  lblModified.Hide;
end;


procedure TfrClickerPrimitives.SaveFile(AFileName: string);
begin
  DoOnSavePrimitivesFile(AFileName, FPrimitives, FOrders, FPrimitiveSettings);
  lblModified.Hide;
end;


procedure TfrClickerPrimitives.ClearContent;
begin
  SetLength(FPrimitives, 0);
  SetLength(FOrders, 0);
  FOIFrame.ReloadContent;
  lblModified.Hide;
end;


procedure TfrClickerPrimitives.GetOrderContentByIndex(AIndex: Integer; var ADestContent: TCompositionOrder);
var
  i: Integer;
begin
  ADestContent.Name := FOrders[AIndex].Name;
  SetLength(ADestContent.Items, Length(FOrders[AIndex].Items));

  for i := 0 to Length(ADestContent.Items) - 1 do
    ADestContent.Items[i] := FOrders[AIndex].Items[i];
end;


procedure TfrClickerPrimitives.DeleteOrderByIndex(AIndex: Integer; ADeleteTab: Boolean = True);
var
  i, j: Integer;
begin
  if ADeleteTab then
    FEditingPanels[AIndex].Free;

  for i := AIndex to Length(FOrders) - 2 do
  begin
    FOrders[i].Name := FOrders[i + 1].Name;

    for j := 0 to Length(FOrders[i].Items) - 1 do
      FOrders[i].Items[j] := FOrders[i + 1].Items[j];

    if ADeleteTab then
      FEditingPanels[i] := FEditingPanels[i + 1];
  end;

  SetLength(FOrders[Length(FOrders) - 1].Items, 0);
  SetLength(FOrders, Length(FOrders) - 1);

  if ADeleteTab then
  begin
    PageControlPreview.Pages[AIndex].Free;
    SetLength(FEditingPanels, Length(FEditingPanels) - 1);
  end;

  ReassignImagePointers;
end;


procedure TfrClickerPrimitives.InsertOrderAtIndex(AIndex: Integer; var ASrcContent: TCompositionOrder);
var
  i, j: Integer;
begin
  SetLength(FOrders, Length(FOrders) + 1);
  SetLength(FOrders[Length(FOrders) - 1].Items, Length(FPrimitives));

  for i := Length(FOrders) - 1 downto AIndex + 1 do
  begin
    FOrders[i].Name := FOrders[i - 1].Name;

    for j := 0 to Length(FOrders[i].Items) - 1 do
      FOrders[i].Items[j] := FOrders[i - 1].Items[j];
  end;

  FOrders[AIndex].Name := ASrcContent.Name;
  for j := 0 to Length(FOrders[AIndex].Items) - 1 do
    FOrders[AIndex].Items[j] := ASrcContent.Items[j];

  ReassignImagePointers;
end;


procedure TfrClickerPrimitives.MoveOrder(ASrcIndex, ADestIndex: Integer);
var
  Ph: TCompositionOrder;
begin
  if ASrcIndex = ADestIndex then
    Exit;

  GetOrderContentByIndex(ASrcIndex, Ph);
  DeleteOrderByIndex(ASrcIndex, False);

  InsertOrderAtIndex(ADestIndex, Ph);
  MoveOrderTabContent(ASrcIndex, ADestIndex);
end;


procedure TfrClickerPrimitives.MoveOrderTabContent(ASrcIndex, ADestIndex: Integer);
var
  ScrollBoxAtSrc: PtrInt;
  i: Integer;
begin
  if ASrcIndex = ADestIndex then
    Exit;

  ScrollBoxAtSrc := PageControlPreview.Pages[ASrcIndex].Tag;

  if ASrcIndex < ADestIndex then
  begin
    for i := ASrcIndex to ADestIndex - 1 do
    begin
      TScrollBox(PageControlPreview.Pages[i + 1].Tag).Parent := PageControlPreview.Pages[i];
      PageControlPreview.Pages[i].Tag := PageControlPreview.Pages[i + 1].Tag;
    end;
  end
  else
  begin
    for i := ASrcIndex downto ADestIndex + 1 do
    begin
      TScrollBox(PageControlPreview.Pages[i - 1].Tag).Parent := PageControlPreview.Pages[i];
      PageControlPreview.Pages[i].Tag := PageControlPreview.Pages[i - 1].Tag;
    end;
  end;

  TScrollBox(ScrollBoxAtSrc).Parent := PageControlPreview.Pages[ADestIndex];
  PageControlPreview.Pages[ADestIndex].Tag := ScrollBoxAtSrc;

  for i := 0 to PageControlPreview.PageCount - 1 do
    PageControlPreview.Pages[i].Caption := FOrders[i].Name;
end;


function TfrClickerPrimitives.GetOrderItemByIndex(AOrderIndex, AItemIndex: Integer): Integer;
begin
  Result := FOrders[AOrderIndex].Items[AItemIndex];
end;


procedure TfrClickerPrimitives.DeleteOrderItemByIndex(AOrderIndex, AItemIndex: Integer);
var
  i: Integer;
begin
  for i := AItemIndex to Length(FOrders[AOrderIndex].Items) - 2 do
    FOrders[AOrderIndex].Items[i] := FOrders[AOrderIndex].Items[i + 1];

  SetLength(FOrders[AOrderIndex].Items, Length(FOrders[AOrderIndex].Items) - 1);
end;


procedure TfrClickerPrimitives.InsertOrderItemAtIndex(AOrderIndex, AItemIndex: Integer; ASrcItem: Integer);
var
  i: Integer;
begin
  SetLength(FOrders[AOrderIndex].Items, Length(FOrders[AOrderIndex].Items) + 1);

  for i := Length(FOrders[AOrderIndex].Items) - 1 downto AItemIndex + 1 do
    FOrders[AOrderIndex].Items[i] := FOrders[AOrderIndex].Items[i - 1];

  FOrders[AOrderIndex].Items[AItemIndex] := ASrcItem;
end;


procedure TfrClickerPrimitives.MoveOrderItem(AOrderIndex, ASrcIndex, ADestIndex: Integer);
var
  Ph: Integer;
begin
  if ASrcIndex = ADestIndex then
    Exit;

  Ph := GetOrderItemByIndex(AOrderIndex, ASrcIndex);
  DeleteOrderItemByIndex(AOrderIndex, ASrcIndex);

  InsertOrderItemAtIndex(AOrderIndex, ADestIndex, Ph);
end;


procedure TfrClickerPrimitives.GetSelectionLabelsByOrderIndex(AOrderIndex: Integer; var ALeftLbl, ATopLbl, ARightLbl, ABottomLbl: TLabel);
var
  TempScrollBox: TScrollBox;
  TempPanel: TPanel;
  i: Integer;
begin
  TempScrollBox := TScrollBox(PageControlPreview.Pages[AOrderIndex].Tag);

  TempPanel := nil;
  for i := 0 to TempScrollBox.ComponentCount - 1 do
    if TempScrollBox.Components[i] is TPanel then
    begin
      TempPanel := TempScrollBox.Components[i] as TPanel;
      Break;
    end;

  ALeftLbl := nil;
  ATopLbl := nil;
  ARightLbl := nil;
  ABottomLbl := nil;

  if TempPanel = nil then
  begin
    MessageBox(Handle, PChar('Cannot get primitives drawing panel on order index: ' + IntToStr(AOrderIndex)), PChar(Application.Title), MB_ICONERROR);
    Exit;
  end;

  for i := 0 to TempPanel.ComponentCount - 1 do
    if (TempPanel.Components[i] is TLabel) or (TempPanel.Components[i] is TPaintedLabel) then
    begin
      case TempPanel.Components[i].Tag of
        CLeftLblID:
           ALeftLbl := TLabel(TempPanel.Components[i]);

        CTopLblID:
          ATopLbl := TLabel(TempPanel.Components[i]);

        CRightLblID:
          ARightLbl := TLabel(TempPanel.Components[i]);

        CBottomLblID:
          ABottomLbl := TLabel(TempPanel.Components[i]);
      end;
    end;
end;


procedure TfrClickerPrimitives.ComposePrimitives(ABmp: TBitmap; AOrderIndex: Integer);
var
  PmtvCompositor: TPrimitivesCompositor;
begin
  PmtvCompositor := TPrimitivesCompositor.Create;
  try
    PmtvCompositor.OnEvaluateReplacementsFunc := HandleOnEvaluateReplacementsFunc;
    PmtvCompositor.OnLoadBitmap := HandleOnLoadBitmap;
    PmtvCompositor.OnLoadRenderedBitmap := HandleOnLoadRenderedBitmap;

    PmtvCompositor.HighContrastOption1 := False;
    PmtvCompositor.HighContrastOption2 := False;
    PmtvCompositor.ComposePrimitives(ABmp, AOrderIndex, chkHighContrast.Checked, FPrimitives, FOrders, FPrimitiveSettings);
  finally
    PmtvCompositor.Free;
  end;
end;


procedure TfrClickerPrimitives.tmrReloadOIContentTimer(Sender: TObject);
begin
  tmrReloadOIContent.Enabled := False;
  FOIFrame.ReloadContent;
end;


procedure TfrClickerPrimitives.tmrDrawZoomTimer(Sender: TObject);
var
  TempBmp: TBitmap;
  tp: TPoint;
  PreviewImage: TImage;
  Idx: Integer;
begin
  tmrDrawZoom.Enabled := False;

  Idx := PageControlPreview.ActivePageIndex;
  if Idx = -1 then
    Exit;

  PreviewImage := TImage(TScrollBox(PageControlPreview.Pages[Idx].Tag).Tag);

  if PreviewImage = nil then
    Exit;

  TempBmp := TBitmap.Create;
  try
    TempBmp.Width := PreviewImage.Width;
    TempBmp.Height := PreviewImage.Height;
    TempBmp.Canvas.Draw(0, 0, PreviewImage.Picture.Bitmap);

    GetCursorPos(tp);
    SetZoomContent(TempBmp, FCurrentMousePosOnPreviewImg.X, FCurrentMousePosOnPreviewImg.Y, tp.X + 50, tp.Y + 50);
  finally
    TempBmp.Free;
  end;
end;


procedure TfrClickerPrimitives.MenuItem_CopyToClipboardClick(Sender: TObject);
var
  Idx: Integer;
  PmtvCompositor: TPrimitivesCompositor;
  Bmp: TBitmap;
begin
  PmtvCompositor := TPrimitivesCompositor.Create;
  try
    PmtvCompositor.OnEvaluateReplacementsFunc := HandleOnEvaluateReplacementsFunc;
    PmtvCompositor.OnLoadBitmap := HandleOnLoadBitmap;
    PmtvCompositor.OnLoadRenderedBitmap := HandleOnLoadRenderedBitmap;

    Idx := PageControlPreview.ActivePageIndex;
    if Idx > Length(FOrders) - 1 then
    begin
      MessageBox(Handle, 'Primitive order index is out of range.', PChar(Application.Title), MB_ICONERROR);
      Exit;
    end;

    Bmp := TBitmap.Create;
    try
      Bmp.PixelFormat := pf24bit;
      Bmp.Width := 100;
      Bmp.Height := 100;
      Bmp.Canvas.Rectangle(0, 0, Bmp.Width, Bmp.Height);

      Bmp.Width := PmtvCompositor.GetMaxX(Bmp.Canvas, FPrimitives) + 1;
      Bmp.Height := PmtvCompositor.GetMaxY(Bmp.Canvas, FPrimitives) + 1;

      PmtvCompositor.HighContrastOption1 := MenuItem_HighContrastOption1.Checked;
      PmtvCompositor.HighContrastOption2 := MenuItem_HighContrastOption2.Checked;
      PmtvCompositor.ComposePrimitives(Bmp, Idx, chkHighContrast.Checked, FPrimitives, FOrders, FPrimitiveSettings);

      Clipboard.Assign(Bmp);
    finally
      Bmp.Free;
    end;
  finally
    PmtvCompositor.Free;
  end;
end;


procedure TfrClickerPrimitives.MenuItem_EditModeClick(Sender: TObject);
begin
  if not MenuItem_EditMode.Checked then
    RebuildAllEditingPoints(0); //clear
end;


procedure TfrClickerPrimitives.MenuItem_HighContrastOption1Click(Sender: TObject);
begin
  chkHighContrastChange(nil); //ToDo: move to another procedure, instead of handler
end;


procedure TfrClickerPrimitives.MenuItem_HighContrastOption2Click(Sender: TObject);
begin
  chkHighContrastChange(nil); //ToDo: move to another procedure, instead of handler
end;


procedure TfrClickerPrimitives.MenuItem_RepaintAllCompositionsFromStaticMenuClick(
  Sender: TObject);
begin
  RepaintAllCompositions;
  BuildImgLstPreviewPrimitives;
  BuildFontColorIconsList;
end;


procedure TfrClickerPrimitives.MenuItem_AddPointToPolygon(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ValueStr: string;
  TempPrimitiveType, TempPrimitiveIndex: Integer;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    ValueStr := StringReplace(MenuData^.MenuItemCaption, '&', '', [rfReplaceAll]);
    ValueStr := Copy(ValueStr, Length(CAddPrimitiveMenuPrefix) + 1, MaxInt);

    TempPrimitiveIndex := MenuData^.PropertyIndex;
    TempPrimitiveType := FPrimitives[TempPrimitiveIndex].PrimitiveType;

    if TempPrimitiveType = -1 then
    begin
      MessageBox(Handle, 'The current primitive type is not implemented.', PChar(Application.Title), MB_ICONERROR);
      Exit;
    end;

    FPrimitives[TempPrimitiveIndex].ClkPolygon.XPoints := FPrimitives[TempPrimitiveIndex].ClkPolygon.XPoints + '30' + CPolygonPointLineBreak;
    FPrimitives[TempPrimitiveIndex].ClkPolygon.YPoints := FPrimitives[TempPrimitiveIndex].ClkPolygon.YPoints + '40' + CPolygonPointLineBreak;

    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex); // not need for tmrReloadOIContent.Enabled := True;
    DoOnTriggerOnControlsModified;  //the pmtv file is modified, not the template

    RepaintAllCompositions;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerPrimitives.MenuItem_RemoveLastPointFromPolygon(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ValueStr: string;
  TempPrimitiveType, TempPrimitiveIndex: Integer;
  ListOfPoints: TStringList;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    ValueStr := StringReplace(MenuData^.MenuItemCaption, '&', '', [rfReplaceAll]);
    ValueStr := Copy(ValueStr, Length(CAddPrimitiveMenuPrefix) + 1, MaxInt);

    TempPrimitiveIndex := MenuData^.PropertyIndex;
    TempPrimitiveType := FPrimitives[TempPrimitiveIndex].PrimitiveType;

    if TempPrimitiveType = -1 then
    begin
      MessageBox(Handle, 'The current primitive type is not implemented.', PChar(Application.Title), MB_ICONERROR);
      Exit;
    end;

    if MessageBox(Handle, 'Are you sure you want to remove the last item?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = IDNO then
      Exit;

    ListOfPoints := TStringList.Create;
    try
      ListOfPoints.LineBreak := CPolygonPointLineBreak;
      ListOfPoints.Text := FPrimitives[TempPrimitiveIndex].ClkPolygon.XPoints;
      if ListOfPoints.Count = 0 then
        Exit;

      ListOfPoints.Delete(ListOfPoints.Count - 1);
      FPrimitives[TempPrimitiveIndex].ClkPolygon.XPoints := ListOfPoints.Text;

      ListOfPoints.Text := FPrimitives[TempPrimitiveIndex].ClkPolygon.YPoints;
      if ListOfPoints.Count = 0 then  //verify also for Y, in case the list is loaded from a bad file
        Exit;

      ListOfPoints.Delete(ListOfPoints.Count - 1);
      FPrimitives[TempPrimitiveIndex].ClkPolygon.YPoints := ListOfPoints.Text;
    finally
      ListOfPoints.Free;
    end;

    FOIFrame.ReloadPropertyItems(MenuData^.CategoryIndex, MenuData^.PropertyIndex); // not need for tmrReloadOIContent.Enabled := True;
    DoOnTriggerOnControlsModified;  //the pmtv file is modified, not the template

    RepaintAllCompositions;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerPrimitives.chkHighContrastChange(Sender: TObject);
begin
  BuildImgLstPreviewPrimitives;
  BuildFontColorIconsList;
  RepaintAllCompositions;
  FOIFrame.RepaintOI;
end;


procedure TfrClickerPrimitives.chkShowPrimitiveEdgesChange(Sender: TObject);
var
  FLeftLimitLabel_ForPrimitive: TPaintedLabel;
  FTopLimitLabel_ForPrimitive: TPaintedLabel;
  FRightLimitLabel_ForPrimitive: TPaintedLabel;
  FBottomLimitLabel_ForPrimitive: TPaintedLabel;
  i: Integer;
begin
  for i := 0 to Length(FOrders) - 1 do
  begin
    GetSelectionLabelsByOrderIndex(i,
                                   TLabel(FLeftLimitLabel_ForPrimitive),
                                   TLabel(FTopLimitLabel_ForPrimitive),
                                   TLabel(FRightLimitLabel_ForPrimitive),
                                   TLabel(FBottomLimitLabel_ForPrimitive));

    FLeftLimitLabel_ForPrimitive.Visible := chkShowPrimitiveEdges.Checked;
    FTopLimitLabel_ForPrimitive.Visible := chkShowPrimitiveEdges.Checked;
    FRightLimitLabel_ForPrimitive.Visible := chkShowPrimitiveEdges.Checked;
    FBottomLimitLabel_ForPrimitive.Visible := chkShowPrimitiveEdges.Checked;
  end;
end;


procedure TfrClickerPrimitives.MenuItem_SavePrimitivesFileClick(Sender: TObject);
begin
  DoOnSaveFromMenu;
end;


procedure TfrClickerPrimitives.ClearAllEditingPoints;
var
  i: Integer;
begin
  for i := 0 to Length(FEditingPrimitivePoints.EditingPoints) - 1 do
    FEditingPrimitivePoints.EditingPoints[i].Free;

  SetLength(FEditingPrimitivePoints.EditingPoints, 0);
end;


procedure TfrClickerPrimitives.RebuildAllEditingPoints(APointCount: Integer);
var
  i: Integer;
begin
  if Length(FPrimitives) = 0 then
    Exit;

  if Length(FOrders) = 0 then
    Exit;

  if PageControlPreview.ActivePageIndex = -1 then
    Exit;

  if APointCount <> Length(FEditingPrimitivePoints.EditingPoints) then
  begin
    ClearAllEditingPoints;
    SetLength(FEditingPrimitivePoints.EditingPoints, APointCount);
    for i := 0 to Length(FEditingPrimitivePoints.EditingPoints) - 1 do
    begin
      FEditingPrimitivePoints.EditingPoints[i] := TEditingPoint.Create(PageControlPreview);
      FEditingPrimitivePoints.EditingPoints[i].Parent := FEditingPanels[PageControlPreview.ActivePageIndex];
      FEditingPrimitivePoints.EditingPoints[i].Visible := MenuItem_EditMode.Checked;
      FEditingPrimitivePoints.EditingPoints[i].Width := CEditingPointSize;
      FEditingPrimitivePoints.EditingPoints[i].Height := CEditingPointSize;
      FEditingPrimitivePoints.EditingPoints[i].BringToFront;
      FEditingPrimitivePoints.EditingPoints[i].Tag := i; //index in array
      FEditingPrimitivePoints.EditingPoints[i].OnMouseDown := HandleEditingPointsOnMouseDown;
      FEditingPrimitivePoints.EditingPoints[i].OnMouseMove := HandleEditingPointsOnMouseMove;
      FEditingPrimitivePoints.EditingPoints[i].OnMouseUp := HandleEditingPointsOnMouseUp;
      FEditingPrimitivePoints.EditingPoints[i].OnMouseEnter := HandleEditingPointsOnMouseEnter;
      FEditingPrimitivePoints.EditingPoints[i].OnMouseLeave := HandleEditingPointsOnMouseLeave;
    end;
  end;
end;


procedure TfrClickerPrimitives.PageControlPreviewChange(Sender: TObject);
var
  i: Integer;
begin
  //for i := 0 to Length(FEditingPrimitivePoints.EditingLabels) - 1 do
  //  FEditingPrimitivePoints.EditingLabels[i].Parent := PageControlPreview.ActivePage;
end;


procedure TfrClickerPrimitives.pnlHorizSplitterMouseDown(Sender: TObject;
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


procedure TfrClickerPrimitives.pnlHorizSplitterMouseMove(Sender: TObject;
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


procedure TfrClickerPrimitives.pnlHorizSplitterMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FHold := False;
end;


procedure TfrClickerPrimitives.FrameResize(Sender: TObject);
var
  NewLeft: Integer;
begin
  NewLeft := pnlHorizSplitter.Left;

  if NewLeft > Width - 270 then
    NewLeft := Width - 270;

  ResizeFrameSectionsBySplitter(NewLeft);
end;


procedure TfrClickerPrimitives.MenuItem_CopyColorUnderMouseCursorClick(
  Sender: TObject);
begin
  Clipboard.AsText := IntToHex(FimgPreviewTpColor, 6);
end;


procedure TfrClickerPrimitives.ResizeFrameSectionsBySplitter(NewLeft: Integer);
begin
  if NewLeft < pnlvstOI.Constraints.MinWidth then
    NewLeft := pnlvstOI.Constraints.MinWidth;

  if NewLeft > Width - 270 then
    NewLeft := Width - 270;

  pnlHorizSplitter.Left := NewLeft;

  pnlPreview.Left := pnlHorizSplitter.Left + pnlHorizSplitter.Width;
  pnlPreview.Width := Width - pnlPreview.Left;
  pnlvstOI.Width := pnlHorizSplitter.Left;
end;


procedure TfrClickerPrimitives.MenuItem_RemoveAllPrimitivesFromList(Sender: TObject);
var
  MenuData: POIMenuItemData;
  i: Integer;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    if MessageBox(Handle, 'Are you sure you want to remove all the primitives from list?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = IDNO then
      Exit;

    SetLength(FPrimitives, 0);

    for i := 0 to Length(FOrders) - 1 do
      SetLength(FOrders[i].Items, 0);

    tmrReloadOIContent.Enabled := True;
    DoOnTriggerOnControlsModified;  //the pmtv file is modified, not the template
  finally
    Dispose(MenuData);
  end;

  BuildImgLstPreviewPrimitives;
  BuildFontColorIconsList;
end;


procedure TfrClickerPrimitives.MenuItem_AddPrimitiveToList(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ValueStr: string;
  TempPrimitiveType, n, i: Integer;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    ValueStr := StringReplace(MenuData^.MenuItemCaption, '&', '', [rfReplaceAll]);
    ValueStr := Copy(ValueStr, Length(CAddPrimitiveMenuPrefix) + 1, MaxInt);
    TempPrimitiveType := PrimitiveTypeNameToIndex(ValueStr);
    n := Length(FPrimitives);

    if TempPrimitiveType = -1 then
    begin
      MessageBox(Handle, 'The current primitive type is not implemented.', PChar(Application.Title), MB_ICONERROR);
      Exit;
    end;

    SetLength(FPrimitives, n + 1);
    FPrimitives[n].PrimitiveType := TempPrimitiveType;
    FPrimitives[n].PrimitiveName := '"' + ValueStr + '"';

    ClearPrimitiveSelection;
    FPrimitives[n].Selected := True;

    CFillInDefaultValuesToPrimitives[TempPrimitiveType](FPrimitives[n]);

    for i := 0 to Length(FOrders) - 1 do
    begin
      SetLength(FOrders[i].Items, Length(FPrimitives));
      FOrders[i].Items[Length(FOrders[i].Items) - 1] := n;
    end;

    tmrReloadOIContent.Enabled := True;
    DoOnTriggerOnControlsModified;  //the pmtv file is modified, not the template

    RepaintAllCompositions;
  finally
    Dispose(MenuData);
  end;

  BuildImgLstPreviewPrimitives;
  BuildFontColorIconsList;
end;


procedure TfrClickerPrimitives.MenuItem_SetValueFromEnumItem(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ValueStr: string;
  TempPrimitiveType: Integer;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    ValueStr := StringReplace(MenuData^.MenuItemCaption, '&', '', [rfReplaceAll]);

    TempPrimitiveType := FPrimitives[MenuData^.PropertyIndex].PrimitiveType;
    if TempPrimitiveType = -1 then
    begin
      MessageBox(Handle, 'The current primitive type is not implemented.', PChar(Application.Title), MB_ICONERROR);
      Exit;
    end;

    CSetPrimitiveValueStrFunctions[TempPrimitiveType](FPrimitives[MenuData^.PropertyIndex], ValueStr, MenuData^.PropertyItemIndex);
    FOIFrame.CancelCurrentEditing;
    DoOnTriggerOnControlsModified;  //the pmtv file is modified, not the template

    RepaintAllCompositions;
  finally
    Dispose(MenuData);
  end;

  BuildImgLstPreviewPrimitives;
  BuildFontColorIconsList;
end;


procedure TfrClickerPrimitives.MenuItem_SetExternallyRenderedFile(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ValueStr: string;
  TempPrimitiveType: Integer;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    ValueStr := StringReplace(MenuData^.MenuItemCaption, '&', '', [rfReplaceAll]);
    if Pos(#8#7, ValueStr) > 0 then
      ValueStr := Copy(ValueStr, 1, Pos(#8#7, ValueStr) - 1);

    TempPrimitiveType := FPrimitives[MenuData^.PropertyIndex].PrimitiveType;
    if TempPrimitiveType = -1 then
    begin
      MessageBox(Handle, 'The current primitive type is not implemented.', PChar(Application.Title), MB_ICONERROR);
      Exit;
    end;

    CSetPrimitiveValueStrFunctions[TempPrimitiveType](FPrimitives[MenuData^.PropertyIndex], ValueStr, MenuData^.PropertyItemIndex);
    FOIFrame.CancelCurrentEditing;
    DoOnTriggerOnControlsModified;  //the pmtv file is modified, not the template

    RepaintAllCompositions;
    FOIFrame.RepaintNodeByLevel(MenuData.NodeLevel, MenuData.CategoryIndex, MenuData.PropertyIndex, MenuData.PropertyItemIndex);
  finally
    Dispose(MenuData);
  end;

 // BuildImgLstPreviewPrimitives;
end;


procedure TfrClickerPrimitives.MenuItem_BrowsePrimitivesImageFromDisk(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ValueStr: string;
  TempPrimitiveType: Integer;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    if not DoOnPictureOpenDialogExecute then
      Exit;

    ValueStr := DoOnGetPictureOpenDialogFileName;

    TempPrimitiveType := FPrimitives[MenuData^.PropertyIndex].PrimitiveType;
    if TempPrimitiveType = -1 then
    begin
      MessageBox(Handle, 'The current primitive type is not implemented.', PChar(Application.Title), MB_ICONERROR);
      Exit;
    end;

    CSetPrimitiveValueStrFunctions[TempPrimitiveType](FPrimitives[MenuData^.PropertyIndex], ValueStr, CImagePrimitive_Path_PropIndex);
    FOIFrame.CancelCurrentEditing;
    DoOnTriggerOnControlsModified;  //the pmtv file is modified, not the template

    RepaintAllCompositions;
    FOIFrame.RepaintNodeByLevel(MenuData.NodeLevel, MenuData.CategoryIndex, MenuData.PropertyIndex, MenuData.PropertyItemIndex);
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerPrimitives.MenuItem_RemovePrimitiveFromList(Sender: TObject);
var
  MenuData: POIMenuItemData;
  IndexToDel, OrderIndexToDel, i, j: Integer;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    if MessageBox(Handle, 'Are you sure you want to remove the current primitive from list?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = IDNO then
      Exit;

    IndexToDel := MenuData^.PropertyIndex;
    for i := IndexToDel to Length(FPrimitives) - 2 do
      FPrimitives[i] := FPrimitives[i + 1];

    SetLength(FPrimitives, Length(FPrimitives) - 1);

    for j := 0 to Length(FOrders) - 1 do
    begin
      //Delete the item, which is equal to IndexToDel
      OrderIndexToDel := -1;
      for i := 0 to Length(FOrders[j].Items) - 1 do
        if FOrders[j].Items[i] = IndexToDel then
        begin
          OrderIndexToDel := i;
          Break;
        end;

      if OrderIndexToDel > -1 then
      begin
        for i := OrderIndexToDel to Length(FOrders[j].Items) - 2 do
          FOrders[j].Items[i] := FOrders[j].Items[i + 1];

        SetLength(FOrders[j].Items, Length(FOrders[j].Items) - 1);
      end;
    end;

    for j := 0 to Length(FOrders) - 1 do
      for i := 0 to Length(FOrders[j].Items) - 1 do
        if FOrders[j].Items[i] > IndexToDel then
          Dec(FOrders[j].Items[i]);

    tmrReloadOIContent.Enabled := True;
    DoOnTriggerOnControlsModified;  //the pmtv file is modified, not the template

    RepaintAllCompositions;
  finally
    Dispose(MenuData);
  end;

  BuildImgLstPreviewPrimitives;
  BuildFontColorIconsList;
end;


procedure TfrClickerPrimitives.MenuItem_RemoveAllCompositionOrdersFromList(Sender: TObject);
var
  MenuData: POIMenuItemData;
  i: Integer;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    if MessageBox(Handle, 'Are you sure you want to remove all composition orders from list?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = IDNO then
      Exit;

    for i := 0 to Length(FOrders) - 1 do
      SetLength(FOrders[i].Items, 0);

    SetLength(FOrders, 0);

    FOIFrame.CancelCurrentEditing;
    tmrReloadOIContent.Enabled := True;
    DoOnTriggerOnControlsModified;  //the pmtv file is modified, not the template

    ClearPreviewTabs;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerPrimitives.MenuItem_AddCompositionOrderToList(Sender: TObject);
const
  CNewOrderName: string = 'new';
var
  MenuData: POIMenuItemData;
  n, i: Integer;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    n := Length(FOrders);

    SetLength(FOrders, n + 1);
    SetLength(FOrders[n].Items, Length(FPrimitives));

    FOrders[n].Name := CNewOrderName;
    for i := 0 to Length(FOrders[n].Items) - 1 do
      FOrders[n].Items[i] := i;

    tmrReloadOIContent.Enabled := True;
    DoOnTriggerOnControlsModified;  //the pmtv file is modified, not the template

    AddPreviewTabWithImage(CNewOrderName);
    RepaintAllCompositions;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerPrimitives.MenuItem_RemoveCompositionOrderFromList(Sender: TObject);
var
  MenuData: POIMenuItemData;
  IndexToDel: Integer;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    if MessageBox(Handle, 'Are you sure you want to remove the current composition order from list?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = IDNO then
      Exit;

    IndexToDel := MenuData^.PropertyIndex;
    DeleteOrderByIndex(IndexToDel);

    tmrReloadOIContent.Enabled := True;
    DoOnTriggerOnControlsModified;  //the pmtv file is modified, not the template
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerPrimitives.MenuItem_RepaintAllCompositions(Sender: TObject);
var
  MenuData: POIMenuItemData;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    RepaintAllCompositions;
  finally
    Dispose(MenuData);
  end;

  BuildImgLstPreviewPrimitives;
  BuildFontColorIconsList;
end;


procedure TfrClickerPrimitives.SetLabelsFromMouseOverPreviewImgPixelColor(APixelColor: TColor);
begin
  lblMouseOnPreviewImgRR.Caption := IntToHex(APixelColor and $FF, 2);
  lblMouseOnPreviewImgGG.Caption := IntToHex(APixelColor shr 8 and $FF, 2);
  lblMouseOnPreviewImgBB.Caption := IntToHex(APixelColor shr 16 and $FF, 2);
end;


procedure TfrClickerPrimitives.ClearPrimitiveSelection;
var
  i: Integer;
begin
  for i := 0 to Length(FPrimitives) - 1 do
    FPrimitives[i].Selected := False;
end;


procedure TfrClickerPrimitives.imgPreviewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  PreviewImage: TImage;
begin
  FCurrentMousePosOnPreviewImg.X := X;
  FCurrentMousePosOnPreviewImg.Y := Y;
  tmrDrawZoom.Enabled := True;

  if PageControlPreview.ActivePageIndex > -1 then
  begin
    lblMouseOnPreviewImg.Caption := IntToStr(X) + ' : ' + IntToStr(Y);
    PreviewImage := TImage(TScrollBox(PageControlPreview.Pages[PageControlPreview.ActivePageIndex].Tag).Tag);
    SetLabelsFromMouseOverPreviewImgPixelColor(PreviewImage.Canvas.Pixels[X, Y]);
  end;
end;


function PointBetweenEndpoints(X, X1, X2: Integer): Boolean;
begin
  Result := False; // :D

  if X1 = X2 then
    Result := X = X1
  else
    if X1 < X2 then
      Result := (X >= X1) and (X <= X2)
    else
      if X1 > X2 then
        Result := (X <= X1) and (X >= X2);
end;


function TfrClickerPrimitives.PointOnPrimitive(X, Y: Integer; var APrimitive: TPrimitiveRec; AWorkingImage: TImage): Boolean;
var
  X1, X2, Y1, Y2: Integer;
  r1, r2, rmax, cx, cy: Integer;
  TempSize: TSize;
  ListOfPoints: TStringList;
  MinP, MaxP, pxy, j: Integer;
begin
  Result := False;

  case APrimitive.PrimitiveType of
    CClkImagePrimitiveCmdIdx:
    begin
      X1 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.X1), 0);
      X2 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.X2), 0);
      Y1 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.Y1), 0);
      Y2 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.Y2), 0);

      Result := PointBetweenEndpoints(X, X1, X2) and PointBetweenEndpoints(Y, Y1, Y2);
    end;

    CClkLinePrimitiveCmdIdx:  ////////////////////////////////// ToDo: find a way to verify 2 points around the line (both for thin and thick lines)
    begin                                                     // Maybe, "create" two lines, one at x-1, y-1  and the other at x+1, y+1 (although far from perfect).
      X1 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.X1), 0);
      X2 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.X2), 0);
      Y1 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.Y1), 0);
      Y2 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.Y2), 0);

      if (X1 <> X2) and (Y1 <> Y2) then  //verify if the line is oblique, to avoid division by 0
      begin
        if Abs(((X - X1) / (X2 - X1)) - ((Y - Y1) / (Y2 - Y1))) < 0.1 then
        //if ((X1 - X) * (Y - Y2)) = ((X - X2) * (Y1 - Y)) then  //See stackoverflow.com, question 17692922
        //if ((Y - Y1) * (X2 - X1)) = ((X - X1) * (Y2 - Y1)) then  //See stackoverflow.com, question 17692922
          Result := PointBetweenEndpoints(X, X1, X2) and PointBetweenEndpoints(Y, Y1, Y2);
      end
      else
        if X1 = X2 then   //vertical line
        begin
          if X = X1 then
            Result := PointBetweenEndpoints(Y, Y1, Y2);
        end
        else
          if Y2 = Y1 then //horizontal line
          begin
            if Y = Y1 then
              Result := PointBetweenEndpoints(X, X1, X2);
          end;
    end;

    CClkRectPrimitiveCmdIdx:
    begin
      X1 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkRect.X1), 0);
      X2 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkRect.X2), 0);
      Y1 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkRect.Y1), 0);
      Y2 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkRect.Y2), 0);

      Result := PointBetweenEndpoints(X, X1, X2) and PointBetweenEndpoints(Y, Y1, Y2);
    end;

    CClkRoundedRectPrimitiveCmdIdx:
    begin
      X1 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkRoundedRect.X1), 0);
      X2 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkRoundedRect.X2), 0);
      Y1 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkRoundedRect.Y1), 0);
      Y2 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkRoundedRect.Y2), 0);

      Result := PointBetweenEndpoints(X, X1, X2) and PointBetweenEndpoints(Y, Y1, Y2);
    end;

    CClkGradientFill:
    begin
      X1 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.X1), 0);
      X2 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.X2), 0);
      Y1 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.Y1), 0);
      Y2 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.Y2), 0);

      Result := PointBetweenEndpoints(X, X1, X2) and PointBetweenEndpoints(Y, Y1, Y2);
    end;

    CClkText:
    begin
      TempSize := AWorkingImage.Canvas.TextExtent(DoOnEvaluateReplacementsFunc(APrimitive.ClkText.Text)); ///// This depends on current text settings (e.g. font name, size, style and orientation)

      X1 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkText.X), 0);
      X2 := X1 + TempSize.Width;
      Y1 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkText.Y), 0);
      Y2 := Y1 + TempSize.Height;

      Result := PointBetweenEndpoints(X, X1, X2) and PointBetweenEndpoints(Y, Y1, Y2);
    end;

    CClkDonutSector:
    begin
      r1 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkDonutSector.Radius1), 30);
      r2 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkDonutSector.Radius2), 90);
      cx := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkDonutSector.Cx), 100);
      cy := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkDonutSector.Cy), 100);
      rmax := Max(r1, r2);

      X1 := cx - rmax;
      X2 := cx + rmax;
      Y1 := cy - rmax;
      Y2 := cy + rmax;

      Result := PointBetweenEndpoints(X, X1, X2) and PointBetweenEndpoints(Y, Y1, Y2);
    end;

    CClkPolygon, CClkPolyBezier:  //Very inaccurate, so far. Only the bounding box is computed for now.
    begin
      ListOfPoints := TStringList.Create;
      try
        ListOfPoints.LineBreak := CPolygonPointLineBreak;

        ListOfPoints.Text := APrimitive.ClkPolygon.XPoints;
        RebuildAllEditingPoints(ListOfPoints.Count);
        MaxP := -MaxInt;
        MinP := MaxInt;
        for j := 0 to ListOfPoints.Count - 1 do
        begin
          pxy := StrToIntDef(DoOnEvaluateReplacementsFunc(ListOfPoints.Strings[j]), 0);
          MaxP := Max(MaxP, pxy);
          MinP := Min(MinP, pxy);
        end;

        X1 := MinP;
        X2 := MaxP;

        ListOfPoints.Text := APrimitive.ClkPolygon.YPoints;
        MaxP := -MaxInt;
        MinP := MaxInt;
        for j := 0 to ListOfPoints.Count - 1 do
        begin
          pxy := StrToIntDef(DoOnEvaluateReplacementsFunc(ListOfPoints.Strings[j]), 0);
          MaxP := Max(MaxP, pxy);
          MinP := Min(MinP, pxy);
        end;

        Y1 := MinP;
        Y2 := MaxP;
      finally
        ListOfPoints.Free;
      end;

      Result := PointBetweenEndpoints(X, X1, X2) and PointBetweenEndpoints(Y, Y1, Y2);
    end; //poly
  end;
end;


procedure TfrClickerPrimitives.SelectPrimitiveByOrderIndex(APrimitiveIndex, AOrderIndex: Integer);
var
  LeftLimitLabel_ForPrimitive: TPaintedLabel;
  TopLimitLabel_ForPrimitive: TPaintedLabel;
  RightLimitLabel_ForPrimitive: TPaintedLabel;
  BottomLimitLabel_ForPrimitive: TPaintedLabel;
begin
  ClearPrimitiveSelection;
  FPrimitives[APrimitiveIndex].Selected := True;
  FOIFrame.SelectNode(CPropertyLevel, CCategory_Primitives, APrimitiveIndex, -1, True, True);

  GetSelectionLabelsByOrderIndex(AOrderIndex,
                                 TLabel(LeftLimitLabel_ForPrimitive),
                                 TLabel(TopLimitLabel_ForPrimitive),
                                 TLabel(RightLimitLabel_ForPrimitive),
                                 TLabel(BottomLimitLabel_ForPrimitive));

  SelectPrimitiveOnPreviewImage(FPrimitives[APrimitiveIndex].PrimitiveType, APrimitiveIndex, AOrderIndex, LeftLimitLabel_ForPrimitive, TopLimitLabel_ForPrimitive, RightLimitLabel_ForPrimitive, BottomLimitLabel_ForPrimitive);

  FEditingPrimitivePoints.OrderIndex := AOrderIndex;
  FEditingPrimitivePoints.SelectedPrimitiveIndex := APrimitiveIndex;
  SetEditPointsFromLimitLabels(FPrimitives[APrimitiveIndex].PrimitiveType, LeftLimitLabel_ForPrimitive, TopLimitLabel_ForPrimitive, RightLimitLabel_ForPrimitive, BottomLimitLabel_ForPrimitive);
end;


procedure TfrClickerPrimitives.imgPreviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, OrderIdx: Integer;
  Img: TImage;
  Found: Boolean;
begin
  Img := Sender as TImage;

  FimgPreviewTpColor := Img.Canvas.Pixels[X, Y];
  imgPreviewColorUnderMouse.Canvas.Pen.Color := 2;
  imgPreviewColorUnderMouse.Canvas.Brush.Color := FimgPreviewTpColor;
  imgPreviewColorUnderMouse.Canvas.Rectangle(0, 0, imgPreviewColorUnderMouse.Width, imgPreviewColorUnderMouse.Height);
  MenuItem_CopyColorUnderMouseCursor.Bitmap := imgPreviewColorUnderMouse.Picture.Bitmap;

  if not MenuItem_EditMode.Checked then
  begin
    ClearPrimitiveSelection;
    Exit;
  end;

  OrderIdx := Img.Tag;
  Found := False;

  if FPrimitiveSettings.CompositorDirection = cdTopBot then
  begin
    for i := Length(FPrimitives) - 1 downto 0 do
      if PointOnPrimitive(X, Y, FPrimitives[FOrders[OrderIdx].Items[i]], Img) then
      begin
        SelectPrimitiveByOrderIndex(FOrders[OrderIdx].Items[i], OrderIdx);
        Found := True;
        Break;
      end;
  end
  else
  begin
    for i := 0 to Length(FPrimitives) - 1 do
      if PointOnPrimitive(X, Y, FPrimitives[FOrders[OrderIdx].Items[i]], Img) then
      begin
        SelectPrimitiveByOrderIndex(FOrders[OrderIdx].Items[i], OrderIdx);
        Found := True;
        Break;
      end;
  end;

  if not Found then
    FOIFrame.ClearNodeSelection;
end;


procedure TfrClickerPrimitives.imgPreviewMouseEnter(Sender: TObject);
var
  tp: TPoint;
  Idx: Integer;
  PreviewImage: TImage;
begin
  Idx := PageControlPreview.ActivePageIndex;
  if Idx = -1 then
    Exit;

  PreviewImage := TImage(TScrollBox(PageControlPreview.Pages[Idx].Tag).Tag);

  if PreviewImage = nil then
    Exit;

  PreviewImage.ShowHint := False;
  GetCursorPos(tp);
  ShowZoom(tp.X + 50, tp.Y + 50);
end;


procedure TfrClickerPrimitives.imgPreviewMouseLeave(Sender: TObject);
var
  Idx: Integer;
  PreviewImage: TImage;
  TempPanel: TPanel;
begin
  Idx := PageControlPreview.ActivePageIndex;
  if Idx = -1 then
    Exit;

  PreviewImage := TImage(TScrollBox(PageControlPreview.Pages[Idx].Tag).Tag);

  if PreviewImage = nil then
    Exit;

  PreviewImage.ShowHint := True;
  HideZoom;

  TempPanel := (PreviewImage.Parent as TPanel);
  TempPanel.Hint := 'Image size: ' + IntToStr(PreviewImage.Width) + ' : ' + IntToStr(PreviewImage.Height) + #13#10 +
                    'Bitmap size: ' + IntToStr(PreviewImage.Picture.Bitmap.Width) + ' : ' + IntToStr(PreviewImage.Picture.Bitmap.Height) + #13#10 +
                    'Green panel size: ' + IntToStr(TempPanel.Width) + ' : ' + IntToStr(TempPanel.Height) + #13#10#13#10 +
                    'When "Edit mode" is enabled (from pop-up menu), ' + #13#10 +
                    'clicking a primitive here on preview, selects it in the Object Inspector.';
end;


procedure TfrClickerPrimitives.pnlPreviewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  lblMouseOnPreviewImg.Caption := IntToStr(X) + ' : ' + IntToStr(Y);
end;


procedure TfrClickerPrimitives.BuildImgLstPreviewPrimitives;
var
  i: Integer;
  Bmp, PreviewBmp: TBitmap;
  PmtvCompositor: TPrimitivesCompositor;
  UsingHighContrast: Boolean;
  DestRect: TRect;
  CurrentPrimitive: TPrimitiveRecArr;

  CfgPenColor, CfgBrushColor, CfgFontColor: TColor;
  CfgPenStyle: TPenStyle;
  CfgPenWidth: Integer;
  CfgBrushStyle: TBrushStyle;
  CfgFont_Name: string;
  CfgFont_Orientation: Integer;
  CfgFont_Pitch: TFontPitch;
  CfgFont_Quality: TFontQuality;
  CfgFont_Size: Integer;
  CfgFont_Style: TFontStyles;
begin
  imglstPreviewPrimitives.Clear;
  DestRect.Left := 0;
  DestRect.Top := 0;
  DestRect.Width := 16;
  DestRect.Height := 16;

  SetLength(CurrentPrimitive, 1);
  PmtvCompositor := TPrimitivesCompositor.Create;
  try
    PmtvCompositor.FileIndex := FFileIndex;
    PmtvCompositor.OnEvaluateReplacementsFunc := HandleOnEvaluateReplacementsFunc;
    PmtvCompositor.OnLoadBitmap := HandleOnLoadBitmap;
    PmtvCompositor.OnLoadRenderedBitmap := HandleOnLoadRenderedBitmap;

    UsingHighContrast := chkHighContrast.Checked;

    CfgPenColor := clGreen;
    CfgBrushColor := clCream;
    CfgFontColor := clHighlight;
    CfgPenStyle := psSolid;
    CfgPenWidth := 1;
    CfgBrushStyle := bsSolid;

    CfgFont_Name := 'Tahoma';
    CfgFont_Orientation := 0;
    CfgFont_Pitch := fpDefault;
    CfgFont_Quality := fqNonAntialiased;
    CfgFont_Size := 8;
    CfgFont_Style := [];

    for i := 0 to Length(FPrimitives) - 1 do
    begin
      Bmp := TBitmap.Create;
      PreviewBmp := TBitmap.Create;
      try
        Bmp.PixelFormat := pf24bit;
        Bmp.Width := 16;
        Bmp.Height := 16;
        PreviewBmp.PixelFormat := pf24bit;
        CurrentPrimitive[0] := FPrimitives[i];
        PreviewBmp.Width := Max(16, PmtvCompositor.GetMaxX(PreviewBmp.Canvas, CurrentPrimitive));
        PreviewBmp.Height := Max(16, PmtvCompositor.GetMaxY(PreviewBmp.Canvas, CurrentPrimitive));

        PreviewBmp.Canvas.Brush.Style := bsSolid;  //reset to some default values
        PreviewBmp.Canvas.Pen.Color := clWhite;
        PreviewBmp.Canvas.Brush.Color := clWhite;
        PreviewBmp.Canvas.Rectangle(0, 0, PreviewBmp.Width, PreviewBmp.Height);

        PreviewBmp.Canvas.Pen.Color := CfgPenColor;
        PreviewBmp.Canvas.Brush.Color := CfgBrushColor;
        PreviewBmp.Canvas.Font.Color := CfgFontColor;
        PreviewBmp.Canvas.Pen.Style := CfgPenStyle;
        PreviewBmp.Canvas.Pen.Width := CfgPenWidth;
        PreviewBmp.Canvas.Brush.Style := CfgBrushStyle;
        PreviewBmp.Canvas.Font.Name := CfgFont_Name;
        PreviewBmp.Canvas.Font.Orientation := CfgFont_Orientation;
        PreviewBmp.Canvas.Font.Pitch := CfgFont_Pitch;
        PreviewBmp.Canvas.Font.Quality := CfgFont_Quality;
        PreviewBmp.Canvas.Font.Size := CfgFont_Size;
        PreviewBmp.Canvas.Font.Style := CfgFont_Style;

        PmtvCompositor.HighContrastOption1 := MenuItem_HighContrastOption1.Checked;
        PmtvCompositor.HighContrastOption2 := MenuItem_HighContrastOption2.Checked;
        PmtvCompositor.PreviewPrimitive(PreviewBmp, UsingHighContrast, FPrimitives, i);

        CfgPenColor := PreviewBmp.Canvas.Pen.Color;
        CfgBrushColor := PreviewBmp.Canvas.Brush.Color;
        CfgFontColor := PreviewBmp.Canvas.Font.Color;
        CfgPenStyle := PreviewBmp.Canvas.Pen.Style;
        CfgPenWidth := PreviewBmp.Canvas.Pen.Width;
        CfgBrushStyle := PreviewBmp.Canvas.Brush.Style;
        CfgFont_Name := PreviewBmp.Canvas.Font.Name;
        CfgFont_Orientation := PreviewBmp.Canvas.Font.Orientation;
        CfgFont_Pitch := PreviewBmp.Canvas.Font.Pitch;
        CfgFont_Quality := PreviewBmp.Canvas.Font.Quality;
        CfgFont_Size := PreviewBmp.Canvas.Font.Size;
        CfgFont_Style := PreviewBmp.Canvas.Font.Style;

        Bmp.Canvas.StretchDraw(DestRect, PreviewBmp);
        imglstPreviewPrimitives.AddMasked(Bmp, 1);  //using 1 as the mask color, allows for all standard colors to be used as background
      finally
        Bmp.Free;
        PreviewBmp.Free;
      end;
    end;
  finally
    SetLength(CurrentPrimitive, 0);
    PmtvCompositor.Free;
  end;
end;


procedure TfrClickerPrimitives.BuildFontColorIconsList;
var
  DummyFindControlOptions: TClkFindControlOptions;
  i: Integer;
begin
  SetLength(DummyFindControlOptions.MatchBitmapText, Length(FPrimitives));

  for i := 0 to Length(FPrimitives) - 1 do
    DummyFindControlOptions.MatchBitmapText[i] := FPrimitives[i].ClkSetFont;

  BuildFontColorIcons(imglstFontColorProperties, DummyFindControlOptions, FOnEvaluateReplacementsFunc);
end;


function TfrClickerPrimitives.DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if not Assigned(FOnLoadBitmap) then
    raise Exception.Create('OnLoadBitmap not assigned.')
  else
    Result := FOnLoadBitmap(ABitmap, AFileName);
end;


function TfrClickerPrimitives.DoOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if not Assigned(FOnLoadRenderedBitmap) then
    raise Exception.Create('OnLoadRenderedBitmap not assigned.')
  else
    Result := FOnLoadRenderedBitmap(ABitmap, AFileName);
end;


procedure TfrClickerPrimitives.DoOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages: TStringList);
begin
  if not Assigned(FOnGetListOfExternallyRenderedImages) then
    raise Exception.Create('OnGetListOfExternallyRenderedImages not assigned.')
  else
    FOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages);
end;


procedure TfrClickerPrimitives.DoOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
begin
  if not Assigned(FOnLoadPrimitivesFile) then
    raise Exception.Create('OnLoadPrimitivesFile not assigned.')
  else
    FOnLoadPrimitivesFile(AFileName, APrimitives, AOrders, ASettings);
end;


procedure TfrClickerPrimitives.DoOnSavePrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
begin
  if not Assigned(FOnSavePrimitivesFile) then
    raise Exception.Create('OnSavePrimitivesFile not assigned.')
  else
    FOnSavePrimitivesFile(AFileName, APrimitives, AOrders, ASettings);
end;


function TfrClickerPrimitives.DoOnEvaluateReplacementsFunc(s: string; Recursive: Boolean = True; AEvalTextCount: Integer = -1): string;
begin
  if not Assigned(FOnEvaluateReplacementsFunc) then
    raise Exception.Create('OnEvaluateReplacementsFunc not assigned.')
  else
    Result := FOnEvaluateReplacementsFunc(s, Recursive, AEvalTextCount);
end;


procedure TfrClickerPrimitives.DoOnTriggerOnControlsModified;
begin
  if Assigned(FOnTriggerOnControlsModified) then
  begin
    FOnTriggerOnControlsModified;
    lblModified.Show;
  end
  else
    raise Exception.Create('OnTriggerOnControlsModified not assigned.');
end;


procedure TfrClickerPrimitives.DoOnSaveFromMenu;
begin
  if Assigned(FOnSaveFromMenu) then
    FOnSaveFromMenu(Self)
  else
    raise Exception.Create('OnTriggerOnControlsModified not assigned.');
end;


function TfrClickerPrimitives.DoOnPictureOpenDialogExecute: Boolean;
begin
  if not Assigned(FOnPictureOpenDialogExecute) then
    raise Exception.Create('OnPictureOpenDialogExecute not assigned.')
  else
    Result := FOnPictureOpenDialogExecute;
end;


function TfrClickerPrimitives.DoOnGetPictureOpenDialogFileName: string;
begin
  if not Assigned(FOnGetPictureOpenDialogFileName) then
    raise Exception.Create('OnGetPictureOpenDialogFileName not assigned.')
  else
    Result := FOnGetPictureOpenDialogFileName;
end;


function TfrClickerPrimitives.GetOrderCount: Integer;
begin
  Result := Length(FOrders);
end;


function TfrClickerPrimitives.HandleOnOIGetCategoryCount: Integer;
begin
  Result := CCategoryCount;
end;


function TfrClickerPrimitives.HandleOnOIGetCategory(AIndex: Integer): string;
begin
  Result := CCategories[AIndex];
end;


function TfrClickerPrimitives.HandleOnOIGetCategoryValue(ACategoryIndex: Integer; var AEditorType: TOIEditorType): string;
begin
  Result := '';

  case ACategoryIndex of
    CCategory_Primitives:
      AEditorType := etUserEditor;

    CCategory_Orders:
      AEditorType := etUserEditor;

    else
      AEditorType := etNone;
  end;
end;


function TfrClickerPrimitives.HandleOnOIGetPropertyCount(ACategoryIndex: Integer): Integer;
begin
  case ACategoryIndex of
    CCategory_Primitives:
      Result := Length(FPrimitives);

    CCategory_Orders:
      Result := Length(FOrders);

    CCategory_Settings:
      Result := CSettingsClkPropCount;

    else
      Result := 0;
  end;
end;


function TfrClickerPrimitives.HandleOnOIGetPropertyName(ACategoryIndex, APropertyIndex: Integer): string;
begin
  case ACategoryIndex of
    CCategory_Primitives:
      Result := CPrimitiveNames[FPrimitives[APropertyIndex].PrimitiveType]; //IntToStr(APropertyIndex); // FPrimitives[APropertyIndex].ClkSetPen;

    CCategory_Orders:
      Result := 'Order [' + IntToStr(APropertyIndex) + ']';

    CCategory_Settings:
      Result := CSettingsNames[APropertyIndex];

    else
      Result := 'unknown';
  end;
end;


function TfrClickerPrimitives.HandleOnOIGetPropertyValue(ACategoryIndex, APropertyIndex: Integer; var AEditorType: TOIEditorType): string;
begin
  case ACategoryIndex of
    CCategory_Primitives:
    begin
      Result := FPrimitives[APropertyIndex].PrimitiveName;
      AEditorType := etTextWithArrow;
    end;

    CCategory_Orders:
    begin
      Result := FOrders[APropertyIndex].Name;
      AEditorType := etTextWithArrow;
    end;

    CCategory_Settings:
    begin
      case APropertyIndex of
        CCompositorDirection_PropIndex:
        begin
          Result := CPrimitiveSettingsPropEnumStrings[APropertyIndex]^[Ord(FPrimitiveSettings.CompositorDirection)];
          AEditorType := etEnumCombo;
        end

        else
          Result := 'unset property';
      end;
    end;

    else
      Result := 'unknown category';
  end;
end;


function TfrClickerPrimitives.HandleOnOIGetListPropertyItemCount(ACategoryIndex, APropertyIndex: Integer): Integer;
var
  i: Integer;
  s: string;
begin
  case ACategoryIndex of
    CCategory_Primitives:
    begin
      case FPrimitives[APropertyIndex].PrimitiveType of
        CClkPolygon, CClkPolyBezier:
        begin
          Result := 0;
          s := FPrimitives[APropertyIndex].ClkPolygon.XPoints;
          for i := 1 to Length(s) - 1 do  // -1, because two characters are compared
            if (s[i] = CPolygonPointLineBreak[1]) and (s[i + 1] = CPolygonPointLineBreak[2]) then
              Inc(Result);

          Result := Result shl 1; //two properties X and Y
          Inc(Result); //Reserved / Filled
        end;

        else
          Result := CClkPrimitivesTypeCounts[FPrimitives[APropertyIndex].PrimitiveType];
      end;
    end;

    CCategory_Orders:
      Result := Length(FOrders[APropertyIndex].Items);

    CCategory_Settings:
      Result := 0;

    else
      Result := 0;
  end;
end;


function TfrClickerPrimitives.HandleOnOIGetListPropertyItemName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
var
  IdxWithPropertyOffset: Integer;
begin
  Result := '';

  case ACategoryIndex of
    CCategory_Primitives:
    begin
      case FPrimitives[APropertyIndex].PrimitiveType of
        CClkPolygon, CClkPolyBezier:
        begin
          if AItemIndex = 0 then
            Result := CPrimitivesMainProperties[FPrimitives[APropertyIndex].PrimitiveType]^[AItemIndex].Name
          else
          begin
            IdxWithPropertyOffset := (AItemIndex - 1) shr 1;

            case FPrimitives[APropertyIndex].PrimitiveType of
              CClkPolygon:
              begin
                if (AItemIndex - 1) and 1 = 0 then //even
                  Result := 'X[' + IntToStr(IdxWithPropertyOffset) +  ']'
                else                               //odd
                  Result := 'Y[' + IntToStr(IdxWithPropertyOffset) +  ']';
              end;

              CClkPolyBezier:
              begin
                if (AItemIndex - 1) and 1 = 0 then //even
                  Result := 'X[' + IntToStr(IdxWithPropertyOffset) +  ']'
                else                               //odd
                  Result := 'Y[' + IntToStr(IdxWithPropertyOffset) +  ']';

                if ((AItemIndex - 1) shr 1) mod 3 in [1, 2] then
                  Result := '  CtrlP.' + Result   //control point
                else
                  Result := 'EndP.' + Result;     //end point
              end;

              else
                Result := 'bug: unhandled primitives type';
            end; //case
          end;
        end;

        else
          Result := CPrimitivesMainProperties[FPrimitives[APropertyIndex].PrimitiveType]^[AItemIndex].Name;
      end; //case
    end;

    CCategory_Orders:
      Result := FPrimitives[FOrders[APropertyIndex].Items[AItemIndex]].PrimitiveName;

    CCategory_Settings:
      Result := '';

    else
      ;
  end;
end;


function TfrClickerPrimitives.HandleOnOIGetListPropertyItemValue(ACategoryIndex, APropertyIndex, AItemIndex: Integer; var AEditorType: TOIEditorType): string;
begin
  Result := '';

  case ACategoryIndex of
    CCategory_Primitives:
    begin
      Result := CGetPrimitiveValueStrFunctions[FPrimitives[APropertyIndex].PrimitiveType](FPrimitives[APropertyIndex], AItemIndex);

      if FPrimitives[APropertyIndex].PrimitiveType in [CClkPolygon, CClkPolyBezier] then
      begin
        if AItemIndex = 0 then
          AEditorType := TOIEditorType.etText
        else
          AEditorType := TOIEditorType.etSpinText;
      end
      else
        AEditorType := CPrimitivesMainProperties[FPrimitives[APropertyIndex].PrimitiveType]^[AItemIndex].EditorType;
    end;

    CCategory_Orders:
    begin
      Result := '[' + IntToStr(FOrders[APropertyIndex].Items[AItemIndex]) + ']';
      AEditorType := etNone; //these items are not editable
    end;
  end;

end;


function TfrClickerPrimitives.HandleOnOIGetDataTypeName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
begin
  Result := '';
end;


function TfrClickerPrimitives.HandleOnOIGetExtraInfo(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
begin
  Result := '';
end;


procedure TfrClickerPrimitives.HandleOnOIGetImageIndexEx(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);
begin

  case Column of
    0:
    begin
      case ANodeLevel of
        CCategoryLevel:
          ;

        CPropertyLevel:
        begin
          case ACategoryIndex of
            CCategory_Primitives:
            begin
              ImageList := imglstPrimitives;
              ImageIndex := FPrimitives[APropertyIndex].PrimitiveType;
            end;

            CCategory_Orders:
            begin

            end;
          end;

        end; //CPropertyLevel

        CPropertyItemLevel:
        begin
          case ACategoryIndex of
            CCategory_Primitives:
              ;

            CCategory_Orders:
            begin
              ImageList := imglstPrimitives;
              ImageIndex := FPrimitives[FOrders[APropertyIndex].Items[AItemIndex]].PrimitiveType;
            end;
          end;
        end;
      end; //NodeLevel
    end; //Column 0

    1:
    begin
      case ANodeLevel of
        CCategoryLevel:
          ;

        CPropertyLevel:
        begin
          case ACategoryIndex of
            CCategory_Primitives:
            begin
              ImageList := imglstPreviewPrimitives;
              ImageIndex := APropertyIndex;
            end;

            CCategory_Orders:
            begin

            end;
          end;

        end; //CPropertyLevel

        CPropertyItemLevel:
        begin
          case ACategoryIndex of
            CCategory_Primitives:
            begin
              if FPrimitives[APropertyIndex].PrimitiveType = CClkSetFontPrimitiveCmdIdx then
              begin
                if AItemIndex in [CSetFontPrimitive_ForegroundColor_PropIndex, CSetFontPrimitive_BackgroundColor_PropIndex] then
                begin
                  ImageList := imglstFontColorProperties;
                  ImageIndex := APropertyIndex shl 1 + AItemIndex;

                  if ImageIndex > imglstFontColorProperties.Count - 1 then
                    BuildFontColorIconsList;
                end;
              end;
            end;

            CCategory_Orders:
            begin
              ImageList := imglstPreviewPrimitives;
              ImageIndex := FOrders[APropertyIndex].Items[AItemIndex];
            end;
          end;
        end;
      end;
    end; //column 1
  end; //case
end;


procedure TfrClickerPrimitives.HandleOnOIEditedText(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; ANewText: string);
var
  PmtvType: Integer;
  PrevValue: string;
begin
  case ACategoryIndex of
    CCategory_Primitives:
      case ANodeLevel of
        CPropertyLevel:
          if FPrimitives[APropertyIndex].PrimitiveName <> ANewText then
          begin
            FPrimitives[APropertyIndex].PrimitiveName := ANewText;
            DoOnTriggerOnControlsModified;
          end;

        CPropertyItemLevel:
        begin
          PmtvType := FPrimitives[APropertyIndex].PrimitiveType;
          PrevValue := CGetPrimitiveValueStrFunctions[PmtvType](FPrimitives[APropertyIndex], AItemIndex);
          CSetPrimitiveValueStrFunctions[PmtvType](FPrimitives[APropertyIndex], ANewText, AItemIndex);

          RepaintAllCompositions;
          BuildImgLstPreviewPrimitives;

          if ANewText <> PrevValue then
            DoOnTriggerOnControlsModified;

          if PmtvType = CClkSetFontPrimitiveCmdIdx then
            BuildFontColorIconsList;
        end;
      end;

    CCategory_Orders:
      case ANodeLevel of
        CPropertyLevel:
        begin
          if FOrders[APropertyIndex].Name <> ANewText then
          begin
            FOrders[APropertyIndex].Name := ANewText;
            PageControlPreview.Pages[APropertyIndex].Caption := ANewText;
            DoOnTriggerOnControlsModified;
          end;
        end;

        CPropertyItemLevel:
          ;
      end;

    CCategory_Settings:
      case ANodeLevel of
        CPropertyLevel:
        begin
          if APropertyIndex = CCompositorDirection_PropIndex then
            if FPrimitiveSettings.CompositorDirection <> CompositorDirectionToIndex(ANewText) then
            begin
              FPrimitiveSettings.CompositorDirection := CompositorDirectionToIndex(ANewText);
              RepaintAllCompositions;
              DoOnTriggerOnControlsModified;
            end;
        end;

        CPropertyItemLevel:
          ;
      end;
  end;
end;


function TfrClickerPrimitives.EditFontProperties(APmtvType, APropertyIndex, AItemIndex: Integer; var ANewItems: string): Boolean;
var
  TempFontDialog: TFontDialog;
begin
  Result := False;

  TempFontDialog := TFontDialog.Create(nil);
  try
    TempFontDialog.Font.Name := ANewItems;
    TempFontDialog.Font.Size := FPrimitives[APropertyIndex].ClkSetFont.FontSize;

    if FPrimitives[APropertyIndex].ClkSetFont.Bold then
      TempFontDialog.Font.Style := TempFontDialog.Font.Style + [fsBold];

    if FPrimitives[APropertyIndex].ClkSetFont.Italic then
      TempFontDialog.Font.Style := TempFontDialog.Font.Style + [fsItalic];

    if FPrimitives[APropertyIndex].ClkSetFont.Underline then
      TempFontDialog.Font.Style := TempFontDialog.Font.Style + [fsUnderline];

    if FPrimitives[APropertyIndex].ClkSetFont.StrikeOut then
      TempFontDialog.Font.Style := TempFontDialog.Font.Style + [fsStrikeOut];

    if not TempFontDialog.Execute then
      Exit;

    ANewItems := TempFontDialog.Font.Name;
    Result := True;

    FPrimitives[APropertyIndex].ClkSetFont.FontName := ANewItems; //redundant, because the OI will call another handler for the property itself
    FPrimitives[APropertyIndex].ClkSetFont.FontSize := TempFontDialog.Font.Size;
    FPrimitives[APropertyIndex].ClkSetFont.Bold := fsBold in TempFontDialog.Font.Style;
    FPrimitives[APropertyIndex].ClkSetFont.Italic := fsItalic in TempFontDialog.Font.Style;
    FPrimitives[APropertyIndex].ClkSetFont.Underline := fsUnderline in TempFontDialog.Font.Style;
    FPrimitives[APropertyIndex].ClkSetFont.StrikeOut := fsStrikeOut in TempFontDialog.Font.Style;
  finally
    TempFontDialog.Free;
  end;
end;


function TfrClickerPrimitives.HandleOnOIEditItems(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewItems: string): Boolean;
var
  PmtvType: Integer;
begin
  Result := False;
  if ACategoryIndex = CCategory_Primitives then
  begin
    case ANodeLevel of
      CPropertyLevel:
        ;

      CPropertyItemLevel:
      begin
        PmtvType := FPrimitives[APropertyIndex].PrimitiveType;
        if PmtvType = CClkSetFontPrimitiveCmdIdx then
          if AItemIndex = CSetFontPrimitive_FontName_PropIndex then
          begin
            Result := EditFontProperties(PmtvType, APropertyIndex, AItemIndex, ANewItems);

            if Result then
              DoOnTriggerOnControlsModified;
          end;
      end;
    end;

  end; //ACategoryIndex
end;



function TfrClickerPrimitives.HandleOnOIGetColorConstsCount(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer;
begin
  Result := 0;
end;


procedure TfrClickerPrimitives.HandleOnOIGetColorConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AColorItemIndex: Integer; var AColorName: string; var AColorValue: Int64);
begin

end;



function TfrClickerPrimitives.HandleOnOIGetEnumConstsCount(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer;
var
  PmtvType: Integer;
begin
  Result := 0;

  case ANodeLevel of
    CPropertyLevel:
      if ACategoryIndex = CCategory_Settings then
        Result := CPrimitiveSettingsPropEnumCounts[APropertyIndex];

    CPropertyItemLevel:
      if ACategoryIndex = CCategory_Primitives then
      begin
        Result := CPrimitivesPropEnumCounts[FPrimitives[APropertyIndex].PrimitiveType]^[AItemIndex];

        PmtvType := FPrimitives[APropertyIndex].PrimitiveType;
        if PmtvType = CClkSetFontPrimitiveCmdIdx then
          if AItemIndex = CSetFontPrimitive_FontName_PropIndex then
            Result := Screen.Fonts.Count;
      end;

  end;
end;


procedure TfrClickerPrimitives.HandleOnOIGetEnumConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEnumItemIndex: Integer; var AEnumItemName: string; var AEnumImgItemIndex: Integer; var AImgLst: TImageList);
var
  PmtvType: Integer;
begin
  AEnumItemName := '';

  case ANodeLevel of
    CPropertyLevel:
      if ACategoryIndex = CCategory_Settings then
        AEnumItemName := CPrimitiveSettingsPropEnumStrings[APropertyIndex]^[AEnumItemIndex];

    CPropertyItemLevel:
      if ACategoryIndex = CCategory_Primitives then
      begin
        PmtvType := FPrimitives[APropertyIndex].PrimitiveType;
        if PmtvType = CClkSetFontPrimitiveCmdIdx then
          if AItemIndex = CSetFontPrimitive_FontName_PropIndex then
          begin
            AEnumItemName := Screen.Fonts.Strings[AEnumItemIndex];
            Exit;
          end;

        AEnumItemName := CPrimitivesPropEnumStrings[FPrimitives[APropertyIndex].PrimitiveType]^[AItemIndex]^[AEnumItemIndex];
      end;
  end;
end;



procedure TfrClickerPrimitives.HandleOnOIPaintText(ANodeData: TNodeDataPropertyRec; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
  const TargetCanvas: TCanvas; Column: TColumnIndex; var TextType: TVSTTextType);
begin
  if ANodeData.Level = 0 then
  begin
    TargetCanvas.Font.Style := [fsBold];
    Exit;
  end;
end;



procedure TfrClickerPrimitives.HandleOnOIBeforeCellPaint(ANodeData: TNodeDataPropertyRec; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
  TargetCanvas: TCanvas; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin

end;



procedure TfrClickerPrimitives.HandleOnTextEditorMouseDown(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;



function TfrClickerPrimitives.HandleOnTextEditorMouseMove(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
end;



procedure TfrClickerPrimitives.HandleOnOITextEditorKeyUp(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin

end;



procedure TfrClickerPrimitives.HandleOnOITextEditorKeyDown(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin

end;



procedure TfrClickerPrimitives.HandleOnOIEditorAssignMenuAndTooltip(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; var APopupMenu: TPopupMenu; var AHint: string; var AShowHint: Boolean);
var
  PmtvType: Integer;
begin
  case ACategoryIndex of
    CCategory_Primitives:
    begin
      case ANodeLevel of
        CPropertyLevel:
          ;

        CPropertyItemLevel:
        begin
          PmtvType := FPrimitives[APropertyIndex].PrimitiveType;
          case PmtvType of
            CClkImagePrimitiveCmdIdx:
            begin
              AShowHint := True;

              case AItemIndex of
                CImagePrimitive_X1_PropIndex:
                  AHint := 'Current value: ' + DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkImage.X1);

                CImagePrimitive_Y1_PropIndex:
                  AHint := 'Current value: ' + DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkImage.Y1);

                CImagePrimitive_X2_PropIndex:
                  AHint := 'Current value: ' + DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkImage.X2);

                CImagePrimitive_Y2_PropIndex:
                  AHint := 'Current value: ' + DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkImage.Y2);

                CImagePrimitive_Stretch_PropIndex:
                begin
                  AHint := 'Set this to 1 or True, to stretch the image to the maximum available size of the composition.' + #13#10;
                  AHint := AHint + 'Current value: ' + DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkImage.Stretch);
                end;

                CImagePrimitive_RenderedExternally_PropIndex:
                begin
                  AHint := 'Set this to 1 or True, to load an externally rendered bmp (i.e. received from a server).' + #13#10 +
                           'The image is identified by path and "stored" in a separate in-mem file system (not the same one used on client-server execution).' + #13#10;
                  AHint := AHint + 'Current value: ' + DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkImage.RenderedExternally);
                end;

                CImagePrimitive_Transparent_PropIndex:
                begin
                  AHint := 'Set this to 1 or True, to have one of the colors as the transparency color.' + #13#10;
                  AHint := AHint + 'Current value: ' + DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkImage.Transparent);
                end;

                CImagePrimitive_TransparentMode_PropIndex:
                begin
                  AHint := 'Set this to 0 or Auto, or 1 or Fixed.' + #13#10;
                  AHint := AHint + 'Current value: ' + DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkImage.TransparentMode);
                end;

                CImagePrimitive_TransparentColor_PropIndex:
                begin
                  AHint := '6-digit hexa number (BGR), for a simple color, or 8-digit hexa number for a special system color.' + #13#10;
                  AHint := AHint + 'When using the value 20000000 (clDefault is a special system color), the transparency color is read from the bottom-left pixel of the image.' + #13#10;
                  AHint := AHint + 'Available special system colors: ' + #13#10;
                  AHint := AHint + '1FFFFFFF   (clNone)' + #13#10;
                  AHint := AHint + '20000000   (clDefault)' + #13#10;
                  AHint := AHint + 'Current value: ' + DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkImage.TransparentColor);
                end;
              end; //case
            end; //CClkImagePrimitiveCmdIdx

            CClkLinePrimitiveCmdIdx:
            begin
              AShowHint := True;

              case AItemIndex of
                CLinePrimitive_X1_PropIndex:
                  AHint := 'Current value: ' + DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkLine.X1);

                CLinePrimitive_Y1_PropIndex:
                  AHint := 'Current value: ' + DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkLine.Y1);

                CLinePrimitive_X2_PropIndex:
                  AHint := 'Current value: ' + DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkLine.X2);

                CLinePrimitive_Y2_PropIndex:
                  AHint := 'Current value: ' + DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkLine.Y2);

                CLinePrimitive_ShowEndpointPixel_PropIndex:
                  AHint := 'Current value: ' + DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkLine.ShowEndpointPixel);
              end; //case
            end; //CClkLinePrimitiveCmdIdx

            CClkRectPrimitiveCmdIdx:
            begin
              AShowHint := True;

              case AItemIndex of
                CRectPrimitive_X1_PropIndex:
                  AHint := 'Current value: ' + DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkRect.X1);

                CRectPrimitive_Y1_PropIndex:
                  AHint := 'Current value: ' + DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkRect.Y1);

                CRectPrimitive_X2_PropIndex:
                  AHint := 'Current value: ' + DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkRect.X2);

                CRectPrimitive_Y2_PropIndex:
                  AHint := 'Current value: ' + DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkRect.Y2);

                CRectPrimitive_ExtendToEndpointCorner_PropIndex:
                  AHint := 'Current value: ' + DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkRect.ExtendToEndpointCorner);
              end; //case
            end; //CClkRectPrimitiveCmdIdx
          end; //case
        end; //item leve
      end; //case
    end; //CCategory_Primitives
  end;
end;


procedure TfrClickerPrimitives.HandleOnOIGetFileDialogSettings(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var AFilter, AInitDir: string);
begin

end;


procedure TfrClickerPrimitives.HandleOnOIArrowEditorClick(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer);
var
  tp: TPoint;
  i: Integer;
  EnumItemName: string;
  PmtvType: Integer;
  ListOfExternallyRenderedImages: TStringList;
begin
  case ACategoryIndex of
    CCategory_Primitives:
      case ANodeLevel of
        CCategoryLevel:
          ;

        CPropertyLevel:
        begin
          FOIEditorMenu.Items.Clear;

          AddMenuItemToPopupMenu(FOIEditorMenu, CRemovePrimitiveMenuPrefix + '"' + FPrimitives[APropertyIndex].PrimitiveName + '"', MenuItem_RemovePrimitiveFromList, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, nil);

          if FPrimitives[APropertyIndex].PrimitiveType in [CClkPolygon, CClkPolyBezier] then
          begin
            AddMenuItemToPopupMenu(FOIEditorMenu, 'Add point to polygon', MenuItem_AddPointToPolygon, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, nil);
            AddMenuItemToPopupMenu(FOIEditorMenu, 'Remove last point from polygon', MenuItem_RemoveLastPointFromPolygon, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, nil);
          end;

          GetCursorPos(tp);
          FOIEditorMenu.PopUp(tp.X, tp.Y);
        end;

        CPropertyItemLevel:
        begin
          FOIEditorMenu.Items.Clear;

          //menus for enum like properties
          for i := 0 to CPrimitivesPropEnumCounts[FPrimitives[APropertyIndex].PrimitiveType]^[AItemIndex] - 1 do
          begin
            EnumItemName := CPrimitivesPropEnumStrings[FPrimitives[APropertyIndex].PrimitiveType]^[AItemIndex]^[i];
            AddMenuItemToPopupMenu(FOIEditorMenu, EnumItemName, MenuItem_SetValueFromEnumItem, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, nil);
          end;

          //menus for other types of properties
          PmtvType := FPrimitives[APropertyIndex].PrimitiveType;
          if PmtvType = CClkImagePrimitiveCmdIdx then
          begin
            case AItemIndex of
              CImagePrimitive_Path_PropIndex:
                if DoOnEvaluateReplacementsFunc(FPrimitives[APropertyIndex].ClkImage.RenderedExternally) = '1' then
                begin
                  ListOfExternallyRenderedImages := TStringList.Create;
                  try
                    DoOnGetListOfExternallyRenderedImages(ListOfExternallyRenderedImages);
                    for i := 0 to ListOfExternallyRenderedImages.Count - 1 do
                      AddMenuItemToPopupMenu(FOIEditorMenu, ListOfExternallyRenderedImages.Strings[i], MenuItem_SetExternallyRenderedFile, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, nil);
                  finally
                    ListOfExternallyRenderedImages.Free;
                  end;
                end
                else
                begin
                  AddMenuItemToPopupMenu(FOIEditorMenu, 'Browse...', MenuItem_BrowsePrimitivesImageFromDisk, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, nil);
                end;
            end; //case
          end;

          GetCursorPos(tp);
          FOIEditorMenu.PopUp(tp.X, tp.Y);
        end;

      end;

    CCategory_Orders:
      case ANodeLevel of
        CCategoryLevel:
          ;

        CPropertyLevel:
        begin
          FOIEditorMenu.Items.Clear;

          AddMenuItemToPopupMenu(FOIEditorMenu, CRemoveOrderMenuPrefix + '"' + FOrders[APropertyIndex].Name + '"', MenuItem_RemoveCompositionOrderFromList, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, nil);

          GetCursorPos(tp);
          FOIEditorMenu.PopUp(tp.X, tp.Y);
        end;

        CPropertyItemLevel:
          ;
      end;
  end; //ACategoryIndex
end;


procedure TfrClickerPrimitives.HandleOnOIUserEditorClick(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ARepaintValue: Boolean);
var
  tp: TPoint;
  i: Integer;
begin
  case ACategoryIndex of
    CCategory_Primitives:
      case ANodeLevel of
        CCategoryLevel:
        begin
          FOIEditorMenu.Items.Clear;

          for i := 0 to CPrimitiveTypeCount - 1 do
          begin
            AddMenuItemToPopupMenu(FOIEditorMenu, CAddPrimitiveMenuPrefix + CPrimitiveNames[i], MenuItem_AddPrimitiveToList, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, nil);
            imgFontColorBuffer.Canvas.Pen.Color := clWhite;
            imgFontColorBuffer.Canvas.Brush.Color := clWhite;
            imgFontColorBuffer.Canvas.Rectangle(0, 0, imgFontColorBuffer.Width, imgFontColorBuffer.Height);

            imgFontColorBuffer.Picture.Graphic.Transparent := False;
            imglstPrimitives.Draw(imgFontColorBuffer.Canvas, 0, 0, i, dsNormal, itImage);
            FOIEditorMenu.Items.Items[i].Bitmap := TBitmap.Create;
            FOIEditorMenu.Items.Items[i].Bitmap.Width := 16;
            FOIEditorMenu.Items.Items[i].Bitmap.Height := 16;
            FOIEditorMenu.Items.Items[i].Bitmap.Transparent := False;
            FOIEditorMenu.Items.Items[i].Bitmap.Canvas.Draw(0, 0, imgFontColorBuffer.Picture.Graphic);
          end;

          AddMenuItemToPopupMenu(FOIEditorMenu, '-', nil, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, nil);
          AddMenuItemToPopupMenu(FOIEditorMenu, 'Remove all primitives from list...', MenuItem_RemoveAllPrimitivesFromList, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, nil);

          GetCursorPos(tp);
          FOIEditorMenu.PopUp(tp.X, tp.Y);
        end;

        CPropertyLevel:
          ;

        CPropertyItemLevel:
          ;
      end; //case ANodeLevel

    CCategory_Orders:
      case ANodeLevel of
        CCategoryLevel:
        begin
          FOIEditorMenu.Items.Clear;

          AddMenuItemToPopupMenu(FOIEditorMenu, 'Add composition order to list', MenuItem_AddCompositionOrderToList, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, nil);
          FOIEditorMenu.Items.Items[FOIEditorMenu.Items.Count - 1].Bitmap := CreateBitmapForMenu(imglstPrimitivesEditorMenu, 0);

          AddMenuItemToPopupMenu(FOIEditorMenu, '-', nil, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, nil);

          AddMenuItemToPopupMenu(FOIEditorMenu, 'Remove all composition orders from list...', MenuItem_RemoveAllCompositionOrdersFromList, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, nil);
          FOIEditorMenu.Items.Items[FOIEditorMenu.Items.Count - 1].Bitmap := CreateBitmapForMenu(imglstPrimitivesEditorMenu, 1);

          AddMenuItemToPopupMenu(FOIEditorMenu, '-', nil, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, nil);

          AddMenuItemToPopupMenu(FOIEditorMenu, 'Repaint all compositions', MenuItem_RepaintAllCompositions, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, nil);
          FOIEditorMenu.Items.Items[FOIEditorMenu.Items.Count - 1].Bitmap := CreateBitmapForMenu(imglstPrimitivesEditorMenu, 2);

          GetCursorPos(tp);
          FOIEditorMenu.PopUp(tp.X, tp.Y);
        end;

        CPropertyLevel:
          ;

        CPropertyItemLevel:
          ;
      end; //case ANodeLevel

    else
      ;
  end; //case ACategoryIndex
end;



function TfrClickerPrimitives.HandleOnOIBrowseFile(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  AFilter, ADialogInitDir: string; var Handled: Boolean; AReturnMultipleFiles: Boolean = False): string;
begin
  Handled := False;
  Result := '';
end;


procedure TfrClickerPrimitives.HandleOnOIAfterSpinTextEditorChanging(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewValue: string);
begin

end;


procedure TfrClickerPrimitives.SetEditPointsFromLimitLabels(APmtvType: Integer; ALeftLimitLabel, ATopLimitLabel, ARightLimitLabel, ABottomLimitLabel: TPaintedLabel);
begin
  if (ALeftLimitLabel = nil) or (ATopLimitLabel = nil) then //it's enough to verify only these two
    Exit;

  case APmtvType of
    CClkImagePrimitiveCmdIdx, CClkRectPrimitiveCmdIdx, CClkRoundedRectPrimitiveCmdIdx, CClkGradientFill:
    begin
      RebuildAllEditingPoints(4);
      FEditingPrimitivePoints.EditingPoints[0].Left := ALeftLimitLabel.Left - CEditingPointOffset;
      FEditingPrimitivePoints.EditingPoints[0].Top := ATopLimitLabel.Top - CEditingPointOffset;
      FEditingPrimitivePoints.EditingPoints[1].Left := ARightLimitLabel.Left - CEditingPointOffset;
      FEditingPrimitivePoints.EditingPoints[1].Top := ATopLimitLabel.Top - CEditingPointOffset;
      FEditingPrimitivePoints.EditingPoints[2].Left := ALeftLimitLabel.Left - CEditingPointOffset;
      FEditingPrimitivePoints.EditingPoints[2].Top := ABottomLimitLabel.Top - CEditingPointOffset;
      FEditingPrimitivePoints.EditingPoints[3].Left := ARightLimitLabel.Left - CEditingPointOffset;
      FEditingPrimitivePoints.EditingPoints[3].Top := ABottomLimitLabel.Top - CEditingPointOffset;
    end;

    CClkLinePrimitiveCmdIdx:
    begin
      RebuildAllEditingPoints(2);
      FEditingPrimitivePoints.EditingPoints[0].Left := ALeftLimitLabel.Left - CEditingPointOffset;
      FEditingPrimitivePoints.EditingPoints[0].Top := ATopLimitLabel.Top - CEditingPointOffset;
      FEditingPrimitivePoints.EditingPoints[1].Left := ARightLimitLabel.Left - CEditingPointOffset;
      FEditingPrimitivePoints.EditingPoints[1].Top := ABottomLimitLabel.Top - CEditingPointOffset;
    end;

    CClkText:
    begin
      RebuildAllEditingPoints(1);
      FEditingPrimitivePoints.EditingPoints[0].Left := ALeftLimitLabel.Left - CEditingPointOffset;
      FEditingPrimitivePoints.EditingPoints[0].Top := ATopLimitLabel.Top - CEditingPointOffset;
    end;

    CClkDonutSector:
    begin
      RebuildAllEditingPoints(1);
      FEditingPrimitivePoints.EditingPoints[0].Left := (ALeftLimitLabel.Left + ARightLimitLabel.Left) shr 1 - CEditingPointOffset;
      FEditingPrimitivePoints.EditingPoints[0].Top := (ATopLimitLabel.Top + ABottomLimitLabel.Top) shr 1 - CEditingPointOffset;
    end;

    CClkPolygon, CClkPolyBezier:
      ; //nothing to update for polygons
  end; //case
end;


procedure TfrClickerPrimitives.SetLimitLabelFromPolygon(APrimitiveIndex: Integer; ALeftLimitLabel, ATopLimitLabel, ARightLimitLabel, ABottomLimitLabel: TPaintedLabel);
var
  ListOfPoints: TStringList;
  MinP, MaxP, pxy, j: Integer;
begin
  ListOfPoints := TStringList.Create;
  try
    ListOfPoints.LineBreak := CPolygonPointLineBreak;

    ListOfPoints.Text := FPrimitives[APrimitiveIndex].ClkPolygon.XPoints;
    RebuildAllEditingPoints(ListOfPoints.Count);
    MaxP := -MaxInt;
    MinP := MaxInt;
    for j := 0 to ListOfPoints.Count - 1 do
    begin
      pxy := StrToIntDef(DoOnEvaluateReplacementsFunc(ListOfPoints.Strings[j]), 0);
      MaxP := Max(MaxP, pxy);
      MinP := Min(MinP, pxy);
      FEditingPrimitivePoints.EditingPoints[j].Left := pxy - CEditingPointOffset;
    end;

    ALeftLimitLabel.Left := MinP;
    ARightLimitLabel.Left := MaxP;

    ListOfPoints.Text := FPrimitives[APrimitiveIndex].ClkPolygon.YPoints;
    MaxP := -MaxInt;
    MinP := MaxInt;
    for j := 0 to ListOfPoints.Count - 1 do
    begin
      pxy := StrToIntDef(DoOnEvaluateReplacementsFunc(ListOfPoints.Strings[j]), 0);
      MaxP := Max(MaxP, pxy);
      MinP := Min(MinP, pxy);
      FEditingPrimitivePoints.EditingPoints[j].Top := pxy - CEditingPointOffset;
    end;

    ATopLimitLabel.Top := MinP;
    ABottomLimitLabel.Top := MaxP;
  finally
    ListOfPoints.Free;
  end;
end;


procedure TfrClickerPrimitives.SelectPrimitiveOnPreviewImage(APmtvType, APrimitiveIndex, AOrderIndex: Integer; ALeftLimitLabel, ATopLimitLabel, ARightLimitLabel, ABottomLimitLabel: TPaintedLabel);
var
  x1, x2, y1, y2, cx, cy, r1, r2, rmax: Integer;
  TempText: string;
  TempBmp: TBitmap;
  TextSize: TSize;
  TempImage: TImage;
begin
  case APmtvType of
    CClkImagePrimitiveCmdIdx:
    begin
      ALeftLimitLabel.Left := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkImage.X1), 0);
      ATopLimitLabel.Top := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkImage.Y1), 0);
      ARightLimitLabel.Left := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkImage.X2), 100);
      ABottomLimitLabel.Top := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkImage.Y2), 100);
    end;

    CClkLinePrimitiveCmdIdx:
    begin
      ALeftLimitLabel.Left := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkLine.X1), 0);
      ATopLimitLabel.Top := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkLine.Y1), 0);
      ARightLimitLabel.Left := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkLine.X2), 100);
      ABottomLimitLabel.Top := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkLine.Y2), 100);
    end;

    CClkRectPrimitiveCmdIdx:
    begin
      ALeftLimitLabel.Left := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkRect.X1), 0);
      ATopLimitLabel.Top := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkRect.Y1), 0);
      ARightLimitLabel.Left := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkRect.X2), 100);
      ABottomLimitLabel.Top := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkRect.Y2), 100);
    end;

    CClkRoundedRectPrimitiveCmdIdx:
    begin
      ALeftLimitLabel.Left := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkRoundedRect.X1), 0);
      ATopLimitLabel.Top := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkRoundedRect.Y1), 0);
      ARightLimitLabel.Left := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkRoundedRect.X2), 100);
      ABottomLimitLabel.Top := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkRoundedRect.Y2), 100);
    end;

    CClkGradientFill:
    begin
      ALeftLimitLabel.Left := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkGradientFill.X1), 0);
      ATopLimitLabel.Top := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkGradientFill.Y1), 0);
      ARightLimitLabel.Left := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkGradientFill.X2), 100);
      ABottomLimitLabel.Top := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkGradientFill.Y2), 100);
    end;

    CClkText:
    begin
      TempText := DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkText.Text);
      TempBmp := TBitmap.Create;
      try
        //init to some defaults
        if (AOrderIndex > -1) and (AOrderIndex < Length(FOrders)) then
        begin
          TempImage := TImage(TScrollBox(PageControlPreview.Pages[AOrderIndex].Tag).Tag);
          TempBmp.Canvas.Font.Name := TempImage.Canvas.Font.Name;
          TempBmp.Canvas.Font.Size := TempImage.Canvas.Font.Size;
          TempBmp.Canvas.Font.Style := TempImage.Canvas.Font.Style;
        end
        else
        begin
          TempBmp.Canvas.Font.Name := 'Tahoma';
          TempBmp.Canvas.Font.Size := 8;
          TempBmp.Canvas.Font.Style := [];
        end;

        //ToDo
        // depending on FPrimitiveSettings.CompositorDirection, the FOrders array should be iterated, to get the first SetFont "primitive"
        //based on that, the TempBmp.Canvas.Font property should be set

        TextSize := TempBmp.Canvas.TextExtent(TempText);
        x1 := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkText.X), 0);
        x2 := x1 + TextSize.cx;
        y1 := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkText.Y), 0);
        y2 := y1 + TextSize.cy;

        ALeftLimitLabel.Left := x1;
        ATopLimitLabel.Top := y1;
        ARightLimitLabel.Left := x2;
        ABottomLimitLabel.Top := y2;
      finally
        TempBmp.Free;
      end;
    end;

    CClkDonutSector:
    begin
      r1 := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkDonutSector.Radius1), 30);
      r2 := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkDonutSector.Radius2), 90);
      cx := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkDonutSector.Cx), 100);
      cy := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[APrimitiveIndex].ClkDonutSector.Cy), 100);
      rmax := Max(r1, r2);

      x1 := cx - rmax;
      x2 := cx + rmax;
      y1 := cy - rmax;
      y2 := cy + rmax;

      ALeftLimitLabel.Left := x1;
      ATopLimitLabel.Top := y1;
      ARightLimitLabel.Left := x2;
      ABottomLimitLabel.Top := y2;
    end;

    CClkPolygon, CClkPolyBezier:
    begin
      SetLimitLabelFromPolygon(APrimitiveIndex, ALeftLimitLabel, ATopLimitLabel, ARightLimitLabel, ABottomLimitLabel);
      //
    end; //Polygon
  end; //case
end;


procedure TfrClickerPrimitives.HandleOnOISelectedNode(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, Column: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PmtvType, i: Integer;
  FLeftLimitLabel_ForPrimitive: TPaintedLabel;
  FTopLimitLabel_ForPrimitive: TPaintedLabel;
  FRightLimitLabel_ForPrimitive: TPaintedLabel;
  FBottomLimitLabel_ForPrimitive: TPaintedLabel;
begin
  case CategoryIndex of
    CCategory_Primitives:
      case NodeLevel of
        CCategoryLevel:
          ;

        CPropertyLevel, CPropertyItemLevel:
        begin
          PmtvType := FPrimitives[PropertyIndex].PrimitiveType;

          for i := 0 to Length(FOrders) - 1 do
          begin
            GetSelectionLabelsByOrderIndex(i,
                                           TLabel(FLeftLimitLabel_ForPrimitive),
                                           TLabel(FTopLimitLabel_ForPrimitive),
                                           TLabel(FRightLimitLabel_ForPrimitive),
                                           TLabel(FBottomLimitLabel_ForPrimitive));

            SelectPrimitiveOnPreviewImage(PmtvType, PropertyIndex, i, FLeftLimitLabel_ForPrimitive, FTopLimitLabel_ForPrimitive, FRightLimitLabel_ForPrimitive, FBottomLimitLabel_ForPrimitive);
          end; //for

          if CategoryIndex = CCategory_Primitives then
            if NodeLevel in [CPropertyLevel, CPropertyItemLevel] then
            begin
              FEditingPrimitivePoints.OrderIndex := PageControlPreview.ActivePageIndex;
              FEditingPrimitivePoints.SelectedPrimitiveIndex := PropertyIndex;
              SetEditPointsFromLimitLabels(PmtvType, FLeftLimitLabel_ForPrimitive, FTopLimitLabel_ForPrimitive, FRightLimitLabel_ForPrimitive, FBottomLimitLabel_ForPrimitive);
            end;
        end; //PropertyLevel

        //CPropertyItemLevel:
        //begin
        //  FEditingPrimitivePoints.OrderIndex := PageControlPreview.ActivePageIndex;
        //end;
      end; //case NodeLevel
  end; //case CategoryIndex
end;


procedure TfrClickerPrimitives.HandleOnOIDragAllowed(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer; var Allowed: Boolean);
begin
  Allowed := (CategoryIndex = CCategory_Orders) and
             (((NodeLevel = CPropertyLevel) and (PropertyItemIndex = -1)) or
             ((NodeLevel = CPropertyItemLevel) and (PropertyItemIndex > -1)));
end;


procedure TfrClickerPrimitives.HandleOnOIDragOver(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, SrcNodeLevel, SrcCategoryIndex, SrcPropertyIndex, SrcPropertyItemIndex: Integer; Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode; var Effect: DWORD; var Accept: Boolean);
var
  MatchingCategory: Boolean;
  SameSrcAndDest: Boolean;
  DraggingOrderName, DraggingOrderItem: Boolean;
  IsPropertyLevel, IsPropertyItemLevel: Boolean;
  DraggingFromTheSameOrder: Boolean;
begin
  MatchingCategory := CategoryIndex = CCategory_Orders;
  SameSrcAndDest := NodeLevel = SrcNodeLevel;
  IsPropertyLevel := NodeLevel = CPropertyLevel;
  IsPropertyItemLevel := NodeLevel = CPropertyItemLevel;

  DraggingOrderName := IsPropertyLevel and (PropertyItemIndex = -1);
  DraggingOrderItem := IsPropertyItemLevel and (PropertyItemIndex > -1);
  DraggingFromTheSameOrder := (PropertyIndex = SrcPropertyIndex) and IsPropertyItemLevel;

  Accept := MatchingCategory and
            SameSrcAndDest and
            (DraggingOrderName or (DraggingOrderItem and DraggingFromTheSameOrder));
end;


procedure TfrClickerPrimitives.HandleOnOIDragDrop(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, SrcNodeLevel, SrcCategoryIndex, SrcPropertyIndex, SrcPropertyItemIndex: Integer; Shift: TShiftState; const Pt: TPoint; var Effect: DWORD; Mode: TDropMode);
begin
  if not ((CategoryIndex = CCategory_Orders) and (SrcCategoryIndex = CCategory_Orders)) then
    Exit;

  //dragging an order
  if (NodeLevel = CPropertyLevel) and (SrcNodeLevel = CPropertyLevel) then
    if (PropertyItemIndex = -1) and (SrcPropertyItemIndex = -1) then
      if PropertyIndex <> SrcPropertyIndex then
      begin
        MoveOrder(SrcPropertyIndex, PropertyIndex);

        FOIFrame.ReloadPropertyItems(CategoryIndex, PropertyIndex, True);
        FOIFrame.ReloadPropertyItems(SrcCategoryIndex, SrcPropertyIndex, True);
        DoOnTriggerOnControlsModified;
      end;

  //dragging an order item
  if (NodeLevel = CPropertyItemLevel) and (SrcNodeLevel = CPropertyItemLevel) then
    if (PropertyItemIndex > -1) and (SrcPropertyItemIndex > -1) then
      if PropertyIndex = SrcPropertyIndex then
        if PropertyItemIndex <> SrcPropertyItemIndex then
        begin
          MoveOrderItem(PropertyIndex, SrcPropertyItemIndex, PropertyItemIndex);

          FOIFrame.ReloadPropertyItems(CategoryIndex, PropertyIndex, True);
          RepaintAllCompositions;
          DoOnTriggerOnControlsModified;
        end;
end;


//Primitive compositor handlers

function TfrClickerPrimitives.HandleOnEvaluateReplacementsFunc(s: string; Recursive: Boolean = True; AEvalTextCount: Integer = -1): string;
begin
  Result := DoOnEvaluateReplacementsFunc(s, Recursive, AEvalTextCount);
end;


function TfrClickerPrimitives.HandleOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  Result := DoOnLoadBitmap(ABitmap, AFileName);
end;


function TfrClickerPrimitives.HandleOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  Result := DoOnLoadRenderedBitmap(ABitmap, AFileName);
end;


procedure TfrClickerPrimitives.UpdatePrimitiveVertex(AEditingPointIndex, NewX, NewY: Integer);
var
  TempPrimitiveType: Integer;
  ListOfPoints: TStringList;

  TempText: string;
  TempBmp: TBitmap;
  TextSize: TSize;
  cx, cy, r1, r2, rmax, x1, x2, y1, y2: Integer;
  TempImage: TImage;

  FLeftLimitLabel_ForPrimitive: TPaintedLabel;
  FTopLimitLabel_ForPrimitive: TPaintedLabel;
  FRightLimitLabel_ForPrimitive: TPaintedLabel;
  FBottomLimitLabel_ForPrimitive: TPaintedLabel;
begin
  TempPrimitiveType := FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].PrimitiveType;

  if Length(FOrders) = 0 then
    Exit;

  GetSelectionLabelsByOrderIndex(0,
                                   TLabel(FLeftLimitLabel_ForPrimitive),
                                   TLabel(FTopLimitLabel_ForPrimitive),
                                   TLabel(FRightLimitLabel_ForPrimitive),
                                   TLabel(FBottomLimitLabel_ForPrimitive));

  case TempPrimitiveType of
    CClkImagePrimitiveCmdIdx:
    begin
      case AEditingPointIndex of
        0:
        begin
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.X1 := IntToStr(NewX);
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.Y1 := IntToStr(NewY);
        end;

        1:
        begin
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.X2 := IntToStr(NewX);
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.Y1 := IntToStr(NewY);
        end;

        2:
        begin
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.X1 := IntToStr(NewX);
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.Y2 := IntToStr(NewY);
        end;

        3:
        begin
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.X2 := IntToStr(NewX);
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.Y2 := IntToStr(NewY);
        end;
      end; //case
    end;

    CClkLinePrimitiveCmdIdx:
    begin
      case AEditingPointIndex of
        0:
        begin
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkLine.X1 := IntToStr(NewX);
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkLine.Y1 := IntToStr(NewY);
        end;

        1:
        begin
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkLine.X2 := IntToStr(NewX);
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkLine.Y2 := IntToStr(NewY);
        end;
      end;
    end;

    CClkRectPrimitiveCmdIdx:
    begin
      case AEditingPointIndex of
        0:
        begin
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkRect.X1 := IntToStr(NewX);
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkRect.Y1 := IntToStr(NewY);
        end;

        1:
        begin
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkRect.X2 := IntToStr(NewX);
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkRect.Y1 := IntToStr(NewY);
        end;

        2:
        begin
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkRect.X1 := IntToStr(NewX);
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkRect.Y2 := IntToStr(NewY);
        end;

        3:
        begin
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkRect.X2 := IntToStr(NewX);
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkRect.Y2 := IntToStr(NewY);
        end;
      end;
    end;

    CClkRoundedRectPrimitiveCmdIdx:
    begin
      case AEditingPointIndex of
        0:
        begin
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkRoundedRect.X1 := IntToStr(NewX);
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkRoundedRect.Y1 := IntToStr(NewY);
        end;

        1:
        begin
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkRoundedRect.X2 := IntToStr(NewX);
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkRoundedRect.Y1 := IntToStr(NewY);
        end;

        2:
        begin
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkRoundedRect.X1 := IntToStr(NewX);
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkRoundedRect.Y2 := IntToStr(NewY);
        end;

        3:
        begin
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkRoundedRect.X2 := IntToStr(NewX);
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkRoundedRect.Y2 := IntToStr(NewY);
        end;
      end;
    end;

    CClkGradientFill:
    begin
      case AEditingPointIndex of
        0:
        begin
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkGradientFill.X1 := IntToStr(NewX);
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkGradientFill.Y1 := IntToStr(NewY);
        end;

        1:
        begin
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkGradientFill.X2 := IntToStr(NewX);
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkGradientFill.Y1 := IntToStr(NewY);
        end;

        2:
        begin
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkGradientFill.X1 := IntToStr(NewX);
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkGradientFill.Y2 := IntToStr(NewY);
        end;

        3:
        begin
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkGradientFill.X2 := IntToStr(NewX);
          FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkGradientFill.Y2 := IntToStr(NewY);
        end;
      end;
    end;

    CClkText:
    begin
      FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkText.X := IntToStr(NewX);
      FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkText.Y := IntToStr(NewY);
    end;

    CClkDonutSector:
    begin
      FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkDonutSector.Cx := IntToStr(NewX);
      FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkDonutSector.Cy := IntToStr(NewY);
    end;

    CClkPolygon, CClkPolyBezier:
    begin
      ListOfPoints := TStringList.Create;
      try
        ListOfPoints.LineBreak := CPolygonPointLineBreak;

        ListOfPoints.Text := FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkPolygon.XPoints;
        ListOfPoints.Strings[AEditingPointIndex] := IntToStr(NewX);
        FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkPolygon.XPoints := ListOfPoints.Text;

        ListOfPoints.Text := FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkPolygon.YPoints;
        ListOfPoints.Strings[AEditingPointIndex] := IntToStr(NewY);
        FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkPolygon.YPoints := ListOfPoints.Text;
      finally
        ListOfPoints.Free;
      end;
    end;
  end; //case

  //update the other selection labels:
  case TempPrimitiveType of
    CClkImagePrimitiveCmdIdx, CClkRectPrimitiveCmdIdx, CClkRoundedRectPrimitiveCmdIdx, CClkGradientFill:
    begin
      case AEditingPointIndex of
        0:
        begin
          FLeftLimitLabel_ForPrimitive.Left := NewX; //FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.X1;
          FTopLimitLabel_ForPrimitive.Top := NewY; //FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.Y1;
          //FRightLimitLabel_ForPrimitive.Left := FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.X2;
          //FBottomLimitLabel_ForPrimitive.Top := FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.Y2;
        end;

        1:
        begin
          //FLeftLimitLabel_ForPrimitive.Left := FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.X1;
          FTopLimitLabel_ForPrimitive.Top := NewY; //FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.Y1;
          FRightLimitLabel_ForPrimitive.Left := NewX; //FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.X2;
          //FBottomLimitLabel_ForPrimitive.Top := FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.Y2;
        end;

        2:
        begin
          FLeftLimitLabel_ForPrimitive.Left := NewX; //FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.X1;
          //FTopLimitLabel_ForPrimitive.Top := FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.Y1;
          //FRightLimitLabel_ForPrimitive.Left := FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.X2;
          FBottomLimitLabel_ForPrimitive.Top := NewY; //FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.Y2;
        end;

        3:
        begin
          //FLeftLimitLabel_ForPrimitive.Left := FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.X1;
          //FTopLimitLabel_ForPrimitive.Top := FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.Y1;
          FRightLimitLabel_ForPrimitive.Left := NewX; //FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.X2;
          FBottomLimitLabel_ForPrimitive.Top := NewY; //FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.Y2;
        end;
      end; //case
    end;

    CClkLinePrimitiveCmdIdx:
    begin
      case AEditingPointIndex of
        0:
        begin
          FLeftLimitLabel_ForPrimitive.Left := NewX; //FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.X1;
          FTopLimitLabel_ForPrimitive.Top := NewY; //FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.Y1;
        end;

        1:
        begin
          FRightLimitLabel_ForPrimitive.Left := NewX; //FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.X2;
          FBottomLimitLabel_ForPrimitive.Top := NewY; //FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkImage.Y2;
        end;
      end; //case
    end;

    CClkText:
    begin
      TempText := DoOnEvaluateReplacementsFunc(FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkText.Text);
      TempBmp := TBitmap.Create;
      try
        TempImage := TImage(TScrollBox(PageControlPreview.Pages[0].Tag).Tag);
        //init to some defaults
        TempBmp.Canvas.Font.Name := TempImage.Canvas.Font.Name;
        TempBmp.Canvas.Font.Size := TempImage.Canvas.Font.Size;
        TempBmp.Canvas.Font.Style := TempImage.Canvas.Font.Style;

        TextSize := TempBmp.Canvas.TextExtent(TempText);

        FLeftLimitLabel_ForPrimitive.Left := NewX;
        FTopLimitLabel_ForPrimitive.Top := NewY;
        FRightLimitLabel_ForPrimitive.Left := NewX + TextSize.cx;
        FBottomLimitLabel_ForPrimitive.Top := NewY + TextSize.cy;
      finally
        TempBmp.Free;
      end;
    end; //CClkText

    CClkDonutSector:
    begin
      r1 := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkDonutSector.Radius1), 30);
      r2 := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[FEditingPrimitivePoints.SelectedPrimitiveIndex].ClkDonutSector.Radius2), 90);
      cx := NewX;
      cy := NewY;
      rmax := Max(r1, r2);

      x1 := cx - rmax;
      x2 := cx + rmax;
      y1 := cy - rmax;
      y2 := cy + rmax;

      FLeftLimitLabel_ForPrimitive.Left := x1;
      FTopLimitLabel_ForPrimitive.Top := y1;
      FRightLimitLabel_ForPrimitive.Left := x2;
      FBottomLimitLabel_ForPrimitive.Top := y2;
    end;

    CClkPolygon, CClkPolyBezier:
      SetLimitLabelFromPolygon(FEditingPrimitivePoints.SelectedPrimitiveIndex, FLeftLimitLabel_ForPrimitive, FTopLimitLabel_ForPrimitive, FRightLimitLabel_ForPrimitive, FBottomLimitLabel_ForPrimitive);
  end; //case

  SetEditPointsFromLimitLabels(TempPrimitiveType, FLeftLimitLabel_ForPrimitive, FTopLimitLabel_ForPrimitive, FRightLimitLabel_ForPrimitive, FBottomLimitLabel_ForPrimitive);
end;


procedure TfrClickerPrimitives.HandleEditingPointsOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;

  GetCursorPos(FMouseDownGlobalPos);

  if not FSelectionHold then
  begin
    FMouseDownSelPos.X := (Sender as TEditingPoint).Left; //component coordinates on the window
    FMouseDownSelPos.Y := (Sender as TEditingPoint).Top; //component coordinates on the window
    FSelectionHold := True;
  end;
end;


procedure TfrClickerPrimitives.HandleEditingPointsOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  PreviewImage: TImage;
  NewLeft, NewTop: Integer;
  CurrentEditingPoint: TEditingPoint;
  tp: TPoint;
begin
  //FCurrentMousePosOnPreviewImg.X := X;
  //FCurrentMousePosOnPreviewImg.Y := Y;
  tmrDrawZoom.Enabled := True;

  if PageControlPreview.ActivePageIndex > -1 then
  begin
    PreviewImage := TImage(TScrollBox(PageControlPreview.Pages[PageControlPreview.ActivePageIndex].Tag).Tag);

    X := X + (Sender as TEditingPoint).Left;
    Y := Y + (Sender as TEditingPoint).Top;

    FCurrentMousePosOnPreviewImg.X := X;
    FCurrentMousePosOnPreviewImg.Y := Y;
    lblMouseOnPreviewImg.Caption := IntToStr(X) + ' : ' + IntToStr(Y);
    SetLabelsFromMouseOverPreviewImgPixelColor(PreviewImage.Canvas.Pixels[X, Y]);
  end
  else
    Exit;

  if not FSelectionHold then
    Exit;

  GetCursorPos(tp);
  if Sender is TEditingPoint then
  begin
    CurrentEditingPoint := Sender as TEditingPoint;

    if GetAsyncKeyState(VK_ESCAPE) < 0 then
    begin
      NewLeft := FMouseDownSelPos.X;
      NewTop := FMouseDownSelPos.Y;
      FSelectionHold := False;
    end
    else
    begin
      NewLeft := FMouseDownSelPos.X + tp.X - FMouseDownGlobalPos.X;
      NewTop := FMouseDownSelPos.Y + tp.Y - FMouseDownGlobalPos.Y;
    end;

    if (NewLeft <> CurrentEditingPoint.Left) or (NewTop <> CurrentEditingPoint.Top) then
      DoOnTriggerOnControlsModified;

    CurrentEditingPoint.Left := Min(NewLeft, PreviewImage.Width);
    CurrentEditingPoint.Top := Min(NewTop, PreviewImage.Height);

    UpdatePrimitiveVertex((Sender as TEditingPoint).Tag, CurrentEditingPoint.Left + CEditingPointOffset, CurrentEditingPoint.Top + CEditingPointOffset);
    RepaintAllCompositions;

    //BuildImgLstPreviewPrimitives;
//
//    tmrUpdateSearchAreaOffsetEditBoxes.Enabled := True;
//    UpdateSearchAreaLabelColorsFromTheirPosition;
  end;
end;


procedure TfrClickerPrimitives.HandleEditingPointsOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectionHold := False;
end;


procedure TfrClickerPrimitives.HandleEditingPointsOnMouseEnter(Sender: TObject);
var
  tp: TPoint;
  Idx: Integer;
  PreviewImage: TImage;
begin
  Idx := PageControlPreview.ActivePageIndex;
  if Idx = -1 then
    Exit;

  PreviewImage := TImage(TScrollBox(PageControlPreview.Pages[Idx].Tag).Tag);

  if PreviewImage = nil then
    Exit;

  PreviewImage.ShowHint := False;
  GetCursorPos(tp);
  ShowZoom(tp.X + 50, tp.Y + 50);
end;


procedure TfrClickerPrimitives.HandleEditingPointsOnMouseLeave(Sender: TObject);
var
  Idx: Integer;
  PreviewImage: TImage;
begin
  Idx := PageControlPreview.ActivePageIndex;
  if Idx = -1 then
    Exit;

  PreviewImage := TImage(TScrollBox(PageControlPreview.Pages[Idx].Tag).Tag);

  if PreviewImage = nil then
    Exit;

  PreviewImage.ShowHint := True;
  HideZoom;
end;

end.

