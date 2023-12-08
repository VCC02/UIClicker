{
    Copyright (C) 2023 VCC
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

  { TfrClickerPrimitives }

  TfrClickerPrimitives = class(TFrame)
    chkShowPrimitiveEdges: TCheckBox;
    chkHighContrast: TCheckBox;
    imglstFontColorProperties: TImageList;
    imglstPreviewPrimitives: TImageList;
    imglstPrimitives: TImageList;
    imgFontColorBuffer: TImage;
    lblModified: TLabel;
    lblMouseOnPreviewImg: TLabel;
    lblMouseOnPreviewImgBB: TLabel;
    lblMouseOnPreviewImgGG: TLabel;
    lblMouseOnPreviewImgRR: TLabel;
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
    procedure MenuItem_CopyToClipboardClick(Sender: TObject);
    procedure MenuItem_SavePrimitivesFileClick(Sender: TObject);
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

    FHold: Boolean;
    FSplitterMouseDownGlobalPos: TPoint;
    FSplitterMouseDownImagePos: TPoint;

    FOIEditorMenu: TPopupMenu;

    FOnLoadBitmap: TOnLoadBitmap;
    FOnLoadRenderedBitmap: TOnLoadRenderedBitmap;
    FOnGetListOfExternallyRenderedImages: TOnGetListOfExternallyRenderedImages;
    FOnLoadPrimitivesFile: TOnLoadPrimitivesFile;
    FOnSavePrimitivesFile: TOnSavePrimitivesFile;
    FOnEvaluateReplacementsFunc: TEvaluateReplacementsFunc;
    FOnTriggerOnControlsModified: TOnTriggerOnControlsModified;
    FOnSaveFromMenu: TNotifyEvent;

    procedure CreateRemainingUIComponents;

    procedure ClearPreviewTabs;
    function AddPreviewTabWithImage(ATabName: string): TImage;
    procedure CreateAllPreviewPages;
    procedure BuildImgLstPreviewPrimitives;
    procedure BuildFontColorIconsList;
    procedure ResizeFrameSectionsBySplitter(NewLeft: Integer);

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
    procedure MenuItem_RemovePrimitiveFromList(Sender: TObject);
    procedure MenuItem_RemoveAllCompositionOrdersFromList(Sender: TObject);
    procedure MenuItem_AddCompositionOrderToList(Sender: TObject);
    procedure MenuItem_RemoveCompositionOrderFromList(Sender: TObject);
    procedure MenuItem_RepaintAllCompositions(Sender: TObject);

    procedure SetLabelsFromMouseOverPreviewImgPixelColor(APixelColor: TColor);
    procedure ClearPrimitiveSelection;
    function PointOnPrimitive(X, Y: Integer; var APrimitive: TPrimitiveRec; AWorkingImage: TImage): Boolean;
    procedure SelectPrimitiveByOrderIndex(APrimitiveIndex: Integer);
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
    function DoOnEvaluateReplacementsFunc(s: string; Recursive: Boolean = True): string;
    procedure DoOnTriggerOnControlsModified;
    procedure DoOnSaveFromMenu;

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

    function HandleOnEvaluateReplacementsFunc(s: string; Recursive: Boolean = True): string;
    function HandleOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function HandleOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFile(AFileName: string);
    procedure SaveFile(AFileName: string);
    procedure ClearContent;
    procedure ComposePrimitives(ABmp: TBitmap; AOrderIndex: Integer);
    procedure RepaintAllCompositions;
    function GetOrderCount: Integer;

    property OnLoadBitmap: TOnLoadBitmap write FOnLoadBitmap;
    property OnLoadRenderedBitmap: TOnLoadRenderedBitmap write FOnLoadRenderedBitmap;
    property OnGetListOfExternallyRenderedImages: TOnGetListOfExternallyRenderedImages write FOnGetListOfExternallyRenderedImages;
    property OnLoadPrimitivesFile: TOnLoadPrimitivesFile write FOnLoadPrimitivesFile; //called by LoadFile
    property OnSavePrimitivesFile: TOnSavePrimitivesFile write FOnSavePrimitivesFile;
    property OnEvaluateReplacementsFunc: TEvaluateReplacementsFunc write FOnEvaluateReplacementsFunc; //called by ComposePrimitives
    property OnTriggerOnControlsModified: TOnTriggerOnControlsModified write FOnTriggerOnControlsModified;
    property OnSaveFromMenu: TNotifyEvent write FOnSaveFromMenu;
  end;

implementation

{$R *.frm}


uses
  ClickerPrimitiveValues, ClickerOIUtils, ClickerPrimitivesCompositor, ClickerZoomPreviewForm,
  FPCanvas, Clipbrd, Dialogs;


const
  CAddPrimitiveMenuPrefix = 'Add ';
  CRemovePrimitiveMenuPrefix = 'Remove ';
  CRemoveOrderMenuPrefix = 'Remove ';

  CLeftLblID = 101;
  CTopLblID = 102;
  CRightLblID = 103;
  CBottomLblID = 104;


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
  FPrimitiveSettings.CompositorDirection := cdTopBot;

  FCurrentMousePosOnPreviewImg.X := Screen.Width - 10;  //init somewhere near the bottom-right corner of the screen
  FCurrentMousePosOnPreviewImg.Y := Screen.Height - 10;
  FHold := False;

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
    PageControlPreview.Pages[i].Free;
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

  FLeftLimitLabel_ForPrimitive.Hide;
  FTopLimitLabel_ForPrimitive.Hide;
  FRightLimitLabel_ForPrimitive.Hide;
  FBottomLimitLabel_ForPrimitive.Hide;
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
    PmtvCompositor.OnEvaluateReplacementsFunc := HandleOnEvaluateReplacementsFunc;
    PmtvCompositor.OnLoadBitmap := HandleOnLoadBitmap;
    PmtvCompositor.OnLoadRenderedBitmap := HandleOnLoadRenderedBitmap;

    UsingHighContrast := chkHighContrast.Checked;
    for i := 0 to Length(FOrders) - 1 do
    begin
      TempScrollBox := TScrollBox(PageControlPreview.Pages[i].Tag);
      PreviewImage := TImage(TempScrollBox.Tag);

      PreviewImage.Picture.Bitmap.Width := PmtvCompositor.GetMaxX(PreviewImage.Picture.Bitmap.Canvas, FPrimitives) + 1;
      PreviewImage.Picture.Bitmap.Height := PmtvCompositor.GetMaxY(PreviewImage.Picture.Bitmap.Canvas, FPrimitives) + 1;

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
  for i := AIndex to Length(FOrders) - 2 do
  begin
    FOrders[i].Name := FOrders[i + 1].Name;

    for j := 0 to Length(FOrders[i].Items) - 1 do
      FOrders[i].Items[j] := FOrders[i + 1].Items[j];
  end;

  SetLength(FOrders[Length(FOrders) - 1].Items, 0);
  SetLength(FOrders, Length(FOrders) - 1);

  if ADeleteTab then
    PageControlPreview.Pages[AIndex].Free;
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

  if TempPanel = nil then
  begin
    MessageBox(Handle, PChar('Cannot get primitives drawing panel on order index: ' + IntToStr(AOrderIndex)), PChar(Application.Title), MB_ICONERROR);
    Exit;
  end;

  ALeftLbl := nil;
  ATopLbl := nil;
  ARightLbl := nil;
  ABottomLbl := nil;

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
      PmtvCompositor.ComposePrimitives(Bmp, Idx, chkHighContrast.Checked, FPrimitives, FOrders, FPrimitiveSettings);

      Clipboard.Assign(Bmp);
    finally
      Bmp.Free;
    end;
  finally
    PmtvCompositor.Free;
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
  TempSize: TSize;
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
    begin
      X1 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.X1), 0);
      X2 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.X2), 0);
      Y1 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.Y1), 0);
      Y2 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.Y2), 0);

      if (X1 <> X2) and (Y1 <> Y2) then  //verify if the line is oblique, to avoid division by 0
      begin
        //if ((X - X1) / (X2 - X1)) - ((Y - Y1) / (Y2 - Y1)) < 0.0001 then
        if ((X1 - X) * (Y - Y2)) = ((X - X2) * (Y1 - Y)) then  //See stackoverflow.com, question 17692922
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
  end;
end;


procedure TfrClickerPrimitives.SelectPrimitiveByOrderIndex(APrimitiveIndex: Integer);
begin
  ClearPrimitiveSelection;
  FPrimitives[APrimitiveIndex].Selected := True;
  FOIFrame.SelectNode(CPropertyLevel, CCategory_Primitives, APrimitiveIndex, -1, True, True);
end;


procedure TfrClickerPrimitives.imgPreviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, OrderIdx: Integer;
  Img: TImage;
  Found: Boolean;
begin
  if not MenuItem_EditMode.Checked then
  begin
    ClearPrimitiveSelection;
    Exit;
  end;

  Img := Sender as TImage;
  OrderIdx := Img.Tag;
  Found := False;

  if FPrimitiveSettings.CompositorDirection = cdTopBot then
  begin
    for i := Length(FPrimitives) - 1 downto 0 do
      if PointOnPrimitive(X, Y, FPrimitives[FOrders[OrderIdx].Items[i]], Img) then
      begin
        SelectPrimitiveByOrderIndex(FOrders[OrderIdx].Items[i]);
        Found := True;
        Break;
      end;
  end
  else
  begin
    for i := 0 to Length(FPrimitives) - 1 do
      if PointOnPrimitive(X, Y, FPrimitives[FOrders[OrderIdx].Items[i]], Img) then
      begin
        SelectPrimitiveByOrderIndex(FOrders[OrderIdx].Items[i]);
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
                    'Green panel size: ' + IntToStr(TempPanel.Width) + ' : ' + IntToStr(TempPanel.Height);
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


function TfrClickerPrimitives.DoOnEvaluateReplacementsFunc(s: string; Recursive: Boolean = True): string;
begin
  if not Assigned(FOnEvaluateReplacementsFunc) then
    raise Exception.Create('OnEvaluateReplacementsFunc not assigned.')
  else
    Result := FOnEvaluateReplacementsFunc(s, Recursive);
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
begin
  case ACategoryIndex of
    CCategory_Primitives:
      Result := CClkPrimitivesTypeCounts[FPrimitives[APropertyIndex].PrimitiveType];

    CCategory_Orders:
      Result := Length(FOrders[APropertyIndex].Items);

    CCategory_Settings:
      Result := 0;

    else
      Result := 0;
  end;
end;


function TfrClickerPrimitives.HandleOnOIGetListPropertyItemName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
begin
  Result := '';

  case ACategoryIndex of
    CCategory_Primitives:
      Result := CPrimitivesMainProperties[FPrimitives[APropertyIndex].PrimitiveType]^[AItemIndex].Name;

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
      AEditorType := CPrimitivesMainProperties[FPrimitives[APropertyIndex].PrimitiveType]^[AItemIndex].EditorType;
    end;

    CCategory_Orders:
    begin
      Result := '[' + IntToStr(FOrders[APropertyIndex].Items[AItemIndex]) + ']';
      AEditorType := etNone; //these items are not editable
    end;
  end;

end;


function TfrClickerPrimitives.HandleOnUIGetDataTypeName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
begin
  Result := '';
end;


function TfrClickerPrimitives.HandleOnUIGetExtraInfo(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
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


procedure TfrClickerPrimitives.HandleOnOIGetEnumConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEnumItemIndex: Integer; var AEnumItemName: string);
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
          if PmtvType = CClkImagePrimitiveCmdIdx then
          begin
            case AItemIndex of
              CImagePrimitive_Stretch_PropIndex:
              begin
                AHint := 'Set this to 1 or True, to stretch the image to the maximum available size of the composition.';
                AShowHint := True;
              end;

              CImagePrimitive_RenderedExternally_PropIndex:
              begin
                AHint := 'Set this to 1 or True, to load an externally rendered bmp (i.e. received from a server).' + #13#10 +
                         'The image is identified by path and "stored" in a separate in-mem file system (not the same one used on client-server execution).';
                AShowHint := True;
              end;

              CImagePrimitive_Transparent_PropIndex:
              begin
                AHint := 'Set this to 1 or True, to have one of the colors as the transparency color.';
                AShowHint := True;
              end;

              CImagePrimitive_TransparentMode_PropIndex:
              begin
                AHint := 'Set this to 0 or Auto, or 1 or Fixed.';
                AShowHint := True;
              end;

              CImagePrimitive_TransparentColor_PropIndex:
              begin
                AHint := '6-digit hexa number (BGR), for a simple color, or 8-digit hexa number for a special system color.' + #13#10;
                AHint := AHint + 'When using the value 20000000 (clDefault is a special system color), the transparency color is read from the bottom-left pixel of the image.' + #13#10;
                AHint := AHint + 'Available special system colors: ' + #13#10;
                AHint := AHint + '1FFFFFFF   (clNone)' + #13#10;
                AHint := AHint + '20000000   (clDefault)';
                AShowHint := True;
              end;
            end; //case
          end; //if
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

          AddMenuItemToPopupMenu(FOIEditorMenu, CRemovePrimitiveMenuPrefix + '"' + FPrimitives[APropertyIndex].PrimitiveName + '"', MenuItem_RemovePrimitiveFromList, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

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
            AddMenuItemToPopupMenu(FOIEditorMenu, EnumItemName, MenuItem_SetValueFromEnumItem, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);
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
                      AddMenuItemToPopupMenu(FOIEditorMenu, ListOfExternallyRenderedImages.Strings[i], MenuItem_SetExternallyRenderedFile, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);
                  finally
                    ListOfExternallyRenderedImages.Free;
                  end;
                end
                else
                begin
                  AddMenuItemToPopupMenu(FOIEditorMenu, 'Browse...', nil {MenuItem_SetExternallyRenderedFile}, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);
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

          AddMenuItemToPopupMenu(FOIEditorMenu, CRemoveOrderMenuPrefix + '"' + FOrders[APropertyIndex].Name + '"', MenuItem_RemoveCompositionOrderFromList, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

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
            AddMenuItemToPopupMenu(FOIEditorMenu, CAddPrimitiveMenuPrefix + CPrimitiveNames[i], MenuItem_AddPrimitiveToList, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);
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

          AddMenuItemToPopupMenu(FOIEditorMenu, '-', nil, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);
          AddMenuItemToPopupMenu(FOIEditorMenu, 'Remove all primitives from list...', MenuItem_RemoveAllPrimitivesFromList, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

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

          AddMenuItemToPopupMenu(FOIEditorMenu, 'Add composition order to list', MenuItem_AddCompositionOrderToList, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);
          AddMenuItemToPopupMenu(FOIEditorMenu, '-', nil, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);
          AddMenuItemToPopupMenu(FOIEditorMenu, 'Remove all composition orders from list...', MenuItem_RemoveAllCompositionOrdersFromList, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);
          AddMenuItemToPopupMenu(FOIEditorMenu, '-', nil, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);
          AddMenuItemToPopupMenu(FOIEditorMenu, 'Repaint all compositions', MenuItem_RepaintAllCompositions, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

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


procedure TfrClickerPrimitives.HandleOnOISelectedNode(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, Column: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  x1, x2, y1, y2, cx, cy, r1, r2, rmax: Integer;
  PmtvType, i: Integer;
  TempText: string;
  TempBmp: TBitmap;
  TextSize: TSize;
  FLeftLimitLabel_ForPrimitive: TPaintedLabel;
  FTopLimitLabel_ForPrimitive: TPaintedLabel;
  FRightLimitLabel_ForPrimitive: TPaintedLabel;
  FBottomLimitLabel_ForPrimitive: TPaintedLabel;
  TempImage: TImage;
begin
  for i := 0 to Length(FOrders) - 1 do
  begin
    GetSelectionLabelsByOrderIndex(i,
                                   TLabel(FLeftLimitLabel_ForPrimitive),
                                   TLabel(FTopLimitLabel_ForPrimitive),
                                   TLabel(FRightLimitLabel_ForPrimitive),
                                   TLabel(FBottomLimitLabel_ForPrimitive));

    TempImage := TImage(TScrollBox(PageControlPreview.Pages[i].Tag).Tag);

    case CategoryIndex of
      CCategory_Primitives:
        case NodeLevel of
          CCategoryLevel:
            ;

          CPropertyLevel:
          begin
            PmtvType := FPrimitives[PropertyIndex].PrimitiveType;

            case PmtvType of
              CClkImagePrimitiveCmdIdx:
              begin
                FLeftLimitLabel_ForPrimitive.Left := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkImage.X1), 0);
                FTopLimitLabel_ForPrimitive.Top := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkImage.Y1), 0);
                FRightLimitLabel_ForPrimitive.Left := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkImage.X2), 100);
                FBottomLimitLabel_ForPrimitive.Top := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkImage.Y2), 100);
              end;

              CClkLinePrimitiveCmdIdx:
              begin
                FLeftLimitLabel_ForPrimitive.Left := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkLine.X1), 0);
                FTopLimitLabel_ForPrimitive.Top := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkLine.Y1), 0);
                FRightLimitLabel_ForPrimitive.Left := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkLine.X2), 100);
                FBottomLimitLabel_ForPrimitive.Top := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkLine.Y2), 100);
              end;

              CClkRectPrimitiveCmdIdx:
              begin
                FLeftLimitLabel_ForPrimitive.Left := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkRect.X1), 0);
                FTopLimitLabel_ForPrimitive.Top := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkRect.Y1), 0);
                FRightLimitLabel_ForPrimitive.Left := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkRect.X2), 100);
                FBottomLimitLabel_ForPrimitive.Top := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkRect.Y2), 100);
              end;

              CClkGradientFill:
              begin
                FLeftLimitLabel_ForPrimitive.Left := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkGradientFill.X1), 0);
                FTopLimitLabel_ForPrimitive.Top := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkGradientFill.Y1), 0);
                FRightLimitLabel_ForPrimitive.Left := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkGradientFill.X2), 100);
                FBottomLimitLabel_ForPrimitive.Top := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkGradientFill.Y2), 100);
              end;

              CClkText:
              begin
                TempText := DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkText.Text);
                TempBmp := TBitmap.Create;
                try
                  //init to some defaults
                  TempBmp.Canvas.Font.Name := TempImage.Canvas.Font.Name;
                  TempBmp.Canvas.Font.Size := TempImage.Canvas.Font.Size;
                  TempBmp.Canvas.Font.Style := TempImage.Canvas.Font.Style;

                  //ToDo
                  // depending on FPrimitiveSettings.CompositorDirection, the FOrders array should be iterated, to get the first SetFont "primitive"
                  //based on that, the TempBmp.Canvas.Font property should be set

                  TextSize := TempBmp.Canvas.TextExtent(TempText);
                  x1 := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkText.X), 0);
                  x2 := x1 + TextSize.cx;
                  y1 := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkText.Y), 0);
                  y2 := y1 + TextSize.cy;

                  FLeftLimitLabel_ForPrimitive.Left := x1;
                  FTopLimitLabel_ForPrimitive.Top := y1;
                  FRightLimitLabel_ForPrimitive.Left := x2;
                  FBottomLimitLabel_ForPrimitive.Top := y2;
                finally
                  TempBmp.Free;
                end;
              end;

              CClkDonutSector:
              begin
                r1 := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkDonutSector.Radius1), 30);
                r2 := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkDonutSector.Radius2), 90);
                cx := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkDonutSector.Cx), 100);
                cy := StrToIntDef(DoOnEvaluateReplacementsFunc(FPrimitives[PropertyIndex].ClkDonutSector.Cy), 100);
                rmax := Max(r1, r2);

                x1 := cx - rmax;
                x2 := cx + rmax;
                y1 := cy - rmax;
                y2 := cx + rmax;

                FLeftLimitLabel_ForPrimitive.Left := x1;
                FTopLimitLabel_ForPrimitive.Top := y1;
                FRightLimitLabel_ForPrimitive.Left := x2;
                FBottomLimitLabel_ForPrimitive.Top := y2;
              end;
            end;
          end;

          CPropertyItemLevel:
            ;
        end;
    end; //case CategoryIndex
  end; //for
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

function TfrClickerPrimitives.HandleOnEvaluateReplacementsFunc(s: string; Recursive: Boolean = True): string;
begin
  Result := DoOnEvaluateReplacementsFunc(s, Recursive);
end;


function TfrClickerPrimitives.HandleOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  Result := DoOnLoadBitmap(ABitmap, AFileName);
end;


function TfrClickerPrimitives.HandleOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  Result := DoOnLoadRenderedBitmap(ABitmap, AFileName);
end;


end.

