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
  VirtualTrees, ImgList, Graphics, Menus, ComCtrls, Types,
  ClickerUtils, ClickerPrimitiveUtils;

type

  { TfrClickerPrimitives }

  TfrClickerPrimitives = class(TFrame)
    imglstPrimitives: TImageList;
    imgFontColorBuffer: TImage;
    MenuItem_CopyToClipboard: TMenuItem;
    PageControlPreview: TPageControl;
    pnlPreview: TPanel;
    pnlvstOI: TPanel;
    pmPreview: TPopupMenu;
    tmrDrawZoom: TTimer;
    tmrReloadOIContent: TTimer;
    procedure MenuItem_CopyToClipboardClick(Sender: TObject);
    procedure tmrDrawZoomTimer(Sender: TObject);
    procedure tmrReloadOIContentTimer(Sender: TObject);
  private
    FOIFrame: TfrObjectInspector;
    FPrimitives: TPrimitiveRecArr;
    FOrders: TCompositionOrderArr; //array of orders  - i.e. array of array of indexes.
    FPrimitiveSettings: TPrimitiveSettings;
    FCurrentMousePosOnPreviewImg: TPoint;

    FOIEditorMenu: TPopupMenu;

    FOnLoadBitmap: TOnLoadBitmap;
    FOnLoadPrimitivesFile: TOnLoadPrimitivesFile;
    FOnSavePrimitivesFile: TOnSavePrimitivesFile;
    FOnEvaluateReplacementsFunc: TEvaluateReplacementsFunc;
    FOnTriggerOnControlsModified: TOnTriggerOnControlsModified;

    procedure CreateRemainingUIComponents;

    procedure ClearPreviewTabs;
    function AddPreviewTabWithImage(ATabName: string): TImage;
    procedure CreateAllPreviewPages;

    procedure GetOrderContentByIndex(AIndex: Integer; var ADestContent: TCompositionOrder);
    procedure DeleteOrderByIndex(AIndex: Integer; ADeleteTab: Boolean = True);
    procedure InsertOrderAtIndex(AIndex: Integer; var ASrcContent: TCompositionOrder);
    procedure MoveOrder(ASrcIndex, ADestIndex: Integer);
    procedure MoveOrderTabContent(ASrcIndex, ADestIndex: Integer);

    function GetOrderItemByIndex(AOrderIndex, AItemIndex: Integer): Integer;
    procedure DeleteOrderItemByIndex(AOrderIndex, AItemIndex: Integer);
    procedure InsertOrderItemAtIndex(AOrderIndex, AItemIndex: Integer; ASrcItem: Integer);
    procedure MoveOrderItem(AOrderIndex, ASrcIndex, ADestIndex: Integer);

    procedure MenuItem_RemoveAllPrimitivesFromList(Sender: TObject);
    procedure MenuItem_AddPrimitiveToList(Sender: TObject);
    procedure MenuItem_SetValueFromEnumItem(Sender: TObject);
    procedure MenuItem_RemovePrimitiveFromList(Sender: TObject);
    procedure MenuItem_RemoveAllCompositionOrdersFromList(Sender: TObject);
    procedure MenuItem_AddCompositionOrderToList(Sender: TObject);
    procedure MenuItem_RemoveCompositionOrderFromList(Sender: TObject);
    procedure MenuItem_RepaintAllCompositions(Sender: TObject);

    procedure imgPreviewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgPreviewMouseEnter(Sender: TObject);
    procedure imgPreviewMouseLeave(Sender: TObject);

    function DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    procedure DoOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    procedure DoOnSavePrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    function DoOnEvaluateReplacementsFunc(s: string; Recursive: Boolean = True): string;
    procedure DoOnTriggerOnControlsModified;

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

    procedure HandleOnOIDragAllowed(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer; var Allowed: Boolean);
    procedure HandleOnOIDragOver(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, SrcNodeLevel, SrcCategoryIndex, SrcPropertyIndex, SrcPropertyItemIndex: Integer; Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode; var Effect: DWORD; var Accept: Boolean);
    procedure HandleOnOIDragDrop(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, SrcNodeLevel, SrcCategoryIndex, SrcPropertyIndex, SrcPropertyItemIndex: Integer; Shift: TShiftState; const Pt: TPoint; var Effect: DWORD; Mode: TDropMode);

    function HandleOnEvaluateReplacementsFunc(s: string; Recursive: Boolean = True): string;
    function HandleOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
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
    property OnLoadPrimitivesFile: TOnLoadPrimitivesFile write FOnLoadPrimitivesFile; //called by LoadFile
    property OnSavePrimitivesFile: TOnSavePrimitivesFile write FOnSavePrimitivesFile;
    property OnEvaluateReplacementsFunc: TEvaluateReplacementsFunc write FOnEvaluateReplacementsFunc; //called by ComposePrimitives
    property OnTriggerOnControlsModified: TOnTriggerOnControlsModified write FOnTriggerOnControlsModified;
  end;

implementation

{$R *.frm}


uses
  ClickerPrimitiveValues, ClickerOIUtils, ClickerPrimitivesCompositor, ClickerZoomPreviewForm,
  FPCanvas, Clipbrd;


const
  CAddPrimitiveMenuPrefix = 'Add ';
  CRemovePrimitiveMenuPrefix = 'Remove ';
  CRemoveOrderMenuPrefix = 'Remove ';


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

  FOIEditorMenu := TPopupMenu.Create(Self);

  FOnLoadBitmap := nil;
  FOnLoadPrimitivesFile := nil;
  FOnSavePrimitivesFile := nil;
  FOnEvaluateReplacementsFunc := nil;
  FOnTriggerOnControlsModified := nil;
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
  TempScrollBox.Width := TempTabSheet.Width;
  TempScrollBox.Height := TempTabSheet.Height;
  TempScrollBox.Anchors := [akLeft, akTop, akRight, akBottom];
  TempScrollBox.HorzScrollBar.Visible := True;
  TempScrollBox.VertScrollBar.Visible := True;
  TempScrollBox.Visible := True;

  TempPanel := TPanel.Create(TempScrollBox);
  TempPanel.Parent := TempScrollBox;
  TempPanel.Left := 0;
  TempPanel.Top := 0;
  TempPanel.Width := TempScrollBox.Width - 20;
  TempPanel.Height := TempScrollBox.Height - 20;
  TempPanel.Anchors := [akLeft, akTop, akRight, akBottom];
  TempPanel.Visible := True;

  PreviewImage := TImage.Create(TempPanel);
  PreviewImage.Parent := TempPanel;
  TempScrollBox.Tag := PtrInt(PreviewImage);  //the image can be easily accessed as TImage(TScrollBox(PageControlPreview.Pages[i].Tag).Tag).Canvas.WhatEver;

  PreviewImage.Left := 0;
  PreviewImage.Top := 0;
  PreviewImage.Width := TempPanel.Width;
  PreviewImage.Height := TempPanel.Height;
  PreviewImage.Anchors := [akLeft, akTop, akRight, akBottom];
  PreviewImage.AutoSize := False;
  PreviewImage.Transparent := False;
  PreviewImage.Visible := True;
  PreviewImage.PopupMenu := pmPreview;

  PreviewImage.Canvas.Pen.Color := clWhite;
  PreviewImage.Canvas.Brush.Color := clWhite;
  PreviewImage.Canvas.Rectangle(0, 0, PreviewImage.Width, PreviewImage.Height);

  //PreviewImage.Canvas.Font.Color := clRed;          //for debugging only
  //PreviewImage.Canvas.TextOut(20, 20, ATabName);    //for debugging only

  PreviewImage.OnMouseMove := imgPreviewMouseMove;
  PreviewImage.OnMouseEnter := imgPreviewMouseEnter;
  PreviewImage.OnMouseLeave := imgPreviewMouseLeave;

  Result := PreviewImage;
end;


procedure TfrClickerPrimitives.CreateAllPreviewPages;
var
  i: Integer;
begin
  ClearPreviewTabs;

  for i := 0 to Length(FOrders) - 1 do
    AddPreviewTabWithImage(FOrders[i].Name);

  PageControlPreview.ActivePageIndex := 0;
end;


procedure TfrClickerPrimitives.RepaintAllCompositions;
var
  i: Integer;
  PreviewImage: TImage;
  PmtvCompositor: TPrimitivesCompositor;
begin
  PmtvCompositor := TPrimitivesCompositor.Create;
  try
    PmtvCompositor.OnEvaluateReplacementsFunc := HandleOnEvaluateReplacementsFunc;
    PmtvCompositor.OnLoadBitmap := HandleOnLoadBitmap;

    for i := 0 to Length(FOrders) - 1 do
    begin
      PreviewImage := TImage(TScrollBox(PageControlPreview.Pages[i].Tag).Tag);
      PmtvCompositor.ComposePrimitives(PreviewImage.Picture.Bitmap, i, FPrimitives, FOrders, FPrimitiveSettings);
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
end;


procedure TfrClickerPrimitives.SaveFile(AFileName: string);
begin
  DoOnSavePrimitivesFile(AFileName, FPrimitives, FOrders, FPrimitiveSettings);
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


procedure TfrClickerPrimitives.ComposePrimitives(ABmp: TBitmap; AOrderIndex: Integer);
var
  PmtvCompositor: TPrimitivesCompositor;
begin
  PmtvCompositor := TPrimitivesCompositor.Create;
  try
    PmtvCompositor.OnEvaluateReplacementsFunc := HandleOnEvaluateReplacementsFunc;
    PmtvCompositor.OnLoadBitmap := HandleOnLoadBitmap;

    PmtvCompositor.ComposePrimitives(ABmp, AOrderIndex, FPrimitives, FOrders, FPrimitiveSettings);
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

      Bmp.Width := PmtvCompositor.GetMaxX(Bmp.Canvas, FPrimitives);
      Bmp.Height := PmtvCompositor.GetMaxY(Bmp.Canvas, FPrimitives);
      PmtvCompositor.ComposePrimitives(Bmp, Idx, FPrimitives, FOrders, FPrimitiveSettings);

      Clipboard.Assign(Bmp);
    finally
      Bmp.Free;
    end;
  finally
    PmtvCompositor.Free;
  end;
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
end;


procedure TfrClickerPrimitives.imgPreviewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FCurrentMousePosOnPreviewImg.X := X;
  FCurrentMousePosOnPreviewImg.Y := Y;
  tmrDrawZoom.Enabled := True;
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


function TfrClickerPrimitives.DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if not Assigned(FOnLoadBitmap) then
    raise Exception.Create('OnLoadBitmap not assigned.')
  else
    Result := FOnLoadBitmap(ABitmap, AFileName);
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
    FOnTriggerOnControlsModified()
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
  if Column <> 0 then
  begin
    ImageIndex := -1;
    Exit;
  end;

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
end;


procedure TfrClickerPrimitives.HandleOnOIEditedText(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; ANewText: string);
var
  PmtvType: Integer;
begin
  case ACategoryIndex of
    CCategory_Primitives:
      case ANodeLevel of
        CPropertyLevel:
          FPrimitives[APropertyIndex].PrimitiveName := ANewText;

        CPropertyItemLevel:
        begin
          PmtvType := FPrimitives[APropertyIndex].PrimitiveType;
          CSetPrimitiveValueStrFunctions[PmtvType](FPrimitives[APropertyIndex], ANewText, AItemIndex);
          RepaintAllCompositions;
        end;
      end;

    CCategory_Orders:
      case ANodeLevel of
        CPropertyLevel:
        begin
          FOrders[APropertyIndex].Name := ANewText;
          PageControlPreview.Pages[APropertyIndex].Caption := ANewText;
        end;

        CPropertyItemLevel:
          ;
      end;

    CCategory_Settings:
      case ANodeLevel of
        CPropertyLevel:
        begin
          if APropertyIndex = CCompositorDirection_PropIndex then
          begin
            FPrimitiveSettings.CompositorDirection := CompositorDirectionToIndex(ANewText);
            RepaintAllCompositions;
          end;
        end;

        CPropertyItemLevel:
          ;
      end;
  end;

  DoOnTriggerOnControlsModified;
end;


function TfrClickerPrimitives.HandleOnOIEditItems(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewItems: string): Boolean;
var
  tp: TPoint;
  i: Integer;
begin
  Result := False;

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


function TfrClickerPrimitives.HandleOnOIGetColorConstsCount(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer;
begin
  Result := 0;
end;


procedure TfrClickerPrimitives.HandleOnOIGetColorConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AColorItemIndex: Integer; var AColorName: string; var AColorValue: Int64);
begin

end;



function TfrClickerPrimitives.HandleOnOIGetEnumConstsCount(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer;
begin
  Result := 0;
  if ANodeLevel = CPropertyItemLevel then
    if ACategoryIndex = CCategory_Primitives then
      Result := CPrimitivesPropEnumCounts[FPrimitives[APropertyIndex].PrimitiveType]^[AItemIndex];

  if ANodeLevel = CPropertyLevel then
    if ACategoryIndex = CCategory_Settings then
      Result := CPrimitiveSettingsPropEnumCounts[APropertyIndex];
end;


procedure TfrClickerPrimitives.HandleOnOIGetEnumConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEnumItemIndex: Integer; var AEnumItemName: string);
begin
  AEnumItemName := '';

  if ANodeLevel = CPropertyItemLevel then
    if ACategoryIndex = CCategory_Primitives then
      AEnumItemName := CPrimitivesPropEnumStrings[FPrimitives[APropertyIndex].PrimitiveType]^[AItemIndex]^[AEnumItemIndex];

  if ANodeLevel = CPropertyLevel then
    if ACategoryIndex = CCategory_Settings then
      AEnumItemName := CPrimitiveSettingsPropEnumStrings[APropertyIndex]^[AEnumItemIndex];
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
begin
  //
end;


procedure TfrClickerPrimitives.HandleOnOIGetFileDialogSettings(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var AFilter, AInitDir: string);
begin

end;


procedure TfrClickerPrimitives.HandleOnOIArrowEditorClick(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer);
var
  tp: TPoint;
  i: Integer;
  EnumItemName: string;
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

          for i := 0 to CPrimitivesPropEnumCounts[FPrimitives[APropertyIndex].PrimitiveType]^[AItemIndex] - 1 do
          begin
            EnumItemName := CPrimitivesPropEnumStrings[FPrimitives[APropertyIndex].PrimitiveType]^[AItemIndex]^[i];
            AddMenuItemToPopupMenu(FOIEditorMenu, EnumItemName, MenuItem_SetValueFromEnumItem, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);
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
begin

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


end.

