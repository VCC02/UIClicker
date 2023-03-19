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
    imgFontColorBuffer: TImage;
    PageControlPreview: TPageControl;
    pnlPreview: TPanel;
    pnlvstOI: TPanel;
    tmrReloadOIContent: TTimer;
    procedure tmrReloadOIContentTimer(Sender: TObject);
  private
    FOIFrame: TfrObjectInspector;
    FPrimitives: TPrimitiveRecArr;
    FOrders: TCompositionOrderArr; //array of orders  - i.e. array of array of indexes.
    FOIEditorMenu: TPopupMenu;

    FOnLoadBitmap: TOnLoadBitmap;
    FOnLoadPrimitivesFile: TOnLoadPrimitivesFile;
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

    procedure MenuItem_RemoveAllPrimitivesFromList(Sender: TObject);
    procedure MenuItem_AddPrimitiveToList(Sender: TObject);
    procedure MenuItem_SetValueFromEnumItem(Sender: TObject);
    procedure MenuItem_RemovePrimitiveFromList(Sender: TObject);
    procedure MenuItem_RemoveAllCompositionOrdersFromList(Sender: TObject);
    procedure MenuItem_AddCompositionOrderToList(Sender: TObject);
    procedure MenuItem_RemoveCompositionOrderFromList(Sender: TObject);
    procedure MenuItem_RepaintAllCompositions(Sender: TObject);

    function DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    procedure DoOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr);
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFile(AFileName: string);
    procedure RenderPrimitives(ABmp: TBitmap; AOrderIndex: Integer);
    procedure RepaintAllCompositions;
    function GetOrderCount: Integer;

    property OnLoadBitmap: TOnLoadBitmap write FOnLoadBitmap;
    property OnLoadPrimitivesFile: TOnLoadPrimitivesFile write FOnLoadPrimitivesFile; //called by LoadFile
    property OnEvaluateReplacementsFunc: TEvaluateReplacementsFunc write FOnEvaluateReplacementsFunc; //called by RenderPrimitives
    property OnTriggerOnControlsModified: TOnTriggerOnControlsModified write FOnTriggerOnControlsModified;
  end;

implementation

{$R *.frm}


uses
  ClickerPrimitiveValues, ClickerOIUtils, FPCanvas;


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
  FOIEditorMenu := TPopupMenu.Create(Self);

  FOnLoadBitmap := nil;
  FOnLoadPrimitivesFile := nil;
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

  PreviewImage.Canvas.Pen.Color := clWhite;
  PreviewImage.Canvas.Brush.Color := clWhite;
  PreviewImage.Canvas.Rectangle(0, 0, PreviewImage.Width, PreviewImage.Height);

  //PreviewImage.Canvas.Font.Color := clRed;          //for debugging only
  //PreviewImage.Canvas.TextOut(20, 20, ATabName);    //for debugging only

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
begin
  for i := 0 to Length(FOrders) - 1 do
  begin
    PreviewImage := TImage(TScrollBox(PageControlPreview.Pages[i].Tag).Tag);
    RenderPrimitives(PreviewImage.Picture.Bitmap, i);
  end;
end;


procedure TfrClickerPrimitives.LoadFile(AFileName: string);
begin
  SetLength(FPrimitives, 0);
  SetLength(FOrders, 0);
  FOIFrame.ReloadContent;  //cleanup here, so the tree won't repaint with dangling pointers

  DoOnLoadPrimitivesFile(AFileName, FPrimitives, FOrders);
  FOIFrame.ReloadContent;
  CreateAllPreviewPages;
  RepaintAllCompositions;
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


procedure RenderPrimitive_SetPen(Sender: TfrClickerPrimitives; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
begin
  ABmp.Canvas.Pen.Color := HexToInt(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Color)); //TColor;
  ABmp.Canvas.Pen.Style := PenStyleNameToIndex(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Style)); //TFPPenStyle;
  ABmp.Canvas.Pen.Width := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Width), 1); //Integer;
  ABmp.Canvas.Pen.Mode := PenModeNameToIndex(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Mode)); //TFPPenMode;
  ABmp.Canvas.Pen.Pattern := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Pattern), 0); //LongWord;
  ABmp.Canvas.Pen.EndCap := PenEndCapNameToIndex(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.EndCap)); //TFPPenEndCap;
  ABmp.Canvas.Pen.JoinStyle := PenJoinStyleNameToIndex(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.JoinStyle)); //TFPPenJoinStyle;
end;


procedure RenderPrimitive_SetBrush(Sender: TfrClickerPrimitives; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
var
  TempBrushPattern: TBrushPattern;
begin
  ABmp.Canvas.Brush.Color := HexToInt(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Color)); //TColor;
  ABmp.Canvas.Brush.Style := BrushStyleNameToIndex(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Style)); //TFPBrushStyle;
  //ABmp.Canvas.Brush.Pattern := TempBrushPattern;    // array[0..31] of Cardinal
end;


procedure RenderPrimitive_SetMisc(Sender: TfrClickerPrimitives; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
begin
  ABmp.Canvas.AntialiasingMode := TAntialiasingMode(StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetMisc.AntialiasingMode), Ord(amDontCare)));
end;


procedure RenderPrimitive_SetFont(Sender: TfrClickerPrimitives; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
begin
  ABmp.Canvas.Font.Color := HexToInt(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetFont.ForegroundColor));
  ABmp.Canvas.Brush.Color := HexToInt(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetFont.BackgroundColor));
  ABmp.Canvas.Font.Name := Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetFont.FontName);
  ABmp.Canvas.Font.Size := APrimitive.ClkSetFont.FontSize;
  ABmp.Canvas.Font.Bold := APrimitive.ClkSetFont.Bold;
  ABmp.Canvas.Font.Italic := APrimitive.ClkSetFont.Italic;
  ABmp.Canvas.Font.Underline := APrimitive.ClkSetFont.Underline;
  ABmp.Canvas.Font.StrikeThrough := APrimitive.ClkSetFont.StrikeOut;

  if APrimitive.ClkSetFont.FontQualityUsesReplacement then
    ABmp.Canvas.Font.Quality := TFontQuality(StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetFont.FontQualityReplacement), Ord(fqDefault)))
  else
    ABmp.Canvas.Font.Quality := APrimitive.ClkSetFont.FontQuality;
end;


procedure RenderPrimitive_Image(Sender: TfrClickerPrimitives; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
var
  SrcBmp: TBitmap;
  SrcBitmapFnm: string;
  WillStretch: Boolean;
  TempRect: TRect;
begin
  ABmp.Canvas.Brush.Color := HexToInt(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Color)); //TColor;
  ABmp.Canvas.Brush.Style := TBrushStyle(StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Style), Ord(bsSolid))); //TFPBrushStyle;

  WillStretch := Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.Stretch) = '1';

  TempRect.Left := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.X1), 10);
  TempRect.Top := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.Y1), 20);

  if WillStretch then
  begin
    TempRect.Right := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.X2), 30);
    TempRect.Bottom := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.Y2), 40);
  end;

  SrcBitmapFnm := Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.Path);
  Sender.DoOnLoadBitmap(SrcBmp, SrcBitmapFnm);

  if WillStretch then
    ABmp.Canvas.StretchDraw(TempRect, SrcBmp)
  else
    ABmp.Canvas.Draw(TempRect.Left, TempRect.Top, SrcBmp);
end;


procedure RenderPrimitive_Line(Sender: TfrClickerPrimitives; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
var
  x1, y1, x2, y2: Integer;
begin
  x1 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.X1), 10);
  y1 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.Y1), 20);
  x2 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.X2), 30);
  y2 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.Y2), 40);

  ABmp.Canvas.Line(x1, y1, x2, y2);
end;


procedure RenderPrimitive_Rect(Sender: TfrClickerPrimitives; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
var
  x1, y1, x2, y2: Integer;
begin
  x1 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRect.X1), 10);
  y1 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRect.Y1), 20);
  x2 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRect.X2), 30);
  y2 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRect.Y2), 40);

  ABmp.Canvas.Rectangle(x1, y1, x2, y2);
end;


procedure RenderPrimitive_GradientFill(Sender: TfrClickerPrimitives; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
var
  TempRect: TRect;
  StartColor, StopColor: TColor;
  GradientDirection: TGradientDirection;
begin
  TempRect.Left := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.X1), 10);
  TempRect.Top := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.Y1), 20);
  TempRect.Right := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.X2), 30);
  TempRect.Bottom := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.Y2), 40);
  StartColor := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.StartColor), clLime);
  StopColor := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.StopColor), clYellow);
  GradientDirection := TGradientDirection(Ord(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.Direction) = '1'));

  ABmp.Canvas.GradientFill(TempRect, StartColor, StopColor, GradientDirection);
end;


procedure RenderPrimitive_Text(Sender: TfrClickerPrimitives; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
var
  X, Y: Integer;
  TempText: string;
begin
  X := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkText.X), 30);
  Y := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkText.Y), 40);
  TempText := Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkText.Text);

  ABmp.Canvas.TextOut(X, Y, TempText);
end;


type
  TRenderPrimitivesProc = procedure(Sender: TfrClickerPrimitives; ABmp: TBitmap; var APrimitive: TPrimitiveRec);


procedure TfrClickerPrimitives.RenderPrimitives(ABmp: TBitmap; AOrderIndex: Integer);
const
  CRenderPrimitives: array[0..CPrimitiveTypeCount - 1] of TRenderPrimitivesProc = (
    @RenderPrimitive_SetPen,
    @RenderPrimitive_SetBrush,
    @RenderPrimitive_SetMisc,
    @RenderPrimitive_SetFont,
    @RenderPrimitive_Image,
    @RenderPrimitive_Line,
    @RenderPrimitive_Rect,
    @RenderPrimitive_GradientFill,
    @RenderPrimitive_Text
  );

var
  i, NewIndex: Integer;
begin
  //ABmp.Width := 150;
  //ABmp.Height := 90;
  //ABmp.PixelFormat := pf24bit;
  ABmp.Transparent := False;
  ABmp.Canvas.Pen.Style := psSolid;
  ABmp.Canvas.Pen.Width := 1;
  ABmp.Canvas.Brush.Style := bsClear;//bsSolid;
  ABmp.Canvas.Pen.Color := clWhite;
  ABmp.Canvas.Brush.Color := clWhite;
  ABmp.Canvas.Rectangle(0, 0, ABmp.Width, ABmp.Height);
  ABmp.Canvas.Brush.Style := bsSolid;

  //for i := 0 to Length(FPrimitives) - 1 do
  for i := Length(FPrimitives) - 1 downto 0 do
  begin
    NewIndex := FOrders[AOrderIndex].Items[i];
    //CRenderPrimitives[FPrimitives[NewIndex].PrimitiveType](Self, ABmp, FPrimitives[i]);
    //CRenderPrimitives[FPrimitives[i].PrimitiveType](Self, ABmp, FPrimitives[NewIndex]);
    CRenderPrimitives[FPrimitives[NewIndex].PrimitiveType](Self, ABmp, FPrimitives[NewIndex]);
  end;
end;


procedure TfrClickerPrimitives.tmrReloadOIContentTimer(Sender: TObject);
begin
  tmrReloadOIContent.Enabled := False;
  FOIFrame.ReloadContent;
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


function TfrClickerPrimitives.DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if not Assigned(FOnLoadBitmap) then
    raise Exception.Create('OnLoadBitmap not assigned.')
  else
    Result := FOnLoadBitmap(ABitmap, AFileName);
end;


procedure TfrClickerPrimitives.DoOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr);
begin
  if not Assigned(FOnLoadPrimitivesFile) then
    raise Exception.Create('OnLoadPrimitivesFile not assigned.')
  else
    FOnLoadPrimitivesFile(AFileName, APrimitives, AOrders);
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
    end

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
  end;
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
            AddMenuItemToPopupMenu(FOIEditorMenu, CAddPrimitiveMenuPrefix + CPrimitiveNames[i], MenuItem_AddPrimitiveToList, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

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
end;


procedure TfrClickerPrimitives.HandleOnOIGetEnumConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEnumItemIndex: Integer; var AEnumItemName: string);
begin
  AEnumItemName := '';

  if ANodeLevel = CPropertyItemLevel then
    if ACategoryIndex = CCategory_Primitives then
      AEnumItemName := CPrimitivesPropEnumStrings[FPrimitives[APropertyIndex].PrimitiveType]^[AItemIndex]^[AEnumItemIndex];
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
var
  Ph2: Integer;
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
      end;

  //dragging an order item
  if (NodeLevel = CPropertyItemLevel) and (SrcNodeLevel = CPropertyItemLevel) then
    if (PropertyItemIndex > -1) and (SrcPropertyItemIndex > -1) then
      if PropertyIndex = SrcPropertyIndex then
        if PropertyItemIndex <> SrcPropertyItemIndex then
        begin
          Ph2 := FOrders[PropertyIndex].Items[PropertyItemIndex];
          FOrders[PropertyIndex].Items[PropertyItemIndex] := FOrders[PropertyIndex].Items[SrcPropertyItemIndex];
          FOrders[PropertyIndex].Items[SrcPropertyItemIndex] := Ph2;

          FOIFrame.ReloadPropertyItems(CategoryIndex, PropertyIndex, True);
          FOIFrame.ReloadPropertyItems(SrcCategoryIndex, SrcPropertyIndex, True);

          RepaintAllCompositions;
        end;
end;


end.

