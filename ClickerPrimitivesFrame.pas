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
  Windows, Classes, SysUtils, Forms, Controls, ExtCtrls, ObjectInspectorFrame, VirtualTrees, ImgList,
  Graphics, Menus, Types, ClickerUtils, ClickerPrimitiveUtils;

type

  { TfrClickerPrimitives }

  TfrClickerPrimitives = class(TFrame)
    imgFontColorBuffer: TImage;
    pnlvstOI: TPanel;
    tmrReloadOIContent: TTimer;
    procedure tmrReloadOIContentTimer(Sender: TObject);
  private
    FOIFrame: TfrObjectInspector;
    FPrimitives: TPrimitiveRecArr;
    FOrders: TPrimitiveOrderArr; //array of orders  - i.e. array of array of indexes.

    FOnLoadBitmap: TOnLoadBitmap;
    FOnLoadPrimitivesFile: TOnLoadPrimitivesFile;
    FOnEvaluateReplacementsFunc: TEvaluateReplacementsFunc;
    FOnTriggerOnControlsModified: TOnTriggerOnControlsModified;

    procedure CreateRemainingUIComponents;

    procedure MenuItem_RemoveAllPrimitivesFromList(Sender: TObject);
    procedure MenuItem_AddPrimitiveToList(Sender: TObject);

    function DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    procedure DoOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TPrimitiveOrderArr);
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFile(AFileName: string);
    procedure RenderPrimitives(ABmp: TBitmap; AOrderIndex: Integer);
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

  pnlvstOI.Anchors := [akBottom, akLeft, akRight, akTop];

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

  FOIFrame.Visible := True;

  FOIFrame.ListItemsVisible := True;
  FOIFrame.DataTypeVisible := True; //False;
  FOIFrame.ExtraInfoVisible := False;
  FOIFrame.PropertyItemHeight := 22; //50;  //this should be 50 for bitmaps

  //FOIFrame.ReloadContent;  //set by loading
  pnlvstOI.Visible := True;
end;


constructor TfrClickerPrimitives.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateRemainingUIComponents;
  SetLength(FPrimitives, 0);
  SetLength(FOrders, 0);

  FOnLoadBitmap := nil;
  FOnLoadPrimitivesFile := nil;
  FOnEvaluateReplacementsFunc := nil;
  FOnTriggerOnControlsModified := nil;
end;


destructor TfrClickerPrimitives.Destroy;
begin
  SetLength(FPrimitives, 0);
  SetLength(FOrders, 0);
  inherited Destroy;
end;


procedure TfrClickerPrimitives.LoadFile(AFileName: string);
begin
  SetLength(FPrimitives, 0);
  SetLength(FOrders, 0);
  FOIFrame.ReloadContent;  //cleanup here, so the tree won't repaint with dangling pointers

  DoOnLoadPrimitivesFile(AFileName, FPrimitives, FOrders);
  FOIFrame.ReloadContent;
end;


procedure RenderPrimitive_SetPen(Sender: TfrClickerPrimitives; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
begin
  ABmp.Canvas.Pen.Color := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Color), clFuchsia); //TColor;
  ABmp.Canvas.Pen.Style := TFPPenStyle(StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Style), Ord(psSolid))); //TFPPenStyle;
  ABmp.Canvas.Pen.Width := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Width), 1); //Integer;
  ABmp.Canvas.Pen.Mode := TFPPenMode(StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Mode), Ord(pmBlack))); //TFPPenMode;
  ABmp.Canvas.Pen.Pattern := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Pattern), 0); //LongWord;
  ABmp.Canvas.Pen.EndCap := TFPPenEndCap(StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.EndCap), Ord(pecRound))); //TFPPenEndCap;
  ABmp.Canvas.Pen.JoinStyle := TFPPenJoinStyle(StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.JoinStyle), Ord(pjsRound))); //TFPPenJoinStyle;
end;


procedure RenderPrimitive_SetBrush(Sender: TfrClickerPrimitives; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
var
  TempBrushPattern: TBrushPattern;
begin
  ABmp.Canvas.Brush.Color := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Color), clTeal); //TColor;
  ABmp.Canvas.Brush.Style := TFPBrushStyle(StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Style), Ord(bsSolid))); //TFPBrushStyle;
  ABmp.Canvas.Brush.Pattern := TempBrushPattern;    // array[0..31] of Cardinal
end;


procedure RenderPrimitive_SetMisc(Sender: TfrClickerPrimitives; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
begin
  ABmp.Canvas.AntialiasingMode := TAntialiasingMode(StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetMisc.AntialiasingMode), Ord(amDontCare)));
end;


procedure RenderPrimitive_SetFont(Sender: TfrClickerPrimitives; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
begin
  ABmp.Canvas.Font.Color := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetFont.ForegroundColor), clFuchsia);
  ABmp.Canvas.Brush.Color := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetFont.BackgroundColor), clLtGray);

end;


procedure RenderPrimitive_Image(Sender: TfrClickerPrimitives; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
var
  SrcBmp: TBitmap;
  SrcBitmapFnm: string;
  WillStretch: Boolean;
  TempRect: TRect;
begin
  ABmp.Canvas.Brush.Color := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Color), clTeal); //TColor;
  ABmp.Canvas.Brush.Style := TFPBrushStyle(StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Style), Ord(bsSolid))); //TFPBrushStyle;

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
    @RenderPrimitive_GradientFill
  );

var
  i: Integer;
begin
  //ToDo:  read order
  ABmp.Width := 150;
  ABmp.Height := 90;

  for i := 0 to Length(FPrimitives) - 1 do
    CRenderPrimitives[FPrimitives[i].PrimitiveType](Self, ABmp, FPrimitives[i]);
end;


procedure TfrClickerPrimitives.tmrReloadOIContentTimer(Sender: TObject);
begin
  tmrReloadOIContent.Enabled := False;
  FOIFrame.ReloadContent;
end;


procedure TfrClickerPrimitives.MenuItem_RemoveAllPrimitivesFromList(Sender: TObject);
var
  MenuData: POIMenuItemData;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
    try
      if MessageBox(Handle, 'Are you sure you want to remove all the primitives from list?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = IDNO then
        Exit;

      SetLength(FPrimitives, 0);

      tmrReloadOIContent.Enabled := True;

      DoOnTriggerOnControlsModified;  //the pmtv file is modified, not the template
    finally
      MenuData^.OwnerMenu.Free;
    end;
  finally
    Dispose(MenuData);
  end;
end;


procedure TfrClickerPrimitives.MenuItem_AddPrimitiveToList(Sender: TObject);
var
  MenuData: POIMenuItemData;
  ValueStr: string;
  TempPrimitiveType, n: Integer;
begin
  MenuData := {%H-}POIMenuItemData((Sender as TMenuItem).Tag);
  try
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

      CFillInDefaultValuesToPrimitives[TempPrimitiveType](FPrimitives[n]);
      tmrReloadOIContent.Enabled := True;

      DoOnTriggerOnControlsModified;  //the pmtv file is modified, not the template
    finally
      MenuData^.OwnerMenu.Free;
    end;
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


procedure TfrClickerPrimitives.DoOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TPrimitiveOrderArr);
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

    CCategory_Order:
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

    CCategory_Order:
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

    CCategory_Order:
      Result := IntToStr(APropertyIndex); // FOrders[];

    else
      Result := 'unknown';
  end;
end;


function TfrClickerPrimitives.HandleOnOIGetPropertyValue(ACategoryIndex, APropertyIndex: Integer; var AEditorType: TOIEditorType): string;
begin
  case ACategoryIndex of
    CCategory_Primitives:
      Result := FPrimitives[APropertyIndex].PrimitiveName;

    CCategory_Order:
      Result := 'not implemented';

    else
      Result := 'unknown category';
  end;
end;


function TfrClickerPrimitives.HandleOnOIGetListPropertyItemCount(ACategoryIndex, APropertyIndex: Integer): Integer;
begin
  case ACategoryIndex of
    CCategory_Primitives:
      Result := CClkPrimitivesTypeCounts[FPrimitives[APropertyIndex].PrimitiveType];

    CCategory_Order:
      Result := 0;

    else
      Result := 0;
  end;
end;


function TfrClickerPrimitives.HandleOnOIGetListPropertyItemName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
begin
  Result := '';

  if ACategoryIndex = CCategory_Primitives then
    Result := CPrimitivesMainProperties[FPrimitives[APropertyIndex].PrimitiveType]^[AItemIndex].Name;
end;


function TfrClickerPrimitives.HandleOnOIGetListPropertyItemValue(ACategoryIndex, APropertyIndex, AItemIndex: Integer; var AEditorType: TOIEditorType): string;
begin
  Result := 'value';

  if ACategoryIndex = CCategory_Primitives then
  begin
    Result := CGetActionValueStr_Primitive[FPrimitives[APropertyIndex].PrimitiveType](FPrimitives[APropertyIndex], AItemIndex);
    AEditorType := CPrimitivesMainProperties[FPrimitives[APropertyIndex].PrimitiveType]^[AItemIndex].EditorType;
  end;
end;


function TfrClickerPrimitives.HandleOnUIGetDataTypeName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
begin
  Result := 'type';
end;


function TfrClickerPrimitives.HandleOnUIGetExtraInfo(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
begin
  Result := 'extra';
end;


procedure TfrClickerPrimitives.HandleOnOIGetImageIndexEx(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);
begin

end;


procedure TfrClickerPrimitives.HandleOnOIEditedText(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; ANewText: string);
begin

end;


function TfrClickerPrimitives.HandleOnOIEditItems(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewItems: string): Boolean;
var
  tp: TPoint;
  PropertyMenu: TPopupMenu;
  i: Integer;
begin
  Result := False;

  if ANodeLevel = CCategoryLevel then
    if ACategoryIndex = CCategory_Primitives then
    begin
      PropertyMenu := TPopupMenu.Create(Self);

      AddMenuItemToPopupMenu(PropertyMenu, 'Remove all primitives from list...', MenuItem_RemoveAllPrimitivesFromList, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);
      AddMenuItemToPopupMenu(PropertyMenu, '-', nil, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

      for i := 0 to CPrimitiveTypeCount - 1 do
        AddMenuItemToPopupMenu(PropertyMenu, CAddPrimitiveMenuPrefix + CPrimitiveNames[i], MenuItem_AddPrimitiveToList, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);

      GetCursorPos(tp);
      PropertyMenu.PopUp(tp.X, tp.Y);
    end;
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
  AEnumItemName := IntToStr(AEnumItemIndex);
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
begin

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


end.

