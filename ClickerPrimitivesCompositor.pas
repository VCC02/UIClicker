{
    Copyright (C) 2025 VCC
    creation date: Apr 2023
    initial release date: 09 Apr 2023

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


unit ClickerPrimitivesCompositor;

{$H+}
{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Graphics,
  ClickerUtils, ClickerPrimitiveUtils;


type
  TPrimitivesCompositor = class
  private
    FFileIndex: Integer; //primitives file index in the list of MatchPrimitiveFiles property
    FHighContrastOption1: Boolean;
    FHighContrastOption2: Boolean;

    FOnEvaluateReplacementsFunc: TEvaluateReplacementsFunc;
    FOnLoadBitmap: TOnLoadBitmap;
    FOnLoadRenderedBitmap: TOnLoadRenderedBitmap;

    procedure SetFontByPrimitive(ADestCanvas: TCanvas; var APrimitive: TPrimitiveRec);

    function DoOnEvaluateReplacementsFunc(s: string; Recursive: Boolean = True): string;
    function DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function DoOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
  public
    constructor Create;
    procedure ComposePrimitives(ABmp: TBitmap; AOrderIndex: Integer; AUseHighContrastColors: Boolean; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var APrimitiveSettings: TPrimitiveSettings);
    procedure PreviewPrimitive(ABmp: TBitmap; AUseHighContrastColors: Boolean; var APrimitives: TPrimitiveRecArr; APrimitiveIndex: Integer);

    function GetMaxX(ADestCanvas: TCanvas; var APrimitives: TPrimitiveRecArr): Integer;
    function GetMaxY(ADestCanvas: TCanvas; var APrimitives: TPrimitiveRecArr): Integer;

    property FileIndex: Integer read FFileIndex write FFileIndex; //primitives file index in the list of MatchPrimitiveFiles property
    property HighContrastOption1: Boolean write FHighContrastOption1;
    property HighContrastOption2: Boolean write FHighContrastOption2;

    property OnEvaluateReplacementsFunc: TEvaluateReplacementsFunc write FOnEvaluateReplacementsFunc;
    property OnLoadBitmap: TOnLoadBitmap write FOnLoadBitmap;
    property OnLoadRenderedBitmap: TOnLoadRenderedBitmap write FOnLoadRenderedBitmap;
  end;


implementation


uses
  FPCanvas, Math, Types, BitmapProcessing;


type
  TPointArr = array of TPoint;


procedure ComposePrimitive_SetPen(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec; AHighContrast: TColor = -1);
begin
  if AHighContrast <> -1 then
    ABmp.Canvas.Pen.Color := AHighContrast
  else
    ABmp.Canvas.Pen.Color := HexToInt(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Color)); //TColor;

  ABmp.Canvas.Pen.Style := PenStyleNameToIndex(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Style)); //TFPPenStyle;
  ABmp.Canvas.Pen.Width := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Width), 1); //Integer;
  ABmp.Canvas.Pen.Mode := PenModeNameToIndex(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Mode)); //TFPPenMode;
  ABmp.Canvas.Pen.EndCap := PenEndCapNameToIndex(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.EndCap)); //TFPPenEndCap;
  ABmp.Canvas.Pen.JoinStyle := PenJoinStyleNameToIndex(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.JoinStyle)); //TFPPenJoinStyle;
end;


procedure ComposePrimitive_SetBrush(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec; AHighContrast: TColor = -1);
begin
  if AHighContrast <> -1 then
    ABmp.Canvas.Brush.Color := AHighContrast
  else
    ABmp.Canvas.Brush.Color := HexToInt(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetBrush.Color)); //TColor;

  ABmp.Canvas.Brush.Style := BrushStyleNameToIndex(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetBrush.Style)); //TFPBrushStyle;
end;


procedure ComposePrimitive_SetMisc(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec; AHighContrast: TColor = -1);
begin
  ABmp.Canvas.AntialiasingMode := TAntialiasingMode(StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetMisc.AntialiasingMode), Ord(amDontCare)));
end;


procedure ComposePrimitive_SetFont(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec; AHighContrast: TColor = -1);
begin
  if AHighContrast <> -1 then
  begin
    ABmp.Canvas.Font.Color := AHighContrast;
    //ABmp.Canvas.Brush.Color    Do not change Brush.Color! It might already be set by ComposePrimitive_SetBrush
  end
  else
  begin
    ABmp.Canvas.Font.Color := HexToInt(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetFont.ForegroundColor));
    ABmp.Canvas.Brush.Color := HexToInt(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetFont.BackgroundColor));

    if ABmp.Canvas.Brush.Color = $1FFFFFFF then
      ABmp.Canvas.Brush.Style := bsClear;
  end;

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

  ABmp.Canvas.Font.CharSet := APrimitive.ClkSetFont.CharSet;
  ABmp.Canvas.Font.Orientation := APrimitive.ClkSetFont.Orientation;
  ABmp.Canvas.Font.Pitch := APrimitive.ClkSetFont.Pitch;
end;


procedure ComposePrimitive_Image(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec; AHighContrast: TColor = -1);
var
  SrcBmp: TBitmap;
  SrcBitmapFnm: string;
  WillStretchStr: string;
  WillStretch: Boolean;
  TempRect: TRect;
  RenderedExternallyStr: string;
  RenderedExternally: Boolean;
  TempTransparentStr: string;
  TempTransparent: Boolean;
  TempTransparentModeStr: string;
  TempTransparentMode: TTransparentMode;
  TempTransparentColorStr: string;
  TempTransparentColor: TColor;
  BmpExists: Boolean;
  ErrSize: TSize;
begin
  ABmp.Canvas.Brush.Color := HexToInt(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetBrush.Color)); //TColor;
  ABmp.Canvas.Brush.Style := TBrushStyle(StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetBrush.Style), Ord(bsSolid))); //TFPBrushStyle;

  WillStretchStr := Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.Stretch);
  WillStretch := (WillStretchStr = '1') or (UpperCase(WillStretchStr) = 'TRUE');

  TempRect.Left := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.X1), 10);
  TempRect.Top := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.Y1), 20);

  if WillStretch then
  begin
    TempRect.Right := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.X2), 30);
    TempRect.Bottom := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.Y2), 40);
  end;

  SrcBitmapFnm := Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.Path);

  RenderedExternallyStr := Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.RenderedExternally);
  RenderedExternally := (RenderedExternallyStr = '1') or (UpperCase(RenderedExternallyStr) = 'TRUE');
  RenderedExternally := RenderedExternally or (Pos(CExtBmp_PrefixUpperCase, UpperCase(APrimitive.ClkImage.Path)) = 1);

  TempTransparentStr := Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.Transparent);
  TempTransparent := (TempTransparentStr = '1') or (UpperCase(TempTransparentStr) = 'TRUE');

  TempTransparentModeStr := Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.TransparentMode);
  TempTransparentMode := TTransparentMode(Ord((TempTransparentModeStr = '1') or (UpperCase(TempTransparentModeStr) = 'Fixed')));  //Auto = 0, Fixed = 1

  TempTransparentColorStr := Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkImage.TransparentColor);
  TempTransparentColor := HexToInt(TempTransparentColorStr);  //e.g. F0F0F0

  SrcBmp := TBitmap.Create;
  try
    SrcBmp.PixelFormat := pf24bit;
    SrcBmp.Transparent := TempTransparent;
    SrcBmp.TransparentMode := TempTransparentMode;
    SrcBmp.TransparentColor := TempTransparentColor;

    if RenderedExternally then
      BmpExists := Sender.DoOnLoadRenderedBitmap(SrcBmp, SrcBitmapFnm)
    else
      BmpExists := Sender.DoOnLoadBitmap(SrcBmp, SrcBitmapFnm);

    if not BmpExists then
    begin
      SrcBmp.Canvas.Pen.Color := clBlack;
      SrcBmp.Canvas.Brush.Color := clBlack;
      SrcBmp.Canvas.Font.Color := clRed;
      SrcBmp.Canvas.Font.Name := 'DejaVu Sans';  //something which might also be available in Linux
      SrcBmp.Canvas.Font.Size := 8;
      SrcBmp.Canvas.Font.Quality := fqNonAntialiased;
      ErrSize := SrcBmp.Canvas.TextExtent('File not found.');

      SrcBmp.Width := Max(ErrSize.cx, SrcBmp.Canvas.TextWidth(SrcBitmapFnm));
      SrcBmp.Height := ErrSize.cy shl 1;
      SrcBmp.Canvas.Rectangle(0, 0, ErrSize.cx, ErrSize.cy);
      SrcBmp.Canvas.TextOut(0, 0, 'File not found.');
      SrcBmp.Canvas.TextOut(0, ErrSize.cy, SrcBitmapFnm);

      SrcBmp.Canvas.Pen.Color := clTeal;
      SrcBmp.Canvas.Line(0, ErrSize.cy - 1, ErrSize.cx - 1 , ErrSize.cy - 1);
    end;

    if WillStretch then
      ABmp.Canvas.StretchDraw(TempRect, SrcBmp)
    else
      ABmp.Canvas.Draw(TempRect.Left, TempRect.Top, SrcBmp);
  finally
    SrcBmp.Free;
  end;
end;


procedure ComposePrimitive_Line(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec; AHighContrast: TColor = -1);
var
  x1, y1, x2, y2: Integer;
  EvalShowEndpointPixel: string;
begin
  EvalShowEndpointPixel := Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.ShowEndpointPixel);

  x1 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.X1), 10);
  y1 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.Y1), 20);
  x2 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.X2), 30);
  y2 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.Y2), 40);

  if (StrToIntDef(EvalShowEndpointPixel, 0) = 1) or (UpperCase(EvalShowEndpointPixel) = 'TRUE') then
  begin                 //do nothing if x1=x2 or y1=y2
    if x1 < x2 then
      Inc(x2);

    if x1 > x2 then
      Dec(x2);

    if y1 < y2 then
      Inc(y2);

    if y1 > y2 then
      Dec(y2);
  end;

  ABmp.Canvas.Line(x1, y1, x2, y2);
end;


procedure ComposePrimitive_Rect(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec; AHighContrast: TColor = -1);
var
  x1, y1, x2, y2: Integer;
  EvalExtendToEndpointCorner: string;
begin
  EvalExtendToEndpointCorner := Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRect.ExtendToEndpointCorner);

  x1 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRect.X1), 10);
  y1 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRect.Y1), 20);
  x2 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRect.X2), 30);
  y2 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRect.Y2), 40);

  if (StrToIntDef(EvalExtendToEndpointCorner, 0) = 1) or (UpperCase(EvalExtendToEndpointCorner) = 'TRUE') then
  begin                 //do nothing if x1=x2 or y1=y2
    if x1 < x2 then
      Inc(x2);

    if x1 > x2 then
      Dec(x2);

    if y1 < y2 then
      Inc(y2);

    if y1 > y2 then
      Dec(y2);
  end;

  ABmp.Canvas.Rectangle(x1, y1, x2, y2);
end;


procedure ComposePrimitive_RoundedRect(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec; AHighContrast: TColor = -1);
var
  x1, y1, x2, y2, rx, ry: Integer;
  EvalExtendToEndpointCorner: string;
begin
  EvalExtendToEndpointCorner := Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRect.ExtendToEndpointCorner);

  x1 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRoundedRect.X1), 10);
  y1 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRoundedRect.Y1), 20);
  x2 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRoundedRect.X2), 30);
  y2 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRoundedRect.Y2), 40);
  rx := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRoundedRect.RX), 3);
  ry := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRoundedRect.RY), 3);

  if (StrToIntDef(EvalExtendToEndpointCorner, 0) = 1) or (UpperCase(EvalExtendToEndpointCorner) = 'TRUE') then
  begin                 //do nothing if x1=x2 or y1=y2
    if x1 < x2 then
      Inc(x2);

    if x1 > x2 then
      Dec(x2);

    if y1 < y2 then
      Inc(y2);

    if y1 > y2 then
      Dec(y2);
  end;

  ABmp.Canvas.RoundRect(x1, y1, x2, y2, rx, ry);
end;


procedure ComposePrimitive_GradientFill(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec; AHighContrast: TColor = -1);
var
  TempRect: TRect;
  StartColor, StopColor: TColor;
  GradientDirection: TGradientDirection;
  GradientDirectionStr: string;
begin
  TempRect.Left := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.X1), 10);
  TempRect.Top := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.Y1), 20);
  TempRect.Right := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.X2), 30);
  TempRect.Bottom := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.Y2), 40);

  if AHighContrast <> -1 then
  begin
    StartColor := (StartColor xor AHighContrast) and $00FFFFFF; //something faster than working on individual R, G, B, channels
    StopColor := (StopColor xor AHighContrast) and $00FFFFFF; //something faster than working on individual R, G, B, channels
  end
  else
  begin
    StartColor := HexToInt(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.StartColor));
    StopColor := HexToInt(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.StopColor));
  end;

  GradientDirectionStr := Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.Direction);
  GradientDirection := TGradientDirection(Ord((GradientDirectionStr = '1') or (GradientDirectionStr = 'gdHorizontal')));

  ABmp.Canvas.GradientFill(TempRect, StartColor, StopColor, GradientDirection);
end;


procedure ComposePrimitive_Text(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec; AHighContrast: TColor = -1);
var
  X, Y: Integer;
  TempText: string;
  WorkingRect: TRect;
begin
  X := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkText.X), 30);
  Y := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkText.Y), 40);
  TempText := Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkText.Text);

  //////////////////////// ToDo:  if all cropping values are 0, then draw on ABmp.Canvas directly, otherwise use a temp bmp and crop from it.

  if ABmp.Canvas.Font.Orientation = 0 then
    ABmp.Canvas.TextOut(X, Y, TempText)
  else
  begin
    WorkingRect := GetRotatedDrawingRectangle(ABmp.Canvas, TempText);

    ABmp.Width := Max(ABmp.Width, WorkingRect.Width + 2);    //so far, enlarging the bitmap, seemed to work without erasing its content
    ABmp.Height := Max(ABmp.Height, WorkingRect.Height + 2);

    ABmp.Canvas.Rectangle(0, 0, ABmp.Width, ABmp.Height);
    ABmp.Canvas.TextOut(WorkingRect.Left, WorkingRect.Top, TempText);
  end;
end;


procedure ComposePrimitive_DonutSector(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec; AHighContrast: TColor = -1);
var
  TempCx, TempCy, TempRadius1, TempRadius2, TempPointCount: Integer;
  TempStartAngle, TempEndAngle, TempAngleSpacing: Extended;
  TempStartColorFG, TempEndColorFG, TempStartColorBG, TempEndColorBG: TColor;
begin
  TempCx := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkDonutSector.Cx), 100);
  TempCy := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkDonutSector.Cy), 100);
  TempRadius1 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkDonutSector.Radius1), 30);
  TempRadius2 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkDonutSector.Radius2), 90);
  TempPointCount := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkDonutSector.PointCount), 40);
  TempStartAngle := StrToFloatDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkDonutSector.StartAngle), -60);
  TempEndAngle := StrToFloatDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkDonutSector.EndAngle), 240);
  TempAngleSpacing := StrToFloatDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkDonutSector.AngleSpacing), 0);
  TempStartColorFG := HexToInt(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkDonutSector.StartColorFG));
  TempEndColorFG := HexToInt(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkDonutSector.EndColorFG));
  TempStartColorBG := HexToInt(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkDonutSector.StartColorBG));
  TempEndColorBG := HexToInt(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkDonutSector.EndColorBG));

  DrawDonutSector(ABmp.Canvas, TempCx, TempCy, TempRadius1, TempRadius2, TempPointCount,
                  TempStartAngle, TempEndAngle, TempAngleSpacing,
                  TempStartColorFG, TempEndColorFG, TempStartColorBG, TempEndColorBG);
end;


procedure PolygonPointsToArray(Sender: TPrimitivesCompositor; var APrimitive: TPrimitiveRec; var ADest: TPointArr);
var
  ListOfX, ListOfY: TStringList;
  i: Integer;
begin
  try
    ListOfX := TStringList.Create;
    ListOfY := TStringList.Create;
    try
      ListOfX.LineBreak := CPolygonPointLineBreak;
      ListOfY.LineBreak := CPolygonPointLineBreak;
      ListOfX.Text := APrimitive.ClkPolygon.XPoints;
      ListOfY.Text := APrimitive.ClkPolygon.YPoints;

      SetLength(ADest, ListOfX.Count);
      for i := 0 to ListOfX.Count - 1 do
      begin
        ADest[i].X := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(ListOfX.Strings[i]), 30);
        ADest[i].Y := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(ListOfY.Strings[i]), 30);
      end;
    finally
      ListOfX.Free;
      ListOfY.Free;
    end;
  except
  end;
end;


procedure ComposePrimitive_Polygon(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec; AHighContrast: TColor = -1);
var
  Points: TPointArr;
begin
  PolygonPointsToArray(Sender, APrimitive, Points);
  ABmp.Canvas.Polygon(Points);
end;


procedure ComposePrimitive_PolyBezier(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec; AHighContrast: TColor = -1);
var
  IsFilled: Boolean;
  Points: TPointArr;
begin
  IsFilled := Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkPolygon.Filled) = '1';
  PolygonPointsToArray(Sender, APrimitive, Points);
  ABmp.Canvas.PolyBezier(Points, IsFilled);
end;


procedure ComposePrimitive_Ellipse(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec; AHighContrast: TColor = -1);
var
  TempX, TempY, TempRX, TempRY: Integer;
begin
  TempX := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkEllipse.X), 30);
  TempY := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkEllipse.Y), 30);
  TempRX := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkEllipse.RX), 9);
  TempRY := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkEllipse.RY), 3);
  ABmp.Canvas.EllipseC(TempX, TempY, TempRX, TempRY);
end;


constructor TPrimitivesCompositor.Create;
begin
  inherited Create;

  FFileIndex := 0; //assume first file, unless explicitly updated
  FHighContrastOption1 := False;
  FHighContrastOption2 := False;

  FOnEvaluateReplacementsFunc := nil;
  FOnLoadBitmap := nil;
  FOnLoadRenderedBitmap := nil;
end;


function TPrimitivesCompositor.DoOnEvaluateReplacementsFunc(s: string; Recursive: Boolean = True): string;
begin
  if Pos('$FileIndex$', s) > 0 then    // $FileIndex$ var is available for primitives only
    s := StringReplace(s, '$FileIndex$', IntToStr(FFileIndex), [rfReplaceAll]);

  if not Assigned(FOnEvaluateReplacementsFunc) then
    raise Exception.Create('OnEvaluateReplacementsFunc not assigned.')
  else
    Result := FOnEvaluateReplacementsFunc(s, Recursive);
end;


function TPrimitivesCompositor.DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if not Assigned(FOnLoadBitmap) then
    raise Exception.Create('OnLoadBitmap not assigned.')
  else
    Result := FOnLoadBitmap(ABitmap, AFileName);
end;


function TPrimitivesCompositor.DoOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if not Assigned(FOnLoadRenderedBitmap) then
    raise Exception.Create('OnLoadRenderedBitmap not assigned.')
  else
    Result := FOnLoadRenderedBitmap(ABitmap, AFileName);
end;


type
  TComposePrimitivesProc = procedure(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec; AHighContrast: TColor = -1);
  TColorArr = array of TColor;

const
  CComposePrimitives: array[0..CPrimitiveTypeCount - 1] of TComposePrimitivesProc = (
    @ComposePrimitive_SetPen,
    @ComposePrimitive_SetBrush,
    @ComposePrimitive_SetMisc,
    @ComposePrimitive_SetFont,
    @ComposePrimitive_Image,
    @ComposePrimitive_Line,
    @ComposePrimitive_Rect,
    @ComposePrimitive_RoundedRect,
    @ComposePrimitive_GradientFill,
    @ComposePrimitive_Text,
    @ComposePrimitive_DonutSector,
    @ComposePrimitive_Polygon,
    @ComposePrimitive_PolyBezier,
    @ComposePrimitive_Ellipse
  );


procedure PrepareCompositionBitmap(ABmp: TBitmap);
begin
  ABmp.PixelFormat := pf24bit;
  ABmp.Transparent := False;
  ABmp.Canvas.Pen.Style := psSolid;
  ABmp.Canvas.Pen.Width := 1;
  ABmp.Canvas.Brush.Style := bsClear;//bsSolid;
  ABmp.Canvas.Pen.Color := clWhite;
  ABmp.Canvas.Brush.Color := clWhite;
  ABmp.Canvas.Rectangle(0, 0, ABmp.Width, ABmp.Height);
end;


procedure BuildHighContrastColors(var AHighContrastColors: TColorArr; APrimitivesLen: Integer; AColorOption1, AColorOption2: Boolean);
var
  i: Integer;
var
  R, G, B: Byte;
begin
  SetLength(AHighContrastColors, APrimitivesLen);
  R := 32 + 32;
  G := 128 + 32;
  B := (224 + 32) and $FF;

  if AColorOption1 then
  begin
    Inc(R, 32);
    Inc(B, 32);
  end;

  for i := 0 to Length(AHighContrastColors) - 1 do
  begin
    AHighContrastColors[i] := RGBToColor(R, G, B);
    Inc(R, 32 + 7);
    Inc(G, 32 + 7);
    Inc(B, 32 + 7);

    if AColorOption2 then
    begin
      //Inc(R, 0);
      Inc(G, 50 + 17);
      Inc(B, 50 + 37);
    end;
  end;
end;


procedure ErasePreviewBmp(ABmp: TBitmap);
begin
  ABmp.Canvas.Brush.Style := bsSolid;  //reset to some default values
  ABmp.Canvas.Pen.Color := clWhite;
  ABmp.Canvas.Brush.Color := clWhite;
end;


procedure TPrimitivesCompositor.ComposePrimitives(ABmp: TBitmap; AOrderIndex: Integer; AUseHighContrastColors: Boolean; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var APrimitiveSettings: TPrimitiveSettings);
var
  i, NewIndex: Integer;
  HighContrastColors: TColorArr;
begin
  PrepareCompositionBitmap(ABmp);

  if Ord(APrimitiveSettings.CompositorDirection) > Ord(High(TCompositorDirection)) then
  begin
    ABmp.Width := ABmp.Width + 400;
    ABmp.Height := ABmp.Height + 30;
    ABmp.Canvas.Brush.Color := clWhite;
    ABmp.Canvas.Font.Color := clRed;
    ABmp.Canvas.TextOut(30, 0, 'Unsupported compositor direction: ' + IntToStr(Ord(APrimitiveSettings.CompositorDirection)));
    Exit;
  end;

  ErasePreviewBmp(ABmp);
  BuildHighContrastColors(HighContrastColors, Length(APrimitives), FHighContrastOption1, FHighContrastOption2);

  case APrimitiveSettings.CompositorDirection of
    cdTopBot:
    begin
      for i := 0 to Length(APrimitives) - 1 do
      begin
        NewIndex := AOrders[AOrderIndex].Items[i];

        if AUseHighContrastColors then
          CComposePrimitives[APrimitives[NewIndex].PrimitiveType](Self, ABmp, APrimitives[NewIndex], HighContrastColors[i])
        else
          CComposePrimitives[APrimitives[NewIndex].PrimitiveType](Self, ABmp, APrimitives[NewIndex], -1);
      end;
    end;

    cdBotTop:
    begin
      for i := Length(APrimitives) - 1 downto 0 do
      begin
        NewIndex := AOrders[AOrderIndex].Items[i];

        if AUseHighContrastColors then
          CComposePrimitives[APrimitives[NewIndex].PrimitiveType](Self, ABmp, APrimitives[NewIndex], HighContrastColors[i])
        else
          CComposePrimitives[APrimitives[NewIndex].PrimitiveType](Self, ABmp, APrimitives[NewIndex], -1);
      end;
    end;
  end;
end;


procedure TPrimitivesCompositor.PreviewPrimitive(ABmp: TBitmap; AUseHighContrastColors: Boolean; var APrimitives: TPrimitiveRecArr; APrimitiveIndex: Integer);
var
  HighContrastColors: TColorArr;
begin
  BuildHighContrastColors(HighContrastColors, Length(APrimitives), FHighContrastOption1, FHighContrastOption2);

  if AUseHighContrastColors then
    CComposePrimitives[APrimitives[APrimitiveIndex].PrimitiveType](Self, ABmp, APrimitives[APrimitiveIndex], HighContrastColors[APrimitiveIndex])
  else
    CComposePrimitives[APrimitives[APrimitiveIndex].PrimitiveType](Self, ABmp, APrimitives[APrimitiveIndex], -1);

  case APrimitives[APrimitiveIndex].PrimitiveType of
    0: //SetPen
      ABmp.Canvas.Line(2, 7, 13, 7);

    1: //SetBrush
      ABmp.Canvas.Rectangle(0, 0, 16, 16);

    2: //SetMisc
      ABmp.Canvas.TextOut(0, 0, 'AA');

    3: //SetFont
      ABmp.Canvas.TextOut(4, 0, 'F');
  end;
end;


//Ideally, the whole primitives file should be composed, to take the entire stack into account. Then, the proper font settings are applied.
procedure TPrimitivesCompositor.SetFontByPrimitive(ADestCanvas: TCanvas; var APrimitive: TPrimitiveRec);
begin
  ADestCanvas.Font.Name := DoOnEvaluateReplacementsFunc(APrimitive.ClkSetFont.FontName);
  ADestCanvas.Font.Size := APrimitive.ClkSetFont.FontSize;
  ADestCanvas.Font.Bold := APrimitive.ClkSetFont.Bold;
  ADestCanvas.Font.Italic := APrimitive.ClkSetFont.Italic;
  ADestCanvas.Font.Underline := APrimitive.ClkSetFont.Underline; //probably, this won't affect text size
  ADestCanvas.Font.StrikeThrough := APrimitive.ClkSetFont.StrikeOut; //probably, this won't affect text size

  if APrimitive.ClkSetFont.FontQualityUsesReplacement then
    ADestCanvas.Font.Quality := TFontQuality(StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitive.ClkSetFont.FontQualityReplacement), Ord(fqDefault)))  //    CFontQualityStr is not used this time
  else
    ADestCanvas.Font.Quality := APrimitive.ClkSetFont.FontQuality;

  ADestCanvas.Font.CharSet := APrimitive.ClkSetFont.CharSet;
  ADestCanvas.Font.Orientation := APrimitive.ClkSetFont.Orientation;
  ADestCanvas.Font.Pitch := APrimitive.ClkSetFont.Pitch;
end;


function TPrimitivesCompositor.GetMaxX(ADestCanvas: TCanvas; var APrimitives: TPrimitiveRecArr): Integer;
var
  i, j: Integer;
  X, W, W2: Integer;
  ListOfXPoints: TStringList;
  WorkingRect: TRect;
begin
  Result := 0;

  for i := 0 to Length(APrimitives) - 1 do
  begin
    case APrimitives[i].PrimitiveType of
      //CClkSetPenPrimitiveCmdIdx:  // = 0;
      //  ;
      //
      //CClkSetBrushPrimitiveCmdIdx: // = 1;
      //  ;
      //
      //CClkSetMiscPrimitiveCmdIdx: // = 2;
      //  ;
      //
      CClkSetFontPrimitiveCmdIdx: // = 3;
        SetFontByPrimitive(ADestCanvas, APrimitives[i]);

      CClkImagePrimitiveCmdIdx: // = 4;
      begin
        X := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkImage.X2), 10); //X2 should be greater than X1
        if Result < X then
          Result := X;
      end;

      CClkLinePrimitiveCmdIdx: // = 5;
      begin
        X := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkLine.X1), 10);
        if Result < X then
          Result := X;

        X := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkLine.X2), 30);
        if Result < X then
          Result := X;
      end;

      CClkRectPrimitiveCmdIdx: // = 6;
      begin
        X := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkRect.X1), 10);
        if Result < X then
          Result := X;

        X := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkRect.X2), 30);
        if Result < X then
          Result := X;
      end;

      CClkGradientFill: // = 7;
      begin
        X := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkGradientFill.X1), 10);
        if Result < X then
          Result := X;

        X := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkGradientFill.X2), 30);
        if Result < X then
          Result := X;
      end;

      CClkText: // = 8;
      begin
        X := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkText.X), 10);

        if ADestCanvas.Font.Orientation = 0 then
        begin
          W := ADestCanvas.TextWidth(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkText.Text));
          Inc(X, W - 1);
        end
        else
        begin
          WorkingRect := GetRotatedDrawingRectangle(ADestCanvas, APrimitives[i].ClkText.Text);
          Inc(X, WorkingRect.Width - 1 {+ 3});
        end;

        if Result < X then
          Result := X;
      end;

      CClkDonutSector: // = 9
      begin
        X := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkDonutSector.Cx), 10);
        W := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkDonutSector.Radius1), 30);
        W2 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkDonutSector.Radius2), 90);
        Inc(X, Max(W, W2) - 1);

        if Result < X then
          Result := X;
      end;

      CClkPolygon:
      begin
        ListOfXPoints := TStringList.Create;
        try
          ListOfXPoints.LineBreak := CPolygonPointLineBreak;
          ListOfXPoints.Text := APrimitives[i].ClkPolygon.XPoints;

          for j := 0 to ListOfXPoints.Count - 1 do
          begin
            X := StrToIntDef(DoOnEvaluateReplacementsFunc(ListOfXPoints.Strings[j]), 10);
            if Result < X then
              Result := X;
          end;
        finally
          ListOfXPoints.Free;
        end;
      end;

      CClkPolyBezier:
      begin
        //on Bezier, some of the points (i.e. editpoints) can go beyond the polygon itself
        ListOfXPoints := TStringList.Create;
        try
          ListOfXPoints.LineBreak := CPolygonPointLineBreak;
          ListOfXPoints.Text := APrimitives[i].ClkPolygon.XPoints;

          for j := 0 to ListOfXPoints.Count - 1 do
          begin
            X := StrToIntDef(DoOnEvaluateReplacementsFunc(ListOfXPoints.Strings[j]), 10);
            if Result < X then
              Result := X;
          end;
        finally
          ListOfXPoints.Free;
        end;
      end;

      CClkEllipse:
      begin
        X := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkEllipse.X), 10);
        W := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkEllipse.RX), 30);
        Inc(X, W - 1);

        if Result < X then
          Result := X;
      end;
    end; //case
  end; //for
end;


function TPrimitivesCompositor.GetMaxY(ADestCanvas: TCanvas; var APrimitives: TPrimitiveRecArr): Integer;
var
  i, j: Integer;
  Y, H, H2: Integer;
  ListOfYPoints: TStringList;
  WorkingRect: TRect;
begin
  Result := 0;

  for i := 0 to Length(APrimitives) - 1 do
  begin
    case APrimitives[i].PrimitiveType of
      //CClkSetPenPrimitiveCmdIdx:  // = 0;
      //  ;
      //
      //CClkSetBrushPrimitiveCmdIdx: // = 1;
      //  ;
      //
      //CClkSetMiscPrimitiveCmdIdx: // = 2;
      //  ;
      //
      CClkSetFontPrimitiveCmdIdx: // = 3;
        SetFontByPrimitive(ADestCanvas, APrimitives[i]);

      CClkImagePrimitiveCmdIdx: // = 4;
      begin
        Y := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkImage.Y2), 10); //Y2 should be greater than Y1
        if Result < Y then
          Result := Y;
      end;

      CClkLinePrimitiveCmdIdx: // = 5;
      begin
        Y := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkLine.Y1), 10);
        if Result < Y then
          Result := Y;

        Y := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkLine.Y2), 30);
        if Result < Y then
          Result := Y;
      end;

      CClkRectPrimitiveCmdIdx: // = 6;
      begin
        Y := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkRect.Y1), 10);
        if Result < Y then
          Result := Y;

        Y := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkRect.Y2), 30);
        if Result < Y then
          Result := Y;
      end;

      CClkGradientFill: // = 7;
      begin
        Y := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkGradientFill.Y1), 10);
        if Result < Y then
          Result := Y;

        Y := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkGradientFill.Y2), 30);
        if Result < Y then
          Result := Y;
      end;

      CClkText: // = 8;
      begin
        Y := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkText.Y), 10);

        if ADestCanvas.Font.Orientation = 0 then
        begin
          H := ADestCanvas.TextHeight(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkText.Text));
          Inc(Y, H - 1);
        end
        else
        begin
          WorkingRect := GetRotatedDrawingRectangle(ADestCanvas, APrimitives[i].ClkText.Text);
          Inc(Y, WorkingRect.Height - 1 {+ 3});
        end;

        if Result < Y then
          Result := Y;
      end;

      CClkDonutSector: // = 9
      begin
        Y := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkDonutSector.Cy), 10);
        H := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkDonutSector.Radius1), 30);
        H2 := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkDonutSector.Radius2), 90);
        Inc(Y, Max(H, H2) - 1);

        if Result < Y then
          Result := Y;
      end;

      CClkPolygon:
      begin
        ListOfYPoints := TStringList.Create;
        try
          ListOfYPoints.LineBreak := CPolygonPointLineBreak;
          ListOfYPoints.Text := APrimitives[i].ClkPolygon.YPoints;

          for j := 0 to ListOfYPoints.Count - 1 do
          begin
            Y := StrToIntDef(DoOnEvaluateReplacementsFunc(ListOfYPoints.Strings[j]), 10);
            if Result < Y then
              Result := Y;
          end;
        finally
          ListOfYPoints.Free;
        end;
      end;

      CClkPolyBezier:
      begin
        //on Bezier, some of the points (i.e. editpoints) can go beyond the polygon itself
        ListOfYPoints := TStringList.Create;
        try
          ListOfYPoints.LineBreak := CPolygonPointLineBreak;
          ListOfYPoints.Text := APrimitives[i].ClkPolygon.YPoints;

          for j := 0 to ListOfYPoints.Count - 1 do
          begin
            Y := StrToIntDef(DoOnEvaluateReplacementsFunc(ListOfYPoints.Strings[j]), 10);
            if Result < Y then
              Result := Y;
          end;
        finally
          ListOfYPoints.Free;
        end;
      end;

      CClkEllipse:
      begin
        Y := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkEllipse.Y), 10);
        H := StrToIntDef(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkEllipse.RY), 30);
        Inc(Y, H - 1);

        if Result < Y then
          Result := Y;
      end;
    end; //case
  end; //for
end;

end.


