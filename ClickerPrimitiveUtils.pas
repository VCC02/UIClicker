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


unit ClickerPrimitiveUtils;

{$H+}
{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Graphics, FPCanvas, ClickerUtils;

type
  TClkSetPen = record
    Color: string; //TColor;
    Style: string; //TFPPenStyle;
    Width: string; //Integer;
    Mode: string; //TFPPenMode;
    EndCap: string; //TFPPenEndCap;
    JoinStyle: string; //TFPPenJoinStyle;
  end;

  TClkSetBrush = record
    Color: string; //TColor;
    Style: string; //TFPBrushStyle;
  end;

  TClkSetMisc = record
    AntialiasingMode: string; //TAntialiasingMode
  end;

  TClkSetFont = TClkFindControlMatchBitmapText;

  TClkImage = record
    X1: string;
    X2: string;
    Y1: string;
    Y2: string;
    Path: string; //path to a bmp (or png) file, which will be part of the composition
    Stretch: string; //Boolean
    RenderedExternally: string; //Boolean;
    Transparent: string; //Boolean;
    TransparentMode: string; //Auto or Fixed
    TransparentColor: string; //BGR (6-digit hexa)
  end;

  TClkLine = record
    X1: string;
    X2: string;
    Y1: string;
    Y2: string;
    ShowEndpointPixel: string;
  end;

  TClkRect = record
    X1: string;
    X2: string;
    Y1: string;
    Y2: string;
    ExtendToEndpointCorner: string;
  end;

  TClkGradientFill = record
    X1: string;
    X2: string;
    Y1: string;
    Y2: string;
    StartColor: string;
    StopColor: string;
    Direction: string; //TGradientDirection;
  end;

  TClkText = record
    Text: string;
    X: string;
    Y: string;
    //Evaluate: Boolean;  //Add this field if ever needed. Otherwise, the text is automatically evaluated. (i.e. all replacements in "Text" are evaluated to their values)
  end;

  TClkDonutSector = record
    Cx: string;          // Integer;
    Cy: string;          // Integer;
    Radius1: string;     // Integer;
    Radius2: string;     // Integer;
    PointCount: string;  // Integer;
    StartAngle: string;  // Extended;
    EndAngle: string;    // Extended;
    AngleSpacing: string;// Extended;
    StartColorFG: string;// TColor
    EndColorFG: string;  // TColor
    StartColorBG: string;// TColor
    EndColorBG: string;  // TColor
  end;


const
  CClkSetPenPrimitiveCmdIdx = 0;
  CClkSetBrushPrimitiveCmdIdx = 1;
  CClkSetMiscPrimitiveCmdIdx = 2;
  CClkSetFontPrimitiveCmdIdx = 3;
  CClkImagePrimitiveCmdIdx = 4;
  CClkLinePrimitiveCmdIdx = 5;
  CClkRectPrimitiveCmdIdx = 6;
  CClkGradientFill = 7;
  CClkText = 8;
  CClkDonutSector = 9;


type
  TCompositorDirection = (cdTopBot, cdBotTop);   //cdTopBot means that the composition is made from [0] to [n-1]. The last item will be fully visible on the final image.

  TPrimitiveSettings = record
    CompositorDirection: TCompositorDirection;
  end;

  TPrimitiveRec = record        //Only one of the "primitive" fields is used at a time. This is similar to TClkActionRec.
    PrimitiveType: Integer; //index of one of the following fields
    PrimitiveName: string;
    Selected: Boolean;  //Selected in editor. Allows dragging and resizing (except for text).

    ClkSetPen: TClkSetPen;
    ClkSetBrush: TClkSetBrush;
    ClkSetMisc: TClkSetMisc;
    ClkSetFont: TClkSetFont;
    ClkImage: TClkImage;
    ClkLine: TClkLine;
    ClkRect: TClkRect;
    ClkGradientFill: TClkGradientFill;
    ClkText: TClkText;
    ClkDonutSector: TClkDonutSector;
  end;

  PPrimitiveRec = ^TPrimitiveRec;

  TPrimitiveRecArr = array of TPrimitiveRec;

  TCompositionOrder = record
    Items: TIntArr;
    Name: string;
  end;

  TCompositionOrderArr = array of TCompositionOrder;

  TOnLoadPrimitivesFile = procedure(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings) of object;
  TOnSavePrimitivesFile = procedure(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings) of object;


const
  CPrimitiveTypeCount = 10;
  CPrimitiveNames: array[0..CPrimitiveTypeCount - 1] of string = (
    'SetPen', 'SetBrush', 'SetMisc', 'SetFont', 'Image', 'Line', 'Rect', 'GradientFill', 'Text', 'DonutSector');

  CPenStyleStr: array[TPenStyle] of string = ('psSolid', 'psDash', 'psDot', 'psDashDot', 'psDashDotDot', 'psinsideFrame', 'psPattern', 'psClear');
  CPenModeStr: array[TPenMode] of string = (
    'pmBlack', 'pmWhite', 'pmNop', 'pmNot', 'pmCopy', 'pmNotCopy',
    'pmMergePenNot', 'pmMaskPenNot', 'pmMergeNotPen', 'pmMaskNotPen', 'pmMerge',
    'pmNotMerge', 'pmMask', 'pmNotMask', 'pmXor', 'pmNotXor'
  );

  CPenEndCapStr: array[TPenEndCap] of string = ('pecRound', 'pecSquare', 'pecFlat');
  CPenJoinStyleStr: array[TPenJoinStyle] of string = ('pjsRound', 'pjsBevel', 'pjsMiter');

  CBrushStyleStr: array[TBrushStyle] of string = (
    'bsSolid', 'bsClear', 'bsHorizontal', 'bsVertical', 'bsFDiagonal',
    'bsBDiagonal', 'bsCross', 'bsDiagCross', 'bsImage', 'bsPattern'
  );

  CAntialiasingModeStr: array[TAntialiasingMode] of string = ('amDontCare', 'amOn', 'amOff');
  CGradientDirectionStr: array[TGradientDirection] of string = ('gdVertical', 'gdHorizontal');

  CCompositorDirectionStr: array[TCompositorDirection] of string = ('cdTopBot', 'cdBotTop');


function PrimitiveTypeNameToIndex(AName: string): Integer;

function PenStyleNameToIndex(AName: string): TPenStyle;
function PenModeNameToIndex(AName: string): TPenMode;
function PenEndCapNameToIndex(AName: string): TPenEndCap;
function PenJoinStyleNameToIndex(AName: string): TPenJoinStyle;
function BrushStyleNameToIndex(AName: string): TBrushStyle;
function CompositorDirectionToIndex(AName: string): TCompositorDirection;

procedure DrawDonutSector(ACanvas: TCanvas; ACx, ACy, Radius1, Radius2, APointCount: Integer; AStartAngle, AEndAngle, AAngleSpacing: Extended; AStartColorFG, AEndColorFG, AStartColorBG, AEndColorBG: TColor);


implementation


uses
  BitmapProcessing, Math;


function PrimitiveTypeNameToIndex(AName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to CPrimitiveTypeCount - 1 do
    if CPrimitiveNames[i] = AName then
    begin
      Result := i;
      Break;
    end;
end;


function PenStyleNameToIndex(AName: string): TPenStyle;
var
  i: TPenStyle;
begin
  Result := Low(TPenStyle);
  for i := Low(TPenStyle) to High(TPenStyle) do
    if CPenStyleStr[i] = AName then
    begin
      Result := i;
      Break;
    end;
end;


function PenModeNameToIndex(AName: string): TPenMode;
var
  i: TPenMode;
begin
  Result := Low(TPenMode);
  for i := Low(TPenMode) to High(TPenMode) do
    if CPenModeStr[i] = AName then
    begin
      Result := i;
      Break;
    end;
end;


function PenEndCapNameToIndex(AName: string): TPenEndCap;
var
  i: TPenEndCap;
begin
  Result := Low(TPenEndCap);
  for i := Low(TPenEndCap) to High(TPenEndCap) do
    if CPenEndCapStr[i] = AName then
    begin
      Result := i;
      Break;
    end;
end;


function PenJoinStyleNameToIndex(AName: string): TPenJoinStyle;
var
  i: TPenJoinStyle;
begin
  Result := Low(TPenJoinStyle);
  for i := Low(TPenJoinStyle) to High(TPenJoinStyle) do
    if CPenJoinStyleStr[i] = AName then
    begin
      Result := i;
      Break;
    end;
end;


function BrushStyleNameToIndex(AName: string): TBrushStyle;
var
  i: TBrushStyle;
begin
  Result := Low(TBrushStyle);
  for i := Low(TBrushStyle) to High(TBrushStyle) do
    if CBrushStyleStr[i] = AName then
    begin
      Result := i;
      Break;
    end;
end;


function CompositorDirectionToIndex(AName: string): TCompositorDirection;
var
  i: TCompositorDirection;
begin
  Result := Low(TCompositorDirection);
  for i := Low(TCompositorDirection) to High(TCompositorDirection) do
    if CCompositorDirectionStr[i] = AName then
    begin
      Result := i;
      Break;
    end;
end;

                                                                                                    //degrees
procedure DrawDonutSector(ACanvas: TCanvas; ACx, ACy, Radius1, Radius2, APointCount: Integer; AStartAngle, AEndAngle, AAngleSpacing: Extended; AStartColorFG, AEndColorFG, AStartColorBG, AEndColorBG: TColor);
var
  ZonePoints: array of TPoint;   //4 points
  FGColors, BGColors: TColorArr;
  CurrentAngle, NextAngle, AngleDiff: Extended;
  CosCAng, CosNAng, SinCAng, SinNAng: Extended;
  i: Integer;
begin
  SetLength(ZonePoints, 4); //two points on Radius1 and two points on Radius2
  SetLength(FGColors, APointCount);
  SetLength(BGColors, APointCount);

  GenerateGradientColors(AStartColorFG, AEndColorFG, APointCount, FGColors);
  GenerateGradientColors(AStartColorBG, AEndColorBG, APointCount, BGColors);

  AngleDiff := DegToRad(AEndAngle - AStartAngle);

  if (AStartColorFG = clNone) and (AEndColorFG = clNone) then
    ACanvas.Pen.Style := psClear;

  for i := 0 to APointCount - 1 do
  begin
    ACanvas.Pen.Color := FGColors[i];
    ACanvas.Brush.Color := BGColors[i];

    CurrentAngle := DegToRad(AStartAngle) + (i / APointCount) * AngleDiff;
    NextAngle := DegToRad(AStartAngle - AAngleSpacing) + ((i + 1) / APointCount) * AngleDiff;

    CosCAng := Cos(CurrentAngle);
    CosNAng := Cos(NextAngle);
    SinCAng := Sin(CurrentAngle);
    SinNAng := Sin(NextAngle);

    ZonePoints[0].X := ACx + Round(Radius1 * CosCAng);
    ZonePoints[1].X := ACx + Round(Radius2 * CosCAng);
    ZonePoints[2].X := ACx + Round(Radius2 * CosNAng);
    ZonePoints[3].X := ACx + Round(Radius1 * CosNAng);

    ZonePoints[0].Y := ACy - Round(Radius1 * SinCAng);
    ZonePoints[1].Y := ACy - Round(Radius2 * SinCAng);
    ZonePoints[2].Y := ACy - Round(Radius2 * SinNAng);
    ZonePoints[3].Y := ACy - Round(Radius1 * SinNAng);

    ACanvas.Polygon(ZonePoints);
  end;
end;


end.

