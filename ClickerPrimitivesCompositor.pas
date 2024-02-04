{
    Copyright (C) 2023 VCC
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

    property OnEvaluateReplacementsFunc: TEvaluateReplacementsFunc write FOnEvaluateReplacementsFunc;
    property OnLoadBitmap: TOnLoadBitmap write FOnLoadBitmap;
    property OnLoadRenderedBitmap: TOnLoadRenderedBitmap write FOnLoadRenderedBitmap;
  end;


implementation


uses
  FPCanvas, Math;


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
      Sender.DoOnLoadRenderedBitmap(SrcBmp, SrcBitmapFnm)
    else
      Sender.DoOnLoadBitmap(SrcBmp, SrcBitmapFnm);

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


procedure ComposePrimitive_GradientFill(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec; AHighContrast: TColor = -1);
var
  TempRect: TRect;
  StartColor, StopColor: TColor;
  GradientDirection: TGradientDirection;
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
    StartColor := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.StartColor), clLime);
    StopColor := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.StopColor), clYellow);
  end;

  GradientDirection := TGradientDirection(Ord(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkGradientFill.Direction) = '1'));

  ABmp.Canvas.GradientFill(TempRect, StartColor, StopColor, GradientDirection);
end;


procedure ComposePrimitive_Text(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec; AHighContrast: TColor = -1);
var
  X, Y: Integer;
  TempText: string;
begin
  X := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkText.X), 30);
  Y := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkText.Y), 40);
  TempText := Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkText.Text);

  //////////////////////// ToDo:  if all cropping values are 0, then draw on ABmp.Canvas directly, otherwise use a temp bmp and crop from it.
  ABmp.Canvas.TextOut(X, Y, TempText);
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


constructor TPrimitivesCompositor.Create;
begin
  inherited Create;

  FFileIndex := 0; //assume first file, unless explicitly updated

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
    @ComposePrimitive_GradientFill,
    @ComposePrimitive_Text,
    @ComposePrimitive_DonutSector
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


procedure BuildHighContrastColors(var AHighContrastColors: TColorArr; APrimitivesLen: Integer);
var
  i: Integer;
var
  R, G, B: Byte;
begin
  SetLength(AHighContrastColors, APrimitivesLen);
  R := 32 + 32;
  G := 128 + 32;
  B := (224 + 32) and $FF;

  for i := 0 to Length(AHighContrastColors) - 1 do
  begin
    AHighContrastColors[i] := RGBToColor(R, G, B);
    Inc(R, 32 + 7);
    Inc(G, 32 + 7);
    Inc(B, 32 + 7);
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
  BuildHighContrastColors(HighContrastColors, Length(APrimitives));

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
  BuildHighContrastColors(HighContrastColors, Length(APrimitives));

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
end;


function TPrimitivesCompositor.GetMaxX(ADestCanvas: TCanvas; var APrimitives: TPrimitiveRecArr): Integer;
var
  i: Integer;
  X, W, W2: Integer;
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
        W := ADestCanvas.TextWidth(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkText.Text));
        Inc(X, W - 1);

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
    end;
  end;
end;


function TPrimitivesCompositor.GetMaxY(ADestCanvas: TCanvas; var APrimitives: TPrimitiveRecArr): Integer;
var
  i: Integer;
  Y, H, H2: Integer;
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
        H := ADestCanvas.TextHeight(DoOnEvaluateReplacementsFunc(APrimitives[i].ClkText.Text));
        Inc(Y, H - 1);

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
    end;
  end;
end;

end.


