{
    Copyright (C) 2023 VCC
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


unit BitmapProcessing;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF Windows}
    Windows,
  {$ENDIF}
  SysUtils, Graphics, ExtCtrls, ClickerUtils;

type                      //2048 is used as 2^11 = 2048, as an optimization. See CanvasToMat_24bit and CanvasToMat_32bit. The next higher resolution would be 4096 * 4096.
  TCanvasMat = array[0 .. 2048 * 2048] of SmallInt;//SmallInt may be better than Byte, because it is signed 16-bit
  PCanvasMat = ^TCanvasMat;

  TRGBPCanvasMat = record
    PR: PCanvasMat;
    PG: PCanvasMat;
    PB: PCanvasMat;
  end;

  THeapCanvasMat = record
    Mat: array of array of SmallInt;
    OffsetX, OffsetY, Width, Height: Integer; //defines the cropped area from the original bitmap
  end;

  TRGBRec = record   //24-bit pixels
    B, G, R: Byte;
  end;
  TScanLineArr = array[0..0] of TRGBRec;

  TRGBAlphaRec = record   //32-bit pixels
    B, G, R, A: Byte;
  end;
  TScanLineAlphaArr = array[0..0] of TRGBAlphaRec;

  EBmpMatchTimeout = class(Exception)

  end;
  

procedure Line(ACnv: TCanvas; x1, y1, x2, y2: Integer);
procedure ScreenShot(SrcHandle: THandle; DestBitmap: TBitmap; XOffsetSrc, YOffsetSrc, Width, Height: Integer);

function BitmapPosMatch(Algorithm: TMatchBitmapAlgorithm;
                        AlgorithmSettings: TMatchBitmapAlgorithmSettings;
                        SourceBitmap, SubBitmap: TBitmap;
                        ColorErrorLevel: Integer;
                        out SubCnvXOffset, SubCnvYOffset: Integer;
                        var AFoundBitmaps: TCompRecArr;
                        TotalErrorCount, FastSearchColorErrorCount: Integer;
                        AUseFastSearch, AIgnoreBackgroundColor, AGetAllBitmaps: Boolean;
                        ABackgroundColor: TColor;
                        var AIgnoredColorsArr: TColorArr;
                        ASleepySearch: Byte;
                        AOutsideTickCount, APrecisionTimeout: QWord;
                        out AResultedErrorCount: Integer;
                        AStopSearchOnDemand: PBoolean = nil;
                        StopSearchOnMismatch: Boolean = True): Boolean;

function AvgTwoTrueColors(Color1, Color2: TColor): TColor;
procedure AvgBitmapWithColor(ASrcBitmap, ADestBitmap: TBitmap; AColor: TColor; XOffset: Integer = -1; YOffset: Integer = -1; Width: Integer = -1; Height: Integer = -1); //if X, Y, W, H are specified, the function operates on that area only
procedure AvgBitmapWithBitmap(ASrcABitmap, ASrcBBitmap, ADestBitmap: TBitmap; XOffset: Integer = -1; YOffset: Integer = -1; Width: Integer = -1; Height: Integer = -1);
function BitmapsAreEqual(ASrcABitmap, ASrcBBitmap: TBitmap; AWidth, AHeight: Integer): Boolean;
procedure GenerateGradientColors(AStartColor, AEndColor: TColor; APointCount: Integer; var AResult: TColorArr);
procedure MakeImageContentTransparent(AImg: TImage);
procedure DrawSearchGrid(AImg: TImage; AlgorithmSettings: TMatchBitmapAlgorithmSettings; AGridWidth, AGridHeight: Integer; AGridColor: TColor; ADisplayGridLineOption: TDisplayGridLineOption);
procedure GetHistogram(ABitmap: TBitmap; var AHist, AHistColorCounts: TIntArr);


implementation


uses
  Forms, Classes, IntegerList;


procedure RandomSleep(ASleepySearch: Byte);
begin
  if Random(8) = 7 then
  begin
    if ASleepySearch and 2 = 2 then     //this should be 0 when using threaded search
      Application.ProcessMessages;

    if ASleepySearch and 1 = 1 then
      Sleep(1);
  end;
end;


procedure Line(ACnv: TCanvas; x1, y1, x2, y2: Integer);
begin
  ACnv.MoveTo(x1, y1);
  ACnv.LineTo(x2, y2);
end;


function ColorIndexInIntArr(AColor: TColor; var AArr: TIntArr): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(AArr) - 1 do
    if AArr[i] = AColor then
    begin
      Result := i;
      Break;
    end;
end;


function CompFunc({const} a, b: Int64): Integer;
begin
  a := a shr 32;  //discard colors
  b := b shr 32;

  if a > b then
    Result := -1
  else
    if a = b then
      Result := 0
    else
      Result := 1;
end;


procedure SortHistogram(var AHist, AHistColorCounts: TIntArr);
var
  SortingArray: TInt64List;
  Item64: Int64;
  i: Integer;
begin
  SortingArray := TInt64List.Create;
  try
    SortingArray.Capacity := Length(AHist);
    for i := 0 to Length(AHist) - 1 do
      SortingArray.Add(AHist[i] or (Int64(AHistColorCounts[i]) shl 32));

    SortingArray.Sort(@CompFunc);

    for i := 0 to Length(AHist) - 1 do
    begin
      Item64 := SortingArray.Items[i];
      AHist[i] := Item64;
      AHistColorCounts[i] := Item64 shr 32;
    end;
  finally
    SortingArray.Free;
  end;
end;


procedure SizedHistogram_24bit(ABitmap: TBitmap; ALeft, ATop, AWidth, AHeight: Integer; var AHist, AHistColorCounts: TIntArr);
var
  i, j: Integer;
  ACanvasLine: ^TScanLineArr;
  AColorRec: TRGBRec;
  Width, Height: Integer;
  TempColor: TColor;
  ColorCounts: TIntArr;
begin
  Width := Max(AWidth, 1);
  Height := Max(AHeight, 1);
  ALeft := Max(ALeft, 0);
  ATop := Max(ATop, 0);

  SetLength(ColorCounts, $FFFFFF + 1);
  FillChar(ColorCounts[0], Length(ColorCounts) * SizeOf(Integer), 0);

  ABitmap.BeginUpdate;  //added for FP (as recommended)
  try
    for i := ATop to Height - 1 do
    begin
      ACanvasLine := ABitmap.{%H-}ScanLine[i];

      for j := ALeft to Width - 1 do
      begin
        AColorRec := ACanvasLine[j];

        TempColor := AColorRec.B shl 16 + AColorRec.G shl 8 + AColorRec.R;
        Inc(ColorCounts[TempColor]);
      end;
    end;
  finally
    ABitmap.EndUpdate;
  end;

  for i := 0 to Length(ColorCounts) - 1 do
    if ColorCounts[i] > 0 then
    begin
      SetLength(AHist, Length(AHist) + 1);
      SetLength(AHistColorCounts, Length(AHistColorCounts) + 1);

      AHist[Length(AHist) - 1] := i;
      AHistColorCounts[Length(AHistColorCounts) - 1] := ColorCounts[i];
    end;

  SetLength(ColorCounts, 0);
  SortHistogram(AHist, AHistColorCounts);
end;


procedure Histogram_24bit(ABitmap: TBitmap; var AHist, AHistColorCounts: TIntArr);
begin
  SizedHistogram_24bit(ABitmap, 0, 0, ABitmap.Width, ABitmap.Height, AHist, AHistColorCounts);
end;


procedure SizedHistogram_32bit(ABitmap: TBitmap; ALeft, ATop, AWidth, AHeight: Integer; var AHist, AHistColorCounts: TIntArr);
var
  i, j: Integer;
  ACanvasLine: ^TScanLineAlphaArr;   //notice alpha
  AColorRec: TRGBAlphaRec;           //notice alpha
  Width, Height: Integer;
  TempColor: TColor;
  ColorCounts: TIntArr;
begin
  Width := Max(AWidth, 1);
  Height := Max(AHeight, 1);
  ALeft := Max(ALeft, 0);
  ATop := Max(ATop, 0);

  SetLength(ColorCounts, $FFFFFF + 1);
  FillChar(ColorCounts[0], Length(ColorCounts) * SizeOf(Integer), 0);

  ABitmap.BeginUpdate;  //added for FP (as recommended)
  try
    for i := ATop to Height - 1 do
    begin
      ACanvasLine := ABitmap.{%H-}ScanLine[i];

      for j := ALeft to Width - 1 do
      begin
        AColorRec := ACanvasLine[j];

        TempColor := AColorRec.B shl 16 + AColorRec.G shl 8 + AColorRec.R;
        Inc(ColorCounts[TempColor]);
      end;
    end;
  finally
    ABitmap.EndUpdate;
  end;

  for i := 0 to Length(ColorCounts) - 1 do
    if ColorCounts[i] > 0 then
    begin
      SetLength(AHist, Length(AHist) + 1);
      SetLength(AHistColorCounts, Length(AHistColorCounts) + 1);

      AHist[Length(AHist) - 1] := i;
      AHistColorCounts[Length(AHistColorCounts) - 1] := ColorCounts[i];
    end;

  SetLength(ColorCounts, 0);
  SortHistogram(AHist, AHistColorCounts);
end;


procedure Histogram_32bit(ABitmap: TBitmap; var AHist, AHistColorCounts: TIntArr);
begin
  SizedHistogram_32bit(ABitmap, 0, 0, ABitmap.Width, ABitmap.Height, AHist, AHistColorCounts);
end;


procedure GetSizedHistogram(ABitmap: TBitmap; ALeft, ATop, AWidth, AHeight: Integer; var AHist, AHistColorCounts: TIntArr);
begin
  case ABitmap.PixelFormat of
    pf24bit:
      SizedHistogram_24bit(ABitmap, ALeft, ATop, AWidth, AHeight, AHist, AHistColorCounts);

    pf32bit:
      SizedHistogram_32bit(ABitmap, ALeft, ATop, AWidth, AHeight, AHist, AHistColorCounts);

    else
      raise Exception.Create('Unsupported PixelFormat when computing histogram. Format index: ' + IntToStr(Ord(ABitmap.PixelFormat)));
  end;
end;


procedure GetHistogram(ABitmap: TBitmap; var AHist, AHistColorCounts: TIntArr);
begin
  case ABitmap.PixelFormat of
    pf24bit:
      Histogram_24bit(ABitmap, AHist, AHistColorCounts);

    pf32bit:
      Histogram_32bit(ABitmap, AHist, AHistColorCounts);

    else
      raise Exception.Create('Unsupported PixelFormat when computing histogram. Format index: ' + IntToStr(Ord(ABitmap.PixelFormat)));
  end;
end;


procedure CanvasToMat_24bit(ABitmap: TBitmap; AMat_R, AMat_G, AMat_B: PCanvasMat);
var
  i, j, Index, i_shl_11: Integer;
  ACanvasLine: ^TScanLineArr;
  AColorRec: TRGBRec;
  Width, Height: Integer;
begin
  Width := ABitmap.Width;   //to avoid calling a getter inside the for loop
  Height := ABitmap.Height; //to avoid calling a getter inside the for loop

  ABitmap.BeginUpdate;  //added for FP (as recommended)
  try
    for i := 0 to Height - 1 do
    begin
      ACanvasLine := ABitmap.{%H-}ScanLine[i];
      i_shl_11 := i shl 11;                 // 2^11 = 2048  (see TCanvasMat datatype)

      for j := 0 to Width - 1 do
      begin
        Index := i_shl_11 + j;
        AColorRec := ACanvasLine[j];

        AMat_R[Index] := AColorRec.R and $FF;
        AMat_G[Index] := AColorRec.G and $FF;
        AMat_B[Index] := AColorRec.B and $FF;
      end;
    end;
  finally
    ABitmap.EndUpdate;
  end;
end;


procedure CanvasToMat_32bit(ABitmap: TBitmap; AMat_R, AMat_G, AMat_B: PCanvasMat);
var
  i, j, Index, i_shl_11: Integer;
  ACanvasLine: ^TScanLineAlphaArr;   //notice alpha
  AColorRec: TRGBAlphaRec;           //notice alpha
  Width, Height: Integer;
begin
  Width := ABitmap.Width;   //to avoid calling a getter inside the for loop
  Height := ABitmap.Height; //to avoid calling a getter inside the for loop

  ABitmap.BeginUpdate;  //added for FP (as recommended)
  try
    for i := 0 to Height - 1 do
    begin
      ACanvasLine := ABitmap.{%H-}ScanLine[i];
      i_shl_11 := i shl 11;

      for j := 0 to Width - 1 do
      begin
        Index := i_shl_11 + j;
        AColorRec := ACanvasLine[j];

        AMat_R[Index] := AColorRec.R and $FF;
        AMat_G[Index] := AColorRec.G and $FF;
        AMat_B[Index] := AColorRec.B and $FF;
      end;
    end;
  finally
    ABitmap.EndUpdate;
  end;
end;


procedure MatToCanvas_24bit(AMat_R, AMat_G, AMat_B: PCanvasMat; ABitmap: TBitmap);
var
  i, j, Index, i_shl_11: Integer;
  ACanvasLine: ^TScanLineArr;
  AColorRec: TRGBRec;
  Width, Height: Integer;
begin
  Width := ABitmap.Width;   //to avoid calling a getter inside the for loop
  Height := ABitmap.Height; //to avoid calling a getter inside the for loop

  ABitmap.BeginUpdate;  //added for FP (as recommended)
  try
    for i := 0 to Height - 1 do
    begin
      ACanvasLine := ABitmap.{%H-}ScanLine[i];
      i_shl_11 := i shl 11;                 // 2^11 = 2048  (see TCanvasMat datatype)

      for j := 0 to Width - 1 do
      begin
        Index := i_shl_11 + j;

        AColorRec.R := AMat_R[Index] and $FF;
        AColorRec.G := AMat_G[Index] and $FF;
        AColorRec.B := AMat_B[Index] and $FF;

        ACanvasLine^[j] := AColorRec;
      end;
    end;
  finally
    ABitmap.EndUpdate;
  end;
end;


procedure MatToCanvas_32bit(AMat_R, AMat_G, AMat_B: PCanvasMat; ABitmap: TBitmap);
var
  i, j, Index, i_shl_11: Integer;
  ACanvasLine: ^TScanLineAlphaArr;   //notice alpha
  AColorRec: TRGBAlphaRec;           //notice alpha
  Width, Height: Integer;
begin
  Width := ABitmap.Width;   //to avoid calling a getter inside the for loop
  Height := ABitmap.Height; //to avoid calling a getter inside the for loop

  ABitmap.BeginUpdate;  //added for FP (as recommended)
  try
    for i := 0 to Height - 1 do
    begin
      ACanvasLine := ABitmap.{%H-}ScanLine[i];
      i_shl_11 := i shl 11;                 // 2^11 = 2048  (see TCanvasMat datatype)

      for j := 0 to Width - 1 do
      begin
        Index := i_shl_11 + j;

        AColorRec.R := AMat_R[Index] and $FF;
        AColorRec.G := AMat_G[Index] and $FF;
        AColorRec.B := AMat_B[Index] and $FF;
        AColorRec.A := 0; //probably a new argument would be needed for alpha

        ACanvasLine^[j] := AColorRec;
      end;
    end;
  finally
    ABitmap.EndUpdate;
  end;
end;


procedure CanvasToMat(ABitmap: TBitmap; AMat_R, AMat_G, AMat_B: PCanvasMat);
begin
  case ABitmap.PixelFormat of
    pf24bit: CanvasToMat_24bit(ABitmap, AMat_R, AMat_G, AMat_B);
    pf32bit: CanvasToMat_32bit(ABitmap, AMat_R, AMat_G, AMat_B);
  else
    raise Exception.Create('Pixel format not handled in CanvasToMat. Format index: ' + IntToStr(Ord(ABitmap.PixelFormat)) + ' (' + IntToStr(PIXELFORMAT_BPP[ABitmap.PixelFormat]) + '-bit)');
  end;
end;


procedure MatToCanvas(AMat_R, AMat_G, AMat_B: PCanvasMat; ABitmap: TBitmap);
begin
  case ABitmap.PixelFormat of
    pf24bit: MatToCanvas_24bit(AMat_R, AMat_G, AMat_B, ABitmap);
    pf32bit: MatToCanvas_32bit(AMat_R, AMat_G, AMat_B, ABitmap);
  else
    raise Exception.Create('Pixel format not handled in MatToCanvas. Format index: ' + IntToStr(Ord(ABitmap.PixelFormat)) + ' (' + IntToStr(PIXELFORMAT_BPP[ABitmap.PixelFormat]) + '-bit)');
  end;
end;


procedure SysRedGreenBlue(ACol: TColor; var AR, AG, AB: SmallInt);
var
  R, G, B: Byte;
begin
  ACol := ColorToRGB(ACol);  //convert if syscolor
  RedGreenBlue(ACol, R, G, B);
  AR := R;
  AG := G;
  AB := B;
end;


function ColorMatches(AColR, AColG, AColB, ASubR, ASubG, ASubB: SmallInt; ColorErrorLevel: Integer): Boolean;
begin
  Result := (Abs(AColR - ASubR) <= ColorErrorLevel) and    //comparing against ColorErrorLevel, means to include antialiasing pixels, which are a mix between the background color and text color
            (Abs(AColG - ASubG) <= ColorErrorLevel) and
            (Abs(AColB - ASubB) <= ColorErrorLevel);
end;


function AtLeastOneColorMatches(var AIgnoredColorsArr: TColorArr; ASubR, ASubG, ASubB: SmallInt; ColorErrorLevel: Integer): Boolean;
var
  i: Integer;
  ColR, ColG, ColB: SmallInt;
begin
  Result := False;

  for i := 0 to Length(AIgnoredColorsArr) - 1 do
  begin
    SysRedGreenBlue(AIgnoredColorsArr[i], ColR, ColG, ColB);

    if ColorMatches(ColR, ColG, ColB, ASubR, ASubG, ASubB, ColorErrorLevel) then
    begin
      Result := True;
      Break;
    end;
  end;
end;


procedure GenerateGradientColors(AStartColor, AEndColor: TColor; APointCount: Integer; var AResult: TColorArr);
var
  i: Integer;
  ColorScale: Extended;
  StartColorR, StartColorG, StartColorB: SmallInt;
  EndColorR, EndColorG, EndColorB: SmallInt;
  DiffR, DiffG, DiffB: SmallInt;
  ResR, ResG, ResB: SmallInt;
begin
  SysRedGreenBlue(AStartColor, StartColorR, StartColorG, StartColorB);
  SysRedGreenBlue(AEndColor, EndColorR, EndColorG, EndColorB);
  DiffR := StartColorR - EndColorR;
  DiffG := StartColorG - EndColorG;
  DiffB := StartColorB - EndColorB;

  SetLength(AResult, APointCount);
  for i := 0 to APointCount - 1 do
  begin
    ColorScale := i / APointCount;
    ResR := Round(StartColorR - ColorScale * DiffR);
    ResG := Round(StartColorG - ColorScale * DiffG);
    ResB := Round(StartColorB - ColorScale * DiffB);

    AResult[i] := RGBToColor(ResR, ResG, ResB);
  end;
end;


function CanvasPos(SrcMat, SubMat: TRGBPCanvasMat;
                   SubCnvXOffset, SubCnvYOffset, SrcCnvWidth, SrcCnvHeight, SubCnvWidth, SubCnvHeight, ColorErrorLevel: Integer;
                   AIgnoreBackgroundColor: Boolean;
                   ABackgroundColor: TColor;
                   var AIgnoredColorsArr: TColorArr;
                   AOutsideTickCount, APrecisionTimeout: QWord;
                   AStopSearchOnDemand: PBoolean = nil;
                   StopAtErrorCount: Integer = -1): Integer;
var
  x, y: Integer;
  ErrorCount: Integer;
  SrcIndex, SubIndex, Src_y_shl_11, Sub_y_shl_11: Integer;
  SourceCanvasMat_R, SubCanvasMat_R: PCanvasMat;
  SourceCanvasMat_G, SubCanvasMat_G: PCanvasMat;
  SourceCanvasMat_B, SubCanvasMat_B: PCanvasMat;
  ABackgroundColor_R: SmallInt;
  ABackgroundColor_G: SmallInt;
  ABackgroundColor_B: SmallInt;
begin
  Result := 0;

  if (SrcCnvWidth < SubCnvWidth + SubCnvXOffset) or (SrcCnvHeight < SubCnvHeight + SubCnvYOffset) then
    Exit;

  SourceCanvasMat_R := SrcMat.PR;
  SourceCanvasMat_G := SrcMat.PG;
  SourceCanvasMat_B := SrcMat.PB;
  SubCanvasMat_R := SubMat.PR;
  SubCanvasMat_G := SubMat.PG;
  SubCanvasMat_B := SubMat.PB;

  ErrorCount := 0;

  if AIgnoreBackgroundColor then
    SysRedGreenBlue(ABackgroundColor, ABackgroundColor_R, ABackgroundColor_G, ABackgroundColor_B);

  for y := 0 to SubCnvHeight - 1 do
  begin
    Src_y_shl_11 := (y + SubCnvYOffset) shl 11;      //shl 11 means that this algorithm expects the max matrix size to be 2048 * 2048
    Sub_y_shl_11 := y shl 11;

    for x := 0 to SubCnvWidth - 1 do
    begin
      if (AOutsideTickCount > 0) and (GetTickCount64 - AOutsideTickCount > APrecisionTimeout) then
      begin
        if StopAtErrorCount > -1 then
          Result := StopAtErrorCount + 1
        else
          raise EBmpMatchTimeout.Create('PrecisionTimeout on searching for SubControl.'); //Exit;
      end;

      SrcIndex := Src_y_shl_11 + x + SubCnvXOffset;
      SubIndex := Sub_y_shl_11 + x;

      if ((Length(AIgnoredColorsArr) > 0) and
           AtLeastOneColorMatches(AIgnoredColorsArr, SubCanvasMat_R[SubIndex], SubCanvasMat_G[SubIndex], SubCanvasMat_B[SubIndex], ColorErrorLevel) or
         (AIgnoreBackgroundColor and
        (ColorMatches(ABackgroundColor_R, ABackgroundColor_G, ABackgroundColor_B, SubCanvasMat_R[SubIndex], SubCanvasMat_G[SubIndex], SubCanvasMat_B[SubIndex], ColorErrorLevel)))) then
        //((ABackgroundColor_R = SubCanvasMat_R[SubIndex]) and
        //(ABackgroundColor_G = SubCanvasMat_G[SubIndex]) and
        //(ABackgroundColor_B = SubCanvasMat_B[SubIndex]))) then
      begin
        //Continue; // commented, because there is a "StopSearchOnDemand" verification
      end
      else
        if ((Abs(SourceCanvasMat_R[SrcIndex] - SubCanvasMat_R[SubIndex]) > ColorErrorLevel) or
          (Abs(SourceCanvasMat_G[SrcIndex] - SubCanvasMat_G[SubIndex]) > ColorErrorLevel) or
          (Abs(SourceCanvasMat_B[SrcIndex] - SubCanvasMat_B[SubIndex]) > ColorErrorLevel)) then
        begin
          Inc(ErrorCount);
          Result := ErrorCount;

          if (StopAtErrorCount > -1) and (ErrorCount > StopAtErrorCount) then  //There may be "colorful" pixels in a black text, as a result of antialiasing. Too many such pixels mean that the compared text is different.
            Exit; //do not break - there are two 'for' loops
        end;

      if (AStopSearchOnDemand <> nil) and AStopSearchOnDemand^ then
        Exit;

      //if (AOutsideTickCount > 0) and (GetTickCount64 - AOutsideTickCount > APrecisionTimeout) then
      //  Exit;
    end; //for
  end; //for
end;


function CanvasPosMatch(SrcMat, SubMat: TRGBPCanvasMat; SubCnvXOffset, SubCnvYOffset, SrcCnvWidth, SrcCnvHeight, SubCnvWidth, SubCnvHeight, ColorErrorLevel, AcceptedErrorCount: Integer; AIgnoreBackgroundColor: Boolean; ABackgroundColor: TColor; var AIgnoredColorsArr: TColorArr; AOutsideTickCount, APrecisionTimeout: QWord; out AResultedErrorCount: Integer; AStopSearchOnDemand: PBoolean = nil; StopSearchOnMismatch: Boolean = True): Boolean;
begin
  if StopSearchOnMismatch then
    AResultedErrorCount := CanvasPos(SrcMat, SubMat, SubCnvXOffset, SubCnvYOffset, SrcCnvWidth, SrcCnvHeight, SubCnvWidth, SubCnvHeight, ColorErrorLevel, AIgnoreBackgroundColor, ABackgroundColor, AIgnoredColorsArr, AOutsideTickCount, APrecisionTimeout, AStopSearchOnDemand, AcceptedErrorCount)
  else
    AResultedErrorCount := CanvasPos(SrcMat, SubMat, SubCnvXOffset, SubCnvYOffset, SrcCnvWidth, SrcCnvHeight, SubCnvWidth, SubCnvHeight, ColorErrorLevel, AIgnoreBackgroundColor, ABackgroundColor, AIgnoredColorsArr, AOutsideTickCount, APrecisionTimeout, AStopSearchOnDemand);

  Result := AResultedErrorCount < AcceptedErrorCount + 1;
end;


//ABkHist, ABkHistColorCounts define the background histogram
//ASearchedHist, ASearchedHistColorCounts define the searched bitmap histogram
//AMinPercentColorMatch sets how much of the ASearchedBmp histogram data should be found in ABkBmp histogram data. The value is expected to be from 0 to 1.
//AMostSignificantColorCount sets how many items from the histogram arrays should be verified.
//AColorErrorLevel is the difference between the two compared colors, for every color channel (R, G, B). It is possible that a scaled value is required, compared to the one entered by a user as a FindSubControl property.
function MatchAreaByHistogram(var ABkHist, ABkHistColorCounts, ASearchedHist, ASearchedHistColorCounts: TIntArr;   //The function assumes that the two histograms are sorted
                              AMinPercentColorMatch: Double;
                              AMostSignificantColorCount, AColorErrorLevel: Integer): Boolean;
var
  i, j: Integer;
  CurrentColorPercent: Double;
  MatchPercents: array of Double;
  BackgroundColor_R, BackgroundColor_G, BackgroundColor_B: SmallInt;
  SearchedColor_R, SearchedColor_G, SearchedColor_B: SmallInt;
begin
  SetLength(MatchPercents, Min(AMostSignificantColorCount, Length(ASearchedHist)));  //Assumes that Length(ASearchedHist) < Length(ABkHist). Otherwise, nothing will be found, as expected.

  for i := 0 to Length(MatchPercents) - 1 do
    MatchPercents[i] := 0;

  for i := 0 to Min(AMostSignificantColorCount, Length(ASearchedHist)) - 1 do
    for j := 0 to Min(AMostSignificantColorCount, Length(ABkHist)) - 1 do
    begin
      SysRedGreenBlue(ABkHist[j], BackgroundColor_R, BackgroundColor_G, BackgroundColor_B);
      SysRedGreenBlue(ASearchedHist[j], SearchedColor_R, SearchedColor_G, SearchedColor_B);

      if ColorMatches(BackgroundColor_R, BackgroundColor_G, BackgroundColor_B,
                      SearchedColor_R, SearchedColor_G, SearchedColor_B,
                      AColorErrorLevel) then
      begin
        CurrentColorPercent := ASearchedHistColorCounts[i] / Max(ABkHistColorCounts[j], 1);
        if Double(MatchPercents[i]) < CurrentColorPercent then
          MatchPercents[i] := CurrentColorPercent;
      end;
    end;

  Result := True;
  for i := 0 to Length(MatchPercents) - 1 do
  begin
    //MessageBox(0, PChar('i = ' + IntToStr(i) + '  val = ' + FloatToStr(MatchPercents[i])), 'Match percents', MB_ICONINFORMATION);

    if MatchPercents[i] < AMinPercentColorMatch then
    begin
      Result := False;
      Break;
    end;
  end;
end;


function BitmapPosMatch_BruteForceWithOffset(SrcMat, SubMat: TRGBPCanvasMat;
                                             XOffset, YOffset, XAmount, YAmount, SrcWidth, SrcHeight, SubWidth, SubHeight, ColorErrorLevel: Integer;
                                             var SubCnvXOffset, SubCnvYOffset: Integer;
                                             TotalErrorCount: Integer;
                                             AIgnoreBackgroundColor: Boolean;
                                             ABackgroundColor: TColor;
                                             var AIgnoredColorsArr: TColorArr;
                                             ASleepySearch: Byte;
                                             AOutsideTickCount, APrecisionTimeout: QWord;
                                             out AResultedErrorCount: Integer;
                                             AStopSearchOnDemand: PBoolean = nil;
                                             StopSearchOnMismatch: Boolean = True): Boolean;
var
  x, y: Integer;
  //Res: TPointArr;
begin
  Result := False;

  for y := YOffset to YAmount do
    for x := XOffset to XAmount do
    begin
      if (AStopSearchOnDemand <> nil) and AStopSearchOnDemand^ then
        Exit;

      if (AOutsideTickCount > 0) and (GetTickCount64 - AOutsideTickCount > APrecisionTimeout) then
        raise EBmpMatchTimeout.Create('PrecisionTimeout on searching for SubControl.'); //Exit;

      if CanvasPosMatch(SrcMat,
                        SubMat,
                        x,
                        y,
                        SrcWidth,
                        SrcHeight,
                        SubWidth,
                        SubHeight,
                        ColorErrorLevel,
                        TotalErrorCount,
                        AIgnoreBackgroundColor,
                        ABackgroundColor,
                        AIgnoredColorsArr,
                        AOutsideTickCount,
                        APrecisionTimeout,
                        AResultedErrorCount,
                        AStopSearchOnDemand,
                        StopSearchOnMismatch) then
      begin
        Result := True;
        SubCnvXOffset := x;
        SubCnvYOffset := y;
        Exit;
      end;

      ////verify again here, right before the RandomSleep call
      //if (AOutsideTickCount > 0) and (GetTickCount64 - AOutsideTickCount > APrecisionTimeout) then
      //  Exit;

      RandomSleep(ASleepySearch);
    end;
end;


function BitmapPosMatch_BruteForce(SrcMat, SubMat: TRGBPCanvasMat;
                                   ASourceBitmapWidth, ASourceBitmapHeight, ASubBitmapWidth, ASubBitmapHeight: Integer;
                                   ColorErrorLevel: Integer;
                                   out SubCnvXOffset, SubCnvYOffset: Integer;
                                   var AFoundBitmaps: TCompRecArr;
                                   TotalErrorCount, FastSearchColorErrorCount: Integer;
                                   AUseFastSearch, AIgnoreBackgroundColor, AGetAllBitmaps: Boolean;
                                   ABackgroundColor: TColor;
                                   var AIgnoredColorsArr: TColorArr;
                                   ASleepySearch: Byte;
                                   AOutsideTickCount, APrecisionTimeout: QWord;
                                   out AResultedErrorCount: Integer;
                                   AStopSearchOnDemand: PBoolean = nil;
                                   StopSearchOnMismatch: Boolean = True): Boolean;
const
  CPreSize = 5; //px
var
  xx, yy: Integer;
  SrcWidth, SrcHeight: Integer;
  SubWidth, SubHeight: Integer;
  XAmount, YAmount: Integer;
  PreErrorCount: Integer;
  PreSizeX, PreSizeY: Integer;
begin                     //default optimization: searching a 5px x 5px area, then if that matches, go for full size search
  Result := False;

  SubCnvXOffset := -1;
  SubCnvYOffset := -1;

  SrcWidth := ASourceBitmapWidth;
  SrcHeight := ASourceBitmapHeight;
  SubWidth := ASubBitmapWidth;
  SubHeight := ASubBitmapHeight;
  
  XAmount := SrcWidth - SubWidth;  // +1 ????
  YAmount := SrcHeight - SubHeight;  // +1 ????

  //old, full search  - to be used as an option
  if not AUseFastSearch then
    Result := BitmapPosMatch_BruteForceWithOffset(SrcMat,
                                                  SubMat,
                                                  0,
                                                  0,
                                                  XAmount,
                                                  YAmount,
                                                  SrcWidth,
                                                  SrcHeight,
                                                  SubWidth,
                                                  SubHeight,
                                                  ColorErrorLevel,
                                                  SubCnvXOffset,
                                                  SubCnvYOffset,
                                                  TotalErrorCount,
                                                  AIgnoreBackgroundColor,
                                                  ABackgroundColor,
                                                  AIgnoredColorsArr,
                                                  ASleepySearch,
                                                  AOutsideTickCount,
                                                  APrecisionTimeout,
                                                  AResultedErrorCount,
                                                  AStopSearchOnDemand)
  else
  begin
    if FastSearchColorErrorCount = -1 then
      PreErrorCount := Round(TotalErrorCount / (SubWidth * SubHeight / Sqr(CPreSize)))
    else
      PreErrorCount := FastSearchColorErrorCount;

    PreSizeX := Min(SubWidth, CPreSize);
    PreSizeY := Min(SubHeight, CPreSize);

    for yy := 0 to YAmount do                //AResultedErrorCount makes sense only when the searched bitmap has the same size as the area in which it is searched for
      for xx := 0 to XAmount do
      begin
        if (AStopSearchOnDemand <> nil) and AStopSearchOnDemand^ then
          Exit;

        if (AOutsideTickCount > 0) and (GetTickCount64 - AOutsideTickCount > APrecisionTimeout) then
          raise EBmpMatchTimeout.Create('PrecisionTimeout on searching for SubControl.'); //Exit;
                                                                                                                           //Avoid ignoring on "pre-search", because of false positives. Those would cause a longer search time.
        if CanvasPosMatch(SrcMat, SubMat, xx, yy, SrcWidth, SrcHeight, PreSizeX, PreSizeY, ColorErrorLevel, PreErrorCount, AIgnoreBackgroundColor {False}, ABackgroundColor, AIgnoredColorsArr, AOutsideTickCount, APrecisionTimeout, AResultedErrorCount, AStopSearchOnDemand, StopSearchOnMismatch) then
        begin
          if CanvasPosMatch(SrcMat, SubMat, xx, yy, SrcWidth, SrcHeight, SubWidth, SubHeight, ColorErrorLevel, TotalErrorCount, AIgnoreBackgroundColor, ABackgroundColor, AIgnoredColorsArr, AOutsideTickCount, APrecisionTimeout, AResultedErrorCount, AStopSearchOnDemand, StopSearchOnMismatch) then
          begin
            Result := True;
            SubCnvXOffset := xx;
            SubCnvYOffset := yy;
            SetLength(AFoundBitmaps, Length(AFoundBitmaps) + 1);
            AFoundBitmaps[Length(AFoundBitmaps) - 1].XOffsetFromParent := xx;
            AFoundBitmaps[Length(AFoundBitmaps) - 1].YOffsetFromParent := yy;
            AFoundBitmaps[Length(AFoundBitmaps) - 1].ResultedErrorCount := AResultedErrorCount;

            if not AGetAllBitmaps then
              Exit;                      //stop only if a single bitmap result is expected
          end;

          ////verify again here, right before the RandomSleep call
          //if (AOutsideTickCount > 0) and (GetTickCount64 - AOutsideTickCount > APrecisionTimeout) then
          //  Exit;

          RandomSleep(ASleepySearch);
        end;
      end;
  end;
end;


function BitmapPosMatch_SimpleGrid_XYMultipleAndOffsets(SrcMat, SubMat: TRGBPCanvasMat;
                                                        AlgorithmSettings: TMatchBitmapAlgorithmSettings;
                                                        SourceBitmap, SubBitmap: TBitmap;
                                                        ColorErrorLevel: Integer;
                                                        out SubCnvXOffset, SubCnvYOffset: Integer;
                                                        var AFoundBitmaps: TCompRecArr;
                                                        TotalErrorCount: Integer;
                                                        AIgnoreBackgroundColor, AGetAllBitmaps: Boolean;
                                                        ABackgroundColor: TColor;
                                                        var AIgnoredColorsArr: TColorArr;
                                                        ASleepySearch: Byte;
                                                        AOutsideTickCount, APrecisionTimeout: QWord;
                                                        out AResultedErrorCount: Integer;
                                                        AStopSearchOnDemand: PBoolean = nil;
                                                        StopSearchOnMismatch: Boolean = True): Boolean;
var
  x, y: Integer;
  SrcWidth, SrcHeight: Integer;
  SubWidth, SubHeight: Integer;
  XAmount, YAmount: Integer;
begin
  Result := False;

  SubCnvXOffset := -1;
  SubCnvYOffset := -1;
  
  SrcWidth := SourceBitmap.Width;
  SrcHeight := SourceBitmap.Height;
  SubWidth := SubBitmap.Width;
  SubHeight := SubBitmap.Height;
  
  XAmount := SrcWidth - SubWidth;  // +1 ????
  YAmount := SrcHeight - SubHeight;  // +1 ????

  if AlgorithmSettings.YMultipleOf < 1 then
    AlgorithmSettings.YMultipleOf := 1;

  if AlgorithmSettings.XMultipleOf < 1 then
    AlgorithmSettings.XMultipleOf := 1;

  for y := AlgorithmSettings.YOffset to YAmount do
    if (y - AlgorithmSettings.YOffset) mod AlgorithmSettings.YMultipleOf = 0 then      //to be replaced with Dec(y, AlgorithmSettings.YMultipleOf)
    begin
      for x := AlgorithmSettings.XOffset to XAmount do
        if (x - AlgorithmSettings.XOffset) mod AlgorithmSettings.XMultipleOf = 0 then  //to be replaced with Dec(x, AlgorithmSettings.XMultipleOf)
        begin
          if (AStopSearchOnDemand <> nil) and AStopSearchOnDemand^ then
            Exit;

          if (AOutsideTickCount > 0) and (GetTickCount64 - AOutsideTickCount > APrecisionTimeout) then
            raise EBmpMatchTimeout.Create('PrecisionTimeout on searching for SubControl.'); //Exit;

          if CanvasPosMatch(SrcMat, SubMat, x, y, SrcWidth, SrcHeight, SubWidth, SubHeight, ColorErrorLevel, TotalErrorCount, AIgnoreBackgroundColor, ABackgroundColor, AIgnoredColorsArr, AOutsideTickCount, APrecisionTimeout, AResultedErrorCount, AStopSearchOnDemand, StopSearchOnMismatch) then
          begin
            Result := True;
            SubCnvXOffset := x;
            SubCnvYOffset := y;
            SetLength(AFoundBitmaps, Length(AFoundBitmaps) + 1);
            AFoundBitmaps[Length(AFoundBitmaps) - 1].XOffsetFromParent := x;
            AFoundBitmaps[Length(AFoundBitmaps) - 1].YOffsetFromParent := y;

            if not AGetAllBitmaps then
              Exit;                      //stop only if a single bitmap result is expected
          end;

          ////verify again here, right before the RandomSleep call
          //if (AOutsideTickCount > 0) and (GetTickCount64 - AOutsideTickCount > APrecisionTimeout) then
          //  Exit;

          RandomSleep(ASleepySearch);
        end;
    end;
end;


function BitmapPosMatch_RawHistogramMatchingZones(SrcMat, SubMat: TRGBPCanvasMat;
                                                  SourceBitmap, SubBitmap: TBitmap;
                                                  ColorErrorLevel: Integer;
                                                  out SubCnvXOffset, SubCnvYOffset: Integer;
                                                  var AFoundBitmaps: TCompRecArr;
                                                  TotalErrorCount, FastSearchColorErrorCount: Integer;
                                                  AUseFastSearch, AIgnoreBackgroundColor, AGetAllBitmaps: Boolean;
                                                  ABackgroundColor: TColor;
                                                  var AIgnoredColorsArr: TColorArr;
                                                  ASleepySearch: Byte;
                                                  AOutsideTickCount, APrecisionTimeout: QWord;
                                                  out AResultedErrorCount: Integer;
                                                  AStopSearchOnDemand: PBoolean = nil;
                                                  StopSearchOnMismatch: Boolean = True): Boolean;
const
  CMostSignificantColorCount: Integer = 10;
var
  BkHist, BkHistColorCounts, SearchedHist, SearchedHistColorCounts: TIntArr;
  x, y, ZonesCountX, ZonesCountY: Integer;
  ZoneLeft, ZoneTop, ZoneWidth, ZoneHeight, BkWidth, BkHeight: Integer;
begin
  Result := False;
  if (SourceBitmap.Width = 0) or (SubBitmap.Width = 0) or (SourceBitmap.Height = 0) or (SubBitmap.Height = 0) then
    Exit;

  GetHistogram(SubBitmap, SearchedHist, SearchedHistColorCounts);
  try
    BkWidth := SourceBitmap.Width;
    BkHeight := SourceBitmap.Height;

    ZonesCountX := BkWidth div SubBitmap.Width;    //the remainder wouldn't be able to match anyway, so using "div" should be ok
    ZonesCountY := BkHeight div SubBitmap.Height;
    ZoneWidth := SubBitmap.Width;
    ZoneHeight := SubBitmap.Height;

    SubCnvXOffset := -1;
    SubCnvYOffset := -1;

    for y := 0 to ZonesCountY - 1 do
    begin
      ZoneTop := y * ZoneHeight;
      if (AStopSearchOnDemand <> nil) and AStopSearchOnDemand^ then
        Exit;

      for x := 0 to ZonesCountX - 1 do
      begin
        if (AStopSearchOnDemand <> nil) and AStopSearchOnDemand^ then
          Exit;

        if (AOutsideTickCount > 0) and (GetTickCount64 - AOutsideTickCount > APrecisionTimeout) then
          raise EBmpMatchTimeout.Create('PrecisionTimeout on searching for SubControl.'); //Exit;

        ZoneLeft := x * ZoneWidth;
        GetSizedHistogram(SourceBitmap, ZoneLeft, ZoneTop, ZoneWidth, ZoneHeight, BkHist, BkHistColorCounts);
        try
          Result := MatchAreaByHistogram(BkHist, BkHistColorCounts, SearchedHist, SearchedHistColorCounts, 56 / 100, CMostSignificantColorCount, ColorErrorLevel + 10);
          if Result then
          begin
            //MessageBox(0, PChar('Matched by histogram at x = ' + IntToStr(x) + '  y = ' + IntToStr(y)), 'Bmp proc', MB_ICONINFORMATION);
            //if BitmapPosMatch_BruteForce(SrcMat,
            //                             SubMat,
            //                             ZoneWidth,
            //                             ZoneHeight,
            //                             ZoneWidth,
            //                             ZoneHeight,
            //                             ColorErrorLevel,
            //                             SubCnvXOffset,
            //                             SubCnvYOffset,
            //                             AFoundBitmaps,
            //                             TotalErrorCount,
            //                             FastSearchColorErrorCount,
            //                             AUseFastSearch,
            //                             AIgnoreBackgroundColor,
            //                             AGetAllBitmaps,
            //                             ABackgroundColor,
            //                             AIgnoredColorsArr,
            //                             ASleepySearch,
            //                             AOutsideTickCount,
            //                             APrecisionTimeout,
            //                             AResultedErrorCount,
            //                             AStopSearchOnDemand,
            //                             StopSearchOnMismatch) then
            if BitmapPosMatch_BruteForceWithOffset(SrcMat,
                                                   SubMat,
                                                   ZoneLeft, //XOffset,
                                                   ZoneTop, //YOffset,
                                                   ZoneWidth {shr 1}, //XAmount,
                                                   ZoneHeight {shr 1}, //YAmount,
                                                   ZoneWidth shl 1 - 1,  //SrcWidth aka background width        -1, because ZoneWidth shl 1 belongs to the next zone
                                                   ZoneHeight shl 1 - 1, //SrcHeight aka background height      -1, because ZoneWidth shl 1 belongs to the next zone
                                                   ZoneWidth,  //SubWidth
                                                   ZoneHeight, //SubHeight
                                                   ColorErrorLevel,
                                                   SubCnvXOffset,
                                                   SubCnvYOffset,
                                                   TotalErrorCount,
                                                   AIgnoreBackgroundColor,
                                                   ABackgroundColor,
                                                   AIgnoredColorsArr,
                                                   ASleepySearch,
                                                   AOutsideTickCount,
                                                   APrecisionTimeout,
                                                   AResultedErrorCount,
                                                   AStopSearchOnDemand) then
            begin
              Result := True;
              Exit; //no mult result for this algorithm :(
            end;
          end;  //matched by histogram
        finally
          SetLength(BkHist, 0);
          SetLength(BkHistColorCounts, 0);
        end;

        RandomSleep(ASleepySearch);
      end;
    end;
  finally
    SetLength(SearchedHist, 0);
    SetLength(SearchedHistColorCounts, 0);
  end;
end;


function BitmapPosMatch(Algorithm: TMatchBitmapAlgorithm;
                        AlgorithmSettings: TMatchBitmapAlgorithmSettings;
                        SourceBitmap, SubBitmap: TBitmap;
                        ColorErrorLevel: Integer;
                        out SubCnvXOffset, SubCnvYOffset: Integer;
                        var AFoundBitmaps: TCompRecArr;
                        TotalErrorCount, FastSearchColorErrorCount: Integer;
                        AUseFastSearch, AIgnoreBackgroundColor, AGetAllBitmaps: Boolean;
                        ABackgroundColor: TColor;
                        var AIgnoredColorsArr: TColorArr;
                        ASleepySearch: Byte;
                        AOutsideTickCount, APrecisionTimeout: QWord;
                        out AResultedErrorCount: Integer;
                        AStopSearchOnDemand: PBoolean = nil;
                        StopSearchOnMismatch: Boolean = True): Boolean;
const
  {%H-}CDebugSubBmpPath = 'E:\SubBmp.bmp';
var
  SourceCanvasMat_R, SubCanvasMat_R: PCanvasMat;
  SourceCanvasMat_G, SubCanvasMat_G: PCanvasMat;
  SourceCanvasMat_B, SubCanvasMat_B: PCanvasMat;
  SizeOfTCanvasMat: Integer;
  SrcMat, SubMat: TRGBPCanvasMat;
begin
  {   //debug code
  if not FileExists(CDebugSubBmpPath) then
    SubBitmap.SaveToFile(CDebugSubBmpPath);
  }

  SizeOfTCanvasMat := SizeOf(TCanvasMat);

  GetMem(SourceCanvasMat_R, SizeOfTCanvasMat);  // 6 * SizeOf(TCanvasMat) = 6 * 8MB  = 48MB.
  GetMem(SourceCanvasMat_G, SizeOfTCanvasMat);
  GetMem(SourceCanvasMat_B, SizeOfTCanvasMat);
  GetMem(SubCanvasMat_R, SizeOfTCanvasMat);
  GetMem(SubCanvasMat_G, SizeOfTCanvasMat);
  GetMem(SubCanvasMat_B, SizeOfTCanvasMat);
  try
    SrcMat.PR := SourceCanvasMat_R;
    SrcMat.PG := SourceCanvasMat_G;
    SrcMat.PB := SourceCanvasMat_B;
    SubMat.PR := SubCanvasMat_R;
    SubMat.PG := SubCanvasMat_G;
    SubMat.PB := SubCanvasMat_B;

    try
      CanvasToMat(SourceBitmap, SourceCanvasMat_R, SourceCanvasMat_G, SourceCanvasMat_B);
    except
      on E: Exception do
        raise Exception.Create(E.Message + '  in SourceBitmap (usually the control from where a section is searched for)'); //SrcCompSearchAreaBitmap
    end;

    try
      CanvasToMat(SubBitmap, SubCanvasMat_R, SubCanvasMat_G, SubCanvasMat_B);
    except
      on E: Exception do
        raise Exception.Create(E.Message + '  in SubBitmap (usually a user text or a user bitmap to be searched for)'); //BitmapToSearchFor
    end;

    case Algorithm of
      mbaBruteForce:
        Result := BitmapPosMatch_BruteForce(SrcMat,
                                            SubMat,
                                            SourceBitmap.Width,
                                            SourceBitmap.Height,
                                            SubBitmap.Width,
                                            SubBitmap.Height,
                                            ColorErrorLevel,
                                            SubCnvXOffset,
                                            SubCnvYOffset,
                                            AFoundBitmaps,
                                            TotalErrorCount,
                                            FastSearchColorErrorCount,
                                            AUseFastSearch,
                                            AIgnoreBackgroundColor,
                                            AGetAllBitmaps,
                                            ABackgroundColor,
                                            AIgnoredColorsArr,
                                            ASleepySearch,
                                            AOutsideTickCount,
                                            APrecisionTimeout,
                                            AResultedErrorCount,
                                            AStopSearchOnDemand,
                                            StopSearchOnMismatch);

      mbaXYMultipleAndOffsets:
        Result := BitmapPosMatch_SimpleGrid_XYMultipleAndOffsets(SrcMat,
                                                                 SubMat,
                                                                 AlgorithmSettings,
                                                                 SourceBitmap,
                                                                 SubBitmap,
                                                                 ColorErrorLevel,
                                                                 SubCnvXOffset,
                                                                 SubCnvYOffset,
                                                                 AFoundBitmaps,
                                                                 TotalErrorCount,
                                                                 AIgnoreBackgroundColor,
                                                                 AGetAllBitmaps,
                                                                 ABackgroundColor,
                                                                 AIgnoredColorsArr,
                                                                 ASleepySearch,
                                                                 AOutsideTickCount,
                                                                 APrecisionTimeout,
                                                                 AResultedErrorCount,
                                                                 AStopSearchOnDemand,
                                                                 StopSearchOnMismatch);
      mbaRawHistogramZones:
        Result := BitmapPosMatch_RawHistogramMatchingZones(SrcMat,
                                                           SubMat,
                                                           SourceBitmap,
                                                           SubBitmap,
                                                           ColorErrorLevel,
                                                           SubCnvXOffset,
                                                           SubCnvYOffset,
                                                           AFoundBitmaps,
                                                           TotalErrorCount,
                                                           FastSearchColorErrorCount,
                                                           AUseFastSearch,
                                                           AIgnoreBackgroundColor,
                                                           AGetAllBitmaps,
                                                           ABackgroundColor,
                                                           AIgnoredColorsArr,
                                                           ASleepySearch,
                                                           AOutsideTickCount,
                                                           APrecisionTimeout,
                                                           AResultedErrorCount,
                                                           AStopSearchOnDemand,
                                                           StopSearchOnMismatch);
    else
      raise Exception.Create('Bitmap search algorithm #' + IntToStr(Ord(Algorithm)) + ' not implemented.');
    end;
  finally
    FreeMem(SourceCanvasMat_R, SizeOfTCanvasMat);
    FreeMem(SourceCanvasMat_G, SizeOfTCanvasMat);
    FreeMem(SourceCanvasMat_B, SizeOfTCanvasMat);
    FreeMem(SubCanvasMat_R, SizeOfTCanvasMat);
    FreeMem(SubCanvasMat_G, SizeOfTCanvasMat);
    FreeMem(SubCanvasMat_B, SizeOfTCanvasMat);
  end;
end;


procedure ScreenShot(SrcHandle: THandle; DestBitmap: TBitmap; XOffsetSrc, YOffsetSrc, Width, Height: Integer);
//const
//  CDebugPath = 'E:\ScrShot.bmp';
var
  DC: HDC;
begin
  DC := GetWindowDC(SrcHandle);
  try
    DestBitmap.Width := Width;
    DestBitmap.Height := Height;
    DestBitmap.PixelFormat := pf24bit;

    DestBitmap.Width := Width;
    DestBitmap.Height := Height;
    DestBitmap.Canvas.Pen.Color := clWhite;
    DestBitmap.Canvas.Brush.Color := clWhite;
    DestBitmap.Canvas.Rectangle(0, 0, Width - 1, Height - 1);     //required, at least in FP, otherwise, the content is full black

    BitBlt(destBitmap.Canvas.Handle,
          0, //X   x-coord of destination upper-left corner
          0, //Y   y-coord of destination upper-left corner
          Width,   //src and dest width
          Height,  //src and dest height
          DC,      //src DC
          XOffsetSrc,     //offset for source
          YOffsetSrc,     //offset for source
          SRCCOPY);

    {   //debug code
    if not FileExists(CDebugPath) then
      destBitmap.SaveToFile(CDebugPath);
    }
  finally
    ReleaseDC(SrcHandle, DC);
  end;
end;


function AvgTwoTrueColors(Color1, Color2: TColor): TColor;
var
  R1, G1, B1: Word;
  R2, G2, B2: Word;
  R, G, B: Byte;
begin
  R1 := Color1 and $FF;
  R2 := Color2 and $FF;

  G1 := (Color1 shr 8) and $FF;
  G2 := (Color2 shr 8) and $FF;

  B1 := (Color1 shr 16) and $FF;
  B2 := (Color2 shr 16) and $FF;

  R := Word(R1 + R2) shr 1;
  G := Word(G1 + G2) shr 1;
  B := Word(B1 + B2) shr 1;

  Result := B shl 16 + G shl 8 + R;
end;


procedure AdjustImgArea(var XOffset, YOffset, Width, Height: Integer; ASrcBitmap: TBitmap);
begin
  if (XOffset < 0) or (YOffset < 0) then
  begin
    XOffset := 0;
    YOffset := 0;
  end;

  if XOffset > ASrcBitmap.Width - 1 then
    XOffset := ASrcBitmap.Width - 1;

  if YOffset > ASrcBitmap.Height - 1 then
    YOffset := ASrcBitmap.Height - 1;

  if (Width < 0) or (Height < 0) then
  begin
    Width := ASrcBitmap.Width;
    Height := ASrcBitmap.Height;
  end;

  if XOffset + Width > ASrcBitmap.Width then
    Width := ASrcBitmap.Width - XOffset;

  if YOffset + Height > ASrcBitmap.Height then
    Height := ASrcBitmap.Height - YOffset;
end;


//This function expects that ADestBitmap is already loaded with a bitmap (likely the same one as ASrcBitmap) and it overwrites an area, specified by XOffset, YOffset, Width, Height.
procedure AvgBitmapWithColor(ASrcBitmap, ADestBitmap: TBitmap; AColor: TColor; XOffset: Integer = -1; YOffset: Integer = -1; Width: Integer = -1; Height: Integer = -1);
var   // 6 * SizeOf(TCanvasMat) =  6 * 8388608 bytes = 48MB  somehow, TaskManager reports twice the size (i.e. 96MB)
  SrcCanvasMat_R, DestCanvasMat_R: PCanvasMat;
  SrcCanvasMat_G, DestCanvasMat_G: PCanvasMat;
  SrcCanvasMat_B, DestCanvasMat_B: PCanvasMat;
  x, y: Integer;
  SrcIndex, Src_y_shl_11: Integer;
  SizeOfTCanvasMat: Integer;
begin
  if (ASrcBitmap.Width = 0) or (ASrcBitmap.Height = 0) then
    Exit;

  SizeOfTCanvasMat := SizeOf(TCanvasMat);

  GetMem(SrcCanvasMat_R, SizeOfTCanvasMat);
  GetMem(SrcCanvasMat_G, SizeOfTCanvasMat);
  GetMem(SrcCanvasMat_B, SizeOfTCanvasMat);
  GetMem(DestCanvasMat_R, SizeOfTCanvasMat);
  GetMem(DestCanvasMat_G, SizeOfTCanvasMat);
  GetMem(DestCanvasMat_B, SizeOfTCanvasMat);
  try
    CanvasToMat(ASrcBitmap, SrcCanvasMat_R, SrcCanvasMat_G, SrcCanvasMat_B);
    CanvasToMat(ADestBitmap, DestCanvasMat_R, DestCanvasMat_G, DestCanvasMat_B);

    AdjustImgArea(XOffset, YOffset, Width, Height, ASrcBitmap);

    for y := 0 to Height - 1 do
    begin
      Src_y_shl_11 := (y + YOffset) shl 11;      //shl 11 means that this algorithm expects the max matrix size to be 2048 * 2048

      for x := 0 to Width - 1 do
      begin
        SrcIndex := Src_y_shl_11 + x + XOffset;

        DestCanvasMat_R[SrcIndex] := Word(SrcCanvasMat_R[SrcIndex] + (AColor      ) and $FF) shr 1;
        DestCanvasMat_G[SrcIndex] := Word(SrcCanvasMat_G[SrcIndex] + (AColor shr 8) and $FF) shr 1;
        DestCanvasMat_B[SrcIndex] := Word(SrcCanvasMat_B[SrcIndex] + (AColor shr 16) and $FF) shr 1;
      end;
    end;

    MatToCanvas(DestCanvasMat_R, DestCanvasMat_G, DestCanvasMat_B, ADestBitmap);
  finally
    FreeMem(SrcCanvasMat_R, SizeOfTCanvasMat);
    FreeMem(SrcCanvasMat_G, SizeOfTCanvasMat);
    FreeMem(SrcCanvasMat_B, SizeOfTCanvasMat);
    FreeMem(DestCanvasMat_R, SizeOfTCanvasMat);
    FreeMem(DestCanvasMat_G, SizeOfTCanvasMat);
    FreeMem(DestCanvasMat_B, SizeOfTCanvasMat);
  end;
end;


//Same function as above, but this one mixes two bitmaps  (not much benefit in refactoring)
//This function expects that ADestBitmap is already loaded with a bitmap (likely the same one as ASrcABitmap and ASrcBBitmap) and it overwrites an area, specified by XOffset, YOffset, Width, Height.
procedure AvgBitmapWithBitmap(ASrcABitmap, ASrcBBitmap, ADestBitmap: TBitmap; XOffset: Integer = -1; YOffset: Integer = -1; Width: Integer = -1; Height: Integer = -1);
var   // 9 * SizeOf(TCanvasMat) =  9 * 8388608 bytes = 72MB
  SrcACanvasMat_R, SrcBCanvasMat_R: PCanvasMat;
  SrcACanvasMat_G, SrcBCanvasMat_G: PCanvasMat;
  SrcACanvasMat_B, SrcBCanvasMat_B: PCanvasMat;
  DestCanvasMat_R: PCanvasMat;
  DestCanvasMat_G: PCanvasMat;
  DestCanvasMat_B: PCanvasMat;
  x, y: Integer;
  SrcIndex, Src_y_shl_11: Integer;
  SizeOfTCanvasMat: Integer;
begin
  if (ASrcABitmap.Width = 0) or (ASrcABitmap.Height = 0) then
    Exit;

  if (ASrcBBitmap.Width = 0) or (ASrcBBitmap.Height = 0) then
    Exit;

  SizeOfTCanvasMat := SizeOf(TCanvasMat);

  GetMem(SrcACanvasMat_R, SizeOfTCanvasMat);
  GetMem(SrcACanvasMat_G, SizeOfTCanvasMat);
  GetMem(SrcACanvasMat_B, SizeOfTCanvasMat);
  GetMem(SrcBCanvasMat_R, SizeOfTCanvasMat);
  GetMem(SrcBCanvasMat_G, SizeOfTCanvasMat);
  GetMem(SrcBCanvasMat_B, SizeOfTCanvasMat);
  GetMem(DestCanvasMat_R, SizeOfTCanvasMat);
  GetMem(DestCanvasMat_G, SizeOfTCanvasMat);
  GetMem(DestCanvasMat_B, SizeOfTCanvasMat);
  try
    CanvasToMat(ASrcABitmap, SrcACanvasMat_R, SrcACanvasMat_G, SrcACanvasMat_B);
    CanvasToMat(ASrcBBitmap, SrcBCanvasMat_R, SrcBCanvasMat_G, SrcBCanvasMat_B);
    CanvasToMat(ADestBitmap, DestCanvasMat_R, DestCanvasMat_G, DestCanvasMat_B);

    AdjustImgArea(XOffset, YOffset, Width, Height, ASrcABitmap);

    for y := 0 to Height - 1 do
    begin
      Src_y_shl_11 := (y + YOffset) shl 11;      //shl 11 means that this algorithm expects the max matrix size to be 2048 * 2048

      for x := 0 to Width - 1 do
      begin
        SrcIndex := Src_y_shl_11 + x + XOffset;

        DestCanvasMat_R[SrcIndex] := Word(SrcACanvasMat_R[SrcIndex] + SrcBCanvasMat_R[SrcIndex]) shr 1;
        DestCanvasMat_G[SrcIndex] := Word(SrcACanvasMat_G[SrcIndex] + SrcBCanvasMat_G[SrcIndex]) shr 1;
        DestCanvasMat_B[SrcIndex] := Word(SrcACanvasMat_B[SrcIndex] + SrcBCanvasMat_B[SrcIndex]) shr 1;
      end;
    end;

    MatToCanvas(DestCanvasMat_R, DestCanvasMat_G, DestCanvasMat_B, ADestBitmap);
  finally
    FreeMem(SrcACanvasMat_R, SizeOfTCanvasMat);
    FreeMem(SrcACanvasMat_G, SizeOfTCanvasMat);
    FreeMem(SrcACanvasMat_B, SizeOfTCanvasMat);
    FreeMem(SrcBCanvasMat_R, SizeOfTCanvasMat);
    FreeMem(SrcBCanvasMat_G, SizeOfTCanvasMat);
    FreeMem(SrcBCanvasMat_B, SizeOfTCanvasMat);
    FreeMem(DestCanvasMat_R, SizeOfTCanvasMat);
    FreeMem(DestCanvasMat_G, SizeOfTCanvasMat);
    FreeMem(DestCanvasMat_B, SizeOfTCanvasMat);
  end;
end;


function BitmapsAreEqual(ASrcABitmap, ASrcBBitmap: TBitmap; AWidth, AHeight: Integer): Boolean;
var
  SrcACanvasMat_R, SrcBCanvasMat_R: PCanvasMat;
  SrcACanvasMat_G, SrcBCanvasMat_G: PCanvasMat;
  SrcACanvasMat_B, SrcBCanvasMat_B: PCanvasMat;
  SizeOfTCanvasMat: Integer;
  x, y: Integer;
  SrcIndex, Src_y_shl_11: Integer;
begin
  SizeOfTCanvasMat := SizeOf(TCanvasMat);

  GetMem(SrcACanvasMat_R, SizeOfTCanvasMat);
  GetMem(SrcACanvasMat_G, SizeOfTCanvasMat);
  GetMem(SrcACanvasMat_B, SizeOfTCanvasMat);
  GetMem(SrcBCanvasMat_R, SizeOfTCanvasMat);
  GetMem(SrcBCanvasMat_G, SizeOfTCanvasMat);
  GetMem(SrcBCanvasMat_B, SizeOfTCanvasMat);
  try
    CanvasToMat(ASrcABitmap, SrcACanvasMat_R, SrcACanvasMat_G, SrcACanvasMat_B);
    CanvasToMat(ASrcBBitmap, SrcBCanvasMat_R, SrcBCanvasMat_G, SrcBCanvasMat_B);

    Result := True;
    for y := 0 to AHeight - 1 do
    begin
      Src_y_shl_11 := y shl 11;      //shl 11 means that this algorithm expects the max matrix size to be 2048 * 2048

      for x := 0 to AWidth - 1 do
      begin
        SrcIndex := Src_y_shl_11 + x;

        if (SrcACanvasMat_R[SrcIndex] <> SrcBCanvasMat_R[SrcIndex]) or
           (SrcACanvasMat_G[SrcIndex] <> SrcBCanvasMat_G[SrcIndex]) or
           (SrcACanvasMat_B[SrcIndex] <> SrcBCanvasMat_B[SrcIndex]) then
        begin
          Result := False;
          Exit;
        end;
      end;

    end;
  finally
    FreeMem(SrcACanvasMat_R, SizeOfTCanvasMat);
    FreeMem(SrcACanvasMat_G, SizeOfTCanvasMat);
    FreeMem(SrcACanvasMat_B, SizeOfTCanvasMat);
    FreeMem(SrcBCanvasMat_R, SizeOfTCanvasMat);
    FreeMem(SrcBCanvasMat_G, SizeOfTCanvasMat);
    FreeMem(SrcBCanvasMat_B, SizeOfTCanvasMat);
  end;
end;


procedure MakeImageContentTransparent(AImg: TImage);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    AImg.Transparent := True;
    AImg.Picture.Bitmap.SaveToStream(Stream);
    Stream.Position := 0;
    AImg.Picture.Bitmap.LoadFromStream(Stream);  //by reloading the bitmap, the content becomes transparent (probably it requires the first vertical line to be of the transparency color)
  finally
    Stream.Free;
  end;
end;


procedure DrawSearchGrid(AImg: TImage; AlgorithmSettings: TMatchBitmapAlgorithmSettings; AGridWidth, AGridHeight: Integer; AGridColor: TColor; ADisplayGridLineOption: TDisplayGridLineOption);
var
  x, y: Integer;
begin
  AImg.Canvas.Pen.Color := AGridColor;

  case ADisplayGridLineOption of
    loDot:
      AImg.Canvas.Pen.Style := psDot;

    loSolid, loTransparentSolid:
      AImg.Canvas.Pen.Style := psSolid;
  end;

  if AlgorithmSettings.YMultipleOf < 1 then
    AlgorithmSettings.YMultipleOf := 1;

  if AlgorithmSettings.XMultipleOf < 1 then
    AlgorithmSettings.XMultipleOf := 1;

  for y := 0 to AGridHeight - 1 do   //starts at one, to avoid overwriting transparency pixels
    if y mod AlgorithmSettings.YMultipleOf = 0 then
      Line(AImg.Canvas, 1, y, AGridWidth - 1, y);

  for x := 0 to AGridWidth - 1 do   //starts at one, to avoid overwriting transparency pixels
    if x mod AlgorithmSettings.XMultipleOf = 0 then
      Line(AImg.Canvas, x, 1, x, AGridHeight - 1);
end;


end.
