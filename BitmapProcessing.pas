{
    Copyright (C) 2025 VCC
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

  TMatchByHistogramNumericSettings = record
    MinPercentColorMatch: Double;
    MostSignificantColorCountInSubBmp: Integer;
    MostSignificantColorCountInBackgroundBmp: Integer;
  end;
  

procedure Line(ACnv: TCanvas; x1, y1, x2, y2: Integer);
procedure ScreenShot(SrcHandle: THandle; DestBitmap: TBitmap; XOffsetSrc, YOffsetSrc, Width, Height: Integer);

function BitmapPosMatch(Algorithm: TMatchBitmapAlgorithm;
                        AlgorithmSettings: TMatchBitmapAlgorithmSettings;
                        AMatchByHistogramNumericSettings: TMatchByHistogramNumericSettings;
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
                        AThreadCount: Integer;
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
procedure FillInMissingHistogramPointsToBeCompared(var AHistA, AHistColorCountsA, AHistB, AHistColorCountsB: TIntArr);
function CompareHistograms(var AHistA, AHistColorCountsA, AHistB, AHistColorCountsB: TIntArr): Double;
procedure SortHistogramCmpArr(var ACmpArr: TDblArr);  //array of results, returned by CompareHistograms


implementation


uses
  Forms, Classes, IntegerList, DoubleList,
  ctypes, CLHeaders;


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


type
  THistCompFunc = function({const} a, b: Int64): Integer;  //TCompareFunc

function CompFuncByColorCount({const} a, b: Int64): Integer;
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


function CompFuncByColor({const} a, b: Int64): Integer;
begin
  a := a and $FFFFFFFF;  //discard color counts
  b := b and $FFFFFFFF;  //discard color counts

  if a > b then
    Result := -1
  else
    if a = b then
      Result := 0
    else
      Result := 1;
end;


procedure CustomSortHistogram(var AHist, AHistColorCounts: TIntArr; ACmpFunc: THistCompFunc);
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

    SortingArray.Sort(@ACmpFunc);

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


procedure SortHistogram(var AHist, AHistColorCounts: TIntArr);  //ByColorCounts
begin
  CustomSortHistogram(AHist, AHistColorCounts, CompFuncByColorCount);
end;


procedure SortHistogramByColor(var AHist, AHistColorCounts: TIntArr);  //used for comparing two histograms
begin
  CustomSortHistogram(AHist, AHistColorCounts, CompFuncByColor);
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
  if (AWidth = 0) or (AHeight = 0) then
  begin
    SetLength(AHist, 0);
    SetLength(AHistColorCounts, 0);
    Exit;
  end;

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
  if (AWidth = 0) or (AHeight = 0) then
  begin
    SetLength(AHist, 0);
    SetLength(AHistColorCounts, 0);
    Exit;
  end;

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


procedure InsertPointIntoTIntArr(var AArr: TIntArr; APoint, AIndex: Integer);
var
  i: Integer;
begin
  if (AIndex < 0) or (AIndex > Length(AArr)) then
    raise Exception.Create('Index out of bounds when inserting item into array.');

  SetLength(AArr, Length(AArr) + 1);
  for i := Length(AArr) - 1 downto AIndex + 1 do
    AArr[i] := AArr[i - 1];

  AArr[AIndex] := APoint;
end;


procedure FillInMissingHistogramPointsToBeCompared(var AHistA, AHistColorCountsA, AHistB, AHistColorCountsB: TIntArr);
var
  i, InsertIdx: Integer;
begin
  for i := Length(AHistB) - 1 downto 0 do
    if ColorIndexInIntArr(AHistB[i], AHistA) = -1 then  //item from histogram B does not exist in histogram A
    begin
      InsertIdx := Length(AHistA); //insert as the last item (for now)
      InsertPointIntoTIntArr(AHistA, AHistB[i], InsertIdx);
      InsertPointIntoTIntArr(AHistColorCountsA, AHistColorCountsB[i], InsertIdx);
    end;

  for i := Length(AHistA) - 1 downto 0 do
    if ColorIndexInIntArr(AHistA[i], AHistB) = -1 then  //item from histogram A does not exist in histogram B
    begin
      InsertIdx := Length(AHistB); //insert as the last item (for now)
      InsertPointIntoTIntArr(AHistB, AHistA[i], InsertIdx);
      InsertPointIntoTIntArr(AHistColorCountsB, AHistColorCountsA[i], InsertIdx);
    end;

  if (Length(AHistA) <> Length(AHistB)) or
     (Length(AHistA) <> Length(AHistColorCountsA)) or
     (Length(AHistB) <> Length(AHistColorCountsB)) then
     raise Exception.Create('Bug: array length mismatch when preparing to compare.');

  SortHistogramByColor(AHistA, AHistColorCountsA);
  SortHistogramByColor(AHistB, AHistColorCountsB);
end;


function CompareHistograms(var AHistA, AHistColorCountsA, AHistB, AHistColorCountsB: TIntArr): Double;
var
  i, DiffInt: Integer;
begin
  if (Length(AHistA) <> Length(AHistB)) or
     (Length(AHistA) <> Length(AHistColorCountsA)) or
     (Length(AHistB) <> Length(AHistColorCountsB)) then
    FillInMissingHistogramPointsToBeCompared(AHistA, AHistColorCountsA, AHistB, AHistColorCountsB);

  if Length(AHistA) = 0 then
  begin
    Result := 0;
    Exit;
  end;

  for i := 0 to Length(AHistA) - 1 do
    if AHistA[i] <> AHistB[i] then
      raise Exception.Create('Bug: The lists of colors, from the two histograms, are different.'); //both arrays should have the same content

  DiffInt := 0;
  for i := 0 to Length(AHistA) - 1 do
    Inc(DiffInt, Abs(AHistColorCountsA[i] - AHistColorCountsB[i]));

  Result := DiffInt / Length(AHistA);  //if dividing by Length(AHistA), it means that comparing results from bitmaps of different sizes, will (most of the times) lead to erroneous results
end;


procedure SortHistogramCmpArr(var ACmpArr: TDblArr);
var
  SortingArray: TDoubleList;
  i: Integer;
begin
  SortingArray := TDoubleList.Create;
  try
    SortingArray.Capacity := Length(ACmpArr);
    for i := 0 to Length(ACmpArr) - 1 do
      SortingArray.Add(ACmpArr[i]);

    SortingArray.Sort;

    for i := 0 to Length(ACmpArr) - 1 do
      ACmpArr[i] := SortingArray.Items[i];
  finally
    SortingArray.Free;
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


function FMax(a, b: Double): Double;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;


function FMin(a, b: Double): Double;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;


//ABkHist, ABkHistColorCounts define the background histogram
//ASearchedHist, ASearchedHistColorCounts define the searched bitmap histogram
//AMinPercentColorMatch sets how much of the ASearchedBmp histogram data should be found in ABkBmp histogram data. The value is expected to be from 0 to 1.
//AMostSignificantColorCount sets how many items from the histogram arrays should be verified.
//AColorErrorLevel is the difference between the two compared colors, for every color channel (R, G, B). It is possible that a scaled value is required, compared to the one entered by a user as a FindSubControl property.
function MatchAreaByHistogram(var ABkHist, ABkHistColorCounts, ASearchedHist, ASearchedHistColorCounts: TIntArr;   //The function assumes that the two histograms are sorted
                              AMinPercentColorMatch: Double;
                              AMostSignificantColorCountInSubBmp, AMostSignificantColorCountInBackgroundBmp: Integer): Boolean;
var
  i, j: Integer;
  CurrentColorPercent: Double;
  MatchPercents: array of Double;
  //BackgroundColor_R, BackgroundColor_G, BackgroundColor_B: SmallInt;
  //SearchedColor_R, SearchedColor_G, SearchedColor_B: SmallInt;
begin
  SetLength(MatchPercents, Min(AMostSignificantColorCountInSubBmp, Length(ASearchedHist)));  //Assumes that Length(ASearchedHist) < Length(ABkHist). Otherwise, nothing will be found, as expected.

  for i := 0 to Length(MatchPercents) - 1 do
    MatchPercents[i] := 0;

  for i := 0 to Min(AMostSignificantColorCountInSubBmp, Length(ASearchedHist)) - 1 do
    for j := 0 to Min(AMostSignificantColorCountInBackgroundBmp, Length(ABkHist)) - 1 do
    begin
      //SysRedGreenBlue(ABkHist[j], BackgroundColor_R, BackgroundColor_G, BackgroundColor_B);
      //SysRedGreenBlue(ASearchedHist[j], SearchedColor_R, SearchedColor_G, SearchedColor_B);
      //
      //if ColorMatches(BackgroundColor_R, BackgroundColor_G, BackgroundColor_B,
      //                SearchedColor_R, SearchedColor_G, SearchedColor_B,
      //                AColorErrorLevel) then
      if ABkHist[j] = ASearchedHist[j] then
      begin
        CurrentColorPercent := ASearchedHistColorCounts[i] / Max(ABkHistColorCounts[j], 1);
        if Double(MatchPercents[i]) < CurrentColorPercent then
          MatchPercents[i] := CurrentColorPercent;
      end;
    end;

  AMinPercentColorMatch := FMax(0, FMin(100, AMinPercentColorMatch));

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


type
  TPosMatchInterval = record
    FIMin, FIMax: Integer;
  end;

  TPosMatchIntervalArr = array of TPosMatchInterval;

  TBmpPosMatchTh = class(TThread)
  private
    FResult: Boolean;
    FDone: Boolean;
    FPosMatchInterval: TPosMatchInterval;
    FShouldStopThread: Boolean; //set from outside, when the thread is not needed anymore

    FXAmount: Integer;
    FStopSearchOnDemand: PBoolean;
    FOutsideTickCount: QWord;
    FPrecisionTimeout: QWord;
    FSrcMat, FSubMat: TRGBPCanvasMat;
    FSrcWidth, FSrcHeight: Integer;
    FSubWidth, FSubHeight: Integer;
    FPreSizeX, FPreSizeY: Integer;
    FColorErrorLevel: Integer;
    FPreErrorCount: Integer;
    FIgnoreBackgroundColor: Boolean;
    FGetAllBitmaps: Boolean;
    FTotalErrorCount: Integer;
    FBackgroundColor: TColor;
    FIgnoredColorsArr: TColorArr; //can be directly copied as a pointer
    FResultedErrorCount: Integer;
    FStopSearchOnMismatch: Boolean;
    FSubCnvXOffset, FSubCnvYOffset: Integer;
    FFoundBitmaps: TCompRecArr; //Should not be set from outside. The results should be collected from this array, from every thread.
  protected
    procedure Execute; override;
  end;

  TBmpPosMatchThArr = array of TBmpPosMatchTh;


procedure SplitIntoSearchIntervals(AAmount, AIntervalCount: Integer; var AIntervals: TPosMatchIntervalArr);
var
  i, IntervalSize: Integer;
begin
  if AIntervalCount < 0 then
    AIntervalCount := 1;

  if AAmount < 0 then
    AAmount := 1;

  SetLength(AIntervals, AIntervalCount);
  IntervalSize := AAmount div AIntervalCount;
  for i := 0 to AIntervalCount - 1 do
  begin
    AIntervals[i].FIMin := i * IntervalSize;
    AIntervals[i].FIMax := (i + 1) * IntervalSize;

    if i > 0 then
      Inc(AIntervals[i].FIMin);
  end;

  AIntervals[AIntervalCount - 1].FIMax := AAmount;
end;


procedure TBmpPosMatchTh.Execute;
var
  xx, yy: Integer;
  YMin, YMax: Integer;
begin
  FDone := False;
  FShouldStopThread := False;
  try
    try
      FResult := False;
      if FStopSearchOnDemand = nil then
      begin
        FResult := False;
        Exit;
      end;

      YMin := FPosMatchInterval.FIMin;
      YMax := FPosMatchInterval.FIMax;

      for yy := YMin to YMax do                //FResultedErrorCount makes sense only when the searched bitmap has the same size as the area in which it is searched for
        for xx := 0 to FXAmount do
        begin
          if FStopSearchOnDemand^ or FShouldStopThread then
            Exit;

          if (FOutsideTickCount > 0) and (GetTickCount64 - FOutsideTickCount > FPrecisionTimeout) then
            raise EBmpMatchTimeout.Create('PrecisionTimeout on searching for SubControl.'); //Exit;
                                                                                                                             //Avoid ignoring on "pre-search", because of false positives. Those would cause a longer search time.
          if CanvasPosMatch(FSrcMat, FSubMat, xx, yy, FSrcWidth, FSrcHeight, FPreSizeX, FPreSizeY, FColorErrorLevel, FPreErrorCount, FIgnoreBackgroundColor {False}, FBackgroundColor, FIgnoredColorsArr, FOutsideTickCount, FPrecisionTimeout, FResultedErrorCount, FStopSearchOnDemand, FStopSearchOnMismatch) then
          begin
            if CanvasPosMatch(FSrcMat, FSubMat, xx, yy, FSrcWidth, FSrcHeight, FSubWidth, FSubHeight, FColorErrorLevel, FTotalErrorCount, FIgnoreBackgroundColor, FBackgroundColor, FIgnoredColorsArr, FOutsideTickCount, FPrecisionTimeout, FResultedErrorCount, FStopSearchOnDemand, FStopSearchOnMismatch) then
            begin
              FResult := True;
              FSubCnvXOffset := xx;
              FSubCnvYOffset := yy;
              SetLength(FFoundBitmaps, Length(FFoundBitmaps) + 1);
              FFoundBitmaps[Length(FFoundBitmaps) - 1].XOffsetFromParent := xx;
              FFoundBitmaps[Length(FFoundBitmaps) - 1].YOffsetFromParent := yy;
              FFoundBitmaps[Length(FFoundBitmaps) - 1].ResultedErrorCount := FResultedErrorCount;

              if not FGetAllBitmaps then
                Exit;                      //stop only if a single bitmap result is expected
            end;

            ////verify again here, right before the RandomSleep call
            //if (AOutsideTickCount > 0) and (GetTickCount64 - AOutsideTickCount > APrecisionTimeout) then
            //  Exit;

            //RandomSleep(ASleepySearch);   //not used in thread, since the thread is intended to run as fast as possible
          end;
        end;
    except
    end;
  finally
    FDone := True;
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
                                   AThreadCount: Integer;
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
  Intervals: TPosMatchIntervalArr;
  i, j: Integer;
  ThArr: TBmpPosMatchThArr;
  AllThreadsDone: Boolean;
  FirstThIndexWithResult: Integer;
  ThreadsAreStillRunning: Boolean;
  tk: QWord;
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

    if AThreadCount > 0 then
    begin
      //just another ugly code, to create threads and split the searching across them
      SplitIntoSearchIntervals(YAmount, Min(AThreadCount, 255), Intervals);   //UIClicker is not that important to run on more than 255 cores.
      SetLength(ThArr, Length(Intervals));
      for i := 0 to Length(Intervals) - 1 do
      begin
        ThArr[i] := TBmpPosMatchTh.Create(True);
        ThArr[i].FreeOnTerminate := False;
        ThArr[i].FDone := False;
        ThArr[i].FPosMatchInterval := Intervals[i];

        ThArr[i].FXAmount := XAmount;
        ThArr[i].FStopSearchOnDemand := AStopSearchOnDemand;
        ThArr[i].FOutsideTickCount := AOutsideTickCount;
        ThArr[i].FPrecisionTimeout := APrecisionTimeout;
        ThArr[i].FSrcMat := SrcMat;
        ThArr[i].FSubMat := SubMat;
        ThArr[i].FSrcWidth := SrcWidth;
        ThArr[i].FSrcHeight := SrcHeight;
        ThArr[i].FSubWidth := SubWidth;
        ThArr[i].FSubHeight := SubHeight;
        ThArr[i].FPreSizeX := PreSizeX;
        ThArr[i].FPreSizeY := PreSizeY;
        ThArr[i].FColorErrorLevel := ColorErrorLevel;
        ThArr[i].FPreErrorCount := PreErrorCount;
        ThArr[i].FIgnoreBackgroundColor := AIgnoreBackgroundColor;
        ThArr[i].FGetAllBitmaps := AGetAllBitmaps;
        ThArr[i].FTotalErrorCount := TotalErrorCount;
        ThArr[i].FBackgroundColor := ABackgroundColor;
        ThArr[i].FIgnoredColorsArr := AIgnoredColorsArr; //can be directly copied as a pointer
        //ThArr[i].FResultedErrorCount: Integer;  //part of result
        ThArr[i].FStopSearchOnMismatch := StopSearchOnMismatch;
        //ThArr[i].FSubCnvXOffset, FSubCnvYOffset: Integer; //part of result
        //ThArr[i].FFoundBitmaps: TCompRecArr; //part of result
      end;

      for i := 0 to Length(Intervals) - 1 do
        ThArr[i].Start;

      FirstThIndexWithResult := -1;
      repeat
        AllThreadsDone := True;
        for i := 0 to Length(Intervals) - 1 do
          AllThreadsDone := AllThreadsDone and ThArr[i].FDone;

        if AGetAllBitmaps then
        begin //all theads must be done
          if AllThreadsDone then
            Break;
        end
        else
        begin //at least one thread must be done
          for i := 0 to Length(Intervals) - 1 do
            if ThArr[i].FDone and ThArr[i].FResult then
            begin
              FirstThIndexWithResult := i;
              Break; //this breaks the for loop
            end;

          if FirstThIndexWithResult > -1 then
            for i := 0 to Length(Intervals) - 1 do
              ThArr[i].FShouldStopThread := True;

          if AllThreadsDone then
            Break;
        end;

        Application.ProcessMessages;
        Sleep(1);
      until False;

      if AGetAllBitmaps then
      begin
        for i := 0 to Length(Intervals) - 1 do
          if ThArr[i].FResult then
          begin
            Result := True;

            for j := 0 to Length(ThArr[i].FFoundBitmaps) - 1 do
            begin
              SubCnvXOffset := ThArr[i].FSubCnvXOffset;
              SubCnvYOffset := ThArr[i].FSubCnvYOffset;
              SetLength(AFoundBitmaps, Length(AFoundBitmaps) + 1);
              AFoundBitmaps[Length(AFoundBitmaps) - 1].XOffsetFromParent := ThArr[i].FFoundBitmaps[j].XOffsetFromParent;
              AFoundBitmaps[Length(AFoundBitmaps) - 1].YOffsetFromParent := ThArr[i].FFoundBitmaps[j].YOffsetFromParent;
              AFoundBitmaps[Length(AFoundBitmaps) - 1].ResultedErrorCount := ThArr[i].FFoundBitmaps[j].ResultedErrorCount;
              AResultedErrorCount := ThArr[i].FFoundBitmaps[j].ResultedErrorCount;
            end;
          end;
      end
      else
      begin
        if FirstThIndexWithResult > -1 then  //at least one thread found the bitmap
        begin
          Result := True;
          SubCnvXOffset := ThArr[FirstThIndexWithResult].FSubCnvXOffset;
          SubCnvYOffset := ThArr[FirstThIndexWithResult].FSubCnvYOffset;

          if Length(ThArr[FirstThIndexWithResult].FFoundBitmaps) > 0 then //it should be > 0
          begin
            SetLength(AFoundBitmaps, Length(AFoundBitmaps) + 1);
            AFoundBitmaps[Length(AFoundBitmaps) - 1].XOffsetFromParent := ThArr[FirstThIndexWithResult].FFoundBitmaps[0].XOffsetFromParent;
            AFoundBitmaps[Length(AFoundBitmaps) - 1].YOffsetFromParent := ThArr[FirstThIndexWithResult].FFoundBitmaps[0].YOffsetFromParent;
            AFoundBitmaps[Length(AFoundBitmaps) - 1].ResultedErrorCount := ThArr[FirstThIndexWithResult].FFoundBitmaps[0].ResultedErrorCount;
            AResultedErrorCount := ThArr[FirstThIndexWithResult].FFoundBitmaps[0].ResultedErrorCount;
          end;
        end;
      end;

      tk := GetTickCount64;
      repeat
        ThreadsAreStillRunning := False;
        for i := 0 to Length(Intervals) - 1 do
          if Assigned(ThArr[i]) and ThArr[i].FDone then
          begin
            ThreadsAreStillRunning := True;
            FreeAndNil(ThArr[i]);
          end;

        if not ThreadsAreStillRunning then
          Break;

        Sleep(1);
      until GetTickCount64 - tk > 1000;
    end  //AThreadCount
    else
    begin
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
    end; //AThreadCount
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
                                                  AMinPercentColorMatch: Double;
                                                  AMostSignificantColorCountInSubBmp, AMostSignificantColorCountInBackgroundBmp: Integer;
                                                  out AResultedErrorCount: Integer;
                                                  AStopSearchOnDemand: PBoolean = nil;
                                                  StopSearchOnMismatch: Boolean = True): Boolean;
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

    AMinPercentColorMatch := AMinPercentColorMatch  / 100;

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
          Result := MatchAreaByHistogram(BkHist,
                                         BkHistColorCounts,
                                         SearchedHist,
                                         SearchedHistColorCounts,
                                         AMinPercentColorMatch,
                                         AMostSignificantColorCountInSubBmp,
                                         AMostSignificantColorCountInBackgroundBmp);

          if Result then
          begin
            MessageBox(0, PChar('Matched by histogram at x = ' + IntToStr(x) + '  y = ' + IntToStr(y)), 'Bmp proc', MB_ICONINFORMATION);
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


function GetKernelSrcRGB(ARGBSizeOnBG, ARGBSizeOnSub: Byte): string;
var
  RGBSizeStrOnBG, RGBSizeStrOnSub: string;
begin      //int is 32-bit, long is 64-bit
  if not (ARGBSizeOnBG in [3, 4]) or not (ARGBSizeOnSub in [3, 4]) then
  begin
    Result := 'Bad code';
    Exit;
  end;

  RGBSizeStrOnBG := IntToStr(ARGBSizeOnBG);
  RGBSizeStrOnSub := IntToStr(ARGBSizeOnSub);

  Result :=
    '__kernel void MatCmp(                      ' + #13#10 +
    '  __global uchar* ABackgroundBmp,          ' + #13#10 +
    '  __global uchar* ASubBmp,                 ' + #13#10 +
    '  __global int* AResultedErrCount,         ' + #13#10 +
    '  const unsigned int ABackgroundWidth,     ' + #13#10 +
    '  const unsigned int ASubBmpWidth,         ' + #13#10 +
    '  const unsigned int ASubBmpHeight,        ' + #13#10 +    //After setting MatCmp as a slave kernel: not needed in this kernel, it is here for compatibility only (to have a similar list of parameters, to be able to directly call this kernel from host, if needed)
    '  const unsigned int AXOffset,             ' + #13#10 +
    '  const unsigned int AYOffset,             ' + #13#10 +
    '  const uchar AColorError,                 ' + #13#10 +
    '  const long ASlaveQueue)                  ' + #13#10 +    //After setting MatCmp as a slave kernel: not needed in this kernel, it is here for compatibility only ...
    '{                                          ' + #13#10 +
    '  int YIdx = get_global_id(0);             ' + #13#10 + //goes from 0 to SubBmpHeight - 1
    '  __global uchar const * BGRow = &ABackgroundBmp[((YIdx + AYOffset) * ABackgroundWidth + AXOffset) * ' + RGBSizeStrOnBG + '];' + #13#10 + //pointer to the current row, indexed by YIdx
    '  __global uchar const * SubRow = &ASubBmp[(YIdx * ASubBmpWidth) * ' + RGBSizeStrOnSub + '];' + #13#10 + //pointer to the current row, indexed by YIdx
    '  int ErrCount = 0;                             ' + #13#10 +
    '  for (int x = 0; x < ASubBmpWidth; x++)        ' + #13#10 +
    '  {                                             ' + #13#10 +
    '     int x0_BG = x * ' + RGBSizeStrOnBG + ' + 0;                        ' + #13#10 +
    '     int x1_BG = x * ' + RGBSizeStrOnBG + ' + 1;                        ' + #13#10 +
    '     int x2_BG = x * ' + RGBSizeStrOnBG + ' + 2;                        ' + #13#10 +
    '     int x0_Sub = x * ' + RGBSizeStrOnSub + ' + 0;                        ' + #13#10 +
    '     int x1_Sub = x * ' + RGBSizeStrOnSub + ' + 1;                        ' + #13#10 +
    '     int x2_Sub = x * ' + RGBSizeStrOnSub + ' + 2;                        ' + #13#10 +
    '     short SubPxB = SubRow[x0_Sub];             ' + #13#10 +
    '     short BGPxB = BGRow[x0_BG];                ' + #13#10 +
    '     short SubPxG = SubRow[x1_Sub];             ' + #13#10 +
    '     short BGPxG = BGRow[x1_BG];                ' + #13#10 +
    '     short SubPxR = SubRow[x2_Sub];             ' + #13#10 +
    '     short BGPxR = BGRow[x2_BG];                ' + #13#10 +
    '     if ((abs(SubPxR - BGPxR) > AColorError) || ' + #13#10 +
    '         (abs(SubPxG - BGPxG) > AColorError) || ' + #13#10 +
    '         (abs(SubPxB - BGPxB) > AColorError))   ' + #13#10 +
    '     {                                          ' + #13#10 +
    '       ErrCount++;                              ' + #13#10 +
    '     }  //if                                    ' + #13#10 +
    '  }  //for                                      ' + #13#10 +
    '  AResultedErrCount[YIdx] = ErrCount;           ' + #13#10 +
    //'}                                               ' + #13#10 +
    //'                                           ' + #13#10 +
    //'__kernel void SlideSearch(                 ' + #13#10 +
    //'  __global uchar* ABackgroundBmp,          ' + #13#10 +
    //'  __global uchar* ASubBmp,                 ' + #13#10 +
    //'  __global int* AResultedErrCount,         ' + #13#10 +
    //'  const unsigned int ABackgroundWidth,     ' + #13#10 +
    //'  const unsigned int ASubBmpWidth,         ' + #13#10 +
    //'  const unsigned int ASubBmpHeight,        ' + #13#10 +
    //'  const unsigned int AXOffset,             ' + #13#10 +
    //'  const unsigned int AYOffset,             ' + #13#10 +
    //'  const uchar AColorError,                 ' + #13#10 +
    //'  const long ASlaveQueue)                  ' + #13#10 +
    //'{                                          ' + #13#10 +
    ////'  queue_t SlaveQueue = get_default_queue();' + #13#10 +     //requies OpenCL >= 2.0 and __opencl_c_device_enqueue    (so... it doesn't work)
    //'  queue_t SlaveQueue = (queue_t)ASlaveQueue;' + #13#10 +
    //'                                           ' + #13#10 +
    //'  ndrange_t ndrange = ndrange_1D(ASubBmpHeight);' + #13#10 +
    //'  kernel_enqueue_flags_t MyFlags;          ' + #13#10 +
    //'  MyFlags = CLK_ENQUEUE_FLAGS_NO_WAIT;     ' + #13#10 +
    //'  int i, j = 0;                            ' + #13#10 +
    //'  //for (i = 0; i < AYOffset; i++)       ' + #13#10 +
    //'    //for (j = 0; j < AXOffset; j++)     ' + #13#10 +
    //'      enqueue_kernel(SlaveQueue,           ' + #13#10 +      //using SlaveQueue, instead of get_default_queue()
    //'        MyFlags,                           ' + #13#10 +      //enqueue_kernel is commented, because using the default queue, messes up the object, so that the clFinish(SlaveCmdQueue) call returns an error.
    //'        ndrange,                           ' + #13#10 +
    //'        ^{MatCmp(ABackgroundBmp, ASubBmp, AResultedErrCount, ABackgroundWidth, ASubBmpWidth, ASubBmpHeight, i, j, AColorError, ASlaveQueue);});                  ' + #13#10 +
    //'  //ToDo: collect the results from all slave kernels. ' + #13#10
    '}                                          ' + #13#10
    ;
end;


function BitmapPosMatch_BruteForceOnGPU(ASrcBmpData, ASubBmpData: Pointer;
                                        ABytesPerPixelOnSrc, ABytesPerPixelOnSub: Integer;
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
                                        AThreadCount: Integer;
                                        out AResultedErrorCount: Integer;
                                        AStopSearchOnDemand: PBoolean = nil;
                                        StopSearchOnMismatch: Boolean = True): Boolean;

  procedure LogCallResult(AError: Integer; AFuncName, AInfo: string);
  begin
    //call some event
    if AError <> 0 then
      raise Exception.Create('Error ' + CLErrorToStr(AError) + ' instead of "' + AInfo + '" at "' + AFuncName + '" OpenCL API call.');
  end;

var
  Error, SecondError: Integer;
  DiffCntPerRow: array of LongInt;
  DifferentCount: LongInt;
  ShouldStop: Boolean;

  KernelSrc: string;

  GlobalSize, GlobalSizeWithDeviceEnqueue: csize_t;
  LocalSize: csize_t;
  DeviceID: cl_device_id;
  Context: cl_context;
  CmdQueue, SlaveCmdQueue: cl_command_queue;
  CLProgram: cl_program;
  CLKernel: cl_kernel;

  i, j, k: Integer;
  BackgroundBmpWidth, BackgroundBmpHeight: Integer;
  SubBmpWidth, SubBmpHeight: Integer;
  XOffset, YOffset: Integer;
  ColorError: Byte;

  BackgroundBufferRef: cl_mem;
  SubBufferRef: cl_mem;
  ResBufferRef: cl_mem;

  DevType: cl_device_type; //GPU
  PlatformIDs: Pcl_platform_id;
  PlatformCount: cl_uint;
  Info: string;
  InfoLen: csize_t;
  QueueProperties: array[0..8] of cl_command_queue_properties;
  OpenCLDll: TOpenCL;
begin
  //ToDo: - Implement another kernel code, which calls MatCmp with the two for loops (XOffset, YOffset). - Requires OpenCL version > 2.0.
  //ToDo: - Implement FastSearch property, which verifies a small rectangle (Top-Left), before going full bmp.
  //ToDo: - Implement ignored colors, using AIgnoredColorsArr.
  //ToDo: - Move the whole code to another (CPU) thread, to avoid blocking the UI.

  Result := False;

  BackgroundBmpWidth := ASourceBitmapWidth;
  BackgroundBmpHeight := ASourceBitmapHeight;
  SubBmpWidth := ASubBitmapWidth;
  SubBmpHeight := ASubBitmapHeight;

  KernelSrc := GetKernelSrcRGB(ABytesPerPixelOnSrc, ABytesPerPixelOnSub);
  OpenCLDll := TOpenCL.Create;
  try
    if not OpenCLDll.Loaded then
      raise Exception.Create('OpenCL not available. The dll is expected to exist at ' + OpenCLDll.ExpectedDllLocation);

    Error := OpenCLDll.clGetPlatformIDs(0, nil, @PlatformCount);
    LogCallResult(Error, 'clGetPlatformIDs', 'PlatformCount: ' + IntToStr(PlatformCount));

    GetMem(PlatformIDs, PlatformCount * SizeOf(cl_platform_id));
    try
      Error := OpenCLDll.clGetPlatformIDs(PlatformCount, PlatformIDs, nil);
      LogCallResult(Error, 'clGetPlatformIDs', '');

      DevType := CL_DEVICE_TYPE_GPU;
      DeviceID := nil;
      Error := OpenCLDll.clGetDeviceIDs(PlatformIDs^, DevType, 1, @DeviceID, nil);   //PlatformIDs[0]

      LogCallResult(Error, 'clGetDeviceIDs', '');

      Context := OpenCLDll.clCreateContext(nil, 1, @DeviceID, nil, nil, Error);
      try
        if Context = nil then
          LogCallResult(Error, 'clCreateContext', '');

        CmdQueue := OpenCLDll.clCreateCommandQueue(Context, DeviceID, 0, Error);
        if CmdQueue = nil then
          LogCallResult(Error, 'clCreateCommandQueue', '');

        //QueueProperties[0] := CL_QUEUE_PROPERTIES;
        //QueueProperties[1] := CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE;
        //QueueProperties[2] := 0;
        //
        //try
        //  CmdQueue := OpenCLDll.clCreateCommandQueueWithProperties(Context, DeviceID, @QueueProperties, Error);
        //  if (CmdQueue = nil) or (Error <> 0) then
        //    LogCallResult(Error, 'clCreateCommandQueueWithProperties CmdQueue', '');
        //except
        //  on E: Exception do
        //    LogCallResult(Error, 'clCreateCommandQueueWithProperties CmdQueue', '', 'Ex: ' + E.Message);
        //end;
        //
        //QueueProperties[0] := CL_QUEUE_PROPERTIES;
        //QueueProperties[1] := CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE or CL_QUEUE_ON_DEVICE or CL_QUEUE_ON_DEVICE_DEFAULT;
        //QueueProperties[2] := 0;
        //
        //try
        //  SlaveCmdQueue := OpenCLDll.clCreateCommandQueueWithProperties(Context, DeviceID, @QueueProperties, Error);  //also tested by creating this queue before the other one.  get_default_queue  still returns 0.
        //  if (SlaveCmdQueue = nil) or (Error <> 0) then
        //    LogCallResult(Error, 'clCreateCommandQueueWithProperties SlaveCmdQueue', '');
        //except
        //  on E: Exception do
        //    LogCallResult(Error, 'clCreateCommandQueueWithProperties SlaveCmdQueue', '', 'Ex: ' + E.Message);
        //end;

        CLProgram := OpenCLDll.clCreateProgramWithSource(Context, 1, PPAnsiChar(@KernelSrc), nil, Error);
        if CLProgram = nil then
          LogCallResult(Error, 'clCreateProgramWithSource', '');

        Error := OpenCLDll.clBuildProgram(CLProgram, 0, nil, nil, nil, nil);
        //LogCallResult(Error, 'clBuildProgram', 'Kernel code compiled.'); //commented, to allow the next call to clGetProgramBuildInfo

        if Error < CL_SUCCESS then
        begin
          SetLength(Info, 32768);
          SecondError := OpenCLDll.clGetProgramBuildInfo(CLProgram, DeviceID, CL_PROGRAM_BUILD_LOG, Length(Info), @Info[1], InfoLen);
          SetLength(Info, InfoLen);
          LogCallResult(SecondError, 'clGetProgramBuildInfo', 'Additional build info.');

          Info := StringReplace(Info, #13#10, '|', [rfReplaceAll]);
          Info := StringReplace(Info, #10, '|', [rfReplaceAll]);
          LogCallResult(Error, 'clBuildProgram', 'Kernel code compiled. ' + Info);
        end;

        CLKernel := OpenCLDll.clCreateKernel(CLProgram, 'MatCmp', Error);
        try
          LogCallResult(Error, 'clCreateKernel', 'Kernel allocated.');

          Error := OpenCLDll.clGetKernelWorkGroupInfo(CLKernel, DeviceID, CL_KERNEL_WORK_GROUP_SIZE, SizeOf(LocalSize), @LocalSize, InfoLen);
          LogCallResult(Error, 'clGetKernelWorkGroupInfo', 'Work group info obtained.');

          BackgroundBufferRef := OpenCLDll.clCreateBuffer(Context, CL_MEM_READ_ONLY, csize_t(ABytesPerPixelOnSrc * BackgroundBmpWidth * BackgroundBmpHeight), nil, Error);
          try
            LogCallResult(Error, 'clCreateBuffer', 'Background buffer created.');

            SubBufferRef := OpenCLDll.clCreateBuffer(Context, CL_MEM_READ_ONLY, csize_t(ABytesPerPixelOnSub * SubBmpWidth * SubBmpHeight), nil, Error);
            try
              LogCallResult(Error, 'clCreateBuffer', 'Sub buffer created.');

              ResBufferRef := OpenCLDll.clCreateBuffer(Context, CL_MEM_WRITE_ONLY, csize_t(SizeOf(LongInt) * SubBmpHeight), nil, Error);
              try
                LogCallResult(Error, 'clCreateBuffer', 'Res buffer created.');

                Error := OpenCLDll.clEnqueueWriteBuffer(CmdQueue, BackgroundBufferRef, CL_TRUE, 0, csize_t(ABytesPerPixelOnSrc * BackgroundBmpWidth * BackgroundBmpHeight), ASrcBmpData, 0, nil, nil);
                LogCallResult(Error, 'clEnqueueWriteBuffer', 'Background buffer written.');

                Error := OpenCLDll.clEnqueueWriteBuffer(CmdQueue, SubBufferRef, CL_TRUE, 0, csize_t(ABytesPerPixelOnSub * SubBmpWidth * SubBmpHeight), ASubBmpData, 0, nil, nil);
                LogCallResult(Error, 'clEnqueueWriteBuffer', 'Sub buffer written.');

                XOffset := 0;
                YOffset := 0;
                ColorError := ColorErrorLevel;

                Error := OpenCLDll.clSetKernelArg(CLKernel, 0, SizeOf(cl_mem), @BackgroundBufferRef); //sizeof(cl_mem)  is SizeOf(Pointer), which can be 4 or 8
                LogCallResult(Error, 'clSetKernelArg', 'BackgroundBufferRef argument set.');

                Error := OpenCLDll.clSetKernelArg(CLKernel, 1, SizeOf(cl_mem), @SubBufferRef); //sizeof(cl_mem)  is SizeOf(Pointer), which can be 4 or 8
                LogCallResult(Error, 'clSetKernelArg', 'SubBufferRef argument set.');

                Error := OpenCLDll.clSetKernelArg(CLKernel, 2, SizeOf(cl_mem), @ResBufferRef); //sizeof(cl_mem)  is SizeOf(Pointer), which can be 4 or 8
                LogCallResult(Error, 'clSetKernelArg', 'ResBufferRef argument set.');

                Error := OpenCLDll.clSetKernelArg(CLKernel, 3, SizeOf(LongInt), @BackgroundBmpWidth);
                LogCallResult(Error, 'clSetKernelArg', 'ABackgroundWidth argument set.');

                Error := OpenCLDll.clSetKernelArg(CLKernel, 4, SizeOf(LongInt), @SubBmpWidth);
                LogCallResult(Error, 'clSetKernelArg', 'ASubBmpWidth argument set.');

                Error := OpenCLDll.clSetKernelArg(CLKernel, 5, SizeOf(LongInt), @SubBmpHeight);
                LogCallResult(Error, 'clSetKernelArg', 'SubBmpHeight argument set.');

                //Error := OpenCLDll.clSetKernelArg(CLKernel, 6, SizeOf(LongInt), @XOffset);
                //LogCallResult(Error, 'clSetKernelArg', 'XOffset argument set.');
                //
                //Error := OpenCLDll.clSetKernelArg(CLKernel, 7, SizeOf(LongInt), @YOffset);
                //LogCallResult(Error, 'clSetKernelArg', 'YOffset argument set.');

                //XOffset := BackgroundBmpWidth - SubBmpWidth - 1;                 //this is the max value of XOffset - will be used for slave kernel
                //Error := OpenCLDll.clSetKernelArg(CLKernel, 6, SizeOf(LongInt), @XOffset);
                //LogCallResult(Error, 'clSetKernelArg', 'XOffset argument set.');
                //
                //YOffset := BackgroundBmpHeight - SubBmpHeight - 1;               //this is the max value of YOffset - will be used for slave kernel
                //Error := OpenCLDll.clSetKernelArg(CLKernel, 7, SizeOf(LongInt), @YOffset);
                //LogCallResult(Error, 'clSetKernelArg', 'YOffset argument set.');

                Error := OpenCLDll.clSetKernelArg(CLKernel, 8, SizeOf(Byte), @ColorError);
                LogCallResult(Error, 'clSetKernelArg', 'ColorError argument set.');

                Error := OpenCLDll.clSetKernelArg(CLKernel, 9, SizeOf(cl_ulong), SlaveCmdQueue);  //using SizeOf(cl_ulong), because the parameter is a QWord on kernel
                LogCallResult(Error, 'clSetKernelArg', 'SlaveCmdQueue argument set.');

                GlobalSize := SubBmpHeight;
                LogCallResult(Error, 'Matrix comparison', 'Starting...');

                //GlobalSizeWithDeviceEnqueue := 1; //one master kernel, although not sure if Local should be 1  //the master kernel has only one intance
                //Error := OpenCLDll.clEnqueueNDRangeKernel(CmdQueue, CLKernel, 1, nil, @GlobalSizeWithDeviceEnqueue, nil, 0, nil, nil);
                //LogCallResult(Error, 'clEnqueueNDRangeKernel CmdQueue', '');

                ShouldStop := False;
                SetLength(DiffCntPerRow, GlobalSize);
                for i := 0 to BackgroundBmpHeight - SubBmpHeight - 1 do       //these two for loops are implemented in SlideSearch kernel
                begin
                  for j := 0 to BackgroundBmpWidth - SubBmpWidth - 1 do
                  begin
                    XOffset := j;
                    YOffset := i;

                    Error := OpenCLDll.clSetKernelArg(CLKernel, 6, SizeOf(LongInt), @XOffset);
                    LogCallResult(Error, 'clSetKernelArg', '');

                    Error := OpenCLDll.clSetKernelArg(CLKernel, 7, SizeOf(LongInt), @YOffset);
                    LogCallResult(Error, 'clSetKernelArg', '');

                    Error := OpenCLDll.clEnqueueNDRangeKernel(CmdQueue, CLKernel, 1, nil, @GlobalSize, nil, 0, nil, nil);
                    LogCallResult(Error, 'clEnqueueNDRangeKernel', '');

                    Error := OpenCLDll.clFinish(CmdQueue);
                    LogCallResult(Error, 'clFinish', '');

                    Error := OpenCLDll.clEnqueueReadBuffer(CmdQueue, ResBufferRef, CL_TRUE, 0, csize_t(SizeOf(LongInt) * GlobalSize), @DiffCntPerRow[0], 0, nil, nil);
                    LogCallResult(Error, 'clEnqueueReadBuffer', '');

                    DifferentCount := 0;
                    for k := 0 to GlobalSize - 1 do //results len
                      Inc(DifferentCount, DiffCntPerRow[k]);

                    if DifferentCount < TotalErrorCount then
                    begin
                      Result := True;
                      AResultedErrorCount := DifferentCount;
                      SubCnvXOffset := XOffset;
                      SubCnvYOffset := YOffset;
                      SetLength(AFoundBitmaps, Length(AFoundBitmaps) + 1);
                      AFoundBitmaps[Length(AFoundBitmaps) - 1].XOffsetFromParent := XOffset;
                      AFoundBitmaps[Length(AFoundBitmaps) - 1].YOffsetFromParent := YOffset;
                      AFoundBitmaps[Length(AFoundBitmaps) - 1].ResultedErrorCount := AResultedErrorCount;

                      if not AGetAllBitmaps then
                        ShouldStop := True;  //stop only if a single bitmap result is expected

                      if ShouldStop then
                        Break;
                    end;

                    if ShouldStop then
                      Break;

                    if (AStopSearchOnDemand <> nil) and AStopSearchOnDemand^ then
                    begin
                      ShouldStop := True;
                      Break;
                    end;

                    if (AOutsideTickCount > 0) and (GetTickCount64 - AOutsideTickCount > APrecisionTimeout) then
                      raise EBmpMatchTimeout.Create('PrecisionTimeout on searching for SubControl.'); //Exit;
                  end; //for j

                  if DifferentCount < TotalErrorCount then
                    Break;

                  if (AOutsideTickCount > 0) and (GetTickCount64 - AOutsideTickCount > APrecisionTimeout) then
                    Break;

                  RandomSleep(ASleepySearch);
                  Application.ProcessMessages;
                end; //for i
              finally
                OpenCLDll.clReleaseMemObject(ResBufferRef);
              end;
            finally
              OpenCLDll.clReleaseMemObject(SubBufferRef);
            end;
          finally
            OpenCLDll.clReleaseMemObject(BackgroundBufferRef);
          end;
        finally  //clCreateKernel
          OpenCLDll.clReleaseKernel(CLKernel);
        end;

        OpenCLDll.clReleaseProgram(CLProgram);
        OpenCLDll.clReleaseCommandQueue(CmdQueue);
        //OpenCLDll.clReleaseCommandQueue(SlaveCmdQueue);   //used for slave kernels
      finally
        OpenCLDll.clReleaseContext(Context);
      end;
    finally
      Freemem(PlatformIDs, PlatformCount * SizeOf(cl_platform_id));
    end;
  finally
    OpenCLDll.Free;
  end;
end;


function BitmapPosMatch(Algorithm: TMatchBitmapAlgorithm;
                        AlgorithmSettings: TMatchBitmapAlgorithmSettings;
                        AMatchByHistogramNumericSettings: TMatchByHistogramNumericSettings;
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
                        AThreadCount: Integer;
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
  SourceStream, SubStream: TMemoryStream;
  i: Integer;
  BytesPerPixelSrc, BytesPerPixelSub: Integer;
  ScLn: Pointer;
begin
  {   //debug code
  if not FileExists(CDebugSubBmpPath) then
    SubBitmap.SaveToFile(CDebugSubBmpPath);
  }

  if Algorithm = mbaBruteForceOnGPU then
  begin
    SourceStream := TMemoryStream.Create;
    SubStream := TMemoryStream.Create;
    try
      if SourceBitmap.PixelFormat = pf24bit then
        BytesPerPixelSrc := 3
      else
        BytesPerPixelSrc := 4;

      if SubBitmap.PixelFormat = pf24bit then
        BytesPerPixelSub := 3
      else
        BytesPerPixelSub := 4;

      for i := 0 to SourceBitmap.Height - 1 do
      begin
        ScLn := SourceBitmap.{%H-}ScanLine[i];
        SourceStream.Write(ScLn^, SourceBitmap.Width * BytesPerPixelSrc);
      end;

      for i := 0 to SubBitmap.Height - 1 do
      begin
        ScLn := SubBitmap.{%H-}ScanLine[i];
        SubStream.Write(ScLn^, SubBitmap.Width * BytesPerPixelSub);
      end;

      Result := BitmapPosMatch_BruteForceOnGPU(SourceStream.Memory,
                                               SubStream.Memory,
                                               BytesPerPixelSrc,
                                               BytesPerPixelSub,
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
                                               AThreadCount,
                                               AResultedErrorCount,
                                               AStopSearchOnDemand,
                                               StopSearchOnMismatch);
    finally
      SourceStream.Free;
      SubStream.Free;
      Exit;
    end;
    Exit;
  end;

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
                                            AThreadCount,
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
                                                           AMatchByHistogramNumericSettings.MinPercentColorMatch,
                                                           AMatchByHistogramNumericSettings.MostSignificantColorCountInSubBmp,
                                                           AMatchByHistogramNumericSettings.MostSignificantColorCountInBackgroundBmp,
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
