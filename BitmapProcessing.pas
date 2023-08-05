{
    Copyright (C) 2022 VCC
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
  Windows, SysUtils, Graphics, ExtCtrls, ClickerUtils;

type                      //2048 is used as 2^11 = 2048, as an optimization. See CanvasToMat_24bit and CanvasToMat_32bit. The next higher resolution would be 4096 * 4096.
  TCanvasMat = array[0 .. 2048 * 2048] of SmallInt;//SmallInt may be better than Byte, because it is signed 16-bit
  PCanvasMat = ^TCanvasMat;

  TRGBPCanvasMat = record
    PR: PCanvasMat;
    PG: PCanvasMat;
    PB: PCanvasMat;
  end;

  TRGBRec = record   //24-bit pixels
    B, G, R: Byte;
  end;
  TScanLineArr = array[0..0] of TRGBRec;

  TRGBAlphaRec = record   //32-bit pixels
    B, G, R, A: Byte;
  end;
  TScanLineAlphaArr = array[0..0] of TRGBAlphaRec;
  

procedure Line(ACnv: TCanvas; x1, y1, x2, y2: Integer);
procedure ScreenShot(SrcHandle: THandle; DestBitmap: TBitmap; XOffsetSrc, YOffsetSrc, Width, Height: Integer);
function BitmapPosMatch(Algorithm: TMatchBitmapAlgorithm; AlgorithmSettings: TMatchBitmapAlgorithmSettings; SourceBitmap, SubBitmap: TBitmap; ColorErrorLevel: Integer; out SubCnvXOffset, SubCnvYOffset: Integer; TotalErrorCount: Integer; AStopSearchOnDemand: PBoolean = nil; StopSearchOnMismatch: Boolean = True): Boolean;
function AvgTwoTrueColors(Color1, Color2: TColor): TColor;
procedure AvgBitmapWithColor(ASrcBitmap, ADestBitmap: TBitmap; AColor: TColor; XOffset: Integer = -1; YOffset: Integer = -1; Width: Integer = -1; Height: Integer = -1); //if X, Y, W, H are specified, the function operates on that area only
procedure AvgBitmapWithBitmap(ASrcABitmap, ASrcBBitmap, ADestBitmap: TBitmap; XOffset: Integer = -1; YOffset: Integer = -1; Width: Integer = -1; Height: Integer = -1);
function BitmapsAreEqual(ASrcABitmap, ASrcBBitmap: TBitmap; AWidth, AHeight: Integer): Boolean;
procedure MakeImageContentTransparent(AImg: TImage);
procedure WipeBitmap(ABitmap: TBitmap; NewWidth, NewHeight: Integer);
procedure WipeImage(AImg: TImage; NewWidth, NewHeight: Integer);
procedure DrawSearchGrid(AImg: TImage; AlgorithmSettings: TMatchBitmapAlgorithmSettings; AGridWidth, AGridHeight: Integer; AGridColor: TColor; ADisplayGridLineOption: TDisplayGridLineOption);


implementation


uses
  Forms, Classes;


procedure RandomSleep;
begin
  if Random(8) = 7 then
  begin
    Application.ProcessMessages;
    Sleep(1);
  end;
end;


procedure Line(ACnv: TCanvas; x1, y1, x2, y2: Integer);
begin
  ACnv.MoveTo(x1, y1);
  ACnv.LineTo(x2, y2);
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
      ACanvasLine := ABitmap.ScanLine[i];
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
      ACanvasLine := ABitmap.ScanLine[i];
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
      ACanvasLine := ABitmap.ScanLine[i];
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
      ACanvasLine := ABitmap.ScanLine[i];
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


function CanvasPos(SrcMat, SubMat: TRGBPCanvasMat; SubCnvXOffset, SubCnvYOffset, SrcCnvWidth, SrcCnvHeight, SubCnvWidth, SubCnvHeight, ColorErrorLevel: Integer; AStopSearchOnDemand: PBoolean = nil; StopAtErrorCount: Integer = -1): Integer;
var
  x, y: Integer;
  ErrorCount: Integer;
  SrcIndex, SubIndex, Src_y_shl_11, Sub_y_shl_11: Integer;
  SourceCanvasMat_R, SubCanvasMat_R: PCanvasMat;
  SourceCanvasMat_G, SubCanvasMat_G: PCanvasMat;
  SourceCanvasMat_B, SubCanvasMat_B: PCanvasMat;
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

  for y := 0 to SubCnvHeight - 1 do
  begin
    Src_y_shl_11 := (y + SubCnvYOffset) shl 11;      //shl 11 means that this algorithm expects the max matrix size to be 2048 * 2048
    Sub_y_shl_11 := y shl 11;

    for x := 0 to SubCnvWidth - 1 do
    begin
      SrcIndex := Src_y_shl_11 + x + SubCnvXOffset;
      SubIndex := Sub_y_shl_11 + x;

      if (Abs(SourceCanvasMat_R[SrcIndex] - SubCanvasMat_R[SubIndex]) > ColorErrorLevel) or
         (Abs(SourceCanvasMat_G[SrcIndex] - SubCanvasMat_G[SubIndex]) > ColorErrorLevel) or
         (Abs(SourceCanvasMat_B[SrcIndex] - SubCanvasMat_B[SubIndex]) > ColorErrorLevel) then
      begin
        Inc(ErrorCount);
        Result := ErrorCount;

        if (StopAtErrorCount > -1) and (ErrorCount > StopAtErrorCount) then  //There may be "colorful" pixels in a black text, as a result of antialiasing. Too many such pixels mean that the compared text is different.
          Exit; //do not break - there are two 'for' loops
      end;

      if (AStopSearchOnDemand <> nil) and AStopSearchOnDemand^ then
        Exit;
    end; //for
  end; //for
end;


function CanvasPosMatch(SrcMat, SubMat: TRGBPCanvasMat; SubCnvXOffset, SubCnvYOffset, SrcCnvWidth, SrcCnvHeight, SubCnvWidth, SubCnvHeight, ColorErrorLevel, AcceptedErrorCount: Integer; AStopSearchOnDemand: PBoolean = nil; StopSearchOnMismatch: Boolean = True): Boolean;
begin
  if StopSearchOnMismatch then
    Result := CanvasPos(SrcMat, SubMat, SubCnvXOffset, SubCnvYOffset, SrcCnvWidth, SrcCnvHeight, SubCnvWidth, SubCnvHeight, ColorErrorLevel, AStopSearchOnDemand, AcceptedErrorCount) < AcceptedErrorCount + 1
  else
    Result := CanvasPos(SrcMat, SubMat, SubCnvXOffset, SubCnvYOffset, SrcCnvWidth, SrcCnvHeight, SubCnvWidth, SubCnvHeight, ColorErrorLevel, AStopSearchOnDemand) < AcceptedErrorCount + 1;
end;


function BitmapPosMatch_BruteForce(SrcMat, SubMat: TRGBPCanvasMat; SourceBitmap, SubBitmap: TBitmap; ColorErrorLevel: Integer; out SubCnvXOffset, SubCnvYOffset: Integer; TotalErrorCount: Integer; AStopSearchOnDemand: PBoolean = nil; StopSearchOnMismatch: Boolean = True): Boolean;
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

  for y := 0 to YAmount do
    for x := 0 to XAmount do
    begin
      if (AStopSearchOnDemand <> nil) and AStopSearchOnDemand^ then
        Exit;

      if CanvasPosMatch(SrcMat, SubMat, x, y, SrcWidth, SrcHeight, SubWidth, SubHeight, ColorErrorLevel, TotalErrorCount, AStopSearchOnDemand, StopSearchOnMismatch) then
      begin
        Result := True;
        SubCnvXOffset := x;
        SubCnvYOffset := y;
        Exit;
      end;

      RandomSleep;
    end;
end;


function BitmapPosMatch_SimpleGrid_XYMultipleAndOffsets(SrcMat, SubMat: TRGBPCanvasMat; AlgorithmSettings: TMatchBitmapAlgorithmSettings; SourceBitmap, SubBitmap: TBitmap; ColorErrorLevel: Integer; out SubCnvXOffset, SubCnvYOffset: Integer; TotalErrorCount: Integer; AStopSearchOnDemand: PBoolean = nil; StopSearchOnMismatch: Boolean = True): Boolean;
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

          if CanvasPosMatch(SrcMat, SubMat, x, y, SrcWidth, SrcHeight, SubWidth, SubHeight, ColorErrorLevel, TotalErrorCount, AStopSearchOnDemand, StopSearchOnMismatch) then
          begin
            Result := True;
            SubCnvXOffset := x;
            SubCnvYOffset := y;
            Exit;
          end;

          RandomSleep;
        end;
    end;
end;


function BitmapPosMatch(Algorithm: TMatchBitmapAlgorithm; AlgorithmSettings: TMatchBitmapAlgorithmSettings; SourceBitmap, SubBitmap: TBitmap; ColorErrorLevel: Integer; out SubCnvXOffset, SubCnvYOffset: Integer; TotalErrorCount: Integer; AStopSearchOnDemand: PBoolean = nil; StopSearchOnMismatch: Boolean = True): Boolean;
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
      mbaBruteForce: Result := BitmapPosMatch_BruteForce(SrcMat, SubMat, SourceBitmap, SubBitmap, ColorErrorLevel, SubCnvXOffset, SubCnvYOffset, TotalErrorCount, AStopSearchOnDemand, StopSearchOnMismatch);
      mbaXYMultipleAndOffsets: Result := BitmapPosMatch_SimpleGrid_XYMultipleAndOffsets(SrcMat, SubMat, AlgorithmSettings, SourceBitmap, SubBitmap, ColorErrorLevel, SubCnvXOffset, SubCnvYOffset, TotalErrorCount, AStopSearchOnDemand, StopSearchOnMismatch);
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
          Width,
          Height,
          DC,
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


procedure DrawWipeRect(ACanvas: TCanvas; NewWidth, NewHeight: Integer);
begin
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := clWhite;
  ACanvas.Pen.Color := clWhite;
  ACanvas.Rectangle(0, 0, NewWidth {- 1}, NewHeight {- 1});
end;


procedure WipeBitmap(ABitmap: TBitmap; NewWidth, NewHeight: Integer);
begin
  ABitmap.Clear;
  ABitmap.Width := NewWidth;
  ABitmap.Height := NewHeight;
  DrawWipeRect(ABitmap.Canvas, NewWidth, NewHeight);
end;


procedure WipeImage(AImg: TImage; NewWidth, NewHeight: Integer);
begin
  AImg.Picture.Clear;
  AImg.Width := NewWidth;
  AImg.Height := NewHeight;
  AImg.Picture.Bitmap.Width := AImg.Width;
  AImg.Picture.Bitmap.Height := AImg.Height;

  DrawWipeRect(AImg.Canvas, NewWidth, NewHeight);
  AImg.Repaint;
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
