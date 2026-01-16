{
    Copyright (C) 2026 VCC
    creation date: 16 Jan 2026
    initial release date: 16 Jan 2026

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


unit ClickerImageEffects;

{$mode ObjFPC}{$H+}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Graphics;


procedure Blur4px(ASrcBmp, ADestBmp: TBitmap);
procedure Blur8px(ASrcBmp, ADestBmp: TBitmap);


implementation


uses
  BitmapProcessing, BitmapConv;


procedure ShiftBmpToLeft(ASrcBmp, ADestBmp: TBitmap; Amount: Integer = 1);
begin
  WipeBitmap(ADestBmp, ASrcBmp.Width, ASrcBmp.Height);
  Bitblt(ADestBmp.Canvas.Handle,
         ASrcBmp.Width - Amount, //X   x-coord of destination upper-left corner
         0, //Y   y-coord of destination upper-left corner
         Amount,   //src and dest width
         ASrcBmp.Height,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         ASrcBmp.Width - Amount,     //offset for source
         0,     //offset for source
         SRCCOPY);

  Bitblt(ADestBmp.Canvas.Handle,
         0, //X   x-coord of destination upper-left corner
         0, //Y   y-coord of destination upper-left corner
         ASrcBmp.Width - Amount,   //src and dest width
         ASrcBmp.Height,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         Amount,     //offset for source
         0,     //offset for source
         SRCCOPY);
end;


procedure ShiftBmpToRight(ASrcBmp, ADestBmp: TBitmap; Amount: Integer = 1);
begin
  WipeBitmap(ADestBmp, ASrcBmp.Width, ASrcBmp.Height);
  Bitblt(ADestBmp.Canvas.Handle,
         0, //X   x-coord of destination upper-left corner
         0, //Y   y-coord of destination upper-left corner
         Amount,   //src and dest width
         ASrcBmp.Height,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         0,     //offset for source
         0,     //offset for source
         SRCCOPY);

  Bitblt(ADestBmp.Canvas.Handle,
         Amount, //X   x-coord of destination upper-left corner
         0, //Y   y-coord of destination upper-left corner
         ASrcBmp.Width - Amount,   //src and dest width
         ASrcBmp.Height,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         0,     //offset for source
         0,     //offset for source
         SRCCOPY);
end;


procedure ShiftBmpToTop(ASrcBmp, ADestBmp: TBitmap; Amount: Integer = 1);
begin
  WipeBitmap(ADestBmp, ASrcBmp.Width, ASrcBmp.Height);
  Bitblt(ADestBmp.Canvas.Handle,
         0, //X   x-coord of destination upper-left corner
         ASrcBmp.Height - Amount, //Y   y-coord of destination upper-left corner
         ASrcBmp.Width,   //src and dest width
         Amount,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         0,     //offset for source
         ASrcBmp.Height - Amount,     //offset for source
         SRCCOPY);

  Bitblt(ADestBmp.Canvas.Handle,
         0, //X   x-coord of destination upper-left corner
         0, //Y   y-coord of destination upper-left corner
         ASrcBmp.Width,   //src and dest width
         ASrcBmp.Height - Amount,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         0,     //offset for source
         Amount,     //offset for source
         SRCCOPY);
end;


procedure ShiftBmpToBottom(ASrcBmp, ADestBmp: TBitmap; Amount: Integer = 1);
begin
  WipeBitmap(ADestBmp, ASrcBmp.Width, ASrcBmp.Height);
  Bitblt(ADestBmp.Canvas.Handle,
         0, //X   x-coord of destination upper-left corner
         0, //Y   y-coord of destination upper-left corner
         ASrcBmp.Width,   //src and dest width
         Amount,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         0,     //offset for source
         0,     //offset for source
         SRCCOPY);

  Bitblt(ADestBmp.Canvas.Handle,
         0, //X   x-coord of destination upper-left corner
         Amount, //Y   y-coord of destination upper-left corner
         ASrcBmp.Width,   //src and dest width
         ASrcBmp.Height - Amount,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         0,     //offset for source
         0,     //offset for source
         SRCCOPY);
end;


procedure ShiftBmpToTopLeft(ASrcBmp, ADestBmp: TBitmap; Amount: Integer = 1);
begin
  WipeBitmap(ADestBmp, ASrcBmp.Width, ASrcBmp.Height);
  Bitblt(ADestBmp.Canvas.Handle,
         ASrcBmp.Width - Amount, //X   x-coord of destination upper-left corner
         0, //Y   y-coord of destination upper-left corner
         Amount,   //src and dest width
         ASrcBmp.Height,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         ASrcBmp.Width - Amount,     //offset for source
         0,     //offset for source
         SRCCOPY); //from Left

  Bitblt(ADestBmp.Canvas.Handle,
         0, //X   x-coord of destination upper-left corner
         ASrcBmp.Height - Amount, //Y   y-coord of destination upper-left corner
         ASrcBmp.Width,   //src and dest width
         Amount,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         0,     //offset for source
         ASrcBmp.Height - Amount,     //offset for source
         SRCCOPY); //from Top

  Bitblt(ADestBmp.Canvas.Handle,
         0, //X   x-coord of destination upper-left corner
         0, //Y   y-coord of destination upper-left corner
         ASrcBmp.Width - Amount,   //src and dest width
         ASrcBmp.Height - Amount,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         Amount,     //offset for source
         Amount,     //offset for source
         SRCCOPY);
end;


procedure ShiftBmpToTopRight(ASrcBmp, ADestBmp: TBitmap; Amount: Integer = 1);
begin
  WipeBitmap(ADestBmp, ASrcBmp.Width, ASrcBmp.Height);
  Bitblt(ADestBmp.Canvas.Handle,
         0, //X   x-coord of destination upper-left corner
         0, //Y   y-coord of destination upper-left corner
         Amount,   //src and dest width
         ASrcBmp.Height,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         0,     //offset for source
         0,     //offset for source
         SRCCOPY); //from Right

  Bitblt(ADestBmp.Canvas.Handle,
         0, //X   x-coord of destination upper-left corner
         ASrcBmp.Height - Amount, //Y   y-coord of destination upper-left corner
         ASrcBmp.Width,   //src and dest width
         Amount,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         0,     //offset for source
         ASrcBmp.Height - Amount,     //offset for source
         SRCCOPY); //from Top

  Bitblt(ADestBmp.Canvas.Handle,
         Amount, //X   x-coord of destination upper-left corner
         0, //Y   y-coord of destination upper-left corner
         ASrcBmp.Width - Amount,   //src and dest width
         ASrcBmp.Height - Amount,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         0,     //offset for source
         Amount,     //offset for source
         SRCCOPY);
end;


procedure ShiftBmpToBottomLeft(ASrcBmp, ADestBmp: TBitmap; Amount: Integer = 1);
begin
  WipeBitmap(ADestBmp, ASrcBmp.Width, ASrcBmp.Height);
  Bitblt(ADestBmp.Canvas.Handle,
         ASrcBmp.Width - Amount, //X   x-coord of destination upper-left corner
         0, //Y   y-coord of destination upper-left corner
         Amount,   //src and dest width
         ASrcBmp.Height,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         ASrcBmp.Width - Amount,     //offset for source
         0,     //offset for source
         SRCCOPY); //from Left

  Bitblt(ADestBmp.Canvas.Handle,
         0, //X   x-coord of destination upper-left corner
         0, //Y   y-coord of destination upper-left corner
         ASrcBmp.Width,   //src and dest width
         Amount,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         0,     //offset for source
         0,     //offset for source
         SRCCOPY);  //from Bottom

  Bitblt(ADestBmp.Canvas.Handle,
         0, //X   x-coord of destination upper-left corner
         Amount, //Y   y-coord of destination upper-left corner
         ASrcBmp.Width - Amount,   //src and dest width
         ASrcBmp.Height - Amount,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         Amount,     //offset for source
         0,     //offset for source
         SRCCOPY);
end;


procedure ShiftBmpToBottomRight(ASrcBmp, ADestBmp: TBitmap; Amount: Integer = 1);
begin
  WipeBitmap(ADestBmp, ASrcBmp.Width, ASrcBmp.Height);
  Bitblt(ADestBmp.Canvas.Handle,
         0, //X   x-coord of destination upper-left corner
         0, //Y   y-coord of destination upper-left corner
         Amount,   //src and dest width
         ASrcBmp.Height,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         0,     //offset for source
         0,     //offset for source
         SRCCOPY); //from Right

  Bitblt(ADestBmp.Canvas.Handle,
         0, //X   x-coord of destination upper-left corner
         0, //Y   y-coord of destination upper-left corner
         ASrcBmp.Width,   //src and dest width
         Amount,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         0,     //offset for source
         0,     //offset for source
         SRCCOPY);  //from Bottom

  Bitblt(ADestBmp.Canvas.Handle,
         Amount, //X   x-coord of destination upper-left corner
         Amount, //Y   y-coord of destination upper-left corner
         ASrcBmp.Width - Amount,   //src and dest width
         ASrcBmp.Height - Amount,  //src and dest height
         ASrcBmp.Canvas.Handle,      //src DC
         0,     //offset for source
         0,     //offset for source
         SRCCOPY);
end;


procedure Blur4px(ASrcBmp, ADestBmp: TBitmap);  //4px means that it uses the left, right, top and bottom pixels around any given pixel, to compute the average.
var
  WorkBmp, LeftBmp, RightBmp, TopBmp, BottomBmp, WorkBmpH, WorkBmpV: TBitmap;
begin
  WorkBmp := TBitmap.Create;
  WorkBmpH := TBitmap.Create;
  WorkBmpV := TBitmap.Create;
  try
    WipeBitmap(WorkBmp, ASrcBmp.Width, ASrcBmp.Height);
    WipeBitmap(WorkBmpH, ASrcBmp.Width, ASrcBmp.Height);
    WipeBitmap(WorkBmpV, ASrcBmp.Width, ASrcBmp.Height);
    WipeBitmap(ADestBmp, ASrcBmp.Width, ASrcBmp.Height);

    LeftBmp := TBitmap.Create;
    RightBmp := TBitmap.Create;
    try
      ShiftBmpToLeft(ASrcBmp, LeftBmp, 1);
      ShiftBmpToRight(ASrcBmp, RightBmp, 1);
      AvgBitmapWithBitmap(LeftBmp, RightBmp, WorkBmpH);
    finally
      LeftBmp.Free;
      RightBmp.Free;
    end;

    TopBmp := TBitmap.Create;
    BottomBmp := TBitmap.Create;
    try
      ShiftBmpToTop(ASrcBmp, TopBmp, 1);
      ShiftBmpToBottom(ASrcBmp, BottomBmp, 1);
      AvgBitmapWithBitmap(TopBmp, BottomBmp, WorkBmpV);
    finally
      TopBmp.Free;
      BottomBmp.Free;
    end;

    AvgBitmapWithBitmap(WorkBmpH, WorkBmpV, WorkBmp);
    AvgBitmapWithBitmap(WorkBmp, ASrcBmp, ADestBmp);
  finally
    WorkBmp.Free;
    WorkBmpH.Free;
    WorkBmpV.Free;
  end;
end;


procedure Blur4pxDiag(ASrcBmp, ADestBmp: TBitmap);  //4px means that it uses the left, right, top and bottom pixels around any given pixel, to compute the average.
var
  WorkBmp, TopLeftBmp, TopRightBmp, BottomLeftBmp, BottomRightBmp, WorkBmpDiagA, WorkBmpDiagB: TBitmap;
begin
  WorkBmp := TBitmap.Create;
  WorkBmpDiagA := TBitmap.Create;
  WorkBmpDiagB := TBitmap.Create;
  try
    WipeBitmap(WorkBmp, ASrcBmp.Width, ASrcBmp.Height);
    WipeBitmap(WorkBmpDiagA, ASrcBmp.Width, ASrcBmp.Height);
    WipeBitmap(WorkBmpDiagB, ASrcBmp.Width, ASrcBmp.Height);
    WipeBitmap(ADestBmp, ASrcBmp.Width, ASrcBmp.Height);

    TopLeftBmp := TBitmap.Create;
    BottomRightBmp := TBitmap.Create;
    try
      ShiftBmpToTopLeft(ASrcBmp, TopLeftBmp, 1);
      ShiftBmpToBottomRight(ASrcBmp, BottomRightBmp, 1);
      AvgBitmapWithBitmap(TopLeftBmp, BottomRightBmp, WorkBmpDiagA);
    finally
      TopLeftBmp.Free;
      BottomRightBmp.Free;
    end;

    BottomLeftBmp := TBitmap.Create;
    TopRightBmp := TBitmap.Create;
    try
      ShiftBmpToBottomLeft(ASrcBmp, BottomLeftBmp, 1);
      ShiftBmpToTopRight(ASrcBmp, TopRightBmp, 1);
      AvgBitmapWithBitmap(BottomLeftBmp, TopRightBmp, WorkBmpDiagB);
    finally
      BottomLeftBmp.Free;
      TopRightBmp.Free;
    end;

    AvgBitmapWithBitmap(WorkBmpDiagA, WorkBmpDiagB, {WorkBmp} ADestBmp);
    //AvgBitmapWithBitmap(WorkBmp, ASrcBmp, ADestBmp);    //no average with the center pixel
  finally
    WorkBmp.Free;
    WorkBmpDiagA.Free;
    WorkBmpDiagB.Free;
  end;
end;


procedure Blur8px(ASrcBmp, ADestBmp: TBitmap);
var
  WorkBmpC, WorkBmpD, WorkBmpCD: TBitmap;
begin
  WorkBmpC := TBitmap.Create;
  WorkBmpD := TBitmap.Create;
  WorkBmpCD := TBitmap.Create;
  try
    WipeBitmap(ADestBmp, ASrcBmp.Width, ASrcBmp.Height);
    WipeBitmap(WorkBmpCD, ASrcBmp.Width, ASrcBmp.Height);

    Blur4px(ASrcBmp, WorkBmpC);
    Blur4pxDiag(ASrcBmp, WorkBmpD);

    AvgBitmapWithBitmap(WorkBmpC, WorkBmpD, WorkBmpCD);
    AvgBitmapWithBitmap(WorkBmpC, WorkBmpCD, ADestBmp);  //The diagonal is scaled down. Ideally, the average algorithm should support multiple bitmaps at once, with scaling.
  finally
    WorkBmpC.Free;
    WorkBmpD.Free;
    WorkBmpCD.Free;
  end;
end;

end.

