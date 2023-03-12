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
    Pattern: string; //LongWord;
    EndCap: string; //TFPPenEndCap;
    JoinStyle: string; //TFPPenJoinStyle;
  end;

  TClkSetBrush = record
    Color: string; //TColor;
    Style: string; //TFPBrushStyle;
    Pattern: string; //TBrushPattern;
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
  end;

  TClkLine = record
    X1: string;
    X2: string;
    Y1: string;
    Y2: string;
  end;

  TClkRect = record
    X1: string;
    X2: string;
    Y1: string;
    Y2: string;
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


const
  CClkSetPenPrimitiveCmdIdx = 0;
  CClkSetBrushPrimitiveCmdIdx = 1;
  CClkSetMiscPrimitiveCmdIdx = 2;
  CClkSetFontPrimitiveCmdIdx = 3;
  CClkImagePrimitiveCmdIdx = 4;
  CClkLinePrimitiveCmdIdx = 5;
  CClkRectPrimitiveCmdIdx = 6;
  CClkGradientFill = 7;


type
  TPrimitiveRec = record        //Only one of the "primitive" fields is used at a time. This is similar to TClkActionRec.
    PrimitiveType: Integer; //index of one of the following fields
    PrimitiveName: string;

    ClkSetPen: TClkSetPen;
    ClkSetBrush: TClkSetBrush;
    ClkSetMisc: TClkSetMisc;
    ClkSetFont: TClkSetFont;
    ClkImage: TClkImage;
    ClkLine: TClkLine;
    ClkRect: TClkRect;
    ClkGradientFill: TClkGradientFill;
  end;

  PPrimitiveRec = ^TPrimitiveRec;

  TPrimitiveRecArr = array of TPrimitiveRec;

  TIntArr = array of Integer;         //redefined from BinSearchValues
  TIntArrArr = array of TIntArr;      //redefined from BinSearchValues
  TPrimitiveOrderArr = TIntArrArr;

  TOnLoadPrimitivesFile = procedure(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TPrimitiveOrderArr) of object;
  TOnSavePrimitivesFile = procedure(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TPrimitiveOrderArr) of object;


const
  CPrimitiveTypeCount = 8;
  CPrimitiveNames: array[0..CPrimitiveTypeCount - 1] of string = (
    'SetPen', 'SetBrush', 'SetMisc', 'SetFont', 'Image', 'Line', 'Rect', 'GradientFill');

function PrimitiveTypeNameToIndex(AName: string): Integer;


implementation


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

end.

