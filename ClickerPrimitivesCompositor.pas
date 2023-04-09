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
    FOnEvaluateReplacementsFunc: TEvaluateReplacementsFunc;
    FOnLoadBitmap: TOnLoadBitmap;

    procedure SetFontByPrimitive(ADestCanvas: TCanvas; var APrimitive: TPrimitiveRec);

    function DoOnEvaluateReplacementsFunc(s: string; Recursive: Boolean = True): string;
    function DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
  public
    constructor Create;
    procedure ComposePrimitives(ABmp: TBitmap; AOrderIndex: Integer; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var APrimitiveSettings: TPrimitiveSettings);

    function GetMaxX(ADestCanvas: TCanvas; var APrimitives: TPrimitiveRecArr): Integer;
    function GetMaxY(ADestCanvas: TCanvas; var APrimitives: TPrimitiveRecArr): Integer;

    property OnEvaluateReplacementsFunc: TEvaluateReplacementsFunc write FOnEvaluateReplacementsFunc;
    property OnLoadBitmap: TOnLoadBitmap write FOnLoadBitmap;
  end;


implementation


uses
  FPCanvas;


procedure ComposePrimitive_SetPen(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
begin
  ABmp.Canvas.Pen.Color := HexToInt(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Color)); //TColor;
  ABmp.Canvas.Pen.Style := PenStyleNameToIndex(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Style)); //TFPPenStyle;
  ABmp.Canvas.Pen.Width := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Width), 1); //Integer;
  ABmp.Canvas.Pen.Mode := PenModeNameToIndex(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.Mode)); //TFPPenMode;
  ABmp.Canvas.Pen.EndCap := PenEndCapNameToIndex(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.EndCap)); //TFPPenEndCap;
  ABmp.Canvas.Pen.JoinStyle := PenJoinStyleNameToIndex(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetPen.JoinStyle)); //TFPPenJoinStyle;
end;


procedure ComposePrimitive_SetBrush(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
begin
  ABmp.Canvas.Brush.Color := HexToInt(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetBrush.Color)); //TColor;
  ABmp.Canvas.Brush.Style := BrushStyleNameToIndex(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetBrush.Style)); //TFPBrushStyle;
end;


procedure ComposePrimitive_SetMisc(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
begin
  ABmp.Canvas.AntialiasingMode := TAntialiasingMode(StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetMisc.AntialiasingMode), Ord(amDontCare)));
end;


procedure ComposePrimitive_SetFont(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
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


procedure ComposePrimitive_Image(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
var
  SrcBmp: TBitmap;
  SrcBitmapFnm: string;
  WillStretch: Boolean;
  TempRect: TRect;
begin
  ABmp.Canvas.Brush.Color := HexToInt(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetBrush.Color)); //TColor;
  ABmp.Canvas.Brush.Style := TBrushStyle(StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkSetBrush.Style), Ord(bsSolid))); //TFPBrushStyle;

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


procedure ComposePrimitive_Line(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
var
  x1, y1, x2, y2: Integer;
begin
  x1 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.X1), 10);
  y1 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.Y1), 20);
  x2 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.X2), 30);
  y2 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkLine.Y2), 40);

  ABmp.Canvas.Line(x1, y1, x2, y2);
end;


procedure ComposePrimitive_Rect(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
var
  x1, y1, x2, y2: Integer;
begin
  x1 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRect.X1), 10);
  y1 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRect.Y1), 20);
  x2 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRect.X2), 30);
  y2 := StrToIntDef(Sender.DoOnEvaluateReplacementsFunc(APrimitive.ClkRect.Y2), 40);

  ABmp.Canvas.Rectangle(x1, y1, x2, y2);
end;


procedure ComposePrimitive_GradientFill(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
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


procedure ComposePrimitive_Text(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec);
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


constructor TPrimitivesCompositor.Create;
begin
  inherited Create;

  FOnEvaluateReplacementsFunc := nil;
  FOnLoadBitmap := nil;
end;


function TPrimitivesCompositor.DoOnEvaluateReplacementsFunc(s: string; Recursive: Boolean = True): string;
begin
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


type
  TComposePrimitivesProc = procedure(Sender: TPrimitivesCompositor; ABmp: TBitmap; var APrimitive: TPrimitiveRec);


procedure TPrimitivesCompositor.ComposePrimitives(ABmp: TBitmap; AOrderIndex: Integer; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var APrimitiveSettings: TPrimitiveSettings);
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
    @ComposePrimitive_Text
  );

var
  i, NewIndex: Integer;
begin
  ABmp.Transparent := False;
  ABmp.Canvas.Pen.Style := psSolid;
  ABmp.Canvas.Pen.Width := 1;
  ABmp.Canvas.Brush.Style := bsClear;//bsSolid;
  ABmp.Canvas.Pen.Color := clWhite;
  ABmp.Canvas.Brush.Color := clWhite;
  ABmp.Canvas.Rectangle(0, 0, ABmp.Width, ABmp.Height);

  ABmp.Canvas.Brush.Style := bsSolid;  //reset to some default values
  ABmp.Canvas.Pen.Color := clWhite;
  ABmp.Canvas.Brush.Color := clWhite;

  case APrimitiveSettings.CompositorDirection of
    cdTopBot:
    begin
      for i := 0 to Length(APrimitives) - 1 do
      begin
        NewIndex := AOrders[AOrderIndex].Items[i];
        CComposePrimitives[APrimitives[NewIndex].PrimitiveType](Self, ABmp, APrimitives[NewIndex]);
      end;
    end;

    cdBotTop:
    begin
      for i := Length(APrimitives) - 1 downto 0 do
      begin
        NewIndex := AOrders[AOrderIndex].Items[i];
        CComposePrimitives[APrimitives[NewIndex].PrimitiveType](Self, ABmp, APrimitives[NewIndex]);
      end;
    end;
  end;
end;


//Ideally, the whole primitive file should be composed, to take the entire stack into account. Then, the proper font settings are applied.
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
  X, W: Integer;
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
        Inc(X, W);

        if Result < X then
          Result := X;
      end;
    end;
  end;
end;


function TPrimitivesCompositor.GetMaxY(ADestCanvas: TCanvas; var APrimitives: TPrimitiveRecArr): Integer;
var
  i: Integer;
  Y, H: Integer;
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
        Inc(Y, H);

        if Result < Y then
          Result := Y;
      end;
    end;
  end;
end;

end.


