{
    Copyright (C) 2022 VCC
    creation date: Dec 2019
    initial release date: 26 Jul 2022

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

unit ClickerPrimitives;

{$mode Delphi}

interface

uses
  Classes, SysUtils, ClickerUtils, ClickerPrimitiveUtils, ClickerIniFiles, Math;


procedure LoadPrimitivesFile(Ini: TClkIniReadonlyFile; var APrimitives: TPrimitiveRecArr; var AOrders: TPrimitiveOrderArr);
procedure SavePrimitivesFile(AStringList: TStringList; var APrimitives: TPrimitiveRecArr; var AOrders: TPrimitiveOrderArr);


implementation


uses
  Graphics;


procedure Get_SetPen_PrimitiveFromIni(AIni: TClkIniReadonlyFile; ASectionIndex: Integer; var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkSetPen.Color := AIni.ReadString(ASectionIndex, 'Color', '0000FF');
  APrimitive.ClkSetPen.Style := AIni.ReadString(ASectionIndex, 'Style', '0'); //TFPPenStyle;
  APrimitive.ClkSetPen.Width := AIni.ReadString(ASectionIndex, 'Width', '1'); //Integer;
  APrimitive.ClkSetPen.Mode := AIni.ReadString(ASectionIndex, 'Mode', '0');  //TFPPenMode;
  APrimitive.ClkSetPen.Pattern := AIni.ReadString(ASectionIndex, 'Pattern', '0'); //LongWord;
  APrimitive.ClkSetPen.EndCap := AIni.ReadString(ASectionIndex, 'EndCap', '0'); //TFPPenEndCap;
  APrimitive.ClkSetPen.JoinStyle := AIni.ReadString(ASectionIndex, 'JoinStyle', '0');
end;


procedure Get_SetBrush_PrimitiveFromIni(AIni: TClkIniReadonlyFile; ASectionIndex: Integer; var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkSetBrush.Color := AIni.ReadString(ASectionIndex, 'Color', '00FFFF');
  APrimitive.ClkSetBrush.Style := AIni.ReadString(ASectionIndex, 'Style', '0');
  APrimitive.ClkSetBrush.Pattern := AIni.ReadString(ASectionIndex, 'Pattern', '0');  //TBrushPattern = array[0..PatternBitCount-1] of TPenPattern;
end;


procedure Get_SetGradientFill_PrimitiveFromIni(AIni: TClkIniReadonlyFile; ASectionIndex: Integer; var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkSetGradientFill.StartColor := AIni.ReadString(ASectionIndex, 'StartColor', '44FF44');
  APrimitive.ClkSetGradientFill.StopColor := AIni.ReadString(ASectionIndex, 'StopColor', 'FF44FF');
  APrimitive.ClkSetGradientFill.Direction := AIni.ReadString(ASectionIndex, 'Direction', '0'); //TGradientDirection
end;


procedure Get_SetFont_PrimitiveFromIni(AIni: TClkIniReadonlyFile; ASectionIndex: Integer; var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkSetFont.ForegroundColor := AIni.ReadString(ASectionIndex, 'ForegroundColor', '000000');
  APrimitive.ClkSetFont.BackgroundColor := AIni.ReadString(ASectionIndex, 'BackgroundColor', 'FFFFFF');
  APrimitive.ClkSetFont.FontName := AIni.ReadString(ASectionIndex, 'FontName', 'Tahoma');
  APrimitive.ClkSetFont.FontSize := AIni.ReadInteger(ASectionIndex, 'FontSize', 8);
  APrimitive.ClkSetFont.Bold := AIni.ReadBool(ASectionIndex, 'Bold', False);
  APrimitive.ClkSetFont.Italic := AIni.ReadBool(ASectionIndex, 'Italic', False);
  APrimitive.ClkSetFont.Underline := AIni.ReadBool(ASectionIndex, 'Underline', False);
  APrimitive.ClkSetFont.StrikeOut := AIni.ReadBool(ASectionIndex, 'StrikeOut', False);
  APrimitive.ClkSetFont.FontQuality := TFontQuality(AIni.ReadInteger(ASectionIndex, 'FontQuality', Integer(fqDefault)));
  APrimitive.ClkSetFont.FontQualityUsesReplacement := AIni.ReadBool(ASectionIndex, 'FontQualityUsesReplacement', False);
  APrimitive.ClkSetFont.FontQualityReplacement := AIni.ReadString(ASectionIndex, 'FontQualityReplacement', '$MyFontQuality$');
  APrimitive.ClkSetFont.ProfileName := AIni.ReadString(ASectionIndex, 'ProfileName', 'Default');
end;


procedure Get_Image_PrimitiveFromIni(AIni: TClkIniReadonlyFile; ASectionIndex: Integer; var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkImage.Path := AIni.ReadString(ASectionIndex, 'Path', '');
end;


procedure Get_Line_PrimitiveFromIni(AIni: TClkIniReadonlyFile; ASectionIndex: Integer; var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkLine.X1 := AIni.ReadString(ASectionIndex, 'X1', '3');
  APrimitive.ClkLine.Y1 := AIni.ReadString(ASectionIndex, 'Y1', '4');
  APrimitive.ClkLine.X2 := AIni.ReadString(ASectionIndex, 'X2', '5');
  APrimitive.ClkLine.Y2 := AIni.ReadString(ASectionIndex, 'Y2', '6');
end;


procedure Get_Rect_PrimitiveFromIni(AIni: TClkIniReadonlyFile; ASectionIndex: Integer; var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkRect.X1 := AIni.ReadString(ASectionIndex, 'X1', '5');
  APrimitive.ClkRect.Y1 := AIni.ReadString(ASectionIndex, 'Y1', '6');
  APrimitive.ClkRect.X2 := AIni.ReadString(ASectionIndex, 'X2', '7');
  APrimitive.ClkRect.Y2 := AIni.ReadString(ASectionIndex, 'Y2', '8');
end;


procedure Get_GradientFill_PrimitiveFromIni(AIni: TClkIniReadonlyFile; ASectionIndex: Integer; var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkGradientFill.X1 := AIni.ReadString(ASectionIndex, 'X1', '3');
  APrimitive.ClkGradientFill.Y1 := AIni.ReadString(ASectionIndex, 'Y1', '4');
  APrimitive.ClkGradientFill.X2 := AIni.ReadString(ASectionIndex, 'X2', '7');
  APrimitive.ClkGradientFill.Y2 := AIni.ReadString(ASectionIndex, 'Y2', '8');
end;


type
  TGetPrimitiveFromIni = procedure(AIni: TClkIniReadonlyFile; ASectionIndex: Integer; var APrimitive: TPrimitiveRec);

const
  CGetAllPrimitiveFromIni: array[0..CPrimitiveTypeCount - 1] of TGetPrimitiveFromIni = (
    @Get_SetPen_PrimitiveFromIni,
    @Get_SetBrush_PrimitiveFromIni,
    @Get_SetGradientFill_PrimitiveFromIni,
    @Get_SetFont_PrimitiveFromIni,
    @Get_Image_PrimitiveFromIni,
    @Get_Line_PrimitiveFromIni,
    @Get_Rect_PrimitiveFromIni,
    @Get_GradientFill_PrimitiveFromIni
  );


procedure LoadPrimitivesFile(Ini: TClkIniReadonlyFile; var APrimitives: TPrimitiveRecArr; var AOrders: TPrimitiveOrderArr);
var
  n, m, i, SectionIndex: Integer;
  PrimitiveTypeStr, PrimitiveIndexStr: string;
  SectionName: string;
  PrimitiveIndex: Integer;
begin
  n := Ini.ReadInteger('Primitives', 'Count', 0);
  m := Ini.ReadInteger('ProcessingOrder', 'Count', 0);
  SetLength(APrimitives, Max(Min(n, 300), 0));  //do not load more than 300 pirimitives
  SetLength(AOrders, Max(Min(m, 30), 0));  //do not load more than 30 orders

  for i := 0 to Ini.GetSectionCount - 1 do
  begin
    SectionIndex := i;
    SectionName := Ini.GetSectionAtIndex(i);

    if (n > 0) and (Pos('Primitive_', SectionName) > 0) then
    begin
      PrimitiveTypeStr := Ini.ReadString(SectionIndex, 'Primitive', 'Line');
      PrimitiveIndexStr := Copy(SectionName, Pos('_', SectionName) + 1, MaxInt);
      PrimitiveIndex := StrToIntDef(PrimitiveIndexStr, 0);
      if (PrimitiveIndex < 0) or (PrimitiveIndex > n - 1) then
        PrimitiveIndex := 0; //this will overwrite first primitive item, instead of raising an exception

      APrimitives[PrimitiveIndex].PrimitiveType := PrimitiveTypeNameToIndex(PrimitiveTypeStr);
      APrimitives[PrimitiveIndex].PrimitiveName := Ini.ReadString(SectionIndex, 'PrimitiveName', 'some primitive');

      CGetAllPrimitiveFromIni[APrimitives[PrimitiveIndex].PrimitiveType](Ini, SectionIndex, APrimitives[PrimitiveIndex]);
    end;

    if (m > 0) and (Pos('Order_', SectionName) > 0) then
    begin

    end;
  end;
end;


procedure AddPrimitive_SetPenToStringList(var APrimitive: TPrimitiveRec; AStringList: TStringList);
begin
  AStringList.Add('Color=' + APrimitive.ClkSetPen.Color);
  AStringList.Add('NewX=' + APrimitive.ClkSetPen.Style);
  AStringList.Add('NewY=' + APrimitive.ClkSetPen.Width);
  AStringList.Add('NewWidth=' + APrimitive.ClkSetPen.Mode);
  AStringList.Add('NewHeight=' + APrimitive.ClkSetPen.Pattern);
  AStringList.Add('NewPositionEnabled=' + APrimitive.ClkSetPen.EndCap);
  AStringList.Add('NewSizeEnabled=' + APrimitive.ClkSetPen.JoinStyle);
end;


procedure AddPrimitive_SetBrushToStringList(var APrimitive: TPrimitiveRec; AStringList: TStringList);
begin
  AStringList.Add('Color=' + APrimitive.ClkSetBrush.Color);
  AStringList.Add('Style=' + APrimitive.ClkSetBrush.Style);
  AStringList.Add('Pattern=' + APrimitive.ClkSetBrush.Pattern);
end;


procedure AddPrimitive_SetGradientFillToStringList(var APrimitive: TPrimitiveRec; AStringList: TStringList);
begin
  AStringList.Add('StartColor=' + APrimitive.ClkSetGradientFill.StartColor);
  AStringList.Add('StopColor=' + APrimitive.ClkSetGradientFill.StopColor);
  AStringList.Add('Direction=' + APrimitive.ClkSetGradientFill.Direction);
end;


procedure AddPrimitive_SetFontToStringList(var APrimitive: TPrimitiveRec; AStringList: TStringList);
begin
  AStringList.Add('ForegroundColor=' + APrimitive.ClkSetFont.ForegroundColor);
  AStringList.Add('BackgroundColor=' + APrimitive.ClkSetFont.BackgroundColor);
  AStringList.Add('FontName=' + APrimitive.ClkSetFont.FontName);
  AStringList.Add('FontSize=' + IntToStr(APrimitive.ClkSetFont.FontSize));
  AStringList.Add('Bold=' + IntToStr(Ord(APrimitive.ClkSetFont.Bold)));
  AStringList.Add('Italic=' + IntToStr(Ord(APrimitive.ClkSetFont.Italic)));
  AStringList.Add('Underline=' + IntToStr(Ord(APrimitive.ClkSetFont.Underline)));
  AStringList.Add('StrikeOut=' + IntToStr(Ord(APrimitive.ClkSetFont.StrikeOut)));
  AStringList.Add('FontQuality=' + IntToStr(Ord(APrimitive.ClkSetFont.FontQuality)));
  AStringList.Add('FontQualityUsesReplacement=' + IntToStr(Ord(APrimitive.ClkSetFont.FontQualityUsesReplacement)));
  AStringList.Add('FontQualityReplacement=' + APrimitive.ClkSetFont.FontQualityReplacement);
  AStringList.Add('ProfileName=' + APrimitive.ClkSetFont.ProfileName);
  AStringList.Add('CropLeft=' + APrimitive.ClkSetFont.CropLeft);
  AStringList.Add('CropTop=' + APrimitive.ClkSetFont.CropTop);
  AStringList.Add('CropRight=' + APrimitive.ClkSetFont.CropRight);
  AStringList.Add('CropBottom=' + APrimitive.ClkSetFont.CropBottom);
end;


procedure AddPrimitive_ImageToStringList(var APrimitive: TPrimitiveRec; AStringList: TStringList);
begin
  AStringList.Add('Path=' + APrimitive.ClkImage.Path);
end;


procedure AddPrimitive_LineToStringList(var APrimitive: TPrimitiveRec; AStringList: TStringList);
begin
  AStringList.Add('X1=' + APrimitive.ClkLine.X1);
  AStringList.Add('Y1=' + APrimitive.ClkLine.Y1);
  AStringList.Add('X2=' + APrimitive.ClkLine.X2);
  AStringList.Add('Y2=' + APrimitive.ClkLine.Y2);
end;


procedure AddPrimitive_RectToStringList(var APrimitive: TPrimitiveRec; AStringList: TStringList);
begin
  AStringList.Add('X1=' + APrimitive.ClkRect.X1);
  AStringList.Add('Y1=' + APrimitive.ClkRect.Y1);
  AStringList.Add('X2=' + APrimitive.ClkRect.X2);
  AStringList.Add('Y2=' + APrimitive.ClkRect.Y2);
end;


procedure AddPrimitive_GradientFillToStringList(var APrimitive: TPrimitiveRec; AStringList: TStringList);
begin
  AStringList.Add('X1=' + APrimitive.ClkGradientFill.X1);
  AStringList.Add('Y1=' + APrimitive.ClkGradientFill.Y1);
  AStringList.Add('X2=' + APrimitive.ClkGradientFill.X2);
  AStringList.Add('Y2=' + APrimitive.ClkGradientFill.Y2);
end;

type
  TAddPrimitive_ToStringList = procedure(var APrimitive: TPrimitiveRec; AStringList: TStringList);

const
  CAddAllPrimitives_ToStringList: array[0..CPrimitiveTypeCount - 1] of TAddPrimitive_ToStringList = (
    @AddPrimitive_SetPenToStringList,
    @AddPrimitive_SetBrushToStringList,
    @AddPrimitive_SetGradientFillToStringList,
    @AddPrimitive_SetFontToStringList,
    @AddPrimitive_ImageToStringList,
    @AddPrimitive_LineToStringList,
    @AddPrimitive_RectToStringList,
    @AddPrimitive_GradientFillToStringList
  );


procedure SavePrimitivesFile(AStringList: TStringList; var APrimitives: TPrimitiveRecArr; var AOrders: TPrimitiveOrderArr);
var
  i: Integer;
  IterationStr: string;
begin
  AStringList.Add('[Primitives]');
  AStringList.Add('Count=' + IntToStr(Length(APrimitives)));
  AStringList.Add('');

  AStringList.Add('[ProcessingOrder]');
  AStringList.Add('Count=' + IntToStr(Length(AOrders)));
  AStringList.Add('');

  for i := 0 to Length(APrimitives) - 1 do
  begin
    IterationStr := IntToStr(i);
    AStringList.Add('[Primitive_' + IterationStr + ']');

    CAddAllPrimitives_ToStringList[APrimitives[i].PrimitiveType](APrimitives[i], AStringList);

    AStringList.Add('');
  end;


  AStringList.Add('');
end;

end.

