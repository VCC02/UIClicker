{
    Copyright (C) 2023 VCC
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


procedure LoadPrimitivesFile(Ini: TClkIniReadonlyFile; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
procedure SavePrimitivesFile(AStringList: TStringList; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);


implementation


uses
  Graphics;


procedure Get_SetPen_PrimitiveFromIni(AIni: TClkIniReadonlyFile; ASectionIndex: Integer; var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkSetPen.Color := AIni.ReadString(ASectionIndex, 'Color', '0000FF');
  APrimitive.ClkSetPen.Style := AIni.ReadString(ASectionIndex, 'Style', '0'); //TFPPenStyle;
  APrimitive.ClkSetPen.Width := AIni.ReadString(ASectionIndex, 'Width', '1'); //Integer;
  APrimitive.ClkSetPen.Mode := AIni.ReadString(ASectionIndex, 'Mode', '0');  //TFPPenMode;
  APrimitive.ClkSetPen.EndCap := AIni.ReadString(ASectionIndex, 'EndCap', '0'); //TFPPenEndCap;
  APrimitive.ClkSetPen.JoinStyle := AIni.ReadString(ASectionIndex, 'JoinStyle', '0');
end;


procedure Get_SetBrush_PrimitiveFromIni(AIni: TClkIniReadonlyFile; ASectionIndex: Integer; var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkSetBrush.Color := AIni.ReadString(ASectionIndex, 'Color', '00FFFF');
  APrimitive.ClkSetBrush.Style := AIni.ReadString(ASectionIndex, 'Style', '0');
end;


procedure Get_SetMisc_PrimitiveFromIni(AIni: TClkIniReadonlyFile; ASectionIndex: Integer; var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkSetMisc.AntialiasingMode := AIni.ReadString(ASectionIndex, 'AntialiasingMode', '0');
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
  APrimitive.ClkSetFont.CropLeft := AIni.ReadString(ASectionIndex, 'CropLeft', '0');
  APrimitive.ClkSetFont.CropTop := AIni.ReadString(ASectionIndex, 'CropTop', '0');
  APrimitive.ClkSetFont.CropRight := AIni.ReadString(ASectionIndex, 'CropRight', '0');
  APrimitive.ClkSetFont.CropBottom := AIni.ReadString(ASectionIndex, 'CropBottom', '0');
  APrimitive.ClkSetFont.IgnoreBackgroundColor := AIni.ReadBool(ASectionIndex, 'IgnoreBackgroundColor', False);
end;


procedure Get_Image_PrimitiveFromIni(AIni: TClkIniReadonlyFile; ASectionIndex: Integer; var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkImage.X1 := AIni.ReadString(ASectionIndex, 'X1', '30');
  APrimitive.ClkImage.Y1 := AIni.ReadString(ASectionIndex, 'Y1', '40');
  APrimitive.ClkImage.X2 := AIni.ReadString(ASectionIndex, 'X2', '50');
  APrimitive.ClkImage.Y2 := AIni.ReadString(ASectionIndex, 'Y2', '60');
  APrimitive.ClkImage.Path := AIni.ReadString(ASectionIndex, 'Path', '');
  APrimitive.ClkImage.Stretch := AIni.ReadString(ASectionIndex, 'Stretch', '0');
  APrimitive.ClkImage.RenderedExternally := AIni.ReadString(ASectionIndex, 'RenderedExternally', '0');
  APrimitive.ClkImage.Transparent := AIni.ReadString(ASectionIndex, 'Transparent', '0');
  APrimitive.ClkImage.TransparentMode := AIni.ReadString(ASectionIndex, 'TransparentMode', '0');
  APrimitive.ClkImage.TransparentColor := AIni.ReadString(ASectionIndex, 'TransparentColor', 'F0F0F0');
end;


procedure Get_Line_PrimitiveFromIni(AIni: TClkIniReadonlyFile; ASectionIndex: Integer; var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkLine.X1 := AIni.ReadString(ASectionIndex, 'X1', '3');
  APrimitive.ClkLine.Y1 := AIni.ReadString(ASectionIndex, 'Y1', '4');
  APrimitive.ClkLine.X2 := AIni.ReadString(ASectionIndex, 'X2', '5');
  APrimitive.ClkLine.Y2 := AIni.ReadString(ASectionIndex, 'Y2', '6');
  APrimitive.ClkLine.ShowEndpointPixel := AIni.ReadString(ASectionIndex, 'ShowEndpointPixel', '1');   //defaults to 1, because this is the behavior which makes sense
end;


procedure Get_Rect_PrimitiveFromIni(AIni: TClkIniReadonlyFile; ASectionIndex: Integer; var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkRect.X1 := AIni.ReadString(ASectionIndex, 'X1', '5');
  APrimitive.ClkRect.Y1 := AIni.ReadString(ASectionIndex, 'Y1', '6');
  APrimitive.ClkRect.X2 := AIni.ReadString(ASectionIndex, 'X2', '7');
  APrimitive.ClkRect.Y2 := AIni.ReadString(ASectionIndex, 'Y2', '8');
  APrimitive.ClkRect.ExtendToEndpointCorner := AIni.ReadString(ASectionIndex, 'ExtendToEndpointCorner', '1');  //defaults to 1, because this is the behavior which makes sense
end;


procedure Get_GradientFill_PrimitiveFromIni(AIni: TClkIniReadonlyFile; ASectionIndex: Integer; var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkGradientFill.X1 := AIni.ReadString(ASectionIndex, 'X1', '3');
  APrimitive.ClkGradientFill.Y1 := AIni.ReadString(ASectionIndex, 'Y1', '4');
  APrimitive.ClkGradientFill.X2 := AIni.ReadString(ASectionIndex, 'X2', '7');
  APrimitive.ClkGradientFill.Y2 := AIni.ReadString(ASectionIndex, 'Y2', '8');
end;


procedure Get_Text_PrimitiveFromIni(AIni: TClkIniReadonlyFile; ASectionIndex: Integer; var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkText.Text := AIni.ReadString(ASectionIndex, 'Text', '');
  APrimitive.ClkText.X := AIni.ReadString(ASectionIndex, 'X', '3');
  APrimitive.ClkText.Y := AIni.ReadString(ASectionIndex, 'Y', '4');
end;


procedure Get_DonutSector_PrimitiveFromIni(AIni: TClkIniReadonlyFile; ASectionIndex: Integer; var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkDonutSector.Cx := AIni.ReadString(ASectionIndex, 'Cx', '100');
  APrimitive.ClkDonutSector.Cy := AIni.ReadString(ASectionIndex, 'Cy', '100');
  APrimitive.ClkDonutSector.Radius1 := AIni.ReadString(ASectionIndex, 'Radius1', '30');
  APrimitive.ClkDonutSector.Radius2 := AIni.ReadString(ASectionIndex, 'Radius2', '90');
  APrimitive.ClkDonutSector.PointCount := AIni.ReadString(ASectionIndex, 'PointCount', '40');
  APrimitive.ClkDonutSector.StartAngle := AIni.ReadString(ASectionIndex, 'StartAngle', '-60');
  APrimitive.ClkDonutSector.EndAngle := AIni.ReadString(ASectionIndex, 'EndAngle', '240');
  APrimitive.ClkDonutSector.AngleSpacing := AIni.ReadString(ASectionIndex, 'AngleSpacing', '0');
  APrimitive.ClkDonutSector.StartColorFG := AIni.ReadString(ASectionIndex, 'StartColorFG', '1FFFFFFF');
  APrimitive.ClkDonutSector.EndColorFG := AIni.ReadString(ASectionIndex, 'EndColorFG', '1FFFFFFF');
  APrimitive.ClkDonutSector.StartColorBG := AIni.ReadString(ASectionIndex, 'StartColorBG', '008080'); //clOlive;
  APrimitive.ClkDonutSector.EndColorBG := AIni.ReadString(ASectionIndex, 'EndColorBG', '808000');  //clTeal;
end;


type
  TGetPrimitiveFromIni = procedure(AIni: TClkIniReadonlyFile; ASectionIndex: Integer; var APrimitive: TPrimitiveRec);

const
  CGetAllPrimitiveFromIni: array[0..CPrimitiveTypeCount - 1] of TGetPrimitiveFromIni = (
    @Get_SetPen_PrimitiveFromIni,
    @Get_SetBrush_PrimitiveFromIni,
    @Get_SetMisc_PrimitiveFromIni,
    @Get_SetFont_PrimitiveFromIni,
    @Get_Image_PrimitiveFromIni,
    @Get_Line_PrimitiveFromIni,
    @Get_Rect_PrimitiveFromIni,
    @Get_GradientFill_PrimitiveFromIni,
    @Get_Text_PrimitiveFromIni,
    @Get_DonutSector_PrimitiveFromIni
  );


procedure LoadPrimitivesFile(Ini: TClkIniReadonlyFile; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
var
  n, m, i, j, SectionIndex: Integer;
  PrimitiveTypeStr, PrimitiveIndexStr, OrderIndexStr: string;
  SectionName: string;
  PrimitiveIndex, OrderIndex: Integer;
begin
  ASettings.CompositorDirection := TCompositorDirection(Ini.ReadInteger('Settings', 'CompositorDirection', Ord(cdTopBot)));

  n := Ini.ReadInteger('Primitives', 'Count', 0);
  m := Ini.ReadInteger('ProcessingOrder', 'Count', 0);
  SetLength(APrimitives, Max(Min(n, 300), 0));  //do not load more than 300 primitives
  SetLength(AOrders, Max(Min(m, 30), 0));  //do not load more than 30 orders

  for i := 0 to m - 1 do
    SetLength(AOrders[i].Items, n);     //every order should have the same number of items as there are primitives

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
      APrimitives[PrimitiveIndex].Selected := False;

      CGetAllPrimitiveFromIni[APrimitives[PrimitiveIndex].PrimitiveType](Ini, SectionIndex, APrimitives[PrimitiveIndex]);
    end;

    if (m > 0) and (Pos('Order_', SectionName) > 0) then
    begin
      OrderIndexStr := Copy(SectionName, Pos('_', SectionName) + 1, MaxInt);
      OrderIndex := StrToIntDef(OrderIndexStr, 0);

      AOrders[OrderIndex].Name := Ini.ReadString(SectionIndex, 'OrderName', '');
      for j := 0 to Length(AOrders[OrderIndex].Items) - 1 do
        AOrders[OrderIndex].Items[j] := Ini.ReadInteger(SectionIndex, 'i' + IntToStr(j), j);
    end;
  end;
end;


procedure AddPrimitive_SetPenToStringList(var APrimitive: TPrimitiveRec; AStringList: TStringList);
begin
  AStringList.Add('Color=' + APrimitive.ClkSetPen.Color);
  AStringList.Add('Style=' + APrimitive.ClkSetPen.Style);
  AStringList.Add('Width=' + APrimitive.ClkSetPen.Width);
  AStringList.Add('Mode=' + APrimitive.ClkSetPen.Mode);
  AStringList.Add('EndCap=' + APrimitive.ClkSetPen.EndCap);
  AStringList.Add('JoinStyle=' + APrimitive.ClkSetPen.JoinStyle);
end;


procedure AddPrimitive_SetBrushToStringList(var APrimitive: TPrimitiveRec; AStringList: TStringList);
begin
  AStringList.Add('Color=' + APrimitive.ClkSetBrush.Color);
  AStringList.Add('Style=' + APrimitive.ClkSetBrush.Style);
end;


procedure AddPrimitive_SetMiscToStringList(var APrimitive: TPrimitiveRec; AStringList: TStringList);
begin
  AStringList.Add('AntialiasingMode=' + APrimitive.ClkSetMisc.AntialiasingMode);
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
  AStringList.Add('IgnoreBackgroundColor=' + IntToStr(Ord(APrimitive.ClkSetFont.IgnoreBackgroundColor)));
end;


procedure AddPrimitive_ImageToStringList(var APrimitive: TPrimitiveRec; AStringList: TStringList);
begin
  AStringList.Add('X1=' + APrimitive.ClkImage.X1);
  AStringList.Add('Y1=' + APrimitive.ClkImage.Y1);
  AStringList.Add('X2=' + APrimitive.ClkImage.X2);
  AStringList.Add('Y2=' + APrimitive.ClkImage.Y2);
  AStringList.Add('Path=' + APrimitive.ClkImage.Path);
  AStringList.Add('Stretch=' + APrimitive.ClkImage.Stretch);
  AStringList.Add('RenderedExternally=' + APrimitive.ClkImage.RenderedExternally);
  AStringList.Add('Transparent=' + APrimitive.ClkImage.Transparent);
  AStringList.Add('TransparentMode=' + APrimitive.ClkImage.TransparentMode);
  AStringList.Add('TransparentColor=' + APrimitive.ClkImage.TransparentColor);
end;


procedure AddPrimitive_LineToStringList(var APrimitive: TPrimitiveRec; AStringList: TStringList);
begin
  AStringList.Add('X1=' + APrimitive.ClkLine.X1);
  AStringList.Add('Y1=' + APrimitive.ClkLine.Y1);
  AStringList.Add('X2=' + APrimitive.ClkLine.X2);
  AStringList.Add('Y2=' + APrimitive.ClkLine.Y2);
  AStringList.Add('ShowEndpointPixel=' + APrimitive.ClkLine.ShowEndpointPixel);
end;


procedure AddPrimitive_RectToStringList(var APrimitive: TPrimitiveRec; AStringList: TStringList);
begin
  AStringList.Add('X1=' + APrimitive.ClkRect.X1);
  AStringList.Add('Y1=' + APrimitive.ClkRect.Y1);
  AStringList.Add('X2=' + APrimitive.ClkRect.X2);
  AStringList.Add('Y2=' + APrimitive.ClkRect.Y2);
  AStringList.Add('ExtendToEndpointCorner=' + APrimitive.ClkRect.ExtendToEndpointCorner);
end;


procedure AddPrimitive_GradientFillToStringList(var APrimitive: TPrimitiveRec; AStringList: TStringList);
begin
  AStringList.Add('X1=' + APrimitive.ClkGradientFill.X1);
  AStringList.Add('Y1=' + APrimitive.ClkGradientFill.Y1);
  AStringList.Add('X2=' + APrimitive.ClkGradientFill.X2);
  AStringList.Add('Y2=' + APrimitive.ClkGradientFill.Y2);
end;


procedure AddPrimitive_TextToStringList(var APrimitive: TPrimitiveRec; AStringList: TStringList);
begin
  AStringList.Add('Text=' + APrimitive.ClkText.Text);
  AStringList.Add('X=' + APrimitive.ClkText.X);
  AStringList.Add('Y=' + APrimitive.ClkText.Y);
end;


procedure AddPrimitive_DonutSectorToStringList(var APrimitive: TPrimitiveRec; AStringList: TStringList);
begin
  AStringList.Add('Cx=' + APrimitive.ClkDonutSector.Cx);
  AStringList.Add('Cy=' + APrimitive.ClkDonutSector.Cy);
  AStringList.Add('Radius1=' + APrimitive.ClkDonutSector.Radius1);
  AStringList.Add('Radius2=' + APrimitive.ClkDonutSector.Radius2);
  AStringList.Add('PointCount=' + APrimitive.ClkDonutSector.PointCount);
  AStringList.Add('StartAngle=' + APrimitive.ClkDonutSector.StartAngle);
  AStringList.Add('EndAngle=' + APrimitive.ClkDonutSector.EndAngle);
  AStringList.Add('AngleSpacing=' + APrimitive.ClkDonutSector.AngleSpacing);
  AStringList.Add('StartColorFG=' + APrimitive.ClkDonutSector.StartColorFG);
  AStringList.Add('EndColorFG=' + APrimitive.ClkDonutSector.EndColorFG);
  AStringList.Add('StartColorBG=' + APrimitive.ClkDonutSector.StartColorBG);
  AStringList.Add('EndColorBG=' + APrimitive.ClkDonutSector.EndColorBG);
end;


type
  TAddPrimitive_ToStringList = procedure(var APrimitive: TPrimitiveRec; AStringList: TStringList);

const
  CAddAllPrimitives_ToStringList: array[0..CPrimitiveTypeCount - 1] of TAddPrimitive_ToStringList = (
    @AddPrimitive_SetPenToStringList,
    @AddPrimitive_SetBrushToStringList,
    @AddPrimitive_SetMiscToStringList,
    @AddPrimitive_SetFontToStringList,
    @AddPrimitive_ImageToStringList,
    @AddPrimitive_LineToStringList,
    @AddPrimitive_RectToStringList,
    @AddPrimitive_GradientFillToStringList,
    @AddPrimitive_TextToStringList,
    @AddPrimitive_DonutSectorToStringList
  );


procedure SavePrimitivesFile(AStringList: TStringList; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
var
  i, j: Integer;
  IterationStr: string;
begin
  AStringList.Add('[Settings]');
  AStringList.Add('CompositorDirection=' + IntToStr(Ord(ASettings.CompositorDirection)));
  AStringList.Add('');

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
    AStringList.Add('PrimitiveName=' + APrimitives[i].PrimitiveName);
    AStringList.Add('Primitive=' + CPrimitiveNames[APrimitives[i].PrimitiveType]);

    CAddAllPrimitives_ToStringList[APrimitives[i].PrimitiveType](APrimitives[i], AStringList);

    AStringList.Add('');
  end;

  AStringList.Add('');

  for i := 0 to Length(AOrders) - 1 do
  begin
    AStringList.Add('[Order_' + IntToStr(i) + ']');
    AStringList.Add('OrderName=' + AOrders[i].Name);

    for j := 0 to Length(AOrders[i].Items) - 1 do
      AStringList.Add('i' + IntToStr(j) + '=' + IntToStr(AOrders[i].Items[j]));

    AStringList.Add('');
  end;
end;

end.

