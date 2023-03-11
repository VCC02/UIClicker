{
    Copyright (C) 2022 VCC
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


unit ClickerPrimitiveValues;

{$mode Delphi}

interface

uses
  Classes, SysUtils, ObjectInspectorFrame, ClickerActionValues, ClickerPrimitiveUtils;


{$DEFINE SubProperties}

//OI stuff
type
  TOIPropDef = record
    Name: string;
    EditorType: TOIEditorType;
  end;

const
  CCategoryCount = 2;
  CPrimitiveTypeCount = 8;

  CCategory_Primitives = 0;  //index of first category  - list of primitives, as added by user
  CCategory_Order = 1;       //index of second category - list of composition orders for all above primitives

  CCategories: array[0..CCategoryCount - 1] of string = ('Primitives', 'Composition orders');

  CPropCountClkSetPenPrimitive = 7;
  CPropCountClkSetBrushPrimitive = 3;
  CPropCountClkSetGradientFillPrimitive = 3;
  CPropCountClkSetFontPrimitive = CPropCount_FindControlMatchBitmapText; //16;
  CPropCountClkImagePrimitive = 1;
  CPropCountClkLinePrimitive = 4;
  CPropCountClkRectPrimitive = 4;
  CPropCountClkGradientFillPrimitive = 4;

  CClkPrimitivesTypeCounts: array[0..CPrimitiveTypeCount - 1] of Integer = (
    CPropCountClkSetPenPrimitive,
    CPropCountClkSetBrushPrimitive,
    CPropCountClkSetGradientFillPrimitive,
    CPropCountClkSetFontPrimitive,
    CPropCountClkImagePrimitive,
    CPropCountClkLinePrimitive,
    CPropCountClkRectPrimitive,
    CPropCountClkGradientFillPrimitive
  );


  CSetPenPrimitiveProperties: array[0..CPropCountClkSetPenPrimitive - 1] of TOIPropDef = (
    (Name: 'Color'; EditorType: etColorCombo),
    (Name: 'Style'; EditorType: etEnumCombo),
    (Name: 'Width'; EditorType: etSpinText),
    (Name: 'Mode'; EditorType: etEnumCombo),
    (Name: 'Pattern'; EditorType: etTextWithArrow),
    (Name: 'EndCap'; EditorType: etEnumCombo),
    (Name: 'JoinStyle'; EditorType: etEnumCombo)
  );

  CSetBrushPrimitiveProperties: array[0..CPropCountClkSetBrushPrimitive - 1] of TOIPropDef = (
    (Name: 'Color'; EditorType: etColorCombo),
    (Name: 'Style'; EditorType: etEnumCombo),
    (Name: 'Pattern'; EditorType: etTextWithArrow)
  );

  CSetGradientFillPrimitiveProperties: array[0..CPropCountClkSetGradientFillPrimitive - 1] of TOIPropDef = (
    (Name: 'StartColor'; EditorType: etColorCombo),
    (Name: 'StopColor'; EditorType: etColorCombo),
    (Name: 'Direction'; EditorType: etEnumCombo)
  );

  CSetFontPrimitiveProperties: array[0..CPropCountClkSetFontPrimitive - 1] of TOIPropDef = (
    (Name: 'ForegroundColor'; EditorType: etColorCombo),
    (Name: 'BackgroundColor'; EditorType: etColorCombo),
    (Name: 'FontName'; EditorType: etEnumComboWithBtn),
    (Name: 'FontSize'; EditorType: etSpinText),
    (Name: 'Bold'; EditorType: etBooleanCombo),
    (Name: 'Italic'; EditorType: etBooleanCombo),
    (Name: 'Underline'; EditorType: etBooleanCombo),
    (Name: 'StrikeOut'; EditorType: etBooleanCombo),
    (Name: 'FontQuality'; EditorType: etEnumCombo),
    (Name: 'FontQualityUsesReplacement'; EditorType: etBooleanCombo),
    (Name: 'FontQualityReplacement'; EditorType: etText),
    (Name: 'ProfileName'; EditorType: etText),
    (Name: 'CropLeft'; EditorType: etSpinText),
    (Name: 'CropTop'; EditorType: etSpinText),
    (Name: 'CropRight'; EditorType: etSpinText),
    (Name: 'CropBottom'; EditorType: etSpinText)
  );

  CImagePrimitiveProperties: array[0..CPropCountClkImagePrimitive - 1] of TOIPropDef = (
    (Name: 'Path'; EditorType: etFilePathWithArrow)
  );

  CLinePrimitiveProperties: array[0..CPropCountClkLinePrimitive - 1] of TOIPropDef = (
    (Name: 'X1'; EditorType: etColorCombo),
    (Name: 'Y1'; EditorType: etColorCombo),
    (Name: 'X2'; EditorType: etColorCombo),
    (Name: 'Y2'; EditorType: etEnumCombo)
  );

  CRectPrimitiveProperties: array[0..CPropCountClkRectPrimitive - 1] of TOIPropDef = (
    (Name: 'X1'; EditorType: etColorCombo),
    (Name: 'Y1'; EditorType: etColorCombo),
    (Name: 'X2'; EditorType: etColorCombo),
    (Name: 'Y2'; EditorType: etEnumCombo)
  );

  CGradientFillPrimitiveProperties: array[0..CPropCountClkGradientFillPrimitive - 1] of TOIPropDef = (
    (Name: 'X1'; EditorType: etColorCombo),
    (Name: 'Y1'; EditorType: etColorCombo),
    (Name: 'X2'; EditorType: etColorCombo),
    (Name: 'Y2'; EditorType: etEnumCombo)
  );

  CSetPenPrimitive_Color_PropIndex = 0;
  CSetPenPrimitive_Style_PropIndex = 1;
  CSetPenPrimitive_Width_PropIndex = 2;
  CSetPenPrimitive_Mode_PropIndex = 3;
  CSetPenPrimitive_Pattern_PropIndex = 4;
  CSetPenPrimitive_EndCap_PropIndex = 5;
  CSetPenPrimitive_JoinStyle_PropIndex = 6;


type
  TArrayOfProperties = array[0..0] of TOIPropDef;
  PArrayOfProperties = ^TArrayOfProperties;
  TArrayOfPropertiesArr = array[0..CPrimitiveTypeCount - 1] of PArrayOfProperties;

const
  CMainProperties: TArrayOfPropertiesArr = (
    @CSetPenPrimitiveProperties,
    @CSetBrushPrimitiveProperties,
    @CSetGradientFillPrimitiveProperties,
    @CSetFontPrimitiveProperties,
    @CImagePrimitiveProperties,
    @CLinePrimitiveProperties,
    @CRectPrimitiveProperties,
    @CGradientFillPrimitiveProperties
  );


{$IFDEF SubProperties}
  function GetActionValueStr_SetPen(APrimitive: PPrimitiveRec; APropertyIndex: Integer): string;
{$ENDIF}


procedure FillInDefaultValuesToPrimitive_SetPen(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_SetBrush(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_SetGradientFill(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_SetFont(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_Image(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_Line(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_Rect(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_GradientFill(var APrimitive: TPrimitiveRec);


type
  TFillInDefaultValuesToPrimitive = procedure(var APrimitive: TPrimitiveRec);

const
  CFillInDefaultValuesToPrimitives: array[0..CPrimitiveTypeCount - 1] of TFillInDefaultValuesToPrimitive = (
    @FillInDefaultValuesToPrimitive_SetPen,
    @FillInDefaultValuesToPrimitive_SetBrush,
    @FillInDefaultValuesToPrimitive_SetGradientFill,
    @FillInDefaultValuesToPrimitive_SetFont,
    @FillInDefaultValuesToPrimitive_Image,
    @FillInDefaultValuesToPrimitive_Line,
    @FillInDefaultValuesToPrimitive_Rect,
    @FillInDefaultValuesToPrimitive_GradientFill
  );


implementation

uses
  Graphics;


{$IFDEF SubProperties}
  function GetActionValueStr_SetPen(APrimitive: PPrimitiveRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := APrimitive^.ClkSetPen.Color; //TColor;
      1: Result := APrimitive^.ClkSetPen.Style; //TFPPenStyle;
      2: Result := APrimitive^.ClkSetPen.Width; //Integer;
      3: Result := APrimitive^.ClkSetPen.Mode; //TFPPenMode;
      4: Result := APrimitive^.ClkSetPen.Pattern; //LongWord;
      5: Result := APrimitive^.ClkSetPen.EndCap; //TFPPenEndCap;
      6: Result := APrimitive^.ClkSetPen.JoinStyle; //TFPPenJoinStyle;
    end;
  end;

{$ENDIF}


procedure FillInDefaultValuesToPrimitive_SetPen(var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkSetPen.Color := '4444FF';
  APrimitive.ClkSetPen.Style := '0';
  APrimitive.ClkSetPen.Width := '1';
  APrimitive.ClkSetPen.Mode := '0';
  APrimitive.ClkSetPen.Pattern := '0';
  APrimitive.ClkSetPen.EndCap := '0';
  APrimitive.ClkSetPen.JoinStyle := '0';
end;


procedure FillInDefaultValuesToPrimitive_SetBrush(var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkSetBrush.Color := '44FF33';
  APrimitive.ClkSetBrush.Style := '0';
  APrimitive.ClkSetBrush.Pattern := '0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0,,0';
end;


procedure FillInDefaultValuesToPrimitive_SetGradientFill(var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkSetGradientFill.StartColor := '00FF33';
  APrimitive.ClkSetGradientFill.StopColor := '330099';
  APrimitive.ClkSetGradientFill.Direction := '0';
end;


procedure FillInDefaultValuesToPrimitive_SetFont(var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkSetFont.ForegroundColor := '000000';
  APrimitive.ClkSetFont.BackgroundColor := 'FFFFFF';
  APrimitive.ClkSetFont.FontName := 'Tahoma';
  APrimitive.ClkSetFont.FontSize := 8;
  APrimitive.ClkSetFont.Bold := False;
  APrimitive.ClkSetFont.Italic := False;
  APrimitive.ClkSetFont.Underline := False;
  APrimitive.ClkSetFont.StrikeOut := False;
  APrimitive.ClkSetFont.FontQuality := fqDefault;
  APrimitive.ClkSetFont.FontQualityUsesReplacement := False;
  APrimitive.ClkSetFont.FontQualityReplacement := '';
  APrimitive.ClkSetFont.ProfileName := 'Default'; //probably, not used anyway
end;


procedure FillInDefaultValuesToPrimitive_Image(var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkImage.Path := '';
end;


procedure FillInDefaultValuesToPrimitive_Line(var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkLine.X1 := '14';
  APrimitive.ClkLine.Y1 := '15';
  APrimitive.ClkLine.X2 := '16';
  APrimitive.ClkLine.Y2 := '17';
end;


procedure FillInDefaultValuesToPrimitive_Rect(var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkRect.X1 := '24';
  APrimitive.ClkRect.Y1 := '26';
  APrimitive.ClkRect.X2 := '27';
  APrimitive.ClkRect.Y2 := '29';
end;


procedure FillInDefaultValuesToPrimitive_GradientFill(var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkGradientFill.X1 := '4';
  APrimitive.ClkGradientFill.Y1 := '5';
  APrimitive.ClkGradientFill.X2 := '16';
  APrimitive.ClkGradientFill.Y2 := '17';
end;

end.

