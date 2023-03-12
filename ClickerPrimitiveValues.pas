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
  Classes, SysUtils, ObjectInspectorFrame, ClickerActionValues,
  ClickerUtils, ClickerPrimitiveUtils, Graphics;


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
  CPropCountClkSetMiscPrimitive = 1;
  CPropCountClkSetFontPrimitive = CPropCount_FindControlMatchBitmapText; //16;
  CPropCountClkImagePrimitive = 6;
  CPropCountClkLinePrimitive = 4;
  CPropCountClkRectPrimitive = 4;
  CPropCountClkGradientFillPrimitive = 7;

  CClkPrimitivesTypeCounts: array[0..CPrimitiveTypeCount - 1] of Integer = (
    CPropCountClkSetPenPrimitive,
    CPropCountClkSetBrushPrimitive,
    CPropCountClkSetMiscPrimitive,
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

  CSetMiscPrimitiveProperties: array[0..CPropCountClkSetMiscPrimitive - 1] of TOIPropDef = (
    (Name: 'AntialiasingMode'; EditorType: etEnumCombo)
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
    (Name: 'X1'; EditorType: etSpinText),
    (Name: 'Y1'; EditorType: etSpinText),
    (Name: 'X2'; EditorType: etSpinText),
    (Name: 'Y2'; EditorType: etSpinText),
    (Name: 'Path'; EditorType: etFilePathWithArrow),
    (Name: 'Stretch'; EditorType: etBooleanCombo)
  );

  CLinePrimitiveProperties: array[0..CPropCountClkLinePrimitive - 1] of TOIPropDef = (
    (Name: 'X1'; EditorType: etSpinText),
    (Name: 'Y1'; EditorType: etSpinText),
    (Name: 'X2'; EditorType: etSpinText),
    (Name: 'Y2'; EditorType: etSpinText)
  );

  CRectPrimitiveProperties: array[0..CPropCountClkRectPrimitive - 1] of TOIPropDef = (
    (Name: 'X1'; EditorType: etSpinText),
    (Name: 'Y1'; EditorType: etSpinText),
    (Name: 'X2'; EditorType: etSpinText),
    (Name: 'Y2'; EditorType: etSpinText)
  );

  CGradientFillPrimitiveProperties: array[0..CPropCountClkGradientFillPrimitive - 1] of TOIPropDef = (
    (Name: 'X1'; EditorType: etSpinText),
    (Name: 'Y1'; EditorType: etSpinText),
    (Name: 'X2'; EditorType: etSpinText),
    (Name: 'Y2'; EditorType: etSpinText),
    (Name: 'StartColor'; EditorType: etColorCombo),
    (Name: 'StopColor'; EditorType: etColorCombo),
    (Name: 'Direction'; EditorType: etEnumCombo)
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
  CPrimitivesMainProperties: TArrayOfPropertiesArr = (
    @CSetPenPrimitiveProperties,
    @CSetBrushPrimitiveProperties,
    @CSetMiscPrimitiveProperties,
    @CSetFontPrimitiveProperties,
    @CImagePrimitiveProperties,
    @CLinePrimitiveProperties,
    @CRectPrimitiveProperties,
    @CGradientFillPrimitiveProperties
  );


{$IFDEF SubProperties}
  function GetActionValueStr_SetPen(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  function GetActionValueStr_SetBrush(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  function GetActionValueStr_SetMisc(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  function GetActionValueStr_SetFont(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  function GetActionValueStr_Image(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  function GetActionValueStr_Line(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  function GetActionValueStr_Rect(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  function GetActionValueStr_GradientFill(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
{$ENDIF}


procedure FillInDefaultValuesToPrimitive_SetPen(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_SetBrush(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_SetMisc(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_SetFont(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_Image(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_Line(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_Rect(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_GradientFill(var APrimitive: TPrimitiveRec);


type
  TFillInDefaultValuesToPrimitive = procedure(var APrimitive: TPrimitiveRec);
  TGetActionValueStr_Primitive = function(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;

const
  CFillInDefaultValuesToPrimitives: array[0..CPrimitiveTypeCount - 1] of TFillInDefaultValuesToPrimitive = (
    @FillInDefaultValuesToPrimitive_SetPen,
    @FillInDefaultValuesToPrimitive_SetBrush,
    @FillInDefaultValuesToPrimitive_SetMisc,
    @FillInDefaultValuesToPrimitive_SetFont,
    @FillInDefaultValuesToPrimitive_Image,
    @FillInDefaultValuesToPrimitive_Line,
    @FillInDefaultValuesToPrimitive_Rect,
    @FillInDefaultValuesToPrimitive_GradientFill
  );


  CGetActionValueStr_Primitive: array[0..CPrimitiveTypeCount - 1] of TGetActionValueStr_Primitive = (
    @GetActionValueStr_SetPen,
    @GetActionValueStr_SetBrush,
    @GetActionValueStr_SetMisc,
    @GetActionValueStr_SetFont,
    @GetActionValueStr_Image,
    @GetActionValueStr_Line,
    @GetActionValueStr_Rect,
    @GetActionValueStr_GradientFill
  );

const
  CSetPenEnumCounts: array[0..CPropCountClkSetPenPrimitive - 1] of Integer = (
    0,  //Color: string; //TColor;
    Ord(High(TPenStyle)) + 1, //Style: string; //TFPPenStyle;
    0, //Width: string; //Integer;
    Ord(High(TPenMode)) + 1, //Mode: string; //TFPPenMode;
    0, //Pattern: string; //LongWord;
    Ord(High(TPenEndCap)) + 1, //EndCap: string; //TFPPenEndCap;
    Ord(High(TPenJoinStyle)) + 1   //JoinStyle: string; //TFPPenJoinStyle;
  );

  CSetBrushEnumCounts: array[0..CPropCountClkSetBrushPrimitive - 1] of Integer = (
    0,  //Color: string; //TColor;
    Ord(High(TBrushStyle)) + 1, //Style: string; //TFPBrushStyle;
    0  //Pattern: string; //TBrushPattern;
  );

  CSetMiscEnumCounts: array[0..CPropCountClkSetMiscPrimitive - 1] of Integer = (
    Ord(High(TAntialiasingMode)) + 1  //Style: string; //TAntialiasingMode;
  );

  CSetFontEnumCounts: array[0..CPropCountClkSetFontPrimitive - 1] of Integer = ( // same as CFindControl_MatchBitmapTextEnumCounts;
    0, //ForegroundColor: string;
    0, //BackgroundColor: string;
    0, //FontName: string;
    0, //FontSize: Integer;
    0, //Bold: Boolean;
    0, //Italic: Boolean;
    0, //Underline: Boolean;
    0, //StrikeOut: Boolean;
    Ord(High(TFontQuality)) + 1,
    0, //FontQualityUsesReplacement: Boolean;
    0, //FontQualityReplacement: string;
    0, //ProfileName: string;
    0, //CropLeft: string;
    0, //CropTop: string;
    0, //CropRight: string;
    0  //CropBottom: string;
  );

  CImageEnumCounts: array[0..CPropCountClkImagePrimitive - 1] of Integer = (
    0,  //X1: string;
    0,  //X2: string;
    0,  //Y1: string;
    0,  //Y2: string;
    0,  //Path: string; //path to a bmp (or png) file, which will be part of the composition
    0   //Stretch: string; //Boolean
  );

  CLineEnumCounts: array[0..CPropCountClkLinePrimitive - 1] of Integer = (
    0,  //X1: string;
    0,  //X2: string;
    0,  //Y1: string;
    0   //Y2: string;
  );

  CRectEnumCounts: array[0..CPropCountClkRectPrimitive - 1] of Integer = (
    0,  //X1: string;
    0,  //X2: string;
    0,  //Y1: string;
    0   //Y2: string;
  );

  CGradientFillEnumCounts: array[0..CPropCountClkGradientFillPrimitive - 1] of Integer = (
    0,  //X1: string;
    0,  //X2: string;
    0,  //Y1: string;
    0,  //Y2: string;
    0,  //Y2: StartColor: string;
    0,  //Y2: StopColor: string;
    Ord(High(TGradientDirection)) + 1 //Direction: string; //TGradientDirection;
  );


  CPrimitivesPropEnumCounts: array[0..CPrimitiveTypeCount - 1] of PArrayOfEnumCounts = (
    @CSetPenEnumCounts,
    @CSetBrushEnumCounts,
    @CSetMiscEnumCounts,
    @CSetFontEnumCounts,
    @CImageEnumCounts,
    @CLineEnumCounts,
    @CRectEnumCounts,
    @CGradientFillEnumCounts
  );


implementation


{$IFDEF SubProperties}
  function GetActionValueStr_SetPen(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := APrimitive.ClkSetPen.Color; //TColor;
      1: Result := APrimitive.ClkSetPen.Style; //TFPPenStyle;
      2: Result := APrimitive.ClkSetPen.Width; //Integer;
      3: Result := APrimitive.ClkSetPen.Mode; //TFPPenMode;
      4: Result := APrimitive.ClkSetPen.Pattern; //LongWord;
      5: Result := APrimitive.ClkSetPen.EndCap; //TFPPenEndCap;
      6: Result := APrimitive.ClkSetPen.JoinStyle; //TFPPenJoinStyle;
      else
        Result := 'unknown';
    end;
  end;


  function GetActionValueStr_SetBrush(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := APrimitive.ClkSetBrush.Color; //TColor;
      1: Result := APrimitive.ClkSetBrush.Style; //TFPBrushStyle;
      2: Result := APrimitive.ClkSetBrush.Pattern; //TBrushPattern;
    end;
  end;


  function GetActionValueStr_SetMisc(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := APrimitive.ClkSetMisc.AntialiasingMode;
      else
        Result := 'unknown';
    end;
  end;


  function GetActionValueStr_SetFont(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := APrimitive.ClkSetFont.ForegroundColor;
      1: Result := APrimitive.ClkSetFont.BackgroundColor;
      2: Result := APrimitive.ClkSetFont.FontName;
      3: Result := IntToStr(APrimitive.ClkSetFont.FontSize);
      4: Result := BoolToStr(APrimitive.ClkSetFont.Bold, True);
      5: Result := BoolToStr(APrimitive.ClkSetFont.Italic, True);
      6: Result := BoolToStr(APrimitive.ClkSetFont.Underline, True);
      7: Result := BoolToStr(APrimitive.ClkSetFont.StrikeOut, True);
      8: Result := CFontQualityStr[APrimitive.ClkSetFont.FontQuality];
      9: Result := BoolToStr(APrimitive.ClkSetFont.FontQualityUsesReplacement, True);
      10: Result := APrimitive.ClkSetFont.FontQualityReplacement;
      11: Result := APrimitive.ClkSetFont.ProfileName;
      12: Result := APrimitive.ClkSetFont.CropLeft;
      13: Result := APrimitive.ClkSetFont.CropTop;
      14: Result := APrimitive.ClkSetFont.CropRight;
      15: Result := APrimitive.ClkSetFont.CropBottom;
      else
        Result := 'unknown';
    end;
  end;


  function GetActionValueStr_Image(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := APrimitive.ClkImage.X1;
      1: Result := APrimitive.ClkImage.Y1;
      2: Result := APrimitive.ClkImage.X2;
      3: Result := APrimitive.ClkImage.Y2;
      4: Result := APrimitive.ClkImage.Path;
      5: Result := APrimitive.ClkImage.Stretch;
      else
        Result := 'unknown';
    end;
  end;


  function GetActionValueStr_Line(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := APrimitive.ClkLine.X1;
      1: Result := APrimitive.ClkLine.Y1;
      2: Result := APrimitive.ClkLine.X2;
      3: Result := APrimitive.ClkLine.Y2;
      else
        Result := 'unknown';
    end;
  end;


  function GetActionValueStr_Rect(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := APrimitive.ClkRect.X1;
      1: Result := APrimitive.ClkRect.Y1;
      2: Result := APrimitive.ClkRect.X2;
      3: Result := APrimitive.ClkRect.Y2;
      else
        Result := 'unknown';
    end;
  end;


  function GetActionValueStr_GradientFill(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := APrimitive.ClkGradientFill.X1;
      1: Result := APrimitive.ClkGradientFill.Y1;
      2: Result := APrimitive.ClkGradientFill.X2;
      3: Result := APrimitive.ClkGradientFill.Y2;
      4: Result := APrimitive.ClkGradientFill.StartColor;
      5: Result := APrimitive.ClkGradientFill.StopColor;
      6: Result := APrimitive.ClkGradientFill.Direction;
      else
        Result := 'unknown';
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


procedure FillInDefaultValuesToPrimitive_SetMisc(var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkSetMisc.AntialiasingMode := '0';  //0 is the default = Doesn't care, which is set to antialiasing
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
  APrimitive.ClkImage.X1 := '14';
  APrimitive.ClkImage.Y1 := '15';
  APrimitive.ClkImage.X2 := '16';
  APrimitive.ClkImage.Y2 := '17';
  APrimitive.ClkImage.Path := '';
  APrimitive.ClkImage.Stretch := '0';
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
  APrimitive.ClkGradientFill.StartColor := '00FF33';
  APrimitive.ClkGradientFill.StopColor := '330099';
  APrimitive.ClkGradientFill.Direction := '0';
end;

end.

