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
  CCategoryCount = 3;

  CCategory_Primitives = 0;  //index of first category  - list of primitives, as added by user
  CCategory_Orders = 1;      //index of second category - list of composition orders for all above primitives
  CCategory_Settings = 2;    //index of third category - various composition settings

  CCategories: array[0..CCategoryCount - 1] of string = ('Primitives', 'Composition orders', 'Settings');

  CPropCountClkSetPenPrimitive = 6;
  CPropCountClkSetBrushPrimitive = 2;
  CPropCountClkSetMiscPrimitive = 1;
  CPropCountClkSetFontPrimitive = CPropCount_FindControlMatchBitmapText; //16;
  CPropCountClkImagePrimitive = 10;
  CPropCountClkLinePrimitive = 5;
  CPropCountClkRectPrimitive = 5;
  CPropCountClkGradientFillPrimitive = 7;
  CPropCountClkTextPrimitive = 3;
  CPropCountClkDonutSectorPrimitive = 12;

  CClkPrimitivesTypeCounts: array[0..CPrimitiveTypeCount - 1] of Integer = (
    CPropCountClkSetPenPrimitive,
    CPropCountClkSetBrushPrimitive,
    CPropCountClkSetMiscPrimitive,
    CPropCountClkSetFontPrimitive,
    CPropCountClkImagePrimitive,
    CPropCountClkLinePrimitive,
    CPropCountClkRectPrimitive,
    CPropCountClkGradientFillPrimitive,
    CPropCountClkTextPrimitive,
    CPropCountClkDonutSectorPrimitive
  );


  CSetPenPrimitiveProperties: array[0..CPropCountClkSetPenPrimitive - 1] of TOIPropDef = (
    (Name: 'Color'; EditorType: etColorCombo),
    (Name: 'Style'; EditorType: etTextWithArrow), //etEnumCombo
    (Name: 'Width'; EditorType: etSpinText),
    (Name: 'Mode'; EditorType: etTextWithArrow),  //etEnumCombo
    (Name: 'EndCap'; EditorType: etTextWithArrow),   //etEnumCombo
    (Name: 'JoinStyle'; EditorType: etTextWithArrow)  //etEnumCombo
  );

  CSetBrushPrimitiveProperties: array[0..CPropCountClkSetBrushPrimitive - 1] of TOIPropDef = (
    (Name: 'Color'; EditorType: etColorCombo),
    (Name: 'Style'; EditorType: etTextWithArrow)    //etEnumCombo
  );

  CSetMiscPrimitiveProperties: array[0..CPropCountClkSetMiscPrimitive - 1] of TOIPropDef = (
    (Name: 'AntialiasingMode'; EditorType: etTextWithArrow)   //etEnumCombo
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
    (Name: 'CropBottom'; EditorType: etSpinText),
    (Name: 'IgnoreBackgroundColor'; EditorType: etBooleanCombo)
  );

  CImagePrimitiveProperties: array[0..CPropCountClkImagePrimitive - 1] of TOIPropDef = (
    (Name: 'X1'; EditorType: etSpinText),
    (Name: 'Y1'; EditorType: etSpinText),
    (Name: 'X2'; EditorType: etSpinText),
    (Name: 'Y2'; EditorType: etSpinText),
    (Name: 'Path'; EditorType: etTextWithArrow),
    (Name: 'Stretch'; EditorType: etText),
    (Name: 'RenderedExternally'; EditorType: etText),
    (Name: 'Transparent'; EditorType: etText),
    (Name: 'TransparentMode'; EditorType: etText),
    (Name: 'TransparentColor'; EditorType: etText)
  );

  CLinePrimitiveProperties: array[0..CPropCountClkLinePrimitive - 1] of TOIPropDef = (
    (Name: 'X1'; EditorType: etSpinText),
    (Name: 'Y1'; EditorType: etSpinText),
    (Name: 'X2'; EditorType: etSpinText),
    (Name: 'Y2'; EditorType: etSpinText),
    (Name: 'ShowEndpointPixel'; EditorType: etText)
  );

  CRectPrimitiveProperties: array[0..CPropCountClkRectPrimitive - 1] of TOIPropDef = (
    (Name: 'X1'; EditorType: etSpinText),
    (Name: 'Y1'; EditorType: etSpinText),
    (Name: 'X2'; EditorType: etSpinText),
    (Name: 'Y2'; EditorType: etSpinText),
    (Name: 'ExtendToEndpointCorner'; EditorType: etText)
  );

  CGradientFillPrimitiveProperties: array[0..CPropCountClkGradientFillPrimitive - 1] of TOIPropDef = (
    (Name: 'X1'; EditorType: etSpinText),
    (Name: 'Y1'; EditorType: etSpinText),
    (Name: 'X2'; EditorType: etSpinText),
    (Name: 'Y2'; EditorType: etSpinText),
    (Name: 'StartColor'; EditorType: etColorCombo),
    (Name: 'StopColor'; EditorType: etColorCombo),
    (Name: 'Direction'; EditorType: etTextWithArrow)  //etEnumCombo
  );

  CTextPrimitiveProperties: array[0..CPropCountClkTextPrimitive - 1] of TOIPropDef = (
    (Name: 'Text'; EditorType: etText),
    (Name: 'X'; EditorType: etSpinText),
    (Name: 'Y'; EditorType: etSpinText)
  );

  CDonutSectorPrimitiveProperties: array[0..CPropCountClkDonutSectorPrimitive - 1] of TOIPropDef = (
    (Name: 'Cx'; EditorType: etSpinText),
    (Name: 'Cy'; EditorType: etSpinText),
    (Name: 'Radius1'; EditorType: etSpinText),
    (Name: 'Radius2'; EditorType: etSpinText),
    (Name: 'PointCount'; EditorType: etSpinText),
    (Name: 'StartAngle'; EditorType: etSpinText),
    (Name: 'EndAngle'; EditorType: etSpinText),
    (Name: 'AngleSpacing'; EditorType: etSpinText),
    (Name: 'StartColorFG'; EditorType: etColorCombo),
    (Name: 'EndColorFG'; EditorType: etColorCombo),
    (Name: 'StartColorBG'; EditorType: etColorCombo),
    (Name: 'EndColorBG'; EditorType: etColorCombo)
  );

  CSetPenPrimitive_Color_PropIndex = 0;
  CSetPenPrimitive_Style_PropIndex = 1;
  CSetPenPrimitive_Width_PropIndex = 2;
  CSetPenPrimitive_Mode_PropIndex = 3;
  CSetPenPrimitive_EndCap_PropIndex = 4;
  CSetPenPrimitive_JoinStyle_PropIndex = 5;

  CSetFontPrimitive_ForegroundColor_PropIndex = 0;
  CSetFontPrimitive_BackgroundColor_PropIndex = 1;
  CSetFontPrimitive_FontName_PropIndex = 2;

  CImagePrimitive_X1_PropIndex = 0;
  CImagePrimitive_Y1_PropIndex = 1;
  CImagePrimitive_X2_PropIndex = 2;
  CImagePrimitive_Y2_PropIndex = 3;
  CImagePrimitive_Path_PropIndex = 4;
  CImagePrimitive_Stretch_PropIndex = 5;
  CImagePrimitive_RenderedExternally_PropIndex = 6;
  CImagePrimitive_Transparent_PropIndex = 7;
  CImagePrimitive_TransparentMode_PropIndex = 8;
  CImagePrimitive_TransparentColor_PropIndex = 9;

  CLinePrimitive_X1_PropIndex = 0;
  CLinePrimitive_Y1_PropIndex = 1;
  CLinePrimitive_X2_PropIndex = 2;
  CLinePrimitive_Y2_PropIndex = 3;
  CLinePrimitive_ShowEndpointPixel_PropIndex = 4;

  CRectPrimitive_X1_PropIndex = 0;
  CRectPrimitive_Y1_PropIndex = 1;
  CRectPrimitive_X2_PropIndex = 2;
  CRectPrimitive_Y2_PropIndex = 3;
  CRectPrimitive_ExtendToEndpointCorner_PropIndex = 4;

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
    @CGradientFillPrimitiveProperties,
    @CTextPrimitiveProperties,
    @CDonutSectorPrimitiveProperties
  );

  CSettingsClkPropCount = 2;
  CSettingsNames: array[0..CSettingsClkPropCount - 1] of string = ('Compositor direction', 'Reserved');


{$IFDEF SubProperties}
  function GetPrimitiveValueStr_SetPen(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  function GetPrimitiveValueStr_SetBrush(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  function GetPrimitiveValueStr_SetMisc(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  function GetPrimitiveValueStr_SetFont(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  function GetPrimitiveValueStr_Image(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  function GetPrimitiveValueStr_Line(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  function GetPrimitiveValueStr_Rect(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  function GetPrimitiveValueStr_GradientFill(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  function GetPrimitiveValueStr_Text(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  function GetPrimitiveValueStr_DonutSector(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
{$ENDIF}


{$IFDEF SubProperties}
  procedure SetPrimitiveValueStr_SetPen(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
  procedure SetPrimitiveValueStr_SetBrush(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
  procedure SetPrimitiveValueStr_SetMisc(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
  procedure SetPrimitiveValueStr_SetFont(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
  procedure SetPrimitiveValueStr_Image(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
  procedure SetPrimitiveValueStr_Line(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
  procedure SetPrimitiveValueStr_Rect(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
  procedure SetPrimitiveValueStr_GradientFill(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
  procedure SetPrimitiveValueStr_Text(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
  procedure SetPrimitiveValueStr_DonutSector(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
{$ENDIF}


procedure FillInDefaultValuesToPrimitive_SetPen(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_SetBrush(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_SetMisc(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_SetFont(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_Image(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_Line(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_Rect(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_GradientFill(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_Text(var APrimitive: TPrimitiveRec);
procedure FillInDefaultValuesToPrimitive_DonutSector(var APrimitive: TPrimitiveRec);


type
  TFillInDefaultValuesToPrimitive = procedure(var APrimitive: TPrimitiveRec);
  TPrimitiveGetActionValueStrFunc = function(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  TPrimitiveSetActionValueStrProc = procedure(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);

const
  CFillInDefaultValuesToPrimitives: array[0..CPrimitiveTypeCount - 1] of TFillInDefaultValuesToPrimitive = (
    @FillInDefaultValuesToPrimitive_SetPen,
    @FillInDefaultValuesToPrimitive_SetBrush,
    @FillInDefaultValuesToPrimitive_SetMisc,
    @FillInDefaultValuesToPrimitive_SetFont,
    @FillInDefaultValuesToPrimitive_Image,
    @FillInDefaultValuesToPrimitive_Line,
    @FillInDefaultValuesToPrimitive_Rect,
    @FillInDefaultValuesToPrimitive_GradientFill,
    @FillInDefaultValuesToPrimitive_Text,
    @FillInDefaultValuesToPrimitive_DonutSector
  );


  CGetPrimitiveValueStrFunctions: array[0..CPrimitiveTypeCount - 1] of TPrimitiveGetActionValueStrFunc = (
    @GetPrimitiveValueStr_SetPen,
    @GetPrimitiveValueStr_SetBrush,
    @GetPrimitiveValueStr_SetMisc,
    @GetPrimitiveValueStr_SetFont,
    @GetPrimitiveValueStr_Image,
    @GetPrimitiveValueStr_Line,
    @GetPrimitiveValueStr_Rect,
    @GetPrimitiveValueStr_GradientFill,
    @GetPrimitiveValueStr_Text,
    @GetPrimitiveValueStr_DonutSector
  );


  CSetPrimitiveValueStrFunctions: array[0..CPrimitiveTypeCount - 1] of TPrimitiveSetActionValueStrProc = (
    @SetPrimitiveValueStr_SetPen,
    @SetPrimitiveValueStr_SetBrush,
    @SetPrimitiveValueStr_SetMisc,
    @SetPrimitiveValueStr_SetFont,
    @SetPrimitiveValueStr_Image,
    @SetPrimitiveValueStr_Line,
    @SetPrimitiveValueStr_Rect,
    @SetPrimitiveValueStr_GradientFill,
    @SetPrimitiveValueStr_Text,
    @SetPrimitiveValueStr_DonutSector
  );

const
  CSetPenEnumCounts: array[0..CPropCountClkSetPenPrimitive - 1] of Integer = (
    0,  //Color: string; //TColor;
    Ord(High(TPenStyle)) + 1, //Style: string; //TFPPenStyle;
    0, //Width: string; //Integer;
    Ord(High(TPenMode)) + 1, //Mode: string; //TFPPenMode;
    Ord(High(TPenEndCap)) + 1, //EndCap: string; //TFPPenEndCap;
    Ord(High(TPenJoinStyle)) + 1   //JoinStyle: string; //TFPPenJoinStyle;
  );

  CSetBrushEnumCounts: array[0..CPropCountClkSetBrushPrimitive - 1] of Integer = (
    0,  //Color: string; //TColor;
    Ord(High(TBrushStyle)) + 1  //Style: string; //TFPBrushStyle;
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
    0, //CropBottom: string;
    0  //IgnoreBackgroundColor: Boolean;
  );

  CImageEnumCounts: array[0..CPropCountClkImagePrimitive - 1] of Integer = (
    0,  //X1: string;
    0,  //X2: string;
    0,  //Y1: string;
    0,  //Y2: string;
    0,  //Path: string; //path to a bmp (or png) file, which will be part of the composition
    0,  //Stretch: string; //Boolean
    0,  //RenderedExternally: string; //Boolean
    0,  //Transparent: string; //
    0,  //TransparentMode: string; //
    0   //TransparentColor: string; //hexa
  );

  CLineEnumCounts: array[0..CPropCountClkLinePrimitive - 1] of Integer = (
    0,  //X1: string;
    0,  //X2: string;
    0,  //Y1: string;
    0,  //Y2: string;
    0   //ShowEndpointPixel: string;
  );

  CRectEnumCounts: array[0..CPropCountClkRectPrimitive - 1] of Integer = (
    0,  //X1: string;
    0,  //X2: string;
    0,  //Y1: string;
    0,  //Y2: string;
    0   //ExtendToEndpointCorner: string;
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

  CTextEnumCounts: array[0..CPropCountClkTextPrimitive - 1] of Integer = (
    0,  //Text: string;
    0,  //X: string;
    0   //Y: string;
  );

  CDonutSectorEnumCounts: array[0..CPropCountClkDonutSectorPrimitive - 1] of Integer = (
    0,  //Cx: string;          // Integer;
    0,  //Cy: string;          // Integer;
    0,  //Radius1: string;     // Integer;
    0,  //Radius2: string;     // Integer;
    0,  //PointCount: string;  // Integer;
    0,  //StartAngle: string;  // Extended;
    0,  //EndAngle: string;    // Extended;
    0,  //AngleSpacing: string;// Extended;
    0,  //StartColorFG: string;// TColor
    0,  //EndColorFG: string;  // TColor
    0,  //StartColorBG: string;// TColor
    0   //EndColorBG: string;  // TColor
  );


  CPrimitivesPropEnumCounts: array[0..CPrimitiveTypeCount - 1] of PArrayOfEnumCounts = (
    @CSetPenEnumCounts,
    @CSetBrushEnumCounts,
    @CSetMiscEnumCounts,
    @CSetFontEnumCounts,
    @CImageEnumCounts,
    @CLineEnumCounts,
    @CRectEnumCounts,
    @CGradientFillEnumCounts,
    @CTextEnumCounts,
    @CDonutSectorEnumCounts
  );


  CSetPenEnumStrings: array[0..CPropCountClkSetPenPrimitive - 1] of PArrayOfString = (
    nil, //Color: string; //TColor;
    @CPenStyleStr, //Style: string; //TFPPenStyle;
    nil, //Width: string; //Integer;
    @CPenModeStr, //Mode: string; //TFPPenMode;
    @CPenEndCapStr, //EndCap: string; //TFPPenEndCap;
    @CPenJoinStyleStr  //JoinStyle: string; //TFPPenJoinStyle;
  );

  CSetBrushEnumStrings: array[0..CPropCountClkSetBrushPrimitive - 1] of PArrayOfString = (
    nil, //Color: string; //TColor;
    @CBrushStyleStr  //Style: string; //TFPBrushStyle;
  );

  CSetMiscEnumStrings: array[0..CPropCountClkSetMiscPrimitive - 1] of PArrayOfString = (
    @CAntialiasingModeStr  //AntialiasingMode: string; //TAntialiasingMode
  );

  CSetFontEnumStrings: array[0..CPropCountClkSetFontPrimitive - 1] of PArrayOfString = (   //same as CFindControl_MatchBitmapTextEnumStrings
    nil, //ForegroundColor: string;
    nil, //BackgroundColor: string;
    nil, //FontName: string;
    nil, //FontSize: Integer;
    nil, //Bold: Boolean;
    nil, //Italic: Boolean;
    nil, //Underline: Boolean;
    nil, //StrikeOut: Boolean;
    @CFontQualityStr,
    nil, //FontQualityUsesReplacement: Boolean;
    nil, //FontQualityReplacement: string;
    nil, //ProfileName: string;
    nil, //CropLeft: string;
    nil, //CropTop: string;
    nil, //CropRight: string;
    nil, //CropBottom: string;
    nil  //IgnoreBackgroundColor: Boolean;
  );

  CImageEnumStrings: array[0..CPropCountClkImagePrimitive - 1] of PArrayOfString = (
    nil, //X1: string;
    nil, //X2: string;
    nil, //Y1: string;
    nil, //Y2: string;
    nil, //Path: string; //path to a bmp (or png) file, which will be part of the composition
    nil, //Stretch: string; //Boolean
    nil, //RenderedExternally: string; //Boolean
    nil, //Transparent: string; //
    nil, //TransparentMode: string; //
    nil  //TransparentColor: string; //
  );

  CLineEnumStrings: array[0..CPropCountClkLinePrimitive - 1] of PArrayOfString = (
    nil, //X1: string;
    nil, //X2: string;
    nil, //Y1: string;
    nil, //Y2: string;
    nil  //ShowEndpointPixel: string;
  );

  CRectEnumStrings: array[0..CPropCountClkRectPrimitive - 1] of PArrayOfString = (
    nil, //X1: string;
    nil, //X2: string;
    nil, //Y1: string;
    nil, //Y2: string;
    nil  //ExtendToEndpointCorner: string;
  );

  CGradientFillEnumStrings: array[0..CPropCountClkGradientFillPrimitive - 1] of PArrayOfString = (
    nil, //X1: string;
    nil, //X2: string;
    nil, //Y1: string;
    nil, //Y2: string;
    nil, //StartColor: string;
    nil, //StopColor: string;
    @CGradientDirectionStr  //Direction: string; //TGradientDirection;
  );

  CTextEnumStrings: array[0..CPropCountClkTextPrimitive - 1] of PArrayOfString = (
    nil, //Text: string;
    nil, //X: string;
    nil  //Y: string;
  );

  CDonutSectorEnumStrings: array[0..CPropCountClkDonutSectorPrimitive - 1] of PArrayOfString = (
    nil, //Cx: string;          // Integer;
    nil, //Cy: string;          // Integer;
    nil, //Radius1: string;     // Integer;
    nil, //Radius2: string;     // Integer;
    nil, //PointCount: string;  // Integer;
    nil, //StartAngle: string;  // Extended;
    nil, //EndAngle: string;    // Extended;
    nil, //AngleSpacing: string;// Extended;
    nil, //StartColorFG: string;// TColor
    nil, //EndColorFG: string;  // TColor
    nil, //StartColorBG: string;// TColor
    nil  //EndColorBG: string;  // TColor
  );

  CPrimitivesPropEnumStrings: array[0..CPrimitiveTypeCount - 1] of PArrayOfEnumStrings = (
    @CSetPenEnumStrings,
    @CSetBrushEnumStrings,
    @CSetMiscEnumStrings,
    @CSetFontEnumStrings,
    @CImageEnumStrings,
    @CLineEnumStrings,
    @CRectEnumStrings,
    @CGradientFillEnumStrings,
    @CTextEnumStrings,
    @CDonutSectorEnumStrings
  );


  ///// Settings
  CCompositorDirection_PropIndex = 0;

  CPrimitiveSettingsPropEnumCounts: array[0..CSettingsClkPropCount - 1] of Integer = (
    Ord(High(TCompositorDirection)) + 1,
    0
  );

  CPrimitiveSettingsPropEnumStrings: array[0..CSettingsClkPropCount - 1] of PArrayOfString = (
    @CCompositorDirectionStr,
    nil
  );


implementation


{$IFDEF SubProperties}
  function GetPrimitiveValueStr_SetPen(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := APrimitive.ClkSetPen.Color; //TColor;
      1: Result := APrimitive.ClkSetPen.Style; //TFPPenStyle;
      2: Result := APrimitive.ClkSetPen.Width; //Integer;
      3: Result := APrimitive.ClkSetPen.Mode; //TFPPenMode;
      4: Result := APrimitive.ClkSetPen.EndCap; //TFPPenEndCap;
      5: Result := APrimitive.ClkSetPen.JoinStyle; //TFPPenJoinStyle;
      else
        Result := 'unknown';
    end;
  end;


  function GetPrimitiveValueStr_SetBrush(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := APrimitive.ClkSetBrush.Color; //TColor;
      1: Result := APrimitive.ClkSetBrush.Style; //TFPBrushStyle;
      else
        Result := 'unknown';
    end;
  end;


  function GetPrimitiveValueStr_SetMisc(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := APrimitive.ClkSetMisc.AntialiasingMode;
      else
        Result := 'unknown';
    end;
  end;


  function GetPrimitiveValueStr_SetFont(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
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
      16: Result := BoolToStr(APrimitive.ClkSetFont.IgnoreBackgroundColor, True);
      else
        Result := 'unknown';
    end;
  end;


  function GetPrimitiveValueStr_Image(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := APrimitive.ClkImage.X1;
      1: Result := APrimitive.ClkImage.Y1;
      2: Result := APrimitive.ClkImage.X2;
      3: Result := APrimitive.ClkImage.Y2;
      4: Result := APrimitive.ClkImage.Path;
      5: Result := APrimitive.ClkImage.Stretch;
      6: Result := APrimitive.ClkImage.RenderedExternally;
      7: Result := APrimitive.ClkImage.Transparent;
      8: Result := APrimitive.ClkImage.TransparentMode;
      9: Result := APrimitive.ClkImage.TransparentColor;
      else
        Result := 'unknown';
    end;
  end;


  function GetPrimitiveValueStr_Line(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := APrimitive.ClkLine.X1;
      1: Result := APrimitive.ClkLine.Y1;
      2: Result := APrimitive.ClkLine.X2;
      3: Result := APrimitive.ClkLine.Y2;
      4: Result := APrimitive.ClkLine.ShowEndpointPixel;
      else
        Result := 'unknown';
    end;
  end;


  function GetPrimitiveValueStr_Rect(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := APrimitive.ClkRect.X1;
      1: Result := APrimitive.ClkRect.Y1;
      2: Result := APrimitive.ClkRect.X2;
      3: Result := APrimitive.ClkRect.Y2;
      4: Result := APrimitive.ClkRect.ExtendToEndpointCorner;
      else
        Result := 'unknown';
    end;
  end;


  function GetPrimitiveValueStr_GradientFill(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
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


  function GetPrimitiveValueStr_Text(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := APrimitive.ClkText.Text;
      1: Result := APrimitive.ClkText.X;
      2: Result := APrimitive.ClkText.Y;
      else
        Result := 'unknown';
    end;
  end;


  function GetPrimitiveValueStr_DonutSector(var APrimitive: TPrimitiveRec; APropertyIndex: Integer): string;
  begin
    case APropertyIndex of
      0: Result := APrimitive.ClkDonutSector.Cx;
      1: Result := APrimitive.ClkDonutSector.Cy;
      2: Result := APrimitive.ClkDonutSector.Radius1;
      3: Result := APrimitive.ClkDonutSector.Radius2;
      4: Result := APrimitive.ClkDonutSector.PointCount;
      5: Result := APrimitive.ClkDonutSector.StartAngle;
      6: Result := APrimitive.ClkDonutSector.EndAngle;
      7: Result := APrimitive.ClkDonutSector.AngleSpacing;
      8: Result := APrimitive.ClkDonutSector.StartColorFG;
      9: Result := APrimitive.ClkDonutSector.EndColorFG;
      10: Result := APrimitive.ClkDonutSector.StartColorBG;
      11: Result := APrimitive.ClkDonutSector.EndColorBG;
      else
        Result := 'unknown';
    end;
  end;
{$ENDIF}


{$IFDEF SubProperties}
  procedure SetPrimitiveValueStr_SetPen(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
  begin
    case APropertyIndex of
      0: APrimitive.ClkSetPen.Color := NewValue; //TColor;
      1: APrimitive.ClkSetPen.Style := NewValue; //TFPPenStyle;
      2: APrimitive.ClkSetPen.Width := NewValue; //Integer;
      3: APrimitive.ClkSetPen.Mode := NewValue; //TFPPenMode;
      4: APrimitive.ClkSetPen.EndCap := NewValue; //TFPPenEndCap;
      5: APrimitive.ClkSetPen.JoinStyle := NewValue; //TFPPenJoinStyle;
      else
        ;
    end;
  end;


  procedure SetPrimitiveValueStr_SetBrush(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
  begin
    case APropertyIndex of
      0: APrimitive.ClkSetBrush.Color := NewValue; //TColor;
      1: APrimitive.ClkSetBrush.Style := NewValue; //TFPBrushStyle;
      else
        ;
    end;
  end;


  procedure SetPrimitiveValueStr_SetMisc(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
  begin
    case APropertyIndex of
       0: APrimitive.ClkSetMisc.AntialiasingMode := NewValue;
      else
        ;
    end;
  end;


  procedure SetPrimitiveValueStr_SetFont(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
  begin
    case APropertyIndex of
      0: APrimitive.ClkSetFont.ForegroundColor := NewValue;
      1: APrimitive.ClkSetFont.BackgroundColor := NewValue;
      2: APrimitive.ClkSetFont.FontName := NewValue;
      3: APrimitive.ClkSetFont.FontSize := StrToIntDef(NewValue, 8);
      4: APrimitive.ClkSetFont.Bold := StrToBool(NewValue);
      5: APrimitive.ClkSetFont.Italic := StrToBool(NewValue);
      6: APrimitive.ClkSetFont.Underline := StrToBool(NewValue);
      7: APrimitive.ClkSetFont.StrikeOut := StrToBool(NewValue);
      8: APrimitive.ClkSetFont.FontQuality := FontQuality_AsStringToValue(NewValue);
      9: APrimitive.ClkSetFont.FontQualityUsesReplacement := StrToBool(NewValue);
      10: APrimitive.ClkSetFont.FontQualityReplacement := NewValue;
      11: APrimitive.ClkSetFont.ProfileName := NewValue;
      12: APrimitive.ClkSetFont.CropLeft := NewValue;
      13: APrimitive.ClkSetFont.CropTop := NewValue;
      14: APrimitive.ClkSetFont.CropRight := NewValue;
      15: APrimitive.ClkSetFont.CropBottom := NewValue;
      16: APrimitive.ClkSetFont.IgnoreBackgroundColor := StrToBool(NewValue);
      else
        ;
    end;
  end;


  procedure SetPrimitiveValueStr_Image(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
  begin
    case APropertyIndex of
      0: APrimitive.ClkImage.X1 := NewValue;
      1: APrimitive.ClkImage.Y1 := NewValue;
      2: APrimitive.ClkImage.X2 := NewValue;
      3: APrimitive.ClkImage.Y2 := NewValue;
      4: APrimitive.ClkImage.Path := NewValue;
      5: APrimitive.ClkImage.Stretch := NewValue;
      6: APrimitive.ClkImage.RenderedExternally := NewValue;
      7: APrimitive.ClkImage.Transparent := NewValue;
      8: APrimitive.ClkImage.TransparentMode := NewValue;
      9: APrimitive.ClkImage.TransparentColor := NewValue;
      else
        ;
    end;
  end;


  procedure SetPrimitiveValueStr_Line(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
  begin
    case APropertyIndex of
      0: APrimitive.ClkLine.X1 := NewValue;
      1: APrimitive.ClkLine.Y1 := NewValue;
      2: APrimitive.ClkLine.X2 := NewValue;
      3: APrimitive.ClkLine.Y2 := NewValue;
      4: APrimitive.ClkLine.ShowEndpointPixel := NewValue;
      else
        ;
    end;
  end;


  procedure SetPrimitiveValueStr_Rect(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
  begin
    case APropertyIndex of
      0: APrimitive.ClkRect.X1 := NewValue;
      1: APrimitive.ClkRect.Y1 := NewValue;
      2: APrimitive.ClkRect.X2 := NewValue;
      3: APrimitive.ClkRect.Y2 := NewValue;
      4: APrimitive.ClkRect.ExtendToEndpointCorner := NewValue;
      else
        ;
    end;
  end;


  procedure SetPrimitiveValueStr_GradientFill(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
  begin
    case APropertyIndex of
      0: APrimitive.ClkGradientFill.X1 := NewValue;
      1: APrimitive.ClkGradientFill.Y1 := NewValue;
      2: APrimitive.ClkGradientFill.X2 := NewValue;
      3: APrimitive.ClkGradientFill.Y2 := NewValue;
      4: APrimitive.ClkGradientFill.StartColor := NewValue;
      5: APrimitive.ClkGradientFill.StopColor := NewValue;
      6: APrimitive.ClkGradientFill.Direction := NewValue;
      else
        ;
    end;
  end;


  procedure SetPrimitiveValueStr_Text(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
  begin
    case APropertyIndex of
      0: APrimitive.ClkText.Text := NewValue;
      1: APrimitive.ClkText.X := NewValue;
      2: APrimitive.ClkText.Y := NewValue;
      else
        ;
    end;
  end;


  procedure SetPrimitiveValueStr_DonutSector(var APrimitive: TPrimitiveRec; NewValue: string; APropertyIndex: Integer);
  begin
    case APropertyIndex of
      0: APrimitive.ClkDonutSector.Cx := NewValue;
      1: APrimitive.ClkDonutSector.Cy := NewValue;
      2: APrimitive.ClkDonutSector.Radius1 := NewValue;
      3: APrimitive.ClkDonutSector.Radius2 := NewValue;
      4: APrimitive.ClkDonutSector.PointCount := NewValue;
      5: APrimitive.ClkDonutSector.StartAngle := NewValue;
      6: APrimitive.ClkDonutSector.EndAngle := NewValue;
      7: APrimitive.ClkDonutSector.AngleSpacing := NewValue;
      8: APrimitive.ClkDonutSector.StartColorFG := NewValue;
      9: APrimitive.ClkDonutSector.EndColorFG := NewValue;
      10: APrimitive.ClkDonutSector.StartColorBG := NewValue;
      11: APrimitive.ClkDonutSector.EndColorBG := NewValue;
      else
        ;
    end;
  end;
{$ENDIF}


procedure FillInDefaultValuesToPrimitive_SetPen(var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkSetPen.Color := '4444FF';
  APrimitive.ClkSetPen.Style := CPenStyleStr[psSolid];
  APrimitive.ClkSetPen.Width := '1';
  APrimitive.ClkSetPen.Mode := CPenModeStr[pmCopy];
  APrimitive.ClkSetPen.EndCap := CPenEndCapStr[pecRound];
  APrimitive.ClkSetPen.JoinStyle := CPenJoinStyleStr[pjsRound];
end;


procedure FillInDefaultValuesToPrimitive_SetBrush(var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkSetBrush.Color := '44FF33';
  APrimitive.ClkSetBrush.Style := CBrushStyleStr[bsSolid];
end;


procedure FillInDefaultValuesToPrimitive_SetMisc(var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkSetMisc.AntialiasingMode := CAntialiasingModeStr[amDontCare];  //0 is the default = Doesn't care, which is set to antialiasing
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
  APrimitive.ClkSetFont.IgnoreBackgroundColor := False;
end;


procedure FillInDefaultValuesToPrimitive_Image(var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkImage.X1 := '14';
  APrimitive.ClkImage.Y1 := '15';
  APrimitive.ClkImage.X2 := '16';
  APrimitive.ClkImage.Y2 := '17';
  APrimitive.ClkImage.Path := '';
  APrimitive.ClkImage.Stretch := '0';
  APrimitive.ClkImage.RenderedExternally := '0';
  APrimitive.ClkImage.Transparent := '0';
  APrimitive.ClkImage.TransparentMode := '0';
  APrimitive.ClkImage.TransparentColor := 'F0F0F0';
end;


procedure FillInDefaultValuesToPrimitive_Line(var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkLine.X1 := '14';
  APrimitive.ClkLine.Y1 := '15';
  APrimitive.ClkLine.X2 := '16';
  APrimitive.ClkLine.Y2 := '17';
  APrimitive.ClkLine.ShowEndpointPixel := '0'; //False
end;


procedure FillInDefaultValuesToPrimitive_Rect(var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkRect.X1 := '24';
  APrimitive.ClkRect.Y1 := '26';
  APrimitive.ClkRect.X2 := '27';
  APrimitive.ClkRect.Y2 := '29';
  APrimitive.ClkRect.ExtendToEndpointCorner := '0'; //False
end;


procedure FillInDefaultValuesToPrimitive_GradientFill(var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkGradientFill.X1 := '4';
  APrimitive.ClkGradientFill.Y1 := '5';
  APrimitive.ClkGradientFill.X2 := '16';
  APrimitive.ClkGradientFill.Y2 := '17';
  APrimitive.ClkGradientFill.StartColor := '00FF33';
  APrimitive.ClkGradientFill.StopColor := '330099';
  APrimitive.ClkGradientFill.Direction := CGradientDirectionStr[gdVertical];
end;


procedure FillInDefaultValuesToPrimitive_Text(var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkText.Text := '';
  APrimitive.ClkText.X := '50';
  APrimitive.ClkText.Y := '60';
end;


procedure FillInDefaultValuesToPrimitive_DonutSector(var APrimitive: TPrimitiveRec);
begin
  APrimitive.ClkDonutSector.Cx := '100';
  APrimitive.ClkDonutSector.Cy := '100';
  APrimitive.ClkDonutSector.Radius1 := '30';
  APrimitive.ClkDonutSector.Radius2 := '90';
  APrimitive.ClkDonutSector.PointCount := '40';
  APrimitive.ClkDonutSector.StartAngle := '-60.2';
  APrimitive.ClkDonutSector.EndAngle := '240.2';
  APrimitive.ClkDonutSector.AngleSpacing := '0.01';
  APrimitive.ClkDonutSector.StartColorFG := '1FFFFFFF';
  APrimitive.ClkDonutSector.EndColorFG := '1FFFFFFF';
  APrimitive.ClkDonutSector.StartColorBG := '008080'; //clOlive;
  APrimitive.ClkDonutSector.EndColorBG := '808000'; //clTeal;
end;

end.

