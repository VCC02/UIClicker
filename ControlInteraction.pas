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


unit ControlInteraction;

{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}


interface


uses
  {$IFDEF Windows}
    Windows,
  {$ELSE}
    LCLIntf, LCLType, Types,
  {$ENDIF}
  Graphics, Messages, BitmapProcessing, ExtCtrls, ClickerUtils;

type
  TMatchingMethod = (mmClass, mmText, mmBitmapText, mmBitmapFiles, mmPrimitiveFiles);
  TMatchingMethods = set of TMatchingMethod;

  TFindControlInputData = record
    ClassName, Text: string;
    ClassNameSeparator, TextSeparator: string;
    DebugBitmap: TBitmap; //image on debugging tab
    BitmapToSearchFor: TBitmap;
    BitmapToSearchOn: TBitmap; //used when not using Screenshot
    DebugGrid: TImage;
    MatchingMethods: TMatchingMethods;
    GlobalSearchArea: TRect;
    InitialRectangleOffsets: TRect;
    SearchAsSubControl: Boolean;
    StartSearchingWithCachedControl: Boolean;
    CachedControlLeft: Integer;
    CachedControlTop: Integer;
    ColorError: Integer;
    AllowedColorErrorCount: Integer;
    DebugTemplateName: string;
    GetAllHandles: Boolean;
    UseFastSearch: Boolean;
    FastSearchAllowedColorErrorCount: Integer;
    IgnoreBackgroundColor: Boolean;
    BackgroundColor: TColor;
    IgnoredColorsArr: TIntArr;
    SleepySearch: Byte;
    StopSearchOnMismatch: Boolean;
    ImageSource: TImageSource;
    OutsideTickCount: QWord;
    PrecisionTimeout: QWord;
    FullBackgroundImageInResult: Boolean;
    MatchByHistogramNumericSettings: TMatchByHistogramNumericSettings;
    CropFromScreenshot: Boolean;
    ThreadCount: Integer;

    OpenCLPath: string;
    GPUPlatformIndex: Integer;
    GPUDeviceIndex: Integer;
    GPUExecutionAvailability: TGPUExecutionAvailability;
    GPUIncludeDashG: Boolean; //when True, clBuildProgram is called with an additional build option, "-g". In case of an error, this allows getting detailed info. Var: $GPUIncludeDashG$
    GPUSlaveQueueFromDevice: Boolean; //when True, the SlaveQueue variable, from the SlideSearch kernel, is assigned in the kernel itself. Var: $SlaveQueueFromDevice$
    GPUUseAllKernelsEvent: Boolean; //when True, the SlideSearch kernel uses AllKernelsEvent variable, instead of AllEvents variable. Var: $UseAllKernelsEvent$
    GPUNdrangeNoLocalParam: Boolean; //when True, the ndrange_1D call is made with one argument (Global). By default, it is called with (Global, Local). Var: $NdrangeNoLocalParam$
    GPUUseEventsInEnqueueKernel: Boolean; //when True, enqueue_kernel is called with 3 additional arguments. By default, it is True, even if undefined. Var: $UseEventsInEnqueueKernel$
    GPUWaitForAllKernelsToBeDone: Boolean; //when True, the main kernel waits for all "generated" kernels to be done. By default, it is True, even if undefined. Var: $WaitForAllKernelsToBeDone$
    GPUReleaseFinalEventAtKernelEnd: Boolean; //when True, the release_event(FinalEvent) call, from the main kernel, is done once, at the end. Var: $ReleaseFinalEventAtKernelEnd$
    GPUIgnoreExecutionAvailability: Boolean; //when True, the ExecutionAvailability property is ignored. If a feature required (at least) a specific OpenCL version, and it is not available, an error should be displayed. Var: $IgnoreExecutionAvailability$
    GPUDbgBuffer: TIntArr; //debugging info / results
  end;


{$IFDEF Windows}
  procedure SetControlText(hw: THandle; NewText: string);
  procedure SelectComboBoxItem(hw: THandle; StartIndex: Integer; TextToSelect: string);
{$ELSE}
  procedure SetControlText(hw: TLCLHandle; NewText: string);
  procedure SelectComboBoxItem(hw: TLCLHandle; StartIndex: Integer; TextToSelect: string);
{$ENDIF}

procedure ComputeScreenshotArea(var InputData: TFindControlInputData; var CompAtPoint: TCompRec; out ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height, CompWidth, CompHeight: Integer);
procedure CroppedFullScreenShot(ACompAtPoint: TCompRec; AFindControlInputData: TFindControlInputData; ACompWidth, ACompHeight: Integer);

function MatchControlByBitmap(Algorithm: TMatchBitmapAlgorithm;
                              AlgorithmSettings: TMatchBitmapAlgorithmSettings;
                              CompAtPoint: TCompRec;
                              var InputData: TFindControlInputData;
                              out SubCnvXOffset, SubCnvYOffset, AResultedErrorCount: Integer;
                              var AFoundBitmaps: TCompRecArr;
                              AStopAllActionsOnDemand: PBoolean;
                              ADisplayGridLineOption: TDisplayGridLineOption): Boolean;

function FindControlOnScreen(InputData: TFindControlInputData;
                             AInitialTickCount: QWord;
                             AStopAllActionsOnDemand: PBoolean;
                             var AResultedControl: TCompRecArr;
                             ADisplayGridLineOption: TDisplayGridLineOption): Boolean;

function FindSubControlOnScreen(Algorithm: TMatchBitmapAlgorithm;
                             AlgorithmSettings: TMatchBitmapAlgorithmSettings;
                             var InputData: TFindControlInputData;
                             AInitialTickCount: QWord;
                             AStopAllActionsOnDemand: PBoolean;
                             var AResultedControl: TCompRecArr;
                             ADisplayGridLineOption: TDisplayGridLineOption): Boolean;

function FindWindowOnScreenByCaptionOrClass(InputData: TFindControlInputData; AStopAllActionsOnDemand: PBoolean; var AResultedControl: TCompRecArr): Boolean;
function FindWindowOnScreenByCaptionAndClass(InputData: TFindControlInputData; AStopAllActionsOnDemand: PBoolean; var AResultedControls: TCompRecArr): Boolean;


const
  CDebugBitmapBevelWidth = 10;//px  - "bevel" thickness around the searched bitmap
  CDebugBitmapBevelHeight = 10;//px


implementation


uses
  SysUtils, Classes, Forms, BinSearchValues, Math, BitmapConv
  , Clipbrd //for debugging only
  ;


{$IFDEF Windows}
  procedure SetControlText(hw: THandle; NewText: string);
  begin
    {if UseWideStringsOnGetControlText then
      SendMessage(hw, WM_SETTEXT, 0, PtrInt(PWideChar(NewText)))   //does not help on Wine
    else}
      SendMessage(hw, WM_SETTEXT, 0, {%H-}PtrInt(@NewText[1]));
  end;


  procedure SelectComboBoxItem(hw: THandle; StartIndex: Integer; TextToSelect: string);
  begin
    SendMessage(hw, CB_SELECTSTRING, StartIndex, {%H-}PtrInt(@TextToSelect[1]));
  end;
{$ELSE}
  const
    WM_SETTEXT = $0000000C;
    CB_SELECTSTRING = 333; //TBD
    WM_USER = $00000400;

  procedure SetControlText(hw: TLCLHandle; NewText: string);
  begin
    {if UseWideStringsOnGetControlText then
      SendMessage(hw, WM_SETTEXT, 0, PtrInt(PWideChar(NewText)))   //does not help on Wine
    else}
      SendMessage(hw, WM_SETTEXT, 0, {%H-}PtrInt(@NewText[1]));
  end;


  procedure SelectComboBoxItem(hw: TLCLHandle; StartIndex: Integer; TextToSelect: string);
  begin
    SendMessage(hw, CB_SELECTSTRING, StartIndex, {%H-}PtrInt(@TextToSelect[1]));
  end;
{$ENDIF}


function NumberOfSubStrings(sub, s: string): Integer;
var
  i, n: Integer;
begin
  Result := 0;
  if Pos(sub, s) = 0 then
    Exit;

  n := 0;
  repeat
    i := Pos(sub, s);
    if i > 0 then
    begin
      Inc(n);
      Delete(s, i, Length(sub));
    end;
  until i = 0;

  Result := n;
end;


function NumberOfItemsInText(AText: string; const Separator: Char): Integer;
var
  i: Integer;
  n: Integer;
  sep: Integer;
begin
  n := 0;
  sep := Length(Separator);

  if (Length(AText) = 0) or (Length(AText) < sep) then
  begin
    Result := 0;
    Exit;
  end;

  if Pos(Separator, AText) = 0 then
  begin
    Result := 1;
    Exit;
  end;

  if AText[1] <> Separator then
    AText := Separator + AText;

  if AText[Length(AText)] <> Separator then
    AText := AText + Separator;

  for i := 1 to Length(AText) do
    if AText[i] = Separator then
      Inc(n);

  if (AText[1] = Separator) or (AText[Length(AText)] = Separator) then
    Dec(n);

  Result := n;
end;

{The Text should be formatted like: 'Item1 Item2 Item3 Item4' .}
{Returns an item from "AText", indexed by "Item" .}
function GetItemFromText(AText: string; Item: Integer; const Separator: Char): string;
var
  BeginPos: Integer;
  EndPos: Integer;
  i: integer;
  n: integer;
  BeginSet: Boolean;
  EndSet: Boolean;
  sep: Integer;
begin
  n := 0;
  BeginPos := 1;
  EndPos := 1;
  BeginSet := False;
  EndSet := False;
  
  sep := Length(Separator);

  if AText > '' then
    if AText[1] <> Separator then
      AText := Separator + AText;
      
  if Copy(AText, Length(AText) - sep + 1, sep) <> Separator then
    AText := AText + Separator;

  for i := 1 to Length(AText) do
  begin
    if Copy(AText, i, sep) = Separator then
      Inc(n);

    if not BeginSet and (n = Item) then
    begin
      BeginPos := i;
      BeginSet := True;
    end;
      
    if not EndSet and (n = Item + 1) then
    begin
      EndPos := i;
      Break;
    end; //n = Item + 1
  end; //for

  Result := Copy(AText, BeginPos + 1, EndPos - BeginPos - 1);
end;


function StringMatchByWildCard(AString, AMatchWithWildCardsString: string): Boolean;
const
  NewSeparator: Char = #1;
var
  i: Integer;
  PiecesInAStringArr, PiecesInMatchWithWildCardArr: array of string;
begin
  Result := False;

  if (Pos('*', AMatchWithWildCardsString) = 0) and (AString <> AMatchWithWildCardsString) then
    Exit;

  try
    SetLength(PiecesInMatchWithWildCardArr, NumberOfItemsInText(AMatchWithWildCardsString, '*'));
    for i := 0 to Length(PiecesInMatchWithWildCardArr) - 1 do
      PiecesInMatchWithWildCardArr[i] := GetItemFromText(AMatchWithWildCardsString, i + 1, '*');

    for i := 0 to Length(PiecesInMatchWithWildCardArr) - 1 do
    begin
      AString := StringReplace(AString, PiecesInMatchWithWildCardArr[i], NewSeparator, [rfReplaceAll]); //at this point, only pieces matched by wildcards should exist here
      AMatchWithWildCardsString := StringReplace(AMatchWithWildCardsString, PiecesInMatchWithWildCardArr[i], NewSeparator, [rfReplaceAll]);
    end;


    if NumberOfSubStrings(NewSeparator, AString) <> NumberOfSubStrings(NewSeparator, AMatchWithWildCardsString) then
      Exit;

    SetLength(PiecesInAStringArr, NumberOfItemsInText(AString, NewSeparator));

    for i := 0 to Length(PiecesInAStringArr) - 1 do
      PiecesInAStringArr[i] := GetItemFromText(AString, i + 1, NewSeparator);

    for i := 0 to Length(PiecesInAStringArr) - 1 do
      AString := StringReplace(AString, PiecesInAStringArr[i], '*', [rfReplaceAll]);

    Result := AString = AMatchWithWildCardsString;
  finally
    SetLength(PiecesInMatchWithWildCardArr, 0);
    SetLength(PiecesInAStringArr, 0);
  end;
end;


function ClassNameOrCaptionInListOfPatternedStrings(ClassNameOrCaption: string; AStringList: TStringList): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to AStringList.Count - 1 do
    if StringMatchByWildCard(ClassNameOrCaption, AStringList.Strings[i]) then
    begin
      Result := True;
      Exit;
    end;  
end;


type
  TFindWindowByPatternObj = class(TObject)
    ListOfControlTexts, ListOfControlClasses: TStringList;
    ResultedControl: TCompRecArr;
    MatchingMethods: TMatchingMethods;
    FoundByEnum: Boolean;
    FindAllControls: Boolean;
  end;

threadvar
  FindWindowByPatternObj: TFindWindowByPatternObj;  


//Callback as required by EnumWindows
function EnumWindowsProc(hwn: THandle; ALParam: LPARAM): BOOL; stdcall;
var
  hwc: TCompRec;
  //FindWindowByPatterObj: TFindWindowByPatternObj;
  Found: Boolean;
begin
  //FindWindowByPatterObj := TFindWindowByPatternObj(ALParam);
  //MessageBox(0, PChar('LParam: ' + IntToStr(ALParam)), 'EnumWindowsProc', MB_ICONINFORMATION);
  hwc := GetWindowClassRec(hwn);

  Found := True;

  if (mmClass in FindWindowByPatternObj.MatchingMethods) then
  begin
    if FindWindowByPatternObj.ListOfControlClasses.Count > 0 then
      Found := Found and (ClassNameOrCaptionInListOfPatternedStrings(hwc.ClassName, FindWindowByPatternObj.ListOfControlClasses))
    else
      Found := Found and (hwc.ClassName = '');
  end;

  if (mmText in FindWindowByPatternObj.MatchingMethods) then
  begin
    if FindWindowByPatternObj.ListOfControlTexts.Count > 0 then
      Found := Found and (ClassNameOrCaptionInListOfPatternedStrings(hwc.Text, FindWindowByPatternObj.ListOfControlTexts))
    else
      Found := Found and (hwc.Text = '');
  end;

  if Found then
  begin
    SetLength(FindWindowByPatternObj.ResultedControl, Length(FindWindowByPatternObj.ResultedControl) + 1);
    FindWindowByPatternObj.ResultedControl[Length(FindWindowByPatternObj.ResultedControl) - 1] := hwc;
  end;

  FindWindowByPatternObj.FoundByEnum := Found or (Length(FindWindowByPatternObj.ResultedControl) > 0);
  Result := not Found; //if not Found, then continue searching  (a.k.a. continue calling this callback)         //To continue enumeration, the callback function must return TRUE; to stop enumeration, it must return FALSE.
  //MSDN Doc: To continue enumeration, the callback function must return TRUE; to stop enumeration, it must return FALSE.

  if FindWindowByPatternObj.FindAllControls then
    Result := True;
end;


//uses '*' as wildcard
function FindWindowByCaptionOrClassPattern(AListOfControlTexts, AListOfControlClasses: TStringList; AMatchingMethods: TMatchingMethods; AFindAllControls: Boolean; var hwc: TCompRecArr): Boolean;
var
  i: Integer;
begin
  FindWindowByPatternObj := TFindWindowByPatternObj.Create;
  try
    FindWindowByPatternObj.FoundByEnum := False;
    FindWindowByPatternObj.ListOfControlTexts := AListOfControlTexts;
    FindWindowByPatternObj.ListOfControlClasses := AListOfControlClasses;
    FindWindowByPatternObj.MatchingMethods := AMatchingMethods;
    FindWindowByPatternObj.FindAllControls := AFindAllControls;
    SetLength(FindWindowByPatternObj.ResultedControl, 0);

    {$IFDEF Windows}
      //MessageBox(0, PChar('LParam: ' + IntToStr(LPARAM(FindWindowByPatterObj))), 'FindWindowByCaptionOrClassPattern', MB_ICONINFORMATION);
      EnumWindows(@EnumWindowsProc, LPARAM(FindWindowByPatternObj));
    {$ELSE}
      //nothing for Linux :(
    {$ENDIF}

    Result := FindWindowByPatternObj.FoundByEnum;

    if Result then
    begin
      SetLength(hwc, Length(FindWindowByPatternObj.ResultedControl));
      for i := 0 to Length(hwc) - 1 do
        hwc[i] := FindWindowByPatternObj.ResultedControl[i];
    end;
  finally
    FindWindowByPatternObj.Free;
  end;
end;


function FindWindowOnScreenByCaptionOrClass(InputData: TFindControlInputData; AStopAllActionsOnDemand: PBoolean; var AResultedControl: TCompRecArr): Boolean;
var
  ListOfControlTexts, ListOfControlClasses: TStringList;
begin
  ListOfControlTexts := TStringList.Create;
  ListOfControlClasses := TStringList.Create;
  try
    ListOfControlTexts.LineBreak := #13#10;
    ListOfControlClasses.LineBreak := #13#10;
    ListOfControlTexts.Text := StringReplace(InputData.Text, InputData.TextSeparator, #13#10, [rfReplaceAll]);             //no fast replace here :( , because the separator can be anything
    ListOfControlClasses.Text := StringReplace(InputData.ClassName, InputData.ClassNameSeparator, #13#10, [rfReplaceAll]); //no fast replace here :( , because the separator can be anything

    Result := FindWindowByCaptionOrClassPattern(ListOfControlTexts, ListOfControlClasses, InputData.MatchingMethods, InputData.GetAllHandles, AResultedControl);
  finally
    ListOfControlTexts.Free;
    ListOfControlClasses.Free;
  end;
end;



//uses '*' as wildcard
function FindWindowByCaptionAndClassPattern(AListOfControlTexts, AListOfControlClasses: TStringList; AMatchingMethods: TMatchingMethods; AGetAllHandles: Boolean; var AResultedControls: TCompRecArr): Boolean;
var
  i, j: Integer;
  AHandle: THandle;
begin
  Result := False;

  SetLength(AResultedControls, 0);
  for i := 0 to AListOfControlTexts.Count - 1 do
    for j := 0 to AListOfControlClasses.Count - 1 do
    begin
      {$IFDEF Windows}
        AHandle := FindWindow(PChar(AListOfControlClasses.Strings[j]), PChar(string(AListOfControlTexts.Strings[i])));
      {$ELSE}
        AHandle := 0; //maybe there is something for Linux
      {$ENDIF}

      if AHandle > 0 then
      begin
        SetLength(AResultedControls, Length(AResultedControls) + 1);
        AResultedControls[Length(AResultedControls) - 1] := GetWindowClassRec(AHandle);

        Result := True;

        if not AGetAllHandles then
          Exit;
      end;
    end;

  if Length(AResultedControls) = 0 then
  begin
    SetLength(AResultedControls, 1);
    AResultedControls[0].Handle := 0;
    AResultedControls[0].ClassName := '';
    AResultedControls[0].Text := '';
    AResultedControls[0].ComponentRectangle.Left := 0;
    AResultedControls[0].ComponentRectangle.Top := 0;
    AResultedControls[0].ComponentRectangle.Width := 0;
    AResultedControls[0].ComponentRectangle.Height := 0;
  end;
end;


function FindWindowOnScreenByCaptionAndClass(InputData: TFindControlInputData; AStopAllActionsOnDemand: PBoolean; var AResultedControls: TCompRecArr): Boolean;
var
  ListOfControlTexts, ListOfControlClasses: TStringList;
begin
  ListOfControlTexts := TStringList.Create;
  ListOfControlClasses := TStringList.Create;
  try
    ListOfControlTexts.LineBreak := #13#10;
    ListOfControlClasses.LineBreak := #13#10;
    ListOfControlTexts.Text := StringReplace(InputData.Text, InputData.TextSeparator, #13#10, [rfReplaceAll]);             //no fast replace here :( , because the separator can be anything
    ListOfControlClasses.Text := StringReplace(InputData.ClassName, InputData.ClassNameSeparator, #13#10, [rfReplaceAll]); //no fast replace here :( , because the separator can be anything

    Result := FindWindowByCaptionAndClassPattern(ListOfControlTexts, ListOfControlClasses, InputData.MatchingMethods, InputData.GetAllHandles, AResultedControls);
  finally
    ListOfControlTexts.Free;
    ListOfControlClasses.Free;
  end;
end;


function ControlIsInListByInfo(AControlInfo: TCompRec; var AvailableControls: TCompRecArr): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(AvailableControls) - 1 do
    if (AvailableControls[i].ClassName = AControlInfo.ClassName) and (AvailableControls[i].Handle = AControlInfo.Handle) then
    begin
      Result := True;
      Exit;
    end;
end;


procedure DisplayDebugBmpForSuccessfulMatch(BitmapToSearchFor, DebugBmp: TBitmap; ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height: Integer; SubCnvXOffset, SubCnvYOffset: Integer; var AFoundBitmaps: TCompRecArr; ATransparentFoundSelection, AFullBackgroundImageInResult: Boolean);
var
  DebugDisplayLeft: Integer; //This is the value backup of the debug screenshot width, before increasing it. Debug information is displayed starting at this value
  BmpWithFoundSelection: TBitmap;
  i: Integer;
  TempBmp: TBitmap;

  procedure DrawFoundSelection;
  begin
    DebugBmp.Canvas.Pen.Color := clRed;
    Line(DebugBmp.Canvas, SubCnvXOffset, 0, SubCnvXOffset, DebugBmp.Height);   //vert
    Line(DebugBmp.Canvas, SubCnvXOffset + BitmapToSearchFor.Width - 1, 0, SubCnvXOffset + BitmapToSearchFor.Width - 1, DebugBmp.Height);  //vert
    Line(DebugBmp.Canvas, 0, SubCnvYOffset, DebugDisplayLeft, SubCnvYOffset);   //horiz
    Line(DebugBmp.Canvas, 0, SubCnvYOffset + BitmapToSearchFor.Height - 1, DebugDisplayLeft, SubCnvYOffset + BitmapToSearchFor.Height - 1); //horiz
  end;

  procedure DrawFoundSelectionBand(AIndex: Integer);
  const
    CBandThickness = 5;
  var
    SubX, SubY: Integer;
    HRectLeft, VRectTop, HRectRight, VRectBottom: TRect;
  begin
    SubX := AFoundBitmaps[AIndex].XOffsetFromParent;
    SubY := AFoundBitmaps[AIndex].YOffsetFromParent;

    DebugBmp.Canvas.Pen.Color := $00AA44;
    DebugBmp.Canvas.Brush.Color := $44FF88;
    DebugBmp.Canvas.Brush.Style := bsSolid;

    HRectLeft.Left := SubX - CBandThickness;
    HRectLeft.Top := SubY;
    HRectLeft.Right := SubX {- 1};
    HRectLeft.Bottom := SubY + BitmapToSearchFor.Height;

    VRectTop.Left := SubX;
    VRectTop.Top := SubY - CBandThickness;
    VRectTop.Right := SubX + BitmapToSearchFor.Width;
    VRectTop.Bottom := SubY {- 1};

    HRectRight.Left := SubX + BitmapToSearchFor.Width {+ 1};
    HRectRight.Top := SubY;
    HRectRight.Right := SubX + BitmapToSearchFor.Width + CBandThickness;
    HRectRight.Bottom := SubY + BitmapToSearchFor.Height;

    VRectBottom.Left := SubX;
    VRectBottom.Top := SubY + BitmapToSearchFor.Height {+ 1};
    VRectBottom.Right := SubX + BitmapToSearchFor.Width;
    VRectBottom.Bottom := SubY + BitmapToSearchFor.Height + CBandThickness;

    DebugBmp.Canvas.Rectangle(HRectLeft);
    DebugBmp.Canvas.Rectangle(VRectTop);
    DebugBmp.Canvas.Rectangle(HRectRight);
    DebugBmp.Canvas.Rectangle(VRectBottom);
  end;

var
  DestRect, SrcRect: TRect;
begin
  DebugDisplayLeft := DebugBmp.Width;  //yes, width

  DebugBmp.Width := Max(DebugBmp.Width, 400); //use Max to allow displaying text
  DebugBmp.Height := Max(DebugBmp.Height, 100); //use Max to allow displaying text

  DebugBmp.Canvas.Brush.Style := bsClear;
  DebugBmp.Canvas.Pen.Color := clLime;
  DebugBmp.Canvas.Pen.Style := psDot;
  DebugBmp.Canvas.Rectangle(ScrShot_Left, ScrShot_Top, ScrShot_Left + ScrShot_Width, ScrShot_Top + ScrShot_Height);
  DebugBmp.Canvas.Pen.Style := psSolid;

  if ATransparentFoundSelection then
  begin
    BmpWithFoundSelection := TBitmap.Create;
    try
      BmpWithFoundSelection.Assign(DebugBmp);
      DrawFoundSelection;
      AvgBitmapWithBitmap(DebugBmp, BmpWithFoundSelection, DebugBmp);
    finally
      BmpWithFoundSelection.Free;
    end;
  end
  else
    DrawFoundSelection;

  if Length(AFoundBitmaps) > 1 then
  begin
    BmpWithFoundSelection := TBitmap.Create;
    try
      BmpWithFoundSelection.Assign(DebugBmp);

      for i := 0 to Length(AFoundBitmaps) - 1 do
        DrawFoundSelectionBand(i);

      AvgBitmapWithBitmap(DebugBmp, BmpWithFoundSelection, DebugBmp);
    finally
      BmpWithFoundSelection.Free;
    end;
  end;

  if Length(AFoundBitmaps) = 1 then   //So far, do this for a single result only. This should get the maximum width and height values from all bitmaps in AFoundBitmaps array.
    if not AFullBackgroundImageInResult then
    begin
      TempBmp := TBitmap.Create;
      try
        TempBmp.SetSize(BitmapToSearchFor.Width + CDebugBitmapBevelWidth shl 1, BitmapToSearchFor.Height + CDebugBitmapBevelHeight shl 1);

        SrcRect.Left := SubCnvXOffset - CDebugBitmapBevelWidth;
        SrcRect.Top := SubCnvYOffset - CDebugBitmapBevelWidth;
        SrcRect.Width := BitmapToSearchFor.Width + CDebugBitmapBevelWidth shl 1;
        SrcRect.Height := BitmapToSearchFor.Height + CDebugBitmapBevelHeight shl 1;

        DestRect.Left := 0;
        DestRect.Top := 0;
        DestRect.Width := SrcRect.Width;
        DestRect.Height := SrcRect.Height;

        TempBmp.Canvas.CopyRect(DestRect, DebugBmp.Canvas, SrcRect);

        DebugBmp.Clear;
        DebugBmp.SetSize(DestRect.Width, DestRect.Height);
        DebugBmp.Assign(TempBmp);
      finally
        TempBmp.Free;
      end;
    end;
end;


procedure DisplayDebugBmpForFailedMatch(BitmapToSearchFor, SrcCompSearchAreaBitmap, DebugBmp: TBitmap;
                                        ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height: Integer;
                                        SubCnvXOffset, SubCnvYOffset: Integer;
                                        AMatchingMethods: TMatchingMethods;
                                        DebugTemplateName, SearchedText: string;
                                        AUsingFastSearch, AIgnoreBackgroundColor, AFullBackgroundImageInResult: Boolean;
                                        ABackgroundColor: TColor);
var
  DebugDisplayLeft: Integer; //This is the value backup of the debug screenshot width, before increasing it. Debug information is displayed starting at this value
  DebugDrawingX, DebugDrawingY: Integer;
  TempWidth, TempHeight: Integer;
  TempBmp: TBitmap;
  DebugInfoBrushStyle: TBrushStyle;
begin
  TempWidth := DebugBmp.Width;
  TempHeight := DebugBmp.Height;

  if not AFullBackgroundImageInResult then
  begin
    DebugDisplayLeft := ScrShot_Left + ScrShot_Width;
    DebugInfoBrushStyle := bsSolid;
    TempWidth := DebugDisplayLeft + 50 + DebugBmp.Canvas.TextWidth(DebugTemplateName);
    TempHeight := Max(130, ScrShot_Top + ScrShot_Height);
  end
  else
  begin
    DebugDisplayLeft := TempWidth;  //yes, width
    DebugInfoBrushStyle := bsClear;
  end;

  TempBmp := TBitmap.Create;
  try
    //The following two lines are very slow, so use a temporary bitmap (TempBmp) to resize.
    //DebugBmp.Width := DebugDisplayLeft + Max(TempWidth, 400);  //400.. for long filenames
    //DebugBmp.Height := Max(Max(TempHeight + 50, BitmapToSearchFor.Height shl 1 + 80), 100); //use Max to allow displaying text

    TempBmp.SetSize(DebugDisplayLeft + Max(TempWidth, 400), Max(Max(TempHeight + 50, BitmapToSearchFor.Height shl 1 + 80), 100)); //Width: 400.. for long filenames,   Height: use Max to allow displaying text
    TempBmp.Canvas.Draw(0, 0, DebugBmp);
    DebugBmp.Clear;
    DebugBmp.Width := TempBmp.Width;
    DebugBmp.Height := TempBmp.Height;
    DebugBmp.Assign(TempBmp);
  finally
    TempBmp.Free;
  end;

  if DebugBmp.Width < 1 then //it may happen, if DebugDisplayLeft is negative
    DebugBmp.Width := 1;

  if DebugBmp.Height < 1 then //it should not happen, from above code
    DebugBmp.Height := 1;

  DebugDrawingX := DebugDisplayLeft + 10;

  TempWidth := BitmapToSearchFor.Width;   //reusing TempWidth var
  TempHeight := BitmapToSearchFor.Height; //reusing TempHeight var

  if mmBitmapText in AMatchingMethods then
  begin
    DebugBmp.Transparent := False;
    DebugBmp.Canvas.Brush.Style := DebugInfoBrushStyle;
    DebugBmp.Canvas.Font.Color := $2244FF;//clRed;
    DebugBmp.Canvas.TextOut(DebugDrawingX, 0, 'Generated by: ' + DebugTemplateName);
    DebugBmp.Canvas.TextOut(DebugDrawingX, 15, 'Text not found: ' + SearchedText);

    DebugBmp.Canvas.Brush.Style := DebugInfoBrushStyle;
    DebugBmp.Canvas.TextOut(DebugDrawingX, 30 + TempHeight + 10, 'BitmapToSearchFor w/h: ' + IntToStr(TempWidth) + ' x ' + IntToStr(TempHeight));
    DebugBmp.Canvas.TextOut(DebugDrawingX, 30 + TempHeight + 25, 'PixelFormat: ' + IntToStr(PIXELFORMAT_BPP[BitmapToSearchFor.PixelFormat]) + '-bit');
    DebugBmp.Canvas.TextOut(DebugDrawingX, 30 + TempHeight + 40, 'Using FastSearch: ' + BoolToStr(AUsingFastSearch, 'Yes', 'No'));
    DebugBmp.Canvas.TextOut(DebugDrawingX, 30 + TempHeight + 55, 'Using IgnoreBackgroundColor: ' + BoolToStr(AIgnoreBackgroundColor, 'Yes', 'No'));
    DebugBmp.Canvas.TextOut(DebugDrawingX, 30 + TempHeight + 70, 'BackgroundColor: ' + IntToHex(ABackgroundColor, 8));

    DebugBmp.Canvas.Pen.Color := clRed;
    DebugBmp.Canvas.Rectangle(DebugDrawingX - 1, 30 - 1, DebugDrawingX + TempWidth + 1, 30 - 1 + TempHeight + 2);

    DebugBmp.Canvas.Brush.Style := bsSolid;
    DebugBmp.Canvas.Brush.Color := ABackgroundColor;
    DebugBmp.Canvas.Rectangle(DebugDrawingX + 200, 30 + TempHeight + 55, DebugDrawingX + 230, 30 + TempHeight + 70 + 13);
    DebugBmp.Canvas.Brush.Color := clBlack;

    DebugBmp.Canvas.Brush.Style := bsSolid;
    DebugBmp.Canvas.Draw(DebugDrawingX, 30, BitmapToSearchFor);
  end;

  if (mmBitmapFiles in AMatchingMethods) or
     (mmPrimitiveFiles in AMatchingMethods) then
  begin
    DebugBmp.Canvas.Brush.Style := DebugInfoBrushStyle;
    DebugBmp.Canvas.Font.Color := clRed;
    DebugBmp.Canvas.TextOut(DebugDrawingX, 0, 'Generated by: ' + DebugTemplateName);
    DebugBmp.Canvas.TextOut(DebugDrawingX, 15, 'Bitmap not found:');
    DebugBmp.Canvas.Draw(DebugDrawingX, 30, BitmapToSearchFor);

    DebugBmp.Canvas.Pen.Color := clRed;
    DebugBmp.Canvas.Rectangle(DebugDrawingX - 1, 30 - 1, DebugDrawingX + TempWidth + 1, 30 - 1 + TempHeight + 2);

    DebugBmp.Canvas.TextOut(DebugDrawingX, 30 + TempHeight + 5, 'In:');
    DebugDrawingY := 30 + TempHeight + 5 + DebugBmp.Canvas.TextHeight('In:');
    DebugBmp.Canvas.Draw(DebugDrawingX, DebugDrawingY, SrcCompSearchAreaBitmap);

    //draw a green dotted rectangle around search area, displayed under "In:' section
    DebugBmp.Canvas.Brush.Style := bsClear; //this should stay bsClear
    DebugBmp.Canvas.Pen.Color := clLime;
    DebugBmp.Canvas.Pen.Style := psDot;
    DebugBmp.Canvas.Rectangle(DebugDrawingX, DebugDrawingY, DebugDrawingX + SrcCompSearchAreaBitmap.Width, DebugDrawingY + SrcCompSearchAreaBitmap.Height);
    DebugBmp.Canvas.Pen.Style := psSolid;
  end;

  DebugBmp.Canvas.Brush.Style := bsClear;   //this should stay bsClear
  DebugBmp.Canvas.Pen.Color := $000080FF;
  DebugBmp.Canvas.Pen.Style := psDot;
  DebugBmp.Canvas.Rectangle(ScrShot_Left, ScrShot_Top, ScrShot_Left + ScrShot_Width, ScrShot_Top + ScrShot_Height);

  {}  //for debuging
  //DebugBmp.Canvas.Pen.Color := clAqua;
  //DebugBmp.Canvas.Rectangle(ScrShot_Left - 1, ScrShot_Top - 1, ScrShot_Left + ScrShot_Width + 1, ScrShot_Top + ScrShot_Height + 1);
  {}

  DebugBmp.Canvas.Pen.Style := psSolid;
end;


procedure DisplayDebugGrid(Algorithm: TMatchBitmapAlgorithm; AlgorithmSettings: TMatchBitmapAlgorithmSettings; DebugGrid: TImage; ScrShot_Width, ScrShot_Height: Integer; ADisplayGridLineOption: TDisplayGridLineOption);
var
  TempImg: TImage;
begin
  WipeImage(DebugGrid, ScrShot_Width, ScrShot_Height);

  DebugGrid.Hint := 'Width = ' + IntToStr(ScrShot_Width) + '  Height = ' + IntToStr(ScrShot_Height) + '  Algorithm = ' + IntToStr(Ord(Algorithm));

  if Algorithm = mbaBruteForce then
  begin
    DebugGrid.Hint := DebugGrid.Hint + #13#10 + 'Nothing to display as debug grid image, when using bruteforce search.';

    DebugGrid.Canvas.Pen.Color := $00C9AFFF;  //pink2
    Line(DebugGrid.Canvas, DebugGrid.Width - 2, DebugGrid.Height - 2, DebugGrid.Width - 1, DebugGrid.Height - 2); //one pixel near the corner, to have some content. This affects transparency.

    Exit;
  end;

  TempImg := TImage.Create(nil);   //using a temp image, because DrawSearchGrid does not use offsets anymore
  try
    TempImg.AutoSize := False;
    WipeImage(TempImg, ScrShot_Width, ScrShot_Height);

    WipeImage(DebugGrid, ScrShot_Width, ScrShot_Height);
    DrawSearchGrid(TempImg, AlgorithmSettings, ScrShot_Width, ScrShot_Height, $00C9AEFF, ADisplayGridLineOption); //pink
    DebugGrid.Canvas.Draw(AlgorithmSettings.XOffset, AlgorithmSettings.YOffset, TempImg.Picture.Graphic);
  finally
    TempImg.Free;
  end;

  MakeImageContentTransparent(DebugGrid);
  DebugGrid.Repaint;  
end;


procedure DbgSaveScreenshotContent(ABmp: TBitmap);
const
  CScrShotBasePath = ''; /////////////////////////////////// to be updated when used
  CPath = CScrShotBasePath + '\MultiScrShot\Pic_';
  CDestHandle: THandle = 1319400;  //updated, based on debug app
  CMsg = WM_USER + 309;
var
  Cnt: Integer;
  BmpPath: string;
begin
  Cnt := SendMessage(CDestHandle, CMsg, 0, 0);
  BmpPath := CPath + IntToHex(Cnt, 4) + '.bmp';

  if not FileExists(BmpPath) then
    ABmp.SaveToFile(BmpPath);
end;


//Searches for BitmapToSearchFor in the bitmap of a component defined by ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height
//SrcCompSearchAreaBitmap - bitmap with source component, defined by InitRect
function MatchByBitmap(Algorithm: TMatchBitmapAlgorithm;
                       AlgorithmSettings: TMatchBitmapAlgorithmSettings;
                       AMatchByHistogramNumericSettings: TMatchByHistogramNumericSettings;
                       ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height: Integer;
                       BitmapToSearchFor, SrcCompSearchAreaBitmap, BitmapToSearchOn: TBitmap;
                       ImageSource: TImageSource;
                       CompHandle: THandle;
                       ColorErr, AllowedColorErrCnt, FastSearchAllowedColorErrCnt: Integer;
                       out SubCnvXOffset, SubCnvYOffset: Integer;
                       var AFoundBitmaps: TCompRecArr;
                       AUseFastSearch, AIgnoreBackgroundColor, AGetAllBitmaps: Boolean;
                       ABackgroundColor: TColor;
                       var AIgnoredColorsArr: TColorArr;
                       ASleepySearch: Byte;
                       AOutsideTickCount, APrecisionTimeout: QWord;
                       AStopSearchOnMismatch: Boolean;
                       ACropFromScreenshot: Boolean;
                       AThreadCount: Integer;
                       ACustomOpenCLPath: string;
                       AGPUPlatformIndex, AGPUDeviceIndex: Integer;
                       AGPUExecutionAvailability: TGPUExecutionAvailability;
                       AGPUIncludeDashG: Boolean;
                       AGPUSlaveQueueFromDevice: Boolean;
                       AGPUUseAllKernelsEvent: Boolean;
                       AGPUNdrangeNoLocalParam: Boolean;
                       AGPUUseEventsInEnqueueKernel: Boolean;
                       AGPUWaitForAllKernelsToBeDone: Boolean;
                       AGPUReleaseFinalEventAtKernelEnd: Boolean;
                       AGPUIgnoreExecutionAvailability: Boolean;
                       var AGPUDbgBuffer: TIntArr;
                       out AResultedErrorCount: Integer;
                       AStopAllActionsOnDemand: PBoolean): Boolean;
var
  i: Integer;
  SrcRect, DestRect: TRect;
  FullScreenBmp: TBitmap;
  hwc: TCompRec;
begin
  Result := False;
                       //SrcCompSearchAreaBitmap is the cropped area, from where BitmapToSearchFor is searched for.

  if ImageSource = isScreenshot then
  begin
    if not ACropFromScreenshot then
      ScreenShot(CompHandle, SrcCompSearchAreaBitmap, ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height)
    else
    begin
      //The following code is similar to the one from CroppedFullScreenShot, maybe it can be replaced (with minor modifications):
      hwc := GetWindowClassRec(CompHandle);

      SrcRect.Left := hwc.ComponentRectangle.Left + ScrShot_Left;
      SrcRect.Top := hwc.ComponentRectangle.Top + ScrShot_Top;
      SrcRect.Width := Max(3, ScrShot_Width);
      SrcRect.Height := Max(3, ScrShot_Height);

      DestRect.Left := 0;
      DestRect.Top := 0;
      DestRect.Width := SrcRect.Width;
      DestRect.Height := SrcRect.Height;
      SrcCompSearchAreaBitmap.SetSize(DestRect.Width, DestRect.Height);

      FullScreenBmp := TBitmap.Create;
      try
        ScreenShot(0, FullScreenBmp, 0, 0, Screen.Width, Screen.Height);  //Screen.DesktopWidth, Screen.DesktopHeight ???
        SrcCompSearchAreaBitmap.Canvas.CopyRect(DestRect, FullScreenBmp.Canvas, SrcRect);
      finally
        FullScreenBmp.Free;
      end;
    end;
  end
  else
  begin
    ScrShot_Width := Min(ScrShot_Width, BitmapToSearchOn.Width);
    ScrShot_Height := Min(ScrShot_Height, BitmapToSearchOn.Height);

    WipeBitmap(SrcCompSearchAreaBitmap, ScrShot_Width, ScrShot_Height);

    SrcRect.Left := ScrShot_Left;
    SrcRect.Top := ScrShot_Top;
    SrcRect.Width := Max(3, ScrShot_Width);
    SrcRect.Height := Max(3, ScrShot_Height);

    DestRect.Left := 0;
    DestRect.Top := 0;
    DestRect.Width := SrcRect.Width;
    DestRect.Height := SrcRect.Height;

    SrcCompSearchAreaBitmap.Canvas.CopyRect(DestRect, BitmapToSearchOn.Canvas, SrcRect);
  end;

  //DbgSaveScreenshotContent(SrcCompSearchAreaBitmap);   ////////////////////// keep commented for production code, also a path has to be updated, see above

  SubCnvXOffset := -1;  //for debugging..
  SubCnvYOffset := -1;  //for debugging..
  if BitmapPosMatch(Algorithm,
                    AlgorithmSettings,
                    AMatchByHistogramNumericSettings,
                    SrcCompSearchAreaBitmap,
                    BitmapToSearchFor,
                    ColorErr,
                    SubCnvXOffset,
                    SubCnvYOffset,
                    AFoundBitmaps,
                    AllowedColorErrCnt,
                    FastSearchAllowedColorErrCnt,
                    AUseFastSearch,
                    AIgnoreBackgroundColor,
                    AGetAllBitmaps,
                    ABackgroundColor,
                    AIgnoredColorsArr,
                    ASleepySearch,
                    AOutsideTickCount,
                    APrecisionTimeout,
                    AThreadCount,
                    ACustomOpenCLPath,
                    AGPUPlatformIndex,
                    AGPUDeviceIndex,
                    AGPUExecutionAvailability,
                    AGPUIncludeDashG,
                    AGPUSlaveQueueFromDevice,
                    AGPUUseAllKernelsEvent,
                    AGPUNdrangeNoLocalParam,
                    AGPUUseEventsInEnqueueKernel,
                    AGPUWaitForAllKernelsToBeDone,
                    AGPUReleaseFinalEventAtKernelEnd,
                    AGPUIgnoreExecutionAvailability,
                    AGPUDbgBuffer,
                    AResultedErrorCount,
                    AStopAllActionsOnDemand,
                    AStopSearchOnMismatch) then
  begin
    Result := True;
    Inc(SubCnvXOffset, ScrShot_Left);
    Inc(SubCnvYOffset, ScrShot_Top);

    //if AGetAllBitmaps then  //the for loop should run regardless, because there can be one array element, which should be updated to global coords
    for i := 0 to Length(AFoundBitmaps) - 1 do
    begin
      Inc(AFoundBitmaps[i].XOffsetFromParent, ScrShot_Left);
      Inc(AFoundBitmaps[i].YOffsetFromParent, ScrShot_Top);
    end;
  end;
end;


procedure ComputeScreenshotArea(var InputData: TFindControlInputData; var CompAtPoint: TCompRec; out ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height, CompWidth, CompHeight: Integer);
begin
  ScrShot_Left := InputData.GlobalSearchArea.Left - CompAtPoint.ComponentRectangle.Left - 0;      //InputData.GlobalSearchArea.Left > hwc.ComponentRectangle.Left  when the offset defined in UI is positive
  ScrShot_Top := InputData.GlobalSearchArea.Top - CompAtPoint.ComponentRectangle.Top - 0;
  ScrShot_Width := InputData.GlobalSearchArea.Right - InputData.GlobalSearchArea.Left + 1;
  ScrShot_Height := InputData.GlobalSearchArea.Bottom - InputData.GlobalSearchArea.Top + 1;
  CompWidth := CompAtPoint.ComponentRectangle.Right - CompAtPoint.ComponentRectangle.Left;
  CompHeight := CompAtPoint.ComponentRectangle.Bottom - CompAtPoint.ComponentRectangle.Top;
end;


procedure CroppedFullScreenShot(ACompAtPoint: TCompRec; AFindControlInputData: TFindControlInputData; ACompWidth, ACompHeight: Integer);
var
  FullScreenBmp: TBitmap;
  SrcRect, DestRect: TRect;
begin
  SrcRect.Left := ACompAtPoint.ComponentRectangle.Left;
  SrcRect.Top := ACompAtPoint.ComponentRectangle.Top;
  SrcRect.Width := Max(3, ACompWidth);
  SrcRect.Height := Max(3, ACompHeight);

  DestRect.Left := 0;
  DestRect.Top := 0;
  DestRect.Width := SrcRect.Width;
  DestRect.Height := SrcRect.Height;

  FullScreenBmp := TBitmap.Create;
  try
    AFindControlInputData.DebugBitmap.SetSize(SrcRect.Width, SrcRect.Height);
    ScreenShot(0, FullScreenBmp, 0, 0, Screen.Width, Screen.Height);  //Screen.DesktopWidth, Screen.DesktopHeight ???
    AFindControlInputData.DebugBitmap.Canvas.CopyRect(DestRect, FullScreenBmp.Canvas, SrcRect);

    if AFindControlInputData.DebugBitmap.Width > ACompWidth then
      AFindControlInputData.DebugBitmap.Width := ACompWidth; //do not set with Max, because that will always set the width

    if AFindControlInputData.DebugBitmap.Height > ACompHeight then
      AFindControlInputData.DebugBitmap.Height := ACompHeight; //do not set with Max, because that will always set the height
  finally
    FullScreenBmp.Free;
  end;
end;


//Searches for BitmapToSearchFor in the bitmap of a component defined by ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height
//SrcCompSearchAreaBitmap - bitmap with source component, defined by InitRect
function MatchControlByBitmap(Algorithm: TMatchBitmapAlgorithm;
                              AlgorithmSettings: TMatchBitmapAlgorithmSettings;
                              CompAtPoint: TCompRec;
                              var InputData: TFindControlInputData;
                              out SubCnvXOffset, SubCnvYOffset, AResultedErrorCount: Integer;
                              var AFoundBitmaps: TCompRecArr;
                              AStopAllActionsOnDemand: PBoolean;
                              ADisplayGridLineOption: TDisplayGridLineOption): Boolean;
var
  ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height, CompWidth, CompHeight: Integer;
  SrcCompSearchAreaBitmap: TBitmap;
  FoundBmp: Boolean;
begin
  ComputeScreenshotArea(InputData, CompAtPoint, ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height, CompWidth, CompHeight);

  if InputData.ImageSource = isScreenshot then
  begin
    if InputData.DebugBitmap <> nil then
    begin
      if not InputData.CropFromScreenshot then
        ScreenShot(CompAtPoint.Handle, InputData.DebugBitmap, 0, 0, CompWidth, CompHeight)   //call this here, before calling MatchByBitmap, to have a screenshot on debug image, while searching :)
      else
        CroppedFullScreenShot(CompAtPoint, InputData, CompWidth, CompHeight);
    end;
  end
  else
  begin
    if InputData.DebugBitmap <> nil then
    begin
      //Do not modify ScrShot_Width or ScrShot_Height!
      WipeBitmap(InputData.DebugBitmap, InputData.BitmapToSearchOn.Width, InputData.BitmapToSearchOn.Height);
      InputData.DebugBitmap.Canvas.Draw(0, 0, InputData.BitmapToSearchOn);
    end;
  end;

  SrcCompSearchAreaBitmap := TBitmap.Create;
  try
    try
      FoundBmp := MatchByBitmap(Algorithm,
                                AlgorithmSettings,
                                InputData.MatchByHistogramNumericSettings,
                                ScrShot_Left,
                                ScrShot_Top,
                                ScrShot_Width,
                                ScrShot_Height,
                                InputData.BitmapToSearchFor,
                                SrcCompSearchAreaBitmap,  //used for debugging
                                InputData.BitmapToSearchOn,
                                InputData.ImageSource,
                                CompAtPoint.Handle,
                                InputData.ColorError,
                                InputData.AllowedColorErrorCount,
                                InputData.FastSearchAllowedColorErrorCount,
                                SubCnvXOffset,
                                SubCnvYOffset,
                                AFoundBitmaps,
                                InputData.UseFastSearch,
                                InputData.IgnoreBackgroundColor,
                                InputData.GetAllHandles,          // GetAllBitmaps is the FindSubControl's version of GetAllHandles
                                InputData.BackgroundColor,
                                InputData.IgnoredColorsArr,
                                InputData.SleepySearch,
                                InputData.OutsideTickCount,
                                InputData.PrecisionTimeout,
                                InputData.StopSearchOnMismatch,
                                InputData.CropFromScreenshot,
                                InputData.ThreadCount,
                                InputData.OpenCLPath,
                                InputData.GPUPlatformIndex,
                                InputData.GPUDeviceIndex,
                                InputData.GPUExecutionAvailability,
                                InputData.GPUIncludeDashG,
                                InputData.GPUSlaveQueueFromDevice,
                                InputData.GPUUseAllKernelsEvent,
                                InputData.GPUNdrangeNoLocalParam,
                                InputData.GPUUseEventsInEnqueueKernel,
                                InputData.GPUWaitForAllKernelsToBeDone,
                                InputData.GPUReleaseFinalEventAtKernelEnd,
                                InputData.GPUIgnoreExecutionAvailability,
                                InputData.GPUDbgBuffer,
                                AResultedErrorCount,
                                AStopAllActionsOnDemand);
    except
      on E: EBmpMatchTimeout do
      begin  //same calls as below (DisplayDebugBmpForFailedMatch and DisplayDebugGrid, for the failed case)
        if InputData.DebugBitmap <> nil then
        begin
          DisplayDebugBmpForFailedMatch(InputData.BitmapToSearchFor, SrcCompSearchAreaBitmap, InputData.DebugBitmap, ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height, SubCnvXOffset, SubCnvYOffset, InputData.MatchingMethods, InputData.DebugTemplateName, InputData.Text, InputData.UseFastSearch, InputData.IgnoreBackgroundColor, InputData.FullBackgroundImageInResult, InputData.BackgroundColor);
          DisplayDebugGrid(Algorithm, AlgorithmSettings, InputData.DebugGrid, ScrShot_Width, ScrShot_Height, ADisplayGridLineOption);
        end;

        raise;
      end;
    end;

    if InputData.DebugBitmap <> nil then
    begin
      if FoundBmp then
        DisplayDebugBmpForSuccessfulMatch(InputData.BitmapToSearchFor, InputData.DebugBitmap, ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height, SubCnvXOffset, SubCnvYOffset, AFoundBitmaps, ADisplayGridLineOption = loTransparentSolid, InputData.FullBackgroundImageInResult)
      else
        DisplayDebugBmpForFailedMatch(InputData.BitmapToSearchFor, SrcCompSearchAreaBitmap, InputData.DebugBitmap, ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height, SubCnvXOffset, SubCnvYOffset, InputData.MatchingMethods, InputData.DebugTemplateName, InputData.Text, InputData.UseFastSearch, InputData.IgnoreBackgroundColor, InputData.FullBackgroundImageInResult, InputData.BackgroundColor);

      DisplayDebugGrid(Algorithm, AlgorithmSettings, InputData.DebugGrid, ScrShot_Width, ScrShot_Height, ADisplayGridLineOption);
    end;
  finally
    SrcCompSearchAreaBitmap.Free;
  end;

  Result := FoundBmp;
end;


function MatchControl(var CompAtPoint: TCompRec;
                      AInputData: TFindControlInputData;
                      AStopAllActionsOnDemand: PBoolean;
                      {var} AvailableControls: TCompRecArr; //Do not pass by reference. Let it create a copy, so that any modification will not affect "source" components (search area).
                      var AFoundSubControls: TCompRecArr; //used when searching with FindSubControl and InputData.GetAllHandles is True
                      ListOfControlTexts, ListOfControlClasses: TStringList;
                      ADisplayGridLineOption: TDisplayGridLineOption): Boolean;
var
  FoundClass, FoundText: Boolean;
begin
  FoundClass := True;
  FoundText := True;

  if mmClass in AInputData.MatchingMethods then
  begin
    if ListOfControlClasses.Count > 0 then
      FoundClass := ClassNameOrCaptionInListOfPatternedStrings(CompAtPoint.ClassName, ListOfControlClasses)
    else
      FoundClass := CompAtPoint.ClassName = '';
  end;

  if mmText in AInputData.MatchingMethods then
  begin
    if ListOfControlTexts.Count > 0 then
      FoundText := ClassNameOrCaptionInListOfPatternedStrings(CompAtPoint.Text, ListOfControlTexts)
    else
      FoundText := CompAtPoint.Text = '';
  end;

  Result := FoundClass and FoundText;
end;


function MatchSubControl(var CompAtPoint: TCompRec;
                         Algorithm: TMatchBitmapAlgorithm;
                         AlgorithmSettings: TMatchBitmapAlgorithmSettings;
                         var InputData: TFindControlInputData;
                         AStopAllActionsOnDemand: PBoolean;
                         {var} AvailableControls: TCompRecArr; //Do not pass by reference. Let it create a copy, so that any modification will not affect "source" components (search area).
                         var AFoundSubControls: TCompRecArr; //used when searching with FindSubControl and InputData.GetAllHandles is True
                         ADisplayGridLineOption: TDisplayGridLineOption): Boolean;
var
  SubCnvXOffset, SubCnvYOffset: Integer;
  i: Integer;
begin
  Result := MatchControlByBitmap(Algorithm,
                                   AlgorithmSettings,
                                   CompAtPoint,
                                   InputData,
                                   SubCnvXOffset,
                                   SubCnvYOffset,
                                   CompAtPoint.ResultedErrorCount,
                                   AFoundSubControls,
                                   AStopAllActionsOnDemand,
                                   ADisplayGridLineOption);

  if Result then    //yes, this "if" should be inside mmBitmap condition
  begin
    for i := 0 to Length(AFoundSubControls) - 1 do
    begin
      AFoundSubControls[i].Handle := CompAtPoint.Handle;
      AFoundSubControls[i].IsSubControl := CompAtPoint.IsSubControl;
      AFoundSubControls[i].ClassName := CompAtPoint.ClassName;
      AFoundSubControls[i].Text := CompAtPoint.Text;
      AFoundSubControls[i].ComponentRectangle := CompAtPoint.ComponentRectangle;

      Inc(AFoundSubControls[i].ComponentRectangle.Left, AFoundSubControls[i].XOffsetFromParent);
      Inc(AFoundSubControls[i].ComponentRectangle.Top, AFoundSubControls[i].YOffsetFromParent);
      AFoundSubControls[i].ComponentRectangle.Right := AFoundSubControls[i].ComponentRectangle.Left + InputData.BitmapToSearchFor.Width;
      AFoundSubControls[i].ComponentRectangle.Bottom := AFoundSubControls[i].ComponentRectangle.Top + InputData.BitmapToSearchFor.Height;
    end;

    CompAtPoint.XOffsetFromParent := SubCnvXOffset;
    CompAtPoint.YOffsetFromParent := SubCnvYOffset;

    Inc(CompAtPoint.ComponentRectangle.Left, SubCnvXOffset);
    Inc(CompAtPoint.ComponentRectangle.Top, SubCnvYOffset);
    CompAtPoint.ComponentRectangle.Right := CompAtPoint.ComponentRectangle.Left + InputData.BitmapToSearchFor.Width;
    CompAtPoint.ComponentRectangle.Bottom := CompAtPoint.ComponentRectangle.Top + InputData.BitmapToSearchFor.Height;
  end;
end;


function GetControlHandleIndexInResultedControls(var AResultedControl: TCompRecArr; ASearchHandle: THandle): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := Length(AResultedControl) - 1 downto 0 do  //using downto, because there is a higher probability that the searched handle is at the end of the array
    if AResultedControl[i].Handle = ASearchHandle then
    begin
      Result := i;
      Break;
    end;
end;


function FindControlOnScreen(InputData: TFindControlInputData;
                             AInitialTickCount: QWord;
                             AStopAllActionsOnDemand: PBoolean;
                             var AResultedControl: TCompRecArr;
                             ADisplayGridLineOption: TDisplayGridLineOption): Boolean;
var
  i, j, k: Integer;
  tp: TPoint;
  CompAtPoint: TCompRec;
  XValues, YValues: TIntArrArr;
  ListOfControlTexts, ListOfControlClasses: TStringList;
  AvailableControls: TCompRecArr;
  FoundSubControls: TCompRecArr;
  GlobalSearchAreaWidth, GlobalSearchAreaHeight: Integer;
  InputDataForCaching: TFindControlInputData;
begin
  Result := False;

  //InputData.BitmapToSearchFor.PixelFormat := pf24bit;  //Leave commented! If the pixel format is different than 24-bit, and changed here, the content is cleared.

  ListOfControlTexts := TStringList.Create;
  ListOfControlClasses := TStringList.Create;
  SetLength(AvailableControls, 0);
  SetLength(FoundSubControls, 0);
  try
    ListOfControlTexts.LineBreak := #13#10;
    ListOfControlClasses.LineBreak := #13#10;
    ListOfControlTexts.Text := StringReplace(InputData.Text, InputData.TextSeparator, #13#10, [rfReplaceAll]);             //no fast replace here :( , because the separator can be anything
    ListOfControlClasses.Text := StringReplace(InputData.ClassName, InputData.ClassNameSeparator, #13#10, [rfReplaceAll]); //no fast replace here :( , because the separator can be anything

    GlobalSearchAreaWidth := InputData.GlobalSearchArea.Right - InputData.GlobalSearchArea.Left;
    GlobalSearchAreaHeight := InputData.GlobalSearchArea.Bottom - InputData.GlobalSearchArea.Top;

    if InputData.StartSearchingWithCachedControl then
    begin
      tp.X := InputData.CachedControlLeft;
      tp.Y := InputData.CachedControlTop;

      if InputData.ImageSource = isScreenshot then
        CompAtPoint := GetWindowClassRec(tp)
      else
      begin
        CompAtPoint.ComponentRectangle.Left := 0;
        CompAtPoint.ComponentRectangle.Top := 0;
        CompAtPoint.ComponentRectangle.Width := 100;  //Not used. Set to a "valid" value, in case it will be used later.
        CompAtPoint.ComponentRectangle.Height := 100; //Not used. Set to a "valid" value, in case it will be used later.
      end;

      CompAtPoint.MouseXOffset := 0;
      CompAtPoint.MouseYOffset := 0;

      InputDataForCaching := InputData;

      //MessageBox(0, PChar('  w: ' + IntToStr(InputDataForCaching.BitmapToSearchFor.Width) +
      //                    '  h: ' + IntToStr(InputDataForCaching.BitmapToSearchFor.Height) +
      //                    '  cw: ' + IntToStr(CompAtPoint.ComponentRectangle.Width) +
      //                    '  ch: ' + IntToStr(CompAtPoint.ComponentRectangle.Height)),
      //           'Dbg w/h',
      //           MB_ICONINFORMATION);

      InputDataForCaching.GlobalSearchArea.Left := tp.X;
      InputDataForCaching.GlobalSearchArea.Top := tp.Y;
      //InputDataForCaching.GlobalSearchArea.Width := InputDataForCaching.BitmapToSearchFor.Width;     //BitmapToSearchFor is not set on FindControl
      //InputDataForCaching.GlobalSearchArea.Height := InputDataForCaching.BitmapToSearchFor.Height;   //BitmapToSearchFor is not set on FindControl
      InputDataForCaching.InitialRectangleOffsets.Left := 0;
      InputDataForCaching.InitialRectangleOffsets.Top := 0;
      InputDataForCaching.InitialRectangleOffsets.Right := 0;
      InputDataForCaching.InitialRectangleOffsets.Bottom := 0;

      CompAtPoint.XOffsetFromParent := 0;
      CompAtPoint.YOffsetFromParent := 0;
      CompAtPoint.ResultedErrorCount := 0;
                                                           //ToDo: enclose this code with a loop for verifying excluded handles
      if MatchControl(CompAtPoint,
                      InputDataForCaching,
                      AStopAllActionsOnDemand,
                      AvailableControls,
                      FoundSubControls,
                      ListOfControlTexts,
                      ListOfControlClasses,
                      ADisplayGridLineOption) then
      begin
        SetLength(AResultedControl, 1);
        AResultedControl[0] := CompAtPoint;
        Result := True;

        //MessageBox(0, PChar('Found by cache in ' + IntToStr(GetTickCount64 - AInitialTickCount)),
        //         'Dbg FindSubControl with cache',
        //         MB_ICONINFORMATION);

        Exit;
      end;

      //MessageBox(0, PChar('Not found by cache. Moving on... ' + IntToStr(GetTickCount64 - AInitialTickCount)),
      //           'Dbg FindSubControl with cache',
      //           MB_ICONINFORMATION);
    end;

    GenerateBinarySearchValuesForImage(GlobalSearchAreaWidth, GlobalSearchAreaHeight, XValues, YValues);

    try
      Result := False;
      SetLength(AResultedControl, 0);
      for k := 0 to Length(XValues) - 1 do   //all granularity levels        //search for multiple controls in the search area, then verify if their bitmaps match
      begin
        for i := 0 to Length(YValues[k]) - 1 do
          for j := 0 to Length(XValues[k]) - 1 do
          begin
            tp.X := XValues[k][j] + InputData.GlobalSearchArea.Left;
            tp.Y := YValues[k][i] + InputData.GlobalSearchArea.Top;

            CompAtPoint := GetWindowClassRec(tp);

            CompAtPoint.XOffsetFromParent := 0;
            CompAtPoint.YOffsetFromParent := 0;

            //SetCursorPos(tp.X, tp.Y); /////////////////// debug only

            if MatchControl(CompAtPoint,
                            InputData,
                            AStopAllActionsOnDemand,
                            AvailableControls,
                            FoundSubControls,
                            ListOfControlTexts,
                            ListOfControlClasses,
                            ADisplayGridLineOption) then
            begin
              if GetControlHandleIndexInResultedControls(AResultedControl, CompAtPoint.Handle) = -1 then
              begin
                SetLength(AResultedControl, Length(AResultedControl) + 1);
                AResultedControl[Length(AResultedControl) - 1] := CompAtPoint;
              end;

              Result := True;

              if not InputData.GetAllHandles then
                Exit;
            end;

            if Random(7) = 2 then
            begin
              Application.ProcessMessages;  //required, to allow stopping from button
              Sleep(1);
            end;

            {$IFDEF Windows}
              if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) and (GetAsyncKeyState(VK_F2) < 0) then
            {$ELSE}
              if (GetKeyState(VK_CONTROL) < 0) and (GetKeyState(VK_SHIFT) < 0) and (GetKeyState(VK_F2) < 0) then
            {$ENDIF}
              AStopAllActionsOnDemand^ := True;

            if AStopAllActionsOnDemand^ then
              Exit;

            if InputData.PrecisionTimeout > 0 then
              if ((GetTickCount64 - AInitialTickCount > InputData.PrecisionTimeout) {or
                 (GetTickCount64 - InputData.OutsideTickCount > InputData.PrecisionTimeout)}) then  //do not uncomment, because this will always timeout
              begin
                //MessageBox(0, 'Stopped by user.', PChar(Application.Title + ' - FindControlOnScreen'), MB_ICONEXCLAMATION);
                Exit;
              end;
          end; //for
      end; //for
    finally
      ClearUsedValues(XValues);
      ClearUsedValues(YValues);
    end;
  finally
    ListOfControlTexts.Free;
    ListOfControlClasses.Free;
    SetLength(AvailableControls, 0);
    SetLength(FoundSubControls, 0);
  end;
end;


procedure FixTRect(var ATRect: TRect);
begin
  if ATRect.Left > Screen.Width - 1 then
    ATRect.Left := Screen.Width - 1;

  if ATRect.Top > Screen.Height - 1 then
    ATRect.Top := Screen.Height - 1;

  if ATRect.Right > Screen.Width - 1 then
    ATRect.Right := Screen.Width - 1;

  if ATRect.Bottom > Screen.Height - 1 then
    ATRect.Bottom := Screen.Height - 1;

  if ATRect.Left < 0 then
    ATRect.Left := 0;

  if ATRect.Top < 0 then
    ATRect.Top := 0;

  if ATRect.Right < 1 then
    ATRect.Right := 1;

  if ATRect.Bottom < 1 then
    ATRect.Bottom := 1;

  if ATRect.Width < 1 then
    ATRect.Width := 1;

  if ATRect.Height < 1 then
    ATRect.Height := 1;
end;


//The search area may have invalid values, as a result of user input. This leads to AVs, like division by 0 or out of memory.
procedure FixInputData(var InputData: TFindControlInputData);
begin
  FixTRect(InputData.GlobalSearchArea);
  FixTRect(InputData.InitialRectangleOffsets);
  //Maybe a combination of GlobalSearchArea and InitialRectangleOffsets will be needed later.
end;


function FindSubControlOnScreen(Algorithm: TMatchBitmapAlgorithm;
                                AlgorithmSettings: TMatchBitmapAlgorithmSettings;
                                var InputData: TFindControlInputData;
                                AInitialTickCount: QWord;
                                AStopAllActionsOnDemand: PBoolean;
                                var AResultedControl: TCompRecArr;
                                ADisplayGridLineOption: TDisplayGridLineOption): Boolean;
var
  cc: Integer;
  tp: TPoint;
  CompAtPoint: TCompRec;
  AvailableControls: TCompRecArr;
  FoundSubControls: TCompRecArr;
  InputDataForCaching: TFindControlInputData;
begin
  Result := False;

  //InputData.BitmapToSearchFor.PixelFormat := pf24bit;  //Leave commented! If the pixel format is different than 24-bit, and changed here, the content is cleared.

  FixInputData(InputData);

  SetLength(AvailableControls, 0);
  SetLength(FoundSubControls, 0);
  try
    if InputData.StartSearchingWithCachedControl then
    begin
      tp.X := InputData.CachedControlLeft;
      tp.Y := InputData.CachedControlTop;

      if InputData.ImageSource = isScreenshot then
        CompAtPoint := GetWindowClassRec(tp)
      else
      begin
        CompAtPoint.ComponentRectangle.Left := 0;
        CompAtPoint.ComponentRectangle.Top := 0;
        CompAtPoint.ComponentRectangle.Width := 100;  //Not used. Set to a "valid" value, in case it will be used later.
        CompAtPoint.ComponentRectangle.Height := 100; //Not used. Set to a "valid" value, in case it will be used later.
      end;

      CompAtPoint.MouseXOffset := 0;
      CompAtPoint.MouseYOffset := 0;

      InputDataForCaching := InputData;

      //MessageBox(0, PChar('  w: ' + IntToStr(InputDataForCaching.BitmapToSearchFor.Width) +
      //                    '  h: ' + IntToStr(InputDataForCaching.BitmapToSearchFor.Height) +
      //                    '  cw: ' + IntToStr(CompAtPoint.ComponentRectangle.Width) +
      //                    '  ch: ' + IntToStr(CompAtPoint.ComponentRectangle.Height)),
      //           'Dbg w/h',
      //           MB_ICONINFORMATION);

      InputDataForCaching.GlobalSearchArea.Left := tp.X;
      InputDataForCaching.GlobalSearchArea.Top := tp.Y;
      InputDataForCaching.GlobalSearchArea.Width := InputDataForCaching.BitmapToSearchFor.Width;
      InputDataForCaching.GlobalSearchArea.Height := InputDataForCaching.BitmapToSearchFor.Height;
      InputDataForCaching.InitialRectangleOffsets.Left := 0;
      InputDataForCaching.InitialRectangleOffsets.Top := 0;
      InputDataForCaching.InitialRectangleOffsets.Right := 0;
      InputDataForCaching.InitialRectangleOffsets.Bottom := 0;

      CompAtPoint.XOffsetFromParent := 0;
      CompAtPoint.YOffsetFromParent := 0;
                                                           //ToDo: enclose this code with a loop for verifying excluded handles
      if MatchSubControl(CompAtPoint,
                         Algorithm,
                         AlgorithmSettings,
                         InputDataForCaching,
                         AStopAllActionsOnDemand,
                         AvailableControls,
                         FoundSubControls,
                         ADisplayGridLineOption) then
      begin
        SetLength(AResultedControl, 1);
        AResultedControl[0] := CompAtPoint;

        SetLength(InputData.GPUDbgBuffer, Length(InputDataForCaching.GPUDbgBuffer));
        Move(InputDataForCaching.GPUDbgBuffer[0], InputData.GPUDbgBuffer[0], Length(InputData.GPUDbgBuffer) * SizeOf(InputData.GPUDbgBuffer[0]));

        Result := True;

        //MessageBox(0, PChar('Found by cache in ' + IntToStr(GetTickCount64 - AInitialTickCount)),
        //         'Dbg FindSubControl with cache',
        //         MB_ICONINFORMATION);

        Exit;
      end;

      //MessageBox(0, PChar('Not found by cache. Moving on... ' + IntToStr(GetTickCount64 - AInitialTickCount)),
      //           'Dbg FindSubControl with cache',
      //           MB_ICONINFORMATION);
    end;

    try
      Result := False;
      SetLength(AResultedControl, 0);

      tp.X := InputData.GlobalSearchArea.Left;
      tp.Y := InputData.GlobalSearchArea.Top;

      if InputData.ImageSource = isScreenshot then
        CompAtPoint := GetWindowClassRec(tp)
      else
      begin
        CompAtPoint.ComponentRectangle.Left := 0;
        CompAtPoint.ComponentRectangle.Top := 0;
        CompAtPoint.ComponentRectangle.Width := 100;  //Not used. Set to a "valid" value, in case it will be used later.
        CompAtPoint.ComponentRectangle.Height := 100; //Not used. Set to a "valid" value, in case it will be used later.
      end;

      CompAtPoint.XOffsetFromParent := 0;
      CompAtPoint.YOffsetFromParent := 0;

      //SetCursorPos(tp.X, tp.Y); /////////////////// debug only

      if MatchSubControl(CompAtPoint,
                         Algorithm,
                         AlgorithmSettings,
                         InputData,
                         AStopAllActionsOnDemand,
                         AvailableControls,
                         FoundSubControls,
                         ADisplayGridLineOption) then
      begin
        SetLength(AResultedControl, Length(FoundSubControls));
        for cc := 0 to Length(FoundSubControls) - 1 do
          AResultedControl[cc] := FoundSubControls[cc];

        Result := True;

        if not InputData.GetAllHandles then
          Exit;
      end;

      if InputData.PrecisionTimeout > 0 then
        if ((GetTickCount64 - AInitialTickCount > InputData.PrecisionTimeout) {or
           (GetTickCount64 - InputData.OutsideTickCount > InputData.PrecisionTimeout)}) then  //do not uncomment, because this will always timeout
        begin
          //MessageBox(0, 'Stopped by user.', PChar(Application.Title + ' - FindControlOnScreen'), MB_ICONEXCLAMATION);
          Exit;
        end;
    finally
      if not InputData.StopSearchOnMismatch then
        if Length(AResultedControl) = 0 then
        begin
          SetLength(AResultedControl, Length(AResultedControl) + 1);
          AResultedControl[Length(AResultedControl) - 1] := CompAtPoint;
        end;
    end;
  finally
    SetLength(AvailableControls, 0);
    SetLength(FoundSubControls, 0);
  end;
end;

end.
