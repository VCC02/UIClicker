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


unit ControlInteraction;

{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}


interface


uses
  Windows, Graphics, Messages, BitmapProcessing, ExtCtrls, ClickerUtils;

type
  TMatchingMethod = (mmClass, mmText, mmBitmapText, mmBitmapFiles);
  TMatchingMethods = set of TMatchingMethod;

  TFindControlInputData = record
    ClassName, Text: string;
    ClassNameSeparator, TextSeparator: string;
    DebugBitmap: TBitmap; //image on debugging tab
    BitmapToSearchFor: TBitmap;
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
  end;


procedure SetControlText(hw: THandle; NewText: string);
procedure SelectComboBoxItem(hw: THandle; StartIndex: Integer; TextToSelect: string);

function MatchControlByBitmap(Algorithm: TMatchBitmapAlgorithm; AlgorithmSettings: TMatchBitmapAlgorithmSettings; CompAtPoint: TCompRec; InputData: TFindControlInputData; out SubCnvXOffset, SubCnvYOffset: Integer; AStopAllActionsOnDemand: PBoolean): Boolean;
function FindControlOnScreen(Algorithm: TMatchBitmapAlgorithm; AlgorithmSettings: TMatchBitmapAlgorithmSettings; InputData: TFindControlInputData; AInitialTickCount, ATimeout: Cardinal; AStopAllActionsOnDemand: PBoolean; out AResultedControl: TCompRec): Boolean;
function FindWindowOnScreenByCaptionOrClass(InputData: TFindControlInputData; AInitialTickCount, ATimeout: Cardinal; AStopAllActionsOnDemand: PBoolean; out AResultedControl: TCompRec): Boolean;
function FindWindowOnScreenByCaptionAndClass(InputData: TFindControlInputData; AInitialTickCount, ATimeout: Cardinal; AStopAllActionsOnDemand: PBoolean; out AResultedControl: TCompRec): Boolean;


implementation


uses
  SysUtils, Classes, Forms, BinSearchValues, Types, Math;


procedure SetControlText(hw: THandle; NewText: string);
begin
  SendMessage(hw, WM_SETTEXT, 0, PtrInt(@NewText[1]));
end;


procedure SelectComboBoxItem(hw: THandle; StartIndex: Integer; TextToSelect: string);
begin
  SendMessage(hw, CB_SELECTSTRING, StartIndex, PtrInt(@TextToSelect[1]));
end;


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
  for i  := 0 to AStringList.Count - 1 do
    if StringMatchByWildCard(ClassNameOrCaption, AStringList.Strings[i]) then
    begin
      Result := True;
      Exit;
    end;  
end;


type
  TFindWindowByPatternObj = class(TObject)
    ListOfControlTexts, ListOfControlClasses: TStringList;
    ResultedControl: TCompRec;
    MatchingMethods: TMatchingMethods;
    FoundByEnum: Boolean;
  end;

var
  FindWindowByPatternObj: TFindWindowByPatternObj;  


//Callback as required by EnumWindows
function EnumWindowsProc(hwn: THandle; ALParam: LPARAM): BOOL;
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

  FindWindowByPatternObj.FoundByEnum := Found;
  Result := not Found; //if not Found, then continue searching  (a.k.a. continue calling this callback)         //To continue enumeration, the callback function must return TRUE; to stop enumeration, it must return FALSE.
end;


//uses '*' as wildcard
function FindWindowByByCaptionOrClassPattern(AListOfControlTexts, AListOfControlClasses: TStringList; AMatchingMethods: TMatchingMethods; var hwc: TCompRec): Boolean;
begin
  FindWindowByPatternObj := TFindWindowByPatternObj.Create;
  try
    FindWindowByPatternObj.FoundByEnum := False;
    FindWindowByPatternObj.ListOfControlTexts := AListOfControlTexts;
    FindWindowByPatternObj.ListOfControlClasses := AListOfControlClasses;
    FindWindowByPatternObj.MatchingMethods := AMatchingMethods;

    //MessageBox(0, PChar('LParam: ' + IntToStr(LPARAM(FindWindowByPatterObj))), 'FindWindowByByCaptionOrClassPattern', MB_ICONINFORMATION);
    EnumWindows(@EnumWindowsProc, LPARAM(FindWindowByPatternObj));

    Result := FindWindowByPatternObj.FoundByEnum;
    
    if Result then
      hwc := FindWindowByPatternObj.ResultedControl;
  finally
    FindWindowByPatternObj.Free;
  end;
end;


function FindWindowOnScreenByCaptionOrClass(InputData: TFindControlInputData; AInitialTickCount, ATimeout: Cardinal; AStopAllActionsOnDemand: PBoolean; out AResultedControl: TCompRec): Boolean;
var
  ListOfControlTexts, ListOfControlClasses: TStringList;
  hwc: TCompRec;
begin
  ListOfControlTexts := TStringList.Create;
  ListOfControlClasses := TStringList.Create;
  try
    ListOfControlTexts.Text := StringReplace(InputData.Text, InputData.TextSeparator, #13#10, [rfReplaceAll]);             //no fast replace here :( , because the separator can be anything
    ListOfControlClasses.Text := StringReplace(InputData.ClassName, InputData.ClassNameSeparator, #13#10, [rfReplaceAll]); //no fast replace here :( , because the separator can be anything

    Result := FindWindowByByCaptionOrClassPattern(ListOfControlTexts, ListOfControlClasses, InputData.MatchingMethods, hwc);
    if Result then
      AResultedControl := hwc;
  finally
    ListOfControlTexts.Free;
    ListOfControlClasses.Free;
  end;
end;



//uses '*' as wildcard
function FindWindowByByCaptionAndClassPattern(AListOfControlTexts, AListOfControlClasses: TStringList; AMatchingMethods: TMatchingMethods; var hwc: TCompRec): Boolean;
var
  i, j: Integer;
  AHandle: THandle;
begin
  Result := False;

  for i := 0 to AListOfControlTexts.Count - 1 do
    for j := 0 to AListOfControlClasses.Count - 1 do
    begin
      AHandle := FindWindow(PChar(AListOfControlClasses.Strings[j]), PChar(string(AListOfControlTexts.Strings[i])));
      if AHandle > 0 then
      begin
        hwc := GetWindowClassRec(AHandle);
        Result := True;
        Exit;
      end;
    end;
end;


function FindWindowOnScreenByCaptionAndClass(InputData: TFindControlInputData; AInitialTickCount, ATimeout: Cardinal; AStopAllActionsOnDemand: PBoolean; out AResultedControl: TCompRec): Boolean;
var
  ListOfControlTexts, ListOfControlClasses: TStringList;
  hwc: TCompRec;
begin
  ListOfControlTexts := TStringList.Create;
  ListOfControlClasses := TStringList.Create;
  try
    ListOfControlTexts.Text := StringReplace(InputData.Text, InputData.TextSeparator, #13#10, [rfReplaceAll]);             //no fast replace here :( , because the separator can be anything
    ListOfControlClasses.Text := StringReplace(InputData.ClassName, InputData.ClassNameSeparator, #13#10, [rfReplaceAll]); //no fast replace here :( , because the separator can be anything

    Result := FindWindowByByCaptionAndClassPattern(ListOfControlTexts, ListOfControlClasses, InputData.MatchingMethods, hwc);

    if Result then
      AResultedControl := hwc;
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


procedure DisplayDebugBmpForSuccessfulMatch(BitmapToSearchFor, DebugBmp: TBitmap; ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height: Integer; SubCnvXOffset, SubCnvYOffset: Integer);
var
  DebugDisplayLeft: Integer; //This is the value backup of the debug screenshot width, before increasing it. Debug information is displayed starting at this value
begin
  DebugDisplayLeft := DebugBmp.Width;  //yes, width

  DebugBmp.Width := Max(DebugBmp.Width, 400); //use Max to allow displaying text
  DebugBmp.Height := Max(DebugBmp.Height, 100); //use Max to allow displaying text

  DebugBmp.Canvas.Pen.Color := clRed;
  Line(DebugBmp.Canvas, SubCnvXOffset, 0, SubCnvXOffset, DebugBmp.Height);   //vert
  Line(DebugBmp.Canvas, SubCnvXOffset + BitmapToSearchFor.Width - 1, 0, SubCnvXOffset + BitmapToSearchFor.Width - 1, DebugBmp.Height);  //vert
  Line(DebugBmp.Canvas, 0, SubCnvYOffset, DebugDisplayLeft, SubCnvYOffset);   //horiz
  Line(DebugBmp.Canvas, 0, SubCnvYOffset + BitmapToSearchFor.Height - 1, DebugDisplayLeft, SubCnvYOffset + BitmapToSearchFor.Height - 1); //horiz

  DebugBmp.Canvas.Brush.Style := bsClear;
  DebugBmp.Canvas.Pen.Color := clLime;
  DebugBmp.Canvas.Pen.Style := psDot;
  DebugBmp.Canvas.Rectangle(ScrShot_Left, ScrShot_Top, ScrShot_Left + ScrShot_Width, ScrShot_Top + ScrShot_Height);
  DebugBmp.Canvas.Pen.Style := psSolid;
end;


procedure DisplayDebugBmpForFailedMatch(BitmapToSearchFor, SrcCompSearchAreaBitmap, DebugBmp: TBitmap; ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height: Integer; SubCnvXOffset, SubCnvYOffset: Integer; AMatchingMethods: TMatchingMethods; DebugTemplateName, SearchedText: string);
var
  DebugDisplayLeft: Integer; //This is the value backup of the debug screenshot width, before increasing it. Debug information is displayed starting at this value
  DebugDrawingX, DebugDrawingY: Integer;
begin
  DebugDisplayLeft := DebugBmp.Width;  //yes, width

  DebugBmp.Width := DebugDisplayLeft + Max(DebugBmp.Width, 400);  //400.. for long filenames
  DebugBmp.Height := Max(DebugBmp.Height, 100); //use Max to allow displaying text

  if DebugBmp.Width < 1 then //it may happen, if DebugDisplayLeft is negative
    DebugBmp.Width := 1;

  if DebugBmp.Height < 1 then //it should not happen, from above code
    DebugBmp.Height := 1;

  DebugDrawingX := DebugDisplayLeft + 10;

  if mmBitmapText in AMatchingMethods then
  begin
    DebugBmp.Transparent := False;
    DebugBmp.Canvas.Brush.Style := bsClear;
    DebugBmp.Canvas.Font.Color := clRed;
    DebugBmp.Canvas.TextOut(DebugDrawingX, 0, 'Generated by: ' + DebugTemplateName);
    DebugBmp.Canvas.TextOut(DebugDrawingX, 15, 'Text not found: ' + SearchedText);
    DebugBmp.Canvas.Draw(DebugDrawingX, 30, BitmapToSearchFor);
    DebugBmp.Canvas.TextOut(DebugDrawingX, 30 + BitmapToSearchFor.Height + 10, 'BitmapToSearchFor w/h: ' + IntToStr(BitmapToSearchFor.Width) + ' x ' + IntToStr(BitmapToSearchFor.Height));
    DebugBmp.Canvas.TextOut(DebugDrawingX, 30 + BitmapToSearchFor.Height + 25, 'PixelFormat: ' + IntToStr(PIXELFORMAT_BPP[BitmapToSearchFor.PixelFormat]) + '-bit');

    DebugBmp.Canvas.Pen.Color := clRed;
    DebugBmp.Canvas.Rectangle(DebugDrawingX - 1, 30 - 1, DebugDrawingX + BitmapToSearchFor.Width + 1, 30 - 1 + BitmapToSearchFor.Height + 2);
  end;

  if mmBitmapFiles in AMatchingMethods then
  begin
    DebugBmp.Canvas.Brush.Style := bsClear;
    DebugBmp.Canvas.Font.Color := clRed;
    DebugBmp.Canvas.TextOut(DebugDrawingX, 0, 'Generated by: ' + DebugTemplateName);
    DebugBmp.Canvas.TextOut(DebugDrawingX, 15, 'Bitmap not found:');
    DebugBmp.Canvas.Draw(DebugDrawingX, 30, BitmapToSearchFor);

    DebugBmp.Canvas.Pen.Color := clRed;
    DebugBmp.Canvas.Rectangle(DebugDrawingX - 1, 30 - 1, DebugDrawingX + BitmapToSearchFor.Width + 1, 30 - 1 + BitmapToSearchFor.Height + 2);

    DebugBmp.Canvas.TextOut(DebugDrawingX, 30 + BitmapToSearchFor.Height + 5, 'In:');
    DebugDrawingY := 30 + BitmapToSearchFor.Height + 5 + DebugBmp.Canvas.TextHeight('In:');
    DebugBmp.Canvas.Draw(DebugDrawingX, DebugDrawingY, SrcCompSearchAreaBitmap);

    //draw a green dotted rectangle around search area, displayed under "In:' section
    DebugBmp.Canvas.Brush.Style := bsClear;
    DebugBmp.Canvas.Pen.Color := clLime;
    DebugBmp.Canvas.Pen.Style := psDot;
    DebugBmp.Canvas.Rectangle(DebugDrawingX, DebugDrawingY, DebugDrawingX + SrcCompSearchAreaBitmap.Width, DebugDrawingY + SrcCompSearchAreaBitmap.Height);
    DebugBmp.Canvas.Pen.Style := psSolid;
  end;

  DebugBmp.Canvas.Brush.Style := bsClear;
  DebugBmp.Canvas.Pen.Color := $000080FF;
  DebugBmp.Canvas.Pen.Style := psDot;
  DebugBmp.Canvas.Rectangle(ScrShot_Left, ScrShot_Top, ScrShot_Left + ScrShot_Width, ScrShot_Top + ScrShot_Height);
  DebugBmp.Canvas.Pen.Style := psSolid;
end;


procedure DisplayDebugGrid(Algorithm: TMatchBitmapAlgorithm; AlgorithmSettings: TMatchBitmapAlgorithmSettings; DebugGrid: TImage; ScrShot_Width, ScrShot_Height: Integer);
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
    DrawSearchGrid(TempImg, AlgorithmSettings, ScrShot_Width, ScrShot_Height, $00C9AEFF); //pink
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
function MatchByBitmap(Algorithm: TMatchBitmapAlgorithm; AlgorithmSettings: TMatchBitmapAlgorithmSettings; ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height: Integer; BitmapToSearchFor, SrcCompSearchAreaBitmap: TBitmap; CompHandle: THandle; ColorErr, AllowedColorErrCnt: Integer; out SubCnvXOffset, SubCnvYOffset: Integer; AStopAllActionsOnDemand: PBoolean): Boolean;
begin
  Result := False;
                       //SrcCompSearchAreaBitmap is the cropped area, from where BitmapToSearchFor is searched for.
  ScreenShot(CompHandle, SrcCompSearchAreaBitmap, ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height);
  //DbgSaveScreenshotContent(SrcCompSearchAreaBitmap);   ////////////////////// keep commented for production code, also a path has to be updated, see above

  SubCnvXOffset := -1;  //for debugging..
  SubCnvYOffset := -1;  //for debugging..
  if BitmapPosMatch(Algorithm, AlgorithmSettings, SrcCompSearchAreaBitmap, BitmapToSearchFor, ColorErr, SubCnvXOffset, SubCnvYOffset, AllowedColorErrCnt, AStopAllActionsOnDemand) then
  begin
    Result := True;
    Inc(SubCnvXOffset, ScrShot_Left);
    Inc(SubCnvYOffset, ScrShot_Top);
  end;
end;


//Searches for BitmapToSearchFor in the bitmap of a component defined by ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height
//SrcCompSearchAreaBitmap - bitmap with source component, defined by InitRect
function MatchControlByBitmap(Algorithm: TMatchBitmapAlgorithm; AlgorithmSettings: TMatchBitmapAlgorithmSettings; CompAtPoint: TCompRec; InputData: TFindControlInputData; out SubCnvXOffset, SubCnvYOffset: Integer; AStopAllActionsOnDemand: PBoolean): Boolean;
var
  ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height, CompWith, CompHeight: Integer;
  SrcCompSearchAreaBitmap: TBitmap;
  FoundBmp: Boolean;
begin
  ScrShot_Left := InputData.GlobalSearchArea.Left - CompAtPoint.ComponentRectangle.Left - 0;      //InputData.GlobalSearchArea.Left > hwc.ComponentRectangle.Left  when the offset defined in UI is positive
  ScrShot_Top := InputData.GlobalSearchArea.Top - CompAtPoint.ComponentRectangle.Top - 0;
  ScrShot_Width := InputData.GlobalSearchArea.Right - InputData.GlobalSearchArea.Left + 1;
  ScrShot_Height := InputData.GlobalSearchArea.Bottom - InputData.GlobalSearchArea.Top + 1;
  CompWith := CompAtPoint.ComponentRectangle.Right - CompAtPoint.ComponentRectangle.Left;
  CompHeight := CompAtPoint.ComponentRectangle.Bottom - CompAtPoint.ComponentRectangle.Top;

  if InputData.DebugBitmap <> nil then
    ScreenShot(CompAtPoint.Handle, InputData.DebugBitmap, 0, 0, CompWith, CompHeight);  //call this here, before calling MatchByBitmap, to have a screenshot on debug image, while searching :)

  SrcCompSearchAreaBitmap := TBitmap.Create;
  try
    FoundBmp := MatchByBitmap(Algorithm,
                              AlgorithmSettings,
                              ScrShot_Left,
                              ScrShot_Top,
                              ScrShot_Width,
                              ScrShot_Height,
                              InputData.BitmapToSearchFor,
                              SrcCompSearchAreaBitmap,  //used for debugging
                              CompAtPoint.Handle,
                              InputData.ColorError,
                              InputData.AllowedColorErrorCount,
                              SubCnvXOffset,
                              SubCnvYOffset,
                              AStopAllActionsOnDemand);

    if InputData.DebugBitmap <> nil then
    begin
      if FoundBmp then
        DisplayDebugBmpForSuccessfulMatch(InputData.BitmapToSearchFor, InputData.DebugBitmap, ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height, SubCnvXOffset, SubCnvYOffset)
      else
        DisplayDebugBmpForFailedMatch(InputData.BitmapToSearchFor, SrcCompSearchAreaBitmap, InputData.DebugBitmap, ScrShot_Left, ScrShot_Top, ScrShot_Width, ScrShot_Height, SubCnvXOffset, SubCnvYOffset, InputData.MatchingMethods, InputData.DebugTemplateName, InputData.Text);

      DisplayDebugGrid(Algorithm, AlgorithmSettings, InputData.DebugGrid, ScrShot_Width, ScrShot_Height);
    end;
  finally
    SrcCompSearchAreaBitmap.Free;
  end;

  Result := FoundBmp;
end;


type
  TMatchingMethodsRec = record
    Matching_Class, Matching_Text, Matching_BitmapText, Matching_BitmapFiles: Boolean;
  end;


function MatchControl(var CompAtPoint: TCompRec;
                      AMatchingMethodsRec: TMatchingMethodsRec;  //this info is already contained by InputData, but it's faster this way
                      Algorithm: TMatchBitmapAlgorithm;
                      AlgorithmSettings: TMatchBitmapAlgorithmSettings;
                      InputData: TFindControlInputData;
                      AStopAllActionsOnDemand: PBoolean;
                      {var} AvailableControls: TCompRecArr; //Do not pass by reference. Let it create a copy, so that any modification will not affect "source" components (search area).
                      ListOfControlTexts, ListOfControlClasses: TStringList
                      ): Boolean;
var
  FoundClass, FoundText, FoundBmp: Boolean;
  SubCnvXOffset, SubCnvYOffset: Integer;
begin
  FoundClass := True;
  FoundText := True;
  FoundBmp := True;

  if AMatchingMethodsRec.Matching_Class then
  begin
    if ListOfControlClasses.Count > 0 then
      FoundClass := ClassNameOrCaptionInListOfPatternedStrings(CompAtPoint.ClassName, ListOfControlClasses)
    else
      FoundClass := CompAtPoint.ClassName = '';
  end;

  if AMatchingMethodsRec.Matching_Text then
  begin
    if ListOfControlTexts.Count > 0 then
      FoundText := ClassNameOrCaptionInListOfPatternedStrings(CompAtPoint.Text, ListOfControlTexts)
    else
      FoundText := CompAtPoint.Text = '';
  end;

  if AMatchingMethodsRec.Matching_BitmapText or AMatchingMethodsRec.Matching_BitmapFiles then
  begin
    if not ControlIsInListByInfo(CompAtPoint, AvailableControls) then
    begin
      SetLength(AvailableControls, Length(AvailableControls) + 1);
      AvailableControls[Length(AvailableControls) - 1] := CompAtPoint;

      FoundBmp := MatchControlByBitmap(Algorithm, AlgorithmSettings, CompAtPoint, InputData, SubCnvXOffset, SubCnvYOffset, AStopAllActionsOnDemand);
    end
    else
      FoundBmp := False;

    if FoundBmp and InputData.SearchAsSubControl then    //yes, this "if" should be inside mmBitmap condition
    begin
      CompAtPoint.XOffsetFromParent := SubCnvXOffset;
      CompAtPoint.YOffsetFromParent := SubCnvYOffset;

      Inc(CompAtPoint.ComponentRectangle.Left, SubCnvXOffset);
      Inc(CompAtPoint.ComponentRectangle.Top, SubCnvYOffset);
      CompAtPoint.ComponentRectangle.Right := CompAtPoint.ComponentRectangle.Left + InputData.BitmapToSearchFor.Width;
      CompAtPoint.ComponentRectangle.Bottom := CompAtPoint.ComponentRectangle.Top + InputData.BitmapToSearchFor.Height;
    end;
  end;

  Result := FoundClass and FoundText and FoundBmp;
end;


function FindControlOnScreen(Algorithm: TMatchBitmapAlgorithm; AlgorithmSettings: TMatchBitmapAlgorithmSettings; InputData: TFindControlInputData; AInitialTickCount, ATimeout: Cardinal; AStopAllActionsOnDemand: PBoolean; out AResultedControl: TCompRec): Boolean;
var
  i, j, k: Integer;
  tp: TPoint;
  CompAtPoint: TCompRec;
  XValues, YValues: TIntArrArr;
  ListOfControlTexts, ListOfControlClasses: TStringList;
  AvailableControls: TCompRecArr;
  GlobalSearchAreaWidth, GlobalSearchAreaHeight: Integer;
  MatchingMethodsRec: TMatchingMethodsRec;
  InputDataForCaching: TFindControlInputData;
begin
  Result := False;

  //InputData.BitmapToSearchFor.PixelFormat := pf24bit;  //Leave commented! If the pixel format is different than 24-bit, then changed here, the content is cleared.

  ListOfControlTexts := TStringList.Create;
  ListOfControlClasses := TStringList.Create;
  SetLength(AvailableControls, 0);
  try
    ListOfControlTexts.Text := StringReplace(InputData.Text, InputData.TextSeparator, #13#10, [rfReplaceAll]);             //no fast replace here :( , because the separator can be anything
    ListOfControlClasses.Text := StringReplace(InputData.ClassName, InputData.ClassNameSeparator, #13#10, [rfReplaceAll]); //no fast replace here :( , because the separator can be anything

    GlobalSearchAreaWidth := InputData.GlobalSearchArea.Right - InputData.GlobalSearchArea.Left;
    GlobalSearchAreaHeight := InputData.GlobalSearchArea.Bottom - InputData.GlobalSearchArea.Top;

    MatchingMethodsRec.Matching_Class := mmClass in InputData.MatchingMethods;
    MatchingMethodsRec.Matching_Text := mmText in InputData.MatchingMethods;
    MatchingMethodsRec.Matching_BitmapText := mmBitmapText in InputData.MatchingMethods;
    MatchingMethodsRec.Matching_BitmapFiles := mmBitmapFiles in InputData.MatchingMethods;

    if InputData.StartSearchingWithCachedControl then
    begin
      tp.X := InputData.CachedControlLeft;
      tp.Y := InputData.CachedControlTop;
      CompAtPoint := GetWindowClassRec(tp);
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

      if MatchControl(CompAtPoint,
                      MatchingMethodsRec,
                      Algorithm,
                      AlgorithmSettings,
                      InputDataForCaching,
                      AStopAllActionsOnDemand,
                      AvailableControls,
                      ListOfControlTexts,
                      ListOfControlClasses) then
      begin
        AResultedControl := CompAtPoint;
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

    if InputData.SearchAsSubControl then //a sub control has its own grid
    begin
      SetLength(XValues, 1);
      SetLength(YValues, 1);
      SetLength(XValues[0], 1);
      SetLength(YValues[0], 1);
      XValues[0][0] := 0;
      YValues[0][0] := 0;
    end
    else
      GenerateBinarySearchValuesForImage(GlobalSearchAreaWidth, GlobalSearchAreaHeight, XValues, YValues);

    try
      for k := 0 to Length(XValues) - 1 do   //all granularity levels        //search for multiple controls in the search area, then verify if their bitmaps match
      begin
        for i := 0 to Length(YValues[k]) - 1 do
          for j := 0 to Length(XValues[k]) - 1 do
          begin
            Result := False;
            tp.X := XValues[k][j] + InputData.GlobalSearchArea.Left;
            tp.Y := YValues[k][i] + InputData.GlobalSearchArea.Top;
            CompAtPoint := GetWindowClassRec(tp);
            CompAtPoint.XOffsetFromParent := 0;
            CompAtPoint.YOffsetFromParent := 0;

            //SetCursorPos(tp.X, tp.Y); /////////////////// debug only

            if MatchControl(CompAtPoint,
                            MatchingMethodsRec,
                            Algorithm,
                            AlgorithmSettings,
                            InputData,
                            AStopAllActionsOnDemand,
                            AvailableControls,
                            ListOfControlTexts,
                            ListOfControlClasses) then
            begin
              AResultedControl := CompAtPoint;
              Result := True;
              Exit;
            end;

            if Random(7) = 2 then
            begin
              Application.ProcessMessages;  //required, to allow stopping from button
              Sleep(1);
            end;

            if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) and (GetAsyncKeyState(VK_F2) < 0) then
              AStopAllActionsOnDemand^ := True;

            if AStopAllActionsOnDemand^ or ((ATimeout > 0) and (GetTickCount64 - AInitialTickCount > ATimeout)) then
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
  end;
end;

end.
