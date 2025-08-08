{
    Copyright (C) 2025 VCC
    creation date: 14 Dec 2024
    initial release date: 14 Dec 2024

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


unit ClickerClientAPI;

{$mode Delphi} //This causes the Enum types to take one byte. ObjFpc causes it to take 4 bytes.
               //Enums were replaced with Byte in the following structures, to not depend on this switch.

interface

uses
  Classes, SysUtils, ClickerUtils, Graphics;


type
  TClkClickOptionsAPI = record
    XClickPointReference: Byte; //TXClickPointReference;
    YClickPointReference: Byte; //TYClickPointReference;
    XClickPointVar: PWideChar;
    YClickPointVar: PWideChar;
    XOffset, YOffset: PWideChar;
    MouseButton: Byte; //TMouseButton;
    ClickWithCtrl: Boolean;
    ClickWithAlt: Boolean;
    ClickWithShift: Boolean;
    ClickWithDoubleClick: Boolean;
    Count: LongInt;
    LeaveMouse: Boolean;
    MoveWithoutClick: Boolean;
    ClickType: LongInt;    //see CClickType_Click and CClickType_DoubleClick
    XClickPointReferenceDest: Byte; //TXClickPointReference;
    YClickPointReferenceDest: Byte; //TYClickPointReference;
    XClickPointVarDest: PWideChar;
    YClickPointVarDest: PWideChar;
    XOffsetDest, YOffsetDest: PWideChar;
    MouseWheelType: Byte; //TMouseWheelType;
    MouseWheelAmount: PWideChar;
    DelayAfterMovingToDestination: PWideChar;
    DelayAfterMouseDown: PWideChar;
    MoveDuration: PWideChar;
    UseClipCursor: Boolean;
  end;

  PClkClickOptionsAPI = ^TClkClickOptionsAPI;

  TClkClickOptionsAPIWS = record
    XClickPointVar: WideString;
    YClickPointVar: WideString;
    XOffset, YOffset: WideString;
    XClickPointVarDest: WideString;
    YClickPointVarDest: WideString;
    XOffsetDest, YOffsetDest: WideString;
    MouseWheelAmount: WideString;
    DelayAfterMovingToDestination: WideString;
    DelayAfterMouseDown: WideString;
    MoveDuration: WideString;
  end;


  TClkExecAppOptionsAPI = record
    PathToApp: PWideChar;
    ListOfParams: PWideChar;
    WaitForApp: Boolean;
    AppStdIn: PWideChar;
    CurrentDir: PWideChar;
    UseInheritHandles: Byte; //TExecAppUseInheritHandles;
    NoConsole: Boolean;
  end;

  PClkExecAppOptionsAPI = ^TClkExecAppOptionsAPI;

  TClkExecAppOptionsAPIWS = record
    PathToApp: WideString;
    ListOfParams: WideString;
    AppStdIn: WideString;
    CurrentDir: WideString;
  end;


  TClkFindControlMatchCriteriaAPI = record
    WillMatchText: Boolean;
    WillMatchClassName: Boolean;

    SearchForControlMode: LongInt; // TSearchForControlMode; //must be LongInt, for proper alignment
  end;

  TClkFindSubControlMatchCriteriaAPI = record
    WillMatchBitmapText: Boolean;
    WillMatchBitmapFiles: Boolean;
    WillMatchPrimitiveFiles: Boolean;
  end;


  TRectStringAPI = record
    Left, Top, Right, Bottom: PWideChar;
    LeftOffset, TopOffset, RightOffset, BottomOffset: PWideChar;
  end;

  TRectStringAPIWS = record
    Left, Top, Right, Bottom: WideString;
    LeftOffset, TopOffset, RightOffset, BottomOffset: WideString;
  end;

  TMatchByHistogramSettingsAPI = record
    MinPercentColorMatch: PWideChar;
    MostSignificantColorCountInSubBmp: PWideChar;
    MostSignificantColorCountInBackgroundBmp: PWideChar;
  end;

  TMatchByHistogramSettingsAPIWS = record
    MinPercentColorMatch: WideString;
    MostSignificantColorCountInSubBmp: WideString;
    MostSignificantColorCountInBackgroundBmp: WideString;
  end;


  TClkFindControlMatchBitmapTextAPI = record
    ForegroundColor: PWideChar;
    BackgroundColor: PWideChar;
    FontName: PWideChar;
    FontSize: LongInt;
    Bold: Boolean;
    Italic: Boolean;
    Underline: Boolean;
    StrikeOut: Boolean;
    FontQuality: Byte; //TFontQuality;
    FontQualityUsesReplacement: Boolean;
    FontQualityReplacement: PWideChar;
    ProfileName: PWideChar;
    CropLeft: PWideChar;
    CropTop: PWideChar;
    CropRight: PWideChar;
    CropBottom: PWideChar;
    IgnoreBackgroundColor: Boolean;
  end;                                  //see a similar structure below if adding new fields

  PClkFindControlMatchBitmapTextAPI = ^TClkFindControlMatchBitmapTextAPI;


  TMatchBitmapTextArray = array[0..0] of TClkFindControlMatchBitmapTextAPI;
  PMatchBitmapTextArray = ^TMatchBitmapTextArray;

  TClkFindControlMatchBitmapTextAPIArr = array of TClkFindControlMatchBitmapTextAPI;

  TMatchBitmapTextRecAPI = record
    ArrLen: Integer; //32-bit
    Items: PMatchBitmapTextArray;
  end;

  PMatchBitmapTextRecAPI = ^TMatchBitmapTextRecAPI;


  TClkFindControlMatchBitmapTextWideString = record
    ForegroundColor: WideString;
    BackgroundColor: WideString;
    FontName: WideString;
    FontSize: Integer;
    Bold: Boolean;
    Italic: Boolean;
    Underline: Boolean;
    StrikeOut: Boolean;
    FontQuality: TFontQuality;
    FontQualityUsesReplacement: Boolean;
    FontQualityReplacement: WideString;
    ProfileName: WideString;
    CropLeft: WideString;
    CropTop: WideString;
    CropRight: WideString;
    CropBottom: WideString;
    IgnoreBackgroundColor: Boolean;
  end;

  TClkFindControlMatchBitmapTextWideStringArr = array of TClkFindControlMatchBitmapTextWideString;


  TClkFindControlOptionsAPI = record
    DummyField: Integer;    //required for proper alignment (otherwise, there is an AV in Python)
    MatchCriteria: TClkFindControlMatchCriteriaAPI;
    AllowToFail: Boolean;
    MatchText: PWideChar;
    MatchClassName: PWideChar;
    MatchTextSeparator: PWideChar;
    MatchClassNameSeparator: PWideChar;
    InitialRectangle: TRectStringAPI;
    UseWholeScreen: Boolean;
    WaitForControlToGoAway: Boolean;
    StartSearchingWithCachedControl: Boolean;
    CachedControlLeft: PWideChar;
    CachedControlTop: PWideChar;
    GetAllControls: Boolean;
    //UseFastSearch: Boolean;
    PrecisionTimeout: Boolean;
    EvaluateTextCount: PWideChar;
  end;

  PClkFindControlOptionsAPI = ^TClkFindControlOptionsAPI;

  TClkFindControlOptionsAPIWS = record
    MatchText: WideString;
    MatchClassName: WideString;
    MatchTextSeparator: WideString;
    MatchClassNameSeparator: WideString;
    InitialRectangle: TRectStringAPIWS;
    CachedControlLeft: WideString;
    CachedControlTop: WideString;
    EvaluateTextCount: WideString;
  end;


  TClkFindSubControlOptionsAPI = record
    DummyField: Integer;    //required for proper alignment (otherwise, there is an AV in Python)
    MatchCriteria: TClkFindSubControlMatchCriteriaAPI;
    AllowToFail: Boolean;
    MatchText: PWideChar;
    MatchBitmapText: PMatchBitmapTextRecAPI; //pointer to a TMatchBitmapTextRecAPI structure, which contains a length (32-bit) and a pointer to an array of TClkFindControlMatchBitmapText;
    MatchBitmapFiles: PWideChar;
    MatchBitmapAlgorithm: Byte; //TMatchBitmapAlgorithm; //change this to LongInt, in case of AV at python side
    MatchBitmapAlgorithmSettings: TMatchBitmapAlgorithmSettings;
    InitialRectangle: TRectStringAPI;
    UseWholeScreen: Boolean;
    ColorError: PWideChar;
    AllowedColorErrorCount: PWideChar;
    WaitForControlToGoAway: Boolean;
    StartSearchingWithCachedControl: Boolean;
    CachedControlLeft: PWideChar;
    CachedControlTop: PWideChar;
    MatchPrimitiveFiles: PWideChar;
    GetAllControls: Boolean;
    UseFastSearch: Boolean;
    FastSearchAllowedColorErrorCount: PWideChar;
    IgnoredColors: PWideChar;
    SleepySearch: Boolean;
    StopSearchOnMismatch: Boolean;
    ImageSource: Byte; //TImageSource;
    SourceFileName: PWideChar;
    ImageSourceFileNameLocation: Byte; //TImageSourceFileNameLocation;
    PrecisionTimeout: Boolean;
    FullBackgroundImageInResult: Boolean;
    MatchByHistogramSettings: TMatchByHistogramSettingsAPI;
    EvaluateTextCount: PWideChar;
    CropFromScreenshot: Boolean;
    ThreadCount: PWideChar;
  end;

  PClkFindSubControlOptionsAPI = ^TClkFindSubControlOptionsAPI;

  TClkFindSubControlOptionsAPIWS = record
    MatchText: WideString;
    //MatchBitmapText: PMatchBitmapTextRecAPI; //pointer to a TMatchBitmapTextRecAPI structure, which contains a length (32-bit) and a pointer to an array of TClkFindControlMatchBitmapText;
    MatchBitmapFiles: WideString;
    InitialRectangle: TRectStringAPIWS;
    ColorError: WideString;
    AllowedColorErrorCount: WideString;
    CachedControlLeft: WideString;
    CachedControlTop: WideString;
    MatchPrimitiveFiles: WideString;
    FastSearchAllowedColorErrorCount: WideString;
    IgnoredColors: WideString;
    SourceFileName: WideString;
    MatchByHistogramSettings: TMatchByHistogramSettingsAPIWS;
    EvaluateTextCount: WideString;
    ThreadCount: WideString;
  end;


  TClkSetTextOptionsAPI = record
    Text: PWideChar;
    ControlType: Byte; //TClkSetTextControlType;
    DelayBetweenKeyStrokes: PWideChar;
    Count: PWideChar;
  end;

  PClkSetTextOptionsAPI = ^TClkSetTextOptionsAPI;

  TClkSetTextOptionsAPIWS = record
    Text: WideString;
    DelayBetweenKeyStrokes: WideString;
    Count: WideString;
  end;


  TClkCallTemplateLoopOptionsAPI = record
    Enabled: Boolean; //When False, the CallTemplate action is executed once, as before. Else, it may be executed or not, based on loop settings.
    Counter: PWideChar;
    InitValue: PWideChar;
    EndValue: PWideChar;
    Direction: Byte; //TLoopDirection;
    BreakCondition: PWideChar; //uses the same format as TClkActionOptions.ActionCondition
    EvalBreakPosition: Byte; //TLoopEvalBreakPosition;
  end;

  TClkCallTemplateLoopOptionsAPIWS = record
    Counter: WideString;
    InitValue: WideString;
    EndValue: WideString;
    BreakCondition: WideString; //uses the same format as TClkActionOptions.ActionCondition
  end;

  TClkCallTemplateOptionsAPI = record
    TemplateFileName: PWideChar;
    ListOfCustomVarsAndValues: PWideChar;
    EvaluateBeforeCalling: Boolean;
    CallTemplateLoop: TClkCallTemplateLoopOptionsAPI;
  end;

  PClkCallTemplateOptionsAPI = ^TClkCallTemplateOptionsAPI;

  TClkCallTemplateOptionsAPIWS = record
    TemplateFileName: WideString;
    ListOfCustomVarsAndValues: WideString;
    CallTemplateLoop: TClkCallTemplateLoopOptionsAPIWS;
  end;

  TClkSleepOptionsAPI = record
    Value: PWideChar;  // [ms]
  end;

  PClkSleepOptionsAPI = ^TClkSleepOptionsAPI;

  TClkSleepOptionsAPIWS = record
    Value: WideString;  // [ms]
  end;


  TClkSetVarOptionsAPI = record
    ListOfVarNames: PWideChar;
    ListOfVarValues: PWideChar;
    ListOfVarEvalBefore: PWideChar;
    FailOnException: Boolean;
  end;

  PClkSetVarOptionsAPI = ^TClkSetVarOptionsAPI;

  TClkSetVarOptionsAPIWS = record
    ListOfVarNames: WideString;
    ListOfVarValues: WideString;
    ListOfVarEvalBefore: WideString;
  end;


  TClkWindowOperationsOptionsAPI = record
    Operation: Byte; //TWindowOperation;
    NewX, NewY, NewWidth, NewHeight: PWideChar;
    NewPositionEnabled, NewSizeEnabled: Boolean;
  end;

  PClkWindowOperationsOptionsAPI = ^TClkWindowOperationsOptionsAPI;

  TClkWindowOperationsOptionsAPIWS = record
    NewX, NewY, NewWidth, NewHeight: WideString;
  end;


  TClkLoadSetVarFromFileOptionsAPI = record
    FileName: PWideChar;
    SetVarActionName: PWideChar;
  end;

  PClkLoadSetVarFromFileOptionsAPI = ^TClkLoadSetVarFromFileOptionsAPI;

  TClkLoadSetVarFromFileOptionsAPIWS = record
    FileName: WideString;
    SetVarActionName: WideString;
  end;


  TClkSaveSetVarToFileOptionsAPI = record
    FileName: PWideChar;
    SetVarActionName: PWideChar;
  end;

  PClkSaveSetVarToFileOptionsAPI = ^TClkSaveSetVarToFileOptionsAPI;

  TClkSaveSetVarToFileOptionsAPIWS = record
    FileName: WideString;
    SetVarActionName: WideString;
  end;


  TClkPluginOptionsAPI = record
    FileName: PWideChar;
    ListOfPropertiesAndValues: PWideChar;
  end;

  PClkPluginOptionsAPI = ^TClkPluginOptionsAPI;

  TClkPluginOptionsAPIWS = record
    FileName: WideString;
    ListOfPropertiesAndValues: WideString;
  end;


  TClkEditTemplateOptionsAPI = record
    Operation: Byte; //TEditTemplateOperation;
    WhichTemplate: Byte; //TEditTemplateWhichTemplate;
    TemplateFileName: PWideChar;
    ListOfEditedProperties: PWideChar;
    ListOfEnabledProperties: PWideChar;
    EditedActionName: PWideChar;
    EditedActionType: Byte; //TClkAction;
    EditedActionCondition: PWideChar;
    EditedActionTimeout: LongInt;
    NewActionName: PWideChar;
    ShouldSaveTemplate: Boolean;
  end;

  PClkEditTemplateOptionsAPI = ^TClkEditTemplateOptionsAPI;

  TClkEditTemplateOptionsAPIWS = record
    TemplateFileName: WideString;
    ListOfEditedProperties: WideString;
    ListOfEnabledProperties: WideString;
    EditedActionName: WideString;
    EditedActionCondition: WideString;
    NewActionName: WideString;
  end;


procedure GetMatchBitmapTextFromAPI(AFindControlMatchBitmapText: PClkFindControlMatchBitmapTextAPI; var ADestMatchBitmapText: TClkFindControlMatchBitmapText);

procedure GetClickOptionsFromAPI(AClickOptions: PClkClickOptionsAPI; var ADestClkAction: TClkActionRec);
procedure GetExecAppOptionsFromAPI(AExecAppOptions: PClkExecAppOptionsAPI; var ADestClkAction: TClkActionRec);
procedure GetFindControlOptionsFromAPI(AFindControlOptions: PClkFindControlOptionsAPI; var ADestClkAction: TClkActionRec);
procedure GetFindSubControlOptionsFromAPI(AFindSubControlOptions: PClkFindSubControlOptionsAPI; var ADestClkAction: TClkActionRec);
procedure GetSetControlTextOptionsFromAPI(ASetControlTextOptions: PClkSetTextOptionsAPI; var ADestClkAction: TClkActionRec);
procedure GetCallTemplateOptionsFromAPI(ACallTemplateOptions: PClkCallTemplateOptionsAPI; var ADestClkAction: TClkActionRec);
procedure GetSleepOptionsFromAPI(ASleepOptions: PClkSleepOptionsAPI; var ADestClkAction: TClkActionRec);
function GetSetVarOptionsFromAPI(ASetVarOptions: PClkSetVarOptionsAPI; var ADestClkAction: TClkActionRec): Integer;
procedure GetWindowOperationsOptionsFromAPI(AWindowOperationsOptions: PClkWindowOperationsOptionsAPI; var ADestClkAction: TClkActionRec);
procedure GetLoadSetVarFromFileOptionsFromAPI(ALoadSetVarFromFileOptions: PClkLoadSetVarFromFileOptionsAPI; var ADestClkAction: TClkActionRec);
procedure GetSaveSetVarToFileOptionsFromAPI(ASaveSetVarToFileOptions: PClkSaveSetVarToFileOptionsAPI; var ADestClkAction: TClkActionRec);
procedure GetPluginOptionsFromAPI(APluginOptions: PClkPluginOptionsAPI; var ADestClkAction: TClkActionRec);
procedure GetEditTemplateOptionsFromAPI(AEditTemplateOptions: PClkEditTemplateOptionsAPI; var ADestClkAction: TClkActionRec);

procedure SetMatchBitmapTextToAPI(var AMatchBitmapText: TClkFindControlMatchBitmapText; var ADestMatchBitmapText: TClkFindControlMatchBitmapTextAPI; var ATempDestWideStr: TClkFindControlMatchBitmapTextWideString);

procedure SetClickOptionsToAPI(var AClickOptions: TClkClickOptions; var ADestClickOptions: TClkClickOptionsAPI; var ATempClickOptionsAPIWS: TClkClickOptionsAPIWS);
procedure SetExecAppOptionsToAPI(var AExecAppOptions: TClkExecAppOptions; var ADestExecAppOptions: TClkExecAppOptionsAPI; var ATempExecAppOptionsAPIWS: TClkExecAppOptionsAPIWS);
procedure SetFindControlOptionsToAPI(var AFindControlOptions: TClkFindControlOptions; var ADestFindControlOptions: TClkFindControlOptionsAPI; var ADestMatchBitmapTextRecAPI: TMatchBitmapTextRecAPI; var ADestMatchBitmapTextArray: TClkFindControlMatchBitmapTextAPIArr; var ATempFindControlOptionsAPIWS: TClkFindControlOptionsAPIWS);
procedure SetFindSubControlOptionsToAPI(var AFindSubControlOptions: TClkFindSubControlOptions; var ADestFindSubControlOptions: TClkFindSubControlOptionsAPI; var ADestMatchBitmapTextRecAPI: TMatchBitmapTextRecAPI; var ADestMatchBitmapTextArray: TClkFindControlMatchBitmapTextAPIArr; var ATempFindSubControlOptionsAPIWS: TClkFindSubControlOptionsAPIWS);
procedure SetSetControlTextOptionsToAPI(var ASetControlTextOptions: TClkSetTextOptions; var ADestSetControlTextOptions: TClkSetTextOptionsAPI; var ATempSetControlTextOptionsAPIWS: TClkSetTextOptionsAPIWS);
procedure SetCallTemplateOptionsToAPI(var ACallTemplateOptions: TClkCallTemplateOptions; var ADestCallTemplateOptions: TClkCallTemplateOptionsAPI; var ATempCallTemplateOptionsAPIWS: TClkCallTemplateOptionsAPIWS);
procedure SetSleepOptionsToAPI(var ASleepOptions: TClkSleepOptions; var ADestSleepOptions: TClkSleepOptionsAPI; var ATempSleepOptionsAPIWS: TClkSleepOptionsAPIWS);
procedure SetSetVarOptionsToAPI(var ASetVarOptions: TClkSetVarOptions; var ADestSetVarOptions: TClkSetVarOptionsAPI; var ATempSetVarOptionsAPIWS: TClkSetVarOptionsAPIWS);
procedure SetWindowOperationsOptionsToAPI(var AWindowOperationsOptions: TClkWindowOperationsOptions; var ADestWindowOperationsOptions: TClkWindowOperationsOptionsAPI; var ATempWindowOperationsOptionsAPIWS: TClkWindowOperationsOptionsAPIWS);
procedure SetLoadSetVarFromFileOptionsToAPI(var ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions; var ADestLoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptionsAPI; var ATempLoadSetVarFromFileOptionsAPIWS: TClkLoadSetVarFromFileOptionsAPIWS);
procedure SetSaveSetVarToFileOptionsToAPI(var ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions; var ADestSaveSetVarToFileOptions: TClkSaveSetVarToFileOptionsAPI; var ATempSaveSetVarToFileOptionsAPIWS: TClkSaveSetVarToFileOptionsAPIWS);
procedure SetPluginOptionsToAPI(var APluginOptions: TClkPluginOptions; var ADestPluginOptions: TClkPluginOptionsAPI; var ATempPluginOptionsAPIWS: TClkPluginOptionsAPIWS);
procedure SetEditTemplateOptionsToAPI(var AEditTemplateOptions: TClkEditTemplateOptions; var ADestEditTemplateOptions: TClkEditTemplateOptionsAPI; var ATempTClkEditTemplateOptionsAPIWS: TClkEditTemplateOptionsAPIWS);


implementation


uses
  DllUtils, Controls, Math;


////////////////

procedure GetMatchBitmapTextFromAPI(AFindControlMatchBitmapText: PClkFindControlMatchBitmapTextAPI; var ADestMatchBitmapText: TClkFindControlMatchBitmapText);
begin
  SetPointedContentToString(@string(PWideChar(AFindControlMatchBitmapText^.ForegroundColor))[1], ADestMatchBitmapText.ForegroundColor);
  SetPointedContentToString(@string(PWideChar(AFindControlMatchBitmapText^.BackgroundColor))[1], ADestMatchBitmapText.BackgroundColor);
  SetPointedContentToString(@string(PWideChar(AFindControlMatchBitmapText^.FontName))[1], ADestMatchBitmapText.FontName);
  ADestMatchBitmapText.FontSize := AFindControlMatchBitmapText^.FontSize;
  ADestMatchBitmapText.Bold := AFindControlMatchBitmapText^.Bold;
  ADestMatchBitmapText.Italic := AFindControlMatchBitmapText^.Italic;
  ADestMatchBitmapText.Underline := AFindControlMatchBitmapText^.Underline;
  ADestMatchBitmapText.StrikeOut := AFindControlMatchBitmapText^.StrikeOut;
  ADestMatchBitmapText.FontQuality := TFontQuality(AFindControlMatchBitmapText^.FontQuality);
  ADestMatchBitmapText.FontQualityUsesReplacement := AFindControlMatchBitmapText^.FontQualityUsesReplacement;
  SetPointedContentToString(@string(PWideChar(AFindControlMatchBitmapText^.FontQualityReplacement))[1], ADestMatchBitmapText.FontQualityReplacement);
  SetPointedContentToString(@string(PWideChar(AFindControlMatchBitmapText^.ProfileName))[1], ADestMatchBitmapText.ProfileName);
  SetPointedContentToString(@string(PWideChar(AFindControlMatchBitmapText^.CropLeft))[1], ADestMatchBitmapText.CropLeft);
  SetPointedContentToString(@string(PWideChar(AFindControlMatchBitmapText^.CropTop))[1], ADestMatchBitmapText.CropTop);
  SetPointedContentToString(@string(PWideChar(AFindControlMatchBitmapText^.CropRight))[1], ADestMatchBitmapText.CropRight);
  SetPointedContentToString(@string(PWideChar(AFindControlMatchBitmapText^.CropBottom))[1], ADestMatchBitmapText.CropBottom);
  ADestMatchBitmapText.IgnoreBackgroundColor := AFindControlMatchBitmapText^.IgnoreBackgroundColor;
end;

////////////////


procedure GetClickOptionsFromAPI(AClickOptions: PClkClickOptionsAPI; var ADestClkAction: TClkActionRec);
begin
  //click stuff
  ADestClkAction.ClickOptions.XClickPointReference := TXClickPointReference(AClickOptions^.XClickPointReference); //xrefLeft;
  ADestClkAction.ClickOptions.YClickPointReference := TYClickPointReference(AClickOptions^.YClickPointReference); //yrefTop;
  SetPointedContentToString(@string(PWideChar(AClickOptions^.XClickPointVar))[1], ADestClkAction.ClickOptions.XClickPointVar);
  SetPointedContentToString(@string(PWideChar(AClickOptions^.YClickPointVar))[1], ADestClkAction.ClickOptions.YClickPointVar);
  SetPointedContentToString(@string(PWideChar(AClickOptions^.XOffset))[1], ADestClkAction.ClickOptions.XOffset);
  SetPointedContentToString(@string(PWideChar(AClickOptions^.YOffset))[1], ADestClkAction.ClickOptions.YOffset);
  ADestClkAction.ClickOptions.MouseButton := TMouseButton(AClickOptions^.MouseButton);
  ADestClkAction.ClickOptions.ClickWithCtrl := AClickOptions^.ClickWithCtrl;
  ADestClkAction.ClickOptions.ClickWithAlt := AClickOptions^.ClickWithAlt;
  ADestClkAction.ClickOptions.ClickWithShift := AClickOptions^.ClickWithShift;
  ADestClkAction.ClickOptions.ClickWithDoubleClick := AClickOptions^.ClickWithDoubleClick;
  ADestClkAction.ClickOptions.Count := AClickOptions^.Count;
  ADestClkAction.ClickOptions.LeaveMouse := AClickOptions^.LeaveMouse;
  ADestClkAction.ClickOptions.MoveWithoutClick := AClickOptions^.MoveWithoutClick;
  ADestClkAction.ClickOptions.ClickType := AClickOptions^.ClickType;
  ADestClkAction.ClickOptions.XClickPointReferenceDest := TXClickPointReference(AClickOptions^.XClickPointReferenceDest);
  ADestClkAction.ClickOptions.YClickPointReferenceDest := TYClickPointReference(AClickOptions^.YClickPointReferenceDest);
  SetPointedContentToString(@string(PWideChar(AClickOptions^.XClickPointVarDest))[1], ADestClkAction.ClickOptions.XClickPointVarDest);
  SetPointedContentToString(@string(PWideChar(AClickOptions^.YClickPointVarDest))[1], ADestClkAction.ClickOptions.YClickPointVarDest);
  SetPointedContentToString(@string(PWideChar(AClickOptions^.XOffsetDest))[1], ADestClkAction.ClickOptions.XOffsetDest);
  SetPointedContentToString(@string(PWideChar(AClickOptions^.YOffsetDest))[1], ADestClkAction.ClickOptions.YOffsetDest);
  ADestClkAction.ClickOptions.MouseWheelType := TMouseWheelType(AClickOptions^.MouseWheelType);
  SetPointedContentToString(@string(PWideChar(AClickOptions^.MouseWheelAmount))[1], ADestClkAction.ClickOptions.MouseWheelAmount);
  SetPointedContentToString(@string(PWideChar(AClickOptions^.DelayAfterMovingToDestination))[1], ADestClkAction.ClickOptions.DelayAfterMovingToDestination);
  SetPointedContentToString(@string(PWideChar(AClickOptions^.DelayAfterMouseDown))[1], ADestClkAction.ClickOptions.DelayAfterMouseDown);
  SetPointedContentToString(@string(PWideChar(AClickOptions^.MoveDuration))[1], ADestClkAction.ClickOptions.MoveDuration);
  ADestClkAction.ClickOptions.UseClipCursor := AClickOptions^.UseClipCursor;
end;


procedure GetExecAppOptionsFromAPI(AExecAppOptions: PClkExecAppOptionsAPI; var ADestClkAction: TClkActionRec);
begin
  //exec app stuff
  SetPointedContentToString(@string(PWideChar(AExecAppOptions^.PathToApp))[1], ADestClkAction.ExecAppOptions.PathToApp);
  SetPointedContentToString(@string(PWideChar(AExecAppOptions^.ListOfParams))[1], ADestClkAction.ExecAppOptions.ListOfParams);
  ADestClkAction.ExecAppOptions.WaitForApp := AExecAppOptions^.WaitForApp;
  SetPointedContentToString(@string(PWideChar(AExecAppOptions^.AppStdIn))[1], ADestClkAction.ExecAppOptions.AppStdIn);
  SetPointedContentToString(@string(PWideChar(AExecAppOptions^.CurrentDir))[1], ADestClkAction.ExecAppOptions.CurrentDir);
  ADestClkAction.ExecAppOptions.UseInheritHandles := TExecAppUseInheritHandles(AExecAppOptions^.UseInheritHandles);
  ADestClkAction.ExecAppOptions.NoConsole := AExecAppOptions^.NoConsole;
end;


procedure GetFindControlOptionsFromAPI(AFindControlOptions: PClkFindControlOptionsAPI; var ADestClkAction: TClkActionRec);
begin
  //find control stuff
  ADestClkAction.FindControlOptions.MatchCriteria.WillMatchText := AFindControlOptions^.MatchCriteria.WillMatchText;
  ADestClkAction.FindControlOptions.MatchCriteria.WillMatchClassName := AFindControlOptions^.MatchCriteria.WillMatchClassName;
  //ADestClkAction.FindControlOptions.MatchCriteria.WillMatchBitmapText := AFindControlOptions^.MatchCriteria.WillMatchBitmapText;
  //ADestClkAction.FindControlOptions.MatchCriteria.WillMatchBitmapFiles := AFindControlOptions^.MatchCriteria.WillMatchBitmapFiles;
  //ADestClkAction.FindControlOptions.MatchCriteria.WillMatchPrimitiveFiles := AFindControlOptions^.MatchCriteria.WillMatchPrimitiveFiles;
  ADestClkAction.FindControlOptions.MatchCriteria.SearchForControlMode := TSearchForControlMode(AFindControlOptions^.MatchCriteria.SearchForControlMode);

  ADestClkAction.FindControlOptions.AllowToFail := AFindControlOptions^.AllowToFail;
  SetPointedContentToString(@string(PWideChar(AFindControlOptions^.MatchText))[1], ADestClkAction.FindControlOptions.MatchText);
  SetPointedContentToString(@string(PWideChar(AFindControlOptions^.MatchClassName))[1], ADestClkAction.FindControlOptions.MatchClassName);
  SetPointedContentToString(@string(PWideChar(AFindControlOptions^.MatchTextSeparator))[1], ADestClkAction.FindControlOptions.MatchTextSeparator);
  SetPointedContentToString(@string(PWideChar(AFindControlOptions^.MatchClassNameSeparator))[1], ADestClkAction.FindControlOptions.MatchClassNameSeparator);
  //SetLength(ADestClkAction.FindControlOptions.MatchBitmapText, 0);
  //SetPointedContentToString(@string(PWideChar(AFindControlOptions^.MatchBitmapFiles))[1], ADestClkAction.FindControlOptions.MatchBitmapFiles);
  //ADestClkAction.FindControlOptions.MatchBitmapAlgorithm := TMatchBitmapAlgorithm(AFindControlOptions^.MatchBitmapAlgorithm);
  //
  //ADestClkAction.FindControlOptions.MatchBitmapAlgorithmSettings := AFindControlOptions^.MatchBitmapAlgorithmSettings;
  SetPointedContentToString(@string(PWideChar(AFindControlOptions^.InitialRectangle.Left))[1], ADestClkAction.FindControlOptions.InitialRectangle.Left);
  SetPointedContentToString(@string(PWideChar(AFindControlOptions^.InitialRectangle.Top))[1], ADestClkAction.FindControlOptions.InitialRectangle.Top);
  SetPointedContentToString(@string(PWideChar(AFindControlOptions^.InitialRectangle.Right))[1], ADestClkAction.FindControlOptions.InitialRectangle.Right);
  SetPointedContentToString(@string(PWideChar(AFindControlOptions^.InitialRectangle.Bottom))[1], ADestClkAction.FindControlOptions.InitialRectangle.Bottom);
  SetPointedContentToString(@string(PWideChar(AFindControlOptions^.InitialRectangle.LeftOffset))[1], ADestClkAction.FindControlOptions.InitialRectangle.LeftOffset);
  SetPointedContentToString(@string(PWideChar(AFindControlOptions^.InitialRectangle.TopOffset))[1], ADestClkAction.FindControlOptions.InitialRectangle.TopOffset);
  SetPointedContentToString(@string(PWideChar(AFindControlOptions^.InitialRectangle.RightOffset))[1], ADestClkAction.FindControlOptions.InitialRectangle.RightOffset);
  SetPointedContentToString(@string(PWideChar(AFindControlOptions^.InitialRectangle.BottomOffset))[1], ADestClkAction.FindControlOptions.InitialRectangle.BottomOffset);

  ADestClkAction.FindControlOptions.UseWholeScreen := AFindControlOptions^.UseWholeScreen;
  //SetPointedContentToString(@string(PWideChar(AFindControlOptions^.ColorError))[1], ADestClkAction.FindControlOptions.ColorError);
  //SetPointedContentToString(@string(PWideChar(AFindControlOptions^.AllowedColorErrorCount))[1], ADestClkAction.FindControlOptions.AllowedColorErrorCount);
  ADestClkAction.FindControlOptions.WaitForControlToGoAway := AFindControlOptions^.WaitForControlToGoAway;
  ADestClkAction.FindControlOptions.StartSearchingWithCachedControl := AFindControlOptions^.StartSearchingWithCachedControl;
  SetPointedContentToString(@string(PWideChar(AFindControlOptions^.CachedControlLeft))[1], ADestClkAction.FindControlOptions.CachedControlLeft);
  SetPointedContentToString(@string(PWideChar(AFindControlOptions^.CachedControlTop))[1], ADestClkAction.FindControlOptions.CachedControlTop);

  //SetPointedContentToString(@string(PWideChar(AFindControlOptions^.MatchPrimitiveFiles))[1], ADestClkAction.FindControlOptions.MatchPrimitiveFiles);
  ADestClkAction.FindControlOptions.GetAllControls := AFindControlOptions^.GetAllControls;
  //ADestClkAction.FindControlOptions.UseFastSearch := AFindControlOptions^.UseFastSearch;
  //SetPointedContentToString(@string(PWideChar(AFindControlOptions^.FastSearchAllowedColorErrorCount))[1], ADestClkAction.FindControlOptions.FastSearchAllowedColorErrorCount);
  //SetPointedContentToString(@string(PWideChar(AFindControlOptions^.IgnoredColors))[1], ADestClkAction.FindControlOptions.IgnoredColors);
  //ADestClkAction.FindControlOptions.SleepySearch := AFindControlOptions^.SleepySearch;
  //ADestClkAction.FindControlOptions.StopSearchOnMismatch := AFindControlOptions^.StopSearchOnMismatch;

  //ADestClkAction.FindControlOptions.ImageSource := TImageSource(AFindControlOptions^.ImageSource);
  //SetPointedContentToString(@string(PWideChar(AFindControlOptions^.SourceFileName))[1], ADestClkAction.FindControlOptions.SourceFileName);
  //ADestClkAction.FindControlOptions.ImageSourceFileNameLocation := TImageSourceFileNameLocation(AFindControlOptions^.ImageSourceFileNameLocation);

  ADestClkAction.FindControlOptions.PrecisionTimeout := AFindControlOptions^.PrecisionTimeout;
  //ADestClkAction.FindControlOptions.FullBackgroundImageInResult := AFindControlOptions^.FullBackgroundImageInResult;

  //SetPointedContentToString(@string(PWideChar(AFindControlOptions^.MatchByHistogramSettings.MinPercentColorMatch))[1], ADestClkAction.FindControlOptions.MatchByHistogramSettings.MinPercentColorMatch);
  //SetPointedContentToString(@string(PWideChar(AFindControlOptions^.MatchByHistogramSettings.MostSignificantColorCountInSubBmp))[1], ADestClkAction.FindControlOptions.MatchByHistogramSettings.MostSignificantColorCountInSubBmp);
  //SetPointedContentToString(@string(PWideChar(AFindControlOptions^.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp))[1], ADestClkAction.FindControlOptions.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp);

  SetPointedContentToString(@string(PWideChar(AFindControlOptions^.EvaluateTextCount))[1], ADestClkAction.FindControlOptions.EvaluateTextCount);
  //ADestClkAction.FindControlOptions.CropFromScreenshot := AFindControlOptions^.CropFromScreenshot;
  //SetPointedContentToString(@string(PWideChar(AFindControlOptions^.ThreadCount))[1], ADestClkAction.FindControlOptions.ThreadCount);

  //if AFindControlOptions^.MatchBitmapText = nil then  //assume the caller sets this field to nil if not used
  //  Exit;
  ////However, if the field is not nil, then it is either valid or an uninitialized pointer.
  ////The number of profiles is limited (to e.g. 60), in case the pointer is invalid and this library reads junk data.
  //SetLength(ADestClkAction.FindControlOptions.MatchBitmapText, Min(AFindControlOptions^.MatchBitmapText^.ArrLen, 60));
  //
  //for i := 0 to Length(ADestClkAction.FindControlOptions.MatchBitmapText) - 1 do
  //  GetMatchBitmapTextFromAPI(@AFindControlOptions^.MatchBitmapText^.Items^[i], ADestClkAction.FindControlOptions.MatchBitmapText[i]);
end;


procedure GetFindSubControlOptionsFromAPI(AFindSubControlOptions: PClkFindSubControlOptionsAPI; var ADestClkAction: TClkActionRec);
var
  i: Integer;
begin
  //find control stuff
  //ADestClkAction.FindSubControlOptions.MatchCriteria.WillMatchText := AFindSubControlOptions^.MatchCriteria.WillMatchText;
  //ADestClkAction.FindSubControlOptions.MatchCriteria.WillMatchClassName := AFindSubControlOptions^.MatchCriteria.WillMatchClassName;
  ADestClkAction.FindSubControlOptions.MatchCriteria.WillMatchBitmapText := AFindSubControlOptions^.MatchCriteria.WillMatchBitmapText;
  ADestClkAction.FindSubControlOptions.MatchCriteria.WillMatchBitmapFiles := AFindSubControlOptions^.MatchCriteria.WillMatchBitmapFiles;
  ADestClkAction.FindSubControlOptions.MatchCriteria.WillMatchPrimitiveFiles := AFindSubControlOptions^.MatchCriteria.WillMatchPrimitiveFiles;
  //ADestClkAction.FindSubControlOptions.MatchCriteria.SearchForControlMode := TSearchForControlMode(AFindSubControlOptions^.MatchCriteria.SearchForControlMode);

  ADestClkAction.FindSubControlOptions.AllowToFail := AFindSubControlOptions^.AllowToFail;
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.MatchText))[1], ADestClkAction.FindSubControlOptions.MatchText);
  SetLength(ADestClkAction.FindSubControlOptions.MatchBitmapText, 0);
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.MatchBitmapFiles))[1], ADestClkAction.FindSubControlOptions.MatchBitmapFiles);
  ADestClkAction.FindSubControlOptions.MatchBitmapAlgorithm := TMatchBitmapAlgorithm(AFindSubControlOptions^.MatchBitmapAlgorithm);

  ADestClkAction.FindSubControlOptions.MatchBitmapAlgorithmSettings := AFindSubControlOptions^.MatchBitmapAlgorithmSettings;
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.InitialRectangle.Left))[1], ADestClkAction.FindSubControlOptions.InitialRectangle.Left);
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.InitialRectangle.Top))[1], ADestClkAction.FindSubControlOptions.InitialRectangle.Top);
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.InitialRectangle.Right))[1], ADestClkAction.FindSubControlOptions.InitialRectangle.Right);
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.InitialRectangle.Bottom))[1], ADestClkAction.FindSubControlOptions.InitialRectangle.Bottom);
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.InitialRectangle.LeftOffset))[1], ADestClkAction.FindSubControlOptions.InitialRectangle.LeftOffset);
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.InitialRectangle.TopOffset))[1], ADestClkAction.FindSubControlOptions.InitialRectangle.TopOffset);
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.InitialRectangle.RightOffset))[1], ADestClkAction.FindSubControlOptions.InitialRectangle.RightOffset);
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.InitialRectangle.BottomOffset))[1], ADestClkAction.FindSubControlOptions.InitialRectangle.BottomOffset);

  ADestClkAction.FindSubControlOptions.UseWholeScreen := AFindSubControlOptions^.UseWholeScreen;
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.ColorError))[1], ADestClkAction.FindSubControlOptions.ColorError);
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.AllowedColorErrorCount))[1], ADestClkAction.FindSubControlOptions.AllowedColorErrorCount);
  ADestClkAction.FindSubControlOptions.WaitForControlToGoAway := AFindSubControlOptions^.WaitForControlToGoAway;
  ADestClkAction.FindSubControlOptions.StartSearchingWithCachedControl := AFindSubControlOptions^.StartSearchingWithCachedControl;
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.CachedControlLeft))[1], ADestClkAction.FindSubControlOptions.CachedControlLeft);
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.CachedControlTop))[1], ADestClkAction.FindSubControlOptions.CachedControlTop);

  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.MatchPrimitiveFiles))[1], ADestClkAction.FindSubControlOptions.MatchPrimitiveFiles);
  ADestClkAction.FindSubControlOptions.GetAllControls := AFindSubControlOptions^.GetAllControls;
  ADestClkAction.FindSubControlOptions.UseFastSearch := AFindSubControlOptions^.UseFastSearch;
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.FastSearchAllowedColorErrorCount))[1], ADestClkAction.FindSubControlOptions.FastSearchAllowedColorErrorCount);
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.IgnoredColors))[1], ADestClkAction.FindSubControlOptions.IgnoredColors);
  ADestClkAction.FindSubControlOptions.SleepySearch := AFindSubControlOptions^.SleepySearch;
  ADestClkAction.FindSubControlOptions.StopSearchOnMismatch := AFindSubControlOptions^.StopSearchOnMismatch;

  ADestClkAction.FindSubControlOptions.ImageSource := TImageSource(AFindSubControlOptions^.ImageSource);
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.SourceFileName))[1], ADestClkAction.FindSubControlOptions.SourceFileName);
  ADestClkAction.FindSubControlOptions.ImageSourceFileNameLocation := TImageSourceFileNameLocation(AFindSubControlOptions^.ImageSourceFileNameLocation);

  ADestClkAction.FindSubControlOptions.PrecisionTimeout := AFindSubControlOptions^.PrecisionTimeout;
  ADestClkAction.FindSubControlOptions.FullBackgroundImageInResult := AFindSubControlOptions^.FullBackgroundImageInResult;

  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.MatchByHistogramSettings.MinPercentColorMatch))[1], ADestClkAction.FindSubControlOptions.MatchByHistogramSettings.MinPercentColorMatch);
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.MatchByHistogramSettings.MostSignificantColorCountInSubBmp))[1], ADestClkAction.FindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInSubBmp);
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp))[1], ADestClkAction.FindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp);

  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.EvaluateTextCount))[1], ADestClkAction.FindSubControlOptions.EvaluateTextCount);
  ADestClkAction.FindSubControlOptions.CropFromScreenshot := AFindSubControlOptions^.CropFromScreenshot;
  SetPointedContentToString(@string(PWideChar(AFindSubControlOptions^.ThreadCount))[1], ADestClkAction.FindSubControlOptions.ThreadCount);

  if AFindSubControlOptions^.MatchBitmapText = nil then  //assume the caller sets this field to nil if not used
    Exit;
  //However, if the field is not nil, then it is either valid or an uninitialized pointer.
  //The number of profiles is limited (to e.g. 60), in case the pointer is invalid and this library reads junk data.
  SetLength(ADestClkAction.FindSubControlOptions.MatchBitmapText, Min(AFindSubControlOptions^.MatchBitmapText^.ArrLen, 60));

  for i := 0 to Length(ADestClkAction.FindSubControlOptions.MatchBitmapText) - 1 do
    GetMatchBitmapTextFromAPI(@AFindSubControlOptions^.MatchBitmapText^.Items^[i], ADestClkAction.FindSubControlOptions.MatchBitmapText[i]);
end;


procedure GetSetControlTextOptionsFromAPI(ASetControlTextOptions: PClkSetTextOptionsAPI; var ADestClkAction: TClkActionRec);
begin
  //set control text stuff
  SetPointedContentToString(@string(PWideChar(ASetControlTextOptions^.Text))[1], ADestClkAction.SetTextOptions.Text);
  ADestClkAction.SetTextOptions.ControlType := TClkSetTextControlType(ASetControlTextOptions^.ControlType);
  SetPointedContentToString(@string(PWideChar(ASetControlTextOptions^.DelayBetweenKeyStrokes))[1], ADestClkAction.SetTextOptions.DelayBetweenKeyStrokes);
  SetPointedContentToString(@string(PWideChar(ASetControlTextOptions^.Count))[1], ADestClkAction.SetTextOptions.Count);
end;


procedure GetCallTemplateOptionsFromAPI(ACallTemplateOptions: PClkCallTemplateOptionsAPI; var ADestClkAction: TClkActionRec);
begin
  //call template stuff
  SetPointedContentToString(@string(PWideChar(ACallTemplateOptions^.TemplateFileName))[1], ADestClkAction.CallTemplateOptions.TemplateFileName);
  SetPointedContentToString(@string(PWideChar(ACallTemplateOptions^.ListOfCustomVarsAndValues))[1], ADestClkAction.CallTemplateOptions.ListOfCustomVarsAndValues);
  ADestClkAction.CallTemplateOptions.EvaluateBeforeCalling := ACallTemplateOptions^.EvaluateBeforeCalling;

  ADestClkAction.CallTemplateOptions.CallTemplateLoop.Enabled := ACallTemplateOptions^.CallTemplateLoop.Enabled;
  SetPointedContentToString(@string(PWideChar(ACallTemplateOptions^.CallTemplateLoop.Counter))[1], ADestClkAction.CallTemplateOptions.CallTemplateLoop.Counter);
  SetPointedContentToString(@string(PWideChar(ACallTemplateOptions^.CallTemplateLoop.InitValue))[1], ADestClkAction.CallTemplateOptions.CallTemplateLoop.InitValue);
  SetPointedContentToString(@string(PWideChar(ACallTemplateOptions^.CallTemplateLoop.EndValue))[1], ADestClkAction.CallTemplateOptions.CallTemplateLoop.EndValue);
  ADestClkAction.CallTemplateOptions.CallTemplateLoop.Direction := TLoopDirection(ACallTemplateOptions^.CallTemplateLoop.Direction);
  SetPointedContentToString(@string(PWideChar(ACallTemplateOptions^.CallTemplateLoop.BreakCondition))[1], ADestClkAction.CallTemplateOptions.CallTemplateLoop.BreakCondition);
  ADestClkAction.CallTemplateOptions.CallTemplateLoop.EvalBreakPosition := TLoopEvalBreakPosition(ACallTemplateOptions^.CallTemplateLoop.EvalBreakPosition);
end;


procedure GetSleepOptionsFromAPI(ASleepOptions: PClkSleepOptionsAPI; var ADestClkAction: TClkActionRec);
begin
  //sleep stuff
  SetPointedContentToString(@string(PWideChar(ASleepOptions^.Value))[1], ADestClkAction.SleepOptions.Value);
end;


function GetSetVarOptionsFromAPI(ASetVarOptions: PClkSetVarOptionsAPI; var ADestClkAction: TClkActionRec): Integer;
var
  ListOfVarNames, ListOfVarValues, ListOfVarEvalBefore: TStringList;
begin
  //SetVar stuff
  SetPointedContentToString(@string(PWideChar(ASetVarOptions^.ListOfVarNames))[1], ADestClkAction.SetVarOptions.ListOfVarNames);
  SetPointedContentToString(@string(PWideChar(ASetVarOptions^.ListOfVarValues))[1], ADestClkAction.SetVarOptions.ListOfVarValues);
  SetPointedContentToString(@string(PWideChar(ASetVarOptions^.ListOfVarEvalBefore))[1], ADestClkAction.SetVarOptions.ListOfVarEvalBefore);
  ADestClkAction.SetVarOptions.FailOnException := ASetVarOptions^.FailOnException;

  Result := 0;

  ListOfVarNames := TStringList.Create;
  ListOfVarValues := TStringList.Create;
  ListOfVarEvalBefore := TStringList.Create;
  try
    ListOfVarNames.LineBreak := #13#10;
    ListOfVarValues.LineBreak := #13#10;
    ListOfVarEvalBefore.LineBreak := #13#10;
    ListOfVarNames.Text := ADestClkAction.SetVarOptions.ListOfVarNames;
    ListOfVarValues.Text := ADestClkAction.SetVarOptions.ListOfVarValues;
    ListOfVarEvalBefore.Text := ADestClkAction.SetVarOptions.ListOfVarEvalBefore;

    if (ListOfVarNames.Count <> ListOfVarValues.Count) or (ListOfVarNames.Count <> ListOfVarEvalBefore.Count) then
    begin
      Result := -1;

      ListOfVarNames.Clear;
      ListOfVarValues.Clear;
      ListOfVarEvalBefore.Clear;

      ListOfVarNames.Add('$DLL_Err_Var$');
      ListOfVarValues.Add('The number of items from ListOfVarValues or ListOfVarEvalBefore, does not match the number of items from ListOfVarNames.');
      ListOfVarEvalBefore.Add('0');

      ListOfVarNames.Add('$DLL_Err_ListOfVarNames.Count$');
      ListOfVarNames.Add('$DLL_Err_ListOfVarValues.Count$');
      ListOfVarNames.Add('$DLL_Err_ListOfVarEvalBefore.Count$');
      ListOfVarValues.Add(IntToStr(ListOfVarNames.Count));
      ListOfVarValues.Add(IntToStr(ListOfVarValues.Count));
      ListOfVarValues.Add(IntToStr(ListOfVarEvalBefore.Count));
      ListOfVarEvalBefore.Add('0');
      ListOfVarEvalBefore.Add('0');
      ListOfVarEvalBefore.Add('0');

      ADestClkAction.SetVarOptions.ListOfVarNames := ListOfVarNames.Text;
      ADestClkAction.SetVarOptions.ListOfVarValues := ListOfVarValues.Text;
      ADestClkAction.SetVarOptions.ListOfVarEvalBefore := ListOfVarEvalBefore.Text;

      //Do not exit, simply create the bad action, then let the user debug it.
    end;
  finally
    ListOfVarNames.Free;
    ListOfVarValues.Free;
    ListOfVarEvalBefore.Free;
  end;
end;


procedure GetWindowOperationsOptionsFromAPI(AWindowOperationsOptions: PClkWindowOperationsOptionsAPI; var ADestClkAction: TClkActionRec);
begin
  //Window Operations stuff
  ADestClkAction.WindowOperationsOptions.Operation := TWindowOperation(AWindowOperationsOptions^.Operation);
  SetPointedContentToString(@string(PWideChar(AWindowOperationsOptions^.NewX))[1], ADestClkAction.WindowOperationsOptions.NewX);
  SetPointedContentToString(@string(PWideChar(AWindowOperationsOptions^.NewY))[1], ADestClkAction.WindowOperationsOptions.NewY);
  SetPointedContentToString(@string(PWideChar(AWindowOperationsOptions^.NewWidth))[1], ADestClkAction.WindowOperationsOptions.NewWidth);
  SetPointedContentToString(@string(PWideChar(AWindowOperationsOptions^.NewHeight))[1], ADestClkAction.WindowOperationsOptions.NewHeight);
  ADestClkAction.WindowOperationsOptions.NewPositionEnabled := AWindowOperationsOptions^.NewPositionEnabled;
  ADestClkAction.WindowOperationsOptions.NewSizeEnabled := AWindowOperationsOptions^.NewSizeEnabled;
end;


procedure GetLoadSetVarFromFileOptionsFromAPI(ALoadSetVarFromFileOptions: PClkLoadSetVarFromFileOptionsAPI; var ADestClkAction: TClkActionRec);
begin
  //LoadSetVarFromFile stuff
  SetPointedContentToString(@string(PWideChar(ALoadSetVarFromFileOptions^.FileName))[1], ADestClkAction.LoadSetVarFromFileOptions.FileName);
  SetPointedContentToString(@string(PWideChar(ALoadSetVarFromFileOptions^.SetVarActionName))[1], ADestClkAction.LoadSetVarFromFileOptions.SetVarActionName);
end;


procedure GetSaveSetVarToFileOptionsFromAPI(ASaveSetVarToFileOptions: PClkSaveSetVarToFileOptionsAPI; var ADestClkAction: TClkActionRec);
begin
  //SaveSetVarToFile stuff
  SetPointedContentToString(@string(PWideChar(ASaveSetVarToFileOptions^.FileName))[1], ADestClkAction.SaveSetVarToFileOptions.FileName);
  SetPointedContentToString(@string(PWideChar(ASaveSetVarToFileOptions^.SetVarActionName))[1], ADestClkAction.SaveSetVarToFileOptions.SetVarActionName);
end;


procedure GetPluginOptionsFromAPI(APluginOptions: PClkPluginOptionsAPI; var ADestClkAction: TClkActionRec);
begin
  //Plugin stuff
  SetPointedContentToString(@string(PWideChar(APluginOptions^.FileName))[1], ADestClkAction.PluginOptions.FileName);
  SetPointedContentToString(@string(PWideChar(APluginOptions^.ListOfPropertiesAndValues))[1], ADestClkAction.PluginOptions.ListOfPropertiesAndValues);
end;


procedure GetEditTemplateOptionsFromAPI(AEditTemplateOptions: PClkEditTemplateOptionsAPI; var ADestClkAction: TClkActionRec);
begin
  //EditTemplate stuff
  ADestClkAction.EditTemplateOptions.Operation := TEditTemplateOperation(AEditTemplateOptions^.Operation);
  ADestClkAction.EditTemplateOptions.WhichTemplate := TEditTemplateWhichTemplate(AEditTemplateOptions^.WhichTemplate);
  SetPointedContentToString(@string(PWideChar(AEditTemplateOptions^.TemplateFileName))[1], ADestClkAction.EditTemplateOptions.TemplateFileName);
  SetPointedContentToString(@string(PWideChar(AEditTemplateOptions^.ListOfEditedProperties))[1], ADestClkAction.EditTemplateOptions.ListOfEditedProperties);
  SetPointedContentToString(@string(PWideChar(AEditTemplateOptions^.ListOfEnabledProperties))[1], ADestClkAction.EditTemplateOptions.ListOfEnabledProperties);
  SetPointedContentToString(@string(PWideChar(AEditTemplateOptions^.EditedActionName))[1], ADestClkAction.EditTemplateOptions.EditedActionName);
  ADestClkAction.EditTemplateOptions.EditedActionType := TClkAction(AEditTemplateOptions^.EditedActionType);
  SetPointedContentToString(@string(PWideChar(AEditTemplateOptions^.EditedActionCondition))[1], ADestClkAction.EditTemplateOptions.EditedActionCondition);
  ADestClkAction.EditTemplateOptions.EditedActionTimeout := AEditTemplateOptions^.EditedActionTimeout;
  SetPointedContentToString(@string(PWideChar(AEditTemplateOptions^.NewActionName))[1], ADestClkAction.EditTemplateOptions.NewActionName);
  ADestClkAction.EditTemplateOptions.ShouldSaveTemplate := AEditTemplateOptions^.ShouldSaveTemplate;
end;


//


procedure SetMatchBitmapTextToAPI(var AMatchBitmapText: TClkFindControlMatchBitmapText; var ADestMatchBitmapText: TClkFindControlMatchBitmapTextAPI; var ATempDestWideStr: TClkFindControlMatchBitmapTextWideString);
begin
  //ADestMatchBitmapText.ForegroundColor := @WideString(AMatchBitmapText.ForegroundColor)[1];
  //ADestMatchBitmapText.BackgroundColor := @WideString(AMatchBitmapText.BackgroundColor)[1];
  //ADestMatchBitmapText.FontName := @WideString(AMatchBitmapText.FontName)[1];

  ATempDestWideStr.ForegroundColor := WideString(AMatchBitmapText.ForegroundColor);    //using ATempDestWideStr, because the strings must remain in memory (at least for a while)
  ATempDestWideStr.BackgroundColor := WideString(AMatchBitmapText.BackgroundColor);
  ATempDestWideStr.FontName := WideString(AMatchBitmapText.FontName);

  ADestMatchBitmapText.ForegroundColor := @(ATempDestWideStr.ForegroundColor)[1];
  ADestMatchBitmapText.BackgroundColor := @(ATempDestWideStr.BackgroundColor)[1];
  ADestMatchBitmapText.FontName := @(ATempDestWideStr.FontName)[1];

  ADestMatchBitmapText.FontSize := AMatchBitmapText.FontSize;
  ADestMatchBitmapText.Bold := AMatchBitmapText.Bold;
  ADestMatchBitmapText.Italic := AMatchBitmapText.Italic;
  ADestMatchBitmapText.Underline := AMatchBitmapText.Underline;
  ADestMatchBitmapText.StrikeOut := AMatchBitmapText.StrikeOut;
  ADestMatchBitmapText.FontQuality := Byte(AMatchBitmapText.FontQuality);
  ADestMatchBitmapText.FontQualityUsesReplacement := AMatchBitmapText.FontQualityUsesReplacement;

  //ADestMatchBitmapText.FontQualityReplacement := @WideString(AMatchBitmapText.FontQualityReplacement)[1];
  //ADestMatchBitmapText.ProfileName := @WideString(AMatchBitmapText.ProfileName)[1];
  //ADestMatchBitmapText.CropLeft := @WideString(AMatchBitmapText.CropLeft)[1];
  //ADestMatchBitmapText.CropTop := @WideString(AMatchBitmapText.CropTop)[1];
  //ADestMatchBitmapText.CropRight := @WideString(AMatchBitmapText.CropRight)[1];
  //ADestMatchBitmapText.CropBottom := @WideString(AMatchBitmapText.CropBottom)[1];

  ATempDestWideStr.FontQualityReplacement := WideString(AMatchBitmapText.FontQualityReplacement);
  ATempDestWideStr.ProfileName := WideString(AMatchBitmapText.ProfileName);
  ATempDestWideStr.CropLeft := WideString(AMatchBitmapText.CropLeft);
  ATempDestWideStr.CropTop := WideString(AMatchBitmapText.CropTop);
  ATempDestWideStr.CropRight := WideString(AMatchBitmapText.CropRight);
  ATempDestWideStr.CropBottom := WideString(AMatchBitmapText.CropBottom);

  ADestMatchBitmapText.FontQualityReplacement := @(ATempDestWideStr.FontQualityReplacement)[1];
  ADestMatchBitmapText.ProfileName := @(ATempDestWideStr.ProfileName)[1];
  ADestMatchBitmapText.CropLeft := @(ATempDestWideStr.CropLeft)[1];
  ADestMatchBitmapText.CropTop := @(ATempDestWideStr.CropTop)[1];
  ADestMatchBitmapText.CropRight := @(ATempDestWideStr.CropRight)[1];
  ADestMatchBitmapText.CropBottom := @(ATempDestWideStr.CropBottom)[1];

  ADestMatchBitmapText.IgnoreBackgroundColor := AMatchBitmapText.IgnoreBackgroundColor;
end;


procedure SetClickOptionsToAPI(var AClickOptions: TClkClickOptions; var ADestClickOptions: TClkClickOptionsAPI; var ATempClickOptionsAPIWS: TClkClickOptionsAPIWS);
begin
  ADestClickOptions.XClickPointReference := Byte(AClickOptions.XClickPointReference);
  ADestClickOptions.YClickPointReference := Byte(AClickOptions.YClickPointReference);
  ATempClickOptionsAPIWS.XClickPointVar := WideString(AClickOptions.XClickPointVar);  ADestClickOptions.XClickPointVar := @ATempClickOptionsAPIWS.XClickPointVar[1];// ADestClickOptions.XClickPointVar := @WideString(AClickOptions.XClickPointVar)[1];
  ATempClickOptionsAPIWS.YClickPointVar := WideString(AClickOptions.YClickPointVar);  ADestClickOptions.YClickPointVar := @ATempClickOptionsAPIWS.YClickPointVar[1];// ADestClickOptions.YClickPointVar := @WideString(AClickOptions.YClickPointVar)[1];
  ATempClickOptionsAPIWS.XOffset := WideString(AClickOptions.XOffset);  ADestClickOptions.XOffset := @ATempClickOptionsAPIWS.XOffset[1];// ADestClickOptions.XOffset := @WideString(AClickOptions.XOffset)[1];
  ATempClickOptionsAPIWS.YOffset := WideString(AClickOptions.YOffset);  ADestClickOptions.YOffset := @ATempClickOptionsAPIWS.YOffset[1];// ADestClickOptions.YOffset := @WideString(AClickOptions.YOffset)[1];
  ADestClickOptions.MouseButton := Byte(AClickOptions.MouseButton);
  ADestClickOptions.ClickWithCtrl := AClickOptions.ClickWithCtrl;
  ADestClickOptions.ClickWithAlt := AClickOptions.ClickWithAlt;
  ADestClickOptions.ClickWithShift := AClickOptions.ClickWithShift;
  ADestClickOptions.ClickWithDoubleClick := AClickOptions.ClickWithDoubleClick;
  ADestClickOptions.Count := AClickOptions.Count;
  ADestClickOptions.LeaveMouse := AClickOptions.LeaveMouse;
  ADestClickOptions.MoveWithoutClick := AClickOptions.MoveWithoutClick;
  ADestClickOptions.ClickType := AClickOptions.ClickType;
  ADestClickOptions.XClickPointReferenceDest := Byte(AClickOptions.XClickPointReferenceDest);
  ADestClickOptions.YClickPointReferenceDest := Byte(AClickOptions.YClickPointReferenceDest);
  ATempClickOptionsAPIWS.XClickPointVarDest := WideString(AClickOptions.XClickPointVarDest);  ADestClickOptions.XClickPointVarDest := @ATempClickOptionsAPIWS.XClickPointVarDest[1];// ADestClickOptions.XClickPointVarDest := @WideString(AClickOptions.XClickPointVarDest)[1];
  ATempClickOptionsAPIWS.YClickPointVarDest := WideString(AClickOptions.YClickPointVarDest);  ADestClickOptions.YClickPointVarDest := @ATempClickOptionsAPIWS.YClickPointVarDest[1];// ADestClickOptions.YClickPointVarDest := @WideString(AClickOptions.YClickPointVarDest)[1];
  ATempClickOptionsAPIWS.XOffsetDest := WideString(AClickOptions.XOffsetDest);  ADestClickOptions.XOffsetDest := @ATempClickOptionsAPIWS.XOffsetDest[1];// ADestClickOptions.XOffsetDest := @WideString(AClickOptions.XOffsetDest)[1];
  ATempClickOptionsAPIWS.YOffsetDest := WideString(AClickOptions.YOffsetDest);  ADestClickOptions.YOffsetDest := @ATempClickOptionsAPIWS.YOffsetDest[1];// ADestClickOptions.YOffsetDest := @WideString(AClickOptions.YOffsetDest)[1];
  ADestClickOptions.MouseWheelType := Byte(AClickOptions.MouseWheelType);
  ATempClickOptionsAPIWS.MouseWheelAmount := WideString(AClickOptions.MouseWheelAmount);  ADestClickOptions.MouseWheelAmount := @ATempClickOptionsAPIWS.MouseWheelAmount[1];// ADestClickOptions.MouseWheelAmount := @WideString(AClickOptions.MouseWheelAmount)[1];
  ATempClickOptionsAPIWS.DelayAfterMovingToDestination := WideString(AClickOptions.DelayAfterMovingToDestination);  ADestClickOptions.DelayAfterMovingToDestination := @ATempClickOptionsAPIWS.DelayAfterMovingToDestination[1];// ADestClickOptions.DelayAfterMovingToDestination := @WideString(AClickOptions.DelayAfterMovingToDestination)[1];
  ATempClickOptionsAPIWS.DelayAfterMouseDown := WideString(AClickOptions.DelayAfterMouseDown);  ADestClickOptions.DelayAfterMouseDown := @ATempClickOptionsAPIWS.DelayAfterMouseDown[1];// ADestClickOptions.DelayAfterMouseDown := @WideString(AClickOptions.DelayAfterMouseDown)[1];
  ATempClickOptionsAPIWS.MoveDuration := WideString(AClickOptions.MoveDuration);  ADestClickOptions.MoveDuration := @ATempClickOptionsAPIWS.MoveDuration[1];// ADestClickOptions.MoveDuration := @WideString(AClickOptions.MoveDuration)[1];
  ADestClickOptions.UseClipCursor := AClickOptions.UseClipCursor;
end;


procedure SetExecAppOptionsToAPI(var AExecAppOptions: TClkExecAppOptions; var ADestExecAppOptions: TClkExecAppOptionsAPI; var ATempExecAppOptionsAPIWS: TClkExecAppOptionsAPIWS);
begin
  ATempExecAppOptionsAPIWS.PathToApp := WideString(AExecAppOptions.PathToApp);  ADestExecAppOptions.PathToApp := @ATempExecAppOptionsAPIWS.PathToApp[1];// ADestExecAppOptions.PathToApp := @WideString(AExecAppOptions.PathToApp)[1];
  ATempExecAppOptionsAPIWS.ListOfParams := WideString(AExecAppOptions.ListOfParams);  ADestExecAppOptions.ListOfParams := @ATempExecAppOptionsAPIWS.ListOfParams[1];// ADestExecAppOptions.ListOfParams := @WideString(AExecAppOptions.ListOfParams)[1];
  ADestExecAppOptions.WaitForApp := AExecAppOptions.WaitForApp;
  ATempExecAppOptionsAPIWS.AppStdIn := WideString(AExecAppOptions.AppStdIn);  ADestExecAppOptions.AppStdIn := @ATempExecAppOptionsAPIWS.AppStdIn[1];// ADestExecAppOptions.AppStdIn := @WideString(AExecAppOptions.AppStdIn)[1];
  ATempExecAppOptionsAPIWS.CurrentDir := WideString(AExecAppOptions.CurrentDir);  ADestExecAppOptions.CurrentDir := @ATempExecAppOptionsAPIWS.CurrentDir[1];// ADestExecAppOptions.CurrentDir := @WideString(AExecAppOptions.CurrentDir)[1];
  ADestExecAppOptions.UseInheritHandles := Byte(AExecAppOptions.UseInheritHandles);
  ADestExecAppOptions.NoConsole := AExecAppOptions.NoConsole;
end;


procedure SetFindControlOptionsToAPI(var AFindControlOptions: TClkFindControlOptions; var ADestFindControlOptions: TClkFindControlOptionsAPI; var ADestMatchBitmapTextRecAPI: TMatchBitmapTextRecAPI; var ADestMatchBitmapTextArray: TClkFindControlMatchBitmapTextAPIArr; var ATempFindControlOptionsAPIWS: TClkFindControlOptionsAPIWS);
begin
  ADestFindControlOptions.MatchCriteria.WillMatchText := AFindControlOptions.MatchCriteria.WillMatchText;
  ADestFindControlOptions.MatchCriteria.WillMatchClassName := AFindControlOptions.MatchCriteria.WillMatchClassName;
  ADestFindControlOptions.MatchCriteria.SearchForControlMode := LongInt(AFindControlOptions.MatchCriteria.SearchForControlMode);

  ADestFindControlOptions.AllowToFail := AFindControlOptions.AllowToFail;
  ATempFindControlOptionsAPIWS.MatchText := WideString(AFindControlOptions.MatchText);  ADestFindControlOptions.MatchText := @ATempFindControlOptionsAPIWS.MatchText[1];// ADestFindControlOptions.MatchText := @WideString(AFindControlOptions.MatchText)[1];
  ATempFindControlOptionsAPIWS.MatchClassName := WideString(AFindControlOptions.MatchClassName);  ADestFindControlOptions.MatchClassName := @ATempFindControlOptionsAPIWS.MatchClassName[1];// ADestFindControlOptions.MatchClassName := @WideString(AFindControlOptions.MatchClassName)[1];
  ATempFindControlOptionsAPIWS.MatchTextSeparator := WideString(AFindControlOptions.MatchTextSeparator);  ADestFindControlOptions.MatchTextSeparator := @ATempFindControlOptionsAPIWS.MatchTextSeparator[1];// ADestFindControlOptions.MatchTextSeparator := @WideString(AFindControlOptions.MatchTextSeparator)[1];
  ATempFindControlOptionsAPIWS.MatchClassNameSeparator := WideString(AFindControlOptions.MatchClassNameSeparator);  ADestFindControlOptions.MatchClassNameSeparator := @ATempFindControlOptionsAPIWS.MatchClassNameSeparator[1];// ADestFindControlOptions.MatchClassNameSeparator := @WideString(AFindControlOptions.MatchClassNameSeparator)[1];
  //
  ATempFindControlOptionsAPIWS.InitialRectangle.Left := WideString(AFindControlOptions.InitialRectangle.Left);  ADestFindControlOptions.InitialRectangle.Left := @ATempFindControlOptionsAPIWS.InitialRectangle.Left[1];// ADestFindControlOptions.InitialRectangle.Left := @WideString(AFindControlOptions.InitialRectangle.Left)[1];
  ATempFindControlOptionsAPIWS.InitialRectangle.Top := WideString(AFindControlOptions.InitialRectangle.Top);  ADestFindControlOptions.InitialRectangle.Top := @ATempFindControlOptionsAPIWS.InitialRectangle.Top[1];// ADestFindControlOptions.InitialRectangle.Top := @WideString(AFindControlOptions.InitialRectangle.Top)[1];
  ATempFindControlOptionsAPIWS.InitialRectangle.Right := WideString(AFindControlOptions.InitialRectangle.Right);  ADestFindControlOptions.InitialRectangle.Right := @ATempFindControlOptionsAPIWS.InitialRectangle.Right[1];// ADestFindControlOptions.InitialRectangle.Right := @WideString(AFindControlOptions.InitialRectangle.Right)[1];
  ATempFindControlOptionsAPIWS.InitialRectangle.Bottom := WideString(AFindControlOptions.InitialRectangle.Bottom);  ADestFindControlOptions.InitialRectangle.Bottom := @ATempFindControlOptionsAPIWS.InitialRectangle.Bottom[1];// ADestFindControlOptions.InitialRectangle.Bottom := @WideString(AFindControlOptions.InitialRectangle.Bottom)[1];
  ATempFindControlOptionsAPIWS.InitialRectangle.LeftOffset := WideString(AFindControlOptions.InitialRectangle.LeftOffset);  ADestFindControlOptions.InitialRectangle.LeftOffset := @ATempFindControlOptionsAPIWS.InitialRectangle.LeftOffset[1];// ADestFindControlOptions.InitialRectangle.LeftOffset := @WideString(AFindControlOptions.InitialRectangle.LeftOffset)[1];
  ATempFindControlOptionsAPIWS.InitialRectangle.TopOffset := WideString(AFindControlOptions.InitialRectangle.TopOffset);  ADestFindControlOptions.InitialRectangle.TopOffset := @ATempFindControlOptionsAPIWS.InitialRectangle.TopOffset[1];// ADestFindControlOptions.InitialRectangle.TopOffset := @WideString(AFindControlOptions.InitialRectangle.TopOffset)[1];
  ATempFindControlOptionsAPIWS.InitialRectangle.RightOffset := WideString(AFindControlOptions.InitialRectangle.RightOffset);  ADestFindControlOptions.InitialRectangle.RightOffset := @ATempFindControlOptionsAPIWS.InitialRectangle.RightOffset[1];// ADestFindControlOptions.InitialRectangle.RightOffset := @WideString(AFindControlOptions.InitialRectangle.RightOffset)[1];
  ATempFindControlOptionsAPIWS.InitialRectangle.BottomOffset := WideString(AFindControlOptions.InitialRectangle.BottomOffset);  ADestFindControlOptions.InitialRectangle.BottomOffset := @ATempFindControlOptionsAPIWS.InitialRectangle.BottomOffset[1];// ADestFindControlOptions.InitialRectangle.BottomOffset := @WideString(AFindControlOptions.InitialRectangle.BottomOffset)[1];

  ADestFindControlOptions.UseWholeScreen := AFindControlOptions.UseWholeScreen;
  ADestFindControlOptions.WaitForControlToGoAway := AFindControlOptions.WaitForControlToGoAway;
  ADestFindControlOptions.StartSearchingWithCachedControl := AFindControlOptions.StartSearchingWithCachedControl;
  ATempFindControlOptionsAPIWS.CachedControlLeft := WideString(AFindControlOptions.CachedControlLeft);  ADestFindControlOptions.CachedControlLeft := @ATempFindControlOptionsAPIWS.CachedControlLeft[1];// ADestFindControlOptions.CachedControlLeft := @WideString(AFindControlOptions.CachedControlLeft)[1];
  ATempFindControlOptionsAPIWS.CachedControlTop := WideString(AFindControlOptions.CachedControlTop);  ADestFindControlOptions.CachedControlTop := @ATempFindControlOptionsAPIWS.CachedControlTop[1];// ADestFindControlOptions.CachedControlTop := @WideString(AFindControlOptions.CachedControlTop)[1];

  ADestFindControlOptions.GetAllControls := AFindControlOptions.GetAllControls;
   ADestFindControlOptions.PrecisionTimeout := AFindControlOptions.PrecisionTimeout;
  ATempFindControlOptionsAPIWS.EvaluateTextCount := WideString(AFindControlOptions.EvaluateTextCount);  ADestFindControlOptions.EvaluateTextCount := @ATempFindControlOptionsAPIWS.EvaluateTextCount[1];// ADestFindControlOptions.EvaluateTextCount := @WideString(AFindControlOptions.EvaluateTextCount)[1];
end;


procedure SetFindSubControlOptionsToAPI(var AFindSubControlOptions: TClkFindSubControlOptions; var ADestFindSubControlOptions: TClkFindSubControlOptionsAPI; var ADestMatchBitmapTextRecAPI: TMatchBitmapTextRecAPI; var ADestMatchBitmapTextArray: TClkFindControlMatchBitmapTextAPIArr; var ATempFindSubControlOptionsAPIWS: TClkFindSubControlOptionsAPIWS);
var
  i: Integer;
  TempDestWideStr: TClkFindControlMatchBitmapTextWideStringArr;   /////////////////////// If there are still memory overwriting issues, then this variable must moved as a procedure parameter, similar to the ADestMatchBitmapTextArray array. This will keep it in memory during the API call.
begin
  ADestFindSubControlOptions.MatchCriteria.WillMatchBitmapText := AFindSubControlOptions.MatchCriteria.WillMatchBitmapText;
  ADestFindSubControlOptions.MatchCriteria.WillMatchBitmapFiles := AFindSubControlOptions.MatchCriteria.WillMatchBitmapFiles;
  ADestFindSubControlOptions.MatchCriteria.WillMatchPrimitiveFiles := AFindSubControlOptions.MatchCriteria.WillMatchPrimitiveFiles;

  ADestFindSubControlOptions.AllowToFail := AFindSubControlOptions.AllowToFail;
  ATempFindSubControlOptionsAPIWS.MatchText := WideString(AFindSubControlOptions.MatchText);  ADestFindSubControlOptions.MatchText := @ATempFindSubControlOptionsAPIWS.MatchText[1];// ADestFindSubControlOptions.MatchText := @WideString(AFindSubControlOptions.MatchText)[1];
  ADestFindSubControlOptions.MatchBitmapText := nil;
  ATempFindSubControlOptionsAPIWS.MatchBitmapFiles := WideString(AFindSubControlOptions.MatchBitmapFiles);  ADestFindSubControlOptions.MatchBitmapFiles := @ATempFindSubControlOptionsAPIWS.MatchBitmapFiles[1];// ADestFindSubControlOptions.MatchBitmapFiles := @WideString(AFindSubControlOptions.MatchBitmapFiles)[1];
  ADestFindSubControlOptions.MatchBitmapAlgorithm := Byte(AFindSubControlOptions.MatchBitmapAlgorithm);

  ADestFindSubControlOptions.MatchBitmapAlgorithmSettings := AFindSubControlOptions.MatchBitmapAlgorithmSettings;
  ATempFindSubControlOptionsAPIWS.InitialRectangle.Left := WideString(AFindSubControlOptions.InitialRectangle.Left);  ADestFindSubControlOptions.InitialRectangle.Left := @ATempFindSubControlOptionsAPIWS.InitialRectangle.Left[1];// ADestFindSubControlOptions.InitialRectangle.Left := @WideString(AFindSubControlOptions.InitialRectangle.Left)[1];
  ATempFindSubControlOptionsAPIWS.InitialRectangle.Top := WideString(AFindSubControlOptions.InitialRectangle.Top);  ADestFindSubControlOptions.InitialRectangle.Top := @ATempFindSubControlOptionsAPIWS.InitialRectangle.Top[1];// ADestFindSubControlOptions.InitialRectangle.Top := @WideString(AFindSubControlOptions.InitialRectangle.Top)[1];
  ATempFindSubControlOptionsAPIWS.InitialRectangle.Right := WideString(AFindSubControlOptions.InitialRectangle.Right);  ADestFindSubControlOptions.InitialRectangle.Right := @ATempFindSubControlOptionsAPIWS.InitialRectangle.Right[1];// ADestFindSubControlOptions.InitialRectangle.Right := @WideString(AFindSubControlOptions.InitialRectangle.Right)[1];
  ATempFindSubControlOptionsAPIWS.InitialRectangle.Bottom := WideString(AFindSubControlOptions.InitialRectangle.Bottom);  ADestFindSubControlOptions.InitialRectangle.Bottom := @ATempFindSubControlOptionsAPIWS.InitialRectangle.Bottom[1];// ADestFindSubControlOptions.InitialRectangle.Bottom := @WideString(AFindSubControlOptions.InitialRectangle.Bottom)[1];
  ATempFindSubControlOptionsAPIWS.InitialRectangle.LeftOffset := WideString(AFindSubControlOptions.InitialRectangle.LeftOffset);  ADestFindSubControlOptions.InitialRectangle.LeftOffset := @ATempFindSubControlOptionsAPIWS.InitialRectangle.LeftOffset[1];// ADestFindSubControlOptions.InitialRectangle.LeftOffset := @WideString(AFindSubControlOptions.InitialRectangle.LeftOffset)[1];
  ATempFindSubControlOptionsAPIWS.InitialRectangle.TopOffset := WideString(AFindSubControlOptions.InitialRectangle.TopOffset);  ADestFindSubControlOptions.InitialRectangle.TopOffset := @ATempFindSubControlOptionsAPIWS.InitialRectangle.TopOffset[1];// ADestFindSubControlOptions.InitialRectangle.TopOffset := @WideString(AFindSubControlOptions.InitialRectangle.TopOffset)[1];
  ATempFindSubControlOptionsAPIWS.InitialRectangle.RightOffset := WideString(AFindSubControlOptions.InitialRectangle.RightOffset);  ADestFindSubControlOptions.InitialRectangle.RightOffset := @ATempFindSubControlOptionsAPIWS.InitialRectangle.RightOffset[1];// ADestFindSubControlOptions.InitialRectangle.RightOffset := @WideString(AFindSubControlOptions.InitialRectangle.RightOffset)[1];
  ATempFindSubControlOptionsAPIWS.InitialRectangle.BottomOffset := WideString(AFindSubControlOptions.InitialRectangle.BottomOffset);  ADestFindSubControlOptions.InitialRectangle.BottomOffset := @ATempFindSubControlOptionsAPIWS.InitialRectangle.BottomOffset[1];// ADestFindSubControlOptions.InitialRectangle.BottomOffset := @WideString(AFindSubControlOptions.InitialRectangle.BottomOffset)[1];

  ADestFindSubControlOptions.UseWholeScreen := AFindSubControlOptions.UseWholeScreen;
  ATempFindSubControlOptionsAPIWS.ColorError := WideString(AFindSubControlOptions.ColorError);  ADestFindSubControlOptions.ColorError := @ATempFindSubControlOptionsAPIWS.ColorError[1];// ADestFindSubControlOptions.ColorError := @WideString(AFindSubControlOptions.ColorError)[1];
  ATempFindSubControlOptionsAPIWS.AllowedColorErrorCount := WideString(AFindSubControlOptions.AllowedColorErrorCount);  ADestFindSubControlOptions.AllowedColorErrorCount := @ATempFindSubControlOptionsAPIWS.AllowedColorErrorCount[1];// ADestFindSubControlOptions.AllowedColorErrorCount := @WideString(AFindSubControlOptions.AllowedColorErrorCount)[1];
  ADestFindSubControlOptions.WaitForControlToGoAway := AFindSubControlOptions.WaitForControlToGoAway;
  ADestFindSubControlOptions.StartSearchingWithCachedControl := AFindSubControlOptions.StartSearchingWithCachedControl;
  ATempFindSubControlOptionsAPIWS.CachedControlLeft := WideString(AFindSubControlOptions.CachedControlLeft);  ADestFindSubControlOptions.CachedControlLeft := @ATempFindSubControlOptionsAPIWS.CachedControlLeft[1];// ADestFindSubControlOptions.CachedControlLeft := @WideString(AFindSubControlOptions.CachedControlLeft)[1];
  ATempFindSubControlOptionsAPIWS.CachedControlTop := WideString(AFindSubControlOptions.CachedControlTop);  ADestFindSubControlOptions.CachedControlTop := @ATempFindSubControlOptionsAPIWS.CachedControlTop[1];// ADestFindSubControlOptions.CachedControlTop := @WideString(AFindSubControlOptions.CachedControlTop)[1];

  ATempFindSubControlOptionsAPIWS.MatchPrimitiveFiles := WideString(AFindSubControlOptions.MatchPrimitiveFiles);  ADestFindSubControlOptions.MatchPrimitiveFiles := @ATempFindSubControlOptionsAPIWS.MatchPrimitiveFiles[1];// ADestFindSubControlOptions.MatchPrimitiveFiles := @WideString(AFindSubControlOptions.MatchPrimitiveFiles)[1];
  ADestFindSubControlOptions.GetAllControls := AFindSubControlOptions.GetAllControls;
  ADestFindSubControlOptions.UseFastSearch := AFindSubControlOptions.UseFastSearch;
  ATempFindSubControlOptionsAPIWS.FastSearchAllowedColorErrorCount := WideString(AFindSubControlOptions.FastSearchAllowedColorErrorCount);  ADestFindSubControlOptions.FastSearchAllowedColorErrorCount := @ATempFindSubControlOptionsAPIWS.FastSearchAllowedColorErrorCount[1];// ADestFindSubControlOptions.FastSearchAllowedColorErrorCount := @WideString(AFindSubControlOptions.FastSearchAllowedColorErrorCount)[1];
  ATempFindSubControlOptionsAPIWS.IgnoredColors := WideString(AFindSubControlOptions.IgnoredColors);  ADestFindSubControlOptions.IgnoredColors := @ATempFindSubControlOptionsAPIWS.IgnoredColors[1];// ADestFindSubControlOptions.IgnoredColors := @WideString(AFindSubControlOptions.IgnoredColors)[1];
  ADestFindSubControlOptions.SleepySearch := AFindSubControlOptions.SleepySearch;
  ADestFindSubControlOptions.StopSearchOnMismatch := AFindSubControlOptions.StopSearchOnMismatch;

  ADestFindSubControlOptions.ImageSource := Byte(AFindSubControlOptions.ImageSource);
  ATempFindSubControlOptionsAPIWS.SourceFileName := WideString(AFindSubControlOptions.SourceFileName);  ADestFindSubControlOptions.SourceFileName := @ATempFindSubControlOptionsAPIWS.SourceFileName[1];// ADestFindSubControlOptions.SourceFileName := @WideString(AFindSubControlOptions.SourceFileName)[1];
  ADestFindSubControlOptions.ImageSourceFileNameLocation := Byte(AFindSubControlOptions.ImageSourceFileNameLocation);

  ADestFindSubControlOptions.PrecisionTimeout := AFindSubControlOptions.PrecisionTimeout;
  ADestFindSubControlOptions.FullBackgroundImageInResult := AFindSubControlOptions.FullBackgroundImageInResult;

  ATempFindSubControlOptionsAPIWS.MatchByHistogramSettings.MinPercentColorMatch := WideString(AFindSubControlOptions.MatchByHistogramSettings.MinPercentColorMatch);  ADestFindSubControlOptions.MatchByHistogramSettings.MinPercentColorMatch := @ATempFindSubControlOptionsAPIWS.MatchByHistogramSettings.MinPercentColorMatch[1];// ADestFindSubControlOptions.MatchByHistogramSettings.MinPercentColorMatch := @WideString(AFindSubControlOptions.MatchByHistogramSettings.MinPercentColorMatch)[1];
  ATempFindSubControlOptionsAPIWS.MatchByHistogramSettings.MostSignificantColorCountInSubBmp := WideString(AFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInSubBmp);  ADestFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInSubBmp := @ATempFindSubControlOptionsAPIWS.MatchByHistogramSettings.MostSignificantColorCountInSubBmp[1];// ADestFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInSubBmp := @WideString(AFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInSubBmp)[1];
  ATempFindSubControlOptionsAPIWS.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp := WideString(AFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp);  ADestFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp := @ATempFindSubControlOptionsAPIWS.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp[1];// ADestFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp := @WideString(AFindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp)[1];

  ATempFindSubControlOptionsAPIWS.EvaluateTextCount := WideString(AFindSubControlOptions.EvaluateTextCount);  ADestFindSubControlOptions.EvaluateTextCount := @ATempFindSubControlOptionsAPIWS.EvaluateTextCount[1];// ATempFindSubControlOptionsAPIWS.EvaluateTextCount := WideString(AFindSubControlOptions.EvaluateTextCount);  ADestFindSubControlOptions.EvaluateTextCount := @ATempFindSubControlOptionsAPIWS.EvaluateTextCount[1];// ADestFindSubControlOptions.EvaluateTextCount := @WideString(AFindSubControlOptions.EvaluateTextCount)[1];
  ADestFindSubControlOptions.CropFromScreenshot := AFindSubControlOptions.CropFromScreenshot;
  ATempFindSubControlOptionsAPIWS.ThreadCount := WideString(AFindSubControlOptions.ThreadCount);  ADestFindSubControlOptions.ThreadCount := @ATempFindSubControlOptionsAPIWS.ThreadCount[1];// ADestFindSubControlOptions.ThreadCount := @WideString(AFindSubControlOptions.ThreadCount)[1];

  if Length(AFindSubControlOptions.MatchBitmapText) = 0 then
  begin
    ADestFindSubControlOptions.MatchBitmapText := nil;
    Exit;
  end;

  ADestFindSubControlOptions.MatchBitmapText := @ADestMatchBitmapTextRecAPI; //PMatchBitmapTextRecAPI

  SetLength(ADestMatchBitmapTextArray, Length(AFindSubControlOptions.MatchBitmapText));
  SetLength(TempDestWideStr, Length(AFindSubControlOptions.MatchBitmapText));

  for i := 0 to Length(AFindSubControlOptions.MatchBitmapText) - 1 do
    SetMatchBitmapTextToAPI(AFindSubControlOptions.MatchBitmapText[i], ADestMatchBitmapTextArray[i], TempDestWideStr[i]);

  ADestMatchBitmapTextRecAPI.ArrLen := Length(AFindSubControlOptions.MatchBitmapText);
  ADestMatchBitmapTextRecAPI.Items := @ADestMatchBitmapTextArray[0];
end;


procedure SetSetControlTextOptionsToAPI(var ASetControlTextOptions: TClkSetTextOptions; var ADestSetControlTextOptions: TClkSetTextOptionsAPI; var ATempSetControlTextOptionsAPIWS: TClkSetTextOptionsAPIWS);
begin
  ATempSetControlTextOptionsAPIWS.Text := WideString(ASetControlTextOptions.Text);  ADestSetControlTextOptions.Text := @ATempSetControlTextOptionsAPIWS.Text[1];// ADestSetControlTextOptions.Text := @WideString(ASetControlTextOptions.Text)[1];
  ADestSetControlTextOptions.ControlType := Byte(ASetControlTextOptions.ControlType);
  ATempSetControlTextOptionsAPIWS.DelayBetweenKeyStrokes := WideString(ASetControlTextOptions.DelayBetweenKeyStrokes);  ADestSetControlTextOptions.DelayBetweenKeyStrokes := @ATempSetControlTextOptionsAPIWS.DelayBetweenKeyStrokes[1];// ADestSetControlTextOptions.DelayBetweenKeyStrokes := @WideString(ASetControlTextOptions.DelayBetweenKeyStrokes)[1];
  ATempSetControlTextOptionsAPIWS.Count := WideString(ASetControlTextOptions.Count);  ADestSetControlTextOptions.Count := @ATempSetControlTextOptionsAPIWS.Count[1];// ADestSetControlTextOptions.Count := @WideString(ASetControlTextOptions.Count)[1];
end;


procedure SetCallTemplateOptionsToAPI(var ACallTemplateOptions: TClkCallTemplateOptions; var ADestCallTemplateOptions: TClkCallTemplateOptionsAPI; var ATempCallTemplateOptionsAPIWS: TClkCallTemplateOptionsAPIWS);
begin
  ATempCallTemplateOptionsAPIWS.TemplateFileName := WideString(ACallTemplateOptions.TemplateFileName);  ADestCallTemplateOptions.TemplateFileName := @ATempCallTemplateOptionsAPIWS.TemplateFileName[1];// ADestCallTemplateOptions.TemplateFileName := @WideString(ACallTemplateOptions.TemplateFileName)[1];
  ATempCallTemplateOptionsAPIWS.ListOfCustomVarsAndValues := WideString(ACallTemplateOptions.ListOfCustomVarsAndValues);  ADestCallTemplateOptions.ListOfCustomVarsAndValues := @ATempCallTemplateOptionsAPIWS.ListOfCustomVarsAndValues[1];// ADestCallTemplateOptions.ListOfCustomVarsAndValues := @WideString(ACallTemplateOptions.ListOfCustomVarsAndValues)[1];
  ADestCallTemplateOptions.EvaluateBeforeCalling := ACallTemplateOptions.EvaluateBeforeCalling;

  ADestCallTemplateOptions.CallTemplateLoop.Enabled := ACallTemplateOptions.CallTemplateLoop.Enabled;
  ATempCallTemplateOptionsAPIWS.CallTemplateLoop.Counter := WideString(ACallTemplateOptions.CallTemplateLoop.Counter);  ADestCallTemplateOptions.CallTemplateLoop.Counter := @ATempCallTemplateOptionsAPIWS.CallTemplateLoop.Counter[1];// ADestCallTemplateOptions.CallTemplateLoop.Counter := @WideString(ACallTemplateOptions.CallTemplateLoop.Counter)[1];
  ATempCallTemplateOptionsAPIWS.CallTemplateLoop.InitValue := WideString(ACallTemplateOptions.CallTemplateLoop.InitValue);  ADestCallTemplateOptions.CallTemplateLoop.InitValue := @ATempCallTemplateOptionsAPIWS.CallTemplateLoop.InitValue[1];// ADestCallTemplateOptions.CallTemplateLoop.InitValue := @WideString(ACallTemplateOptions.CallTemplateLoop.InitValue)[1];
  ATempCallTemplateOptionsAPIWS.CallTemplateLoop.EndValue := WideString(ACallTemplateOptions.CallTemplateLoop.EndValue);  ADestCallTemplateOptions.CallTemplateLoop.EndValue := @ATempCallTemplateOptionsAPIWS.CallTemplateLoop.EndValue[1];// ADestCallTemplateOptions.CallTemplateLoop.EndValue := @WideString(ACallTemplateOptions.CallTemplateLoop.EndValue)[1];
  ADestCallTemplateOptions.CallTemplateLoop.Direction := Byte(ACallTemplateOptions.CallTemplateLoop.Direction);
  ATempCallTemplateOptionsAPIWS.CallTemplateLoop.BreakCondition := WideString(ACallTemplateOptions.CallTemplateLoop.BreakCondition);  ADestCallTemplateOptions.CallTemplateLoop.BreakCondition := @ATempCallTemplateOptionsAPIWS.CallTemplateLoop.BreakCondition[1];// ADestCallTemplateOptions.CallTemplateLoop.BreakCondition := @WideString(ACallTemplateOptions.CallTemplateLoop.BreakCondition)[1];
  ADestCallTemplateOptions.CallTemplateLoop.EvalBreakPosition := Byte(ACallTemplateOptions.CallTemplateLoop.EvalBreakPosition);
end;


procedure SetSleepOptionsToAPI(var ASleepOptions: TClkSleepOptions; var ADestSleepOptions: TClkSleepOptionsAPI; var ATempSleepOptionsAPIWS: TClkSleepOptionsAPIWS);
begin
  ATempSleepOptionsAPIWS.Value := WideString(ASleepOptions.Value);  ADestSleepOptions.Value := @ATempSleepOptionsAPIWS.Value[1];// ADestSleepOptions.Value := @WideString(ASleepOptions.Value)[1];
end;


procedure SetSetVarOptionsToAPI(var ASetVarOptions: TClkSetVarOptions; var ADestSetVarOptions: TClkSetVarOptionsAPI; var ATempSetVarOptionsAPIWS: TClkSetVarOptionsAPIWS);
begin
  ATempSetVarOptionsAPIWS.ListOfVarNames := WideString(ASetVarOptions.ListOfVarNames);  ADestSetVarOptions.ListOfVarNames := @ATempSetVarOptionsAPIWS.ListOfVarNames[1];// ADestSetVarOptions.ListOfVarNames := @WideString(ASetVarOptions.ListOfVarNames)[1];
  ATempSetVarOptionsAPIWS.ListOfVarValues := WideString(ASetVarOptions.ListOfVarValues);  ADestSetVarOptions.ListOfVarValues := @ATempSetVarOptionsAPIWS.ListOfVarValues[1];// ADestSetVarOptions.ListOfVarValues := @WideString(ASetVarOptions.ListOfVarValues)[1];
  ATempSetVarOptionsAPIWS.ListOfVarEvalBefore := WideString(ASetVarOptions.ListOfVarEvalBefore);  ADestSetVarOptions.ListOfVarEvalBefore := @ATempSetVarOptionsAPIWS.ListOfVarEvalBefore[1];// ADestSetVarOptions.ListOfVarEvalBefore := @WideString(ASetVarOptions.ListOfVarEvalBefore)[1];
  ADestSetVarOptions.FailOnException := ASetVarOptions.FailOnException;
end;


procedure SetWindowOperationsOptionsToAPI(var AWindowOperationsOptions: TClkWindowOperationsOptions; var ADestWindowOperationsOptions: TClkWindowOperationsOptionsAPI; var ATempWindowOperationsOptionsAPIWS: TClkWindowOperationsOptionsAPIWS);
begin
  ADestWindowOperationsOptions.Operation := Byte(AWindowOperationsOptions.Operation);
  ATempWindowOperationsOptionsAPIWS.NewX := WideString(AWindowOperationsOptions.NewX);  ADestWindowOperationsOptions.NewX := @ATempWindowOperationsOptionsAPIWS.NewX[1];// ADestWindowOperationsOptions.NewX := @WideString(AWindowOperationsOptions.NewX)[1];
  ATempWindowOperationsOptionsAPIWS.NewY := WideString(AWindowOperationsOptions.NewY);  ADestWindowOperationsOptions.NewY := @ATempWindowOperationsOptionsAPIWS.NewY[1];// ADestWindowOperationsOptions.NewY := @WideString(AWindowOperationsOptions.NewY)[1];
  ATempWindowOperationsOptionsAPIWS.NewWidth := WideString(AWindowOperationsOptions.NewWidth);  ADestWindowOperationsOptions.NewWidth := @ATempWindowOperationsOptionsAPIWS.NewWidth[1];// ADestWindowOperationsOptions.NewWidth := @WideString(AWindowOperationsOptions.NewWidth)[1];
  ATempWindowOperationsOptionsAPIWS.NewHeight := WideString(AWindowOperationsOptions.NewHeight);  ADestWindowOperationsOptions.NewHeight := @ATempWindowOperationsOptionsAPIWS.NewHeight[1];// ADestWindowOperationsOptions.NewHeight := @WideString(AWindowOperationsOptions.NewHeight)[1];
  ADestWindowOperationsOptions.NewPositionEnabled := AWindowOperationsOptions.NewPositionEnabled;
  ADestWindowOperationsOptions.NewSizeEnabled := AWindowOperationsOptions.NewSizeEnabled;
end;


procedure SetLoadSetVarFromFileOptionsToAPI(var ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions; var ADestLoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptionsAPI; var ATempLoadSetVarFromFileOptionsAPIWS: TClkLoadSetVarFromFileOptionsAPIWS);
begin
  ATempLoadSetVarFromFileOptionsAPIWS.FileName := WideString(ALoadSetVarFromFileOptions.FileName);  ADestLoadSetVarFromFileOptions.FileName := @ATempLoadSetVarFromFileOptionsAPIWS.FileName[1];// ADestLoadSetVarFromFileOptions.FileName := @WideString(ALoadSetVarFromFileOptions.FileName)[1];
  ATempLoadSetVarFromFileOptionsAPIWS.SetVarActionName := WideString(ALoadSetVarFromFileOptions.SetVarActionName);  ADestLoadSetVarFromFileOptions.SetVarActionName := @ATempLoadSetVarFromFileOptionsAPIWS.SetVarActionName[1];// ADestLoadSetVarFromFileOptions.SetVarActionName := @WideString(ALoadSetVarFromFileOptions.SetVarActionName)[1];
end;


procedure SetSaveSetVarToFileOptionsToAPI(var ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions; var ADestSaveSetVarToFileOptions: TClkSaveSetVarToFileOptionsAPI; var ATempSaveSetVarToFileOptionsAPIWS: TClkSaveSetVarToFileOptionsAPIWS);
begin
  ATempSaveSetVarToFileOptionsAPIWS.FileName := WideString(ASaveSetVarToFileOptions.FileName);  ADestSaveSetVarToFileOptions.FileName := @ATempSaveSetVarToFileOptionsAPIWS.FileName[1];// ADestSaveSetVarToFileOptions.FileName := @WideString(ASaveSetVarToFileOptions.FileName)[1];
  ATempSaveSetVarToFileOptionsAPIWS.SetVarActionName := WideString(ASaveSetVarToFileOptions.SetVarActionName);  ADestSaveSetVarToFileOptions.SetVarActionName := @ATempSaveSetVarToFileOptionsAPIWS.SetVarActionName[1];// ADestSaveSetVarToFileOptions.SetVarActionName := @WideString(ASaveSetVarToFileOptions.SetVarActionName)[1];
end;


procedure SetPluginOptionsToAPI(var APluginOptions: TClkPluginOptions; var ADestPluginOptions: TClkPluginOptionsAPI; var ATempPluginOptionsAPIWS: TClkPluginOptionsAPIWS);
begin
  ATempPluginOptionsAPIWS.FileName := WideString(APluginOptions.FileName);  ADestPluginOptions.FileName := @ATempPluginOptionsAPIWS.FileName[1];// ADestPluginOptions.FileName := @WideString(APluginOptions.FileName)[1];
  ATempPluginOptionsAPIWS.ListOfPropertiesAndValues := WideString(APluginOptions.ListOfPropertiesAndValues);  ADestPluginOptions.ListOfPropertiesAndValues := @ATempPluginOptionsAPIWS.ListOfPropertiesAndValues[1];// ADestPluginOptions.ListOfPropertiesAndValues := @WideString(APluginOptions.ListOfPropertiesAndValues)[1];
end;


procedure SetEditTemplateOptionsToAPI(var AEditTemplateOptions: TClkEditTemplateOptions; var ADestEditTemplateOptions: TClkEditTemplateOptionsAPI; var ATempTClkEditTemplateOptionsAPIWS: TClkEditTemplateOptionsAPIWS);
begin
  ADestEditTemplateOptions.Operation := Byte(AEditTemplateOptions.Operation);
  ADestEditTemplateOptions.WhichTemplate := Byte(AEditTemplateOptions.WhichTemplate);

  ATempTClkEditTemplateOptionsAPIWS.TemplateFileName := WideString(AEditTemplateOptions.TemplateFileName);  ADestEditTemplateOptions.TemplateFileName := @ATempTClkEditTemplateOptionsAPIWS.TemplateFileName[1];// ADestEditTemplateOptions.TemplateFileName := @WideString(AEditTemplateOptions.TemplateFileName)[1];
  ATempTClkEditTemplateOptionsAPIWS.ListOfEditedProperties := WideString(AEditTemplateOptions.ListOfEditedProperties);  ADestEditTemplateOptions.ListOfEditedProperties := @ATempTClkEditTemplateOptionsAPIWS.ListOfEditedProperties[1];// ADestEditTemplateOptions.ListOfEditedProperties := @WideString(AEditTemplateOptions.ListOfEditedProperties)[1];
  ATempTClkEditTemplateOptionsAPIWS.ListOfEnabledProperties := WideString(AEditTemplateOptions.ListOfEnabledProperties);  ADestEditTemplateOptions.ListOfEnabledProperties := @ATempTClkEditTemplateOptionsAPIWS.ListOfEnabledProperties[1];// ADestEditTemplateOptions.ListOfEnabledProperties := @WideString(AEditTemplateOptions.ListOfEnabledProperties)[1];
  ATempTClkEditTemplateOptionsAPIWS.EditedActionName := WideString(AEditTemplateOptions.EditedActionName);  ADestEditTemplateOptions.EditedActionName := @ATempTClkEditTemplateOptionsAPIWS.EditedActionName[1];// ADestEditTemplateOptions.EditedActionName := @WideString(AEditTemplateOptions.EditedActionName)[1];
  ADestEditTemplateOptions.EditedActionType := Byte(AEditTemplateOptions.EditedActionType);
  ATempTClkEditTemplateOptionsAPIWS.EditedActionCondition := WideString(AEditTemplateOptions.EditedActionCondition);  ADestEditTemplateOptions.EditedActionCondition := @ATempTClkEditTemplateOptionsAPIWS.EditedActionCondition[1];// ADestEditTemplateOptions.EditedActionCondition := @WideString(AEditTemplateOptions.EditedActionCondition)[1];
  ADestEditTemplateOptions.EditedActionTimeout := AEditTemplateOptions.EditedActionTimeout;
  ATempTClkEditTemplateOptionsAPIWS.NewActionName := WideString(AEditTemplateOptions.NewActionName);  ADestEditTemplateOptions.NewActionName := @ATempTClkEditTemplateOptionsAPIWS.NewActionName[1];// ADestEditTemplateOptions.NewActionName := @WideString(AEditTemplateOptions.NewActionName)[1];
  ADestEditTemplateOptions.ShouldSaveTemplate := AEditTemplateOptions.ShouldSaveTemplate;
end;

end.

