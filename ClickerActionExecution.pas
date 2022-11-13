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


unit ClickerActionExecution;

{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, Classes, SysUtils, Forms, StdCtrls, Graphics, ExtCtrls,
  ClickerUtils, ClickerActionsFrame;


type
  TOnSetEditorEnabledState = procedure(AEnabled: Boolean) of object;
  TOnSetEditorTimeoutProgressBarMax = procedure(AMaxValue: Integer) of object;
  TOnSetEditorTimeoutProgressBarPosition = procedure(APositionValue: Integer) of object;
  TOnWaitForBitmapsAvailability = procedure(ListOfBitmapFiles: TStringList) of object;
  TOnCallTemplate = function(Sender: TObject; AFileNameToCall: string; ListOfVariables: TStrings; DebugBitmap: TBitmap; DebugGridImage: TImage; IsDebugging, AShouldStopAtBreakPoint: Boolean; AStackLevel: Integer; AExecutesRemotely: Boolean): Boolean of object;
  TOnSetEditorSleepInfo = procedure(AElapsedTime, ARemainingTime: string) of object;
  TOnGetSelfHandles = procedure(AListOfSelfHandles: TStringList) of object;

  TActionExecution = class
  private
    FClickerVars: TStringList;  //not created here in this class, used from outside
    FStopAllActionsOnDemandFromParent: PBoolean;
    FStopAllActionsOnDemand: PBoolean;
    FLog: TMemo;
    FTemplateFileName: PString;
    FExecutingActionFromRemote: PBoolean;
    FFileLocationOfDepsIsMem: PBoolean;
    FFullTemplatesDir: PString;
    FStackLevel: PInteger;
    FExecutesRemotely: PBoolean;
    FOwnerFrame: TObject;

    FfrClickerActions: TfrClickerActions;  ///////////////////////// temp

    FOnSetEditorEnabledState: TOnSetEditorEnabledState;
    FOnSetEditorTimeoutProgressBarMax: TOnSetEditorTimeoutProgressBarMax;
    FOnSetEditorTimeoutProgressBarPosition: TOnSetEditorTimeoutProgressBarPosition;
    FOnLoadBitmap: TOnLoadBitmap;
    FOnWaitForBitmapsAvailability: TOnWaitForBitmapsAvailability;
    FOnCallTemplate: TOnCallTemplate;
    FOnSetEditorSleepProgressBarMax: TOnSetEditorTimeoutProgressBarMax;
    FOnSetEditorSleepProgressBarPosition: TOnSetEditorTimeoutProgressBarPosition;
    FOnSetEditorSleepInfo: TOnSetEditorSleepInfo;
    FOnGetSelfHandles: TOnGetSelfHandles;

    function GetActionVarValue(VarName: string): string;
    procedure SetActionVarValue(VarName, VarValue: string);
    function EvaluateReplacements(s: string; Recursive: Boolean = True): string;
    procedure AppendErrorMessageToActionVar(NewErrMsg: string);
    procedure PrependErrorMessageToActionVar(NewErrMsg: string);
    function EvaluateHTTP(AValue: string): string;
    procedure PreviewTextOnBmp(var AFindControlOptions: TClkFindControlOptions; AEvaluatedText: string; AProfileIndex: Integer; ASearchedBmp: TBitmap);

    procedure SetLastActionStatus(AActionResult, AAlowedToFail: Boolean);
    procedure CheckManualStopCondition;

    procedure ExecuteClickAction(var AClickOptions: TClkClickOptions);
    function ExecuteFindControlAction(var AFindControlOptions: TClkFindControlOptions; var AActionOptions: TClkActionOptions; IsSubControl: Boolean): Boolean; //returns True if found

    procedure DoOnSetEditorEnabledState(AEnabled: Boolean);
    procedure DoOnSetEditorTimeoutProgressBarMax(AMaxValue: Integer);
    procedure DoOnSetEditorTimeoutProgressBarPosition(APositionValue: Integer);
    function DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    procedure DoOnWaitForBitmapsAvailability(AListOfFiles: TStringList);
    procedure DoOnSetEditorSleepProgressBarMax(AMaxValue: Integer);
    procedure DoOnSetEditorSleepProgressBarPosition(APositionValue: Integer);
    procedure DoOnSetEditorSleepInfo(AElapsedTime, ARemainingTime: string);
    procedure DoOnGetSelfHandles(AListOfSelfHandles: TStringList);
  public
    constructor Create;
    destructor Destroy; override;

    function ExecuteMultiClickAction(var AClickOptions: TClkClickOptions): Boolean;
    function ExecuteExecAppAction(var AExecAppOptions: TClkExecAppOptions; var AActionOptions: TClkActionOptions): Boolean;
    function ExecuteFindControlActionWithTimeout(var AFindControlOptions: TClkFindControlOptions; var AActionOptions: TClkActionOptions; IsSubControl: Boolean): Boolean; //returns True if found
    function ExecuteSetControlTextAction(var ASetTextOptions: TClkSetTextOptions): Boolean;
    function ExecuteCallTemplateAction(var ACallTemplateOptions: TClkCallTemplateOptions; IsDebugging, AShouldStopAtBreakPoint: Boolean): Boolean; //to be moved to private, after ExecuteFindControlAction header
    function ExecuteLoopedCallTemplateAction(var ACallTemplateOptions: TClkCallTemplateOptions; IsDebugging, AShouldStopAtBreakPoint: Boolean): Boolean;
    function ExecuteSleepAction(var ASleepOptions: TClkSleepOptions; var AActionOptions: TClkActionOptions): Boolean;
    function ExecuteSetVarAction(var ASetVarOptions: TClkSetVarOptions): Boolean;
    function ExecuteWindowOperationsAction(var AWindowOperationsOptions: TClkWindowOperationsOptions): Boolean;

    function ExecuteClickActionAsString(AListOfClickOptionsParams: TStrings): Boolean;
    function ExecuteExecAppActionAsString(AListOfExecAppOptionsParams: TStrings): Boolean;
    function ExecuteFindControlActionAsString(AListOfFindControlOptionsParams: TStrings; AIsSubControl: Boolean): Boolean;
    function ExecuteSetControlTextActionAsString(AListOfSetControlTextOptionsParams: TStrings): Boolean;
    function ExecuteCallTemplateActionAsString(AListOfCallTemplateOptionsParams: TStrings): Boolean;
    function ExecuteSleepActionAsString(AListOfSleepOptionsParams: TStrings): Boolean;
    function ExecuteSetVarActionAsString(AListOfSetVarOptionsParams: TStrings): Boolean;
    function ExecuteWindowOperationsActionAsString(AListOfWindowOperationsOptionsParams: TStrings): Boolean;

    //using pointers for the following properties, because the values they are pointing to, can be updated later, not when this class is created
    property ClickerVars: TStringList write FClickerVars;  //not created here in this class, used from outside
    property StopAllActionsOnDemandFromParent: PBoolean write FStopAllActionsOnDemandFromParent;
    property StopAllActionsOnDemand: PBoolean write FStopAllActionsOnDemand;
    property Log: TMemo write FLog;   //not created here in this class, used from outside
    property TemplateFileName: PString write FTemplateFileName;
    property ExecutingActionFromRemote: PBoolean write FExecutingActionFromRemote;
    property FileLocationOfDepsIsMem: PBoolean write FFileLocationOfDepsIsMem;
    property FullTemplatesDir: PString write FFullTemplatesDir;
    property StackLevel: PInteger write FStackLevel;
    property ExecutesRemotely: PBoolean write FExecutesRemotely;
    property OwnerFrame: TObject write FOwnerFrame;

    property frClickerActions: TfrClickerActions read FfrClickerActions write FfrClickerActions;  //not created here in this class, used from outside    ///////////////////////// temp

    property OnSetEditorEnabledState: TOnSetEditorEnabledState write FOnSetEditorEnabledState;
    property OnSetEditorTimeoutProgressBarMax: TOnSetEditorTimeoutProgressBarMax write FOnSetEditorTimeoutProgressBarMax;
    property OnSetEditorTimeoutProgressBarPosition: TOnSetEditorTimeoutProgressBarPosition write FOnSetEditorTimeoutProgressBarPosition;
    property OnLoadBitmap: TOnLoadBitmap write FOnLoadBitmap;
    property OnWaitForBitmapsAvailability: TOnWaitForBitmapsAvailability write FOnWaitForBitmapsAvailability;
    property OnCallTemplate: TOnCallTemplate write FOnCallTemplate;
    property OnSetEditorSleepProgressBarMax: TOnSetEditorTimeoutProgressBarMax write FOnSetEditorSleepProgressBarMax;
    property OnSetEditorSleepProgressBarPosition: TOnSetEditorTimeoutProgressBarPosition write FOnSetEditorSleepProgressBarPosition;
    property OnSetEditorSleepInfo: TOnSetEditorSleepInfo write FOnSetEditorSleepInfo;
    property OnGetSelfHandles: TOnGetSelfHandles write FOnGetSelfHandles;
  end;


implementation


uses
  MouseStuff, Controls,
  {$IFnDEF FPC}
    ShellAPI,
  {$ELSE}
    Process,
  {$ENDIF}
  ControlInteraction, IdHTTP;


constructor TActionExecution.Create;
begin
  //inherited Create;
  FClickerVars := nil; //not created here in this class, used from outside
  FLog := nil;
  FfrClickerActions := nil;
  FExecutingActionFromRemote := nil;
  FFileLocationOfDepsIsMem := nil;
  FFullTemplatesDir := nil;
  FStackLevel := nil;
  FExecutesRemotely := nil;
  FOwnerFrame := nil;

  FOnSetEditorEnabledState := nil;
  FOnSetEditorTimeoutProgressBarMax := nil;
  FOnSetEditorTimeoutProgressBarPosition := nil;
  FOnLoadBitmap := nil;
  FOnWaitForBitmapsAvailability := nil;
  FOnCallTemplate := nil;
  FOnSetEditorSleepProgressBarMax := nil;
  FOnSetEditorSleepProgressBarPosition := nil;
  FOnSetEditorSleepInfo := nil;
  FOnGetSelfHandles := nil;
end;


destructor TActionExecution.Destroy;
begin
  FClickerVars := nil;
  FLog := nil;
  inherited Destroy;
end;


function TActionExecution.GetActionVarValue(VarName: string): string;
begin
  if FClickerVars = nil then
    raise Exception.Create('ClickerVars is not assigned.');

  Result := FClickerVars.Values[VarName];
end;


procedure TActionExecution.SetActionVarValue(VarName, VarValue: string);
begin
  FClickerVars.Values[VarName] := FastReplace_ReturnTo68(VarValue);  //Do not use EvaluateReplacements(VarValue) here, because there are calls which expect the value to be directly assigned !

  if FLog = nil then
    raise Exception.Create('FLog not assigned.');

  if VarName = '$ExecAction_Err$' then
    FLog.Lines.Add(DateTimeToStr(Now) + '  ' + VarValue);
end;


function TActionExecution.EvaluateReplacements(s: string; Recursive: Boolean = True): string;
begin
  Result := EvaluateAllReplacements(FClickerVars, s, Recursive);
end;


procedure TActionExecution.AppendErrorMessageToActionVar(NewErrMsg: string);
begin
  SetActionVarValue('$ExecAction_Err$', GetActionVarValue('$ExecAction_Err$') + FastReplace_ReturnTo68(NewErrMsg));
end;


procedure TActionExecution.PrependErrorMessageToActionVar(NewErrMsg: string);
begin
  SetActionVarValue('$ExecAction_Err$', FastReplace_ReturnTo68(NewErrMsg) + GetActionVarValue('$ExecAction_Err$'));
end;


function TActionExecution.EvaluateHTTP(AValue: string): string;
var
  TempIdHTTP: TIdHTTP;
begin
  Result := AValue;

  if (Pos('$HTTP://', UpperCase(AValue)) > 0) or (Pos('$HTTPS://', UpperCase(AValue)) > 0) then
    if AValue[Length(AValue)] = '$' then
    begin
      AValue := Copy(AValue, 2, Length(AValue) - 2);
      AValue := EvaluateReplacements(AValue);

      try
        TempIdHTTP := TIdHTTP.Create(nil);
        try
          TempIdHTTP.ConnectTimeout := 1000;    //These values should be increased if using a remote server. However, they will slow down the execution in that case.
          TempIdHTTP.ReadTimeout := 1000;
          Result := TempIdHTTP.Get(AValue);
        finally
          TempIdHTTP.Free;
        end;
      except
        on E: Exception do
        begin
          Result := E.Message;
          AppendErrorMessageToActionVar(Result);
        end;
      end;
    end;
end;


procedure TActionExecution.PreviewTextOnBmp(var AFindControlOptions: TClkFindControlOptions; AEvaluatedText: string; AProfileIndex: Integer; ASearchedBmp: TBitmap);
const
  CFontQualitiesStr: array[TFontQuality] of string = ('Default', 'Draft', 'Proof', 'NonAntialiased', 'Antialiased', 'Cleartype', 'CleartypeNatural');
var
  TextDimensions: TSize;
  TextToDisplay: string;
  FontQualityReplacement: Integer;
  FontQualityReplacementStr: string;
  EvalFG, EvalBG: string;
  EvalFGCol, EvalBGCol: TColor;
  i: TFontQuality;
  CropLeft, CropTop, CropRight, CropBottom: Integer;
  APreviewBmp: TBitmap;
  TextWidthAfterCropping, TextHeightAfterCropping: Integer;
begin
  TextToDisplay := AEvaluatedText; //ask once    - it expects an already evaluated text, which is also used somewhere else, so evaluated as less as possible

  APreviewBmp := TBitmap.Create;
  try
    APreviewBmp.PixelFormat := pf24bit;

    EvalFG := EvaluateReplacements(AFindControlOptions.MatchBitmapText[AProfileIndex].ForegroundColor, True);
    EvalBG := EvaluateReplacements(AFindControlOptions.MatchBitmapText[AProfileIndex].BackgroundColor, True);

    EvalFGCol := HexToInt(EvalFG);
    EvalBGCol := HexToInt(EvalBG);

    APreviewBmp.Canvas.Font.Color := EvalFGCol;
    APreviewBmp.Canvas.Font.Name := EvaluateReplacements(AFindControlOptions.MatchBitmapText[AProfileIndex].FontName);

    APreviewBmp.Canvas.Font.Size := AFindControlOptions.MatchBitmapText[AProfileIndex].FontSize;

    APreviewBmp.Canvas.Font.Style := [];

    if AFindControlOptions.MatchBitmapText[AProfileIndex].Bold then
      APreviewBmp.Canvas.Font.Style := APreviewBmp.Canvas.Font.Style + [fsBold];

    if AFindControlOptions.MatchBitmapText[AProfileIndex].Italic then
      APreviewBmp.Canvas.Font.Style := APreviewBmp.Canvas.Font.Style + [fsItalic];

    if AFindControlOptions.MatchBitmapText[AProfileIndex].Underline then
      APreviewBmp.Canvas.Font.Style := APreviewBmp.Canvas.Font.Style + [fsUnderline];

    if AFindControlOptions.MatchBitmapText[AProfileIndex].StrikeOut then
      APreviewBmp.Canvas.Font.Style := APreviewBmp.Canvas.Font.Style + [fsStrikeOut];

    if AFindControlOptions.MatchBitmapText[AProfileIndex].FontQualityUsesReplacement then
    begin
      FontQualityReplacementStr := EvaluateReplacements(AFindControlOptions.MatchBitmapText[AProfileIndex].FontQualityReplacement);  //should return a string in the following set: 'Default', 'Draft', 'Proof', 'NonAntialiased', 'Antialiased', 'Cleartype', 'CleartypeNatural'
      FontQualityReplacement := -1;
      for i := Low(TFontQuality) to High(TFontQuality) do
        if FontQualityReplacementStr = CFontQualitiesStr[i] then
        begin
          FontQualityReplacement := Ord(i);
          Break;
        end;

      if FontQualityReplacement = -1 then
        FontQualityReplacement := 0;  //default to fqDefault

      APreviewBmp.Canvas.Font.Quality := TFontQuality(FontQualityReplacement);
    end
    else
      APreviewBmp.Canvas.Font.Quality := TFontQuality(AFindControlOptions.MatchBitmapText[AProfileIndex].FontQuality);

    APreviewBmp.Canvas.Brush.Color := EvalBGCol;
    APreviewBmp.Canvas.Pen.Color := EvalBGCol;  //yes, BG

    TextDimensions := APreviewBmp.Canvas.TextExtent(TextToDisplay);
    APreviewBmp.Width := TextDimensions.cx;
    APreviewBmp.Height := TextDimensions.cy;

    APreviewBmp.Canvas.Rectangle(0, 0, APreviewBmp.Width - 1, APreviewBmp.Height - 1);
    APreviewBmp.Canvas.TextOut(0, 0, TextToDisplay);     //Do not use replacements here. The editbox should already be updated with replaced strings.

    CropLeft := Max(StrToIntDef(EvaluateReplacements(AFindControlOptions.MatchBitmapText[AProfileIndex].CropLeft), 0), 0);
    CropTop := Max(StrToIntDef(EvaluateReplacements(AFindControlOptions.MatchBitmapText[AProfileIndex].CropTop), 0), 0);
    CropRight := Max(StrToIntDef(EvaluateReplacements(AFindControlOptions.MatchBitmapText[AProfileIndex].CropRight), 0), 0);
    CropBottom := Max(StrToIntDef(EvaluateReplacements(AFindControlOptions.MatchBitmapText[AProfileIndex].CropBottom), 0), 0);

    ASearchedBmp.PixelFormat := pf24bit;
    ASearchedBmp.Canvas.Pen.Color := clLime;
    ASearchedBmp.Canvas.Brush.Color := clLime;
    if (CropLeft <> 0) or (CropTop <> 0) or (CropRight <> 0) or (CropBottom <> 0) then
    begin
      TextWidthAfterCropping := TextDimensions.cx - (CropLeft + CropRight); //CropLeft is increased as left -> right (towards the text). CropRight is increased right-> left  (towards the text).
      TextHeightAfterCropping := TextDimensions.cy - (CropTop + CropBottom);

      if TextWidthAfterCropping = 0 then
        raise Exception.Create('The text width, after cropping, is 0.');

      if TextHeightAfterCropping = 0 then
        raise Exception.Create('The text height, after cropping, is 0.');

      if TextWidthAfterCropping < 0 then
        raise Exception.Create('The text width, after cropping, is negative.');

      if TextHeightAfterCropping < 0 then
        raise Exception.Create('The text height, after cropping, is negative.');

      ASearchedBmp.Width := TextWidthAfterCropping;
      ASearchedBmp.Height := TextHeightAfterCropping;

      ASearchedBmp.Canvas.Rectangle(0, 0, ASearchedBmp.Width, ASearchedBmp.Height); //this rectangle is required, for proper content copying
      BitBlt(ASearchedBmp.Canvas.Handle, 0, 0, ASearchedBmp.Width, ASearchedBmp.Height, APreviewBmp.Canvas.Handle, CropLeft, CropTop, SRCCOPY);

      //HDC hdcDest, // handle to destination DC
      //int nXDest,  // x-coord of destination upper-left corner
      //int nYDest,  // y-coord of destination upper-left corner
      //int nWidth,  // width of destination rectangle
      //int nHeight, // height of destination rectangle
      //HDC hdcSrc,  // handle to source DC
      //int nXSrc,   // x-coordinate of source upper-left corner
      //int nYSrc,   // y-coordinate of source upper-left corner
      //DWORD dwRop  // raster operation code
    end
    else
    begin
      ASearchedBmp.Width := TextDimensions.cx;
      ASearchedBmp.Height := TextDimensions.cy;
      ASearchedBmp.Canvas.Rectangle(0, 0, ASearchedBmp.Width, ASearchedBmp.Height); //this rectangle is required, for proper content copying
      BitBlt(ASearchedBmp.Canvas.Handle, 0, 0, ASearchedBmp.Width, ASearchedBmp.Height, APreviewBmp.Canvas.Handle, 0, 0, SRCCOPY);
    end;
  finally
    APreviewBmp.Free;
  end;
end;


procedure TActionExecution.SetLastActionStatus(AActionResult, AAlowedToFail: Boolean);
begin
  if AActionResult then
    SetActionVarValue('$LastAction_Status$', CActionStatusStr[asSuccessful])
  else
  begin
    if AAlowedToFail then
      SetActionVarValue('$LastAction_Status$', CActionStatusStr[asAllowedFailed])
    else
      SetActionVarValue('$LastAction_Status$', CActionStatusStr[asFailed])
  end;
end;


procedure TActionExecution.DoOnSetEditorEnabledState(AEnabled: Boolean);
begin
  if Assigned(FOnSetEditorEnabledState) then
    FOnSetEditorEnabledState(AEnabled)
  else
    raise Exception.Create('FOnSetEditorEnabledState is not assigned.');
end;


procedure TActionExecution.DoOnSetEditorTimeoutProgressBarMax(AMaxValue: Integer);
begin
  if Assigned(FOnSetEditorTimeoutProgressBarMax) then
    FOnSetEditorTimeoutProgressBarMax(AMaxValue)
  else
    raise Exception.Create('FOnSetEditorTimeoutProgressBarMax is not assigned.');
end;


procedure TActionExecution.DoOnSetEditorTimeoutProgressBarPosition(APositionValue: Integer);
begin
  if Assigned(FOnSetEditorTimeoutProgressBarPosition) then
    FOnSetEditorTimeoutProgressBarPosition(APositionValue)
  else
    raise Exception.Create('FOnSetEditorTimeoutProgressBarPosition is not assigned.');
end;


function TActionExecution.DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if Assigned(FOnLoadBitmap) then
    Result := FOnLoadBitmap(ABitmap, AFileName)
  else
    raise Exception.Create('OnLoadBitmap is not assigned.');
end;


procedure TActionExecution.DoOnWaitForBitmapsAvailability(AListOfFiles: TStringList);
begin
  if Assigned(FOnWaitForBitmapsAvailability) then
    FOnWaitForBitmapsAvailability(AListOfFiles)
  else
    raise Exception.Create('OnWaitForBitmapsAvailability is not assigned.');
end;


procedure TActionExecution.DoOnSetEditorSleepProgressBarMax(AMaxValue: Integer);
begin
  if Assigned(FOnSetEditorSleepProgressBarMax) then
    FOnSetEditorSleepProgressBarMax(AMaxValue)
  else
    raise Exception.Create('FOnSetEditorSleepProgressBarMax is not assigned.');
end;


procedure TActionExecution.DoOnSetEditorSleepProgressBarPosition(APositionValue: Integer);
begin
  if Assigned(FOnSetEditorSleepProgressBarPosition) then
    FOnSetEditorSleepProgressBarPosition(APositionValue)
  else
    raise Exception.Create('FOnSetEditorSleepProgressBarPosition is not assigned.');
end;


procedure TActionExecution.DoOnSetEditorSleepInfo(AElapsedTime, ARemainingTime: string);
begin
  if Assigned(FOnSetEditorSleepInfo) then
    FOnSetEditorSleepInfo(AElapsedTime, ARemainingTime)
  else
    raise Exception.Create('FOnSetEditorSleepInfo is not assigned.');
end;


procedure TActionExecution.DoOnGetSelfHandles(AListOfSelfHandles: TStringList);
begin
  if Assigned(FOnGetSelfHandles) then
    FOnGetSelfHandles(AListOfSelfHandles)
  else
    raise Exception.Create('FOnGetSelfHandles is not assigned.');
end;


procedure TActionExecution.ExecuteClickAction(var AClickOptions: TClkClickOptions);
var
  MouseParams: TStringList;
  XClick, YClick: Integer;
  Control_Left, Control_Top, Control_Width, Control_Height: Integer;
  MXOffset, MYOffset: Integer;
begin
  MouseParams := TStringList.Create;
  try
    Control_Left := StrToIntDef(GetActionVarValue('$Control_Left$'), 0);
    Control_Top := StrToIntDef(GetActionVarValue('$Control_Top$'), 0);
    Control_Width := StrToIntDef(GetActionVarValue('$Control_Width$'), 0);
    Control_Height := StrToIntDef(GetActionVarValue('$Control_Height$'), 0);

    XClick := Control_Left; //global in screen
    YClick := Control_Top;

    MXOffset := StrToIntDef(EvaluateReplacements(AClickOptions.XOffset), 0);
    MYOffset := StrToIntDef(EvaluateReplacements(AClickOptions.YOffset), 0);

    case AClickOptions.XClickPointReference of
      xrefLeft: Inc(XClick, MXOffset);
      xrefRight: Inc(XClick, MXOffset + Control_Width);
      xrefWidth: Inc(XClick, MXOffset - Control_Width - Control_Left);    ///????????????????
      xrefVar: XClick := StrToIntDef(EvaluateReplacements(AClickOptions.XClickPointVar), 0) + MXOffset;
      xrefAbsolute: XClick := MXOffset;
    end;

    case AClickOptions.YClickPointReference of
      yrefTop: Inc(YClick, MYOffset);
      yrefBottom: Inc(YClick, MYOffset + Control_Height);
      yrefHeight: Inc(YClick, MYOffset - Control_Height - Control_Top);    ///????????????????
      yrefVar: YClick := StrToIntDef(EvaluateReplacements(AClickOptions.YClickPointVar), 0) + MYOffset;
      yrefAbsolute: YClick := MYOffset;
    end;

    MouseParams.Values[CMouseX] := IntToStr(XClick);
    MouseParams.Values[CMouseY] := IntToStr(YClick);

    case AClickOptions.MouseButton of
      mbLeft: MouseParams.Values[CMouseButton] := CMouseButtonLeft;
      mbRight: MouseParams.Values[CMouseButton] := CMouseButtonRight;
      mbMiddle: MouseParams.Values[CMouseButton] := CMouseButtonMiddle;
      else
      begin
      end;
    end;

    MouseParams.Values[CMouseShiftState] := '';
    if AClickOptions.ClickWithCtrl then
      MouseParams.Values[CMouseShiftState] := MouseParams.Values[CMouseShiftState] + CShiftStateCtrl;

    if AClickOptions.ClickWithAlt then
      MouseParams.Values[CMouseShiftState] := MouseParams.Values[CMouseShiftState] + ',' + CShiftStateAlt;

    if AClickOptions.ClickWithShift then
      MouseParams.Values[CMouseShiftState] := MouseParams.Values[CMouseShiftState] + ',' + CShiftStateShift;

    if AClickOptions.ClickWithDoubleClick then
      MouseParams.Values[CMouseShiftState] := MouseParams.Values[CMouseShiftState] + ',' + CShiftStateDoubleClick;

    MouseParams.Values[CMouseCursorLeaveMouse] := IntToStr(Ord(AClickOptions.LeaveMouse));
    MouseParams.Values[CMouseMoveWithoutClick] := IntToStr(Ord(AClickOptions.MoveWithoutClick));

    MouseParams.Values[CMouseClickType] := IntToStr(AClickOptions.ClickType);

    if AClickOptions.ClickType = CMouseClickType_Drag then  ///Dest
    begin
      XClick := Control_Left; //global in screen
      YClick := Control_Top;

      MXOffset := StrToIntDef(EvaluateReplacements(AClickOptions.XOffsetDest), 0);
      MYOffset := StrToIntDef(EvaluateReplacements(AClickOptions.YOffsetDest), 0);

      case AClickOptions.XClickPointReferenceDest of
        xrefLeft: Inc(XClick, MXOffset);
        xrefRight: Inc(XClick, MXOffset + Control_Width);
        xrefWidth: Inc(XClick, MXOffset - Control_Width - Control_Left);    ///????????????????
        xrefVar: XClick := StrToIntDef(EvaluateReplacements(AClickOptions.XClickPointVarDest), 0) + MXOffset;
        xrefAbsolute: XClick := MXOffset;
      end;

      case AClickOptions.YClickPointReferenceDest of
        yrefTop: Inc(YClick, MYOffset);
        yrefBottom: Inc(YClick, MYOffset + Control_Height);
        yrefHeight: Inc(YClick, MYOffset - Control_Height - Control_Top);    ///????????????????
        yrefVar: YClick := StrToIntDef(EvaluateReplacements(AClickOptions.YClickPointVarDest), 0) + MYOffset;
        yrefAbsolute: YClick := MYOffset;
      end;

      MouseParams.Values[CMouseXDest] := IntToStr(XClick);
      MouseParams.Values[CMouseYDest] := IntToStr(YClick);
    end; ///Dest

    case AClickOptions.ClickType of
      CMouseClickType_Click, CMouseClickType_Drag:
        ClickTControl(MouseParams);

      CMouseClickType_MouseDown:
        MouseDownTControl(MouseParams);

      CMouseClickType_MouseUp:
        MouseUpTControl(MouseParams);
    end;
  finally
    MouseParams.Free;
  end;
end;


function TActionExecution.ExecuteMultiClickAction(var AClickOptions: TClkClickOptions): Boolean;
var
  i: Integer;
  StopAllActionsOnDemandAddr: PBoolean;
begin
  if FStopAllActionsOnDemandFromParent <> nil then
    StopAllActionsOnDemandAddr := FStopAllActionsOnDemandFromParent
  else
    StopAllActionsOnDemandAddr := FStopAllActionsOnDemand;

  Result := True;

  for i := 0 to AClickOptions.Count - 1 do
  begin
    ExecuteClickAction(AClickOptions);
    Application.ProcessMessages;
    Sleep(3);

    //memLogErr.Lines.Add('$Current_Mouse_Y$: ' + EvaluateReplacements('$Current_Mouse_Y$'));

    if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) and (GetAsyncKeyState(VK_F2) < 0) then
    begin
      if FStopAllActionsOnDemandFromParent <> nil then
        FStopAllActionsOnDemandFromParent^ := True;

      FStopAllActionsOnDemand^ := True;
      Exit;
    end;

    if StopAllActionsOnDemandAddr^ then
    begin
      Result := False;
      Break;
    end;
  end;
end;


function TActionExecution.ExecuteExecAppAction(var AExecAppOptions: TClkExecAppOptions; var AActionOptions: TClkActionOptions): Boolean;
var
  ACmd: string;
  i: Integer;
  AllParams: TStringList;

  {$IFnDEF FPC}
    hwnd: THandle;
    ShellExecuteRes: Cardinal;
    AParams: string;
  {$ELSE}
    AProcess: TProcess;
    ExeInput, ExeOutput: string;
    TempStringList: TStringList;
    tk: QWord;
    TempBuffer: array of Byte;
    MemStream: TMemoryStream;
    TimeoutForAppRun: Integer;
  {$ENDIF}
begin
  ACmd := EvaluateReplacements(AExecAppOptions.PathToApp);

  Result := True;

  if not FileExists(ACmd) then
  begin
    Result := False;
    SetActionVarValue('$ExecAction_Err$', 'File not found: ' + ACmd);
    Exit;
  end;

  AllParams := TStringList.Create;
  try
    AllParams.Text := AExecAppOptions.ListOfParams;

    {$IFnDEF FPC}   //backwards compatibility with Delphi, where there is no TProcess.  Still, there is CreateProcess and WaitForSingleObject.
      AParams := '';
      for i := 0 to AllParams.Count - 1 do
        AParams := AParams + '"' + EvaluateReplacements(AllParams.Strings[i]) + '" ';

      if AParams > '' then
        Delete(AParams, Length(AParams), 1); //delete last ' '

      if AExecAppOptions.WaitForApp then
        hwnd := Handle
      else
        hwnd := 0;

      ShellExecuteRes := ShellExecute(hwnd, 'open', PChar(ACmd), PChar(AParams), PChar(ExtractFileDir(ACmd)), SW_SHOW);
      if ShellExecuteRes > 32 then
      begin
        Result := True;
        SetActionVarValue('$ExecAction_Err$', '');
      end
      else
      begin
        Result := False;
        SetActionVarValue('$ExecAction_Err$', 'ShellExecute error code: ' + IntToStr(ShellExecuteRes));
      end;
    {$ELSE}
      try
        AProcess := TProcess.Create(nil);
        MemStream := TMemoryStream.Create;
        try
          AProcess.Executable := ACmd;

          for i := 0 to AllParams.Count - 1 do
            AProcess.Parameters.Add(EvaluateReplacements(AllParams.Strings[i]));

          AProcess.Options := [poUsePipes, poStderrToOutPut{, poPassInput}];
          AProcess.StartupOptions := AProcess.StartupOptions + [suoUseShowWindow];

          if AExecAppOptions.NoConsole then
            AProcess.Options := AProcess.Options + [poNoConsole];

          case AExecAppOptions.UseInheritHandles of
            uihNo:
              AProcess.InheritHandles := False;

            uihYes:
              AProcess.InheritHandles := True;

            uihOnlyWithStdInOut:
               AProcess.InheritHandles := AExecAppOptions.WaitForApp or
                                         (AExecAppOptions.AppStdIn > '');
          end;  //when InheritHandles is True, the executed application may keep open a listening socket, after closing Clicker, preventing further Clicker processes to listen

          AProcess.ShowWindow := swoShow;

          AProcess.CurrentDirectory := EvaluateReplacements(AExecAppOptions.CurrentDir);
          if AProcess.CurrentDirectory = '' then
            AProcess.CurrentDirectory := ExtractFileDir(ACmd);

          //if AExecAppOptions.WaitForApp then
          //  AProcess.Options := AppProcess.Options + [poWaitOnExit];  //do not use poWaitOnExit, because it blocks the application

          AProcess.Execute;     //run here

          if AProcess.Input = nil then
          begin
            Result := False;
            SetActionVarValue('$ExecAction_Err$', 'Input stream is nil after exec.');
          end;

          ExeInput := EvaluateReplacements(FastReplace_45ToReturn(AExecAppOptions.AppStdIn));

          TempStringList := TStringList.Create;
          try
            TempStringList.Text := ExeInput;
            TempStringList.SaveToStream(AProcess.Input);
          finally
            TempStringList.Free;
          end;

          TimeoutForAppRun := AActionOptions.ActionTimeout;
          if AExecAppOptions.WaitForApp then
          begin
            DoOnSetEditorEnabledState(False);

            try
              DoOnSetEditorTimeoutProgressBarMax(TimeoutForAppRun);
              tk := GetTickCount64;

              repeat
                Application.ProcessMessages;
                Sleep(100);

                if (AProcess.Output <> nil) and (AProcess.Output.NumBytesAvailable > 0) then
                begin
                  SetLength(TempBuffer, AProcess.Output.NumBytesAvailable);
                  if Length(TempBuffer) > 0 then
                  begin
                    AProcess.Output.Read(TempBuffer[0], Length(TempBuffer));
                    MemStream.Write(TempBuffer[0], Length(TempBuffer));
                  end;
                end;

                DoOnSetEditorTimeoutProgressBarPosition(GetTickCount64 - tk);

                if GetTickCount64 - tk >= TimeoutForAppRun then
                begin
                  if FTemplateFileName = nil then
                    raise Exception.Create('FTemplateFileName not set.');

                  PrependErrorMessageToActionVar('Timeout at "' + AActionOptions.ActionName + '" in ' + FTemplateFileName^);
                  Result := False;
                  Break;
                end;

                if ((FStopAllActionsOnDemand <> nil) and FStopAllActionsOnDemand^) or
                   ((FStopAllActionsOnDemandFromParent <> nil) and FStopAllActionsOnDemandFromParent^) then
                begin
                  PrependErrorMessageToActionVar('App Execution manually stopped at "' + AActionOptions.ActionName + '" in ' + FTemplateFileName^);
                  Result := False;
                  Break;
                end;
              until not AProcess.Active;

              if Result then
                SetActionVarValue('$ExecAction_Err$', '');
            finally
              DoOnSetEditorEnabledState(True);
            end;
          end;

          TempStringList := TStringList.Create;
          try
            if (AProcess.Output <> nil) and (AProcess.Output.NumBytesAvailable > 0) then     //read once more (in case the timeout stopped the reading)
            begin
              SetLength(TempBuffer, AProcess.Output.NumBytesAvailable);
              AProcess.Output.Read(TempBuffer[0], Length(TempBuffer));
              MemStream.Write(TempBuffer[0], Length(TempBuffer));
            end;

            MemStream.Position := 0;
            TempStringList.LoadFromStream(MemStream);

            ExeOutput := TempStringList.Text;
          finally
            TempStringList.Free;
          end;
        finally
          AProcess.Free;
          MemStream.Free;
        end;

        SetActionVarValue('$ExecAction_StdOut$', FastReplace_ReturnTo45(ExeOutput));
      except
        on E: Exception do
        begin
          Result := False;
          if (Pos('Stream write error', E.Message) > 0) and
             (AExecAppOptions.UseInheritHandles = uihNo) then
            E.Message := E.Message + '  Make sure the UseInheritHandles option is set to "Yes" or "Only with StdIn / StdOut", when using StdIn or StdOut.';

          if FTemplateFileName = nil then
            raise Exception.Create('FTemplateFileName not set.');

          SetActionVarValue('$ExecAction_Err$', 'Exception "' + E.Message + '" at "' + AActionOptions.ActionName + '" in ' + FTemplateFileName^);
        end;
      end;
    {$ENDIF}
  finally
    AllParams.Free;
  end;

  DoOnSetEditorTimeoutProgressBarPosition(0);
end;


//this function should eventually be split into FindControl and FindSubControl
function TActionExecution.ExecuteFindControlAction(var AFindControlOptions: TClkFindControlOptions; var AActionOptions: TClkActionOptions; IsSubControl: Boolean): Boolean; //returns True if found
{$IFDEF FPC}
  const
    clSystemColor = $FF000000;
{$ENDIF}

  procedure UpdateActionVarValuesFromControl(AControl: TCompRec);
  var
    Control_Width, Control_Height: Integer;
  begin
    Control_Width := AControl.ComponentRectangle.Right - AControl.ComponentRectangle.Left;
    Control_Height := AControl.ComponentRectangle.Bottom - AControl.ComponentRectangle.Top;

    SetActionVarValue('$Control_Text$', AControl.Text);
    SetActionVarValue('$Control_Left$', IntToStr(AControl.ComponentRectangle.Left));
    SetActionVarValue('$Control_Top$', IntToStr(AControl.ComponentRectangle.Top));
    SetActionVarValue('$Control_Right$', IntToStr(AControl.ComponentRectangle.Right));
    SetActionVarValue('$Control_Bottom$', IntToStr(AControl.ComponentRectangle.Bottom));
    SetActionVarValue('$Control_Width$', IntToStr(Control_Width));
    SetActionVarValue('$Control_Height$', IntToStr(Control_Height));
    SetActionVarValue('$Half_Control_Width$', IntToStr(Control_Width shr 1));
    SetActionVarValue('$Half_Control_Height$', IntToStr(Control_Height shr 1));

    SetActionVarValue('$Control_Class$', AControl.ClassName);
    SetActionVarValue('$Control_Handle$', IntToStr(AControl.Handle));
    SetActionVarValue('$DebugVar_SubCnvXOffset$', IntToStr(AControl.XOffsetFromParent));
    SetActionVarValue('$DebugVar_SubCnvYOffset$', IntToStr(AControl.YOffsetFromParent));
  end;

  procedure SetDbgImgPos(AMatchBitmapAlgorithm: TMatchBitmapAlgorithm; AFindControlInputData: TFindControlInputData; AResultedControl: TCompRec);
  begin
    case AFindControlOptions.MatchBitmapAlgorithm of
      mbaBruteForce:
      begin
        frClickerActions.imgDebugGrid.Left := AResultedControl.XOffsetFromParent;
        frClickerActions.imgDebugGrid.Top := AResultedControl.YOffsetFromParent;
      end;

      mbaXYMultipleAndOffsets:
      begin
        frClickerActions.imgDebugGrid.Left := AFindControlInputData.InitialRectangleOffsets.Left;
        frClickerActions.imgDebugGrid.Top := AFindControlInputData.InitialRectangleOffsets.Top;
      end;
    end; //case
  end;

var
  i, j, n: Integer;
  ListOfBitmapFiles: TStringList;
  ResultedControl: TCompRec;
  InitialTickCount, Timeout: QWord;
  FindControlInputData: TFindControlInputData;
  StopAllActionsOnDemandAddr: Pointer;
  EvalFG, EvalBG: string;
  TemplateDir: string;
begin
  Result := False;

  frClickerActions.DebuggingInfoAvailable := False;

  SetActionVarValue('$ExecAction_Err$', '');
  ResultedControl.XOffsetFromParent := 0; //init here, in case FindControlOnScreen does not update it
  ResultedControl.YOffsetFromParent := 0; //init here, in case FindControlOnScreen does not update it

  FindControlInputData.MatchingMethods := [];
  if AFindControlOptions.MatchCriteria.WillMatchText then
    FindControlInputData.MatchingMethods := FindControlInputData.MatchingMethods + [mmText];

  if AFindControlOptions.MatchCriteria.WillMatchClassName then
    FindControlInputData.MatchingMethods := FindControlInputData.MatchingMethods + [mmClass];

  if AFindControlOptions.MatchCriteria.WillMatchBitmapText then
    FindControlInputData.MatchingMethods := FindControlInputData.MatchingMethods + [mmBitmapText];

  if AFindControlOptions.MatchCriteria.WillMatchBitmapFiles then
    FindControlInputData.MatchingMethods := FindControlInputData.MatchingMethods + [mmBitmapFiles];

  if FindControlInputData.MatchingMethods = [] then
  begin
    SetActionVarValue('$ExecAction_Err$', 'No match criteria set.');
    Result := False;
  end;

  FindControlInputData.SearchAsSubControl := IsSubControl;
  FindControlInputData.DebugBitmap := nil;

  FindControlInputData.ClassName := EvaluateReplacements(AFindControlOptions.MatchClassName);
  FindControlInputData.Text := EvaluateReplacements(AFindControlOptions.MatchText, True);

  if AFindControlOptions.MatchCriteria.WillMatchBitmapText then
    if FindControlInputData.Text = '' then
    begin
      SetActionVarValue('$ExecAction_Err$', 'The searched text is empty.');
      SetActionVarValue('$DebugVar_BitmapText$', 'Matching an empty string will lead to a false match.');
      Result := False;
      Exit;
    end;

  FindControlInputData.ClassNameSeparator := AFindControlOptions.MatchClassNameSeparator;
  FindControlInputData.TextSeparator := AFindControlOptions.MatchTextSeparator;
  FindControlInputData.ColorError := StrToIntDef(EvaluateReplacements(AFindControlOptions.ColorError), 0);
  FindControlInputData.AllowedColorErrorCount := StrToIntDef(EvaluateReplacements(AFindControlOptions.AllowedColorErrorCount), 0);
  FindControlInputData.DebugTemplateName := FTemplateFileName^;

  n := Length(AFindControlOptions.MatchBitmapText);
  if n = 0 then
  begin
    SetLength(AFindControlOptions.MatchBitmapText, 1); //this is required, to execute the next for loop, without font profiles
    n := 1;                                            //this code will have to be split in FindControl and FindSubControl. Then, this part will have to be deleted.
    frClickerActions.frClickerFindControl.CreateBMPTextFrames(1);
  end;

  FindControlInputData.StartSearchingWithCachedControl := AFindControlOptions.StartSearchingWithCachedControl;
  FindControlInputData.CachedControlLeft := StrToIntDef(EvaluateReplacements(AFindControlOptions.CachedControlLeft), 0);
  FindControlInputData.CachedControlTop := StrToIntDef(EvaluateReplacements(AFindControlOptions.CachedControlTop), 0);

  for j := 0 to n - 1 do //number of font profiles
  begin
    if j > n - 1 then  //it seems that a FP bug allows "j" to go past n - 1. It may happen on EnumerateWindows only. At best, the memory is overwritten, which causes this behavior.
      Break;

    FindControlInputData.BitmapToSearchFor := TBitmap.Create;
    try
      FindControlInputData.BitmapToSearchFor.PixelFormat := pf24bit;

      if AFindControlOptions.MatchCriteria.WillMatchBitmapText then
      begin
        EvalFG := EvaluateReplacements(AFindControlOptions.MatchBitmapText[j].ForegroundColor, True);
        EvalBG := EvaluateReplacements(AFindControlOptions.MatchBitmapText[j].BackgroundColor, True);

        SetActionVarValue('$DebugVar_TextColors$',
                          'FileName=' + FTemplateFileName^ +
                          ' FG=' + IntToHex(frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].FGColor, 8) +
                          ' BG=' + IntToHex(frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].BGColor, 8) +
                          ' Eval(FG)=' + EvaluateReplacements(AFindControlOptions.MatchBitmapText[j].ForegroundColor, False) + '=' + EvalFG +
                          ' Eval(BG)=' + EvaluateReplacements(AFindControlOptions.MatchBitmapText[j].BackgroundColor, False) + '=' + EvalBG );

        if frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].FGColor and clSystemColor <> 0 then
        begin
          frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].FGColor := clFuchsia;
          FLog.Lines.Add('System color found on text FG: $' + IntToHex(frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].FGColor, 8));
        end;

        if frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].BGColor and clSystemColor <> 0 then
        begin
          frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].BGColor := clLime;
          FLog.Lines.Add('System color found on text BG: $' + IntToHex(frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].BGColor, 8));
        end;

        SetActionVarValue('$DebugVar_BitmapText$', FindControlInputData.Text);

        // frClickerActions.frClickerFindControl.PreviewText;

        try
          PreviewTextOnBmp(AFindControlOptions, FindControlInputData.Text, j, FindControlInputData.BitmapToSearchFor);
        except
          on E: Exception do
            raise Exception.Create(E.Message + '  Profile[' + IntToStr(j) + ']: "' + frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].ProfileName + '".   Searched text: "' + FindControlInputData.Text + '"');
        end;
        //This is the original code for getting the text from the editor, instead of rendering with PreviewTextOnBmp. It should do the same thing.
        //FindControlInputData.BitmapToSearchFor.Width := frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].PreviewImageBitmap.Width;
        //FindControlInputData.BitmapToSearchFor.Height := frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].PreviewImageBitmap.Height;
        //FindControlInputData.BitmapToSearchFor.Canvas.Draw(0, 0, frClickerActions.frClickerFindControl.BMPTextFontProfiles[j].PreviewImageBitmap);   //updated above by PreviewText

        FindControlInputData.DebugBitmap := frClickerActions.imgDebugBmp.Picture.Bitmap;
        FindControlInputData.DebugGrid := frClickerActions.imgDebugGrid;
      end;  //WillMatchBitmapText

      if AFindControlOptions.UseWholeScreen then
      begin
        FindControlInputData.GlobalSearchArea.Left := 0;
        FindControlInputData.GlobalSearchArea.Top := 0;
        FindControlInputData.GlobalSearchArea.Right := Screen.Width;
        FindControlInputData.GlobalSearchArea.Bottom := Screen.Height;
      end
      else
      begin
        FindControlInputData.GlobalSearchArea.Left := StrToIntDef(EvaluateReplacements(AFindControlOptions.InitialRectange.Left), 0);
        FindControlInputData.GlobalSearchArea.Top := StrToIntDef(EvaluateReplacements(AFindControlOptions.InitialRectange.Top), 0);
        FindControlInputData.GlobalSearchArea.Right := StrToIntDef(EvaluateReplacements(AFindControlOptions.InitialRectange.Right), 0);
        FindControlInputData.GlobalSearchArea.Bottom := StrToIntDef(EvaluateReplacements(AFindControlOptions.InitialRectange.Bottom), 0);
      end;

      if not AFindControlOptions.WaitForControlToGoAway then
      begin
        FLog.Lines.Add('Find (Sub)Control with text = "' + FindControlInputData.Text + '"');
        FLog.Lines.Add('Raw GlobalSearchArea.Left = ' + IntToStr(FindControlInputData.GlobalSearchArea.Left));
        FLog.Lines.Add('Raw GlobalSearchArea.Top = ' + IntToStr(FindControlInputData.GlobalSearchArea.Top));
        FLog.Lines.Add('Raw GlobalSearchArea.Right = ' + IntToStr(FindControlInputData.GlobalSearchArea.Right));
        FLog.Lines.Add('Raw GlobalSearchArea.Bottom = ' + IntToStr(FindControlInputData.GlobalSearchArea.Bottom));
      end;

      FindControlInputData.InitialRectangleOffsets.Left := StrToIntDef(EvaluateReplacements(AFindControlOptions.InitialRectange.LeftOffset), 0);
      FindControlInputData.InitialRectangleOffsets.Top := StrToIntDef(EvaluateReplacements(AFindControlOptions.InitialRectange.TopOffset), 0);
      FindControlInputData.InitialRectangleOffsets.Right := StrToIntDef(EvaluateReplacements(AFindControlOptions.InitialRectange.RightOffset), 0);
      FindControlInputData.InitialRectangleOffsets.Bottom := StrToIntDef(EvaluateReplacements(AFindControlOptions.InitialRectange.BottomOffset), 0);

      Inc(FindControlInputData.GlobalSearchArea.Left, FindControlInputData.InitialRectangleOffsets.Left);
      Inc(FindControlInputData.GlobalSearchArea.Top, FindControlInputData.InitialRectangleOffsets.Top);
      Inc(FindControlInputData.GlobalSearchArea.Right, FindControlInputData.InitialRectangleOffsets.Right);
      Inc(FindControlInputData.GlobalSearchArea.Bottom, FindControlInputData.InitialRectangleOffsets.Bottom);

      if not AFindControlOptions.WaitForControlToGoAway then
      begin
        FLog.Lines.Add('(With Offset) GlobalSearchArea.Left = ' + IntToStr(FindControlInputData.GlobalSearchArea.Left));
        FLog.Lines.Add('(With Offset) GlobalSearchArea.Top = ' + IntToStr(FindControlInputData.GlobalSearchArea.Top));
        FLog.Lines.Add('(With Offset) GlobalSearchArea.Right = ' + IntToStr(FindControlInputData.GlobalSearchArea.Right));
        FLog.Lines.Add('(With Offset) GlobalSearchArea.Bottom = ' + IntToStr(FindControlInputData.GlobalSearchArea.Bottom));
      end;

      if (FindControlInputData.GlobalSearchArea.Right - FindControlInputData.GlobalSearchArea.Left < 1) or
         (FindControlInputData.GlobalSearchArea.Bottom - FindControlInputData.GlobalSearchArea.Top < 1) then
      begin
        frClickerActions.imgDebugBmp.Picture.Bitmap.Width := 300;
        frClickerActions.imgDebugBmp.Picture.Bitmap.Height := 300;
        frClickerActions.imgDebugBmp.Canvas.Brush.Color := clWhite;
        frClickerActions.imgDebugBmp.Canvas.Pen.Color := clRed;
        frClickerActions.imgDebugBmp.Canvas.Font.Color := clRed;
        frClickerActions.imgDebugBmp.Canvas.TextOut(0, 0, 'Invalid search area:   ');
        frClickerActions.imgDebugBmp.Canvas.TextOut(0, 15, 'Rectangle width: ' + IntToStr(FindControlInputData.GlobalSearchArea.Right - FindControlInputData.GlobalSearchArea.Left) + '   ');
        frClickerActions.imgDebugBmp.Canvas.TextOut(0, 30, 'Rectangle height: ' + IntToStr(FindControlInputData.GlobalSearchArea.Bottom - FindControlInputData.GlobalSearchArea.Top) + '   ');
        frClickerActions.imgDebugBmp.Canvas.TextOut(0, 45, 'Please verify offsets.   ');

        frClickerActions.imgDebugBmp.Canvas.TextOut(0, 65, 'GlobalRectangle left (with offset): ' + IntToStr(FindControlInputData.GlobalSearchArea.Left) + '   ');
        frClickerActions.imgDebugBmp.Canvas.TextOut(0, 80, 'GlobalRectangle top (with offset): ' + IntToStr(FindControlInputData.GlobalSearchArea.Top) + '   ');
        frClickerActions.imgDebugBmp.Canvas.TextOut(0, 95, 'GlobalRectangle right (with offset): ' + IntToStr(FindControlInputData.GlobalSearchArea.Right) + '   ');
        frClickerActions.imgDebugBmp.Canvas.TextOut(0, 110, 'GlobalRectangle bottom (with offset): ' + IntToStr(FindControlInputData.GlobalSearchArea.Bottom) + '   ');

        frClickerActions.imgDebugBmp.Canvas.TextOut(0, 135, 'Left offset: ' + IntToStr(FindControlInputData.InitialRectangleOffsets.Left) + '   ');
        frClickerActions.imgDebugBmp.Canvas.TextOut(0, 150, 'Top offset: ' + IntToStr(FindControlInputData.InitialRectangleOffsets.Top) + '   ');
        frClickerActions.imgDebugBmp.Canvas.TextOut(0, 165, 'Right offset: ' + IntToStr(FindControlInputData.InitialRectangleOffsets.Right) + '   ');
        frClickerActions.imgDebugBmp.Canvas.TextOut(0, 180, 'Bottom offset: ' + IntToStr(FindControlInputData.InitialRectangleOffsets.Bottom) + '   ');

        frClickerActions.imgDebugBmp.Canvas.TextOut(0, 210, 'FileName: '+ ExtractFileName(FTemplateFileName^));

        FLog.Lines.Add('Exiting find control, because the search area is negative.');
        FLog.Lines.Add('');

        Result := False;
        Continue; //Exit;  moves to the next "for j" iteration
      end;

      InitialTickCount := GetTickCount64;
      if AActionOptions.ActionTimeout < 0 then
        Timeout := 0
      else
        Timeout := AActionOptions.ActionTimeout;

      if FStopAllActionsOnDemandFromParent <> nil then
      begin
        //MessageBox(Handle, 'Using global stop on demand.', PChar(Caption), MB_ICONINFORMATION);
        StopAllActionsOnDemandAddr := FStopAllActionsOnDemandFromParent;
      end
      else
      begin
        //MessageBox(Handle, 'Using local stop on demand.', PChar(Caption), MB_ICONINFORMATION);
        StopAllActionsOnDemandAddr := FStopAllActionsOnDemand;
      end;

      //clear debug image
      frClickerActions.imgDebugBmp.Canvas.Pen.Color := clWhite;
      frClickerActions.imgDebugBmp.Canvas.Brush.Color := clWhite;
      frClickerActions.imgDebugBmp.Canvas.Rectangle(0, 0, frClickerActions.imgDebugBmp.Width, frClickerActions.imgDebugBmp.Height);

      case AFindControlOptions.MatchCriteria.SearchForControlMode of
        sfcmGenGrid:
        begin
          if (mmText in FindControlInputData.MatchingMethods) or
             (mmClass in FindControlInputData.MatchingMethods) or
             (mmBitmapText in FindControlInputData.MatchingMethods) then
          begin
            try
              if FindControlOnScreen(AFindControlOptions.MatchBitmapAlgorithm, AFindControlOptions.MatchBitmapAlgorithmSettings, FindControlInputData, InitialTickCount, Timeout, StopAllActionsOnDemandAddr, ResultedControl) then
              begin
                UpdateActionVarValuesFromControl(ResultedControl);
                frClickerActions.DebuggingInfoAvailable := True;

                Result := True;
                FLog.Lines.Add('Found text: "' + AFindControlOptions.MatchText + '" in ' + IntToStr(GetTickCount64 - InitialTickCount) + 'ms.');

                Exit;  //to prevent further searching for bitmap files
              end;
            finally
              SetDbgImgPos(AFindControlOptions.MatchBitmapAlgorithm, FindControlInputData, ResultedControl);
            end;
          end;

          if mmBitmapFiles in FindControlInputData.MatchingMethods then
          begin
            ListOfBitmapFiles := TStringList.Create;
            try
              ListOfBitmapFiles.Text := AFindControlOptions.MatchBitmapFiles;
              FLog.Lines.Add('Bmp file count to search with: ' + IntToStr(ListOfBitmapFiles.Count));

              if FExecutingActionFromRemote = nil then
                raise Exception.Create('FExecutingActionFromRemote is not assigned.');

              if FFileLocationOfDepsIsMem = nil then
                raise Exception.Create('FFileLocationOfDepsIsMem is not assigned.');

              if FTemplateFileName = nil then
                TemplateDir := 'FTemplateFileName not set.'
              else
                TemplateDir := ExtractFileDir(FTemplateFileName^);

              for i := 0 to ListOfBitmapFiles.Count - 1 do
                ListOfBitmapFiles.Strings[i] := StringReplace(ListOfBitmapFiles.Strings[i], '$TemplateDir$', TemplateDir, [rfReplaceAll]);

              if FExecutingActionFromRemote^ and FFileLocationOfDepsIsMem^ then
                DoOnWaitForBitmapsAvailability(ListOfBitmapFiles);

              for i := 0 to ListOfBitmapFiles.Count - 1 do
              begin
                if not DoOnLoadBitmap(FindControlInputData.BitmapToSearchFor, ListOfBitmapFiles.Strings[i]) then
                begin
                  AppendErrorMessageToActionVar('File not found: "' + ListOfBitmapFiles.Strings[i] + '" ');
                  Continue;
                end;

                FindControlInputData.DebugBitmap := frClickerActions.imgDebugBmp.Picture.Bitmap;
                FindControlInputData.DebugGrid := frClickerActions.imgDebugGrid;

                //memLogErr.Lines.Add('DebugBitmap pixel format: ' + IntToStr(Ord(FindControlInputData.DebugBitmap.PixelFormat))); // [6]  - 24-bit

                InitialTickCount := GetTickCount64;
                if AActionOptions.ActionTimeout < 0 then
                  Timeout := 0
                else
                  Timeout := AActionOptions.ActionTimeout;

                if FindControlOnScreen(AFindControlOptions.MatchBitmapAlgorithm, AFindControlOptions.MatchBitmapAlgorithmSettings, FindControlInputData, InitialTickCount, Timeout, StopAllActionsOnDemandAddr, ResultedControl) then
                begin
                  UpdateActionVarValuesFromControl(ResultedControl);
                  frClickerActions.DebuggingInfoAvailable := True;

                  Result := True;
                  Exit;  //to prevent further searching for other bitmap files
                end;
              end;
            finally
              ListOfBitmapFiles.Free;
              SetDbgImgPos(AFindControlOptions.MatchBitmapAlgorithm, FindControlInputData, ResultedControl);
            end;
          end; //WillMatchBitmapFiles
        end; //generated grid

        sfcmEnumWindows:        //Caption OR Class
        begin
          if FindWindowOnScreenByCaptionOrClass(FindControlInputData, InitialTickCount, Timeout, StopAllActionsOnDemandAddr, ResultedControl) then
          begin
            UpdateActionVarValuesFromControl(ResultedControl);
            Result := True;
            frClickerActions.DebuggingInfoAvailable := True;
            Exit;  //to prevent further searching for bitmap files
          end;
        end;

        sfcmFindWindow:         //Caption AND Class
        begin
          if FindWindowOnScreenByCaptionAndClass(FindControlInputData, InitialTickCount, Timeout, StopAllActionsOnDemandAddr, ResultedControl) then
          begin
            UpdateActionVarValuesFromControl(ResultedControl);
            Result := True;
            frClickerActions.DebuggingInfoAvailable := True;
            Exit;  //to prevent further searching for bitmap files
          end;
        end;
      end //case
    finally
      FindControlInputData.BitmapToSearchFor.Free;
    end;

    if Result then
      Break;
  end;  //for j
end;


function TActionExecution.ExecuteFindControlActionWithTimeout(var AFindControlOptions: TClkFindControlOptions; var AActionOptions: TClkActionOptions; IsSubControl: Boolean): Boolean; //returns True if found
var
  tk: QWord;
  AttemptCount: Integer;
begin
  tk := GetTickCount64;
  frClickerActions.prbTimeout.Max := AActionOptions.ActionTimeout;
  frClickerActions.prbTimeout.Position := 0;
  Result := False;
  AttemptCount := 0;

  repeat
    if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) and (GetAsyncKeyState(VK_F2) < 0) then
    begin
      if FStopAllActionsOnDemandFromParent <> nil then
        FStopAllActionsOnDemandFromParent^ := True;

      if FStopAllActionsOnDemand <> nil then
        FStopAllActionsOnDemand^ := True;
    end;

    if FStopAllActionsOnDemandFromParent <> nil then
      if FStopAllActionsOnDemandFromParent^ then
        if FStopAllActionsOnDemand <> nil then
          FStopAllActionsOnDemand^ := True;

    if FStopAllActionsOnDemand^ then
    begin
      PrependErrorMessageToActionVar('Stopped by user at "' + AActionOptions.ActionName + '" in ' + FTemplateFileName^ + '  ');
      Break;
    end;

    Result := ExecuteFindControlAction(AFindControlOptions, AActionOptions, IsSubControl);

    if AFindControlOptions.WaitForControlToGoAway then  //the control should not be found
      Result := not Result;

    if not Result and AFindControlOptions.AllowToFail then
      Break; //do not set result to True, because it is required to be detected where ExecuteFindControlActionWithTimeout is called

    if Result then
      Break;

    frClickerActions.prbTimeout.Position := GetTickCount64 - tk;
    Application.ProcessMessages;

    Inc(AttemptCount);

    if (frClickerActions.prbTimeout.Max > 0) and (frClickerActions.prbTimeout.Position >= frClickerActions.prbTimeout.Max) then
    begin
      PrependErrorMessageToActionVar('Timeout at "' + AActionOptions.ActionName + '" in ' + FTemplateFileName^ + '  AttemptCount=' + IntToStr(AttemptCount) + '  ');
      Break;
    end;

    Sleep(2);
  until False;

  frClickerActions.prbTimeout.Position := 0;
end;


function TActionExecution.ExecuteSetControlTextAction(var ASetTextOptions: TClkSetTextOptions): Boolean;
var
  Control_Handle: THandle;
  i, Idx: Integer;
  TextToSend: string;
  KeyStrokes: array of TINPUT;
  Err: Integer;
  ErrStr: string;
begin
  Result := True;

  Control_Handle := StrToIntDef(GetActionVarValue('$Control_Handle$'), 0);
  TextToSend := EvaluateReplacements(ASetTextOptions.Text);
  TextToSend := EvaluateHTTP(TextToSend);

  case ASetTextOptions.ControlType of
    stEditBox: SetControlText(Control_Handle, TextToSend);

    stComboBox: SelectComboBoxItem(Control_Handle, 0, TextToSend);

    stKeystrokes:
    begin
      SetLength(KeyStrokes, Length(TextToSend) shl 1);
      try
        for i := 0 to Length(TextToSend) - 1 do   //string len, not array len
        begin
          Idx := i shl 1;
          KeyStrokes[Idx]._Type := INPUT_KEYBOARD; //not sure if needed
          KeyStrokes[Idx].ki.wVk := 0;
          KeyStrokes[Idx].ki.wScan := Ord(TextToSend[i + 1]);
          KeyStrokes[Idx].ki.dwFlags := KEYEVENTF_UNICODE; //0;
          KeyStrokes[Idx].ki.Time := 0;
          KeyStrokes[Idx].ki.ExtraInfo := 0;

          KeyStrokes[Idx + 1]._Type := INPUT_KEYBOARD; //not sure if needed
          KeyStrokes[Idx + 1].ki.wVk := 0;
          KeyStrokes[Idx + 1].ki.wScan := Ord(TextToSend[i + 1]);
          KeyStrokes[Idx + 1].ki.dwFlags := KEYEVENTF_UNICODE or KEYEVENTF_KEYUP;
          KeyStrokes[Idx + 1].ki.Time := 0;
          KeyStrokes[Idx + 1].ki.ExtraInfo := 0;
        end;

        SetLastError(0);
        if Integer(SendInput(Length(KeyStrokes), @KeyStrokes[0], SizeOf(TINPUT))) <> Length(KeyStrokes) then
        begin
          Err := GetLastOSError;
          ErrStr := 'KeyStrokes error: ' + IntToStr(Err) + '  ' + SysErrorMessage(GetLastOSError) + '  Keystrokes count: ' + IntToStr(Length(KeyStrokes));
          SetActionVarValue('$ExecAction_Err$', ErrStr);
          Result := False;
        end;
      finally
        SetLength(KeyStrokes, 0);
      end;
    end;
  end; //case
end;


procedure TActionExecution.CheckManualStopCondition;
begin
  if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) and (GetAsyncKeyState(VK_F2) < 0) then
  begin
    if FStopAllActionsOnDemandFromParent <> nil then
      FStopAllActionsOnDemandFromParent^ := True;

    if FStopAllActionsOnDemand <> nil then
      FStopAllActionsOnDemand^ := True;
  end;

  if FStopAllActionsOnDemandFromParent <> nil then
    if FStopAllActionsOnDemandFromParent^ then
      if FStopAllActionsOnDemand <> nil then
        FStopAllActionsOnDemand^ := True;
end;


function TActionExecution.ExecuteCallTemplateAction(var ACallTemplateOptions: TClkCallTemplateOptions; IsDebugging, AShouldStopAtBreakPoint: Boolean): Boolean;
var
  i: Integer;
  CustomVars: TStringList;
  KeyName, KeyValue, RowString: string;
  Fnm: string;
begin
  Result := False;
  if not Assigned(FOnCallTemplate) then
  begin
    AppendErrorMessageToActionVar('OnCallTemplate not assigned');
    Exit;
  end;

  CustomVars := TStringList.Create;
  try
    CustomVars.Text := FastReplace_45ToReturn(ACallTemplateOptions.ListOfCustomVarsAndValues);

    for i := 0 to CustomVars.Count - 1 do
    begin
      try
        RowString := CustomVars.Strings[i];
        KeyName := Copy(RowString, 1, Pos('=', RowString) - 1);
        KeyValue := Copy(RowString, Pos('=', RowString) + 1, MaxInt);
        KeyValue := EvaluateHTTP(KeyValue);

        if ACallTemplateOptions.EvaluateBeforeCalling then
          SetActionVarValue(KeyName, EvaluateReplacements(KeyValue))
        else
          SetActionVarValue(KeyName, KeyValue);
      except
        //who knows...
      end;
    end;
  finally
    CustomVars.Free;
  end;

  if ACallTemplateOptions.CallOnlyIfCondition then
  begin
    MessageBox(Application.MainForm.Handle, PChar('Using this condition mechanism is deprecated. Please move this condition to the "Condition" tab.' + #13#10 + 'Filename: ' + FTemplateFileName^), PChar(Application.Title), MB_ICONWARNING);

    KeyName := ACallTemplateOptions.CallOnlyIfConditionVarName;
    KeyValue := ACallTemplateOptions.CallOnlyIfConditionVarValue;

    if GetActionVarValue(KeyName) <> KeyValue then
    begin
      Result := True;  //allow further execution
      SetActionVarValue('$ExecAction_Err$', 'Condition not met: ' + GetActionVarValue(KeyName) + ' <> ' + KeyValue);
      Exit;
    end;
  end;

  Fnm := EvaluateReplacements(ACallTemplateOptions.TemplateFileName);

  // the FileExists verification has to be done after checking CallOnlyIfCondition, to allow failing the action based on condition

  Fnm := StringReplace(Fnm, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);

  if not FFileLocationOfDepsIsMem^ then
    if ExtractFileName(Fnm) = Fnm then  //Fnm does not contain a path
    begin
      if FFullTemplatesDir = nil then
        raise Exception.Create('FFullTemplatesDir is not assigned.');

      Fnm := FFullTemplatesDir^ + '\' + Fnm;
    end;

  if FOwnerFrame = nil then
    raise Exception.Create('FOwnerFrame is not assigned.');

  Result := FOnCallTemplate(FOwnerFrame, Fnm, FClickerVars, frClickerActions.imgDebugBmp.Picture.Bitmap, frClickerActions.imgDebugGrid, IsDebugging, AShouldStopAtBreakPoint, FStackLevel^, FExecutesRemotely^);

  if GetActionVarValue('$ExecAction_Err$') <> '' then   ////////////////// ToDo:  improve the error logging
    FLog.Lines.Add(DateTimeToStr(Now) + '  ' + GetActionVarValue('$ExecAction_Err$'));
end;


function TActionExecution.ExecuteLoopedCallTemplateAction(var ACallTemplateOptions: TClkCallTemplateOptions; IsDebugging, AShouldStopAtBreakPoint: Boolean): Boolean;
var
  i: Integer;
  StartValue, StopValue: Integer;
  TempACallTemplateOptions: TClkCallTemplateOptions;
begin
  if not ACallTemplateOptions.CallTemplateLoop.Enabled then
    Result := ExecuteCallTemplateAction(ACallTemplateOptions, IsDebugging, AShouldStopAtBreakPoint)
  else
  begin
    StartValue := StrToIntDef(EvaluateReplacements(ACallTemplateOptions.CallTemplateLoop.InitValue), 0);
    StopValue := StrToIntDef(EvaluateReplacements(ACallTemplateOptions.CallTemplateLoop.EndValue), 0);

    Result := True;
    case ACallTemplateOptions.CallTemplateLoop.Direction of
      ldInc:
      begin
        for i := StartValue to StopValue do
        begin
          SetActionVarValue(ACallTemplateOptions.CallTemplateLoop.Counter, IntToStr(i));

          if ACallTemplateOptions.CallTemplateLoop.BreakCondition <> '' then
            if ACallTemplateOptions.CallTemplateLoop.EvalBreakPosition = lebpBeforeContent then
              if EvaluateActionCondition(ACallTemplateOptions.CallTemplateLoop.BreakCondition, EvaluateReplacements) then
                Break;

          Result := Result and ExecuteCallTemplateAction(ACallTemplateOptions, IsDebugging, AShouldStopAtBreakPoint);

          if ACallTemplateOptions.CallTemplateLoop.BreakCondition <> '' then
            if ACallTemplateOptions.CallTemplateLoop.EvalBreakPosition = lebpAfterContent then
              if EvaluateActionCondition(ACallTemplateOptions.CallTemplateLoop.BreakCondition, EvaluateReplacements) then
                Break;

          CheckManualStopCondition;
          if FStopAllActionsOnDemand <> nil then
            if FStopAllActionsOnDemand^ then
              Break;
        end;
      end;

      ldDec:
      begin
        for i := StartValue downto StopValue do
        begin
          SetActionVarValue(ACallTemplateOptions.CallTemplateLoop.Counter, IntToStr(i));

          if ACallTemplateOptions.CallTemplateLoop.EvalBreakPosition = lebpBeforeContent then
            if EvaluateActionCondition(ACallTemplateOptions.CallTemplateLoop.BreakCondition, EvaluateReplacements) then
              Break;

          Result := Result and ExecuteCallTemplateAction(ACallTemplateOptions, IsDebugging, AShouldStopAtBreakPoint);

          if ACallTemplateOptions.CallTemplateLoop.EvalBreakPosition = lebpBeforeContent then
            if EvaluateActionCondition(ACallTemplateOptions.CallTemplateLoop.BreakCondition, EvaluateReplacements) then
              Break;

          CheckManualStopCondition;
          if FStopAllActionsOnDemand <> nil then
            if FStopAllActionsOnDemand^ then
              Break;
        end;
      end;

      ldAuto:
      begin
        TempACallTemplateOptions := ACallTemplateOptions;
        if StartValue < StopValue then
          TempACallTemplateOptions.CallTemplateLoop.Direction := ldInc
        else
          TempACallTemplateOptions.CallTemplateLoop.Direction := ldDec;

        Result := ExecuteLoopedCallTemplateAction(TempACallTemplateOptions, IsDebugging, AShouldStopAtBreakPoint)
      end;

      else
      begin
        Result := False;
        SetActionVarValue('$ExecAction_Err$', 'Unknown loop direction');
      end;
    end;
  end;
end;


function TActionExecution.ExecuteSleepAction(var ASleepOptions: TClkSleepOptions; var AActionOptions: TClkActionOptions): Boolean;
var
  ValueInt: Integer;
  tk1, tk2, ElapsedTime, RemainingTime: Int64;
begin
  //eventually, implement some fast-forward option, to skip the sleep action while debugging
  ValueInt := StrToIntDef(EvaluateReplacements(ASleepOptions.Value), -1);

  if ValueInt < 0 then
  begin
    Result := False;
    SetActionVarValue('$ExecAction_Err$', 'Invalid sleep value: ' + EvaluateReplacements(ASleepOptions.Value));
    Exit;
  end;

  Result := True;

  if ValueInt < 1 then
    Exit;

  DoOnSetEditorSleepProgressBarMax(ValueInt);

  tk1 := GetTickCount64;
  ElapsedTime := 0;
  repeat
    tk2 := GetTickCount64;

    if tk2 < tk1 then  // In case of a wrap around, call this function again. It will get it right, at the cost of an additional delay.
    begin
      ExecuteSleepAction(ASleepOptions, AActionOptions);
      Break;
    end;

    if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) and (GetAsyncKeyState(VK_F2) < 0) then
    begin
      if FStopAllActionsOnDemandFromParent <> nil then
        FStopAllActionsOnDemandFromParent^ := True;

      if FStopAllActionsOnDemand <> nil then
        FStopAllActionsOnDemand^ := True;
    end;

    if FStopAllActionsOnDemandFromParent <> nil then
      if FStopAllActionsOnDemandFromParent^ then
        if FStopAllActionsOnDemand <> nil then
          FStopAllActionsOnDemand^ := True;

    if FStopAllActionsOnDemand^ then
    begin
      PrependErrorMessageToActionVar('Stopped by user at "' + AActionOptions.ActionName + '" in ' + FTemplateFileName^ + '  ');
      Result := False;
      Break;
    end;

    ElapsedTime := tk2 - tk1;
    RemainingTime := ValueInt - ElapsedTime;

    DoOnSetEditorSleepInfo('Elapsed Time [ms]: ' + IntToStr(ElapsedTime), 'Remaining Time [ms]: ' + IntToStr(RemainingTime));
    DoOnSetEditorSleepProgressBarPosition(ElapsedTime);

    Application.ProcessMessages;
    Sleep(1);
  until ElapsedTime >= ValueInt;
end;


function TActionExecution.ExecuteSetVarAction(var ASetVarOptions: TClkSetVarOptions): Boolean;
var
  TempListOfSetVarNames: TStringList;
  TempListOfSetVarValues: TStringList;
  TempListOfSetVarEvalBefore: TStringList;
  i, j: Integer;
  VarName, VarValue: string;
  ListOfSelfHandles: TStringList;
begin
  Result := False;
  TempListOfSetVarNames := TStringList.Create;
  TempListOfSetVarValues := TStringList.Create;
  TempListOfSetVarEvalBefore := TStringList.Create;
  try
    TempListOfSetVarNames.Text := ASetVarOptions.ListOfVarNames;
    TempListOfSetVarValues.Text := ASetVarOptions.ListOfVarValues;
    TempListOfSetVarEvalBefore.Text := ASetVarOptions.ListOfVarEvalBefore;

    if (TempListOfSetVarNames.Count = 1) and (TempListOfSetVarValues.Count = 0) then //the only variable, passed here, is set to ''
      TempListOfSetVarValues.Add('');

    if TempListOfSetVarNames.Count <> TempListOfSetVarValues.Count then
    begin
      SetActionVarValue('$ExecAction_Err$', 'SetVar: The list of var names has a different length than the list of var values.');
      Exit;
    end;

    if TempListOfSetVarEvalBefore.Count <> TempListOfSetVarNames.Count then
    begin
      SetActionVarValue('$ExecAction_Err$', 'SetVar: The list of var eval infos has a different length than the list of var names.');
      Exit;
    end;

    for i := 0 to TempListOfSetVarNames.Count - 1 do
    begin
      VarName := TempListOfSetVarNames.Strings[i];
      VarValue := TempListOfSetVarValues.Strings[i];

      if (Pos('$Exit(', VarName) = 1) and (VarName[Length(VarName)] = '$') and (VarName[Length(VarName) - 1] = ')') then
      begin
        VarValue := Copy(VarName, Pos('(', VarName) + 1, MaxInt);
        VarValue := Copy(VarValue, 1, Length(VarValue) - 2);
        SetActionVarValue('$ExecAction_Err$', 'Terminating template execution on request.');
        SetActionVarValue('ExitCode', VarValue);

        Result := False;
        Exit;
      end;

      if VarName = '$GetSelfHandles()$' then
      begin
        ListOfSelfHandles := TStringList.Create;
        try
          DoOnGetSelfHandles(ListOfSelfHandles);

          for j := 0 to ListOfSelfHandles.Count - 1 do
            SetActionVarValue('$' + ListOfSelfHandles.Names[j] + '$', ListOfSelfHandles.ValueFromIndex[j]);
        finally
          ListOfSelfHandles.Free;
        end;

        Continue; //use this to prevent adding '$GetSelfHandles()$' as a variable
      end;

      if TempListOfSetVarEvalBefore.Strings[i] = '1' then
        VarValue := EvaluateReplacements(VarValue);

      VarValue := EvaluateHTTP(VarValue);
      SetActionVarValue(VarName, VarValue);
    end;
  finally
    TempListOfSetVarNames.Free;
    TempListOfSetVarValues.Free;
    TempListOfSetVarEvalBefore.Free;
  end;

  Result := True;
end;


function TActionExecution.ExecuteWindowOperationsAction(var AWindowOperationsOptions: TClkWindowOperationsOptions): Boolean;
var
  Hw: THandle;
  Flags: DWord;
  X, Y, cx, cy: LongInt;
begin
  Result := False;
  Hw := StrToIntDef(EvaluateReplacements('$Control_Handle$'), 0);
  if Hw = 0 then
  begin
    SetActionVarValue('$ExecAction_Err$', 'Cannot execute window operations on invalid handle.');
    Exit;
  end;

  case AWindowOperationsOptions.Operation of
    woBringToFront:
    begin
      SetForegroundWindow(Hw);
      Result := True;
    end;

    woMoveResize:
    begin
      Flags := SWP_ASYNCWINDOWPOS or SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOZORDER;

      if AWindowOperationsOptions.NewPositionEabled then
      begin
        X := StrToIntDef(EvaluateReplacements(AWindowOperationsOptions.NewX), 0);
        Y := StrToIntDef(EvaluateReplacements(AWindowOperationsOptions.NewY), 0);
      end
      else
        Flags := Flags or SWP_NOMOVE;

      if AWindowOperationsOptions.NewSizeEabled then
      begin
        cx := StrToIntDef(EvaluateReplacements(AWindowOperationsOptions.NewWidth), 0);
        cy := StrToIntDef(EvaluateReplacements(AWindowOperationsOptions.NewHeight), 0);
      end
      else
        Flags := Flags or SWP_NOSIZE;

      Result := SetWindowPos(Hw, HWND_TOP, X, Y, cx, cy, Flags);

      if not Result then
        SetActionVarValue('$ExecAction_Err$', SysErrorMessage(GetLastError));
    end;

    woClose:
    begin
      SendMessage(Hw, WM_CLOSE, 0, 0);
      Result := True;
    end;

    else
    begin
      Result := False;
    end;
  end;
end;


function TActionExecution.ExecuteClickActionAsString(AListOfClickOptionsParams: TStrings): Boolean;
var
  ClickOptions: TClkClickOptions;
  Temp_XClickPointReference: Integer;
  Temp_YClickPointReference: Integer;
  Temp_MouseButton: Integer;
  Temp_XClickPointReferenceDest: Integer;
  Temp_YClickPointReferenceDest: Integer;
  Temp_ClickType: Integer;
  Temp_Count: Int64;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    Temp_XClickPointReference := StrToIntDef(AListOfClickOptionsParams.Values['XClickPointReference'], 0);
    if (Temp_XClickPointReference < 0) or (Temp_XClickPointReference > Ord(High(TXClickPointReference))) then
    begin
      SetActionVarValue('$ExecAction_Err$', 'XClickPointReference is out of range.');
      Exit;
    end;

    Temp_YClickPointReference := StrToIntDef(AListOfClickOptionsParams.Values['YClickPointReference'], 0);
    if (Temp_YClickPointReference < 0) or (Temp_YClickPointReference > Ord(High(TYClickPointReference))) then
    begin
      SetActionVarValue('$ExecAction_Err$', 'YClickPointReference is out of range.');
      Exit;
    end;

    Temp_MouseButton := StrToIntDef(AListOfClickOptionsParams.Values['MouseButton'], 0);
    if (Temp_MouseButton < 0) or (Temp_MouseButton > Ord(High(TMouseButton))) then
    begin
      SetActionVarValue('$ExecAction_Err$', 'MouseButton is out of range.');
      Exit;
    end;

    Temp_XClickPointReferenceDest := StrToIntDef(AListOfClickOptionsParams.Values['XClickPointReferenceDest'], 0);
    if (Temp_XClickPointReferenceDest < 0) or (Temp_XClickPointReferenceDest > Ord(High(TXClickPointReference))) then
    begin
      SetActionVarValue('$ExecAction_Err$', 'XClickPointReferenceDest is out of range.');
      Exit;
    end;

    Temp_YClickPointReferenceDest := StrToIntDef(AListOfClickOptionsParams.Values['YClickPointReferenceDest'], 0);
    if (Temp_YClickPointReferenceDest < 0) or (Temp_YClickPointReferenceDest > Ord(High(TYClickPointReference))) then
    begin
      SetActionVarValue('$ExecAction_Err$', 'YClickPointReferenceDest is out of range.');
      Exit;
    end;

    Temp_ClickType := StrToIntDef(AListOfClickOptionsParams.Values['ClickType'], CClickType_Click);
    if (Temp_ClickType < 0) or (Temp_ClickType > 3) then
    begin
      SetActionVarValue('$ExecAction_Err$', 'ClickType is out of range.');
      Exit;
    end;

    Temp_Count := StrToInt64Def(AListOfClickOptionsParams.Values['Count'], 1);
    if (Temp_Count < 1) or (Temp_Count > Int64(MaxLongint)) then
    begin
      SetActionVarValue('$ExecAction_Err$', 'Count is out of range.');
      Exit;
    end;

    ClickOptions.XClickPointReference := TXClickPointReference(Temp_XClickPointReference);
    ClickOptions.YClickPointReference := TYClickPointReference(Temp_YClickPointReference);
    ClickOptions.XClickPointVar := AListOfClickOptionsParams.Values['XClickPointVar'];
    ClickOptions.YClickPointVar := AListOfClickOptionsParams.Values['YClickPointVar'];
    ClickOptions.XOffset := AListOfClickOptionsParams.Values['XOffset'];
    ClickOptions.YOffset := AListOfClickOptionsParams.Values['YOffset'];
    ClickOptions.MouseButton := TMouseButton(Temp_MouseButton);
    ClickOptions.ClickWithCtrl := AListOfClickOptionsParams.Values['ClickWithCtrl'] = '1';
    ClickOptions.ClickWithAlt := AListOfClickOptionsParams.Values['ClickWithAlt'] = '1';
    ClickOptions.ClickWithShift := AListOfClickOptionsParams.Values['ClickWithShift'] = '1';
    ClickOptions.ClickWithDoubleClick := AListOfClickOptionsParams.Values['ClickWithDoubleClick'] = '1';
    ClickOptions.Count := Temp_Count;
    ClickOptions.LeaveMouse := AListOfClickOptionsParams.Values['LeaveMouse'] = '1';
    ClickOptions.MoveWithoutClick := AListOfClickOptionsParams.Values['MoveWithoutClick'] = '1';
    ClickOptions.ClickType := Temp_ClickType;    //see CClickType_Click and CClickType_Drag
    ClickOptions.XClickPointReferenceDest := TXClickPointReference(Temp_XClickPointReferenceDest);
    ClickOptions.YClickPointReferenceDest := TYClickPointReference(Temp_YClickPointReferenceDest);
    ClickOptions.XClickPointVarDest := AListOfClickOptionsParams.Values['XClickPointVarDest'];
    ClickOptions.YClickPointVarDest := AListOfClickOptionsParams.Values['YClickPointVarDest'];
    ClickOptions.XOffsetDest := AListOfClickOptionsParams.Values['XOffsetDest'];
    ClickOptions.YOffsetDest := AListOfClickOptionsParams.Values['YOffsetDest'];

    Result := ExecuteMultiClickAction(ClickOptions);
  finally
    SetLastActionStatus(Result, False);
  end;
end;


function TActionExecution.ExecuteExecAppActionAsString(AListOfExecAppOptionsParams: TStrings): Boolean;
var
  ExecAppOptions: TClkExecAppOptions;
  ActionOptions: TClkActionOptions;
  Temp_UseInheritHandles: Integer;
  Temp_ActionTimeout: Int64;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    Temp_UseInheritHandles := StrToIntDef(AListOfExecAppOptionsParams.Values['UseInheritHandles'], 0);
    if (Temp_UseInheritHandles < 0) or (Temp_UseInheritHandles > Ord(High(TExecAppUseInheritHandles))) then
    begin
      SetActionVarValue('$ExecAction_Err$', 'UseInheritHandles is out of range.');
      Exit;
    end;

    Temp_ActionTimeout := StrToIntDef(AListOfExecAppOptionsParams.Values['ActionTimeout'], 1000);
    if (Temp_ActionTimeout < 0) or (Temp_ActionTimeout > 2147483647) then
    begin
      SetActionVarValue('$ExecAction_Err$', 'ActionTimeout is out of range.');
      Exit;
    end;

    ExecAppOptions.PathToApp := AListOfExecAppOptionsParams.Values['PathToApp'];
    ExecAppOptions.ListOfParams := FastReplace_45ToReturn(AListOfExecAppOptionsParams.Values['ListOfParams']);
    ExecAppOptions.WaitForApp := AListOfExecAppOptionsParams.Values['WaitForApp'] = '1';
    ExecAppOptions.AppStdIn := AListOfExecAppOptionsParams.Values['AppStdIn'];
    ExecAppOptions.CurrentDir := AListOfExecAppOptionsParams.Values['CurrentDir'];
    ExecAppOptions.UseInheritHandles := TExecAppUseInheritHandles(Temp_UseInheritHandles);
    ExecAppOptions.NoConsole := AListOfExecAppOptionsParams.Values['NoConsole'] = '1';

    ActionOptions.ActionName := AListOfExecAppOptionsParams.Values['ActionName'];
    ActionOptions.ActionTimeout := Temp_ActionTimeout;
    ActionOptions.Action := acExecApp;

    Result := ExecuteExecAppAction(ExecAppOptions, ActionOptions);
  finally
    SetLastActionStatus(Result, False);
  end;
end;


function TActionExecution.ExecuteFindControlActionAsString(AListOfFindControlOptionsParams: TStrings; AIsSubControl: Boolean): Boolean;
const
  CActionType: array[Boolean] of TClkAction = (acFindControl, acFindSubControl);
var
  FindControlOptions: TClkFindControlOptions;
  ActionOptions: TClkActionOptions;
  Temp_SearchForControlMode: Integer;
  Temp_MatchBitmapTextCount: Integer;
  Temp_MatchBitmapAlgorithm: Integer;
  Temp_ActionTimeout: Int64;
  Temp_FontSize: Integer;
  Temp_FontQuality: Integer;
  Temp_CropLeft, Temp_CropTop, Temp_CropRight, Temp_CropBottom: string;
  i: Integer;
  Prefix: string;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    Temp_SearchForControlMode := StrToIntDef(AListOfFindControlOptionsParams.Values['MatchCriteria.SearchForControlMode'], 0);
    if (Temp_SearchForControlMode < 0) or (Temp_SearchForControlMode > Ord(High(TSearchForControlMode))) then
    begin
      SetActionVarValue('$ExecAction_Err$', 'MatchCriteria.SearchForControlMode is out of range.');
      Exit;
    end;

    Temp_MatchBitmapTextCount := StrToIntDef(AListOfFindControlOptionsParams.Values['MatchBitmapText.Count'], 0);
    if (Temp_MatchBitmapTextCount < 0) or (Temp_MatchBitmapTextCount > 100) then
    begin
      SetActionVarValue('$ExecAction_Err$', 'MatchBitmapText.Count is out of range.');
      Exit;
    end;

    Temp_MatchBitmapAlgorithm := StrToIntDef(AListOfFindControlOptionsParams.Values['MatchBitmapAlgorithm'], 0);
    if (Temp_MatchBitmapAlgorithm < 0) or (Temp_MatchBitmapAlgorithm > Ord(High(TMatchBitmapAlgorithm))) then
    begin
      SetActionVarValue('$ExecAction_Err$', 'MatchBitmapAlgorithm is out of range.');
      Exit;
    end;

    Temp_ActionTimeout := StrToIntDef(AListOfFindControlOptionsParams.Values['ActionTimeout'], 1000);
    if (Temp_ActionTimeout < 0) or (Temp_ActionTimeout > 2147483647) then
    begin
      SetActionVarValue('$ExecAction_Err$', 'ActionTimeout is out of range.');
      Exit;
    end;

    FindControlOptions.MatchCriteria.SearchForControlMode := TSearchForControlMode(Temp_SearchForControlMode);
    FindControlOptions.MatchCriteria.WillMatchText := AListOfFindControlOptionsParams.Values['MatchCriteria.WillMatchText'] = '1';
    FindControlOptions.MatchCriteria.WillMatchClassName := AListOfFindControlOptionsParams.Values['MatchCriteria.WillMatchClassName'] = '1';
    FindControlOptions.MatchCriteria.WillMatchBitmapText := AListOfFindControlOptionsParams.Values['MatchCriteria.WillMatchBitmapText'] = '1';
    FindControlOptions.MatchCriteria.WillMatchBitmapFiles := AListOfFindControlOptionsParams.Values['MatchCriteria.WillMatchBitmapFiles'] = '1';

    FindControlOptions.AllowToFail := AListOfFindControlOptionsParams.Values['AllowToFail'] = '1';
    FindControlOptions.MatchText := AListOfFindControlOptionsParams.Values['MatchText'];
    FindControlOptions.MatchClassName := AListOfFindControlOptionsParams.Values['MatchClassName'];
    FindControlOptions.MatchTextSeparator := AListOfFindControlOptionsParams.Values['MatchTextSeparator'];
    FindControlOptions.MatchClassNameSeparator := AListOfFindControlOptionsParams.Values['MatchClassNameSeparator'];
    SetLength(FindControlOptions.MatchBitmapText, Temp_MatchBitmapTextCount);

    for i := 0 to Temp_MatchBitmapTextCount - 1 do
    begin
      Prefix := 'MatchBitmapText[' + IntToStr(i) + '].';

      Temp_FontSize := StrToIntDef(AListOfFindControlOptionsParams.Values[Prefix + 'FontSize'], 8);
      if (Temp_FontSize < 2) or (Temp_FontSize > 200) then
      begin
        SetActionVarValue('$ExecAction_Err$', Prefix + 'FontSize is out of range.');
        Exit;
      end;

      Temp_FontQuality := StrToIntDef(AListOfFindControlOptionsParams.Values[Prefix + 'FontQuality'], 0);
      if (Temp_FontQuality < 0) or (Temp_FontQuality > Ord(High(TFontQuality))) then
      begin
        SetActionVarValue('$ExecAction_Err$', Prefix + 'FontQuality is out of range.');
        Exit;
      end;

      Temp_CropLeft := AListOfFindControlOptionsParams.Values[Prefix + 'CropLeft'];
      Temp_CropTop := AListOfFindControlOptionsParams.Values[Prefix + 'CropTop'];
      Temp_CropRight := AListOfFindControlOptionsParams.Values[Prefix + 'CropRight'];
      Temp_CropBottom := AListOfFindControlOptionsParams.Values[Prefix + 'CropBottom'];

      if StrToIntDef(Temp_CropLeft, 0) < 0 then
      begin
        SetActionVarValue('$ExecAction_Err$', Prefix + 'CropLeft is out of range.');
        Exit;
      end;

      if StrToIntDef(Temp_CropTop, 0) < 0 then
      begin
        SetActionVarValue('$ExecAction_Err$', Prefix + 'CropTop is out of range.');
        Exit;
      end;

      if StrToIntDef(Temp_CropRight, 0) < 0 then
      begin
        SetActionVarValue('$ExecAction_Err$', Prefix + 'CropRight is out of range.');
        Exit;
      end;

      if StrToIntDef(Temp_CropBottom, 0) < 0 then
      begin
        SetActionVarValue('$ExecAction_Err$', Prefix + 'CropBottom is out of range.');
        Exit;
      end;

      FindControlOptions.MatchBitmapText[i].ForegroundColor := AListOfFindControlOptionsParams.Values[Prefix + 'ForegroundColor'];
      FindControlOptions.MatchBitmapText[i].BackgroundColor := AListOfFindControlOptionsParams.Values[Prefix + 'BackgroundColor'];
      FindControlOptions.MatchBitmapText[i].FontName := AListOfFindControlOptionsParams.Values[Prefix + 'FontName'];
      FindControlOptions.MatchBitmapText[i].FontSize := Temp_FontSize;
      FindControlOptions.MatchBitmapText[i].Bold := AListOfFindControlOptionsParams.Values[Prefix + 'Bold'] = '1';
      FindControlOptions.MatchBitmapText[i].Italic := AListOfFindControlOptionsParams.Values[Prefix + 'Italic'] = '1';
      FindControlOptions.MatchBitmapText[i].Underline := AListOfFindControlOptionsParams.Values[Prefix + 'Underline'] = '1';
      FindControlOptions.MatchBitmapText[i].StrikeOut := AListOfFindControlOptionsParams.Values[Prefix + 'StrikeOut'] = '1';
      FindControlOptions.MatchBitmapText[i].FontQuality := TFontQuality(Temp_FontQuality);
      FindControlOptions.MatchBitmapText[i].FontQualityUsesReplacement := AListOfFindControlOptionsParams.Values[Prefix + 'FontQualityUsesReplacement'] = '1';
      FindControlOptions.MatchBitmapText[i].FontQualityReplacement := AListOfFindControlOptionsParams.Values[Prefix + 'FontQualityReplacement'];
      FindControlOptions.MatchBitmapText[i].ProfileName := AListOfFindControlOptionsParams.Values[Prefix + 'ProfileName'];
      FindControlOptions.MatchBitmapText[i].CropLeft := Temp_CropLeft;
      FindControlOptions.MatchBitmapText[i].CropTop := Temp_CropTop;
      FindControlOptions.MatchBitmapText[i].CropRight := Temp_CropRight;
      FindControlOptions.MatchBitmapText[i].CropBottom := Temp_CropBottom;
    end;

    FindControlOptions.MatchBitmapFiles := FastReplace_45ToReturn(AListOfFindControlOptionsParams.Values['MatchBitmapFiles']); //ListOfStrings
    FindControlOptions.MatchBitmapAlgorithm := TMatchBitmapAlgorithm(Temp_MatchBitmapAlgorithm);

    FindControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf := StrToIntDef(AListOfFindControlOptionsParams.Values['MatchBitmapAlgorithmSettings.XMultipleOf'], 0);
    FindControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf := StrToIntDef(AListOfFindControlOptionsParams.Values['MatchBitmapAlgorithmSettings.YMultipleOf'], 0);
    FindControlOptions.MatchBitmapAlgorithmSettings.XOffset := StrToIntDef(AListOfFindControlOptionsParams.Values['MatchBitmapAlgorithmSettings.XOffset'], 0);
    FindControlOptions.MatchBitmapAlgorithmSettings.YOffset := StrToIntDef(AListOfFindControlOptionsParams.Values['MatchBitmapAlgorithmSettings.YOffset'], 0);

    FindControlOptions.InitialRectange.Left := AListOfFindControlOptionsParams.Values['InitialRectange.Left'];
    FindControlOptions.InitialRectange.Top := AListOfFindControlOptionsParams.Values['InitialRectange.Top'];
    FindControlOptions.InitialRectange.Right := AListOfFindControlOptionsParams.Values['InitialRectange.Right'];
    FindControlOptions.InitialRectange.Bottom := AListOfFindControlOptionsParams.Values['InitialRectange.Bottom'];
    FindControlOptions.InitialRectange.LeftOffset := AListOfFindControlOptionsParams.Values['InitialRectange.LeftOffset'];
    FindControlOptions.InitialRectange.TopOffset := AListOfFindControlOptionsParams.Values['InitialRectange.TopOffset'];
    FindControlOptions.InitialRectange.RightOffset := AListOfFindControlOptionsParams.Values['InitialRectange.RightOffset'];
    FindControlOptions.InitialRectange.BottomOffset := AListOfFindControlOptionsParams.Values['InitialRectange.BottomOffset'];

    FindControlOptions.UseWholeScreen := AListOfFindControlOptionsParams.Values['UseWholeScreen'] = '1';
    FindControlOptions.ColorError := AListOfFindControlOptionsParams.Values['ColorError'];  //string, to allow var replacements
    FindControlOptions.AllowedColorErrorCount := AListOfFindControlOptionsParams.Values['AllowedColorErrorCount'];  //Number of pixels allowed to mismatch
    FindControlOptions.WaitForControlToGoAway := AListOfFindControlOptionsParams.Values['WaitForControlToGoAway'] = '1';
    FindControlOptions.StartSearchingWithCachedControl := AListOfFindControlOptionsParams.Values['StartSearchingWithCachedControl'] = '1';
    FindControlOptions.CachedControlLeft := AListOfFindControlOptionsParams.Values['CachedControlLeft'];
    FindControlOptions.CachedControlTop := AListOfFindControlOptionsParams.Values['CachedControlTop'];

    ActionOptions.ActionName := AListOfFindControlOptionsParams.Values['ActionName'];
    ActionOptions.ActionTimeout := Temp_ActionTimeout;
    ActionOptions.Action := CActionType[AIsSubControl];

    Result := ExecuteFindControlActionWithTimeout(FindControlOptions, ActionOptions, AIsSubControl);
  finally
    SetLastActionStatus(Result, FindControlOptions.AllowToFail);
  end;
end;


function TActionExecution.ExecuteSetControlTextActionAsString(AListOfSetControlTextOptionsParams: TStrings): Boolean;
var
  SetTextOptions: TClkSetTextOptions;
  Temp_ControlType: Integer;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    Temp_ControlType := StrToIntDef(AListOfSetControlTextOptionsParams.Values['ControlType'], 0);
    if (Temp_ControlType < 0) or (Temp_ControlType > Ord(High(TClkSetTextControlType))) then
    begin
      SetActionVarValue('$ExecAction_Err$', 'ControlType is out of range.');
      Exit;
    end;

    SetTextOptions.Text := AListOfSetControlTextOptionsParams.Values['Text'];
    SetTextOptions.ControlType := TClkSetTextControlType(Temp_ControlType);

    Result := ExecuteSetControlTextAction(SetTextOptions);
  finally
    SetLastActionStatus(Result, False);
  end;
end;


function TActionExecution.ExecuteCallTemplateActionAsString(AListOfCallTemplateOptionsParams: TStrings): Boolean;
var
  CallTemplateOptions: TClkCallTemplateOptions;
  Temp_LoopDirection: Integer;
  Temp_LoopEvalBreakPosition: Integer;
  IsDebugging: Boolean;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    Temp_LoopDirection := StrToIntDef(AListOfCallTemplateOptionsParams.Values['Loop.Direction'], 0);
    if (Temp_LoopDirection < 0) or (Temp_LoopDirection > Ord(High(TLoopDirection))) then
    begin
      SetActionVarValue('$ExecAction_Err$', 'Loop.Direction is out of range.');
      Exit;
    end;

    Temp_LoopEvalBreakPosition := StrToIntDef(AListOfCallTemplateOptionsParams.Values['Loop.EvalBreakPosition'], 0);
    if (Temp_LoopEvalBreakPosition < 0) or (Temp_LoopEvalBreakPosition > Ord(High(TLoopEvalBreakPosition))) then
    begin
      SetActionVarValue('$ExecAction_Err$', 'Loop.EvalBreakPosition is out of range.');
      Exit;
    end;

    CallTemplateOptions.TemplateFileName := AListOfCallTemplateOptionsParams.Values['TemplateFileName'];
    CallTemplateOptions.ListOfCustomVarsAndValues := AListOfCallTemplateOptionsParams.Values['ListOfCustomVarsAndValues'];
    CallTemplateOptions.CallOnlyIfCondition := False;       //deprecated - must be set to False, to prevent error messages
    CallTemplateOptions.CallOnlyIfConditionVarName := '';   //deprecated
    CallTemplateOptions.CallOnlyIfConditionVarValue := '';  //deprecated
    CallTemplateOptions.EvaluateBeforeCalling := AListOfCallTemplateOptionsParams.Values['EvaluateBeforeCalling'] = '1';

    CallTemplateOptions.CallTemplateLoop.Enabled := AListOfCallTemplateOptionsParams.Values['Loop.Enabled'] = '1'; //When False, the CallTemplate action is executed once, as before. Else, it may be executed or not, based on loop settings.
    CallTemplateOptions.CallTemplateLoop.Counter := AListOfCallTemplateOptionsParams.Values['Loop.Counter'];
    CallTemplateOptions.CallTemplateLoop.InitValue := AListOfCallTemplateOptionsParams.Values['Loop.InitValue'];
    CallTemplateOptions.CallTemplateLoop.EndValue := AListOfCallTemplateOptionsParams.Values['Loop.EndValue'];
    CallTemplateOptions.CallTemplateLoop.Direction := TLoopDirection(Temp_LoopDirection);
    CallTemplateOptions.CallTemplateLoop.BreakCondition := FastReplace_45ToReturn(AListOfCallTemplateOptionsParams.Values['Loop.BreakCondition']); //uses the same format as TClkActionOptions.ActionCondition
    CallTemplateOptions.CallTemplateLoop.EvalBreakPosition := TLoopEvalBreakPosition(Temp_LoopEvalBreakPosition);

    IsDebugging := AListOfCallTemplateOptionsParams.Values['IsDebugging'] = '1';
    Result := ExecuteLoopedCallTemplateAction(CallTemplateOptions, IsDebugging, IsDebugging); //not sure if AShouldStopAtBreakPoint should be the same as IsDebugging or if it should be another http param

    if not Result then
      FLog.Lines.Add(DateTimeToStr(Now) + '  /ExecuteCallTemplateAction is False. $ExecAction_Err$: ' + EvaluateReplacements('$ExecAction_Err$'))
    else
      FLog.Lines.Add(DateTimeToStr(Now) + '  /ExecuteCallTemplateAction is True.');
  finally
    //SetLastActionStatus(Result, False);  //leave the action status as set by the called template
  end;
end;


function TActionExecution.ExecuteSleepActionAsString(AListOfSleepOptionsParams: TStrings): Boolean;
var
  SleepOptions: TClkSleepOptions;
  ActionOptions: TClkActionOptions;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    SleepOptions.Value := AListOfSleepOptionsParams.Values['Value'];

    ActionOptions.ActionName := AListOfSleepOptionsParams.Values['ActionName'];
    ActionOptions.ActionTimeout := 0;
    ActionOptions.Action := acSleep;

    Result := ExecuteSleepAction(SleepOptions, ActionOptions);
  finally
    SetLastActionStatus(Result, False);
  end;
end;


function TActionExecution.ExecuteSetVarActionAsString(AListOfSetVarOptionsParams: TStrings): Boolean;
var
  SetVarOptions: TClkSetVarOptions;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    SetVarOptions.ListOfVarNames := FastReplace_45ToReturn(AListOfSetVarOptionsParams.Values['ListOfVarNames']);
    SetVarOptions.ListOfVarValues := FastReplace_45ToReturn(AListOfSetVarOptionsParams.Values['ListOfVarValues']);
    SetVarOptions.ListOfVarEvalBefore := FastReplace_45ToReturn(AListOfSetVarOptionsParams.Values['ListOfVarEvalBefore']);

    Result := ExecuteSetVarAction(SetVarOptions);
  finally
    SetLastActionStatus(Result, False);
  end;
end;


function TActionExecution.ExecuteWindowOperationsActionAsString(AListOfWindowOperationsOptionsParams: TStrings): Boolean;
var
  WindowOperationsOptions: TClkWindowOperationsOptions;
  Temp_Operation: Integer;
begin
  Result := False;
  SetActionVarValue('$ExecAction_Err$', '');
  try
    Temp_Operation := StrToIntDef(AListOfWindowOperationsOptionsParams.Values['Operation'], 0);
    if (Temp_Operation < 0) or (Temp_Operation > Ord(High(TWindowOperation))) then
    begin
      SetActionVarValue('$ExecAction_Err$', 'Operation is out of range.');
      Exit;
    end;

    WindowOperationsOptions.Operation := TWindowOperation(Temp_Operation);
    WindowOperationsOptions.NewX := AListOfWindowOperationsOptionsParams.Values['NewX'];
    WindowOperationsOptions.NewY := AListOfWindowOperationsOptionsParams.Values['NewY'];
    WindowOperationsOptions.NewWidth := AListOfWindowOperationsOptionsParams.Values['NewWidth'];
    WindowOperationsOptions.NewHeight := AListOfWindowOperationsOptionsParams.Values['NewHeight'];
    WindowOperationsOptions.NewPositionEabled := AListOfWindowOperationsOptionsParams.Values['NewPositionEabled'] = '1';
    WindowOperationsOptions.NewSizeEabled := AListOfWindowOperationsOptionsParams.Values['NewSizeEabled'] = '1';

    Result := ExecuteWindowOperationsAction(WindowOperationsOptions);
  finally
    SetLastActionStatus(Result, False);
  end;
end;


end.

