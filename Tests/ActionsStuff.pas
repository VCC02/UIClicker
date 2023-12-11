{
    Copyright (C) 2022 VCC
    creation date: Aug 2022
    initial release date: 14 Aug 2022

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


unit ActionsStuff;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ClickerUtils, InMemFileSystem;


function AddClickActionToTemplate(ATemplateFileName: string;

                                  AActionName: string;
                                  AActionTimeout: LongInt; //ms
                                  AActionEnabled: Boolean;
                                  AActionCondition: string;

                                  AClickOptions: TClkClickOptions;
                                  AInMemFS: TInMemFileSystem
                                  ): LongInt;


function AddCallTemplateActionToTemplate(ATemplateFileName: string;

                                         AActionName: string;
                                         AActionTimeout: LongInt; //ms
                                         AActionEnabled: Boolean;
                                         AActionCondition: string;

                                         ACallTemplateOptions: TClkCallTemplateOptions;
                                         AInMemFS: TInMemFileSystem
                                         ): LongInt;


function AddSleepActionToTemplate(ATemplateFileName: string;

                                  AActionName: string;
                                  AActionTimeout: LongInt; //ms
                                  AActionEnabled: Boolean;
                                  AActionCondition: string;

                                  ASleepOptions: TClkSleepOptions;
                                  AInMemFS: TInMemFileSystem
                                  ): LongInt;


function AddSetVarActionToTemplate(ATemplateFileName: string;

                                   AActionName: string;
                                   AActionTimeout: LongInt; //ms
                                   AActionEnabled: Boolean;
                                   AActionCondition: string;

                                   ASetVarOptions: TClkSetVarOptions;
                                   AInMemFS: TInMemFileSystem
                                   ): LongInt;

procedure GetDefaultClickOptions(var AClickOptions: TClkClickOptions);
procedure GenerateClickOptionsForLeaveMouse(X, Y: Integer; var AClickOptions: TClkClickOptions);
procedure GenerateClickOptionsForMouseWheel(AWheelType: TMouseWheelType; AAmount: Integer; var AClickOptions: TClkClickOptions);
procedure GenerateExecAppOptionsForIPConfig(var AExecAppOptions: TClkExecAppOptions);
procedure GenerateFindControlOptionsForMainUIClickerWindow(var AFindControlOptions: TClkFindControlOptions; AAllowToFail: Boolean; ACustomFormCaption: string = 'UI Clicker Main');
procedure GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness(var AFindControlOptions: TClkFindControlOptions; AAllowToFail: Boolean);
procedure GenerateFindSubControlOptionsForMainUIClickerWindow_WinInterpBtn(var AFindControlOptions: TClkFindControlOptions; AAllowToFail: Boolean);
procedure GenerateFindSubControlOptionsForMainUIClickerWindow_PmtvPreviewBtn(var AFindControlOptions: TClkFindControlOptions; AAllowToFail: Boolean);
procedure GenerateSetControlTextOptions(var ASetTextOptions: TClkSetTextOptions; AText: string; AControlType: TClkSetTextControlType);
procedure GenerateCallTemplateOptions(var ACallTemplateOptions: TClkCallTemplateOptions; ATemplateFileName, AListOfVarsAndValues: string; AEvalBefore: Boolean);
procedure GenerateSleepOptions(var ASleepOptions: TClkSleepOptions; AValue: string);
procedure GenerateSetVarOptions_OneVar(var ASetVarOptions: TClkSetVarOptions; AVar, AValue: string; AEvalBefore: Boolean = False);
procedure GenerateWindowOperationsOptionsForFindControlSetup(var AWindowOperationsOptions: TClkWindowOperationsOptions; AOperation: TWindowOperation);
procedure GenerateWindowOperationsOptionsForMouseWheelSetup(var AWindowOperationsOptions: TClkWindowOperationsOptions; AOperation: TWindowOperation);


implementation


uses
  Controls, ClickerTemplates, Graphics;


procedure AddActionToTemplate(ATemplateFileName: string; AClkAction: TClkActionRec; AInMemFS: TInMemFileSystem);
var
  ClkActions: TClkActionsRecArr;
  MemStream: TMemoryStream;
  n: Integer;
  DummyNotes, DummyTemplateIconPath: string;
begin
  if not AInMemFS.FileExistsInMem(ATemplateFileName) then
    AInMemFS.SaveFileToMem(ATemplateFileName, nil, 0);

  MemStream := TMemoryStream.Create;
  try
    AInMemFS.LoadFileFromMemToStream(ATemplateFileName, MemStream);
    MemStream.Position := 0;

    GetTemplateContentFromMemoryStream(ClkActions, DummyNotes, DummyTemplateIconPath, MemStream);

    n := Length(ClkActions);
    SetLength(ClkActions, n + 1);
    CopyActionContent(AClkAction, ClkActions[n]);

    MemStream.Clear;
    GetTemplateContentAsMemoryStream(ClkActions, DummyNotes, DummyTemplateIconPath, MemStream);
    AInMemFS.SaveFileToMem(ATemplateFileName, MemStream.Memory, MemStream.Size);
  finally
    MemStream.Free;
  end;
end;


procedure SetBasicActionOptions(ATemplateFileName: string;
                                AActionName: string;
                                AAction: TClkAction;
                                AActionTimeout: LongInt; //ms
                                AActionEnabled: Boolean;
                                AActionCondition: string;
                                var ATempAction: TClkActionRec);
begin
  ATempAction.ActionOptions.ActionName := AActionName;
  ATempAction.ActionOptions.Action := AAction;
  ATempAction.ActionOptions.ActionTimeout := AActionTimeout;
  ATempAction.ActionOptions.ActionEnabled := AActionEnabled;
  ATempAction.ActionOptions.ActionCondition := AActionCondition;
end;


function AddClickActionToTemplate(ATemplateFileName: string;

                                  AActionName: string;
                                  AActionTimeout: LongInt; //ms
                                  AActionEnabled: Boolean;
                                  AActionCondition: string;

                                  AClickOptions: TClkClickOptions;
                                  AInMemFS: TInMemFileSystem
                                  ): LongInt;
var
  TempAction: TClkActionRec;
begin
  SetBasicActionOptions(ATemplateFileName, AActionName, acClick, AActionTimeout, AActionEnabled, AActionCondition, TempAction);

  //click stuff
  TempAction.ClickOptions := AClickOptions;

  AddActionToTemplate(ATemplateFileName, TempAction, AInMemFS);
  Result := 0;
end;


function AddCallTemplateActionToTemplate(ATemplateFileName: string;

                                         AActionName: string;
                                         AActionTimeout: LongInt; //ms
                                         AActionEnabled: Boolean;
                                         AActionCondition: string;

                                         ACallTemplateOptions: TClkCallTemplateOptions;
                                         AInMemFS: TInMemFileSystem
                                         ): LongInt;
var
  TempAction: TClkActionRec;
begin
  SetBasicActionOptions(ATemplateFileName, AActionName, acCallTemplate, AActionTimeout, AActionEnabled, AActionCondition, TempAction);

  //CallTemplate stuff
  TempAction.CallTemplateOptions := ACallTemplateOptions;

  AddActionToTemplate(ATemplateFileName, TempAction, AInMemFS);
  Result := 0;
end;


function AddSleepActionToTemplate(ATemplateFileName: string;

                                  AActionName: string;
                                  AActionTimeout: LongInt; //ms
                                  AActionEnabled: Boolean;
                                  AActionCondition: string;

                                  ASleepOptions: TClkSleepOptions;
                                  AInMemFS: TInMemFileSystem
                                  ): LongInt;
var
  TempAction: TClkActionRec;
begin
  SetBasicActionOptions(ATemplateFileName, AActionName, acSleep, AActionTimeout, AActionEnabled, AActionCondition, TempAction);

  //Sleep stuff
  TempAction.SleepOptions := ASleepOptions;

  AddActionToTemplate(ATemplateFileName, TempAction, AInMemFS);
  Result := 0;
end;


function AddSetVarActionToTemplate(ATemplateFileName: string;

                                   AActionName: string;
                                   AActionTimeout: LongInt; //ms
                                   AActionEnabled: Boolean;
                                   AActionCondition: string;

                                   ASetVarOptions: TClkSetVarOptions;
                                   AInMemFS: TInMemFileSystem
                                   ): LongInt;
var
  TempAction: TClkActionRec;
begin
  SetBasicActionOptions(ATemplateFileName, AActionName, acSetVar, AActionTimeout, AActionEnabled, AActionCondition, TempAction);

  //click stuff
  TempAction.SetVarOptions := ASetVarOptions;

  AddActionToTemplate(ATemplateFileName, TempAction, AInMemFS);
  Result := 0;
end;


procedure GetDefaultClickOptions(var AClickOptions: TClkClickOptions);
begin
  AClickOptions.XClickPointReference := xrefLeft;
  AClickOptions.YClickPointReference := yrefTop;
  AClickOptions.XClickPointVar := '$Control_Left$';
  AClickOptions.YClickPointVar := '$Control_Top$';
  AClickOptions.XOffset := '';
  AClickOptions.YOffset := '';
  AClickOptions.MouseButton := mbLeft;
  AClickOptions.ClickWithCtrl := False;
  AClickOptions.ClickWithAlt := False;
  AClickOptions.ClickWithShift := False;
  AClickOptions.ClickWithDoubleClick := False;
  AClickOptions.Count := 1;
  AClickOptions.LeaveMouse := False;
  AClickOptions.MoveWithoutClick := False;
  AClickOptions.ClickType := CClickType_Click;    //see CClickType_Click and CClickType_Drag
  AClickOptions.XClickPointReferenceDest := xrefLeft;
  AClickOptions.YClickPointReferenceDest := yrefTop;
  AClickOptions.XClickPointVarDest := '$Control_Left$';
  AClickOptions.YClickPointVarDest := '$Control_Top$';
  AClickOptions.XOffsetDest := '';
  AClickOptions.YOffsetDest := '';
end;


procedure GenerateClickOptionsForLeaveMouse(X, Y: Integer; var AClickOptions: TClkClickOptions);
begin
  AClickOptions.XClickPointReference := xrefAbsolute;
  AClickOptions.YClickPointReference := yrefAbsolute;
  AClickOptions.XOffset := IntToStr(X);
  AClickOptions.YOffset := IntToStr(Y);
  AClickOptions.MouseButton := mbLeft;
  AClickOptions.Count := 1;
  AClickOptions.LeaveMouse := True;
  AClickOptions.MoveWithoutClick := True;
  AClickOptions.ClickType := CClickType_Click;
  AClickOptions.XClickPointReferenceDest := xrefAbsolute;
  AClickOptions.YClickPointReferenceDest := yrefAbsolute;
  AClickOptions.MouseWheelType := mwtVert;
  AClickOptions.MouseWheelAmount := '0';
end;


procedure GenerateClickOptionsForMouseWheel(AWheelType: TMouseWheelType; AAmount: Integer; var AClickOptions: TClkClickOptions);
begin
  AClickOptions.XClickPointReference := xrefAbsolute;
  AClickOptions.YClickPointReference := yrefAbsolute;
  AClickOptions.XOffset := '0';
  AClickOptions.YOffset := '0';
  AClickOptions.MouseButton := mbLeft;
  AClickOptions.Count := 1;
  AClickOptions.LeaveMouse := True;
  AClickOptions.MoveWithoutClick := True;
  AClickOptions.ClickType := CClickType_Wheel;
  AClickOptions.XClickPointReferenceDest := xrefAbsolute;
  AClickOptions.YClickPointReferenceDest := yrefAbsolute;
  AClickOptions.MouseWheelType := AWheelType;
  AClickOptions.MouseWheelAmount := IntToStr(AAmount);
end;


procedure GenerateExecAppOptionsForIPConfig(var AExecAppOptions: TClkExecAppOptions);
begin
  AExecAppOptions.PathToApp := 'C:\Windows\System32\ipconfig.exe';
  AExecAppOptions.ListOfParams := '/all';
  AExecAppOptions.CurrentDir := 'C:\Windows';
  AExecAppOptions.UseInheritHandles := uihNo;
  AExecAppOptions.WaitForApp := True;
  AExecAppOptions.NoConsole := True;
end;


procedure GenerateFindControlOptionsForMainUIClickerWindow(var AFindControlOptions: TClkFindControlOptions; AAllowToFail: Boolean; ACustomFormCaption: string = 'UI Clicker Main');
begin
  AFindControlOptions.MatchCriteria.SearchForControlMode := sfcmGenGrid;
  AFindControlOptions.MatchCriteria.WillMatchText := True;
  AFindControlOptions.MatchCriteria.WillMatchClassName := True;
  AFindControlOptions.MatchCriteria.WillMatchBitmapText := False;
  AFindControlOptions.MatchCriteria.WillMatchBitmapFiles := False;
  AFindControlOptions.MatchCriteria.WillMatchPrimitiveFiles := False;
  AFindControlOptions.MatchText := ACustomFormCaption;
  AFindControlOptions.MatchClassName := 'Window';
  AFindControlOptions.UseWholeScreen := True;
  AFindControlOptions.AllowToFail := AAllowToFail;
  AFindControlOptions.MatchBitmapAlgorithm := mbaBruteForce;
  SetLength(AFindControlOptions.MatchBitmapText, 0);

  AFindControlOptions.WaitForControlToGoAway := False;
  AFindControlOptions.CachedControlLeft := '';
  AFindControlOptions.CachedControlTop := '';
  AFindControlOptions.StartSearchingWithCachedControl := False;
end;


procedure GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness(var AFindControlOptions: TClkFindControlOptions; AAllowToFail: Boolean);
begin
  AFindControlOptions.MatchCriteria.SearchForControlMode := sfcmGenGrid;
  AFindControlOptions.MatchCriteria.WillMatchText := False;
  AFindControlOptions.MatchCriteria.WillMatchClassName := False;
  AFindControlOptions.MatchCriteria.WillMatchBitmapText := True;
  AFindControlOptions.MatchCriteria.WillMatchBitmapFiles := False;
  AFindControlOptions.MatchCriteria.WillMatchPrimitiveFiles := False;
  AFindControlOptions.MatchText := '-bit';  //can be 32-bit or 64-bit, so match both
  AFindControlOptions.UseWholeScreen := False;
  AFindControlOptions.AllowToFail := AAllowToFail;
  AFindControlOptions.MatchBitmapAlgorithm := mbaBruteForce;

  SetLength(AFindControlOptions.MatchBitmapText, 1);
  AFindControlOptions.MatchBitmapText[0].ForegroundColor := '000000';
  AFindControlOptions.MatchBitmapText[0].BackgroundColor := '$Color_BtnFace$';
  AFindControlOptions.MatchBitmapText[0].FontName := 'Segoe UI';
  AFindControlOptions.MatchBitmapText[0].FontSize := 9;
  AFindControlOptions.MatchBitmapText[0].FontQuality := fqDefault;
  AFindControlOptions.MatchBitmapText[0].ProfileName := 'First';
  AFindControlOptions.MatchBitmapText[0].Bold := False;
  AFindControlOptions.MatchBitmapText[0].Italic := False;
  AFindControlOptions.MatchBitmapText[0].Underline := False;
  AFindControlOptions.MatchBitmapText[0].StrikeOut := False;
  AFindControlOptions.MatchBitmapText[0].CropLeft := '';
  AFindControlOptions.MatchBitmapText[0].CropTop := '';
  AFindControlOptions.MatchBitmapText[0].CropRight := '';
  AFindControlOptions.MatchBitmapText[0].CropBottom := '';

  AFindControlOptions.InitialRectangle.Left := '$Control_Left$';
  AFindControlOptions.InitialRectangle.Top := '$Control_Top$';
  AFindControlOptions.InitialRectangle.Right := '$Control_Right$';
  AFindControlOptions.InitialRectangle.Bottom := '$Control_Bottom$';
  AFindControlOptions.InitialRectangle.LeftOffset := '0';
  AFindControlOptions.InitialRectangle.TopOffset := '220';
  AFindControlOptions.InitialRectangle.RightOffset := '-250';
  AFindControlOptions.InitialRectangle.BottomOffset := '0';
  AFindControlOptions.ColorError := '10';
  AFindControlOptions.AllowedColorErrorCount := '40';

  AFindControlOptions.WaitForControlToGoAway := False;
  AFindControlOptions.CachedControlLeft := '';
  AFindControlOptions.CachedControlTop := '';
  AFindControlOptions.StartSearchingWithCachedControl := False;

  AFindControlOptions.MatchBitmapAlgorithmSettings.XOffset := 0;
  AFindControlOptions.MatchBitmapAlgorithmSettings.YOffset := 0;
  AFindControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf := 0;
  AFindControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf := 0;
end;


procedure GenerateFindSubControlOptionsForMainUIClickerWindow_WinInterpBtn(var AFindControlOptions: TClkFindControlOptions; AAllowToFail: Boolean);
const
  CBmpDirAsSrvDisk = 'py\bmps\';
begin
  AFindControlOptions.MatchCriteria.SearchForControlMode := sfcmGenGrid;
  AFindControlOptions.MatchCriteria.WillMatchText := False;
  AFindControlOptions.MatchCriteria.WillMatchClassName := False;
  AFindControlOptions.MatchCriteria.WillMatchBitmapText := False;
  AFindControlOptions.MatchCriteria.WillMatchBitmapFiles := True;
  AFindControlOptions.MatchCriteria.WillMatchPrimitiveFiles := False;
  AFindControlOptions.UseWholeScreen := False;
  AFindControlOptions.AllowToFail := AAllowToFail;
  AFindControlOptions.MatchBitmapAlgorithm := mbaBruteForce;

  SetLength(AFindControlOptions.MatchBitmapText, 0);

  AFindControlOptions.MatchBitmapFiles := CBmpDirAsSrvDisk + 'ShowActionsWindow_Focused.bmp' + #13#10 +
                                          CBmpDirAsSrvDisk + 'ShowActionsWindow_FocusedHighlighted.bmp' + #13#10 +
                                          CBmpDirAsSrvDisk + 'ShowActionsWindow_Unfocused.bmp';

  AFindControlOptions.InitialRectangle.Left := '$Control_Left$';
  AFindControlOptions.InitialRectangle.Top := '$Control_Top$';
  AFindControlOptions.InitialRectangle.Right := '$Control_Right$';
  AFindControlOptions.InitialRectangle.Bottom := '$Control_Bottom$';
  AFindControlOptions.InitialRectangle.LeftOffset := '61';
  AFindControlOptions.InitialRectangle.TopOffset := '90';
  AFindControlOptions.InitialRectangle.RightOffset := '-73';
  AFindControlOptions.InitialRectangle.BottomOffset := '-120';
  AFindControlOptions.ColorError := '25';                    //rendering depends on video card, so allow a high error value
  AFindControlOptions.AllowedColorErrorCount := '400';

  AFindControlOptions.WaitForControlToGoAway := False;
  AFindControlOptions.CachedControlLeft := '';
  AFindControlOptions.CachedControlTop := '';
  AFindControlOptions.StartSearchingWithCachedControl := False;
end;


procedure GenerateFindSubControlOptionsForMainUIClickerWindow_PmtvPreviewBtn(var AFindControlOptions: TClkFindControlOptions; AAllowToFail: Boolean);
const
  CBmpDirAsSrvDisk = 'py\bmps\';
begin
  AFindControlOptions.MatchCriteria.SearchForControlMode := sfcmGenGrid;
  AFindControlOptions.MatchCriteria.WillMatchText := False;
  AFindControlOptions.MatchCriteria.WillMatchClassName := False;
  AFindControlOptions.MatchCriteria.WillMatchBitmapText := False;
  AFindControlOptions.MatchCriteria.WillMatchBitmapFiles := False;
  AFindControlOptions.MatchCriteria.WillMatchPrimitiveFiles := True;
  AFindControlOptions.UseWholeScreen := False;
  AFindControlOptions.AllowToFail := AAllowToFail;
  AFindControlOptions.MatchBitmapAlgorithm := mbaBruteForce;

  SetLength(AFindControlOptions.MatchBitmapText, 0);

  AFindControlOptions.MatchBitmapFiles := '';
  AFindControlOptions.MatchPrimitiveFiles := CBmpDirAsSrvDisk + 'PreviewButtonIcon.pmtv' + #13#10 +
                                             CBmpDirAsSrvDisk + 'PreviewButtonIcon64.pmtv';

  AFindControlOptions.InitialRectangle.Left := '$Control_Left$';
  AFindControlOptions.InitialRectangle.Top := '$Control_Top$';
  AFindControlOptions.InitialRectangle.Right := '$Control_Right$';
  AFindControlOptions.InitialRectangle.Bottom := '$Control_Bottom$';
  AFindControlOptions.InitialRectangle.LeftOffset := '76';
  AFindControlOptions.InitialRectangle.TopOffset := '60';
  AFindControlOptions.InitialRectangle.RightOffset := '-217';
  AFindControlOptions.InitialRectangle.BottomOffset := '-185';
  AFindControlOptions.ColorError := '0';
  AFindControlOptions.AllowedColorErrorCount := '0';

  AFindControlOptions.WaitForControlToGoAway := False;
  AFindControlOptions.CachedControlLeft := '';
  AFindControlOptions.CachedControlTop := '';
  AFindControlOptions.StartSearchingWithCachedControl := False;
end;


procedure GenerateSetControlTextOptions(var ASetTextOptions: TClkSetTextOptions; AText: string; AControlType: TClkSetTextControlType);
begin
  ASetTextOptions.Text := AText;
  ASetTextOptions.ControlType := AControlType;
end;


procedure GenerateCallTemplateOptions(var ACallTemplateOptions: TClkCallTemplateOptions; ATemplateFileName, AListOfVarsAndValues: string; AEvalBefore: Boolean);
begin
  ACallTemplateOptions.TemplateFileName := ATemplateFileName;
  ACallTemplateOptions.ListOfCustomVarsAndValues := AListOfVarsAndValues;
  ACallTemplateOptions.EvaluateBeforeCalling := AEvalBefore;
  ACallTemplateOptions.CallOnlyIfCondition := False;

  ACallTemplateOptions.CallTemplateLoop.Enabled := False;
  ACallTemplateOptions.CallTemplateLoop.Direction := ldInc;
  ACallTemplateOptions.CallTemplateLoop.EvalBreakPosition := lebpAfterContent;
end;


procedure GenerateSleepOptions(var ASleepOptions: TClkSleepOptions; AValue: string);
begin
  ASleepOptions.Value := AValue;
end;


procedure GenerateSetVarOptions_OneVar(var ASetVarOptions: TClkSetVarOptions; AVar, AValue: string; AEvalBefore: Boolean = False);
begin
  ASetVarOptions.ListOfVarNames := AVar;
  ASetVarOptions.ListOfVarValues := AValue;
  ASetVarOptions.ListOfVarEvalBefore := IntToStr(Ord(AEvalBefore));
end;


procedure GenerateWindowOperationsOptionsForFindControlSetup(var AWindowOperationsOptions: TClkWindowOperationsOptions; AOperation: TWindowOperation);
begin
  AWindowOperationsOptions.Operation := AOperation;
  AWindowOperationsOptions.NewX := '90';
  AWindowOperationsOptions.NewY := '90';
  AWindowOperationsOptions.NewWidth := '336';
  AWindowOperationsOptions.NewHeight := '279';
  AWindowOperationsOptions.NewPositionEnabled := False; //should be enabled, only if the window can go offscreen
  AWindowOperationsOptions.NewSizeEnabled := True;
end;


procedure GenerateWindowOperationsOptionsForMouseWheelSetup(var AWindowOperationsOptions: TClkWindowOperationsOptions; AOperation: TWindowOperation);
begin
  AWindowOperationsOptions.Operation := AOperation;
  AWindowOperationsOptions.NewX := '';
  AWindowOperationsOptions.NewY := '';
  AWindowOperationsOptions.NewWidth := '';
  AWindowOperationsOptions.NewHeight := '';
  AWindowOperationsOptions.NewPositionEnabled := False;
  AWindowOperationsOptions.NewSizeEnabled := False;
end;

end.

