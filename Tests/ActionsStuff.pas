{
    Copyright (C) 2024 VCC
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


function AddExecAppActionToTemplate(ATemplateFileName: string;

                                    AActionName: string;
                                    AActionTimeout: LongInt; //ms
                                    AActionEnabled: Boolean;
                                    AActionCondition: string;

                                    AExecAppOptions: TClkExecAppOptions;
                                    AInMemFS: TInMemFileSystem
                                    ): LongInt;


function AddFindControlActionToTemplate(ATemplateFileName: string;

                                        AActionName: string;
                                        AActionTimeout: LongInt; //ms
                                        AActionEnabled: Boolean;
                                        AActionCondition: string;

                                        AFindControlOptions: TClkFindControlOptions;
                                        AInMemFS: TInMemFileSystem;
                                        AIsSubControl: Boolean = False
                                        ): LongInt;


function AddSetControlTextActionToTemplate(ATemplateFileName: string;

                                           AActionName: string;
                                           AActionTimeout: LongInt; //ms
                                           AActionEnabled: Boolean;
                                           AActionCondition: string;

                                           ASetControlTextOptions: TClkSetTextOptions;
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


function AddWindowOperationsActionToTemplate(ATemplateFileName: string;

                                             AActionName: string;
                                             AActionTimeout: LongInt; //ms
                                             AActionEnabled: Boolean;
                                             AActionCondition: string;

                                             AWindowOperationsOptions: TClkWindowOperationsOptions;
                                             AInMemFS: TInMemFileSystem
                                             ): LongInt;


function AddLoadSetVarFromFileActionToTemplate(ATemplateFileName: string;

                                               AActionName: string;
                                               AActionTimeout: LongInt; //ms
                                               AActionEnabled: Boolean;
                                               AActionCondition: string;

                                               ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions;
                                               AInMemFS: TInMemFileSystem
                                               ): LongInt;

function AddSaveSetVarToFileActionToTemplate(ATemplateFileName: string;

                                             AActionName: string;
                                             AActionTimeout: LongInt; //ms
                                             AActionEnabled: Boolean;
                                             AActionCondition: string;

                                             ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions;
                                             AInMemFS: TInMemFileSystem
                                             ): LongInt;


function AddPluginActionToTemplate(ATemplateFileName: string;

                                   AActionName: string;
                                   AActionTimeout: LongInt; //ms
                                   AActionEnabled: Boolean;
                                   AActionCondition: string;

                                   APluginOptions: TClkPluginOptions;
                                   AInMemFS: TInMemFileSystem
                                   ): LongInt;


function AddEditTemplateActionToTemplate(ATemplateFileName: string;

                                         AActionName: string;
                                         AActionTimeout: LongInt; //ms
                                         AActionEnabled: Boolean;
                                         AActionCondition: string;

                                         AEditTemplateOptions: TClkEditTemplateOptions;
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
procedure GenerateFindSubControlOptionsForExtRenderingText(var AFindControlOptions: TClkFindControlOptions; AAllowToFail: Boolean; ASourceFileName: string);
procedure GenerateFindSubControlOptionsForLoadedBackgroundBmp(var AFindControlOptions: TClkFindControlOptions; AAllowToFail: Boolean; ASourceFileName: string);
procedure GenerateFindSubControlOptionsForFullScreenshot(var AFindControlOptions: TClkFindControlOptions; AAllowToFail: Boolean);

procedure GenerateSetControlTextOptions(var ASetTextOptions: TClkSetTextOptions; AText: string; AControlType: TClkSetTextControlType);
procedure GenerateCallTemplateOptions(var ACallTemplateOptions: TClkCallTemplateOptions; ATemplateFileName, AListOfVarsAndValues: string; AEvalBefore: Boolean);
procedure GenerateSleepOptions(var ASleepOptions: TClkSleepOptions; AValue: string);
procedure GenerateSetVarOptions_OneVar(var ASetVarOptions: TClkSetVarOptions; AVar, AValue: string; AEvalBefore: Boolean = False);
procedure GenerateWindowOperationsOptionsForFindControlSetup(var AWindowOperationsOptions: TClkWindowOperationsOptions; AOperation: TWindowOperation);
procedure GenerateWindowOperationsOptionsForMouseWheelSetup(var AWindowOperationsOptions: TClkWindowOperationsOptions; AOperation: TWindowOperation);
procedure GeneratePluginOptions(var APluginOptions: TClkPluginOptions; AFileName, AListOfPropertiesAndValues: string);
procedure GenerateEditTemplateOptions(var AEditTemplateOptions: TClkEditTemplateOptions; AActionType: TClkAction; AOperation: TEditTemplateOperation; AWhichTemplate: TEditTemplateWhichTemplate; ATemplateFileName: string);


procedure GenerateDifferentThanDefault_Click(var AClickOptions: TClkClickOptions);
procedure GenerateDifferentThanDefault_ExecApp(var AExecAppOptions: TClkExecAppOptions);
procedure GenerateDifferentThanDefault_FindControl(var AFindControlOptions: TClkFindControlOptions);
procedure GenerateDifferentThanDefault_FindSubControl(var AFindControlOptions: TClkFindControlOptions);
procedure GenerateDifferentThanDefault_SetControlText(var ASetControlTextOptions: TClkSetTextOptions);
procedure GenerateDifferentThanDefault_CallTemplate(var ACallTemplateOptions: TClkCallTemplateOptions);
procedure GenerateDifferentThanDefault_Sleep(var ASleepOptions: TClkSleepOptions);
procedure GenerateDifferentThanDefault_SetVar(var ASetVarOptions: TClkSetVarOptions);
procedure GenerateDifferentThanDefault_WindowOperations(var AWindowOperationsOptions: TClkWindowOperationsOptions);
procedure GenerateDifferentThanDefault_LoadSetVarFromFile(var ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions);
procedure GenerateDifferentThanDefault_SaveSetVarToFile(var ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions);
procedure GenerateDifferentThanDefault_Plugin(var APluginOptions: TClkPluginOptions);
procedure GenerateDifferentThanDefault_EditTemplate(var AEditTemplateOptions: TClkEditTemplateOptions);


function GenerateDifferentThanDefault_ClickStr: string;
function GenerateDifferentThanDefault_ExecAppStr: string;
function GenerateDifferentThanDefault_FindControlStr: string;
function GenerateDifferentThanDefault_SetControlTextStr: string;
function GenerateDifferentThanDefault_CallTemplateStr: string;
function GenerateDifferentThanDefault_SleepStr: string;
function GenerateDifferentThanDefault_SetVarStr: string;
function GenerateDifferentThanDefault_WindowOperationsStr: string;
function GenerateDifferentThanDefault_LoadSetVarFromFileStr: string;
function GenerateDifferentThanDefault_SaveSetVarToFileStr: string;
function GenerateDifferentThanDefault_PluginStr: string;
function GenerateDifferentThanDefault_EditTemplateStr: string;


implementation


uses
  Controls, ClickerTemplates, ClickerActionProperties, Graphics, Forms;


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

  //Click stuff
  TempAction.ClickOptions := AClickOptions;

  AddActionToTemplate(ATemplateFileName, TempAction, AInMemFS);
  Result := 0;
end;


function AddExecAppActionToTemplate(ATemplateFileName: string;

                                    AActionName: string;
                                    AActionTimeout: LongInt; //ms
                                    AActionEnabled: Boolean;
                                    AActionCondition: string;

                                    AExecAppOptions: TClkExecAppOptions;
                                    AInMemFS: TInMemFileSystem
                                    ): LongInt;
var
  TempAction: TClkActionRec;
begin
  SetBasicActionOptions(ATemplateFileName, AActionName, acExecApp, AActionTimeout, AActionEnabled, AActionCondition, TempAction);

  //ExecApp stuff
  TempAction.ExecAppOptions := AExecAppOptions;

  AddActionToTemplate(ATemplateFileName, TempAction, AInMemFS);
  Result := 0;
end;


function AddFindControlActionToTemplate(ATemplateFileName: string;

                                        AActionName: string;
                                        AActionTimeout: LongInt; //ms
                                        AActionEnabled: Boolean;
                                        AActionCondition: string;

                                        AFindControlOptions: TClkFindControlOptions;
                                        AInMemFS: TInMemFileSystem;
                                        AIsSubControl: Boolean = False
                                        ): LongInt;
var
  TempAction: TClkActionRec;
begin
  if not AIsSubControl then
    SetBasicActionOptions(ATemplateFileName, AActionName, acFindControl, AActionTimeout, AActionEnabled, AActionCondition, TempAction)
  else
    SetBasicActionOptions(ATemplateFileName, AActionName, acFindSubControl, AActionTimeout, AActionEnabled, AActionCondition, TempAction);

  //FindControl stuff
  TempAction.FindControlOptions := AFindControlOptions;

  AddActionToTemplate(ATemplateFileName, TempAction, AInMemFS);
  Result := 0;
end;


function AddSetControlTextActionToTemplate(ATemplateFileName: string;

                                           AActionName: string;
                                           AActionTimeout: LongInt; //ms
                                           AActionEnabled: Boolean;
                                           AActionCondition: string;

                                           ASetControlTextOptions: TClkSetTextOptions;
                                           AInMemFS: TInMemFileSystem
                                           ): LongInt;
var
  TempAction: TClkActionRec;
begin
  SetBasicActionOptions(ATemplateFileName, AActionName, acSetControlText, AActionTimeout, AActionEnabled, AActionCondition, TempAction);

  //SetControlText stuff
  TempAction.SetTextOptions := ASetControlTextOptions;

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

  //SetVar stuff
  TempAction.SetVarOptions := ASetVarOptions;

  AddActionToTemplate(ATemplateFileName, TempAction, AInMemFS);
  Result := 0;
end;


function AddWindowOperationsActionToTemplate(ATemplateFileName: string;

                                             AActionName: string;
                                             AActionTimeout: LongInt; //ms
                                             AActionEnabled: Boolean;
                                             AActionCondition: string;

                                             AWindowOperationsOptions: TClkWindowOperationsOptions;
                                             AInMemFS: TInMemFileSystem
                                             ): LongInt;
var
  TempAction: TClkActionRec;
begin
  SetBasicActionOptions(ATemplateFileName, AActionName, acWindowOperations, AActionTimeout, AActionEnabled, AActionCondition, TempAction);

  //WindowOperations stuff
  TempAction.WindowOperationsOptions := AWindowOperationsOptions;

  AddActionToTemplate(ATemplateFileName, TempAction, AInMemFS);
  Result := 0;
end;


function AddLoadSetVarFromFileActionToTemplate(ATemplateFileName: string;

                                               AActionName: string;
                                               AActionTimeout: LongInt; //ms
                                               AActionEnabled: Boolean;
                                               AActionCondition: string;

                                               ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions;
                                               AInMemFS: TInMemFileSystem
                                               ): LongInt;
var
  TempAction: TClkActionRec;
begin
  SetBasicActionOptions(ATemplateFileName, AActionName, acLoadSetVarFromFile, AActionTimeout, AActionEnabled, AActionCondition, TempAction);

  //LoadSetVarFromFile stuff
  TempAction.LoadSetVarFromFileOptions := ALoadSetVarFromFileOptions;

  AddActionToTemplate(ATemplateFileName, TempAction, AInMemFS);
  Result := 0;
end;


function AddSaveSetVarToFileActionToTemplate(ATemplateFileName: string;

                                             AActionName: string;
                                             AActionTimeout: LongInt; //ms
                                             AActionEnabled: Boolean;
                                             AActionCondition: string;

                                             ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions;
                                             AInMemFS: TInMemFileSystem
                                             ): LongInt;
var
  TempAction: TClkActionRec;
begin
  SetBasicActionOptions(ATemplateFileName, AActionName, acSaveSetVarToFile, AActionTimeout, AActionEnabled, AActionCondition, TempAction);

  //SaveSetVarToFile stuff
  TempAction.SaveSetVarToFileOptions := ASaveSetVarToFileOptions;

  AddActionToTemplate(ATemplateFileName, TempAction, AInMemFS);
  Result := 0;
end;


function AddPluginActionToTemplate(ATemplateFileName: string;

                                   AActionName: string;
                                   AActionTimeout: LongInt; //ms
                                   AActionEnabled: Boolean;
                                   AActionCondition: string;

                                   APluginOptions: TClkPluginOptions;
                                   AInMemFS: TInMemFileSystem
                                   ): LongInt;
var
  TempAction: TClkActionRec;
begin
  SetBasicActionOptions(ATemplateFileName, AActionName, acPlugin, AActionTimeout, AActionEnabled, AActionCondition, TempAction);

  //Plugin stuff
  TempAction.PluginOptions := APluginOptions;

  AddActionToTemplate(ATemplateFileName, TempAction, AInMemFS);
  Result := 0;
end;


function AddEditTemplateActionToTemplate(ATemplateFileName: string;

                                         AActionName: string;
                                         AActionTimeout: LongInt; //ms
                                         AActionEnabled: Boolean;
                                         AActionCondition: string;

                                         AEditTemplateOptions: TClkEditTemplateOptions;
                                         AInMemFS: TInMemFileSystem
                                         ): LongInt;
var
  TempAction: TClkActionRec;
begin
  SetBasicActionOptions(ATemplateFileName, AActionName, acEditTemplate, AActionTimeout, AActionEnabled, AActionCondition, TempAction);

  //EditTemplate stuff
  TempAction.EditTemplateOptions := AEditTemplateOptions;

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
  AClickOptions.DelayAfterMovingToDestination := '50';
  AClickOptions.DelayAfterMouseDown := '200';
  AClickOptions.MoveDuration := '-1';
  AClickOptions.UseClipCursor := False;
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

  AFindControlOptions.UseFastSearch := True;

  AFindControlOptions.SleepySearch := False;
  AFindControlOptions.StopSearchOnMismatch := True;
  AFindControlOptions.ImageSource := isScreenshot;
  AFindControlOptions.ImageSourceFileNameLocation := isflMem;

  AFindControlOptions.PrecisionTimeout := False;
  AFindControlOptions.FullBackgroundImageInResult := True;
  AFindControlOptions.EvaluateTextCount := '';
  AFindControlOptions.CropFromScreenshot := False;
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
  AFindControlOptions.MatchBitmapText[0].IgnoreBackgroundColor := False;

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

  AFindControlOptions.UseFastSearch := True;

  AFindControlOptions.SleepySearch := False;
  AFindControlOptions.StopSearchOnMismatch := True;
  AFindControlOptions.ImageSource := isScreenshot;
  AFindControlOptions.ImageSourceFileNameLocation := isflMem;

  AFindControlOptions.PrecisionTimeout := False;
  AFindControlOptions.FullBackgroundImageInResult := True;
  AFindControlOptions.EvaluateTextCount := '';
  AFindControlOptions.CropFromScreenshot := False;
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

  AFindControlOptions.UseFastSearch := True;

  AFindControlOptions.SleepySearch := False;
  AFindControlOptions.StopSearchOnMismatch := True;
  AFindControlOptions.ImageSource := isScreenshot;
  AFindControlOptions.ImageSourceFileNameLocation := isflMem;

  AFindControlOptions.PrecisionTimeout := False;
  AFindControlOptions.FullBackgroundImageInResult := True;
  AFindControlOptions.EvaluateTextCount := '';
  AFindControlOptions.CropFromScreenshot := False;
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

  AFindControlOptions.UseFastSearch := True;

  AFindControlOptions.SleepySearch := False;
  AFindControlOptions.StopSearchOnMismatch := True;
  AFindControlOptions.ImageSource := isScreenshot;
  AFindControlOptions.ImageSourceFileNameLocation := isflMem;

  AFindControlOptions.PrecisionTimeout := False;
  AFindControlOptions.FullBackgroundImageInResult := True;
  AFindControlOptions.EvaluateTextCount := '';
  AFindControlOptions.CropFromScreenshot := False;
end;


procedure GenerateFindSubControlOptionsForExtRenderingText(var AFindControlOptions: TClkFindControlOptions; AAllowToFail: Boolean; ASourceFileName: string);
begin
  AFindControlOptions.MatchCriteria.SearchForControlMode := sfcmGenGrid;
  AFindControlOptions.MatchCriteria.WillMatchText := False;
  AFindControlOptions.MatchCriteria.WillMatchClassName := False;
  AFindControlOptions.MatchCriteria.WillMatchBitmapText := True;
  AFindControlOptions.MatchCriteria.WillMatchBitmapFiles := False;
  AFindControlOptions.MatchCriteria.WillMatchPrimitiveFiles := False;
  AFindControlOptions.MatchText := 'This is the searched text.';
  AFindControlOptions.UseWholeScreen := False;
  AFindControlOptions.AllowToFail := AAllowToFail;
  AFindControlOptions.MatchBitmapAlgorithm := mbaBruteForce;

  SetLength(AFindControlOptions.MatchBitmapText, 1);
  AFindControlOptions.MatchBitmapText[0].ForegroundColor := '000000';
  AFindControlOptions.MatchBitmapText[0].BackgroundColor := 'FFFFFF';
  AFindControlOptions.MatchBitmapText[0].FontName := 'Tahoma';
  AFindControlOptions.MatchBitmapText[0].FontSize := 36;
  AFindControlOptions.MatchBitmapText[0].FontQuality := fqNonAntialiased;
  AFindControlOptions.MatchBitmapText[0].ProfileName := 'First';
  AFindControlOptions.MatchBitmapText[0].Bold := False;
  AFindControlOptions.MatchBitmapText[0].Italic := False;
  AFindControlOptions.MatchBitmapText[0].Underline := False;
  AFindControlOptions.MatchBitmapText[0].StrikeOut := False;
  AFindControlOptions.MatchBitmapText[0].CropLeft := '';
  AFindControlOptions.MatchBitmapText[0].CropTop := '';
  AFindControlOptions.MatchBitmapText[0].CropRight := '';
  AFindControlOptions.MatchBitmapText[0].CropBottom := '';
  AFindControlOptions.MatchBitmapText[0].IgnoreBackgroundColor := True;

  AFindControlOptions.InitialRectangle.Left := '0';
  AFindControlOptions.InitialRectangle.Top := '0';
  AFindControlOptions.InitialRectangle.Right := '597';   //it has to be greater than Left
  AFindControlOptions.InitialRectangle.Bottom := '232';  //it has to be greater than Top
  AFindControlOptions.InitialRectangle.LeftOffset := '0';
  AFindControlOptions.InitialRectangle.TopOffset := '0';
  AFindControlOptions.InitialRectangle.RightOffset := '0';
  AFindControlOptions.InitialRectangle.BottomOffset := '0';
  AFindControlOptions.ColorError := '0';
  AFindControlOptions.AllowedColorErrorCount := '0';

  AFindControlOptions.WaitForControlToGoAway := False;
  AFindControlOptions.CachedControlLeft := '';
  AFindControlOptions.CachedControlTop := '';
  AFindControlOptions.StartSearchingWithCachedControl := False;

  AFindControlOptions.MatchBitmapAlgorithmSettings.XOffset := 0;
  AFindControlOptions.MatchBitmapAlgorithmSettings.YOffset := 0;
  AFindControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf := 0;
  AFindControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf := 0;

  AFindControlOptions.UseFastSearch := True;

  AFindControlOptions.SleepySearch := False;
  AFindControlOptions.StopSearchOnMismatch := True;
  AFindControlOptions.ImageSource := isFile;
  AFindControlOptions.SourceFileName := ASourceFileName;
  AFindControlOptions.ImageSourceFileNameLocation := isflMem;

  AFindControlOptions.PrecisionTimeout := False;
  AFindControlOptions.FullBackgroundImageInResult := True;
  AFindControlOptions.EvaluateTextCount := '';
  AFindControlOptions.CropFromScreenshot := False;
end;


procedure GenerateFindSubControlOptionsForLoadedBackgroundBmp(var AFindControlOptions: TClkFindControlOptions; AAllowToFail: Boolean; ASourceFileName: string);
begin
  AFindControlOptions.MatchCriteria.SearchForControlMode := sfcmGenGrid;
  AFindControlOptions.MatchCriteria.WillMatchText := False;
  AFindControlOptions.MatchCriteria.WillMatchClassName := False;
  AFindControlOptions.MatchCriteria.WillMatchBitmapText := True;
  AFindControlOptions.MatchCriteria.WillMatchBitmapFiles := False;
  AFindControlOptions.MatchCriteria.WillMatchPrimitiveFiles := False;
  AFindControlOptions.MatchText := 'Long';
  AFindControlOptions.UseWholeScreen := False;
  AFindControlOptions.AllowToFail := AAllowToFail;
  AFindControlOptions.MatchBitmapAlgorithm := mbaBruteForce;

  SetLength(AFindControlOptions.MatchBitmapText, 1);
  AFindControlOptions.MatchBitmapText[0].ForegroundColor := '00D000';
  AFindControlOptions.MatchBitmapText[0].BackgroundColor := 'F3E7A0';
  AFindControlOptions.MatchBitmapText[0].FontName := 'Tahoma';
  AFindControlOptions.MatchBitmapText[0].FontSize := 15;
  AFindControlOptions.MatchBitmapText[0].FontQuality := fqNonAntialiased;
  AFindControlOptions.MatchBitmapText[0].ProfileName := 'First';
  AFindControlOptions.MatchBitmapText[0].Bold := False;
  AFindControlOptions.MatchBitmapText[0].Italic := False;
  AFindControlOptions.MatchBitmapText[0].Underline := False;
  AFindControlOptions.MatchBitmapText[0].StrikeOut := False;
  AFindControlOptions.MatchBitmapText[0].CropLeft := '';
  AFindControlOptions.MatchBitmapText[0].CropTop := '';
  AFindControlOptions.MatchBitmapText[0].CropRight := '';
  AFindControlOptions.MatchBitmapText[0].CropBottom := '';
  AFindControlOptions.MatchBitmapText[0].IgnoreBackgroundColor := False;

  AFindControlOptions.InitialRectangle.Left := '0';
  AFindControlOptions.InitialRectangle.Top := '0';
  AFindControlOptions.InitialRectangle.Right := '130';
  AFindControlOptions.InitialRectangle.Bottom := '30';
  AFindControlOptions.InitialRectangle.LeftOffset := '0';
  AFindControlOptions.InitialRectangle.TopOffset := '0';
  AFindControlOptions.InitialRectangle.RightOffset := '0';
  AFindControlOptions.InitialRectangle.BottomOffset := '0';
  AFindControlOptions.ColorError := '0';
  AFindControlOptions.AllowedColorErrorCount := '0';

  AFindControlOptions.WaitForControlToGoAway := False;
  AFindControlOptions.CachedControlLeft := '';
  AFindControlOptions.CachedControlTop := '';
  AFindControlOptions.StartSearchingWithCachedControl := False;

  AFindControlOptions.MatchBitmapAlgorithmSettings.XOffset := 0;
  AFindControlOptions.MatchBitmapAlgorithmSettings.YOffset := 0;
  AFindControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf := 0;
  AFindControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf := 0;

  AFindControlOptions.UseFastSearch := True;

  AFindControlOptions.SleepySearch := False;
  AFindControlOptions.StopSearchOnMismatch := True;
  AFindControlOptions.ImageSource := isFile;
  AFindControlOptions.SourceFileName := ASourceFileName;
  AFindControlOptions.ImageSourceFileNameLocation := isflDisk;

  AFindControlOptions.PrecisionTimeout := False;
  AFindControlOptions.FullBackgroundImageInResult := True;
  AFindControlOptions.EvaluateTextCount := '';
  AFindControlOptions.CropFromScreenshot := False;
end;


procedure GenerateFindSubControlOptionsForFullScreenshot(var AFindControlOptions: TClkFindControlOptions; AAllowToFail: Boolean);
begin
  AFindControlOptions.MatchCriteria.SearchForControlMode := sfcmGenGrid;
  AFindControlOptions.MatchCriteria.WillMatchText := False;
  AFindControlOptions.MatchCriteria.WillMatchClassName := False;
  AFindControlOptions.MatchCriteria.WillMatchBitmapText := True;
  AFindControlOptions.MatchCriteria.WillMatchBitmapFiles := False;
  AFindControlOptions.MatchCriteria.WillMatchPrimitiveFiles := False;
  AFindControlOptions.MatchText := 'This is the searched text.';
  AFindControlOptions.UseWholeScreen := False;
  AFindControlOptions.AllowToFail := AAllowToFail;
  AFindControlOptions.MatchBitmapAlgorithm := mbaBruteForce;

  SetLength(AFindControlOptions.MatchBitmapText, 1);
  AFindControlOptions.MatchBitmapText[0].ForegroundColor := '000000';
  AFindControlOptions.MatchBitmapText[0].BackgroundColor := 'FFFFFF';
  AFindControlOptions.MatchBitmapText[0].FontName := 'Tahoma';
  AFindControlOptions.MatchBitmapText[0].FontSize := 36;
  AFindControlOptions.MatchBitmapText[0].FontQuality := fqNonAntialiased;
  AFindControlOptions.MatchBitmapText[0].ProfileName := 'First';
  AFindControlOptions.MatchBitmapText[0].Bold := False;
  AFindControlOptions.MatchBitmapText[0].Italic := False;
  AFindControlOptions.MatchBitmapText[0].Underline := False;
  AFindControlOptions.MatchBitmapText[0].StrikeOut := False;
  AFindControlOptions.MatchBitmapText[0].CropLeft := '';
  AFindControlOptions.MatchBitmapText[0].CropTop := '';
  AFindControlOptions.MatchBitmapText[0].CropRight := '';
  AFindControlOptions.MatchBitmapText[0].CropBottom := '';
  AFindControlOptions.MatchBitmapText[0].IgnoreBackgroundColor := False;  //Yes, False

  AFindControlOptions.InitialRectangle.Left := '0';
  AFindControlOptions.InitialRectangle.Top := '0';
  AFindControlOptions.InitialRectangle.Right := IntToStr(Screen.Width);
  AFindControlOptions.InitialRectangle.Bottom := IntToStr(Screen.Height);
  AFindControlOptions.InitialRectangle.LeftOffset := '0';
  AFindControlOptions.InitialRectangle.TopOffset := '0';
  AFindControlOptions.InitialRectangle.RightOffset := '0';
  AFindControlOptions.InitialRectangle.BottomOffset := '0';
  AFindControlOptions.ColorError := '0';
  AFindControlOptions.AllowedColorErrorCount := '0';

  AFindControlOptions.WaitForControlToGoAway := False;
  AFindControlOptions.CachedControlLeft := '';
  AFindControlOptions.CachedControlTop := '';
  AFindControlOptions.StartSearchingWithCachedControl := False;

  AFindControlOptions.MatchBitmapAlgorithmSettings.XOffset := 0;
  AFindControlOptions.MatchBitmapAlgorithmSettings.YOffset := 0;
  AFindControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf := 0;
  AFindControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf := 0;

  AFindControlOptions.UseFastSearch := True;
  AFindControlOptions.FastSearchAllowedColorErrorCount := '10';

  AFindControlOptions.SleepySearch := False;
  AFindControlOptions.StopSearchOnMismatch := True;
  AFindControlOptions.ImageSource := isScreenshot;
  AFindControlOptions.SourceFileName := '';
  AFindControlOptions.ImageSourceFileNameLocation := isflMem;

  AFindControlOptions.PrecisionTimeout := False;
  AFindControlOptions.FullBackgroundImageInResult := True;
  AFindControlOptions.EvaluateTextCount := '-1';
  AFindControlOptions.CropFromScreenshot := False;
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
  ASetVarOptions.FailOnException := False;
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


procedure GeneratePluginOptions(var APluginOptions: TClkPluginOptions; AFileName, AListOfPropertiesAndValues: string);
begin
  APluginOptions.FileName := AFileName;
  APluginOptions.ListOfPropertiesAndValues := AListOfPropertiesAndValues;
end;


procedure GenerateEditTemplateOptions(var AEditTemplateOptions: TClkEditTemplateOptions; AActionType: TClkAction; AOperation: TEditTemplateOperation; AWhichTemplate: TEditTemplateWhichTemplate; ATemplateFileName: string);
var
  TempAction: TClkActionRec;
begin
  AEditTemplateOptions.Operation := AOperation;
  AEditTemplateOptions.WhichTemplate := AWhichTemplate;
  AEditTemplateOptions.TemplateFileName := ATemplateFileName;
  AEditTemplateOptions.ListOfEditedProperties := '';
  AEditTemplateOptions.ListOfEnabledProperties := '';
  AEditTemplateOptions.EditedActionName := 'Test' + CClkActionStr[AActionType];
  AEditTemplateOptions.EditedActionType := AActionType;
  AEditTemplateOptions.EditedActionCondition := '$FirstVar$==$SecondVar$';
  AEditTemplateOptions.EditedActionTimeout := 3333;
  AEditTemplateOptions.NewActionName :='New_' + AEditTemplateOptions.EditedActionName;

  case AActionType of
    acClick:
    begin
      GetDefaultPropertyValues_Click(TempAction.ClickOptions);

      if AOperation in [etoNewAction, etoUpdateAction] then
      begin
        TempAction.ClickOptions.XClickPointReference := xrefWidth;
        TempAction.ClickOptions.XClickPointVar := '$JustAnotherVar$';
        TempAction.ClickOptions.MoveDuration := '77';
      end
      else
      begin                                                              //use some predefined values, which should be returned in case of error
        TempAction.ClickOptions.XClickPointReference := xrefAbsolute;
        TempAction.ClickOptions.XClickPointVar := '$BadValueFromTest$';
        TempAction.ClickOptions.MoveDuration := 'AnotherBadValueFromTest';
      end;

      AEditTemplateOptions.ListOfEditedProperties := GetClickActionProperties(TempAction.ClickOptions);
      AEditTemplateOptions.ListOfEnabledProperties := 'XClickPointReference' + #13#10 +
                                                      'XClickPointVar' + #13#10 +
                                                      'MoveDuration';
    end;

    acExecApp:
    begin
      GetDefaultPropertyValues_ExecApp(TempAction.ExecAppOptions);

      if AOperation in [etoNewAction, etoUpdateAction] then
      begin
        TempAction.ExecAppOptions.PathToApp := 'path to destination';
        TempAction.ExecAppOptions.AppStdIn := 'something typed into app console';
      end
      else
      begin                                                             //use some predefined values, which should be returned in case of error
        TempAction.ExecAppOptions.PathToApp := 'bad path';
        TempAction.ExecAppOptions.AppStdIn := 'unknown console';
      end;

      AEditTemplateOptions.ListOfEditedProperties := GetExecAppActionProperties(TempAction.ExecAppOptions);
      AEditTemplateOptions.ListOfEnabledProperties := 'PathToApp' + #13#10 +
                                                      'AppStdIn';
    end;

    acFindControl:
    begin
      GetDefaultPropertyValues_FindControl(TempAction.FindControlOptions, False);

      if AOperation in [etoNewAction, etoUpdateAction] then
      begin
        TempAction.FindControlOptions.MatchCriteria.WillMatchText := True;
        TempAction.FindControlOptions.MatchCriteria.WillMatchClassName := True;
        TempAction.FindControlOptions.MatchText := 'text to be matched';
        TempAction.FindControlOptions.MatchClassName := 'some window';
        TempAction.FindControlOptions.UseWholeScreen := True;
      end
      else
      begin                                                             //use some predefined values, which should be returned in case of error
        TempAction.FindControlOptions.MatchCriteria.WillMatchText := False;
        TempAction.FindControlOptions.MatchCriteria.WillMatchClassName := False;
        TempAction.FindControlOptions.MatchText := 'missing text';
        TempAction.FindControlOptions.MatchClassName := 'unknown class';
        TempAction.FindControlOptions.UseWholeScreen := False;
      end;

      AEditTemplateOptions.ListOfEditedProperties := GetFindControlActionProperties(TempAction.FindControlOptions);
      AEditTemplateOptions.ListOfEnabledProperties := 'MatchCriteria.WillMatchText' + #13#10 +
                                                      'MatchCriteria.WillMatchClassName' + #13#10 +
                                                      'MatchText' + #13#10 +
                                                      'MatchClassName' + #13#10 +
                                                      'UseWholeScreen';
    end;

    acFindSubControl:
    begin
      GetDefaultPropertyValues_FindControl(TempAction.FindControlOptions, True);

      SetLength(TempAction.FindControlOptions.MatchBitmapText, 2);
      if AOperation in [etoNewAction, etoUpdateAction] then
      begin
        TempAction.FindControlOptions.MatchCriteria.WillMatchText := False;
        TempAction.FindControlOptions.MatchCriteria.WillMatchClassName := False;
        TempAction.FindControlOptions.MatchCriteria.WillMatchBitmapText := True;
        TempAction.FindControlOptions.MatchText := 'bmp text to be matched';
        TempAction.FindControlOptions.MatchClassName := 'class doesn''t matter';
        TempAction.FindControlOptions.MatchBitmapText[0].ForegroundColor := '$FGCol$';
        TempAction.FindControlOptions.MatchBitmapText[0].BackgroundColor := '$BGCol$';
        TempAction.FindControlOptions.MatchBitmapText[1].FontName := 'Mono';
        TempAction.FindControlOptions.MatchBitmapText[1].FontSize := 19;
        TempAction.FindControlOptions.UseWholeScreen := False;
      end
      else
      begin                                                             //use some predefined values, which should be returned in case of error
        TempAction.FindControlOptions.MatchCriteria.WillMatchText := True;
        TempAction.FindControlOptions.MatchCriteria.WillMatchClassName := True;
        TempAction.FindControlOptions.MatchCriteria.WillMatchBitmapText := False;
        TempAction.FindControlOptions.MatchText := 'missing text';
        TempAction.FindControlOptions.MatchClassName := 'unknown class';
        TempAction.FindControlOptions.MatchBitmapText[0].ForegroundColor := 'Bad$FGCol$';
        TempAction.FindControlOptions.MatchBitmapText[0].BackgroundColor := '$BadBGCol$';
        TempAction.FindControlOptions.MatchBitmapText[1].FontName := 'Stereo';
        TempAction.FindControlOptions.MatchBitmapText[1].FontSize := 320;
        TempAction.FindControlOptions.UseWholeScreen := True;
      end;

      AEditTemplateOptions.ListOfEditedProperties := GetFindControlActionProperties(TempAction.FindControlOptions);
      AEditTemplateOptions.ListOfEnabledProperties := 'MatchCriteria.WillMatchText' + #13#10 +
                                                      'MatchCriteria.WillMatchClassName' + #13#10 +
                                                      'MatchCriteria.WillMatchBitmapText' + #13#10 +
                                                      'MatchText' + #13#10 +
                                                      'MatchClassName' + #13#10 +
                                                      'MatchBitmapText[0].ForegroundColor' + #13#10 +
                                                      'MatchBitmapText[0].BackgroundColor' + #13#10 +
                                                      'MatchBitmapText[1].FontName' + #13#10 +
                                                      'MatchBitmapText[1].FontSize' + #13#10 +
                                                      'UseWholeScreen';
    end;

    acSetControlText:
    begin
      GetDefaultPropertyValues_SetControlText(TempAction.SetTextOptions);

      if AOperation in [etoNewAction, etoUpdateAction] then
      begin
        TempAction.SetTextOptions.Text := 'Type this text.';
        TempAction.SetTextOptions.ControlType := stKeystrokes;
        TempAction.SetTextOptions.Count := '$cnt$';
      end
      else
      begin                                                             //use some predefined values, which should be returned in case of error
        TempAction.SetTextOptions.Text := 'bad typewriter';
        TempAction.SetTextOptions.ControlType := stComboBox;
        TempAction.SetTextOptions.Count := '$var$';
      end;

      AEditTemplateOptions.ListOfEditedProperties := GetSetControlTextActionProperties(TempAction.SetTextOptions);
      AEditTemplateOptions.ListOfEnabledProperties := 'Text' + #13#10 +
                                                      'ControlType' + #13#10 +
                                                      'Count';
    end;

    acCallTemplate:
    begin
      GetDefaultPropertyValues_CallTemplate(TempAction.CallTemplateOptions);

      if AOperation in [etoNewAction, etoUpdateAction] then
      begin
        TempAction.CallTemplateOptions.TemplateFileName := 'Template to be called.';
        TempAction.CallTemplateOptions.ListOfCustomVarsAndValues := '$a$=a' + #13#10 + '$b$=b' + #13#10;        //
        TempAction.CallTemplateOptions.CallTemplateLoop.Counter := '$ii$';
      end
      else
      begin                                                             //use some predefined values, which should be returned in case of error
        TempAction.CallTemplateOptions.TemplateFileName := 'bad typewriter';
        TempAction.CallTemplateOptions.ListOfCustomVarsAndValues := '$c$=d' + #13#10 + '$d$=c' + #13#10;
        TempAction.CallTemplateOptions.CallTemplateLoop.Counter := '$a_var$';
      end;

      AEditTemplateOptions.ListOfEditedProperties := GetCallTemplateActionProperties(TempAction.CallTemplateOptions);
      AEditTemplateOptions.ListOfEnabledProperties := 'TemplateFileName' + #13#10 +
                                                      'ListOfCustomVarsAndValues' + #13#10 +
                                                      'CallTemplateLoop.Counter';
    end;

    acSleep:
    begin
      GetDefaultPropertyValues_Sleep(TempAction.SleepOptions);

      if AOperation in [etoNewAction, etoUpdateAction] then
        TempAction.SleepOptions.Value := '456'
      else
        TempAction.SleepOptions.Value := '789';                         //use some predefined values, which should be returned in case of error

      AEditTemplateOptions.ListOfEditedProperties := GetSleepActionProperties(TempAction.SleepOptions);
      AEditTemplateOptions.ListOfEnabledProperties := 'Value';
    end;

    acSetVar:
    begin
      GetDefaultPropertyValues_SetVar(TempAction.SetVarOptions);

      if AOperation in [etoNewAction, etoUpdateAction] then
      begin
        TempAction.SetVarOptions.ListOfVarNames := '$abc$' + #13#10 + '$def$' + #13#10;        //
        TempAction.SetVarOptions.ListOfVarValues := 'a' + #13#10 + 'b' + #13#10;        //
        TempAction.SetVarOptions.ListOfVarEvalBefore := '1' + #13#10 + '1' + #13#10;        //
        TempAction.SetVarOptions.FailOnException := True;
      end
      else
      begin                                                             //use some predefined values, which should be returned in case of error
        TempAction.SetVarOptions.ListOfVarNames := '$123$' + #13#10 + '$456$' + #13#10;        //
        TempAction.SetVarOptions.ListOfVarValues := 'd' + #13#10 + 'c' + #13#10;        //
        TempAction.SetVarOptions.ListOfVarEvalBefore := '0' + #13#10 + '0' + #13#10;        //
        TempAction.SetVarOptions.FailOnException := False;
      end;

      AEditTemplateOptions.ListOfEditedProperties := GetSetVarActionProperties(TempAction.SetVarOptions);
      AEditTemplateOptions.ListOfEnabledProperties := 'ListOfVarNamesValuesAndEvalBefore' + #13#10 +
                                                      'FailOnException';
    end;

    acWindowOperations:
    begin
      GetDefaultPropertyValues_WindowOperations(TempAction.WindowOperationsOptions);

      if AOperation in [etoNewAction, etoUpdateAction] then
      begin
        TempAction.WindowOperationsOptions.NewX := 'X300';
        TempAction.WindowOperationsOptions.NewY := 'Y400';
      end
      else
      begin                                                             //use some predefined values, which should be returned in case of error
        TempAction.WindowOperationsOptions.NewX := 'OldX';
        TempAction.WindowOperationsOptions.NewY := 'OldY';
      end;

      AEditTemplateOptions.ListOfEditedProperties := GetWindowOperationsActionProperties(TempAction.WindowOperationsOptions);
      AEditTemplateOptions.ListOfEnabledProperties := 'NewX' + #13#10 +
                                                      'NewY';
    end;

    acLoadSetVarFromFile:
    begin
      GetDefaultPropertyValues_LoadSetVarFromFile(TempAction.LoadSetVarFromFileOptions);

      if AOperation in [etoNewAction, etoUpdateAction] then
      begin
        TempAction.LoadSetVarFromFileOptions.FileName := '$PathToFile$';
        TempAction.LoadSetVarFromFileOptions.SetVarActionName := '$Action$';
      end
      else
      begin                                                             //use some predefined values, which should be returned in case of error
        TempAction.LoadSetVarFromFileOptions.FileName := '$NoPath$';
        TempAction.LoadSetVarFromFileOptions.SetVarActionName := 'Empty';
      end;

      AEditTemplateOptions.ListOfEditedProperties := GetLoadSetVarFromFileActionProperties(TempAction.LoadSetVarFromFileOptions);
      AEditTemplateOptions.ListOfEnabledProperties := 'FileName' + #13#10 +
                                                      'SetVarActionName';
    end;

    acSaveSetVarToFile:
    begin
      GetDefaultPropertyValues_SaveSetVarToFile(TempAction.SaveSetVarToFileOptions);

      if AOperation in [etoNewAction, etoUpdateAction] then
      begin
        TempAction.SaveSetVarToFileOptions.FileName := '$PathToAnotherFile$';
        TempAction.SaveSetVarToFileOptions.SetVarActionName := '$AnotherAction$';
      end
      else
      begin                                                             //use some predefined values, which should be returned in case of error
        TempAction.SaveSetVarToFileOptions.FileName := '$NoPath$';
        TempAction.SaveSetVarToFileOptions.SetVarActionName := 'Empty';
      end;

      AEditTemplateOptions.ListOfEditedProperties := GetSaveSetVarToFileActionProperties(TempAction.SaveSetVarToFileOptions);
      AEditTemplateOptions.ListOfEnabledProperties := 'FileName' + #13#10 +
                                                      'SetVarActionName';
    end;

    acPlugin:
    begin
      GetDefaultPropertyValues_Plugin(TempAction.PluginOptions);

      if AOperation in [etoNewAction, etoUpdateAction] then
      begin
        TempAction.PluginOptions.FileName := '$AppDir$\..\UIClickerFindWindowsPlugin\lib\i386-win32\UIClickerFindWindows.dll';
        TempAction.PluginOptions.ListOfPropertiesAndValues := 'FindSubControlTopLeftCorner=30FindSubControlBotLeftCorner=40BorderThickness=7';
      end
      else
      begin                                                             //use some predefined values, which should be returned in case of error
        TempAction.PluginOptions.FileName := 'bad path';
        TempAction.PluginOptions.ListOfPropertiesAndValues := 'FindSubControlTopLeftCorner=K_valFindSubControlBotLeftCorner=L_valBorderThickness=17'//'unknown values';
      end;

      AEditTemplateOptions.ListOfEditedProperties := GetPluginActionProperties(TempAction.PluginOptions);
      //AEditTemplateOptions.ListOfEnabledProperties := 'FileName' + #13#10 +
      //                                                'ListOfPropertiesAndValues';   //this is the wrong way of setting it

      AEditTemplateOptions.ListOfEnabledProperties := 'FileName' + #13#10 +
                                                      'FindSubControlTopLeftCorner' + #13#10 +
                                                      'FindSubControlBotLeftCorner' + #13#10 +
                                                      'BorderThickness';
    end;

    acEditTemplate:
    begin
      GetDefaultPropertyValues_EditTemplate(TempAction.EditTemplateOptions);

      if AOperation in [etoNewAction, etoUpdateAction] then
      begin
        TempAction.EditTemplateOptions.Operation := etoDuplicateAction;
        TempAction.EditTemplateOptions.WhichTemplate := etwtSelf;
        TempAction.EditTemplateOptions.TemplateFileName := '$PathToTemplate$';
        TempAction.EditTemplateOptions.ListOfEditedProperties := 'Property=10';
        TempAction.EditTemplateOptions.ListOfEnabledProperties := 'Property';
        TempAction.EditTemplateOptions.EditedActionName := 'Act';
      end
      else
      begin    //use some predefined values, which should be returned in case of error
        TempAction.EditTemplateOptions.Operation := etoExecuteAction;
        TempAction.EditTemplateOptions.WhichTemplate := etwtOther;
        TempAction.EditTemplateOptions.TemplateFileName := '$NoPath$';
        TempAction.EditTemplateOptions.ListOfEditedProperties := 'Empty';
        TempAction.EditTemplateOptions.ListOfEnabledProperties := 'Empty';
        TempAction.EditTemplateOptions.EditedActionName := 'Unknown';
      end;

      AEditTemplateOptions.ListOfEditedProperties := GetEditTemplateActionProperties(TempAction.EditTemplateOptions);
      AEditTemplateOptions.ListOfEnabledProperties := 'Operation' + #13#10 +
                                                      'WhichTemplate' + #13#10 +
                                                      'TemplateFileName' + #13#10 +
                                                      'EditedActionName';
    end;
  end; //case
end;


procedure GenerateDifferentThanDefault_Click(var AClickOptions: TClkClickOptions);
begin
  GetDefaultPropertyValues_Click(AClickOptions);

  AClickOptions.XClickPointReference := xrefVar;
  AClickOptions.YClickPointReference := yrefAbsolute;
  AClickOptions.XClickPointVar := 'FirstVar';
  AClickOptions.YClickPointVar := 'SecondVar';
  AClickOptions.XOffset := 'FirstOffset';
  AClickOptions.YOffset := 'SecondOffset';
  AClickOptions.MouseButton := mbExtra1;
  AClickOptions.ClickWithCtrl := True;
  AClickOptions.ClickWithAlt := True;
  AClickOptions.ClickWithShift := True;
  AClickOptions.ClickWithDoubleClick := True;
  AClickOptions.Count := 10;
  AClickOptions.LeaveMouse := True;
  AClickOptions.MoveWithoutClick := True;
  AClickOptions.ClickType := CClickType_Wheel;    //see CClickType_Click and CClickType_Drag
  AClickOptions.XClickPointReferenceDest := xrefWidth;
  AClickOptions.YClickPointReferenceDest := yrefHeight;
  AClickOptions.XClickPointVarDest := 'ThirdVar';
  AClickOptions.YClickPointVarDest := 'FourthVar';
  AClickOptions.XOffsetDest := 'ThirdOffset';
  AClickOptions.YOffsetDest := 'FourthOffset';
  AClickOptions.MouseWheelType := mwtHoriz;
  AClickOptions.MouseWheelAmount := '30';
  AClickOptions.DelayAfterMovingToDestination := '40';
  AClickOptions.DelayAfterMouseDown := '50';
  AClickOptions.MoveDuration := '60';
  AClickOptions.UseClipCursor := True;
end;


function GenerateDifferentThanDefault_ClickStr: string;
begin
  Result := 'XClickPointReference' + '=' + 'xrefVar' + '&' +
            'YClickPointReference' + '=' + 'yrefAbsolute' + '&' +
            'XClickPointVar' + '=' + 'FirstVar' + '&' +
            'YClickPointVar' + '=' + 'SecondVar' + '&' +
            'XOffset' + '=' + 'FirstOffset' + '&' +
            'YOffset' + '=' + 'SecondOffset' + '&' +
            'MouseButton' + '=' + 'mbExtra1' + '&' +
            'ClickWithCtrl' + '=' + 'True' + '&' +
            'ClickWithAlt' + '=' + 'True' + '&' +
            'ClickWithShift' + '=' + 'True' + '&' +
            'ClickWithDoubleClick' + '=' + 'True' + '&' +
            'Count' + '=' + '10' + '&' +
            'LeaveMouse' + '=' + 'True' + '&' +
            'MoveWithoutClick' + '=' + 'True' + '&' +
            'ClickType' + '=' + 'Wheel' + '&' +
            'XClickPointReferenceDest' + '=' + 'xrefWidth' + '&' +
            'YClickPointReferenceDest' + '=' + 'yrefHeight' + '&' +
            'XClickPointVarDest' + '=' + 'ThirdVar' + '&' +
            'YClickPointVarDest' + '=' + 'FourthVar' + '&' +
            'XOffsetDest' + '=' + 'ThirdOffset' + '&' +
            'YOffsetDest' + '=' + 'FourthOffset' + '&' +
            'MouseWheelType' + '=' + 'mwtHoriz' + '&' +
            'MouseWheelAmount' + '=' + '30' + '&' +
            'DelayAfterMovingToDestination' + '=' + '40' + '&' +
            'DelayAfterMouseDown' + '=' + '50' + '&' +
            'MoveDuration' + '=' + '60' + '&' +
            'UseClipCursor' + '=' + 'True';
end;


procedure GenerateDifferentThanDefault_ExecApp(var AExecAppOptions: TClkExecAppOptions);
begin
  GetDefaultPropertyValues_ExecApp(AExecAppOptions);

  AExecAppOptions.PathToApp := 'TheNonExistentApp';
  AExecAppOptions.ListOfParams := 'NewParams';
  AExecAppOptions.WaitForApp := True;
  AExecAppOptions.AppStdIn := 'MyStdIn';
  AExecAppOptions.CurrentDir := 'TheDir';
  AExecAppOptions.UseInheritHandles := uihYes;
  AExecAppOptions.NoConsole := True;
end;


function GenerateDifferentThanDefault_ExecAppStr: string;
begin
  Result := 'PathToApp' + '=' + 'TheNonExistentApp' + '&' +
            'ListOfParams' + '=' + 'NewParams' + '&' +
            'WaitForApp' + '=' + 'True' + '&' +
            'AppStdIn' + '=' + 'MyStdIn' + '&' +
            'CurrentDir' + '=' + 'TheDir' + '&' +
            'UseInheritHandles' + '=' + 'uihYes' + '&' +
            'NoConsole' + '=' + 'True';
end;


procedure GenerateDifferentThanDefault_FindControl(var AFindControlOptions: TClkFindControlOptions);
begin
  GetDefaultPropertyValues_FindControl(AFindControlOptions);

  AFindControlOptions.MatchCriteria.WillMatchText := False;
  AFindControlOptions.MatchCriteria.WillMatchClassName := False;
  AFindControlOptions.MatchCriteria.WillMatchBitmapText := True;
  AFindControlOptions.MatchCriteria.WillMatchBitmapFiles := True;
  AFindControlOptions.MatchCriteria.WillMatchPrimitiveFiles := True;
  AFindControlOptions.MatchCriteria.SearchForControlMode := sfcmEnumWindows;
  AFindControlOptions.AllowToFail := True;
  AFindControlOptions.MatchText := 'some text';
  AFindControlOptions.MatchClassName := 'some class';
  AFindControlOptions.MatchTextSeparator := 'txt sep';
  AFindControlOptions.MatchClassNameSeparator := 'class sep';
  AFindControlOptions.MatchBitmapFiles := 'bmp';
  AFindControlOptions.MatchBitmapAlgorithm := mbaRawHistogramZones;
  AFindControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf := 3;
  AFindControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf := 4;
  AFindControlOptions.MatchBitmapAlgorithmSettings.XOffset := 5;
  AFindControlOptions.MatchBitmapAlgorithmSettings.YOffset := 6;
  AFindControlOptions.InitialRectangle.Left := '$Control_L$';
  AFindControlOptions.InitialRectangle.Top := '$Control_T$';
  AFindControlOptions.InitialRectangle.Right := '$Control_R$';
  AFindControlOptions.InitialRectangle.Bottom := '$Control_B$';
  AFindControlOptions.InitialRectangle.LeftOffset := '30';
  AFindControlOptions.InitialRectangle.TopOffset := '40';
  AFindControlOptions.InitialRectangle.RightOffset := '50';
  AFindControlOptions.InitialRectangle.BottomOffset := '60';
  AFindControlOptions.UseWholeScreen := False;
  AFindControlOptions.ColorError := '70';
  AFindControlOptions.AllowedColorErrorCount := '80';
  AFindControlOptions.WaitForControlToGoAway := True;
  AFindControlOptions.StartSearchingWithCachedControl := True;
  AFindControlOptions.CachedControlLeft := '90';
  AFindControlOptions.CachedControlTop := '100';
  AFindControlOptions.MatchPrimitiveFiles := 'pmtv';
  AFindControlOptions.MatchPrimitiveFiles_Modified := '';
  AFindControlOptions.GetAllControls := True;
  AFindControlOptions.UseFastSearch := False;
  AFindControlOptions.FastSearchAllowedColorErrorCount := '700';
  AFindControlOptions.IgnoredColors := '800';
  AFindControlOptions.SleepySearch := True;
  AFindControlOptions.StopSearchOnMismatch := True;
  AFindControlOptions.ImageSource := isFile;
  AFindControlOptions.SourceFileName := 'Fnm';
  AFindControlOptions.ImageSourceFileNameLocation := isflDisk;
  AFindControlOptions.PrecisionTimeout := True;
  AFindControlOptions.FullBackgroundImageInResult := False;
  AFindControlOptions.MatchByHistogramSettings.MinPercentColorMatch := '250';
  AFindControlOptions.MatchByHistogramSettings.MostSignificantColorCountInSubBmp := '310';
  AFindControlOptions.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp := '415';
  AFindControlOptions.EvaluateTextCount := '-17';
  AFindControlOptions.CropFromScreenshot := True;
  AFindControlOptions.ThreadCount := '30';
end;


function GenerateDifferentThanDefault_FindControlStr: string;
begin
  Result := 'MatchCriteria.WillMatchText' + '=' + 'False' + '&' +
            'MatchCriteria.WillMatchClassName' + '=' + 'False' + '&' +
            'MatchCriteria.WillMatchBitmapText' + '=' + 'True' + '&' +
            'MatchCriteria.WillMatchBitmapFiles' + '=' + 'True' + '&' +
            'MatchCriteria.WillMatchPrimitiveFiles' + '=' + 'True' + '&' +
            'MatchCriteria.SearchForControlMode' + '=' + 'sfcmEnumWindows' + '&' +
            'AllowToFail' + '=' + 'True' + '&' +

            'MatchText' + '=' + 'some text' + '&' +
            'MatchClassName' + '=' + 'some class' + '&' +
            'MatchTextSeparator' + '=' + 'txt sep' + '&' +
            'MatchClassNameSeparator' + '=' + 'class sep' + '&' +

            'MatchBitmapText.Count' + '=' + '2' + '&' +
            //GetMatchBitmapTextContent(AFindControlOptions.MatchBitmapText) +
            'MatchBitmapFiles' + '=' + 'bmp' + '&' +
            'MatchBitmapAlgorithm' + '=' + 'mbaRawHistogramZones' + '&' +
            'MatchBitmapAlgorithmSettings.XMultipleOf' + '=' + '3' + '&' +
            'MatchBitmapAlgorithmSettings.YMultipleOf' + '=' + '4' + '&' +
            'MatchBitmapAlgorithmSettings.XOffset' + '=' + '5' + '&' +
            'MatchBitmapAlgorithmSettings.YOffset' + '=' + '6' + '&' +
            'InitialRectangle.Left' + '=' + '$Control_L$' + '&' +
            'InitialRectangle.Top' + '=' + '$Control_T$' + '&' +
            'InitialRectangle.Right' + '=' + '$Control_R$' + '&' +
            'InitialRectangle.Bottom' + '=' + '$Control_B$' + '&' +
            'InitialRectangle.LeftOffset' + '=' + '30' + '&' +
            'InitialRectangle.TopOffset' + '=' + '40' + '&' +
            'InitialRectangle.RightOffset' + '=' + '50' + '&' +
            'InitialRectangle.BottomOffset' + '=' + '60' + '&' +
            'UseWholeScreen' + '=' + 'False' + '&' +
            'ColorError' + '=' + '70' + '&' +
            'AllowedColorErrorCount' + '=' + '80' + '&' +
            'WaitForControlToGoAway' + '=' + 'True' + '&' +
            'StartSearchingWithCachedControl' + '=' + 'True' + '&' +
            'CachedControlLeft' + '=' + '90' + '&' +
            'CachedControlTop' + '=' + '100' + '&' +
            'MatchPrimitiveFiles' + '=' +'pmtv' + '&' +
            'GetAllControls' + '=' + 'True' + '&' +
            'UseFastSearch' + '=' + 'False' + '&' +
            'FastSearchAllowedColorErrorCount' + '=' + '700' + '&' +
            'IgnoredColors' + '=' + '800' + '&' +
            'SleepySearch' + '=' + 'True' + '&' +
            'StopSearchOnMismatch' + '=' + 'True' + '&' +
            'ImageSource' + '=' + 'isFile' + '&' +
            'SourceFileName' + '=' + 'Fnm' + '&' +
            'ImageSourceFileNameLocation' + '=' + 'isflDisk' + '&' +
            'PrecisionTimeout' + '=' + 'True' + '&' +
            'FullBackgroundImageInResult' + '=' + 'False' + '&' +

            'MatchByHistogramSettings.MinPercentColorMatch' + '=' + '250' + '&' +
            'MatchByHistogramSettings.MostSignificantColorCountInSubBmp' + '=' + '310' + '&' +
            'MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp' + '=' + '415' + '&' +

            'EvaluateTextCount' + '=' + '-17' + '&' +
            'CropFromScreenshot' + '=' + 'True' + '&' +
            'ThreadCount' + '=' + '30'
            ;
end;


procedure GenerateDifferentThanDefault_FindSubControl(var AFindControlOptions: TClkFindControlOptions);
begin
  GenerateDifferentThanDefault_FindControl(AFindControlOptions);

  SetLength(AFindControlOptions.MatchBitmapText, 2);
  //no need to call GetDefaultPropertyValues_FindControl_MatchBitmapText();

  AFindControlOptions.MatchBitmapText[0].ForegroundColor := '$Color_3Window$';
  AFindControlOptions.MatchBitmapText[0].BackgroundColor := '$Color_4Highlight$';
  AFindControlOptions.MatchBitmapText[0].FontName := 'Courier New';
  AFindControlOptions.MatchBitmapText[0].FontSize := 9;
  AFindControlOptions.MatchBitmapText[0].FontQualityReplacement := '80';
  AFindControlOptions.MatchBitmapText[0].FontQuality := fqAntialiased;
  AFindControlOptions.MatchBitmapText[0].FontQualityUsesReplacement := True;
  AFindControlOptions.MatchBitmapText[0].Bold := True;
  AFindControlOptions.MatchBitmapText[0].Italic := True;
  AFindControlOptions.MatchBitmapText[0].Underline := True;
  AFindControlOptions.MatchBitmapText[0].StrikeOut := True;
  AFindControlOptions.MatchBitmapText[0].CropLeft := '70';
  AFindControlOptions.MatchBitmapText[0].CropTop := '80';
  AFindControlOptions.MatchBitmapText[0].CropRight := '90';
  AFindControlOptions.MatchBitmapText[0].CropBottom := '100';
  AFindControlOptions.MatchBitmapText[0].IgnoreBackgroundColor := True;
  AFindControlOptions.MatchBitmapText[0].ProfileName := 'P0';

  AFindControlOptions.MatchBitmapText[1].ForegroundColor := '$Color_7Window$';
  AFindControlOptions.MatchBitmapText[1].BackgroundColor := '$Color_8Highlight$';
  AFindControlOptions.MatchBitmapText[1].FontName := 'Courier Old';
  AFindControlOptions.MatchBitmapText[1].FontSize := 10;
  AFindControlOptions.MatchBitmapText[1].FontQualityReplacement := '90';
  AFindControlOptions.MatchBitmapText[1].FontQuality := fqCleartypeNatural;
  AFindControlOptions.MatchBitmapText[1].FontQualityUsesReplacement := True;
  AFindControlOptions.MatchBitmapText[1].Bold := True;
  AFindControlOptions.MatchBitmapText[1].Italic := True;
  AFindControlOptions.MatchBitmapText[1].Underline := True;
  AFindControlOptions.MatchBitmapText[1].StrikeOut := True;
  AFindControlOptions.MatchBitmapText[1].CropLeft := '270';
  AFindControlOptions.MatchBitmapText[1].CropTop := '380';
  AFindControlOptions.MatchBitmapText[1].CropRight := '490';
  AFindControlOptions.MatchBitmapText[1].CropBottom := '500';
  AFindControlOptions.MatchBitmapText[1].IgnoreBackgroundColor := True;
  AFindControlOptions.MatchBitmapText[1].ProfileName := 'P1';
end;


procedure GenerateDifferentThanDefault_SetControlText(var ASetControlTextOptions: TClkSetTextOptions);
begin
  GetDefaultPropertyValues_SetControlText(ASetControlTextOptions);

  ASetControlTextOptions.Text := 'some text';
  ASetControlTextOptions.ControlType := stComboBox;
  ASetControlTextOptions.DelayBetweenKeyStrokes := '300';
  ASetControlTextOptions.Count := '7';
end;


function GenerateDifferentThanDefault_SetControlTextStr: string;
begin
  Result := 'Text' + '=' + 'some text' + '&' +
            'ControlType' + '=' + 'stComboBox' + '&' +
            'DelayBetweenKeyStrokes' + '=' + '300' + '&' +
            'Count' + '=' + '7';
end;


procedure GenerateDifferentThanDefault_CallTemplate(var ACallTemplateOptions: TClkCallTemplateOptions);
begin
  GetDefaultPropertyValues_CallTemplate(ACallTemplateOptions);

  ACallTemplateOptions.TemplateFileName := 'NonExistentTemplate';
  ACallTemplateOptions.ListOfCustomVarsAndValues := '$NewVar$=3';
  ACallTemplateOptions.EvaluateBeforeCalling := True;
  ACallTemplateOptions.CallOnlyIfCondition := False; //still required, to prevent a pop-up, until the feature is removed
  ACallTemplateOptions.CallTemplateLoop.Enabled := True;
  ACallTemplateOptions.CallTemplateLoop.Counter := '$cnt$';
  ACallTemplateOptions.CallTemplateLoop.InitValue := '78';
  ACallTemplateOptions.CallTemplateLoop.EndValue := '79';
  ACallTemplateOptions.CallTemplateLoop.Direction := ldAuto;
  ACallTemplateOptions.CallTemplateLoop.BreakCondition := '$a$==$b$';
  ACallTemplateOptions.CallTemplateLoop.EvalBreakPosition := lebpAfterContent;
end;


function GenerateDifferentThanDefault_CallTemplateStr: string;
begin
  Result := 'TemplateFileName' + '=' + 'NonExistentTemplate' + '&' +
            'ListOfCustomVarsAndValues' + '=' + '$NewVar$=3' + '&' +
            'EvaluateBeforeCalling' + '=' + 'True' + '&' +

            'Loop.Enabled' + '=' + 'True' + '&' +
            'Loop.Counter' + '=' + '$cnt$' + '&' +
            'Loop.InitValue' + '=' + '78' + '&' +
            'Loop.EndValue' + '=' + '79' + '&' +
            'Loop.Direction' + '=' + 'ldAuto' + '&' +
            'Loop.BreakCondition' + '=' + '$a$==$b$' + '&' +
            'Loop.EvalBreakPosition' + '=' + 'lebpAfterContent';
end;


procedure GenerateDifferentThanDefault_Sleep(var ASleepOptions: TClkSleepOptions);
begin
  GetDefaultPropertyValues_Sleep(ASleepOptions);
  ASleepOptions.Value := '3';
end;


function GenerateDifferentThanDefault_SleepStr: string;
begin
  Result := 'Value' + '=' + '3';
end;


procedure GenerateDifferentThanDefault_SetVar(var ASetVarOptions: TClkSetVarOptions);
begin
  GetDefaultPropertyValues_SetVar(ASetVarOptions);

  ASetVarOptions.ListOfVarNames := '$MyVar$';
  ASetVarOptions.ListOfVarValues := 'MyValue';
  ASetVarOptions.ListOfVarEvalBefore := '1';
  ASetVarOptions.FailOnException := True;
end;


function GenerateDifferentThanDefault_SetVarStr: string;
begin
  Result := 'ListOfVarNames' + '=' + '$MyVar$' + '&' +
            'ListOfVarValues' + '=' + 'MyValue' + '&' +
            'ListOfVarEvalBefore' + '=' + '1' + '&' +
            'FailOnException' + '=' + 'True';
end;


procedure GenerateDifferentThanDefault_WindowOperations(var AWindowOperationsOptions: TClkWindowOperationsOptions);
begin
  GetDefaultPropertyValues_WindowOperations(AWindowOperationsOptions);

  AWindowOperationsOptions.Operation := woMoveResize;
  AWindowOperationsOptions.NewX := '$Control_Left$';
  AWindowOperationsOptions.NewY := '$Control_Top$';
  AWindowOperationsOptions.NewWidth := '$Control_Width$';
  AWindowOperationsOptions.NewHeight := '$Control_Height$';
  AWindowOperationsOptions.NewPositionEnabled := True;
  AWindowOperationsOptions.NewSizeEnabled := True;
end;


function GenerateDifferentThanDefault_WindowOperationsStr: string;
begin
  Result := 'Operation' + '=' + 'woMoveResize' + '&' +
            'NewX' + '=' + '$Control_Left$' + '&' +
            'NewY' + '=' + '$Control_Top$' + '&' +
            'NewWidth' + '=' + '$Control_Width$' + '&' +
            'NewHeight' + '=' + '$Control_Height$' + '&' +
            'NewPositionEnabled' + '=' + 'True' + '&' +
            'NewSizeEnabled' + '=' + 'True';
end;


procedure GenerateDifferentThanDefault_LoadSetVarFromFile(var ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions);
begin
  GetDefaultPropertyValues_LoadSetVarFromFile(ALoadSetVarFromFileOptions);

  ALoadSetVarFromFileOptions.FileName := 'UnknownLoadedFile';
  ALoadSetVarFromFileOptions.SetVarActionName := '$SetVarNewName$';
end;


function GenerateDifferentThanDefault_LoadSetVarFromFileStr: string;
begin
  Result := 'FileName' + '=' + 'UnknownLoadedFile' + '&' +
            'SetVarActionName' + '=' + '$SetVarNewName$';
end;


procedure GenerateDifferentThanDefault_SaveSetVarToFile(var ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions);
begin
  GetDefaultPropertyValues_SaveSetVarToFile(ASaveSetVarToFileOptions);

  ASaveSetVarToFileOptions.FileName := 'UnknownSavedFile';
  ASaveSetVarToFileOptions.SetVarActionName := '$SetVarOldName$';
end;


function GenerateDifferentThanDefault_SaveSetVarToFileStr: string;
begin
  Result := 'FileName' + '=' + 'UnknownSavedFile' + '&' +
            'SetVarActionName' + '=' + '$SetVarOldName$';
end;


procedure GenerateDifferentThanDefault_Plugin(var APluginOptions: TClkPluginOptions);
begin
  GetDefaultPropertyValues_Plugin(APluginOptions);

  APluginOptions.FileName := 'PluginFnm';
  APluginOptions.ListOfPropertiesAndValues := 'NoPropertiesAndValues';
  APluginOptions.ListOfPropertiesAndTypes := '';
  APluginOptions.CachedCount := 0;
  APluginOptions.ListOfInitValues := '3';
end;


function GenerateDifferentThanDefault_PluginStr: string;
begin
  Result := 'FileName' + '=' + 'PluginFnm' + '&' +
            'ListOfPropertiesAndValues' + '=' + 'NoPropertiesAndValues';
end;


procedure GenerateDifferentThanDefault_EditTemplate(var AEditTemplateOptions: TClkEditTemplateOptions);
var
  ExecAppOptions: TClkExecAppOptions;
begin
  GetDefaultPropertyValues_EditTemplate(AEditTemplateOptions);

  AEditTemplateOptions.Operation := etoDeleteAction;
  AEditTemplateOptions.WhichTemplate := etwtSelf;
  AEditTemplateOptions.TemplateFileName := 'NoFnm';
  AEditTemplateOptions.ListOfEnabledProperties := 'Ena';
  AEditTemplateOptions.EditedActionName := 'Edt';
  AEditTemplateOptions.EditedActionType := acExecApp; //TClkAction(CClkUnsetAction); // acClick;
  AEditTemplateOptions.EditedActionCondition := '$3$==$4$';
  AEditTemplateOptions.EditedActionTimeout := 2000;
  AEditTemplateOptions.NewActionName := 'Generic';
  AEditTemplateOptions.ShouldSaveTemplate := False;

  AEditTemplateOptions.ListOfEditedProperties_ET := 'LstEdt';
  AEditTemplateOptions.ListOfEnabledProperties_ET := 'LstEna';

  GetDefaultPropertyValues_ExecApp(ExecAppOptions);
  AEditTemplateOptions.ListOfEditedProperties := GetExecAppActionProperties(ExecAppOptions);
end;


function GenerateDifferentThanDefault_EditTemplateStr: string;
begin
  Result := 'Operation' + '=' + 'etoDeleteAction' + '&' +
            'WhichTemplate' + '=' + 'etwtSelf' + '&' +
            'TemplateFileName' + '=' + 'NoFnm' + '&';
                                                      //full string: 'PathToApp=ListOfParams=WaitForApp=0AppStdIn=CurrentDir=UseInheritHandles=0NoConsole=0'
  Result := Result + 'ListOfEditedProperties' + '=' + 'PathToApp=' + '&'; //ListOfEditedProperties stores a #18 separated list of key=value strings

  Result := Result +
            'ListOfEnabledProperties' + '=' + 'Ena' + '&' +
            'EditedActionName' + '=' + 'Edt' + '&' +
            'EditedActionType' + '=' + 'ExecApp' + '&' +
            'EditedActionCondition' + '=' + '$3$==$4$' + '&' +
            'EditedActionTimeout' + '=' + '2000' + '&' +
            'NewActionName' + '=' + 'Generic' + '&' +
            'ShouldSaveTemplate' + '=' + 'False';
end;


end.

