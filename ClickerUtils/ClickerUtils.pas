{
    Copyright (C) 2022 VCC
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


unit ClickerUtils;

{$H+}
{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
    LCLIntf,
  {$ELSE}
    Windows,
  {$ENDIF}
  SysUtils, Classes, VirtualTrees, Graphics, Controls,
  IdGlobal, DCPmd5, ClickerIniFiles;


type
  TClkAction = (acClick, acExecApp, acFindControl, acFindSubControl, acSetControlText, acCallTemplate, acSleep, acSetVar, acWindowOperations);
  TClkSetTextControlType = (stEditBox, stComboBox, stKeystrokes);
  TSearchForControlMode = (sfcmGenGrid, sfcmEnumWindows, sfcmFindWindow);
  TWindowOperation = (woBringToFront, woMoveResize, woClose);
                      //SetForegroundWindow, SetWindowPos, WM_CLOSE

  TActionDebuggingStatus = (adsNone, adsPrev, adsCurrent, adsNext);
  TActionStatus = (asNotStarted, asFailed, asSuccessful, asInProgress, asAllowedFailed);

  TEvaluateReplacementsFunc = function(s: string; Recursive: Boolean = True): string of object;
  TOnTriggerOnControlsModified = procedure of object;
  TOnEvaluateReplacements = function(s: string): string of object;
  TOnCopyControlTextAndClassFromMainWindow = procedure(ACompProvider: string; out AControlText, AControlClass: string) of object;
  TOnGetExtraSearchAreaDebuggingImage = function(AExtraBitmap: TBitmap): Boolean of object;
  TOnGetExtraSearchAreaDebuggingImageWithStackLevel = function(AExtraBitmap: TBitmap; AStackLevel: Integer): Boolean of object;
  TOnGetConnectionAddress = function: string of object;
  TOnGetSelectedCompFromRemoteWin = function: THandle of object;
  TOnLoadBitmap = function(ABitmap: TBitmap; AFileName: string): Boolean of object; //returns True if file loaded, and False if file not found
  TOnEditActionCondition = function(var AActionCondition: string): Boolean of object;

  TOnFileExists = function(const AFileName: string): Boolean of object;
  TOnTClkIniReadonlyFileCreate = function(AFileName: string): TClkIniReadonlyFile of object;
  TOnSaveTemplateToFile = procedure(AStringList: TStringList; const AFileName: string) of object;
  TOnSetTemplateOpenDialogInitialDir = procedure(AInitialDir: string) of object;
  TOnTemplateOpenSetMultiSelect = procedure of object; //the dialog is set to allow multiple files to be selected, then it is restored automatically to single file
  TOnTemplateOpenDialogExecute = function: Boolean of object;
  TOnGetTemplateOpenDialogFileName = function: string of object;
  TOnSetTemplateOpenDialogFileName = procedure(AFileName: string) of object;
  TOnSetPictureOpenDialogInitialDir = procedure(AInitialDir: string) of object;
  TOnPictureOpenDialogExecute = function: Boolean of object;
  TOnGetPictureOpenDialogFileName = function: string of object;

const
  CClkActionStr: array[Low(TClkAction)..High(TClkAction)] of string = ('Click', 'ExecApp', 'FindControl', 'FindSubControl', 'SetControlText', 'CallTemplate', 'Sleep', 'SetVar', 'WindowOperations');

  CClickType_Click = 0;
  CClickType_Drag = 1;

  CFuncExVarName = '$FunctionException$';


type
  TClkActionOptions = record
    ActionName: string;
    Action: TClkAction;
    ActionTimeout: Integer; //ms
    ActionEnabled: Boolean;
    ActionCondition: string;
  end;

  TXClickPointReference = (xrefLeft, xrefRight, xrefWidth, xrefVar, xrefAbsolute);    //see CXOffsetReference below if modified
  TYClickPointReference = (yrefTop, yrefBottom, yrefHeight, yrefVar, yrefAbsolute);   //see CYOffsetReference below if modified

  TClkClickOptions = record
    XClickPointReference: TXClickPointReference;
    YClickPointReference: TYClickPointReference;
    XClickPointVar: string;
    YClickPointVar: string;
    XOffset, YOffset: string;
    MouseButton: TMouseButton;
    ClickWithCtrl: Boolean;
    ClickWithAlt: Boolean;
    ClickWithShift: Boolean;
    ClickWithDoubleClick: Boolean;
    Count: Integer;
    LeaveMouse: Boolean;
    MoveWithoutClick: Boolean;
    ClickType: Integer;    //see CClickType_Click and CClickType_Drag
    XClickPointReferenceDest: TXClickPointReference;
    YClickPointReferenceDest: TYClickPointReference;
    XClickPointVarDest: string;
    YClickPointVarDest: string;
    XOffsetDest, YOffsetDest: string;
  end;

  TExecAppUseInheritHandles = (uihNo, uihYes, uihOnlyWithStdInOut);

  TClkExecAppOptions = record
    PathToApp: string;
    ListOfParams: string;
    WaitForApp: Boolean;
    AppStdIn: string;
    CurrentDir: string;
    UseInheritHandles: TExecAppUseInheritHandles;
    NoConsole: Boolean;
  end;

  TRectString = record
    Left, Top, Right, Bottom: string;
    LeftOffset, TopOffset, RightOffset, BottomOffset: string;
  end;

  TClkFindControlMatchCriteria = record
    WillMatchText: Boolean;
    WillMatchClassName: Boolean;
    WillMatchBitmapText: Boolean;
    WillMatchBitmapFiles: Boolean;
    SearchForControlMode: TSearchForControlMode;
  end;

  TClkFindControlMatchBitmapText = record
    ForegroundColor: string;
    BackgroundColor: string;
    FontName: string;
    FontSize: Integer;
    Bold: Boolean;
    Italic: Boolean;
    Underline: Boolean;
    StrikeOut: Boolean;
    FontQuality: TFontQuality;
    FontQualityUsesReplacement: Boolean;
    FontQualityReplacement: string;
    ProfileName: string;
    CropLeft: string;
    CropTop: string;
    CropRight: string;
    CropBottom: string;
  end;

  TClkFindControlMatchBitmapTextArr = array of TClkFindControlMatchBitmapText;

  //once implemented, do not change the order of these values, because they are saved in files as integers
  TMatchBitmapAlgorithm = (mbaBruteForce, mbaXYMultipleAndOffsets);

  TMatchBitmapAlgorithmSettings = record
    XMultipleOf: Integer;
    YMultipleOf: Integer;
    XOffset: Integer;
    YOffset: Integer;
  end;

  TClkFindControlOptions = record
    MatchCriteria: TClkFindControlMatchCriteria;
    AllowToFail: Boolean;
    MatchText: string;
    MatchClassName: string;
    MatchTextSeparator: string;
    MatchClassNameSeparator: string;
    MatchBitmapText: TClkFindControlMatchBitmapTextArr;
    MatchBitmapFiles: string; //ListOfStrings
    MatchBitmapAlgorithm: TMatchBitmapAlgorithm;
    MatchBitmapAlgorithmSettings: TMatchBitmapAlgorithmSettings;
    InitialRectange: TRectString;
    UseWholeScreen: Boolean;
    ColorError: string;  //string, to allow var replacements
    AllowedColorErrorCount: string;  //Number of pixels allowed to mismatch
    WaitForControlToGoAway: Boolean;
    StartSearchingWithCachedControl: Boolean;
    CachedControlLeft: string;
    CachedControlTop: string;
  end;

  TClkSetTextOptions = record
    Text: string;
    ControlType: TClkSetTextControlType;
    //other future options
  end;

  TLoopDirection = (ldInc, ldDec, ldAuto);
  TLoopEvalBreakPosition = (lebpAfterContent, lebpBeforeContent);

  TClkCallTemplateLoop = record
    Enabled: Boolean; //When False, the CallTemplate action is executed once, as before. Else, it may be executed or not, based on loop settings.
    Counter: string;
    InitValue: string;
    EndValue: string;
    Direction: TLoopDirection;
    BreakCondition: string; //uses the same format as TClkActionOptions.ActionCondition
    EvalBreakPosition: TLoopEvalBreakPosition;
  end;

  TClkCallTemplateOptions = record
    TemplateFileName: string;
    ListOfCustomVarsAndValues: string;
    CallOnlyIfCondition: Boolean;         //deprecated  - these fields are here for compatibility with some old templates. Action conditions are available to all actions.
    CallOnlyIfConditionVarName: string;   //deprecated  - these fields are here for compatibility with some old templates
    CallOnlyIfConditionVarValue: string;  //deprecated  - these fields are here for compatibility with some old templates
    EvaluateBeforeCalling: Boolean;
    CallTemplateLoop: TClkCallTemplateLoop;
  end;

  TClkSleepOptions = record
    Value: string;  // [ms]
  end;

  TClkSetVarOptions = record
    ListOfVarNames: string;
    ListOfVarValues: string;
    ListOfVarEvalBefore: string;
  end;

  TClkWindowOperationsOptions = record
    Operation: TWindowOperation;
    NewX, NewY, NewWidth, NewHeight: string;
    NewPositionEabled, NewSizeEabled: Boolean;
  end;

  TActionBreakPoint = record
    Exists: Boolean; //when False, the action has no breakpoint
    Enabled: Boolean;
    Condition: string;  //Empty string means no condition. Otherwise, it should be a condition which can be evaluated to 0 or 1.
  end;

  TClkActionRec = record
    ActionDebuggingStatus: TActionDebuggingStatus;
    ActionBreakPoint: TActionBreakPoint;
    ActionStatus: TActionStatus;
    ActionSkipped: Boolean; //this should be false before executing an action, and true after the action, only if the condition is false

    ActionOptions: TClkActionOptions;
    ClickOptions: TClkClickOptions;
    ExecAppOptions: TClkExecAppOptions;
    FindControlOptions: TClkFindControlOptions;
    SetTextOptions: TClkSetTextOptions;
    CallTemplateOptions: TClkCallTemplateOptions;
    SleepOptions: TClkSleepOptions;
    SetVarOptions: TClkSetVarOptions;
    WindowOperationsOptions: TClkWindowOperationsOptions;
  end;

  TClkActionsRecArr = array of TClkActionRec;


  TVarRepl = record
    VarName: string;
    VarValue: string;
  end;

  TVarReplArr = array of TVarRepl;


const
  CActionStatusStr: array[Low(TActionStatus)..High(TActionStatus)] of string = ('Not Started', 'Failed', 'Successful', 'In Progress', 'Allowed Failed');
  CBoolToCheckState: array[Boolean] of TCheckState = (csUncheckedNormal, csCheckedNormal);

  CCompNotEqual = '<>';        //all these comparison operators should be two characters long
  CCompEqual = '==';
  CCompLessThan = '<?';
  CCompGreaterThan = '>?';
  CCompLessThanOrEqual = '<=';
  CCompGreaterThanOrEqual = '>=';
  CComparisonOperators: array[0..5] of string = (CCompNotEqual, CCompEqual, CCompLessThan, CCompGreaterThan, CCompLessThanOrEqual, CCompGreaterThanOrEqual);

  CLabel_Orange: TColor = $366FFF;
  CLabel_LightGreen: TColor = $4CC123;


function FastReplace_ReturnTo45(s: string): string;
function FastReplace_45ToReturn(s: string): string;
function FastReplace_ReturnTo68(s: string): string; //used for storing CRLF (replaced by #6#8) inside a CRLF separated list of variables
function FastReplace_68ToReturn(s: string): string; //used for storing CRLF (replaced by #6#8) inside a CRLF separated list of variables
function FastReplace_ReturnTo87(s: string): string; //should be used for remote execution only
function FastReplace_87ToReturn(s: string): string; //should be used for remote execution only
function FastReplace_ReturnToCSV(s: string): string;

function GetIsUserAnAdmin: string;

function EvaluateAllReplacements(AListOfVars: TStringList; s: string; Recursive: Boolean = True): string;
function CreateDirWithSubDirs(ADir: string): Boolean;
function HexToInt(s: string): Cardinal;

procedure RawExpressionToParts(RawExpression: string; out Op1, Op2, OpEq: string);
function MatchCriteriaToString(Criteria: TClkFindControlMatchCriteria): string;
function EvaluateActionCondition(AActionCondition: string; AEvalReplacementsFunc: TEvaluateReplacementsFunc): Boolean;

function ArrOfByteToHex(var AArr: TIdBytes): string;
function ComputeHash(AFileContent: Pointer; AFileSize: Int64): string;
function GetFileHash(AFileName: string): string;


implementation


uses
  ShellAPI, Forms;


{#13#10 -> #4#5}
function FastReplace_ReturnTo45(s: string): string;
var
  i, n: Integer;
begin
  n := Pos(#13, s);
  if n = 0 then
  begin
    Result := s;
    Exit;
  end;

  for i := n to Length(s) - 1 do
    if s[i] = #13 then
      if s[i + 1] = #10 then
      begin
        s[i] := #4;
        s[i + 1] := #5;
        Continue;
      end;

  Result := s;
end;


function FastReplace_45ToReturn(s: string): string;
var
  i, n: Integer;
begin
  n := Pos(#4, s);
  if n = 0 then
  begin
    Result := s;
    Exit;
  end;

  for i := n to Length(s) - 1 do
    if s[i] = #4 then
      if s[i + 1] = #5 then
      begin
        s[i] := #13;
        s[i + 1] := #10;
        Continue;
      end;

  Result := s;
end;


{#13#10 -> #6#8}
function FastReplace_ReturnTo68(s: string): string;
var
  i, n: Integer;
begin
  n := Pos(#13, s);
  if n = 0 then
  begin
    Result := s;
    Exit;
  end;

  for i := n to Length(s) - 1 do
    if s[i] = #13 then
      if s[i + 1] = #10 then
      begin
        s[i] := #6;
        s[i + 1] := #8;
        Continue;
      end;

  Result := s;
end;


function FastReplace_68ToReturn(s: string): string;
var
  i, n: Integer;
begin
  n := Pos(#6, s);
  if n = 0 then
  begin
    Result := s;
    Exit;
  end;

  for i := n to Length(s) - 1 do
    if s[i] = #6 then
      if s[i + 1] = #8 then
      begin
        s[i] := #13;
        s[i + 1] := #10;
        Continue;
      end;

  Result := s;
end;


function FastReplace_ReturnTo87(s: string): string; //should be used for remote execution only
var
  i, n: Integer;
begin
  n := Pos(#13, s);
  if n = 0 then
  begin
    Result := s;
    Exit;
  end;

  for i := n to Length(s) - 1 do
    if s[i] = #13 then
      if s[i + 1] = #10 then
      begin
        s[i] := #8;
        s[i + 1] := #7;
        Continue;
      end;

  Result := s;
end;


function FastReplace_87ToReturn(s: string): string; //should be used for remote execution only
var
  i, n: Integer;
begin
  n := Pos(#8, s);
  if n = 0 then
  begin
    Result := s;
    Exit;
  end;

  for i := n to Length(s) - 1 do
    if s[i] = #8 then
      if s[i + 1] = #7 then
      begin
        s[i] := #13;
        s[i + 1] := #10;
        Continue;
      end;

  Result := s;
end;


function FastReplace_ReturnToCSV(s: string): string;
var
  i, n: Integer;
begin
  n := Pos(#13, s);
  if n = 0 then
  begin
    Result := s;
    Exit;
  end;

  for i := n to Length(s) do
    if s[i] = #13 then
      s[i] := ','
    else
      if s[i] = #10 then
        s[i] := ' ';

  Result := s;
end;


function GetIsUserAnAdmin: string;
type
  TIsUserAnAdmin = function: Boolean;
const
  CResult: array[Boolean] of string = ('', '  [Is admin]');
var
  DllHandle: THandle;
  IsUserAnAdmin: TIsUserAnAdmin;
begin
  Result := '  [admin unknown]';

  DllHandle := LoadLibrary(shell32);
  if DllHandle <> 0 then
  begin
    {$IFDEF FPC}
      IsUserAnAdmin := TIsUserAnAdmin(GetProcAddress(DllHandle, 'IsUserAnAdmin'));
    {$ELSE}
      @IsUserAnAdmin := GetProcAddress(DllHandle, 'IsUserAnAdmin');
    {$ENDIF}
    Result := CResult[IsUserAnAdmin()];

    FreeLibrary(DllHandle);
  end;
end;


function ReplaceOnce(AListOfVars: TStringList; s: string; AReplaceRandom: Boolean = True): string;   forward;


function ReplaceRandom(AListOfVars: TStringList; s: string): string;
var
  PosComma: Integer;
  RandomArgs, InitialRandomArgs: string;
  RandMin, RandMax: Integer;
  RandMinStr, RandMaxStr: string;
  RandomValueStr: string;
begin
  RandomArgs := Copy(s, Pos('(', s) + 1, MaxInt);
  RandomArgs := Copy(RandomArgs, 1, Pos(')$', RandomArgs) - 1);
  InitialRandomArgs := RandomArgs;

  if RandomArgs = '' then
    RandomValueStr := IntToStr(Random(65536))
  else
  begin
    RandomArgs := ReplaceOnce(AListOfVars, RandomArgs, False);
    RandomArgs := StringReplace(RandomArgs, ' ', '', [rfReplaceAll]);
    PosComma := Pos(',', RandomArgs);

    if PosComma > 0 then
    begin
      RandMinStr := Copy(RandomArgs, 1, PosComma - 1);
      RandMaxStr := Copy(RandomArgs, PosComma + 1, MaxInt);

      RandMin := StrToIntDef(RandMinStr, 0);
      RandMax := StrToIntDef(RandMaxStr, MaxInt);
    end
    else
    begin
      RandMin := 0;
      RandMax := StrToIntDef(RandomArgs, MaxInt);
    end;

    RandomValueStr := IntToStr(RandMin + Random(RandMax - RandMin));
  end;

  Result := StringReplace(s, '$Random(' + InitialRandomArgs + ')$', RandomValueStr, [rfReplaceAll]);
end;


function ReplaceSum(AListOfVars: TStringList; s: string): string;
var
  PosComma: Integer;
  SumArgs, InitialSumArgs: string;
  SumOperand1, SumOperand2: Integer;
  SumOperand1Str, SumOperand2Str: string;
  ResultValueStr: string;
begin
  SumArgs := Copy(s, Pos('(', s) + 1, MaxInt);
  SumArgs := Copy(SumArgs, 1, Pos(')$', SumArgs) - 1);
  InitialSumArgs := SumArgs;

  if SumArgs = '' then
    ResultValueStr := '0'
  else
  begin
    SumArgs := ReplaceOnce(AListOfVars, SumArgs, False);
    SumArgs := StringReplace(SumArgs, ' ', '', [rfReplaceAll]);
    PosComma := Pos(',', SumArgs);

    if PosComma > 0 then
    begin
      SumOperand1Str := Copy(SumArgs, 1, PosComma - 1);
      SumOperand2Str := Copy(SumArgs, PosComma + 1, MaxInt);

      SumOperand1 := StrToIntDef(SumOperand1Str, 0);
      SumOperand2 := StrToIntDef(SumOperand2Str, 0);
    end
    else
    begin
      SumOperand1 := 0;
      SumOperand2 := StrToIntDef(SumArgs, 0);
    end;

    ResultValueStr := IntToStr(SumOperand1 + SumOperand2);
  end;

  Result := StringReplace(s, '$Sum(' + InitialSumArgs + ')$', ResultValueStr, [rfReplaceAll]);
end;


function ReplaceDiff(AListOfVars: TStringList; s: string): string;
var
  PosComma: Integer;
  DiffArgs, InitialDiffArgs: string;
  DiffOperand1, DiffOperand2: Integer;
  DiffOperand1Str, DiffOperand2Str: string;
  ResultValueStr: string;
begin
  DiffArgs := Copy(s, Pos('(', s) + 1, MaxInt);
  DiffArgs := Copy(DiffArgs, 1, Pos(')$', DiffArgs) - 1);
  InitialDiffArgs := DiffArgs;

  if DiffArgs = '' then
    ResultValueStr := '0'
  else
  begin
    DiffArgs := ReplaceOnce(AListOfVars, DiffArgs, False);
    DiffArgs := StringReplace(DiffArgs, ' ', '', [rfReplaceAll]);
    PosComma := Pos(',', DiffArgs);

    if PosComma > 0 then
    begin
      DiffOperand1Str := Copy(DiffArgs, 1, PosComma - 1);
      DiffOperand2Str := Copy(DiffArgs, PosComma + 1, MaxInt);

      DiffOperand1 := StrToIntDef(DiffOperand1Str, 0);
      DiffOperand2 := StrToIntDef(DiffOperand2Str, 0);
    end
    else
    begin
      DiffOperand1 := 0;
      DiffOperand2 := StrToIntDef(DiffArgs, 0);
    end;

    ResultValueStr := IntToStr(DiffOperand1 - DiffOperand2);
  end;

  Result := StringReplace(s, '$Diff(' + InitialDiffArgs + ')$', ResultValueStr, [rfReplaceAll]);
end;


function ReplaceExtractFileDir(s: string): string;
var
  DirArgs, InitialDirArgs: string;
begin
  DirArgs := Copy(s, Pos('(', s) + 1, MaxInt);
  DirArgs := Copy(DirArgs, 1, Pos(')$', DirArgs) - 1);
  InitialDirArgs := DirArgs;

  Result := StringReplace(s, '$ExtractFileDir(' + InitialDirArgs + ')$', ExtractFileDir(DirArgs), [rfReplaceAll]);
end;


function ReplaceChr(s: string): string;
var
  Args, InitialArgs: string;
  ArgsNum: Integer;
begin
  Args := Copy(s, Pos('(', s) + 1, MaxInt);
  Args := Copy(Args, 1, Pos(')$', Args) - 1);
  InitialArgs := Args;
  ArgsNum := StrToIntDef(Args, 65);

  Result := StringReplace(s, '$Chr(' + InitialArgs + ')$', Chr(ArgsNum), [rfReplaceAll]);
end;


function ReplaceFastReplace_45ToReturn(s: string): string;
var
  Args, InitialArgs: string;
begin
  Args := Copy(s, Pos('(', s) + 1, MaxInt);
  Args := Copy(Args, 1, Pos(')$', Args) - 1);
  InitialArgs := Args;

  Result := StringReplace(s, '$FastReplace_45ToReturn(' + InitialArgs + ')$', FastReplace_45ToReturn(Args), [rfReplaceAll]);
end;


function ReplaceFastReplace_ReturnTo45(s: string): string;
var
  Args, InitialArgs: string;
begin
  Args := Copy(s, Pos('(', s) + 1, MaxInt);
  Args := Copy(Args, 1, Pos(')$', Args) - 1);
  InitialArgs := Args;

  Result := StringReplace(s, '$FastReplace_ReturnTo45(' + InitialArgs + ')$', FastReplace_ReturnTo45(Args), [rfReplaceAll]);
end;


function ReplaceExit(s: string): string;
var
  Args, InitialArgs: string;
begin
  Args := Copy(s, Pos('(', s) + 1, MaxInt);
  Args := Copy(Args, 1, Pos(')$', Args) - 1);
  InitialArgs := Args;

  Result := StringReplace(s, '$Exit(' + InitialArgs + ')$', 'Exit(<ExitCode>) should be called from SetVar action, to stop the template.', [rfReplaceAll]);
end;


function ReplaceStringContains(AListOfVars: TStringList; s: string): string;  //as limitation, the substring should not contain commas
var
  PosComma: Integer;
  Args, InitialArgs: string;
  Operand1Str, Operand2Str: string;
  ResultValueStr: string;
begin
  Args := Copy(s, Pos('(', s) + 1, MaxInt);
  Args := Copy(Args, 1, Pos(')$', Args) - 1);
  InitialArgs := Args;

  if Args = '' then
    ResultValueStr := '0'
  else
  begin
    Args := ReplaceOnce(AListOfVars, Args, False);
    Args := StringReplace(Args, ' ', '', [rfReplaceAll]);
    PosComma := Pos(',', Args);

    if PosComma > 0 then
    begin
      Operand1Str := Copy(Args, 1, PosComma - 1);
      Operand2Str := Copy(Args, PosComma + 1, MaxInt);
    end
    else
    begin
      Operand1Str := '';
      Operand2Str := Args;
    end;

    ResultValueStr := IntToStr(Ord(Pos(Operand1Str, Operand2Str)));
  end;

  Result := StringReplace(s, '$StringContains(' + InitialArgs + ')$', ResultValueStr, [rfReplaceAll]);
end;


function ReplaceCreateDir(s: string): string;
var
  DirArgs, InitialDirArgs: string;
begin
  DirArgs := Copy(s, Pos('(', s) + 1, MaxInt);
  DirArgs := Copy(DirArgs, 1, Pos(')$', DirArgs) - 1);
  InitialDirArgs := DirArgs;

  Result := StringReplace(s, '$CreateDir(' + InitialDirArgs + ')$', IntToStr(Ord(CreateDirWithSubDirs(DirArgs))), [rfReplaceAll]);
end;


function ReplaceLoadTextFile(AListOfVars: TStringList; s: string): string;
var
  FileArgs, InitialFileArgs: string;
  AStringList: TStringList;
begin
  FileArgs := Copy(s, Pos('(', s) + 1, MaxInt);
  FileArgs := Copy(FileArgs, 1, Pos(')$', FileArgs) - 1);
  InitialFileArgs := FileArgs;

  FileArgs := StringReplace(FileArgs, '"', '', [rfReplaceAll]);

  Result := '';
  try
    AStringList := TStringList.Create;
    try
      AStringList.LoadFromFile(FileArgs);
      Result := FastReplace_ReturnTo45(AStringList.Text);

      if Result = #4#5 then
        Result := '';
    finally
      AStringList.Free;
    end;

    Result := StringReplace(s, '$LoadTextFile(' + InitialFileArgs + ')$', Result, [rfReplaceAll]);
  except
    on E: Exception do
    begin
      Result := '';
      AListOfVars.Values[CFuncExVarName] := E.Message;  //can be file not found, or some permission error
    end;
  end;
end;


function ReplaceItemCount(s: string): string;
var
  ItemArgs, InitialItemArgs: string;
  Count, i: Integer;
begin
  ItemArgs := Copy(s, Pos('(', s) + 1, MaxInt);
  ItemArgs := Copy(ItemArgs, 1, Pos(')$', ItemArgs) - 1);
  InitialItemArgs := ItemArgs;

  Count := 0;
  for i := 1 to Length(ItemArgs) - 1 do  //yes, from 1 to len -1
    if (ItemArgs[i] = #4) and (ItemArgs[i + 1] = #5) then
    begin
      Inc(Count);
      Continue;
    end;

  Result := StringReplace(s, '$ItemCount(' + InitialItemArgs + ')$', IntToStr(Count), [rfReplaceAll]);
end;


function ReplaceGetTextItem(AListOfVars: TStringList; s: string): string;
var
  ItemArgs, InitialItemArgs, Content, IndexStr: string;
  CurrentIndex, i, ItemIndex, PosComma, ItemPos, PrevItemPos: Integer;
  Found: Boolean;
begin
  ItemArgs := Copy(s, Pos('(', s) + 1, MaxInt);
  ItemArgs := Copy(ItemArgs, 1, Pos(')$', ItemArgs) - 1);
  InitialItemArgs := ItemArgs;

  if ItemArgs = '' then
  begin
    Result := '';
    AListOfVars.Values[CFuncExVarName] := 'Missing arguments for GetTextItem.';
    Exit;
  end
  else
  begin
    PosComma := Pos(',', ItemArgs);

    if PosComma > 0 then
    begin
      Content := Copy(ItemArgs, 1, PosComma - 1);
      IndexStr := Copy(ItemArgs, PosComma + 1, MaxInt);
    end
    else
    begin
      Content := Copy(ItemArgs, 1, MaxInt);
      IndexStr := '0';
    end;
  end;

  ItemIndex := StrToIntDef(IndexStr, -1);
  if ItemIndex = -1 then
  begin
    Result := '';
    AListOfVars.Values[CFuncExVarName] := 'List index out of bounds: -1';
    Exit;
  end;

  ItemPos := 1;
  PrevItemPos := 1;
                 //this parser should be faster than converting the content to CRLF separated string and assigning it to a TStringList
  CurrentIndex := -1;
  Found := False;
  for i := 1 to Length(Content) - 1 do  //yes, from 1 to len -1
    if (Content[i] = #4) and (Content[i + 1] = #5) then
    begin
      Inc(CurrentIndex);

      if CurrentIndex >= ItemIndex then
      begin
        Found := True;
        ItemPos := i;
        Break;
      end;

      PrevItemPos := i + 2;
      Continue;
    end;

  if not Found then
  begin
    Result := '';
    AListOfVars.Values[CFuncExVarName] := 'List index out of bounds: ' + IntToStr(ItemIndex);
    Exit;
  end;

  Result := Copy(Content, PrevItemPos, ItemPos - PrevItemPos);
  Result := StringReplace(s, '$GetTextItem(' + InitialItemArgs + ')$', Result, [rfReplaceAll]);
end;


function ReplaceOnce(AListOfVars: TStringList; s: string; AReplaceRandom: Boolean = True): string;
var
  i: Integer;
  tp: TPoint;
  CurrentName, CurrentValue: string;
begin
  for i := 0 to AListOfVars.Count - 1 do
  begin
    CurrentName := AListOfVars.Names[i];
    CurrentValue := AListOfVars.ValueFromIndex[i];

    if Pos(CurrentName, CurrentValue) = 0 then  //avoid expanding a string into its "superset", because it will lead to infinite recursion (e.g.: $TextToFind$=$TextToFind$=$ProjectName$ )
      s := StringReplace(s, CurrentName, CurrentValue, [rfReplaceAll]);
  end;

  if Pos('$Random(', s) > 0 then
  begin
    if Random(7) = 3 then
    begin
      Sleep(3);
      Randomize;
    end;

    s := ReplaceRandom(AListOfVars, s);
  end;

  if Pos('$ExtractFileDir(', s) > 0 then
    s := ReplaceExtractFileDir(s);

  if Pos('$Chr(', s) > 0 then
    s := ReplaceChr(s);

  if Pos('$Current_Mouse_X$', s) > 0 then
  begin
    GetCursorPos(tp);
    s := StringReplace(s, '$Current_Mouse_X$', IntToStr(tp.X), [rfReplaceAll]);
  end;

  if Pos('$Current_Mouse_Y$', s) > 0 then
  begin
    GetCursorPos(tp);
    s := StringReplace(s, '$Current_Mouse_Y$', IntToStr(tp.Y), [rfReplaceAll]);
  end;

  if Pos('$CRLF$', s) > 0 then
    s := StringReplace(s, '$CRLF$', #13#10, [rfReplaceAll]);

  if Pos('$#4#5$', s) > 0 then
    s := StringReplace(s, '$#4#5$', #4#5, [rfReplaceAll]);

  if Pos('$Now$', s) > 0 then
    s := StringReplace(s, '$Now$', DateTimeToStr(Now), [rfReplaceAll]);

  if Pos('$Exit(', s) > 0 then
    s := ReplaceExit(s);

  if Pos('$StringContains(', s) > 0 then
    s := ReplaceStringContains(AListOfVars, s);

  if Pos('$CreateDir(', s) > 0 then
    s := ReplaceCreateDir(s);

  if Pos('$LoadTextFile(', s) > 0 then
    s := ReplaceLoadTextFile(AListOfVars, s);

  if Pos('$ItemCount(', s) > 0 then
    s := ReplaceItemCount(s);

  if Pos('$GetTextItem(', s) > 0 then
    s := ReplaceGetTextItem(AListOfVars, s);

  if Pos('$FastReplace_45ToReturn(', s) > 0 then
    s := ReplaceFastReplace_45ToReturn(s);

  if Pos('$FastReplace_ReturnTo45(', s) > 0 then
    s := ReplaceFastReplace_ReturnTo45(s);

  if Pos('$Sum(', s) > 0 then
    s := ReplaceSum(AListOfVars, s);

  if Pos('$Diff(', s) > 0 then
    s := ReplaceDiff(AListOfVars, s);

  Result := s;
end;


function EvaluateAllReplacements(AListOfVars: TStringList; s: string; Recursive: Boolean = True): string;
var
  temp_s: string;
  i: Integer;
begin
  if s = '' then
  begin
    Result := '';
    Exit;
  end;

  temp_s := ReplaceOnce(AListOfVars, s);

  if Recursive and (Pos('$', temp_s) > 0) then
  begin
    i := 0;
    repeat
      Result := temp_s;
      temp_s := ReplaceOnce(AListOfVars, Result);
      if temp_s = Result then //no replacements found
        Break;

      Inc(i);
    until False or (i > 1000);
  end
  else
    Result := temp_s;
end;


function CreateDirWithSubDirs(ADir: string): Boolean;   //requires absolute paths
var
  ADirTemp, PrevTemp: string;
begin
  Result := True; ///success
  if DirectoryExists(ADir) then
    Exit;

  ADirTemp := ADir;
  repeat
    if ADirTemp = '' then
    begin
      Result := False;
      Exit;
    end;

    if not CreateDir(ADirTemp) then
    begin
      PrevTemp := ADirTemp;
      ADirTemp := ExtractFileDir(ADirTemp);
      if PrevTemp = ADirTemp then
      begin
        Result := False;
        Exit;
      end;
    end
    else
      ADirTemp := ADir;
  until DirectoryExists(ADir);
end;


function Pow16(x: Byte): Cardinal;
var
  i: Byte;
begin
  Result := 1;
  for i := 1 to x do
    Result := Result shl 4;
end;


function HexaDigitToByte(ch: Char): Byte;
begin
  Result := 0;
  Ch := UpCase(Ch);
  if Ch in ['0'..'9'] then
  begin
    Result := Ord(Ch) - 48;
    Exit;
  end;

  if Ch in ['A'..'F'] then
    Result := Ord(Ch) - 65 + 10;
end;


function HexToInt(s: string): Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := Length(s) downto 1 do
    if s[i] in ['0'..'9', 'a'..'f', 'A'..'F'] then
      Result := Result + HexaDigitToByte(s[i]) * Pow16(Length(s) - i);
end;


procedure RawExpressionToParts(RawExpression: string; out Op1, Op2, OpEq: string);
var
  EqPos, i: Integer;
begin
  {EqPos := Pos('==', RawExpression);
  if EqPos = 0 then
    EqPos := Pos('<>', RawExpression)
  else}

  EqPos := 0;

  for i := Low(CComparisonOperators) to High(CComparisonOperators) do
  begin
    EqPos := Pos(CComparisonOperators[i], RawExpression);
    if EqPos > 0 then
      Break;
  end;

  if EqPos = 0 then
    raise Exception.Create('Unknown operator in expression:' + RawExpression);

  Op1 := Copy(RawExpression, 1, EqPos - 1);
  Op2 := Copy(RawExpression, EqPos + 2, MaxInt);
  OpEq := Copy(RawExpression, EqPos, 2);
end;


function MatchCriteriaToString(Criteria: TClkFindControlMatchCriteria): string;
begin
  Result := '';
  if Criteria.WillMatchText then
    Result := Result + 'Text';

  if Criteria.WillMatchClassName then
  begin
    if Result > '' then
      Result := Result + ', ';
    Result := Result + 'Class';
  end;

  if Criteria.WillMatchBitmapText then
  begin
    if Result > '' then
      Result := Result + ', ';
    Result := Result + 'Bmp';
  end;

  if Criteria.WillMatchBitmapFiles then
  begin
    if Result > '' then
      Result := Result + ', ';
    Result := Result + 'Bmp files';
  end;
end;


function EvaluateActionCondition(AActionCondition: string; AEvalReplacementsFunc: TEvaluateReplacementsFunc): Boolean;
var
  RawCondition: string;
  AStringList, AConditionPart: TStringList;
  i, j: Integer;
  PartialResult, EvalResult: Boolean;
  Op1, Op2, OpEq: string;
begin
  Result := False;
  EvalResult := False;

  if (AActionCondition = '') or (AActionCondition = #13#10) then
  begin
    Result := True;
    Exit;
  end;

  AStringList := TStringList.Create;
  try
    AStringList.Text := AActionCondition;

    for i := 0 to AStringList.Count - 1 do
    begin
      PartialResult := True;
      AConditionPart := TStringList.Create;
      try
        AConditionPart.Text := StringReplace(AStringList.Strings[i], #5#6, #13#10, [rfReplaceAll]);

        for j := 0 to AConditionPart.Count - 1 do
        begin
          RawCondition := AConditionPart.Strings[j];
          RawExpressionToParts(RawCondition, Op1, Op2, OpEq);

          if OpEq = CCompNotEqual then
            EvalResult := AEvalReplacementsFunc(Op1) <> AEvalReplacementsFunc(Op2)
          else
            if OpEq = CCompEqual then
              EvalResult := AEvalReplacementsFunc(Op1) = AEvalReplacementsFunc(Op2)
            else
              if OpEq = CCompLessThan then
                EvalResult := AEvalReplacementsFunc(Op1) < AEvalReplacementsFunc(Op2)
              else
                if OpEq = CCompGreaterThan then
                  EvalResult := AEvalReplacementsFunc(Op1) > AEvalReplacementsFunc(Op2)
                else
                  if OpEq = CCompLessThanOrEqual then
                    EvalResult := AEvalReplacementsFunc(Op1) <= AEvalReplacementsFunc(Op2)
                  else
                    if OpEq = CCompGreaterThanOrEqual then
                      EvalResult := AEvalReplacementsFunc(Op1) >= AEvalReplacementsFunc(Op2)
                    else
                      raise Exception.Create('operator "' + OpEq + '" not implemented in EvaluateActionCondition.');

          PartialResult := PartialResult and EvalResult;
        end; //for j
      finally
        AConditionPart.Free;
      end;

      Result := Result or PartialResult;
    end;
  finally
    AStringList.Free;
  end;
end;


function ArrOfByteToHex(var AArr: TIdBytes): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(AArr) - 1 do
    Result := Result + IntToHex(AArr[i], 2);
end;


function ComputeHash(AFileContent: Pointer; AFileSize: Int64): string;
var
  DCP_md5: TDCP_md5;
  BinHash: TIdBytes;
begin
  DCP_md5 := TDCP_md5.Create(nil);
  try
    SetLength(BinHash, 20);
    try
      DCP_md5.Init;
      DCP_md5.Update(AFileContent^, AFileSize);
      DCP_md5.Final(BinHash[0]);

      SetLength(BinHash, 16);
      Result := ArrOfByteToHex(BinHash);
    finally
      SetLength(BinHash, 0);
    end;
  finally
    DCP_md5.Free;
  end;
end;


function GetFileHash(AFileName: string): string;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(AFileName);
    Result := ComputeHash(Stream.Memory, Stream.Size);
  finally
    Stream.Free;
  end;
end;


end.
