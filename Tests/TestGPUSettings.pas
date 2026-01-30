{
    Copyright (C) 2026 VCC
    creation date: 29 Jan 2026
    initial release date: 30 Jan 2026

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


unit TestGPUSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, TestHTTPAPI;


type
  TDeviceInfo = record
    DeviceName: string;
    DeviceVersion: string;
    DeviceOpenCLCVersion: string;
    DevicePlatformVersion: string;
    DeviceExtensions: string;
  end;
  TDeviceInfoArr = array of TDeviceInfo;

  TPlatformInfo = record
    PlatformName: string;
    PlatformVersion: string;
    PlatformExtensions: string;
    Devices: TDeviceInfoArr;
  end;
  TPlatformInfoArr = array of TPlatformInfo;

  TTestGPUSettings = class(TTestHTTPAPI)
  private
    procedure ExpectSuccessfulActionWithExtraTestResult(AResponse: string; AMsgForSuccess: string = '');

    procedure GetCLInfo(ACustomOpenCLPath: string = '');
    procedure FindMainUIClickerWindow;
    procedure BringMainUIClickerWindowToFront;
    procedure FindDashBitOnMainUIClickerWindow(ATargetPlatform, ATargetDevice: Integer);

  public
    constructor Create; override;
    destructor Destroy; override;

  published
    procedure BeforeAll_AlwaysExecute;

    procedure Test_FindDashBitOnMainUIClickerWindow_HappyFlow;

    procedure AfterAll_AlwaysExecute;
  end;

implementation

uses
  PitstopTestRunner, PitstopTestCommands, Expectations, AsyncProcess, UITestUtils,
  ClickerUtils, ClickerCLUtils, ClickerActionsClient, ClickerActionProperties,
  testregistry;


const
  CExtraCaption = 'Testing_GPU';

var
  TestUIClicker_Proc: TAsyncProcess;
  FGPUInfo: TPlatformInfoArr;


constructor TTestGPUSettings.Create;
begin
  inherited Create;
  TestServerAddress := CTestServerAddress;
end;


destructor TTestGPUSettings.Destroy;
begin
  inherited Destroy;
end;


procedure TTestGPUSettings.ExpectSuccessfulActionWithExtraTestResult(AResponse: string; AMsgForSuccess: string = '');
begin
  try
    ExpectSuccessfulAction(AResponse);
    frmPitstopTestRunner.SetExtraTestResult(Self, AMsgForSuccess);
  except
    on E: Exception do
    begin
      frmPitstopTestRunner.SetExtraTestResult(Self, E.Message);
      raise;
    end;
  end;
end;


procedure TTestGPUSettings.GetCLInfo(ACustomOpenCLPath: string = '');
var
  Response: string;
  SetVarOptions: TClkSetVarOptions;
  ReturnedVars: TStringList;
  PlatformIndexStr, DeviceIndexStr, PlatformAndDeviceIndexStr: string;
  i, j: Integer;
  GPUFound: Boolean;
begin
  GetDefaultPropertyValues_SetVar(SetVarOptions);
  SetVarOptions.ListOfVarNames := '$OpenCLInfoToVars(' + ACustomOpenCLPath + ')$';   //this calls the GetOpenCLInfo function from ClickerCLUtils, through UIClicker
  SetVarOptions.ListOfVarValues := '';
  SetVarOptions.ListOfVarEvalBefore := '0';

  Response := FastReplace_87ToReturn(ExecuteSetVarAction(TestServerAddress, SetVarOptions));
  ExpectSuccessfulActionWithExtraTestResult(Response, 'Received CL vars.');

  ReturnedVars := TStringList.Create;
  try
    ReturnedVars.Text := Response;

    SetLength(FGPUInfo, StrToIntDef(ReturnedVars.Values['$CL.PlatformCount$'], 0));
    for i := 0 to Length(FGPUInfo) - 1 do
    begin
      PlatformIndexStr := IntToStr(i);
      FGPUInfo[i].PlatformName := ReturnedVars.Values['$CL.PlatformName[' + PlatformIndexStr + ']$'];
      FGPUInfo[i].PlatformVersion := ReturnedVars.Values['$CL.PlatformVersion[' + PlatformIndexStr + ']$'];
      FGPUInfo[i].PlatformExtensions := ReturnedVars.Values['$CL.PlatformExtensions[' + PlatformIndexStr + ']$'];

      SetLength(FGPUInfo[i].Devices, StrToIntDef(ReturnedVars.Values['$CL.DeviceCount$'], 0));
      for j := 0 to Length(FGPUInfo[i].Devices) - 1 do
      begin
        DeviceIndexStr := IntToStr(j);
        PlatformAndDeviceIndexStr := PlatformIndexStr + ', ' + DeviceIndexStr;

        FGPUInfo[i].Devices[j].DeviceName := ReturnedVars.Values['$CL.DeviceName[' + PlatformAndDeviceIndexStr + ']$'];
        FGPUInfo[i].Devices[j].DeviceVersion := ReturnedVars.Values['$CL.DeviceVersion[' + PlatformAndDeviceIndexStr + ']$'];
        FGPUInfo[i].Devices[j].DeviceOpenCLCVersion := ReturnedVars.Values['$CL.DeviceOpenCLCVersion[' + PlatformAndDeviceIndexStr + ']$'];
        FGPUInfo[i].Devices[j].DevicePlatformVersion := ReturnedVars.Values['$CL.DevicePlatformVersion[' + PlatformAndDeviceIndexStr + ']$'];
        FGPUInfo[i].Devices[j].DeviceExtensions := ReturnedVars.Values['$CL.DeviceExtensions[' + PlatformAndDeviceIndexStr + ']$'];
      end;
    end;
  finally
    ReturnedVars.Free;
  end;

  Expect(Length(FGPUInfo)).ToBeGreaterThan(0, 'At least an OpenCL platform is required to run these tests.');
  GPUFound := False;
  for i := 0 to Length(FGPUInfo) - 1 do
    if Length(FGPUInfo[i].Devices) > 0 then
    begin
      GPUFound := True;
      Break;
    end;

  Expect(GPUFound).ToBe(True, 'At least an OpenCL compatible GPU is required to run these tests.');
end;


procedure TTestGPUSettings.BeforeAll_AlwaysExecute;
var
  PathToTestUIClicker: string;
begin
  PathToTestUIClicker := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\UIClicker.exe');
  TestUIClicker_Proc := CreateUIClickerProcess(PathToTestUIClicker, '--SetExecMode Server --ServerPort ' + CTestServerPort + ' --ExtraCaption ' + CExtraCaption);

  try
    CreatePitstopCommandServer;
    SendTestVarsToUIClickerUnderTest;

    GetCLInfo;
    frmPitstopTestRunner.SetExtraTestResult(Self, 'AlwaysOK');
  except
    on E: Exception do
    begin
      frmPitstopTestRunner.SetExtraTestResult(Self, E.Message);
      raise;
    end;
  end;
end;


procedure TTestGPUSettings.FindMainUIClickerWindow;
var
  Response: string;
  FindControlOptions: TClkFindControlOptions;
begin
  GetDefaultPropertyValues_FindControl(FindControlOptions);
  FindControlOptions.MatchText := 'UI Clicker Main - ' + CExtraCaption;
  FindControlOptions.MatchClassName := 'Window';
  FindControlOptions.MatchCriteria.SearchForControlMode := sfcmFindWindow;
  Response := FastReplace_87ToReturn(ExecuteFindControlAction(TestServerAddress, FindControlOptions, 'Find UIClicker Main', 3000, CREParam_FileLocation_ValueDisk));

  ExpectSuccessfulActionWithExtraTestResult(Response, 'Main window found.');
end;


procedure TTestGPUSettings.BringMainUIClickerWindowToFront;
var
  Response: string;
  WindowOperationsOptions: TClkWindowOperationsOptions;
begin
  GetDefaultPropertyValues_WindowOperations(WindowOperationsOptions);
  Response := FastReplace_87ToReturn(ExecuteWindowOperationsAction(TestServerAddress, WindowOperationsOptions));

  ExpectSuccessfulActionWithExtraTestResult(Response, 'Main window is visible.');
end;


procedure TTestGPUSettings.FindDashBitOnMainUIClickerWindow(ATargetPlatform, ATargetDevice: Integer);
var
  Response: string;
  FindSubControlOptions: TClkFindSubControlOptions;
  i: Integer;
begin
  GetDefaultPropertyValues_FindSubControl(FindSubControlOptions, 2);
  FindSubControlOptions.MatchText := '-bit';

  for i := 0 to Length(FindSubControlOptions.MatchBitmapText) - 1 do
  begin
    FindSubControlOptions.MatchBitmapText[i].ForegroundColor := '$Color_WindowText$';
    FindSubControlOptions.MatchBitmapText[i].BackgroundColor := '$Color_BtnFace$';
    FindSubControlOptions.MatchBitmapText[i].FontName := 'Segoe UI';
    FindSubControlOptions.MatchBitmapText[i].FontSize := 9;
  end;

  FindSubControlOptions.MatchBitmapText[0].FontQuality := fqAntialiased; //TFontQuality(4)
  FindSubControlOptions.MatchBitmapText[1].FontQuality := fqCleartype; //TFontQuality(5)

  FindSubControlOptions.MatchBitmapAlgorithm := mbaBruteForceOnGPU; //TMatchBitmapAlgorithm(3)
  FindSubControlOptions.InitialRectangle.LeftOffset := '14';
  FindSubControlOptions.InitialRectangle.TopOffset := '237';
  FindSubControlOptions.InitialRectangle.RightOffset := '-280';
  FindSubControlOptions.InitialRectangle.BottomOffset := '-11';
  FindSubControlOptions.ColorError := '10';
  FindSubControlOptions.AllowedColorErrorCount := '30';
  FindSubControlOptions.GPUSettings.TargetPlatform := IntToStr(ATargetPlatform);
  FindSubControlOptions.GPUSettings.TargetDevice := IntToStr(ATargetDevice);
  //eventually, set ExecutionAvailability from params

  Response := FastReplace_87ToReturn(ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'Find bit', 2000, CREParam_FileLocation_ValueDisk));

  try
    ExpectSuccessfulActionWithExtraTestResult(Response, '-bit found.');
  except
    on E: Exception do
      if Pos('Expected "$ExecAction_Err$" to be an item of list', E.Message) > 0 then
        raise Exception.Create(Response)  //Response contains an error message, not the list of vars
      else
        raise;
  end;
end;


procedure TTestGPUSettings.Test_FindDashBitOnMainUIClickerWindow_HappyFlow;
var
  i, j: Integer;
  s: string;
  ExFound: Boolean;
begin
  ExFound := False;
  s := 'PlatformCount: ' + IntToStr(Length(FGPUInfo)) + ' ';
  for i := 0 to Length(FGPUInfo) - 1 do
  begin
    s := 'DeviceCount[' + IntToStr(i) + ']: ' + IntToStr(Length(FGPUInfo[i].Devices)) + ' ';

    for j := 0 to Length(FGPUInfo[i].Devices) - 1 do
    begin
      FindMainUIClickerWindow;
      BringMainUIClickerWindowToFront;

      try
        FindDashBitOnMainUIClickerWindow(i, j); //get all platforms and devices, then iterate through them
      except
        on E: Exception do
        begin
          s := s + 'Err[' + IntToStr(i) + ',' + IntToStr(j) + ']:' + E.Message + ' ';
          ExFound := True;
        end;
      end;
    end;
  end;

  try
    frmPitstopTestRunner.SetExtraTestResult(Self, GetVarValueFromServer(CGPUDbgVar_AdditionalGPUInfo) {+ ' TargetPlatform ' + ' TargetDevice'} + ' Run info: ' + s);
  except
    on E: Exception do
      frmPitstopTestRunner.SetExtraTestResult(Self, 'Cannot get extra info about GPU run: ' + E.Message);
  end;

  Expect(ExFound).ToBe(False, 'Expected to find the subcontrol on all platforms and devices.');
end;


procedure TTestGPUSettings.AfterAll_AlwaysExecute;
begin
  if TestUIClicker_Proc <> nil then
  begin
    TestUIClicker_Proc.Terminate(0);
    TestUIClicker_Proc.Free;
  end;

  DestroyPitstopCommandServer;
  frmPitstopTestRunner.SetExtraTestResult(Self, 'AlwaysDone');
end;


var
  i: Integer;

initialization
  RegisterTest(TTestGPUSettings);
  TestUIClicker_Proc := nil;
  SetLength(FGPUInfo, 0);

finalization
  for i := 0 to Length(FGPUInfo) - 1 do
    SetLength(FGPUInfo[i].Devices, 0);

  SetLength(FGPUInfo, 0);
end.

