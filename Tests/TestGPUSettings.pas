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
  Classes, SysUtils, Graphics, Menus, TestHTTPAPI;


type
  TTestGPUSettings = class(TTestHTTPAPI)
  private
    procedure ExpectSuccessfulActionWithExtraTestResult(AResponse: string; AMsgForSuccess: string = '');

    function GetCLInfo(ACustomOpenCLPath: string = ''): string;
    procedure FindMainUIClickerWindow;
    procedure BringMainUIClickerWindowToFront;
    procedure FindDashBitOnMainUIClickerWindow(ATargetPlatform, ATargetDevice: Integer);
    function FindDashBitOnMainUIClickerWindow_AllActionsWithInfo(ATargetPlatform, ATargetDevice: Integer; out AExFound: Boolean): string;

    procedure SetPlatformAndDeviceFromPersistentSettings;
    procedure SavePlatformAndDeviceToPersistentSettings;
    procedure SetPlatformAndDeviceFromAvailableHardware;
    procedure CreateGPUMenuSettings;

    procedure GPUPlatformAndDeviceOnClick(Sender: TObject);
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
  GPUTestUtils, testregistry;


const
  CExtraCaption = 'Testing_GPU';
  CGPUDeviceSettingsCategory = 'GPU';

var
  TestUIClicker_Proc: TAsyncProcess;
  FGPUInfo: TPlatformInfoArr;
  GPUMenuSettingsCreated: Boolean;
  SelectedPlatorm: string;
  SelectedDevice: string;


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


function TTestGPUSettings.GetCLInfo(ACustomOpenCLPath: string = ''): string;
var
  Response: string;
  SetVarOptions: TClkSetVarOptions;
  GPUFound: Boolean;
  i: Integer;
begin
  GetDefaultPropertyValues_SetVar(SetVarOptions);
  SetVarOptions.ListOfVarNames := '$OpenCLInfoToVars(' + ACustomOpenCLPath + ')$';   //this calls the GetOpenCLInfo function from ClickerCLUtils, through UIClicker
  SetVarOptions.ListOfVarValues := '';
  SetVarOptions.ListOfVarEvalBefore := '0';

  Result := ExecuteSetVarAction(TestServerAddress, SetVarOptions);
  Response := FastReplace_87ToReturn(Result);
  ExpectSuccessfulActionWithExtraTestResult(Response, 'Received CL vars.');
  DecodeCLInfoFromUIClickerVars(Response, FGPUInfo);

  //SetLength(FGPUInfo[0].Devices, Length(FGPUInfo[0].Devices) + 1);  FGPUInfo[0].Devices[Length(FGPUInfo[0].Devices) - 1].DeviceName := 'NewDev';  //Dbg code, to display one more device
  //SetLength(FGPUInfo[0].Devices, Length(FGPUInfo[0].Devices) + 1);  FGPUInfo[0].Devices[Length(FGPUInfo[0].Devices) - 1].DeviceName := 'LastDev';  //Dbg code, to display one more device

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


procedure TTestGPUSettings.SetPlatformAndDeviceFromPersistentSettings;
var
  TestSettings: TStringList;
begin
  TestSettings := TStringList.Create;
  try
    frmPitstopTestRunner.GetPersistentTestSettings(TestSettings);
    SelectedPlatorm := TestSettings.Values['$SelectedPlatorm$'];
    SelectedDevice := TestSettings.Values['$SelectedDevice$'];
  finally
    TestSettings.Free;
  end;
end;


procedure TTestGPUSettings.SavePlatformAndDeviceToPersistentSettings;
var
  TestSettings: TStringList;
begin
  TestSettings := TStringList.Create;
  try
    frmPitstopTestRunner.GetPersistentTestSettings(TestSettings);
    frmPitstopTestRunner.SetValueToPersistentTestSettings('$SelectedPlatorm$', SelectedPlatorm); //it should be ok if set to ''
    frmPitstopTestRunner.SetValueToPersistentTestSettings('$SelectedDevice$', SelectedDevice);   //it should be ok if set to ''
  finally
    TestSettings.Free;
  end;
end;


procedure TTestGPUSettings.SetPlatformAndDeviceFromAvailableHardware;
var
  PlatformIndex: Integer;
begin
  if SelectedPlatorm = '' then
    if Length(FGPUInfo) > 0 then
      SelectedPlatorm := FGPUInfo[0].PlatformName;

  if SelectedDevice = '' then
  begin
    PlatformIndex := GetPlatformIndexByName(SelectedPlatorm, FGPUInfo);
    if PlatformIndex > -1 then
      if Length(FGPUInfo[PlatformIndex].Devices) > 0 then
        SelectedDevice := FGPUInfo[PlatformIndex].Devices[0].DeviceName;
  end;
end;


procedure SetPlatformAndDeviceByDeviceName(ADeviceName: string);
var
  i, j: Integer;
begin
  for i := 0 to Length(FGPUInfo) - 1 do
    for j := 0 to Length(FGPUInfo[i].Devices) - 1 do
      if FGPUInfo[i].Devices[j].DeviceName = ADeviceName then
      begin
        SelectedPlatorm := FGPUInfo[i].PlatformName;
        SelectedDevice := FGPUInfo[i].Devices[j].DeviceName;
        Exit;
      end;
end;


procedure TTestGPUSettings.GPUPlatformAndDeviceOnClick(Sender: TObject);
var
  TempDeviceName: string;
begin
  TempDeviceName := (Sender as TMenuItem).Caption;
  TempDeviceName := StringReplace(TempDeviceName, '&', '', [rfReplaceAll]);

  SetPlatformAndDeviceByDeviceName(TempDeviceName);
  SavePlatformAndDeviceToPersistentSettings;
  frmPitstopTestRunner.UpdateTestSettingsItemCheckedState(CGPUDeviceSettingsCategory, SelectedPlatorm, SelectedDevice, True); //The menu item will be cleared, so it can't be used as: (Sender as TMenuItem).Checked := True;

  frmPitstopTestRunner.AddToLog('Setting testing device to ' + TempDeviceName);
end;


procedure TTestGPUSettings.CreateGPUMenuSettings;
var
  i, j: Integer;
begin
  SetPlatformAndDeviceFromAvailableHardware;

  for i := 0 to Length(FGPUInfo) - 1 do
  begin
    frmPitstopTestRunner.RegisterTestSettings(CGPUDeviceSettingsCategory, '', FGPUInfo[i].PlatformName, nil, False, False, False);

    for j := 0 to Length(FGPUInfo[i].Devices) - 1 do
      frmPitstopTestRunner.RegisterTestSettings(CGPUDeviceSettingsCategory, FGPUInfo[i].PlatformName, FGPUInfo[i].Devices[j].DeviceName, @GPUPlatformAndDeviceOnClick, False, True, FGPUInfo[i].Devices[j].DeviceName = SelectedDevice);
  end;

  frmPitstopTestRunner.RegisterTestSettings('-', '', '-', nil, False, False, False);

  frmPitstopTestRunner.RegisterTestSettings('OpenCL dll', '', 'OpenCL.dll', nil, False, True, False);
  frmPitstopTestRunner.RegisterTestSettings('OpenCL dll', '', 'CLMock.dll', nil, False, True, False);
end;


procedure TTestGPUSettings.BeforeAll_AlwaysExecute;
var
  PathToTestUIClicker, Info: string;
begin
  PathToTestUIClicker := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\UIClicker.exe');
  TestUIClicker_Proc := CreateUIClickerProcess(PathToTestUIClicker, '--SetExecMode Server --ServerPort ' + CTestServerPort + ' --ExtraCaption ' + CExtraCaption);

  try
    CreatePitstopCommandServer;
    SendTestVarsToUIClickerUnderTest;

    //Info := GetCLInfo(ExtractFilePath(ParamStr(0)) + 'TestFiles\CLMock\lib\' + 'i386-win32' + 'CLMock.dll');
    Info := GetCLInfo;
    frmPitstopTestRunner.SetExtraTestResult(Self, Info);

    if not GPUMenuSettingsCreated then
    begin
      SetPlatformAndDeviceFromPersistentSettings;
      CreateGPUMenuSettings;
      GPUMenuSettingsCreated := True;
    end;
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


function TTestGPUSettings.FindDashBitOnMainUIClickerWindow_AllActionsWithInfo(ATargetPlatform, ATargetDevice: Integer; out AExFound: Boolean): string;
begin
  Result := '';
  AExFound := False;

  FindMainUIClickerWindow;
  BringMainUIClickerWindowToFront;

  try
    FindDashBitOnMainUIClickerWindow(ATargetPlatform, ATargetDevice);
  except
    on E: Exception do
    begin
      Result  := Result + 'Err[' + IntToStr(ATargetPlatform) + ',' + IntToStr(ATargetDevice) + ']:' + E.Message + ' ';
      AExFound := True;
    end;
  end;
end;


procedure TTestGPUSettings.Test_FindDashBitOnMainUIClickerWindow_HappyFlow;
var
  s: string;
  ExFound: Boolean;
  i, j: Integer;
begin
  if TestVars.Values['$RunOnAllPlatformsAndDevices$'] = 'True' then
  begin
    s := 'PlatformCount: ' + IntToStr(Length(FGPUInfo)) + ' ';
    for i := 0 to Length(FGPUInfo) - 1 do
    begin
      s := s + 'DeviceCount[' + IntToStr(i) + ']: ' + IntToStr(Length(FGPUInfo[i].Devices)) + ' ';

      for j := 0 to Length(FGPUInfo[i].Devices) - 1 do
        s := s + FindDashBitOnMainUIClickerWindow_AllActionsWithInfo(i, j, ExFound);
    end;
  end
  else
  begin
    s := 'PlatformCount: ' + IntToStr(Length(FGPUInfo)) + ' ';
    s := s + 'DeviceCount[' + IntToStr(0) + ']: ' + IntToStr(Length(FGPUInfo[0].Devices)) + ' ';   //change 0 to configured platform
    s := s + FindDashBitOnMainUIClickerWindow_AllActionsWithInfo(0, 0, ExFound);
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
  GPUMenuSettingsCreated := False;
  SelectedPlatorm := '';
  SelectedDevice := '';

finalization
  for i := 0 to Length(FGPUInfo) - 1 do
    SetLength(FGPUInfo[i].Devices, 0);

  SetLength(FGPUInfo, 0);
end.

