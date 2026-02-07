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
    procedure BringMainUIClickerWindowIntoFullView;

    function FindDashBitOnMainUIClickerWindow(ATargetPlatform, ATargetDevice: Integer): string;
    function FindDashBitOnBMP(ATargetPlatform, ATargetDevice: Integer): string;

    function FindDashBitOnMainUIClickerWindow_AllActionsWithInfo(ATargetPlatform, ATargetDevice: Integer; out AExFound: Boolean): string;
    function FindDashBitOnBMP_AllActionsWithInfo(ATargetPlatform, ATargetDevice: Integer; out AExFound: Boolean): string;

    procedure GetPlatformAndDeviceFromPersistentSettings;
    procedure SavePlatformAndDeviceToPersistentSettings;
    procedure SetPlatformAndDeviceFromAvailableHardware;

    procedure GetOpenCLDllFromPersistentSettings;
    procedure SaveOpenCLDllToPersistentSettings;

    function GetSelectedOpenCLDllPath: string;
    procedure CreateGPUMenuSettings;

    procedure GPUPlatformAndDeviceOnClick(Sender: TObject);
    procedure OpenCLDllSelection(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure BeforeAll_AlwaysExecute;
    procedure Test_FindDashBitOnMainUIClickerWindow_HappyFlow;
    procedure Test_FindDashBitOnBMP_HappyFlow;
    procedure AfterAll_AlwaysExecute;
  end;


  TTestGPUSettingsInfo = class(TTestGPUSettings)   //should be run once, with $SetPlatformsAndDevices$ set to False
  published
    procedure BeforeAll_AlwaysExecute;
    procedure AfterAll_AlwaysExecute;
  end;


  TTestGPUSettingsByTarget = class(TTestGPUSettings)   //can be run multiple times, with $SetPlatformsAndDevices$ set to True, for every platform and device
  published
    procedure BeforeAll_AlwaysExecute;

    procedure Test_FindDashBitOnMainUIClickerWindow_HappyFlow;

    procedure AfterAll_AlwaysExecute;
  end;


  TTestGPUSettingsByTargetWithSingleAction = class(TTestGPUSettings)   //can be run multiple times, with $SetPlatformsAndDevices$ set to True, for every platform and device
  published
    procedure BeforeAll_AlwaysExecute;

    procedure Test_FindDashBitOnBMP_HappyFlow;

    procedure AfterAll_AlwaysExecute;
  end;


implementation


uses
  PitstopTestRunner, PitstopTestCommands, PitstopTestUtils,
  Expectations, AsyncProcess, UITestUtils, GPUTestUtils,
  ClickerUtils, ClickerCLUtils, ClickerActionsClient, ClickerActionProperties,
  testregistry, Forms;


const
  CExtraCaption = 'Testing_GPU';
  CGPUDeviceSettingsCategory = 'GPU';
  COpenCLDllSettingsCategory = 'OpenCL dll';

  COpenCLDll_Sys = 'OpenCL.dll (from System32)';
  COpenCLDll_Mock = 'CLMock.dll (from UIClicker tests)';

var
  TestUIClicker_Proc: TAsyncProcess;
  FGPUInfo: TPlatformInfoArr;
  GPUMenuSettingsCreated: Boolean;
  SelectedPlatorm: string;
  SelectedDevice: string;
  SelectedOpenCLDll: string;
  UIClickerBitness: string;


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


procedure TTestGPUSettings.GetPlatformAndDeviceFromPersistentSettings;
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


procedure TTestGPUSettings.GetOpenCLDllFromPersistentSettings;
var
  TestSettings: TStringList;
begin
  TestSettings := TStringList.Create;
  try
    frmPitstopTestRunner.GetPersistentTestSettings(TestSettings);
    SelectedOpenCLDll := TestSettings.Values['$OpenCLDll$'];
  finally
    TestSettings.Free;
  end;
end;


procedure TTestGPUSettings.SaveOpenCLDllToPersistentSettings;
var
  TestSettings: TStringList;
begin
  TestSettings := TStringList.Create;
  try
    frmPitstopTestRunner.GetPersistentTestSettings(TestSettings);
    frmPitstopTestRunner.SetValueToPersistentTestSettings('$OpenCLDll$', SelectedOpenCLDll);
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


procedure TTestGPUSettings.OpenCLDllSelection(Sender: TObject);
begin
  SelectedOpenCLDll := (Sender as TMenuItem).Caption;
  SelectedOpenCLDll := StringReplace(SelectedOpenCLDll, '&', '', [rfReplaceAll]);

  SaveOpenCLDllToPersistentSettings;
  frmPitstopTestRunner.UpdateTestSettingsItemCheckedState(COpenCLDllSettingsCategory, '', SelectedOpenCLDll, True);

  frmPitstopTestRunner.AddToLog('Setting OpenCL dll to ' + SelectedOpenCLDll);
  MessageBoxFunction('Please restart the test runner, to load the new settings.', PChar(Application.Title), 0);
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

  frmPitstopTestRunner.RegisterTestSettings(COpenCLDllSettingsCategory, '', COpenCLDll_Sys, @OpenCLDllSelection, False, True, SelectedOpenCLDll = COpenCLDll_Sys);
  frmPitstopTestRunner.RegisterTestSettings(COpenCLDllSettingsCategory, '', COpenCLDll_Mock, @OpenCLDllSelection, False, True, SelectedOpenCLDll = COpenCLDll_Mock);
end;


function TTestGPUSettings.GetSelectedOpenCLDllPath: string;
begin
  Result := ''; //this will cause the loader to use System32
  if SelectedOpenCLDll = COpenCLDll_Sys then
    Exit;

  if SelectedOpenCLDll = COpenCLDll_Mock then
  begin
    if UIClickerBitness = '' then
    begin
      UIClickerBitness := GetVarValueFromServer('$AppBitness$') + '-' + GetVarValueFromServer('$OSBitness$');   //returns something like 'i386-win32'
      if (UIClickerBitness <> 'i386-win32') and (UIClickerBitness <> 'x86_64-win64') then
      begin
        frmPitstopTestRunner.AddToLog('Unknown UIClicker bitness: "' + UIClickerBitness + '". Setting to default: i386-win32');
        UIClickerBitness := 'i386-win32';
      end;
    end;

    Result := ExtractFilePath(ParamStr(0)) + 'TestFiles\CLMock\lib\' + UIClickerBitness + '\CLMock.dll';
  end;
end;


procedure TTestGPUSettings.BeforeAll_AlwaysExecute;
var
  PathToTestUIClicker, Info: string;
  SelectedPlatormIndex, SelectedDeviceIndex: Integer;
begin
  PathToTestUIClicker := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\UIClicker.exe');
  TestUIClicker_Proc := CreateUIClickerProcess(PathToTestUIClicker, '--SetExecMode Server --ServerPort ' + CTestServerPort + ' --ExtraCaption ' + CExtraCaption);
  GeneralConnectTimeout := 2000; //just a bit more than default
  GeneralReadTimeout := 60000; //1min

  try
    try
      SendTestVarsToUIClickerUnderTest;
      GetOpenCLDllFromPersistentSettings;

      if SelectedOpenCLDll = COpenCLDll_Sys then
        Info := GetCLInfo
      else
        if SelectedOpenCLDll = COpenCLDll_Mock then
          Info := GetCLInfo(GetSelectedOpenCLDllPath)
        else
          Info := 'Bad OpenCL dll selection.';

      if Self is TTestGPUSettingsInfo then  //return UIClicker vars when used as info only
        frmPitstopTestRunner.SetExtraTestResult(Self, Info);

      if (TestVars <> nil) and (TestVars.Values['$SetPlatformsAndDevices$'] = 'True') then
      begin
        SelectedPlatormIndex := StrToIntDef(TestVars.Values['$SelectedPlatorm$'], 0);
        if (SelectedPlatormIndex > -1) and (SelectedPlatormIndex < Length(FGPUInfo)) then
        begin
          SelectedPlatorm := FGPUInfo[SelectedPlatormIndex].PlatformName;

          SelectedDeviceIndex := StrToIntDef(TestVars.Values['$SelectedDevice$'], 0);
          if (SelectedDeviceIndex > -1) and (SelectedDeviceIndex < Length(FGPUInfo[SelectedPlatormIndex].Devices)) then
          begin
            SelectedDevice := FGPUInfo[SelectedPlatormIndex].Devices[SelectedDeviceIndex].DeviceName;
            frmPitstopTestRunner.AddToLog('Platform and device set from client, to ' + SelectedPlatorm + ' and ' + SelectedDevice);
          end;
        end;
      end;
    finally
      if not GPUMenuSettingsCreated then        //This part has to be inside a "finally" section, because the above depend on requests to UIClicker, which if fail, may prevent the creation of the settings menu.
      begin
        GetPlatformAndDeviceFromPersistentSettings;
        //GetOpenCLDllFromPersistentSettings;   //called above

        CreateGPUMenuSettings;
        GPUMenuSettingsCreated := True;
      end;
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


procedure TTestGPUSettings.BringMainUIClickerWindowIntoFullView;
var
  Response: string;
  WindowOperationsOptions: TClkWindowOperationsOptions;
begin
  GetDefaultPropertyValues_WindowOperations(WindowOperationsOptions);
  WindowOperationsOptions.Operation := woFitIntoView;
  Response := FastReplace_87ToReturn(ExecuteWindowOperationsAction(TestServerAddress, WindowOperationsOptions));

  ExpectSuccessfulActionWithExtraTestResult(Response, 'Main window is in full view.');
end;


function TTestGPUSettings.FindDashBitOnMainUIClickerWindow(ATargetPlatform, ATargetDevice: Integer): string;
var
  Response: string;
  FindSubControlOptions: TClkFindSubControlOptions;
  i: Integer;
  AllVars: TStringList;
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
  FindSubControlOptions.GPUSettings.OpenCLPath := GetSelectedOpenCLDllPath;
  FindSubControlOptions.GPUSettings.TargetPlatform := IntToStr(ATargetPlatform);
  FindSubControlOptions.GPUSettings.TargetDevice := IntToStr(ATargetDevice);
  //eventually, set ExecutionAvailability from params

  Result := ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'Find bit', 2000, CREParam_FileLocation_ValueDisk);
  Response := FastReplace_87ToReturn(Result);

  try
    ExpectSuccessfulActionWithExtraTestResult(Response, '-bit found.');
  except
    on E: Exception do
      if Pos('Expected "$ExecAction_Err$" to be an item of list', E.Message) > 0 then
        raise Exception.Create(Response)  //Response contains an error message, not the list of vars
      else
        raise;
  end;

  AllVars := TStringList.Create;
  try
    AllVars.LineBreak := #13#10;
    AllVars.Text := Response;

    Expect(AllVars.Values['$DebugVar_SubCnvXOffset$']).ToBe('28', 'Unexpected $DebugVar_SubCnvXOffset$');
    Expect(AllVars.Values['$DebugVar_SubCnvYOffset$']).ToBe('245', 'Unexpected $DebugVar_SubCnvYOffset$');
  finally
    AllVars.Free;
  end;
end;


function TTestGPUSettings.FindDashBitOnBMP(ATargetPlatform, ATargetDevice: Integer): string;
var
  Response: string;
  FindSubControlOptions: TClkFindSubControlOptions;
  AllVars: TStringList;
begin
  GetDefaultPropertyValues_FindSubControl(FindSubControlOptions);
  FindSubControlOptions.MatchCriteria.WillMatchBitmapText := False;
  FindSubControlOptions.MatchCriteria.WillMatchBitmapFiles := True;
  SetLength(FindSubControlOptions.MatchBitmapText, 1);
  FindSubControlOptions.MatchBitmapFiles := '$AppDir$\Tests\TestFiles\DashBitRawAntialiased.bmp';
  FindSubControlOptions.MatchBitmapAlgorithm := mbaBruteForceOnGPU; //TMatchBitmapAlgorithm(3)
  FindSubControlOptions.InitialRectangle.Left := '0';
  FindSubControlOptions.InitialRectangle.Top := '0';
  FindSubControlOptions.InitialRectangle.Right := '0';
  FindSubControlOptions.InitialRectangle.Bottom := '0';
  FindSubControlOptions.InitialRectangle.RightOffset := '42';
  FindSubControlOptions.InitialRectangle.BottomOffset := '31';
  FindSubControlOptions.ColorError := '1';
  FindSubControlOptions.AllowedColorErrorCount := '1';
  FindSubControlOptions.ImageSource := isFile; //TImageSource(1)
  FindSubControlOptions.SourceFileName := '$AppDir$\Tests\TestFiles\BG32DashBit.bmp';
  FindSubControlOptions.ImageSourceFileNameLocation := isflDisk; //TImageSourceFileNameLocation(0)
  FindSubControlOptions.GPUSettings.OpenCLPath := GetSelectedOpenCLDllPath;
  FindSubControlOptions.GPUSettings.TargetPlatform := IntToStr(ATargetPlatform);
  FindSubControlOptions.GPUSettings.TargetDevice := IntToStr(ATargetDevice);

  Result := ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'Find bit', 2000, CREParam_FileLocation_ValueDisk);
  Response := FastReplace_87ToReturn(Result);

  try
    ExpectSuccessfulActionWithExtraTestResult(Response, '32-bit found.');
  except
    on E: Exception do
      if Pos('Expected "$ExecAction_Err$" to be an item of list', E.Message) > 0 then
        raise Exception.Create(Response)  //Response contains an error message, not the list of vars
      else
        raise;
  end;

  AllVars := TStringList.Create;
  try
    AllVars.LineBreak := #13#10;
    AllVars.Text := Response;

    Expect(AllVars.Values['$DebugVar_SubCnvXOffset$']).ToBe('14', 'Unexpected $DebugVar_SubCnvXOffset$');
    Expect(AllVars.Values['$DebugVar_SubCnvYOffset$']).ToBe('8', 'Unexpected $DebugVar_SubCnvYOffset$');
  finally
    AllVars.Free;
  end;
end;


function TTestGPUSettings.FindDashBitOnMainUIClickerWindow_AllActionsWithInfo(ATargetPlatform, ATargetDevice: Integer; out AExFound: Boolean): string;
begin
  Result := '';
  AExFound := False;

  FindMainUIClickerWindow;
  BringMainUIClickerWindowToFront;
  BringMainUIClickerWindowIntoFullView;
  FindMainUIClickerWindow; //call again, because bringing into view may imply moving the window

  try
    Result := FindDashBitOnMainUIClickerWindow(ATargetPlatform, ATargetDevice);
  except
    on E: Exception do
    begin
      Result  := Result + 'Err[' + IntToStr(ATargetPlatform) + ',' + IntToStr(ATargetDevice) + ']:' + E.Message + ' ';
      AExFound := True;
    end;
  end;
end;


function TTestGPUSettings.FindDashBitOnBMP_AllActionsWithInfo(ATargetPlatform, ATargetDevice: Integer; out AExFound: Boolean): string;
begin
  Result := '';
  AExFound := False;

  try
    Result := FindDashBitOnBMP(ATargetPlatform, ATargetDevice);
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
  PlatformIndex, DeviceIndex: Integer;
begin
  if TestVars.Values['$RunOnAllPlatformsAndDevices$'] = 'True' then
  begin
    s := CRunInfoPrefix;
    for i := 0 to Length(FGPUInfo) - 1 do
      for j := 0 to Length(FGPUInfo[i].Devices) - 1 do
        s := s + FindDashBitOnMainUIClickerWindow_AllActionsWithInfo(i, j, ExFound);
  end
  else
  begin
    PlatformIndex := GetPlatformIndexByName(SelectedPlatorm, FGPUInfo);
    DeviceIndex := GetDeviceIndexByName(PlatformIndex, SelectedDevice, FGPUInfo);
    s := CRunInfoPrefix + FindDashBitOnMainUIClickerWindow_AllActionsWithInfo(PlatformIndex, DeviceIndex, ExFound);
  end;

  try
    frmPitstopTestRunner.SetExtraTestResult(Self, GetVarValueFromServer(CGPUDbgVar_AdditionalGPUInfo) + ' ' + s);
  except
    on E: Exception do
      frmPitstopTestRunner.SetExtraTestResult(Self, 'Cannot get extra info about GPU run: ' + E.Message);
  end;

  Expect(ExFound).ToBe(False, 'Expected to find the subcontrol on all platforms and devices.');
end;


procedure TTestGPUSettings.Test_FindDashBitOnBMP_HappyFlow;             //ToDo: refactoring
var
  s: string;
  ExFound: Boolean;
  i, j: Integer;
  PlatformIndex, DeviceIndex: Integer;
begin
  if TestVars.Values['$RunOnAllPlatformsAndDevices$'] = 'True' then
  begin
    s := CRunInfoPrefix;
    for i := 0 to Length(FGPUInfo) - 1 do
      for j := 0 to Length(FGPUInfo[i].Devices) - 1 do
        s := s + FindDashBitOnBMP_AllActionsWithInfo(i, j, ExFound);    //different call than above
  end
  else
  begin
    PlatformIndex := GetPlatformIndexByName(SelectedPlatorm, FGPUInfo);
    DeviceIndex := GetDeviceIndexByName(PlatformIndex, SelectedDevice, FGPUInfo);
    s := CRunInfoPrefix + FindDashBitOnBMP_AllActionsWithInfo(PlatformIndex, DeviceIndex, ExFound);  //different call than above
  end;

  try
    frmPitstopTestRunner.SetExtraTestResult(Self, GetVarValueFromServer(CGPUDbgVar_AdditionalGPUInfo) + ' ' + s);
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

  frmPitstopTestRunner.SetExtraTestResult(Self, 'AlwaysDone');
end;


procedure TTestGPUSettingsInfo.BeforeAll_AlwaysExecute;
begin
  frmPitstopTestRunner.AddToLog('Running tests from TTestGPUSettingsInfo.');
  inherited BeforeAll_AlwaysExecute;
end;


procedure TTestGPUSettingsInfo.AfterAll_AlwaysExecute;
begin
  inherited AfterAll_AlwaysExecute;
end;


procedure TTestGPUSettingsByTarget.BeforeAll_AlwaysExecute;
begin
  frmPitstopTestRunner.AddToLog('Running tests from TTestGPUSettingsByTarget.');
  inherited BeforeAll_AlwaysExecute;
end;


procedure TTestGPUSettingsByTarget.Test_FindDashBitOnMainUIClickerWindow_HappyFlow;
begin
  inherited Test_FindDashBitOnMainUIClickerWindow_HappyFlow;
end;


procedure TTestGPUSettingsByTarget.AfterAll_AlwaysExecute;
begin
  inherited AfterAll_AlwaysExecute;
end;


procedure TTestGPUSettingsByTargetWithSingleAction.BeforeAll_AlwaysExecute;
begin
  frmPitstopTestRunner.AddToLog('Running tests from TTestGPUSettingsByTargetWithSingleAction.');
  inherited BeforeAll_AlwaysExecute;
end;


procedure TTestGPUSettingsByTargetWithSingleAction.Test_FindDashBitOnBMP_HappyFlow;
begin
  inherited Test_FindDashBitOnBMP_HappyFlow;
end;


procedure TTestGPUSettingsByTargetWithSingleAction.AfterAll_AlwaysExecute;
begin
  inherited AfterAll_AlwaysExecute;
end;


var
  i: Integer;

initialization
  RegisterTest(TTestGPUSettingsInfo);
  RegisterTest(TTestGPUSettingsByTarget);
  RegisterTest(TTestGPUSettingsByTargetWithSingleAction);

  TestUIClicker_Proc := nil;
  SetLength(FGPUInfo, 0);
  GPUMenuSettingsCreated := False;
  SelectedPlatorm := '';
  SelectedDevice := '';
  SelectedOpenCLDll := COpenCLDll_Sys;
  UIClickerBitness := '';

finalization
  for i := 0 to Length(FGPUInfo) - 1 do
    SetLength(FGPUInfo[i].Devices, 0);

  SetLength(FGPUInfo, 0);
end.

