{
    Copyright (C) 2025 VCC
    creation date: 26 Dec 2025
    initial release date: 26 Dec 2025

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


unit ClickerCLUtils;

{$mode Delphi}

interface

uses
  Classes, SysUtils, ClickerUtils, CLHeaders;


function GetGPUPlatformInfo(AOnAddToLog: TOnAddToLog; AOpenCLDll: TOpenCL; APlatform: cl_platform_id; APlatformParamName: cl_platform_info): string;
function GetGPUDeviceInfo(AOnAddToLog: TOnAddToLog; AOpenCLDll: TOpenCL; ADevice: cl_Device_id; ADeviceParamName: cl_device_info; ACLParamIsInt: Boolean = False): string;
procedure GetOpenCLInfo(AOnAddToLog: TOnAddToLog; ACustomOpenCLLocation: string = '');


const
  CGPUDbgVar_GPUIncludeDashG = '$GPUIncludeDashG$';
  CGPUDbgVar_GPUSlaveQueueFromDevice = '$GPUSlaveQueueFromDevice$';
  CGPUDbgVar_GPUUseAllKernelsEvent = '$GPUUseAllKernelsEvent$';
  CGPUDbgVar_GPUNdrangeNoLocalParam = '$GPUNdrangeNoLocalParam$';
  CGPUDbgVar_GPUUseEventsInEnqueueKernel = '$GPUUseEventsInEnqueueKernel$';
  CGPUDbgVar_GPUWaitForAllKernelsToBeDone = '$GPUWaitForAllKernelsToBeDone$';
  CGPUDbgVar_GPUReleaseFinalEventAtKernelEnd = '$GPUReleaseFinalEventAtKernelEnd$';
  CGPUDbgVar_GPUIgnoreExecutionAvailability = '$GPUIgnoreExecutionAvailability$';

  CGPUDbgVar_AdditionalGPUInfo = '$AdditionalGPUInfo$';

implementation


type
//  size_t = PtrUInt;
  csize_t = PtrUInt;

function GetGPUPlatformInfo(AOnAddToLog: TOnAddToLog; AOpenCLDll: TOpenCL; APlatform: cl_platform_id; APlatformParamName: cl_platform_info): string;
var
  InfoLen: csize_t;
  Info: string;
  Error: Integer;
begin
  SetLength(Info, 32768);
  Error := AOpenCLDll.clGetPlatformInfo(APlatform, APlatformParamName, csize_t(Length(Info)), @Info[1], InfoLen);

  if Error < 0 then
  begin
    AOnAddToLog('Error getting platform info: ' + CLErrorToStr(Error));
    Exit;
  end;

  SetLength(Info, InfoLen - 1);  // -1, to avoid copying the null byte
  Result := Info;
end;


function GetGPUDeviceInfo(AOnAddToLog: TOnAddToLog; AOpenCLDll: TOpenCL; ADevice: cl_Device_id; ADeviceParamName: cl_device_info; ACLParamIsInt: Boolean = False): string;
var
  InfoLen: csize_t;
  Info: string;
  Error: Integer;
begin
  SetLength(Info, 32768);

  try
    Error := AOpenCLDll.clGetDeviceInfo(ADevice, ADeviceParamName, csize_t(Length(Info)), @Info[1], InfoLen);
    if Error < 0 then
    begin
      AOnAddToLog('Error getting device info: ' + CLErrorToStr(Error));
      Exit;
    end;
  except
    on E: Exception do
    begin
      Result := 'Ex: ' + E.Message + ' at getting device info for parameter ' + IntToStr(ADeviceParamName);
      Exit;
    end;
  end;

  SetLength(Info, InfoLen - 1); // -1, to avoid copying the null byte

  if ACLParamIsInt then
    Info := IntToStr(PDWord(@Info[1])^);

  Result := Info;
end;


procedure GetOpenCLInfo(AOnAddToLog: TOnAddToLog; ACustomOpenCLLocation: string = '');
var
  OpenCLDll: TOpenCL;

  function LogCallResult(AError: Integer; AFuncName, AInfo: string): string;
  begin
    Result := 'Error ' + CLErrorToStr(AError) + ' at "' + AFuncName + '" OpenCL API call. ' + AInfo;
  end;

  function GetPlatformInfo(APlatform: cl_platform_id; APlatformParamName: cl_platform_info; APlatformParamNameStr: string): string;
  var
    InfoLen: csize_t;
    Info: string;
    Error: Integer;
  begin
    SetLength(Info, 32768);
    Error := OpenCLDll.clGetPlatformInfo(APlatform, APlatformParamName, csize_t(Length(Info)), @Info[1], InfoLen);

    if Error < 0 then
    begin
      Result := LogCallResult(Error, 'clGetPlatformInfo(' + APlatformParamNameStr + ')', '');
      Exit;
    end;

    SetLength(Info, InfoLen - 1);  // -1, to avoid copying the null byte
    Result := Info;
  end;

  function GetPlatformInfoStr(APlatformIndex: Integer; APlatform: cl_platform_id; APlatformParamName: cl_platform_info; APlatformParamNameStr: string): string;
  begin
    Result := APlatformParamNameStr + '[' + IntToStr(APlatformIndex) + ']: ' + GetPlatformInfo(APlatform, APlatformParamName, APlatformParamNameStr);
  end;

  function GetDeviceInfo(ADevice: cl_Device_id; ADeviceParamName: cl_device_info; ADeviceParamNameStr: string; ACLParamIsInt: Boolean = False): string;
  var
    InfoLen: csize_t;
    Info: string;
    Error: Integer;
  begin
    SetLength(Info, 32768);

    try
      Error := OpenCLDll.clGetDeviceInfo(ADevice, ADeviceParamName, csize_t(Length(Info)), @Info[1], InfoLen);
    except
      on E: Exception do
      begin
        Result := 'Ex: ' + E.Message + ' at ' + ADeviceParamNameStr;
        Exit;
      end;
    end;

    if Error < 0 then
    begin
      Result := LogCallResult(Error, 'clGetDeviceInfo(' + ADeviceParamNameStr + ')', '');
      Exit;
    end;

    SetLength(Info, InfoLen - 1); // -1, to avoid copying the null byte

    if ACLParamIsInt then
      Info := IntToStr(PDWord(@Info[1])^);

    Result := Info;
  end;

  function GetDeviceInfoStr(APlatformIndex, ADeviceIndex: Integer; ADevice: cl_Device_id; ADeviceParamName: cl_device_info; ADeviceParamNameStr: string; ACLParamIsInt: Boolean = False): string;
  begin
    Result := ADeviceParamNameStr + '[' + IntToStr(APlatformIndex) + ', ' + IntToStr(ADeviceIndex) + ']: ' + GetDeviceInfo(ADevice, ADeviceParamName, ADeviceParamNameStr, ACLParamIsInt);
  end;

var
  Error: Integer;
  PlatformCount, DeviceCount: cl_uint;
  PlatformIDs: ^cl_platform_id_arr;
  DeviceIDs: ^cl_device_id_arr;
  i, j: Integer;
begin
  OpenCLDll := TOpenCL.Create;
  try
    if ACustomOpenCLLocation <> '' then
      if OpenCLDll.ExpectedDllLocation <> ACustomOpenCLLocation then
      begin
        OpenCLDll.ExpectedDllFileName := ExtractFileName(ACustomOpenCLLocation);
        OpenCLDll.ExpectedDllDir := ExtractFileDir(ACustomOpenCLLocation);
        OpenCLDll.LoadOpenCLLibrary;
      end;

    if not OpenCLDll.Loaded then
    begin
      AOnAddToLog('OpenCL not available. The dll is expected to exist at ' + OpenCLDll.ExpectedDllLocation);
      Exit;
    end;

    Error := OpenCLDll.clGetPlatformIDs(0, nil, @PlatformCount);
    LogCallResult(Error, 'clGetPlatformIDs', 'PlatformCount: ' + IntToStr(PlatformCount));
    AOnAddToLog('');
    AOnAddToLog('PlatformCount: ' + IntToStr(PlatformCount));

    GetMem(PlatformIDs, PlatformCount * SizeOf(cl_platform_id));
    try
      Error := OpenCLDll.clGetPlatformIDs(PlatformCount, Pcl_platform_id(PlatformIDs), nil);
      LogCallResult(Error, 'clGetPlatformIDs', '');

      for i := 0 to PlatformCount - 1 do
      begin
        AOnAddToLog('Platform[' + IntToStr(i) + ']: ');  //Leave the ' ' after ':', because it is needed by a parser!
        AOnAddToLog('  ' + GetPlatformInfoStr(i, PlatformIDs[i], CL_PLATFORM_PROFILE, 'PlatformProfile'));
        AOnAddToLog('  ' + GetPlatformInfoStr(i, PlatformIDs[i], CL_PLATFORM_VERSION, 'PlatformVersion'));
        AOnAddToLog('  ' + GetPlatformInfoStr(i, PlatformIDs[i], CL_PLATFORM_NAME, 'PlatformName'));
        AOnAddToLog('  ' + GetPlatformInfoStr(i, PlatformIDs[i], CL_PLATFORM_VENDOR, 'PlatformVendor'));
        AOnAddToLog('  ' + GetPlatformInfoStr(i, PlatformIDs[i], CL_PLATFORM_EXTENSIONS, 'PlatformExtensions'));

        Error := OpenCLDll.clGetDeviceIDs(PlatformIDs[i], CL_DEVICE_TYPE_GPU, 0, nil, @DeviceCount);
        LogCallResult(Error, 'clGetDeviceIDs', 'DeviceCount: ' + IntToStr(DeviceCount));
        AOnAddToLog('  ' + 'DeviceCount[' + IntToStr(i) + ']: ' + IntToStr(DeviceCount));


        GetMem(DeviceIDs, DeviceCount * SizeOf(cl_device_id));
        try
          Error := OpenCLDll.clGetDeviceIDs(PlatformIDs[i], CL_DEVICE_TYPE_GPU, DeviceCount, Pcl_device_id(DeviceIDs), nil);
          LogCallResult(Error, 'clGetDeviceIDs', '');

          for j := 0 to DeviceCount - 1 do
          begin
            AOnAddToLog('  Device[' + IntToStr(i) + ', ' + IntToStr(j) + ']: ');  //Leave the ' ' after ':', because it is needed by a parser!
            AOnAddToLog('    ' + GetDeviceInfoStr(i, j, DeviceIDs[j], CL_DEVICE_NAME, 'DeviceName'));
            AOnAddToLog('    ' + GetDeviceInfoStr(i, j, DeviceIDs[j], CL_DEVICE_VENDOR, 'DeviceVendor'));
            AOnAddToLog('    ' + GetDeviceInfoStr(i, j, DeviceIDs[j], CL_DEVICE_VERSION, 'DeviceVersion'));
            AOnAddToLog('    ' + GetDeviceInfoStr(i, j, DeviceIDs[j], CL_DEVICE_PROFILE, 'DeviceProfile'));
            AOnAddToLog('    ' + GetDeviceInfoStr(i, j, DeviceIDs[j], CL_DEVICE_OPENCL_C_VERSION, 'DeviceOpenCLCVersion'));
            AOnAddToLog('    ' + GetDeviceInfoStr(i, j, DeviceIDs[j], CL_DEVICE_EXTENSIONS, 'DeviceExtensions'));
            AOnAddToLog('    ' + GetDeviceInfoStr(i, j, DeviceIDs[j], CL_DEVICE_PLATFORM_VERSION, 'DevicePlatformVersion'));

            AOnAddToLog('    ' + GetDeviceInfoStr(i, j, DeviceIDs[j], CL_DEVICE_TYPE_INFO, 'DeviceTypeInfo', True));    //CL_DEVICE_TYPE_GPU
            AOnAddToLog('    ' + GetDeviceInfoStr(i, j, DeviceIDs[j], CL_DEVICE_GLOBAL_MEM_SIZE, 'DeviceGlobalMemSize', True));
            AOnAddToLog('    ' + GetDeviceInfoStr(i, j, DeviceIDs[j], CL_DEVICE_IMAGE_SUPPORT, 'DeviceImageSupport', True));
            AOnAddToLog('    ' + GetDeviceInfoStr(i, j, DeviceIDs[j], CL_DEVICE_LOCAL_MEM_SIZE, 'DeviceLocalMemSize', True));
            AOnAddToLog('    ' + GetDeviceInfoStr(i, j, DeviceIDs[j], CL_DEVICE_AVAILABLE, 'DeviceAvailable', True));
            AOnAddToLog('    ' + GetDeviceInfoStr(i, j, DeviceIDs[j], CL_DEVICE_COMPILER_AVAILABLE, 'DeviceCompilerAvailable', True));
            AOnAddToLog('    ' + GetDeviceInfoStr(i, j, DeviceIDs[j], CL_DEVICE_EXECUTION_CAPABILITIES, 'DeviceExecutionCapabilities', True));
          end;
        finally
          Freemem(DeviceIDs, DeviceCount * SizeOf(cl_device_id));
        end;
      end;
    finally
      Freemem(PlatformIDs, PlatformCount * SizeOf(cl_platform_id));
    end;
  finally
    OpenCLDll.Free;
  end;
end;

end.

