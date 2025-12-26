{
    Copyright (C) 2024 VCC
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

end.

