{
    Copyright (C) 2026 VCC
    creation date: 01 Feb 2026
    initial release date: 01 Feb 2026

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


unit GPUTestUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;


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


procedure DecodeCLInfoFromUIClickerVars(AUIClickerVars: string; var AGPUInfo: TPlatformInfoArr);
function GetPlatformIndexByName(AName: string; var AGPUInfo: TPlatformInfoArr): Integer;


implementation


procedure DecodeCLInfoFromUIClickerVars(AUIClickerVars: string; var AGPUInfo: TPlatformInfoArr);
var
  ReturnedVars: TStringList;
  PlatformIndexStr, DeviceIndexStr, PlatformAndDeviceIndexStr: string;
  i, j: Integer;
begin
  ReturnedVars := TStringList.Create;
  try
    ReturnedVars.LineBreak := #13#10;
    ReturnedVars.Text := AUIClickerVars;

    SetLength(AGPUInfo, StrToIntDef(ReturnedVars.Values['$CL.PlatformCount$'], 0));
    for i := 0 to Length(AGPUInfo) - 1 do
    begin
      PlatformIndexStr := IntToStr(i);
      AGPUInfo[i].PlatformName := ReturnedVars.Values['$CL.PlatformName[' + PlatformIndexStr + ']$'];
      AGPUInfo[i].PlatformVersion := ReturnedVars.Values['$CL.PlatformVersion[' + PlatformIndexStr + ']$'];
      AGPUInfo[i].PlatformExtensions := ReturnedVars.Values['$CL.PlatformExtensions[' + PlatformIndexStr + ']$'];

      SetLength(AGPUInfo[i].Devices, StrToIntDef(ReturnedVars.Values['$CL.DeviceCount$'], 0));
      for j := 0 to Length(AGPUInfo[i].Devices) - 1 do
      begin
        DeviceIndexStr := IntToStr(j);
        PlatformAndDeviceIndexStr := PlatformIndexStr + ', ' + DeviceIndexStr;

        AGPUInfo[i].Devices[j].DeviceName := ReturnedVars.Values['$CL.DeviceName[' + PlatformAndDeviceIndexStr + ']$'];
        AGPUInfo[i].Devices[j].DeviceVersion := ReturnedVars.Values['$CL.DeviceVersion[' + PlatformAndDeviceIndexStr + ']$'];
        AGPUInfo[i].Devices[j].DeviceOpenCLCVersion := ReturnedVars.Values['$CL.DeviceOpenCLCVersion[' + PlatformAndDeviceIndexStr + ']$'];
        AGPUInfo[i].Devices[j].DevicePlatformVersion := ReturnedVars.Values['$CL.DevicePlatformVersion[' + PlatformAndDeviceIndexStr + ']$'];
        AGPUInfo[i].Devices[j].DeviceExtensions := ReturnedVars.Values['$CL.DeviceExtensions[' + PlatformAndDeviceIndexStr + ']$'];
      end;
    end;
  finally
    ReturnedVars.Free;
  end;
end;


function GetPlatformIndexByName(AName: string; var AGPUInfo: TPlatformInfoArr): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(AGPUInfo) - 1 do
    if AGPUInfo[i].PlatformName = AName then
    begin
      Result := i;
      Break;
    end;
end;

end.

