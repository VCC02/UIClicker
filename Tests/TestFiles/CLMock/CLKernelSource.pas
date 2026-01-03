{
    Copyright (C) 2026 VCC
    creation date: 01 Jan 2026
    initial release date: 02 Jan 2026

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


unit CLKernelSource;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;


type
  TByteArray0 = array[0..0] of Byte;
  PByteArray0 = ^TByteArray0;

  TIntArray0 = array[0..0] of Integer;
  PIntArray0 = ^TIntArray0;

  queue_t = Pointer;
  clk_event_t = Pointer; //TBD
  ndrange_t = Pointer; //TBD

  TMatCmpSrc = class
  private
    FInstanceIndex: Integer;
    FRGBSizeStrOnBG: Byte;
    FRGBSizeStrOnSub: Byte;

    function get_global_id(dimindx: Integer): Integer;
  public
    constructor Create;
    procedure MatCmp(ABackgroundBmp, ASubBmp: PByteArray0;
                     AResultedErrCount: PIntArray0;
                     AKernelDone: PByteArray0;
                     ABackgroundWidth, ASubBmpWidth, ASubBmpHeight, AXOffset, AYOffset: DWord;
                     AColorError: Byte;
                     ASlaveQueue: Pointer);

    property InstanceIndex: Integer read FInstanceIndex write FInstanceIndex;  //this will be used to get the result of get_global_id(0)
    property RGBSizeStrOnBG: Byte read FRGBSizeStrOnBG write FRGBSizeStrOnBG;
    property RGBSizeStrOnSub: Byte read FRGBSizeStrOnSub write FRGBSizeStrOnSub;
  end;


  TSlideSearchSrc = class
  private
    FGPUSlaveQueueFromDevice: Boolean;
    FGPUUseAllKernelsEvent: Boolean;
    FGPUNdrangeNoLocalParam: Boolean;
    FGPUUseEventsInEnqueueKernel: Boolean;
    FGPUWaitForAllKernelsToBeDone: Boolean;
    FGPUReleaseFinalEventAtKernelEnd: Boolean;

    FDefaultQueue: queue_t;

    function get_default_queue: queue_t;
  public
    constructor Create;

    procedure SlideSearch(ABackgroundBmp, ASubBmp: PByteArray0;
                          AResultedErrCount, ADebuggingInfo: PIntArray0;
                          AKernelDone: PByteArray0;
                          ABackgroundWidth, ASubBmpWidth, ASubBmpHeight, AXOffset, AYOffset: DWord;
                          AColorError: Byte;
                          ASlaveQueue: Pointer;
                          ATotalErrorCount: DWord);

    property GPUSlaveQueueFromDevice: Boolean read FGPUSlaveQueueFromDevice write FGPUSlaveQueueFromDevice;
    property GPUUseAllKernelsEvent: Boolean read FGPUUseAllKernelsEvent write FGPUUseAllKernelsEvent;
    property GPUNdrangeNoLocalParam: Boolean read FGPUNdrangeNoLocalParam write FGPUNdrangeNoLocalParam;
    property GPUUseEventsInEnqueueKernel: Boolean read FGPUUseEventsInEnqueueKernel write FGPUUseEventsInEnqueueKernel;
    property GPUWaitForAllKernelsToBeDone: Boolean read FGPUWaitForAllKernelsToBeDone write FGPUWaitForAllKernelsToBeDone;
    property GPUReleaseFinalEventAtKernelEnd: Boolean read FGPUReleaseFinalEventAtKernelEnd write FGPUReleaseFinalEventAtKernelEnd;

    property DefaultQueue: queue_t write FDefaultQueue;
  end;


implementation


constructor TMatCmpSrc.Create;
begin
  inherited Create;
  FInstanceIndex := 0;
  FRGBSizeStrOnBG := 3;
  FRGBSizeStrOnSub := 3;
end;


function TMatCmpSrc.get_global_id(dimindx: Integer): Integer;
begin
  Result := FInstanceIndex;
end;


procedure TMatCmpSrc.MatCmp(ABackgroundBmp, ASubBmp: PByteArray0;
                            AResultedErrCount: PIntArray0;
                            AKernelDone: PByteArray0;
                            ABackgroundWidth, ASubBmpWidth, ASubBmpHeight, AXOffset, AYOffset: DWord;
                            AColorError: Byte;
                            ASlaveQueue: Pointer);
var
  YIdx: LongInt;
  BGRow: PByteArray0;
  SubRow: PByteArray0;
  BGRowOffset: PtrUInt;
  SubRowOffset: PtrUInt;
  ErrCount, x: LongInt;
  x0_BG, x1_BG, x2_BG: LongInt;
  x0_Sub, x1_Sub, x2_Sub: LongInt;
  SubPxB, BGPxB, SubPxG, BGPxG, SubPxR, BGPxR: ShortInt;
begin
  YIdx := get_global_id(0);

  BGRowOffset := ((DWord(YIdx) + AYOffset) * ABackgroundWidth + AXOffset) * DWord(FRGBSizeStrOnBG);
  SubRowOffset := DWord(YIdx) * ASubBmpWidth * DWord(FRGBSizeStrOnSub);

  BGRow := PByteArray0(PtrUInt(ABackgroundBmp) + BGRowOffset);
  SubRow := PByteArray0(PtrUInt(ASubBmp) + SubRowOffset);

  ErrCount := 0;
  for x := 0 to ASubBmpWidth - 1 do
  begin
    x0_BG := x * FRGBSizeStrOnBG + 0;
    x1_BG := x * FRGBSizeStrOnBG + 1;
    x2_BG := x * FRGBSizeStrOnBG + 2;
    x0_Sub := x * FRGBSizeStrOnSub + 0;
    x1_Sub := x * FRGBSizeStrOnSub + 1;
    x2_Sub := x * FRGBSizeStrOnSub + 2;

    SubPxB := SubRow^[x0_Sub];
    BGPxB := SubRow^[x0_BG];
    SubPxG := SubRow^[x1_Sub];
    BGPxG := SubRow^[x1_BG];
    SubPxR := SubRow^[x2_Sub];
    BGPxR := SubRow^[x2_BG];

    if ((abs(SubPxR - BGPxR) > AColorError) or
        (abs(SubPxG - BGPxG) > AColorError) or
        (abs(SubPxB - BGPxB) > AColorError)) then
      Inc(ErrCount);
  end;

  AResultedErrCount^[YIdx] := ErrCount;
  AKernelDone^[YIdx] := 1;
end;


constructor TSlideSearchSrc.Create;
begin
  inherited Create;

  FGPUSlaveQueueFromDevice := False;
  FGPUUseAllKernelsEvent := False;
  FGPUNdrangeNoLocalParam := False;
  FGPUUseEventsInEnqueueKernel := False;
  FGPUWaitForAllKernelsToBeDone := False;
  FGPUReleaseFinalEventAtKernelEnd := False;

  FDefaultQueue := nil;
end;


function TSlideSearchSrc.get_default_queue: queue_t;
begin
  Result := FDefaultQueue;
end;


procedure TSlideSearchSrc.SlideSearch(ABackgroundBmp, ASubBmp: PByteArray0;
                                      AResultedErrCount, ADebuggingInfo: PIntArray0;
                                      AKernelDone: PByteArray0;
                                      ABackgroundWidth, ASubBmpWidth, ASubBmpHeight, AXOffset, AYOffset: DWord;
                                      AColorError: Byte;
                                      ASlaveQueue: Pointer;
                                      ATotalErrorCount: DWord);
var
  SlaveQueue: queue_t;
  AllKernelsEvent: clk_event_t;
  AllEvents: array of clk_event_t;
  FinalEvent: clk_event_t;
  ndrange: ndrange_t;
begin
  if FGPUSlaveQueueFromDevice then
    SlaveQueue := get_default_queue    //requires OpenCL >= 2.0 and __opencl_c_device_enqueue
  else
    SlaveQueue := queue_t(ASlaveQueue);  //Default option of AGPUSlaveQueueFromDevice

  if not FGPUUseAllKernelsEvent then
    SetLength(AllEvents, ASubBmpHeight);

  //...
end;

end.

