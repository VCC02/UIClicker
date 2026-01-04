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
  Classes, SysUtils, ctypes;


type
  TByteArray0 = array[0..0] of Byte;
  PByteArray0 = ^TByteArray0;

  TIntArray0 = array[0..0] of Integer;
  PIntArray0 = ^TIntArray0;

  queue_t = Pointer;
  clk_event_t = record
    Waiting: Boolean; //TBD
  end;

  Pclk_event_t = ^clk_event_t;

  ndrange_t = PtrUInt;
  kernel_enqueue_flags_t = (CLK_ENQUEUE_FLAGS_NO_WAIT, CLK_ENQUEUE_FLAGS_WAIT_KERNEL, CLK_ENQUEUE_FLAGS_WAIT_WORK_GROUP);

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


  TMatCmpParams = record
    BackgroundBmp, SubBmp: PByteArray0;
    ResultedErrCount: PIntArray0;
    KernelDone: PByteArray0;
    BackgroundWidth, SubBmpWidth, SubBmpHeight, XOffset, YOffset: DWord;
    ColorError: Byte;
    SlaveQueue: Pointer;
  end;

  PMatCmpParams = ^TMatCmpParams;


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
    function ndrange_1D(AGlobalWorkSize: csize_t): ndrange_t; overload;
    function ndrange_1D(AGlobalWorkSize, ALocalWorkSize: csize_t): ndrange_t; overload;

    function enqueue_kernel(AQueue: queue_t; AFlags: kernel_enqueue_flags_t; ANdRange: ndrange_t; AMatCmpParams: PMatCmpParams): Integer; overload;
    function enqueue_kernel(AQueue: queue_t; AFlags: kernel_enqueue_flags_t; ANdRange: ndrange_t; ANumEventsInWaitList: DWord; AEventWaitList: Pclk_event_t; var AEventRet: clk_event_t; AMatCmpParams: PMatCmpParams): Integer; overload;
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


function TSlideSearchSrc.ndrange_1D(AGlobalWorkSize: csize_t): ndrange_t;
begin
  Result := AGlobalWorkSize;
end;


function TSlideSearchSrc.ndrange_1D(AGlobalWorkSize, ALocalWorkSize: csize_t): ndrange_t;
begin
  Result := ALocalWorkSize;
end;


function TSlideSearchSrc.enqueue_kernel(AQueue: queue_t; AFlags: kernel_enqueue_flags_t; ANdRange: ndrange_t; AMatCmpParams: PMatCmpParams): Integer;
var
  Mat: TMatCmpSrc;
  i: Integer;
begin
  for i := 0 to ANdRange - 1 do
  begin
    Mat := TMatCmpSrc.Create;
    try
      Mat.InstanceIndex := i;
      //Mat.RGBSizeStrOnBG := FRGBSizeStrOnBG;  //ToDo: implement this info
      //Mat.RGBSizeStrOnSub := FRGBSizeStrOnSub;//ToDo: implement this info

      Mat.MatCmp(AMatCmpParams^.BackgroundBmp,
                 AMatCmpParams^.SubBmp,
                 AMatCmpParams^.ResultedErrCount,
                 AMatCmpParams^.KernelDone,
                 AMatCmpParams^.BackgroundWidth,
                 AMatCmpParams^.SubBmpWidth,
                 AMatCmpParams^.SubBmpHeight,
                 AMatCmpParams^.XOffset,
                 AMatCmpParams^.YOffset,
                 AMatCmpParams^.ColorError,
                 AMatCmpParams^.SlaveQueue);
    finally
      Mat.Free;
    end;
  end;
end;


function TSlideSearchSrc.enqueue_kernel(AQueue: queue_t; AFlags: kernel_enqueue_flags_t; ANdRange: ndrange_t; ANumEventsInWaitList: DWord; AEventWaitList: Pclk_event_t; var AEventRet: clk_event_t; AMatCmpParams: PMatCmpParams): Integer;
begin
  enqueue_kernel(AQueue, AFlags, ANdRange, ANumEventsInWaitList, AEventWaitList, AEventRet, AMatCmpParams);
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
  MyFlags: kernel_enqueue_flags_t;
  i, j, k: Integer;
  Found, AllKernelsDone: Boolean;
  EnqKrnErr, EnqMrkErr, XOffset, YOffset, DifferentCount: Integer;
  MatCmpParams: TMatCmpParams;
begin
  if FGPUSlaveQueueFromDevice then
    SlaveQueue := get_default_queue    //requires OpenCL >= 2.0 and __opencl_c_device_enqueue
  else
    SlaveQueue := queue_t(ASlaveQueue);  //Default option of AGPUSlaveQueueFromDevice

  if not FGPUUseAllKernelsEvent then
    SetLength(AllEvents, ASubBmpHeight);

  if FGPUNdrangeNoLocalParam then
    ndrange := ndrange_1D(ASubBmpHeight)
  else
    ndrange := ndrange_1D(1, ASubBmpHeight);

  MyFlags := CLK_ENQUEUE_FLAGS_NO_WAIT;

  Found := False;
  AllKernelsDone := False;

  EnqKrnErr := -1234;
  EnqMrkErr := -4567;
  XOffset := AXOffset;
  YOffset := AYOffset;
  DifferentCount := 0;

  for i := 0 to YOffset - 1 do
  begin
    for j := 0 to XOffset - 1 do
    begin
      for k := 0 to ASubBmpHeight - 1 do
        AKernelDone^[k] := 0;

      MatCmpParams.BackgroundBmp := ABackgroundBmp;
      MatCmpParams.SubBmp := ASubBmp;
      MatCmpParams.ResultedErrCount := AResultedErrCount;
      MatCmpParams.KernelDone := AKernelDone;
      MatCmpParams.BackgroundWidth := ABackgroundWidth;
      MatCmpParams.SubBmpWidth := ASubBmpWidth;
      MatCmpParams.SubBmpHeight := ASubBmpHeight;
      MatCmpParams.XOffset := j;
      MatCmpParams.YOffset := i;
      MatCmpParams.ColorError := AColorError;
      MatCmpParams.SlaveQueue := SlaveQueue;

      if not FGPUUseAllKernelsEvent then
      begin
        if FGPUUseEventsInEnqueueKernel then
        begin
          for k := 0 to ASubBmpHeight - 1 do    //this line is used only when calling with AllEvents, and should not exist when calling with AllKernelsEvent
            EnqKrnErr := enqueue_kernel(SlaveQueue, MyFlags, ndrange, 0, nil, AllEvents[k], @MatCmpParams);
        end
        else
        begin
          for k := 0 to ASubBmpHeight - 1 do    //this line is used only when calling with AllEvents, and should not exist when calling with AllKernelsEvent
            EnqKrnErr := enqueue_kernel(SlaveQueue, MyFlags, ndrange, @MatCmpParams);
        end;
      end
      else
      begin
        if FGPUUseEventsInEnqueueKernel then
          EnqKrnErr := enqueue_kernel(SlaveQueue, MyFlags, ndrange, 0, nil, AllKernelsEvent, @MatCmpParams)
        else
          EnqKrnErr := enqueue_kernel(SlaveQueue, MyFlags, ndrange, @MatCmpParams);
      end;

      if EnqKrnErr < 0 then
        Break;
    end;

    if Found or (EnqKrnErr < 0) then
      Break;
  end;

  ;
  //...
end;

end.

