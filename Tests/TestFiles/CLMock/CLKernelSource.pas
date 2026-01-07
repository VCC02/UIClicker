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

  //queue_t_rec = record
  //  Kernel: TThread; // the thread object   //to be converted to array
  //end;

  queue_t = Pointer; //^queue_t_rec;  //The problem, so far, is that a queue can be passed from host, as a pointer. If queue_t doesn't have the same structure, it ends up overwriting memory.

  clk_event_t = record
    Done: Byte; //ideally, using string, because it is auto-initialized to '', but a byte can be "indexed" easily
  end;

  Pclk_event_t = ^clk_event_t;
  PPclk_event_t = ^Pclk_event_t;

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
                     ABackgroundWidth, ASubBmpWidth, ASubBmpHeight, AXOffset, AYOffset: Integer;
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
    BackgroundWidth, SubBmpWidth, SubBmpHeight, XOffset, YOffset: Integer;
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
    FRGBSizeStrOnBG: Byte;
    FRGBSizeStrOnSub: Byte;

    FCreatedKernels: array of Pointer; //Using this array, for now, instead of a queue command/operation/item. This should be moved to the queue_t_rec structure.

    function get_default_queue: queue_t;
    function ndrange_1D(AGlobalWorkSize: csize_t): ndrange_t; overload;
    function ndrange_1D(AGlobalWorkSize, ALocalWorkSize: csize_t): ndrange_t; overload;

    function enqueue_kernel(AQueue: queue_t; AFlags: kernel_enqueue_flags_t; ANdRange: ndrange_t; AMatCmpParams: PMatCmpParams): Integer; overload;
    function enqueue_kernel(AQueue: queue_t; AFlags: kernel_enqueue_flags_t; ANdRange: ndrange_t; ANumEventsInWaitList: DWord; AEventWaitList: Pclk_event_t; var AEventRet: Pclk_event_t; AMatCmpParams: PMatCmpParams): Integer; overload;

    function AllThreadsDone: Boolean;
    function enqueue_marker(AQueue: queue_t; ANumEventsInWaitList: DWord; AEventWaitList: PPclk_event_t; AEventRet: PPclk_event_t): Integer;
    procedure release_event(AEvent: Pclk_event_t);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SlideSearch(ABackgroundBmp, ASubBmp: PByteArray0;
                          AResultedErrCount, ADebuggingInfo: PIntArray0;
                          AKernelDone: PByteArray0;
                          ABackgroundWidth, ASubBmpWidth, ASubBmpHeight, AXOffset, AYOffset: Integer;
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
    property RGBSizeStrOnBG: Byte read FRGBSizeStrOnBG write FRGBSizeStrOnBG;
    property RGBSizeStrOnSub: Byte read FRGBSizeStrOnSub write FRGBSizeStrOnSub;
  end;


implementation


uses
  CLHeaders;


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
                            ABackgroundWidth, ASubBmpWidth, ASubBmpHeight, AXOffset, AYOffset: Integer;
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
  SubPxB, BGPxB, SubPxG, BGPxG, SubPxR, BGPxR: SmallInt;
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
    BGPxB := BGRow^[x0_BG];
    SubPxG := SubRow^[x1_Sub];
    BGPxG := BGRow^[x1_BG];
    SubPxR := SubRow^[x2_Sub];
    BGPxR := BGRow^[x2_BG];

    if ((abs(SubPxR - BGPxR) > AColorError) or
        (abs(SubPxG - BGPxG) > AColorError) or
        (abs(SubPxB - BGPxB) > AColorError)) then
      Inc(ErrCount);
  end;

  AResultedErrCount^[YIdx] := ErrCount;
  AKernelDone^[YIdx] := 1;
end;


type
  TRunMatCmpSrc = class(TThread)
  private
    FMatCmpParams: PMatCmpParams;
    FInstanceIndex: Integer;
    FRGBSizeStrOnBG: Byte;
    FRGBSizeStrOnSub: Byte;
    FDone: Boolean;
  protected
    procedure Execute; override;
  end;


procedure TRunMatCmpSrc.Execute;
var
  Mat: TMatCmpSrc;
begin
  try
    try
      Mat := TMatCmpSrc.Create;
      try
        Mat.InstanceIndex := FInstanceIndex;
        Mat.RGBSizeStrOnBG := FRGBSizeStrOnBG;
        Mat.RGBSizeStrOnSub := FRGBSizeStrOnSub;

        Mat.MatCmp(FMatCmpParams^.BackgroundBmp,
                   FMatCmpParams^.SubBmp,
                   FMatCmpParams^.ResultedErrCount,
                   FMatCmpParams^.KernelDone,
                   FMatCmpParams^.BackgroundWidth,
                   FMatCmpParams^.SubBmpWidth,
                   FMatCmpParams^.SubBmpHeight,
                   FMatCmpParams^.XOffset,
                   FMatCmpParams^.YOffset,
                   FMatCmpParams^.ColorError,
                   FMatCmpParams^.SlaveQueue);
      finally
        Mat.Free;
      end;
    except
    end;
  finally
    FDone := True;
  end;
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
  FRGBSizeStrOnBG := 3;
  FRGBSizeStrOnSub := 3;
end;


destructor TSlideSearchSrc.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(FCreatedKernels) - 1 do
    TRunMatCmpSrc(FCreatedKernels[i]).Destroy;

  SetLength(FCreatedKernels, 0);

  inherited Destroy;
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
  //Mat: TMatCmpSrc;
  i: Integer;
  Th: TRunMatCmpSrc;
begin
  if AQueue = nil then
  begin
    Result := CLK_INVALID_QUEUE;
    Exit;
  end;

  SetLength(FCreatedKernels, ANdRange);
  for i := 0 to Integer(ANdRange) - 1 do
  begin
    //Mat := TMatCmpSrc.Create;               //The irony is that using muliple threads, results in slower execution.
    //try
    //  Mat.InstanceIndex := i;
    //  Mat.RGBSizeStrOnBG := FRGBSizeStrOnBG;
    //  Mat.RGBSizeStrOnSub := FRGBSizeStrOnSub;
    //
    //  Mat.MatCmp(AMatCmpParams^.BackgroundBmp,
    //             AMatCmpParams^.SubBmp,
    //             AMatCmpParams^.ResultedErrCount,
    //             AMatCmpParams^.KernelDone,
    //             AMatCmpParams^.BackgroundWidth,
    //             AMatCmpParams^.SubBmpWidth,
    //             AMatCmpParams^.SubBmpHeight,
    //             AMatCmpParams^.XOffset,
    //             AMatCmpParams^.YOffset,
    //             AMatCmpParams^.ColorError,
    //             AMatCmpParams^.SlaveQueue);
    //finally
    //  Mat.Free;
    //end;

    Th := TRunMatCmpSrc.Create(True);
    Th.FDone := False;
    Th.FMatCmpParams := AMatCmpParams;
    Th.FInstanceIndex := i;
    Th.FRGBSizeStrOnBG := FRGBSizeStrOnBG;
    Th.FRGBSizeStrOnSub := FRGBSizeStrOnSub;
    //Pass the event AEventRet to the Th object, and set it as AEventRet^.Done := 1; after done

    TRunMatCmpSrc(FCreatedKernels[i]) := Th;
    Th.Start;
  end;

  //AQueue^.Kernel := Th;   //add to array
  Result := CL_SUCCESS; //CLK_SUCCESS
end;


function TSlideSearchSrc.enqueue_kernel(AQueue: queue_t; AFlags: kernel_enqueue_flags_t; ANdRange: ndrange_t; ANumEventsInWaitList: DWord; AEventWaitList: Pclk_event_t; var AEventRet: Pclk_event_t; AMatCmpParams: PMatCmpParams): Integer;
begin
  Result := enqueue_kernel(AQueue, AFlags, ANdRange, AMatCmpParams);
  New(AEventRet);
  AEventRet^.Done := 0;  //Most likely, a single event is returned. This should become 1 after finishing execution.
end;


function TSlideSearchSrc.AllThreadsDone: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Length(FCreatedKernels) - 1 do
    if not TRunMatCmpSrc(FCreatedKernels[i]).FDone then
    begin
      Result := False;
      Break;
    end;
end;


function TSlideSearchSrc.enqueue_marker(AQueue: queue_t; ANumEventsInWaitList: DWord; AEventWaitList: PPclk_event_t; AEventRet: PPclk_event_t): Integer;
type
  Tclk_event_ts = array[0..0] of clk_event_t;
  Pclk_event_ts = ^Tclk_event_ts;
var
  i: Integer;
  CurrentEvent: ^clk_event_t;
  AllDone: Boolean;
  tk: QWord;
begin
  Result := CL_SUCCESS; //CLK_SUCCESS

  try
    if AQueue = nil then
    begin
      Result := CLK_INVALID_QUEUE;
      Exit;
    end;

    if AEventWaitList = nil then
    begin
      Result := CLK_INVALID_EVENT_WAIT_LIST;
      Exit;
    end;

    if AEventRet = nil then
      Exit; //nop

    New(AEventRet^);
    AEventRet^^.Done := 0;

    tk := GetTickCount64;
    repeat
      //Sleep(1);  //better leave commented

      if GetTickCount64 - tk > 5000 then  //a unique value of timeout
      begin
        Result := -200; //just a value for timeout
        Exit;
      end;
    until AllThreadsDone;

    //for i := 0 to Length(FCreatedKernels) - 1 do
    AEventRet^^.Done := 1;  //based on all threads

                                    //uncomment the following code after implementing events which manage threads:
    //tk := GetTickCount64;
    //AllDone := True;
    //repeat
    //  for i := 0 to Integer(ANumEventsInWaitList) - 1 do
    //  begin
    //    CurrentEvent := @Pclk_event_ts(AEventWaitList)^[i];
    //    if CurrentEvent^.Done = 0 then
    //      AllDone := False;
    //  end;
    //
    //  if GetTickCount64 - tk > 6000 then  //a unique value of timeout
    //  begin
    //    Result := -200; //just a value for timeout
    //    Exit;
    //  end;
    //until AllDone;

    AEventRet^^.Done := 1;
  finally
    //AEventRet^.Done := 1; //moved outside of the finally section, to be executed only if there is no timeout
  end;
end;


procedure TSlideSearchSrc.release_event(AEvent: Pclk_event_t);
begin
  //Do nothing for now. If clk_event_t is a pointer type, then release_event should free memory.
  Dispose(AEvent);
end;


function get_work_dim: Integer;
begin
  Result := 1; //TBD
end;


function get_global_size(ADimIndx: Integer): Integer;
begin
  if (ADimIndx < 0) or (ADimIndx > get_work_dim - 1) then
  begin
    Result := 1;
    Exit;
  end;

  Result := 1; ///TBD   //the result should come from host
end;


function get_local_size(ADimIndx: Integer): Integer;
begin
  if (ADimIndx < 0) or (ADimIndx > get_work_dim - 1) then
  begin
    Result := 1;
    Exit;
  end;

  Result := 1; ///TBD   //the result should come from host
end;


function get_enqueued_local_size(ADimIndx: Integer): Integer;
begin
  if (ADimIndx < 0) or (ADimIndx > get_work_dim - 1) then
  begin
    Result := 1;
    Exit;
  end;

  Result := 1; ///TBD
end;


procedure TSlideSearchSrc.SlideSearch(ABackgroundBmp, ASubBmp: PByteArray0;
                                      AResultedErrCount, ADebuggingInfo: PIntArray0;
                                      AKernelDone: PByteArray0;
                                      ABackgroundWidth, ASubBmpWidth, ASubBmpHeight, AXOffset, AYOffset: Integer; //these should be eventually changed to signed (Integer)
                                      AColorError: Byte;
                                      ASlaveQueue: Pointer;
                                      ATotalErrorCount: DWord);
var
  SlaveQueue: queue_t;
  AllKernelsEvent: Pclk_event_t;
  AllEvents: array of Pclk_event_t;  //array of pointers, to allow initializing every element to nil
  FinalEvent: Pclk_event_t;
  ndrange: ndrange_t;
  MyFlags: kernel_enqueue_flags_t;
  i, j, k: Integer;
  Found, AllKernelsDone: Boolean;
  EnqKrnErr, EnqMrkErr, XOffset, YOffset, DifferentCount: Integer;
  TotalErrorCount: Integer;

  MatCmpParams: TMatCmpParams;
  tk: QWord;
begin
  if FGPUSlaveQueueFromDevice then
    SlaveQueue := get_default_queue    //requires OpenCL >= 2.0 and __opencl_c_device_enqueue
  else
    SlaveQueue := queue_t(ASlaveQueue);  //Default option of AGPUSlaveQueueFromDevice

  if not FGPUUseAllKernelsEvent then
  begin
    SetLength(AllEvents, ASubBmpHeight);
    for k := 0 to Length(AllEvents) - 1 do
      AllEvents[k] := nil;  //init here, so that enqueue_marker will return an error on null poiters instead of uninitialized pointers
  end;

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

      if not FGPUUseAllKernelsEvent then         // $GPUUseAllKernelsEvent$ should be set to True in UIClicker, at least until enqueue_kernel is confirmed to set an entire array of events (like AllEvents in the code below)
      begin
        if FGPUUseEventsInEnqueueKernel then
        begin
          //for k := 0 to ASubBmpHeight - 1 do    //this line is used only when calling with AllEvents, and should not exist when calling with AllKernelsEvent
            EnqKrnErr := enqueue_kernel(SlaveQueue, MyFlags, ndrange, 0, nil, AllEvents[0]{[k]}, @MatCmpParams);  //by passing @AllEvents[0], enqueue_kernel can write multiple items
        end
        else
        begin
          //for k := 0 to ASubBmpHeight - 1 do    //this line is used only when calling with AllEvents, and should not exist when calling with AllKernelsEvent
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

      tk := GetTickCount64;
      //wait for all kernels to be done
      AllKernelsDone := False;
      while not AllKernelsDone do
      begin
        if GetTickCount64 - tk > 7000 then  //7s is a unique value, which can identify where timeout comes from
          Break; //a safety mechanism, which is not in the original GPU code

        AllKernelsDone := True;
        for k := 0 to ASubBmpHeight - 1 do
          if AKernelDone^[k] = 0 then
          begin
            AllKernelsDone := False;
            Break;
          end;
        //Sleep(1); //At least in here, if not in the real GPU code. It seams that task switching is too expensive, so leave commented.
      end;

      ADebuggingInfo^[0] := EnqKrnErr;

      if FGPUUseAllKernelsEvent then
        EnqMrkErr := enqueue_marker(SlaveQueue, 1, @AllKernelsEvent, @FinalEvent)  //when using AllKernelsEvent
      else
        EnqMrkErr := enqueue_marker(SlaveQueue, ASubBmpHeight, @AllEvents[0], @FinalEvent);

      ADebuggingInfo^[1] := EnqMrkErr;

      if FGPUUseEventsInEnqueueKernel then
      begin
        if FGPUUseAllKernelsEvent then
          release_event(AllKernelsEvent)  //when using AllKernelsEvent
        else
          for k := 0 to ASubBmpHeight - 1 do
            release_event(AllEvents[k]);

        if not FGPUReleaseFinalEventAtKernelEnd then
          release_event(FinalEvent);
      end;

      DifferentCount := 0;
      for k := 0 to ASubBmpHeight - 1 do
        Inc(DifferentCount, AResultedErrCount^[k]);

      TotalErrorCount := ATotalErrorCount;
      if DifferentCount < TotalErrorCount then
      begin
        Found := True;
        Break;
      end;

      if EnqKrnErr < 0 then
        Break;
    end;  //for j

    if Found or (EnqKrnErr < 0) then
      Break;
  end;  //for i

  ADebuggingInfo^[2] := i;
  ADebuggingInfo^[3] := j;
  ADebuggingInfo^[4] := DifferentCount;
  ADebuggingInfo^[5] := Integer(Found);
  ADebuggingInfo^[6] := get_work_dim;
  ADebuggingInfo^[7] := get_global_size(1);
  ADebuggingInfo^[8] := get_local_size(1);
  ADebuggingInfo^[9] := get_enqueued_local_size(1);
  ADebuggingInfo^[10] := ATotalErrorCount;
  ADebuggingInfo^[11] := Integer(EnqKrnErr);     //with typecast  - maybe it doesn't matter
  ADebuggingInfo^[12] := Integer(AllKernelsDone);

  if FGPUUseEventsInEnqueueKernel then
  begin
    //  release_event(AllKernelsEvent);         //TBD

    if FGPUReleaseFinalEventAtKernelEnd then
      release_event(FinalEvent);
  end;
end;

end.

