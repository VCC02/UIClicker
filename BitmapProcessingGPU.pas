{
    Copyright (C) 2026 VCC
    creation date: 14 Jan 2026 - code moved from BitmapProcessing.pas
    initial release date: 14 Jan 2026

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


unit BitmapProcessingGPU;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Graphics, ClickerUtils;


procedure RandomSleep(ASleepySearch: Byte);
function BitmapPosMatch_BruteForceOnGPU(ASrcBmpData, ASubBmpData: Pointer;
                                        ABytesPerPixelOnSrc, ABytesPerPixelOnSub: Integer;
                                        ASourceBitmapWidth, ASourceBitmapHeight, ASubBitmapWidth, ASubBitmapHeight: Integer;
                                        ColorErrorLevel: Integer;
                                        out SubCnvXOffset, SubCnvYOffset: Integer;
                                        var AFoundBitmaps: TCompRecArr;
                                        TotalErrorCount, FastSearchColorErrorCount: Integer;
                                        AUseFastSearch, AIgnoreBackgroundColor, AGetAllBitmaps: Boolean;
                                        ABackgroundColor: TColor;
                                        var AIgnoredColorsArr: TColorArr;
                                        ASleepySearch: Byte;
                                        AOutsideTickCount, APrecisionTimeout: QWord;
                                        AThreadCount: Integer;
                                        ACustomOpenCLPath: string;
                                        AGPUPlatformIndex, AGPUDeviceIndex: Integer;
                                        AGPUExecutionAvailability: TGPUExecutionAvailability;
                                        AGPUIncludeDashG: Boolean;
                                        AGPUSlaveQueueFromDevice: Boolean;
                                        AGPUUseAllKernelsEvent: Boolean;
                                        AGPUNdrangeNoLocalParam: Boolean;
                                        AGPUUseEventsInEnqueueKernel: Boolean;
                                        AGPUWaitForAllKernelsToBeDone: Boolean;
                                        AGPUReleaseFinalEventAtKernelEnd: Boolean;
                                        AGPUIgnoreExecutionAvailability: Boolean;
                                        var AGPUDbgBuffer: TIntArr;
                                        out AResultedErrorCount: Integer;
                                        AStopSearchOnDemand: PBoolean = nil;
                                        StopSearchOnMismatch: Boolean = True): Boolean;

implementation


uses
  ctypes, CLHeaders, Forms;


procedure RandomSleep(ASleepySearch: Byte);
begin
  if Random(8) = 7 then
  begin
    if ASleepySearch and 2 = 2 then     //this should be 0 when using threaded search
      Application.ProcessMessages;

    if ASleepySearch and 1 = 1 then
      Sleep(1);
  end;
end;


function GetKernelSrcRGB_MatCmp(ARGBSizeOnBG, ARGBSizeOnSub: Byte): string;
var
  RGBSizeStrOnBG, RGBSizeStrOnSub: string;
begin      //int is 32-bit, long is 64-bit
  if not (ARGBSizeOnBG in [3, 4]) or not (ARGBSizeOnSub in [3, 4]) then
  begin
    Result := 'Bad code';
    Exit;
  end;

  RGBSizeStrOnBG := IntToStr(ARGBSizeOnBG);
  RGBSizeStrOnSub := IntToStr(ARGBSizeOnSub);

  Result :=
    '__kernel void MatCmp(                      ' + #13#10 +
    '  __global uchar* ABackgroundBmp,          ' + #13#10 +
    '  __global uchar* ASubBmp,                 ' + #13#10 +
    '  __global int* AResultedErrCount,         ' + #13#10 +
    '  __global uchar* AKernelDone,             ' + #13#10 +
    '  const int ABackgroundWidth,     ' + #13#10 +
    '  const int ASubBmpWidth,         ' + #13#10 +
    '  const int ASubBmpHeight,        ' + #13#10 +    //After setting MatCmp as a slave kernel: not needed in this kernel, it is here for compatibility only (to have a similar list of parameters, to be able to directly call this kernel from host, if needed)
    '  const int AXOffset,             ' + #13#10 +
    '  const int AYOffset,             ' + #13#10 +
    '  const uchar AColorError,                 ' + #13#10 +
    '  const ulong ASlaveQueue)                 ' + #13#10 +    //After setting MatCmp as a slave kernel: not needed in this kernel, it is here for compatibility only ...
    '{                                          ' + #13#10 +
    '  int YIdx = get_global_id(0);             ' + #13#10 + //goes from 0 to SubBmpHeight - 1
    '  __global uchar const * BGRow = &ABackgroundBmp[((YIdx + AYOffset) * ABackgroundWidth + AXOffset) * ' + RGBSizeStrOnBG + '];' + #13#10 + //pointer to the current row, indexed by YIdx
    '  __global uchar const * SubRow = &ASubBmp[(YIdx * ASubBmpWidth) * ' + RGBSizeStrOnSub + '];' + #13#10 + //pointer to the current row, indexed by YIdx
    '  int ErrCount = 0;                             ' + #13#10 +
    '  for (int x = 0; x < ASubBmpWidth; x++)        ' + #13#10 +
    '  {                                             ' + #13#10 +
    '     int x0_BG = x * ' + RGBSizeStrOnBG + ' + 0;                        ' + #13#10 +
    '     int x1_BG = x * ' + RGBSizeStrOnBG + ' + 1;                        ' + #13#10 +
    '     int x2_BG = x * ' + RGBSizeStrOnBG + ' + 2;                        ' + #13#10 +
    '     int x0_Sub = x * ' + RGBSizeStrOnSub + ' + 0;                        ' + #13#10 +
    '     int x1_Sub = x * ' + RGBSizeStrOnSub + ' + 1;                        ' + #13#10 +
    '     int x2_Sub = x * ' + RGBSizeStrOnSub + ' + 2;                        ' + #13#10 +
    '     short SubPxB = SubRow[x0_Sub];             ' + #13#10 +
    '     short BGPxB = BGRow[x0_BG];                ' + #13#10 +
    '     short SubPxG = SubRow[x1_Sub];             ' + #13#10 +
    '     short BGPxG = BGRow[x1_BG];                ' + #13#10 +
    '     short SubPxR = SubRow[x2_Sub];             ' + #13#10 +
    '     short BGPxR = BGRow[x2_BG];                ' + #13#10 +
    '     if ((abs(SubPxR - BGPxR) > AColorError) || ' + #13#10 +
    '         (abs(SubPxG - BGPxG) > AColorError) || ' + #13#10 +
    '         (abs(SubPxB - BGPxB) > AColorError))   ' + #13#10 +
    '     {                                          ' + #13#10 +
    '       ErrCount++;                              ' + #13#10 +
    '     }  //if                                    ' + #13#10 +
    '  }  //for                                      ' + #13#10 +
    '  AResultedErrCount[YIdx] = ErrCount;           ' + #13#10 +
    '  AKernelDone[YIdx] = 1;                        ' + #13#10 +
    '}';
end;


function GetKernelSrcRGB_SlideSearch(ASubBmpHeightStr: string;
                                     AGPUSlaveQueueFromDevice: Boolean;
                                     AGPUUseAllKernelsEvent: Boolean;
                                     AGPUNdrangeNoLocalParam: Boolean;
                                     AGPUUseEventsInEnqueueKernel: Boolean;
                                     AGPUWaitForAllKernelsToBeDone: Boolean;
                                     AGPUReleaseFinalEventAtKernelEnd: Boolean
                                    ): string; //ASubBmpHeightStr should have the same value as ASubBmpHeight argument.
begin
  Result :=
    #13#10 +
    '__kernel void SlideSearch(                 ' + #13#10 +
    '  __global uchar* ABackgroundBmp,          ' + #13#10 +
    '  __global uchar* ASubBmp,                 ' + #13#10 +
    '  __global int* AResultedErrCount,         ' + #13#10 +
    '  __global int* ADebuggingInfo,            ' + #13#10 +
    '  __global uchar* AKernelDone,             ' + #13#10 +
    '  const int ABackgroundWidth,     ' + #13#10 +
    '  const int ASubBmpWidth,         ' + #13#10 +
    '  const int ASubBmpHeight,        ' + #13#10 +
    '  const int AXOffset,             ' + #13#10 +
    '  const int AYOffset,             ' + #13#10 +
    '  const uchar AColorError,                 ' + #13#10 +
    '  const ulong ASlaveQueue,                 ' + #13#10 +
    '  const int ATotalErrorCount)     ' + #13#10 +
    '{                                          ' + #13#10;

    if AGPUSlaveQueueFromDevice then
      Result := Result + '  queue_t SlaveQueue = get_default_queue();' + #13#10     //requires OpenCL >= 2.0 and __opencl_c_device_enqueue
    else                                         //get_default_queue can return nil if the default queue has not been created
      Result := Result + '  queue_t SlaveQueue = (queue_t)ASlaveQueue;' + #13#10;   //Default option of AGPUSlaveQueueFromDevice

    if AGPUUseAllKernelsEvent then
      Result := Result + '  clk_event_t AllKernelsEvent;             ' + #13#10       //when using AllKernelsEvent
    else
      Result := Result + '  clk_event_t AllEvents[' + ASubBmpHeightStr + '];' + #13#10;  //Constrained array of the same length as the 1D range, i.e. the sub BMP height.

    Result := Result +
    '  clk_event_t FinalEvent;                  ' + #13#10 + //used as a return event, from enqueue_marker
    '                                           ' + #13#10;

    if not AGPUUseAllKernelsEvent then
    begin
      Result := Result +
      '  for (k = 0; k < ASubBmpHeight; k++)      ' + #13#10 +
      '    AllEvents[k] = NULL;                   ' + #13#10#13#10;   //init here, so that enqueue_marker will return an error on null poiters instead of uninitialized pointers
    end;

    if AGPUNdrangeNoLocalParam then
      Result := Result + '  ndrange_t ndrange = ndrange_1D(ASubBmpHeight);' + #13#10
    else
      Result := Result + '  ndrange_t ndrange = ndrange_1D(1, ASubBmpHeight);' + #13#10;   //default

    Result := Result +
    '  kernel_enqueue_flags_t MyFlags;          ' + #13#10 +
    '  MyFlags = CLK_ENQUEUE_FLAGS_NO_WAIT;     ' + #13#10 +
    '  int i, j, k = 0;                         ' + #13#10 +
    '  bool Found = false;                      ' + #13#10 +
    '  bool AllKernelsDone = false;             ' + #13#10 +
    '  int EnqKrnErr = -1234;                   ' + #13#10 +
    '  int EnqMrkErr = -4567;                   ' + #13#10 +
    '  int XOffset = AXOffset;                  ' + #13#10 +
    '  int YOffset = AYOffset;                  ' + #13#10 +
    '  int DifferentCount = 0;                  ' + #13#10 +
    '  int WhileIterations = 0;                 ' + #13#10 +

    '  for (i = 0; i < AYOffset; i++)           ' + #13#10 +
    '  {                                        ' + #13#10 +
    '    for (j = 0; j < AXOffset; j++)         ' + #13#10 +
    '    {                                      ' + #13#10 +
    '      for (k = 0; k < ASubBmpHeight; k++)  ' + #13#10 +
    '        AKernelDone[k] = 0;                ' + #13#10 +
    ''                                            + #13#10;

    //if not AGPUUseAllKernelsEvent then   //ndrange should return a ASubBmpHeight wide range. Then, it makes no sense to call enqueue_kernel multiple times.
    //  Result := Result + '      for (k = 0; k < ASubBmpHeight; k++)  ' + #13#10;    //this line is used only when calling with AllEvents, and should not exist when calling with AllKernelsEvent

    Result := Result +
    '      EnqKrnErr = enqueue_kernel(          ' + #13#10 +
    '        SlaveQueue,                        ' + #13#10 +      //using SlaveQueue, instead of get_default_queue()
    '        MyFlags,                           ' + #13#10 +      //enqueue_kernel is commented, because using the default queue, messes up the object, so that the clFinish(SlaveCmdQueue) call returns an error.
    '        ndrange,                           ' + #13#10;

    if AGPUUseEventsInEnqueueKernel then     //False by default
    begin
      Result := Result +
      '        0,            //comment for err -10' + #13#10 +
      '        NULL,         //comment for err -10' + #13#10;

      if AGPUUseAllKernelsEvent then
        Result := Result + '        &AllKernelsEvent, //comment for err -10' + #13#10
      else
        //Result := Result + '        &AllEvents[k], //comment for err -10' + #13#10;
      Result := Result + '        &AllEvents, //comment for err -10' + #13#10;
    end;

    Result := Result +
    '        ^{MatCmp(ABackgroundBmp, ASubBmp, AResultedErrCount, AKernelDone, ABackgroundWidth, ASubBmpWidth, ASubBmpHeight, j, i, AColorError, SlaveQueue);});                  ' + #13#10;

    if AGPUWaitForAllKernelsToBeDone then
    begin
      Result := Result +
      '      if (EnqKrnErr == 0)                  ' + #13#10 +   //do not wait in case of an error
      '      {                                    ' + #13#10 +
      '        //wait for all kernels to be done  ' + #13#10 +
      '        AllKernelsDone = false;            ' + #13#10 +
      '        WhileIterations = 0;               ' + #13#10 +
      '        while (!AllKernelsDone)            ' + #13#10 +
      '        {                                  ' + #13#10 +
      '          WhileIterations++;               ' + #13#10 +
      '          AllKernelsDone = true;           ' + #13#10 +
      '          for (k = 0; k < ASubBmpHeight; k++)' + #13#10 +
      '            if (AKernelDone[k] == 0)       ' + #13#10 +
      '            {                              ' + #13#10 +
      '              AllKernelsDone = false;      ' + #13#10 +
      '              break;                       ' + #13#10 +
      '            }                              ' + #13#10 +
      '          //it would be nice to have a sleep call here' + #13#10 +
      '        } //while                          ' + #13#10 +
      '      } //if (EnqKrnErr == 0)              ' + #13#10;
    end;

    Result := Result +
      '      ADebuggingInfo[0] = EnqKrnErr;       ' + #13#10;

    if AGPUUseEventsInEnqueueKernel then     //False by default
    begin
      Result := Result +
      '      if (EnqKrnErr == 0)                  ' + #13#10 +   //do not wait in case of an error
      '      {                                    ' + #13#10;

      if AGPUUseAllKernelsEvent then
        Result := Result + '        EnqMrkErr = enqueue_marker(SlaveQueue, 1, AllKernelsEvent, &FinalEvent);' + #13#10    //when using AllKernelsEvent
      else
        Result := Result + '        EnqMrkErr = enqueue_marker(SlaveQueue, ASubBmpHeight, AllEvents, &FinalEvent);' + #13#10;

      Result := Result +
      '      } //if (EnqKrnErr == 0)              ' + #13#10;
    end;

    Result := Result +
    //
    '      ADebuggingInfo[1] = EnqMrkErr;       ' + #13#10;

    if AGPUUseEventsInEnqueueKernel then      //False by default
    begin
      Result := Result +
      '      if (EnqKrnErr == 0)                  ' + #13#10 +   //do not release in case of an error
      '      {                                    ' + #13#10;

      if AGPUUseAllKernelsEvent then
        Result := Result +'          release_event(AllKernelsEvent);    ' + #13#10    //when using AllKernelsEvent
      else
        Result := Result +
          '        for (k = 0; k < ASubBmpHeight; k++)  ' + #13#10 +
          '          release_event(AllEvents[k]);       ' + #13#10;

      if not AGPUReleaseFinalEventAtKernelEnd then    //False by default
        Result := Result +
        '        if (EnqMrkErr == 0)                    ' + #13#10 +  //this is EnqMrkErr, for releasing FinalEvent
        '          release_event(FinalEvent);           ' + #13#10;

      Result := Result +
      '      } //if (EnqKrnErr == 0)              ' + #13#10;
    end;

    Result := Result +
    ''                                            + #13#10 +
    '      DifferentCount = 0;              ' + #13#10 +     //collect the results from all slave kernels
    '      for (k = 0; k < ASubBmpHeight; k++)  ' + #13#10 +
    '        DifferentCount += AResultedErrCount[k];' + #13#10 +
    ''                                            + #13#10 +
    '      int TotalErrorCount = ATotalErrorCount;' + #13#10 +
    '      if (DifferentCount < TotalErrorCount)' + #13#10 +
    '      {                                    ' + #13#10 +
    '        Found = true;                      ' + #13#10 +
    '        break;'                              + #13#10 +     //commented, together with for i, for j
    '      }'                                     + #13#10 +
    ''                                            + #13#10 +
    '      if (EnqKrnErr < 0)                   ' + #13#10 +     //commented, together with for i, for j
    '      {                                    ' + #13#10 +
    '        break;'                              + #13#10 +
    '      }'                                     + #13#10 +
    '    }' + #13#10 + //for j
    '    if (Found || (EnqKrnErr < 0))          ' + #13#10 +
    '      break;'                                + #13#10 +
    '  }' + #13#10 + //for i                                     //commented, together with for i, for j
    ''                                            + #13#10 +
    '  ADebuggingInfo[2] = i;                   ' + #13#10 +
    '  ADebuggingInfo[3] = j;                   ' + #13#10 +  //returns 1 when EnqKrnErr is -10. This means that the first "for j" iteration is ok, then it errors.
    '  ADebuggingInfo[4] = DifferentCount;      ' + #13#10 +  //returns 97 for a single enqueue_kernel call, with i = 0 and j = 0 and waiting for all kernels to be done (with while loop).
    '  ADebuggingInfo[5] = (int)Found;          ' + #13#10 +
    '  ADebuggingInfo[6] = get_work_dim();      ' + #13#10 +  //returns 1
    '  ADebuggingInfo[7] = get_global_size(1);  ' + #13#10 +  //returns 1
    '  ADebuggingInfo[8] = get_local_size(1);   ' + #13#10 +  //returns 1
    '  ADebuggingInfo[9] = get_enqueued_local_size(1);' + #13#10 +  //returns 1
    '  ADebuggingInfo[10] = ATotalErrorCount;   ' + #13#10 +     //returns 30 for a single enqueue_kernel call, with i = 0 and j = 0.
    '  ADebuggingInfo[11] = (int)EnqKrnErr + 3000;' + #13#10 +  //with typecast  - maybe it doesn't matter   //returns 3000, for a single enqueue_kernel call, without events and with/without clFinish call in host.  Returns 2990 (= -10 + 3000), when using for i, for j and waiting in while loop.
    '  ADebuggingInfo[12] = (int)AllKernelsDone;' + #13#10 +   //returns 1 for a single enqueue_kernel call, with i = 0 and j = 0 and waiting for all kernels to be done (with while loop).
    '  ADebuggingInfo[13] = WhileIterations;    ' + #13#10;

    if AGPUUseEventsInEnqueueKernel then    //False by default
    begin
      Result := Result +
      '      if (EnqKrnErr == 0)                  ' + #13#10 +   //do not release in case of an error
      '      {                                    ' + #13#10;

      //Result := Result + '  release_event(AllKernelsEvent);          ' + #13#10;  //TBD

      if AGPUReleaseFinalEventAtKernelEnd then   //False by default
      begin
        Result := Result +
        '        if (EnqMrkErr == 0)                    ' + #13#10;  //this is EnqMrkErr, for releasing FinalEvent
        Result := Result + '  release_event(FinalEvent);               ' + #13#10;  //Don't change the indentation here. It is used by a test file, to get value of GPUReleaseFinalEventAtKernelEnd option.
      end;

      Result := Result +
      '      } //if (EnqKrnErr == 0)              ' + #13#10;
    end;

    Result := Result +
    '}'                                           + #13#10
    ;
end;


procedure LogCallResult(AError: Integer; AFuncName, AInfo: string; AExtraErrorInfo: string = '');
var
  Msg: string;
begin
  if AError = 0 then
    Exit;

  while Pos(#0, AExtraErrorInfo) > 0 do
  begin
    Delete(AExtraErrorInfo, Pos(#0, AExtraErrorInfo), 1);
    if Length(AExtraErrorInfo) = 0 then
      Break;
  end;

  if Pos(#0, AInfo) > 0 then
    Delete(AInfo, Pos(#0, AInfo), 1);

  Msg := 'Error ' + CLErrorToStr(AError) + ' " at "' + AFuncName + '" OpenCL API call.  ' + AExtraErrorInfo;
  if AInfo <> '' then
    Msg := Msg + '  Expected: ' + AInfo;

  raise Exception.Create(Msg);
end;


function BruteForceOnGPU_MatCmp(OpenCLDll: TOpenCL;
                                CLKernel: cl_kernel;
                                AContext: cl_context;
                                CmdQueue, SlaveCmdQueue: cl_command_queue;
                                ASrcBmpData, ASubBmpData: Pointer;
                                ABytesPerPixelOnSrc, ABytesPerPixelOnSub: Integer;
                                ASourceBitmapWidth, ASourceBitmapHeight, ASubBitmapWidth, ASubBitmapHeight: Integer;
                                ColorErrorLevel: Integer;
                                TotalErrorCount, FastSearchColorErrorCount: Integer;
                                AUseFastSearch, AIgnoreBackgroundColor, AGetAllBitmaps: Boolean;
                                ASleepySearch: Byte;
                                AOutsideTickCount, APrecisionTimeout: QWord;
                                out AResultedErrorCount: Integer;
                                out SubCnvXOffset, SubCnvYOffset: Integer;
                                var AFoundBitmaps: TCompRecArr;
                                var ADbgBuffer: TIntArr;
                                AStopSearchOnDemand: PBoolean = nil
                               ): Boolean;
var
  Error: Integer;

  BackgroundBufferRef: cl_mem;
  SubBufferRef: cl_mem;
  ResBufferRef: cl_mem;
  KernelDoneBufferRef: cl_mem;

  i, j, k: Integer;
  BackgroundBmpWidth, BackgroundBmpHeight: Integer;
  SubBmpWidth, SubBmpHeight: Integer;
  XOffset, YOffset: Integer;
  ColorError: Byte;
  GlobalSize: csize_t;
  ShouldStop: Boolean;

  DiffCntPerRow: array of LongInt;
  DifferentCount: LongInt;
begin
  Result := False;

  BackgroundBmpWidth := ASourceBitmapWidth;
  BackgroundBmpHeight := ASourceBitmapHeight;
  SubBmpWidth := ASubBitmapWidth;
  SubBmpHeight := ASubBitmapHeight;
  SetLength(ADbgBuffer, 0);

  BackgroundBufferRef := OpenCLDll.clCreateBuffer(AContext, CL_MEM_READ_ONLY, csize_t(ABytesPerPixelOnSrc * BackgroundBmpWidth * BackgroundBmpHeight), nil, Error);
  try
    LogCallResult(Error, 'clCreateBuffer', 'Background buffer created.');

    SubBufferRef := OpenCLDll.clCreateBuffer(AContext, CL_MEM_READ_ONLY, csize_t(ABytesPerPixelOnSub * SubBmpWidth * SubBmpHeight), nil, Error);
    try
      LogCallResult(Error, 'clCreateBuffer', 'Sub buffer created.');

      ResBufferRef := OpenCLDll.clCreateBuffer(AContext, CL_MEM_WRITE_ONLY, csize_t(SizeOf(LongInt) * SubBmpHeight), nil, Error);
      try
        LogCallResult(Error, 'clCreateBuffer', 'Res buffer created.');

        KernelDoneBufferRef := OpenCLDll.clCreateBuffer(AContext, CL_MEM_WRITE_ONLY, csize_t(SizeOf(LongInt) * SubBmpHeight), nil, Error);
        try
          Error := OpenCLDll.clEnqueueWriteBuffer(CmdQueue, BackgroundBufferRef, CL_TRUE, 0, csize_t(ABytesPerPixelOnSrc * BackgroundBmpWidth * BackgroundBmpHeight), ASrcBmpData, 0, nil, nil);
          LogCallResult(Error, 'clEnqueueWriteBuffer', 'Background buffer written.');

          Error := OpenCLDll.clEnqueueWriteBuffer(CmdQueue, SubBufferRef, CL_TRUE, 0, csize_t(ABytesPerPixelOnSub * SubBmpWidth * SubBmpHeight), ASubBmpData, 0, nil, nil);
          LogCallResult(Error, 'clEnqueueWriteBuffer', 'Sub buffer written.');

          XOffset := 0;
          YOffset := 0;
          ColorError := ColorErrorLevel;

          Error := OpenCLDll.clSetKernelArg(CLKernel, 0, SizeOf(cl_mem), @BackgroundBufferRef); //sizeof(cl_mem)  is SizeOf(Pointer), which can be 4 or 8
          LogCallResult(Error, 'clSetKernelArg', 'BackgroundBufferRef argument set.');

          Error := OpenCLDll.clSetKernelArg(CLKernel, 1, SizeOf(cl_mem), @SubBufferRef); //sizeof(cl_mem)  is SizeOf(Pointer), which can be 4 or 8
          LogCallResult(Error, 'clSetKernelArg', 'SubBufferRef argument set.');

          Error := OpenCLDll.clSetKernelArg(CLKernel, 2, SizeOf(cl_mem), @ResBufferRef); //sizeof(cl_mem)  is SizeOf(Pointer), which can be 4 or 8
          LogCallResult(Error, 'clSetKernelArg', 'ResBufferRef argument set.');

          Error := OpenCLDll.clSetKernelArg(CLKernel, 3, SizeOf(cl_mem), @KernelDoneBufferRef); //sizeof(cl_mem)  is SizeOf(Pointer), which can be 4 or 8
          LogCallResult(Error, 'clSetKernelArg', 'KernelDoneBufferRef argument set.');

          Error := OpenCLDll.clSetKernelArg(CLKernel, 4, SizeOf(LongInt), @BackgroundBmpWidth);
          LogCallResult(Error, 'clSetKernelArg', 'ABackgroundWidth argument set.');

          Error := OpenCLDll.clSetKernelArg(CLKernel, 5, SizeOf(LongInt), @SubBmpWidth);
          LogCallResult(Error, 'clSetKernelArg', 'ASubBmpWidth argument set.');

          Error := OpenCLDll.clSetKernelArg(CLKernel, 6, SizeOf(LongInt), @SubBmpHeight);
          LogCallResult(Error, 'clSetKernelArg', 'SubBmpHeight argument set.');

          //Error := OpenCLDll.clSetKernelArg(CLKernel, 7, SizeOf(LongInt), @XOffset);
          //LogCallResult(Error, 'clSetKernelArg', 'XOffset argument set.');
          //
          //Error := OpenCLDll.clSetKernelArg(CLKernel, 8, SizeOf(LongInt), @YOffset);
          //LogCallResult(Error, 'clSetKernelArg', 'YOffset argument set.');

          //XOffset := BackgroundBmpWidth - SubBmpWidth - 1;                 //this is the max value of XOffset - will be used for slave kernel
          //Error := OpenCLDll.clSetKernelArg(CLKernel, 7, SizeOf(LongInt), @XOffset);
          //LogCallResult(Error, 'clSetKernelArg', 'XOffset argument set.');
          //
          //YOffset := BackgroundBmpHeight - SubBmpHeight - 1;               //this is the max value of YOffset - will be used for slave kernel
          //Error := OpenCLDll.clSetKernelArg(CLKernel, 8, SizeOf(LongInt), @YOffset);
          //LogCallResult(Error, 'clSetKernelArg', 'YOffset argument set.');

          Error := OpenCLDll.clSetKernelArg(CLKernel, 9, SizeOf(Byte), @ColorError);
          LogCallResult(Error, 'clSetKernelArg', 'ColorError argument set.');

          Error := OpenCLDll.clSetKernelArg(CLKernel, 10, SizeOf(cl_ulong), @SlaveCmdQueue);  //using SizeOf(cl_ulong), because the parameter is a QWord on kernel
          LogCallResult(Error, 'clSetKernelArg', 'SlaveCmdQueue argument set.');  //This was plain SlaveCmdQueue, instead of @SlaveCmdQueue.

          GlobalSize := SubBmpHeight;
          LogCallResult(Error, 'Matrix comparison', 'Starting...');

          ShouldStop := False;
          SetLength(DiffCntPerRow, GlobalSize);
          for i := 0 to BackgroundBmpHeight - SubBmpHeight - 1 do       //these two for loops are implemented in SlideSearch kernel
          begin
            for j := 0 to BackgroundBmpWidth - SubBmpWidth - 1 do
            begin
              XOffset := j;
              YOffset := i;

              Error := OpenCLDll.clSetKernelArg(CLKernel, 7, SizeOf(LongInt), @XOffset);
              LogCallResult(Error, 'clSetKernelArg', '');

              Error := OpenCLDll.clSetKernelArg(CLKernel, 8, SizeOf(LongInt), @YOffset);
              LogCallResult(Error, 'clSetKernelArg', '');

              Error := OpenCLDll.clEnqueueNDRangeKernel(CmdQueue, CLKernel, 1, nil, @GlobalSize, nil, 0, nil, nil);
              LogCallResult(Error, 'clEnqueueNDRangeKernel', '');

              Error := OpenCLDll.clFinish(CmdQueue);
              LogCallResult(Error, 'clFinish', '');

              Error := OpenCLDll.clEnqueueReadBuffer(CmdQueue, ResBufferRef, CL_TRUE, 0, csize_t(SizeOf(LongInt) * GlobalSize), @DiffCntPerRow[0], 0, nil, nil);
              LogCallResult(Error, 'clEnqueueReadBuffer', '');

              DifferentCount := 0;
              for k := 0 to GlobalSize - 1 do //results len
                if DiffCntPerRow[k] >= 0 then
                  Inc(DifferentCount, DiffCntPerRow[k]);

              if DifferentCount < TotalErrorCount then
              begin
                Result := True;
                AResultedErrorCount := DifferentCount;
                SubCnvXOffset := XOffset;
                SubCnvYOffset := YOffset;
                SetLength(AFoundBitmaps, Length(AFoundBitmaps) + 1);
                AFoundBitmaps[Length(AFoundBitmaps) - 1].XOffsetFromParent := XOffset;
                AFoundBitmaps[Length(AFoundBitmaps) - 1].YOffsetFromParent := YOffset;
                AFoundBitmaps[Length(AFoundBitmaps) - 1].ResultedErrorCount := AResultedErrorCount;

                if not AGetAllBitmaps then
                  ShouldStop := True;  //stop only if a single bitmap result is expected

                if ShouldStop then
                  Break;
              end;

              if ShouldStop then
                Break;

              if (AStopSearchOnDemand <> nil) and AStopSearchOnDemand^ then
              begin
                ShouldStop := True;
                Break;
              end;

              if (AOutsideTickCount > 0) and (GetTickCount64 - AOutsideTickCount > APrecisionTimeout) then
                raise EBmpMatchTimeout.Create('PrecisionTimeout on searching for SubControl.'); //Exit;
            end; //for j

            if DifferentCount < TotalErrorCount then
              Break;

            if (AOutsideTickCount > 0) and (GetTickCount64 - AOutsideTickCount > APrecisionTimeout) then
              Break;

            RandomSleep(ASleepySearch);
            Application.ProcessMessages;
          end; //for i
        finally
          OpenCLDll.clReleaseMemObject(KernelDoneBufferRef);
        end;
      finally
        OpenCLDll.clReleaseMemObject(ResBufferRef);
      end;
    finally
      OpenCLDll.clReleaseMemObject(SubBufferRef);
    end;
  finally
    OpenCLDll.clReleaseMemObject(BackgroundBufferRef);
  end;
end;


function BruteForceOnGPU_SlideSearch(OpenCLDll: TOpenCL;
                                     CLKernel: cl_kernel;
                                     AContext: cl_context;
                                     CmdQueue, SlaveCmdQueue: cl_command_queue;
                                     ASrcBmpData, ASubBmpData: Pointer;
                                     ABytesPerPixelOnSrc, ABytesPerPixelOnSub: Integer;
                                     ASourceBitmapWidth, ASourceBitmapHeight, ASubBitmapWidth, ASubBitmapHeight: Integer;
                                     AColorErrorLevel: Integer;
                                     ATotalErrorCount, FastSearchColorErrorCount: Integer;
                                     AUseFastSearch, AIgnoreBackgroundColor, AGetAllBitmaps: Boolean;
                                     ASleepySearch: Byte;
                                     AOutsideTickCount, APrecisionTimeout: QWord;
                                     out AResultedErrorCount: Integer;
                                     out SubCnvXOffset, SubCnvYOffset: Integer;
                                     var AFoundBitmaps: TCompRecArr;
                                     var ADbgBuffer: TIntArr;
                                     AStopSearchOnDemand: PBoolean = nil
                                    ): Boolean;
var
  Error: Integer;
  DiffCntPerRow: TIntArr;//TIntArr is array of LongInt;

  BackgroundBufferRef: cl_mem;
  SubBufferRef: cl_mem;
  ResBufferRef: cl_mem;
  DbgBufferRef: cl_mem;
  KernelDoneBufferRef: cl_mem;

  BackgroundBmpWidth, BackgroundBmpHeight: Integer;
  SubBmpWidth, SubBmpHeight: Integer;
  XOffset, YOffset: Integer;
  ColorError: Byte;
  GlobalSize, GlobalSizeWithDeviceEnqueue: csize_t;
  DifferentCount: LongInt; //same type as DbgBuffer items
  Found: Boolean;
begin
  Result := False;

  BackgroundBmpWidth := ASourceBitmapWidth;
  BackgroundBmpHeight := ASourceBitmapHeight;
  SubBmpWidth := ASubBitmapWidth;
  SubBmpHeight := ASubBitmapHeight;

  BackgroundBufferRef := OpenCLDll.clCreateBuffer(AContext, CL_MEM_READ_ONLY, csize_t(ABytesPerPixelOnSrc * BackgroundBmpWidth * BackgroundBmpHeight), nil, Error);
  try
    LogCallResult(Error, 'clCreateBuffer', 'Background buffer created.');

    SubBufferRef := OpenCLDll.clCreateBuffer(AContext, CL_MEM_READ_ONLY, csize_t(ABytesPerPixelOnSub * SubBmpWidth * SubBmpHeight), nil, Error);
    try
      LogCallResult(Error, 'clCreateBuffer', 'Sub buffer created.');

      ResBufferRef := OpenCLDll.clCreateBuffer(AContext, CL_MEM_WRITE_ONLY, csize_t(SizeOf(LongInt) * SubBmpHeight), nil, Error);
      try
        LogCallResult(Error, 'clCreateBuffer', 'Res buffer created.');

        SetLength(ADbgBuffer, 20);
        DbgBufferRef := OpenCLDll.clCreateBuffer(AContext, CL_MEM_WRITE_ONLY, csize_t(SizeOf(LongInt) * Length(ADbgBuffer)), nil, Error); //20 items
        try
          LogCallResult(Error, 'clCreateBuffer', 'Dbg buffer created.');

          KernelDoneBufferRef := OpenCLDll.clCreateBuffer(AContext, CL_MEM_WRITE_ONLY, csize_t(SubBmpHeight), nil, Error);
          try
            LogCallResult(Error, 'clCreateBuffer', 'KernelDoneBufferRef buffer created.');

            Error := OpenCLDll.clEnqueueWriteBuffer(CmdQueue, BackgroundBufferRef, CL_TRUE, 0, csize_t(ABytesPerPixelOnSrc * BackgroundBmpWidth * BackgroundBmpHeight), ASrcBmpData, 0, nil, nil);
            LogCallResult(Error, 'clEnqueueWriteBuffer', 'Background buffer written.');

            Error := OpenCLDll.clEnqueueWriteBuffer(CmdQueue, SubBufferRef, CL_TRUE, 0, csize_t(ABytesPerPixelOnSub * SubBmpWidth * SubBmpHeight), ASubBmpData, 0, nil, nil);
            LogCallResult(Error, 'clEnqueueWriteBuffer', 'Sub buffer written.');

            XOffset := 0;
            YOffset := 0;
            ColorError := AColorErrorLevel;


            Error := OpenCLDll.clSetKernelArg(CLKernel, 0, SizeOf(cl_mem), @BackgroundBufferRef); //sizeof(cl_mem)  is SizeOf(Pointer), which can be 4 or 8
            LogCallResult(Error, 'clSetKernelArg', 'BackgroundBufferRef argument set.');

            Error := OpenCLDll.clSetKernelArg(CLKernel, 1, SizeOf(cl_mem), @SubBufferRef); //sizeof(cl_mem)  is SizeOf(Pointer), which can be 4 or 8
            LogCallResult(Error, 'clSetKernelArg', 'SubBufferRef argument set.');

            Error := OpenCLDll.clSetKernelArg(CLKernel, 2, SizeOf(cl_mem), @ResBufferRef); //sizeof(cl_mem)  is SizeOf(Pointer), which can be 4 or 8
            LogCallResult(Error, 'clSetKernelArg', 'ResBufferRef argument set.');

            Error := OpenCLDll.clSetKernelArg(CLKernel, 3, SizeOf(cl_mem), @DbgBufferRef); //sizeof(cl_mem)  is SizeOf(Pointer), which can be 4 or 8
            LogCallResult(Error, 'clSetKernelArg', 'DbgBufferRef argument set.');

            Error := OpenCLDll.clSetKernelArg(CLKernel, 4, SizeOf(cl_mem), @KernelDoneBufferRef); //sizeof(cl_mem)  is SizeOf(Pointer), which can be 4 or 8
            LogCallResult(Error, 'clSetKernelArg', 'KernelDoneBufferRef argument set.');

            Error := OpenCLDll.clSetKernelArg(CLKernel, 5, SizeOf(LongInt), @BackgroundBmpWidth);
            LogCallResult(Error, 'clSetKernelArg', 'ABackgroundWidth argument set.');

            Error := OpenCLDll.clSetKernelArg(CLKernel, 6, SizeOf(LongInt), @SubBmpWidth);
            LogCallResult(Error, 'clSetKernelArg', 'ASubBmpWidth argument set.');

            Error := OpenCLDll.clSetKernelArg(CLKernel, 7, SizeOf(LongInt), @SubBmpHeight);
            LogCallResult(Error, 'clSetKernelArg', 'SubBmpHeight argument set.');

            XOffset := BackgroundBmpWidth - SubBmpWidth - 1;
            Error := OpenCLDll.clSetKernelArg(CLKernel, 8, SizeOf(LongInt), @XOffset);
            LogCallResult(Error, 'clSetKernelArg', 'XOffset argument set.');

            YOffset := BackgroundBmpHeight - SubBmpHeight - 1;
            Error := OpenCLDll.clSetKernelArg(CLKernel, 9, SizeOf(LongInt), @YOffset);
            LogCallResult(Error, 'clSetKernelArg', 'YOffset argument set.');

            Error := OpenCLDll.clSetKernelArg(CLKernel, 10, SizeOf(Byte), @ColorError);
            LogCallResult(Error, 'clSetKernelArg', 'ColorError argument set.');

            Error := OpenCLDll.clSetKernelArg(CLKernel, 11, SizeOf(cl_ulong), @SlaveCmdQueue);  //using SizeOf(cl_ulong), because the parameter is a QWord on kernel
            LogCallResult(Error, 'clSetKernelArg', 'SlaveCmdQueue argument set.');  //This was plain SlaveCmdQueue, instead of @SlaveCmdQueue.

            Error := OpenCLDll.clSetKernelArg(CLKernel, 12, SizeOf(ATotalErrorCount), @ATotalErrorCount);
            LogCallResult(Error, 'clSetKernelArg', 'TotalErrorCount argument set.');

            GlobalSize := SubBmpHeight;
            LogCallResult(Error, 'Matrix comparison', 'Starting...');

            GlobalSizeWithDeviceEnqueue := 1; //one master kernel, although not sure if Local should be 1
            Error := OpenCLDll.clEnqueueNDRangeKernel(CmdQueue, CLKernel, 1, nil, @GlobalSizeWithDeviceEnqueue, nil, 0, nil, nil);
            LogCallResult(Error, 'clEnqueueNDRangeKernel CmdQueue', '');

            Error := OpenCLDll.clFinish(CmdQueue);     //see also ResBufferRef := OpenCLDll.clCreateBuffer  - the buffer is created two items longer
            LogCallResult(Error, 'clFinish CmdQueue (Before clEnqueueReadBuffer)', '');
            SetLength(DiffCntPerRow, GlobalSize);
            Error := OpenCLDll.clEnqueueReadBuffer(CmdQueue, ResBufferRef, CL_TRUE, 0, csize_t(SizeOf(LongInt) * Length(DiffCntPerRow)), @DiffCntPerRow[0], 0, nil, nil);
            LogCallResult(Error, 'clEnqueueReadBuffer DiffCntPerRow', '', '  kernel enqueue err is DiffCntPerRow[Len-2] = ' + IntToStr(DiffCntPerRow[Length(DiffCntPerRow) - 2]) + '  kernel get_default_queue is DiffCntPerRow[Len-1] = ' + IntToStr(DiffCntPerRow[Length(DiffCntPerRow) - 1]) + '  Length(DiffCntPerRow) = ' + IntToStr(Length(DiffCntPerRow)));

            //AddToLog('ErrCount:' + #13#10 +
            //         '  ResultedErrCount[0] = ' + IntToStr(DiffCntPerRow[0]) + #13#10 +
            //         '  ResultedErrCount[1] = ' + IntToStr(DiffCntPerRow[1]) + #13#10 +
            //         '  ResultedErrCount[2] = ' + IntToStr(DiffCntPerRow[2]) + #13#10 +
            //         '  ResultedErrCount[n - 4] = ' + IntToStr(DiffCntPerRow[Length(DiffCntPerRow) - 4]) + #13#10 +
            //         '  ResultedErrCount[n - 3] = ' + IntToStr(DiffCntPerRow[Length(DiffCntPerRow) - 3]) + #13#10 +
            //         '  ResultedErrCount[n - 2] = ' + IntToStr(DiffCntPerRow[Length(DiffCntPerRow) - 2]) + #13#10 +
            //         '  ResultedErrCount[n - 1] = ' + IntToStr(DiffCntPerRow[Length(DiffCntPerRow) - 1]));

            FillChar(ADbgBuffer[0], Length(ADbgBuffer), SizeOf(ADbgBuffer[0]));
            Error := OpenCLDll.clEnqueueReadBuffer(CmdQueue, DbgBufferRef, CL_TRUE, 0, csize_t(SizeOf(LongInt) * Length(ADbgBuffer)), @ADbgBuffer[0], 0, nil, nil);
            LogCallResult(Error, 'clEnqueueReadBuffer DbgBuffer', '', '  kernel enqueue err is ' + IntToStr(ADbgBuffer[0]) + '  kernel enqueue_marker is ' + IntToStr(DiffCntPerRow[1]) + '  Length(DbgBuffer) = ' + IntToStr(Length(ADbgBuffer)));

            //AddToLog('Misc info:' + #13#10 +
            //         '  enqueue_kernel = ' + IntToStr(ADbgBuffer[0]) + #13#10 +
            //         '  enqueue_marker = ' + IntToStr(ADbgBuffer[1]) + #13#10 +
            //         '  i = ' + IntToStr(ADbgBuffer[2]) + #13#10 +
            //         '  j = ' + IntToStr(ADbgBuffer[3]) + #13#10 +
            //         '  DifferentCount = ' + IntToStr(ADbgBuffer[4]) + #13#10 +
            //         '  Found = ' + IntToStr(ADbgBuffer[5]) + #13#10 +
            //         '  get_work_dim on "SlideSearch" = ' + IntToStr(ADbgBuffer[6]) + #13#10 +
            //         '  get_global_size on "SlideSearch" = ' + IntToStr(ADbgBuffer[7]) + #13#10 +
            //         '  get_local_size on "SlideSearch" = ' + IntToStr(ADbgBuffer[8]) + #13#10 +
            //         '  get_enqueued_local_size on "SlideSearch" = ' + IntToStr(ADbgBuffer[9]) + #13#10 +
            //         '  TotalErrorCount = ' + IntToStr(ADbgBuffer[10]) + #13#10 +
            //         '  ADbgBuffer[11] = ' + IntToStr(ADbgBuffer[11]) + #13#10 +
            //         '  ADbgBuffer[12] = ' + IntToStr(ADbgBuffer[12]) + #13#10 +
            //         '  ADbgBuffer[13] = ' + IntToStr(ADbgBuffer[13]) + #13#10 +
            //         '  ADbgBuffer[14] = ' + IntToStr(ADbgBuffer[14]) + #13#10 +
            //         '  ADbgBuffer[15] = ' + IntToStr(ADbgBuffer[15]) + #13#10 +
            //         '  ADbgBuffer[16] = ' + IntToStr(ADbgBuffer[16]) + #13#10 +
            //         '  ADbgBuffer[17] = ' + IntToStr(ADbgBuffer[17]) + #13#10 +
            //         '  ADbgBuffer[18] = ' + IntToStr(ADbgBuffer[18]) + #13#10 +
            //         '  ADbgBuffer[19] = ' + IntToStr(ADbgBuffer[19]) + #13#10
            //        );

            //Error := OpenCLDll.clFinish(SlaveCmdQueue);                     //CL_INVALID_COMMAND_QUEUE if command_queue is not a valid host command-queue.
            //LogCallResult(Error, 'clFinish SlaveCmdQueue', '');

            DifferentCount := ADbgBuffer[4];
            Found := ADbgBuffer[5] > 0;

            if Found then
            begin
              Result := True;
              AResultedErrorCount := DifferentCount;
              SubCnvXOffset := ADbgBuffer[3];  //j   //old XOffset
              SubCnvYOffset := ADbgBuffer[2];  //i  //old YOffset
              SetLength(AFoundBitmaps, Length(AFoundBitmaps) + 1);
              AFoundBitmaps[Length(AFoundBitmaps) - 1].XOffsetFromParent := SubCnvXOffset; //old XOffset
              AFoundBitmaps[Length(AFoundBitmaps) - 1].YOffsetFromParent := SubCnvYOffset; //old YOffset
              AFoundBitmaps[Length(AFoundBitmaps) - 1].ResultedErrorCount := AResultedErrorCount;
            end;

            if (ADbgBuffer[0] <> CL_SUCCESS) or ((ADbgBuffer[1] <> CL_SUCCESS) and (ADbgBuffer[1] <> -4567)) then   //CLK_SUCCESS       -4567 is the initial value of ADbgBuffer[1]
              Result := False;
          finally
            OpenCLDll.clReleaseMemObject(KernelDoneBufferRef);
          end;
        finally
          OpenCLDll.clReleaseMemObject(DbgBufferRef);
        end;
      finally
        OpenCLDll.clReleaseMemObject(ResBufferRef);
      end;
    finally
      OpenCLDll.clReleaseMemObject(SubBufferRef);
    end;
  finally
    OpenCLDll.clReleaseMemObject(BackgroundBufferRef);
  end;
end;


function BitmapPosMatch_BruteForceOnGPU(ASrcBmpData, ASubBmpData: Pointer;
                                        ABytesPerPixelOnSrc, ABytesPerPixelOnSub: Integer;
                                        ASourceBitmapWidth, ASourceBitmapHeight, ASubBitmapWidth, ASubBitmapHeight: Integer;
                                        ColorErrorLevel: Integer;
                                        out SubCnvXOffset, SubCnvYOffset: Integer;
                                        var AFoundBitmaps: TCompRecArr;
                                        TotalErrorCount, FastSearchColorErrorCount: Integer;
                                        AUseFastSearch, AIgnoreBackgroundColor, AGetAllBitmaps: Boolean;
                                        ABackgroundColor: TColor;
                                        var AIgnoredColorsArr: TColorArr;
                                        ASleepySearch: Byte;
                                        AOutsideTickCount, APrecisionTimeout: QWord;
                                        AThreadCount: Integer;
                                        ACustomOpenCLPath: string;
                                        AGPUPlatformIndex, AGPUDeviceIndex: Integer;
                                        AGPUExecutionAvailability: TGPUExecutionAvailability;
                                        AGPUIncludeDashG: Boolean;
                                        AGPUSlaveQueueFromDevice: Boolean;
                                        AGPUUseAllKernelsEvent: Boolean;
                                        AGPUNdrangeNoLocalParam: Boolean;
                                        AGPUUseEventsInEnqueueKernel: Boolean;
                                        AGPUWaitForAllKernelsToBeDone: Boolean;
                                        AGPUReleaseFinalEventAtKernelEnd: Boolean;
                                        AGPUIgnoreExecutionAvailability: Boolean;
                                        var AGPUDbgBuffer: TIntArr;
                                        out AResultedErrorCount: Integer;
                                        AStopSearchOnDemand: PBoolean = nil;
                                        StopSearchOnMismatch: Boolean = True): Boolean;
var
  Error, SecondError: Integer;
  KernelSrc: string;

  TempLocalSize: csize_t;
  TempContext: cl_context;
  CmdQueue, SlaveCmdQueue: cl_command_queue;
  CLProgram: cl_program;
  CLKernel: cl_kernel;

  DevType: cl_device_type; //GPU
  PlatformIDs: ^cl_platform_id_arr;
  DeviceIDs: ^cl_device_id_arr;
  PlatformCount, DeviceCount: cl_uint;
  BuildOptions: string;
  Info: string;
  InfoLen: csize_t;
  QueueProperties: array[0..8] of cl_command_queue_properties;
  OpenCLDll: TOpenCL;
  ShouldCall_SlideSearch: Boolean;
begin
  //ToDo: - Implement FastSearch property, which verifies a small rectangle (Top-Left), before going full bmp.
  //ToDo: - Implement ignored colors, using AIgnoredColorsArr.
  //ToDo: - Move the whole code to another (CPU) thread, to avoid blocking the UI.

  Result := False;

  if (AGPUPlatformIndex = -1) or (AGPUPlatformIndex > 10) then  //assume no more than 10 platforms
  begin
    LogCallResult(CL_INVALID_PLATFORM, 'Platform validation', 'a valid value, not ' + IntToStr(AGPUPlatformIndex), 'No platform selected. Exiting.');
    Exit;
  end;

  if (AGPUDeviceIndex = -1) or (AGPUDeviceIndex > 100) then     //assume no more than 100 GPUs
  begin
    LogCallResult(CL_INVALID_DEVICE, 'Device validation, not ' + IntToStr(AGPUDeviceIndex), 'No device selected. Exiting.');
    Exit;
  end;

  KernelSrc := GetKernelSrcRGB_MatCmp(ABytesPerPixelOnSrc, ABytesPerPixelOnSub);

  ShouldCall_SlideSearch := AGPUExecutionAvailability in [eaOpenCL3Only, eaOpenCL3Then1, eaOpenCL3Then1ThenCPU];

  if ShouldCall_SlideSearch then
    KernelSrc := KernelSrc + GetKernelSrcRGB_SlideSearch(IntToStr(ASubBitmapHeight),
                                                         AGPUSlaveQueueFromDevice,
                                                         AGPUUseAllKernelsEvent,
                                                         AGPUNdrangeNoLocalParam,
                                                         AGPUUseEventsInEnqueueKernel,
                                                         AGPUWaitForAllKernelsToBeDone,
                                                         AGPUReleaseFinalEventAtKernelEnd);

  OpenCLDll := TOpenCL.Create;
  try
    if ACustomOpenCLPath <> '' then
      if OpenCLDll.ExpectedDllLocation <> ACustomOpenCLPath then
      begin
        OpenCLDll.ExpectedDllFileName := ExtractFileName(ACustomOpenCLPath);
        OpenCLDll.ExpectedDllDir := ExtractFileDir(ACustomOpenCLPath);
        OpenCLDll.LoadOpenCLLibrary;
      end;

    if not OpenCLDll.Loaded then
      raise Exception.Create('OpenCL not available. The dll is expected to exist at ' + OpenCLDll.ExpectedDllLocation);

    Error := OpenCLDll.clGetPlatformIDs(0, nil, @PlatformCount);
    LogCallResult(Error, 'clGetPlatformIDs', 'PlatformCount: ' + IntToStr(PlatformCount));

    GetMem(PlatformIDs, PlatformCount * SizeOf(cl_platform_id));
    try
      Error := OpenCLDll.clGetPlatformIDs(PlatformCount, Pcl_platform_id(PlatformIDs), nil);
      LogCallResult(Error, 'clGetPlatformIDs', '');

      DevType := CL_DEVICE_TYPE_GPU;
      Error := OpenCLDll.clGetDeviceIDs(PlatformIDs[AGPUPlatformIndex], DevType, 0, nil, @DeviceCount);
      LogCallResult(Error, 'clGetDeviceIDs', 'DeviceCount: ' + IntToStr(DeviceCount));

      GetMem(DeviceIDs, DeviceCount * SizeOf(cl_device_id));
      try
        Error := OpenCLDll.clGetDeviceIDs(PlatformIDs[AGPUPlatformIndex], DevType, DeviceCount, Pcl_device_id(DeviceIDs), nil);
        LogCallResult(Error, 'clGetDeviceIDs', '');

        TempContext := OpenCLDll.clCreateContext(nil, 1, @DeviceIDs[AGPUDeviceIndex], nil, nil, Error);
        try
          if TempContext = nil then
            LogCallResult(Error, 'clCreateContext', '', 'Error is ' + IntToStr(Error));

          QueueProperties[0] := CL_QUEUE_PROPERTIES;
          QueueProperties[1] := CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE;
          QueueProperties[2] := 0;

          try
            CmdQueue := OpenCLDll.clCreateCommandQueueWithProperties(TempContext, DeviceIDs[AGPUDeviceIndex], @QueueProperties, Error);
            if (CmdQueue = nil) or (Error <> 0) then
              LogCallResult(Error, 'clCreateCommandQueueWithProperties CmdQueue', '');
          except
            on E: Exception do
              LogCallResult(Error, 'clCreateCommandQueueWithProperties CmdQueue', '', 'Ex: ' + E.Message);
          end;

          QueueProperties[0] := CL_QUEUE_PROPERTIES;
          QueueProperties[1] := CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE or CL_QUEUE_ON_DEVICE or CL_QUEUE_ON_DEVICE_DEFAULT;
          QueueProperties[2] := 0;

          try
            SlaveCmdQueue := OpenCLDll.clCreateCommandQueueWithProperties(TempContext, DeviceIDs[AGPUDeviceIndex], @QueueProperties, Error);  //also tested by creating this queue before the other one.  get_default_queue  still returns 0.
            if (SlaveCmdQueue = nil) or (Error <> 0) then
              LogCallResult(Error, 'clCreateCommandQueueWithProperties SlaveCmdQueue', '');
          except
            on E: Exception do
              LogCallResult(Error, 'clCreateCommandQueueWithProperties SlaveCmdQueue', '', 'Ex: ' + E.Message);
          end;

          try
            CLProgram := OpenCLDll.clCreateProgramWithSource(TempContext, 1, PPAnsiChar(@KernelSrc), nil, Error);
            if CLProgram = nil then
              LogCallResult(Error, 'clCreateProgramWithSource', '');

            try
              BuildOptions := '';
              if AGPUIncludeDashG then
                BuildOptions := '-g'; //Requires support for OpenCL 2.0 or higher (TBD).

              if BuildOptions <> '' then  //has -g
                BuildOptions := BuildOptions + ' ';

              BuildOptions := BuildOptions + '-cl-kernel-arg-info';  //used for getting additional debugging info from enqueue_kernel.
              BuildOptions := BuildOptions + ' -cl-std=CL2.0';
              Error := OpenCLDll.clBuildProgram(CLProgram, 0, nil, @BuildOptions[1], nil, nil);
              //LogCallResult(Error, 'clBuildProgram', 'Kernel code compiled.'); //commented, to allow the next call to clGetProgramBuildInfo

              if Error < CL_SUCCESS then
              begin
                SetLength(Info, 32768);
                SecondError := OpenCLDll.clGetProgramBuildInfo(CLProgram, DeviceIDs[AGPUDeviceIndex], CL_PROGRAM_BUILD_LOG, Length(Info), @Info[1], InfoLen);
                SetLength(Info, InfoLen);
                LogCallResult(SecondError, 'clGetProgramBuildInfo', 'Additional build info.');

                Info := StringReplace(Info, #13#10, '|', [rfReplaceAll]);
                Info := StringReplace(Info, #10, '|', [rfReplaceAll]);
                LogCallResult(Error, 'clBuildProgram', 'Kernel code compiled.', Info);
              end;

              if ShouldCall_SlideSearch then
              begin
                CLKernel := OpenCLDll.clCreateKernel(CLProgram, 'SlideSearch', Error);   //call SlideSearch
                try
                  LogCallResult(Error, 'clCreateKernel', 'Kernel allocated.');

                  Error := OpenCLDll.clGetKernelWorkGroupInfo(CLKernel, DeviceIDs[AGPUDeviceIndex], CL_KERNEL_WORK_GROUP_SIZE, SizeOf(TempLocalSize), @TempLocalSize, InfoLen);
                  LogCallResult(Error, 'clGetKernelWorkGroupInfo', 'Work group info obtained.');

                  Result := BruteForceOnGPU_SlideSearch(OpenCLDll, CLKernel, TempContext, CmdQueue, SlaveCmdQueue,
                                                        ASrcBmpData, ASubBmpData, ABytesPerPixelOnSrc, ABytesPerPixelOnSub,
                                                        ASourceBitmapWidth, ASourceBitmapHeight, ASubBitmapWidth, ASubBitmapHeight,
                                                        ColorErrorLevel, TotalErrorCount, FastSearchColorErrorCount, AUseFastSearch,
                                                        AIgnoreBackgroundColor, AGetAllBitmaps, ASleepySearch,
                                                        AOutsideTickCount, APrecisionTimeout,
                                                        AResultedErrorCount, SubCnvXOffset, SubCnvYOffset, AFoundBitmaps, AGPUDbgBuffer, AStopSearchOnDemand);
                finally  //clCreateKernel
                  OpenCLDll.clReleaseKernel(CLKernel);
                end;
              end
              else
              begin
                CLKernel := OpenCLDll.clCreateKernel(CLProgram, 'MatCmp', Error);    //call MatCmp
                try
                  LogCallResult(Error, 'clCreateKernel', 'Kernel allocated.');

                  Error := OpenCLDll.clGetKernelWorkGroupInfo(CLKernel, DeviceIDs[AGPUDeviceIndex], CL_KERNEL_WORK_GROUP_SIZE, SizeOf(TempLocalSize), @TempLocalSize, InfoLen);
                  LogCallResult(Error, 'clGetKernelWorkGroupInfo', 'Work group info obtained.');

                  Result := BruteForceOnGPU_MatCmp(OpenCLDll, CLKernel, TempContext, CmdQueue, SlaveCmdQueue,
                                                   ASrcBmpData, ASubBmpData, ABytesPerPixelOnSrc, ABytesPerPixelOnSub,
                                                   ASourceBitmapWidth, ASourceBitmapHeight, ASubBitmapWidth, ASubBitmapHeight,
                                                   ColorErrorLevel, TotalErrorCount, FastSearchColorErrorCount, AUseFastSearch,
                                                   AIgnoreBackgroundColor, AGetAllBitmaps, ASleepySearch,
                                                   AOutsideTickCount, APrecisionTimeout,
                                                   AResultedErrorCount, SubCnvXOffset, SubCnvYOffset, AFoundBitmaps, AGPUDbgBuffer, AStopSearchOnDemand);
                finally  //clCreateKernel
                  OpenCLDll.clReleaseKernel(CLKernel);
                end;
              end; //ShouldCall_SlideSearch

            finally
              OpenCLDll.clReleaseProgram(CLProgram);
            end; //clCreateProgramWithSource
          finally
            OpenCLDll.clReleaseCommandQueue(CmdQueue);
            OpenCLDll.clReleaseCommandQueue(SlaveCmdQueue);   //used for slave kernels
          end;
        finally
          OpenCLDll.clReleaseContext(TempContext);
        end;
      finally
        Freemem(DeviceIDs, DeviceCount * SizeOf(cl_device_id));
      end;
    finally
      Freemem(PlatformIDs, PlatformCount * SizeOf(cl_platform_id));
    end;
  finally
    OpenCLDll.Free;
  end;
end;

end.

