{
    Copyright (C) 2024 VCC
    creation date: Dec 2019
    initial release date: 13 Sep 2022

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


unit MouseStuff;

interface


uses
  Windows, Classes;

const

  CClickIgnored = 'ClickIgnored';
  CMouseX = 'X';
  CMouseY = 'Y';
  CDeltaX = 'DeltaX';
  CDeltaY = 'DeltaY';
  CMouseButton = 'MouseButton';
  CMouseShiftState = 'MouseShiftState';
  CMouseCursorLeaveMouse = 'LeaveMouse';
  CMouseMoveWithoutClick = 'MoveWithoutClick';
  CMouseClickType = 'ClickType';
  CMouseXDest = 'XDest';
  CMouseYDest = 'YDest';
  CMouseDelayAfterMovingToDestination = 'DelayAfterMovingToDestination';
  CMouseDelayAfterMouseDown = 'DelayAfterMouseDown';
  CMouseMoveDuration = 'MoveDuration';

  CMouseWheelType = 'WheelType';
  CMouseWheelVertWheel = 'VWheel';
  CMouseWheelHorizWheel = 'HWheel';
  CMouseWheelAmount = 'WheelAmount'; //the values of this key are in 120 unit increments  (can also be negative)
  CMouseUseClipCursor = 'UseClipCursor';

  //Same values are defined in ClickerUtils. They are redefined here, to avoid a dependency.
  CMouseClickType_Click = 0;
  CMouseClickType_Drag = 1;
  CMouseClickType_MouseDown = 2;
  CMouseClickType_MouseUp = 3;
  CMouseClickType_Wheel = 4;

  CMouseClickType_DragStr = '1';


  CMouseButtonLeft = 'Left'; //used by default, if not provided
  CMouseButtonRight = 'Right';
  CMouseButtonMiddle = 'Middle';
  CShiftStateShift = 'Shift';
  CShiftStateAlt = 'Alt';
  CShiftStateCtrl = 'Ctrl';
  CShiftStateDoubleClick = 'Double';


procedure ClickTControl(AParams: TStringList);
procedure MouseDownTControl(AParams: TStrings);
procedure MouseUpTControl(AParams: TStrings);
procedure MouseWheelTControl(AParams: TStrings);

  
implementation

uses
  SysUtils, Forms, Controls, Math;


function MouseMoveValueByOffset(v1, v2: Integer): Integer;
begin
  if v1 = v2 then
    Result := 0
  else
    if v1 < v2 then
      Result := -1
    else
      Result := 1;
end;


procedure ClipCursorToOnePixel(AX, AY: Integer);
var
  ClipCursorRect: TRect;
begin
  ClipCursorRect.Left := AX;
  ClipCursorRect.Top := AY;
  ClipCursorRect.Right := AX + 1;
  ClipCursorRect.Bottom := AY + 1;
  ClipCursor(ClipCursorRect);
end;


procedure MoveMouseCursor(DestTp: TPoint; AUseClipCursor: Boolean; ACallAppProcMsg: Boolean = True); //slow move, not teleporting
var
  SrcTp: TPoint;
  IncX, IncY: Integer;
  IncCount, SleepDistance: Integer;
  MaxDiff: Integer;
begin
  GetCursorPos(SrcTp);

  IncX := SrcTp.X;
  IncY := SrcTp.Y;
  MaxDiff := Max(Abs(SrcTp.X - DestTp.X), Abs(SrcTp.Y - DestTp.Y));
  IncCount := 0;
  SleepDistance := MaxDiff shr 3;

  repeat
    Inc(IncX, MouseMoveValueByOffset(DestTp.X, IncX));
    Inc(IncY, MouseMoveValueByOffset(DestTp.Y, IncY));
    Inc(IncCount);

    SetCursorPos(IncX, IncY);
    if AUseClipCursor then
      ClipCursorToOnePixel(IncX, IncY);

    if (SleepDistance > 0) and (IncCount mod SleepDistance = 0) then
    begin
      if ACallAppProcMsg then
        Application.ProcessMessages;

      Sleep(1);
    end;
  until (IncX = DestTp.X) and (IncY = DestTp.Y);

  //move mouse once again, to trigger MouseMove event   -  not sure if required or working
  Sleep(1);
  Inc(IncX);
  SetCursorPos(IncX, IncY);

  if AUseClipCursor then
    ClipCursorToOnePixel(IncX, IncY);
  
  Sleep(1);
  Dec(IncX);
  SetCursorPos(IncX, IncY);

  if AUseClipCursor then
    ClipCursorToOnePixel(IncX, IncY);
end;


procedure MoveMouseCursorWithDuration(DestTp: TPoint; ADuration: Integer; AUseClipCursor: Boolean; ACallAppProcMsg: Boolean = True); //slow move, not teleporting
const              //it seems that using Sleep calls, with args greater than 16, gives more accurate results than with lower values
  CResolutionDiv = 4; //powers of two   .. A greater value means less Sleep calls, which should get closer to the overall requested duration.
  CResolutionMul = 1 shl CResolutionDiv;
var
  SrcTp: TPoint;
  IncX, IncY: Extended;
  IncXDist, IncYDist: Extended;
  IncCount, SleepDistance: Integer;
  MaxDiff, SleepAmount: Integer;
  DurationDiv: Extended; //Integer;

begin
  if ADuration < 1 then
  begin
    SetCursorPos(DestTp.X, DestTp.Y);
    Exit;
  end;

  GetCursorPos(SrcTp);

  IncX := SrcTp.X;
  IncY := SrcTp.Y;
  MaxDiff := Max(Abs(SrcTp.X - DestTp.X), Abs(SrcTp.Y - DestTp.Y));
  IncCount := 0;
  SleepDistance := MaxDiff shr 3;

  SleepAmount := 1 shl CResolutionDiv;
  DurationDiv := ADuration / CResolutionMul;
  if DurationDiv = 0 then
    DurationDiv := 1;

  IncXDist := (DestTp.X - SrcTp.X) / DurationDiv;
  IncYDist := (DestTp.Y - SrcTp.Y) / DurationDiv;

  repeat
    IncX := IncX + IncXDist;
    IncY := IncY + IncYDist;
    Inc(IncCount, SleepAmount);

    SetCursorPos(Round(IncX), Round(IncY));
    if AUseClipCursor then
      ClipCursorToOnePixel(Round(IncX), Round(IncY));

    if ACallAppProcMsg then
      if (SleepDistance > 0) and (IncCount mod SleepDistance = 0) then
        Application.ProcessMessages;

    Sleep(SleepAmount);  //the loop will take a bit longer.
  until IncCount >= ADuration;

  //move mouse once again, to trigger MouseMove event   -  not sure if required or working
  Sleep(1);
  IncX := IncX + 1;
  SetCursorPos(Round(IncX), Round(IncY));

  if AUseClipCursor then
    ClipCursorToOnePixel(Round(IncX), Round(IncY));

  Sleep(1);
  SetCursorPos(DestTp.X, DestTp.Y);

  if AUseClipCursor then
    ClipCursorToOnePixel(DestTp.X, DestTp.Y);
end;


procedure GetMouseEventsFromParam(AParams: TStrings; DefaultX, DefaultY: Integer; out X, Y: Integer; out XDest, YDest: Integer; out IsDragging: Boolean; out AShift: TShiftState; out AButton: TMouseButton; out ALeaveMouse, AUseClipCursor: Boolean);
begin
  X := StrToIntDef(AParams.Values[CMouseX], DefaultX);
  Y := StrToIntDef(AParams.Values[CMouseY], DefaultY);
  AButton := mbLeft;
  AShift := [];
  XDest := StrToIntDef(AParams.Values[CMouseXDest], DefaultX);
  YDest := StrToIntDef(AParams.Values[CMouseYDest], DefaultY);

  IsDragging := AParams.Values[CMouseClickType] = CMouseClickType_DragStr;

  if AParams.Values[CMouseButton] = CMouseButtonRight then
    AButton := TMouseButton(mbRight);

  if AParams.Values[CMouseButton] = CMouseButtonMiddle then
    AButton := TMouseButton(mbMiddle);

  if AButton = mbLeft then
    Include(AShift, ssLeft);

  if AButton = mbRight then
    Include(AShift, ssRight);

  if AButton = mbMiddle then
    Include(AShift, ssMiddle);    

  if Pos(CShiftStateShift, AParams.Values[CMouseShiftState]) > 0 then
    Include(AShift, ssShift);

  if Pos(CShiftStateAlt, AParams.Values[CMouseShiftState]) > 0 then
    Include(AShift, ssAlt);

  if Pos(CShiftStateCtrl, AParams.Values[CMouseShiftState]) > 0 then
    Include(AShift, ssCtrl);

  //if Pos(CShiftStateDoubleClick, AParams.Values[CMouseShiftState]) > 0 then
  //  Include(AShift, ssDouble);    //ssDouble seems to do nothing

  ALeaveMouse := AParams.Values[CMouseCursorLeaveMouse] = '1';
  AUseClipCursor := AParams.Values[CMouseUseClipCursor] = '1';
end;


procedure SimulateSpecialKeys(AShift: TShiftState; AKeyState: Byte);
begin
  if ssAlt in AShift then
    keybd_event(VK_MENU, 0, AKeyState, 0);

  if ssCtrl in AShift then
    keybd_event(VK_CONTROL, 0, AKeyState, 0);

  if ssShift in AShift then
    keybd_event(VK_SHIFT, 0, AKeyState, 0);
end;


procedure SetMouseButtonStates(AButton: TMouseButton; out AMouseBtnDownState, AMouseBtnUpState: Word);
begin
  case AButton of
    mbLeft:
    begin
      AMouseBtnDownState := MOUSEEVENTF_LEFTDOWN;
      AMouseBtnUpState := MOUSEEVENTF_LEFTUP;
    end;

    mbRight:
    begin
      AMouseBtnDownState := MOUSEEVENTF_RIGHTDOWN;
      AMouseBtnUpState := MOUSEEVENTF_RIGHTUP;
    end;

    mbMiddle:
    begin
      AMouseBtnDownState := MOUSEEVENTF_MIDDLEDOWN;
      AMouseBtnUpState := MOUSEEVENTF_MIDDLEUP;
    end;
  else
    AMouseBtnDownState := MOUSEEVENTF_LEFTDOWN;
    AMouseBtnUpState := MOUSEEVENTF_LEFTUP;
  end;
end;


procedure SetBasicMouseInfo(var AInputs: TInput; X, Y: Integer);
begin
  {$IFDEF FPC} AInputs._Type {$ELSE} AInputs.Itype {$ENDIF} := INPUT_MOUSE;
  AInputs.mi.dx := Round(X shl 16 / Screen.Width);
  AInputs.mi.dy := Round(Y shl 16 / Screen.Height);
  AInputs.mi.mouseData := 0;
  AInputs.mi.time := 0;
  {$IFDEF FPC} AInputs.mi.ExtraInfo {$ELSE} AInputs.mi.dwExtraInfo {$ENDIF} := 0;
end;


procedure ClickTControl(AParams: TStringList);
var
  X, Y, XDest, YDest: Integer;       //offsets
  IsDragging: Boolean;
  AShift: TShiftState;
  AButton: TMouseButton;
  ClickPoint, DestPoint: TPoint;
  InitialPoint: TPoint;
  LeaveMouse, UseClipCursor: Boolean;

  procedure ExecMouseClick;
  var
    AMouseBtnDownState, AMouseBtnUpState: Word;
    AInputs: TInput;
    MoveWithoutClick: Boolean;
    DelayAfterMovingToDestination: Integer;
    DelayAfterMouseDown: Integer;
    MoveDuration: Integer;
  begin
    ClickPoint.X := X;
    ClickPoint.Y := Y;

    MoveWithoutClick := AParams.Values['MoveWithoutClick'] = '1';
    DelayAfterMovingToDestination := Max(1, StrToIntDef(AParams.Values[CMouseDelayAfterMovingToDestination], 50));
    DelayAfterMouseDown := Max(1, StrToIntDef(AParams.Values[CMouseDelayAfterMouseDown], 200));
    MoveDuration := Max(-1, StrToIntDef(AParams.Values[CMouseMoveDuration], -1));

    if MoveDuration < 1 then
      MoveMouseCursor(ClickPoint, UseClipCursor, False)
    else
    begin
      if not IsDragging then
        MoveMouseCursorWithDuration(ClickPoint, MoveDuration, UseClipCursor, MoveDuration > 5000);  //there will be race-conditions on client-server execution, if using App.ProcMsg
    end;

    if not MoveWithoutClick then
    begin
      SetMouseButtonStates(AButton, AMouseBtnDownState, AMouseBtnUpState);
      SimulateSpecialKeys(AShift, 0);
      //Application.ProcessMessages;

      SetBasicMouseInfo(AInputs, X, Y);

      AInputs.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MOVE;
      mouse_event(AInputs.mi.dwFlags, DWord(AInputs.mi.dx), DWord(AInputs.mi.dy), 0, 0);  //at ClickPoint

      if UseClipCursor then
        ClipCursorToOnePixel(X, Y);

      //Application.ProcessMessages;
      Sleep(DelayAfterMovingToDestination);  //delay after moving to destination

      AInputs.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or AMouseBtnDownState;
      mouse_event(AInputs.mi.dwFlags, DWord(AInputs.mi.dx), DWord(AInputs.mi.dy), 0, 0);  //at ClickPoint
      //Application.ProcessMessages;
      Sleep(DelayAfterMouseDown);  //delay after mouse down
    end;

    if IsDragging then
    begin
      DestPoint.X := XDest;
      DestPoint.Y := YDest;
      MoveMouseCursorWithDuration(DestPoint, MoveDuration, UseClipCursor, MoveDuration > 5000);  //there will be race-conditions on client-server execution, if using App.ProcMsg

      SetBasicMouseInfo(AInputs, XDest, YDest);

      AInputs.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MOVE;
      mouse_event(AInputs.mi.dwFlags, DWord(AInputs.mi.dx), DWord(AInputs.mi.dy), 0, 0);  //at destination

      //Application.ProcessMessages;
      Sleep(DelayAfterMovingToDestination);   //delay after moving to destination
    end;

    if not MoveWithoutClick then
    begin
      if UseClipCursor then
      begin
        if not IsDragging then
          ClipCursorToOnePixel(X, Y)
        else
          ClipCursorToOnePixel(XDest, YDest);
      end;

      AInputs.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or AMouseBtnUpState;
      mouse_event(AInputs.mi.dwFlags, DWord(AInputs.mi.dx), DWord(AInputs.mi.dy), 0, 0);  //at ClickPoint if not dragging, at destination if dragging
      //Application.ProcessMessages;

      if AButton <> mbMiddle then
      begin
        AInputs.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MOVE;
        mouse_event(AInputs.mi.dwFlags, DWord(AInputs.mi.dx), DWord(AInputs.mi.dy), 0, 0); //same as above
        //Application.ProcessMessages;
      end;

      SimulateSpecialKeys(AShift, KEYEVENTF_KEYUP);
      //Application.ProcessMessages;
    end;
  end;

begin
  Randomize;
  GetMouseEventsFromParam(AParams, 3, 3, X, Y, XDest, YDest, IsDragging, AShift, AButton, LeaveMouse, UseClipCursor);

  GetCursorPos(InitialPoint);
  try
    if IsDragging then
      SetCursorPos(XDest, YDest);   //usually harmless, but it might be needed in some special corner cases

    try
      ExecMouseClick;
    finally
      if UseClipCursor then
        ClipCursor(nil);
    end;
  finally
    if IsDragging then
      SetCursorPos(XDest, YDest)
    else
      if not LeaveMouse {or IsDragging} then
        SetCursorPos(InitialPoint.X, InitialPoint.Y);
  end;
end;


procedure MouseDownTControl(AParams: TStrings);
var
  X, Y, XDest, YDest: Integer;       //offsets
  IsDragging: Boolean;
  AShift: TShiftState;
  AButton: TMouseButton;
  ClickPoint: TPoint;
  tp: TPoint;
  LeaveMouse, UseClipCursor: Boolean;

  procedure ExecMouseDown;
  var
    AMouseBtnDownState, AMouseBtnUpState: Word;
    AInputs: TInput;
  begin
    ClickPoint.X := X;
    ClickPoint.Y := Y;
    MoveMouseCursor(ClickPoint, UseClipCursor, False);

    SetMouseButtonStates(AButton, AMouseBtnDownState, AMouseBtnUpState);
    SimulateSpecialKeys(AShift, 0);
    //Application.ProcessMessages;   //commented, to allow calling from server thread (for lower latency)

    if UseClipCursor then
      ClipCursorToOnePixel(X, Y);

    try
      SetBasicMouseInfo(AInputs, X, Y);

      AInputs.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or AMouseBtnDownState;
      mouse_event(AInputs.mi.dwFlags, DWord(AInputs.mi.dx), DWord(AInputs.mi.dy), 0, 0);
      //Application.ProcessMessages;
      //Sleep(100);
      //SimulateSpecialKeys(AShift, KEYEVENTF_KEYUP); //usually, this key should already be released here, only on MouseUp
    finally
      if UseClipCursor then
        ClipCursor(nil);
    end;
  end;
begin
  GetCursorPos(tp);
  GetMouseEventsFromParam(AParams, tp.X, tp.Y, X, Y, XDest, YDest, IsDragging, AShift, AButton, LeaveMouse, UseClipCursor);
  ExecMouseDown;
end;


procedure MouseUpTControl(AParams: TStrings);
var
  X, Y, XDest, YDest: Integer;       //offsets
  IsDragging: Boolean;
  AShift: TShiftState;
  AButton: TMouseButton;
  ClickPoint: TPoint;
  tp: TPoint;
  LeaveMouse, UseClipCursor: Boolean;

  procedure ExecMouseUp;
  var
    AMouseBtnDownState, AMouseBtnUpState: Word;
    AInputs: TInput;
  begin
    ClickPoint.X := X;
    ClickPoint.Y := Y;
    MoveMouseCursor(ClickPoint, UseClipCursor, False);

    SetMouseButtonStates(AButton, AMouseBtnDownState, AMouseBtnUpState);
    //SimulateSpecialKeys(AShift, 0);  //usually, this key should already be down from MouseDown
    //Application.ProcessMessages;

    if UseClipCursor then
      ClipCursorToOnePixel(X, Y);

    try
      SetBasicMouseInfo(AInputs, X, Y);

      AInputs.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or AMouseBtnUpState;
      mouse_event(AInputs.mi.dwFlags, DWord(AInputs.mi.dx), DWord(AInputs.mi.dy), 0, 0);
      //Application.ProcessMessages;
      //Sleep(100);
      SimulateSpecialKeys(AShift, KEYEVENTF_KEYUP);
    finally
      if UseClipCursor then
        ClipCursor(nil);
    end;
  end;
begin
  GetCursorPos(tp);
  GetMouseEventsFromParam(AParams, tp.X, tp.Y, X, Y, XDest, YDest, IsDragging, AShift, AButton, LeaveMouse, UseClipCursor);
  ExecMouseUp;
end;


procedure MouseWheelTControl(AParams: TStrings);
var
  Amount: Integer;
begin
  Amount := StrToIntDef(AParams.Values[CMouseWheelAmount], 0);

  if AParams.Values[CMouseWheelType] = CMouseWheelVertWheel then
    mouse_event(MOUSEEVENTF_WHEEL, 0, 0, DWord(Amount * WHEEL_DELTA), 0);

  if AParams.Values[CMouseWheelType] = CMouseWheelHorizWheel then
    mouse_event(MOUSEEVENTF_HWHEEL, 0, 0, DWord(Amount), 0);  //the official MSDN doc does not mention doing a multiplication by WHEEL_DELTA
end;

end.
