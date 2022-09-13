{
    Copyright (C) 2022 VCC
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

  CMouseClickType_Click = 0;
  CMouseClickType_Drag = 1;
  CMouseClickType_MouseDown = 2;
  CMouseClickType_MouseUp = 3;

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
  

procedure MoveMouseCursor(DestTp: TPoint; ACallAppProcMsg: Boolean = True); //slow move, not teleporting
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
  
  Sleep(1);
  Dec(IncX);
  SetCursorPos(IncX, IncY);
end;


procedure GetMouseEventsFromParam(AParams: TStrings; DefaultX, DefaultY: Integer; out X, Y: Integer; out XDest, YDest: Integer; out IsDragging: Boolean; out AShift: TShiftState; out AButton: TMouseButton);
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
  ClickPoint: TPoint;
  InitialPoint: TPoint;

  procedure ExecMouseClick;
  var
    AMouseBtnDownState, AMouseBtnUpState: Word;
    AInputs: TInput;
    MoveWithoutClick: Boolean;
  begin
    ClickPoint.X := X;
    ClickPoint.Y := Y;
    MoveMouseCursor(ClickPoint);

    MoveWithoutClick := AParams.Values['MoveWithoutClick'] = '1';

    if not MoveWithoutClick then
    begin
      SetMouseButtonStates(AButton, AMouseBtnDownState, AMouseBtnUpState);
      SimulateSpecialKeys(AShift, 0);
      Application.ProcessMessages;

      SetBasicMouseInfo(AInputs, X, Y);

      AInputs.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MOVE;
      mouse_event(AInputs.mi.dwFlags, DWord(AInputs.mi.dx), DWord(AInputs.mi.dy), 0, 0);
      Application.ProcessMessages;
      Sleep(50);

      AInputs.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or AMouseBtnDownState;
      mouse_event(AInputs.mi.dwFlags, DWord(AInputs.mi.dx), DWord(AInputs.mi.dy), 0, 0);
      Application.ProcessMessages;
      Sleep(100);
    end;

    if IsDragging then
    begin
      SetBasicMouseInfo(AInputs, XDest, YDest);

      AInputs.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MOVE;
      mouse_event(AInputs.mi.dwFlags, DWord(AInputs.mi.dx), DWord(AInputs.mi.dy), 0, 0);

      Application.ProcessMessages;
      Sleep(50);
    end;

    if not MoveWithoutClick then
    begin
      AInputs.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or AMouseBtnUpState;
      mouse_event(AInputs.mi.dwFlags, DWord(AInputs.mi.dx), DWord(AInputs.mi.dy), 0, 0);
      Application.ProcessMessages;

      if AButton <> mbMiddle then
      begin
        AInputs.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MOVE;
        mouse_event(AInputs.mi.dwFlags, DWord(AInputs.mi.dx), DWord(AInputs.mi.dy), 0, 0);
        Application.ProcessMessages;
      end;

      SimulateSpecialKeys(AShift, KEYEVENTF_KEYUP);
      Application.ProcessMessages;
    end;
  end;
begin
  Randomize;
  GetMouseEventsFromParam(AParams, 3, 3, X, Y, XDest, YDest, IsDragging, AShift, AButton);

  GetCursorPos(InitialPoint);
  try
    ExecMouseClick;
  finally
    if AParams.Values['LeaveMouse'] <> '1' then
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

  procedure ExecMouseDown;
  var
    AMouseBtnDownState, AMouseBtnUpState: Word;
    AInputs: TInput;
  begin
    ClickPoint.X := X;
    ClickPoint.Y := Y;
    MoveMouseCursor(ClickPoint, False);

    SetMouseButtonStates(AButton, AMouseBtnDownState, AMouseBtnUpState);
    SimulateSpecialKeys(AShift, 0);
    //Application.ProcessMessages;   //commented, to allow calling from server thread (for lower latency)

    SetBasicMouseInfo(AInputs, X, Y);

    AInputs.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or AMouseBtnDownState;
    mouse_event(AInputs.mi.dwFlags, DWord(AInputs.mi.dx), DWord(AInputs.mi.dy), 0, 0);
    //Application.ProcessMessages;
    //Sleep(100);
    //SimulateSpecialKeys(AShift, KEYEVENTF_KEYUP); //usually, this key should already be released here, only on MouseUp
  end;
begin
  GetCursorPos(tp);
  GetMouseEventsFromParam(AParams, tp.X, tp.Y, X, Y, XDest, YDest, IsDragging, AShift, AButton);
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

  procedure ExecMouseUp;
  var
    AMouseBtnDownState, AMouseBtnUpState: Word;
    AInputs: TInput;
  begin
    ClickPoint.X := X;
    ClickPoint.Y := Y;
    MoveMouseCursor(ClickPoint, False);

    SetMouseButtonStates(AButton, AMouseBtnDownState, AMouseBtnUpState);
    //SimulateSpecialKeys(AShift, 0);  //usually, this key should already be down from MouseDown
    //Application.ProcessMessages;

    SetBasicMouseInfo(AInputs, X, Y);

    AInputs.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or AMouseBtnUpState;
    mouse_event(AInputs.mi.dwFlags, DWord(AInputs.mi.dx), DWord(AInputs.mi.dy), 0, 0);
    //Application.ProcessMessages;
    //Sleep(100);
    SimulateSpecialKeys(AShift, KEYEVENTF_KEYUP);
  end;
begin
  GetCursorPos(tp);
  GetMouseEventsFromParam(AParams, tp.X, tp.Y, X, Y, XDest, YDest, IsDragging, AShift, AButton);
  ExecMouseUp;
end;

end.
