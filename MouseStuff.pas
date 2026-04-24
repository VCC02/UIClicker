{
    Copyright (C) 2026 VCC
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
  {$IFDEF Windows}
    Windows,
  {$ELSE}
    LCLIntf, LCLType,
  {$ENDIF}
  Classes;

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
  CMouseRealisticMoving = 'RealisticMoving';

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

  {$IFDEF Windows}
    ClipCursor(ClipCursorRect);
  {$ELSE}
    //nothing so far
  {$ENDIF}
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
  DurationInc: Integer;
  tk, OverallTk: QWord;
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

  SleepAmount := CResolutionMul;

  IncXDist := (DestTp.X - SrcTp.X) / SleepAmount;
  IncYDist := (DestTp.Y - SrcTp.Y) / SleepAmount;
  DurationInc := Trunc(ADuration / SleepAmount);

  OverallTk := GetTickCount64;
  repeat
    IncX := IncX + IncXDist;
    IncY := IncY + IncYDist;
    Inc(IncCount);

    SetCursorPos(Round(IncX), Round(IncY));
    if AUseClipCursor then
      ClipCursorToOnePixel(Round(IncX), Round(IncY));

    if ACallAppProcMsg then
      if (SleepDistance > 0) and (IncCount mod SleepDistance = 0) then
        Application.ProcessMessages;

    tk := GetTickCount64;
    repeat
      if ACallAppProcMsg then
        Application.ProcessMessages;  //this takes less than sleep
    until GetTickCount64 - tk >= DurationInc;

    if GetTickCount64 - OverallTk >= ADuration then
      Break;
  until IncCount >= SleepAmount - 1;

  //move mouse once again, to trigger MouseMove event   -  not sure if required or working
  tk := GetTickCount64;
  repeat
    if ACallAppProcMsg then
      Application.ProcessMessages;  //this takes less than sleep
  until GetTickCount64 - tk >= 1;

  IncX := IncX + 1;
  SetCursorPos(Round(IncX), Round(IncY));

  if AUseClipCursor then
    ClipCursorToOnePixel(Round(IncX), Round(IncY));

  tk := GetTickCount64;
  repeat
    if ACallAppProcMsg then
      Application.ProcessMessages;  //this takes less than sleep
  until GetTickCount64 - tk >= 1;

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
  {$IFDEF Windows}
    if ssAlt in AShift then
      keybd_event(VK_MENU, 0, AKeyState, 0);

    if ssCtrl in AShift then
      keybd_event(VK_CONTROL, 0, AKeyState, 0);

    if ssShift in AShift then
      keybd_event(VK_SHIFT, 0, AKeyState, 0);
  {$ELSE}
    //
  {$ENDIF}
end;


procedure SetMouseButtonStates(AButton: TMouseButton; out AMouseBtnDownState, AMouseBtnUpState: Word);
begin
  {$IFDEF Windows}
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
  {$ELSE}
    AMouseBtnDownState := 0;
    AMouseBtnUpState := 0;
  {$ENDIF}
end;


{$IFDEF Windows}
procedure SetBasicMouseInfo(var AInputs: TInput; X, Y: Integer);
begin
  {$IFDEF FPC} AInputs._Type {$ELSE} AInputs.Itype {$ENDIF} := INPUT_MOUSE;
  AInputs.mi.dx := Round(X shl 16 / Screen.Width);
  AInputs.mi.dy := Round(Y shl 16 / Screen.Height);
  AInputs.mi.mouseData := 0;
  AInputs.mi.time := 0;
  {$IFDEF FPC} AInputs.mi.ExtraInfo {$ELSE} AInputs.mi.dwExtraInfo {$ENDIF} := 0;
end;


type
  TRMPoint = record
    tp: TPoint;
    MoveDuration: Extended;
  end;

  TRMPointArr = array of TRMPoint;


procedure SecondGrip;
var
  CurrentPoint, NewPoint: TPoint;
begin
  GetCursorPos(CurrentPoint);

  if Random(2) = 1 then
    Exit;

  NewPoint.X := Random(80) - 40 + CurrentPoint.X;
  NewPoint.Y := Random(40) - 20 + CurrentPoint.Y;
  SetCursorPos((CurrentPoint.X + NewPoint.X) shr 1 + Random(50), (CurrentPoint.Y + NewPoint.Y) shr 1 + Random(30));
  Sleep(30 + Random(50));
  Randomize;

  SetCursorPos((CurrentPoint.X + NewPoint.X) shr 1 - Random(50), (CurrentPoint.Y + NewPoint.Y) shr 1 - Random(30));
  Sleep(60 + Random(50));
  Randomize;

  if Random(3) = 1 then
  begin
    SetCursorPos((CurrentPoint.X + NewPoint.X * 3) shr 2 + Random(50), (CurrentPoint.Y + NewPoint.Y * 3) shr 2 + Random(10));
    Sleep(20 + Random(40));
    Randomize;

    SetCursorPos((CurrentPoint.X + NewPoint.X * 3) shr 2 - Random(30), (CurrentPoint.Y + NewPoint.Y * 3) shr 2 - Random(10));
    Sleep(40 + Random(30));
    Randomize;
  end;

  if Random(3) = 1 then
  begin
    SetCursorPos((CurrentPoint.X + NewPoint.X * 3) shr 2 + Random(60), (CurrentPoint.Y + NewPoint.Y * 3) shr 2 + Random(10));
    Sleep(30 + Random(60));
    Randomize;

    SetCursorPos((CurrentPoint.X + NewPoint.X * 3) shr 2 - Random(60), (CurrentPoint.Y + NewPoint.Y * 3) shr 2 - Random(10));
    Sleep(50 + Random(50));
    Randomize;
  end;

  if Random(3) = 1 then
  begin
    SetCursorPos((CurrentPoint.X + NewPoint.X * 3) shr 2 + Random(40), (CurrentPoint.Y + NewPoint.Y * 3) shr 2 + Random(30));
    Sleep(30 + Random(60));
    Randomize;

    SetCursorPos((CurrentPoint.X + NewPoint.X * 3) shr 2 - Random(40), (CurrentPoint.Y + NewPoint.Y * 3) shr 2 - Random(30));
    Sleep(50 + Random(50));
    Randomize;
  end;

  if Random(2) = 1 then
  begin
    SetCursorPos((CurrentPoint.X + NewPoint.X * 7) shr 3 + Random(20), (CurrentPoint.Y + NewPoint.Y * 7) shr 3 + Random(10));
    Sleep(40 + Random(70));
    Randomize;

    SetCursorPos((CurrentPoint.X + NewPoint.X * 7) shr 3 - Random(20), (CurrentPoint.Y + NewPoint.Y * 7) shr 3 - Random(10));
    Sleep(60 + Random(70));
    Randomize;
  end;

  SetCursorPos(NewPoint.X, NewPoint.Y);
  Sleep(60 + Random(60));
end;


procedure FirstGrip;
var
  CurrentPoint, NewPoint: TPoint;
  XDir, YDir: TValueSign;
  i: Integer;
begin
  if Random(4) <> 1 then  //first grip
    Exit;

  GetCursorPos(CurrentPoint);
  NewPoint := CurrentPoint;

  Sleep(1);
  Randomize;
  if Random(2) = 1 then
    XDir := 1
  else
    XDir := -1;

  Sleep(1);
  Randomize;
  if Random(2) = 1 then
    YDir := 1
  else
    YDir := -1;

  for i := 0 to 15 + Random(20) do
  begin
    if Random(28) = 14 then
    begin
      Sleep(1);
      Randomize;
      XDir := -XDir;
    end;

    if Random(28) = 14 then
    begin
      Sleep(1);
      Randomize;
      YDir := -YDir;
    end;

    Inc(NewPoint.X, XDir * Random(5));
    Inc(NewPoint.Y, YDir * Random(5));
    SetCursorPos(NewPoint.X, NewPoint.Y);
    Sleep(3 + Random(5));
  end;

  SecondGrip;
end;


procedure GeneratePointsForRealisticMoving(var ARealisticMovingPoints: TRMPointArr; ADestPoint: TPoint; ATotalMoveDuration: Integer; out ALineLength: Extended);
const
  CSlowdown = 0.3;
var
  CurrentPoint: TPoint;
  i: Integer;
  AdjustingPointCount: Integer; //where the "user" would pause the natural movement and adust the mouse (e.g. better grip, or because of lack of space, or bad trajectory)
  SmallDuration, CurrentOverallDuration, DurationAmountEx: Extended;
  HalfSmallDuration: Integer;
  DurationAmount: Integer;
  X, X1, X2, Y, Y1, Y2, XAmount, YAmount, DivXAmount, DivYAmount: Extended;
  tk: QWord;
begin
  GetCursorPos(CurrentPoint);
  Randomize;

  try
    ALineLength := Sqrt(Sqr(CurrentPoint.X - ADestPoint.X) + Sqr(CurrentPoint.Y - ADestPoint.Y));
    if ALineLength < 1 then
      ALineLength := 30;
  except
  end;

  AdjustingPointCount := Random(6);                    //approx 60 points for 1200px
  SetLength(ARealisticMovingPoints, Round((ALineLength * 60) / 1200) + Random(5) + AdjustingPointCount);   //must be greater than 0

  if Length(ARealisticMovingPoints) < 2 then  //usually for debugging, because there is a minimum value above
    SetLength(ARealisticMovingPoints, 2);

  //Generate durations:
  if Length(ARealisticMovingPoints) = 1 then
    SmallDuration := ATotalMoveDuration
  else
    SmallDuration := (ATotalMoveDuration / (Length(ARealisticMovingPoints) - 1)) * CSlowdown;  //Multiplied by CSlowdown, because the overall duration will end up to much.

  for i := 0 to Length(ARealisticMovingPoints) - 1 do
    ARealisticMovingPoints[i].MoveDuration := SmallDuration;

  if SmallDuration > 5 then   //modify durations only if they can be perceived
  begin
    HalfSmallDuration := Round(SmallDuration) shr 1;
    for i := 0 to Length(ARealisticMovingPoints) - 2 do
      if Random(3) = 1 then
      begin
        DurationAmount := Max(3, 1 + Random(HalfSmallDuration));
        ARealisticMovingPoints[i].MoveDuration := ARealisticMovingPoints[i].MoveDuration + DurationAmount;         //add to the current
        ARealisticMovingPoints[i + 1].MoveDuration := ARealisticMovingPoints[i + 1].MoveDuration - DurationAmount; //and remove from the next
      end;

    CurrentOverallDuration := 0;
    for i := 0 to Length(ARealisticMovingPoints) - 1 do
      CurrentOverallDuration := CurrentOverallDuration + ARealisticMovingPoints[i].MoveDuration;

    if CurrentOverallDuration > ATotalMoveDuration then //Added too much, so remove from all
    begin
      DurationAmountEx := (CurrentOverallDuration - ATotalMoveDuration) / (Length(ARealisticMovingPoints) - 1);
      for i := 0 to Length(ARealisticMovingPoints) - 1 do
      begin
        ARealisticMovingPoints[i].MoveDuration := ARealisticMovingPoints[i].MoveDuration - DurationAmountEx;
        if ARealisticMovingPoints[i].MoveDuration < 1 then
          ARealisticMovingPoints[i].MoveDuration := 1;
      end;
    end;
  end;

  //Generate points on the line defined by endpoints (x1, y1) and (x2, y2):
  // (x - x1) / (x2 - x1) = (y - y1) / (y2 - y1)

  tk := GetTickCount64;
  repeat
    Application.ProcessMessages;
  until GetTickCount64 - tk >= 1;

  Randomize;

  ARealisticMovingPoints[0].tp := CurrentPoint;
  ARealisticMovingPoints[Length(ARealisticMovingPoints) - 1].tp := ADestPoint;

  X1 := CurrentPoint.X;
  X2 := ADestPoint.X;
  Y1 := CurrentPoint.Y;
  Y2 := ADestPoint.Y;

  X := X1;
  Y := Y1;
  XAmount := (X2 - X1) / (Length(ARealisticMovingPoints) - 1);  //Length() points, Length() - 1 spaces between points
  YAmount := (Y2 - Y1) / (Length(ARealisticMovingPoints) - 1);

  DivXAmount := XAmount {* 1.5} / 1;//18;  //the added error should be less than this value
  DivYAmount := YAmount {* 1.5} / 1;//18;

  for i := 1 to Length(ARealisticMovingPoints) - 2 do
  begin
    X := X + XAmount;
    Y := Y + YAmount;

    if i < Length(ARealisticMovingPoints) - 1 then
      if Random(3) > 0 then
      begin
        //using positive and negative offsets:
        X := X + Random(Round(DivXAmount * 1.2)) - Random(Round(DivXAmount * 1.2));
        Y := Y + Random(Round(DivYAmount)) - Random(Round(DivYAmount));
      end;

    ARealisticMovingPoints[i].tp.X := Round(X);
    ARealisticMovingPoints[i].tp.Y := Round(Y);
  end;
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
    i, GripCount: Integer;
    RealisticMovingPoints: TRMPointArr;
    RealisticMoving: Boolean;
    LineLength: Extended;
  begin
    ClickPoint.X := X;
    ClickPoint.Y := Y;

    MoveWithoutClick := AParams.Values['MoveWithoutClick'] = '1';
    DelayAfterMovingToDestination := Max(1, StrToIntDef(AParams.Values[CMouseDelayAfterMovingToDestination], 50));
    DelayAfterMouseDown := Max(1, StrToIntDef(AParams.Values[CMouseDelayAfterMouseDown], 200));
    MoveDuration := Max(-1, StrToIntDef(AParams.Values[CMouseMoveDuration], -1));
    RealisticMoving := AParams.Values[CMouseRealisticMoving] = '1';

    if MoveDuration < 1 then
      MoveMouseCursor(ClickPoint, UseClipCursor, False)
    else
    begin
      if not IsDragging then
      begin
        if not RealisticMoving then
          MoveMouseCursorWithDuration(ClickPoint, MoveDuration, UseClipCursor, MoveDuration > 5000)  //there will be race-conditions on client-server execution, if using App.ProcMsg
        else
        begin
          FirstGrip;
          GeneratePointsForRealisticMoving(RealisticMovingPoints, ClickPoint, MoveDuration, LineLength);
          Randomize;
          GripCount := 0;

          for i := 0 to Length(RealisticMovingPoints) - 1 do
          begin
            MoveMouseCursorWithDuration(RealisticMovingPoints[i].tp, Round(RealisticMovingPoints[i].MoveDuration), UseClipCursor, MoveDuration > 5000);
            Application.ProcessMessages; //Without this call, UIClicker "freezes" and it is not allowed to move the cursor. Because of this, the cursor doesn't end up at its destination.

            if (i > Length(RealisticMovingPoints) - 27) and  //i > .. means that this adjustment is made at the end of the movement
               (i < Length(RealisticMovingPoints) - 6) and
               (Random(10) = 8) and (GripCount < 2) and
               (LineLength > 450) then //do not do this for short distances
            begin
              Sleep(200 + Random(200)); //"better grip"
              Inc(GripCount);
            end;
          end;
        end;
      end;
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

      if not RealisticMoving then
        MoveMouseCursorWithDuration(DestPoint, MoveDuration, UseClipCursor, MoveDuration > 5000)  //there will be race-conditions on client-server execution, if using App.ProcMsg
      else
      begin
        FirstGrip;
        GeneratePointsForRealisticMoving(RealisticMovingPoints, DestPoint, MoveDuration, LineLength);
        Randomize;

        for i := 0 to Length(RealisticMovingPoints) - 1 do
        begin
          MoveMouseCursorWithDuration(RealisticMovingPoints[i].tp, Round(RealisticMovingPoints[i].MoveDuration), UseClipCursor, MoveDuration > 5000);
          Application.ProcessMessages;

          //Dragging will require further tweaking, because it is a precision operation.
          if (i > Length(RealisticMovingPoints) - 27) and  //i > .. means that this adjustment is made at the end of the movement
             (i < Length(RealisticMovingPoints) - 6) and
             (Random(10) = 8) and (GripCount < 3) and
             (LineLength > 450) then //do not do this for short distances
          begin
            Sleep(100 + Random(200)); //"better grip"
            Inc(GripCount);
          end;
        end;
      end;

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
{$ELSE}
  procedure ClickTControl(AParams: TStringList);
  begin
    //
  end;


  procedure MouseDownTControl(AParams: TStrings);
  begin
    //
  end;


  procedure MouseUpTControl(AParams: TStrings);
  begin
    //
  end;
{$ENDIF}


procedure MouseWheelTControl(AParams: TStrings);
var
  Amount: Integer;
begin
  Amount := StrToIntDef(AParams.Values[CMouseWheelAmount], 0);

  {$IFDEF Windows}
    if AParams.Values[CMouseWheelType] = CMouseWheelVertWheel then
      mouse_event(MOUSEEVENTF_WHEEL, 0, 0, DWord(Amount * WHEEL_DELTA), 0);

    if AParams.Values[CMouseWheelType] = CMouseWheelHorizWheel then
      mouse_event(MOUSEEVENTF_HWHEEL, 0, 0, DWord(Amount), 0);  //the official MSDN doc does not mention doing a multiplication by WHEEL_DELTA
  {$ELSE}
    //
  {$ENDIF}
end;

end.
