{
    Copyright (C) 2022 VCC
    creation date: Feb 2023
    initial release date: 13 Feb 2023

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


unit ClickerSleepFrame;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls, StdCtrls;

type

  { TfrClickerSleep }

  TfrClickerSleep = class(TFrame)
    lblSleepInfo: TLabel;
    pnlSleepElapsedTime: TPanel;
    pnlSleepRemainingTime: TPanel;
    prbSleep: TProgressBar;
  private
    procedure SetProgressBarMax(AValue: Integer);
    procedure SetProgressBarPosition(AValue: Integer);
  public
    property ProgressBarMax: Integer write SetProgressBarMax;
    property ProgressBarPosition: Integer write SetProgressBarPosition;

    procedure SetEditorSleepInfo(AElapsedTime, ARemainingTime: string);
  end;


implementation

{$R *.frm}


procedure TfrClickerSleep.SetProgressBarMax(AValue: Integer);
begin
  prbSleep.Max := AValue;
end;


procedure TfrClickerSleep.SetProgressBarPosition(AValue: Integer);
begin
  prbSleep.Position := AValue;
end;


procedure TfrClickerSleep.SetEditorSleepInfo(AElapsedTime, ARemainingTime: string);
begin
  pnlSleepElapsedTime.Caption := AElapsedTime;
  pnlSleepRemainingTime.Caption := ARemainingTime;
end;

end.

