object frClickerSleep: TfrClickerSleep
  Left = 0
  Height = 240
  Top = 0
  Width = 434
  ClientHeight = 240
  ClientWidth = 434
  TabOrder = 0
  DesignLeft = 86
  DesignTop = 85
  object pnlSleepElapsedTime: TPanel
    Left = 3
    Height = 24
    Top = 88
    Width = 182
    Alignment = taLeftJustify
    Caption = 'Elapsed Time [ms]:'
    TabOrder = 0
  end
  object pnlSleepRemainingTime: TPanel
    Left = 3
    Height = 24
    Top = 118
    Width = 182
    Alignment = taLeftJustify
    Caption = 'Remaining Time [ms]:'
    TabOrder = 1
  end
  object prbSleep: TProgressBar
    Left = 3
    Height = 17
    Top = 148
    Width = 182
    Smooth = True
    TabOrder = 2
  end
  object lblSleepInfo: TLabel
    Left = 0
    Height = 15
    Hint = 'Waiting for the proper event or control property, is the right way to solve a race condition. Use the "sleep" action if the event/property is not available.'
    Top = 64
    Width = 414
    Caption = 'Use this action only as a last resort (e.g. blinking or resizing controls/windows).'
    ParentShowHint = False
    ShowHint = True
  end
end
