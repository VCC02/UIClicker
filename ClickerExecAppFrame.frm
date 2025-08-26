object frClickerExecApp: TfrClickerExecApp
  Left = 0
  Height = 109
  Top = 0
  Width = 333
  ClientHeight = 109
  ClientWidth = 333
  TabOrder = 0
  DesignLeft = 86
  DesignTop = 85
  object lblExecAppParams: TLabel
    Left = 2
    Height = 15
    Top = 0
    Width = 59
    Caption = 'Parameters'
  end
  object memExecAppParams: TMemo
    Left = 2
    Height = 89
    Hint = 'Enter each parameter on a new line.'#13#10'Do not add quotes when running executables.'#13#10'(Double) quotes may be needed when opening a weblink.'#13#10'Replacements are available.'
    Top = 16
    Width = 270
    Anchors = [akTop, akLeft, akRight, akBottom]
    ParentShowHint = False
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 0
    WordWrap = False
    OnChange = memExecAppParamsChange
  end
end
