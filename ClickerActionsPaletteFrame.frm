object frClickerActionsPalette: TfrClickerActionsPalette
  Left = 0
  Height = 240
  Top = 0
  Width = 171
  ClientHeight = 240
  ClientWidth = 171
  TabOrder = 0
  DesignLeft = 86
  DesignTop = 85
  object vstActionsPalette: TVirtualStringTree
    Left = 0
    Height = 240
    Top = 0
    Width = 171
    Anchors = [akTop, akLeft, akRight, akBottom]
    Colors.UnfocusedColor = clMedGray
    DefaultNodeHeight = 26
    DefaultText = 'Node'
    Header.AutoSizeIndex = 0
    Header.Columns = <>
    Header.DefaultHeight = 17
    Header.Height = 17
    Header.MainColumn = -1
    Indent = 4
    TabOrder = 0
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnGetText = vstActionsPaletteGetText
    OnGetImageIndex = vstActionsPaletteGetImageIndex
  end
  object tmrHide: TTimer
    Enabled = False
    Interval = 1
    OnTimer = tmrHideTimer
    Left = 50
    Top = 24
  end
end
