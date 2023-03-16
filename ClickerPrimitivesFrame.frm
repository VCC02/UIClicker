object frClickerPrimitives: TfrClickerPrimitives
  Left = 0
  Height = 260
  Top = 0
  Width = 399
  ClientHeight = 260
  ClientWidth = 399
  Color = clDefault
  Constraints.MinWidth = 399
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  DesignLeft = 86
  DesignTop = 85
  object pnlvstOI: TPanel
    Left = 0
    Height = 256
    Top = 0
    Width = 278
    Anchors = [akTop, akLeft, akBottom]
    BevelOuter = bvNone
    Caption = 'pnlvstOI'
    ClientHeight = 256
    ClientWidth = 278
    Color = 13500339
    Constraints.MinHeight = 208
    Constraints.MinWidth = 270
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    ParentColor = False
    ParentFont = False
    TabOrder = 0
    Visible = False
    object imgFontColorBuffer: TImage
      Left = 16
      Height = 16
      Top = 16
      Width = 16
      AutoSize = True
      Picture.Data = {
        07544269746D617036030000424D360300000000000036000000280000001000
        0000100000000100180000000000000300000000000000000000000000000000
        0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFF
      }
      Visible = False
    end
  end
  object pnlPreview: TPanel
    Left = 280
    Height = 258
    Top = 0
    Width = 118
    Anchors = [akTop, akLeft, akRight, akBottom]
    Caption = 'Preview'
    ClientHeight = 258
    ClientWidth = 118
    ParentColor = False
    TabOrder = 1
    object lblRenderingInfo: TLabel
      Left = 7
      Height = 30
      Top = 160
      Width = 106
      Alignment = taCenter
      Anchors = [akTop]
      Caption = 'Please add'#13#10'composition orders.'
    end
  end
  object tmrReloadOIContent: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrReloadOIContentTimer
    Left = 208
    Top = 192
  end
end
