object frmClickerRemoteScreen: TfrmClickerRemoteScreen
  Left = 387
  Height = 385
  Top = 43
  Width = 767
  Caption = 'Clicker Remote Screen'
  ClientHeight = 385
  ClientWidth = 767
  Constraints.MinHeight = 385
  Constraints.MinWidth = 767
  LCLVersion = '8.4'
  OnCreate = FormCreate
  object btnRefreshScreenshot: TButton
    Left = 8
    Height = 25
    Hint = 'Clicking sends a request to server, at the specified address in Actions window.'
    Top = 0
    Width = 115
    Caption = 'Refresh screenshot'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnClick = btnRefreshScreenshotClick
  end
  object scrboxScannedComponents: TScrollBox
    Left = 0
    Height = 328
    Top = 56
    Width = 764
    HorzScrollBar.Page = 184
    HorzScrollBar.Tracking = True
    VertScrollBar.Page = 184
    VertScrollBar.Tracking = True
    Anchors = [akTop, akLeft, akRight, akBottom]
    ClientHeight = 324
    ClientWidth = 760
    TabOrder = 1
    OnMouseWheel = scrboxScannedComponentsMouseWheel
    object imgScreenshot: TImage
      Left = 28
      Height = 156
      Top = 28
      Width = 156
      OnMouseDown = imgScreenshotMouseDown
      OnMouseUp = imgScreenshotMouseUp
    end
  end
  object lbeCompText: TLabeledEdit
    Left = 480
    Height = 23
    Top = 24
    Width = 136
    Color = clBtnFace
    EditLabel.Height = 15
    EditLabel.Width = 136
    EditLabel.Caption = 'Comp text'
    ReadOnly = True
    TabOrder = 2
  end
  object lbeCompClass: TLabeledEdit
    Left = 624
    Height = 23
    Top = 24
    Width = 136
    Color = clBtnFace
    EditLabel.Height = 15
    EditLabel.Width = 136
    EditLabel.Caption = 'Component class'
    ReadOnly = True
    TabOrder = 3
  end
  object lblInfo: TLabel
    Left = 8
    Height = 15
    Hint = 'Clicking sends a request to server, at the specified address in Actions window.'
    Top = 32
    Width = 327
    Caption = 'Click on the image below, to get the component info at point.'
    ParentShowHint = False
    ShowHint = True
  end
  object edtHandle: TEdit
    Left = 416
    Height = 15
    Top = 32
    Width = 61
    BorderStyle = bsNone
    Color = clBtnFace
    ParentShowHint = False
    ReadOnly = True
    ShowHint = True
    TabOrder = 4
  end
  object lblHandle: TLabel
    Left = 376
    Height = 15
    Top = 32
    Width = 38
    Caption = 'Handle'
    ParentShowHint = False
    ShowHint = True
  end
  object lblMouseTool: TLabel
    Left = 160
    Height = 15
    Top = 5
    Width = 63
    Caption = 'Mouse tool:'
  end
  object cmbMouseTool: TComboBox
    Left = 232
    Height = 21
    Hint = 'Do not use "Remote click" when both the client and server are running on the same machine.'
    Top = 2
    Width = 152
    ItemHeight = 15
    ItemIndex = 0
    Items.Strings = (
      'Component selection'
      'Remote click'
    )
    ParentShowHint = False
    ShowHint = True
    Style = csOwnerDrawFixed
    TabOrder = 5
    Text = 'Component selection'
  end
  object spdbtnExtraRefresh: TSpeedButton
    Left = 121
    Height = 25
    Top = 0
    Width = 18
    Font.Color = clWindowText
    Glyph.Data = {
      EA000000424DEA0000000000000036000000280000000B000000050000000100
      180000000000B400000000000000000000000000000000000000FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFF39841AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFF
      FFFFFFFFFFFFFFFFFFFF39841A39841A39841AFFFFFFFFFFFFFFFFFFFFFFFF00
      0000FFFFFFFFFFFFFFFFFF39841A39841A39841A39841A39841AFFFFFFFFFFFF
      FFFFFF000000FFFFFFFFFFFF39841A39841A39841A39841A39841A39841A3984
      1AFFFFFFFFFFFF000000FFFFFF39841A39841A39841A39841A39841A39841A39
      841A39841A39841AFFFFFF000000
    }
    ParentFont = False
    OnClick = spdbtnExtraRefreshClick
  end
  object pmExtraRefresh: TPopupMenu
    Left = 229
    Top = 64
    object MenuItem_RefreshAfterMouseUp: TMenuItem
      AutoCheck = True
      Caption = 'Refresh after "MouseUp"'
    end
  end
end
