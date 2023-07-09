object frClickerWinInterp: TfrClickerWinInterp
  Left = 0
  Height = 501
  Top = 0
  Width = 893
  ClientHeight = 501
  ClientWidth = 893
  TabOrder = 0
  DesignLeft = 86
  DesignTop = 85
  object scrboxScannedComponents: TScrollBox
    Left = 8
    Height = 456
    Top = 37
    Width = 416
    HorzScrollBar.Page = 265
    HorzScrollBar.Tracking = True
    VertScrollBar.Page = 265
    VertScrollBar.Tracking = True
    Anchors = [akTop, akLeft, akRight, akBottom]
    ClientHeight = 452
    ClientWidth = 412
    TabOrder = 0
    OnMouseWheel = scrboxScannedComponentsMouseWheel
    object imgScreenshot: TImage
      Left = 28
      Height = 121
      Top = 28
      Width = 121
      OnMouseDown = imgScannedWindowMouseDown
      OnMouseMove = imgScannedWindowMouseMove
      PopupMenu = pmScreenshot
      Visible = False
    end
    object imgAvgScreenshotAndGreenComp: TImage
      Left = 56
      Height = 121
      Top = 56
      Width = 121
      OnMouseDown = imgScannedWindowMouseDown
      OnMouseMove = imgScannedWindowMouseMove
      PopupMenu = pmScreenshot
      Visible = False
    end
    object imgAvgScreenshotAndAssignedComp: TImage
      Left = 84
      Height = 121
      Top = 84
      Width = 121
      OnMouseDown = imgScannedWindowMouseDown
      OnMouseMove = imgScannedWindowMouseMove
      PopupMenu = pmScreenshot
      Visible = False
    end
    object imgScannedWindow: TImage
      Left = 0
      Height = 121
      Top = 0
      Width = 121
      OnMouseDown = imgScannedWindowMouseDown
      OnMouseMove = imgScannedWindowMouseMove
      PopupMenu = pmScreenshot
    end
    object imgLiveScreenshot: TImage
      Left = 113
      Height = 121
      Top = 113
      Width = 121
      OnMouseDown = imgLiveScreenshotMouseDown
      Visible = False
    end
    object imgHandleColors: TImage
      Left = 144
      Height = 121
      Top = 144
      Width = 121
      OnMouseDown = imgScannedWindowMouseDown
      OnMouseMove = imgScannedWindowMouseMove
      PopupMenu = pmScreenshot
      Visible = False
    end
  end
  object pnlDrag: TPanel
    Left = 8
    Height = 26
    Hint = 'Use this box to get the target component from another window.'
    Top = 8
    Width = 322
    Caption = 'Drag the mouse cursor, from this box to the target window'
    Color = clYellow
    ParentColor = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnMouseDown = pnlDragMouseDown
    OnMouseMove = pnlDragMouseMove
    OnMouseUp = pnlDragMouseUp
  end
  object memCompInfo: TMemo
    Left = 432
    Height = 93
    Top = 400
    Width = 264
    Anchors = [akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object btnStartRec: TButton
    Left = 704
    Height = 25
    Top = 304
    Width = 97
    Anchors = [akRight, akBottom]
    Caption = 'Start Recording'
    OnClick = btnStartRecClick
    TabOrder = 3
  end
  object btnStopRec: TButton
    Left = 704
    Height = 25
    Top = 334
    Width = 97
    Anchors = [akRight, akBottom]
    Caption = 'Stop Recording'
    OnClick = btnStopRecClick
    TabOrder = 4
  end
  object prbRecording: TProgressBar
    Left = 704
    Height = 16
    Top = 280
    Width = 140
    Anchors = [akRight, akBottom]
    Smooth = True
    TabOrder = 5
  end
  object chkMinimizeWhileRecording: TCheckBox
    Left = 704
    Height = 19
    Top = 376
    Width = 140
    Anchors = [akRight, akBottom]
    Caption = 'Minimize on recording'
    TabOrder = 6
  end
  object lbeStep: TLabeledEdit
    Left = 832
    Height = 23
    Hint = 'Scanning granularity'
    Top = 336
    Width = 48
    Anchors = [akRight, akBottom]
    EditLabel.Height = 15
    EditLabel.Width = 48
    EditLabel.Caption = 'Step [px]'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    Text = '1'
  end
  object lblGauge: TLabel
    Left = 872
    Height = 15
    Top = 280
    Width = 16
    Anchors = [akRight, akBottom]
    Caption = '0%'
  end
  object chkHighlightSelectedComponent: TCheckBox
    Left = 432
    Height = 19
    Top = 376
    Width = 181
    Anchors = [akRight, akBottom]
    Caption = 'Highlight selected component'
    Checked = True
    OnChange = chkHighlightSelectedComponentChange
    State = cbChecked
    TabOrder = 8
  end
  object rdgrpLayers: TRadioGroup
    Left = 432
    Height = 96
    Top = 272
    Width = 249
    Anchors = [akRight, akBottom]
    AutoFill = True
    Caption = 'Layers'
    ChildSizing.LeftRightSpacing = 6
    ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
    ChildSizing.EnlargeVertical = crsHomogenousChildResize
    ChildSizing.ShrinkHorizontal = crsScaleChilds
    ChildSizing.ShrinkVertical = crsScaleChilds
    ChildSizing.Layout = cclLeftToRightThenTopToBottom
    ChildSizing.ControlsPerLine = 1
    ClientHeight = 76
    ClientWidth = 245
    ItemIndex = 1
    Items.Strings = (
      'Screenshot'
      'Component rectangles'
      'Avg(Screenshot, Green Comp Rect)'
      'Avg(Screenshot, Assigned Comp Rect)'
    )
    OnClick = rdgrpLayersClick
    TabOrder = 9
  end
  object pnlMouseCoordsOnScreenshot: TPanel
    Left = 336
    Height = 25
    Top = 8
    Width = 88
    Caption = '0:0'
    TabOrder = 10
  end
  object lblHighlightingLabels: TLabel
    Left = 704
    Height = 15
    Top = 400
    Width = 124
    Anchors = [akRight, akBottom]
    Caption = 'Highlighting lines color'
  end
  object btnExport: TButton
    Left = 813
    Height = 25
    Hint = 'Export as YAML.'
    Top = 468
    Width = 75
    Anchors = [akRight, akBottom]
    Caption = 'Export...'
    OnClick = btnExportClick
    ParentShowHint = False
    ShowHint = True
    TabOrder = 11
  end
  object btnSaveTree: TButton
    Left = 704
    Height = 25
    Hint = 'Save to binary file.'#13#10'The Left/Top/Right/Bottom values depend on window position on screen as it was recorded.'
    Top = 468
    Width = 75
    Anchors = [akRight, akBottom]
    Caption = 'Save Tree...'
    OnClick = btnSaveTreeClick
    ParentShowHint = False
    ShowHint = True
    TabOrder = 12
  end
  object btnLoadTree: TButton
    Left = 704
    Height = 25
    Hint = 'Load from binary file.'
    Top = 442
    Width = 75
    Anchors = [akRight, akBottom]
    Caption = 'Load Tree...'
    OnClick = btnLoadTreeClick
    ParentShowHint = False
    ShowHint = True
    TabOrder = 13
  end
  object spdbtnExtraRecording: TSpeedButton
    Left = 800
    Height = 25
    Top = 304
    Width = 18
    Anchors = [akRight, akBottom]
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
    OnClick = spdbtnExtraRecordingClick
    ParentFont = False
  end
  object imgSpinner: TImage
    Left = 802
    Height = 16
    Hint = 'Waiting for server...'
    Top = 334
    Width = 16
    Anchors = [akRight, akBottom]
    ParentShowHint = False
    Picture.Data = {
      1754506F727461626C654E6574776F726B47726170686963B900000089504E47
      0D0A1A0A0000000D494844520000001000000010080200000090916836000000
      017352474200AECE1CE90000000467414D410000B18F0BFC6105000000097048
      597300000EC300000EC301C76FA8640000004E49444154384F63F84F22006978
      FF64033252DAE80347685258340015FDFFC40047687AD035405443CC8620343D
      434003104114C111B26A20C2A20188206643109A14760D78D048D64002F8FF1F
      0028306487B25356190000000049454E44AE426082
    }
    ShowHint = True
    Visible = False
  end
  object imgSpinnerDiff: TImage
    Left = 686
    Height = 16
    Hint = 'Found subcontrol...'
    Top = 280
    Width = 16
    Anchors = [akRight, akBottom]
    ParentShowHint = False
    Picture.Data = {
      1754506F727461626C654E6574776F726B47726170686963B900000089504E47
      0D0A1A0A0000000D494844520000001000000010080200000090916836000000
      017352474200AECE1CE90000000467414D410000B18F0BFC6105000000097048
      597300000EC300000EC301C76FA8640000004E49444154384F63F84F22006978
      FF64033252DAE80347685258340015FDFFC40047687AD035405443CC8620343D
      434003104114C111B26A20C2A20188206643109A14760D78D048D64002F8FF1F
      0028306487B25356190000000049454E44AE426082
    }
    ShowHint = True
    Visible = False
  end
  object pnlvstComponents: TPanel
    Left = 432
    Height = 256
    Top = 8
    Width = 456
    Anchors = [akTop, akLeft, akBottom]
    Caption = 'pnlvstComponents'
    Color = clYellow
    ParentColor = False
    TabOrder = 14
  end
  object colboxHighlightingLabels: TColorBox
    Left = 704
    Height = 22
    Top = 417
    Width = 184
    Anchors = [akRight, akBottom]
    ItemHeight = 16
    OnSelect = colboxHighlightingLabelsSelect
    TabOrder = 15
  end
  object pmComponents: TPopupMenu
    Left = 687
    Top = 211
    object MenuItemCopyFindControlActionsToClipBoard: TMenuItem
      Caption = 'Create FindControl actions and copy them to clipboard'
      OnClick = MenuItemCopyFindControlActionsToClipBoardClick
    end
    object MenuItemCopyFindControlAndClickActionsToClipBoard: TMenuItem
      Caption = 'Create FindControl (+Click) actions and copy them to clipboard'
      OnClick = MenuItemCopyFindControlAndClickActionsToClipBoardClick
    end
    object MenuItemCopyFindControlAndCachePositionActionsToClipBoard: TMenuItem
      Caption = 'Create FindControl (+Cache position) actions and copy them to clipboard'
      OnClick = MenuItemCopyFindControlAndCachePositionActionsToClipBoardClick
    end
  end
  object pmScreenshot: TPopupMenu
    Left = 307
    Top = 346
    object MenuItem_CopySelectionToClipboard: TMenuItem
      Caption = 'Copy selection to clipboard'
      OnClick = MenuItem_CopySelectionToClipboardClick
    end
    object MenuItem_SaveSelectionToFile: TMenuItem
      Caption = 'Save selection to file...'
      OnClick = MenuItem_SaveSelectionToFileClick
    end
    object MenuItem_CopySelectedComponentToClipboard: TMenuItem
      Caption = 'Copy selected component to clipboard'
      OnClick = MenuItem_CopySelectedComponentToClipboardClick
    end
    object MenuItem_SaveSelectedComponentToFile: TMenuItem
      Caption = 'Save selected component to file...'
      OnClick = MenuItem_SaveSelectedComponentToFileClick
    end
  end
  object pmExtraRecording: TPopupMenu
    Left = 628
    Top = 436
    object MenuItem_RecordFromRemote: TMenuItem
      Caption = 'Record from remote...'
      Bitmap.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000064000000640000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF33761633761633761633761633761633761633
        7616337616337616337616337616337616337616337616FFFFFFFFFFFF337616
        C9F2B7C9F2B7C9F2B7C9F2B7C9F2B7C9F2B7C9F2B7C9F2B7C9F2B7C9F2B7C9F2
        B7C9F2B7337616FFFFFFFFFFFF337616C9F2B7C9F2B7C9F2B7C9F2B7C9F2B7C9
        F2B7C9F2B7C9F2B7C9F2B7C9F2B7C9F2B7C9F2B7337616FFFFFFFFFFFF337616
        C9F2B7C9F2B73376163376163376163376163376163376163376163376163376
        16C9F2B7337616FFFFFFFFFFFF337616C9F2B7C9F2B7337616C9F2B7C9F2B7C9
        F2B7C9F2B7C9F2B7C9F2B7C9F2B7337616C9F2B7337616FFFFFFFFFFFF337616
        C9F2B7C9F2B7337616C9F2B7C9F2B7337616337616337616337616C9F2B73376
        16C9F2B7337616FFFFFFFFFFFF337616C9F2B7C9F2B7337616C9F2B7C9F2B733
        7616B5ED9CB5ED9C337616C9F2B7337616C9F2B7337616FFFFFFFFFFFF337616
        C9F2B7C9F2B7337616C9F2B7C9F2B7337616B5ED9CB5ED9C337616C9F2B73376
        16C9F2B7337616FFFFFFFFFFFF337616C9F2B7C9F2B7337616C9F2B7C9F2B733
        7616337616337616337616C9F2B7337616C9F2B7337616FFFFFFFFFFFF337616
        C9F2B7C9F2B7337616C9F2B7C9F2B7C9F2B7C9F2B7C9F2B7C9F2B7C9F2B73376
        16C9F2B7337616FFFFFFFFFFFF337616C9F2B7C9F2B7337616C9F2B7C9F2B7C9
        F2B7C9F2B7C9F2B7C9F2B7C9F2B7337616C9F2B7337616FFFFFFFFFFFF337616
        C9F2B7C9F2B73376163376163376163376163376163376163376163376163376
        16C9F2B7337616FFFFFFFFFFFF337616C9F2B7C9F2B7C9F2B7C9F2B7C9F2B7C9
        F2B7C9F2B7C9F2B7C9F2B7C9F2B7C9F2B7C9F2B7337616FFFFFFFFFFFF337616
        3376163376163376163376163376163376163376163376163376163376163376
        16337616337616FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      }
      OnClick = MenuItem_RecordFromRemoteClick
    end
    object Separator1: TMenuItem
      Caption = '-'
    end
    object MenuItemRecordWithMouseSwipe: TMenuItem
      Caption = 'Record with mouse swipe'
      Bitmap.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FFFFFFF9F7F1
        FFFFFFFFFFFFC2C2C21919192E2E2E212121D5D5D5DADADA7575758181817979
        79E5E5E5E5E5E5A1A1A1FFFFFFFFFFFFFFFFFFFFFFFF5B5B5B3636363A3A3A54
        5454FFFFFF9C9C9C868686888888989898FFFFFFBBBBBBACACAC4B4B4BCACACA
        FFFFFFD5D5D52525257A7A7A1A1A1ABDBDBDE5E5E57C7C7CAFAFAF757575D7D7
        D7EDEDEDA6A6A6C8C8C81616162E2E2EC2C2C2A0A0A01C1C1C5C5C5C333333DA
        DADAC6C6C67676769D9D9D848484E5E5E5D8D8D8A2A2A2BCBCBC2E2E2E535353
        2E2E2E4848484242422424247A7A7A8181819191918D8D8D7B7B7BAFAFAFA9A9
        A9B4B4B4B1B1B1A5A5A5333333D5D5D55454540B0B0B7979791D1D1DBDBDBD9E
        9E9E818181858585777777D7D7D7C4C4C4B3B3B3B5B5B5A2A2A2393939000000
        0000009393936A6A6A1F1F1F2A2A2A3333331F1F1F4747477878787F7F7F8484
        84787878909090A3A3A33F3F3F00000000000000000000000000000000000074
        7474383838666666666666666666ABABAB878787979797979797434343000000
        000000000000000000000000959595323232BDBDBD666666666666BFBFBF8484
        84D7D7D7979797979797454545000000000000000000000000B8B8B83232329E
        9E9E666666666666D4D4D4848484C4C4C4979797979797E1E1E1464646000000
        0000000000000000003A3A3A7E7E7E666666666666666666888888B1B1B19797
        97979797979797AEAEAE4646460000000000000000004A4A4A62626266666666
        6666666666929292A0A0A0979797979797979797B5B5B5BEBEBE464646000000
        0000005F5F5F4D4D4D9090906666666666669F9F9F949494B3B3B39797979797
        97BDBDBDB6B6B6FFFFFF4646460000007A7A7A3F3F3FFFFFFF909090666666AF
        AFAF8B8B8BFFFFFFB3B3B3979797C8C8C8B0B0B0FFFFFFFFFFFF464646999999
        3A3A3AFFFFFFFFFFFF909090C1C1C1888888FFFFFFFFFFFFB3B3B3D4D4D4AEAE
        AEFFFFFFFFFFFFFFFFFF474747404040FFFFFFFFFFFFFFFFFF9090908C8C8CFF
        FFFFFFFFFFFFFFFFB3B3B3B0B0B0FFFFFFFFFFFFFFFFFFFFFFFF
      }
      OnClick = MenuItemRecordWithMouseSwipeClick
    end
  end
  object imglstSpinner: TImageList
    Left = 484
    Top = 436
    Bitmap = {
      4C7A080000001000000010000000CA0000000000000078DAED98410E80300804
      7D8BEFF1B1FA27EF267E00E3C1C43660A9A035B8076E9D36B2BBA196883A32D6
      382F94D730F549716B247E5FDFAD9414B707C71F6C7E3EB74744DEDA3F0FFDB4
      05DE876F5D4F7EFF950F4B7C290725FF97721899B7F6CF433FE41FF9FF022F79
      58EB7F2943DAFC49198ECE5BFBE7A11FF28FFCB7E471FFC1FD07F9075FCBE3FD
      E53FEF2FC87F1C5EA3BBC46B7DC7F135BE8FC85BFBE7A11FE67F5BBE463BC97F
      5AEF9CF93BDE8DC45BFBE7A11FFEFFBF917FCC3FCC3FDC1FDFCBFF0668FB3E98
    }
  end
  object tmrSpinner: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrSpinnerTimer
    Left = 788
    Top = 212
  end
end
