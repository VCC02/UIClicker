object frClickerWinInterp: TfrClickerWinInterp
  Left = 0
  Height = 501
  Top = 0
  Width = 893
  ClientHeight = 501
  ClientWidth = 893
  ParentBackground = False
  TabOrder = 0
  OnResize = FrameResize
  DesignLeft = 86
  DesignTop = 85
  object pnlFrameBK: TPanel
    Left = 0
    Height = 503
    Top = 0
    Width = 893
    Anchors = [akTop, akLeft, akRight, akBottom]
    Caption = 'pnlFrameBK'
    ClientHeight = 503
    ClientWidth = 893
    TabOrder = 0
    object pnlWinInterpSettings: TPanel
      Left = 425
      Height = 493
      Top = 0
      Width = 468
      Anchors = [akTop, akLeft, akRight, akBottom]
      Caption = 'pnlWinInterpSettings'
      ClientHeight = 493
      ClientWidth = 468
      ParentBackground = False
      ParentColor = False
      TabOrder = 0
      object memCompInfo: TMemo
        Left = 0
        Height = 93
        Top = 396
        Width = 264
        Anchors = [akLeft, akBottom]
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
      object btnStartRec: TButton
        Left = 273
        Height = 25
        Top = 320
        Width = 97
        Anchors = [akLeft, akBottom]
        Caption = 'Start Recording'
        TabOrder = 1
        OnClick = btnStartRecClick
      end
      object btnStopRec: TButton
        Left = 273
        Height = 25
        Top = 350
        Width = 97
        Anchors = [akLeft, akBottom]
        Caption = 'Stop Recording'
        TabOrder = 2
        OnClick = btnStopRecClick
      end
      object prbRecording: TProgressBar
        Left = 273
        Height = 16
        Top = 296
        Width = 140
        Anchors = [akLeft, akBottom]
        Smooth = True
        TabOrder = 3
      end
      object lbeStep: TLabeledEdit
        Left = 401
        Height = 23
        Hint = 'Scanning granularity'
        Top = 352
        Width = 48
        Anchors = [akLeft, akBottom]
        EditLabel.Height = 15
        EditLabel.Width = 48
        EditLabel.Caption = 'Step [px]'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        Text = '1'
      end
      object lblGauge: TLabel
        Left = 425
        Height = 15
        Top = 296
        Width = 16
        Anchors = [akLeft, akBottom]
        Caption = '0%'
      end
      object rdgrpLayers: TRadioGroup
        Left = 0
        Height = 104
        Hint = 'The live screenshot allows selecting an area by dragging.'#13#10'All the others select a component by clicking.'#13#10'Also, when the active page is Settings, the selection targets avoided zones.'
        Top = 288
        Width = 249
        Anchors = [akLeft, akBottom]
        AutoFill = True
        Caption = 'Layers'
        ChildSizing.LeftRightSpacing = 6
        ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
        ChildSizing.EnlargeVertical = crsHomogenousChildResize
        ChildSizing.ShrinkHorizontal = crsScaleChilds
        ChildSizing.ShrinkVertical = crsScaleChilds
        ChildSizing.Layout = cclLeftToRightThenTopToBottom
        ChildSizing.ControlsPerLine = 1
        ClientHeight = 84
        ClientWidth = 245
        ItemIndex = 1
        Items.Strings = (
          'Screenshot'
          'Component rectangles'
          'Avg(Screenshot, Green Comp Rect)'
          'Avg(Screenshot, Assigned Comp Rect)'
          'Live screenshot'
        )
        ParentBackground = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        OnClick = rdgrpLayersClick
      end
      object btnExport: TButton
        Left = 382
        Height = 25
        Hint = 'Export as YAML.'
        Top = 464
        Width = 75
        Anchors = [akLeft, akBottom]
        Caption = 'Export...'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
        OnClick = btnExportClick
      end
      object btnSaveTree: TButton
        Left = 273
        Height = 25
        Hint = 'Save to binary file.'#13#10'The Left/Top/Right/Bottom values depend on window position on screen as it was recorded.'
        Top = 464
        Width = 75
        Anchors = [akLeft, akBottom]
        Caption = 'Save Tree...'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
        OnClick = btnSaveTreeClick
      end
      object btnLoadTree: TButton
        Left = 273
        Height = 25
        Hint = 'Load from binary file.'
        Top = 438
        Width = 75
        Anchors = [akLeft, akBottom]
        Caption = 'Load Tree...'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
        OnClick = btnLoadTreeClick
      end
      object spdbtnExtraRecording: TSpeedButton
        Left = 369
        Height = 25
        Top = 320
        Width = 18
        Anchors = [akLeft, akBottom]
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
        OnClick = spdbtnExtraRecordingClick
      end
      object imgSpinner: TImage
        Left = 371
        Height = 16
        Hint = 'Waiting for server...'
        Top = 350
        Width = 16
        Anchors = [akLeft, akBottom]
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
        Left = 255
        Height = 16
        Hint = 'Found subcontrol...'
        Top = 296
        Width = 16
        Anchors = [akLeft, akBottom]
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
      object prbRecordingWithMouseSwipe: TProgressBar
        Left = 273
        Height = 8
        Top = 312
        Width = 140
        Anchors = [akLeft, akBottom]
        Smooth = True
        TabOrder = 9
        Visible = False
      end
      object PageControlWinInterp: TPageControl
        Left = 0
        Height = 288
        Top = 0
        Width = 465
        ActivePage = TabSheet_Components
        Anchors = [akTop, akLeft, akRight, akBottom]
        TabIndex = 0
        TabOrder = 10
        OnChange = PageControlWinInterpChange
        object TabSheet_Components: TTabSheet
          Caption = 'Components'
          ClientHeight = 260
          ClientWidth = 457
          object pnlvstComponents: TPanel
            Left = 0
            Height = 260
            Top = 0
            Width = 464
            Anchors = [akTop, akLeft, akRight, akBottom]
            Caption = 'pnlvstComponents'
            Color = clYellow
            ParentBackground = False
            ParentColor = False
            TabOrder = 0
          end
        end
        object TabSheet_Settings: TTabSheet
          Caption = 'Settings'
          ClientHeight = 260
          ClientWidth = 457
          object pnlvstSettings: TPanel
            Left = 0
            Height = 152
            Top = 16
            Width = 368
            Anchors = [akTop, akLeft, akRight, akBottom]
            Caption = 'pnlvstSettings'
            Color = 14090197
            ParentBackground = False
            ParentColor = False
            TabOrder = 0
          end
          object lblAvoidedZones: TLabel
            Left = 0
            Height = 15
            Top = 0
            Width = 285
            Caption = 'Avoided zones (used on recording with mouse swipe):'
          end
          object btNewZone: TButton
            Left = 372
            Height = 25
            Top = 16
            Width = 83
            Anchors = [akTop, akRight]
            Caption = 'New zone'
            TabOrder = 1
            OnClick = btNewZoneClick
          end
          object btnDeleteZone: TButton
            Left = 372
            Height = 25
            Top = 48
            Width = 83
            Anchors = [akTop, akRight]
            Caption = 'Delete zone...'
            TabOrder = 2
            OnClick = btnDeleteZoneClick
          end
          object chkFullScr: TCheckBox
            Left = 0
            Height = 19
            Hint = 'When checked, the MouseSwipe scan uses full screenshots, then crops the bitmaps to the required size.'#13#10'This is way slower than component screenshots, but it allows capturing content, which is not visible otherwise.'
            Top = 192
            Width = 125
            Anchors = [akLeft, akBottom]
            Caption = 'Full screen scanning'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 3
          end
          object chkMinimizeWhileRecording: TCheckBox
            Left = 148
            Height = 19
            Top = 238
            Width = 138
            Anchors = [akLeft, akBottom]
            Caption = 'Minimize on recording'
            TabOrder = 4
          end
          object lblHighlightingLabels: TLabel
            Left = 0
            Height = 15
            Top = 216
            Width = 124
            Anchors = [akLeft, akBottom]
            Caption = 'Highlighting lines color'
          end
          object colboxHighlightingLabels: TColorBox
            Left = 0
            Height = 22
            Top = 235
            Width = 136
            Anchors = [akLeft, akBottom]
            ItemHeight = 16
            TabOrder = 5
            OnSelect = colboxHighlightingLabelsSelect
          end
          object chkUseHCursor: TCheckBox
            Left = 224
            Height = 19
            Hint = '[in work]'#13#10'Reads mouse cursor handle on every position during recording with mouse swipe.'#13#10'This may be useful when interactable components do not display a visual feedback while hovered,'#13#10'so the scan relies on changing the mouse cursor to detect component edges.'
            Top = 168
            Width = 82
            Anchors = [akLeft, akBottom]
            Caption = 'Use hCursor'
            Checked = True
            ParentShowHint = False
            ShowHint = True
            State = cbChecked
            TabOrder = 6
          end
          object chkRecordSelectedAreaOnly: TCheckBox
            Left = 148
            Height = 19
            Top = 192
            Width = 152
            Anchors = [akLeft, akBottom]
            Caption = 'Record selected area only'
            TabOrder = 7
            OnChange = chkRecordSelectedAreaOnlyChange
          end
          object chkRecordWithEdgeExtending: TCheckBox
            Left = 148
            Height = 19
            Hint = 'When checked, scanned component edges are "extended" until the screenshot changes.'#13#10'If there are two adjacent components, they may end up being recorded as a single component.'#13#10'When unchecked, many edges and corners might be reported as new subcomponents (false positives).'
            Top = 216
            Width = 165
            Anchors = [akLeft, akBottom]
            Caption = 'Record with edge extending'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 8
            OnChange = chkRecordWithEdgeExtendingChange
          end
          object btnLoadZones: TButton
            Left = 372
            Height = 25
            Top = 80
            Width = 83
            Anchors = [akTop, akRight]
            Caption = 'Load zones...'
            TabOrder = 9
            OnClick = btnLoadZonesClick
          end
          object btnSaveZones: TButton
            Left = 372
            Height = 25
            Top = 111
            Width = 83
            Anchors = [akTop, akRight]
            Caption = 'Save zones...'
            TabOrder = 10
            OnClick = btnSaveZonesClick
          end
          object spdbtnMoveUp: TSpeedButton
            Left = 372
            Height = 25
            Hint = 'Move Up'
            Top = 143
            Width = 20
            Anchors = [akTop, akRight]
            Font.Color = clWindowText
            Glyph.Data = {
              EA000000424DEA0000000000000036000000280000000B000000050000000100
              180000000000B400000000000000000000000000000000000000FFFFFF39841A
              39841A39841A39841A39841A39841A39841A39841A39841AFFFFFF000000FFFF
              FFFFFFFF39841A39841A39841A39841A39841A39841A39841AFFFFFFFFFFFF00
              0000FFFFFFFFFFFFFFFFFF39841A39841A39841A39841A39841AFFFFFFFFFFFF
              FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFF39841A39841A39841AFFFFFFFFFF
              FFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF39841AFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFF000000
            }
            ShowHint = True
            ParentFont = False
            ParentShowHint = False
            OnClick = spdbtnMoveUpClick
          end
          object spdbtnMoveDown: TSpeedButton
            Left = 391
            Height = 25
            Hint = 'Move Down'
            Top = 143
            Width = 20
            Anchors = [akTop, akRight]
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
            ShowHint = True
            ParentFont = False
            ParentShowHint = False
            OnClick = spdbtnMoveDownClick
          end
          object lbeMouseCursorPosToScreenshotDelay: TLabeledEdit
            Left = 324
            Height = 23
            Hint = 'Delay in ms, between setting the mouse cursor position and taking the (component) screenshot.'#13#10'Some UIs required a greater delay because of animations. This will lead to longer scanning time.'#13#10'Valid values are from 0 to 1000.'#13#10'For example, on a 32x32 scanning area and a configured value of 1, this delay only will take at least 1024ms.'#13#10'This option is used by recording with mouse swipe.'
            Top = 188
            Width = 124
            Anchors = [akLeft, akBottom]
            EditLabel.Height = 15
            EditLabel.Width = 124
            EditLabel.Caption = 'Mouse to scr delay'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 11
            Text = '1'
          end
          object chkHighlightSelectedComponent: TCheckBox
            Left = 0
            Height = 19
            Top = 168
            Width = 179
            Anchors = [akLeft, akBottom]
            Caption = 'Highlight selected component'
            Checked = True
            State = cbChecked
            TabOrder = 12
            OnChange = chkHighlightSelectedComponentChange
          end
          object chkBringTargetToFront: TCheckBox
            Left = 324
            Height = 19
            Hint = 'Focuses target window, to avoid being overlapped by any other windows.'#13#10'There are cases where this may not work (different window states, inclusing "Stay on top").'
            Top = 216
            Width = 123
            Anchors = [akLeft, akBottom]
            Caption = 'Bring target to front'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 13
          end
          object chkBringTargetToFrontPeriodically: TCheckBox
            Left = 324
            Height = 19
            Hint = 'When checked, the target window is brought to front on every new Y increment.'#13#10'This may be required when the target window keeps being overlayed by other windows.'
            Top = 239
            Width = 153
            Anchors = [akLeft, akBottom]
            Caption = 'Bring periodically to front'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 14
          end
          object btnClearZones: TButton
            Left = 410
            Height = 25
            Top = 143
            Width = 45
            Anchors = [akTop, akRight]
            Caption = 'Clear...'
            TabOrder = 15
            OnClick = btnClearZonesClick
          end
        end
      end
      object imgEnabledPause: TImage
        Left = 371
        Height = 16
        Hint = 'Press Shift-Esc during MouseSwipe recording, to pause.'
        Top = 367
        Width = 16
        Anchors = [akLeft, akBottom]
        ParentShowHint = False
        Picture.Data = {
          1754506F727461626C654E6574776F726B47726170686963A600000089504E47
          0D0A1A0A0000000D494844520000001000000010080200000090916836000000
          0467414D410000B18F0BFC6105000000097048597300000EC300000EC301C76F
          A8640000004849444154384F63FCFFFF3F032900A6E1BA27980B069ADBA10C20
          C0106782B041E0F331207A796F07940B07A8E2481A8803A31A8801C34BC3CBD7
          9F8008CA410268E224266F0606004FF1240F972595880000000049454E44AE42
          6082
        }
        ShowHint = True
      end
      object imgDisabledPause: TImage
        Left = 384
        Height = 16
        Hint = 'Press Shift-Esc, to continue.'
        Top = 367
        Width = 16
        Anchors = [akLeft, akBottom]
        ParentShowHint = False
        Picture.Data = {
          1754506F727461626C654E6574776F726B47726170686963A800000089504E47
          0D0A1A0A0000000D494844520000001000000010080200000090916836000000
          017352474200AECE1CE90000000467414D410000B18F0BFC6105000000097048
          597300000EC300000EC301C76FA8640000003D49444154384FEDCB310A003008
          04C13CD83FA4F5D726B882824DD23BE51EB7EC531CA4A02092A3E4613B55A5A0
          F739D401737079B80914F41E87576607076750B7795D07DE0000000049454E44
          AE426082
        }
        ShowHint = True
        Visible = False
      end
      object chkContinuouslyScreenshotByKeys: TCheckBox
        Left = 273
        Height = 19
        Hint = 'When checked, the screenshot image is updated by pressing/holding the Ctrl-Alt-Shift key combination.'#13#10'Make sure the live screenshot is displayed over the other layers.'#13#10'The live screenshot may be switched back automatically on a new recording.'
        Top = 396
        Width = 191
        Anchors = [akLeft, akBottom]
        Caption = 'Continuously screenshot by keys'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 11
        OnChange = chkContinuouslyScreenshotByKeysChange
      end
      object chkShowZoom: TCheckBox
        Left = 273
        Height = 19
        Top = 416
        Width = 80
        Anchors = [akLeft, akBottom]
        Caption = 'Show zoom'
        TabOrder = 12
      end
    end
    object pnlDrag: TPanel
      Left = 4
      Height = 26
      Hint = 'Use this box to get the target component from another window.'
      Top = 8
      Width = 322
      Caption = 'Drag the mouse cursor, from this box to the target window'
      Color = clYellow
      ParentBackground = False
      ParentColor = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnMouseDown = pnlDragMouseDown
      OnMouseMove = pnlDragMouseMove
      OnMouseUp = pnlDragMouseUp
    end
    object pnlMouseCoordsOnScreenshot: TPanel
      Left = 332
      Height = 25
      Top = 9
      Width = 72
      Caption = '0:0'
      TabOrder = 2
    end
    object scrboxScannedComponents: TScrollBox
      Left = 4
      Height = 456
      Top = 37
      Width = 408
      HorzScrollBar.Page = 321
      HorzScrollBar.Tracking = True
      VertScrollBar.Page = 321
      VertScrollBar.Tracking = True
      Anchors = [akTop, akLeft, akRight, akBottom]
      ClientHeight = 452
      ClientWidth = 404
      TabOrder = 3
      OnMouseWheel = scrboxScannedComponentsMouseWheel
      object imgScreenshot: TImage
        Left = 28
        Height = 121
        Top = 28
        Width = 121
        PopupMenu = pmScreenshot
        Visible = False
        OnMouseDown = imgScannedWindowMouseDown
        OnMouseEnter = imgScannedWindowMouseEnter
        OnMouseLeave = imgScannedWindowMouseLeave
        OnMouseMove = imgScannedWindowMouseMove
      end
      object imgAvgScreenshotAndGreenComp: TImage
        Left = 56
        Height = 121
        Top = 56
        Width = 121
        PopupMenu = pmScreenshot
        Visible = False
        OnMouseDown = imgScannedWindowMouseDown
        OnMouseEnter = imgScannedWindowMouseEnter
        OnMouseLeave = imgScannedWindowMouseLeave
        OnMouseMove = imgScannedWindowMouseMove
      end
      object imgAvgScreenshotAndAssignedComp: TImage
        Left = 84
        Height = 121
        Top = 84
        Width = 121
        PopupMenu = pmScreenshot
        Visible = False
        OnMouseDown = imgScannedWindowMouseDown
        OnMouseEnter = imgScannedWindowMouseEnter
        OnMouseLeave = imgScannedWindowMouseLeave
        OnMouseMove = imgScannedWindowMouseMove
      end
      object imgScannedWindow: TImage
        Left = 0
        Height = 121
        Top = 0
        Width = 121
        PopupMenu = pmScreenshot
        OnMouseDown = imgScannedWindowMouseDown
        OnMouseEnter = imgScannedWindowMouseEnter
        OnMouseLeave = imgScannedWindowMouseLeave
        OnMouseMove = imgScannedWindowMouseMove
      end
      object imgLiveScreenshot: TImage
        Left = 113
        Height = 121
        Hint = 'Live screenshot'#13#10'The live screenshot allows selecting an area by dragging.'#13#10'Right-click and select "Update tree values from selection", to update the tree with the new selection values.'
        Top = 112
        Width = 121
        ParentShowHint = False
        PopupMenu = pmScreenshot
        ShowHint = True
        Visible = False
        OnMouseDown = imgLiveScreenshotMouseDown
        OnMouseEnter = imgScannedWindowMouseEnter
        OnMouseLeave = imgScannedWindowMouseLeave
        OnMouseMove = imgLiveScreenshotMouseMove
        OnMouseUp = imgLiveScreenshotMouseUp
      end
      object imgHandleColors: TImage
        Left = 144
        Height = 121
        Top = 144
        Width = 121
        PopupMenu = pmScreenshot
        Visible = False
        OnMouseDown = imgScannedWindowMouseDown
        OnMouseEnter = imgScannedWindowMouseEnter
        OnMouseLeave = imgScannedWindowMouseLeave
        OnMouseMove = imgScannedWindowMouseMove
      end
      object imgScannedWindowWithText: TImage
        Left = 172
        Height = 121
        Top = 172
        Width = 121
        ParentShowHint = False
        PopupMenu = pmScreenshot
        Visible = False
        OnMouseDown = imgScannedWindowMouseDown
        OnMouseEnter = imgScannedWindowMouseEnter
        OnMouseLeave = imgScannedWindowMouseLeave
        OnMouseMove = imgScannedWindowMouseMove
      end
      object imgScannedWindowWithAvoidedZones: TImage
        Left = 200
        Height = 121
        Hint = 'Avoided zones'
        Top = 200
        Width = 121
        ParentShowHint = False
        ShowHint = True
        Transparent = True
        Visible = False
        OnMouseDown = imgScannedWindowWithAvoidedZonesMouseDown
        OnMouseEnter = imgScannedWindowWithAvoidedZonesMouseEnter
        OnMouseLeave = imgScannedWindowWithAvoidedZonesMouseLeave
        OnMouseMove = imgScannedWindowWithAvoidedZonesMouseMove
      end
    end
    object pnlHorizSplitter: TPanel
      Cursor = crHSplit
      Left = 413
      Height = 493
      Top = 0
      Width = 11
      Anchors = [akTop, akLeft, akBottom]
      Color = 13041606
      ParentBackground = False
      ParentColor = False
      TabOrder = 4
      OnMouseDown = pnlHorizSplitterMouseDown
      OnMouseMove = pnlHorizSplitterMouseMove
      OnMouseUp = pnlHorizSplitterMouseUp
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
    object Separator3: TMenuItem
      Caption = '-'
    end
    object MenuItem_UpdateTreeValuesFromSelection: TMenuItem
      Caption = 'Update tree values from selection'
      OnClick = MenuItem_UpdateTreeValuesFromSelectionClick
    end
    object MenuItem_UpdateTreeValuesFromSelectionToANewComponent: TMenuItem
      Caption = 'Update tree values from selection to a new component'
      OnClick = MenuItem_UpdateTreeValuesFromSelectionToANewComponentClick
    end
    object Separator6: TMenuItem
      Caption = '-'
    end
    object MenuItem_ClearScreenshots: TMenuItem
      Caption = 'Clear screenshots'
      OnClick = MenuItem_ClearScreenshotsClick
    end
    object Separator7: TMenuItem
      Caption = '-'
    end
    object MenuItem_ShowLiveScreenshot: TMenuItem
      Caption = 'Show live screenshot'
    end
    object MenuItem_HideLiveScreenshot: TMenuItem
      Caption = 'Hide live screenshot'
    end
    object MenuItem_CopyLiveScreenshotToMainScreenshot: TMenuItem
      Caption = 'Copy live screenshot to main screenshot'
      OnClick = MenuItem_CopyLiveScreenshotToMainScreenshotClick
    end
  end
  object pmComponents: TPopupMenu
    Left = 280
    Top = 231
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
    object Separator5: TMenuItem
      Caption = '-'
    end
    object MenuItem_AddSubcomponent: TMenuItem
      Caption = 'Add subcomponent'
      OnClick = MenuItem_AddSubcomponentClick
    end
    object MenuItem_DeleteSubComponent: TMenuItem
      Caption = 'Delete (sub)component'
      OnClick = MenuItem_DeleteSubComponentClick
    end
  end
  object pmExtraRecording: TPopupMenu
    Left = 221
    Top = 432
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
    object Separator2: TMenuItem
      Caption = '-'
    end
    object MenuItem_RecordMultipleSizes: TMenuItem
      Caption = 'Record multiple sizes...'
      OnClick = MenuItem_RecordMultipleSizesClick
    end
    object MenuItem_ConfigureMultiSizeRecording: TMenuItem
      Caption = 'Configure multi-size recording...'
      Enabled = False
    end
    object Separator4: TMenuItem
      Caption = '-'
    end
  end
  object imglstSpinner: TImageList
    Left = 77
    Top = 432
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
    Left = 381
    Top = 232
  end
  object tmrScan: TTimer
    Enabled = False
    Interval = 1
    OnTimer = tmrScanTimer
    Left = 307
    Top = 413
  end
  object tmrEditComponents: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrEditComponentsTimer
    Left = 456
    Top = 16
  end
  object tmrEditSettings: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrEditSettingsTimer
    Left = 584
    Top = 16
  end
  object pmAvoidedZones: TPopupMenu
    Left = 728
    Top = 16
    object MenuItem_ClearZones: TMenuItem
      Caption = 'Clear zones'
      OnClick = MenuItem_ClearZonesClick
    end
  end
  object tmrScanByKeys: TTimer
    Enabled = False
    Interval = 1
    OnTimer = tmrScanByKeysTimer
    Left = 624
    Top = 413
  end
  object pmVarsEditor: TPopupMenu
    Left = 728
    Top = 79
  end
  object tmrDrawZoom: TTimer
    Enabled = False
    Interval = 50
    OnTimer = tmrDrawZoomTimer
    Left = 456
    Top = 80
  end
end
