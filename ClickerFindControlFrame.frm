object frClickerFindControl: TfrClickerFindControl
  Left = 0
  Height = 250
  Top = 0
  Width = 877
  ClientHeight = 250
  ClientWidth = 877
  Constraints.MinHeight = 250
  Constraints.MinWidth = 877
  TabOrder = 0
  DesignLeft = 86
  DesignTop = 85
  object PageControlMatch: TPageControl
    Left = 0
    Height = 250
    Hint = 'See matching criteria in the "Criteria" tab.'
    Top = 1
    Width = 877
    ActivePage = TabSheetActionFindSubControlSearchArea
    Anchors = [akTop, akLeft, akRight, akBottom]
    Constraints.MinHeight = 201
    Constraints.MinWidth = 650
    Font.Height = -11
    Font.Name = 'Tahoma'
    Images = imglstFindCriteria
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabIndex = 4
    TabOrder = 0
    OnChange = PageControlMatchChange
    object TabSheetActionFindSubControlCriteria: TTabSheet
      Caption = 'Criteria'
      ClientHeight = 175
      ClientWidth = 869
      ImageIndex = 2
      object lblInfoFindSubControl: TLabel
        Left = 3
        Height = 13
        Hint = 'The $Control_Left$, $Control_Top$, $Control_Width$, $Control_Height$, $Control_Right$, $Control_Bottom$ variables ar set with the subcontrol offset.'
        Top = 101
        Width = 447
        Caption = 'When selecting FindSubControl action, only bitmaps can be matched (BMP Text or BMP Files).'
        ParentShowHint = False
        ShowHint = True
      end
      object lblInfoFindSubControl2: TLabel
        Left = 3
        Height = 13
        Hint = 'The $Control_Left$, $Control_Top$, $Control_Width$, $Control_Height$, $Control_Right$, $Control_Bottom$ variables ar set with the subcontrol offset.'
        Top = 120
        Width = 346
        Caption = 'A SubControl does not have a handle of its own, it is a part of a control.'
        ParentShowHint = False
        ShowHint = True
      end
      object lblInfoFindSubControl3: TLabel
        Left = 3
        Height = 13
        Hint = 'The "Allowed Failed" response can be used for conditional execution (call action).'
        Top = 139
        Width = 440
        Caption = 'When the action is allowed to fail and it fails, $LastAction_Status$ is set to "Allowed Failed".'
        ParentShowHint = False
        ShowHint = True
      end
      object chkMatchText: TCheckBox
        Left = 3
        Height = 19
        Top = 0
        Width = 74
        Caption = 'Match Text'
        OnClick = chkMatchTextClick
        TabOrder = 0
      end
      object chkMatchBitmapText: TCheckBox
        Left = 3
        Height = 19
        Top = 46
        Width = 109
        Caption = 'Match Bitmap Text'
        OnClick = chkMatchBitmapTextClick
        TabOrder = 1
      end
      object chkMatchBitmapFiles: TCheckBox
        Left = 3
        Height = 19
        Top = 69
        Width = 108
        Caption = 'Match Bitmap Files'
        OnClick = chkMatchBitmapFilesClick
        TabOrder = 2
      end
      object chkMatchClassName: TCheckBox
        Left = 3
        Height = 19
        Top = 23
        Width = 107
        Caption = 'Match Class Name'
        OnClick = chkMatchClassNameClick
        TabOrder = 3
      end
      object rdgrpSearchForControlMode: TRadioGroup
        Left = 144
        Height = 82
        Top = 0
        Width = 137
        AutoFill = True
        Caption = 'Search For Control Mode'
        ChildSizing.LeftRightSpacing = 6
        ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
        ChildSizing.EnlargeVertical = crsHomogenousChildResize
        ChildSizing.ShrinkHorizontal = crsScaleChilds
        ChildSizing.ShrinkVertical = crsScaleChilds
        ChildSizing.Layout = cclLeftToRightThenTopToBottom
        ChildSizing.ControlsPerLine = 1
        ClientHeight = 64
        ClientWidth = 133
        ItemIndex = 0
        Items.Strings = (
          'Generate Grid (Slow)'
          'Enumerate Windows'
          'Find Window (Fast)'
        )
        OnClick = rdgrpSearchForControlModeClick
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
      end
      object chkAllowToFail: TCheckBox
        Left = 304
        Height = 19
        Hint = 'When checked, the execution flow does not stop if the searched (sub)control is not found.'
        Top = 3
        Width = 79
        Caption = 'Allow To Fail'
        OnClick = chkAllowToFailClick
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
      end
      object chkWaitForControlToGoAway: TCheckBox
        Left = 304
        Height = 19
        Hint = 'When checked, the action expects to find no control, using the current settings.'
        Top = 24
        Width = 160
        Caption = 'Wait For Control To Go Away'
        OnClick = chkWaitForControlToGoAwayClick
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
      end
    end
    object TabSheetActionFindSubControlText: TTabSheet
      Caption = 'Text'
      ClientHeight = 224
      ClientWidth = 869
      ImageIndex = 2
      object lbeMatchText: TLabeledEdit
        Left = 3
        Height = 21
        Hint = 'Wildcards are available ("*"). Variable replacements are available. Used on matching text and BMP text.'#13#10#13#10'For controls, which can have different text values (e.g. a window displaying a different title), these values can be e.g. comma separated. In that case, the text separator is a comma.'#13#10'For example: a window can display "MyTitle" or "MyTitle (modified)". In that case, the "Match Text" editbox can contain "MyTitle,MyTitle (modified)", without quotes, by using the comma separator.'
        Top = 24
        Width = 197
        EditLabel.Height = 13
        EditLabel.Width = 197
        EditLabel.Caption = 'Match Text'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnChange = lbeMatchTextChange
      end
      object lbeMatchClassName: TLabeledEdit
        Left = 3
        Height = 21
        Hint = 'Wildcards are available ("*"). Variable replacements are available.'#13#10'There are applications which can have one or more of their windows, registered with class name, containing a randomly generated string.'
        Top = 68
        Width = 197
        EditLabel.Height = 13
        EditLabel.Width = 197
        EditLabel.Caption = 'Match Class Name'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnChange = lbeMatchClassNameChange
      end
      object lbeMatchTextSeparator: TLabeledEdit
        Left = 208
        Height = 21
        Hint = 'No replacements available.'
        Top = 24
        Width = 107
        EditLabel.Height = 13
        EditLabel.Width = 107
        EditLabel.Caption = 'Match Text Separator'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnChange = lbeMatchTextSeparatorChange
      end
      object lbeMatchClassNameSeparator: TLabeledEdit
        Left = 208
        Height = 21
        Hint = 'No replacements available.'
        Top = 68
        Width = 107
        EditLabel.Height = 13
        EditLabel.Width = 107
        EditLabel.Caption = 'Match Class Separator'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnChange = lbeMatchClassNameSeparatorChange
      end
      object lblTextMatchingInfo: TLabel
        Left = 3
        Height = 13
        Top = 132
        Width = 133
        Caption = 'Text matching enabled.'
        Font.Color = clMaroon
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object grpFindControlDetailsOnWindow: TGroupBox
        Left = 320
        Height = 128
        Top = 8
        Width = 320
        Caption = 'Find control details on window'
        ClientHeight = 110
        ClientWidth = 316
        TabOrder = 4
        object pnlDrag: TPanel
          Left = 8
          Height = 26
          Hint = 'Use this box to get the target component from another window.'
          Top = 0
          Width = 304
          Caption = 'Drag the mouse cursor, from this box to the target window'
          Color = clYellow
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          ParentColor = False
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnMouseDown = pnlDragMouseDown
          OnMouseMove = pnlDragMouseMove
          OnMouseUp = pnlDragMouseUp
        end
        object lbeFoundControlText: TLabeledEdit
          Left = 8
          Height = 21
          Hint = 'Not all controls will have text.'
          Top = 44
          Width = 152
          Color = clScrollBar
          EditLabel.Height = 13
          EditLabel.Width = 152
          EditLabel.Caption = 'Found control text'
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 1
        end
        object lbeFoundControlClass: TLabeledEdit
          Left = 168
          Height = 21
          Top = 44
          Width = 144
          Color = clScrollBar
          EditLabel.Height = 13
          EditLabel.Width = 144
          EditLabel.Caption = 'Found control class'
          ReadOnly = True
          TabOrder = 2
        end
        object lblFoundControlInfo: TLabel
          Left = 8
          Height = 13
          Top = 72
          Width = 56
          Caption = 'Control info'
        end
        object chkAutoCopyValuesToMatchEditboxes: TCheckBox
          Left = 8
          Height = 19
          Hint = 'When checked, the contents of "Found control text" and "Found control class" editboxes, are copied to the "Match Text" and "Match Class Name" editboxes, after dragging the mouse cursor to a control.'
          Top = 91
          Width = 203
          Caption = 'Autocopy values to "Match" editboxes'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
      end
      object btnCopyFoundValues: TButton
        Left = 3
        Height = 25
        Top = 92
        Width = 180
        Caption = 'Copy values from found control'
        OnClick = btnCopyFoundValuesClick
        TabOrder = 5
      end
      object spdbtnExtraCopyValueWindows: TSpeedButton
        Left = 182
        Height = 25
        Top = 92
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
        OnClick = spdbtnExtraCopyValueWindowsClick
        ParentFont = False
      end
    end
    object TabSheetActionFindSubControlBMPText: TTabSheet
      Caption = 'BMP Text'
      ClientHeight = 224
      ClientWidth = 869
      ImageIndex = 2
      object lblBMPTextMatchingInfo: TLabel
        Left = 446
        Height = 13
        Top = 144
        Width = 158
        Caption = 'BMP text matching enabled.'
        Font.Color = clMaroon
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object btnAddNewFontProfile: TButton
        Left = 446
        Height = 25
        Top = 48
        Width = 139
        Caption = 'Add font profile'
        OnClick = btnAddNewFontProfileClick
        TabOrder = 1
      end
      object btnRemoveFontProfile: TButton
        Left = 446
        Height = 25
        Top = 110
        Width = 139
        Caption = 'Remove font profile'
        OnClick = btnRemoveFontProfileClick
        TabOrder = 2
      end
      object lbeCurrentFontProfileName: TLabeledEdit
        Left = 446
        Height = 21
        Top = 19
        Width = 139
        EditLabel.Height = 13
        EditLabel.Width = 139
        EditLabel.Caption = 'Font profile name'
        TabOrder = 3
        OnEditingDone = lbeCurrentFontProfileNameEditingDone
      end
      object tabctrlBMPText: TTabControl
        Left = 0
        Height = 223
        Top = 0
        Width = 440
        OnChange = tabctrlBMPTextChange
        Anchors = [akTop, akLeft, akBottom]
        Constraints.MinHeight = 174
        Constraints.MinWidth = 437
        TabOrder = 4
      end
      object lbeMatchBitmapText: TLabeledEdit
        Left = 4
        Height = 21
        Hint = 'Please edit from "Text" tab in order to update an action.'#13#10'This editbox is for text preview only.'
        Top = 39
        Width = 158
        Color = 13487565
        EditLabel.Height = 13
        EditLabel.Width = 158
        EditLabel.Caption = 'Bitmap Text'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnKeyUp = lbeMatchBitmapTextKeyUp
      end
      object bitbtnUpdateFontProfile: TBitBtn
        Left = 446
        Height = 25
        Top = 80
        Width = 139
        Caption = 'Update name'
        OnClick = bitbtnUpdateFontProfileClick
        TabOrder = 5
      end
      object chkDisplayCroppingLines: TCheckBox
        Left = 446
        Height = 19
        Top = 192
        Width = 122
        Caption = 'Display cropping lines'
        OnChange = chkDisplayCroppingLinesChange
        TabOrder = 6
      end
    end
    object TabSheetActionFindSubControlBMPFiles: TTabSheet
      Caption = 'BMP Files'
      ClientHeight = 224
      ClientWidth = 869
      ImageIndex = 2
      object pnlvstMatchBitmapFiles: TPanel
        Left = 3
        Height = 120
        Top = 3
        Width = 396
        Caption = 'pnlvstMatchBitmapFiles'
        Color = clYellow
        ParentColor = False
        TabOrder = 7
      end
      object lbeMatchBitmapFile: TLabeledEdit
        Left = 64
        Height = 21
        Top = 129
        Width = 385
        EditLabel.Height = 13
        EditLabel.Width = 51
        EditLabel.Caption = 'Bitmap File'
        LabelPosition = lpLeft
        TabOrder = 0
      end
      object btnBrowseBitmap: TButton
        Left = 455
        Height = 21
        Hint = 'Browse Bitmap'
        Top = 129
        Width = 26
        Caption = '...'
        OnClick = btnBrowseBitmapClick
        ParentShowHint = False
        TabOrder = 1
      end
      object btnAddBmpFile: TButton
        Left = 405
        Height = 25
        Top = 5
        Width = 75
        Caption = 'Add...'
        OnClick = btnAddBmpFileClick
        TabOrder = 2
      end
      object btnUpdateBmpFile: TButton
        Left = 405
        Height = 25
        Top = 36
        Width = 75
        Caption = 'Update'
        OnClick = btnUpdateBmpFileClick
        TabOrder = 3
      end
      object btnRemoveBmpFile: TButton
        Left = 405
        Height = 25
        Top = 67
        Width = 75
        Caption = 'Remove...'
        OnClick = btnRemoveBmpFileClick
        TabOrder = 4
      end
      object lstMatchBitmapFiles: TListBox
        Left = 88
        Height = 47
        Top = 38
        Width = 49
        Color = clSkyBlue
        ItemHeight = 0
        TabOrder = 5
        Visible = False
      end
      object btnClear: TButton
        Left = 405
        Height = 25
        Top = 98
        Width = 76
        Caption = 'Clear...'
        OnClick = btnClearClick
        TabOrder = 6
      end
      object lblBMPFilesMatchingInfo: TLabel
        Left = 3
        Height = 13
        Top = 149
        Width = 131
        Caption = 'BMP matching enabled.'
        Font.Color = clMaroon
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object TabSheetActionFindSubControlSearchArea: TTabSheet
      Caption = 'Search Area'
      ClientHeight = 224
      ClientWidth = 869
      ImageIndex = 2
      ParentShowHint = False
      ShowHint = True
      object lblInitRectOffsetsInfo: TLabel
        Left = 3
        Height = 13
        Top = 115
        Width = 264
        Caption = 'Offsets can be used to limit or extend the search area.'
      end
      object lbeSearchRectTop: TLabeledEdit
        Left = 88
        Height = 21
        Hint = 'Top edge of the search area. Variable replacements are available.'
        Top = 15
        Width = 80
        EditLabel.Height = 13
        EditLabel.Width = 80
        EditLabel.Caption = 'Top Edge'
        Enabled = False
        ParentShowHint = False
        PopupMenu = pmStandardControlRefVars
        ShowHint = True
        TabOrder = 0
        Text = '$Control_Top$'
        OnChange = lbeSearchRectTopChange
        OnMouseEnter = lbeSearchRectTopMouseEnter
        OnMouseUp = lbeSearchRectTopMouseUp
      end
      object lbeSearchRectRight: TLabeledEdit
        Left = 172
        Height = 21
        Hint = 'Right edge of the search area. Variable replacements are available.'
        Top = 15
        Width = 86
        EditLabel.Height = 13
        EditLabel.Width = 86
        EditLabel.Caption = 'Right Edge'
        Enabled = False
        ParentShowHint = False
        PopupMenu = pmStandardControlRefVars
        ShowHint = True
        TabOrder = 1
        Text = '$Control_Right$'
        OnChange = lbeSearchRectRightChange
        OnMouseEnter = lbeSearchRectRightMouseEnter
        OnMouseUp = lbeSearchRectRightMouseUp
      end
      object lbeSearchRectLeft: TLabeledEdit
        Left = 3
        Height = 21
        Hint = 'Left edge of the search area. Variable replacements are available.'
        Top = 15
        Width = 80
        EditLabel.Height = 13
        EditLabel.Width = 80
        EditLabel.Caption = 'Left Edge'
        Enabled = False
        ParentShowHint = False
        PopupMenu = pmStandardControlRefVars
        ShowHint = True
        TabOrder = 2
        Text = '$Control_Left$'
        OnChange = lbeSearchRectLeftChange
        OnMouseEnter = lbeSearchRectLeftMouseEnter
        OnMouseUp = lbeSearchRectLeftMouseUp
      end
      object lbeSearchRectBottom: TLabeledEdit
        Left = 262
        Height = 21
        Hint = 'Bottom edge of the search area. Variable replacements are available.'
        Top = 15
        Width = 94
        EditLabel.Height = 13
        EditLabel.Width = 94
        EditLabel.Caption = 'Bottom Edge'
        Enabled = False
        ParentShowHint = False
        PopupMenu = pmStandardControlRefVars
        ShowHint = True
        TabOrder = 3
        Text = '$Control_Bottom$'
        OnChange = lbeSearchRectBottomChange
        OnMouseEnter = lbeSearchRectBottomMouseEnter
        OnMouseUp = lbeSearchRectBottomMouseUp
      end
      object chkUseWholeScreenAsSearchArea: TCheckBox
        Left = 4
        Height = 19
        Hint = 'Use the whole screen as search area, if checked. Use the current control as search area, if not checked.'#13#10'The search area is modified by offsets.'
        Top = 42
        Width = 123
        Caption = 'Use the whole screen'
        Checked = True
        OnClick = chkUseWholeScreenAsSearchAreaClick
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 4
      end
      object lbeSearchRectLeftOffset: TLabeledEdit
        Left = 4
        Height = 21
        Hint = 'Offset for the left edge of the search area. Variable replacements are available.'#13#10'Use Ctrl+MouseLeft drag (up/down) to increase or decrease the current value.'#13#10'Hold Shift while using Ctrl+MouseLeft, to update the right offset value at the same time.'
        Top = 89
        Width = 80
        EditLabel.Height = 13
        EditLabel.Width = 80
        EditLabel.Caption = 'Left Offset'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        Text = '0'
        OnChange = lbeSearchRectLeftOffsetChange
        OnMouseDown = lbeSearchRectLeftOffsetMouseDown
        OnMouseMove = lbeSearchRectLeftOffsetMouseMove
      end
      object lbeSearchRectTopOffset: TLabeledEdit
        Left = 88
        Height = 21
        Hint = 'Offset for the top edge of the search area. Variable replacements are available.'#13#10'Use Ctrl+MouseLeft drag (up/down) to increase or decrease the current value.'#13#10'Hold Shift while using Ctrl+MouseLeft, to update the bottom offset value at the same time.'
        Top = 89
        Width = 80
        EditLabel.Height = 13
        EditLabel.Width = 80
        EditLabel.Caption = 'Top Offset'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
        Text = '0'
        OnChange = lbeSearchRectTopOffsetChange
        OnMouseDown = lbeSearchRectTopOffsetMouseDown
        OnMouseMove = lbeSearchRectTopOffsetMouseMove
      end
      object lbeSearchRectRightOffset: TLabeledEdit
        Left = 171
        Height = 21
        Hint = 'Offset for the right edge of the search area. Variable replacements are available.'#13#10'Use Ctrl+MouseLeft drag (up/down) to increase or decrease the current value.'#13#10'Hold Shift while using Ctrl+MouseLeft, to update the left offset value at the same time.'
        Top = 89
        Width = 86
        EditLabel.Height = 13
        EditLabel.Width = 86
        EditLabel.Caption = 'Right Offset'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
        Text = '0'
        OnChange = lbeSearchRectRightOffsetChange
        OnMouseDown = lbeSearchRectRightOffsetMouseDown
        OnMouseMove = lbeSearchRectRightOffsetMouseMove
      end
      object lbeSearchRectBottomOffset: TLabeledEdit
        Left = 262
        Height = 21
        Hint = 'Offset for the bottom edge of the search area. Variable replacements are available.'#13#10'Use Ctrl+MouseLeft drag (up/down) to increase or decrease the current value.'#13#10'Hold Shift while using Ctrl+MouseLeft, to update the top offset value at the same time.'
        Top = 89
        Width = 94
        EditLabel.Height = 13
        EditLabel.Width = 94
        EditLabel.Caption = 'Bottom Offset'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
        Text = '0'
        OnChange = lbeSearchRectBottomOffsetChange
        OnMouseDown = lbeSearchRectBottomOffsetMouseDown
        OnMouseMove = lbeSearchRectBottomOffsetMouseMove
      end
      object btnDisplaySearchAreaDebuggingImage: TButton
        Left = 368
        Height = 25
        Hint = 'This button can be "clicked" with F6 key while debugging.'
        Top = 0
        Width = 105
        Caption = 'Display dbg img'
        OnClick = btnDisplaySearchAreaDebuggingImageClick
        TabOrder = 9
      end
      object spdbtnDisplaySearchAreaDbgImgMenu: TSpeedButton
        Left = 472
        Height = 25
        Hint = 'Display list of files from "BMP files" tab.'
        Top = 0
        Width = 19
        Enabled = False
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
        OnClick = spdbtnDisplaySearchAreaDbgImgMenuClick
        ShowHint = True
        ParentShowHint = False
      end
      object chkShowBMPFileDbgImg: TCheckBox
        Left = 282
        Height = 19
        Hint = 'Displays an overlapped image with loaded/selected BMP file.'#13#10'Also used when copying the debug image to clipboard.'
        Top = 112
        Width = 74
        AutoSize = False
        Caption = 'Show BMP'
        Checked = True
        OnClick = chkShowBMPFileDbgImgClick
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 10
      end
      object chkShowBMPTextDbgImg: TCheckBox
        Left = 282
        Height = 19
        Hint = 'Displays an overlapped image of generated BMP text.'#13#10'Also used when copying the debug image to clipboard.'
        Top = 136
        Width = 74
        AutoSize = False
        Caption = 'Show Text'
        Checked = True
        OnClick = chkShowBMPTextDbgImgClick
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 11
      end
      object lblReservedSpaceForDbgImg: TLabel
        Left = 368
        Height = 181
        Top = 40
        Width = 496
        Anchors = [akTop, akLeft, akRight, akBottom]
        AutoSize = False
        Caption = 'Reserved space'#13#10'for DbgImg'
        Color = clYellow
        Transparent = False
        Visible = False
      end
      object lblMouseOnDbgImg: TLabel
        Left = 296
        Height = 13
        Top = 42
        Width = 16
        Caption = '0:0'
      end
      object lblSearchAreaValidValuesInfo: TLabel
        Left = 3
        Height = 13
        Hint = 'When setting the search area and the offset values, make sure the previous action is the last executed action.'#13#10'Also it has to be successfully executed. Otherwise, the reference values will be wrong.'
        Top = 156
        Width = 349
        Caption = 'The above values should be valid before executing the action, not after.'
        ParentShowHint = False
        ShowHint = True
      end
      object imgUpdateLeftTopOffsets: TImage
        Left = 376
        Height = 16
        Top = 24
        Width = 16
        Picture.Data = {
          07544269746D617036030000424D360300000000000036000000280000001000
          0000100000000100180000000000000300000000000000000000000000000000
          0000FFFFFFFFFFFFFFFFFFFFFFFF277FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF277F
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF277FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF277F
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF150088150088150088150088150088150088
          150088150088FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1500
          880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF150088FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF1500880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF
          0EC9FF150088FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1500
          880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF150088FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF1500880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF
          0EC9FF150088FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1500
          880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF150088FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF1500880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF
          0EC9FF150088FFFFFFFFFFFFFFFFFFFFFFFF277FFF277FFF277FFF277FFF1500
          88150088150088150088150088150088150088150088277FFF277FFF277FFF27
          7FFFFFFFFFFFFFFFFFFFFFFFFFFF277FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF277F
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF277FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF277F
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF
        }
        Visible = False
      end
      object imgUpdateLeftTopRightBottomOffsets: TImage
        Left = 400
        Height = 16
        Top = 24
        Width = 16
        Picture.Data = {
          07544269746D617036030000424D360300000000000036000000280000001000
          0000100000000100180000000000000300000000000000000000000000000000
          0000FFFFFFFFFFFFFFFFFFFFFFFF277FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF4CB122FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF277F
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4CB122FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF277FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF4CB122FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF277F
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4CB122FFFFFFFFFFFFFFFFFFFF
          FFFF4CB1224CB1224CB1224CB122150088150088150088150088150088150088
          1500881500884CB1224CB1224CB1224CB122FFFFFFFFFFFFFFFFFFFFFFFF1500
          880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF150088FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF1500880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF
          0EC9FF150088FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1500
          880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF150088FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF1500880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF
          0EC9FF150088FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1500
          880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF150088FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF1500880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF
          0EC9FF150088FFFFFFFFFFFFFFFFFFFFFFFF277FFF277FFF277FFF277FFF1500
          88150088150088150088150088150088150088150088277FFF277FFF277FFF27
          7FFFFFFFFFFFFFFFFFFFFFFFFFFF277FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF4CB122FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF277F
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4CB122FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF277FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF4CB122FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF277F
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4CB122FFFFFFFFFFFFFFFFFFFF
          FFFF
        }
        Visible = False
      end
      object lblPreviewControl_Width: TLabel
        Left = 498
        Height = 13
        Top = 1
        Width = 81
        Caption = '$Control_Width$'
      end
      object lblPreviewControl_Height: TLabel
        Left = 498
        Height = 13
        Top = 19
        Width = 84
        Caption = '$Control_Height$'
      end
      object lblMouseOnDbgImgBB: TLabel
        Left = 296
        Height = 13
        Top = 56
        Width = 12
        Caption = 'BB'
        Font.Color = 16734553
        Font.Height = -11
        Font.Name = 'Tahoma'
        ParentFont = False
      end
      object lblMouseOnDbgImgGG: TLabel
        Left = 312
        Height = 13
        Top = 56
        Width = 14
        Caption = 'GG'
        Font.Color = clGreen
        Font.Height = -11
        Font.Name = 'Tahoma'
        ParentFont = False
      end
      object lblMouseOnDbgImgRR: TLabel
        Left = 328
        Height = 13
        Top = 56
        Width = 14
        Caption = 'RR'
        Font.Color = 187
        Font.Height = -11
        Font.Name = 'Tahoma'
        ParentFont = False
      end
      object imgCopyBkImg: TImage
        Left = 596
        Height = 16
        Top = 4
        Width = 16
        Picture.Data = {
          1754506F727461626C654E6574776F726B47726170686963C000000089504E47
          0D0A1A0A0000000D494844520000001000000010080200000090916836000000
          017352474200AECE1CE90000000467414D410000B18F0BFC6105000000097048
          597300000EC300000EC301C76FA8640000005549444154384F63F84F22006950
          DAE843244268F85FAF4E100D8886F74F367430880249640610E1D380AC14CEC0
          A901532984815303A65208039F0634A51006010DC84A210C9C1AF0233A6B2012
          41359000FEFF0700FFB3E984741491240000000049454E44AE426082
        }
        Visible = False
      end
      object imgCopyBMPImg: TImage
        Left = 624
        Height = 16
        Top = 4
        Width = 16
        Picture.Data = {
          1754506F727461626C654E6574776F726B477261706869632E01000089504E47
          0D0A1A0A0000000D494844520000001000000010080200000090916836000000
          017352474200AECE1CE90000000467414D410000B18F0BFC6105000000097048
          597300000EC300000EC301C76FA864000000C349444154384F63FCFFFF3F0329
          00A44179932F944708DCF5DBCC0461A5F99FBC7BEE3610E1614054423594D70B
          7536BEC3CF8000A8062020520FD40F407B81A2DBD2BF4144D180D74C2E20A96C
          A48AF003DC8CDCD3FA100411EF4A5A0F24E1C603018A93A02C2450362F104822
          3B89054201F9100D934D2F4244BCF79C8630B6BA980249A0EC2C3017DD491037
          A0016427413D0D09663C9E06EA99B5D11CE16988194089CB523F80241A032805
          F721BA93F0302000259488D14372E223317933300000CFA8A4864EFE6AF10000
          000049454E44AE426082
        }
        Visible = False
      end
      object imgCopyTextImg: TImage
        Left = 652
        Height = 16
        Top = 4
        Width = 16
        Picture.Data = {
          1754506F727461626C654E6574776F726B477261706869633801000089504E47
          0D0A1A0A0000000D494844520000001000000010080200000090916836000000
          017352474200AECE1CE90000000467414D410000B18F0BFC6105000000097048
          597300000EC300000EC301C76FA864000000CD49444154384F63FCFFFF3F0329
          00A44179932F944708DCF5DBCC0461A5F99FBC7BEE3610E1614054423594D70B
          7536BEC3CF8000A8062020520FD40F407B81A2DBD2BF4144D180D74C2E20A96C
          A40AF4030B440862C67786AC5BB76E01B931BD4780E492621B20A9A6A676B971
          1A5001481D9A935C5C5C800CA0EA5333938008A20D2888EC24A806381F1740B7
          01D98C871B2ACDD2E7011190011141360EEA694830E3F13450CFAC8DE6888883
          980194B82CF50348A2318052389D8487010128A1448C1E92131F89C99B810100
          2D85ACDC0FB8F2B70000000049454E44AE426082
        }
        Visible = False
      end
      object imgCopyBkAndBMPImg: TImage
        Left = 680
        Height = 16
        Top = 4
        Width = 16
        Picture.Data = {
          1754506F727461626C654E6574776F726B47726170686963F800000089504E47
          0D0A1A0A0000000D49484452000000100000001008060000001FF3FF61000000
          017352474200AECE1CE90000000467414D410000B18F0BFC6105000000097048
          597300000EC200000EC20115284A800000008D49444154384F63FC0F040C1400
          B001CA9B7CA15CD2C05DBFCD0803EE9EBB0D15260E281BA9820D6082F2C90614
          1B80E1856DE9DFC0342EE035930B4CE3F542EE697D388681AEA4F550162A20DA
          0B65F302A12C54C002A551C064D38B50160383F79ED3501603C3561753280B01
          B0BA009773B101AC06203B17642B0C6303C329219103E09909CA270330300000
          CECF49588530313D0000000049454E44AE426082
        }
        Visible = False
      end
      object imgCopySelAreaFromBkImg: TImage
        Left = 708
        Height = 16
        Top = 4
        Width = 16
        Picture.Data = {
          1754506F727461626C654E6574776F726B477261706869631C01000089504E47
          0D0A1A0A0000000D49484452000000100000001008060000001FF3FF61000000
          017352474200AECE1CE90000000467414D410000B18F0BFC6105000000097048
          597300000EC200000EC20115284A80000000B149444154384F63FC0F040C1400
          B001CA9B7CA15CD2C05DBFCD0803EE9EBB0D15260E281BA9621AC0D878132A8D
          0AFED7AB43590800338009CA87039062644C08B0406938D896FE0DCA82824654
          31AF995C50160460B80004724FEBC3310C7425AD87B250015603B081B2798150
          162AC06AC064D38B700C02DE7B4E83313680D5005CCEC506B01A80EEDCAD2EA6
          608C0D60C4027A288300363118C070012831216342807A49991C003700CA2703
          303000007F445F708CC7AAF20000000049454E44AE426082
        }
        Visible = False
      end
    end
    object TabSheetActionFindSubControlBMPSettings: TTabSheet
      Caption = 'Text/BMP Settings'
      ClientHeight = 224
      ClientWidth = 869
      ImageIndex = 2
      ParentShowHint = False
      ShowHint = True
      object chkSearchCachedLeftAndTopFirst: TCheckBox
        Left = 240
        Height = 19
        Hint = 'When checked, the control is checked at the specified cached $My_Control_Left$ and $My_Control_Top$ var replacements, before using the search grid.'#13#10'In order to cache the control coordinates, please add a SetVar action after this one, by assigning:'#13#10'$My_Control_Left$ to $Control_Left$'#13#10'and'#13#10'$My_Control_Top$ to $Control_Top$'#13#10'where $My_Control_Left$ and $My_Control_Top$ are the cached values. The "Eval before" checkboxes have to be set.'#13#10'Each Find(Sub)Control action, which uses caching, will have to use its own set of $My_Control_Left$ and $My_Control_Top$ vars.'#13#10'The cached values are global coordinates, so they will become invalid even for a subcontrol if the parent window is moved.'
        Top = 21
        Width = 158
        Caption = 'Search cached Left/Top vars'
        OnClick = chkSearchCachedLeftAndTopFirstClick
        ParentShowHint = False
        ShowHint = True
        TabOrder = 9
      end
      object lblMatchBitmapTextSearchAlgorithm: TLabel
        Left = 110
        Height = 13
        Top = 3
        Width = 170
        Caption = 'Algorithm (used on FindSubControl)'
      end
      object lbeColorError: TLabeledEdit
        Left = 3
        Height = 21
        Hint = 'When matching bitmaps, which contain antialiasing pixels (see smooth text), some of those pixels will not match. The "Color Error" represents the difference between the color values for the two compared pixels, for each RGB channel. The "Color Error Count" is the allowed number of mismatching pixels. Variable replacements are available.'#13#10'If at least one of the three color channels (R, G, B) mismatches by at least ColorError, it counts as an error point.'
        Top = 19
        Width = 94
        EditLabel.Height = 13
        EditLabel.Width = 94
        EditLabel.Caption = 'Color Error'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Text = '0'
        OnChange = lbeColorErrorChange
      end
      object lbeAllowedColorErrorCount: TLabeledEdit
        Left = 3
        Height = 21
        Hint = 'When matching bitmaps, which contain antialiasing pixels (see smooth text), some of those pixels will not match. The "Color Error" represents the difference between the color values for the two compared pixels, for each RGB channel. The "Color Error Count" is the allowed number of mismatching pixels. Variable replacements are available.'
        Top = 59
        Width = 94
        EditLabel.Height = 13
        EditLabel.Width = 94
        EditLabel.Caption = 'Color Error Count'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Text = '0'
        OnChange = lbeAllowedColorErrorCountChange
      end
      object cmbMatchBitmapTextSearchAlgorithm: TComboBox
        Left = 110
        Height = 22
        Top = 19
        Width = 120
        ItemHeight = 16
        ItemIndex = 0
        Items.Strings = (
          'Brute Force'
          'X and Y are multiple of a value and use offsets'
        )
        OnChange = cmbMatchBitmapTextSearchAlgorithmChange
        OnCloseUp = cmbMatchBitmapTextSearchAlgorithmCloseUp
        OnDropDown = cmbMatchBitmapTextSearchAlgorithmDropDown
        Style = csOwnerDrawFixed
        TabOrder = 2
        Text = 'Brute Force'
      end
      object lbeMatchBitmapAlgorithmXMulOf: TLabeledEdit
        Left = 110
        Height = 21
        Top = 59
        Width = 63
        EditLabel.Height = 13
        EditLabel.Width = 63
        EditLabel.Caption = 'X Multiple Of'
        Enabled = False
        TabOrder = 3
        OnChange = lbeMatchBitmapAlgorithmXMulOfChange
      end
      object lbeMatchBitmapAlgorithmXOffset: TLabeledEdit
        Left = 179
        Height = 21
        Top = 59
        Width = 51
        EditLabel.Height = 13
        EditLabel.Width = 51
        EditLabel.Caption = 'X Offset'
        Enabled = False
        TabOrder = 4
        OnChange = lbeMatchBitmapAlgorithmXOffsetChange
      end
      object lbeMatchBitmapAlgorithmYMulOf: TLabeledEdit
        Left = 110
        Height = 21
        Top = 99
        Width = 63
        EditLabel.Height = 13
        EditLabel.Width = 63
        EditLabel.Caption = 'Y Multiple Of'
        Enabled = False
        TabOrder = 5
        OnChange = lbeMatchBitmapAlgorithmYMulOfChange
      end
      object lbeMatchBitmapAlgorithmYOffset: TLabeledEdit
        Left = 179
        Height = 21
        Top = 99
        Width = 51
        EditLabel.Height = 13
        EditLabel.Width = 51
        EditLabel.Caption = 'Y Offset'
        Enabled = False
        TabOrder = 6
        OnChange = lbeMatchBitmapAlgorithmYOffsetChange
      end
      object lblBMPSettingsInfo: TLabel
        Left = 3
        Height = 13
        Top = 131
        Width = 177
        Caption = 'Used for "BMP Text" and "BMP Files".'
      end
      object lblSearchInfo: TLabel
        Left = 3
        Height = 13
        Top = 147
        Width = 254
        Caption = 'The bitmap searching algorithm stops on error count.'
      end
      object lbeFindCachedControlLeft: TLabeledEdit
        Left = 240
        Height = 21
        Top = 59
        Width = 152
        EditLabel.Height = 13
        EditLabel.Width = 152
        EditLabel.Caption = 'Cached $Control_Left$'
        Enabled = False
        TabOrder = 7
        OnChange = lbeFindCachedControlLeftChange
      end
      object lbeFindCachedControlTop: TLabeledEdit
        Left = 240
        Height = 21
        Top = 99
        Width = 152
        EditLabel.Height = 13
        EditLabel.Width = 152
        EditLabel.Caption = 'Cached $Control_Top$'
        Enabled = False
        TabOrder = 8
        OnChange = lbeFindCachedControlTopChange
      end
      object lblCachingInfo: TLabel
        Left = 3
        Height = 13
        Hint = 'Using caching on multpile font profiles may indeed be faster than without it, but for every cache miss, the search defaults to the selected algorithm.'
        Top = 161
        Width = 379
        Caption = 'Caching is more effective when searching for controls with a single font profile.'
      end
      object chkShowGridOnBMPPreview: TCheckBox
        Left = 3
        Height = 19
        Hint = 'This will not be saved in project.'
        Top = 101
        Width = 67
        Caption = 'Show grid'
        OnChange = chkShowGridOnBMPPreviewChange
        TabOrder = 10
      end
      object updownXMulOf: TUpDown
        Left = 155
        Height = 19
        Top = 60
        Width = 17
        Enabled = False
        Max = 1000
        Min = 0
        OnChangingEx = updownXMulOfChangingEx
        ParentColor = False
        Position = 0
        TabOrder = 11
        Flat = True
      end
      object updownYMulOf: TUpDown
        Left = 155
        Height = 19
        Top = 100
        Width = 17
        Enabled = False
        Max = 1000
        Min = 0
        OnChangingEx = updownYMulOfChangingEx
        ParentColor = False
        Position = 0
        TabOrder = 12
        Flat = True
      end
      object updownXOffset: TUpDown
        Left = 212
        Height = 19
        Top = 60
        Width = 17
        Enabled = False
        Max = 1000
        Min = -1000
        OnChangingEx = updownXOffsetChangingEx
        ParentColor = False
        Position = 0
        TabOrder = 13
        Flat = True
      end
      object updownYOffset: TUpDown
        Left = 212
        Height = 19
        Top = 100
        Width = 17
        Enabled = False
        Max = 1000
        Min = -1000
        OnChangingEx = updownYOffsetChangingEx
        ParentColor = False
        Position = 0
        TabOrder = 14
        Flat = True
      end
    end
  end
  object imglstFindCriteria: TImageList
    Height = 15
    Width = 15
    Left = 784
    Top = 48
    Bitmap = {
      4C7A030000000F0000000F000000780300000000000078DAC5945B6EDB461486
      DDC76E271B28D0A76E2741D85B92064DD338B1D34D640F5940D1DCD038A91D3B
      BA93A2789328521225911C5E8CFC3D678636D40E5F623F44C08F436AFE6FFE99
      C11CEEED5DEF77F4DD37F85CEDB21DC7C1C072311ABB301D1FA6EB60EC7BB003
      1FD6D4833F5B200817982D171A3B1CDBE84CC6B06C1BA6EDC19AF818BB1E2601
      71D3195C521CC788D71B8DED3B01CC8923F326AE0B67E62298CDE04D23C46184
      3971F3D506CB75A6B16E3087134C31F103F8F310F328C4348E102D16889215D6
      9B04DB4D0A51961A1B46318268AEFCCB188B5522B5D96C90A75B2479062104AA
      A2D058F6ACD66BAC93144BF2A6698A2C27A539445EA01025B22297F5FF2C7BD2
      6D461929D6598E3A17280AC570659E33F352686CCE3EFA9F6B550894554ECFA5
      F4CB3C1A6395A28D2DE55845396551A1A2FD29B6998FFF2FD5582BDBAC892BCF
      CFBE8BEC8B75952DE79C32236AB547524D1EC16BE5BD966ADE4C14AD6775D5FB
      FCA57ED7EDC1F7DD214E3A7D52171FBA039CF1FBC0446F3C42DF32317226E878
      2EF564A4B1A7BD218E067D747A3D7C24A66B8E480E86B6457DE5C1F1E7F0C229
      DD77BD7FCF4C1BBD1131A4A13DC680726CEA7DEEAF70BE049E7F8D385923D9E8
      FD6BBB53D89E4FF38734FF0C611862493D9BAD52C9B1F86EE579AEB17E447DBD
      58490FF7529E64A8B6957C9F3DDB93E2FB53B7DCAB642B4885F4B0FF53595C32
      2CBE6339DDBBB6DEE77BFB89D6B3EBDFE5F86E736656A67AFF5645D303FFE579
      CEBAD8CA1E90FBA56C8DAD73EABB5A7AF8DBC21CDF7BDEA328A99F328173EA47
      F668AC10973D9415E7A8C5B9642FFAB7283359B7857ECE75A9BE173C475A65EA
      3BD07C7338577D7384DCD397EE4123FCF35AFAF6C657F85CEDB2C6EBDB30FE32
      60BC20BDFE01C61B7AFFFB7B186FE9F988EABBFB30DE934EEFEBEC8B5B305EDD
      84F192EA4BF2BE629E59D2D1CF4A27BFC0E83C6CC9FD91FCB755DE1BCA7EC762
      E61EE5DD53DCD94362F775F6ED5DD24FB44E9AE39F3B308E4927C47CA0757EFC
      1546F701E9110CEB50678F69DEE3BBCA7FCA190F94BA94D5FF1DC6701FC6E809
      0CF34067D9D3F98D321E296F9FEA909F1F533D20EE90B8C7B26A2C7B7AFB8A19
      B05F6528E640F15CAD273ADBFC2FAB49754CBCD9709CC763AC511B7BD88CB1FF
      A9DA9F6477F22D35D6CAEE66CB7379BA93DD8CB59DB3CCFC63C7D79CCDE533CF
      77D07A5657BDCF57D5BFE22C6D59
    }
  end
  object pmStandardControlRefVars: TPopupMenu
    Left = 784
    Top = 120
    object MenuItemCopyRefToClipboard: TMenuItem
      Caption = 'Copy Ref To Clipboard'
      OnClick = MenuItemCopyRefToClipboardClick
    end
    object MenuItemPasteRefFromClipboard: TMenuItem
      Caption = 'Paste Ref From Clipboard'
      OnClick = MenuItemPasteRefFromClipboardClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object MenuItemControl_Left: TMenuItem
      Caption = '$Control_Left$'
      Bitmap.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF241CED
        150088150088150088150088150088150088150088150088150088FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF241CED0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0E
        C9FF0EC9FF0EC9FF150088FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF241CED
        0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF150088FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF241CED0EC9FF0EC9FF241CED0EC9FF0EC9FF0E
        C9FF0EC9FF0EC9FF150088FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF241CED
        0EC9FF241CED241CED0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF150088FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF241CED241CED241CED241CED241CED241CED24
        1CED241CED241CED241CED241CED241CED241CED241CEDFFFFFFFFFFFF241CED
        0EC9FF241CED241CED0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF150088FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF241CED0EC9FF0EC9FF241CED0EC9FF0EC9FF0E
        C9FF0EC9FF0EC9FF150088FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF241CED
        0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF150088FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF241CED15008815008815008815008815008815
        0088150088150088150088FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      }
      OnClick = MenuItemControl_EdgeRefGenericClick
    end
    object MenuItemControl_Top: TMenuItem
      Caption = '$Control_Top$'
      Bitmap.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF241CEDFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF241CEDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF241CEDFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF241CEDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF150088150088150088150088150088241CED15
        0088150088150088150088FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF150088
        0EC9FF0EC9FF0EC9FF0EC9FF241CED0EC9FF0EC9FF0EC9FF150088FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF1500880EC9FF0EC9FF0EC9FF0EC9FF241CED0E
        C9FF0EC9FF0EC9FF150088FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF150088
        0EC9FF0EC9FF0EC9FF0EC9FF241CED0EC9FF0EC9FF0EC9FF150088FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF1500880EC9FF0EC9FF0EC9FF0EC9FF241CED0E
        C9FF0EC9FF0EC9FF150088FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF150088
        0EC9FF0EC9FF0EC9FF0EC9FF241CED0EC9FF0EC9FF0EC9FF150088FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF1500880EC9FF0EC9FF241CED241CED241CED24
        1CED241CED0EC9FF150088FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF150088
        0EC9FF0EC9FF0EC9FF241CED241CED241CED0EC9FF0EC9FF150088FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF1500880EC9FF0EC9FF0EC9FF0EC9FF241CED0E
        C9FF0EC9FF0EC9FF150088FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF241CED
        241CED241CED241CED241CED241CED241CED241CED241CED241CEDFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      }
      OnClick = MenuItemControl_EdgeRefGenericClick
    end
    object MenuItemControl_Right: TMenuItem
      Caption = '$Control_Right$'
      Bitmap.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF150088
        150088150088150088150088150088150088150088150088241CEDFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF1500880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0E
        C9FF0EC9FF0EC9FF241CEDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF150088
        0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF241CEDFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF1500880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0E
        C9FF0EC9FF0EC9FF241CEDFFFFFFFFFFFF241CEDFFFFFFFFFFFFFFFFFF150088
        0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF241CEDFFFFFF241C
        ED241CEDFFFFFFFFFFFFFFFFFF1500880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0E
        C9FF0EC9FF0EC9FF241CED241CED241CED241CED241CED241CEDFFFFFF150088
        0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF241CEDFFFFFF241C
        ED241CEDFFFFFFFFFFFFFFFFFF1500880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0E
        C9FF0EC9FF0EC9FF241CEDFFFFFFFFFFFF241CEDFFFFFFFFFFFFFFFFFF150088
        0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF241CEDFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF15008815008815008815008815008815008815
        0088150088150088241CEDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      }
      OnClick = MenuItemControl_EdgeRefGenericClick
    end
    object MenuItemControl_Bottom: TMenuItem
      Caption = '$Control_Bottom$'
      Bitmap.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF241CEDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF241CEDFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF241CED241CED241CED241CED241CEDFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF241CED241CED24
        1CEDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF241CEDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF241CED241CED241CED241CED241CED241CED24
        1CED241CED241CED241CEDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF150088
        0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF150088FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF1500880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0E
        C9FF0EC9FF0EC9FF150088FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF150088
        0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF150088FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF1500880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0E
        C9FF0EC9FF0EC9FF150088FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF150088
        0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF150088FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF1500880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0E
        C9FF0EC9FF0EC9FF150088FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF150088
        0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0EC9FF150088FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF1500880EC9FF0EC9FF0EC9FF0EC9FF0EC9FF0E
        C9FF0EC9FF0EC9FF150088FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF150088
        150088150088150088150088150088150088150088150088150088FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      }
      OnClick = MenuItemControl_EdgeRefGenericClick
    end
  end
  object imglstMatchBitmapFiles: TImageList
    Height = 50
    Width = 50
    Left = 648
    Top = 48
  end
  object pmExtraCopyValueWindows: TPopupMenu
    Left = 584
    Top = 152
    object MenuItem_CopyTextAndClassFromPreviewWindow: TMenuItem
      Caption = 'Copy values from preview window'
      OnClick = CopyTextAndClassFromPreviewWindowClick
    end
    object MenuItem_CopyTextAndClassFromWinInterpWindow: TMenuItem
      Caption = 'Copy values from window interpreter'
      OnClick = CopyTextAndClassFromWinInterpWindowClick
    end
    object MenuItem_CopyTextAndClassFromRemoteScreenWindow: TMenuItem
      Caption = 'Copy values from remote screen'
      OnClick = CopyTextAndClassFromRemoteScreenWindowClick
    end
  end
  object tmrUpdateSearchAreaOffsetEditBoxes: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrUpdateSearchAreaOffsetEditBoxesTimer
    Left = 275
    Top = 206
  end
end
