object frClickerActions: TfrClickerActions
  Left = 0
  Height = 280
  Top = 0
  Width = 1056
  Anchors = [akTop, akLeft, akRight, akBottom]
  ClientHeight = 280
  ClientWidth = 1056
  Constraints.MinHeight = 260
  Constraints.MinWidth = 688
  TabOrder = 0
  TabStop = True
  DesignLeft = 86
  DesignTop = 85
  object PageControlActionExecution: TPageControl
    Left = 0
    Height = 276
    Top = 0
    Width = 1052
    ActivePage = TabSheetAction
    Anchors = [akTop, akLeft, akRight, akBottom]
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Images = imglstActionExecution
    ParentFont = False
    TabIndex = 0
    TabOrder = 0
    TabPosition = tpLeft
    object TabSheetAction: TTabSheet
      Caption = 'Action'
      ClientHeight = 268
      ClientWidth = 1024
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      ImageIndex = 0
      ParentFont = False
      object lblAction: TLabel
        Left = 1
        Height = 13
        Top = 47
        Width = 57
        Caption = 'Action Type'
      end
      object spdbtnCommonTimeouts: TSpeedButton
        Left = 122
        Height = 23
        Top = 110
        Width = 16
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Webdings'
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
        OnClick = spdbtnCommonTimeoutsClick
        ParentFont = False
      end
      object spdbtnClear: TSpeedButton
        Left = 91
        Height = 26
        Top = 168
        Width = 48
        Caption = 'Clear'
        OnClick = spdbtnClearClick
      end
      object PageControlActions: TPageControl
        Left = 141
        Height = 268
        Top = 1
        Width = 890
        ActivePage = TabSheetActionFindSubControl
        Anchors = [akTop, akLeft, akRight, akBottom]
        Images = imglstActions
        TabIndex = 3
        TabOrder = 5
        OnChange = PageControlActionsChange
        object TabSheetActionClick: TTabSheet
          Caption = 'Click'
          ClientHeight = 233
          ClientWidth = 882
          ImageIndex = 0
          object lblXClickReference: TLabel
            Left = 3
            Height = 13
            Top = 10
            Width = 83
            Caption = 'X Click Reference'
          end
          object lblYClickReference: TLabel
            Left = 3
            Height = 13
            Top = 65
            Width = 83
            Caption = 'Y Click Reference'
          end
          object cmbXClickReference: TComboBox
            Left = 3
            Height = 22
            Top = 25
            Width = 145
            ItemHeight = 16
            ItemIndex = 0
            Items.Strings = (
              'Control Left'
              'Control Right'
              'Control Width'
              'Var/Replacement'
              'Screen Absolute X'
            )
            OnChange = cmbXClickReferenceChange
            Style = csOwnerDrawFixed
            TabOrder = 0
            Text = 'Control Left'
          end
          object cmbYClickReference: TComboBox
            Left = 3
            Height = 22
            Top = 80
            Width = 145
            ItemHeight = 16
            ItemIndex = 0
            Items.Strings = (
              'Control Top'
              'Control Bottom'
              'Control Height'
              'Var/Replacement'
              'Screen Absolute Y'
            )
            OnChange = cmbYClickReferenceChange
            Style = csOwnerDrawFixed
            TabOrder = 1
            Text = 'Control Top'
          end
          object lbeClickXOffset: TLabeledEdit
            Left = 320
            Height = 21
            Hint = 'Replacements are available.'#13#10'Examples of random value:'#13#10'$Random(50, 100)$'#13#10'$Random($MMin$, $MMax$)$'
            Top = 25
            Width = 81
            EditLabel.Height = 13
            EditLabel.Width = 81
            EditLabel.Caption = 'X Offset'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
            OnChange = lbeClickXOffsetChange
          end
          object lbeClickYOffset: TLabeledEdit
            Left = 320
            Height = 21
            Hint = 'Replacements are available.'#13#10'Examples of random value:'#13#10'$Random(50, 100)$'#13#10'$Random($MMin$, $MMax$)$'
            Top = 80
            Width = 81
            EditLabel.Height = 13
            EditLabel.Width = 81
            EditLabel.Caption = 'Y Offset'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 3
            OnChange = lbeClickYOffsetChange
          end
          object rdgrpMouseButton: TRadioGroup
            Left = 411
            Height = 65
            Top = -2
            Width = 93
            AutoFill = True
            Caption = 'Mouse Button'
            ChildSizing.LeftRightSpacing = 6
            ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
            ChildSizing.EnlargeVertical = crsHomogenousChildResize
            ChildSizing.ShrinkHorizontal = crsScaleChilds
            ChildSizing.ShrinkVertical = crsScaleChilds
            ChildSizing.Layout = cclLeftToRightThenTopToBottom
            ChildSizing.ControlsPerLine = 1
            ClientHeight = 47
            ClientWidth = 89
            ItemIndex = 0
            Items.Strings = (
              'Left'
              'Right'
              'Middle'
            )
            OnClick = rdgrpMouseButtonClick
            TabOrder = 4
          end
          object chkClickWithCtrl: TCheckBox
            Left = 3
            Height = 19
            Top = 108
            Width = 86
            Caption = 'Click With Ctrl'
            OnClick = chkClickWithCtrlClick
            TabOrder = 5
          end
          object chkClickWithAlt: TCheckBox
            Left = 3
            Height = 19
            Top = 131
            Width = 82
            Caption = 'Click With Alt'
            OnClick = chkClickWithAltClick
            TabOrder = 6
          end
          object chkClickWithShift: TCheckBox
            Left = 3
            Height = 19
            Top = 154
            Width = 91
            Caption = 'Click With Shift'
            OnClick = chkClickWithShiftClick
            TabOrder = 7
          end
          object chkClickWithDoubleClick: TCheckBox
            Left = 154
            Height = 19
            Hint = 'Deprecated (please set the "Count" value to 2, to simulate a double-click).'
            Top = 107
            Width = 77
            Caption = 'Double Click'
            Enabled = False
            OnChange = chkClickWithDoubleClickChange
            ParentShowHint = False
            ShowHint = True
            TabOrder = 8
          end
          object btnCopyOffsetsFromMain: TButton
            Left = 320
            Height = 25
            Top = 108
            Width = 184
            Caption = 'Copy Offsets From Main Window'
            TabOrder = 9
            Visible = False
          end
          object lbeMultiClickCount: TLabeledEdit
            Left = 411
            Height = 21
            Top = 80
            Width = 46
            EditLabel.Height = 13
            EditLabel.Width = 46
            EditLabel.Caption = 'Count'
            TabOrder = 10
            Text = '1'
            OnChange = lbeMultiClickCountChange
          end
          object lbeClickVarX: TLabeledEdit
            Left = 154
            Height = 21
            Hint = 'The provided variables must be global/screen coordinates.'#13#10'The $Current_Mouse_X$ var/replacement can be used as global mouse X coordinate.'
            Top = 25
            Width = 152
            EditLabel.Height = 13
            EditLabel.Width = 152
            EditLabel.Caption = 'Click Var X'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 11
            Text = '$Control_Left$'
            OnChange = lbeClickVarXChange
          end
          object lbeClickVarY: TLabeledEdit
            Left = 154
            Height = 21
            Hint = 'The provided variables must be global/screen coordinates.'#13#10'The $Current_Mouse_Y$ var/replacement can be used as global mouse Y coordinate.'
            Top = 80
            Width = 152
            EditLabel.Height = 13
            EditLabel.Width = 152
            EditLabel.Caption = 'Click Var Y'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 12
            Text = '$Control_Top$'
            OnChange = lbeClickVarYChange
          end
          object chkLeaveMouse: TCheckBox
            Left = 154
            Height = 19
            Hint = 'When checked, the mouse cursor position is not reset after running the action.'#13#10'It may be required if the click action will open a pop-up menu, to cause the menu to open at that location.'#13#10'This is also useful for debugging, to verify offsets.'
            Top = 154
            Width = 83
            Caption = 'Leave Mouse'
            OnChange = chkLeaveMouseChange
            ParentShowHint = False
            ShowHint = True
            TabOrder = 13
          end
          object chkMoveWithoutClick: TCheckBox
            Left = 154
            Height = 19
            Top = 131
            Width = 111
            Caption = 'Move Without Click'
            OnChange = chkMoveWithoutClickChange
            TabOrder = 14
          end
          object cmbClickType: TComboBox
            Left = 320
            Height = 19
            Top = 160
            Width = 140
            ItemHeight = 13
            ItemIndex = 0
            Items.Strings = (
              'Click'
              'Drag'
              'ButtonDown'
              'ButtonUp'
            )
            OnChange = cmbClickTypeChange
            Style = csOwnerDrawFixed
            TabOrder = 15
            Text = 'Click'
          end
          object lblClickType: TLabel
            Left = 320
            Height = 13
            Top = 144
            Width = 48
            Caption = 'Click Type'
          end
        end
        object TabSheetActionExecApp: TTabSheet
          Caption = 'Exec App'
          ClientHeight = 233
          ClientWidth = 882
          ImageIndex = 1
          object lblExecAppParams: TLabel
            Left = 0
            Height = 13
            Top = 98
            Width = 55
            Caption = 'Parameters'
          end
          object lbeExecAppPath: TLabeledEdit
            Left = 0
            Height = 21
            Hint = 'Full path (without quotes) to executable or other file to be open with associated application. Replacements are available.'
            Top = 14
            Width = 326
            EditLabel.Height = 13
            EditLabel.Width = 326
            EditLabel.Caption = 'Path'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnChange = lbeExecAppPathChange
          end
          object btnBrowseExecApp: TButton
            Left = 335
            Height = 21
            Top = 14
            Width = 25
            Caption = '...'
            OnClick = btnBrowseExecAppClick
            TabOrder = 1
          end
          object chkWaitForApp: TCheckBox
            Left = 368
            Height = 19
            Top = 14
            Width = 83
            Caption = 'Wait For App'
            OnChange = chkWaitForAppChange
            TabOrder = 2
          end
          object memExecAppParams: TMemo
            Left = 0
            Height = 89
            Hint = 'Enter each parameter on a new line. Do not add quotes. Replacements are available.'
            Top = 114
            Width = 326
            OnChange = memExecAppParamsChange
            ParentShowHint = False
            ScrollBars = ssBoth
            ShowHint = True
            TabOrder = 3
            WordWrap = False
          end
          object lbeExecAppStdIn: TLabeledEdit
            Left = 0
            Height = 21
            Hint = 'All #4#5 (a.k.a. 0x4:0x5) occurrences are replaced with CRLF (#13#10) before executing the application.'#13#10'Var/replacements are available. E.g.: $ExecAction_StdIn$'#13#10'When this parameter is empty string, the executed application can run without inherited handles.'
            Top = 64
            Width = 326
            EditLabel.Height = 13
            EditLabel.Width = 326
            EditLabel.Caption = 'StdIn'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 4
            OnChange = lbeExecAppStdInChange
          end
          object lbeExecAppCurrentDir: TLabeledEdit
            Left = 335
            Height = 21
            Hint = 'Application current directory.'#13#10'Replacements are avaialable.'#13#10'Example: $ExtractFileDir($PathToMyFile$)$'
            Top = 64
            Width = 307
            EditLabel.Height = 13
            EditLabel.Width = 307
            EditLabel.Caption = 'Current directory'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 5
            OnChange = lbeExecAppCurrentDirChange
          end
          object cmbUseInheritHandles: TComboBox
            Left = 472
            Height = 19
            Hint = 'Required mostly when passing data through StdIn.'
            Top = 14
            Width = 170
            ItemHeight = 13
            ItemIndex = 2
            Items.Strings = (
              'No'
              'Yes'
              'Only with StdIn / StdOut'
            )
            OnChange = cmbUseInheritHandlesChange
            ParentShowHint = False
            ShowHint = True
            Style = csOwnerDrawFixed
            TabOrder = 6
            Text = 'Only with StdIn / StdOut'
          end
          object lblUseInheritHandles: TLabel
            Left = 472
            Height = 13
            Top = 0
            Width = 91
            Caption = 'Use InheritHandles'
          end
          object chkNoConsole: TCheckBox
            Left = 335
            Height = 19
            Hint = 'When checked, console applications are not displayed in a new window.'#13#10'UI applications can create and display system consoles. For those applications, this option may cause problems if checked.'
            Top = 114
            Width = 72
            Caption = 'No console'
            OnChange = chkNoConsoleChange
            ParentShowHint = False
            ShowHint = True
            TabOrder = 7
          end
        end
        object TabSheetActionFindControl: TTabSheet
          Caption = 'Find Control'
          ImageIndex = 2
        end
        object TabSheetActionFindSubControl: TTabSheet
          Caption = 'Find SubControl'
          ImageIndex = 3
        end
        object TabSheetActionSetText: TTabSheet
          Caption = 'Set Text'
          ClientHeight = 233
          ClientWidth = 882
          ImageIndex = 4
          object lblInfoSetText: TLabel
            Left = 3
            Height = 13
            Top = 149
            Width = 482
            Caption = 'The proper control type has to be selected, for the proper API call. Uses $Control_Handle$ variable.'
          end
          object lbeSetNewText: TLabeledEdit
            Left = 3
            Height = 21
            Top = 25
            Width = 876
            Anchors = [akTop, akLeft, akRight]
            EditLabel.Height = 13
            EditLabel.Width = 876
            EditLabel.Caption = 'New Text'
            TabOrder = 0
            OnChange = lbeSetNewTextChange
          end
          object rdgrpSetTextControlType: TRadioGroup
            Left = 3
            Height = 85
            Hint = 'Uses WM_SETTEXT or CB_SELECTSTRING messages or emulates keystrokes..'
            Top = 52
            Width = 121
            AutoFill = True
            Caption = 'Control Type'
            ChildSizing.LeftRightSpacing = 6
            ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
            ChildSizing.EnlargeVertical = crsHomogenousChildResize
            ChildSizing.ShrinkHorizontal = crsScaleChilds
            ChildSizing.ShrinkVertical = crsScaleChilds
            ChildSizing.Layout = cclLeftToRightThenTopToBottom
            ChildSizing.ControlsPerLine = 1
            ClientHeight = 67
            ClientWidth = 117
            Items.Strings = (
              'EditBox'
              'ComboBox'
              'Keystrokes'
            )
            OnClick = rdgrpSetTextControlTypeClick
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
          end
          object lblSetVarToHttpInfo2: TLabel
            Left = 3
            Height = 13
            Hint = 'Every time a variable, with such a value, is evaluated, an http call is made.'
            Top = 168
            Width = 483
            Caption = 'HTTP calls are available, as var values, using the following format: $http://<server:port>/[params]$'
            ParentShowHint = False
            ShowHint = True
          end
          object lblSetTextInfo: TLabel
            Left = 136
            Height = 52
            Top = 56
            Width = 562
            Caption = 'Most edit boxes and combo boxes can be set, using the first two options.'#13#10'However, depending on their usage on the target application, this approach might not be enough.'#13#10'For edit boxes, the action can be configured to use key strokes.'#13#10'For combo boxes, this action will have to be replaced by multiple actions, to open the box, finding text, selecting etc.'
          end
        end
        object TabSheetActionCall: TTabSheet
          Caption = 'Call'
          ClientHeight = 233
          ClientWidth = 882
          ImageIndex = 5
          OnMouseLeave = TabSheetActionCallMouseLeave
          ParentShowHint = False
          object lblCustomUserVarsBeforeCall: TLabel
            Left = 3
            Height = 13
            Hint = 'Right-click to add/remove. Double-click to edit.'
            Top = 40
            Width = 173
            Caption = 'Custom user variables before calling'
            ParentShowHint = False
            ShowHint = True
          end
          object lblConditionEquals: TLabel
            Left = 781
            Height = 13
            Top = 118
            Width = 8
            Caption = '='
          end
          object lblDeprecatedCondition: TLabel
            Left = 720
            Height = 13
            Top = 39
            Width = 159
            Caption = 'Deprecated Condition Mechanism'
            Color = 13027071
            Transparent = False
          end
          object vallstCustomVariables: TValueListEditor
            Left = 3
            Height = 150
            Hint = 'These variables are passed to the called template. Right-click for adding/removing.'
            Top = 58
            Width = 366
            Color = 13828080
            DefaultColWidth = 180
            FixedCols = 0
            ParentShowHint = False
            PopupMenu = pmCustomVars
            RowCount = 2
            ShowHint = True
            TabOrder = 0
            OnExit = vallstCustomVariablesExit
            OnSetEditText = vallstCustomVariablesSetEditText
            DisplayOptions = [doColumnTitles, doKeyColFixed]
            KeyOptions = [keyEdit, keyUnique]
            TitleCaptions.Strings = (
              'Variable'
              'Value'
            )
            OnValidate = vallstCustomVariablesValidate
            ColWidths = (
              180
              140
            )
          end
          object chkCallOnlyIfContitionIsTrue: TCheckBox
            Left = 752
            Height = 19
            Top = 58
            Width = 123
            Caption = 'Call Only If Contition:'
            OnClick = chkCallOnlyIfContitionIsTrueClick
            TabOrder = 1
          end
          object lbeCallOnlyIfContitionVarName: TLabeledEdit
            Left = 785
            Height = 21
            Hint = 'Custom variables can also be used.'
            Top = 91
            Width = 88
            EditLabel.Height = 13
            EditLabel.Width = 88
            EditLabel.Caption = 'Variable'
            Enabled = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
            OnChange = lbeCallOnlyIfContitionVarNameChange
          end
          object lbeCallOnlyIfContitionVarValue: TLabeledEdit
            Left = 784
            Height = 21
            Hint = 'Replacements are available as values.'
            Top = 146
            Width = 88
            EditLabel.Height = 13
            EditLabel.Width = 88
            EditLabel.Caption = 'Value'
            Enabled = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 3
            OnChange = lbeCallOnlyIfContitionVarValueChange
          end
          object btnSelectNoTemplate: TButton
            Left = 278
            Height = 21
            Top = 17
            Width = 72
            Caption = 'Select none'
            OnClick = btnSelectNoTemplateClick
            TabOrder = 4
          end
          object chkEvaluateVarsBeforeCalling: TCheckBox
            Left = 358
            Height = 19
            Hint = 'If unchecked, the values are passed as strings.'
            Top = 14
            Width = 153
            Caption = 'Evaluate vars before calling'
            OnClick = chkEvaluateVarsBeforeCallingClick
            ParentShowHint = False
            ShowHint = True
            TabOrder = 5
          end
          object lbeTemplateFileName: TLabeledEdit
            Left = 3
            Height = 21
            Hint = 'Replacements are available'
            Top = 17
            Width = 229
            EditLabel.Height = 13
            EditLabel.Width = 229
            EditLabel.Caption = 'Template file name'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 6
            OnChange = lbeTemplateFileNameChange
          end
          object spdbtnBrowseLocalTemplates: TSpeedButton
            Left = 232
            Height = 21
            Hint = 'Templates from the local dir'
            Top = 17
            Width = 16
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
            OnClick = spdbtnBrowseLocalTemplatesClick
            ShowHint = True
            ParentShowHint = False
          end
          object btnBrowseOtherTemplates: TButton
            Left = 248
            Height = 21
            Top = 17
            Width = 24
            Caption = '...'
            OnClick = btnBrowseOtherTemplatesClick
            TabOrder = 7
          end
          object lblSetVarToHttpInfo1: TLabel
            Left = 3
            Height = 13
            Hint = 'Every time a variable, with such a value, is evaluated, an http call is made.'
            Top = 216
            Width = 483
            Caption = 'HTTP calls are available, as var values, using the following format: $http://<server:port>/[params]$'
            ParentShowHint = False
            ShowHint = True
          end
          object vstCustomVariables: TVirtualStringTree
            Left = 3
            Height = 156
            Hint = 'These variables are set on every loop iteration.'
            Top = 58
            Width = 371
            Colors.UnfocusedColor = clMedGray
            Colors.UnfocusedSelectionColor = clGradientInactiveCaption
            DefaultText = 'Node'
            Header.AutoSizeIndex = 0
            Header.Columns = <            
              item
                MinWidth = 150
                Position = 0
                Text = 'Variable'
                Width = 150
              end            
              item
                MinWidth = 200
                Position = 1
                Text = 'Value'
                Width = 200
              end>
            Header.DefaultHeight = 17
            Header.Height = 21
            Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
            Header.Style = hsFlatButtons
            Indent = 4
            ParentShowHint = False
            PopupMenu = pmCustomVars
            ShowHint = True
            TabOrder = 8
            TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toDisableAutoscrollOnEdit]
            TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnDblClick]
            TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowVertGridLines, toThemeAware, toUseBlendedImages]
            TreeOptions.SelectionOptions = [toFullRowSelect]
            OnDblClick = vstCustomVariablesDblClick
            OnEdited = vstCustomVariablesEdited
            OnEditing = vstCustomVariablesEditing
            OnGetText = vstCustomVariablesGetText
            OnKeyDown = vstCustomVariablesKeyDown
            OnMouseUp = vstCustomVariablesMouseUp
            OnNewText = vstCustomVariablesNewText
          end
          object grpCallTemplateLoop: TGroupBox
            Left = 384
            Height = 174
            Top = 40
            Width = 304
            Caption = 'Loop'
            ClientHeight = 156
            ClientWidth = 300
            TabOrder = 9
            object chkCallTemplateLoopEnabled: TCheckBox
              Left = 8
              Height = 19
              Top = 0
              Width = 58
              Caption = 'Enabled'
              OnChange = chkCallTemplateLoopEnabledChange
              TabOrder = 0
            end
            object lbeCallTemplateLoopCounter: TLabeledEdit
              Left = 8
              Height = 21
              Hint = 'Replacements are available'
              Top = 40
              Width = 88
              EditLabel.Height = 13
              EditLabel.Width = 88
              EditLabel.Caption = 'Counter'
              ParentShowHint = False
              ShowHint = True
              TabOrder = 1
              OnChange = lbeCallTemplateLoopCounterChange
            end
            object lbeCallTemplateLoopInitValue: TLabeledEdit
              Left = 104
              Height = 21
              Hint = 'Replacements are available'
              Top = 40
              Width = 192
              EditLabel.Height = 13
              EditLabel.Width = 192
              EditLabel.Caption = 'Init value'
              ParentShowHint = False
              ShowHint = True
              TabOrder = 2
              OnChange = lbeCallTemplateLoopInitValueChange
            end
            object lbeCallTemplateLoopEndValue: TLabeledEdit
              Left = 104
              Height = 21
              Hint = 'Replacements are available.'#13#10'If the expression evaluator can''t properly parse this string, please use a SetVar action to break down the expression into smaller pieces.'#13#10#13#10'For example, $Diff($ItemCount($RemoteVars$)$,1)$ can be split into the following SetVar items (both set to be evaluated):'#13#10#13#10'$RemoteVarsCount$ = $ItemCount($RemoteVars$)$'#13#10'$LastItemIndex$ = $Diff($RemoteVarsCount$,1)$'
              Top = 88
              Width = 192
              EditLabel.Height = 13
              EditLabel.Width = 192
              EditLabel.Caption = 'End value'
              ParentShowHint = False
              ShowHint = True
              TabOrder = 3
              OnChange = lbeCallTemplateLoopEndValueChange
            end
            object cmbCallTemplateLoopDirection: TComboBox
              Left = 8
              Height = 19
              Hint = 'Loop direction can be:'#13#10'Inc - when the counter should be incremented.'#13#10'Dec - when the counter should be decremented.'#13#10'Auto - when the init and the end values can be anything, so the counter direction has to be evaluated before starting the loop.'
              Top = 88
              Width = 88
              ItemHeight = 13
              ItemIndex = 0
              Items.Strings = (
                'Inc'
                'Dec'
                'Auto'
              )
              OnChange = cmbCallTemplateLoopDirectionChange
              ParentShowHint = False
              ShowHint = True
              Style = csOwnerDrawFixed
              TabOrder = 4
              Text = 'Inc'
            end
            object lblCallTemplateLoopDirection: TLabel
              Left = 8
              Height = 13
              Top = 72
              Width = 42
              Caption = 'Direction'
            end
            object lblCallTemplateLoopBreakPosition: TLabel
              Left = 8
              Height = 13
              Top = 118
              Width = 67
              Caption = 'Break position'
            end
            object cmbCallTemplateLoopBreakPosition: TComboBox
              Left = 8
              Height = 19
              Hint = 'The "Break" call can be made, before or after the actual "CallTemplate" call.'
              Top = 134
              Width = 88
              ItemHeight = 13
              ItemIndex = 0
              Items.Strings = (
                'After'
                'Before'
              )
              OnChange = cmbCallTemplateLoopBreakPositionChange
              ParentShowHint = False
              ShowHint = True
              Style = csOwnerDrawFixed
              TabOrder = 5
              Text = 'After'
            end
            object lbeCallTemplateLoopBreakCondition: TLabeledEdit
              Left = 104
              Height = 21
              Hint = 'This editbox is read-only. Please use the "..." button next to it, to open the condition editor.'
              Top = 134
              Width = 160
              EditLabel.Height = 13
              EditLabel.Width = 160
              EditLabel.Caption = 'Break condition'
              ParentShowHint = False
              ReadOnly = True
              ShowHint = True
              TabOrder = 6
              OnChange = lbeCallTemplateLoopBreakConditionChange
            end
            object btnBrowseCallTemplateLoopBreakCondition: TButton
              Left = 272
              Height = 21
              Top = 134
              Width = 24
              Caption = '...'
              OnClick = btnBrowseCallTemplateLoopBreakConditionClick
              TabOrder = 7
            end
            object lblFeatureInWork: TLabel
              Left = 104
              Height = 21
              Hint = 'What does not work, is closing subtemplates when remote debugging.'#13#10'So, do not click the stop button when remote debugging CallTemplate actions with loops.'
              Top = 0
              Width = 165
              Caption = '[Feature in work...]'
              Font.Color = clGray
              Font.Height = -17
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentFont = False
              ParentShowHint = False
              ShowHint = True
            end
          end
        end
        object TabSheetActionSleep: TTabSheet
          Caption = 'Sleep'
          ClientHeight = 233
          ClientWidth = 882
          ImageIndex = 6
          object lblSleepInfo: TLabel
            Left = 8
            Height = 13
            Hint = 'Waiting for the proper event or control property, is the right way to solve a race condition. Use the "sleep" action if the event/property is not available.'
            Top = 64
            Width = 379
            Caption = 'Use this action only as a last resort (e.g. blinking or resizing controls/windows).'
            ParentShowHint = False
            ShowHint = True
          end
          object lblSleepInfo2: TLabel
            Left = 130
            Height = 13
            Top = 31
            Width = 193
            Caption = 'Sleep values, lower than 1, are ignored.'
          end
          object lbeSleep: TLabeledEdit
            Left = 3
            Height = 21
            Hint = 'Variable replacements are available.'
            Top = 28
            Width = 121
            EditLabel.Height = 13
            EditLabel.Width = 121
            EditLabel.Caption = 'Sleep [ms]'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnChange = lbeSleepChange
          end
          object pnlSleepElapsedTime: TPanel
            Left = 3
            Height = 24
            Top = 83
            Width = 182
            Alignment = taLeftJustify
            Caption = 'Elapsed Time [ms]:'
            TabOrder = 1
          end
          object pnlSleepRemainingTime: TPanel
            Left = 3
            Height = 24
            Top = 113
            Width = 182
            Alignment = taLeftJustify
            Caption = 'Remaining Time [ms]:'
            TabOrder = 2
          end
          object prbSleep: TProgressBar
            Left = 3
            Height = 17
            Top = 143
            Width = 182
            Smooth = True
            TabOrder = 3
          end
        end
        object TabSheetActionSetVar: TTabSheet
          Caption = 'Set Variable'
          ClientHeight = 233
          ClientWidth = 882
          ImageIndex = 7
          object vstSetVar: TVirtualStringTree
            Left = 0
            Height = 200
            Top = 8
            Width = 464
            CheckImageKind = ckXP
            Colors.UnfocusedColor = clMedGray
            Colors.UnfocusedSelectionColor = clGradientInactiveCaption
            DefaultText = 'Node'
            Header.AutoSizeIndex = 0
            Header.Columns = <            
              item
                CheckBox = True
                MinWidth = 100
                Position = 0
                Text = 'Eval before set'
                Width = 100
              end            
              item
                MinWidth = 150
                Position = 1
                Text = 'Variable'
                Width = 150
              end            
              item
                MinWidth = 300
                Position = 2
                Text = 'Value'
                Width = 300
              end>
            Header.DefaultHeight = 21
            Header.Height = 21
            Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible]
            Header.Style = hsFlatButtons
            Indent = 4
            PopupMenu = pmSetVars
            StateImages = imglstSetVar
            TabOrder = 0
            TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toDisableAutoscrollOnEdit]
            TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
            TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages]
            TreeOptions.SelectionOptions = [toFullRowSelect, toMiddleClickSelect, toRightClickSelect]
            OnChecked = vstSetVarChecked
            OnChecking = vstSetVarChecking
            OnDblClick = vstSetVarDblClick
            OnEdited = vstSetVarEdited
            OnEditing = vstSetVarEditing
            OnGetText = vstSetVarGetText
            OnGetImageIndex = vstSetVarGetImageIndex
            OnInitNode = vstSetVarInitNode
            OnKeyDown = vstSetVarKeyDown
            OnMouseUp = vstSetVarMouseUp
            OnNewText = vstSetVarNewText
          end
          object lblSetVarToHttpInfo: TLabel
            Left = 0
            Height = 13
            Hint = 'Every time a variable, with such a value, is evaluated, an http call is made.'
            Top = 216
            Width = 483
            Caption = 'HTTP calls are available, as var values, using the following format: $http://<server:port>/[params]$'
            ParentShowHint = False
            ShowHint = True
          end
          object lblAvailableFunctions: TLabel
            Left = 472
            Height = 13
            Hint = 'Special vars are either updated on every use, or their values can''t be changed (consts).'
            Top = 8
            Width = 174
            Caption = 'Available functions and special vars:'
            ParentShowHint = False
            ShowHint = True
          end
          object memAvailableFunctions: TMemo
            Left = 472
            Height = 184
            Hint = 'The "$GetSelfHandles()$" and "$Exit(<ExitCode>)$" functions do not return a value, so they must be placed in the "Variable" column of a SetVar action.'#13#10'Also, they can''t be evaluated using the console or as part of a condition.'#13#10'All the other function should be called from the "Value" column.'
            Top = 24
            Width = 280
            Color = clBtnFace
            Lines.Strings = (
              '$ExtractFileDir(<DirName>)$ '
              '$Random(<min>, <max>)$'
              '$Random(<max>)$'
              '$Sum(<op1>, <op2>)$'
              '$Diff(<op1>, <op2>)$'
              '$http://<server:port>/[params]$'
              '$FastReplace_45ToReturn(<some_string>)$'
              '$FastReplace_ReturnTo45(<some_string>)$'
              '$FastReplace_45To87(<some_string>)$'
              '$FastReplace_87To45(<some_string>)$'
              '$Exit(<ExitCode>)$'
              '$CreateDir(<PathToNewDir>)$'
              '$LoadTextFile(<PathToTextFile>)$'
              '$ItemCount($TextFileContent$)$'
              '$GetTextItem($TextFileContent$,<ItemIndex>)$'
              '$StringContains(<SubString>, <String>)$'
              '$ExtractFileName(<PathToFile>)$'
              '$ExtractFileExt(<PathToFile>)$'
              '$ExtractFileNameNoExt(<PathToFile>)$'
              '$UpdateControlInfo(<Handle>)$'
              '$GetSelfHandles()$'
              '$GetKeyNameFromPair(<key>=<value)$'
              '$GetKeyValueFromPair(<key>=<value)$'
              '$Chr(<ByteValue>)$'
              ''
              '$IncBrightness(<HexColor>[,Amount])$'
              '$DecBrightness(<HexColor>[,Amount])$'
              '$IncBrightnessR(<HexColor>[,Amount])$'
              '$IncBrightnessG(<HexColor>[,Amount])$'
              '$IncBrightnessB(<HexColor>[,Amount])$'
              '$DecBrightnessR(<HexColor>[,Amount])$'
              '$DecBrightnessG(<HexColor>[,Amount])$'
              '$DecBrightnessB(<HexColor>[,Amount])$'
              ''
              '$Current_Mouse_X$'
              '$Current_Mouse_Y$'
              '$CRLF$'
              '$#4#5$'
              '$Now$'
              ''
            )
            ParentShowHint = False
            ReadOnly = True
            ScrollBars = ssBoth
            ShowHint = True
            TabOrder = 1
            WordWrap = False
          end
        end
        object TabSheetActionWindowOperations: TTabSheet
          Caption = 'Window Operations'
          ClientHeight = 233
          ClientWidth = 882
          ImageIndex = 8
          object cmbWindowOperationsType: TComboBox
            Left = 8
            Height = 19
            Top = 32
            Width = 116
            ItemHeight = 13
            ItemIndex = 0
            Items.Strings = (
              'Bring to front'
              'Move / resize'
              'Close'
            )
            OnChange = cmbWindowOperationsTypeChange
            Style = csOwnerDrawFixed
            TabOrder = 0
            Text = 'Bring to front'
          end
          object lblWindowOperation: TLabel
            Left = 8
            Height = 13
            Top = 16
            Width = 52
            Caption = 'Operation:'
          end
          object lbeWindowOperationsX: TLabeledEdit
            Left = 8
            Height = 21
            Top = 80
            Width = 176
            EditLabel.Height = 13
            EditLabel.Width = 176
            EditLabel.Caption = 'New X'
            Enabled = False
            TabOrder = 1
            OnChange = lbeWindowOperationsXChange
          end
          object lbeWindowOperationsY: TLabeledEdit
            Left = 8
            Height = 21
            Top = 128
            Width = 176
            EditLabel.Height = 13
            EditLabel.Width = 176
            EditLabel.Caption = 'New Y'
            Enabled = False
            TabOrder = 2
            OnChange = lbeWindowOperationsYChange
          end
          object lbeWindowOperationsWidth: TLabeledEdit
            Left = 200
            Height = 21
            Top = 80
            Width = 176
            EditLabel.Height = 13
            EditLabel.Width = 176
            EditLabel.Caption = 'New Width'
            Enabled = False
            TabOrder = 3
            OnChange = lbeWindowOperationsWidthChange
          end
          object lbeWindowOperationsHeight: TLabeledEdit
            Left = 200
            Height = 21
            Top = 128
            Width = 176
            EditLabel.Height = 13
            EditLabel.Width = 176
            EditLabel.Caption = 'New Height'
            Enabled = False
            TabOrder = 4
            OnChange = lbeWindowOperationsHeightChange
          end
          object chkWindowOperationsEnablePos: TCheckBox
            Left = 8
            Height = 19
            Top = 168
            Width = 76
            Caption = 'Set position'
            OnChange = chkWindowOperationsEnablePosChange
            TabOrder = 5
          end
          object chkWindowOperationsEnableSize: TCheckBox
            Left = 200
            Height = 19
            Top = 168
            Width = 57
            Caption = 'Set size'
            OnChange = chkWindowOperationsEnableSizeChange
            TabOrder = 6
          end
          object btnSetFromControlLeftAndTop: TButton
            Left = 8
            Height = 25
            Hint = 'Evaluates $Control_Left$ and $Control_Top$ variables and updates "New X" and "New Y" to them.'
            Top = 192
            Width = 176
            Caption = 'Set from $Control_Left/Top$'
            OnClick = btnSetFromControlLeftAndTopClick
            ParentShowHint = False
            ShowHint = True
            TabOrder = 7
          end
          object btnSetFromControlWidthAndHeight: TButton
            Left = 200
            Height = 25
            Hint = 'Evaluates $Control_Width$ and $Control_Height$ variables and updates "New Width" and "New Height" to them.'
            Top = 192
            Width = 176
            Caption = 'Set from $Control_Width/Height$'
            OnClick = btnSetFromControlWidthAndHeightClick
            ParentShowHint = False
            ShowHint = True
            TabOrder = 8
          end
        end
      end
      object lbeActionName: TLabeledEdit
        Left = 1
        Height = 21
        Hint = 'General purpose string'
        Top = 22
        Width = 137
        EditLabel.Height = 13
        EditLabel.Width = 137
        EditLabel.Caption = 'Action Name'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnChange = lbeActionNameChange
      end
      object cmbActions: TComboBox
        Left = 1
        Height = 22
        Top = 62
        Width = 137
        ItemHeight = 16
        Items.Strings = (
          'Click'
          'ExecApp'
          'FindControl'
          'FindSubControl'
          'SetControlText'
          'CallTemplate'
          'Sleep'
          'SetVar'
          'WindowOperations'
        )
        OnChange = cmbActionsChange
        Style = csOwnerDrawFixed
        TabOrder = 1
      end
      object lbeActionTimeout: TLabeledEdit
        Left = 1
        Height = 21
        Hint = 'Implemented for FindControl, FindSubControl and ExecApp.'#13#10'The FindSubControl algorithm may often take longer than this timeout, which is by design.'
        Top = 111
        Width = 120
        EditLabel.Height = 13
        EditLabel.Width = 120
        EditLabel.Caption = 'Action Timeout [ms]'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnChange = lbeActionTimeoutChange
      end
      object prbTimeout: TProgressBar
        Left = 1
        Height = 8
        Top = 136
        Width = 137
        Smooth = True
        TabOrder = 3
      end
      object chkStopOnError: TCheckBox
        Left = 49
        Height = 19
        Top = 196
        Width = 86
        Caption = 'Stop On Error'
        Checked = True
        State = cbChecked
        TabOrder = 4
      end
    end
    object TabSheetCondition: TTabSheet
      Caption = 'Condition'
      ClientHeight = 268
      ClientWidth = 1024
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      ImageIndex = 1
      ParentFont = False
      object pnlActionConditions: TPanel
        Left = 0
        Height = 272
        Top = 0
        Width = 1022
        Anchors = [akTop, akLeft, akRight, akBottom]
        ParentColor = False
        TabOrder = 0
      end
    end
    object TabSheetDebugging: TTabSheet
      Caption = 'Debugging'
      ClientHeight = 268
      ClientWidth = 1024
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      ImageIndex = 2
      ParentFont = False
      object lblDebugBitmapXMouseOffset: TLabel
        Left = 544
        Height = 13
        Top = 0
        Width = 27
        Caption = 'mx: 0'
      end
      object lblDebugBitmapYMouseOffset: TLabel
        Left = 613
        Height = 13
        Top = 0
        Width = 27
        Caption = 'my: 0'
      end
      object lblVarReplacements: TLabel
        Left = 3
        Height = 13
        Top = 0
        Width = 120
        Caption = 'Variables / Replacements'
      end
      object lblBitmaps: TLabel
        Left = 341
        Height = 13
        Top = 0
        Width = 37
        Caption = 'Bitmaps'
      end
      object vallstVariables: TValueListEditor
        Left = 3
        Height = 218
        Hint = 'These variables can be manually edited in place for every action. They are reset when playing all actions. Right-click for options.'
        Top = 18
        Width = 332
        Anchors = [akTop, akLeft, akBottom]
        DefaultColWidth = 170
        FixedCols = 0
        ParentShowHint = False
        PopupMenu = pmDebugVars
        RowCount = 2
        ScrollBars = ssVertical
        ShowHint = True
        TabOrder = 1
        DisplayOptions = [doColumnTitles, doKeyColFixed]
        KeyOptions = [keyEdit, keyUnique]
        TitleCaptions.Strings = (
          'Variable'
          'Value'
        )
        OnValidate = vallstVariablesValidate
        ColWidths = (
          170
          162
        )
      end
      object scrboxDebugBmp: TScrollBox
        Left = 341
        Height = 218
        Top = 18
        Width = 680
        HorzScrollBar.Page = 87
        HorzScrollBar.Smooth = True
        HorzScrollBar.Tracking = True
        VertScrollBar.Page = 86
        VertScrollBar.Smooth = True
        VertScrollBar.Tracking = True
        Anchors = [akTop, akLeft, akRight, akBottom]
        ClientHeight = 214
        ClientWidth = 676
        Color = clWindow
        ParentColor = False
        PopupMenu = pmDebugImage
        TabOrder = 2
        OnMouseWheel = scrboxDebugBmpMouseWheel
        object imgDebugBmp: TImage
          Left = 0
          Height = 2
          Top = 0
          Width = 2
          AutoSize = True
          OnMouseMove = imgDebugBmpMouseMove
          Picture.Data = {
            07544269746D617046000000424D460000000000000036000000280000000200
            0000020000000100180000000000100000000000000000000000000000000000
            0000FFFFFFFFFFFF0000FFFFFFFFFFFF0000
          }
          PopupMenu = pmDebugImage
        end
        object imgDebugGrid: TImage
          Left = 14
          Height = 73
          Top = 13
          Width = 73
          OnMouseMove = imgDebugBmpMouseMove
          ParentShowHint = False
          PopupMenu = pmDebugImage
          ShowHint = True
          Transparent = True
        end
      end
      object chkShowDebugGrid: TCheckBox
        Left = 456
        Height = 19
        Top = -2
        Width = 67
        Caption = 'Show grid'
        OnClick = chkShowDebugGridClick
        TabOrder = 0
      end
      object lblMouseOnExecDbgImgBB: TLabel
        Left = 676
        Height = 13
        Top = 0
        Width = 12
        Caption = 'BB'
        Font.Color = 16734553
        Font.Height = -11
        Font.Name = 'Tahoma'
        ParentFont = False
      end
      object lblMouseOnExecDbgImgGG: TLabel
        Left = 692
        Height = 13
        Top = 0
        Width = 14
        Caption = 'GG'
        Font.Color = clGreen
        Font.Height = -11
        Font.Name = 'Tahoma'
        ParentFont = False
      end
      object lblMouseOnExecDbgImgRR: TLabel
        Left = 708
        Height = 13
        Top = 0
        Width = 14
        Caption = 'RR'
        Font.Color = 187
        Font.Height = -11
        Font.Name = 'Tahoma'
        ParentFont = False
      end
    end
  end
  object pmCommonTimeouts: TPopupMenu
    Left = 64
    Top = 224
    object N01: TMenuItem
      Caption = '0'
      OnClick = N01Click
    end
    object N10001: TMenuItem
      Caption = '1000'
      OnClick = N01Click
    end
    object N100001: TMenuItem
      Caption = '10000'
      OnClick = N01Click
    end
    object N300001: TMenuItem
      Caption = '30000'
      OnClick = N01Click
    end
  end
  object pmCustomVars: TPopupMenu
    Left = 256
    Top = 224
    object AddCustomVarRow1: TMenuItem
      Caption = 'Add Variable'
      OnClick = AddCustomVarRow1Click
    end
    object RemoveCustomVarRow1: TMenuItem
      Caption = 'Remove Variable...'
      OnClick = RemoveCustomVarRow1Click
    end
  end
  object imglstActions: TImageList
    Height = 24
    Width = 24
    Left = 128
    Top = 224
    Bitmap = {
      4C7A1300000018000000180000009A0A00000000000078DAED9C6D6C14C719C7
      FD9DCF4502811005FA810FA8028937B50241010B709A0AABAD0B898B45DD8A0F
      552B3952B94A50242091DA58D88EA97DF639BEF80562887D360AF14B2E186C37
      B14D0A96306FF6D986D82476FC4600DBC5424F77C6B7CB3237BB3B3B3B735CE2
      99D55FB3BB77FB7B9E79E699B9396B7C4949F2CB860D1B40267FD7AE5DB075EB
      56693676EEDC09858585B079F36690C5D72AF0F97CB065CB1690C5474A4F4F87
      94941490C547DABD7B37646464802C3E12EA8BA3478F0AB1B163C78E187E7D7D
      3DAC5DBB164A4A4A3CDBD8BE7D7B0C1F293F3F1FDB0887C39E6C447386AAACAC
      2CD8B871A327FEA64D9B0C5E5E5E1E1C3E7C18962D5B06AB57AF8675EBD6E173
      340679F988D1DDDD0DA9A9A9B060C1023872E4082C5DBA14C705C5FFF8F1E370
      F2E4496EFE9A356B60DFBE7D80EAE4E464D8B66D1BAC5CB9128F3711F9B370E1
      42CCBE7AF52A949696C292254BE0C08103383622F87BF6EC818E8E0ECC42715A
      B56A15F67DF1E2C59E738756F6EFDF0FEBD7AFC7630CF589683E8AD3A2458B70
      CC962F5F8EFB5EB40DD4CFC8FF13274E48FDEC51451555543117E80B0114FC02
      E0CCCF01DEDBACD51BB47A9356FF6CEE3A7F1D40EE4FB57AA376ADBD76663BC0
      3BAF035CBE00CCFCCC15007F583157EB42D787A235924FB3F99666E72F7BA133
      F5AF9AAD3C36FE253FC0FD1980C82440DF23AD8ECA7C8ED4F3ADA65180DE614D
      F760CC97C5C66FFD04E0A315EC7AEF5F582399BF63E3375F349E4D9EE88E118E
      9F59D1F77EFDC73719F997E69ED19EA5F268D2DE7F3F9391FF593D17BF3FE3F7
      6CFC702317BFE7E041367E43D38BBE636423DD4D67E537BBCB9F680EDD7CF310
      1BBFB6C1C83937BAF5C621E6F1FBED6F53E1AB5FEEC57A10ADADD4F7DA1E18FA
      550A5C7DFD0DB59651459557F1992EB058F1CFF7F678565A5A9A2DDFFCB701F2
      9A7CCD2CFD1E0B9F7C86764EDA10CDB7BAE7363E4E7CD29648FF696D11CD27DB
      21323E3CFD2B2BFFD17D514AB4B969E59231304B349B664B765B143BBEFC78E5
      68A2F101CD3C3215873272B7508AF479199DCBF097E48B98FF11D38EEF75FDC3
      C2F7B2FE11C5B7B9E72A3E6ED73F22FDA7B545349FD20E61F1E1E95F59F92F72
      AEA5F165CF3F7622D73F22F924CF8D0D9EF8C8E4CB880F2F3BD1F832F3D38BFF
      F3657DE8A588FC6E47FB9E87CE45979696165BBEFE1973ECD831E35C24DF6C47
      84FFE4FA89E4D3EC985F2365E53FC9A1D972B267171F3BBED957A758CE47BECC
      F83BF158F9B2F25FE6F845E732A4FEAAAFCA7C2FEDD573C355494984A2F37CDC
      F9567FC722D664DEF803A3D612C5BF759F2E0E3E796EF049DF3DF0F56B917C4B
      09E0D36C89F4DF365682F932F287AB5F5C8C2F25A504FBBC52E57BB0CFE861F9
      FB702D798B2174CDBAB7C8CD3E26DDCEFFBEF95A387FE46208BA33D361AABFCF
      D5DE28163E7A0DF91D397114B741747CD06B3AD70B9FC62199E67E26FB9B854D
      7B5E97DEC74843C100DCF8F56BF87E926921E0C6FFC14081C1467D6B2EFDEFBE
      8DEF0F7D10E08EFF83825CCC407E4E0F3E30EE7FF3D1B93976691177FF8E851B
      0DDF471B2EBDC8D3BA6A7CEFC1991CEEFC99191A34624B72ECFA8685FF7C6606
      BAFF74D088F9F3E9A998316C955B2C7CAB98F3EE6D349789D62B467B6FFF3933
      C647A739C46BFE5B8D5B51E3D7ABFFAAC4FDF336EEFA51D23B609668B6F9DAEE
      EF5B3CB6ED9E31B707D9E56D1F2D36B47311B133B348BB3C7C32DE762CAFB121
      9F778A9B12BF7EE36B00992C91FD6497175EED902C32BFF5716F37C6ACC6068D
      457B8E755CD98D11BBB9CB8ACF3A3F90F3204B5B59E6371ACB693EB09B1FDCE4
      89CCFC8CC7F85252F2A2BDB53F06AF5AFAF0E39744F2FF1649712DF49D06D5E8
      F9D0E084211A5F7FAF134BFFAE64BECFC2F72A27FE4FCA805B6EF8643FE912C5
      97EDBF8A4F62C6C72E6E89E0BFCCF18BAECDAF7B95E2BBE78B965A17282929B1
      EE9D407B4ECDFB42C89ADC37E274DF4D6DDA530E4EF7D19E5D5D5EEDD26ADD9E
      533C78E363752E936F6E8FBEE7998C1F2F5F49494992E270044B82D095DD25CD
      5A59B00CCA03E5F2F8656580140E844126BF34500AA14048B88DDCDC5CFC1BBA
      4815A515102A156BC3CCC7360215501BA805597CDDC6F5C075213672727262F8
      E8F7BE2F072EC3F542EF364E9F3E1DC3D76D34163742A430E2C946767636958F
      7E5FDAEFF743B9BF1CAE9CB9C26DA3A8A8087310F3D4A95380ECA136217E7171
      31349637425D699D677E797939563018C4F2E7FBA1D9DF0C2DFE16B8E2E7F7BF
      A0A000F3ABAAAA5EE287FD6188F8239EFB17FD4675654125DCA8BA01E5C1B936
      207B882F223FDB72DB60F09F839835903700D5D5D580625615A882487644F87C
      112E0A633EB65150259C3FF0EE000402012CD406D1FC895313102C0A62FF4345
      A1387DCAA9431DEA5087768CDF06E80C02749400B4176B7551B40E44EB42802F
      FE1DBDAFA9437B6FCB5980FE6E60E6D7E5D0556B3AFF54B3D5A8D9F9A402863E
      ACD76CB5B3F1EF5D03989C05189FD63463ADB1A7513DD1340A539F36B0F1EFF7
      00DCCA61577B1BD693BA0B6CFCFEBBC6B365D32331C2F1332BFADEC717AB19F9
      F7E69ED19EA5F268D2DE3F59C7C8EFEBE1E24F846A18F9BD5CFCB110E367696F
      E445DF31B291466B58F9FDEEF2279A43C3D58CDF49EEF41A39E74623AC7CADCD
      4FCF7F088FCE56604D466B2B8D5756C077E72A61E06CB55ACBA8431DAFE210FA
      03D0747E6472D2B39A9A9A6CF9E8BBB02EF29A7CCD2CFD1E0B9F7C86764EDA10
      CDB7BAE7363E4E7CD29648FF696D11CD27DB21323E3CFD2B2BFFD17D514AB4B9
      A9B26C1ACC12CDA6D992DD16C58E2F3F5E399A80FCEFFDEF3F4F4D744B515A5A
      5A92CE97E12FC91735FFDBF145AC7F9CF85ED73F22F876EB1F37F1E159FF88F2
      DF6AFD23924F5BFF888A0F4FFFCACA7F91EB1F1A5FF6FC632772FD23924FF2DC
      D8E0898F4CBE8CF8F0B2138D2F333FBDF8CF2A9DEFE619ECFF0FE0686D6D0519
      32F34597E1E1615B7E45450556575797712E926FB623C27FDD47D257FD9C66C7
      FC1A292BFF490ECD96933DBBF8D8F1CDBE3AC5723EF265C6DF89C7CA9795FF32
      C72F3A9721F5577D75CCF743FDFEB39248A13D87AF82AFEFD7B49357BE8876C7
      936F752E92AF5F8BE4F3F41F4FDEC9F09FA52F648C9B78F0458C2F25A5F9F6FF
      5FEAF0BECFE8D1F56B30F8BEDF10BA66DE5BE4621F936E67F6F177C2F94F6E77
      C370CD7978363EE66E6F1403DFE7F361BFC73E6BC26D101D1FC4D7B99EF83E88
      E1904C733FC7F4B7039FCC13527A1F637DD9090F2B3FC0F751FB78FC9FEC6C37
      D8A86FCD65BCA579CEF7FF7672C77FA2FD3F9881FC7C363961DC7F7CB36B8EFD
      650777FF3EEDED317C7F7AEFCE8B3CBD7513DF9BF8A28D3B7F661F4D1AB12539
      767DC3C27F3E3B0BC3A10B46CC9F3F7B163386AD728B856F1573EEBD8DA6636A
      A0CF68EFC8C59A181F8D3CE4E43BE5BFE5B815357E7DDEE2A38EB87FDEC65DA7
      DFFE1CCC12CD365F0FDDB1FEFB168F6DBB67CCED417679DB478B0DED5C44ECCC
      2CD22E0F9F8CB71DCB6B6CC8E79DE2A6C4AF9AE63E90C912D94F7679E1D50EC9
      22F35B1FF77663CC6A6CD058B4E758C795DD18B19BBBACF8ACF303390FB2B495
      657EA3B19CE603BBF9C14D9EC8CCCF788C2F25252FFA47F6DFC1AB6A3BDB5E12
      C93FF771996B65651CC4357A3EF2D590211A5F7FAF130BD5E6F7A27316BE5739
      F12B2EB570CB0D9FEC275DA2F8B2FD57F149CCF8D8C52D11FC97397ED1B5F975
      AF527CF77CD1FA217CAEFF1F866DEA51
    }
  end
  object pmDebugVars: TPopupMenu
    Left = 224
    Top = 224
    object CopyDebugValuesListToClipboard1: TMenuItem
      Caption = 'Copy debug values list to clipboard'
      OnClick = CopyDebugValuesListToClipboard1Click
    end
    object PasteDebugValuesListFromClipboard1: TMenuItem
      Caption = 'Paste debug values list from clipboard'
      OnClick = PasteDebugValuesListFromClipboard1Click
    end
    object PasteDebugValuesListFromMainExecutionList1: TMenuItem
      Caption = 'Paste debug values list from main execution list'
      Enabled = False
      Bitmap.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000C9AEFFE8A200
        E8A200E8A200E8A200E8A200E8A200E8A200E8A200E8A200E8A200E8A200E8A2
        00E8A200E8A200E8A200C9AEFFE8A200E8A200E8A200E8A200E8A200E8A200E8
        A200E8A200E8A200E8A200E8A200E8A200E8A200E8A200E8A200C9AEFFE8A200
        E8A200E8A200E8A200E8A200E8A200E8A200E8A200E8A200E8A200E8A200E8A2
        00E8A200E8A200E8A200C9AEFFE8A200E8A200E8A200E8A200E8A200E8A200FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC9AEFFE8A200
        E8A200E8A200E8A200E8A200E8A200FFFFFFE8A200E8A200E8A200E8A200E8A2
        00E8A200E8A200FFFFFFC9AEFFE8A200E8A200E8A200FFFFFFE8A200E8A200FF
        FFFFE8A200E8A200E8A200E8A200E8A200E8A200E8A200FFFFFFC9AEFFE8A200
        E8A200E8A200FFFFFFFFFFFFE8A200FFFFFFE8A200E8A200E8A200E8A200E8A2
        00E8A200E8A200FFFFFFC9AEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFE8A200E8A200E8A200E8A200E8A200E8A200E8A200FFFFFFC9AEFFFFFFFF
        E8A200E8A200FFFFFFFFFFFFE8A200FFFFFFE8A200E8A200E8A200E8A200E8A2
        00E8A200E8A200FFFFFFC9AEFFFFFFFFE8A200E8A200FFFFFFE8A200E8A200FF
        FFFFE8A200E8A200E8A200E8A200E8A200E8A200E8A200FFFFFFC9AEFFFFFFFF
        E8A200E8A200E8A200E8A200E8A200FFFFFFE8A200E8A200E8A200E8A200E8A2
        00E8A200E8A200FFFFFFC9AEFFFFFFFFE8A200E8A200E8A200E8A200E8A200FF
        FFFFE8A200E8A200E8A200E8A200E8A200E8A200E8A200FFFFFFC9AEFFFFFFFF
        E8A200E8A200E8A200E8A200E8A200FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFC9AEFFFFFFFFE8A200E8A200E8A200E8A200E8A200E8
        A200E8A200E8A200E8A200E8A200E8A200E8A200E8A200E8A200C9AEFFE8A200
        E8A200E8A200E8A200E8A200E8A200E8A200E8A200E8A200E8A200E8A200E8A2
        00E8A200E8A200E8A200C9AEFFC9AEFFC9AEFFC9AEFFC9AEFFC9AEFFC9AEFFC9
        AEFFC9AEFFC9AEFFC9AEFFC9AEFFC9AEFFC9AEFFC9AEFFC9AEFF
      }
      OnClick = PasteDebugValuesListFromMainExecutionList1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object AddVariable1: TMenuItem
      Caption = 'Add Variable'
      OnClick = AddVariable1Click
    end
    object RemoveVariable1: TMenuItem
      Caption = 'Remove Variable...'
      OnClick = RemoveVariable1Click
    end
  end
  object imglstActionExecution: TImageList
    Left = 160
    Top = 224
    Bitmap = {
      4C7A0600000010000000100000007C0600000000000078DACD970B54CC591CC7
      FFAC43CEC16EDA3DE5600F2BA9ED7844E59137475659E788CA8A5E62B4B3212D
      A9653C4A134AA52253D114B335DB6B48B5A4D2430F69D04E6C4DB698F22E762D
      EDEBBBFFFFD48C31CDA3F97738EE39DF73FFF79EFBF93DEF7F1E04A1FB1897B3
      0E04CD21CC12C3ADEA102C2F6EA66563D9950050FE13EFE6E9CCA7B61449D9E5
      A5BB69F99E73792B26E4BAE16ABA48673EAA211366B7BE938A8EEFBEB0EB2AD9
      52F6F4DD7CAD7CD3F956F80A8FCBCFA52577D54C9BEFD6DCA708BC9508B35C77
      E9D92C4999F43CD5676A9D7DA65C23EF7F9323AF0F25AF6BE1605E3FD6DDAF1F
      B4C67D83BC5726175CE5BCECD998E40B526B7B55378FEAC36FC540E99B8A83BD
      AE79519AF02DD63CCF13F582169D7A36AFD0B78BCFF540A6A454E77EE7B456C2
      BB26129519F5B4DFD177394A4A4AA8B8D017B67B7E6F2C35582C963616EA44B1
      8AB3AAFC29DBEA248B5B69EF83AA97AA986535539393DAFA519C52AD54E983BA
      237DBD5F7D64690FC90B6854CACD7BD89E2F80B6739AE494110BCFF444DA36A6
      EE7101C1B2C628EF99D89FF226962A0934DABCF50870488E02116C837EC10BB0
      84970893C0D5B08E22BFCB0E4CED91575EDD73ECE2A5C2E71407C9571B30F2A8
      2BA69C0DC1DCEC588CE278C120CC09FA07EC3171CF5A957E65FCA684180C3BB8
      02B6B96970AD2AC7C0707BB0DADA60C50F81C56E6FDC68D61CF7B7597C187376
      60A3F026C624F8C0B9EC1C821E3FC1D2FC6498866C46402ABF075F43DADC972E
      005593016C5BB85EAB8663F9CF303CE186A027E477734B0BA66784C19CB509D6
      816B70FA8A486E83111F03733F3BD887FAE1CB7D6B49DF7E60D68B60CAFD1EAB
      4B33C1923CC096C6067CC1D90AA7C858343E057C4EC74B67996F992DC323CE58
      59740EDE75420C3EFA35764B24F06B14C3FD7A0DD987D9C8AF7EAD32F71397AE
      A1E8CE5FD0637F052F61256CF31330232314FE4DCD60DCFC0516BC439840C66E
      C372435DDB1B7FE56260197B1B16ECDF8469FE8E1878D0163EB76FC024C917AB
      8AD2C11489E0585C00BD8376C82C7D86801FF9F038110EC59EF1AB5AA46BCAEE
      68B60BD65CCD811E19BBFBF572B8D654C230C2131B6252E48C4B74883C7765B1
      2F566064B407FA1FB6C5ACCC68321F3BCCDAB5BD47DC9AC43E7F15A6A19B3189
      BD0531823AA92FCFB8A3D8CBCFA6FD0E5179527552DEA7351A08DA9F538E224F
      101D9F8068A669E301C991FE8D3157677E32964859E2D147F47CDF27593181E5
      B75C74E68D6005A293E8129DD117B67D989435C13CADFC865F9920FE1C2D3F37
      A5B2BB665A7C33C5DBD1AFD3585A1FEAAC35ECBACE3777ADA757DB6BF6FD7AEC
      9BFA507AF129883F4674F7ABBFD6B89DA87BD5A8C0CB9E497E51EDEADED5EDB9
      FEDB31507A36A4D7355F22747C9B6D22B0EE3643B79E49BA59B28E5658A673BF
      6DB002C4EF865851D7BBFFE800DE8B3AEEE7E0D19D38FCF7D017983A0AD49A2E
      4BCD8A3CF5AC49CA2CB556E6210ED2A8075CA2CB46F7BA076F3B4DAD2896F22D
      9DBBF79479CAB63AC9E25694B6FCD5E5AC28757C6F586DF1CBF2559793A6FA51
      9C62AD54495DFF14E3D6D44F65BEB7396BCA5FDDFDEACDFDA6CBBE6B154A0A11
      521BD2A77818250CECBCE24BDBC6C208330C8E1E00F3C04148C83D24B7D3F6B2
      4DA3CDF6CE7678E67961D0493D0CE10C85C3A5EDB00C1B8BC53C1B1071448FBC
      44ADB588BAC842F0B92D286CC8C5F81473CCBCE484A5150C8CCFB6C0E7DC7118
      7DDC08B3234D55FA95F1BBB31818C1198995352C307F3B05FD1423447696607E
      B123E687CFC0B3E78F34C6CD2ADB8B8982C5F093A4C3ECFC2C6C6C0C45EC3F95
      5825DC014B8E358E15ECEBC13F7ED1064E7128A89A7C9C680066730A3C1A2330
      26C31CB1FF56E1C8CBCB5850BA1E3322ACB024CC0405A26CB90D562603B3828D
      E01ABF08D3A34D49DF0B11F844008BFC85F0680842C4AB52ECE9B800F3EC39F0
      4EF6C6EBBF5F21F8FC56E92CF32DB33536C918EB45A10878980683B32310FEBA
      18411DF9D8728F47F661306E8B6FABCC3D47C843D3E33B189EF8199937170EC2
      AD5858E60CF68B02EC6CCB824DC15A58465AC13E62229EBF6A97DBB8FFEC2ED6
      9D9C0F87684B2C3E340EFAF106D8F3948FC979F3E05EBF1F818FB3E1591F83E1
      F186A8A9AFC1B14BFBE0CF778362CF2AC585D23565D734C1149B9A0E63F89911
      F0B9770ACC162EC69E9D8C005EA09CD9C67396E7AEACA45A2EC6A74DC2D0A4E1
      585CBE91CCC790FCDDBDA847DC9AC4ADE0C232C11AB3E3E742502690FADAF593
      3BE20AE9BF87549E549D94F7FF0709159B00
    }
  end
  object pmDebugImage: TPopupMenu
    Left = 320
    Top = 224
    object MenuItemSaveDebugImage: TMenuItem
      Caption = 'Save Debug Image As Bmp...'
      OnClick = MenuItemSaveDebugImageClick
    end
    object MenuItemCopyDebugImage: TMenuItem
      Caption = 'Copy Debug Image to Clipboard'
      OnClick = MenuItemCopyDebugImageClick
    end
    object MenuItemEraseDebugImage: TMenuItem
      Caption = 'Erase Debug Image'
      OnClick = MenuItemEraseDebugImageClick
    end
  end
  object pmSetVars: TPopupMenu
    Left = 592
    Top = 225
    object MenuItem_AddSetVar: TMenuItem
      Caption = 'Add Variable'
      OnClick = MenuItem_AddSetVarClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object MenuItem_RemoveSetVar: TMenuItem
      Caption = 'Remove Variable'
      OnClick = MenuItem_RemoveSetVarClick
    end
  end
  object tmrEditSetVars: TTimer
    Enabled = False
    Interval = 1
    OnTimer = tmrEditSetVarsTimer
    Left = 624
    Top = 225
  end
  object imglstSetVar: TImageList
    Left = 656
    Top = 224
    Bitmap = {
      4C7A010000001000000010000000630000000000000078DAFBFFFF3FC37F2A60
      51868EFF304C888F2C0E93C3661E2E7BC81523248EEE2652CC459727D57E4AF4
      E30A536C7E2064FF40605C6EC4954EF0F90B5BF811320F5FB89F5CF7FF3FA138
      21360EF0E987D903A289751BBE70C496D7B0E90700985D9E01
    }
  end
  object tmrEditCustomVars: TTimer
    Enabled = False
    Interval = 1
    OnTimer = tmrEditCustomVarsTimer
    Left = 760
    Top = 224
  end
end
