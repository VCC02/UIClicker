object frClickerCallTemplate: TfrClickerCallTemplate
  Left = 0
  Height = 240
  Top = 0
  Width = 629
  ClientHeight = 240
  ClientWidth = 629
  TabOrder = 0
  DesignLeft = 86
  DesignTop = 85
  object lblCustomUserVarsBeforeCall: TLabel
    Left = 3
    Height = 15
    Hint = 'Right-click to add/remove. Double-click to edit.'
    Top = 0
    Width = 191
    Caption = 'Custom user variables before calling'
    ParentShowHint = False
    ShowHint = True
  end
  object vstCustomVariables: TVirtualStringTree
    Left = 3
    Height = 212
    Hint = 'These variables are set on every loop iteration.'
    Top = 20
    Width = 621
    Anchors = [akTop, akLeft, akRight, akBottom]
    Colors.UnfocusedColor = clMedGray
    Colors.UnfocusedSelectionColor = clGradientInactiveCaption
    DefaultNodeHeight = 21
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
    TabOrder = 0
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
    TabOrder = 1
    Visible = False
    DisplayOptions = [doColumnTitles, doKeyColFixed]
    KeyOptions = [keyEdit, keyUnique]
    TitleCaptions.Strings = (
      'Variable'
      'Value'
    )
    ColWidths = (
      180
      140
    )
  end
  object pmCustomVars: TPopupMenu
    Left = 284
    Top = 188
    object AddCustomVarRow1: TMenuItem
      Caption = 'Add Variable'
      OnClick = AddCustomVarRow1Click
    end
    object RemoveCustomVarRow1: TMenuItem
      Caption = 'Remove Variable...'
      OnClick = RemoveCustomVarRow1Click
    end
  end
  object tmrEditCustomVars: TTimer
    Enabled = False
    Interval = 1
    OnTimer = tmrEditCustomVarsTimer
    Left = 504
    Top = 184
  end
end
