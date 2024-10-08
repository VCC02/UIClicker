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
    Width = 339
    Caption = 'Custom user variables before calling (set on every loop iteration)'
    ParentShowHint = False
    ShowHint = True
  end
  object vstCustomVariables: TVirtualStringTree
    Left = 3
    Height = 196
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
    OnPaintText = vstCustomVariablesPaintText
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
  object spdbtnMoveUp: TSpeedButton
    Left = 3
    Height = 22
    Hint = 'Move Up'
    Top = 217
    Width = 20
    Anchors = [akLeft, akBottom]
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
    Left = 27
    Height = 22
    Hint = 'Move Down'
    Top = 217
    Width = 20
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
    ShowHint = True
    ParentFont = False
    ParentShowHint = False
    OnClick = spdbtnMoveDownClick
  end
  object spdbtnNewVariable: TSpeedButton
    Left = 51
    Height = 22
    Hint = 'New variable'
    Top = 217
    Width = 20
    Anchors = [akLeft, akBottom]
    Font.Color = clWindowText
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC9AEFFC9
      AEFFC9AEFFC9AEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFF277FFF277FFF277FFF277FFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF83A2FE83
      A2FE83A2FE83A2FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFF277FFF277FFF277FFF83A2FEFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF277FFF27
      7FFF277FFF83A2FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC9AEFF
      277FFF83A2FE277FFF277FFF277FFF277FFF277FFF83A2FE83A2FE83A2FE83A2
      FE277FFFC9AEFFFFFFFFFFFFFFC9AEFF277FFF83A2FE277FFF277FFF277FFF27
      7FFF277FFF277FFF277FFF277FFF83A2FE277FFFC9AEFFFFFFFFFFFFFFC9AEFF
      277FFF83A2FE277FFF277FFF277FFF277FFF277FFF277FFF277FFF277FFF83A2
      FE277FFFC9AEFFFFFFFFFFFFFFC9AEFF277FFF9D99F79D99F79D99F79D99F727
      7FFF277FFF277FFF277FFF277FFF83A2FE277FFFC9AEFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFF9D99F7277FFF277FFF83A2FEFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9D99F727
      7FFF277FFF83A2FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFF9D99F783A2FE83A2FE83A2FEFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF277FFF27
      7FFF277FFF277FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFC9AEFFC9AEFFC9AEFFC9AEFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    }
    ShowHint = True
    ParentFont = False
    ParentShowHint = False
    OnClick = spdbtnNewVariableClick
  end
  object spdbtnRemoveSelectedVariable: TSpeedButton
    Left = 75
    Height = 22
    Hint = 'Remove selected variable'
    Top = 217
    Width = 20
    Anchors = [akLeft, akBottom]
    Font.Color = clWindowText
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAE8DFF
      1561FF6C82FF1561FF1561FF1561FF1561FF1561FF6C82FF6C82FF6C82FF6C82
      FF1561FFAE8DFFFFFFFFFFFFFFAE8DFF1561FF6C82FF1561FF1561FF1561FF15
      61FF1561FF1561FF1561FF1561FF6C82FF1561FFAE8DFFFFFFFFFFFFFFAE8DFF
      1561FF6C82FF1561FF1561FF1561FF1561FF1561FF1561FF1561FF1561FF6C82
      FF1561FFAE8DFFFFFFFFFFFFFFAE8DFF1561FF847AFF847AFF847AFF847AFF15
      61FF1561FF1561FF1561FF1561FF6C82FF1561FFAE8DFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    }
    ShowHint = True
    ParentFont = False
    ParentShowHint = False
    OnClick = spdbtnRemoveSelectedVariableClick
  end
  object lblSetVarWarning: TLabel
    Left = 104
    Height = 15
    Hint = 'Because of a limitation of saving these values, please do not use plain #4#5 separators.'#13#10'Use $#4#5$ replacement instead.'
    Top = 217
    Width = 326
    Anchors = [akLeft, akBottom]
    Caption = 'Do not use plain ASCII #4#5 characters in var names or values!'
    Font.Color = clMaroon
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object pmCustomVars: TPopupMenu
    Left = 284
    Top = 188
    object AddCustomVarRow1: TMenuItem
      Caption = 'Add Variable'
      Bitmap.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC9AEFFC9
        AEFFC9AEFFC9AEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF277FFF277FFF277FFF277FFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF83A2FE83
        A2FE83A2FE83A2FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF277FFF277FFF277FFF83A2FEFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF277FFF27
        7FFF277FFF83A2FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC9AEFF
        277FFF83A2FE277FFF277FFF277FFF277FFF277FFF83A2FE83A2FE83A2FE83A2
        FE277FFFC9AEFFFFFFFFFFFFFFC9AEFF277FFF83A2FE277FFF277FFF277FFF27
        7FFF277FFF277FFF277FFF277FFF83A2FE277FFFC9AEFFFFFFFFFFFFFFC9AEFF
        277FFF83A2FE277FFF277FFF277FFF277FFF277FFF277FFF277FFF277FFF83A2
        FE277FFFC9AEFFFFFFFFFFFFFFC9AEFF277FFF9D99F79D99F79D99F79D99F727
        7FFF277FFF277FFF277FFF277FFF83A2FE277FFFC9AEFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF9D99F7277FFF277FFF83A2FEFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9D99F727
        7FFF277FFF83A2FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF9D99F783A2FE83A2FE83A2FEFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF277FFF27
        7FFF277FFF277FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFC9AEFFC9AEFFC9AEFFC9AEFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      }
      OnClick = AddCustomVarRow1Click
    end
    object RemoveCustomVarRow1: TMenuItem
      Caption = 'Remove Variable...'
      Bitmap.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAE8DFF
        1561FF6C82FF1561FF1561FF1561FF1561FF1561FF6C82FF6C82FF6C82FF6C82
        FF1561FFAE8DFFFFFFFFFFFFFFAE8DFF1561FF6C82FF1561FF1561FF1561FF15
        61FF1561FF1561FF1561FF1561FF6C82FF1561FFAE8DFFFFFFFFFFFFFFAE8DFF
        1561FF6C82FF1561FF1561FF1561FF1561FF1561FF1561FF1561FF1561FF6C82
        FF1561FFAE8DFFFFFFFFFFFFFFAE8DFF1561FF847AFF847AFF847AFF847AFF15
        61FF1561FF1561FF1561FF1561FF6C82FF1561FFAE8DFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      }
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
