object frClickerConditionEditor: TfrClickerConditionEditor
  Left = 0
  Height = 264
  Top = 0
  Width = 748
  ClientHeight = 264
  ClientWidth = 748
  TabOrder = 0
  DesignLeft = 86
  DesignTop = 85
  object pnlvstActionConditions: TPanel
    Left = 3
    Height = 230
    Top = 3
    Width = 730
    Anchors = [akTop, akLeft, akRight, akBottom]
    Caption = 'pnlvstActionConditions'
    ParentColor = False
    TabOrder = 0
    Visible = False
  end
  object lblLastActionStatusValidValues: TLabel
    Left = 3
    Height = 15
    Top = 241
    Width = 173
    Anchors = [akLeft, akBottom]
    Caption = '$LastAction_Status$ Valid values:'
  end
  object spdbtnAddOR: TSpeedButton
    Left = 616
    Height = 22
    Top = 241
    Width = 55
    Anchors = [akRight, akBottom]
    Caption = 'Add OR'
    OnClick = spdbtnAddORClick
  end
  object spdbtnAddAND: TSpeedButton
    Left = 678
    Height = 22
    Top = 241
    Width = 55
    Anchors = [akRight, akBottom]
    Caption = 'Add AND'
    OnClick = spdbtnAddANDClick
  end
  object pmActionConditionsEval: TPopupMenu
    OnPopup = pmActionConditionsEvalPopup
    Left = 688
    Top = 152
    object MenuItemNotEqual: TMenuItem
      Caption = 'String Not Equal'
      RadioItem = True
      OnClick = MenuItemNotEqualClick
    end
    object MenuItemEqual: TMenuItem
      Caption = 'String Equal'
      RadioItem = True
      OnClick = MenuItemEqualClick
    end
    object MenuItemLessThan: TMenuItem
      Caption = 'String Less Than'
      RadioItem = True
      OnClick = MenuItemLessThanClick
    end
    object MenuItemGreaterThan: TMenuItem
      Caption = 'String Greater Than'
      RadioItem = True
      OnClick = MenuItemGreaterThanClick
    end
    object MenuItemLessThanOrEqual: TMenuItem
      Caption = 'String Less Than Or Equal'
      RadioItem = True
      OnClick = MenuItemLessThanOrEqualClick
    end
    object MenuItemGreaterThanOrEqual: TMenuItem
      Caption = 'String Greater Than Or Equal'
      RadioItem = True
      OnClick = MenuItemGreaterThanOrEqualClick
    end
    object MenuItemIntNotEqual: TMenuItem
      Caption = 'Integer Not Equal'
      RadioItem = True
      OnClick = MenuItemIntNotEqualClick
    end
    object MenuItemIntEqual: TMenuItem
      Caption = 'Integer Equal'
      RadioItem = True
      OnClick = MenuItemIntEqualClick
    end
    object MenuItemIntLessThan: TMenuItem
      Caption = 'Integer Less Than'
      RadioItem = True
      OnClick = MenuItemIntLessThanClick
    end
    object MenuItemIntGreaterThan: TMenuItem
      Caption = 'Integer Greater Than'
      RadioItem = True
      OnClick = MenuItemIntGreaterThanClick
    end
    object MenuItemIntLessThanOrEqual: TMenuItem
      Caption = 'Integer Less Than Or Equal'
      RadioItem = True
      OnClick = MenuItemIntLessThanOrEqualClick
    end
    object MenuItemIntGreaterThanOrEqual: TMenuItem
      Caption = 'Integer Greater Than Or Equal'
      RadioItem = True
      OnClick = MenuItemIntGreaterThanOrEqualClick
    end
    object MenuItemExtNotEqual: TMenuItem
      Caption = 'Extended Not Equal'
      RadioItem = True
      OnClick = MenuItemExtNotEqualClick
    end
    object MenuItemExtEqual: TMenuItem
      Caption = 'Extended Equal'
      RadioItem = True
      OnClick = MenuItemExtEqualClick
    end
    object MenuItemExtLessThan: TMenuItem
      Caption = 'Extended Less Than'
      RadioItem = True
      OnClick = MenuItemExtLessThanClick
    end
    object MenuItemExtGreaterThan: TMenuItem
      Caption = 'Extended Greater Than'
      RadioItem = True
      OnClick = MenuItemExtGreaterThanClick
    end
    object MenuItemExtLessThanOrEqual: TMenuItem
      Caption = 'Extended Less Than Or Equal'
      RadioItem = True
      OnClick = MenuItemExtLessThanOrEqualClick
    end
    object MenuItemExtGreaterThanOrEqual: TMenuItem
      Caption = 'Extended Greater Than Or Equal'
      RadioItem = True
      OnClick = MenuItemExtGreaterThanOrEqualClick
    end
  end
  object pmActionConditions: TPopupMenu
    Left = 560
    Top = 152
    object MenuItemRemoveExpressionPart: TMenuItem
      Caption = 'Remove Expression Part (group of columns)...'
      OnClick = MenuItemRemoveExpressionPartClick
    end
    object MenuItemRemoveTerm: TMenuItem
      Caption = 'Remove term (row)...'
      OnClick = MenuItemRemoveTermClick
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object MenuItem_AddLastActionStatusEqualsAllowedFailed: TMenuItem
      Caption = 'Add $LastAction_Status$ == "Allowed Failed"'
      OnClick = MenuItem_AddLastActionStatusEqualsAllowedFailedClick
    end
    object MenuItem_AddLastActionStatusEqualsSuccessful: TMenuItem
      Caption = 'Add $LastAction_Status$ == "Successful"'
      OnClick = MenuItem_AddLastActionStatusEqualsSuccessfulClick
    end
    object MenuItem_AddPrefixWithZerosEqualsNumber: TMenuItem
      Caption = 'Add $PrefixWithZeros($SomeNumber$,6)$ == 001234'
      OnClick = MenuItem_AddPrefixWithZerosEqualsNumberClick
    end
    object MenuItemAddStrLenMyVar: TMenuItem
      Caption = 'Add $StrLen($MyVar$)$ == 0'
      OnClick = MenuItemAddStrLenMyVarClick
    end
  end
  object tmrEditingCondition: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrEditingConditionTimer
    Left = 232
    Top = 152
  end
  object imglstComparisonOperators: TImageList
    Left = 408
    Top = 152
    Bitmap = {
      4C7A120000001000000010000000EA0100000000000078DAED58C16DC4300C6B
      87E8029DA73BE5DB1DBA446E86ACD27F47B8420F0342E0F86232711C8B0284BC
      A8388C44C29A7FFF9E68CC00765996E7D7E3137A5FC2DAB325D6629AA622D66A
      6FA561FD33F7FD567B2BD3B97DF6C657EECC89B35C96F8339CE72A97BDF508DB
      5F287626E7776E3CFFA346A957F7F46F69FEF7CCCFD5C19E9FE52F6ADC6DFED1
      DEF5BE82E26AB077C1ADE787D52F565F22D4617D62D4F93F821BF55F9B3A8CFE
      8EAAA38CFE9E31DBF27FF9BFFC3F86FFA377A73483C8DD49FE2FFF978EF6E3FF
      8806F819ACD580ABFCFFE3ED1BE6C7B0E87E16C55ABCDAFF5BEDAD34AC7FE6BE
      BFF4AFD3B991FD7F2BBE7267AED9FFAFF9F25CE5B2B71E61FB0BC56AFFDF4794
      7A754FFF46DFFFB3FC458D68F7FFDA7FEF7135D8BBE0D6F3C3EA17AB2F11EAB0
      3E31EAFC1FC18DFAAF4D1D467F47D551467FCF986DF9BFFC5FFE1FC3FFD9FD3F
      727792FFCBFFA5A3FDF83FBBFFAFD580ABFCFFFDE701F36358743F8B622D5EED
      FFADF6561AD63F73DF5FFAD7E9DCC8FEBF155FB933D7ECFFD77C79AE72D95B8F
      B0FD8562B5FFEF234ABDBAA77FA3EFFF59FEA246B4FB7FEDBFF7B81AEC5D70EB
      F961F58BD5970875589F1875FE8FE046FDD7A60EA3BFA3EA28A3BF67CCB6FC5F
      FE2FFF8FE1FFECFE1FB93BC9FFE5FFD2D17EFC9FDDFFD76A8061FF013FC55239
    }
  end
end
