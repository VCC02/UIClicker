object frClickerSetVar: TfrClickerSetVar
  Left = 0
  Height = 269
  Top = 0
  Width = 790
  Anchors = [akTop, akLeft, akRight, akBottom]
  ClientHeight = 269
  ClientWidth = 790
  TabOrder = 0
  DesignLeft = 86
  DesignTop = 85
  object vstSetVar: TVirtualStringTree
    Left = 0
    Height = 200
    Top = 8
    Width = 464
    Anchors = [akTop, akLeft, akBottom]
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
  object memAvailableFunctions: TMemo
    Left = 472
    Height = 184
    Hint = 'The "$GetSelfHandles()$" and "$Exit(<ExitCode>)$" functions do not return a value, so they must be placed in the "Variable" column of a SetVar action.'#13#10'Also, they can''t be evaluated using the console or as part of a condition.'#13#10'All the other function should be called from the "Value" column.'
    Top = 24
    Width = 280
    Anchors = [akTop, akLeft, akBottom]
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
  object lblAvailableFunctions: TLabel
    Left = 472
    Height = 15
    Hint = 'Special vars are either updated on every use, or their values can''t be changed (consts).'
    Top = 8
    Width = 190
    Caption = 'Available functions and special vars:'
    ParentShowHint = False
    ShowHint = True
  end
  object lblSetVarToHttpInfo: TLabel
    Left = 0
    Height = 15
    Hint = 'Every time a variable, with such a value, is evaluated, an http call is made.'
    Top = 216
    Width = 520
    Anchors = [akLeft, akBottom]
    Caption = 'HTTP calls are available, as var values, using the following format: $http://<server:port>/[params]$'
    ParentShowHint = False
    ShowHint = True
  end
  object pmSetVars: TPopupMenu
    Left = 620
    Top = 241
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
    Left = 652
    Top = 241
  end
  object imglstSetVar: TImageList
    Left = 684
    Top = 241
    Bitmap = {
      4C7A010000001000000010000000630000000000000078DAFBFFFF3FC37F2A60
      51868EFF304C888F2C0E93C3661E2E7BC81523248EEE2652CC459727D57E4AF4
      E30A536C7E2064FF40605C6EC4954EF0F90B5BF811320F5FB89F5CF7FF3FA138
      21360EF0E987D903A289751BBE70C496D7B0E90700985D9E01
    }
  end
end
