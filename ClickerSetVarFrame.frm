object frClickerSetVar: TfrClickerSetVar
  Left = 0
  Height = 269
  Top = 0
  Width = 790
  Anchors = [akTop, akLeft, akRight, akBottom]
  ClientHeight = 269
  ClientWidth = 790
  OnResize = FrameResize
  TabOrder = 0
  DesignLeft = 86
  DesignTop = 85
  object pnlHorizSplitter: TPanel
    Cursor = crHSplit
    Left = 436
    Height = 269
    Top = 0
    Width = 11
    Anchors = [akTop, akLeft, akBottom]
    Color = 13041606
    ParentColor = False
    TabOrder = 0
    OnMouseDown = pnlHorizSplitterMouseDown
    OnMouseMove = pnlHorizSplitterMouseMove
    OnMouseUp = pnlHorizSplitterMouseUp
  end
  object pnlVars: TPanel
    Left = 0
    Height = 269
    Top = 0
    Width = 436
    Anchors = [akTop, akLeft, akRight, akBottom]
    Caption = 'pnlVars'
    ClientHeight = 269
    ClientWidth = 436
    Constraints.MinWidth = 200
    TabOrder = 1
    object vstSetVar: TVirtualStringTree
      Left = 0
      Height = 207
      Top = 0
      Width = 434
      Anchors = [akTop, akLeft, akRight, akBottom]
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
      Height = 30
      Hint = 'Every time a variable, with such a value, is evaluated, an http call is made.'
      Top = 208
      Width = 346
      Anchors = [akLeft, akBottom]
      Caption = 'HTTP calls are available, as var values, using the following format:'#13#10'$http://<server:port>/[params]$'
      ParentShowHint = False
      ShowHint = True
    end
    object spdbtnMoveUp: TSpeedButton
      Left = 0
      Height = 22
      Hint = 'Move Up'
      Top = 243
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
      OnClick = spdbtnMoveUpClick
      ShowHint = True
      ParentFont = False
      ParentShowHint = False
    end
    object spdbtnMoveDown: TSpeedButton
      Left = 24
      Height = 22
      Hint = 'Move Down'
      Top = 243
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
      OnClick = spdbtnMoveDownClick
      ShowHint = True
      ParentFont = False
      ParentShowHint = False
    end
  end
  object pnlFunctions: TPanel
    Left = 448
    Height = 269
    Top = 0
    Width = 341
    Anchors = [akTop, akLeft, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'pnlFunctions'
    ClientHeight = 269
    ClientWidth = 341
    Constraints.MinWidth = 341
    TabOrder = 2
    object memAvailableFunctions: TMemo
      Left = 0
      Height = 247
      Hint = 'The "$GetSelfHandles()$" and "$Exit(<ExitCode>)$" functions do not return a value, so they must be placed in the "Variable" column of a SetVar action.'#13#10'Also, they can''t be evaluated using the console or as part of a condition.'#13#10'All the other function should be called from the "Value" column.'
      Top = 20
      Width = 335
      Anchors = [akTop, akLeft, akRight, akBottom]
      Color = clBtnFace
      Lines.Strings = (
        '$ExtractFileDir(<DirName>)$'
        '$Random(<min>, <max>)$'
        '$Random(<max>)$'
        '$Sum(<op1>, <op2>)$'
        '$Diff(<op1>, <op2>)$'
        '$Mul(<op1>, <op2>)$'
        '$Div(<op1>, <op2>)$'
        '$FMul(<op1>, <op2>)$'
        '$FDiv(<op1>, <op2>)$'
        '$EFMul(<op1>, <op2>)$'
        '$EFDiv(<op1>, <op2>)$'
        '$Abs(<op>)$'
        '$FAbs(<op>)$'
        '$EFAbs(<op>)$'
        '$PrefixWithZeros(<Number>, <TotalNumberOfDigits>)$'
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
        '$GetKeyNameFromPair(<key>=<value>)$'
        '$GetKeyValueFromPair(<key>=<value>)$'
        '$Chr(<ByteValue>)$'
        '$IncBrightness(<HexColor>[,Amount])$'
        '$DecBrightness(<HexColor>[,Amount])$'
        '$IncBrightnessR(<HexColor>[,Amount])$'
        '$IncBrightnessG(<HexColor>[,Amount])$'
        '$IncBrightnessB(<HexColor>[,Amount])$'
        '$DecBrightnessR(<HexColor>[,Amount])$'
        '$DecBrightnessG(<HexColor>[,Amount])$'
        '$DecBrightnessB(<HexColor>[,Amount])$'
        '$Current_Mouse_X$'
        '$Current_Mouse_Y$'
        '$CRLF$'
        '$#4#5$'
        '$Now$'
        '$RenderBmpExternally()$'
        '$GetActionProperties()$'
      )
      ParentShowHint = False
      ReadOnly = True
      ScrollBars = ssBoth
      ShowHint = True
      TabOrder = 0
      WordWrap = False
    end
    object lblAvailableFunctions: TLabel
      Left = 2
      Height = 20
      Hint = 'Special vars are either updated on every use, or their values can''t be changed (consts).'
      Top = 0
      Width = 335
      AutoSize = False
      Caption = 'Available functions and special vars:'
      Color = clBtnFace
      ParentShowHint = False
      ShowHint = True
      Transparent = False
    end
  end
  object pmSetVars: TPopupMenu
    Left = 560
    Top = 216
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
    Left = 640
    Top = 216
  end
  object imglstSetVar: TImageList
    Left = 728
    Top = 216
    Bitmap = {
      4C7A010000001000000010000000630000000000000078DAFBFFFF3FC37F2A60
      51868EFF304C888F2C0E93C3661E2E7BC81523248EEE2652CC459727D57E4AF4
      E30A536C7E2064FF40605C6EC4954EF0F90B5BF811320F5FB89F5CF7FF3FA138
      21360EF0E987D903A289751BBE70C496D7B0E90700985D9E01
    }
  end
end
