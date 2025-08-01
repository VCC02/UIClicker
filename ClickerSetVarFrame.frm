object frClickerSetVar: TfrClickerSetVar
  Left = 0
  Height = 269
  Top = 0
  Width = 790
  Anchors = [akTop, akLeft, akRight, akBottom]
  ClientHeight = 269
  ClientWidth = 790
  TabOrder = 0
  OnResize = FrameResize
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
    ParentBackground = False
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
      OnCreateEditor = vstSetVarCreateEditor
      OnDblClick = vstSetVarDblClick
      OnEdited = vstSetVarEdited
      OnEditing = vstSetVarEditing
      OnGetText = vstSetVarGetText
      OnPaintText = vstSetVarPaintText
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
      ShowHint = True
      ParentFont = False
      ParentShowHint = False
      OnClick = spdbtnMoveUpClick
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
      ShowHint = True
      ParentFont = False
      ParentShowHint = False
      OnClick = spdbtnMoveDownClick
    end
    object lblSetVarWarning: TLabel
      Left = 104
      Height = 15
      Hint = 'Because of a limitation of saving these values, please do not use plain #4#5 separators.'#13#10'Use $#4#5$ replacement instead.'
      Top = 247
      Width = 326
      Anchors = [akLeft, akBottom]
      Caption = 'Do not use plain ASCII #4#5 characters in var names or values!'
      Font.Color = clMaroon
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object spdbtnNewVariable: TSpeedButton
      Left = 48
      Height = 22
      Hint = 'New variable'
      Top = 243
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
      Left = 72
      Height = 22
      Hint = 'Remove selected variable'
      Top = 243
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
      Hint = 'The "$GetSelfHandles()$" and "$Exit(<ExitCode>)$" functions do not return a value, so they must be placed in the "Variable" column of a SetVar action.'#13#10'Also, they can''t be evaluated using the console or as part of a condition.'#13#10'All the other function should be called from the "Value" column.'#13#10'Function and var names are case sensitive.'
      Top = 20
      Width = 335
      Anchors = [akTop, akLeft, akRight, akBottom]
      Color = clBtnFace
      Lines.Strings = (
        '$Random(<min>, <max>)$'
        '$Random(<max>)$'
        '$Sum(<arg1>, <arg2>)$'
        '$Diff(<arg1>, <arg2>)$'
        '$Mul(<arg1>, <arg2>)$'
        '$Div(<arg1>, <arg2>)$'
        '$FMul(<arg1>, <arg2>)$'
        '$FDiv(<arg1>, <arg2>)$'
        '$EFMul(<arg1>, <arg2>)$'
        '$EFDiv(<arg1>, <arg2>)$'
        '$Abs(<arg>)$'
        '$FAbs(<arg>)$'
        '$EFAbs(<arg>)$'
        '$PrefixWithZeros(<Number>, <TotalNumberOfDigits>)$'
        '$http://<server:port>/[params]$'
        '$FastReplace_45ToReturn(<some_string>)$'
        '$FastReplace_ReturnTo45(<some_string>)$'
        '$FastReplace_45To87(<some_string>)$'
        '$FastReplace_87To45(<some_string>)$'
        '$FastReplace_45To68(<some_string>)$'
        '$FastReplace_68To45(<some_string>)$'
        '$StringReplace(<StringToBeSearchedFrom>~^~<OldSubString>~^~<NewSubString>)$'
        '$Exit(<ExitCode>)$'
        '$CreateDir(<PathToNewDir>)$'
        '$LoadTextFile(<PathToTextFile>)$'
        '$ItemCount($TextFileContent$)$'
        '$GetTextItem($TextFileContent$,<ItemIndex>)$'
        '$IndexOfTextItem($TextFileContent$,<Item>)$'
        '$Str0(<some_string>,<CharacterIndex>)$'
        '$Str1(<some_string>,<CharacterIndex>)$'
        '$StrLen(<some_string>)$'
        '$StringContains(<SubString>, <String>)$'
        '$ExtractFileDir(<PathToFile>)$'
        '$ExtractFilePath(<PathToFile>)$'
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
        '$Current_Mouse_hCursor$'
        '$CRLF$'
        '$#4#5$'
        '$#6#8$'
        '$Now$'
        '$RenderBmpExternally()$'
        '$GetActionProperties()$'
        '$GetImageDimensions()$'
        '$GetExternallyRenderedImageDimensions()$'
        '$FullHistogramDisk()$'
        '$FullHistogramExternallyRendered()$'
        '$PartialHistogramDisk()$'
        '$PartialHistogramExternallyRendered()$'
        '$GetWindowLongPtr(<ControlHandle>, <Index>)$'
        '$GetWindowProcessId(<ControlHandle>)$'
        '$GenerateAndSaveTree(<TreePath>[,<Step>[,<UseMouseSwipe>]])$'
        '$SetWinInterpOption(<WinInterpOptionName>, <WinInterpOptionValue>)$'
        '$Console([<Value>])$'
        '$GetListOfFonts()$'
        '$UpperCase(<some_string>)$'
        '$LowerCase(<some_string>)$'
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
      ParentColor = False
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
      OnClick = MenuItem_AddSetVarClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object MenuItem_RemoveSetVar: TMenuItem
      Caption = 'Remove Variable'
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
  object pmVarsEditor: TPopupMenu
    Left = 301
    Top = 88
    object MenuItem_BrowseFile: TMenuItem
      Caption = 'Browse...'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000B0E4EFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB0E4EFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1DE6
        B5FF1DE6B5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1DE6B5FF1DE6B5FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF1DE6B5FF1DE6B5FFFFFFFFFF1DE6B5FF0047
        B3FF0047B3FF1DE6B5FFFFFFFFFFFFFFFFFF1DE6B5FF0047B3FF0047B3FF1DE6
        B5FFFFFFFFFFFFFFFFFF1DE6B5FF0047B3FF0047B3FF1DE6B5FF0047B3FF0047
        B3FF0047B3FF0047B3FF1DE6B5FF1DE6B5FF0047B3FF0047B3FF0047B3FF0047
        B3FF1DE6B5FF1DE6B5FF0047B3FF0047B3FF0047B3FF0047B3FF0047B3FF0047
        B3FF0047B3FF0047B3FF1DE6B5FF1DE6B5FF0047B3FF0047B3FF0047B3FF0047
        B3FF1DE6B5FF1DE6B5FF0047B3FF0047B3FF0047B3FF0047B3FF1DE6B5FF0047
        B3FF0047B3FF1DE6B5FFFFFFFFFFFFFFFFFF1DE6B5FF0047B3FF0047B3FF1DE6
        B5FFFFFFFFFFFFFFFFFF1DE6B5FF0047B3FF0047B3FF1DE6B5FFFFFFFFFF1DE6
        B5FF1DE6B5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1DE6B5FF1DE6B5FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF1DE6B5FF1DE6B5FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB0E4EFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB0E4EFFF
      }
      OnClick = MenuItem_BrowseFileClick
    end
    object MenuItem_ReplaceWithAppDir: TMenuItem
      Caption = 'Replace with $AppDir$'
      OnClick = MenuItem_ReplaceWithAppDirClick
    end
    object MenuItem_ReplaceWithTemplateDir: TMenuItem
      Caption = 'Replace with $TemplateDir$'
      OnClick = MenuItem_ReplaceWithTemplateDirClick
    end
    object MenuItem_ReplaceWithSelfTemplateDir: TMenuItem
      Caption = 'Replace with $SelfTemplateDir$'
      OnClick = MenuItem_ReplaceWithSelfTemplateDirClick
    end
  end
  object tmrCreateEditor: TTimer
    Enabled = False
    Interval = 1
    OnTimer = tmrCreateEditorTimer
    Left = 326
    Top = 38
  end
  object tmrSetEditorTextColor: TTimer
    Enabled = False
    Interval = 200
    OnTimer = tmrSetEditorTextColorTimer
    Left = 326
    Top = 143
  end
  object pnSetVarFormat: TPopupNotifier
    Color = 10813439
    Icon.Data = {
      1754506F727461626C654E6574776F726B47726170686963E800000089504E47
      0D0A1A0A0000000D49484452000000180000001808020000006F15AAAF000000
      0674524E5300FF00FF00FF37581B7D0000009D4944415438CBED94CD0D80200C
      855B76D03D5CC129DCC44417D065F4EE063A8A4B3C0F900A4513438817ED8DD2
      EFF58706064039CC5026FB85DE175AFA268D3F410000062A9064023280914BC9
      D0627F5E8E0F929574C2D3EA972657E2F48315482A5447DC8BAAA39176823AC3
      FAEF7AF74143F326DE586BE4B2C57E3DB810E4818A389BE5D5442D10FB1D98EB
      F9DD42565D9DB69002F2FFB17D59E800AC841FE0E9D208310000000049454E44
      AE426082
    }
    Text = '$<varname>$'
    TextFont.Color = 185
    TextFont.Style = [fsBold]
    Title = 'Variables must have the following format:'
    TitleFont.Style = [fsBold]
    Visible = False
    Left = 144
    Top = 128
  end
end
