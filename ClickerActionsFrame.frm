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
  OnResize = FrameResize
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
    Images = dmClickerIcons.imglstActionExecution
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
      object pnlvstOI: TPanel
        Left = 0
        Height = 256
        Top = 0
        Width = 440
        Anchors = [akTop, akLeft, akBottom]
        BevelOuter = bvNone
        Caption = 'pnlvstOI'
        ClientHeight = 256
        ClientWidth = 440
        Color = 13500339
        Constraints.MinHeight = 208
        Constraints.MinWidth = 350
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        ParentBackground = False
        ParentColor = False
        ParentFont = False
        TabOrder = 1
        Visible = False
        object imgFontColorBuffer: TImage
          Left = 383
          Height = 16
          Top = 204
          Width = 16
          AutoSize = True
          Picture.Data = {
            07544269746D617036030000424D360300000000000036000000280000001000
            0000100000000100180000000000000300000000000000000000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFF
          }
          Visible = False
        end
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
        ParentFont = False
      end
      object prbTimeout: TProgressBar
        Left = 2
        Height = 8
        Top = 258
        Width = 137
        Anchors = [akLeft, akBottom]
        Smooth = True
        TabOrder = 0
      end
      object pnlExtra: TPanel
        Left = 452
        Height = 266
        Top = 0
        Width = 570
        Anchors = [akTop, akRight, akBottom]
        ClientHeight = 266
        ClientWidth = 570
        ParentColor = False
        TabOrder = 2
        object pnlCover: TPanel
          Left = 400
          Height = 50
          Top = 152
          Width = 170
          TabOrder = 0
          Visible = False
        end
      end
      object pnlHorizSplitter: TPanel
        Cursor = crHSplit
        Left = 441
        Height = 266
        Top = 0
        Width = 10
        Anchors = [akTop, akLeft, akBottom]
        Caption = 'ActionEditorSplitter'
        Color = 13041606
        Font.Color = 13041606
        Font.Height = -11
        Font.Name = 'Tahoma'
        ParentBackground = False
        ParentColor = False
        ParentFont = False
        TabOrder = 3
        OnMouseDown = pnlHorizSplitterMouseDown
        OnMouseMove = pnlHorizSplitterMouseMove
        OnMouseUp = pnlHorizSplitterMouseUp
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
        Width = 1042
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
      object pnlVars: TPanel
        Left = 0
        Height = 268
        Top = 0
        Width = 336
        Anchors = [akTop, akLeft, akBottom]
        BevelOuter = bvNone
        ClientHeight = 268
        ClientWidth = 336
        Constraints.MinHeight = 208
        Constraints.MinWidth = 336
        TabOrder = 0
        object lblVarReplacements: TLabel
          Left = 3
          Height = 13
          Top = 0
          Width = 120
          Caption = 'Variables / Replacements'
        end
        object vstVariables: TVirtualStringTree
          Left = 3
          Height = 253
          Hint = 'These variables can be manually edited in place for every action. They are reset when playing all actions. Right-click for options.'
          Top = 18
          Width = 331
          Anchors = [akTop, akLeft, akRight, akBottom]
          Colors.UnfocusedSelectionColor = clGradientInactiveCaption
          DefaultText = 'Node'
          EditDelay = 10
          Header.AutoSizeIndex = 0
          Header.Columns = <          
            item
              MinWidth = 130
              Position = 0
              Text = 'Variable'
              Width = 130
            end          
            item
              MinWidth = 150
              Position = 1
              Text = 'Value'
              Width = 150
            end>
          Header.Height = 21
          Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
          Header.Style = hsFlatButtons
          ParentShowHint = False
          PopupMenu = pmDebugVars
          ShowHint = True
          TabOrder = 0
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toDisableAutoscrollOnFocus, toAutoChangeScale, toDisableAutoscrollOnEdit]
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
          TreeOptions.SelectionOptions = [toFullRowSelect]
          OnCreateEditor = vstVariablesCreateEditor
          OnEdited = vstVariablesEdited
          OnEditing = vstVariablesEditing
          OnGetText = vstVariablesGetText
          OnMouseDown = vstVariablesMouseDown
          OnNewText = vstVariablesNewText
        end
        object chkDecodeVariables: TCheckBox
          Left = 160
          Height = 17
          Hint = 'When checked, all variables, containing ASCII4 ASCII5 (#4#5) characters, are decoded as list of values and appear as subvariables.'#13#10'When a subvariable, with a numeric value, is selected, multiple selection lines appear on the resulted bitmap.'#13#10'The following variables are decoded for selecting an area on the resulted bitmap:'#13#10#13#10'$AllControl_XOffsets$, $AllControl_YOffsets$, $AllControl_Lefts$, $AllControl_Tops$ etc'#13#10'$DecodedWindows_XOffset$, $DecodedWindows_YOffset$, $DecodedWindows_Control_Lefts$, $DecodedWindows_Control_Tops$ etc'#13#10'$DecodedWindows_XOffset_WE$, $DecodedWindows_YOffset_WE$, $DecodedWindows_Control_Lefts_WE$, $DecodedWindows_Control_Tops_WE$ etc'
          Top = 0
          Width = 100
          Caption = 'Decode variables'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnChange = chkDecodeVariablesChange
        end
      end
      object pnlResults: TPanel
        Left = 347
        Height = 268
        Top = 0
        Width = 677
        Anchors = [akTop, akLeft, akRight, akBottom]
        Caption = 'pnlResults'
        ClientHeight = 268
        ClientWidth = 677
        TabOrder = 1
        object lblBitmaps: TLabel
          Left = 2
          Height = 13
          Top = 0
          Width = 37
          Caption = 'Bitmaps'
        end
        object chkShowDebugGrid: TCheckBox
          Left = 117
          Height = 17
          Top = 0
          Width = 65
          Caption = 'Show grid'
          TabOrder = 0
          OnClick = chkShowDebugGridClick
        end
        object lblDebugBitmapXMouseOffset: TLabel
          Left = 205
          Height = 13
          Top = 0
          Width = 27
          Caption = 'mx: 0'
        end
        object lblDebugBitmapYMouseOffset: TLabel
          Left = 274
          Height = 13
          Top = 0
          Width = 27
          Caption = 'my: 0'
        end
        object lblMouseOnExecDbgImgBB: TLabel
          Left = 337
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
          Left = 353
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
          Left = 369
          Height = 13
          Top = 0
          Width = 14
          Caption = 'RR'
          Font.Color = 187
          Font.Height = -11
          Font.Name = 'Tahoma'
          ParentFont = False
        end
        object scrboxDebugBmp: TScrollBox
          Left = 2
          Height = 246
          Top = 18
          Width = 672
          HorzScrollBar.Page = 87
          HorzScrollBar.Smooth = True
          HorzScrollBar.Tracking = True
          VertScrollBar.Page = 86
          VertScrollBar.Smooth = True
          VertScrollBar.Tracking = True
          Anchors = [akTop, akLeft, akRight, akBottom]
          ClientHeight = 242
          ClientWidth = 668
          Color = clWindow
          ParentBackground = False
          ParentColor = False
          PopupMenu = pmDebugImage
          TabOrder = 1
          OnMouseWheel = scrboxDebugBmpMouseWheel
          object imgDebugBmp: TImage
            Left = 0
            Height = 2
            Top = 0
            Width = 2
            AutoSize = True
            Picture.Data = {
              07544269746D617046000000424D460000000000000036000000280000000200
              0000020000000100180000000000100000000000000000000000000000000000
              0000FFFFFFFFFFFF0000FFFFFFFFFFFF0000
            }
            PopupMenu = pmDebugImage
            OnMouseEnter = imgDebugBmpMouseEnter
            OnMouseLeave = imgDebugBmpMouseLeave
            OnMouseMove = imgDebugBmpMouseMove
          end
          object imgDebugGrid: TImage
            Left = 14
            Height = 73
            Top = 13
            Width = 73
            ParentShowHint = False
            PopupMenu = pmDebugImage
            ShowHint = True
            Transparent = True
            OnMouseMove = imgDebugBmpMouseMove
          end
        end
        object imgPluginFileName: TImage
          Left = 469
          Height = 16
          Top = 1
          Width = 16
          AutoSize = True
          Picture.Data = {
            1754506F727461626C654E6574776F726B477261706869636A02000089504E47
            0D0A1A0A0000000D49484452000000100000001008060000001FF3FF61000000
            017352474200AECE1CE90000000467414D410000B18F0BFC6105000000097048
            597300000EC200000EC20115284A80000001FF49444154384F95934D48545118
            869F73E7E630338C9576A73F28CBC0E80721A87641429B284B8836ED5AB5885C
            B82A68936BA18D42B3168916415990B83068959B821224534A2BE3CE64E3FCD9
            FD3D9D3B5E99994B903E67713ECE3DEF7BBEEF3BF708A9E87C7E99CD30DF3B1E
            460D0406879F5D0AA6FF323C6584511D2DF4D934D16CB76C1094D168F24F03D7
            7779B53CC6E8D21066F9875A91EB1F1423AF330CB4BEAD9B04756CF4C0574389
            E5DCCA8CBC5A32E4453B2EB30B0FD49A233D351AD9D03465F065F51313F93166
            ED69927903E96A587E95C95F8F99CA3DA56A55C29D75F470AE315D98E451B21F
            2DAD6A6BD71031C18B834308754EBA98A16BF50C092389102254447B60839717
            B8A69ACB3E9EEBE3C724FE360F5F97D89E45D1FA1D6E5EA7C920EDECE2B637CC
            1D2B8BF81EC73795B8A84C1C495514B9EFF471B3D2CD6CE17DA888189C304ED3
            D3D1C791D449C8EB48958D970BB252A7AB5EE4DAE728A54D7EAE2D858A480FDE
            7C7EC993998768719DB6E421F6D2C187D204FE0E87946670DDB90B2997B3077A
            D4EE6C4DD39481A69A6356BEB2BCB2C0ADA383DCEB1E617FE538B2A0AEBB2C38
            BFEF0A373AFB49B4A44245C4C0753D1C5BC3B5041F17DF515A2B627ECBF16751
            C356FF93BA76841A8D349570E1D835BA32A76A712ADECAF6441B83E74609347A
            AC85DD3BF7345D61C0969E7394F9DE71FE020B312744F57A85D3000000004945
            4E44AE426082
          }
          Visible = False
        end
        object imgPlugin: TImage
          Left = 497
          Height = 16
          Top = 1
          Width = 16
          AutoSize = True
          Picture.Data = {
            1754506F727461626C654E6574776F726B477261706869637F02000089504E47
            0D0A1A0A0000000D494844520000001000000010080200000090916836000000
            0674524E5300D700E200D297C813EE000000097048597300000EC400000EC401
            952B0E1B0000021F4944415428CF6D923D68130114C7FFEF2EB9DC07B9A6C556
            5A6D0ACD50B141510BC5497030A0E8A288195CBB08AE5671D44974F063725041
            100411A78A38B83828AD435D6C40DA7CB5B6258DB94B2FB9DCDD7B0ED69A427E
            E3E3FD788FF7FE2422F8C797CD8F6F1A8F3AF0A7A3F3F9896BE8897431BB7CF1
            2CEBE744BF50186E6C37A4174AB7DCDF1E41A0B2489F376C9B76CF01B15FCDCA
            DCFACB66AC4EA0AD709D1D9593613CA13D29DE240040263A911BBFF45FA8F96B
            73F4B4965A510CA234404440F5D0C22ABE118880A9C57C0E5D8208C413D68040
            C80434500C0A2010021148207B5602439A50DB06A96A34D8245D608235508C00
            F9DBBDE214EAE146C6CCDA7A4A8982C8A8EE9FD1EFCFA65E1895416E821DF0B6
            709B594420CBDE8F3B41FE2EF2AF6B0F5B1D8F5A6D4F2046C20430F3E17461E8
            13198046644249104C51620405044AD78FDCC22B65D35D7B3BFFDC0F7C3FF037
            1A5576C12EA429EC805D610758B23984402C77D05293B458FA7AFBFDE5281245
            D11A7DE5D144D64A5A45FADEB2EBA413E9985EBB32D93FBD11567243F9ECC8B1
            1800306A9D22541C8DCEDC38F9D8B6EC67F3F7DEB90F94402484CAEAD5ECF5AE
            2BED240488D01F1F3A3030A6C5B58356065B1002A940B4E7D35DD110949DA5D5
            AD52BBD32AFD5E6247C4053B90606F3474D51C37A70694310012D0C2CFCF93A3
            C7ABABE5093A051F68517ADFE16E8182B0E3780EF3CE604333B4B8EE7A0D16DE
            AD58467257F803F1921BCBD9889FA80000000049454E44AE426082
          }
          Visible = False
        end
        object lblMouseOnExecDbgImg: TLabel
          Left = 398
          Height = 13
          Top = 0
          Width = 66
          AutoSize = False
          Color = 12247438
          Font.Color = 187
          Font.Height = -11
          Font.Name = 'Tahoma'
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
      end
      object pnlHorizSplitterResults: TPanel
        Cursor = crHSplit
        Left = 337
        Height = 268
        Top = 0
        Width = 10
        Anchors = [akTop, akLeft, akBottom]
        Caption = 'ResultsSplitter'
        Color = 13041606
        Font.Color = 13041606
        Font.Height = -11
        Font.Name = 'Tahoma'
        ParentBackground = False
        ParentColor = False
        ParentFont = False
        TabOrder = 2
        OnMouseDown = pnlHorizSplitterResultsMouseDown
        OnMouseMove = pnlHorizSplitterResultsMouseMove
        OnMouseUp = pnlHorizSplitterResultsMouseUp
      end
    end
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
  object tmrReloadOIContent: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrReloadOIContentTimer
    Left = 942
    Top = 233
  end
  object pmWindowOperationsEditors: TPopupMenu
    Left = 136
    Top = 112
    object MenuItem_SetFromControlLeftAndTop: TMenuItem
      Caption = 'Set from $Control_Left/Top$'
      Enabled = False
      OnClick = MenuItem_SetFromControlLeftAndTopClick
    end
    object MenuItem_SetFromControlWidthAndHeight: TMenuItem
      Caption = 'Set from $Control_Width/Height$'
      Enabled = False
      OnClick = MenuItem_SetFromControlWidthAndHeightClick
    end
  end
  object pmStandardControlRefVars: TPopupMenu
    Left = 632
    Top = 233
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
  object pmStandardColorVariables: TPopupMenu
    OnPopup = pmStandardColorVariablesPopup
    Left = 800
    Top = 233
    object MenuItemCopyColorToClipboard: TMenuItem
      Caption = 'Copy Color To Clipboard'
      OnClick = MenuItemCopyRefToClipboardClick
    end
    object MenuItemPasteColorFromClipboard: TMenuItem
      Caption = 'Paste Color From Clipboard'
      OnClick = MenuItemPasteRefFromClipboardClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object MenuItemColor_Window: TMenuItem
      Caption = '$Color_Window$'
      OnClick = MenuItemControl_EdgeRefGenericClick
    end
    object MenuItemColor_BtnFace: TMenuItem
      Caption = '$Color_BtnFace$'
      OnClick = MenuItemControl_EdgeRefGenericClick
    end
    object MenuItemColor_ActiveCaption: TMenuItem
      Caption = '$Color_ActiveCaption$'
      OnClick = MenuItemControl_EdgeRefGenericClick
    end
    object MenuItemColor_InactiveCaption: TMenuItem
      Caption = '$Color_InactiveCaption$'
      OnClick = MenuItemControl_EdgeRefGenericClick
    end
    object MenuItemColor_Highlight: TMenuItem
      Caption = '$Color_Highlight$'
      OnClick = MenuItemControl_EdgeRefGenericClick
    end
    object MenuItemColor_WindowText: TMenuItem
      Caption = '$Color_WindowText$'
      OnClick = MenuItemControl_EdgeRefGenericClick
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object MenuItemColor_GrayText: TMenuItem
      Caption = '$Color_GrayText$'
      OnClick = MenuItemControl_EdgeRefGenericClick
    end
    object MenuItemColor_GradientActiveCaption: TMenuItem
      Caption = '$Color_GradientActiveCaption$'
      OnClick = MenuItemControl_EdgeRefGenericClick
    end
    object MenuItemColor_GradientInactiveCaption: TMenuItem
      Caption = '$Color_GradientInactiveCaption$'
      OnClick = MenuItemControl_EdgeRefGenericClick
    end
    object MenuItemColor_ScrollBar: TMenuItem
      Caption = '$Color_ScrollBar$'
      OnClick = MenuItemControl_EdgeRefGenericClick
    end
    object MenuItemColor_3DDkShadow: TMenuItem
      Caption = '$Color_3DDkShadow$'
      OnClick = MenuItemControl_EdgeRefGenericClick
    end
    object MenuItemColor_3DLight: TMenuItem
      Caption = '$Color_3DLight$'
      OnClick = MenuItemControl_EdgeRefGenericClick
    end
    object MenuItemColor_WindowFrame: TMenuItem
      Caption = '$Color_WindowFrame$'
      OnClick = MenuItemControl_EdgeRefGenericClick
    end
  end
  object tmrDrawZoom: TTimer
    Enabled = False
    Interval = 50
    OnTimer = tmrDrawZoomTimer
    Left = 408
    Top = 224
  end
  object pmPathReplacements: TPopupMenu
    Left = 631
    Top = 176
    object MenuItem_ReplaceWithAppDir: TMenuItem
      Caption = 'Replace part of the path with $AppDir$'
      OnClick = MenuItem_ReplaceWithAppDirClick
    end
    object MenuItem_ReplaceWithTemplateDir: TMenuItem
      Caption = 'Replace part of the path with $TemplateDir$'
      OnClick = MenuItem_ReplaceWithTemplateDirClick
    end
    object MenuItem_ReplaceWithSelfTemplateDir: TMenuItem
      Caption = 'Replace part of the path with $SelfTemplateDir$'
      OnClick = MenuItem_ReplaceWithSelfTemplateDirClick
    end
  end
  object tmrClkVariables: TTimer
    Enabled = False
    Interval = 1
    OnTimer = tmrClkVariablesTimer
    Left = 88
    Top = 168
  end
  object tmrEditClkVariables: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrEditClkVariablesTimer
    Left = 85
    Top = 79
  end
  object tmrOnChangeEditTemplateEditingActionType: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrOnChangeEditTemplateEditingActionTypeTimer
    Left = 828
    Top = 120
  end
end
