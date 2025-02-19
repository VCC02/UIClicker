object frmClickerFontFinderSettings: TfrmClickerFontFinderSettings
  Left = 287
  Height = 289
  Top = 242
  Width = 529
  Caption = 'Clicker Font Finder Settings'
  ClientHeight = 289
  ClientWidth = 529
  Constraints.MinHeight = 289
  Constraints.MinWidth = 510
  LCLVersion = '8.4'
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  object vstFonts: TVirtualStringTree
    Left = 8
    Height = 160
    Top = 20
    Width = 336
    Anchors = [akTop, akLeft, akRight, akBottom]
    CheckImageKind = ckXP
    Colors.UnfocusedColor = clMedGray
    DefaultText = 'Node'
    Header.AutoSizeIndex = 0
    Header.Columns = <    
      item
        MinWidth = 200
        Position = 0
        Text = 'Name'
        Width = 200
      end    
      item
        MinWidth = 200
        Position = 1
        Text = 'Preview [text]'
        Width = 200
      end    
      item
        MinWidth = 500
        Position = 2
        Text = 'Preview [All letters]'
        Width = 500
      end    
      item
        MinWidth = 100
        Position = 3
        Text = 'Histogram diff'
        Width = 100
      end>
    Header.DefaultHeight = 21
    Header.Height = 21
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Header.Style = hsFlatButtons
    ParentShowHint = False
    PopupMenu = pmSelection
    ShowHint = True
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
    OnBeforeCellPaint = vstFontsBeforeCellPaint
    OnChecked = vstFontsChecked
    OnChecking = vstFontsChecking
    OnGetText = vstFontsGetText
    OnPaintText = vstFontsPaintText
    OnInitNode = vstFontsInitNode
    OnKeyDown = vstFontsKeyDown
  end
  object spnedtMinSize: TSpinEdit
    Left = 350
    Height = 23
    Top = 20
    Width = 59
    Anchors = [akTop, akRight]
    MaxValue = 100
    MinValue = 7
    TabOrder = 1
    Value = 8
  end
  object spnedtMaxSize: TSpinEdit
    Left = 425
    Height = 23
    Top = 20
    Width = 58
    Anchors = [akTop, akRight]
    MaxValue = 100
    MinValue = 7
    TabOrder = 2
    Value = 9
  end
  object lblFontNames: TLabel
    Left = 8
    Height = 15
    Top = 4
    Width = 29
    Caption = 'Fonts'
  end
  object lblMinSize: TLabel
    Left = 350
    Height = 15
    Top = 4
    Width = 43
    Anchors = [akTop, akRight]
    Caption = 'Min size'
  end
  object lblMaxSize: TLabel
    Left = 424
    Height = 15
    Top = 4
    Width = 44
    Anchors = [akTop, akRight]
    Caption = 'Max size'
  end
  object chkShowAllFonts: TCheckBox
    Left = 424
    Height = 19
    Hint = 'When unchecked, only the used fonts are displayed'
    Top = 48
    Width = 92
    Anchors = [akTop, akRight]
    Caption = 'Show all fonts'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnChange = chkShowAllFontsChange
  end
  object grpPreview: TGroupBox
    Left = 8
    Height = 97
    Top = 184
    Width = 508
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Preview'
    ClientHeight = 77
    ClientWidth = 504
    Color = clMoneyGreen
    ParentBackground = False
    ParentColor = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    object lblPreviewText: TLabel
      Left = 8
      Height = 15
      Hint = 'Preview by font settings.'
      Top = 0
      Width = 71
      Caption = 'TextToSearch'
      ParentShowHint = False
      ShowHint = True
      Transparent = False
    end
    object imgPreviewByBmp: TImage
      Left = 400
      Height = 71
      Hint = 'Preview by cropped bitmap from baackground.'
      Top = 0
      Width = 90
      ParentShowHint = False
      ShowHint = True
    end
  end
  object btnOK: TButton
    Left = 350
    Height = 25
    Top = 151
    Width = 75
    Anchors = [akTop, akRight]
    Caption = 'OK'
    TabOrder = 5
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 441
    Height = 25
    Top = 151
    Width = 75
    Anchors = [akTop, akRight]
    Caption = 'Cancel'
    TabOrder = 6
    OnClick = btnCancelClick
  end
  object spnedtPreviewSize: TSpinEdit
    Left = 350
    Height = 23
    Top = 68
    Width = 59
    Anchors = [akTop, akRight]
    MaxValue = 100
    MinValue = 7
    TabOrder = 7
    Value = 8
    OnChange = spnedtPreviewSizeChange
  end
  object lblPreviewSize: TLabel
    Left = 350
    Height = 15
    Top = 48
    Width = 63
    Anchors = [akTop, akRight]
    Caption = 'Preview size'
  end
  object lbeSearch: TLabeledEdit
    Left = 350
    Height = 23
    Top = 116
    Width = 166
    Anchors = [akTop, akRight]
    EditLabel.Height = 15
    EditLabel.Width = 166
    EditLabel.Caption = 'Search'
    TabOrder = 8
    OnChange = lbeSearchChange
  end
  object chkSortResults: TCheckBox
    Left = 424
    Height = 19
    Hint = 'If checked, the list of fonts, returned to the finder algorithm, is sorted by histogram differences.'
    Top = 72
    Width = 76
    Anchors = [akTop, akRight]
    Caption = 'Sort results'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 9
  end
  object prbDiffs: TProgressBar
    Left = 350
    Height = 5
    Hint = 'Computing histograms...'
    Top = 175
    Width = 166
    Anchors = [akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 10
    Visible = False
  end
  object lblFontStatistics: TLabel
    Left = 124
    Height = 15
    Top = 4
    Width = 20
    Caption = 'Stat'
  end
  object tmrChecked: TTimer
    Enabled = False
    Interval = 1
    OnTimer = tmrCheckedTimer
    Left = 240
    Top = 96
  end
  object tmrStartup: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrStartupTimer
    Left = 434
    Top = 203
  end
  object pmSelection: TPopupMenu
    Left = 108
    Top = 69
    object MenuItem_CheckAll: TMenuItem
      Caption = 'Check all'
      OnClick = MenuItem_CheckAllClick
    end
    object MenuItem_UnCheckAll: TMenuItem
      Caption = 'Uncheck all'
      OnClick = MenuItem_UnCheckAllClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MenuItem_CheckAllSelected: TMenuItem
      Caption = 'Check all selected'
      OnClick = MenuItem_CheckAllSelectedClick
    end
    object MenuItem_UnCheckAllSelected: TMenuItem
      Caption = 'Uncheck all selected'
      OnClick = MenuItem_UnCheckAllSelectedClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object MenuItem_SelectAllCheckedByPreservingSelection: TMenuItem
      Caption = 'Select all checked (preserve current selection)'
      OnClick = MenuItem_SelectAllCheckedByPreservingSelectionClick
    end
    object MenuItem_SelectAllCheckedByClearingSelection: TMenuItem
      Caption = 'Select all checked (clear current selection first)'
      OnClick = MenuItem_SelectAllCheckedByClearingSelectionClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object MenuItem_InvertSelection: TMenuItem
      Caption = 'Invert selection'
      OnClick = MenuItem_InvertSelectionClick
    end
    object MenuItem_InvertCheckedState: TMenuItem
      Caption = 'Invert checked state'
      OnClick = MenuItem_InvertCheckedStateClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object MenuItem_ImportListOfFonts: TMenuItem
      Caption = 'Import list of fonts from text file'
      object MenuItem_ImportListOfFonts_ReplaceExisting: TMenuItem
        Caption = 'Replace existing list...'
        OnClick = MenuItem_ImportListOfFonts_ReplaceExistingClick
      end
      object MenuItem_ImportListOfFonts_MergeWithExisting: TMenuItem
        Caption = 'Merge with existing list...'
        OnClick = MenuItem_ImportListOfFonts_MergeWithExistingClick
      end
    end
    object MenuItem_ExportListOfSelectedFonts: TMenuItem
      Caption = 'Export list of selected fonts...'
      OnClick = MenuItem_ExportListOfSelectedFontsClick
    end
  end
end
