object frmClickerTemplateCallTree: TfrmClickerTemplateCallTree
  Left = 487
  Height = 375
  Top = 156
  Width = 564
  Caption = 'Clicker Template - Call tree'
  ClientHeight = 375
  ClientWidth = 564
  Constraints.MinHeight = 375
  Constraints.MinWidth = 564
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  LCLVersion = '7.5'
  object memTemplates: TMemo
    Left = 8
    Height = 90
    Top = 24
    Width = 408
    Anchors = [akTop, akLeft, akRight]
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object lblInfoTemplates: TLabel
    Left = 8
    Height = 15
    Top = 8
    Width = 291
    Caption = 'Paste here the list of templates to generate the call tree.'
  end
  object btnBrowse: TButton
    Left = 480
    Height = 25
    Top = 24
    Width = 75
    Anchors = [akTop, akRight]
    Caption = 'Browse'
    OnClick = btnBrowseClick
    TabOrder = 1
  end
  object btnGenerate: TButton
    Left = 480
    Height = 25
    Top = 57
    Width = 75
    Anchors = [akTop, akRight]
    Caption = 'Generate'
    OnClick = btnGenerateClick
    TabOrder = 2
  end
  object vstCallTree: TVirtualStringTree
    Left = 8
    Height = 178
    Top = 144
    Width = 547
    Anchors = [akTop, akLeft, akRight, akBottom]
    Colors.UnfocusedColor = clMedGray
    Colors.UnfocusedSelectionColor = clGradientInactiveCaption
    DefaultText = 'Node'
    Header.AutoSizeIndex = -1
    Header.Columns = <>
    Header.DefaultHeight = 17
    Header.MainColumn = -1
    ParentShowHint = False
    PopupMenu = pmTree
    TabOrder = 3
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnBeforeCellPaint = vstCallTreeBeforeCellPaint
    OnClick = vstCallTreeClick
    OnGetText = vstCallTreeGetText
  end
  object lblCallTree: TLabel
    Left = 8
    Height = 15
    Top = 128
    Width = 43
    Caption = 'Call tree'
  end
  object chkDisplayFullPaths: TCheckBox
    Left = 423
    Height = 19
    Top = 121
    Width = 110
    Anchors = [akTop, akRight]
    Caption = 'Display full paths'
    OnChange = chkDisplayFullPathsChange
    TabOrder = 4
  end
  object chkFullPathComparison: TCheckBox
    Left = 423
    Height = 19
    Hint = 'When checked, the full template path is verified, to identify a call.'
    Top = 96
    Width = 132
    Anchors = [akTop, akRight]
    Caption = 'Full path comparison'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
  end
  object lblSelectedPath: TLabel
    Left = 8
    Height = 15
    Top = 357
    Width = 71
    Anchors = [akLeft, akBottom]
    Caption = 'Selected Path'
  end
  object edtSearch: TEdit
    Left = 8
    Height = 23
    Hint = 'The search string depends on "Display full paths" checkbox.'
    Top = 328
    Width = 344
    Anchors = [akLeft, akRight, akBottom]
    OnChange = edtSearchChange
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    TextHint = 'Search'
  end
  object cmbSearchMode: TComboBox
    Left = 440
    Height = 21
    Top = 330
    Width = 115
    Anchors = [akRight, akBottom]
    ItemHeight = 15
    ItemIndex = 0
    Items.Strings = (
      'Hide items'
      'Highlight items'
    )
    OnChange = cmbSearchModeChange
    Style = csOwnerDrawFixed
    TabOrder = 7
    Text = 'Hide items'
  end
  object lblSearchMode: TLabel
    Left = 366
    Height = 15
    Top = 333
    Width = 69
    Anchors = [akRight, akBottom]
    Caption = 'Search mode'
  end
  object tmrSearch: TTimer
    Enabled = False
    Interval = 200
    OnTimer = tmrSearchTimer
    Left = 352
    Top = 272
  end
  object pmTree: TPopupMenu
    Left = 272
    Top = 272
    object MenuItem_CopySelectedFileNameToClipboard: TMenuItem
      Caption = 'Copy selected filename to clipboard'
      OnClick = MenuItem_CopySelectedFileNameToClipboardClick
    end
    object MenuItem_CopySelectedFilePathToClipboard: TMenuItem
      Caption = 'Copy selected filepath to clipboard'
      OnClick = MenuItem_CopySelectedFilePathToClipboardClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MenuItem_Export: TMenuItem
      Caption = 'Export to file...'
      OnClick = MenuItem_ExportClick
    end
  end
end
