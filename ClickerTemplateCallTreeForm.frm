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
  LCLVersion = '8.4'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
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
    Caption = 'Browse...'
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object btnGenerate: TButton
    Left = 480
    Height = 25
    Top = 57
    Width = 75
    Anchors = [akTop, akRight]
    Caption = 'Generate'
    TabOrder = 2
    OnClick = btnGenerateClick
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
    Header.Columns = <    
      item
        MinWidth = 450
        Position = 0
        Text = 'Template name / path'
        Width = 450
      end    
      item
        MinWidth = 700
        Position = 1
        Text = 'Icon path'
        Width = 700
      end>
    Header.DefaultHeight = 17
    Header.Height = 17
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Header.Style = hsFlatButtons
    ParentShowHint = False
    PopupMenu = pmTree
    StateImages = imglstTemplateIcons
    TabOrder = 3
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnBeforeCellPaint = vstCallTreeBeforeCellPaint
    OnClick = vstCallTreeClick
    OnGetText = vstCallTreeGetText
    OnGetImageIndex = vstCallTreeGetImageIndex
  end
  object lblCallTree: TLabel
    Left = 8
    Height = 15
    Top = 128
    Width = 43
    Caption = 'Call tree'
  end
  object chkDisplayFullPaths: TCheckBox
    Left = 425
    Height = 19
    Top = 121
    Width = 108
    Anchors = [akTop, akRight]
    Caption = 'Display full paths'
    TabOrder = 4
    OnChange = chkDisplayFullPathsChange
  end
  object chkFullPathComparison: TCheckBox
    Left = 425
    Height = 19
    Hint = 'When checked, the full template path is verified, to identify a call.'
    Top = 96
    Width = 130
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
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    TextHint = 'Search'
    OnChange = edtSearchChange
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
    Style = csOwnerDrawFixed
    TabOrder = 7
    Text = 'Hide items'
    OnChange = cmbSearchModeChange
  end
  object lblSearchMode: TLabel
    Left = 366
    Height = 15
    Top = 333
    Width = 69
    Anchors = [akRight, akBottom]
    Caption = 'Search mode'
  end
  object chkDisplayCaller: TCheckBox
    Left = 264
    Height = 19
    Top = 121
    Width = 123
    Anchors = [akTop, akRight]
    Caption = 'Display caller action'
    TabOrder = 8
    OnChange = chkDisplayCallerChange
  end
  object chkRawPaths: TCheckBox
    Left = 152
    Height = 19
    Hint = 'When checked, the displayed paths are not resoved using var/replacements,'#13#10' like $SelfTemplateDir$, $TemplateDir$ and $AppDir$.'
    Top = 121
    Width = 72
    Anchors = [akTop, akRight]
    Caption = 'Raw paths'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 9
    OnChange = chkRawPathsChange
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
  object imglstTemplateIcons: TImageList
    Left = 456
    Top = 216
  end
end
