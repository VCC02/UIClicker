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
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  LCLVersion = '7.5'
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
    Left = 425
    Height = 19
    Hint = 'When unchecked, only the used fonts are displayed'
    Top = 68
    Width = 94
    Anchors = [akTop, akRight]
    Caption = 'Show all fonts'
    OnChange = chkShowAllFontsChange
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
  end
  object grpPreview: TGroupBox
    Left = 8
    Height = 105
    Top = 176
    Width = 510
    Anchors = [akLeft, akBottom]
    Caption = 'Preview'
    ClientHeight = 85
    ClientWidth = 506
    TabOrder = 4
    object lblPreviewText: TLabel
      Left = 8
      Height = 15
      Top = 0
      Width = 71
      Caption = 'TextToSearch'
    end
  end
  object btnOK: TButton
    Left = 350
    Height = 25
    Top = 151
    Width = 75
    Anchors = [akTop, akRight]
    Caption = 'OK'
    OnClick = btnOKClick
    TabOrder = 5
  end
  object btnCancel: TButton
    Left = 444
    Height = 25
    Top = 151
    Width = 75
    Anchors = [akTop, akRight]
    Caption = 'Cancel'
    OnClick = btnCancelClick
    TabOrder = 6
  end
  object spnedtPreviewSize: TSpinEdit
    Left = 350
    Height = 23
    Top = 68
    Width = 59
    Anchors = [akTop, akRight]
    MaxValue = 100
    MinValue = 7
    OnChange = spnedtPreviewSizeChange
    TabOrder = 7
    Value = 8
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
    Left = 352
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
  end
end
