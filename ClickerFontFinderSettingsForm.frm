object frmClickerFontFinderSettings: TfrmClickerFontFinderSettings
  Left = 287
  Height = 289
  Top = 242
  Width = 510
  Caption = 'Clicker Font Finder Settings'
  ClientHeight = 289
  ClientWidth = 510
  Constraints.MinHeight = 289
  Constraints.MinWidth = 510
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  LCLVersion = '7.5'
  object vstFonts: TVirtualStringTree
    Left = 8
    Height = 152
    Top = 24
    Width = 328
    Anchors = [akTop, akLeft, akRight, akBottom]
    Colors.UnfocusedColor = clMedGray
    DefaultText = 'Node'
    Header.AutoSizeIndex = 0
    Header.Columns = <>
    Header.DefaultHeight = 17
    Header.MainColumn = -1
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnChecked = vstFontsChecked
    OnChecking = vstFontsChecking
    OnGetText = vstFontsGetText
    OnInitNode = vstFontsInitNode
  end
  object spnedtMinSize: TSpinEdit
    Left = 344
    Height = 23
    Top = 24
    Width = 59
    Anchors = [akTop, akRight]
    MaxValue = 100
    MinValue = 7
    TabOrder = 1
    Value = 8
  end
  object spnedtMaxSize: TSpinEdit
    Left = 416
    Height = 23
    Top = 24
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
    Left = 344
    Height = 15
    Top = 4
    Width = 43
    Anchors = [akTop, akRight]
    Caption = 'Min size'
  end
  object lblMaxSize: TLabel
    Left = 416
    Height = 15
    Top = 4
    Width = 44
    Anchors = [akTop, akRight]
    Caption = 'Max size'
  end
  object chkShowAllFonts: TCheckBox
    Left = 344
    Height = 19
    Hint = 'When unchecked, only the used fonts are displayed'
    Top = 64
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
    Width = 491
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Preview'
    ClientHeight = 85
    ClientWidth = 487
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
    Left = 344
    Height = 25
    Top = 151
    Width = 75
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    OnClick = btnOKClick
    TabOrder = 5
  end
  object btnCancel: TButton
    Left = 424
    Height = 25
    Top = 151
    Width = 75
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    OnClick = btnCancelClick
    TabOrder = 6
  end
  object tmrChecked: TTimer
    Enabled = False
    Interval = 1
    OnTimer = tmrCheckedTimer
    Left = 434
    Top = 102
  end
  object tmrStartup: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrStartupTimer
    Left = 434
    Top = 203
  end
end
