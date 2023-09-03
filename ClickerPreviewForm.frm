object frmClickerControlPreview: TfrmClickerControlPreview
  Left = 387
  Height = 837
  Top = 156
  Width = 1187
  Caption = 'Clicker Control Preview'
  ClientHeight = 837
  ClientWidth = 1187
  Color = clBtnFace
  Constraints.MinHeight = 837
  Constraints.MinWidth = 1187
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  FormStyle = fsStayOnTop
  OnCreate = FormCreate
  LCLVersion = '7.8'
  object lblInstructions: TLabel
    Left = 24
    Height = 13
    Top = 0
    Width = 315
    Caption = 'Hold Ctrl+Shift, to take screenshot under mouse cursor.'
    Font.Color = 185
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object scrboxScreenshot: TScrollBox
    Left = 375
    Height = 821
    Top = 8
    Width = 801
    HorzScrollBar.Increment = 76
    HorzScrollBar.Page = 762
    HorzScrollBar.Smooth = True
    HorzScrollBar.Tracking = True
    VertScrollBar.Increment = 75
    VertScrollBar.Page = 756
    VertScrollBar.Smooth = True
    VertScrollBar.Tracking = True
    Anchors = [akTop, akLeft, akRight, akBottom]
    ClientHeight = 817
    ClientWidth = 797
    TabOrder = 0
    object pnlBase: TPanel
      Left = 0
      Height = 756
      Top = 0
      Width = 762
      ClientHeight = 756
      ClientWidth = 762
      Color = clSkyBlue
      ParentColor = False
      TabOrder = 0
      object imgScreenshot: TImage
        Left = 15
        Height = 736
        Top = 15
        Width = 736
        OnMouseDown = imgScreenshotMouseDown
        OnMouseMove = imgScreenshotMouseMove
        OnMouseUp = imgScreenshotMouseUp
      end
      object pnlCaptureWidth: TPanel
        Left = 13
        Height = 10
        Top = 0
        Width = 714
        Color = 33023
        ParentColor = False
        TabOrder = 0
      end
      object pnlCaptureHeight: TPanel
        Left = 0
        Height = 721
        Top = 17
        Width = 10
        Color = 33023
        ParentColor = False
        TabOrder = 1
      end
      object pnlSelLeft: TPanel
        Left = 80
        Height = 172
        Top = 108
        Width = 9
        BevelOuter = bvNone
        Color = clRed
        ParentColor = False
        TabOrder = 2
        Visible = False
      end
      object pnlSelTop: TPanel
        Left = 95
        Height = 15
        Top = 87
        Width = 185
        BevelOuter = bvNone
        Color = clRed
        ParentColor = False
        TabOrder = 3
        Visible = False
      end
      object pnlSelRight: TPanel
        Left = 286
        Height = 172
        Top = 108
        Width = 11
        BevelOuter = bvNone
        Color = clRed
        ParentColor = False
        TabOrder = 4
        Visible = False
      end
      object pnlSelBottom: TPanel
        Left = 95
        Height = 10
        Top = 286
        Width = 185
        BevelOuter = bvNone
        Color = clRed
        ParentColor = False
        TabOrder = 5
        Visible = False
      end
    end
  end
  object grpWinInfo: TGroupBox
    Left = 8
    Height = 204
    Top = 36
    Width = 361
    Caption = 'WinInfo'
    ClientHeight = 186
    ClientWidth = 357
    TabOrder = 1
    object lbeHandle: TLabeledEdit
      Left = 1
      Height = 21
      Top = 96
      Width = 121
      Color = clBtnFace
      EditLabel.Height = 13
      EditLabel.Width = 121
      EditLabel.Caption = 'Handle'
      ReadOnly = True
      TabOrder = 0
    end
    object lbeClass: TLabeledEdit
      Left = 1
      Height = 21
      Top = 56
      Width = 347
      Color = clBtnFace
      EditLabel.Height = 13
      EditLabel.Width = 347
      EditLabel.Caption = 'Class'
      ReadOnly = True
      TabOrder = 1
    end
    object lbeText: TLabeledEdit
      Left = 1
      Height = 21
      Top = 13
      Width = 346
      Color = clBtnFace
      EditLabel.Height = 13
      EditLabel.Width = 346
      EditLabel.Caption = 'Text'
      ReadOnly = True
      TabOrder = 2
    end
    object lbeRect: TLabeledEdit
      Left = 128
      Height = 21
      Top = 96
      Width = 219
      Color = clBtnFace
      EditLabel.Height = 13
      EditLabel.Width = 219
      EditLabel.Caption = 'Rectangle'
      ReadOnly = True
      TabOrder = 3
    end
    object lbeMouseXOffset: TLabeledEdit
      Left = 1
      Height = 21
      Top = 135
      Width = 82
      Color = clBtnFace
      EditLabel.Height = 13
      EditLabel.Width = 82
      EditLabel.Caption = 'Mouse: X Offset'
      ReadOnly = True
      TabOrder = 4
    end
    object lbeMouseYOffset: TLabeledEdit
      Left = 89
      Height = 21
      Top = 135
      Width = 82
      Color = clBtnFace
      EditLabel.Height = 13
      EditLabel.Width = 82
      EditLabel.Caption = 'Mouse: Y Offset'
      ReadOnly = True
      TabOrder = 5
    end
    object lbeMouseGX: TLabeledEdit
      Left = 177
      Height = 21
      Top = 135
      Width = 82
      Color = clBtnFace
      EditLabel.Height = 13
      EditLabel.Width = 82
      EditLabel.Caption = 'Mouse: Global X'
      ReadOnly = True
      TabOrder = 6
    end
    object lbeMouseGY: TLabeledEdit
      Left = 265
      Height = 21
      Top = 135
      Width = 82
      Color = clBtnFace
      EditLabel.Height = 13
      EditLabel.Width = 82
      EditLabel.Caption = 'Mouse: Global Y'
      ReadOnly = True
      TabOrder = 7
    end
    object pnlDrag: TPanel
      Left = 1
      Height = 22
      Hint = 'Use this box to get the target component from another window.'
      Top = 160
      Width = 346
      Caption = 'Drag the mouse cursor, from this box to the target window'
      Color = clYellow
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      ParentColor = False
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      OnMouseDown = pnlDragMouseDown
      OnMouseMove = pnlDragMouseMove
      OnMouseUp = pnlDragMouseUp
    end
  end
  object grpTestSetControlText: TGroupBox
    Left = 8
    Height = 110
    Top = 725
    Width = 230
    Caption = 'Test Set Control Text'
    ClientHeight = 92
    ClientWidth = 226
    TabOrder = 2
    object lbeTestSetControlTextHandle: TLabeledEdit
      Left = 14
      Height = 21
      Top = 15
      Width = 89
      EditLabel.Height = 13
      EditLabel.Width = 89
      EditLabel.Caption = 'Control Handle'
      TabOrder = 0
    end
    object btnTestSetControlText: TButton
      Left = 116
      Height = 25
      Top = 65
      Width = 89
      Caption = 'Set'
      OnClick = btnTestSetControlTextClick
      TabOrder = 1
    end
    object rdgrpTestSetTextControlType: TRadioGroup
      Left = 116
      Height = 58
      Hint = 'Use WM_SETTEXT or CB_SELECTSTRING messages.'
      Top = 1
      Width = 89
      AutoFill = True
      Caption = 'Control Type'
      ChildSizing.LeftRightSpacing = 6
      ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
      ChildSizing.EnlargeVertical = crsHomogenousChildResize
      ChildSizing.ShrinkHorizontal = crsScaleChilds
      ChildSizing.ShrinkVertical = crsScaleChilds
      ChildSizing.Layout = cclLeftToRightThenTopToBottom
      ChildSizing.ControlsPerLine = 1
      ClientHeight = 40
      ClientWidth = 85
      ItemIndex = 0
      Items.Strings = (
        'EditBox'
        'ComboBox'
      )
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
    end
    object lbeTestSetControlTextNewText: TLabeledEdit
      Left = 14
      Height = 21
      Top = 65
      Width = 89
      EditLabel.Height = 13
      EditLabel.Width = 89
      EditLabel.Caption = 'New Text'
      TabOrder = 3
    end
  end
  object chkStayOnTop: TCheckBox
    Left = 264
    Height = 19
    Top = 810
    Width = 76
    Caption = 'Stay on top'
    Checked = True
    OnClick = chkStayOnTopClick
    State = cbChecked
    TabOrder = 3
  end
  object chkScanningTimer: TCheckBox
    Left = 8
    Height = 19
    Hint = 'Check this, to allow scanning the control under mouse cursor, when Ctrl+Shift keys are down.'
    Top = 14
    Width = 177
    Caption = 'Screenshots scanning timer'
    Color = clBtnFace
    Font.Color = 185
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    OnChange = chkScanningTimerChange
    ParentColor = False
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
  end
  object chkNoKeysScanningTimer: TCheckBox
    Left = 208
    Height = 19
    Top = 14
    Width = 151
    Caption = 'No keys scanning timer'
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    OnChange = chkNoKeysScanningTimerChange
    ParentFont = False
    TabOrder = 5
  end
  object pnlImageSelection: TPanel
    Left = 0
    Height = 480
    Top = 240
    Width = 371
    Caption = 'pnlImageSelection'
    ClientHeight = 480
    ClientWidth = 371
    TabOrder = 6
    object lblCropImage: TLabel
      Left = 8
      Height = 13
      Top = 23
      Width = 95
      Caption = 'Crop image settings'
    end
    object lblCropLeft: TLabel
      Left = 341
      Height = 13
      Top = 77
      Width = 6
      Caption = '0'
    end
    object lblCropRight: TLabel
      Left = 341
      Height = 13
      Top = 107
      Width = 6
      Caption = '0'
    end
    object lblCropTop: TLabel
      Left = 8
      Height = 13
      Top = 463
      Width = 6
      Caption = '0'
    end
    object lblCropBottom: TLabel
      Left = 63
      Height = 13
      Top = 463
      Width = 6
      Caption = '0'
    end
    object trbCropLeft: TTrackBar
      Left = 0
      Height = 33
      Top = 73
      Width = 337
      OnChange = trbCropLeftChange
      Position = 3
      TabOrder = 0
    end
    object trbCropRight: TTrackBar
      Left = 0
      Height = 32
      Top = 96
      Width = 337
      OnChange = trbCropRightChange
      Position = 8
      TickMarks = tmTopLeft
      TabOrder = 1
    end
    object trbCropTop: TTrackBar
      Left = 24
      Height = 337
      Top = 126
      Width = 33
      OnChange = trbCropTopChange
      Orientation = trVertical
      Position = 3
      TabOrder = 2
    end
    object trbCropBottom: TTrackBar
      Left = 47
      Height = 337
      Top = 126
      Width = 32
      OnChange = trbCropBottomChange
      Orientation = trVertical
      Position = 8
      TickMarks = tmTopLeft
      TabOrder = 3
    end
    object chkShowCropRectangle: TCheckBox
      Left = 8
      Height = 19
      Top = 3
      Width = 123
      Caption = 'Show Crop Rectangle'
      OnClick = chkShowCropRectangleClick
      TabOrder = 4
    end
    object scrboxCrop: TScrollBox
      Left = 85
      Height = 281
      Top = 135
      Width = 284
      HorzScrollBar.Increment = 25
      HorzScrollBar.Page = 257
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Increment = 25
      VertScrollBar.Page = 257
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      ClientHeight = 277
      ClientWidth = 280
      Color = clSkyBlue
      ParentColor = False
      TabOrder = 5
      object imgCrop: TImage
        Left = 0
        Height = 257
        Top = 0
        Width = 257
      end
    end
    object trbCropZoom: TTrackBar
      Left = 117
      Height = 34
      Top = 429
      Width = 150
      Max = 4
      OnChange = trbCropZoomChange
      Position = 0
      TabOrder = 6
    end
    object btnSaveImage: TButton
      Left = 138
      Height = 25
      Top = 42
      Width = 101
      Caption = 'Save image as...'
      OnClick = btnSaveImageClick
      TabOrder = 7
    end
    object btnSelectionRectangleColor: TButton
      Left = 138
      Height = 25
      Top = 11
      Width = 119
      Caption = 'Sel Rectangle Color...'
      OnClick = btnSelectionRectangleColorClick
      TabOrder = 8
    end
    object btnSelectFull: TButton
      Left = 4
      Height = 25
      Top = 42
      Width = 75
      Caption = 'Select Full'
      OnClick = btnSelectFullClick
      TabOrder = 9
    end
    object btnCopyImageToClipboard: TButton
      Left = 265
      Height = 25
      Top = 42
      Width = 104
      Caption = 'Copy To Clipboard'
      OnClick = btnCopyImageToClipboardClick
      TabOrder = 10
    end
    object lblZoom: TLabel
      Left = 85
      Height = 13
      Top = 432
      Width = 30
      Caption = 'Zoom:'
    end
    object lblInfo: TLabel
      Left = 265
      Height = 23
      Top = 11
      Width = 103
      AutoSize = False
      Caption = 'Reserved space'
      Visible = False
    end
  end
  object tmrScan: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrScanTimer
    Left = 800
    Top = 96
  end
  object SavePictureDialog1: TSavePictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 968
    Top = 96
  end
  object ColorDialog1: TColorDialog
    Color = clBlack
    CustomColors.Strings = (
      'ColorA=000000'
      'ColorB=000080'
      'ColorC=008000'
      'ColorD=008080'
      'ColorE=800000'
      'ColorF=800080'
      'ColorG=808000'
      'ColorH=808080'
      'ColorI=C0C0C0'
      'ColorJ=0000FF'
      'ColorK=00FF00'
      'ColorL=00FFFF'
      'ColorM=FF0000'
      'ColorN=FF00FF'
      'ColorO=FFFF00'
      'ColorP=FFFFFF'
      'ColorQ=C0DCC0'
      'ColorR=F0CAA6'
      'ColorS=F0FBFF'
      'ColorT=A4A0A0'
    )
    Left = 872
    Top = 96
  end
  object tmrStartup: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrStartupTimer
    Left = 720
    Top = 96
  end
end
