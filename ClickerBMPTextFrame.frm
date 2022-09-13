object frClickerBMPText: TfrClickerBMPText
  Left = 0
  Height = 206
  Top = 0
  Width = 434
  ClientHeight = 206
  ClientWidth = 434
  TabOrder = 0
  DesignLeft = 86
  DesignTop = 85
  object cmbMatchBitmapTextFontQuality: TComboBox
    Left = 40
    Height = 22
    Hint = 'Available variable replacement values (case sensitive): Default, Draft, Proof, NonAntialiased, Antialiased, Cleartype, CleartypeNatural'
    Top = 128
    Width = 121
    ItemHeight = 16
    Items.Strings = (
      'Default'
      'Draft'
      'Proof'
      'NonAntialiased'
      'Antialiased'
      'Cleartype'
      'CleartypeNatural'
      'Var Replacement'
    )
    OnChange = cmbMatchBitmapTextFontQualityChange
    OnDropDown = cmbMatchBitmapTextFontQualityDropDown
    ParentShowHint = False
    ShowHint = True
    Style = csOwnerDrawFixed
    TabOrder = 1
  end
  object edtFontQualityReplacement: TEdit
    Left = 40
    Height = 23
    Top = 128
    Width = 100
    TabOrder = 0
    Text = '$MyFontQuality$'
    Visible = False
  end
  object lblMatchBitmapTextFontQuality: TLabel
    Left = 3
    Height = 15
    Top = 133
    Width = 38
    Caption = 'Quality'
  end
  object chkItalic: TCheckBox
    Left = 3
    Height = 19
    Top = 107
    Width = 45
    Caption = 'Italic'
    OnClick = chkItalicClick
    TabOrder = 2
  end
  object chkBold: TCheckBox
    Left = 3
    Height = 19
    Top = 89
    Width = 44
    Caption = 'Bold'
    OnClick = chkBoldClick
    TabOrder = 3
  end
  object chkUnderline: TCheckBox
    Left = 54
    Height = 19
    Top = 89
    Width = 71
    Caption = 'Underline'
    OnClick = chkUnderlineClick
    TabOrder = 4
  end
  object chkStrikeOut: TCheckBox
    Left = 54
    Height = 19
    Top = 107
    Width = 69
    Caption = 'StrikeOut'
    OnClick = chkStrikeOutClick
    TabOrder = 5
  end
  object lbeMatchBitmapTextSize: TLabeledEdit
    Left = 127
    Height = 23
    Hint = 'No replacements available.'
    Top = 103
    Width = 34
    EditLabel.Height = 15
    EditLabel.Width = 34
    EditLabel.Caption = 'Size'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    Text = '8'
    OnChange = lbeMatchBitmapTextSizeChange
  end
  object lbeMatchBitmapTextFontName: TLabeledEdit
    Left = 3
    Height = 23
    Hint = 'Variable replacements are available. Because of font editing limitations, please do not use multiple replacements at the same time (e.g. "$var1$_$var2$").'
    Top = 62
    Width = 126
    EditLabel.Height = 15
    EditLabel.Width = 126
    EditLabel.Caption = 'Font Name'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    OnChange = lbeMatchBitmapTextFontNameChange
  end
  object btnBrowseFont: TButton
    Left = 135
    Height = 21
    Hint = 'Browse Font'
    Top = 62
    Width = 26
    Caption = '...'
    OnClick = btnBrowseFontClick
    ParentShowHint = False
    TabOrder = 8
  end
  object scrboxPreview: TScrollBox
    Left = 167
    Height = 89
    Top = 62
    Width = 265
    HorzScrollBar.Increment = 25
    HorzScrollBar.Page = 257
    HorzScrollBar.Smooth = True
    HorzScrollBar.Tracking = True
    VertScrollBar.Page = 85
    VertScrollBar.Smooth = True
    VertScrollBar.Tracking = True
    ClientHeight = 85
    ClientWidth = 261
    Color = clYellow
    ParentColor = False
    PopupMenu = pmPreviewImage
    TabOrder = 9
    object imgPreview: TImage
      Left = 1
      Height = 84
      Hint = 'Right-click for options'
      Top = 1
      Width = 256
      OnResize = imgPreviewResize
      ParentShowHint = False
      PopupMenu = pmPreviewImage
      ShowHint = True
    end
  end
  object lblPreview: TLabel
    Left = 167
    Height = 15
    Hint = 'Click this text to refresh the preview bitmap.'
    Top = 48
    Width = 41
    Caption = 'Preview'
    ParentShowHint = False
    ShowHint = True
    OnClick = lblPreviewClick
  end
  object lbeMatchBitmapTextFGColor: TLabeledEdit
    Left = 167
    Height = 23
    Hint = '6-digit hex value in BGR format, or variable replacements.'#13#10'Right-click for predefined values.'
    Top = 18
    Width = 99
    EditLabel.Height = 15
    EditLabel.Width = 99
    EditLabel.Caption = 'FG Color'
    ParentShowHint = False
    PopupMenu = pmStandardColorVariables
    ShowHint = True
    TabOrder = 10
    OnChange = lbeMatchBitmapTextFGColorChange
    OnMouseUp = lbeMatchBitmapTextFGColorMouseUp
  end
  object lbeMatchBitmapTextBGColor: TLabeledEdit
    Left = 272
    Height = 23
    Hint = '6-digit hex value in BGR format, or variable replacements.'#13#10'Right-click for predefined values.'
    Top = 18
    Width = 89
    EditLabel.Height = 15
    EditLabel.Width = 89
    EditLabel.Caption = 'BG Color'
    ParentShowHint = False
    PopupMenu = pmStandardColorVariables
    ShowHint = True
    TabOrder = 11
    OnChange = lbeMatchBitmapTextBGColorChange
    OnMouseUp = lbeMatchBitmapTextBGColorMouseUp
  end
  object pnlFG: TPanel
    Left = 367
    Height = 38
    Hint = 'Double click to set FG'
    Top = 1
    Width = 20
    Caption = 'FG'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 12
    OnDblClick = pnlFGDblClick
  end
  object pnlBG: TPanel
    Left = 389
    Height = 38
    Hint = 'Double click to set BG'
    Top = 1
    Width = 20
    Caption = 'BG'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 13
    OnDblClick = pnlBGDblClick
  end
  object pnlProfileName: TPanel
    Left = 272
    Height = 17
    Top = 43
    Width = 160
    Caption = 'Profile Name'
    TabOrder = 14
  end
  object lbeMatchBitmapTextCropLeft: TLabeledEdit
    Left = 3
    Height = 23
    Hint = 'Variable replacements are available.'#13#10'Positive values are expected.'
    Top = 176
    Width = 97
    EditLabel.Height = 15
    EditLabel.Width = 97
    EditLabel.Caption = 'Crop Left'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 15
    OnChange = lbeMatchBitmapTextCropLeftChange
    OnKeyDown = lbeMatchBitmapTextCropLeftKeyDown
  end
  object lbeMatchBitmapTextCropTop: TLabeledEdit
    Left = 111
    Height = 23
    Hint = 'Variable replacements are available.'#13#10'Positive values are expected.'
    Top = 176
    Width = 97
    EditLabel.Height = 15
    EditLabel.Width = 97
    EditLabel.Caption = 'Crop Top'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 16
    OnChange = lbeMatchBitmapTextCropTopChange
    OnKeyDown = lbeMatchBitmapTextCropTopKeyDown
  end
  object lbeMatchBitmapTextCropBottom: TLabeledEdit
    Left = 327
    Height = 23
    Hint = 'Variable replacements are available.'#13#10'Positive values are expected.'
    Top = 176
    Width = 97
    EditLabel.Height = 15
    EditLabel.Width = 97
    EditLabel.Caption = 'Crop Bottom'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 17
    OnChange = lbeMatchBitmapTextCropBottomChange
    OnKeyDown = lbeMatchBitmapTextCropBottomKeyDown
  end
  object lbeMatchBitmapTextCropRight: TLabeledEdit
    Left = 219
    Height = 23
    Hint = 'Variable replacements are available.'#13#10'Positive values are expected.'
    Top = 176
    Width = 97
    EditLabel.Height = 15
    EditLabel.Width = 97
    EditLabel.Caption = 'Crop Right'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 18
    OnChange = lbeMatchBitmapTextCropRightChange
    OnKeyDown = lbeMatchBitmapTextCropRightKeyDown
  end
  object pmPreviewImage: TPopupMenu
    Left = 336
    Top = 94
    object MenuItemSavePreviewImage: TMenuItem
      Caption = 'Save preview image as bmp...'
      OnClick = MenuItemSavePreviewImageClick
    end
    object MenuItemCopyPreviewImage: TMenuItem
      Caption = 'Copy preview image to clipboard'
      OnClick = MenuItemCopyPreviewImageClick
    end
    object MenuItemCopyPreviewImageAndCroppingLines: TMenuItem
      Caption = 'Copy preview image and cropping lines to clipboard'
      OnClick = MenuItemCopyPreviewImageAndCroppingLinesClick
    end
    object MenuItemCopyCroppedPreviewImage: TMenuItem
      Caption = 'Copy cropped preview image to clipboard'
      OnClick = MenuItemCopyCroppedPreviewImageClick
    end
    object MenuItemErasePreviewImage: TMenuItem
      Caption = 'Erase preview image'
      OnClick = MenuItemErasePreviewImageClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MenuItemCopyCroppingValuesToOtherProfiles: TMenuItem
      Caption = 'Copy these cropping values to all the other font profiles'
      OnClick = MenuItemCopyCroppingValuesToOtherProfilesClick
    end
  end
  object pmStandardColorVariables: TPopupMenu
    OnPopup = pmStandardColorVariablesPopup
    Left = 232
    Top = 78
    object MenuItemCopyColorToClipboard: TMenuItem
      Caption = 'Copy Color To Clipboard'
      OnClick = MenuItemCopyColorToClipboardClick
    end
    object MenuItemPasteColorFromClipboard: TMenuItem
      Caption = 'Paste Color From Clipboard'
      OnClick = MenuItemPasteColorFromClipboardClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object MenuItemColor_Window: TMenuItem
      Caption = '$Color_Window$'
      OnClick = MenuItemColor_GenericClick
    end
    object MenuItemColor_BtnFace: TMenuItem
      Caption = '$Color_BtnFace$'
      OnClick = MenuItemColor_GenericClick
    end
    object MenuItemColor_ActiveCaption: TMenuItem
      Caption = '$Color_ActiveCaption$'
      OnClick = MenuItemColor_GenericClick
    end
    object MenuItemColor_InactiveCaption: TMenuItem
      Caption = '$Color_InactiveCaption$'
      OnClick = MenuItemColor_GenericClick
    end
    object MenuItemColor_Highlight: TMenuItem
      Caption = '$Color_Highlight$'
      OnClick = MenuItemColor_GenericClick
    end
    object MenuItemColor_WindowText: TMenuItem
      Caption = '$Color_WindowText$'
      OnClick = MenuItemColor_GenericClick
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object MenuItemColor_GrayText: TMenuItem
      Caption = '$Color_GrayText$'
      OnClick = MenuItemColor_GenericClick
    end
    object MenuItemColor_GradientActiveCaption: TMenuItem
      Caption = '$Color_GradientActiveCaption$'
      OnClick = MenuItemColor_GenericClick
    end
    object MenuItemColor_GradientInactiveCaption: TMenuItem
      Caption = '$Color_GradientInactiveCaption$'
      OnClick = MenuItemColor_GenericClick
    end
    object MenuItemColor_ScrollBar: TMenuItem
      Caption = '$Color_ScrollBar$'
      OnClick = MenuItemColor_GenericClick
    end
    object MenuItemColor_3DDkShadow: TMenuItem
      Caption = '$Color_3DDkShadow$'
      OnClick = MenuItemColor_GenericClick
    end
    object MenuItemColor_3DLight: TMenuItem
      Caption = '$Color_3DLight$'
      OnClick = MenuItemColor_GenericClick
    end
    object MenuItemColor_WindowFrame: TMenuItem
      Caption = '$Color_WindowFrame$'
      OnClick = MenuItemColor_GenericClick
    end
  end
  object tmrUpdateCropEditBoxes: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrUpdateCropEditBoxesTimer
    Left = 81
  end
  object tmrStartup: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrStartupTimer
    Left = 224
    Top = 8
  end
end
