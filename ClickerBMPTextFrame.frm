object frClickerBMPText: TfrClickerBMPText
  Left = 0
  Height = 158
  Top = 0
  Width = 434
  ClientHeight = 158
  ClientWidth = 434
  TabOrder = 0
  DesignLeft = 86
  DesignTop = 85
  object scrboxPreview: TScrollBox
    Left = 0
    Height = 89
    Top = 14
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
    TabOrder = 0
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
    Left = 0
    Height = 15
    Hint = 'Click this text to refresh the preview bitmap.'
    Top = 0
    Width = 41
    Caption = 'Preview'
    ParentShowHint = False
    ShowHint = True
    OnClick = lblPreviewClick
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
  object tmrUpdateCropEditBoxes: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrUpdateCropEditBoxesTimer
    Left = 64
    Top = 80
  end
  object tmrStartup: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrStartupTimer
    Left = 200
    Top = 94
  end
end
