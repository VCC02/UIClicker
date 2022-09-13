object frmClickerTemplateNotes: TfrmClickerTemplateNotes
  Left = 387
  Height = 240
  Top = 43
  Width = 320
  Caption = 'Clicker Template Notes'
  ClientHeight = 240
  ClientWidth = 320
  LCLVersion = '7.5'
  object memNotes: TMemo
    Left = 8
    Height = 194
    Top = 8
    Width = 302
    Anchors = [akTop, akLeft, akRight, akBottom]
    Font.CharSet = ANSI_CHARSET
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Pitch = fpFixed
    Font.Quality = fqDraft
    ParentFont = False
    TabOrder = 0
    WordWrap = False
  end
  object btnOK: TButton
    Left = 80
    Height = 25
    Top = 208
    Width = 75
    Anchors = [akBottom]
    Caption = 'OK'
    OnClick = btnOKClick
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 168
    Height = 25
    Top = 208
    Width = 75
    Anchors = [akBottom]
    Caption = 'Cancel'
    OnClick = btnCancelClick
    TabOrder = 2
  end
end
