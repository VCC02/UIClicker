object frmClickerConditionEditor: TfrmClickerConditionEditor
  Left = 387
  Height = 319
  Top = 43
  Width = 1027
  Caption = 'Clicker Condition Editor'
  ClientHeight = 319
  ClientWidth = 1027
  Constraints.MinHeight = 263
  Constraints.MinWidth = 888
  OnClose = FormClose
  OnCreate = FormCreate
  LCLVersion = '8.4'
  object pnlActionConditions: TPanel
    Left = 0
    Height = 272
    Top = 0
    Width = 1022
    Anchors = [akTop, akLeft, akRight, akBottom]
    ParentColor = False
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 432
    Height = 25
    Top = 288
    Width = 75
    Anchors = [akBottom]
    Caption = 'OK'
    OnClick = btnOKClick
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 520
    Height = 25
    Top = 288
    Width = 75
    Anchors = [akBottom]
    Caption = 'Cancel'
    OnClick = btnCancelClick
    TabOrder = 2
  end
end
