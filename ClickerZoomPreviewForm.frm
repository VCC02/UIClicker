object frmClickerZoomPreview: TfrmClickerZoomPreview
  Left = 242
  Height = 242
  Top = 227
  Width = 242
  BorderStyle = bsNone
  Caption = 'Clicker Zoom'
  ClientHeight = 242
  ClientWidth = 242
  Color = clLime
  FormStyle = fsStayOnTop
  OnCreate = FormCreate
  LCLVersion = '7.9'
  object imgZoom: TImage
    Left = 1
    Height = 240
    Top = 1
    Width = 240
  end
  object tmrStartup: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrStartupTimer
    Left = 82
    Top = 44
  end
end
