{
    Copyright (C) 2022 VCC
    creation date: Dec 2019
    initial release date: 13 Sep 2022

    author: VCC
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:
    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
    DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
    OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}


unit ClickerPreviewForm;

{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, {Messages,} SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ExtDlgs, IniFiles;

type

  { TfrmClickerControlPreview }

  TfrmClickerControlPreview = class(TForm)
    chkScanningTimer: TCheckBox;
    lblZoom: TLabel;
    tmrScan: TTimer;
    lblInstructions: TLabel;
    trbCropLeft: TTrackBar;
    trbCropRight: TTrackBar;
    lblCropImage: TLabel;
    lblCropLeft: TLabel;
    lblCropRight: TLabel;
    trbCropTop: TTrackBar;
    trbCropBottom: TTrackBar;
    lblCropTop: TLabel;
    lblCropBottom: TLabel;
    chkShowCropRectangle: TCheckBox;
    scrboxCrop: TScrollBox;
    imgCrop: TImage;
    trbCropZoom: TTrackBar;
    btnSaveImage: TButton;
    SavePictureDialog1: TSavePictureDialog;
    scrboxScreenshot: TScrollBox;
    pnlBase: TPanel;
    imgScreenshot: TImage;
    pnlCaptureWidth: TPanel;
    pnlCaptureHeight: TPanel;
    pnlSelLeft: TPanel;
    pnlSelTop: TPanel;
    pnlSelRight: TPanel;
    pnlSelBottom: TPanel;
    ColorDialog1: TColorDialog;
    btnSelectionRectangleColor: TButton;
    grpWinInfo: TGroupBox;
    lbeHandle: TLabeledEdit;
    lbeClass: TLabeledEdit;
    lbeText: TLabeledEdit;
    lbeRect: TLabeledEdit;
    btnSelectFull: TButton;
    lbeMouseXOffset: TLabeledEdit;
    lbeMouseYOffset: TLabeledEdit;
    tmrStartup: TTimer;
    btnCopyImageToClipboard: TButton;
    lbeMouseGX: TLabeledEdit;
    lbeMouseGY: TLabeledEdit;
    grpTestSetControlText: TGroupBox;
    lbeTestSetControlTextHandle: TLabeledEdit;
    btnTestSetControlText: TButton;
    rdgrpTestSetTextControlType: TRadioGroup;
    lbeTestSetControlTextNewText: TLabeledEdit;
    chkStayOnTop: TCheckBox;
    procedure chkScanningTimerChange(Sender: TObject);
    procedure tmrScanTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure trbCropLeftChange(Sender: TObject);
    procedure trbCropRightChange(Sender: TObject);
    procedure trbCropTopChange(Sender: TObject);
    procedure trbCropBottomChange(Sender: TObject);
    procedure chkShowCropRectangleClick(Sender: TObject);
    procedure trbCropZoomChange(Sender: TObject);
    procedure btnSaveImageClick(Sender: TObject);
    procedure imgScreenshotMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgScreenshotMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgScreenshotMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure btnSelectionRectangleColorClick(Sender: TObject);
    procedure btnSelectFullClick(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
    procedure btnCopyImageToClipboardClick(Sender: TObject);
    procedure btnTestSetControlTextClick(Sender: TObject);
    procedure chkStayOnTopClick(Sender: TObject);
  private
    { Private declarations }
    FSelecting: Boolean;
    FSelectingXStart: Integer;
    FSelectingYStart: Integer;

    pnlImgCoords: TPanel;

    procedure SetCropRectangleByTrb;
    procedure DrawCroppedImage;
  public
    { Public declarations }
    procedure LoadSettings(AIni: TMemIniFile);
    procedure SaveSettings(AIni: TMemIniFile);
  end;


var
  frmClickerControlPreview: TfrmClickerControlPreview;

implementation


uses
  Math, ControlInteraction, BitmapProcessing, ClickerUtils, Clipbrd;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.frm}
{$ENDIF}


procedure TfrmClickerControlPreview.LoadSettings(AIni: TMemIniFile);
begin
  Left := AIni.ReadInteger('PreviewWindow', 'Left', Left);
  Top := AIni.ReadInteger('PreviewWindow', 'Top', Top);
  Width := AIni.ReadInteger('PreviewWindow', 'Width', Width);
  Height := AIni.ReadInteger('PreviewWindow', 'Height', Height);
  chkStayOnTop.Checked := AIni.ReadBool('PreviewWindow', 'StayOnTop', chkStayOnTop.Checked);
end;


procedure TfrmClickerControlPreview.SaveSettings(AIni: TMemIniFile);
begin
  AIni.WriteInteger('PreviewWindow', 'Left', Left);
  AIni.WriteInteger('PreviewWindow', 'Top', Top);
  AIni.WriteInteger('PreviewWindow', 'Width', Width);
  AIni.WriteInteger('PreviewWindow', 'Height', Height);
  AIni.WriteBool('PreviewWindow', 'StayOnTop', chkStayOnTop.Checked);
end;


procedure TfrmClickerControlPreview.DrawCroppedImage;
var
  ACropWidth, ACropHeight: Integer;         
  ATempBitmap: TBitmap;
  StretchRect: TRect;
begin
  ACropWidth := trbCropRight.Position - trbCropLeft.Position;
  ACropHeight := trbCropBottom.Position - trbCropTop.Position;
                                                                     
  imgCrop.Width := ACropWidth shl trbCropZoom.Position;
  imgCrop.Height := ACropHeight shl trbCropZoom.Position;
  imgCrop.Stretch := True;

  imgCrop.Picture.Bitmap.Width := imgCrop.Width;
  imgCrop.Picture.Bitmap.Height := imgCrop.Height;

  ATempBitmap := TBitmap.Create;
  try
    ATempBitmap.Width := ACropWidth;
    ATempBitmap.Height := ACropHeight;
    ATempBitmap.PixelFormat := pf24bit;
    BitBlt(ATempBitmap.Canvas.Handle, 0, 0, ACropWidth, ACropHeight, imgScreenshot.Picture.Bitmap.Canvas.Handle, trbCropLeft.Position, trbCropTop.Position, SRCCOPY);

    StretchRect.Left := 0;
    StretchRect.Top := 0;
    StretchRect.Right := ACropWidth shl trbCropZoom.Position;
    StretchRect.Bottom := ACropHeight shl trbCropZoom.Position;
    imgCrop.Canvas.StretchDraw(StretchRect, ATempBitmap);
  finally
    ATempBitmap.Free;
  end;
end;           


procedure TfrmClickerControlPreview.SetCropRectangleByTrb;
begin
  pnlSelLeft.Left := imgScreenshot.Left + trbCropLeft.Position;
  pnlSelLeft.Height := trbCropBottom.Position - trbCropTop.Position;
  pnlSelLeft.Top := imgScreenshot.Top + trbCropTop.Position;

  pnlSelTop.Left := pnlSelLeft.Left;
  pnlSelTop.Width := trbCropRight.Position - trbCropLeft.Position;
  pnlSelTop.Top := pnlSelLeft.Top;

  pnlSelRight.Top := pnlSelLeft.Top;
  pnlSelRight.Left := imgScreenshot.Left + trbCropRight.Position;
  pnlSelRight.Height := pnlSelLeft.Height;

  pnlSelBottom.Left := pnlSelTop.Left;
  pnlSelBottom.Width := pnlSelTop.Width;
  pnlSelBottom.Top := imgScreenshot.Top + trbCropBottom.Position;
end;


procedure TfrmClickerControlPreview.btnCopyImageToClipboardClick(Sender: TObject);
begin
  Clipboard.Assign(imgCrop.Picture);
end;


procedure TfrmClickerControlPreview.btnSaveImageClick(Sender: TObject);
begin
  if not SavePictureDialog1.Execute then
    Exit;

  if UpperCase(ExtractFileExt(SavePictureDialog1.FileName)) <> '.BMP' then
    SavePictureDialog1.FileName := SavePictureDialog1.FileName + '.bmp';

  imgCrop.Picture.SaveToFile(SavePictureDialog1.FileName);
end;


procedure TfrmClickerControlPreview.btnSelectFullClick(Sender: TObject);
begin
  try
    trbCropLeft.Position := 0;
    trbCropRight.Position := trbCropRight.Max;
    trbCropTop.Position := 0;
    trbCropBottom.Position := trbCropBottom.Max;

    SetCropRectangleByTrb;
    DrawCroppedImage;
  except                      //there is a bug somewhere in the code above
    on E: Exception do
      MessageBox(Handle, PChar(E.Message), PChar(Caption), MB_ICONERROR);
  end;
end;


procedure TfrmClickerControlPreview.btnSelectionRectangleColorClick(Sender: TObject);
begin
  ColorDialog1.Color := pnlSelLeft.Color;
  if not ColorDialog1.Execute then
    Exit;

  pnlSelLeft.Color := ColorDialog1.Color;
  pnlSelTop.Color := ColorDialog1.Color;
  pnlSelRight.Color := ColorDialog1.Color;
  pnlSelBottom.Color := ColorDialog1.Color;
end;


procedure TfrmClickerControlPreview.btnTestSetControlTextClick(Sender: TObject);
var
  ControlHandle: THandle;
  NewText: string;
begin
  ControlHandle := StrToIntDef(lbeTestSetControlTextHandle.Text, 0);
  NewText := lbeTestSetControlTextNewText.Text;

  case rdgrpTestSetTextControlType.ItemIndex of
    0: SetControlText(ControlHandle, NewText);
    1: SelectComboBoxItem(ControlHandle, 0, NewText);
  end;
end;


procedure TfrmClickerControlPreview.chkShowCropRectangleClick(Sender: TObject);
begin
  pnlSelLeft.Visible := chkShowCropRectangle.Checked;
  pnlSelTop.Visible := chkShowCropRectangle.Checked;
  pnlSelRight.Visible := chkShowCropRectangle.Checked;
  pnlSelBottom.Visible := chkShowCropRectangle.Checked;

  SetCropRectangleByTrb;
  DrawCroppedImage;
end;


procedure TfrmClickerControlPreview.chkStayOnTopClick(Sender: TObject);
begin
  if chkStayOnTop.Checked then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;  
end;


procedure TfrmClickerControlPreview.FormCreate(Sender: TObject);
begin
  imgScreenshot.Canvas.Brush.Color := clWhite;
  imgScreenshot.Canvas.Rectangle(0, 0, imgScreenshot.Width - 1, imgScreenshot.Height - 1);
  imgScreenshot.Picture.Bitmap.Width := imgScreenshot.Width;
  imgScreenshot.Picture.Bitmap.Height := imgScreenshot.Height;

  pnlSelLeft.Width := 1;
  pnlSelTop.Height := 1;
  pnlSelRight.Width := 1;
  pnlSelBottom.Height := 1;

  if imgCrop.Picture.Bitmap = nil then
    imgCrop.Picture.Bitmap := TBitmap.Create;

  FSelecting := False;
  tmrStartup.Enabled := True;
  Application.HintHidePause := 20000;

  {$IFNDEF FPC}
    scrboxCrop.HorzScrollBar.Style := ssFlat;
    scrboxCrop.VertScrollBar.Style := ssFlat;
    scrboxScreenshot.HorzScrollBar.Style := ssFlat;
    scrboxScreenshot.VertScrollBar.Style := ssFlat;
  {$ENDIF}

  pnlImgCoords := TPanel.Create(Self);
  pnlImgCoords.Parent := grpWinInfo;
  pnlImgCoords.Left := 238;
  pnlImgCoords.Height := 18;
  pnlImgCoords.Top := -1;
  pnlImgCoords.Width := 118;
  pnlImgCoords.Caption := 'Coords';
end;


procedure TfrmClickerControlPreview.imgScreenshotMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FSelecting := True;
    
    trbCropLeft.Position := X;
    trbCropRight.Position := X;
    trbCropTop.Position := Y;
    trbCropBottom.Position := Y;

    FSelectingXStart := X;
    FSelectingYStart := Y;
  end;
end;


procedure TfrmClickerControlPreview.imgScreenshotMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if FSelecting then
  begin
    trbCropLeft.Position := Min(X, FSelectingXStart);
    trbCropRight.Position := Max(X, FSelectingXStart);
    trbCropTop.Position := Min(Y, FSelectingYStart);
    trbCropBottom.Position := Max(Y, FSelectingYStart);

    if (X <> FSelectingXStart) or (Y <> FSelectingYStart) then
      if not chkShowCropRectangle.Checked then
        chkShowCropRectangle.Checked := True;
  end;

  pnlImgCoords.Caption := IntToStr(X) + ' : ' + IntToStr(Y);
end;


procedure TfrmClickerControlPreview.imgScreenshotMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    FSelecting := False;
end;


procedure TfrmClickerControlPreview.tmrScanTimer(Sender: TObject);
var
  tp: TPoint;
  hwc: TCompRec;
  CompWidth, CompHeight: Integer;
begin
  if (GetAsyncKeyState(VK_CONTROL) < 0) and (GetAsyncKeyState(VK_SHIFT) < 0) then
  begin
    GetCursorPos(tp);
    lbeMouseGX.Text := IntToStr(tp.X);
    lbeMouseGY.Text := IntToStr(tp.Y);

    
    hwc := GetWindowClassRec(tp);

    lbeHandle.Text := IntToStr(hwc.Handle);
    lbeClass.Text := hwc.ClassName;
    lbeText.Text := hwc.Text;

    lbeRect.Text := ' Left=' + IntToStr(hwc.ComponentRectangle.Left) +
                    ' Top=' + IntToStr(hwc.ComponentRectangle.Top) +
                    ' Right=' + IntToStr(hwc.ComponentRectangle.Right) +
                    ' Bottom=' + IntToStr(hwc.ComponentRectangle.Bottom) +
                    ' Width=' + IntToStr(hwc.ComponentRectangle.Right - hwc.ComponentRectangle.Left) +
                    ' Height=' + IntToStr(hwc.ComponentRectangle.Bottom - hwc.ComponentRectangle.Top);

    lbeMouseXOffset.Text := IntToStr(hwc.MouseXOffset);
    lbeMouseYOffset.Text := IntToStr(hwc.MouseYOffset);            

    CompWidth := hwc.ComponentRectangle.Right - hwc.ComponentRectangle.Left;
    CompHeight := hwc.ComponentRectangle.Bottom - hwc.ComponentRectangle.Top;

    if CompWidth > 16383 then
      CompWidth := 16383;

    if CompHeight > 16383 then
      CompHeight := 16383;

    pnlCaptureWidth.Width := CompWidth;
    pnlCaptureHeight.Height := CompHeight;
    {
    if CompWidth < imgScreenshot.Width then
    begin
      imgScreenshot.Canvas.Brush.Color := clAqua;
      imgScreenshot.Canvas.Pen.Color := clAqua;
      imgScreenshot.Canvas.Rectangle(CompWidth, 0, imgScreenshot.Width - 1, CompHeight);
    end;

    if CompHeight < imgScreenshot.Height then
    begin
      imgScreenshot.Canvas.Brush.Color := clLime;
      imgScreenshot.Canvas.Pen.Color := clLime;
      imgScreenshot.Canvas.Rectangle(0, CompHeight, CompWidth, imgScreenshot.Height - 1);
    end;

    if (CompWidth < imgScreenshot.Width) and (CompHeight < imgScreenshot.Height) then
    begin
      imgScreenshot.Canvas.Brush.Color := clRed;
      imgScreenshot.Canvas.Pen.Color := clRed;
      imgScreenshot.Canvas.Rectangle(CompWidth, CompHeight, imgScreenshot.Width - 1, imgScreenshot.Height - 1);
    end;    }

    imgScreenshot.Width := CompWidth;             //AV here after debugging, then reloading a template while debugging and pressing Continue (AV), then pressing Ctrl key to enter here
    imgScreenshot.Height := CompHeight;
    pnlBase.Width := imgScreenshot.Left + imgScreenshot.Width + 5;
    pnlBase.Height := imgScreenshot.Top + imgScreenshot.Height + 5;

    ScreenShot(hwc.Handle, imgScreenshot.Picture.Bitmap, 0, 0, CompWidth, CompHeight);
    //imgScreenshot.Repaint;

    trbCropLeft.Max := CompWidth;
    trbCropRight.Max := CompWidth;
    trbCropTop.Max := CompHeight;
    trbCropBottom.Max := CompHeight;
  end;
end;


procedure TfrmClickerControlPreview.chkScanningTimerChange(Sender: TObject);
begin
  tmrScan.Enabled := chkScanningTimer.Checked;

  if chkScanningTimer.Checked then
    chkScanningTimer.Font.Color := $000000B9
  else
    chkScanningTimer.Font.Color := clDefault;
end;


procedure TfrmClickerControlPreview.tmrStartupTimer(Sender: TObject);
begin
  tmrStartup.Enabled := False;
end;


procedure TfrmClickerControlPreview.trbCropLeftChange(Sender: TObject);
begin
  if trbCropLeft.Position > trbCropRight.Position then
    trbCropLeft.Position := trbCropRight.Position;
    
  lblCropLeft.Caption := IntToStr(trbCropLeft.Position);
  SetCropRectangleByTrb;
  DrawCroppedImage;
end;


procedure TfrmClickerControlPreview.trbCropRightChange(Sender: TObject);
begin
  if trbCropRight.Position < trbCropLeft.Position then
    trbCropRight.Position := trbCropLeft.Position;
    
  lblCropRight.Caption := IntToStr(trbCropRight.Position);
  SetCropRectangleByTrb;
  DrawCroppedImage;
end;


procedure TfrmClickerControlPreview.trbCropTopChange(Sender: TObject);
begin
  if trbCropTop.Position > trbCropBottom.Position then
    trbCropTop.Position := trbCropBottom.Position;
    
  lblCropTop.Caption := IntToStr(trbCropTop.Position);
  SetCropRectangleByTrb;
  DrawCroppedImage;
end;


procedure TfrmClickerControlPreview.trbCropBottomChange(Sender: TObject);
begin
  if trbCropBottom.Position < trbCropTop.Position then
    trbCropBottom.Position := trbCropTop.Position;

  lblCropBottom.Caption := IntToStr(trbCropBottom.Position);
  SetCropRectangleByTrb;
  DrawCroppedImage;
end;


procedure TfrmClickerControlPreview.trbCropZoomChange(Sender: TObject);
begin
  DrawCroppedImage;
end;


end.
