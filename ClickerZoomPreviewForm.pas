unit ClickerZoomPreviewForm;

{$mode Delphi}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TfrmClickerZoomPreview }

  TfrmClickerZoomPreview = class(TForm)
    imgZoom: TImage;
  private

  public

  end;


procedure SetZoomContent(ABitmap: TBitmap; AXCenter, AYCenter, AWinPosX, AWinPosY: Integer);
procedure ShowZoom(AWinPosX, AWinPosY: Integer);
procedure HideZoom;


var
  frmClickerZoomPreview: TfrmClickerZoomPreview;


implementation

{$R *.frm}


uses
  BitmapProcessing;


procedure SetZoomContent(ABitmap: TBitmap; AXCenter, AYCenter, AWinPosX, AWinPosY: Integer);
var
  DestRect: TRect;
  CroppedBmp: TBitmap;
begin
  DestRect.Left := 0;
  DestRect.Top := 0;
  DestRect.Right := frmClickerZoomPreview.imgZoom.Width shl 3;
  DestRect.Bottom := frmClickerZoomPreview.imgZoom.Height shl 3;

  WipeImage(frmClickerZoomPreview.imgZoom, frmClickerZoomPreview.imgZoom.Width, frmClickerZoomPreview.imgZoom.Height);

  CroppedBmp := TBitmap.Create;
  try
    CroppedBmp.Width := frmClickerZoomPreview.imgZoom.Width;
    CroppedBmp.Height := frmClickerZoomPreview.imgZoom.Height;
    WipeBitmap(CroppedBmp, CroppedBmp.Width, CroppedBmp.Height);
    BitBlt(CroppedBmp.Canvas.Handle, 0, 0, CroppedBmp.Width, CroppedBmp.Height, ABitmap.Canvas.Handle, AXCenter - CroppedBmp.Width shr 4, AYCenter - CroppedBmp.Height shr 4, SRCCOPY);

      //BitBlt param definition
      //HDC hdcDest, // handle to destination DC
      //int nXDest,  // x-coord of destination upper-left corner
      //int nYDest,  // y-coord of destination upper-left corner
      //int nWidth,  // width of destination rectangle
      //int nHeight, // height of destination rectangle
      //HDC hdcSrc,  // handle to source DC
      //int nXSrc,   // x-coordinate of source upper-left corner
      //int nYSrc,   // y-coordinate of source upper-left corner
      //DWORD dwRop  // raster operation code

    frmClickerZoomPreview.imgZoom.Canvas.StretchDraw(DestRect, CroppedBmp);
  finally
    CroppedBmp.Free;
  end;

  frmClickerZoomPreview.Left := AWinPosX;
  frmClickerZoomPreview.Top := AWinPosY;
end;


procedure ShowZoom(AWinPosX, AWinPosY: Integer);
begin
  frmClickerZoomPreview.Show;
end;


procedure HideZoom;
begin
  frmClickerZoomPreview.Hide;
end;

end.

