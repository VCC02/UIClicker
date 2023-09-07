unit ClickerZoomPreviewForm;

{$mode Delphi}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TfrmClickerZoomPreview }

  TfrmClickerZoomPreview = class(TForm)
    imgZoom: TImage;
    tmrStartup: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
  private
    FlblLeft: TLabel;
    FlblTop: TLabel;
    FlblRight: TLabel;
    FlblBottom: TLabel;
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
  Factor: Integer;
begin
  DestRect.Left := 0;
  DestRect.Top := 0;
  DestRect.Right := frmClickerZoomPreview.imgZoom.Width shl 3;
  DestRect.Bottom := frmClickerZoomPreview.imgZoom.Height shl 3;
  Factor := frmClickerZoomPreview.imgZoom.Width shr 4;

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

    if AXCenter - CroppedBmp.Width shr 4 < 0 then
    begin
      frmClickerZoomPreview.imgZoom.Canvas.Pen.Color := $A0FFFF; //light yellow
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Style := bsSolid;
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Color := $A0FFFF; //light yellow
      frmClickerZoomPreview.imgZoom.Canvas.Rectangle(0, 0, (CroppedBmp.Width shr 4 - AXCenter) shl 3, frmClickerZoomPreview.imgZoom.Height);

      frmClickerZoomPreview.imgZoom.Canvas.Pen.Color := $A0FFFF; //light yellow
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Style := bsFDiagonal;
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Color := $008888FF;
      frmClickerZoomPreview.imgZoom.Canvas.Rectangle(0, 0, (CroppedBmp.Width shr 4 - AXCenter) shl 3, frmClickerZoomPreview.imgZoom.Height);
    end;

    if AYCenter - CroppedBmp.Height shr 4 < 0 then
    begin
      frmClickerZoomPreview.imgZoom.Canvas.Pen.Color := $A0FFFF; //light yellow
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Style := bsSolid;
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Color := $A0FFFF; //light yellow
      frmClickerZoomPreview.imgZoom.Canvas.Rectangle(0, 0, frmClickerZoomPreview.imgZoom.Height, (CroppedBmp.Height shr 4 - AYCenter) shl 3);

      frmClickerZoomPreview.imgZoom.Canvas.Pen.Color := $A0FFFF; //light yellow
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Style := bsFDiagonal;
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Color := $008888FF;
      frmClickerZoomPreview.imgZoom.Canvas.Rectangle(0, 0, frmClickerZoomPreview.imgZoom.Height, (CroppedBmp.Height shr 4 - AYCenter) shl 3);
    end;

    if ABitmap.Width - AXCenter < Factor then
    begin
      frmClickerZoomPreview.imgZoom.Canvas.Pen.Color := $A0FFFF; //light yellow
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Style := bsSolid;
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Color := $A0FFFF; //light yellow
      frmClickerZoomPreview.imgZoom.Canvas.Rectangle((ABitmap.Width - AXCenter + Factor) shl 3, 0, CroppedBmp.Width, frmClickerZoomPreview.imgZoom.Height);

      frmClickerZoomPreview.imgZoom.Canvas.Pen.Color := $A0FFFF; //light yellow
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Style := bsFDiagonal;
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Color := $008888FF;
      frmClickerZoomPreview.imgZoom.Canvas.Rectangle((ABitmap.Width - AXCenter + Factor) shl 3, 0, CroppedBmp.Width, frmClickerZoomPreview.imgZoom.Height);
    end;

    if ABitmap.Height - AYCenter < Factor then
    begin
      frmClickerZoomPreview.imgZoom.Canvas.Pen.Color := $A0FFFF; //light yellow
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Style := bsSolid;
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Color := $A0FFFF; //light yellow
      frmClickerZoomPreview.imgZoom.Canvas.Rectangle(0, (ABitmap.Height - AYCenter + Factor) shl 3, frmClickerZoomPreview.imgZoom.Width, CroppedBmp.Height);

      frmClickerZoomPreview.imgZoom.Canvas.Pen.Color := $A0FFFF; //light yellow
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Style := bsFDiagonal;
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Color := $008888FF;
      frmClickerZoomPreview.imgZoom.Canvas.Rectangle(0, (ABitmap.Height - AYCenter + Factor) shl 3, frmClickerZoomPreview.imgZoom.Width, CroppedBmp.Height);
    end;
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

{ TfrmClickerZoomPreview }

procedure TfrmClickerZoomPreview.FormCreate(Sender: TObject);
begin
  tmrStartup.Enabled := True;
end;


procedure TfrmClickerZoomPreview.tmrStartupTimer(Sender: TObject);
const
  CLabelColor: TColor = $00FF8855;
begin
  tmrStartup.Enabled := False;

  FlblLeft := TLabel.Create(Self);
  FlblTop := TLabel.Create(Self);
  FlblRight := TLabel.Create(Self);
  FlblBottom := TLabel.Create(Self);

  FlblLeft := TLabel.Create(Self);
  FlblTop := TLabel.Create(Self);
  FlblRight := TLabel.Create(Self);
  FlblBottom := TLabel.Create(Self);

  FlblLeft.Parent := Self;
  FlblTop.Parent := Self;
  FlblRight.Parent := Self;
  FlblBottom.Parent := Self;

  FlblLeft.AutoSize := False;
  FlblTop.AutoSize := False;
  FlblRight.AutoSize := False;
  FlblBottom.AutoSize := False;

  FlblLeft.Caption := '';
  FlblTop.Caption := '';
  FlblRight.Caption := '';
  FlblBottom.Caption := '';

  FlblLeft.Color := CLabelColor;
  FlblTop.Color := CLabelColor;
  FlblRight.Color := CLabelColor;
  FlblBottom.Color := CLabelColor;

  FlblLeft.Left := imgZoom.Left + imgZoom.Width shr 1 - 1;
  FlblLeft.Top := imgZoom.Top;
  FlblLeft.Width := 1;
  FlblLeft.Height := imgZoom.Height;

  FlblTop.Left := imgZoom.Left;
  FlblTop.Top := imgZoom.Top + imgZoom.Height shr 1 - 1;
  FlblTop.Width := imgZoom.Width;
  FlblTop.Height := 1;

  FlblRight.Left := imgZoom.Left + imgZoom.Width shr 1 + 8 - 1;
  FlblRight.Top := imgZoom.Top;
  FlblRight.Width := 1;
  FlblRight.Height := imgZoom.Height;

  FlblBottom.Left := imgZoom.Left;
  FlblBottom.Top := imgZoom.Top + imgZoom.Height shr 1 + 8 - 1;
  FlblBottom.Width := imgZoom.Width;
  FlblBottom.Height := 1;

  FlblLeft.Transparent := False;
  FlblTop.Transparent := False;
  FlblRight.Transparent := False;
  FlblBottom.Transparent := False;

  FlblLeft.Visible := True;
  FlblTop.Visible := True;
  FlblRight.Visible := True;
  FlblBottom.Visible := True;

  FlblLeft.BringToFront;
  FlblTop.BringToFront;
  FlblRight.BringToFront;
  FlblBottom.BringToFront;
end;

end.
