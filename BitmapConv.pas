{
    Copyright (C) 2023 VCC
    creation date: 07 Dec 2023
    initial release date: 07 Dec 2023

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


unit BitmapConv;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF Windows}
    Windows, ShellAPI,
  {$ENDIF}
  SysUtils, Graphics, ExtCtrls;


procedure WipeBitmap(ABitmap: TBitmap; NewWidth, NewHeight: Integer);
procedure WipeImage(AImg: TImage; NewWidth, NewHeight: Integer);

procedure DrawExeIconOnBmp(ABitmap: TBitmap; AFileName: string);
procedure DrawPngOnBmp(ABitmap: TBitmap; AFileName: string);
procedure DrawJpgOnBmp(ABitmap: TBitmap; AFileName: string);
procedure DrawIcoOnBmp(ABitmap: TBitmap; AFileName: string);


implementation


procedure DrawWipeRect(ACanvas: TCanvas; NewWidth, NewHeight: Integer);
begin
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := clWhite;
  ACanvas.Pen.Color := clWhite;
  ACanvas.Rectangle(0, 0, NewWidth {- 1}, NewHeight {- 1});
end;


procedure WipeBitmap(ABitmap: TBitmap; NewWidth, NewHeight: Integer);
begin
  ABitmap.Clear;
  ABitmap.Width := NewWidth;
  ABitmap.Height := NewHeight;
  DrawWipeRect(ABitmap.Canvas, NewWidth, NewHeight);
end;


procedure WipeImage(AImg: TImage; NewWidth, NewHeight: Integer);
begin
  AImg.Picture.Clear;
  AImg.Width := NewWidth;
  AImg.Height := NewHeight;
  AImg.Picture.Bitmap.Width := AImg.Width;
  AImg.Picture.Bitmap.Height := AImg.Height;

  DrawWipeRect(AImg.Canvas, NewWidth, NewHeight);
  AImg.Repaint;
end;


procedure DrawExeIconOnBmp(ABitmap: TBitmap; AFileName: string);
var
  IconHandle: THandle;
begin
  WipeBitmap(ABitmap, 32, 32);

  {$IFDEF Windows}
    IconHandle := ExtractIcon(HINSTANCE, PChar(AFileName), 0);
    try
      if IconHandle > 1 then
      begin
        if not DrawIcon(ABitmap.Canvas.Handle, 0, 0, IconHandle) then
        begin
          ABitmap.Canvas.Font.Color := clYellow;
          ABitmap.Canvas.Brush.Color := clGreen;
          ABitmap.Canvas.TextOut(1, 1, 'SysErr:');
          ABitmap.Canvas.TextOut(1, 16, IntToStr(GetLastError));
        end;
      end
      else
      begin
        ABitmap.Canvas.Font.Color := clFuchsia;
        ABitmap.Canvas.Brush.Color := clYellow;
        ABitmap.Canvas.TextOut(1, 1, 'Handle:');
        ABitmap.Canvas.TextOut(1, 16, IntToStr(IconHandle));
      end;
    finally
      DestroyIcon(IconHandle);
    end;
  {$ELSE}
    ABitmap.Canvas.Font.Color := clLime;
    ABitmap.Canvas.Brush.Color := clGreen;
    ABitmap.Canvas.TextOut(1, 1, 'MSWin');
    ABitmap.Canvas.TextOut(1, 16, 'Only');
  {$ENDIF}
end;


procedure DrawPngOnBmp(ABitmap: TBitmap; AFileName: string);
var
  TempPng: TPNGImage;
begin
  TempPng := TPNGImage.Create;
  try
    TempPng.LoadFromFile(AFileName);
    WipeBitmap(ABitmap, TempPng.Width, TempPng.Height);

    ABitmap.Canvas.Draw(0, 0, TempPng);
  finally
    TempPng.Free;
  end;
end;


procedure DrawJpgOnBmp(ABitmap: TBitmap; AFileName: string);
var
  TempJpg: TJPEGImage;
begin
  TempJpg := TJPEGImage.Create;
  try
    TempJpg.LoadFromFile(AFileName);
    WipeBitmap(ABitmap, TempJpg.Width, TempJpg.Height);

    ABitmap.Canvas.Draw(0, 0, TempJpg);
  finally
    TempJpg.Free;
  end;
end;


procedure DrawIcoOnBmp(ABitmap: TBitmap; AFileName: string);
var
  TempIco: TIcon;
begin
  TempIco := TIcon.Create;
  try
    TempIco.LoadFromFile(AFileName);
    WipeBitmap(ABitmap, TempIco.Width, TempIco.Height);

    ABitmap.Canvas.Draw(0, 0, TempIco);
  finally
    TempIco.Free;
  end;
end;


end.

