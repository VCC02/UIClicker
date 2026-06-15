{
    Copyright (C) 2026 VCC
    creation date: 14 Jun 2026
    initial release date: 14 Jun 2026

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


unit VCCLogo;

interface


uses
  {$IFDEF Windows}
    Windows
  {$ELSE}
    LCLIntf, LCLType
  {$ENDIF}
  , Forms, StdCtrls, ExtCtrls;


procedure AddImgLogoToForm(AForm: TForm; AAdditionalMemo: TMemo = nil);


implementation


uses
  Graphics;


type
  TFormHandlers = class
  private
    FOwnerForm: TForm;
    FAdditionalMemo: TMemo;
    procedure HandleOnDblClick(Sender: TObject);
  end;


var
  imgLogo: TImage;
  Handlers: TFormHandlers;


procedure TFormHandlers.HandleOnDblClick(Sender: TObject);
const
  CLogoColors: array[0..31] of TColor = (
    $E9C677,
    $E1C275,
    $DEBE73,
    $E1BA71,
    $E2B66E,
    $DCB46B,
    $D6AF68,
    $D7AA66,
    $D9A764,
    $D3A461,
    $D2A060,
    $D09C5E,
    $CF985C,
    $CC9459,
    $C99156,
    $C88D52,
    $C78A4F,
    $C6874D,
    $C2824B,
    $C17F4A,
    $BF7947,
    $BF7747,
    $BB7343,
    $BD7042,
    $B86E3C,
    $D1985C,
    $E4BC74,
    $D8AF66,
    $D0A05D,
    $C98F55,
    $C0804B,
    $BC7C4F
  );
var
  i: Integer;
  s: string;
begin
  {$IFDEF Windows}
    if GetAsyncKeyState(VK_ESCAPE) < 0 then
  {$ELSE}
    if GetKeyState(VK_ESCAPE) < 0 then
  {$ENDIF}
    if imgLogo = nil then
    begin
      imgLogo := TImage.Create(Handlers.FOwnerForm);
      imgLogo.Visible := False;
      imgLogo.Parent := Handlers.FOwnerForm;
      imgLogo.Width := 32;
      imgLogo.Height := 32;
      imgLogo.Left := 4;
      imgLogo.Top := 4;

      imgLogo.Picture.Bitmap.Width := 32;
      imgLogo.Picture.Bitmap.Height := 32;
      imgLogo.Picture.Bitmap.PixelFormat := pf24bit;
      imgLogo.Picture.Bitmap.Canvas.Brush.Color := clWhite;
      imgLogo.Picture.Bitmap.Canvas.Pen.Color := clWhite;
      imgLogo.Picture.Bitmap.Canvas.Rectangle(0, 0, imgLogo.Width, imgLogo.Height);

      for i := 0 to 31 do
      begin
        imgLogo.Picture.Bitmap.Canvas.Pen.Color := CLogoColors[i];
        imgLogo.Picture.Bitmap.Canvas.MoveTo(i, 0);
        imgLogo.Picture.Bitmap.Canvas.LineTo(i, 31);
      end;

      s := 'Author: VCC';

      imgLogo.Hint := s;
      imgLogo.Hint := imgLogo.Hint + #13#10;

      if Handlers.FAdditionalMemo <> nil then
        Handlers.FAdditionalMemo.Lines.Add(s);

      s := 'https://github.com/VCC02/UIClicker/';

      imgLogo.Hint := imgLogo.Hint + s;

      if Handlers.FAdditionalMemo <> nil then
        Handlers.FAdditionalMemo.Lines.Add(s);

      imgLogo.Visible := True;
      imgLogo.ShowHint := True;
    end;
end;


procedure AddImgLogoToForm(AForm: TForm; AAdditionalMemo: TMemo = nil);
begin
  Handlers.FOwnerForm := AForm;
  Handlers.FAdditionalMemo := AAdditionalMemo;
  AForm.OnDblClick := Handlers.HandleOnDblClick;
end;


initialization
  imgLogo := nil;
  Handlers := TFormHandlers.Create;
  Handlers.FAdditionalMemo := nil;

finalization
  Handlers.Free;
end.
