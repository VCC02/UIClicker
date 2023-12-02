{
    Copyright (C) 2023 VCC
    creation date: 19 Oct 2023
    initial release date: 19 Oct 2023

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


unit GradientTextMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, IdSchedulerOfThreadPool, IdHTTPServer, IdCustomTCPServer, IdContext,
  IdCustomHTTPServer, IdSync, IdCoderMIME;

type

  { TfrmGradientTextMain }

  TfrmGradientTextMain = class(TForm)
    btnResetTrackBar: TButton;
    chkKeepAlive: TCheckBox;
    chkServerActive: TCheckBox;
    edtButtonClickDuration: TEdit;
    edtTrackBarDragDuration: TEdit;
    grpRenderingServer: TGroupBox;
    grpButtonClickDuration: TGroupBox;
    grpTrackBarDragDuration: TGroupBox;
    grpClickTesting: TGroupBox;
    IdDecoderMIME1: TIdDecoderMIME;
    IdHTTPServer1: TIdHTTPServer;
    IdSchedulerOfThreadPool1: TIdSchedulerOfThreadPool;
    imgBrowserRendering: TImage;
    imgGradient: TImage;
    lblCustomText: TLabel;
    lbeServerModePort: TLabeledEdit;
    lblServerInfo: TLabel;
    trbTestTrackBar: TTrackBar;
    procedure btnResetTrackBarClick(Sender: TObject);
    procedure btnResetTrackBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnResetTrackBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chkServerActiveChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure IdHTTPServer1CommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure IdHTTPServer1Connect(AContext: TIdContext);
    procedure IdHTTPServer1Exception(AContext: TIdContext; AException: Exception
      );
    procedure trbTestTrackBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure trbTestTrackBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    procedure ProcessGetImageRequest(AParams: TStrings; ADestBmp: TBitmap; ASrcImg: TImage);
    function ProcessPutRenderingResult(AIn: string): string;
  public

  end;


  TSyncObj = class(TIdSync)
    FParams: TStrings;
    FDestBmp: TBitmap;
    FSrcImg: TImage;
  protected
    procedure DoSynchronize; override;
  end;


  TSyncPutRenderingResult = class(TIdSync)
    FData: string;
    FResult: string;
  protected
    procedure DoSynchronize; override;
  end;


const
  CRECmd_GetGradientImage = 'GetGradientImage';     //returns the yellow-fuchsia image  (used for testing)
  CRECmd_GetRenderingPage = 'GetRenderingPage';     //returns the html+js page. This request is made by a web browser, to render an image on page
  CRECmd_SetRenderingResult = 'SetRenderingResult'; //The web browser sends this request, to give back the rendering result to this server.
  CRECmd_GetRenderingImage = 'GetRenderingImage';   //Returns the rendered image (the one set by browser).
                                                    //The above commands (and protocol) should be replaced by some websocket-based protoocol.
  CRECmd_StartBrowserWithRenderingPage = 'StartBrowserWithRenderingPage';

var
  frmGradientTextMain: TfrmGradientTextMain;

implementation

{$R *.frm}


uses
  ClickerUtils, ShellApi;

var
  ButtonTk: QWord;
  TrackBarTk: QWord;


{TSyncObj}

procedure TSyncObj.DoSynchronize;
begin
  frmGradientTextMain.lblCustomText.Visible := FParams.Values['MatchText'] <> '';

  frmGradientTextMain.lblCustomText.Caption := FParams.Values['MatchText'];
  frmGradientTextMain.lblCustomText.Font.Color := HexToInt(FParams.Values['MatchBitmapText[0].ForegroundColor']);
  frmGradientTextMain.lblCustomText.Color := HexToInt(FParams.Values['MatchBitmapText[0].BackgroundColor']);
  frmGradientTextMain.lblCustomText.Font.Name := FParams.Values['MatchBitmapText[0].FontName'];
  frmGradientTextMain.lblCustomText.Font.Size := StrToIntDef(FParams.Values['MatchBitmapText[0].FontSize'], 8);
  frmGradientTextMain.lblCustomText.Font.Bold := FParams.Values['MatchBitmapText[0].Bold'] = '1';
  frmGradientTextMain.lblCustomText.Font.Italic := FParams.Values['MatchBitmapText[0].Italic'] = '1';
  frmGradientTextMain.lblCustomText.Font.Underline := FParams.Values['MatchBitmapText[0].Underline'] = '1';
  frmGradientTextMain.lblCustomText.Font.StrikeThrough := FParams.Values['MatchBitmapText[0].StrikeOut'] = '1';
  frmGradientTextMain.lblCustomText.Font.Quality := TFontQuality(StrToIntDef(FParams.Values['MatchBitmapText[0].FontQuality'], 3));

  frmGradientTextMain.lblCustomText.Transparent := frmGradientTextMain.lblCustomText.Color = $1FFFFFFF;

  frmGradientTextMain.lblCustomText.Hint := FParams.Text;
  frmGradientTextMain.lblCustomText.ShowHint := True;

  if FSrcImg = frmGradientTextMain.imgGradient then
    FDestBmp.Assign(FSrcImg.Picture.Bitmap);  //copy content from gradient image

  if FSrcImg = frmGradientTextMain.imgBrowserRendering then
  begin
    FDestBmp.Width := FSrcImg.Width;
    FDestBmp.Height := FSrcImg.Height;
    FSrcImg.Picture.PNG.TransparentColor := clWhite;
    FDestBmp.Canvas.Brush.Color := clWhite;
    FDestBmp.Canvas.Draw(0, 0, FSrcImg.Picture.PNG);  //copy content from rendered image
  end;
end;


{TSyncPutRenderingResult}

procedure TSyncPutRenderingResult.DoSynchronize;
var
  TempStream: TMemoryStream;
begin
  FResult := 'Image received';

  TempStream := TMemoryStream.Create;
  try
    try
      frmGradientTextMain.IdDecoderMIME1.DecodeStream(FData, TempStream);
      TempStream.Position := 0;
      frmGradientTextMain.imgBrowserRendering.Picture.LoadFromStream(TempStream);
    except
      on E: Exception do
      begin
        FResult := E.Message;
        frmGradientTextMain.imgBrowserRendering.Picture.Bitmap := TBitmap.Create;
        frmGradientTextMain.imgBrowserRendering.Picture.Bitmap.Width := frmGradientTextMain.imgBrowserRendering.Width;
        frmGradientTextMain.imgBrowserRendering.Picture.Bitmap.Height := frmGradientTextMain.imgBrowserRendering.Height;
        frmGradientTextMain.imgBrowserRendering.Picture.Bitmap.Canvas.Brush.Color := clWhite;
        frmGradientTextMain.imgBrowserRendering.Picture.Bitmap.Canvas.Font.Color := clRed;
        frmGradientTextMain.imgBrowserRendering.Picture.Bitmap.Canvas.TextOut(10, 10, E.Message)
      end;
    end;
  finally
    TempStream.Free;
  end;
end;


{ TfrmGradientTextMain }

procedure TfrmGradientTextMain.FormCreate(Sender: TObject);
begin
  imgGradient.Canvas.Font.Color := clBlack;
  imgGradient.Canvas.Font.Name := 'Tahoma';
  imgGradient.Canvas.Font.Size := 36;
  imgGradient.Canvas.Font.Quality := fqNonAntialiased;
  imgGradient.Canvas.Brush.Style := bsClear;
  imgGradient.Canvas.TextOut(10, 150, 'This is the searched text.');
end;


procedure TfrmGradientTextMain.ProcessGetImageRequest(AParams: TStrings; ADestBmp: TBitmap; ASrcImg: TImage);
var
  SyncObj: TSyncObj;
begin
  SyncObj := TSyncObj.Create;
  try
    SyncObj.FParams := AParams;
    SyncObj.FDestBmp := ADestBmp;
    SyncObj.FSrcImg := ASrcImg;
    SyncObj.Synchronize;
  finally
    SyncObj.Free;
  end;
end;


function TfrmGradientTextMain.ProcessPutRenderingResult(AIn: string): string;
var
  SyncObj: TSyncPutRenderingResult;
begin
  SyncObj := TSyncPutRenderingResult.Create;
  try
    SyncObj.FData := Copy(AIn, Length('data:image/png;base64,') + 1, MaxInt);
    SyncObj.Synchronize;
    Result := SyncObj.FResult;
  finally
    SyncObj.Free;
  end;
end;


procedure GetRenderingPage(AStream: TStream);
var
  Fnm: string;
  TempStream: TMemoryStream;
begin
  Fnm := ExtractFilePath(ParamStr(0)) + 'TestRenderingPage.html';
  if not FileExists(Fnm) then
    Exit;

  TempStream := TMemoryStream.Create;
  try
    TempStream.LoadFromFile(Fnm);
    TempStream.Position := 0;
    AStream.Position := 0;
    AStream.CopyFrom(TempStream, TempStream.Size);
  finally
    TempStream.Clear;
  end;
end;


procedure TfrmGradientTextMain.IdHTTPServer1CommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  GettingImage: Boolean;
  Bmp: TBitmap;
  Cmd: string;
begin
  Cmd := ARequestInfo.Document;

  AResponseInfo.ContentType := 'text/plain'; // 'text/html';  default type

  if Cmd = '/' + CRECmd_GetRenderingPage then
  begin
    AResponseInfo.ContentText := '';  //maybe not needed
    AResponseInfo.ContentType := 'text/html';

    if AResponseInfo.ContentStream = nil then
      AResponseInfo.ContentStream := TMemoryStream.Create;

    GetRenderingPage(AResponseInfo.ContentStream);
    Exit;
  end;

  if Pos('/' + CRECmd_SetRenderingResult, Cmd) = 1 then
  begin
    AResponseInfo.ContentType := 'text/html';
    AResponseInfo.ContentText := ProcessPutRenderingResult(Copy(Cmd, Length('/' + CRECmd_SetRenderingResult + '/') + 1, MaxInt));
    Exit;
  end;

  if Cmd = '/' + CRECmd_StartBrowserWithRenderingPage then
  begin
    AResponseInfo.ContentText := 'OK';  //maybe not needed
    ShellExecute(Handle, 'open', PChar('http://127.0.0.1:' + lbeServerModePort.Text + '/' + CRECmd_GetRenderingPage), '', '', 5);  //SW_SHOW
  end;


  GettingImage := False;
  Bmp := nil; //set here, in case ProcessServerCommand handles it

  if Cmd = '/' + CRECmd_GetGradientImage then
  begin
    Bmp := TBitmap.Create;
    GettingImage := True;
    ProcessGetImageRequest(ARequestInfo.Params, Bmp, imgGradient);
  end;

  if Cmd = '/' + CRECmd_GetRenderingImage then
  begin
    Bmp := TBitmap.Create;
    GettingImage := True;
    ProcessGetImageRequest(ARequestInfo.Params, Bmp, imgBrowserRendering);
  end;

  try
    if GettingImage then
    begin
      AResponseInfo.ContentText := '';

      if AResponseInfo.ContentText <> '' then  //old logic, from UIClicker
      begin
        Bmp.Width := Bmp.Canvas.TextWidth(AResponseInfo.ContentText) + 10;
        Bmp.Height := 20;
        Bmp.Canvas.Brush.Color := clWhite;
        Bmp.Canvas.Font.Color := $000000AA;
        Bmp.Canvas.TextOut(3, 2, AResponseInfo.ContentText);
        AResponseInfo.ContentText := ''; //reset this, to send the response in the proper format
      end;

      AResponseInfo.ContentType := 'image/bmp'; //'application/octet-stream';
      AResponseInfo.ContentDisposition := 'inline'; //display it in browser
      AResponseInfo.CharSet := 'US-ASCII';  //this is actually required, to prevent converting ASCII characters from 128-255 to '?'

      AResponseInfo.ContentStream := TMemoryStream.Create;
      try
        Bmp.SaveToStream(AResponseInfo.ContentStream);

        AResponseInfo.ContentLength := AResponseInfo.ContentStream.Size;

        AResponseInfo.WriteHeader;
        AResponseInfo.WriteContent;
      finally
        AResponseInfo.ContentStream.Free;
        AResponseInfo.ContentStream := nil;
      end;

      Exit; //useful if there are other commands after this
    end;
  finally
    if GettingImage then
      Bmp.Free;
  end;
end;


procedure TfrmGradientTextMain.IdHTTPServer1Connect(AContext: TIdContext);
begin
  AContext.Connection.Socket.ReadTimeout := 3600000;   //if no bytes are received in 1h, then close the connection
end;


procedure TfrmGradientTextMain.IdHTTPServer1Exception(AContext: TIdContext;
  AException: Exception);
begin
  try
    if AException.Message <> 'Connection Closed Gracefully.' then
      //AddToLogFromThread('Server exception: ' + AException.Message);
  except
  end;
end;


procedure TfrmGradientTextMain.trbTestTrackBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TrackBarTk := GetTickCount64;
end;


procedure TfrmGradientTextMain.trbTestTrackBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  edtTrackBarDragDuration.Text := IntToStr(GetTickCount64 - TrackBarTk);
end;


procedure TfrmGradientTextMain.btnResetTrackBarClick(Sender: TObject);
begin
  trbTestTrackBar.Position := 0;
end;


procedure TfrmGradientTextMain.btnResetTrackBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ButtonTk := GetTickCount64;
end;


procedure TfrmGradientTextMain.btnResetTrackBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  edtButtonClickDuration.Text := IntToStr(GetTickCount64 - ButtonTk);
end;


procedure TfrmGradientTextMain.chkServerActiveChange(Sender: TObject);
var
  s: string;
begin
  if chkServerActive.Checked then
  begin
    try
      IdHTTPServer1.DefaultPort := StrToIntDef(lbeServerModePort.Text, 5444);
      IdHTTPServer1.KeepAlive := chkKeepAlive.Checked;
      IdHTTPServer1.Active := True;

      s := 'Server is listening on port ' + IntToStr(IdHTTPServer1.DefaultPort);

      lblServerInfo.Caption := s;
      lblServerInfo.Font.Color := clGreen;
      lblServerInfo.Hint := '';
    except
      on E: Exception do
      begin
        lblServerInfo.Caption := E.Message;
        lblServerInfo.Font.Color := $000000BB;

        if E.Message = 'Could not bind socket.' then
        begin
          lblServerInfo.Caption := lblServerInfo.Caption + '  (hover for hint)';
          lblServerInfo.Hint := 'Make sure there is no other instance of UIClicker or other application listening on the port.';
          lblServerInfo.Hint := lblServerInfo.Hint + #13#10 + 'If there is another application, started by UIClicker in server mode, with inherited handles, it may keep the socket in use.';
        end;
      end;
    end;
  end
  else
  begin
    IdHTTPServer1.Active := False;
    lblServerInfo.Caption := 'Server module is inactive';
    lblServerInfo.Font.Color := clGray;
    lblServerInfo.Hint := '';
  end;
end;

end.

