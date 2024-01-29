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


unit ClickerRemoteScreenForm;

{$mode ObjFPC}{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Menus, ClickerUtils, IniFiles, Types;

type
  { TfrmClickerRemoteScreen }

  TfrmClickerRemoteScreen = class(TForm)
    btnRefreshScreenshot: TButton;
    cmbMouseTool: TComboBox;
    edtHandle: TEdit;
    imgScreenshot: TImage;
    lblMouseTool: TLabel;
    lblHandle: TLabel;
    lblInfo: TLabel;
    lbeCompText: TLabeledEdit;
    lbeCompClass: TLabeledEdit;
    MenuItem_RefreshAfterMouseUp: TMenuItem;
    pmExtraRefresh: TPopupMenu;
    scrboxScannedComponents: TScrollBox;
    spdbtnExtraRefresh: TSpeedButton;
    procedure btnRefreshScreenshotClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure imgScreenshotMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgScreenshotMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure scrboxScannedComponentsMouseWheel(Sender: TObject;
      Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure spdbtnExtraRefreshClick(Sender: TObject);
  private
    FOnGetConnectionAddress: TOnGetConnectionAddress;
    FSelectedComponentHandle: THandle;
    FSelectedComponentText: string;
    FSelectedComponentClassName: string;
    FRemoteScreenWidth: Integer;
    FRemoteScreenHeight: Integer;
    FRemoteSelectedRect: TRect;

    FSelectedComponentLeftLimitLabel: TLabel;
    FSelectedComponentTopLimitLabel: TLabel;
    FSelectedComponentRightLimitLabel: TLabel;
    FSelectedComponentBottomLimitLabel: TLabel;

    function DoOnGetConnectionAddress: string;
    function GetRemoteComponent(X, Y: Integer): Boolean;
    procedure RefreshScreenshot;

    procedure CreateRemainingComponents;
  public
    procedure LoadSettings(AIni: TMemIniFile);
    procedure SaveSettings(AIni: TMemIniFile);

    property SelectedComponentHandle: THandle read FSelectedComponentHandle;
    property SelectedComponentText: string read FSelectedComponentText;
    property SelectedComponentClassName: string read FSelectedComponentClassName;
    property OnGetConnectionAddress: TOnGetConnectionAddress read FOnGetConnectionAddress write FOnGetConnectionAddress;
  end;

var
  frmClickerRemoteScreen: TfrmClickerRemoteScreen;

implementation

{$R *.frm}

uses
  ClickerActionsClient, MouseStuff;

{ TfrmClickerRemoteScreen }


procedure TfrmClickerRemoteScreen.LoadSettings(AIni: TMemIniFile);
begin
  Left := AIni.ReadInteger('RemoteScreenWindow', 'Left', Min(Left, Screen.DesktopWidth - 60));
  Top := AIni.ReadInteger('RemoteScreenWindow', 'Top', Min(Top, Screen.DesktopHeight - 60));
  Width := AIni.ReadInteger('RemoteScreenWindow', 'Width', Min(Width, Screen.DesktopWidth - 40));
  Height := AIni.ReadInteger('RemoteScreenWindow', 'Height', Min(Height, Screen.DesktopHeight - 40));
end;


procedure TfrmClickerRemoteScreen.SaveSettings(AIni: TMemIniFile);
begin
  AIni.WriteInteger('RemoteScreenWindow', 'Left', Min(Left, Screen.DesktopWidth - 60));
  AIni.WriteInteger('RemoteScreenWindow', 'Top', Min(Top, Screen.DesktopHeight - 60));
  AIni.WriteInteger('RemoteScreenWindow', 'Width', Min(Width, Screen.DesktopWidth - 40));
  AIni.WriteInteger('RemoteScreenWindow', 'Height', Min(Height, Screen.DesktopHeight - 40));
end;


function TfrmClickerRemoteScreen.DoOnGetConnectionAddress: string;
begin
  if not Assigned(FOnGetConnectionAddress) then
  begin
    Result := '';
    Exit;
  end;

  Result := FOnGetConnectionAddress();
end;


procedure TfrmClickerRemoteScreen.RefreshScreenshot;
var
  ServerAddress: string;
begin
  btnRefreshScreenshot.Enabled := False;
  try
    if imgScreenshot.Tag = 0 then
    begin
      if GetRemoteComponent(0, 0) then
        imgScreenshot.Tag := 1;
    end;

    if FRemoteScreenWidth > 0 then
      imgScreenshot.Width := FRemoteScreenWidth;

    if FRemoteScreenWidth > 0 then
      imgScreenshot.Height := FRemoteScreenHeight;

    ServerAddress := DoOnGetConnectionAddress;
    if ServerAddress = '' then
    begin
      imgScreenshot.Canvas.Pen.Color := clWhite;
      imgScreenshot.Canvas.Brush.Color := clWhite;
      imgScreenshot.Canvas.Rectangle(0, 0, imgScreenshot.Width - 1, imgScreenshot.Height - 1);
      imgScreenshot.Canvas.Font.Color := clRed;
      imgScreenshot.Canvas.Font.Size := 8;

      imgScreenshot.Canvas.TextOut(0, 0, 'OnGetConnectionAddress');
      imgScreenshot.Canvas.TextOut(0, 15, 'not assinged.');
      Exit;
    end;

    GetScreenShotImageFromServer(ServerAddress, imgScreenshot.Picture.Bitmap);
  finally
    btnRefreshScreenshot.Enabled := True;
  end;
end;


procedure TfrmClickerRemoteScreen.btnRefreshScreenshotClick(Sender: TObject);
begin
  RefreshScreenshot;
end;


procedure TfrmClickerRemoteScreen.CreateRemainingComponents;
begin
  FSelectedComponentLeftLimitLabel := TLabel.Create(Self);
  FSelectedComponentTopLimitLabel := TLabel.Create(Self);
  FSelectedComponentRightLimitLabel := TLabel.Create(Self);
  FSelectedComponentBottomLimitLabel := TLabel.Create(Self);

  FSelectedComponentLeftLimitLabel.Parent := scrboxScannedComponents;
  FSelectedComponentTopLimitLabel.Parent := scrboxScannedComponents;
  FSelectedComponentRightLimitLabel.Parent := scrboxScannedComponents;
  FSelectedComponentBottomLimitLabel.Parent := scrboxScannedComponents;

  FSelectedComponentLeftLimitLabel.AutoSize := False;
  FSelectedComponentTopLimitLabel.AutoSize := False;
  FSelectedComponentRightLimitLabel.AutoSize := False;
  FSelectedComponentBottomLimitLabel.AutoSize := False;

  FSelectedComponentLeftLimitLabel.Caption := '';
  FSelectedComponentTopLimitLabel.Caption := '';
  FSelectedComponentRightLimitLabel.Caption := '';
  FSelectedComponentBottomLimitLabel.Caption := '';

  FSelectedComponentLeftLimitLabel.Color := CLabel_Orange;
  FSelectedComponentTopLimitLabel.Color := CLabel_Orange;
  FSelectedComponentRightLimitLabel.Color := CLabel_Orange;
  FSelectedComponentBottomLimitLabel.Color := CLabel_Orange;

  FSelectedComponentLeftLimitLabel.Width := 1;
  FSelectedComponentTopLimitLabel.Height := 1;
  FSelectedComponentRightLimitLabel.Width := 1;
  FSelectedComponentBottomLimitLabel.Height := 1;

  FSelectedComponentLeftLimitLabel.Transparent := False;
  FSelectedComponentTopLimitLabel.Transparent := False;
  FSelectedComponentRightLimitLabel.Transparent := False;
  FSelectedComponentBottomLimitLabel.Transparent := False;

  FSelectedComponentLeftLimitLabel.BringToFront;
  FSelectedComponentTopLimitLabel.BringToFront;
  FSelectedComponentRightLimitLabel.BringToFront;
  FSelectedComponentBottomLimitLabel.BringToFront;
end;


procedure TfrmClickerRemoteScreen.FormCreate(Sender: TObject);
begin
  FOnGetConnectionAddress := nil;
  FRemoteScreenWidth := -1;
  FRemoteScreenHeight := -1;
  FSelectedComponentHandle := 0;

  imgScreenshot.Left := 0;
  imgScreenshot.Top := 0;

  CreateRemainingComponents;
end;


function TfrmClickerRemoteScreen.GetRemoteComponent(X, Y: Integer): Boolean;
var
  Response: TStringList;
  ServerAddress, ResponseStatus: string;
begin
  Result := False;

  ServerAddress := DoOnGetConnectionAddress;
  if ServerAddress = '' then
  begin
    lbeCompText.Text := 'undefined';
    lbeCompClass.Text := 'undefined';
    edtHandle.Text := '';
    Exit;
  end;

  Response := TStringList.Create;
  try
    Response.Text := FastReplace_87ToReturn(GetCompInfoAtPoint(ServerAddress, X, Y));

    ResponseStatus := Response.Values[CREResp_ErrParam];
    if ResponseStatus <> CREResp_ErrResponseOK then
    begin
      MessageBox(Handle, PChar('Error getting component details: ' + ResponseStatus), PChar(Application.Title), MB_ICONERROR);
      Exit;
    end;

    edtHandle.Text := Response.Values[CREResp_HandleParam];
    lbeCompText.Text := Response.Values[CREResp_TextParam];
    lbeCompClass.Text := Response.Values[CREResp_ClassParam];
    FRemoteScreenWidth := StrToIntDef(Response.Values[CREResp_ScreenWidth], -1);
    FRemoteScreenHeight := StrToIntDef(Response.Values[CREResp_ScreenHeight], -1);

    FRemoteSelectedRect.Left := StrToIntDef(Response.Values[CREResp_CompLeft], 0);
    FRemoteSelectedRect.Top := StrToIntDef(Response.Values[CREResp_CompTop], 0);
    FRemoteSelectedRect.Width := StrToIntDef(Response.Values[CREResp_CompWidth], 1);
    FRemoteSelectedRect.Height := StrToIntDef(Response.Values[CREResp_CompHeight], 1);

    FSelectedComponentHandle := StrToIntDef(Response.Values[CREResp_HandleParam], 0);
    FSelectedComponentText := lbeCompText.Text;
    FSelectedComponentClassName := lbeCompClass.Text;

    lblHandle.Hint := 'Left: ' + IntToStr(FRemoteSelectedRect.Left) + #13#10 +
                      'Top: ' + IntToStr(FRemoteSelectedRect.Top) + #13#10 +
                      'Width: ' + IntToStr(FRemoteSelectedRect.Width) + #13#10 +
                      'Height: ' + IntToStr(FRemoteSelectedRect.Height);

    edtHandle.Hint := lblHandle.Hint;
  finally
    Response.Free;
  end;

  Result := True;
end;


procedure TfrmClickerRemoteScreen.imgScreenshotMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MouseParams: TStringList;
begin
  case cmbMouseTool.ItemIndex of
    0:   //component selection
    begin;
      if Button <> mbLeft then
        Exit; //if FDragging then

      GetRemoteComponent(X, Y);

      FSelectedComponentLeftLimitLabel.Left := FRemoteSelectedRect.Left;
      FSelectedComponentTopLimitLabel.Top := FRemoteSelectedRect.Top;
      FSelectedComponentRightLimitLabel.Left := FRemoteSelectedRect.Right;
      FSelectedComponentBottomLimitLabel.Top := FRemoteSelectedRect.Bottom;

      FSelectedComponentLeftLimitLabel.Height := FRemoteScreenHeight;
      FSelectedComponentTopLimitLabel.Width := FRemoteScreenWidth;
      FSelectedComponentRightLimitLabel.Height := FRemoteScreenHeight;
      FSelectedComponentBottomLimitLabel.Width := FRemoteScreenWidth;
    end;

    1:   //remote click
    begin
      MouseParams := TStringList.Create;
      try
        MouseParams.Values[CMouseX] := IntToStr(X);
        MouseParams.Values[CMouseY] := IntToStr(Y);
        MouseParams.Values[CMouseButton] := IntToStr(Ord(Button));
        SendMouseDown(DoOnGetConnectionAddress, MouseParams);
      finally
        MouseParams.Free;
      end;
    end;
  end; //case
end;


procedure TfrmClickerRemoteScreen.imgScreenshotMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MouseParams: TStringList;
begin
  case cmbMouseTool.ItemIndex of
    0:   //component selection
    begin

    end;

    1:   //remote click
    begin
      MouseParams := TStringList.Create;
      try
        MouseParams.Values[CMouseX] := IntToStr(X);
        MouseParams.Values[CMouseY] := IntToStr(Y);
        MouseParams.Values[CMouseButton] := IntToStr(Ord(Button));
        SendMouseUp(DoOnGetConnectionAddress, MouseParams);
      finally
        MouseParams.Free;
      end;

      if pmExtraRefresh.Items.Items[0].Checked then
        RefreshScreenshot;
    end;
  end; //case
end;


procedure TfrmClickerRemoteScreen.scrboxScannedComponentsMouseWheel(
  Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  Factor: Integer;
begin
  if ssCtrl in Shift then
    Factor := 1
  else
    Factor := 3;

  if ssShift in Shift then
    scrboxScannedComponents.HorzScrollBar.Position := scrboxScannedComponents.HorzScrollBar.Position - WheelDelta div Factor
  else
    scrboxScannedComponents.VertScrollBar.Position := scrboxScannedComponents.VertScrollBar.Position - WheelDelta div Factor;

  Handled := True;
end;


procedure TfrmClickerRemoteScreen.spdbtnExtraRefreshClick(Sender: TObject);
var
  tp: TPoint;
begin
  GetCursorPos(tp);
  pmExtraRefresh.PopUp(tp.X, tp.Y);
end;


end.

