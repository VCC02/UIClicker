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


unit ClickerWinInterpForm;

{$mode ObjFPC}{$H+}

interface


uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Menus, Buttons, VirtualTrees, IniFiles,
  ClickerUtils, ClickerWinInterpFrame;

type

  { TfrmClickerWinInterp }

  TfrmClickerWinInterp = class(TForm)
    procedure FormCreate(Sender: TObject);

  private
    FWinInterpFrame: TfrClickerWinInterp;

    function GetOnGetConnectionAddress: TOnGetConnectionAddress;
    procedure SetOnGetConnectionAddress(Value: TOnGetConnectionAddress);

    function GetOnGetSelectedCompFromRemoteWin: TOnGetSelectedCompFromRemoteWin;
    procedure SetOnGetSelectedCompFromRemoteWin(Value: TOnGetSelectedCompFromRemoteWin);

    function GetOnInsertTreeComponent: TOnInsertTreeComponent;
    procedure SetOnInsertTreeComponent(Value: TOnInsertTreeComponent);

    function GetOnClearWinInterp: TOnClearWinInterp;
    procedure SetOnClearWinInterp(Value: TOnClearWinInterp);

    function GetOnOpenDialogExecute: TOnOpenDialogExecute;
    procedure SetOnOpenDialogExecute(Value: TOnOpenDialogExecute);
    function GetOnGetOpenDialogFileName: TOnGetOpenDialogFileName;
    procedure SetOnGetOpenDialogFileName(Value: TOnGetOpenDialogFileName);
    function GetOnSaveDialogExecute: TOnOpenDialogExecute;
    procedure SetOnSaveDialogExecute(Value: TOnOpenDialogExecute);
    function GetOnGetSaveDialogFileName: TOnGetOpenDialogFileName;
    procedure SetOnGetSaveDialogFileName(Value: TOnGetOpenDialogFileName);

    function GetOnLoadFileFromStream: TOnLoadFileFromStream;
    procedure SetOnLoadFileFromStream(Value: TOnLoadFileFromStream);
    function GetOnSaveFileToStream: TOnLoadFileFromStream;
    procedure SetOnSaveFileToStream(Value: TOnLoadFileFromStream);
    function GetOnFileExists: TOnFileExists;
    procedure SetOnFileExists(Value: TOnFileExists);

    function GetSelectedComponentText: string;
    function GetSelectedComponentClassName: string;
  public
    procedure LoadSettings(AIni: TMemIniFile);
    procedure SaveSettings(AIni: TMemIniFile);

    procedure GetTreeContent(AStream: TMemoryStream);
    procedure RecordComponent(AInterprettedHandle: THandle; var ImgMatrix: TColorArr; var ImgHWMatrix: THandleArr; AStep: Integer = 1);
    procedure RecordWithMouseSwipe(AInterprettedHandle: THandle; AStep: Integer = 1);
    procedure GetCurrentlyRecordedScreenShotImage(ABmp: TBitmap);
    procedure SaveImages(ABasePath: string);

    property SelectedComponentText: string read GetSelectedComponentText;
    property SelectedComponentClassName: string read GetSelectedComponentClassName;

    property OnGetConnectionAddress: TOnGetConnectionAddress read GetOnGetConnectionAddress write SetOnGetConnectionAddress;
    property OnGetSelectedCompFromRemoteWin: TOnGetSelectedCompFromRemoteWin read GetOnGetSelectedCompFromRemoteWin write SetOnGetSelectedCompFromRemoteWin;
    property OnInsertTreeComponent: TOnInsertTreeComponent read GetOnInsertTreeComponent write SetOnInsertTreeComponent;
    property OnClearWinInterp: TOnClearWinInterp read GetOnClearWinInterp write SetOnClearWinInterp;

    property OnOpenDialogExecute: TOnOpenDialogExecute read GetOnOpenDialogExecute write SetOnOpenDialogExecute;
    property OnGetOpenDialogFileName: TOnGetOpenDialogFileName read GetOnGetOpenDialogFileName write SetOnGetOpenDialogFileName;
    property OnSaveDialogExecute: TOnOpenDialogExecute read GetOnSaveDialogExecute write SetOnSaveDialogExecute;
    property OnGetSaveDialogFileName: TOnGetOpenDialogFileName read GetOnGetSaveDialogFileName write SetOnGetSaveDialogFileName;

    property OnLoadFileFromStream: TOnLoadFileFromStream read GetOnLoadFileFromStream write SetOnLoadFileFromStream;
    property OnSaveFileToStream: TOnLoadFileFromStream read GetOnSaveFileToStream write SetOnSaveFileToStream;
    property OnFileExists: TOnFileExists read GetOnFileExists write SetOnFileExists;
  end;

var
  frmClickerWinInterp: TfrmClickerWinInterp;

implementation

{$R *.frm}

uses
  Math;

{ TfrmClickerWinInterp }

procedure TfrmClickerWinInterp.FormCreate(Sender: TObject);
begin
  FWinInterpFrame := TfrClickerWinInterp.Create(Self);
  FWinInterpFrame.Left := 0;
  FWinInterpFrame.Top := 0;
  FWinInterpFrame.Width := Width;
  FWinInterpFrame.Height := Height;
  FWinInterpFrame.Parent := Self;
  FWinInterpFrame.Anchors := [akLeft, akTop, akRight, akBottom];
  FWinInterpFrame.Visible := True;
end;


function TfrmClickerWinInterp.GetOnGetConnectionAddress: TOnGetConnectionAddress;
begin
  Result := FWinInterpFrame.OnGetConnectionAddress;
end;


procedure TfrmClickerWinInterp.SetOnGetConnectionAddress(Value: TOnGetConnectionAddress);
begin
  FWinInterpFrame.OnGetConnectionAddress := Value;
end;


function TfrmClickerWinInterp.GetOnGetSelectedCompFromRemoteWin: TOnGetSelectedCompFromRemoteWin;
begin
  Result := FWinInterpFrame.OnGetSelectedCompFromRemoteWin;
end;


procedure TfrmClickerWinInterp.SetOnGetSelectedCompFromRemoteWin(Value: TOnGetSelectedCompFromRemoteWin);
begin
  FWinInterpFrame.OnGetSelectedCompFromRemoteWin := Value;
end;


function TfrmClickerWinInterp.GetOnInsertTreeComponent: TOnInsertTreeComponent;
begin
  Result := FWinInterpFrame.OnInsertTreeComponent;
end;


procedure TfrmClickerWinInterp.SetOnInsertTreeComponent(Value: TOnInsertTreeComponent);
begin
  FWinInterpFrame.OnInsertTreeComponent := Value;
end;


function TfrmClickerWinInterp.GetOnClearWinInterp: TOnClearWinInterp;
begin
  Result := FWinInterpFrame.OnClearWinInterp;
end;


procedure TfrmClickerWinInterp.SetOnClearWinInterp(Value: TOnClearWinInterp);
begin
  FWinInterpFrame.OnClearWinInterp := Value;
end;


function TfrmClickerWinInterp.GetOnOpenDialogExecute: TOnOpenDialogExecute;
begin
  Result := FWinInterpFrame.OnOpenDialogExecute;
end;


procedure TfrmClickerWinInterp.SetOnOpenDialogExecute(Value: TOnOpenDialogExecute);
begin
  FWinInterpFrame.OnOpenDialogExecute := Value;
end;


function TfrmClickerWinInterp.GetOnGetOpenDialogFileName: TOnGetOpenDialogFileName;
begin
  Result := FWinInterpFrame.OnGetOpenDialogFileName;
end;


procedure TfrmClickerWinInterp.SetOnGetOpenDialogFileName(Value: TOnGetOpenDialogFileName);
begin
  FWinInterpFrame.OnGetOpenDialogFileName := Value;
end;


function TfrmClickerWinInterp.GetOnSaveDialogExecute: TOnOpenDialogExecute;
begin
  Result := FWinInterpFrame.OnSaveDialogExecute;
end;


procedure TfrmClickerWinInterp.SetOnSaveDialogExecute(Value: TOnOpenDialogExecute);
begin
  FWinInterpFrame.OnSaveDialogExecute := Value;
end;


function TfrmClickerWinInterp.GetOnGetSaveDialogFileName: TOnGetOpenDialogFileName;
begin
  Result := FWinInterpFrame.OnGetSaveDialogFileName;
end;


procedure TfrmClickerWinInterp.SetOnGetSaveDialogFileName(Value: TOnGetOpenDialogFileName);
begin
  FWinInterpFrame.OnGetSaveDialogFileName := Value;
end;


function TfrmClickerWinInterp.GetOnLoadFileFromStream: TOnLoadFileFromStream;
begin
  Result := FWinInterpFrame.OnLoadFileFromStream;
end;


procedure TfrmClickerWinInterp.SetOnLoadFileFromStream(Value: TOnLoadFileFromStream);
begin
  FWinInterpFrame.OnLoadFileFromStream := Value;
end;


function TfrmClickerWinInterp.GetOnSaveFileToStream: TOnLoadFileFromStream;
begin
  Result := FWinInterpFrame.OnSaveFileToStream;
end;


procedure TfrmClickerWinInterp.SetOnSaveFileToStream(Value: TOnLoadFileFromStream);
begin
  FWinInterpFrame.OnSaveFileToStream := Value;
end;


function TfrmClickerWinInterp.GetOnFileExists: TOnFileExists;
begin
  Result := FWinInterpFrame.OnFileExists;
end;


procedure TfrmClickerWinInterp.SetOnFileExists(Value: TOnFileExists);
begin
  FWinInterpFrame.OnFileExists := Value;
end;


function TfrmClickerWinInterp.GetSelectedComponentText: string;
begin
  Result := FWinInterpFrame.SelectedComponentText;
end;


function TfrmClickerWinInterp.GetSelectedComponentClassName: string;
begin
  Result := FWinInterpFrame.SelectedComponentClassName;
end;


procedure TfrmClickerWinInterp.LoadSettings(AIni: TMemIniFile);
begin
  Left := AIni.ReadInteger('WinInterpWindow', 'Left', Min(Left, Screen.DesktopWidth - 60));
  Top := AIni.ReadInteger('WinInterpWindow', 'Top', Min(Top, Screen.DesktopHeight - 60));
  Width := AIni.ReadInteger('WinInterpWindow', 'Width', Min(Width, Screen.DesktopWidth - 40));
  Height := AIni.ReadInteger('WinInterpWindow', 'Height', Min(Height, Screen.DesktopHeight - 40));

  FWinInterpFrame.LoadSettings(AIni);
end;


procedure TfrmClickerWinInterp.SaveSettings(AIni: TMemIniFile);
begin
  AIni.WriteInteger('WinInterpWindow', 'Left', Min(Left, Screen.DesktopWidth - 60));
  AIni.WriteInteger('WinInterpWindow', 'Top', Min(Top, Screen.DesktopHeight - 60));
  AIni.WriteInteger('WinInterpWindow', 'Width', Min(Width, Screen.DesktopWidth - 40));
  AIni.WriteInteger('WinInterpWindow', 'Height', Min(Height, Screen.DesktopHeight - 40));

  FWinInterpFrame.SaveSettings(AIni);
end;


procedure TfrmClickerWinInterp.GetTreeContent(AStream: TMemoryStream);
begin
  FWinInterpFrame.GetTreeContent(AStream);
end;


procedure TfrmClickerWinInterp.RecordComponent(AInterprettedHandle: THandle; var ImgMatrix: TColorArr; var ImgHWMatrix: THandleArr; AStep: Integer = 1);
begin
  FWinInterpFrame.RecordComponent(AInterprettedHandle, ImgMatrix, ImgHWMatrix, AStep);
end;


procedure TfrmClickerWinInterp.RecordWithMouseSwipe(AInterprettedHandle: THandle; AStep: Integer = 1);
begin
  FWinInterpFrame.RecordWithMouseSwipe(AInterprettedHandle, AStep);
end;


procedure TfrmClickerWinInterp.GetCurrentlyRecordedScreenShotImage(ABmp: TBitmap);
begin
  FWinInterpFrame.GetCurrentlyRecordedScreenShotImage(ABmp);
end;


procedure TfrmClickerWinInterp.SaveImages(ABasePath: string);
begin
  FWinInterpFrame.SaveImages(ABasePath);
end;


end.

