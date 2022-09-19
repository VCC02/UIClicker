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


unit UIClickerMainFormFP;

{$H+}
{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, ExtDlgs, ClickerIniFiles;

type

  { TfrmUIClickerMainForm }

  TfrmUIClickerMainForm = class(TForm)
    bitbtnShowActionsForm: TBitBtn;
    bitbtnShowPreviewForm: TBitBtn;
    bitbtnShowRemoteScreenShotForm: TBitBtn;
    bitbtnShowTemplateCallTree: TBitBtn;
    bitbtnShowWinInterp: TBitBtn;
    lblBitness: TLabel;
    OpenDialog1: TOpenDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    SaveDialog1: TSaveDialog;
    tmrStartup: TTimer;
    procedure btnShowActionsFormClick(Sender: TObject);
    procedure btnShowPreviewFormClick(Sender: TObject);
    procedure btnShowRemoteScreenShotFormClick(Sender: TObject);
    procedure btnShowTemplateCallTreeClick(Sender: TObject);
    procedure btnShowWinInterpClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
  private
    FAllFormsAreCreated: Boolean;

    procedure LoadSettings;
    procedure SaveSettings;
    procedure HandleOnCopyControlTextAndClassFromMainWindow(ACompProvider: string; out AControlText, AControlClass: string);
    function HandleOnGetConnectionAddress: string;
    procedure HandleOnRecordComponent(ACompHandle: THandle; ATreeContentStream: TMemoryStream);
    function HandleOnGetSelectedCompFromRemoteWin: THandle;
    procedure HandleOnGetCurrentlyRecordedScreenShotImage(ABmp: TBitmap);

    function HandleOnFileExists(const FileName: string): Boolean;
    function HandleOnTClkIniReadonlyFileCreate(AFileName: string): TClkIniReadonlyFile;
    procedure HandleOnSaveTemplateToFile(AStringList: TStringList; const FileName: string);
    procedure HandleOnSetTemplateOpenDialogInitialDir(AInitialDir: string);
    function HandleOnTemplateOpenDialogExecute: Boolean;
    function HandleOnGetTemplateOpenDialogFileName: string;
    procedure HandleOnSetTemplateSaveDialogInitialDir(AInitialDir: string);
    function HandleOnTemplateSaveDialogExecute: Boolean;
    function HandleOnGetTemplateSaveDialogFileName: string;
    procedure HandleOnSetTemplateSaveDialogFileName(AFileName: string);
    procedure HandleOnSetPictureOpenDialogInitialDir(AInitialDir: string);
    function HandleOnPictureOpenDialogExecute: Boolean;
    function HandleOnGetPictureOpenDialogFileName: string;
    function HandleOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;

    procedure HandleOnTemplateOpenSetMultiSelect;
  public
    property AllFormsAreCreated: Boolean write FAllFormsAreCreated;
  end;

var
  frmUIClickerMainForm: TfrmUIClickerMainForm;

implementation

{$R *.frm}


uses
  ClickerPreviewForm, ClickerWinInterpForm, ClickerTemplateCallTreeForm, ClickerActionsForm,
  ClickerActionsClient, IniFiles, ClickerFindControlFrame, ClickerRemoteScreenForm;

{ TfrmUIClickerMainForm }


procedure TfrmUIClickerMainForm.LoadSettings;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Clicker.ini');
  try
    Left := Ini.ReadInteger('MainWindow', 'Left', Left);
    Top := Ini.ReadInteger('MainWindow', 'Top', Top);
    Width := Ini.ReadInteger('MainWindow', 'Width', Width);
    Height := Ini.ReadInteger('MainWindow', 'Height', Height);

    frmClickerControlPreview.LoadSettings(Ini);
    frmClickerActions.LoadSettings(Ini);
    frmClickerWinInterp.LoadSettings(Ini);
    frmClickerTemplateCallTree.LoadSettings(Ini);
    frmClickerRemoteScreen.LoadSettings(Ini);

    frmClickerActions.OnCopyControlTextAndClassFromMainWindow := HandleOnCopyControlTextAndClassFromMainWindow;
    frmClickerRemoteScreen.OnGetConnectionAddress := HandleOnGetConnectionAddress;
    frmClickerWinInterp.OnGetConnectionAddress := HandleOnGetConnectionAddress;
    frmClickerWinInterp.OnGetSelectedCompFromRemoteWin := HandleOnGetSelectedCompFromRemoteWin;

    frmClickerActions.OnRecordComponent := HandleOnRecordComponent;
    frmClickerActions.OnGetCurrentlyRecordedScreenShotImage := HandleOnGetCurrentlyRecordedScreenShotImage;
    frmClickerActions.OnFileExists := HandleOnFileExists;
    frmClickerActions.OnTClkIniReadonlyFileCreate := HandleOnTClkIniReadonlyFileCreate;
    frmClickerActions.OnSaveTemplateToFile := HandleOnSaveTemplateToFile;
    frmClickerActions.OnSetTemplateOpenDialogInitialDir := HandleOnSetTemplateOpenDialogInitialDir;
    frmClickerActions.OnTemplateOpenDialogExecute := HandleOnTemplateOpenDialogExecute;
    frmClickerActions.OnGetTemplateOpenDialogFileName := HandleOnGetTemplateOpenDialogFileName;
    frmClickerActions.OnSetTemplateSaveDialogInitialDir := HandleOnSetTemplateSaveDialogInitialDir;
    frmClickerActions.OnTemplateSaveDialogExecute := HandleOnTemplateSaveDialogExecute;
    frmClickerActions.OnGetTemplateSaveDialogFileName := HandleOnGetTemplateSaveDialogFileName;
    frmClickerActions.OnSetTemplateSaveDialogFileName := HandleOnSetTemplateSaveDialogFileName;
    frmClickerActions.OnSetPictureOpenDialogInitialDir := HandleOnSetPictureOpenDialogInitialDir;
    frmClickerActions.OnPictureOpenDialogExecute := HandleOnPictureOpenDialogExecute;
    frmClickerActions.OnGetPictureOpenDialogFileName := HandleOnGetPictureOpenDialogFileName;
    frmClickerActions.OnLoadBitmap := HandleOnLoadBitmap;

    frmClickerTemplateCallTree.OnTemplateOpenSetMultiSelect := HandleOnTemplateOpenSetMultiSelect;
    frmClickerTemplateCallTree.OnFileExists := HandleOnFileExists;
    frmClickerTemplateCallTree.OnTClkIniReadonlyFileCreate := HandleOnTClkIniReadonlyFileCreate;
    frmClickerTemplateCallTree.OnTemplateOpenDialogExecute := HandleOnTemplateOpenDialogExecute;
    frmClickerTemplateCallTree.OnGetTemplateOpenDialogFileName := HandleOnGetTemplateOpenDialogFileName;
  finally
    Ini.Free;
  end;
end;


procedure TfrmUIClickerMainForm.SaveSettings;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Clicker.ini');
  try
    Ini.WriteInteger('MainWindow', 'Left', Left);
    Ini.WriteInteger('MainWindow', 'Top', Top);
    Ini.WriteInteger('MainWindow', 'Width', Width);
    Ini.WriteInteger('MainWindow', 'Height', Height);

    frmClickerControlPreview.SaveSettings(Ini);
    frmClickerActions.SaveSettings(Ini);
    frmClickerWinInterp.SaveSettings(Ini);
    frmClickerTemplateCallTree.SaveSettings(Ini);
    frmClickerRemoteScreen.SaveSettings(Ini);

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


procedure TfrmUIClickerMainForm.btnShowPreviewFormClick(Sender: TObject);
begin
  frmClickerControlPreview.Show;
end;


procedure TfrmUIClickerMainForm.btnShowRemoteScreenShotFormClick(Sender: TObject);
begin
  frmClickerRemoteScreen.Show;
end;


procedure TfrmUIClickerMainForm.btnShowActionsFormClick(Sender: TObject);
begin
  frmClickerActions.Show;
end;


procedure TfrmUIClickerMainForm.btnShowWinInterpClick(Sender: TObject);
begin
  frmClickerWinInterp.Show;
end;


procedure TfrmUIClickerMainForm.btnShowTemplateCallTreeClick(Sender: TObject);
begin
  frmClickerTemplateCallTree.Show;
end;


procedure TfrmUIClickerMainForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  GeneralClosingApp := True;  //Prevent waiting for response loops to keep going.
  SaveSettings;
  Enabled := False; //A simple solution to a race condition, caused by multi-clicking the "X" button of this window, while stopping the server module.
end;


procedure TfrmUIClickerMainForm.FormCreate(Sender: TObject);
begin
  FAllFormsAreCreated := False;
  tmrStartup.Enabled := True;
end;


procedure TfrmUIClickerMainForm.tmrStartupTimer(Sender: TObject);
begin
  if not FAllFormsAreCreated then
    Exit;

  tmrStartup.Enabled := False;
  LoadSettings;

  if SizeOf(Pointer) = 4 then
    lblBitness.Caption := '32-bit'
  else
    lblBitness.Caption := '64-bit';
end;


procedure TfrmUIClickerMainForm.HandleOnCopyControlTextAndClassFromMainWindow(ACompProvider: string; out AControlText, AControlClass: string);
begin
  if ACompProvider = CPreviewWindow then
  begin
    AControlText := frmClickerControlPreview.lbeText.Text;
    AControlClass := frmClickerControlPreview.lbeClass.Text;
    Exit;
  end;

  if ACompProvider = CWinInterpWindow then
  begin
    AControlText := frmClickerWinInterp.SelectedComponentText;
    AControlClass := frmClickerWinInterp.SelectedComponentClassName;
    Exit;
  end;

  if ACompProvider = CRemoteScreenWindow then
  begin
    AControlText := frmClickerRemoteScreen.SelectedComponentText;
    AControlClass := frmClickerRemoteScreen.SelectedComponentClassName;
    Exit;
  end;

  AControlText := '<unknown text provider>';
  AControlClass := '<unknown class provider>';
end;


function TfrmUIClickerMainForm.HandleOnGetConnectionAddress: string;
begin
  Result := frmClickerActions.ConfiguredRemoteAddress; //not thread safe
end;


procedure TfrmUIClickerMainForm.HandleOnRecordComponent(ACompHandle: THandle; ATreeContentStream: TMemoryStream);
var
  ImgMatrix: TColorArr;
  ImgHWMatrix: THandleArr;
begin
  try
    frmClickerWinInterp.RecordComponent(ACompHandle, ImgMatrix, ImgHWMatrix);
  finally
    SetLength(ImgMatrix, 0);
    SetLength(ImgHWMatrix, 0);
  end;

  frmClickerWinInterp.GetTreeContent(ATreeContentStream);
end;


function TfrmUIClickerMainForm.HandleOnGetSelectedCompFromRemoteWin: THandle;
begin
  Result := frmClickerRemoteScreen.SelectedComponentHandle;
end;


procedure TfrmUIClickerMainForm.HandleOnGetCurrentlyRecordedScreenShotImage(ABmp: TBitmap);
begin
  frmClickerWinInterp.GetCurrentlyRecordedScreenShotImage(ABmp);
end;


function TfrmUIClickerMainForm.HandleOnFileExists(const FileName: string): Boolean;
begin
  Result := FileExists(FileName);
end;


function TfrmUIClickerMainForm.HandleOnTClkIniReadonlyFileCreate(AFileName: string): TClkIniReadonlyFile;
begin
  Result := TClkIniReadonlyFile.Create(AFileName);
end;


procedure TfrmUIClickerMainForm.HandleOnSaveTemplateToFile(AStringList: TStringList; const FileName: string);
begin
  AStringList.SaveToFile(FileName);
end;


procedure TfrmUIClickerMainForm.HandleOnSetTemplateOpenDialogInitialDir(AInitialDir: string);
begin
  OpenDialog1.InitialDir := AInitialDir;
end;


function TfrmUIClickerMainForm.HandleOnTemplateOpenDialogExecute: Boolean;
begin
  Result := OpenDialog1.Execute;
  OpenDialog1.Options := OpenDialog1.Options - [ofAllowMultiSelect];
end;


function TfrmUIClickerMainForm.HandleOnGetTemplateOpenDialogFileName: string;
begin
  if OpenDialog1.Files.Count > 1 then
    Result := OpenDialog1.Files.Text
  else
    Result := OpenDialog1.FileName;
end;


procedure TfrmUIClickerMainForm.HandleOnSetTemplateSaveDialogInitialDir(AInitialDir: string);
begin
  SaveDialog1.InitialDir := AInitialDir;
end;


function TfrmUIClickerMainForm.HandleOnTemplateSaveDialogExecute: Boolean;
begin
  Result := SaveDialog1.Execute;
end;


function TfrmUIClickerMainForm.HandleOnGetTemplateSaveDialogFileName: string;
begin
  Result := SaveDialog1.FileName;
end;


procedure TfrmUIClickerMainForm.HandleOnSetTemplateSaveDialogFileName(AFileName: string);
begin
  SaveDialog1.FileName := AFileName;
end;


procedure TfrmUIClickerMainForm.HandleOnSetPictureOpenDialogInitialDir(AInitialDir: string);
begin
  OpenPictureDialog1.InitialDir := AInitialDir;
end;


function TfrmUIClickerMainForm.HandleOnPictureOpenDialogExecute: Boolean;
begin
  Result := OpenPictureDialog1.Execute;
end;


function TfrmUIClickerMainForm.HandleOnGetPictureOpenDialogFileName: string;
begin
  Result := OpenPictureDialog1.FileName;
end;


function TfrmUIClickerMainForm.HandleOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if FileExists(AFileName) then
  begin
    ABitmap.LoadFromFile(AFileName);
    Result := True;
  end
  else
    Result := False;
end;


procedure TfrmUIClickerMainForm.HandleOnTemplateOpenSetMultiSelect;
begin
  OpenDialog1.Options := OpenDialog1.Options + [ofAllowMultiSelect];
end;

end.

