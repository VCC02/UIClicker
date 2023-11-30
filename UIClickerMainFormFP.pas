{
    Copyright (C) 2023 VCC
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
  Buttons, ExtDlgs, ClickerIniFiles, ClickerPrimitiveUtils;

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
    procedure ComposePrimitiveOnBmp(ABmp: TBitmap; APmtvFile: string);
    function HandleOnEvaluateReplacements(s: string; Recursive: Boolean = True): string;

    procedure HandleOnCopyControlTextAndClassFromMainWindow(ACompProvider: string; out AControlText, AControlClass: string);
    function HandleOnGetConnectionAddress: string;
    procedure HandleOnRecordComponent(ACompHandle: THandle; ATreeContentStream: TMemoryStream);
    function HandleOnGetSelectedCompFromRemoteWin: THandle;
    procedure HandleOnGetCurrentlyRecordedScreenShotImage(ABmp: TBitmap);

    function HandleOnFileExists(const FileName: string): Boolean;
    function HandleOnTClkIniReadonlyFileCreate(AFileName: string): TClkIniReadonlyFile;
    procedure HandleOnSaveTemplateToFile(AStringList: TStringList; const FileName: string);

    procedure HandleOnSetOpenDialogMultiSelect;
    procedure HandleOnSetOpenDialogInitialDir(AInitialDir: string);
    function HandleOnOpenDialogExecute(AFilter: string): Boolean;
    function HandleOnGetOpenDialogFileName: string;
    procedure HandleOnSetSaveDialogInitialDir(AInitialDir: string);
    function HandleOnSaveDialogExecute(AFilter: string): Boolean;
    function HandleOnGetSaveDialogFileName: string;
    procedure HandleOnSetSaveDialogFileName(AFileName: string);

    procedure HandleOnSetPictureSetOpenDialogMultiSelect;
    procedure HandleOnSetPictureOpenDialogInitialDir(AInitialDir: string);
    function HandleOnPictureOpenDialogExecute: Boolean;
    function HandleOnGetPictureOpenDialogFileName: string;
    function HandleOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function HandleOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;

    procedure HandleOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    procedure HandleOnSavePrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    procedure HandleOnGetSelfHandles(AListOfSelfHandles: TStringList);
  public
    property AllFormsAreCreated: Boolean write FAllFormsAreCreated;
  end;

var
  frmUIClickerMainForm: TfrmUIClickerMainForm;

implementation

{$R *.frm}

{ Checklist for adding a new field to an action structure and its property in ObjectInspector.
  - In ClickerUtils.pas, add the new field to the desired structure (like)
  - In ClickerActionValues.pas, add the new field to structures, functions and constants (including count(s))
  - Set default value for the new field when adding a new action (from palette)  - see TfrClickerActionsArr.FPaletteVsMouseUp
  - In ClickerActonsArrFrame.pas, update action settings  (see UpdateActionsArrFromControls and UpdateControlsFromActionsArr) if required
  - In ClickerTemplates.pas, add new field (Load and Save). If the actions is FindControl or FindSubControl, the field has to be added to CopyActionContent, as well.
  - In ActionExecution.pas, handle the new field.
  - In ClickerActionProperties.pas, handle the new field.
  - In ClickerActionsFrame.pas, add new icon to the "imglst*" image list for that action.
  - In ClickerActionsFrame.pas, Add menu, hint, (and custom editors, if required). Some properties may have to be grayed-out depending on other properties (see HandleOnOIPaintText).
  - In ClickerActionsClient.pas, add the new field
  - In ClickerClient.ppr, add the new field (in structure(s) and function(s)), then recompile the dll (ClickerClient)
  - In UIClickerTypes.py (and wrapper.py), add the new field and make sure it works
  - optionally, if the new field (as an ObjectInspector property) is related to other fields, so that it makes sense to be added right after them, and not at the end of the structure, then various other structures will have to be adjusted (including imglst icons)
  - Add/Modify some tests
  - Update documentation (and API)
  - If there are standalone applications, which are derived from UIClicker (e.g. EDAUIClicker), and directly depend on the new field, they have to be updated
}


uses
  ClickerPreviewForm, ClickerWinInterpForm, ClickerWinInterpFrame, ClickerTemplateCallTreeForm,
  ClickerActionsClient, IniFiles, ClickerFindControlFrame, ClickerRemoteScreenForm,
  ClickerUtils, ClickerPrimitives, ClickerActionsForm, ClickerPrimitivesCompositor;

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

    frmClickerActions.OnSetOpenDialogMultiSelect := HandleOnSetOpenDialogMultiSelect;
    frmClickerActions.OnSetOpenDialogInitialDir := HandleOnSetOpenDialogInitialDir;
    frmClickerActions.OnOpenDialogExecute := HandleOnOpenDialogExecute;
    frmClickerActions.OnGetOpenDialogFileName := HandleOnGetOpenDialogFileName;
    frmClickerActions.OnSetSaveDialogInitialDir := HandleOnSetSaveDialogInitialDir;
    frmClickerActions.OnSaveDialogExecute := HandleOnSaveDialogExecute;
    frmClickerActions.OnGetSaveDialogFileName := HandleOnGetSaveDialogFileName;
    frmClickerActions.OnSetSaveDialogFileName := HandleOnSetSaveDialogFileName;

    frmClickerActions.OnSetPictureSetOpenDialogMultiSelect := HandleOnSetPictureSetOpenDialogMultiSelect;
    frmClickerActions.OnSetPictureOpenDialogInitialDir := HandleOnSetPictureOpenDialogInitialDir;
    frmClickerActions.OnPictureOpenDialogExecute := HandleOnPictureOpenDialogExecute;
    frmClickerActions.OnGetPictureOpenDialogFileName := HandleOnGetPictureOpenDialogFileName;
    frmClickerActions.OnLoadBitmap := HandleOnLoadBitmap;
    frmClickerActions.OnLoadPrimitivesFile := HandleOnLoadPrimitivesFile;
    frmClickerActions.OnSavePrimitivesFile := HandleOnSavePrimitivesFile;
    frmClickerActions.OnGetSelfHandles := HandleOnGetSelfHandles;

    frmClickerTemplateCallTree.OnSetOpenDialogMultiSelect := HandleOnSetOpenDialogMultiSelect;
    frmClickerTemplateCallTree.OnFileExists := HandleOnFileExists;
    frmClickerTemplateCallTree.OnTClkIniReadonlyFileCreate := HandleOnTClkIniReadonlyFileCreate;
    frmClickerTemplateCallTree.OnOpenDialogExecute := HandleOnOpenDialogExecute;
    frmClickerTemplateCallTree.OnGetOpenDialogFileName := HandleOnGetOpenDialogFileName;
  finally
    Ini.Free;
  end;
end;


procedure TfrmUIClickerMainForm.SaveSettings;
var
  Ini: TMemIniFile;
begin
  {$IFDEF TestBuild}
    if GetCmdLineOptionValue('--SkipSavingSettings') = 'Yes' then
      Exit;
  {$ENDIF}

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
{$IFDEF TestBuild}
  var
    ExtraCaption: string;
{$ENDIF}
begin
  if not FAllFormsAreCreated then
    Exit;

  tmrStartup.Enabled := False;
  LoadSettings;

  if SizeOf(Pointer) = 4 then
    lblBitness.Caption := '32-bit'
  else
    lblBitness.Caption := '64-bit';

  {$IFDEF TestBuild}
    ExtraCaption := GetCmdLineOptionValue('--ExtraCaption');
    if ExtraCaption <> '' then
    begin
      Caption := Caption + ' - ' + ExtraCaption;
      frmClickerControlPreview.Caption := frmClickerControlPreview.Caption + ' - ' + ExtraCaption;
      frmClickerActions.Caption := frmClickerActions.Caption + ' - ' + ExtraCaption;
      frmClickerWinInterp.Caption := frmClickerWinInterp.Caption + ' - ' + ExtraCaption;
      frmClickerTemplateCallTree.Caption := frmClickerTemplateCallTree.Caption + ' - ' + ExtraCaption;
      frmClickerRemoteScreen.Caption := frmClickerRemoteScreen.Caption + ' - ' + ExtraCaption;
    end;
  {$ENDIF}
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


procedure TfrmUIClickerMainForm.HandleOnSetOpenDialogMultiSelect;
begin
  OpenDialog1.Options := OpenDialog1.Options + [ofAllowMultiSelect];
end;


procedure TfrmUIClickerMainForm.HandleOnSetOpenDialogInitialDir(AInitialDir: string);
begin
  OpenDialog1.InitialDir := AInitialDir;
end;


function TfrmUIClickerMainForm.HandleOnOpenDialogExecute(AFilter: string): Boolean;
begin
  OpenDialog1.Filter := AFilter;
  Result := OpenDialog1.Execute;
  OpenDialog1.Options := OpenDialog1.Options - [ofAllowMultiSelect];
end;


function TfrmUIClickerMainForm.HandleOnGetOpenDialogFileName: string;
begin
  if OpenDialog1.Files.Count > 1 then
    Result := OpenDialog1.Files.Text
  else
    Result := OpenDialog1.FileName;
end;


procedure TfrmUIClickerMainForm.HandleOnSetSaveDialogInitialDir(AInitialDir: string);
begin
  SaveDialog1.InitialDir := AInitialDir;
end;


function TfrmUIClickerMainForm.HandleOnSaveDialogExecute(AFilter: string): Boolean;
begin
  SaveDialog1.Filter := AFilter;
  Result := SaveDialog1.Execute;
  OpenDialog1.Options := OpenDialog1.Options - [ofAllowMultiSelect];
end;


function TfrmUIClickerMainForm.HandleOnGetSaveDialogFileName: string;
begin
  Result := SaveDialog1.FileName;
end;


procedure TfrmUIClickerMainForm.HandleOnSetSaveDialogFileName(AFileName: string);
begin
  SaveDialog1.FileName := AFileName;
end;


procedure TfrmUIClickerMainForm.HandleOnSetPictureSetOpenDialogMultiSelect;
begin
  OpenPictureDialog1.Options := OpenPictureDialog1.Options + [ofAllowMultiSelect];
end;


procedure TfrmUIClickerMainForm.HandleOnSetPictureOpenDialogInitialDir(AInitialDir: string);
begin
  OpenPictureDialog1.InitialDir := AInitialDir;
end;


function TfrmUIClickerMainForm.HandleOnPictureOpenDialogExecute: Boolean;
begin
  Result := OpenPictureDialog1.Execute;
  OpenPictureDialog1.Options := OpenPictureDialog1.Options - [ofAllowMultiSelect];
end;


function TfrmUIClickerMainForm.HandleOnGetPictureOpenDialogFileName: string;
begin
  if OpenPictureDialog1.Files.Count > 1 then
    Result := OpenPictureDialog1.Files.Text
  else
    Result := OpenPictureDialog1.FileName;
end;


function TfrmUIClickerMainForm.HandleOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if FileExists(AFileName) then
  begin
    if UpperCase(ExtractFileExt(AFileName)) = '.BMP' then
      ABitmap.LoadFromFile(AFileName)
    else
      if UpperCase(ExtractFileExt(AFileName)) = '.PMTV' then
        ComposePrimitiveOnBmp(ABitmap, AFileName);

    Result := True;
  end
  else
    Result := False;
end;


function TfrmUIClickerMainForm.HandleOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if frmClickerActions.RenderedInMemFileSystem.FileExistsInMem(AFileName) then
  begin
    frmClickerActions.LoadBmpFromInMemFileSystem(AFileName, ABitmap, frmClickerActions.RenderedInMemFileSystem);
    Result := True;
  end
  else
    Result := False;
end;


procedure TfrmUIClickerMainForm.HandleOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
var
  MemStream: TMemoryStream;
  Ini: TClkIniReadonlyFile;
begin
  if FileExists(AFileName) then
  begin
    MemStream := TMemoryStream.Create;
    try
      MemStream.LoadFromFile(AFileName);
      MemStream.Position := 0;

      Ini := TClkIniReadonlyFile.Create(MemStream);
      try
        LoadPrimitivesFile(Ini, APrimitives, AOrders, ASettings);
      finally
        Ini.Free;
      end;
    finally
      MemStream.Free;
    end;
  end;
end;


procedure TfrmUIClickerMainForm.HandleOnSavePrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
var
  FileContent: TStringList;
begin
  FileContent := TStringList.Create;
  try
    SavePrimitivesFile(FileContent, APrimitives, AOrders, ASettings);
    FileContent.SaveToFile(AFileName);
  finally
    FileContent.Free;
  end;
end;


procedure TfrmUIClickerMainForm.HandleOnGetSelfHandles(AListOfSelfHandles: TStringList);
var
  i: Integer;
  CurrentForm: TForm;
begin
  for i := 0 to Application.ComponentCount - 1 do
    if Application.Components[i] is TForm then
    begin
      try
        CurrentForm := Application.Components[i] as TForm;
        AListOfSelfHandles.Add(CurrentForm.Name + '_Handle=' + IntToStr(CurrentForm.Handle));
      except
      end;
    end;
end;


procedure TfrmUIClickerMainForm.ComposePrimitiveOnBmp(ABmp: TBitmap; APmtvFile: string);
var
  MemStream: TMemoryStream;
  Ini: TClkIniReadonlyFile;
  Primitives: TPrimitiveRecArr;
  Orders: TCompositionOrderArr;
  PrimitiveSettings: TPrimitiveSettings;
  PmtvCompositor: TPrimitivesCompositor;
  UsingHighContrast: Boolean;
begin
  if FileExists(APmtvFile) then
  begin
    MemStream := TMemoryStream.Create;
    try
      MemStream.LoadFromFile(APmtvFile);
      MemStream.Position := 0;

      Ini := TClkIniReadonlyFile.Create(MemStream);
      try
        PrimitiveSettings.CompositorDirection := cdTopBot;  //this can also come as parameter
        LoadPrimitivesFile(Ini, Primitives, Orders, PrimitiveSettings);

        PmtvCompositor := TPrimitivesCompositor.Create;
        try
          PmtvCompositor.OnEvaluateReplacementsFunc := HandleOnEvaluateReplacements;
          PmtvCompositor.OnLoadBitmap := HandleOnLoadBitmap;
          PmtvCompositor.OnLoadRenderedBitmap := HandleOnLoadRenderedBitmap;

          UsingHighContrast := False; //update this if it will ever be an option from parameter
          if Length(Orders) > 0 then
          begin
            ABmp.Width := PmtvCompositor.GetMaxX(ABmp.Canvas, Primitives);
            ABmp.Height := PmtvCompositor.GetMaxY(ABmp.Canvas, Primitives);
            PmtvCompositor.ComposePrimitives(ABmp, 0, UsingHighContrast, Primitives, Orders, PrimitiveSettings);
          end;
        finally
          PmtvCompositor.Free;
        end;
      finally
        Ini.Free;
      end;
    finally
      MemStream.Free;
    end;
  end;
end;


function TfrmUIClickerMainForm.HandleOnEvaluateReplacements(s: string; Recursive: Boolean = True): string;  //this handler is used in this unit only
begin
  Result := frmClickerActions.frClickerActionsArrMain.frClickerActions.EvaluateReplacements(s, Recursive);
end;

end.

