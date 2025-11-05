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
    bitbtnShowTemplateCallTree: TBitBtn;
    bitbtnShowWinInterp: TBitBtn;
    lblBitness: TLabel;
    OpenDialog1: TOpenDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    SaveDialog1: TSaveDialog;
    tmrStartup: TTimer;
    procedure btnShowActionsFormClick(Sender: TObject);
    procedure btnShowPreviewFormClick(Sender: TObject);
    procedure btnShowTemplateCallTreeClick(Sender: TObject);
    procedure btnShowWinInterpClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
  private
    FAllFormsAreCreated: Boolean;

    procedure LoadSettings;
    procedure SaveSettings;
    procedure SetHandles;

    procedure ComposePrimitiveOnBmp(ABmp: TBitmap; APmtvFile: string);
    function HandleOnEvaluateReplacements(s: string; Recursive: Boolean = True; AEvalTextCount: Integer = -1): string;

    procedure HandleOnCopyControlTextAndClassFromMainWindow(ACompProvider: string; out AControlText, AControlClass: string);
    function HandleOnGetConnectionAddress: string;
    procedure HandleOnReLoadActionsWindowSettings;
    procedure HandleOnRecordComponent(ACompHandle: THandle; ATreeContentStream: TMemoryStream);
    procedure HandleOnGetCurrentlyRecordedScreenShotImage(ABmp: TBitmap);

    function HandleOnFileExists(const FileName: string): Boolean;
    function HandleOnTClkIniReadonlyFileCreate(AFileName: string): TClkIniReadonlyFile;
    function HandleOnTClkIniFileCreate(AFileName: string): TClkIniFile;
    procedure HandleOnSaveTemplateToFile(AStringList: TStringList; const FileName: string);

    procedure HandleOnSetOpenDialogMultiSelect;
    procedure HandleOnSetOpenDialogInitialDir(AInitialDir: string);
    function HandleOnOpenDialogExecute(AFilter: string): Boolean;
    function HandleOnGetOpenDialogFileName: string;
    function HandleOnGetFullTemplatesDir: string;
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
    function HandleOnLoadRawPmtv(APmtvFile: TMemoryStream; AFileName: string): Boolean;

    procedure HandleOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    procedure HandleOnSavePrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    procedure HandleOnGetSelfHandles(AListOfSelfHandles: TStringList);
    function HandleOnGenerateAndSaveTreeWithWinInterp(AHandle: THandle; ATreeFileName: string; AStep: Integer; AUseMouseSwipe: Boolean): Boolean;
    function HandleOnSetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue: string): Boolean;

    procedure HandleOnLoadFileFromStream(AFileName: string; AStream: TMemoryStream);
    procedure HandleOnSaveFileToStream(AFileName: string; AStream: TMemoryStream);
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
  - Set default value for the new field when adding a new action (from palette)  - see TfrClickerActionsArr.OverwriteActionAtIndexWithDefault  and also ClickerActionProperties.pas
  - In ClickerActionsArrFrame.pas, update action settings  (see UpdateActionsArrFromControls and UpdateControlsFromActionsArr) if required   (mostly when adding new font/text properties)
  - In ClickerTemplates.pas, add new field (Load and Save). If the action is FindControl or FindSubControl, the field has to be added to CopyActionContent, as well.
  - In ActionExecution.pas, handle the new field.
  - In ClickerActionProperties.pas, handle the new field.
  - In ClickerActionsFrame.pas, add new icon to the "imglst*" image list for that field.
  - In ClickerActionsFrame.pas, Add menu, hint, (and custom editors, if required). Some properties may have to be grayed-out depending on other properties (see HandleOnOIPaintText).
  - In ClickerClientAPI.pas and ClickerClientIntf.pas, add the new field (in structure(s) and function(s)), then recompile the dll (ClickerClient)
  - In ActionsStuff.pas, add the new field to GenerateDifferentThanDefault_<Action> functions.
  - In UIClickerTypes.py (and wrapper.py), add the new field and make sure it works
  - In py\Tests\RunExecute<ActionName>Action.py test file for that particular action type, add the new fields
  - optionally, if the new field (as an ObjectInspector property) is related to other fields, so that it makes sense to be added right after them, and not at the end of the structure, then various other structures will have to be adjusted (including imglst icons)
  - Add/Modify some tests
  - Update documentation (and API)
  - If there are standalone applications, which are derived from UIClicker (e.g. EDAUIClicker), and directly depend on the new field, they have to be updated
}


uses
  ClickerPreviewForm, ClickerWinInterpForm, ClickerWinInterpFrame, ClickerTemplateCallTreeForm,
  ClickerActionsClient, IniFiles, ClickerFindControlFrame, BitmapConv,
  ClickerUtils, ClickerPrimitives, ClickerActionsForm, ClickerPrimitivesCompositor, Math;

{ TfrmUIClickerMainForm }


procedure TfrmUIClickerMainForm.SetHandles;
begin
  frmClickerWinInterp.OnGetConnectionAddress := HandleOnGetConnectionAddress;
  frmClickerWinInterp.OnOpenDialogExecute := HandleOnOpenDialogExecute;
  frmClickerWinInterp.OnGetOpenDialogFileName := HandleOnGetOpenDialogFileName;
  frmClickerWinInterp.OnSaveDialogExecute := HandleOnSaveDialogExecute;
  frmClickerWinInterp.OnGetSaveDialogFileName := HandleOnGetSaveDialogFileName;
  frmClickerWinInterp.OnLoadFileFromStream := HandleOnLoadFileFromStream;
  frmClickerWinInterp.OnSaveFileToStream := HandleOnSaveFileToStream;
  frmClickerWinInterp.OnFileExists := HandleOnFileExists;

  frmClickerActions.OnCopyControlTextAndClassFromMainWindow := HandleOnCopyControlTextAndClassFromMainWindow;
  frmClickerActions.OnReLoadSettings := HandleOnReLoadActionsWindowSettings;
  frmClickerActions.OnRecordComponent := HandleOnRecordComponent;
  frmClickerActions.OnGetCurrentlyRecordedScreenShotImage := HandleOnGetCurrentlyRecordedScreenShotImage;
  frmClickerActions.OnFileExists := HandleOnFileExists;
  frmClickerActions.OnTClkIniReadonlyFileCreate := HandleOnTClkIniReadonlyFileCreate;
  frmClickerActions.OnTClkIniFileCreate := HandleOnTClkIniFileCreate;
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
  frmClickerActions.OnLoadRawPmtv := HandleOnLoadRawPmtv;
  frmClickerActions.OnLoadPrimitivesFile := HandleOnLoadPrimitivesFile;
  frmClickerActions.OnSavePrimitivesFile := HandleOnSavePrimitivesFile;
  frmClickerActions.OnGetSelfHandles := HandleOnGetSelfHandles;
  frmClickerActions.OnGenerateAndSaveTreeWithWinInterp := HandleOnGenerateAndSaveTreeWithWinInterp;
  frmClickerActions.OnSetWinInterpOption := HandleOnSetWinInterpOption;

  frmClickerTemplateCallTree.OnSetOpenDialogMultiSelect := HandleOnSetOpenDialogMultiSelect;
  frmClickerTemplateCallTree.OnFileExists := HandleOnFileExists;
  frmClickerTemplateCallTree.OnTClkIniReadonlyFileCreate := HandleOnTClkIniReadonlyFileCreate;
  frmClickerTemplateCallTree.OnOpenDialogExecute := HandleOnOpenDialogExecute;
  frmClickerTemplateCallTree.OnGetOpenDialogFileName := HandleOnGetOpenDialogFileName;
  frmClickerTemplateCallTree.OnGetFullTemplatesDir := HandleOnGetFullTemplatesDir;
  frmClickerTemplateCallTree.OnLoadBitmap := HandleOnLoadBitmap;
end;


procedure TfrmUIClickerMainForm.LoadSettings;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Clicker.ini');
  try
    Left := Ini.ReadInteger('MainWindow', 'Left', Min(Left, Screen.DesktopWidth - 60));
    Top := Ini.ReadInteger('MainWindow', 'Top', Min(Top, Screen.DesktopHeight - 60));
    Width := Ini.ReadInteger('MainWindow', 'Width', Min(Width, Screen.DesktopWidth - 40));
    Height := Ini.ReadInteger('MainWindow', 'Height', Min(Height, Screen.DesktopHeight - 40));

    frmClickerControlPreview.LoadSettings(Ini);
    frmClickerActions.LoadSettings(Ini);
    frmClickerWinInterp.LoadSettings(Ini);
    frmClickerTemplateCallTree.LoadSettings(Ini);
  finally
    Ini.Free;
  end;

  SetHandles;
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
    Ini.WriteInteger('MainWindow', 'Left', Min(Left, Screen.DesktopWidth - 60));
    Ini.WriteInteger('MainWindow', 'Top', Min(Top, Screen.DesktopHeight - 60));
    Ini.WriteInteger('MainWindow', 'Width', Min(Width, Screen.DesktopWidth - 40));
    Ini.WriteInteger('MainWindow', 'Height', Min(Height, Screen.DesktopHeight - 40));

    frmClickerControlPreview.SaveSettings(Ini);
    frmClickerActions.SaveSettings(Ini);
    frmClickerWinInterp.SaveSettings(Ini);
    frmClickerTemplateCallTree.SaveSettings(Ini);

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


procedure TfrmUIClickerMainForm.btnShowPreviewFormClick(Sender: TObject);
begin
  frmClickerControlPreview.Show;
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
      //Do not set Application.Title := 'UIClicker - ' + ExtraCaption;, because this will modify MessageBox titles, which will not be found in tests.
    end;
  {$ENDIF}

  {$IFnDEF Windows}
    Font.Size := 8;
  {$ENDIF}
end;


procedure TfrmUIClickerMainForm.HandleOnCopyControlTextAndClassFromMainWindow(ACompProvider: string; out AControlText, AControlClass: string);
begin
  if ACompProvider = CExtProvPreviewWindow then
  begin
    AControlText := frmClickerControlPreview.lbeText.Text;
    AControlClass := frmClickerControlPreview.lbeClass.Text;
    Exit;
  end;

  if ACompProvider = CExtProvWinInterpWindow then
  begin
    AControlText := frmClickerWinInterp.SelectedComponentText;
    AControlClass := frmClickerWinInterp.SelectedComponentClassName;
    Exit;
  end;

  if ACompProvider = CExtProvSystemMenu then
  begin
    AControlText := '';
    AControlClass := '#32768';
    Exit;
  end;

  AControlText := '<unknown text provider>';
  AControlClass := '<unknown class provider>';
end;


function TfrmUIClickerMainForm.HandleOnGetConnectionAddress: string;
begin
  Result := frmClickerActions.ConfiguredRemoteAddress; //not thread safe
end;


procedure TfrmUIClickerMainForm.HandleOnReLoadActionsWindowSettings;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Clicker.ini');
  try
    frmClickerActions.LoadSettings(Ini);
  finally
    Ini.Free;
  end;
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


function TfrmUIClickerMainForm.HandleOnTClkIniFileCreate(AFileName: string): TClkIniFile;
begin
  Result := TClkIniFile.Create(AFileName);
end;


procedure TfrmUIClickerMainForm.HandleOnSaveTemplateToFile(AStringList: TStringList; const FileName: string);
begin
  AStringList.SaveToFile(FileName);   //do not add extension to FileName, because this event handler is used for other text files as well
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


function TfrmUIClickerMainForm.HandleOnGetFullTemplatesDir: string;
begin
  try
    Result := frmClickerActions.FullTemplatesDir;
  except
    Result := ''; //in case frmClickerActions is nil
  end;
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
  Result := FileExists(AFileName);

  if Result then
  begin
    if UpperCase(ExtractFileExt(AFileName)) = '.BMP' then
      ABitmap.LoadFromFile(AFileName)
    else
      if UpperCase(ExtractFileExt(AFileName)) = '.PMTV' then
        ComposePrimitiveOnBmp(ABitmap, AFileName)
      else
        if UpperCase(ExtractFileExt(AFileName)) = '.EXE' then
          DrawExeIconOnBmp(ABitmap, AFileName)
        else
          if (UpperCase(ExtractFileExt(AFileName)) = '.PNG') then
            DrawPngOnBmp(ABitmap, AFileName)
          else
            if (UpperCase(ExtractFileExt(AFileName)) = '.JPG') or (UpperCase(ExtractFileExt(AFileName)) = '.JPEG') then
              DrawJpgOnBmp(ABitmap, AFileName)
            else
              if (UpperCase(ExtractFileExt(AFileName)) = '.ICO') then
                DrawIcoOnBmp(ABitmap, AFileName);
  end;
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


function TfrmUIClickerMainForm.HandleOnLoadRawPmtv(APmtvFile: TMemoryStream; AFileName: string): Boolean;
begin
  Result := FileExists(AFileName);
  if Result then
    APmtvFile.LoadFromFile(AFileName);
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
    FileContent.LineBreak := #13#10;
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


function TfrmUIClickerMainForm.HandleOnGenerateAndSaveTreeWithWinInterp(AHandle: THandle; ATreeFileName: string; AStep: Integer; AUseMouseSwipe: Boolean): Boolean;
var
  ImgMatrix: TColorArr;
  ImgHWMatrix: THandleArr;
  TreeContent: TMemoryStream;
begin
  Result := False;

  //if AHandle = 0 then
  //begin
  //  Result := False;
  //  Exit;
  //end;
  //
  TreeContent := TMemoryStream.Create;
  try
    if AUseMouseSwipe then
      frmClickerWinInterp.RecordWithMouseSwipe(AHandle, AStep)
    else
      frmClickerWinInterp.RecordComponent(AHandle, ImgMatrix, ImgHWMatrix, AStep);

    frmClickerWinInterp.GetTreeContent(TreeContent);

    TreeContent.Position := 0;
    HandleOnSaveFileToStream(ATreeFileName, TreeContent);
    frmClickerWinInterp.SaveImages(ATreeFileName);

    Result := True;
  finally
    SetLength(ImgMatrix, 0);
    SetLength(ImgHWMatrix, 0);
    TreeContent.Free;
  end;
end;


function TfrmUIClickerMainForm.HandleOnSetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue: string): Boolean;
begin
  Result := frmClickerWinInterp.SetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue);
end;


procedure TfrmUIClickerMainForm.HandleOnLoadFileFromStream(AFileName: string; AStream: TMemoryStream);
begin
  AStream.LoadFromFile(AFileName);
end;


procedure TfrmUIClickerMainForm.HandleOnSaveFileToStream(AFileName: string; AStream: TMemoryStream);
begin
  CreateDirWithSubDirs(ExtractFileDir(AFileName));
  AStream.SaveToFile(AFileName);
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


function TfrmUIClickerMainForm.HandleOnEvaluateReplacements(s: string; Recursive: Boolean = True; AEvalTextCount: Integer = -1): string;  //this handler is used in this unit only
begin
  Result := frmClickerActions.frClickerActionsArrMain.frClickerActions.EvaluateReplacements(s, Recursive, AEvalTextCount);
end;

end.

