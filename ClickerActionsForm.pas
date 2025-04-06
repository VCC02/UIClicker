{
    Copyright (C) 2025 VCC
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


unit ClickerActionsForm;

{$H+}
{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}


interface

uses
  {$IFDEF Windows}
    Windows,
  {$ELSE}
    LCLIntf, LCLType, Types,
  {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Menus, ColorBox, ClickerActionsArrFrame, InMemFileSystem,
  IdHTTPServer, IdSchedulerOfThreadPool, IdCustomHTTPServer, IdContext, IdSync, IdGlobal,
  PollingFIFO, ClickerFileProviderClient, IniFiles, ClickerUtils, ClickerActionExecution,
  ClickerIniFiles, ClickerPrimitiveUtils, ClickerPluginArchive;

{$IFnDEF Windows}
  {$UNDEF MemPlugins}
{$ENDIF}


type
  TLoggingSyncObj = class(TIdSync)
  private
    FMsg: string;
  protected
    procedure DoSynchronize; override;
  end;

  TSyncHTTPCmd = class(TIdSync)
  private
    FCmd: string;
    FParams: TStrings;
    FResult: string;
    FErrCode: Integer;
    FFrame: TfrClickerActionsArr;
    FBmp: TBitmap;
    FGPStream: TMemoryStream;
  protected
    procedure DoSynchronize; override;
  public
    constructor Create; override;
  end;

  TSetVarSyncObj = class(TIdSync)
  private
    FVarName, FVarValue: string;
  protected
    procedure DoSynchronize; override;
  end;


  TOnReLoadSettings = procedure of object;
  TOnRecordComponent = procedure(ACompHandle: THandle; ATreeContentStream: TMemoryStream) of object;
  TOnGetCurrentlyRecordedScreenShotImage = procedure(ABmp: TBitmap) of object;

  { TfrmClickerActions }

  TfrmClickerActions = class(TForm)
    btnBrowseActionTemplatesDir: TButton;
    btnTestConnection: TButton;
    chkAutoEnableSwitchingTabsOnDebugging: TCheckBox;
    chkAutoSwitchToExecutingTab: TCheckBox;
    chkDisplayActivity: TCheckBox;
    chkKeepAlive: TCheckBox;
    chkSetExperimentsToClientMode: TCheckBox;
    chkServerActive: TCheckBox;
    chkStayOnTop: TCheckBox;
    cmbExecMode: TComboBox;
    cmbFilesExistence: TComboBox;
    cmbImgPreviewGridType: TComboBox;
    colcmbTopLeftValid: TColorBox;
    colcmbBotRightValid: TColorBox;
    colcmbTopLeftInvalid: TColorBox;
    colcmbBotRightInvalid: TColorBox;
    cmbClientModeServerAddress: TComboBox;
    grpSelectionColors: TGroupBox;
    grpMissingFilesMonitoring: TGroupBox;
    grpAllowedFileExtensionsForServer: TGroupBox;
    grpAllowedFileDirsForServer: TGroupBox;
    grpVariables: TGroupBox;
    IdHTTPServer1: TIdHTTPServer;
    IdSchedulerOfThreadPool1: TIdSchedulerOfThreadPool;
    imglstCalledTemplates: TImageList;
    imglstMainPage: TImageList;
    lblServerAddress: TLabel;
    lblBotRightInvalidColor: TLabel;
    lblTopLeftValidColor: TLabel;
    lblGridType: TLabel;
    lblClientMode: TLabel;
    lbeConnectTimeout: TLabeledEdit;
    lblServerMode: TLabel;
    lblFileMonitoringThreadInfo: TLabel;
    lblFilesExistence: TLabel;
    lblAdminStatus: TLabel;
    lblExp1: TLabel;
    lblExp2: TLabel;
    lblLocalModeInfo: TLabel;
    lblServerInfo: TLabel;
    lbeServerModePort: TLabeledEdit;
    lbePathToTemplates: TLabeledEdit;
    lblExecMode: TLabel;
    lblBotRightValidColor: TLabel;
    lblTopLeftInvalidColor: TLabel;
    memAllowedFileExtensionsForServer: TMemo;
    memAllowedFileDirsForServer: TMemo;
    memVariables: TMemo;
    MenuItem_RemoveServerAddress: TMenuItem;
    PageControlExecMode: TPageControl;
    PageControlMain: TPageControl;
    PageControlPlayer: TPageControl;
    pnlMissingFilesRequest: TPanel;
    pmClientModeServerAddress: TPopupMenu;
    scrboxMain: TScrollBox;
    TabSheetLocalMode: TTabSheet;
    TabSheetClientMode: TTabSheet;
    TabSheetServerMode: TTabSheet;
    TabSheetSettings: TTabSheet;
    TabSheetTemplateExec: TTabSheet;
    TabSheetExecMainPlayer: TTabSheet;
    TabSheetExperiments1: TTabSheet;
    TabSheetExperiments2: TTabSheet;
    tmrDelayedShow: TTimer;
    tmrUpdateSelectionColorsFromColorBoxes: TTimer;
    tmrUpdateColors: TTimer;
    tmrDisplayMissingFilesRequests: TTimer;
    tmrStartup: TTimer;
    procedure btnBrowseActionTemplatesDirClick(Sender: TObject);
    procedure btnTestConnectionClick(Sender: TObject);
    procedure chkAutoEnableSwitchingTabsOnDebuggingChange(Sender: TObject);
    procedure chkAutoSwitchToExecutingTabChange(Sender: TObject);
    procedure chkDisplayActivityChange(Sender: TObject);
    procedure chkServerActiveChange(Sender: TObject);
    procedure chkStayOnTopClick(Sender: TObject);
    procedure cmbExecModeChange(Sender: TObject);
    procedure cmbImgPreviewGridTypeChange(Sender: TObject);
    procedure colcmbBotRightInvalidChange(Sender: TObject);
    procedure colcmbBotRightValidChange(Sender: TObject);
    procedure colcmbTopLeftInvalidChange(Sender: TObject);
    procedure colcmbTopLeftValidChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IdHTTPServer1CommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure IdHTTPServer1CommandOther(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure IdHTTPServer1Connect(AContext: TIdContext);
    procedure IdHTTPServer1Exception(AContext: TIdContext; AException: Exception
      );
    procedure lbePathToTemplatesChange(Sender: TObject);
    procedure lbePathToTemplatesKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure pmClientModeServerAddressPopup(Sender: TObject);
    procedure tmrDelayedShowTimer(Sender: TObject);
    procedure tmrDisplayMissingFilesRequestsTimer(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
    procedure tmrUpdateColorsTimer(Sender: TObject);
    procedure tmrUpdateSelectionColorsFromColorBoxesTimer(Sender: TObject);
  private
    FStopAllActionsOnDemand: Boolean;
    FFullTemplatesDir: string;
    FBMPsDir: string;
    FInMemFileSystem: TInMemFileSystem;   //for client-server execution
    FRenderedInMemFileSystem: TInMemFileSystem;  //for externally rendered images
    FPluginsInMemFileSystem: TInMemFileSystem;  //for storing received plugins in memory

    FDecDecHashPluginInMemFSArr: TDecDecHashPluginInMemFSArr;
    FDecDecHashArrCritSec: TRTLCriticalSection;

    FFileAvailabilityFIFO: TPollingFIFO;
    FPollForMissingServerFiles: TPollForMissingServerFiles;
    FProcessingMissingFilesRequestByClient: Boolean; //for activity info
    FProcessingMissingFilesRequestByServer: Boolean; //for activity info

    FPreviewSelectionColors: TSelectionColors;

    FTerminateWaitForFileAvailability: Boolean;
    FTerminateWaitForMultipleFilesAvailability: Boolean;
    FAutoSwitchToExecutingTab: Boolean;
    FAutoEnableSwitchingTabsOnDebugging: Boolean;
    FFontFinderSettings: TFontFinderSettings;

    FFirstDisplaying: Boolean;
    FRecentTemplates: TStringList;

    FOnReLoadSettings: TOnReLoadSettings;
    FOnCopyControlTextAndClassFromMainWindow: TOnCopyControlTextAndClassFromMainWindow;
    FOnRecordComponent: TOnRecordComponent;
    FOnGetCurrentlyRecordedScreenShotImage: TOnGetCurrentlyRecordedScreenShotImage;
    FOnLoadBitmap: TOnLoadBitmap;
    FOnLoadPrimitivesFile: TOnLoadPrimitivesFile;
    FOnSavePrimitivesFile: TOnSavePrimitivesFile;
    FOnGetSelfHandles: TOnGetSelfHandles;

    FOnFileExists: TOnFileExists;
    FOnTClkIniReadonlyFileCreate: TOnTClkIniReadonlyFileCreate;
    FOnTClkIniFileCreate: TOnTClkIniFileCreate;
    FOnSaveTemplateToFile: TOnSaveTemplateToFile;

    FOnSetOpenDialogMultiSelect: TOnSetOpenDialogMultiSelect;
    FOnSetOpenDialogInitialDir: TOnSetOpenDialogInitialDir;
    FOnOpenDialogExecute: TOnOpenDialogExecute;
    FOnGetOpenDialogFileName: TOnGetOpenDialogFileName;
    FOnSetSaveDialogInitialDir: TOnSetOpenDialogInitialDir;
    FOnSaveDialogExecute: TOnOpenDialogExecute;
    FOnGetSaveDialogFileName: TOnGetOpenDialogFileName;
    FOnSetSaveDialogFileName: TOnSetOpenDialogFileName;

    FOnSetPictureSetOpenDialogMultiSelect: TOnSetPictureSetOpenDialogMultiSelect;
    FOnSetPictureOpenDialogInitialDir: TOnSetPictureOpenDialogInitialDir;
    FOnPictureOpenDialogExecute: TOnPictureOpenDialogExecute;
    FOnGetPictureOpenDialogFileName: TOnGetPictureOpenDialogFileName;

    FOnGenerateAndSaveTreeWithWinInterp: TOnGenerateAndSaveTreeWithWinInterp;
    FOnSetWinInterpOption: TOnSetWinInterpOption;

    //frClickerActionsArrMain: TfrClickerActionsArr;    //eventually, these fields should be made private again, and accessed through methods
    //frClickerActionsArrExperiment1: TfrClickerActionsArr;
    //frClickerActionsArrExperiment2: TfrClickerActionsArr;

    FControlPlayerPopup: TPopupMenu;

    procedure SetFullTemplatesDir(Value: string);

    function GetBMPsDir: string;
    procedure SetBMPsDir(Value: string);

    function GetConfiguredRemoteAddress: string;
    function GetActionExecution: TActionExecution;
    procedure UpdatePreviewSelectionColorsFromColorBoxes;

    procedure DoOnReLoadSettings;
    procedure DoOnRecordComponent(ACompHandle: THandle; ATreeContentStream: TMemoryStream);
    procedure DoOnGetCurrentlyRecordedScreenShotImage(ABmp: TBitmap);

    function DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    procedure DoOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    procedure DoOnSavePrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    function DoOnFileExists(const AFileName: string): Boolean;
    procedure DoOnGetSelfHandles(AListOfSelfHandles: TStringList);

    function DoOnTClkIniReadonlyFileCreate(AFileName: string): TClkIniReadonlyFile;
    function DoOnTClkIniFileCreate(AFileName: string): TClkIniFile;
    procedure DoOnSaveTemplateToFile(AStringList: TStringList; const AFileName: string);

    procedure DoOnSetOpenDialogMultiSelect;
    procedure DoOnSetOpenDialogInitialDir(AInitialDir: string);
    function DoOnOpenDialogExecute(AFilter: string): Boolean;
    function DoOnGetOpenDialogFileName: string;
    procedure DoOnSetSaveDialogInitialDir(AInitialDir: string);
    function DoOnSaveDialogExecute(AFilter: string): Boolean;
    function DoOnGetSaveDialogFileName: string;
    procedure DoOnSetSaveDialogFileName(AFileName: string);

    procedure DoOnSetPictureSetOpenDialogMultiSelect;
    procedure DoOnSetPictureOpenDialogInitialDir(AInitialDir: string);
    function DoOnPictureOpenDialogExecute: Boolean;
    function DoOnGetPictureOpenDialogFileName: string;

    function DoOnGenerateAndSaveTreeWithWinInterp(AHandle: THandle; ATreeFileName: string; AStep: Integer; AUseMouseSwipe: Boolean): Boolean;
    function DoOnSetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue: string): Boolean;

    procedure MenuItemOpenTemplateAsExp1Click(Sender: TObject);
    procedure MenuItemOpenTemplateAsExp2Click(Sender: TObject);
    procedure frClickerActionsArrExperiment1PasteDebugValuesListFromMainExecutionList1Click(Sender: TObject);
    procedure frClickerActionsArrExperiment2PasteDebugValuesListFromMainExecutionList1Click(Sender: TObject);

    function GetListOfWaitingFiles: string;
    function GetCompAtPoint(AParams: TStrings): string;

    procedure SetExecutionMode(AMode: Integer);
    procedure ProcessChangingExecutionMode;
    procedure UpdateGridType;

    procedure HandleNewFrameRefreshButton(Sender: TObject);
    function frClickerActionsArrOnCallTemplate(Sender: TObject; AFileNameToCall: string; ListOfVariables: TStrings; DebugBitmap: TBitmap; DebugGridImage: TImage; IsDebugging, AShouldStopAtBreakPoint: Boolean; AStackLevel: Integer; AExecutesRemotely: Boolean): Boolean;
    procedure HandleOnCopyControlTextAndClassFromMainWindow(ACompProvider: string; out AControlText, AControlClass: string);
    function HandleOnGetExtraSearchAreaDebuggingImageWithStackLevel(AExtraBitmap: TBitmap; AStackLevel: Integer): Boolean;

    procedure HandleOnWaitForFileAvailability(AFileName: string); //ClickerActionsArrFrame instances call this, to add a filename to FIFO
    procedure HandleOnWaitForMultipleFilesAvailability(AListOfFiles: TStringList); //ClickerActionsArrFrame instances call this, to add multiple filenames to FIFO
    procedure HandleOnWaitForBitmapsAvailability(AListOfBitmapFiles: TStringList);  //ClickerActionsArrFrame instances call this, to add multiple bmps to FIFO, if not found
    procedure HandleOnTerminateWaitForMultipleFilesAvailability;

    function HandleOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function HandleOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
    function HandleOnLoadPluginFromInMemFS(APlugin: TMemoryStream; AFileName: string): Boolean;
    procedure HandleOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages: TStringList);

    {$IFDEF MemPlugins}
      procedure HandleOnGetListOfInMemPlugins(AListOfInMemPlugins: TStringList);
      procedure HandleOnLoadPluginFromDiskToPluginInMemFileSystem(APluginPath: string);
    {$ENDIF}

    function HandleOnRenderBmpExternally(ARequest: string): string;
    procedure HandleOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    procedure HandleOnSavePrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
    function HandleOnFileExists(const FileName: string): Boolean;
    procedure HandleOnGetSelfHandles(AListOfSelfHandles: TStringList);

    procedure HandleLogMissingServerFile(AMsg: string);
    procedure HandleOnLoadMissingFileContent(AFileName: string; AFileContent: TMemoryStream);
    procedure HandleOnDenyFile(AFileName: string);

    procedure HandleOnBeforeRequestingListOfMissingFiles;  //client thread calls this, without UI sync
    procedure HandleOnAfterRequestingListOfMissingFiles;   //client thread calls this, without UI sync

    function HandleOnComputeInMemFileHash(AFileContent: Pointer; AFileSize: Int64): string;

    function HandleOnTClkIniReadonlyFileCreate(AFileName: string): TClkIniReadonlyFile;
    function HandleOnTClkIniFileCreate(AFileName: string): TClkIniFile;
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

    function HandleOnGetGridDrawingOption: TDisplayGridLineOption;
    procedure HandleOnGetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);
    procedure HandleOnSetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);

    procedure HandleOnRetrieveRenderedBmpFromServer(ARemoteAddress, AFnm: string);
    procedure HandleOnOpenCalledTemplateInExperimentTab(AExperimentIndex: Integer; ATemplatePath: string);
    procedure HandleOnSaveFileToExtRenderingInMemFS(AFileName: string; AContent: Pointer; AFileSize: Int64);

    procedure HandleOnAddFileNameToRecent(AFileName: string);
    procedure HandleOnGetListOfRecentFiles(AList: TStringList);
    function HandleOnGenerateAndSaveTreeWithWinInterp(AHandle: THandle; ATreeFileName: string; AStep: Integer; AUseMouseSwipe: Boolean): Boolean;
    function HandleOnSetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue: string): Boolean;

    function HandleOnGetListeningPort: Word;
    procedure HandlePluginAddToLog(s: string);
    procedure HandleOnSetVar(AVarName, AVarValue: string);

    procedure HandleOnRemoveServerAddressClick(Sender: TObject);

    procedure CreateRemainingUIComponents;
    function GetClickerActionsArrFrameByStackLevel(AStackLevel: Integer): TfrClickerActionsArr;

    procedure AddToLog(s: string);
    function ProcessServerCmd(ASyncObj: TSyncHTTPCmd): string; //used in server mode

    property BMPsDir: string read GetBMPsDir write SetBMPsDir;
  public
    frClickerActionsArrMain: TfrClickerActionsArr;           //to be removed from public
    frClickerActionsArrExperiment1: TfrClickerActionsArr;
    frClickerActionsArrExperiment2: TfrClickerActionsArr;

    procedure LoadSettings(AIni: TMemIniFile);
    procedure SaveSettings(AIni: TMemIniFile);

    class procedure LoadBmpFromInMemFileSystem(AFileName: string; ABmp: TBitmap; AInMemFileSystem: TInMemFileSystem);
    class procedure SaveBmpToInMemFileSystem(AFileName: string; ABmp: TBitmap; AInMemFileSystem: TInMemFileSystem);

    //public properties (because of creating new instances)
    property FullTemplatesDir: string read FFullTemplatesDir write SetFullTemplatesDir;  //no trailing backslash
    property ConfiguredRemoteAddress: string read GetConfiguredRemoteAddress;
    property ActionExecution: TActionExecution read GetActionExecution;
    property StopAllActionsOnDemand: Boolean read FStopAllActionsOnDemand write FStopAllActionsOnDemand;

    property RenderedInMemFileSystem: TInMemFileSystem read FRenderedInMemFileSystem;  //for externally rendered images
    property PluginsInMemFileSystem: TInMemFileSystem read FPluginsInMemFileSystem;    //for plugins

    property OnReLoadSettings: TOnReLoadSettings read FOnReLoadSettings write FOnReLoadSettings;
    property OnCopyControlTextAndClassFromMainWindow: TOnCopyControlTextAndClassFromMainWindow read FOnCopyControlTextAndClassFromMainWindow write FOnCopyControlTextAndClassFromMainWindow;
    property OnRecordComponent: TOnRecordComponent read FOnRecordComponent write FOnRecordComponent;
    property OnGetCurrentlyRecordedScreenShotImage: TOnGetCurrentlyRecordedScreenShotImage read FOnGetCurrentlyRecordedScreenShotImage write FOnGetCurrentlyRecordedScreenShotImage;
    property OnLoadBitmap: TOnLoadBitmap read FOnLoadBitmap write FOnLoadBitmap;
    property OnLoadPrimitivesFile: TOnLoadPrimitivesFile write FOnLoadPrimitivesFile;
    property OnSavePrimitivesFile: TOnSavePrimitivesFile write FOnSavePrimitivesFile;
    property OnGetSelfHandles: TOnGetSelfHandles write FOnGetSelfHandles;

    property OnFileExists: TOnFileExists write FOnFileExists;
    property OnTClkIniReadonlyFileCreate: TOnTClkIniReadonlyFileCreate write FOnTClkIniReadonlyFileCreate;
    property OnTClkIniFileCreate: TOnTClkIniFileCreate write FOnTClkIniFileCreate;
    property OnSaveTemplateToFile: TOnSaveTemplateToFile write FOnSaveTemplateToFile;

    property OnSetOpenDialogMultiSelect: TOnSetOpenDialogMultiSelect write FOnSetOpenDialogMultiSelect;
    property OnSetOpenDialogInitialDir: TOnSetOpenDialogInitialDir write FOnSetOpenDialogInitialDir;
    property OnOpenDialogExecute: TOnOpenDialogExecute write FOnOpenDialogExecute;
    property OnGetOpenDialogFileName: TOnGetOpenDialogFileName write FOnGetOpenDialogFileName;
    property OnSetSaveDialogInitialDir: TOnSetOpenDialogInitialDir write FOnSetSaveDialogInitialDir;
    property OnSaveDialogExecute: TOnOpenDialogExecute write FOnSaveDialogExecute;
    property OnGetSaveDialogFileName: TOnGetOpenDialogFileName write FOnGetSaveDialogFileName;
    property OnSetSaveDialogFileName: TOnSetOpenDialogFileName write FOnSetSaveDialogFileName;

    property OnSetPictureSetOpenDialogMultiSelect: TOnSetPictureSetOpenDialogMultiSelect write FOnSetPictureSetOpenDialogMultiSelect;
    property OnSetPictureOpenDialogInitialDir: TOnSetPictureOpenDialogInitialDir write FOnSetPictureOpenDialogInitialDir;
    property OnPictureOpenDialogExecute: TOnPictureOpenDialogExecute write FOnPictureOpenDialogExecute;
    property OnGetPictureOpenDialogFileName: TOnGetPictureOpenDialogFileName write FOnGetPictureOpenDialogFileName;

    property OnGenerateAndSaveTreeWithWinInterp: TOnGenerateAndSaveTreeWithWinInterp write FOnGenerateAndSaveTreeWithWinInterp;
    property OnSetWinInterpOption: TOnSetWinInterpOption write FOnSetWinInterpOption;
  end;


var
  frmClickerActions: TfrmClickerActions;

implementation

{$R *.frm}


uses
  BitmapProcessing, ClickerActionsClient, MouseStuff, ClickerPrimitives,
  ClickerExtraUtils, Math;


const
  CExpectedFileLocation: array[Boolean] of TFileLocation = (flDisk, flMem {flDiskThenMem}); //eventually, convert this into a function and read app settings


procedure TLoggingSyncObj.DoSynchronize;
begin
  frmClickerActions.AddToLog(FMsg);
end;


procedure AddToLogFromThread(s: string);
var
  SyncObj: TLoggingSyncObj;
begin
  SyncObj := TLoggingSyncObj.Create;
  try
    SyncObj.FMsg := s;
    SyncObj.Synchronize;
  finally
    SyncObj.Free;
  end;
end;


procedure TSetVarSyncObj.DoSynchronize;
begin
  frmClickerActions.frClickerActionsArrMain.SetActionVarValue(FVarName, FVarValue);
end;


procedure SetVarFromThread(AVarName, AVarValue: string);
var
  SyncObj: TSetVarSyncObj;
begin
  SyncObj := TSetVarSyncObj.Create;
  try
    SyncObj.FVarName := AVarName;
    SyncObj.FVarValue := AVarValue;
    SyncObj.Synchronize;
  finally
    SyncObj.Free;
  end;
end;


class procedure TfrmClickerActions.LoadBmpFromInMemFileSystem(AFileName: string; ABmp: TBitmap; AInMemFileSystem: TInMemFileSystem);
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    AInMemFileSystem.LoadFileFromMemToStream(AFileName, MemStream);
    MemStream.Position := 0;

    try
      ABmp.LoadFromStream(MemStream, MemStream.Size);
    except
      on E: Exception do
      begin
        ABmp.Width := ABmp.Canvas.TextWidth(E.Message) + 20;
        ABmp.Height := 30;
        ABmp.Canvas.Brush.Color := clRed;
        ABmp.Canvas.Font.Color := clWhite;
        ABmp.Canvas.TextOut(10, 10, E.Message);
      end;
    end;
  finally
    MemStream.Free;
  end;
end;


class procedure TfrmClickerActions.SaveBmpToInMemFileSystem(AFileName: string; ABmp: TBitmap; AInMemFileSystem: TInMemFileSystem);
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    ABmp.SaveToStream(MemStream);
    AInMemFileSystem.SaveFileToMem(AFileName, MemStream.Memory, MemStream.Size);
  finally
    MemStream.Free;
  end;
end;


procedure LoadPmtvFromInMemFileSystem(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings; AInMemFileSystem: TInMemFileSystem);
var
  MemStream: TMemoryStream;
  Ini: TClkIniReadonlyFile;
begin
  MemStream := TMemoryStream.Create;
  try
    AInMemFileSystem.LoadFileFromMemToStream(AFileName, MemStream);
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


procedure SavePmtvToInMemFileSystem(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; ASettings: TPrimitiveSettings; AInMemFileSystem: TInMemFileSystem);
var
  MemStream: TMemoryStream;
  ContentAsStringList: TStringList;
begin
  MemStream := TMemoryStream.Create;
  try
    ContentAsStringList := TStringList.Create;
    try
      SavePrimitivesFile(ContentAsStringList, APrimitives, AOrders, ASettings);

      MemStream.Position := 0;
      ContentAsStringList.SaveToStream(MemStream);

      MemStream.Position := 0;
      AInMemFileSystem.SaveFileToMem(AFileName, MemStream.Memory, MemStream.Size);
    finally
      ContentAsStringList.Free;
    end;
  finally
    MemStream.Free;
  end;
end;


procedure TfrmClickerActions.LoadSettings(AIni: TMemIniFile);
var
  i, n: Integer;
begin
  Left := AIni.ReadInteger('ActionsWindow', 'Left', Min(Left, Screen.DesktopWidth - 60));
  Top := AIni.ReadInteger('ActionsWindow', 'Top', Min(Top, Screen.DesktopHeight - 60));
  Width := AIni.ReadInteger('ActionsWindow', 'Width', Min(Width, Screen.DesktopWidth - 40));
  Height := AIni.ReadInteger('ActionsWindow', 'Height', Min(Height, Screen.DesktopHeight - 40));

  chkStayOnTop.Checked := AIni.ReadBool('ActionsWindow', 'StayOnTop', chkStayOnTop.Checked);
  PageControlMain.ActivePageIndex := AIni.ReadInteger('ActionsWindow', 'ActivePageIndex', PageControlMain.ActivePageIndex);

  frClickerActionsArrMain.frClickerActions.frClickerFindControl.chkDisplayCroppingLines.Checked := AIni.ReadBool('ActionsWindow', 'DisplayCroppingLines.Main', True);
  frClickerActionsArrExperiment1.frClickerActions.frClickerFindControl.chkDisplayCroppingLines.Checked := AIni.ReadBool('ActionsWindow', 'DisplayCroppingLines.Exp1', True);
  frClickerActionsArrExperiment2.frClickerActions.frClickerFindControl.chkDisplayCroppingLines.Checked := AIni.ReadBool('ActionsWindow', 'DisplayCroppingLines.Exp2', True);

  chkDisplayActivity.Checked := AIni.ReadBool('ActionsWindow', 'DisplayActivity', chkDisplayActivity.Checked);

  cmbClientModeServerAddress.Clear;
  for i := 0 to AIni.ReadInteger('ActionsWindow', 'ClientModeServerAddressCount', 0) - 1 do
    cmbClientModeServerAddress.Items.Add(AIni.ReadString('ActionsWindow', 'ClientModeServerAddressCount_' + IntToStr(i), ''));

  cmbClientModeServerAddress.Text := AIni.ReadString('ActionsWindow', 'ClientModeServerAddress', cmbClientModeServerAddress.Text);
  lbeConnectTimeout.Text := AIni.ReadString('ActionsWindow', 'ConnectTimeout', lbeConnectTimeout.Text);
  chkSetExperimentsToClientMode.Checked := AIni.ReadBool('ActionsWindow', 'SetExperimentsToClientMode', chkSetExperimentsToClientMode.Checked);

  lbeServerModePort.Text := AIni.ReadString('ActionsWindow', 'ServerModePort', lbeServerModePort.Text);
  chkKeepAlive.Checked := AIni.ReadBool('ActionsWindow', 'KeepAlive', chkKeepAlive.Checked);
  cmbFilesExistence.ItemIndex := AIni.ReadInteger('ActionsWindow', 'FilesExistenceMode', cmbFilesExistence.ItemIndex);

  n := AIni.ReadInteger('ActionsWindow', 'AllowedFileExtensionsForServer.Count', 0);
  if n > 0 then
  begin
    memAllowedFileExtensionsForServer.Clear;
    for i := 0 to n - 1 do
      memAllowedFileExtensionsForServer.Lines.Add(AIni.ReadString('ActionsWindow', 'AllowedFileExtensionsForServer_' + IntToStr(i), '.bmp'));
  end;

  n := AIni.ReadInteger('ActionsWindow', 'AllowedFileDirsForServer.Count', 0);
  if n > 0 then
  begin
    memAllowedFileDirsForServer.Clear;
    for i := 0 to n - 1 do
      memAllowedFileDirsForServer.Lines.Add(AIni.ReadString('ActionsWindow', 'AllowedFileDirsForServer_' + IntToStr(i), ''));
  end
  else
    memAllowedFileDirsForServer.Lines.Add(ExtractFilePath(ParamStr(0)) + 'ActionTemplates');

  FAutoSwitchToExecutingTab := AIni.ReadBool('ActionsWindow', 'AutoSwitchToExecutingTab', True);
  chkAutoSwitchToExecutingTab.Checked := FAutoSwitchToExecutingTab;
  FAutoEnableSwitchingTabsOnDebugging := AIni.ReadBool('ActionsWindow', 'AutoEnableSwitchingTabsOnDebugging', True);
  chkAutoEnableSwitchingTabsOnDebugging.Checked := FAutoEnableSwitchingTabsOnDebugging;
  cmbImgPreviewGridType.ItemIndex := AIni.ReadInteger('ActionsWindow', 'GridType', 0);
  UpdateGridType;

  FPreviewSelectionColors.TopLeft_Valid := AIni.ReadInteger('ActionsWindow', 'TopLeft_ValidColor', CLabel_Orange);
  FPreviewSelectionColors.BotRight_Valid := AIni.ReadInteger('ActionsWindow', 'BotRight_ValidColor', CLabel_LightGreen);
  FPreviewSelectionColors.TopLeft_Invalid := AIni.ReadInteger('ActionsWindow', 'TopLeft_InvalidColor', clRed);
  FPreviewSelectionColors.BotRight_Invalid := AIni.ReadInteger('ActionsWindow', 'BotRight_InvalidColor', clMaroon);

  FFontFinderSettings.ListOfUsedFonts := FastReplace_45ToReturn(AIni.ReadString('ActionsWindow', 'ListOfUsedFonts', 'Tahoma'#13#10'Segoe UI'));
  FFontFinderSettings.MinFontSize := AIni.ReadInteger('ActionsWindow', 'MinFontSize', 7);
  FFontFinderSettings.MaxFontSize := AIni.ReadInteger('ActionsWindow', 'MaxFontSize', 9);
  FFontFinderSettings.ShowAllFonts := AIni.ReadBool('ActionsWindow', 'ShowAllFonts', False);
  FFontFinderSettings.SortResultsByHistogram := AIni.ReadBool('ActionsWindow', 'SortResultsByHistogram', False);
  FFontFinderSettings.WinRect.Left := AIni.ReadInteger('FontFinderSettingsWindow', 'Left', Left);
  FFontFinderSettings.WinRect.Top := AIni.ReadInteger('FontFinderSettingsWindow', 'Top', Top);
  FFontFinderSettings.WinRect.Width := AIni.ReadInteger('FontFinderSettingsWindow', 'Width', Width);
  FFontFinderSettings.WinRect.Height := AIni.ReadInteger('FontFinderSettingsWindow', 'Height', Height);

  SetLength(FFontFinderSettings.ColWidths, AIni.ReadInteger('FontFinderSettingsWindow', 'ColWidthCount', 0));
  for i := 0 to Length(FFontFinderSettings.ColWidths) - 1 do
    FFontFinderSettings.ColWidths[i] := AIni.ReadInteger('FontFinderSettingsWindow', 'ColWidth_' + IntToStr(i), 200);

  frClickerActionsArrMain.LoadSettings(AIni, 'ActionsWindow', 'Main');
  frClickerActionsArrExperiment1.LoadSettings(AIni, 'ActionsWindow', 'Exp1');
  frClickerActionsArrExperiment2.LoadSettings(AIni, 'ActionsWindow', 'Exp2');

  colcmbTopLeftValid.Selected := FPreviewSelectionColors.TopLeft_Valid;
  colcmbBotRightValid.Selected := FPreviewSelectionColors.BotRight_Valid;
  colcmbTopLeftInvalid.Selected := FPreviewSelectionColors.TopLeft_Invalid;
  colcmbBotRightInvalid.Selected := FPreviewSelectionColors.BotRight_Invalid;

  colcmbTopLeftValid.AutoSelect := False;
  colcmbBotRightValid.AutoSelect := False;
  colcmbTopLeftInvalid.AutoSelect := False;
  colcmbBotRightInvalid.AutoSelect := False;
  colcmbTopLeftValid.ItemIndex := colcmbTopLeftValid.Items.IndexOfObject(TObject(TColor(FPreviewSelectionColors.TopLeft_Valid)));
  colcmbBotRightValid.ItemIndex := colcmbBotRightValid.Items.IndexOfObject(TObject(TColor(FPreviewSelectionColors.BotRight_Valid)));
  colcmbTopLeftInvalid.ItemIndex := colcmbTopLeftInvalid.Items.IndexOfObject(TObject(TColor(FPreviewSelectionColors.TopLeft_Invalid)));
  colcmbBotRightInvalid.ItemIndex := colcmbBotRightInvalid.Items.IndexOfObject(TObject(TColor(FPreviewSelectionColors.BotRight_Invalid)));
  colcmbTopLeftValid.AutoSelect := True;
  colcmbBotRightValid.AutoSelect := True;
  colcmbTopLeftInvalid.AutoSelect := True;
  colcmbBotRightInvalid.AutoSelect := True;

  FullTemplatesDir := AIni.ReadString('Dirs', 'FullTemplatesDir', '$AppDir$\ActionTemplates');
  BMPsDir := AIni.ReadString('Dirs', 'BMPsDir', '');

  lbePathToTemplates.Text := StringReplace(FullTemplatesDir, ExtractFileDir(ParamStr(0)), '$AppDir$', [rfReplaceAll]);

  frClickerActionsArrExperiment1.AllowedFileDirsForServer := memAllowedFileDirsForServer.Lines.Text;
  frClickerActionsArrExperiment2.AllowedFileDirsForServer := memAllowedFileDirsForServer.Lines.Text;
  frClickerActionsArrMain.AllowedFileDirsForServer := memAllowedFileDirsForServer.Lines.Text;
  frClickerActionsArrExperiment1.AllowedFileExtensionsForServer := memAllowedFileExtensionsForServer.Lines.Text;
  frClickerActionsArrExperiment2.AllowedFileExtensionsForServer := memAllowedFileExtensionsForServer.Lines.Text;
  frClickerActionsArrMain.AllowedFileExtensionsForServer := memAllowedFileExtensionsForServer.Lines.Text;

  FRecentTemplates.Clear;
  n := AIni.ReadInteger('RecentFiles', 'Count', 0);       //AIni.ReadSection('RecentFiles', FRecentTemplates); would be better, because it doesn't require every item to have its own key. That may be available after replacing TMemIniFile with UIClicker's ini.
  for i := 0 to n - 1 do
    FRecentTemplates.Add(AIni.ReadString('RecentFiles', 'File_' + IntToStr(i), ''));
end;


procedure TfrmClickerActions.SaveSettings(AIni: TMemIniFile);
var
  i, n: Integer;
begin
  AIni.WriteInteger('ActionsWindow', 'Left', Min(Left, Screen.DesktopWidth - 60));
  AIni.WriteInteger('ActionsWindow', 'Top', Min(Top, Screen.DesktopHeight - 60));
  AIni.WriteInteger('ActionsWindow', 'Width', Min(Width, Screen.DesktopWidth - 40));
  AIni.WriteInteger('ActionsWindow', 'Height', Min(Height, Screen.DesktopHeight - 40));

  AIni.WriteBool('ActionsWindow', 'StayOnTop', chkStayOnTop.Checked);
  AIni.WriteInteger('ActionsWindow', 'ActivePageIndex', PageControlMain.ActivePageIndex);

  AIni.WriteBool('ActionsWindow', 'DisplayCroppingLines.Main', frClickerActionsArrMain.frClickerActions.frClickerFindControl.chkDisplayCroppingLines.Checked);
  AIni.WriteBool('ActionsWindow', 'DisplayCroppingLines.Exp1', frClickerActionsArrExperiment1.frClickerActions.frClickerFindControl.chkDisplayCroppingLines.Checked);
  AIni.WriteBool('ActionsWindow', 'DisplayCroppingLines.Exp2', frClickerActionsArrExperiment2.frClickerActions.frClickerFindControl.chkDisplayCroppingLines.Checked);

  AIni.WriteBool('ActionsWindow', 'DisplayActivity', chkDisplayActivity.Checked);

  AIni.WriteInteger('ActionsWindow', 'ClientModeServerAddressCount', cmbClientModeServerAddress.Items.Count);
  for i := 0 to cmbClientModeServerAddress.Items.Count - 1 do
    AIni.WriteString('ActionsWindow', 'ClientModeServerAddressCount_' + IntToStr(i), cmbClientModeServerAddress.Items.Strings[i]);

  AIni.WriteString('ActionsWindow', 'ClientModeServerAddress', cmbClientModeServerAddress.Text);
  AIni.WriteString('ActionsWindow', 'ConnectTimeout', lbeConnectTimeout.Text);
  AIni.WriteBool('ActionsWindow', 'SetExperimentsToClientMode', chkSetExperimentsToClientMode.Checked);

  AIni.WriteString('ActionsWindow', 'ServerModePort', lbeServerModePort.Text);
  AIni.WriteBool('ActionsWindow', 'KeepAlive', chkKeepAlive.Checked);
  AIni.WriteInteger('ActionsWindow', 'FilesExistenceMode', cmbFilesExistence.ItemIndex);

  n := memAllowedFileExtensionsForServer.Lines.Count;
  AIni.WriteInteger('ActionsWindow', 'AllowedFileExtensionsForServer.Count', n);

  for i := 0 to n - 1 do
    AIni.WriteString('ActionsWindow', 'AllowedFileExtensionsForServer_' + IntToStr(i), memAllowedFileExtensionsForServer.Lines.Strings[i]);

  n := memAllowedFileDirsForServer.Lines.Count;
  AIni.WriteInteger('ActionsWindow', 'AllowedFileDirsForServer.Count', n);

  for i := 0 to n - 1 do
    AIni.WriteString('ActionsWindow', 'AllowedFileDirsForServer_' + IntToStr(i), memAllowedFileDirsForServer.Lines.Strings[i]);

  AIni.WriteBool('ActionsWindow', 'AutoSwitchToExecutingTab', FAutoSwitchToExecutingTab);
  AIni.WriteBool('ActionsWindow', 'AutoEnableSwitchingTabsOnDebugging', FAutoEnableSwitchingTabsOnDebugging);
  AIni.WriteInteger('ActionsWindow', 'GridType', cmbImgPreviewGridType.ItemIndex);

  AIni.WriteString('Dirs', 'BMPsDir', BMPsDir);
  AIni.WriteString('Dirs', 'FullTemplatesDir', StringReplace(FullTemplatesDir, ExtractFileDir(ParamStr(0)), '$AppDir$', [rfReplaceAll]));

  AIni.WriteInteger('ActionsWindow', 'TopLeft_ValidColor', FPreviewSelectionColors.TopLeft_Valid);
  AIni.WriteInteger('ActionsWindow', 'BotRight_ValidColor', FPreviewSelectionColors.BotRight_Valid);
  AIni.WriteInteger('ActionsWindow', 'TopLeft_InvalidColor', FPreviewSelectionColors.TopLeft_Invalid );
  AIni.WriteInteger('ActionsWindow', 'BotRight_InvalidColor', FPreviewSelectionColors.BotRight_Invalid);

  AIni.WriteString('ActionsWindow', 'ListOfUsedFonts', FastReplace_ReturnTo45(FFontFinderSettings.ListOfUsedFonts));
  AIni.WriteInteger('ActionsWindow', 'MinFontSize', FFontFinderSettings.MinFontSize);
  AIni.WriteInteger('ActionsWindow', 'MaxFontSize', FFontFinderSettings.MaxFontSize);
  AIni.WriteBool('ActionsWindow', 'ShowAllFonts', FFontFinderSettings.ShowAllFonts);
  AIni.WriteBool('ActionsWindow', 'SortResultsByHistogram', FFontFinderSettings.SortResultsByHistogram);
  AIni.WriteInteger('ActionsWindow', 'MaxFontSize', FFontFinderSettings.MaxFontSize);

  AIni.WriteInteger('FontFinderSettingsWindow', 'Left', FFontFinderSettings.WinRect.Left);
  AIni.WriteInteger('FontFinderSettingsWindow', 'Top', FFontFinderSettings.WinRect.Top);
  AIni.WriteInteger('FontFinderSettingsWindow', 'Width', FFontFinderSettings.WinRect.Width);
  AIni.WriteInteger('FontFinderSettingsWindow', 'Height', FFontFinderSettings.WinRect.Height);

  AIni.WriteInteger('FontFinderSettingsWindow', 'ColWidthCount', Length(FFontFinderSettings.ColWidths));
  for i := 0 to Length(FFontFinderSettings.ColWidths) - 1 do
    AIni.WriteInteger('FontFinderSettingsWindow', 'ColWidth_' + IntToStr(i), FFontFinderSettings.ColWidths[i]);

  frClickerActionsArrMain.SaveSettings(AIni, 'ActionsWindow', 'Main');
  frClickerActionsArrExperiment1.SaveSettings(AIni, 'ActionsWindow', 'Exp1');
  frClickerActionsArrExperiment2.SaveSettings(AIni, 'ActionsWindow', 'Exp2');

  n := FRecentTemplates.Count;
  AIni.WriteInteger('RecentFiles', 'Count', n);       //AIni.ReadSection('RecentFiles', FRecentTemplates); would be better, because it doesn't require every item to have its own key. That may be available after replacing TMemIniFile with UIClicker's ini.
  for i := 0 to n - 1 do
    AIni.WriteString('RecentFiles', 'File_' + IntToStr(i), FRecentTemplates.Strings[i]);
end;


procedure TfrmClickerActions.AddToLog(s: string);
begin
  frClickerActionsArrMain.AddToLog(DateTimeToStr(Now) + '  ' + s);
end;


procedure TfrmClickerActions.CreateRemainingUIComponents;
var
  MenuItem: TMenuItem;
begin
  frClickerActionsArrExperiment1 := TfrClickerActionsArr.Create(Self);
  frClickerActionsArrExperiment1.Name := 'frClickerActionsArrExp1';
  frClickerActionsArrExperiment1.Parent := TabSheetExperiments1;
  frClickerActionsArrExperiment1.Left := 4;
  frClickerActionsArrExperiment1.Top := 12;
  frClickerActionsArrExperiment1.Width := TabSheetExperiments1.Width - 8;
  frClickerActionsArrExperiment1.Height := TabSheetExperiments1.Height - 10;
  frClickerActionsArrExperiment1.TabOrder := 0;
  frClickerActionsArrExperiment1.TabStop := True;
  frClickerActionsArrExperiment1.StackLevel := 0;

  frClickerActionsArrExperiment2 := TfrClickerActionsArr.Create(Self);
  frClickerActionsArrExperiment2.Name := 'frClickerActionsArrExp2';
  frClickerActionsArrExperiment2.Parent := TabSheetExperiments2;
  frClickerActionsArrExperiment2.Left := 4;
  frClickerActionsArrExperiment2.Top := 12;
  frClickerActionsArrExperiment2.Width := frClickerActionsArrExperiment1.Width;
  frClickerActionsArrExperiment2.Height := frClickerActionsArrExperiment1.Height;
  frClickerActionsArrExperiment2.TabOrder := 0;
  frClickerActionsArrExperiment2.TabStop := True;
  frClickerActionsArrExperiment2.StackLevel := 0;

  lblExp1.Free;
  lblExp2.Free;

  FControlPlayerPopup := TPopupMenu.Create(Self);

  MenuItem := TMenuItem.Create(FControlPlayerPopup);
  MenuItem.Caption := 'Open current template as experiment 1';
  MenuItem.Name := 'MenuItemOpenTemplateAsExp1';
  MenuItem.OnClick := MenuItemOpenTemplateAsExp1Click;
  FControlPlayerPopup.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FControlPlayerPopup);
  MenuItem.Caption := 'Open current template as experiment 2';
  MenuItem.Name := 'MenuItemOpenTemplateAsExp2';
  MenuItem.OnClick := MenuItemOpenTemplateAsExp2Click;
  FControlPlayerPopup.Items.Add(MenuItem);

  PageControlPlayer.PopupMenu := FControlPlayerPopup;

  FControlPlayerPopup := TPopupMenu.Create(Self); //dummy menu, to prevent displaying the above menu all over the frame
  TabSheetExecMainPlayer.PopupMenu := FControlPlayerPopup;
end;


procedure TfrmClickerActions.FormCreate(Sender: TObject);
var
  OSVerNumber, OSVerStr: string;
  hmod: THandle;
  AdminStatus: string;
  i: TDisplayGridLineOption;
begin
  CreateRemainingUIComponents;

  AdminStatus := GetIsUserAnAdmin;
  Caption := Caption + AdminStatus;
  lblAdminStatus.Caption := AdminStatus;
  lblAdminStatus.Show;

  FBMPsDir := '';
  FFullTemplatesDir := 'not set'; //'$AppDir$\ActionTemplates';

  FInMemFileSystem := TInMemFileSystem.Create;
  FInMemFileSystem.OnComputeInMemFileHash := HandleOnComputeInMemFileHash;

  FRenderedInMemFileSystem := TInMemFileSystem.Create;
  FRenderedInMemFileSystem.OnComputeInMemFileHash := HandleOnComputeInMemFileHash;

  FPluginsInMemFileSystem := TInMemFileSystem.Create;
  FPluginsInMemFileSystem.OnComputeInMemFileHash := HandleOnComputeInMemFileHash;

  SetLength(FDecDecHashPluginInMemFSArr, 0);

  {$IFDEF Windows}
    InitializeCriticalSection(FDecDecHashArrCritSec);
  {$ELSE}
    InitCriticalSection(FDecDecHashArrCritSec);
  {$ENDIF}

  FFileAvailabilityFIFO := TPollingFIFO.Create;
  FAutoSwitchToExecutingTab := False;
  FAutoEnableSwitchingTabsOnDebugging := False;

  FFirstDisplaying := False;

  FOnReLoadSettings := nil;
  FOnCopyControlTextAndClassFromMainWindow := nil;
  FOnRecordComponent := nil;
  FOnGetCurrentlyRecordedScreenShotImage := nil;
  FOnLoadBitmap := nil;
  FOnLoadPrimitivesFile := nil;
  FOnSavePrimitivesFile := nil;

  FOnGetSelfHandles := nil;

  FOnFileExists := nil;
  FOnTClkIniReadonlyFileCreate := nil;
  FOnTClkIniFileCreate := nil;
  FOnSaveTemplateToFile := nil;

  FOnSetOpenDialogMultiSelect := nil;
  FOnSetOpenDialogInitialDir := nil;
  FOnOpenDialogExecute := nil;
  FOnGetOpenDialogFileName := nil;
  FOnSetSaveDialogInitialDir := nil;
  FOnSaveDialogExecute := nil;
  FOnGetSaveDialogFileName := nil;
  FOnSetSaveDialogFileName := nil;

  FOnSetPictureSetOpenDialogMultiSelect := nil;
  FOnSetPictureOpenDialogInitialDir := nil;
  FOnPictureOpenDialogExecute := nil;
  FOnGetPictureOpenDialogFileName := nil;
  FOnGenerateAndSaveTreeWithWinInterp := nil;
  FOnSetWinInterpOption := nil;

  FStopAllActionsOnDemand := False;
  PageControlMain.ActivePageIndex := 0;
  PageControlExecMode.ActivePageIndex := 0;
  PageControlMain.Caption := 'Main';
  PageControlExecMode.Caption := 'ExecMode';
  PageControlPlayer.Caption := 'Player';

  FRecentTemplates := TStringList.Create;

  colcmbTopLeftValid.AddItem('clOrange', TObject(QWord(CLabel_Orange)));
  colcmbTopLeftValid.AddItem('clLightGreen', TObject(QWord(CLabel_LightGreen)));
  colcmbBotRightValid.AddItem('clOrange', TObject(QWord(CLabel_Orange)));
  colcmbBotRightValid.AddItem('clLightGreen', TObject(QWord(CLabel_LightGreen)));
  colcmbTopLeftInvalid.AddItem('clOrange', TObject(QWord(CLabel_Orange)));
  colcmbTopLeftInvalid.AddItem('clLightGreen', TObject(QWord(CLabel_LightGreen)));
  colcmbBotRightInvalid.AddItem('clOrange', TObject(QWord(CLabel_Orange)));
  colcmbBotRightInvalid.AddItem('clLightGreen', TObject(QWord(CLabel_LightGreen)));

  frClickerActionsArrMain := TfrClickerActionsArr.Create(Self);
  frClickerActionsArrMain.Parent := scrboxMain;
  scrboxMain.Tag := PtrInt(frClickerActionsArrMain);
  frClickerActionsArrMain.Left := 0;
  frClickerActionsArrMain.Top := 0;
  frClickerActionsArrMain.Width := scrboxMain.Width - 4;
  frClickerActionsArrMain.Height := scrboxMain.Height;
  frClickerActionsArrMain.Constraints.MinWidth := frClickerActionsArrMain.Width;
  frClickerActionsArrMain.Constraints.MinHeight := frClickerActionsArrMain.Height;
  frClickerActionsArrMain.Anchors := [akLeft, akTop, akRight, akBottom];
  frClickerActionsArrMain.StackLevel := 0;

  frClickerActionsArrMain.PopupMenu := nil;
  frClickerActionsArrExperiment1.PopupMenu := nil;
  frClickerActionsArrExperiment2.PopupMenu := nil;

  frClickerActionsArrExperiment1.InitFrame;
  frClickerActionsArrExperiment2.InitFrame;
  frClickerActionsArrMain.InitFrame;
  PageControlMain.Anchors := PageControlMain.Anchors + [akRight];

  for i := Low(TDisplayGridLineOption) to High(TDisplayGridLineOption) do
    cmbImgPreviewGridType.Items.Add(CDisplayGridLineOptionStr[i]);

  cmbImgPreviewGridType.ItemIndex := Ord(loDot);

  memVariables.Lines.Add('$Screen_Width$=' + IntToStr(Screen.Width));
  memVariables.Lines.Add('$Screen_Height$=' + IntToStr(Screen.Height));
  memVariables.Lines.Add('$Desktop_Width$=' + IntToStr(Screen.DesktopWidth));
  memVariables.Lines.Add('$Desktop_Height$=' + IntToStr(Screen.DesktopHeight));

  memVariables.Lines.Add('$Color_Highlight$=' + IntToHex(GetSysColor(COLOR_HIGHLIGHT), 6));
  memVariables.Lines.Add('$Color_BtnFace$=' + IntToHex(GetSysColor(COLOR_BTNFACE), 6));
  memVariables.Lines.Add('$Color_ActiveCaption$=' + IntToHex(GetSysColor(COLOR_ACTIVECAPTION), 6));
  memVariables.Lines.Add('$Color_InactiveCaption$=' + IntToHex(GetSysColor(COLOR_INACTIVECAPTION), 6));
  memVariables.Lines.Add('$Color_Window$=' + IntToHex(GetSysColor(COLOR_WINDOW), 6));
  memVariables.Lines.Add('$Color_WindowText$=' + IntToHex(GetSysColor(COLOR_WINDOWTEXT), 6));
  memVariables.Lines.Add('$Color_GrayText$=' + IntToHex(GetSysColor(COLOR_GRAYTEXT), 6));
  memVariables.Lines.Add('$Color_GradientActiveCaption$=' + IntToHex(GetSysColor(COLOR_GRADIENTACTIVECAPTION), 6));
  memVariables.Lines.Add('$Color_GradientInactiveCaption$=' + IntToHex(GetSysColor(COLOR_GRADIENTINACTIVECAPTION), 6));
  memVariables.Lines.Add('$Color_ScrollBar$=' + IntToHex(GetSysColor(COLOR_SCROLLBAR), 6));
  memVariables.Lines.Add('$Color_3DDkShadow$=' + IntToHex(GetSysColor(COLOR_3DDKSHADOW), 6));
  memVariables.Lines.Add('$Color_3DLight$=' + IntToHex(GetSysColor(COLOR_3DLIGHT), 6));
  memVariables.Lines.Add('$Color_WindowFrame$=' + IntToHex(GetSysColor(COLOR_WINDOWFRAME), 6));

  {$IFDEF Windows}
    //See MS docs for how to read Win32MajorVersion. It's not very reliable.
    OSVerNumber := IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion) + '.' + IntToStr(Win32BuildNumber);
    OSVerStr := 'Unknown';

    hmod := LoadLibraryEx(PChar(ParamStr(0)), 0, LOAD_LIBRARY_AS_DATAFILE);
    try
      if (hmod <> 0) and (FindResource(hmod, MakeIntResource(1), RT_MANIFEST) > 0) then
      begin
        //has manifest
        if (Win32MajorVersion = 6) and (Win32MinorVersion = 3) then
          OSVerStr := 'Win8.1';

        if (Win32MajorVersion = 6) and (Win32MinorVersion = 2) then
          OSVerStr := 'Win8';

        if (Win32MajorVersion = 10) {and (Win32MinorVersion = 2)} then
          OSVerStr := 'Win11';

        if (Win32MajorVersion = 10) {and (Win32MinorVersion = 2)} then
          OSVerStr := 'Win10';
      end
      else
      begin
        if (Win32MajorVersion = 6) and (Win32MinorVersion = 2) then
          OSVerStr := 'Win8.1';  //or Win8 or 10  //returning Win8.1 as it may be found more often
      end;

      if (Win32MajorVersion = 6) and (Win32MinorVersion = 1) then
        OSVerStr := 'Win7';

      if (Win32MajorVersion = 6) and (Win32MinorVersion = 0) then
        OSVerStr := 'WinVista';

      if (Win32MajorVersion = 5) and ((Win32MinorVersion = 1) or (Win32MinorVersion = 2)) then
        OSVerStr := 'WinXP';
    finally
      FreeLibrary(hmod);
    end;
  {$ELSE}
    OSVerNumber := '?';
    OSVerStr := 'unknown';  //probably Linux
  {$ENDIF}

  memVariables.Lines.Add('$OSVerNumber$=' + OSVerNumber);
  memVariables.Lines.Add('$OSVer$=' + OSVerStr);
  memVariables.Lines.Add('$ExitCode$=');
  memVariables.Lines.Add('$AppDir$=' + ExtractFileDir(ParamStr(0)));
  memVariables.Lines.Add('$TemplateDir$=' + FFullTemplatesDir);
  memVariables.Lines.Add('$SelfTemplateDir$=' + frClickerActionsArrMain.FileName);   //frClickerActionsArrMain.FileName is empty here, because no template is being executed at this point

  {$IFDEF CPU64}
    memVariables.Lines.Add('$AppBitness$=x86_64');
  {$ELSE}
    memVariables.Lines.Add('$AppBitness$=i386');
  {$ENDIF}

  {$IFDEF Windows}
    {$IFDEF CPU64}
      memVariables.Lines.Add('$OSBitness$=win64');
    {$ELSE}
      memVariables.Lines.Add('$OSBitness$=win32');
    {$ENDIF}
  {$ENDIF}

  memVariables.Lines.Add('$SelfActionName$=');
  memVariables.Lines.Add('$SelfActionIndex$=-1');

  frClickerActionsArrExperiment1.SetVariables(memVariables.Lines);
  frClickerActionsArrExperiment2.SetVariables(memVariables.Lines);
  frClickerActionsArrMain.SetVariables(memVariables.Lines);

  frClickerActionsArrExperiment1.OnCallTemplate := nil;
  frClickerActionsArrExperiment2.OnCallTemplate := nil;
  frClickerActionsArrMain.OnCallTemplate := frClickerActionsArrOnCallTemplate;

  //Set above, when loading settings from ini.
  //frClickerActionsArrExperiment1.AllowedFileDirsForServer := memAllowedFileDirsForServer.Lines.Text;
  //frClickerActionsArrExperiment2.AllowedFileDirsForServer := memAllowedFileDirsForServer.Lines.Text;
  //frClickerActionsArrMain.AllowedFileDirsForServer := memAllowedFileDirsForServer.Lines.Text;
  //frClickerActionsArrExperiment1.AllowedFileExtensionsForServer := memAllowedFileExtensionsForServer.Lines.Text;
  //frClickerActionsArrExperiment2.AllowedFileExtensionsForServer := memAllowedFileExtensionsForServer.Lines.Text;
  //frClickerActionsArrMain.AllowedFileExtensionsForServer := memAllowedFileExtensionsForServer.Lines.Text;

  frClickerActionsArrExperiment1.InMemFS := FInMemFileSystem;
  frClickerActionsArrExperiment2.InMemFS := FInMemFileSystem;
  frClickerActionsArrMain.InMemFS := FInMemFileSystem;

  frClickerActionsArrExperiment1.ExtRenderingInMemFS := FRenderedInMemFileSystem;
  frClickerActionsArrExperiment2.ExtRenderingInMemFS := FRenderedInMemFileSystem;
  frClickerActionsArrMain.ExtRenderingInMemFS := FRenderedInMemFileSystem;

  {$IFDEF MemPlugins}
    frClickerActionsArrExperiment1.MemPluginsInMemFS := FPluginsInMemFileSystem;
    frClickerActionsArrExperiment2.MemPluginsInMemFS := FPluginsInMemFileSystem;
    frClickerActionsArrMain.MemPluginsInMemFS := FPluginsInMemFileSystem;
  {$ENDIF}

  //do not set FRenderedInMemFileSystem here, as it is used for externally rendered images, not for other files

  frClickerActionsArrExperiment1.GridDrawingOption := TDisplayGridLineOption(cmbImgPreviewGridType.ItemIndex);
  frClickerActionsArrExperiment2.GridDrawingOption := TDisplayGridLineOption(cmbImgPreviewGridType.ItemIndex);
  frClickerActionsArrMain.GridDrawingOption := TDisplayGridLineOption(cmbImgPreviewGridType.ItemIndex);

  //frClickerActionsArrExperiment1.PreviewSelectionColors := FPreviewSelectionColors;  //do not set the PreviewSelectionColors property here, because the objects are not created yet
  //frClickerActionsArrExperiment2.PreviewSelectionColors := FPreviewSelectionColors;
  //frClickerActionsArrMain.PreviewSelectionColors := FPreviewSelectionColors;
  FPreviewSelectionColors.TopLeft_Valid := CLabel_Orange;
  FPreviewSelectionColors.BotRight_Valid := CLabel_LightGreen;
  FPreviewSelectionColors.TopLeft_Invalid := clRed;
  FPreviewSelectionColors.BotRight_Invalid := clMaroon;

  frClickerActionsArrExperiment1.OnExecuteRemoteActionAtIndex := nil;
  frClickerActionsArrExperiment2.OnExecuteRemoteActionAtIndex := nil;
  //frClickerActionsArrMain.OnExecuteRemoteActionAtIndex := frClickerActionsArrOnExecuteRemoteActionAtIndex;

  frClickerActionsArrExperiment1.frClickerActions.PredefinedVarCount := memVariables.Lines.Count;
  frClickerActionsArrExperiment2.frClickerActions.PredefinedVarCount := memVariables.Lines.Count;
  frClickerActionsArrMain.frClickerActions.PredefinedVarCount := memVariables.Lines.Count;

  frClickerActionsArrExperiment1.frClickerActions.PasteDebugValuesListFromMainExecutionList1.Enabled := True;
  frClickerActionsArrExperiment2.frClickerActions.PasteDebugValuesListFromMainExecutionList1.Enabled := True;

  frClickerActionsArrExperiment1.StopAllActionsOnDemandFromParent := nil;
  frClickerActionsArrExperiment2.StopAllActionsOnDemandFromParent := nil;
  frClickerActionsArrMain.StopAllActionsOnDemandFromParent := @FStopAllActionsOnDemand;

  frClickerActionsArrMain.OnCopyControlTextAndClassFromMainWindow := HandleOnCopyControlTextAndClassFromMainWindow;
  frClickerActionsArrMain.OnGetExtraSearchAreaDebuggingImageWithStackLevel := HandleOnGetExtraSearchAreaDebuggingImageWithStackLevel;
  frClickerActionsArrMain.OnWaitForFileAvailability := HandleOnWaitForFileAvailability;
  frClickerActionsArrMain.OnWaitForMultipleFilesAvailability := HandleOnWaitForMultipleFilesAvailability;
  frClickerActionsArrMain.OnWaitForBitmapsAvailability := HandleOnWaitForBitmapsAvailability;
  frClickerActionsArrMain.OnTerminateWaitForMultipleFilesAvailability := HandleOnTerminateWaitForMultipleFilesAvailability;
  frClickerActionsArrMain.OnLoadBitmap := HandleOnLoadBitmap;
  frClickerActionsArrMain.OnLoadRenderedBitmap := HandleOnLoadRenderedBitmap;
  frClickerActionsArrMain.OnLoadPluginFromInMemFS := HandleOnLoadPluginFromInMemFS;
  frClickerActionsArrMain.OnGetListOfExternallyRenderedImages := HandleOnGetListOfExternallyRenderedImages;

  {$IFDEF MemPlugins}
    frClickerActionsArrMain.OnGetListOfInMemPlugins := HandleOnGetListOfInMemPlugins;
    frClickerActionsArrMain.OnLoadPluginFromDiskToPluginInMemFileSystem := HandleOnLoadPluginFromDiskToPluginInMemFileSystem;
  {$ENDIF}

  frClickerActionsArrMain.OnRenderBmpExternally := HandleOnRenderBmpExternally;
  frClickerActionsArrMain.OnLoadPrimitivesFile := HandleOnLoadPrimitivesFile;
  frClickerActionsArrMain.OnSavePrimitivesFile := HandleOnSavePrimitivesFile;
  frClickerActionsArrMain.OnFileExists := HandleOnFileExists;
  frClickerActionsArrMain.ActionExecution.OnGetSelfHandles := HandleOnGetSelfHandles;
  frClickerActionsArrMain.OnTClkIniReadonlyFileCreate := HandleOnTClkIniReadonlyFileCreate;
  frClickerActionsArrMain.OnTClkIniFileCreate := HandleOnTClkIniFileCreate;
  frClickerActionsArrMain.OnSaveTemplateToFile := HandleOnSaveTemplateToFile;

  frClickerActionsArrMain.OnSetOpenDialogMultiSelect := HandleOnSetOpenDialogMultiSelect;
  frClickerActionsArrMain.OnSetOpenDialogInitialDir := HandleOnSetOpenDialogInitialDir;
  frClickerActionsArrMain.OnOpenDialogExecute := HandleOnOpenDialogExecute;
  frClickerActionsArrMain.OnGetOpenDialogFileName := HandleOnGetOpenDialogFileName;
  frClickerActionsArrMain.OnSetSaveDialogInitialDir := HandleOnSetSaveDialogInitialDir;
  frClickerActionsArrMain.OnSaveDialogExecute := HandleOnSaveDialogExecute;
  frClickerActionsArrMain.OnGetSaveDialogFileName := HandleOnGetSaveDialogFileName;
  frClickerActionsArrMain.OnSetSaveDialogFileName := HandleOnSetSaveDialogFileName;

  frClickerActionsArrMain.OnSetPictureOpenDialogInitialDir := HandleOnSetPictureOpenDialogInitialDir;
  frClickerActionsArrMain.OnSetPictureSetOpenDialogMultiSelect := HandleOnSetPictureSetOpenDialogMultiSelect;
  frClickerActionsArrMain.OnPictureOpenDialogExecute := HandleOnPictureOpenDialogExecute;
  frClickerActionsArrMain.OnGetPictureOpenDialogFileName := HandleOnGetPictureOpenDialogFileName;

  frClickerActionsArrMain.OnGetGridDrawingOption := HandleOnGetGridDrawingOption;
  frClickerActionsArrMain.OnGetFontFinderSettings := HandleOnGetFontFinderSettings;
  frClickerActionsArrMain.OnSetFontFinderSettings := HandleOnSetFontFinderSettings;

  frClickerActionsArrMain.OnRetrieveRenderedBmpFromServer := HandleOnRetrieveRenderedBmpFromServer;
  frClickerActionsArrMain.OnOpenCalledTemplateInExperimentTab := HandleOnOpenCalledTemplateInExperimentTab;
  frClickerActionsArrMain.OnSaveFileToExtRenderingInMemFS := HandleOnSaveFileToExtRenderingInMemFS;

  frClickerActionsArrMain.OnAddFileNameToRecent := HandleOnAddFileNameToRecent;
  frClickerActionsArrMain.OnGetListOfRecentFiles := HandleOnGetListOfRecentFiles;
  frClickerActionsArrMain.OnGenerateAndSaveTreeWithWinInterp := HandleOnGenerateAndSaveTreeWithWinInterp;
  frClickerActionsArrMain.OnSetWinInterpOption := HandleOnSetWinInterpOption;
  frClickerActionsArrMain.OnGetListeningPort := HandleOnGetListeningPort;

  frClickerActionsArrExperiment1.frClickerActions.PasteDebugValuesListFromMainExecutionList1.OnClick := frClickerActionsArrExperiment1PasteDebugValuesListFromMainExecutionList1Click;
  frClickerActionsArrExperiment2.frClickerActions.PasteDebugValuesListFromMainExecutionList1.OnClick := frClickerActionsArrExperiment2PasteDebugValuesListFromMainExecutionList1Click;
  frClickerActionsArrExperiment1.OnCopyControlTextAndClassFromMainWindow := HandleOnCopyControlTextAndClassFromMainWindow;
  frClickerActionsArrExperiment2.OnCopyControlTextAndClassFromMainWindow := HandleOnCopyControlTextAndClassFromMainWindow;
  frClickerActionsArrExperiment1.OnGetExtraSearchAreaDebuggingImageWithStackLevel := HandleOnGetExtraSearchAreaDebuggingImageWithStackLevel;
  frClickerActionsArrExperiment2.OnGetExtraSearchAreaDebuggingImageWithStackLevel := HandleOnGetExtraSearchAreaDebuggingImageWithStackLevel;

  frClickerActionsArrExperiment1.OnWaitForFileAvailability := HandleOnWaitForFileAvailability;
  frClickerActionsArrExperiment2.OnWaitForFileAvailability := HandleOnWaitForFileAvailability;
  frClickerActionsArrExperiment1.OnWaitForMultipleFilesAvailability := HandleOnWaitForMultipleFilesAvailability;
  frClickerActionsArrExperiment2.OnWaitForMultipleFilesAvailability := HandleOnWaitForMultipleFilesAvailability;
  frClickerActionsArrExperiment1.OnWaitForBitmapsAvailability := HandleOnWaitForBitmapsAvailability;
  frClickerActionsArrExperiment2.OnWaitForBitmapsAvailability := HandleOnWaitForBitmapsAvailability;
  frClickerActionsArrExperiment1.OnTerminateWaitForMultipleFilesAvailability := HandleOnTerminateWaitForMultipleFilesAvailability;
  frClickerActionsArrExperiment2.OnTerminateWaitForMultipleFilesAvailability := HandleOnTerminateWaitForMultipleFilesAvailability;
  frClickerActionsArrExperiment1.OnLoadBitmap := HandleOnLoadBitmap;
  frClickerActionsArrExperiment2.OnLoadBitmap := HandleOnLoadBitmap;
  frClickerActionsArrExperiment1.OnLoadRenderedBitmap := HandleOnLoadRenderedBitmap;
  frClickerActionsArrExperiment2.OnLoadRenderedBitmap := HandleOnLoadRenderedBitmap;
  frClickerActionsArrExperiment1.OnLoadPluginFromInMemFS := HandleOnLoadPluginFromInMemFS;
  frClickerActionsArrExperiment2.OnLoadPluginFromInMemFS := HandleOnLoadPluginFromInMemFS;
  frClickerActionsArrExperiment1.OnGetListOfExternallyRenderedImages := HandleOnGetListOfExternallyRenderedImages;
  frClickerActionsArrExperiment2.OnGetListOfExternallyRenderedImages := HandleOnGetListOfExternallyRenderedImages;

  {$IFDEF MemPlugins}
    frClickerActionsArrExperiment1.OnGetListOfInMemPlugins := HandleOnGetListOfInMemPlugins;
    frClickerActionsArrExperiment2.OnGetListOfInMemPlugins := HandleOnGetListOfInMemPlugins;

    frClickerActionsArrExperiment1.OnLoadPluginFromDiskToPluginInMemFileSystem := HandleOnLoadPluginFromDiskToPluginInMemFileSystem;
    frClickerActionsArrExperiment2.OnLoadPluginFromDiskToPluginInMemFileSystem := HandleOnLoadPluginFromDiskToPluginInMemFileSystem;
  {$ENDIF}

  frClickerActionsArrExperiment1.OnRenderBmpExternally := HandleOnRenderBmpExternally;
  frClickerActionsArrExperiment2.OnRenderBmpExternally := HandleOnRenderBmpExternally;
  frClickerActionsArrExperiment1.OnLoadPrimitivesFile := HandleOnLoadPrimitivesFile;
  frClickerActionsArrExperiment2.OnLoadPrimitivesFile := HandleOnLoadPrimitivesFile;
  frClickerActionsArrExperiment1.OnSavePrimitivesFile := HandleOnSavePrimitivesFile;
  frClickerActionsArrExperiment2.OnSavePrimitivesFile := HandleOnSavePrimitivesFile;
  frClickerActionsArrExperiment1.OnFileExists := HandleOnFileExists;
  frClickerActionsArrExperiment2.OnFileExists := HandleOnFileExists;
  frClickerActionsArrExperiment1.ActionExecution.OnGetSelfHandles := HandleOnGetSelfHandles;
  frClickerActionsArrExperiment2.ActionExecution.OnGetSelfHandles := HandleOnGetSelfHandles;
  frClickerActionsArrExperiment1.OnTClkIniReadonlyFileCreate := HandleOnTClkIniReadonlyFileCreate;
  frClickerActionsArrExperiment2.OnTClkIniReadonlyFileCreate := HandleOnTClkIniReadonlyFileCreate;
  frClickerActionsArrExperiment1.OnTClkIniFileCreate := HandleOnTClkIniFileCreate;
  frClickerActionsArrExperiment2.OnTClkIniFileCreate := HandleOnTClkIniFileCreate;
  frClickerActionsArrExperiment1.OnSaveTemplateToFile := HandleOnSaveTemplateToFile;
  frClickerActionsArrExperiment2.OnSaveTemplateToFile := HandleOnSaveTemplateToFile;

  frClickerActionsArrExperiment1.OnSetOpenDialogMultiSelect := HandleOnSetOpenDialogMultiSelect;
  frClickerActionsArrExperiment2.OnSetOpenDialogMultiSelect := HandleOnSetOpenDialogMultiSelect;
  frClickerActionsArrExperiment1.OnSetOpenDialogInitialDir := HandleOnSetOpenDialogInitialDir;
  frClickerActionsArrExperiment2.OnSetOpenDialogInitialDir := HandleOnSetOpenDialogInitialDir;
  frClickerActionsArrExperiment1.OnOpenDialogExecute := HandleOnOpenDialogExecute;
  frClickerActionsArrExperiment2.OnOpenDialogExecute := HandleOnOpenDialogExecute;
  frClickerActionsArrExperiment1.OnGetOpenDialogFileName := HandleOnGetOpenDialogFileName;
  frClickerActionsArrExperiment2.OnGetOpenDialogFileName := HandleOnGetOpenDialogFileName;
  frClickerActionsArrExperiment1.OnSetSaveDialogInitialDir := HandleOnSetSaveDialogInitialDir;
  frClickerActionsArrExperiment2.OnSetSaveDialogInitialDir := HandleOnSetSaveDialogInitialDir;
  frClickerActionsArrExperiment1.OnSaveDialogExecute := HandleOnSaveDialogExecute;
  frClickerActionsArrExperiment2.OnSaveDialogExecute := HandleOnSaveDialogExecute;
  frClickerActionsArrExperiment1.OnGetSaveDialogFileName := HandleOnGetSaveDialogFileName;
  frClickerActionsArrExperiment2.OnGetSaveDialogFileName := HandleOnGetSaveDialogFileName;
  frClickerActionsArrExperiment1.OnSetSaveDialogFileName := HandleOnSetSaveDialogFileName;
  frClickerActionsArrExperiment2.OnSetSaveDialogFileName := HandleOnSetSaveDialogFileName;

  frClickerActionsArrExperiment1.OnSetPictureOpenDialogInitialDir := HandleOnSetPictureOpenDialogInitialDir;
  frClickerActionsArrExperiment2.OnSetPictureOpenDialogInitialDir := HandleOnSetPictureOpenDialogInitialDir;
  frClickerActionsArrExperiment1.OnSetPictureSetOpenDialogMultiSelect := HandleOnSetPictureSetOpenDialogMultiSelect;
  frClickerActionsArrExperiment2.OnSetPictureSetOpenDialogMultiSelect := HandleOnSetPictureSetOpenDialogMultiSelect;
  frClickerActionsArrExperiment1.OnPictureOpenDialogExecute := HandleOnPictureOpenDialogExecute;
  frClickerActionsArrExperiment2.OnPictureOpenDialogExecute := HandleOnPictureOpenDialogExecute;
  frClickerActionsArrExperiment1.OnGetPictureOpenDialogFileName := HandleOnGetPictureOpenDialogFileName;
  frClickerActionsArrExperiment2.OnGetPictureOpenDialogFileName := HandleOnGetPictureOpenDialogFileName;

  frClickerActionsArrExperiment1.OnGetGridDrawingOption := HandleOnGetGridDrawingOption;
  frClickerActionsArrExperiment2.OnGetGridDrawingOption := HandleOnGetGridDrawingOption;
  frClickerActionsArrExperiment1.OnGetFontFinderSettings := HandleOnGetFontFinderSettings;
  frClickerActionsArrExperiment2.OnGetFontFinderSettings := HandleOnGetFontFinderSettings;
  frClickerActionsArrExperiment1.OnSetFontFinderSettings := HandleOnSetFontFinderSettings;
  frClickerActionsArrExperiment2.OnSetFontFinderSettings := HandleOnSetFontFinderSettings;

  frClickerActionsArrExperiment1.OnRetrieveRenderedBmpFromServer := HandleOnRetrieveRenderedBmpFromServer;
  frClickerActionsArrExperiment2.OnRetrieveRenderedBmpFromServer := HandleOnRetrieveRenderedBmpFromServer;
  frClickerActionsArrExperiment1.OnOpenCalledTemplateInExperimentTab := HandleOnOpenCalledTemplateInExperimentTab;
  frClickerActionsArrExperiment2.OnOpenCalledTemplateInExperimentTab := HandleOnOpenCalledTemplateInExperimentTab;

  frClickerActionsArrExperiment1.OnSaveFileToExtRenderingInMemFS := HandleOnSaveFileToExtRenderingInMemFS;
  frClickerActionsArrExperiment2.OnSaveFileToExtRenderingInMemFS := HandleOnSaveFileToExtRenderingInMemFS;

  frClickerActionsArrExperiment1.OnAddFileNameToRecent := HandleOnAddFileNameToRecent;
  frClickerActionsArrExperiment2.OnAddFileNameToRecent := HandleOnAddFileNameToRecent;
  frClickerActionsArrExperiment1.OnGetListOfRecentFiles := HandleOnGetListOfRecentFiles;
  frClickerActionsArrExperiment2.OnGetListOfRecentFiles := HandleOnGetListOfRecentFiles;
  frClickerActionsArrExperiment1.OnGenerateAndSaveTreeWithWinInterp := HandleOnGenerateAndSaveTreeWithWinInterp;
  frClickerActionsArrExperiment2.OnGenerateAndSaveTreeWithWinInterp := HandleOnGenerateAndSaveTreeWithWinInterp;
  frClickerActionsArrExperiment1.OnSetWinInterpOption := HandleOnSetWinInterpOption;
  frClickerActionsArrExperiment2.OnSetWinInterpOption := HandleOnSetWinInterpOption;
  frClickerActionsArrExperiment1.OnGetListeningPort := HandleOnGetListeningPort;
  frClickerActionsArrExperiment2.OnGetListeningPort := HandleOnGetListeningPort;

  AddToLog('ProcessID: ' + IntToStr(GetProcessID));

  tmrStartup.Enabled := True;
end;


procedure TfrmClickerActions.FormDestroy(Sender: TObject);
var
  tk: QWord;
begin
  GeneralClosingApp := True;  //prevent waiting for response loops to keep going
  FreeAndNil(FRecentTemplates);

  try
    if IdHTTPServer1.Active then
    begin
      IdHTTPServer1.Active := False;

      tk := GetTickCount64;
      repeat
        Application.ProcessMessages;
        Sleep(1);
      until (GetTickCount64 - tk > 2000) or not IdHTTPServer1.Active;
    end;
  except
  end;

  try
    if FPollForMissingServerFiles <> nil then
    begin
      FPollForMissingServerFiles.Terminate;
      tk := GetTickCount64;
      repeat
        if FPollForMissingServerFiles.Done then    //client mode
          Break;

        Application.ProcessMessages;
        Sleep(1);
      until GetTickCount64 - tk > 2000;

      FPollForMissingServerFiles.Free;
    end;
  except
  end;

  try   //wait 300ms more, for any other loop that might be using this flag
    tk := GetTickCount64;
    repeat
      if FPollForMissingServerFiles <> nil then
        if FPollForMissingServerFiles.Done then    //client mode
          Break;

      Application.ProcessMessages;
      Sleep(1);
    until GetTickCount64 - tk > 300;
  except
  end;

  FreeAndNil(FFileAvailabilityFIFO); //destroy the FIFO before the in-mem filesystem
  FreeAndNil(FInMemFileSystem);
  FreeAndNil(FRenderedInMemFileSystem);
  FreeAndNil(FPluginsInMemFileSystem);

  DoneCriticalSection(FDecDecHashArrCritSec);
  SetLength(FDecDecHashPluginInMemFSArr, 0);

  try
    frClickerActionsArrMain.frClickerActions.frClickerConditionEditor.ClearActionConditionPreview;
    frClickerActionsArrExperiment1.frClickerActions.frClickerConditionEditor.ClearActionConditionPreview;
    frClickerActionsArrExperiment2.frClickerActions.frClickerConditionEditor.ClearActionConditionPreview;
  finally
    FreeAndNil(frClickerActionsArrMain);
  end;
end;


procedure TfrmClickerActions.FormShow(Sender: TObject);
begin
  tmrUpdateColors.Enabled := True;

  if not FFirstDisplaying then
  begin
    FFirstDisplaying := True;
    tmrDelayedShow.Enabled := True;
  end;
end;


procedure TfrmClickerActions.DoOnReLoadSettings;
begin
  if not Assigned(FOnReLoadSettings) then
    Exit; //do not rais exceptions, because other tools, based on this window might not be updated yet
  FOnReLoadSettings();
end;


procedure TfrmClickerActions.DoOnRecordComponent(ACompHandle: THandle; ATreeContentStream: TMemoryStream);
begin
  if Assigned(FOnRecordComponent) then
    FOnRecordComponent(ACompHandle, ATreeContentStream)
  else
    raise Exception.Create('OnRecordComponent not assigned.');
end;


procedure TfrmClickerActions.DoOnGetCurrentlyRecordedScreenShotImage(ABmp: TBitmap);
begin
  if Assigned(FOnGetCurrentlyRecordedScreenShotImage) then
    FOnGetCurrentlyRecordedScreenShotImage(ABmp)
  else
    raise Exception.Create('OnGetCurrentlyRecordedScreenShotImage not assigned.');
end;


function TfrmClickerActions.DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if Assigned(FOnLoadBitmap) then
    Result := FOnLoadBitmap(ABitmap, AFileName)
  else
    Result := False;
end;


procedure TfrmClickerActions.DoOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
begin
  if not Assigned(FOnLoadPrimitivesFile) then
    raise Exception.Create('OnLoadPrimitivesFile not assigned.')
  else
    FOnLoadPrimitivesFile(AFileName, APrimitives, AOrders, ASettings);
end;


procedure TfrmClickerActions.DoOnSavePrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
begin
  if not Assigned(FOnSavePrimitivesFile) then
    raise Exception.Create('OnSavePrimitivesFile not assigned.')
  else
    FOnSavePrimitivesFile(AFileName, APrimitives, AOrders, ASettings);
end;


function TfrmClickerActions.DoOnFileExists(const AFileName: string): Boolean;
begin
  if not Assigned(FOnFileExists) then
    raise Exception.Create('OnFileExists is not assigned.')
  else
    Result := FOnFileExists(AFileName);
end;


procedure TfrmClickerActions.DoOnGetSelfHandles(AListOfSelfHandles: TStringList);
begin
  if not Assigned(FOnGetSelfHandles) then
    raise Exception.Create('OnGetSelfHandles is not assigned.')
  else
    FOnGetSelfHandles(AListOfSelfHandles);
end;


function TfrmClickerActions.DoOnTClkIniReadonlyFileCreate(AFileName: string): TClkIniReadonlyFile;
begin
  if not Assigned(FOnTClkIniReadonlyFileCreate) then
    raise Exception.Create('OnTClkIniReadonlyFileCreate is not assigned.')
  else
    Result := FOnTClkIniReadonlyFileCreate(AFileName);
end;


function TfrmClickerActions.DoOnTClkIniFileCreate(AFileName: string): TClkIniFile;
begin
  if not Assigned(FOnTClkIniFileCreate) then
    raise Exception.Create('OnTClkIniFileCreate is not assigned.')
  else
    Result := FOnTClkIniFileCreate(AFileName);
end;


procedure TfrmClickerActions.DoOnSaveTemplateToFile(AStringList: TStringList; const AFileName: string);
begin
  if not Assigned(FOnSaveTemplateToFile) then
    raise Exception.Create('OnSaveTemplateToFile is not assigned.')
  else
    FOnSaveTemplateToFile(AStringList, AFileName);
end;


procedure TfrmClickerActions.DoOnSetOpenDialogMultiSelect;
begin
  if not Assigned(FOnSetOpenDialogMultiSelect) then
    raise Exception.Create('OnSetOpenDialogMultiSelect is not assigned.')
  else
    FOnSetOpenDialogMultiSelect;
end;


procedure TfrmClickerActions.DoOnSetOpenDialogInitialDir(AInitialDir: string);
begin
  if not Assigned(FOnSetOpenDialogInitialDir) then
    raise Exception.Create('OnSetOpenDialogInitialDir is not assigned.')
  else
    FOnSetOpenDialogInitialDir(AInitialDir);
end;


function TfrmClickerActions.DoOnOpenDialogExecute(AFilter: string): Boolean;
begin
  if not Assigned(FOnOpenDialogExecute) then
    raise Exception.Create('OnOpenDialogExecute is not assigned.')
  else
    Result := FOnOpenDialogExecute(AFilter);
end;


function TfrmClickerActions.DoOnGetOpenDialogFileName: string;
begin
  if not Assigned(FOnGetOpenDialogFileName) then
    raise Exception.Create('OnGetOpenDialogFileName is not assigned.')
  else
    Result := FOnGetOpenDialogFileName;
end;


procedure TfrmClickerActions.DoOnSetSaveDialogInitialDir(AInitialDir: string);
begin
  if not Assigned(FOnSetSaveDialogInitialDir) then
    raise Exception.Create('OnSetSaveDialogInitialDir is not assigned.')
  else
    FOnSetSaveDialogInitialDir(AInitialDir);
end;


function TfrmClickerActions.DoOnSaveDialogExecute(AFilter: string): Boolean;
begin
  if not Assigned(FOnSaveDialogExecute) then
    raise Exception.Create('OnSaveDialogExecute is not assigned.')
  else
    Result := FOnSaveDialogExecute(AFilter);
end;


function TfrmClickerActions.DoOnGetSaveDialogFileName: string;
begin
  if not Assigned(FOnGetSaveDialogFileName) then
    raise Exception.Create('OnGetSaveDialogFileName is not assigned.')
  else
    Result := FOnGetSaveDialogFileName;
end;


procedure TfrmClickerActions.DoOnSetSaveDialogFileName(AFileName: string);
begin
  if not Assigned(FOnSetSaveDialogFileName) then
    raise Exception.Create('OnSetSaveDialogFileName is not assigned.')
  else
    FOnSetSaveDialogFileName(AFileName);
end;


procedure TfrmClickerActions.DoOnSetPictureSetOpenDialogMultiSelect;
begin
  if not Assigned(FOnSetPictureSetOpenDialogMultiSelect) then
    raise Exception.Create('OnSetPictureSetOpenDialogMultiSelect not assigned.')
  else
    FOnSetPictureSetOpenDialogMultiSelect;
end;


procedure TfrmClickerActions.DoOnSetPictureOpenDialogInitialDir(AInitialDir: string);
begin
  if not Assigned(FOnSetPictureOpenDialogInitialDir) then
    raise Exception.Create('OnSetPictureOpenDialogInitialDir not assigned.')
  else
    FOnSetPictureOpenDialogInitialDir(AInitialDir);
end;


function TfrmClickerActions.DoOnPictureOpenDialogExecute: Boolean;
begin
  if not Assigned(FOnPictureOpenDialogExecute) then
    raise Exception.Create('OnPictureOpenDialogExecute not assigned.')
  else
    Result := FOnPictureOpenDialogExecute;
end;


function TfrmClickerActions.DoOnGetPictureOpenDialogFileName: string;
begin
  if not Assigned(FOnGetPictureOpenDialogFileName) then
    raise Exception.Create('OnGetPictureOpenDialogFileName not assigned.')
  else
    Result := FOnGetPictureOpenDialogFileName;
end;


function TfrmClickerActions.DoOnGenerateAndSaveTreeWithWinInterp(AHandle: THandle; ATreeFileName: string; AStep: Integer; AUseMouseSwipe: Boolean): Boolean;
begin
  if not Assigned(FOnGenerateAndSaveTreeWithWinInterp) then
    raise Exception.Create('OnGenerateAndSaveTreeWithWinInterp not assigned.')
  else
    Result := FOnGenerateAndSaveTreeWithWinInterp(AHandle, ATreeFileName, AStep, AUseMouseSwipe);
end;


function TfrmClickerActions.DoOnSetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue: string): Boolean;
begin
  if not Assigned(FOnSetWinInterpOption) then
    raise Exception.Create('OnSetWinInterpOption not assigned.')
  else
    Result := FOnSetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue);
end;


function TfrmClickerActions.GetClickerActionsArrFrameByStackLevel(AStackLevel: Integer): TfrClickerActionsArr;
var
  i: Integer;
  ATab: TTabSheet;
  AScr: TScrollBox;
begin
  Result := nil;

  if AStackLevel = 0 then
  begin
    Result := frClickerActionsArrMain; //from scrboxMain
    Exit;
  end;

  ATab := PageControlPlayer.Pages[AStackLevel];

  AScr := nil;
  for i := 0 to ATab.ComponentCount - 1 do
    if ATab.Components[i] is TScrollBox then
    begin
      AScr := ATab.Components[i] as TScrollBox;
      Result := TfrClickerActionsArr(AScr.Tag);
      Break;
    end;
end;


procedure TfrmClickerActions.MenuItemOpenTemplateAsExp1Click(Sender: TObject);
var
  TemplatePath: string;
  TempStream: TMemoryStream;
  CurrentFrame: TfrClickerActionsArr;
begin
  if frClickerActionsArrMain.FileName = '' then
  begin
    MessageBox(Handle, 'No template is loaded.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  if frClickerActionsArrExperiment1.FileName <> '' then
    if MessageBox(Handle, 'There is a template already loaded. Continue?', PChar(Caption), MB_ICONQUESTION + MB_YESNO) = IDNO then
      Exit;

  TemplatePath := StringReplace(lbePathToTemplates.Text, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]) + '\';

  if PageControlPlayer.ActivePageIndex = 0 then
  begin
    if ExtractFileName(frClickerActionsArrMain.FileName) = frClickerActionsArrMain.FileName then
      frClickerActionsArrExperiment1.LoadTemplate(TemplatePath + frClickerActionsArrMain.FileName)
    else
      frClickerActionsArrExperiment1.LoadTemplate(frClickerActionsArrMain.FileName);
  end
  else
  begin
    CurrentFrame := GetClickerActionsArrFrameByStackLevel(PageControlPlayer.ActivePageIndex);
    if CurrentFrame <> nil then
      frClickerActionsArrExperiment1.LoadTemplate(CurrentFrame.FileName);
  end;

  PageControlMain.ActivePageIndex := 2;

  TempStream := TMemoryStream.Create;
  try
    frClickerActionsArrMain.frClickerActions.ClkVariables.SaveToStream(TempStream);
    TempStream.Position := 0;
    frClickerActionsArrExperiment1.frClickerActions.ClkVariables.LoadFromStream(TempStream);
  finally
    TempStream.Free;
  end;
end;


procedure TfrmClickerActions.MenuItemOpenTemplateAsExp2Click(Sender: TObject);
var
  TemplatePath: string;
  TempStream: TMemoryStream;
  CurrentFrame: TfrClickerActionsArr;
begin
  if frClickerActionsArrMain.FileName = '' then
  begin
    MessageBox(Handle, 'No template is loaded.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  if frClickerActionsArrExperiment2.FileName <> '' then
    if MessageBox(Handle, 'There is a template already loaded. Continue?', PChar(Caption), MB_ICONQUESTION + MB_YESNO) = IDNO then
      Exit;

  TemplatePath := StringReplace(lbePathToTemplates.Text, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]) + '\';

  if PageControlPlayer.ActivePageIndex = 0 then
    begin
    if ExtractFileName(frClickerActionsArrMain.FileName) = frClickerActionsArrMain.FileName then
      frClickerActionsArrExperiment2.LoadTemplate(TemplatePath + frClickerActionsArrMain.FileName)
    else
      frClickerActionsArrExperiment2.LoadTemplate(frClickerActionsArrMain.FileName);
  end
  else
  begin
    CurrentFrame := GetClickerActionsArrFrameByStackLevel(PageControlPlayer.ActivePageIndex);
    if CurrentFrame <> nil then
      frClickerActionsArrExperiment2.LoadTemplate(CurrentFrame.FileName);
  end;

  PageControlMain.ActivePageIndex := 3;

  TempStream := TMemoryStream.Create;
  try
    frClickerActionsArrMain.frClickerActions.ClkVariables.SaveToStream(TempStream);
    TempStream.Position := 0;
    frClickerActionsArrExperiment2.frClickerActions.ClkVariables.LoadFromStream(TempStream);
  finally
    TempStream.Free;
  end;
end;


procedure TfrmClickerActions.HandleNewFrameRefreshButton(Sender: TObject);
var
  NewFrame: TfrClickerActionsArr;
begin
  NewFrame := TfrClickerActionsArr((Sender as TButton).Tag);
  NewFrame.Repaint;
end;


function TfrmClickerActions.frClickerActionsArrOnCallTemplate(Sender: TObject; AFileNameToCall: string; ListOfVariables: TStrings; DebugBitmap: TBitmap; DebugGridImage: TImage; IsDebugging, AShouldStopAtBreakPoint: Boolean; AStackLevel: Integer; AExecutesRemotely: Boolean): Boolean;
var
  NewTabSheet: TTabSheet;
  NewFrame: TfrClickerActionsArr;
  ScrBox: TScrollBox;
  ABtn: TButton;
  FileLoc: TFileLocation;
begin
  NewTabSheet := TTabSheet.Create(PageControlPlayer);
  try
    NewTabSheet.Caption := ExtractFileName(AFileNameToCall);
    NewTabSheet.PageControl := PageControlPlayer;
    NewTabSheet.ImageIndex := 0;
    NewTabSheet.Width := TabSheetExecMainPlayer.Width;
    NewTabSheet.Height := TabSheetExecMainPlayer.Height;

    ABtn := TButton.Create(NewTabSheet);
    ABtn.Parent := NewTabSheet;
    ABtn.Left := 80;
    ABtn.Top := 80;
    ABtn.Width := 50;
    ABtn.Height := 60;
    ABtn.Caption := 'Refresh';
    ABtn.OnClick := HandleNewFrameRefreshButton;
    ABtn.Show;  //  this causes the pagecontrol to switch tabs

    if not FAutoSwitchToExecutingTab and not (FAutoEnableSwitchingTabsOnDebugging and IsDebugging) then
      PageControlPlayer.ActivePageIndex := PageControlPlayer.PageCount - 2; //switch back to current tab, to prevent wasting time on repainting (useful of VMs without GPU acceleration)

    ScrBox := TScrollBox.Create(NewTabSheet {Self}); //using NewTabSheet, to allow finding the scrollbox
    try
      ScrBox.Parent := NewTabSheet;
      ScrBox.Left := 0;
      ScrBox.Top := 0;
      ScrBox.Width := NewTabSheet.Width;
      ScrBox.Height := NewTabSheet.Height;

      ScrBox.HorzScrollBar.Smooth := True;
      ScrBox.HorzScrollBar.Tracking := True;
      ScrBox.VertScrollBar.Smooth := True;
      ScrBox.VertScrollBar.Tracking := True;

      ScrBox.Anchors := [akLeft, akTop, akRight, akBottom];

      NewFrame := TfrClickerActionsArr.Create(nil);
      try
        ScrBox.Tag := PtrInt(NewFrame);
        NewFrame.Parent := ScrBox;
        NewFrame.Left := 0;
        NewFrame.Top := 0;
        NewFrame.Width := frClickerActionsArrMain.Width;
        NewFrame.Height := frClickerActionsArrMain.Height;

        ABtn.Tag := PtrInt(NewFrame);

        NewFrame.ShouldStopAtBreakPoint := AShouldStopAtBreakPoint;
        NewFrame.StackLevel := AStackLevel + 1;
        NewFrame.ExecutesRemotely := AExecutesRemotely; //a client executes remotely
        NewFrame.ExecutingActionFromRemote := frClickerActionsArrMain.ExecutingActionFromRemote; //should be true in server mode
        NewFrame.UseLocalDebugger := frClickerActionsArrMain.UseLocalDebugger;
        NewFrame.FileLocationOfDepsIsMem := frClickerActionsArrMain.FileLocationOfDepsIsMem;
        NewFrame.FullTemplatesDir := FFullTemplatesDir;
        NewFrame.AllowedFileDirsForServer := memAllowedFileDirsForServer.Lines.Text;
        NewFrame.AllowedFileExtensionsForServer := memAllowedFileExtensionsForServer.Lines.Text;
        NewFrame.RemoteAddress := frClickerActionsArrMain.RemoteAddress;
        NewFrame.InMemFS := FInMemFileSystem;
        NewFrame.ExtRenderingInMemFS := FRenderedInMemFileSystem;

        {$IFDEF MemPlugins}
          NewFrame.MemPluginsInMemFS := FPluginsInMemFileSystem;
        {$ENDIF}

        //do not set FRenderedInMemFileSystem here
        NewFrame.GridDrawingOption := TDisplayGridLineOption(cmbImgPreviewGridType.ItemIndex);

        try
          NewFrame.PreviewSelectionColors := FPreviewSelectionColors;
        except
        end;

        NewFrame.OnCallTemplate := frClickerActionsArrOnCallTemplate;
        NewFrame.OnCopyControlTextAndClassFromMainWindow := HandleOnCopyControlTextAndClassFromMainWindow;
        NewFrame.OnGetExtraSearchAreaDebuggingImageWithStackLevel := HandleOnGetExtraSearchAreaDebuggingImageWithStackLevel;

        NewFrame.OnWaitForFileAvailability := HandleOnWaitForFileAvailability;
        NewFrame.OnWaitForMultipleFilesAvailability := HandleOnWaitForMultipleFilesAvailability;
        NewFrame.OnWaitForBitmapsAvailability := HandleOnWaitForBitmapsAvailability;
        NewFrame.OnTerminateWaitForMultipleFilesAvailability := HandleOnTerminateWaitForMultipleFilesAvailability;

        NewFrame.OnLoadBitmap := HandleOnLoadBitmap;
        NewFrame.OnLoadRenderedBitmap := HandleOnLoadRenderedBitmap;
        NewFrame.OnLoadPluginFromInMemFS := HandleOnLoadPluginFromInMemFS;
        NewFrame.OnGetListOfExternallyRenderedImages := HandleOnGetListOfExternallyRenderedImages;

        {$IFDEF MemPlugins}
          NewFrame.OnGetListOfInMemPlugins := HandleOnGetListOfInMemPlugins;
          NewFrame.OnLoadPluginFromDiskToPluginInMemFileSystem := HandleOnLoadPluginFromDiskToPluginInMemFileSystem;
        {$ENDIF}

        NewFrame.OnRenderBmpExternally := HandleOnRenderBmpExternally;
        NewFrame.OnLoadPrimitivesFile := HandleOnLoadPrimitivesFile;
        NewFrame.OnSavePrimitivesFile := HandleOnSavePrimitivesFile;
        NewFrame.OnFileExists := HandleOnFileExists;
        NewFrame.ActionExecution.OnGetSelfHandles := HandleOnGetSelfHandles;

        NewFrame.OnTClkIniReadonlyFileCreate := HandleOnTClkIniReadonlyFileCreate;
        NewFrame.OnTClkIniFileCreate := HandleOnTClkIniFileCreate;
        NewFrame.OnSaveTemplateToFile := HandleOnSaveTemplateToFile;

        NewFrame.OnSetOpenDialogMultiSelect := HandleOnSetOpenDialogMultiSelect;
        NewFrame.OnSetOpenDialogInitialDir := HandleOnSetOpenDialogInitialDir;
        NewFrame.OnOpenDialogExecute := HandleOnOpenDialogExecute;
        NewFrame.OnGetOpenDialogFileName := HandleOnGetOpenDialogFileName;
        NewFrame.OnSetSaveDialogInitialDir := HandleOnSetSaveDialogInitialDir;
        NewFrame.OnSaveDialogExecute := HandleOnSaveDialogExecute;
        NewFrame.OnGetSaveDialogFileName := HandleOnGetSaveDialogFileName;
        NewFrame.OnSetSaveDialogFileName := HandleOnSetSaveDialogFileName;

        NewFrame.OnSetPictureOpenDialogInitialDir := HandleOnSetPictureOpenDialogInitialDir;
        NewFrame.OnSetPictureSetOpenDialogMultiSelect := HandleOnSetPictureSetOpenDialogMultiSelect;
        NewFrame.OnPictureOpenDialogExecute := HandleOnPictureOpenDialogExecute;
        NewFrame.OnGetPictureOpenDialogFileName := HandleOnGetPictureOpenDialogFileName;

        NewFrame.OnGetGridDrawingOption := HandleOnGetGridDrawingOption;
        NewFrame.OnGetFontFinderSettings := HandleOnGetFontFinderSettings;
        NewFrame.OnSetFontFinderSettings := HandleOnSetFontFinderSettings;

        NewFrame.OnRetrieveRenderedBmpFromServer := HandleOnRetrieveRenderedBmpFromServer;
        NewFrame.OnOpenCalledTemplateInExperimentTab := HandleOnOpenCalledTemplateInExperimentTab;
        NewFrame.OnSaveFileToExtRenderingInMemFS := HandleOnSaveFileToExtRenderingInMemFS;

        NewFrame.OnAddFileNameToRecent := HandleOnAddFileNameToRecent;
        NewFrame.OnGetListOfRecentFiles := HandleOnGetListOfRecentFiles;
        NewFrame.OnGenerateAndSaveTreeWithWinInterp := HandleOnGenerateAndSaveTreeWithWinInterp;
        NewFrame.OnSetWinInterpOption := HandleOnSetWinInterpOption;
        NewFrame.OnGetListeningPort := HandleOnGetListeningPort;

        if FAutoSwitchToExecutingTab or (FAutoEnableSwitchingTabsOnDebugging and IsDebugging) then
        begin
          PageControlPlayer.ActivePageIndex := PageControlPlayer.PageCount - 1;
          NewFrame.Show;
        end;

        FileLoc := CExpectedFileLocation[NewFrame.FileLocationOfDepsIsMem];

        if not NewFrame.ExecutingActionFromRemote then  //this is client or local mode
        begin
          if NewFrame.ExecutesRemotely then  //this is client mode
          begin
            frmClickerActions.AddToLog('[client] Detected client mode when calling template, level=' + IntToStr(AStackLevel));

            if ExtractFileName(AFileNameToCall) = AFileNameToCall then  //AFileNameToCall does not contain a path
              NewFrame.LoadTemplate(FFullTemplatesDir + '\' + AFileNameToCall{, FileLoc, FInMemFileSystem})
            else
              NewFrame.LoadTemplate(AFileNameToCall{, FileLoc, FInMemFileSystem});

            if GetServerFileExpectancy(NewFrame.RemoteAddress) = CREResp_FileExpectancy_ValueFromClient then
            begin
              frmClickerActions.AddToLog('[client] file expectancy is to send files when calling template, level=' + IntToStr(AStackLevel));

              //setting name is required, because NewFrame.LoadTemplate is not supposed to set the filename
              if ExtractFileName(AFileNameToCall) = AFileNameToCall then
                NewFrame.FileName := FFullTemplatesDir + '\' + AFileNameToCall
              else
                NewFrame.FileName := AFileNameToCall;

              NewFrame.FileName := StringReplace(NewFrame.FileName, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);

              frmClickerActions.AddToLog('[client] Sending template (" ' + NewFrame.FileName + ' ") and other missing files to server..');
              //frmClickerActions.AddToLog('[client response] ' + NewFrame.SetCurrentClientTemplateInServer(True)); //do not use SetCurrentClientTemplateInServer, because it sends files event if outside of permitted folders
              frmClickerActions.AddToLog('[client response] ' + SendLoadTemplateInExecListRequest(NewFrame.RemoteAddress, NewFrame.FileName, NewFrame.StackLevel)); //instead of SendMissingFilesToServer

              //frmClickerActions.AddToLog('[client] ' + NewFrame.SendMissingFilesToServer);
            end;
          end
          else
          begin  //local mode
            if ExtractFileName(AFileNameToCall) = AFileNameToCall then  //AFileNameToCall does not contain a path
            begin
              frmClickerActions.AddToLog('[local] Loading template: "' + FFullTemplatesDir + '\' + AFileNameToCall + '"  FileLoc = ' + CFileLocationStr[FileLoc] + '   [using default template dir]');
              NewFrame.LoadTemplate(FFullTemplatesDir + '\' + AFileNameToCall{, FileLoc, FInMemFileSystem});
            end
            else
            begin
              frmClickerActions.AddToLog('[local] Loading template: "' + AFileNameToCall + '"  FileLoc = ' + CFileLocationStr[FileLoc]);
              if not DoOnFileExists(AFileNameToCall) then
                AddToLog('Template (to be loaded) not found: ' + AFileNameToCall);

              NewFrame.LoadTemplate(AFileNameToCall{, FileLoc, FInMemFileSystem});
            end;
          end;
        end
        else
        begin   //server mode
          frmClickerActions.AddToLog('[server] Detected server mode when calling template, level=' + IntToStr(AStackLevel));

          //the current implementation does not allow using files from disk in server mode
          if (ExtractFileName(AFileNameToCall) = AFileNameToCall) and (FileLoc <> flMem) then  //AFileNameToCall does not contain a path
          begin
            frmClickerActions.AddToLog('[server] Loading template: "' + FFullTemplatesDir + '\' + AFileNameToCall + '"  FileLoc = ' + CFileLocationStr[FileLoc] + '   [using default template dir]');
            NewFrame.LoadTemplate(FFullTemplatesDir + '\' + AFileNameToCall, FileLoc, FInMemFileSystem);
          end
          else
          begin
            frmClickerActions.AddToLog('[server] Loading template: "' + AFileNameToCall + '"  FileLoc = ' + CFileLocationStr[FileLoc]);
            NewFrame.LoadTemplate(AFileNameToCall, FileLoc, FInMemFileSystem);
          end;

          if not DoOnFileExists(AFileNameToCall) then
            frmClickerActions.AddToLog('[server] Template file not found: "' + AFileNameToCall + '"');
        end;

        NewFrame.InitFrame; //after "LoadTemplate", before "FileName :="

        NewFrame.FileName := AFileNameToCall;   //set to AFileNameToCall, after calling InitFrame
        NewFrame.Modified := True; //trigger updating label
        NewFrame.Modified := False;
        NewFrame.StopAllActionsOnDemandFromParent := @FStopAllActionsOnDemand;

        NewFrame.spdbtnPlaySelectedAction.Enabled := False;
        NewFrame.spdbtnPlayAllActions.Enabled := False;
        NewFrame.spdbtnStopPlaying.Enabled := True;
        NewFrame.frClickerActions.PredefinedVarCount := memVariables.Lines.Count;

        if TfrClickerActionsArr(Sender).chkEnableDebuggerKeys.Checked then
          NewFrame.chkEnableDebuggerKeys.Checked := True;

        NewFrame.Repaint;
        try
          if FStopAllActionsOnDemand then
          begin
            Result := False;
            Exit;
          end;

          Result := NewFrame.PlayAllActions(AFileNameToCall, ListOfVariables, IsDebugging);
        finally
          DebugBitmap.Width := NewFrame.frClickerActions.imgDebugBmp.Picture.Bitmap.Width;
          DebugBitmap.Height := NewFrame.frClickerActions.imgDebugBmp.Picture.Bitmap.Height;

          //clear debug image first, to fix transparency
          DebugBitmap.Transparent := False;
          DebugBitmap.Canvas.Pen.Color := clWhite;
          DebugBitmap.Canvas.Brush.Color := clWhite;
          DebugBitmap.Canvas.Rectangle(0, 0, NewFrame.frClickerActions.imgDebugBmp.Width, NewFrame.frClickerActions.imgDebugBmp.Height);

          DebugBitmap.Assign(NewFrame.frClickerActions.imgDebugBmp.Picture.Bitmap); // DebugBitmap.Canvas.Draw(0, 0, NewFrame.frClickerActions.imgDebugBmp.Picture.Bitmap);    better load bmp, than drawing on it

          DebugGridImage.Left := NewFrame.frClickerActions.imgDebugGrid.Left;
          DebugGridImage.Top := NewFrame.frClickerActions.imgDebugGrid.Top;
          DebugGridImage.Width := NewFrame.frClickerActions.imgDebugGrid.Width;
          DebugGridImage.Height := NewFrame.frClickerActions.imgDebugGrid.Height;
          DebugGridImage.Picture.Bitmap.Width := DebugGridImage.Width;
          DebugGridImage.Picture.Bitmap.Height := DebugGridImage.Height;

          DebugGridImage.Canvas.Brush.Style := bsSolid;
          DebugGridImage.Canvas.Brush.Color := clWhite;
          DebugGridImage.Canvas.Pen.Color := clWhite;
          DebugGridImage.Canvas.Rectangle(0, 0, DebugGridImage.Width, DebugGridImage.Height);
          DebugGridImage.Canvas.Draw(0, 0, NewFrame.frClickerActions.imgDebugGrid.Picture.Bitmap);
          MakeImageContentTransparent(DebugGridImage);

          NewFrame.spdbtnPlaySelectedAction.Enabled := True;
          NewFrame.spdbtnPlayAllActions.Enabled := True;
          NewFrame.spdbtnStopPlaying.Enabled := False;
        end;

        NewFrame.frClickerActions.frClickerConditionEditor.ClearActionConditionPreview; //destroy string lists and set array length to 0
        NewFrame.Visible := False;  //It seems that there is a race condition, which causes the frame to stay visible and keep some dangling pointers. If set to hidden, before being destroyed, might fix the issue.
      finally                       //The bug is reproduced when pressing the Stop button and a CallTemplate or Plugin action is next to be executed.
        NewFrame.Free;
      end;
    finally
      ScrBox.Free;
    end;
  finally
    NewTabSheet.Free;
  end;

  PageControlPlayer.ActivePageIndex := PageControlPlayer.PageCount - 1;
  PageControlPlayer.Repaint;

  if PageControlPlayer.ActivePageIndex > -1 then   //it should always be > -1
    PageControlPlayer.Pages[PageControlPlayer.ActivePageIndex].Repaint;
end;


procedure TfrmClickerActions.frClickerActionsArrExperiment1PasteDebugValuesListFromMainExecutionList1Click(
  Sender: TObject);
begin
  frClickerActionsArrExperiment1.frClickerActions.SetDebugVariablesFromListOfStrings(frClickerActionsArrMain.frClickerActions.ClkVariables.Text);
end;


procedure TfrmClickerActions.frClickerActionsArrExperiment2PasteDebugValuesListFromMainExecutionList1Click(
  Sender: TObject);
begin
  frClickerActionsArrExperiment2.frClickerActions.SetDebugVariablesFromListOfStrings(frClickerActionsArrMain.frClickerActions.ClkVariables.Text);
end;


procedure TfrmClickerActions.HandleOnCopyControlTextAndClassFromMainWindow(ACompProvider: string; out AControlText, AControlClass: string);
begin
  if not Assigned(FOnCopyControlTextAndClassFromMainWindow) then
    raise Exception.Create('OnCopyControlTextAndClass not assigned for ' + Caption)
  else
    FOnCopyControlTextAndClassFromMainWindow(ACompProvider, AControlText, AControlClass);
end;


function TfrmClickerActions.HandleOnGetExtraSearchAreaDebuggingImageWithStackLevel(AExtraBitmap: TBitmap; AStackLevel: Integer): Boolean;
var
  Response: string;
  NewSize: TSize;
begin
  if frClickerActionsArrMain.ExecutesRemotely then
  begin
    Response := GetSearchAreaDebugImageFromServer(GetConfiguredRemoteAddress, AStackLevel, AExtraBitmap);

    if Response = '' then
    begin
      Result := True;
      frClickerActionsArrMain.AddToLog('Received SearchArea bitmap: ' + IntToStr(AExtraBitmap.Width) + ':' + IntToStr(AExtraBitmap.Height));
    end
    else
    begin
      AExtraBitmap.Canvas.Font.Color := clMaroon;
      AExtraBitmap.Canvas.Brush.Color := clWhite;

      NewSize := AExtraBitmap.Canvas.TextExtent(Response);
      AExtraBitmap.Width := NewSize.Width + 10;
      AExtraBitmap.Height := NewSize.Height;
      AExtraBitmap.Canvas.TextOut(5, 0, Response);
      Result := False;
    end;
  end
  else
    Result := False;
end;


procedure TfrmClickerActions.HandleOnWaitForFileAvailability(AFileName: string); //ClickerActionsArrFrame instances call this, to add a filename to FIFO
var
  tk: QWord;
begin
  FFileAvailabilityFIFO.Put(AFileName);

  FTerminateWaitForFileAvailability := False;
  tk := GetTickCount64;
  repeat
    if FInMemFileSystem.FileExistsInMem(AFileName) then
      Exit;

    Application.ProcessMessages;
    Sleep(5);

    if GeneralClosingApp then
      Exit;

    if FTerminateWaitForFileAvailability then
    begin
      FTerminateWaitForFileAvailability := False;
      AddToLog('No more waiting for file availability. Stopped on demand.');
      Exit;
    end;
  until GetTickCount64 - tk > CWaitForFileAvailabilityTimeout; //if file is not received from client in 5min, simply exit and let the action fail
end;


procedure TfrmClickerActions.HandleOnWaitForMultipleFilesAvailability(AListOfFiles: TStringList);
var
  tk: QWord;
  TempListOfFiles: TStringList;
  i: Integer;
  AllExist: Boolean;
begin
  FFileAvailabilityFIFO.PutMultiple(AListOfFiles);

  TempListOfFiles := TStringList.Create;
  try
    TempListOfFiles.AddStrings(AListOfFiles);

    FTerminateWaitForMultipleFilesAvailability := False;
    tk := GetTickCount64;
    repeat
      AllExist := True;
      for i := TempListOfFiles.Count - 1 downto 0 do
        if not FInMemFileSystem.FileExistsInMem(TempListOfFiles.Strings[i]) then
          AllExist := False
        else
          TempListOfFiles.Delete(i); //remove existent files from being verified again

      if AllExist then
        Exit;

      Application.ProcessMessages;
      Sleep(5);

      if GeneralClosingApp then
        Exit;

      if FTerminateWaitForMultipleFilesAvailability then
      begin
        FTerminateWaitForMultipleFilesAvailability := False;
        Exit;
      end;
    until GetTickCount64 - tk > CWaitForMultipleFilesAvailabilityTimeout * TempListOfFiles.Count; //if file is not received from client in 1min, simply exit and let the action fail
  finally
    TempListOfFiles.Free;
  end;
end;


procedure TfrmClickerActions.HandleOnWaitForBitmapsAvailability(AListOfBitmapFiles: TStringList);
var
  ListOfNonExistentBmps: TStringList;
begin
  ListOfNonExistentBmps := TStringList.Create;
  try
    ExtractNonExistentFiles(AListOfBitmapFiles, ListOfNonExistentBmps, flMem {flDiskThenMem}, FInMemFileSystem);
    if ListOfNonExistentBmps.Count > 0 then
    begin
      AddToLogFromThread('Waiting for the following bitmaps to exist: ' + #13#10 + AListOfBitmapFiles.Text);
      HandleOnWaitForMultipleFilesAvailability(ListOfNonExistentBmps);
    end;
  finally
    ListOfNonExistentBmps.Free;
  end;
end;


procedure TfrmClickerActions.HandleOnTerminateWaitForMultipleFilesAvailability;
begin
  FTerminateWaitForFileAvailability := True; //stop also this one
  FTerminateWaitForMultipleFilesAvailability := True;
end;


function TfrmClickerActions.HandleOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  Result := False;

  if CExpectedFileLocation[frClickerActionsArrMain.FileLocationOfDepsIsMem] = flMem then
  begin
    if not FInMemFileSystem.FileExistsInMem(AFileName) then
      Exit;
  end
  else
    if CExpectedFileLocation[frClickerActionsArrMain.FileLocationOfDepsIsMem] = flDisk then
    begin
      if not DoOnFileExists(AFileName) then
        Exit;
    end;

  //replaced FileExistsInDiskOrMemWithPriority with the above logic, because of disk handlers
  //if not FileExistsInDiskOrMemWithPriority(AFileName, FInMemFileSystem, CExpectedFileLocation[frClickerActionsArrMain.FileLocationOfDepsIsMem]) then
  //  Exit;

  if frClickerActionsArrMain.FileLocationOfDepsIsMem then
  begin
    LoadBmpFromInMemFileSystem(AFileName, ABitmap, FInMemFileSystem);
    Result := True;
  end
  else
    Result := DoOnLoadBitmap(ABitmap, AFileName);
end;


function TfrmClickerActions.HandleOnLoadRenderedBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if FRenderedInMemFileSystem.FileExistsInMem(AFileName) then
  begin
    LoadBmpFromInMemFileSystem(AFileName, ABitmap, FRenderedInMemFileSystem);
    Result := True;
  end
  else
    Result := False;
end;


function TfrmClickerActions.HandleOnLoadPluginFromInMemFS(APlugin: TMemoryStream; AFileName: string): Boolean;
var
  i: Integer;
begin
  Result := False;

  //if FPluginsInMemFileSystem.FileExistsInMem(AFileName) then  //FPluginsInMemFileSystem is no longer used to store plugins, however, it points to Mem:\
  //begin
  //  FPluginsInMemFileSystem.LoadFileFromMemToStream(AFileName, APlugin);
  //  Result := True;
  //end;

  EnterCriticalSection(FDecDecHashArrCritSec);
  try
    for i := 0 to Length(FDecDecHashPluginInMemFSArr) - 1 do
      if not FDecDecHashPluginInMemFSArr[i].IsDecDecHashPlugin then
        if FDecDecHashPluginInMemFSArr[i].InMemFS.FileExistsInMem(AFileName) then
        begin
          FDecDecHashPluginInMemFSArr[i].InMemFS.LoadFileFromMemToStream(AFileName, APlugin);
          Result := True;
          Exit;
        end;
  finally
    LeaveCriticalSection(FDecDecHashArrCritSec);
  end;
end;


procedure TfrmClickerActions.HandleOnGetListOfExternallyRenderedImages(AListOfExternallyRenderedImages: TStringList);
var
  Stream: TMemoryStream;
  Bmp: TBitmap;
  i: Integer;
  TempName: string;
begin
  FRenderedInMemFileSystem.ListMemFiles(AListOfExternallyRenderedImages);

  for i := 0 to AListOfExternallyRenderedImages.Count - 1 do
  begin
    Stream := TMemoryStream.Create;
    Bmp := TBitmap.Create;
    try
      TempName := AListOfExternallyRenderedImages.Strings[i];
      FRenderedInMemFileSystem.LoadFileFromMemToStream(TempName, Stream);

      Stream.Position := 0;
      try
        Bmp.LoadFromStream(Stream, Stream.Size);
      except
        on E: Exception do
          AddToLog('Error loading received bitmap: "' + E.Message + '"   of size: ' + IntToStr(Stream.Size) + 'B.');
      end;

      AListOfExternallyRenderedImages.Strings[i] := TempName + #8#7 + IntToStr(Bmp.Width) + ':' + IntToStr(Bmp.Height) + '  ' + IntToStr(Stream.Size) + 'B';
    finally
      Stream.Free;
      Bmp.Free;
    end;
  end;
end;


{$IFDEF MemPlugins}
  procedure TfrmClickerActions.HandleOnGetListOfInMemPlugins(AListOfInMemPlugins: TStringList);
  var
    i: Integer;
    TempListOfInMemPlugins: TStringList;
  begin
    //FPluginsInMemFileSystem.ListMemFiles(AListOfInMemPlugins);       //FPluginsInMemFileSystem is no longer used to store plugins, however, it points to Mem:\
    //
    EnterCriticalSection(FDecDecHashArrCritSec);
    try
      AddToLog('..... Quering for list of mem plugins..  ArrLen = ' + IntToStr(Length(FDecDecHashPluginInMemFSArr)));

      for i := 0 to Length(FDecDecHashPluginInMemFSArr) - 1 do
        if not FDecDecHashPluginInMemFSArr[i].IsDecDecHashPlugin then
        begin
          TempListOfInMemPlugins := TStringList.Create;
          try
            FDecDecHashPluginInMemFSArr[i].InMemFS.ListMemFiles(TempListOfInMemPlugins);
            AListOfInMemPlugins.AddStrings(TempListOfInMemPlugins);

            AddToLog('..... Found ' + IntToStr(TempListOfInMemPlugins.Count) + ' files(s) at index ' + IntToStr(i) + ':  ' + FastReplace_ReturnToCSV(TempListOfInMemPlugins.Text));
          finally
            TempListOfInMemPlugins.Free;
          end;
        end;

      TempListOfInMemPlugins := TStringList.Create;
      try
        FPluginsInMemFileSystem.ListMemFiles(TempListOfInMemPlugins);
        AddToLog('..... Found ' + IntToStr(TempListOfInMemPlugins.Count) + ' files(s) in plugins InMem FS:  ' + FastReplace_ReturnToCSV(TempListOfInMemPlugins.Text));
      finally
        TempListOfInMemPlugins.Free;
      end;
    finally
      LeaveCriticalSection(FDecDecHashArrCritSec);
    end;

    for i := 0 to AListOfInMemPlugins.Count - 1 do
      AListOfInMemPlugins.Strings[i] := AListOfInMemPlugins.Strings[i] + #8#7;
  end;


  procedure TfrmClickerActions.HandleOnLoadPluginFromDiskToPluginInMemFileSystem(APluginPath: string);
  var
    MemStream: TMemoryStream;
    Idx: Integer;
  begin
    if not FileExists(APluginPath) then
    begin
      AddToLog('File not found on disk: ' + APluginPath);
      Exit;
    end;

    MemStream := TMemoryStream.Create;
    try
      MemStream.LoadFromFile(APluginPath);

      //FPluginsInMemFileSystem is no longer used to store plugins, however, it points to Mem:\
      //FPluginsInMemFileSystem.SaveFileToMem(CMemPluginLocationPrefix + PathDelim + ExtractFileName(APluginPath), MemStream.Memory, MemStream.Size);

      EnterCriticalSection(FDecDecHashArrCritSec);
      try
        Idx := GetPluginInMemFSIndex(FDecDecHashPluginInMemFSArr, ExtractFileName(APluginPath));
        if Idx = -1 then
        begin
          SetLength(FDecDecHashPluginInMemFSArr, Length(FDecDecHashPluginInMemFSArr) + 1);
          Idx := Length(FDecDecHashPluginInMemFSArr) - 1;
        end;

        FDecDecHashPluginInMemFSArr[Idx].InMemFS := TInMemFileSystem.Create;
        FDecDecHashPluginInMemFSArr[Idx].Name := ExtractFileName(APluginPath);
        FDecDecHashPluginInMemFSArr[Idx].PreventReusingInMemFS := True;
        FDecDecHashPluginInMemFSArr[Idx].IsDecDecHashPlugin := False; //no DecDecHashPlugin from disk
        FDecDecHashPluginInMemFSArr[Idx].InMemFS.SaveFileToMem(CMemPluginLocationPrefix + PathDelim + ExtractFileName(APluginPath), MemStream.Memory, MemStream.Size);
      finally
        LeaveCriticalSection(FDecDecHashArrCritSec);
      end;
    finally
      MemStream.Free;
    end;
  end;
{$ENDIF}


function TfrmClickerActions.HandleOnRenderBmpExternally(ARequest: string): string;
var
  ListOfParams: TStringList;
  FullLink, Filename, SrvAddrPort, Cmd, Params: string;
  IncludeFilenameInRequest: Boolean;
  TempFileContent: TMemoryStream;
begin
  Result := 'not set';

  ListOfParams := TStringList.Create;
  try
    ListOfParams.Text := FastReplace_45ToReturn(ARequest);

    Filename := ListOfParams.Values[CExtBmp_Filename];
    if Filename = '' then
    begin
      Result := 'The "' + CExtBmp_Filename + '" parameter is empty or missing from parameter list.';
      Exit;
    end;

    if Length(Filename) > CExtBmp_FilenameMaxLen then
    begin
      Filename := Copy(Filename, 1, CExtBmp_FilenameMaxLen);
      AddToLog('Truncating rendered bitmap filename to "' + Filename + '", because its length is greater than ' + IntToStr(CExtBmp_FilenameMaxLen) + '.');
    end;

    SrvAddrPort := ListOfParams.Values[CExtBmp_SrvAddrPort];   //something like 'http://127.0.0.1:15444/'
    if SrvAddrPort = '' then
    begin
      Result := 'The "' + CExtBmp_SrvAddrPort + '" parameter is empty or missing from parameter list.';
      Exit;
    end;

    if SrvAddrPort[Length(SrvAddrPort)] <> '/' then
      SrvAddrPort := SrvAddrPort + '/';

    Cmd := ListOfParams.Values[CExtBmp_Cmd];  //something like 'RenderMyBmp'
    if Cmd = '' then
    begin
      Result := 'The "' + CExtBmp_Cmd + '" parameter is empty or missing from parameter list.';
      Exit;
    end;

    FullLink := SrvAddrPort + Cmd;
    Params := ListOfParams.Values[CExtBmp_Params];  //should not include '?'

    IncludeFilenameInRequest := ListOfParams.Values[CExtBmp_IncludeFilenameInRequest] = '1';
    if IncludeFilenameInRequest then
      Params := Params + '&' + CExtBmp_Filename + '=' + Filename;

    if Params > '' then
      FullLink := FullLink + '?' + Params;

    TempFileContent := TMemoryStream.Create;
    try
      Result := SendGetFileRequestToServer(FullLink, TempFileContent);

      if Result = '' then
        FRenderedInMemFileSystem.SaveFileToMem(Filename, TempFileContent.Memory, TempFileContent.Size);
    finally
      TempFileContent.Free;
    end;
  finally
    ListOfParams.Free;
  end;
end;


procedure TfrmClickerActions.HandleOnLoadPrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
begin
  if CExpectedFileLocation[frClickerActionsArrMain.FileLocationOfDepsIsMem] = flMem then
  begin
    if not FInMemFileSystem.FileExistsInMem(AFileName) then
      Exit;
  end
  else
    if CExpectedFileLocation[frClickerActionsArrMain.FileLocationOfDepsIsMem] = flDisk then
    begin
      if not DoOnFileExists(AFileName) then
        Exit;
    end;

  //replaced FileExistsInDiskOrMemWithPriority with the above logic, because of disk handlers
  //if not FileExistsInDiskOrMemWithPriority(AFileName, FInMemFileSystem, CExpectedFileLocation[frClickerActionsArrMain.FileLocationOfDepsIsMem]) then
  //  Exit;

  if frClickerActionsArrMain.FileLocationOfDepsIsMem then
    LoadPmtvFromInMemFileSystem(AFileName, APrimitives, AOrders, ASettings, FInMemFileSystem)
  else
    DoOnLoadPrimitivesFile(AFileName, APrimitives, AOrders, ASettings);
end;


procedure TfrmClickerActions.HandleOnSavePrimitivesFile(AFileName: string; var APrimitives: TPrimitiveRecArr; var AOrders: TCompositionOrderArr; var ASettings: TPrimitiveSettings);
begin
  if frClickerActionsArrMain.FileLocationOfDepsIsMem then
    SavePmtvToInMemFileSystem(AFileName, APrimitives, AOrders, ASettings, FInMemFileSystem)
  else
    DoOnSavePrimitivesFile(AFileName, APrimitives, AOrders, ASettings);
end;


function TfrmClickerActions.GetListOfWaitingFiles: string;
begin
  Result := FastReplace_ReturnTo87(FFileAvailabilityFIFO.PopAllAsString);
end;


function TfrmClickerActions.GetCompAtPoint(AParams: TStrings): string;
var
  tp: TPoint;
  Comp: TCompRec;
  ResultLst: TStringList;
begin
  tp.X := StrToIntDef(AParams.Values[CREParam_X], -1);
  tp.Y := StrToIntDef(AParams.Values[CREParam_Y], -1);

  if (tp.X = -1) or (tp.Y = -1) then
  begin
    Result := CREResp_ErrParam + '=X or Y are invalid';
    Exit;
  end;

  Comp := GetWindowClassRec(tp);

  ResultLst := TStringList.Create;
  try
    ResultLst.Add(CREResp_ErrParam + '=' + CREResp_ErrResponseOK);
    ResultLst.Add(CREResp_HandleParam + '=' + IntToStr(Comp.Handle));
    ResultLst.Add(CREResp_TextParam + '=' + Comp.Text);
    ResultLst.Add(CREResp_ClassParam + '=' + Comp.ClassName);
    ResultLst.Add(CREResp_ScreenWidth + '=' + IntToStr(Screen.Width));
    ResultLst.Add(CREResp_ScreenHeight + '=' + IntToStr(Screen.Height));
    ResultLst.Add(CREResp_CompLeft + '=' + IntToStr(Comp.ComponentRectangle.Left));
    ResultLst.Add(CREResp_CompTop + '=' + IntToStr(Comp.ComponentRectangle.Top));
    ResultLst.Add(CREResp_CompWidth + '=' + IntToStr(Comp.ComponentRectangle.Width));
    ResultLst.Add(CREResp_CompHeight + '=' + IntToStr(Comp.ComponentRectangle.Height));

    Result := FastReplace_ReturnTo87(ResultLst.Text);
  finally
    ResultLst.Free;
  end;
end;


procedure TfrmClickerActions.SetFullTemplatesDir(Value: string);
begin
  if FFullTemplatesDir <> Value then
  begin
    Value := StringReplace(Value, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);
    FFullTemplatesDir := Value;
    frClickerActionsArrMain.FullTemplatesDir := Value;
    frClickerActionsArrExperiment1.FullTemplatesDir := Value;
    frClickerActionsArrExperiment2.FullTemplatesDir := Value;
  end;
end;


function TfrmClickerActions.GetBMPsDir: string;
begin
  try
    Result := frClickerActionsArrMain.frClickerActions.BMPsDir;  //frClickerActionsArrMain may not be created when GetBMPsProjectsDir is called
  except
    Result := '';
  end;

  if Result = '' then
    Result := frClickerActionsArrExperiment1.frClickerActions.BMPsDir;

  if Result = '' then
    Result := frClickerActionsArrExperiment2.frClickerActions.BMPsDir;
end;


procedure TfrmClickerActions.SetBMPsDir(Value: string);
begin
  if Value <> FBMPsDir then
  begin
    FBMPsDir := Value;
    frClickerActionsArrExperiment1.frClickerActions.BMPsDir := FBMPsDir;
    frClickerActionsArrExperiment2.frClickerActions.BMPsDir := FBMPsDir;
    frClickerActionsArrMain.frClickerActions.BMPsDir := FBMPsDir;
  end;
end;


function TfrmClickerActions.GetConfiguredRemoteAddress: string;
begin
  Result := cmbClientModeServerAddress.Text;
end;


function TfrmClickerActions.GetActionExecution: TActionExecution;
begin
  Result := frClickerActionsArrMain.ActionExecution;
end;

//Remote execution stuff

//called in server mode
function TfrmClickerActions.ProcessServerCmd(ASyncObj: TSyncHTTPCmd): string;

  function GetClkVariables87Obj: string;  //using ASyncObj, to be able to call different experiments
  begin
    Result := FastReplace_ReturnTo87(ASyncObj.FFrame.frClickerActions.ClkVariables.Text);
  end;

  function GetClkVariables87: string;  //probably the same as above, although ASyncObj may point to a different frame
  begin
    Result := FastReplace_ReturnTo87(frClickerActionsArrMain.frClickerActions.ClkVariables.Text);
  end;

var
  TabIdx: Integer;
  {$IFDEF MemPlugins}
    {$IFDEF PluginTesting}
      i, FSIdx: Integer;
      ListOfFileNames: TStringList;
    {$ENDIF}
  {$ENDIF}
  IsDebuggingFromClient: Boolean;
  ErrMsg: string;
  Fnm: string;
  RemoteState: Boolean;
  TempStr: string;
begin
  Result := 'ok';  //default if not setting any result, as in CRECmd_ExecuteCommandAtIndex

  AddToLog('Request: ' + ASyncObj.FCmd + '  ' + FastReplace_ReturnToCSV(ASyncObj.FParams.Text));

  if ASyncObj.FFrame = nil then
  begin
    TabIdx := StrToInt64Def(ASyncObj.FParams.Values[CREParam_StackLevel], -1);

    if TabIdx = -1 then
    begin
      Result := '[Server error] Stack level not specified.';
      ASyncObj.FErrCode := 1;
      Exit;
    end;
                                           //make sure this executes only for requests which actually use TabIdx
    if (TabIdx < 0) or (TabIdx > PageControlPlayer.PageCount - 1) then    //accessing PageControlPlayer should be done only from here, the UI thread
    begin
      Result := '[Server error] Stack level out of bounds: ' + IntToStr(TabIdx) + '. This happens when there is no template loaded in a new tab (with the requested stack level), as a result of "call template" action. It is also possible that the template is loaded, then exited before being executed.';
      ASyncObj.FErrCode := 1;
      Exit;
    end;

    ASyncObj.FFrame := GetClickerActionsArrFrameByStackLevel(TabIdx);

    if ASyncObj.FFrame = nil then
    begin
      Result := '[Server error] Can''t get frame at index: ' + IntToStr(TabIdx);
      ASyncObj.FErrCode := 1;
      Exit;
    end;
  end;


  if ASyncObj.FCmd = '/' + CRECmd_ExecuteCommandAtIndex then
  begin
    IsDebuggingFromClient := ASyncObj.FParams.Values[CREParam_IsDebugging] = '1';

    ASyncObj.FFrame.RemoteExActionIndex := StrToInt64Def(ASyncObj.FParams.Values[CREParam_ActionIdx], -1);
    ASyncObj.FFrame.ExecuteActionFromClient(IsDebuggingFromClient);

    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_GetExecuteCommandAtIndexResult then  //Not always called by client. It is usually called as a local request from ProcessServerCommand.
  begin
    Result := CREResp_RemoteExecResponseVar + '=' + IntToStr(Ord(ASyncObj.FFrame.RemoteExCmdResult));
    Result := Result + #8#7 + GetClkVariables87Obj;
    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_StopTemplateExecution then
  begin
    ASyncObj.FFrame.StopAllActionsOnDemand := True;
    ASyncObj.FFrame.StopAllActionsOnDemandFromParent^ := True;
    Result := CREResp_Done;
    frClickerActionsArrMain.AddToLog('Stopping template at stack level ' + IntToStr(ASyncObj.FFrame.StackLevel));
    ASyncObj.FFrame.AddToLog('Stopping template at stack level ' + IntToStr(ASyncObj.FFrame.StackLevel));
    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_ExitTemplate then
  begin
    ASyncObj.FFrame.ExitTemplateFromRemote;
    frClickerActionsArrMain.AddToLog('Closing template at stack level ' + IntToStr(ASyncObj.FFrame.StackLevel) + '  Frame FileName: ' + ExtractFileName(ASyncObj.FFrame.FileName));
    ASyncObj.FFrame.AddToLog('Closing template at stack level ' + IntToStr(ASyncObj.FFrame.StackLevel));
    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_GetAllReplacementVars then
  begin   //similar to CRECmd_GetExecuteCommandAtIndexResult, but return the var list, without executing an action
    Result := GetClkVariables87Obj;
    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_GetResultedDebugImage then
  begin
    Result := '';
    if not Assigned(ASyncObj.FBmp) then
    begin
      Result := 'Error: Output bitmap not assigned in server handler.';
      Exit;
    end;

    ASyncObj.FBmp.PixelFormat := pf24bit;

    if ASyncObj.FErrCode <> 0 then
    begin
      ErrMsg := 'Err: ' + IntToStr(ASyncObj.FErrCode);
      ASyncObj.FBmp.Width := ASyncObj.FBmp.Canvas.TextWidth(ErrMsg) + 10;
      ASyncObj.FBmp.Height := 15;
      ASyncObj.FBmp.Canvas.Font.Color := clRed;
      ASyncObj.FBmp.Canvas.TextOut(5, 1, ErrMsg);
      ASyncObj.FFrame.AddToLog('Can''t send debug bmp, because the addressed frame is not set at level ' + ASyncObj.FParams.Values[CREParam_StackLevel]);
    end
    else
    begin
      ASyncObj.FBmp.Width := ASyncObj.FFrame.frClickerActions.imgDebugBmp.Width;
      ASyncObj.FBmp.Height := ASyncObj.FFrame.frClickerActions.imgDebugBmp.Height;
      ASyncObj.FBmp.Assign(ASyncObj.FFrame.frClickerActions.imgDebugBmp.Picture.Bitmap); //ASyncObj.FBmp.Canvas.Draw(0, 0, ASyncObj.FFrame.frClickerActions.imgDebugBmp.Picture.Bitmap); //using Canvas, requires the device context to be available (a.k.a. Canvas.Handle)

      if ASyncObj.FParams.Values[CREParam_Grid] = '1' then
      begin
        ASyncObj.FBmp.Canvas.Draw(ASyncObj.FFrame.frClickerActions.imgDebugGrid.Left,
                                  ASyncObj.FFrame.frClickerActions.imgDebugGrid.Top,
                                  ASyncObj.FFrame.frClickerActions.imgDebugGrid.Picture.Bitmap);
        ASyncObj.FFrame.AddToLog('Added grid to debug bmp..');
      end;

      if ASyncObj.FFrame <> frClickerActionsArrMain then
              ASyncObj.FFrame.AddToLog('Sending debug bmp of: ' + IntToStr(ASyncObj.FBmp.Width) + ' x ' + IntToStr(ASyncObj.FBmp.Height) + '  from Frame at level ' + IntToStr(ASyncObj.FFrame.StackLevel));
      frClickerActionsArrMain.AddToLog('Sending debug bmp of: ' + IntToStr(ASyncObj.FBmp.Width) + ' x ' + IntToStr(ASyncObj.FBmp.Height) + '  from Frame at level ' + IntToStr(ASyncObj.FFrame.StackLevel));
    end;

    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_GetSearchAreaDebugImage then
  begin
    Result := '';
    if not Assigned(ASyncObj.FBmp) then
    begin
      Result := 'Error: Output bitmap not assigned in server handler.';
      Exit;
    end;

    ASyncObj.FBmp.PixelFormat := pf24bit;

    if ASyncObj.FErrCode <> 0 then
    begin
      ErrMsg := 'Err: ' + IntToStr(ASyncObj.FErrCode);
      ASyncObj.FBmp.Width := ASyncObj.FBmp.Canvas.TextWidth(ErrMsg) + 10;
      ASyncObj.FBmp.Height := 15;
      ASyncObj.FBmp.Canvas.Font.Color := clRed;
      ASyncObj.FBmp.Canvas.TextOut(5, 1, ErrMsg);
      ASyncObj.FFrame.AddToLog('Can''t send search area debug bmp, because the addressed frame is not set at level ' + ASyncObj.FParams.Values[CREParam_StackLevel]);
    end
    else
    begin
      //if not Assigned(ASyncObj.FFrame.frClickerActions.frClickerFindControl.SearchAreaControlDbgImg) then  //take a screenshot anyway
        ASyncObj.FFrame.frClickerActions.frClickerFindControl.DisplayDebuggingImage;

      ASyncObj.FBmp.Width := ASyncObj.FFrame.frClickerActions.frClickerFindControl.SearchAreaControlDbgImg.Width;
      ASyncObj.FBmp.Height := ASyncObj.FFrame.frClickerActions.frClickerFindControl.SearchAreaControlDbgImg.Height;
      ASyncObj.FBmp.Assign(ASyncObj.FFrame.frClickerActions.frClickerFindControl.SearchAreaControlDbgImg.Picture.Bitmap); //ASyncObj.FBmp.Canvas.Draw(0, 0, ASyncObj.FFrame.frClickerActions.imgDebugBmp.Picture.Bitmap); //using Canvas, requires the device context to be available (a.k.a. Canvas.Handle)

      if ASyncObj.FFrame <> frClickerActionsArrMain then
              ASyncObj.FFrame.AddToLog('Sending debug bmp of: ' + IntToStr(ASyncObj.FBmp.Width) + ' x ' + IntToStr(ASyncObj.FBmp.Height) + '  from Frame at level ' + IntToStr(ASyncObj.FFrame.StackLevel));
      frClickerActionsArrMain.AddToLog('Sending search area debug bmp of: ' + IntToStr(ASyncObj.FBmp.Width) + ' x ' + IntToStr(ASyncObj.FBmp.Height) + '  from Frame at level ' + IntToStr(ASyncObj.FFrame.StackLevel));
    end;

    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_GetScreenShotImage then
  begin
    Result := '';
    if not Assigned(ASyncObj.FBmp) then
    begin
      Result := 'Error: Output bitmap not assigned in server handler.';
      Exit;
    end;

    ASyncObj.FBmp.PixelFormat := pf24bit;
    ASyncObj.FBmp.Width := Screen.Width;
    ASyncObj.FBmp.Height := Screen.Height;
    ScreenShot(0, ASyncObj.FBmp, 0, 0, ASyncObj.FBmp.Width, ASyncObj.FBmp.Height);
    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_GetCurrentlyRecordedScreenShotImage then
  begin
    Result := '';
    if not Assigned(ASyncObj.FBmp) then
    begin
      Result := 'Error: Output bitmap not assigned in server handler.';
      Exit;
    end;

    ASyncObj.FBmp.PixelFormat := pf24bit;
    DoOnGetCurrentlyRecordedScreenShotImage(ASyncObj.FBmp);
    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_LoadTemplateInExecList then
  begin
    Fnm := ASyncObj.FParams.Values[CREParam_FileName];
    RemoteState := ASyncObj.FFrame.ExecutingActionFromRemote;
    try
      ASyncObj.FFrame.ExecutingActionFromRemote := True;  //set to true, to prevent messageboxes
      ASyncObj.FFrame.LoadTemplate(Fnm, flMem, FInMemFileSystem);
    finally
      ASyncObj.FFrame.ExecutingActionFromRemote := RemoteState //restore, in case the server is running unattended
    end;

    frClickerActionsArrMain.AddToLog('Loading template in main list from mem.');
    Result := CREResp_TemplateLoaded;
    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_GetFileExpectancy then    //let the client know if the server expects missing files to be sent to
  begin
    case cmbFilesExistence.ItemIndex of
      0: Result := CREResp_FileExpectancy_ValueOnDisk;
      1: Result := CREResp_FileExpectancy_ValueFromClient;
      else
         Result := CREResp_FileExpectancy_ValueUnkown;
    end;

    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_RecordComponent then
  begin
    DoOnRecordComponent(StrToInt64Def(ASyncObj.FParams.Values[CREParam_Handle], -1), ASyncObj.FGPStream);
    Result := ''; //must be '', when responding with files
    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_ClearInMemFileSystem then
  begin
    FInMemFileSystem.Clear;
    Result := CREResp_Done;
    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_SetVariable then
  begin
    ASyncObj.FFrame.SetActionVarValue(ASyncObj.FParams.Values[CREParam_Var], ASyncObj.FParams.Values[CREParam_Value]);
    Result := CREResp_Done;
    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_TerminateWaitingForFileAvailability then
  begin
    TempStr := ASyncObj.FParams.Values[CREParam_TerminateWaitingLoop];
    frClickerActionsArrMain.AddToLog('Terminating waiting loops on request... The waiting action(s) should fail because of missing files.  Loop type: ' + TempStr);

    if TempStr = CREParam_TerminateWaitingLoop_ValueSingle then
      FTerminateWaitForFileAvailability := True
    else
      if TempStr = CREParam_TerminateWaitingLoop_ValueMulti then
        FTerminateWaitForMultipleFilesAvailability := True
      else
      begin
        FTerminateWaitForFileAvailability := True;
        FTerminateWaitForMultipleFilesAvailability := True;
      end;

    frClickerActionsArrMain.AddToLog('The waiting loops should be terminated (on request).');
    Result := CREResp_Done;
    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_PluginCmd then
  begin                               //ASyncObj.FFrame.StackLevel should be set automatically
    if ASyncObj.FParams.Values[CREParam_Cmd] = CREParam_Plugin_ContinueAll then
      ASyncObj.FFrame.PluginContinueAll := True;

    if ASyncObj.FParams.Values[CREParam_Cmd] = CREParam_Plugin_StepOver then
      ASyncObj.FFrame.PluginStepOver := True;

    if ASyncObj.FParams.Values[CREParam_Cmd] = CREParam_Plugin_SetBreakpoint then
    begin
      ASyncObj.FFrame.AddToLog('Setting breakpoint cmd: ' + FastReplace_ReturnToCSV(ASyncObj.FParams.Text));
      ASyncObj.FFrame.frClickerActions.frClickerPlugin.SetBreakpoint(StrToIntDef(ASyncObj.FParams.Values[CREParam_Plugin_SetBreakpoint_LineIndex], -1),
                                                                     StrToIntDef(ASyncObj.FParams.Values[CREParam_Plugin_SetBreakpoint_SelectedSourceFileIndex], -1),
                                                                     ASyncObj.FParams.Values[CREParam_Plugin_SetBreakpoint_Enabled] = '1');
    end;

    Result := CREResp_Done; //this response is for above commands

    if ASyncObj.FParams.Values[CREParam_Cmd] = CREParam_Plugin_RequestLineNumber then
    begin
      try
        Result := ASyncObj.FFrame.frClickerActions.frClickerPlugin.SelectedLine;
        AddToLog('SelectedLine response: "' + Result + '".');
      except
        on E: Exception do
        begin
          Result := CREResp_PluginDebuggingNotAvailable;
          AddToLog(CREResp_PluginDebuggingNotAvailable + ' exception: ' + E.Message);
        end;
      end;
    end;

    Exit;
  end;

  //if ASyncObj.FCmd = '/' + CRECmd_MouseDown then  //implemented here, in UI thread, if MouseDownTControl calls Application.ProcessMessages;
  //begin
  //  MouseDownTControl(ASyncObj.FParams);
  //  Result := CREResp_Done;
  //  Exit;
  //end;
  //
  //if ASyncObj.FCmd = '/' + CRECmd_MouseUp then    //implemented here, in UI thread, if MouseDownTControl calls Application.ProcessMessages;
  //begin
  //  MouseUpTControl(ASyncObj.FParams);
  //  Result := CREResp_Done;
  //  Exit;
  //end;

  if ASyncObj.FCmd = '/' + CRECmd_ExecuteClickAction then
  begin
    Result := CREResp_RemoteExecResponseVar + '=' + IntToStr(Ord(frClickerActionsArrMain.ActionExecution.ExecuteClickActionAsString(ASyncObj.FParams)));
    Result := Result + #8#7 + GetClkVariables87;
    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_ExecuteExecAppAction then
  begin
    Result := CREResp_RemoteExecResponseVar + '=' + IntToStr(Ord(frClickerActionsArrMain.ActionExecution.ExecuteExecAppActionAsString(ASyncObj.FParams)));
    Result := Result + #8#7 + GetClkVariables87;
    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_ExecuteFindControlAction then
  begin
    frClickerActionsArrMain.ExecutingActionFromRemote := True;
    frClickerActionsArrMain.FileLocationOfDepsIsMem := ASyncObj.FParams.Values[CREParam_FileLocation] = CREParam_FileLocation_ValueMem; //to load files from in-mem FS
    try
      frClickerActionsArrMain.StopAllActionsOnDemand := False;
      if frClickerActionsArrMain.StopAllActionsOnDemandFromParent <> nil then
        frClickerActionsArrMain.StopAllActionsOnDemandFromParent^ := False; //set this to avoid stopping child instances

      Result := CREResp_RemoteExecResponseVar + '=' + IntToStr(Ord(frClickerActionsArrMain.ActionExecution.ExecuteFindControlActionAsString(ASyncObj.FParams)));
      Result := Result + #8#7 + GetClkVariables87;
    finally
      frClickerActionsArrMain.ExecutingActionFromRemote := False;
      frClickerActionsArrMain.FileLocationOfDepsIsMem := False;
    end;

    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_ExecuteFindSubControlAction then
  begin
    frClickerActionsArrMain.frClickerActions.frClickerFindControl.UpdateBitmapAlgorithmSettings; //to hide grid
    frClickerActionsArrMain.ExecutingActionFromRemote := True;
    frClickerActionsArrMain.FileLocationOfDepsIsMem := ASyncObj.FParams.Values[CREParam_FileLocation] = CREParam_FileLocation_ValueMem; //to load files from in-mem FS
    try
      frClickerActionsArrMain.StopAllActionsOnDemand := False;
      if frClickerActionsArrMain.StopAllActionsOnDemandFromParent <> nil then
        frClickerActionsArrMain.StopAllActionsOnDemandFromParent^ := False; //set this to avoid stopping child instances

      Result := CREResp_RemoteExecResponseVar + '=' + IntToStr(Ord(frClickerActionsArrMain.ActionExecution.ExecuteFindSubControlActionAsString(ASyncObj.FParams)));
      Result := Result + #8#7 + GetClkVariables87;
    finally
      frClickerActionsArrMain.ExecutingActionFromRemote := False;
      frClickerActionsArrMain.FileLocationOfDepsIsMem := False;
    end;

    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_ExecuteSetControlTextAction then
  begin
    Result := CREResp_RemoteExecResponseVar + '=' + IntToStr(Ord(frClickerActionsArrMain.ActionExecution.ExecuteSetControlTextActionAsString(ASyncObj.FParams)));
    Result := Result + #8#7 + GetClkVariables87;
    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_ExecuteCallTemplateAction then
  begin
    frClickerActionsArrMain.ExecutingActionFromRemote := True;

    if ASyncObj.FParams.Values[CREParam_UseLocalDebugger] = '1' then
      frClickerActionsArrMain.UseLocalDebugger := True;

    if ASyncObj.FParams.Values[CREParam_UseServerDebugging] = '1' then
      frClickerActionsArrMain.UseLocalDebugger := True;

    frClickerActionsArrMain.FileLocationOfDepsIsMem := ASyncObj.FParams.Values[CREParam_FileLocation] = CREParam_FileLocation_ValueMem; //to load files from in-mem FS
    try
      frClickerActionsArrMain.StopAllActionsOnDemand := False;
      if frClickerActionsArrMain.StopAllActionsOnDemandFromParent <> nil then
        frClickerActionsArrMain.StopAllActionsOnDemandFromParent^ := False; //set this to avoid stopping child instances

      Result := CREResp_RemoteExecResponseVar + '=' + IntToStr(Ord(frClickerActionsArrMain.ActionExecution.ExecuteCallTemplateActionAsString(ASyncObj.FParams)));
      Result := Result + #8#7 + GetClkVariables87;
    finally
      frClickerActionsArrMain.ExecutingActionFromRemote := False;
      frClickerActionsArrMain.FileLocationOfDepsIsMem := False;

      if ASyncObj.FParams.Values[CREParam_UseLocalDebugger] = '1' then
        frClickerActionsArrMain.UseLocalDebugger := False;

      if ASyncObj.FParams.Values[CREParam_UseServerDebugging] = '1' then
      frClickerActionsArrMain.UseLocalDebugger := False;
    end;

    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_ExecuteSleepAction then
  begin
    frClickerActionsArrMain.StopAllActionsOnDemand := False;
    if frClickerActionsArrMain.StopAllActionsOnDemandFromParent <> nil then
      frClickerActionsArrMain.StopAllActionsOnDemandFromParent^ := False; //set this to avoid stopping child instances

    Result := CREResp_RemoteExecResponseVar + '=' + IntToStr(Ord(frClickerActionsArrMain.ActionExecution.ExecuteSleepActionAsString(ASyncObj.FParams)));
    Result := Result + #8#7 + GetClkVariables87;
    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_ExecuteSetVarAction then
  begin
    Result := CREResp_RemoteExecResponseVar + '=' + IntToStr(Ord(frClickerActionsArrMain.ActionExecution.ExecuteSetVarActionAsString(ASyncObj.FParams)));
    Result := Result + #8#7 + GetClkVariables87;
    Exit;
  end;

  if ASyncObj.FCmd = '/' + CRECmd_ExecuteWindowOperationsAction then
  begin
    Result := CREResp_RemoteExecResponseVar + '=' + IntToStr(Ord(frClickerActionsArrMain.ActionExecution.ExecuteWindowOperationsActionAsString(ASyncObj.FParams)));
    Result := Result + #8#7 + GetClkVariables87;
    Exit;
  end;

  if (ASyncObj.FCmd = '/' + CRECmd_ExecuteLoadSetVarFromFile) or
     (ASyncObj.FCmd = '/' + CRECmd_ExecuteLoadSetVarFromFileAction) then   //fixed the missing "Action" suffix.
  begin
    Result := CREResp_RemoteExecResponseVar + '=' + IntToStr(Ord(frClickerActionsArrMain.ActionExecution.ExecuteLoadSetVarFromFileActionAsString(ASyncObj.FParams)));
    Result := Result + #8#7 + GetClkVariables87;
    Exit;
  end;

  if (ASyncObj.FCmd = '/' + CRECmd_ExecuteSaveSetVarToFile) or
     (ASyncObj.FCmd = '/' + CRECmd_ExecuteSaveSetVarToFileAction) then     //fixed the missing "Action" suffix.
  begin
    Result := CREResp_RemoteExecResponseVar + '=' + IntToStr(Ord(frClickerActionsArrMain.ActionExecution.ExecuteSaveSetVarToFileActionAsString(ASyncObj.FParams)));
    Result := Result + #8#7 + GetClkVariables87;
    Exit;
  end;

  if (ASyncObj.FCmd = '/' + CRECmd_ExecutePlugin) or
     (ASyncObj.FCmd = '/' + CRECmd_ExecutePluginAction) then //fixed the missing "Action" suffix.
  begin
    RemoteState := ASyncObj.FFrame.ExecutingActionFromRemote;
    try
      frClickerActionsArrMain.ExecutingActionFromRemote := True;

      Result := CREResp_RemoteExecResponseVar + '=' + IntToStr(Ord(frClickerActionsArrMain.ActionExecution.ExecutePluginActionAsString(ASyncObj.FParams)));
      Result := Result + #8#7 + GetClkVariables87;
    finally
      ASyncObj.FFrame.ExecutingActionFromRemote := RemoteState //restore, in case the server is running unattended
    end;

    Exit;
  end;

  if (ASyncObj.FCmd = '/' + CRECmd_ExecuteEditTemplate) or
     (ASyncObj.FCmd = '/' + CRECmd_ExecuteEditTemplateAction) then //fixed the missing "Action" suffix.
  begin
    RemoteState := ASyncObj.FFrame.ExecutingActionFromRemote;
    try
      frClickerActionsArrMain.ExecutingActionFromRemote := True;

      Result := CREResp_RemoteExecResponseVar + '=' + IntToStr(Ord(frClickerActionsArrMain.ActionExecution.ExecuteEditTemplateActionAsString(ASyncObj.FParams)));
      Result := Result + #8#7 + GetClkVariables87;
    finally
      ASyncObj.FFrame.ExecutingActionFromRemote := RemoteState //restore, in case the server is running unattended
    end;

    Exit;
  end;

  {$IFDEF MemPlugins}
    {$IFDEF PluginTesting} //These requests are implemented for testing only. Ideally, they should be part of some unit testing only, not exposed by UIClicker.
      if ASyncObj.FCmd = '/' + CRECmd_GetMemPluginInMemFSCount then
      begin
        EnterCriticalSection(FDecDecHashArrCritSec);
        try
          Result := IntToStr(Length(FDecDecHashPluginInMemFSArr));
        finally
          LeaveCriticalSection(FDecDecHashArrCritSec);
        end;
        Exit;
      end;

      if ASyncObj.FCmd = '/' + CRECmd_DeleteAllMemPluginInMemFSes then
      begin
        EnterCriticalSection(FDecDecHashArrCritSec);
        try
          for i := 0 to Length(FDecDecHashPluginInMemFSArr) - 1 do
            FDecDecHashPluginInMemFSArr[i].InMemFS.Free;
          SetLength(FDecDecHashPluginInMemFSArr, 0);
        finally
          LeaveCriticalSection(FDecDecHashArrCritSec);
        end;

        Result := CREResp_ErrResponseOK;
        Exit;
      end;

      if ASyncObj.FCmd = '/' + CRECmd_GetListOfFilesFromMemPluginInMemFS then
      begin
        EnterCriticalSection(FDecDecHashArrCritSec);
        try
          FSIdx := StrToIntDef(ASyncObj.FParams.Values[CREParam_PluginFSIdx], -2);
          if (FSIdx <= -2) or (FSIdx > Length(FDecDecHashPluginInMemFSArr) - 1) then
          begin
            Result := CREResp_PluginFileSystemIndexOutOfBounds;
            Exit;
          end;

          ListOfFileNames := TStringList.Create;
          try
            if FSIdx = -1 then
              FPluginsInMemFileSystem.ListMemFiles(ListOfFileNames)
            else
              FDecDecHashPluginInMemFSArr[FSIdx].InMemFS.ListMemFiles(ListOfFileNames);

            Result := FastReplace_ReturnTo87(ListOfFileNames.Text);
            if Result = '' then
              Result := CREResp_ErrResponseOK;   //return a predefined text, to avoid returning '..200 OK..'

            AddToLog(CRECmd_GetListOfFilesFromMemPluginInMemFS + ': ' + Result);
          finally
            ListOfFileNames.Free;
          end;
        finally
          LeaveCriticalSection(FDecDecHashArrCritSec);
        end;

        Exit;
      end;

      if ASyncObj.FCmd = '/' + CRECmd_DeleteAllFilesFromMemPluginInMemFS then
      begin
        EnterCriticalSection(FDecDecHashArrCritSec);
        try
          FSIdx := StrToIntDef(ASyncObj.FParams.Values[CREParam_PluginFSIdx], -2);
          if (FSIdx <= -2) or (FSIdx > Length(FDecDecHashPluginInMemFSArr) - 1) then
          begin
            Result := CREResp_PluginFileSystemIndexOutOfBounds;
            Exit;
          end;

          if FSIdx = -1 then
            FPluginsInMemFileSystem.Clear
          else
            FDecDecHashPluginInMemFSArr[FSIdx].InMemFS.Clear;

          Result := CREResp_ErrResponseOK;
        finally
          LeaveCriticalSection(FDecDecHashArrCritSec);
        end;

        Exit;
      end;
    {$ELSE}
      if (ASyncObj.FCmd = '/' + CRECmd_GetMemPluginInMemFSCount) or
         (ASyncObj.FCmd = '/' + CRECmd_DeleteAllMemPluginInMemFSes) or
         (ASyncObj.FCmd = '/' + CRECmd_GetListOfFilesFromMemPluginInMemFS) or
         (ASyncObj.FCmd = '/' + CRECmd_DeleteAllFilesFromMemPluginInMemFS) then
      begin
        Result := CREResp_NotImplemented;
        Exit;
      end;
    {$ENDIF}
  {$ELSE}
    if (ASyncObj.FCmd = '/' + CRECmd_GetMemPluginInMemFSCount) or
       (ASyncObj.FCmd = '/' + CRECmd_DeleteAllMemPluginInMemFSes) or
       (ASyncObj.FCmd = '/' + CRECmd_GetListOfFilesFromMemPluginInMemFS) or
       (ASyncObj.FCmd = '/' + CRECmd_DeleteAllFilesFromMemPluginInMemFS) then
    begin
      Result := CREResp_NotImplemented;
      Exit;
    end;
  {$ENDIF}

  Result := 'unknown command';  //default if no command is recognized
  frClickerActionsArrMain.AddToLog(Result + ': ' + ASyncObj.FCmd);
  ASyncObj.FErrCode := 2;
end;


constructor TSyncHTTPCmd.Create;
begin
  inherited Create;
  FBmp := nil; //init here, to avoid dangling pointers
  FErrCode := 0;
end;


procedure TSyncHTTPCmd.DoSynchronize;
begin
  FResult := frmClickerActions.ProcessServerCmd(Self);
end;


function ProcessServerCommand(ACmd: string; AParams: TStrings; AOutBmp: TBitmap; AGPStream: TMemoryStream; ARenderedInMemFileSystem: TInMemFileSystem): string;
var
  SyncObj: TSyncHTTPCmd;
  Fnm: string;
  TempStream: TMemoryStream;
begin
  try
    //ExecuteCommand requires a special handling with two parts: executing the actual command, then getting the debugging content
    if ACmd = '/' + CRECmd_ExecuteCommandAtIndex then
    begin
      SyncObj := TSyncHTTPCmd.Create;
      try
        SyncObj.FCmd := ACmd;
        SyncObj.FParams := AParams; //it's ok to pass the pointer, however, it may not be ok to modify the list
        SyncObj.FFrame := nil;  //will get assigned on the first sync call
        SyncObj.FErrCode := 0;

        SyncObj.Synchronize;

        if SyncObj.FErrCode = 0 then
        begin
          repeat                 //this loop executes in the context of server's thread
            Sleep(2);
          until not SyncObj.FFrame.ExecutingActionFromRemote;  //reset by timer

          SyncObj.FCmd := '/' + CRECmd_GetExecuteCommandAtIndexResult;
          SyncObj.Synchronize;    //process a local request
        end;

        Result := SyncObj.FResult;
      finally
        SyncObj.Free;
      end;

      Exit;
    end;

    if ACmd = '/' + CRECmd_TestConnection then
    begin
      Result := CREResp_ConnectionOK;
      Exit;
    end;

    if ACmd = '/' + CRECmd_GetRenderedFile then
    begin
      Fnm := Copy(AParams.Values[CREParam_FileName], 1, CExtBmp_FilenameMaxLen);
      AddToLogFromThread('Requested rendered file: "' + Fnm + '"');

      if (Fnm <> '') and ARenderedInMemFileSystem.FileExistsInMem(Fnm) then
      begin
        TempStream := TMemoryStream.Create;
        try
          ARenderedInMemFileSystem.LoadFileFromMemToStream(Fnm, TempStream);
          TempStream.Position := 0;
          AOutBmp.LoadFromStream(TempStream, TempStream.Size);
          Result := '';
        finally
          TempStream.Free;
        end;
      end
      else
      begin
        Result := CREResp_FileNotFound + ': ' + Fnm;
        AddToLogFromThread('Requested rendered file not found: "' + Fnm + '"');
      end;

      Exit;
    end;

    if ACmd = '/' + CRECmd_MouseDown then
    begin
      MouseDownTControl(AParams);      //can be called from server thread, if it doesn't call Application.ProcessMessages
      Result := CREResp_Done;
      Exit;
    end;

    if ACmd = '/' + CRECmd_MouseUp then
    begin
      MouseUpTControl(AParams);
      Result := CREResp_Done;
      Exit;
    end;

    //default behavior
    SyncObj := TSyncHTTPCmd.Create;
    try
      SyncObj.FCmd := ACmd;
      SyncObj.FParams := AParams; //it's ok to pass the pointer, however, it may not be ok to modify the list
      SyncObj.FFrame := nil;  //will get assigned on the first sync call
      SyncObj.FErrCode := 0;
      SyncObj.FBmp := AOutBmp;
      SyncObj.FGPStream := AGPStream;

      SyncObj.Synchronize;

      Result := SyncObj.FResult;
    finally
      SyncObj.Free;
    end;
  except
    on E: Exception do
      Result := 'ProcessServerCommand exception: ' + E.Message;
  end;
end;


procedure TfrmClickerActions.IdHTTPServer1CommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  GettingImage, GettingGPStream: Boolean;
  Bmp: TBitmap;
  GPStream: TMemoryStream;
  Cmd: string;
begin
  Cmd := ARequestInfo.Document;
  ARequestInfo.Params.LineBreak := #13#10;

  AResponseInfo.ContentType := 'text/plain'; // 'text/html';  default type

  if Cmd = '/' + CRECmd_GetListOfWaitingFiles then   //this has priority over other commands  (it is used with KeepAlive)
  begin
    FProcessingMissingFilesRequestByServer := True;  //reset by timer
    AResponseInfo.ContentText := GetListOfWaitingFiles;

    if AResponseInfo.ContentText <> '' then
      AddToLogFromThread('Requesting list of missing files. List: ' + AResponseInfo.ContentText);

    Exit;
  end;

  if Cmd = '/' + CRECmd_GetListOfRenderedFiles then
  begin
    AResponseInfo.ContentText := FRenderedInMemFileSystem.ListMemFilesWithHashAsString;
    Exit;
  end;

  if Cmd = '/' + CRECmd_GetCompInfoAtPoint then
  begin
    AResponseInfo.ContentText := GetCompAtPoint(ARequestInfo.Params);
    Exit;
  end;

  GettingImage := False;
  if (Cmd = '/' + CRECmd_GetResultedDebugImage) or
     (Cmd = '/' + CRECmd_GetSearchAreaDebugImage) or
     (Cmd = '/' + CRECmd_GetScreenShotImage) or
     (Cmd = '/' + CRECmd_GetCurrentlyRecordedScreenShotImage) or
     (Cmd = '/' + CRECmd_GetRenderedFile) then
  begin
    Bmp := TBitmap.Create;
    GettingImage := True;
  end
  else
    Bmp := nil; //set here, in case ProcessServerCommand handles it

  GettingGPStream := False;
  if Cmd = '/' + CRECmd_RecordComponent then
  begin
    GPStream := TMemoryStream.Create;
    GettingGPStream := True;
  end
  else
    GPStream := nil;

  try
    AResponseInfo.ContentText := ProcessServerCommand(Cmd, ARequestInfo.Params, Bmp, GPStream, FRenderedInMemFileSystem);

    if GettingImage then
    begin
      if AResponseInfo.ContentText <> '' then
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
        //AddToLogFromThread('Sending bitmap of ' + IntToStr(AResponseInfo.ContentStream.Size) + ' bytes in size.');

        AResponseInfo.WriteHeader;
        AResponseInfo.WriteContent;
      finally
        AResponseInfo.ContentStream.Free;
        AResponseInfo.ContentStream := nil;
      end;

      Exit;
    end;

    if GettingGPStream then
    begin
      AResponseInfo.ContentText := '';
      AResponseInfo.ContentType := 'application/octet-stream';
      AResponseInfo.ContentDisposition := 'attachment';   //download by browser
      AResponseInfo.CharSet := 'US-ASCII';  //this is actually required, to prevent converting ASCII characters from 128-255 to '?'

      AResponseInfo.ContentStream := TMemoryStream.Create;
      try
        GPStream.Position := 0;
        AResponseInfo.ContentStream.CopyFrom(GPStream, GPStream.Size);

        AResponseInfo.ContentLength := AResponseInfo.ContentStream.Size;
        AddToLogFromThread('Sending stream of ' + IntToStr(AResponseInfo.ContentStream.Size) + ' bytes in size.');

        AResponseInfo.WriteHeader;
        AResponseInfo.WriteContent;
      finally
        AResponseInfo.ContentStream.Free;
        AResponseInfo.ContentStream := nil;
        AContext.Connection.Disconnect(True);
      end;

      Exit;
    end;
  finally
    if GettingImage then
      Bmp.Free;

    if GettingGPStream then
      GPStream.Free;
  end;
end;


procedure TfrmClickerActions.IdHTTPServer1CommandOther(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

  procedure RespondWithText(ErrMsg: string);
  begin
    AResponseInfo.ContentStream := TMemoryStream.Create;
    try
      AResponseInfo.ContentStream.Write(ErrMsg[1], Length(ErrMsg));
      AResponseInfo.ContentLength := AResponseInfo.ContentStream.Size;

      AResponseInfo.WriteHeader;
      AResponseInfo.WriteContent;
    finally
      AResponseInfo.ContentStream.Free;
      AResponseInfo.ContentStream := nil;
    end;
  end;

var
  Fnm, Msg: string;
  TempMemStream: TMemoryStream;
  ListOfFileNames, ListOfFileFound: TStringList;
  VerifyHashes: Boolean;
  i: Integer;
  {$IFDEF MemPlugins}
    UpperCaseFnmExt: string;
    PluginArchive: TClickerPluginArchive;
    IsDecDecHash: Boolean;
    PreventReusingInMemFSFlag: Boolean;
    InMemFSIndex: Integer;
    PrevDecDecHashPlugin: Integer;
  {$ENDIF}
begin
  if ARequestInfo.CommandType <> hcPUT then
    Exit;

  if ARequestInfo.PostStream = nil then
  begin
    RespondWithText('Expecting a file.');
    Exit;
  end;

  //it's ok to handle filesystem-related requests from server thread

  if ARequestInfo.Document = '/' + CRECmd_SendFileToServer then
  begin
    Fnm := ARequestInfo.Params.Values[CREParam_FileName];
    if Trim(Fnm) = '' then
    begin
      RespondWithText('Expecting a valid filename.');
      Exit;
    end;

    if ARequestInfo.PostStream.Size > 100 * 1048576 then
    begin
      RespondWithText(CREResp_FileTooLarge);
      Exit;
    end;

    if FInMemFileSystem.TotalFileSize > 100 * 1048576 then
    begin
      RespondWithText(CREResp_FileSystemFull);
      Exit;
    end;

    if FInMemFileSystem.TotalFileCount > 300 then
    begin
      RespondWithText(CREResp_TooManyFilesInFileSystem);
      Exit;
    end;

    TempMemStream := TMemoryStream.Create;
    try
      TempMemStream.CopyFrom(ARequestInfo.PostStream, ARequestInfo.PostStream.Size);
      FInMemFileSystem.SaveFileToMem(Fnm, TempMemStream.Memory, TempMemStream.Size);   //FInMemFileSystem is used for general purpose files (including bitmaps) in server mode.

      Msg := 'Received file: "' + Fnm + '"  of ' + IntToStr(TempMemStream.Size) + ' bytes in size.';
      AddToLogFromThread(Msg);
      RespondWithText(Msg);
    finally
      TempMemStream.Free;
    end;

    Exit;
  end;

  if ARequestInfo.Document = '/' + CRECmd_SetRenderedFile then
  begin
    Fnm := ARequestInfo.Params.Values[CREParam_FileName];
    if Trim(Fnm) = '' then
    begin
      RespondWithText('Expecting a valid filename.');
      Exit;
    end;

    if ARequestInfo.PostStream.Size > 100 * 1048576 then
    begin
      RespondWithText(CREResp_FileTooLarge);
      Exit;
    end;

    if FRenderedInMemFileSystem.TotalFileSize > 100 * 1048576 then
    begin
      RespondWithText(CREResp_FileSystemFull);
      Exit;
    end;

    if FRenderedInMemFileSystem.TotalFileCount > 200 then
    begin
      RespondWithText(CREResp_TooManyFilesInFileSystem);
      Exit;
    end;

    TempMemStream := TMemoryStream.Create;
    try
      TempMemStream.CopyFrom(ARequestInfo.PostStream, ARequestInfo.PostStream.Size);
      FRenderedInMemFileSystem.SaveFileToMem(Fnm, TempMemStream.Memory, TempMemStream.Size);   //FRenderedInMemFileSystem is used for bitmaps

      Msg := 'Received file: "' + Fnm + '"  of ' + IntToStr(TempMemStream.Size) + ' bytes in size (for ExtRndInMemFS).';
      AddToLogFromThread(Msg);
      RespondWithText(CREResp_ErrResponseOK);
    finally
      TempMemStream.Free;
    end;

    Exit;
  end;

  {$IFDEF MemPlugins}
    if (ARequestInfo.Document = '/' + CRECmd_SetMemPluginFile) or
       (ARequestInfo.Document = '/' + CRECmd_SetMemPluginArchiveFile) then
    begin
      IsDecDecHash := ARequestInfo.Params.Values[CREParam_IsDecDecHash] = '1';
      PreventReusingInMemFSFlag := Pos(CREParam_PreventReusingInMemFS + '=' + 'True', ARequestInfo.Params.Values[CREParam_AdditionalInfo]) > 0;

      Fnm := ARequestInfo.Params.Values[CREParam_FileName];
      UpperCaseFnmExt := UpperCase(ExtractFileExt(Fnm));

      //if IsDecDecHash or (not IsDecDecHash and (UpperCase(ExtractFileExt(ARequestInfo.Params.Values[CREParam_FileName])) = '.DLL')) then
      begin
        AddToLogFromThread('Archive AdditionalInfo: ' + ARequestInfo.Params.Values[CREParam_AdditionalInfo]);
        AddToLogFromThread('Archive PreventReusingInMemFSFlag: ' + BoolToStr(PreventReusingInMemFSFlag, 'True', 'False'));

        EnterCriticalSection(FDecDecHashArrCritSec);
        try
          InMemFSIndex := GetPluginInMemFSIndex(FDecDecHashPluginInMemFSArr, ARequestInfo.Params.Values[CREParam_FileName]);
          if InMemFSIndex = -1 then
          begin
            if Length(FDecDecHashPluginInMemFSArr) >= 45 then //21 then
            begin
              RespondWithText(CREResp_TooManyPluginFileSystems);
              AddToLogFromThread(Msg);
              Exit;
            end
            else
            begin
              if ((UpperCaseFnmExt = '.DBGSYM') or (UpperCaseFnmExt = '.DBGSYMARC')) and (Length(FDecDecHashPluginInMemFSArr) > 0) then
              begin            //.dbgsym files end up in the first FS
                InMemFSIndex := 0; //use the first available item, although it may not be related to its plugin
                AddToLogFromThread('Using the first InMemFS for a plugin .dbgsym file.');
              end
              else
              begin
                PrevDecDecHashPlugin := Length(FDecDecHashPluginInMemFSArr);
                AddToLogFromThread('Allocating a new InMemFS for a plugin.');
                SetLength(FDecDecHashPluginInMemFSArr, Length(FDecDecHashPluginInMemFSArr) + 1);
                InMemFSIndex := Length(FDecDecHashPluginInMemFSArr) - 1;

                FDecDecHashPluginInMemFSArr[InMemFSIndex].Name := ARequestInfo.Params.Values[CREParam_FileName];
                FDecDecHashPluginInMemFSArr[InMemFSIndex].InMemFS := TInMemFileSystem.Create; //allocate a new FS for this plugin
                FDecDecHashPluginInMemFSArr[InMemFSIndex].PreventReusingInMemFS := PreventReusingInMemFSFlag;
              end;

              if (UpperCaseFnmExt = '.DBGSYM') or (UpperCaseFnmExt = '.DBGSYMARC') then
                if not IsDecDecHash then
                  FDecDecHashPluginInMemFSArr[InMemFSIndex].PreventReusingInMemFS := False;  // .dbgsym files will not allocate a new FS

              if FDecDecHashPluginInMemFSArr[InMemFSIndex].PreventReusingInMemFS then
                AddToLogFromThread('This FS prevents being reused for other archives with the same name: ' + FDecDecHashPluginInMemFSArr[InMemFSIndex].Name);
            end;                                                                  ///////////////ToDo: deallocate InMemFS, and remove item from FDecDecHashPluginInMemFSArr, on exception and when not needed anymore
          end   // InMemFSIndex = -1
          else
          begin // File found, InMemFSIndex > -1
            if FDecDecHashPluginInMemFSArr[InMemFSIndex].PreventReusingInMemFS or
              (FDecDecHashPluginInMemFSArr[InMemFSIndex].IsDecDecHashPlugin <> IsDecDecHash) then  //changing the IsDecDecHashPlugin flag is not alowed
            begin        //It would be nice to allocate a new FS with a random name and respond with that name, but in case of error, that name has to be combined with the error message.
              AddToLogFromThread('There may be available file systems, but this one is not allowed to be allocated, because the name already exists and it can''t be reused. FS Name: ' + FDecDecHashPluginInMemFSArr[InMemFSIndex].Name);
              RespondWithText(CREResp_TooManyPluginFileSystems);
              AddToLogFromThread(Msg);
              Exit;
            end;

            AddToLogFromThread('Received a plugin with an existing name. Reusing FS: ' + ARequestInfo.Params.Values[CREParam_FileName] + '  at index ' + IntToStr(InMemFSIndex) + '.  Archive[' + IntToStr(InMemFSIndex) + '].PreventReusingInMemFSFlag: ' + BoolToStr(FDecDecHashPluginInMemFSArr[InMemFSIndex].PreventReusingInMemFS, 'True', 'False'));
          end;
        finally
          LeaveCriticalSection(FDecDecHashArrCritSec);
        end;
      end;
    end;   //CRECmd_SetMemPluginFile or CRECmd_SetMemPluginArchiveFile

    if ARequestInfo.Document = '/' + CRECmd_SetMemPluginFile then
    begin
      Fnm := ARequestInfo.Params.Values[CREParam_FileName];
      if Trim(Fnm) = '' then
      begin
        RespondWithText('Expecting a valid filename.');
        Exit;
      end;

      if ARequestInfo.PostStream.Size > 100 * 1048576 then
      begin
        RespondWithText(CREResp_FileTooLarge);
        Exit;
      end;

      if ExtractFileName(Fnm) = Fnm then //no path
        Fnm := CMemPluginLocationPrefix + PathDelim + Fnm;

      if Pos(':' + PathDelim, Fnm) = 2 then
        Fnm := CMemPluginLocationPrefix + Copy(Fnm, 3, MaxInt);

      if Pos(UpperCase(CMemPluginLocationPrefix), UpperCase(Fnm)) = 1 then  // e.g. MEM:\PathToDll\MyPlugin.dll
        Fnm := CMemPluginLocationPrefix + Copy(Fnm, 5, MaxInt);

      UpperCaseFnmExt := UpperCase(ExtractFileExt(Fnm));

      TempMemStream := TMemoryStream.Create;
      try
        TempMemStream.CopyFrom(ARequestInfo.PostStream, ARequestInfo.PostStream.Size);

        EnterCriticalSection(FDecDecHashArrCritSec);
        try
          if FPluginsInMemFileSystem.TotalFileSize > 100 * 1048576 then  //FPluginsInMemFileSystem validations moved inside CritSec
          begin
            RespondWithText(CREResp_FileSystemFull);
            Exit;
          end;

          if FPluginsInMemFileSystem.TotalFileCount > 200 then
          begin
            RespondWithText(CREResp_TooManyFilesInFileSystem);
            Exit;
          end;

          if not IsDecDecHash and (UpperCaseFnmExt <> '.DLL') and (UpperCaseFnmExt <> '.DLLARC') and (UpperCaseFnmExt <> '.DBGSYM') and (UpperCaseFnmExt <> '.DBGSYMARC') then  //ordinary plugin             // save ordinary plugins to new FSes
            FPluginsInMemFileSystem.SaveFileToMem(Fnm, TempMemStream.Memory, TempMemStream.Size)   //FPluginsInMemFileSystem is used for plugins and their files (dbgsym or config files)
          else   //DecDecHash plugin
            FDecDecHashPluginInMemFSArr[InMemFSIndex].InMemFS.SaveFileToMem(Fnm, TempMemStream.Memory, TempMemStream.Size);   //FDecDecHashPluginInMemFSArr is used for DecDecHash plugins and their files (dbgsym or config files)

          FDecDecHashPluginInMemFSArr[InMemFSIndex].IsDecDecHashPlugin := IsDecDecHash;
        finally
          LeaveCriticalSection(FDecDecHashArrCritSec);
        end;

        if not IsDecDecHash then
          Msg := 'Received plugin file: "' + Fnm + '"  of ' + IntToStr(TempMemStream.Size) + ' bytes in size (for PluginInMemFS).'
        else
          Msg := 'Received DecDecHash plugin file: "' + Fnm + '"  of ' + IntToStr(TempMemStream.Size) + ' bytes in size (for DecDecHashPluginInMemFS).';

        AddToLogFromThread(Msg);
        RespondWithText(CREResp_ErrResponseOK);
      finally
        TempMemStream.Free;
      end;

      Exit;
    end;

    if ARequestInfo.Document = '/' + CRECmd_SetMemPluginArchiveFile then
    begin
      Fnm := ARequestInfo.Params.Values[CREParam_FileName];
      if Trim(Fnm) = '' then
      begin
        RespondWithText('Expecting a valid filename.');
        Exit;
      end;

      if ARequestInfo.PostStream.Size > 100 * 1048576 then
      begin
        RespondWithText(CREResp_FileTooLarge);
        Exit;
      end;

      UpperCaseFnmExt := UpperCase(ExtractFileExt(Fnm));

      TempMemStream := TMemoryStream.Create;
      try
        TempMemStream.CopyFrom(ARequestInfo.PostStream, ARequestInfo.PostStream.Size);
        //FPluginsInMemFileSystem.SaveFileToMem(Fnm, TempMemStream.Memory, TempMemStream.Size);   //FPluginsInMemFileSystem is used for plugins and their files (dbgsym or config files)

        if not IsDecDecHash then
          Msg := 'Received plugin archive file: "' + Fnm + '"  of ' + IntToStr(TempMemStream.Size) + ' bytes in size (for PluginInMemFS).'
        else
          Msg := 'Received DecDecHash plugin archive file: "' + Fnm + '"  of ' + IntToStr(TempMemStream.Size) + ' bytes in size (for DecDecHashPluginInMemFS).';

        EnterCriticalSection(FDecDecHashArrCritSec);
        try
          if FPluginsInMemFileSystem.TotalFileSize > 100 * 1048576 then  //FPluginsInMemFileSystem validations moved inside CritSec
          begin
            RespondWithText(CREResp_FileSystemFull);
            Exit;
          end;

          if FPluginsInMemFileSystem.TotalFileCount > 200 then
          begin
            RespondWithText(CREResp_TooManyFilesInFileSystem);
            Exit;
          end;

          try
            PluginArchive := TClickerPluginArchive.Create;
            try
              PluginArchive.OnAddToLog := HandlePluginAddToLog;
              PluginArchive.OnSetVar := HandleOnSetVar;

              if not IsDecDecHash and (UpperCaseFnmExt <> '.DLL') and (UpperCaseFnmExt <> '.DLLARC') and (UpperCaseFnmExt <> '.DBGSYM') and (UpperCaseFnmExt <> '.DBGSYMARC') then              // save ordinary plugins to new FSes
                PluginArchive.PluginsInMemFS := FPluginsInMemFileSystem   //ordinary plugin
              else
                PluginArchive.PluginsInMemFS := FDecDecHashPluginInMemFSArr[InMemFSIndex].InMemFS;  //DecDecHash plugin

              FDecDecHashPluginInMemFSArr[InMemFSIndex].IsDecDecHashPlugin := IsDecDecHash;

              AddToLogFromThread('DecryptionPluginName is "' + ARequestInfo.Params.Values[CREParam_DecryptionPluginName] + '".');
              AddToLogFromThread('DecompressionPluginName is "' + ARequestInfo.Params.Values[CREParam_DecompressionPluginName] + '".');
              AddToLogFromThread('HashingPluginName is "' + ARequestInfo.Params.Values[CREParam_HashingPluginName] + '".');

              PluginArchive.ExtractArchive(TempMemStream,
                                           @FDecDecHashPluginInMemFSArr,
                                           ARequestInfo.Params.Values[CREParam_DecryptionPluginName],
                                           ARequestInfo.Params.Values[CREParam_DecompressionPluginName],
                                           ARequestInfo.Params.Values[CREParam_HashingPluginName],
                                           StrToIntDef(ARequestInfo.Params.Values[CREParam_CompressionLevel], 0),
                                           ARequestInfo.Params.Values[CREParam_AdditionalInfo]);
            finally
              PluginArchive.Free;
            end;

            AddToLogFromThread(Msg);
            RespondWithText(CREResp_ErrResponseOK);
          except
            on E: Exception do
            begin
              Msg := Msg + ' But...';
              AddToLogFromThread(Msg);
              AddToLogFromThread('DecryptionPluginName is "' + ARequestInfo.Params.Values[CREParam_DecryptionPluginName] + '".');
              AddToLogFromThread('DecompressionPluginName is "' + ARequestInfo.Params.Values[CREParam_DecompressionPluginName] + '".');
              AddToLogFromThread('HashingPluginName is "' + ARequestInfo.Params.Values[CREParam_HashingPluginName] + '".');

              //Remove some "internal" details
              if E.Message = 'OnDecompress is not assigned.' then
                E.Message := 'Decompression plugin not set.';

              if Pos('Archive is invalid. Hash mismatch.', E.Message) = 1 then   //remove hash info
              begin
                E.Message := 'Archive is invalid. Hash mismatch.';

                //Archive cannot be decrypted, so remove the FS if allocated for it
                if PrevDecDecHashPlugin = Length(FDecDecHashPluginInMemFSArr) - 1 then
                begin
                  FDecDecHashPluginInMemFSArr[Length(FDecDecHashPluginInMemFSArr) - 1].InMemFS.Free;
                  SetLength(FDecDecHashPluginInMemFSArr, Length(FDecDecHashPluginInMemFSArr) - 1);
                end;
              end;

              if E.Message = 'Cannot find exported symbol.' then
                E.Message := 'One of the required functions is not exported by the plugin.';

              Msg := CREResp_PluginError + ': ' + E.Message;
              AddToLogFromThread(Msg);
              RespondWithText(Msg);
            end;
          end;
        finally
          LeaveCriticalSection(FDecDecHashArrCritSec);
        end;
      finally
        TempMemStream.Free;
      end;

      Exit;
    end;
  {$ELSE}
    if (ARequestInfo.Document = '/' + CRECmd_SetMemPluginFile) or
       (ARequestInfo.Document = '/' + CRECmd_SetMemPluginArchiveFile) then
    begin
      RespondWithText(CREResp_NotImplemented);
      Exit;
    end;
  {$ENDIF}  //MemPlugins

  if ARequestInfo.Document = '/' + CRECmd_GetFileExistenceOnServer then
  begin
    VerifyHashes := ARequestInfo.Params.Values[CREParam_VerifyHashes] = '1';

    ListOfFileNames := TStringList.Create;
    ListOfFileFound := TStringList.Create;
    try
      ARequestInfo.PostStream.Position := 0;
      ListOfFileNames.LoadFromStream(ARequestInfo.PostStream);

      //AddToLogFromThread('List of files to be verified if exist in in-mem fs (' + IntToStr(ListOfFileNames.Count) + '):' + #13#10 + ListOfFileNames.Text);  //for debugging only
      //AddToLogFromThread('===================================== End of file names to be verified. ===============  DebugParam=' + ARequestInfo.Params.Values[CREParam_DebugParam]);

      if VerifyHashes then
      begin
        for i := 0 to ListOfFileNames.Count - 1 do
          ListOfFileFound.Add(IntToStr(Ord(FInMemFileSystem.FileExistsInMemWithHash(ListOfFileNames.Strings[i]))));
      end
      else
      begin
        for i := 0 to ListOfFileNames.Count - 1 do
          ListOfFileFound.Add(IntToStr(Ord(FInMemFileSystem.FileExistsInMem(ListOfFileNames.Strings[i]))));
      end;

      RespondWithText(ListOfFileFound.Text);
    finally
      ListOfFileNames.Free;
      ListOfFileFound.Free;
    end;

    Exit;
  end;
end;


procedure TfrmClickerActions.IdHTTPServer1Connect(AContext: TIdContext);
begin
  AContext.Connection.Socket.ReadTimeout := 3600000;   //if no bytes are received in 1h, then close the connection
end;


procedure TfrmClickerActions.IdHTTPServer1Exception(AContext: TIdContext;
  AException: Exception);
begin
  try
    if AException.Message <> 'Connection Closed Gracefully.' then
      AddToLogFromThread('Server exception: ' + AException.Message);
  except
  end;
end;


procedure TfrmClickerActions.lbePathToTemplatesChange(Sender: TObject);
begin
  lbePathToTemplates.Hint := 'The $AppDir$ replacement is available.';

  if Pos('..', lbePathToTemplates.Text) > 0 then //there may be files or folder names with two dots which will not be allowed because of this
  begin
    lbePathToTemplates.Font.Color := clRed;
    lbePathToTemplates.Hint := lbePathToTemplates.Hint + #13#10#13#10 +
                               'The ".." folder name is not allowed in paths, because UIClicker''s file provider does not allow it.';
  end
  else
    lbePathToTemplates.Font.Color := clWindowText;
end;


procedure TfrmClickerActions.lbePathToTemplatesKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  FullTemplatesDir := lbePathToTemplates.Text;
  if FFullTemplatesDir > '' then
    if FFullTemplatesDir[Length(FFullTemplatesDir)] = '\' then
      FFullTemplatesDir := Copy(FFullTemplatesDir, 1, Length(FFullTemplatesDir) - 1);
end;


procedure TfrmClickerActions.pmClientModeServerAddressPopup(Sender: TObject);
var
  i: Integer;
  TempMenuItem: TMenuItem;
begin
  MenuItem_RemoveServerAddress.Clear;

  for i := 0 to cmbClientModeServerAddress.Items.Count - 1 do
  begin
    TempMenuItem := TMenuItem.Create(Self);
    TempMenuItem.Caption := cmbClientModeServerAddress.Items.Strings[i];
    TempMenuItem.OnClick := HandleOnRemoveServerAddressClick;

    MenuItem_RemoveServerAddress.Insert(0, TempMenuItem);
  end;
end;


procedure TfrmClickerActions.HandleOnRemoveServerAddressClick(Sender: TObject);
var
  s: string;
begin
  s := (Sender as TMenuItem).Caption;
  s := StringReplace(s, '&', '', [rfReplaceAll]);
  cmbClientModeServerAddress.Items.Delete(cmbClientModeServerAddress.Items.IndexOf(s));
end;


procedure TfrmClickerActions.tmrDelayedShowTimer(Sender: TObject);
begin
  tmrDelayedShow.Enabled := False;
  DoOnReLoadSettings;        //load settings again, after adjusting various frame widths/heights and positions
end;


//using a polling timer, instead of sync-ing every request with UI
procedure TfrmClickerActions.tmrDisplayMissingFilesRequestsTimer(Sender: TObject);
const
  CRequestColor: array[Boolean] of TColor = (clGreen, clLime);
  CRequestFontColor: array[Boolean] of TColor = (clWhite, clBlack);
begin
  case cmbExecMode.ItemIndex of
    0:
    begin
      pnlMissingFilesRequest.Color := CRequestColor[False];
      pnlMissingFilesRequest.Font.Color := CRequestFontColor[False];
    end;

    1:
      if FProcessingMissingFilesRequestByClient then
      begin
        FProcessingMissingFilesRequestByClient := False;
        pnlMissingFilesRequest.Color := CRequestColor[True];
        pnlMissingFilesRequest.Font.Color := CRequestFontColor[True];
      end
      else
      begin   //displaying "False" on next timer iteration, to allow it to be visible
        pnlMissingFilesRequest.Color := CRequestColor[False];
        pnlMissingFilesRequest.Font.Color := CRequestFontColor[False];
      end;

    2:
      if FProcessingMissingFilesRequestByServer then
      begin
        FProcessingMissingFilesRequestByServer := False;
        pnlMissingFilesRequest.Color := CRequestColor[True];
        pnlMissingFilesRequest.Font.Color := CRequestFontColor[True];
      end
      else
      begin   //displaying "False" on next timer iteration, to allow it to be visible
        pnlMissingFilesRequest.Color := CRequestColor[False];
        pnlMissingFilesRequest.Font.Color := CRequestFontColor[False];
      end;
  end;
end;


procedure TfrmClickerActions.tmrStartupTimer(Sender: TObject);
var
  {$IFDEF TestBuild}
    ExecMode: string;
    AutoSwitchToExecTab: string;
    AutoEnableSwitchTabsOnDebugging: string;
    UseWideStringsOnGetControlTextOption: string;
    AddAppArgsToLog: string;
  {$ELSE}

  {$ENDIF}
  i: Integer;
begin
  tmrStartup.Enabled := False;

  //some startup code here
  {$IFDEF TestBuild}
    ExecMode := GetCmdLineOptionValue('--SetExecMode');
    SetExecutionMode(cmbExecMode.Items.IndexOf(ExecMode));

    AutoSwitchToExecTab := GetCmdLineOptionValue('--AutoSwitchToExecTab');
    AutoEnableSwitchTabsOnDebugging := GetCmdLineOptionValue('--AutoEnableSwitchTabsOnDebugging');
    UseWideStringsOnGetControlTextOption := GetCmdLineOptionValue('--UseWideStringsOnGetControlText');
    AddAppArgsToLog := GetCmdLineOptionValue('--AddAppArgsToLog');

    if AutoSwitchToExecTab <> '' then
    begin
      chkAutoSwitchToExecutingTab.Checked := AutoSwitchToExecTab = 'Yes';
      FAutoSwitchToExecutingTab := chkAutoSwitchToExecutingTab.Checked;
    end;

    if AutoEnableSwitchTabsOnDebugging <> '' then
    begin
      chkAutoEnableSwitchingTabsOnDebugging.Checked := AutoEnableSwitchTabsOnDebugging = 'Yes';
      FAutoEnableSwitchingTabsOnDebugging := chkAutoEnableSwitchingTabsOnDebugging.Checked;
    end;

    if UseWideStringsOnGetControlTextOption <> '' then
    begin
      UseWideStringsOnGetControlText := UseWideStringsOnGetControlTextOption = 'Yes';
      if UseWideStringsOnGetControlText then
        AddToLog('Using wide strings...')
      else
        AddToLog('Not using wide strings...');
    end;

    if AddAppArgsToLog = 'Yes' then
    begin
      for i := 1 to ParamCount shr 1 do
        AddToLog(ParamStr(i shl 1 - 1) + ' = "' + ParamStr(i shl 1 - 1 + 1) + '"');
    end;
  {$ELSE}
    for i := 1 to ParamCount do
      if (ParamStr(i) = '--ExtraCaption') or (ParamStr(i) = '--SetExecMode') or (ParamStr(i) = '--ServerPort') then  // some common options
        AddToLog('The application is not built for testing, so it won''t accept command line options. Please rebuild with "TestBuild" compiler directive.');
  {$ENDIF}

  try
    frClickerActionsArrMain.SetActionVarValue('$TemplateDir$', FFullTemplatesDir);
    frClickerActionsArrExperiment1.SetActionVarValue('$TemplateDir$', FFullTemplatesDir);
    frClickerActionsArrExperiment2.SetActionVarValue('$TemplateDir$', FFullTemplatesDir);

    //frClickerActionsArrMain.SetActionVarValue('$SelfTemplateDir$', frClickerActionsArrMain.FileName);  //FileName is still not available
  except
    //AV in case the frames are not created
    AddToLog('Action frames are not created when setting $TemplateDir$');
  end;

  {$IFnDEF Windows}
    Font.Size := 8;
    grpMissingFilesMonitoring.Font.Size := 7;
    lbePathToTemplates.Font.Size := 7;
    cmbExecMode.Font.Size := 7;
    lblAdminStatus.Font.Size := 7;
    cmbClientModeServerAddress.Font.Size := 7;
    lbeConnectTimeout.Font.Size := 7;
    lbeServerModePort.Font.Size := 7;
    cmbFilesExistence.Font.Size := 7;

    lbePathToTemplates.LabelSpacing := 1;
    lblServerAddress.Font.Size := 7;
    lbeConnectTimeout.LabelSpacing := 1;
    lbeServerModePort.LabelSpacing := 1;
  {$ENDIF}
end;


procedure TfrmClickerActions.tmrUpdateColorsTimer(Sender: TObject);
begin
  tmrUpdateColors.Enabled := False;
  //colcmbTopLeftValid.Selected := FPreviewSelectionColors.TopLeft_Valid;
  //colcmbBotRightValid.Selected := FPreviewSelectionColors.BotRight_Valid;
  //colcmbTopLeftInvalid.Selected := FPreviewSelectionColors.TopLeft_Invalid;
  //colcmbBotRightInvalid.Selected := FPreviewSelectionColors.BotRight_Invalid;

  colcmbTopLeftValid.AutoSelect := False;
  colcmbBotRightValid.AutoSelect := False;
  colcmbTopLeftInvalid.AutoSelect := False;
  colcmbBotRightInvalid.AutoSelect := False;
  colcmbTopLeftValid.ItemIndex := colcmbTopLeftValid.Items.IndexOfObject(TObject(TColor(FPreviewSelectionColors.TopLeft_Valid)));
  colcmbBotRightValid.ItemIndex := colcmbBotRightValid.Items.IndexOfObject(TObject(TColor(FPreviewSelectionColors.BotRight_Valid)));
  colcmbTopLeftInvalid.ItemIndex := colcmbTopLeftInvalid.Items.IndexOfObject(TObject(TColor(FPreviewSelectionColors.TopLeft_Invalid)));
  colcmbBotRightInvalid.ItemIndex := colcmbBotRightInvalid.Items.IndexOfObject(TObject(TColor(FPreviewSelectionColors.BotRight_Invalid)));
  colcmbTopLeftValid.AutoSelect := True;
  colcmbBotRightValid.AutoSelect := True;
  colcmbTopLeftInvalid.AutoSelect := True;
  colcmbBotRightInvalid.AutoSelect := True;
end;


procedure TfrmClickerActions.tmrUpdateSelectionColorsFromColorBoxesTimer(
  Sender: TObject);
begin
  tmrUpdateSelectionColorsFromColorBoxes.Enabled := False;

  FPreviewSelectionColors.TopLeft_Valid := colcmbTopLeftValid.Selected;
  FPreviewSelectionColors.BotRight_Valid := colcmbBotRightValid.Selected;
  FPreviewSelectionColors.TopLeft_Invalid := colcmbTopLeftInvalid.Selected;
  FPreviewSelectionColors.BotRight_Invalid := colcmbBotRightInvalid.Selected;

  if not Assigned(frClickerActionsArrMain) then
    Exit;

  //using individual try..except sections, because some of the objects may not be created yet
  try
    frClickerActionsArrExperiment1.PreviewSelectionColors := FPreviewSelectionColors;
  except
  end;

  try
    frClickerActionsArrExperiment2.PreviewSelectionColors := FPreviewSelectionColors;
  except
  end;

  try
    frClickerActionsArrMain.PreviewSelectionColors := FPreviewSelectionColors;
  except
  end;
end;


procedure TfrmClickerActions.SetExecutionMode(AMode: Integer);
var
  Port, ConnectsTo: string;
  ConnectionTimeout: Integer;
begin
  if AMode = -1 then
    Exit;

  cmbExecMode.ItemIndex := AMode;

  case AMode of
    1: //client
    begin
      ConnectsTo := GetCmdLineOptionValue('--ConnectsTo');
      cmbClientModeServerAddress.Text := ConnectsTo;
      ConnectionTimeout := StrToIntDef(GetCmdLineOptionValue('--ConnectionTimeout'), 1000);

      if ConnectionTimeout < -1 then
        ConnectionTimeout := -1;
      lbeConnectTimeout.Text := IntToStr(ConnectionTimeout);
    end;

    2: //server
    begin
      Port := GetCmdLineOptionValue('--ServerPort');
      if Port <> '' then
        lbeServerModePort.Text := IntToStr(StrToIntDef(Port, 5444));

      chkServerActive.Checked := True;
    end;
  end;

  ProcessChangingExecutionMode;
end;


procedure TfrmClickerActions.ProcessChangingExecutionMode;
const
  CClientExecModeInfoTxt: array[0..2] of string = ('off', 'on', 'off');
  CServerExecModeInfoTxt: array[0..2] of string = ('off', 'off', 'on');
  CClientExecModeInfoColor: array[0..2] of TColor = (clOlive, clGreen, clOlive);
  CServerExecModeInfoColor: array[0..2] of TColor = (clOlive, clOlive, clGreen);
var
  tk: QWord;
begin
  frClickerActionsArrMain.ExecutesRemotely := cmbExecMode.ItemIndex = 1;
  frClickerActionsArrExperiment1.ExecutesRemotely := (cmbExecMode.ItemIndex = 1) and chkSetExperimentsToClientMode.Checked;
  frClickerActionsArrExperiment2.ExecutesRemotely := (cmbExecMode.ItemIndex = 1) and chkSetExperimentsToClientMode.Checked;

  frClickerActionsArrMain.ExecutingActionFromRemote := False;        //prevent entering a waiting loop in PlayAllActions
  frClickerActionsArrExperiment1.ExecutingActionFromRemote := False;
  frClickerActionsArrExperiment2.ExecutingActionFromRemote := False;

  PageControlExecMode.ActivePageIndex := cmbExecMode.ItemIndex;

  try
    if cmbExecMode.ItemIndex <> 2 then  //it is local or client mode, then deactivate server if active
    begin
      cmbExecMode.Enabled := False;
      try
        if IdHTTPServer1.Active then    //switching from server to local or client mode
        begin
          IdHTTPServer1.Active := False;

          tk := GetTickCount64;
          repeat
            Application.ProcessMessages;
            Sleep(10);

            if GetTickCount64 - tk > GeneralConnectTimeout shl 1 + CFileProviderIterationInterval + 100 then
              Break; //a delay long enough, so the client module does not connect to the server module of the same app instance
          until False;
        end;

        chkServerActive.Checked := False;
      finally
        cmbExecMode.Enabled := True;
      end;
    end;

    if cmbExecMode.ItemIndex <> 1 then   //local or server
    begin
      if FPollForMissingServerFiles <> nil then
      begin
        frClickerActionsArrMain.AddToLog('Stopping "missing files" monitoring thread for client mode.');
        FPollForMissingServerFiles.Terminate;

        cmbExecMode.Enabled := False;
        try
          tk := GetTickCount64;
          repeat
            Application.ProcessMessages;
            Sleep(10);

            if FPollForMissingServerFiles.Done then
            begin
              frClickerActionsArrMain.AddToLog('Monitoring thread terminated.');
              Break;
            end;

            if GetTickCount64 - tk > 1500 then
            begin
              frClickerActionsArrMain.AddToLog('Timeout waiting for monitoring thread to terminate. The thread is still running (probably waiting).');
              Break;
            end;
          until False;
        finally
          cmbExecMode.Enabled := True;
        end;

        FPollForMissingServerFiles := nil;
      end;

      if cmbExecMode.ItemIndex = 2 then
        IdHTTPServer1.KeepAlive := chkKeepAlive.Checked;
    end;

    if cmbExecMode.ItemIndex = 1 then //client
    begin
      GeneralConnectTimeout := StrToIntDef(lbeConnectTimeout.Text, 1000); //update GeneralConnectTimeout only after deactivating the server module

      frClickerActionsArrMain.RemoteAddress := GetConfiguredRemoteAddress;
      frClickerActionsArrExperiment1.RemoteAddress := GetConfiguredRemoteAddress;
      frClickerActionsArrExperiment2.RemoteAddress := GetConfiguredRemoteAddress;

      if FPollForMissingServerFiles <> nil then
      begin
        cmbExecMode.ItemIndex := 0; //force local mode, for now, then let the user switch again later
        Sleep(100);
        PageControlExecMode.ActivePageIndex := cmbExecMode.ItemIndex;
        frClickerActionsArrMain.AddToLog('Stopping "missing files" monitoring thread (again) for client mode. (If the execution mode keeps keeps going back to "Local", you might have to close the whole application).');
        try
          FPollForMissingServerFiles.Terminate; //terminate existing thread, so a new one can be created
        except
          on E: Exception do
            frClickerActionsArrMain.AddToLog('Exception on stopping client thread. Maybe it''s already done: ' + E.Message);
        end;

        try
          tk := GetTickCount64;
          repeat
            Application.ProcessMessages;
            Sleep(10);

            if FPollForMissingServerFiles.Done then
            begin
              frClickerActionsArrMain.AddToLog('Monitoring thread terminated.');
              Break;
            end;

            if GetTickCount64 - tk > 1500 then
            begin
              frClickerActionsArrMain.AddToLog('Timeout waiting for monitoring thread to terminate. The thread is still running (probably waiting).');
              Break;
            end;
          until False;

          if not FPollForMissingServerFiles.Done then
            Exit; //let the user switch back again
        except
          on E: Exception do
            frClickerActionsArrMain.AddToLog('Exception waiting to stop client thread. Maybe it''s already done: ' + E.Message);
        end;

        frClickerActionsArrMain.AddToLog('Creating a new client thread...');
        cmbExecMode.ItemIndex := 1; //force local mode, for now, then let the user switch again later
        Sleep(10);
        PageControlExecMode.ActivePageIndex := cmbExecMode.ItemIndex;
      end;

      FPollForMissingServerFiles := TPollForMissingServerFiles.Create(True);
      FPollForMissingServerFiles.RemoteAddress := GetConfiguredRemoteAddress;
      FPollForMissingServerFiles.ConnectTimeout := StrToIntDef(lbeConnectTimeout.Text, 1000);
      FPollForMissingServerFiles.FullTemplatesDir := FFullTemplatesDir;

      FPollForMissingServerFiles.AddListOfAccessibleDirs(memAllowedFileDirsForServer.Lines);
      FPollForMissingServerFiles.AddListOfAccessibleFileExtensions(memAllowedFileExtensionsForServer.Lines);
      FPollForMissingServerFiles.OnBeforeRequestingListOfMissingFiles := HandleOnBeforeRequestingListOfMissingFiles;
      FPollForMissingServerFiles.OnAfterRequestingListOfMissingFiles := HandleOnAfterRequestingListOfMissingFiles;
      FPollForMissingServerFiles.OnFileExists := HandleOnFileExists;
      FPollForMissingServerFiles.OnLogMissingServerFile := HandleLogMissingServerFile;
      FPollForMissingServerFiles.OnLoadMissingFileContent := HandleOnLoadMissingFileContent;
      FPollForMissingServerFiles.OnDenyFile := HandleOnDenyFile;
      FPollForMissingServerFiles.Start;

      frClickerActionsArrMain.AddToLog('Started "missing files" monitoring thread for client mode.');
      if cmbClientModeServerAddress.Items.IndexOf(cmbClientModeServerAddress.Text) = -1 then
        cmbClientModeServerAddress.Items.Add(cmbClientModeServerAddress.Text);
    end;
  finally
    lblClientMode.Caption := 'Client mode ' + CClientExecModeInfoTxt[cmbExecMode.ItemIndex];
    lblServerMode.Caption := 'Server mode ' + CServerExecModeInfoTxt[cmbExecMode.ItemIndex];
    lblClientMode.Font.Color := CClientExecModeInfoColor[cmbExecMode.ItemIndex];
    lblServerMode.Font.Color := CServerExecModeInfoColor[cmbExecMode.ItemIndex];
  end;

  Sleep(100); //prevent fast changing of running modes
end;


procedure TfrmClickerActions.cmbExecModeChange(Sender: TObject);
begin
  ProcessChangingExecutionMode;
end;


procedure TfrmClickerActions.UpdateGridType;
var
  NewOption: TDisplayGridLineOption;
begin
  NewOption := TDisplayGridLineOption(cmbImgPreviewGridType.ItemIndex);

  frClickerActionsArrExperiment1.GridDrawingOption := NewOption;
  frClickerActionsArrExperiment2.GridDrawingOption := NewOption;
  frClickerActionsArrMain.GridDrawingOption := NewOption;
end;


procedure TfrmClickerActions.cmbImgPreviewGridTypeChange(Sender: TObject);
begin
  UpdateGridType;
end;


procedure TfrmClickerActions.UpdatePreviewSelectionColorsFromColorBoxes;
begin
  tmrUpdateSelectionColorsFromColorBoxes.Enabled := True;
end;


procedure TfrmClickerActions.colcmbTopLeftValidChange(Sender: TObject);
begin
  UpdatePreviewSelectionColorsFromColorBoxes;
end;


procedure TfrmClickerActions.colcmbBotRightValidChange(Sender: TObject);
begin
  UpdatePreviewSelectionColorsFromColorBoxes;
end;


procedure TfrmClickerActions.colcmbTopLeftInvalidChange(Sender: TObject);
begin
  UpdatePreviewSelectionColorsFromColorBoxes;
end;


procedure TfrmClickerActions.colcmbBotRightInvalidChange(Sender: TObject);
begin
  UpdatePreviewSelectionColorsFromColorBoxes;
end;


procedure TfrmClickerActions.chkServerActiveChange(Sender: TObject);
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
      AddToLog(s);

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


procedure TfrmClickerActions.chkStayOnTopClick(Sender: TObject);
begin
  if chkStayOnTop.Checked then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;


procedure TfrmClickerActions.btnTestConnectionClick(Sender: TObject);
var
  RemoteAddress, Response: string;
begin
  RemoteAddress := GetConfiguredRemoteAddress;
  Response := TestConnection(RemoteAddress);
  MessageBox(Handle, PChar('Server response: ' + Response), PChar(Application.Title), MB_ICONINFORMATION);
end;


procedure TfrmClickerActions.chkAutoSwitchToExecutingTabChange(Sender: TObject);
begin
  FAutoSwitchToExecutingTab := chkAutoSwitchToExecutingTab.Checked;
end;


procedure TfrmClickerActions.chkAutoEnableSwitchingTabsOnDebuggingChange(
  Sender: TObject);
begin
  FAutoEnableSwitchingTabsOnDebugging := chkAutoEnableSwitchingTabsOnDebugging.Checked;
end;


procedure TfrmClickerActions.btnBrowseActionTemplatesDirClick(Sender: TObject);
var
  AOpenDialog: TSelectDirectoryDialog;
  s: string;
begin
  AOpenDialog := TSelectDirectoryDialog.Create(nil);
  try
    AOpenDialog.Filter := 'Clicker template files (*.clktmpl)|*.clktmpl|All files (*.*)|*.*';
    AOpenDialog.InitialDir := StringReplace(lbePathToTemplates.Text, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);

    if AOpenDialog.Execute then
    begin
      lbePathToTemplates.Text := StringReplace(AOpenDialog.FileName, ExtractFileDir(ParamStr(0)), '$AppDir$', [rfReplaceAll]);
      FullTemplatesDir := lbePathToTemplates.Text;
      if FullTemplatesDir > '' then
        if FullTemplatesDir[Length(FullTemplatesDir)] = '\' then
        begin
          s := FullTemplatesDir;
          Delete(s, Length(s), 1);
          FullTemplatesDir := s;
        end;
    end;
  finally
    AOpenDialog.Free;
  end;
end;


procedure TfrmClickerActions.chkDisplayActivityChange(Sender: TObject);
begin
  tmrDisplayMissingFilesRequests.Enabled := chkDisplayActivity.Checked;
end;


procedure TfrmClickerActions.HandleOnBeforeRequestingListOfMissingFiles;  //client thread calls this, without UI sync
begin
  FProcessingMissingFilesRequestByClient := True;
end;


procedure TfrmClickerActions.HandleOnAfterRequestingListOfMissingFiles;   //client thread calls this, without UI sync
begin
  //FProcessingMissingFilesRequestByClient := False;  //let the timer reset the flag
end;


function TfrmClickerActions.HandleOnComputeInMemFileHash(AFileContent: Pointer; AFileSize: Int64): string;
begin
  Result := ComputeHash(AFileContent, AFileSize);
end;


function TfrmClickerActions.HandleOnFileExists(const FileName: string): Boolean;
var
  TempFileName: string;
begin
  TempFileName := FileName;
  TempFileName := StringReplace(TempFileName, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);
  TempFileName := StringReplace(TempFileName, '$TemplateDir$', FFullTemplatesDir, [rfReplaceAll]);
  ////////////////////////////// '$SelfTemplateDir$' depends on the caller frame

  //Result := False;         //It seems that templates end up here without setting FileLocationOfDepsIsMem to flMem.
  //
  //if CExpectedFileLocation[frClickerActionsArrMain.FileLocationOfDepsIsMem] = flMem then  ////////////////////////////// ToDo: handle also other frames than frClickerActionsArrMain
  //  Result := FInMemFileSystem.FileExistsInMem(TempFileName)
  //else
  //  if CExpectedFileLocation[frClickerActionsArrMain.FileLocationOfDepsIsMem] = flDisk then ////////////////////////////// ToDo: handle also other frames than frClickerActionsArrMain
  //    Result := DoOnFileExists(TempFileName);

  Result := DoOnFileExists(TempFileName);
end;


procedure TfrmClickerActions.HandleOnGetSelfHandles(AListOfSelfHandles: TStringList);
begin
  DoOnGetSelfHandles(AListOfSelfHandles);
end;


procedure TfrmClickerActions.HandleLogMissingServerFile(AMsg: string);
begin
  AddToLogFromThread(AMsg);
end;


procedure TfrmClickerActions.HandleOnLoadMissingFileContent(AFileName: string; AFileContent: TMemoryStream);
begin
  AFileName := StringReplace(AFileName, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);
  AFileName := StringReplace(AFileName, '$TemplateDir$', FFullTemplatesDir, [rfReplaceAll]);

  AFileContent.LoadFromFile(AFileName);
  try
    AFileContent.LoadFromFile(AFileName);
  except
    Sleep(300); //maybe the file is in use by another thread, so wait a bit, then load again
    AFileContent.LoadFromFile(AFileName);
  end;
end;


procedure TfrmClickerActions.HandleOnDenyFile(AFileName: string);
var
  Response: string;
begin                //This handler is executed by a different thread, not the UI one.
  AddToLogFromThread('Sending a "' + CRECmd_TerminateWaitingForFileAvailability + '" command to server, because of denied file: "' + AFileName + '".');
  Response := TerminateWaitingForFileAvailability(ConfiguredRemoteAddress, CREParam_TerminateWaitingLoop_ValueAll, 0, False);
  AddToLogFromThread('"TerminateWaitingForFileAvailability" response: ' + Response);
end;


function TfrmClickerActions.HandleOnTClkIniReadonlyFileCreate(AFileName: string): TClkIniReadonlyFile;
begin
  Result := DoOnTClkIniReadonlyFileCreate(AFileName);
end;


function TfrmClickerActions.HandleOnTClkIniFileCreate(AFileName: string): TClkIniFile;
begin
  Result := DoOnTClkIniFileCreate(AFileName);
end;


procedure TfrmClickerActions.HandleOnSaveTemplateToFile(AStringList: TStringList; const FileName: string);
begin
  DoOnSaveTemplateToFile(AStringList, FileName);
end;


procedure TfrmClickerActions.HandleOnSetOpenDialogMultiSelect;
begin
  DoOnSetOpenDialogMultiSelect;
end;


procedure TfrmClickerActions.HandleOnSetOpenDialogInitialDir(AInitialDir: string);
begin
  DoOnSetOpenDialogInitialDir(AInitialDir);
end;


function TfrmClickerActions.HandleOnOpenDialogExecute(AFilter: string): Boolean;
begin
  Result := DoOnOpenDialogExecute(AFilter);
end;


function TfrmClickerActions.HandleOnGetOpenDialogFileName: string;
begin
  Result := DoOnGetOpenDialogFileName;
end;


procedure TfrmClickerActions.HandleOnSetSaveDialogInitialDir(AInitialDir: string);
begin
  DoOnSetSaveDialogInitialDir(AInitialDir);
end;


function TfrmClickerActions.HandleOnSaveDialogExecute(AFilter: string): Boolean;
begin
  Result := DoOnSaveDialogExecute(AFilter);
end;


function TfrmClickerActions.HandleOnGetSaveDialogFileName: string;
begin
  Result := DoOnGetSaveDialogFileName;
end;


procedure TfrmClickerActions.HandleOnSetSaveDialogFileName(AFileName: string);
begin
  DoOnSetSaveDialogFileName(AFileName);
end;


procedure TfrmClickerActions.HandleOnSetPictureSetOpenDialogMultiSelect;
begin
  DoOnSetPictureSetOpenDialogMultiSelect;
end;


procedure TfrmClickerActions.HandleOnSetPictureOpenDialogInitialDir(AInitialDir: string);
begin
  DoOnSetPictureOpenDialogInitialDir(AInitialDir);
end;


function TfrmClickerActions.HandleOnPictureOpenDialogExecute: Boolean;
begin
  Result := DoOnPictureOpenDialogExecute;
end;


function TfrmClickerActions.HandleOnGetPictureOpenDialogFileName: string;
begin
  Result := DoOnGetPictureOpenDialogFileName;
end;


function TfrmClickerActions.HandleOnGetGridDrawingOption: TDisplayGridLineOption;
var
  SelectedOption: Integer;
begin
  SelectedOption := cmbImgPreviewGridType.ItemIndex;
  if SelectedOption < 0 then
    SelectedOption := 0;

  Result := TDisplayGridLineOption(SelectedOption);
end;


procedure TfrmClickerActions.HandleOnGetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);
begin
  AFontFinderSettings := FFontFinderSettings;
end;


procedure TfrmClickerActions.HandleOnSetFontFinderSettings(var AFontFinderSettings: TFontFinderSettings);
begin
  FFontFinderSettings := AFontFinderSettings;
end;


procedure TfrmClickerActions.HandleOnRetrieveRenderedBmpFromServer(ARemoteAddress, AFnm: string);
var
  ReceivedList: TStringList;
  ReceivedItem, BmpRes: string;
  i, Idx: Integer;
  Bmp: TBitmap;
begin
  ReceivedList := TStringList.Create;
  try
    ReceivedList.Text := GetListOfRenderedFilesFromServer(ARemoteAddress, False); // and verify if local file has the same hash

    ReceivedItem := '';
    Idx := -1;
    for i := 0 to ReceivedList.Count - 1 do
    begin
      ReceivedItem := ReceivedList.Strings[i];

      if Pos(AFnm + CDefaultInMemFileNameHashSeparator, ReceivedItem) = 1 then
      begin
        Idx := i;
        Break;
      end;
    end;

    if Idx > -1 then
      if not FRenderedInMemFileSystem.FileExistsInMemWithHash(ReceivedItem) then
      begin
        AddToLog('Updating rendered bitmap: "' + AFnm + '"...');

        Bmp := TBitmap.Create;
        try
          BmpRes := GetRenderedFileFromServer(ARemoteAddress, AFnm, Bmp);
          AddToLog('Rendered bitmap updating result: ' + BmpRes);

          if BmpRes = '' then
            SaveBmpToInMemFileSystem(AFnm, Bmp, FRenderedInMemFileSystem);
        finally
          Bmp.Free;
        end;
      end;
  finally
    ReceivedList.Free;
  end;
end;


procedure TfrmClickerActions.HandleOnOpenCalledTemplateInExperimentTab(AExperimentIndex: Integer; ATemplatePath: string);
begin
  case AExperimentIndex of
    0:
    begin
      frClickerActionsArrExperiment1.LoadTemplateWithUIUpdate(ATemplatePath);
      PageControlMain.ActivePage := TabSheetExperiments1;
    end;

    1:
    begin
      frClickerActionsArrExperiment2.LoadTemplateWithUIUpdate(ATemplatePath);
      PageControlMain.ActivePage := TabSheetExperiments2;
    end;
  end;
end;


procedure TfrmClickerActions.HandleOnSaveFileToExtRenderingInMemFS(AFileName: string; AContent: Pointer; AFileSize: Int64);
begin
  FRenderedInMemFileSystem.SaveFileToMem(AFileName, AContent, AFileSize);
end;


procedure TfrmClickerActions.HandleOnAddFileNameToRecent(AFileName: string);
begin
  if FRecentTemplates.IndexOf(AFileName) = -1 then
    if UpperCase(ExtractFileExt(AFileName)) = '.CLKTMPL' then
      FRecentTemplates.Add(AFileName);
end;


procedure TfrmClickerActions.HandleOnGetListOfRecentFiles(AList: TStringList);
begin
  AList.Assign(FRecentTemplates);
end;


function TfrmClickerActions.HandleOnGenerateAndSaveTreeWithWinInterp(AHandle: THandle; ATreeFileName: string; AStep: Integer; AUseMouseSwipe: Boolean): Boolean;
begin
  Result := DoOnGenerateAndSaveTreeWithWinInterp(AHandle, ATreeFileName, AStep, AUseMouseSwipe);
end;


function TfrmClickerActions.HandleOnSetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue: string): Boolean;
begin
  Result := DoOnSetWinInterpOption(AWinInterpOptionName, AWinInterpOptionValue);
end;


function TfrmClickerActions.HandleOnGetListeningPort: Word;
begin
  Result := StrToIntDef(lbeServerModePort.Text, 5444);
end;


procedure TfrmClickerActions.HandlePluginAddToLog(s: string);
begin
  AddToLog('Plugin: ' + s);
end;


procedure TfrmClickerActions.HandleOnSetVar(AVarName, AVarValue: string);
begin
  SetVarFromThread(AVarName, AVarValue);
end;


initialization
  frmClickerActions := nil;

end.

