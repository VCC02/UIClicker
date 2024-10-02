{
    Copyright (C) 2024 VCC
    creation date: Dec 2019
    initial release date: 26 Jul 2022

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


unit ClickerActionsClient;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ClickerUtils, Graphics;


type
  TClientThread = class(TThread)
  private
    FLink: string;
    FResult: string;
    FDone: Boolean;
    FConnectTimeout: Integer;
    FStreamForServer: TMemoryStream;
    FResponseStream: TMemoryStream;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean {$IFDEF FPC}; const StackSize: SizeUInt = DefaultStackSize {$ENDIF});
    property Link: string read FLink write FLink;
    property Result: string read FResult;
    property Done: Boolean read FDone;
    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout;
  end;


const
  CDefaultNoTemplate = 'NoName.clktmpl';
  CWaitForFileAvailabilityTimeout = 300000; //5min / file, if waiting for a single file
  CWaitForMultipleFilesAvailabilityTimeout = 60000;  //1min / file, if waiting for a multiple file
  CClientExceptionPrefix = 'Client exception: ';

const
  CREParam_ActionIdx = 'ActionIdx';
  CREParam_StackLevel = 'StackLevel';
  CREParam_IsDebugging = 'IsDebugging';
  CREParam_UseLocalDebugger = 'UseLocalDebugger';
  CREParam_Grid = 'Grid';
  CREParam_FileName = 'FileName';
  CREParam_FileLocation = 'FileLocation';
  CREParam_VerifyHashes = 'VerifyHashes';

  CREParam_X = 'X';
  CREParam_Y = 'Y';
  CREParam_Handle = 'Handle';

  CREParam_FileLocation_ValueDisk = 'Disk';
  CREParam_FileLocation_ValueMem = 'Mem';

  CREParam_DebugParam = 'Dbg';
  CREParam_Var = 'Var';
  CREParam_Value = 'Value';
  CREParam_Cmd = 'Cmd';

  CREParam_TerminateWaitingLoop = 'Loop'; //loop type can be one of the following values: All, Single, Multi
  CREParam_TerminateWaitingLoop_ValueAll = 'All';  //both loops, for single file and multiple files
  CREParam_TerminateWaitingLoop_ValueSingle = 'Single'; //terminates the loop waiting for a single file
  CREParam_TerminateWaitingLoop_ValueMulti = 'Multi';   //terminates the loop waiting for multiple files

  CREParam_Plugin_ContinueAll = 'ContinueAll';
  CREParam_Plugin_StepOver = 'StepOver';
  CREParam_Plugin_RequestLineNumber = 'RequestLineNumber';
  CREParam_Plugin_SetBreakpoint = 'SetBreakpoint';
  CREParam_Plugin_SetBreakpoint_LineIndex = 'LineIndex';
  CREParam_Plugin_SetBreakpoint_SelectedSourceFileIndex = 'SelectedSourceFileIndex';
  CREParam_Plugin_SetBreakpoint_Enabled = 'Enabled';

  CRECmd_TestConnection = 'TestConnection';
  CRECmd_ExecuteCommandAtIndex = 'ExecuteCommandAtIndex';
  CRECmd_GetExecuteCommandAtIndexResult = 'GetExecuteCommandAtIndexResult';
  CRECmd_StopTemplateExecution = 'StopTemplateExecution';
  CRECmd_ExitTemplate = 'ExitTemplate';
  CRECmd_GetAllReplacementVars = 'GetAllReplacementVars';
  CRECmd_SendFileToServer = 'SendFileToServer';
  CRECmd_LoadTemplateInExecList = 'LoadTemplateInExecList';
  CRECmd_GetFileExpectancy = 'GetFileExpectancy';
  CRECmd_GetFileExistenceOnServer = 'GetFileExistenceOnServer';
  CRECmd_GetListOfWaitingFiles = 'GetListOfWaitingFiles';
  CRECmd_GetResultedDebugImage = 'GetResultedDebugImage'; //the image from the debugging tab, resulted after executing FindSubControl
  CRECmd_GetSearchAreaDebugImage = 'GetSearchAreaDebugImage'; //the image from Find(Sub)Control tabs, as a screenshot of the latest found (sub)control.
  CRECmd_GetScreenShotImage = 'GetScreenShotImage';
  CRECmd_GetCurrentlyRecordedScreenShotImage = 'GetCurrentlyRecordedScreenShotImage';
  CRECmd_GetCompInfoAtPoint = 'GetCompInfoAtPoint';
  CRECmd_RecordComponent = 'RecordComponent';
  CRECmd_ClearInMemFileSystem = 'ClearInMemFileSystem';
  CRECmd_SetVariable = 'SetVariable';
  CRECmd_TerminateWaitingForFileAvailability = 'TerminateWaitingForFileAvailability';  //three options, both loops, single file only, multi files only  (default both).  See CREParam_TerminateWaitingLoop params.
  CRECmd_GetListOfRenderedFiles = 'GetListOfRenderedFiles'; //all filenames + hashes in the second in-mem file system (rendered files, received from a rendering server)
  CRECmd_GetRenderedFile = 'GetRenderedFile';  //Retreives a bitmap file from ExternalRenderingInMem file system. Files can be added here by executing $RenderBmpExternally()$ or they can be saved by plugins.
  CRECmd_SetRenderedFile = 'SetRenderedFile';  //Adds or updates a bitmap file to the ExternalRenderingInMem file system.
  CRECmd_MouseDown = 'MouseDown';
  CRECmd_MouseUp = 'MouseUp';
  CRECmd_PluginCmd = 'PluginCmd'; //requires additional parameters to decide what command to do. It is used to "remote click" plugin debugging buttons.   see CREParam_Plugin_ContinueAll

  CRECmd_ExecuteClickAction = 'ExecuteClickAction';
  CRECmd_ExecuteExecAppAction = 'ExecuteExecAppAction';
  CRECmd_ExecuteFindControlAction = 'ExecuteFindControlAction';
  CRECmd_ExecuteFindSubControlAction = 'ExecuteFindSubControlAction';
  CRECmd_ExecuteSetControlTextAction = 'ExecuteSetControlTextAction';
  CRECmd_ExecuteCallTemplateAction = 'ExecuteCallTemplateAction';
  CRECmd_ExecuteSleepAction = 'ExecuteSleepAction';
  CRECmd_ExecuteSetVarAction = 'ExecuteSetVarAction';
  CRECmd_ExecuteWindowOperationsAction = 'ExecuteWindowOperationsAction';
  CRECmd_ExecuteLoadSetVarFromFile = 'ExecuteLoadSetVarFromFile';
  CRECmd_ExecuteSaveSetVarToFile = 'ExecuteSaveSetVarToFile';
  CRECmd_ExecutePlugin = 'ExecutePlugin';
  CRECmd_ExecuteEditTemplate = 'ExecuteEditTemplate';

  CREResp_ConnectionOK = 'Connection ok';
  CREResp_RemoteExecResponseVar = '$RemoteExecResponse$';
  CREResp_FileExpectancy_ValueOnDisk = 'OnDisk';           //the server expects that templates and bmps to exist on disk
  CREResp_FileExpectancy_ValueFromClient = 'FromClient';   //the server expects that templates and bmps to be received from client
  CREResp_FileExpectancy_ValueUnkown = 'Unkown';           //default response when the option is not handled by server
  CREResp_ReceivedFile = 'Received file';
  CREResp_TemplateLoaded = 'Loaded';
  CREResp_FileNotFound = 'FileNotFound';
  CREResp_PluginDebuggingNotAvailable = 'PluginDebuggingNotAvailable'; //the plugin debugging frame is not created, probably because the request is made outside of a plugin debugging session

  CREResp_ActionNotFound = 'ActionNotFound';
  CREResp_ActionAlreadyExists = 'ActionAlreadyExists';

  CREResp_ErrParam = 'Err';
  CREResp_ErrResponseOK = 'OK';
  CREResp_Done = 'Done';
  CREResp_HandleParam = 'Handle';
  CREResp_TextParam = 'Text';
  CREResp_ClassParam = 'Class';
  CREResp_ScreenWidth = 'ScreenWidth';
  CREResp_ScreenHeight = 'ScreenHeight';
  CREResp_CompLeft = 'CompLeft';
  CREResp_CompTop = 'CompTop';
  CREResp_CompWidth = 'CompWidth';
  CREResp_CompHeight = 'CompHeight';


function TestConnection(ARemoteAddress: string; ACallAppProcMsg: Boolean = True): string;
function WaitForServerResponse(ATh: TClientThread; ACallAppProcMsg: Boolean = True): Boolean; //used for requests with custom waiting
function SendTextRequestToServer(AFullLink: string; ACallAppProcMsg: Boolean = True): string;
function SendGetFileRequestToServer(AFullLink: string; AStream: TMemoryStream): string;
//function SendFileToServer(AFullLink: string; AFileContent, AResponseStream: TMemoryStream; ACallAppProcMsg: Boolean = True): string; overload; //expose this only if needed
function SendFileToServer(AFullLink: string; AFileContent: TMemoryStream; ACallAppProcMsg: Boolean = True): string; overload;

function StopRemoteTemplateExecution(ARemoteAddress: string; AStackLevel: Integer; ACallAppProcMsg: Boolean = True): string;
function ExitRemoteTemplate(ARemoteAddress: string; AStackLevel: Integer): string;  //called by client, to send a request to server to close a tab
function GetAllReplacementVars(ARemoteAddress: string; AStackLevel: Integer): string;
function GetDebugImageFromServer(ARemoteAddress: string; AStackLevel: Integer; AReceivedBmp: TBitmap; AWithGrid: Boolean): string; //returns error message if any
function GetDebugImageFromServerAsStream(ARemoteAddress: string; AStackLevel: Integer; AReceivedStream: TMemoryStream; AWithGrid: Boolean): string; //returns error message if any
function GetSearchAreaDebugImageFromServer(ARemoteAddress: string; AStackLevel: Integer; AReceivedBmp: TBitmap): string; //returns error message if any
function SendTemplateContentToServer(ARemoteAddress, AFileName: string; var ACustomClkActions: TClkActionsRecArr): string;
function SendLoadTemplateInExecListRequest(ARemoteAddress, AFileName: string; AStackLevel: Integer): string;
function GetServerFileExpectancy(ARemoteAddress: string): string;
function GetFileExistenceOnServer(ARemoteAddress: string; AListOfFiles, AListOfResults: TStringList; AListOfFilesIncludesHashes: Boolean; ADebugParam: string = ''): string; overload;
function GetFileExistenceOnServer(ARemoteAddress: string; AFileName: string; AFileIncludesHash: Boolean; ADebugParam: string = ''): Boolean; overload;

function GetScreenShotImageFromServer(ARemoteAddress: string; AReceivedBmp: TBitmap): string; //returns error message if any
function GetCurrentlyRecordedScreenShotImageFromServer(ARemoteAddress: string; AReceivedBmp: TBitmap): string; //returns error message if any
function GetCompInfoAtPoint(ARemoteAddress: string; X, Y: Integer): string;
function RecordComponentOnServer(ARemoteAddress: string; AHandle: THandle; AComponentContent: TMemoryStream): string;
function ClearInMemFileSystem(ARemoteAddress: string): string;
function SetVariable(ARemoteAddress, AVarName, AVarValue: string; AStackLevel: Integer): string;
function TerminateWaitingForFileAvailability(ARemoteAddress, ALoopType: string; AStackLevel: Integer; ACallAppProcMsg: Boolean = True): string;
function GetListOfRenderedFilesFromServer(ARemoteAddress: string; ACallAppProcMsg: Boolean = True): string;
function GetRenderedFileFromServer(ARemoteAddress: string; AFileName: string; AReceivedBmp: TBitmap): string; //Returns error message if any. Returns a bitmap with error as text if file not found.

function SendMouseDown(ARemoteAddress: string; AMouseParams: TStringList): string;
function SendMouseUp(ARemoteAddress: string; AMouseParams: TStringList): string;
function SendPluginCmd(ARemoteAddress: string; APluginCmd: string; AStackLevel: Integer; ACallAppProcMsg: Boolean = True): string;

function ExecuteClickAction(ARemoteAddress: string; AClickOptions: TClkClickOptions; ACallAppProcMsg: Boolean = True): string;
function ExecuteExecAppAction(ARemoteAddress: string; AExecAppOptions: TClkExecAppOptions; AActionName: string; AActionTimeout: Integer; ACallAppProcMsg: Boolean = True): string;
function ExecuteFindControlAction(ARemoteAddress: string; AFindControlOptions: TClkFindControlOptions; AActionName: string; AActionTimeout: Integer; AFileLocation: string; ACallAppProcMsg: Boolean = True): string;
function ExecuteFindSubControlAction(ARemoteAddress: string; AFindControlOptions: TClkFindControlOptions; AActionName: string; AActionTimeout: Integer; AFileLocation: string; ACallAppProcMsg: Boolean = True): string;
function ExecuteSetControlTextAction(ARemoteAddress: string; ASetTextOptions: TClkSetTextOptions; ACallAppProcMsg: Boolean = True): string;
function ExecuteCallTemplateAction(ARemoteAddress: string; ACallTemplateOptions: TClkCallTemplateOptions; AIsDebugging, AUseLocalDebugger: Boolean; AFileLocation: string; ACallAppProcMsg: Boolean = True): string;
function ExecuteSleepAction(ARemoteAddress: string; ASleepOptions: TClkSleepOptions; AActionName: string; ACallAppProcMsg: Boolean = True): string;
function ExecuteSetVarAction(ARemoteAddress: string; ASetVarOptions: TClkSetVarOptions; ACallAppProcMsg: Boolean = True): string;
function ExecuteWindowOperationsAction(ARemoteAddress: string; AWindowOperationsOptions: TClkWindowOperationsOptions; ACallAppProcMsg: Boolean = True): string;
function ExecuteLoadSetVarFromFileAction(ARemoteAddress: string; ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions; ACallAppProcMsg: Boolean = True): string;
function ExecuteSaveSetVarToFileAction(ARemoteAddress: string; ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions; ACallAppProcMsg: Boolean = True): string;
function ExecutePluginAction(ARemoteAddress: string; APluginOptions: TClkPluginOptions; ACallAppProcMsg: Boolean = True): string;
function ExecuteEditTemplateAction(ARemoteAddress: string; AEditTemplateOptions: TClkEditTemplateOptions; ACallAppProcMsg: Boolean = True): string;

procedure GetListOfUsedFilesFromLoadedTemplate(var AClkActions: TClkActionsRecArr; AListOfFiles: TStringList);
function SendMissingFilesToServer(ARemoteAddress: string; var AClkActions: TClkActionsRecArr): string;
function SetClientTemplateInServer(ARemoteAddress, AFileName: string; var AClkActions: TClkActionsRecArr; AStackLevel: Integer; ASendFileOnly: Boolean = False): string;


var
  GeneralConnectTimeout: Integer;  //using a global var for this timeout, instead of passing it through all functions
  GeneralClosingApp: Boolean;      //same as GeneralConnectTimeout. This var is set by a form, on destroy.

implementation


uses
  IdHTTP, Forms, ClickerTemplates, InMemFileSystem, ClickerActionProperties,
  ClickerExtraUtils;


{TClientThread}


procedure TClientThread.Execute;
var
  IdHTTPClient: TIdHTTP;
begin
  try
    IdHTTPClient := TIdHTTP.Create;   //an already created object should be used for "KeepAlive" connections
    try
      IdHTTPClient.ConnectTimeout := FConnectTimeout;
      IdHTTPClient.ReadTimeout := 3600000; //40000;  //1h instead of 40s, to allow debugging or other long Find(Sub)Control waits
      IdHTTPClient.UseNagle := False;

      if FStreamForServer <> nil then
      begin
        FStreamForServer.Position := 0;
        IdHTTPClient.Put(FLink, FStreamForServer, FResponseStream);
        FResult := '';
      end
      else
      begin
        if FResponseStream = nil then
          FResult := IdHTTPClient.Get(FLink)
        else
          IdHTTPClient.Get(FLink, FResponseStream);
      end;
    finally
      IdHTTPClient.Free;
    end;
  except
    on E: Exception do
      FResult := CClientExceptionPrefix + E.Message;
  end;

  FDone := True;
end;


constructor TClientThread.Create(CreateSuspended: Boolean {$IFDEF FPC}; const StackSize: SizeUInt = DefaultStackSize {$ENDIF});
begin
  inherited Create(CreateSuspended, StackSize);
  FDone := False;
  FResult := '';
  FStreamForServer := nil;
  FResponseStream := nil;
end;


//Set ACallAppProcMsg to False when calling from a different thread
function WaitForServerResponse(ATh: TClientThread; ACallAppProcMsg: Boolean = True): Boolean;
var
  tk: QWord;
begin
  tk := GetTickCount64;
  repeat
    if GeneralClosingApp then
      Break;

    if ACallAppProcMsg then
      Application.ProcessMessages;

    if GeneralClosingApp then
      Break;

    Sleep(2);
  until ATh.FDone or (GetTickCount64 - tk >= 3600000); //1h   a big timeout is required, because of processing stacks

  Result := ATh.FDone;
end;


function SendTextRequestToServer(AFullLink: string; ACallAppProcMsg: Boolean = True): string;
var
  Th: TClientThread;
begin
  Th := TClientThread.Create(True);   //without using thread, the client blocks both this UI and the server's UI, because it doesn't read  - some sort of deadlock
  try
    Th.FLink := AFullLink;
    Th.FConnectTimeout := GeneralConnectTimeout;

    Th.Start;

    WaitForServerResponse(Th, ACallAppProcMsg);
    Result := Th.FResult;
  finally
    Th.Free;
  end;
end;


function SendGetFileRequestToServer(AFullLink: string; AStream: TMemoryStream): string;
var
  Th: TClientThread;
begin
  Th := TClientThread.Create(True);   //without using thread, the client blocks both this UI and the server's UI, because it doesn't read  - some sort of deadlock
  try
    Th.FLink := AFullLink;
    Th.FConnectTimeout := GeneralConnectTimeout;

    Th.FResponseStream := AStream;
    Th.Start;

    WaitForServerResponse(Th);
    Result := Th.FResult; //IntToStr(Stream.Size); //if debugging, and returning Stream.Size, make sure this result is not used to test for an error message

    AStream.Position := 0;
  finally
    Th.Free;
  end;
end;


function SendGetBmpRequestToServer(AFullLink: string; ABmp: TBitmap): string;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Result := SendGetFileRequestToServer(AFullLink, Stream);
    Stream.Position := 0;
    ABmp.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;


//Returns client-side errors in Result and server-side errors in AResponseStream.
function SendFileToServer(AFullLink: string; AFileContent, AResponseStream: TMemoryStream; ACallAppProcMsg: Boolean = True): string; overload;
var
  Th: TClientThread;
begin
  Th := TClientThread.Create(True);   //without using thread, the client blocks both this UI and the server's UI, because it doesn't read  - some sort of deadlock
  try
    Th.FLink := AFullLink;
    Th.FConnectTimeout := GeneralConnectTimeout;

    Th.FStreamForServer := AFileContent;
    Th.FResponseStream := AResponseStream;
    Th.Start;

    WaitForServerResponse(Th, ACallAppProcMsg);
    Result := Th.FResult; //IntToStr(Stream.Size); //if debugging, and returning Stream.Size, make sure this result is not used to test for an error message
  finally
    Th.Free;
  end;
end;


//wrapper over the other SendFileToServer function, which returns all types of errors in result
function SendFileToServer(AFullLink: string; AFileContent: TMemoryStream; ACallAppProcMsg: Boolean = True): string; overload;
var
  ResponseStream: TMemoryStream;
begin
  ResponseStream := TMemoryStream.Create;
  try
    Result := SendFileToServer(AFullLink, AFileContent, ResponseStream, ACallAppProcMsg);

    if Result = '' then
    begin
      ResponseStream.Position := 0;
      SetLength(Result, ResponseStream.Size);
      ResponseStream.Read(Result[1], ResponseStream.Size);
    end;
  finally
    ResponseStream.Free;
  end;
end;





function TestConnection(ARemoteAddress: string; ACallAppProcMsg: Boolean = True): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_TestConnection, ACallAppProcMsg);
end;


function StopRemoteTemplateExecution(ARemoteAddress: string; AStackLevel: Integer; ACallAppProcMsg: Boolean = True): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_StopTemplateExecution + '?' +
                                    CREParam_StackLevel + '=' + IntToStr(AStackLevel),
                                    ACallAppProcMsg);
end;


//used while debugging, to close templates which open automatically
function ExitRemoteTemplate(ARemoteAddress: string; AStackLevel: Integer): string;  //called by client, to send a request to server to close a tab
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_ExitTemplate + '?' +
                                    CREParam_StackLevel + '=' + IntToStr(AStackLevel));
end;


function GetAllReplacementVars(ARemoteAddress: string; AStackLevel: Integer): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_GetAllReplacementVars + '?' +
                                    CREParam_StackLevel + '=' + IntToStr(AStackLevel));
end;


function GetDebugImageFromServer(ARemoteAddress: string; AStackLevel: Integer; AReceivedBmp: TBitmap; AWithGrid: Boolean): string; //returns error message if any
var
  Link: string;
begin
  Link := ARemoteAddress + CRECmd_GetResultedDebugImage + '?' +
                           CREParam_StackLevel + '=' + IntToStr(AStackLevel) + '&' +
                           CREParam_Grid + '=' + IntToStr(Ord(AWithGrid));

  Result := SendGetBmpRequestToServer(Link, AReceivedBmp);
end;


function GetDebugImageFromServerAsStream(ARemoteAddress: string; AStackLevel: Integer; AReceivedStream: TMemoryStream; AWithGrid: Boolean): string; //returns error message if any
var
  Link: string;
begin
  Link := ARemoteAddress + CRECmd_GetResultedDebugImage + '?' +
                           CREParam_StackLevel + '=' + IntToStr(AStackLevel) + '&' +
                           CREParam_Grid + '=' + IntToStr(Ord(AWithGrid));

  Result := SendGetFileRequestToServer(Link, AReceivedStream);
end;


function GetSearchAreaDebugImageFromServer(ARemoteAddress: string; AStackLevel: Integer; AReceivedBmp: TBitmap): string; //returns error message if any
var
  Link: string;
begin
  Link := ARemoteAddress + CRECmd_GetSearchAreaDebugImage + '?' +
                           CREParam_StackLevel + '=' + IntToStr(AStackLevel);

  Result := SendGetBmpRequestToServer(Link, AReceivedBmp);
end;


function SendTemplateContentToServer(ARemoteAddress, AFileName: string; var ACustomClkActions: TClkActionsRecArr): string;
var
  Link: string;
  FileName: string;
  FileContentMem: TMemoryStream;
begin
  if AFileName = '' then
    FileName := CDefaultNoTemplate
  else
    FileName := AFileName;  //use the currently loaded FileName

  Link := ARemoteAddress + CRECmd_SendFileToServer + '?' +
                           CREParam_FileName + '=' + FileName;
                           //maybe, add here a parameter to decide if the file should be saved to disk (on server side)

  FileContentMem := TMemoryStream.Create;
  try
    GetTemplateContentAsMemoryStream(ACustomClkActions, '', '', FileContentMem);
    Result := SendFileToServer(Link, FileContentMem);
  finally
    FileContentMem.Free;
  end;
end;


function SendLoadTemplateInExecListRequest(ARemoteAddress, AFileName: string; AStackLevel: Integer): string;
var
  Link: string;
  FileName: string;
begin
  if AFileName = '' then
    FileName := CDefaultNoTemplate
  else
    FileName := AFileName;  //use the currently loaded FileName

  Link := ARemoteAddress + CRECmd_LoadTemplateInExecList + '?' +
                           CREParam_StackLevel + '=' + IntToStr(AStackLevel) + '&' +
                           CREParam_FileName + '=' + FileName;

  Result := SendTextRequestToServer(Link);
end;


function GetServerFileExpectancy(ARemoteAddress: string): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_GetFileExpectancy + '?' + CREParam_StackLevel + '=0'); //stack level is needed for protocol only
end;


function GetFileExistenceOnServer(ARemoteAddress: string; AListOfFiles, AListOfResults: TStringList; AListOfFilesIncludesHashes: Boolean; ADebugParam: string = ''): string;
var
  Link: string;
  MemStream: TMemoryStream;
  RespStream: TMemoryStream;
  TempListOfFileResults: string;
begin
  Link := ARemoteAddress + CRECmd_GetFileExistenceOnServer;
  Link := Link + '?' + CREParam_VerifyHashes + '=' + IntToStr(Ord(AListOfFilesIncludesHashes)) +
                 '&' + CREParam_DebugParam + '=' + ADebugParam;

  MemStream := TMemoryStream.Create;
  RespStream := TMemoryStream.Create;
  try
    AListOfFiles.SaveToStream(MemStream);
    Result := SendFileToServer(Link, MemStream, RespStream);

    if Result = '' then   //no errors on client side, so return content
    begin
      RespStream.Position := 0;
      SetLength(TempListOfFileResults, RespStream.Size);
      RespStream.Read(TempListOfFileResults[1], RespStream.Size);

      AListOfResults.Text := TempListOfFileResults;
    end;
  finally
    MemStream.Free;
    RespStream.Free;
  end;
end;


function GetFileExistenceOnServer(ARemoteAddress: string; AFileName: string; AFileIncludesHash: Boolean; ADebugParam: string = ''): Boolean; overload;
var
  ListOfFiles, ListOfResults: TStringList;
begin
  ListOfFiles := TStringList.Create;
  ListOfResults := TStringList.Create;
  try
    ListOfFiles.Add(AFileName);

    Result := GetFileExistenceOnServer(ARemoteAddress, ListOfFiles, ListOfResults, AFileIncludesHash, 'OneFile_' + ADebugParam) = '';
    Result := Result and (ListOfResults.Count > 0) and (ListOfResults.Strings[0] = '1');
  finally
    ListOfFiles.Free;
    ListOfResults.Free;
  end;
end;


function GetScreenShotImageFromServer(ARemoteAddress: string; AReceivedBmp: TBitmap): string; //returns error message if any
var
  Link: string;
begin
  Link := ARemoteAddress + CRECmd_GetScreenShotImage + '?' + CREParam_StackLevel + '=0'; //stack level is needed for protocol only

  Result := SendGetBmpRequestToServer(Link, AReceivedBmp);
end;


function GetCurrentlyRecordedScreenShotImageFromServer(ARemoteAddress: string; AReceivedBmp: TBitmap): string; //returns error message if any
var
  Link: string;
begin
  Link := ARemoteAddress + CRECmd_GetCurrentlyRecordedScreenShotImage + '?' + CREParam_StackLevel + '=0'; //stack level is needed for protocol only

  Result := SendGetBmpRequestToServer(Link, AReceivedBmp);
end;


function GetCompInfoAtPoint(ARemoteAddress: string; X, Y: Integer): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_GetCompInfoAtPoint + '?' +
                                    CREParam_X + '=' + IntToStr(X) + '&' +
                                    CREParam_Y + '=' + IntToStr(Y));
end;


function RecordComponentOnServer(ARemoteAddress: string; AHandle: THandle; AComponentContent: TMemoryStream): string;
begin
  Result := SendGetFileRequestToServer(ARemoteAddress + CRECmd_RecordComponent + '?' +
                                       CREParam_StackLevel + '=0' + '&' +
                                       CREParam_Handle + '=' + IntToStr(AHandle),
                                       AComponentContent);
end;


function ClearInMemFileSystem(ARemoteAddress: string): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_ClearInMemFileSystem + '?' +
                                    CREParam_StackLevel + '=0');
end;


function SetVariable(ARemoteAddress, AVarName, AVarValue: string; AStackLevel: Integer): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_SetVariable + '?' +
                                    CREParam_StackLevel + '=' + IntToStr(AStackLevel) + '&' +
                                    CREParam_Var + '=' + AVarName + '&' +
                                    CREParam_Value + '=' + AVarValue);
end;


function TerminateWaitingForFileAvailability(ARemoteAddress, ALoopType: string; AStackLevel: Integer; ACallAppProcMsg: Boolean = True): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_TerminateWaitingForFileAvailability + '?' +
                                    CREParam_StackLevel + '=' + IntToStr(AStackLevel) + '&' +
                                    CREParam_TerminateWaitingLoop + '=' + ALoopType,
                                    ACallAppProcMsg);
end;


function GetListOfRenderedFilesFromServer(ARemoteAddress: string; ACallAppProcMsg: Boolean = True): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_GetListOfRenderedFiles,
                                    ACallAppProcMsg);
end;


function GetRenderedFileFromServer(ARemoteAddress: string; AFileName: string; AReceivedBmp: TBitmap): string; //Returns error message if any. Returns a bitmap with error as text if file not found.
var
  Link: string;
begin
  Link := ARemoteAddress + CRECmd_GetRenderedFile + '?' +
                           CREParam_FileName + '=' + AFileName;

  Result := SendGetBmpRequestToServer(Link, AReceivedBmp);
end;


function SendMouseDown(ARemoteAddress: string; AMouseParams: TStringList): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_MouseDown + '?' +
                                    CREParam_StackLevel + '=0' + '&' +
                                    StringReplace(AMouseParams.Text, #13#10, '&', [rfReplaceAll]));
end;


function SendMouseUp(ARemoteAddress: string; AMouseParams: TStringList): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_MouseUp + '?' +
                                    CREParam_StackLevel + '=0' + '&' +
                                    StringReplace(AMouseParams.Text, #13#10, '&', [rfReplaceAll]));
end;


function SendPluginCmd(ARemoteAddress: string; APluginCmd: string; AStackLevel: Integer; ACallAppProcMsg: Boolean = True): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_PluginCmd + '?' +
                                    CREParam_StackLevel + '=' + IntToStr(AStackLevel) + '&' +
                                    CREParam_Cmd + '=' + APluginCmd);
end;


function ExecuteClickAction(ARemoteAddress: string; AClickOptions: TClkClickOptions; ACallAppProcMsg: Boolean = True): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_ExecuteClickAction + '?' +
                                    CREParam_StackLevel + '=0' + '&' +   //use the main editor
                                    GetClickActionProperties(AClickOptions),
                                    ACallAppProcMsg
                                    );
end;


function ExecuteExecAppAction(ARemoteAddress: string; AExecAppOptions: TClkExecAppOptions; AActionName: string; AActionTimeout: Integer; ACallAppProcMsg: Boolean = True): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_ExecuteExecAppAction + '?' +
                                    CREParam_StackLevel + '=0' + '&' +   //use the main editor
                                    GetExecAppActionProperties(AExecAppOptions) + '&' +

                                    CPropertyName_ActionName + '=' + AActionName + '&' +
                                    CPropertyName_ActionTimeout + '=' + IntToStr(AActionTimeout),
                                    ACallAppProcMsg
                                    );
end;


function ExecuteGenericFindControlAction(ARemoteAddress: string; AFindControlOptions: TClkFindControlOptions; AActionName: string; AActionTimeout: Integer; AFileLocation: string; AActionType: string; ACallAppProcMsg: Boolean = True): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + AActionType + '?' +
                                    CREParam_StackLevel + '=0' + '&' +   //use the main editor
                                    GetFindControlActionProperties(AFindControlOptions) + '&' +

                                    CPropertyName_ActionName + '=' + AActionName + '&' +
                                    CPropertyName_ActionTimeout + '=' + IntToStr(AActionTimeout) + '&' +
                                    CREParam_FileLocation + '=' + AFileLocation,
                                    ACallAppProcMsg
                                    );
end;


function ExecuteFindControlAction(ARemoteAddress: string; AFindControlOptions: TClkFindControlOptions; AActionName: string; AActionTimeout: Integer; AFileLocation: string; ACallAppProcMsg: Boolean = True): string;
begin
  Result := ExecuteGenericFindControlAction(ARemoteAddress, AFindControlOptions, AActionName, AActionTimeout, AFileLocation, CRECmd_ExecuteFindControlAction, ACallAppProcMsg);
end;


function ExecuteFindSubControlAction(ARemoteAddress: string; AFindControlOptions: TClkFindControlOptions; AActionName: string; AActionTimeout: Integer; AFileLocation: string; ACallAppProcMsg: Boolean = True): string;
begin
  Result := ExecuteGenericFindControlAction(ARemoteAddress, AFindControlOptions, AActionName, AActionTimeout, AFileLocation, CRECmd_ExecuteFindSubControlAction, ACallAppProcMsg);
end;


function ExecuteSetControlTextAction(ARemoteAddress: string; ASetTextOptions: TClkSetTextOptions; ACallAppProcMsg: Boolean = True): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_ExecuteSetControlTextAction + '?' +
                                    CREParam_StackLevel + '=0' + '&' +   //use the main editor
                                    GetSetControlTextActionProperties(ASetTextOptions),
                                    ACallAppProcMsg
                                    );
end;


function ExecuteCallTemplateAction(ARemoteAddress: string; ACallTemplateOptions: TClkCallTemplateOptions; AIsDebugging, AUseLocalDebugger: Boolean; AFileLocation: string; ACallAppProcMsg: Boolean = True): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_ExecuteCallTemplateAction + '?' +
                                    CREParam_StackLevel + '=0' + '&' +   //use the main editor
                                    GetCallTemplateActionProperties(ACallTemplateOptions) + '&' +

                                    CREParam_IsDebugging + '=' + IntToStr(Ord(AIsDebugging)) + '&' +
                                    CREParam_FileLocation + '=' + AFileLocation + '&' +
                                    CREParam_UseLocalDebugger + '=' + IntToStr(Ord(AUseLocalDebugger)),

                                    ACallAppProcMsg
                                    );
end;


function ExecuteSleepAction(ARemoteAddress: string; ASleepOptions: TClkSleepOptions; AActionName: string; ACallAppProcMsg: Boolean = True): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_ExecuteSleepAction + '?' +
                                    CREParam_StackLevel + '=0' + '&' +   //use the main editor
                                    GetSleepActionProperties(ASleepOptions) + '&' +
                                    CPropertyName_ActionName + '=' + AActionName,
                                    ACallAppProcMsg
                                    );
end;


function ExecuteSetVarAction(ARemoteAddress: string; ASetVarOptions: TClkSetVarOptions; ACallAppProcMsg: Boolean = True): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_ExecuteSetVarAction + '?' +
                                    CREParam_StackLevel + '=0' + '&' +   //use the main editor
                                    GetSetVarActionProperties(ASetVarOptions),
                                    ACallAppProcMsg
                                    );
end;


function ExecuteWindowOperationsAction(ARemoteAddress: string; AWindowOperationsOptions: TClkWindowOperationsOptions; ACallAppProcMsg: Boolean = True): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_ExecuteWindowOperationsAction + '?' +
                                    CREParam_StackLevel + '=0' + '&' +   //use the main editor
                                    GetWindowOperationsActionProperties(AWindowOperationsOptions),
                                    ACallAppProcMsg
                                    );
end;


function ExecuteLoadSetVarFromFileAction(ARemoteAddress: string; ALoadSetVarFromFileOptions: TClkLoadSetVarFromFileOptions; ACallAppProcMsg: Boolean = True): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_ExecuteLoadSetVarFromFile + '?' +
                                    CREParam_StackLevel + '=0' + '&' +   //use the main editor
                                    GetLoadSetVarFromFileActionProperties(ALoadSetVarFromFileOptions),
                                    ACallAppProcMsg
                                    );
end;


function ExecuteSaveSetVarToFileAction(ARemoteAddress: string; ASaveSetVarToFileOptions: TClkSaveSetVarToFileOptions; ACallAppProcMsg: Boolean = True): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_ExecuteSaveSetVarToFile + '?' +
                                    CREParam_StackLevel + '=0' + '&' +   //use the main editor
                                    GetSaveSetVarToFileActionProperties(ASaveSetVarToFileOptions),
                                    ACallAppProcMsg
                                    );
end;


function ExecutePluginAction(ARemoteAddress: string; APluginOptions: TClkPluginOptions; ACallAppProcMsg: Boolean = True): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_ExecutePlugin + '?' +
                                    CREParam_StackLevel + '=0' + '&' +   //use the main editor
                                    GetPluginActionProperties(APluginOptions),
                                    ACallAppProcMsg
                                    );
end;


function ExecuteEditTemplateAction(ARemoteAddress: string; AEditTemplateOptions: TClkEditTemplateOptions; ACallAppProcMsg: Boolean = True): string;
begin
  Result := SendTextRequestToServer(ARemoteAddress + CRECmd_ExecuteEditTemplate + '?' +
                                    CREParam_StackLevel + '=0' + '&' +   //use the main editor
                                    GetEditTemplateActionProperties(AEditTemplateOptions, True),
                                    ACallAppProcMsg
                                    );
end;


//==============================================================================
//list of bmp files for now, but it can include all other files, which have to be sent to server
procedure GetListOfUsedFilesFromLoadedTemplate(var AClkActions: TClkActionsRecArr; AListOfFiles: TStringList);
var
  i, j: Integer;
  TempStringList: TStringList;
begin
  for i := 0 to Length(AClkActions) - 1 do
    if AClkActions[i].ActionOptions.Action in [acFindControl, acFindSubControl] then
    begin
      TempStringList := TStringList.Create;
      try
        TempStringList.Text := AClkActions[i].FindControlOptions.MatchBitmapFiles;

        for j := 0 to TempStringList.Count - 1 do
          AListOfFiles.Add(TempStringList.Strings[j]);
      finally
        TempStringList.Free;
      end;
    end;
end;


procedure AddHashesToFileNames(AListOfFiles, AListOfFilesWithHashes: TStringList);
var
  i: Integer;
  Fnm, Hash: string;
begin
  for i := 0 to AListOfFiles.Count - 1 do
  begin
    Fnm := AListOfFiles.Strings[i];

    if FileExists(Fnm) then
      Hash := GetFileHash(Fnm)
    else
      Hash := '';

    AListOfFilesWithHashes.Add(AListOfFiles.Strings[i] + CDefaultInMemFileNameHashSeparator + Hash);
  end;
end;


//Sends this template and the bitmaps it might use.
function SendMissingFilesToServer(ARemoteAddress: string; var AClkActions: TClkActionsRecArr): string;
var
  ListOfUsedFiles, ListOfUsedFilesWithHashes, FileExistenceOnServer: TStringList;
  Link: string;
  i: Integer;
  FileContent: TMemoryStream;
begin
  ListOfUsedFiles := TStringList.Create;
  ListOfUsedFilesWithHashes := TStringList.Create;
  FileExistenceOnServer := TStringList.Create;
  try
    GetListOfUsedFilesFromLoadedTemplate(AClkActions, ListOfUsedFiles);
    AddHashesToFileNames(ListOfUsedFiles, ListOfUsedFilesWithHashes);

    Result := GetFileExistenceOnServer(ARemoteAddress, ListOfUsedFiles, FileExistenceOnServer, True, 'SendMissingFilesToServer with used files.');
    if Result = '' then
      for i := 0 to FileExistenceOnServer.Count - 1 do
        if FileExistenceOnServer[i] = '0' then
        begin
          Link := ARemoteAddress + CRECmd_SendFileToServer + '?' +
                                   CREParam_FileName + '=' + ListOfUsedFiles.Strings[i];

          FileContent := TMemoryStream.Create;
          try
            if FileExists(ListOfUsedFiles.Strings[i]) then
            begin
              FileContent.LoadFromFile(ListOfUsedFiles.Strings[i]);
              FileContent.Position := 0;
              Result := Result + 'Sending file: ' + ListOfUsedFiles.Strings[i] + ' -> ' +
                                 SendFileToServer(Link, FileContent) + #13#10;
            end
            else
              Result := Result + 'Cannot send non-existent file: ' + ListOfUsedFiles.Strings[i];
          finally
            FileContent.Free;
          end;
        end;
  finally
    ListOfUsedFiles.Free;
    ListOfUsedFilesWithHashes.Free;
    FileExistenceOnServer.Free;
  end;
end;


function SetClientTemplateInServer(ARemoteAddress, AFileName: string; var AClkActions: TClkActionsRecArr; AStackLevel: Integer; ASendFileOnly: Boolean = False): string;
var
  Hash: string;
  FileContentMem: TMemoryStream;
  FileNameWithHash: string;
begin
  Result := '';

  FileContentMem := TMemoryStream.Create;
  try
    GetTemplateContentAsMemoryStream(AClkActions, '', '', FileContentMem);
    Hash := ComputeHash(FileContentMem, FileContentMem.Size);
  finally
    FileContentMem.Free;
  end;

  FileNameWithHash := AFileName + CDefaultInMemFileNameHashSeparator + Hash;
  if not GetFileExistenceOnServer(ARemoteAddress, FileNameWithHash, True, 'SetClientTemplateInServer') then
  begin
    Result := SendTemplateContentToServer(ARemoteAddress, AFileName, AClkActions); //////////////// GetTemplateContentAsMemoryStream is called again in here
    if Pos(CREResp_ReceivedFile, Result) = 0 then
      Exit;
  end;

  if ASendFileOnly then
    Exit;

  Result := SendLoadTemplateInExecListRequest(ARemoteAddress, AFileName, AStackLevel);  //should return CREResp_TemplateLoaded for success
end;


initialization
  GeneralConnectTimeout := 1000;
  GeneralClosingApp := False;

end.

