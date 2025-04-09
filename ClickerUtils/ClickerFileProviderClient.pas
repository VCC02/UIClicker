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


unit ClickerFileProviderClient;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF Windows}
    Windows,
  {$ELSE}
    LCLIntf, LCLType,
  {$ENDIF}
  Classes, SysUtils, ClickerUtils, ClickerFileProviderUtils;


type
  TPollForMissingServerFilesProcessingEvent = procedure of object;
  TOnLogMissingServerFile = procedure(AMsg: string) of object;
  TOnLoadMissingFileContent = procedure(AFileName: string; AFileContent: TMemoryStream) of object;
  TOnDenyFile = procedure(AFileName: string) of object;


  TPollForMissingServerFiles = class(TThread)
  private
    FRemoteAddress: string;
    FDone: Boolean;
    FConnectTimeout: Integer;

    FCritSec: TRTLCriticalSection;
    FLogOutput: TStringList;
    FFileProvider: TFileProvider;

    FOnBeforeRequestingListOfMissingFiles: TPollForMissingServerFilesProcessingEvent;
    FOnAfterRequestingListOfMissingFiles: TPollForMissingServerFilesProcessingEvent;
    FOnFileExists: TOnFileExists;
    FOnLogMissingServerFile: TOnLogMissingServerFile;
    FOnLoadMissingFileContent: TOnLoadMissingFileContent;
    FOnDenyFile: TOnDenyFile;

    procedure DoOnBeforeRequestingListOfMissingFiles;
    procedure DoOnAfterRequestingListOfMissingFiles;
    function DoOnFileExists(const AFileName: string): Boolean;
    procedure DoOnLogMissingServerFile(AMsg: string);
    procedure DoOnLoadMissingFileContent(AFileName: string; AFileContent: TMemoryStream);
    procedure DoOnDenyFile(AFileName: string);

    procedure AddUniqueMessageToLog(AMsg: string);
    procedure RemoveDeniedFilesFromList(AList: TStringList);

    function GetFullTemplatesDir: string;
    procedure SetFullTemplatesDir(Value: string);

    function GetFullAppDir: string;
    procedure SetFullAppDir(Value: string);
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean {$IFDEF FPC}; const StackSize: SizeUInt = DefaultStackSize {$ENDIF});
    destructor Destroy; override;

    procedure AddListOfAccessibleDirs(AList: TStrings); overload;
    procedure AddListOfAccessibleFileExtensions(AList: TStrings); overload;
    procedure AddListOfAccessibleDirs(AListOfDirsStr: string); overload;
    procedure AddListOfAccessibleFileExtensions(AListOfExtensionsStr: string); overload;
    procedure GetLogContent(AList: TStrings);

    property RemoteAddress: string read FRemoteAddress write FRemoteAddress;
    property Done: Boolean read FDone;
    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout;
    property FullTemplatesDir: string read GetFullTemplatesDir write SetFullTemplatesDir;
    property FullAppDir: string read GetFullAppDir write SetFullAppDir;

    property OnBeforeRequestingListOfMissingFiles: TPollForMissingServerFilesProcessingEvent read FOnBeforeRequestingListOfMissingFiles write FOnBeforeRequestingListOfMissingFiles;
    property OnAfterRequestingListOfMissingFiles: TPollForMissingServerFilesProcessingEvent read FOnAfterRequestingListOfMissingFiles write FOnAfterRequestingListOfMissingFiles;
    property OnFileExists: TOnFileExists write FOnFileExists;
    property OnLogMissingServerFile: TOnLogMissingServerFile write FOnLogMissingServerFile;
    property OnLoadMissingFileContent: TOnLoadMissingFileContent write FOnLoadMissingFileContent;
    property OnDenyFile: TOnDenyFile write FOnDenyFile;
  end;


const
  CFileProviderIterationInterval = 200; //[ms]


implementation


uses
  IdHTTP, ClickerActionsClient;


{TPollForMissingServerFiles}

constructor TPollForMissingServerFiles.Create(CreateSuspended: Boolean {$IFDEF FPC}; const StackSize: SizeUInt = DefaultStackSize {$ENDIF});
begin
  inherited Create(CreateSuspended, StackSize);
  FDone := False;
  FFileProvider := TFileProvider.Create;
  FFileProvider.FullTemplatesDir := '';

  FOnBeforeRequestingListOfMissingFiles := nil;
  FOnAfterRequestingListOfMissingFiles := nil;
  FOnFileExists := nil;
  FOnLogMissingServerFile := nil;
  FOnLoadMissingFileContent := nil;
  FOnDenyFile := nil;

  {$IFDEF Windows}
    InitializeCriticalSection(FCritSec);
  {$ELSE}
    InitCriticalSection(FCritSec);
  {$ENDIF}

  FLogOutput := TStringList.Create;
  FLogOutput.LineBreak := #13#10;
end;


destructor TPollForMissingServerFiles.Destroy;
begin
  {$IFDEF Windows}
    DeleteCriticalSection(FCritSec);
  {$ELSE}
    DoneCriticalSection(FCritSec);
  {$ENDIF}

  FreeAndNil(FLogOutput);
  FFileProvider.Free;

  inherited Destroy;
end;


function TPollForMissingServerFiles.GetFullTemplatesDir: string;
begin
  Result := FFileProvider.FullTemplatesDir;
end;


procedure TPollForMissingServerFiles.SetFullTemplatesDir(Value: string);
begin
  FFileProvider.FullTemplatesDir := Value;
end;


function TPollForMissingServerFiles.GetFullAppDir: string;
begin
  Result := FFileProvider.FullAppDir;
end;


procedure TPollForMissingServerFiles.SetFullAppDir(Value: string);
begin
  FFileProvider.FullAppDir := Value;
end;


procedure TPollForMissingServerFiles.AddListOfAccessibleDirs(AList: TStrings); overload;
begin
  FFileProvider.AddListOfAccessibleDirs(AList);
end;


procedure TPollForMissingServerFiles.AddListOfAccessibleFileExtensions(AList: TStrings); overload;
begin
  FFileProvider.AddListOfAccessibleFileExtensions(AList);
end;


procedure TPollForMissingServerFiles.AddListOfAccessibleDirs(AListOfDirsStr: string); overload;
begin
  FFileProvider.AddListOfAccessibleDirs(AListOfDirsStr);
end;


procedure TPollForMissingServerFiles.AddListOfAccessibleFileExtensions(AListOfExtensionsStr: string); overload;
begin
  FFileProvider.AddListOfAccessibleFileExtensions(AListOfExtensionsStr);
end;


procedure TPollForMissingServerFiles.DoOnBeforeRequestingListOfMissingFiles;
begin
  if Assigned(FOnBeforeRequestingListOfMissingFiles) then
    FOnBeforeRequestingListOfMissingFiles();
end;


procedure TPollForMissingServerFiles.DoOnAfterRequestingListOfMissingFiles;
begin
  if Assigned(FOnAfterRequestingListOfMissingFiles) then
    FOnAfterRequestingListOfMissingFiles();
end;


function TPollForMissingServerFiles.DoOnFileExists(const AFileName: string): Boolean;
begin
  if not Assigned(FOnFileExists) then
    raise Exception.Create('OnFileExists is not assigned.')
  else
    Result := FOnFileExists(AFileName);
end;


procedure TPollForMissingServerFiles.DoOnLogMissingServerFile(AMsg: string);
begin
  if Assigned(FOnLogMissingServerFile) then
    FOnLogMissingServerFile(AMsg);    //no exception should be raised if not assigned
end;


procedure TPollForMissingServerFiles.DoOnLoadMissingFileContent(AFileName: string; AFileContent: TMemoryStream);
begin
  if not Assigned(FOnLoadMissingFileContent) then
    raise Exception.Create('OnLoadMissingFileContent is not assigned.')
  else
    FOnLoadMissingFileContent(AFileName, AFileContent);
end;


procedure TPollForMissingServerFiles.DoOnDenyFile(AFileName: string);
begin
  if not Assigned(FOnDenyFile) then
    raise Exception.Create('OnDenyFile is not assigned.')
  else
    FOnDenyFile(AFileName);
end;


procedure TPollForMissingServerFiles.AddUniqueMessageToLog(AMsg: string);
begin
  EnterCriticalSection(FCritSec);
  try
    if FLogOutput.IndexOf(AMsg) = -1 then
    begin
      FLogOutput.Add(AMsg);
      DoOnLogMissingServerFile(AMsg);
    end;
  finally
    LeaveCriticalSection(FCritSec);
  end;
end;


procedure TPollForMissingServerFiles.GetLogContent(AList: TStrings);
begin
  EnterCriticalSection(FCritSec);
  try
    AList.AddStrings(FLogOutput);
  finally
    LeaveCriticalSection(FCritSec);
  end;
end;


procedure TPollForMissingServerFiles.RemoveDeniedFilesFromList(AList: TStringList);
var
  i: Integer;
  DenyReason: string;
begin
  for i := AList.Count - 1 downto 0 do
    if not FFileProvider.FileIsAllowed(AList.Strings[i], DenyReason) then
    begin
      AddUniqueMessageToLog('Requested file "' + AList.Strings[i] + '" is not allowed to be sent to server. DenyReason: ' + DenyReason);
      DoOnDenyFile(AList.Strings[i]);
      AList.Delete(i);
    end;
end;


procedure TPollForMissingServerFiles.Execute;
var
  IdHTTPClient: TIdHTTP;
  ListOfFiles: TStringList;
  ListStr: string;
  i: Integer;
  FileContent: TMemoryStream;
  LinkForSendingFiles: string;
begin
  try
    IdHTTPClient := TIdHTTP.Create;   //an already created object should be used for "KeepAlive" connections
    try
      IdHTTPClient.ConnectTimeout := FConnectTimeout;
      IdHTTPClient.ReadTimeout := 40000;  //40s, this request should be processed with high priority, regardless of debugging
      IdHTTPClient.UseNagle := False;

      repeat
        try
          if GeneralClosingApp then
          begin
            FDone := True;
            Terminate;
            Exit;
          end;

          DoOnBeforeRequestingListOfMissingFiles;
          ListOfFiles := TStringList.Create;
          try
            ListOfFiles.LineBreak := #13#10;
            ListStr := IdHTTPClient.Get(FRemoteAddress + CRECmd_GetListOfWaitingFiles);  //file names, read from FIFO

            if ListStr <> '<HTML><BODY><B>200 OK</B></BODY></HTML>' then   //this is the standard TIdHTTPServer response, if no user response is provided
            begin
              ListOfFiles.Text := FastReplace_87ToReturn(ListStr);

              //Restrict the files to one or more directories and some specific file types, so the server can't ask for every possible file
              RemoveDeniedFilesFromList(ListOfFiles);

              for i := 0 to ListOfFiles.Count - 1 do
                if DoOnFileExists(ListOfFiles.Strings[i]) then
                begin
                  try
                    FileContent := TMemoryStream.Create;
                    try
                      DoOnLoadMissingFileContent(ListOfFiles.Strings[i], FileContent);

                      LinkForSendingFiles := FRemoteAddress + CRECmd_SendFileToServer + '?' +
                                             CREParam_FileName + '=' + ListOfFiles.Strings[i];

                      DoOnLogMissingServerFile('Sending "' + ListOfFiles.Strings[i] + '"');
                      SendFileToServer(LinkForSendingFiles, FileContent, False);
                    finally
                      FileContent.Free;
                    end;
                  except
                    on E: Exception do
                      DoOnLogMissingServerFile('File provider: ' + E.Message);
                  end;
                end
                else
                begin
                  DoOnLogMissingServerFile('File provider: File not found: "' + ListOfFiles.Strings[i] + '"');
                  DoOnLogMissingServerFile('File provider: AppDir: "' + FFileProvider.FullAppDir + '"');
                end;
            end;
          finally
            DoOnAfterRequestingListOfMissingFiles;
            ListOfFiles.Free;
          end;
        except
        end;

        Sleep(CFileProviderIterationInterval);
      until Terminated;

      FDone := True;
    finally
      IdHTTPClient.Free;
    end;
  except
  end;
end;

end.

