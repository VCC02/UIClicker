{
    Copyright (C) 2022 VCC
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
  Classes, SysUtils;


type
  TPollForMissingServerFilesProcessingEvent = procedure of object;

  TPollForMissingServerFiles = class(TThread)
  private
    FRemoteAddress: string;
    FDone: Boolean;
    FConnectTimeout: Integer;
    FListOfAccessibleDirs: TStringList;
    FListOfAccessibleFileExtensions: TStringList;

    FOnBeforeRequestingListOfMissingFiles: TPollForMissingServerFilesProcessingEvent;
    FOnAfterRequestingListOfMissingFiles: TPollForMissingServerFilesProcessingEvent;

    procedure DoOnBeforeRequestingListOfMissingFiles;
    procedure DoOnAfterRequestingListOfMissingFiles;

    function FileIsAllowed(AFileName: string): Boolean;
    procedure RemoveDeniedFilesFromList(AList: TStringList);
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean {$IFDEF FPC}; const StackSize: SizeUInt = DefaultStackSize {$ENDIF});
    destructor Destroy; override;

    procedure AddListOfAccessibleDirs(AList: TStrings);
    procedure AddListOfAccessibleFileExtensions(AList: TStrings);

    property RemoteAddress: string read FRemoteAddress write FRemoteAddress;
    property Done: Boolean read FDone;
    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout;

    property OnBeforeRequestingListOfMissingFiles: TPollForMissingServerFilesProcessingEvent read FOnBeforeRequestingListOfMissingFiles write FOnBeforeRequestingListOfMissingFiles;
    property OnAfterRequestingListOfMissingFiles: TPollForMissingServerFilesProcessingEvent read FOnAfterRequestingListOfMissingFiles write FOnAfterRequestingListOfMissingFiles;
  end;


const
  CFileProviderIterationInterval = 200; //[ms]


implementation


uses
  IdHTTP, ClickerUtils, ClickerActionsClient;


{TPollForMissingServerFiles}

constructor TPollForMissingServerFiles.Create(CreateSuspended: Boolean {$IFDEF FPC}; const StackSize: SizeUInt = DefaultStackSize {$ENDIF});
begin
  inherited Create(CreateSuspended, StackSize);
  FListOfAccessibleDirs := TStringList.Create;
  FListOfAccessibleFileExtensions := TStringList.Create;
  FDone := False;
end;


destructor TPollForMissingServerFiles.Destroy;
begin
  FreeAndNil(FListOfAccessibleDirs);
  FreeAndNil(FListOfAccessibleFileExtensions);
  inherited Destroy;
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


function TPollForMissingServerFiles.FileIsAllowed(AFileName: string): Boolean;
var
  FileExt, FilePath: string;
  i: Integer;
  FoundExt, FoundDir: Boolean;
  CurrentItem: string;
begin
  Result := False;
  AFileName := UpperCase(AFileName);
  FileExt := ExtractFileExt(AFileName);

  FoundExt := False;
  for i := 0 to FListOfAccessibleFileExtensions.Count - 1 do
  begin
    CurrentItem := FListOfAccessibleFileExtensions.Strings[i];
    if CurrentItem = FileExt then
    begin
      FoundExt := True;
      Break;
    end;
  end;

  if not FoundExt then
    Exit;

  FoundDir := False;
  FilePath := ExtractFilePath(AFileName);
  for i := 0 to FListOfAccessibleDirs.Count - 1 do
  begin
    CurrentItem := FListOfAccessibleDirs.Strings[i];
    if (CurrentItem > '') and (CurrentItem[Length(CurrentItem)] <> PathDelim) then
      CurrentItem := CurrentItem + PathDelim;  //make sure there are consistent results, regardless of the last '\' existence

    if (Pos(CurrentItem, FilePath) = 1) and (Pos('..', CurrentItem) = 0) then
    begin
      FoundDir := True;
      Break;
    end;
  end;

  if not FoundDir then
    Exit;

  Result := True;
end;


procedure TPollForMissingServerFiles.AddListOfAccessibleDirs(AList: TStrings);
var
  i: Integer;
begin
  for i := 0 to AList.Count - 1 do
    FListOfAccessibleDirs.Add(UpperCase(AList.Strings[i]));
end;


procedure TPollForMissingServerFiles.AddListOfAccessibleFileExtensions(AList: TStrings);
var
  i: Integer;
begin
  for i := 0 to AList.Count - 1 do
    FListOfAccessibleFileExtensions.Add(UpperCase(AList.Strings[i]));
end;


procedure TPollForMissingServerFiles.RemoveDeniedFilesFromList(AList: TStringList);
var
  i: Integer;
begin
  for i := AList.Count - 1 downto 0 do
    if not FileIsAllowed(AList.Strings[i]) then
       AList.Delete(i);
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
            ListStr := IdHTTPClient.Get(FRemoteAddress + CRECmd_GetListOfWaitingFiles);  //file names, read from FIFO
            ListOfFiles.Text := FastReplace_87ToReturn(ListStr);

            //Restrict the files to one or more directories and some specific file types, so the server can't ask for every possible file
            RemoveDeniedFilesFromList(ListOfFiles);

            for i := 0 to ListOfFiles.Count - 1 do
              if FileExists(ListOfFiles.Strings[i]) then
              begin
                try
                  FileContent := TMemoryStream.Create;
                  try
                    try
                      FileContent.LoadFromFile(ListOfFiles.Strings[i]);
                    except
                      Sleep(300); //maybe the file is in use by another thread, so wait a bit, then load again
                      FileContent.LoadFromFile(ListOfFiles.Strings[i]);
                    end;

                    LinkForSendingFiles := FRemoteAddress + CRECmd_SendFileToServer + '?' +
                                           CREParam_FileName + '=' + ListOfFiles.Strings[i];
                    SendFileToServer(LinkForSendingFiles, FileContent, False);
                  finally
                    FileContent.Free;
                  end;
                except

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

