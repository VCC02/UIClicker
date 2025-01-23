{
    Copyright (C) 2025 VCC
    creation date: 09 Jan 2025
    initial release date: 14 Jan 2025

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


unit ClickerPluginArchive;

{$mode Delphi}

interface

uses
  Classes, SysUtils, MemArchive, InMemFileSystem, ClickerUtils, ClickerActionPlugins,
  DCPmd5;


type
  TDecDecHashPluginInMemFS = record
    Name: string;
    InMemFS: TInMemFileSystem;
    PreventReusingInMemFS: Boolean; //If true, new archives with the same name are not using this FS
    IsDecDecHashPlugin: Boolean;
  end;

  TDecDecHashPluginInMemFSArr = array of TDecDecHashPluginInMemFS;
  PDecDecHashPluginInMemFSArr = ^TDecDecHashPluginInMemFSArr;

  TClickerPluginArchive = class
  private
    FOnAddToLog: TOnAddToLog;
    FOnSetVar: TOnSetVar;

    FArchive: TMemArchive;
    FPluginsInMemFS: TInMemFileSystem;
    FDecDecHashPluginInMemFSArr: PDecDecHashPluginInMemFSArr;

    FDecryptingStream: TMemoryStream; //set during decryption
    FDecompressingStream: TMemoryStream; //set during decompressing
    FHashingResult: Pointer; //set during hashing

    FDecryptionArchiveName: string;
    FDecompressionArchiveName: string;
    FHashingArchiveName: string;

    FDecryptionPluginName: string;
    FDecompressionPluginName: string;
    FHashingPluginName: string;

    procedure DoOnAddToLog(s: string);
    procedure DoOnSetVar(AVarName, AVarValue: string);

    procedure HandleOnActionPlugin_SetTemplateVar(AVarName, AVarValue: Pointer); cdecl;
    procedure HandleOnActionPlugin_AddToLog(ALogMsg: Pointer); cdecl;

    function HandleOnActionPlugin_DecDecHashInMemFS(AArchiveName: string; ACallbackIndex: Integer; AInData1, AInData2: Pointer; AInDataLen1, AInDataLen2: Int64; AOnFileContent: TOnFileContentObj): Int64; cdecl;
    function HandleOnActionPlugin_DecryptInMemFS(ACallbackIndex: Integer; AInData1, AInData2: Pointer; AInDataLen1, AInDataLen2: Int64; AOnFileContent: TOnFileContentObj): Int64; cdecl;
    function HandleOnActionPlugin_DecompressInMemFS(ACallbackIndex: Integer; AInData1, AInData2: Pointer; AInDataLen1, AInDataLen2: Int64; AOnFileContent: TOnFileContentObj): Int64; cdecl;
    function HandleOnActionPlugin_HashInMemFS(ACallbackIndex: Integer; AInData1, AInData2: Pointer; AInDataLen1, AInDataLen2: Int64; AOnFileContent: TOnFileContentObj): Int64; cdecl;

    procedure HandleOnFileContent_Decrypt(AStreamContent: Pointer; AStreamSize: Int64); cdecl;   //AStreamContent points to the decrypted stream
    procedure HandleOnFileContent_Decompress(AStreamContent: Pointer; AStreamSize: Int64); cdecl;   //AStreamContent points to the decrypted stream
    procedure HandleOnFileContent_Hashing(AStreamContent: Pointer; AStreamSize: Int64); cdecl;   //AStreamContent points to the hashing result

    procedure HandleOnInitEncryption(var AArcKey: TArr32OfByte);
    procedure HandleOnGetKeyFromPassword(APassword: string; var AArcKey: TArr32OfByte);
    procedure HandleOnDecryptArchive(AArchiveStream: TMemoryStream);

    function HandleOnDecompress(AArchiveStream, APlainStream: TMemoryStream): Boolean;
    procedure HandleOnComputeArchiveHash(AArchiveStream: Pointer; AArchiveStreamSize: Int64; var AResultedHash: TArr32OfByte; AAdditionalInfo: string = '');
  public
    constructor Create;
    destructor Destroy; override;

    //Every DecDecHash plugin (and it config files) reside(s) in its own InMemFS, which is different than PluginsInMemFS, set below.
    procedure ExtractArchive(AArchiveStream: TMemoryStream; ADecDecHashPluginInMemFSArr: PDecDecHashPluginInMemFSArr; ADecryptionPluginName, ADecompressionPluginName, AHashingPluginName: string; ACompressionLevel: Integer; AAdditionalInfo: string);

    property PluginsInMemFS: TInMemFileSystem write FPluginsInMemFS; //must be set by owner before calling ExtractArchive
    property OnAddToLog: TOnAddToLog write FOnAddToLog;
    property OnSetVar: TOnSetVar write FOnSetVar;
  end;


function GetPluginInMemFSIndex(var ADecDecHashPluginInMemFSArr: TDecDecHashPluginInMemFSArr; AName: string): Integer;


const
  CPluginPath32BitPrefix = 'i386-win32\';
  CPluginPath64BitPrefix = 'x86-win64\';


implementation


uses
  Math, DllUtils, ClickerActionPluginLoader
  {$IFDEF MemPlugins}
    , DynMemLib
  {$ENDIF}
  ;


function GetPluginInMemFSIndex(var ADecDecHashPluginInMemFSArr: TDecDecHashPluginInMemFSArr; AName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(ADecDecHashPluginInMemFSArr) - 1 do
    if ADecDecHashPluginInMemFSArr[i].Name = AName then
    begin
      Result := i;
      Break;
    end;
end;


{$IFDEF MemPlugins}
  type
    TConfigurePluginForMemoryContent_Proc = function(AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS_Obj;
                                                     AOnActionPlugin_SetTemplateVar: TOnActionPlugin_SetTemplateVar_Obj;
                                                     AOnActionPlugin_AddToLog: TOnActionPlugin_AddToLog_Obj): Boolean; cdecl;
    TProcessMemoryContent_Proc = function(AInData: Pointer; AInDataLen: Int64; AOnFileContent: TOnFileContentObj): Integer; cdecl;
{$ENDIF}


constructor TClickerPluginArchive.Create;
begin
  inherited Create;
  FArchive := TMemArchive.Create;

  FPluginsInMemFS := nil;
  FOnAddToLog := nil;
end;


destructor TClickerPluginArchive.Destroy;
begin
  inherited Destroy;
end;


procedure TClickerPluginArchive.DoOnAddToLog(s: string);
begin
  if Assigned(FOnAddToLog) then
    FOnAddToLog(s);
end;


procedure TClickerPluginArchive.DoOnSetVar(AVarName, AVarValue: string);
begin
  if Assigned(FOnSetVar) then
    FOnSetVar(AVarName, AVarValue);
end;


procedure TClickerPluginArchive.HandleOnActionPlugin_SetTemplateVar(AVarName, AVarValue: Pointer); cdecl;
var
  VarName, VarValue: string;
begin
  SetPointedContentToString(AVarName, VarName);
  SetPointedContentToString(AVarValue, VarValue);

  if Pos(#0, VarName) > 0 then
    VarName := Copy(VarName, 1, Pos(#0, VarName) - 1);

  if Pos(#0, VarValue) > 0 then
    VarValue := Copy(VarValue, 1, Pos(#0, VarValue) - 1);

  DoOnSetVar(VarName, VarValue);
end;


procedure TClickerPluginArchive.HandleOnActionPlugin_AddToLog(ALogMsg: Pointer); cdecl;
var
  s: string;
begin
  SetPointedContentToString(ALogMsg, s);
  DoOnAddToLog(s);
end;


function TClickerPluginArchive.HandleOnActionPlugin_DecDecHashInMemFS(AArchiveName: string; ACallbackIndex: Integer; AInData1, AInData2: Pointer; AInDataLen1, AInDataLen2: Int64; AOnFileContent: TOnFileContentObj): Int64; cdecl;
var
  InMemFSAccess: TPluginInMemFSAccess;
  PluginIdx: Integer;
begin
  PluginIdx := GetPluginInMemFSIndex(FDecDecHashPluginInMemFSArr^, AArchiveName);
  if PluginIdx = -1 then
    raise Exception.Create('Archive not found for plugin configuration: ' + AArchiveName);

  InMemFSAccess := TPluginInMemFSAccess.Create;
  try
    InMemFSAccess.InMemFS := FDecDecHashPluginInMemFSArr^[PluginIdx].InMemFS;
    Result := InMemFSAccess.InMemFSFunc(ACallbackIndex, AInData1, AInData2, AInDataLen1, AInDataLen2, AOnFileContent);
  finally
    InMemFSAccess.Free;
  end;
end;


function TClickerPluginArchive.HandleOnActionPlugin_DecryptInMemFS(ACallbackIndex: Integer; AInData1, AInData2: Pointer; AInDataLen1, AInDataLen2: Int64; AOnFileContent: TOnFileContentObj): Int64; cdecl;
begin
  Result := HandleOnActionPlugin_DecDecHashInMemFS(FDecryptionArchiveName, ACallbackIndex, AInData1, AInData2, AInDataLen1, AInDataLen2, AOnFileContent);
end;


function TClickerPluginArchive.HandleOnActionPlugin_DecompressInMemFS(ACallbackIndex: Integer; AInData1, AInData2: Pointer; AInDataLen1, AInDataLen2: Int64; AOnFileContent: TOnFileContentObj): Int64; cdecl;
begin
  Result := HandleOnActionPlugin_DecDecHashInMemFS(FDecompressionArchiveName, ACallbackIndex, AInData1, AInData2, AInDataLen1, AInDataLen2, AOnFileContent);
end;


function TClickerPluginArchive.HandleOnActionPlugin_HashInMemFS(ACallbackIndex: Integer; AInData1, AInData2: Pointer; AInDataLen1, AInDataLen2: Int64; AOnFileContent: TOnFileContentObj): Int64; cdecl;
begin
  Result := HandleOnActionPlugin_DecDecHashInMemFS(FHashingArchiveName, ACallbackIndex, AInData1, AInData2, AInDataLen1, AInDataLen2, AOnFileContent);
end;


procedure TClickerPluginArchive.HandleOnInitEncryption(var AArcKey: TArr32OfByte);
begin
  //nothing here
end;


procedure TClickerPluginArchive.HandleOnGetKeyFromPassword(APassword: string; var AArcKey: TArr32OfByte);
begin
  //nothing here
end;


procedure TClickerPluginArchive.HandleOnFileContent_Decrypt(AStreamContent: Pointer; AStreamSize: Int64); cdecl;   //AStreamContent points to the decrypted stream
begin
  //no need to call SetSize here
  Move(AStreamContent^, FDecryptingStream.Memory^, FDecryptingStream.Size); //use FDecryptingStream.Size here, not AStreamSize  (in case AStreamSize is not set properly)
end;


procedure TClickerPluginArchive.HandleOnDecryptArchive(AArchiveStream: TMemoryStream);
{$IFDEF MemPlugins}
  var
    PluginIdx: Integer;
    PluginInMemFS: TDecDecHashPluginInMemFS;
    MemLoader: TDynMemLib;
    ConfigurePluginForMemoryContent: TConfigurePluginForMemoryContent_Proc;
    ProcessMemoryContent: TProcessMemoryContent_Proc;
    LibStream: TMemoryStream;
{$ENDIF}
begin
  if FDecryptionPluginName = '' then
    Exit;

  if AArchiveStream.Size mod 32 <> 0 then
    raise Exception.Create('Decryption required padding.');  //For some reason, the archive is not the proper size, so it will require a larger size for decryption.

  //FDecryptionPluginName should end up here, using the following format: <ArhiveName.dllarc>|<PluginName.dll>
  FDecryptionArchiveName := Copy(FDecryptionPluginName, 1, Pos('|', FDecryptionPluginName) - 1);
  FDecryptionPluginName := Copy(FDecryptionPluginName, Pos('|', FDecryptionPluginName) + 1, MaxInt);

  {$IFDEF MemPlugins}
    PluginIdx := GetPluginInMemFSIndex(FDecDecHashPluginInMemFSArr^, FDecryptionArchiveName);
    if PluginIdx = -1 then
      raise Exception.Create('Decryption plugin not found: ' + FDecryptionPluginName);

    PluginInMemFS := FDecDecHashPluginInMemFSArr[PluginIdx];

    if not PluginInMemFS.InMemFS.FileExistsInMem(FDecryptionPluginName) then
      raise Exception.Create('Decryption plugin not found in InMemFS: ' + FDecryptionPluginName + '   Count = ' + IntToStr(PluginInMemFS.InMemFS.TotalFileCount));

    MemLoader := TDynMemLib.Create;
    try
      LibStream := TMemoryStream.Create;
      try
        LibStream.SetSize(PluginInMemFS.InMemFS.GetFileSize(FDecryptionPluginName));
        PluginInMemFS.InMemFS.LoadFileFromMem(FDecryptionPluginName, LibStream.Memory);

        MemLoader.MemLoadLibrary(LibStream.Memory);
        try
          @ConfigurePluginForMemoryContent := MemLoader.MemGetProcAddress('ConfigurePluginForMemoryContent');
          if @ConfigurePluginForMemoryContent = nil then
            raise Exception.Create('Invalid decryption plugin: ' + FDecryptionPluginName + '.  ConfigurePluginForMemoryContent function not found.');

          @ProcessMemoryContent := MemLoader.MemGetProcAddress('ProcessMemoryContent');
          if @ProcessMemoryContent = nil then
            raise Exception.Create('Invalid decryption plugin: ' + FDecryptionPluginName + '.  ProcessMemoryContent function not found.');

          DoOnAddToLog('Configuring decryption plugin..');
          try
            ConfigurePluginForMemoryContent(HandleOnActionPlugin_DecryptInMemFS, HandleOnActionPlugin_SetTemplateVar, HandleOnActionPlugin_AddToLog);
          except
            on E: Exception do
            begin
              DoOnAddToLog('Ex on configuring decryption: ' + E.Message);
              raise;
            end;
          end;

          DoOnAddToLog('Decrypting archive..');
          try
            FDecryptingStream := AArchiveStream;
            ProcessMemoryContent(AArchiveStream.Memory, AArchiveStream.Size, HandleOnFileContent_Decrypt);
          except
            on E: Exception do
            begin
              DoOnAddToLog('Ex on decrypting archive: ' + E.Message);
              raise;
            end;
          end;
          DoOnAddToLog('Done decrypting archive..');
        finally
          MemLoader.MemFreeLibrary;
        end;
      finally
        LibStream.Free;
      end;
    finally
      MemLoader.Free;
    end;
  {$ENDIF}
end;


procedure TClickerPluginArchive.HandleOnFileContent_Decompress(AStreamContent: Pointer; AStreamSize: Int64); cdecl;   //AStreamContent points to the decrypted stream
begin
  FDecompressingStream.SetSize(Min(AStreamSize, 100 * 1048576));  //this will truncate the stream if larger than 100MB
  Move(AStreamContent^, FDecompressingStream.Memory^, FDecompressingStream.Size);
end;


function TClickerPluginArchive.HandleOnDecompress(AArchiveStream, APlainStream: TMemoryStream): Boolean;  //AArchiveStream is the source, APlainStream is the destination
{$IFDEF MemPlugins}
  var
    PluginIdx: Integer;
    PluginInMemFS: TDecDecHashPluginInMemFS;
    MemLoader: TDynMemLib;
    ConfigurePluginForMemoryContent: TConfigurePluginForMemoryContent_Proc;
    ProcessMemoryContent: TProcessMemoryContent_Proc;
    LibStream: TMemoryStream;
{$ENDIF}
begin
  Result := True;
  if FDecompressionPluginName = '' then
  begin
    APlainStream.CopyFrom(AArchiveStream, AArchiveStream.Size);   //no compression
    Exit;
  end;

  //FDecompressionPluginName should end up here, using the following format: <ArhiveName.dllarc>|<PluginName.dll>
  FDecompressionArchiveName := Copy(FDecompressionPluginName, 1, Pos('|', FDecompressionPluginName) - 1);
  FDecompressionPluginName := Copy(FDecompressionPluginName, Pos('|', FDecompressionPluginName) + 1, MaxInt);

  {$IFDEF MemPlugins}
    PluginIdx := GetPluginInMemFSIndex(FDecDecHashPluginInMemFSArr^, FDecompressionArchiveName);
    if PluginIdx = -1 then
      raise Exception.Create('Decompression plugin not found: ' + FDecompressionPluginName);

    PluginInMemFS := FDecDecHashPluginInMemFSArr[PluginIdx];

    if not PluginInMemFS.InMemFS.FileExistsInMem(FDecompressionPluginName) then
      raise Exception.Create('Decompression plugin not found in InMemFS: ' + FDecompressionPluginName + '   Count = ' + IntToStr(PluginInMemFS.InMemFS.TotalFileCount));

    MemLoader := TDynMemLib.Create;
    try
      LibStream := TMemoryStream.Create;
      try
        LibStream.SetSize(PluginInMemFS.InMemFS.GetFileSize(FDecompressionPluginName));
        PluginInMemFS.InMemFS.LoadFileFromMem(FDecompressionPluginName, LibStream.Memory);

        MemLoader.MemLoadLibrary(LibStream.Memory);
        try
          @ConfigurePluginForMemoryContent := MemLoader.MemGetProcAddress('ConfigurePluginForMemoryContent');
          if @ConfigurePluginForMemoryContent = nil then
            raise Exception.Create('Invalid decompression plugin: ' + FDecryptionPluginName + '.  ConfigurePluginForMemoryContent function not found.');

          @ProcessMemoryContent := MemLoader.MemGetProcAddress('ProcessMemoryContent');
          if @ProcessMemoryContent = nil then
            raise Exception.Create('Invalid decompression plugin: ' + FDecompressionPluginName + '.  ProcessMemoryContent function not found.');

          DoOnAddToLog('Configuring decompression plugin ..');
          try
            ConfigurePluginForMemoryContent(HandleOnActionPlugin_DecompressInMemFS, HandleOnActionPlugin_SetTemplateVar, HandleOnActionPlugin_AddToLog);
          except
            on E: Exception do
            begin
              DoOnAddToLog('Ex on configuring decompression: ' + E.Message);
              raise;
            end;
          end;

          DoOnAddToLog('Decompressing archive..  Initial size = ' + IntToStr(AArchiveStream.Size));
          try
            FDecompressingStream := APlainStream;
            if ProcessMemoryContent(AArchiveStream.Memory, AArchiveStream.Size, HandleOnFileContent_Decompress) = -1 then
              raise Exception.Create('Cannot decompress archive: "' + FDecompressionArchiveName + '". The decompression may not be configured properly.');
          except
            on E: Exception do
            begin
              DoOnAddToLog('Ex on decompressing archive: ' + E.Message);
              raise;
            end;
          end;
          DoOnAddToLog('Done decompressing archive..  Decompressed size = ' + IntToStr(APlainStream.Size));
        finally
          MemLoader.MemFreeLibrary;
        end;
      finally
        LibStream.Free;
      end;
    finally
      MemLoader.Free;
    end;
  {$ENDIF}
end;


procedure TClickerPluginArchive.HandleOnFileContent_Hashing(AStreamContent: Pointer; AStreamSize: Int64); cdecl;   //AStreamContent points to the hashing result
begin
  Move(AStreamContent^, FHashingResult^, Min(32, AStreamSize));    //the number of bytes to be copied has to match the below AResultedHash parameter
end;


procedure TClickerPluginArchive.HandleOnComputeArchiveHash(AArchiveStream: Pointer; AArchiveStreamSize: Int64; var AResultedHash: TArr32OfByte; AAdditionalInfo: string = '');
var
  {$IFDEF MemPlugins}
    PluginIdx: Integer;
    PluginInMemFS: TDecDecHashPluginInMemFS;
    MemLoader: TDynMemLib;
    ConfigurePluginForMemoryContent: TConfigurePluginForMemoryContent_Proc;
    ProcessMemoryContent: TProcessMemoryContent_Proc;
    LibStream: TMemoryStream;
  {$ENDIF}

  MD5: TDCP_md5;
begin
  FillChar(AResultedHash[0], Length(AResultedHash), 0);  //provide a default

  if FHashingPluginName = '' then
  begin
    DoOnAddToLog('Using built-in hash (hashing plugin not provided).');

    MD5 := TDCP_md5.Create(nil);    //MD5 is already used by ClickerExtraUtils.pas
    try
      MD5.Init;
      MD5.Update(AArchiveStream^, AArchiveStreamSize);
      MD5.Final(AResultedHash);
    finally
      MD5.Free;
    end;

    Exit;
  end;

  //FHashingPluginName should end up here, using the following format: <ArhiveName.dllarc>|<PluginName.dll>
  FHashingArchiveName := Copy(FHashingPluginName, 1, Pos('|', FHashingPluginName) - 1);
  FHashingPluginName := Copy(FHashingPluginName, Pos('|', FHashingPluginName) + 1, MaxInt);
  DoOnAddToLog('Using hashing plugin: ' + FHashingPluginName + ' from ' + FHashingArchiveName);

  {$IFDEF MemPlugins}
    PluginIdx := GetPluginInMemFSIndex(FDecDecHashPluginInMemFSArr^, FHashingArchiveName);
    if PluginIdx = -1 then
      raise Exception.Create('Hashing plugin not found: ' + FHashingPluginName);

    PluginInMemFS := FDecDecHashPluginInMemFSArr[PluginIdx];

    if not PluginInMemFS.InMemFS.FileExistsInMem(FHashingPluginName) then
      raise Exception.Create('Hashing plugin not found in InMemFS: ' + FHashingPluginName + '   Count = ' + IntToStr(PluginInMemFS.InMemFS.TotalFileCount));

    MemLoader := TDynMemLib.Create;
    try
      LibStream := TMemoryStream.Create;
      try
        LibStream.SetSize(PluginInMemFS.InMemFS.GetFileSize(FHashingPluginName));
        PluginInMemFS.InMemFS.LoadFileFromMem(FHashingPluginName, LibStream.Memory);

        MemLoader.MemLoadLibrary(LibStream.Memory);
        try
          @ConfigurePluginForMemoryContent := MemLoader.MemGetProcAddress('ConfigurePluginForMemoryContent');
          if @ConfigurePluginForMemoryContent = nil then
            raise Exception.Create('Invalid hashing plugin: ' + FDecryptionPluginName + '.  ConfigurePluginForMemoryContent function not found.');

          @ProcessMemoryContent := MemLoader.MemGetProcAddress('ProcessMemoryContent');
          if @ProcessMemoryContent = nil then
            raise Exception.Create('Invalid hashing plugin: ' + FHashingPluginName + '.  ProcessMemoryContent function not found.');

          DoOnAddToLog('Configuring hashing plugin..');
          try
            ConfigurePluginForMemoryContent(HandleOnActionPlugin_HashInMemFS, HandleOnActionPlugin_SetTemplateVar, HandleOnActionPlugin_AddToLog);
          except
            on E: Exception do
            begin
              DoOnAddToLog('Ex on configuring hashing: ' + E.Message);
              raise;
            end;
          end;

          FHashingResult := @AResultedHash;
          ProcessMemoryContent(AArchiveStream, AArchiveStreamSize, HandleOnFileContent_Hashing);
        finally
          MemLoader.MemFreeLibrary;
        end;
      finally
        LibStream.Free;
      end;
    finally
      MemLoader.Free;
    end;
  {$ENDIF}
end;


procedure TClickerPluginArchive.ExtractArchive(AArchiveStream: TMemoryStream; ADecDecHashPluginInMemFSArr: PDecDecHashPluginInMemFSArr; ADecryptionPluginName, ADecompressionPluginName, AHashingPluginName: string; ACompressionLevel: Integer; AAdditionalInfo: string);
var
  ListOfFiles: TStringList;
  i, n: Integer;
  BadPluginPathBitPrefix, GoodPluginPathBitPrefix: string;
  ExtractedFile: TMemoryStream;
  Fnm: string;
begin
  {$IFDEF CPU64}
    BadPluginPathBitPrefix := UpperCase(CPluginPath32BitPrefix);    //32-bit prefix on 64-bit UIClicker
    GoodPluginPathBitPrefix := UpperCase(CPluginPath64BitPrefix);
  {$ELSE} //32
    BadPluginPathBitPrefix := UpperCase(CPluginPath64BitPrefix);    //64-bit prefix on 32-bit UIClicker
    GoodPluginPathBitPrefix := UpperCase(CPluginPath32BitPrefix);
  {$ENDIF}

  FDecryptionPluginName := ADecryptionPluginName;
  FDecompressionPluginName := ADecompressionPluginName;
  FHashingPluginName := AHashingPluginName;

  ListOfFiles := TStringList.Create;
  try
    FArchive.OnComputeArchiveHash := HandleOnComputeArchiveHash;

    if ADecompressionPluginName > '' then
    begin
      FArchive.OnDecompress := HandleOnDecompress;
      //other compression options
    end;
    FArchive.CompressionLevel := ACompressionLevel;

    if ADecryptionPluginName > '' then
    begin
      FArchive.Password := 'dummy'; //The password has to be <> '', to call decryption. However, the decryption key should be managed outside of this component.
      FArchive.OnInitEncryption := HandleOnInitEncryption;
      FArchive.OnGetKeyFromPassword := HandleOnGetKeyFromPassword;
      FArchive.OnDecryptArchive := HandleOnDecryptArchive;
    end;

    FDecDecHashPluginInMemFSArr := ADecDecHashPluginInMemFSArr; //copy pointer only

    FArchive.OpenArchive(AArchiveStream, False);
    try
      FArchive.GetListOfFiles(ListOfFiles);

      n := ListOfFiles.Count;
      for i := 0 to n - 1 do
        if Pos(BadPluginPathBitPrefix, UpperCase(ListOfFiles.Strings[i])) = 0 then  //do not extract 64-bit dll on 32-bit UIClicker or viceversa
        begin
          try
            ExtractedFile := TMemoryStream.Create;
            try
              Fnm := ListOfFiles.Strings[i];

              if Pos(GoodPluginPathBitPrefix, UpperCase(Fnm)) = 1 then  //the file name starts with 'i386-win32\' or 'x86-win64\'
                Fnm := Copy(Fnm, Length(GoodPluginPathBitPrefix) + 1, MaxInt); //discard 'i386-win32\' or 'x86-win64\' prefix, because this is used only to decide about plugin bitness
              //Plugins without the "good" prefix are still extracted. Plugins with bad prefix are not.

              {$IFDEF MemPlugins}
                if ExtractFileName(Fnm) = Fnm then //no path
                  Fnm := CMemPluginLocationPrefix + PathDelim + Fnm;

                if Pos(':' + PathDelim, Fnm) = 2 then
                  Fnm := CMemPluginLocationPrefix + Copy(Fnm, 3, MaxInt);

                if Pos(UpperCase(CMemPluginLocationPrefix), UpperCase(Fnm)) = 1 then  // e.g. MEM:\PathToDll\MyPlugin.dll
                  Fnm := CMemPluginLocationPrefix + Copy(Fnm, 5, MaxInt);

                if Pos(UpperCase(CMemPluginLocationPrefix), UpperCase(Fnm)) = 0 then  //still no 'Mem:\' prefix
                  Fnm := CMemPluginLocationPrefix + PathDelim + Fnm;
              {$ENDIF}

              FArchive.ExtractToStream(ListOfFiles.Strings[i], ExtractedFile); //uses the original filename, from archive   - raises an exception if something goes wrong, either from the archive component or one of its handlers
              FPluginsInMemFS.SaveFileToMem(Fnm, ExtractedFile.Memory, ExtractedFile.Size);  //uses the modified filename, based on bitness and Mem: prefix

              FillChar(ExtractedFile.Memory^, ExtractedFile.Size, 0);
            finally
              ExtractedFile.Free;
            end;
          finally
            if not FPluginsInMemFS.FileExistsInMem(Fnm) then
              raise Exception.Create('File not extracted and saved successfully: "' + Fnm + '".  Initial filename: "' + ListOfFiles.Strings[i] + '". Stopping..')
            else
              DoOnAddToLog('File extracted and saved successfully: "' + Fnm + '".  Initial filename: "' + ListOfFiles.Strings[i] + '".');
          end;
        end; //for, if
    finally
      try   //even the exception processing mechanism is affected by this memory corruption bug
        if ListOfFiles.Count <> n then
          DoOnAddToLog('Archive extraction corrupted memory.  i = ' + IntToStr(i) + '  n = ' + IntToStr(n) + '  Files.Count = ' + IntToStr(ListOfFiles.Count));
      except
      end;

      FArchive.CloseArchive;
    end;

    DoOnAddToLog('Done extracting archive..');
  finally
    ListOfFiles.Free;
  end;
end;

end.

