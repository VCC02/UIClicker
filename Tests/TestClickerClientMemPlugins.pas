{
    Copyright (C) 2025 VCC
    creation date: 13 Jan 2025
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


unit TestClickerClientMemPlugins;

{$mode Delphi}{$H+}

interface

uses
  LCLIntf, Classes, SysUtils, TestHTTPAPI, fpcunit, testregistry, Expectations,
  ClickerUtils, ClickerActionsClient, MemArchive;

type
  TTestClickerClientMemPlugins = class(TTestHTTPAPI)
  private
    FLoadClickerClientRes: Boolean;
    FUseDefaultUIClickerHash: Boolean;
    FUseDefaultEncryptionKey: Boolean;
    FUseBadEncryptionKey: Boolean;

    function Get_FindWindows_PluginPath_RelativeToTestApp: string;
    function Get_FindWindows_DbgSymPluginPath_RelativeToTestApp: string;
    function Get_Typewriter_PluginPath_RelativeToTestApp: string;
    function Get_Typewriter_DbgSymPluginPath_RelativeToTestApp: string;
    procedure SendGenericMemPluginFileToServer_HappyFlow(AFileName: string);

    procedure HandleOnInitEncryption(var AArcKey: TArr32OfByte);
    procedure HandleOnGetKeyFromPassword(APassword: string; var ArcKey: TArr32OfByte);
    procedure HandleOnEncryptArchive(AArchiveStream: TMemoryStream);
    procedure HandleOnEncryptionCleanup;
    procedure HandleOnCompress(APlainStream, AArchiveStream: TMemoryStream; ACompressionLevel: Integer);
    procedure HandleOnComputeArchiveHash(AArchiveStream: Pointer; AArchiveStreamSize: Int64; var AResultedHash: TArr32OfByte; AAdditionalInfo: string = '');
    procedure SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(AFileName, AFileNameInsideArchive, ADecryptionPluginName, ADecompressionPluginName, AHashingPluginName: string; AUseCompression: Boolean; AAdditionalInfo: string; AIsDecDecHash, ACreateCustomKey: Boolean; AExpectedError: string);
    procedure SendMultiBitnessMemPluginArchiveFileToServer(AFileName, AFileNameInsideArchive, AConfigStringAsFile, AFileName_x32, AFileNameInsideArchive_x32, AConfigStringAsFile_x32, AFileName_x64, AFileNameInsideArchive_x64, AConfigStringAsFile_x64, ADecryptionPluginName, ADecompressionPluginName, AHashingPluginName: string; AUseCompression: Boolean; AAdditionalInfo: string; AIsDecDecHash, ACreateCustomKey: Boolean; AExpectedError: string);

    procedure Test_SendMemPluginArchiveFileToServer_WithDecryptOnly_CfgKey(AUseCustomKey, AUseBadKey: Boolean; AExpectedError: string);

    procedure ExpectEmptyFSes;
    procedure ExpectFindWindowsPluginAndItsKey;
    procedure ExpectDecDecHashAndFindWindowsPluginAndItsKey(ADecDecHashPluginFnm: string; AExtraFnm: string = '');
    procedure ExpectAllDecDecHashAndFindWindowsPluginAndTheirKey(ADecryptionPluginFnm, ADecompressionPluginFnm, AHashingPluginFnm: string);
    procedure ExpectTypewriterPluginAndItsKey;
    procedure ExpectTestPluginAndItsKey;
    procedure ExpectListOfFilesPluginInMemFS(APluginFnm, AExpectedFilesInFS: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    constructor Create; override;
  published
    procedure Test_SendMemPluginFileToServer_HappyFlow;
    procedure Test_SendMemPluginDbgSymFileToServer_HappyFlow;

    procedure Test_SendMemPluginArchiveFileToServer_NoDecDecHash_HappyFlow;
    procedure Test_SendMemPluginArchiveFileToServer_NoDecDecHash_ReusingInMemFS;

    procedure Test_SendMemPluginArchiveFileToServer_WithDecompOnlyAndEmptyPluginName_HappyFlow;
    procedure Test_SendMemPluginArchiveFileToServer_WithDecryptOnlyAndNoPlugin_HappyFlow;
    procedure Test_SendMemPluginArchiveFileToServer_WithDecompOnlyAndNoPlugin_HappyFlow;
    procedure Test_SendMemPluginArchiveFileToServer_WithHashingOnlyAndNoPlugin_HappyFlow;

    procedure Test_SendMemPluginArchiveFileToServer_WithDecryptOnly_HappyFlow;
    procedure Test_SendMemPluginArchiveFileToServer_WithDecompOnly_HappyFlow;
    procedure Test_SendMemPluginArchiveFileToServer_WithHashingOnly_HappyFlow;
    procedure Test_SendMemPluginArchiveFileToServer_WithAllDecDecHashPlugins_HappyFlow;
    procedure Test_SendMemPluginArchiveFileToServer_WithDecryptOnly_CustomKey;
    procedure Test_SendMemPluginArchiveFileToServer_WithDecryptOnly_BadKey;
    procedure Test_SendMemPluginArchiveFileToServer_AllDbgSymInFS0;
    procedure Test_SendMemPluginArchiveFileToServer_GetListOfFilesDecDecHashInMemFS;
    procedure Test_SendMemPluginArchiveFileToServer_GetListOfFilesPluginInMemFS;
    procedure Test_SendMemPluginArchiveFileToServer_GetListOfFilesPluginInMemFS_PreventReusingInMemFS_StayFalse;
    procedure Test_SendMemPluginArchiveFileToServer_GetListOfFilesPluginInMemFS_PreventReusingInMemFS_StayTrue;
    procedure Test_SendMemPluginArchiveFileToServer_DummyFiles_GetListOfFilesPluginInMemFS;
    procedure Test_SendMemPluginWithoutArchiveFileToServer_DummyFiles_GetListOfFilesPluginInMemFS;

    procedure Test_SendSameBitnessMemPlugin_HappyFlow;
    procedure Test_SendOutsideBitnessOnlyMemPlugin_HappyFlow;
    procedure Test_SendBothBitnessesMemPlugin_HappyFlow;
  end;


implementation


uses
  Controls,
  DCPsha256, DCPmd5, DCPrijndael, TplLzmaUnit,
  Graphics, DllUtils, ClickerClientIntf, ActionsStuff;


const
  CTestDriverAddress = 'http://127.0.0.1:25444/';


constructor TTestClickerClientMemPlugins.Create;
begin
  inherited Create;
  TestServerAddress := CTestServerAddress;
  FLoadClickerClientRes := False;
end;


procedure TTestClickerClientMemPlugins.SetUp;
const
  CTestingRequirementInfo = 'These tests require UIClicker to be built with the following compiler directives: MemPlugins and PluginTesting.';
var
  RecServerAddress: string;
begin
  inherited SetUp;
  FLoadClickerClientRes := LoadClickerClient('..\ClickerClient\ClickerClient.dll');
  Expect(FLoadClickerClientRes).ToBe(True, 'Can''t load ClickerClient.dll');

  InitClickerClient;
  SetServerAddress(@WideString(TestServerAddress)[1]);

  SetLength(RecServerAddress, CMaxSharedStringLength);
  SetLength(RecServerAddress, GetServerAddress(@RecServerAddress[1]));

  Expect(RecServerAddress).ToBe(TestServerAddress, 'The configured server address does not match the expected one in ClickerClient.');
  SetVariable(TestServerAddress, '$ExecAction_Err$', '', 0);  //clear errors  - required if the debugged action should be successful
  SetVariable(CTestDriverAddress, '$ExtraCaption2$', '', 0);  //required for finding action window

  FUseDefaultUIClickerHash := True;
  FUseDefaultEncryptionKey := True;
  FUseBadEncryptionKey := False;

  Expect(DeleteAllMemPluginInMemFSes(CTestServerAddress, True)).ToBe(CREResp_ErrResponseOK, CTestingRequirementInfo);
  Expect(DeleteAllFilesFromMemPluginInMemFS(CTestServerAddress, -1, True)).ToBe(CREResp_ErrResponseOK, CTestingRequirementInfo);
  Expect(GetMemPluginInMemFSCount(CTestServerAddress)).ToBe('0', 'FS count in SetUp');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, -1)).ToBe(CREResp_ErrResponseOK, CTestingRequirementInfo);
end;


procedure TTestClickerClientMemPlugins.TearDown;
begin
  if FLoadClickerClientRes then
  begin
    try
      DoneClickerClient;
    finally
      UnLoadClickerClient;
    end;
  end;

  inherited TearDown;
end;


function TTestClickerClientMemPlugins.Get_FindWindows_PluginPath_RelativeToTestApp: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + '..\..\UIClickerFindWindowsPlugin\lib\' + GetPluginBitnessDirName + '\UIClickerFindWindows.dll';
end;


function TTestClickerClientMemPlugins.Get_FindWindows_DbgSymPluginPath_RelativeToTestApp: string;
begin
  Result := ExtractFullFileNameNoExt(Get_FindWindows_PluginPath_RelativeToTestApp) + '.dbgsym';
end;


function TTestClickerClientMemPlugins.Get_Typewriter_PluginPath_RelativeToTestApp: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + '..\..\UIClickerTypewriterPlugin\lib\' + GetPluginBitnessDirName + '\UIClickerTypewriter.dll';
end;


function TTestClickerClientMemPlugins.Get_Typewriter_DbgSymPluginPath_RelativeToTestApp: string;
begin
  Result := ExtractFullFileNameNoExt(Get_Typewriter_PluginPath_RelativeToTestApp) + '.dbgsym';
end;


procedure TTestClickerClientMemPlugins.SendGenericMemPluginFileToServer_HappyFlow(AFileName: string);
var
  MemStream: TMemoryStream;
  FileNameWS: WideString;
  Response: string;
  ResLen: Integer;
begin
  Expect(FileExists(AFileName)).ToBe(True, 'The test expects this file to exist on disk: "' + AFileName + '".');

  MemStream := TMemoryStream.Create;
  try
    FileNameWS := WideString(ExtractFileName(AFileName));
    SetLength(Response, CMaxSharedStringLength);

    MemStream.LoadFromFile(AFileName);
    ResLen := SendMemPluginFileToServer(@FileNameWS[1], MemStream.Memory, MemStream.Size, @Response[1]);
    SetLength(Response, ResLen);
  finally
    MemStream.Free;
  end;

  try
    Expect(Response).ToBe(CREResp_ErrResponseOK);
  except
    on E: EExp do
      Expect(Response).ToBe(CREResp_NotImplemented);
  end;
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginFileToServer_HappyFlow;
var
  Fnm: string;
begin
  Fnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  SendGenericMemPluginFileToServer_HappyFlow(Fnm);

  Expect(GetMemPluginInMemFSCount(CTestServerAddress)).ToBe('1', 'FS count');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, -1)).ToBe(CREResp_ErrResponseOK, 'from main FS');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, 0)).ToBe('Mem:\' + ExtractFileName(Fnm) + #8#7, 'From FS 0');
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginDbgSymFileToServer_HappyFlow;
var
  Fnm: string;
begin
  Fnm := Get_FindWindows_DbgSymPluginPath_RelativeToTestApp;
  SendGenericMemPluginFileToServer_HappyFlow(Fnm);

  Expect(GetMemPluginInMemFSCount(CTestServerAddress)).ToBe('1', 'FS count');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, -1)).ToBe(CREResp_ErrResponseOK, 'from main FS');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, 0)).ToBe('Mem:\' + ExtractFileName(Fnm) + #8#7, 'From FS 0'); //dbgsym files end up in FS0
end;


procedure TTestClickerClientMemPlugins.HandleOnCompress(APlainStream, AArchiveStream: TMemoryStream; ACompressionLevel: Integer);
var
  Options: TplLzmaOptions;
begin
  //AArchiveStream.CopyFrom(APlainStream, APlainStream.Size);  // - for debugging - no compression used

  Options.Algorithm := 2;
  Options.DictionarySize := 1048576;
  Options.EOS := True;
  Options.MatchFinder := 1;
  Options.NumBenchMarkPasses := 10;
  Options.Fb := 273; // 5;
  Options.Lc := 3;
  Options.Lp := 0;
  Options.Pb := 0;
  CompressStreamLzma(APlainStream, AArchiveStream, Options);
end;


procedure TTestClickerClientMemPlugins.HandleOnInitEncryption(var AArcKey: TArr32OfByte);
begin
  //
end;


procedure TTestClickerClientMemPlugins.HandleOnGetKeyFromPassword(APassword: string; var ArcKey: TArr32OfByte);
begin
  //
end;


const
  CCustomKey = '0123456789ABCDEF0123456789ABCDEF';


procedure TTestClickerClientMemPlugins.HandleOnEncryptArchive(AArchiveStream: TMemoryStream);
var
  AES: TDCP_rijndael;
  i: Integer;
  Key: array[0..31] of Byte;
begin
  AES := TDCP_rijndael.Create(nil);
  try
    FillChar(Key, Length(Key), 0);

    if not FUseDefaultEncryptionKey then
      Move(CCustomKey, Key, Length(Key));

    if FUseBadEncryptionKey then
      FillChar(Key, Length(Key), 3); //a bad key

    AES.Init(Key, Length(Key) shl 3, nil);

    try
      for i := 0 to AArchiveStream.Size shr 4 - 1 do   //ALen2 is ALen1 + padding
        AES.EncryptECB(Pointer(UInt64(AArchiveStream.Memory) + i shl 4)^, Pointer(UInt64(AArchiveStream.Memory) + i shl 4)^);
    finally
      AES.Burn;
    end;
  finally
    AES.Free;
  end;

  //Xor example
  //for i := 0 to AArchiveStream.Size - 1 do
  //  PByte(Pointer(UInt64(AArchiveStream.Memory) + i))^ := PByte(Pointer(UInt64(AArchiveStream.Memory) + i))^ xor 3;
end;


procedure TTestClickerClientMemPlugins.HandleOnEncryptionCleanup;
begin
  //
end;


procedure TTestClickerClientMemPlugins.HandleOnComputeArchiveHash(AArchiveStream: Pointer; AArchiveStreamSize: Int64; var AResultedHash: TArr32OfByte; AAdditionalInfo: string = '');
var
  SHA256: TDCP_sha256;
  MD5: TDCP_md5;
begin
  FillChar(AResultedHash, 32, 0);  //init with 0, because MD5 returns only 16 bytes

  if FUseDefaultUIClickerHash then //MD5 is already used by UIClicker for file integrity (ClickerExtraUtils.pas)
  begin
    MD5 := TDCP_md5.Create(nil);
    try
      MD5.Init;
      MD5.Update(AArchiveStream^, AArchiveStreamSize);
      MD5.Final(AResultedHash);
    finally
      MD5.Free;
    end;
    Exit;
  end;

  SHA256 := TDCP_sha256.Create(nil);   //This is implemented by the hashing plugin. (it could have been other algorithm)
  try
    SHA256.Init;
    SHA256.Update(AArchiveStream^, AArchiveStreamSize);
    SHA256.Final(AResultedHash);
  finally
    SHA256.Free;
  end;
end;


procedure TTestClickerClientMemPlugins.SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(AFileName, AFileNameInsideArchive, ADecryptionPluginName, ADecompressionPluginName, AHashingPluginName: string; AUseCompression: Boolean; AAdditionalInfo: string; AIsDecDecHash, ACreateCustomKey: Boolean; AExpectedError: string);
var
  MemStream, ArchiveStream: TMemoryStream;
  FileNameWS, DecryptionPluginNameWS, DecompressionPluginNameWS, HashingPluginNameWS, AdditionalInfoWS: WideString;
  Response: string;
  ResLen: Integer;
  Archive: TMemArchive;
begin
  Expect(FileExists(AFileName)).ToBe(True, 'The test expects this file to exist on disk: "' + AFileName + '".');

  MemStream := TMemoryStream.Create;
  ArchiveStream := TMemoryStream.Create;
  Archive := TMemArchive.Create;
  try
    SetLength(Response, CMaxSharedStringLength);
    MemStream.LoadFromFile(AFileName);

    if ADecryptionPluginName > '' then
    begin
      Archive.OnInitEncryption := HandleOnInitEncryption;
      Archive.OnGetKeyFromPassword := HandleOnGetKeyFromPassword;
      Archive.OnEncryptArchive := HandleOnEncryptArchive;
      Archive.OnEncryptionCleanup := HandleOnEncryptionCleanup;
      Archive.Password := 'dummy';  //setting the password to a value, different than '', is enough to call the encryption handlers
                                    //however, in this example, this mechanism is not used. The key is hardcoded. (same as in the example plugin)
    end;

    if AUseCompression then
      Archive.OnCompress := HandleOnCompress;

    Archive.CompressionLevel := 9 * Ord(AUseCompression);

    Archive.OnComputeArchiveHash := HandleOnComputeArchiveHash;
    Archive.OpenArchive(ArchiveStream, True);
    try
      Archive.AddFromStream(AFileNameInsideArchive, MemStream);  //this should be .dll

      if ACreateCustomKey then
      begin
        Archive.AddFromString('Key.txt', CCustomKey);
        Archive.AddFromString('UseKey.txt', 'True');
      end
      else
        Archive.AddFromString('UseKey.txt', 'False');
    finally
      Archive.CloseArchive;
    end;

    AFileName := AFileName + 'arc'; //results .dllarc

    FileNameWS := WideString(ExtractFileName(AFileName));
    DecryptionPluginNameWS := WideString(ADecryptionPluginName);
    DecompressionPluginNameWS := WideString(ADecompressionPluginName);
    HashingPluginNameWS := WideString(AHashingPluginName);
    AdditionalInfoWS := WideString(AAdditionalInfo); //for example a list of compression parameters

    ResLen := SendMemPluginArchiveFileToServer(@FileNameWS[1],
                                               @DecryptionPluginNameWS[1],
                                               @DecompressionPluginNameWS[1],
                                               @HashingPluginNameWS[1],
                                               ArchiveStream.Memory,
                                               ArchiveStream.Size,
                                               Archive.CompressionLevel,
                                               @AdditionalInfoWS[1],
                                               AIsDecDecHash,
                                               @Response[1]);
    SetLength(Response, ResLen);
  finally
    MemStream.Free;
    ArchiveStream.Free;
    Archive.Free;
  end;

  try
    Expect(Response).ToBe(AExpectedError);
  except
    on E: EExp do
      Expect(Response).ToBe(CREResp_NotImplemented, 'Previous expected response: ' + AExpectedError);
  end;
end;


procedure TTestClickerClientMemPlugins.SendMultiBitnessMemPluginArchiveFileToServer(AFileName, AFileNameInsideArchive, AConfigStringAsFile,
                                                                                    AFileName_x32, AFileNameInsideArchive_x32, AConfigStringAsFile_x32,
                                                                                    AFileName_x64, AFileNameInsideArchive_x64, AConfigStringAsFile_x64,
                                                                                    ADecryptionPluginName, ADecompressionPluginName, AHashingPluginName: string;
                                                                                    AUseCompression: Boolean;
                                                                                    AAdditionalInfo: string;
                                                                                    AIsDecDecHash, ACreateCustomKey: Boolean;
                                                                                    AExpectedError: string);
var
  MemStream, ArchiveStream: TMemoryStream;
  FileNameWS, DecryptionPluginNameWS, DecompressionPluginNameWS, HashingPluginNameWS, AdditionalInfoWS: WideString;
  Response: string;
  ResLen: Integer;
  Archive: TMemArchive;
begin
  //Expect(FileExists(AFileName)).ToBe(True, 'The test expects this file to exist on disk: "' + AFileName + '".');   //not for this type of test
  Expect(AFileName).NotToBe('', 'AFileName must be different than empty string, because it is used as archive name. It doesn''t have to point to a file on disk.');

  MemStream := TMemoryStream.Create;
  ArchiveStream := TMemoryStream.Create;
  Archive := TMemArchive.Create;
  try
    SetLength(Response, CMaxSharedStringLength);

    if ADecryptionPluginName > '' then
    begin
      Archive.OnInitEncryption := HandleOnInitEncryption;
      Archive.OnGetKeyFromPassword := HandleOnGetKeyFromPassword;
      Archive.OnEncryptArchive := HandleOnEncryptArchive;
      Archive.OnEncryptionCleanup := HandleOnEncryptionCleanup;
      Archive.Password := 'dummy';  //setting the password to a value, different than '', is enough to call the encryption handlers
                                    //however, in this example, this mechanism is not used. The key is hardcoded. (same as in the example plugin)
    end;

    if AUseCompression then
      Archive.OnCompress := HandleOnCompress;

    Archive.CompressionLevel := 9 * Ord(AUseCompression);

    Archive.OnComputeArchiveHash := HandleOnComputeArchiveHash;
    Archive.OpenArchive(ArchiveStream, True);
    try
      //Load the default plugin, if it exists on disk
      if FileExists(AFileName) then
      begin
        MemStream.LoadFromFile(AFileName);
        Archive.AddFromStream(AFileNameInsideArchive, MemStream);  //this should be .dll
      end;

      //Load 32-bit and 64-bit versions of the plugin, if they exist on disk
      if FileExists(AFileName_x32) then
      begin
        MemStream.LoadFromFile(AFileName_x32);
        Archive.AddFromStream(AFileNameInsideArchive_x32, MemStream);
      end;

      if FileExists(AFileName_x64) then
      begin
        MemStream.LoadFromFile(AFileName_x64);
        Archive.AddFromStream(AFileNameInsideArchive_x64, MemStream);
      end;

      if ACreateCustomKey then
      begin
        Archive.AddFromString('Key.txt', CCustomKey);
        Archive.AddFromString('UseKey.txt', 'True');
      end
      else
        Archive.AddFromString('UseKey.txt', 'False');

      if AConfigStringAsFile <> '' then
        Archive.AddFromString(ExtractFullFileNameNoExt(AFileNameInsideArchive) + '.cfg', AConfigStringAsFile);

      if AConfigStringAsFile_x32 <> '' then
        Archive.AddFromString(ExtractFullFileNameNoExt(AFileNameInsideArchive_x32) + '.cfg', AConfigStringAsFile_x32);

      if AConfigStringAsFile_x64 <> '' then
        Archive.AddFromString(ExtractFullFileNameNoExt(AFileNameInsideArchive_x64) + '.cfg', AConfigStringAsFile_x64);
    finally
      Archive.CloseArchive;
    end;

    AFileName := AFileName + 'arc'; //results .dllarc

    FileNameWS := WideString(ExtractFileName(AFileName));
    DecryptionPluginNameWS := WideString(ADecryptionPluginName);
    DecompressionPluginNameWS := WideString(ADecompressionPluginName);
    HashingPluginNameWS := WideString(AHashingPluginName);
    AdditionalInfoWS := WideString(AAdditionalInfo); //for example a list of compression parameters

    ResLen := SendMemPluginArchiveFileToServer(@FileNameWS[1],
                                               @DecryptionPluginNameWS[1],
                                               @DecompressionPluginNameWS[1],
                                               @HashingPluginNameWS[1],
                                               ArchiveStream.Memory,
                                               ArchiveStream.Size,
                                               Archive.CompressionLevel,
                                               @AdditionalInfoWS[1],
                                               AIsDecDecHash,
                                               @Response[1]);
    SetLength(Response, ResLen);
  finally
    MemStream.Free;
    ArchiveStream.Free;
    Archive.Free;
  end;

  try
    Expect(Response).ToBe(AExpectedError);
  except
    on E: EExp do
      Expect(Response).ToBe(CREResp_NotImplemented, 'Previous expected response: ' + AExpectedError);
  end;
end;


procedure TTestClickerClientMemPlugins.ExpectEmptyFSes;
begin
  Expect(GetMemPluginInMemFSCount(CTestServerAddress)).ToBe('1', 'no FSes');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, -1)).ToBe(CREResp_ErrResponseOK, 'from main FS');
end;


procedure TTestClickerClientMemPlugins.ExpectFindWindowsPluginAndItsKey;
var
  Fnm: string;
begin
  Fnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  Delete(Fnm, 1, 3); //Delete drive
  Fnm := 'Mem:\' + Fnm;

  Expect(GetMemPluginInMemFSCount(CTestServerAddress)).ToBe('1', 'FS count');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, -1)).ToBe(CREResp_ErrResponseOK, 'from main FS');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, 0)).ToBe(Fnm + #8#7 + 'Mem:\UseKey.txt' + #8#7, 'From FS 0');
end;


procedure TTestClickerClientMemPlugins.ExpectDecDecHashAndFindWindowsPluginAndItsKey(ADecDecHashPluginFnm: string; AExtraFnm: string = '');
var
  Fnm: string;
begin
  Fnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  Fnm := 'Mem:\' + ExtractFileName(Fnm);
  ADecDecHashPluginFnm := 'Mem:\' + ExtractFileName(ADecDecHashPluginFnm);

  if AExtraFnm > '' then
    ADecDecHashPluginFnm := ADecDecHashPluginFnm + #8#7 + AExtraFnm;

  Expect(GetMemPluginInMemFSCount(CTestServerAddress)).ToBe('2', 'FS count');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, -1)).ToBe(CREResp_ErrResponseOK, 'from main FS');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, 0)).ToBe(ADecDecHashPluginFnm + #8#7 + 'Mem:\UseKey.txt' + #8#7, 'From FS 0');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, 1)).ToBe(Fnm + #8#7 + 'Mem:\UseKey.txt' + #8#7, 'From FS 1');
end;


procedure TTestClickerClientMemPlugins.ExpectAllDecDecHashAndFindWindowsPluginAndTheirKey(ADecryptionPluginFnm, ADecompressionPluginFnm, AHashingPluginFnm: string);
var
  Fnm: string;
begin
  Fnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  Fnm := 'Mem:\' + ExtractFileName(Fnm);
  ADecryptionPluginFnm := 'Mem:\' + ExtractFileName(ADecryptionPluginFnm);
  ADecompressionPluginFnm := 'Mem:\' + ExtractFileName(ADecompressionPluginFnm);
  AHashingPluginFnm := 'Mem:\' + ExtractFileName(AHashingPluginFnm);

  Expect(GetMemPluginInMemFSCount(CTestServerAddress)).ToBe('4', 'FS count');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, -1)).ToBe(CREResp_ErrResponseOK, 'from main FS');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, 0)).ToBe(ADecryptionPluginFnm + #8#7 + 'Mem:\UseKey.txt' + #8#7, 'From FS 0');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, 1)).ToBe(ADecompressionPluginFnm + #8#7 + 'Mem:\UseKey.txt' + #8#7, 'From FS 1');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, 2)).ToBe(AHashingPluginFnm + #8#7 + 'Mem:\UseKey.txt' + #8#7, 'From FS 2');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, 3)).ToBe(Fnm + #8#7 + 'Mem:\UseKey.txt' + #8#7, 'From FS 3');
end;


procedure TTestClickerClientMemPlugins.ExpectTypewriterPluginAndItsKey;
var
  Fnm: string;
begin
  Fnm := Get_Typewriter_PluginPath_RelativeToTestApp;
  Delete(Fnm, 1, 3); //Delete drive
  Fnm := 'Mem:\' + Fnm;

  Expect(GetMemPluginInMemFSCount(CTestServerAddress)).ToBe('1', 'FS count');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, -1)).ToBe(CREResp_ErrResponseOK, 'from main FS');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, 0)).ToBe(Fnm + #8#7 + 'Mem:\UseKey.txt' + #8#7, 'From FS 0');
end;


procedure TTestClickerClientMemPlugins.ExpectTestPluginAndItsKey;
var
  TestPluginName: string;
  Fnm: string;
begin
  Fnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  Fnm := 'Mem:\' + ExtractFileName(Fnm);
  TestPluginName := 'Mem:\' + 'TestPlugin.dll';

  Expect(GetMemPluginInMemFSCount(CTestServerAddress)).ToBe('2', 'FS count');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, -1)).ToBe(CREResp_ErrResponseOK, 'from main FS');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, 0)).ToBe(TestPluginName + #8#7 + 'Mem:\UseKey.txt' + #8#7, 'From FS 0');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, 1)).ToBe(Fnm + #8#7 + 'Mem:\UseKey.txt' + #8#7, 'From FS 1');
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginArchiveFileToServer_NoDecDecHash_HappyFlow;
var
  Fnm: string;
begin
  Fnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(Fnm, Fnm, '', '', '', False, 'none', False, False, CREResp_ErrResponseOK);
  ExpectFindWindowsPluginAndItsKey;
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginArchiveFileToServer_NoDecDecHash_ReusingInMemFS;
var
  Fnm: string;
begin
  Fnm := Get_Typewriter_PluginPath_RelativeToTestApp;   //uses a different plugin than the other tests
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(Fnm, Fnm, '', '', '', False, CREParam_PreventReusingInMemFS + '%3D' + 'True', True, False, CREResp_ErrResponseOK);
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(Fnm, Fnm, '', '', '', False, 'none', True, False, CREResp_TooManyPluginFileSystems);
  ExpectTypewriterPluginAndItsKey;
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginArchiveFileToServer_WithDecompOnlyAndEmptyPluginName_HappyFlow;
var
  Fnm: string;
begin
  Fnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(Fnm, Fnm, '', '', '', True, 'none', False, False, 'PluginError: Decompression plugin not set.');
  ExpectEmptyFSes;
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginArchiveFileToServer_WithDecryptOnlyAndNoPlugin_HappyFlow;
var
  Fnm: string;
begin
  Fnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(Fnm, Fnm, 'Decrypt.dll', '', '', False, 'none', False, False, 'PluginError: Decryption plugin not found: Decrypt.dll');
  ExpectEmptyFSes;
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginArchiveFileToServer_WithDecompOnlyAndNoPlugin_HappyFlow;
var
  Fnm: string;
begin
  Fnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(Fnm, Fnm, '', 'Decomp.dll', '', True, 'none', False, False, 'PluginError: Decompression plugin not found: Decomp.dll');
  ExpectEmptyFSes;
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginArchiveFileToServer_WithHashingOnlyAndNoPlugin_HappyFlow;
var
  Fnm: string;
begin
  Fnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(Fnm, Fnm, '', '', 'Hashing.dll', False, 'none', False, False, 'PluginError: Hashing plugin not found: Hashing.dll');
  ExpectEmptyFSes;
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginArchiveFileToServer_WithDecryptOnly_HappyFlow;
var
  DecryptionPluginName: string;
  Fnm: string;
begin
  Fnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  DecryptionPluginName := ExtractFilePath(ParamStr(0)) + '..\..\UIClickerDecryptionExamplePlugin\lib\' + GetPluginBitnessDirName + '\UIClickerDecryptionExample.dll';

  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(DecryptionPluginName, ExtractFileName(DecryptionPluginName), '', '', '', False, '', True, False, CREResp_ErrResponseOK);
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(Fnm, ExtractFileName(Fnm), ExtractFileName(DecryptionPluginName) + 'arc|Mem:\' + ExtractFileName(DecryptionPluginName), '', '', False, 'none', False, False, CREResp_ErrResponseOK);
  ExpectDecDecHashAndFindWindowsPluginAndItsKey(DecryptionPluginName);
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginArchiveFileToServer_WithDecompOnly_HappyFlow;
var
  DecompressionPluginName: string;
  Fnm: string;
begin
  Fnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  DecompressionPluginName := ExtractFilePath(ParamStr(0)) + '..\..\UIClickerDecompressionExamplePlugin\lib\' + GetPluginBitnessDirName + '\UIClickerDecompressionExample.dll';

  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(DecompressionPluginName, ExtractFileName(DecompressionPluginName), '', '', '', False, '', True, False, CREResp_ErrResponseOK);
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(Fnm, ExtractFileName(Fnm), '', ExtractFileName(DecompressionPluginName) + 'arc|Mem:\' + ExtractFileName(DecompressionPluginName), '', True, 'none', False, False, CREResp_ErrResponseOK);
  ExpectDecDecHashAndFindWindowsPluginAndItsKey(DecompressionPluginName);
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginArchiveFileToServer_WithHashingOnly_HappyFlow;
var
  HashingPluginName: string;
  Fnm: string;
begin
  Fnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  HashingPluginName := ExtractFilePath(ParamStr(0)) + '..\..\UIClickerHashingExamplePlugin\lib\' + GetPluginBitnessDirName + '\UIClickerHashingExample.dll';

  FUseDefaultUIClickerHash := True;
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(HashingPluginName, ExtractFileName(HashingPluginName), '', '', '', False, '', True, False, CREResp_ErrResponseOK);

  FUseDefaultUIClickerHash := False; //when False, the SHA256 algorithm is used (from plugin by UIClicker)
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(Fnm, ExtractFileName(Fnm), '', '', ExtractFileName(HashingPluginName) + 'arc|Mem:\' + ExtractFileName(HashingPluginName), False, 'none', False, False, CREResp_ErrResponseOK);
  ExpectDecDecHashAndFindWindowsPluginAndItsKey(HashingPluginName);
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginArchiveFileToServer_WithAllDecDecHashPlugins_HappyFlow;
var
  DecryptionPluginName: string;
  DecompressionPluginName: string;
  HashingPluginName: string;
  Fnm: string;
begin
  Fnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  DecryptionPluginName := ExtractFilePath(ParamStr(0)) + '..\..\UIClickerDecryptionExamplePlugin\lib\' + GetPluginBitnessDirName + '\UIClickerDecryptionExample.dll';
  DecompressionPluginName := ExtractFilePath(ParamStr(0)) + '..\..\UIClickerDecompressionExamplePlugin\lib\' + GetPluginBitnessDirName + '\UIClickerDecompressionExample.dll';
  HashingPluginName := ExtractFilePath(ParamStr(0)) + '..\..\UIClickerHashingExamplePlugin\lib\' + GetPluginBitnessDirName + '\UIClickerHashingExample.dll';

  FUseDefaultUIClickerHash := True;
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(DecryptionPluginName, 'New' + ExtractFileName(DecryptionPluginName), '', '', '', False, '', True, False, CREResp_ErrResponseOK);
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(DecompressionPluginName, 'New' + ExtractFileName(DecompressionPluginName), '', '', '', False, '', True, False, CREResp_ErrResponseOK);
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(HashingPluginName, 'New' + ExtractFileName(HashingPluginName), '', '', '', False, '', True, False, CREResp_ErrResponseOK);

  FUseDefaultUIClickerHash := False; //when False, the SHA256 algorithm is used (from plugin by UIClicker)
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(Fnm,
                                                                 ExtractFileName(Fnm),
                                                                 ExtractFileName(DecryptionPluginName) + 'arc|Mem:\' + 'New' + ExtractFileName(DecryptionPluginName),
                                                                 ExtractFileName(DecompressionPluginName) + 'arc|Mem:\' + 'New' + ExtractFileName(DecompressionPluginName),
                                                                 ExtractFileName(HashingPluginName) + 'arc|Mem:\' + 'New' + ExtractFileName(HashingPluginName),
                                                                 False,
                                                                 'none',
                                                                 False,
                                                                 False,
                                                                 CREResp_ErrResponseOK);

  ExpectAllDecDecHashAndFindWindowsPluginAndTheirKey('New' + ExtractFileName(DecryptionPluginName), 'New' + ExtractFileName(DecompressionPluginName), 'New' + ExtractFileName(HashingPluginName));
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginArchiveFileToServer_WithDecryptOnly_CfgKey(AUseCustomKey, AUseBadKey: Boolean; AExpectedError: string);
var
  DecryptionPluginName: string;
  Fnm: string;
begin
  Fnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  DecryptionPluginName := ExtractFilePath(ParamStr(0)) + '..\..\UIClickerDecryptionExamplePlugin\lib\' + GetPluginBitnessDirName + '\UIClickerDecryptionExample.dll';

  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(DecryptionPluginName, ExtractFileName(DecryptionPluginName), '', '', '', False, '', True, True, CREResp_ErrResponseOK);

  if AUseCustomKey then
    FUseDefaultEncryptionKey := False;

  if AUseBadKey then
    FUseBadEncryptionKey := True;

  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(Fnm, ExtractFileName(Fnm), ExtractFileName(DecryptionPluginName) + 'arc|Mem:\' + ExtractFileName(DecryptionPluginName), '', '', False, 'none', False, False, AExpectedError);

  if AUseBadKey then
  begin
    Expect(GetMemPluginInMemFSCount(CTestServerAddress)).ToBe('1', 'FS count');
    Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, -1)).ToBe(CREResp_ErrResponseOK, 'from main FS');
    Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, 0)).ToBe('Mem:\' + ExtractFileName(DecryptionPluginName) + #8#7 + 'Mem:\Key.txt' + #8#7 + 'Mem:\UseKey.txt' + #8#7, 'from first FS');
  end
  else
    ExpectDecDecHashAndFindWindowsPluginAndItsKey(DecryptionPluginName, 'Mem:\Key.txt');
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginArchiveFileToServer_WithDecryptOnly_CustomKey;
begin
  Test_SendMemPluginArchiveFileToServer_WithDecryptOnly_CfgKey(True, False, CREResp_ErrResponseOK);
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginArchiveFileToServer_WithDecryptOnly_BadKey;
begin
  Test_SendMemPluginArchiveFileToServer_WithDecryptOnly_CfgKey(False, True, 'PluginError: Archive is invalid. Hash mismatch.');
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginArchiveFileToServer_AllDbgSymInFS0;
var
  FirstPluginFnm, SecondPluginFnm: string;
  FirstDbgSymFnm, SecondDbgSymFnm: string;
begin
  FirstPluginFnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  FirstDbgSymFnm := Get_FindWindows_DbgSymPluginPath_RelativeToTestApp;
  SecondPluginFnm := Get_Typewriter_PluginPath_RelativeToTestApp;
  SecondDbgSymFnm := Get_Typewriter_DbgSymPluginPath_RelativeToTestApp;

  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(FirstPluginFnm, ExtractFileName(FirstPluginFnm), '', '', '', False, '', False, False, CREResp_ErrResponseOK);
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(FirstDbgSymFnm, ExtractFileName(FirstDbgSymFnm), '', '', '', False, '', False, False, CREResp_ErrResponseOK);
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(SecondPluginFnm, ExtractFileName(SecondPluginFnm), '', '', '', False, '', False, False, CREResp_ErrResponseOK);
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(SecondDbgSymFnm, ExtractFileName(SecondDbgSymFnm), '', '', '', False, '', False, False, CREResp_ErrResponseOK);

  Expect(GetMemPluginInMemFSCount(CTestServerAddress)).ToBe('2', 'FS count');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, -1)).ToBe(CREResp_ErrResponseOK, 'from main FS');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, 0)).ToBe('Mem:\' + ExtractFileName(FirstPluginFnm) + #8#7 + 'Mem:\UseKey.txt' + #8#7 + 'Mem:\' + ExtractFileName(FirstDbgSymFnm) + #8#7 + 'Mem:\' + ExtractFileName(SecondDbgSymFnm) + #8#7, 'From FS 0');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, 1)).ToBe('Mem:\' + ExtractFileName(SecondPluginFnm) + #8#7 + 'Mem:\UseKey.txt' + #8#7, 'From FS 1');
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginArchiveFileToServer_GetListOfFilesDecDecHashInMemFS;
var
  TestPluginName: string;
  Fnm: string;
begin
  Fnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  TestPluginName := ExtractFilePath(ParamStr(0)) + 'TestFiles\TestPlugin\lib\' + GetPluginBitnessDirName + '\TestPlugin.dll';

  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(TestPluginName, ExtractFileName(TestPluginName), '', '', '', False, '', True, False, CREResp_ErrResponseOK);
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(Fnm, ExtractFileName(Fnm), '', ExtractFileName(TestPluginName) + 'arc|Mem:\' + ExtractFileName(TestPluginName), '', True, 'none', False, False, CREResp_ErrResponseOK);

  Expect(GetVarValueFromServer('$ListOfFilesInDecDecHashPluginInMemFS$')).ToBe('Mem:\' + ExtractFileName(TestPluginName) + #6#8 + 'Mem:\UseKey.txt' + #6#8);
  ExpectTestPluginAndItsKey;
end;


procedure TTestClickerClientMemPlugins.ExpectListOfFilesPluginInMemFS(APluginFnm, AExpectedFilesInFS: string);
var
  PluginOptions: TClkPluginOptions;
begin
  Expect(SetVariable(CTestServerAddress, '$ListOfFilesInPluginInMemFS$', '---', 0)).ToBe(CREResp_Done, 'Init var');
  Expect(GetVarValueFromServer('$ListOfFilesInPluginInMemFS$')).ToBe('---', 'Init var set');
  GeneratePluginOptions(PluginOptions, APluginFnm, '');
  ExpectSuccessfulAction(FastReplace_87ToReturn(ClickerActionsClient.ExecutePluginAction(CTestServerAddress, PluginOptions, True, False, False)));
  Expect(GetVarValueFromServer('$ListOfFilesInPluginInMemFS$')).ToBe(AExpectedFilesInFS, 'Var set by plugin');
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginArchiveFileToServer_GetListOfFilesPluginInMemFS;
var
  TestPluginName: string;
begin
  TestPluginName := ExtractFilePath(ParamStr(0)) + 'TestFiles\TestPlugin\lib\' + GetPluginBitnessDirName + '\TestPlugin.dll';
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(TestPluginName, 'SimpleTestPlugin.dll', '', '', '', False, '', False, False, CREResp_ErrResponseOK);

  Expect(GetMemPluginInMemFSCount(CTestServerAddress)).ToBe('1', 'FS count');
  ExpectListOfFilesPluginInMemFS('Mem:\SimpleTestPlugin.dll', '');
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginArchiveFileToServer_GetListOfFilesPluginInMemFS_PreventReusingInMemFS_StayFalse;
var
  TestPluginName: string;
begin
  TestPluginName := ExtractFilePath(ParamStr(0)) + 'TestFiles\TestPlugin\lib\' + GetPluginBitnessDirName + '\TestPlugin.dll';

  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(TestPluginName, 'SimpleTestPlugin.dll', '', '', '', False, '', False, False, CREResp_ErrResponseOK);
  Expect(GetMemPluginInMemFSCount(CTestServerAddress)).ToBe('1', 'FS count - before');
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(TestPluginName, 'SimpleTestPlugin.dll', '', '', '', False, CREParam_PreventReusingInMemFS + '%3D' + 'True', False, False, CREResp_ErrResponseOK);
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(TestPluginName, 'SimpleTestPlugin.dll', '', '', '', False, '', False, False, CREResp_ErrResponseOK);

  Expect(GetMemPluginInMemFSCount(CTestServerAddress)).ToBe('1', 'FS count - after');  //even after sending with a different Prevent flag, the existing one should remain unchanged
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(TestPluginName, 'SimpleTestPlugin.dll', '', '', '', False, '', False, False, CREResp_ErrResponseOK);
  Expect(GetMemPluginInMemFSCount(CTestServerAddress)).ToBe('1', 'FS count - second after');  //even after sending with a different Prevent flag, the existing one should remain unchanged

  ExpectListOfFilesPluginInMemFS('Mem:\SimpleTestPlugin.dll', '');
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginArchiveFileToServer_GetListOfFilesPluginInMemFS_PreventReusingInMemFS_StayTrue;
var
  TestPluginName: string;
begin
  TestPluginName := ExtractFilePath(ParamStr(0)) + 'TestFiles\TestPlugin\lib\' + GetPluginBitnessDirName + '\TestPlugin.dll';

  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(TestPluginName, 'SimpleTestPlugin.dll', '', '', '', False, CREParam_PreventReusingInMemFS + '%3D' + 'True', False, False, CREResp_ErrResponseOK);
  Expect(GetMemPluginInMemFSCount(CTestServerAddress)).ToBe('1', 'FS count - before');
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(TestPluginName, 'SimpleTestPlugin.dll', '', '', '', False, '', False, False, CREResp_TooManyPluginFileSystems);
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(TestPluginName, 'SimpleTestPlugin.dll', '', '', '', False, '', False, False, CREResp_TooManyPluginFileSystems);

  Expect(GetMemPluginInMemFSCount(CTestServerAddress)).ToBe('1', 'FS count - after');  //even after sending with a different Prevent flag, the existing one should remain unchanged
  ExpectListOfFilesPluginInMemFS('Mem:\SimpleTestPlugin.dll', '');
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginArchiveFileToServer_DummyFiles_GetListOfFilesPluginInMemFS;
var
  TestPluginName: string;
  Fnm: string;
begin
  Fnm := ExtractFilePath(ParamStr(0)) + 'TestFiles\TestPlugin\TestPlugin.ppr';
  TestPluginName := ExtractFilePath(ParamStr(0)) + 'TestFiles\TestPlugin\lib\' + GetPluginBitnessDirName + '\TestPlugin.dll';

  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(Fnm, ExtractFileName(Fnm), '', '', '', False, '', False, False, CREResp_ErrResponseOK);
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(TestPluginName, ExtractFileName(TestPluginName), '', '', '', False, '', False, False, CREResp_ErrResponseOK);

  Expect(GetMemPluginInMemFSCount(CTestServerAddress)).ToBe('2', 'FS count');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, -1)).ToBe('Mem:\' + ExtractFileName(Fnm) + #8#7 + 'Mem:\UseKey.txt' + #8#7, 'from main FS');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, 0)).ToBe({'Mem:\' + ExtractFileName(TestPluginName) + #8#7 + 'Mem:\UseKey.txt' + #8#7} CREResp_ErrResponseOK, 'From FS 0'); //allocated for ppr, although the ppr file is extracted in the main FS
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, 1)).ToBe('Mem:\' + ExtractFileName(TestPluginName) + #8#7 + 'Mem:\UseKey.txt' + #8#7, 'From FS 1');

  ExpectListOfFilesPluginInMemFS('Mem:\' + ExtractFileName(TestPluginName), 'Mem:\' + ExtractFileName(Fnm) + #6#8 + 'Mem:\UseKey.txt' + #6#8);
end;


procedure TTestClickerClientMemPlugins.Test_SendMemPluginWithoutArchiveFileToServer_DummyFiles_GetListOfFilesPluginInMemFS;
var
  Fnm: string;
begin
  Fnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  Fnm := ExtractFilePath(ParamStr(0)) + 'TestFiles\TestPlugin\TestPlugin.ppr';

  SendGenericMemPluginFileToServer_HappyFlow(Fnm);

  Expect(GetMemPluginInMemFSCount(CTestServerAddress)).ToBe('1', 'FS count');
  Expect(GetListOfFilesFromMemPluginInMemFS(CTestServerAddress, -1)).ToBe('Mem:\' + ExtractFileName(Fnm) + #8#7, 'from main FS');
end;


procedure TTestClickerClientMemPlugins.Test_SendSameBitnessMemPlugin_HappyFlow;
var
  TestPluginName: string;
  Fnm: string;
  Bitness: string;
begin
  Fnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  Bitness := GetPluginBitnessDirName;
  TestPluginName := ExtractFilePath(ParamStr(0)) + 'TestFiles\TestPlugin\lib\' + Bitness + '\TestPlugin.dll';

  Expect(SetVariable(CTestServerAddress, '$BitnessCfg$', '---', 0)).ToBe(CREResp_Done, 'Init var');
  Expect(GetVarValueFromServer('$BitnessCfg$')).ToBe('---', 'Init var set');

  SendMultiBitnessMemPluginArchiveFileToServer(TestPluginName, ExtractFileName(TestPluginName), 'Outside bitness',
                                               TestPluginName, 'i386-win32\' + ExtractFileName(TestPluginName), '(' + Bitness + ')',
                                               TestPluginName, 'x86_64-win64\' + ExtractFileName(TestPluginName), '(' + Bitness + ')',
                                               '', '', '', False, 'none', True, False, CREResp_ErrResponseOK);

  //Use TestPluginName as decompressor, to run it in cfg mode. It should load the config file:
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(Fnm, ExtractFileName(Fnm), '', ExtractFileName(TestPluginName) + 'arc|Mem:\' + ExtractFileName(TestPluginName), '', True, 'none', False, False, CREResp_ErrResponseOK);
  Expect(GetVarValueFromServer('$BitnessCfg$')).ToBe('Bitness: (' + Bitness + ')', 'Var set by plugin');
end;


procedure TTestClickerClientMemPlugins.Test_SendOutsideBitnessOnlyMemPlugin_HappyFlow;
var
  TestPluginName: string;
  Fnm: string;
  Bitness: string;
begin
  Fnm := Get_FindWindows_PluginPath_RelativeToTestApp;
  Bitness := GetPluginBitnessDirName;
  TestPluginName := ExtractFilePath(ParamStr(0)) + 'TestFiles\TestPlugin\lib\' + Bitness + '\TestPlugin.dll';

  Expect(SetVariable(CTestServerAddress, '$BitnessCfg$', '---', 0)).ToBe(CREResp_Done, 'Init var');
  Expect(GetVarValueFromServer('$BitnessCfg$')).ToBe('---', 'Init var set');

  SendMultiBitnessMemPluginArchiveFileToServer(TestPluginName, ExtractFileName(TestPluginName), 'Outside bitness',
                                               '', '', '',
                                               '', '', '',
                                               '', '', '', False, 'none', True, False, CREResp_ErrResponseOK);

  //Use TestPluginName as decompressor, to run it in cfg mode. It should load the config file:
  SendGenericMemPluginArchiveFileToServer_SinglePlugin_HappyFlow(Fnm, ExtractFileName(Fnm), '', ExtractFileName(TestPluginName) + 'arc|Mem:\' + ExtractFileName(TestPluginName), '', True, 'none', False, False, CREResp_ErrResponseOK);
  Expect(GetVarValueFromServer('$BitnessCfg$')).ToBe('Bitness: Outside bitness', 'Var set by plugin');
end;


procedure TTestClickerClientMemPlugins.Test_SendBothBitnessesMemPlugin_HappyFlow;
var
  TestPluginName, TestPluginName32, TestPluginName64: string;
  Bitness: string;
  PluginOptions: TClkPluginOptions;
begin
  Bitness := GetPluginBitnessDirName;
  TestPluginName := 'NoTestPluginExpectedHere.dll';  //should not exist on disk for this test
  TestPluginName32 := ExtractFilePath(ParamStr(0)) + 'TestFiles\TestPlugin\lib\' + 'i386-win32' + '\TestPlugin.dll';
  TestPluginName64 := ExtractFilePath(ParamStr(0)) + 'TestFiles\TestPlugin\lib\' + 'x86_64-win64' + '\TestPlugin.dll';

  Expect(FileExists(TestPluginName32)).ToBe(True, 'The test expects this file to exist on disk: "' + TestPluginName32 + '".');
  Expect(FileExists(TestPluginName64)).ToBe(True, 'The test expects this file to exist on disk: "' + TestPluginName64 + '".');

  Expect(SetVariable(CTestServerAddress, '$PluginBitnessDir$', '---', 0)).ToBe(CREResp_Done, 'Init var');
  Expect(GetVarValueFromServer('$PluginBitnessDir$')).ToBe('---', 'Init var set');

  SendMultiBitnessMemPluginArchiveFileToServer(TestPluginName, ExtractFileName(TestPluginName), 'Outside bitness',
                                               TestPluginName32, 'i386-win32\' + ExtractFileName(TestPluginName32), '(' + Bitness + ')',
                                               TestPluginName64, 'x86_64-win64\' + ExtractFileName(TestPluginName64), '(' + Bitness + ')',
                                               '', '', '', False, 'none', False, False, CREResp_ErrResponseOK); //set as action plugin this time

  GeneratePluginOptions(PluginOptions, 'Mem:\TestPlugin.dll', '');
  ExpectSuccessfulAction(FastReplace_87ToReturn(ClickerActionsClient.ExecutePluginAction(CTestServerAddress, PluginOptions, True, False, False)));
  Expect(GetVarValueFromServer('$PluginBitnessDir$')).ToBe(Bitness, 'Var set by plugin'); //on ExecutePlugin only
end;


initialization

  RegisterTest(TTestClickerClientMemPlugins);

end.

