{
    Copyright (C) 2026 VCC
    creation date: 13 Jan 2026
    initial release date: 14 Jan 2026

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


unit ClickerActionPluginAccess;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, ImgList, Graphics,
  ClickerActionPlugins, ClickerUtils;


procedure UpdatePropertyIcons(APluginReference: Pointer; AOnActionPlugin_UpdatePropertyIcons: TOnActionPlugin_UpdatePropertyIcons; AIcons: TImageList; APropertiesCount: Integer);

function LoadBitmap(APluginReference: Pointer;
                    AOnActionPlugin_GetAllowedFilesInfo: TOnActionPlugin_GetAllowedFilesInfo;
                    AOnActionPlugin_LoadBitmap: TOnActionPlugin_LoadBitmap;
                    AOnActionPlugin_AddToLog: TOnActionPlugin_AddToLog;
                    AListOfAllVars: TStringList;
                    var AFnm: string;
                    ADestBmp: TBitmap;
                    AFileLocation: TImageSourceFileNameLocation): Boolean;

function LoadBitmapAsStream(APluginReference: Pointer;
                    AOnActionPlugin_GetAllowedFilesInfo: TOnActionPlugin_GetAllowedFilesInfo;
                    AOnActionPlugin_LoadBitmap: TOnActionPlugin_LoadBitmap;
                    AOnActionPlugin_AddToLog: TOnActionPlugin_AddToLog;
                    AListOfAllVars: TStringList;
                    AFnm: string;
                    AMemStream: TMemoryStream;
                    AFileLocation: TImageSourceFileNameLocation): Boolean;

procedure SaveBitmapToExtRenderingInMemFS(APluginReference: Pointer; AOnActionPlugin_SetBitmap: TOnActionPlugin_SetBitmap; ABmp: TBitmap; AFnm: string);
procedure GetActionInfoByIndex(APluginReference: Pointer; AOnActionPlugin_GetActionInfoByIndex: TOnActionPlugin_GetActionInfoByIndex; AIndex: Integer; out AActionName: string; var AActionType: DWord);


implementation


uses
  DllUtils, ClickerFileProviderUtils;


procedure UpdatePropertyIcons(APluginReference: Pointer; AOnActionPlugin_UpdatePropertyIcons: TOnActionPlugin_UpdatePropertyIcons; AIcons: TImageList; APropertiesCount: Integer);
var
  i: Integer;
  TempMemStream: TMemoryStream;
  Bmp: TBitmap;
begin
  for i := 0 to APropertiesCount - 1 do
  begin
    TempMemStream := TMemoryStream.Create;
    Bmp := TBitmap.Create;
    try
      Bmp.PixelFormat := pf24bit;
      Bmp.Width := AIcons.Width;
      Bmp.Height := AIcons.Height;
      Bmp.Canvas.Pen.Color := clWhite;
      Bmp.Canvas.Brush.Color := clWhite;
      Bmp.Canvas.Rectangle(0, 0, Bmp.Width, Bmp.Height);

      AIcons.Draw(Bmp.Canvas, 0, 0, i, dsNormal, itImage);
      Bmp.SaveToStream(TempMemStream);

      AOnActionPlugin_UpdatePropertyIcons(APluginReference, TempMemStream.Memory, TempMemStream.Size);
    finally
      TempMemStream.Free;
      Bmp.Free;
    end;
  end;
end;


function BitmapIsAllowedToBeLoaded(APluginReference: Pointer; AOnActionPlugin_GetAllowedFilesInfo: TOnActionPlugin_GetAllowedFilesInfo; AFnm: string; out ADenyReason: string): Boolean;
var
  TempFileProvider: TFileProvider;
  FullTemplatesDir, AllowedFileDirsForServer, AllowedFileExtensionsForServer: string;
  FullTemplatesDirPtr, AllowedFileDirsForServerPtr, AllowedFileExtensionsForServerPtr: Pointer;
begin
  TempFileProvider := TFileProvider.Create;
  try
    SetLength(FullTemplatesDir, CMaxSharedStringLength + 1);
    SetLength(AllowedFileDirsForServer, CMaxSharedStringLength + 1);
    SetLength(AllowedFileExtensionsForServer, CMaxSharedStringLength + 1);

    FullTemplatesDirPtr := @FullTemplatesDir[1];
    AllowedFileDirsForServerPtr := @AllowedFileDirsForServer[1];
    AllowedFileExtensionsForServerPtr := @AllowedFileExtensionsForServer[1];

    AOnActionPlugin_GetAllowedFilesInfo(APluginReference, FullTemplatesDirPtr, AllowedFileDirsForServerPtr, AllowedFileExtensionsForServerPtr);
    FullTemplatesDir := Copy(FullTemplatesDir, 1, Pos(#0, FullTemplatesDir) - 1);
    AllowedFileDirsForServer := Copy(AllowedFileDirsForServer, 1, Pos(#0, AllowedFileDirsForServer) - 1);
    AllowedFileExtensionsForServer := Copy(AllowedFileExtensionsForServer, 1, Pos(#0, AllowedFileExtensionsForServer) - 1);

    TempFileProvider.FullTemplatesDir := FullTemplatesDir;

    TempFileProvider.AddListOfAccessibleDirs(AllowedFileDirsForServer);
    TempFileProvider.AddListOfAccessibleFileExtensions(AllowedFileExtensionsForServer);

    Result := TempFileProvider.FileIsAllowed(AFnm, ADenyReason);
  finally
    TempFileProvider.Free;
  end;
end;


procedure HandleOnFileContent(ACallReference, AStreamContent: Pointer; AStreamSize: Int64); cdecl;
var
  TempStream: TMemoryStream;
  DestBmp: TBitmap;
begin
  if AStreamSize <= 0 then
    Exit;

  DestBmp := TBitmap(ACallReference); //created by caller

  TempStream := TMemoryStream.Create;
  try
    TempStream.SetSize(AStreamSize);
    Move(AStreamContent^, TempStream.Memory^, AStreamSize);
    DestBmp.LoadFromStream(TempStream, AStreamSize);
  finally
    TempStream.Free;
  end;
end;


procedure HandleOnFileContentAsRawStream(ACallReference, AStreamContent: Pointer; AStreamSize: Int64); cdecl;
var
  DestMemStream: TMemoryStream;
begin
  if AStreamSize <= 0 then
    Exit;

  DestMemStream := TMemoryStream(ACallReference); //created by caller
  DestMemStream.SetSize(AStreamSize);
  Move(AStreamContent^, DestMemStream.Memory^, AStreamSize);
end;


function LoadBitmap(APluginReference: Pointer;
                    AOnActionPlugin_GetAllowedFilesInfo: TOnActionPlugin_GetAllowedFilesInfo;
                    AOnActionPlugin_LoadBitmap: TOnActionPlugin_LoadBitmap;
                    AOnActionPlugin_AddToLog: TOnActionPlugin_AddToLog;
                    AListOfAllVars: TStringList;
                    var AFnm: string;
                    ADestBmp: TBitmap;
                    AFileLocation: TImageSourceFileNameLocation): Boolean;
var
  TempDenyReason, ErrMsg: string;
begin
  AFnm := EvaluateAllReplacements(AListOfAllVars, AFnm);

  if BitmapIsAllowedToBeLoaded(APluginReference, AOnActionPlugin_GetAllowedFilesInfo, AFnm, TempDenyReason) then                                      //isflDisk=0, isflMem=1
    Result := AOnActionPlugin_LoadBitmap(APluginReference, @AFnm[1], ADestBmp, Ord(AFileLocation), @HandleOnFileContent)
  else
  begin
    ErrMsg := 'Error: File is not allowed to be loaded from ' + AFnm;
    AOnActionPlugin_AddToLog(APluginReference, @ErrMsg[1]);
    Result := False;
  end;
end;


function LoadBitmapAsStream(APluginReference: Pointer;
                    AOnActionPlugin_GetAllowedFilesInfo: TOnActionPlugin_GetAllowedFilesInfo;
                    AOnActionPlugin_LoadBitmap: TOnActionPlugin_LoadBitmap;
                    AOnActionPlugin_AddToLog: TOnActionPlugin_AddToLog;
                    AListOfAllVars: TStringList;
                    AFnm: string;
                    AMemStream: TMemoryStream;
                    AFileLocation: TImageSourceFileNameLocation): Boolean;
var
  TempDenyReason, ErrMsg: string;
  IsPmtv: Byte;
begin
  AFnm := EvaluateAllReplacements(AListOfAllVars, AFnm);

  if BitmapIsAllowedToBeLoaded(APluginReference, AOnActionPlugin_GetAllowedFilesInfo, AFnm, TempDenyReason) then                                      //isflDisk=0, isflMem=1
  begin
    IsPmtv := Ord(UpperCase(ExtractFileExt(AFnm)) = '.PMTV') shl 3;  //use bit 3 for "DoNotRender" flag, to reserve bits 2 and 1 for future use.
    Result := AOnActionPlugin_LoadBitmap(APluginReference, @AFnm[1], AMemStream, Ord(AFileLocation) or IsPmtv, @HandleOnFileContentAsRawStream)
  end
  else
  begin
    ErrMsg := 'Bitmap/pmtv is not allowed to be loaded: "' + AFnm + '"   ' + TempDenyReason + '   ImgSrc: ' + CImageSourceFileNameLocationStr[AFileLocation];
    AOnActionPlugin_AddToLog(APluginReference, @ErrMsg[1]);
    Result := False;
  end;
end;


procedure SaveBitmapToExtRenderingInMemFS(APluginReference: Pointer; AOnActionPlugin_SetBitmap: TOnActionPlugin_SetBitmap; ABmp: TBitmap; AFnm: string);
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    ABmp.SaveToStream(MemStream);

    AOnActionPlugin_SetBitmap(APluginReference, @AFnm[1], MemStream.Memory, MemStream.Size, ABmp.Width, ABmp.Height);
  finally
    MemStream.Free;
  end;
end;


procedure GetActionInfoByIndex(APluginReference: Pointer; AOnActionPlugin_GetActionInfoByIndex: TOnActionPlugin_GetActionInfoByIndex; AIndex: Integer; out AActionName: string; var AActionType: DWord);
var
  Buffer: string;
  ActionNamePtr: Pointer;
  NameLengthDWord: DWord;
begin
  SetLength(Buffer, CMaxSharedStringLength + 1);
  ActionNamePtr := @Buffer[1];
  NameLengthDWord := 0;

  AActionName := '';
  AOnActionPlugin_GetActionInfoByIndex(APluginReference, AIndex, ActionNamePtr, @NameLengthDWord, @AActionType);
  SetPointedContentToString(ActionNamePtr, AActionName);
  SetLength(AActionName, NameLengthDWord);
end;

end.

