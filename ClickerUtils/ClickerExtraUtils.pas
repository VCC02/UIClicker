{
    Copyright (C) 2024 VCC
    creation date: 14 Jan 2024
    initial release date: 15 Jan 2024

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


unit ClickerExtraUtils;

{$H+}
{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  IdGlobal, DCPmd5, VirtualTrees;


function ArrOfByteToHex(var AArr: TIdBytes): string;
function ComputeHash(AFileContent: Pointer; AFileSize: Int64): string;
function GetFileHash(AFileName: string): string;
function GetNodeByIndex(AVst: TVirtualStringTree; AIndex: Integer): PVirtualNode;
procedure SelectNodeByIndex(AVst: TVirtualStringTree; AIndex: Integer; AScrollIntoView: Boolean = False; ACenter: Boolean = False);


implementation


function ArrOfByteToHex(var AArr: TIdBytes): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(AArr) - 1 do
    Result := Result + IntToHex(AArr[i], 2);
end;


function ComputeHash(AFileContent: Pointer; AFileSize: Int64): string;
var
  DCP_md5: TDCP_md5;
  BinHash: TIdBytes;
begin
  DCP_md5 := TDCP_md5.Create(nil);
  try
    SetLength(BinHash, 20);
    try
      DCP_md5.Init;
      DCP_md5.Update(AFileContent^, AFileSize);
      DCP_md5.Final(BinHash[0]);

      SetLength(BinHash, 16);
      Result := ArrOfByteToHex(BinHash);
    finally
      SetLength(BinHash, 0);
    end;
  finally
    DCP_md5.Free;
  end;
end;


function GetFileHash(AFileName: string): string;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(AFileName);
    Result := ComputeHash(Stream.Memory, Stream.Size);
  finally
    Stream.Free;
  end;
end;


function GetNodeByIndex(AVst: TVirtualStringTree; AIndex: Integer): PVirtualNode;
var
  Node: PVirtualNode;
begin
  Result := nil;
  Node := AVst.GetFirst;
  if Node = nil then
    Exit;

  repeat
    if Integer(Node^.Index) = AIndex then
    begin
      Result := Node;
      Break;
    end;

    Node := Node^.NextSibling;
  until Node = nil;
end;


procedure SelectNodeByIndex(AVst: TVirtualStringTree; AIndex: Integer; AScrollIntoView: Boolean = False; ACenter: Boolean = False);
var
  Node: PVirtualNode;
begin
  Node := GetNodeByIndex(AVst, AIndex);
  if Node = nil then
    Exit;

  AVst.Selected[Node] := True;

  if AScrollIntoView then
    AVst.ScrollIntoView(Node, ACenter);
end;

end.

