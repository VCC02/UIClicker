{
    Copyright (C) 2024 VCC
    creation date: 24 May 2024
    initial release date: 25 May 2024

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


unit ClickerVstUtils;

{$mode Delphi}

interface

uses
  Classes, SysUtils, VirtualTrees;


function GetNodeByIndex(AVst: TVirtualStringTree; AIndex: Integer): PVirtualNode;
procedure SelectNodeByIndex(AVst: TVirtualStringTree; AIndex: Integer; AScrollIntoView: Boolean = False; ACenter: Boolean = False);


implementation


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

