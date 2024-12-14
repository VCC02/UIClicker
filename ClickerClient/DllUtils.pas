{
    Copyright (C) 2022 VCC
    creation date: Jul 2022
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

unit DllUtils;

{$mode Delphi}

interface

uses
  Classes, SysUtils;


const
  CMaxSharedStringLength = 10 * 1048576; //10MB

var
  ClientLastError: string;


function SetPointedContentFromString(ASrc: string; ADest: Pointer; AMaxLen: Integer = CMaxSharedStringLength): Integer;
procedure SetPointedContentToString(ASrc: Pointer; var ADest: string);


implementation


uses
  Math;


function SetPointedContentFromString(ASrc: string; ADest: Pointer; AMaxLen: Integer = CMaxSharedStringLength): Integer;
begin
  if Length(ASrc) > AMaxLen then
  begin
    ASrc := 'Can''t set the string over its allocated size. Please increase CMaxSharedStringLength to at least ' + IntToStr(Length(ASrc)) + ', or simply pass a greater value.';
    ClientLastError := ASrc;
  end;

  Result := Length(ASrc);
  if Result > AMaxLen then
    Result := AMaxLen;

  if ASrc = '' then
    ASrc := '0';   //prevent copying from nil

  Move(ASrc[1], ADest^, Result + 1); //use + 1, to copy the null terminating character
end;


procedure SetPointedContentToString(ASrc: Pointer; var ADest: string);
begin
  if ASrc = nil then
  begin
    ADest := '';
    Exit;
  end;

  SetLength(ADest, Min(StrLen(ASrc), CMaxSharedStringLength));
  Move(ASrc^, ADest[1], Length(ADest));
end;

end.

