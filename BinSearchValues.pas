{
    Copyright (C) 2022 VCC
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


unit BinSearchValues;

interface

type
  TIntArr = array of Integer;
  TIntArrArr = array of TIntArr;

procedure GenerateBinarySearchValuesForImage(ImageWidth, ImageHeight: Integer; var XValues, YValues: TIntArrArr);
procedure GenerateBinarySearchValues(SpaceWidth: Integer; var Values: TIntArrArr);
procedure ClearUsedValues(var Values: TIntArrArr);


implementation


uses
  SysUtils;


procedure GenerateBinarySearchValues(SpaceWidth: Integer; var Values: TIntArrArr);
var
  i, j, n, x: Integer;
  PartExt, HalfExt: Extended;
begin
  PartExt := SpaceWidth;
  SetLength(Values, 1);
  
  for i := 1 to SpaceWidth do
  begin
    HalfExt := PartExt / 2;

    for j := 0 to Round(SpaceWidth / PartExt) do
    begin
      x := Round(HalfExt + j * PartExt);
      if x > SpaceWidth then
        Continue;

      n := Length(Values) - 1; 
      SetLength(Values[n], Length(Values[n]) + 1);
      Values[n][Length(Values[n]) - 1] := x;
    end;

    PartExt := PartExt / 2; //do not use shifting, let it be extended
    if PartExt <= 3 then     //3
      Break;

    SetLength(Values, Length(Values) + 1);  
  end;
end;


procedure ExtendIntArrArr(var Values: TIntArrArr; NewLen: Integer);
var
  i, j, OldLen: Integer;
begin
  OldLen := Length(Values);
  SetLength(Values, NewLen);
  
  for i := OldLen to NewLen - 1 do
  begin
    SetLength(Values[i], Length(Values[OldLen - 1]));
    for j := 0 to Length(Values[i]) - 1 do
      Values[i][j] := Values[OldLen - 1][j];
  end;
end;  


procedure GenerateBinarySearchValuesForImage(ImageWidth, ImageHeight: Integer; var XValues, YValues: TIntArrArr);
begin
  GenerateBinarySearchValues(ImageWidth, XValues);
  GenerateBinarySearchValues(ImageHeight, YValues);

  if Length(XValues) > Length(YValues) then
    ExtendIntArrArr(YValues, Length(XValues))
  else
    if Length(YValues) > Length(XValues) then
      ExtendIntArrArr(XValues, Length(YValues));

  if Length(YValues) <> Length(XValues) then
    raise Exception.Create('Bug in extend');
end;


procedure ClearUsedValues(var Values: TIntArrArr);
var
  i: Integer;
begin
  for i := 0 to Length(Values) - 1 do
    SetLength(Values[i], 0);

  SetLength(Values, 0);
end;


end.
