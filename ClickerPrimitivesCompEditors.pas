{
    Copyright (C) 2023 VCC
    creation date: Jun 2023
    initial release date: 08 Jun 2023

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


unit ClickerPrimitivesCompEditors;

{$mode Delphi}

interface

uses
  Classes, SysUtils, StdCtrls;


type
  TLabelArr = array of TLabel;

  TPrimitive_LineEditors = class
  private
    Endpoint1: TLabel;
    Endpoint2: TLabel;
  end;


  TPrimitive_RectangleEditors = class
  private
    Endpoint_TopL: TLabel;
    Endpoint_TopR: TLabel;
    Endpoint_BotL: TLabel;
    Endpoint_BotR: TLabel;

    Endpoint_L: TLabel;
    Endpoint_T: TLabel;
    Endpoint_R: TLabel;
    Endpoint_B: TLabel;
  end;


  TPrimitive_BezierEditors = class
  private
    Endpoints: TLabelArr;
  end;

  //The text primitive is not resizable, so there is no editor for it.  The text itself can be dragged, but that's all.

implementation

{
- Every primitive should have its own editor, which is a class, containing the editpoints (TLabel objects, used for dragging)
  They have to be primitive-specific, because each primitive has its own vertices (a line has 2, a rectangle has 8, a bezier curve has many)
- Each edit point has to be specifically assigned to edit one part of the primitive, so there is little generic (sharable) code
- Edit points cannot be stored into an array, except for bezier or polygons


}

end.

