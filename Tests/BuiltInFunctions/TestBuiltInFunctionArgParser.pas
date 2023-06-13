{
    Copyright (C) 2023 VCC
    creation date: Jun 2019
    initial release date: 13 Jun 2023

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


unit TestBuiltInFunctionArgParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type

  //using functions like '$FastReplace_45ToReturn(<arg>)$' to test the argument parser, since the parser itself is not exposed
  TTestBuiltInFunctionArgParser = class(TTestCase)
  private
    FVars: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNoBracketsWith_45ToReturn;
    procedure TestNoBracketsWith_ReturnTo45;
    procedure TestNoBracketsWith_Diff;

    procedure TestNoBrackets_NestedReturnTo45_CRLF;
    procedure TestNoBrackets_Nested45ToReturn_CRLF;
    procedure TestNoBrackets_NestedReturnTo45_45;
    procedure TestNoBrackets_Nested45ToReturn_45;

    procedure TestWithLeftBracket;
    procedure TestWithRightBracket;
  end;


implementation


uses
  ClickerUtils, Expectations;


procedure TTestBuiltInFunctionArgParser.SetUp;
begin
  FVars := TStringList.Create;
end;


procedure TTestBuiltInFunctionArgParser.TearDown;
begin
  FVars.Free;
end;


procedure TTestBuiltInFunctionArgParser.TestNoBracketsWith_45ToReturn;
begin
  FVars.Add('$abc$=12'#4#5'34'#4#5'56');
  Expect(EvaluateAllReplacements(FVars, '$FastReplace_45ToReturn($abc$)$')).ToBe('12'#13#10'34'#13#10'56');
end;


procedure TTestBuiltInFunctionArgParser.TestNoBracketsWith_ReturnTo45;
begin
  FVars.Add('$abc$=12$CRLF$34$CRLF$56');
  Expect(EvaluateAllReplacements(FVars, '$FastReplace_ReturnTo45($abc$)$')).ToBe('12'#4#5'34'#4#5'56');
end;


procedure TTestBuiltInFunctionArgParser.TestNoBracketsWith_Diff;
begin
  FVars.Add('$abc$=15');
  FVars.Add('$def$=7');
  Expect(EvaluateAllReplacements(FVars, '$Diff($abc$, $def$)$')).ToBe('8');
end;


procedure TTestBuiltInFunctionArgParser.TestNoBrackets_NestedReturnTo45_CRLF;
const
  CArg = '12$CRLF$34$CRLF$56';
begin
  Expect(EvaluateAllReplacements(FVars, '$FastReplace_45ToReturn($FastReplace_ReturnTo45(' + CArg + ')$)$')).ToBe('12'#13#10'34'#13#10'56');
end;


procedure TTestBuiltInFunctionArgParser.TestNoBrackets_Nested45ToReturn_CRLF;
const
  CArg = '12$CRLF$34$CRLF$56';
begin
  Expect(EvaluateAllReplacements(FVars, '$FastReplace_ReturnTo45($FastReplace_45ToReturn(' + CArg + ')$)$')).ToBe('12'#13#10'34'#13#10'56');
end;


procedure TTestBuiltInFunctionArgParser.TestNoBrackets_NestedReturnTo45_45;
const
  CArg = '12$#4#5$34$#4#5$56';
begin
  Expect(EvaluateAllReplacements(FVars, '$FastReplace_45ToReturn($FastReplace_ReturnTo45(' + CArg + ')$)$')).ToBe('12'#4#5'34'#4#5'56');
end;


procedure TTestBuiltInFunctionArgParser.TestNoBrackets_Nested45ToReturn_45;
const
  CArg = '12$#4#5$34$#4#5$56';
begin
  Expect(EvaluateAllReplacements(FVars, '$FastReplace_ReturnTo45($FastReplace_45ToReturn(' + CArg + ')$)$')).ToBe('12'#4#5'34'#4#5'56');
end;


procedure TTestBuiltInFunctionArgParser.TestWithLeftBracket;
begin
  FVars.Add('$abc$=12'#4#5'3(4'#4#5'56');
  Expect(EvaluateAllReplacements(FVars, '$FastReplace_45ToReturn($abc$)$')).ToBe('12'#13#10'3(4'#13#10'56');
end;


procedure TTestBuiltInFunctionArgParser.TestWithRightBracket;
begin
  FVars.Add('$abc$=12'#4#5'3)4'#4#5'56');
  Expect(EvaluateAllReplacements(FVars, '$FastReplace_45ToReturn($abc$)$')).ToBe('12'#13#10'3)4'#13#10'56');
end;


initialization

  RegisterTest(TTestBuiltInFunctionArgParser);
end.

