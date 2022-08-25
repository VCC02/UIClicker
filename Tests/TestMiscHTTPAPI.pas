{
    Copyright (C) 2022 VCC
    creation date: Aug 2022
    initial release date: 25 Aug 2022

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


unit TestMiscHTTPAPI;

{$mode ObjFPC}{$H+}

interface

uses
  LCLIntf, Classes, SysUtils, TestHTTPAPI, fpcunit, testregistry, Expectations;


type

  TTestMiscHTTPAPI = class(TTestHTTPAPI)
  published
    procedure Test_TestConnection;
    procedure Test_SetVariable_HappyFlow;
    procedure Test_SetVariable_BadStackLevel;
  end;


implementation


uses
  ClickerActionsClient, ClickerUtils, Controls;


procedure TTestMiscHTTPAPI.Test_TestConnection;
begin
  Expect(TestConnection(CTestServerAddress)).ToBe(CREResp_ConnectionOK);
end;


procedure TTestMiscHTTPAPI.Test_SetVariable_HappyFlow;
begin
  Expect(SetVariable(CTestServerAddress, '$MyVar$', 'some value', 0)).ToBe(CREResp_Done);
end;


procedure TTestMiscHTTPAPI.Test_SetVariable_BadStackLevel;
const
  CBadValue = 'some bad value';
var
  Response: string;
  ListOfVars: TStringList;
begin
  Expect(SetVariable(CTestServerAddress, '$MyVar$', CBadValue, 10)).ToBe(CExpectedBadStackLevelResponse);

  Response := FastReplace_87ToReturn(GetAllReplacementVars(CTestServerAddress, 0));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    Expect(ListOfVars).WithItem('$MyVar$').DifferentThanValue(CBadValue);
  finally
    ListOfVars.Free;
  end;
end;


initialization

  RegisterTest(TTestMiscHTTPAPI);
end.

