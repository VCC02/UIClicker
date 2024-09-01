{
    Copyright (C) 2024 VCC
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
  private
    procedure Test_GetDebugImageFromServerWithSizeOption(AFullSize, ABmpFound: Boolean);
  public
    constructor Create; override;
  published
    procedure Test_TestConnection;
    procedure Test_SetVariable_HappyFlow;
    procedure Test_SetVariable_BadStackLevel;

    procedure Test_GetDebugImageFromServer_FullSize_Found;
    procedure Test_GetDebugImageFromServer_MinimumSize_Found;
    procedure Test_GetDebugImageFromServer_FullSize_NotFound;
    procedure Test_GetDebugImageFromServer_MinimumSize_NotFound;

    procedure Test_GetDebugImageFromServer_BadStackIndex;
  end;


implementation


uses
  ClickerActionsClient, ClickerUtils, Controls, Graphics, ActionsStuff;


constructor TTestMiscHTTPAPI.Create;
begin
  inherited Create;
  TestServerAddress := CTestServerAddress;
end;


procedure TTestMiscHTTPAPI.Test_TestConnection;
begin
  Expect(TestConnection(TestServerAddress)).ToBe(CREResp_ConnectionOK);
end;


procedure TTestMiscHTTPAPI.Test_SetVariable_HappyFlow;
begin
  Expect(SetVariable(TestServerAddress, '$MyVar$', 'some value', 0)).ToBe(CREResp_Done);
end;


procedure TTestMiscHTTPAPI.Test_SetVariable_BadStackLevel;
const
  CBadValue = 'some bad value';
var
  Response: string;
  ListOfVars: TStringList;
begin
  Expect(SetVariable(TestServerAddress, '$MyVar$', CBadValue, 10)).ToBe(CExpectedBadStackLevelResponse);

  Response := FastReplace_87ToReturn(GetAllReplacementVars(TestServerAddress, 0));

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := Response;
    Expect(ListOfVars).WithItem('$MyVar$').DifferentThanValue(CBadValue);
  finally
    ListOfVars.Free;
  end;
end;


procedure TTestMiscHTTPAPI.Test_GetDebugImageFromServerWithSizeOption(AFullSize, ABmpFound: Boolean);
var
  Response: string;
  FindSubControlOptions: TClkFindControlOptions;
  Bmp: TBitmap;
begin
  SetupTargetWindowFor_FindSubControl;
  GenerateFindSubControlOptionsForMainUIClickerWindow_Bitness(FindSubControlOptions, False);
  FindSubControlOptions.FullBackgroundImageInResult := AFullSize;

  if not ABmpFound then
    FindSubControlOptions.MatchText := 'some non-existent text';

  ExecuteFindSubControlAction(TestServerAddress, FindSubControlOptions, 'Test GetDebugImageFromServer on UIClicker Main', 3000, CREParam_FileLocation_ValueMem);

  Bmp := TBitmap.Create;
  try
    Response := GetDebugImageFromServer(TestServerAddress, 0, Bmp, False);
    Expect(Response).ToBe('');

    if ABmpFound then
    begin
      if AFullSize then
      begin
        Expect(Bmp.Width).ToBe(400);
        Expect(Bmp.Height).ToBe(279);
      end
      else
      begin
        Expect(Bmp.Width).ToBe(39);
        Expect(Bmp.Height).ToBe(35);
      end;
    end
    else
    begin
      if AFullSize then
      begin
        Expect(Bmp.Width).ToBe(736);
        Expect(Bmp.Height).ToBe(329);
      end
      else
      begin
        Expect(Bmp.Width).ToBeGreaterThanOrEqualTo(487); //Can be greater than, if a template with a long path is already loaded. That template has nothing to do with this result.
        Expect(Bmp.Height).ToBe(330);
      end;
    end;
  finally
    Bmp.Free;
  end;
end;


procedure TTestMiscHTTPAPI.Test_GetDebugImageFromServer_FullSize_Found;
begin
  Test_GetDebugImageFromServerWithSizeOption(True, True);
end;


procedure TTestMiscHTTPAPI.Test_GetDebugImageFromServer_MinimumSize_Found;
begin
  Test_GetDebugImageFromServerWithSizeOption(False, True);
end;


procedure TTestMiscHTTPAPI.Test_GetDebugImageFromServer_FullSize_NotFound;
begin
  Test_GetDebugImageFromServerWithSizeOption(True, False);
end;


procedure TTestMiscHTTPAPI.Test_GetDebugImageFromServer_MinimumSize_NotFound;
begin
  Test_GetDebugImageFromServerWithSizeOption(False, False);
end;


procedure TTestMiscHTTPAPI.Test_GetDebugImageFromServer_BadStackIndex;
var
  Response: string;
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Response := GetDebugImageFromServer(TestServerAddress, 10, Bmp, False);
    Expect(Response).ToBe('');

    Expect(Bmp.Width).ToBeGreaterThan(1000);  //the returned bmp contains a long error message
    Expect(Bmp.Height).ToBeGreaterThan(18);
  finally
    Bmp.Free;
  end;
end;

initialization

  RegisterTest(TTestMiscHTTPAPI);
end.

