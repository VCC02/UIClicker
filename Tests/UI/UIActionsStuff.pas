{
    Copyright (C) 2024 VCC
    creation date: Oct 2022
    initial release date: 30 Oct 2022

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


unit UIActionsStuff;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ClickerUtils, InMemFileSystem, ClickerActionValues;

type
  TOIInteractionData = record
    BasicPropertyInfo: TOIPropDef;   //name and editor type
    //PropertyName: string;
    PropertyNamesToExpand: string; //0 = do not expand, it doesn't have subproperties.  1 = should expand, to access subproperties
    PropertyValue: string; //what to expect, or what to set
    //EditorType: string; //how to interact with the OI editor for that particular property
  end;

  TOIInteractionDataArr = array of TOIInteractionData;


procedure GenerateExecAppOptionsForTestDriver(var AExecAppOptions: TClkExecAppOptions; AExeArgs: string);
procedure LoadTemplateFromDiskIntoInMemFS(ATemplateName: string; AInMemFS: TInMemFileSystem);

procedure ListOfSerializedPropertiesToOIInteractionData(AListOfSerProps: string; ABasicPropInfo: PArrayOfProperties; APropIsXP: PArrayOfEnumCounts; APropCnt: Integer; var AProperties: TOIInteractionDataArr);

procedure StartWebBrowserForTextRendering(AUIClickerAddress: string);
procedure FindSubControl_With_ShellExecute_RenderingRequest(AUIClickerAddress: string);
procedure CloseWebBrowserForTextRendering(AUIClickerAddress: string);


implementation


uses
  ClickerActionProperties, TestHTTPAPI, ClickerActionsClient;

procedure GenerateExecAppOptionsForTestDriver(var AExecAppOptions: TClkExecAppOptions; AExeArgs: string);
begin
  AExecAppOptions.PathToApp := ExtractFilePath(ParamStr(0)) + '..\..\TestDriver\UIClicker.exe';
  AExecAppOptions.ListOfParams := AExeArgs;
  AExecAppOptions.CurrentDir := ExtractFileDir(AExecAppOptions.PathToApp);
  AExecAppOptions.UseInheritHandles := uihNo;
  AExecAppOptions.WaitForApp := False;
  AExecAppOptions.NoConsole := True;
  AExecAppOptions.VerifyFileExistence := True;
end;


procedure LoadTemplateFromDiskIntoInMemFS(ATemplateName: string; AInMemFS: TInMemFileSystem);
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    MemStream.LoadFromFile(ATemplateName);
    AInMemFS.SaveFileToMem(ATemplateName, MemStream.Memory, MemStream.Size);
  finally
    MemStream.Free;
  end;
end;


procedure ListOfSerializedPropertiesToOIInteractionData(AListOfSerProps: string; ABasicPropInfo: PArrayOfProperties; APropIsXP: PArrayOfEnumCounts; APropCnt: Integer; var AProperties: TOIInteractionDataArr);
var
  ListOfSerProps: TStringList;   //this contains default values
  i: Integer;
begin
  ListOfSerProps := TStringList.Create;
  try
    ListOfSerProps.Text := StringReplace(AListOfSerProps, '&', #13#10, [rfReplaceAll]);

    SetLength(AProperties, APropCnt);
    for i := 0 to APropCnt - 1 do
    begin
      AProperties[i].BasicPropertyInfo := ABasicPropInfo^[i];
      AProperties[i].PropertyNamesToExpand := IntToStr(APropIsXP^[i]);
      AProperties[i].PropertyValue := ListOfSerProps.Values[AProperties[i].BasicPropertyInfo.Name];
    end;
  finally
    ListOfSerProps.Free;
  end;
end;


procedure FindWebBrowserForTextRendering(AUIClickerAddress: string);
var
  FindControlOptions: TClkFindControlOptions;
begin
  GetDefaultPropertyValues_FindControl(FindControlOptions);
  FindControlOptions.MatchText := 'Mozilla Firefox';
  FindControlOptions.MatchClassName := 'MozillaWindowClass';
  ExpectSuccessfulAction(FastReplace_87ToReturn(ExecuteFindControlAction(AUIClickerAddress, FindControlOptions, 'Find FF', 3000, CREParam_FileLocation_ValueMem)));
end;


procedure StartWebBrowserForTextRendering(AUIClickerAddress: string);
var
  ExecAppOptions: TClkExecAppOptions;
  SleepOptions: TClkSleepOptions;
begin
  GetDefaultPropertyValues_ExecApp(ExecAppOptions);
  ExecAppOptions.PathToApp := 'explorer';
  ExecAppOptions.ListOfParams := 'http://';
  ExpectSuccessfulAction(FastReplace_87ToReturn(ExecuteExecAppAction(AUIClickerAddress, ExecAppOptions, 'Start browser', 1000)));

  GetDefaultPropertyValues_Sleep(SleepOptions);
  ExpectSuccessfulAction(FastReplace_87ToReturn(ExecuteSleepAction(AUIClickerAddress, SleepOptions, 'Sleep')));

  FindWebBrowserForTextRendering(AUIClickerAddress);
end;


procedure CloseWebBrowserForTextRendering(AUIClickerAddress: string);
var
  WindowOperationsOptions: TClkWindowOperationsOptions;
begin
  FindWebBrowserForTextRendering(AUIClickerAddress);

  GetDefaultPropertyValues_WindowOperations(WindowOperationsOptions);
  WindowOperationsOptions.Operation := TWindowOperation(2);
  ExpectSuccessfulAction(FastReplace_87ToReturn(ExecuteWindowOperationsAction(AUIClickerAddress, WindowOperationsOptions)));
end;


procedure FindSubControl_With_ShellExecute_RenderingRequest(AUIClickerAddress: string);
var
  FindSubControlOptions: TClkFindSubControlOptions;
begin
  GetDefaultPropertyValues_FindSubControl(FindSubControlOptions);
  FindSubControlOptions.MatchText := 'The quick brown fox bdqW.';
  SetLength(FindSubControlOptions.MatchBitmapText, 4);
  FindSubControlOptions.MatchBitmapText[0].FontSize := 15;
  FindSubControlOptions.MatchBitmapText[0].CropLeft := '7';
  FindSubControlOptions.MatchBitmapText[0].CropTop := '8';
  FindSubControlOptions.MatchBitmapText[0].CropRight := '9';
  FindSubControlOptions.MatchBitmapText[0].CropBottom := '10';
  FindSubControlOptions.MatchBitmapText[1].ForegroundColor := '000080';
  FindSubControlOptions.MatchBitmapText[1].BackgroundColor := '008000';
  FindSubControlOptions.MatchBitmapText[1].FontName := 'Segoe UI';
  FindSubControlOptions.MatchBitmapText[1].FontSize := 20;
  FindSubControlOptions.MatchBitmapText[1].Italic := True;
  FindSubControlOptions.MatchBitmapText[1].ProfileName := 'Profile [1]';
  FindSubControlOptions.MatchBitmapText[2].ForegroundColor := '808000';
  FindSubControlOptions.MatchBitmapText[2].BackgroundColor := 'FFFF00';
  FindSubControlOptions.MatchBitmapText[2].FontName := 'Segoe UI';
  FindSubControlOptions.MatchBitmapText[2].FontSize := 9;
  FindSubControlOptions.MatchBitmapText[2].Bold := True;
  FindSubControlOptions.MatchBitmapText[2].ProfileName := 'Profile [2]';
  FindSubControlOptions.MatchBitmapText[3].ForegroundColor := '008000';
  FindSubControlOptions.MatchBitmapText[3].BackgroundColor := '00FFFF';
  FindSubControlOptions.MatchBitmapText[3].FontName := 'Courier New';
  FindSubControlOptions.MatchBitmapText[3].FontSize := 60;
  FindSubControlOptions.MatchBitmapText[3].ProfileName := 'Profile [3]';
  FindSubControlOptions.UseTextRenderingInBrowser := True;
  FindSubControlOptions.RenderingInBrowserSettings.ReceivingBitmapsTimeout := 3010;

  ExpectSuccessfulAction(FastReplace_87ToReturn(ExecuteFindSubControlAction(AUIClickerAddress, FindSubControlOptions, 'FindSubControl', 1000, CREParam_FileLocation_ValueMem)));
end;



end.

