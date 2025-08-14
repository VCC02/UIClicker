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

implementation


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

end.

