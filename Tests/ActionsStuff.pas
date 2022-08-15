{
    Copyright (C) 2022 VCC
    creation date: Aug 2022
    initial release date: 14 Aug 2022

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


unit ActionsStuff;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ClickerUtils, InMemFileSystem;


function AddClickActionToTemplate(ATemplateFileName: string;

                                  AActionName: string;
                                  AActionTimeout: LongInt; //ms
                                  AActionEnabled: Boolean;
                                  AActionCondition: string;

                                  AClickOptions: TClkClickOptions;
                                  AInMemFS: TInMemFileSystem
                                  ): LongInt;


procedure GetDefaultClickOptions(var AClickOptions: TClkClickOptions);


implementation


uses
  Controls, ClickerTemplates;


procedure AddActionToTemplate(ATemplateFileName: string; AClkAction: TClkActionRec; AInMemFS: TInMemFileSystem);
var
  ClkActions: TClkActionsRecArr;
  MemStream: TMemoryStream;
  n: Integer;
begin
  if not AInMemFS.FileExistsInMem(ATemplateFileName) then
    AInMemFS.SaveFileToMem(ATemplateFileName, nil, 0);

  MemStream := TMemoryStream.Create;
  try
    AInMemFS.LoadFileFromMemToStream(ATemplateFileName, MemStream);
    MemStream.Position := 0;

    GetTemplateContentFromMemoryStream(ClkActions, MemStream);

    n := Length(ClkActions);
    SetLength(ClkActions, n + 1);
    CopyActionContent(AClkAction, ClkActions[n]);

    MemStream.Clear;
    GetTemplateContentAsMemoryStream(ClkActions, MemStream);
    AInMemFS.SaveFileToMem(ATemplateFileName, MemStream.Memory, MemStream.Size);
  finally
    MemStream.Free;
  end;
end;


procedure SetBasicActionOptions(ATemplateFileName: string;
                                AActionName: string;
                                AAction: TClkAction;
                                AActionTimeout: LongInt; //ms
                                AActionEnabled: Boolean;
                                AActionCondition: string;
                                var ATempAction: TClkActionRec);
begin
  ATempAction.ActionOptions.ActionName := AActionName;
  ATempAction.ActionOptions.Action := AAction;
  ATempAction.ActionOptions.ActionTimeout := AActionTimeout;
  ATempAction.ActionOptions.ActionEnabled := AActionEnabled;
  ATempAction.ActionOptions.ActionCondition := AActionCondition;
end;


function AddClickActionToTemplate(ATemplateFileName: string;

                                  AActionName: string;
                                  AActionTimeout: LongInt; //ms
                                  AActionEnabled: Boolean;
                                  AActionCondition: string;

                                  AClickOptions: TClkClickOptions;
                                  AInMemFS: TInMemFileSystem
                                  ): LongInt;
var
  TempAction: TClkActionRec;
begin
  SetBasicActionOptions(ATemplateFileName, AActionName, acClick, AActionTimeout, AActionEnabled, AActionCondition, TempAction);

  //click stuff
  TempAction.ClickOptions := AClickOptions;

  AddActionToTemplate(ATemplateFileName, TempAction, AInMemFS);
  Result := 0;
end;


procedure GetDefaultClickOptions(var AClickOptions: TClkClickOptions);
begin
  AClickOptions.XClickPointReference := xrefLeft;
  AClickOptions.YClickPointReference := yrefTop;
  AClickOptions.XClickPointVar := '$Control_Left$';
  AClickOptions.YClickPointVar := '$Control_Top$';
  AClickOptions.XOffset := '';
  AClickOptions.YOffset := '';
  AClickOptions.MouseButton := mbLeft;
  AClickOptions.ClickWithCtrl := False;
  AClickOptions.ClickWithAlt := False;
  AClickOptions.ClickWithShift := False;
  AClickOptions.ClickWithDoubleClick := False;
  AClickOptions.Count := 1;
  AClickOptions.LeaveMouse := False;
  AClickOptions.MoveWithoutClick := False;
  AClickOptions.ClickType := CClickType_Click;    //see CClickType_Click and CClickType_Drag
  AClickOptions.XClickPointReferenceDest := xrefLeft;
  AClickOptions.YClickPointReferenceDest := yrefTop;
  AClickOptions.XClickPointVarDest := '$Control_Left$';
  AClickOptions.YClickPointVarDest := '$Control_Top$';
  AClickOptions.XOffsetDest := '';
  AClickOptions.YOffsetDest := '';
end;

end.

