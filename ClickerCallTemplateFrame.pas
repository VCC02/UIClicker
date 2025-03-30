{
    Copyright (C) 2022 VCC
    creation date: Feb 2023
    initial release date: 13 Feb 2023

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


unit ClickerCallTemplateFrame;

{$mode Delphi}

interface

uses
  {$IFDEF Windows}
    Windows,
  {$ELSE}
    LCLIntf, LCLType,
  {$ENDIF}
  Classes, SysUtils, Forms, Controls, StdCtrls, Menus, ExtCtrls,
  ValEdit, Buttons, VirtualTrees, ClickerUtils, Graphics;

type

  { TfrClickerCallTemplate }

  TfrClickerCallTemplate = class(TFrame)
    AddCustomVarRow1: TMenuItem;
    lblCustomUserVarsBeforeCall: TLabel;
    lblSetVarWarning: TLabel;
    pmCustomVars: TPopupMenu;
    RemoveCustomVarRow1: TMenuItem;
    spdbtnMoveDown: TSpeedButton;
    spdbtnMoveUp: TSpeedButton;
    spdbtnNewVariable: TSpeedButton;
    spdbtnRemoveSelectedVariable: TSpeedButton;
    tmrEditCustomVars: TTimer;
    vallstCustomVariables: TValueListEditor;
    vstCustomVariables: TVirtualStringTree;
    procedure AddCustomVarRow1Click(Sender: TObject);
    procedure RemoveCustomVarRow1Click(Sender: TObject);
    procedure spdbtnMoveDownClick(Sender: TObject);
    procedure spdbtnMoveUpClick(Sender: TObject);
    procedure spdbtnNewVariableClick(Sender: TObject);
    procedure spdbtnRemoveSelectedVariableClick(Sender: TObject);
    procedure tmrEditCustomVarsTimer(Sender: TObject);
    procedure vstCustomVariablesDblClick(Sender: TObject);
    procedure vstCustomVariablesEdited(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vstCustomVariablesEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstCustomVariablesGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure vstCustomVariablesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure vstCustomVariablesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vstCustomVariablesNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
    procedure vstCustomVariablesPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
  private
    FCustomVarsMouseUpHitInfo: THitInfo;
    FCustomVarsEditingText: string;
    FCustomVarsUpdatedVstText: Boolean;

    FOnTriggerOnControlsModified: TOnTriggerOnControlsModified;

    procedure DoOnTriggerOnControlsModified;

    procedure AddNewVariable;
    procedure RemoveVar;
  public
    constructor Create(AOwner: TComponent); override;

    function GetListOfCustomVariables: string;
    procedure SetListOfCustomVariables(Value: string);

    property OnTriggerOnControlsModified: TOnTriggerOnControlsModified write FOnTriggerOnControlsModified;
  end;


implementation

{$R *.frm}

{ TfrClickerCallTemplate }


constructor TfrClickerCallTemplate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOnTriggerOnControlsModified := nil;
end;


procedure TfrClickerCallTemplate.DoOnTriggerOnControlsModified;
begin
  if not Assigned(FOnTriggerOnControlsModified) then
    raise Exception.Create('OnTriggerOnControlsModified not assigned.')
  else
    FOnTriggerOnControlsModified;
end;


function TfrClickerCallTemplate.GetListOfCustomVariables: string;
begin
  Result := vallstCustomVariables.Strings.Text;
end;


procedure TfrClickerCallTemplate.SetListOfCustomVariables(Value: string);
begin
  //if vallstCustomVariables.Strings.Text <> Value then  //For some reason, this has to stay commented. Otherwise, it won't update the tree.
  begin
    vallstCustomVariables.Strings.Text := Value;
    vstCustomVariables.RootNodeCount := vallstCustomVariables.Strings.Count;
    vstCustomVariables.Repaint;
  end;
end;


procedure TfrClickerCallTemplate.tmrEditCustomVarsTimer(Sender: TObject);
begin
  tmrEditCustomVars.Enabled := False;

  if FCustomVarsMouseUpHitInfo.HitNode = nil then
    Exit;

  vstCustomVariables.EditNode(FCustomVarsMouseUpHitInfo.HitNode, FCustomVarsMouseUpHitInfo.HitColumn);
end;


procedure TfrClickerCallTemplate.AddNewVariable;
begin
  vallstCustomVariables.Strings.Add('');
  vstCustomVariables.RootNodeCount := vallstCustomVariables.Strings.Count;
  vstCustomVariables.Repaint;

  vstCustomVariables.Selected[vstCustomVariables.GetLast] := True;

  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerCallTemplate.RemoveVar;
var
  Node: PVirtualNode;
begin
  try
    //if MessageBox(Handle, PChar('Remove variable?' + #13#10 + vallstCustomVariables.Strings[vallstCustomVariables.Selection.Top - 1]), 'Selection', MB_ICONQUESTION + MB_YESNO) = IDNO then
    //  Exit;

    Node := vstCustomVariables.GetFirstSelected;

    if Node = nil then
    begin
      MessageBox(Handle, 'Please select a variable to be deleted.', PChar(Application.Title), MB_ICONINFORMATION);
      Exit;
    end;

    if MessageBox(Handle, PChar('Remove variable?' + #13#10 + vallstCustomVariables.Strings[Node^.Index]), 'Selection', MB_ICONQUESTION + MB_YESNO) = IDNO then
      Exit;

    //vallstCustomVariables.Strings.Delete(vallstCustomVariables.Selection.Top - 1);
    vallstCustomVariables.Strings.Delete(Node^.Index);

    vstCustomVariables.RootNodeCount := vallstCustomVariables.Strings.Count;
    vstCustomVariables.Repaint;
    DoOnTriggerOnControlsModified;
  except
  end;
end;


procedure TfrClickerCallTemplate.AddCustomVarRow1Click(Sender: TObject);
begin
  AddNewVariable;
end;


procedure TfrClickerCallTemplate.RemoveCustomVarRow1Click(Sender: TObject);
begin
  RemoveVar;
end;


procedure TfrClickerCallTemplate.spdbtnMoveUpClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstCustomVariables.GetFirstSelected;
  if (Node = nil) or (Node = vstCustomVariables.GetFirst) or (vstCustomVariables.RootNodeCount = 1) then
    Exit;

  vallstCustomVariables.Strings.Move(Node^.Index, Node^.Index - 1);

  //UpdateNodeCheckStateFromEvalBefore(Node^.PrevSibling);   //uncomment these if there will ever be individual evaluations
  //UpdateNodeCheckStateFromEvalBefore(Node);

  vstCustomVariables.ClearSelection;
  vstCustomVariables.Selected[Node^.PrevSibling] := True;
  vstCustomVariables.ScrollIntoView(Node, True);
  vstCustomVariables.Repaint;
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerCallTemplate.spdbtnMoveDownClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstCustomVariables.GetFirstSelected;
  if (Node = nil) or (Node = vstCustomVariables.GetLast) or (vstCustomVariables.RootNodeCount = 1) then
    Exit;

  vallstCustomVariables.Strings.Move(Node^.Index, Node^.Index + 1);

  //UpdateNodeCheckStateFromEvalBefore(Node^.NextSibling);    //uncomment these if there will ever be individual evaluations
  //UpdateNodeCheckStateFromEvalBefore(Node);

  vstCustomVariables.ClearSelection;
  vstCustomVariables.Selected[Node^.NextSibling] := True;
  vstCustomVariables.ScrollIntoView(Node, True);
  vstCustomVariables.Repaint;
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerCallTemplate.spdbtnNewVariableClick(Sender: TObject);
begin
  AddNewVariable;
end;


procedure TfrClickerCallTemplate.spdbtnRemoveSelectedVariableClick(
  Sender: TObject);
begin
  RemoveVar;
end;


procedure TfrClickerCallTemplate.vstCustomVariablesDblClick(Sender: TObject);
begin
  tmrEditCustomVars.Enabled := True;
end;


procedure TfrClickerCallTemplate.vstCustomVariablesEdited(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  NewLine: string;
begin
  if FCustomVarsMouseUpHitInfo.HitNode = nil then
    Exit;

  if not FCustomVarsUpdatedVstText then
    Exit;

  case Column of
    0:
    begin
      if Trim(FCustomVarsEditingText) = '' then
      begin
        MessageBox(Handle, 'Variable name must not be empty.', PChar(Application.Title), MB_ICONERROR);
        Exit;
      end;

      if (FCustomVarsEditingText[1] <> '$') or (FCustomVarsEditingText[Length(FCustomVarsEditingText)] <> '$') then
      begin
        MessageBox(Handle, 'Variable name must be enclosed by two "$" characters. E.g. "$my_var$" (without double quotes).', PChar(Application.Title), MB_ICONERROR);
        Exit;
      end;

      NewLine := FCustomVarsEditingText + '=' + vallstCustomVariables.Strings.ValueFromIndex[Node^.Index];

      if vallstCustomVariables.Strings.Strings[Node^.Index] <> NewLine then
      begin
        vallstCustomVariables.Strings.Strings[Node^.Index] := NewLine;
        DoOnTriggerOnControlsModified;
      end;
    end;

    1:
    begin
      if vallstCustomVariables.Strings.ValueFromIndex[Node^.Index] <> FCustomVarsEditingText then
      begin
        vallstCustomVariables.Strings.Strings[Node^.Index] := vallstCustomVariables.Strings.Names[Node^.Index] + '=' + FCustomVarsEditingText; //vallstCustomVariables.Strings.ValueFromIndex[Node^.Index] := FCustomVarsEditingText; //Do not set ValueFromIndex !!!
        DoOnTriggerOnControlsModified;
      end;
    end;
  end;
end;


procedure TfrClickerCallTemplate.vstCustomVariablesEditing(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
begin
  Allowed := Column < 2;
  FCustomVarsUpdatedVstText := False;
end;


procedure TfrClickerCallTemplate.vstCustomVariablesGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
begin
  try
    case Column of
      0: CellText := vallstCustomVariables.Strings.Names[Node^.Index];
      1: CellText := vallstCustomVariables.Strings.ValueFromIndex[Node^.Index];
    end;
  except
    CellText := 'bug';
  end;
end;


procedure TfrClickerCallTemplate.vstCustomVariablesKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    RemoveCustomVarRow1Click(RemoveCustomVarRow1);
end;


procedure TfrClickerCallTemplate.vstCustomVariablesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  vstCustomVariables.GetHitTestInfoAt(X, Y, True, FCustomVarsMouseUpHitInfo);
end;


procedure TfrClickerCallTemplate.vstCustomVariablesNewText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  const NewText: String);
begin
  FCustomVarsEditingText := FastReplace_ReturnTo68(NewText);
  FCustomVarsUpdatedVstText := True;
end;


procedure TfrClickerCallTemplate.vstCustomVariablesPaintText(
  Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
begin
  if Pos(#4#5, vallstCustomVariables.Strings[Node^.Index]) > 0 then
    TargetCanvas.Font.Color := clRed
  else
    TargetCanvas.Font.Color := clWindowText;
end;

end.

