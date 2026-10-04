{
    Copyright (C) 2026 VCC
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
  Buttons, VirtualTrees, ClickerUtils, Graphics;

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
    vstCustomVariables: TVirtualStringTree;
    procedure AddCustomVarRow1Click(Sender: TObject);
    procedure RemoveCustomVarRow1Click(Sender: TObject);
    procedure spdbtnMoveDownClick(Sender: TObject);
    procedure spdbtnMoveUpClick(Sender: TObject);
    procedure spdbtnNewVariableClick(Sender: TObject);
    procedure spdbtnRemoveSelectedVariableClick(Sender: TObject);
    procedure tmrEditCustomVarsTimer(Sender: TObject);
    procedure vstCustomVariablesCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure vstCustomVariablesDblClick(Sender: TObject);
    procedure vstCustomVariablesEdited(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vstCustomVariablesEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstCustomVariablesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: boolean; var ImageIndex: integer);
    procedure vstCustomVariablesGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure vstCustomVariablesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure vstCustomVariablesKeyUp(Sender: TObject; var Key: Word;
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
    FTextEditorEditBox: TEdit;  //pointer to the built-in editor

    FCallTemplateContent_Vars: TStringList;
    FCallTemplateContent_Values: TStringList;

    FOnTriggerOnControlsModified: TOnTriggerOnControlsModified;

    procedure DoOnTriggerOnControlsModified;

    procedure AddNewVariable;
    procedure RemoveVar;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetListOfCustomVariables: string;
    procedure SetListOfCustomVariables(Value: string);

    property OnTriggerOnControlsModified: TOnTriggerOnControlsModified write FOnTriggerOnControlsModified;
  end;


implementation

{$R *.frm}

uses
  ClickerIconsDM, ClickerIniFiles, Clipbrd;

{ TfrClickerCallTemplate }


constructor TfrClickerCallTemplate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCallTemplateContent_Vars := TStringList.Create;
  FCallTemplateContent_Values := TStringList.Create;
  FCallTemplateContent_Vars.LineBreak := #13#10;
  FCallTemplateContent_Values.LineBreak := #13#10;
  FOnTriggerOnControlsModified := nil;
end;


destructor TfrClickerCallTemplate.Destroy;
begin
  FreeAndNil(FCallTemplateContent_Vars);
  FreeAndNil(FCallTemplateContent_Values);

  inherited Destroy;
end;


procedure TfrClickerCallTemplate.DoOnTriggerOnControlsModified;
begin
  if not Assigned(FOnTriggerOnControlsModified) then
    raise Exception.Create('OnTriggerOnControlsModified not assigned.')
  else
    FOnTriggerOnControlsModified;
end;


function TfrClickerCallTemplate.GetListOfCustomVariables: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FCallTemplateContent_Vars.Count - 1 do
    Result := Result + FCallTemplateContent_Vars.Strings[i] + '=' + FCallTemplateContent_Values.Strings[i] + #13#10;
end;


procedure TfrClickerCallTemplate.SetListOfCustomVariables(Value: string);
var
  TempList: TStringList;
  i: Integer;
begin
  TempList := TStringList.Create;
  try
    TempList.LineBreak := #13#10;
    TempList.Text := Value;

    FCallTemplateContent_Vars.Clear;
    FCallTemplateContent_Values.Clear;

    for i := 0 to TempList.Count - 1 do
    begin
      FCallTemplateContent_Vars.Add(TempList.Names[i]);
      FCallTemplateContent_Values.Add(TempList.ValueFromIndex[i]);
    end;

    vstCustomVariables.RootNodeCount := TempList.Count;
    vstCustomVariables.Repaint;
  finally
    TempList.Free;
  end;
end;


procedure TfrClickerCallTemplate.tmrEditCustomVarsTimer(Sender: TObject);
begin
  tmrEditCustomVars.Enabled := False;

  if FCustomVarsMouseUpHitInfo.HitNode = nil then
    Exit;

  vstCustomVariables.EditNode(FCustomVarsMouseUpHitInfo.HitNode, FCustomVarsMouseUpHitInfo.HitColumn);
end;


procedure TfrClickerCallTemplate.vstCustomVariablesCreateEditor(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out
  EditLink: IVTEditLink);
var
  TempStringEditLink: TStringEditLink;
begin
  TempStringEditLink := TStringEditLink.Create;
  EditLink := TempStringEditLink;

  FTextEditorEditBox := TEdit(TCustomEdit(TempStringEditLink.Edit));

  FTextEditorEditBox.Font.Style := vstCustomVariables.Font.Style;
  FTextEditorEditBox.Font.Name := vstCustomVariables.Font.Name;
  FTextEditorEditBox.Font.Size := vstCustomVariables.Font.Size;
end;


procedure TfrClickerCallTemplate.AddNewVariable;
begin
  FCallTemplateContent_Vars.Add('');
  FCallTemplateContent_Values.Add('');

  vstCustomVariables.RootNodeCount := FCallTemplateContent_Vars.Count;
  vstCustomVariables.Repaint;

  vstCustomVariables.ClearSelection;
  vstCustomVariables.Selected[vstCustomVariables.GetLast] := True;

  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerCallTemplate.RemoveVar;
var
  Node: PVirtualNode;
begin
  try
    Node := vstCustomVariables.GetFirstSelected;

    if Node = nil then
    begin
      MessageBox(Handle, 'Please select a variable to be deleted.', PChar(Application.Title), MB_ICONINFORMATION);
      Exit;
    end;

    Node := vstCustomVariables.GetLast;
    repeat
      if vstCustomVariables.Selected[Node] then
      begin
        FCallTemplateContent_Vars.Delete(Node^.Index);
        FCallTemplateContent_Values.Delete(Node^.Index);
      end;

      Node := vstCustomVariables.GetPrevious(Node);
    until Node = nil;

    vstCustomVariables.RootNodeCount := FCallTemplateContent_Vars.Count;
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
  if MessageBox(Handle, 'Remove selected variable(s)?', 'Selection', MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  RemoveVar;
end;


procedure TfrClickerCallTemplate.spdbtnMoveUpClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstCustomVariables.GetFirstSelected;
  if (Node = nil) or (Node = vstCustomVariables.GetFirst) or (vstCustomVariables.RootNodeCount = 1) then
    Exit;

  FCallTemplateContent_Vars.Move(Node^.Index, Node^.Index - 1);
  FCallTemplateContent_Values.Move(Node^.Index, Node^.Index - 1);

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

  FCallTemplateContent_Vars.Move(Node^.Index, Node^.Index + 1);
  FCallTemplateContent_Values.Move(Node^.Index, Node^.Index + 1);

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
  if MessageBox(Handle, 'Remove selected variable(s)?', 'Selection', MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  RemoveVar;
end;


procedure TfrClickerCallTemplate.vstCustomVariablesDblClick(Sender: TObject);
begin
  tmrEditCustomVars.Enabled := True;
end;


procedure TfrClickerCallTemplate.vstCustomVariablesEdited(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  if FCustomVarsMouseUpHitInfo.HitNode = nil then
    Exit;

  if not FCustomVarsUpdatedVstText then
    Exit;

  case Column of
    0:
    begin
      if (Trim(FCustomVarsEditingText) = '') or
         (Length(FCustomVarsEditingText) < 3) or
         (FCustomVarsEditingText[1] <> '$') or
         (FCustomVarsEditingText[Length(FCustomVarsEditingText)] <> '$') then
        dmClickerIcons.DisplayVarFormatNotifier(FTextEditorEditBox.Handle);

      if FCallTemplateContent_Vars.Strings[Node^.Index] <> FCustomVarsEditingText then
      begin
        FCallTemplateContent_Vars.Strings[Node^.Index] := FCustomVarsEditingText;
        DoOnTriggerOnControlsModified;
      end;
    end;

    1:
    begin
      if FCallTemplateContent_Values.Strings[Node^.Index] <> FCustomVarsEditingText then
      begin
        FCallTemplateContent_Values.Strings[Node^.Index] := FCustomVarsEditingText;
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


procedure TfrClickerCallTemplate.vstCustomVariablesGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: boolean; var ImageIndex: integer);
begin
  if Column = 0 then
    ImageIndex := 0;
end;


procedure TfrClickerCallTemplate.vstCustomVariablesGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
begin
  try
    case Column of
      0: CellText := FCallTemplateContent_Vars.Strings[Node^.Index];
      1: CellText := FCallTemplateContent_Values.Strings[Node^.Index];
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


procedure TfrClickerCallTemplate.vstCustomVariablesKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  Content: TStringList;
  Ini: TClkIniFile;
  i, cnt: Integer;
  Node: PVirtualNode;
begin
  if (Key in [Ord('C'), Ord('V'), Ord('X')]) and (ssCtrl in Shift) then
  begin
    cnt := 0;
    Content := TStringList.Create;
    try
      if Key = Ord('V') then
        Content.Text := Clipboard.AsText;

      Ini := TClkIniFile.Create(Content);
      try
        if Key in [Ord('C'), Ord('X')] then
        begin
          Node := vstCustomVariables.GetFirstSelected;
          if Node = nil then
            Exit;

          repeat
            if vstCustomVariables.Selected[Node] then
            begin                                           //using 'SetVar' section name, for compatibility with SetVar action
              Ini.WriteString('SetVar', 'Vars_' + IntToStr(cnt), FCallTemplateContent_Vars[Node^.Index]);
              Ini.WriteString('SetVar', 'Values_' + IntToStr(cnt), FCallTemplateContent_Values[Node^.Index]);
              Inc(cnt);
            end;

            Node := vstCustomVariables.GetNextSelected(Node);
          until Node = nil;

          Ini.WriteInteger('SetVar', 'Count', cnt);
          // no need to call Ini.Update...;
          Ini.GetFileContent(Content);
          Clipboard.AsText := Content.Text;

          if Key = Ord('X') then
            RemoveVar;
        end; //Copy / Cut

        if Key = Ord('V') then
        begin
          for i := 0 to Ini.ReadInteger('SetVar', 'Count', 0) - 1 do
          begin
            FCallTemplateContent_Vars.Add(Ini.ReadString('SetVar', 'Vars_' + IntToStr(i), '$var$'));
            FCallTemplateContent_Values.Add(Ini.ReadString('SetVar', 'Values_' + IntToStr(i), 'value'));
          end;

          if Integer(vstCustomVariables.RootNodeCount) <> FCallTemplateContent_Vars.Count then
          begin
            vstCustomVariables.RootNodeCount := FCallTemplateContent_Vars.Count;
            vstCustomVariables.Repaint;
            DoOnTriggerOnControlsModified;
          end;
        end; //Paste
      finally
        Ini.Free;
      end;
    finally
      Content.Free;
    end;
  end;
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
  if Pos(#4#5, FCallTemplateContent_Vars.Strings[Node^.Index] + '=' + FCallTemplateContent_Values.Strings[Node^.Index]) > 0 then
    TargetCanvas.Font.Color := clRed
  else
    TargetCanvas.Font.Color := clWindowText;
end;

end.

