{
    Copyright (C) 2022 VCC
    creation date: Feb 2023
    initial release date: 08 Feb 2023

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


unit ClickerSetVarFrame;

{$mode Delphi}

interface

uses
  {$IFDEF Windows}
    Windows,
  {$ELSE}
    LCLIntf, LCLType,
  {$ENDIF}
  Classes, SysUtils, Forms, Controls, Menus, ExtCtrls, StdCtrls,
  Buttons, Dialogs, Graphics, PopupNotifier, VirtualTrees, ClickerUtils;

type

  { TfrClickerSetVar }

  TfrClickerSetVar = class(TFrame)
    imglstSetVar: TImageList;
    lblSetVarWarning: TLabel;
    lblAvailableFunctions: TLabel;
    lblSetVarToHttpInfo: TLabel;
    memAvailableFunctions: TMemo;
    MenuItem_ReplaceWithSelfTemplateDir: TMenuItem;
    MenuItem_ReplaceWithTemplateDir: TMenuItem;
    MenuItem_ReplaceWithAppDir: TMenuItem;
    MenuItem_BrowseFile: TMenuItem;
    MenuItem_AddSetVar: TMenuItem;
    MenuItem_RemoveSetVar: TMenuItem;
    N4: TMenuItem;
    pnlVars: TPanel;
    pnlFunctions: TPanel;
    pmSetVars: TPopupMenu;
    pnlHorizSplitter: TPanel;
    pmVarsEditor: TPopupMenu;
    pnSetVarFormat: TPopupNotifier;
    spdbtnMoveDown: TSpeedButton;
    spdbtnNewVariable: TSpeedButton;
    spdbtnMoveUp: TSpeedButton;
    spdbtnRemoveSelectedVariable: TSpeedButton;
    tmrSetEditorTextColor: TTimer;
    tmrCreateEditor: TTimer;
    tmrEditSetVars: TTimer;
    vstSetVar: TVirtualStringTree;
    procedure FrameResize(Sender: TObject);
    procedure MenuItem_AddSetVarClick(Sender: TObject);
    procedure MenuItem_BrowseFileClick(Sender: TObject);
    procedure MenuItem_RemoveSetVarClick(Sender: TObject);
    procedure MenuItem_ReplaceWithAppDirClick(Sender: TObject);
    procedure MenuItem_ReplaceWithSelfTemplateDirClick(Sender: TObject);
    procedure MenuItem_ReplaceWithTemplateDirClick(Sender: TObject);
    procedure pnlHorizSplitterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlHorizSplitterMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlHorizSplitterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure spdbtnMoveDownClick(Sender: TObject);
    procedure spdbtnMoveUpClick(Sender: TObject);
    procedure spdbtnNewVariableClick(Sender: TObject);
    procedure spdbtnRemoveSelectedVariableClick(Sender: TObject);
    procedure tmrCreateEditorTimer(Sender: TObject);
    procedure tmrEditSetVarsTimer(Sender: TObject);
    procedure tmrSetEditorTextColorTimer(Sender: TObject);
    procedure vstSetVarChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstSetVarChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
    procedure vstSetVarCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure vstSetVarDblClick(Sender: TObject);
    procedure vstSetVarEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure vstSetVarEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure vstSetVarGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstSetVarGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vstSetVarInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstSetVarKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure vstSetVarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vstSetVarNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: String);
    procedure vstSetVarPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
  private
    FSetVarContent_Vars: TStringList;
    FSetVarContent_Values: TStringList;
    FSetVarContent_EvalBefore: TStringList;
    FFailOnException: Boolean;

    FSetVarMouseUpHitInfo: THitInfo;
    FSetVarEditingText: string;
    FSetVarUpdatedVstText: Boolean;

    FHold: Boolean;
    FSplitterMouseDownGlobalPos: TPoint;
    FSplitterMouseDownImagePos: TPoint;

    FTextEditorEditBox: TEdit;  //pointer to the built-in editor

    FOnTriggerOnControlsModified: TOnTriggerOnControlsModified;
    FOnGetFullTemplatesDir: TOnGetFullTemplatesDir;
    FOnGetSelfTemplatesDir: TOnGetFullTemplatesDir;
    FOnShowAutoComplete: TOnShowAutoComplete;

    procedure UpdateNodeCheckStateFromEvalBefore(ANode: PVirtualNode);
    procedure ResizeFrameSectionsBySplitter(NewLeft: Integer);
    procedure UpdateVstCheckStates;
    procedure AddNewVariable;
    procedure RemoveSetVar;
    procedure SetVSTEditBoxByTyping(ATextToSend: string);

    procedure DoOnTriggerOnControlsModified;
    function DoOnGetFullTemplatesDir: string;
    function DoOnGetSelfTemplatesDir: string;
    procedure DoOnShowAutoComplete(AEdit: TEdit);

    procedure edtTextEditorExit(Sender: TObject);
    procedure edtTextEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtTextEditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetListOfSetVars: TClkSetVarOptions;
    procedure SetListOfSetVars(Value: TClkSetVarOptions);

    property OnTriggerOnControlsModified: TOnTriggerOnControlsModified write FOnTriggerOnControlsModified;
    property OnGetFullTemplatesDir: TOnGetFullTemplatesDir write FOnGetFullTemplatesDir;
    property OnGetSelfTemplatesDir: TOnGetFullTemplatesDir write FOnGetSelfTemplatesDir;
    property OnShowAutoComplete: TOnShowAutoComplete write FOnShowAutoComplete;
  end;


implementation


uses
  AutoCompleteForm;

{$R *.frm}

{ TfrClickerSetVar }


constructor TfrClickerSetVar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSetVarContent_Vars := TStringList.Create;
  FSetVarContent_Values := TStringList.Create;
  FSetVarContent_EvalBefore := TStringList.Create;
  FFailOnException := False;

  FHold := False;

  FOnTriggerOnControlsModified := nil;
  FOnGetFullTemplatesDir := nil;
  FOnGetSelfTemplatesDir := nil;
  FOnShowAutoComplete := nil;

  FSetVarUpdatedVstText := False;
  FTextEditorEditBox := nil;
end;


destructor TfrClickerSetVar.Destroy;
begin
  FSetVarContent_Vars.Free;
  FSetVarContent_Values.Free;
  FSetVarContent_EvalBefore.Free;

  inherited Destroy;
end;


function TfrClickerSetVar.GetListOfSetVars: TClkSetVarOptions;
begin
  Result.ListOfVarNames := FSetVarContent_Vars.Text;
  Result.ListOfVarValues := FSetVarContent_Values.Text;
  Result.ListOfVarEvalBefore := FSetVarContent_EvalBefore.Text;
  Result.FailOnException := FFailOnException;
end;


procedure TfrClickerSetVar.UpdateVstCheckStates;
const
  CNodeCheckState: array[Boolean] of TCheckState = (csUncheckedNormal, csCheckedNormal);
var
  Node: PVirtualNode;
begin
  if vstSetVar.RootNodeCount > 0 then
  begin
    Node := vstSetVar.GetFirst;
    repeat
      vstSetVar.CheckState[Node] := CNodeCheckState[FSetVarContent_EvalBefore.Strings[Node^.Index] = '1'];
      Node := Node^.NextSibling;
    until Node = nil;
  end;
end;


procedure TfrClickerSetVar.SetListOfSetVars(Value: TClkSetVarOptions);
begin
  FSetVarContent_Vars.Text := Value.ListOfVarNames;
  FSetVarContent_Values.Text := Value.ListOfVarValues;
  FSetVarContent_EvalBefore.Text := Value.ListOfVarEvalBefore;
  FFailOnException := Value.FailOnException;

  //if Integer(vstSetVar.RootNodeCount) <> FSetVarContent_Vars.Count then  //Leave this commented! The new list might have the same length (with different content), so refresh the vst.
  begin
    vstSetVar.RootNodeCount := FSetVarContent_Vars.Count;
    UpdateVstCheckStates;
    vstSetVar.Repaint;
    DoOnTriggerOnControlsModified;
  end;
end;


procedure TfrClickerSetVar.DoOnTriggerOnControlsModified;
begin
  if not Assigned(FOnTriggerOnControlsModified) then
    raise Exception.Create('OnTriggerOnControlsModified not assigned.')
  else
    FOnTriggerOnControlsModified;
end;


function TfrClickerSetVar.DoOnGetFullTemplatesDir: string;
begin
  if not Assigned(FOnGetFullTemplatesDir) then
    raise Exception.Create('OnGetFullTemplatesDir not assigned.')
  else
    Result := FOnGetFullTemplatesDir();
end;


function TfrClickerSetVar.DoOnGetSelfTemplatesDir: string;
begin
  if not Assigned(FOnGetSelfTemplatesDir) then
    raise Exception.Create('OnGetSelfTemplatesDir not assigned.')
  else
    Result := FOnGetSelfTemplatesDir();
end;


procedure TfrClickerSetVar.DoOnShowAutoComplete(AEdit: TEdit);
begin
  if not Assigned(FOnShowAutoComplete) then
    raise Exception.Create('OnShowAutoComplete not assigned.')
  else
    FOnShowAutoComplete(AEdit);
end;


procedure TfrClickerSetVar.tmrEditSetVarsTimer(Sender: TObject);
begin
  tmrEditSetVars.Enabled := False;

  if FSetVarMouseUpHitInfo.HitNode = nil then
    Exit;

  vstSetVar.EditNode(FSetVarMouseUpHitInfo.HitNode, FSetVarMouseUpHitInfo.HitColumn);
end;


procedure TfrClickerSetVar.tmrSetEditorTextColorTimer(Sender: TObject);
begin
  tmrSetEditorTextColor.Enabled := False;

  if Assigned(FTextEditorEditBox) then
    if Pos(#4#5, FTextEditorEditBox.Text) > 0 then
      FTextEditorEditBox.Font.Color := clRed
    else
      FTextEditorEditBox.Font.Color := clWindowText;
end;


procedure TfrClickerSetVar.vstSetVarChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  FSetVarContent_EvalBefore.Strings[Node^.Index] := IntToStr(Ord(Node^.CheckState = csCheckedNormal));
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerSetVar.vstSetVarChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin
  Allowed := True;
end;


procedure TfrClickerSetVar.vstSetVarCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  TempStringEditLink: TStringEditLink;
begin
  TempStringEditLink := TStringEditLink.Create;
  EditLink := TempStringEditLink;

  //tmrCreateEditor.Enabled := True;
  FTextEditorEditBox := TEdit(TCustomEdit(TempStringEditLink.Edit));

  FTextEditorEditBox.PopupMenu := pmVarsEditor;
  FTextEditorEditBox.OnExit := edtTextEditorExit;
  FTextEditorEditBox.OnKeyDown := edtTextEditorKeyDown;
  FTextEditorEditBox.OnKeyUp := edtTextEditorKeyUp;

  FTextEditorEditBox.Show;

  Application.ProcessMessages;
  FTextEditorEditBox.SetFocus;

  tmrSetEditorTextColor.Enabled := True;
end;


procedure TfrClickerSetVar.tmrCreateEditorTimer(Sender: TObject);
begin
  tmrCreateEditor.Enabled := False;
  //FTextEditorEditBox := TEdit.Create(Self);
  //FTextEditorEditBox.Parent := vstSetVar;
  //FTextEditorEditBox.Left := vstSetVar.Left;
  //FTextEditorEditBox.Top := vstSetVar.Top + 30;
  //FTextEditorEditBox.Width := 100;
  //
  //FTextEditorEditBox.PopupMenu := pmVarsEditor;
  //FTextEditorEditBox.OnExit := edtTextEditorExit;
  //FTextEditorEditBox.OnKeyDown := edtTextEditorKeyDown;
  //FTextEditorEditBox.OnKeyUp := edtTextEditorKeyUp;
  //
  //FTextEditorEditBox.Show;
  //FTextEditorEditBox.SetFocus;
end;


procedure TfrClickerSetVar.vstSetVarDblClick(Sender: TObject);
begin
  tmrEditSetVars.Enabled := True;
end;


procedure TfrClickerSetVar.vstSetVarEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  hwc: TCompRec;
  NotifierFormRect: TRect;
begin
  if FSetVarMouseUpHitInfo.HitNode = nil then
    Exit;

  if not FSetVarUpdatedVstText then
    Exit;

  case Column of
    0:
    begin
      //nothing here
    end;

    1:
    begin
      if (Trim(FSetVarEditingText) = '') or
         (Length(FSetVarEditingText) < 3) or
         (FSetVarEditingText[1] <> '$') or
         (FSetVarEditingText[Length(FSetVarEditingText)] <> '$') then
      begin
        //MessageBox(Handle, 'The variable name must have the following format: "$<varname>$" .', PChar(Application.Title), MB_ICONINFORMATION);
        //Exit;
        hwc := GetWindowClassRec(FTextEditorEditBox.Handle);

        pnSetVarFormat.vNotifierForm.BorderWidth := 1;
        pnSetVarFormat.vNotifierForm.BorderStyle := bsSizeToolWin;
        pnSetVarFormat.vNotifierForm.Width := 300;
        pnSetVarFormat.vNotifierForm.Height := 50;

        NotifierFormRect.Left := hwc.ComponentRectangle.Left;
        NotifierFormRect.Top := hwc.ComponentRectangle.Bottom + 3;
        NotifierFormRect.Width := pnSetVarFormat.vNotifierForm.Width;
        NotifierFormRect.Height := pnSetVarFormat.vNotifierForm.Height;

        if NotifierFormRect.Right > Screen.Width - 3 then
          Dec(NotifierFormRect.Left, Screen.Width - 3 - NotifierFormRect.Right);

        if NotifierFormRect.Bottom > Screen.Height then
          NotifierFormRect.Top := hwc.ComponentRectangle.Top - NotifierFormRect.Height - 3;

        pnSetVarFormat.ShowAtPos(NotifierFormRect.Left, NotifierFormRect.Top);
      end;

      FSetVarContent_Vars.Strings[Node^.Index] := FSetVarEditingText;
      DoOnTriggerOnControlsModified;
    end;

    2:
    begin
      FSetVarContent_Values.Strings[Node^.Index] := FSetVarEditingText;
      DoOnTriggerOnControlsModified;
    end;

    else
      MessageBox(Handle, 'Editing wrong column (bug)', PChar(Application.Title), MB_ICONERROR);
  end;

  FTextEditorEditBox := nil;
end;


procedure TfrClickerSetVar.vstSetVarEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Column in [1, 2];
  FSetVarUpdatedVstText := False;
end;


procedure TfrClickerSetVar.vstSetVarGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if Column = 0 then
    ImageIndex := 0;
end;


procedure TfrClickerSetVar.vstSetVarGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
const
  CEvalState: array[Boolean] of string = ('No', 'Yes');
begin
  try
    case Column of
      0: CellText := CEvalState[FSetVarContent_EvalBefore.Strings[Node^.Index] = '1'];
      1: CellText := FSetVarContent_Vars.Strings[Node^.Index];
      2: CellText := FSetVarContent_Values.Strings[Node^.Index];
    end;
  except
    on E: Exception do
      CellText := 'bug: ' + E.Message;
  end;
end;


procedure TfrClickerSetVar.vstSetVarInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Node^.CheckType := ctCheckBox;
  Node^.CheckState := csUncheckedNormal;
end;


procedure TfrClickerSetVar.vstSetVarKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    MenuItem_RemoveSetVarClick(MenuItem_RemoveSetVar);
end;


procedure TfrClickerSetVar.vstSetVarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  vstSetVar.GetHitTestInfoAt(X, Y, True, FSetVarMouseUpHitInfo);
end;


procedure TfrClickerSetVar.vstSetVarNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
begin
  FSetVarEditingText := NewText;
  FSetVarUpdatedVstText := True;
end;


procedure TfrClickerSetVar.vstSetVarPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if Pos(#4#5, FSetVarContent_Values.Strings[Node^.Index]) > 0 then
    TargetCanvas.Font.Color := clRed
  else
    TargetCanvas.Font.Color := clWindowText;
end;


procedure TfrClickerSetVar.AddNewVariable;
begin
  FSetVarContent_Vars.Add('');
  FSetVarContent_Values.Add('');
  FSetVarContent_EvalBefore.Add('');

  vstSetVar.RootNodeCount := FSetVarContent_Vars.Count;
  vstSetVar.Repaint;
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerSetVar.RemoveSetVar;
var
  Node: PVirtualNode;
begin
  Node := vstSetVar.GetFirstSelected;
  if Node = nil then
  begin
    MessageBox(Handle, 'Please select an item to be removed.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  if MessageBox(Handle, 'Are you sure you want to remove the selected item?', PChar(Caption), MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  FSetVarContent_Vars.Delete(Node^.Index);
  FSetVarContent_Values.Delete(Node^.Index);
  FSetVarContent_EvalBefore.Delete(Node^.Index);

  vstSetVar.RootNodeCount := FSetVarContent_Vars.Count;
  UpdateVstCheckStates;
  vstSetVar.Repaint;
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerSetVar.MenuItem_AddSetVarClick(Sender: TObject);
begin
  AddNewVariable;
end;


procedure TfrClickerSetVar.SetVSTEditBoxByTyping(ATextToSend: string);
{$IFDEF Windows}
  var
    KeyStrokes: array of TINPUT;
    i, Idx: Integer;
{$ENDIF}
begin
  if Assigned(FTextEditorEditBox) then
  begin
    {$IFDEF Windows}
      //FTextEditorEditBox.Text := ATextToSend;   //It seems that VTV doesn't get the message, so "type" the whole content.

      FTextEditorEditBox.Text := '';
      FTextEditorEditBox.SetFocus; //just in case

      SetLength(KeyStrokes, Length(ATextToSend) shl 1);

      for i := 0 to Length(ATextToSend) - 1 do   //string len, not array len
      begin
        Idx := i shl 1;
        KeyStrokes[Idx]._Type := INPUT_KEYBOARD; //not sure if needed
        KeyStrokes[Idx].ki.wVk := 0;
        KeyStrokes[Idx].ki.wScan := Ord(ATextToSend[i + 1]);
        KeyStrokes[Idx].ki.dwFlags := KEYEVENTF_UNICODE; //0;
        KeyStrokes[Idx].ki.Time := 0;
        KeyStrokes[Idx].ki.ExtraInfo := 0;

        KeyStrokes[Idx + 1]._Type := INPUT_KEYBOARD; //not sure if needed
        KeyStrokes[Idx + 1].ki.wVk := 0;
        KeyStrokes[Idx + 1].ki.wScan := Ord(ATextToSend[i + 1]);
        KeyStrokes[Idx + 1].ki.dwFlags := KEYEVENTF_UNICODE or KEYEVENTF_KEYUP;
        KeyStrokes[Idx + 1].ki.Time := 0;
        KeyStrokes[Idx + 1].ki.ExtraInfo := 0;
      end;

      SendInput(Length(KeyStrokes), @KeyStrokes[0], SizeOf(TINPUT));
    {$ELSE}
      FTextEditorEditBox.Text := ''; //clear first
      FTextEditorEditBox.Text := ATextToSend;
    {$ENDIF}
  end;
end;


procedure TfrClickerSetVar.MenuItem_BrowseFileClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    if not OpenDialog.Execute then
      Exit;

    SetVSTEditBoxByTyping(OpenDialog.FileName);
  finally
    OpenDialog.Free;
  end;
end;


procedure TfrClickerSetVar.MenuItem_RemoveSetVarClick(Sender: TObject);
begin
  RemoveSetVar;
end;


procedure TfrClickerSetVar.MenuItem_ReplaceWithAppDirClick(Sender: TObject);
var
  NewValue: string;
begin
  NewValue := StringReplace(FTextEditorEditBox.Text, ExtractFilePath(ParamStr(0)), '$AppDir$' + PathDelim, [rfReplaceAll]);
  SetVSTEditBoxByTyping(NewValue);
end;


procedure TfrClickerSetVar.MenuItem_ReplaceWithTemplateDirClick(Sender: TObject);
var
  NewValue: string;
begin
  NewValue := StringReplace(FTextEditorEditBox.Text, DoOnGetFullTemplatesDir, '$TemplateDir$' {+ PathDelim}, [rfReplaceAll]);
  SetVSTEditBoxByTyping(NewValue);
end;


procedure TfrClickerSetVar.MenuItem_ReplaceWithSelfTemplateDirClick(
  Sender: TObject);
var
  NewValue: string;
begin
  NewValue := StringReplace(FTextEditorEditBox.Text, DoOnGetSelfTemplatesDir, '$SelfTemplateDir$' {+ PathDelim}, [rfReplaceAll]);
  SetVSTEditBoxByTyping(NewValue);
end;


procedure TfrClickerSetVar.FrameResize(Sender: TObject);
var
  NewLeft: Integer;
begin
  NewLeft := pnlHorizSplitter.Left;

  if NewLeft > Width - 100 then
    NewLeft := Width - 100;

  ResizeFrameSectionsBySplitter(NewLeft);
end;


procedure TfrClickerSetVar.ResizeFrameSectionsBySplitter(NewLeft: Integer);
begin
  if NewLeft < pnlVars.Constraints.MinWidth then
    NewLeft := pnlVars.Constraints.MinWidth;

  if NewLeft > Width - 100 then
    NewLeft := Width - 100;

  pnlHorizSplitter.Left := NewLeft;

  pnlFunctions.Left := pnlHorizSplitter.Left + pnlHorizSplitter.Width;
  pnlFunctions.Width := Width - pnlFunctions.Left;
  pnlVars.Width := pnlHorizSplitter.Left;
end;


procedure TfrClickerSetVar.pnlHorizSplitterMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Shift <> [ssLeft] then
    Exit;

  if not FHold then
  begin
    GetCursorPos(FSplitterMouseDownGlobalPos);

    FSplitterMouseDownImagePos.X := pnlHorizSplitter.Left;
    FHold := True;
  end;
end;


procedure TfrClickerSetVar.pnlHorizSplitterMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  tp: TPoint;
  NewLeft: Integer;
begin
  if Shift <> [ssLeft] then
    Exit;

  if not FHold then
    Exit;

  GetCursorPos(tp);
  NewLeft := FSplitterMouseDownImagePos.X + tp.X - FSplitterMouseDownGlobalPos.X;

  ResizeFrameSectionsBySplitter(NewLeft);
end;


procedure TfrClickerSetVar.pnlHorizSplitterMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FHold := False;
end;


procedure TfrClickerSetVar.UpdateNodeCheckStateFromEvalBefore(ANode: PVirtualNode);
const
  CCheckStates: array[Boolean] of TCheckState = (csUnCheckedNormal, csCheckedNormal);
begin
  ANode.CheckState := CCheckStates[FSetVarContent_EvalBefore.Strings[ANode^.Index] = '1'];
end;


procedure TfrClickerSetVar.spdbtnMoveUpClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstSetVar.GetFirstSelected;
  if (Node = nil) or (Node = vstSetVar.GetFirst) or (vstSetVar.RootNodeCount = 1) then
    Exit;

  FSetVarContent_Vars.Move(Node^.Index, Node^.Index - 1);
  FSetVarContent_Values.Move(Node^.Index, Node^.Index - 1);
  FSetVarContent_EvalBefore.Move(Node^.Index, Node^.Index - 1);

  UpdateNodeCheckStateFromEvalBefore(Node^.PrevSibling);
  UpdateNodeCheckStateFromEvalBefore(Node);

  vstSetVar.ClearSelection;
  vstSetVar.Selected[Node^.PrevSibling] := True;
  vstSetVar.ScrollIntoView(Node, True);
  vstSetVar.Repaint;
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerSetVar.spdbtnNewVariableClick(Sender: TObject);
begin
  AddNewVariable;
end;


procedure TfrClickerSetVar.spdbtnRemoveSelectedVariableClick(Sender: TObject);
begin
  RemoveSetVar;
end;


procedure TfrClickerSetVar.spdbtnMoveDownClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstSetVar.GetFirstSelected;
  if (Node = nil) or (Node = vstSetVar.GetLast) or (vstSetVar.RootNodeCount = 1) then
    Exit;

  FSetVarContent_Vars.Move(Node^.Index, Node^.Index + 1);
  FSetVarContent_Values.Move(Node^.Index, Node^.Index + 1);
  FSetVarContent_EvalBefore.Move(Node^.Index, Node^.Index + 1);

  UpdateNodeCheckStateFromEvalBefore(Node^.NextSibling);
  UpdateNodeCheckStateFromEvalBefore(Node);

  vstSetVar.ClearSelection;
  vstSetVar.Selected[Node^.NextSibling] := True;
  vstSetVar.ScrollIntoView(Node, True);
  vstSetVar.Repaint;
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerSetVar.edtTextEditorExit(Sender: TObject);
begin
  if Assigned(frmAutoComplete) and not frmAutoComplete.Focused then  //if AutoCompleteVisible then
    CloseAutoComplete
  else
  begin
    FSetVarEditingText := FTextEditorEditBox.Text;
    FSetVarUpdatedVstText := True;
    FreeAndNil(FTextEditorEditBox);
  end;
end;


procedure TfrClickerSetVar.edtTextEditorKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_SPACE then
    if ssCtrl in Shift then
    begin
      Key := 0;
      Exit;
    end;

  if Key in [VK_RETURN, VK_ESCAPE] then
    if AutoCompleteVisible then
      CloseAutoComplete;
end;


procedure TfrClickerSetVar.edtTextEditorKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  Editor: TEdit;
begin
  Editor := TEdit(TCustomEdit(Sender));

  if Key = VK_SPACE then
    if ssCtrl in Shift then
    begin
      Key := 0;
      DoOnShowAutoComplete(Editor);
    end;

  if AutoCompleteVisible then
    UpdateAutoComplete(Editor);

  if Pos(#4#5, Editor.Text) > 0 then
    Editor.Font.Color := clRed
  else
    Editor.Font.Color := clWindowText;
end;


end.

