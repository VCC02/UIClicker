{
    Copyright (C) 2022 VCC
    creation date: Dec 2019
    initial release date: 13 Sep 2022

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


unit ClickerConditionEditorFrame;

{$H+}
{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, Menus,
  Graphics, VirtualTrees, ClickerUtils;

type
  TOnControlsModified = procedure of object;

  { TfrClickerConditionEditor }

  TfrClickerConditionEditor = class(TFrame)
    imglstComparisonOperators: TImageList;
    lblLastActionStatusValidValues: TLabel;
    MenuItem_AddPrefixWithZerosEqualsNumber: TMenuItem;
    MenuItemEqual: TMenuItem;
    MenuItemGreaterThan: TMenuItem;
    MenuItemGreaterThanOrEqual: TMenuItem;
    MenuItemLessThan: TMenuItem;
    MenuItemLessThanOrEqual: TMenuItem;
    MenuItemNotEqual: TMenuItem;
    MenuItemRemoveExpressionPart: TMenuItem;
    MenuItemRemoveTerm: TMenuItem;
    MenuItem_AddLastActionStatusEqualsAllowedFailed: TMenuItem;
    MenuItem_AddLastActionStatusEqualsSuccessful: TMenuItem;
    N6: TMenuItem;
    pmActionConditions: TPopupMenu;
    pmActionConditionsEval: TPopupMenu;
    pnlvstActionConditions: TPanel;
    spdbtnAddAND: TSpeedButton;
    spdbtnAddOR: TSpeedButton;
    tmrEditingCondition: TTimer;
    procedure MenuItemEqualClick(Sender: TObject);
    procedure MenuItemGreaterThanClick(Sender: TObject);
    procedure MenuItemGreaterThanOrEqualClick(Sender: TObject);
    procedure MenuItemLessThanClick(Sender: TObject);
    procedure MenuItemLessThanOrEqualClick(Sender: TObject);
    procedure MenuItemNotEqualClick(Sender: TObject);
    procedure MenuItemRemoveExpressionPartClick(Sender: TObject);
    procedure MenuItemRemoveTermClick(Sender: TObject);
    procedure MenuItem_AddLastActionStatusEqualsAllowedFailedClick(
      Sender: TObject);
    procedure MenuItem_AddLastActionStatusEqualsSuccessfulClick(Sender: TObject
      );
    procedure MenuItem_AddPrefixWithZerosEqualsNumberClick(Sender: TObject);
    procedure spdbtnAddANDClick(Sender: TObject);
    procedure spdbtnAddORClick(Sender: TObject);
    procedure tmrEditingConditionTimer(Sender: TObject);

    procedure vstActionConditionsDblClick(Sender: TObject);
    procedure vstActionConditionsEditCancelled(Sender: TBaseVirtualTree; Column: TColumnIndex);
    procedure vstActionConditionsEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vstActionConditionsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstActionConditionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: {$IFDEF FPC} string {$ELSE} WideString {$ENDIF});
    procedure vstActionConditionsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstActionConditionsHeaderMouseDown(Sender: TVTHeader; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure vstActionConditionsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure vstActionConditionsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure vstActionConditionsNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; {$IFDEF FPC} const {$ENDIF} NewText: {$IFDEF FPC} string {$ELSE} WideString {$ENDIF});
    procedure vstActionConditionsColumnResize(Sender: TVTHeader; Column: TColumnIndex);
  private
    FOnControlsModified: TOnControlsModified;
    FActionConditionsHeaderHitInfo: THitInfo;
    FActionConditionsMouseUpHitInfo: THitInfo;
    FUpdatedVstText: Boolean;
    FActionConditionForPreview: array of TStringList;
    FEditingText: string;
    FColumnWidths: TIntArr;
    FAddingColumn: Boolean;

    vstActionConditions: TVirtualStringTree;

    function GetColumnWidths(Index: Integer): Integer;
    procedure SetColumnWidths(Index: Integer; NewValue: Integer);

    procedure CreateRemainingUIComponents;
    procedure TriggerOnControlsModified;

    procedure AddExpressionColumns(IsLastColumn: Boolean);
    procedure SetConditionOperator(ANewOperator: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ClearActionConditionPreview;
    function ConditionsAvailable: Boolean; //returns True if the editor has any conditions loaded
    function GetActionCondition: string; //serialize
    procedure DisplayActionCondition(ACondition: string);

    property ColumnWidths[Index: Integer]: Integer read GetColumnWidths write SetColumnWidths;

    property OnControlsModified: TOnControlsModified read FOnControlsModified write FOnControlsModified;
  end;


implementation

{$R *.frm}


{ TfrClickerConditionEditor }

constructor TfrClickerConditionEditor.Create(AOwner: TComponent);
var
  i: TActionStatus;
begin
  inherited Create(AOwner);
  CreateRemainingUIComponents;

  lblLastActionStatusValidValues.Caption := '$LastAction_Status$ valid values (no quotes): ';
  for i := Low(TActionStatus) to High(TActionStatus) do
    lblLastActionStatusValidValues.Caption := lblLastActionStatusValidValues.Caption + '"' + CActionStatusStr[i] + '"  ';

  SetLength(FColumnWidths, 4 * 5);
  FAddingColumn := False;
end;


destructor TfrClickerConditionEditor.Destroy;
var
  i: Integer;
begin
  for i := 0 to vstActionConditions.Header.Columns.Count - 1 do
    ColumnWidths[i] := vstActionConditions.Header.Columns.Items[i].Width;

  inherited Destroy;
end;


procedure TfrClickerConditionEditor.CreateRemainingUIComponents;
begin
  vstActionConditions := TVirtualStringTree.Create(Self);
  vstActionConditions.Parent := Self;

  vstActionConditions.Left := 3;
  vstActionConditions.Top := 3;
  vstActionConditions.Width := pnlvstActionConditions.Width;
  vstActionConditions.Height := pnlvstActionConditions.Height;
  vstActionConditions.Anchors := [akBottom, akLeft, akRight, akTop];
  vstActionConditions.Hint := 'Double-click a comparison sign to change it. Right-click for other options.';
  vstActionConditions.Header.AutoSizeIndex := -1;
  vstActionConditions.Header.DefaultHeight := 17;
  vstActionConditions.Header.Font.Charset := DEFAULT_CHARSET;
  vstActionConditions.Header.Font.Color := clWindowText;
  vstActionConditions.Header.Font.Height := -11;
  vstActionConditions.Header.Font.Name := 'Tahoma';
  vstActionConditions.Header.Font.Style := [];
  vstActionConditions.Header.Height := 21;
  vstActionConditions.Header.MainColumn := -1;
  vstActionConditions.Header.Options := [hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible];
  vstActionConditions.Header.PopupMenu := pmActionConditions;
  vstActionConditions.Header.Style := hsFlatButtons;
  vstActionConditions.Indent := 4;
  vstActionConditions.ParentShowHint := False;
  vstActionConditions.PopupMenu := pmActionConditions;
  vstActionConditions.ShowHint := True;
  vstActionConditions.StateImages := imglstComparisonOperators;
  vstActionConditions.TabOrder := 0;
  vstActionConditions.TreeOptions.AutoOptions := [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toDisableAutoscrollOnEdit];
  vstActionConditions.TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages];
  vstActionConditions.TreeOptions.SelectionOptions := [toFullRowSelect, toRightClickSelect];
  vstActionConditions.OnDblClick := vstActionConditionsDblClick;
  vstActionConditions.OnEditCancelled := vstActionConditionsEditCancelled;
  vstActionConditions.OnEdited := vstActionConditionsEdited;
  vstActionConditions.OnEditing := vstActionConditionsEditing;
  vstActionConditions.OnGetText := vstActionConditionsGetText;
  vstActionConditions.OnGetImageIndex := vstActionConditionsGetImageIndex;
  vstActionConditions.OnHeaderMouseDown := vstActionConditionsHeaderMouseDown;
  vstActionConditions.OnMouseDown := vstActionConditionsMouseDown;
  vstActionConditions.OnMouseUp := vstActionConditionsMouseUp;
  vstActionConditions.OnNewText := vstActionConditionsNewText;
  vstActionConditions.OnColumnResize := vstActionConditionsColumnResize;
  vstActionConditions.Colors.UnfocusedSelectionColor := clGradientInactiveCaption;
end;


procedure TfrClickerConditionEditor.TriggerOnControlsModified;
begin
  if not Assigned(FOnControlsModified) then
    raise Exception.Create('FOnControlsModified is not assinged.')
  else
    FOnControlsModified();
end;


procedure TfrClickerConditionEditor.ClearActionConditionPreview;
var
  i: Integer;
begin
  for i := 0 to Length(FActionConditionForPreview) - 1 do
    FActionConditionForPreview[i].Free;

  SetLength(FActionConditionForPreview, 0);
  vstActionConditions.Header.Columns.Clear;
  vstActionConditions.RootNodeCount := 0;
end;


procedure TfrClickerConditionEditor.AddExpressionColumns(IsLastColumn: Boolean);
var
  AColumn: TVirtualTreeColumn;
begin
  FAddingColumn := True;
  try
    AColumn := vstActionConditions.Header.Columns.Add;
    AColumn.Text := '(Op1';
    AColumn.MinWidth := 100;
    AColumn.Width := Max(AColumn.MinWidth, ColumnWidths[vstActionConditions.Header.Columns.Count - 1]);
    AColumn.Alignment := taCenter;

    AColumn := vstActionConditions.Header.Columns.Add;
    AColumn.Text := 'Eq';
    AColumn.MinWidth := 25;
    AColumn.Width := Max(AColumn.MinWidth, ColumnWidths[vstActionConditions.Header.Columns.Count - 1]);
    AColumn.Alignment := taCenter;

    AColumn := vstActionConditions.Header.Columns.Add;
    AColumn.Text := 'Op2)';
    AColumn.MinWidth := 100;
    AColumn.Width := Max(AColumn.MinWidth, ColumnWidths[vstActionConditions.Header.Columns.Count - 1]);
    AColumn.Alignment := taCenter;

    AColumn := vstActionConditions.Header.Columns.Add;
    if not IsLastColumn then
      AColumn.Text := 'AND'
    else
      AColumn.Text := 'OR';

    AColumn.MinWidth := 45;
    AColumn.Width := Max(AColumn.MinWidth, ColumnWidths[vstActionConditions.Header.Columns.Count - 1]);
    AColumn.Alignment := taCenter;
  finally
    FAddingColumn := False;
  end;
end;


function TfrClickerConditionEditor.GetColumnWidths(Index: Integer): Integer;
begin
  if (Index > -1) and (Index < Length(FColumnWidths)) then
    Result := FColumnWidths[Index]
  else
    Result := 100; //some default
end;


procedure TfrClickerConditionEditor.SetColumnWidths(Index: Integer; NewValue: Integer);
begin
  if (Index > -1) and (Index < Length(FColumnWidths)) then
    FColumnWidths[Index] := NewValue;
end;


procedure TfrClickerConditionEditor.DisplayActionCondition(ACondition: string);
var
  AStringList: TStringList;
  i: Integer;
  MaxANDsCount: Integer;
begin
  ClearActionConditionPreview;

  AStringList := TStringList.Create;
  try
    AStringList.Text := ACondition;
    SetLength(FActionConditionForPreview, AStringList.Count);

    MaxANDsCount := 0;
    for i := 0 to Length(FActionConditionForPreview) - 1 do
    begin
      FActionConditionForPreview[i] := TStringList.Create;
      FActionConditionForPreview[i].Text := StringReplace(AStringList[i], #5#6, #13#10, [rfReplaceAll]);

      MaxANDsCount := Max(MaxANDsCount, FActionConditionForPreview[i].Count);
    end;
  finally
    AStringList.Free;
  end;

  for i := 1 to MaxANDsCount do
    AddExpressionColumns(i = MaxANDsCount);

  vstActionConditions.RootNodeCount := Length(FActionConditionForPreview);
  vstActionConditions.Repaint;
end;


function TfrClickerConditionEditor.GetActionCondition: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(FActionConditionForPreview) - 1 do
    Result := Result + StringReplace(FActionConditionForPreview[i].Text, #13#10, #5#6, [rfReplaceAll]) + #13#10;
end;


procedure TfrClickerConditionEditor.SetConditionOperator(ANewOperator: string);
var
  Op1, Op2, OpEq, RawExpr: string;
begin
  RawExpr := FActionConditionForPreview[FActionConditionsMouseUpHitInfo.HitNode^.Index].Strings[FActionConditionsMouseUpHitInfo.HitColumn shr 2]; //var==var    or   var<>var
  RawExpressionToParts(RawExpr, Op1, Op2, OpEq);

  FActionConditionForPreview[FActionConditionsMouseUpHitInfo.HitNode^.Index].Strings[FActionConditionsMouseUpHitInfo.HitColumn shr 2] := Op1 + ANewOperator + Op2;

  vstActionConditions.RepaintNode(FActionConditionsMouseUpHitInfo.HitNode);
  TriggerOnControlsModified;
end;


function TfrClickerConditionEditor.ConditionsAvailable: Boolean;
begin
  Result := vstActionConditions.RootNodeCount > 0;
end;


function ComparisonOperatorToIndex(AOp: string): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := Low(CComparisonOperators) to High(CComparisonOperators) do
    if CComparisonOperators[i] = AOp then
    begin
      Result := i;
      Break;
    end;
end;


procedure TfrClickerConditionEditor.tmrEditingConditionTimer(Sender: TObject);
begin
  tmrEditingCondition.Enabled := False;

  if FActionConditionsMouseUpHitInfo.HitNode = nil then
    Exit;

  if Byte(FActionConditionsMouseUpHitInfo.HitColumn mod 4) in [0, 2] then
    vstActionConditions.EditNode(FActionConditionsMouseUpHitInfo.HitNode, FActionConditionsMouseUpHitInfo.HitColumn);
end;


procedure TfrClickerConditionEditor.vstActionConditionsDblClick(Sender: TObject);
var
  tp: TPoint;
begin
  if FActionConditionsMouseUpHitInfo.HitColumn mod 4 = 1 then
  begin
    GetCursorPos(tp);
    pmActionConditionsEval.PopUp(tp.X, tp.Y);
  end
  else
  begin
    tmrEditingCondition.Enabled := True;
    //FEditingText := '';
  end;
end;


procedure TfrClickerConditionEditor.vstActionConditionsEditCancelled(Sender: TBaseVirtualTree; Column: TColumnIndex);
begin
  //FEditingText := '';
end;


procedure TfrClickerConditionEditor.vstActionConditionsEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Byte(Column mod 4) in [0, 2];
  FUpdatedVstText := False;
end;


procedure TfrClickerConditionEditor.vstActionConditionsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: {$IFDEF FPC} string {$ELSE} WideString {$ENDIF});
var
  Op1, Op2, OpEq, RawExpr: string;
  ExprIdx: Integer;
begin
  try
    ExprIdx := Column shr 2;

    case Column mod 4 of
      0:
      begin
        if ExprIdx > FActionConditionForPreview[Node^.Index].Count - 1 then
          CellText := 'idx bug'
        else
        begin
          RawExpr := FActionConditionForPreview[Node^.Index].Strings[ExprIdx];
          RawExpressionToParts(RawExpr, Op1, Op2, OpEq);
          CellText := Op1;
        end;
      end;

      {1:
      begin
        RawExpr := FActionConditionForPreview[Node^.Index].Strings[ExprIdx];
        RawExpressionToParts(RawExpr, Op1, Op2, OpEq);
        CellText := OpEq;
      end;}

      2:
      begin
        if ExprIdx > FActionConditionForPreview[Node^.Index].Count - 1 then
          CellText := 'idx bug'
        else
        begin
          RawExpr := FActionConditionForPreview[Node^.Index].Strings[ExprIdx];
          RawExpressionToParts(RawExpr, Op1, Op2, OpEq);
          CellText := Op2;
        end;
      end;

      3:
      begin
        if Column = vstActionConditions.Header.Columns.Count - 1 then
        begin
          if Node^.Index = vstActionConditions.RootNodeCount - 1 then
            CellText := ''
          else
            CellText := 'OR'
        end
        else
          CellText := 'AND';
      end
    else
      CellText := '';
    end;
  except
    CellText := 'bug';
  end;
end;


procedure TfrClickerConditionEditor.vstActionConditionsEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  Op1, Op2, OpEq, RawExpr: string;
begin
  if FActionConditionsMouseUpHitInfo.HitNode = nil then
    Exit;

  if not FUpdatedVstText then
    Exit;

  //if (Trim(FEditingText) = '') or (FEditingText[1] <> '$') or (FEditingText[Length(FEditingText)] <> '$') then
  //  Exit;

  RawExpr := FActionConditionForPreview[FActionConditionsMouseUpHitInfo.HitNode^.Index].Strings[FActionConditionsMouseUpHitInfo.HitColumn shr 2]; //var==var    or   var<>var
  RawExpressionToParts(RawExpr, Op1, Op2, OpEq);

  case Column mod 4 of
    0: Op1 := FEditingText;
    2: Op2 := FEditingText;
  else
    raise Exception.Create('Editing wrong column (bug)');
  end;

  FActionConditionForPreview[FActionConditionsMouseUpHitInfo.HitNode^.Index].Strings[FActionConditionsMouseUpHitInfo.HitColumn shr 2] := Op1 + OpEq + Op2;
  TriggerOnControlsModified;
end;


procedure TfrClickerConditionEditor.vstActionConditionsGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  Op1, Op2, OpEq, RawExpr: string;
begin
  try
    if Column mod 4 = 1 then
    begin
      RawExpr := FActionConditionForPreview[Node^.Index].Strings[Column shr 2];
      RawExpressionToParts(RawExpr, Op1, Op2, OpEq);

      ImageIndex := ComparisonOperatorToIndex(OpEq);
    end;
  except
  end;
end;


procedure TfrClickerConditionEditor.vstActionConditionsHeaderMouseDown(
  Sender: TVTHeader; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  vstActionConditions.GetHitTestInfoAt(X, Y, True, FActionConditionsHeaderHitInfo);
end;


procedure TfrClickerConditionEditor.vstActionConditionsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //vstActionConditions.GetHitTestInfoAt(X, Y, True, FActionConditionsMouseDownHitInfo);
end;


procedure TfrClickerConditionEditor.vstActionConditionsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  vstActionConditions.GetHitTestInfoAt(X, Y, True, FActionConditionsMouseUpHitInfo);
end;


procedure TfrClickerConditionEditor.vstActionConditionsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; {$IFDEF FPC} const {$ENDIF} NewText: {$IFDEF FPC} string {$ELSE} WideString {$ENDIF});
begin
  FEditingText := NewText;
  Randomize;
  FUpdatedVstText := True;
end;


procedure TfrClickerConditionEditor.vstActionConditionsColumnResize(Sender: TVTHeader; Column: TColumnIndex);
begin
  if not FAddingColumn then   //This handler is called by the VST, both when manually resizing columns and when creating columns and setting their width in code.
  begin
    if (Column > -1) and (Column < Length(FColumnWidths)) then
      FColumnWidths[Column] := vstActionConditions.Header.Columns.Items[Column].Width;
  end;
end;


procedure TfrClickerConditionEditor.spdbtnAddORClick(Sender: TObject);   //new row
var
  i: Integer;
  s: string;
begin
  SetLength(FActionConditionForPreview, Length(FActionConditionForPreview) + 1);
  FActionConditionForPreview[Length(FActionConditionForPreview) - 1] := TStringList.Create;

  if Length(FActionConditionForPreview) > 1 then //there is already an item to copy from
  begin
    s := '';
    for i := 0 to FActionConditionForPreview[0].Count - 1 do
      s := s + '$my_var$==$my_var$' + #13#10;

    FActionConditionForPreview[Length(FActionConditionForPreview) - 1].Text := s;
  end
  else
  begin
    AddExpressionColumns(True); //OR
    FActionConditionForPreview[Length(FActionConditionForPreview) - 1].Text := '$my_var$==$my_var$';
  end;

  vstActionConditions.RootNodeCount := Length(FActionConditionForPreview);
  vstActionConditions.Repaint;

  TriggerOnControlsModified;
end;


procedure TfrClickerConditionEditor.spdbtnAddANDClick(Sender: TObject);   //new set of columns
var
  i: Integer;
begin
  if Length(FActionConditionForPreview) = 0 then
  begin
    SetLength(FActionConditionForPreview, Length(FActionConditionForPreview) + 1);
    FActionConditionForPreview[Length(FActionConditionForPreview) - 1] := TStringList.Create;   //create a row if there is none
  end;

  for i := 0 to Length(FActionConditionForPreview) - 1 do
    FActionConditionForPreview[i].Add('$my_var$==$my_var$');

  if vstActionConditions.Header.Columns.Count > 0 then
    vstActionConditions.Header.Columns.Items[vstActionConditions.Header.Columns.Count - 1].Text := 'AND'; //convert last OR to an AND

  AddExpressionColumns(True); //OR
  vstActionConditions.RootNodeCount := Length(FActionConditionForPreview);

  TriggerOnControlsModified;
end;


procedure TfrClickerConditionEditor.MenuItemRemoveTermClick(Sender: TObject);
var
  Node: PVirtualNode;
  i: Integer;
begin
  Node := vstActionConditions.GetFirstSelected;
  if Node = nil then
  begin
    MessageBox(Handle, 'No expression term selected. Please select one (a row).', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  if MessageBox(Handle, PChar('Are you sure you want to remove the selected term (row) ?' + #13#10 + 'This will be done for all expression parts (columns).'), PChar(Caption), MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  FActionConditionForPreview[Node^.Index].Free;
  for i := Node^.Index to Length(FActionConditionForPreview) - 2 do
    FActionConditionForPreview[i] := FActionConditionForPreview[i + 1];

  SetLength(FActionConditionForPreview, Length(FActionConditionForPreview) - 1);
  vstActionConditions.RootNodeCount := Length(FActionConditionForPreview);
  vstActionConditions.Repaint;

  if Length(FActionConditionForPreview) = 0 then
    for i := vstActionConditions.Header.Columns.Count - 1 downto 0 do
      vstActionConditions.Header.Columns.Delete(i);

  TriggerOnControlsModified;
end;


procedure TfrClickerConditionEditor.MenuItemRemoveExpressionPartClick(
  Sender: TObject);
var
  i: Integer;
  ExpressionIndex: Integer;
begin
  if vstActionConditions.RootNodeCount = 0 then
  begin
    MessageBox(Handle, 'Nothing to remove.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  if FActionConditionsHeaderHitInfo.HitColumn < 0 then
  begin
    MessageBox(Handle, 'Please click on a column title from header, to get the proper column index.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  ExpressionIndex := FActionConditionsHeaderHitInfo.HitColumn shr 2;

  if MessageBox(Handle, PChar('Are you sure you want to remove the expression parts from group ' + IntToStr(ExpressionIndex) + ' ?' + #13#10 + 'This will be done for all terms (rows).'), PChar(Caption), MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  for i := 0 to Length(FActionConditionForPreview) - 1 do
    FActionConditionForPreview[i].Delete(ExpressionIndex);

  for i := vstActionConditions.Header.Columns.Count - 1 downto vstActionConditions.Header.Columns.Count - 4 do
    vstActionConditions.Header.Columns.Delete(i);

  if vstActionConditions.Header.Columns.Count > 0 then
    vstActionConditions.Header.Columns.Header.Columns[vstActionConditions.Header.Columns.Count - 1].Text := 'OR';

  FActionConditionsHeaderHitInfo.HitColumn := -1;

  if vstActionConditions.Header.Columns.Count = 0 then
  begin
    for i := 0 to Length(FActionConditionForPreview) - 1 do
      FActionConditionForPreview[i].Free;

    SetLength(FActionConditionForPreview, 0);
    vstActionConditions.RootNodeCount := 0;
    vstActionConditions.Repaint;
  end;

  TriggerOnControlsModified;
end;


procedure TfrClickerConditionEditor.MenuItemNotEqualClick(Sender: TObject);
begin
  SetConditionOperator(CCompNotEqual);
end;


procedure TfrClickerConditionEditor.MenuItemEqualClick(Sender: TObject);
begin
  SetConditionOperator(CCompEqual);
end;


procedure TfrClickerConditionEditor.MenuItemLessThanClick(Sender: TObject);
begin
  SetConditionOperator(CCompLessThan);
end;


procedure TfrClickerConditionEditor.MenuItemGreaterThanClick(Sender: TObject);
begin
  SetConditionOperator(CCompGreaterThan);
end;


procedure TfrClickerConditionEditor.MenuItemLessThanOrEqualClick(Sender: TObject);
begin
  SetConditionOperator(CCompLessThanOrEqual);
end;


procedure TfrClickerConditionEditor.MenuItemGreaterThanOrEqualClick(
  Sender: TObject);
begin
  SetConditionOperator(CCompGreaterThanOrEqual);
end;


procedure TfrClickerConditionEditor.MenuItem_AddLastActionStatusEqualsAllowedFailedClick
  (Sender: TObject);
var
  i: Integer;
  s: string;
begin
  SetLength(FActionConditionForPreview, Length(FActionConditionForPreview) + 1);
  FActionConditionForPreview[Length(FActionConditionForPreview) - 1] := TStringList.Create;

  if Length(FActionConditionForPreview) > 1 then //there is already an item to copy from
  begin
    s := '';
    for i := 0 to FActionConditionForPreview[0].Count - 1 do
      s := s + '$LastAction_Status$==Allowed Failed' + #13#10;

    FActionConditionForPreview[Length(FActionConditionForPreview) - 1].Text := s;
  end
  else
  begin
    AddExpressionColumns(True); //OR
    FActionConditionForPreview[Length(FActionConditionForPreview) - 1].Text := '$LastAction_Status$==Allowed Failed';
  end;

  vstActionConditions.RootNodeCount := Length(FActionConditionForPreview);
  vstActionConditions.Repaint;

  TriggerOnControlsModified;
end;


procedure TfrClickerConditionEditor.MenuItem_AddLastActionStatusEqualsSuccessfulClick
  (Sender: TObject);
var
  i: Integer;
  s: string;
begin
  SetLength(FActionConditionForPreview, Length(FActionConditionForPreview) + 1);
  FActionConditionForPreview[Length(FActionConditionForPreview) - 1] := TStringList.Create;

  if Length(FActionConditionForPreview) > 1 then //there is already an item to copy from
  begin
    s := '';
    for i := 0 to FActionConditionForPreview[0].Count - 1 do
      s := s + '$LastAction_Status$==Successful' + #13#10;

    FActionConditionForPreview[Length(FActionConditionForPreview) - 1].Text := s;
  end
  else
  begin
    AddExpressionColumns(True); //OR
    FActionConditionForPreview[Length(FActionConditionForPreview) - 1].Text := '$LastAction_Status$==Successful';
  end;

  vstActionConditions.RootNodeCount := Length(FActionConditionForPreview);
  vstActionConditions.Repaint;

  TriggerOnControlsModified;
end;


procedure TfrClickerConditionEditor.MenuItem_AddPrefixWithZerosEqualsNumberClick
  (Sender: TObject);
var
  i: Integer;
  s: string;
begin
  SetLength(FActionConditionForPreview, Length(FActionConditionForPreview) + 1);
  FActionConditionForPreview[Length(FActionConditionForPreview) - 1] := TStringList.Create;

  if Length(FActionConditionForPreview) > 1 then //there is already an item to copy from
  begin
    s := '';
    for i := 0 to FActionConditionForPreview[0].Count - 1 do
      s := s + '$PrefixWithZeros($SomeNumber$,6)$==001234' + #13#10;

    FActionConditionForPreview[Length(FActionConditionForPreview) - 1].Text := s;
  end
  else
  begin
    AddExpressionColumns(True); //OR
    FActionConditionForPreview[Length(FActionConditionForPreview) - 1].Text := '$PrefixWithZeros($SomeNumber$,6)$==001234';
  end;

  vstActionConditions.RootNodeCount := Length(FActionConditionForPreview);
  vstActionConditions.Repaint;

  TriggerOnControlsModified;
end;


end.

