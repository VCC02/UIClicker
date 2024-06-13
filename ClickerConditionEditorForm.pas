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


unit ClickerConditionEditorForm;

{$H+}
{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ClickerConditionEditorFrame;

type

  { TfrmClickerConditionEditor }

  TfrmClickerConditionEditor = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    pnlActionConditions: TPanel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FfrClickerConditionEditor: TfrClickerConditionEditor;
    FModified: Boolean;

    procedure CreateRemainingUIComponents;
    procedure ClickerConditionEditorControlsModified;
  public

  end;


function EditActionCondition(var AActionCondition: string): Boolean;

implementation

{$R *.frm}


function EditActionCondition(var AActionCondition: string): Boolean;
var
  frmClickerConditionEditor: TfrmClickerConditionEditor;
begin
  Result := False;
  Application.CreateForm(TfrmClickerConditionEditor, frmClickerConditionEditor);

  frmClickerConditionEditor.FfrClickerConditionEditor.DisplayActionCondition(AActionCondition);
  frmClickerConditionEditor.ShowModal;

  if (frmClickerConditionEditor.Tag = 1) and (frmClickerConditionEditor.FModified) then
  begin
    Result := True;
    AActionCondition := frmClickerConditionEditor.FfrClickerConditionEditor.GetActionCondition;
  end;
end;

{ TfrmClickerConditionEditor }

procedure TfrmClickerConditionEditor.FormCreate(Sender: TObject);
begin
  CreateRemainingUIComponents;
  FModified := False;
end;


procedure TfrmClickerConditionEditor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;


procedure TfrmClickerConditionEditor.btnOKClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;


procedure TfrmClickerConditionEditor.btnCancelClick(Sender: TObject);
begin
  Tag := 0;
  Close;
end;


procedure TfrmClickerConditionEditor.ClickerConditionEditorControlsModified;
begin
  FModified := True;
end;


procedure TfrmClickerConditionEditor.CreateRemainingUIComponents;
begin
  FfrClickerConditionEditor := TfrClickerConditionEditor.Create(Self);
  FfrClickerConditionEditor.Parent := pnlActionConditions; //for some reason, using TabSheetCondition leads to a hidden frame

  FfrClickerConditionEditor.Left := 3;
  FfrClickerConditionEditor.Top := 3;
  FfrClickerConditionEditor.Width := pnlActionConditions.Width - 3;
  FfrClickerConditionEditor.Height := pnlActionConditions.Height - 12;
  FfrClickerConditionEditor.Anchors := [akBottom, akLeft, akRight, akTop];
  FfrClickerConditionEditor.OnControlsModified := ClickerConditionEditorControlsModified;
  FfrClickerConditionEditor.Visible := True;
end;


end.

