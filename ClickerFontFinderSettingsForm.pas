{
    Copyright (C) 2023 VCC
    creation date: 22 Nov 2023
    initial release date: 23 Nov 2023

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


unit ClickerFontFinderSettingsForm;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Spin, StdCtrls,
  ExtCtrls, VirtualTrees, ClickerUtils;

type

  { TfrmClickerFontFinderSettings }

  TfrmClickerFontFinderSettings = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    chkShowAllFonts: TCheckBox;
    grpPreview: TGroupBox;
    lblMinSize: TLabel;
    lblMaxSize: TLabel;
    lblFontNames: TLabel;
    lblPreviewText: TLabel;
    spnedtMinSize: TSpinEdit;
    spnedtMaxSize: TSpinEdit;
    tmrStartup: TTimer;
    tmrChecked: TTimer;
    vstFonts: TVirtualStringTree;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure chkShowAllFontsChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrCheckedTimer(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
    procedure vstFontsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstFontsChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
    procedure vstFontsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vstFontsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private
    FListOfUsedFonts: TStringList;  //only the used fonts
    FLatestCheckedNode: PVirtualNode;

    procedure SetVSTCheckStates;
    procedure RefreshDisplayFontSettings;
    procedure HandleNodeChecked;
  public

  end;


function EditFontFinderSettings(var AFontFinderSettings: TFontFinderSettings; APreviewFont: TFont; AFGColor, ABGColor: TColor; APreviewText: string): Boolean;


implementation

{$R *.frm}


uses
  Math;


function EditFontFinderSettings(var AFontFinderSettings: TFontFinderSettings; APreviewFont: TFont; AFGColor, ABGColor: TColor; APreviewText: string): Boolean;
var
  frmClickerFontFinderSettings: TfrmClickerFontFinderSettings;
begin
  Result := False;
  Application.CreateForm(TfrmClickerFontFinderSettings, frmClickerFontFinderSettings);

  frmClickerFontFinderSettings.FListOfUsedFonts.Text := AFontFinderSettings.ListOfUsedFonts;
  frmClickerFontFinderSettings.spnedtMinSize.Value := AFontFinderSettings.MinFontSize;
  frmClickerFontFinderSettings.spnedtMaxSize.Value := AFontFinderSettings.MaxFontSize;
  frmClickerFontFinderSettings.chkShowAllFonts.Checked := AFontFinderSettings.ShowAllFonts;

  frmClickerFontFinderSettings.lblPreviewText.Caption := APreviewText;

  if frmClickerFontFinderSettings.FListOfUsedFonts.Count > 0 then
    frmClickerFontFinderSettings.lblPreviewText.Font.Name := frmClickerFontFinderSettings.FListOfUsedFonts.Strings[0];

  frmClickerFontFinderSettings.lblPreviewText.Font.Size := Max(8, APreviewFont.Size);
  frmClickerFontFinderSettings.lblPreviewText.Font.Color := AFGColor; //APreviewFont.Color;
  frmClickerFontFinderSettings.lblPreviewText.Font.Quality := APreviewFont.Quality;
  frmClickerFontFinderSettings.lblPreviewText.Font.Style := APreviewFont.Style;
  frmClickerFontFinderSettings.lblPreviewText.Color := ABGColor;

  frmClickerFontFinderSettings.vstFonts.Hint := 'Initial fonts: ' + #13#10#13#10 + AFontFinderSettings.ListOfUsedFonts;
  frmClickerFontFinderSettings.SetVSTCheckStates;

  frmClickerFontFinderSettings.ShowModal;

  if frmClickerFontFinderSettings.Tag = 1 then
  begin
    Result := True;
    AFontFinderSettings.ListOfUsedFonts := frmClickerFontFinderSettings.FListOfUsedFonts.Text;
    AFontFinderSettings.MinFontSize := frmClickerFontFinderSettings.spnedtMinSize.Value;
    AFontFinderSettings.MaxFontSize := frmClickerFontFinderSettings.spnedtMaxSize.Value;
    AFontFinderSettings.ShowAllFonts := frmClickerFontFinderSettings.chkShowAllFonts.Checked;
  end;
end;


{ TfrmClickerFontFinderSettings }

procedure TfrmClickerFontFinderSettings.FormCreate(Sender: TObject);
begin
  vstFonts.RootNodeCount := Screen.Fonts.Count;
  FListOfUsedFonts := TStringList.Create;
  FLatestCheckedNode := nil;
  tmrStartup.Enabled := True;
end;


procedure TfrmClickerFontFinderSettings.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FListOfUsedFonts);
end;


procedure TfrmClickerFontFinderSettings.tmrCheckedTimer(Sender: TObject);
begin
  tmrChecked.Enabled := False;
  HandleNodeChecked;
end;


procedure TfrmClickerFontFinderSettings.tmrStartupTimer(Sender: TObject);
begin
  tmrStartup.Enabled := False;
  RefreshDisplayFontSettings;
end;


procedure TfrmClickerFontFinderSettings.RefreshDisplayFontSettings;
var
  ShowAll: Boolean;
  Node: PVirtualNode;
begin
  ShowAll := chkShowAllFonts.Checked;
  Node := vstFonts.GetFirst;
  if Node = nil then
    Exit;

  repeat
    vstFonts.IsVisible[Node] := ShowAll or (Node^.CheckState = csCheckedNormal);
    Node := Node^.NextSibling;
  until Node = nil;

  vstFonts.Repaint;
end;


procedure TfrmClickerFontFinderSettings.chkShowAllFontsChange(Sender: TObject);
begin
  RefreshDisplayFontSettings;
end;


procedure TfrmClickerFontFinderSettings.btnOKClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;


procedure TfrmClickerFontFinderSettings.btnCancelClick(Sender: TObject);
begin
  Tag := 0;
  Close;
end;


procedure TfrmClickerFontFinderSettings.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;


procedure TfrmClickerFontFinderSettings.HandleNodeChecked;
var
  FontIsUsed: Boolean;
  FontIndexInUsedList: Integer;
begin
  if FLatestCheckedNode = nil then
    Exit;

  FontIndexInUsedList := FListOfUsedFonts.IndexOf(Screen.Fonts.Strings[FLatestCheckedNode^.Index]);
  FontIsUsed := FontIndexInUsedList > -1;

  if FontIsUsed then
  begin
    if FLatestCheckedNode^.CheckState = csUncheckedNormal then //remove from list
      FListOfUsedFonts.Delete(FontIndexInUsedList);
  end
  else
    if FLatestCheckedNode^.CheckState = csCheckedNormal then  //add to list
      FListOfUsedFonts.Add(Screen.Fonts.Strings[FLatestCheckedNode^.Index]);
end;


procedure TfrmClickerFontFinderSettings.vstFontsChecked(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FLatestCheckedNode := Node;
  tmrChecked.Enabled := True;
end;


procedure TfrmClickerFontFinderSettings.vstFontsChecking(
  Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState;
  var Allowed: Boolean);
begin
  Allowed := True;
end;


procedure TfrmClickerFontFinderSettings.vstFontsGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
begin
  try
    CellText := Screen.Fonts.Strings[Node^.Index];
  except
    CellText := 'bug'; //in case the list changes on the fly
  end;
end;


procedure TfrmClickerFontFinderSettings.SetVSTCheckStates;
var
  Node: PVirtualNode;
begin
  Node := vstFonts.GetFirst;
  if Node = nil then
    Exit;

  repeat
    Node^.CheckType := ctCheckBox;   //this is also executed on vstFontsInitNode, but only for nodes which come into view

    try
      if FListOfUsedFonts.IndexOf(Screen.Fonts.Strings[Node^.Index]) > -1 then
        vstFonts.CheckState[Node] := csCheckedNormal
      else
        vstFonts.CheckState[Node] := csUncheckedNormal;
    except
    end;

    Node := Node^.NextSibling;
  until Node = nil;
end;


procedure TfrmClickerFontFinderSettings.vstFontsInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  Node^.CheckType := ctCheckBox;
end;

end.

